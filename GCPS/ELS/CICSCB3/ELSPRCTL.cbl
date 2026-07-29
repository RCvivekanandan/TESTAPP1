00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSPRCTL
00003  PROGRAM-ID.         ELSPRCTL.                                       LV002
00004                                                                   ELSPRCTL
00005  AUTHOR.             EDWARD G LISS                                ELSPRCTL
00006                                                                   ELSPRCTL
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSPRCTL
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSPRCTL
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSPRCTL
00010                      233 N. MICHIGAN AVE                          ELSPRCTL
00011                      CHICAGO, ILLINOIS 60601                      ELSPRCTL
00012                                                                   ELSPRCTL
00013  DATE-WRITTEN.       06-NOV-1986.                                 ELSPRCTL
00014                                                                   ELSPRCTL
00015  DATE-COMPILED.                                                   ELSPRCTL
00016                                                                   ELSPRCTL
00017  SECURITY.           COPYRIGHT 1986,                              ELSPRCTL
00018                      HEALTH CARE SERVICE CORPORATION              ELSPRCTL
00019      SKIP3                                                        ELSPRCTL
00020  ENVIRONMENT DIVISION.                                            ELSPRCTL
00021                                                                   ELSPRCTL
00022  CONFIGURATION SECTION.                                           ELSPRCTL
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELSPRCTL
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELSPRCTL
00025      EJECT                                                        ELSPRCTL
00026 ******************************************************************ELSPRCTL
00027 *                                                                *ELSPRCTL
00028 *    PROGRAM:    ELSPRCTL                                        *ELSPRCTL
00029 *    DATE:       06-NOV-1986                                     *ELSPRCTL
00030 *    AUTHOR:     EDWARD G LISS                                   *ELSPRCTL
00031 *    FUNCTION:                                                   *ELSPRCTL
00032 *      THIS MODULE IS THE PROVIDER CONTROL SELECTOR.  IT ASKS    *ELSPRCTL
00033 *      THE USER TO DECIDE WHICH PROVIDE CONTROL THE INQUIRY      *ELSPRCTL
00034 *      IS FOR.                                                   *ELSPRCTL
00035 *                                                                *ELSPRCTL
00036 *    NOTES:                                                      *ELSPRCTL
00037 *                                                                *ELSPRCTL
00038 ******************************************************************ELSPRCTL
00039 *                                                                *ELSPRCTL
00040 *                      MAINTENANCE HISTORY                       *ELSPRCTL
00041 *                                                                *ELSPRCTL
00042 *  MOD     DATE     BY  DRPT                ACTION               *ELSPRCTL
00043 * ----- ----------- --- ----- ---------------------------------- *ELSPRCTL
00044 * 01.00 06-NOV-1986 EGL       CREATED                            *ELSPRCTL
00045 *                                                                *ELSPRCTL
00046 * 01.01 11-APR-1988 EGL       CONVERTED FROM STRUCTURES          *ELSPRCTL
00047 *                                                                *ELSPRCTL
00048 * 01.02 02-MAY-1989 GEM       STORAGE MANAGEMENT ENHANCEMENTS.   *ELSPRCTL
00049 *                                                                *ELSPRCTL
00050 * 01.03 15-MAY-1989 EGL/GEM   CORRECTED CICS ABEND AZTS.         *ELSPRCTL
00051 *                                                                *ELSPRCTL
00052 * 01.04 19-JUL-1989 EGL       CORRECTED PROGRAM DESCRIPTION.     *ELSPRCTL
00053 *                                                                *ELSPRCTL
00054 * 01.05 05-JUN-1990 GEM       ADD GVL TABULAR NOTICE PROCESSING. *ELSPRCTL
00055 *                                                                *ELSPRCTL
00056 * 01.06 21-JUN-1990 GEM       COSMETIC CHANGE TO NOTICE.         *ELSPRCTL
00057 *                                                                *ELSPRCTL
00058 * 01.07 05-FEB-1996 AKK       CHANGES GVL NOTICE TO READ SPECIAL *ELSPRCTL
00059 *                             PROVIDER CONSIDERATIONS.           *ELSPRCTL
00060 *                                                                *ELSPRCTL
00061 * 01.08 21-OCT-1997 AKK       ADD SUPPORT FOR YR2000 AND TX MERGE.ELSPRCTL
00062 *                                                                *ELSPRCTL
00063 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST     ELSPRCTL
00064 *                                                                *ELSPRCTL
00065 ******************************************************************ELSPRCTL
00066      EJECT                                                        ELSPRCTL
00067  DATA DIVISION.                                                   ELSPRCTL
00068                                                                   ELSPRCTL
00069  WORKING-STORAGE SECTION.                                         ELSPRCTL
00070                                                                   ELSPRCTL
00071  77  WS-DUPLICATE-PC-SW             PICTURE X VALUE 'Y'.          ELSPRCTL
00072      88  WS-NO-DUPLICATE-PC                   VALUE 'N'.          ELSPRCTL
00073      88  WS-DUPLICATE-PC                      VALUE 'Y'.          ELSPRCTL
00074                                                                   ELSPRCTL
00075  77  WS-CONTRACT-TALLY              PICTURE S9(4) COMP SYNC.      ELSPRCTL
00076                                                                   ELSPRCTL
00077  01  WS-GVL-TAB-FND-SW              PICTURE X VALUE 'N'.          ELSPRCTL
00078      88  WS-GVL-TABULAR-FOUND                 VALUE 'Y'.          ELSPRCTL
00079                                                                   ELSPRCTL
00080  01  WS-GCG-SUB                     PICTURE S9(4) COMP SYNC.      ELSPRCTL
00081  01  WS-COUNTERS.                                                 ELSPRCTL
00082      05  WS-TOTAL-HEADINGS-LINES    PICTURE S9(4) COMP SYNC.      ELSPRCTL
00083                                                                   ELSPRCTL
00084 *                                                                 ELSPRCTL
00085 *      MENU FORMAT AREAS                                          ELSPRCTL
00086 *                                                                 ELSPRCTL
00087  01  WS-PC-DETAIL.                                                ELSPRCTL
00088      05  FILLER                     PICTURE X(2).                 ELSPRCTL
00089      05  WS-PC-CODE                 PICTURE X(2).                 ELSPRCTL
00090      05  FILLER                     PICTURE X.                    ELSPRCTL
00091      05  WS-PC-DESCRIPTION          PICTURE X(74).                ELSPRCTL
00092                                                                   ELSPRCTL
00093  01  WS-SCREEN-TITLES.                                            ELSPRCTL
00094      05  WS-INST-BASIC-PROV-CNTL    PICTURE X(50) VALUE           ELSPRCTL
00095          'SELECT INSTITUTIONAL BASIC PROVIDER CONTROL'.           ELSPRCTL
00096      05  WS-INST-SUPPL-PROV-CNTL    PICTURE X(50) VALUE           ELSPRCTL
00097          'SELECT INSTITUTIONAL SUPPLEMENTAL PROVIDER CONTROL'.    ELSPRCTL
00098      05  WS-PROF-BASIC-PROV-CNTL    PICTURE X(50) VALUE           ELSPRCTL
00099          'SELECT PROFESSIONAL BASIC PROVIDER CONTROL'.            ELSPRCTL
00100      05  WS-PROF-SUPPL-PROV-CNTL    PICTURE X(50) VALUE           ELSPRCTL
00101          'SELECT PROFESSIONAL SUPPLEMENTAL PROVIDER CONTROL'.     ELSPRCTL
00102  01  WS-PC-MENU-HEADINGS.                                         ELSPRCTL
00103      05  WS-PC-NUM-HEADING-LINES    PICTURE S9(4) COMP SYNC       ELSPRCTL
00104                                             VALUE +5.             ELSPRCTL
00105      05  WS-PC-HEADING-LINE-DEF.                                  ELSPRCTL
00106          10  WS-PC-HEAD-LINE-1.                                   ELSPRCTL
00107              15  WS-TYPE-TITLE-1   PICTURE X(14).                 ELSPRCTL
00108              15  FILLER            PICTURE X(65) VALUE            ELSPRCTL
00109                  'BENEFITS FOR THIS GROUP/SECTION VARY ACCORDING TELSPRCTL
00110 -                'O THE TYPE OF'.                                 ELSPRCTL
00111          10  WS-PC-HEAD-LINE-2.                                   ELSPRCTL
00112              15  FILLER            PICTURE X(79) VALUE            ELSPRCTL
00113                  'PROVIDER WHO PERFORMS THE SERVICES.  SELECT THE ELSPRCTL
00114 -                'PARTICULAR TYPE OF'.                            ELSPRCTL
00115          10  WS-PC-HEAD-LINE-3.                                   ELSPRCTL
00116              15  WS-TYPE-TITLE-2   PICTURE X(14).                 ELSPRCTL
00117              15  FILLER            PICTURE X(65) VALUE            ELSPRCTL
00118                  'PROVIDER FOR WHICH YOU WISH BENEFITS TO BE DISPLELSPRCTL
00119 -                'AYED.'.                                         ELSPRCTL
00120          10  WS-PC-HEAD-LINE-4.                                   ELSPRCTL
00121              15  FILLER            PICTURE X(16) VALUE            ELSPRCTL
00122                           'CODE DESCRIPTION'.                     ELSPRCTL
00123                                                                   ELSPRCTL
00124  01  WS-GVL-NOTICE-LINES.                                         ELSPRCTL
00125      05  WS-GVL-NUM-NOTICE-LINES    PICTURE S9(4) COMP SYNC       ELSPRCTL
00126                                             VALUE +5.             ELSPRCTL
00127      05  WS-GVL-NOTICE-LINE-DEF.                                  ELSPRCTL
00128          10  WS-GVL-NOTICE-LINE-1.                                ELSPRCTL
00129              15  FILLER            PICTURE X(05) VALUE SPACES.    ELSPRCTL
00130              15  FILLER            PICTURE X(33) VALUE            ELSPRCTL
00131                 '** NOTE *************************'.              ELSPRCTL
00132              15  FILLER            PICTURE X(27) VALUE            ELSPRCTL
00133                 '***************************'.                    ELSPRCTL
00134              15  FILLER            PICTURE X(14) VALUE SPACES.    ELSPRCTL
00135          10  WS-GVL-NOTICE-LINE-2.                                ELSPRCTL
00136              15  FILLER            PICTURE X(05) VALUE SPACES.    ELSPRCTL
00137              15  FILLER            PICTURE X(38) VALUE            ELSPRCTL
00138                 '* CERTAIN PROVIDERS FOR GROUP/SECTION '.         ELSPRCTL
00139              15  FILLER            PICTURE X(22) VALUE            ELSPRCTL
00140                 'HAVE SPECIAL PAYMENT *'.                         ELSPRCTL
00141              15  FILLER            PICTURE X(14) VALUE SPACES.    ELSPRCTL
00142          10  WS-GVL-NOTICE-LINE-3.                                ELSPRCTL
00143              15  FILLER            PICTURE X(05) VALUE SPACES.    ELSPRCTL
00144              15  FILLER            PICTURE X(33) VALUE            ELSPRCTL
00145 *               '* LEVELS. SEE THE VARIABLE LEVEL '.              ELSPRCTL
00146                 '* LEVELS. SEE SPECIAL PROVIDER CO'.              ELSPRCTL
00147              15  FILLER            PICTURE X(27) VALUE            ELSPRCTL
00148 *               'PROVIDERS TOPIC (GVL).    *'.                    ELSPRCTL
00149                 'NSIDERATIONS TOPIC (SPC). *'.                    ELSPRCTL
00150              15  FILLER            PICTURE X(14) VALUE SPACES.    ELSPRCTL
00151          10  WS-GVL-NOTICE-LINE-4.                                ELSPRCTL
00152              15  FILLER            PICTURE X(05) VALUE SPACES.    ELSPRCTL
00153              15  FILLER            PICTURE X(33) VALUE            ELSPRCTL
00154                 '*********************************'.              ELSPRCTL
00155              15  FILLER            PICTURE X(27) VALUE            ELSPRCTL
00156                 '***************************'.                    ELSPRCTL
00157              15  FILLER            PICTURE X(14) VALUE SPACES.    ELSPRCTL
00158                                                                   ELSPRCTL
00159  01  WS-DUMMY-PTR         POINTER.                                ELSPRCTL
00160                                                                   ELSPRCTL
00161      EJECT                                                        ELSPRCTL
00162  COPY ELSTCWAC.                                                   ELSPRCTL
00163      EJECT                                                        ELSPRCTL
00164  LINKAGE SECTION.                                                 ELSPRCTL
00165                                                                   ELSPRCTL
00166  01  DFHCOMMAREA.                                                 ELSPRCTL
00167  COPY ELSCOMMC.                                                   ELSPRCTL
00168      EJECT                                                        ELSPRCTL
00169  COPY ELSCIA2C.                                                   ELSPRCTL
00170      EJECT                                                        ELSPRCTL
00171  COPY ELSSSCBC.                                                   ELSPRCTL
00172      EJECT                                                        ELSPRCTL
00173  COPY ELSKTBCC.                                                   ELSPRCTL
00174      EJECT                                                        ELSPRCTL
00175  COPY ELSMHDGC.                                                   ELSPRCTL
00176      EJECT                                                        ELSPRCTL
00177  COPY ELSMOPTC.                                                   ELSPRCTL
00178      EJECT                                                        ELSPRCTL
00179  COPY ELSMENUC.                                                   ELSPRCTL
00180      EJECT                                                        ELSPRCTL
00181  COPY ELSIOPMC.                                                   ELSPRCTL
00182      EJECT                                                        ELSPRCTL
00183  COPY ELSCMIFC.                                                   ELSPRCTL
00184      EJECT                                                        ELSPRCTL
00185  COPY ELSCMDSC.                                                   ELSPRCTL
00186      EJECT                                                        ELSPRCTL
00187  COPY ELSKEYSC.                                                   ELSPRCTL
00188      EJECT                                                        ELSPRCTL
00189  01  GROUP-SPECIFIC-REC.                                          ELSPRCTL
00190  COPY GCGROUPC.                                                   ELSPRCTL
00191      EJECT                                                        ELSPRCTL
00192  PROCEDURE DIVISION.                                              ELSPRCTL
00193 ************************************************************      ELSPRCTL
00194 *                                                          *      ELSPRCTL
00195 *                    PROCEDURE DIVISION                    *      ELSPRCTL
00196 *                                                          *      ELSPRCTL
00197 ************************************************************      ELSPRCTL
00198                                                                   ELSPRCTL
00199                                                                   ELSPRCTL
00200 ************************************************************      ELSPRCTL
00201 *                                                          *      ELSPRCTL
00202 *        PROVIDER CONTROL SELECTION                        *      ELSPRCTL
00203 *                                                          *      ELSPRCTL
00204 ************************************************************      ELSPRCTL
00205  PROVIDER-CONTROL-SELECTION.                                      ELSPRCTL
00206      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELSPRCTL
00207          PERFORM INVALID-COMMAREA-ABEND.                          ELSPRCTL
00208      PERFORM INITIALIZE-MODULE.                                   ELSPRCTL
00209      PERFORM MAIN-PROCESSING.                                     ELSPRCTL
00210      GOBACK.                                                      ELSPRCTL
00211                                                                   ELSPRCTL
00212                                                                   ELSPRCTL
00213 ************************************************************      ELSPRCTL
00214 *                                                          *      ELSPRCTL
00215 *        INITIALIZE MODULE                                 *      ELSPRCTL
00216 *                                                          *      ELSPRCTL
00217 ************************************************************      ELSPRCTL
00218  INITIALIZE-MODULE.                                               ELSPRCTL
00219      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSPRCTL
00220          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSPRCTL
00221                                                                   ELSPRCTL
00222      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSPRCTL
00223      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00224          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSPRCTL
00225                                                                   ELSPRCTL
00226                                                                   ELSPRCTL
00227 ************************************************************      ELSPRCTL
00228 *                                                          *      ELSPRCTL
00229 *        MAIN PROCESSING                                   *      ELSPRCTL
00230 *                                                          *      ELSPRCTL
00231 ************************************************************      ELSPRCTL
00232  MAIN-PROCESSING.                                                 ELSPRCTL
00233      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSPRCTL
00234          PERFORM PREPARE-PROVIDER-CONTROL-MENU                    ELSPRCTL
00235      ELSE IF SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)               ELSPRCTL
00236          PERFORM PROCESS-RESPONSE                                 ELSPRCTL
00237      ELSE                                                         ELSPRCTL
00238          PERFORM SYSTEM-LOGIC-ERROR.                              ELSPRCTL
00239                                                                   ELSPRCTL
00240                                                                   ELSPRCTL
00241 ************************************************************      ELSPRCTL
00242 *                                                          *      ELSPRCTL
00243 *        PREPARE PROVIDER CONTROL MENU                     *      ELSPRCTL
00244 *                                                          *      ELSPRCTL
00245 ************************************************************      ELSPRCTL
00246  PREPARE-PROVIDER-CONTROL-MENU.                                   ELSPRCTL
00247      PERFORM CHECK-IF-GVL-TABULAR-EXIST.                          ELSPRCTL
00248      PERFORM INITIALIZE-MENU.                                     ELSPRCTL
00249      PERFORM FORMAT-THE-MENU-CHOICES.                             ELSPRCTL
00250      PERFORM BEGIN-MENU-MODE.                                     ELSPRCTL
00251      EJECT                                                        ELSPRCTL
00252                                                                   ELSPRCTL
00253                                                                   ELSPRCTL
00254 ************************************************************      ELSPRCTL
00255 *                                                          *      ELSPRCTL
00256 *        INITIALIZE MENU                                   *      ELSPRCTL
00257 *                                                          *      ELSPRCTL
00258 ************************************************************      ELSPRCTL
00259  INITIALIZE-MENU.                                                 ELSPRCTL
00260      PERFORM INITIALIZE-THE-MENU-POINTERS.                        ELSPRCTL
00261      PERFORM INITIALIZE-THE-MENU-FILE.                            ELSPRCTL
00262      PERFORM INITIALIZE-THE-MENU-TABLES.                          ELSPRCTL
00263      EJECT                                                        ELSPRCTL
00264                                                                   ELSPRCTL
00265                                                                   ELSPRCTL
00266 ************************************************************      ELSPRCTL
00267 *                                                          *      ELSPRCTL
00268 *        INITIALIZE THE MENU POINTERS                      *      ELSPRCTL
00269 *                                                          *      ELSPRCTL
00270 ************************************************************      ELSPRCTL
00271  INITIALIZE-THE-MENU-POINTERS.                                    ELSPRCTL
00272      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSPRCTL
00273      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00274                            WS-DUMMY-PTR.                          ELSPRCTL
00275      IF CIA-RC-PTR-NULL                                           ELSPRCTL
00276          PERFORM ALLOCATE-CMI-AREA.                               ELSPRCTL
00277      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSPRCTL
00278      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00279          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELSPRCTL
00280                                                                   ELSPRCTL
00281      SET CIA-ELSKTBC-DDN TO TRUE.                                 ELSPRCTL
00282      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00283                            WS-DUMMY-PTR.                          ELSPRCTL
00284      IF CIA-RC-PTR-NULL                                           ELSPRCTL
00285          PERFORM READ-THE-CONTRACT-KEY-TABLE.                     ELSPRCTL
00286      SET CIA-ELSKTBC-DDN TO TRUE.                                 ELSPRCTL
00287      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00288          ADDRESS OF KTC-GCCONTR-KEY-TABLE.                        ELSPRCTL
00289                                                                   ELSPRCTL
00290      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSPRCTL
00291      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00292                            WS-DUMMY-PTR.                          ELSPRCTL
00293      IF CIA-RC-PTR-NULL                                           ELSPRCTL
00294          PERFORM ALLOCATE-MENU-IO-PARAMETER-BLO.                  ELSPRCTL
00295      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSPRCTL
00296      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00297          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSPRCTL
00298                                                                   ELSPRCTL
00299                                                                   ELSPRCTL
00300 ************************************************************      ELSPRCTL
00301 *                                                          *      ELSPRCTL
00302 *        ALLOCATE CMI AREA                                 *      ELSPRCTL
00303 *                                                          *      ELSPRCTL
00304 ************************************************************      ELSPRCTL
00305  ALLOCATE-CMI-AREA.                                               ELSPRCTL
00306      SET CIA-ELSCMIF-DDN    TO  TRUE.                             ELSPRCTL
00307      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSPRCTL
00308      PERFORM CALL-THE-STORAGE-MANAGER.                            ELSPRCTL
00309      EJECT                                                        ELSPRCTL
00310                                                                   ELSPRCTL
00311                                                                   ELSPRCTL
00312 ************************************************************      ELSPRCTL
00313 *                                                          *      ELSPRCTL
00314 *        READ THE CONTRACT KEY TABLE                       *      ELSPRCTL
00315 *                                                          *      ELSPRCTL
00316 ************************************************************      ELSPRCTL
00317  READ-THE-CONTRACT-KEY-TABLE.                                     ELSPRCTL
00318      SET CIA-ELSKTBC-DDN   TO  TRUE.                              ELSPRCTL
00319      SET CIA-STG-RETRIEVE  TO  TRUE.                              ELSPRCTL
00320      PERFORM CALL-THE-STORAGE-MANAGER.                            ELSPRCTL
00321      EJECT                                                        ELSPRCTL
00322                                                                   ELSPRCTL
00323                                                                   ELSPRCTL
00324 ************************************************************      ELSPRCTL
00325 *                                                          *      ELSPRCTL
00326 *        ALLOCATE MENU IO PARAMETER BLOCK                  *      ELSPRCTL
00327 *                                                          *      ELSPRCTL
00328 ************************************************************      ELSPRCTL
00329  ALLOCATE-MENU-IO-PARAMETER-BLO.                                  ELSPRCTL
00330      SET CIA-ELSMENU-DDN    TO  TRUE.                             ELSPRCTL
00331      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSPRCTL
00332      PERFORM CALL-THE-STORAGE-MANAGER.                            ELSPRCTL
00333                                                                   ELSPRCTL
00334                                                                   ELSPRCTL
00335 ************************************************************      ELSPRCTL
00336 *                                                          *      ELSPRCTL
00337 *        INITIALIZE THE MENU FILE                          *      ELSPRCTL
00338 *                                                          *      ELSPRCTL
00339 ************************************************************      ELSPRCTL
00340  INITIALIZE-THE-MENU-FILE.                                        ELSPRCTL
00341      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSPRCTL
00342      SET IOP-DEL TO TRUE.                                         ELSPRCTL
00343      SET IOP-FCQ-NONE TO TRUE.                                    ELSPRCTL
00344      SET IOP-KVQ-NONE TO TRUE.                                    ELSPRCTL
00345      PERFORM CALL-INPUT-OUTPUT.                                   ELSPRCTL
00346                                                                   ELSPRCTL
00347                                                                   ELSPRCTL
00348 ************************************************************      ELSPRCTL
00349 *                                                          *      ELSPRCTL
00350 *        INITIALIZE THE MENU TABLES                        *      ELSPRCTL
00351 *                                                          *      ELSPRCTL
00352 ************************************************************      ELSPRCTL
00353  INITIALIZE-THE-MENU-TABLES.                                      ELSPRCTL
00354      PERFORM INITIALIZE-MENU-SELECTIONS.                          ELSPRCTL
00355      PERFORM INITIALIZE-MENU-HEADINGS.                            ELSPRCTL
00356      PERFORM INITIALIZE-MENU-DESCRIPTIONS.                        ELSPRCTL
00357      EJECT                                                        ELSPRCTL
00358                                                                   ELSPRCTL
00359                                                                   ELSPRCTL
00360 ************************************************************      ELSPRCTL
00361 *                                                          *      ELSPRCTL
00362 *        INITIALIZE MENU SELECTIONS                        *      ELSPRCTL
00363 *                                                          *      ELSPRCTL
00364 ************************************************************      ELSPRCTL
00365  INITIALIZE-MENU-SELECTIONS.                                      ELSPRCTL
00366      PERFORM DETERMINE-CONTRACT-TYPE-UNDERX.                      ELSPRCTL
00367      MOVE ZEROS TO WS-CONTRACT-TALLY.                             ELSPRCTL
00368      PERFORM CONTRACT-TALLY                                       ELSPRCTL
00369          VARYING KTC-IDX FROM 1 BY 1                              ELSPRCTL
00370                  UNTIL KTC-IDX > KTC-NBR-KEYS.                    ELSPRCTL
00371      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSPRCTL
00372      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER        ELSPRCTL
00373          +                                                        ELSPRCTL
00374                         LENGTH OF MSO-MENU-OPT *                  ELSPRCTL
00375                         WS-CONTRACT-TALLY.                        ELSPRCTL
00376      SET CIA-STG-GETMAIN TO TRUE.                                 ELSPRCTL
00377      PERFORM CALL-THE-STORAGE-MANAGER.                            ELSPRCTL
00378      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSPRCTL
00379      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00380          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSPRCTL
00381      MOVE 2 TO MSO-OPT-LEN.                                       ELSPRCTL
00382      SET MSO-OPT-TYP-AN TO TRUE.                                  ELSPRCTL
00383      MOVE ZERO TO MSO-NBR-MENU-OPTS.                              ELSPRCTL
00384      MOVE LOW-VALUES TO SSB-MNU-CHOICE (1).                       ELSPRCTL
00385      MOVE 1   TO MSO-MIN-CHOICES                                  ELSPRCTL
00386                  MSO-MAX-CHOICES.                                 ELSPRCTL
00387      EJECT                                                        ELSPRCTL
00388                                                                   ELSPRCTL
00389                                                                   ELSPRCTL
00390 ************************************************************      ELSPRCTL
00391 *                                                          *      ELSPRCTL
00392 *        DETERMINE CONTRACT TYPE UNDER SELECTION           *      ELSPRCTL
00393 *                                                          *      ELSPRCTL
00394 ************************************************************      ELSPRCTL
00395  DETERMINE-CONTRACT-TYPE-UNDERX.                                  ELSPRCTL
00396      IF SSB-SS-GET-PROV-CTL-BAS-INST                              ELSPRCTL
00397          PERFORM SET-UP-BASIC-INSTITUTIONAL                       ELSPRCTL
00398      ELSE IF SSB-SS-GET-PROV-CTL-SUP-INST                         ELSPRCTL
00399          PERFORM SET-UP-SUPPL-INSTITUTIONAL                       ELSPRCTL
00400      ELSE IF SSB-SS-GET-PROV-CTL-BAS-PROF                         ELSPRCTL
00401          PERFORM SET-UP-BASIC-PROFESSIONAL                        ELSPRCTL
00402      ELSE IF SSB-SS-GET-PROV-CTL-SUP-PROF                         ELSPRCTL
00403          PERFORM SET-UP-SUPPL-PROFESSIONAL                        ELSPRCTL
00404      ELSE                                                         ELSPRCTL
00405          PERFORM SYSTEM-LOGIC-ERROR.                              ELSPRCTL
00406      EJECT                                                        ELSPRCTL
00407                                                                   ELSPRCTL
00408                                                                   ELSPRCTL
00409 ************************************************************      ELSPRCTL
00410 *                                                          *      ELSPRCTL
00411 *        SET UP BASIC INSTITUTIONAL                        *      ELSPRCTL
00412 *                                                          *      ELSPRCTL
00413 ************************************************************      ELSPRCTL
00414  SET-UP-BASIC-INSTITUTIONAL.                                      ELSPRCTL
00415      MOVE WS-INST-BASIC-PROV-CNTL                                 ELSPRCTL
00416           TO SSB-MNU-TITLE.                                       ELSPRCTL
00417      SET KTC-SEL-IDX TO 1.                                        ELSPRCTL
00418      EJECT                                                        ELSPRCTL
00419                                                                   ELSPRCTL
00420                                                                   ELSPRCTL
00421 ************************************************************      ELSPRCTL
00422 *                                                          *      ELSPRCTL
00423 *        SET UP SUPPL INSTITUTIONAL                        *      ELSPRCTL
00424 *                                                          *      ELSPRCTL
00425 ************************************************************      ELSPRCTL
00426  SET-UP-SUPPL-INSTITUTIONAL.                                      ELSPRCTL
00427      MOVE WS-INST-SUPPL-PROV-CNTL                                 ELSPRCTL
00428           TO SSB-MNU-TITLE.                                       ELSPRCTL
00429      SET KTC-SEL-IDX TO 2.                                        ELSPRCTL
00430      EJECT                                                        ELSPRCTL
00431                                                                   ELSPRCTL
00432                                                                   ELSPRCTL
00433 ************************************************************      ELSPRCTL
00434 *                                                          *      ELSPRCTL
00435 *        SET UP BASIC PROFESSIONAL                         *      ELSPRCTL
00436 *                                                          *      ELSPRCTL
00437 ************************************************************      ELSPRCTL
00438  SET-UP-BASIC-PROFESSIONAL.                                       ELSPRCTL
00439      MOVE WS-PROF-BASIC-PROV-CNTL                                 ELSPRCTL
00440           TO SSB-MNU-TITLE.                                       ELSPRCTL
00441      SET KTC-SEL-IDX TO 3.                                        ELSPRCTL
00442      EJECT                                                        ELSPRCTL
00443                                                                   ELSPRCTL
00444                                                                   ELSPRCTL
00445 ************************************************************      ELSPRCTL
00446 *                                                          *      ELSPRCTL
00447 *        SET UP SUPPL PROFESSIONAL                         *      ELSPRCTL
00448 *                                                          *      ELSPRCTL
00449 ************************************************************      ELSPRCTL
00450  SET-UP-SUPPL-PROFESSIONAL.                                       ELSPRCTL
00451      MOVE WS-PROF-SUPPL-PROV-CNTL                                 ELSPRCTL
00452           TO SSB-MNU-TITLE.                                       ELSPRCTL
00453      SET KTC-SEL-IDX TO 4.                                        ELSPRCTL
00454      EJECT                                                        ELSPRCTL
00455                                                                   ELSPRCTL
00456                                                                   ELSPRCTL
00457 ************************************************************      ELSPRCTL
00458 *                                                          *      ELSPRCTL
00459 *        CONTRACT TALLY                                    *      ELSPRCTL
00460 *                                                          *      ELSPRCTL
00461 ************************************************************      ELSPRCTL
00462  CONTRACT-TALLY.                                                  ELSPRCTL
00463      IF KTC-SEL (KTC-IDX, KTC-SEL-IDX)                            ELSPRCTL
00464          PERFORM ADD-1-TO-TALLY.                                  ELSPRCTL
00465                                                                   ELSPRCTL
00466                                                                   ELSPRCTL
00467 ************************************************************      ELSPRCTL
00468 *                                                          *      ELSPRCTL
00469 *        ADD 1 TO TALLY                                    *      ELSPRCTL
00470 *                                                          *      ELSPRCTL
00471 ************************************************************      ELSPRCTL
00472  ADD-1-TO-TALLY.                                                  ELSPRCTL
00473      ADD 1 TO WS-CONTRACT-TALLY.                                  ELSPRCTL
00474      EJECT                                                        ELSPRCTL
00475                                                                   ELSPRCTL
00476                                                                   ELSPRCTL
00477 ************************************************************      ELSPRCTL
00478 *                                                          *      ELSPRCTL
00479 *        INITIALIZE MENU HEADINGS                          *      ELSPRCTL
00480 *                                                          *      ELSPRCTL
00481 ************************************************************      ELSPRCTL
00482  INITIALIZE-MENU-HEADINGS.                                        ELSPRCTL
00483      PERFORM ALLOCATE-MENU-HEADINGS.                              ELSPRCTL
00484      PERFORM BUILD-MENU-HEADINGS.                                 ELSPRCTL
00485                                                                   ELSPRCTL
00486                                                                   ELSPRCTL
00487 ************************************************************      ELSPRCTL
00488 *                                                          *      ELSPRCTL
00489 *        ALLOCATE MENU HEADINGS                            *      ELSPRCTL
00490 *                                                          *      ELSPRCTL
00491 ************************************************************      ELSPRCTL
00492  ALLOCATE-MENU-HEADINGS.                                          ELSPRCTL
00493      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSPRCTL
00494      IF WS-GVL-TABULAR-FOUND                                      ELSPRCTL
00495         MOVE ZEROES TO WS-TOTAL-HEADINGS-LINES                    ELSPRCTL
00496         COMPUTE WS-TOTAL-HEADINGS-LINES =                         ELSPRCTL
00497                 WS-PC-NUM-HEADING-LINES + WS-GVL-NUM-NOTICE-LINES ELSPRCTL
00498         COMPUTE CIA-AREA-LEN =                                    ELSPRCTL
00499                 LENGTH OF MHD-NBR-HDG-LINES +                     ELSPRCTL
00500                 LENGTH OF MHD-HDG-LINE * WS-TOTAL-HEADINGS-LINES  ELSPRCTL
00501      ELSE                                                         ELSPRCTL
00502         COMPUTE CIA-AREA-LEN =                                    ELSPRCTL
00503                 LENGTH OF MHD-NBR-HDG-LINES +                     ELSPRCTL
00504                 LENGTH OF MHD-HDG-LINE * WS-PC-NUM-HEADING-LINES  ELSPRCTL
00505      END-IF.                                                      ELSPRCTL
00506      SET CIA-STG-GETMAIN TO TRUE.                                 ELSPRCTL
00507      PERFORM CALL-THE-STORAGE-MANAGER.                            ELSPRCTL
00508      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSPRCTL
00509      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00510          ADDRESS OF MHD-MENU-HEADINGS.                            ELSPRCTL
00511                                                                   ELSPRCTL
00512                                                                   ELSPRCTL
00513 ************************************************************      ELSPRCTL
00514 *                                                          *      ELSPRCTL
00515 *        BUILD MENU HEADINGS                               *      ELSPRCTL
00516 *                                                          *      ELSPRCTL
00517 ************************************************************      ELSPRCTL
00518  BUILD-MENU-HEADINGS.                                             ELSPRCTL
00519      IF WS-GVL-TABULAR-FOUND                                      ELSPRCTL
00520         MOVE WS-TOTAL-HEADINGS-LINES TO MHD-NBR-HDG-LINES         ELSPRCTL
00521      ELSE                                                         ELSPRCTL
00522         MOVE WS-PC-NUM-HEADING-LINES TO MHD-NBR-HDG-LINES         ELSPRCTL
00523      END-IF.                                                      ELSPRCTL
00524      PERFORM BUILD-HEADING-LINES-1-THRU-3.                        ELSPRCTL
00525      IF WS-GVL-TABULAR-FOUND                                      ELSPRCTL
00526         PERFORM BUILD-GVL-TABULAR-NOTICE.                         ELSPRCTL
00527      PERFORM BUILD-HEADING-LINES-4-THRU-5.                        ELSPRCTL
00528      EJECT                                                        ELSPRCTL
00529                                                                   ELSPRCTL
00530                                                                   ELSPRCTL
00531 ************************************************************      ELSPRCTL
00532 *                                                          *      ELSPRCTL
00533 *        BUILD HEADING LINES 1 THRU 3                      *      ELSPRCTL
00534 *                                                          *      ELSPRCTL
00535 ************************************************************      ELSPRCTL
00536  BUILD-HEADING-LINES-1-THRU-3.                                    ELSPRCTL
00537      IF SSB-SS-GET-PROV-CTL-BAS-PROF                              ELSPRCTL
00538              OR SSB-SS-GET-PROV-CTL-SUP-PROF                      ELSPRCTL
00539          PERFORM USE-PROFESSIONAL-NAME                            ELSPRCTL
00540      ELSE                                                         ELSPRCTL
00541          PERFORM USE-INSTITUTIONAL-NAME.                          ELSPRCTL
00542      MOVE WS-PC-HEAD-LINE-1       TO MHD-HDG-LINE (1).            ELSPRCTL
00543      MOVE WS-PC-HEAD-LINE-2       TO MHD-HDG-LINE (2).            ELSPRCTL
00544      MOVE WS-PC-HEAD-LINE-3       TO MHD-HDG-LINE (3).            ELSPRCTL
00545                                                                   ELSPRCTL
00546                                                                   ELSPRCTL
00547 ************************************************************      ELSPRCTL
00548 *                                                          *      ELSPRCTL
00549 *        BUILD GVL TABULAR NOTICE                          *      ELSPRCTL
00550 *                                                          *      ELSPRCTL
00551 ************************************************************      ELSPRCTL
00552  BUILD-GVL-TABULAR-NOTICE.                                        ELSPRCTL
00553      MOVE SPACES                  TO MHD-HDG-LINE (4).            ELSPRCTL
00554      MOVE WS-GVL-NOTICE-LINE-1    TO MHD-HDG-LINE (5).            ELSPRCTL
00555      MOVE WS-GVL-NOTICE-LINE-2    TO MHD-HDG-LINE (6).            ELSPRCTL
00556      MOVE WS-GVL-NOTICE-LINE-3    TO MHD-HDG-LINE (7).            ELSPRCTL
00557      MOVE WS-GVL-NOTICE-LINE-4    TO MHD-HDG-LINE (8).            ELSPRCTL
00558                                                                   ELSPRCTL
00559                                                                   ELSPRCTL
00560 ************************************************************      ELSPRCTL
00561 *                                                          *      ELSPRCTL
00562 *        USE PROFESSIONAL NAME                             *      ELSPRCTL
00563 *                                                          *      ELSPRCTL
00564 ************************************************************      ELSPRCTL
00565  USE-PROFESSIONAL-NAME.                                           ELSPRCTL
00566      MOVE 'PROFESSIONAL' TO WS-TYPE-TITLE-1                       ELSPRCTL
00567                             WS-TYPE-TITLE-2.                      ELSPRCTL
00568                                                                   ELSPRCTL
00569                                                                   ELSPRCTL
00570 ************************************************************      ELSPRCTL
00571 *                                                          *      ELSPRCTL
00572 *        USE INSTITUTIONAL NAME                            *      ELSPRCTL
00573 *                                                          *      ELSPRCTL
00574 ************************************************************      ELSPRCTL
00575  USE-INSTITUTIONAL-NAME.                                          ELSPRCTL
00576      MOVE 'INSTITUTIONAL' TO WS-TYPE-TITLE-1                      ELSPRCTL
00577                              WS-TYPE-TITLE-2.                     ELSPRCTL
00578                                                                   ELSPRCTL
00579                                                                   ELSPRCTL
00580 ************************************************************      ELSPRCTL
00581 *                                                          *      ELSPRCTL
00582 *        BUILD HEADING LINES 4 THRU 5                      *      ELSPRCTL
00583 *                                                          *      ELSPRCTL
00584 ************************************************************      ELSPRCTL
00585  BUILD-HEADING-LINES-4-THRU-5.                                    ELSPRCTL
00586      IF WS-GVL-TABULAR-FOUND                                      ELSPRCTL
00587         MOVE SPACES            TO  MHD-HDG-LINE (09)              ELSPRCTL
00588         MOVE WS-PC-HEAD-LINE-4 TO  MHD-HDG-LINE (10)              ELSPRCTL
00589      ELSE                                                         ELSPRCTL
00590         MOVE SPACES            TO  MHD-HDG-LINE (4)               ELSPRCTL
00591         MOVE WS-PC-HEAD-LINE-4 TO  MHD-HDG-LINE (5)               ELSPRCTL
00592      END-IF.                                                      ELSPRCTL
00593      EJECT                                                        ELSPRCTL
00594                                                                   ELSPRCTL
00595                                                                   ELSPRCTL
00596 ************************************************************      ELSPRCTL
00597 *                                                          *      ELSPRCTL
00598 *        CHECK IF #GVL TABULAR EXIST                       *      ELSPRCTL
00599 *                                                          *      ELSPRCTL
00600 ************************************************************      ELSPRCTL
00601  CHECK-IF-GVL-TABULAR-EXIST.                                      ELSPRCTL
00602      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELSPRCTL
00603      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00604         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                   ELSPRCTL
00605                                                                   ELSPRCTL
00606      IF CIA-RC-PTR-NULL                                           ELSPRCTL
00607         SET CIA-STG-GETMAIN TO TRUE                               ELSPRCTL
00608         PERFORM CALL-THE-STORAGE-MANAGER                          ELSPRCTL
00609         SET CIA-GCGRPSPC-DDN TO TRUE                              ELSPRCTL
00610         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSPRCTL
00611            ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                ELSPRCTL
00612                                                                   ELSPRCTL
00613      PERFORM ESTAB-ADDR-OF-ELSKEYSC.                              ELSPRCTL
00614      INITIALIZE KWA-GCGRPSPC-KEY.                                 ELSPRCTL
00615      MOVE SSB-PLAN-CODE TO  KWA-GCG-PLAN-CODE.                    ELSPRCTL
00616      MOVE SSB-GROUP-NUMBER TO KWA-GCG-GROUP-NUMBER.               ELSPRCTL
00617      MOVE SSB-SECTN-NO TO KWA-GCG-SECTION-NUMBER.                 ELSPRCTL
00618      MOVE SSB-PKG-CODE TO KWA-GCG-PKG-CODE.                       ELSPRCTL
00619      MOVE SSB-GRP-FAM-REL-LVL TO KWA-GCG-FAM-REL-LVL.             ELSPRCTL
00620      MOVE SSB-GROUP-EFF-DATE-CEN TO KWA-GCG-EFF-DATE-CENTURY.     ELSPRCTL
00621      COMPUTE IOP-KEY-LEN = LENGTH OF KWA-GCG-PLAN-CODE   +        ELSPRCTL
00622                            LENGTH OF KWA-GCG-GROUP-NUMBER +       ELSPRCTL
00623                            LENGTH OF KWA-GCG-SECTION-NUMBER +     ELSPRCTL
00624                            LENGTH OF KWA-GCG-PKG-CODE       +     ELSPRCTL
00625                            LENGTH OF KWA-GCG-FAM-REL-LVL +        ELSPRCTL
00626                            LENGTH OF KWA-GCG-EFF-DATE-CENTURY.    ELSPRCTL
00627      MOVE KWA-FILE-KEY TO IOP-FILE-KEY.                           ELSPRCTL
00628      PERFORM INITIALIZE-GCGRPSPC-FILE.                            ELSPRCTL
00629                                                                   ELSPRCTL
00630      IF IOP-RC-OK                                                 ELSPRCTL
00631         SET ADDRESS OF GROUP-SPECIFIC-REC TO IOP-REC-PTR          ELSPRCTL
00632      ELSE                                                         ELSPRCTL
00633         PERFORM SIGNAL-NO-GROUP-SECTION.                          ELSPRCTL
00634                                                                   ELSPRCTL
00635      PERFORM GCG-TAB-ID-LOOKUP.                                   ELSPRCTL
00636                                                                   ELSPRCTL
00637  INITIALIZE-GCGRPSPC-FILE.                                        ELSPRCTL
00638      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELSPRCTL
00639      MOVE SPACES TO IOP-AIX-DDNAME.                               ELSPRCTL
00640      SET IOP-RD              TO TRUE.                             ELSPRCTL
00641      SET IOP-FCQ-NONE        TO TRUE.                             ELSPRCTL
00642      SET IOP-KVQ-EQ          TO TRUE.                             ELSPRCTL
00643      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELSPRCTL
00644      PERFORM CALL-INPUT-OUTPUT.                                   ELSPRCTL
00645                                                                   ELSPRCTL
00646  ESTAB-ADDR-OF-ELSKEYSC.                                          ELSPRCTL
00647      PERFORM SET-KWA-ADDRESS.                                     ELSPRCTL
00648      IF NOT CIA-RC-OK                                             ELSPRCTL
00649         PERFORM ALLOCATE-ELSKEYSC.                                ELSPRCTL
00650                                                                   ELSPRCTL
00651  ALLOCATE-ELSKEYSC.                                               ELSPRCTL
00652      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELSPRCTL
00653      MOVE ZERO TO CIA-AREA-LEN.                                   ELSPRCTL
00654      SET CIA-STG-GETMAIN TO TRUE.                                 ELSPRCTL
00655      PERFORM CALL-THE-STORAGE-MANAGER.                            ELSPRCTL
00656      PERFORM SET-KWA-ADDRESS.                                     ELSPRCTL
00657                                                                   ELSPRCTL
00658  SET-KWA-ADDRESS.                                                 ELSPRCTL
00659      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELSPRCTL
00660      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00661           ADDRESS OF KWA-FILE-KEY-WORK-AREA.                      ELSPRCTL
00662                                                                   ELSPRCTL
00663 ************************************************************      ELSPRCTL
00664 *                                                          *      ELSPRCTL
00665 *        GCG TAB ID LOOKUP                                 *      ELSPRCTL
00666 *                                                          *      ELSPRCTL
00667 ************************************************************      ELSPRCTL
00668  GCG-TAB-ID-LOOKUP.                                               ELSPRCTL
00669      IF SSB-SS-GET-PROV-CTL-BAS-INST                              ELSPRCTL
00670      OR SSB-SS-GET-PROV-CTL-SUP-INST                              ELSPRCTL
00671         PERFORM GCG-TAB-ID-INST-LOOKUP                            ELSPRCTL
00672           VARYING WS-GCG-SUB FROM 1 BY 1                          ELSPRCTL
00673             UNTIL WS-GVL-TABULAR-FOUND                            ELSPRCTL
00674             OR WS-GCG-SUB > GCG-COUNT-TAB-PROVN-POINTERS.         ELSPRCTL
00675      IF SSB-SS-GET-PROV-CTL-BAS-PROF                              ELSPRCTL
00676      OR SSB-SS-GET-PROV-CTL-SUP-PROF                              ELSPRCTL
00677         PERFORM GCG-TAB-ID-PROF-LOOKUP                            ELSPRCTL
00678           VARYING WS-GCG-SUB FROM 1 BY 1                          ELSPRCTL
00679             UNTIL WS-GVL-TABULAR-FOUND                            ELSPRCTL
00680             OR WS-GCG-SUB > GCG-COUNT-TAB-PROVN-POINTERS.         ELSPRCTL
00681                                                                   ELSPRCTL
00682                                                                   ELSPRCTL
00683 ************************************************************      ELSPRCTL
00684 *                                                          *      ELSPRCTL
00685 *        GCG TAB ID INST LOOKUP                            *      ELSPRCTL
00686 *                                                          *      ELSPRCTL
00687 ************************************************************      ELSPRCTL
00688  GCG-TAB-ID-INST-LOOKUP.                                          ELSPRCTL
00689      SET GCG-INDEX TO WS-GCG-SUB.                                 ELSPRCTL
00690      IF GCG-TAB-ID (GCG-INDEX) =                                  ELSPRCTL
00691        '#GVLF ' OR '#GVLG ' OR '#GVLH '                           ELSPRCTL
00692           IF GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZEROES             ELSPRCTL
00693              SET WS-GVL-TABULAR-FOUND TO TRUE                     ELSPRCTL
00694      END-IF.                                                      ELSPRCTL
00695                                                                   ELSPRCTL
00696                                                                   ELSPRCTL
00697 ************************************************************      ELSPRCTL
00698 *                                                          *      ELSPRCTL
00699 *        GCG TAB ID INST LOOKUP                            *      ELSPRCTL
00700 *                                                          *      ELSPRCTL
00701 ************************************************************      ELSPRCTL
00702  GCG-TAB-ID-PROF-LOOKUP.                                          ELSPRCTL
00703      SET GCG-INDEX TO WS-GCG-SUB.                                 ELSPRCTL
00704      IF GCG-TAB-ID (GCG-INDEX) =                                  ELSPRCTL
00705        '#GVLP ' OR '#GVLQ ' OR '#GVLR '                           ELSPRCTL
00706         IF GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZEROES               ELSPRCTL
00707            SET WS-GVL-TABULAR-FOUND TO TRUE                       ELSPRCTL
00708      END-IF.                                                      ELSPRCTL
00709                                                                   ELSPRCTL
00710                                                                   ELSPRCTL
00711 ************************************************************      ELSPRCTL
00712 *                                                          *      ELSPRCTL
00713 *        INITIALIZE MENU DESCRIPTIONS                      *      ELSPRCTL
00714 *                                                          *      ELSPRCTL
00715 ************************************************************      ELSPRCTL
00716  INITIALIZE-MENU-DESCRIPTIONS.                                    ELSPRCTL
00717      SET IOP-GETMAIN-REC TO TRUE.                                 ELSPRCTL
00718      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSPRCTL
00719      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00720          ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS.                   ELSPRCTL
00721      COMPUTE IOP-REC-LEN = LENGTH OF MSD-NBR-DESCR-LINES          ELSPRCTL
00722          +                                                        ELSPRCTL
00723                         LENGTH OF MSD-DESCR-LINE *                ELSPRCTL
00724                         CIA-MVO.                                  ELSPRCTL
00725      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSPRCTL
00726      SET CIA-STG-GETMAIN TO TRUE.                                 ELSPRCTL
00727      PERFORM CALL-THE-STORAGE-MANAGER.                            ELSPRCTL
00728      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS                    ELSPRCTL
00729          TO IOP-REC-PTR.                                          ELSPRCTL
00730                                                                   ELSPRCTL
00731                                                                   ELSPRCTL
00732 ************************************************************      ELSPRCTL
00733 *                                                          *      ELSPRCTL
00734 *        FORMAT THE MENU CHOICES                           *      ELSPRCTL
00735 *                                                          *      ELSPRCTL
00736 ************************************************************      ELSPRCTL
00737  FORMAT-THE-MENU-CHOICES.                                         ELSPRCTL
00738      PERFORM DETERMINE-IF-CONTRACT-APPLIES                        ELSPRCTL
00739          VARYING KTC-IDX FROM 1 BY 1                              ELSPRCTL
00740                  UNTIL KTC-IDX > KTC-NBR-KEYS.                    ELSPRCTL
00741                                                                   ELSPRCTL
00742                                                                   ELSPRCTL
00743 ************************************************************      ELSPRCTL
00744 *                                                          *      ELSPRCTL
00745 *        DETERMINE IF CONTRACT APPLIES                     *      ELSPRCTL
00746 *                                                          *      ELSPRCTL
00747 ************************************************************      ELSPRCTL
00748  DETERMINE-IF-CONTRACT-APPLIES.                                   ELSPRCTL
00749      IF KTC-SEL (KTC-IDX, KTC-SEL-IDX)                            ELSPRCTL
00750          PERFORM ADD-TO-MENU.                                     ELSPRCTL
00751      EJECT                                                        ELSPRCTL
00752                                                                   ELSPRCTL
00753                                                                   ELSPRCTL
00754 ************************************************************      ELSPRCTL
00755 *                                                          *      ELSPRCTL
00756 *        ADD TO MENU                                       *      ELSPRCTL
00757 *                                                          *      ELSPRCTL
00758 ************************************************************      ELSPRCTL
00759  ADD-TO-MENU.                                                     ELSPRCTL
00760      PERFORM CHECK-FOR-MENU-ITEM-DUPLICATES.                      ELSPRCTL
00761      IF WS-NO-DUPLICATE-PC                                        ELSPRCTL
00762          PERFORM ACCEPT-THE-ITEM.                                 ELSPRCTL
00763                                                                   ELSPRCTL
00764                                                                   ELSPRCTL
00765 ************************************************************      ELSPRCTL
00766 *                                                          *      ELSPRCTL
00767 *        CHECK FOR MENU ITEM DUPLICATES                    *      ELSPRCTL
00768 *                                                          *      ELSPRCTL
00769 ************************************************************      ELSPRCTL
00770  CHECK-FOR-MENU-ITEM-DUPLICATES.                                  ELSPRCTL
00771      SET MSO-IDX TO 1.                                            ELSPRCTL
00772      SEARCH MSO-MENU-OPT  VARYING MSO-IDX                         ELSPRCTL
00773          AT END                                                   ELSPRCTL
00774               SET WS-NO-DUPLICATE-PC TO TRUE                      ELSPRCTL
00775          WHEN KTC-PROVDR-CONTROL (KTC-IDX) =                      ELSPRCTL
00776               MSO-OPT-SEL (MSO-IDX)                               ELSPRCTL
00777                    SET WS-DUPLICATE-PC TO TRUE.                   ELSPRCTL
00778                                                                   ELSPRCTL
00779                                                                   ELSPRCTL
00780 ************************************************************      ELSPRCTL
00781 *                                                          *      ELSPRCTL
00782 *        ACCEPT THE ITEM                                   *      ELSPRCTL
00783 *                                                          *      ELSPRCTL
00784 ************************************************************      ELSPRCTL
00785  ACCEPT-THE-ITEM.                                                 ELSPRCTL
00786      ADD 1 TO MSO-NBR-MENU-OPTS.                                  ELSPRCTL
00787      SET MSO-IDX TO MSO-NBR-MENU-OPTS.                            ELSPRCTL
00788      MOVE KTC-PROVDR-CONTROL (KTC-IDX)                            ELSPRCTL
00789           TO  MSO-OPT-SEL (MSO-IDX)                               ELSPRCTL
00790               MSO-OPT-KWD (MSO-IDX).                              ELSPRCTL
00791      PERFORM FORMAT-THE-ITEM-DESCRIPTION.                         ELSPRCTL
00792      EJECT                                                        ELSPRCTL
00793                                                                   ELSPRCTL
00794                                                                   ELSPRCTL
00795 ************************************************************      ELSPRCTL
00796 *                                                          *      ELSPRCTL
00797 *        FORMAT THE ITEM DESCRIPTION                       *      ELSPRCTL
00798 *                                                          *      ELSPRCTL
00799 ************************************************************      ELSPRCTL
00800  FORMAT-THE-ITEM-DESCRIPTION.                                     ELSPRCTL
00801      PERFORM GET-DESCRIPTION-OF-THE-CODE.                         ELSPRCTL
00802      PERFORM PREPARE-THE-DETAIL-LINE.                             ELSPRCTL
00803      PERFORM ADD-ITEM-DESCRIPTION-TO-MENU.                        ELSPRCTL
00804                                                                   ELSPRCTL
00805                                                                   ELSPRCTL
00806 ************************************************************      ELSPRCTL
00807 *                                                          *      ELSPRCTL
00808 *        GET DESCRIPTION OF THE CODE                       *      ELSPRCTL
00809 *                                                          *      ELSPRCTL
00810 ************************************************************      ELSPRCTL
00811  GET-DESCRIPTION-OF-THE-CODE.                                     ELSPRCTL
00812      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELSPRCTL
00813      PERFORM REFORMAT-PC-DESCRIPTION.                             ELSPRCTL
00814                                                                   ELSPRCTL
00815                                                                   ELSPRCTL
00816 ************************************************************      ELSPRCTL
00817 *                                                          *      ELSPRCTL
00818 *        CALL CODES MANUAL INTERFACE                       *      ELSPRCTL
00819 *                                                          *      ELSPRCTL
00820 ************************************************************      ELSPRCTL
00821  CALL-CODES-MANUAL-INTERFACE.                                     ELSPRCTL
00822      MOVE 'CONTRACT'             TO CMF-RECORD-PREFIX.            ELSPRCTL
00823      MOVE 'PROVDR-CONTROL'       TO                               ELSPRCTL
00824          CMF-ELEMENT-SYSTEM-NAME.                                 ELSPRCTL
00825      MOVE KTC-PROVDR-CONTROL (KTC-IDX)                            ELSPRCTL
00826                                  TO CMF-CODE-VALUE.               ELSPRCTL
00827      EXEC CICS LINK                                               ELSPRCTL
00828                PROGRAM('ELUCMIF')                                 ELSPRCTL
00829                COMMAREA(DFHCOMMAREA)                              ELSPRCTL
00830                END-EXEC.                                          ELSPRCTL
00831      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELSPRCTL
00832      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRCTL
00833          ADDRESS OF CMF-DESCR.                                    ELSPRCTL
00834      EJECT                                                        ELSPRCTL
00835                                                                   ELSPRCTL
00836                                                                   ELSPRCTL
00837 ************************************************************      ELSPRCTL
00838 *                                                          *      ELSPRCTL
00839 *        REFORMAT PC DESCRIPTION                           *      ELSPRCTL
00840 *                                                          *      ELSPRCTL
00841 ************************************************************      ELSPRCTL
00842  REFORMAT-PC-DESCRIPTION.                                         ELSPRCTL
00843      PERFORM MOVE-RAW-TEXT                                        ELSPRCTL
00844          VARYING TCAR-TO-SUB FROM 1 BY 1                          ELSPRCTL
00845                  UNTIL TCAR-TO-SUB > CMF-NBR-DESCR-LINES          ELSPRCTL
00846                     OR TCAR-TO-SUB > 20.                          ELSPRCTL
00847      COMPUTE TCAR-AREA-LENGTH = (TCAR-TO-SUB - 1) *               ELSPRCTL
00848              LENGTH TCAR-FROM-LINE.                               ELSPRCTL
00849      MOVE CIA-MVO   TO  TCAR-OUTPUT-FIELD-COUNT.                  ELSPRCTL
00850      MOVE LENGTH OF WS-PC-DESCRIPTION TO                          ELSPRCTL
00851                  TCAR-OUTPUT-FIELD-1-LEN                          ELSPRCTL
00852                  TCAR-OUTPUT-FIELD-2-LEN                          ELSPRCTL
00853                  TCAR-OUTPUT-FIELD-3-LEN.                         ELSPRCTL
00854      PERFORM TEXT-FORMAT.                                         ELSPRCTL
00855                                                                   ELSPRCTL
00856                                                                   ELSPRCTL
00857 ************************************************************      ELSPRCTL
00858 *                                                          *      ELSPRCTL
00859 *        MOVE RAW TEXT                                     *      ELSPRCTL
00860 *                                                          *      ELSPRCTL
00861 ************************************************************      ELSPRCTL
00862  MOVE-RAW-TEXT.                                                   ELSPRCTL
00863      MOVE CMF-DESCR-LINE (TCAR-TO-SUB)                            ELSPRCTL
00864           TO TCAR-FROM-LINE (TCAR-TO-SUB).                        ELSPRCTL
00865      EJECT                                                        ELSPRCTL
00866                                                                   ELSPRCTL
00867                                                                   ELSPRCTL
00868 ************************************************************      ELSPRCTL
00869 *                                                          *      ELSPRCTL
00870 *        PREPARE THE DETAIL LINE                           *      ELSPRCTL
00871 *                                                          *      ELSPRCTL
00872 ************************************************************      ELSPRCTL
00873  PREPARE-THE-DETAIL-LINE.                                         ELSPRCTL
00874      MOVE SPACES TO WS-PC-DETAIL.                                 ELSPRCTL
00875      MOVE KTC-PROVDR-CONTROL (KTC-IDX)                            ELSPRCTL
00876          TO WS-PC-CODE.                                           ELSPRCTL
00877      MOVE TCAR-OUTPUT-FIELDS-USED                                 ELSPRCTL
00878          TO MSD-NBR-DESCR-LINES.                                  ELSPRCTL
00879      PERFORM MOVE-FORMATTED-TEXT                                  ELSPRCTL
00880          VARYING TCAR-TO-SUB FROM 1 BY 1                          ELSPRCTL
00881                  UNTIL TCAR-TO-SUB >                              ELSPRCTL
00882              TCAR-OUTPUT-FIELDS-USED.                             ELSPRCTL
00883                                                                   ELSPRCTL
00884                                                                   ELSPRCTL
00885 ************************************************************      ELSPRCTL
00886 *                                                          *      ELSPRCTL
00887 *        MOVE FORMATTED TEXT                               *      ELSPRCTL
00888 *                                                          *      ELSPRCTL
00889 ************************************************************      ELSPRCTL
00890  MOVE-FORMATTED-TEXT.                                             ELSPRCTL
00891      MOVE TCAR-OPF-DATA (TCAR-TO-SUB)                             ELSPRCTL
00892           TO WS-PC-DESCRIPTION.                                   ELSPRCTL
00893      MOVE WS-PC-DETAIL                                            ELSPRCTL
00894           TO MSD-DESCR-LINE (TCAR-TO-SUB).                        ELSPRCTL
00895      MOVE SPACES TO WS-PC-CODE.                                   ELSPRCTL
00896      EJECT                                                        ELSPRCTL
00897                                                                   ELSPRCTL
00898                                                                   ELSPRCTL
00899 ************************************************************      ELSPRCTL
00900 *                                                          *      ELSPRCTL
00901 *        ADD ITEM DESCRIPTION TO MENU                      *      ELSPRCTL
00902 *                                                          *      ELSPRCTL
00903 ************************************************************      ELSPRCTL
00904  ADD-ITEM-DESCRIPTION-TO-MENU.                                    ELSPRCTL
00905      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSPRCTL
00906      SET IOP-ADD TO TRUE.                                         ELSPRCTL
00907      SET IOP-FCQ-NONE TO TRUE.                                    ELSPRCTL
00908      PERFORM CALL-INPUT-OUTPUT.                                   ELSPRCTL
00909      EJECT                                                        ELSPRCTL
00910                                                                   ELSPRCTL
00911                                                                   ELSPRCTL
00912 ************************************************************      ELSPRCTL
00913 *                                                          *      ELSPRCTL
00914 *        BEGIN MENU MODE                                   *      ELSPRCTL
00915 *                                                          *      ELSPRCTL
00916 ************************************************************      ELSPRCTL
00917  BEGIN-MENU-MODE.                                                 ELSPRCTL
00918      SET SSB-START-MENU (SSB-SELECTOR-STATE)                      ELSPRCTL
00919          TO TRUE.                                                 ELSPRCTL
00920      EJECT                                                        ELSPRCTL
00921                                                                   ELSPRCTL
00922                                                                   ELSPRCTL
00923 ************************************************************      ELSPRCTL
00924 *                                                          *      ELSPRCTL
00925 *        PROCESS RESPONSE                                  *      ELSPRCTL
00926 *                                                          *      ELSPRCTL
00927 ************************************************************      ELSPRCTL
00928  PROCESS-RESPONSE.                                                ELSPRCTL
00929      IF SSB-SS-GET-PROV-CTL-BAS-INST                              ELSPRCTL
00930          PERFORM PROCESS-BASIC-INST                               ELSPRCTL
00931      ELSE IF SSB-SS-GET-PROV-CTL-SUP-INST                         ELSPRCTL
00932          PERFORM PROCESS-SUPPL-INST                               ELSPRCTL
00933      ELSE IF SSB-SS-GET-PROV-CTL-BAS-PROF                         ELSPRCTL
00934          PERFORM PROCESS-BASIC-PROF                               ELSPRCTL
00935      ELSE IF SSB-SS-GET-PROV-CTL-SUP-PROF                         ELSPRCTL
00936          PERFORM PROCESS-SUPPL-PROF                               ELSPRCTL
00937      ELSE                                                         ELSPRCTL
00938          PERFORM SYSTEM-LOGIC-ERROR.                              ELSPRCTL
00939      PERFORM COMPLETE-SELECTION.                                  ELSPRCTL
00940      EJECT                                                        ELSPRCTL
00941                                                                   ELSPRCTL
00942                                                                   ELSPRCTL
00943 ************************************************************      ELSPRCTL
00944 *                                                          *      ELSPRCTL
00945 *        PROCESS BASIC INST                                *      ELSPRCTL
00946 *                                                          *      ELSPRCTL
00947 ************************************************************      ELSPRCTL
00948  PROCESS-BASIC-INST.                                              ELSPRCTL
00949      MOVE SSB-MNU-CHOICE (1) TO                                   ELSPRCTL
00950           SSB-INST-BAS-PROVDR-CONTROL.                            ELSPRCTL
00951      EJECT                                                        ELSPRCTL
00952                                                                   ELSPRCTL
00953                                                                   ELSPRCTL
00954 ************************************************************      ELSPRCTL
00955 *                                                          *      ELSPRCTL
00956 *        PROCESS SUPPL INST                                *      ELSPRCTL
00957 *                                                          *      ELSPRCTL
00958 ************************************************************      ELSPRCTL
00959  PROCESS-SUPPL-INST.                                              ELSPRCTL
00960      MOVE SSB-MNU-CHOICE (1) TO                                   ELSPRCTL
00961           SSB-INST-SUP-PROVDR-CONTROL.                            ELSPRCTL
00962      EJECT                                                        ELSPRCTL
00963                                                                   ELSPRCTL
00964                                                                   ELSPRCTL
00965 ************************************************************      ELSPRCTL
00966 *                                                          *      ELSPRCTL
00967 *        PROCESS BASIC PROF                                *      ELSPRCTL
00968 *                                                          *      ELSPRCTL
00969 ************************************************************      ELSPRCTL
00970  PROCESS-BASIC-PROF.                                              ELSPRCTL
00971      MOVE SSB-MNU-CHOICE (1) TO                                   ELSPRCTL
00972           SSB-PROF-BAS-PROVDR-CONTROL.                            ELSPRCTL
00973      EJECT                                                        ELSPRCTL
00974                                                                   ELSPRCTL
00975                                                                   ELSPRCTL
00976 ************************************************************      ELSPRCTL
00977 *                                                          *      ELSPRCTL
00978 *        PROCESS SUPPL PROF                                *      ELSPRCTL
00979 *                                                          *      ELSPRCTL
00980 ************************************************************      ELSPRCTL
00981  PROCESS-SUPPL-PROF.                                              ELSPRCTL
00982      MOVE SSB-MNU-CHOICE (1) TO                                   ELSPRCTL
00983           SSB-PROF-SUP-PROVDR-CONTROL.                            ELSPRCTL
00984      EJECT                                                        ELSPRCTL
00985                                                                   ELSPRCTL
00986                                                                   ELSPRCTL
00987 ************************************************************      ELSPRCTL
00988 *                                                          *      ELSPRCTL
00989 *        COMPLETE SELECTION                                *      ELSPRCTL
00990 *                                                          *      ELSPRCTL
00991 ************************************************************      ELSPRCTL
00992  COMPLETE-SELECTION.                                              ELSPRCTL
00993      SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.              ELSPRCTL
00994      EJECT                                                        ELSPRCTL
00995                                                                   ELSPRCTL
00996                                                                   ELSPRCTL
00997 ************************************************************      ELSPRCTL
00998 *                                                          *      ELSPRCTL
00999 *        CALL THE STORAGE MANAGER                          *      ELSPRCTL
01000 *                                                          *      ELSPRCTL
01001 ************************************************************      ELSPRCTL
01002  CALL-THE-STORAGE-MANAGER.                                        ELSPRCTL
01003      EXEC CICS LINK                                               ELSPRCTL
01004                PROGRAM('ELUSTGMG')                                ELSPRCTL
01005                COMMAREA(DFHCOMMAREA)                              ELSPRCTL
01006                END-EXEC.                                          ELSPRCTL
01007                                                                   ELSPRCTL
01008                                                                   ELSPRCTL
01009 ************************************************************      ELSPRCTL
01010 *                                                          *      ELSPRCTL
01011 *        INVALID COMMAREA ABEND                            *      ELSPRCTL
01012 *                                                          *      ELSPRCTL
01013 ************************************************************      ELSPRCTL
01014  INVALID-COMMAREA-ABEND.                                          ELSPRCTL
01015      SET CIA-AB-DFHCOMMAREA TO TRUE.                              ELSPRCTL
01016      EXEC CICS ABEND                                              ELSPRCTL
01017                ABCODE(CIA-ABCODE)                                 ELSPRCTL
01018                END-EXEC.                                          ELSPRCTL
01019      EJECT                                                        ELSPRCTL
01020                                                                   ELSPRCTL
01021                                                                   ELSPRCTL
01022 ************************************************************      ELSPRCTL
01023 *                                                          *      ELSPRCTL
01024 *        SYSTEM LOGIC ERROR                                *      ELSPRCTL
01025 *                                                          *      ELSPRCTL
01026 ************************************************************      ELSPRCTL
01027  SYSTEM-LOGIC-ERROR.                                              ELSPRCTL
01028      SET CIA-AB-UNDEF TO TRUE.                                    ELSPRCTL
01029      EXEC CICS ABEND                                              ELSPRCTL
01030                ABCODE(CIA-ABCODE)                                 ELSPRCTL
01031                END-EXEC.                                          ELSPRCTL
01032                                                                   ELSPRCTL
01033                                                                   ELSPRCTL
01034 ************************************************************      ELSPRCTL
01035 *                                                          *      ELSPRCTL
01036 *        SIGNAL NO GROUP SECTION                           *      ELSPRCTL
01037 *                                                          *      ELSPRCTL
01038 ************************************************************      ELSPRCTL
01039  SIGNAL-NO-GROUP-SECTION.                                         ELSPRCTL
01040      SET CIA-AB-NOTFND-GRP-SECT TO TRUE.                          ELSPRCTL
01041      EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.               ELSPRCTL
01042                                                                   ELSPRCTL
01043                                                                   ELSPRCTL
01044 ************************************************************      ELSPRCTL
01045 *                                                          *      ELSPRCTL
01046 *        CALL INPUT OUTPUT                                 *      ELSPRCTL
01047 *                                                          *      ELSPRCTL
01048 ************************************************************      ELSPRCTL
01049  CALL-INPUT-OUTPUT.                                               ELSPRCTL
01050      EXEC CICS LINK PROGRAM  ('ELUIOPGM')                         ELSPRCTL
01051                     COMMAREA (DFHCOMMAREA)                        ELSPRCTL
01052      END-EXEC.                                                    ELSPRCTL
01053                                                                   ELSPRCTL
01054                                                                   ELSPRCTL
01055 ************************************************************      ELSPRCTL
01056 *                                                          *      ELSPRCTL
01057 *        TEXT FORMAT                                       *      ELSPRCTL
01058 *                                                          *      ELSPRCTL
01059 ************************************************************      ELSPRCTL
01060  TEXT-FORMAT.                                                     ELSPRCTL
01061      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELSPRCTL
01062      PERFORM TCPR-000-TEXT-UNSTRING.                              ELSPRCTL
01063      COPY ELSTCOMP.                                               ELSPRCTL
