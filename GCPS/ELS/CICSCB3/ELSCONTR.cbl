00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSCONTR
00003  PROGRAM-ID.         ELSCONTR.                                       LV002
00004                                                                   ELSCONTR
00005  AUTHOR.             NINA CERVANTES.                              ELSCONTR
00006                                                                   ELSCONTR
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSCONTR
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSCONTR
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSCONTR
00010                      233 N. MICHIGAN AVE                          ELSCONTR
00011                      CHICAGO, ILLINOIS 60601                      ELSCONTR
00012                                                                   ELSCONTR
00013  DATE-WRITTEN.       05-NOV-1986.                                 ELSCONTR
00014                                                                   ELSCONTR
00015  DATE-COMPILED.                                                   ELSCONTR
00016                                                                   ELSCONTR
00017  SECURITY.           COPYRIGHT 1986,                              ELSCONTR
00018                      HEALTH CARE SERVICE CORPORATION              ELSCONTR
00019      SKIP3                                                        ELSCONTR
00020  ENVIRONMENT DIVISION.                                            ELSCONTR
00021                                                                   ELSCONTR
00022  CONFIGURATION SECTION.                                           ELSCONTR
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELSCONTR
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELSCONTR
00025 ******************************************************************ELSCONTR
00026 *                                                                *ELSCONTR
00027 *                      MAINTENANCE HISTORY                       *ELSCONTR
00028 *                                                                *ELSCONTR
00029 *  MOD     DATE     BY  DRPT                ACTION               *ELSCONTR
00030 * ----- ----------- --- ----- ---------------------------------- *ELSCONTR
00031 * 01.00 05-NOV-1986 NAC       CREATED                            *ELSCONTR
00032 *                                                                *ELSCONTR
00033 * 01.01 31-MAR-1989 NAC       DESTRUCT CONVERSION USING STRUCTURE*ELSCONTR
00034 *                             USING VER: 3.5.                    *ELSCONTR
00035 * 01.02 31-AUG-1989 EGL       STORAGE MANAGEMENT CHANGES         *ELSCONTR
00036 *                             ALSO CORRECTED FAWLTY LOGIC FOR    *ELSCONTR
00037 *                             ASSEMBLING THE SCREEN, WHICH       *ELSCONTR
00038 *                             RESULTED IN MEANINGLESS LOGIC.     *ELSCONTR
00039 * 01.03 03-NOV-1997 AKK       ADD SUPPORT FOR YEAR 2000          *ELSCONTR
00040 *                             AND TX MERGER.                      ELSCONTR
00041 *                                                                 ELSCONTR
00042 *       08-AUG-2003 AKK       REGEN'D FOR ORDER OF COMPILE       *ELSCONTR
00043 ******************************************************************ELSCONTR
00044  DATA DIVISION.                                                   ELSCONTR
00045                                                                   ELSCONTR
00046  FILE SECTION.                                                    ELSCONTR
00047                                                                   ELSCONTR
00048  WORKING-STORAGE SECTION.                                         ELSCONTR
00049  77  FILLER                  PIC X(29)   VALUE                    ELSCONTR
00050      '***ELSCONTR WS BEGINS HERE***'.                             ELSCONTR
00051                                                                   ELSCONTR
00052  01  WS-REFORMAT-VALUE.                                           ELSCONTR
00053      03  WS-OPT-EFF          PIC S9(7)   COMP-3.                  ELSCONTR
00054      03  WS-OPT-TERMN        PIC S9(7)   COMP-3.                  ELSCONTR
00055      03  FILLER              PIC X(10)  VALUE SPACES.             ELSCONTR
00056                                                                   ELSCONTR
00057  01  SUB1                    PIC S9(4) COMP-3 VALUE +0.           ELSCONTR
00058  01  WS-MOVE-SUB             PIC S9(4) COMP SYNC VALUE +0.        ELSCONTR
00059  01  WS-MAX-TCAR-LINES       PIC S9(4) COMP SYNC VALUE +20.       ELSCONTR
00060  01  WS-MENU-TITLE1          PIC X(33)   VALUE                    ELSCONTR
00061      'CONTRACT EFFECTIVE DATE SELECTION'.                         ELSCONTR
00062  01  WS-MENU-TITLE2          PIC X(47)   VALUE                    ELSCONTR
00063      'INSTITUTIONAL CONTRACT EFFECTIVE DATE SELECTION'.           ELSCONTR
00064  01  WS-MENU-TITLE3          PIC X(46)   VALUE                    ELSCONTR
00065      'PROFESSIONAL CONTRACT EFFECTIVE DATE SELECTION'.            ELSCONTR
00066                                                                   ELSCONTR
00067  01  MAX-HEADER-LNS          PIC S9(4)   COMP VALUE +6.           ELSCONTR
00068  01  WS-HDR-PHRASE1.                                              ELSCONTR
00069      03  WS-HDR-PHRASE1-A    PIC X(74)        VALUE               ELSCONTR
00070      'BENEFITS FOR THIS GROUP/SECTION VARY ACCORDING TO THE SPECIFELSCONTR
00071 -    'IC SERVICE OR '.                                            ELSCONTR
00072      03  WS-HDR-PHRASE1-B    PIC X(74)        VALUE               ELSCONTR
00073      'ADMISSION DATE.  SELECT THE DATE RANGE YOU WANT TO SEE FROM ELSCONTR
00074 -    'THE LIST BELOW'.                                            ELSCONTR
00075  01  WS-HDR-PHRASE2.                                              ELSCONTR
00076      03  WS-HDR-PHRASE2-A    PIC X(24)        VALUE               ELSCONTR
00077      '  CONTRACT LEVEL IS FOR '.                                  ELSCONTR
00078  01  MAX-DETAIL-LNS          PIC S9(4)   COMP VALUE +3.           ELSCONTR
00079  01  WS-DTL-LN.                                                   ELSCONTR
00080      03  WS-DTL-LINE.                                             ELSCONTR
00081              05  WS-DTL-RANGE-NO     PIC 999.                     ELSCONTR
00082              05  FILLER              PIC X    VALUE SPACE.        ELSCONTR
00083              05  WS-DTL-FROM-DT      PIC 99/99/99.                ELSCONTR
00084              05  FILLER              PIC XXX  VALUE ' - '.        ELSCONTR
00085              05  WS-DTL-TO-DT        PIC 99/99/99.                ELSCONTR
00086              05  FILLER              PIC X(2) VALUE SPACES.       ELSCONTR
00087              05  WS-DTL-DESCRIP1     PIC X(54).                   ELSCONTR
00088                                                                   ELSCONTR
00089  01  WS-NOT-VIEWABLE-MSG.                                         ELSCONTR
00090      05  FILLER                      PIC X(33) VALUE              ELSCONTR
00091          '*** NOT AVAILABLE FOR VIEWING ***'.                     ELSCONTR
00092      05  FILLER                      PIC X(17) VALUE SPACES.      ELSCONTR
00093                                                                   ELSCONTR
00094  01  WS-HGADATES-PARMS.                                           ELSCONTR
00095      COPY HGCDAT01.                                               ELSCONTR
00096 /                                                                 ELSCONTR
00097  LINKAGE SECTION.                                                 ELSCONTR
00098                                                                   ELSCONTR
00099  01  DFHCOMMAREA.                                                 ELSCONTR
00100      COPY ELSCOMMC.                                               ELSCONTR
00101 /                                                                 ELSCONTR
00102      COPY ELSCIA2C.                                               ELSCONTR
00103 /                                                                 ELSCONTR
00104      COPY ELSSSCBC.                                               ELSCONTR
00105 /                                                                 ELSCONTR
00106      COPY ELSIOPMC.                                               ELSCONTR
00107 /                                                                 ELSCONTR
00108      COPY ELSMHDGC.                                               ELSCONTR
00109 /                                                                 ELSCONTR
00110      COPY ELSMOPTC.                                               ELSCONTR
00111 /                                                                 ELSCONTR
00112      COPY ELSMENUC.                                               ELSCONTR
00113 /                                                                 ELSCONTR
00114      COPY ELSKTBCC.                                               ELSCONTR
00115 /                                                                 ELSCONTR
00116      COPY ELSTCWAC.                                               ELSCONTR
00117 /                                                                 ELSCONTR
00118      COPY ELSCMDSC.                                               ELSCONTR
00119 /                                                                 ELSCONTR
00120      COPY ELSCMIFC.                                               ELSCONTR
00121 /                                                                 ELSCONTR
00122  PROCEDURE DIVISION.                                              ELSCONTR
00123      PERFORM INITIALIZATION.                                      ELSCONTR
00124      PERFORM DETERMINE-MODULE-STATUS.                             ELSCONTR
00125      GOBACK.                                                      ELSCONTR
00126                                                                   ELSCONTR
00127                                                                   ELSCONTR
00128 ************************************************************      ELSCONTR
00129 *                                                          *      ELSCONTR
00130 *        INITIALIZATION                                    *      ELSCONTR
00131 *                                                          *      ELSCONTR
00132 ************************************************************      ELSCONTR
00133  INITIALIZATION.                                                  ELSCONTR
00134      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELSCONTR
00135          EXEC CICS ABEND                                          ELSCONTR
00136                    ABCODE('EL01')                                 ELSCONTR
00137                    END-EXEC.                                      ELSCONTR
00138      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSCONTR
00139                 ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.         ELSCONTR
00140      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSCONTR
00141      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCONTR
00142                 ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.           ELSCONTR
00143                                                                   ELSCONTR
00144                                                                   ELSCONTR
00145 ************************************************************      ELSCONTR
00146 *                                                          *      ELSCONTR
00147 *        DETERMINE MODULE STATUS                           *      ELSCONTR
00148 *                                                          *      ELSCONTR
00149 ************************************************************      ELSCONTR
00150  DETERMINE-MODULE-STATUS.                                         ELSCONTR
00151      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSCONTR
00152          PERFORM INITIAL-CALL-FOR-CONTRACT-EFFE                   ELSCONTR
00153      ELSE IF SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)               ELSCONTR
00154          PERFORM COMPLETE-PROCESS-FOR-CONTRACTX                   ELSCONTR
00155      ELSE                                                         ELSCONTR
00156          SET CIA-AB-UNDEF TO TRUE                                 ELSCONTR
00157          EXEC CICS ABEND                                          ELSCONTR
00158                    ABCODE(CIA-ABCODE)                             ELSCONTR
00159                    END-EXEC.                                      ELSCONTR
00160 /***********************************************************      ELSCONTR
00161 *                                                          *      ELSCONTR
00162 *        INITIAL CALL FOR CONTRACT EFFECTIVE DATE          *      ELSCONTR
00163 *                                                          *      ELSCONTR
00164 ************************************************************      ELSCONTR
00165  INITIAL-CALL-FOR-CONTRACT-EFFE.                                  ELSCONTR
00166      PERFORM PREPARE-STORAGE-AREAS.                               ELSCONTR
00167      PERFORM INITIALIZE-MENU-TABLES.                              ELSCONTR
00168      PERFORM BUILD-MENU.                                          ELSCONTR
00169      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSCONTR
00170                                                                   ELSCONTR
00171                                                                   ELSCONTR
00172 ************************************************************      ELSCONTR
00173 *                                                          *      ELSCONTR
00174 *        PREPARE STORAGE AREAS                             *      ELSCONTR
00175 *                                                          *      ELSCONTR
00176 ************************************************************      ELSCONTR
00177  PREPARE-STORAGE-AREAS.                                           ELSCONTR
00178      PERFORM PURGE-ELSMENU-AREA.                                  ELSCONTR
00179                                                                   ELSCONTR
00180      SET CIA-ELSKTBC-DDN TO TRUE.                                 ELSCONTR
00181      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCONTR
00182                 ADDRESS OF KTC-GCCONTR-KEY-TABLE.                 ELSCONTR
00183      IF CIA-RC-PTR-NULL                                           ELSCONTR
00184          PERFORM RETRIEVE-CONTRACT-KEYS-TABLE                     ELSCONTR
00185          SET CIA-ELSKTBC-DDN TO TRUE                              ELSCONTR
00186          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELSCONTR
00187                     ADDRESS OF KTC-GCCONTR-KEY-TABLE.             ELSCONTR
00188                                                                   ELSCONTR
00189      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSCONTR
00190      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCONTR
00191                 ADDRESS OF CMF-CODES-MANUAL-INTERFACE.            ELSCONTR
00192      IF CIA-RC-PTR-NULL                                           ELSCONTR
00193          PERFORM ALLOCATE-CODES-MANUAL-INTERFAC                   ELSCONTR
00194          SET CIA-ELSCMIF-DDN TO TRUE                              ELSCONTR
00195          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELSCONTR
00196                     ADDRESS OF CMF-CODES-MANUAL-INTERFACE.        ELSCONTR
00197                                                                   ELSCONTR
00198      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELSCONTR
00199      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCONTR
00200                 ADDRESS OF TCAR-COMPRESSION-WORK-AREA.            ELSCONTR
00201      IF CIA-RC-PTR-NULL                                           ELSCONTR
00202          PERFORM ALLOCATE-TEXT-COMPRESSION-WORK                   ELSCONTR
00203          SET CIA-ELSTCWA-DDN TO TRUE                              ELSCONTR
00204          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELSCONTR
00205                     ADDRESS OF TCAR-COMPRESSION-WORK-AREA.        ELSCONTR
00206 /***********************************************************      ELSCONTR
00207 *                                                          *      ELSCONTR
00208 *        RETRIEVE CONTRACT KEYS TABLE                      *      ELSCONTR
00209 *                                                          *      ELSCONTR
00210 ************************************************************      ELSCONTR
00211  RETRIEVE-CONTRACT-KEYS-TABLE.                                    ELSCONTR
00212      SET CIA-ELSKTBC-DDN   TO  TRUE.                              ELSCONTR
00213      SET CIA-STG-RETRIEVE  TO  TRUE.                              ELSCONTR
00214      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSCONTR
00215                                                                   ELSCONTR
00216                                                                   ELSCONTR
00217 ************************************************************      ELSCONTR
00218 *                                                          *      ELSCONTR
00219 *        ALLOCATE CODES MANUAL INTERFACE AREA              *      ELSCONTR
00220 *                                                          *      ELSCONTR
00221 ************************************************************      ELSCONTR
00222  ALLOCATE-CODES-MANUAL-INTERFAC.                                  ELSCONTR
00223      SET CIA-ELSCMIF-DDN    TO  TRUE.                             ELSCONTR
00224      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSCONTR
00225      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSCONTR
00226                                                                   ELSCONTR
00227                                                                   ELSCONTR
00228 ************************************************************      ELSCONTR
00229 *                                                          *      ELSCONTR
00230 *        ALLOCATE TEXT COMPRESSION WORKAREA                *      ELSCONTR
00231 *                                                          *      ELSCONTR
00232 ************************************************************      ELSCONTR
00233  ALLOCATE-TEXT-COMPRESSION-WORK.                                  ELSCONTR
00234      SET CIA-ELSTCWA-DDN    TO  TRUE.                             ELSCONTR
00235      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSCONTR
00236      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSCONTR
00237                                                                   ELSCONTR
00238                                                                   ELSCONTR
00239 ************************************************************      ELSCONTR
00240 *                                                          *      ELSCONTR
00241 *        PURGE ELSMENU AREA                                *      ELSCONTR
00242 *                                                          *      ELSCONTR
00243 ************************************************************      ELSCONTR
00244  PURGE-ELSMENU-AREA.                                              ELSCONTR
00245      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSCONTR
00246      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCONTR
00247                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELSCONTR
00248      IF CIA-RC-PTR-NULL                                           ELSCONTR
00249          PERFORM ALLOCATE-ELSMENU-IOPARM-BLOCK                    ELSCONTR
00250          SET CIA-ELSMENU-DDN TO TRUE                              ELSCONTR
00251          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELSCONTR
00252                     ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.       ELSCONTR
00253      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSCONTR
00254      SET IOP-DEL TO TRUE.                                         ELSCONTR
00255      SET IOP-FCQ-NONE TO TRUE.                                    ELSCONTR
00256      SET IOP-KVQ-NONE TO TRUE.                                    ELSCONTR
00257      PERFORM LINK-TO-ELUIOPGM.                                    ELSCONTR
00258 /***********************************************************      ELSCONTR
00259 *                                                          *      ELSCONTR
00260 *        ALLOCATE ELSMENU IOPARM BLOCK                     *      ELSCONTR
00261 *                                                          *      ELSCONTR
00262 ************************************************************      ELSCONTR
00263  ALLOCATE-ELSMENU-IOPARM-BLOCK.                                   ELSCONTR
00264      SET CIA-ELSMENU-DDN    TO  TRUE.                             ELSCONTR
00265      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSCONTR
00266      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSCONTR
00267                                                                   ELSCONTR
00268                                                                   ELSCONTR
00269 ************************************************************      ELSCONTR
00270 *                                                          *      ELSCONTR
00271 *        LINK TO STORAGE MANAGER                           *      ELSCONTR
00272 *                                                          *      ELSCONTR
00273 ************************************************************      ELSCONTR
00274  LINK-TO-STORAGE-MANAGER.                                         ELSCONTR
00275      EXEC CICS LINK                                               ELSCONTR
00276                PROGRAM ('ELUSTGMG')                               ELSCONTR
00277                COMMAREA(DFHCOMMAREA)                              ELSCONTR
00278                END-EXEC.                                          ELSCONTR
00279 /***********************************************************      ELSCONTR
00280 *                                                          *      ELSCONTR
00281 *        INITIALIZE MENU TABLES                            *      ELSCONTR
00282 *                                                          *      ELSCONTR
00283 ************************************************************      ELSCONTR
00284  INITIALIZE-MENU-TABLES.                                          ELSCONTR
00285      PERFORM ALLOCATE-MENU-HEADINGS-AREA.                         ELSCONTR
00286      PERFORM ALLOCATE-MENU-SELECTIONS-AREA.                       ELSCONTR
00287      PERFORM ALLOCATE-MENU-DESCRIPTIONS-ARE.                      ELSCONTR
00288      INITIALIZE SSB-MNU-CHOICE (1).                               ELSCONTR
00289                                                                   ELSCONTR
00290                                                                   ELSCONTR
00291 ************************************************************      ELSCONTR
00292 *                                                          *      ELSCONTR
00293 *        ALLOCATE MENU SELECTIONS AREA                     *      ELSCONTR
00294 *                                                          *      ELSCONTR
00295 ************************************************************      ELSCONTR
00296  ALLOCATE-MENU-SELECTIONS-AREA.                                   ELSCONTR
00297      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSCONTR
00298      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER        ELSCONTR
00299          +   (LENGTH OF MSO-MENU-OPT * KTC-NBR-KEYS ).            ELSCONTR
00300      SET CIA-STG-GETMAIN TO TRUE.                                 ELSCONTR
00301      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSCONTR
00302      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSCONTR
00303      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCONTR
00304                 ADDRESS OF MSO-MENU-SELECTION-VALUES.             ELSCONTR
00305                                                                   ELSCONTR
00306                                                                   ELSCONTR
00307 ************************************************************      ELSCONTR
00308 *                                                          *      ELSCONTR
00309 *        ALLOCATE MENU HEADINGS AREA                       *      ELSCONTR
00310 *                                                          *      ELSCONTR
00311 ************************************************************      ELSCONTR
00312  ALLOCATE-MENU-HEADINGS-AREA.                                     ELSCONTR
00313      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSCONTR
00314      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +         ELSCONTR
00315              (MAX-HEADER-LNS  *  LENGTH OF MHD-HDG-LINE).         ELSCONTR
00316      SET CIA-STG-GETMAIN TO TRUE.                                 ELSCONTR
00317      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSCONTR
00318      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSCONTR
00319      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCONTR
00320                 ADDRESS OF MHD-MENU-HEADINGS.                     ELSCONTR
00321 /***********************************************************      ELSCONTR
00322 *                                                          *      ELSCONTR
00323 *        ALLOCATE MENU DESCRIPTIONS AREA                   *      ELSCONTR
00324 *                                                          *      ELSCONTR
00325 ************************************************************      ELSCONTR
00326  ALLOCATE-MENU-DESCRIPTIONS-ARE.                                  ELSCONTR
00327      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSCONTR
00328      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCONTR
00329                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELSCONTR
00330      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSCONTR
00331      COMPUTE IOP-REC-LEN  = LENGTH OF MSD-NBR-DESCR-LINES +       ELSCONTR
00332              (MAX-DETAIL-LNS  * LENGTH OF MSD-DESCR-LINE).        ELSCONTR
00333      SET IOP-GETMAIN-REC TO TRUE.                                 ELSCONTR
00334      SET CIA-STG-GETMAIN TO TRUE.                                 ELSCONTR
00335      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELSCONTR
00336      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSCONTR
00337      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS                    ELSCONTR
00338           TO IOP-REC-PTR.                                         ELSCONTR
00339 /***********************************************************      ELSCONTR
00340 *                                                          *      ELSCONTR
00341 *        BUILD MENU                                        *      ELSCONTR
00342 *                                                          *      ELSCONTR
00343 ************************************************************      ELSCONTR
00344  BUILD-MENU.                                                      ELSCONTR
00345      MOVE LENGTH OF MSO-OPT-NUM-3  TO MSO-OPT-LEN.                ELSCONTR
00346      SET MSO-OPT-TYP-NUM           TO TRUE.                       ELSCONTR
00347                                                                   ELSCONTR
00348      PERFORM SELECT-PROVIDER-CLASS-WORDING.                       ELSCONTR
00349      PERFORM TEST-FOR-PRESENCE-OF-PROVIDERX.                      ELSCONTR
00350      PERFORM CONSTRUCT-MENU-HEADER.                               ELSCONTR
00351                                                                   ELSCONTR
00352      PERFORM CREATE-MENU-DETAIL.                                  ELSCONTR
00353                                                                   ELSCONTR
00354                                                                   ELSCONTR
00355 ************************************************************      ELSCONTR
00356 *                                                          *      ELSCONTR
00357 *        SELECT PROVIDER CLASS WORDING                     *      ELSCONTR
00358 *                                                          *      ELSCONTR
00359 ************************************************************      ELSCONTR
00360  SELECT-PROVIDER-CLASS-WORDING.                                   ELSCONTR
00361      INITIALIZE TCAR-FROM-AREA.                                   ELSCONTR
00362      MOVE ZERO TO TCAR-FROM-SUB.                                  ELSCONTR
00363      EVALUATE TRUE                                                ELSCONTR
00364        WHEN SSB-SS-GET-CONT-EFF-INST-BAS                          ELSCONTR
00365            PERFORM CONSTRUCT-INST-BASICX                          ELSCONTR
00366        WHEN SSB-SS-GET-CONT-EFF-INST-SUP                          ELSCONTR
00367            PERFORM CONSTRUCT-INST-SUPP-W                          ELSCONTR
00368        WHEN SSB-SS-GET-CONT-EFF-PROF-BAS                          ELSCONTR
00369            PERFORM CONSTRUCT-PROF-BASIC-W                         ELSCONTR
00370        WHEN SSB-SS-GET-CONT-EFF-PROF-SUP                          ELSCONTR
00371            PERFORM CONSTRUCT-PROF-SUPP-WO                         ELSCONTR
00372        WHEN OTHER                                                 ELSCONTR
00373            PERFORM UNKNOWN-PARM-ERROR                             ELSCONTR
00374      END-EVALUATE.                                                ELSCONTR
00375 /***********************************************************      ELSCONTR
00376 *                                                          *      ELSCONTR
00377 *        CONSTRUCT MENU HEADER                             *      ELSCONTR
00378 *                                                          *      ELSCONTR
00379 ************************************************************      ELSCONTR
00380  CONSTRUCT-MENU-HEADER.                                           ELSCONTR
00381      IF TCAR-FROM-SUB < WS-MAX-TCAR-LINES                         ELSCONTR
00382          ADD 1 TO TCAR-FROM-SUB                                   ELSCONTR
00383          MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).              ELSCONTR
00384      PERFORM TEXT-COMPRESSION.                                    ELSCONTR
00385      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELSCONTR
00386      MOVE +6          TO TCAR-OUTPUT-FIELD-COUNT.                 ELSCONTR
00387      MOVE +79         TO TCAR-OUTPUT-FIELD-1-LEN                  ELSCONTR
00388                          TCAR-OUTPUT-FIELD-2-LEN                  ELSCONTR
00389                          TCAR-OUTPUT-FIELD-3-LEN                  ELSCONTR
00390                          TCAR-OUTPUT-FIELD-4-LEN                  ELSCONTR
00391                          TCAR-OUTPUT-FIELD-5-LEN                  ELSCONTR
00392                          TCAR-OUTPUT-FIELD-6-LEN.                 ELSCONTR
00393      PERFORM TEXT-UNSTRING.                                       ELSCONTR
00394      MOVE TCAR-OUTPUT-FIELDS-USED TO MHD-NBR-HDG-LINES.           ELSCONTR
00395      PERFORM                                                      ELSCONTR
00396          VARYING WS-MOVE-SUB FROM 1 BY 1                          ELSCONTR
00397            UNTIL WS-MOVE-SUB > TCAR-OUTPUT-FIELDS-USED            ELSCONTR
00398            SET MHD-IDX TO WS-MOVE-SUB                             ELSCONTR
00399            MOVE TCAR-OPF-DATA (WS-MOVE-SUB)                       ELSCONTR
00400              TO MHD-HDG-LINE (MHD-IDX)                            ELSCONTR
00401      END-PERFORM.                                                 ELSCONTR
00402      ADD 1 TO MHD-NBR-HDG-LINES.                                  ELSCONTR
00403      SET MHD-IDX UP BY 1.                                         ELSCONTR
00404      MOVE SPACES TO MHD-HDG-LINE (MHD-IDX).                       ELSCONTR
00405 /***********************************************************      ELSCONTR
00406 *                                                          *      ELSCONTR
00407 *        CONSTRUCT INSTITUTIONAL BASIC WORDING             *      ELSCONTR
00408 *                                                          *      ELSCONTR
00409 ************************************************************      ELSCONTR
00410  CONSTRUCT-INST-BASICX.                                           ELSCONTR
00411      IF SSB-INST-BAS-L-O-B = '4'  AND                             ELSCONTR
00412         SSB-PROF-BAS-L-O-B = '4'  AND                             ELSCONTR
00413         SSB-INST-BAS-PROVDR-CONTROL =                             ELSCONTR
00414              SSB-PROF-BAS-PROVDR-CONTROL                          ELSCONTR
00415                MOVE WS-MENU-TITLE1 TO SSB-MNU-TITLE               ELSCONTR
00416                PERFORM MOVE-WS-HDR-PHRASE1                        ELSCONTR
00417      ELSE                                                         ELSCONTR
00418          MOVE WS-MENU-TITLE2 TO SSB-MNU-TITLE                     ELSCONTR
00419          IF TCAR-FROM-SUB < WS-MAX-TCAR-LINES                     ELSCONTR
00420              ADD 1 TO TCAR-FROM-SUB                               ELSCONTR
00421              IF SSB-INST-BAS-L-O-B = '4'                          ELSCONTR
00422                   MOVE 'INSTITUTIONAL BASIC' TO                   ELSCONTR
00423                          TCAR-FROM-LINE (TCAR-FROM-SUB)           ELSCONTR
00424              ELSE                                                 ELSCONTR
00425                   MOVE 'INSTITUTIONAL' TO                         ELSCONTR
00426                          TCAR-FROM-LINE (TCAR-FROM-SUB)           ELSCONTR
00427              END-IF                                               ELSCONTR
00428              PERFORM MOVE-WS-HDR-PHRASE1                          ELSCONTR
00429          END-IF                                                   ELSCONTR
00430      END-IF.                                                      ELSCONTR
00431                                                                   ELSCONTR
00432                                                                   ELSCONTR
00433 ************************************************************      ELSCONTR
00434 *                                                          *      ELSCONTR
00435 *        CONSTRUCT INSTITUTIONAL SUPP WORDING              *      ELSCONTR
00436 *                                                          *      ELSCONTR
00437 ************************************************************      ELSCONTR
00438  CONSTRUCT-INST-SUPP-W.                                           ELSCONTR
00439      MOVE WS-MENU-TITLE2 TO SSB-MNU-TITLE.                        ELSCONTR
00440      IF TCAR-FROM-SUB < WS-MAX-TCAR-LINES                         ELSCONTR
00441         ADD 1 TO TCAR-FROM-SUB                                    ELSCONTR
00442         MOVE 'INSTITUTIONAL SUPPLEMENTAL ' TO                     ELSCONTR
00443              TCAR-FROM-LINE (TCAR-FROM-SUB)                       ELSCONTR
00444         PERFORM MOVE-WS-HDR-PHRASE1.                              ELSCONTR
00445 /***********************************************************      ELSCONTR
00446 *                                                          *      ELSCONTR
00447 *        CONSTRUCT PROFESSIONAL BASIC WORDING              *      ELSCONTR
00448 *                                                          *      ELSCONTR
00449 ************************************************************      ELSCONTR
00450  CONSTRUCT-PROF-BASIC-W.                                          ELSCONTR
00451      IF SSB-INST-BAS-L-O-B = '4'  AND                             ELSCONTR
00452         SSB-PROF-BAS-L-O-B = '4'  AND                             ELSCONTR
00453         SSB-INST-BAS-PROVDR-CONTROL =                             ELSCONTR
00454            SSB-PROF-BAS-PROVDR-CONTROL                            ELSCONTR
00455                MOVE WS-MENU-TITLE1 TO SSB-MNU-TITLE               ELSCONTR
00456                PERFORM MOVE-WS-HDR-PHRASE1                        ELSCONTR
00457      ELSE                                                         ELSCONTR
00458          MOVE WS-MENU-TITLE3 TO SSB-MNU-TITLE                     ELSCONTR
00459          IF TCAR-FROM-SUB < WS-MAX-TCAR-LINES                     ELSCONTR
00460              ADD 1 TO TCAR-FROM-SUB                               ELSCONTR
00461              IF SSB-PROF-BAS-L-O-B = '4'                          ELSCONTR
00462                  MOVE 'PROFESSIONAL' TO                           ELSCONTR
00463                        TCAR-FROM-LINE (TCAR-FROM-SUB)             ELSCONTR
00464              ELSE                                                 ELSCONTR
00465                  MOVE 'PROFESSIONAL BASIC' TO                     ELSCONTR
00466                        TCAR-FROM-LINE (TCAR-FROM-SUB)             ELSCONTR
00467              END-IF                                               ELSCONTR
00468              PERFORM MOVE-WS-HDR-PHRASE1                          ELSCONTR
00469          END-IF                                                   ELSCONTR
00470      END-IF.                                                      ELSCONTR
00471                                                                   ELSCONTR
00472                                                                   ELSCONTR
00473 ************************************************************      ELSCONTR
00474 *                                                          *      ELSCONTR
00475 *        CONSTRUCT PROFESSIONAL SUPP WORDING               *      ELSCONTR
00476 *                                                          *      ELSCONTR
00477 ************************************************************      ELSCONTR
00478  CONSTRUCT-PROF-SUPP-WO.                                          ELSCONTR
00479      MOVE WS-MENU-TITLE3 TO SSB-MNU-TITLE.                        ELSCONTR
00480      IF TCAR-FROM-SUB < WS-MAX-TCAR-LINES                         ELSCONTR
00481          ADD 1 TO TCAR-FROM-SUB                                   ELSCONTR
00482          MOVE 'PROFESSIONAL SUPPLEMENTAL '                        ELSCONTR
00483            TO TCAR-FROM-LINE (TCAR-FROM-SUB)                      ELSCONTR
00484          PERFORM MOVE-WS-HDR-PHRASE1.                             ELSCONTR
00485 /***********************************************************      ELSCONTR
00486 *                                                          *      ELSCONTR
00487 *        MOVE WS-HDR-PHRASE 1                              *      ELSCONTR
00488 *                                                          *      ELSCONTR
00489 ************************************************************      ELSCONTR
00490  MOVE-WS-HDR-PHRASE1.                                             ELSCONTR
00491      IF TCAR-FROM-SUB < WS-MAX-TCAR-LINES                         ELSCONTR
00492          ADD 1 TO TCAR-FROM-SUB                                   ELSCONTR
00493          MOVE WS-HDR-PHRASE1-A TO                                 ELSCONTR
00494               TCAR-FROM-LINE (TCAR-FROM-SUB)                      ELSCONTR
00495          IF TCAR-FROM-SUB < WS-MAX-TCAR-LINES                     ELSCONTR
00496              ADD 1 TO TCAR-FROM-SUB                               ELSCONTR
00497              MOVE WS-HDR-PHRASE1-B TO                             ELSCONTR
00498                   TCAR-FROM-LINE (TCAR-FROM-SUB).                 ELSCONTR
00499 /***********************************************************      ELSCONTR
00500 *                                                          *      ELSCONTR
00501 *        TEST FOR PRESENCE OF PROVIDER CONTROL             *      ELSCONTR
00502 *                                                          *      ELSCONTR
00503 ************************************************************      ELSCONTR
00504  TEST-FOR-PRESENCE-OF-PROVIDERX.                                  ELSCONTR
00505      IF SSB-SS-GET-CONT-EFF-INST-BAS  AND                         ELSCONTR
00506         SSB-INST-BAS-PROVDR-CONTROL NOT =  ZEROES                 ELSCONTR
00507            PERFORM CONSTRUCT-INST-BASIC-PROV                      ELSCONTR
00508      ELSE                                                         ELSCONTR
00509          IF SSB-SS-GET-CONT-EFF-INST-SUP  AND                     ELSCONTR
00510             SSB-INST-SUP-PROVDR-CONTROL NOT =  ZEROES             ELSCONTR
00511                  PERFORM CONSTRUCT-INST-SUPP-PROV                 ELSCONTR
00512          ELSE                                                     ELSCONTR
00513              IF SSB-SS-GET-CONT-EFF-PROF-BAS AND                  ELSCONTR
00514                 SSB-PROF-BAS-PROVDR-CONTROL NOT =  ZEROES         ELSCONTR
00515                      PERFORM CONSTRUCT-PROF-BASIC-PROV            ELSCONTR
00516              ELSE                                                 ELSCONTR
00517              IF SSB-SS-GET-CONT-EFF-PROF-SUP   AND                ELSCONTR
00518                 SSB-PROF-SUP-PROVDR-CONTROL NOT =  ZEROES         ELSCONTR
00519                      PERFORM CONSTRUCT-PROF-SUPP-PROV.            ELSCONTR
00520                                                                   ELSCONTR
00521                                                                   ELSCONTR
00522  CONSTRUCT-INST-BASIC-PROV.                                       ELSCONTR
00523      MOVE SSB-INST-BAS-PROVDR-CONTROL TO CMF-CODE-VALUE.          ELSCONTR
00524      PERFORM CONSTRUCT-BASIC-PROVIDER-CONTR.                      ELSCONTR
00525                                                                   ELSCONTR
00526  CONSTRUCT-INST-SUPP-PROV.                                        ELSCONTR
00527      MOVE SSB-INST-SUP-PROVDR-CONTROL TO CMF-CODE-VALUE.          ELSCONTR
00528      PERFORM CONSTRUCT-BASIC-PROVIDER-CONTR.                      ELSCONTR
00529                                                                   ELSCONTR
00530  CONSTRUCT-PROF-BASIC-PROV.                                       ELSCONTR
00531      MOVE SSB-PROF-BAS-PROVDR-CONTROL TO CMF-CODE-VALUE.          ELSCONTR
00532      PERFORM CONSTRUCT-BASIC-PROVIDER-CONTR.                      ELSCONTR
00533                                                                   ELSCONTR
00534  CONSTRUCT-PROF-SUPP-PROV.                                        ELSCONTR
00535      MOVE SSB-PROF-SUP-PROVDR-CONTROL TO CMF-CODE-VALUE.          ELSCONTR
00536      PERFORM CONSTRUCT-BASIC-PROVIDER-CONTR.                      ELSCONTR
00537                                                                   ELSCONTR
00538  CONSTRUCT-BASIC-PROVIDER-CONTR.                                  ELSCONTR
00539      PERFORM TRANSLATE-PROVIDER-CONTROL.                          ELSCONTR
00540      ADD 1 TO TCAR-FROM-SUB.                                      ELSCONTR
00541      MOVE WS-HDR-PHRASE2 TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELSCONTR
00542      PERFORM                                                      ELSCONTR
00543          VARYING WS-MOVE-SUB FROM 1 BY 1                          ELSCONTR
00544            UNTIL WS-MOVE-SUB > CMF-NBR-DESCR-LINES                ELSCONTR
00545               OR TCAR-FROM-SUB = WS-MAX-TCAR-LINES                ELSCONTR
00546          ADD 1 TO TCAR-FROM-SUB                                   ELSCONTR
00547          MOVE CMF-DESCR-LINE (WS-MOVE-SUB) TO                     ELSCONTR
00548               TCAR-FROM-LINE (TCAR-FROM-SUB)                      ELSCONTR
00549      END-PERFORM.                                                 ELSCONTR
00550 /***********************************************************      ELSCONTR
00551 *                                                          *      ELSCONTR
00552 *        TRANSLATE PROVIDER CONTROL                        *      ELSCONTR
00553 *                                                          *      ELSCONTR
00554 ************************************************************      ELSCONTR
00555  TRANSLATE-PROVIDER-CONTROL.                                      ELSCONTR
00556      MOVE 'CONTRACT'         TO CMF-RECORD-PREFIX.                ELSCONTR
00557      MOVE 'PROVDR-CONTROL'   TO CMF-ELEMENT-SYSTEM-NAME.          ELSCONTR
00558      EXEC CICS LINK                                               ELSCONTR
00559                PROGRAM ('ELUCMIF')                                ELSCONTR
00560                COMMAREA (DFHCOMMAREA)                             ELSCONTR
00561                END-EXEC.                                          ELSCONTR
00562      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELSCONTR
00563      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCONTR
00564                 ADDRESS OF CMF-DESCR.                             ELSCONTR
00565                                                                   ELSCONTR
00566                                                                   ELSCONTR
00567 ************************************************************      ELSCONTR
00568 *                                                          *      ELSCONTR
00569 *        CREATE MENU DETAIL                                *      ELSCONTR
00570 *                                                          *      ELSCONTR
00571 ************************************************************      ELSCONTR
00572  CREATE-MENU-DETAIL.                                              ELSCONTR
00573      EVALUATE TRUE                                                ELSCONTR
00574      WHEN SSB-SS-GET-CONT-EFF-INST-BAS                            ELSCONTR
00575          SET KTC-SEL-IDX TO 1                                     ELSCONTR
00576      WHEN SSB-SS-GET-CONT-EFF-INST-SUP                            ELSCONTR
00577          SET KTC-SEL-IDX TO 2                                     ELSCONTR
00578      WHEN SSB-SS-GET-CONT-EFF-PROF-BAS                            ELSCONTR
00579          SET KTC-SEL-IDX TO 3                                     ELSCONTR
00580      WHEN SSB-SS-GET-CONT-EFF-PROF-SUP                            ELSCONTR
00581          SET KTC-SEL-IDX TO 4                                     ELSCONTR
00582      WHEN OTHER                                                   ELSCONTR
00583          PERFORM UNKNOWN-PARM-ERROR                               ELSCONTR
00584      END-EVALUATE.                                                ELSCONTR
00585      INITIALIZE SUB1.                                             ELSCONTR
00586      SET MSO-IDX TO SUB1.                                         ELSCONTR
00587      PERFORM                                                      ELSCONTR
00588          VARYING KTC-IDX FROM 1 BY 1                              ELSCONTR
00589                    UNTIL KTC-IDX > KTC-NBR-KEYS                   ELSCONTR
00590          IF KTC-SEL (KTC-IDX, KTC-SEL-IDX) OR                     ELSCONTR
00591             KTC-EXC (KTC-IDX, KTC-SEL-IDX)                        ELSCONTR
00592               PERFORM LOAD-OPTION                                 ELSCONTR
00593          END-IF                                                   ELSCONTR
00594      END-PERFORM.                                                 ELSCONTR
00595      MOVE SUB1   TO MSO-NBR-MENU-OPTS.                            ELSCONTR
00596      MOVE 1      TO MSO-MIN-CHOICES                               ELSCONTR
00597                     MSO-MAX-CHOICES.                              ELSCONTR
00598 /***********************************************************      ELSCONTR
00599 *                                                          *      ELSCONTR
00600 *        LOAD OPTION                                       *      ELSCONTR
00601 *                                                          *      ELSCONTR
00602 ************************************************************      ELSCONTR
00603  LOAD-OPTION.                                                     ELSCONTR
00604      ADD 1 TO SUB1.                                               ELSCONTR
00605      SET MSO-IDX UP BY 1.                                         ELSCONTR
00606      INITIALIZE MSO-OPT-SEL  (MSO-IDX).                           ELSCONTR
00607      MOVE SUB1  TO  MSO-OPT-NUM-3 (MSO-IDX).                      ELSCONTR
00608      MOVE KTC-EFF-DT-CENTURY (KTC-IDX)   TO  WS-OPT-EFF.          ELSCONTR
00609      MOVE KTC-TERM-DT-CENTURY (KTC-IDX) TO  WS-OPT-TERMN.         ELSCONTR
00610      IF KTC-EXC (KTC-IDX, KTC-SEL-IDX)                            ELSCONTR
00611          MOVE HIGH-VALUES  TO MSO-OPT-KWD (MSO-IDX)               ELSCONTR
00612                               MSO-OPT-SEL (MSO-IDX)               ELSCONTR
00613      ELSE                                                         ELSCONTR
00614          MOVE WS-REFORMAT-VALUE  TO  MSO-OPT-KWD (MSO-IDX).       ELSCONTR
00615      PERFORM CREATE-ITEM-DESCRIPTION-PER-SE.                      ELSCONTR
00616                                                                   ELSCONTR
00617                                                                   ELSCONTR
00618 ************************************************************      ELSCONTR
00619 *                                                          *      ELSCONTR
00620 *        CREATE ITEM DESCRIPTION PER SELECTED OCCURRENCE   *      ELSCONTR
00621 *                                                          *      ELSCONTR
00622 ************************************************************      ELSCONTR
00623  CREATE-ITEM-DESCRIPTION-PER-SE.                                  ELSCONTR
00624      MOVE SPACES                  TO WS-DTL-LN.                   ELSCONTR
00625      MOVE SUB1                    TO WS-DTL-RANGE-NO.             ELSCONTR
00626      PERFORM CONVERT-DETAIL-EFFECTIVE-DATE.                       ELSCONTR
00627      PERFORM CONVERT-DETAIL-TERMINATION-DAT.                      ELSCONTR
00628      SET MSD-IDX                 TO 1.                            ELSCONTR
00629      SET MSD-NBR-DESCR-LINES     TO MSD-IDX.                      ELSCONTR
00630      IF KTC-EXC (KTC-IDX, KTC-SEL-IDX)                            ELSCONTR
00631          MOVE WS-NOT-VIEWABLE-MSG    TO WS-DTL-DESCRIP1.          ELSCONTR
00632      MOVE WS-DTL-LN              TO MSD-DESCR-LINE                ELSCONTR
00633          (MSD-IDX).                                               ELSCONTR
00634      PERFORM ADD-ITEM-DESCRIPTION-TO-MENU.                        ELSCONTR
00635 /***********************************************************      ELSCONTR
00636 *                                                          *      ELSCONTR
00637 *        CONVERT DETAIL EFFECTIVE DATE                     *      ELSCONTR
00638 *                                                          *      ELSCONTR
00639 ************************************************************      ELSCONTR
00640  CONVERT-DETAIL-EFFECTIVE-DATE.                                   ELSCONTR
00641      MOVE KTC-EFF-DT (KTC-IDX)    TO HGADATE-JULIAN1.             ELSCONTR
00642      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSCONTR
00643      IF HGADATE-RETURN NOT = '00'                                 ELSCONTR
00644          MOVE ZEROES TO HGADATE-DATE2.                            ELSCONTR
00645      MOVE HGADATE-DATE2 TO WS-DTL-FROM-DT.                        ELSCONTR
00646                                                                   ELSCONTR
00647                                                                   ELSCONTR
00648 ************************************************************      ELSCONTR
00649 *                                                          *      ELSCONTR
00650 *        CONVERT DETAIL TERMINATION DATE                   *      ELSCONTR
00651 *                                                          *      ELSCONTR
00652 ************************************************************      ELSCONTR
00653  CONVERT-DETAIL-TERMINATION-DAT.                                  ELSCONTR
00654      MOVE KTC-TERMN-DT  (KTC-IDX) TO HGADATE-JULIAN1.             ELSCONTR
00655      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSCONTR
00656      IF HGADATE-RETURN NOT = '00'                                 ELSCONTR
00657          MOVE ZEROES TO HGADATE-DATE2.                            ELSCONTR
00658      MOVE HGADATE-DATE2 TO WS-DTL-TO-DT.                          ELSCONTR
00659                                                                   ELSCONTR
00660                                                                   ELSCONTR
00661 ************************************************************      ELSCONTR
00662 *                                                          *      ELSCONTR
00663 *        LINK TO ELUIOPGM                                  *      ELSCONTR
00664 *                                                          *      ELSCONTR
00665 ************************************************************      ELSCONTR
00666  LINK-TO-ELUIOPGM.                                                ELSCONTR
00667      EXEC CICS LINK                                               ELSCONTR
00668                PROGRAM('ELUIOPGM')                                ELSCONTR
00669                COMMAREA(DFHCOMMAREA)                              ELSCONTR
00670                END-EXEC.                                          ELSCONTR
00671 /***********************************************************      ELSCONTR
00672 *                                                          *      ELSCONTR
00673 *        ADD ITEM DESCRIPTION TO MENU                      *      ELSCONTR
00674 *                                                          *      ELSCONTR
00675 ************************************************************      ELSCONTR
00676  ADD-ITEM-DESCRIPTION-TO-MENU.                                    ELSCONTR
00677      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSCONTR
00678      SET IOP-ADD TO TRUE.                                         ELSCONTR
00679      SET IOP-FCQ-NONE TO TRUE.                                    ELSCONTR
00680      SET IOP-KVQ-NONE TO TRUE.                                    ELSCONTR
00681      PERFORM LINK-TO-ELUIOPGM.                                    ELSCONTR
00682                                                                   ELSCONTR
00683                                                                   ELSCONTR
00684 ************************************************************      ELSCONTR
00685 *                                                          *      ELSCONTR
00686 *        COMPLETE PROCESS FOR CONTRACT EFFECTIVE DATE      *      ELSCONTR
00687 *                                                          *      ELSCONTR
00688 ************************************************************      ELSCONTR
00689  COMPLETE-PROCESS-FOR-CONTRACTX.                                  ELSCONTR
00690      MOVE SSB-MNU-CHOICE (1) TO WS-REFORMAT-VALUE.                ELSCONTR
00691      EVALUATE TRUE                                                ELSCONTR
00692      WHEN SSB-SS-GET-CONT-EFF-INST-BAS                            ELSCONTR
00693          PERFORM MOVE-INST-BASIC-EFF-D                            ELSCONTR
00694      WHEN SSB-SS-GET-CONT-EFF-INST-SUP                            ELSCONTR
00695          PERFORM MOVE-INST-SUPP-EFF-DA                            ELSCONTR
00696      WHEN SSB-SS-GET-CONT-EFF-PROF-BAS                            ELSCONTR
00697          PERFORM MOVE-PROF-BASIC-EFF-DA                           ELSCONTR
00698      WHEN SSB-SS-GET-CONT-EFF-PROF-SUP                            ELSCONTR
00699          PERFORM MOVE-PROF-SUPP-EFF-DAT                           ELSCONTR
00700      WHEN OTHER                                                   ELSCONTR
00701          PERFORM UNKNOWN-PARM-ERROR                               ELSCONTR
00702      END-EVALUATE.                                                ELSCONTR
00703      SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.              ELSCONTR
00704                                                                   ELSCONTR
00705  MOVE-INST-BASIC-EFF-D.                                           ELSCONTR
00706      MOVE WS-OPT-EFF             TO  SSB-INST-BAS-EFF-DT-CEN.     ELSCONTR
00707      MOVE WS-OPT-TERMN      TO  SSB-CONT-TERMN-DATE-CEN (1).      ELSCONTR
00708                                                                   ELSCONTR
00709  MOVE-INST-SUPP-EFF-DA.                                           ELSCONTR
00710      MOVE WS-OPT-EFF        TO  SSB-INST-SUP-EFF-DT-CEN.          ELSCONTR
00711      MOVE WS-OPT-TERMN      TO  SSB-CONT-TERMN-DATE-CEN (2).      ELSCONTR
00712                                                                   ELSCONTR
00713  MOVE-PROF-BASIC-EFF-DA.                                          ELSCONTR
00714      MOVE WS-OPT-EFF             TO  SSB-PROF-BAS-EFF-DT-CEN.     ELSCONTR
00715      MOVE WS-OPT-TERMN      TO  SSB-CONT-TERMN-DATE-CEN (3).      ELSCONTR
00716                                                                   ELSCONTR
00717  MOVE-PROF-SUPP-EFF-DAT.                                          ELSCONTR
00718      MOVE WS-OPT-EFF        TO  SSB-PROF-SUP-EFF-DATE-CC.         ELSCONTR
00719      MOVE WS-OPT-TERMN      TO  SSB-CONT-TERMN-DATE-CEN (4).      ELSCONTR
00720 /***********************************************************      ELSCONTR
00721 *                                                          *      ELSCONTR
00722 *        JULIAN TO GREG CONVERSION                         *      ELSCONTR
00723 *                                                          *      ELSCONTR
00724 ************************************************************      ELSCONTR
00725  JULIAN-TO-GREG-CONVERSION.                                       ELSCONTR
00726      MOVE 'CNV'     TO HGADATE-FUNC.                              ELSCONTR
00727      MOVE 'J'       TO HGADATE-FORM1.                             ELSCONTR
00728      MOVE 'M'       TO HGADATE-FORM2.                             ELSCONTR
00729      MOVE ZEROES    TO HGADATE-RETURN                             ELSCONTR
00730                        HGADATE-DATE2.                             ELSCONTR
00731      EXEC CICS LINK PROGRAM('HGADATES')                           ELSCONTR
00732                     COMMAREA(WS-HGADATES-PARMS)                   ELSCONTR
00733                     END-EXEC.                                     ELSCONTR
00734                                                                   ELSCONTR
00735                                                                   ELSCONTR
00736 ************************************************************      ELSCONTR
00737 *                                                          *      ELSCONTR
00738 *        UNKNOWN PARM ERROR                                *      ELSCONTR
00739 *                                                          *      ELSCONTR
00740 ************************************************************      ELSCONTR
00741  UNKNOWN-PARM-ERROR.                                              ELSCONTR
00742      SET CIA-AB-PARM-ERR TO TRUE.                                 ELSCONTR
00743      EXEC CICS ABEND                                              ELSCONTR
00744                ABCODE(CIA-ABCODE)                                 ELSCONTR
00745      END-EXEC.                                                    ELSCONTR
00746                                                                   ELSCONTR
00747                                                                   ELSCONTR
00748 ************************************************************      ELSCONTR
00749 *                                                          *      ELSCONTR
00750 *        TEXT COMPRESSION                                  *      ELSCONTR
00751 *                                                          *      ELSCONTR
00752 ************************************************************      ELSCONTR
00753  TEXT-COMPRESSION.                                                ELSCONTR
00754      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELSCONTR
00755                                                                   ELSCONTR
00756                                                                   ELSCONTR
00757 ************************************************************      ELSCONTR
00758 *                                                          *      ELSCONTR
00759 *        TEXT UNSTRING                                     *      ELSCONTR
00760 *                                                          *      ELSCONTR
00761 ************************************************************      ELSCONTR
00762  TEXT-UNSTRING.                                                   ELSCONTR
00763      PERFORM TCPR-000-TEXT-UNSTRING.                              ELSCONTR
00764      COPY ELSTCOMP.                                               ELSCONTR
