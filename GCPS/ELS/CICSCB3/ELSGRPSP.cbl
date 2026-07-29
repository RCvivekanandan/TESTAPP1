00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSGRPSP
00003  PROGRAM-ID.         ELSGRPSP.                                       LV002
00004                                                                   ELSGRPSP
00005  AUTHOR.             NINA CERVANTES.                              ELSGRPSP
00006                                                                   ELSGRPSP
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSGRPSP
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSGRPSP
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSGRPSP
00010                      233 N. MICHIGAN AVE                          ELSGRPSP
00011                      CHICAGO, ILLINOIS 60601                      ELSGRPSP
00012                                                                   ELSGRPSP
00013  DATE-WRITTEN.       03-NOV-1986.                                 ELSGRPSP
00014                                                                   ELSGRPSP
00015  DATE-COMPILED.                                                   ELSGRPSP
00016                                                                   ELSGRPSP
00017  SECURITY.           COPYRIGHT 1986,                              ELSGRPSP
00018                      HEALTH CARE SERVICE CORPORATION              ELSGRPSP
00019      SKIP3                                                        ELSGRPSP
00020 *                                                                 ELSGRPSP
00021 *   CONVERTED FROM STRUCTURES 04/10/89  EGL                       ELSGRPSP
00022 *                                                                 ELSGRPSP
00023 *   STORAGE MANAGEMENT ENHANCEMENTS 04/12/89  GEM                 ELSGRPSP
00024 *                                                                 ELSGRPSP
00025 *   ADD SUPPORT FOR YR 2000 AND TX MERGER  10/29/97 AKK           ELSGRPSP
00026 *                                                                 ELSGRPSP
00027 *   GEN'D FOR TEST OF ORDER OF COMPILE     08/13/03 AKK           ELSGRPSP
00028 *                                                                 ELSGRPSP
00029  ENVIRONMENT DIVISION.                                            ELSGRPSP
00030                                                                   ELSGRPSP
00031  CONFIGURATION SECTION.                                           ELSGRPSP
00032  SOURCE-COMPUTER.    IBM-3033.                                    ELSGRPSP
00033  OBJECT-COMPUTER.    IBM-3033.                                    ELSGRPSP
00034      EJECT                                                        ELSGRPSP
00035  DATA DIVISION.                                                   ELSGRPSP
00036                                                                   ELSGRPSP
00037  FILE SECTION.                                                    ELSGRPSP
00038                                                                   ELSGRPSP
00039  WORKING-STORAGE SECTION.                                         ELSGRPSP
00040  77  FILLER                  PIC X(29)   VALUE                    ELSGRPSP
00041      '***ELSGRPSP WS BEGINS HERE***'.                             ELSGRPSP
00042                                                                   ELSGRPSP
00043                                                                   ELSGRPSP
00044  01  WS-REFORMAT-VALUE.                                           ELSGRPSP
00045      03  WS-OPT-EFF          PIC S9(7)   COMP-3.                  ELSGRPSP
00046      03  WS-OPT-TERM         PIC S9(7)   COMP-3.                  ELSGRPSP
00047      03  FILLER              PIC X(10)  VALUE SPACES.             ELSGRPSP
00048                                                                   ELSGRPSP
00049  01  SUB1                    PIC S9(4) COMP-3 VALUE +0.           ELSGRPSP
00050  01  WS-MENU-TITLE           PIC X(39)   VALUE                    ELSGRPSP
00051      'GROUP SPECIFIC EFFECTIVE DATE SELECTION'.                   ELSGRPSP
00052                                                                   ELSGRPSP
00053  01  MAX-HEADER-LNS          PIC S9(4)   COMP VALUE +4.           ELSGRPSP
00054  01  WS-HEADINGS.                                                 ELSGRPSP
00055      02  WS-HEADING-LINES.                                        ELSGRPSP
00056          03  WS-HDR-LN1.                                          ELSGRPSP
00057              05 FILLER       PIC X(08)   VALUE 'BETWEEN '.        ELSGRPSP
00058              05  WS-HDR-FROM-DT        PIC 99/99/99.              ELSGRPSP
00059              05  FILLER                PIC X(5)   VALUE           ELSGRPSP
00060                                                 ' AND '.          ELSGRPSP
00061              05  WS-HDR-TO-DT          PIC 99/99/99.              ELSGRPSP
00062              05  FILLER                PIC X(50)    VALUE         ELSGRPSP
00063                  ' CERTAIN TERMS OF THE OVERALL GROUP/SECTION'.   ELSGRPSP
00064          03  WS-HDR-LN2      PIC X(79)   VALUE                    ELSGRPSP
00065                  'COVERAGE CHANGED.  PLEASE SELECT THE SPECIFIC DAELSGRPSP
00066 -                'TE RANGE FROM THE LIST BELOW.'.                 ELSGRPSP
00067          03  WS-HDR-LN3      PIC X(79)   VALUE  SPACES.           ELSGRPSP
00068          03  WS-HDR-LN4      PIC X(79)   VALUE                    ELSGRPSP
00069              '      FROM      TO    LINE OF BUSINESS'.            ELSGRPSP
00070      02  WS-HDR-LN  REDEFINES  WS-HEADING-LINES   PIC X(79)       ELSGRPSP
00071                      OCCURS 4 TIMES.                              ELSGRPSP
00072  01  MAX-DETAIL-LNS          PIC S9(4)   COMP VALUE +3.           ELSGRPSP
00073  01  WS-DTL-LN.                                                   ELSGRPSP
00074          03  WS-DTL-LN1.                                          ELSGRPSP
00075              05  WS-DTL-RANGE-NO     PIC 999.                     ELSGRPSP
00076              05  FILLER              PIC X    VALUE SPACE.        ELSGRPSP
00077              05  WS-DTL-FROM-DT      PIC 99/99/99.                ELSGRPSP
00078              05  FILLER              PIC X    VALUE SPACE.        ELSGRPSP
00079              05  WS-DTL-TO-DT        PIC 99/99/99.                ELSGRPSP
00080              05  FILLER              PIC X    VALUE SPACES.       ELSGRPSP
00081              05  WS-DTL-DESCRIP1     PIC X(57).                   ELSGRPSP
00082                                                                   ELSGRPSP
00083  01  WS-NOT-VIEWABLE-MSG.                                         ELSGRPSP
00084      05  FILLER                      PIC X(33) VALUE              ELSGRPSP
00085          '*** NOT AVAILABLE FOR VIEWING ***'.                     ELSGRPSP
00086      05  FILLER                      PIC X(17) VALUE SPACES.      ELSGRPSP
00087                                                                   ELSGRPSP
00088  01  WS-HGADATES-PARMS.                                           ELSGRPSP
00089      COPY HGCDAT01.                                               ELSGRPSP
00090 /                                                                 ELSGRPSP
00091  LINKAGE SECTION.                                                 ELSGRPSP
00092                                                                   ELSGRPSP
00093  01  DFHCOMMAREA.                                                 ELSGRPSP
00094      COPY ELSCOMMC.                                               ELSGRPSP
00095 /                                                                 ELSGRPSP
00096      COPY ELSCIA2C.                                               ELSGRPSP
00097 /                                                                 ELSGRPSP
00098      COPY ELSSSCBC.                                               ELSGRPSP
00099 /                                                                 ELSGRPSP
00100      COPY ELSIOPMC.                                               ELSGRPSP
00101 /                                                                 ELSGRPSP
00102      COPY ELSMHDGC.                                               ELSGRPSP
00103 /                                                                 ELSGRPSP
00104      COPY ELSMOPTC.                                               ELSGRPSP
00105 /                                                                 ELSGRPSP
00106      COPY ELSMENUC.                                               ELSGRPSP
00107 /                                                                 ELSGRPSP
00108      COPY ELSKTBGC.                                               ELSGRPSP
00109 /                                                                 ELSGRPSP
00110      COPY ELSCMIFC.                                               ELSGRPSP
00111 /                                                                 ELSGRPSP
00112      COPY ELSCMDSC.                                               ELSGRPSP
00113 /                                                                 ELSGRPSP
00114      EJECT                                                        ELSGRPSP
00115  PROCEDURE DIVISION.                                              ELSGRPSP
00116 ************************************************************      ELSGRPSP
00117 *                                                          *      ELSGRPSP
00118 *                    PROCEDURE DIVISION                    *      ELSGRPSP
00119 *                                                          *      ELSGRPSP
00120 ************************************************************      ELSGRPSP
00121                                                                   ELSGRPSP
00122                                                                   ELSGRPSP
00123 ************************************************************      ELSGRPSP
00124 *                                                          *      ELSGRPSP
00125 *        GROUP SPECIFIC EFFECTIVE DATE SELECTOR            *      ELSGRPSP
00126 *                                                          *      ELSGRPSP
00127 ************************************************************      ELSGRPSP
00128  GROUP-SPECIFIC-EFFECTIVE-DATEX.                                  ELSGRPSP
00129      PERFORM INITIALIZE-MODULE.                                   ELSGRPSP
00130      PERFORM DETERMINE-MODULE-STATUS.                             ELSGRPSP
00131      PERFORM RETURN-TO-CALLER.                                    ELSGRPSP
00132                                                                   ELSGRPSP
00133                                                                   ELSGRPSP
00134 ************************************************************      ELSGRPSP
00135 *                                                          *      ELSGRPSP
00136 *        INITIALIZE MODULE                                 *      ELSGRPSP
00137 *                                                          *      ELSGRPSP
00138 ************************************************************      ELSGRPSP
00139  INITIALIZE-MODULE.                                               ELSGRPSP
00140      PERFORM CHECK-COMMAREA-LENGTH.                               ELSGRPSP
00141      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELSGRPSP
00142                                                                   ELSGRPSP
00143                                                                   ELSGRPSP
00144 ************************************************************      ELSGRPSP
00145 *                                                          *      ELSGRPSP
00146 *        CHECK COMMAREA LENGTH                             *      ELSGRPSP
00147 *                                                          *      ELSGRPSP
00148 ************************************************************      ELSGRPSP
00149  CHECK-COMMAREA-LENGTH.                                           ELSGRPSP
00150      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELSGRPSP
00151          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELSGRPSP
00152                                                                   ELSGRPSP
00153                                                                   ELSGRPSP
00154 ************************************************************      ELSGRPSP
00155 *                                                          *      ELSGRPSP
00156 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELSGRPSP
00157 *                                                          *      ELSGRPSP
00158 ************************************************************      ELSGRPSP
00159  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELSGRPSP
00160      SET CIA-AB-DFHCOMMAREA TO TRUE.                              ELSGRPSP
00161      EXEC CICS ABEND                                              ELSGRPSP
00162                ABCODE(CIA-ABCODE)                                 ELSGRPSP
00163                END-EXEC.                                          ELSGRPSP
00164                                                                   ELSGRPSP
00165                                                                   ELSGRPSP
00166 ************************************************************      ELSGRPSP
00167 *                                                          *      ELSGRPSP
00168 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELSGRPSP
00169 *                                                          *      ELSGRPSP
00170 ************************************************************      ELSGRPSP
00171  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELSGRPSP
00172      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSGRPSP
00173          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSGRPSP
00174                                                                   ELSGRPSP
00175      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSGRPSP
00176      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSP
00177          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSGRPSP
00178      EJECT                                                        ELSGRPSP
00179                                                                   ELSGRPSP
00180                                                                   ELSGRPSP
00181 ************************************************************      ELSGRPSP
00182 *                                                          *      ELSGRPSP
00183 *        DETERMINE MODULE STATUS                           *      ELSGRPSP
00184 *                                                          *      ELSGRPSP
00185 ************************************************************      ELSGRPSP
00186  DETERMINE-MODULE-STATUS.                                         ELSGRPSP
00187      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSGRPSP
00188          PERFORM INITIAL-CALL-FOR-SECTION-MENU                    ELSGRPSP
00189      ELSE IF SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)               ELSGRPSP
00190          PERFORM COMPLETE-PROCESS-FOR-SECTION-M                   ELSGRPSP
00191      ELSE                                                         ELSGRPSP
00192          PERFORM SIGNAL-UNDEFINED-MODULE-STATUS.                  ELSGRPSP
00193      EJECT                                                        ELSGRPSP
00194                                                                   ELSGRPSP
00195                                                                   ELSGRPSP
00196 ************************************************************      ELSGRPSP
00197 *                                                          *      ELSGRPSP
00198 *        SIGNAL UNDEFINED MODULE STATUS                    *      ELSGRPSP
00199 *                                                          *      ELSGRPSP
00200 ************************************************************      ELSGRPSP
00201  SIGNAL-UNDEFINED-MODULE-STATUS.                                  ELSGRPSP
00202      SET CIA-AB-UNDEF TO TRUE.                                    ELSGRPSP
00203      EXEC CICS ABEND                                              ELSGRPSP
00204                ABCODE(CIA-ABCODE)                                 ELSGRPSP
00205                END-EXEC.                                          ELSGRPSP
00206                                                                   ELSGRPSP
00207                                                                   ELSGRPSP
00208 ************************************************************      ELSGRPSP
00209 *                                                          *      ELSGRPSP
00210 *        INITIAL CALL FOR SECTION MENU                     *      ELSGRPSP
00211 *                                                          *      ELSGRPSP
00212 ************************************************************      ELSGRPSP
00213  INITIAL-CALL-FOR-SECTION-MENU.                                   ELSGRPSP
00214      PERFORM PREPARE-STORAGE-AREAS.                               ELSGRPSP
00215      PERFORM BUILD-MENU.                                          ELSGRPSP
00216      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSGRPSP
00217      EJECT                                                        ELSGRPSP
00218                                                                   ELSGRPSP
00219                                                                   ELSGRPSP
00220 ************************************************************      ELSGRPSP
00221 *                                                          *      ELSGRPSP
00222 *        PREPARE STORAGE AREAS                             *      ELSGRPSP
00223 *                                                          *      ELSGRPSP
00224 ************************************************************      ELSGRPSP
00225  PREPARE-STORAGE-AREAS.                                           ELSGRPSP
00226      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSP
00227      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSP
00228          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSGRPSP
00229      IF CIA-RC-PTR-NULL                                           ELSGRPSP
00230          PERFORM ALLOCATE-ELSMENU-IOPARM-AREA.                    ELSGRPSP
00231                                                                   ELSGRPSP
00232      PERFORM PURGE-ELSMENU-AREA.                                  ELSGRPSP
00233                                                                   ELSGRPSP
00234      SET CIA-ELSKTBG-DDN TO TRUE.                                 ELSGRPSP
00235      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSP
00236          ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                       ELSGRPSP
00237      IF CIA-RC-PTR-NULL                                           ELSGRPSP
00238          PERFORM RETRIEVE-GROUP-SPECIFIC-KEYS-T.                  ELSGRPSP
00239                                                                   ELSGRPSP
00240      SET CIA-ELSKTBG-DDN TO TRUE.                                 ELSGRPSP
00241      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSP
00242          ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                       ELSGRPSP
00243      MOVE WS-MENU-TITLE TO SSB-MNU-TITLE.                         ELSGRPSP
00244      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSGRPSP
00245      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSP
00246          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELSGRPSP
00247      IF CIA-RC-PTR-NULL                                           ELSGRPSP
00248         PERFORM ALLOCATE-CODES-MANUAL-INTERFAC.                   ELSGRPSP
00249      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSGRPSP
00250      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSP
00251          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELSGRPSP
00252      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELSGRPSP
00253      PERFORM INITIALIZE-MENU-TABLES.                              ELSGRPSP
00254      EJECT                                                        ELSGRPSP
00255                                                                   ELSGRPSP
00256                                                                   ELSGRPSP
00257 ************************************************************      ELSGRPSP
00258 *                                                          *      ELSGRPSP
00259 *        RETRIEVE GROUP SPECIFIC KEYS TABLE                *      ELSGRPSP
00260 *                                                          *      ELSGRPSP
00261 ************************************************************      ELSGRPSP
00262  RETRIEVE-GROUP-SPECIFIC-KEYS-T.                                  ELSGRPSP
00263      SET CIA-ELSKTBG-DDN   TO  TRUE.                              ELSGRPSP
00264      SET CIA-STG-RETRIEVE  TO  TRUE.                              ELSGRPSP
00265      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSP
00266      EJECT                                                        ELSGRPSP
00267                                                                   ELSGRPSP
00268                                                                   ELSGRPSP
00269 ************************************************************      ELSGRPSP
00270 *                                                          *      ELSGRPSP
00271 *        ALLOCATE ELSMENU IOPARM AREA                      *      ELSGRPSP
00272 *                                                          *      ELSGRPSP
00273 ************************************************************      ELSGRPSP
00274  ALLOCATE-ELSMENU-IOPARM-AREA.                                    ELSGRPSP
00275      SET CIA-ELSMENU-DDN    TO  TRUE.                             ELSGRPSP
00276      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSGRPSP
00277      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSP
00278      EJECT                                                        ELSGRPSP
00279                                                                   ELSGRPSP
00280                                                                   ELSGRPSP
00281 ************************************************************      ELSGRPSP
00282 *                                                          *      ELSGRPSP
00283 *        ALLOCATE CODES MANUAL INTERFACE AREA              *      ELSGRPSP
00284 *                                                          *      ELSGRPSP
00285 ************************************************************      ELSGRPSP
00286  ALLOCATE-CODES-MANUAL-INTERFAC.                                  ELSGRPSP
00287      SET CIA-ELSCMIF-DDN    TO  TRUE.                             ELSGRPSP
00288      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSGRPSP
00289      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSP
00290      EJECT                                                        ELSGRPSP
00291                                                                   ELSGRPSP
00292                                                                   ELSGRPSP
00293 ************************************************************      ELSGRPSP
00294 *                                                          *      ELSGRPSP
00295 *        PURGE ELSMENU AREA                                *      ELSGRPSP
00296 *                                                          *      ELSGRPSP
00297 ************************************************************      ELSGRPSP
00298  PURGE-ELSMENU-AREA.                                              ELSGRPSP
00299      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSP
00300      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSP
00301          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSGRPSP
00302      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSP
00303      SET IOP-DEL TO TRUE.                                         ELSGRPSP
00304      SET IOP-FCQ-NONE TO TRUE.                                    ELSGRPSP
00305      SET IOP-KVQ-NONE TO TRUE.                                    ELSGRPSP
00306      PERFORM LINK-TO-ELUIOPGM.                                    ELSGRPSP
00307                                                                   ELSGRPSP
00308                                                                   ELSGRPSP
00309 ************************************************************      ELSGRPSP
00310 *                                                          *      ELSGRPSP
00311 *        LINK TO STORAGE MANAGER                           *      ELSGRPSP
00312 *                                                          *      ELSGRPSP
00313 ************************************************************      ELSGRPSP
00314  LINK-TO-STORAGE-MANAGER.                                         ELSGRPSP
00315      EXEC CICS LINK                                               ELSGRPSP
00316                PROGRAM ('ELUSTGMG')                               ELSGRPSP
00317                COMMAREA(DFHCOMMAREA)                              ELSGRPSP
00318                END-EXEC.                                          ELSGRPSP
00319      EJECT                                                        ELSGRPSP
00320                                                                   ELSGRPSP
00321                                                                   ELSGRPSP
00322 ************************************************************      ELSGRPSP
00323 *                                                          *      ELSGRPSP
00324 *        INITIALIZE MENU TABLES                            *      ELSGRPSP
00325 *                                                          *      ELSGRPSP
00326 ************************************************************      ELSGRPSP
00327  INITIALIZE-MENU-TABLES.                                          ELSGRPSP
00328      PERFORM ALLOCATE-MENU-HEADINGS-AREA.                         ELSGRPSP
00329      PERFORM ALLOCATE-MENU-SELECTIONS-AREA.                       ELSGRPSP
00330      PERFORM ALLOCATE-MENU-DESCRIPTIONS-ARE.                      ELSGRPSP
00331      INITIALIZE SSB-MNU-CHOICE (1).                               ELSGRPSP
00332      EJECT                                                        ELSGRPSP
00333                                                                   ELSGRPSP
00334                                                                   ELSGRPSP
00335 ************************************************************      ELSGRPSP
00336 *                                                          *      ELSGRPSP
00337 *        ALLOCATE MENU SELECTIONS AREA                     *      ELSGRPSP
00338 *                                                          *      ELSGRPSP
00339 ************************************************************      ELSGRPSP
00340  ALLOCATE-MENU-SELECTIONS-AREA.                                   ELSGRPSP
00341      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSGRPSP
00342      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER        ELSGRPSP
00343          +                                                        ELSGRPSP
00344              (LENGTH OF MSO-MENU-OPT * KTG-NBR-KEYS ).            ELSGRPSP
00345      SET CIA-STG-GETMAIN TO TRUE.                                 ELSGRPSP
00346      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSP
00347      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSGRPSP
00348      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSP
00349          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSGRPSP
00350                                                                   ELSGRPSP
00351                                                                   ELSGRPSP
00352 ************************************************************      ELSGRPSP
00353 *                                                          *      ELSGRPSP
00354 *        ALLOCATE MENU HEADINGS AREA                       *      ELSGRPSP
00355 *                                                          *      ELSGRPSP
00356 ************************************************************      ELSGRPSP
00357  ALLOCATE-MENU-HEADINGS-AREA.                                     ELSGRPSP
00358      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSGRPSP
00359      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +         ELSGRPSP
00360              (MAX-HEADER-LNS  *  LENGTH OF MHD-HDG-LINE           ELSGRPSP
00361          ).                                                       ELSGRPSP
00362      SET CIA-STG-GETMAIN TO TRUE.                                 ELSGRPSP
00363      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSP
00364      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSGRPSP
00365      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSP
00366          ADDRESS OF MHD-MENU-HEADINGS.                            ELSGRPSP
00367      EJECT                                                        ELSGRPSP
00368                                                                   ELSGRPSP
00369                                                                   ELSGRPSP
00370 ************************************************************      ELSGRPSP
00371 *                                                          *      ELSGRPSP
00372 *        ALLOCATE MENU DESCRIPTIONS AREA                   *      ELSGRPSP
00373 *                                                          *      ELSGRPSP
00374 ************************************************************      ELSGRPSP
00375  ALLOCATE-MENU-DESCRIPTIONS-ARE.                                  ELSGRPSP
00376      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSP
00377      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSP
00378          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSGRPSP
00379      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSP
00380      COMPUTE IOP-REC-LEN  = LENGTH OF MSD-NBR-DESCR-LINES +       ELSGRPSP
00381              (MAX-DETAIL-LNS  * LENGTH OF                         ELSGRPSP
00382          MSD-DESCR-LINE).                                         ELSGRPSP
00383      SET IOP-GETMAIN-REC TO TRUE.                                 ELSGRPSP
00384      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSP
00385      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS                    ELSGRPSP
00386                                        TO IOP-REC-PTR.            ELSGRPSP
00387                                                                   ELSGRPSP
00388                                                                   ELSGRPSP
00389 ************************************************************      ELSGRPSP
00390 *                                                          *      ELSGRPSP
00391 *        BUILD MENU                                        *      ELSGRPSP
00392 *                                                          *      ELSGRPSP
00393 ************************************************************      ELSGRPSP
00394  BUILD-MENU.                                                      ELSGRPSP
00395      PERFORM CREATE-MENU-OPTIONS.                                 ELSGRPSP
00396      PERFORM CREATE-MENU-HEADER.                                  ELSGRPSP
00397      PERFORM CREATE-MENU-DETAIL.                                  ELSGRPSP
00398      EJECT                                                        ELSGRPSP
00399                                                                   ELSGRPSP
00400                                                                   ELSGRPSP
00401 ************************************************************      ELSGRPSP
00402 *                                                          *      ELSGRPSP
00403 *        CREATE MENU HEADER                                *      ELSGRPSP
00404 *                                                          *      ELSGRPSP
00405 ************************************************************      ELSGRPSP
00406  CREATE-MENU-HEADER.                                              ELSGRPSP
00407      PERFORM CONVERT-HEADER-EFFECTIVE-DATE.                       ELSGRPSP
00408      PERFORM CONVERT-HEADER-TERMINATION-DAT.                      ELSGRPSP
00409      MOVE MAX-HEADER-LNS TO MHD-NBR-HDG-LINES.                    ELSGRPSP
00410      PERFORM LOAD-HEADER-TABLE                                    ELSGRPSP
00411          VARYING MHD-IDX FROM 1 BY 1                              ELSGRPSP
00412                    UNTIL MHD-IDX GREATER THAN                     ELSGRPSP
00413              MAX-HEADER-LNS.                                      ELSGRPSP
00414                                                                   ELSGRPSP
00415                                                                   ELSGRPSP
00416 ************************************************************      ELSGRPSP
00417 *                                                          *      ELSGRPSP
00418 *        CONVERT HEADER EFFECTIVE DATE                     *      ELSGRPSP
00419 *                                                          *      ELSGRPSP
00420 ************************************************************      ELSGRPSP
00421  CONVERT-HEADER-EFFECTIVE-DATE.                                   ELSGRPSP
00422      MOVE SSB-SRV-FROM-DATE       TO HGADATE-JULIAN1.             ELSGRPSP
00423      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSGRPSP
00424      IF HGADATE-RETURN NOT = '00'                                 ELSGRPSP
00425          PERFORM INITIALIZE-RETURNED-DATE.                        ELSGRPSP
00426      MOVE HGADATE-DATE2 TO WS-HDR-FROM-DT.                        ELSGRPSP
00427      EJECT                                                        ELSGRPSP
00428                                                                   ELSGRPSP
00429                                                                   ELSGRPSP
00430 ************************************************************      ELSGRPSP
00431 *                                                          *      ELSGRPSP
00432 *        CONVERT HEADER TERMINATION DATE                   *      ELSGRPSP
00433 *                                                          *      ELSGRPSP
00434 ************************************************************      ELSGRPSP
00435  CONVERT-HEADER-TERMINATION-DAT.                                  ELSGRPSP
00436      MOVE SSB-SRV-TO-DATE         TO HGADATE-JULIAN1.             ELSGRPSP
00437      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSGRPSP
00438      IF HGADATE-RETURN NOT = '00'                                 ELSGRPSP
00439          PERFORM INITIALIZE-RETURNED-DATE.                        ELSGRPSP
00440      MOVE HGADATE-DATE2 TO WS-HDR-TO-DT.                          ELSGRPSP
00441                                                                   ELSGRPSP
00442                                                                   ELSGRPSP
00443 ************************************************************      ELSGRPSP
00444 *                                                          *      ELSGRPSP
00445 *        LOAD HEADER TABLE                                 *      ELSGRPSP
00446 *                                                          *      ELSGRPSP
00447 ************************************************************      ELSGRPSP
00448  LOAD-HEADER-TABLE.                                               ELSGRPSP
00449      SET SUB1 TO MHD-IDX.                                         ELSGRPSP
00450      MOVE WS-HDR-LN (SUB1) TO MHD-HDG-LINE (MHD-IDX).             ELSGRPSP
00451      EJECT                                                        ELSGRPSP
00452                                                                   ELSGRPSP
00453                                                                   ELSGRPSP
00454 ************************************************************      ELSGRPSP
00455 *                                                          *      ELSGRPSP
00456 *        CREATE MENU OPTIONS                               *      ELSGRPSP
00457 *                                                          *      ELSGRPSP
00458 ************************************************************      ELSGRPSP
00459  CREATE-MENU-OPTIONS.                                             ELSGRPSP
00460      MOVE LENGTH OF MSO-OPT-NUM-3  TO MSO-OPT-LEN.                ELSGRPSP
00461      SET MSO-OPT-TYP-NUM           TO TRUE.                       ELSGRPSP
00462      EJECT                                                        ELSGRPSP
00463                                                                   ELSGRPSP
00464                                                                   ELSGRPSP
00465 ************************************************************      ELSGRPSP
00466 *                                                          *      ELSGRPSP
00467 *        CREATE MENU DETAIL                                *      ELSGRPSP
00468 *                                                          *      ELSGRPSP
00469 ************************************************************      ELSGRPSP
00470  CREATE-MENU-DETAIL.                                              ELSGRPSP
00471      INITIALIZE SUB1.                                             ELSGRPSP
00472      SET MSO-IDX TO SUB1.                                         ELSGRPSP
00473      PERFORM DUMP-GROUP-SPECIFIC-KEYS-TABLE                       ELSGRPSP
00474          VARYING KTG-IDX FROM 1 BY 1                              ELSGRPSP
00475                    UNTIL KTG-IDX GREATER THAN KTG-NBR-KEYS.       ELSGRPSP
00476      MOVE SUB1                     TO                             ELSGRPSP
00477          MSO-NBR-MENU-OPTS.                                       ELSGRPSP
00478      MOVE 1   TO MSO-MIN-CHOICES                                  ELSGRPSP
00479                  MSO-MAX-CHOICES.                                 ELSGRPSP
00480                                                                   ELSGRPSP
00481                                                                   ELSGRPSP
00482 ************************************************************      ELSGRPSP
00483 *                                                          *      ELSGRPSP
00484 *        DUMP GROUP SPECIFIC KEYS TABLE                    *      ELSGRPSP
00485 *                                                          *      ELSGRPSP
00486 ************************************************************      ELSGRPSP
00487  DUMP-GROUP-SPECIFIC-KEYS-TABLE.                                  ELSGRPSP
00488      IF KTG-SEL (KTG-IDX) OR KTG-EXC (KTG-IDX)                    ELSGRPSP
00489          PERFORM LOAD-OPTION.                                     ELSGRPSP
00490      EJECT                                                        ELSGRPSP
00491                                                                   ELSGRPSP
00492                                                                   ELSGRPSP
00493 ************************************************************      ELSGRPSP
00494 *                                                          *      ELSGRPSP
00495 *        LOAD OPTION                                       *      ELSGRPSP
00496 *                                                          *      ELSGRPSP
00497 ************************************************************      ELSGRPSP
00498  LOAD-OPTION.                                                     ELSGRPSP
00499      ADD 1 TO SUB1.                                               ELSGRPSP
00500      SET MSO-IDX UP BY 1.                                         ELSGRPSP
00501      INITIALIZE                      MSO-OPT-SEL                  ELSGRPSP
00502          (MSO-IDX).                                               ELSGRPSP
00503      MOVE SUB1                   TO  MSO-OPT-NUM-3  (MSO-IDX).    ELSGRPSP
00504      MOVE KTG-EFF-DT-CENTURY (KTG-IDX)   TO  WS-OPT-EFF.          ELSGRPSP
00505      MOVE KTG-TERM-DT-CENTURY (KTG-IDX)  TO  WS-OPT-TERM.         ELSGRPSP
00506      IF KTG-EXC (KTG-IDX)                                         ELSGRPSP
00507          PERFORM PUT-HIGH-VALUES-IN-OPTION-SELE                   ELSGRPSP
00508      ELSE                                                         ELSGRPSP
00509          PERFORM PUT-REFORMAT-VALUE-IN-OPTION-S.                  ELSGRPSP
00510      PERFORM CREATE-ITEM-DESCRIPTION-PER-SE.                      ELSGRPSP
00511                                                                   ELSGRPSP
00512                                                                   ELSGRPSP
00513 ************************************************************      ELSGRPSP
00514 *                                                          *      ELSGRPSP
00515 *        PUT HIGH VALUES IN OPTION SELECTOR                *      ELSGRPSP
00516 *                                                          *      ELSGRPSP
00517 ************************************************************      ELSGRPSP
00518  PUT-HIGH-VALUES-IN-OPTION-SELE.                                  ELSGRPSP
00519      MOVE HIGH-VALUES             TO MSO-OPT-KWD                  ELSGRPSP
00520          (MSO-IDX).                                               ELSGRPSP
00521      MOVE HIGH-VALUES             TO MSO-OPT-SEL (MSO-IDX).       ELSGRPSP
00522                                                                   ELSGRPSP
00523                                                                   ELSGRPSP
00524 ************************************************************      ELSGRPSP
00525 *                                                          *      ELSGRPSP
00526 *        PUT REFORMAT VALUE IN OPTION SELECTOR             *      ELSGRPSP
00527 *                                                          *      ELSGRPSP
00528 ************************************************************      ELSGRPSP
00529  PUT-REFORMAT-VALUE-IN-OPTION-S.                                  ELSGRPSP
00530      MOVE WS-REFORMAT-VALUE      TO  MSO-OPT-KWD (MSO-IDX).       ELSGRPSP
00531      EJECT                                                        ELSGRPSP
00532                                                                   ELSGRPSP
00533                                                                   ELSGRPSP
00534 ************************************************************      ELSGRPSP
00535 *                                                          *      ELSGRPSP
00536 *        CREATE ITEM DESCRIPTION PER SELECTED OCCURRENCE   *      ELSGRPSP
00537 *                                                          *      ELSGRPSP
00538 ************************************************************      ELSGRPSP
00539  CREATE-ITEM-DESCRIPTION-PER-SE.                                  ELSGRPSP
00540      MOVE SUB1                    TO WS-DTL-RANGE-NO.             ELSGRPSP
00541      PERFORM CONVERT-DETAIL-EFFECTIVE-DATE.                       ELSGRPSP
00542      PERFORM CONVERT-DETAIL-TERMINATION-DAT.                      ELSGRPSP
00543      PERFORM TRANSLATE-LINE-OF-BUSINESS.                          ELSGRPSP
00544      SET MSD-IDX                 TO 1.                            ELSGRPSP
00545      SET MSD-NBR-DESCR-LINES     TO MSD-IDX.                      ELSGRPSP
00546      MOVE WS-DTL-LN              TO MSD-DESCR-LINE (MSD-IDX).     ELSGRPSP
00547      IF KTG-EXC (KTG-IDX)                                         ELSGRPSP
00548          PERFORM PUT-NOT-VIEWABLE-MSG-IN-LINE.                    ELSGRPSP
00549      PERFORM ADD-ITEM-DESCRIPTION-TO-MENU.                        ELSGRPSP
00550                                                                   ELSGRPSP
00551                                                                   ELSGRPSP
00552 ************************************************************      ELSGRPSP
00553 *                                                          *      ELSGRPSP
00554 *        PUT NOT VIEWABLE MSG IN LINE                      *      ELSGRPSP
00555 *                                                          *      ELSGRPSP
00556 ************************************************************      ELSGRPSP
00557  PUT-NOT-VIEWABLE-MSG-IN-LINE.                                    ELSGRPSP
00558      SET MSD-IDX UP BY 1.                                         ELSGRPSP
00559      SET MSD-NBR-DESCR-LINES     TO MSD-IDX.                      ELSGRPSP
00560      MOVE SPACES                 TO WS-DTL-LN.                    ELSGRPSP
00561      MOVE WS-NOT-VIEWABLE-MSG    TO WS-DTL-DESCRIP1.              ELSGRPSP
00562      MOVE WS-DTL-LN              TO MSD-DESCR-LINE                ELSGRPSP
00563          (MSD-IDX).                                               ELSGRPSP
00564      EJECT                                                        ELSGRPSP
00565                                                                   ELSGRPSP
00566                                                                   ELSGRPSP
00567 ************************************************************      ELSGRPSP
00568 *                                                          *      ELSGRPSP
00569 *        CONVERT DETAIL EFFECTIVE DATE                     *      ELSGRPSP
00570 *                                                          *      ELSGRPSP
00571 ************************************************************      ELSGRPSP
00572  CONVERT-DETAIL-EFFECTIVE-DATE.                                   ELSGRPSP
00573      MOVE KTG-EFF-DT (KTG-IDX)    TO HGADATE-JULIAN1.             ELSGRPSP
00574      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSGRPSP
00575      IF HGADATE-RETURN NOT = '00'                                 ELSGRPSP
00576          PERFORM INITIALIZE-RETURNED-DATE.                        ELSGRPSP
00577      MOVE HGADATE-DATE2 TO WS-DTL-FROM-DT.                        ELSGRPSP
00578      EJECT                                                        ELSGRPSP
00579                                                                   ELSGRPSP
00580                                                                   ELSGRPSP
00581 ************************************************************      ELSGRPSP
00582 *                                                          *      ELSGRPSP
00583 *        CONVERT DETAIL TERMINATION DATE                   *      ELSGRPSP
00584 *                                                          *      ELSGRPSP
00585 ************************************************************      ELSGRPSP
00586  CONVERT-DETAIL-TERMINATION-DAT.                                  ELSGRPSP
00587      MOVE KTG-TERMN-DT  (KTG-IDX) TO HGADATE-JULIAN1.             ELSGRPSP
00588      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSGRPSP
00589      IF HGADATE-RETURN NOT = '00'                                 ELSGRPSP
00590          PERFORM INITIALIZE-RETURNED-DATE.                        ELSGRPSP
00591      MOVE HGADATE-DATE2 TO WS-DTL-TO-DT.                          ELSGRPSP
00592                                                                   ELSGRPSP
00593                                                                   ELSGRPSP
00594 ************************************************************      ELSGRPSP
00595 *                                                          *      ELSGRPSP
00596 *        INITIALIZE RETURNED DATE                          *      ELSGRPSP
00597 *                                                          *      ELSGRPSP
00598 ************************************************************      ELSGRPSP
00599  INITIALIZE-RETURNED-DATE.                                        ELSGRPSP
00600      MOVE ZEROES TO HGADATE-DATE2.                                ELSGRPSP
00601                                                                   ELSGRPSP
00602                                                                   ELSGRPSP
00603 ************************************************************      ELSGRPSP
00604 *                                                          *      ELSGRPSP
00605 *        LINK TO ELUIOPGM                                  *      ELSGRPSP
00606 *                                                          *      ELSGRPSP
00607 ************************************************************      ELSGRPSP
00608  LINK-TO-ELUIOPGM.                                                ELSGRPSP
00609      EXEC CICS LINK                                               ELSGRPSP
00610                PROGRAM('ELUIOPGM')                                ELSGRPSP
00611                COMMAREA(DFHCOMMAREA)                              ELSGRPSP
00612                END-EXEC.                                          ELSGRPSP
00613      EJECT                                                        ELSGRPSP
00614                                                                   ELSGRPSP
00615                                                                   ELSGRPSP
00616 ************************************************************      ELSGRPSP
00617 *                                                          *      ELSGRPSP
00618 *        TRANSLATE LINE OF BUSINESS                        *      ELSGRPSP
00619 *                                                          *      ELSGRPSP
00620 ************************************************************      ELSGRPSP
00621  TRANSLATE-LINE-OF-BUSINESS.                                      ELSGRPSP
00622      MOVE 'GROUP'                       TO                        ELSGRPSP
00623          CMF-RECORD-PREFIX.                                       ELSGRPSP
00624      MOVE 'L-O-B-CONTRACT-LEVEL-IND'    TO                        ELSGRPSP
00625          CMF-ELEMENT-SYSTEM-NAME.                                 ELSGRPSP
00626      MOVE KTG-L-O-B-CONTRACT-LEVEL-IND  (KTG-IDX)                 ELSGRPSP
00627                                         TO                        ELSGRPSP
00628          CMF-CODE-VALUE.                                          ELSGRPSP
00629      EXEC CICS LINK PROGRAM('ELUCMIF')                            ELSGRPSP
00630                     COMMAREA(DFHCOMMAREA)                         ELSGRPSP
00631                     END-EXEC.                                     ELSGRPSP
00632      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELSGRPSP
00633      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSP
00634          ADDRESS OF CMF-DESCR.                                    ELSGRPSP
00635      MOVE CMF-DESCR-LINE (1) TO WS-DTL-DESCRIP1.                  ELSGRPSP
00636      EJECT                                                        ELSGRPSP
00637                                                                   ELSGRPSP
00638                                                                   ELSGRPSP
00639 ************************************************************      ELSGRPSP
00640 *                                                          *      ELSGRPSP
00641 *        ADD ITEM DESCRIPTION TO MENU                      *      ELSGRPSP
00642 *                                                          *      ELSGRPSP
00643 ************************************************************      ELSGRPSP
00644  ADD-ITEM-DESCRIPTION-TO-MENU.                                    ELSGRPSP
00645      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSP
00646      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSP
00647          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSGRPSP
00648      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSP
00649      SET IOP-ADD TO TRUE.                                         ELSGRPSP
00650      SET IOP-FCQ-NONE TO TRUE.                                    ELSGRPSP
00651      SET IOP-KVQ-NONE TO TRUE.                                    ELSGRPSP
00652      PERFORM LINK-TO-ELUIOPGM.                                    ELSGRPSP
00653      EJECT                                                        ELSGRPSP
00654                                                                   ELSGRPSP
00655                                                                   ELSGRPSP
00656 ************************************************************      ELSGRPSP
00657 *                                                          *      ELSGRPSP
00658 *        COMPLETE PROCESS FOR SECTION MENU                 *      ELSGRPSP
00659 *                                                          *      ELSGRPSP
00660 ************************************************************      ELSGRPSP
00661  COMPLETE-PROCESS-FOR-SECTION-M.                                  ELSGRPSP
00662      MOVE SSB-MNU-CHOICE (1) TO WS-REFORMAT-VALUE.                ELSGRPSP
00663      MOVE WS-OPT-EFF  TO  SSB-GROUP-EFF-DATE-CEN.                 ELSGRPSP
00664      MOVE WS-OPT-TERM TO  SSB-GROUP-TERM-DATE-CEN.                ELSGRPSP
00665      SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.              ELSGRPSP
00666      EJECT                                                        ELSGRPSP
00667                                                                   ELSGRPSP
00668                                                                   ELSGRPSP
00669 ************************************************************      ELSGRPSP
00670 *                                                          *      ELSGRPSP
00671 *        JULIAN TO GREG CONVERSION                         *      ELSGRPSP
00672 *                                                          *      ELSGRPSP
00673 ************************************************************      ELSGRPSP
00674  JULIAN-TO-GREG-CONVERSION.                                       ELSGRPSP
00675      MOVE 'CNV'     TO HGADATE-FUNC.                              ELSGRPSP
00676      MOVE 'J'       TO HGADATE-FORM1.                             ELSGRPSP
00677      MOVE 'M'       TO HGADATE-FORM2.                             ELSGRPSP
00678      MOVE ZEROES    TO HGADATE-RETURN                             ELSGRPSP
00679                        HGADATE-DATE2.                             ELSGRPSP
00680      EXEC CICS LINK PROGRAM('HGADATES')                           ELSGRPSP
00681                     COMMAREA(WS-HGADATES-PARMS)                   ELSGRPSP
00682                     END-EXEC.                                     ELSGRPSP
00683      EJECT                                                        ELSGRPSP
00684                                                                   ELSGRPSP
00685                                                                   ELSGRPSP
00686 ************************************************************      ELSGRPSP
00687 *                                                          *      ELSGRPSP
00688 *        RETURN TO CALLER                                  *      ELSGRPSP
00689 *                                                          *      ELSGRPSP
00690 ************************************************************      ELSGRPSP
00691  RETURN-TO-CALLER.                                                ELSGRPSP
00692      EXEC CICS RETURN                                             ELSGRPSP
00693                END-EXEC.                                          ELSGRPSP
00694      GOBACK.                                                      ELSGRPSP
