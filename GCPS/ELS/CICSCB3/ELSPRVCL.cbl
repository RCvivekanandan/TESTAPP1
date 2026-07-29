00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSPRVCL
00003  PROGRAM-ID.         ELSPRVCL.                                       LV002
00004                                                                   ELSPRVCL
00005  AUTHOR.             NINA CERVANTES.                              ELSPRVCL
00006                                                                   ELSPRVCL
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSPRVCL
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSPRVCL
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSPRVCL
00010                      233 N. MICHIGAN AVE                          ELSPRVCL
00011                      CHICAGO, ILLINOIS 60601                      ELSPRVCL
00012                                                                   ELSPRVCL
00013  DATE-WRITTEN.       03-NOV-1986.                                 ELSPRVCL
00014                                                                   ELSPRVCL
00015  DATE-COMPILED.                                                   ELSPRVCL
00016                                                                   ELSPRVCL
00017  SECURITY.           COPYRIGHT 1986,                              ELSPRVCL
00018                      HEALTH CARE SERVICE CORPORATION              ELSPRVCL
00019      SKIP3                                                        ELSPRVCL
00020  ENVIRONMENT DIVISION.                                            ELSPRVCL
00021                                                                   ELSPRVCL
00022  CONFIGURATION SECTION.                                           ELSPRVCL
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELSPRVCL
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELSPRVCL
00025 *                                                                 ELSPRVCL
00026 *   CONVERTED FROM STRUCTURES  04/11/89  ED LISS                  ELSPRVCL
00027 *                                                                 ELSPRVCL
00028 *   STORAGE MENAGEMENT ENHANCEMENTS  04/13/89  GEM                ELSPRVCL
00029 *                                                                 ELSPRVCL
00030 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST     ELSPRVCL
00031 *                                                                 ELSPRVCL
00032      EJECT                                                        ELSPRVCL
00033  DATA DIVISION.                                                   ELSPRVCL
00034                                                                   ELSPRVCL
00035  FILE SECTION.                                                    ELSPRVCL
00036                                                                   ELSPRVCL
00037  WORKING-STORAGE SECTION.                                         ELSPRVCL
00038                                                                   ELSPRVCL
00039  77  FILLER                  PIC X(29)   VALUE                    ELSPRVCL
00040      '***ELSPRVCL WS BEGINS HERE***'.                             ELSPRVCL
00041                                                                   ELSPRVCL
00042  01  SUB1                    PIC S9(4)   COMP VALUE +0.           ELSPRVCL
00043  01  WS-MENU-TITLE           PIC X(35)   VALUE                    ELSPRVCL
00044      'SELECT PROVIDER CATEGORY TO DISPLAY'.                       ELSPRVCL
00045                                                                   ELSPRVCL
00046  01  MAX-OPTIONS             PIC S9(4)   COMP VALUE +03.          ELSPRVCL
00047  01  WS-HEADINGS.                                                 ELSPRVCL
00048      02  WS-HEADING-LINES.                                        ELSPRVCL
00049          03  WS-HDR-LN1      PIC X(79)   VALUE                    ELSPRVCL
00050                 'BENEFITS FOR THIS TOPIC ARE AVAILABLE FOR BOTH PRELSPRVCL
00051 -               'OFESSIONAL AND INSTITUTIONAL'.                   ELSPRVCL
00052          03  WS-HDR-LN2      PIC X(79)   VALUE                    ELSPRVCL
00053                 'PROVIDERS.  INDICATE WHICH CATEGORY YOU WISH TO SELSPRVCL
00054 -               'EE.'.                                            ELSPRVCL
00055          03  WS-HDR-LN3      PIC X(79)   VALUE  SPACES.           ELSPRVCL
00056          03  WS-HDR-LN4      PIC X(79)   VALUE  SPACES.           ELSPRVCL
00057          03  WS-HDR-LN5      PIC X(79)   VALUE  SPACES.           ELSPRVCL
00058          03  WS-HDR-LN6      PIC X(79)   VALUE                    ELSPRVCL
00059              ' I INSTITUTIONAL'.                                  ELSPRVCL
00060          03  WS-HDR-LN7      PIC X(79)   VALUE  SPACES.           ELSPRVCL
00061          03  WS-HDR-LN8      PIC X(79)   VALUE                    ELSPRVCL
00062              ' P PROFESSIONAL'.                                   ELSPRVCL
00063          03  WS-HDR-LN9      PIC X(79)   VALUE  SPACES.           ELSPRVCL
00064          03  WS-HDR-LN10     PIC X(79)   VALUE                    ELSPRVCL
00065              ' B BOTH INSTITUTIONAL AND PROFESSIONAL'.            ELSPRVCL
00066          03  WS-HDR-LN11     PIC X(79)   VALUE  SPACES.           ELSPRVCL
00067          03  WS-HDR-LN12     PIC X(79)   VALUE                    ELSPRVCL
00068              'IF YOU DO NOT ENTER A CHOICE, BOTH INSTITUTIONAL ANDELSPRVCL
00069 -            ' PROFESSIONAL BENEFITS WILL'.                       ELSPRVCL
00070          03  WS-HDR-LN13     PIC X(79)   VALUE                    ELSPRVCL
00071              'BE DISPLAYED'.                                      ELSPRVCL
00072          03  WS-HDR-LN14     PIC X(79)   VALUE  SPACES.           ELSPRVCL
00073          03  WS-HDR-LN15     PIC X(79)   VALUE  SPACES.           ELSPRVCL
00074      02  FILLER  REDEFINES  WS-HEADING-LINES.                     ELSPRVCL
00075          03  WS-HDR-LN  OCCURS 15 PIC X(79).                      ELSPRVCL
00076                                                                   ELSPRVCL
00077  LINKAGE SECTION.                                                 ELSPRVCL
00078  01  DFHCOMMAREA.                                                 ELSPRVCL
00079      COPY ELSCOMMC.                                               ELSPRVCL
00080 /                                                                 ELSPRVCL
00081      COPY ELSCIA2C.                                               ELSPRVCL
00082 /                                                                 ELSPRVCL
00083      COPY ELSIOPMC.                                               ELSPRVCL
00084 /                                                                 ELSPRVCL
00085      COPY ELSSSCBC.                                               ELSPRVCL
00086 /                                                                 ELSPRVCL
00087      COPY ELSMHDGC.                                               ELSPRVCL
00088 /                                                                 ELSPRVCL
00089      COPY ELSMOPTC.                                               ELSPRVCL
00090 /                                                                 ELSPRVCL
00091      EJECT                                                        ELSPRVCL
00092  PROCEDURE DIVISION.                                              ELSPRVCL
00093 ************************************************************      ELSPRVCL
00094 *                                                          *      ELSPRVCL
00095 *        PROVIDER CLASS SELECTOR                           *      ELSPRVCL
00096 *                                                          *      ELSPRVCL
00097 ************************************************************      ELSPRVCL
00098  PROVIDER-CLASS-SELECTOR.                                         ELSPRVCL
00099      PERFORM INITIALIZE-MODULE.                                   ELSPRVCL
00100      PERFORM DETERMINE-MODULE-STATUS.                             ELSPRVCL
00101      PERFORM RETURN-TO-CALLER.                                    ELSPRVCL
00102                                                                   ELSPRVCL
00103                                                                   ELSPRVCL
00104 ************************************************************      ELSPRVCL
00105 *                                                          *      ELSPRVCL
00106 *        INITIALIZE MODULE                                 *      ELSPRVCL
00107 *                                                          *      ELSPRVCL
00108 ************************************************************      ELSPRVCL
00109  INITIALIZE-MODULE.                                               ELSPRVCL
00110      PERFORM CHECK-COMMAREA-LENGTH.                               ELSPRVCL
00111      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELSPRVCL
00112                                                                   ELSPRVCL
00113                                                                   ELSPRVCL
00114 ************************************************************      ELSPRVCL
00115 *                                                          *      ELSPRVCL
00116 *        CHECK COMMAREA LENGTH                             *      ELSPRVCL
00117 *                                                          *      ELSPRVCL
00118 ************************************************************      ELSPRVCL
00119  CHECK-COMMAREA-LENGTH.                                           ELSPRVCL
00120      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELSPRVCL
00121          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELSPRVCL
00122                                                                   ELSPRVCL
00123                                                                   ELSPRVCL
00124 ************************************************************      ELSPRVCL
00125 *                                                          *      ELSPRVCL
00126 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELSPRVCL
00127 *                                                          *      ELSPRVCL
00128 ************************************************************      ELSPRVCL
00129  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELSPRVCL
00130      SET CIA-AB-DFHCOMMAREA TO TRUE.                              ELSPRVCL
00131      EXEC CICS ABEND                                              ELSPRVCL
00132                ABCODE(CIA-ABCODE)                                 ELSPRVCL
00133                END-EXEC.                                          ELSPRVCL
00134                                                                   ELSPRVCL
00135                                                                   ELSPRVCL
00136 ************************************************************      ELSPRVCL
00137 *                                                          *      ELSPRVCL
00138 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELSPRVCL
00139 *                                                          *      ELSPRVCL
00140 ************************************************************      ELSPRVCL
00141  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELSPRVCL
00142      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSPRVCL
00143          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSPRVCL
00144                                                                   ELSPRVCL
00145      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSPRVCL
00146      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRVCL
00147          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSPRVCL
00148      EJECT                                                        ELSPRVCL
00149                                                                   ELSPRVCL
00150                                                                   ELSPRVCL
00151 ************************************************************      ELSPRVCL
00152 *                                                          *      ELSPRVCL
00153 *        DETERMINE MODULE STATUS                           *      ELSPRVCL
00154 *                                                          *      ELSPRVCL
00155 ************************************************************      ELSPRVCL
00156  DETERMINE-MODULE-STATUS.                                         ELSPRVCL
00157      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSPRVCL
00158          PERFORM INITIAL-CALL-FOR-PROVIDER-MENU                   ELSPRVCL
00159      ELSE IF SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)               ELSPRVCL
00160          PERFORM COMPLETE-PROCESS-FOR-PROVIDERX                   ELSPRVCL
00161      ELSE                                                         ELSPRVCL
00162          PERFORM SIGNAL-UNDEFINED-MODULE-STATUS.                  ELSPRVCL
00163                                                                   ELSPRVCL
00164                                                                   ELSPRVCL
00165 ************************************************************      ELSPRVCL
00166 *                                                          *      ELSPRVCL
00167 *        SIGNAL UNDEFINED MODULE STATUS                    *      ELSPRVCL
00168 *                                                          *      ELSPRVCL
00169 ************************************************************      ELSPRVCL
00170  SIGNAL-UNDEFINED-MODULE-STATUS.                                  ELSPRVCL
00171      SET CIA-AB-UNDEF TO TRUE.                                    ELSPRVCL
00172      EXEC CICS ABEND                                              ELSPRVCL
00173                ABCODE(CIA-ABCODE)                                 ELSPRVCL
00174                END-EXEC.                                          ELSPRVCL
00175      EJECT                                                        ELSPRVCL
00176 ************************************************************      ELSPRVCL
00177 *                                                          *      ELSPRVCL
00178 *        INITIAL CALL FOR PROVIDER MENU                    *      ELSPRVCL
00179 *                                                          *      ELSPRVCL
00180 ************************************************************      ELSPRVCL
00181  INITIAL-CALL-FOR-PROVIDER-MENU.                                  ELSPRVCL
00182      MOVE 'B' TO SSB-MNU-CHOICE (1).                              ELSPRVCL
00183      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSPRVCL
00184      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRVCL
00185          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSPRVCL
00186      IF CIA-RC-PTR-NULL                                           ELSPRVCL
00187          PERFORM ALLOCATE-ELSMENU-IOPARM-AREA.                    ELSPRVCL
00188      PERFORM DEALLOCATE-MENU-FILE.                                ELSPRVCL
00189      PERFORM ACQUIRE-STORAGE-AREAS.                               ELSPRVCL
00190      PERFORM BUILD-MENU-HEADERS.                                  ELSPRVCL
00191      PERFORM BUILD-VALID-SELECTIONS.                              ELSPRVCL
00192      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSPRVCL
00193                                                                   ELSPRVCL
00194                                                                   ELSPRVCL
00195 ************************************************************      ELSPRVCL
00196 *                                                          *      ELSPRVCL
00197 *        ALLOCATE ELSMENU IOPARM AREA                      *      ELSPRVCL
00198 *                                                          *      ELSPRVCL
00199 ************************************************************      ELSPRVCL
00200  ALLOCATE-ELSMENU-IOPARM-AREA.                                    ELSPRVCL
00201      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSPRVCL
00202      SET CIA-STG-GETMAIN TO TRUE.                                 ELSPRVCL
00203      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSPRVCL
00204      EJECT                                                        ELSPRVCL
00205 ************************************************************      ELSPRVCL
00206 *                                                          *      ELSPRVCL
00207 *        DEALLOCATE MENU FILE                              *      ELSPRVCL
00208 *                                                          *      ELSPRVCL
00209 ************************************************************      ELSPRVCL
00210  DEALLOCATE-MENU-FILE.                                            ELSPRVCL
00211      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSPRVCL
00212      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRVCL
00213          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSPRVCL
00214      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSPRVCL
00215      SET IOP-DEL          TO TRUE.                                ELSPRVCL
00216      SET IOP-FCQ-NONE     TO TRUE.                                ELSPRVCL
00217      SET IOP-KVQ-NONE     TO TRUE.                                ELSPRVCL
00218      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELSPRVCL
00219                     COMMAREA(DFHCOMMAREA)                         ELSPRVCL
00220                     END-EXEC.                                     ELSPRVCL
00221      EJECT                                                        ELSPRVCL
00222 ************************************************************      ELSPRVCL
00223 *                                                          *      ELSPRVCL
00224 *        ACQUIRE STORAGE AREAS                             *      ELSPRVCL
00225 *                                                          *      ELSPRVCL
00226 ************************************************************      ELSPRVCL
00227  ACQUIRE-STORAGE-AREAS.                                           ELSPRVCL
00228      PERFORM GET-HEADING-STORAGE-AREA.                            ELSPRVCL
00229      PERFORM GET-SELECTION-CODE-KEYWORD-ARE.                      ELSPRVCL
00230                                                                   ELSPRVCL
00231                                                                   ELSPRVCL
00232 ************************************************************      ELSPRVCL
00233 *                                                          *      ELSPRVCL
00234 *        GET HEADING STORAGE AREA                          *      ELSPRVCL
00235 *                                                          *      ELSPRVCL
00236 ************************************************************      ELSPRVCL
00237  GET-HEADING-STORAGE-AREA.                                        ELSPRVCL
00238      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSPRVCL
00239      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRVCL
00240          ADDRESS OF MHD-MENU-HEADINGS.                            ELSPRVCL
00241      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES           ELSPRVCL
00242                           + (LENGTH OF MHD-HDG-LINE * CIA-MVO).   ELSPRVCL
00243      SET CIA-STG-GETMAIN TO TRUE.                                 ELSPRVCL
00244      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSPRVCL
00245      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSPRVCL
00246      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRVCL
00247          ADDRESS OF MHD-MENU-HEADINGS.                            ELSPRVCL
00248      MOVE CIA-MVO          TO MHD-NBR-HDG-LINES.                  ELSPRVCL
00249                                                                   ELSPRVCL
00250                                                                   ELSPRVCL
00251 ************************************************************      ELSPRVCL
00252 *                                                          *      ELSPRVCL
00253 *        GET SELECTION CODE KEYWORD AREA                   *      ELSPRVCL
00254 *                                                          *      ELSPRVCL
00255 ************************************************************      ELSPRVCL
00256  GET-SELECTION-CODE-KEYWORD-ARE.                                  ELSPRVCL
00257      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSPRVCL
00258      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +      ELSPRVCL
00259              (LENGTH OF MSO-MENU-OPT * MAX-OPTIONS).              ELSPRVCL
00260      SET CIA-STG-GETMAIN TO TRUE.                                 ELSPRVCL
00261      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSPRVCL
00262      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSPRVCL
00263      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPRVCL
00264          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSPRVCL
00265                                                                   ELSPRVCL
00266                                                                   ELSPRVCL
00267 ************************************************************      ELSPRVCL
00268 *                                                          *      ELSPRVCL
00269 *        LINK TO STORAGE MANAGER                           *      ELSPRVCL
00270 *                                                          *      ELSPRVCL
00271 ************************************************************      ELSPRVCL
00272  LINK-TO-STORAGE-MANAGER.                                         ELSPRVCL
00273      EXEC CICS LINK PROGRAM ('ELUSTGMG')                          ELSPRVCL
00274                     COMMAREA (DFHCOMMAREA)                        ELSPRVCL
00275                     END-EXEC.                                     ELSPRVCL
00276      EJECT                                                        ELSPRVCL
00277 ************************************************************      ELSPRVCL
00278 *                                                          *      ELSPRVCL
00279 *        BUILD MENU HEADERS                                *      ELSPRVCL
00280 *                                                          *      ELSPRVCL
00281 ************************************************************      ELSPRVCL
00282  BUILD-MENU-HEADERS.                                              ELSPRVCL
00283      MOVE WS-MENU-TITLE    TO SSB-MNU-TITLE.                      ELSPRVCL
00284      PERFORM LOAD-MENU-HEADINGS                                   ELSPRVCL
00285          VARYING MHD-IDX FROM 1 BY 1 UNTIL                        ELSPRVCL
00286                     MHD-IDX GREATER THAN MHD-NBR-HDG-LINES.       ELSPRVCL
00287                                                                   ELSPRVCL
00288                                                                   ELSPRVCL
00289 ************************************************************      ELSPRVCL
00290 *                                                          *      ELSPRVCL
00291 *        LOAD MENU HEADINGS                                *      ELSPRVCL
00292 *                                                          *      ELSPRVCL
00293 ************************************************************      ELSPRVCL
00294  LOAD-MENU-HEADINGS.                                              ELSPRVCL
00295      SET SUB1 TO MHD-IDX.                                         ELSPRVCL
00296      MOVE WS-HDR-LN (SUB1) TO MHD-HDG-LINE (MHD-IDX).             ELSPRVCL
00297      EJECT                                                        ELSPRVCL
00298 ************************************************************      ELSPRVCL
00299 *                                                          *      ELSPRVCL
00300 *        BUILD VALID SELECTIONS                            *      ELSPRVCL
00301 *                                                          *      ELSPRVCL
00302 ************************************************************      ELSPRVCL
00303  BUILD-VALID-SELECTIONS.                                          ELSPRVCL
00304      MOVE MAX-OPTIONS TO MSO-NBR-MENU-OPTS.                       ELSPRVCL
00305      MOVE LENGTH OF MSO-OPT-NUM-1   TO MSO-OPT-LEN.               ELSPRVCL
00306      SET MSO-OPT-TYP-AN   TO TRUE.                                ELSPRVCL
00307      MOVE 1   TO MSO-MIN-CHOICES                                  ELSPRVCL
00308                  MSO-MAX-CHOICES.                                 ELSPRVCL
00309      SET MSO-IDX TO 1.                                            ELSPRVCL
00310      MOVE 'I' TO MSO-OPT-SEL (MSO-IDX)                            ELSPRVCL
00311                  MSO-OPT-KWD (MSO-IDX).                           ELSPRVCL
00312      SET MSO-IDX TO 2.                                            ELSPRVCL
00313      MOVE 'P' TO MSO-OPT-SEL (MSO-IDX)                            ELSPRVCL
00314                  MSO-OPT-KWD (MSO-IDX).                           ELSPRVCL
00315      SET MSO-IDX TO 3.                                            ELSPRVCL
00316      MOVE 'B' TO MSO-OPT-SEL (MSO-IDX)                            ELSPRVCL
00317                  MSO-OPT-KWD (MSO-IDX).                           ELSPRVCL
00318                                                                   ELSPRVCL
00319                                                                   ELSPRVCL
00320 ************************************************************      ELSPRVCL
00321 *                                                          *      ELSPRVCL
00322 *        COMPLETE PROCESS FOR PROVIDER MENU                *      ELSPRVCL
00323 *                                                          *      ELSPRVCL
00324 ************************************************************      ELSPRVCL
00325  COMPLETE-PROCESS-FOR-PROVIDERX.                                  ELSPRVCL
00326      MOVE SSB-MNU-CHOICE (1) TO SSB-PROVIDER-CLASS.               ELSPRVCL
00327      SET SSB-COMPLETED (SSB-SELECTOR-STATE)     TO                ELSPRVCL
00328          TRUE.                                                    ELSPRVCL
00329      EJECT                                                        ELSPRVCL
00330 ************************************************************      ELSPRVCL
00331 *                                                          *      ELSPRVCL
00332 *        RETURN TO CALLER                                  *      ELSPRVCL
00333 *                                                          *      ELSPRVCL
00334 ************************************************************      ELSPRVCL
00335  RETURN-TO-CALLER.                                                ELSPRVCL
00336      EXEC CICS RETURN                                             ELSPRVCL
00337                END-EXEC.                                          ELSPRVCL
00338      GOBACK.                                                      ELSPRVCL
