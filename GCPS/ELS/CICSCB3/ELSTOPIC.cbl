00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSTOPIC
00003  PROGRAM-ID.         ELSTOPIC.                                       LV002
00004                                                                   ELSTOPIC
00005  AUTHOR.             JOHN CURIN,  KEANE, INC.                     ELSTOPIC
00006                                                                   ELSTOPIC
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSTOPIC
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSTOPIC
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSTOPIC
00010                      233 N. MICHIGAN AVE                          ELSTOPIC
00011                      CHICAGO, ILLINOIS 60601                      ELSTOPIC
00012                                                                   ELSTOPIC
00013  DATE-WRITTEN.       22-OCT-1986.                                 ELSTOPIC
00014                                                                   ELSTOPIC
00015  DATE-COMPILED.                                                   ELSTOPIC
00016                                                                   ELSTOPIC
00017  SECURITY.           COPYRIGHT 1986,                              ELSTOPIC
00018                      HEALTH CARE SERVICE CORPORATION              ELSTOPIC
00019      SKIP3                                                        ELSTOPIC
00020 ******************************************************************ELSTOPIC
00021 *                                                                *ELSTOPIC
00022 *    PROGRAM:    ELSTOPIC                                        *ELSTOPIC
00023 *    DATE:       22-OCT-1986                                     *ELSTOPIC
00024 *    AUTHOR:     JOHN CURIN                                      *ELSTOPIC
00025 *    FUNCTION:   TOPIC SELECTOR PROGRAM                          *ELSTOPIC
00026 *                                                                *ELSTOPIC
00027 *                                                                *ELSTOPIC
00028 ******************************************************************ELSTOPIC
00029 *                                                                *ELSTOPIC
00030 *                      MAINTENANCE HISTORY                       *ELSTOPIC
00031 *                                                                *ELSTOPIC
00032 *  MOD     DATE     BY  DRPT                ACTION               *ELSTOPIC
00033 * ----- ----------- --- ----- ---------------------------------- *ELSTOPIC
00034 * 01.00 22-OCT-1986 JTC       CREATED                            *ELSTOPIC
00035 *                                                                *ELSTOPIC
00036 * 01.01 22-SEP-1987 REB       ISSUE ABEND CODES OF 'EL01' IF AN  *ELSTOPIC
00037 *                             INVALID COMMAREA AND 'EL02' IF THE *ELSTOPIC
00038 *                             CIA BLOCK IS NOT PRESENT.          *ELSTOPIC
00039 * 01.02 21-AUG-1989 EGL       DESTRUCTED AND ADDED NEW STORAGE   *ELSTOPIC
00040 *                             MANAGEMENT.                        *ELSTOPIC
00041 *                                                                *ELSTOPIC
00042 * 02.00 OCTOBER 14 1994 RGO   ADD CODE TO PREVENT SEEING ANY     *ELSTOPIC
00043 *                             INFO ON CERTAIN GROUP/SECTIONS.    *ELSTOPIC
00044 *                                                                *ELSTOPIC
00045 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST    *ELSTOPIC
00046 *                                                                *ELSTOPIC
00047 ******************************************************************ELSTOPIC
00048      TITLE 'ELSTOPIC - TOPIC SELECTOR PROGRAM'.                   ELSTOPIC
00049  ENVIRONMENT DIVISION.                                            ELSTOPIC
00050                                                                   ELSTOPIC
00051  CONFIGURATION SECTION.                                           ELSTOPIC
00052  SOURCE-COMPUTER.    IBM-3033.                                    ELSTOPIC
00053  OBJECT-COMPUTER.    IBM-3033.                                    ELSTOPIC
00054      EJECT                                                        ELSTOPIC
00055  DATA DIVISION.                                                   ELSTOPIC
00056                                                                   ELSTOPIC
00057  WORKING-STORAGE SECTION.                                         ELSTOPIC
00058  01  WS-GROUP-SECTION.                                            ELSTOPIC
00059      10 WS-GRP-NO     PIC X(06) VALUE SPACE.                      ELSTOPIC
00060      10 WS-SECTN-NO   PIC X(04) VALUE SPACE.                      ELSTOPIC
00061  01  LOCK-TOP1.                                                   ELSTOPIC
00062      05 TOP-1         PIC X(53) VALUE                             ELSTOPIC
00063       'ELS SYSTEM IS NOT AUTHORIZED TO DISPLAY INFORMATION '.     ELSTOPIC
00064      05 TOP-12        PIC X(26) VALUE                             ELSTOPIC
00065       'FOR THIS GROUP AND SECTION'.                               ELSTOPIC
00066  01  LOCK-TOP2        PIC X(78) VALUE                             ELSTOPIC
00067       'PRESS <PF5> TO RETURN TO PREVIOUS SCREEN'.                 ELSTOPIC
00068  01  LOCK-BLANK       PIC X(78) VALUE SPACES.                     ELSTOPIC
00069                                                                   ELSTOPIC
00070 *** TOPIC SELECTOR WORK AREA                                      ELSTOPIC
00071      COPY ELSTOPIC.                                               ELSTOPIC
00072                                                                   ELSTOPIC
00073  01  WS-HOLD-TOPIC-NAME.                                          ELSTOPIC
00074      05  WS-HOLD-TOPIC-NUMBER    PIC XXX.                         ELSTOPIC
00075      05  WS-HOLD-TOPIC-NAME-ONLY PIC X(47).                       ELSTOPIC
00076      EJECT                                                        ELSTOPIC
00077  LINKAGE SECTION.                                                 ELSTOPIC
00078                                                                   ELSTOPIC
00079  01  DFHCOMMAREA.                                                 ELSTOPIC
00080      COPY ELSCOMMC.                                               ELSTOPIC
00081      EJECT                                                        ELSTOPIC
00082      COPY ELSCIA2C.                                               ELSTOPIC
00083      EJECT                                                        ELSTOPIC
00084      COPY ELSCMDSC.                                               ELSTOPIC
00085      EJECT                                                        ELSTOPIC
00086      COPY ELSCMIFC.                                               ELSTOPIC
00087      EJECT                                                        ELSTOPIC
00088      COPY ELSIOPMC.                                               ELSTOPIC
00089      EJECT                                                        ELSTOPIC
00090      COPY ELSSSCBC.                                               ELSTOPIC
00091      EJECT                                                        ELSTOPIC
00092      COPY ELSMENUC.                                               ELSTOPIC
00093      EJECT                                                        ELSTOPIC
00094      COPY ELSMHDGC.                                               ELSTOPIC
00095      EJECT                                                        ELSTOPIC
00096      COPY ELSMOPTC.                                               ELSTOPIC
00097      EJECT                                                        ELSTOPIC
00098  PROCEDURE DIVISION.                                              ELSTOPIC
00099 ************************************************************      ELSTOPIC
00100 *                                                          *      ELSTOPIC
00101 *        PERFORM TOPIC SELECTOR FUNCTIONS                  *      ELSTOPIC
00102 *                                                          *      ELSTOPIC
00103 ************************************************************      ELSTOPIC
00104                                                                   ELSTOPIC
00105      PERFORM INITIALIZE-MODULE.                                   ELSTOPIC
00106      PERFORM PROCESS-TOPIC-SELECTOR-REQUEST.                      ELSTOPIC
00107      EXEC CICS  RETURN                                            ELSTOPIC
00108                 END-EXEC.                                         ELSTOPIC
00109      GOBACK.                                                      ELSTOPIC
00110 /***********************************************************      ELSTOPIC
00111 *                                                          *      ELSTOPIC
00112 *        INITIALIZE MODULE                                 *      ELSTOPIC
00113 *                                                          *      ELSTOPIC
00114 ************************************************************      ELSTOPIC
00115  INITIALIZE-MODULE.                                               ELSTOPIC
00116      PERFORM CHECK-COMMAREA-LENGTH.                               ELSTOPIC
00117      PERFORM ESTABLISH-ADDRESSING-TO-CIA.                         ELSTOPIC
00118      PERFORM ESTABLISH-ADDRESSING-TO-SSCB.                        ELSTOPIC
00119                                                                   ELSTOPIC
00120 *           ************************************************      ELSTOPIC
00121 *    ESTABLISH ADDRESSIBILITY TO CMI AREA, AND ALLOCATE           ELSTOPIC
00122 *           ************************************************      ELSTOPIC
00123      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSTOPIC
00124      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSTOPIC
00125                            ADDRESS OF CMF-CODES-MANUAL-INTERFACE. ELSTOPIC
00126      IF CIA-RC-PTR-NULL                                           ELSTOPIC
00127          SET CIA-ELSCMIF-DDN TO TRUE                              ELSTOPIC
00128          SET CIA-STG-GETMAIN TO TRUE                              ELSTOPIC
00129          PERFORM CALL-STORAGE-SUBPROGRAM                          ELSTOPIC
00130          SET CIA-ELSCMIF-DDN TO TRUE                              ELSTOPIC
00131          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELSTOPIC
00132                            ADDRESS OF CMF-CODES-MANUAL-INTERFACE  ELSTOPIC
00133      END-IF.                                                      ELSTOPIC
00134  CHECK-COMMAREA-LENGTH.                                           ELSTOPIC
00135      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELSTOPIC
00136          EXEC CICS ABEND                                          ELSTOPIC
00137                    ABCODE('EL01')                                 ELSTOPIC
00138                    END-EXEC.                                      ELSTOPIC
00139                                                                   ELSTOPIC
00140  ESTABLISH-ADDRESSING-TO-CIA.                                     ELSTOPIC
00141      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSTOPIC
00142                 ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.         ELSTOPIC
00143                                                                   ELSTOPIC
00144  ESTABLISH-ADDRESSING-TO-SSCB.                                    ELSTOPIC
00145      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSTOPIC
00146      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSTOPIC
00147                 ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.           ELSTOPIC
00148      IF CIA-RC-PTR-NULL                                           ELSTOPIC
00149          SET CIA-AB-PARM-MISSING TO TRUE                          ELSTOPIC
00150          EXEC CICS ABEND ABCODE(CIA-ABCODE)                       ELSTOPIC
00151                    END-EXEC.                                      ELSTOPIC
00152      EJECT                                                        ELSTOPIC
00153 ************************************************************      ELSTOPIC
00154 *                                                          *      ELSTOPIC
00155 *        PROCESS TOPIC SELECTOR REQUEST                    *      ELSTOPIC
00156 *                                                          *      ELSTOPIC
00157 ************************************************************      ELSTOPIC
00158  PROCESS-TOPIC-SELECTOR-REQUEST.                                  ELSTOPIC
00159      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSTOPIC
00160          PERFORM PROCESS-BUILD-MENU-REQUEST                       ELSTOPIC
00161      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSTOPIC
00162          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSTOPIC
00163      ELSE                                                         ELSTOPIC
00164          PERFORM SIGNAL-INVALID-TOPIC-SELECTORX.                  ELSTOPIC
00165                                                                   ELSTOPIC
00166                                                                   ELSTOPIC
00167 ************************************************************      ELSTOPIC
00168 *                                                          *      ELSTOPIC
00169 *        PROCESS BUILD MENU REQUEST                        *      ELSTOPIC
00170 *                                                          *      ELSTOPIC
00171 ************************************************************      ELSTOPIC
00172  PROCESS-BUILD-MENU-REQUEST.                                      ELSTOPIC
00173      INITIALIZE SSB-MNU-CHOICE (1).                               ELSTOPIC
00174      PERFORM DELETE-MENU-FILE.                                    ELSTOPIC
00175      PERFORM ACQUIRE-STORAGE-AREAS.                               ELSTOPIC
00176      PERFORM CHECK-GRP-SCTN-LOCKOUT.                              ELSTOPIC
00177      IF CMF-RC-OK                                                 ELSTOPIC
00178         PERFORM LOCKOUT-PROCESS                                   ELSTOPIC
00179      ELSE                                                         ELSTOPIC
00180          PERFORM BUILD-MENU-HEADERS                               ELSTOPIC
00181          PERFORM BUILD-TOPIC-TITLES                               ELSTOPIC
00182          PERFORM BUILD-VALID-TOPIC-SELECTIONS                     ELSTOPIC
00183      END-IF.                                                      ELSTOPIC
00184                                                                   ELSTOPIC
00185      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSTOPIC
00186 /***********************************************************      ELSTOPIC
00187 *                                                          *      ELSTOPIC
00188 *        DELETE MENU FILE                                  *      ELSTOPIC
00189 *                                                          *      ELSTOPIC
00190 ************************************************************      ELSTOPIC
00191  DELETE-MENU-FILE.                                                ELSTOPIC
00192      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSTOPIC
00193      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSTOPIC
00194                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELSTOPIC
00195      IF CIA-RC-PTR-NULL                                           ELSTOPIC
00196          PERFORM ALLOCATE-MENU-AREA.                              ELSTOPIC
00197      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSTOPIC
00198      SET IOP-DEL          TO TRUE.                                ELSTOPIC
00199      SET IOP-FCQ-NONE     TO TRUE.                                ELSTOPIC
00200      SET IOP-KVQ-NONE     TO TRUE.                                ELSTOPIC
00201      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSTOPIC
00202                                                                   ELSTOPIC
00203  ALLOCATE-MENU-AREA.                                              ELSTOPIC
00204      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSTOPIC
00205      SET CIA-STG-GETMAIN TO TRUE.                                 ELSTOPIC
00206      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSTOPIC
00207      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSTOPIC
00208      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSTOPIC
00209                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELSTOPIC
00210                                                                   ELSTOPIC
00211                                                                   ELSTOPIC
00212 ************************************************************      ELSTOPIC
00213 *                                                          *      ELSTOPIC
00214 *        ACQUIRE STORAGE AREAS                             *      ELSTOPIC
00215 *                                                          *      ELSTOPIC
00216 ************************************************************      ELSTOPIC
00217  ACQUIRE-STORAGE-AREAS.                                           ELSTOPIC
00218      PERFORM GET-HEADING-STORAGE-AREA.                            ELSTOPIC
00219      PERFORM GET-SELECTION-CODE-KEYWORD-ARE.                      ELSTOPIC
00220      PERFORM GET-DESCRIPTION-LINE-AREA.                           ELSTOPIC
00221 /***********************************************************      ELSTOPIC
00222 *                                                          *      ELSTOPIC
00223 *        GET HEADING STORAGE AREA                          *      ELSTOPIC
00224 *                                                          *      ELSTOPIC
00225 ************************************************************      ELSTOPIC
00226  GET-HEADING-STORAGE-AREA.                                        ELSTOPIC
00227      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSTOPIC
00228      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +         ELSTOPIC
00229             (LENGTH OF MHD-HDG-LINE * TOP-NUMBER-HEADINGS).       ELSTOPIC
00230      SET CIA-STG-GETMAIN TO TRUE.                                 ELSTOPIC
00231      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSTOPIC
00232      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSTOPIC
00233      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSTOPIC
00234                 ADDRESS OF MHD-MENU-HEADINGS.                     ELSTOPIC
00235                                                                   ELSTOPIC
00236                                                                   ELSTOPIC
00237 ************************************************************      ELSTOPIC
00238 *                                                          *      ELSTOPIC
00239 *        GET SELECTION CODE KEYWORD AREA                   *      ELSTOPIC
00240 *                                                          *      ELSTOPIC
00241 ************************************************************      ELSTOPIC
00242  GET-SELECTION-CODE-KEYWORD-ARE.                                  ELSTOPIC
00243      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSTOPIC
00244      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +      ELSTOPIC
00245              (LENGTH OF MSO-MENU-OPT * TOP-NUMBER-CODES).         ELSTOPIC
00246      SET CIA-STG-GETMAIN TO TRUE.                                 ELSTOPIC
00247      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSTOPIC
00248      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSTOPIC
00249      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSTOPIC
00250                 ADDRESS OF MSO-MENU-SELECTION-VALUES.             ELSTOPIC
00251 /***********************************************************      ELSTOPIC
00252 *                                                          *      ELSTOPIC
00253 *        GET DESCRIPTION LINE AREA                         *      ELSTOPIC
00254 *                                                          *      ELSTOPIC
00255 ************************************************************      ELSTOPIC
00256  GET-DESCRIPTION-LINE-AREA.                                       ELSTOPIC
00257      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSTOPIC
00258      SET CIA-STG-GETMAIN TO TRUE.                                 ELSTOPIC
00259      SET IOP-GETMAIN-REC TO TRUE.                                 ELSTOPIC
00260      COMPUTE IOP-REC-LEN = LENGTH OF MSD-NBR-DESCR-LINES +        ELSTOPIC
00261         (LENGTH OF MSD-DESCR-LINE * TOP-MENU-LINE-COUNT).         ELSTOPIC
00262      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSTOPIC
00263      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS TO IOP-REC-PTR.    ELSTOPIC
00264 /***********************************************************      ELSTOPIC
00265 *                                                          *      ELSTOPIC
00266 *        CALL STORAGE SUBPROGRAM                           *      ELSTOPIC
00267 *                                                          *      ELSTOPIC
00268 ************************************************************      ELSTOPIC
00269  CALL-STORAGE-SUBPROGRAM.                                         ELSTOPIC
00270      EXEC CICS LINK PROGRAM ('ELUSTGMG')                          ELSTOPIC
00271                     COMMAREA (DFHCOMMAREA)                        ELSTOPIC
00272                     END-EXEC.                                     ELSTOPIC
00273 /***********************************************************      ELSTOPIC
00274 *                                                          *      ELSTOPIC
00275 *        BUILD MENU HEADERS                                *      ELSTOPIC
00276 *                                                          *      ELSTOPIC
00277 ************************************************************      ELSTOPIC
00278  BUILD-MENU-HEADERS.                                              ELSTOPIC
00279      MOVE TOP-NUMBER-HEADINGS TO MHD-NBR-HDG-LINES.               ELSTOPIC
00280      SET MHD-IDX TO 1.                                            ELSTOPIC
00281      PERFORM                                                      ELSTOPIC
00282          VARYING TOP-HEAD-INDEX FROM 1 BY 1                       ELSTOPIC
00283            UNTIL TOP-HEAD-INDEX > TOP-NUMBER-HEADINGS             ELSTOPIC
00284          MOVE TOP-TOPIC-HEADING-LINE (TOP-HEAD-INDEX) TO          ELSTOPIC
00285                       MHD-HDG-LINE (MHD-IDX)                      ELSTOPIC
00286          SET MHD-IDX UP BY 1                                      ELSTOPIC
00287      END-PERFORM.                                                 ELSTOPIC
00288 /***********************************************************      ELSTOPIC
00289 *                                                          *      ELSTOPIC
00290 *        BUILD TOPIC TITLES                                *      ELSTOPIC
00291 *                                                          *      ELSTOPIC
00292 ************************************************************      ELSTOPIC
00293  BUILD-TOPIC-TITLES.                                              ELSTOPIC
00294      MOVE TOP-TOPIC-TITLE TO SSB-MNU-TITLE.                       ELSTOPIC
00295      SET MSD-IDX TO 1.                                            ELSTOPIC
00296      MOVE TOP-MENU-LINE-COUNT            TO MSD-NBR-DESCR-LINES.  ELSTOPIC
00297      SET IOP-ADD       TO TRUE.                                   ELSTOPIC
00298      SET IOP-FCQ-NONE  TO TRUE.                                   ELSTOPIC
00299      SET IOP-KVQ-NONE  TO TRUE.                                   ELSTOPIC
00300      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSTOPIC
00301      PERFORM LOAD-TOPIC-NAMES                                     ELSTOPIC
00302          VARYING TOP-TOPIC-INDEX FROM 1 BY 1                      ELSTOPIC
00303                     UNTIL TOP-TOPIC-INDEX IS GREATER THAN         ELSTOPIC
00304              TOP-NUMBER-CODES.                                    ELSTOPIC
00305                                                                   ELSTOPIC
00306                                                                   ELSTOPIC
00307 ************************************************************      ELSTOPIC
00308 *                                                          *      ELSTOPIC
00309 *        LOAD TOPIC NAMES                                  *      ELSTOPIC
00310 *                                                          *      ELSTOPIC
00311 ************************************************************      ELSTOPIC
00312  LOAD-TOPIC-NAMES.                                                ELSTOPIC
00313      MOVE TOP-TOPIC-NAME (TOP-TOPIC-INDEX) TO MSD-DESCR-LINE      ELSTOPIC
00314          (MSD-IDX).                                               ELSTOPIC
00315      MOVE LENGTH OF MSD-MENU-ITEM-DESCRIPTIONS TO IOP-REC-LEN.    ELSTOPIC
00316      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSTOPIC
00317 /***********************************************************      ELSTOPIC
00318 *                                                          *      ELSTOPIC
00319 *        BUILD VALID TOPIC SELECTIONS                      *      ELSTOPIC
00320 *                                                          *      ELSTOPIC
00321 ************************************************************      ELSTOPIC
00322  BUILD-VALID-TOPIC-SELECTIONS.                                    ELSTOPIC
00323      PERFORM LOAD-MENU-OPTS-HEADER.                               ELSTOPIC
00324      SET MSO-IDX TO 1.                                            ELSTOPIC
00325      PERFORM LOAD-TOPIC-CODES-KEYWORD                             ELSTOPIC
00326          VARYING TOP-TOPIC-INDEX FROM 1 BY 1                      ELSTOPIC
00327                  UNTIL TOP-TOPIC-INDEX IS GREATER THAN            ELSTOPIC
00328              TOP-NUMBER-CODES.                                    ELSTOPIC
00329                                                                   ELSTOPIC
00330                                                                   ELSTOPIC
00331 ************************************************************      ELSTOPIC
00332 *                                                          *      ELSTOPIC
00333 *        LOAD MENU OPTS HEADER                             *      ELSTOPIC
00334 *                                                          *      ELSTOPIC
00335 ************************************************************      ELSTOPIC
00336  LOAD-MENU-OPTS-HEADER.                                           ELSTOPIC
00337      MOVE TOP-NUMBER-CODES TO MSO-NBR-MENU-OPTS.                  ELSTOPIC
00338      MOVE 1   TO MSO-MIN-CHOICES                                  ELSTOPIC
00339                  MSO-MAX-CHOICES.                                 ELSTOPIC
00340      MOVE LENGTH OF TOP-VALID-SELECT TO MSO-OPT-LEN.              ELSTOPIC
00341      SET MSO-OPT-TYP-NUM  TO TRUE.                                ELSTOPIC
00342                                                                   ELSTOPIC
00343                                                                   ELSTOPIC
00344 ************************************************************      ELSTOPIC
00345 *                                                          *      ELSTOPIC
00346 *        LOAD TOPIC CODES KEYWORD                          *      ELSTOPIC
00347 *                                                          *      ELSTOPIC
00348 ************************************************************      ELSTOPIC
00349  LOAD-TOPIC-CODES-KEYWORD.                                        ELSTOPIC
00350      MOVE TOP-VALID-SELECT(TOP-TOPIC-INDEX) TO                    ELSTOPIC
00351           MSO-OPT-SEL(MSO-IDX).                                   ELSTOPIC
00352      MOVE TOP-KEYWORD(TOP-TOPIC-INDEX) TO                         ELSTOPIC
00353           MSO-OPT-KWD(MSO-IDX).                                   ELSTOPIC
00354      SET MSO-IDX UP BY 1.                                         ELSTOPIC
00355 /***********************************************************      ELSTOPIC
00356 *                                                          *      ELSTOPIC
00357 *        PROCESS MENU COMPLETED REQUEST                    *      ELSTOPIC
00358 *                                                          *      ELSTOPIC
00359 ************************************************************      ELSTOPIC
00360  PROCESS-MENU-COMPLETED-REQUEST.                                  ELSTOPIC
00361      MOVE SSB-MNU-CHOICE (1) TO SSB-TOPIC.                        ELSTOPIC
00362      PERFORM MOVE-TOPIC-TITLE-TO-SSB-TOPICX.                      ELSTOPIC
00363      SET SSB-COMPLETED (SSB-SELECTOR-STATE)     TO                ELSTOPIC
00364          TRUE.                                                    ELSTOPIC
00365                                                                   ELSTOPIC
00366                                                                   ELSTOPIC
00367 ************************************************************      ELSTOPIC
00368 *                                                          *      ELSTOPIC
00369 *        MOVE TOPIC TITLE TO SSB-TOPIC-PHRASE              *      ELSTOPIC
00370 *                                                          *      ELSTOPIC
00371 ************************************************************      ELSTOPIC
00372  MOVE-TOPIC-TITLE-TO-SSB-TOPICX.                                  ELSTOPIC
00373      SET TOP-TOPIC-INDEX TO 1.                                    ELSTOPIC
00374      SEARCH TOP-TOPIC-INFO                                        ELSTOPIC
00375                VARYING TOP-TOPIC-INDEX                            ELSTOPIC
00376                  AT END                                           ELSTOPIC
00377                    SET CIA-AB-PGM-LOGIC TO TRUE                   ELSTOPIC
00378                    EXEC CICS ABEND                                ELSTOPIC
00379                              ABCODE(CIA-ABCODE)                   ELSTOPIC
00380                    END-EXEC                                       ELSTOPIC
00381         WHEN                                                      ELSTOPIC
00382          SSB-MNU-CHOICE (1) =                                     ELSTOPIC
00383          TOP-KEYWORD(TOP-TOPIC-INDEX)                             ELSTOPIC
00384                 MOVE TOP-TOPIC-NAME(TOP-TOPIC-INDEX) TO           ELSTOPIC
00385                    WS-HOLD-TOPIC-NAME                             ELSTOPIC
00386                 MOVE WS-HOLD-TOPIC-NAME-ONLY                      ELSTOPIC
00387                        TO                                         ELSTOPIC
00388                             SSB-TOPIC-PHRASE.                     ELSTOPIC
00389                                                                   ELSTOPIC
00390                                                                   ELSTOPIC
00391                                                                   ELSTOPIC
00392                                                                   ELSTOPIC
00393 ************************************************************      ELSTOPIC
00394 *                                                          *      ELSTOPIC
00395 *        CALL INPUT-OUTPUT SUBPROGRAM                      *      ELSTOPIC
00396 *                                                          *      ELSTOPIC
00397 ************************************************************      ELSTOPIC
00398  CALL-INPUT-OUTPUT-SUBPROGRAM.                                    ELSTOPIC
00399      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELSTOPIC
00400                     COMMAREA(DFHCOMMAREA)                         ELSTOPIC
00401                     END-EXEC.                                     ELSTOPIC
00402                                                                   ELSTOPIC
00403                                                                   ELSTOPIC
00404 ************************************************************      ELSTOPIC
00405 *                                                          *      ELSTOPIC
00406 *        SIGNAL INVALID TOPIC SELECTOR REQUEST             *      ELSTOPIC
00407 *                                                          *      ELSTOPIC
00408 ************************************************************      ELSTOPIC
00409  SIGNAL-INVALID-TOPIC-SELECTORX.                                  ELSTOPIC
00410      SET CIA-AB-UNDEF TO TRUE.                                    ELSTOPIC
00411      EXEC CICS ABEND                                              ELSTOPIC
00412                ABCODE(CIA-ABCODE)                                 ELSTOPIC
00413                END-EXEC.                                          ELSTOPIC
00414 ************************************************************      ELSTOPIC
00415 *                                                          *      ELSTOPIC
00416 *    CHECK GROUP/SECTION LOCKOUT                           *      ELSTOPIC
00417 *                                                          *      ELSTOPIC
00418 ************************************************************      ELSTOPIC
00419  CHECK-GRP-SCTN-LOCKOUT.                                          ELSTOPIC
00420      MOVE '@ELS' TO CMF-RECORD-PREFIX.                            ELSTOPIC
00421      MOVE 'LOCKOUT-GRP-SCTN-ALL' TO CMF-ELEMENT-SYSTEM-NAME.      ELSTOPIC
00422      MOVE SSB-GRP-NO TO WS-GRP-NO.                                ELSTOPIC
00423      MOVE SSB-SECT-NO TO WS-SECTN-NO.                             ELSTOPIC
00424      MOVE WS-GROUP-SECTION TO CMF-CODE-VALUE.                     ELSTOPIC
00425      EXEC CICS LINK                                               ELSTOPIC
00426                PROGRAM ('ELUCMIF')                                ELSTOPIC
00427                COMMAREA (DFHCOMMAREA)                             ELSTOPIC
00428                END-EXEC.                                          ELSTOPIC
00429      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELSTOPIC
00430      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSTOPIC
00431                            ADDRESS OF CMF-DESCR.                  ELSTOPIC
00432                                                                   ELSTOPIC
00433 ************************************************************      ELSTOPIC
00434 *  LOCKOUT-PROCESS.                                        *      ELSTOPIC
00435 ************************************************************      ELSTOPIC
00436  LOCKOUT-PROCESS.                                                 ELSTOPIC
00437      MOVE TOP-NUMBER-HEADINGS TO MHD-NBR-HDG-LINES.               ELSTOPIC
00438      SET MHD-IDX TO 1.                                            ELSTOPIC
00439      MOVE LOCK-TOP1 TO MHD-HDG-LINE(MHD-IDX).                     ELSTOPIC
00440      SET MHD-IDX TO 2.                                            ELSTOPIC
00441      MOVE LOCK-BLANK TO MHD-HDG-LINE(MHD-IDX).                    ELSTOPIC
00442      SET MHD-IDX TO 3.                                            ELSTOPIC
00443      MOVE LOCK-BLANK TO MHD-HDG-LINE(MHD-IDX).                    ELSTOPIC
00444      SET MHD-IDX TO 4.                                            ELSTOPIC
00445      MOVE LOCK-TOP2 TO MHD-HDG-LINE(MHD-IDX).                     ELSTOPIC
00446                                                                   ELSTOPIC
00447      MOVE TOP-TOPIC-TITLE TO SSB-MNU-TITLE.                       ELSTOPIC
00448      SET MSD-IDX TO 1.                                            ELSTOPIC
00449      MOVE TOP-MENU-LINE-COUNT TO MSD-NBR-DESCR-LINES.             ELSTOPIC
00450      SET IOP-ADD TO TRUE.                                         ELSTOPIC
00451      SET IOP-FCQ-NONE TO TRUE.                                    ELSTOPIC
00452      SET IOP-KVQ-NONE TO TRUE.                                    ELSTOPIC
00453      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSTOPIC
00454                                                                   ELSTOPIC
00455 *    STUFF DONE IN BUILD TOPIC-TITLES.                            ELSTOPIC
00456                                                                   ELSTOPIC
00457      SET TOP-TOPIC-INDEX TO 1.                                    ELSTOPIC
00458      MOVE LOCK-BLANK                                              ELSTOPIC
00459                      TO MSD-DESCR-LINE(MSD-IDX).                  ELSTOPIC
00460      MOVE LENGTH OF MSD-MENU-ITEM-DESCRIPTIONS TO IOP-REC-LEN.    ELSTOPIC
00461      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSTOPIC
00462                                                                   ELSTOPIC
00463      PERFORM BUILD-VALID-TOPIC-SELECTIONS.                        ELSTOPIC
00464                                                                   ELSTOPIC
00465                                                                   ELSTOPIC
