00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSPTAGE
00003  PROGRAM-ID.         ELSPTAGE.                                       LV002
00004                                                                   ELSPTAGE
00005  AUTHOR.             JOHN CURIN,  KEANE, INC.                     ELSPTAGE
00006                                                                   ELSPTAGE
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSPTAGE
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSPTAGE
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSPTAGE
00010                      233 N. MICHIGAN AVE                          ELSPTAGE
00011                      CHICAGO, ILLINOIS 60601                      ELSPTAGE
00012                                                                   ELSPTAGE
00013  DATE-WRITTEN.       29-OCT-1986.                                 ELSPTAGE
00014                                                                   ELSPTAGE
00015  DATE-COMPILED.                                                   ELSPTAGE
00016                                                                   ELSPTAGE
00017  SECURITY.           COPYRIGHT 1986,                              ELSPTAGE
00018                      HEALTH CARE SERVICE CORPORATION              ELSPTAGE
00019 ******************************************************************ELSPTAGE
00020 *              MAINTENANCE HISTORY                                ELSPTAGE
00021 *                                                                 ELSPTAGE
00022 *   CONVERTED FROM STRUCTURES 04/11/89  ED LISS                   ELSPTAGE
00023 *                                                                 ELSPTAGE
00024 *   STORAGE MANAGEMENT ENHANCEMENT 04/11/89  GEM                  ELSPTAGE
00025 *                                                                 ELSPTAGE
00026 *   CORRECTED ERROR MADE DURING ABOVE 08/16/89 AKK                ELSPTAGE
00027 *                                                                 ELSPTAGE
00028 *   CHANGED WS-FR-ENTRY AND WS-HOLD-FR FROM PIC X(01) TO PIC X(02)ELSPTAGE
00029 *    FOR FAMILY RELATIONSHIP EXPANSION. 09/24/91 JPB              ELSPTAGE
00030 *                                                                 ELSPTAGE
00031 *   REPLACED TERMINATE ROUTINE WITH GOBACK. 09/27/91 JPB          ELSPTAGE
00032 *                                                                 ELSPTAGE
00033 *   ADDED '0M' TO FR-NON-VARIABLE. 10/02/91 JPB                   ELSPTAGE
00034 *                                                                 ELSPTAGE
00035 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST     ELSPTAGE
00036 *                                                                 ELSPTAGE
00037 ******************************************************************ELSPTAGE
00038      SKIP3                                                        ELSPTAGE
00039  ENVIRONMENT DIVISION.                                            ELSPTAGE
00040                                                                   ELSPTAGE
00041  CONFIGURATION SECTION.                                           ELSPTAGE
00042  SOURCE-COMPUTER.    IBM-3033.                                    ELSPTAGE
00043  OBJECT-COMPUTER.    IBM-3033.                                    ELSPTAGE
00044      EJECT                                                        ELSPTAGE
00045  DATA DIVISION.                                                   ELSPTAGE
00046                                                                   ELSPTAGE
00047  FILE SECTION.                                                    ELSPTAGE
00048                                                                   ELSPTAGE
00049  WORKING-STORAGE SECTION.                                         ELSPTAGE
00050                                                                   ELSPTAGE
00051 *** PATIENT AGE AREA                                              ELSPTAGE
00052                                                                   ELSPTAGE
00053  01  WS-SEL-NBR                PIC  9(01).                        ELSPTAGE
00054  01  WS-SEL-NBR-X  REDEFINES  WS-SEL-NBR                          ELSPTAGE
00055                                PIC  X(01).                        ELSPTAGE
00056                                                                   ELSPTAGE
00057  01  WS-HOLD-FR                PIC  X(02).                        ELSPTAGE
00058      88  FR-NON-VARIABLE            VALUES '0M', '00' THRU '05'.  ELSPTAGE
00059                                                                   ELSPTAGE
00060  01  WS-TABLE-COUNT.                                              ELSPTAGE
00061      05  WS-NUMBER-HEADINGS    PIC S9(04) COMP VALUE +2.          ELSPTAGE
00062      05  WS-MENU-LINE-COUNT    PIC S9(04) COMP VALUE +2.          ELSPTAGE
00063                                                                   ELSPTAGE
00064  01  WS-AGE-TITLE              PIC X(18)            VALUE         ELSPTAGE
00065      'SELECT PATIENT AGE'.                                        ELSPTAGE
00066                                                                   ELSPTAGE
00067  01  WS-PAT-AGE-MEM-HEADINGS.                                     ELSPTAGE
00068      05  FILLER                PIC X(78)            VALUE         ELSPTAGE
00069      'BENEFITS FOR THIS GROUP/SECTION VARY ACCORDING TO THE MEMBERELSPTAGE
00070 -    'S AGE             '.                                        ELSPTAGE
00071                                                                   ELSPTAGE
00072  01  WS-PAT-AGE-NON-MEMBER-HEADINGS.                              ELSPTAGE
00073      05  FILLER                PIC X(78)            VALUE         ELSPTAGE
00074      'BENEFITS FOR THIS GROUP/SECTION VARY ACCORDING TO THE SPOUSEELSPTAGE
00075 -    '/DEPENDENTS AGE   '.                                        ELSPTAGE
00076                                                                   ELSPTAGE
00077  01  WS-PAT-AGE-PATIENT-HEADINGS.                                 ELSPTAGE
00078      05  FILLER                PIC X(78)            VALUE         ELSPTAGE
00079      'BENEFITS FOR THIS GROUP/SECTION VARY ACCORDING TO THE PATIENELSPTAGE
00080 -    'TS AGE            '.                                        ELSPTAGE
00081                                                                   ELSPTAGE
00082  01  WS-FAM-REL-LEVEL-ARRAY.                                      ELSPTAGE
00083      05  WS-NBR-FR               PIC S9(04) COMP.                 ELSPTAGE
00084      05  WS-FR-ENTRY             PIC  X(02)                       ELSPTAGE
00085                                  OCCURS  1  TO  50  TIMES         ELSPTAGE
00086                                  DEPENDING  ON  WS-NBR-FR         ELSPTAGE
00087                                  ASCENDING  KEY IS                ELSPTAGE
00088                                             WS-FR-ENTRY           ELSPTAGE
00089                                  INDEXED BY  WS-FR-IDX,           ELSPTAGE
00090                                              WS-FR-ALT-IDX,       ELSPTAGE
00091                                              WS-FR-MAX-IDX.       ELSPTAGE
00092      EJECT                                                        ELSPTAGE
00093  LINKAGE SECTION.                                                 ELSPTAGE
00094                                                                   ELSPTAGE
00095  01  DFHCOMMAREA.                                                 ELSPTAGE
00096      COPY ELSCOMMC.                                               ELSPTAGE
00097      EJECT                                                        ELSPTAGE
00098      COPY ELSCIA2C.                                               ELSPTAGE
00099      EJECT                                                        ELSPTAGE
00100      COPY ELSIOPMC.                                               ELSPTAGE
00101      EJECT                                                        ELSPTAGE
00102      COPY ELSSSCBC.                                               ELSPTAGE
00103      EJECT                                                        ELSPTAGE
00104      COPY ELSMENUC.                                               ELSPTAGE
00105      EJECT                                                        ELSPTAGE
00106      COPY ELSMHDGC.                                               ELSPTAGE
00107      EJECT                                                        ELSPTAGE
00108      COPY ELSMOPTC.                                               ELSPTAGE
00109      EJECT                                                        ELSPTAGE
00110      COPY ELSSUBTG.                                               ELSPTAGE
00111      EJECT                                                        ELSPTAGE
00112      COPY ELSKEYSC.                                               ELSPTAGE
00113      EJECT                                                        ELSPTAGE
00114      COPY ELSKTBCC.                                               ELSPTAGE
00115      EJECT                                                        ELSPTAGE
00116      COPY ELSKTBGC.                                               ELSPTAGE
00117      EJECT                                                        ELSPTAGE
00118      COPY ELSCMIFC.                                               ELSPTAGE
00119      EJECT                                                        ELSPTAGE
00120      COPY ELSCMDSC.                                               ELSPTAGE
00121      EJECT                                                        ELSPTAGE
00122      EJECT                                                        ELSPTAGE
00123  PROCEDURE DIVISION.                                              ELSPTAGE
00124 ************************************************************      ELSPTAGE
00125 *                                                          *      ELSPTAGE
00126 *                    PROCEDURE DIVISION                    *      ELSPTAGE
00127 *                                                          *      ELSPTAGE
00128 ************************************************************      ELSPTAGE
00129                                                                   ELSPTAGE
00130                                                                   ELSPTAGE
00131 ************************************************************      ELSPTAGE
00132 *                                                          *      ELSPTAGE
00133 *        PATIENT AGE SELECTOR                              *      ELSPTAGE
00134 *                                                          *      ELSPTAGE
00135 ************************************************************      ELSPTAGE
00136  PATIENT-AGE-SELECTOR.                                            ELSPTAGE
00137      PERFORM PATIENT-AGE-SELECTOR-INITIALIZ.                      ELSPTAGE
00138      PERFORM PATIENT-AGE-SELECTOR-PROCESS.                        ELSPTAGE
00139      GOBACK.                                                      ELSPTAGE
00140                                                                   ELSPTAGE
00141                                                                   ELSPTAGE
00142 ************************************************************      ELSPTAGE
00143 *                                                          *      ELSPTAGE
00144 *        PATIENT AGE SELECTOR.INITIALIZE                   *      ELSPTAGE
00145 *                                                          *      ELSPTAGE
00146 ************************************************************      ELSPTAGE
00147  PATIENT-AGE-SELECTOR-INITIALIZ.                                  ELSPTAGE
00148      PERFORM CHECK-COMMAREA-LENGTH.                               ELSPTAGE
00149      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELSPTAGE
00150      PERFORM ESTABLISH-ADDRESSING-TO-SELECT.                      ELSPTAGE
00151      PERFORM ESTABLISH-ADDRESSING-TO-KEY-WO.                      ELSPTAGE
00152      PERFORM ESTABLISH-ADDRESSING-TO-CONTRA.                      ELSPTAGE
00153      PERFORM ESTABLISH-ADDRESSING-TO-GROUPX.                      ELSPTAGE
00154      PERFORM ESTABLISH-ADDRESSING-TO-CODESX.                      ELSPTAGE
00155      PERFORM INITIALIZE-FAMILY-REL-ARRAY.                         ELSPTAGE
00156                                                                   ELSPTAGE
00157                                                                   ELSPTAGE
00158 ************************************************************      ELSPTAGE
00159 *                                                          *      ELSPTAGE
00160 *        PATIENT AGE SELECTOR.PROCESS                      *      ELSPTAGE
00161 *                                                          *      ELSPTAGE
00162 ************************************************************      ELSPTAGE
00163  PATIENT-AGE-SELECTOR-PROCESS.                                    ELSPTAGE
00164      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSPTAGE
00165          PERFORM PROCESS-BUILD-PATIENT-AGE-MENU                   ELSPTAGE
00166      ELSE IF SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)               ELSPTAGE
00167          PERFORM PROCESS-PATIENT-AGE-MENU-COMPL                   ELSPTAGE
00168      ELSE                                                         ELSPTAGE
00169          PERFORM SIGNAL-UNKNOWN-ERROR.                            ELSPTAGE
00170      EJECT                                                        ELSPTAGE
00171                                                                   ELSPTAGE
00172                                                                   ELSPTAGE
00173 ************************************************************      ELSPTAGE
00174 *                                                          *      ELSPTAGE
00175 *        PROCESS BUILD PATIENT AGE MENU                    *      ELSPTAGE
00176 *                                                          *      ELSPTAGE
00177 ************************************************************      ELSPTAGE
00178  PROCESS-BUILD-PATIENT-AGE-MENU.                                  ELSPTAGE
00179      PERFORM LOAD-FAMILY-REL-ARRAY.                               ELSPTAGE
00180      MOVE WS-AGE-TITLE  TO  SSB-MNU-TITLE.                        ELSPTAGE
00181      INITIALIZE SSB-MNU-CHOICE (1).                               ELSPTAGE
00182      PERFORM DELETE-MENU-FILE.                                    ELSPTAGE
00183      PERFORM ACQUIRE-STORAGE-AREAS.                               ELSPTAGE
00184      PERFORM BUILD-MENU-HEADERS.                                  ELSPTAGE
00185      PERFORM BUILD-VALID-TOPIC-SELECTIONS.                        ELSPTAGE
00186      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSPTAGE
00187                                                                   ELSPTAGE
00188                                                                   ELSPTAGE
00189 ************************************************************      ELSPTAGE
00190 *                                                          *      ELSPTAGE
00191 *        LOAD FAMILY REL ARRAY                             *      ELSPTAGE
00192 *                                                          *      ELSPTAGE
00193 ************************************************************      ELSPTAGE
00194  LOAD-FAMILY-REL-ARRAY.                                           ELSPTAGE
00195      PERFORM LOAD-CONTRACT-SELECTED-ENTRIES                       ELSPTAGE
00196          VARYING KTC-IDX  FROM  1  BY  1                          ELSPTAGE
00197                  UNTIL   KTC-IDX  >  KTC-NBR-KEYS.                ELSPTAGE
00198      PERFORM LOAD-GROUP-SPECIFIC-SELECTED-E                       ELSPTAGE
00199          VARYING KTG-IDX  FROM  1  BY  1                          ELSPTAGE
00200                   UNTIL   KTG-IDX  >  KTG-NBR-KEYS.               ELSPTAGE
00201      EJECT                                                        ELSPTAGE
00202                                                                   ELSPTAGE
00203                                                                   ELSPTAGE
00204 ************************************************************      ELSPTAGE
00205 *                                                          *      ELSPTAGE
00206 *        LOAD CONTRACT SELECTED ENTRIES                    *      ELSPTAGE
00207 *                                                          *      ELSPTAGE
00208 ************************************************************      ELSPTAGE
00209  LOAD-CONTRACT-SELECTED-ENTRIES.                                  ELSPTAGE
00210      IF KTC-SEL (KTC-IDX,1)  OR  KTC-SEL (KTC-IDX,2)              ELSPTAGE
00211             OR KTC-SEL (KTC-IDX,3)  OR  KTC-SEL                   ELSPTAGE
00212          (KTC-IDX,4)                                              ELSPTAGE
00213          PERFORM ADD-CONTRACT-FAMILY-RELATION-L.                  ELSPTAGE
00214                                                                   ELSPTAGE
00215                                                                   ELSPTAGE
00216 ************************************************************      ELSPTAGE
00217 *                                                          *      ELSPTAGE
00218 *        ADD CONTRACT FAMILY RELATION LVL TO ARRAY         *      ELSPTAGE
00219 *                                                          *      ELSPTAGE
00220 ************************************************************      ELSPTAGE
00221  ADD-CONTRACT-FAMILY-RELATION-L.                                  ELSPTAGE
00222      MOVE KTC-FAM-REL-LVL (KTC-IDX)  TO  WS-HOLD-FR.              ELSPTAGE
00223      IF NOT FR-NON-VARIABLE                                       ELSPTAGE
00224          PERFORM ADD-FR-LVL-TO-ARRAY-IF-NOT-ALR.                  ELSPTAGE
00225      EJECT                                                        ELSPTAGE
00226                                                                   ELSPTAGE
00227                                                                   ELSPTAGE
00228 ************************************************************      ELSPTAGE
00229 *                                                          *      ELSPTAGE
00230 *        LOAD GROUP SPECIFIC SELECTED ENTRIES              *      ELSPTAGE
00231 *                                                          *      ELSPTAGE
00232 ************************************************************      ELSPTAGE
00233  LOAD-GROUP-SPECIFIC-SELECTED-E.                                  ELSPTAGE
00234      IF KTG-SEL (KTG-IDX)                                         ELSPTAGE
00235          PERFORM ADD-GRPSPC-FAMILY-RELATION-LVL.                  ELSPTAGE
00236                                                                   ELSPTAGE
00237                                                                   ELSPTAGE
00238 ************************************************************      ELSPTAGE
00239 *                                                          *      ELSPTAGE
00240 *        ADD GRPSPC FAMILY RELATION LVL TO ARRAY           *      ELSPTAGE
00241 *                                                          *      ELSPTAGE
00242 ************************************************************      ELSPTAGE
00243  ADD-GRPSPC-FAMILY-RELATION-LVL.                                  ELSPTAGE
00244      MOVE KTG-FAM-REL-LVL (KTG-IDX)  TO  WS-HOLD-FR.              ELSPTAGE
00245      IF NOT FR-NON-VARIABLE                                       ELSPTAGE
00246          PERFORM ADD-FR-LVL-TO-ARRAY-IF-NOT-ALR.                  ELSPTAGE
00247                                                                   ELSPTAGE
00248                                                                   ELSPTAGE
00249 ************************************************************      ELSPTAGE
00250 *                                                          *      ELSPTAGE
00251 *        ADD FR LVL TO ARRAY IF NOT ALREADY PRESENT        *      ELSPTAGE
00252 *                                                          *      ELSPTAGE
00253 ************************************************************      ELSPTAGE
00254  ADD-FR-LVL-TO-ARRAY-IF-NOT-ALR.                                  ELSPTAGE
00255      SEARCH ALL WS-FR-ENTRY                                       ELSPTAGE
00256            WHEN WS-FR-ENTRY (WS-FR-IDX)       =                   ELSPTAGE
00257          WS-HOLD-FR                                               ELSPTAGE
00258                MOVE SPACES  TO  WS-HOLD-FR.                       ELSPTAGE
00259      IF WS-HOLD-FR  NOT  =  SPACES  AND  LOW-VALUES               ELSPTAGE
00260          PERFORM INSERT-FAMILY-RELATION-LEVEL.                    ELSPTAGE
00261                                                                   ELSPTAGE
00262                                                                   ELSPTAGE
00263 ************************************************************      ELSPTAGE
00264 *                                                          *      ELSPTAGE
00265 *        INSERT FAMILY RELATION LEVEL                      *      ELSPTAGE
00266 *                                                          *      ELSPTAGE
00267 ************************************************************      ELSPTAGE
00268  INSERT-FAMILY-RELATION-LEVEL.                                    ELSPTAGE
00269      PERFORM INITIALIZE-FAM-REL-SEARCH-POIN.                      ELSPTAGE
00270      PERFORM OPEN-FAMILY-RELATION-TABLE-SLO                       ELSPTAGE
00271          UNTIL WS-HOLD-FR > WS-FR-ENTRY (WS-FR-ALT-IDX).          ELSPTAGE
00272      PERFORM INSERT-FAMILY-RELATION-INTO-OP.                      ELSPTAGE
00273      EJECT                                                        ELSPTAGE
00274                                                                   ELSPTAGE
00275                                                                   ELSPTAGE
00276 ************************************************************      ELSPTAGE
00277 *                                                          *      ELSPTAGE
00278 *        INITIALIZE FAM REL SEARCH POINTERS                *      ELSPTAGE
00279 *                                                          *      ELSPTAGE
00280 ************************************************************      ELSPTAGE
00281  INITIALIZE-FAM-REL-SEARCH-POIN.                                  ELSPTAGE
00282      SET WS-FR-IDX      TO  WS-FR-MAX-IDX.                        ELSPTAGE
00283      SET WS-FR-ALT-IDX  TO  WS-FR-IDX.                            ELSPTAGE
00284      SET WS-FR-ALT-IDX  DOWN  BY  1.                              ELSPTAGE
00285      ADD  1             TO  WS-NBR-FR.                            ELSPTAGE
00286      SET WS-FR-MAX-IDX  TO  WS-NBR-FR.                            ELSPTAGE
00287      MOVE HIGH-VALUES   TO  WS-FR-ENTRY                           ELSPTAGE
00288          (WS-FR-MAX-IDX).                                         ELSPTAGE
00289      EJECT                                                        ELSPTAGE
00290                                                                   ELSPTAGE
00291                                                                   ELSPTAGE
00292 ************************************************************      ELSPTAGE
00293 *                                                          *      ELSPTAGE
00294 *        OPEN FAMILY RELATION TABLE SLOT                   *      ELSPTAGE
00295 *                                                          *      ELSPTAGE
00296 ************************************************************      ELSPTAGE
00297  OPEN-FAMILY-RELATION-TABLE-SLO.                                  ELSPTAGE
00298      MOVE WS-FR-ENTRY (WS-FR-ALT-IDX)                             ELSPTAGE
00299                  TO  WS-FR-ENTRY (WS-FR-IDX).                     ELSPTAGE
00300      SET WS-FR-IDX      DOWN  BY  1.                              ELSPTAGE
00301      SET WS-FR-ALT-IDX  DOWN  BY  1.                              ELSPTAGE
00302                                                                   ELSPTAGE
00303                                                                   ELSPTAGE
00304 ************************************************************      ELSPTAGE
00305 *                                                          *      ELSPTAGE
00306 *        INSERT FAMILY RELATION INTO OPEN SLOT             *      ELSPTAGE
00307 *                                                          *      ELSPTAGE
00308 ************************************************************      ELSPTAGE
00309  INSERT-FAMILY-RELATION-INTO-OP.                                  ELSPTAGE
00310      MOVE WS-HOLD-FR  TO  WS-FR-ENTRY (WS-FR-IDX).                ELSPTAGE
00311      EJECT                                                        ELSPTAGE
00312                                                                   ELSPTAGE
00313                                                                   ELSPTAGE
00314 ************************************************************      ELSPTAGE
00315 *                                                          *      ELSPTAGE
00316 *        DELETE MENU FILE                                  *      ELSPTAGE
00317 *                                                          *      ELSPTAGE
00318 ************************************************************      ELSPTAGE
00319  DELETE-MENU-FILE.                                                ELSPTAGE
00320      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSPTAGE
00321      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00322          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSPTAGE
00323      IF CIA-RC-PTR-NULL                                           ELSPTAGE
00324          PERFORM ALLOCATE-MENU-AREA.                              ELSPTAGE
00325      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSPTAGE
00326      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00327          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSPTAGE
00328      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSPTAGE
00329      SET IOP-DEL          TO TRUE.                                ELSPTAGE
00330      SET IOP-FCQ-NONE     TO TRUE.                                ELSPTAGE
00331      SET IOP-KVQ-NONE     TO TRUE.                                ELSPTAGE
00332      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSPTAGE
00333      PERFORM DEALLOCATE-MENU-AREA.                                ELSPTAGE
00334      EJECT                                                        ELSPTAGE
00335                                                                   ELSPTAGE
00336                                                                   ELSPTAGE
00337 ************************************************************      ELSPTAGE
00338 *                                                          *      ELSPTAGE
00339 *        ACQUIRE STORAGE AREAS                             *      ELSPTAGE
00340 *                                                          *      ELSPTAGE
00341 ************************************************************      ELSPTAGE
00342  ACQUIRE-STORAGE-AREAS.                                           ELSPTAGE
00343      PERFORM GET-HEADING-STORAGE-AREA.                            ELSPTAGE
00344      PERFORM GET-SELECTION-CODE-KEYWORD-ARE.                      ELSPTAGE
00345      PERFORM GET-DESCRIPTION-LINE-AREA.                           ELSPTAGE
00346                                                                   ELSPTAGE
00347                                                                   ELSPTAGE
00348 ************************************************************      ELSPTAGE
00349 *                                                          *      ELSPTAGE
00350 *        GET HEADING STORAGE AREA                          *      ELSPTAGE
00351 *                                                          *      ELSPTAGE
00352 ************************************************************      ELSPTAGE
00353  GET-HEADING-STORAGE-AREA.                                        ELSPTAGE
00354      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSPTAGE
00355      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +         ELSPTAGE
00356               (LENGTH OF MHD-HDG-LINE *                           ELSPTAGE
00357          WS-NUMBER-HEADINGS).                                     ELSPTAGE
00358      SET CIA-STG-GETMAIN TO TRUE.                                 ELSPTAGE
00359      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSPTAGE
00360      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSPTAGE
00361      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00362          ADDRESS OF MHD-MENU-HEADINGS.                            ELSPTAGE
00363                                                                   ELSPTAGE
00364                                                                   ELSPTAGE
00365 ************************************************************      ELSPTAGE
00366 *                                                          *      ELSPTAGE
00367 *        GET SELECTION CODE KEYWORD AREA                   *      ELSPTAGE
00368 *                                                          *      ELSPTAGE
00369 ************************************************************      ELSPTAGE
00370  GET-SELECTION-CODE-KEYWORD-ARE.                                  ELSPTAGE
00371      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSPTAGE
00372      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +      ELSPTAGE
00373              (LENGTH OF MSO-MENU-OPT *                            ELSPTAGE
00374          GEN-NUMBER-CODES).                                       ELSPTAGE
00375      SET CIA-STG-GETMAIN TO TRUE.                                 ELSPTAGE
00376      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSPTAGE
00377      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSPTAGE
00378      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00379          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSPTAGE
00380      EJECT                                                        ELSPTAGE
00381                                                                   ELSPTAGE
00382                                                                   ELSPTAGE
00383 ************************************************************      ELSPTAGE
00384 *                                                          *      ELSPTAGE
00385 *        GET DESCRIPTION LINE AREA                         *      ELSPTAGE
00386 *                                                          *      ELSPTAGE
00387 ************************************************************      ELSPTAGE
00388  GET-DESCRIPTION-LINE-AREA.                                       ELSPTAGE
00389      PERFORM SET-IOPM-ADDRESS-FOR-MENU-FILE.                      ELSPTAGE
00390      SET CIA-ELSMENU-DDN   TO  TRUE.                              ELSPTAGE
00391      SET CIA-STG-GETMAIN   TO  TRUE.                              ELSPTAGE
00392      SET IOP-GETMAIN-REC   TO  TRUE.                              ELSPTAGE
00393      COMPUTE IOP-REC-LEN  =  LENGTH OF MSD-NBR-DESCR-LINES +      ELSPTAGE
00394                (LENGTH OF MSD-DESCR-LINE *                        ELSPTAGE
00395          WS-MENU-LINE-COUNT).                                     ELSPTAGE
00396      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSPTAGE
00397      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS TO IOP-REC-PTR.    ELSPTAGE
00398      MOVE  2  TO  MSD-NBR-DESCR-LINES.                            ELSPTAGE
00399                                                                   ELSPTAGE
00400                                                                   ELSPTAGE
00401 ************************************************************      ELSPTAGE
00402 *                                                          *      ELSPTAGE
00403 *        SET IOPM ADDRESS FOR MENU FILE                    *      ELSPTAGE
00404 *                                                          *      ELSPTAGE
00405 ************************************************************      ELSPTAGE
00406  SET-IOPM-ADDRESS-FOR-MENU-FILE.                                  ELSPTAGE
00407      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSPTAGE
00408      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00409          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSPTAGE
00410                                                                   ELSPTAGE
00411                                                                   ELSPTAGE
00412 ************************************************************      ELSPTAGE
00413 *                                                          *      ELSPTAGE
00414 *        ALLOCATE MENU AREA                                *      ELSPTAGE
00415 *                                                          *      ELSPTAGE
00416 ************************************************************      ELSPTAGE
00417  ALLOCATE-MENU-AREA.                                              ELSPTAGE
00418      SET  CIA-ELSMENU-DDN TO TRUE.                                ELSPTAGE
00419      SET  CIA-STG-GETMAIN TO TRUE.                                ELSPTAGE
00420      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSPTAGE
00421      EJECT                                                        ELSPTAGE
00422                                                                   ELSPTAGE
00423                                                                   ELSPTAGE
00424 ************************************************************      ELSPTAGE
00425 *                                                          *      ELSPTAGE
00426 *        DEALLOCATE MENU AREA                              *      ELSPTAGE
00427 *                                                          *      ELSPTAGE
00428 ************************************************************      ELSPTAGE
00429  DEALLOCATE-MENU-AREA.                                            ELSPTAGE
00430      SET  CIA-ELSMENU-DDN  TO TRUE.                               ELSPTAGE
00431      SET  CIA-STG-FREEMAIN TO TRUE.                               ELSPTAGE
00432      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSPTAGE
00433                                                                   ELSPTAGE
00434                                                                   ELSPTAGE
00435 ************************************************************      ELSPTAGE
00436 *                                                          *      ELSPTAGE
00437 *        CALL STORAGE SUBPROGRAM                           *      ELSPTAGE
00438 *                                                          *      ELSPTAGE
00439 ************************************************************      ELSPTAGE
00440  CALL-STORAGE-SUBPROGRAM.                                         ELSPTAGE
00441      EXEC CICS LINK PROGRAM ('ELUSTGMG')                          ELSPTAGE
00442                     COMMAREA (DFHCOMMAREA)                        ELSPTAGE
00443                     END-EXEC.                                     ELSPTAGE
00444      EJECT                                                        ELSPTAGE
00445                                                                   ELSPTAGE
00446                                                                   ELSPTAGE
00447 ************************************************************      ELSPTAGE
00448 *                                                          *      ELSPTAGE
00449 *        BUILD MENU HEADERS                                *      ELSPTAGE
00450 *                                                          *      ELSPTAGE
00451 ************************************************************      ELSPTAGE
00452  BUILD-MENU-HEADERS.                                              ELSPTAGE
00453      MOVE WS-NUMBER-HEADINGS  TO  MHD-NBR-HDG-LINES.              ELSPTAGE
00454      SET MHD-IDX              TO  1.                              ELSPTAGE
00455      IF SSB-MEMBER                                                ELSPTAGE
00456          PERFORM USE-MEMBER-AGE-HEADING                           ELSPTAGE
00457      ELSE IF SSB-SPOUSE OR SSB-DEPENDENT                          ELSPTAGE
00458          PERFORM USE-SPOUSE-DEPENDENT-AGE-HEADI                   ELSPTAGE
00459      ELSE                                                         ELSPTAGE
00460          PERFORM USE-PATIENT-AGE-HEADING.                         ELSPTAGE
00461                                                                   ELSPTAGE
00462                                                                   ELSPTAGE
00463 ************************************************************      ELSPTAGE
00464 *                                                          *      ELSPTAGE
00465 *        USE MEMBER AGE HEADING                            *      ELSPTAGE
00466 *                                                          *      ELSPTAGE
00467 ************************************************************      ELSPTAGE
00468  USE-MEMBER-AGE-HEADING.                                          ELSPTAGE
00469      MOVE WS-PAT-AGE-MEM-HEADINGS  TO  MHD-HDG-LINE               ELSPTAGE
00470          (MHD-IDX).                                               ELSPTAGE
00471                                                                   ELSPTAGE
00472                                                                   ELSPTAGE
00473 ************************************************************      ELSPTAGE
00474 *                                                          *      ELSPTAGE
00475 *        USE SPOUSE DEPENDENT AGE HEADING                  *      ELSPTAGE
00476 *                                                          *      ELSPTAGE
00477 ************************************************************      ELSPTAGE
00478  USE-SPOUSE-DEPENDENT-AGE-HEADI.                                  ELSPTAGE
00479      MOVE WS-PAT-AGE-NON-MEMBER-HEADINGS  TO                      ELSPTAGE
00480                               MHD-HDG-LINE (MHD-IDX).             ELSPTAGE
00481                                                                   ELSPTAGE
00482                                                                   ELSPTAGE
00483 ************************************************************      ELSPTAGE
00484 *                                                          *      ELSPTAGE
00485 *        USE PATIENT AGE HEADING                           *      ELSPTAGE
00486 *                                                          *      ELSPTAGE
00487 ************************************************************      ELSPTAGE
00488  USE-PATIENT-AGE-HEADING.                                         ELSPTAGE
00489      MOVE WS-PAT-AGE-PATIENT-HEADINGS  TO                         ELSPTAGE
00490                               MHD-HDG-LINE (MHD-IDX).             ELSPTAGE
00491      EJECT                                                        ELSPTAGE
00492                                                                   ELSPTAGE
00493                                                                   ELSPTAGE
00494 ************************************************************      ELSPTAGE
00495 *                                                          *      ELSPTAGE
00496 *        BUILD VALID TOPIC SELECTIONS                      *      ELSPTAGE
00497 *                                                          *      ELSPTAGE
00498 ************************************************************      ELSPTAGE
00499  BUILD-VALID-TOPIC-SELECTIONS.                                    ELSPTAGE
00500      PERFORM LOAD-MENU-OPTS-HEADER.                               ELSPTAGE
00501      SET MSO-IDX TO 1.                                            ELSPTAGE
00502      PERFORM LOAD-TABLES                                          ELSPTAGE
00503          VARYING WS-FR-IDX  FROM  2  BY  1                        ELSPTAGE
00504                UNTIL   WS-FR-IDX  =  WS-FR-MAX-IDX.               ELSPTAGE
00505                                                                   ELSPTAGE
00506                                                                   ELSPTAGE
00507 ************************************************************      ELSPTAGE
00508 *                                                          *      ELSPTAGE
00509 *        LOAD MENU OPTS HEADER                             *      ELSPTAGE
00510 *                                                          *      ELSPTAGE
00511 ************************************************************      ELSPTAGE
00512  LOAD-MENU-OPTS-HEADER.                                           ELSPTAGE
00513      MOVE WS-NBR-FR       TO    MSO-NBR-MENU-OPTS.                ELSPTAGE
00514      SUBTRACT  2          FROM  MSO-NBR-MENU-OPTS.                ELSPTAGE
00515      MOVE 1   TO MSO-MIN-CHOICES                                  ELSPTAGE
00516                  MSO-MAX-CHOICES.                                 ELSPTAGE
00517      MOVE +1              TO    MSO-OPT-LEN.                      ELSPTAGE
00518      SET MSO-OPT-TYP-NUM  TO    TRUE.                             ELSPTAGE
00519                                                                   ELSPTAGE
00520                                                                   ELSPTAGE
00521 ************************************************************      ELSPTAGE
00522 *                                                          *      ELSPTAGE
00523 *        LOAD TABLES                                       *      ELSPTAGE
00524 *                                                          *      ELSPTAGE
00525 ************************************************************      ELSPTAGE
00526  LOAD-TABLES.                                                     ELSPTAGE
00527      PERFORM LOAD-FAMILY-RELATION-TRANSLATI.                      ELSPTAGE
00528      PERFORM LOAD-FAMILY-RELATION-CODES-KEY.                      ELSPTAGE
00529      SET MSO-IDX UP BY 1.                                         ELSPTAGE
00530      EJECT                                                        ELSPTAGE
00531                                                                   ELSPTAGE
00532                                                                   ELSPTAGE
00533 ************************************************************      ELSPTAGE
00534 *                                                          *      ELSPTAGE
00535 *        LOAD FAMILY RELATION TRANSLATIONS                 *      ELSPTAGE
00536 *                                                          *      ELSPTAGE
00537 ************************************************************      ELSPTAGE
00538  LOAD-FAMILY-RELATION-TRANSLATI.                                  ELSPTAGE
00539      PERFORM TRANSLATE-FAMILY-RELATION-CODE.                      ELSPTAGE
00540      PERFORM SET-IOPM-ADDRESS-FOR-MENU-FILE.                      ELSPTAGE
00541      SET IOP-ADD       TO  TRUE.                                  ELSPTAGE
00542      SET IOP-FCQ-NONE  TO  TRUE.                                  ELSPTAGE
00543      SET IOP-KVQ-NONE  TO  TRUE.                                  ELSPTAGE
00544      SET CIA-ELSMENU-DDN   TO TRUE.                               ELSPTAGE
00545      SET WS-SEL-NBR    TO  WS-FR-IDX.                             ELSPTAGE
00546      SUBTRACT  1  FROM  WS-SEL-NBR.                               ELSPTAGE
00547      STRING WS-SEL-NBR-X '  ' CMF-DESCR-LINE (1)                  ELSPTAGE
00548            DELIMITED BY SIZE                                      ELSPTAGE
00549            INTO MSD-DESCR-LINE (MSD-IDX).                         ELSPTAGE
00550      MOVE LENGTH OF MSD-MENU-ITEM-DESCRIPTIONS  TO                ELSPTAGE
00551          IOP-REC-LEN.                                             ELSPTAGE
00552      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSPTAGE
00553      EJECT                                                        ELSPTAGE
00554                                                                   ELSPTAGE
00555                                                                   ELSPTAGE
00556 ************************************************************      ELSPTAGE
00557 *                                                          *      ELSPTAGE
00558 *        TRANSLATE FAMILY RELATION CODE                    *      ELSPTAGE
00559 *                                                          *      ELSPTAGE
00560 ************************************************************      ELSPTAGE
00561  TRANSLATE-FAMILY-RELATION-CODE.                                  ELSPTAGE
00562      MOVE WS-FR-ENTRY (WS-FR-IDX)  TO  CMF-CODE-VALUE.            ELSPTAGE
00563      MOVE 'SSB-PT-AGE'             TO  CMF-ELEMENT-SYSTEM-NAME.   ELSPTAGE
00564      MOVE '@ELS    '               TO  CMF-RECORD-PREFIX.         ELSPTAGE
00565      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELSPTAGE
00566                                                                   ELSPTAGE
00567                                                                   ELSPTAGE
00568 ************************************************************      ELSPTAGE
00569 *                                                          *      ELSPTAGE
00570 *        CALL CODES MANUAL INTERFACE                       *      ELSPTAGE
00571 *                                                          *      ELSPTAGE
00572 ************************************************************      ELSPTAGE
00573  CALL-CODES-MANUAL-INTERFACE.                                     ELSPTAGE
00574      INITIALIZE CMF-RETURN-CODE.                                  ELSPTAGE
00575      EXEC CICS LINK PROGRAM ('ELUCMIF')                           ELSPTAGE
00576                     COMMAREA (DFHCOMMAREA)                        ELSPTAGE
00577                     END-EXEC.                                     ELSPTAGE
00578      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELSPTAGE
00579      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00580          ADDRESS OF CMF-DESCR.                                    ELSPTAGE
00581                                                                   ELSPTAGE
00582                                                                   ELSPTAGE
00583 ************************************************************      ELSPTAGE
00584 *                                                          *      ELSPTAGE
00585 *        LOAD FAMILY RELATION CODES KEYWORD                *      ELSPTAGE
00586 *                                                          *      ELSPTAGE
00587 ************************************************************      ELSPTAGE
00588  LOAD-FAMILY-RELATION-CODES-KEY.                                  ELSPTAGE
00589      MOVE SPACES      TO  MSO-OPT-SEL (MSO-IDX).                  ELSPTAGE
00590      MOVE WS-SEL-NBR  TO  MSO-OPT-NUM-1 (MSO-IDX).                ELSPTAGE
00591      MOVE WS-FR-ENTRY (WS-FR-IDX)  TO  MSO-OPT-KWD                ELSPTAGE
00592          (MSO-IDX).                                               ELSPTAGE
00593                                                                   ELSPTAGE
00594                                                                   ELSPTAGE
00595 ************************************************************      ELSPTAGE
00596 *                                                          *      ELSPTAGE
00597 *        PROCESS PATIENT AGE MENU COMPLETED                *      ELSPTAGE
00598 *                                                          *      ELSPTAGE
00599 ************************************************************      ELSPTAGE
00600  PROCESS-PATIENT-AGE-MENU-COMPL.                                  ELSPTAGE
00601      MOVE SSB-MNU-CHOICE (1) TO SSB-PT-AGE.                       ELSPTAGE
00602      SET SSB-COMPLETED (SSB-SELECTOR-STATE)     TO TRUE.          ELSPTAGE
00603                                                                   ELSPTAGE
00604                                                                   ELSPTAGE
00605 ************************************************************      ELSPTAGE
00606 *                                                          *      ELSPTAGE
00607 *        CHECK COMMAREA LENGTH                             *      ELSPTAGE
00608 *                                                          *      ELSPTAGE
00609 ************************************************************      ELSPTAGE
00610  CHECK-COMMAREA-LENGTH.                                           ELSPTAGE
00611      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELSPTAGE
00612          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELSPTAGE
00613      EJECT                                                        ELSPTAGE
00614                                                                   ELSPTAGE
00615                                                                   ELSPTAGE
00616 ************************************************************      ELSPTAGE
00617 *                                                          *      ELSPTAGE
00618 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELSPTAGE
00619 *                                                          *      ELSPTAGE
00620 ************************************************************      ELSPTAGE
00621  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELSPTAGE
00622      SET CIA-AB-DFHCOMMAREA                                       ELSPTAGE
00623        TO TRUE.                                                   ELSPTAGE
00624      EXEC CICS ABEND                                              ELSPTAGE
00625                ABCODE(CIA-ABCODE)                                 ELSPTAGE
00626                END-EXEC.                                          ELSPTAGE
00627                                                                   ELSPTAGE
00628                                                                   ELSPTAGE
00629 ************************************************************      ELSPTAGE
00630 *                                                          *      ELSPTAGE
00631 *        CALL INPUT-OUTPUT SUBPROGRAM                      *      ELSPTAGE
00632 *                                                          *      ELSPTAGE
00633 ************************************************************      ELSPTAGE
00634  CALL-INPUT-OUTPUT-SUBPROGRAM.                                    ELSPTAGE
00635      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELSPTAGE
00636                     COMMAREA(DFHCOMMAREA)                         ELSPTAGE
00637                     END-EXEC.                                     ELSPTAGE
00638      EJECT                                                        ELSPTAGE
00639                                                                   ELSPTAGE
00640                                                                   ELSPTAGE
00641 ************************************************************      ELSPTAGE
00642 *                                                          *      ELSPTAGE
00643 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELSPTAGE
00644 *                                                          *      ELSPTAGE
00645 ************************************************************      ELSPTAGE
00646  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELSPTAGE
00647      IF ECA-CIA-PTR IS NOT EQUAL NULL                             ELSPTAGE
00648          PERFORM SET-CIA-ADDRESS                                  ELSPTAGE
00649      ELSE                                                         ELSPTAGE
00650          PERFORM SIGNAL-CIA-ADDRESSING-ERROR.                     ELSPTAGE
00651                                                                   ELSPTAGE
00652                                                                   ELSPTAGE
00653 ************************************************************      ELSPTAGE
00654 *                                                          *      ELSPTAGE
00655 *        SET CIA ADDRESS                                   *      ELSPTAGE
00656 *                                                          *      ELSPTAGE
00657 ************************************************************      ELSPTAGE
00658  SET-CIA-ADDRESS.                                                 ELSPTAGE
00659      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSPTAGE
00660          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSPTAGE
00661                                                                   ELSPTAGE
00662                                                                   ELSPTAGE
00663 ************************************************************      ELSPTAGE
00664 *                                                          *      ELSPTAGE
00665 *        SIGNAL CIA ADDRESSING ERROR                       *      ELSPTAGE
00666 *                                                          *      ELSPTAGE
00667 ************************************************************      ELSPTAGE
00668  SIGNAL-CIA-ADDRESSING-ERROR.                                     ELSPTAGE
00669      SET  CIA-AB-ELSCIA-PTR                                       ELSPTAGE
00670         TO TRUE.                                                  ELSPTAGE
00671      EXEC CICS ABEND                                              ELSPTAGE
00672                ABCODE(CIA-ABCODE)                                 ELSPTAGE
00673                END-EXEC.                                          ELSPTAGE
00674      EJECT                                                        ELSPTAGE
00675                                                                   ELSPTAGE
00676                                                                   ELSPTAGE
00677 ************************************************************      ELSPTAGE
00678 *                                                          *      ELSPTAGE
00679 *        ESTABLISH ADDRESSING TO SELECTOR CONTROL AREA     *      ELSPTAGE
00680 *                                                          *      ELSPTAGE
00681 ************************************************************      ELSPTAGE
00682  ESTABLISH-ADDRESSING-TO-SELECT.                                  ELSPTAGE
00683      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSPTAGE
00684      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00685          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSPTAGE
00686      IF CIA-RC-PTR-NULL                                           ELSPTAGE
00687          PERFORM SIGNAL-MISSING-PARAMETER                         ELSPTAGE
00688      ELSE                                                         ELSPTAGE
00689          PERFORM SET-SSCB-ADDRESS.                                ELSPTAGE
00690                                                                   ELSPTAGE
00691                                                                   ELSPTAGE
00692 ************************************************************      ELSPTAGE
00693 *                                                          *      ELSPTAGE
00694 *        SET SSCB ADDRESS                                  *      ELSPTAGE
00695 *                                                          *      ELSPTAGE
00696 ************************************************************      ELSPTAGE
00697  SET-SSCB-ADDRESS.                                                ELSPTAGE
00698      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSPTAGE
00699      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00700          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSPTAGE
00701                                                                   ELSPTAGE
00702                                                                   ELSPTAGE
00703 ************************************************************      ELSPTAGE
00704 *                                                          *      ELSPTAGE
00705 *        SIGNAL MISSING PARAMETER                          *      ELSPTAGE
00706 *                                                          *      ELSPTAGE
00707 ************************************************************      ELSPTAGE
00708  SIGNAL-MISSING-PARAMETER.                                        ELSPTAGE
00709      SET CIA-AB-PARM-MISSING TO TRUE.                             ELSPTAGE
00710      EXEC CICS ABEND ABCODE(CIA-ABCODE)                           ELSPTAGE
00711                END-EXEC.                                          ELSPTAGE
00712      EJECT                                                        ELSPTAGE
00713                                                                   ELSPTAGE
00714                                                                   ELSPTAGE
00715 ************************************************************      ELSPTAGE
00716 *                                                          *      ELSPTAGE
00717 *        ESTABLISH ADDRESSING TO KEY WORK AREA             *      ELSPTAGE
00718 *                                                          *      ELSPTAGE
00719 ************************************************************      ELSPTAGE
00720  ESTABLISH-ADDRESSING-TO-KEY-WO.                                  ELSPTAGE
00721      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELSPTAGE
00722      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00723          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELSPTAGE
00724      IF CIA-RC-PTR-NULL                                           ELSPTAGE
00725          PERFORM ALLOCATE-KEY-WORK-AREA.                          ELSPTAGE
00726      PERFORM SET-KWA-ADDRESS.                                     ELSPTAGE
00727                                                                   ELSPTAGE
00728                                                                   ELSPTAGE
00729 ************************************************************      ELSPTAGE
00730 *                                                          *      ELSPTAGE
00731 *        ALLOCATE KEY WORK AREA                            *      ELSPTAGE
00732 *                                                          *      ELSPTAGE
00733 ************************************************************      ELSPTAGE
00734  ALLOCATE-KEY-WORK-AREA.                                          ELSPTAGE
00735      SET CIA-ELSKEYS-DDN   TO  TRUE.                              ELSPTAGE
00736      MOVE ZERO             TO  CIA-AREA-LEN.                      ELSPTAGE
00737      SET CIA-STG-GETMAIN   TO  TRUE.                              ELSPTAGE
00738      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSPTAGE
00739                                                                   ELSPTAGE
00740                                                                   ELSPTAGE
00741 ************************************************************      ELSPTAGE
00742 *                                                          *      ELSPTAGE
00743 *        SET KWA ADDRESS                                   *      ELSPTAGE
00744 *                                                          *      ELSPTAGE
00745 ************************************************************      ELSPTAGE
00746  SET-KWA-ADDRESS.                                                 ELSPTAGE
00747      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELSPTAGE
00748      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00749          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELSPTAGE
00750      EJECT                                                        ELSPTAGE
00751                                                                   ELSPTAGE
00752                                                                   ELSPTAGE
00753 ************************************************************      ELSPTAGE
00754 *                                                          *      ELSPTAGE
00755 *        ESTABLISH ADDRESSING TO CONTRACT KEY TBL AREA     *      ELSPTAGE
00756 *                                                          *      ELSPTAGE
00757 ************************************************************      ELSPTAGE
00758  ESTABLISH-ADDRESSING-TO-CONTRA.                                  ELSPTAGE
00759      SET CIA-ELSKTBC-DDN TO TRUE.                                 ELSPTAGE
00760      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00761          ADDRESS OF KTC-GCCONTR-KEY-TABLE.                        ELSPTAGE
00762      IF CIA-RC-PTR-NULL                                           ELSPTAGE
00763         PERFORM RETRIEVE-THE-CONTRACT-KEY-TBLX.                   ELSPTAGE
00764      PERFORM SET-CONTRACT-KEY-TBL-ADDRESS.                        ELSPTAGE
00765                                                                   ELSPTAGE
00766                                                                   ELSPTAGE
00767 ************************************************************      ELSPTAGE
00768 *                                                          *      ELSPTAGE
00769 *        RETRIEVE THE CONTRACT KEY TBL ADDRESS             *      ELSPTAGE
00770 *                                                          *      ELSPTAGE
00771 ************************************************************      ELSPTAGE
00772  RETRIEVE-THE-CONTRACT-KEY-TBLX.                                  ELSPTAGE
00773      SET CIA-ELSKTBC-DDN   TO  TRUE.                              ELSPTAGE
00774      SET CIA-STG-RETRIEVE  TO  TRUE.                              ELSPTAGE
00775      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSPTAGE
00776                                                                   ELSPTAGE
00777                                                                   ELSPTAGE
00778 ************************************************************      ELSPTAGE
00779 *                                                          *      ELSPTAGE
00780 *        SET CONTRACT KEY TBL ADDRESS                      *      ELSPTAGE
00781 *                                                          *      ELSPTAGE
00782 ************************************************************      ELSPTAGE
00783  SET-CONTRACT-KEY-TBL-ADDRESS.                                    ELSPTAGE
00784      SET CIA-ELSKTBC-DDN TO TRUE.                                 ELSPTAGE
00785      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00786          ADDRESS OF KTC-GCCONTR-KEY-TABLE.                        ELSPTAGE
00787      EJECT                                                        ELSPTAGE
00788                                                                   ELSPTAGE
00789                                                                   ELSPTAGE
00790 ************************************************************      ELSPTAGE
00791 *                                                          *      ELSPTAGE
00792 *        ESTABLISH ADDRESSING TO GROUP SPECIFIC KEY TBL ARE*      ELSPTAGE
00793 *                                                          *      ELSPTAGE
00794 ************************************************************      ELSPTAGE
00795  ESTABLISH-ADDRESSING-TO-GROUPX.                                  ELSPTAGE
00796      SET CIA-ELSKTBG-DDN TO TRUE.                                 ELSPTAGE
00797      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00798          ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                       ELSPTAGE
00799      IF CIA-RC-PTR-NULL                                           ELSPTAGE
00800          PERFORM RETRIEVE-THE-GROUP-SPECIFIC-TB.                  ELSPTAGE
00801      PERFORM SET-GROUP-SPECIFIC-KEY-TBL-ADD.                      ELSPTAGE
00802                                                                   ELSPTAGE
00803                                                                   ELSPTAGE
00804 ************************************************************      ELSPTAGE
00805 *                                                          *      ELSPTAGE
00806 *        RETRIEVE THE GROUP SPECIFIC TBL ADDRESS           *      ELSPTAGE
00807 *                                                          *      ELSPTAGE
00808 ************************************************************      ELSPTAGE
00809  RETRIEVE-THE-GROUP-SPECIFIC-TB.                                  ELSPTAGE
00810      SET  CIA-ELSKTBG-DDN  TO  TRUE.                              ELSPTAGE
00811      SET CIA-STG-RETRIEVE  TO  TRUE.                              ELSPTAGE
00812      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSPTAGE
00813                                                                   ELSPTAGE
00814                                                                   ELSPTAGE
00815 ************************************************************      ELSPTAGE
00816 *                                                          *      ELSPTAGE
00817 *        SET GROUP SPECIFIC KEY TBL ADDRESS                *      ELSPTAGE
00818 *                                                          *      ELSPTAGE
00819 ************************************************************      ELSPTAGE
00820  SET-GROUP-SPECIFIC-KEY-TBL-ADD.                                  ELSPTAGE
00821      SET CIA-ELSKTBG-DDN TO TRUE.                                 ELSPTAGE
00822      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00823          ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                       ELSPTAGE
00824      EJECT                                                        ELSPTAGE
00825                                                                   ELSPTAGE
00826                                                                   ELSPTAGE
00827 ************************************************************      ELSPTAGE
00828 *                                                          *      ELSPTAGE
00829 *        ESTABLISH ADDRESSING TO CODES MANUAL INTERFACE ARE*      ELSPTAGE
00830 *                                                          *      ELSPTAGE
00831 ************************************************************      ELSPTAGE
00832  ESTABLISH-ADDRESSING-TO-CODESX.                                  ELSPTAGE
00833      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSPTAGE
00834      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00835          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELSPTAGE
00836      IF CIA-RC-PTR-NULL                                           ELSPTAGE
00837         PERFORM ALLOCATE-CODES-MANUAL-AREA.                       ELSPTAGE
00838      PERFORM SET-CODES-MANUAL-ADDRESS.                            ELSPTAGE
00839                                                                   ELSPTAGE
00840                                                                   ELSPTAGE
00841 ************************************************************      ELSPTAGE
00842 *                                                          *      ELSPTAGE
00843 *        ALLOCATE CODES MANUAL AREA                        *      ELSPTAGE
00844 *                                                          *      ELSPTAGE
00845 ************************************************************      ELSPTAGE
00846  ALLOCATE-CODES-MANUAL-AREA.                                      ELSPTAGE
00847      SET CIA-ELSCMIF-DDN   TO  TRUE.                              ELSPTAGE
00848      SET CIA-STG-GETMAIN   TO  TRUE.                              ELSPTAGE
00849      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSPTAGE
00850                                                                   ELSPTAGE
00851                                                                   ELSPTAGE
00852 ************************************************************      ELSPTAGE
00853 *                                                          *      ELSPTAGE
00854 *        SET CODES MANUAL ADDRESS                          *      ELSPTAGE
00855 *                                                          *      ELSPTAGE
00856 ************************************************************      ELSPTAGE
00857  SET-CODES-MANUAL-ADDRESS.                                        ELSPTAGE
00858      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSPTAGE
00859      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPTAGE
00860          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELSPTAGE
00861      EJECT                                                        ELSPTAGE
00862                                                                   ELSPTAGE
00863                                                                   ELSPTAGE
00864 ************************************************************      ELSPTAGE
00865 *                                                          *      ELSPTAGE
00866 *        INITIALIZE FAMILY REL ARRAY                       *      ELSPTAGE
00867 *                                                          *      ELSPTAGE
00868 ************************************************************      ELSPTAGE
00869  INITIALIZE-FAMILY-REL-ARRAY.                                     ELSPTAGE
00870      MOVE +2            TO  WS-NBR-FR.                            ELSPTAGE
00871      MOVE LOW-VALUES    TO  WS-FR-ENTRY (1).                      ELSPTAGE
00872      MOVE HIGH-VALUES   TO  WS-FR-ENTRY (2).                      ELSPTAGE
00873      SET WS-FR-MAX-IDX  TO  2.                                    ELSPTAGE
00874                                                                   ELSPTAGE
00875                                                                   ELSPTAGE
00876 ************************************************************      ELSPTAGE
00877 *                                                          *      ELSPTAGE
00878 *        SIGNAL UNKNOWN ERROR                              *      ELSPTAGE
00879 *                                                          *      ELSPTAGE
00880 ************************************************************      ELSPTAGE
00881  SIGNAL-UNKNOWN-ERROR.                                            ELSPTAGE
00882      SET CIA-AB-UNDEF TO TRUE.                                    ELSPTAGE
00883      PERFORM ABEND-THE-PROGRAM.                                   ELSPTAGE
00884      EJECT                                                        ELSPTAGE
00885                                                                   ELSPTAGE
00886                                                                   ELSPTAGE
00887 ************************************************************      ELSPTAGE
00888 *                                                          *      ELSPTAGE
00889 *        ABEND THE PROGRAM                                 *      ELSPTAGE
00890 *                                                          *      ELSPTAGE
00891 ************************************************************      ELSPTAGE
00892  ABEND-THE-PROGRAM.                                               ELSPTAGE
00893      EXEC CICS ABEND                                              ELSPTAGE
00894                ABCODE(CIA-ABCODE)                                 ELSPTAGE
00895                END-EXEC.                                          ELSPTAGE
