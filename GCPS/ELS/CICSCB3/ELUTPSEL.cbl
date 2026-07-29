00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUTPSEL
00003  PROGRAM-ID.         ELUTPSEL.                                       LV001
00004                                                                   ELUTPSEL
00005  AUTHOR.             EDWARD G LISS                                ELUTPSEL
00006                                                                   ELUTPSEL
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUTPSEL
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUTPSEL
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUTPSEL
00010                      233 N. MICHIGAN AVE                          ELUTPSEL
00011                      CHICAGO, ILLINOIS 60601                      ELUTPSEL
00012                                                                   ELUTPSEL
00013  DATE-WRITTEN.       14-NOV-1986.                                 ELUTPSEL
00014                                                                   ELUTPSEL
00015  DATE-COMPILED.                                                   ELUTPSEL
00016                                                                   ELUTPSEL
00017  SECURITY.           COPYRIGHT 1986,                              ELUTPSEL
00018                      HEALTH CARE SERVICE CORPORATION              ELUTPSEL
00019      SKIP3                                                        ELUTPSEL
00020  ENVIRONMENT DIVISION.                                            ELUTPSEL
00021                                                                   ELUTPSEL
00022  CONFIGURATION SECTION.                                           ELUTPSEL
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELUTPSEL
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELUTPSEL
00025      EJECT                                                        ELUTPSEL
00026 ******************************************************************ELUTPSEL
00027 *                                                                *ELUTPSEL
00028 *    PROGRAM:    ELUTPSEL                                        *ELUTPSEL
00029 *    DATE:       14-NOV-1986                                     *ELUTPSEL
00030 *    AUTHOR:     EDWARD G LISS                                   *ELUTPSEL
00031 *    FUNCTION:                                                   *ELUTPSEL
00032 *       THIS MODULE DETEMINES WHICH TOPIC SELECTORS NEED         *ELUTPSEL
00033 *       TO BE RUN.                                               *ELUTPSEL
00034 *                                                                *ELUTPSEL
00035 *    NOTES:                                                      *ELUTPSEL
00036 *                                                                *ELUTPSEL
00037 ******************************************************************ELUTPSEL
00038 *                                                                *ELUTPSEL
00039 *                      MAINTENANCE HISTORY                       *ELUTPSEL
00040 *                                                                *ELUTPSEL
00041 *  MOD     DATE     BY  DRPT                ACTION               *ELUTPSEL
00042 * ----- ----------- --- ----- ---------------------------------- *ELUTPSEL
00043 * 01.00 14-NOV-1986 EGL       CREATED                            *ELUTPSEL
00044 * 01.01 19-JUN-1987 EGL       MODIFIED TO ALLOW ALLOW PRE-       *ELUTPSEL
00045 *                             SELECTION TO DERIVE AN \
00046 *                             RESPONSE\
00047 *                             IS NOT FOUND IN THE TOPIC TABLE    *ELUTPSEL
00048 *                             FOR THE PARTICULAR ATTRIBUTE.      *ELUTPSEL
00049 * 01.02 27-JUL-1987 EGL       MODIFIED TO SUPPORT IMPROVED       *ELUTPSEL
00050 *                             RESELECTION.                       *ELUTPSEL
00051 * 01.03 23-NOV-1987 EGL       MODIFIED TO SUPPORT STACKED MENU   *ELUTPSEL
00052 *                             PROCESSING - SET THE               *ELUTPSEL
00053 *                                SSB-PUSH-INDICATOR              *ELUTPSEL
00054 *                             TO ACCOMPLISH THIS.                *ELUTPSEL
00055 * 01.04 06-SEP-1989 EGL       DESTRUCTED PROGRAM AND MADE        *ELUTPSEL
00056 *                             STORAGE ENHANCEMENT CHANGES        *ELUTPSEL
00057 *                                                                *ELUTPSEL
00058 ******************************************************************ELUTPSEL
00059      EJECT                                                        ELUTPSEL
00060  DATA DIVISION.                                                   ELUTPSEL
00061                                                                   ELUTPSEL
00062  WORKING-STORAGE SECTION.                                         ELUTPSEL
00063  01  WS-MISC-STUFF.                                               ELUTPSEL
00064      05  WS-SUB-TOPIC-SW           PICTURE X.                     ELUTPSEL
00065          88  WS-SUB-TOPIC                  VALUE 'Y'.             ELUTPSEL
00066          88  WS-NO-SUB-TOPIC               VALUE 'N'.             ELUTPSEL
00067                                                                   ELUTPSEL
00068      05  WS-SEARCH-IND             PICTURE X.                     ELUTPSEL
00069          88  WS-SEARCH-OK                  VALUE 'Y'.             ELUTPSEL
00070          88  WS-SEARCH-FAILED              VALUE 'N'.             ELUTPSEL
00071                                                                   ELUTPSEL
00072      05  WS-ATTR-IND               PICTURE X.                     ELUTPSEL
00073          88  WS-ATTR-FOUND                 VALUE 'Y'.             ELUTPSEL
00074          88  WS-ATTR-NOT-FOUND             VALUE 'N'.             ELUTPSEL
00075                                                                   ELUTPSEL
00076      05  WS-INPUT-IND              PICTURE X.                     ELUTPSEL
00077          88  WS-DERIVED-INPUT              VALUE 'D'.             ELUTPSEL
00078          88  WS-USER-INPUT                 VALUE 'U'.             ELUTPSEL
00079          88  WS-DATA-UNAVAIL               VALUE 'X'.             ELUTPSEL
00080                                                                   ELUTPSEL
00081      05  WS-TOPIC-IND              PICTURE X(12) VALUE '&'.       ELUTPSEL
00082      05  WS-GENERIC-IND            PICTURE X(12) VALUE '*'.       ELUTPSEL
00083      05  WS-FIRST-IDX              PICTURE S9(4) COMP SYNC.       ELUTPSEL
00084      05  WS-LAST-IDX               PICTURE S9(4) COMP SYNC.       ELUTPSEL
00085      EJECT                                                        ELUTPSEL
00086  COPY ELSTPTBC.                                                   ELUTPSEL
00087      EJECT                                                        ELUTPSEL
00088  LINKAGE SECTION.                                                 ELUTPSEL
00089  01  DFHCOMMAREA.                                                 ELUTPSEL
00090  COPY ELSCOMMC.                                                   ELUTPSEL
00091      EJECT                                                        ELUTPSEL
00092  COPY ELSCIA2C.                                                   ELUTPSEL
00093      EJECT                                                        ELUTPSEL
00094  COPY ELSSSCBC.                                                   ELUTPSEL
00095      EJECT                                                        ELUTPSEL
00096  PROCEDURE DIVISION.                                              ELUTPSEL
00097 ************************************************************      ELUTPSEL
00098 *                                                          *      ELUTPSEL
00099 *        TOPIC SELECTION                                   *      ELUTPSEL
00100 *                                                          *      ELUTPSEL
00101 ************************************************************      ELUTPSEL
00102  TOPIC-SELECTION.                                                 ELUTPSEL
00103      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELUTPSEL
00104          PERFORM INVALID-COMMAREA-ABEND.                          ELUTPSEL
00105      PERFORM INITIALIZATION.                                      ELUTPSEL
00106      PERFORM PROCESS-TOPICS.                                      ELUTPSEL
00107      EXEC CICS RETURN                                             ELUTPSEL
00108                END-EXEC.                                          ELUTPSEL
00109                                                                   ELUTPSEL
00110                                                                   ELUTPSEL
00111 ************************************************************      ELUTPSEL
00112 *                                                          *      ELUTPSEL
00113 *        INITIALIZATION                                    *      ELUTPSEL
00114 *                                                          *      ELUTPSEL
00115 ************************************************************      ELUTPSEL
00116  INITIALIZATION.                                                  ELUTPSEL
00117      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUTPSEL
00118                 ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.         ELUTPSEL
00119      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELUTPSEL
00120      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUTPSEL
00121                 ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.           ELUTPSEL
00122      MOVE SPACES TO SSB-ACTION-MODULE                             ELUTPSEL
00123                     SSB-PUSH-INDICATOR.                           ELUTPSEL
00124      EJECT                                                        ELUTPSEL
00125 ************************************************************      ELUTPSEL
00126 *                                                          *      ELUTPSEL
00127 *        PROCESS TOPICS                                    *      ELUTPSEL
00128 *                                                          *      ELUTPSEL
00129 ************************************************************      ELUTPSEL
00130  PROCESS-TOPICS.                                                  ELUTPSEL
00131      PERFORM CHECK-ALL-TOPICS                                     ELUTPSEL
00132          VARYING SSB-SELECTOR-STATE                               ELUTPSEL
00133              FROM SSB-SELECTOR-STATE BY 1                         ELUTPSEL
00134              UNTIL SSB-SS-SELECTION-DONE                          ELUTPSEL
00135                  OR NOT CIA-SEL-TYP-TOP (SSB-SELECTOR-STATE)      ELUTPSEL
00136                  OR SSB-ACTION-MODULE NOT = SPACES.               ELUTPSEL
00137      IF SSB-ACTION-MODULE NOT = SPACES                            ELUTPSEL
00138          SUBTRACT 1 FROM SSB-SELECTOR-STATE.                      ELUTPSEL
00139                                                                   ELUTPSEL
00140                                                                   ELUTPSEL
00141 ************************************************************      ELUTPSEL
00142 *                                                          *      ELUTPSEL
00143 *        CHECK ALL TOPICS                                  *      ELUTPSEL
00144 *                                                          *      ELUTPSEL
00145 ************************************************************      ELUTPSEL
00146  CHECK-ALL-TOPICS.                                                ELUTPSEL
00147      SET WS-ATTR-NOT-FOUND TO TRUE.                               ELUTPSEL
00148      SET WS-USER-INPUT TO TRUE.                                   ELUTPSEL
00149      IF SSB-RESELECTION (SSB-SELECTOR-STATE)                      ELUTPSEL
00150               OR SSB-SEL-DATA-AVAIL (SSB-SELECTOR-STATE)          ELUTPSEL
00151          PERFORM PROCESS-INPUT-SELECTIONS.                        ELUTPSEL
00152      IF WS-ATTR-NOT-FOUND                                         ELUTPSEL
00153          PERFORM CHECK-CURRENT-TOPIC.                             ELUTPSEL
00154      IF WS-DERIVED-INPUT                                          ELUTPSEL
00155          PERFORM PROCESS-INPUT-SELECTIONS.                        ELUTPSEL
00156      EJECT                                                        ELUTPSEL
00157 ************************************************************      ELUTPSEL
00158 *                                                          *      ELUTPSEL
00159 *        PROCESS INPUT SELECTIONS                          *      ELUTPSEL
00160 *                                                          *      ELUTPSEL
00161 ************************************************************      ELUTPSEL
00162  PROCESS-INPUT-SELECTIONS.                                        ELUTPSEL
00163      SET TST-FIRST-IDX TO SSB-TOP-SEL-FIRST.                      ELUTPSEL
00164      SET TST-LAST-IDX  TO SSB-TOP-SEL-LAST.                       ELUTPSEL
00165      EVALUATE TRUE                                                ELUTPSEL
00166      WHEN SSB-SS-GET-TOPIC                                        ELUTPSEL
00167          PERFORM PROCESS-MAIN-TOPIC                               ELUTPSEL
00168      WHEN SSB-SS-GET-SUBTOPIC                                     ELUTPSEL
00169          PERFORM PROCESS-SUB-TOPIC                                ELUTPSEL
00170      WHEN SSB-SS-GET-PROVIDER-CLASS                               ELUTPSEL
00171          PERFORM PROCESS-PROVIDER-CLASS                           ELUTPSEL
00172      WHEN SSB-SS-GET-SERVICE-LOC                                  ELUTPSEL
00173          PERFORM PROCESS-SERVICE-LOCATION                         ELUTPSEL
00174      WHEN SSB-SS-GET-MODIFIER-1                                   ELUTPSEL
00175          PERFORM PROCESS-MODIFIER-1                               ELUTPSEL
00176      WHEN SSB-SS-GET-MODIFIER-2                                   ELUTPSEL
00177          PERFORM PROCESS-MODIFIER-2                               ELUTPSEL
00178      WHEN OTHER                                                   ELUTPSEL
00179          PERFORM SYSTEM-LOGIC-ERROR.                              ELUTPSEL
00180 /***********************************************************      ELUTPSEL
00181 *                                                          *      ELUTPSEL
00182 *        PROCESS MAIN TOPIC                                *      ELUTPSEL
00183 *                                                          *      ELUTPSEL
00184 ************************************************************      ELUTPSEL
00185  PROCESS-MAIN-TOPIC.                                              ELUTPSEL
00186      PERFORM ELIMINATE-LOW-TOPICS.                                ELUTPSEL
00187      PERFORM ELIMINATE-HIGH-TOPICS.                               ELUTPSEL
00188      PERFORM ACCEPT-ATTRIBUTE.                                    ELUTPSEL
00189                                                                   ELUTPSEL
00190                                                                   ELUTPSEL
00191 ************************************************************      ELUTPSEL
00192 *                                                          *      ELUTPSEL
00193 *        ELIMINATE LOW TOPICS                              *      ELUTPSEL
00194 *                                                          *      ELUTPSEL
00195 ************************************************************      ELUTPSEL
00196  ELIMINATE-LOW-TOPICS.                                            ELUTPSEL
00197      MOVE ZERO TO SSB-TOP-SEL-FIRST.                              ELUTPSEL
00198      SET WS-SEARCH-FAILED TO TRUE.                                ELUTPSEL
00199      PERFORM VARYING WS-FIRST-IDX FROM 1 BY 1                     ELUTPSEL
00200          UNTIL WS-FIRST-IDX > TST-NUMBER-OF-ITEMS                 ELUTPSEL
00201             OR WS-SEARCH-OK                                       ELUTPSEL
00202          IF TST-TOPIC (WS-FIRST-IDX) = SSB-TOPIC                  ELUTPSEL
00203              SET TST-FIRST-IDX TO WS-FIRST-IDX                    ELUTPSEL
00204              SET WS-SEARCH-OK TO TRUE                             ELUTPSEL
00205          END-IF                                                   ELUTPSEL
00206      END-PERFORM.                                                 ELUTPSEL
00207      IF WS-SEARCH-FAILED                                          ELUTPSEL
00208          PERFORM TABLE-SYNC-ERROR.                                ELUTPSEL
00209                                                                   ELUTPSEL
00210                                                                   ELUTPSEL
00211 ************************************************************      ELUTPSEL
00212 *                                                          *      ELUTPSEL
00213 *        ELIMINATE HIGH TOPICS                             *      ELUTPSEL
00214 *                                                          *      ELUTPSEL
00215 ************************************************************      ELUTPSEL
00216  ELIMINATE-HIGH-TOPICS.                                           ELUTPSEL
00217      SET WS-SEARCH-FAILED TO TRUE.                                ELUTPSEL
00218      PERFORM VARYING WS-LAST-IDX FROM WS-FIRST-IDX BY 1           ELUTPSEL
00219             UNTIL WS-LAST-IDX > TST-NUMBER-OF-ITEMS               ELUTPSEL
00220                OR WS-SEARCH-OK                                    ELUTPSEL
00221          IF TST-TOPIC (WS-LAST-IDX) NOT = SSB-TOPIC               ELUTPSEL
00222              SET TST-LAST-IDX TO WS-LAST-IDX                      ELUTPSEL
00223              SET TST-LAST-IDX DOWN BY 1                           ELUTPSEL
00224              SET WS-SEARCH-OK TO TRUE                             ELUTPSEL
00225          END-IF                                                   ELUTPSEL
00226      END-PERFORM.                                                 ELUTPSEL
00227      IF WS-SEARCH-FAILED                                          ELUTPSEL
00228           SET TST-LAST-IDX TO TST-NUMBER-OF-ITEMS.                ELUTPSEL
00229      EJECT                                                        ELUTPSEL
00230 ************************************************************      ELUTPSEL
00231 *                                                          *      ELUTPSEL
00232 *        PROCESS SUB TOPIC                                 *      ELUTPSEL
00233 *                                                          *      ELUTPSEL
00234 ************************************************************      ELUTPSEL
00235  PROCESS-SUB-TOPIC.                                               ELUTPSEL
00236      IF SSB-RESELECTION (SSB-SELECTOR-STATE)                      ELUTPSEL
00237          PERFORM RESELECT-SUB-TOPIC                               ELUTPSEL
00238      ELSE                                                         ELUTPSEL
00239          IF TST-SUB-TOPIC (TST-FIRST-IDX) = WS-GENERIC-IND        ELUTPSEL
00240              PERFORM ACCEPT-ATTRIBUTE                             ELUTPSEL
00241          ELSE                                                     ELUTPSEL
00242              PERFORM ELIMINATE-OTHER-SUB-TOPICS.                  ELUTPSEL
00243                                                                   ELUTPSEL
00244                                                                   ELUTPSEL
00245 ************************************************************      ELUTPSEL
00246 *                                                          *      ELUTPSEL
00247 *        RESELECT SUB TOPIC                                *      ELUTPSEL
00248 *                                                          *      ELUTPSEL
00249 ************************************************************      ELUTPSEL
00250  RESELECT-SUB-TOPIC.                                              ELUTPSEL
00251      IF TST-SUB-TOPIC (TST-FIRST-IDX + 1) =  WS-GENERIC-IND       ELUTPSEL
00252          PERFORM RESELECT-ATTRIBUTE                               ELUTPSEL
00253      ELSE                                                         ELUTPSEL
00254          IF TST-SUB-TOPIC (TST-FIRST-IDX) =  WS-TOPIC-IND         ELUTPSEL
00255              PERFORM ELIMINATE-OTHER-SUB-TOPICS                   ELUTPSEL
00256          ELSE                                                     ELUTPSEL
00257              PERFORM FORCE-SUB-TOPIC-VALUE.                       ELUTPSEL
00258                                                                   ELUTPSEL
00259                                                                   ELUTPSEL
00260 ************************************************************      ELUTPSEL
00261 *                                                          *      ELUTPSEL
00262 *        FORCE SUB TOPIC VALUE                             *      ELUTPSEL
00263 *                                                          *      ELUTPSEL
00264 ************************************************************      ELUTPSEL
00265  FORCE-SUB-TOPIC-VALUE.                                           ELUTPSEL
00266      SET WS-DERIVED-INPUT TO TRUE.                                ELUTPSEL
00267      PERFORM ELIMINATE-OTHER-SUB-TOPICS.                          ELUTPSEL
00268      EJECT                                                        ELUTPSEL
00269 ************************************************************      ELUTPSEL
00270 *                                                          *      ELUTPSEL
00271 *        ELIMINATE OTHER SUB TOPICS                        *      ELUTPSEL
00272 *                                                          *      ELUTPSEL
00273 ************************************************************      ELUTPSEL
00274  ELIMINATE-OTHER-SUB-TOPICS.                                      ELUTPSEL
00275      PERFORM ELIMINATE-LOW-SUB-TOPICS.                            ELUTPSEL
00276      IF WS-SEARCH-OK                                              ELUTPSEL
00277          PERFORM ELIMINATE-HIGH-SUB-TOPICS.                       ELUTPSEL
00278                                                                   ELUTPSEL
00279                                                                   ELUTPSEL
00280 ************************************************************      ELUTPSEL
00281 *                                                          *      ELUTPSEL
00282 *        ELIMINATE LOW SUB TOPICS                          *      ELUTPSEL
00283 *                                                          *      ELUTPSEL
00284 ************************************************************      ELUTPSEL
00285  ELIMINATE-LOW-SUB-TOPICS.                                        ELUTPSEL
00286      SET WS-SEARCH-FAILED TO TRUE.                                ELUTPSEL
00287      PERFORM VARYING WS-FIRST-IDX FROM SSB-TOP-SEL-FIRST BY 1     ELUTPSEL
00288              UNTIL WS-FIRST-IDX > SSB-TOP-SEL-LAST                ELUTPSEL
00289                 OR WS-SEARCH-OK                                   ELUTPSEL
00290          SET TST-FIRST-IDX TO WS-FIRST-IDX                        ELUTPSEL
00291          IF TST-SUB-TOPIC (WS-FIRST-IDX) = SSB-SUB-TOPIC          ELUTPSEL
00292              SET TST-FIRST-IDX TO WS-FIRST-IDX                    ELUTPSEL
00293              SET WS-SEARCH-OK TO TRUE                             ELUTPSEL
00294          END-IF                                                   ELUTPSEL
00295      END-PERFORM.                                                 ELUTPSEL
00296      IF WS-SEARCH-FAILED                                          ELUTPSEL
00297          IF SSB-RESELECTION (SSB-SELECTOR-STATE)                  ELUTPSEL
00298              PERFORM DETERMINE-STATUS-OF-SUB-TOPIC                ELUTPSEL
00299          ELSE                                                     ELUTPSEL
00300              PERFORM INTERNAL-TABLE-ERROR.                        ELUTPSEL
00301                                                                   ELUTPSEL
00302                                                                   ELUTPSEL
00303 ************************************************************      ELUTPSEL
00304 *                                                          *      ELUTPSEL
00305 *        DETERMINE STATUS OF SUB TOPIC                     *      ELUTPSEL
00306 *                                                          *      ELUTPSEL
00307 ************************************************************      ELUTPSEL
00308  DETERMINE-STATUS-OF-SUB-TOPIC.                                   ELUTPSEL
00309      IF TST-SUB-TOPIC (SSB-TOP-SEL-FIRST) = SPACES                ELUTPSEL
00310          PERFORM FLAG-INPUT-AS-UNAVAILABLE                        ELUTPSEL
00311      ELSE                                                         ELUTPSEL
00312          PERFORM PROMPT-USER-FOR-VALID-INPUT.                     ELUTPSEL
00313      EJECT                                                        ELUTPSEL
00314 ************************************************************      ELUTPSEL
00315 *                                                          *      ELUTPSEL
00316 *        ELIMINATE HIGH SUB TOPICS                         *      ELUTPSEL
00317 *                                                          *      ELUTPSEL
00318 ************************************************************      ELUTPSEL
00319  ELIMINATE-HIGH-SUB-TOPICS.                                       ELUTPSEL
00320      SET WS-SEARCH-FAILED TO TRUE.                                ELUTPSEL
00321      PERFORM VARYING WS-LAST-IDX FROM WS-FIRST-IDX BY 1           ELUTPSEL
00322              UNTIL WS-LAST-IDX > SSB-TOP-SEL-LAST                 ELUTPSEL
00323                 OR WS-SEARCH-OK                                   ELUTPSEL
00324          IF TST-SUB-TOPIC (WS-LAST-IDX) NOT = SSB-SUB-TOPIC       ELUTPSEL
00325              SET TST-LAST-IDX TO WS-LAST-IDX                      ELUTPSEL
00326              SET TST-LAST-IDX DOWN BY 1                           ELUTPSEL
00327              SET WS-SEARCH-OK TO TRUE                             ELUTPSEL
00328          END-IF                                                   ELUTPSEL
00329      END-PERFORM.                                                 ELUTPSEL
00330      IF WS-SEARCH-FAILED                                          ELUTPSEL
00331          SET TST-LAST-IDX TO SSB-TOP-SEL-LAST.                    ELUTPSEL
00332      PERFORM ACCEPT-ATTRIBUTE.                                    ELUTPSEL
00333      EJECT                                                        ELUTPSEL
00334 ************************************************************      ELUTPSEL
00335 *                                                          *      ELUTPSEL
00336 *        PROCESS PROVIDER CLASS                            *      ELUTPSEL
00337 *                                                          *      ELUTPSEL
00338 ************************************************************      ELUTPSEL
00339  PROCESS-PROVIDER-CLASS.                                          ELUTPSEL
00340      IF SSB-RESELECTION (SSB-SELECTOR-STATE)                      ELUTPSEL
00341          PERFORM RESELECT-PROVIDER-CLASS                          ELUTPSEL
00342      ELSE                                                         ELUTPSEL
00343          IF TST-PROVIDER-CLASS (TST-FIRST-IDX) = WS-GENERIC-IND   ELUTPSEL
00344              PERFORM ACCEPT-ATTRIBUTE                             ELUTPSEL
00345          ELSE                                                     ELUTPSEL
00346              PERFORM ELIMINATE-OTHER-PROVIDER-CLASS.              ELUTPSEL
00347                                                                   ELUTPSEL
00348                                                                   ELUTPSEL
00349 ************************************************************      ELUTPSEL
00350 *                                                          *      ELUTPSEL
00351 *        RESELECT PROVIDER CLASS                           *      ELUTPSEL
00352 *                                                          *      ELUTPSEL
00353 ************************************************************      ELUTPSEL
00354  RESELECT-PROVIDER-CLASS.                                         ELUTPSEL
00355      IF TST-PROVIDER-CLASS (TST-FIRST-IDX + 1) = WS-GENERIC-IND   ELUTPSEL
00356          PERFORM RESELECT-ATTRIBUTE                               ELUTPSEL
00357      ELSE                                                         ELUTPSEL
00358          IF TST-PROVIDER-CLASS (TST-FIRST-IDX) = WS-TOPIC-IND     ELUTPSEL
00359              PERFORM ELIMINATE-OTHER-PROVIDER-CLASS               ELUTPSEL
00360          ELSE                                                     ELUTPSEL
00361              PERFORM FORCE-PROVIDER-CLASS-VALUE.                  ELUTPSEL
00362      EJECT                                                        ELUTPSEL
00363 ************************************************************      ELUTPSEL
00364 *                                                          *      ELUTPSEL
00365 *        FORCE PROVIDER CLASS VALUE                        *      ELUTPSEL
00366 *                                                          *      ELUTPSEL
00367 ************************************************************      ELUTPSEL
00368  FORCE-PROVIDER-CLASS-VALUE.                                      ELUTPSEL
00369      SET WS-DERIVED-INPUT TO TRUE.                                ELUTPSEL
00370      PERFORM ELIMINATE-OTHER-PROVIDER-CLASS.                      ELUTPSEL
00371                                                                   ELUTPSEL
00372                                                                   ELUTPSEL
00373 ************************************************************      ELUTPSEL
00374 *                                                          *      ELUTPSEL
00375 *        ELIMINATE OTHER PROVIDER CLASSES                  *      ELUTPSEL
00376 *                                                          *      ELUTPSEL
00377 ************************************************************      ELUTPSEL
00378  ELIMINATE-OTHER-PROVIDER-CLASS.                                  ELUTPSEL
00379      PERFORM ELIMINATE-LOW-PROVIDER-CLASSES.                      ELUTPSEL
00380      IF WS-SEARCH-OK                                              ELUTPSEL
00381          PERFORM ELIMINATE-HIGH-PROVIDER-CLASSE.                  ELUTPSEL
00382                                                                   ELUTPSEL
00383                                                                   ELUTPSEL
00384 ************************************************************      ELUTPSEL
00385 *                                                          *      ELUTPSEL
00386 *        ELIMINATE LOW PROVIDER CLASSES                    *      ELUTPSEL
00387 *                                                          *      ELUTPSEL
00388 ************************************************************      ELUTPSEL
00389  ELIMINATE-LOW-PROVIDER-CLASSES.                                  ELUTPSEL
00390      SET WS-SEARCH-FAILED TO TRUE.                                ELUTPSEL
00391      PERFORM VARYING WS-FIRST-IDX FROM SSB-TOP-SEL-FIRST BY 1     ELUTPSEL
00392                UNTIL WS-FIRST-IDX > SSB-TOP-SEL-LAST              ELUTPSEL
00393                   OR WS-SEARCH-OK                                 ELUTPSEL
00394          IF TST-PROVIDER-CLASS (WS-FIRST-IDX) =                   ELUTPSEL
00395                         SSB-PROVIDER-CLASS                        ELUTPSEL
00396              SET TST-FIRST-IDX TO WS-FIRST-IDX                    ELUTPSEL
00397              SET WS-SEARCH-OK TO TRUE                             ELUTPSEL
00398          END-IF                                                   ELUTPSEL
00399      END-PERFORM.                                                 ELUTPSEL
00400      IF WS-SEARCH-FAILED                                          ELUTPSEL
00401          IF SSB-RESELECTION (SSB-SELECTOR-STATE)                  ELUTPSEL
00402              PERFORM DETERMINE-STATUS-OF-PROVIDER-C               ELUTPSEL
00403          ELSE                                                     ELUTPSEL
00404              PERFORM INTERNAL-TABLE-ERROR.                        ELUTPSEL
00405                                                                   ELUTPSEL
00406                                                                   ELUTPSEL
00407 ************************************************************      ELUTPSEL
00408 *                                                          *      ELUTPSEL
00409 *        DETERMINE STATUS OF PROVIDER CLASS                *      ELUTPSEL
00410 *                                                          *      ELUTPSEL
00411 ************************************************************      ELUTPSEL
00412  DETERMINE-STATUS-OF-PROVIDER-C.                                  ELUTPSEL
00413      IF TST-PROVIDER-CLASS (SSB-TOP-SEL-FIRST) = SPACES           ELUTPSEL
00414          PERFORM FLAG-INPUT-AS-UNAVAILABLE                        ELUTPSEL
00415      ELSE                                                         ELUTPSEL
00416          PERFORM PROMPT-USER-FOR-VALID-INPUT.                     ELUTPSEL
00417                                                                   ELUTPSEL
00418                                                                   ELUTPSEL
00419 ************************************************************      ELUTPSEL
00420 *                                                          *      ELUTPSEL
00421 *        ELIMINATE HIGH PROVIDER CLASSES                   *      ELUTPSEL
00422 *                                                          *      ELUTPSEL
00423 ************************************************************      ELUTPSEL
00424  ELIMINATE-HIGH-PROVIDER-CLASSE.                                  ELUTPSEL
00425      SET WS-SEARCH-FAILED TO TRUE.                                ELUTPSEL
00426      PERFORM VARYING WS-LAST-IDX FROM WS-FIRST-IDX BY 1           ELUTPSEL
00427                UNTIL WS-LAST-IDX > SSB-TOP-SEL-LAST               ELUTPSEL
00428                   OR WS-SEARCH-OK                                 ELUTPSEL
00429          IF TST-PROVIDER-CLASS (WS-LAST-IDX) NOT =                ELUTPSEL
00430                 SSB-PROVIDER-CLASS                                ELUTPSEL
00431              SET WS-SEARCH-OK TO TRUE                             ELUTPSEL
00432              SET TST-LAST-IDX TO WS-LAST-IDX                      ELUTPSEL
00433              SET TST-LAST-IDX DOWN BY 1                           ELUTPSEL
00434          END-IF                                                   ELUTPSEL
00435      END-PERFORM.                                                 ELUTPSEL
00436      IF WS-SEARCH-FAILED                                          ELUTPSEL
00437          SET TST-LAST-IDX TO SSB-TOP-SEL-LAST.                    ELUTPSEL
00438      PERFORM ACCEPT-ATTRIBUTE.                                    ELUTPSEL
00439      EJECT                                                        ELUTPSEL
00440 ************************************************************      ELUTPSEL
00441 *                                                          *      ELUTPSEL
00442 *        PROCESS SERVICE LOCATION                          *      ELUTPSEL
00443 *                                                          *      ELUTPSEL
00444 ************************************************************      ELUTPSEL
00445  PROCESS-SERVICE-LOCATION.                                        ELUTPSEL
00446      IF SSB-RESELECTION (SSB-SELECTOR-STATE)                      ELUTPSEL
00447          PERFORM RESELECT-SERVICE-LOCATION                        ELUTPSEL
00448      ELSE                                                         ELUTPSEL
00449          IF TST-SERVICE-CLASS (TST-FIRST-IDX) = WS-GENERIC-IND    ELUTPSEL
00450              PERFORM ACCEPT-ATTRIBUTE                             ELUTPSEL
00451          ELSE                                                     ELUTPSEL
00452              PERFORM ELIMINATE-OTHER-SERVICE-LOCATI.              ELUTPSEL
00453                                                                   ELUTPSEL
00454                                                                   ELUTPSEL
00455 ************************************************************      ELUTPSEL
00456 *                                                          *      ELUTPSEL
00457 *        RESELECT SERVICE LOCATION                         *      ELUTPSEL
00458 *                                                          *      ELUTPSEL
00459 ************************************************************      ELUTPSEL
00460  RESELECT-SERVICE-LOCATION.                                       ELUTPSEL
00461      IF TST-SERVICE-CLASS (TST-FIRST-IDX + 1) = WS-GENERIC-IND    ELUTPSEL
00462          PERFORM RESELECT-ATTRIBUTE                               ELUTPSEL
00463      ELSE                                                         ELUTPSEL
00464          IF TST-SERVICE-CLASS (TST-FIRST-IDX) = WS-TOPIC-IND      ELUTPSEL
00465              PERFORM ELIMINATE-OTHER-SERVICE-LOCATI               ELUTPSEL
00466          ELSE                                                     ELUTPSEL
00467              PERFORM FORCE-SERVICE-LOCATION-VALUE.                ELUTPSEL
00468                                                                   ELUTPSEL
00469                                                                   ELUTPSEL
00470 ************************************************************      ELUTPSEL
00471 *                                                          *      ELUTPSEL
00472 *        FORCE SERVICE LOCATION VALUE                      *      ELUTPSEL
00473 *                                                          *      ELUTPSEL
00474 ************************************************************      ELUTPSEL
00475  FORCE-SERVICE-LOCATION-VALUE.                                    ELUTPSEL
00476      SET WS-DERIVED-INPUT TO TRUE.                                ELUTPSEL
00477      PERFORM ELIMINATE-OTHER-SERVICE-LOCATI.                      ELUTPSEL
00478                                                                   ELUTPSEL
00479                                                                   ELUTPSEL
00480 ************************************************************      ELUTPSEL
00481 *                                                          *      ELUTPSEL
00482 *        ELIMINATE OTHER SERVICE LOCATIONS                 *      ELUTPSEL
00483 *                                                          *      ELUTPSEL
00484 ************************************************************      ELUTPSEL
00485  ELIMINATE-OTHER-SERVICE-LOCATI.                                  ELUTPSEL
00486      PERFORM ELIMINATE-LOW-SERVICE-LOCATION.                      ELUTPSEL
00487      IF WS-SEARCH-OK                                              ELUTPSEL
00488          PERFORM ELIMINATE-HIGH-SERVICE-LOCATIO.                  ELUTPSEL
00489                                                                   ELUTPSEL
00490                                                                   ELUTPSEL
00491 ************************************************************      ELUTPSEL
00492 *                                                          *      ELUTPSEL
00493 *        ELIMINATE LOW SERVICE LOCATIONS                   *      ELUTPSEL
00494 *                                                          *      ELUTPSEL
00495 ************************************************************      ELUTPSEL
00496  ELIMINATE-LOW-SERVICE-LOCATION.                                  ELUTPSEL
00497      SET WS-SEARCH-FAILED TO TRUE.                                ELUTPSEL
00498      PERFORM VARYING WS-FIRST-IDX FROM SSB-TOP-SEL-FIRST BY 1     ELUTPSEL
00499                UNTIL WS-FIRST-IDX > SSB-TOP-SEL-LAST              ELUTPSEL
00500                   OR WS-SEARCH-OK                                 ELUTPSEL
00501          IF TST-SERVICE-CLASS (WS-FIRST-IDX) =                    ELUTPSEL
00502                   SSB-SERVICE-CLASS                               ELUTPSEL
00503              SET WS-SEARCH-OK TO TRUE                             ELUTPSEL
00504              SET TST-FIRST-IDX TO WS-FIRST-IDX                    ELUTPSEL
00505          END-IF                                                   ELUTPSEL
00506      END-PERFORM.                                                 ELUTPSEL
00507      IF WS-SEARCH-FAILED                                          ELUTPSEL
00508          IF SSB-RESELECTION (SSB-SELECTOR-STATE)                  ELUTPSEL
00509              PERFORM DETERMINE-STATUS-OF-SERVICE-LO               ELUTPSEL
00510          ELSE                                                     ELUTPSEL
00511              PERFORM INTERNAL-TABLE-ERROR.                        ELUTPSEL
00512                                                                   ELUTPSEL
00513                                                                   ELUTPSEL
00514 ************************************************************      ELUTPSEL
00515 *                                                          *      ELUTPSEL
00516 *        DETERMINE STATUS OF SERVICE LOCATION              *      ELUTPSEL
00517 *                                                          *      ELUTPSEL
00518 ************************************************************      ELUTPSEL
00519  DETERMINE-STATUS-OF-SERVICE-LO.                                  ELUTPSEL
00520      IF TST-SERVICE-CLASS (SSB-TOP-SEL-FIRST) = SPACES            ELUTPSEL
00521          PERFORM FLAG-INPUT-AS-UNAVAILABLE                        ELUTPSEL
00522      ELSE                                                         ELUTPSEL
00523          PERFORM PROMPT-USER-FOR-VALID-INPUT.                     ELUTPSEL
00524                                                                   ELUTPSEL
00525                                                                   ELUTPSEL
00526 ************************************************************      ELUTPSEL
00527 *                                                          *      ELUTPSEL
00528 *        ELIMINATE HIGH SERVICE LOCATIONS                  *      ELUTPSEL
00529 *                                                          *      ELUTPSEL
00530 ************************************************************      ELUTPSEL
00531  ELIMINATE-HIGH-SERVICE-LOCATIO.                                  ELUTPSEL
00532      SET WS-SEARCH-FAILED TO TRUE.                                ELUTPSEL
00533      PERFORM VARYING WS-LAST-IDX FROM WS-FIRST-IDX BY 1           ELUTPSEL
00534                UNTIL WS-LAST-IDX > SSB-TOP-SEL-LAST               ELUTPSEL
00535                   OR WS-SEARCH-OK                                 ELUTPSEL
00536          IF TST-SERVICE-CLASS (WS-LAST-IDX) NOT =                 ELUTPSEL
00537                 SSB-SERVICE-CLASS                                 ELUTPSEL
00538              SET WS-SEARCH-OK TO TRUE                             ELUTPSEL
00539              SET TST-LAST-IDX TO WS-LAST-IDX                      ELUTPSEL
00540              SET TST-LAST-IDX DOWN BY 1                           ELUTPSEL
00541          END-IF                                                   ELUTPSEL
00542      END-PERFORM.                                                 ELUTPSEL
00543      IF WS-SEARCH-FAILED                                          ELUTPSEL
00544          SET TST-LAST-IDX TO SSB-TOP-SEL-LAST.                    ELUTPSEL
00545      PERFORM ACCEPT-ATTRIBUTE.                                    ELUTPSEL
00546      EJECT                                                        ELUTPSEL
00547 ************************************************************      ELUTPSEL
00548 *                                                          *      ELUTPSEL
00549 *        PROCESS MODIFIER 1                                *      ELUTPSEL
00550 *                                                          *      ELUTPSEL
00551 ************************************************************      ELUTPSEL
00552  PROCESS-MODIFIER-1.                                              ELUTPSEL
00553      IF SSB-RESELECTION (SSB-SELECTOR-STATE)                      ELUTPSEL
00554          PERFORM RESELECT-MODIFIER-1                              ELUTPSEL
00555      ELSE                                                         ELUTPSEL
00556          IF TST-MODIFIER-1 (TST-FIRST-IDX) = WS-GENERIC-IND       ELUTPSEL
00557              PERFORM ACCEPT-ATTRIBUTE                             ELUTPSEL
00558          ELSE                                                     ELUTPSEL
00559              PERFORM ELIMINATE-OTHER-MODIFIER-1.                  ELUTPSEL
00560                                                                   ELUTPSEL
00561                                                                   ELUTPSEL
00562 ************************************************************      ELUTPSEL
00563 *                                                          *      ELUTPSEL
00564 *        RESELECT MODIFIER 1                               *      ELUTPSEL
00565 *                                                          *      ELUTPSEL
00566 ************************************************************      ELUTPSEL
00567  RESELECT-MODIFIER-1.                                             ELUTPSEL
00568      IF TST-MODIFIER-1 (TST-FIRST-IDX + 1) = WS-GENERIC-IND       ELUTPSEL
00569          PERFORM RESELECT-ATTRIBUTE                               ELUTPSEL
00570      ELSE                                                         ELUTPSEL
00571          IF TST-MODIFIER-1 (TST-FIRST-IDX) = WS-TOPIC-IND         ELUTPSEL
00572              PERFORM ELIMINATE-OTHER-MODIFIER-1                   ELUTPSEL
00573          ELSE                                                     ELUTPSEL
00574              PERFORM FORCE-MODIFIER-1-VALUE.                      ELUTPSEL
00575                                                                   ELUTPSEL
00576                                                                   ELUTPSEL
00577 ************************************************************      ELUTPSEL
00578 *                                                          *      ELUTPSEL
00579 *        FORCE MODIFIER 1 VALUE                            *      ELUTPSEL
00580 *                                                          *      ELUTPSEL
00581 ************************************************************      ELUTPSEL
00582  FORCE-MODIFIER-1-VALUE.                                          ELUTPSEL
00583      SET WS-DERIVED-INPUT TO TRUE.                                ELUTPSEL
00584      PERFORM ELIMINATE-OTHER-MODIFIER-1.                          ELUTPSEL
00585                                                                   ELUTPSEL
00586                                                                   ELUTPSEL
00587 ************************************************************      ELUTPSEL
00588 *                                                          *      ELUTPSEL
00589 *        ELIMINATE OTHER MODIFIER 1                        *      ELUTPSEL
00590 *                                                          *      ELUTPSEL
00591 ************************************************************      ELUTPSEL
00592  ELIMINATE-OTHER-MODIFIER-1.                                      ELUTPSEL
00593      PERFORM ELIMINATE-LOW-MODIFIER-1.                            ELUTPSEL
00594      IF WS-SEARCH-OK                                              ELUTPSEL
00595          PERFORM ELIMINATE-HIGH-MODIFIER-1.                       ELUTPSEL
00596                                                                   ELUTPSEL
00597                                                                   ELUTPSEL
00598 ************************************************************      ELUTPSEL
00599 *                                                          *      ELUTPSEL
00600 *        ELIMINATE LOW MODIFIER-1                          *      ELUTPSEL
00601 *                                                          *      ELUTPSEL
00602 ************************************************************      ELUTPSEL
00603  ELIMINATE-LOW-MODIFIER-1.                                        ELUTPSEL
00604      SET WS-SEARCH-FAILED TO TRUE.                                ELUTPSEL
00605      PERFORM VARYING WS-FIRST-IDX FROM SSB-TOP-SEL-FIRST BY 1     ELUTPSEL
00606                UNTIL WS-FIRST-IDX > SSB-TOP-SEL-LAST              ELUTPSEL
00607                   OR WS-SEARCH-OK                                 ELUTPSEL
00608          IF TST-MODIFIER-1 (WS-FIRST-IDX) =                       ELUTPSEL
00609                   SSB-MODIFIER-1                                  ELUTPSEL
00610              SET WS-SEARCH-OK TO TRUE                             ELUTPSEL
00611              SET TST-FIRST-IDX TO WS-FIRST-IDX                    ELUTPSEL
00612          END-IF                                                   ELUTPSEL
00613      END-PERFORM.                                                 ELUTPSEL
00614      IF WS-SEARCH-FAILED                                          ELUTPSEL
00615          IF SSB-RESELECTION (SSB-SELECTOR-STATE)                  ELUTPSEL
00616              PERFORM DETERMINE-STATUS-OF-MODIFIER-1               ELUTPSEL
00617          ELSE                                                     ELUTPSEL
00618              PERFORM INTERNAL-TABLE-ERROR.                        ELUTPSEL
00619                                                                   ELUTPSEL
00620                                                                   ELUTPSEL
00621 ************************************************************      ELUTPSEL
00622 *                                                          *      ELUTPSEL
00623 *        DETERMINE STATUS OF MODIFIER-1                    *      ELUTPSEL
00624 *                                                          *      ELUTPSEL
00625 ************************************************************      ELUTPSEL
00626  DETERMINE-STATUS-OF-MODIFIER-1.                                  ELUTPSEL
00627      IF TST-MODIFIER-1 (SSB-TOP-SEL-FIRST) = SPACES               ELUTPSEL
00628          PERFORM FLAG-INPUT-AS-UNAVAILABLE                        ELUTPSEL
00629      ELSE                                                         ELUTPSEL
00630          PERFORM PROMPT-USER-FOR-VALID-INPUT.                     ELUTPSEL
00631                                                                   ELUTPSEL
00632                                                                   ELUTPSEL
00633 ************************************************************      ELUTPSEL
00634 *                                                          *      ELUTPSEL
00635 *        ELIMINATE HIGH MODIFIER-1                         *      ELUTPSEL
00636 *                                                          *      ELUTPSEL
00637 ************************************************************      ELUTPSEL
00638  ELIMINATE-HIGH-MODIFIER-1.                                       ELUTPSEL
00639      SET WS-SEARCH-FAILED TO TRUE.                                ELUTPSEL
00640      PERFORM VARYING WS-LAST-IDX FROM WS-FIRST-IDX BY 1           ELUTPSEL
00641                UNTIL WS-LAST-IDX > SSB-TOP-SEL-LAST               ELUTPSEL
00642                   OR WS-SEARCH-OK                                 ELUTPSEL
00643          IF TST-MODIFIER-1 (WS-LAST-IDX) NOT =                    ELUTPSEL
00644                  SSB-MODIFIER-1                                   ELUTPSEL
00645              SET TST-LAST-IDX TO WS-LAST-IDX                      ELUTPSEL
00646              SET TST-LAST-IDX DOWN BY 1                           ELUTPSEL
00647              SET WS-SEARCH-OK TO TRUE                             ELUTPSEL
00648          END-IF                                                   ELUTPSEL
00649      END-PERFORM.                                                 ELUTPSEL
00650      IF WS-SEARCH-FAILED                                          ELUTPSEL
00651          SET TST-LAST-IDX TO SSB-TOP-SEL-LAST.                    ELUTPSEL
00652      PERFORM ACCEPT-ATTRIBUTE.                                    ELUTPSEL
00653      EJECT                                                        ELUTPSEL
00654 ************************************************************      ELUTPSEL
00655 *                                                          *      ELUTPSEL
00656 *        PROCESS MODIFIER 2                                *      ELUTPSEL
00657 *                                                          *      ELUTPSEL
00658 ************************************************************      ELUTPSEL
00659  PROCESS-MODIFIER-2.                                              ELUTPSEL
00660      IF SSB-RESELECTION (SSB-SELECTOR-STATE)                      ELUTPSEL
00661          PERFORM RESELECT-MODIFIER-2                              ELUTPSEL
00662      ELSE                                                         ELUTPSEL
00663          IF TST-MODIFIER-2 (TST-FIRST-IDX) = WS-GENERIC-IND       ELUTPSEL
00664              PERFORM ACCEPT-ATTRIBUTE                             ELUTPSEL
00665          ELSE                                                     ELUTPSEL
00666              PERFORM ELIMINATE-OTHER-MODIFIER-2.                  ELUTPSEL
00667      IF SSB-TOP-SEL-FIRST = SSB-TOP-SEL-LAST                      ELUTPSEL
00668          PERFORM TOPIC-PROGRAM-FOUND                              ELUTPSEL
00669      ELSE                                                         ELUTPSEL
00670          PERFORM INTERNAL-TABLE-ERROR.                            ELUTPSEL
00671                                                                   ELUTPSEL
00672                                                                   ELUTPSEL
00673 ************************************************************      ELUTPSEL
00674 *                                                          *      ELUTPSEL
00675 *        RESELECT MODIFIER 2                               *      ELUTPSEL
00676 *                                                          *      ELUTPSEL
00677 ************************************************************      ELUTPSEL
00678  RESELECT-MODIFIER-2.                                             ELUTPSEL
00679      IF TST-MODIFIER-2 (TST-FIRST-IDX + 1) = WS-GENERIC-IND       ELUTPSEL
00680          PERFORM RESELECT-ATTRIBUTE                               ELUTPSEL
00681      ELSE                                                         ELUTPSEL
00682          IF TST-MODIFIER-2 (TST-FIRST-IDX) = WS-TOPIC-IND         ELUTPSEL
00683              PERFORM ELIMINATE-OTHER-MODIFIER-2                   ELUTPSEL
00684          ELSE                                                     ELUTPSEL
00685              PERFORM FORCE-MODIFIER-2-VALUE.                      ELUTPSEL
00686                                                                   ELUTPSEL
00687                                                                   ELUTPSEL
00688 ************************************************************      ELUTPSEL
00689 *                                                          *      ELUTPSEL
00690 *        FORCE MODIFIER 2 VALUE                            *      ELUTPSEL
00691 *                                                          *      ELUTPSEL
00692 ************************************************************      ELUTPSEL
00693  FORCE-MODIFIER-2-VALUE.                                          ELUTPSEL
00694      SET WS-DERIVED-INPUT TO TRUE.                                ELUTPSEL
00695      PERFORM ELIMINATE-OTHER-MODIFIER-2.                          ELUTPSEL
00696                                                                   ELUTPSEL
00697                                                                   ELUTPSEL
00698 ************************************************************      ELUTPSEL
00699 *                                                          *      ELUTPSEL
00700 *        ELIMINATE OTHER MODIFIER 2                        *      ELUTPSEL
00701 *                                                          *      ELUTPSEL
00702 ************************************************************      ELUTPSEL
00703  ELIMINATE-OTHER-MODIFIER-2.                                      ELUTPSEL
00704      PERFORM ELIMINATE-LOW-MODIFIER-2.                            ELUTPSEL
00705      IF WS-SEARCH-OK                                              ELUTPSEL
00706          PERFORM ELIMINATE-HIGH-MODIFIER-2.                       ELUTPSEL
00707                                                                   ELUTPSEL
00708                                                                   ELUTPSEL
00709 ************************************************************      ELUTPSEL
00710 *                                                          *      ELUTPSEL
00711 *        ELIMINATE LOW MODIFIER 2                          *      ELUTPSEL
00712 *                                                          *      ELUTPSEL
00713 ************************************************************      ELUTPSEL
00714  ELIMINATE-LOW-MODIFIER-2.                                        ELUTPSEL
00715      SET WS-SEARCH-FAILED TO TRUE.                                ELUTPSEL
00716      PERFORM VARYING WS-FIRST-IDX FROM SSB-TOP-SEL-FIRST BY 1     ELUTPSEL
00717                UNTIL WS-FIRST-IDX > SSB-TOP-SEL-LAST              ELUTPSEL
00718                   OR WS-SEARCH-OK                                 ELUTPSEL
00719          IF TST-MODIFIER-2 (WS-FIRST-IDX) =                       ELUTPSEL
00720                   SSB-MODIFIER-2                                  ELUTPSEL
00721              SET WS-SEARCH-OK TO TRUE                             ELUTPSEL
00722              SET TST-FIRST-IDX TO WS-FIRST-IDX                    ELUTPSEL
00723          END-IF                                                   ELUTPSEL
00724      END-PERFORM.                                                 ELUTPSEL
00725      IF WS-SEARCH-FAILED                                          ELUTPSEL
00726          IF SSB-RESELECTION (SSB-SELECTOR-STATE)                  ELUTPSEL
00727              PERFORM DETERMINE-STATUS-OF-MODIFIER-2               ELUTPSEL
00728          ELSE                                                     ELUTPSEL
00729              PERFORM INTERNAL-TABLE-ERROR.                        ELUTPSEL
00730                                                                   ELUTPSEL
00731                                                                   ELUTPSEL
00732 ************************************************************      ELUTPSEL
00733 *                                                          *      ELUTPSEL
00734 *        DETERMINE STATUS OF MODIFIER-2                    *      ELUTPSEL
00735 *                                                          *      ELUTPSEL
00736 ************************************************************      ELUTPSEL
00737  DETERMINE-STATUS-OF-MODIFIER-2.                                  ELUTPSEL
00738      IF TST-MODIFIER-2 (SSB-TOP-SEL-FIRST) = SPACES               ELUTPSEL
00739          PERFORM FLAG-INPUT-AS-UNAVAILABLE                        ELUTPSEL
00740      ELSE                                                         ELUTPSEL
00741          PERFORM PROMPT-USER-FOR-VALID-INPUT.                     ELUTPSEL
00742                                                                   ELUTPSEL
00743                                                                   ELUTPSEL
00744 ************************************************************      ELUTPSEL
00745 *                                                          *      ELUTPSEL
00746 *        ELIMINATE HIGH MODIFIER 2                         *      ELUTPSEL
00747 *                                                          *      ELUTPSEL
00748 ************************************************************      ELUTPSEL
00749  ELIMINATE-HIGH-MODIFIER-2.                                       ELUTPSEL
00750      SET WS-SEARCH-FAILED TO TRUE.                                ELUTPSEL
00751      PERFORM VARYING WS-LAST-IDX FROM WS-FIRST-IDX BY 1           ELUTPSEL
00752                UNTIL WS-LAST-IDX > SSB-TOP-SEL-LAST               ELUTPSEL
00753                   OR WS-SEARCH-OK                                 ELUTPSEL
00754          IF TST-MODIFIER-2 (WS-LAST-IDX) NOT =                    ELUTPSEL
00755                     SSB-MODIFIER-2                                ELUTPSEL
00756              SET WS-SEARCH-OK TO TRUE                             ELUTPSEL
00757              SET TST-LAST-IDX TO WS-LAST-IDX                      ELUTPSEL
00758              SET TST-LAST-IDX DOWN BY 1                           ELUTPSEL
00759          END-IF                                                   ELUTPSEL
00760      END-PERFORM.                                                 ELUTPSEL
00761      IF WS-SEARCH-FAILED                                          ELUTPSEL
00762          SET TST-LAST-IDX TO SSB-TOP-SEL-LAST.                    ELUTPSEL
00763      PERFORM ACCEPT-ATTRIBUTE.                                    ELUTPSEL
00764      EJECT                                                        ELUTPSEL
00765 ************************************************************      ELUTPSEL
00766 *                                                          *      ELUTPSEL
00767 *        RESELECT ATTRIBUTE                                *      ELUTPSEL
00768 *                                                          *      ELUTPSEL
00769 ************************************************************      ELUTPSEL
00770  RESELECT-ATTRIBUTE.                                              ELUTPSEL
00771      SET TST-FIRST-IDX UP BY 1.                                   ELUTPSEL
00772      PERFORM ACCEPT-ATTRIBUTE.                                    ELUTPSEL
00773                                                                   ELUTPSEL
00774                                                                   ELUTPSEL
00775 ************************************************************      ELUTPSEL
00776 *                                                          *      ELUTPSEL
00777 *        ACCEPT ATTRIBUTE                                  *      ELUTPSEL
00778 *                                                          *      ELUTPSEL
00779 ************************************************************      ELUTPSEL
00780  ACCEPT-ATTRIBUTE.                                                ELUTPSEL
00781      SET SSB-TOP-SEL-FIRST TO TST-FIRST-IDX.                      ELUTPSEL
00782      SET SSB-TOP-SEL-LAST  TO TST-LAST-IDX.                       ELUTPSEL
00783      SET WS-ATTR-FOUND     TO TRUE.                               ELUTPSEL
00784      IF WS-DERIVED-INPUT                                          ELUTPSEL
00785          SET SSB-DATA-DERIVED (SSB-SELECTOR-STATE)                ELUTPSEL
00786              TO TRUE                                              ELUTPSEL
00787      ELSE                                                         ELUTPSEL
00788          SET SSB-SEL-DATA-AVAIL (SSB-SELECTOR-STATE)              ELUTPSEL
00789              TO TRUE.                                             ELUTPSEL
00790                                                                   ELUTPSEL
00791                                                                   ELUTPSEL
00792 ************************************************************      ELUTPSEL
00793 *                                                          *      ELUTPSEL
00794 *        FLAG INPUT AS UNAVAILABLE                         *      ELUTPSEL
00795 *                                                          *      ELUTPSEL
00796 ************************************************************      ELUTPSEL
00797  FLAG-INPUT-AS-UNAVAILABLE.                                       ELUTPSEL
00798      SET SSB-DATA-KNOWN-NOT-AVAIL (SSB-SELECTOR-STATE)            ELUTPSEL
00799          TO TRUE.                                                 ELUTPSEL
00800      SET WS-ATTR-FOUND     TO TRUE.                               ELUTPSEL
00801      SET WS-DATA-UNAVAIL   TO TRUE.                               ELUTPSEL
00802                                                                   ELUTPSEL
00803                                                                   ELUTPSEL
00804 ************************************************************      ELUTPSEL
00805 *                                                          *      ELUTPSEL
00806 *        PROMPT USER FOR VALID INPUT                       *      ELUTPSEL
00807 *                                                          *      ELUTPSEL
00808 ************************************************************      ELUTPSEL
00809  PROMPT-USER-FOR-VALID-INPUT.                                     ELUTPSEL
00810      SET SSB-INITIAL-CALL (SSB-SELECTOR-STATE) TO TRUE.           ELUTPSEL
00811      EJECT                                                        ELUTPSEL
00812 ************************************************************      ELUTPSEL
00813 *                                                          *      ELUTPSEL
00814 *        CHECK CURRENT TOPIC                               *      ELUTPSEL
00815 *                                                          *      ELUTPSEL
00816 ************************************************************      ELUTPSEL
00817  CHECK-CURRENT-TOPIC.                                             ELUTPSEL
00818      SET TST-FIRST-IDX TO SSB-TOP-SEL-FIRST.                      ELUTPSEL
00819      SET TST-LAST-IDX  TO SSB-TOP-SEL-LAST.                       ELUTPSEL
00820      EVALUATE TRUE                                                ELUTPSEL
00821      WHEN SSB-SS-GET-TOPIC                                        ELUTPSEL
00822          PERFORM SELECT-MAIN-TOPIC                                ELUTPSEL
00823      WHEN SSB-SS-GET-SUBTOPIC                                     ELUTPSEL
00824          PERFORM SELECT-SUB-TOPIC                                 ELUTPSEL
00825      WHEN SSB-SS-GET-PROVIDER-CLASS                               ELUTPSEL
00826          PERFORM SELECT-PROVIDER-CLASS                            ELUTPSEL
00827      WHEN SSB-SS-GET-SERVICE-LOC                                  ELUTPSEL
00828          PERFORM SELECT-SERVICE-LOCATION                          ELUTPSEL
00829      WHEN SSB-SS-GET-MODIFIER-1                                   ELUTPSEL
00830          PERFORM SELECT-MODIFIER-1                                ELUTPSEL
00831      WHEN SSB-SS-GET-MODIFIER-2                                   ELUTPSEL
00832          PERFORM SELECT-MODIFIER-2                                ELUTPSEL
00833      WHEN OTHER                                                   ELUTPSEL
00834          PERFORM SYSTEM-LOGIC-ERROR.                              ELUTPSEL
00835      EJECT                                                        ELUTPSEL
00836 ************************************************************      ELUTPSEL
00837 *                                                          *      ELUTPSEL
00838 *        SELECT MAIN TOPIC                                 *      ELUTPSEL
00839 *                                                          *      ELUTPSEL
00840 ************************************************************      ELUTPSEL
00841  SELECT-MAIN-TOPIC.                                               ELUTPSEL
00842      IF TST-TOPIC (1) NOT = WS-TOPIC-IND                          ELUTPSEL
00843          PERFORM INTERNAL-TABLE-ERROR.                            ELUTPSEL
00844      MOVE TST-SEL-PGM-NAME (1) TO SSB-ACTION-MODULE.              ELUTPSEL
00845      SET SSB-INITIAL-CALL (SSB-SELECTOR-STATE) TO TRUE.           ELUTPSEL
00846                                                                   ELUTPSEL
00847                                                                   ELUTPSEL
00848 ************************************************************      ELUTPSEL
00849 *                                                          *      ELUTPSEL
00850 *        SELECT SUB TOPIC                                  *      ELUTPSEL
00851 *                                                          *      ELUTPSEL
00852 ************************************************************      ELUTPSEL
00853  SELECT-SUB-TOPIC.                                                ELUTPSEL
00854      IF SSB-TOP-SEL-FIRST = SSB-TOP-SEL-LAST                      ELUTPSEL
00855          PERFORM PRE-SELECT-SUB-TOPIC                             ELUTPSEL
00856      ELSE                                                         ELUTPSEL
00857          PERFORM DETERMINE-IF-SUB-TOPICS-EXISTS.                  ELUTPSEL
00858                                                                   ELUTPSEL
00859                                                                   ELUTPSEL
00860 ************************************************************      ELUTPSEL
00861 *                                                          *      ELUTPSEL
00862 *        DETERMINE IF SUB TOPICS EXISTS                    *      ELUTPSEL
00863 *                                                          *      ELUTPSEL
00864 ************************************************************      ELUTPSEL
00865  DETERMINE-IF-SUB-TOPICS-EXISTS.                                  ELUTPSEL
00866      IF TST-SUB-TOPIC (TST-FIRST-IDX) = WS-TOPIC-IND              ELUTPSEL
00867          PERFORM CHOOSE-TOPIC-SELECTOR                            ELUTPSEL
00868      ELSE IF TST-SUB-TOPIC (TST-FIRST-IDX) NOT =                  ELUTPSEL
00869          WS-GENERIC-IND                                           ELUTPSEL
00870          PERFORM PRE-SELECT-SUB-TOPIC                             ELUTPSEL
00871      ELSE                                                         ELUTPSEL
00872          PERFORM INTERNAL-TABLE-ERROR.                            ELUTPSEL
00873                                                                   ELUTPSEL
00874                                                                   ELUTPSEL
00875 ************************************************************      ELUTPSEL
00876 *                                                          *      ELUTPSEL
00877 *        PRE SELECT SUB TOPIC                              *      ELUTPSEL
00878 *                                                          *      ELUTPSEL
00879 ************************************************************      ELUTPSEL
00880  PRE-SELECT-SUB-TOPIC.                                            ELUTPSEL
00881      MOVE TST-SUB-TOPIC (TST-FIRST-IDX) TO                        ELUTPSEL
00882          SSB-SUB-TOPIC.                                           ELUTPSEL
00883      IF SSB-SUB-TOPIC = SPACES                                    ELUTPSEL
00884          PERFORM FLAG-ATTRIBUTE-AS-NOT-USED                       ELUTPSEL
00885      ELSE                                                         ELUTPSEL
00886          PERFORM FLAG-ATTRIBUTE-AS-DERIVED.                       ELUTPSEL
00887      EJECT                                                        ELUTPSEL
00888 ************************************************************      ELUTPSEL
00889 *                                                          *      ELUTPSEL
00890 *        SELECT PROVIDER CLASS                             *      ELUTPSEL
00891 *                                                          *      ELUTPSEL
00892 ************************************************************      ELUTPSEL
00893  SELECT-PROVIDER-CLASS.                                           ELUTPSEL
00894      IF SSB-TOP-SEL-FIRST = SSB-TOP-SEL-LAST                      ELUTPSEL
00895          PERFORM PRE-SELECT-PROVIDER-CLASS                        ELUTPSEL
00896      ELSE                                                         ELUTPSEL
00897          PERFORM DETERMINE-IF-PROVIDER-CLASSESX.                  ELUTPSEL
00898                                                                   ELUTPSEL
00899                                                                   ELUTPSEL
00900 ************************************************************      ELUTPSEL
00901 *                                                          *      ELUTPSEL
00902 *        DETERMINE IF PROVIDER CLASSES EXISTS              *      ELUTPSEL
00903 *                                                          *      ELUTPSEL
00904 ************************************************************      ELUTPSEL
00905  DETERMINE-IF-PROVIDER-CLASSESX.                                  ELUTPSEL
00906      IF TST-PROVIDER-CLASS (TST-FIRST-IDX) =                      ELUTPSEL
00907              WS-TOPIC-IND                                         ELUTPSEL
00908          PERFORM CHOOSE-TOPIC-SELECTOR                            ELUTPSEL
00909      ELSE                                                         ELUTPSEL
00910          IF TST-PROVIDER-CLASS (TST-FIRST-IDX) NOT =              ELUTPSEL
00911                  WS-GENERIC-IND                                   ELUTPSEL
00912              PERFORM PRE-SELECT-PROVIDER-CLASS                    ELUTPSEL
00913          ELSE                                                     ELUTPSEL
00914              PERFORM INTERNAL-TABLE-ERROR.                        ELUTPSEL
00915                                                                   ELUTPSEL
00916                                                                   ELUTPSEL
00917 ************************************************************      ELUTPSEL
00918 *                                                          *      ELUTPSEL
00919 *        PRE SELECT PROVIDER CLASS                         *      ELUTPSEL
00920 *                                                          *      ELUTPSEL
00921 ************************************************************      ELUTPSEL
00922  PRE-SELECT-PROVIDER-CLASS.                                       ELUTPSEL
00923      MOVE TST-PROVIDER-CLASS (TST-FIRST-IDX)                      ELUTPSEL
00924           TO SSB-PROVIDER-CLASS.                                  ELUTPSEL
00925      IF SSB-PROVIDER-CLASS = SPACES                               ELUTPSEL
00926          PERFORM FLAG-ATTRIBUTE-AS-NOT-USED                       ELUTPSEL
00927      ELSE                                                         ELUTPSEL
00928          PERFORM FLAG-ATTRIBUTE-AS-DERIVED.                       ELUTPSEL
00929      EJECT                                                        ELUTPSEL
00930 ************************************************************      ELUTPSEL
00931 *                                                          *      ELUTPSEL
00932 *        SELECT SERVICE LOCATION                           *      ELUTPSEL
00933 *                                                          *      ELUTPSEL
00934 ************************************************************      ELUTPSEL
00935  SELECT-SERVICE-LOCATION.                                         ELUTPSEL
00936      IF SSB-TOP-SEL-FIRST = SSB-TOP-SEL-LAST                      ELUTPSEL
00937          PERFORM PRE-SELECT-SERVICE-LOCATION                      ELUTPSEL
00938      ELSE                                                         ELUTPSEL
00939          PERFORM DETERMINE-IF-SERVICE-LOCATIONS.                  ELUTPSEL
00940                                                                   ELUTPSEL
00941                                                                   ELUTPSEL
00942 ************************************************************      ELUTPSEL
00943 *                                                          *      ELUTPSEL
00944 *        DETERMINE IF SERVICE LOCATIONS EXISTS             *      ELUTPSEL
00945 *                                                          *      ELUTPSEL
00946 ************************************************************      ELUTPSEL
00947  DETERMINE-IF-SERVICE-LOCATIONS.                                  ELUTPSEL
00948      IF TST-SERVICE-CLASS (TST-FIRST-IDX) =                       ELUTPSEL
00949              WS-TOPIC-IND                                         ELUTPSEL
00950          PERFORM CHOOSE-TOPIC-SELECTOR                            ELUTPSEL
00951      ELSE                                                         ELUTPSEL
00952          IF TST-SERVICE-CLASS (TST-FIRST-IDX) NOT =               ELUTPSEL
00953              WS-GENERIC-IND                                       ELUTPSEL
00954                PERFORM PRE-SELECT-SERVICE-LOCATION                ELUTPSEL
00955          ELSE                                                     ELUTPSEL
00956              PERFORM INTERNAL-TABLE-ERROR.                        ELUTPSEL
00957                                                                   ELUTPSEL
00958                                                                   ELUTPSEL
00959 ************************************************************      ELUTPSEL
00960 *                                                          *      ELUTPSEL
00961 *        PRE SELECT SERVICE LOCATION                       *      ELUTPSEL
00962 *                                                          *      ELUTPSEL
00963 ************************************************************      ELUTPSEL
00964  PRE-SELECT-SERVICE-LOCATION.                                     ELUTPSEL
00965      MOVE TST-SERVICE-CLASS (TST-FIRST-IDX)                       ELUTPSEL
00966           TO SSB-SERVICE-CLASS.                                   ELUTPSEL
00967      IF SSB-SERVICE-CLASS = SPACES                                ELUTPSEL
00968          PERFORM FLAG-ATTRIBUTE-AS-NOT-USED                       ELUTPSEL
00969      ELSE                                                         ELUTPSEL
00970          PERFORM FLAG-ATTRIBUTE-AS-DERIVED.                       ELUTPSEL
00971      EJECT                                                        ELUTPSEL
00972 ************************************************************      ELUTPSEL
00973 *                                                          *      ELUTPSEL
00974 *        SELECT MODIFIER 1                                 *      ELUTPSEL
00975 *                                                          *      ELUTPSEL
00976 ************************************************************      ELUTPSEL
00977  SELECT-MODIFIER-1.                                               ELUTPSEL
00978      IF SSB-TOP-SEL-FIRST = SSB-TOP-SEL-LAST                      ELUTPSEL
00979          PERFORM PRE-SELECT-MODIFIER-1                            ELUTPSEL
00980      ELSE                                                         ELUTPSEL
00981          PERFORM DETERMINE-IF-ANY-MODIFIER-1-EX.                  ELUTPSEL
00982                                                                   ELUTPSEL
00983                                                                   ELUTPSEL
00984 ************************************************************      ELUTPSEL
00985 *                                                          *      ELUTPSEL
00986 *        DETERMINE IF ANY MODIFIER 1 EXISTS                *      ELUTPSEL
00987 *                                                          *      ELUTPSEL
00988 ************************************************************      ELUTPSEL
00989  DETERMINE-IF-ANY-MODIFIER-1-EX.                                  ELUTPSEL
00990      IF TST-MODIFIER-1 (TST-FIRST-IDX) = WS-TOPIC-IND             ELUTPSEL
00991          PERFORM CHOOSE-TOPIC-SELECTOR                            ELUTPSEL
00992      ELSE                                                         ELUTPSEL
00993          IF TST-MODIFIER-1 (TST-FIRST-IDX) NOT =                  ELUTPSEL
00994               WS-GENERIC-IND                                      ELUTPSEL
00995              PERFORM PRE-SELECT-MODIFIER-1                        ELUTPSEL
00996          ELSE                                                     ELUTPSEL
00997              PERFORM INTERNAL-TABLE-ERROR.                        ELUTPSEL
00998                                                                   ELUTPSEL
00999                                                                   ELUTPSEL
01000 ************************************************************      ELUTPSEL
01001 *                                                          *      ELUTPSEL
01002 *        PRE SELECT MODIFIER 1                             *      ELUTPSEL
01003 *                                                          *      ELUTPSEL
01004 ************************************************************      ELUTPSEL
01005  PRE-SELECT-MODIFIER-1.                                           ELUTPSEL
01006      MOVE TST-MODIFIER-1 (TST-FIRST-IDX) TO                       ELUTPSEL
01007          SSB-MODIFIER-1.                                          ELUTPSEL
01008      IF SSB-MODIFIER-1 = SPACES                                   ELUTPSEL
01009          PERFORM FLAG-ATTRIBUTE-AS-NOT-USED                       ELUTPSEL
01010      ELSE                                                         ELUTPSEL
01011          PERFORM FLAG-ATTRIBUTE-AS-DERIVED.                       ELUTPSEL
01012      EJECT                                                        ELUTPSEL
01013 ************************************************************      ELUTPSEL
01014 *                                                          *      ELUTPSEL
01015 *        SELECT MODIFIER 2                                 *      ELUTPSEL
01016 *                                                          *      ELUTPSEL
01017 ************************************************************      ELUTPSEL
01018  SELECT-MODIFIER-2.                                               ELUTPSEL
01019      IF SSB-TOP-SEL-FIRST = SSB-TOP-SEL-LAST                      ELUTPSEL
01020          PERFORM PRE-SELECT-MODIFIER-2                            ELUTPSEL
01021      ELSE                                                         ELUTPSEL
01022          PERFORM DETERMINE-IF-ANY-MODIFIER-2-EX.                  ELUTPSEL
01023                                                                   ELUTPSEL
01024                                                                   ELUTPSEL
01025 ************************************************************      ELUTPSEL
01026 *                                                          *      ELUTPSEL
01027 *        DETERMINE IF ANY MODIFIER 2 EXISTS                *      ELUTPSEL
01028 *                                                          *      ELUTPSEL
01029 ************************************************************      ELUTPSEL
01030  DETERMINE-IF-ANY-MODIFIER-2-EX.                                  ELUTPSEL
01031      IF TST-MODIFIER-2 (TST-FIRST-IDX) = WS-TOPIC-IND             ELUTPSEL
01032          PERFORM CHOOSE-TOPIC-SELECTOR                            ELUTPSEL
01033      ELSE                                                         ELUTPSEL
01034          IF TST-MODIFIER-2 (TST-FIRST-IDX) NOT =                  ELUTPSEL
01035               WS-GENERIC-IND                                      ELUTPSEL
01036              PERFORM PRE-SELECT-MODIFIER-2                        ELUTPSEL
01037          ELSE                                                     ELUTPSEL
01038              PERFORM INTERNAL-TABLE-ERROR.                        ELUTPSEL
01039                                                                   ELUTPSEL
01040                                                                   ELUTPSEL
01041 ************************************************************      ELUTPSEL
01042 *                                                          *      ELUTPSEL
01043 *        PRE SELECT MODIFIER 2                             *      ELUTPSEL
01044 *                                                          *      ELUTPSEL
01045 ************************************************************      ELUTPSEL
01046  PRE-SELECT-MODIFIER-2.                                           ELUTPSEL
01047      MOVE TST-MODIFIER-2 (TST-FIRST-IDX) TO                       ELUTPSEL
01048          SSB-MODIFIER-2.                                          ELUTPSEL
01049      IF SSB-MODIFIER-2 = SPACES                                   ELUTPSEL
01050          PERFORM FLAG-ATTRIBUTE-AS-NOT-USED                       ELUTPSEL
01051      ELSE                                                         ELUTPSEL
01052          PERFORM FLAG-ATTRIBUTE-AS-DERIVED.                       ELUTPSEL
01053      PERFORM TOPIC-PROGRAM-FOUND.                                 ELUTPSEL
01054      EJECT                                                        ELUTPSEL
01055 ************************************************************      ELUTPSEL
01056 *                                                          *      ELUTPSEL
01057 *        TOPIC PROGRAM FOUND                               *      ELUTPSEL
01058 *                                                          *      ELUTPSEL
01059 ************************************************************      ELUTPSEL
01060  TOPIC-PROGRAM-FOUND.                                             ELUTPSEL
01061      MOVE TST-SEL-PGM-NAME (SSB-TOP-SEL-FIRST)                    ELUTPSEL
01062          TO SSB-TOPIC-PGM.                                        ELUTPSEL
01063                                                                   ELUTPSEL
01064                                                                   ELUTPSEL
01065 ************************************************************      ELUTPSEL
01066 *                                                          *      ELUTPSEL
01067 *        FLAG ATTRIBUTE AS DERIVED                         *      ELUTPSEL
01068 *                                                          *      ELUTPSEL
01069 ************************************************************      ELUTPSEL
01070  FLAG-ATTRIBUTE-AS-DERIVED.                                       ELUTPSEL
01071      SET WS-DERIVED-INPUT TO TRUE.                                ELUTPSEL
01072                                                                   ELUTPSEL
01073                                                                   ELUTPSEL
01074 ************************************************************      ELUTPSEL
01075 *                                                          *      ELUTPSEL
01076 *        FLAG ATTRIBUTE AS NOT USED                        *      ELUTPSEL
01077 *                                                          *      ELUTPSEL
01078 ************************************************************      ELUTPSEL
01079  FLAG-ATTRIBUTE-AS-NOT-USED.                                      ELUTPSEL
01080      SET SSB-NOT-USED (SSB-SELECTOR-STATE) TO TRUE.               ELUTPSEL
01081                                                                   ELUTPSEL
01082                                                                   ELUTPSEL
01083 ************************************************************      ELUTPSEL
01084 *                                                          *      ELUTPSEL
01085 *        CHOOSE TOPIC SELECTOR                             *      ELUTPSEL
01086 *                                                          *      ELUTPSEL
01087 ************************************************************      ELUTPSEL
01088  CHOOSE-TOPIC-SELECTOR.                                           ELUTPSEL
01089      PERFORM MOVE-PUSH-INDICATOR.                                 ELUTPSEL
01090      MOVE TST-SEL-PGM-NAME (SSB-TOP-SEL-FIRST)                    ELUTPSEL
01091          TO SSB-ACTION-MODULE.                                    ELUTPSEL
01092      SET SSB-INITIAL-CALL (SSB-SELECTOR-STATE) TO TRUE.           ELUTPSEL
01093      ADD 1 TO SSB-TOP-SEL-FIRST.                                  ELUTPSEL
01094      EJECT                                                        ELUTPSEL
01095 ************************************************************      ELUTPSEL
01096 *                                                          *      ELUTPSEL
01097 *        MOVE PUSH INDICATOR                               *      ELUTPSEL
01098 *                                                          *      ELUTPSEL
01099 ************************************************************      ELUTPSEL
01100  MOVE-PUSH-INDICATOR.                                             ELUTPSEL
01101      IF SSB-SS-GET-MODIFIER-1                                     ELUTPSEL
01102          MOVE TST-STACK-MODIFIER-1-SW (TST-FIRST-IDX)             ELUTPSEL
01103               TO SSB-PUSH-INDICATOR.                              ELUTPSEL
01104      IF SSB-SS-GET-MODIFIER-2                                     ELUTPSEL
01105          MOVE TST-STACK-MODIFIER-2-SW (TST-FIRST-IDX)             ELUTPSEL
01106               TO SSB-PUSH-INDICATOR.                              ELUTPSEL
01107                                                                   ELUTPSEL
01108                                                                   ELUTPSEL
01109 ************************************************************      ELUTPSEL
01110 *                                                          *      ELUTPSEL
01111 *        INVALID COMMAREA ABEND                            *      ELUTPSEL
01112 *                                                          *      ELUTPSEL
01113 ************************************************************      ELUTPSEL
01114  INVALID-COMMAREA-ABEND.                                          ELUTPSEL
01115      SET CIA-AB-DFHCOMMAREA TO TRUE.                              ELUTPSEL
01116      EXEC CICS ABEND                                              ELUTPSEL
01117                ABCODE(CIA-ABCODE)                                 ELUTPSEL
01118                END-EXEC.                                          ELUTPSEL
01119                                                                   ELUTPSEL
01120                                                                   ELUTPSEL
01121 ************************************************************      ELUTPSEL
01122 *                                                          *      ELUTPSEL
01123 *        TABLE SYNC ERROR                                  *      ELUTPSEL
01124 *                                                          *      ELUTPSEL
01125 ************************************************************      ELUTPSEL
01126  TABLE-SYNC-ERROR.                                                ELUTPSEL
01127      SET CIA-AB-TOP-SEL-SYNC TO TRUE.                             ELUTPSEL
01128      EXEC CICS ABEND                                              ELUTPSEL
01129                ABCODE(CIA-ABCODE)                                 ELUTPSEL
01130                END-EXEC.                                          ELUTPSEL
01131                                                                   ELUTPSEL
01132                                                                   ELUTPSEL
01133 ************************************************************      ELUTPSEL
01134 *                                                          *      ELUTPSEL
01135 *        INTERNAL TABLE ERROR                              *      ELUTPSEL
01136 *                                                          *      ELUTPSEL
01137 ************************************************************      ELUTPSEL
01138  INTERNAL-TABLE-ERROR.                                            ELUTPSEL
01139      SET CIA-AB-TOP-SEL TO TRUE.                                  ELUTPSEL
01140      EXEC CICS ABEND                                              ELUTPSEL
01141                ABCODE(CIA-ABCODE)                                 ELUTPSEL
01142                END-EXEC.                                          ELUTPSEL
01143      EJECT                                                        ELUTPSEL
01144 ************************************************************      ELUTPSEL
01145 *                                                          *      ELUTPSEL
01146 *        SYSTEM LOGIC ERROR                                *      ELUTPSEL
01147 *                                                          *      ELUTPSEL
01148 ************************************************************      ELUTPSEL
01149  SYSTEM-LOGIC-ERROR.                                              ELUTPSEL
01150      SET CIA-AB-UNDEF TO TRUE.                                    ELUTPSEL
01151      EXEC CICS ABEND                                              ELUTPSEL
01152                ABCODE(CIA-ABCODE)                                 ELUTPSEL
01153                END-EXEC.                                          ELUTPSEL
