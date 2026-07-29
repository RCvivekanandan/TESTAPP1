00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSCSMU 
00003  PROGRAM-ID.         ELSCSMU.                                        LV002
00004                                                                   ELSCSMU 
00005  AUTHOR.             RICK BARILEAU.                               ELSCSMU 
00006                                                                   ELSCSMU 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSCSMU 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSCSMU 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSCSMU 
00010                      233 N. MICHIGAN AVE                          ELSCSMU 
00011                      CHICAGO, ILLINOIS 60601                      ELSCSMU 
00012                                                                   ELSCSMU 
00013  DATE-WRITTEN.       03-DEC-1987.                                 ELSCSMU 
00014                                                                   ELSCSMU 
00015  DATE-COMPILED.                                                   ELSCSMU 
00016                                                                   ELSCSMU 
00017  SECURITY.           COPYRIGHT 1987,                              ELSCSMU 
00018                      HEALTH CARE SERVICE CORPORATION              ELSCSMU 
00019      SKIP3                                                        ELSCSMU 
00020 ******************************************************************ELSCSMU 
00021 *                                                                *ELSCSMU 
00022 *    PROGRAM:    ELSCSMU                                         *ELSCSMU 
00023 *    DATE:       03-DEC-1987                                     *ELSCSMU 
00024 *    AUTHOR:     RICK BARILEAU                                   *ELSCSMU 
00025 *    FUNCTION:   CONTRACT SUMMARY SELECTION PROGRAM              *ELSCSMU 
00026 *                                                                *ELSCSMU 
00027 ******************************************************************ELSCSMU 
00028 *                                                                *ELSCSMU 
00029 *                      MAINTENANCE HISTORY                       *ELSCSMU 
00030 *                                                                *ELSCSMU 
00031 *  MOD     DATE     BY  DRPT                ACTION               *ELSCSMU 
00032 * ----- ----------- --- ----- ---------------------------------- *ELSCSMU 
00033 * 01.00 03-DEC-1987 REB       CREATED                            *ELSCSMU 
00034 *                                                                *ELSCSMU 
00035 * 01.01 15-DEC-1987 REB       USERS REQUESTED THAT 'SERVICES' BE *ELSCSMU 
00036 *                             DISPLAYED WHERE 'BENEFITS' WAS.    *ELSCSMU 
00037 *                                                                *ELSCSMU 
00038 * 01.02 19-JAN-1988 REB       ALLOW PROGRAM TO HANDLE MULTIPLE   *ELSCSMU 
00039 *                             SELECTIONS ON C.S. MENU.           *ELSCSMU 
00040 *                                                                *ELSCSMU 
00041 * 01.03 25-JAN-1988 REB       CHANGED PARM THAT ELSMENU WILL SEE.*ELSCSMU 
00042 *                                                                *ELSCSMU 
00043 * 01.04 09-MAR-1988 REB       ADDED LOGIC TO STORE RESPONSES IN  *ELSCSMU 
00044 *                             NEW 'SSB-CS-MNU-RESPONSE-TABLE'    *ELSCSMU 
00045 *                             THAT IS WHERE ELTCSMRY WILL LOOK   *ELSCSMU 
00046 *                             FOR SELECTIONS OFF C.S. MENU.      *ELSCSMU 
00047 *                                                                *ELSCSMU 
00048 * 01.05 28-MAT-1988 REB       VERBIAGE CHANGE ON MENU.           *ELSCSMU 
00049 *                                                                *ELSCSMU 
00050 * 01.06 03-MAY-1988 NAC       INSERT LOGIC TO CARRY TOPIC TEXT   *ELSCSMU 
00051 *                             FOR ELUPROLG WITHIN THE SSCB.      *ELSCSMU 
00052 *                                                                *ELSCSMU 
00053 * 01.07 07-APR-1989 EGL       CONVERTED PROGRAM FROM STRUCTURES  *ELSCSMU 
00054 *                                                                *ELSCSMU 
00055 * 01.08 25-APR-1989 GEM       STORAGE MANAGEMENT ENHANCEMENTS    *ELSCSMU 
00056 *                                                                *ELSCSMU 
00057 * 01.09 27-AUG-1991 AKK       ADD PROCESSING TO LOCK SELECTED    *ELSCSMU 
00058 *                             GROUP/SECTIONS OUT OF CONTRACT     *ELSCSMU 
00059 *                             SUMMARY.  LOCKED OUT GROUP/SECT-   *ELSCSMU 
00060 *                             IONS ARE FOUND IN THE CODES MANUAL *ELSCSMU 
00061 *                             UNDER RECORD '@ELS' DATA ELEMENT   *ELSCSMU 
00062 *                             'LOCKOUT-GRP-SCTN-CS'.             *ELSCSMU 
00063 *                                                                *ELSCSMU 
00064 * 01.10 23-OCT-1997 AKK       ADD SUPPORT FOR YR 2000 AND TX     *ELSCSMU 
00065 *                             MERGE.                             *ELSCSMU 
00066 *       12-AUG-2003 AKK       GEN'D TO TEST ORDER OF COMPILE     *ELSCSMU 
00067 *                                                                *ELSCSMU 
00068 ******************************************************************ELSCSMU 
00069      EJECT                                                        ELSCSMU 
00070  ENVIRONMENT DIVISION.                                            ELSCSMU 
00071                                                                   ELSCSMU 
00072  CONFIGURATION SECTION.                                           ELSCSMU 
00073  SOURCE-COMPUTER.    IBM-3033.                                    ELSCSMU 
00074  OBJECT-COMPUTER.    IBM-3033.                                    ELSCSMU 
00075      SKIP3                                                        ELSCSMU 
00076  DATA DIVISION.                                                   ELSCSMU 
00077                                                                   ELSCSMU 
00078  FILE SECTION.                                                    ELSCSMU 
00079                                                                   ELSCSMU 
00080  WORKING-STORAGE SECTION.                                         ELSCSMU 
00081  01  MISC-WS.                                                     ELSCSMU 
00082      05  WS-GROUP-SECTION.                                        ELSCSMU 
00083          10  WS-GRP-NO       PIC X(06)         VALUE SPACE.       ELSCSMU 
00084          10  WS-SECTN-NO     PIC X(04)         VALUE SPACE.       ELSCSMU 
00085                                                                   ELSCSMU 
00086      05  CS-MNU-TITLE        PIC X(34)         VALUE 'SELECT CONTRELSCSMU 
00087 -    'ACT SUMMARY SUBTOPIC '.                                     ELSCSMU 
00088      05  CS-NUM-CATEGORIES   PIC S9(4) COMP    VALUE +8.          ELSCSMU 
00089      05  CS-NUM-HEADINGS     PIC S9(4) COMP    VALUE +15.         ELSCSMU 
00090                                                                   ELSCSMU 
00091      05  WS-SPECIAL-MSG.                                          ELSCSMU 
00092          10  FILLER          PIC X(44)         VALUE              ELSCSMU 
00093              'CONTRACT SUMMARY IS NOT AVAILABLE FOR GROUP '.      ELSCSMU 
00094          10  WS-GROUP        PIC X(06)         VALUE SPACES.      ELSCSMU 
00095          10  FILLER          PIC X(09)         VALUE              ELSCSMU 
00096              ' SECTION '.                                         ELSCSMU 
00097          10  WS-SECTION      PIC X(04)         VALUE SPACES.      ELSCSMU 
00098          10  FILLER          PICTURE X(01)                        ELSCSMU 
00099              VALUE '.'.                                           ELSCSMU 
00100                                                                   ELSCSMU 
00101      05 WS-SEE-TOPICS-MSG.                                        ELSCSMU 
00102         10 FILLER                PICTURE  X(79)                   ELSCSMU 
00103            VALUE 'SEE OTHER RELEVANT TOPICS FOR BENEFIT INFORMATIOELSCSMU 
00104 -                'N.'.                                            ELSCSMU 
00105      05  WS-ENTER-MSG.                                            ELSCSMU 
00106          10  FILLER          PIC X(25)         VALUE              ELSCSMU 
00107              'PRESS <ENTER> TO CONTINUE'.                         ELSCSMU 
00108                                                                   ELSCSMU 
00109  01  CS-CATEGORY-TABLE-AREA.                                      ELSCSMU 
00110      05  CS-HEADING-AREA.                                         ELSCSMU 
00111          10  FILLER             PIC X(79)      VALUE              ELSCSMU 
00112          'SELECT THE TOPIC(S) YOU WISH TO DISPLAY FROM THE LIST BEELSCSMU 
00113 -        'LOW AND PRESS THE '.                                    ELSCSMU 
00114          10  FILLER             PIC X(79)      VALUE              ELSCSMU 
00115              '<ENTER> KEY.'.                                      ELSCSMU 
00116          10  FILLER             PIC X(79)      VALUE SPACES.      ELSCSMU 
00117          10  FILLER             PIC X(79)      VALUE              ELSCSMU 
00118          '                                                KEYWORD ELSCSMU 
00119 -        '                      '.                                ELSCSMU 
00120          10  FILLER             PIC X(79)      VALUE SPACES.      ELSCSMU 
00121          10  FILLER             PIC X(79)      VALUE              ELSCSMU 
00122          '1.  COST CONTAINMENT                              CC'.  ELSCSMU 
00123          10  FILLER             PIC X(79)      VALUE              ELSCSMU 
00124          '2.  INPATIENT HOSPITAL SERVICES                   IPHB'.ELSCSMU 
00125          10  FILLER             PIC X(79)      VALUE              ELSCSMU 
00126          '3.  INPATIENT PHYSICIAN SERVICES                  MD'.  ELSCSMU 
00127          10  FILLER             PIC X(79)      VALUE              ELSCSMU 
00128          '4.  OUTPATIENT SERVICES                           OUT'. ELSCSMU 
00129          10  FILLER             PIC X(79)      VALUE              ELSCSMU 
00130          '5.  OB/STERILIZATION                              OBS'. ELSCSMU 
00131          10  FILLER             PIC X(79)      VALUE              ELSCSMU 
00132          '6.  PSYCHIATRIC                                   MENT'.ELSCSMU 
00133          10  FILLER             PIC X(79)      VALUE              ELSCSMU 
00134          '7.  GENERAL CONTRACT INFORMATION                  GCI'. ELSCSMU 
00135          10  FILLER             PIC X(79)      VALUE              ELSCSMU 
00136          '8.  ALL OF THE ABOVE                              ALL'. ELSCSMU 
00137          10  FILLER             PIC X(79)      VALUE SPACES.      ELSCSMU 
00138          10  FILLER             PIC X(79)      VALUE SPACES.      ELSCSMU 
00139                                                                   ELSCSMU 
00140      05  CS-HEADING             REDEFINES CS-HEADING-AREA         ELSCSMU 
00141                                 OCCURS 15 TIMES                   ELSCSMU 
00142                                 INDEXED BY CS-HEAD-IDX            ELSCSMU 
00143                                 PIC X(79).                        ELSCSMU 
00144                                                                   ELSCSMU 
00145      05  CS-VALID-SELECT-DEFINITION.                              ELSCSMU 
00146          10  FILLER             PIC X(05)            VALUE        ELSCSMU 
00147              '1CC  '.                                             ELSCSMU 
00148          10  FILLER             PIC X(05)            VALUE        ELSCSMU 
00149              '2IPHB'.                                             ELSCSMU 
00150          10  FILLER             PIC X(05)            VALUE        ELSCSMU 
00151              '3MD  '.                                             ELSCSMU 
00152          10  FILLER             PIC X(05)            VALUE        ELSCSMU 
00153              '4OUT '.                                             ELSCSMU 
00154          10  FILLER             PIC X(05)            VALUE        ELSCSMU 
00155              '5OBS '.                                             ELSCSMU 
00156          10  FILLER             PIC X(05)            VALUE        ELSCSMU 
00157              '6MENT'.                                             ELSCSMU 
00158          10  FILLER             PIC X(05)            VALUE        ELSCSMU 
00159              '7GCI '.                                             ELSCSMU 
00160          10  FILLER             PIC X(05)            VALUE        ELSCSMU 
00161              '8ALL '.                                             ELSCSMU 
00162 *                                                                 ELSCSMU 
00163      05  CS-CATEGORY-INFO  REDEFINES CS-VALID-SELECT-DEFINITION   ELSCSMU 
00164                            OCCURS 8 TIMES                         ELSCSMU 
00165                            INDEXED BY CS-IDX.                     ELSCSMU 
00166          10  CS-CHOICE     PIC X(01).                             ELSCSMU 
00167          10  CS-KEYWORD    PIC X(04).                             ELSCSMU 
00168                                                                   ELSCSMU 
00169  01  WS-DUMMY-PTR               POINTER.                          ELSCSMU 
00170      EJECT                                                        ELSCSMU 
00171  LINKAGE SECTION.                                                 ELSCSMU 
00172                                                                   ELSCSMU 
00173  01  DFHCOMMAREA.                                                 ELSCSMU 
00174      COPY ELSCOMMC.                                               ELSCSMU 
00175      EJECT                                                        ELSCSMU 
00176      COPY ELSCIA2C.                                               ELSCSMU 
00177      EJECT                                                        ELSCSMU 
00178      COPY ELSCMDSC.                                               ELSCSMU 
00179      EJECT                                                        ELSCSMU 
00180      COPY ELSCMIFC.                                               ELSCSMU 
00181      EJECT                                                        ELSCSMU 
00182      COPY ELSIOPMC.                                               ELSCSMU 
00183      EJECT                                                        ELSCSMU 
00184      COPY ELSSSCBC.                                               ELSCSMU 
00185      EJECT                                                        ELSCSMU 
00186      COPY ELSMHDGC.                                               ELSCSMU 
00187      EJECT                                                        ELSCSMU 
00188      COPY ELSMOPTC.                                               ELSCSMU 
00189      EJECT                                                        ELSCSMU 
00190 ************************************************************      ELSCSMU 
00191 *                                                          *      ELSCSMU 
00192 *                    PROCEDURE DIVISION                    *      ELSCSMU 
00193 *                                                          *      ELSCSMU 
00194 ************************************************************      ELSCSMU 
00195                                                                   ELSCSMU 
00196  PROCEDURE DIVISION.                                              ELSCSMU 
00197                                                                   ELSCSMU 
00198 ************************************************************      ELSCSMU 
00199 *                                                          *      ELSCSMU 
00200 *        PERFORM CONTRACT SUMMARY SELECTION FUNCTION       *      ELSCSMU 
00201 *                                                          *      ELSCSMU 
00202 ************************************************************      ELSCSMU 
00203  PERFORM-CONTRACT-SUMMARY-SELEC.                                  ELSCSMU 
00204      PERFORM INITIALIZE-MODULE.                                   ELSCSMU 
00205      PERFORM PROCESS-CONTRACT-SUMMARY.                            ELSCSMU 
00206      GOBACK.                                                      ELSCSMU 
00207      EJECT                                                        ELSCSMU 
00208 ************************************************************      ELSCSMU 
00209 *                                                          *      ELSCSMU 
00210 *        INITIALIZE                                        *      ELSCSMU 
00211 *                                                          *      ELSCSMU 
00212 ************************************************************      ELSCSMU 
00213  INITIALIZE-MODULE.                                               ELSCSMU 
00214      PERFORM CHECK-COMMAREA-LENGTH.                               ELSCSMU 
00215      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELSCSMU 
00216      PERFORM ESTABLISH-ADDRESSING-TO-SELECT.                      ELSCSMU 
00217      PERFORM ESTABLISH-ADDRESSING-TO-CMI.                         ELSCSMU 
00218                                                                   ELSCSMU 
00219 ************************************************************      ELSCSMU 
00220 *                                                          *      ELSCSMU 
00221 *        CHECK COMMAREA LENGTH                             *      ELSCSMU 
00222 *                                                          *      ELSCSMU 
00223 ************************************************************      ELSCSMU 
00224  CHECK-COMMAREA-LENGTH.                                           ELSCSMU 
00225      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELSCSMU 
00226         EXEC CICS ABEND ABCODE('EL01') END-EXEC.                  ELSCSMU 
00227                                                                   ELSCSMU 
00228 ************************************************************      ELSCSMU 
00229 *                                                          *      ELSCSMU 
00230 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELSCSMU 
00231 *                                                          *      ELSCSMU 
00232 ************************************************************      ELSCSMU 
00233  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELSCSMU 
00234      IF ECA-CIA-PTR IS NOT EQUAL NULL                             ELSCSMU 
00235         CALL 'ELUINISM' USING DFHCOMMAREA                         ELSCSMU 
00236                         ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA  ELSCSMU 
00237      ELSE                                                         ELSCSMU 
00238         EXEC CICS ABEND ABCODE('EL02') END-EXEC.                  ELSCSMU 
00239                                                                   ELSCSMU 
00240 ************************************************************      ELSCSMU 
00241 *                                                          *      ELSCSMU 
00242 *        ESTABLISH ADDRESSING TO SELECTOR CONTROL AREA     *      ELSCSMU 
00243 *                                                          *      ELSCSMU 
00244 ************************************************************      ELSCSMU 
00245  ESTABLISH-ADDRESSING-TO-SELECT.                                  ELSCSMU 
00246      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSCSMU 
00247      CALL 'ELUSETAD'                                              ELSCSMU 
00248         USING DFHCOMMAREA                                         ELSCSMU 
00249               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.             ELSCSMU 
00250      IF CIA-RC-PTR-NULL                                           ELSCSMU 
00251          PERFORM SIGNAL-MISSING-PARAMETER.                        ELSCSMU 
00252                                                                   ELSCSMU 
00253 ************************************************************      ELSCSMU 
00254 *                                                          *      ELSCSMU 
00255 *        ESTABLISH ADDRESSING TO CMI                       *      ELSCSMU 
00256 *                                                          *      ELSCSMU 
00257 ************************************************************      ELSCSMU 
00258  ESTABLISH-ADDRESSING-TO-CMI.                                     ELSCSMU 
00259      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSCSMU 
00260      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCSMU 
00261                            ADDRESS OF CMF-CODES-MANUAL-INTERFACE. ELSCSMU 
00262      IF CIA-RC-PTR-NULL                                           ELSCSMU 
00263         PERFORM ALLOCATE-CMI-AREA.                                ELSCSMU 
00264                                                                   ELSCSMU 
00265 ************************************************************      ELSCSMU 
00266 *                                                          *      ELSCSMU 
00267 *        ALLOCATE CMI AREA                                 *      ELSCSMU 
00268 *                                                          *      ELSCSMU 
00269 ************************************************************      ELSCSMU 
00270  ALLOCATE-CMI-AREA.                                               ELSCSMU 
00271      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSCSMU 
00272      SET CIA-STG-GETMAIN TO TRUE.                                 ELSCSMU 
00273      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSCSMU 
00274      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSCSMU 
00275      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCSMU 
00276                            ADDRESS OF CMF-CODES-MANUAL-INTERFACE. ELSCSMU 
00277      EJECT                                                        ELSCSMU 
00278 ************************************************************      ELSCSMU 
00279 *                                                          *      ELSCSMU 
00280 *        PROCESS CONTRACT SUMMARY                          *      ELSCSMU 
00281 *                                                          *      ELSCSMU 
00282 ************************************************************      ELSCSMU 
00283  PROCESS-CONTRACT-SUMMARY.                                        ELSCSMU 
00284      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSCSMU 
00285          PERFORM PROCESS-BUILD-MENU-REQUEST                       ELSCSMU 
00286      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSCSMU 
00287          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSCSMU 
00288      ELSE                                                         ELSCSMU 
00289          PERFORM SIGNAL-INVALID-CONTRACT-SUMMAR.                  ELSCSMU 
00290      EJECT                                                        ELSCSMU 
00291                                                                   ELSCSMU 
00292 ************************************************************      ELSCSMU 
00293 *                                                          *      ELSCSMU 
00294 *        PROCESS BUILD MENU REQUEST                        *      ELSCSMU 
00295 *                                                          *      ELSCSMU 
00296 ************************************************************      ELSCSMU 
00297  PROCESS-BUILD-MENU-REQUEST.                                      ELSCSMU 
00298      PERFORM DELETE-MENU-FILE.                                    ELSCSMU 
00299      PERFORM ACQUIRE-STORAGE-AREAS.                               ELSCSMU 
00300      PERFORM CHECK-GRP-SCTN-LOCKOUT.                              ELSCSMU 
00301      IF CMF-RC-OK                                                 ELSCSMU 
00302         PERFORM LOCKOUT-PROCESSING                                ELSCSMU 
00303      ELSE                                                         ELSCSMU 
00304         PERFORM NORMAL-PROCESSING.                                ELSCSMU 
00305      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSCSMU 
00306                                                                   ELSCSMU 
00307 ************************************************************      ELSCSMU 
00308 *                                                          *      ELSCSMU 
00309 *        DELETE MENU FILE                                  *      ELSCSMU 
00310 *                                                          *      ELSCSMU 
00311 ************************************************************      ELSCSMU 
00312  DELETE-MENU-FILE.                                                ELSCSMU 
00313      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSCSMU 
00314      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCSMU 
00315          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSCSMU 
00316      IF CIA-RC-PTR-NULL                                           ELSCSMU 
00317         PERFORM ALLOCATE-MENU-AREA                                ELSCSMU 
00318         SET CIA-ELSMENU-DDN TO TRUE                               ELSCSMU 
00319         CALL 'ELUSETAD'                                           ELSCSMU 
00320            USING DFHCOMMAREA                                      ELSCSMU 
00321                  ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.          ELSCSMU 
00322      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSCSMU 
00323      SET IOP-DEL          TO TRUE.                                ELSCSMU 
00324      SET IOP-FCQ-NONE     TO TRUE.                                ELSCSMU 
00325      SET IOP-KVQ-NONE     TO TRUE.                                ELSCSMU 
00326      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSCSMU 
00327                                                                   ELSCSMU 
00328 ************************************************************      ELSCSMU 
00329 *                                                          *      ELSCSMU 
00330 *        ALLOCATE MENU AREA                                *      ELSCSMU 
00331 *                                                          *      ELSCSMU 
00332 ************************************************************      ELSCSMU 
00333  ALLOCATE-MENU-AREA.                                              ELSCSMU 
00334      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSCSMU 
00335      SET CIA-STG-GETMAIN  TO TRUE.                                ELSCSMU 
00336      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSCSMU 
00337                                                                   ELSCSMU 
00338 ************************************************************      ELSCSMU 
00339 *                                                          *      ELSCSMU 
00340 *        ACQUIRE STORAGE AREAS                             *      ELSCSMU 
00341 *                                                          *      ELSCSMU 
00342 ************************************************************      ELSCSMU 
00343  ACQUIRE-STORAGE-AREAS.                                           ELSCSMU 
00344      PERFORM GET-HEADING-STORAGE-AREA.                            ELSCSMU 
00345      PERFORM GET-SELECTION-CODE-KEYWORD-ARE.                      ELSCSMU 
00346                                                                   ELSCSMU 
00347 ************************************************************      ELSCSMU 
00348 *                                                          *      ELSCSMU 
00349 *        GET HEADING STORAGE AREA                          *      ELSCSMU 
00350 *                                                          *      ELSCSMU 
00351 ************************************************************      ELSCSMU 
00352  GET-HEADING-STORAGE-AREA.                                        ELSCSMU 
00353      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSCSMU 
00354      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +         ELSCSMU 
00355               (LENGTH OF MHD-HDG-LINE *                           ELSCSMU 
00356          CS-NUM-HEADINGS).                                        ELSCSMU 
00357      SET CIA-STG-GETMAIN TO TRUE.                                 ELSCSMU 
00358      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSCSMU 
00359      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSCSMU 
00360      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCSMU 
00361          ADDRESS OF MHD-MENU-HEADINGS.                            ELSCSMU 
00362                                                                   ELSCSMU 
00363 ************************************************************      ELSCSMU 
00364 *                                                          *      ELSCSMU 
00365 *        GET SELECTION CODE KEYWORD AREA                   *      ELSCSMU 
00366 *                                                          *      ELSCSMU 
00367 ************************************************************      ELSCSMU 
00368  GET-SELECTION-CODE-KEYWORD-ARE.                                  ELSCSMU 
00369      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSCSMU 
00370      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +      ELSCSMU 
00371              (LENGTH OF MSO-MENU-OPT *                            ELSCSMU 
00372          CS-NUM-CATEGORIES).                                      ELSCSMU 
00373      SET CIA-STG-GETMAIN TO TRUE.                                 ELSCSMU 
00374      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSCSMU 
00375      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSCSMU 
00376      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCSMU 
00377          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSCSMU 
00378      EJECT                                                        ELSCSMU 
00379 ************************************************************      ELSCSMU 
00380 *                                                          *      ELSCSMU 
00381 *    CHECK GROUP/SECTION LOCKOUT                           *      ELSCSMU 
00382 *                                                          *      ELSCSMU 
00383 ************************************************************      ELSCSMU 
00384  CHECK-GRP-SCTN-LOCKOUT.                                          ELSCSMU 
00385      MOVE '@ELS' TO CMF-RECORD-PREFIX.                            ELSCSMU 
00386      MOVE 'LOCKOUT-GRP-SCTN-CS' TO CMF-ELEMENT-SYSTEM-NAME.       ELSCSMU 
00387      MOVE SSB-GRP-NO TO WS-GRP-NO.                                ELSCSMU 
00388      MOVE SSB-SECT-NO TO WS-SECTN-NO.                             ELSCSMU 
00389      MOVE WS-GROUP-SECTION TO CMF-CODE-VALUE.                     ELSCSMU 
00390      EXEC CICS LINK                                               ELSCSMU 
00391                PROGRAM ('ELUCMIF')                                ELSCSMU 
00392                COMMAREA (DFHCOMMAREA)                             ELSCSMU 
00393                END-EXEC.                                          ELSCSMU 
00394      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELSCSMU 
00395      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCSMU 
00396                            ADDRESS OF CMF-DESCR.                  ELSCSMU 
00397                                                                   ELSCSMU 
00398 ************************************************************      ELSCSMU 
00399 *                                                          *      ELSCSMU 
00400 *    GROUP/SECTION LOCKOUT PROCESSING                      *      ELSCSMU 
00401 *                                                          *      ELSCSMU 
00402 ************************************************************      ELSCSMU 
00403  LOCKOUT-PROCESSING.                                              ELSCSMU 
00404      MOVE CS-MNU-TITLE TO SSB-MNU-TITLE.                          ELSCSMU 
00405      MOVE CS-NUM-HEADINGS    TO MHD-NBR-HDG-LINES.                ELSCSMU 
00406      PERFORM ENTER-MSG-PROCESSING.                                ELSCSMU 
00407      PERFORM LOCKOUT-MSG-PROCESSING.                              ELSCSMU 
00408                                                                   ELSCSMU 
00409 ************************************************************      ELSCSMU 
00410 *                                                          *      ELSCSMU 
00411 *    ENTER MSG PROCESSING                                  *      ELSCSMU 
00412 *                                                          *      ELSCSMU 
00413 ************************************************************      ELSCSMU 
00414  ENTER-MSG-PROCESSING.                                            ELSCSMU 
00415      MOVE 0 TO MSO-MIN-CHOICES                                    ELSCSMU 
00416                MSO-MAX-CHOICES.                                   ELSCSMU 
00417      SET MSO-IDX TO 1.                                            ELSCSMU 
00418      MOVE SPACE TO MSO-OPT-SEL(MSO-IDX)                           ELSCSMU 
00419                    MSO-OPT-KWD(MSO-IDX).                          ELSCSMU 
00420      MOVE LENGTH OF CS-KEYWORD TO MSO-OPT-LEN.                    ELSCSMU 
00421      SET MSO-OPT-TYP-AN TO TRUE.                                  ELSCSMU 
00422      MOVE 1 TO MSO-NBR-MENU-OPTS.                                 ELSCSMU 
00423      MOVE WS-ENTER-MSG TO SSB-MNU-CHOICE-TABLE.                   ELSCSMU 
00424                                                                   ELSCSMU 
00425 ************************************************************      ELSCSMU 
00426 *                                                          *      ELSCSMU 
00427 *    LOCKOUT MESSAGE PROCESSING                            *      ELSCSMU 
00428 *                                                          *      ELSCSMU 
00429 ************************************************************      ELSCSMU 
00430  LOCKOUT-MSG-PROCESSING.                                          ELSCSMU 
00431      SET MHD-IDX TO 5.                                            ELSCSMU 
00432      MOVE SSB-GRP-NO TO WS-GROUP.                                 ELSCSMU 
00433      MOVE SSB-SECT-NO TO WS-SECTION.                              ELSCSMU 
00434      MOVE WS-SPECIAL-MSG TO MHD-HDG-LINE(MHD-IDX).                ELSCSMU 
00435      SET MHD-IDX TO 7.                                            ELSCSMU 
00436      MOVE WS-SEE-TOPICS-MSG TO MHD-HDG-LINE(MHD-IDX).             ELSCSMU 
00437      EJECT                                                        ELSCSMU 
00438 ************************************************************      ELSCSMU 
00439 *                                                          *      ELSCSMU 
00440 *        NORMAL PROCESSING                                 *      ELSCSMU 
00441 *                                                          *      ELSCSMU 
00442 ************************************************************      ELSCSMU 
00443  NORMAL-PROCESSING.                                               ELSCSMU 
00444      PERFORM INITIALIZE-MENU-CHOICES.                             ELSCSMU 
00445      MOVE SPACES TO SSB-MNU-CHOICE-TABLE.                         ELSCSMU 
00446      PERFORM BUILD-MENU-HEADERS.                                  ELSCSMU 
00447      PERFORM BUILD-MENU-BODY.                                     ELSCSMU 
00448                                                                   ELSCSMU 
00449 ************************************************************      ELSCSMU 
00450 *                                                          *      ELSCSMU 
00451 *        INITIALIZE MENU CHOICES                           *      ELSCSMU 
00452 *                                                          *      ELSCSMU 
00453 ************************************************************      ELSCSMU 
00454  INITIALIZE-MENU-CHOICES.                                         ELSCSMU 
00455      MOVE +1 TO MSO-MIN-CHOICES.                                  ELSCSMU 
00456      MOVE +7 TO MSO-MAX-CHOICES.                                  ELSCSMU 
00457                                                                   ELSCSMU 
00458 ************************************************************      ELSCSMU 
00459 *                                                          *      ELSCSMU 
00460 *        BUILD MENU HEADERS                                *      ELSCSMU 
00461 *                                                          *      ELSCSMU 
00462 ************************************************************      ELSCSMU 
00463  BUILD-MENU-HEADERS.                                              ELSCSMU 
00464      MOVE CS-MNU-TITLE       TO SSB-MNU-TITLE.                    ELSCSMU 
00465      MOVE CS-NUM-HEADINGS    TO MHD-NBR-HDG-LINES.                ELSCSMU 
00466      SET MHD-IDX TO 1.                                            ELSCSMU 
00467      PERFORM LOAD-MENU-HEADINGS                                   ELSCSMU 
00468         VARYING CS-HEAD-IDX FROM 1 BY 1                           ELSCSMU 
00469           UNTIL CS-HEAD-IDX > CS-NUM-HEADINGS.                    ELSCSMU 
00470                                                                   ELSCSMU 
00471 ************************************************************      ELSCSMU 
00472 *                                                          *      ELSCSMU 
00473 *        LOAD MENU HEADINGS                                *      ELSCSMU 
00474 *                                                          *      ELSCSMU 
00475 ************************************************************      ELSCSMU 
00476  LOAD-MENU-HEADINGS.                                              ELSCSMU 
00477      MOVE CS-HEADING(CS-HEAD-IDX) TO MHD-HDG-LINE(MHD-IDX).       ELSCSMU 
00478      SET MHD-IDX UP BY 1.                                         ELSCSMU 
00479                                                                   ELSCSMU 
00480 ************************************************************      ELSCSMU 
00481 *                                                          *      ELSCSMU 
00482 *        BUILD MENU BODY                                   *      ELSCSMU 
00483 *                                                          *      ELSCSMU 
00484 ************************************************************      ELSCSMU 
00485  BUILD-MENU-BODY.                                                 ELSCSMU 
00486      PERFORM INITIALIZE-BUILD-MENU-BODY.                          ELSCSMU 
00487      SET MSO-IDX TO 1.                                            ELSCSMU 
00488      PERFORM LOAD-TOPIC-CODES-KEYWORD                             ELSCSMU 
00489         VARYING CS-IDX FROM 1 BY 1                                ELSCSMU 
00490           UNTIL CS-IDX > CS-NUM-CATEGORIES.                       ELSCSMU 
00491                                                                   ELSCSMU 
00492 ************************************************************      ELSCSMU 
00493 *                                                          *      ELSCSMU 
00494 *        INITIALIZE BUILD MENU BODY                        *      ELSCSMU 
00495 *                                                          *      ELSCSMU 
00496 ************************************************************      ELSCSMU 
00497  INITIALIZE-BUILD-MENU-BODY.                                      ELSCSMU 
00498      MOVE CS-NUM-CATEGORIES    TO MSO-NBR-MENU-OPTS.              ELSCSMU 
00499      MOVE LENGTH OF CS-KEYWORD TO MSO-OPT-LEN.                    ELSCSMU 
00500      SET MSO-OPT-TYP-AN        TO TRUE.                           ELSCSMU 
00501                                                                   ELSCSMU 
00502 ************************************************************      ELSCSMU 
00503 *                                                          *      ELSCSMU 
00504 *        LOAD TOPIC CODES KEYWORD                          *      ELSCSMU 
00505 *                                                          *      ELSCSMU 
00506 ************************************************************      ELSCSMU 
00507  LOAD-TOPIC-CODES-KEYWORD.                                        ELSCSMU 
00508      MOVE CS-CHOICE(CS-IDX) TO MSO-OPT-SEL(MSO-IDX).              ELSCSMU 
00509      MOVE CS-KEYWORD(CS-IDX) TO MSO-OPT-KWD(MSO-IDX).             ELSCSMU 
00510      SET MSO-IDX UP BY 1.                                         ELSCSMU 
00511      EJECT                                                        ELSCSMU 
00512 ************************************************************      ELSCSMU 
00513 *                                                          *      ELSCSMU 
00514 *        PROCESS MENU COMPLETED REQUEST                    *      ELSCSMU 
00515 *                                                          *      ELSCSMU 
00516 ************************************************************      ELSCSMU 
00517  PROCESS-MENU-COMPLETED-REQUEST.                                  ELSCSMU 
00518      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSCSMU 
00519      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCSMU 
00520          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSCSMU 
00521      IF MSO-MIN-CHOICES = 0                                       ELSCSMU 
00522         PERFORM DO-LOCKOUT-MENU-COMPLETE                          ELSCSMU 
00523      ELSE                                                         ELSCSMU 
00524         PERFORM DO-NORMAL-MENU-COMPLETE.                          ELSCSMU 
00525                                                                   ELSCSMU 
00526 ************************************************************      ELSCSMU 
00527 *                                                          *      ELSCSMU 
00528 *        DO LOCKOUT MENU COMPLETE                          *      ELSCSMU 
00529 *                                                          *      ELSCSMU 
00530 *    NOTE: IN ORDER TO HANDLE LOCKOUT, WE WILL FORCE       *      ELSCSMU 
00531 *          TOPIC, SUBTOPIC AND MODIFIER-1 (CURRENT)        *      ELSCSMU 
00532 *          SELECTOR STATES BACK TO INITIAL AND RESET THE   *      ELSCSMU 
00533 *          'CURRENT' SELECTOR POINTER TO THE BEGINNING.    *      ELSCSMU 
00534 *                                                          *      ELSCSMU 
00535 ************************************************************      ELSCSMU 
00536  DO-LOCKOUT-MENU-COMPLETE.                                        ELSCSMU 
00537      MOVE SPACES TO SSB-TOPIC                                     ELSCSMU 
00538                     SSB-SUB-TOPIC                                 ELSCSMU 
00539                     SSB-MODIFIER-1.                               ELSCSMU 
00540      MOVE SPACES TO SSB-TOPIC-PHRASE                              ELSCSMU 
00541                     SSB-SUB-TOPIC-PHRASE                          ELSCSMU 
00542                     SSB-MODIFIER-1-PHRASE.                        ELSCSMU 
00543      SET SSB-INITIAL-CALL(SSB-SELECTOR-STATE) TO TRUE.            ELSCSMU 
00544      SET SSB-SS-GET-TOPIC TO TRUE.                                ELSCSMU 
00545      SET SSB-INITIAL-CALL(SSB-SELECTOR-STATE) TO TRUE.            ELSCSMU 
00546      SET SSB-SS-GET-SUBTOPIC TO TRUE.                             ELSCSMU 
00547      SET SSB-INITIAL-CALL(SSB-SELECTOR-STATE) TO TRUE.            ELSCSMU 
00548      SET SSB-SS-GET-GROUP TO TRUE.                                ELSCSMU 
00549                                                                   ELSCSMU 
00550 ************************************************************      ELSCSMU 
00551 *                                                          *      ELSCSMU 
00552 *        DO NORMAL MENU COMPLETE                           *      ELSCSMU 
00553 *                                                          *      ELSCSMU 
00554 ************************************************************      ELSCSMU 
00555  DO-NORMAL-MENU-COMPLETE.                                         ELSCSMU 
00556      SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.              ELSCSMU 
00557      MOVE 'CONTRACT SUMMARY' TO SSB-MODIFIER-1-PHRASE.            ELSCSMU 
00558      MOVE SPACES TO SSB-CS-MNU-RESPONSE-TABLE.                    ELSCSMU 
00559      SET SSB-MNU-IDX TO +1.                                       ELSCSMU 
00560      PERFORM STORE-CS-SELECTIONS-INTO-CS-RE                       ELSCSMU 
00561          VARYING SSB-CS-RESP-IDX FROM +1 BY +1                    ELSCSMU 
00562             UNTIL    SSB-CS-RESP-IDX > SSB-MNU-NUM-CHOICES        ELSCSMU 
00563                   OR SSB-CS-RESP-IDX > +10.                       ELSCSMU 
00564                                                                   ELSCSMU 
00565 ************************************************************      ELSCSMU 
00566 *                                                          *      ELSCSMU 
00567 *        STORE CS SELECTIONS INTO CS RESPONSE TABLE        *      ELSCSMU 
00568 *                                                          *      ELSCSMU 
00569 ************************************************************      ELSCSMU 
00570  STORE-CS-SELECTIONS-INTO-CS-RE.                                  ELSCSMU 
00571      MOVE SSB-MNU-CHOICE (SSB-MNU-IDX)                            ELSCSMU 
00572        TO SSB-CS-RESPONSE (SSB-CS-RESP-IDX).                      ELSCSMU 
00573      SET  SSB-MNU-IDX UP BY +1.                                   ELSCSMU 
00574      EJECT                                                        ELSCSMU 
00575 ************************************************************      ELSCSMU 
00576 *                                                          *      ELSCSMU 
00577 *        CALL INPUT-OUTPUT SUBPROGRAM                      *      ELSCSMU 
00578 *                                                          *      ELSCSMU 
00579 ************************************************************      ELSCSMU 
00580  CALL-INPUT-OUTPUT-SUBPROGRAM.                                    ELSCSMU 
00581      EXEC CICS LINK                                               ELSCSMU 
00582                PROGRAM ('ELUIOPGM')                               ELSCSMU 
00583                COMMAREA(DFHCOMMAREA)                              ELSCSMU 
00584                END-EXEC.                                          ELSCSMU 
00585                                                                   ELSCSMU 
00586 ************************************************************      ELSCSMU 
00587 *                                                          *      ELSCSMU 
00588 *        CALL STORAGE SUBPROGRAM                           *      ELSCSMU 
00589 *                                                          *      ELSCSMU 
00590 ************************************************************      ELSCSMU 
00591  CALL-STORAGE-SUBPROGRAM.                                         ELSCSMU 
00592      EXEC CICS LINK                                               ELSCSMU 
00593                PROGRAM ('ELUSTGMG')                               ELSCSMU 
00594                COMMAREA (DFHCOMMAREA)                             ELSCSMU 
00595                END-EXEC.                                          ELSCSMU 
00596                                                                   ELSCSMU 
00597 ************************************************************      ELSCSMU 
00598 *                                                          *      ELSCSMU 
00599 *        SIGNAL MISSING PARAMETER                          *      ELSCSMU 
00600 *                                                          *      ELSCSMU 
00601 ************************************************************      ELSCSMU 
00602  SIGNAL-MISSING-PARAMETER.                                        ELSCSMU 
00603      SET CIA-AB-PARM-MISSING TO TRUE.                             ELSCSMU 
00604      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELSCSMU 
00605                                                                   ELSCSMU 
00606 ************************************************************      ELSCSMU 
00607 *                                                          *      ELSCSMU 
00608 *        SIGNAL INVALID CONTRACT SUMMARY SELECTOR REQUEST  *      ELSCSMU 
00609 *                                                          *      ELSCSMU 
00610 ************************************************************      ELSCSMU 
00611  SIGNAL-INVALID-CONTRACT-SUMMAR.                                  ELSCSMU 
00612      SET CIA-AB-UNDEF TO TRUE.                                    ELSCSMU 
00613      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELSCSMU 
