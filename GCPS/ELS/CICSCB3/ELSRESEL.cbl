00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSRESEL
00003  PROGRAM-ID.         ELSRESEL.                                       LV002
00004                                                                   ELSRESEL
00005  AUTHOR.             EDWARD G LISS                                ELSRESEL
00006                                                                   ELSRESEL
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSRESEL
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSRESEL
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSRESEL
00010                      233 N. MICHIGAN AVE                          ELSRESEL
00011                      CHICAGO, ILLINOIS 60601                      ELSRESEL
00012                                                                   ELSRESEL
00013  DATE-WRITTEN.       06-NOV-1986.                                 ELSRESEL
00014                                                                   ELSRESEL
00015  DATE-COMPILED.                                                   ELSRESEL
00016                                                                   ELSRESEL
00017  SECURITY.           COPYRIGHT 1986,                              ELSRESEL
00018                      HEALTH CARE SERVICE CORPORATION              ELSRESEL
00019      SKIP3                                                        ELSRESEL
00020  ENVIRONMENT DIVISION.                                            ELSRESEL
00021                                                                   ELSRESEL
00022  CONFIGURATION SECTION.                                           ELSRESEL
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELSRESEL
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELSRESEL
00025      EJECT                                                        ELSRESEL
00026 ******************************************************************ELSRESEL
00027 *                                                                *ELSRESEL
00028 *    PROGRAM:    ELSRESEL                                        *ELSRESEL
00029 *    DATE:       06-NOV-1986                                     *ELSRESEL
00030 *    AUTHOR:     EDWARD G LISS                                   *ELSRESEL
00031 *    FUNCTION:                                                   *ELSRESEL
00032 *      THIS MODULE PRESENTS THE LIST OF OPTION THE USER HAS      *ELSRESEL
00033 *      IF THE RESELECT FUNCTION KEY IS PRESSED.                  *ELSRESEL
00034 *                                                                *ELSRESEL
00035 *    NOTES:                                                      *ELSRESEL
00036 *                                                                *ELSRESEL
00037 ******************************************************************ELSRESEL
00038 *                                                                *ELSRESEL
00039 *                      MAINTENANCE HISTORY                       *ELSRESEL
00040 *                                                                *ELSRESEL
00041 *  MOD     DATE     BY  DRPT                ACTION               *ELSRESEL
00042 * ----- ----------- --- ----- ---------------------------------- *ELSRESEL
00043 * 01.00 06-NOV-1986 EGL       CREATED                            *ELSRESEL
00044 *                                                                *ELSRESEL
00045 * 01.01 22-SEP-1987 REB       EXPLICITLY STATED ABEND CODE OF    *ELSRESEL
00046 *                             'EL01' WHEN INVALID COMMAREA EXISTS*ELSRESEL
00047 * 01.02 24-NOV-1987 EGL       ADDED SUPPORT FOR MENU STACK POP   *ELSRESEL
00048 *                             VIA PF3/15                         *ELSRESEL
00049 * 01.03 21-JAN-1988 EGL       ADDED TRANSLATION OF SELECTOR      *ELSRESEL
00050 *                             STATES TO MENU CHOICE FOR UN-      *ELSRESEL
00051 *                             STACKING MENUS.                    *ELSRESEL
00052 * 01.04 12-DEC-1988 EGL       CORRECT EL99 RESULTING FROM        *ELSRESEL
00053 *                             RESELECTION MENUS BEING STACKED    *ELSRESEL
00054 *                             WHEN THEY SHOULD NOT BE.  ALSO,    *ELSRESEL
00055 *                             CONVERTED TO NEW STORAGE MANAGER.  *ELSRESEL
00056 * 01.05 28-AUG-1989 EGL       DESTRUCTED PROGRAM                 *ELSRESEL
00057 * 01.06 20-AUG-1997 AKK       ADDED SUPPORT FOR YR 2000 AND TX   *ELSRESEL
00058 *                             MERGE.                             *ELSRESEL
00059 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST    *ELSRESEL
00060 ******************************************************************ELSRESEL
00061      EJECT                                                        ELSRESEL
00062  DATA DIVISION.                                                   ELSRESEL
00063                                                                   ELSRESEL
00064  WORKING-STORAGE SECTION.                                         ELSRESEL
00065                                                                   ELSRESEL
00066  01  WS-MISC-STUFF.                                               ELSRESEL
00067      05  WS-MOVE-SUB                PICTURE S9(4) COMP SYNC.      ELSRESEL
00068      05  WS-WORK-SUB                PICTURE S9(4) COMP SYNC.      ELSRESEL
00069      05  WS-DUMMY-PTR               POINTER.                      ELSRESEL
00070                                                                   ELSRESEL
00071      05  WS-DATE-EDIT-OUT.                                        ELSRESEL
00072          10  WS-DATE-EDIT-IN        PICTURE 99/99/99.             ELSRESEL
00073                                                                   ELSRESEL
00074      05  WS-TRANSLATE-CHOICE        PICTURE S9(4) COMP SYNC.      ELSRESEL
00075                                                                   ELSRESEL
00076      05  WS-RESPONSE-BREAKDOWN.                                   ELSRESEL
00077          10  WS-RESPONSE            PICTURE 99.                   ELSRESEL
00078          10  FILLER                 PICTURE X(14).                ELSRESEL
00079                                                                   ELSRESEL
00080 *                                                                 ELSRESEL
00081 *      MENU FORMAT AREAS                                          ELSRESEL
00082 *                                                                 ELSRESEL
00083                                                                   ELSRESEL
00084  01  WS-MENU-HEADINGS.                                            ELSRESEL
00085      05  WS-NUM-HEADING-LINES  PICTURE S9(4) COMP SYNC VALUE 1.   ELSRESEL
00086      05  WS-HEAD-LINE.                                            ELSRESEL
00087          10  FILLER            PICTURE X(69) VALUE                ELSRESEL
00088              'SELECT ALL THE CRITERION YOU WISH TO CHANGE FROM THEELSRESEL
00089 -            ' LIST BELOW.'.                                      ELSRESEL
00090      EJECT                                                        ELSRESEL
00091  01  WS-MENU-LINES.                                               ELSRESEL
00092      05  WS-MENU-LINE-1.                                          ELSRESEL
00093          10  FILLER            PICTURE S9(3) COMP   VALUE 1.      ELSRESEL
00094          10  FILLER            PICTURE S9(3) COMP   VALUE 1.      ELSRESEL
00095          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00096              'NEW GROUP/SECTION/SUBSRIBER NUMBER'.                ELSRESEL
00097      05  WS-MENU-LINE-2.                                          ELSRESEL
00098          10  FILLER            PICTURE S9(3) COMP   VALUE 2.      ELSRESEL
00099          10  FILLER            PICTURE S9(3) COMP   VALUE 2.      ELSRESEL
00100          10  FILLER            PICTURE X(23) VALUE                ELSRESEL
00101              'NEW SECTION NUMBER FOR '.                           ELSRESEL
00102          10  WS-MENU-LINE-GROUP                                   ELSRESEL
00103                                PICTURE X(6).                      ELSRESEL
00104          10  FILLER            PICTURE X(45) VALUE SPACES.        ELSRESEL
00105      05  WS-MENU-LINE-3.                                          ELSRESEL
00106          10  FILLER            PICTURE S9(3) COMP   VALUE 3.      ELSRESEL
00107          10  FILLER            PICTURE S9(3) COMP   VALUE 5.      ELSRESEL
00108          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00109              'NEW TOPIC'.                                         ELSRESEL
00110      05  WS-MENU-LINE-4.                                          ELSRESEL
00111          10  FILLER            PICTURE S9(3) COMP   VALUE 4.      ELSRESEL
00112          10  FILLER            PICTURE S9(3) COMP   VALUE 6.      ELSRESEL
00113          10  FILLER            PICTURE X(18) VALUE                ELSRESEL
00114              'NEW SUB-TOPIC FOR '.                                ELSRESEL
00115          10  WS-MENU-SUB-TOPIC PICTURE X(56) VALUE SPACES.        ELSRESEL
00116      05  WS-MENU-LINE-5.                                          ELSRESEL
00117          10  FILLER            PICTURE S9(3) COMP   VALUE 5.      ELSRESEL
00118          10  FILLER            PICTURE S9(3) COMP   VALUE 7.      ELSRESEL
00119          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00120              'DISPLAY BENEFITS FOR INSTITUTIONAL, PROFESSIONAL OR ELSRESEL
00121 -            'BOTH'.                                              ELSRESEL
00122      05  WS-MENU-LINE-6.                                          ELSRESEL
00123          10  FILLER            PICTURE S9(3) COMP   VALUE 6.      ELSRESEL
00124          10  FILLER            PICTURE S9(3) COMP   VALUE 8.      ELSRESEL
00125          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00126              'MEDICARE ELIGIBILITY'.                              ELSRESEL
00127      05  WS-MENU-LINE-7.                                          ELSRESEL
00128          10  FILLER            PICTURE S9(3) COMP   VALUE 7.      ELSRESEL
00129          10  FILLER            PICTURE S9(3) COMP   VALUE 9.      ELSRESEL
00130          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00131              'FAMILY RELATIONSHIP'.                               ELSRESEL
00132      05  WS-MENU-LINE-8.                                          ELSRESEL
00133          10  FILLER            PICTURE S9(3) COMP   VALUE 8.      ELSRESEL
00134          10  FILLER            PICTURE S9(3) COMP   VALUE 10.     ELSRESEL
00135          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00136              'PATIENT AGE'.                                       ELSRESEL
00137      05  WS-MENU-LINE-9.                                          ELSRESEL
00138          10  FILLER            PICTURE S9(3) COMP   VALUE 9.      ELSRESEL
00139          10  FILLER            PICTURE S9(3) COMP   VALUE 11.     ELSRESEL
00140          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00141              'GROUP SPECIFIC EFFECTIVE DATE'.                     ELSRESEL
00142      05  WS-MENU-LINE-10.                                         ELSRESEL
00143          10  FILLER            PICTURE S9(3) COMP   VALUE 10.     ELSRESEL
00144          10  FILLER            PICTURE S9(3) COMP   VALUE 12.     ELSRESEL
00145          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00146              'PROVIDER CONTROL - BASIC INSTITUTIONAL'.            ELSRESEL
00147      05  WS-MENU-LINE-11.                                         ELSRESEL
00148          10  FILLER            PICTURE S9(3) COMP   VALUE 11.     ELSRESEL
00149          10  FILLER            PICTURE S9(3) COMP   VALUE 13.     ELSRESEL
00150          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00151              'PROVIDER CONTROL - SUPPLEMENTAL INSTITUTIONAL'.     ELSRESEL
00152      05  WS-MENU-LINE-12.                                         ELSRESEL
00153          10  FILLER            PICTURE S9(3) COMP   VALUE 12.     ELSRESEL
00154          10  FILLER            PICTURE S9(3) COMP   VALUE 14.     ELSRESEL
00155          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00156              'PROVIDER CONTROL - BASIC PROFESSIONAL'.             ELSRESEL
00157      05  WS-MENU-LINE-13.                                         ELSRESEL
00158          10  FILLER            PICTURE S9(3) COMP   VALUE 13.     ELSRESEL
00159          10  FILLER            PICTURE S9(3) COMP   VALUE 15.     ELSRESEL
00160          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00161              'PROVIDER CONTROL - SUPPLEMENTAL PROFESSIONAL'.      ELSRESEL
00162      05  WS-MENU-LINE-14.                                         ELSRESEL
00163          10  FILLER            PICTURE S9(3) COMP   VALUE 14.     ELSRESEL
00164          10  FILLER            PICTURE S9(3) COMP   VALUE 16.     ELSRESEL
00165          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00166              'CONTRACT EFFECTIVE DATE - BASIC INSTITUTIONAL'.     ELSRESEL
00167      05  WS-MENU-LINE-15.                                         ELSRESEL
00168          10  FILLER            PICTURE S9(3) COMP   VALUE 15.     ELSRESEL
00169          10  FILLER            PICTURE S9(3) COMP   VALUE 17.     ELSRESEL
00170          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00171            'CONTRACT EFFECTIVE DATE - SUPPLEMENTAL INSTITUTIONAL'.ELSRESEL
00172      05  WS-MENU-LINE-16.                                         ELSRESEL
00173          10  FILLER            PICTURE S9(3) COMP   VALUE 16.     ELSRESEL
00174          10  FILLER            PICTURE S9(3) COMP   VALUE 18.     ELSRESEL
00175          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00176              'CONTRACT EFFECTIVE DATE - BASIC PROFESSIONAL'.      ELSRESEL
00177      05  WS-MENU-LINE-17.                                         ELSRESEL
00178          10  FILLER            PICTURE S9(3) COMP   VALUE 17.     ELSRESEL
00179          10  FILLER            PICTURE S9(3) COMP   VALUE 19.     ELSRESEL
00180          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00181            'CONTRACT EFFECTIVE DATE - SUPPLEMENTAL PROFESSIONAL'. ELSRESEL
00182      05  WS-MENU-LINE-18.                                         ELSRESEL
00183          10  FILLER            PICTURE S9(3) COMP   VALUE 18.     ELSRESEL
00184          10  FILLER            PICTURE S9(3) COMP   VALUE 20.     ELSRESEL
00185          10  FILLER            PICTURE X(74) VALUE                ELSRESEL
00186              'DISPLAY BENEFITS FOR INPATIENT, OUTPATIENT OR BOTH'.ELSRESEL
00187      05  WS-MENU-LINE-19.                                         ELSRESEL
00188          10  FILLER            PICTURE S9(3) COMP   VALUE 19.     ELSRESEL
00189          10  FILLER            PICTURE S9(3) COMP   VALUE 21.     ELSRESEL
00190          10  FILLER            PICTURE X(15) VALUE                ELSRESEL
00191                                              'NEW OPTION FOR '.   ELSRESEL
00192          10  WS-MENU-MOD-1     PICTURE X(59) VALUE SPACES.        ELSRESEL
00193      05  WS-MENU-LINE-20.                                         ELSRESEL
00194          10  FILLER            PICTURE S9(3) COMP   VALUE 20.     ELSRESEL
00195          10  FILLER            PICTURE S9(3) COMP   VALUE 22.     ELSRESEL
00196          10  WS-MENU-MOD-2     PICTURE X(74) VALUE SPACES.        ELSRESEL
00197                                                                   ELSRESEL
00198  01  WS-MENU-TABLE-AREA  REDEFINES WS-MENU-LINES.                 ELSRESEL
00199      05  WS-MENU-TABLE         OCCURS 20 TIMES                    ELSRESEL
00200                                INDEXED BY WS-MENU-IDX.            ELSRESEL
00201          10  WS-MENU-CHOICE    PICTURE S9(3) COMP.                ELSRESEL
00202              88  WS-MENU-GROUP              VALUE 1.              ELSRESEL
00203              88  WS-MENU-SECTION            VALUE 2.              ELSRESEL
00204              88  WS-MENU-TOPIC              VALUE 3.              ELSRESEL
00205              88  WS-MENU-SUBTOPIC           VALUE 4.              ELSRESEL
00206              88  WS-MENU-PROVIDER-CLASS     VALUE 5.              ELSRESEL
00207              88  WS-MENU-MEDCA-ELIG         VALUE 6.              ELSRESEL
00208              88  WS-MENU-FAM-REL            VALUE 7.              ELSRESEL
00209              88  WS-MENU-PT-AGE             VALUE 8.              ELSRESEL
00210              88  WS-MENU-GRP-SPEC-EFF-DATE  VALUE 9.              ELSRESEL
00211              88  WS-MENU-PROV-CTL-BAS-INST  VALUE 10.             ELSRESEL
00212              88  WS-MENU-PROV-CTL-SUP-INST  VALUE 11.             ELSRESEL
00213              88  WS-MENU-PROV-CTL-BAS-PROF  VALUE 12.             ELSRESEL
00214              88  WS-MENU-PROV-CTL-SUP-PROF  VALUE 13.             ELSRESEL
00215              88  WS-MENU-CONT-EFF-BAS-INST  VALUE 14.             ELSRESEL
00216              88  WS-MENU-CONT-EFF-SUP-INST  VALUE 15.             ELSRESEL
00217              88  WS-MENU-CONT-EFF-BAS-PROF  VALUE 16.             ELSRESEL
00218              88  WS-MENU-CONT-EFF-SUP-PROF  VALUE 17.             ELSRESEL
00219              88  WS-MENU-SERVICE-LOC        VALUE 18.             ELSRESEL
00220              88  WS-MENU-MODIFIER-1         VALUE 19.             ELSRESEL
00221              88  WS-MENU-MODIFIER-2         VALUE 20.             ELSRESEL
00222          10  WS-SEL-STATE-ARG  PICTURE S9(3) COMP  .              ELSRESEL
00223          10  WS-MENU-TEXT      PICTURE X(74).                     ELSRESEL
00224                                                                   ELSRESEL
00225  01  WS-NUM-MENU-LINES      PICTURE S9(4) COMP SYNC VALUE 20.     ELSRESEL
00226                                                                   ELSRESEL
00227  01  WS-FORMATTED-LINE.                                           ELSRESEL
00228      05  WS-FL-CHOICE       PICTURE ZZZ9B.                        ELSRESEL
00229      05  WS-FL-CHOICE-X  REDEFINES WS-FL-CHOICE                   ELSRESEL
00230                             PICTURE X(5).                         ELSRESEL
00231      05  WS-FL-TEXT         PICTURE X(74).                        ELSRESEL
00232      EJECT                                                        ELSRESEL
00233      COPY DFHAID.                                                 ELSRESEL
00234      EJECT                                                        ELSRESEL
00235  01  WS-HGADATE-PARM.                                             ELSRESEL
00236      COPY HGCDAT01.                                               ELSRESEL
00237      EJECT                                                        ELSRESEL
00238  LINKAGE SECTION.                                                 ELSRESEL
00239                                                                   ELSRESEL
00240  01  DFHCOMMAREA.                                                 ELSRESEL
00241  COPY ELSCOMMC.                                                   ELSRESEL
00242      EJECT                                                        ELSRESEL
00243  COPY ELSCIA2C.                                                   ELSRESEL
00244      EJECT                                                        ELSRESEL
00245  COPY ELSSSCBC.                                                   ELSRESEL
00246      EJECT                                                        ELSRESEL
00247  COPY ELSMHDGC.                                                   ELSRESEL
00248      EJECT                                                        ELSRESEL
00249  COPY ELSMOPTC.                                                   ELSRESEL
00250      EJECT                                                        ELSRESEL
00251  COPY ELSMENUC.                                                   ELSRESEL
00252      EJECT                                                        ELSRESEL
00253  COPY ELSKTBSC.                                                   ELSRESEL
00254      EJECT                                                        ELSRESEL
00255  COPY ELSIOPMC.                                                   ELSRESEL
00256      EJECT                                                        ELSRESEL
00257  PROCEDURE DIVISION.                                              ELSRESEL
00258 ************************************************************      ELSRESEL
00259 *                                                          *      ELSRESEL
00260 *        RESELECTION PROCESSING                            *      ELSRESEL
00261 *                                                          *      ELSRESEL
00262 ************************************************************      ELSRESEL
00263  RESELECTION-PROCESSING.                                          ELSRESEL
00264      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELSRESEL
00265          PERFORM INVALID-COMMAREA-ABEND.                          ELSRESEL
00266      PERFORM INITIALIZE-MODULE.                                   ELSRESEL
00267      PERFORM MAIN-PROCESSING.                                     ELSRESEL
00268      EXEC CICS RETURN                                             ELSRESEL
00269                END-EXEC.                                          ELSRESEL
00270                                                                   ELSRESEL
00271                                                                   ELSRESEL
00272 ************************************************************      ELSRESEL
00273 *                                                          *      ELSRESEL
00274 *        INITIALIZE MODULE                                 *      ELSRESEL
00275 *                                                          *      ELSRESEL
00276 ************************************************************      ELSRESEL
00277  INITIALIZE-MODULE.                                               ELSRESEL
00278      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSRESEL
00279          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSRESEL
00280      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSRESEL
00281      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSRESEL
00282          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSRESEL
00283      SET CIA-ELSKTBS-DDN TO TRUE.                                 ELSRESEL
00284      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSRESEL
00285          ADDRESS OF KTS-SECTIONS-KEY-TABLE.                       ELSRESEL
00286      IF CIA-RC-PTR-NULL                                           ELSRESEL
00287          PERFORM READ-SECTION-TABLE.                              ELSRESEL
00288 /***********************************************************      ELSRESEL
00289 *                                                          *      ELSRESEL
00290 *        READ SECTION TABLE                                *      ELSRESEL
00291 *                                                          *      ELSRESEL
00292 ************************************************************      ELSRESEL
00293  READ-SECTION-TABLE.                                              ELSRESEL
00294      SET CIA-ELSKTBS-DDN TO TRUE.                                 ELSRESEL
00295      SET CIA-STG-RETRIEVE TO TRUE.                                ELSRESEL
00296      PERFORM CALL-STORAGE-MANAGER.                                ELSRESEL
00297      SET CIA-ELSKTBS-DDN TO TRUE.                                 ELSRESEL
00298      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSRESEL
00299          ADDRESS OF KTS-SECTIONS-KEY-TABLE.                       ELSRESEL
00300      IF CIA-RC-PTR-NULL                                           ELSRESEL
00301          PERFORM SYSTEM-LOGIC-ERROR.                              ELSRESEL
00302                                                                   ELSRESEL
00303                                                                   ELSRESEL
00304 ************************************************************      ELSRESEL
00305 *                                                          *      ELSRESEL
00306 *        MAIN PROCESSING                                   *      ELSRESEL
00307 *                                                          *      ELSRESEL
00308 ************************************************************      ELSRESEL
00309  MAIN-PROCESSING.                                                 ELSRESEL
00310      IF EIBAID = DFHPF3 OR DFHPF15                                ELSRESEL
00311          PERFORM RESELECT-PUSHED-MENU                             ELSRESEL
00312      ELSE                                                         ELSRESEL
00313          IF EIBAID = DFHPF4 OR DFHPF16                            ELSRESEL
00314              PERFORM RESELECT-TOPIC-ONLY                          ELSRESEL
00315          ELSE                                                     ELSRESEL
00316              IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)             ELSRESEL
00317                  PERFORM PREPARE-RESELECTION-MENU                 ELSRESEL
00318              ELSE                                                 ELSRESEL
00319                  IF SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)        ELSRESEL
00320                      PERFORM PROCESS-RESPONSE                     ELSRESEL
00321                  ELSE                                             ELSRESEL
00322                      PERFORM SYSTEM-LOGIC-ERROR.                  ELSRESEL
00323 /***********************************************************      ELSRESEL
00324 *                                                          *      ELSRESEL
00325 *        RESELECT PUSHED MENU                              *      ELSRESEL
00326 *                                                          *      ELSRESEL
00327 ************************************************************      ELSRESEL
00328  RESELECT-PUSHED-MENU.                                            ELSRESEL
00329      IF SSB-STACK-EMPTY                                           ELSRESEL
00330          PERFORM RESELECT-TOPIC-ONLY                              ELSRESEL
00331      ELSE                                                         ELSRESEL
00332          PERFORM UNSTACK-A-MENU.                                  ELSRESEL
00333                                                                   ELSRESEL
00334                                                                   ELSRESEL
00335 ************************************************************      ELSRESEL
00336 *                                                          *      ELSRESEL
00337 *        UNSTACK A MENU                                    *      ELSRESEL
00338 *                                                          *      ELSRESEL
00339 ************************************************************      ELSRESEL
00340  UNSTACK-A-MENU.                                                  ELSRESEL
00341      PERFORM FORCE-MENU-SELECTION.                                ELSRESEL
00342      SUBTRACT 1 FROM SSB-STACK-CURRENT-ITEM.                      ELSRESEL
00343      PERFORM PROCESS-RESPONSE.                                    ELSRESEL
00344                                                                   ELSRESEL
00345                                                                   ELSRESEL
00346 ************************************************************      ELSRESEL
00347 *                                                          *      ELSRESEL
00348 *        FORCE MENU SELECTION                              *      ELSRESEL
00349 *                                                          *      ELSRESEL
00350 ************************************************************      ELSRESEL
00351  FORCE-MENU-SELECTION.                                            ELSRESEL
00352      MOVE SSB-STACK-STATE (SSB-STACK-CURRENT-ITEM)                ELSRESEL
00353         TO WS-TRANSLATE-CHOICE.                                   ELSRESEL
00354      SET WS-MENU-IDX TO 1.                                        ELSRESEL
00355      SEARCH WS-MENU-TABLE VARYING WS-MENU-IDX                     ELSRESEL
00356          AT END                                                   ELSRESEL
00357              MOVE ZERO TO WS-TRANSLATE-CHOICE                     ELSRESEL
00358          WHEN WS-SEL-STATE-ARG (WS-MENU-IDX) =                    ELSRESEL
00359               WS-TRANSLATE-CHOICE                                 ELSRESEL
00360              SET WS-TRANSLATE-CHOICE TO WS-MENU-IDX               ELSRESEL
00361         END-SEARCH.                                               ELSRESEL
00362      IF WS-TRANSLATE-CHOICE = ZERO                                ELSRESEL
00363          PERFORM PROGRAM-LOGIC-ERROR.                             ELSRESEL
00364      MOVE WS-TRANSLATE-CHOICE TO WS-RESPONSE.                     ELSRESEL
00365      MOVE WS-RESPONSE TO SSB-MNU-CHOICE (1).                      ELSRESEL
00366      MOVE 1 TO SSB-MNU-NUM-CHOICES.                               ELSRESEL
00367 /***********************************************************      ELSRESEL
00368 *                                                          *      ELSRESEL
00369 *        RESELECT TOPIC ONLY                               *      ELSRESEL
00370 *                                                          *      ELSRESEL
00371 ************************************************************      ELSRESEL
00372  RESELECT-TOPIC-ONLY.                                             ELSRESEL
00373      PERFORM RESELECT-TOPIC.                                      ELSRESEL
00374      PERFORM SET-EXIT-STATUS.                                     ELSRESEL
00375                                                                   ELSRESEL
00376                                                                   ELSRESEL
00377 ************************************************************      ELSRESEL
00378 *                                                          *      ELSRESEL
00379 *        PREPARE RESELECTION MENU                          *      ELSRESEL
00380 *                                                          *      ELSRESEL
00381 ************************************************************      ELSRESEL
00382  PREPARE-RESELECTION-MENU.                                        ELSRESEL
00383      PERFORM INITIALIZE-MENU.                                     ELSRESEL
00384      MOVE SSB-GRP-NO TO WS-MENU-LINE-GROUP.                       ELSRESEL
00385      PERFORM DISPLAY-GROUP-SECTION-OPTION.                        ELSRESEL
00386      IF NOT SSB-NO-GRP-NO                                         ELSRESEL
00387              AND KTS-NBR-KEYS > 1                                 ELSRESEL
00388          PERFORM DISPLAY-SECTION-OPTION.                          ELSRESEL
00389      PERFORM ADD-LINES-TO-MENU                                    ELSRESEL
00390          VARYING WS-MENU-IDX FROM 3 BY 1                          ELSRESEL
00391                  UNTIL WS-MENU-IDX > WS-NUM-MENU-LINES.           ELSRESEL
00392      PERFORM BEGIN-MENU-MODE.                                     ELSRESEL
00393 /***********************************************************      ELSRESEL
00394 *                                                          *      ELSRESEL
00395 *        INITIALIZE MENU                                   *      ELSRESEL
00396 *                                                          *      ELSRESEL
00397 ************************************************************      ELSRESEL
00398  INITIALIZE-MENU.                                                 ELSRESEL
00399      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSRESEL
00400      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSRESEL
00401          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSRESEL
00402      IF CIA-RC-PTR-NULL                                           ELSRESEL
00403          PERFORM ALLOCATE-MENU-IO-PARAMETER-BLO.                  ELSRESEL
00404      PERFORM INITIALIZE-THE-MENU-FILE.                            ELSRESEL
00405      PERFORM INITIALIZE-THE-MENU-TABLES.                          ELSRESEL
00406                                                                   ELSRESEL
00407                                                                   ELSRESEL
00408 ************************************************************      ELSRESEL
00409 *                                                          *      ELSRESEL
00410 *        ALLOCATE MENU IO PARAMETER BLOCK                  *      ELSRESEL
00411 *                                                          *      ELSRESEL
00412 ************************************************************      ELSRESEL
00413  ALLOCATE-MENU-IO-PARAMETER-BLO.                                  ELSRESEL
00414      SET CIA-ELSMENU-DDN    TO  TRUE.                             ELSRESEL
00415      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSRESEL
00416      PERFORM CALL-THE-STORAGE-MANAGER.                            ELSRESEL
00417      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSRESEL
00418      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSRESEL
00419          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSRESEL
00420                                                                   ELSRESEL
00421                                                                   ELSRESEL
00422 ************************************************************      ELSRESEL
00423 *                                                          *      ELSRESEL
00424 *        INITIALIZE THE MENU FILE                          *      ELSRESEL
00425 *                                                          *      ELSRESEL
00426 ************************************************************      ELSRESEL
00427  INITIALIZE-THE-MENU-FILE.                                        ELSRESEL
00428      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSRESEL
00429      SET IOP-DEL TO TRUE.                                         ELSRESEL
00430      SET IOP-FCQ-NONE TO TRUE.                                    ELSRESEL
00431      SET IOP-KVQ-NONE TO TRUE.                                    ELSRESEL
00432      EXEC CICS LINK                                               ELSRESEL
00433                PROGRAM('ELUIOPGM')                                ELSRESEL
00434                COMMAREA(DFHCOMMAREA)                              ELSRESEL
00435                END-EXEC.                                          ELSRESEL
00436 /***********************************************************      ELSRESEL
00437 *                                                          *      ELSRESEL
00438 *        INITIALIZE THE MENU TABLES                        *      ELSRESEL
00439 *                                                          *      ELSRESEL
00440 ************************************************************      ELSRESEL
00441  INITIALIZE-THE-MENU-TABLES.                                      ELSRESEL
00442      PERFORM INITIALIZE-MENU-SELECTIONS.                          ELSRESEL
00443      PERFORM INITIALIZE-MENU-HEADINGS.                            ELSRESEL
00444      PERFORM INITIALIZE-MENU-DESCRIPTIONS.                        ELSRESEL
00445                                                                   ELSRESEL
00446                                                                   ELSRESEL
00447 ************************************************************      ELSRESEL
00448 *                                                          *      ELSRESEL
00449 *        INITIALIZE MENU SELECTIONS                        *      ELSRESEL
00450 *                                                          *      ELSRESEL
00451 ************************************************************      ELSRESEL
00452  INITIALIZE-MENU-SELECTIONS.                                      ELSRESEL
00453      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSRESEL
00454      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER        ELSRESEL
00455          +  LENGTH OF MSO-MENU-OPT * WS-NUM-MENU-LINES.           ELSRESEL
00456      SET CIA-STG-GETMAIN TO TRUE.                                 ELSRESEL
00457      PERFORM CALL-THE-STORAGE-MANAGER.                            ELSRESEL
00458      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSRESEL
00459      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSRESEL
00460          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSRESEL
00461      MOVE 2 TO MSO-OPT-LEN.                                       ELSRESEL
00462      SET MSO-OPT-TYP-NUM TO TRUE.                                 ELSRESEL
00463      MOVE ZERO TO MSO-NBR-MENU-OPTS.                              ELSRESEL
00464                                                                   ELSRESEL
00465                                                                   ELSRESEL
00466 ************************************************************      ELSRESEL
00467 *                                                          *      ELSRESEL
00468 *        INITIALIZE MENU HEADINGS                          *      ELSRESEL
00469 *                                                          *      ELSRESEL
00470 ************************************************************      ELSRESEL
00471  INITIALIZE-MENU-HEADINGS.                                        ELSRESEL
00472      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSRESEL
00473      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES           ELSRESEL
00474          +                                                        ELSRESEL
00475                         LENGTH OF MHD-HDG-LINE *                  ELSRESEL
00476                         WS-NUM-HEADING-LINES.                     ELSRESEL
00477      SET CIA-STG-GETMAIN TO TRUE.                                 ELSRESEL
00478      PERFORM CALL-THE-STORAGE-MANAGER.                            ELSRESEL
00479      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSRESEL
00480      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSRESEL
00481         ADDRESS OF MHD-MENU-HEADINGS.                             ELSRESEL
00482 /***********************************************************      ELSRESEL
00483 *                                                          *      ELSRESEL
00484 *        INITIALIZE MENU DESCRIPTIONS                      *      ELSRESEL
00485 *                                                          *      ELSRESEL
00486 ************************************************************      ELSRESEL
00487  INITIALIZE-MENU-DESCRIPTIONS.                                    ELSRESEL
00488      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSRESEL
00489      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSRESEL
00490                            WS-DUMMY-PTR.                          ELSRESEL
00491      SET IOP-GETMAIN-REC TO TRUE.                                 ELSRESEL
00492      COMPUTE IOP-REC-LEN = LENGTH OF MSD-NBR-DESCR-LINES          ELSRESEL
00493          + LENGTH OF MSD-DESCR-LINE * CIA-MVO.                    ELSRESEL
00494      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSRESEL
00495      SET CIA-STG-GETMAIN TO TRUE.                                 ELSRESEL
00496      PERFORM CALL-THE-STORAGE-MANAGER.                            ELSRESEL
00497      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS                    ELSRESEL
00498          TO IOP-REC-PTR.                                          ELSRESEL
00499                                                                   ELSRESEL
00500                                                                   ELSRESEL
00501 ************************************************************      ELSRESEL
00502 *                                                          *      ELSRESEL
00503 *        CALL THE STORAGE MANAGER                          *      ELSRESEL
00504 *                                                          *      ELSRESEL
00505 ************************************************************      ELSRESEL
00506  CALL-THE-STORAGE-MANAGER.                                        ELSRESEL
00507      EXEC CICS LINK                                               ELSRESEL
00508                PROGRAM('ELUSTGMG')                                ELSRESEL
00509                COMMAREA(DFHCOMMAREA)                              ELSRESEL
00510                END-EXEC.                                          ELSRESEL
00511                                                                   ELSRESEL
00512                                                                   ELSRESEL
00513 ************************************************************      ELSRESEL
00514 *                                                          *      ELSRESEL
00515 *        DISPLAY GROUP SECTION OPTION                      *      ELSRESEL
00516 *                                                          *      ELSRESEL
00517 ************************************************************      ELSRESEL
00518  DISPLAY-GROUP-SECTION-OPTION.                                    ELSRESEL
00519      SET SSB-SS-GET-GROUP TO TRUE.                                ELSRESEL
00520      SET WS-MENU-IDX TO 1.                                        ELSRESEL
00521      PERFORM ADD-AN-ITEM-TO-MENU.                                 ELSRESEL
00522 /***********************************************************      ELSRESEL
00523 *                                                          *      ELSRESEL
00524 *        DISPLAY SECTION OPTION                            *      ELSRESEL
00525 *                                                          *      ELSRESEL
00526 ************************************************************      ELSRESEL
00527  DISPLAY-SECTION-OPTION.                                          ELSRESEL
00528      SET SSB-SS-GET-SECTION TO TRUE.                              ELSRESEL
00529      SET WS-MENU-IDX TO 2.                                        ELSRESEL
00530      PERFORM ADD-AN-ITEM-TO-MENU.                                 ELSRESEL
00531                                                                   ELSRESEL
00532                                                                   ELSRESEL
00533 ************************************************************      ELSRESEL
00534 *                                                          *      ELSRESEL
00535 *        ADD LINES TO MENU                                 *      ELSRESEL
00536 *                                                          *      ELSRESEL
00537 ************************************************************      ELSRESEL
00538  ADD-LINES-TO-MENU.                                               ELSRESEL
00539      EVALUATE TRUE                                                ELSRESEL
00540          WHEN WS-MENU-TOPIC (WS-MENU-IDX)                         ELSRESEL
00541              SET SSB-SS-GET-TOPIC TO TRUE                         ELSRESEL
00542          WHEN WS-MENU-SUBTOPIC (WS-MENU-IDX)                      ELSRESEL
00543              SET SSB-SS-GET-SUBTOPIC TO TRUE                      ELSRESEL
00544              IF SSB-SEL-DATA-AVAIL (SSB-SELECTOR-STATE)           ELSRESEL
00545                  MOVE SSB-TOPIC-PHRASE TO WS-MENU-SUB-TOPIC       ELSRESEL
00546              END-IF                                               ELSRESEL
00547          WHEN WS-MENU-PROVIDER-CLASS (WS-MENU-IDX)                ELSRESEL
00548              SET SSB-SS-GET-PROVIDER-CLASS TO TRUE                ELSRESEL
00549          WHEN WS-MENU-MEDCA-ELIG (WS-MENU-IDX)                    ELSRESEL
00550              SET SSB-SS-GET-MEDCA-ELIG TO TRUE                    ELSRESEL
00551          WHEN WS-MENU-FAM-REL (WS-MENU-IDX)                       ELSRESEL
00552              SET SSB-SS-GET-FAM-REL TO TRUE                       ELSRESEL
00553          WHEN WS-MENU-PT-AGE (WS-MENU-IDX)                        ELSRESEL
00554              SET SSB-SS-GET-PT-AGE TO TRUE                        ELSRESEL
00555          WHEN WS-MENU-GRP-SPEC-EFF-DATE (WS-MENU-IDX)             ELSRESEL
00556              SET SSB-SS-GET-GRP-SPEC-EFF-DATE TO TRUE             ELSRESEL
00557          WHEN WS-MENU-PROV-CTL-BAS-INST (WS-MENU-IDX)             ELSRESEL
00558              SET SSB-SS-GET-PROV-CTL-BAS-INST TO TRUE             ELSRESEL
00559          WHEN WS-MENU-PROV-CTL-SUP-INST (WS-MENU-IDX)             ELSRESEL
00560              SET SSB-SS-GET-PROV-CTL-SUP-INST TO TRUE             ELSRESEL
00561          WHEN WS-MENU-PROV-CTL-BAS-PROF (WS-MENU-IDX)             ELSRESEL
00562              SET SSB-SS-GET-PROV-CTL-BAS-PROF TO TRUE             ELSRESEL
00563          WHEN WS-MENU-PROV-CTL-SUP-PROF (WS-MENU-IDX)             ELSRESEL
00564              SET SSB-SS-GET-PROV-CTL-SUP-PROF TO TRUE             ELSRESEL
00565          WHEN WS-MENU-CONT-EFF-BAS-INST (WS-MENU-IDX)             ELSRESEL
00566              SET SSB-SS-GET-CONT-EFF-INST-BAS TO TRUE             ELSRESEL
00567          WHEN WS-MENU-CONT-EFF-SUP-INST (WS-MENU-IDX)             ELSRESEL
00568              SET SSB-SS-GET-CONT-EFF-INST-SUP TO TRUE             ELSRESEL
00569          WHEN WS-MENU-CONT-EFF-BAS-PROF (WS-MENU-IDX)             ELSRESEL
00570              SET SSB-SS-GET-CONT-EFF-PROF-BAS TO TRUE             ELSRESEL
00571          WHEN WS-MENU-CONT-EFF-SUP-PROF (WS-MENU-IDX)             ELSRESEL
00572              SET SSB-SS-GET-CONT-EFF-PROF-SUP TO TRUE             ELSRESEL
00573          WHEN WS-MENU-SERVICE-LOC (WS-MENU-IDX)                   ELSRESEL
00574              SET SSB-SS-GET-SERVICE-LOC TO TRUE                   ELSRESEL
00575          WHEN WS-MENU-MODIFIER-1 (WS-MENU-IDX)                    ELSRESEL
00576              MOVE SSB-TOPIC-PHRASE TO WS-MENU-MOD-1               ELSRESEL
00577              SET SSB-SS-GET-MODIFIER-1 TO TRUE                    ELSRESEL
00578          WHEN WS-MENU-MODIFIER-2 (WS-MENU-IDX)                    ELSRESEL
00579              MOVE SSB-MODIFIER-2-PHRASE TO WS-MENU-MOD-2          ELSRESEL
00580              SET SSB-SS-GET-MODIFIER-2 TO TRUE                    ELSRESEL
00581          WHEN OTHER                                               ELSRESEL
00582              PERFORM PROGRAM-LOGIC-ERROR                          ELSRESEL
00583      END-EVALUATE.                                                ELSRESEL
00584                                                                   ELSRESEL
00585      IF SSB-SEL-DATA-AVAIL (SSB-SELECTOR-STATE)                   ELSRESEL
00586          PERFORM ADD-AN-ITEM-TO-MENU.                             ELSRESEL
00587 /***********************************************************      ELSRESEL
00588 *                                                          *      ELSRESEL
00589 *        ADD AN ITEM TO MENU                               *      ELSRESEL
00590 *                                                          *      ELSRESEL
00591 ************************************************************      ELSRESEL
00592  ADD-AN-ITEM-TO-MENU.                                             ELSRESEL
00593      PERFORM ADD-OPTION-TO-VALID-LIST.                            ELSRESEL
00594      PERFORM FORMAT-MENU-LINE.                                    ELSRESEL
00595      PERFORM WRITE-LINE-TO-MENU-FILE.                             ELSRESEL
00596                                                                   ELSRESEL
00597                                                                   ELSRESEL
00598 ************************************************************      ELSRESEL
00599 *                                                          *      ELSRESEL
00600 *        ADD OPTION TO VALID LIST                          *      ELSRESEL
00601 *                                                          *      ELSRESEL
00602 ************************************************************      ELSRESEL
00603  ADD-OPTION-TO-VALID-LIST.                                        ELSRESEL
00604      ADD 1 TO MSO-NBR-MENU-OPTS.                                  ELSRESEL
00605      SET MSO-IDX TO MSO-NBR-MENU-OPTS.                            ELSRESEL
00606      MOVE SPACES         TO MSO-MENU-OPT (MSO-IDX).               ELSRESEL
00607      PERFORM ADD-AVAILABLE-OPTION.                                ELSRESEL
00608                                                                   ELSRESEL
00609                                                                   ELSRESEL
00610 ************************************************************      ELSRESEL
00611 *                                                          *      ELSRESEL
00612 *        ADD AVAILABLE OPTION                              *      ELSRESEL
00613 *                                                          *      ELSRESEL
00614 ************************************************************      ELSRESEL
00615  ADD-AVAILABLE-OPTION.                                            ELSRESEL
00616      MOVE WS-MENU-CHOICE (WS-MENU-IDX)                            ELSRESEL
00617           TO MSO-OPT-NUM-2 (MSO-IDX).                             ELSRESEL
00618      MOVE MSO-OPT-SEL (MSO-IDX)                                   ELSRESEL
00619           TO MSO-OPT-KWD (MSO-IDX).                               ELSRESEL
00620                                                                   ELSRESEL
00621                                                                   ELSRESEL
00622 ************************************************************      ELSRESEL
00623 *                                                          *      ELSRESEL
00624 *        FORMAT MENU LINE                                  *      ELSRESEL
00625 *                                                          *      ELSRESEL
00626 ************************************************************      ELSRESEL
00627  FORMAT-MENU-LINE.                                                ELSRESEL
00628      MOVE WS-MENU-CHOICE (WS-MENU-IDX)                            ELSRESEL
00629          TO WS-FL-CHOICE.                                         ELSRESEL
00630      MOVE WS-MENU-TEXT   (WS-MENU-IDX)                            ELSRESEL
00631          TO WS-FL-TEXT.                                           ELSRESEL
00632 /***********************************************************      ELSRESEL
00633 *                                                          *      ELSRESEL
00634 *        WRITE LINE TO MENU FILE                           *      ELSRESEL
00635 *                                                          *      ELSRESEL
00636 ************************************************************      ELSRESEL
00637  WRITE-LINE-TO-MENU-FILE.                                         ELSRESEL
00638      MOVE 1 TO MSD-NBR-DESCR-LINES.                               ELSRESEL
00639      MOVE WS-FORMATTED-LINE TO MSD-DESCR-LINE (1).                ELSRESEL
00640      SET CIA-ELSMENU-DDN    TO TRUE.                              ELSRESEL
00641      SET IOP-ADD TO TRUE.                                         ELSRESEL
00642      SET IOP-FCQ-NONE TO TRUE.                                    ELSRESEL
00643      EXEC CICS LINK                                               ELSRESEL
00644                PROGRAM('ELUIOPGM')                                ELSRESEL
00645                COMMAREA(DFHCOMMAREA)                              ELSRESEL
00646                END-EXEC.                                          ELSRESEL
00647                                                                   ELSRESEL
00648                                                                   ELSRESEL
00649 ************************************************************      ELSRESEL
00650 *                                                          *      ELSRESEL
00651 *        BEGIN MENU MODE                                   *      ELSRESEL
00652 *                                                          *      ELSRESEL
00653 ************************************************************      ELSRESEL
00654  BEGIN-MENU-MODE.                                                 ELSRESEL
00655      PERFORM CREATE-THE-MENU-HEADINGS.                            ELSRESEL
00656      MOVE SPACES TO SSB-MNU-CHOICE-TABLE.                         ELSRESEL
00657      MOVE 1                 TO MSO-MIN-CHOICES.                   ELSRESEL
00658      MOVE MSO-NBR-MENU-OPTS TO MSO-MAX-CHOICES.                   ELSRESEL
00659      SET SSB-SS-GET-RESELECT TO TRUE.                             ELSRESEL
00660      SET SSB-START-MENU (SSB-SELECTOR-STATE)                      ELSRESEL
00661          TO TRUE.                                                 ELSRESEL
00662      MOVE SPACE             TO SSB-PUSH-INDICATOR.                ELSRESEL
00663                                                                   ELSRESEL
00664                                                                   ELSRESEL
00665 ************************************************************      ELSRESEL
00666 *                                                          *      ELSRESEL
00667 *        CREATE THE MENU HEADINGS                          *      ELSRESEL
00668 *                                                          *      ELSRESEL
00669 ************************************************************      ELSRESEL
00670  CREATE-THE-MENU-HEADINGS.                                        ELSRESEL
00671      MOVE WS-NUM-HEADING-LINES TO MHD-NBR-HDG-LINES.              ELSRESEL
00672      MOVE WS-HEAD-LINE            TO MHD-HDG-LINE (1).            ELSRESEL
00673      MOVE 'RESELECTION'           TO SSB-MNU-TITLE.               ELSRESEL
00674 /***********************************************************      ELSRESEL
00675 *                                                          *      ELSRESEL
00676 *        PROCESS RESPONSE                                  *      ELSRESEL
00677 *                                                          *      ELSRESEL
00678 ************************************************************      ELSRESEL
00679  PROCESS-RESPONSE.                                                ELSRESEL
00680      PERFORM PROCESS-EACH-CHOICE                                  ELSRESEL
00681          VARYING SSB-MNU-IDX FROM 1 BY 1                          ELSRESEL
00682                     UNTIL SSB-MNU-IDX >                           ELSRESEL
00683              SSB-MNU-NUM-CHOICES.                                 ELSRESEL
00684      PERFORM SET-EXIT-STATUS.                                     ELSRESEL
00685                                                                   ELSRESEL
00686                                                                   ELSRESEL
00687 ************************************************************      ELSRESEL
00688 *                                                          *      ELSRESEL
00689 *        PROCESS EACH CHOICE                               *      ELSRESEL
00690 *                                                          *      ELSRESEL
00691 ************************************************************      ELSRESEL
00692  PROCESS-EACH-CHOICE.                                             ELSRESEL
00693      MOVE SSB-MNU-CHOICE (SSB-MNU-IDX) TO                         ELSRESEL
00694          WS-RESPONSE-BREAKDOWN.                                   ELSRESEL
00695      SET WS-MENU-IDX TO WS-RESPONSE.                              ELSRESEL
00696      EVALUATE TRUE                                                ELSRESEL
00697          WHEN WS-MENU-GROUP (WS-MENU-IDX)                         ELSRESEL
00698              PERFORM RESELECT-GROUP                               ELSRESEL
00699          WHEN WS-MENU-SECTION (WS-MENU-IDX)                       ELSRESEL
00700              PERFORM RESELECT-SECTION                             ELSRESEL
00701          WHEN WS-MENU-TOPIC (WS-MENU-IDX)                         ELSRESEL
00702              PERFORM RESELECT-TOPIC                               ELSRESEL
00703          WHEN WS-MENU-SUBTOPIC (WS-MENU-IDX)                      ELSRESEL
00704              PERFORM RESELECT-SUB-TOPIC                           ELSRESEL
00705          WHEN WS-MENU-PROVIDER-CLASS (WS-MENU-IDX)                ELSRESEL
00706              PERFORM RESELECT-PROVIDER-CLASS                      ELSRESEL
00707          WHEN WS-MENU-MEDCA-ELIG (WS-MENU-IDX)                    ELSRESEL
00708              PERFORM RESELECT-MEDICARE                            ELSRESEL
00709          WHEN WS-MENU-FAM-REL (WS-MENU-IDX)                       ELSRESEL
00710              PERFORM RESELECT-FAMILY-REL                          ELSRESEL
00711          WHEN WS-MENU-PT-AGE (WS-MENU-IDX)                        ELSRESEL
00712              PERFORM RESELECT-PATIENT-AGE                         ELSRESEL
00713          WHEN WS-MENU-GRP-SPEC-EFF-DATE (WS-MENU-IDX)             ELSRESEL
00714              PERFORM RESELECT-GROUP-SPEC-EFF-DATE                 ELSRESEL
00715          WHEN WS-MENU-PROV-CTL-BAS-INST (WS-MENU-IDX)             ELSRESEL
00716              PERFORM RESELECT-PROVIDER-CTL-BAS-INST               ELSRESEL
00717          WHEN WS-MENU-PROV-CTL-SUP-INST (WS-MENU-IDX)             ELSRESEL
00718              PERFORM RESELECT-PROVIDER-CTL-SUP-INST               ELSRESEL
00719          WHEN WS-MENU-PROV-CTL-BAS-PROF (WS-MENU-IDX)             ELSRESEL
00720              PERFORM RESELECT-PROVIDER-CTL-BAS-PROF               ELSRESEL
00721          WHEN WS-MENU-PROV-CTL-SUP-PROF (WS-MENU-IDX)             ELSRESEL
00722              PERFORM RESELECT-PROVIDER-CTL-SUP-PROF               ELSRESEL
00723          WHEN WS-MENU-CONT-EFF-BAS-INST (WS-MENU-IDX)             ELSRESEL
00724              PERFORM RESELECT-CONT-EFF-DATE-BAS-INS               ELSRESEL
00725          WHEN WS-MENU-CONT-EFF-SUP-INST (WS-MENU-IDX)             ELSRESEL
00726              PERFORM RESELECT-CONT-EFF-DATE-SUP-INS               ELSRESEL
00727          WHEN WS-MENU-CONT-EFF-BAS-PROF (WS-MENU-IDX)             ELSRESEL
00728              PERFORM RESELECT-CONT-EFF-DATE-BAS-PRO               ELSRESEL
00729          WHEN WS-MENU-CONT-EFF-SUP-PROF (WS-MENU-IDX)             ELSRESEL
00730              PERFORM RESELECT-CONT-EFF-DATE-SUP-PRO               ELSRESEL
00731          WHEN WS-MENU-SERVICE-LOC     (WS-MENU-IDX)               ELSRESEL
00732              PERFORM RESELECT-SERVICE-LOCATION                    ELSRESEL
00733          WHEN WS-MENU-MODIFIER-1     (WS-MENU-IDX)                ELSRESEL
00734              PERFORM RESELECT-MODIFIER-1                          ELSRESEL
00735          WHEN WS-MENU-MODIFIER-2     (WS-MENU-IDX)                ELSRESEL
00736              PERFORM RESELECT-MODIFIER-2                          ELSRESEL
00737          WHEN OTHER                                               ELSRESEL
00738              PERFORM PROGRAM-LOGIC-ERROR                          ELSRESEL
00739      END-EVALUATE.                                                ELSRESEL
00740 /***********************************************************      ELSRESEL
00741 *                                                          *      ELSRESEL
00742 *        SET EXIT STATUS                                   *      ELSRESEL
00743 *                                                          *      ELSRESEL
00744 ************************************************************      ELSRESEL
00745  SET-EXIT-STATUS.                                                 ELSRESEL
00746      SET SSB-SS-GET-RESELECT TO TRUE.                             ELSRESEL
00747      SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.              ELSRESEL
00748      SET SSB-SS-GET-GROUP TO TRUE.                                ELSRESEL
00749                                                                   ELSRESEL
00750                                                                   ELSRESEL
00751 ************************************************************      ELSRESEL
00752 *                                                          *      ELSRESEL
00753 *        RESELECT GROUP                                    *      ELSRESEL
00754 *                                                          *      ELSRESEL
00755 ************************************************************      ELSRESEL
00756  RESELECT-GROUP.                                                  ELSRESEL
00757      PERFORM INITIALIZE-GROUP-VARIABLES.                          ELSRESEL
00758      PERFORM INITIALIZE-CONTROL-VALUES.                           ELSRESEL
00759 /***********************************************************      ELSRESEL
00760 *                                                          *      ELSRESEL
00761 *        INITIALIZE GROUP VARIABLES                        *      ELSRESEL
00762 *                                                          *      ELSRESEL
00763 ************************************************************      ELSRESEL
00764  INITIALIZE-GROUP-VARIABLES.                                      ELSRESEL
00765      MOVE LOW-VALUES        TO SSB-GROUP-NUMBER                   ELSRESEL
00766                                SSB-SECTN-NO                       ELSRESEL
00767                                SSB-INST-BAS-CONTRACT              ELSRESEL
00768                                SSB-INST-SUP-CONTRACT              ELSRESEL
00769                                SSB-PROF-BAS-CONTRACT              ELSRESEL
00770                                SSB-PROF-SUP-CONTRACT              ELSRESEL
00771                                SSB-SERVICE-DATES                  ELSRESEL
00772                                SSB-SUBSCRIBER-KEYS                ELSRESEL
00773                                SSB-SUBSCRIBER-KEYS                ELSRESEL
00774                                SSB-TOPIC-SELECTIONS               ELSRESEL
00775                                SSB-SUB-TOPIC                      ELSRESEL
00776                                SSB-PROVIDER-CLASS                 ELSRESEL
00777                                SSB-MEDCA-ELIGY                    ELSRESEL
00778                                SSB-FAM-REL                        ELSRESEL
00779                                SSB-PT-AGE                         ELSRESEL
00780                                SSB-GRP-SPECIF-DATA                ELSRESEL
00781                                SSB-INST-BAS-PROVDR-CONTROL        ELSRESEL
00782                                SSB-INST-SUP-PROVDR-CONTROL        ELSRESEL
00783                                SSB-PROF-BAS-PROVDR-CONTROL        ELSRESEL
00784                                SSB-PROF-SUP-PROVDR-CONTROL        ELSRESEL
00785                                SSB-SERVICE-CLASS                  ELSRESEL
00786                                SSB-MODIFIER-1                     ELSRESEL
00787                                SSB-MODIFIER-2.                    ELSRESEL
00788      MOVE ZEROS             TO SSB-INST-BAS-EFF-DT-CEN            ELSRESEL
00789                                SSB-INST-BAS-TERMN-DT-CEN          ELSRESEL
00790                                SSB-INST-SUP-EFF-DT-CEN            ELSRESEL
00791                                SSB-INST-SUP-TERMN-DT-CEN          ELSRESEL
00792                                SSB-PROF-BAS-EFF-DT-CEN            ELSRESEL
00793                                SSB-PROF-BAS-TERM-DT-CEN           ELSRESEL
00794                                SSB-PROF-SUP-EFF-DATE-CC           ELSRESEL
00795                                SSB-PROF-SUP-TERMIN-DATE-CC.       ELSRESEL
00796 /***********************************************************      ELSRESEL
00797 *                                                          *      ELSRESEL
00798 *        INITIALIZE CONTROL VALUES                         *      ELSRESEL
00799 *                                                          *      ELSRESEL
00800 ************************************************************      ELSRESEL
00801  INITIALIZE-CONTROL-VALUES.                                       ELSRESEL
00802      MOVE SPACES  TO  SSB-ACTION-MODULE.                          ELSRESEL
00803      MOVE ALL '0' TO  SSB-MODULE-STATUS-TABLE.                    ELSRESEL
00804      SET SSB-SS-GET-RESELECT TO TRUE.                             ELSRESEL
00805      SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.              ELSRESEL
00806                                                                   ELSRESEL
00807                                                                   ELSRESEL
00808 ************************************************************      ELSRESEL
00809 *                                                          *      ELSRESEL
00810 *        RESELECT SECTION                                  *      ELSRESEL
00811 *                                                          *      ELSRESEL
00812 ************************************************************      ELSRESEL
00813  RESELECT-SECTION.                                                ELSRESEL
00814      MOVE LOW-VALUES        TO SSB-SECTN-NO.                      ELSRESEL
00815      SET SSB-SS-GET-SECTION TO TRUE.                              ELSRESEL
00816      SET SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                    ELSRESEL
00817           TO TRUE.                                                ELSRESEL
00818      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00819                                                                   ELSRESEL
00820                                                                   ELSRESEL
00821 ************************************************************      ELSRESEL
00822 *                                                          *      ELSRESEL
00823 *        RESELECT TOPIC                                    *      ELSRESEL
00824 *                                                          *      ELSRESEL
00825 ************************************************************      ELSRESEL
00826  RESELECT-TOPIC.                                                  ELSRESEL
00827      SET SSB-SS-GET-TOPIC TO TRUE.                                ELSRESEL
00828      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00829      PERFORM FORCE-RESELECTION-OF-SUB-TOPIC.                      ELSRESEL
00830      EJECT                                                        ELSRESEL
00831                                                                   ELSRESEL
00832                                                                   ELSRESEL
00833 ************************************************************      ELSRESEL
00834 *                                                          *      ELSRESEL
00835 *        FORCE RESELECTION OF SUB TOPICS                   *      ELSRESEL
00836 *                                                          *      ELSRESEL
00837 ************************************************************      ELSRESEL
00838  FORCE-RESELECTION-OF-SUB-TOPIC.                                  ELSRESEL
00839      SET SSB-SS-GET-SUBTOPIC TO TRUE.                             ELSRESEL
00840      SET SSB-INITIAL-CALL (SSB-SELECTOR-STATE) TO TRUE.           ELSRESEL
00841      SET SSB-SS-GET-MODIFIER-1 TO TRUE.                           ELSRESEL
00842      SET SSB-INITIAL-CALL (SSB-SELECTOR-STATE) TO TRUE.           ELSRESEL
00843 /***********************************************************      ELSRESEL
00844 *                                                          *      ELSRESEL
00845 *        RESELECT SUB TOPIC                                *      ELSRESEL
00846 *                                                          *      ELSRESEL
00847 ************************************************************      ELSRESEL
00848  RESELECT-SUB-TOPIC.                                              ELSRESEL
00849      SET SSB-SS-GET-SUBTOPIC TO TRUE.                             ELSRESEL
00850      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00851                                                                   ELSRESEL
00852                                                                   ELSRESEL
00853 ************************************************************      ELSRESEL
00854 *                                                          *      ELSRESEL
00855 *        RESELECT PROVIDER CLASS                           *      ELSRESEL
00856 *                                                          *      ELSRESEL
00857 ************************************************************      ELSRESEL
00858  RESELECT-PROVIDER-CLASS.                                         ELSRESEL
00859      SET SSB-SS-GET-PROVIDER-CLASS TO TRUE.                       ELSRESEL
00860      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00861                                                                   ELSRESEL
00862                                                                   ELSRESEL
00863 ************************************************************      ELSRESEL
00864 *                                                          *      ELSRESEL
00865 *        RESELECT MEDICARE                                 *      ELSRESEL
00866 *                                                          *      ELSRESEL
00867 ************************************************************      ELSRESEL
00868  RESELECT-MEDICARE.                                               ELSRESEL
00869      SET SSB-SS-GET-MEDCA-ELIG TO TRUE.                           ELSRESEL
00870      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00871                                                                   ELSRESEL
00872                                                                   ELSRESEL
00873 ************************************************************      ELSRESEL
00874 *                                                          *      ELSRESEL
00875 *        RESELECT FAMILY REL                               *      ELSRESEL
00876 *                                                          *      ELSRESEL
00877 ************************************************************      ELSRESEL
00878  RESELECT-FAMILY-REL.                                             ELSRESEL
00879      SET SSB-SS-GET-FAM-REL TO TRUE.                              ELSRESEL
00880      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00881                                                                   ELSRESEL
00882                                                                   ELSRESEL
00883 ************************************************************      ELSRESEL
00884 *                                                          *      ELSRESEL
00885 *        RESELECT PATIENT AGE                              *      ELSRESEL
00886 *                                                          *      ELSRESEL
00887 ************************************************************      ELSRESEL
00888  RESELECT-PATIENT-AGE.                                            ELSRESEL
00889      SET SSB-SS-GET-PT-AGE TO TRUE.                               ELSRESEL
00890      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00891 /***********************************************************      ELSRESEL
00892 *                                                          *      ELSRESEL
00893 *        RESELECT GROUP SPEC EFF DATE                      *      ELSRESEL
00894 *                                                          *      ELSRESEL
00895 ************************************************************      ELSRESEL
00896  RESELECT-GROUP-SPEC-EFF-DATE.                                    ELSRESEL
00897      SET SSB-SS-GET-GRP-SPEC-EFF-DATE TO TRUE.                    ELSRESEL
00898      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00899                                                                   ELSRESEL
00900                                                                   ELSRESEL
00901 ************************************************************      ELSRESEL
00902 *                                                          *      ELSRESEL
00903 *        RESELECT PROVIDER CTL BAS INST                    *      ELSRESEL
00904 *                                                          *      ELSRESEL
00905 ************************************************************      ELSRESEL
00906  RESELECT-PROVIDER-CTL-BAS-INST.                                  ELSRESEL
00907      SET SSB-SS-GET-PROV-CTL-BAS-INST TO TRUE.                    ELSRESEL
00908      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00909                                                                   ELSRESEL
00910                                                                   ELSRESEL
00911 ************************************************************      ELSRESEL
00912 *                                                          *      ELSRESEL
00913 *        RESELECT PROVIDER CTL SUP INST                    *      ELSRESEL
00914 *                                                          *      ELSRESEL
00915 ************************************************************      ELSRESEL
00916  RESELECT-PROVIDER-CTL-SUP-INST.                                  ELSRESEL
00917      SET SSB-SS-GET-PROV-CTL-BAS-INST TO TRUE.                    ELSRESEL
00918      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00919                                                                   ELSRESEL
00920                                                                   ELSRESEL
00921 ************************************************************      ELSRESEL
00922 *                                                          *      ELSRESEL
00923 *        RESELECT PROVIDER CTL BAS PROF                    *      ELSRESEL
00924 *                                                          *      ELSRESEL
00925 ************************************************************      ELSRESEL
00926  RESELECT-PROVIDER-CTL-BAS-PROF.                                  ELSRESEL
00927      SET SSB-SS-GET-PROV-CTL-BAS-INST TO TRUE.                    ELSRESEL
00928      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00929                                                                   ELSRESEL
00930                                                                   ELSRESEL
00931 ************************************************************      ELSRESEL
00932 *                                                          *      ELSRESEL
00933 *        RESELECT PROVIDER CTL SUP PROF                    *      ELSRESEL
00934 *                                                          *      ELSRESEL
00935 ************************************************************      ELSRESEL
00936  RESELECT-PROVIDER-CTL-SUP-PROF.                                  ELSRESEL
00937      SET SSB-SS-GET-PROV-CTL-BAS-INST TO TRUE.                    ELSRESEL
00938      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00939 /***********************************************************      ELSRESEL
00940 *                                                          *      ELSRESEL
00941 *        RESELECT CONT EFF DATE BAS INST                   *      ELSRESEL
00942 *                                                          *      ELSRESEL
00943 ************************************************************      ELSRESEL
00944  RESELECT-CONT-EFF-DATE-BAS-INS.                                  ELSRESEL
00945      SET SSB-SS-GET-CONT-EFF-INST-BAS TO TRUE.                    ELSRESEL
00946      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00947                                                                   ELSRESEL
00948                                                                   ELSRESEL
00949 ************************************************************      ELSRESEL
00950 *                                                          *      ELSRESEL
00951 *        RESELECT CONT EFF DATE SUP INST                   *      ELSRESEL
00952 *                                                          *      ELSRESEL
00953 ************************************************************      ELSRESEL
00954  RESELECT-CONT-EFF-DATE-SUP-INS.                                  ELSRESEL
00955      SET SSB-SS-GET-CONT-EFF-INST-BAS TO TRUE.                    ELSRESEL
00956      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00957                                                                   ELSRESEL
00958                                                                   ELSRESEL
00959 ************************************************************      ELSRESEL
00960 *                                                          *      ELSRESEL
00961 *        RESELECT CONT EFF DATE BAS PROF                   *      ELSRESEL
00962 *                                                          *      ELSRESEL
00963 ************************************************************      ELSRESEL
00964  RESELECT-CONT-EFF-DATE-BAS-PRO.                                  ELSRESEL
00965      SET SSB-SS-GET-CONT-EFF-INST-BAS TO TRUE.                    ELSRESEL
00966      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00967                                                                   ELSRESEL
00968                                                                   ELSRESEL
00969 ************************************************************      ELSRESEL
00970 *                                                          *      ELSRESEL
00971 *        RESELECT CONT EFF DATE SUP PROF                   *      ELSRESEL
00972 *                                                          *      ELSRESEL
00973 ************************************************************      ELSRESEL
00974  RESELECT-CONT-EFF-DATE-SUP-PRO.                                  ELSRESEL
00975      SET SSB-SS-GET-CONT-EFF-INST-BAS TO TRUE.                    ELSRESEL
00976      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00977                                                                   ELSRESEL
00978                                                                   ELSRESEL
00979 ************************************************************      ELSRESEL
00980 *                                                          *      ELSRESEL
00981 *        RESELECT SERVICE LOCATION                         *      ELSRESEL
00982 *                                                          *      ELSRESEL
00983 ************************************************************      ELSRESEL
00984  RESELECT-SERVICE-LOCATION.                                       ELSRESEL
00985      SET SSB-SS-GET-SERVICE-LOC TO TRUE.                          ELSRESEL
00986      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00987 /***********************************************************      ELSRESEL
00988 *                                                          *      ELSRESEL
00989 *        RESELECT MODIFIER 1                               *      ELSRESEL
00990 *                                                          *      ELSRESEL
00991 ************************************************************      ELSRESEL
00992  RESELECT-MODIFIER-1.                                             ELSRESEL
00993      SET SSB-SS-GET-MODIFIER-1 TO TRUE.                           ELSRESEL
00994      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
00995                                                                   ELSRESEL
00996                                                                   ELSRESEL
00997 ************************************************************      ELSRESEL
00998 *                                                          *      ELSRESEL
00999 *        RESELECT MODIFIER 2                               *      ELSRESEL
01000 *                                                          *      ELSRESEL
01001 ************************************************************      ELSRESEL
01002  RESELECT-MODIFIER-2.                                             ELSRESEL
01003      SET SSB-SS-GET-MODIFIER-2 TO TRUE.                           ELSRESEL
01004      PERFORM INITIALIZE-STATUS-INDICATORS.                        ELSRESEL
01005                                                                   ELSRESEL
01006                                                                   ELSRESEL
01007 ************************************************************      ELSRESEL
01008 *                                                          *      ELSRESEL
01009 *        INITIALIZE STATUS INDICATORS                      *      ELSRESEL
01010 *                                                          *      ELSRESEL
01011 ************************************************************      ELSRESEL
01012  INITIALIZE-STATUS-INDICATORS.                                    ELSRESEL
01013      PERFORM DETERMINE-CORRECT-STATUS                             ELSRESEL
01014          VARYING WS-WORK-SUB FROM 3 BY 1                          ELSRESEL
01015                     UNTIL WS-WORK-SUB > SSB-MAX-MODULES.          ELSRESEL
01016                                                                   ELSRESEL
01017                                                                   ELSRESEL
01018 ************************************************************      ELSRESEL
01019 *                                                          *      ELSRESEL
01020 *        DETERMINE CORRECT STATUS                          *      ELSRESEL
01021 *                                                          *      ELSRESEL
01022 ************************************************************      ELSRESEL
01023  DETERMINE-CORRECT-STATUS.                                        ELSRESEL
01024      IF (WS-WORK-SUB > SSB-SELECTOR-STATE                         ELSRESEL
01025                 AND SSB-NOT-USED (WS-WORK-SUB))                   ELSRESEL
01026               OR WS-WORK-SUB = SSB-SELECTOR-STATE                 ELSRESEL
01027               OR SSB-DATA-DERIVED   (WS-WORK-SUB)                 ELSRESEL
01028           SET SSB-INITIAL-CALL (WS-WORK-SUB) TO TRUE              ELSRESEL
01029      ELSE                                                         ELSRESEL
01030          IF SSB-SEL-DATA-AVAIL (WS-WORK-SUB)                      ELSRESEL
01031               OR SSB-DATA-KNOWN-NOT-AVAIL (WS-WORK-SUB)           ELSRESEL
01032              SET SSB-RESELECTION (WS-WORK-SUB) TO TRUE.           ELSRESEL
01033 /***********************************************************      ELSRESEL
01034 *                                                          *      ELSRESEL
01035 *        CALL STORAGE MANAGER                              *      ELSRESEL
01036 *                                                          *      ELSRESEL
01037 ************************************************************      ELSRESEL
01038  CALL-STORAGE-MANAGER.                                            ELSRESEL
01039      EXEC CICS LINK                                               ELSRESEL
01040                PROGRAM('ELUSTGMG')                                ELSRESEL
01041                COMMAREA(DFHCOMMAREA)                              ELSRESEL
01042                END-EXEC.                                          ELSRESEL
01043                                                                   ELSRESEL
01044                                                                   ELSRESEL
01045 ************************************************************      ELSRESEL
01046 *                                                          *      ELSRESEL
01047 *        INVALID COMMAREA ABEND                            *      ELSRESEL
01048 *                                                          *      ELSRESEL
01049 ************************************************************      ELSRESEL
01050  INVALID-COMMAREA-ABEND.                                          ELSRESEL
01051      EXEC CICS ABEND                                              ELSRESEL
01052                ABCODE('EL01')                                     ELSRESEL
01053                END-EXEC.                                          ELSRESEL
01054                                                                   ELSRESEL
01055                                                                   ELSRESEL
01056 ************************************************************      ELSRESEL
01057 *                                                          *      ELSRESEL
01058 *        PROGRAM LOGIC ERROR                               *      ELSRESEL
01059 *                                                          *      ELSRESEL
01060 ************************************************************      ELSRESEL
01061  PROGRAM-LOGIC-ERROR.                                             ELSRESEL
01062      SET CIA-AB-PGM-LOGIC TO TRUE.                                ELSRESEL
01063      EXEC CICS ABEND                                              ELSRESEL
01064                ABCODE(CIA-ABCODE)                                 ELSRESEL
01065                END-EXEC.                                          ELSRESEL
01066                                                                   ELSRESEL
01067                                                                   ELSRESEL
01068 ************************************************************      ELSRESEL
01069 *                                                          *      ELSRESEL
01070 *        SYSTEM LOGIC ERROR                                *      ELSRESEL
01071 *                                                          *      ELSRESEL
01072 ************************************************************      ELSRESEL
01073  SYSTEM-LOGIC-ERROR.                                              ELSRESEL
01074      SET CIA-AB-UNDEF TO TRUE.                                    ELSRESEL
01075      EXEC CICS ABEND                                              ELSRESEL
01076                ABCODE(CIA-ABCODE)                                 ELSRESEL
01077                END-EXEC.                                          ELSRESEL
