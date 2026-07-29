00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSSLPVE
00003  PROGRAM-ID.         ELSSLPVE.                                       LV002
00004                                                                   ELSSLPVE
00005  AUTHOR.             JOHN T. CURIN, KEANE,INC.                    ELSSLPVE
00006                                                                   ELSSLPVE
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSSLPVE
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSSLPVE
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSSLPVE
00010                      233 N. MICHIGAN AVE                          ELSSLPVE
00011                      CHICAGO, ILLINOIS 60601                      ELSSLPVE
00012                                                                   ELSSLPVE
00013  DATE-WRITTEN.       06-NOV-1986.                                 ELSSLPVE
00014                                                                   ELSSLPVE
00015  DATE-COMPILED.                                                   ELSSLPVE
00016                                                                   ELSSLPVE
00017  SECURITY.           COPYRIGHT 1986,                              ELSSLPVE
00018                      HEALTH CARE SERVICE CORPORATION              ELSSLPVE
00019      SKIP3                                                        ELSSLPVE
00020 ******************************************************************ELSSLPVE
00021 *                                                                *ELSSLPVE
00022 *    PROGRAM:    ELSSLPVE                                        *ELSSLPVE
00023 *    DATE:       06-NOV-1986                                     *ELSSLPVE
00024 *    AUTHOR:     JOHN CURIN                                      *ELSSLPVE
00025 *    FUNCTION:   PROVIDER ELEIGIBILITY SELECTOR PROGRAM          *ELSSLPVE
00026 *                                                                *ELSSLPVE
00027 *                                                                *ELSSLPVE
00028 ******************************************************************ELSSLPVE
00029 *                                                                *ELSSLPVE
00030 *                      MAINTENANCE HISTORY                       *ELSSLPVE
00031 *                                                                *ELSSLPVE
00032 *  MOD     DATE     BY  DRPT                ACTION               *ELSSLPVE
00033 * ----- ----------- --- ----- ---------------------------------- *ELSSLPVE
00034 * 01.00 06-NOV-1986 JTC       CREATED                            *ELSSLPVE
00035 *                                                                *ELSSLPVE
00036 * 01.01 22-SEP-1987 REB       ISSUE ABEND CODE OF 'EL01' FOR AN  *ELSSLPVE
00037 *                             INVALID COMMAREA AND 'EL02' IF THE *ELSSLPVE
00038 *                             CIA BLOCK IS NOT PRESENT.          *ELSSLPVE
00039 * 01.02 28-AUG-1989 EGL       DESTRUCTED AND CHANGED TO USE NEW  *ELSSLPVE
00040 *                             STORAGE MANAGEMENT.                *ELSSLPVE
00041 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST    *ELSSLPVE
00042 ******************************************************************ELSSLPVE
00043                                                                   ELSSLPVE
00044  ENVIRONMENT DIVISION.                                            ELSSLPVE
00045                                                                   ELSSLPVE
00046  CONFIGURATION SECTION.                                           ELSSLPVE
00047  SOURCE-COMPUTER.    IBM-3033.                                    ELSSLPVE
00048  OBJECT-COMPUTER.    IBM-3033.                                    ELSSLPVE
00049      EJECT                                                        ELSSLPVE
00050  DATA DIVISION.                                                   ELSSLPVE
00051                                                                   ELSSLPVE
00052  FILE SECTION.                                                    ELSSLPVE
00053                                                                   ELSSLPVE
00054  WORKING-STORAGE SECTION.                                         ELSSLPVE
00055                                                                   ELSSLPVE
00056 *** PROVIDER ELIGIBILITY WORK AREA                                ELSSLPVE
00057                                                                   ELSSLPVE
00058  01  WS-TABLE-COUNT.                                              ELSSLPVE
00059      05  WS-NUMBER-HEADINGS PIC S9(4) COMP     VALUE +8.          ELSSLPVE
00060      05  WS-MENU-LINE-COUNT PIC S9(4) COMP     VALUE +1.          ELSSLPVE
00061                                                                   ELSSLPVE
00062  01  WS-PROV-ELIG-TITLE     PIC X(44)            VALUE            ELSSLPVE
00063      'SELECT PROVIDER TO DISPLAY ELIGIBLE BENEFITS'.              ELSSLPVE
00064                                                                   ELSSLPVE
00065  01  WS-TWO-BYTE-CODE       PIC XX             VALUE SPACE.       ELSSLPVE
00066  01  WS-PROVIDER-TYPE-REDF REDEFINES WS-TWO-BYTE-CODE.            ELSSLPVE
00067      05  WS-FIRST-BYTE      PIC X.                                ELSSLPVE
00068          88 WS-INSTITUTIONAL-TYPE VALUES '0' THRU '9'.            ELSSLPVE
00069      05  WS-SECOND-BYTE     PIC X.                                ELSSLPVE
00070                                                                   ELSSLPVE
00071  01  WS-ELPCN-KEY.                                                ELSSLPVE
00072      05  WS-CN-RECORD-PREFIX   PIC X(08)       VALUE '#PVE    '.  ELSSLPVE
00073      05  WS-CN-COBOL-NAME      PIC X(30)       VALUE              ELSSLPVE
00074          'PROVIDER-CODE                 '.                        ELSSLPVE
00075                                                                   ELSSLPVE
00076  01  WS-PROV-ELIG-HEADINGS.                                       ELSSLPVE
00077      05  FILLER                PIC X(78)            VALUE         ELSSLPVE
00078      'THE PROVIDER ELIGIBILITY TOPIC LISTS BENEFITS FOR WHICH A PAELSSLPVE
00079 -    'RTICULAR PROVIDER '.                                        ELSSLPVE
00080      05  FILLER                PIC X(78)            VALUE         ELSSLPVE
00081      'TYPE IS ELIGIBLE.  SELECT THE PROVIDER TYPE YOU WISH TO SEE ELSSLPVE
00082 -    'FROM THE LIST    '.                                         ELSSLPVE
00083      05  FILLER                 PIC X(78)            VALUE        ELSSLPVE
00084      'BELOW AND PRESS <ENTER> TO SEE THE LIST OF BENEFITS FOR WHICELSSLPVE
00085 -    'H THAT PROVIDER  '.                                         ELSSLPVE
00086      05  FILLER                 PIC X(78)            VALUE        ELSSLPVE
00087      'TYPE IS ELIGIBLE.  PRESS <ENTER> WITHOUT SLECTING A PROVIDERELSSLPVE
00088 -    ' TYPE TO SEE MORE'.                                         ELSSLPVE
00089      05  FILLER                 PIC X(78)            VALUE        ELSSLPVE
00090      'PROVIDER TYPES THAT ARE AVAILABLE.                          ELSSLPVE
00091 -    '                 '.                                         ELSSLPVE
00092      05  FILLER                 PIC X(78)            VALUE        ELSSLPVE
00093      '                                                            ELSSLPVE
00094 -    '                 '.                                         ELSSLPVE
00095      05  FILLER                 PIC X(78)            VALUE        ELSSLPVE
00096      'TYPE CODE  PROVIDER TYPE                                    ELSSLPVE
00097 -    '                 '.                                         ELSSLPVE
00098      05  FILLER                 PIC X(78)            VALUE        ELSSLPVE
00099      '                                                            ELSSLPVE
00100 -    '                 '.                                         ELSSLPVE
00101  01  WS-PROV-ELIG-HEAD REDEFINES WS-PROV-ELIG-HEADINGS.           ELSSLPVE
00102      05  WS-PROV-ELIG-HEADING-LINE                                ELSSLPVE
00103                      OCCURS 8 TIMES                               ELSSLPVE
00104                      INDEXED BY WS-HEAD-INDEX                     ELSSLPVE
00105                          PIC X(78).                               ELSSLPVE
00106                                                                   ELSSLPVE
00107      EJECT                                                        ELSSLPVE
00108  LINKAGE SECTION.                                                 ELSSLPVE
00109                                                                   ELSSLPVE
00110  01  DFHCOMMAREA.                                                 ELSSLPVE
00111      COPY ELSCOMMC.                                               ELSSLPVE
00112      EJECT                                                        ELSSLPVE
00113      COPY ELSCIA2C.                                               ELSSLPVE
00114      EJECT                                                        ELSSLPVE
00115      COPY ELSIOPMC.                                               ELSSLPVE
00116      EJECT                                                        ELSSLPVE
00117      COPY ELSSSCBC.                                               ELSSLPVE
00118      EJECT                                                        ELSSLPVE
00119      COPY ELSMENUC.                                               ELSSLPVE
00120      EJECT                                                        ELSSLPVE
00121      COPY ELSMHDGC.                                               ELSSLPVE
00122      EJECT                                                        ELSSLPVE
00123      COPY ELSMOPTC.                                               ELSSLPVE
00124      EJECT                                                        ELSSLPVE
00125      COPY ELSKEYSC.                                               ELSSLPVE
00126      EJECT                                                        ELSSLPVE
00127  01  COBOL-NAME-AREA.                                             ELSSLPVE
00128      COPY ELPCNC.                                                 ELSSLPVE
00129  01  CODE-VALUE-AREA.                                             ELSSLPVE
00130      COPY ELPCVC.                                                 ELSSLPVE
00131      EJECT                                                        ELSSLPVE
00132  PROCEDURE DIVISION.                                              ELSSLPVE
00133 ************************************************************      ELSSLPVE
00134 *                                                          *      ELSSLPVE
00135 *        PROVIDER ELIGIBILITY SELECTOR                     *      ELSSLPVE
00136 *                                                          *      ELSSLPVE
00137 ************************************************************      ELSSLPVE
00138  PROVIDER-ELIGIBILITY-SELECTOR.                                   ELSSLPVE
00139      PERFORM INITIALIZE-MODULE.                                   ELSSLPVE
00140      PERFORM PROCESS-PVE.                                         ELSSLPVE
00141      PERFORM TERMINATE-MODULE.                                    ELSSLPVE
00142                                                                   ELSSLPVE
00143                                                                   ELSSLPVE
00144 ************************************************************      ELSSLPVE
00145 *                                                          *      ELSSLPVE
00146 *        INITIALIZE MODULE                                 *      ELSSLPVE
00147 *                                                          *      ELSSLPVE
00148 ************************************************************      ELSSLPVE
00149  INITIALIZE-MODULE.                                               ELSSLPVE
00150      PERFORM CHECK-COMMAREA-SIZE.                                 ELSSLPVE
00151      PERFORM SET-ADDR-OF-CIA.                                     ELSSLPVE
00152      PERFORM SET-ADDR-OF-SSCB.                                    ELSSLPVE
00153      PERFORM SET-ADDR-OF-KEY-WORK-AREA.                           ELSSLPVE
00154                                                                   ELSSLPVE
00155                                                                   ELSSLPVE
00156 ************************************************************      ELSSLPVE
00157 *                                                          *      ELSSLPVE
00158 *        CHECK COMMAREA SIZE                               *      ELSSLPVE
00159 *                                                          *      ELSSLPVE
00160 ************************************************************      ELSSLPVE
00161  CHECK-COMMAREA-SIZE.                                             ELSSLPVE
00162      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELSSLPVE
00163          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELSSLPVE
00164                                                                   ELSSLPVE
00165                                                                   ELSSLPVE
00166 ************************************************************      ELSSLPVE
00167 *                                                          *      ELSSLPVE
00168 *        SET ADDR OF CIA                                   *      ELSSLPVE
00169 *                                                          *      ELSSLPVE
00170 ************************************************************      ELSSLPVE
00171  SET-ADDR-OF-CIA.                                                 ELSSLPVE
00172      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSSLPVE
00173                 ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.         ELSSLPVE
00174      IF CIA-RC-PTR-NULL                                           ELSSLPVE
00175          PERFORM SIGNAL-CIA-ADDRESSING-ERROR.                     ELSSLPVE
00176 /***********************************************************      ELSSLPVE
00177 *                                                          *      ELSSLPVE
00178 *        SET ADDR OF SSCB                                  *      ELSSLPVE
00179 *                                                          *      ELSSLPVE
00180 ************************************************************      ELSSLPVE
00181  SET-ADDR-OF-SSCB.                                                ELSSLPVE
00182      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSSLPVE
00183      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLPVE
00184                 ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.           ELSSLPVE
00185      IF CIA-RC-PTR-NULL                                           ELSSLPVE
00186          PERFORM SIGNAL-MISSING-PARAMETER.                        ELSSLPVE
00187                                                                   ELSSLPVE
00188                                                                   ELSSLPVE
00189 ************************************************************      ELSSLPVE
00190 *                                                          *      ELSSLPVE
00191 *        SET ADDR OF KEY WORK AREA                         *      ELSSLPVE
00192 *                                                          *      ELSSLPVE
00193 ************************************************************      ELSSLPVE
00194  SET-ADDR-OF-KEY-WORK-AREA.                                       ELSSLPVE
00195      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELSSLPVE
00196      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLPVE
00197                 ADDRESS OF KWA-FILE-KEY-WORK-AREA.                ELSSLPVE
00198      IF ADDRESS OF KWA-FILE-KEY-WORK-AREA = NULL                  ELSSLPVE
00199          PERFORM ALLOCATE-KEY-WORK-AREA.                          ELSSLPVE
00200                                                                   ELSSLPVE
00201                                                                   ELSSLPVE
00202 ************************************************************      ELSSLPVE
00203 *                                                          *      ELSSLPVE
00204 *        ALLOCATE KEY WORK AREA                            *      ELSSLPVE
00205 *                                                          *      ELSSLPVE
00206 ************************************************************      ELSSLPVE
00207  ALLOCATE-KEY-WORK-AREA.                                          ELSSLPVE
00208      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELSSLPVE
00209      MOVE ZERO TO CIA-AREA-LEN.                                   ELSSLPVE
00210      PERFORM ACQUIRE-CONTROLLED-STORAGE.                          ELSSLPVE
00211      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELSSLPVE
00212      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLPVE
00213                 ADDRESS OF KWA-FILE-KEY-WORK-AREA.                ELSSLPVE
00214 /***********************************************************      ELSSLPVE
00215 *                                                          *      ELSSLPVE
00216 *        PROCESS PVE                                       *      ELSSLPVE
00217 *                                                          *      ELSSLPVE
00218 ************************************************************      ELSSLPVE
00219  PROCESS-PVE.                                                     ELSSLPVE
00220      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSSLPVE
00221          PERFORM PROCESS-BUILD-PVE-MENU-REQUEST                   ELSSLPVE
00222      ELSE IF SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)               ELSSLPVE
00223          PERFORM PROCESS-PVE-MENU-COMPLETED-REQ                   ELSSLPVE
00224      ELSE                                                         ELSSLPVE
00225          PERFORM SIGNAL-UNKNOWN-ERROR.                            ELSSLPVE
00226                                                                   ELSSLPVE
00227                                                                   ELSSLPVE
00228 ************************************************************      ELSSLPVE
00229 *                                                          *      ELSSLPVE
00230 *        PROCESS BUILD PVE MENU REQUEST                    *      ELSSLPVE
00231 *                                                          *      ELSSLPVE
00232 ************************************************************      ELSSLPVE
00233  PROCESS-BUILD-PVE-MENU-REQUEST.                                  ELSSLPVE
00234      INITIALIZE SSB-MNU-CHOICE (1).                               ELSSLPVE
00235      PERFORM DELETE-MENU-FILE.                                    ELSSLPVE
00236      PERFORM ACQUIRE-STORAGE-AREAS.                               ELSSLPVE
00237      PERFORM BUILD-MENU-HEADERS.                                  ELSSLPVE
00238      PERFORM BUILD-TOPIC-TITLES.                                  ELSSLPVE
00239      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSSLPVE
00240                                                                   ELSSLPVE
00241                                                                   ELSSLPVE
00242 ************************************************************      ELSSLPVE
00243 *                                                          *      ELSSLPVE
00244 *        DELETE MENU FILE                                  *      ELSSLPVE
00245 *                                                          *      ELSSLPVE
00246 ************************************************************      ELSSLPVE
00247  DELETE-MENU-FILE.                                                ELSSLPVE
00248      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSLPVE
00249      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLPVE
00250                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELSSLPVE
00251      IF ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS = NULL             ELSSLPVE
00252          PERFORM ALLOCATE-MENU-AREA.                              ELSSLPVE
00253      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSSLPVE
00254      SET IOP-DEL          TO TRUE.                                ELSSLPVE
00255      SET IOP-FCQ-NONE     TO TRUE.                                ELSSLPVE
00256      SET IOP-KVQ-NONE     TO TRUE.                                ELSSLPVE
00257      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSSLPVE
00258 /***********************************************************      ELSSLPVE
00259 *                                                          *      ELSSLPVE
00260 *        ACQUIRE STORAGE AREAS                             *      ELSSLPVE
00261 *                                                          *      ELSSLPVE
00262 ************************************************************      ELSSLPVE
00263  ACQUIRE-STORAGE-AREAS.                                           ELSSLPVE
00264      PERFORM GET-HEADING-STORAGE-AREA.                            ELSSLPVE
00265      PERFORM GET-SELECTION-CODE-KEYWORD-ARE.                      ELSSLPVE
00266      PERFORM GET-DESCRIPTION-LINE-AREA.                           ELSSLPVE
00267                                                                   ELSSLPVE
00268                                                                   ELSSLPVE
00269 ************************************************************      ELSSLPVE
00270 *                                                          *      ELSSLPVE
00271 *        GET HEADING STORAGE AREA                          *      ELSSLPVE
00272 *                                                          *      ELSSLPVE
00273 ************************************************************      ELSSLPVE
00274  GET-HEADING-STORAGE-AREA.                                        ELSSLPVE
00275      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSSLPVE
00276      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES           ELSSLPVE
00277          + (LENGTH OF MHD-HDG-LINE * WS-NUMBER-HEADINGS).         ELSSLPVE
00278      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSLPVE
00279      PERFORM CALL-STORAGE-MANAGEMENT-SUBPRO.                      ELSSLPVE
00280      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSSLPVE
00281      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLPVE
00282                 ADDRESS OF MHD-MENU-HEADINGS.                     ELSSLPVE
00283                                                                   ELSSLPVE
00284                                                                   ELSSLPVE
00285 ************************************************************      ELSSLPVE
00286 *                                                          *      ELSSLPVE
00287 *        GET SELECTION CODE KEYWORD AREA                   *      ELSSLPVE
00288 *                                                          *      ELSSLPVE
00289 ************************************************************      ELSSLPVE
00290  GET-SELECTION-CODE-KEYWORD-ARE.                                  ELSSLPVE
00291      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSSLPVE
00292      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLPVE
00293                 ADDRESS OF MSO-MENU-SELECTION-VALUES.             ELSSLPVE
00294      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +      ELSSLPVE
00295              (LENGTH OF MSO-MENU-OPT * CIA-MVO).                  ELSSLPVE
00296      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSLPVE
00297      PERFORM CALL-STORAGE-MANAGEMENT-SUBPRO.                      ELSSLPVE
00298      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSSLPVE
00299      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLPVE
00300                 ADDRESS OF MSO-MENU-SELECTION-VALUES.             ELSSLPVE
00301 /***********************************************************      ELSSLPVE
00302 *                                                          *      ELSSLPVE
00303 *        GET DESCRIPTION LINE AREA                         *      ELSSLPVE
00304 *                                                          *      ELSSLPVE
00305 ************************************************************      ELSSLPVE
00306  GET-DESCRIPTION-LINE-AREA.                                       ELSSLPVE
00307      PERFORM SET-IOPM-ADDRESS-FOR-MENU-FILE.                      ELSSLPVE
00308      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSLPVE
00309      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSLPVE
00310      SET IOP-GETMAIN-REC TO TRUE.                                 ELSSLPVE
00311      COMPUTE IOP-REC-LEN = LENGTH OF MSD-NBR-DESCR-LINES +        ELSSLPVE
00312             (LENGTH OF MSD-DESCR-LINE * WS-MENU-LINE-COUNT).      ELSSLPVE
00313      PERFORM CALL-STORAGE-MANAGEMENT-SUBPRO.                      ELSSLPVE
00314      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS TO IOP-REC-PTR.    ELSSLPVE
00315                                                                   ELSSLPVE
00316                                                                   ELSSLPVE
00317 ************************************************************      ELSSLPVE
00318 *                                                          *      ELSSLPVE
00319 *        ALLOCATE MENU AREA                                *      ELSSLPVE
00320 *                                                          *      ELSSLPVE
00321 ************************************************************      ELSSLPVE
00322  ALLOCATE-MENU-AREA.                                              ELSSLPVE
00323      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSLPVE
00324      PERFORM ACQUIRE-CONTROLLED-STORAGE.                          ELSSLPVE
00325      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSLPVE
00326      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLPVE
00327                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELSSLPVE
00328 /***********************************************************      ELSSLPVE
00329 *                                                          *      ELSSLPVE
00330 *        BUILD MENU HEADERS                                *      ELSSLPVE
00331 *                                                          *      ELSSLPVE
00332 ************************************************************      ELSSLPVE
00333  BUILD-MENU-HEADERS.                                              ELSSLPVE
00334      MOVE WS-NUMBER-HEADINGS TO MHD-NBR-HDG-LINES.                ELSSLPVE
00335      SET MHD-IDX TO 1.                                            ELSSLPVE
00336      PERFORM                                                      ELSSLPVE
00337          VARYING WS-HEAD-INDEX FROM 1 BY 1 UNTIL                  ELSSLPVE
00338                     WS-HEAD-INDEX > WS-NUMBER-HEADINGS            ELSSLPVE
00339            MOVE WS-PROV-ELIG-HEADING-LINE (WS-HEAD-INDEX) TO      ELSSLPVE
00340                 MHD-HDG-LINE (MHD-IDX)                            ELSSLPVE
00341            SET MHD-IDX UP BY 1                                    ELSSLPVE
00342      END-PERFORM.                                                 ELSSLPVE
00343                                                                   ELSSLPVE
00344                                                                   ELSSLPVE
00345 ************************************************************      ELSSLPVE
00346 *                                                          *      ELSSLPVE
00347 *        BUILD TOPIC TITLES                                *      ELSSLPVE
00348 *                                                          *      ELSSLPVE
00349 ************************************************************      ELSSLPVE
00350  BUILD-TOPIC-TITLES.                                              ELSSLPVE
00351      PERFORM READ-COBOL-NAME-RECORD.                              ELSSLPVE
00352      PERFORM START-BROWSE-OF-CODE-VALUE-FIL.                      ELSSLPVE
00353      PERFORM READ-CODE-VALUE-NEXT-RECORD.                         ELSSLPVE
00354      IF CV-RECORD-PREFIX NOT = CN-RECORD-PREFIX                   ELSSLPVE
00355          PERFORM SIGNAL-BAD-CODE-VALUE-READ.                      ELSSLPVE
00356      MOVE WS-PROV-ELIG-TITLE TO SSB-MNU-TITLE.                    ELSSLPVE
00357      PERFORM LOAD-MENU-OPTS-HEADER.                               ELSSLPVE
00358      INITIALIZE MSO-NBR-MENU-OPTS.                                ELSSLPVE
00359      SET MSO-IDX TO MSO-NBR-MENU-OPTS.                            ELSSLPVE
00360      SET MSD-IDX TO 1.                                            ELSSLPVE
00361      MOVE WS-MENU-LINE-COUNT TO MSD-NBR-DESCR-LINES.              ELSSLPVE
00362                                                                   ELSSLPVE
00363      PERFORM CHECK-CODE-LOAD-TABLES                               ELSSLPVE
00364          UNTIL CV-RECORD-PREFIX NOT = CN-RECORD-PREFIX OR         ELSSLPVE
00365                     CV-ELEMENT-NBR NOT = CN-ELEMENT-NBR OR        ELSSLPVE
00366                     IOP-RC-ENDFILE.                               ELSSLPVE
00367      PERFORM END-BROWSE-OF-CODE-VALUE-FILE.                       ELSSLPVE
00368      SET MSO-NBR-MENU-OPTS TO MSO-IDX.                            ELSSLPVE
00369      MOVE 1   TO MSO-MIN-CHOICES                                  ELSSLPVE
00370                  MSO-MAX-CHOICES.                                 ELSSLPVE
00371 /***********************************************************      ELSSLPVE
00372 *                                                          *      ELSSLPVE
00373 *        CHECK CODE LOAD TABLES                            *      ELSSLPVE
00374 *                                                          *      ELSSLPVE
00375 ************************************************************      ELSSLPVE
00376  CHECK-CODE-LOAD-TABLES.                                          ELSSLPVE
00377      MOVE CV-CODE-VALUE TO WS-TWO-BYTE-CODE.                      ELSSLPVE
00378      IF ( (SSB-PROV-CLASS-INST AND WS-INSTITUTIONAL-TYPE) OR      ELSSLPVE
00379                  SSB-PROV-CLASS-BOTH) OR                          ELSSLPVE
00380           ( (SSB-PROV-CLASS-PROF AND NOT WS-INSTITUTIONAL-TYPE)   ELSSLPVE
00381                 OR SSB-PROV-CLASS-BOTH)                           ELSSLPVE
00382          PERFORM LOAD-TABLES.                                     ELSSLPVE
00383      PERFORM READ-CODE-VALUE-NEXT-RECORD.                         ELSSLPVE
00384                                                                   ELSSLPVE
00385                                                                   ELSSLPVE
00386 ************************************************************      ELSSLPVE
00387 *                                                          *      ELSSLPVE
00388 *        LOAD MENU OPTS HEADER                             *      ELSSLPVE
00389 *                                                          *      ELSSLPVE
00390 ************************************************************      ELSSLPVE
00391  LOAD-MENU-OPTS-HEADER.                                           ELSSLPVE
00392      MOVE LENGTH OF WS-TWO-BYTE-CODE TO MSO-OPT-LEN.              ELSSLPVE
00393      SET MSO-OPT-TYP-AN   TO TRUE.                                ELSSLPVE
00394                                                                   ELSSLPVE
00395                                                                   ELSSLPVE
00396 ************************************************************      ELSSLPVE
00397 *                                                          *      ELSSLPVE
00398 *        LOAD TABLES                                       *      ELSSLPVE
00399 *                                                          *      ELSSLPVE
00400 ************************************************************      ELSSLPVE
00401  LOAD-TABLES.                                                     ELSSLPVE
00402      PERFORM LOAD-TOPIC-NAMES.                                    ELSSLPVE
00403      PERFORM LOAD-TOPIC-CODES-KEYWORD.                            ELSSLPVE
00404 /***********************************************************      ELSSLPVE
00405 *                                                          *      ELSSLPVE
00406 *        LOAD TOPIC NAMES                                  *      ELSSLPVE
00407 *                                                          *      ELSSLPVE
00408 ************************************************************      ELSSLPVE
00409  LOAD-TOPIC-NAMES.                                                ELSSLPVE
00410      PERFORM SET-IOPM-ADDRESS-FOR-MENU-FILE.                      ELSSLPVE
00411      SET IOP-ADD       TO TRUE.                                   ELSSLPVE
00412      SET IOP-FCQ-NONE  TO TRUE.                                   ELSSLPVE
00413      SET IOP-KVQ-NONE  TO TRUE.                                   ELSSLPVE
00414      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSLPVE
00415      STRING '  ' WS-TWO-BYTE-CODE '       '                       ELSSLPVE
00416         CV-VALUE-DESC-LINE(MSD-IDX) DELIMITED BY SIZE             ELSSLPVE
00417                INTO MSD-DESCR-LINE (MSD-IDX).                     ELSSLPVE
00418      MOVE LENGTH OF MSD-MENU-ITEM-DESCRIPTIONS TO                 ELSSLPVE
00419          IOP-REC-LEN.                                             ELSSLPVE
00420      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSSLPVE
00421                                                                   ELSSLPVE
00422                                                                   ELSSLPVE
00423 ************************************************************      ELSSLPVE
00424 *                                                          *      ELSSLPVE
00425 *        LOAD TOPIC CODES KEYWORD                          *      ELSSLPVE
00426 *                                                          *      ELSSLPVE
00427 ************************************************************      ELSSLPVE
00428  LOAD-TOPIC-CODES-KEYWORD.                                        ELSSLPVE
00429      SET MSO-IDX UP BY 1.                                         ELSSLPVE
00430      MOVE WS-TWO-BYTE-CODE TO                                     ELSSLPVE
00431           MSO-OPT-SEL(MSO-IDX),                                   ELSSLPVE
00432           MSO-OPT-KWD(MSO-IDX).                                   ELSSLPVE
00433                                                                   ELSSLPVE
00434                                                                   ELSSLPVE
00435 ************************************************************      ELSSLPVE
00436 *                                                          *      ELSSLPVE
00437 *        PROCESS PVE MENU COMPLETED REQUEST                *      ELSSLPVE
00438 *                                                          *      ELSSLPVE
00439 ************************************************************      ELSSLPVE
00440  PROCESS-PVE-MENU-COMPLETED-REQ.                                  ELSSLPVE
00441      MOVE SSB-MNU-CHOICE (1) TO SSB-MODIFIER-1.                   ELSSLPVE
00442      SET SSB-COMPLETED (SSB-SELECTOR-STATE)     TO                ELSSLPVE
00443          TRUE.                                                    ELSSLPVE
00444 /***********************************************************      ELSSLPVE
00445 *                                                          *      ELSSLPVE
00446 *        START BROWSE OF CODE VALUE FILE                   *      ELSSLPVE
00447 *                                                          *      ELSSLPVE
00448 ************************************************************      ELSSLPVE
00449  START-BROWSE-OF-CODE-VALUE-FIL.                                  ELSSLPVE
00450      PERFORM SET-IOPM-ADDRESS-FOR-CODE-VALU.                      ELSSLPVE
00451      SET CIA-ELPCV-DDN TO TRUE.                                   ELSSLPVE
00452      INITIALIZE KWA-ELPCV-KEY.                                    ELSSLPVE
00453      MOVE CN-RECORD-PREFIX TO KWA-CV-RECORD-PREFIX.               ELSSLPVE
00454      MOVE CN-ELEMENT-NBR   TO KWA-CV-ELEMENT-NBR.                 ELSSLPVE
00455      MOVE KWA-ELPCV-KEY    TO IOP-FILE-KEY.                       ELSSLPVE
00456      MOVE +11      TO IOP-KEY-LEN.                                ELSSLPVE
00457      SET IOP-ST-BR TO TRUE.                                       ELSSLPVE
00458      SET IOP-FCQ-GEN TO TRUE.                                     ELSSLPVE
00459      SET IOP-KVQ-EQ TO TRUE.                                      ELSSLPVE
00460      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELSSLPVE
00461      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSSLPVE
00462      IF NOT IOP-RC-OK                                             ELSSLPVE
00463          PERFORM SIGNAL-BAD-CODE-VALUE-READ.                      ELSSLPVE
00464                                                                   ELSSLPVE
00465                                                                   ELSSLPVE
00466 ************************************************************      ELSSLPVE
00467 *                                                          *      ELSSLPVE
00468 *        END BROWSE OF CODE VALUE FILE                     *      ELSSLPVE
00469 *                                                          *      ELSSLPVE
00470 ************************************************************      ELSSLPVE
00471  END-BROWSE-OF-CODE-VALUE-FILE.                                   ELSSLPVE
00472      PERFORM SET-IOPM-ADDRESS-FOR-CODE-VALU.                      ELSSLPVE
00473      SET CIA-ELPCV-DDN TO TRUE.                                   ELSSLPVE
00474      SET IOP-END-BR TO TRUE.                                      ELSSLPVE
00475      SET IOP-FCQ-NONE TO TRUE.                                    ELSSLPVE
00476      SET IOP-KVQ-NONE TO TRUE.                                    ELSSLPVE
00477      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELSSLPVE
00478      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSSLPVE
00479 /***********************************************************      ELSSLPVE
00480 *                                                          *      ELSSLPVE
00481 *        READ CODE VALUE NEXT RECORD                       *      ELSSLPVE
00482 *                                                          *      ELSSLPVE
00483 ************************************************************      ELSSLPVE
00484  READ-CODE-VALUE-NEXT-RECORD.                                     ELSSLPVE
00485      PERFORM SET-IOPM-ADDRESS-FOR-CODE-VALU.                      ELSSLPVE
00486      SET CIA-ELPCV-DDN  TO TRUE.                                  ELSSLPVE
00487      SET IOP-RD-NXT     TO TRUE.                                  ELSSLPVE
00488      SET IOP-FCQ-NONE   TO TRUE.                                  ELSSLPVE
00489      SET IOP-KVQ-NONE   TO TRUE.                                  ELSSLPVE
00490      SET IOP-STG-MODE-LOCATE  TO TRUE.                            ELSSLPVE
00491      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSSLPVE
00492      SET ADDRESS OF CODE-VALUE-AREA TO IOP-REC-PTR.               ELSSLPVE
00493                                                                   ELSSLPVE
00494                                                                   ELSSLPVE
00495 ************************************************************      ELSSLPVE
00496 *                                                          *      ELSSLPVE
00497 *        READ COBOL NAME RECORD                            *      ELSSLPVE
00498 *                                                          *      ELSSLPVE
00499 ************************************************************      ELSSLPVE
00500  READ-COBOL-NAME-RECORD.                                          ELSSLPVE
00501      PERFORM SET-IOPM-ADDRESS-FOR-COBOL-NAM.                      ELSSLPVE
00502      INITIALIZE KWA-ELPCN-KEY.                                    ELSSLPVE
00503      MOVE WS-ELPCN-KEY  TO KWA-ELPCN-KEY.                         ELSSLPVE
00504      MOVE KWA-ELPCN-KEY TO IOP-FILE-KEY.                          ELSSLPVE
00505      MOVE +38           TO IOP-KEY-LEN.                           ELSSLPVE
00506      SET CIA-ELPCN-DDN  TO TRUE.                                  ELSSLPVE
00507      SET IOP-RD TO TRUE.                                          ELSSLPVE
00508      SET IOP-FCQ-NONE TO TRUE.                                    ELSSLPVE
00509      SET IOP-KVQ-EQ   TO TRUE.                                    ELSSLPVE
00510      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELSSLPVE
00511      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSSLPVE
00512      IF NOT IOP-RC-OK                                             ELSSLPVE
00513          PERFORM SIGNAL-BAD-COBOL-NAME-READ.                      ELSSLPVE
00514      SET ADDRESS OF COBOL-NAME-AREA TO IOP-REC-PTR.               ELSSLPVE
00515 /***********************************************************      ELSSLPVE
00516 *                                                          *      ELSSLPVE
00517 *        SET IOPM ADDRESS FOR CODE VALUE                   *      ELSSLPVE
00518 *                                                          *      ELSSLPVE
00519 ************************************************************      ELSSLPVE
00520  SET-IOPM-ADDRESS-FOR-CODE-VALU.                                  ELSSLPVE
00521      SET CIA-ELPCV-DDN TO TRUE.                                   ELSSLPVE
00522      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLPVE
00523                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELSSLPVE
00524      IF CIA-RC-PTR-NULL                                           ELSSLPVE
00525          SET CIA-ELPCV-DDN TO TRUE                                ELSSLPVE
00526          PERFORM ACQUIRE-CONTROLLED-STORAGE                       ELSSLPVE
00527          SET CIA-ELPCV-DDN TO TRUE                                ELSSLPVE
00528          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELSSLPVE
00529                     ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.       ELSSLPVE
00530                                                                   ELSSLPVE
00531                                                                   ELSSLPVE
00532 ************************************************************      ELSSLPVE
00533 *                                                          *      ELSSLPVE
00534 *        SET IOPM ADDRESS FOR COBOL NAME                   *      ELSSLPVE
00535 *                                                          *      ELSSLPVE
00536 ************************************************************      ELSSLPVE
00537  SET-IOPM-ADDRESS-FOR-COBOL-NAM.                                  ELSSLPVE
00538      SET CIA-ELPCN-DDN TO TRUE.                                   ELSSLPVE
00539      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLPVE
00540                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELSSLPVE
00541      IF CIA-RC-PTR-NULL                                           ELSSLPVE
00542          SET CIA-ELPCN-DDN TO TRUE                                ELSSLPVE
00543          PERFORM ACQUIRE-CONTROLLED-STORAGE                       ELSSLPVE
00544          SET CIA-ELPCN-DDN TO TRUE                                ELSSLPVE
00545          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELSSLPVE
00546                     ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.       ELSSLPVE
00547                                                                   ELSSLPVE
00548                                                                   ELSSLPVE
00549 ************************************************************      ELSSLPVE
00550 *                                                          *      ELSSLPVE
00551 *        SET IOPM ADDRESS FOR MENU FILE                    *      ELSSLPVE
00552 *                                                          *      ELSSLPVE
00553 ************************************************************      ELSSLPVE
00554  SET-IOPM-ADDRESS-FOR-MENU-FILE.                                  ELSSLPVE
00555      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSLPVE
00556      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLPVE
00557                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELSSLPVE
00558                                                                   ELSSLPVE
00559                                                                   ELSSLPVE
00560 ************************************************************      ELSSLPVE
00561 *                                                          *      ELSSLPVE
00562 *        ACQUIRE CONTROLLED STORAGE                        *      ELSSLPVE
00563 *                                                          *      ELSSLPVE
00564 ************************************************************      ELSSLPVE
00565  ACQUIRE-CONTROLLED-STORAGE.                                      ELSSLPVE
00566      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSLPVE
00567      PERFORM CALL-STORAGE-MANAGEMENT-SUBPRO.                      ELSSLPVE
00568 /***********************************************************      ELSSLPVE
00569 *                                                          *      ELSSLPVE
00570 *        TERMINATE MODULE                                  *      ELSSLPVE
00571 *                                                          *      ELSSLPVE
00572 ************************************************************      ELSSLPVE
00573  TERMINATE-MODULE.                                                ELSSLPVE
00574      EXEC CICS RETURN END-EXEC.                                   ELSSLPVE
00575      GOBACK.                                                      ELSSLPVE
00576                                                                   ELSSLPVE
00577                                                                   ELSSLPVE
00578 ************************************************************      ELSSLPVE
00579 *                                                          *      ELSSLPVE
00580 *        CALL STORAGE MANAGEMENT SUBPROGRAM                *      ELSSLPVE
00581 *                                                          *      ELSSLPVE
00582 ************************************************************      ELSSLPVE
00583  CALL-STORAGE-MANAGEMENT-SUBPRO.                                  ELSSLPVE
00584      EXEC CICS LINK PROGRAM  ('ELUSTGMG')                         ELSSLPVE
00585                     COMMAREA (DFHCOMMAREA)                        ELSSLPVE
00586                     END-EXEC.                                     ELSSLPVE
00587                                                                   ELSSLPVE
00588                                                                   ELSSLPVE
00589 ************************************************************      ELSSLPVE
00590 *                                                          *      ELSSLPVE
00591 *        CALL INPUT-OUTPUT SUBPROGRAM                      *      ELSSLPVE
00592 *                                                          *      ELSSLPVE
00593 ************************************************************      ELSSLPVE
00594  CALL-INPUT-OUTPUT-SUBPROGRAM.                                    ELSSLPVE
00595      EXEC CICS LINK PROGRAM  ('ELUIOPGM')                         ELSSLPVE
00596                     COMMAREA (DFHCOMMAREA)                        ELSSLPVE
00597                     END-EXEC.                                     ELSSLPVE
00598                                                                   ELSSLPVE
00599                                                                   ELSSLPVE
00600 ************************************************************      ELSSLPVE
00601 *                                                          *      ELSSLPVE
00602 *        SIGNAL UNKNOWN ERROR                              *      ELSSLPVE
00603 *                                                          *      ELSSLPVE
00604 ************************************************************      ELSSLPVE
00605  SIGNAL-UNKNOWN-ERROR.                                            ELSSLPVE
00606      SET CIA-AB-UNDEF TO TRUE.                                    ELSSLPVE
00607      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELSSLPVE
00608 /***********************************************************      ELSSLPVE
00609 *                                                          *      ELSSLPVE
00610 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELSSLPVE
00611 *                                                          *      ELSSLPVE
00612 ************************************************************      ELSSLPVE
00613  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELSSLPVE
00614      EXEC CICS ABEND ABCODE('EL01') END-EXEC.                     ELSSLPVE
00615                                                                   ELSSLPVE
00616                                                                   ELSSLPVE
00617 ************************************************************      ELSSLPVE
00618 *                                                          *      ELSSLPVE
00619 *        SIGNAL CIA ADDRESSING ERROR                       *      ELSSLPVE
00620 *                                                          *      ELSSLPVE
00621 ************************************************************      ELSSLPVE
00622  SIGNAL-CIA-ADDRESSING-ERROR.                                     ELSSLPVE
00623      EXEC CICS ABEND ABCODE('EL02') END-EXEC.                     ELSSLPVE
00624                                                                   ELSSLPVE
00625                                                                   ELSSLPVE
00626 ************************************************************      ELSSLPVE
00627 *                                                          *      ELSSLPVE
00628 *        SIGNAL MISSING PARAMETER                          *      ELSSLPVE
00629 *                                                          *      ELSSLPVE
00630 ************************************************************      ELSSLPVE
00631  SIGNAL-MISSING-PARAMETER.                                        ELSSLPVE
00632      SET CIA-AB-PARM-MISSING TO TRUE.                             ELSSLPVE
00633      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELSSLPVE
00634                                                                   ELSSLPVE
00635                                                                   ELSSLPVE
00636 ************************************************************      ELSSLPVE
00637 *                                                          *      ELSSLPVE
00638 *        SIGNAL BAD COBOL NAME READ                        *      ELSSLPVE
00639 *                                                          *      ELSSLPVE
00640 ************************************************************      ELSSLPVE
00641  SIGNAL-BAD-COBOL-NAME-READ.                                      ELSSLPVE
00642      SET CIA-AB-NOTFND-ELPCN TO TRUE.                             ELSSLPVE
00643      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELSSLPVE
00644                                                                   ELSSLPVE
00645                                                                   ELSSLPVE
00646 ************************************************************      ELSSLPVE
00647 *                                                          *      ELSSLPVE
00648 *        SIGNAL BAD CODE VALUE READ                        *      ELSSLPVE
00649 *                                                          *      ELSSLPVE
00650 ************************************************************      ELSSLPVE
00651  SIGNAL-BAD-CODE-VALUE-READ.                                      ELSSLPVE
00652      SET CIA-AB-NOTFND-ELPCN TO TRUE.                             ELSSLPVE
00653      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELSSLPVE
