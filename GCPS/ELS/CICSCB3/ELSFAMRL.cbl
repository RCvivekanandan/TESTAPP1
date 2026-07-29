00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSFAMRL
00003  PROGRAM-ID.         ELSFAMRL.                                       LV002
00004                                                                   ELSFAMRL
00005  AUTHOR.             JOHN CURIN,  KEANE, INC.                     ELSFAMRL
00006                                                                   ELSFAMRL
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSFAMRL
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSFAMRL
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSFAMRL
00010                      233 N. MICHIGAN AVE                          ELSFAMRL
00011                      CHICAGO, ILLINOIS 60601                      ELSFAMRL
00012                                                                   ELSFAMRL
00013  DATE-WRITTEN.       29-OCT-1986.                                 ELSFAMRL
00014                                                                   ELSFAMRL
00015  DATE-COMPILED.                                                   ELSFAMRL
00016                                                                   ELSFAMRL
00017  SECURITY.           COPYRIGHT 1986,                              ELSFAMRL
00018                      HEALTH CARE SERVICE CORPORATION              ELSFAMRL
00019      SKIP3                                                        ELSFAMRL
00020  ENVIRONMENT DIVISION.                                            ELSFAMRL
00021                                                                   ELSFAMRL
00022  CONFIGURATION SECTION.                                           ELSFAMRL
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELSFAMRL
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELSFAMRL
00025      EJECT                                                        ELSFAMRL
00026 ******************************************************************ELSFAMRL
00027 *                                                                 ELSFAMRL
00028 *                       MAINTENANCE HISTORY                       ELSFAMRL
00029 *                                                                 ELSFAMRL
00030 * 01.00  04/25/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS           ELSFAMRL
00031 *                                                                 ELSFAMRL
00032 *        08/13/03  AKK  COMPILE TO TEST ORDER OF COMPILE          ELSFAMRL
00033 *                                                                 ELSFAMRL
00034 ******************************************************************ELSFAMRL
00035      EJECT                                                        ELSFAMRL
00036  DATA DIVISION.                                                   ELSFAMRL
00037                                                                   ELSFAMRL
00038  FILE SECTION.                                                    ELSFAMRL
00039                                                                   ELSFAMRL
00040  WORKING-STORAGE SECTION.                                         ELSFAMRL
00041                                                                   ELSFAMRL
00042 *** FAMILY RELATIONSHIP WORK AREA                                 ELSFAMRL
00043                                                                   ELSFAMRL
00044  01  WS-TABLE-COUNTS.                                             ELSFAMRL
00045      05  WS-NUMBER-CODES     PIC S9(4) COMP    VALUE +3.          ELSFAMRL
00046      05  WS-NUMBER-HEADINGS PIC S9(4) COMP     VALUE +15.         ELSFAMRL
00047                                                                   ELSFAMRL
00048  01  WS-FAM-REL-TITLE          PIC X(36)            VALUE         ELSFAMRL
00049      'SELECT FAMILY RELATIONSHIP'.                                ELSFAMRL
00050                                                                   ELSFAMRL
00051  01  WS-FAM-REL-HEADINGS.                                         ELSFAMRL
00052      05  FILLER                PIC X(78)            VALUE         ELSFAMRL
00053      'BENEFITS FOR THIS GROUP/SECTION VARY DEPENDING ON THE RELATIELSFAMRL
00054 -    'ONSHIP OF THE    '.                                         ELSFAMRL
00055      05  FILLER                PIC X(78)            VALUE         ELSFAMRL
00056      'PATIENT TO THE SUBSCRIBER.  INDICATE WHETHER THE PATIENT INVELSFAMRL
00057 -    'OLVED IN THIS    '.                                         ELSFAMRL
00058      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00059      'INQUIRY IS THE:                                             ELSFAMRL
00060 -    '                 '.                                         ELSFAMRL
00061      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00062      '                                                            ELSFAMRL
00063 -    '                 '.                                         ELSFAMRL
00064      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00065      '  M MEMBER                                                  ELSFAMRL
00066 -    '                 '.                                         ELSFAMRL
00067      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00068      '                                                            ELSFAMRL
00069 -    '                 '.                                         ELSFAMRL
00070      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00071      '  S SPOUSE                                                  ELSFAMRL
00072 -    '                 '.                                         ELSFAMRL
00073      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00074      '                                                            ELSFAMRL
00075 -    '                 '.                                         ELSFAMRL
00076      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00077      '  D DEPENDENT                                               ELSFAMRL
00078 -    '                 '.                                         ELSFAMRL
00079      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00080      '                                                            ELSFAMRL
00081 -    '                 '.                                         ELSFAMRL
00082      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00083      '                                                            ELSFAMRL
00084 -    '                 '.                                         ELSFAMRL
00085      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00086      '                                                            ELSFAMRL
00087 -    '                 '.                                         ELSFAMRL
00088      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00089      '                                                            ELSFAMRL
00090 -    '                 '.                                         ELSFAMRL
00091      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00092      '                                                            ELSFAMRL
00093 -    '                 '.                                         ELSFAMRL
00094      05  FILLER                 PIC X(78)            VALUE        ELSFAMRL
00095      '                                                            ELSFAMRL
00096 -    '                 '.                                         ELSFAMRL
00097  01  WS-FAM-REL-HEAD REDEFINES WS-FAM-REL-HEADINGS.               ELSFAMRL
00098      05  WS-FAM-REL-HEADING-LINE                                  ELSFAMRL
00099                      OCCURS 15 TIMES                              ELSFAMRL
00100                      INDEXED BY WS-HEAD-INDEX                     ELSFAMRL
00101                          PIC X(78).                               ELSFAMRL
00102                                                                   ELSFAMRL
00103  01  WS-FAM-REL-VALID-DEFINITION.                                 ELSFAMRL
00104      05  FILLER                 PIC X(17)            VALUE        ELSFAMRL
00105      'MMEM             '.                                         ELSFAMRL
00106      05  FILLER                 PIC X(17)            VALUE        ELSFAMRL
00107      'SSPOUSE          '.                                         ELSFAMRL
00108      05  FILLER                 PIC X(17)            VALUE        ELSFAMRL
00109      'DDEPENDENT       '.                                         ELSFAMRL
00110  01  WS-FAM-REL-TABLE REDEFINES WS-FAM-REL-VALID-DEFINITION.      ELSFAMRL
00111      05  WS-FAM-REL-INFO                                          ELSFAMRL
00112                      OCCURS 3 TIMES                               ELSFAMRL
00113                      INDEXED BY WS-FAM-REL-INDEX.                 ELSFAMRL
00114          10  WS-VALID-SELECT PIC X.                               ELSFAMRL
00115          10  WS-KEYWORD       PIC X(16).                          ELSFAMRL
00116                                                                   ELSFAMRL
00117  01  WS-DUMMY-PTR               POINTER.                          ELSFAMRL
00118      EJECT                                                        ELSFAMRL
00119  LINKAGE SECTION.                                                 ELSFAMRL
00120                                                                   ELSFAMRL
00121  01  DFHCOMMAREA.                                                 ELSFAMRL
00122      COPY ELSCOMMC.                                               ELSFAMRL
00123      EJECT                                                        ELSFAMRL
00124      COPY ELSCIA2C.                                               ELSFAMRL
00125      EJECT                                                        ELSFAMRL
00126      COPY ELSIOPMC.                                               ELSFAMRL
00127      EJECT                                                        ELSFAMRL
00128      COPY ELSSSCBC.                                               ELSFAMRL
00129      EJECT                                                        ELSFAMRL
00130      COPY ELSMHDGC.                                               ELSFAMRL
00131      EJECT                                                        ELSFAMRL
00132      COPY ELSMOPTC.                                               ELSFAMRL
00133      EJECT                                                        ELSFAMRL
00134      EJECT                                                        ELSFAMRL
00135  PROCEDURE DIVISION.                                              ELSFAMRL
00136 *                                                                 ELSFAMRL
00137 ************************************************************      ELSFAMRL
00138 *                                                          *      ELSFAMRL
00139 *                    PROCEDURE DIVISION                    *      ELSFAMRL
00140 *                                                          *      ELSFAMRL
00141 ************************************************************      ELSFAMRL
00142                                                                   ELSFAMRL
00143                                                                   ELSFAMRL
00144 ************************************************************      ELSFAMRL
00145 *                                                          *      ELSFAMRL
00146 *        PERFORM FAMILY RELATIONSHIP FUNCTIONS             *      ELSFAMRL
00147 *                                                          *      ELSFAMRL
00148 ************************************************************      ELSFAMRL
00149  PERFORM-FAMILY-RELATIONSHIP-FU.                                  ELSFAMRL
00150      PERFORM INITIALIZE-MODULE.                                   ELSFAMRL
00151      PERFORM PROCESS-FAMILY-RELATIONSHIP-RE.                      ELSFAMRL
00152      EXEC CICS  RETURN                                            ELSFAMRL
00153                 END-EXEC.                                         ELSFAMRL
00154                                                                   ELSFAMRL
00155                                                                   ELSFAMRL
00156 ************************************************************      ELSFAMRL
00157 *                                                          *      ELSFAMRL
00158 *        INITIALIZE MODULE                                 *      ELSFAMRL
00159 *                                                          *      ELSFAMRL
00160 ************************************************************      ELSFAMRL
00161  INITIALIZE-MODULE.                                               ELSFAMRL
00162      PERFORM CHECK-COMMAREA-LENGTH.                               ELSFAMRL
00163      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELSFAMRL
00164      PERFORM ESTABLISH-ADDRESSING-TO-SELECT.                      ELSFAMRL
00165                                                                   ELSFAMRL
00166                                                                   ELSFAMRL
00167 ************************************************************      ELSFAMRL
00168 *                                                          *      ELSFAMRL
00169 *        CHECK COMMAREA LENGTH                             *      ELSFAMRL
00170 *                                                          *      ELSFAMRL
00171 ************************************************************      ELSFAMRL
00172  CHECK-COMMAREA-LENGTH.                                           ELSFAMRL
00173      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELSFAMRL
00174          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELSFAMRL
00175                                                                   ELSFAMRL
00176                                                                   ELSFAMRL
00177 ************************************************************      ELSFAMRL
00178 *                                                          *      ELSFAMRL
00179 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELSFAMRL
00180 *                                                          *      ELSFAMRL
00181 ************************************************************      ELSFAMRL
00182  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELSFAMRL
00183      SET CIA-AB-DFHCOMMAREA                                       ELSFAMRL
00184         TO TRUE.                                                  ELSFAMRL
00185      EXEC CICS ABEND                                              ELSFAMRL
00186                ABCODE(CIA-ABCODE)                                 ELSFAMRL
00187                END-EXEC.                                          ELSFAMRL
00188      EJECT                                                        ELSFAMRL
00189                                                                   ELSFAMRL
00190                                                                   ELSFAMRL
00191 ************************************************************      ELSFAMRL
00192 *                                                          *      ELSFAMRL
00193 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELSFAMRL
00194 *                                                          *      ELSFAMRL
00195 ************************************************************      ELSFAMRL
00196  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELSFAMRL
00197      IF ECA-CIA-PTR IS NOT EQUAL NULL                             ELSFAMRL
00198          PERFORM SET-CIA-ADDRESS                                  ELSFAMRL
00199      ELSE                                                         ELSFAMRL
00200          PERFORM SIGNAL-CIA-ADDRESSING-ERROR.                     ELSFAMRL
00201                                                                   ELSFAMRL
00202                                                                   ELSFAMRL
00203 ************************************************************      ELSFAMRL
00204 *                                                          *      ELSFAMRL
00205 *        SET CIA ADDRESS                                   *      ELSFAMRL
00206 *                                                          *      ELSFAMRL
00207 ************************************************************      ELSFAMRL
00208  SET-CIA-ADDRESS.                                                 ELSFAMRL
00209      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSFAMRL
00210          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSFAMRL
00211                                                                   ELSFAMRL
00212                                                                   ELSFAMRL
00213 ************************************************************      ELSFAMRL
00214 *                                                          *      ELSFAMRL
00215 *        SIGNAL CIA ADDRESSING ERROR                       *      ELSFAMRL
00216 *                                                          *      ELSFAMRL
00217 ************************************************************      ELSFAMRL
00218  SIGNAL-CIA-ADDRESSING-ERROR.                                     ELSFAMRL
00219      SET  CIA-AB-ELSCIA-PTR                                       ELSFAMRL
00220          TO TRUE.                                                 ELSFAMRL
00221      EXEC CICS ABEND                                              ELSFAMRL
00222                ABCODE(CIA-ABCODE)                                 ELSFAMRL
00223                END-EXEC.                                          ELSFAMRL
00224      EJECT                                                        ELSFAMRL
00225                                                                   ELSFAMRL
00226                                                                   ELSFAMRL
00227 ************************************************************      ELSFAMRL
00228 *                                                          *      ELSFAMRL
00229 *        ESTABLISH ADDRESSING TO SELECTOR CONTROL AREA     *      ELSFAMRL
00230 *                                                          *      ELSFAMRL
00231 ************************************************************      ELSFAMRL
00232  ESTABLISH-ADDRESSING-TO-SELECT.                                  ELSFAMRL
00233      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSFAMRL
00234      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSFAMRL
00235                            WS-DUMMY-PTR.                          ELSFAMRL
00236      IF NOT CIA-RC-PTR-NULL                                       ELSFAMRL
00237          PERFORM SET-SSCB-ADDRESS                                 ELSFAMRL
00238      ELSE                                                         ELSFAMRL
00239          PERFORM SIGNAL-MISSING-PARAMETER.                        ELSFAMRL
00240                                                                   ELSFAMRL
00241                                                                   ELSFAMRL
00242 ************************************************************      ELSFAMRL
00243 *                                                          *      ELSFAMRL
00244 *        SET SSCB ADDRESS                                  *      ELSFAMRL
00245 *                                                          *      ELSFAMRL
00246 ************************************************************      ELSFAMRL
00247  SET-SSCB-ADDRESS.                                                ELSFAMRL
00248      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSFAMRL
00249      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSFAMRL
00250          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSFAMRL
00251                                                                   ELSFAMRL
00252                                                                   ELSFAMRL
00253 ************************************************************      ELSFAMRL
00254 *                                                          *      ELSFAMRL
00255 *        SIGNAL MISSING PARAMETER                          *      ELSFAMRL
00256 *                                                          *      ELSFAMRL
00257 ************************************************************      ELSFAMRL
00258  SIGNAL-MISSING-PARAMETER.                                        ELSFAMRL
00259      SET CIA-AB-PARM-MISSING TO TRUE.                             ELSFAMRL
00260      EXEC CICS ABEND ABCODE(CIA-ABCODE)                           ELSFAMRL
00261                END-EXEC.                                          ELSFAMRL
00262      EJECT                                                        ELSFAMRL
00263                                                                   ELSFAMRL
00264                                                                   ELSFAMRL
00265 ************************************************************      ELSFAMRL
00266 *                                                          *      ELSFAMRL
00267 *        PROCESS FAMILY RELATIONSHIP REQUEST               *      ELSFAMRL
00268 *                                                          *      ELSFAMRL
00269 ************************************************************      ELSFAMRL
00270  PROCESS-FAMILY-RELATIONSHIP-RE.                                  ELSFAMRL
00271      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSFAMRL
00272          PERFORM PROCESS-BUILD-MENU-REQUEST                       ELSFAMRL
00273      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSFAMRL
00274          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSFAMRL
00275      ELSE                                                         ELSFAMRL
00276          PERFORM SIGNAL-INVALID-FAMILY-RELATION.                  ELSFAMRL
00277      EJECT                                                        ELSFAMRL
00278                                                                   ELSFAMRL
00279                                                                   ELSFAMRL
00280 ************************************************************      ELSFAMRL
00281 *                                                          *      ELSFAMRL
00282 *        SIGNAL INVALID FAMILY RELATIONSHIP REQUEST        *      ELSFAMRL
00283 *                                                          *      ELSFAMRL
00284 ************************************************************      ELSFAMRL
00285  SIGNAL-INVALID-FAMILY-RELATION.                                  ELSFAMRL
00286      SET CIA-AB-UNDEF TO TRUE.                                    ELSFAMRL
00287      EXEC CICS ABEND                                              ELSFAMRL
00288                ABCODE(CIA-ABCODE)                                 ELSFAMRL
00289                END-EXEC.                                          ELSFAMRL
00290                                                                   ELSFAMRL
00291                                                                   ELSFAMRL
00292 ************************************************************      ELSFAMRL
00293 *                                                          *      ELSFAMRL
00294 *        PROCESS BUILD MENU REQUEST                        *      ELSFAMRL
00295 *                                                          *      ELSFAMRL
00296 ************************************************************      ELSFAMRL
00297  PROCESS-BUILD-MENU-REQUEST.                                      ELSFAMRL
00298      INITIALIZE SSB-MNU-CHOICE-TABLE.                             ELSFAMRL
00299      PERFORM DELETE-MENU-FILE.                                    ELSFAMRL
00300      PERFORM ACQUIRE-STORAGE-AREAS.                               ELSFAMRL
00301      PERFORM BUILD-MENU-HEADERS.                                  ELSFAMRL
00302      PERFORM BUILD-VALID-SELECTIONS.                              ELSFAMRL
00303      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSFAMRL
00304      EJECT                                                        ELSFAMRL
00305                                                                   ELSFAMRL
00306                                                                   ELSFAMRL
00307 ************************************************************      ELSFAMRL
00308 *                                                          *      ELSFAMRL
00309 *        DELETE MENU FILE                                  *      ELSFAMRL
00310 *                                                          *      ELSFAMRL
00311 ************************************************************      ELSFAMRL
00312  DELETE-MENU-FILE.                                                ELSFAMRL
00313      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSFAMRL
00314      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSFAMRL
00315                            WS-DUMMY-PTR.                          ELSFAMRL
00316      IF CIA-RC-PTR-NULL                                           ELSFAMRL
00317          PERFORM ALLOCATE-MENU-AREA.                              ELSFAMRL
00318      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSFAMRL
00319      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSFAMRL
00320          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSFAMRL
00321      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSFAMRL
00322      SET IOP-DEL          TO TRUE.                                ELSFAMRL
00323      SET IOP-FCQ-NONE     TO TRUE.                                ELSFAMRL
00324      SET IOP-KVQ-NONE     TO TRUE.                                ELSFAMRL
00325      PERFORM CALL-IO-PROGRAM.                                     ELSFAMRL
00326      PERFORM DEALLOCATE-MENU-AREA.                                ELSFAMRL
00327      EJECT                                                        ELSFAMRL
00328                                                                   ELSFAMRL
00329                                                                   ELSFAMRL
00330 ************************************************************      ELSFAMRL
00331 *                                                          *      ELSFAMRL
00332 *        CALL IO PROGRAM                                   *      ELSFAMRL
00333 *                                                          *      ELSFAMRL
00334 ************************************************************      ELSFAMRL
00335  CALL-IO-PROGRAM.                                                 ELSFAMRL
00336      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELSFAMRL
00337                COMMAREA(DFHCOMMAREA)                              ELSFAMRL
00338                END-EXEC.                                          ELSFAMRL
00339      EJECT                                                        ELSFAMRL
00340                                                                   ELSFAMRL
00341                                                                   ELSFAMRL
00342 ************************************************************      ELSFAMRL
00343 *                                                          *      ELSFAMRL
00344 *        DEALLOCATE MENU AREA                              *      ELSFAMRL
00345 *                                                          *      ELSFAMRL
00346 ************************************************************      ELSFAMRL
00347  DEALLOCATE-MENU-AREA.                                            ELSFAMRL
00348      SET  CIA-ELSMENU-DDN  TO TRUE.                               ELSFAMRL
00349      SET  CIA-STG-FREEMAIN TO TRUE.                               ELSFAMRL
00350      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSFAMRL
00351                                                                   ELSFAMRL
00352                                                                   ELSFAMRL
00353 ************************************************************      ELSFAMRL
00354 *                                                          *      ELSFAMRL
00355 *        ALLOCATE MENU AREA                                *      ELSFAMRL
00356 *                                                          *      ELSFAMRL
00357 ************************************************************      ELSFAMRL
00358  ALLOCATE-MENU-AREA.                                              ELSFAMRL
00359      SET  CIA-ELSMENU-DDN TO TRUE.                                ELSFAMRL
00360      SET  CIA-STG-GETMAIN TO TRUE.                                ELSFAMRL
00361      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSFAMRL
00362                                                                   ELSFAMRL
00363                                                                   ELSFAMRL
00364 ************************************************************      ELSFAMRL
00365 *                                                          *      ELSFAMRL
00366 *        CALL STORAGE SUBPROGRAM                           *      ELSFAMRL
00367 *                                                          *      ELSFAMRL
00368 ************************************************************      ELSFAMRL
00369  CALL-STORAGE-SUBPROGRAM.                                         ELSFAMRL
00370      EXEC CICS LINK PROGRAM ('ELUSTGMG')                          ELSFAMRL
00371                     COMMAREA (DFHCOMMAREA)                        ELSFAMRL
00372                     END-EXEC.                                     ELSFAMRL
00373      EJECT                                                        ELSFAMRL
00374                                                                   ELSFAMRL
00375                                                                   ELSFAMRL
00376 ************************************************************      ELSFAMRL
00377 *                                                          *      ELSFAMRL
00378 *        ACQUIRE STORAGE AREAS                             *      ELSFAMRL
00379 *                                                          *      ELSFAMRL
00380 ************************************************************      ELSFAMRL
00381  ACQUIRE-STORAGE-AREAS.                                           ELSFAMRL
00382      PERFORM GET-HEADING-STORAGE-AREA.                            ELSFAMRL
00383      PERFORM GET-SELECTION-CODE-KEYWORD-ARE.                      ELSFAMRL
00384                                                                   ELSFAMRL
00385                                                                   ELSFAMRL
00386 ************************************************************      ELSFAMRL
00387 *                                                          *      ELSFAMRL
00388 *        GET HEADING STORAGE AREA                          *      ELSFAMRL
00389 *                                                          *      ELSFAMRL
00390 ************************************************************      ELSFAMRL
00391  GET-HEADING-STORAGE-AREA.                                        ELSFAMRL
00392      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSFAMRL
00393      COMPUTE CIA-AREA-LEN = LENGTH OF                             ELSFAMRL
00394              MHD-NBR-HDG-LINES +                                  ELSFAMRL
00395                   (LENGTH OF MHD-HDG-LINE *                       ELSFAMRL
00396          WS-NUMBER-HEADINGS).                                     ELSFAMRL
00397      SET CIA-STG-GETMAIN TO TRUE.                                 ELSFAMRL
00398      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSFAMRL
00399      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSFAMRL
00400      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSFAMRL
00401          ADDRESS OF MHD-MENU-HEADINGS.                            ELSFAMRL
00402      EJECT                                                        ELSFAMRL
00403                                                                   ELSFAMRL
00404                                                                   ELSFAMRL
00405 ************************************************************      ELSFAMRL
00406 *                                                          *      ELSFAMRL
00407 *        GET SELECTION CODE KEYWORD AREA                   *      ELSFAMRL
00408 *                                                          *      ELSFAMRL
00409 ************************************************************      ELSFAMRL
00410  GET-SELECTION-CODE-KEYWORD-ARE.                                  ELSFAMRL
00411      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSFAMRL
00412      COMPUTE CIA-AREA-LEN = LENGTH OF                             ELSFAMRL
00413          MSO-MENU-OPTS-HEADER +                                   ELSFAMRL
00414          (LENGTH OF MSO-MENU-OPT * WS-NUMBER-CODES).              ELSFAMRL
00415      SET CIA-STG-GETMAIN TO TRUE.                                 ELSFAMRL
00416      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSFAMRL
00417      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSFAMRL
00418      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSFAMRL
00419          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSFAMRL
00420      EJECT                                                        ELSFAMRL
00421                                                                   ELSFAMRL
00422                                                                   ELSFAMRL
00423 ************************************************************      ELSFAMRL
00424 *                                                          *      ELSFAMRL
00425 *        BUILD MENU HEADERS                                *      ELSFAMRL
00426 *                                                          *      ELSFAMRL
00427 ************************************************************      ELSFAMRL
00428  BUILD-MENU-HEADERS.                                              ELSFAMRL
00429      MOVE WS-FAM-REL-TITLE TO SSB-MNU-TITLE.                      ELSFAMRL
00430      MOVE WS-NUMBER-HEADINGS TO MHD-NBR-HDG-LINES.                ELSFAMRL
00431      SET MHD-IDX TO 1.                                            ELSFAMRL
00432      PERFORM LOAD-MENU-HEADINGS                                   ELSFAMRL
00433          VARYING WS-HEAD-INDEX FROM 1 BY 1                        ELSFAMRL
00434                     UNTIL WS-HEAD-INDEX >                         ELSFAMRL
00435              WS-NUMBER-HEADINGS.                                  ELSFAMRL
00436                                                                   ELSFAMRL
00437                                                                   ELSFAMRL
00438 ************************************************************      ELSFAMRL
00439 *                                                          *      ELSFAMRL
00440 *        LOAD MENU HEADINGS                                *      ELSFAMRL
00441 *                                                          *      ELSFAMRL
00442 ************************************************************      ELSFAMRL
00443  LOAD-MENU-HEADINGS.                                              ELSFAMRL
00444      MOVE WS-FAM-REL-HEADING-LINE (WS-HEAD-INDEX) TO              ELSFAMRL
00445          MHD-HDG-LINE (MHD-IDX).                                  ELSFAMRL
00446      SET MHD-IDX UP BY 1.                                         ELSFAMRL
00447      EJECT                                                        ELSFAMRL
00448                                                                   ELSFAMRL
00449                                                                   ELSFAMRL
00450 ************************************************************      ELSFAMRL
00451 *                                                          *      ELSFAMRL
00452 *        BUILD VALID SELECTIONS                            *      ELSFAMRL
00453 *                                                          *      ELSFAMRL
00454 ************************************************************      ELSFAMRL
00455  BUILD-VALID-SELECTIONS.                                          ELSFAMRL
00456      PERFORM LOAD-MENU-OPTS-HEADER.                               ELSFAMRL
00457      SET MSO-IDX TO 1.                                            ELSFAMRL
00458      PERFORM LOAD-TOPIC-CODES-KEYWORD                             ELSFAMRL
00459          VARYING WS-FAM-REL-INDEX FROM 1 BY 1                     ELSFAMRL
00460                           UNTIL WS-FAM-REL-INDEX IS GREATER       ELSFAMRL
00461              THAN                                                 ELSFAMRL
00462                       WS-NUMBER-CODES.                            ELSFAMRL
00463                                                                   ELSFAMRL
00464                                                                   ELSFAMRL
00465 ************************************************************      ELSFAMRL
00466 *                                                          *      ELSFAMRL
00467 *        LOAD MENU OPTS HEADER                             *      ELSFAMRL
00468 *                                                          *      ELSFAMRL
00469 ************************************************************      ELSFAMRL
00470  LOAD-MENU-OPTS-HEADER.                                           ELSFAMRL
00471      MOVE WS-NUMBER-CODES TO MSO-NBR-MENU-OPTS.                   ELSFAMRL
00472      MOVE LENGTH OF WS-VALID-SELECT TO MSO-OPT-LEN.               ELSFAMRL
00473      SET MSO-OPT-TYP-AN   TO TRUE.                                ELSFAMRL
00474      MOVE 1 TO MSO-MIN-CHOICES                                    ELSFAMRL
00475                MSO-MAX-CHOICES.                                   ELSFAMRL
00476                                                                   ELSFAMRL
00477                                                                   ELSFAMRL
00478 ************************************************************      ELSFAMRL
00479 *                                                          *      ELSFAMRL
00480 *        LOAD TOPIC CODES KEYWORD                          *      ELSFAMRL
00481 *                                                          *      ELSFAMRL
00482 ************************************************************      ELSFAMRL
00483  LOAD-TOPIC-CODES-KEYWORD.                                        ELSFAMRL
00484      MOVE WS-VALID-SELECT(WS-FAM-REL-INDEX) TO                    ELSFAMRL
00485           MSO-OPT-SEL(MSO-IDX).                                   ELSFAMRL
00486      MOVE WS-KEYWORD(WS-FAM-REL-INDEX) TO                         ELSFAMRL
00487           MSO-OPT-KWD(MSO-IDX).                                   ELSFAMRL
00488      SET MSO-IDX UP BY 1.                                         ELSFAMRL
00489      EJECT                                                        ELSFAMRL
00490                                                                   ELSFAMRL
00491                                                                   ELSFAMRL
00492 ************************************************************      ELSFAMRL
00493 *                                                          *      ELSFAMRL
00494 *        PROCESS MENU COMPLETED REQUEST                    *      ELSFAMRL
00495 *                                                          *      ELSFAMRL
00496 ************************************************************      ELSFAMRL
00497  PROCESS-MENU-COMPLETED-REQUEST.                                  ELSFAMRL
00498      PERFORM MOVE-DECODED-VALUE-TO-SSB.                           ELSFAMRL
00499      SET SSB-COMPLETED (SSB-SELECTOR-STATE)     TO                ELSFAMRL
00500           TRUE.                                                   ELSFAMRL
00501                                                                   ELSFAMRL
00502                                                                   ELSFAMRL
00503 ************************************************************      ELSFAMRL
00504 *                                                          *      ELSFAMRL
00505 *        MOVE DECODED VALUE TO SSB                         *      ELSFAMRL
00506 *                                                          *      ELSFAMRL
00507 ************************************************************      ELSFAMRL
00508  MOVE-DECODED-VALUE-TO-SSB.                                       ELSFAMRL
00509      SET WS-FAM-REL-INDEX TO 1.                                   ELSFAMRL
00510      SEARCH WS-FAM-REL-INFO                                       ELSFAMRL
00511            VARYING WS-FAM-REL-INDEX                               ELSFAMRL
00512              AT END                                               ELSFAMRL
00513                SET CIA-AB-PGM-LOGIC TO TRUE                       ELSFAMRL
00514                EXEC CICS ABEND                                    ELSFAMRL
00515                          ABCODE(CIA-ABCODE)                       ELSFAMRL
00516                END-EXEC                                           ELSFAMRL
00517            WHEN                                                   ELSFAMRL
00518                SSB-MNU-CHOICE (1) =                               ELSFAMRL
00519          WS-KEYWORD(WS-FAM-REL-INDEX)                             ELSFAMRL
00520                    MOVE WS-VALID-SELECT(WS-FAM-REL-INDEX)         ELSFAMRL
00521          TO                                                       ELSFAMRL
00522                       SSB-FAM-REL                                 ELSFAMRL
00523         END-SEARCH.                                               ELSFAMRL
00524      EJECT                                                        ELSFAMRL
00525  GOBACK-PARAGRAPH.                                                ELSFAMRL
00526 ************************************************************      ELSFAMRL
00527 *                                                          *      ELSFAMRL
00528 *                         STOP RUN                         *      ELSFAMRL
00529 *                                                          *      ELSFAMRL
00530 ************************************************************      ELSFAMRL
00531      GOBACK.                                                      ELSFAMRL
