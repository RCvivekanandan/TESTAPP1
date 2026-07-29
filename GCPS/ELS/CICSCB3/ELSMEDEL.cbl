00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSMEDEL
00003  PROGRAM-ID.         ELSMEDEL.                                       LV002
00004                                                                   ELSMEDEL
00005  AUTHOR.             JOHN CURIN,  KEANE, INC.                     ELSMEDEL
00006                                                                   ELSMEDEL
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSMEDEL
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSMEDEL
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSMEDEL
00010                      233 N. MICHIGAN AVE                          ELSMEDEL
00011                      CHICAGO, ILLINOIS 60601                      ELSMEDEL
00012                                                                   ELSMEDEL
00013  DATE-WRITTEN.       28-OCT-1986.                                 ELSMEDEL
00014                                                                   ELSMEDEL
00015  DATE-COMPILED.                                                   ELSMEDEL
00016                                                                   ELSMEDEL
00017  SECURITY.           COPYRIGHT 1986,                              ELSMEDEL
00018                      HEALTH CARE SERVICE CORPORATION              ELSMEDEL
00019      SKIP3                                                        ELSMEDEL
00020 ******************************************************************ELSMEDEL
00021 *                                                                *ELSMEDEL
00022 *    COPYBOOK:   ELSMEDEL                                        *ELSMEDEL
00023 *    DATE:       28-OCT-1986                                     *ELSMEDEL
00024 *    AUTHOR:     JOHN CURIN                                      *ELSMEDEL
00025 *    FUNCTION:   MEDICARE ELIGIBILITY SELECTOR PROGRAM           *ELSMEDEL
00026 *                                                                *ELSMEDEL
00027 *                                                                *ELSMEDEL
00028 ******************************************************************ELSMEDEL
00029 *                                                                *ELSMEDEL
00030 *                      MAINTENANCE HISTORY                       *ELSMEDEL
00031 *                                                                *ELSMEDEL
00032 *  MOD     DATE     BY  DRPT                ACTION               *ELSMEDEL
00033 * ----- ----------- --- ----- ---------------------------------- *ELSMEDEL
00034 * 01.00 28-OCT-1986 JC        CREATED                            *ELSMEDEL
00035 *                                                                *ELSMEDEL
00036 * 01.01 22-SEP-1987 REB       ISSUE ABEND CODES OF 'EL01' IF AN  *ELSMEDEL
00037 *                             INVALID COMMAREA  OR 'EL02' IF THE *ELSMEDEL
00038 *                             CIA BLOCK IS NOT PRESENT.          *ELSMEDEL
00039 * 01.02 10-APR-1989 EGL       CONVERTED FROM STRUCTURES          *ELSMEDEL
00040 *                                                                *ELSMEDEL
00041 * 01.03 11-APR-1989 GEM       STORAGE MANAGEMENT ENHANCEMENTS    *ELSMEDEL
00042 *                                                                *ELSMEDEL
00043 *       13-APR-2003 AKK       REGEN TO TEST ORDER OF COMPILES    *ELSMEDEL
00044 *                                                                *ELSMEDEL
00045 ******************************************************************ELSMEDEL
00046                                                                   ELSMEDEL
00047  ENVIRONMENT DIVISION.                                            ELSMEDEL
00048                                                                   ELSMEDEL
00049  CONFIGURATION SECTION.                                           ELSMEDEL
00050  SOURCE-COMPUTER.    IBM-3033.                                    ELSMEDEL
00051  OBJECT-COMPUTER.    IBM-3033.                                    ELSMEDEL
00052      EJECT                                                        ELSMEDEL
00053  DATA DIVISION.                                                   ELSMEDEL
00054                                                                   ELSMEDEL
00055  FILE SECTION.                                                    ELSMEDEL
00056                                                                   ELSMEDEL
00057  WORKING-STORAGE SECTION.                                         ELSMEDEL
00058                                                                   ELSMEDEL
00059 *** MEDICARE ELIGIBILITY WORK AREA                                ELSMEDEL
00060                                                                   ELSMEDEL
00061  01  WS-TABLE-COUNTS.                                             ELSMEDEL
00062      05  WS-NUMBER-CODES     PIC S9(4) COMP    VALUE +2.          ELSMEDEL
00063      05  WS-NUMBER-HEADINGS PIC S9(4) COMP     VALUE +15.         ELSMEDEL
00064                                                                   ELSMEDEL
00065  01  WS-MEDICARE-TITLE         PIC X(37)            VALUE         ELSMEDEL
00066      'SELECT MEDICARE ELIGIBILITY'.                               ELSMEDEL
00067                                                                   ELSMEDEL
00068  01  WS-MEDICARE-HEADINGS.                                        ELSMEDEL
00069      05  FILLER                PIC X(78)            VALUE         ELSMEDEL
00070      'BENEFITS FOR THIS GROUP/SECTION VARY ACCORDING TO WHETHER THELSMEDEL
00071 -    'E PATIENT IS OR IS'.                                        ELSMEDEL
00072      05  FILLER                PIC X(78)            VALUE         ELSMEDEL
00073      'NOT ELIGIBLE FOR MEDICARE COVERAGE                          ELSMEDEL
00074 -    '                 '.                                         ELSMEDEL
00075      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00076      '                                                            ELSMEDEL
00077 -    '                 '.                                         ELSMEDEL
00078      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00079      '                                                            ELSMEDEL
00080 -    '                 '.                                         ELSMEDEL
00081      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00082      '  Y MEDICARE ELIGIBLE                                       ELSMEDEL
00083 -    '                 '.                                         ELSMEDEL
00084      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00085      '                                                            ELSMEDEL
00086 -    '                 '.                                         ELSMEDEL
00087      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00088      '  N NOT MEDICARE ELIGIBLE                                   ELSMEDEL
00089 -    '                 '.                                         ELSMEDEL
00090      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00091      '                                                            ELSMEDEL
00092 -    '                 '.                                         ELSMEDEL
00093      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00094      '                                                            ELSMEDEL
00095 -    '                 '.                                         ELSMEDEL
00096      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00097      '                                                            ELSMEDEL
00098 -    '                 '.                                         ELSMEDEL
00099      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00100      '                                                            ELSMEDEL
00101 -    '                 '.                                         ELSMEDEL
00102      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00103      '                                                            ELSMEDEL
00104 -    '                 '.                                         ELSMEDEL
00105      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00106      '                                                            ELSMEDEL
00107 -    '                 '.                                         ELSMEDEL
00108      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00109      '                                                            ELSMEDEL
00110 -    '                 '.                                         ELSMEDEL
00111      05  FILLER                 PIC X(78)            VALUE        ELSMEDEL
00112      '                                                            ELSMEDEL
00113 -    '                 '.                                         ELSMEDEL
00114  01  WS-MEDICARE-HEAD REDEFINES WS-MEDICARE-HEADINGS.             ELSMEDEL
00115      05  WS-MEDICARE-HEADING-LINE                                 ELSMEDEL
00116                      OCCURS 15 TIMES                              ELSMEDEL
00117                      INDEXED BY WS-HEAD-INDEX                     ELSMEDEL
00118                          PIC X(78).                               ELSMEDEL
00119                                                                   ELSMEDEL
00120  01  WS-MEDICARE-VALID-DEFINITION.                                ELSMEDEL
00121      05  FILLER                 PIC X(17)            VALUE        ELSMEDEL
00122      'YMEDELIG         '.                                         ELSMEDEL
00123      05  FILLER                 PIC X(17)            VALUE        ELSMEDEL
00124      'NNONMED          '.                                         ELSMEDEL
00125  01  WS-MEDICARE-TABLE REDEFINES WS-MEDICARE-VALID-DEFINITION.    ELSMEDEL
00126      05  WS-MEDICARE-INFO                                         ELSMEDEL
00127                      OCCURS 2 TIMES                               ELSMEDEL
00128                      INDEXED BY WS-MEDICARE-INDEX.                ELSMEDEL
00129          10  WS-VALID-SELECT  PIC X.                              ELSMEDEL
00130          10  WS-KEYWORD       PIC X(16).                          ELSMEDEL
00131                                                                   ELSMEDEL
00132      EJECT                                                        ELSMEDEL
00133  LINKAGE SECTION.                                                 ELSMEDEL
00134                                                                   ELSMEDEL
00135  01  DFHCOMMAREA.                                                 ELSMEDEL
00136      COPY ELSCOMMC.                                               ELSMEDEL
00137      EJECT                                                        ELSMEDEL
00138      COPY ELSCIA2C.                                               ELSMEDEL
00139      EJECT                                                        ELSMEDEL
00140      COPY ELSIOPMC.                                               ELSMEDEL
00141      EJECT                                                        ELSMEDEL
00142      COPY ELSSSCBC.                                               ELSMEDEL
00143      EJECT                                                        ELSMEDEL
00144      COPY ELSMHDGC.                                               ELSMEDEL
00145      EJECT                                                        ELSMEDEL
00146      COPY ELSMOPTC.                                               ELSMEDEL
00147      EJECT                                                        ELSMEDEL
00148      EJECT                                                        ELSMEDEL
00149  PROCEDURE DIVISION.                                              ELSMEDEL
00150 ************************************************************      ELSMEDEL
00151 *                                                          *      ELSMEDEL
00152 *                    PROCEDURE DIVISION                    *      ELSMEDEL
00153 *                                                          *      ELSMEDEL
00154 ************************************************************      ELSMEDEL
00155                                                                   ELSMEDEL
00156                                                                   ELSMEDEL
00157 ************************************************************      ELSMEDEL
00158 *                                                          *      ELSMEDEL
00159 *        PERFORM MEDICARE ELIGIBILITY FUNCTIONS            *      ELSMEDEL
00160 *                                                          *      ELSMEDEL
00161 ************************************************************      ELSMEDEL
00162  PERFORM-MEDICARE-ELIGIBILITY-F.                                  ELSMEDEL
00163      PERFORM INITIALIZE-MODULE.                                   ELSMEDEL
00164      PERFORM PROCESS-MEDICARE-ELIGIBILITY-R.                      ELSMEDEL
00165      EXEC CICS  RETURN                                            ELSMEDEL
00166                 END-EXEC.                                         ELSMEDEL
00167                                                                   ELSMEDEL
00168                                                                   ELSMEDEL
00169 ************************************************************      ELSMEDEL
00170 *                                                          *      ELSMEDEL
00171 *        INITIALIZE MODULE                                 *      ELSMEDEL
00172 *                                                          *      ELSMEDEL
00173 ************************************************************      ELSMEDEL
00174  INITIALIZE-MODULE.                                               ELSMEDEL
00175      PERFORM CHECK-COMMAREA-LENGTH.                               ELSMEDEL
00176      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELSMEDEL
00177      PERFORM ESTABLISH-ADDRESSING-TO-SELECT.                      ELSMEDEL
00178      EJECT                                                        ELSMEDEL
00179                                                                   ELSMEDEL
00180                                                                   ELSMEDEL
00181 ************************************************************      ELSMEDEL
00182 *                                                          *      ELSMEDEL
00183 *        PROCESS MEDICARE ELIGIBILITY REQUEST              *      ELSMEDEL
00184 *                                                          *      ELSMEDEL
00185 ************************************************************      ELSMEDEL
00186  PROCESS-MEDICARE-ELIGIBILITY-R.                                  ELSMEDEL
00187      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSMEDEL
00188          PERFORM PROCESS-BUILD-MENU-REQUEST                       ELSMEDEL
00189      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSMEDEL
00190          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSMEDEL
00191      ELSE                                                         ELSMEDEL
00192          PERFORM SIGNAL-INVALID-MEDICARE-ELIGIB.                  ELSMEDEL
00193                                                                   ELSMEDEL
00194                                                                   ELSMEDEL
00195 ************************************************************      ELSMEDEL
00196 *                                                          *      ELSMEDEL
00197 *        PROCESS BUILD MENU REQUEST                        *      ELSMEDEL
00198 *                                                          *      ELSMEDEL
00199 ************************************************************      ELSMEDEL
00200  PROCESS-BUILD-MENU-REQUEST.                                      ELSMEDEL
00201      INITIALIZE SSB-MNU-CHOICE (1).                               ELSMEDEL
00202      PERFORM DELETE-MENU-FILE.                                    ELSMEDEL
00203      PERFORM ACQUIRE-STORAGE-AREAS.                               ELSMEDEL
00204      PERFORM BUILD-MENU-HEADERS.                                  ELSMEDEL
00205      PERFORM BUILD-VALID-SELECTIONS.                              ELSMEDEL
00206      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSMEDEL
00207      EJECT                                                        ELSMEDEL
00208                                                                   ELSMEDEL
00209                                                                   ELSMEDEL
00210 ************************************************************      ELSMEDEL
00211 *                                                          *      ELSMEDEL
00212 *        DELETE MENU FILE                                  *      ELSMEDEL
00213 *                                                          *      ELSMEDEL
00214 ************************************************************      ELSMEDEL
00215  DELETE-MENU-FILE.                                                ELSMEDEL
00216      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSMEDEL
00217      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEDEL
00218          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSMEDEL
00219      IF CIA-RC-PTR-NULL                                           ELSMEDEL
00220          PERFORM ALLOCATE-MENU-AREA.                              ELSMEDEL
00221                                                                   ELSMEDEL
00222      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSMEDEL
00223      SET IOP-DEL          TO TRUE.                                ELSMEDEL
00224      SET IOP-FCQ-NONE     TO TRUE.                                ELSMEDEL
00225      SET IOP-KVQ-NONE     TO TRUE.                                ELSMEDEL
00226      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSMEDEL
00227      PERFORM DEALLOCATE-MENU-AREA.                                ELSMEDEL
00228      EJECT                                                        ELSMEDEL
00229                                                                   ELSMEDEL
00230                                                                   ELSMEDEL
00231 ************************************************************      ELSMEDEL
00232 *                                                          *      ELSMEDEL
00233 *        ACQUIRE STORAGE AREAS                             *      ELSMEDEL
00234 *                                                          *      ELSMEDEL
00235 ************************************************************      ELSMEDEL
00236  ACQUIRE-STORAGE-AREAS.                                           ELSMEDEL
00237      PERFORM GET-HEADING-STORAGE-AREA.                            ELSMEDEL
00238      PERFORM GET-SELECTION-CODE-KEYWORD-ARE.                      ELSMEDEL
00239                                                                   ELSMEDEL
00240                                                                   ELSMEDEL
00241 ************************************************************      ELSMEDEL
00242 *                                                          *      ELSMEDEL
00243 *        GET HEADING STORAGE AREA                          *      ELSMEDEL
00244 *                                                          *      ELSMEDEL
00245 ************************************************************      ELSMEDEL
00246  GET-HEADING-STORAGE-AREA.                                        ELSMEDEL
00247      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSMEDEL
00248      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +         ELSMEDEL
00249               (LENGTH OF MHD-HDG-LINE *                           ELSMEDEL
00250          WS-NUMBER-HEADINGS).                                     ELSMEDEL
00251      SET CIA-STG-GETMAIN TO TRUE.                                 ELSMEDEL
00252      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSMEDEL
00253      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSMEDEL
00254      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEDEL
00255          ADDRESS OF MHD-MENU-HEADINGS.                            ELSMEDEL
00256                                                                   ELSMEDEL
00257                                                                   ELSMEDEL
00258 ************************************************************      ELSMEDEL
00259 *                                                          *      ELSMEDEL
00260 *        GET SELECTION CODE KEYWORD AREA                   *      ELSMEDEL
00261 *                                                          *      ELSMEDEL
00262 ************************************************************      ELSMEDEL
00263  GET-SELECTION-CODE-KEYWORD-ARE.                                  ELSMEDEL
00264      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSMEDEL
00265      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +      ELSMEDEL
00266              (LENGTH OF MSO-MENU-OPT *                            ELSMEDEL
00267          WS-NUMBER-CODES).                                        ELSMEDEL
00268      SET CIA-STG-GETMAIN TO TRUE.                                 ELSMEDEL
00269      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSMEDEL
00270      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSMEDEL
00271      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEDEL
00272          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSMEDEL
00273                                                                   ELSMEDEL
00274                                                                   ELSMEDEL
00275 ************************************************************      ELSMEDEL
00276 *                                                          *      ELSMEDEL
00277 *        ALLOCATE MENU AREA                                *      ELSMEDEL
00278 *                                                          *      ELSMEDEL
00279 ************************************************************      ELSMEDEL
00280  ALLOCATE-MENU-AREA.                                              ELSMEDEL
00281      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSMEDEL
00282      SET  CIA-STG-GETMAIN TO TRUE.                                ELSMEDEL
00283      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSMEDEL
00284      EJECT                                                        ELSMEDEL
00285                                                                   ELSMEDEL
00286                                                                   ELSMEDEL
00287 ************************************************************      ELSMEDEL
00288 *                                                          *      ELSMEDEL
00289 *        DEALLOCATE MENU AREA                              *      ELSMEDEL
00290 *                                                          *      ELSMEDEL
00291 ************************************************************      ELSMEDEL
00292  DEALLOCATE-MENU-AREA.                                            ELSMEDEL
00293      SET  CIA-ELSMENU-DDN  TO TRUE.                               ELSMEDEL
00294      SET  CIA-STG-FREEMAIN TO TRUE.                               ELSMEDEL
00295      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSMEDEL
00296                                                                   ELSMEDEL
00297                                                                   ELSMEDEL
00298 ************************************************************      ELSMEDEL
00299 *                                                          *      ELSMEDEL
00300 *        CALL STORAGE SUBPROGRAM                           *      ELSMEDEL
00301 *                                                          *      ELSMEDEL
00302 ************************************************************      ELSMEDEL
00303  CALL-STORAGE-SUBPROGRAM.                                         ELSMEDEL
00304      EXEC CICS LINK PROGRAM ('ELUSTGMG')                          ELSMEDEL
00305                     COMMAREA (DFHCOMMAREA)                        ELSMEDEL
00306                     END-EXEC.                                     ELSMEDEL
00307      EJECT                                                        ELSMEDEL
00308                                                                   ELSMEDEL
00309                                                                   ELSMEDEL
00310 ************************************************************      ELSMEDEL
00311 *                                                          *      ELSMEDEL
00312 *        BUILD MENU HEADERS                                *      ELSMEDEL
00313 *                                                          *      ELSMEDEL
00314 ************************************************************      ELSMEDEL
00315  BUILD-MENU-HEADERS.                                              ELSMEDEL
00316      MOVE WS-MEDICARE-TITLE TO SSB-MNU-TITLE.                     ELSMEDEL
00317      MOVE WS-NUMBER-HEADINGS TO MHD-NBR-HDG-LINES.                ELSMEDEL
00318      SET MHD-IDX TO 1.                                            ELSMEDEL
00319      PERFORM LOAD-MENU-HEADINGS                                   ELSMEDEL
00320          VARYING WS-HEAD-INDEX FROM 1 BY 1 UNTIL                  ELSMEDEL
00321                     WS-HEAD-INDEX > WS-NUMBER-HEADINGS.           ELSMEDEL
00322                                                                   ELSMEDEL
00323                                                                   ELSMEDEL
00324 ************************************************************      ELSMEDEL
00325 *                                                          *      ELSMEDEL
00326 *        LOAD MENU HEADINGS                                *      ELSMEDEL
00327 *                                                          *      ELSMEDEL
00328 ************************************************************      ELSMEDEL
00329  LOAD-MENU-HEADINGS.                                              ELSMEDEL
00330      MOVE WS-MEDICARE-HEADING-LINE (WS-HEAD-INDEX) TO             ELSMEDEL
00331                                            MHD-HDG-LINE           ELSMEDEL
00332          (MHD-IDX).                                               ELSMEDEL
00333      SET MHD-IDX UP BY 1.                                         ELSMEDEL
00334      EJECT                                                        ELSMEDEL
00335                                                                   ELSMEDEL
00336                                                                   ELSMEDEL
00337 ************************************************************      ELSMEDEL
00338 *                                                          *      ELSMEDEL
00339 *        BUILD VALID SELECTIONS                            *      ELSMEDEL
00340 *                                                          *      ELSMEDEL
00341 ************************************************************      ELSMEDEL
00342  BUILD-VALID-SELECTIONS.                                          ELSMEDEL
00343      PERFORM LOAD-MENU-OPTS-HEADER.                               ELSMEDEL
00344      SET MSO-IDX TO 1.                                            ELSMEDEL
00345      PERFORM LOAD-TOPIC-CODES-KEYWORD                             ELSMEDEL
00346          VARYING WS-MEDICARE-INDEX FROM 1 BY 1                    ELSMEDEL
00347                  UNTIL WS-MEDICARE-INDEX IS GREATER THAN          ELSMEDEL
00348              WS-NUMBER-CODES.                                     ELSMEDEL
00349                                                                   ELSMEDEL
00350                                                                   ELSMEDEL
00351 ************************************************************      ELSMEDEL
00352 *                                                          *      ELSMEDEL
00353 *        LOAD MENU OPTS HEADER                             *      ELSMEDEL
00354 *                                                          *      ELSMEDEL
00355 ************************************************************      ELSMEDEL
00356  LOAD-MENU-OPTS-HEADER.                                           ELSMEDEL
00357      MOVE WS-NUMBER-CODES TO MSO-NBR-MENU-OPTS.                   ELSMEDEL
00358      MOVE LENGTH OF WS-VALID-SELECT TO MSO-OPT-LEN.               ELSMEDEL
00359      SET MSO-OPT-TYP-AN   TO TRUE.                                ELSMEDEL
00360      MOVE 1   TO MSO-MIN-CHOICES                                  ELSMEDEL
00361                  MSO-MAX-CHOICES.                                 ELSMEDEL
00362                                                                   ELSMEDEL
00363                                                                   ELSMEDEL
00364 ************************************************************      ELSMEDEL
00365 *                                                          *      ELSMEDEL
00366 *        LOAD TOPIC CODES KEYWORD                          *      ELSMEDEL
00367 *                                                          *      ELSMEDEL
00368 ************************************************************      ELSMEDEL
00369  LOAD-TOPIC-CODES-KEYWORD.                                        ELSMEDEL
00370      MOVE WS-VALID-SELECT(WS-MEDICARE-INDEX) TO                   ELSMEDEL
00371           MSO-OPT-SEL(MSO-IDX).                                   ELSMEDEL
00372      MOVE WS-KEYWORD(WS-MEDICARE-INDEX) TO                        ELSMEDEL
00373           MSO-OPT-KWD(MSO-IDX).                                   ELSMEDEL
00374      SET MSO-IDX UP BY 1.                                         ELSMEDEL
00375      EJECT                                                        ELSMEDEL
00376                                                                   ELSMEDEL
00377                                                                   ELSMEDEL
00378 ************************************************************      ELSMEDEL
00379 *                                                          *      ELSMEDEL
00380 *        PROCESS MENU COMPLETED REQUEST                    *      ELSMEDEL
00381 *                                                          *      ELSMEDEL
00382 ************************************************************      ELSMEDEL
00383  PROCESS-MENU-COMPLETED-REQUEST.                                  ELSMEDEL
00384      IF SSB-MNU-CHOICE (1) = 'MEDELIG'                            ELSMEDEL
00385          PERFORM MOVE-YES                                         ELSMEDEL
00386      ELSE                                                         ELSMEDEL
00387          PERFORM MOVE-NO.                                         ELSMEDEL
00388      SET SSB-COMPLETED (SSB-SELECTOR-STATE)     TO                ELSMEDEL
00389          TRUE.                                                    ELSMEDEL
00390                                                                   ELSMEDEL
00391                                                                   ELSMEDEL
00392 ************************************************************      ELSMEDEL
00393 *                                                          *      ELSMEDEL
00394 *        MOVE YES                                          *      ELSMEDEL
00395 *                                                          *      ELSMEDEL
00396 ************************************************************      ELSMEDEL
00397  MOVE-YES.                                                        ELSMEDEL
00398      MOVE 'Y' TO SSB-MEDCA-ELIGY.                                 ELSMEDEL
00399                                                                   ELSMEDEL
00400                                                                   ELSMEDEL
00401 ************************************************************      ELSMEDEL
00402 *                                                          *      ELSMEDEL
00403 *        MOVE NO                                           *      ELSMEDEL
00404 *                                                          *      ELSMEDEL
00405 ************************************************************      ELSMEDEL
00406  MOVE-NO.                                                         ELSMEDEL
00407      MOVE 'N' TO SSB-MEDCA-ELIGY.                                 ELSMEDEL
00408                                                                   ELSMEDEL
00409                                                                   ELSMEDEL
00410 ************************************************************      ELSMEDEL
00411 *                                                          *      ELSMEDEL
00412 *        CHECK COMMAREA LENGTH                             *      ELSMEDEL
00413 *                                                          *      ELSMEDEL
00414 ************************************************************      ELSMEDEL
00415  CHECK-COMMAREA-LENGTH.                                           ELSMEDEL
00416      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELSMEDEL
00417          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELSMEDEL
00418                                                                   ELSMEDEL
00419                                                                   ELSMEDEL
00420 ************************************************************      ELSMEDEL
00421 *                                                          *      ELSMEDEL
00422 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELSMEDEL
00423 *                                                          *      ELSMEDEL
00424 ************************************************************      ELSMEDEL
00425  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELSMEDEL
00426      EXEC CICS ABEND                                              ELSMEDEL
00427                ABCODE('EL01')                                     ELSMEDEL
00428                END-EXEC.                                          ELSMEDEL
00429                                                                   ELSMEDEL
00430                                                                   ELSMEDEL
00431 ************************************************************      ELSMEDEL
00432 *                                                          *      ELSMEDEL
00433 *        CALL INPUT-OUTPUT SUBPROGRAM                      *      ELSMEDEL
00434 *                                                          *      ELSMEDEL
00435 ************************************************************      ELSMEDEL
00436  CALL-INPUT-OUTPUT-SUBPROGRAM.                                    ELSMEDEL
00437      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELSMEDEL
00438                     COMMAREA(DFHCOMMAREA)                         ELSMEDEL
00439                     END-EXEC.                                     ELSMEDEL
00440      EJECT                                                        ELSMEDEL
00441                                                                   ELSMEDEL
00442                                                                   ELSMEDEL
00443 ************************************************************      ELSMEDEL
00444 *                                                          *      ELSMEDEL
00445 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELSMEDEL
00446 *                                                          *      ELSMEDEL
00447 ************************************************************      ELSMEDEL
00448  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELSMEDEL
00449      IF ECA-CIA-PTR IS NOT EQUAL NULL                             ELSMEDEL
00450          PERFORM SET-CIA-ADDRESS                                  ELSMEDEL
00451      ELSE                                                         ELSMEDEL
00452          PERFORM SIGNAL-CIA-ADDRESSING-ERROR.                     ELSMEDEL
00453                                                                   ELSMEDEL
00454                                                                   ELSMEDEL
00455 ************************************************************      ELSMEDEL
00456 *                                                          *      ELSMEDEL
00457 *        SET CIA ADDRESS                                   *      ELSMEDEL
00458 *                                                          *      ELSMEDEL
00459 ************************************************************      ELSMEDEL
00460  SET-CIA-ADDRESS.                                                 ELSMEDEL
00461      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSMEDEL
00462          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSMEDEL
00463                                                                   ELSMEDEL
00464                                                                   ELSMEDEL
00465 ************************************************************      ELSMEDEL
00466 *                                                          *      ELSMEDEL
00467 *        SIGNAL CIA ADDRESSING ERROR                       *      ELSMEDEL
00468 *                                                          *      ELSMEDEL
00469 ************************************************************      ELSMEDEL
00470  SIGNAL-CIA-ADDRESSING-ERROR.                                     ELSMEDEL
00471      EXEC CICS ABEND                                              ELSMEDEL
00472                ABCODE('EL02')                                     ELSMEDEL
00473                END-EXEC.                                          ELSMEDEL
00474      EJECT                                                        ELSMEDEL
00475                                                                   ELSMEDEL
00476                                                                   ELSMEDEL
00477 ************************************************************      ELSMEDEL
00478 *                                                          *      ELSMEDEL
00479 *        ESTABLISH ADDRESSING TO SELECTOR CONTROL AREA     *      ELSMEDEL
00480 *                                                          *      ELSMEDEL
00481 ************************************************************      ELSMEDEL
00482  ESTABLISH-ADDRESSING-TO-SELECT.                                  ELSMEDEL
00483      PERFORM SET-SSCB-ADDRESS.                                    ELSMEDEL
00484      IF CIA-RC-PTR-NULL                                           ELSMEDEL
00485         PERFORM SIGNAL-MISSING-PARAMETER.                         ELSMEDEL
00486                                                                   ELSMEDEL
00487                                                                   ELSMEDEL
00488 ************************************************************      ELSMEDEL
00489 *                                                          *      ELSMEDEL
00490 *        SET SSCB ADDRESS                                  *      ELSMEDEL
00491 *                                                          *      ELSMEDEL
00492 ************************************************************      ELSMEDEL
00493  SET-SSCB-ADDRESS.                                                ELSMEDEL
00494      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSMEDEL
00495      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEDEL
00496          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSMEDEL
00497                                                                   ELSMEDEL
00498                                                                   ELSMEDEL
00499 ************************************************************      ELSMEDEL
00500 *                                                          *      ELSMEDEL
00501 *        SIGNAL MISSING PARAMETER                          *      ELSMEDEL
00502 *                                                          *      ELSMEDEL
00503 ************************************************************      ELSMEDEL
00504  SIGNAL-MISSING-PARAMETER.                                        ELSMEDEL
00505      SET CIA-AB-PARM-MISSING TO TRUE.                             ELSMEDEL
00506      EXEC CICS ABEND ABCODE(CIA-ABCODE)                           ELSMEDEL
00507                END-EXEC.                                          ELSMEDEL
00508      EJECT                                                        ELSMEDEL
00509                                                                   ELSMEDEL
00510                                                                   ELSMEDEL
00511 ************************************************************      ELSMEDEL
00512 *                                                          *      ELSMEDEL
00513 *        SIGNAL INVALID MEDICARE ELIGIBILITY REQUEST       *      ELSMEDEL
00514 *                                                          *      ELSMEDEL
00515 ************************************************************      ELSMEDEL
00516  SIGNAL-INVALID-MEDICARE-ELIGIB.                                  ELSMEDEL
00517      SET CIA-AB-UNDEF TO TRUE.                                    ELSMEDEL
00518      EXEC CICS ABEND                                              ELSMEDEL
00519                ABCODE(CIA-ABCODE)                                 ELSMEDEL
00520                END-EXEC.                                          ELSMEDEL
00521      EJECT                                                        ELSMEDEL
00522  GOBACK-PARAGRAPH.                                                ELSMEDEL
00523 ************************************************************      ELSMEDEL
00524 *                                                          *      ELSMEDEL
00525 *                         STOP RUN                         *      ELSMEDEL
00526 *                                                          *      ELSMEDEL
00527 ************************************************************      ELSMEDEL
00528      GOBACK.                                                      ELSMEDEL
