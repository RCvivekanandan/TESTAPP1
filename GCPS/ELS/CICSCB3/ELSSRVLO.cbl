00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSSRVLO
00003  PROGRAM-ID.         ELSSRVLO.                                       LV002
00004                                                                   ELSSRVLO
00005  AUTHOR.             JOHN CURIN,  KEANE, INC.                     ELSSRVLO
00006                                                                   ELSSRVLO
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSSRVLO
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSSRVLO
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSSRVLO
00010                      233 N. MICHIGAN AVE                          ELSSRVLO
00011                      CHICAGO, ILLINOIS 60601                      ELSSRVLO
00012                                                                   ELSSRVLO
00013  DATE-WRITTEN.       31-OCT-1986.                                 ELSSRVLO
00014                                                                   ELSSRVLO
00015  DATE-COMPILED.                                                   ELSSRVLO
00016                                                                   ELSSRVLO
00017  SECURITY.           COPYRIGHT 1986,                              ELSSRVLO
00018                      HEALTH CARE SERVICE CORPORATION              ELSSRVLO
00019      SKIP3                                                        ELSSRVLO
00020 ******************************************************************ELSSRVLO
00021 *                                                                *ELSSRVLO
00022 *    PROGRAM:    ELSSRVLO                                        *ELSSRVLO
00023 *    DATE:       31-OCT-1986                                     *ELSSRVLO
00024 *    AUTHOR:     JOHN CURIN                                      *ELSSRVLO
00025 *    FUNCTION:   SERVICE LOCATION SELECTOR PROGRAM               *ELSSRVLO
00026 *                                                                *ELSSRVLO
00027 *                                                                *ELSSRVLO
00028 ******************************************************************ELSSRVLO
00029 *                                                                *ELSSRVLO
00030 *                      MAINTENANCE HISTORY                       *ELSSRVLO
00031 *                                                                *ELSSRVLO
00032 *  MOD     DATE     BY  DRPT                ACTION               *ELSSRVLO
00033 * ----- ----------- --- ----- ---------------------------------- *ELSSRVLO
00034 * 01.00 31-OCT-1986 JTC       CREATED                            *ELSSRVLO
00035 *                                                                *ELSSRVLO
00036 * 01.01 22-SEP-1987 REB       ISSUE ABEND CODES OF 'EL01' IF AN  *ELSSRVLO
00037 *                             INVALID COMMAREA AND 'EL02' IF THE *ELSSRVLO
00038 *                             CIA BLOCK IS NOT PRESENT.          *ELSSRVLO
00039 *                                                                *ELSSRVLO
00040 * 01.02 22-AUG-1989 EGL       DESTRUCTED PROGRAM AND ADDED NEW   *ELSSRVLO
00041 *                             STORAGE MANAGEMENT ROUTINES.       *ELSSRVLO
00042 *                                                                *ELSSRVLO
00043 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST    *ELSSRVLO
00044 *                                                                *ELSSRVLO
00045 ******************************************************************ELSSRVLO
00046                                                                   ELSSRVLO
00047  ENVIRONMENT DIVISION.                                            ELSSRVLO
00048                                                                   ELSSRVLO
00049  CONFIGURATION SECTION.                                           ELSSRVLO
00050  SOURCE-COMPUTER.    IBM-3033.                                    ELSSRVLO
00051  OBJECT-COMPUTER.    IBM-3033.                                    ELSSRVLO
00052      EJECT                                                        ELSSRVLO
00053  DATA DIVISION.                                                   ELSSRVLO
00054                                                                   ELSSRVLO
00055  FILE SECTION.                                                    ELSSRVLO
00056                                                                   ELSSRVLO
00057  WORKING-STORAGE SECTION.                                         ELSSRVLO
00058                                                                   ELSSRVLO
00059 *** SERVICE LOCATION WORK AREA                                    ELSSRVLO
00060                                                                   ELSSRVLO
00061  01  WS-TABLE-COUNTS.                                             ELSSRVLO
00062      05  WS-NUMBER-CODES     PIC S9(4) COMP    VALUE +3.          ELSSRVLO
00063      05  WS-NUMBER-HEADINGS PIC S9(4) COMP     VALUE +15.         ELSSRVLO
00064                                                                   ELSSRVLO
00065  01  WS-SER-LOC-TITLE          PIC X(33)            VALUE         ELSSRVLO
00066      'SELECT BASIS OF SERVICE'.                                   ELSSRVLO
00067                                                                   ELSSRVLO
00068  01  DEFAULT-VALUE             PIC X(16)            VALUE         ELSSRVLO
00069      'B               '.                                          ELSSRVLO
00070                                                                   ELSSRVLO
00071  01  WS-SER-LOC-HEADINGS.                                         ELSSRVLO
00072      05  FILLER                PIC X(78)            VALUE         ELSSRVLO
00073      'BENEFITS FOR THIS TOPIC ARE CATEGORIZED ACCORDING TO WHETHERELSSRVLO
00074 -    ' THE SERVICES ARE '.                                        ELSSRVLO
00075      05  FILLER                PIC X(78)            VALUE         ELSSRVLO
00076      'PERFORMED ON AN INPATIENT OR AN OUTPATIENT BASIS.  INDICATE ELSSRVLO
00077 -    'WHICH CATEGORY YOU'.                                        ELSSRVLO
00078      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00079      'WISH TO SEE.                                                ELSSRVLO
00080 -    '                  '.                                        ELSSRVLO
00081      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00082      '                                                            ELSSRVLO
00083 -    '                  '.                                        ELSSRVLO
00084      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00085      '                                                            ELSSRVLO
00086 -    '                  '.                                        ELSSRVLO
00087      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00088      '  I INPATIENT                                               ELSSRVLO
00089 -    '                  '.                                        ELSSRVLO
00090      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00091      '                                                            ELSSRVLO
00092 -    '                  '.                                        ELSSRVLO
00093      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00094      '  O OUTPATIENT                                              ELSSRVLO
00095 -    '                  '.                                        ELSSRVLO
00096      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00097      '                                                            ELSSRVLO
00098 -    '                  '.                                        ELSSRVLO
00099      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00100      '  B BOTH                                                    ELSSRVLO
00101 -    '                  '.                                        ELSSRVLO
00102      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00103      '                                                            ELSSRVLO
00104 -    '                  '.                                        ELSSRVLO
00105      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00106      'IF YOU DO NOT ENTER A CHOICE, BOTH INPATIENT AND OUTPATIENT ELSSRVLO
00107 -    'BENEFITS WILL BE  '.                                        ELSSRVLO
00108      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00109      'DISPLAYED.                                                  ELSSRVLO
00110 -    '                  '.                                        ELSSRVLO
00111      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00112      '                                                            ELSSRVLO
00113 -    '                  '.                                        ELSSRVLO
00114      05  FILLER                 PIC X(78)            VALUE        ELSSRVLO
00115      '                                                            ELSSRVLO
00116 -    '                  '.                                        ELSSRVLO
00117  01  WS-SER-LOC-HEAD REDEFINES WS-SER-LOC-HEADINGS.               ELSSRVLO
00118      05  WS-SER-LOC-HEADING-LINE                                  ELSSRVLO
00119                      OCCURS 15 TIMES                              ELSSRVLO
00120                      INDEXED BY WS-HEAD-INDEX                     ELSSRVLO
00121                          PIC X(78).                               ELSSRVLO
00122                                                                   ELSSRVLO
00123  01  WS-SER-LOC-VALID-DEFINITION.                                 ELSSRVLO
00124      05  FILLER                 PIC X(17)            VALUE        ELSSRVLO
00125      'IIP              '.                                         ELSSRVLO
00126      05  FILLER                 PIC X(17)            VALUE        ELSSRVLO
00127      'OOP              '.                                         ELSSRVLO
00128      05  FILLER                 PIC X(17)            VALUE        ELSSRVLO
00129      'BBOTH            '.                                         ELSSRVLO
00130  01  WS-SER-LOC-TABLE REDEFINES WS-SER-LOC-VALID-DEFINITION.      ELSSRVLO
00131      05  WS-SER-LOC-INFO                                          ELSSRVLO
00132                      OCCURS 3 TIMES                               ELSSRVLO
00133                      INDEXED BY WS-SER-LOC-INDEX.                 ELSSRVLO
00134          10  WS-VALID-SELECT PIC X.                               ELSSRVLO
00135          10  WS-KEYWORD       PIC X(16).                          ELSSRVLO
00136                                                                   ELSSRVLO
00137                                                                   ELSSRVLO
00138      EJECT                                                        ELSSRVLO
00139  LINKAGE SECTION.                                                 ELSSRVLO
00140                                                                   ELSSRVLO
00141  01  DFHCOMMAREA.                                                 ELSSRVLO
00142      COPY ELSCOMMC.                                               ELSSRVLO
00143      EJECT                                                        ELSSRVLO
00144      COPY ELSCIA2C.                                               ELSSRVLO
00145      EJECT                                                        ELSSRVLO
00146      COPY ELSIOPMC.                                               ELSSRVLO
00147      EJECT                                                        ELSSRVLO
00148      COPY ELSSSCBC.                                               ELSSRVLO
00149      EJECT                                                        ELSSRVLO
00150      COPY ELSMHDGC.                                               ELSSRVLO
00151      EJECT                                                        ELSSRVLO
00152      COPY ELSMOPTC.                                               ELSSRVLO
00153      EJECT                                                        ELSSRVLO
00154  PROCEDURE DIVISION.                                              ELSSRVLO
00155 ************************************************************      ELSSRVLO
00156 *                                                          *      ELSSRVLO
00157 *        PERFORM SERVICE LOCATION FUNCTIONS                *      ELSSRVLO
00158 *                                                          *      ELSSRVLO
00159 ************************************************************      ELSSRVLO
00160  PERFORM-SERVICE-LOCATION-FUNCT.                                  ELSSRVLO
00161      PERFORM INITIALIZE-MODULE.                                   ELSSRVLO
00162      PERFORM PROCESS-SERVICE-LOCATION-REQUE.                      ELSSRVLO
00163      EXEC CICS  RETURN                                            ELSSRVLO
00164                 END-EXEC.                                         ELSSRVLO
00165      GOBACK.                                                      ELSSRVLO
00166                                                                   ELSSRVLO
00167 ************************************************************      ELSSRVLO
00168 *                                                          *      ELSSRVLO
00169 *        INITIALIZE MODULE                                 *      ELSSRVLO
00170 *                                                          *      ELSSRVLO
00171 ************************************************************      ELSSRVLO
00172  INITIALIZE-MODULE.                                               ELSSRVLO
00173      PERFORM CHECK-COMMAREA-LENGTH.                               ELSSRVLO
00174      PERFORM ESTABLISH-ADDRESSING-TO-CIA.                         ELSSRVLO
00175      PERFORM ESTABLISH-ADDRESSING-TO-SSB.                         ELSSRVLO
00176                                                                   ELSSRVLO
00177  CHECK-COMMAREA-LENGTH.                                           ELSSRVLO
00178      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELSSRVLO
00179          EXEC CICS ABEND                                          ELSSRVLO
00180                    ABCODE('EL01')                                 ELSSRVLO
00181                    END-EXEC.                                      ELSSRVLO
00182                                                                   ELSSRVLO
00183  ESTABLISH-ADDRESSING-TO-CIA.                                     ELSSRVLO
00184      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSSRVLO
00185                 ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.         ELSSRVLO
00186                                                                   ELSSRVLO
00187  ESTABLISH-ADDRESSING-TO-SSB.                                     ELSSRVLO
00188      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSSRVLO
00189      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSRVLO
00190                 ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.           ELSSRVLO
00191      IF CIA-RC-PTR-NULL                                           ELSSRVLO
00192          SET CIA-AB-PARM-MISSING TO TRUE                          ELSSRVLO
00193          EXEC CICS ABEND ABCODE(CIA-ABCODE)                       ELSSRVLO
00194                    END-EXEC.                                      ELSSRVLO
00195 /***********************************************************      ELSSRVLO
00196 *                                                          *      ELSSRVLO
00197 *        PROCESS SERVICE LOCATION REQUEST                  *      ELSSRVLO
00198 *                                                          *      ELSSRVLO
00199 ************************************************************      ELSSRVLO
00200  PROCESS-SERVICE-LOCATION-REQUE.                                  ELSSRVLO
00201      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSSRVLO
00202          PERFORM PROCESS-BUILD-MENU-REQUEST                       ELSSRVLO
00203      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSSRVLO
00204          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSSRVLO
00205      ELSE                                                         ELSSRVLO
00206          PERFORM SIGNAL-INVALID-SERVICE-LOCATIO.                  ELSSRVLO
00207                                                                   ELSSRVLO
00208                                                                   ELSSRVLO
00209 ************************************************************      ELSSRVLO
00210 *                                                          *      ELSSRVLO
00211 *        PROCESS BUILD MENU REQUEST                        *      ELSSRVLO
00212 *                                                          *      ELSSRVLO
00213 ************************************************************      ELSSRVLO
00214  PROCESS-BUILD-MENU-REQUEST.                                      ELSSRVLO
00215      MOVE DEFAULT-VALUE TO SSB-MNU-CHOICE (1).                    ELSSRVLO
00216      PERFORM DELETE-MENU-FILE.                                    ELSSRVLO
00217      PERFORM ACQUIRE-STORAGE-AREAS.                               ELSSRVLO
00218      PERFORM BUILD-MENU-HEADERS.                                  ELSSRVLO
00219      PERFORM BUILD-VALID-SELECTIONS.                              ELSSRVLO
00220      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSSRVLO
00221 /***********************************************************      ELSSRVLO
00222 *                                                          *      ELSSRVLO
00223 *        DELETE MENU FILE                                  *      ELSSRVLO
00224 *                                                          *      ELSSRVLO
00225 ************************************************************      ELSSRVLO
00226  DELETE-MENU-FILE.                                                ELSSRVLO
00227      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSRVLO
00228      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSRVLO
00229                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELSSRVLO
00230      IF CIA-RC-PTR-NULL                                           ELSSRVLO
00231          PERFORM ALLOCATE-MENU-AREA.                              ELSSRVLO
00232      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSSRVLO
00233      SET IOP-DEL          TO TRUE.                                ELSSRVLO
00234      SET IOP-FCQ-NONE     TO TRUE.                                ELSSRVLO
00235      SET IOP-KVQ-NONE     TO TRUE.                                ELSSRVLO
00236      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSSRVLO
00237      PERFORM DEALLOCATE-MENU-AREA.                                ELSSRVLO
00238                                                                   ELSSRVLO
00239  ALLOCATE-MENU-AREA.                                              ELSSRVLO
00240      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSRVLO
00241      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSRVLO
00242      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSRVLO
00243      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSRVLO
00244      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSRVLO
00245                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELSSRVLO
00246                                                                   ELSSRVLO
00247  DEALLOCATE-MENU-AREA.                                            ELSSRVLO
00248      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSRVLO
00249      SET CIA-STG-FREEMAIN TO TRUE.                                ELSSRVLO
00250      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSRVLO
00251 /***********************************************************      ELSSRVLO
00252 *                                                          *      ELSSRVLO
00253 *        ACQUIRE STORAGE AREAS                             *      ELSSRVLO
00254 *                                                          *      ELSSRVLO
00255 ************************************************************      ELSSRVLO
00256  ACQUIRE-STORAGE-AREAS.                                           ELSSRVLO
00257      PERFORM GET-HEADING-STORAGE-AREA.                            ELSSRVLO
00258      PERFORM GET-SELECTION-CODE-KEYWORD-ARE.                      ELSSRVLO
00259                                                                   ELSSRVLO
00260  GET-HEADING-STORAGE-AREA.                                        ELSSRVLO
00261      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSSRVLO
00262      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +         ELSSRVLO
00263               (LENGTH OF MHD-HDG-LINE * WS-NUMBER-HEADINGS).      ELSSRVLO
00264      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSRVLO
00265      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSRVLO
00266      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSSRVLO
00267      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSRVLO
00268                 ADDRESS OF MHD-MENU-HEADINGS.                     ELSSRVLO
00269                                                                   ELSSRVLO
00270  GET-SELECTION-CODE-KEYWORD-ARE.                                  ELSSRVLO
00271      SET  CIA-ELSMOPT-DDN TO TRUE.                                ELSSRVLO
00272      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +      ELSSRVLO
00273              (LENGTH OF MSO-MENU-OPT * WS-NUMBER-CODES).          ELSSRVLO
00274      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSRVLO
00275      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSRVLO
00276      SET  CIA-ELSMOPT-DDN TO TRUE.                                ELSSRVLO
00277      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSRVLO
00278                 ADDRESS OF MSO-MENU-SELECTION-VALUES.             ELSSRVLO
00279                                                                   ELSSRVLO
00280  CALL-STORAGE-SUBPROGRAM.                                         ELSSRVLO
00281      EXEC CICS LINK PROGRAM ('ELUSTGMG')                          ELSSRVLO
00282                     COMMAREA (DFHCOMMAREA)                        ELSSRVLO
00283                     END-EXEC.                                     ELSSRVLO
00284 /***********************************************************      ELSSRVLO
00285 *                                                          *      ELSSRVLO
00286 *        BUILD MENU HEADERS                                *      ELSSRVLO
00287 *                                                          *      ELSSRVLO
00288 ************************************************************      ELSSRVLO
00289  BUILD-MENU-HEADERS.                                              ELSSRVLO
00290      MOVE WS-SER-LOC-TITLE TO SSB-MNU-TITLE.                      ELSSRVLO
00291      MOVE WS-NUMBER-HEADINGS TO MHD-NBR-HDG-LINES.                ELSSRVLO
00292      SET MHD-IDX TO 1.                                            ELSSRVLO
00293      PERFORM                                                      ELSSRVLO
00294          VARYING WS-HEAD-INDEX FROM 1 BY 1 UNTIL                  ELSSRVLO
00295                     WS-HEAD-INDEX > WS-NUMBER-HEADINGS            ELSSRVLO
00296          MOVE WS-SER-LOC-HEADING-LINE (WS-HEAD-INDEX) TO          ELSSRVLO
00297                          MHD-HDG-LINE (MHD-IDX)                   ELSSRVLO
00298          SET MHD-IDX UP BY 1                                      ELSSRVLO
00299      END-PERFORM.                                                 ELSSRVLO
00300                                                                   ELSSRVLO
00301                                                                   ELSSRVLO
00302 ************************************************************      ELSSRVLO
00303 *                                                          *      ELSSRVLO
00304 *        BUILD VALID SELECTIONS                            *      ELSSRVLO
00305 *                                                          *      ELSSRVLO
00306 ************************************************************      ELSSRVLO
00307  BUILD-VALID-SELECTIONS.                                          ELSSRVLO
00308      MOVE WS-NUMBER-CODES TO MSO-NBR-MENU-OPTS.                   ELSSRVLO
00309      MOVE 1   TO MSO-MIN-CHOICES                                  ELSSRVLO
00310                  MSO-MAX-CHOICES.                                 ELSSRVLO
00311      MOVE LENGTH OF WS-VALID-SELECT TO MSO-OPT-LEN.               ELSSRVLO
00312      SET MSO-OPT-TYP-AN   TO TRUE.                                ELSSRVLO
00313                                                                   ELSSRVLO
00314      SET MSO-IDX TO 1.                                            ELSSRVLO
00315      PERFORM                                                      ELSSRVLO
00316          VARYING WS-SER-LOC-INDEX FROM 1 BY 1                     ELSSRVLO
00317                  UNTIL WS-SER-LOC-INDEX > WS-NUMBER-CODES         ELSSRVLO
00318          MOVE WS-VALID-SELECT(WS-SER-LOC-INDEX) TO                ELSSRVLO
00319               MSO-OPT-SEL(MSO-IDX)                                ELSSRVLO
00320          MOVE WS-KEYWORD(WS-SER-LOC-INDEX) TO                     ELSSRVLO
00321               MSO-OPT-KWD(MSO-IDX)                                ELSSRVLO
00322          SET MSO-IDX UP BY 1                                      ELSSRVLO
00323      END-PERFORM.                                                 ELSSRVLO
00324 /***********************************************************      ELSSRVLO
00325 *                                                          *      ELSSRVLO
00326 *        PROCESS MENU COMPLETED REQUEST                    *      ELSSRVLO
00327 *                                                          *      ELSSRVLO
00328 ************************************************************      ELSSRVLO
00329  PROCESS-MENU-COMPLETED-REQUEST.                                  ELSSRVLO
00330      MOVE SSB-MNU-CHOICE (1) TO SSB-SERVICE-CLASS.                ELSSRVLO
00331      SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.              ELSSRVLO
00332                                                                   ELSSRVLO
00333                                                                   ELSSRVLO
00334 ************************************************************      ELSSRVLO
00335 *                                                          *      ELSSRVLO
00336 *        CALL INPUT-OUTPUT SUBPROGRAM                      *      ELSSRVLO
00337 *                                                          *      ELSSRVLO
00338 ************************************************************      ELSSRVLO
00339  CALL-INPUT-OUTPUT-SUBPROGRAM.                                    ELSSRVLO
00340      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELSSRVLO
00341                     COMMAREA(DFHCOMMAREA)                         ELSSRVLO
00342                     END-EXEC.                                     ELSSRVLO
00343                                                                   ELSSRVLO
00344                                                                   ELSSRVLO
00345 ************************************************************      ELSSRVLO
00346 *                                                          *      ELSSRVLO
00347 *        SIGNAL INVALID SERVICE LOCATION REQUEST           *      ELSSRVLO
00348 *                                                          *      ELSSRVLO
00349 ************************************************************      ELSSRVLO
00350  SIGNAL-INVALID-SERVICE-LOCATIO.                                  ELSSRVLO
00351      SET CIA-AB-UNDEF TO TRUE.                                    ELSSRVLO
00352      EXEC CICS ABEND                                              ELSSRVLO
00353                ABCODE(CIA-ABCODE)                                 ELSSRVLO
00354                END-EXEC.                                          ELSSRVLO
