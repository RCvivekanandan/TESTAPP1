00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPSUMRY
00003  PROGRAM-ID.         ELPSUMRY.                                       LV001
00004                                                                   ELPSUMRY
00005  AUTHOR.             EDWARD G LISS.                               ELPSUMRY
00006                                                                   ELPSUMRY
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPSUMRY
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPSUMRY
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPSUMRY
00010                      233 N. MICHIGAN AVE                          ELPSUMRY
00011                      CHICAGO, ILLINOIS 60601                      ELPSUMRY
00012                                                                   ELPSUMRY
00013  DATE-WRITTEN.       22-SEP-1989.                                 ELPSUMRY
00014                                                                   ELPSUMRY
00015  DATE-COMPILED.                                                   ELPSUMRY
00016                                                                   ELPSUMRY
00017  SECURITY.           COPYRIGHT 1989,                              ELPSUMRY
00018                      HEALTH CARE SERVICE CORPORATION              ELPSUMRY
00019  TITLE 'ELS ABEND PROCESSING - PRINT ABEND SUMMARY REPORT'.       ELPSUMRY
00020 ******************************************************************ELPSUMRY
00021 *                                                                *ELPSUMRY
00022 *                    ELS ABEND PROCESSING                        *ELPSUMRY
00023 *                                                                *ELPSUMRY
00024 *   THIS SUBROUTINE PRINTS THE ABEND SUMMARY REPORT.  ONLY 1     *ELPSUMRY
00025 *   CALL IS REQUIRED TO PRINT THE ENTIRE SUMMARY FILE.           *ELPSUMRY
00026 *                                                                *ELPSUMRY
00027 *   THE INPUT CONSISTS OF ONLY THE HEADER RECORDS FROM THE       *ELPSUMRY
00028 *   ELS SNAPSHOT FILE.                                           *ELPSUMRY
00029 *                                                                *ELPSUMRY
00030 ******************************************************************ELPSUMRY
00031 *                                                                *ELPSUMRY
00032 *                      MAINTENANCE HISTORY                       *ELPSUMRY
00033 *                                                                *ELPSUMRY
00034 *  MOD     DATE     BY  DRPT                ACTION               *ELPSUMRY
00035 * ----- ----------- --- ----- ---------------------------------- *ELPSUMRY
00036 * 01.00 22-SEP-1989 EGL       CREATED.                           *ELPSUMRY
00037 *                                                                *ELPSUMRY
00038 ******************************************************************ELPSUMRY
00039                                                                   ELPSUMRY
00040  ENVIRONMENT DIVISION.                                            ELPSUMRY
00041                                                                   ELPSUMRY
00042  CONFIGURATION SECTION.                                           ELPSUMRY
00043  SOURCE-COMPUTER.    IBM-3090.                                    ELPSUMRY
00044  OBJECT-COMPUTER.    IBM-3090.                                    ELPSUMRY
00045                                                                   ELPSUMRY
00046  INPUT-OUTPUT SECTION.                                            ELPSUMRY
00047  FILE-CONTROL.                                                    ELPSUMRY
00048       SELECT SUMMARY-FILE  ASSIGN TO UT-S-SUMMARY.                ELPSUMRY
00049     EJECT                                                         ELPSUMRY
00050  DATA DIVISION.                                                   ELPSUMRY
00051                                                                   ELPSUMRY
00052  FILE SECTION.                                                    ELPSUMRY
00053  FD  SUMMARY-FILE                                                 ELPSUMRY
00054      BLOCK CONTAINS 0 RECORDS                                     ELPSUMRY
00055      LABEL RECORDS ARE STANDARD                                   ELPSUMRY
00056      RECORDING MODE IS V.                                         ELPSUMRY
00057      COPY ELSNAPSC.                                               ELPSUMRY
00058 /                                                                 ELPSUMRY
00059  WORKING-STORAGE SECTION.                                         ELPSUMRY
00060  01  FILLER           PIC X(18)   VALUE '*START OF ELPSUMRY'.     ELPSUMRY
00061  01  WS-MISC-STUFF.                                               ELPSUMRY
00062      05  WS-EOF-SW    PIC X       VALUE 'N'.                      ELPSUMRY
00063          88  WS-EOF               VALUE 'Y'.                      ELPSUMRY
00064      05  WS-TOTAL           PIC S9(5)  COMP-3 VALUE ZERO.         ELPSUMRY
00065      05  WS-TOTAL-SSD       PIC S9(5)  COMP-3 VALUE ZERO.         ELPSUMRY
00066      05  WS-TOTAL-SD        PIC S9(5)  COMP-3 VALUE ZERO.         ELPSUMRY
00067      05  WS-TOTAL-SSD-SD    PIC S9(5)  COMP-3 VALUE ZERO.         ELPSUMRY
00068      05  WS-JULIAN-DATE            PIC 9(5).                      ELPSUMRY
00069      05  WS-GREGORIAN-DATE         PIC 9(6).                      ELPSUMRY
00070  01  OUTPUT-LINES.                                                ELPSUMRY
00071      03  HEADING-LINE-1.                                          ELPSUMRY
00072          05  FILLER                PIC X(01)  VALUE '1'.          ELPSUMRY
00073          05  FILLER                PIC X(38)  VALUE SPACES.       ELPSUMRY
00074          05  FILLER                PIC X(51)  VALUE               ELPSUMRY
00075       'S U M M A R Y   A N D   R O U T I N G   R E P O R T'.      ELPSUMRY
00076          05  FILLER                PIC X(43)  VALUE SPACES.       ELPSUMRY
00077      03  HEADING-LINE-2.                                          ELPSUMRY
00078          05  FILLER                PIC X(01)  VALUE '-'.          ELPSUMRY
00079          05  FILLER                PIC X(12)  VALUE SPACES.       ELPSUMRY
00080          05  FILLER                PIC X(05)  VALUE 'ABEND'.      ELPSUMRY
00081          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00082          05  FILLER                PIC X(04)  VALUE 'TERM'.       ELPSUMRY
00083          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00084          05  FILLER                PIC X(05)  VALUE 'ABEND'.      ELPSUMRY
00085          05  FILLER                PIC X(06)  VALUE SPACES.       ELPSUMRY
00086          05  FILLER                PIC X(05)  VALUE 'ABEND'.      ELPSUMRY
00087          05  FILLER                PIC X(05)  VALUE SPACES.       ELPSUMRY
00088          05  FILLER                PIC X(06)  VALUE 'SYSTEM'.     ELPSUMRY
00089          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00090          05  FILLER                PIC X(06)  VALUE 'APPLID'.     ELPSUMRY
00091          05  FILLER                PIC X(04)  VALUE SPACES.       ELPSUMRY
00092          05  FILLER                PIC X(05)  VALUE 'FACI-'.      ELPSUMRY
00093          05  FILLER                PIC X(02)  VALUE SPACES.       ELPSUMRY
00094          05  FILLER                PIC X(22)  VALUE               ELPSUMRY
00095              '.........USER.........'.                            ELPSUMRY
00096          05  FILLER                PIC X(04)  VALUE SPACES.       ELPSUMRY
00097          05  FILLER                PIC X(04)  VALUE 'TASK'.       ELPSUMRY
00098          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00099          05  FILLER                PIC X(09)  VALUE 'ROUTED TO'.  ELPSUMRY
00100          05  FILLER                PIC X(16)  VALUE SPACES.       ELPSUMRY
00101      03  HEADING-LINE-3.                                          ELPSUMRY
00102          05  FILLER                PIC X(01)  VALUE ' '.          ELPSUMRY
00103          05  FILLER                PIC X(13)  VALUE SPACES.       ELPSUMRY
00104          05  FILLER                PIC X(04)  VALUE 'CODE'.       ELPSUMRY
00105          05  FILLER                PIC X(04)  VALUE SPACES.       ELPSUMRY
00106          05  FILLER                PIC X(02)  VALUE 'ID'.         ELPSUMRY
00107          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00108          05  FILLER                PIC X(08)  VALUE '..DATE..'.   ELPSUMRY
00109          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00110          05  FILLER                PIC X(08)  VALUE '..TIME..'.   ELPSUMRY
00111          05  FILLER                PIC X(05)  VALUE SPACES.       ELPSUMRY
00112          05  FILLER                PIC X(02)  VALUE 'ID'.         ELPSUMRY
00113          05  FILLER                PIC X(05)  VALUE SPACES.       ELPSUMRY
00114          05  FILLER                PIC X(06)  VALUE 'REGION'.     ELPSUMRY
00115          05  FILLER                PIC X(04)  VALUE SPACES.       ELPSUMRY
00116          05  FILLER                PIC X(04)  VALUE 'LITY'.       ELPSUMRY
00117          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00118          05  FILLER                PIC X(05)  VALUE 'INIT.'.      ELPSUMRY
00119          05  FILLER                PIC X(02)  VALUE SPACES.       ELPSUMRY
00120          05  FILLER                PIC X(02)  VALUE 'ID'.         ELPSUMRY
00121          05  FILLER                PIC X(08)  VALUE SPACES.       ELPSUMRY
00122          05  FILLER                PIC X(05)  VALUE 'DEPT.'.      ELPSUMRY
00123          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00124          05  FILLER                PIC X(06)  VALUE 'NUMBER'.     ELPSUMRY
00125          05  FILLER                PIC X(02)  VALUE SPACES.       ELPSUMRY
00126          05  FILLER                PIC X(03)  VALUE 'SSD'.        ELPSUMRY
00127          05  FILLER                PIC X(04)  VALUE SPACES.       ELPSUMRY
00128          05  FILLER                PIC X(02)  VALUE 'SD'.         ELPSUMRY
00129          05  FILLER                PIC X(16)  VALUE SPACES.       ELPSUMRY
00130      03  HEADING-LINE-4.                                          ELPSUMRY
00131          05  FILLER                PIC X(01)  VALUE ' '.          ELPSUMRY
00132          05  FILLER                PIC X(13)  VALUE SPACES.       ELPSUMRY
00133          05  FILLER                PIC X(04)  VALUE '===='.       ELPSUMRY
00134          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00135          05  FILLER                PIC X(04)  VALUE '===='.       ELPSUMRY
00136          05  FILLER                PIC X(02)  VALUE SPACES.       ELPSUMRY
00137          05  FILLER                PIC X(08)  VALUE '========'.   ELPSUMRY
00138          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00139          05  FILLER                PIC X(08)  VALUE '========'.   ELPSUMRY
00140          05  FILLER                PIC X(04)  VALUE SPACES.       ELPSUMRY
00141          05  FILLER                PIC X(04)  VALUE '===='.       ELPSUMRY
00142          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00143          05  FILLER                PIC X(08)  VALUE '========'.   ELPSUMRY
00144          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00145          05  FILLER                PIC X(04)  VALUE '===='.       ELPSUMRY
00146          05  FILLER                PIC X(04)  VALUE SPACES.       ELPSUMRY
00147          05  FILLER                PIC X(03)  VALUE '==='.        ELPSUMRY
00148          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00149          05  FILLER                PIC X(08)  VALUE '========'.   ELPSUMRY
00150          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00151          05  FILLER                PIC X(03)  VALUE '==='.        ELPSUMRY
00152          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00153          05  FILLER                PIC X(07)  VALUE '======='.    ELPSUMRY
00154          05  FILLER                PIC X(04)  VALUE SPACES.       ELPSUMRY
00155          05  FILLER                PIC X(01)  VALUE '='.          ELPSUMRY
00156          05  FILLER                PIC X(05)  VALUE SPACES.       ELPSUMRY
00157          05  FILLER                PIC X(01)  VALUE '='.          ELPSUMRY
00158          05  FILLER                PIC X(16)  VALUE SPACES.       ELPSUMRY
00159      03  DETAIL-LINE-1.                                           ELPSUMRY
00160          05  FILLER                PIC X(01)  VALUE ' '.          ELPSUMRY
00161          05  FILLER                PIC X(13)  VALUE SPACES.       ELPSUMRY
00162          05  DL1-ABEND-CODE        PIC X(04)  VALUE SPACES.       ELPSUMRY
00163          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00164          05  DL1-TERM-ID           PIC X(04)  VALUE SPACES.       ELPSUMRY
00165          05  FILLER                PIC X(02)  VALUE SPACES.       ELPSUMRY
00166          05  DL1-ABEND-DATE        PIC 99/99/99.                  ELPSUMRY
00167          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00168          05  DL1-ABEND-TIME        PIC 99B99B99.                  ELPSUMRY
00169          05  FILLER   REDEFINES DL1-ABEND-TIME.                   ELPSUMRY
00170              10  FILLER            PIC XX.                        ELPSUMRY
00171              10  DL1-COLON-1       PIC X.                         ELPSUMRY
00172              10  FILLER            PIC XX.                        ELPSUMRY
00173              10  DL1-COLON-2       PIC X.                         ELPSUMRY
00174              10  FILLER            PIC XX.                        ELPSUMRY
00175          05  FILLER                PIC X(04)  VALUE SPACES.       ELPSUMRY
00176          05  DL1-SYSID             PIC X(04)  VALUE SPACES.       ELPSUMRY
00177          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00178          05  DL1-APPLID            PIC X(08)  VALUE SPACES.       ELPSUMRY
00179          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00180          05  DL1-FACILITY          PIC X(04)  VALUE SPACES.       ELPSUMRY
00181          05  FILLER                PIC X(04)  VALUE SPACES.       ELPSUMRY
00182          05  DL1-USER-INIT         PIC X(03)  VALUE SPACES.       ELPSUMRY
00183          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00184          05  DL1-USER-ID           PIC X(08)  VALUE SPACES.       ELPSUMRY
00185          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00186          05  DL1-USER-DEPT         PIC X(03)  VALUE SPACES.       ELPSUMRY
00187          05  FILLER                PIC X(03)  VALUE SPACES.       ELPSUMRY
00188          05  DL1-TASK-NUMBER       PIC 9(07)  VALUE ZEROES.       ELPSUMRY
00189          05  FILLER                PIC X(04)  VALUE SPACES.       ELPSUMRY
00190          05  DL1-SSD-ROUTED        PIC X(01)  VALUE SPACES.       ELPSUMRY
00191          05  FILLER                PIC X(05)  VALUE SPACES.       ELPSUMRY
00192          05  DL1-SD-ROUTED         PIC X(01)  VALUE SPACES.       ELPSUMRY
00193          05  FILLER                PIC X(16)  VALUE SPACES.       ELPSUMRY
00194      03  SUMMARY-LINE-1.                                          ELPSUMRY
00195          05  FILLER                PIC X(01)  VALUE '0'.          ELPSUMRY
00196          05  FILLER                PIC X(09)  VALUE SPACES.       ELPSUMRY
00197          05  FILLER                PIC X(21)  VALUE               ELPSUMRY
00198              'TOTAL ABENDS REPORTED'.                             ELPSUMRY
00199          05  FILLER                PIC X(01)  VALUE SPACES.       ELPSUMRY
00200          05  SL1-TOTAL             PIC ZZZZ9  VALUE ZEROES.       ELPSUMRY
00201          05  FILLER                PIC X(07)  VALUE SPACES.       ELPSUMRY
00202          05  FILLER                PIC X(10)  VALUE 'ROUTED SSD'. ELPSUMRY
00203          05  FILLER                PIC X(01)  VALUE SPACES.       ELPSUMRY
00204          05  SL1-TOTAL-SSD         PIC ZZZZ9  VALUE ZEROES.       ELPSUMRY
00205          05  FILLER                PIC X(07)  VALUE SPACES.       ELPSUMRY
00206          05  FILLER                PIC X(12)  VALUE               ELPSUMRY
00207              'ROUTED TO SD'.                                      ELPSUMRY
00208          05  FILLER                PIC X(01)  VALUE SPACES.       ELPSUMRY
00209          05  SL1-TOTAL-SD          PIC ZZZZ9  VALUE ZEROES.       ELPSUMRY
00210          05  FILLER                PIC X(06)  VALUE SPACES.       ELPSUMRY
00211          05  FILLER                PIC X(20)  VALUE               ELPSUMRY
00212              'ROUTED TO SSD AND SD'.                              ELPSUMRY
00213          05  FILLER                PIC X(01)  VALUE SPACES.       ELPSUMRY
00214          05  SL1-TOTAL-SSD-SD      PIC ZZZZ9  VALUE ZEROES.       ELPSUMRY
00215          05  FILLER                PIC X(16)  VALUE SPACES.       ELPSUMRY
00216 /                                                                 ELPSUMRY
00217      COPY ELSNAPRC.                                               ELPSUMRY
00218                                                                   ELPSUMRY
00219  01  FILLER           PIC X(16)   VALUE '*END OF ELPSUMRY'.       ELPSUMRY
00220 /                                                                 ELPSUMRY
00221  LINKAGE SECTION.                                                 ELPSUMRY
00222  COPY ELSPRCBC.                                                   ELPSUMRY
00223 /                                                                 ELPSUMRY
00224  COPY ELSNAPPC.                                                   ELPSUMRY
00225 /                                                                 ELPSUMRY
00226  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK.                ELPSUMRY
00227      PERFORM 0000-INITIALIZATION.                                 ELPSUMRY
00228      PERFORM 1000-PRINT-REPORT                                    ELPSUMRY
00229           UNTIL WS-EOF.                                           ELPSUMRY
00230      PERFORM 0100-TERMINATION.                                    ELPSUMRY
00231      MOVE ZERO TO RETURN-CODE.                                    ELPSUMRY
00232      GOBACK.                                                      ELPSUMRY
00233                                                                   ELPSUMRY
00234  0000-INITIALIZATION.                                             ELPSUMRY
00235      PERFORM 8000-PRINT-PAGE-HEADINGS.                            ELPSUMRY
00236      OPEN INPUT SUMMARY-FILE.                                     ELPSUMRY
00237      PERFORM 8500-READ-SUMMARY.                                   ELPSUMRY
00238                                                                   ELPSUMRY
00239  0100-TERMINATION.                                                ELPSUMRY
00240      CLOSE SUMMARY-FILE.                                          ELPSUMRY
00241      IF WS-TOTAL > ZERO                                           ELPSUMRY
00242          MOVE WS-TOTAL            TO SL1-TOTAL                    ELPSUMRY
00243          MOVE WS-TOTAL-SSD        TO SL1-TOTAL-SSD                ELPSUMRY
00244          MOVE WS-TOTAL-SD         TO SL1-TOTAL-SD                 ELPSUMRY
00245          MOVE WS-TOTAL-SSD-SD     TO SL1-TOTAL-SSD-SD             ELPSUMRY
00246          MOVE SUMMARY-LINE-1      TO PCB-PRINT-AREA               ELPSUMRY
00247          PERFORM 9000-PRINT-LINE                                  ELPSUMRY
00248      END-IF.                                                      ELPSUMRY
00249 /                                                                 ELPSUMRY
00250  1000-PRINT-REPORT.                                               ELPSUMRY
00251      IF PCB-CURRENT-LINE > PCB-MAX-LINES                          ELPSUMRY
00252           PERFORM 8000-PRINT-PAGE-HEADINGS.                       ELPSUMRY
00253      CALL 'ELUADDRS' USING SSR-SUB-REC (1)                        ELPSUMRY
00254                 ADDRESS OF SSP-SNAP-SHOT-PREFIX.                  ELPSUMRY
00255                                                                   ELPSUMRY
00256      MOVE SPACES TO DETAIL-LINE-1.                                ELPSUMRY
00257      MOVE SSR-ABEND-CODE                                          ELPSUMRY
00258        TO DL1-ABEND-CODE.                                         ELPSUMRY
00259      MOVE SSR-TERMINAL-ID                                         ELPSUMRY
00260        TO DL1-TERM-ID.                                            ELPSUMRY
00261      MOVE SSR-ABEND-DATE                                          ELPSUMRY
00262        TO WS-JULIAN-DATE.                                         ELPSUMRY
00263      CALL 'TSGGREG' USING WS-JULIAN-DATE WS-GREGORIAN-DATE.       ELPSUMRY
00264      MOVE WS-GREGORIAN-DATE                                       ELPSUMRY
00265        TO DL1-ABEND-DATE.                                         ELPSUMRY
00266      MOVE SSR-ABEND-TIME                                          ELPSUMRY
00267        TO DL1-ABEND-TIME.                                         ELPSUMRY
00268      MOVE ':' TO DL1-COLON-1, DL1-COLON-2.                        ELPSUMRY
00269      MOVE SSR-CICS-SYSTEM-ID                                      ELPSUMRY
00270        TO DL1-SYSID.                                              ELPSUMRY
00271      MOVE SSR-CICS-APPL-ID                                        ELPSUMRY
00272        TO DL1-APPLID.                                             ELPSUMRY
00273      MOVE SSP-FACILITY                                            ELPSUMRY
00274        TO DL1-FACILITY.                                           ELPSUMRY
00275      MOVE SSP-OPERATOR-INITIALS                                   ELPSUMRY
00276        TO DL1-USER-INIT.                                          ELPSUMRY
00277      MOVE SSP-USER-ID                                             ELPSUMRY
00278        TO DL1-USER-ID.                                            ELPSUMRY
00279      MOVE SSP-USER-DEPT-NO                                        ELPSUMRY
00280        TO DL1-USER-DEPT.                                          ELPSUMRY
00281      MOVE SSP-TASK-NUMBER                                         ELPSUMRY
00282        TO DL1-TASK-NUMBER.                                        ELPSUMRY
00283      PERFORM 1100-DETERMINE-ROUTING.                              ELPSUMRY
00284      MOVE DETAIL-LINE-1 TO PCB-PRINT-AREA.                        ELPSUMRY
00285      PERFORM 9000-PRINT-LINE.                                     ELPSUMRY
00286      PERFORM 8500-READ-SUMMARY.                                   ELPSUMRY
00287 /                                                                 ELPSUMRY
00288  1100-DETERMINE-ROUTING.                                          ELPSUMRY
00289      ADD 1 TO WS-TOTAL.                                           ELPSUMRY
00290      SEARCH ALL ART-ABEND-ROUTE-TBL                               ELPSUMRY
00291          AT END                                                   ELPSUMRY
00292             MOVE ' ' TO DL1-SSD-ROUTED                            ELPSUMRY
00293             MOVE 'Y' TO DL1-SD-ROUTED                             ELPSUMRY
00294             ADD 1 TO WS-TOTAL-SD                                  ELPSUMRY
00295          WHEN ART-ABEND-CODE (ART-ABEND-IDX) =                    ELPSUMRY
00296               SSR-ABEND-CODE                                      ELPSUMRY
00297              PERFORM 1110-DETERMINE-ROUTING                       ELPSUMRY
00298      END-SEARCH.                                                  ELPSUMRY
00299                                                                   ELPSUMRY
00300  1110-DETERMINE-ROUTING.                                          ELPSUMRY
00301      SEARCH ALL ART-ABEND-REC-ROUTE-TBL                           ELPSUMRY
00302         AT END                                                    ELPSUMRY
00303             MOVE ' ' TO DL1-SSD-ROUTED                            ELPSUMRY
00304             MOVE 'Y' TO DL1-SD-ROUTED                             ELPSUMRY
00305             ADD 1 TO WS-TOTAL-SD                                  ELPSUMRY
00306         WHEN ART-DDNAME (ART-DDNAME-IDX)  =                       ELPSUMRY
00307              SSR-DDNAME                                           ELPSUMRY
00308             PERFORM 1120-PRINT-ROUTING                            ELPSUMRY
00309      END-SEARCH.                                                  ELPSUMRY
00310                                                                   ELPSUMRY
00311  1120-PRINT-ROUTING.                                              ELPSUMRY
00312      SET ART-SSD-IDX TO ART-ABEND-IDX.                            ELPSUMRY
00313      SET ART-SD-IDX  TO ART-ABEND-IDX.                            ELPSUMRY
00314      IF ART-PRINT-SSD (ART-DDNAME-IDX, ART-SSD-IDX)               ELPSUMRY
00315         MOVE 'Y' TO DL1-SSD-ROUTED                                ELPSUMRY
00316      ELSE                                                         ELPSUMRY
00317         MOVE ' ' TO DL1-SSD-ROUTED                                ELPSUMRY
00318      END-IF.                                                      ELPSUMRY
00319                                                                   ELPSUMRY
00320      IF ART-PRINT-SD (ART-DDNAME-IDX, ART-SD-IDX)                 ELPSUMRY
00321         MOVE 'Y' TO DL1-SD-ROUTED                                 ELPSUMRY
00322      ELSE                                                         ELPSUMRY
00323         MOVE ' ' TO DL1-SD-ROUTED                                 ELPSUMRY
00324      END-IF.                                                      ELPSUMRY
00325                                                                   ELPSUMRY
00326      IF ART-PRINT-SSD (ART-DDNAME-IDX, ART-SSD-IDX) AND           ELPSUMRY
00327         ART-PRINT-SD  (ART-DDNAME-IDX, ART-SD-IDX)                ELPSUMRY
00328           ADD 1 TO WS-TOTAL-SSD-SD                                ELPSUMRY
00329      ELSE                                                         ELPSUMRY
00330          IF ART-PRINT-SSD (ART-DDNAME-IDX, ART-SSD-IDX)           ELPSUMRY
00331              ADD 1 TO WS-TOTAL-SSD                                ELPSUMRY
00332          ELSE                                                     ELPSUMRY
00333              IF ART-PRINT-SD (ART-DDNAME-IDX, ART-SD-IDX)         ELPSUMRY
00334                  ADD 1 TO WS-TOTAL-SD.                            ELPSUMRY
00335 /                                                                 ELPSUMRY
00336  8000-PRINT-PAGE-HEADINGS.                                        ELPSUMRY
00337      MOVE HEADING-LINE-1 TO PCB-PRINT-AREA.                       ELPSUMRY
00338      PERFORM 9000-PRINT-LINE.                                     ELPSUMRY
00339      MOVE HEADING-LINE-2 TO PCB-PRINT-AREA.                       ELPSUMRY
00340      PERFORM 9000-PRINT-LINE.                                     ELPSUMRY
00341      MOVE HEADING-LINE-3 TO PCB-PRINT-AREA.                       ELPSUMRY
00342      PERFORM 9000-PRINT-LINE.                                     ELPSUMRY
00343      MOVE HEADING-LINE-4 TO PCB-PRINT-AREA.                       ELPSUMRY
00344      PERFORM 9000-PRINT-LINE.                                     ELPSUMRY
00345      MOVE SPACES         TO PCB-PRINT-AREA.                       ELPSUMRY
00346      PERFORM 9000-PRINT-LINE.                                     ELPSUMRY
00347                                                                   ELPSUMRY
00348  8500-READ-SUMMARY.                                               ELPSUMRY
00349      READ SUMMARY-FILE                                            ELPSUMRY
00350          AT END                                                   ELPSUMRY
00351               SET WS-EOF TO TRUE.                                 ELPSUMRY
00352                                                                   ELPSUMRY
00353  9000-PRINT-LINE.                                                 ELPSUMRY
00354      SET PCB-PRINT-LINE             TO TRUE.                      ELPSUMRY
00355      INSPECT PCB-PRINT-AREA REPLACING ALL LOW-VALUE BY '_'.       ELPSUMRY
00356      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPSUMRY
