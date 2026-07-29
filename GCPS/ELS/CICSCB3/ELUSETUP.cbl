00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUSETUP
00003  PROGRAM-ID.         ELUSETUP.                                       LV001
00004                                                                   ELUSETUP
00005  AUTHOR.             EDWARD G LISS                                ELUSETUP
00006                                                                   ELUSETUP
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUSETUP
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUSETUP
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUSETUP
00010                      233 N. MICHIGAN AVE                          ELUSETUP
00011                      CHICAGO, ILLINOIS 60601                      ELUSETUP
00012                                                                   ELUSETUP
00013  DATE-WRITTEN.       27-OCT-1986.                                 ELUSETUP
00014                                                                   ELUSETUP
00015  DATE-COMPILED.                                                   ELUSETUP
00016                                                                   ELUSETUP
00017  SECURITY.           COPYRIGHT 1986,                              ELUSETUP
00018                      HEALTH CARE SERVICE CORPORATION              ELUSETUP
00019      SKIP3                                                        ELUSETUP
00020  ENVIRONMENT DIVISION.                                            ELUSETUP
00021                                                                   ELUSETUP
00022  CONFIGURATION SECTION.                                           ELUSETUP
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELUSETUP
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELUSETUP
00025      EJECT                                                        ELUSETUP
00026 ******************************************************************ELUSETUP
00027 *                                                                *ELUSETUP
00028 *      THIS MODULE CREATES AND READS MAIN STORAGE AREAS FOR      *ELUSETUP
00029 *      THE CONTRACT AND GROUP SPECIFIC RECORDS NEEDED BY THE     *ELUSETUP
00030 *      ENGLISH CONTRACT INQUIRY SYSTEM.                          *ELUSETUP
00031 *                                                                *ELUSETUP
00032 ******************************************************************ELUSETUP
00033 *                                                                *ELUSETUP
00034 *                      MAINTENANCE HISTORY                       *ELUSETUP
00035 *                                                                *ELUSETUP
00036 *  MOD     DATE     BY  DRPT                ACTION               *ELUSETUP
00037 * ----- ----------- --- ----- ---------------------------------- *ELUSETUP
00038 * 01.00 27-OCT-1986 EGL       CREATED                            *ELUSETUP
00039 * 01.01 19-JUN-1987 EGL       ADDED ELSTGTBC - STORAGE ALLOC-    *ELUSETUP
00040 *                             ATION TABLE - FOR CONDITIONAL      *ELUSETUP
00041 *                             STORAGE ALLOCATION                 *ELUSETUP
00042 * 01.02 10-AUG-1988 EGL       CHANGED PROGRAM LOGIC TO BUILD     *ELUSETUP
00043 *                             GROUP AND CONTRACT RECORD KEYS     *ELUSETUP
00044 *                             EVEN IF THE RECORDS WILL NOT BE    *ELUSETUP
00045 *                             READ.  THIS IS REQUIRED SINCE      *ELUSETUP
00046 *                             SPECIAL BENEFITS MESSAGES CAN USE  *ELUSETUP
00047 *                             INFORMATION FROM THE KEYS.         *ELUSETUP
00048 *                             ALSO CHANGE MODULE TO USE THE NEW  *ELUSETUP
00049 *                             STORAGE MANAGEMENT ROUTINES.       *ELUSETUP
00050 * 01.03 25-OCT-1989 EGL       DESTRUCTED PROGRAM                 *ELUSETUP
00051 * 01.04 26-JAN-1990 EGL       ADDED PATCH TO TRAP THE SITUATION  *ELUSETUP
00052 *                             WHEN GROUP AND CONTRACT RECORDS    *ELUSETUP
00053 *                             WHICH ARE UNAVAILABLE MAKE IT TO   *ELUSETUP
00054 *                             TOPIC PROCESSING.  THIS SHOULD NOT *ELUSETUP
00055 *                             HAPPEN.  HOWEVER, ...              *ELUSETUP
00056 *                                                                *ELUSETUP
00057 * 01.05 13-OCT-1997 AKK       COMPILED FOR YR2000.               *ELUSETUP
00058 *                                                                *ELUSETUP
00059 * 01.06 19-JAN-1998 AKK       CORRECTED ERRORS IN CONTRACT-      *ELUSETUP
00060 *                             TALLY PARAGRAPH                    *ELUSETUP
00061 ******************************************************************ELUSETUP
00062                                                                   ELUSETUP
00063      EJECT                                                        ELUSETUP
00064  DATA DIVISION.                                                   ELUSETUP
00065  WORKING-STORAGE SECTION.                                         ELUSETUP
00066  01  FILLER                     PICTURE X(32)                     ELUSETUP
00067           VALUE '******* WS STARTS HERE *******'.                 ELUSETUP
00068                                                                   ELUSETUP
00069 * TEXAS REGIONS FOR PACKAGE CODE CHECK                            ELUSETUP
00070  01  WS-APPLID.                                                   ELUSETUP
00071      02 FILLER                   PIC X(03).                       ELUSETUP
00072      02 FILLER                   PIC X(04).                       ELUSETUP
00073         88 TEXAS-REGION          VALUES                           ELUSETUP
00074         'XAI1' 'XAI2' 'XAB1' 'XAB2' 'XAB3' 'XAB4' 'XAB5'          ELUSETUP
00075         'XAB6' 'XAB7' 'XAB8' 'XAB9' 'XAS1' 'XAS2' 'XFB1'          ELUSETUP
00076         'XFB2' 'XF01'.                                            ELUSETUP
00077                                                                   ELUSETUP
00078  01  WS-TALLY-ERROR-SW          PICTURE X.                        ELUSETUP
00079      88  WS-TALLIES-OK                  VALUE 'N'.                ELUSETUP
00080      88  WS-TALLY-ERROR                 VALUE 'Y'.                ELUSETUP
00081                                                                   ELUSETUP
00082  01  WS-NULL-PTR                POINTER VALUE NULL.               ELUSETUP
00083  01  WS-PLAN-CODE               PICTURE X(03) VALUE ZERO.         ELUSETUP
00084  01  WS-PKG-CODE                PICTURE X(03) VALUE ZERO.         ELUSETUP
00085                                                                   ELUSETUP
00086  01  WS-TALLY-AREA.                                               ELUSETUP
00087      05  WS-GROUP-TALLY         PICTURE S9(4) COMP SYNC.          ELUSETUP
00088      05  WS-CONT-INST-BAS-TALLY PICTURE S9(4) COMP SYNC.          ELUSETUP
00089      05  WS-CONT-INST-SUP-TALLY PICTURE S9(4) COMP SYNC.          ELUSETUP
00090      05  WS-CONT-PROF-BAS-TALLY PICTURE S9(4) COMP SYNC.          ELUSETUP
00091      05  WS-CONT-PROF-SUP-TALLY PICTURE S9(4) COMP SYNC.          ELUSETUP
00092                                                                   ELUSETUP
00093  01  WS-INDEX-AREA.                                               ELUSETUP
00094      05  WS-GROUP-INDEX         PICTURE S9(8) COMP SYNC.          ELUSETUP
00095      05  WS-CONT-INST-BAS-INDEX PICTURE S9(8) COMP SYNC.          ELUSETUP
00096      05  WS-CONT-INST-SUP-INDEX PICTURE S9(8) COMP SYNC.          ELUSETUP
00097      05  WS-CONT-PROF-BAS-INDEX PICTURE S9(8) COMP SYNC.          ELUSETUP
00098      05  WS-CONT-PROF-SUP-INDEX PICTURE S9(8) COMP SYNC.          ELUSETUP
00099      05  WS-KTC-IDX             PICTURE S9(8) COMP SYNC.          ELUSETUP
00100      05  WS-KTG-IDX             PICTURE S9(8) COMP SYNC.          ELUSETUP
00101      EJECT                                                        ELUSETUP
00102  COPY ELSTGTBC.                                                   ELUSETUP
00103      EJECT                                                        ELUSETUP
00104  LINKAGE SECTION.                                                 ELUSETUP
00105  01  DFHCOMMAREA.                                                 ELUSETUP
00106  COPY ELSCOMMC.                                                   ELUSETUP
00107      EJECT                                                        ELUSETUP
00108  COPY ELSCIA2C.                                                   ELUSETUP
00109      EJECT                                                        ELUSETUP
00110  COPY ELSSSCBC.                                                   ELUSETUP
00111      EJECT                                                        ELUSETUP
00112  COPY ELSIOPMC.                                                   ELUSETUP
00113      EJECT                                                        ELUSETUP
00114  COPY ELSKEYSC.                                                   ELUSETUP
00115      EJECT                                                        ELUSETUP
00116  COPY ELSKTBCC.                                                   ELUSETUP
00117      EJECT                                                        ELUSETUP
00118  COPY ELSKTBGC.                                                   ELUSETUP
00119      EJECT                                                        ELUSETUP
00120  COPY ELSOUTPC.                                                   ELUSETUP
00121      EJECT                                                        ELUSETUP
00122  01  GCG-GCGRPSPC-RECORD.                                         ELUSETUP
00123  COPY GCGROUPC.                                                   ELUSETUP
00124      EJECT                                                        ELUSETUP
00125  01  GCT-GCCONTRC-RECORD.                                         ELUSETUP
00126  COPY GCCONTRC.                                                   ELUSETUP
00127      EJECT                                                        ELUSETUP
00128  PROCEDURE DIVISION.                                              ELUSETUP
00129 ************************************************************      ELUSETUP
00130 *                                                          *      ELUSETUP
00131 *        SET UP THE TOPIC LEVEL STORAGE                    *      ELUSETUP
00132 *                                                          *      ELUSETUP
00133 ************************************************************      ELUSETUP
00134  SET-UP-THE-TOPIC-LEVEL-STORAGE.                                  ELUSETUP
00135      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELUSETUP
00136          EXEC CICS ABEND                                          ELUSETUP
00137                    ABCODE('EL01')                                 ELUSETUP
00138                    END-EXEC.                                      ELUSETUP
00139      PERFORM INITIALIZATION.                                      ELUSETUP
00140      PERFORM MAIN-PROCESSING.                                     ELUSETUP
00141      GOBACK.                                                      ELUSETUP
00142                                                                   ELUSETUP
00143                                                                   ELUSETUP
00144 ************************************************************      ELUSETUP
00145 *                                                          *      ELUSETUP
00146 *        INITIALIZATION                                    *      ELUSETUP
00147 *                                                          *      ELUSETUP
00148 ************************************************************      ELUSETUP
00149  INITIALIZATION.                                                  ELUSETUP
00150      PERFORM INITIALIZE-SYSTEM-POINTERS.                          ELUSETUP
00151      PERFORM DETERMINE-TOPIC-STORAGE-AMT.                         ELUSETUP
00152      PERFORM INITIALIZE-ELSKEYS.                                  ELUSETUP
00153      PERFORM READ-KEY-TABLES.                                     ELUSETUP
00154      PERFORM ELIMINATE-NON-APPL-CONTRACTS.                        ELUSETUP
00155      PERFORM VERIFY-UNIQUE-SELECTION.                             ELUSETUP
00156                                                                   ELUSETUP
00157                                                                   ELUSETUP
00158 ************************************************************      ELUSETUP
00159 *                                                          *      ELUSETUP
00160 *        INITIALIZE SYSTEM POINTERS                        *      ELUSETUP
00161 *                                                          *      ELUSETUP
00162 ************************************************************      ELUSETUP
00163  INITIALIZE-SYSTEM-POINTERS.                                      ELUSETUP
00164      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUSETUP
00165          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELUSETUP
00166      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELUSETUP
00167      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUSETUP
00168          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELUSETUP
00169 /***********************************************************      ELUSETUP
00170 *                                                          *      ELUSETUP
00171 *        DETERMINE TOPIC STORAGE AMT                       *      ELUSETUP
00172 *                                                          *      ELUSETUP
00173 ************************************************************      ELUSETUP
00174  DETERMINE-TOPIC-STORAGE-AMT.                                     ELUSETUP
00175      SEARCH ALL SAT-TABLE                                         ELUSETUP
00176           AT END                                                  ELUSETUP
00177               SET CIA-AB-PGM-LOGIC TO TRUE                        ELUSETUP
00178               EXEC CICS ABEND                                     ELUSETUP
00179                         ABCODE(CIA-ABCODE)                        ELUSETUP
00180               END-EXEC                                            ELUSETUP
00181           WHEN SAT-TOPIC-PGM-NAME (SAT-IDX)                       ELUSETUP
00182                = SSB-TOPIC-PGM                                    ELUSETUP
00183              CONTINUE                                             ELUSETUP
00184        END-SEARCH.                                                ELUSETUP
00185                                                                   ELUSETUP
00186                                                                   ELUSETUP
00187 ************************************************************      ELUSETUP
00188 *                                                          *      ELUSETUP
00189 *        INITIALIZE ELSKEYS                                *      ELUSETUP
00190 *                                                          *      ELUSETUP
00191 ************************************************************      ELUSETUP
00192  INITIALIZE-ELSKEYS.                                              ELUSETUP
00193      PERFORM SET-ADDR-OF-ELSKEYS.                                 ELUSETUP
00194      IF CIA-RC-PTR-NULL                                           ELUSETUP
00195          PERFORM ALLOCATE-ELSKEYS-AREA.                           ELUSETUP
00196                                                                   ELUSETUP
00197                                                                   ELUSETUP
00198 ************************************************************      ELUSETUP
00199 *                                                          *      ELUSETUP
00200 *        ALLOCATE ELSKEYS AREA                             *      ELUSETUP
00201 *                                                          *      ELUSETUP
00202 ************************************************************      ELUSETUP
00203  ALLOCATE-ELSKEYS-AREA.                                           ELUSETUP
00204      SET CIA-STG-GETMAIN     TO   TRUE.                           ELUSETUP
00205      SET CIA-ELSKEYS-DDN     TO   TRUE.                           ELUSETUP
00206      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00207      PERFORM SET-ADDR-OF-ELSKEYS.                                 ELUSETUP
00208                                                                   ELUSETUP
00209                                                                   ELUSETUP
00210 ************************************************************      ELUSETUP
00211 *                                                          *      ELUSETUP
00212 *        SET ADDR OF ELSKEYS                               *      ELUSETUP
00213 *                                                          *      ELUSETUP
00214 ************************************************************      ELUSETUP
00215  SET-ADDR-OF-ELSKEYS.                                             ELUSETUP
00216      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELUSETUP
00217      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUSETUP
00218          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELUSETUP
00219 /***********************************************************      ELUSETUP
00220 *                                                          *      ELUSETUP
00221 *        READ KEY TABLES                                   *      ELUSETUP
00222 *                                                          *      ELUSETUP
00223 ************************************************************      ELUSETUP
00224  READ-KEY-TABLES.                                                 ELUSETUP
00225      PERFORM SET-ADDR-OF-CONTRACT-KEY-TABLE.                      ELUSETUP
00226      IF CIA-RC-PTR-NULL                                           ELUSETUP
00227          PERFORM READ-CONTRACT-KEY-TABLE.                         ELUSETUP
00228      PERFORM SET-ADDR-OF-GROUP-KEY-TABLE.                         ELUSETUP
00229      IF CIA-RC-PTR-NULL                                           ELUSETUP
00230          PERFORM READ-GROUP-SPECIFIC-KEY-TABLE.                   ELUSETUP
00231                                                                   ELUSETUP
00232                                                                   ELUSETUP
00233 ************************************************************      ELUSETUP
00234 *                                                          *      ELUSETUP
00235 *        READ CONTRACT KEY TABLE                           *      ELUSETUP
00236 *                                                          *      ELUSETUP
00237 ************************************************************      ELUSETUP
00238  READ-CONTRACT-KEY-TABLE.                                         ELUSETUP
00239      SET CIA-ELSKTBC-DDN  TO TRUE.                                ELUSETUP
00240      SET CIA-STG-RETRIEVE TO TRUE.                                ELUSETUP
00241      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00242      PERFORM SET-ADDR-OF-CONTRACT-KEY-TABLE.                      ELUSETUP
00243                                                                   ELUSETUP
00244                                                                   ELUSETUP
00245 ************************************************************      ELUSETUP
00246 *                                                          *      ELUSETUP
00247 *        SET ADDR OF CONTRACT KEY TABLE                    *      ELUSETUP
00248 *                                                          *      ELUSETUP
00249 ************************************************************      ELUSETUP
00250  SET-ADDR-OF-CONTRACT-KEY-TABLE.                                  ELUSETUP
00251      SET CIA-ELSKTBC-DDN  TO TRUE.                                ELUSETUP
00252      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUSETUP
00253          ADDRESS OF KTC-GCCONTR-KEY-TABLE.                        ELUSETUP
00254 /***********************************************************      ELUSETUP
00255 *                                                          *      ELUSETUP
00256 *        READ GROUP SPECIFIC KEY TABLE                     *      ELUSETUP
00257 *                                                          *      ELUSETUP
00258 ************************************************************      ELUSETUP
00259  READ-GROUP-SPECIFIC-KEY-TABLE.                                   ELUSETUP
00260      SET CIA-ELSKTBG-DDN  TO TRUE.                                ELUSETUP
00261      SET CIA-STG-RETRIEVE TO TRUE.                                ELUSETUP
00262      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00263      PERFORM SET-ADDR-OF-GROUP-KEY-TABLE.                         ELUSETUP
00264                                                                   ELUSETUP
00265                                                                   ELUSETUP
00266 ************************************************************      ELUSETUP
00267 *                                                          *      ELUSETUP
00268 *        SET ADDR OF GROUP KEY TABLE                       *      ELUSETUP
00269 *                                                          *      ELUSETUP
00270 ************************************************************      ELUSETUP
00271  SET-ADDR-OF-GROUP-KEY-TABLE.                                     ELUSETUP
00272      SET CIA-ELSKTBG-DDN TO TRUE.                                 ELUSETUP
00273      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUSETUP
00274          ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                       ELUSETUP
00275 /***********************************************************      ELUSETUP
00276 *                                                          *      ELUSETUP
00277 *        ELIMINATE NON APPL CONTRACTS                      *      ELUSETUP
00278 *                                                          *      ELUSETUP
00279 ************************************************************      ELUSETUP
00280  ELIMINATE-NON-APPL-CONTRACTS.                                    ELUSETUP
00281      IF SSB-PROV-CLASS-PROF                                       ELUSETUP
00282          PERFORM ELIMINATE-INST-CONTRACTS.                        ELUSETUP
00283      IF SSB-PROV-CLASS-INST                                       ELUSETUP
00284          PERFORM ELIMINATE-PROF-CONTRACTS.                        ELUSETUP
00285                                                                   ELUSETUP
00286                                                                   ELUSETUP
00287 ************************************************************      ELUSETUP
00288 *                                                          *      ELUSETUP
00289 *        ELIMINATE INST CONTRACTS                          *      ELUSETUP
00290 *                                                          *      ELUSETUP
00291 ************************************************************      ELUSETUP
00292  ELIMINATE-INST-CONTRACTS.                                        ELUSETUP
00293      SET KTC-SEL-IDX TO 1.                                        ELUSETUP
00294      PERFORM REJECT-ALL-TYPE-CONTRACTS.                           ELUSETUP
00295      SET KTC-SEL-IDX TO 2.                                        ELUSETUP
00296      PERFORM REJECT-ALL-TYPE-CONTRACTS.                           ELUSETUP
00297                                                                   ELUSETUP
00298                                                                   ELUSETUP
00299 ************************************************************      ELUSETUP
00300 *                                                          *      ELUSETUP
00301 *        ELIMINATE PROF CONTRACTS                          *      ELUSETUP
00302 *                                                          *      ELUSETUP
00303 ************************************************************      ELUSETUP
00304  ELIMINATE-PROF-CONTRACTS.                                        ELUSETUP
00305      SET KTC-SEL-IDX TO 3.                                        ELUSETUP
00306      PERFORM REJECT-ALL-TYPE-CONTRACTS.                           ELUSETUP
00307      SET KTC-SEL-IDX TO 4.                                        ELUSETUP
00308      PERFORM REJECT-ALL-TYPE-CONTRACTS.                           ELUSETUP
00309                                                                   ELUSETUP
00310                                                                   ELUSETUP
00311 ************************************************************      ELUSETUP
00312 *                                                          *      ELUSETUP
00313 *        REJECT ALL TYPE CONTRACTS                         *      ELUSETUP
00314 *                                                          *      ELUSETUP
00315 ************************************************************      ELUSETUP
00316  REJECT-ALL-TYPE-CONTRACTS.                                       ELUSETUP
00317      PERFORM                                                      ELUSETUP
00318          VARYING WS-KTC-IDX FROM 1 BY 1                           ELUSETUP
00319                   UNTIL WS-KTC-IDX > KTC-NBR-KEYS                 ELUSETUP
00320             SET KTC-IDX TO WS-KTC-IDX                             ELUSETUP
00321             SET KTC-REJ (KTC-IDX, KTC-SEL-IDX) TO TRUE            ELUSETUP
00322      END-PERFORM.                                                 ELUSETUP
00323 /***********************************************************      ELUSETUP
00324 *                                                          *      ELUSETUP
00325 *        VERIFY UNIQUE SELECTION                           *      ELUSETUP
00326 *                                                          *      ELUSETUP
00327 ************************************************************      ELUSETUP
00328  VERIFY-UNIQUE-SELECTION.                                         ELUSETUP
00329      EXEC CICS ASSIGN APPLID (WS-APPLID) END-EXEC.                ELUSETUP
00330      PERFORM GROUP-TALLY.                                         ELUSETUP
00331      PERFORM CONTRACT-TALLY.                                      ELUSETUP
00332      PERFORM CHECK-THE-TALLIES.                                   ELUSETUP
00333      IF WS-TALLY-ERROR                                            ELUSETUP
00334          PERFORM NON-UNIQUE-ABEND.                                ELUSETUP
00335      PERFORM BUILD-RECORD-KEYS.                                   ELUSETUP
00336                                                                   ELUSETUP
00337                                                                   ELUSETUP
00338 ************************************************************      ELUSETUP
00339 *                                                          *      ELUSETUP
00340 *        GROUP TALLY                                       *      ELUSETUP
00341 *                                                          *      ELUSETUP
00342 ************************************************************      ELUSETUP
00343  GROUP-TALLY.                                                     ELUSETUP
00344      MOVE ZEROS TO WS-GROUP-TALLY.                                ELUSETUP
00345      PERFORM                                                      ELUSETUP
00346          VARYING WS-KTG-IDX FROM 1 BY 1                           ELUSETUP
00347                  UNTIL WS-KTG-IDX > KTG-NBR-KEYS                  ELUSETUP
00348             EVALUATE TRUE                                         ELUSETUP
00349                 WHEN KTG-SEL (WS-KTG-IDX) AND TEXAS-REGION        ELUSETUP
00350                    IF SSB-PKG-CODE = KTG-PKG-CODE (WS-KTG-IDX)    ELUSETUP
00351                       PERFORM DO-SELECTED-GROUP                   ELUSETUP
00352                    END-IF                                         ELUSETUP
00353                 WHEN KTG-SEL (WS-KTG-IDX) AND NOT TEXAS-REGION    ELUSETUP
00354                     PERFORM DO-SELECTED-GROUP                     ELUSETUP
00355             END-EVALUATE                                          ELUSETUP
00356      END-PERFORM.                                                 ELUSETUP
00357 ************************************************************      ELUSETUP
00358 *                                                          *      ELUSETUP
00359 *        DO SELECTED GROUP                                 *      ELUSETUP
00360 *                                                          *      ELUSETUP
00361 ************************************************************      ELUSETUP
00362  DO-SELECTED-GROUP.                                               ELUSETUP
00363      ADD 1 TO WS-GROUP-TALLY.                                     ELUSETUP
00364      MOVE WS-KTG-IDX TO WS-GROUP-INDEX.                           ELUSETUP
00365 /***********************************************************      ELUSETUP
00366 *                                                          *      ELUSETUP
00367 *        CONTRACT TALLY                                    *      ELUSETUP
00368 *                                                          *      ELUSETUP
00369 ************************************************************      ELUSETUP
00370  CONTRACT-TALLY.                                                  ELUSETUP
00371      MOVE ZEROS TO WS-CONT-INST-BAS-TALLY                         ELUSETUP
00372                    WS-CONT-INST-SUP-TALLY                         ELUSETUP
00373                    WS-CONT-PROF-BAS-TALLY                         ELUSETUP
00374                    WS-CONT-PROF-SUP-TALLY.                        ELUSETUP
00375      PERFORM DO-CONTRACT-TALLY                                    ELUSETUP
00376          VARYING WS-KTC-IDX FROM 1 BY 1                           ELUSETUP
00377                  UNTIL WS-KTC-IDX > KTC-NBR-KEYS.                 ELUSETUP
00378                                                                   ELUSETUP
00379                                                                   ELUSETUP
00380 ************************************************************      ELUSETUP
00381 *                                                          *      ELUSETUP
00382 *        DO CONTRACT TALLY                                 *      ELUSETUP
00383 *                                                          *      ELUSETUP
00384 ************************************************************      ELUSETUP
00385  DO-CONTRACT-TALLY.                                               ELUSETUP
00386      SET KTC-IDX TO WS-KTC-IDX.                                   ELUSETUP
00387      IF KTC-INST-BAS-SEL (KTC-IDX)                                ELUSETUP
00388         EVALUATE TRUE                                             ELUSETUP
00389            WHEN TEXAS-REGION AND (SSB-PKG-CODE =                  ELUSETUP
00390                KTC-PKG-CODE (KTC-IDX))                            ELUSETUP
00391                 ADD 1 TO WS-CONT-INST-BAS-TALLY                   ELUSETUP
00392                 MOVE WS-KTC-IDX TO WS-CONT-INST-BAS-INDEX         ELUSETUP
00393            WHEN NOT TEXAS-REGION                                  ELUSETUP
00394                 ADD 1 TO WS-CONT-INST-BAS-TALLY                   ELUSETUP
00395                 MOVE WS-KTC-IDX TO WS-CONT-INST-BAS-INDEX         ELUSETUP
00396         END-EVALUATE                                              ELUSETUP
00397      END-IF.                                                      ELUSETUP
00398       IF KTC-INST-SUP-SEL (KTC-IDX)                               ELUSETUP
00399         EVALUATE TRUE                                             ELUSETUP
00400            WHEN TEXAS-REGION AND (SSB-PKG-CODE =                  ELUSETUP
00401                KTC-PKG-CODE (KTC-IDX))                            ELUSETUP
00402               ADD 1 TO WS-CONT-INST-SUP-TALLY                     ELUSETUP
00403               MOVE WS-KTC-IDX TO WS-CONT-INST-SUP-INDEX           ELUSETUP
00404            WHEN NOT TEXAS-REGION                                  ELUSETUP
00405               ADD 1 TO WS-CONT-INST-SUP-TALLY                     ELUSETUP
00406               MOVE WS-KTC-IDX TO WS-CONT-INST-SUP-INDEX           ELUSETUP
00407          END-EVALUATE                                             ELUSETUP
00408       END-IF.                                                     ELUSETUP
00409       IF KTC-PROF-BAS-SEL (KTC-IDX)                               ELUSETUP
00410         EVALUATE TRUE                                             ELUSETUP
00411            WHEN TEXAS-REGION AND (SSB-PKG-CODE =                  ELUSETUP
00412                KTC-PKG-CODE (KTC-IDX))                            ELUSETUP
00413                 ADD 1 TO WS-CONT-PROF-BAS-TALLY                   ELUSETUP
00414                 MOVE WS-KTC-IDX TO WS-CONT-PROF-BAS-INDEX         ELUSETUP
00415            WHEN NOT TEXAS-REGION                                  ELUSETUP
00416              ADD 1 TO WS-CONT-PROF-BAS-TALLY                      ELUSETUP
00417              MOVE WS-KTC-IDX TO WS-CONT-PROF-BAS-INDEX            ELUSETUP
00418          END-EVALUATE                                             ELUSETUP
00419       END-IF.                                                     ELUSETUP
00420       IF KTC-PROF-SUP-SEL (KTC-IDX)                               ELUSETUP
00421         EVALUATE TRUE                                             ELUSETUP
00422            WHEN TEXAS-REGION AND (SSB-PKG-CODE =                  ELUSETUP
00423                KTC-PKG-CODE (KTC-IDX))                            ELUSETUP
00424              ADD 1 TO WS-CONT-PROF-SUP-TALLY                      ELUSETUP
00425              MOVE WS-KTC-IDX TO WS-CONT-PROF-SUP-INDEX            ELUSETUP
00426            WHEN NOT TEXAS-REGION                                  ELUSETUP
00427              ADD 1 TO WS-CONT-PROF-SUP-TALLY                      ELUSETUP
00428              MOVE WS-KTC-IDX TO WS-CONT-PROF-SUP-INDEX            ELUSETUP
00429          END-EVALUATE                                             ELUSETUP
00430       END-IF.                                                     ELUSETUP
00431 /***********************************************************      ELUSETUP
00432 *                                                          *      ELUSETUP
00433 *        CHECK THE TALLIES                                 *      ELUSETUP
00434 *                                                          *      ELUSETUP
00435 ************************************************************      ELUSETUP
00436  CHECK-THE-TALLIES.                                               ELUSETUP
00437      SET WS-TALLIES-OK TO TRUE.                                   ELUSETUP
00438      IF SAT-GROUP-SPEC-REQ (SAT-IDX)                              ELUSETUP
00439          IF WS-GROUP-TALLY > 1                                    ELUSETUP
00440              SET WS-TALLY-ERROR TO TRUE.                          ELUSETUP
00441                                                                   ELUSETUP
00442      IF SAT-CONTRACT-REQ (SAT-IDX)                                ELUSETUP
00443          IF  WS-CONT-INST-BAS-TALLY > 1 OR                        ELUSETUP
00444              WS-CONT-INST-SUP-TALLY > 1 OR                        ELUSETUP
00445              WS-CONT-PROF-BAS-TALLY > 1 OR                        ELUSETUP
00446              WS-CONT-PROF-SUP-TALLY > 1                           ELUSETUP
00447                 SET WS-TALLY-ERROR TO TRUE.                       ELUSETUP
00448 /***********************************************************      ELUSETUP
00449 *                                                          *      ELUSETUP
00450 *        BUILD RECORD KEYS                                 *      ELUSETUP
00451 *                                                          *      ELUSETUP
00452 ************************************************************      ELUSETUP
00453  BUILD-RECORD-KEYS.                                               ELUSETUP
00454      IF WS-GROUP-TALLY > ZERO                                     ELUSETUP
00455          PERFORM BUILD-GROUP-SPECIFIC-KEY.                        ELUSETUP
00456      IF WS-CONT-INST-BAS-TALLY > ZERO                             ELUSETUP
00457          PERFORM BUILD-INST-BAS-KEY.                              ELUSETUP
00458      IF WS-CONT-INST-SUP-TALLY > ZERO                             ELUSETUP
00459          PERFORM BUILD-INST-SUP-KEY.                              ELUSETUP
00460      IF WS-CONT-PROF-BAS-TALLY > ZERO                             ELUSETUP
00461          PERFORM BUILD-PROF-BAS-KEY.                              ELUSETUP
00462      IF WS-CONT-PROF-SUP-TALLY > ZERO                             ELUSETUP
00463          PERFORM BUILD-PROF-SUP-KEY.                              ELUSETUP
00464                                                                   ELUSETUP
00465                                                                   ELUSETUP
00466 ************************************************************      ELUSETUP
00467 *                                                          *      ELUSETUP
00468 *        BUILD GROUP SPECIFIC KEY                          *      ELUSETUP
00469 *                                                          *      ELUSETUP
00470 ************************************************************      ELUSETUP
00471  BUILD-GROUP-SPECIFIC-KEY.                                        ELUSETUP
00472      SET KTG-IDX TO WS-GROUP-INDEX.                               ELUSETUP
00473      MOVE KTG-FAM-REL-LVL (KTG-IDX)                               ELUSETUP
00474          TO SSB-GRP-FAM-REL-LVL.                                  ELUSETUP
00475      MOVE KTG-EFF-DT-CENTURY  (KTG-IDX)                           ELUSETUP
00476          TO SSB-GROUP-EFF-DATE-CEN.                               ELUSETUP
00477      MOVE KTG-TERM-DT-CENTURY (KTG-IDX)                           ELUSETUP
00478          TO SSB-GROUP-TERM-DATE-CEN.                              ELUSETUP
00479 /***********************************************************      ELUSETUP
00480 *                                                          *      ELUSETUP
00481 *        BUILD INST BAS KEY                                *      ELUSETUP
00482 *                                                          *      ELUSETUP
00483 ************************************************************      ELUSETUP
00484  BUILD-INST-BAS-KEY.                                              ELUSETUP
00485      SET KTC-IDX TO WS-CONT-INST-BAS-INDEX.                       ELUSETUP
00486      SET SSB-CONT-IDX TO 1.                                       ELUSETUP
00487      PERFORM COMMON-BUILD-CONTRACT-KEY.                           ELUSETUP
00488                                                                   ELUSETUP
00489                                                                   ELUSETUP
00490 ************************************************************      ELUSETUP
00491 *                                                          *      ELUSETUP
00492 *        BUILD INST SUP KEY                                *      ELUSETUP
00493 *                                                          *      ELUSETUP
00494 ************************************************************      ELUSETUP
00495  BUILD-INST-SUP-KEY.                                              ELUSETUP
00496      SET KTC-IDX TO WS-CONT-INST-SUP-INDEX.                       ELUSETUP
00497      SET SSB-CONT-IDX TO 2.                                       ELUSETUP
00498      PERFORM COMMON-BUILD-CONTRACT-KEY.                           ELUSETUP
00499                                                                   ELUSETUP
00500                                                                   ELUSETUP
00501 ************************************************************      ELUSETUP
00502 *                                                          *      ELUSETUP
00503 *        BUILD PROF BAS KEY                                *      ELUSETUP
00504 *                                                          *      ELUSETUP
00505 ************************************************************      ELUSETUP
00506  BUILD-PROF-BAS-KEY.                                              ELUSETUP
00507      SET KTC-IDX TO WS-CONT-PROF-BAS-INDEX.                       ELUSETUP
00508      SET SSB-CONT-IDX TO 3.                                       ELUSETUP
00509      PERFORM COMMON-BUILD-CONTRACT-KEY.                           ELUSETUP
00510                                                                   ELUSETUP
00511                                                                   ELUSETUP
00512 ************************************************************      ELUSETUP
00513 *                                                          *      ELUSETUP
00514 *        BUILD PROF SUP KEY                                *      ELUSETUP
00515 *                                                          *      ELUSETUP
00516 ************************************************************      ELUSETUP
00517  BUILD-PROF-SUP-KEY.                                              ELUSETUP
00518      SET KTC-IDX TO WS-CONT-PROF-SUP-INDEX.                       ELUSETUP
00519      SET SSB-CONT-IDX TO 4.                                       ELUSETUP
00520      PERFORM COMMON-BUILD-CONTRACT-KEY.                           ELUSETUP
00521                                                                   ELUSETUP
00522                                                                   ELUSETUP
00523 ************************************************************      ELUSETUP
00524 *                                                          *      ELUSETUP
00525 *        COMMON BUILD CONTRACT KEY                         *      ELUSETUP
00526 *                                                          *      ELUSETUP
00527 ************************************************************      ELUSETUP
00528  COMMON-BUILD-CONTRACT-KEY.                                       ELUSETUP
00529                                                                   ELUSETUP
00530      MOVE KTC-L-O-B           (KTC-IDX)                           ELUSETUP
00531          TO SSB-CONT-L-O-B (SSB-CONT-IDX).                        ELUSETUP
00532      MOVE KTC-PROVDR-CONTROL  (KTC-IDX)                           ELUSETUP
00533          TO SSB-CONT-PROVDR-CONTROL (SSB-CONT-IDX).               ELUSETUP
00534      MOVE KTC-FAM-REL-LVL     (KTC-IDX)                           ELUSETUP
00535          TO SSB-CONT-FAM-REL-LVL (SSB-CONT-IDX).                  ELUSETUP
00536      MOVE KTC-EFF-DT-CENTURY     (KTC-IDX)                        ELUSETUP
00537          TO SSB-CONT-EFF-DATE-CEN (SSB-CONT-IDX).                 ELUSETUP
00538      IF SSB-CONT-TERMN-DATE-CEN (SSB-CONT-IDX)  NOT NUMERIC       ELUSETUP
00539          MOVE KTC-TERM-DT-CENTURY (KTC-IDX)                       ELUSETUP
00540             TO SSB-CONT-TERMN-DATE-CEN     (SSB-CONT-IDX)         ELUSETUP
00541      END-IF.                                                      ELUSETUP
00542 /***********************************************************      ELUSETUP
00543 *                                                          *      ELUSETUP
00544 *        MAIN PROCESSING                                   *      ELUSETUP
00545 *                                                          *      ELUSETUP
00546 ************************************************************      ELUSETUP
00547  MAIN-PROCESSING.                                                 ELUSETUP
00548      PERFORM INITIALIZE-RECORD-POINTERS.                          ELUSETUP
00549      IF SAT-GROUP-SPEC-REQ (SAT-IDX)                              ELUSETUP
00550          PERFORM READ-GROUP-SPECIFIC-RECORD.                      ELUSETUP
00551      IF SAT-CONTRACT-REQ (SAT-IDX)                                ELUSETUP
00552          PERFORM READ-CONTRACT-RECORDS.                           ELUSETUP
00553      PERFORM ALLOCATE-TOPIC-WORK-AREAS.                           ELUSETUP
00554                                                                   ELUSETUP
00555                                                                   ELUSETUP
00556 ************************************************************      ELUSETUP
00557 *                                                          *      ELUSETUP
00558 *        INITIALIZE RECORD POINTERS                        *      ELUSETUP
00559 *                                                          *      ELUSETUP
00560 ************************************************************      ELUSETUP
00561  INITIALIZE-RECORD-POINTERS.                                      ELUSETUP
00562      SET CIA-ELSGRPSP-DDN  TO TRUE.                               ELUSETUP
00563      PERFORM SET-ADDRESS-TO-NULL.                                 ELUSETUP
00564      SET CIA-ELSCONIB-DDN  TO TRUE.                               ELUSETUP
00565      PERFORM SET-ADDRESS-TO-NULL.                                 ELUSETUP
00566      SET CIA-ELSCONIS-DDN  TO TRUE.                               ELUSETUP
00567      PERFORM SET-ADDRESS-TO-NULL.                                 ELUSETUP
00568      SET CIA-ELSCONPB-DDN  TO TRUE.                               ELUSETUP
00569      PERFORM SET-ADDRESS-TO-NULL.                                 ELUSETUP
00570      SET CIA-ELSCONPS-DDN  TO TRUE.                               ELUSETUP
00571      PERFORM SET-ADDRESS-TO-NULL.                                 ELUSETUP
00572                                                                   ELUSETUP
00573                                                                   ELUSETUP
00574 ************************************************************      ELUSETUP
00575 *                                                          *      ELUSETUP
00576 *        SET ADDRESS TO NULL                               *      ELUSETUP
00577 *                                                          *      ELUSETUP
00578 ************************************************************      ELUSETUP
00579  SET-ADDRESS-TO-NULL.                                             ELUSETUP
00580      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELUSETUP
00581                            WS-NULL-PTR.                           ELUSETUP
00582 /***********************************************************      ELUSETUP
00583 *                                                          *      ELUSETUP
00584 *        READ GROUP SPECIFIC RECORD                        *      ELUSETUP
00585 *                                                          *      ELUSETUP
00586 ************************************************************      ELUSETUP
00587  READ-GROUP-SPECIFIC-RECORD.                                      ELUSETUP
00588      PERFORM ESTABLISH-GROUP-SPECIFIC-IO-BL.                      ELUSETUP
00589      IF CIA-RC-PTR-NULL                                           ELUSETUP
00590          PERFORM ALLOCATE-GROUP-SPECIFIC-IO-BLO.                  ELUSETUP
00591      PERFORM MOVE-GROUP-SPECIFIC-KEY.                             ELUSETUP
00592      PERFORM CALL-GROUP-SPECIFIC-IO-MODULE.                       ELUSETUP
00593      PERFORM FREE-GROUP-SPECIFIC-IO-BLOCK.                        ELUSETUP
00594                                                                   ELUSETUP
00595                                                                   ELUSETUP
00596 ************************************************************      ELUSETUP
00597 *                                                          *      ELUSETUP
00598 *        ALLOCATE GROUP SPECIFIC IO BLOCK                  *      ELUSETUP
00599 *                                                          *      ELUSETUP
00600 ************************************************************      ELUSETUP
00601  ALLOCATE-GROUP-SPECIFIC-IO-BLO.                                  ELUSETUP
00602      SET CIA-STG-GETMAIN      TO   TRUE.                          ELUSETUP
00603      SET CIA-GCGRPSPC-DDN     TO   TRUE.                          ELUSETUP
00604      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00605      PERFORM ESTABLISH-GROUP-SPECIFIC-IO-BL.                      ELUSETUP
00606                                                                   ELUSETUP
00607                                                                   ELUSETUP
00608 ************************************************************      ELUSETUP
00609 *                                                          *      ELUSETUP
00610 *        ESTABLISH GROUP SPECIFIC IO BLOCK                 *      ELUSETUP
00611 *                                                          *      ELUSETUP
00612 ************************************************************      ELUSETUP
00613  ESTABLISH-GROUP-SPECIFIC-IO-BL.                                  ELUSETUP
00614      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELUSETUP
00615      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUSETUP
00616          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELUSETUP
00617                                                                   ELUSETUP
00618                                                                   ELUSETUP
00619 ************************************************************      ELUSETUP
00620 *                                                          *      ELUSETUP
00621 *        MOVE GROUP SPECIFIC KEY                           *      ELUSETUP
00622 *                                                          *      ELUSETUP
00623 ************************************************************      ELUSETUP
00624  MOVE-GROUP-SPECIFIC-KEY.                                         ELUSETUP
00625                                                                   ELUSETUP
00626      MOVE SSB-PLAN-CODE             TO KWA-GCG-PLAN-CODE.         ELUSETUP
00627      MOVE SSB-GROUP-NUMBER          TO KWA-GCG-GROUP-NUMBER.      ELUSETUP
00628      MOVE SSB-SECTN-NO              TO                            ELUSETUP
00629          KWA-GCG-SECTION-NUMBER.                                  ELUSETUP
00630      MOVE SSB-PKG-CODE              TO KWA-GCG-PKG-CODE.          ELUSETUP
00631      MOVE SSB-GRP-FAM-REL-LVL       TO KWA-GCG-FAM-REL-LVL.       ELUSETUP
00632      MOVE SSB-GROUP-EFF-DATE-CEN    TO KWA-GCG-EFF-DATE-CENTURY.  ELUSETUP
00633                                                                   ELUSETUP
00634                                                                   ELUSETUP
00635 ************************************************************      ELUSETUP
00636 *                                                          *      ELUSETUP
00637 *        CALL GROUP SPECIFIC IO MODULE                     *      ELUSETUP
00638 *                                                          *      ELUSETUP
00639 ************************************************************      ELUSETUP
00640  CALL-GROUP-SPECIFIC-IO-MODULE.                                   ELUSETUP
00641      SET  CIA-GCGRPSPC-DDN  TO  TRUE.                             ELUSETUP
00642      MOVE KWA-GCGRPSPC-KEY  TO  IOP-FILE-KEY.                     ELUSETUP
00643      SET IOP-REC-PTR        TO  NULL.                             ELUSETUP
00644      SET IOP-RD  TO  TRUE.                                        ELUSETUP
00645      SET IOP-FCQ-NONE TO TRUE.                                    ELUSETUP
00646      SET IOP-KVQ-EQ TO TRUE.                                      ELUSETUP
00647      SET IOP-STG-MODE-MOVE TO TRUE.                               ELUSETUP
00648      EXEC CICS LINK                                               ELUSETUP
00649                PROGRAM('ELUIOPGM')                                ELUSETUP
00650                COMMAREA(DFHCOMMAREA)                              ELUSETUP
00651                END-EXEC.                                          ELUSETUP
00652      IF NOT IOP-RC-OK                                             ELUSETUP
00653          PERFORM GROUP-SPECIFIC-NOT-FOUND-ABEND.                  ELUSETUP
00654      SET ADDRESS OF GCG-GCGRPSPC-RECORD                           ELUSETUP
00655                  TO IOP-REC-PTR.                                  ELUSETUP
00656      IF GCG-INTER-RELATIONAL-CODE = ZEROS                         ELUSETUP
00657          PERFORM GROUP-RECORD-UNAVAILABLE                         ELUSETUP
00658      END-IF.                                                      ELUSETUP
00659      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELUSETUP
00660      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELUSETUP
00661                            IOP-REC-PTR.                           ELUSETUP
00662      SET IOP-REC-PTR          TO   NULL.                          ELUSETUP
00663                                                                   ELUSETUP
00664                                                                   ELUSETUP
00665 ************************************************************      ELUSETUP
00666 *                                                          *      ELUSETUP
00667 *        FREE GROUP SPECIFIC IO BLOCK                      *      ELUSETUP
00668 *                                                          *      ELUSETUP
00669 ************************************************************      ELUSETUP
00670  FREE-GROUP-SPECIFIC-IO-BLOCK.                                    ELUSETUP
00671      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELUSETUP
00672      SET CIA-STG-FREEMAIN     TO   TRUE.                          ELUSETUP
00673      SET IOP-FREEMAIN-ALL     TO   TRUE.                          ELUSETUP
00674      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00675 /***********************************************************      ELUSETUP
00676 *                                                          *      ELUSETUP
00677 *        READ CONTRACT RECORDS                             *      ELUSETUP
00678 *                                                          *      ELUSETUP
00679 ************************************************************      ELUSETUP
00680  READ-CONTRACT-RECORDS.                                           ELUSETUP
00681      PERFORM SET-ADDR-OF-CONTRACT-IO-BLOCK.                       ELUSETUP
00682      IF CIA-RC-PTR-NULL                                           ELUSETUP
00683          PERFORM ALLOCATE-CONTRACT-IO-BLOCK.                      ELUSETUP
00684      IF WS-CONT-INST-BAS-TALLY > ZERO                             ELUSETUP
00685          PERFORM READ-BASIC-INST-CONTRACT-RECOR.                  ELUSETUP
00686      IF WS-CONT-INST-SUP-TALLY > ZERO                             ELUSETUP
00687          PERFORM READ-SUPL-INST-CONTRACT-RECORD.                  ELUSETUP
00688      IF WS-CONT-PROF-BAS-TALLY > ZERO                             ELUSETUP
00689          PERFORM READ-BASIC-PROF-CONTRACT-RECOR.                  ELUSETUP
00690      IF WS-CONT-PROF-SUP-TALLY > ZERO                             ELUSETUP
00691          PERFORM READ-SUPL-PROF-CONTRACT-RECORD.                  ELUSETUP
00692      PERFORM FREE-CONTRACT-IO-BLOCK.                              ELUSETUP
00693                                                                   ELUSETUP
00694                                                                   ELUSETUP
00695 ************************************************************      ELUSETUP
00696 *                                                          *      ELUSETUP
00697 *        ALLOCATE CONTRACT IO BLOCK                        *      ELUSETUP
00698 *                                                          *      ELUSETUP
00699 ************************************************************      ELUSETUP
00700  ALLOCATE-CONTRACT-IO-BLOCK.                                      ELUSETUP
00701      SET CIA-STG-GETMAIN      TO   TRUE.                          ELUSETUP
00702      SET CIA-GCCONTR-DDN      TO   TRUE.                          ELUSETUP
00703      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00704      PERFORM SET-ADDR-OF-CONTRACT-IO-BLOCK.                       ELUSETUP
00705                                                                   ELUSETUP
00706                                                                   ELUSETUP
00707 ************************************************************      ELUSETUP
00708 *                                                          *      ELUSETUP
00709 *        SET ADDR OF CONTRACT IO BLOCK                     *      ELUSETUP
00710 *                                                          *      ELUSETUP
00711 ************************************************************      ELUSETUP
00712  SET-ADDR-OF-CONTRACT-IO-BLOCK.                                   ELUSETUP
00713      SET CIA-GCCONTR-DDN TO TRUE.                                 ELUSETUP
00714      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUSETUP
00715          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELUSETUP
00716                                                                   ELUSETUP
00717                                                                   ELUSETUP
00718 ************************************************************      ELUSETUP
00719 *                                                          *      ELUSETUP
00720 *        READ BASIC INST CONTRACT RECORD                   *      ELUSETUP
00721 *                                                          *      ELUSETUP
00722 ************************************************************      ELUSETUP
00723  READ-BASIC-INST-CONTRACT-RECOR.                                  ELUSETUP
00724      SET SSB-CONT-IDX TO 1.                                       ELUSETUP
00725      PERFORM BUILD-CONTRACT-KEYS.                                 ELUSETUP
00726      PERFORM CALL-CONTRACT-IO-MODULE.                             ELUSETUP
00727      SET CIA-ELSCONIB-DDN TO TRUE.                                ELUSETUP
00728      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELUSETUP
00729                            IOP-REC-PTR.                           ELUSETUP
00730                                                                   ELUSETUP
00731                                                                   ELUSETUP
00732 ************************************************************      ELUSETUP
00733 *                                                          *      ELUSETUP
00734 *        READ SUPL INST CONTRACT RECORD                    *      ELUSETUP
00735 *                                                          *      ELUSETUP
00736 ************************************************************      ELUSETUP
00737  READ-SUPL-INST-CONTRACT-RECORD.                                  ELUSETUP
00738      SET SSB-CONT-IDX TO 2.                                       ELUSETUP
00739      PERFORM BUILD-CONTRACT-KEYS.                                 ELUSETUP
00740      PERFORM CALL-CONTRACT-IO-MODULE.                             ELUSETUP
00741      SET CIA-ELSCONIS-DDN TO TRUE.                                ELUSETUP
00742      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELUSETUP
00743                            IOP-REC-PTR.                           ELUSETUP
00744                                                                   ELUSETUP
00745                                                                   ELUSETUP
00746 ************************************************************      ELUSETUP
00747 *                                                          *      ELUSETUP
00748 *        READ BASIC PROF CONTRACT RECORD                   *      ELUSETUP
00749 *                                                          *      ELUSETUP
00750 ************************************************************      ELUSETUP
00751  READ-BASIC-PROF-CONTRACT-RECOR.                                  ELUSETUP
00752      SET SSB-CONT-IDX TO 3.                                       ELUSETUP
00753      PERFORM BUILD-CONTRACT-KEYS.                                 ELUSETUP
00754      PERFORM CALL-CONTRACT-IO-MODULE.                             ELUSETUP
00755      SET CIA-ELSCONPB-DDN TO TRUE.                                ELUSETUP
00756      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELUSETUP
00757                            IOP-REC-PTR.                           ELUSETUP
00758                                                                   ELUSETUP
00759                                                                   ELUSETUP
00760 ************************************************************      ELUSETUP
00761 *                                                          *      ELUSETUP
00762 *        READ SUPL PROF CONTRACT RECORD                    *      ELUSETUP
00763 *                                                          *      ELUSETUP
00764 ************************************************************      ELUSETUP
00765  READ-SUPL-PROF-CONTRACT-RECORD.                                  ELUSETUP
00766      SET SSB-CONT-IDX TO 4.                                       ELUSETUP
00767      PERFORM BUILD-CONTRACT-KEYS.                                 ELUSETUP
00768      PERFORM CALL-CONTRACT-IO-MODULE.                             ELUSETUP
00769      SET CIA-ELSCONPS-DDN TO TRUE.                                ELUSETUP
00770      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELUSETUP
00771                            IOP-REC-PTR.                           ELUSETUP
00772                                                                   ELUSETUP
00773                                                                   ELUSETUP
00774 ************************************************************      ELUSETUP
00775 *                                                          *      ELUSETUP
00776 *        BUILD CONTRACT KEYS                               *      ELUSETUP
00777 *                                                          *      ELUSETUP
00778 ************************************************************      ELUSETUP
00779  BUILD-CONTRACT-KEYS.                                             ELUSETUP
00780      MOVE SSB-PLAN-CODE         TO KWA-GCT-PLAN-CODE.             ELUSETUP
00781      MOVE SSB-GROUP-NUMBER                                        ELUSETUP
00782           TO  KWA-GCT-GROUP-NUMBER.                               ELUSETUP
00783      MOVE SSB-SECTN-NO                                            ELUSETUP
00784           TO  KWA-GCT-SECTION-NUMBER.                             ELUSETUP
00785      MOVE SSB-PKG-CODE          TO KWA-GCT-PKG-CODE.              ELUSETUP
00786      MOVE SSB-CONT-L-O-B (SSB-CONT-IDX)                           ELUSETUP
00787           TO  KWA-GCT-L-O-B.                                      ELUSETUP
00788      MOVE SSB-CONT-PROVDR-CONTROL (SSB-CONT-IDX)                  ELUSETUP
00789           TO  KWA-GCT-PROVDR-CONTROL.                             ELUSETUP
00790      MOVE SSB-CONT-FAM-REL-LVL (SSB-CONT-IDX)                     ELUSETUP
00791           TO  KWA-GCT-FAM-REL-LVL.                                ELUSETUP
00792      MOVE SSB-CONT-EFF-DATE-CEN (SSB-CONT-IDX)                    ELUSETUP
00793           TO  KWA-GCT-EFFECTIVE-DATE-CENTURY.                     ELUSETUP
00794                                                                   ELUSETUP
00795                                                                   ELUSETUP
00796 ************************************************************      ELUSETUP
00797 *                                                          *      ELUSETUP
00798 *        CALL CONTRACT IO MODULE                           *      ELUSETUP
00799 *                                                          *      ELUSETUP
00800 ************************************************************      ELUSETUP
00801  CALL-CONTRACT-IO-MODULE.                                         ELUSETUP
00802      MOVE KWA-GCCONTR-KEY  TO  IOP-FILE-KEY.                      ELUSETUP
00803      SET CIA-GCCONTR-DDN   TO  TRUE.                              ELUSETUP
00804      SET IOP-REC-PTR TO NULL.                                     ELUSETUP
00805      SET IOP-RD  TO  TRUE.                                        ELUSETUP
00806      SET IOP-FCQ-NONE TO TRUE.                                    ELUSETUP
00807      SET IOP-KVQ-EQ TO TRUE.                                      ELUSETUP
00808      SET IOP-STG-MODE-MOVE TO TRUE.                               ELUSETUP
00809      EXEC CICS LINK                                               ELUSETUP
00810                PROGRAM('ELUIOPGM')                                ELUSETUP
00811                COMMAREA(DFHCOMMAREA)                              ELUSETUP
00812                END-EXEC.                                          ELUSETUP
00813      IF NOT IOP-RC-OK                                             ELUSETUP
00814          PERFORM CONTRACT-NOT-FOUND-ABEND.                        ELUSETUP
00815      SET ADDRESS OF GCT-GCCONTRC-RECORD                           ELUSETUP
00816          TO IOP-REC-PTR.                                          ELUSETUP
00817      IF GCT-INTER-REL-VALUES = ZEROS                              ELUSETUP
00818          PERFORM CONTRACT-RECORD-UNAVAILABLE                      ELUSETUP
00819      END-IF.                                                      ELUSETUP
00820                                                                   ELUSETUP
00821                                                                   ELUSETUP
00822 ************************************************************      ELUSETUP
00823 *                                                          *      ELUSETUP
00824 *        FREE CONTRACT IO BLOCK                            *      ELUSETUP
00825 *                                                          *      ELUSETUP
00826 ************************************************************      ELUSETUP
00827  FREE-CONTRACT-IO-BLOCK.                                          ELUSETUP
00828      SET CIA-STG-FREEMAIN     TO   TRUE.                          ELUSETUP
00829      SET IOP-REC-PTR          TO   NULL.                          ELUSETUP
00830      SET IOP-FREEMAIN-ALL     TO   TRUE.                          ELUSETUP
00831      SET CIA-GCCONTR-DDN      TO   TRUE.                          ELUSETUP
00832      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00833 /***********************************************************      ELUSETUP
00834 *                                                          *      ELUSETUP
00835 *        ALLOCATE TOPIC WORK AREAS                         *      ELUSETUP
00836 *                                                          *      ELUSETUP
00837 ************************************************************      ELUSETUP
00838  ALLOCATE-TOPIC-WORK-AREAS.                                       ELUSETUP
00839      PERFORM ALLOCATE-CONDITIONAL-AREAS.                          ELUSETUP
00840      PERFORM ALLOCATE-NON-CONDITIONAL-AREAS.                      ELUSETUP
00841                                                                   ELUSETUP
00842                                                                   ELUSETUP
00843 ************************************************************      ELUSETUP
00844 *                                                          *      ELUSETUP
00845 *        ALLOCATE CONDITIONAL AREAS                        *      ELUSETUP
00846 *                                                          *      ELUSETUP
00847 ************************************************************      ELUSETUP
00848  ALLOCATE-CONDITIONAL-AREAS.                                      ELUSETUP
00849      IF SAT-PAY-LVL-GRP-REQ (SAT-IDX)                             ELUSETUP
00850          PERFORM ALLOCATE-ELSPLGSW-AREA.                          ELUSETUP
00851      IF SAT-SUBR-PARM-REQ (SAT-IDX)                               ELUSETUP
00852          PERFORM ALLOCATE-ELSSRTP-AREA.                           ELUSETUP
00853      IF SAT-BEN-PROV-REQ (SAT-IDX)                                ELUSETUP
00854          PERFORM ALLOCATE-GCBENPRV-AREA.                          ELUSETUP
00855      IF SAT-FIELD-VAL-REQ (SAT-IDX)                               ELUSETUP
00856          PERFORM ALLOCATE-GCFLDVAL-AREA.                          ELUSETUP
00857      IF SAT-SYS-TABULAR-REQ (SAT-IDX)                             ELUSETUP
00858          PERFORM ALLOCATE-GCSYSTBL-AREA.                          ELUSETUP
00859      IF SAT-TABULAR-REQ (SAT-IDX)                                 ELUSETUP
00860          PERFORM ALLOCATE-GCTABULR-AREA.                          ELUSETUP
00861      IF SAT-WORK-FILE1-REQ (SAT-IDX)                              ELUSETUP
00862          PERFORM ALLOCATE-ELSWKFL1-AREA.                          ELUSETUP
00863      IF SAT-WORK-FILE2-REQ (SAT-IDX)                              ELUSETUP
00864          PERFORM ALLOCATE-ELSWKFL2-AREA.                          ELUSETUP
00865      IF SAT-WORK-FILE3-REQ (SAT-IDX)                              ELUSETUP
00866          PERFORM ALLOCATE-ELSWKFL3-AREA.                          ELUSETUP
00867      IF SAT-WORK-FILE4-REQ (SAT-IDX)                              ELUSETUP
00868          PERFORM ALLOCATE-ELSWKFL4-AREA.                          ELUSETUP
00869 /***********************************************************      ELUSETUP
00870 *                                                          *      ELUSETUP
00871 *        ALLOCATE ELSPLGSW AREA                            *      ELUSETUP
00872 *                                                          *      ELUSETUP
00873 ************************************************************      ELUSETUP
00874  ALLOCATE-ELSPLGSW-AREA.                                          ELUSETUP
00875      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
00876      SET CIA-ELSPLGSW-DDN           TO  TRUE.                     ELUSETUP
00877      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00878                                                                   ELUSETUP
00879                                                                   ELUSETUP
00880 ************************************************************      ELUSETUP
00881 *                                                          *      ELUSETUP
00882 *        ALLOCATE ELSSRTP AREA                             *      ELUSETUP
00883 *                                                          *      ELUSETUP
00884 ************************************************************      ELUSETUP
00885  ALLOCATE-ELSSRTP-AREA.                                           ELUSETUP
00886      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
00887      SET CIA-ELSSRTP-DDN            TO  TRUE.                     ELUSETUP
00888      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00889                                                                   ELUSETUP
00890                                                                   ELUSETUP
00891 ************************************************************      ELUSETUP
00892 *                                                          *      ELUSETUP
00893 *        ALLOCATE GCBENPRV AREA                            *      ELUSETUP
00894 *                                                          *      ELUSETUP
00895 ************************************************************      ELUSETUP
00896  ALLOCATE-GCBENPRV-AREA.                                          ELUSETUP
00897      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
00898      SET CIA-GCBENPRV-DDN           TO  TRUE.                     ELUSETUP
00899      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00900                                                                   ELUSETUP
00901                                                                   ELUSETUP
00902 ************************************************************      ELUSETUP
00903 *                                                          *      ELUSETUP
00904 *        ALLOCATE GCFLDVAL AREA                            *      ELUSETUP
00905 *                                                          *      ELUSETUP
00906 ************************************************************      ELUSETUP
00907  ALLOCATE-GCFLDVAL-AREA.                                          ELUSETUP
00908      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
00909      SET CIA-GCFLDVAL-DDN           TO  TRUE.                     ELUSETUP
00910      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00911 /***********************************************************      ELUSETUP
00912 *                                                          *      ELUSETUP
00913 *        ALLOCATE GCSYSTBL AREA                            *      ELUSETUP
00914 *                                                          *      ELUSETUP
00915 ************************************************************      ELUSETUP
00916  ALLOCATE-GCSYSTBL-AREA.                                          ELUSETUP
00917      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
00918      SET CIA-GCSYSTBL-DDN           TO  TRUE.                     ELUSETUP
00919      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00920                                                                   ELUSETUP
00921                                                                   ELUSETUP
00922 ************************************************************      ELUSETUP
00923 *                                                          *      ELUSETUP
00924 *        ALLOCATE GCTABULR AREA                            *      ELUSETUP
00925 *                                                          *      ELUSETUP
00926 ************************************************************      ELUSETUP
00927  ALLOCATE-GCTABULR-AREA.                                          ELUSETUP
00928      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
00929      SET CIA-GCTABULR-DDN           TO  TRUE.                     ELUSETUP
00930      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00931                                                                   ELUSETUP
00932                                                                   ELUSETUP
00933 ************************************************************      ELUSETUP
00934 *                                                          *      ELUSETUP
00935 *        ALLOCATE ELSWKFL1 AREA                            *      ELUSETUP
00936 *                                                          *      ELUSETUP
00937 ************************************************************      ELUSETUP
00938  ALLOCATE-ELSWKFL1-AREA.                                          ELUSETUP
00939      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
00940      SET CIA-ELSWKFL1-DDN           TO  TRUE.                     ELUSETUP
00941      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00942                                                                   ELUSETUP
00943                                                                   ELUSETUP
00944 ************************************************************      ELUSETUP
00945 *                                                          *      ELUSETUP
00946 *        ALLOCATE ELSWKFL2 AREA                            *      ELUSETUP
00947 *                                                          *      ELUSETUP
00948 ************************************************************      ELUSETUP
00949  ALLOCATE-ELSWKFL2-AREA.                                          ELUSETUP
00950      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
00951      SET CIA-ELSWKFL2-DDN           TO  TRUE.                     ELUSETUP
00952      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00953 /***********************************************************      ELUSETUP
00954 *                                                          *      ELUSETUP
00955 *        ALLOCATE ELSWKFL3 AREA                            *      ELUSETUP
00956 *                                                          *      ELUSETUP
00957 ************************************************************      ELUSETUP
00958  ALLOCATE-ELSWKFL3-AREA.                                          ELUSETUP
00959      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
00960      SET CIA-ELSWKFL3-DDN           TO  TRUE.                     ELUSETUP
00961      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00962                                                                   ELUSETUP
00963                                                                   ELUSETUP
00964 ************************************************************      ELUSETUP
00965 *                                                          *      ELUSETUP
00966 *        ALLOCATE ELSWKFL4 AREA                            *      ELUSETUP
00967 *                                                          *      ELUSETUP
00968 ************************************************************      ELUSETUP
00969  ALLOCATE-ELSWKFL4-AREA.                                          ELUSETUP
00970      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
00971      SET CIA-ELSWKFL4-DDN           TO  TRUE.                     ELUSETUP
00972      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00973 /***********************************************************      ELUSETUP
00974 *                                                          *      ELUSETUP
00975 *        ALLOCATE NON CONDITIONAL AREAS                    *      ELUSETUP
00976 *                                                          *      ELUSETUP
00977 ************************************************************      ELUSETUP
00978  ALLOCATE-NON-CONDITIONAL-AREAS.                                  ELUSETUP
00979      PERFORM ALLOCATE-ELSCMIF-AREA.                               ELUSETUP
00980      PERFORM ALLOCATE-ELSOUTP-AREA.                               ELUSETUP
00981      PERFORM ALLOCATE-ELSTCWA-AREA.                               ELUSETUP
00982                                                                   ELUSETUP
00983                                                                   ELUSETUP
00984 ************************************************************      ELUSETUP
00985 *                                                          *      ELUSETUP
00986 *        ALLOCATE ELSCMIF AREA                             *      ELUSETUP
00987 *                                                          *      ELUSETUP
00988 ************************************************************      ELUSETUP
00989  ALLOCATE-ELSCMIF-AREA.                                           ELUSETUP
00990      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
00991      SET CIA-ELSCMIF-DDN            TO  TRUE.                     ELUSETUP
00992      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
00993                                                                   ELUSETUP
00994                                                                   ELUSETUP
00995 ************************************************************      ELUSETUP
00996 *                                                          *      ELUSETUP
00997 *        ALLOCATE ELSOUTP AREA                             *      ELUSETUP
00998 *                                                          *      ELUSETUP
00999 ************************************************************      ELUSETUP
01000  ALLOCATE-ELSOUTP-AREA.                                           ELUSETUP
01001      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
01002      SET CIA-ELSOUTP-DDN            TO  TRUE.                     ELUSETUP
01003      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
01004      SET  CIA-ELSOUTP-DDN TO TRUE.                                ELUSETUP
01005      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUSETUP
01006          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELUSETUP
01007      INITIALIZE COF-OUTPUT-INTERFACE.                             ELUSETUP
01008                                                                   ELUSETUP
01009                                                                   ELUSETUP
01010 ************************************************************      ELUSETUP
01011 *                                                          *      ELUSETUP
01012 *        ALLOCATE ELSTCWA AREA                             *      ELUSETUP
01013 *                                                          *      ELUSETUP
01014 ************************************************************      ELUSETUP
01015  ALLOCATE-ELSTCWA-AREA.                                           ELUSETUP
01016      SET CIA-STG-GETMAIN            TO  TRUE.                     ELUSETUP
01017      SET CIA-ELSTCWA-DDN            TO  TRUE.                     ELUSETUP
01018      PERFORM CALL-STORAGE-MANAGER.                                ELUSETUP
01019 /***********************************************************      ELUSETUP
01020 *                                                          *      ELUSETUP
01021 *        CALL STORAGE MANAGER                              *      ELUSETUP
01022 *                                                          *      ELUSETUP
01023 ************************************************************      ELUSETUP
01024  CALL-STORAGE-MANAGER.                                            ELUSETUP
01025      EXEC CICS LINK                                               ELUSETUP
01026                PROGRAM('ELUSTGMG')                                ELUSETUP
01027                COMMAREA(DFHCOMMAREA)                              ELUSETUP
01028                END-EXEC.                                          ELUSETUP
01029                                                                   ELUSETUP
01030                                                                   ELUSETUP
01031 ************************************************************      ELUSETUP
01032 *                                                          *      ELUSETUP
01033 *        GROUP SPECIFIC NOT FOUND ABEND                    *      ELUSETUP
01034 *                                                          *      ELUSETUP
01035 ************************************************************      ELUSETUP
01036  GROUP-SPECIFIC-NOT-FOUND-ABEND.                                  ELUSETUP
01037      SET CIA-AB-NOTFND-GCGRPSPC  TO TRUE.                         ELUSETUP
01038      EXEC CICS ABEND                                              ELUSETUP
01039                ABCODE(CIA-ABCODE)                                 ELUSETUP
01040                END-EXEC.                                          ELUSETUP
01041                                                                   ELUSETUP
01042                                                                   ELUSETUP
01043 ************************************************************      ELUSETUP
01044 *                                                          *      ELUSETUP
01045 *        CONTRACT NOT FOUND ABEND                          *      ELUSETUP
01046 *                                                          *      ELUSETUP
01047 ************************************************************      ELUSETUP
01048  CONTRACT-NOT-FOUND-ABEND.                                        ELUSETUP
01049      SET CIA-AB-NOTFND-GCCONTR   TO TRUE.                         ELUSETUP
01050      EXEC CICS ABEND                                              ELUSETUP
01051                ABCODE(CIA-ABCODE)                                 ELUSETUP
01052                END-EXEC.                                          ELUSETUP
01053                                                                   ELUSETUP
01054                                                                   ELUSETUP
01055 ************************************************************      ELUSETUP
01056 *                                                          *      ELUSETUP
01057 *        NON UNIQUE ABEND                                  *      ELUSETUP
01058 *                                                          *      ELUSETUP
01059 ************************************************************      ELUSETUP
01060  NON-UNIQUE-ABEND.                                                ELUSETUP
01061      SET CIA-AB-NOT-UNIQUE TO TRUE.                               ELUSETUP
01062      EXEC CICS ABEND                                              ELUSETUP
01063                ABCODE(CIA-ABCODE)                                 ELUSETUP
01064                END-EXEC.                                          ELUSETUP
01065 /                                                                 ELUSETUP
01066 ************************************************************      ELUSETUP
01067 *                                                          *      ELUSETUP
01068 *        GROUP RECORD UNAVAILABLE                          *      ELUSETUP
01069 *                                                          *      ELUSETUP
01070 ************************************************************      ELUSETUP
01071  GROUP-RECORD-UNAVAILABLE.                                        ELUSETUP
01072      SET CIA-AB-GROUP-SPEC-NOT-AVAIL TO TRUE.                     ELUSETUP
01073      EXEC CICS ABEND                                              ELUSETUP
01074                ABCODE(CIA-ABCODE)                                 ELUSETUP
01075                END-EXEC.                                          ELUSETUP
01076                                                                   ELUSETUP
01077                                                                   ELUSETUP
01078 ************************************************************      ELUSETUP
01079 *                                                          *      ELUSETUP
01080 *        CONTRACT RECORD UNAVAILABLE                       *      ELUSETUP
01081 *                                                          *      ELUSETUP
01082 ************************************************************      ELUSETUP
01083  CONTRACT-RECORD-UNAVAILABLE.                                     ELUSETUP
01084      SET CIA-AB-CONTRACT-NOT-AVAIL TO TRUE.                       ELUSETUP
01085      EXEC CICS ABEND                                              ELUSETUP
01086                ABCODE(CIA-ABCODE)                                 ELUSETUP
01087                END-EXEC.                                          ELUSETUP
