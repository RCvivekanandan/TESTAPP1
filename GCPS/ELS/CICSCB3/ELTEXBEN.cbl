00001 *                                                                 09/03/03
00002  IDENTIFICATION DIVISION.                                         ELTEXBEN
00003                                                                      LV002
00004  PROGRAM-ID.         ELTEXBEN.                                    ELTEXBEN
00005                                                                   ELTEXBEN
00006  AUTHOR.             NINA CERVANTES.                              ELTEXBEN
00007                                                                   ELTEXBEN
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTEXBEN
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTEXBEN
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTEXBEN
00011                      233 N. MICHIGAN AVE                          ELTEXBEN
00012                      CHICAGO, ILLINOIS 60601                      ELTEXBEN
00013                                                                   ELTEXBEN
00014  DATE-WRITTEN.       08-APR-1987.                                 ELTEXBEN
00015                                                                   ELTEXBEN
00016  DATE-COMPILED.                                                   ELTEXBEN
00017                                                                   ELTEXBEN
00018  SECURITY.           COPYRIGHT 1987,                              ELTEXBEN
00019                      HEALTH CARE SERVICE CORPORATION              ELTEXBEN
00020      TITLE 'ELTEXBEN  HISTORY'.                                   ELTEXBEN
00021 ******************************************************************ELTEXBEN
00022 *                                                                *ELTEXBEN
00023 *                      MAINTENANCE HISTORY                       *ELTEXBEN
00024 *                                                                *ELTEXBEN
00025 *  MOD     DATE     BY  DRPT                ACTION               *ELTEXBEN
00026 * ----- ----------- --- ----- ---------------------------------- *ELTEXBEN
00027 * 01.00 17-APR-1987 NAC       CREATED                            *ELTEXBEN
00028 * 02.00 03-JUN-1987 NAC       ADD ADDITIONAL SUPPRESSED PROVISIONSELTEXBEN
00029 *       05-AUG-1987 REB       ADD ADDITIONAL SUPPRESSED PROVISIONSELTEXBEN
00030 *       10-AUG-1987 REB       ADD ADDITIONAL SUPPRESSED PROVISIONSELTEXBEN
00031 *       14-AUG-1987 REB       ADD ADDITIONAL SUPPRESSED PROVISIONSELTEXBEN
00032 *       26-AUG-1987 REB       CHANGE LOGIC FOR THE SUPPRESSED    *ELTEXBEN
00033 *                             PROVISIONS. PUT IN W/S AND CHECK   *ELTEXBEN
00034 *                             AGAINST THOSE VALUES.              *ELTEXBEN
00035 *                                                                *ELTEXBEN
00036 * 02.01 23-SEP-1987 REB       MORE SUPPRESSED PROVISIONS ADDED   *ELTEXBEN
00037 *                             AND REARRANGE THEM IN ALPHA ORDER. *ELTEXBEN
00038 *                                                                *ELTEXBEN
00039 * 02.02 28-SEP-1987 REB       MORE SUPPRESSED PROVISIONS ADDED   *ELTEXBEN
00040 *                             PER 9/23/87 PROVISION LISTING.     *ELTEXBEN
00041 *                                                                *ELTEXBEN
00042 * 03.01 08-AUG-1989 RKH       REPLACED HARDCODED SUPPRESSED      *ELTEXBEN
00043 *                             PROVISIONS WITH ELSBPTBL COPYBOOK  *ELTEXBEN
00044 *                                                                *ELTEXBEN
00045 * 04.01 10-MAY-1990 AKK       STOPPED USING THE FIELD VAL TO     *ELTEXBEN
00046 *                             CREATE THE LS-BEN TABLE THIS PROG  *ELTEXBEN
00047 *                             USES.  THIS CHANGE WAS PART OF WHAT*ELTEXBEN
00048 *                             WAS DONE TO FIX A DISP.  COVERED   *ELTEXBEN
00049 *                             BENEFITS WERE BEING LISTED AND THIS*ELTEXBEN
00050 *                             PROG SHOULD ONLY PRINT EXCLUDED    *ELTEXBEN
00051 *                             BENEFITS. DISCP #D00031            *ELTEXBEN
00052 * 04.02 14-JUN-1990 AKK       WHEN 'BOTH' SELECTED ONLY PROFES-  *ELTEXBEN
00053 *                             SIONAL BENEFITS SHOW -- FIXED.     *ELTEXBEN
00054 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTEXBEN
00055 *                                                                 ELTEXBEN
00056 ******************************************************************ELTEXBEN
00057                                                                   ELTEXBEN
00058      TITLE 'ELTEXBEN  WORKING STORAGE SECTION'.                   ELTEXBEN
00059  ENVIRONMENT DIVISION.                                            ELTEXBEN
00060  DATA DIVISION.                                                   ELTEXBEN
00061  WORKING-STORAGE SECTION.                                         ELTEXBEN
00062  77  FILLER                      PIC X(30) VALUE                  ELTEXBEN
00063      '***ELTEXBEN WORKING STORAGE***'.                            ELTEXBEN
00064  01  WS-MISC.                                                     ELTEXBEN
00065      05  BP-SUB                  PIC S9(4) COMP SYNC VALUE ZERO.  ELTEXBEN
00066      05  WS-BP-ID-6.                                              ELTEXBEN
00067          10  WS-BP-ID-5          PIC X(5)  VALUE SPACES.          ELTEXBEN
00068          10  WS-BP-ID-FORMAT     PIC X     VALUE SPACES.          ELTEXBEN
00069      05  WS-BP-MAX               PIC S9(03) VALUE 734.            ELTEXBEN
00070      05  WS-LS-MAX               PIC S9(03) VALUE 737.            ELTEXBEN
00071      05  WS-LS-TABLE-LEN         PIC S9(04) VALUE 6635.           ELTEXBEN
00072                                                                   ELTEXBEN
00073  01  WS-ELTEXBEN-SWITCHES.                                        ELTEXBEN
00074          10  WS-CONT-REC-TYPE-IB PIC X(2)  VALUE SPACES.          ELTEXBEN
00075              88  WS-INST-BASIC-CNT          VALUE 'IB'.           ELTEXBEN
00076          10  WS-CONT-REC-TYPE-IS PIC X(2)  VALUE SPACES.          ELTEXBEN
00077              88  WS-INST-SUPPL-CNT          VALUE 'IS'.           ELTEXBEN
00078          10  WS-CONT-REC-TYPE-PB PIC X(2)  VALUE SPACES.          ELTEXBEN
00079              88  WS-PROF-BASIC-CNT          VALUE 'PB'.           ELTEXBEN
00080          10  WS-CONT-REC-TYPE-PS PIC X(2)  VALUE SPACES.          ELTEXBEN
00081              88  WS-PROF-SUPPL-CNT          VALUE 'PS'.           ELTEXBEN
00082          10  WS-FIRST-TIME-THRU  PIC X(3)  VALUE SPACES.          ELTEXBEN
00083              88  WS-FIRST-TIME              VALUE 'YES'.          ELTEXBEN
00084              88  WS-NOT-FIRST-TIME          VALUE 'NO '.          ELTEXBEN
00085          10  WS-PROGRESS-SWITCH  PIC X(3)  VALUE SPACES.          ELTEXBEN
00086              88  IN-PROGRESS                VALUE 'YES'.          ELTEXBEN
00087              88  DONE                       VALUE 'NO '.          ELTEXBEN
00088          10  WS-UPDATE-BEN-DONE  PIC X(3)  VALUE SPACES.          ELTEXBEN
00089              88  WS-BEN-UPDATED             VALUE 'YES'.          ELTEXBEN
00090              88  WS-BEN-NOT-UPDATED         VALUE 'NO '.          ELTEXBEN
00091          10  WS-CONTRACT-TYPE    PIC X(1)  VALUE SPACES.          ELTEXBEN
00092              88  PROCESSING-INST            VALUE 'I'.            ELTEXBEN
00093              88  PROCESSING-PROF            VALUE 'P'.            ELTEXBEN
00094                                                                   ELTEXBEN
00095  01  WS-FILLER                   PIC X(8)  VALUE 'COUNTTBL'.      ELTEXBEN
00096  01  WS-COUNT-TABLE.                                              ELTEXBEN
00097      05  WS-COUNTERS.                                             ELTEXBEN
00098          10  WS-INST-BASIC-APPLICABLE                             ELTEXBEN
00099                                  PIC S9(4) COMP VALUE +0.         ELTEXBEN
00100          10  WS-INST-BASIC-FOUND-COUNT                            ELTEXBEN
00101                                  PIC S9(4) COMP VALUE +0.         ELTEXBEN
00102          10  WS-INST-SUPP-APPLICABLE                              ELTEXBEN
00103                                  PIC S9(4) COMP VALUE +0.         ELTEXBEN
00104          10  WS-INST-SUPP-FOUND-COUNT                             ELTEXBEN
00105                                  PIC S9(4) COMP VALUE +0.         ELTEXBEN
00106          10  WS-PROF-BASIC-APPLICABLE                             ELTEXBEN
00107                                  PIC S9(4) COMP VALUE +0.         ELTEXBEN
00108          10  WS-PROF-BASIC-FOUND-COUNT                            ELTEXBEN
00109                                  PIC S9(4) COMP VALUE +0.         ELTEXBEN
00110          10  WS-PROF-SUPP-APPLICABLE                              ELTEXBEN
00111                                  PIC S9(4) COMP VALUE +0.         ELTEXBEN
00112          10  WS-PROF-SUPP-FOUND-COUNT                             ELTEXBEN
00113                                  PIC S9(4) COMP VALUE +0.         ELTEXBEN
00114      05  WS-COUNTERS-RDF REDEFINES WS-COUNTERS.                   ELTEXBEN
00115          10  WS-COUNT-ITEM OCCURS 4 TIMES                         ELTEXBEN
00116                       INDEXED BY WS-COUNT-INDEX.                  ELTEXBEN
00117              15  WS-APPLICABLE   PIC S9(4) COMP.                  ELTEXBEN
00118              15  WS-FOUND-COUNT  PIC S9(4) COMP.                  ELTEXBEN
00119                                                                   ELTEXBEN
00120 /*****************************************************************ELTEXBEN
00121 ** LINES FOR BUILDING SCREEN OUTPUT                             **ELTEXBEN
00122 ******************************************************************ELTEXBEN
00123  01  WS-HEADER-LINES.                                             ELTEXBEN
00124      05  WS-HDR-LINE-2A.                                          ELTEXBEN
00125        10  FILLER                 PIC X(24) VALUE SPACES.         ELTEXBEN
00126        10  FILLER                 PIC X(31) VALUE                 ELTEXBEN
00127            'INSTITUTIONAL EXCLUDED BENEFITS'.                     ELTEXBEN
00128        10  FILLER                 PIC X(24) VALUE SPACES.         ELTEXBEN
00129                                                                   ELTEXBEN
00130      05  WS-HDR-LINE-2B.                                          ELTEXBEN
00131        10  FILLER                 PIC X(25) VALUE SPACES.         ELTEXBEN
00132        10  FILLER                 PIC X(30) VALUE                 ELTEXBEN
00133            'PROFESSIONAL EXCLUDED BENEFITS'.                      ELTEXBEN
00134        10  FILLER                 PIC X(24) VALUE SPACES.         ELTEXBEN
00135                                                                   ELTEXBEN
00136      05  WS-HDR-LINE-2C.                                          ELTEXBEN
00137        10  FILLER                 PIC X(18) VALUE SPACES.         ELTEXBEN
00138        10  FILLER                 PIC X(44) VALUE                 ELTEXBEN
00139            'SUPPLEMENTAL INSTITUTIONAL EXCLUDED BENEFITS'.        ELTEXBEN
00140        10  FILLER                 PIC X(18) VALUE SPACES.         ELTEXBEN
00141                                                                   ELTEXBEN
00142      05  WS-HDR-LINE-2D.                                          ELTEXBEN
00143        10  FILLER                 PIC X(18) VALUE SPACES.         ELTEXBEN
00144        10  FILLER                 PIC X(43) VALUE                 ELTEXBEN
00145            'SUPPLEMENTAL PROFESSIONAL EXCLUDED BENEFITS'.         ELTEXBEN
00146        10  FILLER                 PIC X(19) VALUE SPACES.         ELTEXBEN
00147                                                                   ELTEXBEN
00148 **************************************************************    ELTEXBEN
00149 *** SCREEN BODY LINES                                             ELTEXBEN
00150 **************************************************************    ELTEXBEN
00151      05  WS-BLANK-LINE            PIC X(79) VALUE SPACES.         ELTEXBEN
00152      05  WS-ONLY-LINE             PIC X(79) VALUE SPACES.         ELTEXBEN
00153      05  WS-ALL-EXCLUDED          PIC X(26) VALUE                 ELTEXBEN
00154          'ALL BENEFITS ARE EXCLUDED.'.                            ELTEXBEN
00155      05  WS-NONE-EXCLUDED         PIC X(25) VALUE                 ELTEXBEN
00156          'NO BENEFITS ARE EXCLUDED.'.                             ELTEXBEN
00157      TITLE ' SUPPRESSED BENEFIT PROVISIONS'.                      ELTEXBEN
00158      COPY ELSBPTBL.                                               ELTEXBEN
00159      TITLE 'LINKAGE SECTION'.                                     ELTEXBEN
00160  LINKAGE SECTION.                                                 ELTEXBEN
00161  01  DFHCOMMAREA.                                                 ELTEXBEN
00162      COPY ELSCOMMC.                                               ELTEXBEN
00163 /                                                                 ELTEXBEN
00164      COPY ELSCIA2C.                                               ELTEXBEN
00165 /                                                                 ELTEXBEN
00166      COPY ELSCMDSC.                                               ELTEXBEN
00167 /                                                                 ELTEXBEN
00168      COPY ELSCMIFC.                                               ELTEXBEN
00169 /                                                                 ELTEXBEN
00170      COPY ELSIOPMC.                                               ELTEXBEN
00171 /                                                                 ELTEXBEN
00172      COPY ELSKEYSC.                                               ELTEXBEN
00173 /                                                                 ELTEXBEN
00174      COPY ELSOUTPC.                                               ELTEXBEN
00175 /                                                                 ELTEXBEN
00176      COPY ELSSSCBC.                                               ELTEXBEN
00177 /                                                                 ELTEXBEN
00178  01  ELR-GRP-REC-AREA.                                            ELTEXBEN
00179      COPY GCGROUPC.                                               ELTEXBEN
00180 /                                                                 ELTEXBEN
00181  01  ELR-CONTR-REC-AREA.                                          ELTEXBEN
00182      COPY GCCONTRC.                                               ELTEXBEN
00183 /*****************************************************************ELTEXBEN
00184 ** GCPS FIELD VALIDATION RECORD (CONTAINS VALID 6-CHAR.         **ELTEXBEN
00185 ** BENEFIT PROVISION IDENTIFIERS)                               **ELTEXBEN
00186 ******************************************************************ELTEXBEN
00187  01  GCFLDVL2.                                                    ELTEXBEN
00188      COPY GCV4LRCC.                                               ELTEXBEN
00189                                                                   ELTEXBEN
00190 /*****************************************************************ELTEXBEN
00191 ** WORK TABLE AREA OBTAINED BY GETMAIN                          **ELTEXBEN
00192 ******************************************************************ELTEXBEN
00193  01  LS-BEN-TABLE.                                                ELTEXBEN
00194      05  LS-BEN-ACTIVE-ITEMS     PIC S9(4) COMP.                  ELTEXBEN
00195      05  LS-BEN-ITEM OCCURS 1 TO  737 TIMES                       ELTEXBEN
00196                      DEPENDING ON LS-BEN-ACTIVE-ITEMS             ELTEXBEN
00197                      ASCENDING KEY IS LS-BEN-ID-5                 ELTEXBEN
00198                      INDEXED BY LS-BEN-INDEX1.                    ELTEXBEN
00199          10  LS-BEN-ID-5                   PIC X(5).              ELTEXBEN
00200          10  LS-BEN-FLAGS.                                        ELTEXBEN
00201              15  LS-BEN-INST-BASIC-FLAG    PIC X.                 ELTEXBEN
00202                  88  INST-BASIC-FOUND      VALUE 'Y'.             ELTEXBEN
00203                  88  INST-BASIC-NOT-FOUND  VALUE 'N'.             ELTEXBEN
00204                  88  INST-BASIC-UNDEFINED  VALUE 'X'.             ELTEXBEN
00205              15  LS-BEN-INST-SUPP-FLAG     PIC X.                 ELTEXBEN
00206                  88  INST-SUPP-FOUND       VALUE 'Y'.             ELTEXBEN
00207                  88  INST-SUPP-NOT-FOUND   VALUE 'N'.             ELTEXBEN
00208                  88  INST-SUPP-UNDEFINED   VALUE 'X'.             ELTEXBEN
00209              15  LS-BEN-PROF-BASIC-FLAG    PIC X.                 ELTEXBEN
00210                  88  PROF-BASIC-FOUND      VALUE 'Y'.             ELTEXBEN
00211                  88  PROF-BASIC-NOT-FOUND  VALUE 'N'.             ELTEXBEN
00212                  88  PROF-BASIC-UNDEFINED  VALUE 'X'.             ELTEXBEN
00213              15  LS-BEN-PROF-SUPP-FLAG     PIC X.                 ELTEXBEN
00214                  88  PROF-SUPP-FOUND       VALUE 'Y'.             ELTEXBEN
00215                  88  PROF-SUPP-NOT-FOUND   VALUE 'N'.             ELTEXBEN
00216                  88  PROF-SUPP-UNDEFINED   VALUE 'X'.             ELTEXBEN
00217          10  LS-BEN-FLAGS-RDF REDEFINES LS-BEN-FLAGS.             ELTEXBEN
00218              15  LS-BEN-FLAG OCCURS 4 TIMES                       ELTEXBEN
00219                              INDEXED BY LS-BEN-INDEX2             ELTEXBEN
00220                                            PIC X.                 ELTEXBEN
00221                  88  BENEFIT-FOUND         VALUE 'Y'.             ELTEXBEN
00222                  88  BENEFIT-NOT-FOUND     VALUE 'N'.             ELTEXBEN
00223                  88  BENEFIT-UNDEFINED     VALUE 'X'.             ELTEXBEN
00224      TITLE 'PROCEDURE-DIVISION'.                                  ELTEXBEN
00225  PROCEDURE DIVISION.                                              ELTEXBEN
00226                                                                   ELTEXBEN
00227 ************************************************************      ELTEXBEN
00228 *                                                          *      ELTEXBEN
00229 *        TOPIC EXCLUDED BENEFITS                           *      ELTEXBEN
00230 *                                                          *      ELTEXBEN
00231 ************************************************************      ELTEXBEN
00232  TOPIC-EXCLUDED-BENEFITS.                                         ELTEXBEN
00233                                                                   ELTEXBEN
00234      PERFORM EST-ADDRESS-OF-CNTRL-BLKS.                           ELTEXBEN
00235      PERFORM EST-ADDRESS-OF-WORK-AREAS.                           ELTEXBEN
00236      PERFORM PROCESS-BEN-PROVN.                                   ELTEXBEN
00237      GOBACK.                                                      ELTEXBEN
00238                                                                   ELTEXBEN
00239      TITLE 'ESTABLISH ADDRESSABILITY OF CONTROL BLKS'.            ELTEXBEN
00240 ************************************************************      ELTEXBEN
00241 *                                                          *      ELTEXBEN
00242 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELTEXBEN
00243 *                                                          *      ELTEXBEN
00244 ************************************************************      ELTEXBEN
00245  EST-ADDRESS-OF-CNTRL-BLKS.                                       ELTEXBEN
00246 *                                                                 ELTEXBEN
00247      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTEXBEN
00248         EXEC CICS ABEND                                           ELTEXBEN
00249                ABCODE('EL01')                                     ELTEXBEN
00250         END-EXEC.                                                 ELTEXBEN
00251 *                                                                 ELTEXBEN
00252      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTEXBEN
00253            ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.              ELTEXBEN
00254                                                                   ELTEXBEN
00255      IF ECA-CIA-PTR = NULL                                        ELTEXBEN
00256          EXEC CICS ABEND                                          ELTEXBEN
00257                ABCODE('EL02')                                     ELTEXBEN
00258           END-EXEC.                                               ELTEXBEN
00259                                                                   ELTEXBEN
00260 *                                                                 ELTEXBEN
00261      SET  CIA-ELSSSCB-DDN TO TRUE.                                ELTEXBEN
00262                                                                   ELTEXBEN
00263      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXBEN
00264            ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                ELTEXBEN
00265                                                                   ELTEXBEN
00266      IF CIA-RC-PTR-NULL                                           ELTEXBEN
00267          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTEXBEN
00268                                                                   ELTEXBEN
00269         TITLE 'ESTABLISH ADDRESSABILITY OF WORK AREAS'.           ELTEXBEN
00270 ************************************************************      ELTEXBEN
00271 *                                                          *      ELTEXBEN
00272 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTEXBEN
00273 *                                                          *      ELTEXBEN
00274 ************************************************************      ELTEXBEN
00275  EST-ADDRESS-OF-WORK-AREAS.                                       ELTEXBEN
00276                                                                   ELTEXBEN
00277 *===>     ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC              ELTEXBEN
00278                                                                   ELTEXBEN
00279      SET  CIA-ELSGRPSP-DDN TO TRUE.                               ELTEXBEN
00280                                                                   ELTEXBEN
00281      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXBEN
00282           ADDRESS OF ELR-GRP-REC-AREA.                            ELTEXBEN
00283                                                                   ELTEXBEN
00284      IF CIA-RC-PTR-NULL                                           ELTEXBEN
00285          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTEXBEN
00286                                                                   ELTEXBEN
00287 *===>   ESTABLISH ADDRESSABILITY OF CODES MANUAL INTERFACE        ELTEXBEN
00288                                                                   ELTEXBEN
00289      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTEXBEN
00290                                                                   ELTEXBEN
00291      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXBEN
00292           ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                  ELTEXBEN
00293                                                                   ELTEXBEN
00294      IF CIA-RC-PTR-NULL                                           ELTEXBEN
00295          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTEXBEN
00296                                                                   ELTEXBEN
00297 *==>     ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE             ELTEXBEN
00298                                                                   ELTEXBEN
00299      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTEXBEN
00300                                                                   ELTEXBEN
00301      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXBEN
00302            ADDRESS OF COF-OUTPUT-INTERFACE.                       ELTEXBEN
00303                                                                   ELTEXBEN
00304      IF CIA-RC-PTR-NULL                                           ELTEXBEN
00305          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTEXBEN
00306                                                                   ELTEXBEN
00307 *===>     ESTABLISH ADDRESSABILITY OF KEY WORK AREA               ELTEXBEN
00308                                                                   ELTEXBEN
00309      SET  CIA-ELSKEYS-DDN TO TRUE.                                ELTEXBEN
00310                                                                   ELTEXBEN
00311      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXBEN
00312            ADDRESS OF KWA-FILE-KEY-WORK-AREA.                     ELTEXBEN
00313                                                                   ELTEXBEN
00314      IF CIA-RC-PTR-NULL                                           ELTEXBEN
00315          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTEXBEN
00316                                                                   ELTEXBEN
00317         TITLE 'SIGNAL UNALLOC AREAR ERROR'.                       ELTEXBEN
00318 ************************************************************      ELTEXBEN
00319 *                                                          *      ELTEXBEN
00320 *        SIGNAL UNALLOC AREA ERROR                         *      ELTEXBEN
00321 *                                                          *      ELTEXBEN
00322 ************************************************************      ELTEXBEN
00323  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTEXBEN
00324      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTEXBEN
00325      EXEC CICS ABEND                                              ELTEXBEN
00326                ABCODE(CIA-ABCODE)                                 ELTEXBEN
00327         END-EXEC.                                                 ELTEXBEN
00328                                                                   ELTEXBEN
00329      TITLE 'LINK TO STORAGE MANAGER'.                             ELTEXBEN
00330 ************************************************************      ELTEXBEN
00331 *                                                          *      ELTEXBEN
00332 *        LINK TO STORAGE MANAGER                           *      ELTEXBEN
00333 *                                                          *      ELTEXBEN
00334 ************************************************************      ELTEXBEN
00335  LINK-TO-STORAGE-MANAGER.                                         ELTEXBEN
00336      SET CIA-STG-GETMAIN TO TRUE.                                 ELTEXBEN
00337      EXEC CICS LINK                                               ELTEXBEN
00338                PROGRAM ('ELUSTGMG')                               ELTEXBEN
00339                COMMAREA (DFHCOMMAREA)                             ELTEXBEN
00340                END-EXEC.                                          ELTEXBEN
00341                                                                   ELTEXBEN
00342      TITLE 'PROCESS BEN PROVN'.                                   ELTEXBEN
00343 ************************************************************      ELTEXBEN
00344 *                                                          *      ELTEXBEN
00345 *        PROCESS BENEFIT PROVISION                         *      ELTEXBEN
00346 *                                                          *      ELTEXBEN
00347 ************************************************************      ELTEXBEN
00348  PROCESS-BEN-PROVN.                                               ELTEXBEN
00349      SET WS-FIRST-TIME TO TRUE.                                   ELTEXBEN
00350      PERFORM BUILD-BENEFIT-TABLE.                                 ELTEXBEN
00351      PERFORM PROCESS-BENEFIT-TABLE.                               ELTEXBEN
00352      PERFORM GENERATE-OUTPUT.                                     ELTEXBEN
00353      PERFORM END-OUTPUT-PAGE.                                     ELTEXBEN
00354                                                                   ELTEXBEN
00355      TITLE 'BUILD BENEFIT TABLE'.                                 ELTEXBEN
00356 ************************************************************      ELTEXBEN
00357 *                                                          *      ELTEXBEN
00358 *        BUILD BENEFIT TABLE                               *      ELTEXBEN
00359 *                                                          *      ELTEXBEN
00360 ************************************************************      ELTEXBEN
00361  BUILD-BENEFIT-TABLE.                                             ELTEXBEN
00362       PERFORM ESTABLISH-BENEFIT-TABLE.                            ELTEXBEN
00363       MOVE 0 TO LS-BEN-ACTIVE-ITEMS.                              ELTEXBEN
00364       SET LS-BEN-INDEX1 TO 1.                                     ELTEXBEN
00365       SET LS-BEN-INDEX1 DOWN BY 1.                                ELTEXBEN
00366       PERFORM VARYING BP-SUB FROM 1 BY 1                          ELTEXBEN
00367          UNTIL BP-SUB > BPL-NBR-TBL-ENTRIES                       ELTEXBEN
00368             SET BPL-IDX TO BP-SUB                                 ELTEXBEN
00369             IF    (BPL-NOT-ALT-PROVN (BPL-IDX)                    ELTEXBEN
00370               AND BPL-STD-PROVN (BPL-IDX))                        ELTEXBEN
00371               PERFORM ACCEPT-PROVISION-ID                         ELTEXBEN
00372             END-IF                                                ELTEXBEN
00373       END-PERFORM.                                                ELTEXBEN
00374                                                                   ELTEXBEN
00375 ************************************************************      ELTEXBEN
00376 *                                                          *      ELTEXBEN
00377 *        ESTABLISH BENEFIT TABLE                           *      ELTEXBEN
00378 *LS-TABLE-LENGTH MUST BE UPDATED EACH TIME MAX IS UPDATED  *      ELTEXBEN
00379 ************************************************************      ELTEXBEN
00380  ESTABLISH-BENEFIT-TABLE.                                         ELTEXBEN
00381       COMPUTE CIA-AREA-LEN = LENGTH OF LS-BEN-ACTIVE-ITEMS +      ELTEXBEN
00382           WS-LS-MAX * (LENGTH OF LS-BEN-ID-5 + LENGTH OF          ELTEXBEN
00383              LS-BEN-ITEM).                                        ELTEXBEN
00384       SET CIA-STG-GETMAIN TO TRUE.                                ELTEXBEN
00385       SET CIA-ELSPGMW1-DDN TO TRUE.                               ELTEXBEN
00386       PERFORM LINK-TO-STORAGE-MANAGER.                            ELTEXBEN
00387       SET CIA-ELSPGMW1-DDN TO TRUE.                               ELTEXBEN
00388       CALL 'ELUSETAD' USING DFHCOMMAREA                           ELTEXBEN
00389                             ADDRESS OF LS-BEN-TABLE.              ELTEXBEN
00390       IF CIA-RC-PTR-NULL                                          ELTEXBEN
00391          PERFORM SIGNAL-UNALLOC-AREA.                             ELTEXBEN
00392                                                                   ELTEXBEN
00393 ************************************************************      ELTEXBEN
00394 *                                                          *      ELTEXBEN
00395 *        ACCEPT PROVISION ID                               *      ELTEXBEN
00396 *                                                          *      ELTEXBEN
00397 ************************************************************      ELTEXBEN
00398  ACCEPT-PROVISION-ID.                                             ELTEXBEN
00399      IF WS-FIRST-TIME                                             ELTEXBEN
00400        PERFORM DO-BP-PROVN-MOVE                                   ELTEXBEN
00401        SET WS-NOT-FIRST-TIME TO TRUE                              ELTEXBEN
00402      ELSE                                                         ELTEXBEN
00403         IF BPL-BP-PROVN (BPL-IDX) = LS-BEN-ID-5 (LS-BEN-INDEX1)   ELTEXBEN
00404           CONTINUE                                                ELTEXBEN
00405         ELSE                                                      ELTEXBEN
00406            PERFORM DO-BP-PROVN-MOVE                               ELTEXBEN
00407         END-IF                                                    ELTEXBEN
00408      END-IF.                                                      ELTEXBEN
00409                                                                   ELTEXBEN
00410      IF BPL-INST-PROVN (BPL-IDX)                                  ELTEXBEN
00411        SET INST-BASIC-NOT-FOUND (LS-BEN-INDEX1) TO TRUE           ELTEXBEN
00412        SET INST-SUPP-NOT-FOUND (LS-BEN-INDEX1) TO TRUE            ELTEXBEN
00413      ELSE                                                         ELTEXBEN
00414           SET PROF-BASIC-NOT-FOUND (LS-BEN-INDEX1) TO TRUE        ELTEXBEN
00415           SET PROF-SUPP-NOT-FOUND (LS-BEN-INDEX1) TO TRUE.        ELTEXBEN
00416                                                                   ELTEXBEN
00417 ************************************************************      ELTEXBEN
00418 *                                                          *      ELTEXBEN
00419 *        DO BP PROVN MOVE                                  *      ELTEXBEN
00420 *                                                          *      ELTEXBEN
00421 ************************************************************      ELTEXBEN
00422  DO-BP-PROVN-MOVE.                                                ELTEXBEN
00423      SET LS-BEN-INDEX1 UP BY 1.                                   ELTEXBEN
00424      ADD 1 TO LS-BEN-ACTIVE-ITEMS.                                ELTEXBEN
00425      MOVE BPL-BP-PROVN (BPL-IDX)                                  ELTEXBEN
00426            TO LS-BEN-ID-5 (LS-BEN-INDEX1).                        ELTEXBEN
00427      MOVE 'XXXX' TO LS-BEN-FLAGS (LS-BEN-INDEX1).                 ELTEXBEN
00428                                                                   ELTEXBEN
00429 /                                                                 ELTEXBEN
00430 ************************************************************      ELTEXBEN
00431 *                                                          *      ELTEXBEN
00432 *        PROCESS BENEFIT TABLE                            *       ELTEXBEN
00433 *                                                          *      ELTEXBEN
00434 ************************************************************      ELTEXBEN
00435  PROCESS-BENEFIT-TABLE.                                           ELTEXBEN
00436      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTEXBEN
00437      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXBEN
00438             ADDRESS OF ELR-CONTR-REC-AREA.                        ELTEXBEN
00439      IF CIA-RC-PTR-NULL                                           ELTEXBEN
00440         CONTINUE                                                  ELTEXBEN
00441      ELSE                                                         ELTEXBEN
00442         SET WS-INST-BASIC-CNT TO TRUE                             ELTEXBEN
00443         SET PROCESSING-INST TO TRUE                               ELTEXBEN
00444         SET LS-BEN-INDEX2 TO 1                                    ELTEXBEN
00445         SET WS-COUNT-INDEX TO 1                                   ELTEXBEN
00446         PERFORM SCAN-PROVISION-LIST.                              ELTEXBEN
00447      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTEXBEN
00448      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXBEN
00449             ADDRESS OF ELR-CONTR-REC-AREA.                        ELTEXBEN
00450      IF CIA-RC-PTR-NULL                                           ELTEXBEN
00451         CONTINUE                                                  ELTEXBEN
00452      ELSE                                                         ELTEXBEN
00453         SET WS-INST-SUPPL-CNT TO TRUE                             ELTEXBEN
00454         SET PROCESSING-INST TO TRUE                               ELTEXBEN
00455         SET LS-BEN-INDEX2 TO 2                                    ELTEXBEN
00456         SET WS-COUNT-INDEX TO 2                                   ELTEXBEN
00457         PERFORM SCAN-PROVISION-LIST.                              ELTEXBEN
00458      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTEXBEN
00459      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXBEN
00460             ADDRESS OF ELR-CONTR-REC-AREA.                        ELTEXBEN
00461      IF CIA-RC-PTR-NULL                                           ELTEXBEN
00462         CONTINUE                                                  ELTEXBEN
00463      ELSE                                                         ELTEXBEN
00464         SET WS-PROF-BASIC-CNT TO TRUE                             ELTEXBEN
00465         SET PROCESSING-PROF TO TRUE                               ELTEXBEN
00466         SET LS-BEN-INDEX2 TO 3                                    ELTEXBEN
00467         SET WS-COUNT-INDEX TO 3                                   ELTEXBEN
00468         PERFORM SCAN-PROVISION-LIST.                              ELTEXBEN
00469      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTEXBEN
00470      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXBEN
00471             ADDRESS OF ELR-CONTR-REC-AREA.                        ELTEXBEN
00472      IF CIA-RC-PTR-NULL                                           ELTEXBEN
00473         CONTINUE                                                  ELTEXBEN
00474      ELSE                                                         ELTEXBEN
00475         SET WS-PROF-SUPPL-CNT TO TRUE                             ELTEXBEN
00476         SET PROCESSING-PROF TO TRUE                               ELTEXBEN
00477         SET LS-BEN-INDEX2 TO 4                                    ELTEXBEN
00478         SET WS-COUNT-INDEX TO 4                                   ELTEXBEN
00479         PERFORM SCAN-PROVISION-LIST.                              ELTEXBEN
00480 /                                                                 ELTEXBEN
00481 ************************************************************      ELTEXBEN
00482 *                                                          *      ELTEXBEN
00483 *        SCAN PROVISION LIST                               *      ELTEXBEN
00484 *                                                          *      ELTEXBEN
00485 ************************************************************      ELTEXBEN
00486  SCAN-PROVISION-LIST.                                             ELTEXBEN
00487      SET GCT-INDEX TO 1.                                          ELTEXBEN
00488      SET LS-BEN-INDEX1 TO 1.                                      ELTEXBEN
00489      SET IN-PROGRESS TO TRUE.                                     ELTEXBEN
00490      PERFORM WITH TEST BEFORE UNTIL DONE                          ELTEXBEN
00491         EVALUATE TRUE                                             ELTEXBEN
00492         WHEN GCT-BP-ID (GCT-INDEX) < LS-BEN-ID-5 (LS-BEN-INDEX1)  ELTEXBEN
00493            SET GCT-INDEX UP BY 1                                  ELTEXBEN
00494         WHEN GCT-BP-ID (GCT-INDEX) = LS-BEN-ID-5 (LS-BEN-INDEX1)  ELTEXBEN
00495            IF PROCESSING-INST                                     ELTEXBEN
00496               IF WS-INST-BASIC-CNT OR WS-INST-SUPPL-CNT           ELTEXBEN
00497                  SET BENEFIT-FOUND (LS-BEN-INDEX1, LS-BEN-INDEX2) ELTEXBEN
00498                    TO TRUE                                        ELTEXBEN
00499                    ADD 1 TO WS-FOUND-COUNT (WS-COUNT-INDEX)       ELTEXBEN
00500               END-IF                                              ELTEXBEN
00501            ELSE                                                   ELTEXBEN
00502               IF WS-PROF-BASIC-CNT OR WS-PROF-SUPPL-CNT           ELTEXBEN
00503                  SET BENEFIT-FOUND                                ELTEXBEN
00504                       (LS-BEN-INDEX1, LS-BEN-INDEX2)              ELTEXBEN
00505                     TO TRUE                                       ELTEXBEN
00506                  ADD 1 TO WS-FOUND-COUNT (WS-COUNT-INDEX)         ELTEXBEN
00507               END-IF                                              ELTEXBEN
00508            END-IF                                                 ELTEXBEN
00509            SET GCT-INDEX UP BY 1                                  ELTEXBEN
00510            SET LS-BEN-INDEX1 UP BY 1                              ELTEXBEN
00511         WHEN OTHER                                                ELTEXBEN
00512            SET LS-BEN-INDEX1 UP BY 1                              ELTEXBEN
00513         END-EVALUATE                                              ELTEXBEN
00514         IF   LS-BEN-INDEX1 > LS-BEN-ACTIVE-ITEMS                  ELTEXBEN
00515           OR GCT-INDEX > GCT-COUNT-BEN-PROVN-POINTERS             ELTEXBEN
00516              SET DONE TO TRUE                                     ELTEXBEN
00517          END-IF                                                   ELTEXBEN
00518       END-PERFORM.                                                ELTEXBEN
00519                                                                   ELTEXBEN
00520      TITLE 'GENERATE OUTPUT'.                                     ELTEXBEN
00521 ************************************************************      ELTEXBEN
00522 *                                                          *      ELTEXBEN
00523 *        GENERATE OUTPUT                                   *      ELTEXBEN
00524 *                                                          *      ELTEXBEN
00525 ************************************************************      ELTEXBEN
00526  GENERATE-OUTPUT.                                                 ELTEXBEN
00527      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH)  AND         ELTEXBEN
00528              WS-INST-BASIC-CNT                                    ELTEXBEN
00529          PERFORM GENERATE-INSTITUTIONAL-BASIC-O.                  ELTEXBEN
00530                                                                   ELTEXBEN
00531      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH)  AND         ELTEXBEN
00532              WS-INST-SUPPL-CNT                                    ELTEXBEN
00533          PERFORM GENERATE-INSTITUTIONAL-SUPP-OU.                  ELTEXBEN
00534                                                                   ELTEXBEN
00535      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH)  AND         ELTEXBEN
00536              WS-PROF-BASIC-CNT                                    ELTEXBEN
00537          PERFORM GENERATE-PROFESSIONAL-BASIC-OU.                  ELTEXBEN
00538                                                                   ELTEXBEN
00539      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH)  AND         ELTEXBEN
00540              WS-PROF-SUPPL-CNT                                    ELTEXBEN
00541          PERFORM GENERATE-PROFESSIONAL-SUPP-OUT.                  ELTEXBEN
00542                                                                   ELTEXBEN
00543                                                                   ELTEXBEN
00544 /***********************************************************      ELTEXBEN
00545 *                                                          *      ELTEXBEN
00546 *        GENERATE INSTITUTIONAL BASIC OUTPUT               *      ELTEXBEN
00547 *                                                          *      ELTEXBEN
00548 ************************************************************      ELTEXBEN
00549  GENERATE-INSTITUTIONAL-BASIC-O.                                  ELTEXBEN
00550      MOVE WS-HDR-LINE-2A TO COF-HDR-LINE (2).                     ELTEXBEN
00551      PERFORM CREATE-HEADER.                                       ELTEXBEN
00552      SET LS-BEN-INDEX2 TO 1.                                      ELTEXBEN
00553      SET WS-COUNT-INDEX TO 1.                                     ELTEXBEN
00554      PERFORM BUILD-OUTPUT.                                        ELTEXBEN
00555                                                                   ELTEXBEN
00556                                                                   ELTEXBEN
00557 /***********************************************************      ELTEXBEN
00558 *                                                          *      ELTEXBEN
00559 *        GENERATE INSTITUTIONAL SUPP OUTPUT                *      ELTEXBEN
00560 *                                                          *      ELTEXBEN
00561 ************************************************************      ELTEXBEN
00562  GENERATE-INSTITUTIONAL-SUPP-OU.                                  ELTEXBEN
00563      MOVE WS-HDR-LINE-2C TO COF-HDR-LINE (2).                     ELTEXBEN
00564      PERFORM CREATE-HEADER.                                       ELTEXBEN
00565      SET LS-BEN-INDEX2 TO 2.                                      ELTEXBEN
00566      SET WS-COUNT-INDEX TO 2.                                     ELTEXBEN
00567      PERFORM BUILD-OUTPUT.                                        ELTEXBEN
00568                                                                   ELTEXBEN
00569                                                                   ELTEXBEN
00570 /***********************************************************      ELTEXBEN
00571 *                                                          *      ELTEXBEN
00572 *        GENERATE PROFESSIONAL BASIC OUTPUT                *      ELTEXBEN
00573 *                                                          *      ELTEXBEN
00574 ************************************************************      ELTEXBEN
00575  GENERATE-PROFESSIONAL-BASIC-OU.                                  ELTEXBEN
00576      MOVE WS-HDR-LINE-2B TO COF-HDR-LINE (2).                     ELTEXBEN
00577      PERFORM CREATE-HEADER.                                       ELTEXBEN
00578      SET LS-BEN-INDEX2 TO 3.                                      ELTEXBEN
00579      SET WS-COUNT-INDEX TO 3.                                     ELTEXBEN
00580      PERFORM BUILD-OUTPUT.                                        ELTEXBEN
00581                                                                   ELTEXBEN
00582                                                                   ELTEXBEN
00583 /***********************************************************      ELTEXBEN
00584 *                                                          *      ELTEXBEN
00585 *        GENERATE PROFESSIONAL SUPP OUTPUT                 *      ELTEXBEN
00586 *                                                          *      ELTEXBEN
00587 ************************************************************      ELTEXBEN
00588  GENERATE-PROFESSIONAL-SUPP-OUT.                                  ELTEXBEN
00589      MOVE WS-HDR-LINE-2D TO COF-HDR-LINE (2).                     ELTEXBEN
00590      PERFORM CREATE-HEADER.                                       ELTEXBEN
00591      SET LS-BEN-INDEX2 TO 4.                                      ELTEXBEN
00592      SET WS-COUNT-INDEX TO 4.                                     ELTEXBEN
00593      PERFORM BUILD-OUTPUT.                                        ELTEXBEN
00594                                                                   ELTEXBEN
00595                                                                   ELTEXBEN
00596 /***********************************************************      ELTEXBEN
00597 *                                                          *      ELTEXBEN
00598 *        CREATE HEADER                                     *      ELTEXBEN
00599 *                                                          *      ELTEXBEN
00600 ************************************************************      ELTEXBEN
00601  CREATE-HEADER.                                                   ELTEXBEN
00602      SET COF-NEW-PAGE TO TRUE.                                    ELTEXBEN
00603      MOVE 2 TO COF-NBR-HDR-LINES.                                 ELTEXBEN
00604      MOVE ZERO TO COF-NBR-DTL-LINES.                              ELTEXBEN
00605      PERFORM CALL-ELUOUTPT.                                       ELTEXBEN
00606                                                                   ELTEXBEN
00607 /***********************************************************      ELTEXBEN
00608 *                                                          *      ELTEXBEN
00609 *        BUILD OUTPUT                                      *      ELTEXBEN
00610 *                                                          *      ELTEXBEN
00611 ************************************************************      ELTEXBEN
00612  BUILD-OUTPUT.                                                    ELTEXBEN
00613      IF WS-FOUND-COUNT (WS-COUNT-INDEX) = ZERO                    ELTEXBEN
00614          PERFORM PRINT-ALL-EXCLUDED-PHRASE                        ELTEXBEN
00615      ELSE IF WS-FOUND-COUNT (WS-COUNT-INDEX) =                    ELTEXBEN
00616                          WS-APPLICABLE (WS-COUNT-INDEX)           ELTEXBEN
00617          PERFORM PRINT-NONE-EXCLUDED-PHRASE                       ELTEXBEN
00618      ELSE                                                         ELTEXBEN
00619          PERFORM PRINT-EXCLUDED-LINE.                             ELTEXBEN
00620                                                                   ELTEXBEN
00621                                                                   ELTEXBEN
00622 ************************************************************      ELTEXBEN
00623 *                                                          *      ELTEXBEN
00624 *        PRINT ALL EXCLUDED PHRASE                         *      ELTEXBEN
00625 *                                                          *      ELTEXBEN
00626 ************************************************************      ELTEXBEN
00627  PRINT-ALL-EXCLUDED-PHRASE.                                       ELTEXBEN
00628      MOVE +2 TO COF-NBR-DTL-LINES.                                ELTEXBEN
00629      MOVE SPACES TO COF-DTL-LINE (1).                             ELTEXBEN
00630      MOVE WS-ALL-EXCLUDED TO COF-DTL-LINE (2).                    ELTEXBEN
00631      PERFORM CALL-ELUOUTPT.                                       ELTEXBEN
00632                                                                   ELTEXBEN
00633                                                                   ELTEXBEN
00634 ************************************************************      ELTEXBEN
00635 *                                                          *      ELTEXBEN
00636 *        PRINT NONE EXCLUDED PHRASE                        *      ELTEXBEN
00637 *                                                          *      ELTEXBEN
00638 ************************************************************      ELTEXBEN
00639  PRINT-NONE-EXCLUDED-PHRASE.                                      ELTEXBEN
00640      MOVE +2 TO COF-NBR-DTL-LINES.                                ELTEXBEN
00641      MOVE SPACES TO COF-DTL-LINE (1).                             ELTEXBEN
00642      MOVE WS-NONE-EXCLUDED TO COF-DTL-LINE (2).                   ELTEXBEN
00643      PERFORM CALL-ELUOUTPT.                                       ELTEXBEN
00644                                                                   ELTEXBEN
00645 ************************************************************      ELTEXBEN
00646 *                                                          *      ELTEXBEN
00647 *        PRINT EXCLUDED LINE                               *      ELTEXBEN
00648 *                                                          *      ELTEXBEN
00649 ************************************************************      ELTEXBEN
00650  PRINT-EXCLUDED-LINE.                                             ELTEXBEN
00651      PERFORM BUILD-EXCLUDED-LINE                                  ELTEXBEN
00652          VARYING LS-BEN-INDEX1 FROM 1 BY 1                        ELTEXBEN
00653                     UNTIL LS-BEN-INDEX1 >                         ELTEXBEN
00654              LS-BEN-ACTIVE-ITEMS.                                 ELTEXBEN
00655                                                                   ELTEXBEN
00656 ************************************************************      ELTEXBEN
00657 *                                                          *      ELTEXBEN
00658 *        BUILD EXCLUDED LINE                               *      ELTEXBEN
00659 *                                                          *      ELTEXBEN
00660 ************************************************************      ELTEXBEN
00661  BUILD-EXCLUDED-LINE.                                             ELTEXBEN
00662      IF BENEFIT-NOT-FOUND (LS-BEN-INDEX1 LS-BEN-INDEX2)           ELTEXBEN
00663          PERFORM WRITE-EXCLUDED-VERBIAGE.                         ELTEXBEN
00664                                                                   ELTEXBEN
00665                                                                   ELTEXBEN
00666 ************************************************************      ELTEXBEN
00667 *                                                          *      ELTEXBEN
00668 *        WRITE EXCLUDED VERBIAGE                           *      ELTEXBEN
00669 *                                                          *      ELTEXBEN
00670 ************************************************************      ELTEXBEN
00671  WRITE-EXCLUDED-VERBIAGE.                                         ELTEXBEN
00672      MOVE 'BP' TO CMF-RECORD-PREFIX.                              ELTEXBEN
00673      MOVE LS-BEN-ID-5 (LS-BEN-INDEX1) TO                          ELTEXBEN
00674          CMF-CODE-VALUE.                                          ELTEXBEN
00675      MOVE 'BEN-PR-ID' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTEXBEN
00676      PERFORM LINK-TO-CODES-MANUAL.                                ELTEXBEN
00677      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTEXBEN
00678      MOVE CMF-DESCR-LINE (1)                                      ELTEXBEN
00679                 TO COF-DTL-LINE (COF-NBR-DTL-LINES).              ELTEXBEN
00680      PERFORM CALL-ELUOUTPT.                                       ELTEXBEN
00681                                                                   ELTEXBEN
00682                                                                   ELTEXBEN
00683 ************************************************************      ELTEXBEN
00684 *                                                          *      ELTEXBEN
00685 *        LINK TO CODES MANUAL                              *      ELTEXBEN
00686 *                                                          *      ELTEXBEN
00687 ************************************************************      ELTEXBEN
00688  LINK-TO-CODES-MANUAL.                                            ELTEXBEN
00689      EXEC CICS LINK                                               ELTEXBEN
00690                PROGRAM ('ELUCMIF')                                ELTEXBEN
00691                COMMAREA (DFHCOMMAREA)                             ELTEXBEN
00692                END-EXEC.                                          ELTEXBEN
00693      SET CIA-ELSCMDSC-DDN     TO TRUE.                            ELTEXBEN
00694                                                                   ELTEXBEN
00695      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXBEN
00696            ADDRESS OF CMF-DESCR.                                  ELTEXBEN
00697                                                                   ELTEXBEN
00698 ************************************************************      ELTEXBEN
00699 *                                                          *      ELTEXBEN
00700 *        END OUTPUT PAGE                                   *      ELTEXBEN
00701 *                                                          *      ELTEXBEN
00702 ************************************************************      ELTEXBEN
00703  END-OUTPUT-PAGE.                                                 ELTEXBEN
00704      SET COF-END TO TRUE.                                         ELTEXBEN
00705      INITIALIZE COF-NBR-HDR-LINES                                 ELTEXBEN
00706                 COF-NBR-DTL-LINES.                                ELTEXBEN
00707      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTEXBEN
00708                       COMMAREA(DFHCOMMAREA)                       ELTEXBEN
00709                       END-EXEC.                                   ELTEXBEN
00710                                                                   ELTEXBEN
00711 ************************************************************      ELTEXBEN
00712 *                                                          *      ELTEXBEN
00713 *        CALL ELUOUTPT                                     *      ELTEXBEN
00714 *                                                          *      ELTEXBEN
00715 ************************************************************      ELTEXBEN
00716  CALL-ELUOUTPT.                                                   ELTEXBEN
00717      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTEXBEN
00718                       COMMAREA(DFHCOMMAREA)                       ELTEXBEN
00719                       END-EXEC.                                   ELTEXBEN
00720      INITIALIZE COF-FUNCTION                                      ELTEXBEN
00721                 COF-NBR-DTL-LINES.                                ELTEXBEN
00722                                                                   ELTEXBEN
00723 ************************************************************      ELTEXBEN
00724 *                                                          *      ELTEXBEN
00725 *     SIGNAL UNALLOC AREA                                  *      ELTEXBEN
00726 *                                                          *      ELTEXBEN
00727 ************************************************************      ELTEXBEN
00728  SIGNAL-UNALLOC-AREA.                                             ELTEXBEN
00729      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTEXBEN
00730      EXEC CICS ABEND                                              ELTEXBEN
00731                ABCODE(CIA-ABCODE)                                 ELTEXBEN
00732      END-EXEC.                                                    ELTEXBEN
00733                                                                   ELTEXBEN
00734 ************************************************************      ELTEXBEN
00735 *                                                          *      ELTEXBEN
00736 *     END OF THE PROGRAM                                   *      ELTEXBEN
00737 *                                                          *      ELTEXBEN
00738 ************************************************************      ELTEXBEN
00739      TITLE 'PROGRAM: ELTEXBEN  '.                                 ELTEXBEN
