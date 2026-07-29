00001 *      LAST MAINTENANCE TIME: 14.33.41  DATE: 03/31/89            06/29/02
00002 * STRUCTURE(S) MEMBER ELUCSPLGPL - LEVEL 036 AS OF 03/19/88       ELUCSPLG
00003 * FROM PANLIB R360059.STRUCTPL.PANLIB                                LV001
00004 *                                                                 ELUCSPLG
00005  IDENTIFICATION DIVISION.                                         ELUCSPLG
00006                                                                   ELUCSPLG
00007  PROGRAM-ID.         ELUCSPLG.                                    ELUCSPLG
00008                                                                   ELUCSPLG
00009  AUTHOR.             ANNE KEFFER KING.                            ELUCSPLG
00010                                                                   ELUCSPLG
00011  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUCSPLG
00012                      A MUTUAL LEGAL RESERVE COMPANY               ELUCSPLG
00013                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUCSPLG
00014                      233 N. MICHIGAN AVE                          ELUCSPLG
00015                      CHICAGO, ILLINOIS 60601                      ELUCSPLG
00016                                                                   ELUCSPLG
00017  DATE-WRITTEN.       15-JAN-1988.                                 ELUCSPLG
00018                                                                   ELUCSPLG
00019  DATE-COMPILED.                                                   ELUCSPLG
00020                                                                   ELUCSPLG
00021  SECURITY.           COPYRIGHT 1986,                              ELUCSPLG
00022                      HEALTH CARE SERVICE CORPORATION              ELUCSPLG
00023      SKIP3                                                        ELUCSPLG
00024  ENVIRONMENT DIVISION.                                            ELUCSPLG
00025                                                                   ELUCSPLG
00026  CONFIGURATION SECTION.                                           ELUCSPLG
00027  SOURCE-COMPUTER.    IBM-3090.                                    ELUCSPLG
00028  OBJECT-COMPUTER.    IBM-3090.                                    ELUCSPLG
00029                                                                   ELUCSPLG
00030 ******************************************************************ELUCSPLG
00031 * ELUCSPLG -- ELS:                                               *ELUCSPLG
00032 *                                                                *ELUCSPLG
00033 *ELUCSPLG IDENTIFIES WHICH BENEFIT PROVISIONS IN EACH BENEFIT    *ELUCSPLG
00034 *PROVISION GROUPING HAVE IDENTICAL OUTPUT FOR THE INDIVIDUAL     *ELUCSPLG
00035 *SUBTOPIC.                                                       *ELUCSPLG
00036 ******************************************************************ELUCSPLG
00037 *                                                                *ELUCSPLG
00038 *                      MAINTENANCE HISTORY                       *ELUCSPLG
00039 *                                                                *ELUCSPLG
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELUCSPLG
00041 * ----- ----------- --- ----- ---------------------------------- *ELUCSPLG
00042 * 01.00 15-JAN-1988 AKK       CREATED                            *ELUCSPLG
00043 *                                                                *ELUCSPLG
00044 * 01.01 31-MAR-1989 AKK       DESTRUCT PROGRAM PREVIOUSLY IN     *ELUCSPLG
00045 *                             STRUCTURES.                        *ELUCSPLG
00046 * 01.02 13-APR-1989 GEM       STORAGE MANAGEMENT ENHANCEMENTS    *ELUCSPLG
00047 *                                                                *ELUCSPLG
00048 * 01.03 31-JUL-1989 AKK       CORRECTION DUE TO ERROR IN STORAGE *ELUCSPLG
00049 *                             MANAGEMENT.                        *ELUCSPLG
00050 ******************************************************************ELUCSPLG
00051                                                                   ELUCSPLG
00052  DATA DIVISION.                                                   ELUCSPLG
00053  WORKING-STORAGE SECTION.                                         ELUCSPLG
00054  01  NEW-BP-IDX                 PIC S9(04) COMP.                  ELUCSPLG
00055  01  OLD-BP-IDX                 PIC S9(04) COMP.                  ELUCSPLG
00056 *                                                                 ELUCSPLG
00057  LINKAGE SECTION.                                                 ELUCSPLG
00058  01  DFHCOMMAREA.                                                 ELUCSPLG
00059  COPY ELSCOMMC.                                                   ELUCSPLG
00060 *                                                                 ELUCSPLG
00061  COPY ELSCIA2C.                                                   ELUCSPLG
00062 /                                                                 ELUCSPLG
00063  COPY ELSCSBPC.                                                   ELUCSPLG
00064 /                                                                 ELUCSPLG
00065  COPY ELSCSPTC.                                                   ELUCSPLG
00066 /                                                                 ELUCSPLG
00067  COPY ELSCSPGC.                                                   ELUCSPLG
00068 *                                                                 ELUCSPLG
00069  PROCEDURE DIVISION.                                              ELUCSPLG
00070 ************************************************************      ELUCSPLG
00071 *                                                          *      ELUCSPLG
00072 *                    PROCEDURE DIVISION                    *      ELUCSPLG
00073 *                                                          *      ELUCSPLG
00074 ************************************************************      ELUCSPLG
00075                                                                   ELUCSPLG
00076                                                                   ELUCSPLG
00077 ************************************************************      ELUCSPLG
00078 *                                                          *      ELUCSPLG
00079 *        PAYMENT LEVEL GROUPING                            *      ELUCSPLG
00080 *                                                          *      ELUCSPLG
00081 ************************************************************      ELUCSPLG
00082  PAYMENT-LEVEL-GROUPING.                                          ELUCSPLG
00083      PERFORM INITIALIZATION.                                      ELUCSPLG
00084      PERFORM PROCESS-PAYMENT-LEVEL-GROUPING.                      ELUCSPLG
00085      GOBACK.                                                      ELUCSPLG
00086                                                                   ELUCSPLG
00087                                                                   ELUCSPLG
00088 ************************************************************      ELUCSPLG
00089 *                                                          *      ELUCSPLG
00090 *        INITIALIZATION                                    *      ELUCSPLG
00091 *                                                          *      ELUCSPLG
00092 ************************************************************      ELUCSPLG
00093  INITIALIZATION.                                                  ELUCSPLG
00094      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELUCSPLG
00095      PERFORM ESTABLISH-ADDRESSABILITY-OF-PO.                      ELUCSPLG
00096                                                                   ELUCSPLG
00097                                                                   ELUCSPLG
00098 ************************************************************      ELUCSPLG
00099 *                                                          *      ELUCSPLG
00100 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELUCSPLG
00101 *                                                          *      ELUCSPLG
00102 ************************************************************      ELUCSPLG
00103  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELUCSPLG
00104      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELUCSPLG
00105      PERFORM ESTABLISH-ADDRESSABILITY-OF-CI.                      ELUCSPLG
00106                                                                   ELUCSPLG
00107                                                                   ELUCSPLG
00108 ************************************************************      ELUCSPLG
00109 *                                                          *      ELUCSPLG
00110 *        CHECK FOR VALID COMMAREA                          *      ELUCSPLG
00111 *                                                          *      ELUCSPLG
00112 ************************************************************      ELUCSPLG
00113  CHECK-FOR-VALID-COMMAREA.                                        ELUCSPLG
00114      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELUCSPLG
00115          EXEC CICS ABEND                                          ELUCSPLG
00116                    ABCODE('EL01')                                 ELUCSPLG
00117           END-EXEC.                                               ELUCSPLG
00118                                                                   ELUCSPLG
00119                                                                   ELUCSPLG
00120 ************************************************************      ELUCSPLG
00121 *                                                          *      ELUCSPLG
00122 *        ESTABLISH ADDRESSABILITY OF CIA                   *      ELUCSPLG
00123 *                                                          *      ELUCSPLG
00124 ************************************************************      ELUCSPLG
00125  ESTABLISH-ADDRESSABILITY-OF-CI.                                  ELUCSPLG
00126      IF ECA-CIA-PTR = NULL                                        ELUCSPLG
00127         EXEC CICS ABEND                                           ELUCSPLG
00128                  ABCODE('EL02')                                   ELUCSPLG
00129           END-EXEC                                                ELUCSPLG
00130                                                                   ELUCSPLG
00131      ELSE                                                         ELUCSPLG
00132          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELUCSPLG
00133                                                                   ELUCSPLG
00134                                                                   ELUCSPLG
00135 ************************************************************      ELUCSPLG
00136 *                                                          *      ELUCSPLG
00137 *        ESTABLISH ADDRESSABILITY OF POINTER LIST          *      ELUCSPLG
00138 *                                                          *      ELUCSPLG
00139 ************************************************************      ELUCSPLG
00140  ESTABLISH-ADDRESSABILITY-OF-PO.                                  ELUCSPLG
00141      SET CIA-ELSCSPTC-DDN TO TRUE.                                ELUCSPLG
00142      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSPLG
00143          ADDRESS OF CSPT-POINTER-LIST.                            ELUCSPLG
00144      IF CIA-RC-PTR-NULL                                           ELUCSPLG
00145         PERFORM SIGNAL-UNALLOC-AREA-ERROR.                        ELUCSPLG
00146                                                                   ELUCSPLG
00147                                                                   ELUCSPLG
00148 ************************************************************      ELUCSPLG
00149 *                                                          *      ELUCSPLG
00150 *        ESTABLISH ADDRESS OF CSPT                         *      ELUCSPLG
00151 *                                                          *      ELUCSPLG
00152 ************************************************************      ELUCSPLG
00153  ESTABLISH-ADDRESS-OF-CSPT.                                       ELUCSPLG
00154      SET CIA-ELSCSPTC-DDN TO TRUE.                                ELUCSPLG
00155      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSPLG
00156          ADDRESS OF CSPT-POINTER-LIST.                            ELUCSPLG
00157                                                                   ELUCSPLG
00158                                                                   ELUCSPLG
00159 ************************************************************      ELUCSPLG
00160 *                                                          *      ELUCSPLG
00161 *        SIGNAL UNALLOC AREA ERROR                         *      ELUCSPLG
00162 *                                                          *      ELUCSPLG
00163 ************************************************************      ELUCSPLG
00164  SIGNAL-UNALLOC-AREA-ERROR.                                       ELUCSPLG
00165      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUCSPLG
00166      PERFORM SIGNAL-ABEND.                                        ELUCSPLG
00167                                                                   ELUCSPLG
00168                                                                   ELUCSPLG
00169 ************************************************************      ELUCSPLG
00170 *                                                          *      ELUCSPLG
00171 *        SIGNAL ABEND                                      *      ELUCSPLG
00172 *                                                          *      ELUCSPLG
00173 ************************************************************      ELUCSPLG
00174  SIGNAL-ABEND.                                                    ELUCSPLG
00175      EXEC CICS ABEND                                              ELUCSPLG
00176                ABCODE(CIA-ABCODE)                                 ELUCSPLG
00177         END-EXEC.                                                 ELUCSPLG
00178                                                                   ELUCSPLG
00179                                                                   ELUCSPLG
00180 ************************************************************      ELUCSPLG
00181 *                                                          *      ELUCSPLG
00182 *        ESTABLISH ADDRESS OF CIA                          *      ELUCSPLG
00183 *                                                          *      ELUCSPLG
00184 ************************************************************      ELUCSPLG
00185  ESTABLISH-ADDRESS-OF-CIA.                                        ELUCSPLG
00186      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUCSPLG
00187          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELUCSPLG
00188                                                                   ELUCSPLG
00189                                                                   ELUCSPLG
00190 ************************************************************      ELUCSPLG
00191 *                                                          *      ELUCSPLG
00192 *        PROCESS PAYMENT LEVEL GROUPING                    *      ELUCSPLG
00193 *                                                          *      ELUCSPLG
00194 ************************************************************      ELUCSPLG
00195  PROCESS-PAYMENT-LEVEL-GROUPING.                                  ELUCSPLG
00196      PERFORM GROUP-PAYMENT-LEVELS-FOR-EACHX                       ELUCSPLG
00197          VARYING CSPT-IDX FROM 1 BY 1                             ELUCSPLG
00198                      UNTIL CSPT-IDX > CSPT-TBL-CNT.               ELUCSPLG
00199                                                                   ELUCSPLG
00200                                                                   ELUCSPLG
00201 ************************************************************      ELUCSPLG
00202 *                                                          *      ELUCSPLG
00203 *        GROUP PAYMENT LEVELS FOR EACH SUBTOPIC            *      ELUCSPLG
00204 *                                                          *      ELUCSPLG
00205 ************************************************************      ELUCSPLG
00206  GROUP-PAYMENT-LEVELS-FOR-EACHX.                                  ELUCSPLG
00207      IF CSPT-BP-TBL-PTR (CSPT-IDX) NOT = NULLS                    ELUCSPLG
00208         PERFORM ESTABLISH-ADDRESSABILITY-OF-PR                    ELUCSPLG
00209         PERFORM CREATE-PROVISION-GROUP-TABLE                      ELUCSPLG
00210         PERFORM CREATE-FIRST-PAYMENT-LEVEL                        ELUCSPLG
00211         PERFORM BUILD-PAYMENT-LEVELS                              ELUCSPLG
00212              VARYING NEW-BP-IDX FROM 2 BY 1                       ELUCSPLG
00213                           UNTIL NEW-BP-IDX > CSBP-TBL-CNT.        ELUCSPLG
00214                                                                   ELUCSPLG
00215                                                                   ELUCSPLG
00216 ************************************************************      ELUCSPLG
00217 *                                                          *      ELUCSPLG
00218 *        ESTABLISH ADDRESSABILITY OF PROVISION TABLE       *      ELUCSPLG
00219 *                                                          *      ELUCSPLG
00220 ************************************************************      ELUCSPLG
00221  ESTABLISH-ADDRESSABILITY-OF-PR.                                  ELUCSPLG
00222 *    SET CSPT-BP-TBL-PTR (CSPT-IDX) TO                            ELUCSPLG
00223 *        ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE.                 ELUCSPLG
00224      SET ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE                  ELUCSPLG
00225         TO CSPT-BP-TBL-PTR (CSPT-IDX).                            ELUCSPLG
00226      SET CIA-ELSCSBPC-DDN TO TRUE.                                ELUCSPLG
00227      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELUCSPLG
00228          ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE.                 ELUCSPLG
00229                                                                   ELUCSPLG
00230                                                                   ELUCSPLG
00231 ************************************************************      ELUCSPLG
00232 *                                                          *      ELUCSPLG
00233 *        CREATE PROVISION GROUP TABLE                      *      ELUCSPLG
00234 *                                                          *      ELUCSPLG
00235 ************************************************************      ELUCSPLG
00236  CREATE-PROVISION-GROUP-TABLE.                                    ELUCSPLG
00237      SET CIA-ELSCSPG-DDN TO TRUE.                                 ELUCSPLG
00238      PERFORM ALLOCATE-PROVISION-GROUP-TABLE.                      ELUCSPLG
00239      MOVE CSBP-SUBTOPIC TO CSPG-SUBTOPIC.                         ELUCSPLG
00240      MOVE CSBP-GROUP-CNT TO CSPG-TBL-CNT.                         ELUCSPLG
00241      PERFORM INITIALIZE-SPLIT-INDICATORS                          ELUCSPLG
00242          VARYING CSPG-IDX FROM 1 BY 1 UNTIL                       ELUCSPLG
00243                       CSPG-IDX > CSPG-TBL-CNT.                    ELUCSPLG
00244                                                                   ELUCSPLG
00245                                                                   ELUCSPLG
00246 ************************************************************      ELUCSPLG
00247 *                                                          *      ELUCSPLG
00248 *        ALLOCATE PROVISION GROUP TABLE                    *      ELUCSPLG
00249 *                                                          *      ELUCSPLG
00250 ************************************************************      ELUCSPLG
00251  ALLOCATE-PROVISION-GROUP-TABLE.                                  ELUCSPLG
00252      COMPUTE CIA-AREA-LEN = LENGTH OF CSPG-FIXED-PORTION          ELUCSPLG
00253          +                                                        ELUCSPLG
00254         (CSBP-GROUP-CNT * LENGTH OF                               ELUCSPLG
00255          CSPG-PROVISION-GROUP-ENTRY).                             ELUCSPLG
00256      PERFORM ACQUIRE-CONTROLLED-STORAGE.                          ELUCSPLG
00257      PERFORM ESTABLISH-ADDRESS-OF-PROVISION.                      ELUCSPLG
00258                                                                   ELUCSPLG
00259                                                                   ELUCSPLG
00260 ************************************************************      ELUCSPLG
00261 *                                                          *      ELUCSPLG
00262 *        ESTABLISH ADDRESS OF PROVISION GROUP TABLE        *      ELUCSPLG
00263 *                                                          *      ELUCSPLG
00264 ************************************************************      ELUCSPLG
00265  ESTABLISH-ADDRESS-OF-PROVISION.                                  ELUCSPLG
00266      SET CIA-ELSCSPG-DDN TO TRUE.                                 ELUCSPLG
00267      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSPLG
00268          ADDRESS OF CSPG-PROVISION-GROUP-TABLE.                   ELUCSPLG
00269                                                                   ELUCSPLG
00270      SET CSPT-PROV-GRP-PTR (CSPT-IDX) TO                          ELUCSPLG
00271          ADDRESS OF CSPG-PROVISION-GROUP-TABLE.                   ELUCSPLG
00272                                                                   ELUCSPLG
00273                                                                   ELUCSPLG
00274 ************************************************************      ELUCSPLG
00275 *                                                          *      ELUCSPLG
00276 *        ACQUIRE CONTROLLED STORAGE                        *      ELUCSPLG
00277 *                                                          *      ELUCSPLG
00278 ************************************************************      ELUCSPLG
00279  ACQUIRE-CONTROLLED-STORAGE.                                      ELUCSPLG
00280      SET CIA-STG-GETMAIN TO TRUE.                                 ELUCSPLG
00281      PERFORM LINK-TO-STORAGE-MANAGER.                             ELUCSPLG
00282                                                                   ELUCSPLG
00283                                                                   ELUCSPLG
00284 ************************************************************      ELUCSPLG
00285 *                                                          *      ELUCSPLG
00286 *        INITIALIZE SPLIT INDICATORS                       *      ELUCSPLG
00287 *                                                          *      ELUCSPLG
00288 ************************************************************      ELUCSPLG
00289  INITIALIZE-SPLIT-INDICATORS.                                     ELUCSPLG
00290      SET CSPG-GROUP-NOT-COVERED (CSPG-IDX),                       ELUCSPLG
00291          CSPG-NO-INSTITUTIONAL-SPLIT (CSPG-IDX),                  ELUCSPLG
00292          CSPG-NO-PROFESSIONAL-SPLIT (CSPG-IDX) TO TRUE.           ELUCSPLG
00293                                                                   ELUCSPLG
00294                                                                   ELUCSPLG
00295 ************************************************************      ELUCSPLG
00296 *                                                          *      ELUCSPLG
00297 *        CREATE FIRST PAYMENT LEVEL                        *      ELUCSPLG
00298 *                                                          *      ELUCSPLG
00299 ************************************************************      ELUCSPLG
00300  CREATE-FIRST-PAYMENT-LEVEL.                                      ELUCSPLG
00301      MOVE 1 TO NEW-BP-IDX                                         ELUCSPLG
00302                OLD-BP-IDX.                                        ELUCSPLG
00303      MOVE NEW-BP-IDX TO CSBP-PAYMENT-LVL (NEW-BP-IDX).            ELUCSPLG
00304      SET CSPG-IDX TO CSBP-PROVISION-GRP (NEW-BP-IDX).             ELUCSPLG
00305      PERFORM DETERMINE-IF-PROVISION-GROUP-C.                      ELUCSPLG
00306      PERFORM INDICATE-PROVIDER-CLASS-MAKEUP.                      ELUCSPLG
00307                                                                   ELUCSPLG
00308                                                                   ELUCSPLG
00309 ************************************************************      ELUCSPLG
00310 *                                                          *      ELUCSPLG
00311 *        BUILD PAYMENT LEVELS                              *      ELUCSPLG
00312 *                                                          *      ELUCSPLG
00313 ************************************************************      ELUCSPLG
00314  BUILD-PAYMENT-LEVELS.                                            ELUCSPLG
00315      PERFORM IDENTIFY-EXISTING-PAYMENT-LEVE                       ELUCSPLG
00316          VARYING OLD-BP-IDX FROM 1 BY 1                           ELUCSPLG
00317                     UNTIL OLD-BP-IDX > NEW-BP-IDX - 1             ELUCSPLG
00318                     OR    CSBP-PAYMENT-LVL (NEW-BP-IDX) >         ELUCSPLG
00319              0.                                                   ELUCSPLG
00320      IF CSBP-PAYMENT-LVL (NEW-BP-IDX) = ZERO                      ELUCSPLG
00321          MOVE NEW-BP-IDX TO CSBP-PAYMENT-LVL (NEW-BP-IDX)         ELUCSPLG
00322          SET CSPG-IDX TO CSBP-PROVISION-GRP (NEW-BP-IDX)          ELUCSPLG
00323          PERFORM DETERMINE-IF-PROVISION-GROUP-C                   ELUCSPLG
00324          PERFORM INDICATE-PROVIDER-CLASS-MAKEUP.                  ELUCSPLG
00325                                                                   ELUCSPLG
00326                                                                   ELUCSPLG
00327 ************************************************************      ELUCSPLG
00328 *                                                          *      ELUCSPLG
00329 *        IDENTIFY EXISTING PAYMENT LEVEL                   *      ELUCSPLG
00330 *                                                          *      ELUCSPLG
00331 ************************************************************      ELUCSPLG
00332  IDENTIFY-EXISTING-PAYMENT-LEVE.                                  ELUCSPLG
00333      IF CSBP-PAYMENT-LVL (OLD-BP-IDX) = OLD-BP-IDX                ELUCSPLG
00334          PERFORM COMPARE-NEW-PROVISION-TO-PREVI.                  ELUCSPLG
00335                                                                   ELUCSPLG
00336                                                                   ELUCSPLG
00337 ************************************************************      ELUCSPLG
00338 *                                                          *      ELUCSPLG
00339 *        DETERMINE IF PROVISION GROUP COVERED              *      ELUCSPLG
00340 *                                                          *      ELUCSPLG
00341 ************************************************************      ELUCSPLG
00342  DETERMINE-IF-PROVISION-GROUP-C.                                  ELUCSPLG
00343      IF CSBP-COVERED  (NEW-BP-IDX) OR                             ELUCSPLG
00344                   CSBP-COVERED-ON-SUPP (NEW-BP-IDX)               ELUCSPLG
00345          SET CSPG-GROUP-COVERED (CSPG-IDX) TO TRUE.               ELUCSPLG
00346                                                                   ELUCSPLG
00347                                                                   ELUCSPLG
00348 ************************************************************      ELUCSPLG
00349 *                                                          *      ELUCSPLG
00350 *        INDICATE PROVIDER CLASS MAKEUP OF PROV GRP        *      ELUCSPLG
00351 *                                                          *      ELUCSPLG
00352 ************************************************************      ELUCSPLG
00353  INDICATE-PROVIDER-CLASS-MAKEUP.                                  ELUCSPLG
00354      IF CSBP-INSTITUTIONAL (NEW-BP-IDX)                           ELUCSPLG
00355          PERFORM ADD-INST-PROV-CLASS-TO-MAKEUP                    ELUCSPLG
00356      ELSE                                                         ELUCSPLG
00357          PERFORM ADD-PROF-PROV-CLASS-TO-MAKEUP.                   ELUCSPLG
00358                                                                   ELUCSPLG
00359                                                                   ELUCSPLG
00360 ************************************************************      ELUCSPLG
00361 *                                                          *      ELUCSPLG
00362 *        ADD INST PROV CLASS TO MAKEUP                     *      ELUCSPLG
00363 *                                                          *      ELUCSPLG
00364 ************************************************************      ELUCSPLG
00365  ADD-INST-PROV-CLASS-TO-MAKEUP.                                   ELUCSPLG
00366      IF CSPG-PROFESSIONAL (CSPG-IDX) OR CSPG-BOTH                 ELUCSPLG
00367          (CSPG-IDX)                                               ELUCSPLG
00368          SET CSPG-BOTH (CSPG-IDX) TO TRUE                         ELUCSPLG
00369      ELSE                                                         ELUCSPLG
00370          SET CSPG-INSTITUTIONAL (CSPG-IDX) TO TRUE.               ELUCSPLG
00371                                                                   ELUCSPLG
00372                                                                   ELUCSPLG
00373 ************************************************************      ELUCSPLG
00374 *                                                          *      ELUCSPLG
00375 *        ADD PROF PROV CLASS TO MAKEUP                     *      ELUCSPLG
00376 *                                                          *      ELUCSPLG
00377 ************************************************************      ELUCSPLG
00378  ADD-PROF-PROV-CLASS-TO-MAKEUP.                                   ELUCSPLG
00379      IF CSPG-INSTITUTIONAL (CSPG-IDX) OR CSPG-BOTH                ELUCSPLG
00380          (CSPG-IDX)                                               ELUCSPLG
00381          SET CSPG-BOTH (CSPG-IDX) TO TRUE                         ELUCSPLG
00382      ELSE                                                         ELUCSPLG
00383          SET CSPG-PROFESSIONAL (CSPG-IDX) TO TRUE.                ELUCSPLG
00384                                                                   ELUCSPLG
00385                                                                   ELUCSPLG
00386 ************************************************************      ELUCSPLG
00387 *                                                          *      ELUCSPLG
00388 *        COMPARE NEW PROVISION TO PREVIOUS PROVISIONS      *      ELUCSPLG
00389 *                                                          *      ELUCSPLG
00390 ************************************************************      ELUCSPLG
00391  COMPARE-NEW-PROVISION-TO-PREVI.                                  ELUCSPLG
00392      IF CSBP-PAYMENT-LEVEL-DATA (OLD-BP-IDX) =                    ELUCSPLG
00393                   CSBP-PAYMENT-LEVEL-DATA (NEW-BP-IDX)            ELUCSPLG
00394          MOVE OLD-BP-IDX TO CSBP-PAYMENT-LVL (NEW-BP-IDX)         ELUCSPLG
00395      ELSE                                                         ELUCSPLG
00396          PERFORM DETERMINE-IF-PROVISION-GROUP-S.                  ELUCSPLG
00397                                                                   ELUCSPLG
00398                                                                   ELUCSPLG
00399 ************************************************************      ELUCSPLG
00400 *                                                          *      ELUCSPLG
00401 *        DETERMINE IF PROVISION GROUP SPLIT OCCURS         *      ELUCSPLG
00402 *                                                          *      ELUCSPLG
00403 ************************************************************      ELUCSPLG
00404  DETERMINE-IF-PROVISION-GROUP-S.                                  ELUCSPLG
00405      IF CSBP-PAYMENT-LEVEL-KEY (NEW-BP-IDX) =                     ELUCSPLG
00406                   CSBP-PAYMENT-LEVEL-KEY (OLD-BP-IDX)             ELUCSPLG
00407          PERFORM SPLIT-PAYMENT-LEVEL-IN-CURRENT.                  ELUCSPLG
00408                                                                   ELUCSPLG
00409                                                                   ELUCSPLG
00410 ************************************************************      ELUCSPLG
00411 *                                                          *      ELUCSPLG
00412 *        SPLIT PAYMENT LEVEL IN CURRENT PROVISION GROUP    *      ELUCSPLG
00413 *                                                          *      ELUCSPLG
00414 ************************************************************      ELUCSPLG
00415  SPLIT-PAYMENT-LEVEL-IN-CURRENT.                                  ELUCSPLG
00416      SET CSPG-IDX TO CSBP-PROVISION-GRP (NEW-BP-IDX).             ELUCSPLG
00417      IF CSBP-INSTITUTIONAL (NEW-BP-IDX)                           ELUCSPLG
00418          SET CSPG-INSTITUTIONAL-SPLIT (CSPG-IDX) TO TRUE          ELUCSPLG
00419      ELSE                                                         ELUCSPLG
00420         SET CSPG-PROFESSIONAL-SPLIT (CSPG-IDX) TO TRUE.           ELUCSPLG
00421                                                                   ELUCSPLG
00422                                                                   ELUCSPLG
00423 ************************************************************      ELUCSPLG
00424 *                                                          *      ELUCSPLG
00425 *        LINK TO STORAGE MANAGER                           *      ELUCSPLG
00426 *                                                          *      ELUCSPLG
00427 ************************************************************      ELUCSPLG
00428  LINK-TO-STORAGE-MANAGER.                                         ELUCSPLG
00429      EXEC CICS LINK                                               ELUCSPLG
00430                PROGRAM ('ELUSTGMG')                               ELUCSPLG
00431                COMMAREA (DFHCOMMAREA)                             ELUCSPLG
00432      END-EXEC.                                                    ELUCSPLG
00433                                                                   ELUCSPLG
