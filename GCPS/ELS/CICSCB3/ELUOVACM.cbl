00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUOVACM
00003  PROGRAM-ID.           ELUOVACM.                                     LV001
00004                                                                   ELUOVACM
00005  AUTHOR.               RICK BARILEAU.                             ELUOVACM
00006                                                                   ELUOVACM
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELUOVACM
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELUOVACM
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELUOVACM
00010                        233 N. MICHIGAN AVE                        ELUOVACM
00011                        CHICAGO, ILLINOIS 60601                    ELUOVACM
00012                                                                   ELUOVACM
00013  DATE-WRITTEN.         31-AUG-1988.                               ELUOVACM
00014                                                                   ELUOVACM
00015  ENVIRONMENT DIVISION.                                            ELUOVACM
00016  CONFIGURATION SECTION.                                           ELUOVACM
00017  SOURCE-COMPUTER. IBM-3033.                                       ELUOVACM
00018  OBJECT-COMPUTER. IBM-3033.                                       ELUOVACM
00019                                                                   ELUOVACM
00020 ****************************************************************  ELUOVACM
00021 *                                                              *  ELUOVACM
00022 *  ELUOVACM :  THIS MODULE IS BEING CALLLED BY 'ELUCSCOV'.     *  ELUOVACM
00023 *              THE PURPOSE IS DETERMINE WHETHER A GIVEN        *  ELUOVACM
00024 *              BENEFIT PROVISION IS COVERED UNDER ANY OR ALL   *  ELUOVACM
00025 *              OF THE OVERALL ACCUM (S).                       *  ELUOVACM
00026 *                                                              *  ELUOVACM
00027 ****************************************************************  ELUOVACM
00028 *                      MAINTENANCE HISTORY                     *  ELUOVACM
00029 *                                                              *  ELUOVACM
00030 *  MOD     DATE      BY  DRPT              ACTION              *  ELUOVACM
00031 * ----- ----------- --- ----- ---------------------------------*  ELUOVACM
00032 * 01.00 31-AUG-1988 REB       CREATED                          *  ELUOVACM
00033 * 01.01 26-SEP-1988 NAC       REMOVE LOGIC THAT LOOKS AT THE   *  ELUOVACM
00034 *                             BENEFIT PROVISION LEVEL ACCUMS   *  ELUOVACM
00035 *                             AND INTERROGATE THE CONFIDENCE   *  ELUOVACM
00036 *                             FACTOR ENTRIES CONTAINED ON THE  *  ELUOVACM
00037 *                             RANKING REQUEST BLOCK.           *  ELUOVACM
00038 * 01.02 16-OCT-1989 EGL       DESTRUCTED AND REMOVED THE       *  ELUOVACM
00039 *                             OVERALL-IDX ITEMS FROM THE       *  ELUOVACM
00040 *                             CPBP- TABLE                      *  ELUOVACM
00041 ****************************************************************  ELUOVACM
00042  DATA DIVISION.                                                   ELUOVACM
00043  WORKING-STORAGE SECTION.                                         ELUOVACM
00044  01  WS-BEGIN                       PIC X(32)   VALUE             ELUOVACM
00045      '*THIS IS THE START OF ELUOVACM *'.                          ELUOVACM
00046                                                                   ELUOVACM
00047  01  WS-SWITCHES.                                                 ELUOVACM
00048      05  WS-BP-QUALIFY-SW           PIC  X(01)  VALUE SPACE.      ELUOVACM
00049          88  BP-DISQUALIFIED                    VALUE 'D'.        ELUOVACM
00050          88  BP-QUALIFIED                       VALUE 'Q'.        ELUOVACM
00051      05  WS-PROCESS-ACCUM-SW        PIC  X(01)  VALUE SPACE.      ELUOVACM
00052          88  PROCESS-ABM                        VALUE '1'.        ELUOVACM
00053          88  PROCESS-ACL                        VALUE '2'.        ELUOVACM
00054          88  PROCESS-ADL                        VALUE '3'.        ELUOVACM
00055      05  WS-PROVISION-SW            PIC  X(01)  VALUE SPACE.      ELUOVACM
00056          88  PROVISION-FOUND                    VALUE 'F'.        ELUOVACM
00057          88  PROVISION-NOT-FOUND                VALUE 'N'.        ELUOVACM
00058      05  WS-RR-QUALIFY-SW           PIC  X(01)  VALUE SPACE.      ELUOVACM
00059          88  RANK-REQ-DISQUALIFIED              VALUE 'Y'.        ELUOVACM
00060          88  RANK-REQ-QUALIFIED                 VALUE 'Z'.        ELUOVACM
00061                                                                   ELUOVACM
00062  01  WS-CF-THRESHOLD                COMP-1 VALUE +0.200000E+00.   ELUOVACM
00063                                                                   ELUOVACM
00064  LINKAGE SECTION.                                                 ELUOVACM
00065  01  DFHCOMMAREA.                                                 ELUOVACM
00066      COPY ELSCOMMC.                                               ELUOVACM
00067 /                                                                 ELUOVACM
00068      COPY ELSCIA2C.                                               ELUOVACM
00069 /    COPYBOOK USED FOR CONTRACT SUMMARY BENEFIT PROVISION TABLE   ELUOVACM
00070      COPY ELSCSBPC.                                               ELUOVACM
00071 /    COPYBOOK USED FOR CONTRACT SUMMARY POINTER TABLE             ELUOVACM
00072      COPY ELSCSPTC.                                               ELUOVACM
00073 /    COPYBOOK USED FOR OVERALL ACCUM ATBL ACCUMULATOR TABLE       ELUOVACM
00074      COPY ELSATBLC.                                               ELUOVACM
00075 /    COPYBOOK USED FOR OVERALL ACCUM IBGR TABLE                   ELUOVACM
00076      COPY ELSIBGRC.                                               ELUOVACM
00077 /    COPYBOOK USED FOR OVERALL ACCUM ACCUMULATOR TABLE            ELUOVACM
00078      COPY ELSCSACC.                                               ELUOVACM
00079 /    COPYBOOK USED FOR OVERALL ACCUM RANKING REQUEST TABLE        ELUOVACM
00080      COPY ELSRRBLC.                                               ELUOVACM
00081 /                                                                 ELUOVACM
00082  01  IBGR-RECORD.                                                 ELUOVACM
00083      COPY GCTIBGRC.                                               ELUOVACM
00084                                                                   ELUOVACM
00085      EJECT                                                        ELUOVACM
00086  PROCEDURE DIVISION.                                              ELUOVACM
00087 ************************************************************      ELUOVACM
00088 *                                                          *      ELUOVACM
00089 *        OVERALL ACCUM DETERMINATION                       *      ELUOVACM
00090 *                                                          *      ELUOVACM
00091 ************************************************************      ELUOVACM
00092  OVERALL-ACCUM-DETERMINATION.                                     ELUOVACM
00093      PERFORM INITIALIZE-MODULE.                                   ELUOVACM
00094      PERFORM MAIN-PROCESS.                                        ELUOVACM
00095      PERFORM TERMINATE-MODULE.                                    ELUOVACM
00096                                                                   ELUOVACM
00097                                                                   ELUOVACM
00098 ************************************************************      ELUOVACM
00099 *                                                          *      ELUOVACM
00100 *        INITIALIZE MODULE                                 *      ELUOVACM
00101 *                                                          *      ELUOVACM
00102 ************************************************************      ELUOVACM
00103  INITIALIZE-MODULE.                                               ELUOVACM
00104      PERFORM ESTABLISH-ADDR-OF-CONTROL-BLKS.                      ELUOVACM
00105      PERFORM ESTABLISH-ADDR-OF-POINTER-LIST.                      ELUOVACM
00106      PERFORM ESTABLISH-ADDR-OF-CS-ACCUM-TAB.                      ELUOVACM
00107                                                                   ELUOVACM
00108                                                                   ELUOVACM
00109 ************************************************************      ELUOVACM
00110 *                                                          *      ELUOVACM
00111 *        ESTABLISH ADDR OF CONTROL BLKS                    *      ELUOVACM
00112 *                                                          *      ELUOVACM
00113 ************************************************************      ELUOVACM
00114  ESTABLISH-ADDR-OF-CONTROL-BLKS.                                  ELUOVACM
00115      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELUOVACM
00116          EXEC CICS ABEND                                          ELUOVACM
00117                    ABCODE ('EL01')                                ELUOVACM
00118          END-EXEC.                                                ELUOVACM
00119      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUOVACM
00120           ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.               ELUOVACM
00121      IF ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA = NULL           ELUOVACM
00122          EXEC CICS ABEND                                          ELUOVACM
00123                    ABCODE ('EL02')                                ELUOVACM
00124          END-EXEC.                                                ELUOVACM
00125 /***********************************************************      ELUOVACM
00126 *                                                          *      ELUOVACM
00127 *        ESTABLISH ADDR OF POINTER LIST                    *      ELUOVACM
00128 *                                                          *      ELUOVACM
00129 ************************************************************      ELUOVACM
00130  ESTABLISH-ADDR-OF-POINTER-LIST.                                  ELUOVACM
00131      SET  CIA-ELSCSPTC-DDN   TO TRUE.                             ELUOVACM
00132      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUOVACM
00133                      ADDRESS OF CSPT-POINTER-LIST.                ELUOVACM
00134      IF CIA-RC-PTR-NULL                                           ELUOVACM
00135          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELUOVACM
00136                                                                   ELUOVACM
00137                                                                   ELUOVACM
00138 ************************************************************      ELUOVACM
00139 *                                                          *      ELUOVACM
00140 *        ESTABLISH ADDR OF CS ACCUM TABLE                  *      ELUOVACM
00141 *                                                          *      ELUOVACM
00142 ************************************************************      ELUOVACM
00143  ESTABLISH-ADDR-OF-CS-ACCUM-TAB.                                  ELUOVACM
00144      SET  CIA-ELSCSAC-DDN    TO TRUE.                             ELUOVACM
00145      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUOVACM
00146                      ADDRESS OF CSAC-ACCUMULATOR-TABLE.           ELUOVACM
00147      IF CIA-RC-PTR-NULL                                           ELUOVACM
00148          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELUOVACM
00149 /***********************************************************      ELUOVACM
00150 *                                                          *      ELUOVACM
00151 *        MAIN PROCESS                                      *      ELUOVACM
00152 *                                                          *      ELUOVACM
00153 ************************************************************      ELUOVACM
00154  MAIN-PROCESS.                                                    ELUOVACM
00155      PERFORM                                                      ELUOVACM
00156          VARYING CSPT-IDX FROM 1 BY 1                             ELUOVACM
00157                 UNTIL  CSPT-IDX > CSPT-TBL-CNT                    ELUOVACM
00158          IF CSPT-BP-TBL-PTR (CSPT-IDX) NOT = NULLS                ELUOVACM
00159              PERFORM PROCESS-ENTIRE-CS-BP-TABLE                   ELUOVACM
00160          END-IF                                                   ELUOVACM
00161      END-PERFORM.                                                 ELUOVACM
00162                                                                   ELUOVACM
00163                                                                   ELUOVACM
00164 ************************************************************      ELUOVACM
00165 *                                                          *      ELUOVACM
00166 *        PROCESS ENTIRE CS BP TABLE                        *      ELUOVACM
00167 *                                                          *      ELUOVACM
00168 ************************************************************      ELUOVACM
00169  PROCESS-ENTIRE-CS-BP-TABLE.                                      ELUOVACM
00170      SET  ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE TO              ELUOVACM
00171           CSPT-BP-TBL-PTR (CSPT-IDX).                             ELUOVACM
00172      PERFORM SEARCH-PROVISIONS-FOR-QUALIFIE                       ELUOVACM
00173          VARYING CSBP-X-IDX FROM 1 BY 1                           ELUOVACM
00174                 UNTIL   CSBP-X-IDX > CSBP-TBL-CNT.                ELUOVACM
00175 /***********************************************************      ELUOVACM
00176 *                                                          *      ELUOVACM
00177 *        SEARCH PROVISIONS FOR QUALIFIED PROVISIONS        *      ELUOVACM
00178 *                                                          *      ELUOVACM
00179 ************************************************************      ELUOVACM
00180  SEARCH-PROVISIONS-FOR-QUALIFIE.                                  ELUOVACM
00181 ***********************************************************       ELUOVACM
00182 ** THE WORD \
00183 ** THAT IT HAS PASSED ALL REQUIREMENTS :                 **       ELUOVACM
00184 **                                                       **       ELUOVACM
00185 ** 1.) PROVN-PRICING-METHD > ZERO                        **       ELUOVACM
00186 ** 2.) THE BENEFIT PROVISION IS NOT A FORMAT \
00187 ** 3.) BENEFIT PROVISION COVERED AND PAYMENT REQUESTED   **       ELUOVACM
00188 **             -------  OR  -------                      **       ELUOVACM
00189 **     BENEFIT PROVISION COVERED ON SUPPLEMENTAL AND     **       ELUOVACM
00190 **     INDICATED TO USE SUPPLEMENTAL INFORMATION         **       ELUOVACM
00191 ***********************************************************       ELUOVACM
00192      SET  BP-QUALIFIED      TO TRUE.                              ELUOVACM
00193      IF (CSBP-COVERED (CSBP-X-IDX) AND                            ELUOVACM
00194                 CSBP-PAYMENT-REQUESTED (CSBP-X-IDX))              ELUOVACM
00195                            OR                                     ELUOVACM
00196         (CSBP-COVERED-ON-SUPP (CSBP-X-IDX) AND                    ELUOVACM
00197                 CSBP-USE-SUPP-INFO (CSBP-X-IDX))                  ELUOVACM
00198            PERFORM CONTINUE-TO-INTERROGATE-BENEFI                 ELUOVACM
00199      ELSE                                                         ELUOVACM
00200          CONTINUE.                                                ELUOVACM
00201                                                                   ELUOVACM
00202                                                                   ELUOVACM
00203 ************************************************************      ELUOVACM
00204 *                                                          *      ELUOVACM
00205 *        CONTINUE TO INTERROGATE BENEFIT PROVISION         *      ELUOVACM
00206 *                                                          *      ELUOVACM
00207 ************************************************************      ELUOVACM
00208  CONTINUE-TO-INTERROGATE-BENEFI.                                  ELUOVACM
00209      IF (CSBP-PROVN-PRICING-METHD (CSBP-X-IDX) NOT > ZERO         ELUOVACM
00210          OR  CSBP-FORMAT-W (CSBP-X-IDX))                          ELUOVACM
00211          CONTINUE                                                 ELUOVACM
00212      ELSE                                                         ELUOVACM
00213          PERFORM PROCESS-ALL-APPLICABLE-ACCUMUL.                  ELUOVACM
00214 /***********************************************************      ELUOVACM
00215 *                                                          *      ELUOVACM
00216 *        PROCESS ALL APPLICABLE ACCUMULATORS POINTERS      *      ELUOVACM
00217 *                                                          *      ELUOVACM
00218 ************************************************************      ELUOVACM
00219  PROCESS-ALL-APPLICABLE-ACCUMUL.                                  ELUOVACM
00220      IF CSAC-ABM-GC-TBL-PTR NOT = NULL                            ELUOVACM
00221          PERFORM PROCESS-FOR-ABM-TYPE.                            ELUOVACM
00222      IF CSAC-ACL-GC-TBL-PTR NOT = NULL                            ELUOVACM
00223          PERFORM PROCESS-FOR-ACL-TYPE.                            ELUOVACM
00224      IF CSAC-ADL-GC-TBL-PTR NOT = NULL                            ELUOVACM
00225          PERFORM PROCESS-FOR-ADL-TYPE.                            ELUOVACM
00226 /***********************************************************      ELUOVACM
00227 *                                                          *      ELUOVACM
00228 *        PROCESS FOR ABM TYPE                              *      ELUOVACM
00229 *                                                          *      ELUOVACM
00230 ************************************************************      ELUOVACM
00231  PROCESS-FOR-ABM-TYPE.                                            ELUOVACM
00232      SET  PROCESS-ABM TO TRUE.                                    ELUOVACM
00233      SET  ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST                     ELUOVACM
00234           TO CSAC-ABM-RR-PTR.                                     ELUOVACM
00235      SET  ADDRESS OF ATBL-ACCUMULATOR-TABLE  TO                   ELUOVACM
00236          CSAC-ABM-GC-TBL-PTR.                                     ELUOVACM
00237      PERFORM SEARCH-THRU-ENTIRE-RANKING-REQ                       ELUOVACM
00238          VARYING RRBL-X-IDX FROM 1 BY 1                           ELUOVACM
00239             UNTIL   RRBL-X-IDX > RRBL-TBL-CNT                     ELUOVACM
00240              OR      CSBP-ADDITIONAL-ABM-TEXT (CSBP-X-IDX).       ELUOVACM
00241                                                                   ELUOVACM
00242                                                                   ELUOVACM
00243 ************************************************************      ELUOVACM
00244 *                                                          *      ELUOVACM
00245 *        PROCESS FOR ACL TYPE                              *      ELUOVACM
00246 *                                                          *      ELUOVACM
00247 ************************************************************      ELUOVACM
00248  PROCESS-FOR-ACL-TYPE.                                            ELUOVACM
00249      SET  PROCESS-ACL TO TRUE.                                    ELUOVACM
00250      SET  ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST                     ELUOVACM
00251            TO CSAC-ACL-RR-PTR.                                    ELUOVACM
00252      SET  ADDRESS OF ATBL-ACCUMULATOR-TABLE  TO                   ELUOVACM
00253          CSAC-ACL-GC-TBL-PTR.                                     ELUOVACM
00254      PERFORM SEARCH-THRU-ENTIRE-RANKING-REQ                       ELUOVACM
00255          VARYING RRBL-X-IDX FROM 1 BY 1                           ELUOVACM
00256            UNTIL   RRBL-X-IDX > RRBL-TBL-CNT                      ELUOVACM
00257             OR      CSBP-ADDITIONAL-ACL-TEXT (CSBP-X-IDX).        ELUOVACM
00258                                                                   ELUOVACM
00259                                                                   ELUOVACM
00260 ************************************************************      ELUOVACM
00261 *                                                          *      ELUOVACM
00262 *        PROCESS FOR ADL TYPE                              *      ELUOVACM
00263 *                                                          *      ELUOVACM
00264 ************************************************************      ELUOVACM
00265  PROCESS-FOR-ADL-TYPE.                                            ELUOVACM
00266      SET  PROCESS-ADL                        TO TRUE.             ELUOVACM
00267      SET  ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST                     ELUOVACM
00268           TO CSAC-ADL-RR-PTR.                                     ELUOVACM
00269      SET  ADDRESS OF ATBL-ACCUMULATOR-TABLE  TO                   ELUOVACM
00270          CSAC-ADL-GC-TBL-PTR.                                     ELUOVACM
00271      PERFORM SEARCH-THRU-ENTIRE-RANKING-REQ                       ELUOVACM
00272          VARYING RRBL-X-IDX FROM 1 BY 1                           ELUOVACM
00273            UNTIL   RRBL-X-IDX > RRBL-TBL-CNT                      ELUOVACM
00274              OR    CSBP-ADDITIONAL-ADL-TEXT  (CSBP-X-IDX).        ELUOVACM
00275 /***********************************************************      ELUOVACM
00276 *                                                          *      ELUOVACM
00277 *        SEARCH THRU ENTIRE RANKING REQUEST LIST           *      ELUOVACM
00278 *                                                          *      ELUOVACM
00279 ************************************************************      ELUOVACM
00280  SEARCH-THRU-ENTIRE-RANKING-REQ.                                  ELUOVACM
00281      IF (RRBL-REGARD-INSTITUTIONAL (RRBL-X-IDX) AND               ELUOVACM
00282          CSBP-INSTITUTIONAL (CSBP-X-IDX))                         ELUOVACM
00283                    OR                                             ELUOVACM
00284         (RRBL-REGARD-PROFESSIONAL (RRBL-X-IDX) AND                ELUOVACM
00285          CSBP-PROFESSIONAL (CSBP-X-IDX))                          ELUOVACM
00286            PERFORM INTERROGATE-SERVICE-CLASS                      ELUOVACM
00287      ELSE                                                         ELUOVACM
00288            CONTINUE.                                              ELUOVACM
00289                                                                   ELUOVACM
00290                                                                   ELUOVACM
00291 ************************************************************      ELUOVACM
00292 *                                                          *      ELUOVACM
00293 *        INTERROGATE SERVICE CLASS                         *      ELUOVACM
00294 *                                                          *      ELUOVACM
00295 ************************************************************      ELUOVACM
00296  INTERROGATE-SERVICE-CLASS.                                       ELUOVACM
00297      IF RRBL-REGARD-INPATIENT (RRBL-X-IDX)                        ELUOVACM
00298          PERFORM CHECK-FOR-INPATIENT                              ELUOVACM
00299      ELSE                                                         ELUOVACM
00300          PERFORM CHECK-FOR-OUTPATIENT.                            ELUOVACM
00301                                                                   ELUOVACM
00302                                                                   ELUOVACM
00303 ************************************************************      ELUOVACM
00304 *                                                          *      ELUOVACM
00305 *        CHECK FOR INPATIENT                               *      ELUOVACM
00306 *                                                          *      ELUOVACM
00307 ************************************************************      ELUOVACM
00308  CHECK-FOR-INPATIENT.                                             ELUOVACM
00309      IF CSBP-INPATIENT (CSBP-X-IDX) OR                            ELUOVACM
00310                 CSBP-BOTH (CSBP-X-IDX)                            ELUOVACM
00311          PERFORM FINISH-RANKING-REQUEST-LIST                      ELUOVACM
00312      ELSE                                                         ELUOVACM
00313          CONTINUE.                                                ELUOVACM
00314                                                                   ELUOVACM
00315                                                                   ELUOVACM
00316 ************************************************************      ELUOVACM
00317 *                                                          *      ELUOVACM
00318 *        CHECK FOR OUTPATIENT                              *      ELUOVACM
00319 *                                                          *      ELUOVACM
00320 ************************************************************      ELUOVACM
00321  CHECK-FOR-OUTPATIENT.                                            ELUOVACM
00322      IF CSBP-OUTPATIENT (CSBP-X-IDX) OR                           ELUOVACM
00323                 CSBP-BOTH (CSBP-X-IDX)                            ELUOVACM
00324          PERFORM FINISH-RANKING-REQUEST-LIST                      ELUOVACM
00325      ELSE                                                         ELUOVACM
00326          CONTINUE.                                                ELUOVACM
00327 /***********************************************************      ELUOVACM
00328 *                                                          *      ELUOVACM
00329 *        FINISH RANKING REQUEST LIST                       *      ELUOVACM
00330 *                                                          *      ELUOVACM
00331 ************************************************************      ELUOVACM
00332  FINISH-RANKING-REQUEST-LIST.                                     ELUOVACM
00333      PERFORM                                                      ELUOVACM
00334          VARYING RRBL-Y-IDX FROM 1 BY 1                           ELUOVACM
00335                  UNTIL RRBL-Y-IDX > ATBL-TBL-CNT                  ELUOVACM
00336          IF RRBL-CF-ENTRIES (RRBL-X-IDX, RRBL-Y-IDX)              ELUOVACM
00337                  > WS-CF-THRESHOLD  OR   =  WS-CF-THRESHOLD       ELUOVACM
00338              PERFORM INTERROGATE-ATBL-IBGR-SLOT-NUM               ELUOVACM
00339          END-IF                                                   ELUOVACM
00340      END-PERFORM.                                                 ELUOVACM
00341                                                                   ELUOVACM
00342                                                                   ELUOVACM
00343 ************************************************************      ELUOVACM
00344 *                                                          *      ELUOVACM
00345 *        INTERROGATE ATBL IBGR SLOT NUMBER                 *      ELUOVACM
00346 *                                                          *      ELUOVACM
00347 ************************************************************      ELUOVACM
00348  INTERROGATE-ATBL-IBGR-SLOT-NUM.                                  ELUOVACM
00349      SET  ATBL-X-IDX  TO  RRBL-Y-IDX.                             ELUOVACM
00350      IF ATBL-BENEFIT-PERIOD (ATBL-X-IDX) =                        ELUOVACM
00351                 RRBL-BENEFIT-PERIOD (RRBL-X-IDX)                  ELUOVACM
00352          PERFORM PROCESS-ACCUM-TABULAR.                           ELUOVACM
00353                                                                   ELUOVACM
00354                                                                   ELUOVACM
00355 ************************************************************      ELUOVACM
00356 *                                                          *      ELUOVACM
00357 *        PROCESS ACCUM TABULAR                             *      ELUOVACM
00358 *                                                          *      ELUOVACM
00359 ************************************************************      ELUOVACM
00360  PROCESS-ACCUM-TABULAR.                                           ELUOVACM
00361      IF ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX) > ZEROS                ELUOVACM
00362          PERFORM SEARCH-IBGR-RECORD-ON-ELSIBGRC                   ELUOVACM
00363      ELSE                                                         ELUOVACM
00364          PERFORM DETERMINE-IF-ADDITIONAL-TEXT-S.                  ELUOVACM
00365 /***********************************************************      ELUOVACM
00366 *                                                          *      ELUOVACM
00367 *        SEARCH IBGR RECORD ON ELSIBGRC TABLE              *      ELUOVACM
00368 *                                                          *      ELUOVACM
00369 ************************************************************      ELUOVACM
00370  SEARCH-IBGR-RECORD-ON-ELSIBGRC.                                  ELUOVACM
00371      SET  CIA-ELSIBGR-DDN           TO TRUE.                      ELUOVACM
00372      CALL 'ELUSETAD'  USING DFHCOMMAREA                           ELUOVACM
00373                  ADDRESS OF IBGR-INTERNAL-TABS-TABLE.             ELUOVACM
00374      IF CIA-RC-PTR-NULL                                           ELUOVACM
00375          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELUOVACM
00376      PERFORM                                                      ELUOVACM
00377          VARYING IBGR-X-IDX FROM 1 BY 1                           ELUOVACM
00378                 UNTIL   IBGR-X-IDX > IBGR-TBL-CNT                 ELUOVACM
00379          IF IBGR-SLOT-NUMBER (IBGR-X-IDX) =                       ELUOVACM
00380                    ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX)             ELUOVACM
00381              PERFORM DETERMINE-IF-PROVISION-IS-COVE               ELUOVACM
00382          END-IF                                                   ELUOVACM
00383      END-PERFORM.                                                 ELUOVACM
00384                                                                   ELUOVACM
00385                                                                   ELUOVACM
00386 ************************************************************      ELUOVACM
00387 *                                                          *      ELUOVACM
00388 *        DETERMINE IF PROVISION IS COVERED OVERALL         *      ELUOVACM
00389 *                                                          *      ELUOVACM
00390 ************************************************************      ELUOVACM
00391  DETERMINE-IF-PROVISION-IS-COVE.                                  ELUOVACM
00392      IF IBGR-TABULAR-PTR (IBGR-X-IDX) = NULL                      ELUOVACM
00393          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELUOVACM
00394      SET  ADDRESS OF IBGR-RECORD TO IBGR-TABULAR-PTR              ELUOVACM
00395          (IBGR-X-IDX).                                            ELUOVACM
00396      SET  PROVISION-NOT-FOUND    TO TRUE.                         ELUOVACM
00397      PERFORM SEARCH-IBGR-RECORD-FOR-BENEFIT                       ELUOVACM
00398          VARYING GX1-INDEX FROM 1 BY 1                            ELUOVACM
00399                 UNTIL   GX1-INDEX > GX1-ENTRY-COUNT               ELUOVACM
00400                 OR      GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) >   ELUOVACM
00401                         CSBP-BP-KEY (CSBP-X-IDX).                 ELUOVACM
00402      IF (GX1-ID-ARGUMENT-EXCLUDED AND PROVISION-NOT-FOUND)        ELUOVACM
00403              OR                                                   ELUOVACM
00404         (GX1-ID-ARGUMENT-INCLUDED AND PROVISION-FOUND)            ELUOVACM
00405          PERFORM DETERMINE-IF-ADDITIONAL-TEXT-S.                  ELUOVACM
00406                                                                   ELUOVACM
00407                                                                   ELUOVACM
00408 ************************************************************      ELUOVACM
00409 *                                                          *      ELUOVACM
00410 *        SEARCH IBGR RECORD FOR BENEFIT PROVISION          *      ELUOVACM
00411 *                                                          *      ELUOVACM
00412 ************************************************************      ELUOVACM
00413  SEARCH-IBGR-RECORD-FOR-BENEFIT.                                  ELUOVACM
00414      IF GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) =                   ELUOVACM
00415                CSBP-BP-KEY (CSBP-X-IDX)                           ELUOVACM
00416          PERFORM INDICATE-PROVISION-FOUND-ON-IB.                  ELUOVACM
00417 /***********************************************************      ELUOVACM
00418 *                                                          *      ELUOVACM
00419 *        INDICATE PROVISION FOUND ON IBGR RECORD           *      ELUOVACM
00420 *                                                          *      ELUOVACM
00421 ************************************************************      ELUOVACM
00422  INDICATE-PROVISION-FOUND-ON-IB.                                  ELUOVACM
00423      SET  PROVISION-FOUND        TO TRUE.                         ELUOVACM
00424                                                                   ELUOVACM
00425                                                                   ELUOVACM
00426 ************************************************************      ELUOVACM
00427 *                                                          *      ELUOVACM
00428 *        DETERMINE IF ADDITIONAL TEXT SWITCH IS NECESSARY  *      ELUOVACM
00429 *                                                          *      ELUOVACM
00430 ************************************************************      ELUOVACM
00431  DETERMINE-IF-ADDITIONAL-TEXT-S.                                  ELUOVACM
00432      EVALUATE TRUE                                                ELUOVACM
00433      WHEN PROCESS-ABM                                             ELUOVACM
00434          IF CSBP-BAMA-OVERALL-COVERAGE (CSBP-X-IDX)               ELUOVACM
00435              SET CSBP-ADDITIONAL-ABM-TEXT (CSBP-X-IDX) TO         ELUOVACM
00436                  TRUE                                             ELUOVACM
00437          ELSE                                                     ELUOVACM
00438              SET CSBP-BAMA-OVERALL-COVERAGE (CSBP-X-IDX) TO       ELUOVACM
00439                  TRUE                                             ELUOVACM
00440          END-IF                                                   ELUOVACM
00441      WHEN PROCESS-ACL                                             ELUOVACM
00442          IF CSBP-COINS-OVERALL-COVERAGE (CSBP-X-IDX)              ELUOVACM
00443              SET CSBP-ADDITIONAL-ACL-TEXT (CSBP-X-IDX) TO         ELUOVACM
00444                  TRUE                                             ELUOVACM
00445          ELSE                                                     ELUOVACM
00446              SET CSBP-COINS-OVERALL-COVERAGE (CSBP-X-IDX) TO      ELUOVACM
00447                  TRUE                                             ELUOVACM
00448          END-IF                                                   ELUOVACM
00449      WHEN PROCESS-ADL                                             ELUOVACM
00450          IF CSBP-DEDL-OVERALL-COVERAGE (CSBP-X-IDX)               ELUOVACM
00451              SET CSBP-ADDITIONAL-ADL-TEXT (CSBP-X-IDX) TO         ELUOVACM
00452                  TRUE                                             ELUOVACM
00453          ELSE                                                     ELUOVACM
00454              SET CSBP-DEDL-OVERALL-COVERAGE (CSBP-X-IDX) TO       ELUOVACM
00455                  TRUE                                             ELUOVACM
00456          END-IF                                                   ELUOVACM
00457      WHEN OTHER                                                   ELUOVACM
00458          SET CIA-AB-PGM-LOGIC TO TRUE                             ELUOVACM
00459          EXEC CICS ABEND ABCODE(CIA-ABCODE)                       ELUOVACM
00460          END-EXEC                                                 ELUOVACM
00461      END-EVALUATE.                                                ELUOVACM
00462 ************************************************************      ELUOVACM
00463 *                                                          *      ELUOVACM
00464 *        SIGNAL UNALLOCATED AREA ERROR                     *      ELUOVACM
00465 *                                                          *      ELUOVACM
00466 ************************************************************      ELUOVACM
00467  SIGNAL-UNALLOCATED-AREA-ERROR.                                   ELUOVACM
00468      SET  CIA-AB-UNALLOC-AREA  TO TRUE.                           ELUOVACM
00469      EXEC CICS ABEND                                              ELUOVACM
00470                ABCODE (CIA-ABCODE)                                ELUOVACM
00471      END-EXEC.                                                    ELUOVACM
00472                                                                   ELUOVACM
00473                                                                   ELUOVACM
00474 ************************************************************      ELUOVACM
00475 *                                                          *      ELUOVACM
00476 *        TERMINATE MODULE                                  *      ELUOVACM
00477 *                                                          *      ELUOVACM
00478 ************************************************************      ELUOVACM
00479  TERMINATE-MODULE.                                                ELUOVACM
00480      GOBACK.                                                      ELUOVACM
