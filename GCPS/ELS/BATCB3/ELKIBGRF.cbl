00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIBGRF
00003  PROGRAM-ID.           ELKIBGRF.                                     LV004
00004                                                                   ELKIBGRF
00005  AUTHOR.               BARBARA KEIB.                              ELKIBGRF
00006                                                                   ELKIBGRF
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIBGRF
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIBGRF
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIBGRF
00010                        233 N. MICHIGAN AVE                        ELKIBGRF
00011                        CHICAGO, ILLINOIS 60601                    ELKIBGRF
00012                                                                   ELKIBGRF
00013  DATE-WRITTEN.         28-SEP-1992.                               ELKIBGRF
00014                                                                   ELKIBGRF
00015  ENVIRONMENT DIVISION.                                            ELKIBGRF
00016                                                                   ELKIBGRF
00017  CONFIGURATION SECTION.                                           ELKIBGRF
00018                                                                   ELKIBGRF
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKIBGRF
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKIBGRF
00021                                                                   ELKIBGRF
00022 ******************************************************************ELKIBGRF
00023 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIBGRF
00024 *  ELKIBGRF - COMPUTE CONFIDENCE FACTORS FOR A LIST OF IBGR      *ELKIBGRF
00025 *             TABULARS.                                          *ELKIBGRF
00026 *                                                                *ELKIBGRF
00027 *              THIS PROGRAM CONTROLS THE COMPUTATION OF CONFI-   *ELKIBGRF
00028 *              DENCE FACTORS BASED ON #IBGR TABULARS.  FACTORS   *ELKIBGRF
00029 *              FOR A LIST OF ONE OR MORE #IBGR TABS ARE COMPUTED *ELKIBGRF
00030 *              UNDER CONTROL OF THIS PROGRAM.  FACTORS ARE       *ELKIBGRF
00031 *              COMPUTEDC FOR OVERALL ACCUMULATOR DETERMINATION,  *ELKIBGRF
00032 *              UNWEIGHTED LIST MATCHING OR WEIGHTED LIST MATCH-  *ELKIBGRF
00033 *              ING, DEPENDING ON THE SETTING OF THE INPUT PARMS. *ELKIBGRF
00034 *                                                                *ELKIBGRF
00035 ******************************************************************ELKIBGRF
00036 *                      MAINTENANCE HISTORY                       *ELKIBGRF
00037 *                                                                *ELKIBGRF
00038 *  MOD     DATE      BY                    ACTION                *ELKIBGRF
00039 * ----- ----------- --- -----------------------------------------*ELKIBGRF
00040 * ----- ----------- --- -----------------------------------------*ELKIBGRF
00041 * 01.00 28-SEP-1992 BAK CREATED                                  *ELKIBGRF
00042 *                                                                *ELKIBGRF
00043 * 01.06 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX         *ELKIBGRF
00044 *                             ASM RECOMPILES                     *ELKIBGRF
00045 *                                                                *ELKIBGRF
00046 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIBGRF
00047 *                                                                *ELKIBGRF
00048 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELKIBGRF
00049 *                                                                *ELKIBGRF
00050 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIBGRF
00051 *                             CICSCB3 COMPILER FIX             *  ELKIBGRF
00052 ******************************************************************ELKIBGRF
00053 /                                                                 ELKIBGRF
00054  DATA DIVISION.                                                   ELKIBGRF
00055                                                                   ELKIBGRF
00056  WORKING-STORAGE SECTION.                                         ELKIBGRF
00057                                                                   ELKIBGRF
00058  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIBGRF
00059    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIBGRF
00060    88  WS-EXTRA-PARM                   VALUE +4.                  ELKIBGRF
00061    88  WS-MISSING-PARM                 VALUE +12.                 ELKIBGRF
00062    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIBGRF
00063                                                                   ELKIBGRF
00064                                                                   ELKIBGRF
00065      COPY ELSCFDBC.                                               ELKIBGRF
00066 /                                                                 ELKIBGRF
00067  LINKAGE SECTION.                                                 ELKIBGRF
00068                                                                   ELKIBGRF
00069 /                                                                 ELKIBGRF
00070                                                                   ELKIBGRF
00071      COPY ELSIBGRC.                                               ELKIBGRF
00072                                                                   ELKIBGRF
00073  01  LS-MATCH-LIST             PIC X.                             ELKIBGRF
00074                                                                   ELKIBGRF
00075      COPY ELSBPVLC.                                               ELKIBGRF
00076                                                                   ELKIBGRF
00077      COPY ELSBPVWC.                                               ELKIBGRF
00078                                                                   ELKIBGRF
00079  01  IBGR-TABULAR-RECORD.                                         ELKIBGRF
00080      COPY GCTIBGRC.                                               ELKIBGRF
00081                                                                   ELKIBGRF
00082                                                                   ELKIBGRF
00083 /*****************************************************************ELKIBGRF
00084 *                                                                *ELKIBGRF
00085 *    PROCEDURE DIVISION                                          *ELKIBGRF
00086 *                                                                *ELKIBGRF
00087 ******************************************************************ELKIBGRF
00088                                                                   ELKIBGRF
00089  PROCEDURE DIVISION USING IBGR-INTERNAL-TABS-TABLE                ELKIBGRF
00090                               LS-MATCH-LIST.                      ELKIBGRF
00091                                                                   ELKIBGRF
00092  0000-DETERMINE-IBGR-CONFIDENCE.                                  ELKIBGRF
00093                                                                   ELKIBGRF
00094      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELKIBGRF
00095         SET WS-MISSING-PARM TO TRUE                               ELKIBGRF
00096      ELSE                                                         ELKIBGRF
00097         PERFORM 0100-INITIALIZATION                               ELKIBGRF
00098         PERFORM 1000-PROCESS-IBGR-TABULAR.                        ELKIBGRF
00099      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIBGRF
00100      GOBACK.                                                      ELKIBGRF
00101                                                                   ELKIBGRF
00102 ******************************************************************ELKIBGRF
00103 *                                                                *ELKIBGRF
00104 *    INITIALIZATION                                              *ELKIBGRF
00105 *                                                                *ELKIBGRF
00106 ******************************************************************ELKIBGRF
00107                                                                   ELKIBGRF
00108  0100-INITIALIZATION.                                             ELKIBGRF
00109                                                                   ELKIBGRF
00110      INITIALIZE CFDB-CF-IBGR.                                     ELKIBGRF
00111                                                                   ELKIBGRF
00112 ******************************************************************ELKIBGRF
00113 *                                                                *ELKIBGRF
00114 *    PROCESS-IBGR-TABULAR                                        *ELKIBGRF
00115 *                                                                *ELKIBGRF
00116 ******************************************************************ELKIBGRF
00117                                                                   ELKIBGRF
00118  1000-PROCESS-IBGR-TABULAR.                                       ELKIBGRF
00119                                                                   ELKIBGRF
00120      EVALUATE TRUE                                                ELKIBGRF
00121      WHEN IBGR-CF-CALC-OV                                         ELKIBGRF
00122         PERFORM 1100-COMPUTE-OVERALL-CONF,                        ELKIBGRF
00123      WHEN IBGR-CF-CALC-MTCH                                       ELKIBGRF
00124         PERFORM 2000-COMPUTE-UNWEIGHTED-MATCH,                    ELKIBGRF
00125      WHEN IBGR-CF-CALC-WT-MTCH                                    ELKIBGRF
00126         PERFORM 3000-COMPUTE-WEIGHTED-MATCH,                      ELKIBGRF
00127      WHEN OTHER SET WS-MISSING-PARM TO TRUE                       ELKIBGRF
00128      END-EVALUATE.                                                ELKIBGRF
00129                                                                   ELKIBGRF
00130 ******************************************************************ELKIBGRF
00131 *                                                                *ELKIBGRF
00132 *    COMPUTE OVERALL CONFIDENCE FACTORS                          *ELKIBGRF
00133 *                                                                *ELKIBGRF
00134 ******************************************************************ELKIBGRF
00135                                                                   ELKIBGRF
00136  1100-COMPUTE-OVERALL-CONF.                                       ELKIBGRF
00137                                                                   ELKIBGRF
00138      SET IBGR-MAX-IDX TO IBGR-TBL-CNT.                            ELKIBGRF
00139      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO                        ELKIBGRF
00140          ADDRESS OF LS-MATCH-LIST.                                ELKIBGRF
00141      IF ADDRESS OF BPVL-BNFT-PRVSN-TBL NOT EQUAL NULL             ELKIBGRF
00142         SET WS-EXTRA-PARM TO TRUE.                                ELKIBGRF
00143      PERFORM 1200-COMPUTE-OVERALL-CONF-SLOT                       ELKIBGRF
00144           VARYING IBGR-IDX FROM 1 BY 1                            ELKIBGRF
00145             UNTIL IBGR-IDX > IBGR-MAX-IDX OR                      ELKIBGRF
00146               WS-INTERNAL-ERROR.                                  ELKIBGRF
00147                                                                   ELKIBGRF
00148 ******************************************************************ELKIBGRF
00149 *                                                                *ELKIBGRF
00150 *    COMPUTE OVERALL CONFIDENCE FACTORS FOR EACH SLOT NUMBER     *ELKIBGRF
00151 *                                                                *ELKIBGRF
00152 ******************************************************************ELKIBGRF
00153                                                                   ELKIBGRF
00154  1200-COMPUTE-OVERALL-CONF-SLOT.                                  ELKIBGRF
00155                                                                   ELKIBGRF
00156      SET ADDRESS OF IBGR-TABULAR-RECORD TO                        ELKIBGRF
00157                         IBGR-TABULAR-PTR (IBGR-IDX).              ELKIBGRF
00158      CALL 'ELKIBGRC' USING IBGR-TABULAR-RECORD                    ELKIBGRF
00159                           CFDB-CNFDNC-FCTR-DATA-BLCK.             ELKIBGRF
00160      IF RETURN-CODE = ZERO                                        ELKIBGRF
00161         SET IBGR-CF-CALC-OK TO TRUE                               ELKIBGRF
00162         MOVE CFDB-CF-IBGR-INSTTNL TO IBGR-CF-INST (IBGR-IDX)      ELKIBGRF
00163         MOVE CFDB-CF-IBGR-PRFSNL  TO IBGR-CF-PROF (IBGR-IDX)      ELKIBGRF
00164         MOVE CFDB-CF-IBGR-IP-ONLY TO IBGR-CF-IP (IBGR-IDX)        ELKIBGRF
00165         MOVE CFDB-CF-IBGR-IP-BOTH TO IBGR-CF-IP-BOTH (IBGR-IDX)   ELKIBGRF
00166         MOVE CFDB-CF-IBGR-OP-ONLY TO IBGR-CF-OP (IBGR-IDX)        ELKIBGRF
00167         MOVE CFDB-CF-IBGR-OP-BOTH TO IBGR-CF-OP-BOTH (IBGR-IDX)   ELKIBGRF
00168         MOVE CFDB-CF-IBGR-OV      TO IBGR-CF-OV (IBGR-IDX)        ELKIBGRF
00169      ELSE                                                         ELKIBGRF
00170         SET WS-INTERNAL-ERROR TO TRUE                             ELKIBGRF
00171         SET IBGR-CF-CALC-FAIL TO TRUE.                            ELKIBGRF
00172                                                                   ELKIBGRF
00173 ******************************************************************ELKIBGRF
00174 *                                                                *ELKIBGRF
00175 *    COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE FACTORS            *ELKIBGRF
00176 *                                                                *ELKIBGRF
00177 ******************************************************************ELKIBGRF
00178                                                                   ELKIBGRF
00179  2000-COMPUTE-UNWEIGHTED-MATCH.                                   ELKIBGRF
00180                                                                   ELKIBGRF
00181      SET IBGR-MAX-IDX TO IBGR-TBL-CNT.                            ELKIBGRF
00182      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO                        ELKIBGRF
00183          ADDRESS OF LS-MATCH-LIST.                                ELKIBGRF
00184      IF ADDRESS OF BPVL-BNFT-PRVSN-TBL = NULL                     ELKIBGRF
00185         SET WS-MISSING-PARM TO TRUE                               ELKIBGRF
00186      ELSE                                                         ELKIBGRF
00187      PERFORM 2100-COMPUTE-UNWEIGHTED-SLOT                         ELKIBGRF
00188           VARYING IBGR-IDX FROM 1 BY 1                            ELKIBGRF
00189             UNTIL IBGR-IDX > IBGR-MAX-IDX OR                      ELKIBGRF
00190               WS-INTERNAL-ERROR.                                  ELKIBGRF
00191                                                                   ELKIBGRF
00192 ******************************************************************ELKIBGRF
00193 *                                                                *ELKIBGRF
00194 *    COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE FACTORS FOR SLOTS  *ELKIBGRF
00195 *                                                                *ELKIBGRF
00196 ******************************************************************ELKIBGRF
00197                                                                   ELKIBGRF
00198  2100-COMPUTE-UNWEIGHTED-SLOT.                                    ELKIBGRF
00199                                                                   ELKIBGRF
00200      SET ADDRESS OF IBGR-TABULAR-RECORD TO                        ELKIBGRF
00201                         IBGR-TABULAR-PTR (IBGR-IDX).              ELKIBGRF
00202      CALL 'ELKIBGRM' USING IBGR-TABULAR-RECORD                    ELKIBGRF
00203                            BPVL-BNFT-PRVSN-TBL                    ELKIBGRF
00204                           CFDB-CNFDNC-FCTR-DATA-BLCK.             ELKIBGRF
00205      IF RETURN-CODE = ZERO                                        ELKIBGRF
00206         SET IBGR-CF-CALC-OK TO TRUE                               ELKIBGRF
00207         MOVE CFDB-CF-IBGR-UNWGHTD-LST-MTCH                        ELKIBGRF
00208                       TO IBGR-CF-LIST-MTCH (IBGR-IDX)             ELKIBGRF
00209      ELSE                                                         ELKIBGRF
00210         SET WS-INTERNAL-ERROR TO TRUE                             ELKIBGRF
00211         SET IBGR-CF-CALC-FAIL TO TRUE.                            ELKIBGRF
00212                                                                   ELKIBGRF
00213 ******************************************************************ELKIBGRF
00214 *                                                                *ELKIBGRF
00215 *    COMPUTE WEIGHTED LIST MATCH CONFIDENCE FACTORS              *ELKIBGRF
00216 *                                                                *ELKIBGRF
00217 ******************************************************************ELKIBGRF
00218                                                                   ELKIBGRF
00219  3000-COMPUTE-WEIGHTED-MATCH.                                     ELKIBGRF
00220                                                                   ELKIBGRF
00221      SET IBGR-MAX-IDX TO IBGR-TBL-CNT.                            ELKIBGRF
00222      SET ADDRESS OF BPVW-BNFT-PRVSN-TBL TO                        ELKIBGRF
00223          ADDRESS OF LS-MATCH-LIST.                                ELKIBGRF
00224      IF ADDRESS OF BPVW-BNFT-PRVSN-TBL = NULL                     ELKIBGRF
00225         SET WS-MISSING-PARM TO TRUE                               ELKIBGRF
00226      ELSE                                                         ELKIBGRF
00227      PERFORM 3100-COMPUTE-WEIGHTED-SLOT                           ELKIBGRF
00228           VARYING IBGR-IDX FROM 1 BY 1                            ELKIBGRF
00229             UNTIL IBGR-IDX > IBGR-MAX-IDX OR                      ELKIBGRF
00230               WS-INTERNAL-ERROR.                                  ELKIBGRF
00231                                                                   ELKIBGRF
00232 ******************************************************************ELKIBGRF
00233 *                                                                *ELKIBGRF
00234 *    COMPUTE WEIGHTED LIST MATCH CONFIDENCE FACTORS FOR SLOTS    *ELKIBGRF
00235 *                                                                *ELKIBGRF
00236 ******************************************************************ELKIBGRF
00237                                                                   ELKIBGRF
00238  3100-COMPUTE-WEIGHTED-SLOT.                                      ELKIBGRF
00239                                                                   ELKIBGRF
00240      SET ADDRESS OF IBGR-TABULAR-RECORD TO                        ELKIBGRF
00241                         IBGR-TABULAR-PTR (IBGR-IDX).              ELKIBGRF
00242      CALL 'ELKIBGRW' USING IBGR-TABULAR-RECORD                    ELKIBGRF
00243                            BPVW-BNFT-PRVSN-TBL                    ELKIBGRF
00244                           CFDB-CNFDNC-FCTR-DATA-BLCK.             ELKIBGRF
00245      IF RETURN-CODE = ZERO                                        ELKIBGRF
00246         SET IBGR-CF-CALC-OK TO TRUE                               ELKIBGRF
00247         MOVE CFDB-CF-IBGR-WGHTD-LST-MTCH                          ELKIBGRF
00248                       TO IBGR-CF-LIST-MTCH (IBGR-IDX)             ELKIBGRF
00249      ELSE                                                         ELKIBGRF
00250         SET WS-INTERNAL-ERROR TO TRUE                             ELKIBGRF
00251         SET IBGR-CF-CALC-FAIL TO TRUE.                            ELKIBGRF
