00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGNF
00003  PROGRAM-ID.           ELKIPGNF.                                     LV004
00004                                                                   ELKIPGNF
00005  AUTHOR.               BARBARA KEIB.                              ELKIPGNF
00006                                                                   ELKIPGNF
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGNF
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGNF
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGNF
00010                        233 N. MICHIGAN AVE                        ELKIPGNF
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGNF
00012                                                                   ELKIPGNF
00013                                                                   ELKIPGNF
00014  DATE-WRITTEN.         25-SEP-1992.                               ELKIPGNF
00015                                                                   ELKIPGNF
00016  ENVIRONMENT DIVISION.                                            ELKIPGNF
00017                                                                   ELKIPGNF
00018  CONFIGURATION SECTION.                                           ELKIPGNF
00019                                                                   ELKIPGNF
00020  SOURCE-COMPUTER.      IBM-3090.                                  ELKIPGNF
00021  OBJECT-COMPUTER.      IBM-3090.                                  ELKIPGNF
00022                                                                   ELKIPGNF
00023 ******************************************************************ELKIPGNF
00024 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIPGNF
00025 *                                                                *ELKIPGNF
00026 *  ELKIPGNF - COMPUTE CONFIDENCE FACTORS FOR A LIST OF IPGN      *ELKIPGNF
00027 *             TABULARS.                                          *ELKIPGNF
00028 *                                                                *ELKIPGNF
00029 *              THIS PROGRAM CONTROLS THE COMPUTATION OF CONFI-   *ELKIPGNF
00030 *              DENCE FACTORS BASED ON #IPGN TABULARS.  FACTORS   *ELKIPGNF
00031 *              FOR A LIST OF ONE OR MORE #IPGN TABS ARE COMPUTED *ELKIPGNF
00032 *              UNDER CONTROL OF THIS PROGRAM.  FACTORS ARE       *ELKIPGNF
00033 *              COMPUTEDC FOR OVERALL ACCUMULATOR DETERMINATION,  *ELKIPGNF
00034 *              UNWEIGHTED LIST MATCHING OR WEIGHTED LIST MATCH-  *ELKIPGNF
00035 *              ING, DEPENDING ON THE SETTING OF THE INPUT PARMS. *ELKIPGNF
00036 ******************************************************************ELKIPGNF
00037 *                      MAINTENANCE HISTORY                       *ELKIPGNF
00038 *                                                                *ELKIPGNF
00039 *                                                                *ELKIPGNF
00040 *  MOD     DATE      BY                    ACTION                *ELKIPGNF
00041 * ----- ----------- --- -----------------------------------------*ELKIPGNF
00042 * 01.00 25-SEP-1992 BAK CREATED                                  *ELKIPGNF
00043 *                                                                *ELKIPGNF
00044 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGNF
00045 *                                                                *ELKIPGNF
00046 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGNF
00047 *                                                                *ELKIPGNF
00048 * 02.01 09-JAN-2004 AKK INTERTEST S0C7                           *ELKIPGNF
00049 *                                                                *ELKIPGNF
00050 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGNF
00051 *                             CICSCB3 COMPILER FIX             *  ELKIPGNF
00052 ******************************************************************ELKIPGNF
00053 /                                                                 ELKIPGNF
00054  DATA DIVISION.                                                   ELKIPGNF
00055                                                                   ELKIPGNF
00056  WORKING-STORAGE SECTION.                                         ELKIPGNF
00057                                                                   ELKIPGNF
00058  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIPGNF
00059    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIPGNF
00060    88  WS-EXTRA-PARM                   VALUE +4.                  ELKIPGNF
00061    88  WS-MISSING-PARM                 VALUE +12.                 ELKIPGNF
00062    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIPGNF
00063                                                                   ELKIPGNF
00064                                                                   ELKIPGNF
00065      COPY ELSCFDBC.                                               ELKIPGNF
00066                                                                   ELKIPGNF
00067                                                                   ELKIPGNF
00068 /                                                                 ELKIPGNF
00069  LINKAGE SECTION.                                                 ELKIPGNF
00070                                                                   ELKIPGNF
00071 /                                                                 ELKIPGNF
00072                                                                   ELKIPGNF
00073      COPY ELSIPGNC.                                               ELKIPGNF
00074                                                                   ELKIPGNF
00075  01 LS-MATCH-LIST             PIC X.                              ELKIPGNF
00076                                                                   ELKIPGNF
00077      COPY ELSPVNLC.                                               ELKIPGNF
00078                                                                   ELKIPGNF
00079                                                                   ELKIPGNF
00080      COPY ELSPVNWC.                                               ELKIPGNF
00081                                                                   ELKIPGNF
00082                                                                   ELKIPGNF
00083  01 IPGN-TABULAR-RECORD.                                          ELKIPGNF
00084      COPY GCTIPGNC.                                               ELKIPGNF
00085                                                                   ELKIPGNF
00086 /*****************************************************************ELKIPGNF
00087 *                                                                *ELKIPGNF
00088 *    PROCEDURE DIVISION                                          *ELKIPGNF
00089 *                                                                *ELKIPGNF
00090 ******************************************************************ELKIPGNF
00091                                                                   ELKIPGNF
00092  PROCEDURE DIVISION USING IPGN-INTERNAL-TABS-TABLE                ELKIPGNF
00093                             LS-MATCH-LIST.                        ELKIPGNF
00094                                                                   ELKIPGNF
00095                                                                   ELKIPGNF
00096  0000-DETERMINE-IPGN-CONFIDENCE.                                  ELKIPGNF
00097                                                                   ELKIPGNF
00098      IF ADDRESS OF IPGN-INTERNAL-TABS-TABLE = NULL                ELKIPGNF
00099         SET WS-MISSING-PARM TO TRUE                               ELKIPGNF
00100      ELSE                                                         ELKIPGNF
00101         PERFORM 0100-INITIALIZATION                               ELKIPGNF
00102         PERFORM 1000-PROCESS-IPGN-TABULAR.                        ELKIPGNF
00103      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGNF
00104      GOBACK.                                                      ELKIPGNF
00105                                                                   ELKIPGNF
00106 ******************************************************************ELKIPGNF
00107 *                                                                *ELKIPGNF
00108 *    INITIALIZATION                                              *ELKIPGNF
00109 *                                                                *ELKIPGNF
00110 ******************************************************************ELKIPGNF
00111                                                                   ELKIPGNF
00112  0100-INITIALIZATION.                                             ELKIPGNF
00113                                                                   ELKIPGNF
00114      INITIALIZE CFDB-CF-IPGN.                                     ELKIPGNF
00115                                                                   ELKIPGNF
00116 ******************************************************************ELKIPGNF
00117 *                                                                *ELKIPGNF
00118 *    PROCESS-IPGN-TABULAR                                        *ELKIPGNF
00119 *                                                                *ELKIPGNF
00120 ******************************************************************ELKIPGNF
00121                                                                   ELKIPGNF
00122  1000-PROCESS-IPGN-TABULAR.                                       ELKIPGNF
00123                                                                   ELKIPGNF
00124      EVALUATE TRUE                                                ELKIPGNF
00125      WHEN IPGN-CF-CALC-OV                                         ELKIPGNF
00126         PERFORM 1100-COMPUTE-OVERALL-CONF,                        ELKIPGNF
00127      WHEN IPGN-CF-CALC-MTCH                                       ELKIPGNF
00128         PERFORM 2000-COMPUTE-UNWEIGHTED-MATCH,                    ELKIPGNF
00129      WHEN IPGN-CF-CALC-WT-MTCH                                    ELKIPGNF
00130         PERFORM 3000-COMPUTE-WEIGHTED-MATCH,                      ELKIPGNF
00131      WHEN OTHER SET WS-MISSING-PARM TO TRUE                       ELKIPGNF
00132      END-EVALUATE.                                                ELKIPGNF
00133                                                                   ELKIPGNF
00134 ******************************************************************ELKIPGNF
00135 *                                                                *ELKIPGNF
00136 *    COMPUTE OVERALL CONFIDENCE FACTORS                          *ELKIPGNF
00137 *                                                                *ELKIPGNF
00138 ******************************************************************ELKIPGNF
00139                                                                   ELKIPGNF
00140  1100-COMPUTE-OVERALL-CONF.                                       ELKIPGNF
00141                                                                   ELKIPGNF
00142      SET IPGN-MAX-IDX TO IPGN-TBL-CNT.                            ELKIPGNF
00143      SET ADDRESS OF PVNL-PRVDR-NBR-TBL TO                         ELKIPGNF
00144          ADDRESS OF LS-MATCH-LIST.                                ELKIPGNF
00145      IF ADDRESS OF PVNL-PRVDR-NBR-TBL NOT EQUAL NULL              ELKIPGNF
00146         SET WS-EXTRA-PARM TO TRUE.                                ELKIPGNF
00147      PERFORM 1200-COMPUTE-OVERALL-SLOT                            ELKIPGNF
00148           VARYING IPGN-IDX FROM 1 BY 1                            ELKIPGNF
00149             UNTIL IPGN-IDX > IPGN-MAX-IDX OR                      ELKIPGNF
00150               WS-INTERNAL-ERROR.                                  ELKIPGNF
00151                                                                   ELKIPGNF
00152 ******************************************************************ELKIPGNF
00153 *                                                                *ELKIPGNF
00154 *    COMPUTE OVERALL CONFIDENCE FACTORS FOR EACH SLOT NUMBER     *ELKIPGNF
00155 *                                                                *ELKIPGNF
00156 ******************************************************************ELKIPGNF
00157                                                                   ELKIPGNF
00158  1200-COMPUTE-OVERALL-SLOT.                                       ELKIPGNF
00159                                                                   ELKIPGNF
00160      SET ADDRESS OF IPGN-TABULAR-RECORD TO                        ELKIPGNF
00161                    IPGN-TABULAR-PTR (IPGN-IDX).                   ELKIPGNF
00162      CALL 'ELKIPGNC' USING IPGN-TABULAR-RECORD                    ELKIPGNF
00163                              CFDB-CNFDNC-FCTR-DATA-BLCK.          ELKIPGNF
00164      IF RETURN-CODE = ZERO                                        ELKIPGNF
00165         MOVE CFDB-CF-IPGN-OV TO IPGN-CF-OV (IPGN-IDX)             ELKIPGNF
00166         SET IPGN-CF-CALC-OK TO TRUE                               ELKIPGNF
00167      ELSE                                                         ELKIPGNF
00168         SET WS-INTERNAL-ERROR TO TRUE                             ELKIPGNF
00169         SET IPGN-CF-CALC-FAIL TO TRUE.                            ELKIPGNF
00170                                                                   ELKIPGNF
00171 ******************************************************************ELKIPGNF
00172 *                                                                *ELKIPGNF
00173 *    COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE FACTORS            *ELKIPGNF
00174 *                                                                *ELKIPGNF
00175 ******************************************************************ELKIPGNF
00176                                                                   ELKIPGNF
00177  2000-COMPUTE-UNWEIGHTED-MATCH.                                   ELKIPGNF
00178                                                                   ELKIPGNF
00179      SET IPGN-MAX-IDX TO IPGN-TBL-CNT.                            ELKIPGNF
00180      SET ADDRESS OF PVNL-PRVDR-NBR-TBL TO                         ELKIPGNF
00181          ADDRESS OF LS-MATCH-LIST.                                ELKIPGNF
00182      IF ADDRESS OF PVNL-PRVDR-NBR-TBL = NULL                      ELKIPGNF
00183         SET WS-MISSING-PARM TO TRUE                               ELKIPGNF
00184      ELSE                                                         ELKIPGNF
00185      PERFORM 2100-COMPUTE-UNWEIGHTED-SLOT                         ELKIPGNF
00186           VARYING IPGN-IDX FROM 1 BY 1                            ELKIPGNF
00187             UNTIL IPGN-IDX > IPGN-MAX-IDX OR                      ELKIPGNF
00188               WS-INTERNAL-ERROR.                                  ELKIPGNF
00189                                                                   ELKIPGNF
00190 ******************************************************************ELKIPGNF
00191 *                                                                *ELKIPGNF
00192 *    COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE FACTORS FOR SLOTS  *ELKIPGNF
00193 *                                                                *ELKIPGNF
00194 ******************************************************************ELKIPGNF
00195                                                                   ELKIPGNF
00196  2100-COMPUTE-UNWEIGHTED-SLOT.                                    ELKIPGNF
00197                                                                   ELKIPGNF
00198      SET ADDRESS OF IPGN-TABULAR-RECORD TO                        ELKIPGNF
00199                    IPGN-TABULAR-PTR (IPGN-IDX).                   ELKIPGNF
00200      CALL 'ELKIPGNM' USING IPGN-TABULAR-RECORD                    ELKIPGNF
00201                              PVNL-PRVDR-NBR-TBL                   ELKIPGNF
00202                               CFDB-CNFDNC-FCTR-DATA-BLCK.         ELKIPGNF
00203      IF RETURN-CODE = ZERO                                        ELKIPGNF
00204         MOVE CFDB-CF-IPGN-UNWGHTD-LST-MTCH TO                     ELKIPGNF
00205                      IPGN-CF-LIST-MTCH (IPGN-IDX)                 ELKIPGNF
00206         SET IPGN-CF-CALC-OK TO TRUE                               ELKIPGNF
00207      ELSE                                                         ELKIPGNF
00208         SET WS-INTERNAL-ERROR TO TRUE                             ELKIPGNF
00209         SET IPGN-CF-CALC-FAIL TO TRUE.                            ELKIPGNF
00210                                                                   ELKIPGNF
00211 ******************************************************************ELKIPGNF
00212 *                                                                *ELKIPGNF
00213 *    COMPUTE WEIGHTED LIST MATCH CONFIDENCE FACTORS              *ELKIPGNF
00214 *                                                                *ELKIPGNF
00215 ******************************************************************ELKIPGNF
00216                                                                   ELKIPGNF
00217  3000-COMPUTE-WEIGHTED-MATCH.                                     ELKIPGNF
00218                                                                   ELKIPGNF
00219      SET IPGN-MAX-IDX TO IPGN-TBL-CNT.                            ELKIPGNF
00220      SET ADDRESS OF PVNW-PRVDR-NBR-TBL TO                         ELKIPGNF
00221          ADDRESS OF LS-MATCH-LIST.                                ELKIPGNF
00222      IF ADDRESS OF PVNW-PRVDR-NBR-TBL = NULL                      ELKIPGNF
00223         SET WS-MISSING-PARM TO TRUE                               ELKIPGNF
00224      ELSE                                                         ELKIPGNF
00225       PERFORM 3100-COMPUTE-WEIGHTED-SLOT                          ELKIPGNF
00226           VARYING IPGN-IDX FROM 1 BY 1                            ELKIPGNF
00227             UNTIL IPGN-IDX > IPGN-MAX-IDX OR                      ELKIPGNF
00228               WS-INTERNAL-ERROR.                                  ELKIPGNF
00229                                                                   ELKIPGNF
00230 ******************************************************************ELKIPGNF
00231 *                                                                *ELKIPGNF
00232 *    COMPUTE WEIGHTED LIST MATCH CONFIDENCE FACTORS FOR SLOTS    *ELKIPGNF
00233 *                                                                *ELKIPGNF
00234 ******************************************************************ELKIPGNF
00235                                                                   ELKIPGNF
00236  3100-COMPUTE-WEIGHTED-SLOT.                                      ELKIPGNF
00237                                                                   ELKIPGNF
00238      SET ADDRESS OF IPGN-TABULAR-RECORD TO                        ELKIPGNF
00239                    IPGN-TABULAR-PTR (IPGN-IDX).                   ELKIPGNF
00240      CALL 'ELKIPGNW' USING IPGN-TABULAR-RECORD                    ELKIPGNF
00241                            PVNW-PRVDR-NBR-TBL                     ELKIPGNF
00242                              CFDB-CNFDNC-FCTR-DATA-BLCK.          ELKIPGNF
00243      IF RETURN-CODE = ZERO                                        ELKIPGNF
00244         MOVE CFDB-CF-IPGN-WGHTD-LST-MTCH TO                       ELKIPGNF
00245                      IPGN-CF-LIST-MTCH (IPGN-IDX)                 ELKIPGNF
00246         SET IPGN-CF-CALC-OK TO TRUE                               ELKIPGNF
00247      ELSE                                                         ELKIPGNF
00248         SET WS-INTERNAL-ERROR TO TRUE                             ELKIPGNF
00249         SET IPGN-CF-CALC-FAIL TO TRUE.                            ELKIPGNF
