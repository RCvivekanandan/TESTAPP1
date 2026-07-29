00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGSF
00003  PROGRAM-ID.           ELKIPGSF.                                     LV004
00004                                                                   ELKIPGSF
00005  AUTHOR.               ANNE KEFFER KING.                          ELKIPGSF
00006                                                                   ELKIPGSF
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGSF
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGSF
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGSF
00010                        233 N. MICHIGAN AVE                        ELKIPGSF
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGSF
00012                                                                   ELKIPGSF
00013  DATE-WRITTEN.         25-AUG-2000.                               ELKIPGSF
00014                                                                   ELKIPGSF
00015  ENVIRONMENT DIVISION.                                            ELKIPGSF
00016                                                                   ELKIPGSF
00017  CONFIGURATION SECTION.                                           ELKIPGSF
00018                                                                   ELKIPGSF
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKIPGSF
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKIPGSF
00021                                                                   ELKIPGSF
00022 ******************************************************************ELKIPGSF
00023 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIPGSF
00024 *  ELKIPGSF - COMPUTE CONFIDENCE FACTORS FOR A LIST OF IPGS      *ELKIPGSF
00025 *             TABULARS.                                          *ELKIPGSF
00026 *                                                                *ELKIPGSF
00027 *              THIS PROGRAM CONTROLS THE COMPUTATION OF CONFI-   *ELKIPGSF
00028 *              DENCE FACTORS BASED ON #IPGS TABULARS.  FACTORS   *ELKIPGSF
00029 *              FOR A LIST OF ONE OR MORE #IPGS TABS ARE COMPUTED *ELKIPGSF
00030 *              UNDER CONTROL OF THIS PROGRAM.  FACTORS ARE       *ELKIPGSF
00031 *              COMPUTED FOR OVERALL ACCUMULATOR DETERMINATION,   *ELKIPGSF
00032 *              UNWEIGHTED LIST MATCHING OR WEIGHTED LIST MATCH-  *ELKIPGSF
00033 *              ING, DEPENDING ON THE SETTING OF THE INPUT PARMS. *ELKIPGSF
00034 *                                                                *ELKIPGSF
00035 ******************************************************************ELKIPGSF
00036 *                      MAINTENANCE HISTORY                       *ELKIPGSF
00037 *                                                                *ELKIPGSF
00038 *  MOD     DATE      BY                    ACTION                *ELKIPGSF
00039 * ----- ----------- --- -----------------------------------------*ELKIPGSF
00040 * 01.00 24-AUG-2000 AKK CONED FROM ELKIPGTF                      *ELKIPGSF
00041 *                                                                *ELKIPGSF
00042 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGSF
00043 *                                                                *ELKIPGSF
00044 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGSF
00045 *                                                                *ELKIPGSF
00046 * 02.01 09-JAN-2004 AKK S0C7                                     *ELKIPGSF
00047 *                                                                *ELKIPGSF
00048 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGSF
00049 *                             CICSCB3 COMPILER FIX             *  ELKIPGSF
00050 ******************************************************************ELKIPGSF
00051 /                                                                 ELKIPGSF
00052  DATA DIVISION.                                                   ELKIPGSF
00053                                                                   ELKIPGSF
00054  WORKING-STORAGE SECTION.                                         ELKIPGSF
00055                                                                   ELKIPGSF
00056  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIPGSF
00057    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIPGSF
00058    88  WS-EXTRA-PARM                   VALUE +4.                  ELKIPGSF
00059    88  WS-MISSING-PARM                 VALUE +12.                 ELKIPGSF
00060    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIPGSF
00061                                                                   ELKIPGSF
00062                                                                   ELKIPGSF
00063      COPY ELSCFDBC.                                               ELKIPGSF
00064                                                                   ELKIPGSF
00065                                                                   ELKIPGSF
00066 /                                                                 ELKIPGSF
00067  LINKAGE SECTION.                                                 ELKIPGSF
00068                                                                   ELKIPGSF
00069 /                                                                 ELKIPGSF
00070                                                                   ELKIPGSF
00071      COPY ELSIPGSC.                                               ELKIPGSF
00072                                                                   ELKIPGSF
00073  01 LS-MATCH-LIST              PIC X.                             ELKIPGSF
00074                                                                   ELKIPGSF
00075      COPY ELSPVSLC.                                               ELKIPGSF
00076                                                                   ELKIPGSF
00077                                                                   ELKIPGSF
00078      COPY ELSPVSWC.                                               ELKIPGSF
00079                                                                   ELKIPGSF
00080                                                                   ELKIPGSF
00081  01 IPGS-TABULAR-RECORD.                                          ELKIPGSF
00082      COPY GCTIPGSC.                                               ELKIPGSF
00083                                                                   ELKIPGSF
00084 /*****************************************************************ELKIPGSF
00085 *                                                                *ELKIPGSF
00086 *    PROCEDURE DIVISION                                          *ELKIPGSF
00087 *                                                                *ELKIPGSF
00088 ******************************************************************ELKIPGSF
00089                                                                   ELKIPGSF
00090  PROCEDURE DIVISION USING IPGS-INTERNAL-TABS-TABLE                ELKIPGSF
00091                             LS-MATCH-LIST.                        ELKIPGSF
00092                                                                   ELKIPGSF
00093  0000-DETERMINE-IPGS-CONFIDENCE.                                  ELKIPGSF
00094                                                                   ELKIPGSF
00095      IF ADDRESS OF IPGS-INTERNAL-TABS-TABLE = NULL                ELKIPGSF
00096         SET WS-MISSING-PARM TO TRUE                               ELKIPGSF
00097      ELSE                                                         ELKIPGSF
00098         PERFORM 0100-INITIALIZATION                               ELKIPGSF
00099         PERFORM 1000-PROCESS-IPGS-TABULAR.                        ELKIPGSF
00100      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGSF
00101      GOBACK.                                                      ELKIPGSF
00102                                                                   ELKIPGSF
00103 ******************************************************************ELKIPGSF
00104 *                                                                *ELKIPGSF
00105 *    INITIALIZATION                                              *ELKIPGSF
00106 *                                                                *ELKIPGSF
00107 ******************************************************************ELKIPGSF
00108                                                                   ELKIPGSF
00109  0100-INITIALIZATION.                                             ELKIPGSF
00110                                                                   ELKIPGSF
00111      INITIALIZE CFDB-CF-IPGS.                                     ELKIPGSF
00112                                                                   ELKIPGSF
00113 ******************************************************************ELKIPGSF
00114 *                                                                *ELKIPGSF
00115 *    PROCESS-IPGS-TABULAR                                        *ELKIPGSF
00116 *                                                                *ELKIPGSF
00117 ******************************************************************ELKIPGSF
00118                                                                   ELKIPGSF
00119  1000-PROCESS-IPGS-TABULAR.                                       ELKIPGSF
00120                                                                   ELKIPGSF
00121      EVALUATE TRUE                                                ELKIPGSF
00122      WHEN IPGS-CF-CALC-OV                                         ELKIPGSF
00123         PERFORM 1100-COMPUTE-OVERALL-CONF,                        ELKIPGSF
00124      WHEN IPGS-CF-CALC-MTCH                                       ELKIPGSF
00125         PERFORM 2000-COMPUTE-UNWEIGHTED-MATCH,                    ELKIPGSF
00126      WHEN IPGS-CF-CALC-WT-MTCH                                    ELKIPGSF
00127         PERFORM 3000-COMPUTE-WEIGHTED-MATCH,                      ELKIPGSF
00128      WHEN OTHER SET WS-MISSING-PARM TO TRUE                       ELKIPGSF
00129      END-EVALUATE.                                                ELKIPGSF
00130                                                                   ELKIPGSF
00131 ******************************************************************ELKIPGSF
00132 *                                                                *ELKIPGSF
00133 *    COMPUTE OVERALL CONFIDENCE FACTORS                          *ELKIPGSF
00134 *                                                                *ELKIPGSF
00135 ******************************************************************ELKIPGSF
00136                                                                   ELKIPGSF
00137  1100-COMPUTE-OVERALL-CONF.                                       ELKIPGSF
00138                                                                   ELKIPGSF
00139      SET IPGS-MAX-IDX TO IPGS-TBL-CNT.                            ELKIPGSF
00140      SET ADDRESS OF PVSL-PRVDR-SPC-TBL TO                         ELKIPGSF
00141          ADDRESS OF LS-MATCH-LIST.                                ELKIPGSF
00142      IF ADDRESS OF PVSL-PRVDR-SPC-TBL NOT EQUAL NULL              ELKIPGSF
00143         SET WS-EXTRA-PARM TO TRUE.                                ELKIPGSF
00144      PERFORM 1200-COMPUTE-OVERALL-CONF-SLOT                       ELKIPGSF
00145           VARYING IPGS-IDX FROM 1 BY 1                            ELKIPGSF
00146             UNTIL IPGS-IDX > IPGS-MAX-IDX OR                      ELKIPGSF
00147               WS-INTERNAL-ERROR.                                  ELKIPGSF
00148                                                                   ELKIPGSF
00149 ******************************************************************ELKIPGSF
00150 *                                                                *ELKIPGSF
00151 *    COMPUTE OVERALL CONFIDENCE FACTORS FOR EACH SLOT NUMBER     *ELKIPGSF
00152 *                                                                *ELKIPGSF
00153 ******************************************************************ELKIPGSF
00154                                                                   ELKIPGSF
00155  1200-COMPUTE-OVERALL-CONF-SLOT.                                  ELKIPGSF
00156                                                                   ELKIPGSF
00157      SET ADDRESS OF IPGS-TABULAR-RECORD TO                        ELKIPGSF
00158                        IPGS-TABULAR-PTR (IPGS-IDX).               ELKIPGSF
00159      CALL 'ELKIPGSC' USING IPGS-TABULAR-RECORD                    ELKIPGSF
00160                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIPGSF
00161      IF RETURN-CODE = ZERO                                        ELKIPGSF
00162         MOVE CFDB-CF-IPGS-PRFSNL TO IPGS-CF-PROF (IPGS-IDX)       ELKIPGSF
00163         MOVE CFDB-CF-IPGS-PLAN TO IPGS-CF-PLAN (IPGS-IDX)         ELKIPGSF
00164         MOVE CFDB-CF-IPGS-NON-PLAN TO IPGS-CF-NON-PLAN (IPGS-IDX) ELKIPGSF
00165         MOVE CFDB-CF-IPGS-OV TO IPGS-CF-OV (IPGS-IDX)             ELKIPGSF
00166         SET IPGS-CF-CALC-OK TO TRUE                               ELKIPGSF
00167      ELSE                                                         ELKIPGSF
00168         SET WS-INTERNAL-ERROR TO TRUE                             ELKIPGSF
00169         SET IPGS-CF-CALC-FAIL TO TRUE.                            ELKIPGSF
00170                                                                   ELKIPGSF
00171 ******************************************************************ELKIPGSF
00172 *                                                                *ELKIPGSF
00173 *    COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE FACTORS            *ELKIPGSF
00174 *                                                                *ELKIPGSF
00175 ******************************************************************ELKIPGSF
00176                                                                   ELKIPGSF
00177  2000-COMPUTE-UNWEIGHTED-MATCH.                                   ELKIPGSF
00178                                                                   ELKIPGSF
00179      SET IPGS-MAX-IDX TO IPGS-TBL-CNT.                            ELKIPGSF
00180      SET ADDRESS OF PVSL-PRVDR-SPC-TBL TO                         ELKIPGSF
00181          ADDRESS OF LS-MATCH-LIST.                                ELKIPGSF
00182      IF ADDRESS OF PVSL-PRVDR-SPC-TBL = NULL                      ELKIPGSF
00183         SET WS-MISSING-PARM TO TRUE                               ELKIPGSF
00184      ELSE                                                         ELKIPGSF
00185      PERFORM 2100-COMPUTE-UNWEIGHTED-SLOT                         ELKIPGSF
00186           VARYING IPGS-IDX FROM 1 BY 1                            ELKIPGSF
00187             UNTIL IPGS-IDX > IPGS-MAX-IDX OR                      ELKIPGSF
00188               WS-INTERNAL-ERROR.                                  ELKIPGSF
00189                                                                   ELKIPGSF
00190 ******************************************************************ELKIPGSF
00191 *                                                                *ELKIPGSF
00192 *    COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE FACTORS FOR SLOTS  *ELKIPGSF
00193 *                                                                *ELKIPGSF
00194 ******************************************************************ELKIPGSF
00195                                                                   ELKIPGSF
00196  2100-COMPUTE-UNWEIGHTED-SLOT.                                    ELKIPGSF
00197                                                                   ELKIPGSF
00198      SET ADDRESS OF IPGS-TABULAR-RECORD TO                        ELKIPGSF
00199                        IPGS-TABULAR-PTR (IPGS-IDX).               ELKIPGSF
00200      CALL 'ELKIPGSM' USING IPGS-TABULAR-RECORD                    ELKIPGSF
00201                            PVSL-PRVDR-SPC-TBL                     ELKIPGSF
00202                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIPGSF
00203      IF RETURN-CODE = ZERO                                        ELKIPGSF
00204         MOVE CFDB-CF-IPGS-UNWGHTD-LST-MTCH TO                     ELKIPGSF
00205                        IPGS-CF-LIST-MTCH (IPGS-IDX)               ELKIPGSF
00206         SET IPGS-CF-CALC-OK TO TRUE                               ELKIPGSF
00207      ELSE                                                         ELKIPGSF
00208         SET WS-INTERNAL-ERROR TO TRUE                             ELKIPGSF
00209         SET IPGS-CF-CALC-FAIL TO TRUE.                            ELKIPGSF
00210                                                                   ELKIPGSF
00211 ******************************************************************ELKIPGSF
00212 *                                                                *ELKIPGSF
00213 *    COMPUTE WEIGHTED LIST MATCH CONFIDENCE FACTORS              *ELKIPGSF
00214 *                                                                *ELKIPGSF
00215 ******************************************************************ELKIPGSF
00216                                                                   ELKIPGSF
00217  3000-COMPUTE-WEIGHTED-MATCH.                                     ELKIPGSF
00218                                                                   ELKIPGSF
00219      SET IPGS-MAX-IDX TO IPGS-TBL-CNT.                            ELKIPGSF
00220      SET ADDRESS OF PVSW-PRVDR-SPC-TBL TO                         ELKIPGSF
00221          ADDRESS OF LS-MATCH-LIST.                                ELKIPGSF
00222      IF ADDRESS OF PVSW-PRVDR-SPC-TBL = NULL                      ELKIPGSF
00223         SET WS-MISSING-PARM TO TRUE                               ELKIPGSF
00224      ELSE                                                         ELKIPGSF
00225      PERFORM 3100-COMPUTE-WEIGHTED-SLOT                           ELKIPGSF
00226           VARYING IPGS-IDX FROM 1 BY 1                            ELKIPGSF
00227             UNTIL IPGS-IDX > IPGS-MAX-IDX OR                      ELKIPGSF
00228               WS-INTERNAL-ERROR.                                  ELKIPGSF
00229                                                                   ELKIPGSF
00230 ******************************************************************ELKIPGSF
00231 *                                                                *ELKIPGSF
00232 *    COMPUTE WEIGHTED LIST MATCH CONFIDENCE FACTORS FOR SLOTS    *ELKIPGSF
00233 *                                                                *ELKIPGSF
00234 ******************************************************************ELKIPGSF
00235                                                                   ELKIPGSF
00236  3100-COMPUTE-WEIGHTED-SLOT.                                      ELKIPGSF
00237                                                                   ELKIPGSF
00238      SET ADDRESS OF IPGS-TABULAR-RECORD TO                        ELKIPGSF
00239                        IPGS-TABULAR-PTR (IPGS-IDX).               ELKIPGSF
00240      CALL 'ELKIPGSW' USING IPGS-TABULAR-RECORD                    ELKIPGSF
00241                             PVSW-PRVDR-SPC-TBL                    ELKIPGSF
00242                                CFDB-CNFDNC-FCTR-DATA-BLCK.        ELKIPGSF
00243      IF RETURN-CODE = ZERO                                        ELKIPGSF
00244         MOVE CFDB-CF-IPGS-WGHTD-LST-MTCH TO                       ELKIPGSF
00245                        IPGS-CF-LIST-MTCH (IPGS-IDX)               ELKIPGSF
00246         SET IPGS-CF-CALC-OK TO TRUE                               ELKIPGSF
00247      ELSE                                                         ELKIPGSF
00248         SET WS-INTERNAL-ERROR TO TRUE                             ELKIPGSF
00249         SET IPGS-CF-CALC-FAIL TO TRUE.                            ELKIPGSF
00250                                                                   ELKIPGSF
00251                                                                   ELKIPGSF
