00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIDGDF
00003  PROGRAM-ID.           ELKIDGDF.                                     LV004
00004                                                                   ELKIDGDF
00005  AUTHOR.               BARBARA KEIB.                              ELKIDGDF
00006                                                                   ELKIDGDF
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIDGDF
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIDGDF
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIDGDF
00010                        233 N. MICHIGAN AVE                        ELKIDGDF
00011                        CHICAGO, ILLINOIS 60601                    ELKIDGDF
00012                                                                   ELKIDGDF
00013  DATE-WRITTEN.         28-SEP-1992.                               ELKIDGDF
00014                                                                   ELKIDGDF
00015  ENVIRONMENT DIVISION.                                            ELKIDGDF
00016                                                                   ELKIDGDF
00017  CONFIGURATION SECTION.                                           ELKIDGDF
00018                                                                   ELKIDGDF
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKIDGDF
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKIDGDF
00021                                                                   ELKIDGDF
00022 ******************************************************************ELKIDGDF
00023 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIDGDF
00024 *  ELKIDGDF - COMPUTE CONFIDENCE FACTORS FOR A LIST OF IDGD      *ELKIDGDF
00025 *             TABULARS.                                          *ELKIDGDF
00026 *                                                                *ELKIDGDF
00027 *              THIS PROGRAM CONTROLS THE COMPUTATION OF CONFI-   *ELKIDGDF
00028 *              DENCE FACTORS BASED ON #IDGD TABULARS.  FACTORS   *ELKIDGDF
00029 *              FOR A LIST OF ONE OR MORE #IDGD TABS ARE COMPUTED *ELKIDGDF
00030 *              UNDER CONTROL OF THIS PROGRAM.  FACTORS ARE       *ELKIDGDF
00031 *              COMPUTEDC FOR OVERALL ACCUMULATOR DETERMINATION,  *ELKIDGDF
00032 *              UNWEIGHTED LIST MATCHING OR WEIGHTED LIST MATCH-  *ELKIDGDF
00033 *              ING, DEPENDING ON THE SETTING OF THE INPUT PARMS. *ELKIDGDF
00034 *                                                                *ELKIDGDF
00035 ******************************************************************ELKIDGDF
00036 *                      MAINTENANCE HISTORY                       *ELKIDGDF
00037 *  MOD     DATE      BY                    ACTION                *ELKIDGDF
00038 * ----- ----------- --- -----------------------------------------*ELKIDGDF
00039 * 01.00 28-SEP-1992 BAK CREATED                                  *ELKIDGDF
00040 *                                                                *ELKIDGDF
00041 * 01.06 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX         *ELKIDGDF
00042 *                             ASM RECOMPILES                     *ELKIDGDF
00043 *                                                                *ELKIDGDF
00044 * 02.00 07-MAY-2003 AKK       REGEN'D FOR EXPANSION OF           *ELKIDGDF
00045 *                             PROC/DIAGNOSIS.                    *ELKIDGDF
00046 *                                                                *ELKIDGDF
00047 * 02.01 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIDGDF
00048 *                                                                *ELKIDGDF
00049 * 02.02 09-JAN-2004 AKK S0C7 INTERTEST                           *ELKIDGDF
00050 *                                                                *ELKIDGDF
00051 * 02.03 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIDGDF
00052 *                             CICSCB3 COMPILER FIX             *  ELKIDGDF
00053 ******************************************************************ELKIDGDF
00054 /                                                                 ELKIDGDF
00055  DATA DIVISION.                                                   ELKIDGDF
00056                                                                   ELKIDGDF
00057  WORKING-STORAGE SECTION.                                         ELKIDGDF
00058                                                                   ELKIDGDF
00059  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIDGDF
00060    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIDGDF
00061    88  WS-EXTRA-PARM                   VALUE +4.                  ELKIDGDF
00062    88  WS-MISSING-PARM                 VALUE +12.                 ELKIDGDF
00063    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIDGDF
00064                                                                   ELKIDGDF
00065                                                                   ELKIDGDF
00066      COPY ELSCFDBC.                                               ELKIDGDF
00067                                                                   ELKIDGDF
00068 /                                                                 ELKIDGDF
00069  LINKAGE SECTION.                                                 ELKIDGDF
00070                                                                   ELKIDGDF
00071 /                                                                 ELKIDGDF
00072                                                                   ELKIDGDF
00073      COPY ELSIDGDC.                                               ELKIDGDF
00074                                                                   ELKIDGDF
00075                                                                   ELKIDGDF
00076  01  LS-MATCH-LIST               PIC X.                           ELKIDGDF
00077                                                                   ELKIDGDF
00078      COPY ELSDXSLC.                                               ELKIDGDF
00079                                                                   ELKIDGDF
00080                                                                   ELKIDGDF
00081      COPY ELSDXSWC.                                               ELKIDGDF
00082                                                                   ELKIDGDF
00083                                                                   ELKIDGDF
00084  01  IDGD-TABULAR-RECORD.                                         ELKIDGDF
00085      COPY GCTIDGDC.                                               ELKIDGDF
00086                                                                   ELKIDGDF
00087                                                                   ELKIDGDF
00088 /*****************************************************************ELKIDGDF
00089 *                                                                *ELKIDGDF
00090 *    PROCEDURE DIVISION                                          *ELKIDGDF
00091 *                                                                *ELKIDGDF
00092 ******************************************************************ELKIDGDF
00093                                                                   ELKIDGDF
00094  PROCEDURE DIVISION USING IDGD-INTERNAL-TABS-TABLE                ELKIDGDF
00095                            LS-MATCH-LIST.                         ELKIDGDF
00096                                                                   ELKIDGDF
00097  0000-DETERMINE-IDGD-CONFIDENCE.                                  ELKIDGDF
00098                                                                   ELKIDGDF
00099      IF ADDRESS OF IDGD-INTERNAL-TABS-TABLE = NULL                ELKIDGDF
00100         SET WS-MISSING-PARM TO TRUE                               ELKIDGDF
00101      ELSE                                                         ELKIDGDF
00102         PERFORM 0100-INITIALIZATION                               ELKIDGDF
00103         PERFORM 1000-PROCESS-IDGD-TABULAR.                        ELKIDGDF
00104      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIDGDF
00105      GOBACK.                                                      ELKIDGDF
00106                                                                   ELKIDGDF
00107 ******************************************************************ELKIDGDF
00108 *                                                                *ELKIDGDF
00109 *    INITIALIZATION                                              *ELKIDGDF
00110 *                                                                *ELKIDGDF
00111 ******************************************************************ELKIDGDF
00112                                                                   ELKIDGDF
00113  0100-INITIALIZATION.                                             ELKIDGDF
00114                                                                   ELKIDGDF
00115      INITIALIZE CFDB-CF-IDGD.                                     ELKIDGDF
00116                                                                   ELKIDGDF
00117 ******************************************************************ELKIDGDF
00118 *                                                                *ELKIDGDF
00119 *    PROCESS-IDGD-TABULAR                                        *ELKIDGDF
00120 *                                                                *ELKIDGDF
00121 ******************************************************************ELKIDGDF
00122                                                                   ELKIDGDF
00123  1000-PROCESS-IDGD-TABULAR.                                       ELKIDGDF
00124                                                                   ELKIDGDF
00125      EVALUATE TRUE                                                ELKIDGDF
00126      WHEN IDGD-CF-CALC-OV                                         ELKIDGDF
00127         PERFORM 1100-COMPUTE-OVERALL-CONF,                        ELKIDGDF
00128      WHEN IDGD-CF-CALC-MTCH                                       ELKIDGDF
00129         PERFORM 2000-COMPUTE-UNWEIGHTED-MATCH,                    ELKIDGDF
00130      WHEN IDGD-CF-CALC-WT-MTCH                                    ELKIDGDF
00131         PERFORM 3000-COMPUTE-WEIGHTED-MATCH,                      ELKIDGDF
00132      WHEN OTHER SET WS-MISSING-PARM TO TRUE                       ELKIDGDF
00133      END-EVALUATE.                                                ELKIDGDF
00134                                                                   ELKIDGDF
00135 ******************************************************************ELKIDGDF
00136 *                                                                *ELKIDGDF
00137 *    COMPUTE OVERALL CONFIDENCE FACTORS                          *ELKIDGDF
00138 *                                                                *ELKIDGDF
00139 ******************************************************************ELKIDGDF
00140                                                                   ELKIDGDF
00141  1100-COMPUTE-OVERALL-CONF.                                       ELKIDGDF
00142                                                                   ELKIDGDF
00143      SET IDGD-MAX-IDX TO IDGD-TBL-CNT.                            ELKIDGDF
00144      SET ADDRESS OF DXSL-DX-TBL TO                                ELKIDGDF
00145           ADDRESS OF LS-MATCH-LIST.                               ELKIDGDF
00146      IF ADDRESS OF DXSL-DX-TBL NOT EQUAL NULL                     ELKIDGDF
00147         SET WS-EXTRA-PARM TO TRUE.                                ELKIDGDF
00148      PERFORM 1200-COMPUTE-OVERALL-SLOT                            ELKIDGDF
00149           VARYING IDGD-IDX FROM 1 BY 1                            ELKIDGDF
00150             UNTIL IDGD-IDX > IDGD-MAX-IDX OR                      ELKIDGDF
00151               IDGD-CF-CALC-FAIL.                                  ELKIDGDF
00152                                                                   ELKIDGDF
00153 ******************************************************************ELKIDGDF
00154 *                                                                *ELKIDGDF
00155 *    COMPUTE OVERALL CONFIDENCE FACTORS FOR EACH SLOT NUMBER     *ELKIDGDF
00156 *                                                                *ELKIDGDF
00157 ******************************************************************ELKIDGDF
00158                                                                   ELKIDGDF
00159  1200-COMPUTE-OVERALL-SLOT.                                       ELKIDGDF
00160                                                                   ELKIDGDF
00161      SET ADDRESS OF IDGD-TABULAR-RECORD TO                        ELKIDGDF
00162                          IDGD-TABULAR-PTR (IDGD-IDX).             ELKIDGDF
00163      CALL 'ELKIDGDC' USING IDGD-TABULAR-RECORD                    ELKIDGDF
00164                           CFDB-CNFDNC-FCTR-DATA-BLCK.             ELKIDGDF
00165      IF RETURN-CODE = ZERO                                        ELKIDGDF
00166         MOVE CFDB-CF-IDGD-OV TO IDGD-CF-OV (IDGD-IDX)             ELKIDGDF
00167         SET IDGD-CF-CALC-OK TO TRUE                               ELKIDGDF
00168      ELSE                                                         ELKIDGDF
00169         SET IDGD-CF-CALC-FAIL TO TRUE.                            ELKIDGDF
00170         MOVE RETURN-CODE TO WS-RETURN-CODE.                       ELKIDGDF
00171                                                                   ELKIDGDF
00172 ******************************************************************ELKIDGDF
00173 *                                                                *ELKIDGDF
00174 *    COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE FACTORS            *ELKIDGDF
00175 *                                                                *ELKIDGDF
00176 ******************************************************************ELKIDGDF
00177                                                                   ELKIDGDF
00178  2000-COMPUTE-UNWEIGHTED-MATCH.                                   ELKIDGDF
00179                                                                   ELKIDGDF
00180      SET IDGD-MAX-IDX TO IDGD-TBL-CNT.                            ELKIDGDF
00181      SET ADDRESS OF DXSL-DX-TBL TO                                ELKIDGDF
00182           ADDRESS OF LS-MATCH-LIST.                               ELKIDGDF
00183      IF ADDRESS OF DXSL-DX-TBL = NULL                             ELKIDGDF
00184         SET WS-MISSING-PARM TO TRUE                               ELKIDGDF
00185      ELSE                                                         ELKIDGDF
00186      PERFORM 2100-COMPUTE-UNWEIGHTED-SLOT                         ELKIDGDF
00187           VARYING IDGD-IDX FROM 1 BY 1                            ELKIDGDF
00188             UNTIL IDGD-IDX > IDGD-MAX-IDX OR                      ELKIDGDF
00189               IDGD-CF-CALC-FAIL.                                  ELKIDGDF
00190                                                                   ELKIDGDF
00191 ******************************************************************ELKIDGDF
00192 *                                                                *ELKIDGDF
00193 *    COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE FACTORS FOR SLOTS  *ELKIDGDF
00194 *                                                                *ELKIDGDF
00195 ******************************************************************ELKIDGDF
00196                                                                   ELKIDGDF
00197  2100-COMPUTE-UNWEIGHTED-SLOT.                                    ELKIDGDF
00198                                                                   ELKIDGDF
00199      SET ADDRESS OF IDGD-TABULAR-RECORD TO                        ELKIDGDF
00200                          IDGD-TABULAR-PTR (IDGD-IDX).             ELKIDGDF
00201      CALL 'ELKIDGDM' USING IDGD-TABULAR-RECORD                    ELKIDGDF
00202                            DXSL-DX-TBL                            ELKIDGDF
00203                           CFDB-CNFDNC-FCTR-DATA-BLCK.             ELKIDGDF
00204      IF RETURN-CODE = ZERO                                        ELKIDGDF
00205         MOVE CFDB-CF-IDGD-UNWGHTD-LST-MTCH                        ELKIDGDF
00206                        TO IDGD-CF-LIST-MTCH (IDGD-IDX)            ELKIDGDF
00207         SET IDGD-CF-CALC-OK TO TRUE                               ELKIDGDF
00208      ELSE                                                         ELKIDGDF
00209         SET IDGD-CF-CALC-FAIL TO TRUE.                            ELKIDGDF
00210         MOVE RETURN-CODE TO WS-RETURN-CODE.                       ELKIDGDF
00211                                                                   ELKIDGDF
00212 ******************************************************************ELKIDGDF
00213 *                                                                *ELKIDGDF
00214 *    COMPUTE WEIGHTED LIST MATCH CONFIDENCE FACTORS              *ELKIDGDF
00215 *                                                                *ELKIDGDF
00216 ******************************************************************ELKIDGDF
00217                                                                   ELKIDGDF
00218  3000-COMPUTE-WEIGHTED-MATCH.                                     ELKIDGDF
00219                                                                   ELKIDGDF
00220      SET IDGD-MAX-IDX TO IDGD-TBL-CNT.                            ELKIDGDF
00221      SET ADDRESS OF DXSW-DX-TBL TO                                ELKIDGDF
00222           ADDRESS OF LS-MATCH-LIST.                               ELKIDGDF
00223      IF ADDRESS OF DXSW-DX-TBL = NULL                             ELKIDGDF
00224         SET WS-MISSING-PARM TO TRUE                               ELKIDGDF
00225      ELSE                                                         ELKIDGDF
00226      PERFORM 3100-COMPUTE-WEIGHTED-SLOT                           ELKIDGDF
00227           VARYING IDGD-IDX FROM 1 BY 1                            ELKIDGDF
00228             UNTIL IDGD-IDX > IDGD-MAX-IDX OR                      ELKIDGDF
00229               IDGD-CF-CALC-FAIL.                                  ELKIDGDF
00230                                                                   ELKIDGDF
00231 ******************************************************************ELKIDGDF
00232 *                                                                *ELKIDGDF
00233 *    COMPUTE WEIGHTED LIST MATCH CONFIDENCE FACTORS FOR SLOTS    *ELKIDGDF
00234 *                                                                *ELKIDGDF
00235 ******************************************************************ELKIDGDF
00236                                                                   ELKIDGDF
00237  3100-COMPUTE-WEIGHTED-SLOT.                                      ELKIDGDF
00238                                                                   ELKIDGDF
00239      SET ADDRESS OF IDGD-TABULAR-RECORD TO                        ELKIDGDF
00240                          IDGD-TABULAR-PTR (IDGD-IDX).             ELKIDGDF
00241      CALL 'ELKIDGDW' USING IDGD-TABULAR-RECORD                    ELKIDGDF
00242                             DXSW-DX-TBL                           ELKIDGDF
00243                           CFDB-CNFDNC-FCTR-DATA-BLCK.             ELKIDGDF
00244      IF RETURN-CODE = ZERO                                        ELKIDGDF
00245         MOVE CFDB-CF-IDGD-WGHTD-LST-MTCH                          ELKIDGDF
00246                        TO IDGD-CF-LIST-MTCH (IDGD-IDX)            ELKIDGDF
00247         SET IDGD-CF-CALC-OK TO TRUE                               ELKIDGDF
00248      ELSE                                                         ELKIDGDF
00249         SET IDGD-CF-CALC-FAIL TO TRUE.                            ELKIDGDF
00250         MOVE RETURN-CODE TO WS-RETURN-CODE.                       ELKIDGDF
00251                                                                   ELKIDGDF
00252                                                                   ELKIDGDF
