00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGTF
00003  PROGRAM-ID.           ELKIPGTF.                                     LV004
00004                                                                   ELKIPGTF
00005  AUTHOR.               BARBARA KEIB.                              ELKIPGTF
00006                                                                   ELKIPGTF
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGTF
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGTF
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGTF
00010                        233 N. MICHIGAN AVE                        ELKIPGTF
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGTF
00012                                                                   ELKIPGTF
00013                                                                   ELKIPGTF
00014  DATE-WRITTEN.         26-SEP-1992.                               ELKIPGTF
00015                                                                   ELKIPGTF
00016  ENVIRONMENT DIVISION.                                            ELKIPGTF
00017                                                                   ELKIPGTF
00018  CONFIGURATION SECTION.                                           ELKIPGTF
00019                                                                   ELKIPGTF
00020  SOURCE-COMPUTER.      IBM-3090.                                  ELKIPGTF
00021  OBJECT-COMPUTER.      IBM-3090.                                  ELKIPGTF
00022                                                                   ELKIPGTF
00023 ******************************************************************ELKIPGTF
00024 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIPGTF
00025 *                                                                *ELKIPGTF
00026 *  ELKIPGTF - COMPUTE CONFIDENCE FACTORS FOR A LIST OF IPGT      *ELKIPGTF
00027 *             TABULARS.                                          *ELKIPGTF
00028 *                                                                *ELKIPGTF
00029 *              THIS PROGRAM CONTROLS THE COMPUTATION OF CONFI-   *ELKIPGTF
00030 *              DENCE FACTORS BASED ON #IPGT TABULARS.  FACTORS   *ELKIPGTF
00031 *              FOR A LIST OF ONE OR MORE #IPGT TABS ARE COMPUTED *ELKIPGTF
00032 *              UNDER CONTROL OF THIS PROGRAM.  FACTORS ARE       *ELKIPGTF
00033 *              COMPUTED FOR OVERALL ACCUMULATOR DETERMINATION,   *ELKIPGTF
00034 *              UNWEIGHTED LIST MATCHING OR WEIGHTED LIST MATCH-  *ELKIPGTF
00035 *              ING, DEPENDING ON THE SETTING OF THE INPUT PARMS. *ELKIPGTF
00036 *                                                                *ELKIPGTF
00037 ******************************************************************ELKIPGTF
00038 *                      MAINTENANCE HISTORY                       *ELKIPGTF
00039 *                                                                *ELKIPGTF
00040 *  MOD     DATE      BY                    ACTION                *ELKIPGTF
00041 * ----- ----------- --- -----------------------------------------*ELKIPGTF
00042 * 01.00 25-SEP-1992 BAK CREATED                                  *ELKIPGTF
00043 *                                                                *ELKIPGTF
00044 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGTF
00045 *                                                                *ELKIPGTF
00046 * 01.02 09-JAN-2004 AKK S0C7 FOR INTERTEST                       *ELKIPGTF
00047 *                                                                *ELKIPGTF
00048 * 01.03 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGTF
00049 *                             CICSCB3 COMPILER FIX             *  ELKIPGTF
00050 ******************************************************************ELKIPGTF
00051 /                                                                 ELKIPGTF
00052  DATA DIVISION.                                                   ELKIPGTF
00053                                                                   ELKIPGTF
00054  WORKING-STORAGE SECTION.                                         ELKIPGTF
00055                                                                   ELKIPGTF
00056  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIPGTF
00057    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIPGTF
00058    88  WS-EXTRA-PARM                   VALUE +4.                  ELKIPGTF
00059    88  WS-MISSING-PARM                 VALUE +12.                 ELKIPGTF
00060    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIPGTF
00061                                                                   ELKIPGTF
00062                                                                   ELKIPGTF
00063      COPY ELSCFDBC.                                               ELKIPGTF
00064                                                                   ELKIPGTF
00065                                                                   ELKIPGTF
00066 /                                                                 ELKIPGTF
00067  LINKAGE SECTION.                                                 ELKIPGTF
00068                                                                   ELKIPGTF
00069 /                                                                 ELKIPGTF
00070                                                                   ELKIPGTF
00071      COPY ELSIPGTC.                                               ELKIPGTF
00072                                                                   ELKIPGTF
00073  01 LS-MATCH-LIST              PIC X.                             ELKIPGTF
00074                                                                   ELKIPGTF
00075      COPY ELSPVTLC.                                               ELKIPGTF
00076                                                                   ELKIPGTF
00077                                                                   ELKIPGTF
00078      COPY ELSPVTWC.                                               ELKIPGTF
00079                                                                   ELKIPGTF
00080                                                                   ELKIPGTF
00081  01 IPGT-TABULAR-RECORD.                                          ELKIPGTF
00082      COPY GCTIPGTC.                                               ELKIPGTF
00083                                                                   ELKIPGTF
00084 /*****************************************************************ELKIPGTF
00085 *                                                                *ELKIPGTF
00086 *    PROCEDURE DIVISION                                          *ELKIPGTF
00087 *                                                                *ELKIPGTF
00088 ******************************************************************ELKIPGTF
00089                                                                   ELKIPGTF
00090  PROCEDURE DIVISION USING IPGT-INTERNAL-TABS-TABLE                ELKIPGTF
00091                             LS-MATCH-LIST.                        ELKIPGTF
00092                                                                   ELKIPGTF
00093  0000-DETERMINE-IPGT-CONFIDENCE.                                  ELKIPGTF
00094                                                                   ELKIPGTF
00095      IF ADDRESS OF IPGT-INTERNAL-TABS-TABLE = NULL                ELKIPGTF
00096         SET WS-MISSING-PARM TO TRUE                               ELKIPGTF
00097      ELSE                                                         ELKIPGTF
00098         PERFORM 0100-INITIALIZATION                               ELKIPGTF
00099         PERFORM 1000-PROCESS-IPGT-TABULAR.                        ELKIPGTF
00100      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGTF
00101      GOBACK.                                                      ELKIPGTF
00102                                                                   ELKIPGTF
00103 ******************************************************************ELKIPGTF
00104 *                                                                *ELKIPGTF
00105 *    INITIALIZATION                                              *ELKIPGTF
00106 *                                                                *ELKIPGTF
00107 ******************************************************************ELKIPGTF
00108                                                                   ELKIPGTF
00109  0100-INITIALIZATION.                                             ELKIPGTF
00110                                                                   ELKIPGTF
00111      INITIALIZE CFDB-CF-IPGT.                                     ELKIPGTF
00112                                                                   ELKIPGTF
00113 ******************************************************************ELKIPGTF
00114 *                                                                *ELKIPGTF
00115 *    PROCESS-IPGT-TABULAR                                        *ELKIPGTF
00116 *                                                                *ELKIPGTF
00117 ******************************************************************ELKIPGTF
00118                                                                   ELKIPGTF
00119  1000-PROCESS-IPGT-TABULAR.                                       ELKIPGTF
00120                                                                   ELKIPGTF
00121      EVALUATE TRUE                                                ELKIPGTF
00122      WHEN IPGT-CF-CALC-OV                                         ELKIPGTF
00123         PERFORM 1100-COMPUTE-OVERALL-CONF,                        ELKIPGTF
00124      WHEN IPGT-CF-CALC-MTCH                                       ELKIPGTF
00125         PERFORM 2000-COMPUTE-UNWEIGHTED-MATCH,                    ELKIPGTF
00126      WHEN IPGT-CF-CALC-WT-MTCH                                    ELKIPGTF
00127         PERFORM 3000-COMPUTE-WEIGHTED-MATCH,                      ELKIPGTF
00128      WHEN OTHER SET WS-MISSING-PARM TO TRUE                       ELKIPGTF
00129      END-EVALUATE.                                                ELKIPGTF
00130                                                                   ELKIPGTF
00131 ******************************************************************ELKIPGTF
00132 *                                                                *ELKIPGTF
00133 *    COMPUTE OVERALL CONFIDENCE FACTORS                          *ELKIPGTF
00134 *                                                                *ELKIPGTF
00135 ******************************************************************ELKIPGTF
00136                                                                   ELKIPGTF
00137  1100-COMPUTE-OVERALL-CONF.                                       ELKIPGTF
00138                                                                   ELKIPGTF
00139      SET IPGT-MAX-IDX TO IPGT-TBL-CNT.                            ELKIPGTF
00140      SET ADDRESS OF PVTL-PRVDR-TYP-TBL TO                         ELKIPGTF
00141          ADDRESS OF LS-MATCH-LIST.                                ELKIPGTF
00142      IF ADDRESS OF PVTL-PRVDR-TYP-TBL NOT EQUAL NULL              ELKIPGTF
00143         SET WS-EXTRA-PARM TO TRUE.                                ELKIPGTF
00144      PERFORM 1200-COMPUTE-OVERALL-CONF-SLOT                       ELKIPGTF
00145           VARYING IPGT-IDX FROM 1 BY 1                            ELKIPGTF
00146             UNTIL IPGT-IDX > IPGT-MAX-IDX OR                      ELKIPGTF
00147               WS-INTERNAL-ERROR.                                  ELKIPGTF
00148                                                                   ELKIPGTF
00149 ******************************************************************ELKIPGTF
00150 *                                                                *ELKIPGTF
00151 *    COMPUTE OVERALL CONFIDENCE FACTORS FOR EACH SLOT NUMBER     *ELKIPGTF
00152 *                                                                *ELKIPGTF
00153 ******************************************************************ELKIPGTF
00154                                                                   ELKIPGTF
00155  1200-COMPUTE-OVERALL-CONF-SLOT.                                  ELKIPGTF
00156                                                                   ELKIPGTF
00157      SET ADDRESS OF IPGT-TABULAR-RECORD TO                        ELKIPGTF
00158                        IPGT-TABULAR-PTR (IPGT-IDX).               ELKIPGTF
00159      CALL 'ELKIPGTC' USING IPGT-TABULAR-RECORD                    ELKIPGTF
00160                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIPGTF
00161      IF RETURN-CODE = ZERO                                        ELKIPGTF
00162         MOVE CFDB-CF-IPGT-INSTTNL TO IPGT-CF-INST (IPGT-IDX)      ELKIPGTF
00163         MOVE CFDB-CF-IPGT-PRFSNL TO IPGT-CF-PROF (IPGT-IDX)       ELKIPGTF
00164         MOVE CFDB-CF-IPGT-PLAN TO IPGT-CF-PLAN (IPGT-IDX)         ELKIPGTF
00165         MOVE CFDB-CF-IPGT-NON-PLAN TO IPGT-CF-NON-PLAN (IPGT-IDX) ELKIPGTF
00166         MOVE CFDB-CF-IPGT-OV TO IPGT-CF-OV (IPGT-IDX)             ELKIPGTF
00167         SET IPGT-CF-CALC-OK TO TRUE                               ELKIPGTF
00168      ELSE                                                         ELKIPGTF
00169         SET WS-INTERNAL-ERROR TO TRUE                             ELKIPGTF
00170         SET IPGT-CF-CALC-FAIL TO TRUE.                            ELKIPGTF
00171                                                                   ELKIPGTF
00172 ******************************************************************ELKIPGTF
00173 *                                                                *ELKIPGTF
00174 *    COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE FACTORS            *ELKIPGTF
00175 *                                                                *ELKIPGTF
00176 ******************************************************************ELKIPGTF
00177                                                                   ELKIPGTF
00178  2000-COMPUTE-UNWEIGHTED-MATCH.                                   ELKIPGTF
00179                                                                   ELKIPGTF
00180      SET IPGT-MAX-IDX TO IPGT-TBL-CNT.                            ELKIPGTF
00181      SET ADDRESS OF PVTL-PRVDR-TYP-TBL TO                         ELKIPGTF
00182          ADDRESS OF LS-MATCH-LIST.                                ELKIPGTF
00183      IF ADDRESS OF PVTL-PRVDR-TYP-TBL = NULL                      ELKIPGTF
00184         SET WS-MISSING-PARM TO TRUE                               ELKIPGTF
00185      ELSE                                                         ELKIPGTF
00186      PERFORM 2100-COMPUTE-UNWEIGHTED-SLOT                         ELKIPGTF
00187           VARYING IPGT-IDX FROM 1 BY 1                            ELKIPGTF
00188             UNTIL IPGT-IDX > IPGT-MAX-IDX OR                      ELKIPGTF
00189               WS-INTERNAL-ERROR.                                  ELKIPGTF
00190                                                                   ELKIPGTF
00191 ******************************************************************ELKIPGTF
00192 *                                                                *ELKIPGTF
00193 *    COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE FACTORS FOR SLOTS  *ELKIPGTF
00194 *                                                                *ELKIPGTF
00195 ******************************************************************ELKIPGTF
00196                                                                   ELKIPGTF
00197  2100-COMPUTE-UNWEIGHTED-SLOT.                                    ELKIPGTF
00198                                                                   ELKIPGTF
00199      SET ADDRESS OF IPGT-TABULAR-RECORD TO                        ELKIPGTF
00200                        IPGT-TABULAR-PTR (IPGT-IDX).               ELKIPGTF
00201      CALL 'ELKIPGTM' USING IPGT-TABULAR-RECORD                    ELKIPGTF
00202                            PVTL-PRVDR-TYP-TBL                     ELKIPGTF
00203                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIPGTF
00204      IF RETURN-CODE = ZERO                                        ELKIPGTF
00205         MOVE CFDB-CF-IPGT-UNWGHTD-LST-MTCH TO                     ELKIPGTF
00206                        IPGT-CF-LIST-MTCH (IPGT-IDX)               ELKIPGTF
00207         SET IPGT-CF-CALC-OK TO TRUE                               ELKIPGTF
00208      ELSE                                                         ELKIPGTF
00209         SET WS-INTERNAL-ERROR TO TRUE                             ELKIPGTF
00210         SET IPGT-CF-CALC-FAIL TO TRUE.                            ELKIPGTF
00211                                                                   ELKIPGTF
00212 ******************************************************************ELKIPGTF
00213 *                                                                *ELKIPGTF
00214 *    COMPUTE WEIGHTED LIST MATCH CONFIDENCE FACTORS              *ELKIPGTF
00215 *                                                                *ELKIPGTF
00216 ******************************************************************ELKIPGTF
00217                                                                   ELKIPGTF
00218  3000-COMPUTE-WEIGHTED-MATCH.                                     ELKIPGTF
00219                                                                   ELKIPGTF
00220      SET IPGT-MAX-IDX TO IPGT-TBL-CNT.                            ELKIPGTF
00221      SET ADDRESS OF PVTW-PRVDR-TYP-TBL TO                         ELKIPGTF
00222          ADDRESS OF LS-MATCH-LIST.                                ELKIPGTF
00223      IF ADDRESS OF PVTW-PRVDR-TYP-TBL = NULL                      ELKIPGTF
00224         SET WS-MISSING-PARM TO TRUE                               ELKIPGTF
00225      ELSE                                                         ELKIPGTF
00226      PERFORM 3100-COMPUTE-WEIGHTED-SLOT                           ELKIPGTF
00227           VARYING IPGT-IDX FROM 1 BY 1                            ELKIPGTF
00228             UNTIL IPGT-IDX > IPGT-MAX-IDX OR                      ELKIPGTF
00229               WS-INTERNAL-ERROR.                                  ELKIPGTF
00230                                                                   ELKIPGTF
00231 ******************************************************************ELKIPGTF
00232 *                                                                *ELKIPGTF
00233 *    COMPUTE WEIGHTED LIST MATCH CONFIDENCE FACTORS FOR SLOTS    *ELKIPGTF
00234 *                                                                *ELKIPGTF
00235 ******************************************************************ELKIPGTF
00236                                                                   ELKIPGTF
00237  3100-COMPUTE-WEIGHTED-SLOT.                                      ELKIPGTF
00238                                                                   ELKIPGTF
00239      SET ADDRESS OF IPGT-TABULAR-RECORD TO                        ELKIPGTF
00240                        IPGT-TABULAR-PTR (IPGT-IDX).               ELKIPGTF
00241      CALL 'ELKIPGTW' USING IPGT-TABULAR-RECORD                    ELKIPGTF
00242                             PVTW-PRVDR-TYP-TBL                    ELKIPGTF
00243                                CFDB-CNFDNC-FCTR-DATA-BLCK.        ELKIPGTF
00244      IF RETURN-CODE = ZERO                                        ELKIPGTF
00245         MOVE CFDB-CF-IPGT-WGHTD-LST-MTCH TO                       ELKIPGTF
00246                        IPGT-CF-LIST-MTCH (IPGT-IDX)               ELKIPGTF
00247         SET IPGT-CF-CALC-OK TO TRUE                               ELKIPGTF
00248      ELSE                                                         ELKIPGTF
00249         SET WS-INTERNAL-ERROR TO TRUE                             ELKIPGTF
00250         SET IPGT-CF-CALC-FAIL TO TRUE.                            ELKIPGTF
00251                                                                   ELKIPGTF
00252                                                                   ELKIPGTF
