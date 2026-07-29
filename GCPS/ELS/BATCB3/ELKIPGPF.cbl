00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGPF
00003  PROGRAM-ID.           ELKIPGPF.                                     LV004
00004                                                                   ELKIPGPF
00005  AUTHOR.               BARBARA KEIB.                              ELKIPGPF
00006                                                                   ELKIPGPF
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGPF
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGPF
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGPF
00010                        233 N. MICHIGAN AVE                        ELKIPGPF
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGPF
00012                                                                   ELKIPGPF
00013  DATE-WRITTEN.         25-SEP-1992.                               ELKIPGPF
00014                                                                   ELKIPGPF
00015  ENVIRONMENT DIVISION.                                            ELKIPGPF
00016                                                                   ELKIPGPF
00017  CONFIGURATION SECTION.                                           ELKIPGPF
00018                                                                   ELKIPGPF
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKIPGPF
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKIPGPF
00021                                                                   ELKIPGPF
00022 ******************************************************************ELKIPGPF
00023 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIPGPF
00024 *                                                                *ELKIPGPF
00025 *  ELKIPGPF - COMPUTE CONFIDENCE FACTORS FOR A LIST OF IPGP      *ELKIPGPF
00026 *             TABULARS.                                          *ELKIPGPF
00027 *                                                                *ELKIPGPF
00028 *              THIS PROGRAM CONTROLS THE COMPUTATION OF CONFI-   *ELKIPGPF
00029 *              DENCE FACTORS BASED ON #IPGP TABULARS.  FACTORS   *ELKIPGPF
00030 *              FOR A LIST OF ONE OR MORE #IPGP TABS ARE COMPUTED *ELKIPGPF
00031 *              UNDER CONTROL OF THIS PROGRAM.  FACTORS ARE       *ELKIPGPF
00032 *              COMPUTEDC FOR OVERALL ACCUMULATOR DETERMINATION,  *ELKIPGPF
00033 *              UNWEIGHTED LIST MATCHING OR WEIGHTED LIST MATCH-  *ELKIPGPF
00034 *              ING, DEPENDING ON THE SETTING OF THE INPUT PARMS. *ELKIPGPF
00035 *                                                                *ELKIPGPF
00036 ******************************************************************ELKIPGPF
00037 *                      MAINTENANCE HISTORY                       *ELKIPGPF
00038 *                                                                *ELKIPGPF
00039 *  MOD     DATE      BY                    ACTION                *ELKIPGPF
00040 * ----- ----------- --- -----------------------------------------*ELKIPGPF
00041 * 01.00 25-SEP-1992 BAK CREATED                                  *ELKIPGPF
00042 *                                                                *ELKIPGPF
00043 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGPF
00044 *                                                                *ELKIPGPF
00045 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGPF
00046 *                                                                *ELKIPGPF
00047 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELKIPGPF
00048 *                                                                *ELKIPGPF
00049 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGPF
00050 *                             CICSCB3 COMPILER FIX             *  ELKIPGPF
00051 ******************************************************************ELKIPGPF
00052 /                                                                 ELKIPGPF
00053  DATA DIVISION.                                                   ELKIPGPF
00054                                                                   ELKIPGPF
00055  WORKING-STORAGE SECTION.                                         ELKIPGPF
00056                                                                   ELKIPGPF
00057  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIPGPF
00058    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIPGPF
00059    88  WS-EXTRA-PARM                   VALUE +4.                  ELKIPGPF
00060    88  WS-MISSING-PARM                 VALUE +12.                 ELKIPGPF
00061    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIPGPF
00062                                                                   ELKIPGPF
00063                                                                   ELKIPGPF
00064      COPY ELSCFDBC.                                               ELKIPGPF
00065                                                                   ELKIPGPF
00066                                                                   ELKIPGPF
00067 /                                                                 ELKIPGPF
00068  LINKAGE SECTION.                                                 ELKIPGPF
00069                                                                   ELKIPGPF
00070 /                                                                 ELKIPGPF
00071                                                                   ELKIPGPF
00072      COPY ELSIPGPC.                                               ELKIPGPF
00073                                                                   ELKIPGPF
00074  01  LS-MATCH-LIST               PIC X.                           ELKIPGPF
00075                                                                   ELKIPGPF
00076      COPY ELSPRCLC.                                               ELKIPGPF
00077                                                                   ELKIPGPF
00078                                                                   ELKIPGPF
00079      COPY ELSPRCWC.                                               ELKIPGPF
00080                                                                   ELKIPGPF
00081                                                                   ELKIPGPF
00082  01  IPGP-TABULAR-RECORD.                                         ELKIPGPF
00083      COPY GCTIPGPC.                                               ELKIPGPF
00084                                                                   ELKIPGPF
00085 /*****************************************************************ELKIPGPF
00086 *                                                                *ELKIPGPF
00087 *    PROCEDURE DIVISION                                          *ELKIPGPF
00088 *                                                                *ELKIPGPF
00089 ******************************************************************ELKIPGPF
00090                                                                   ELKIPGPF
00091  PROCEDURE DIVISION USING IPGP-INTERNAL-TABS-TABLE                ELKIPGPF
00092                             LS-MATCH-LIST.                        ELKIPGPF
00093                                                                   ELKIPGPF
00094  0000-DETERMINE-IPGP-CONFIDENCE.                                  ELKIPGPF
00095                                                                   ELKIPGPF
00096      IF ADDRESS OF IPGP-INTERNAL-TABS-TABLE = NULL                ELKIPGPF
00097         SET WS-MISSING-PARM TO TRUE                               ELKIPGPF
00098      ELSE                                                         ELKIPGPF
00099         PERFORM 0100-INITIALIZATION                               ELKIPGPF
00100         PERFORM 1000-PROCESS-IPGP-TABULAR.                        ELKIPGPF
00101      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGPF
00102      GOBACK.                                                      ELKIPGPF
00103                                                                   ELKIPGPF
00104 ******************************************************************ELKIPGPF
00105 *                                                                *ELKIPGPF
00106 *    INITIALIZATION                                              *ELKIPGPF
00107 *                                                                *ELKIPGPF
00108 ******************************************************************ELKIPGPF
00109                                                                   ELKIPGPF
00110  0100-INITIALIZATION.                                             ELKIPGPF
00111                                                                   ELKIPGPF
00112      INITIALIZE CFDB-CF-IPGP.                                     ELKIPGPF
00113                                                                   ELKIPGPF
00114 ******************************************************************ELKIPGPF
00115 *                                                                *ELKIPGPF
00116 *    PROCESS-IPGP-TABULAR                                        *ELKIPGPF
00117 *                                                                *ELKIPGPF
00118 ******************************************************************ELKIPGPF
00119                                                                   ELKIPGPF
00120  1000-PROCESS-IPGP-TABULAR.                                       ELKIPGPF
00121                                                                   ELKIPGPF
00122      EVALUATE TRUE                                                ELKIPGPF
00123      WHEN IPGP-CF-CALC-OV                                         ELKIPGPF
00124         PERFORM 1100-COMPUTE-OVERALL-CONF,                        ELKIPGPF
00125      WHEN IPGP-CF-CALC-MTCH                                       ELKIPGPF
00126         PERFORM 2000-COMPUTE-UNWEIGHTED-MATCH,                    ELKIPGPF
00127      WHEN IPGP-CF-CALC-WT-MTCH                                    ELKIPGPF
00128         PERFORM 3000-COMPUTE-WEIGHTED-MATCH,                      ELKIPGPF
00129      WHEN OTHER SET WS-MISSING-PARM TO TRUE                       ELKIPGPF
00130      END-EVALUATE.                                                ELKIPGPF
00131                                                                   ELKIPGPF
00132 ******************************************************************ELKIPGPF
00133 *                                                                *ELKIPGPF
00134 *    COMPUTE OVERALL CONFIDENCE FACTORS                          *ELKIPGPF
00135 *                                                                *ELKIPGPF
00136 ******************************************************************ELKIPGPF
00137                                                                   ELKIPGPF
00138  1100-COMPUTE-OVERALL-CONF.                                       ELKIPGPF
00139                                                                   ELKIPGPF
00140      SET IPGP-MAX-IDX TO IPGP-TBL-CNT.                            ELKIPGPF
00141      SET ADDRESS OF PRCL-DX-TBL TO                                ELKIPGPF
00142          ADDRESS OF LS-MATCH-LIST.                                ELKIPGPF
00143      IF ADDRESS OF PRCL-DX-TBL NOT EQUAL NULL                     ELKIPGPF
00144         SET WS-EXTRA-PARM TO TRUE.                                ELKIPGPF
00145      PERFORM 1200-COMPUTE-OVERALL-CONF-SLOT                       ELKIPGPF
00146           VARYING IPGP-IDX FROM 1 BY 1                            ELKIPGPF
00147             UNTIL IPGP-IDX > IPGP-MAX-IDX OR                      ELKIPGPF
00148               WS-INTERNAL-ERROR.                                  ELKIPGPF
00149                                                                   ELKIPGPF
00150 ******************************************************************ELKIPGPF
00151 *                                                                *ELKIPGPF
00152 *    COMPUTE OVERALL CONFIDENCE FACTORS FOR EACH SLOT NUMBER     *ELKIPGPF
00153 *                                                                *ELKIPGPF
00154 ******************************************************************ELKIPGPF
00155                                                                   ELKIPGPF
00156  1200-COMPUTE-OVERALL-CONF-SLOT.                                  ELKIPGPF
00157                                                                   ELKIPGPF
00158      SET ADDRESS OF IPGP-TABULAR-RECORD TO                        ELKIPGPF
00159                               IPGP-TABULAR-PTR (IPGP-IDX).        ELKIPGPF
00160      CALL 'ELKIPGPC' USING IPGP-TABULAR-RECORD                    ELKIPGPF
00161                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIPGPF
00162      IF RETURN-CODE = ZERO                                        ELKIPGPF
00163         MOVE CFDB-CF-IPGP-INSTTNL TO IPGP-CF-INST (IPGP-IDX)      ELKIPGPF
00164         MOVE CFDB-CF-IPGP-PRFSNL TO IPGP-CF-PROF (IPGP-IDX)       ELKIPGPF
00165         MOVE CFDB-CF-IPGP-OV TO IPGP-CF-OV (IPGP-IDX)             ELKIPGPF
00166         SET IPGP-CF-CALC-OK TO TRUE                               ELKIPGPF
00167      ELSE                                                         ELKIPGPF
00168         SET WS-INTERNAL-ERROR TO TRUE                             ELKIPGPF
00169         SET IPGP-CF-CALC-FAIL TO TRUE.                            ELKIPGPF
00170                                                                   ELKIPGPF
00171 ******************************************************************ELKIPGPF
00172 *                                                                *ELKIPGPF
00173 *    COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE FACTORS            *ELKIPGPF
00174 *                                                                *ELKIPGPF
00175 ******************************************************************ELKIPGPF
00176                                                                   ELKIPGPF
00177  2000-COMPUTE-UNWEIGHTED-MATCH.                                   ELKIPGPF
00178                                                                   ELKIPGPF
00179      SET IPGP-MAX-IDX TO IPGP-TBL-CNT.                            ELKIPGPF
00180      SET ADDRESS OF PRCL-DX-TBL TO                                ELKIPGPF
00181          ADDRESS OF LS-MATCH-LIST.                                ELKIPGPF
00182      IF ADDRESS OF PRCL-DX-TBL = NULL                             ELKIPGPF
00183         SET WS-MISSING-PARM TO TRUE                               ELKIPGPF
00184      ELSE                                                         ELKIPGPF
00185      PERFORM 2100-COMPUTE-UNWEIGHTED-SLOT                         ELKIPGPF
00186           VARYING IPGP-IDX FROM 1 BY 1                            ELKIPGPF
00187             UNTIL IPGP-IDX > IPGP-MAX-IDX OR                      ELKIPGPF
00188               WS-INTERNAL-ERROR.                                  ELKIPGPF
00189                                                                   ELKIPGPF
00190 ******************************************************************ELKIPGPF
00191 *                                                                *ELKIPGPF
00192 *    COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE FACTORS FOR SLOTS  *ELKIPGPF
00193 *                                                                *ELKIPGPF
00194 ******************************************************************ELKIPGPF
00195                                                                   ELKIPGPF
00196  2100-COMPUTE-UNWEIGHTED-SLOT.                                    ELKIPGPF
00197                                                                   ELKIPGPF
00198      SET ADDRESS OF IPGP-TABULAR-RECORD TO                        ELKIPGPF
00199                               IPGP-TABULAR-PTR (IPGP-IDX).        ELKIPGPF
00200      CALL 'ELKIPGPM' USING IPGP-TABULAR-RECORD                    ELKIPGPF
00201                            PRCL-DX-TBL                            ELKIPGPF
00202                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIPGPF
00203      IF RETURN-CODE = ZERO                                        ELKIPGPF
00204         MOVE CFDB-CF-IPGP-UNWGHTD-LST-MTCH                        ELKIPGPF
00205                            TO IPGP-CF-LIST-MTCH (IPGP-IDX)        ELKIPGPF
00206         SET IPGP-CF-CALC-OK TO TRUE                               ELKIPGPF
00207      ELSE                                                         ELKIPGPF
00208         SET WS-INTERNAL-ERROR TO TRUE                             ELKIPGPF
00209         SET IPGP-CF-CALC-FAIL TO TRUE.                            ELKIPGPF
00210                                                                   ELKIPGPF
00211 ******************************************************************ELKIPGPF
00212 *                                                                *ELKIPGPF
00213 *    COMPUTE WEIGHTED LIST MATCH CONFIDENCE FACTORS              *ELKIPGPF
00214 *                                                                *ELKIPGPF
00215 ******************************************************************ELKIPGPF
00216                                                                   ELKIPGPF
00217  3000-COMPUTE-WEIGHTED-MATCH.                                     ELKIPGPF
00218                                                                   ELKIPGPF
00219      SET IPGP-MAX-IDX TO IPGP-TBL-CNT.                            ELKIPGPF
00220      SET ADDRESS OF PRCW-DX-TBL TO                                ELKIPGPF
00221          ADDRESS OF LS-MATCH-LIST.                                ELKIPGPF
00222      IF ADDRESS OF PRCW-DX-TBL = NULL                             ELKIPGPF
00223         SET WS-MISSING-PARM TO TRUE                               ELKIPGPF
00224      ELSE                                                         ELKIPGPF
00225      PERFORM 3100-COMPUTE-WEIGHTED-SLOT                           ELKIPGPF
00226           VARYING IPGP-IDX FROM 1 BY 1                            ELKIPGPF
00227             UNTIL IPGP-IDX > IPGP-MAX-IDX OR                      ELKIPGPF
00228                WS-INTERNAL-ERROR.                                 ELKIPGPF
00229                                                                   ELKIPGPF
00230 ******************************************************************ELKIPGPF
00231 *                                                                *ELKIPGPF
00232 *    COMPUTE WEIGHTED LIST MATCH CONFIDENCE FACTORS FOR SLOTS    *ELKIPGPF
00233 *                                                                *ELKIPGPF
00234 ******************************************************************ELKIPGPF
00235                                                                   ELKIPGPF
00236  3100-COMPUTE-WEIGHTED-SLOT.                                      ELKIPGPF
00237                                                                   ELKIPGPF
00238      SET ADDRESS OF IPGP-TABULAR-RECORD TO                        ELKIPGPF
00239                               IPGP-TABULAR-PTR (IPGP-IDX).        ELKIPGPF
00240      CALL 'ELKIPGPW' USING IPGP-TABULAR-RECORD                    ELKIPGPF
00241                                PRCW-DX-TBL                        ELKIPGPF
00242                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIPGPF
00243      IF RETURN-CODE = ZERO                                        ELKIPGPF
00244         MOVE CFDB-CF-IPGP-WGHTD-LST-MTCH                          ELKIPGPF
00245                            TO IPGP-CF-LIST-MTCH (IPGP-IDX)        ELKIPGPF
00246         SET IPGP-CF-CALC-OK TO TRUE                               ELKIPGPF
00247      ELSE                                                         ELKIPGPF
00248         SET WS-INTERNAL-ERROR TO TRUE                             ELKIPGPF
00249         SET IPGP-CF-CALC-FAIL TO TRUE.                            ELKIPGPF
