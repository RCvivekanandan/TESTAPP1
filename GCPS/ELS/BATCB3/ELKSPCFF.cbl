00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKSPCFF
00003  PROGRAM-ID.           ELKSPCFF.                                     LV004
00004                                                                   ELKSPCFF
00005  AUTHOR.               BARBARA KEIB.                              ELKSPCFF
00006                                                                   ELKSPCFF
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKSPCFF
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKSPCFF
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKSPCFF
00010                        233 N. MICHIGAN AVE                        ELKSPCFF
00011                        CHICAGO, ILLINOIS 60601                    ELKSPCFF
00012                                                                   ELKSPCFF
00013  DATE-WRITTEN.         23-NOV-1992.                               ELKSPCFF
00014                                                                   ELKSPCFF
00015  ENVIRONMENT DIVISION.                                            ELKSPCFF
00016  CONFIGURATION SECTION.                                           ELKSPCFF
00017  SOURCE-COMPUTER. IBM-3090.                                       ELKSPCFF
00018  OBJECT-COMPUTER. IBM-3090.                                       ELKSPCFF
00019                                                                   ELKSPCFF
00020 ****************************************************************  ELKSPCFF
00021 *AKK 12/06/05 REGEN FOR TEST                                   *  ELKSPCFF
00022 *  ELKSPCFF :  COMPUTE CONFIDENCE FACTORS FOR SPECIAL ACCUMS.  *  ELKSPCFF
00023 *                                                              *  ELKSPCFF
00024 *              THIS PROGRAM COMPUTES THE PARTIAL CONFIDENCE    *  ELKSPCFF
00025 *              FACTORS FOR EACH AND EVERY ACCUMULATOR LOADED   *  ELKSPCFF
00026 *              INTO THE ACCUMULATOR TABLES FOR THE SPECIAL     *  ELKSPCFF
00027 *              ACCUMULATORS.                                   *  ELKSPCFF
00028 *                                                              *  ELKSPCFF
00029 *              -OVERALL PER:                                   *  ELKSPCFF
00030 *               CONDITION BITS                                 *  ELKSPCFF
00031 *               COST CONTAINMENT                               *  ELKSPCFF
00032 *               DIAGNOSES                                      *  ELKSPCFF
00033 *               INTERNAL DESCRIPTOR                            *  ELKSPCFF
00034 *               PLACE OF TREATMENT                             *  ELKSPCFF
00035 *               PROCEDURE CODES                                *  ELKSPCFF
00036 *               PROVIDER NUMBERS                               *  ELKSPCFF
00037 *               PROVIDER TYPES                                 *  ELKSPCFF
00038 *               SERVICE GROUP                                  *  ELKSPCFF
00039 *               VALUE QUALIFER                                 *  ELKSPCFF
00040 *              -OVERALL SPECIAL CONFIDENCE FACTOR              *  ELKSPCFF
00041 *                                                              *  ELKSPCFF
00042 *              THE PARTIAL CONFIDENCE FACTORS ARE RETURNED IN  *  ELKSPCFF
00043 *              THEIR RESPECTIVE CONTRACT SUMMARY ACCUMULATOR   *  ELKSPCFF
00044 *              TABLE ENTRIES, AND ARE AVAILABLE FOR USE BY     *  ELKSPCFF
00045 *              OTHER PROCESSES IN DETERMINING OVERALL AND      *  ELKSPCFF
00046 *              SPECIAL CASE ACCUMULATORS.                      *  ELKSPCFF
00047 *                                                              *  ELKSPCFF
00048 *                                                              *  ELKSPCFF
00049 ****************************************************************  ELKSPCFF
00050 *                      MAINTENANCE HISTORY                     *  ELKSPCFF
00051 *                                                              *  ELKSPCFF
00052 *  MOD     DATE      BY  DRPT              ACTION              *  ELKSPCFF
00053 * ----- ----------- --- ----- ---------------------------------*  ELKSPCFF
00054 * 01.00 23-NOV-1992 BAK       CREATED                          *  ELKSPCFF
00055 * 01.01 22-DEC-1992 BAK       CORRECTED INITIALIZE OF WORK     *  ELKSPCFF
00056 *                             AREAS.                           *  ELKSPCFF
00057 * 01.02 07-SEP-2000 AKK       ADDED SUPPORT FOR PROVIDER       *  ELKSPCFF
00058 *                             SPECIALTY TABULAR #IPGS.         *  ELKSPCFF
00059 *                                                                *ELKSPCFF
00060 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKSPCFF
00061 *                                                                *ELKSPCFF
00062 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKSPCFF
00063 *                                                                *ELKSPCFF
00064 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELKSPCFF
00065 *                                                                *ELKSPCFF
00066 * 02.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER CHANGES         *ELKSPCFF
00067 *                                                                *ELKSPCFF
00068 ****************************************************************  ELKSPCFF
00069                                                                   ELKSPCFF
00070                                                                   ELKSPCFF
00071  DATA DIVISION.                                                   ELKSPCFF
00072  WORKING-STORAGE SECTION.                                         ELKSPCFF
00073                                                                   ELKSPCFF
00074                                                                   ELKSPCFF
00075  77  FILLER                      PIC X(42)   VALUE                ELKSPCFF
00076      '***ELKSPCFF WORKING STORAGE BEGINS HERE***'.                ELKSPCFF
00077                                                                   ELKSPCFF
00078                                                                   ELKSPCFF
00079  01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.      ELKSPCFF
00080                                                                   ELKSPCFF
00081      88 WS-SUCCESSFULL-CALL       VALUE ZERO.                     ELKSPCFF
00082      88 WS-UNIDENT-PARM          VALUE +8.                        ELKSPCFF
00083      88 WS-MISSING-PARM          VALUE +12.                       ELKSPCFF
00084      88 WS-INTERNAL-ERROR        VALUE +16.                       ELKSPCFF
00085                                                                   ELKSPCFF
00086  01  WS-FACTOR-WEIGHTS.                                           ELKSPCFF
00087                                                                   ELKSPCFF
00088      02 WS-CW-TRUE               COMP-1      VALUE +1.000000E+00. ELKSPCFF
00089      02 WS-CW-FALSE              COMP-1      VALUE -1.000000E+00. ELKSPCFF
00090 *                                                                 ELKSPCFF
00091  01  WS-WEIGHTED-CONFIDENCE-FACTORS.                              ELKSPCFF
00092                                                                   ELKSPCFF
00093      02 WS-CW-SP-BNFT-PRVSN      COMP-1   VALUE +0.900000E+00.    ELKSPCFF
00094      02 WS-CW-SP-CNDTN-BTS       COMP-1   VALUE +0.900000E+00.    ELKSPCFF
00095      02 WS-CW-SP-CST-CNTNMT      COMP-1   VALUE +0.950000E+00.    ELKSPCFF
00096      02 WS-CW-SP-DGNSS           COMP-1   VALUE +0.850000E+00.    ELKSPCFF
00097      02 WS-CW-SP-INT-DSCRPT      COMP-1   VALUE +0.850000E+00.    ELKSPCFF
00098      02 WS-CW-SP-PLC-TRTMNT      COMP-1   VALUE +0.900000E+00.    ELKSPCFF
00099      02 WS-CW-SP-PRCDR           COMP-1   VALUE +0.850000E+00.    ELKSPCFF
00100      02 WS-CW-SP-PRVDR-NBR       COMP-1   VALUE +0.850000E+00.    ELKSPCFF
00101      02 WS-CW-SP-PRVDR-TYP       COMP-1   VALUE +0.850000E+00.    ELKSPCFF
00102      02 WS-CW-SP-PRVDR-SPC       COMP-1   VALUE +0.850000E+00.    ELKSPCFF
00103      02 WS-CW-SP-SRVC-GRP        COMP-1   VALUE +0.800000E+00.    ELKSPCFF
00104      02 WS-CW-SP-VL-QLFR         COMP-1   VALUE +0.850000E+00.    ELKSPCFF
00105 *                                                                 ELKSPCFF
00106 *                                                                 ELKSPCFF
00107  01  WS-WORK-CONFIDENCE-FACTORS.                                  ELKSPCFF
00108                                                                   ELKSPCFF
00109      02 WS-CW-SP                 COMP-1.                          ELKSPCFF
00110      02 WS-CW-0                  COMP-1.                          ELKSPCFF
00111      02 WS-CW-1                  COMP-1.                          ELKSPCFF
00112      02 WS-CW-2                  COMP-1.                          ELKSPCFF
00113      02 WS-CW-3                  COMP-1.                          ELKSPCFF
00114      02 WS-CW-4                  COMP-1.                          ELKSPCFF
00115      02 WS-CW-5                  COMP-1.                          ELKSPCFF
00116      02 WS-CW-6                  COMP-1.                          ELKSPCFF
00117      02 WS-CW-7                  COMP-1.                          ELKSPCFF
00118      02 WS-CW-8                  COMP-1.                          ELKSPCFF
00119      02 WS-CW-9                  COMP-1.                          ELKSPCFF
00120      02 WS-CW-10                 COMP-1.                          ELKSPCFF
00121      02 WS-CW-11                 COMP-1.                          ELKSPCFF
00122      02 WS-CW-12                 COMP-1.                          ELKSPCFF
00123 /                                                                 ELKSPCFF
00124  LINKAGE SECTION.                                                 ELKSPCFF
00125      COPY ELSATBLC.                                               ELKSPCFF
00126 *                                                                 ELKSPCFF
00127 /***********************************************************      ELKSPCFF
00128 *                                                          *      ELKSPCFF
00129 *    COMPUTE CONFIDENCE FACTORS FOR ACCUMULATORS           *      ELKSPCFF
00130 *                                                          *      ELKSPCFF
00131 ************************************************************      ELKSPCFF
00132                                                                   ELKSPCFF
00133  PROCEDURE DIVISION USING ATBL-ACCUMULATOR-TABLE.                 ELKSPCFF
00134                                                                   ELKSPCFF
00135      IF ADDRESS OF ATBL-ACCUMULATOR-TABLE = NULL                  ELKSPCFF
00136         SET WS-MISSING-PARM TO TRUE                               ELKSPCFF
00137      ELSE                                                         ELKSPCFF
00138         PERFORM 1000-PROCESS-ALL-ATBL-ENTRIES.                    ELKSPCFF
00139      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKSPCFF
00140      GOBACK.                                                      ELKSPCFF
00141                                                                   ELKSPCFF
00142 ************************************************************      ELKSPCFF
00143 *                                                          *      ELKSPCFF
00144 *   PROCESS ALL ATBL ENTRIES FOR OVERALL CONFIDENCE FACTOR *      ELKSPCFF
00145 *                                                          *      ELKSPCFF
00146 ************************************************************      ELKSPCFF
00147                                                                   ELKSPCFF
00148  1000-PROCESS-ALL-ATBL-ENTRIES.                                   ELKSPCFF
00149                                                                   ELKSPCFF
00150      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELKSPCFF
00151      PERFORM 2000-COMPUTE-SP-CONF-FACTOR                          ELKSPCFF
00152          VARYING ATBL-IDX FROM 1 BY 1                             ELKSPCFF
00153             UNTIL ATBL-IDX > ATBL-MAX-IDX.                        ELKSPCFF
00154                                                                   ELKSPCFF
00155 ************************************************************      ELKSPCFF
00156 *                                                          *      ELKSPCFF
00157 *   COMPUTE SPECIAL OVERALL CONFIDENCE FACTOR              *      ELKSPCFF
00158 *                                                          *      ELKSPCFF
00159 ************************************************************      ELKSPCFF
00160                                                                   ELKSPCFF
00161  2000-COMPUTE-SP-CONF-FACTOR.                                     ELKSPCFF
00162                                                                   ELKSPCFF
00163      INITIALIZE WS-WORK-CONFIDENCE-FACTORS.                       ELKSPCFF
00164                                                                   ELKSPCFF
00165      CALL 'ELKFLAND'                                              ELKSPCFF
00166         USING WS-CW-0                                             ELKSPCFF
00167               ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX)                    ELKSPCFF
00168               ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)                     ELKSPCFF
00169               ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)                    ELKSPCFF
00170               ATBL-CF-SP-DGNSS (ATBL-IDX)                         ELKSPCFF
00171               ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX)                ELKSPCFF
00172               ATBL-CF-SP-PLC-TRTMNT (ATBL-IDX)                    ELKSPCFF
00173               ATBL-CF-SP-PRCDR (ATBL-IDX)                         ELKSPCFF
00174               ATBL-CF-SP-PRVDR-NBR (ATBL-IDX)                     ELKSPCFF
00175               ATBL-CF-SP-PRVDR-TYP (ATBL-IDX)                     ELKSPCFF
00176               ATBL-CF-SP-PRVDR-SPC (ATBL-IDX)                     ELKSPCFF
00177               ATBL-CF-SP-SRVC-GRP (ATBL-IDX)                      ELKSPCFF
00178               ATBL-CF-SP-VL-QLFR (ATBL-IDX).                      ELKSPCFF
00179                                                                   ELKSPCFF
00180      IF WS-CW-0 = WS-CW-FALSE                                     ELKSPCFF
00181         MOVE WS-CW-FALSE TO ATBL-CF-SP (ATBL-IDX)                 ELKSPCFF
00182      ELSE                                                         ELKSPCFF
00183         IF WS-CW-0 = WS-CW-TRUE                                   ELKSPCFF
00184            MOVE WS-CW-TRUE TO ATBL-CF-SP (ATBL-IDX)               ELKSPCFF
00185      ELSE                                                         ELKSPCFF
00186         COMPUTE WS-CW-1  =                                        ELKSPCFF
00187            ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX) * WS-CW-SP-CNDTN-BTS  ELKSPCFF
00188         COMPUTE WS-CW-2  =                                        ELKSPCFF
00189            ATBL-CF-SP-CNDTN-BTS  (ATBL-IDX) * WS-CW-SP-CNDTN-BTS  ELKSPCFF
00190         COMPUTE WS-CW-3  =                                        ELKSPCFF
00191            ATBL-CF-SP-CST-CNTNMT (ATBL-IDX) * WS-CW-SP-CST-CNTNMT ELKSPCFF
00192         COMPUTE WS-CW-4  =                                        ELKSPCFF
00193            ATBL-CF-SP-DGNSS      (ATBL-IDX) * WS-CW-SP-DGNSS      ELKSPCFF
00194         COMPUTE WS-CW-5  =                                        ELKSPCFF
00195            ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX) *                 ELKSPCFF
00196                                               WS-CW-SP-INT-DSCRPT ELKSPCFF
00197         COMPUTE WS-CW-6  =                                        ELKSPCFF
00198            ATBL-CF-SP-PLC-TRTMNT (ATBL-IDX) * WS-CW-SP-PLC-TRTMNT ELKSPCFF
00199         COMPUTE WS-CW-7  =                                        ELKSPCFF
00200            ATBL-CF-SP-PRCDR      (ATBL-IDX) * WS-CW-SP-PRCDR      ELKSPCFF
00201         COMPUTE WS-CW-8  =                                        ELKSPCFF
00202            ATBL-CF-SP-PRVDR-NBR  (ATBL-IDX) * WS-CW-SP-PRVDR-NBR  ELKSPCFF
00203         COMPUTE WS-CW-9  =                                        ELKSPCFF
00204            ATBL-CF-SP-PRVDR-TYP  (ATBL-IDX) * WS-CW-SP-PRVDR-TYP  ELKSPCFF
00205         COMPUTE WS-CW-10 =                                        ELKSPCFF
00206            ATBL-CF-SP-SRVC-GRP   (ATBL-IDX) * WS-CW-SP-SRVC-GRP   ELKSPCFF
00207         COMPUTE WS-CW-11 =                                        ELKSPCFF
00208            ATBL-CF-SP-VL-QLFR    (ATBL-IDX) * WS-CW-SP-VL-QLFR    ELKSPCFF
00209         COMPUTE WS-CW-12 =                                        ELKSPCFF
00210            ATBL-CF-SP-PRVDR-SPC  (ATBL-IDX) * WS-CW-SP-PRVDR-SPC  ELKSPCFF
00211                                                                   ELKSPCFF
00212         CALL 'ELKFLCMB'                                           ELKSPCFF
00213            USING WS-CW-SP                                         ELKSPCFF
00214                 WS-CW-1                                           ELKSPCFF
00215                 WS-CW-2                                           ELKSPCFF
00216                 WS-CW-3                                           ELKSPCFF
00217                 WS-CW-4                                           ELKSPCFF
00218                 WS-CW-5                                           ELKSPCFF
00219                 WS-CW-6                                           ELKSPCFF
00220                 WS-CW-7                                           ELKSPCFF
00221                 WS-CW-8                                           ELKSPCFF
00222                 WS-CW-9                                           ELKSPCFF
00223                 WS-CW-10                                          ELKSPCFF
00224                 WS-CW-11                                          ELKSPCFF
00225                 WS-CW-12                                          ELKSPCFF
00226         MOVE WS-CW-SP TO ATBL-CF-SP (ATBL-IDX)                    ELKSPCFF
00227         END-IF                                                    ELKSPCFF
00228      END-IF.                                                      ELKSPCFF
00229                                                                   ELKSPCFF
00230      SET WS-SUCCESSFULL-CALL TO TRUE.                             ELKSPCFF
