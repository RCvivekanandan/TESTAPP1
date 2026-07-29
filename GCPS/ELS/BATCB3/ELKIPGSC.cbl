00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGSC
00003  PROGRAM-ID.              ELKIPGSC                                   LV004
00004                                                                   ELKIPGSC
00005  AUTHOR.                  ANNE KEFFER KING.                       ELKIPGSC
00006                                                                   ELKIPGSC
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGSC
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGSC
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGSC
00010                        233 N. MICHIGAN AVE                        ELKIPGSC
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGSC
00012                                                                   ELKIPGSC
00013                                                                   ELKIPGSC
00014  DATE-WRITTEN.         25-AUG-2000.                               ELKIPGSC
00015                                                                   ELKIPGSC
00016  ENVIRONMENT DIVISION.                                            ELKIPGSC
00017  CONFIGURATION SECTION.                                           ELKIPGSC
00018  SOURCE-COMPUTER. IBM-3090.                                       ELKIPGSC
00019  OBJECT-COMPUTER. IBM-3090.                                       ELKIPGSC
00020                                                                   ELKIPGSC
00021 ****************************************************************  ELKIPGSC
00022 *AKK 12/06/05 REGEN FOR TEST                                   *  ELKIPGSC
00023 *                                                              *  ELKIPGSC
00024 *  ELKIPGSC :  THIS PROGRAM COMPUTES THE OVERALL CONFIDENCE    *  ELKIPGSC
00025 *              FACTORS FOR A SINGLE #IPGS TABULAR RECORD.      *  ELKIPGSC
00026 *                                                              *  ELKIPGSC
00027 *              THE CONFIDENCE FACTOR VALUES COMPUTED ARE FOR:  *  ELKIPGSC
00028 *              INSTITUTIONAL, PROFESSIONAL                     *  ELKIPGSC
00029 *              PLAN, NON-PLAN AND OVERALL.                     *  ELKIPGSC
00030 *                                                              *  ELKIPGSC
00031 *              VALUES ARE COMPUTED BASED UPON INFORMATION IN A *  ELKIPGSC
00032 *              MASTER PROVIDER SPEC LIST WHICH CONTAINS        *  ELKIPGSC
00033 *              INFORMATION NECESSARY TO DERIVE THE ABOVE       *  ELKIPGSC
00034 *              CONFIDENCE FACTORS.                             *  ELKIPGSC
00035 *                                                              *  ELKIPGSC
00036 ****************************************************************  ELKIPGSC
00037 *                      MAINTENANCE HISTORY                     *  ELKIPGSC
00038 *                                                              *  ELKIPGSC
00039 *  MOD     DATE      BY  DRPT              ACTION              *  ELKIPGSC
00040 * ----- ----------- --- ----- ---------------------------------*  ELKIPGSC
00041 * 01.00 25-AUG-2000 AKK       CLONED  FROM ELKIPGTC            *  ELKIPGSC
00042 *                                                                *ELKIPGSC
00043 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGSC
00044 *                                                                *ELKIPGSC
00045 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGSC
00046 *                                                                *ELKIPGSC
00047 * 02.01 09-JAN-2004 AKK S0C7                                     *ELKIPGSC
00048 *                                                                *ELKIPGSC
00049 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGSC
00050 *                             CICSCB3 COMPILER FIX             *  ELKIPGSC
00051 ****************************************************************  ELKIPGSC
00052                                                                   ELKIPGSC
00053  DATA DIVISION.                                                   ELKIPGSC
00054                                                                   ELKIPGSC
00055  WORKING-STORAGE SECTION.                                         ELKIPGSC
00056                                                                   ELKIPGSC
00057  01  WS-BEGIN                       PIC X(32)   VALUE             ELKIPGSC
00058      '**WORKING STORAGE FOR ELKIPGSC**'.                          ELKIPGSC
00059                                                                   ELKIPGSC
00060  01  WS-CONFIDENCE-FACTORS.                                       ELKIPGSC
00061      05  WS-CONF-INST            COMP-1 VALUE ZERO.               ELKIPGSC
00062      05  WS-CONF-PROF            COMP-1 VALUE ZERO.               ELKIPGSC
00063      05  WS-CONF-PLAN            COMP-1 VALUE ZERO.               ELKIPGSC
00064      05  WS-CONF-NON-PLAN        COMP-1 VALUE ZERO.               ELKIPGSC
00065      05  WS-CONF-OV              COMP-1 VALUE ZERO.               ELKIPGSC
00066                                                                   ELKIPGSC
00067  01  WS-RETURN-CODE                 PIC S9(04) COMP  VALUE ZERO.  ELKIPGSC
00068      88  WS-SUCCESSFUL-CALL              VALUE ZERO.              ELKIPGSC
00069      88  WS-INVALID-PARM                 VALUE +8.                ELKIPGSC
00070      88  WS-MISSING-PARM                 VALUE +12.               ELKIPGSC
00071      88  WS-INTERNAL-ERROR               VALUE +16.               ELKIPGSC
00072                                                                   ELKIPGSC
00073  01  WS-GXS-MAX-INDEX             INDEX.                          ELKIPGSC
00074                                                                   ELKIPGSC
00075      COPY ELSCFTB9.                                               ELKIPGSC
00076                                                                   ELKIPGSC
00077  01  WS-END                         PIC X(32)   VALUE             ELKIPGSC
00078      '**END WORKING STORAGE-ELKIPGTC**'.                          ELKIPGSC
00079 /                                                                 ELKIPGSC
00080  LINKAGE SECTION.                                                 ELKIPGSC
00081                                                                   ELKIPGSC
00082  01  IPGS-RECORD.                                                 ELKIPGSC
00083      COPY GCTIPGSC.                                               ELKIPGSC
00084                                                                   ELKIPGSC
00085                                                                   ELKIPGSC
00086      COPY ELSCFDBC.                                               ELKIPGSC
00087                                                                   ELKIPGSC
00088 /***********************************************************      ELKIPGSC
00089 *                                                          *      ELKIPGSC
00090 *    CREATE IPGS CONFIDENCE FACTORS TABLE                  *      ELKIPGSC
00091 *                                                          *      ELKIPGSC
00092 ************************************************************      ELKIPGSC
00093                                                                   ELKIPGSC
00094  PROCEDURE DIVISION USING IPGS-RECORD                             ELKIPGSC
00095                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIPGSC
00096                                                                   ELKIPGSC
00097  0000-COMPUTE-OVERALL-CONF-FACT.                                  ELKIPGSC
00098      IF ADDRESS OF IPGS-RECORD = NULL OR                          ELKIPGSC
00099         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIPGSC
00100         SET WS-MISSING-PARM TO TRUE                               ELKIPGSC
00101      ELSE                                                         ELKIPGSC
00102         PERFORM 0100-INITIALIZE                                   ELKIPGSC
00103         PERFORM 1000-PROCESS-IPGS-TABULAR.                        ELKIPGSC
00104      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGSC
00105      GOBACK.                                                      ELKIPGSC
00106                                                                   ELKIPGSC
00107 ************************************************************      ELKIPGSC
00108 *                                                          *      ELKIPGSC
00109 *        INITIALIZE                                        *      ELKIPGSC
00110 *                                                          *      ELKIPGSC
00111 ************************************************************      ELKIPGSC
00112                                                                   ELKIPGSC
00113  0100-INITIALIZE.                                                 ELKIPGSC
00114                                                                   ELKIPGSC
00115      INITIALIZE WS-CONFIDENCE-FACTORS.                            ELKIPGSC
00116      MOVE ZEROS TO WS-RETURN-CODE.                                ELKIPGSC
00117                                                                   ELKIPGSC
00118 ************************************************************      ELKIPGSC
00119 *                                                          *      ELKIPGSC
00120 *    PROCESS IPGS TABULAR                                  *      ELKIPGSC
00121 *                                                          *      ELKIPGSC
00122 ************************************************************      ELKIPGSC
00123                                                                   ELKIPGSC
00124  1000-PROCESS-IPGS-TABULAR.                                       ELKIPGSC
00125                                                                   ELKIPGSC
00126      SET CFT9-MAX-IDX TO CFT9-NBR-TBL-ENTRIES.                    ELKIPGSC
00127      SET GXS-INDEX TO GXS-ENTRY-COUNT.                            ELKIPGSC
00128      SET WS-GXS-MAX-INDEX TO GXS-INDEX.                           ELKIPGSC
00129      SET CFT9-IDX TO 1.                                           ELKIPGSC
00130      SET GXS-INDEX TO 1.                                          ELKIPGSC
00131      PERFORM 1100-MATCH-IPGS-TAB-TO-MPROV                         ELKIPGSC
00132           UNTIL GXS-INDEX > WS-GXS-MAX-INDEX OR                   ELKIPGSC
00133                 CFT9-IDX > CFT9-MAX-IDX.                          ELKIPGSC
00134      PERFORM 2000-COMPUTE-IPGS-CONF-FACTORS.                      ELKIPGSC
00135                                                                   ELKIPGSC
00136 ************************************************************      ELKIPGSC
00137 *                                                          *      ELKIPGSC
00138 *    MATCH IPGS TABULAR TO MASTER PROVIDER LIST            *      ELKIPGSC
00139 *                                                          *      ELKIPGSC
00140 ************************************************************      ELKIPGSC
00141                                                                   ELKIPGSC
00142  1100-MATCH-IPGS-TAB-TO-MPROV.                                    ELKIPGSC
00143                                                                   ELKIPGSC
00144      IF GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX) =                  ELKIPGSC
00145             CFT9-PT (CFT9-IDX)                                    ELKIPGSC
00146         PERFORM 1200-UPDATE-CONFIDENCE-FACTORS                    ELKIPGSC
00147      ELSE                                                         ELKIPGSC
00148         IF GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX) >               ELKIPGSC
00149               CFT9-PT (CFT9-IDX)                                  ELKIPGSC
00150             SET CFT9-IDX UP BY 1                                  ELKIPGSC
00151         ELSE                                                      ELKIPGSC
00152             SET GXS-INDEX UP BY 1.                                ELKIPGSC
00153                                                                   ELKIPGSC
00154                                                                   ELKIPGSC
00155 ************************************************************      ELKIPGSC
00156 *                                                          *      ELKIPGSC
00157 *    UPDATE CONFIDENCE FACTORS ACCORDING TO PROV CATEGORIES*      ELKIPGSC
00158 *                                                          *      ELKIPGSC
00159 ************************************************************      ELKIPGSC
00160                                                                   ELKIPGSC
00161  1200-UPDATE-CONFIDENCE-FACTORS.                                  ELKIPGSC
00162                                                                   ELKIPGSC
00163      ADD CFT9-CF-PT-PLAN (CFT9-IDX) TO WS-CONF-PLAN.              ELKIPGSC
00164      ADD CFT9-CF-PT-NONPLAN (CFT9-IDX) TO WS-CONF-NON-PLAN.       ELKIPGSC
00165      ADD CFT9-CF-PT-PROF (CFT9-IDX) TO WS-CONF-PROF.              ELKIPGSC
00166      ADD CFT9-CF-PT-OV (CFT9-IDX) TO WS-CONF-OV.                  ELKIPGSC
00167      SET CFT9-IDX UP BY 1.                                        ELKIPGSC
00168      SET GXS-INDEX UP BY 1.                                       ELKIPGSC
00169                                                                   ELKIPGSC
00170 ************************************************************      ELKIPGSC
00171 *                                                                 ELKIPGSC
00172 *    COMPUTE IPGS CONFIDENCE FACTORS                              ELKIPGSC
00173 *                                                                 ELKIPGSC
00174 ************************************************************      ELKIPGSC
00175                                                                   ELKIPGSC
00176  2000-COMPUTE-IPGS-CONF-FACTORS.                                  ELKIPGSC
00177                                                                   ELKIPGSC
00178      IF GXS-ID-ARGUMENT-INCLUDED                                  ELKIPGSC
00179         PERFORM 2100-COMPUTE-INCLUDED-FACTORS                     ELKIPGSC
00180      ELSE                                                         ELKIPGSC
00181         IF GXS-ID-ARGUMENT-EXCLUDED                               ELKIPGSC
00182            PERFORM 2200-COMPUTE-EXCLUDED-FACTORS                  ELKIPGSC
00183         ELSE                                                      ELKIPGSC
00184            SET WS-MISSING-PARM TO TRUE.                           ELKIPGSC
00185                                                                   ELKIPGSC
00186 ************************************************************      ELKIPGSC
00187 *                                                                 ELKIPGSC
00188 *    COMPUTE INCLUDED CONFIDENCE FACTORS                          ELKIPGSC
00189 *                                                                 ELKIPGSC
00190 ************************************************************      ELKIPGSC
00191                                                                   ELKIPGSC
00192  2100-COMPUTE-INCLUDED-FACTORS.                                   ELKIPGSC
00193                                                                   ELKIPGSC
00194      COMPUTE CFDB-CF-IPGS-PRFSNL =                                ELKIPGSC
00195         ((WS-CONF-PROF) * 2 ) - 1.                                ELKIPGSC
00196                                                                   ELKIPGSC
00197      COMPUTE CFDB-CF-IPGS-PLAN =                                  ELKIPGSC
00198         ((WS-CONF-PLAN) * 2 ) - 1.                                ELKIPGSC
00199                                                                   ELKIPGSC
00200      COMPUTE CFDB-CF-IPGS-NON-PLAN =                              ELKIPGSC
00201         ((WS-CONF-NON-PLAN) * 2 ) - 1.                            ELKIPGSC
00202                                                                   ELKIPGSC
00203      COMPUTE CFDB-CF-IPGS-OV =                                    ELKIPGSC
00204         ((WS-CONF-OV) * 2 ) - 1.                                  ELKIPGSC
00205                                                                   ELKIPGSC
00206 ************************************************************      ELKIPGSC
00207 *                                                                 ELKIPGSC
00208 *    COMPUTE EXCLUDED CONFIDENCE FACTORS                          ELKIPGSC
00209 *                                                                 ELKIPGSC
00210 ************************************************************      ELKIPGSC
00211                                                                   ELKIPGSC
00212  2200-COMPUTE-EXCLUDED-FACTORS.                                   ELKIPGSC
00213                                                                   ELKIPGSC
00214      COMPUTE CFDB-CF-IPGS-PRFSNL =                                ELKIPGSC
00215         ((1 - WS-CONF-PROF) * 2 ) - 1.                            ELKIPGSC
00216                                                                   ELKIPGSC
00217      COMPUTE CFDB-CF-IPGS-PLAN =                                  ELKIPGSC
00218         ((1 - WS-CONF-PLAN) * 2 ) - 1.                            ELKIPGSC
00219                                                                   ELKIPGSC
00220      COMPUTE CFDB-CF-IPGS-NON-PLAN =                              ELKIPGSC
00221         ((1 - WS-CONF-NON-PLAN) * 2 ) - 1.                        ELKIPGSC
00222                                                                   ELKIPGSC
00223      COMPUTE CFDB-CF-IPGS-OV =                                    ELKIPGSC
00224         ((1 - WS-CONF-OV) * 2 ) - 1.                              ELKIPGSC
00225                                                                   ELKIPGSC
