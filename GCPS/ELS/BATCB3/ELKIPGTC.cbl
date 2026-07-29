00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGTC
00003  PROGRAM-ID.              ELKIPGTC                                   LV004
00004                                                                   ELKIPGTC
00005  AUTHOR.                  BARBARA KEIB                            ELKIPGTC
00006                                                                   ELKIPGTC
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGTC
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGTC
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGTC
00010                        233 N. MICHIGAN AVE                        ELKIPGTC
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGTC
00012                                                                   ELKIPGTC
00013                                                                   ELKIPGTC
00014  DATE-WRITTEN.         09-OCT-1992.                               ELKIPGTC
00015                                                                   ELKIPGTC
00016  ENVIRONMENT DIVISION.                                            ELKIPGTC
00017  CONFIGURATION SECTION.                                           ELKIPGTC
00018  SOURCE-COMPUTER. IBM-3090.                                       ELKIPGTC
00019  OBJECT-COMPUTER. IBM-3090.                                       ELKIPGTC
00020                                                                   ELKIPGTC
00021 ****************************************************************  ELKIPGTC
00022 *AKK 12/06/05 REGEN FOR TEST                                   *  ELKIPGTC
00023 *  ELKIPGTC :  THIS PROGRAM COMPUTES THE OVERALL CONFIDENCE    *  ELKIPGTC
00024 *              FACTORS FOR A SINGLE #IPGT TABULAR RECORD.      *  ELKIPGTC
00025 *                                                              *  ELKIPGTC
00026 *              THE CONFIDENCE FACTOR VALUES COMPUTED ARE FOR:  *  ELKIPGTC
00027 *              INSTITUTIONAL, PROFESSIONAL                     *  ELKIPGTC
00028 *              PLAN, NON-PLAN AND OVERALL.                     *  ELKIPGTC
00029 *                                                              *  ELKIPGTC
00030 *              VALUES ARE COMPUTED BASED UPON INFORMATION IN A *  ELKIPGTC
00031 *              MASTER PROVIDER TYPE LIST WHICH CONTAINS        *  ELKIPGTC
00032 *              INFORMATION NECESSARY TO DERIVE THE ABOVE       *  ELKIPGTC
00033 *              CONFIDENCE FACTORS.                             *  ELKIPGTC
00034 *                                                              *  ELKIPGTC
00035 ****************************************************************  ELKIPGTC
00036 *                      MAINTENANCE HISTORY                     *  ELKIPGTC
00037 *                                                              *  ELKIPGTC
00038 *                                                              *  ELKIPGTC
00039 *  MOD     DATE      BY  DRPT              ACTION              *  ELKIPGTC
00040 * ----- ----------- --- ----- ---------------------------------*  ELKIPGTC
00041 * 01.00 09-OCT-1992 BAK       CREATED                          *  ELKIPGTC
00042 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGTC
00043 *                                                                *ELKIPGTC
00044 * 01.01 09-JAN-2004 AKK S0C7 TEST                                *ELKIPGTC
00045 *                                                                *ELKIPGTC
00046 * 01.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGTC
00047 *                             CICSCB3 COMPILER FIX             *  ELKIPGTC
00048 ****************************************************************  ELKIPGTC
00049                                                                   ELKIPGTC
00050  DATA DIVISION.                                                   ELKIPGTC
00051                                                                   ELKIPGTC
00052  WORKING-STORAGE SECTION.                                         ELKIPGTC
00053                                                                   ELKIPGTC
00054  01  WS-BEGIN                       PIC X(32)   VALUE             ELKIPGTC
00055      '**WORKING STORAGE FOR ELKIPGTC**'.                          ELKIPGTC
00056                                                                   ELKIPGTC
00057  01  WS-CONFIDENCE-FACTORS.                                       ELKIPGTC
00058      05  WS-CONF-INST            COMP-1 VALUE ZERO.               ELKIPGTC
00059      05  WS-CONF-PROF            COMP-1 VALUE ZERO.               ELKIPGTC
00060      05  WS-CONF-PLAN            COMP-1 VALUE ZERO.               ELKIPGTC
00061      05  WS-CONF-NON-PLAN        COMP-1 VALUE ZERO.               ELKIPGTC
00062      05  WS-CONF-OV              COMP-1 VALUE ZERO.               ELKIPGTC
00063                                                                   ELKIPGTC
00064  01  WS-RETURN-CODE                 PIC S9(04) COMP  VALUE ZERO.  ELKIPGTC
00065      88  WS-SUCCESSFUL-CALL              VALUE ZERO.              ELKIPGTC
00066      88  WS-INVALID-PARM                 VALUE +8.                ELKIPGTC
00067      88  WS-MISSING-PARM                 VALUE +12.               ELKIPGTC
00068      88  WS-INTERNAL-ERROR               VALUE +16.               ELKIPGTC
00069                                                                   ELKIPGTC
00070  01  WS-GX3-MAX-INDEX             INDEX.                          ELKIPGTC
00071                                                                   ELKIPGTC
00072      COPY ELSCFTB2.                                               ELKIPGTC
00073                                                                   ELKIPGTC
00074  01  WS-END                         PIC X(32)   VALUE             ELKIPGTC
00075      '**END WORKING STORAGE-ELKIPGTC**'.                          ELKIPGTC
00076 /                                                                 ELKIPGTC
00077  LINKAGE SECTION.                                                 ELKIPGTC
00078                                                                   ELKIPGTC
00079  01  IPGT-RECORD.                                                 ELKIPGTC
00080      COPY GCTIPGTC.                                               ELKIPGTC
00081                                                                   ELKIPGTC
00082                                                                   ELKIPGTC
00083      COPY ELSCFDBC.                                               ELKIPGTC
00084                                                                   ELKIPGTC
00085 /***********************************************************      ELKIPGTC
00086 *                                                          *      ELKIPGTC
00087 *    CREATE IPGT CONFIDENCE FACTORS TABLE                  *      ELKIPGTC
00088 *                                                          *      ELKIPGTC
00089 ************************************************************      ELKIPGTC
00090                                                                   ELKIPGTC
00091  PROCEDURE DIVISION USING IPGT-RECORD                             ELKIPGTC
00092                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIPGTC
00093                                                                   ELKIPGTC
00094  0000-COMPUTE-OVERALL-CONF-FACT.                                  ELKIPGTC
00095      IF ADDRESS OF IPGT-RECORD = NULL OR                          ELKIPGTC
00096         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIPGTC
00097         SET WS-MISSING-PARM TO TRUE                               ELKIPGTC
00098      ELSE                                                         ELKIPGTC
00099         PERFORM 0100-INITIALIZE                                   ELKIPGTC
00100         PERFORM 1000-PROCESS-IPGT-TABULAR.                        ELKIPGTC
00101      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGTC
00102      GOBACK.                                                      ELKIPGTC
00103                                                                   ELKIPGTC
00104 ************************************************************      ELKIPGTC
00105 *                                                          *      ELKIPGTC
00106 *        INITIALIZE                                        *      ELKIPGTC
00107 *                                                          *      ELKIPGTC
00108 ************************************************************      ELKIPGTC
00109                                                                   ELKIPGTC
00110  0100-INITIALIZE.                                                 ELKIPGTC
00111                                                                   ELKIPGTC
00112      INITIALIZE WS-CONFIDENCE-FACTORS.                            ELKIPGTC
00113      MOVE ZEROS TO WS-RETURN-CODE.                                ELKIPGTC
00114                                                                   ELKIPGTC
00115 ************************************************************      ELKIPGTC
00116 *                                                          *      ELKIPGTC
00117 *    PROCESS IPGT TABULAR                                  *      ELKIPGTC
00118 *                                                          *      ELKIPGTC
00119 ************************************************************      ELKIPGTC
00120                                                                   ELKIPGTC
00121  1000-PROCESS-IPGT-TABULAR.                                       ELKIPGTC
00122                                                                   ELKIPGTC
00123      SET CFT2-MAX-IDX TO CFT2-NBR-TBL-ENTRIES.                    ELKIPGTC
00124      SET GX3-INDEX TO GX3-ENTRY-COUNT.                            ELKIPGTC
00125      SET WS-GX3-MAX-INDEX TO GX3-INDEX.                           ELKIPGTC
00126      SET CFT2-IDX TO 1.                                           ELKIPGTC
00127      SET GX3-INDEX TO 1.                                          ELKIPGTC
00128      PERFORM 1100-MATCH-IPGT-TAB-TO-MPROV                         ELKIPGTC
00129           UNTIL GX3-INDEX > WS-GX3-MAX-INDEX OR                   ELKIPGTC
00130                 CFT2-IDX > CFT2-MAX-IDX.                          ELKIPGTC
00131      PERFORM 2000-COMPUTE-IPGT-CONF-FACTORS.                      ELKIPGTC
00132                                                                   ELKIPGTC
00133 ************************************************************      ELKIPGTC
00134 *                                                          *      ELKIPGTC
00135 *    MATCH IPGT TABULAR TO MASTER PROVIDER LIST            *      ELKIPGTC
00136 *                                                          *      ELKIPGTC
00137 ************************************************************      ELKIPGTC
00138                                                                   ELKIPGTC
00139  1100-MATCH-IPGT-TAB-TO-MPROV.                                    ELKIPGTC
00140                                                                   ELKIPGTC
00141      IF GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX) =                  ELKIPGTC
00142             CFT2-PT (CFT2-IDX)                                    ELKIPGTC
00143         PERFORM 1200-UPDATE-CONFIDENCE-FACTORS                    ELKIPGTC
00144      ELSE                                                         ELKIPGTC
00145         IF GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX) >               ELKIPGTC
00146               CFT2-PT (CFT2-IDX)                                  ELKIPGTC
00147             SET CFT2-IDX UP BY 1                                  ELKIPGTC
00148         ELSE                                                      ELKIPGTC
00149             SET GX3-INDEX UP BY 1.                                ELKIPGTC
00150                                                                   ELKIPGTC
00151                                                                   ELKIPGTC
00152 ************************************************************      ELKIPGTC
00153 *                                                          *      ELKIPGTC
00154 *    UPDATE CONFIDENCE FACTORS ACCORDING TO PROV CATEGORIES*      ELKIPGTC
00155 *                                                          *      ELKIPGTC
00156 ************************************************************      ELKIPGTC
00157                                                                   ELKIPGTC
00158  1200-UPDATE-CONFIDENCE-FACTORS.                                  ELKIPGTC
00159                                                                   ELKIPGTC
00160      ADD CFT2-CF-PT-INST (CFT2-IDX) TO WS-CONF-INST.              ELKIPGTC
00161      ADD CFT2-CF-PT-PLAN (CFT2-IDX) TO WS-CONF-PLAN.              ELKIPGTC
00162      ADD CFT2-CF-PT-NONPLAN (CFT2-IDX) TO WS-CONF-NON-PLAN.       ELKIPGTC
00163      ADD CFT2-CF-PT-PROF (CFT2-IDX) TO WS-CONF-PROF.              ELKIPGTC
00164      ADD CFT2-CF-PT-OV (CFT2-IDX) TO WS-CONF-OV.                  ELKIPGTC
00165      SET CFT2-IDX UP BY 1.                                        ELKIPGTC
00166      SET GX3-INDEX UP BY 1.                                       ELKIPGTC
00167                                                                   ELKIPGTC
00168 ************************************************************      ELKIPGTC
00169 *                                                                 ELKIPGTC
00170 *    COMPUTE IPGT CONFIDENCE FACTORS                              ELKIPGTC
00171 *                                                                 ELKIPGTC
00172 ************************************************************      ELKIPGTC
00173                                                                   ELKIPGTC
00174  2000-COMPUTE-IPGT-CONF-FACTORS.                                  ELKIPGTC
00175                                                                   ELKIPGTC
00176      IF GX3-ID-ARGUMENT-INCLUDED                                  ELKIPGTC
00177         PERFORM 2100-COMPUTE-INCLUDED-FACTORS                     ELKIPGTC
00178      ELSE                                                         ELKIPGTC
00179         IF GX3-ID-ARGUMENT-EXCLUDED                               ELKIPGTC
00180            PERFORM 2200-COMPUTE-EXCLUDED-FACTORS                  ELKIPGTC
00181         ELSE                                                      ELKIPGTC
00182            SET WS-MISSING-PARM TO TRUE.                           ELKIPGTC
00183                                                                   ELKIPGTC
00184 ************************************************************      ELKIPGTC
00185 *                                                                 ELKIPGTC
00186 *    COMPUTE INCLUDED CONFIDENCE FACTORS                          ELKIPGTC
00187 *                                                                 ELKIPGTC
00188 ************************************************************      ELKIPGTC
00189                                                                   ELKIPGTC
00190  2100-COMPUTE-INCLUDED-FACTORS.                                   ELKIPGTC
00191                                                                   ELKIPGTC
00192      COMPUTE CFDB-CF-IPGT-INSTTNL =                               ELKIPGTC
00193         ((WS-CONF-INST) * 2 ) - 1.                                ELKIPGTC
00194                                                                   ELKIPGTC
00195      COMPUTE CFDB-CF-IPGT-PRFSNL =                                ELKIPGTC
00196         ((WS-CONF-PROF) * 2 ) - 1.                                ELKIPGTC
00197                                                                   ELKIPGTC
00198      COMPUTE CFDB-CF-IPGT-PLAN =                                  ELKIPGTC
00199         ((WS-CONF-PLAN) * 2 ) - 1.                                ELKIPGTC
00200                                                                   ELKIPGTC
00201      COMPUTE CFDB-CF-IPGT-NON-PLAN =                              ELKIPGTC
00202         ((WS-CONF-NON-PLAN) * 2 ) - 1.                            ELKIPGTC
00203                                                                   ELKIPGTC
00204      COMPUTE CFDB-CF-IPGT-OV =                                    ELKIPGTC
00205         ((WS-CONF-OV) * 2 ) - 1.                                  ELKIPGTC
00206                                                                   ELKIPGTC
00207 ************************************************************      ELKIPGTC
00208 *                                                                 ELKIPGTC
00209 *    COMPUTE EXCLUDED CONFIDENCE FACTORS                          ELKIPGTC
00210 *                                                                 ELKIPGTC
00211 ************************************************************      ELKIPGTC
00212                                                                   ELKIPGTC
00213  2200-COMPUTE-EXCLUDED-FACTORS.                                   ELKIPGTC
00214                                                                   ELKIPGTC
00215      COMPUTE CFDB-CF-IPGT-INSTTNL =                               ELKIPGTC
00216         ((1 - WS-CONF-INST) * 2 ) - 1.                            ELKIPGTC
00217                                                                   ELKIPGTC
00218      COMPUTE CFDB-CF-IPGT-PRFSNL =                                ELKIPGTC
00219         ((1 - WS-CONF-PROF) * 2 ) - 1.                            ELKIPGTC
00220                                                                   ELKIPGTC
00221      COMPUTE CFDB-CF-IPGT-PLAN =                                  ELKIPGTC
00222         ((1 - WS-CONF-PLAN) * 2 ) - 1.                            ELKIPGTC
00223                                                                   ELKIPGTC
00224      COMPUTE CFDB-CF-IPGT-NON-PLAN =                              ELKIPGTC
00225         ((1 - WS-CONF-NON-PLAN) * 2 ) - 1.                        ELKIPGTC
00226                                                                   ELKIPGTC
00227      COMPUTE CFDB-CF-IPGT-OV =                                    ELKIPGTC
00228         ((1 - WS-CONF-OV) * 2 ) - 1.                              ELKIPGTC
00229                                                                   ELKIPGTC
