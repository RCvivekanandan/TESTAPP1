00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIBGRC
00003  PROGRAM-ID.              ELKIBGRC                                   LV004
00004                                                                   ELKIBGRC
00005  AUTHOR.                  BARBARA KEIB                            ELKIBGRC
00006                                                                   ELKIBGRC
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIBGRC
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIBGRC
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIBGRC
00010                        233 N. MICHIGAN AVE                        ELKIBGRC
00011                        CHICAGO, ILLINOIS 60601                    ELKIBGRC
00012                                                                   ELKIBGRC
00013  DATE-WRITTEN.         29-SEP-1992.                               ELKIBGRC
00014                                                                   ELKIBGRC
00015  ENVIRONMENT DIVISION.                                            ELKIBGRC
00016  CONFIGURATION SECTION.                                           ELKIBGRC
00017  SOURCE-COMPUTER. IBM-3090.                                       ELKIBGRC
00018  OBJECT-COMPUTER. IBM-3090.                                       ELKIBGRC
00019                                                                   ELKIBGRC
00020 ****************************************************************  ELKIBGRC
00021 *AKK 12/06/05 REGEN FOR TEST                                   *  ELKIBGRC
00022 *  ELKIBGRC :  THIS PROGRAM COMPUTES THE OVERALL CONFIDENCE    *  ELKIBGRC
00023 *              FACTORS FOR A SINGLE #IBGR TABULAR RECORD.      *  ELKIBGRC
00024 *                                                              *  ELKIBGRC
00025 *              THE CONFIDENCE FACTOR VALUES COMPUTED ARE FOR:  *  ELKIBGRC
00026 *              INSTITUTIONAL, PROFESSIONAL                     *  ELKIBGRC
00027 *              INPATIENT ONLY, INPATIENT OR BOTH               *  ELKIBGRC
00028 *              OUTPATIENT ONLY, OUTPATIENT OR BOTH, OVERALL    *  ELKIBGRC
00029 *                                                              *  ELKIBGRC
00030 ****************************************************************  ELKIBGRC
00031 *                      MAINTENANCE HISTORY                     *  ELKIBGRC
00032 *                                                              *  ELKIBGRC
00033 *  MOD     DATE      BY  DRPT              ACTION              *  ELKIBGRC
00034 * ----- ----------- --- ----- ---------------------------------*  ELKIBGRC
00035 * 01.00 29-SEP-1992 BAK       CREATED                          *  ELKIBGRC
00036 * 01.00 23-DEC-1992 BAK       CORRECTED IP/OP COUNTING         *  ELKIBGRC
00037 * 01.01 13-MAR-2000 AKK       ADDED DISPLAY FIELDS TO TEST     *  ELKIBGRC
00038 *                                                                *ELKIBGRC
00039 * 01.06 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX         *ELKIBGRC
00040 *                             ASM RECOMPILES                     *ELKIBGRC
00041 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIBGRC
00042 *                                                                *ELKIBGRC
00043 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELKIBGRC
00044 *                                                                *ELKIBGRC
00045 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIBGRC
00046 *                             CICSCB3 COMPILER FIX             *  ELKIBGRC
00047 ****************************************************************  ELKIBGRC
00048                                                                   ELKIBGRC
00049  DATA DIVISION.                                                   ELKIBGRC
00050                                                                   ELKIBGRC
00051  WORKING-STORAGE SECTION.                                         ELKIBGRC
00052                                                                   ELKIBGRC
00053  01  WS-BEGIN                       PIC X(32)   VALUE             ELKIBGRC
00054      '**WORKING STORAGE FOR ELKIBGRC**'.                          ELKIBGRC
00055                                                                   ELKIBGRC
00056  01  WS-TEST-FIELDS.                                              ELKIBGRC
00057      05  WS-DISPLAY-OP-COUNT     PIC S9V9(10) VALUE ZERO.         ELKIBGRC
00058      05  WS-CF-IBGR-INSTTNL      PIC S9V9(10) VALUE ZERO.         ELKIBGRC
00059      05  WS-CF-IBGR-PRFSNL       PIC S9V9(10) VALUE ZERO.         ELKIBGRC
00060      05  WS-CF-IBGR-IP-ONLY      PIC S9V9(10) VALUE ZERO.         ELKIBGRC
00061      05  WS-CF-IBGR-IP-BOTH      PIC S9V9(10) VALUE ZERO.         ELKIBGRC
00062      05  WS-CF-IBGR-OP-BOTH      PIC S9V9(10) VALUE ZERO.         ELKIBGRC
00063      05  WS-CF-IBGR-OP-ONLY      PIC S9V9(10) VALUE ZERO.         ELKIBGRC
00064      05  WS-CF-IBGR-OV           PIC S9V9(10) VALUE ZERO.         ELKIBGRC
00065                                                                   ELKIBGRC
00066  01  WS-COUNTS.                                                   ELKIBGRC
00067      05  WS-COUNT-INST           PIC S9(04)  COMP  VALUE ZERO.    ELKIBGRC
00068      05  WS-COUNT-PROF           PIC S9(04)  COMP  VALUE ZERO.    ELKIBGRC
00069      05  WS-COUNT-IP             PIC S9(04)  COMP  VALUE ZERO.    ELKIBGRC
00070      05  WS-COUNT-IP-BOTH        PIC S9(04)  COMP  VALUE ZERO.    ELKIBGRC
00071      05  WS-COUNT-OP             PIC S9(04)  COMP  VALUE ZERO.    ELKIBGRC
00072      05  WS-COUNT-OP-BOTH        PIC S9(04)  COMP  VALUE ZERO.    ELKIBGRC
00073      05  WS-COUNT-OV             PIC S9(04)  COMP  VALUE ZERO.    ELKIBGRC
00074      05  WS-COUNT-LIST-MTCH      PIC S9(04)  COMP  VALUE ZERO.    ELKIBGRC
00075                                                                   ELKIBGRC
00076  01  WS-CONFIDENCE-FACTORS.                                       ELKIBGRC
00077      05  WS-INCL-INST            COMP-1 VALUE ZERO.               ELKIBGRC
00078      05  WS-INCL-PROF            COMP-1 VALUE ZERO.               ELKIBGRC
00079      05  WS-INCL-IP              COMP-1 VALUE ZERO.               ELKIBGRC
00080      05  WS-INCL-IP-BOTH         COMP-1 VALUE ZERO.               ELKIBGRC
00081      05  WS-INCL-OP              COMP-1 VALUE ZERO.               ELKIBGRC
00082      05  WS-INCL-OP-BOTH         COMP-1 VALUE ZERO.               ELKIBGRC
00083      05  WS-INCL-OV              COMP-1 VALUE ZERO.               ELKIBGRC
00084      05  WS-INCL-LIST-MTCH       COMP-1 VALUE ZERO.               ELKIBGRC
00085                                                                   ELKIBGRC
00086  01  WS-ELIGIBILITY-FACTORS.                                      ELKIBGRC
00087      05  WS-ELIG-INST            COMP-1 VALUE ZERO.               ELKIBGRC
00088      05  WS-ELIG-PROF            COMP-1 VALUE ZERO.               ELKIBGRC
00089      05  WS-ELIG-IP              COMP-1 VALUE ZERO.               ELKIBGRC
00090      05  WS-ELIG-IP-BOTH         COMP-1 VALUE ZERO.               ELKIBGRC
00091      05  WS-ELIG-OP              COMP-1 VALUE ZERO.               ELKIBGRC
00092      05  WS-ELIG-OP-BOTH         COMP-1 VALUE ZERO.               ELKIBGRC
00093      05  WS-ELIG-OV              COMP-1 VALUE ZERO.               ELKIBGRC
00094      05  WS-ELIG-LIST-MTCH       COMP-1 VALUE ZERO.               ELKIBGRC
00095                                                                   ELKIBGRC
00096  01  WS-RETURN-CODE                 PIC S9(04) COMP  VALUE ZERO.  ELKIBGRC
00097      88  WS-SUCCESSFUL-CALL              VALUE ZERO.              ELKIBGRC
00098      88  WS-INVALID-PARM                 VALUE +8.                ELKIBGRC
00099      88  WS-MISSING-PARM                 VALUE +12.               ELKIBGRC
00100      88  WS-INTERNAL-ERROR               VALUE +16.               ELKIBGRC
00101                                                                   ELKIBGRC
00102  01  WS-GX1-MAX-INDEX             INDEX.                          ELKIBGRC
00103                                                                   ELKIBGRC
00104      COPY ELSBPTBL.                                               ELKIBGRC
00105 /                                                                 ELKIBGRC
00106  01  WS-END                         PIC X(32)   VALUE             ELKIBGRC
00107      '**END WORKING STORAGE-ELKIBGRC**'.                          ELKIBGRC
00108 /                                                                 ELKIBGRC
00109  LINKAGE SECTION.                                                 ELKIBGRC
00110 /                                                                 ELKIBGRC
00111  01  IBGR-RECORD.                                                 ELKIBGRC
00112      COPY GCTIBGRC.                                               ELKIBGRC
00113                                                                   ELKIBGRC
00114                                                                   ELKIBGRC
00115      COPY ELSCFDBC.                                               ELKIBGRC
00116                                                                   ELKIBGRC
00117 /***********************************************************      ELKIBGRC
00118 *                                                          *      ELKIBGRC
00119 *    CREATE IBGR CONFIDENCE FACTORS TABLE                  *      ELKIBGRC
00120 *                                                          *      ELKIBGRC
00121 ************************************************************      ELKIBGRC
00122                                                                   ELKIBGRC
00123  PROCEDURE DIVISION USING IBGR-RECORD                             ELKIBGRC
00124                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIBGRC
00125                                                                   ELKIBGRC
00126  0000-COMPUTE-OVERALL-CONF-FACT.                                  ELKIBGRC
00127      IF ADDRESS OF IBGR-RECORD = NULL OR                          ELKIBGRC
00128         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIBGRC
00129         SET WS-MISSING-PARM TO TRUE                               ELKIBGRC
00130      ELSE                                                         ELKIBGRC
00131         PERFORM 0100-INITIALIZE                                   ELKIBGRC
00132         PERFORM 1000-PROCESS-IBGR-TABULAR.                        ELKIBGRC
00133      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIBGRC
00134      GOBACK.                                                      ELKIBGRC
00135                                                                   ELKIBGRC
00136 ************************************************************      ELKIBGRC
00137 *                                                          *      ELKIBGRC
00138 *        INITIALIZE                                        *      ELKIBGRC
00139 *                                                          *      ELKIBGRC
00140 ************************************************************      ELKIBGRC
00141                                                                   ELKIBGRC
00142  0100-INITIALIZE.                                                 ELKIBGRC
00143                                                                   ELKIBGRC
00144      INITIALIZE WS-COUNTS.                                        ELKIBGRC
00145      INITIALIZE WS-ELIGIBILITY-FACTORS.                           ELKIBGRC
00146      INITIALIZE WS-CONFIDENCE-FACTORS.                            ELKIBGRC
00147      MOVE ZEROS TO WS-RETURN-CODE.                                ELKIBGRC
00148                                                                   ELKIBGRC
00149 ************************************************************      ELKIBGRC
00150 *                                                          *      ELKIBGRC
00151 *    PROCESS IBGR TABULAR                                  *      ELKIBGRC
00152 *                                                          *      ELKIBGRC
00153 ************************************************************      ELKIBGRC
00154                                                                   ELKIBGRC
00155  1000-PROCESS-IBGR-TABULAR.                                       ELKIBGRC
00156                                                                   ELKIBGRC
00157      PERFORM 1200-COUNT-NBR-DEFINED-BPV.                          ELKIBGRC
00158      IF WS-SUCCESSFUL-CALL                                        ELKIBGRC
00159         PERFORM 2000-COMPUTE-IBGR-CONF-FACTORS.                   ELKIBGRC
00160                                                                   ELKIBGRC
00161 ************************************************************      ELKIBGRC
00162 *                                                          *      ELKIBGRC
00163 *    COUNT NUMBER OF DEFINED BENEFIT PROVISIONS IN IBGR TAB*      ELKIBGRC
00164 *                                                          *      ELKIBGRC
00165 ************************************************************      ELKIBGRC
00166                                                                   ELKIBGRC
00167  1200-COUNT-NBR-DEFINED-BPV.                                      ELKIBGRC
00168                                                                   ELKIBGRC
00169      PERFORM 1300-SAVE-ELIGIBILITY-COUNTS.                        ELKIBGRC
00170      SET BPL-MAX-IDX TO BPL-NBR-TBL-ENTRIES.                      ELKIBGRC
00171      SET GX1-INDEX TO GX1-ENTRY-COUNT.                            ELKIBGRC
00172      SET WS-GX1-MAX-INDEX TO GX1-INDEX.                           ELKIBGRC
00173      SET BPL-IDX TO 1.                                            ELKIBGRC
00174      SET GX1-INDEX TO 1.                                          ELKIBGRC
00175      PERFORM 1400-MATCH-IBGR-TAB-TO-MBPV                          ELKIBGRC
00176           UNTIL GX1-INDEX > WS-GX1-MAX-INDEX OR                   ELKIBGRC
00177              BPL-IDX > BPL-MAX-IDX OR                             ELKIBGRC
00178                WS-INTERNAL-ERROR.                                 ELKIBGRC
00179                                                                   ELKIBGRC
00180 ************************************************************      ELKIBGRC
00181 *                                                          *      ELKIBGRC
00182 *    SAVE ELIGIBILTY COUNTS FOR BENEFIT PROVISION LIST     *      ELKIBGRC
00183 *                                                          *      ELKIBGRC
00184 ************************************************************      ELKIBGRC
00185                                                                   ELKIBGRC
00186  1300-SAVE-ELIGIBILITY-COUNTS.                                    ELKIBGRC
00187                                                                   ELKIBGRC
00188      MOVE BPL-NBR-IP-ENTRIES TO WS-ELIG-IP.                       ELKIBGRC
00189      MOVE BPL-NBR-OP-ENTRIES TO WS-ELIG-OP.                       ELKIBGRC
00190      COMPUTE WS-ELIG-IP-BOTH =                                    ELKIBGRC
00191          BPL-NBR-BOTH-ENTRIES + WS-ELIG-IP.                       ELKIBGRC
00192      COMPUTE WS-ELIG-OP-BOTH =                                    ELKIBGRC
00193          BPL-NBR-BOTH-ENTRIES + WS-ELIG-OP.                       ELKIBGRC
00194      MOVE BPL-NBR-INST-ENTRIES TO WS-ELIG-INST.                   ELKIBGRC
00195      MOVE BPL-NBR-PROF-ENTRIES TO WS-ELIG-PROF.                   ELKIBGRC
00196      MOVE BPL-NBR-TBL-ENTRIES TO WS-ELIG-OV.                      ELKIBGRC
00197                                                                   ELKIBGRC
00198 ************************************************************      ELKIBGRC
00199 *                                                          *      ELKIBGRC
00200 *    MATCH IBGR TABULAR TO MASTER BENEFIT PROVISION LIST   *      ELKIBGRC
00201 *                                                          *      ELKIBGRC
00202 ************************************************************      ELKIBGRC
00203                                                                   ELKIBGRC
00204  1400-MATCH-IBGR-TAB-TO-MBPV.                                     ELKIBGRC
00205                                                                   ELKIBGRC
00206      IF GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) =                   ELKIBGRC
00207             BPL-BP-ID (BPL-IDX)                                   ELKIBGRC
00208         PERFORM 1500-UPDATE-COUNTS                                ELKIBGRC
00209             SET BPL-IDX UP BY 1                                   ELKIBGRC
00210             SET GX1-INDEX UP BY 1                                 ELKIBGRC
00211      ELSE                                                         ELKIBGRC
00212         IF GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) >                ELKIBGRC
00213               BPL-BP-ID (BPL-IDX)                                 ELKIBGRC
00214             SET BPL-IDX UP BY 1                                   ELKIBGRC
00215         ELSE                                                      ELKIBGRC
00216             SET GX1-INDEX UP BY 1.                                ELKIBGRC
00217                                                                   ELKIBGRC
00218                                                                   ELKIBGRC
00219 ************************************************************      ELKIBGRC
00220 *                                                          *      ELKIBGRC
00221 *    UPDATE COUNTS ACCORDING TO PROVISION CATEGORIES       *      ELKIBGRC
00222 *                                                          *      ELKIBGRC
00223 ************************************************************      ELKIBGRC
00224                                                                   ELKIBGRC
00225  1500-UPDATE-COUNTS.                                              ELKIBGRC
00226                                                                   ELKIBGRC
00227      IF    BPL-ALT-PROVN (BPL-IDX)                                ELKIBGRC
00228         OR BPL-NON-STD-PROVN (BPL-IDX)                            ELKIBGRC
00229      THEN                                                         ELKIBGRC
00230         CONTINUE                                                  ELKIBGRC
00231      ELSE                                                         ELKIBGRC
00232         ADD 1 TO WS-COUNT-OV                                      ELKIBGRC
00233                                                                   ELKIBGRC
00234         EVALUATE TRUE                                             ELKIBGRC
00235            WHEN BPL-BOTH (BPL-IDX)                                ELKIBGRC
00236               ADD 1 TO WS-COUNT-IP-BOTH                           ELKIBGRC
00237               ADD 1 TO WS-COUNT-OP-BOTH                           ELKIBGRC
00238            WHEN BPL-IP (BPL-IDX)                                  ELKIBGRC
00239               ADD 1 TO WS-COUNT-IP                                ELKIBGRC
00240               ADD 1 TO WS-COUNT-IP-BOTH                           ELKIBGRC
00241            WHEN BPL-OP (BPL-IDX)                                  ELKIBGRC
00242               ADD 1 TO WS-COUNT-OP                                ELKIBGRC
00243               ADD 1 TO WS-COUNT-OP-BOTH                           ELKIBGRC
00244            WHEN OTHER                                             ELKIBGRC
00245               SET WS-INTERNAL-ERROR TO TRUE                       ELKIBGRC
00246            END-EVALUATE                                           ELKIBGRC
00247                                                                   ELKIBGRC
00248         EVALUATE TRUE                                             ELKIBGRC
00249            WHEN BPL-INST-PROVN (BPL-IDX)                          ELKIBGRC
00250               ADD 1 TO WS-COUNT-INST                              ELKIBGRC
00251            WHEN BPL-PROF-PROVN (BPL-IDX)                          ELKIBGRC
00252               ADD 1 TO WS-COUNT-PROF                              ELKIBGRC
00253            WHEN OTHER                                             ELKIBGRC
00254               SET WS-INTERNAL-ERROR TO TRUE                       ELKIBGRC
00255            END-EVALUATE                                           ELKIBGRC
00256      END-IF.                                                      ELKIBGRC
00257                                                                   ELKIBGRC
00258 ************************************************************      ELKIBGRC
00259 *                                                                 ELKIBGRC
00260 *    COMPUTE IBGR CONFIDENCE FACTORS                              ELKIBGRC
00261 *                                                                 ELKIBGRC
00262 ************************************************************      ELKIBGRC
00263                                                                   ELKIBGRC
00264  2000-COMPUTE-IBGR-CONF-FACTORS.                                  ELKIBGRC
00265                                                                   ELKIBGRC
00266      IF GX1-ID-ARGUMENT-INCLUDED                                  ELKIBGRC
00267         MOVE WS-COUNT-INST TO WS-INCL-INST                        ELKIBGRC
00268         MOVE WS-COUNT-PROF TO WS-INCL-PROF                        ELKIBGRC
00269         MOVE WS-COUNT-IP TO WS-INCL-IP                            ELKIBGRC
00270         MOVE WS-COUNT-IP-BOTH TO WS-INCL-IP-BOTH                  ELKIBGRC
00271         MOVE WS-COUNT-OP TO WS-INCL-OP                            ELKIBGRC
00272         MOVE WS-COUNT-OP TO                                       ELKIBGRC
00273                  WS-DISPLAY-OP-COUNT                              ELKIBGRC
00274         MOVE WS-COUNT-OP-BOTH TO WS-INCL-OP-BOTH                  ELKIBGRC
00275         MOVE WS-COUNT-OV TO WS-INCL-OV                            ELKIBGRC
00276         PERFORM 2100-COMPUTE-CONF-FACTORS                         ELKIBGRC
00277      ELSE                                                         ELKIBGRC
00278         IF GX1-ID-ARGUMENT-EXCLUDED                               ELKIBGRC
00279            COMPUTE WS-INCL-INST =                                 ELKIBGRC
00280                 WS-ELIG-INST -  WS-COUNT-INST                     ELKIBGRC
00281            COMPUTE WS-INCL-PROF =                                 ELKIBGRC
00282                 WS-ELIG-PROF -  WS-COUNT-PROF                     ELKIBGRC
00283            COMPUTE WS-INCL-IP =                                   ELKIBGRC
00284                 WS-ELIG-IP -  WS-COUNT-IP                         ELKIBGRC
00285            COMPUTE WS-INCL-IP-BOTH =                              ELKIBGRC
00286                 WS-ELIG-IP-BOTH -  WS-COUNT-IP-BOTH               ELKIBGRC
00287            COMPUTE WS-INCL-OP =                                   ELKIBGRC
00288                 WS-ELIG-OP -  WS-COUNT-OP                         ELKIBGRC
00289            COMPUTE WS-INCL-OP-BOTH =                              ELKIBGRC
00290                 WS-ELIG-OP-BOTH -  WS-COUNT-OP-BOTH               ELKIBGRC
00291            COMPUTE WS-INCL-OV =                                   ELKIBGRC
00292                 WS-ELIG-OV -  WS-COUNT-OV                         ELKIBGRC
00293            PERFORM 2100-COMPUTE-CONF-FACTORS                      ELKIBGRC
00294         ELSE                                                      ELKIBGRC
00295            SET WS-MISSING-PARM TO TRUE.                           ELKIBGRC
00296                                                                   ELKIBGRC
00297 ************************************************************      ELKIBGRC
00298 *                                                                 ELKIBGRC
00299 *    COMPUTE CONFIDENCE FACTORS                                   ELKIBGRC
00300 *                                                                 ELKIBGRC
00301 ************************************************************      ELKIBGRC
00302                                                                   ELKIBGRC
00303  2100-COMPUTE-CONF-FACTORS.                                       ELKIBGRC
00304                                                                   ELKIBGRC
00305      COMPUTE CFDB-CF-IBGR-INSTTNL =                               ELKIBGRC
00306         ((WS-INCL-INST / WS-ELIG-INST) * 2 ) - 1.                 ELKIBGRC
00307      MOVE CFDB-CF-IBGR-INSTTNL TO WS-CF-IBGR-INSTTNL.             ELKIBGRC
00308                                                                   ELKIBGRC
00309      COMPUTE CFDB-CF-IBGR-PRFSNL =                                ELKIBGRC
00310         ((WS-INCL-PROF / WS-ELIG-PROF) * 2 ) - 1.                 ELKIBGRC
00311      MOVE CFDB-CF-IBGR-PRFSNL TO WS-CF-IBGR-PRFSNL.               ELKIBGRC
00312                                                                   ELKIBGRC
00313      COMPUTE CFDB-CF-IBGR-IP-ONLY =                               ELKIBGRC
00314         ((WS-INCL-IP / WS-ELIG-IP) * 2 ) - 1.                     ELKIBGRC
00315      MOVE CFDB-CF-IBGR-IP-ONLY TO WS-CF-IBGR-IP-ONLY.             ELKIBGRC
00316                                                                   ELKIBGRC
00317      COMPUTE CFDB-CF-IBGR-IP-BOTH =                               ELKIBGRC
00318         ((WS-INCL-IP-BOTH / WS-ELIG-IP-BOTH) * 2 ) - 1.           ELKIBGRC
00319      MOVE CFDB-CF-IBGR-IP-BOTH TO WS-CF-IBGR-IP-BOTH.             ELKIBGRC
00320                                                                   ELKIBGRC
00321      COMPUTE CFDB-CF-IBGR-OP-ONLY =                               ELKIBGRC
00322         ((WS-INCL-OP / WS-ELIG-OP) * 2 ) - 1.                     ELKIBGRC
00323      MOVE CFDB-CF-IBGR-OP-ONLY TO WS-CF-IBGR-OP-ONLY.             ELKIBGRC
00324                                                                   ELKIBGRC
00325      COMPUTE CFDB-CF-IBGR-OP-BOTH =                               ELKIBGRC
00326         ((WS-INCL-OP-BOTH / WS-ELIG-OP-BOTH) * 2 ) - 1.           ELKIBGRC
00327      MOVE CFDB-CF-IBGR-OP-BOTH TO WS-CF-IBGR-OP-BOTH.             ELKIBGRC
00328                                                                   ELKIBGRC
00329      COMPUTE CFDB-CF-IBGR-OV =                                    ELKIBGRC
00330         ((WS-INCL-OV / WS-ELIG-OV) * 2 ) - 1.                     ELKIBGRC
00331      MOVE CFDB-CF-IBGR-OV TO WS-CF-IBGR-OV.                       ELKIBGRC
00332                                                                   ELKIBGRC
