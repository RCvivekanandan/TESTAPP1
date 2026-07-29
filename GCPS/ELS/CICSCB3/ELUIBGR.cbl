00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUIBGR 
00003  PROGRAM-ID.              ELUIBGR                                    LV001
00004                                                                   ELUIBGR 
00005  AUTHOR.                  R. BARILEAU                             ELUIBGR 
00006                           R. LUKETICH (REWRITE SEP 1989).         ELUIBGR 
00007                                                                   ELUIBGR 
00008  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELUIBGR 
00009                        A MUTUAL LEGAL RESERVE COMPANY             ELUIBGR 
00010                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELUIBGR 
00011                        233 N. MICHIGAN AVE                        ELUIBGR 
00012                        CHICAGO, ILLINOIS 60601                    ELUIBGR 
00013                                                                   ELUIBGR 
00014  DATE-WRITTEN.         07-SEP-1988.                               ELUIBGR 
00015                        20-SEP-1989 REWRITTEN.                     ELUIBGR 
00016                                                                   ELUIBGR 
00017  ENVIRONMENT DIVISION.                                            ELUIBGR 
00018  CONFIGURATION SECTION.                                           ELUIBGR 
00019  SOURCE-COMPUTER. IBM-3033.                                       ELUIBGR 
00020  OBJECT-COMPUTER. IBM-3033.                                       ELUIBGR 
00021                                                                   ELUIBGR 
00022 ****************************************************************  ELUIBGR 
00023 *                                                              *  ELUIBGR 
00024 *  ELUIBGR :   THIS MODULE IS BEING CALLLED BY 'ELUOVCFD'.     *  ELUIBGR 
00025 *              THE PURPOSE IS DETERMINE THE PARTIAL            *  ELUIBGR 
00026 *              CONFIDENCE FACTOR FOR THE OVERALL ATTRIBUTE.    *  ELUIBGR 
00027 *                                                              *  ELUIBGR 
00028 ****************************************************************  ELUIBGR 
00029 *                      MAINTENANCE HISTORY                     *  ELUIBGR 
00030 *                                                              *  ELUIBGR 
00031 *  MOD     DATE      BY  DRPT              ACTION              *  ELUIBGR 
00032 * ----- ----------- --- ----- ---------------------------------*  ELUIBGR 
00033 * 01.00 07-SEP-1988 REB       CREATED                          *  ELUIBGR 
00034 * 01.01 19-OCT-1988 NAC       REVISE THE LOGIC THAT DETERMINES *  ELUIBGR 
00035 *                             THE INCLUDE COUNTS FOR ELSIBGRC. *  ELUIBGR 
00036 * 02.01 20-SEP=1989 RJL       SIGNIFICANT REVISION TO INCOR-   *  ELUIBGR 
00037 *                             PORATE ALL IBGR CF CALCULATIONS  *  ELUIBGR 
00038 *                             IN THIS MODULE INSTEAD OF        *  ELUIBGR 
00039 *                             SPLITTING COMPUTATIONS BETWEEN   *  ELUIBGR 
00040 *                             THIS MODULE AND ELUOVCFD.  ALSO, *  ELUIBGR 
00041 *                             INCORPORATE NEW BENEFIT          *  ELUIBGR 
00042 *                             PROVISION INFORMATION TABLE      *  ELUIBGR 
00043 *                             (BPL).                           *  ELUIBGR 
00044 *                                                              *  ELUIBGR 
00045 * 02.02 06-MAY-1991 JPB       DROPPED MULTIPLICATION AND       *  ELUIBGR 
00046 *                             SUBTRACTION FROM CALCULATION.    *  ELUIBGR 
00047 *                             ADDED CODE TO MOVE FLOATING-PT 0 *  ELUIBGR 
00048 *                             TO FIELD IF NECESSARY.           *  ELUIBGR 
00049 *                                                              *  ELUIBGR 
00050 ****************************************************************  ELUIBGR 
00051                                                                   ELUIBGR 
00052  DATA DIVISION.                                                   ELUIBGR 
00053                                                                   ELUIBGR 
00054  WORKING-STORAGE SECTION.                                         ELUIBGR 
00055                                                                   ELUIBGR 
00056  01  WS-BEGIN                       PIC X(32)   VALUE             ELUIBGR 
00057      '*THIS IS THE START OF ELUIBGR *'.                           ELUIBGR 
00058                                                                   ELUIBGR 
00059  01  WS-ELIG-COUNTS.                                              ELUIBGR 
00060      02 WS-ELIG-BOTH             PICTURE S9(04)          COMP.    ELUIBGR 
00061      02 WS-ELIG-IP               PICTURE S9(04)          COMP.    ELUIBGR 
00062      02 WS-ELIG-IP-BOTH          PICTURE S9(04)          COMP.    ELUIBGR 
00063      02 WS-ELIG-OP               PICTURE S9(04)          COMP.    ELUIBGR 
00064      02 WS-ELIG-OP-BOTH          PICTURE S9(04)          COMP.    ELUIBGR 
00065      02 WS-ELIG-INST             PICTURE S9(04)          COMP.    ELUIBGR 
00066      02 WS-ELIG-PROF             PICTURE S9(04)          COMP.    ELUIBGR 
00067      02 WS-ELIG-OV               PICTURE S9(04)          COMP.    ELUIBGR 
00068                                                                   ELUIBGR 
00069  01  WS-INCL-COUNTS.                                              ELUIBGR 
00070      02 WS-INCL-BOTH             PICTURE S9(04)          COMP.    ELUIBGR 
00071      02 WS-INCL-IP               PICTURE S9(04)          COMP.    ELUIBGR 
00072      02 WS-INCL-IP-BOTH          PICTURE S9(04)          COMP.    ELUIBGR 
00073      02 WS-INCL-OP               PICTURE S9(04)          COMP.    ELUIBGR 
00074      02 WS-INCL-OP-BOTH          PICTURE S9(04)          COMP.    ELUIBGR 
00075      02 WS-INCL-INST             PICTURE S9(04)          COMP.    ELUIBGR 
00076      02 WS-INCL-PROF             PICTURE S9(04)          COMP.    ELUIBGR 
00077      02 WS-INCL-OV               PICTURE S9(04)          COMP.    ELUIBGR 
00078                                                                   ELUIBGR 
00079  01  WS-ELIG-FACTORS.                                             ELUIBGR 
00080      02 WS-ELIG-FACT-IP          COMP-1.                          ELUIBGR 
00081      02 WS-ELIG-FACT-IP-BOTH     COMP-1.                          ELUIBGR 
00082      02 WS-ELIG-FACT-OP          COMP-1.                          ELUIBGR 
00083      02 WS-ELIG-FACT-OP-BOTH     COMP-1.                          ELUIBGR 
00084      02 WS-ELIG-FACT-INST        COMP-1.                          ELUIBGR 
00085      02 WS-ELIG-FACT-PROF        COMP-1.                          ELUIBGR 
00086      02 WS-ELIG-FACT-OV          COMP-1.                          ELUIBGR 
00087                                                                   ELUIBGR 
00088  01  WS-INCL-FACTORS.                                             ELUIBGR 
00089      02 WS-INCL-FACT-IP          COMP-1.                          ELUIBGR 
00090      02 WS-INCL-FACT-IP-BOTH     COMP-1.                          ELUIBGR 
00091      02 WS-INCL-FACT-OP          COMP-1.                          ELUIBGR 
00092      02 WS-INCL-FACT-OP-BOTH     COMP-1.                          ELUIBGR 
00093      02 WS-INCL-FACT-INST        COMP-1.                          ELUIBGR 
00094      02 WS-INCL-FACT-PROF        COMP-1.                          ELUIBGR 
00095      02 WS-INCL-FACT-OV          COMP-1.                          ELUIBGR 
00096 /                                                                 ELUIBGR 
00097  COPY ELSBPTBL.                                                   ELUIBGR 
00098 /                                                                 ELUIBGR 
00099  LINKAGE SECTION.                                                 ELUIBGR 
00100                                                                   ELUIBGR 
00101  01  DFHCOMMAREA.                                                 ELUIBGR 
00102      COPY ELSCOMMC.                                               ELUIBGR 
00103 /                                                                 ELUIBGR 
00104      COPY ELSCIA2C.                                               ELUIBGR 
00105 /    COPYBOOK USED FOR OVERALL ACCUM IBGR TABLE                   ELUIBGR 
00106      COPY ELSIBGRC.                                               ELUIBGR 
00107 /    GCPS COPYBOOK USED FOR IBGN RECORD LAYOUT                    ELUIBGR 
00108  01  IBGR-RECORD.                                                 ELUIBGR 
00109      COPY GCTIBGRC.                                               ELUIBGR 
00110 /***********************************************************      ELUIBGR 
00111 *                                                          *      ELUIBGR 
00112 *    CREATE IBGR CONFIDENCE FACTORS TABLE                  *      ELUIBGR 
00113 *                                                          *      ELUIBGR 
00114 ************************************************************      ELUIBGR 
00115                                                                   ELUIBGR 
00116  PROCEDURE DIVISION.                                              ELUIBGR 
00117                                                                   ELUIBGR 
00118  000-CREATE-IBGR-CF-TBL.                                          ELUIBGR 
00119      PERFORM 001-INITIALIZE.                                      ELUIBGR 
00120      PERFORM 100-PROCESS.                                         ELUIBGR 
00121      GOBACK.                                                      ELUIBGR 
00122                                                                   ELUIBGR 
00123 ************************************************************      ELUIBGR 
00124 *                                                          *      ELUIBGR 
00125 *        INITIALIZE                                        *      ELUIBGR 
00126 *                                                          *      ELUIBGR 
00127 ************************************************************      ELUIBGR 
00128                                                                   ELUIBGR 
00129  001-INITIALIZE.                                                  ELUIBGR 
00130      PERFORM 900-CHECK-FOR-VALID-COMMAREA.                        ELUIBGR 
00131      PERFORM 901-ESTAB-ADDR-CIA.                                  ELUIBGR 
00132      PERFORM 902-INITIALIZE-STORAGE-CTL.                          ELUIBGR 
00133      PERFORM 920-ESTAB-ADDR-IBGR-TBL.                             ELUIBGR 
00134      PERFORM 010-DETERMINE-IBGR-ELIG-COUNTS.                      ELUIBGR 
00135      PERFORM 020-COPY-ELIG-CNTS-TO-FACTS.                         ELUIBGR 
00136                                                                   ELUIBGR 
00137 ************************************************************      ELUIBGR 
00138 *                                                          *      ELUIBGR 
00139 *    DETERMINE IBGR ELIGIBLE COUNTS                        *      ELUIBGR 
00140 *                                                          *      ELUIBGR 
00141 ************************************************************      ELUIBGR 
00142                                                                   ELUIBGR 
00143  010-DETERMINE-IBGR-ELIG-COUNTS.                                  ELUIBGR 
00144      MOVE BPL-NBR-BOTH-ENTRIES TO WS-ELIG-BOTH.                   ELUIBGR 
00145      MOVE BPL-NBR-IP-ENTRIES TO WS-ELIG-IP.                       ELUIBGR 
00146      COMPUTE WS-ELIG-IP-BOTH                                      ELUIBGR 
00147         = WS-ELIG-BOTH + WS-ELIG-IP.                              ELUIBGR 
00148      MOVE BPL-NBR-OP-ENTRIES TO WS-ELIG-OP.                       ELUIBGR 
00149      COMPUTE WS-ELIG-OP-BOTH                                      ELUIBGR 
00150         = WS-ELIG-BOTH + WS-ELIG-OP.                              ELUIBGR 
00151      MOVE BPL-NBR-INST-ENTRIES TO WS-ELIG-INST.                   ELUIBGR 
00152      MOVE BPL-NBR-PROF-ENTRIES TO WS-ELIG-PROF.                   ELUIBGR 
00153      MOVE BPL-NBR-TBL-ENTRIES TO WS-ELIG-OV.                      ELUIBGR 
00154                                                                   ELUIBGR 
00155 ************************************************************      ELUIBGR 
00156 *                                                          *      ELUIBGR 
00157 *    COPY ELIGIBLE COUNTS TO ELIGIBLE FACTORS              *      ELUIBGR 
00158 *                                                          *      ELUIBGR 
00159 ************************************************************      ELUIBGR 
00160                                                                   ELUIBGR 
00161  020-COPY-ELIG-CNTS-TO-FACTS.                                     ELUIBGR 
00162      MOVE WS-ELIG-IP      TO WS-ELIG-FACT-IP.                     ELUIBGR 
00163      MOVE WS-ELIG-IP-BOTH TO WS-ELIG-FACT-IP-BOTH.                ELUIBGR 
00164      MOVE WS-ELIG-OP      TO WS-ELIG-FACT-OP.                     ELUIBGR 
00165      MOVE WS-ELIG-OP-BOTH TO WS-ELIG-FACT-OP-BOTH.                ELUIBGR 
00166      MOVE WS-ELIG-INST    TO WS-ELIG-FACT-INST.                   ELUIBGR 
00167      MOVE WS-ELIG-PROF    TO WS-ELIG-FACT-PROF.                   ELUIBGR 
00168      MOVE WS-ELIG-OV      TO WS-ELIG-FACT-OV.                     ELUIBGR 
00169                                                                   ELUIBGR 
00170 ************************************************************      ELUIBGR 
00171 *                                                          *      ELUIBGR 
00172 *    PROCESS                                               *      ELUIBGR 
00173 *                                                          *      ELUIBGR 
00174 ************************************************************      ELUIBGR 
00175                                                                   ELUIBGR 
00176  100-PROCESS.                                                     ELUIBGR 
00177      PERFORM 101-PROCESS-EACH-IBGR                                ELUIBGR 
00178         VARYING IBGR-IDX FROM 1 BY 1                              ELUIBGR 
00179           UNTIL IBGR-IDX > IBGR-TBL-CNT.                          ELUIBGR 
00180                                                                   ELUIBGR 
00181 ************************************************************      ELUIBGR 
00182 *                                                          *      ELUIBGR 
00183 *    PROCESS EACH IBGR TABULAR IN ELSIBGR TABLE            *      ELUIBGR 
00184 *                                                          *      ELUIBGR 
00185 ************************************************************      ELUIBGR 
00186                                                                   ELUIBGR 
00187  101-PROCESS-EACH-IBGR.                                           ELUIBGR 
00188      IF IBGR-TABULAR-PTR (IBGR-IDX) NOT = NULL                    ELUIBGR 
00189      THEN                                                         ELUIBGR 
00190         PERFORM 200-PROCESS-IBGR-RECORD-GIVEN.                    ELUIBGR 
00191                                                                   ELUIBGR 
00192 ************************************************************      ELUIBGR 
00193 *                                                          *      ELUIBGR 
00194 *    PROCESS IBGR RECORD GIVEN                             *      ELUIBGR 
00195 *                                                          *      ELUIBGR 
00196 ************************************************************      ELUIBGR 
00197                                                                   ELUIBGR 
00198  200-PROCESS-IBGR-RECORD-GIVEN.                                   ELUIBGR 
00199      SET ADDRESS OF IBGR-RECORD TO IBGR-TABULAR-PTR (IBGR-IDX).   ELUIBGR 
00200      INITIALIZE WS-INCL-COUNTS.                                   ELUIBGR 
00201      PERFORM 210-DETERMINE-IBGR-INCL-VAL.                         ELUIBGR 
00202      PERFORM 291-COPY-INCL-CNTS-TO-FACTS.                         ELUIBGR 
00203      PERFORM 299-COMPUTE-IBGR-CF.                                 ELUIBGR 
00204                                                                   ELUIBGR 
00205 ************************************************************      ELUIBGR 
00206 *                                                          *      ELUIBGR 
00207 *    DETERMINE IBGR INCLUDE VALUES                         *      ELUIBGR 
00208 *                                                          *      ELUIBGR 
00209 ************************************************************      ELUIBGR 
00210                                                                   ELUIBGR 
00211  210-DETERMINE-IBGR-INCL-VAL.                                     ELUIBGR 
00212      PERFORM 220-COUNT-TABULAR-CONTENTS                           ELUIBGR 
00213         VARYING GX1-INDEX FROM 1 BY 1                             ELUIBGR 
00214           UNTIL GX1-INDEX EQUAL GX1-ENTRY-COUNT.                  ELUIBGR 
00215                                                                   ELUIBGR 
00216      IF GX1-ID-ARGUMENT-EXCLUDED                                  ELUIBGR 
00217      THEN                                                         ELUIBGR 
00218         PERFORM 280-CONVERT-EXCL-TO-INCL.                         ELUIBGR 
00219                                                                   ELUIBGR 
00220      COMPUTE WS-INCL-IP-BOTH =                                    ELUIBGR 
00221         WS-INCL-BOTH + WS-INCL-IP.                                ELUIBGR 
00222      COMPUTE WS-INCL-OP-BOTH =                                    ELUIBGR 
00223         WS-INCL-BOTH + WS-INCL-OP.                                ELUIBGR 
00224                                                                   ELUIBGR 
00225 ************************************************************      ELUIBGR 
00226 *                                                          *      ELUIBGR 
00227 *    COUNT TABULAR CONTENTS                                *      ELUIBGR 
00228 *                                                          *      ELUIBGR 
00229 ************************************************************      ELUIBGR 
00230                                                                   ELUIBGR 
00231  220-COUNT-TABULAR-CONTENTS.                                      ELUIBGR 
00232      SEARCH ALL BPL-TBL                                           ELUIBGR 
00233         AT END                                                    ELUIBGR 
00234            CONTINUE                                               ELUIBGR 
00235         WHEN   BPL-BP-ID (BPL-IDX)                                ELUIBGR 
00236              = GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)              ELUIBGR 
00237            PERFORM 230-UPDATE-COUNTS                              ELUIBGR 
00238         END-SEARCH.                                               ELUIBGR 
00239                                                                   ELUIBGR 
00240 ************************************************************      ELUIBGR 
00241 *                                                          *      ELUIBGR 
00242 *    UPDATE COUNTS ACCORDING TO PROVISION CATEGORIES       *      ELUIBGR 
00243 *                                                          *      ELUIBGR 
00244 ************************************************************      ELUIBGR 
00245                                                                   ELUIBGR 
00246  230-UPDATE-COUNTS.                                               ELUIBGR 
00247      IF    BPL-ALT-PROVN (BPL-IDX)                                ELUIBGR 
00248         OR BPL-NON-STD-PROVN (BPL-IDX)                            ELUIBGR 
00249      THEN                                                         ELUIBGR 
00250         CONTINUE                                                  ELUIBGR 
00251      ELSE                                                         ELUIBGR 
00252         ADD 1 TO WS-INCL-OV                                       ELUIBGR 
00253                                                                   ELUIBGR 
00254         EVALUATE TRUE                                             ELUIBGR 
00255            WHEN BPL-BOTH (BPL-IDX)                                ELUIBGR 
00256               ADD 1 TO WS-INCL-BOTH                               ELUIBGR 
00257            WHEN BPL-IP (BPL-IDX)                                  ELUIBGR 
00258               ADD 1 TO WS-INCL-IP                                 ELUIBGR 
00259            WHEN BPL-OP (BPL-IDX)                                  ELUIBGR 
00260               ADD 1 TO WS-INCL-OP                                 ELUIBGR 
00261            WHEN OTHER                                             ELUIBGR 
00262               PERFORM 989-SIGNAL-PROGRAM-LOGIC-ERROR              ELUIBGR 
00263            END-EVALUATE                                           ELUIBGR 
00264                                                                   ELUIBGR 
00265         EVALUATE TRUE                                             ELUIBGR 
00266            WHEN BPL-INST-PROVN (BPL-IDX)                          ELUIBGR 
00267               ADD 1 TO WS-INCL-INST                               ELUIBGR 
00268            WHEN BPL-PROF-PROVN (BPL-IDX)                          ELUIBGR 
00269               ADD 1 TO WS-INCL-PROF                               ELUIBGR 
00270            WHEN OTHER                                             ELUIBGR 
00271               PERFORM 989-SIGNAL-PROGRAM-LOGIC-ERROR              ELUIBGR 
00272            END-EVALUATE                                           ELUIBGR 
00273      END-IF.                                                      ELUIBGR 
00274                                                                   ELUIBGR 
00275 ************************************************************      ELUIBGR 
00276 *                                                          *      ELUIBGR 
00277 *    CONVERT EXCLUDED COUNTS TO INCLUDED COUNTS            *      ELUIBGR 
00278 *                                                          *      ELUIBGR 
00279 ************************************************************      ELUIBGR 
00280                                                                   ELUIBGR 
00281  280-CONVERT-EXCL-TO-INCL.                                        ELUIBGR 
00282      COMPUTE WS-INCL-BOTH    = WS-ELIG-BOTH    - WS-INCL-BOTH.    ELUIBGR 
00283      COMPUTE WS-INCL-IP      = WS-ELIG-IP      - WS-INCL-IP.      ELUIBGR 
00284      COMPUTE WS-INCL-IP-BOTH = WS-ELIG-IP-BOTH - WS-INCL-IP-BOTH. ELUIBGR 
00285      COMPUTE WS-INCL-OP      = WS-ELIG-OP      - WS-INCL-OP.      ELUIBGR 
00286      COMPUTE WS-INCL-OP-BOTH = WS-ELIG-OP-BOTH - WS-INCL-OP-BOTH. ELUIBGR 
00287      COMPUTE WS-INCL-INST    = WS-ELIG-INST    - WS-INCL-INST.    ELUIBGR 
00288      COMPUTE WS-INCL-PROF    = WS-ELIG-PROF    - WS-INCL-PROF.    ELUIBGR 
00289      COMPUTE WS-INCL-OV      = WS-ELIG-OV      - WS-INCL-OV.      ELUIBGR 
00290                                                                   ELUIBGR 
00291 ************************************************************      ELUIBGR 
00292 *                                                          *      ELUIBGR 
00293 *    COPY INCLUDED COUNTS TO INCLUDED FACTORS              *      ELUIBGR 
00294 *                                                          *      ELUIBGR 
00295 ************************************************************      ELUIBGR 
00296                                                                   ELUIBGR 
00297  291-COPY-INCL-CNTS-TO-FACTS.                                     ELUIBGR 
00298      MOVE WS-INCL-IP      TO WS-INCL-FACT-IP.                     ELUIBGR 
00299      MOVE WS-INCL-IP-BOTH TO WS-INCL-FACT-IP-BOTH.                ELUIBGR 
00300      MOVE WS-INCL-OP      TO WS-INCL-FACT-OP.                     ELUIBGR 
00301      MOVE WS-INCL-OP-BOTH TO WS-INCL-FACT-OP-BOTH.                ELUIBGR 
00302      MOVE WS-INCL-INST    TO WS-INCL-FACT-INST.                   ELUIBGR 
00303      MOVE WS-INCL-PROF    TO WS-INCL-FACT-PROF.                   ELUIBGR 
00304      MOVE WS-INCL-OV      TO WS-INCL-FACT-OV.                     ELUIBGR 
00305                                                                   ELUIBGR 
00306 /***********************************************************      ELUIBGR 
00307 *                                                          *      ELUIBGR 
00308 *    COMPUTE IBGR CONFIDENCE FACTORS                       *      ELUIBGR 
00309 *                                                          *      ELUIBGR 
00310 *    NOTE THAT THE CALCULATION NORMALIZES THE CONFIDENCE   *      ELUIBGR 
00311 *    FACTOR INTO A RANGE OF (-1..+1) AS FOLLOWS:           *      ELUIBGR 
00312 *                                                          *      ELUIBGR 
00313 *       ((X/Y) * 2) - 1                                    *      ELUIBGR 
00314 *                                                          *      ELUIBGR 
00315 *    FOR EXAMPLE, IF X=15 AND Y=20                         *      ELUIBGR 
00316 *                                                          *      ELUIBGR 
00317 *         X/Y            =  0.75                           *      ELUIBGR 
00318 *        (   ) * 2       =  1.50                           *      ELUIBGR 
00319 *       (         ) - 1  =  0.50                           *      ELUIBGR 
00320 *                                                          *      ELUIBGR 
00321 *    ANOTHER EXAMPLE, IF X=5 AND Y=20                      *      ELUIBGR 
00322 *                                                          *      ELUIBGR 
00323 *         X/Y            =  0.25                           *      ELUIBGR 
00324 *        (   ) * 2       =  0.50                           *      ELUIBGR 
00325 *       (         ) - 1  = -0.50                           *      ELUIBGR 
00326 *                                                          *      ELUIBGR 
00327 *    IN THIS WAY, AN IBGR WITH FEW INCLUDED ENTRIES IS     *      ELUIBGR 
00328 *    GIVEN A NEGATIVE CONFIDENCE FACTOR.                   *      ELUIBGR 
00329 *                                                          *      ELUIBGR 
00330 ************************************************************      ELUIBGR 
00331                                                                   ELUIBGR 
00332  299-COMPUTE-IBGR-CF.                                             ELUIBGR 
00333      COMPUTE IBGR-CF-IP      (IBGR-IDX) =                         ELUIBGR 
00334          (WS-INCL-FACT-IP      / WS-ELIG-FACT-IP     ).           ELUIBGR 
00335           IF IBGR-CF-IP      (IBGR-IDX) = 0                       ELUIBGR 
00336              THEN MOVE -1.0E00 TO IBGR-CF-IP (IBGR-IDX).          ELUIBGR 
00337                                                                   ELUIBGR 
00338      COMPUTE IBGR-CF-IP-BOTH (IBGR-IDX) =                         ELUIBGR 
00339          (WS-INCL-FACT-IP-BOTH / WS-ELIG-FACT-IP-BOTH).           ELUIBGR 
00340           IF IBGR-CF-IP-BOTH (IBGR-IDX) = 0                       ELUIBGR 
00341              THEN MOVE -1.0E00 TO IBGR-CF-IP-BOTH(IBGR-IDX).      ELUIBGR 
00342                                                                   ELUIBGR 
00343      COMPUTE IBGR-CF-OP      (IBGR-IDX) =                         ELUIBGR 
00344          (WS-INCL-FACT-OP      / WS-ELIG-FACT-OP     ).           ELUIBGR 
00345           IF IBGR-CF-OP      (IBGR-IDX) = 0                       ELUIBGR 
00346              THEN MOVE -1.0E00 TO IBGR-CF-OP (IBGR-IDX).          ELUIBGR 
00347                                                                   ELUIBGR 
00348      COMPUTE IBGR-CF-OP-BOTH (IBGR-IDX) =                         ELUIBGR 
00349          (WS-INCL-FACT-OP-BOTH / WS-ELIG-FACT-OP-BOTH).           ELUIBGR 
00350           IF IBGR-CF-OP-BOTH (IBGR-IDX) = 0                       ELUIBGR 
00351              THEN MOVE -1.0E00 TO IBGR-CF-OP-BOTH (IBGR-IDX).     ELUIBGR 
00352                                                                   ELUIBGR 
00353      COMPUTE IBGR-CF-INST    (IBGR-IDX) =                         ELUIBGR 
00354          (WS-INCL-FACT-INST    / WS-ELIG-FACT-INST   ).           ELUIBGR 
00355           IF IBGR-CF-INST    (IBGR-IDX) = 0                       ELUIBGR 
00356              THEN MOVE -1.0E00 TO IBGR-CF-INST (IBGR-IDX).        ELUIBGR 
00357                                                                   ELUIBGR 
00358      COMPUTE IBGR-CF-PROF    (IBGR-IDX) =                         ELUIBGR 
00359          (WS-INCL-FACT-PROF    / WS-ELIG-FACT-PROF   ).           ELUIBGR 
00360           IF IBGR-CF-PROF    (IBGR-IDX) = 0                       ELUIBGR 
00361              THEN MOVE -1.0E00 TO IBGR-CF-PROF (IBGR-IDX).        ELUIBGR 
00362                                                                   ELUIBGR 
00363      COMPUTE IBGR-CF-OV      (IBGR-IDX) =                         ELUIBGR 
00364         ((WS-INCL-FACT-OV      / WS-ELIG-FACT-OV     ) * 2 ) - 1. ELUIBGR 
00365                                                                   ELUIBGR 
00366 ************************************************************      ELUIBGR 
00367 *                                                          *      ELUIBGR 
00368 *    CHECK FOR VALID COMMAREA                              *      ELUIBGR 
00369 *                                                          *      ELUIBGR 
00370 ************************************************************      ELUIBGR 
00371                                                                   ELUIBGR 
00372  900-CHECK-FOR-VALID-COMMAREA.                                    ELUIBGR 
00373      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELUIBGR 
00374      THEN                                                         ELUIBGR 
00375         EXEC CICS ABEND ABCODE('EL01') END-EXEC.                  ELUIBGR 
00376                                                                   ELUIBGR 
00377 ************************************************************      ELUIBGR 
00378 *                                                          *      ELUIBGR 
00379 *    ESTABLISH ADDRESSABILITY OF THE COMMON INTERFACE AREA *      ELUIBGR 
00380 *                                                          *      ELUIBGR 
00381 ************************************************************      ELUIBGR 
00382                                                                   ELUIBGR 
00383  901-ESTAB-ADDR-CIA.                                              ELUIBGR 
00384      IF ECA-CIA-PTR = NULL                                        ELUIBGR 
00385      THEN                                                         ELUIBGR 
00386         EXEC CICS ABEND ABCODE('EL02') END-EXEC                   ELUIBGR 
00387      ELSE                                                         ELUIBGR 
00388         SET ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA              ELUIBGR 
00389            TO ECA-CIA-PTR.                                        ELUIBGR 
00390                                                                   ELUIBGR 
00391 ************************************************************      ELUIBGR 
00392 *                                                          *      ELUIBGR 
00393 *    INITIALIZE STORAGE CONTROL TABLE                      *      ELUIBGR 
00394 *                                                          *      ELUIBGR 
00395 ************************************************************      ELUIBGR 
00396                                                                   ELUIBGR 
00397  902-INITIALIZE-STORAGE-CTL.                                      ELUIBGR 
00398      CALL 'ELUINISM'                                              ELUIBGR 
00399         USING DFHCOMMAREA                                         ELUIBGR 
00400               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.           ELUIBGR 
00401                                                                   ELUIBGR 
00402 ************************************************************      ELUIBGR 
00403 *                                                          *      ELUIBGR 
00404 *    ESTABLISH ADDRESSABILITY OF IBGR TABLE                *      ELUIBGR 
00405 *                                                          *      ELUIBGR 
00406 ************************************************************      ELUIBGR 
00407                                                                   ELUIBGR 
00408  920-ESTAB-ADDR-IBGR-TBL.                                         ELUIBGR 
00409      SET CIA-ELSIBGR-DDN TO TRUE.                                 ELUIBGR 
00410      CALL 'ELUSETAD'                                              ELUIBGR 
00411          USING DFHCOMMAREA                                        ELUIBGR 
00412                ADDRESS OF IBGR-INTERNAL-TABS-TABLE.               ELUIBGR 
00413      IF CIA-RC-PTR-NULL                                           ELUIBGR 
00414      THEN                                                         ELUIBGR 
00415         PERFORM 991-SIGNAL-UNALLOC-AREA-ERROR.                    ELUIBGR 
00416                                                                   ELUIBGR 
00417 ************************************************************      ELUIBGR 
00418 *                                                          *      ELUIBGR 
00419 *    SIGNAL PROGRAM LOGIC ERROR                            *      ELUIBGR 
00420 *                                                          *      ELUIBGR 
00421 ************************************************************      ELUIBGR 
00422                                                                   ELUIBGR 
00423  989-SIGNAL-PROGRAM-LOGIC-ERROR.                                  ELUIBGR 
00424      SET CIA-AB-PGM-LOGIC TO TRUE.                                ELUIBGR 
00425      PERFORM 999-SIGNAL-ABEND.                                    ELUIBGR 
00426                                                                   ELUIBGR 
00427 ************************************************************      ELUIBGR 
00428 *                                                          *      ELUIBGR 
00429 *    SIGNAL UNALLOCATED AREA ERROR                         *      ELUIBGR 
00430 *                                                          *      ELUIBGR 
00431 ************************************************************      ELUIBGR 
00432                                                                   ELUIBGR 
00433  991-SIGNAL-UNALLOC-AREA-ERROR.                                   ELUIBGR 
00434      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUIBGR 
00435      PERFORM 999-SIGNAL-ABEND.                                    ELUIBGR 
00436                                                                   ELUIBGR 
00437 ************************************************************      ELUIBGR 
00438 *                                                          *      ELUIBGR 
00439 *    SIGNAL ABEND                                          *      ELUIBGR 
00440 *                                                          *      ELUIBGR 
00441 ************************************************************      ELUIBGR 
00442                                                                   ELUIBGR 
00443  999-SIGNAL-ABEND.                                                ELUIBGR 
00444      EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.                ELUIBGR 
00445                                                                   ELUIBGR 
