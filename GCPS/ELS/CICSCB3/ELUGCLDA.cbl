00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELUGCLDA
00003  PROGRAM-ID.           ELUGCLDA.                                     LV005
00004                                                                   ELUGCLDA
00005  AUTHOR.               R. LUKETICH                                ELUGCLDA
00006                                                                   ELUGCLDA
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELUGCLDA
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELUGCLDA
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELUGCLDA
00010                        233 N. MICHIGAN AVE                        ELUGCLDA
00011                        CHICAGO, ILLINOIS 60601                    ELUGCLDA
00012                                                                   ELUGCLDA
00013  DATE-WRITTEN.         16-OCT-1989 (REWRITTEN)                    ELUGCLDA
00014                                                                   ELUGCLDA
00015                                                                   ELUGCLDA
00016  DATE-COMPILED.                                                   ELUGCLDA
00017                                                                   ELUGCLDA
00018  SECURITY.             COPYRIGHT 1986,                            ELUGCLDA
00019                        HEALTH CARE SERVICE CORPORATION            ELUGCLDA
00020                                                                   ELUGCLDA
00021  ENVIRONMENT DIVISION.                                            ELUGCLDA
00022                                                                   ELUGCLDA
00023  CONFIGURATION SECTION.                                           ELUGCLDA
00024                                                                   ELUGCLDA
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELUGCLDA
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELUGCLDA
00027 ******************************************************************ELUGCLDA
00028 *                                                                *ELUGCLDA
00029 *                            FUNCTION                            *ELUGCLDA
00030 *                                                                *ELUGCLDA
00031 * THIS PROGRAM WILL SEARCH TO FIND AND READ EVERY POSSIBLE       *ELUGCLDA
00032 * ACCUMULATOR ATTACHED AT THE GROUP SPECIFIC AND CONTRACT        *ELUGCLDA
00033 * LEVELS. IT WILL THEN SAVE THAT ACCUM RECORD AND STORE IT IN    *ELUGCLDA
00034 * THE CONTRACT SUMMARY ACCUMULATOR TABLE (COPYBOOK ELSCSACC) FOR *ELUGCLDA
00035 * FUTURE PROCESSING BY SUCCEEDING MODULES.                       *ELUGCLDA
00036 *                                                                *ELUGCLDA
00037 * NOTE:  THE COPY LIBS GCTIBGRC, GCTIDGDC, GCTIPGNC, GCTIPGPC    *ELUGCLDA
00038 *        AND GCTIPGTC ARE NOT NEEDED SINCE THE TABULAR DATA      *ELUGCLDA
00039 *        IS MOVED FROM THE ALL LEVEL TABULAR AREA.               *ELUGCLDA
00040 *                                                                *ELUGCLDA
00041 ******************************************************************ELUGCLDA
00042 *                      MAINTENANCE HISTORY                       *ELUGCLDA
00043 *                                                                *ELUGCLDA
00044 *  MOD     DATE     BY                DESCRIPTION                *ELUGCLDA
00045 * ----- ----------- --- ---------------------------------------- *ELUGCLDA
00046 * 01.00 27-JUL-1988 AKK CREATED                                  *ELUGCLDA
00047 * 02.00 12-OCT-1989 RJL REWRITTEN TO CLEAN UP LOGIC STRUCTURE,   *ELUGCLDA
00048 *                       CONVERT FROM STRUCTURE(S) CODE GENERATOR *ELUGCLDA
00049 *                       AND IMPROVE EFFICIENCY.                  *ELUGCLDA
00050 *                                                                *ELUGCLDA
00051 * 02.01 16-OCT-1992 BAK ADD 3 NEW CONDITION BITS.  CHANGED MOVE  *ELUGCLDA
00052 *                       OF CONDITION BITS TO BE GROUP MOVE.      *ELUGCLDA
00053 *                       CHANGED IPGD TO IDGD.  ADDED SUPPORT FOR *ELUGCLDA
00054 *                       IDGD AND IPGP TABULARS.  ALSO CHANGED    *ELUGCLDA
00055 *                       LINKS TO CALLS.                          *ELUGCLDA
00056 *                                                                *ELUGCLDA
00057 * 02.02 28-AUG-2000 AKK ADD SUPORT FOR #IPGS                     *ELUGCLDA
00058 *                                                                *ELUGCLDA
00059 * 02.03 25-SEP-2000 JP  CODING TO LOAD NEW ATBL FIELDS TO        *ELUGCLDA
00060 *                       TO SUPPORT REALMED.                      *ELUGCLDA
00061 *                                                                *ELUGCLDA
00062 * 02.04 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELUGCLDA
00063 *                                                                *ELUGCLDA
00064 * 02.05 09-JAN-2004 AKK S0C7 INTERTEST                           *ELUGCLDA
00065 *                                                                *ELUGCLDA
00066 * 02.06 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *ELUGCLDA
00067 *                                                                *ELUGCLDA
00068 * 03.01 13-JUL-2005 AKK REGEN FOR INTERTEST                      *ELUGCLDA
00069 *                                                                *ELUGCLDA
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00070 *                                                                *ELUGCLDA
00070 ******************************************************************ELUGCLDA
00071 /                                                                 ELUGCLDA
00072  DATA DIVISION.                                                   ELUGCLDA
00073                                                                   ELUGCLDA
00074  FILE SECTION.                                                    ELUGCLDA
00075                                                                   ELUGCLDA
00076  WORKING-STORAGE SECTION.                                         ELUGCLDA
00077                                                                   ELUGCLDA
00078  01  PROGRAM-CONSTANTS.                                           ELUGCLDA
00079      05  PC-ABM                  PICTURE  X(06) VALUE '#ABM  '.   ELUGCLDA
00080      05  PC-ACL                  PICTURE  X(06) VALUE '#ACL  '.   ELUGCLDA
00081      05  PC-ADL                  PICTURE  X(06) VALUE '#ADL  '.   ELUGCLDA
00082      05  PC-AOL                  PICTURE  X(06) VALUE '#AOL  '.   ELUGCLDA
00083      05  PC-ACP                  PICTURE  X(06) VALUE '#ACP  '.   ELUGCLDA
00084      05  PC-IBGR                 PICTURE  X(06) VALUE '#IBGR '.   ELUGCLDA
00085      05  PC-IDGD                 PICTURE  X(06) VALUE '#IDGD '.   ELUGCLDA
00086      05  PC-IPGN                 PICTURE  X(06) VALUE '#IPGN '.   ELUGCLDA
00087      05  PC-IPGP                 PICTURE  X(06) VALUE '#IPGP '.   ELUGCLDA
00088      05  PC-IPGT                 PICTURE  X(06) VALUE '#IPGT '.   ELUGCLDA
00089      05  PC-IPGS                 PICTURE  X(06) VALUE '#IPGS '.   ELUGCLDA
00090                                                                   ELUGCLDA
00091   01  WS-WORK-AREAS.                                              ELUGCLDA
00092       02 WS-MAX-TABULARS         PICTURE S9(04)          COMP     ELUGCLDA
00093                                  VALUE +18.                       ELUGCLDA
00094       02 WS-SAVE-SLOT-NBR        PICTURE S9(07)          COMP-3.  ELUGCLDA
00095       02 SLOT-FOUND-SW           PICTURE  X(01).                  ELUGCLDA
00096          88 SLOT-FOUND           VALUE 'D'.                       ELUGCLDA
00097          88 SLOT-NOT-FOUND       VALUE 'N'.                       ELUGCLDA
00098                                                                   ELUGCLDA
00099  01  WS-ACCUM-SLOT-NBR-TBLS.                                      ELUGCLDA
00100      02 WS-HOLD-ID-SUB           PICTURE S9(04)          COMP.    ELUGCLDA
00101         88 WS-HOLD-ABM-ID        VALUE +1.                        ELUGCLDA
00102         88 WS-HOLD-ACL-ID        VALUE +2.                        ELUGCLDA
00103         88 WS-HOLD-ADL-ID        VALUE +3.                        ELUGCLDA
00104         88 WS-HOLD-AOL-ID        VALUE +4.                        ELUGCLDA
00105         88 WS-HOLD-ACP-ID        VALUE +5.                        ELUGCLDA
00106      02 WS-HOLD-SLOT-SUB         PICTURE S9(04)          COMP.    ELUGCLDA
00107      02 WS-HOLD-TBLS.                                             ELUGCLDA
00108         03 WS-HOLD-ABM-TBL.                                       ELUGCLDA
00109            04 WS-NBR-ABM-SLOTS   PICTURE S9(04)          COMP.    ELUGCLDA
00110            04 WS-HOLD-ABM        OCCURS 5 TIMES.                  ELUGCLDA
00111               05 WS-HOLD-ABM-SLOT-NBR                             ELUGCLDA
00112                                  PICTURE S9(07)          COMP-3.  ELUGCLDA
00113         03 WS-HOLD-ACL-TBL.                                       ELUGCLDA
00114            04 WS-NBR-ACL-SLOTS   PICTURE S9(04)          COMP.    ELUGCLDA
00115            04 WS-HOLD-ACL        OCCURS 5 TIMES.                  ELUGCLDA
00116               05 WS-HOLD-ACL-SLOT-NBR                             ELUGCLDA
00117                                  PICTURE S9(07)          COMP-3.  ELUGCLDA
00118         03 WS-HOLD-ADL-TBL.                                       ELUGCLDA
00119            04 WS-NBR-ADL-SLOTS   PICTURE S9(04)          COMP.    ELUGCLDA
00120            04 WS-HOLD-ADL        OCCURS 5 TIMES.                  ELUGCLDA
00121               05 WS-HOLD-ADL-SLOT-NBR                             ELUGCLDA
00122                                  PICTURE S9(07)          COMP-3.  ELUGCLDA
00123         03 WS-HOLD-AOL-TBL.                                       ELUGCLDA
00124            04 WS-NBR-AOL-SLOTS   PICTURE S9(04)          COMP.    ELUGCLDA
00125            04 WS-HOLD-AOL        OCCURS 5 TIMES.                  ELUGCLDA
00126               05 WS-HOLD-AOL-SLOT-NBR                             ELUGCLDA
00127                                  PICTURE S9(07)          COMP-3.  ELUGCLDA
00128         03 WS-HOLD-ACP-TBL.                                       ELUGCLDA
00129            04 WS-NBR-ACP-SLOTS   PICTURE S9(04)          COMP.    ELUGCLDA
00130            04 WS-HOLD-ACP        OCCURS 5 TIMES.                  ELUGCLDA
00131               05 WS-HOLD-ACP-SLOT-NBR                             ELUGCLDA
00132                                  PICTURE S9(07)          COMP-3.  ELUGCLDA
00133      02 WS-HOLD-TBLS-ARRAY       REDEFINES WS-HOLD-TBLS.          ELUGCLDA
00134         03 WS-HOLD-TBL           OCCURS 5 TIMES.                  ELUGCLDA
00135            04 WS-NBR-SLOTS       PICTURE S9(04)          COMP.    ELUGCLDA
00136            04 WS-HOLD            OCCURS 5 TIMES.                  ELUGCLDA
00137               05 WS-HOLD-SLOT-NBR                                 ELUGCLDA
00138                                  PICTURE S9(07)          COMP-3.  ELUGCLDA
00139                                                                   ELUGCLDA
00140                                                                   ELUGCLDA
00141 **** THE FOLLOWING SECTION WAS ADDED TO SUPPORT REALMED           ELUGCLDA
00142  01  WS-ACCUM-COMMON-FIELDS.                                      ELUGCLDA
00143      05 WS-GROUP-NBR                PIC X(09).                    ELUGCLDA
00144      05 WS-SECTION-NBR              PIC X(05).                    ELUGCLDA
00145      05 WS-GS-CON-FEAK-INDICATORS.                                ELUGCLDA
00146         10  WS-GS-CON-FEAK-IND-ABM  PIC X(01).                    ELUGCLDA
00147         10  WS-GS-CON-FEAK-IND-ACL  PIC X(01).                    ELUGCLDA
00148         10  WS-GS-CON-FEAK-IND-ADL  PIC X(01).                    ELUGCLDA
00149         10  WS-GS-CON-FEAK-IND-AOL  PIC X(01).                    ELUGCLDA
00150         10  WS-GS-CON-FEAK-IND-ACP  PIC X(01).                    ELUGCLDA
00151      05 WS-GS-PSEU-NBR-USING-INDS.                                ELUGCLDA
00152         10  WS-GS-PSEU-NBR-IND-ABM  PIC X(01).                    ELUGCLDA
00153         10  WS-GS-PSEU-NBR-IND-ACL  PIC X(01).                    ELUGCLDA
00154         10  WS-GS-PSEU-NBR-IND-ADL  PIC X(01).                    ELUGCLDA
00155         10  WS-GS-PSEU-NBR-IND-AOL  PIC X(01).                    ELUGCLDA
00156         10  WS-GS-PSEU-NBR-IND-ACP  PIC X(01).                    ELUGCLDA
00157      05 WS-GS-PSEUDO-GROUP-NBR      PIC X(09).                    ELUGCLDA
00158      05 WS-GS-PSEUDO-SECTION-NBR    PIC X(05).                    ELUGCLDA
00159      05 WS-CONTRACT-BGN-DT-MMDD     PIC X(04).                    ELUGCLDA
00160                                                                   ELUGCLDA
00161 /                                                                 ELUGCLDA
00162  LINKAGE SECTION.                                                 ELUGCLDA
00163  01  DFHCOMMAREA.                                                 ELUGCLDA
00164      COPY ELSCOMMC.                                               ELUGCLDA
00165 /                                                                 ELUGCLDA
00166      COPY ELSCIA2C.                                               ELUGCLDA
00167 /                                                                 ELUGCLDA
00168      COPY ELSSSCBC.                                               ELUGCLDA
00169 /                                                                 ELUGCLDA
00170      COPY ELSIOPMC.                                               ELUGCLDA
00171 /                                                                 ELUGCLDA
00172      COPY ELSKEYSC.                                               ELUGCLDA
00173 /                                                                 ELUGCLDA
00174      COPY ELSCSACC.                                               ELUGCLDA
00175 /                                                                 ELUGCLDA
00176      COPY ELSATBLC.                                               ELUGCLDA
00177 /                                                                 ELUGCLDA
00178      COPY ELSIBGRC.                                               ELUGCLDA
00179 /                                                                 ELUGCLDA
00180      COPY ELSIDGDC.                                               ELUGCLDA
00181 /                                                                 ELUGCLDA
00182      COPY ELSIPGNC.                                               ELUGCLDA
00183 /                                                                 ELUGCLDA
00184      COPY ELSIPGPC.                                               ELUGCLDA
00185 /                                                                 ELUGCLDA
00186      COPY ELSIPGTC.                                               ELUGCLDA
00187 /                                                                 ELUGCLDA
00188      COPY ELSIPGSC.                                               ELUGCLDA
00189 /                                                                 ELUGCLDA
00190  01  GROUP-SPECIFIC-RECORD.                                       ELUGCLDA
00191      COPY GCGROUPC.                                               ELUGCLDA
00192 /                                                                 ELUGCLDA
00193  01  CONTRACT-RECORD.                                             ELUGCLDA
00194      COPY GCCONTRC.                                               ELUGCLDA
00195 /                                                                 ELUGCLDA
00196  01  MAXIMUM-RECORD.                                              ELUGCLDA
00197      COPY GCTABMC.                                                ELUGCLDA
00198 /                                                                 ELUGCLDA
00199  01  COINSURANCE-RECORD.                                          ELUGCLDA
00200      COPY GCTACLC.                                                ELUGCLDA
00201 /                                                                 ELUGCLDA
00202  01  DEDUCTIBLE-RECORD.                                           ELUGCLDA
00203      COPY GCTADLC.                                                ELUGCLDA
00204 /                                                                 ELUGCLDA
00205  01  OPX-RECORD.                                                  ELUGCLDA
00206      COPY GCTAOLC.                                                ELUGCLDA
00207 /                                                                 ELUGCLDA
00208  01  COPAY-RECORD.                                                ELUGCLDA
00209      COPY GCTACPC.                                                ELUGCLDA
00210 /*****************************************************************ELUGCLDA
00211 *                                                                *ELUGCLDA
00212 *    BUILD TABLE OF COMBINED GROUP SPECIFIC AND CONTRACT LEVEL   *ELUGCLDA
00213 *    ACCUMULATORS FOR CONTRACT SUMMARY PROCESSING                *ELUGCLDA
00214 *                                                                *ELUGCLDA
00215 *    PROCEDURE DIVISION                                          *ELUGCLDA
00216 *                                                                *ELUGCLDA
00217 ******************************************************************ELUGCLDA
00218                                                                   ELUGCLDA
00219  PROCEDURE DIVISION.                                              ELUGCLDA
00220                                                                   ELUGCLDA
00221  000-BLD-CMB-GS-C-ACCUM-TBL.                                      ELUGCLDA
00222      PERFORM 001-INITIALIZE.                                      ELUGCLDA
00223      PERFORM 100-PROCESS.                                         ELUGCLDA
00224      GOBACK.                                                      ELUGCLDA
00225                                                                   ELUGCLDA
00226 ******************************************************************ELUGCLDA
00227 *                                                                *ELUGCLDA
00228 *    INITIALIZE                                                  *ELUGCLDA
00229 *                                                                *ELUGCLDA
00230 ******************************************************************ELUGCLDA
00231                                                                   ELUGCLDA
00232  001-INITIALIZE.                                                  ELUGCLDA
00233      PERFORM 990-ESTAB-ECI-STG-ENVIRON.                           ELUGCLDA
00234      PERFORM 902-ESTAB-ADDR-ELSKEYS.                              ELUGCLDA
00235      PERFORM 904-ESTAB-ADDR-ELSSSCB.                              ELUGCLDA
00236      MOVE ZERO TO WS-HOLD-ID-SUB                                  ELUGCLDA
00237                   WS-HOLD-SLOT-SUB                                ELUGCLDA
00238                   WS-NBR-ABM-SLOTS                                ELUGCLDA
00239                   WS-NBR-ACL-SLOTS                                ELUGCLDA
00240                   WS-NBR-ADL-SLOTS                                ELUGCLDA
00241                   WS-NBR-AOL-SLOTS                                ELUGCLDA
00242                   WS-NBR-ACP-SLOTS.                               ELUGCLDA
00243                                                                   ELUGCLDA
00244 ******************************************************************ELUGCLDA
00245 *                                                                *ELUGCLDA
00246 *    PROCESS                                                     *ELUGCLDA
00247 *                                                                *ELUGCLDA
00248 ******************************************************************ELUGCLDA
00249                                                                   ELUGCLDA
00250  100-PROCESS.                                                     ELUGCLDA
00251      PERFORM 800-ACQ-ACCUM-PTR-TBL.                               ELUGCLDA
00252      PERFORM 110-GET-ACCUM-SLOT-NBRS.                             ELUGCLDA
00253      PERFORM 200-LOAD-MAXIMUM.                                    ELUGCLDA
00254      PERFORM 300-LOAD-COINSURANCE.                                ELUGCLDA
00255      PERFORM 400-LOAD-DEDUCTIBLE.                                 ELUGCLDA
00256      PERFORM 500-LOAD-OUT-OF-POCKET.                              ELUGCLDA
00257      PERFORM 600-LOAD-COPAY.                                      ELUGCLDA
00258      PERFORM 700-LOAD-INTERNAL-TABULARS.                          ELUGCLDA
00259                                                                   ELUGCLDA
00260 ******************************************************************ELUGCLDA
00261 *                                                                *ELUGCLDA
00262 *    GET THE ACCUMULATOR SLOT NUMBERS FROM THE GROUP             *ELUGCLDA
00263 *    SPECIFIC AND CONTRACT RECORDS, AND STORE THEM IN            *ELUGCLDA
00264 *    A TABLE.  EACH SLOT NUMBER IS ONLY STORED ONCE.             *ELUGCLDA
00265 *                                                                *ELUGCLDA
00266 ******************************************************************ELUGCLDA
00267                                                                   ELUGCLDA
00268  110-GET-ACCUM-SLOT-NBRS.                                         ELUGCLDA
00269                                                                   ELUGCLDA
00270      PERFORM 905-ESTAB-ADDR-ELSGRPSP.                             ELUGCLDA
00271      PERFORM 115-LOAD-GS-COMMON-FIELDS                            ELUGCLDA
00272      PERFORM 111-GET-GS-ACCUM-SLOTS.                              ELUGCLDA
00273                                                                   ELUGCLDA
00274      PERFORM 906-ESTAB-ADDR-ELSCONIB.                             ELUGCLDA
00275      IF NOT CIA-RC-PTR-NULL                                       ELUGCLDA
00276      THEN                                                         ELUGCLDA
00277         PERFORM 112-GET-CONT-ACCUM-SLOTS.                         ELUGCLDA
00278                                                                   ELUGCLDA
00279      PERFORM 906-ESTAB-ADDR-ELSCONIS.                             ELUGCLDA
00280      IF NOT CIA-RC-PTR-NULL                                       ELUGCLDA
00281      THEN                                                         ELUGCLDA
00282         PERFORM 112-GET-CONT-ACCUM-SLOTS.                         ELUGCLDA
00283                                                                   ELUGCLDA
00284      PERFORM 906-ESTAB-ADDR-ELSCONPB.                             ELUGCLDA
00285      IF NOT CIA-RC-PTR-NULL                                       ELUGCLDA
00286      THEN                                                         ELUGCLDA
00287         PERFORM 112-GET-CONT-ACCUM-SLOTS.                         ELUGCLDA
00288                                                                   ELUGCLDA
00289      PERFORM 906-ESTAB-ADDR-ELSCONPS.                             ELUGCLDA
00290      IF NOT CIA-RC-PTR-NULL                                       ELUGCLDA
00291      THEN                                                         ELUGCLDA
00292         PERFORM 112-GET-CONT-ACCUM-SLOTS.                         ELUGCLDA
00293                                                                   ELUGCLDA
00294 ******************************************************************ELUGCLDA
00295 *                                                                *ELUGCLDA
00296 *    GET THE GROUP SPECIFIC LEVEL ACCUMULATOR SLOT NUMBERS       *ELUGCLDA
00297 *                                                                *ELUGCLDA
00298 ******************************************************************ELUGCLDA
00299                                                                   ELUGCLDA
00300  111-GET-GS-ACCUM-SLOTS.                                          ELUGCLDA
00301      PERFORM WITH TEST BEFORE                                     ELUGCLDA
00302         VARYING GCG-INDEX FROM 1 BY 1                             ELUGCLDA
00303           UNTIL GCG-INDEX > GCG-COUNT-TAB-PROVN-POINTERS          ELUGCLDA
00304         IF GCG-TAB-ID (GCG-INDEX) NOT = HIGH-VALUES               ELUGCLDA
00305         THEN                                                      ELUGCLDA
00306            IF GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZEROES            ELUGCLDA
00307            THEN                                                   ELUGCLDA
00308               PERFORM 114-MOVE-GCG-TAB-AND-EVALUATE               ELUGCLDA
00309            ELSE                                                   ELUGCLDA
00310               CONTINUE                                            ELUGCLDA
00311            END-IF                                                 ELUGCLDA
00312         ELSE                                                      ELUGCLDA
00313            CONTINUE                                               ELUGCLDA
00314         END-IF                                                    ELUGCLDA
00315         END-PERFORM.                                              ELUGCLDA
00316                                                                   ELUGCLDA
00317 ******************************************************************ELUGCLDA
00318 *                                                                *ELUGCLDA
00319 *    GET THE CONTRACT LEVEL ACCUMULATOR SLOT NUMBERS             *ELUGCLDA
00320 *                                                                *ELUGCLDA
00321 ******************************************************************ELUGCLDA
00322                                                                   ELUGCLDA
00323  112-GET-CONT-ACCUM-SLOTS.                                        ELUGCLDA
00324                                                                   ELUGCLDA
00325 ** FOLLOWING CONTRACT FIELD IS SAVED IN WS FOR REALMED PROCESSING ELUGCLDA
00326      IF GCT-CNTRCT-YEAR-BGN-DT-MMDD NOT = SPACES                  ELUGCLDA
00327       MOVE GCT-CNTRCT-YEAR-BGN-DT-MMDD TO WS-CONTRACT-BGN-DT-MMDD ELUGCLDA
00328      END-IF                                                       ELUGCLDA
00329                                                                   ELUGCLDA
00330      PERFORM WITH TEST BEFORE                                     ELUGCLDA
00331         VARYING GCT-TAB-INDEX FROM 1 BY 1                         ELUGCLDA
00332           UNTIL GCT-TAB-INDEX > WS-MAX-TABULARS                   ELUGCLDA
00333         IF GCT-CON-TAB-ID (GCT-TAB-INDEX) NOT = HIGH-VALUES       ELUGCLDA
00334         THEN                                                      ELUGCLDA
00335            IF GCT-CON-TAB-SLOT (GCT-TAB-INDEX) NOT = ZEROES       ELUGCLDA
00336            THEN                                                   ELUGCLDA
00337               PERFORM 113-MOVE-GCT-TAB-AND-EVALUATE               ELUGCLDA
00338            ELSE                                                   ELUGCLDA
00339               CONTINUE                                            ELUGCLDA
00340            END-IF                                                 ELUGCLDA
00341         ELSE                                                      ELUGCLDA
00342            CONTINUE                                               ELUGCLDA
00343         END-IF                                                    ELUGCLDA
00344         END-PERFORM.                                              ELUGCLDA
00345                                                                   ELUGCLDA
00346 ******************************************************************ELUGCLDA
00347 *                                                                *ELUGCLDA
00348 *    MOVE CONTRACT TABULAR AND EVALUATE IT FOR USE               *ELUGCLDA
00349 *                                                                *ELUGCLDA
00350 ******************************************************************ELUGCLDA
00351                                                                   ELUGCLDA
00352  113-MOVE-GCT-TAB-AND-EVALUATE.                                   ELUGCLDA
00353      MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)                        ELUGCLDA
00354           TO WS-SAVE-SLOT-NBR.                                    ELUGCLDA
00355      EVALUATE GCT-CON-TAB-ID (GCT-TAB-INDEX)                      ELUGCLDA
00356           WHEN PC-ABM                                             ELUGCLDA
00357              SET WS-HOLD-ABM-ID TO TRUE                           ELUGCLDA
00358              PERFORM 121-STORE-ACCUM-SLOT-NBR                     ELUGCLDA
00359           WHEN PC-ACL                                             ELUGCLDA
00360              SET WS-HOLD-ACL-ID TO TRUE                           ELUGCLDA
00361              PERFORM 121-STORE-ACCUM-SLOT-NBR                     ELUGCLDA
00362           WHEN PC-ADL                                             ELUGCLDA
00363              SET WS-HOLD-ADL-ID TO TRUE                           ELUGCLDA
00364              PERFORM 121-STORE-ACCUM-SLOT-NBR                     ELUGCLDA
00365           WHEN PC-AOL                                             ELUGCLDA
00366              SET WS-HOLD-AOL-ID TO TRUE                           ELUGCLDA
00367              PERFORM 121-STORE-ACCUM-SLOT-NBR                     ELUGCLDA
00368           WHEN PC-ACP                                             ELUGCLDA
00369              SET WS-HOLD-ACP-ID TO TRUE                           ELUGCLDA
00370              PERFORM 121-STORE-ACCUM-SLOT-NBR                     ELUGCLDA
00371           WHEN OTHER                                              ELUGCLDA
00372              CONTINUE                                             ELUGCLDA
00373           END-EVALUATE.                                           ELUGCLDA
00374                                                                   ELUGCLDA
00375 ******************************************************************ELUGCLDA
00376 *                                                                *ELUGCLDA
00377 *    MOVE GROUP SPECIFIC TABULARE AND EVALUATE IT FOR USE        *ELUGCLDA
00378 *                                                                *ELUGCLDA
00379 ******************************************************************ELUGCLDA
00380                                                                   ELUGCLDA
00381  114-MOVE-GCG-TAB-AND-EVALUATE.                                   ELUGCLDA
00382      MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO WS-SAVE-SLOT-NBR.        ELUGCLDA
00383      EVALUATE GCG-TAB-ID (GCG-INDEX)                              ELUGCLDA
00384         WHEN PC-ABM                                               ELUGCLDA
00385            SET WS-HOLD-ABM-ID TO TRUE                             ELUGCLDA
00386            PERFORM 121-STORE-ACCUM-SLOT-NBR                       ELUGCLDA
00387         WHEN PC-ACL                                               ELUGCLDA
00388            SET WS-HOLD-ACL-ID TO TRUE                             ELUGCLDA
00389            PERFORM 121-STORE-ACCUM-SLOT-NBR                       ELUGCLDA
00390         WHEN PC-ADL                                               ELUGCLDA
00391            SET WS-HOLD-ADL-ID TO TRUE                             ELUGCLDA
00392            PERFORM 121-STORE-ACCUM-SLOT-NBR                       ELUGCLDA
00393         WHEN PC-AOL                                               ELUGCLDA
00394            SET WS-HOLD-AOL-ID TO TRUE                             ELUGCLDA
00395            PERFORM 121-STORE-ACCUM-SLOT-NBR                       ELUGCLDA
00396         WHEN PC-ACP                                               ELUGCLDA
00397            SET WS-HOLD-ACP-ID TO TRUE                             ELUGCLDA
00398            PERFORM 121-STORE-ACCUM-SLOT-NBR                       ELUGCLDA
00399         WHEN OTHER                                                ELUGCLDA
00400            CONTINUE                                               ELUGCLDA
00401      END-EVALUATE.                                                ELUGCLDA
00402                                                                   ELUGCLDA
00403 ******************************************************************ELUGCLDA
00404 *                                                                *ELUGCLDA
00405 *  LOAD GROUP SPECIFIC COMMON FIELDS INTO WORKING STORAGE        *ELUGCLDA
00406 *                                                                *ELUGCLDA
00407 ******************************************************************ELUGCLDA
00408  115-LOAD-GS-COMMON-FIELDS.                                       ELUGCLDA
00409                                                                   ELUGCLDA
00410      MOVE GCG-GROUP-NUM    TO   WS-GROUP-NBR                      ELUGCLDA
00411      MOVE GCG-SECTION-NUM  TO   WS-SECTION-NBR                    ELUGCLDA
00412                                                                   ELUGCLDA
00413      MOVE GCG-ACC-USG-CON-FEAKS-MAX-IND TO WS-GS-CON-FEAK-IND-ABM ELUGCLDA
00414      MOVE GCG-ACC-USG-CON-FEAKS-CO-IND  TO WS-GS-CON-FEAK-IND-ACL ELUGCLDA
00415      MOVE GCG-ACC-USG-CON-FEAKS-DED-IND TO WS-GS-CON-FEAK-IND-ADL ELUGCLDA
00416      MOVE GCG-ACC-USG-CON-FEAKS-OPX-IND TO WS-GS-CON-FEAK-IND-AOL ELUGCLDA
00417      MOVE GCG-ACC-USG-CON-FEAKS-COP-IND TO WS-GS-CON-FEAK-IND-ACP ELUGCLDA
00418                                                                   ELUGCLDA
00419      MOVE GCG-PSEU-NBR-USG-BEN-AGG-MAXM TO WS-GS-PSEU-NBR-IND-ABM ELUGCLDA
00420      MOVE GCG-PSEU-NBR-USG-COINS-LIMITS TO WS-GS-PSEU-NBR-IND-ACL ELUGCLDA
00421      MOVE GCG-PSEU-NBR-USG-DEDU-LIMITS  TO WS-GS-PSEU-NBR-IND-ADL ELUGCLDA
00422      MOVE GCG-PSEU-NBR-USG-OPX-LIMITS   TO WS-GS-PSEU-NBR-IND-AOL ELUGCLDA
00423      MOVE GCG-PSEU-NBR-USG-COPAY        TO WS-GS-PSEU-NBR-IND-ACP ELUGCLDA
00424                                                                   ELUGCLDA
00425      MOVE GCG-ACCUM-PSEUDO-GRP-NBR      TO WS-GS-PSEUDO-GROUP-NBR ELUGCLDA
00426      MOVE GCG-ACCUM-PSEUDO-SECTION-NBR  TO                        ELUGCLDA
00427          WS-GS-PSEUDO-SECTION-NBR.                                ELUGCLDA
00428                                                                   ELUGCLDA
00429                                                                   ELUGCLDA
00430 ******************************************************************ELUGCLDA
00431 *                                                                *ELUGCLDA
00432 *    STORE ACCUMULATOR TABULAR SLOT NUMBER IN HOLD TABLE         *ELUGCLDA
00433 *                                                                *ELUGCLDA
00434 ******************************************************************ELUGCLDA
00435                                                                   ELUGCLDA
00436  121-STORE-ACCUM-SLOT-NBR.                                        ELUGCLDA
00437      SET SLOT-NOT-FOUND TO TRUE.                                  ELUGCLDA
00438      PERFORM WITH TEST BEFORE                                     ELUGCLDA
00439         VARYING WS-HOLD-SLOT-SUB FROM 1 BY 1                      ELUGCLDA
00440           UNTIL      WS-HOLD-SLOT-SUB                             ELUGCLDA
00441                    > WS-NBR-SLOTS (WS-HOLD-ID-SUB)                ELUGCLDA
00442                 OR SLOT-FOUND                                     ELUGCLDA
00443         IF   WS-SAVE-SLOT-NBR                                     ELUGCLDA
00444            = WS-HOLD-SLOT-NBR (WS-HOLD-ID-SUB WS-HOLD-SLOT-SUB)   ELUGCLDA
00445         THEN                                                      ELUGCLDA
00446            SET SLOT-FOUND TO TRUE                                 ELUGCLDA
00447         ELSE                                                      ELUGCLDA
00448            CONTINUE                                               ELUGCLDA
00449         END-IF                                                    ELUGCLDA
00450         END-PERFORM.                                              ELUGCLDA
00451                                                                   ELUGCLDA
00452      IF SLOT-FOUND                                                ELUGCLDA
00453      THEN                                                         ELUGCLDA
00454         CONTINUE                                                  ELUGCLDA
00455      ELSE                                                         ELUGCLDA
00456         ADD 1 TO WS-NBR-SLOTS (WS-HOLD-ID-SUB)                    ELUGCLDA
00457         MOVE WS-NBR-SLOTS (WS-HOLD-ID-SUB) TO WS-HOLD-SLOT-SUB    ELUGCLDA
00458         MOVE WS-SAVE-SLOT-NBR                                     ELUGCLDA
00459           TO WS-HOLD-SLOT-NBR (WS-HOLD-ID-SUB WS-HOLD-SLOT-SUB).  ELUGCLDA
00460                                                                   ELUGCLDA
00461 /*****************************************************************ELUGCLDA
00462 *                                                                *ELUGCLDA
00463 *    LOAD MAXIMUMS ACCUMULATOR TABLE FOR CONTRACT SUMMARY        *ELUGCLDA
00464 *                                                                *ELUGCLDA
00465 ******************************************************************ELUGCLDA
00466                                                                   ELUGCLDA
00467  200-LOAD-MAXIMUM.                                                ELUGCLDA
00468      IF WS-NBR-ABM-SLOTS > 0                                      ELUGCLDA
00469      THEN                                                         ELUGCLDA
00470         PERFORM 820-CREATE-NEW-ATBL                               ELUGCLDA
00471         SET CSAC-ABM-GC-TBL-PTR                                   ELUGCLDA
00472          TO ADDRESS OF ATBL-ACCUMULATOR-TABLE                     ELUGCLDA
00473         MOVE ZERO TO ATBL-TBL-CNT                                 ELUGCLDA
00474         PERFORM 210-LOAD-MAXIMUM-ENTRIES                          ELUGCLDA
00475            VARYING WS-HOLD-SLOT-SUB FROM 1 BY 1                   ELUGCLDA
00476              UNTIL WS-HOLD-SLOT-SUB > WS-NBR-ABM-SLOTS.           ELUGCLDA
00477                                                                   ELUGCLDA
00478 ******************************************************************ELUGCLDA
00479 *                                                                *ELUGCLDA
00480 *    LOAD MAXIMUMS CONTRACT SUMMARY ACCUMULATOR TABLE            *ELUGCLDA
00481 *                                                                *ELUGCLDA
00482 ******************************************************************ELUGCLDA
00483                                                                   ELUGCLDA
00484  210-LOAD-MAXIMUM-ENTRIES.                                        ELUGCLDA
00485      MOVE PC-ABM TO KWA-PROVISION-ID.                             ELUGCLDA
00486      MOVE WS-HOLD-ABM-SLOT-NBR (WS-HOLD-SLOT-SUB)                 ELUGCLDA
00487        TO KWA-PROVISION-SLOT-NO.                                  ELUGCLDA
00488      PERFORM 810-READ-TABULAR-RECORD.                             ELUGCLDA
00489      SET ADDRESS OF MAXIMUM-RECORD TO IOP-REC-PTR.                ELUGCLDA
00490      PERFORM 211-COPY-ABM-TBL-VALUES                              ELUGCLDA
00491         VARYING GAA-INDEX FROM 1 BY 1                             ELUGCLDA
00492           UNTIL GAA-INDEX > GAA-ENTRY-COUNT.                      ELUGCLDA
00493                                                                   ELUGCLDA
00494 ******************************************************************ELUGCLDA
00495 *                                                                *ELUGCLDA
00496 *    COPY MAXIMUM TABLE VALUES INTO THE CONTRACT SUMMARY         *ELUGCLDA
00497 *    ACCUMULATOR TABLE                                           *ELUGCLDA
00498 *                                                                *ELUGCLDA
00499 ******************************************************************ELUGCLDA
00500                                                                   ELUGCLDA
00501  211-COPY-ABM-TBL-VALUES.                                         ELUGCLDA
00502      IF GAA-BAMA-BENEFIT-PERIOD (GAA-INDEX) = HIGH-VALUES         ELUGCLDA
00503      THEN                                                         ELUGCLDA
00504         CONTINUE                                                  ELUGCLDA
00505      ELSE                                                         ELUGCLDA
00506         ADD 1 TO ATBL-TBL-CNT                                     ELUGCLDA
00507         IF ATBL-TBL-FULL                                          ELUGCLDA
00508         THEN                                                      ELUGCLDA
00509            PERFORM 994-SIGNAL-INCR-TBL-SIZE                       ELUGCLDA
00510         ELSE                                                      ELUGCLDA
00511            SET ATBL-X-IDX TO ATBL-TBL-CNT                         ELUGCLDA
00512            PERFORM 212-LOAD-ABM-VALUES                            ELUGCLDA
00513            PERFORM 213-LOAD-ABM-INTERNAL-SLOTS.                   ELUGCLDA
00514                                                                   ELUGCLDA
00515 ******************************************************************ELUGCLDA
00516 *                                                                *ELUGCLDA
00517 *    LOAD MAXIMUM VALUES INTO THE CONTRACT SUMMARY ACCUMULATOR   *ELUGCLDA
00518 *    TABLE                                                       *ELUGCLDA
00519 *                                                                *ELUGCLDA
00520 ******************************************************************ELUGCLDA
00521                                                                   ELUGCLDA
00522  212-LOAD-ABM-VALUES.                                             ELUGCLDA
00523      MOVE GAA-PROVISION-SLOT-NO                                   ELUGCLDA
00524        TO ATBL-SLOT-NUMBER (ATBL-X-IDX).                          ELUGCLDA
00525      MOVE GAA-BAMA-BEN-PER-MAX-OVRD-IND (GAA-INDEX)               ELUGCLDA
00526        TO ATBL-BEN-PER-MAX-OVRD-IND (ATBL-X-IDX).                 ELUGCLDA
00527      MOVE GAA-BAMA-BEN-PER-TIME-FCTR (GAA-INDEX)                  ELUGCLDA
00528        TO ATBL-BEN-PER-TIME-FCTR (ATBL-X-IDX).                    ELUGCLDA
00529      MOVE GAA-BAMA-BEN-PER-TIME-QUAL (GAA-INDEX)                  ELUGCLDA
00530        TO ATBL-BEN-PER-TIME-QUAL (ATBL-X-IDX).                    ELUGCLDA
00531      MOVE GAA-BAMA-BENEFIT-PERIOD (GAA-INDEX)                     ELUGCLDA
00532        TO  ATBL-BENEFIT-PERIOD (ATBL-X-IDX).                      ELUGCLDA
00533      MOVE GAA-BAMA-CLAIM-LVL-ACCUM-IND (GAA-INDEX)                ELUGCLDA
00534        TO ATBL-CLAIM-LVL-ACCUM-IND (ATBL-X-IDX).                  ELUGCLDA
00535      MOVE GAA-BAMA-CO-PAY-IND (GAA-INDEX)                         ELUGCLDA
00536        TO ATBL-CO-PAY-IND (ATBL-X-IDX).                           ELUGCLDA
00537      MOVE GAA-BAMA-CONDITION (GAA-INDEX)                          ELUGCLDA
00538        TO ATBL-CONDITION (ATBL-X-IDX).                            ELUGCLDA
00539      MOVE GAA-BAMA-COST-CONTAIN-IND (GAA-INDEX)                   ELUGCLDA
00540        TO ATBL-COST-CONTAIN-IND (ATBL-X-IDX).                     ELUGCLDA
00541      MOVE GAA-BAMA-DAY-FACTOR-IND (GAA-INDEX)                     ELUGCLDA
00542        TO ATBL-DAY-FACTOR-IND (ATBL-X-IDX).                       ELUGCLDA
00543      MOVE GAA-BAMA-DEFINITION (GAA-INDEX)                         ELUGCLDA
00544        TO ATBL-DEFINITION (ATBL-X-IDX).                           ELUGCLDA
00545      MOVE GAA-BAMA-FAM-OR-INDIV (GAA-INDEX)                       ELUGCLDA
00546        TO ATBL-FAM-OR-INDIV (ATBL-X-IDX).                         ELUGCLDA
00547      MOVE GAA-BAMA-FYI-VALUE (GAA-INDEX)                          ELUGCLDA
00548        TO ATBL-FYI-VALUE (ATBL-X-IDX).                            ELUGCLDA
00549      MOVE GAA-BAMA-INTERNAL-DESCRIPTOR (GAA-INDEX)                ELUGCLDA
00550        TO ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX).                  ELUGCLDA
00551      MOVE GAA-BAMA-INTERVAL-OVRD-IND (GAA-INDEX)                  ELUGCLDA
00552        TO ATBL-INTERVAL-OVRD-IND (ATBL-X-IDX).                    ELUGCLDA
00553      MOVE GAA-BAMA-INTERVAL-OVRD-VALUE (GAA-INDEX)                ELUGCLDA
00554        TO ATBL-INTERVAL-OVRD-VALUE (ATBL-X-IDX).                  ELUGCLDA
00555      MOVE GAA-BAMA-INTERVAL-TIME-FCTR (GAA-INDEX)                 ELUGCLDA
00556        TO ATBL-INTERVAL-TIME-FCTR (ATBL-X-IDX).                   ELUGCLDA
00557      MOVE GAA-BAMA-INTERVAL-TYPE (GAA-INDEX)                      ELUGCLDA
00558        TO ATBL-INTERVAL-TYPE (ATBL-X-IDX).                        ELUGCLDA
00559      MOVE GAA-BAMA-INTERVAL-TYPE (GAA-INDEX)                      ELUGCLDA
00560        TO ATBL-INTERVAL-TYPE (ATBL-X-IDX).                        ELUGCLDA
00561      MOVE GAA-BAMA-L-O-B (GAA-INDEX)                              ELUGCLDA
00562        TO ATBL-L-O-B (ATBL-X-IDX).                                ELUGCLDA
00563      MOVE GAA-BAMA-PLACE-OF-TREATMENT (GAA-INDEX)                 ELUGCLDA
00564        TO ATBL-PLACE-OF-TREATMENT (ATBL-X-IDX).                   ELUGCLDA
00565      MOVE GAA-BAMA-REINSTATEMENT-IND (GAA-INDEX)                  ELUGCLDA
00566        TO ATBL-REINSTATEMENT-IND (ATBL-X-IDX).                    ELUGCLDA
00567      MOVE GAA-BAMA-SERVICE-GROUP (GAA-INDEX)                      ELUGCLDA
00568        TO ATBL-SERVICE-GROUP (ATBL-X-IDX).                        ELUGCLDA
00569      MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                        ELUGCLDA
00570        TO ATBL-VALUE-LIMIT (ATBL-X-IDX).                          ELUGCLDA
00571      MOVE GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX)                    ELUGCLDA
00572        TO ATBL-VALUE-QUALIFIER (ATBL-X-IDX).                      ELUGCLDA
00573                                                                   ELUGCLDA
00574 *THE FOLLOWING LOADS FIELDS ADDED TO FACILITATE REALMED PROCESSINGELUGCLDA
00575                                                                   ELUGCLDA
00576      MOVE PC-ABM TO ATBL-ACCUM-DESC (ATBL-X-IDX)                  ELUGCLDA
00577      MOVE WS-GS-CON-FEAK-IND-ABM TO                               ELUGCLDA
00578           ATBL-CON-FEAK-IND (ATBL-X-IDX)                          ELUGCLDA
00579      MOVE WS-GS-PSEU-NBR-IND-ABM TO                               ELUGCLDA
00580           ATBL-PSEU-NBR-USING-IND (ATBL-X-IDX)                    ELUGCLDA
00581      MOVE WS-GROUP-NBR     TO ATBL-GRP-NBR (ATBL-X-IDX)           ELUGCLDA
00582      MOVE WS-SECTION-NBR   TO ATBL-SECT-NBR (ATBL-X-IDX)          ELUGCLDA
00583      MOVE WS-GS-PSEUDO-GROUP-NBR    TO                            ELUGCLDA
00584           ATBL-PSEUDO-GRP-NBR (ATBL-X-IDX)                        ELUGCLDA
00585      MOVE WS-GS-PSEUDO-SECTION-NBR  TO                            ELUGCLDA
00586           ATBL-PSEUDO-SECT-NBR (ATBL-X-IDX)                       ELUGCLDA
00587      MOVE WS-CONTRACT-BGN-DT-MMDD   TO                            ELUGCLDA
00588           ATBL-CON-BGN-DT-MMDD (ATBL-X-IDX)                       ELUGCLDA
00589                                                                   ELUGCLDA
00590      MOVE GAA-BAMA-AGE-LIMIT-FROM (GAA-INDEX) TO                  ELUGCLDA
00591           ATBL-AGE-LIMIT-FROM (ATBL-X-IDX)                        ELUGCLDA
00592      MOVE GAA-BAMA-AGE-LIMIT-TO (GAA-INDEX) TO                    ELUGCLDA
00593           ATBL-AGE-LIMIT-TO (ATBL-X-IDX)                          ELUGCLDA
00594      MOVE GAA-BAMA-AGE-QUAL-IND-FROM (GAA-INDEX) TO               ELUGCLDA
00595           ATBL-AGE-QUAL-FROM (ATBL-X-IDX)                         ELUGCLDA
00596      MOVE GAA-BAMA-AGE-QUAL-IND-TO (GAA-INDEX) TO                 ELUGCLDA
00597           ATBL-AGE-QUAL-TO (ATBL-X-IDX).                          ELUGCLDA
00598                                                                   ELUGCLDA
00599 ******************************************************************ELUGCLDA
00600 *                                                                *ELUGCLDA
00601 *    LOAD MAXIMUM INTERAL TABULAR SLOT NUMBERS INTO THE CONTRACT *ELUGCLDA
00602 *    SUMMARY ACCUMULATOR TABLE                                   *ELUGCLDA
00603 *                                                                *ELUGCLDA
00604 ******************************************************************ELUGCLDA
00605                                                                   ELUGCLDA
00606  213-LOAD-ABM-INTERNAL-SLOTS.                                     ELUGCLDA
00607      INITIALIZE ATBL-INTERNAL-TABULARS (ATBL-X-IDX).              ELUGCLDA
00608      PERFORM WITH TEST BEFORE                                     ELUGCLDA
00609         VARYING GAA-INT-INDEX FROM 1 BY 1                         ELUGCLDA
00610           UNTIL   GAA-INT-INDEX                                   ELUGCLDA
00611                 > GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX)          ELUGCLDA
00612         IF GAA-INT-ID (GAA-INDEX GAA-INT-INDEX) = HIGH-VALUES     ELUGCLDA
00613         THEN                                                      ELUGCLDA
00614            MOVE ZERO TO WS-SAVE-SLOT-NBR                          ELUGCLDA
00615         ELSE                                                      ELUGCLDA
00616            MOVE GAA-INT-SLOT (GAA-INDEX GAA-INT-INDEX)            ELUGCLDA
00617              TO WS-SAVE-SLOT-NBR                                  ELUGCLDA
00618         END-IF                                                    ELUGCLDA
00619         EVALUATE GAA-INT-ID (GAA-INDEX GAA-INT-INDEX) ALSO TRUE   ELUGCLDA
00620            WHEN PC-IBGR ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
00621               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
00622                 TO ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
00623               PERFORM 831-ADD-IBGR-TO-TBL                         ELUGCLDA
00624            WHEN PC-IDGD ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
00625               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
00626                 TO ATBL-IDGD-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
00627               PERFORM 832-ADD-IDGD-TO-TBL                         ELUGCLDA
00628            WHEN PC-IPGN ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
00629               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
00630                 TO ATBL-IPGN-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
00631               PERFORM 833-ADD-IPGN-TO-TBL                         ELUGCLDA
00632            WHEN PC-IPGP ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
00633               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
00634                 TO ATBL-IPGP-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
00635               PERFORM 834-ADD-IPGP-TO-TBL                         ELUGCLDA
00636            WHEN PC-IPGT ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
00637               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
00638                 TO ATBL-IPGT-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
00639               PERFORM 835-ADD-IPGT-TO-TBL                         ELUGCLDA
00640            WHEN PC-IPGS ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
00641               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
00642                 TO ATBL-IPGS-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
00643               PERFORM 836-ADD-IPGS-TO-TBL                         ELUGCLDA
00644            WHEN OTHER                                             ELUGCLDA
00645               CONTINUE                                            ELUGCLDA
00646            END-EVALUATE                                           ELUGCLDA
00647         END-PERFORM.                                              ELUGCLDA
00648                                                                   ELUGCLDA
00649 /*****************************************************************ELUGCLDA
00650 *                                                                *ELUGCLDA
00651 *    LOAD COINSURANCES ACCUMULATOR TABLE FOR CONTRACT SUMMARY    *ELUGCLDA
00652 *                                                                *ELUGCLDA
00653 ******************************************************************ELUGCLDA
00654                                                                   ELUGCLDA
00655  300-LOAD-COINSURANCE.                                            ELUGCLDA
00656      IF WS-NBR-ACL-SLOTS > 0                                      ELUGCLDA
00657      THEN                                                         ELUGCLDA
00658         PERFORM 820-CREATE-NEW-ATBL                               ELUGCLDA
00659         SET CSAC-ACL-GC-TBL-PTR                                   ELUGCLDA
00660          TO ADDRESS OF ATBL-ACCUMULATOR-TABLE                     ELUGCLDA
00661         MOVE ZERO TO ATBL-TBL-CNT                                 ELUGCLDA
00662         PERFORM 310-LOAD-COINSURANCE-ENTRIES                      ELUGCLDA
00663            VARYING WS-HOLD-SLOT-SUB FROM 1 BY 1                   ELUGCLDA
00664              UNTIL WS-HOLD-SLOT-SUB > WS-NBR-ACL-SLOTS.           ELUGCLDA
00665                                                                   ELUGCLDA
00666 ******************************************************************ELUGCLDA
00667 *                                                                *ELUGCLDA
00668 *    LOAD COINSRUANCE CONTRACT SUMMARY ACCUMULATOR TABLE         *ELUGCLDA
00669 *                                                                *ELUGCLDA
00670 ******************************************************************ELUGCLDA
00671                                                                   ELUGCLDA
00672  310-LOAD-COINSURANCE-ENTRIES.                                    ELUGCLDA
00673      MOVE PC-ACL TO KWA-PROVISION-ID.                             ELUGCLDA
00674      MOVE WS-HOLD-ACL-SLOT-NBR (WS-HOLD-SLOT-SUB)                 ELUGCLDA
00675        TO KWA-PROVISION-SLOT-NO.                                  ELUGCLDA
00676      PERFORM 810-READ-TABULAR-RECORD.                             ELUGCLDA
00677      SET ADDRESS OF COINSURANCE-RECORD TO IOP-REC-PTR.            ELUGCLDA
00678      PERFORM 311-COPY-ACL-TBL-VALUES                              ELUGCLDA
00679         VARYING GAB-INDEX FROM 1 BY 1                             ELUGCLDA
00680           UNTIL GAB-INDEX > GAB-ENTRY-COUNT.                      ELUGCLDA
00681                                                                   ELUGCLDA
00682 ******************************************************************ELUGCLDA
00683 *                                                                *ELUGCLDA
00684 *    COPY COINSURANCE TABLE VALUES INTO THE CONTRACT SUMMARY     *ELUGCLDA
00685 *    ACCUMULATOR TABLE                                           *ELUGCLDA
00686 *                                                                *ELUGCLDA
00687 ******************************************************************ELUGCLDA
00688                                                                   ELUGCLDA
00689  311-COPY-ACL-TBL-VALUES.                                         ELUGCLDA
00690      IF GAB-COINS-BENEFIT-PERIOD ( GAB-INDEX) = HIGH-VALUES       ELUGCLDA
00691      THEN                                                         ELUGCLDA
00692         CONTINUE                                                  ELUGCLDA
00693      ELSE                                                         ELUGCLDA
00694         ADD 1 TO ATBL-TBL-CNT                                     ELUGCLDA
00695         IF ATBL-TBL-FULL                                          ELUGCLDA
00696         THEN                                                      ELUGCLDA
00697            PERFORM 994-SIGNAL-INCR-TBL-SIZE                       ELUGCLDA
00698         ELSE                                                      ELUGCLDA
00699            SET ATBL-X-IDX TO ATBL-TBL-CNT                         ELUGCLDA
00700            PERFORM 312-LOAD-ACL-VALUES                            ELUGCLDA
00701            PERFORM 313-LOAD-ACL-INTERNAL-SLOTS.                   ELUGCLDA
00702                                                                   ELUGCLDA
00703 ******************************************************************ELUGCLDA
00704 *                                                                *ELUGCLDA
00705 *    LOAD COINSURANCE VALUES INTO THE CONTRACT SUMMARY           *ELUGCLDA
00706 *    ACCUMULATOR TABLE                                           *ELUGCLDA
00707 *                                                                *ELUGCLDA
00708 ******************************************************************ELUGCLDA
00709                                                                   ELUGCLDA
00710  312-LOAD-ACL-VALUES.                                             ELUGCLDA
00711      MOVE GAB-PROVISION-SLOT-NO                                   ELUGCLDA
00712        TO ATBL-SLOT-NUMBER (ATBL-X-IDX).                          ELUGCLDA
00713      MOVE GAB-COINS-1ST-DOLR-COVRGE-LMT (GAB-INDEX)               ELUGCLDA
00714        TO ATBL-1ST-DOLR-COVRGE-LMT (ATBL-X-IDX).                  ELUGCLDA
00715      MOVE GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX)                ELUGCLDA
00716        TO ATBL-ASCEND-DESCEND-IND (ATBL-X-IDX).                   ELUGCLDA
00717      MOVE GAB-COINS-BEN-PER-TIME-FCTR (GAB-INDEX)                 ELUGCLDA
00718        TO ATBL-BEN-PER-TIME-FCTR (ATBL-X-IDX).                    ELUGCLDA
00719      MOVE GAB-COINS-BEN-PER-TIME-QUAL (GAB-INDEX)                 ELUGCLDA
00720        TO ATBL-BEN-PER-TIME-QUAL (ATBL-X-IDX).                    ELUGCLDA
00721      MOVE GAB-COINS-BENEFIT-PERIOD (GAB-INDEX)                    ELUGCLDA
00722        TO  ATBL-BENEFIT-PERIOD (ATBL-X-IDX).                      ELUGCLDA
00723      MOVE GAB-COINS-BISCENDING-IND (GAB-INDEX)                    ELUGCLDA
00724        TO ATBL-BISCEND-IND (ATBL-X-IDX).                          ELUGCLDA
00725      MOVE GAB-COINS-CLAIM-LVL-ACCUM-IND (GAB-INDEX)               ELUGCLDA
00726        TO ATBL-CLAIM-LVL-ACCUM-IND (ATBL-X-IDX).                  ELUGCLDA
00727      MOVE GAB-COINS-CO-PAY-IND (GAB-INDEX)                        ELUGCLDA
00728        TO ATBL-CO-PAY-IND (ATBL-X-IDX).                           ELUGCLDA
00729      MOVE GAB-COINS-CONDITION (GAB-INDEX)                         ELUGCLDA
00730        TO ATBL-CONDITION (ATBL-X-IDX).                            ELUGCLDA
00731      MOVE GAB-COINS-COST-CONTAIN-IND (GAB-INDEX)                  ELUGCLDA
00732        TO ATBL-COST-CONTAIN-IND (ATBL-X-IDX).                     ELUGCLDA
00733      MOVE GAB-COINS-DAY-FACTOR-IND (GAB-INDEX)                    ELUGCLDA
00734        TO ATBL-DAY-FACTOR-IND (ATBL-X-IDX).                       ELUGCLDA
00735      MOVE GAB-COINS-DEFINITION (GAB-INDEX)                        ELUGCLDA
00736        TO ATBL-DEFINITION (ATBL-X-IDX).                           ELUGCLDA
00737      MOVE GAB-COINS-FAM-OR-INDIV (GAB-INDEX)                      ELUGCLDA
00738        TO ATBL-FAM-OR-INDIV (ATBL-X-IDX).                         ELUGCLDA
00739      MOVE GAB-COINS-FYI-VALUE (GAB-INDEX)                         ELUGCLDA
00740        TO ATBL-FYI-VALUE (ATBL-X-IDX).                            ELUGCLDA
00741      MOVE GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX)               ELUGCLDA
00742        TO ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX).                  ELUGCLDA
00743      MOVE GAB-COINS-INTERVAL-OVRD-IND (GAB-INDEX)                 ELUGCLDA
00744        TO ATBL-INTERVAL-OVRD-IND (ATBL-X-IDX).                    ELUGCLDA
00745      MOVE GAB-COINS-INTERVAL-OVRD-VALUE (GAB-INDEX)               ELUGCLDA
00746        TO ATBL-INTERVAL-OVRD-VALUE (ATBL-X-IDX).                  ELUGCLDA
00747      MOVE GAB-COINS-INTERVAL-TIME-FCTR (GAB-INDEX)                ELUGCLDA
00748        TO ATBL-INTERVAL-TIME-FCTR (ATBL-X-IDX).                   ELUGCLDA
00749      MOVE GAB-COINS-INTERVAL-TYPE (GAB-INDEX)                     ELUGCLDA
00750        TO ATBL-INTERVAL-TYPE (ATBL-X-IDX).                        ELUGCLDA
00751      MOVE GAB-COINS-INTERVAL-TYPE (GAB-INDEX)                     ELUGCLDA
00752        TO ATBL-INTERVAL-TYPE (ATBL-X-IDX).                        ELUGCLDA
00753      MOVE GAB-COINS-L-O-B (GAB-INDEX)                             ELUGCLDA
00754        TO ATBL-L-O-B (ATBL-X-IDX).                                ELUGCLDA
00755      MOVE GAB-COINS-LMT-MANDATORY-IND (GAB-INDEX)                 ELUGCLDA
00756        TO ATBL-MANDATORY-IND (ATBL-X-IDX).                        ELUGCLDA
00757      MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)                     ELUGCLDA
00758        TO ATBL-PERCENT-LEVEL (ATBL-X-IDX).                        ELUGCLDA
00759      MOVE GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)                ELUGCLDA
00760        TO ATBL-PLACE-OF-TREATMENT (ATBL-X-IDX).                   ELUGCLDA
00761      MOVE GAB-COINS-REINSTATEMENT-IND (GAB-INDEX)                 ELUGCLDA
00762        TO ATBL-REINSTATEMENT-IND (ATBL-X-IDX).                    ELUGCLDA
00763      MOVE GAB-COINS-SERVICE-GROUP (GAB-INDEX)                     ELUGCLDA
00764        TO ATBL-SERVICE-GROUP (ATBL-X-IDX).                        ELUGCLDA
00765      MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                       ELUGCLDA
00766        TO ATBL-VALUE-LIMIT (ATBL-X-IDX).                          ELUGCLDA
00767      MOVE GAB-COINS-VALUE-QUALIFIER (GAB-INDEX)                   ELUGCLDA
00768        TO ATBL-VALUE-QUALIFIER (ATBL-X-IDX).                      ELUGCLDA
00769                                                                   ELUGCLDA
00770 *THE FOLLOWING LOADS FIELDS ADDED TO FACILITATE REALMED PROCESSINGELUGCLDA
00771                                                                   ELUGCLDA
00772      MOVE PC-ACL TO ATBL-ACCUM-DESC (ATBL-X-IDX)                  ELUGCLDA
00773      MOVE WS-GS-CON-FEAK-IND-ACL TO                               ELUGCLDA
00774           ATBL-CON-FEAK-IND (ATBL-X-IDX)                          ELUGCLDA
00775      MOVE WS-GS-PSEU-NBR-IND-ACL TO                               ELUGCLDA
00776           ATBL-PSEU-NBR-USING-IND (ATBL-X-IDX)                    ELUGCLDA
00777      MOVE WS-GROUP-NBR     TO ATBL-GRP-NBR (ATBL-X-IDX)           ELUGCLDA
00778      MOVE WS-SECTION-NBR   TO ATBL-SECT-NBR (ATBL-X-IDX)          ELUGCLDA
00779      MOVE WS-GS-PSEUDO-GROUP-NBR    TO                            ELUGCLDA
00780           ATBL-PSEUDO-GRP-NBR (ATBL-X-IDX)                        ELUGCLDA
00781      MOVE WS-GS-PSEUDO-SECTION-NBR  TO                            ELUGCLDA
00782           ATBL-PSEUDO-SECT-NBR (ATBL-X-IDX)                       ELUGCLDA
00783      MOVE WS-CONTRACT-BGN-DT-MMDD   TO                            ELUGCLDA
00784           ATBL-CON-BGN-DT-MMDD (ATBL-X-IDX)                       ELUGCLDA
00785                                                                   ELUGCLDA
00786      MOVE GAB-COINS-AGE-LIMIT-FROM (GAB-INDEX) TO                 ELUGCLDA
00787           ATBL-AGE-LIMIT-FROM (ATBL-X-IDX)                        ELUGCLDA
00788      MOVE GAB-COINS-AGE-LIMIT-TO (GAB-INDEX) TO                   ELUGCLDA
00789           ATBL-AGE-LIMIT-TO (ATBL-X-IDX)                          ELUGCLDA
00790      MOVE GAB-COINS-AGE-QUAL-IND-FROM (GAB-INDEX) TO              ELUGCLDA
00791           ATBL-AGE-QUAL-FROM (ATBL-X-IDX)                         ELUGCLDA
00792      MOVE GAB-COINS-AGE-QUAL-IND-TO (GAB-INDEX) TO                ELUGCLDA
00793           ATBL-AGE-QUAL-TO (ATBL-X-IDX).                          ELUGCLDA
00794                                                                   ELUGCLDA
00795 ******************************************************************ELUGCLDA
00796 *                                                                *ELUGCLDA
00797 *    LOAD COINSURANCE INTERNAL TABULAR SLOT NUMBERS INTO THE     *ELUGCLDA
00798 *    CONTRACT SUMMARY ACCUMULATOR TABLE                          *ELUGCLDA
00799 *                                                                *ELUGCLDA
00800 ******************************************************************ELUGCLDA
00801                                                                   ELUGCLDA
00802  313-LOAD-ACL-INTERNAL-SLOTS.                                     ELUGCLDA
00803      INITIALIZE ATBL-INTERNAL-TABULARS (ATBL-X-IDX).              ELUGCLDA
00804      PERFORM WITH TEST BEFORE                                     ELUGCLDA
00805         VARYING GAB-INT-INDEX FROM 1 BY 1                         ELUGCLDA
00806           UNTIL   GAB-INT-INDEX                                   ELUGCLDA
00807                 > GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX)          ELUGCLDA
00808         IF GAB-INT-ID (GAB-INDEX GAB-INT-INDEX) = HIGH-VALUES     ELUGCLDA
00809         THEN                                                      ELUGCLDA
00810            MOVE ZERO TO WS-SAVE-SLOT-NBR                          ELUGCLDA
00811         ELSE                                                      ELUGCLDA
00812            MOVE GAB-INT-SLOT (GAB-INDEX GAB-INT-INDEX)            ELUGCLDA
00813              TO WS-SAVE-SLOT-NBR                                  ELUGCLDA
00814         END-IF                                                    ELUGCLDA
00815         EVALUATE GAB-INT-ID (GAB-INDEX GAB-INT-INDEX) ALSO TRUE   ELUGCLDA
00816            WHEN PC-IBGR ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
00817               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
00818                 TO ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
00819               PERFORM 831-ADD-IBGR-TO-TBL                         ELUGCLDA
00820            WHEN PC-IDGD ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
00821               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
00822                 TO ATBL-IDGD-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
00823               PERFORM 832-ADD-IDGD-TO-TBL                         ELUGCLDA
00824            WHEN PC-IPGN ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
00825               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
00826                 TO ATBL-IPGN-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
00827               PERFORM 833-ADD-IPGN-TO-TBL                         ELUGCLDA
00828            WHEN PC-IPGP ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
00829               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
00830                 TO ATBL-IPGP-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
00831               PERFORM 834-ADD-IPGP-TO-TBL                         ELUGCLDA
00832            WHEN PC-IPGT ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
00833               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
00834                 TO ATBL-IPGT-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
00835               PERFORM 835-ADD-IPGT-TO-TBL                         ELUGCLDA
00836            WHEN PC-IPGS ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
00837               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
00838                 TO ATBL-IPGS-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
00839               PERFORM 836-ADD-IPGS-TO-TBL                         ELUGCLDA
00840            WHEN OTHER                                             ELUGCLDA
00841               CONTINUE                                            ELUGCLDA
00842            END-EVALUATE                                           ELUGCLDA
00843         END-PERFORM.                                              ELUGCLDA
00844                                                                   ELUGCLDA
00845 /*****************************************************************ELUGCLDA
00846 *                                                                *ELUGCLDA
00847 *    LOAD DEDUCTIBLES ACCUMULATOR TABLE FOR CONTRACT SUMMARY     *ELUGCLDA
00848 *                                                                *ELUGCLDA
00849 ******************************************************************ELUGCLDA
00850                                                                   ELUGCLDA
00851  400-LOAD-DEDUCTIBLE.                                             ELUGCLDA
00852      IF WS-NBR-ADL-SLOTS > 0                                      ELUGCLDA
00853      THEN                                                         ELUGCLDA
00854         PERFORM 820-CREATE-NEW-ATBL                               ELUGCLDA
00855         SET CSAC-ADL-GC-TBL-PTR                                   ELUGCLDA
00856          TO ADDRESS OF ATBL-ACCUMULATOR-TABLE                     ELUGCLDA
00857         MOVE ZERO TO ATBL-TBL-CNT                                 ELUGCLDA
00858         PERFORM 410-LOAD-DEDUCTIBLE-ENTRIES                       ELUGCLDA
00859            VARYING WS-HOLD-SLOT-SUB FROM 1 BY 1                   ELUGCLDA
00860              UNTIL WS-HOLD-SLOT-SUB > WS-NBR-ADL-SLOTS.           ELUGCLDA
00861                                                                   ELUGCLDA
00862 ******************************************************************ELUGCLDA
00863 *                                                                *ELUGCLDA
00864 *    LOAD DEDUCTIBLES CONTRACT SUMMARY ACCUMULATOR TABLE         *ELUGCLDA
00865 *                                                                *ELUGCLDA
00866 ******************************************************************ELUGCLDA
00867                                                                   ELUGCLDA
00868  410-LOAD-DEDUCTIBLE-ENTRIES.                                     ELUGCLDA
00869      MOVE PC-ADL TO KWA-PROVISION-ID.                             ELUGCLDA
00870      MOVE WS-HOLD-ADL-SLOT-NBR (WS-HOLD-SLOT-SUB)                 ELUGCLDA
00871        TO KWA-PROVISION-SLOT-NO.                                  ELUGCLDA
00872      PERFORM 810-READ-TABULAR-RECORD.                             ELUGCLDA
00873      SET ADDRESS OF DEDUCTIBLE-RECORD TO IOP-REC-PTR.             ELUGCLDA
00874      PERFORM 411-COPY-ADL-TBL-VALUES                              ELUGCLDA
00875         VARYING GAC-INDEX FROM 1 BY 1                             ELUGCLDA
00876           UNTIL GAC-INDEX > GAC-ENTRY-COUNT.                      ELUGCLDA
00877                                                                   ELUGCLDA
00878 ******************************************************************ELUGCLDA
00879 *                                                                *ELUGCLDA
00880 *    COPY DEDUCTIBLE TABLE VALUES INTO THE CONTRACT SUMMARY      *ELUGCLDA
00881 *    ACCUMULATOR TABLE                                           *ELUGCLDA
00882 *                                                                *ELUGCLDA
00883 ******************************************************************ELUGCLDA
00884                                                                   ELUGCLDA
00885  411-COPY-ADL-TBL-VALUES.                                         ELUGCLDA
00886      IF GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX) = HIGH-VALUES         ELUGCLDA
00887      THEN                                                         ELUGCLDA
00888         CONTINUE                                                  ELUGCLDA
00889      ELSE                                                         ELUGCLDA
00890         ADD 1 TO ATBL-TBL-CNT                                     ELUGCLDA
00891         IF ATBL-TBL-FULL                                          ELUGCLDA
00892         THEN                                                      ELUGCLDA
00893            PERFORM 994-SIGNAL-INCR-TBL-SIZE                       ELUGCLDA
00894         ELSE                                                      ELUGCLDA
00895            SET ATBL-X-IDX TO ATBL-TBL-CNT                         ELUGCLDA
00896            PERFORM 412-LOAD-ADL-VALUES                            ELUGCLDA
00897            PERFORM 413-LOAD-ADL-INTERNAL-SLOTS.                   ELUGCLDA
00898                                                                   ELUGCLDA
00899 ******************************************************************ELUGCLDA
00900 *                                                                *ELUGCLDA
00901 *    LOAD DEDUCTIBLE VALUES INTO THE CONTRACT SUMMARY            *ELUGCLDA
00902 *    ACCUMULATOR TABLE                                           *ELUGCLDA
00903 *                                                                *ELUGCLDA
00904 ******************************************************************ELUGCLDA
00905                                                                   ELUGCLDA
00906  412-LOAD-ADL-VALUES.                                             ELUGCLDA
00907      MOVE GAC-PROVISION-SLOT-NO                                   ELUGCLDA
00908        TO ATBL-SLOT-NUMBER (ATBL-X-IDX).                          ELUGCLDA
00909      MOVE GAC-DEDL-BEN-PER-TIME-FCTR (GAC-INDEX)                  ELUGCLDA
00910        TO ATBL-BEN-PER-TIME-FCTR (ATBL-X-IDX).                    ELUGCLDA
00911      MOVE GAC-DEDL-BEN-PER-TIME-QUAL (GAC-INDEX)                  ELUGCLDA
00912        TO ATBL-BEN-PER-TIME-QUAL (ATBL-X-IDX).                    ELUGCLDA
00913      MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)                     ELUGCLDA
00914        TO  ATBL-BENEFIT-PERIOD (ATBL-X-IDX).                      ELUGCLDA
00915      MOVE GAC-CARRY-OVER-CREDIT-IND (GAC-INDEX)                   ELUGCLDA
00916        TO  ATBL-CARRY-OVER-CREDIT-IND (ATBL-X-IDX).               ELUGCLDA
00917      MOVE GAC-DEDL-CLAIM-LVL-ACCUM-IND (GAC-INDEX)                ELUGCLDA
00918        TO ATBL-CLAIM-LVL-ACCUM-IND (ATBL-X-IDX).                  ELUGCLDA
00919      MOVE GAC-DEDL-CO-PAY-IND (GAC-INDEX)                         ELUGCLDA
00920        TO ATBL-CO-PAY-IND (ATBL-X-IDX).                           ELUGCLDA
00921      MOVE GAC-DEDL-CONDITION (GAC-INDEX)                          ELUGCLDA
00922        TO ATBL-CONDITION (ATBL-X-IDX).                            ELUGCLDA
00923      MOVE GAC-DEDL-COST-CONTAIN-IND (GAC-INDEX)                   ELUGCLDA
00924        TO ATBL-COST-CONTAIN-IND (ATBL-X-IDX).                     ELUGCLDA
00925      MOVE GAC-DEDL-DAY-FACTOR-IND (GAC-INDEX)                     ELUGCLDA
00926        TO ATBL-DAY-FACTOR-IND (ATBL-X-IDX).                       ELUGCLDA
00927      MOVE GAC-DEDL-DEFINITION (GAC-INDEX)                         ELUGCLDA
00928        TO ATBL-DEFINITION (ATBL-X-IDX).                           ELUGCLDA
00929      MOVE GAC-DEDL-FAM-OR-INDIV (GAC-INDEX)                       ELUGCLDA
00930        TO ATBL-FAM-OR-INDIV (ATBL-X-IDX).                         ELUGCLDA
00931      MOVE GAC-DEDL-FYI-VALUE (GAC-INDEX)                          ELUGCLDA
00932        TO ATBL-FYI-VALUE (ATBL-X-IDX).                            ELUGCLDA
00933      MOVE GAC-DEDL-INTERNAL-DESCRIPTOR (GAC-INDEX)                ELUGCLDA
00934        TO ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX).                  ELUGCLDA
00935      MOVE GAC-DEDL-INTERVAL-OVRD-IND (GAC-INDEX)                  ELUGCLDA
00936        TO ATBL-INTERVAL-OVRD-IND (ATBL-X-IDX).                    ELUGCLDA
00937      MOVE GAC-DEDL-INTERVAL-OVRD-VALUE (GAC-INDEX)                ELUGCLDA
00938        TO ATBL-INTERVAL-OVRD-VALUE (ATBL-X-IDX).                  ELUGCLDA
00939      MOVE GAC-DEDL-INTERVAL-TIME-FCTR (GAC-INDEX)                 ELUGCLDA
00940        TO ATBL-INTERVAL-TIME-FCTR (ATBL-X-IDX).                   ELUGCLDA
00941      MOVE GAC-DEDL-INTERVAL-TYPE (GAC-INDEX)                      ELUGCLDA
00942        TO ATBL-INTERVAL-TYPE (ATBL-X-IDX).                        ELUGCLDA
00943      MOVE GAC-DEDL-INTERVAL-TYPE (GAC-INDEX)                      ELUGCLDA
00944        TO ATBL-INTERVAL-TYPE (ATBL-X-IDX).                        ELUGCLDA
00945      MOVE GAC-DEDL-L-O-B (GAC-INDEX)                              ELUGCLDA
00946        TO ATBL-L-O-B (ATBL-X-IDX).                                ELUGCLDA
00947      MOVE GAC-DEDL-MANDATORY-IND (GAC-INDEX)                      ELUGCLDA
00948        TO ATBL-MANDATORY-IND (ATBL-X-IDX).                        ELUGCLDA
00949      MOVE GAC-DEDL-PLACE-OF-TREATMENT (GAC-INDEX)                 ELUGCLDA
00950        TO ATBL-PLACE-OF-TREATMENT (ATBL-X-IDX).                   ELUGCLDA
00951      MOVE GAC-DEDL-SERVICE-GROUP (GAC-INDEX)                      ELUGCLDA
00952        TO ATBL-SERVICE-GROUP (ATBL-X-IDX).                        ELUGCLDA
00953      MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                        ELUGCLDA
00954        TO ATBL-VALUE-LIMIT (ATBL-X-IDX).                          ELUGCLDA
00955      MOVE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)                    ELUGCLDA
00956        TO ATBL-VALUE-QUALIFIER (ATBL-X-IDX).                      ELUGCLDA
00957                                                                   ELUGCLDA
00958 *THE FOLLOWING LOADS FIELDS ADDED TO FACILITATE REALMED PROCESSINGELUGCLDA
00959 *                                                                 ELUGCLDA
00960      MOVE PC-ADL TO ATBL-ACCUM-DESC (ATBL-X-IDX)                  ELUGCLDA
00961      MOVE WS-GS-CON-FEAK-IND-ADL TO                               ELUGCLDA
00962           ATBL-CON-FEAK-IND (ATBL-X-IDX)                          ELUGCLDA
00963      MOVE WS-GS-PSEU-NBR-IND-ADL TO                               ELUGCLDA
00964           ATBL-PSEU-NBR-USING-IND (ATBL-X-IDX)                    ELUGCLDA
00965      MOVE WS-GROUP-NBR     TO ATBL-GRP-NBR (ATBL-X-IDX)           ELUGCLDA
00966      MOVE WS-SECTION-NBR   TO ATBL-SECT-NBR (ATBL-X-IDX)          ELUGCLDA
00967      MOVE WS-GS-PSEUDO-GROUP-NBR    TO                            ELUGCLDA
00968           ATBL-PSEUDO-GRP-NBR (ATBL-X-IDX)                        ELUGCLDA
00969      MOVE WS-GS-PSEUDO-SECTION-NBR  TO                            ELUGCLDA
00970           ATBL-PSEUDO-SECT-NBR (ATBL-X-IDX)                       ELUGCLDA
00971      MOVE WS-CONTRACT-BGN-DT-MMDD   TO                            ELUGCLDA
00972           ATBL-CON-BGN-DT-MMDD (ATBL-X-IDX)                       ELUGCLDA
00973                                                                   ELUGCLDA
00974      MOVE GAC-DEDL-AGE-LIMIT-FROM (GAC-INDEX) TO                  ELUGCLDA
00975           ATBL-AGE-LIMIT-FROM (ATBL-X-IDX)                        ELUGCLDA
00976      MOVE GAC-DEDL-AGE-LIMIT-TO (GAC-INDEX) TO                    ELUGCLDA
00977           ATBL-AGE-LIMIT-TO (ATBL-X-IDX)                          ELUGCLDA
00978      MOVE GAC-DEDL-AGE-QUAL-IND-FROM (GAC-INDEX) TO               ELUGCLDA
00979           ATBL-AGE-QUAL-FROM (ATBL-X-IDX)                         ELUGCLDA
00980      MOVE GAC-DEDL-AGE-QUAL-IND-TO (GAC-INDEX) TO                 ELUGCLDA
00981           ATBL-AGE-QUAL-TO (ATBL-X-IDX).                          ELUGCLDA
00982                                                                   ELUGCLDA
00983 ******************************************************************ELUGCLDA
00984 *                                                                *ELUGCLDA
00985 *    LOAD DEDUCTIBLE INTERAL TABULAR SLOT NUMBERS INTO THE       *ELUGCLDA
00986 *    CONTRACT SUMMARY ACCUMULATOR TABLE                          *ELUGCLDA
00987 *                                                                *ELUGCLDA
00988 ******************************************************************ELUGCLDA
00989                                                                   ELUGCLDA
00990  413-LOAD-ADL-INTERNAL-SLOTS.                                     ELUGCLDA
00991      INITIALIZE ATBL-INTERNAL-TABULARS (ATBL-X-IDX).              ELUGCLDA
00992      PERFORM WITH TEST BEFORE                                     ELUGCLDA
00993         VARYING GAC-INT-INDEX FROM 1 BY 1                         ELUGCLDA
00994           UNTIL   GAC-INT-INDEX                                   ELUGCLDA
00995                 > GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX)          ELUGCLDA
00996         IF GAC-INT-ID (GAC-INDEX GAC-INT-INDEX) = HIGH-VALUES     ELUGCLDA
00997         THEN                                                      ELUGCLDA
00998            MOVE ZERO TO WS-SAVE-SLOT-NBR                          ELUGCLDA
00999         ELSE                                                      ELUGCLDA
01000            MOVE GAC-INT-SLOT (GAC-INDEX GAC-INT-INDEX)            ELUGCLDA
01001              TO WS-SAVE-SLOT-NBR                                  ELUGCLDA
01002         END-IF                                                    ELUGCLDA
01003         EVALUATE GAC-INT-ID (GAC-INDEX GAC-INT-INDEX) ALSO TRUE   ELUGCLDA
01004            WHEN PC-IBGR ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01005               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01006                 TO ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01007               PERFORM 831-ADD-IBGR-TO-TBL                         ELUGCLDA
01008            WHEN PC-IDGD ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01009               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01010                 TO ATBL-IDGD-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01011               PERFORM 832-ADD-IDGD-TO-TBL                         ELUGCLDA
01012            WHEN PC-IPGN ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01013               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01014                 TO ATBL-IPGN-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01015               PERFORM 833-ADD-IPGN-TO-TBL                         ELUGCLDA
01016            WHEN PC-IPGP ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01017               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01018                 TO ATBL-IPGP-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01019               PERFORM 834-ADD-IPGP-TO-TBL                         ELUGCLDA
01020            WHEN PC-IPGT ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01021               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01022                 TO ATBL-IPGT-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01023               PERFORM 835-ADD-IPGT-TO-TBL                         ELUGCLDA
01024            WHEN PC-IPGS ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01025               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01026                 TO ATBL-IPGS-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01027               PERFORM 836-ADD-IPGS-TO-TBL                         ELUGCLDA
01028            WHEN OTHER                                             ELUGCLDA
01029               CONTINUE                                            ELUGCLDA
01030            END-EVALUATE                                           ELUGCLDA
01031         END-PERFORM.                                              ELUGCLDA
01032                                                                   ELUGCLDA
01033 /*****************************************************************ELUGCLDA
01034 *                                                                *ELUGCLDA
01035 *    LOAD OUT-OF-POCKET EXPENSE LIMITS ACCUMULATOR TABLE FOR     *ELUGCLDA
01036 *    CONTRACT SUMMARY                                            *ELUGCLDA
01037 *                                                                *ELUGCLDA
01038 ******************************************************************ELUGCLDA
01039                                                                   ELUGCLDA
01040  500-LOAD-OUT-OF-POCKET.                                          ELUGCLDA
01041      IF WS-NBR-AOL-SLOTS > 0                                      ELUGCLDA
01042      THEN                                                         ELUGCLDA
01043         PERFORM 820-CREATE-NEW-ATBL                               ELUGCLDA
01044         SET CSAC-AOL-GC-TBL-PTR                                   ELUGCLDA
01045          TO ADDRESS OF ATBL-ACCUMULATOR-TABLE                     ELUGCLDA
01046         MOVE ZERO TO ATBL-TBL-CNT                                 ELUGCLDA
01047         PERFORM 510-LOAD-OUT-OF-POCKET-ENTRIES                    ELUGCLDA
01048            VARYING WS-HOLD-SLOT-SUB FROM 1 BY 1                   ELUGCLDA
01049              UNTIL WS-HOLD-SLOT-SUB > WS-NBR-AOL-SLOTS.           ELUGCLDA
01050                                                                   ELUGCLDA
01051 ******************************************************************ELUGCLDA
01052 *                                                                *ELUGCLDA
01053 *    LOAD OUT-OF-POCKETS CONTRACT SUMMARY ACCUMULATOR TABLE      *ELUGCLDA
01054 *                                                                *ELUGCLDA
01055 ******************************************************************ELUGCLDA
01056                                                                   ELUGCLDA
01057  510-LOAD-OUT-OF-POCKET-ENTRIES.                                  ELUGCLDA
01058      MOVE PC-AOL TO KWA-PROVISION-ID.                             ELUGCLDA
01059      MOVE WS-HOLD-AOL-SLOT-NBR (WS-HOLD-SLOT-SUB)                 ELUGCLDA
01060        TO KWA-PROVISION-SLOT-NO.                                  ELUGCLDA
01061      PERFORM 810-READ-TABULAR-RECORD.                             ELUGCLDA
01062      SET ADDRESS OF OPX-RECORD TO IOP-REC-PTR.                    ELUGCLDA
01063      PERFORM 511-COPY-AOL-TBL-VALUES                              ELUGCLDA
01064         VARYING GAD-INDEX FROM 1 BY 1                             ELUGCLDA
01065           UNTIL GAD-INDEX > GAD-ENTRY-COUNT.                      ELUGCLDA
01066                                                                   ELUGCLDA
01067 ******************************************************************ELUGCLDA
01068 *                                                                *ELUGCLDA
01069 *    COPY OUT-OF-POCKET TABLE VALUES INTO THE CONTRACT SUMMARY   *ELUGCLDA
01070 *    ACCUMULATOR TABLE                                           *ELUGCLDA
01071 *                                                                *ELUGCLDA
01072 ******************************************************************ELUGCLDA
01073                                                                   ELUGCLDA
01074  511-COPY-AOL-TBL-VALUES.                                         ELUGCLDA
01075      IF GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX) = HIGH-VALUES        ELUGCLDA
01076      THEN                                                         ELUGCLDA
01077         CONTINUE                                                  ELUGCLDA
01078      ELSE                                                         ELUGCLDA
01079         ADD 1 TO ATBL-TBL-CNT                                     ELUGCLDA
01080         IF ATBL-TBL-FULL                                          ELUGCLDA
01081         THEN                                                      ELUGCLDA
01082            PERFORM 994-SIGNAL-INCR-TBL-SIZE                       ELUGCLDA
01083         ELSE                                                      ELUGCLDA
01084            SET ATBL-X-IDX TO ATBL-TBL-CNT                         ELUGCLDA
01085            PERFORM 512-LOAD-AOL-VALUES                            ELUGCLDA
01086            PERFORM 513-LOAD-AOL-INTERNAL-SLOTS.                   ELUGCLDA
01087                                                                   ELUGCLDA
01088 ******************************************************************ELUGCLDA
01089 *                                                                *ELUGCLDA
01090 *    LOAD OUT-OF-POCKET VALUES INTO THE CONTRACT SUMMARY         *ELUGCLDA
01091 *    ACCUMULATOR TABLE                                           *ELUGCLDA
01092 *                                                                *ELUGCLDA
01093 ******************************************************************ELUGCLDA
01094                                                                   ELUGCLDA
01095  512-LOAD-AOL-VALUES.                                             ELUGCLDA
01096      MOVE GAD-PROVISION-SLOT-NO                                   ELUGCLDA
01097        TO ATBL-SLOT-NUMBER (ATBL-X-IDX).                          ELUGCLDA
01098      MOVE GAD-O-P-X-ASCEND-DESCEND-IND(GAD-INDEX)                 ELUGCLDA
01099        TO ATBL-ASCEND-DESCEND-IND (ATBL-X-IDX).                   ELUGCLDA
01100      MOVE GAD-O-P-X-BEN-PER-TIME-FCTR (GAD-INDEX)                 ELUGCLDA
01101        TO ATBL-BEN-PER-TIME-FCTR (ATBL-X-IDX).                    ELUGCLDA
01102      MOVE GAD-O-P-X-BEN-PER-TIME-QUAL (GAD-INDEX)                 ELUGCLDA
01103        TO ATBL-BEN-PER-TIME-QUAL (ATBL-X-IDX).                    ELUGCLDA
01104      MOVE GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)                    ELUGCLDA
01105        TO  ATBL-BENEFIT-PERIOD (ATBL-X-IDX).                      ELUGCLDA
01106      MOVE GAD-O-P-X-CLAIM-LVL-ACCUM-IND (GAD-INDEX)               ELUGCLDA
01107        TO ATBL-CLAIM-LVL-ACCUM-IND (ATBL-X-IDX).                  ELUGCLDA
01108      MOVE GAD-O-P-X-CO-PAY-IND (GAD-INDEX)                        ELUGCLDA
01109        TO ATBL-CO-PAY-IND (ATBL-X-IDX).                           ELUGCLDA
01110      MOVE GAD-O-P-X-CONDITION (GAD-INDEX)                         ELUGCLDA
01111        TO ATBL-CONDITION (ATBL-X-IDX).                            ELUGCLDA
01112      MOVE GAD-O-P-X-COST-CONTAIN-IND (GAD-INDEX)                  ELUGCLDA
01113        TO ATBL-COST-CONTAIN-IND (ATBL-X-IDX).                     ELUGCLDA
01114      MOVE GAD-O-P-X-DAY-FACTOR-IND (GAD-INDEX)                    ELUGCLDA
01115        TO ATBL-DAY-FACTOR-IND (ATBL-X-IDX).                       ELUGCLDA
01116      MOVE GAD-O-P-X-DEFINITION (GAD-INDEX)                        ELUGCLDA
01117        TO ATBL-DEFINITION (ATBL-X-IDX).                           ELUGCLDA
01118      MOVE GAD-O-P-X-FAM-OR-INDIV (GAD-INDEX)                      ELUGCLDA
01119        TO ATBL-FAM-OR-INDIV (ATBL-X-IDX).                         ELUGCLDA
01120      MOVE GAD-O-P-X-FYI-VALUE (GAD-INDEX)                         ELUGCLDA
01121        TO ATBL-FYI-VALUE (ATBL-X-IDX).                            ELUGCLDA
01122      MOVE GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX)               ELUGCLDA
01123        TO ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX).                  ELUGCLDA
01124      MOVE GAD-O-P-X-INTERVAL-OVRD-IND (GAD-INDEX)                 ELUGCLDA
01125        TO ATBL-INTERVAL-OVRD-IND (ATBL-X-IDX).                    ELUGCLDA
01126      MOVE GAD-O-P-X-INTERVAL-OVRD-VALUE (GAD-INDEX)               ELUGCLDA
01127        TO ATBL-INTERVAL-OVRD-VALUE (ATBL-X-IDX).                  ELUGCLDA
01128      MOVE GAD-O-P-X-INTERVAL-TIME-FCTR (GAD-INDEX)                ELUGCLDA
01129        TO ATBL-INTERVAL-TIME-FCTR (ATBL-X-IDX).                   ELUGCLDA
01130      MOVE GAD-O-P-X-INTERVAL-TYPE (GAD-INDEX)                     ELUGCLDA
01131        TO ATBL-INTERVAL-TYPE (ATBL-X-IDX).                        ELUGCLDA
01132      MOVE GAD-O-P-X-INTERVAL-TYPE (GAD-INDEX)                     ELUGCLDA
01133        TO ATBL-INTERVAL-TYPE (ATBL-X-IDX).                        ELUGCLDA
01134      MOVE GAD-O-P-X-L-O-B (GAD-INDEX)                             ELUGCLDA
01135        TO ATBL-L-O-B (ATBL-X-IDX).                                ELUGCLDA
01136      MOVE GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX)                     ELUGCLDA
01137        TO ATBL-PERCENT-LEVEL (ATBL-X-IDX).                        ELUGCLDA
01138      MOVE GAD-O-P-X-PLACE-OF-TREATMENT (GAD-INDEX)                ELUGCLDA
01139        TO ATBL-PLACE-OF-TREATMENT (ATBL-X-IDX).                   ELUGCLDA
01140      MOVE GAD-O-P-X-SERVICE-GROUP (GAD-INDEX)                     ELUGCLDA
01141        TO ATBL-SERVICE-GROUP (ATBL-X-IDX).                        ELUGCLDA
01142      MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)                       ELUGCLDA
01143        TO ATBL-VALUE-LIMIT (ATBL-X-IDX).                          ELUGCLDA
01144      MOVE GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)                   ELUGCLDA
01145        TO ATBL-VALUE-QUALIFIER (ATBL-X-IDX).                      ELUGCLDA
01146                                                                   ELUGCLDA
01147 *THE FOLLOWING LOADS FIELDS ADDED TO FACILITATE REALMED PROCESSINGELUGCLDA
01148 *                                                                 ELUGCLDA
01149      MOVE PC-AOL TO ATBL-ACCUM-DESC (ATBL-X-IDX)                  ELUGCLDA
01150      MOVE WS-GS-CON-FEAK-IND-AOL TO                               ELUGCLDA
01151           ATBL-CON-FEAK-IND (ATBL-X-IDX)                          ELUGCLDA
01152      MOVE WS-GS-PSEU-NBR-IND-AOL TO                               ELUGCLDA
01153           ATBL-PSEU-NBR-USING-IND (ATBL-X-IDX)                    ELUGCLDA
01154      MOVE WS-GROUP-NBR     TO ATBL-GRP-NBR (ATBL-X-IDX)           ELUGCLDA
01155      MOVE WS-SECTION-NBR   TO ATBL-SECT-NBR (ATBL-X-IDX)          ELUGCLDA
01156      MOVE WS-GS-PSEUDO-GROUP-NBR    TO                            ELUGCLDA
01157           ATBL-PSEUDO-GRP-NBR (ATBL-X-IDX)                        ELUGCLDA
01158      MOVE WS-GS-PSEUDO-SECTION-NBR  TO                            ELUGCLDA
01159           ATBL-PSEUDO-SECT-NBR (ATBL-X-IDX)                       ELUGCLDA
01160      MOVE WS-CONTRACT-BGN-DT-MMDD   TO                            ELUGCLDA
01161           ATBL-CON-BGN-DT-MMDD (ATBL-X-IDX)                       ELUGCLDA
01162                                                                   ELUGCLDA
01163      MOVE GAD-O-P-X-AGE-LIMIT-FROM (GAD-INDEX) TO                 ELUGCLDA
01164           ATBL-AGE-LIMIT-FROM (ATBL-X-IDX)                        ELUGCLDA
01165      MOVE GAD-O-P-X-AGE-LIMIT-TO (GAD-INDEX) TO                   ELUGCLDA
01166           ATBL-AGE-LIMIT-TO (ATBL-X-IDX)                          ELUGCLDA
01167      MOVE GAD-O-P-X-AGE-QUAL-IND-FROM (GAD-INDEX) TO              ELUGCLDA
01168           ATBL-AGE-QUAL-FROM (ATBL-X-IDX)                         ELUGCLDA
01169      MOVE GAD-O-P-X-AGE-QUAL-IND-TO (GAD-INDEX) TO                ELUGCLDA
01170           ATBL-AGE-QUAL-TO (ATBL-X-IDX).                          ELUGCLDA
01171                                                                   ELUGCLDA
01172 ******************************************************************ELUGCLDA
01173 *                                                                *ELUGCLDA
01174 *    LOAD OUT-OF-POCKET  INTERNAL TABULAR SLOT NUMBERS INTO THE  *ELUGCLDA
01175 *    CONTRACT SUMMARY ACCUMULATOR TABLE                          *ELUGCLDA
01176 *                                                                *ELUGCLDA
01177 ******************************************************************ELUGCLDA
01178                                                                   ELUGCLDA
01179  513-LOAD-AOL-INTERNAL-SLOTS.                                     ELUGCLDA
01180      INITIALIZE ATBL-INTERNAL-TABULARS (ATBL-X-IDX).              ELUGCLDA
01181      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01182         VARYING GAD-INT-INDEX FROM 1 BY 1                         ELUGCLDA
01183           UNTIL   GAD-INT-INDEX                                   ELUGCLDA
01184                 > GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX)          ELUGCLDA
01185         IF GAD-INT-ID (GAD-INDEX GAD-INT-INDEX) = HIGH-VALUES     ELUGCLDA
01186         THEN                                                      ELUGCLDA
01187            MOVE ZERO TO WS-SAVE-SLOT-NBR                          ELUGCLDA
01188         ELSE                                                      ELUGCLDA
01189            MOVE GAD-INT-SLOT (GAD-INDEX GAD-INT-INDEX)            ELUGCLDA
01190              TO WS-SAVE-SLOT-NBR                                  ELUGCLDA
01191         END-IF                                                    ELUGCLDA
01192         EVALUATE GAD-INT-ID (GAD-INDEX GAD-INT-INDEX) ALSO TRUE   ELUGCLDA
01193            WHEN PC-IBGR ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01194               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01195                 TO ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01196               PERFORM 831-ADD-IBGR-TO-TBL                         ELUGCLDA
01197            WHEN PC-IDGD ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01198               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01199                 TO ATBL-IDGD-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01200               PERFORM 832-ADD-IDGD-TO-TBL                         ELUGCLDA
01201            WHEN PC-IPGN ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01202               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01203                 TO ATBL-IPGN-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01204               PERFORM 833-ADD-IPGN-TO-TBL                         ELUGCLDA
01205            WHEN PC-IPGP ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01206               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01207                 TO ATBL-IPGP-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01208               PERFORM 834-ADD-IPGP-TO-TBL                         ELUGCLDA
01209            WHEN PC-IPGT ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01210               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01211                 TO ATBL-IPGT-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01212               PERFORM 835-ADD-IPGT-TO-TBL                         ELUGCLDA
01213            WHEN PC-IPGS ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01214               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01215                 TO ATBL-IPGS-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01216               PERFORM 836-ADD-IPGS-TO-TBL                         ELUGCLDA
01217            WHEN OTHER                                             ELUGCLDA
01218               CONTINUE                                            ELUGCLDA
01219            END-EVALUATE                                           ELUGCLDA
01220         END-PERFORM.                                              ELUGCLDA
01221                                                                   ELUGCLDA
01222                                                                   ELUGCLDA
01223 /*****************************************************************ELUGCLDA
01224 *                                                                *ELUGCLDA
01225 *    LOAD CO-PAY ACCUMULATOR TABLE FOR CONTRACT SUMMARY          *ELUGCLDA
01226 *                                                                *ELUGCLDA
01227 ******************************************************************ELUGCLDA
01228                                                                   ELUGCLDA
01229  600-LOAD-COPAY.                                                  ELUGCLDA
01230      IF WS-NBR-ACP-SLOTS > 0                                      ELUGCLDA
01231      THEN                                                         ELUGCLDA
01232         PERFORM 820-CREATE-NEW-ATBL                               ELUGCLDA
01233         SET CSAC-ACP-GC-TBL-PTR                                   ELUGCLDA
01234          TO ADDRESS OF ATBL-ACCUMULATOR-TABLE                     ELUGCLDA
01235         MOVE ZERO TO ATBL-TBL-CNT                                 ELUGCLDA
01236         PERFORM 610-LOAD-COPAY-ENTRIES                            ELUGCLDA
01237            VARYING WS-HOLD-SLOT-SUB FROM 1 BY 1                   ELUGCLDA
01238              UNTIL WS-HOLD-SLOT-SUB > WS-NBR-ACP-SLOTS.           ELUGCLDA
01239                                                                   ELUGCLDA
01240 ******************************************************************ELUGCLDA
01241 *                                                                *ELUGCLDA
01242 *    LOAD CO-PAY CONTRACT SUMMARY ACCUMULATOR TABLE              *ELUGCLDA
01243 *                                                                *ELUGCLDA
01244 ******************************************************************ELUGCLDA
01245                                                                   ELUGCLDA
01246  610-LOAD-COPAY-ENTRIES.                                          ELUGCLDA
01247      MOVE PC-ACP TO KWA-PROVISION-ID.                             ELUGCLDA
01248      MOVE WS-HOLD-ACP-SLOT-NBR (WS-HOLD-SLOT-SUB)                 ELUGCLDA
01249        TO KWA-PROVISION-SLOT-NO.                                  ELUGCLDA
01250      PERFORM 810-READ-TABULAR-RECORD.                             ELUGCLDA
01251      SET ADDRESS OF COPAY-RECORD      TO IOP-REC-PTR.             ELUGCLDA
01252      PERFORM 611-COPY-ACP-TBL-VALUES                              ELUGCLDA
01253         VARYING GAF-INDEX FROM 1 BY 1                             ELUGCLDA
01254           UNTIL GAF-INDEX > GAF-ENTRY-COUNT.                      ELUGCLDA
01255                                                                   ELUGCLDA
01256 ******************************************************************ELUGCLDA
01257 *                                                                *ELUGCLDA
01258 *    COPY CO-PAY TABLE VALUES INTO THE CONTRACT SUMMARY          *ELUGCLDA
01259 *    ACCUMULATOR TABLE                                           *ELUGCLDA
01260 *                                                                *ELUGCLDA
01261 ******************************************************************ELUGCLDA
01262                                                                   ELUGCLDA
01263  611-COPY-ACP-TBL-VALUES.                                         ELUGCLDA
01264      IF GAF-COPAY-BENEFIT-PERIOD (GAF-INDEX) = HIGH-VALUES        ELUGCLDA
01265      THEN                                                         ELUGCLDA
01266         CONTINUE                                                  ELUGCLDA
01267      ELSE                                                         ELUGCLDA
01268         ADD 1 TO ATBL-TBL-CNT                                     ELUGCLDA
01269         IF ATBL-TBL-FULL                                          ELUGCLDA
01270         THEN                                                      ELUGCLDA
01271            PERFORM 994-SIGNAL-INCR-TBL-SIZE                       ELUGCLDA
01272         ELSE                                                      ELUGCLDA
01273            SET ATBL-X-IDX TO ATBL-TBL-CNT                         ELUGCLDA
01274            PERFORM 612-LOAD-ACP-VALUES                            ELUGCLDA
01275            PERFORM 613-LOAD-ACP-INTERNAL-SLOTS.                   ELUGCLDA
01276                                                                   ELUGCLDA
01277 ******************************************************************ELUGCLDA
01278 *                                                                *ELUGCLDA
01279 *    LOAD CO-PAY VALUES INTO THE CONTRACT SUMMARY                *ELUGCLDA
01280 *    ACCUMULATOR TABLE                                           *ELUGCLDA
01281 *                                                                *ELUGCLDA
01282 ******************************************************************ELUGCLDA
01283                                                                   ELUGCLDA
01284  612-LOAD-ACP-VALUES.                                             ELUGCLDA
01285      MOVE GAF-PROVISION-SLOT-NO                                   ELUGCLDA
01286        TO ATBL-SLOT-NUMBER (ATBL-X-IDX).                          ELUGCLDA
01287      MOVE GAF-COPAY-BEN-PER-TIME-FCTR (GAF-INDEX)                 ELUGCLDA
01288        TO ATBL-BEN-PER-TIME-FCTR (ATBL-X-IDX).                    ELUGCLDA
01289      MOVE GAF-COPAY-BEN-PER-TIME-QUAL (GAF-INDEX)                 ELUGCLDA
01290        TO ATBL-BEN-PER-TIME-QUAL (ATBL-X-IDX).                    ELUGCLDA
01291      MOVE GAF-COPAY-BENEFIT-PERIOD (GAF-INDEX)                    ELUGCLDA
01292        TO  ATBL-BENEFIT-PERIOD (ATBL-X-IDX).                      ELUGCLDA
01293      MOVE GAF-COPAY-CLAIM-LVL-ACCUM-IND (GAF-INDEX)               ELUGCLDA
01294        TO ATBL-CLAIM-LVL-ACCUM-IND (ATBL-X-IDX).                  ELUGCLDA
01295      MOVE GAF-COPAY-CO-PAY-IND (GAF-INDEX)                        ELUGCLDA
01296        TO ATBL-CO-PAY-IND (ATBL-X-IDX).                           ELUGCLDA
01297      MOVE GAF-COPAY-CONDITION (GAF-INDEX)                         ELUGCLDA
01298        TO ATBL-CONDITION (ATBL-X-IDX).                            ELUGCLDA
01299      MOVE GAF-COPAY-COST-CONTAIN-IND (GAF-INDEX)                  ELUGCLDA
01300        TO ATBL-COST-CONTAIN-IND (ATBL-X-IDX).                     ELUGCLDA
01301      MOVE GAF-COPAY-DAY-FACTOR-IND (GAF-INDEX)                    ELUGCLDA
01302        TO ATBL-DAY-FACTOR-IND (ATBL-X-IDX).                       ELUGCLDA
01303      MOVE GAF-COPAY-DEFINITION (GAF-INDEX)                        ELUGCLDA
01304        TO ATBL-DEFINITION (ATBL-X-IDX).                           ELUGCLDA
01305      MOVE GAF-COPAY-FAM-OR-INDIV (GAF-INDEX)                      ELUGCLDA
01306        TO ATBL-FAM-OR-INDIV (ATBL-X-IDX).                         ELUGCLDA
01307      MOVE GAF-COPAY-FYI-VALUE (GAF-INDEX)                         ELUGCLDA
01308        TO ATBL-FYI-VALUE (ATBL-X-IDX).                            ELUGCLDA
01309      MOVE GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX)               ELUGCLDA
01310        TO ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX).                  ELUGCLDA
01311      MOVE GAF-COPAY-INTERVAL-OVRD-IND (GAF-INDEX)                 ELUGCLDA
01312        TO ATBL-INTERVAL-OVRD-IND (ATBL-X-IDX).                    ELUGCLDA
01313      MOVE GAF-COPAY-INTERVAL-OVRD-VALUE (GAF-INDEX)               ELUGCLDA
01314        TO ATBL-INTERVAL-OVRD-VALUE (ATBL-X-IDX).                  ELUGCLDA
01315      MOVE GAF-COPAY-INTERVAL-TIME-FCTR (GAF-INDEX)                ELUGCLDA
01316        TO ATBL-INTERVAL-TIME-FCTR (ATBL-X-IDX).                   ELUGCLDA
01317      MOVE GAF-COPAY-INTERVAL-TYPE (GAF-INDEX)                     ELUGCLDA
01318        TO ATBL-INTERVAL-TYPE (ATBL-X-IDX).                        ELUGCLDA
01319      MOVE GAF-COPAY-L-O-B (GAF-INDEX)                             ELUGCLDA
01320        TO ATBL-L-O-B (ATBL-X-IDX).                                ELUGCLDA
01321      MOVE GAF-COPAY-MANDATORY-IND (GAF-INDEX)                     ELUGCLDA
01322        TO ATBL-MANDATORY-IND (ATBL-X-IDX).                        ELUGCLDA
01323      MOVE GAF-COPAY-PLACE-OF-TREATMENT (GAF-INDEX)                ELUGCLDA
01324        TO ATBL-PLACE-OF-TREATMENT (ATBL-X-IDX).                   ELUGCLDA
01325      MOVE GAF-COPAY-SERVICE-GROUP (GAF-INDEX)                     ELUGCLDA
01326        TO ATBL-SERVICE-GROUP (ATBL-X-IDX).                        ELUGCLDA
01327      MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                       ELUGCLDA
01328        TO ATBL-VALUE-LIMIT (ATBL-X-IDX).                          ELUGCLDA
01329      MOVE GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX)                   ELUGCLDA
01330        TO ATBL-VALUE-QUALIFIER (ATBL-X-IDX).                      ELUGCLDA
01331                                                                   ELUGCLDA
01332 *THE FOLLOWING LOADS FIELDS ADDED TO FACILITATE REALMED PROCESSINGELUGCLDA
01333 *                                                                 ELUGCLDA
01334      MOVE PC-ACP TO ATBL-ACCUM-DESC (ATBL-X-IDX)                  ELUGCLDA
01335      MOVE WS-GS-CON-FEAK-IND-ACP TO                               ELUGCLDA
01336           ATBL-CON-FEAK-IND (ATBL-X-IDX)                          ELUGCLDA
01337      MOVE WS-GS-PSEU-NBR-IND-ACP TO                               ELUGCLDA
01338           ATBL-PSEU-NBR-USING-IND (ATBL-X-IDX)                    ELUGCLDA
01339      MOVE WS-GROUP-NBR     TO ATBL-GRP-NBR (ATBL-X-IDX)           ELUGCLDA
01340      MOVE WS-SECTION-NBR   TO ATBL-SECT-NBR (ATBL-X-IDX)          ELUGCLDA
01341      MOVE WS-GS-PSEUDO-GROUP-NBR    TO                            ELUGCLDA
01342           ATBL-PSEUDO-GRP-NBR (ATBL-X-IDX)                        ELUGCLDA
01343      MOVE WS-GS-PSEUDO-SECTION-NBR  TO                            ELUGCLDA
01344           ATBL-PSEUDO-SECT-NBR (ATBL-X-IDX)                       ELUGCLDA
01345      MOVE WS-CONTRACT-BGN-DT-MMDD   TO                            ELUGCLDA
01346           ATBL-CON-BGN-DT-MMDD (ATBL-X-IDX)                       ELUGCLDA
01347                                                                   ELUGCLDA
01348      MOVE GAF-COPAY-AGE-LIMIT-FROM (GAF-INDEX) TO                 ELUGCLDA
01349           ATBL-AGE-LIMIT-FROM (ATBL-X-IDX)                        ELUGCLDA
01350      MOVE GAF-COPAY-AGE-LIMIT-TO (GAF-INDEX) TO                   ELUGCLDA
01351           ATBL-AGE-LIMIT-TO (ATBL-X-IDX)                          ELUGCLDA
01352      MOVE GAF-COPAY-AGE-QUAL-IND-FROM (GAF-INDEX) TO              ELUGCLDA
01353           ATBL-AGE-QUAL-FROM (ATBL-X-IDX)                         ELUGCLDA
01354      MOVE GAF-COPAY-AGE-QUAL-IND-TO (GAF-INDEX) TO                ELUGCLDA
01355           ATBL-AGE-QUAL-TO (ATBL-X-IDX).                          ELUGCLDA
01356                                                                   ELUGCLDA
01357 ******************************************************************ELUGCLDA
01358 *                                                                *ELUGCLDA
01359 *    LOAD CO-PAY INTERAL TABULAR SLOT NUMBERS INTO THE           *ELUGCLDA
01360 *    CONTRACT SUMMARY ACCUMULATOR TABLE                          *ELUGCLDA
01361 *                                                                *ELUGCLDA
01362 ******************************************************************ELUGCLDA
01363                                                                   ELUGCLDA
01364  613-LOAD-ACP-INTERNAL-SLOTS.                                     ELUGCLDA
01365      INITIALIZE ATBL-INTERNAL-TABULARS (ATBL-X-IDX).              ELUGCLDA
01366      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01367         VARYING GAF-INT-INDEX FROM 1 BY 1                         ELUGCLDA
01368           UNTIL   GAF-INT-INDEX                                   ELUGCLDA
01369                 > GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX)          ELUGCLDA
01370         IF GAF-INT-ID (GAF-INDEX GAF-INT-INDEX) = HIGH-VALUES     ELUGCLDA
01371         THEN                                                      ELUGCLDA
01372            MOVE ZERO TO WS-SAVE-SLOT-NBR                          ELUGCLDA
01373         ELSE                                                      ELUGCLDA
01374            MOVE GAF-INT-SLOT (GAF-INDEX GAF-INT-INDEX)            ELUGCLDA
01375              TO WS-SAVE-SLOT-NBR                                  ELUGCLDA
01376         END-IF                                                    ELUGCLDA
01377         EVALUATE GAF-INT-ID (GAF-INDEX GAF-INT-INDEX) ALSO TRUE   ELUGCLDA
01378            WHEN PC-IBGR ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01379               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01380                 TO ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01381               PERFORM 831-ADD-IBGR-TO-TBL                         ELUGCLDA
01382            WHEN PC-IDGD ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01383               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01384                 TO ATBL-IDGD-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01385               PERFORM 832-ADD-IDGD-TO-TBL                         ELUGCLDA
01386            WHEN PC-IPGN ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01387               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01388                 TO ATBL-IPGN-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01389               PERFORM 833-ADD-IPGN-TO-TBL                         ELUGCLDA
01390            WHEN PC-IPGP ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01391               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01392                 TO ATBL-IPGP-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01393               PERFORM 834-ADD-IPGP-TO-TBL                         ELUGCLDA
01394            WHEN PC-IPGT ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01395               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01396                 TO ATBL-IPGT-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01397               PERFORM 835-ADD-IPGT-TO-TBL                         ELUGCLDA
01398            WHEN PC-IPGS ALSO WS-SAVE-SLOT-NBR > ZERO              ELUGCLDA
01399               MOVE WS-SAVE-SLOT-NBR                               ELUGCLDA
01400                 TO ATBL-IPGS-SLOT-NUMBER (ATBL-X-IDX)             ELUGCLDA
01401               PERFORM 836-ADD-IPGS-TO-TBL                         ELUGCLDA
01402            WHEN OTHER                                             ELUGCLDA
01403               CONTINUE                                            ELUGCLDA
01404            END-EVALUATE                                           ELUGCLDA
01405         END-PERFORM.                                              ELUGCLDA
01406                                                                   ELUGCLDA
01407 /*****************************************************************ELUGCLDA
01408 *                                                                *ELUGCLDA
01409 *    PROCESS INTERNAL TABULARS                                   *ELUGCLDA
01410 *                                                                *ELUGCLDA
01411 ******************************************************************ELUGCLDA
01412                                                                   ELUGCLDA
01413  700-LOAD-INTERNAL-TABULARS.                                      ELUGCLDA
01414                                                                   ELUGCLDA
01415      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE NOT = NULL            ELUGCLDA
01416         PERFORM 710-LOAD-IBGR-INTERNAL-TABS                       ELUGCLDA
01417      ELSE                                                         ELUGCLDA
01418         CONTINUE.                                                 ELUGCLDA
01419                                                                   ELUGCLDA
01420      IF ADDRESS OF IDGD-INTERNAL-TABS-TABLE NOT = NULL            ELUGCLDA
01421         PERFORM 720-LOAD-IDGD-INTERNAL-TABS                       ELUGCLDA
01422      ELSE                                                         ELUGCLDA
01423         CONTINUE.                                                 ELUGCLDA
01424                                                                   ELUGCLDA
01425      IF ADDRESS OF IPGN-INTERNAL-TABS-TABLE NOT = NULL            ELUGCLDA
01426         PERFORM 730-LOAD-IPGN-INTERNAL-TABS                       ELUGCLDA
01427      ELSE                                                         ELUGCLDA
01428         CONTINUE.                                                 ELUGCLDA
01429                                                                   ELUGCLDA
01430      IF ADDRESS OF IPGP-INTERNAL-TABS-TABLE NOT = NULL            ELUGCLDA
01431         PERFORM 740-LOAD-IPGP-INTERNAL-TABS                       ELUGCLDA
01432      ELSE                                                         ELUGCLDA
01433         CONTINUE.                                                 ELUGCLDA
01434                                                                   ELUGCLDA
01435      IF ADDRESS OF IPGT-INTERNAL-TABS-TABLE NOT = NULL            ELUGCLDA
01436         PERFORM 750-LOAD-IPGT-INTERNAL-TABS                       ELUGCLDA
01437      ELSE                                                         ELUGCLDA
01438         CONTINUE.                                                 ELUGCLDA
01439                                                                   ELUGCLDA
01440      IF ADDRESS OF IPGS-INTERNAL-TABS-TABLE NOT = NULL            ELUGCLDA
01441         PERFORM 760-LOAD-IPGS-INTERNAL-TABS                       ELUGCLDA
01442      ELSE                                                         ELUGCLDA
01443         CONTINUE.                                                 ELUGCLDA
01444                                                                   ELUGCLDA
01445 /*****************************************************************ELUGCLDA
01446 *                                                                *ELUGCLDA
01447 *    LOAD BENEFIT PROVISION INTERNAL TABULARS                    *ELUGCLDA
01448 *                                                                *ELUGCLDA
01449 ******************************************************************ELUGCLDA
01450                                                                   ELUGCLDA
01451  710-LOAD-IBGR-INTERNAL-TABS.                                     ELUGCLDA
01452      MOVE PC-IBGR TO KWA-PROVISION-ID.                            ELUGCLDA
01453      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01454         VARYING IBGR-IDX FROM 1 BY 1                              ELUGCLDA
01455           UNTIL IBGR-IDX > IBGR-TBL-CNT                           ELUGCLDA
01456         MOVE IBGR-SLOT-NUMBER (IBGR-IDX)                          ELUGCLDA
01457           TO KWA-PROVISION-SLOT-NO                                ELUGCLDA
01458         PERFORM 811-READ-TABULAR-RECORD-KEEP                      ELUGCLDA
01459         SET IBGR-TABULAR-PTR (IBGR-IDX) TO IOP-REC-PTR            ELUGCLDA
01460         SET IOP-REC-PTR TO NULL                                   ELUGCLDA
01461         END-PERFORM.                                              ELUGCLDA
01462                                                                   ELUGCLDA
01463 /*****************************************************************ELUGCLDA
01464 *                                                                *ELUGCLDA
01465 *    LOAD DIAGNOSIS INTERNAL TABULARS                            *ELUGCLDA
01466 *                                                                *ELUGCLDA
01467 ******************************************************************ELUGCLDA
01468                                                                   ELUGCLDA
01469  720-LOAD-IDGD-INTERNAL-TABS.                                     ELUGCLDA
01470      MOVE PC-IDGD TO KWA-PROVISION-ID.                            ELUGCLDA
01471      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01472         VARYING IDGD-IDX FROM 1 BY 1                              ELUGCLDA
01473           UNTIL IDGD-IDX > IDGD-TBL-CNT                           ELUGCLDA
01474         MOVE IDGD-SLOT-NUMBER (IDGD-IDX)                          ELUGCLDA
01475           TO KWA-PROVISION-SLOT-NO                                ELUGCLDA
01476         PERFORM 811-READ-TABULAR-RECORD-KEEP                      ELUGCLDA
01477         SET IDGD-TABULAR-PTR (IDGD-IDX) TO IOP-REC-PTR            ELUGCLDA
01478         SET IOP-REC-PTR TO NULL                                   ELUGCLDA
01479         END-PERFORM.                                              ELUGCLDA
01480                                                                   ELUGCLDA
01481 /*****************************************************************ELUGCLDA
01482 *                                                                *ELUGCLDA
01483 *    LOAD PROVIDER NUMBER INTERNAL TABULARS                      *ELUGCLDA
01484 *                                                                *ELUGCLDA
01485 ******************************************************************ELUGCLDA
01486                                                                   ELUGCLDA
01487  730-LOAD-IPGN-INTERNAL-TABS.                                     ELUGCLDA
01488      MOVE PC-IPGN TO KWA-PROVISION-ID.                            ELUGCLDA
01489      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01490         VARYING IPGN-X-IDX FROM 1 BY 1                            ELUGCLDA
01491           UNTIL IPGN-X-IDX > IPGN-TBL-CNT                         ELUGCLDA
01492         MOVE IPGN-SLOT-NUMBER (IPGN-X-IDX)                        ELUGCLDA
01493           TO KWA-PROVISION-SLOT-NO                                ELUGCLDA
01494         PERFORM 811-READ-TABULAR-RECORD-KEEP                      ELUGCLDA
01495         SET IPGN-TABULAR-PTR (IPGN-X-IDX) TO IOP-REC-PTR          ELUGCLDA
01496         SET IOP-REC-PTR TO NULL                                   ELUGCLDA
01497         END-PERFORM.                                              ELUGCLDA
01498                                                                   ELUGCLDA
01499 ******************************************************************ELUGCLDA
01500 *                                                                *ELUGCLDA
01501 *    LOAD PROCEDURE CODE INTERNAL TABULARS                       *ELUGCLDA
01502 *                                                                *ELUGCLDA
01503 ******************************************************************ELUGCLDA
01504                                                                   ELUGCLDA
01505  740-LOAD-IPGP-INTERNAL-TABS.                                     ELUGCLDA
01506      MOVE PC-IPGP TO KWA-PROVISION-ID.                            ELUGCLDA
01507      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01508         VARYING IPGP-IDX FROM 1 BY 1                              ELUGCLDA
01509           UNTIL IPGP-IDX > IPGP-TBL-CNT                           ELUGCLDA
01510         MOVE IPGP-SLOT-NUMBER (IPGP-IDX)                          ELUGCLDA
01511           TO KWA-PROVISION-SLOT-NO                                ELUGCLDA
01512         PERFORM 811-READ-TABULAR-RECORD-KEEP                      ELUGCLDA
01513         SET IPGP-TABULAR-PTR (IPGP-IDX) TO IOP-REC-PTR            ELUGCLDA
01514         SET IOP-REC-PTR TO NULL                                   ELUGCLDA
01515         END-PERFORM.                                              ELUGCLDA
01516                                                                   ELUGCLDA
01517 /*****************************************************************ELUGCLDA
01518 *                                                                *ELUGCLDA
01519 *    LOAD PROVIDER TYPE INTERNAL TABULARS                        *ELUGCLDA
01520 *                                                                *ELUGCLDA
01521 ******************************************************************ELUGCLDA
01522                                                                   ELUGCLDA
01523  750-LOAD-IPGT-INTERNAL-TABS.                                     ELUGCLDA
01524      MOVE PC-IPGT TO KWA-PROVISION-ID.                            ELUGCLDA
01525      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01526         VARYING IPGT-X-IDX FROM 1 BY 1                            ELUGCLDA
01527           UNTIL IPGT-X-IDX > IPGT-TBL-CNT                         ELUGCLDA
01528         MOVE IPGT-SLOT-NUMBER (IPGT-X-IDX)                        ELUGCLDA
01529           TO KWA-PROVISION-SLOT-NO                                ELUGCLDA
01530         PERFORM 811-READ-TABULAR-RECORD-KEEP                      ELUGCLDA
01531         SET IPGT-TABULAR-PTR (IPGT-X-IDX) TO IOP-REC-PTR          ELUGCLDA
01532         SET IOP-REC-PTR TO NULL                                   ELUGCLDA
01533         END-PERFORM.                                              ELUGCLDA
01534                                                                   ELUGCLDA
01535 /*****************************************************************ELUGCLDA
01536 *                                                                *ELUGCLDA
01537 *    LOAD PROVIDER SPEC INTERNAL TABULARS                        *ELUGCLDA
01538 *                                                                *ELUGCLDA
01539 ******************************************************************ELUGCLDA
01540                                                                   ELUGCLDA
01541  760-LOAD-IPGS-INTERNAL-TABS.                                     ELUGCLDA
01542      MOVE PC-IPGS TO KWA-PROVISION-ID.                            ELUGCLDA
01543      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01544         VARYING IPGS-X-IDX FROM 1 BY 1                            ELUGCLDA
01545           UNTIL IPGS-X-IDX > IPGS-TBL-CNT                         ELUGCLDA
01546         MOVE IPGS-SLOT-NUMBER (IPGS-X-IDX)                        ELUGCLDA
01547           TO KWA-PROVISION-SLOT-NO                                ELUGCLDA
01548         PERFORM 811-READ-TABULAR-RECORD-KEEP                      ELUGCLDA
01549         SET IPGS-TABULAR-PTR (IPGS-X-IDX) TO IOP-REC-PTR          ELUGCLDA
01550         SET IOP-REC-PTR TO NULL                                   ELUGCLDA
01551         END-PERFORM.                                              ELUGCLDA
01552                                                                   ELUGCLDA
01553 ******************************************************************ELUGCLDA
01554 *                                                                *ELUGCLDA
01555 *    ACQUIRE ACCUMULATOR POINTER TABLE AREA                      *ELUGCLDA
01556 *                                                                *ELUGCLDA
01557 ******************************************************************ELUGCLDA
01558                                                                   ELUGCLDA
01559  800-ACQ-ACCUM-PTR-TBL.                                           ELUGCLDA
01560      SET CIA-ELSCSAC-DDN TO TRUE.                                 ELUGCLDA
01561      COMPUTE CIA-AREA-LEN = LENGTH OF CSAC-ACCUMULATOR-TABLE.     ELUGCLDA
01562      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGCLDA
01563      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUGCLDA
01564      SET CIA-ELSCSAC-DDN TO TRUE.                                 ELUGCLDA
01565      CALL 'ELUSETAD'                                              ELUGCLDA
01566         USING DFHCOMMAREA                                         ELUGCLDA
01567               ADDRESS OF CSAC-ACCUMULATOR-TABLE.                  ELUGCLDA
01568                                                                   ELUGCLDA
01569 /*****************************************************************ELUGCLDA
01570 *                                                                *ELUGCLDA
01571 *    READ TABULAR RECORD - LOCATE MODE                           *ELUGCLDA
01572 *                                                                *ELUGCLDA
01573 ******************************************************************ELUGCLDA
01574                                                                   ELUGCLDA
01575  810-READ-TABULAR-RECORD.                                         ELUGCLDA
01576      SET CIA-GCTABULR-DDN TO TRUE.                                ELUGCLDA
01577      CALL 'ELUSETAD'                                              ELUGCLDA
01578         USING DFHCOMMAREA                                         ELUGCLDA
01579               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELUGCLDA
01580      IF CIA-RC-PTR-NULL                                           ELUGCLDA
01581      THEN                                                         ELUGCLDA
01582          PERFORM 812-ACQ-STG-GCTABULR.                            ELUGCLDA
01583      SET IOP-RD TO TRUE.                                          ELUGCLDA
01584      SET IOP-FCQ-NONE TO TRUE.                                    ELUGCLDA
01585      SET IOP-KVQ-EQ TO TRUE.                                      ELUGCLDA
01586      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUGCLDA
01587      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELUGCLDA
01588      PERFORM 952-CALL-I-O-MODULE.                                 ELUGCLDA
01589      IF IOP-RC-NOTFND                                             ELUGCLDA
01590      THEN                                                         ELUGCLDA
01591         PERFORM 992-SIGNAL-TABULAR-NOT-FOUND                      ELUGCLDA
01592      ELSE                                                         ELUGCLDA
01593         IF NOT IOP-RC-OK                                          ELUGCLDA
01594         THEN                                                      ELUGCLDA
01595            PERFORM 993-SIGNAL-CRITICAL-I-O-ERROR                  ELUGCLDA
01596         ELSE                                                      ELUGCLDA
01597            CONTINUE.                                              ELUGCLDA
01598                                                                   ELUGCLDA
01599 /*****************************************************************ELUGCLDA
01600 *                                                                *ELUGCLDA
01601 *    READ TABULAR RECORD - MOVE MODE                             *ELUGCLDA
01602 *                                                                *ELUGCLDA
01603 ******************************************************************ELUGCLDA
01604                                                                   ELUGCLDA
01605  811-READ-TABULAR-RECORD-KEEP.                                    ELUGCLDA
01606      SET CIA-GCTABULR-DDN TO TRUE.                                ELUGCLDA
01607      CALL 'ELUSETAD'                                              ELUGCLDA
01608         USING DFHCOMMAREA                                         ELUGCLDA
01609               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELUGCLDA
01610      IF CIA-RC-PTR-NULL                                           ELUGCLDA
01611      THEN                                                         ELUGCLDA
01612          PERFORM 812-ACQ-STG-GCTABULR.                            ELUGCLDA
01613      SET IOP-RD TO TRUE.                                          ELUGCLDA
01614      SET IOP-FCQ-NONE TO TRUE.                                    ELUGCLDA
01615      SET IOP-KVQ-EQ TO TRUE.                                      ELUGCLDA
01616      SET IOP-STG-MODE-MOVE TO TRUE.                               ELUGCLDA
01617      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELUGCLDA
01618      PERFORM 952-CALL-I-O-MODULE.                                 ELUGCLDA
01619      IF IOP-RC-NOTFND                                             ELUGCLDA
01620      THEN                                                         ELUGCLDA
01621         PERFORM 992-SIGNAL-TABULAR-NOT-FOUND                      ELUGCLDA
01622      ELSE                                                         ELUGCLDA
01623         IF NOT IOP-RC-OK                                          ELUGCLDA
01624         THEN                                                      ELUGCLDA
01625            PERFORM 993-SIGNAL-CRITICAL-I-O-ERROR                  ELUGCLDA
01626         ELSE                                                      ELUGCLDA
01627            CONTINUE.                                              ELUGCLDA
01628                                                                   ELUGCLDA
01629 ******************************************************************ELUGCLDA
01630 *                                                                *ELUGCLDA
01631 *    ACQUIRE STORAGE FOR GCTABULAR IOP BLOCK                     *ELUGCLDA
01632 *                                                                *ELUGCLDA
01633 ******************************************************************ELUGCLDA
01634                                                                   ELUGCLDA
01635  812-ACQ-STG-GCTABULR.                                            ELUGCLDA
01636      SET CIA-GCTABULR-DDN TO TRUE.                                ELUGCLDA
01637      COMPUTE CIA-AREA-LEN = LENGTH OF IOP-INPUT-OUTPUT-PARAMETERS.ELUGCLDA
01638      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGCLDA
01639      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUGCLDA
01640      CALL 'ELUSETAD'                                              ELUGCLDA
01641         USING DFHCOMMAREA                                         ELUGCLDA
01642         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                   ELUGCLDA
01643      IF CIA-RC-PTR-NULL                                           ELUGCLDA
01644      THEN                                                         ELUGCLDA
01645         PERFORM 991-SIGNAL-UNALLOC-AREA.                          ELUGCLDA
01646                                                                   ELUGCLDA
01647 ******************************************************************ELUGCLDA
01648 *                                                                *ELUGCLDA
01649 *    CREATE A NEW CONTRACT SUMMARY ACCUMULATOR TABLE             *ELUGCLDA
01650 *                                                                *ELUGCLDA
01651 *    - FORCE STORAGE MANAGEMENT TABLE POINTER TO NULL            *ELUGCLDA
01652 *    - GET THE MVO VALUE                                         *ELUGCLDA
01653 *    - ALLOCATE THE NEW AREA                                     *ELUGCLDA
01654 *    - GET ADDRESS OF THE NEW AREA                               *ELUGCLDA
01655 *                                                                *ELUGCLDA
01656 ******************************************************************ELUGCLDA
01657                                                                   ELUGCLDA
01658  820-CREATE-NEW-ATBL.                                             ELUGCLDA
01659                                                                   ELUGCLDA
01660      SET CIA-ELSATBL-DDN TO TRUE.                                 ELUGCLDA
01661      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO NULL.               ELUGCLDA
01662      CALL 'ELUSAVAD'                                              ELUGCLDA
01663         USING DFHCOMMAREA                                         ELUGCLDA
01664               ADDRESS OF ATBL-ACCUMULATOR-TABLE.                  ELUGCLDA
01665                                                                   ELUGCLDA
01666      CALL 'ELUSETAD'                                              ELUGCLDA
01667         USING DFHCOMMAREA                                         ELUGCLDA
01668               ADDRESS OF ATBL-ACCUMULATOR-TABLE.                  ELUGCLDA
01669                                                                   ELUGCLDA
01670      COMPUTE CIA-AREA-LEN =                                       ELUGCLDA
01671           LENGTH OF ATBL-TBL-CNT                                  ELUGCLDA
01672         + (CIA-MVO * LENGTH OF ATBL-ACCUMULATOR).                 ELUGCLDA
01673      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGCLDA
01674      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUGCLDA
01675                                                                   ELUGCLDA
01676      IF NOT CIA-RC-OK                                             ELUGCLDA
01677      THEN                                                         ELUGCLDA
01678         PERFORM 991-SIGNAL-UNALLOC-AREA                           ELUGCLDA
01679      ELSE                                                         ELUGCLDA
01680         CALL 'ELUSETAD'                                           ELUGCLDA
01681            USING DFHCOMMAREA                                      ELUGCLDA
01682                  ADDRESS OF ATBL-ACCUMULATOR-TABLE                ELUGCLDA
01683         IF CIA-RC-PTR-NULL                                        ELUGCLDA
01684         THEN                                                      ELUGCLDA
01685            PERFORM 991-SIGNAL-UNALLOC-AREA                        ELUGCLDA
01686         ELSE                                                      ELUGCLDA
01687            CONTINUE.                                              ELUGCLDA
01688                                                                   ELUGCLDA
01689 /*****************************************************************ELUGCLDA
01690 *                                                                *ELUGCLDA
01691 *    ADD BENEFIT PROVISION INTERNAL TABULAR SLOT NUMBER TO       *ELUGCLDA
01692 *    TABLE                                                       *ELUGCLDA
01693 *                                                                *ELUGCLDA
01694 ******************************************************************ELUGCLDA
01695                                                                   ELUGCLDA
01696  831-ADD-IBGR-TO-TBL.                                             ELUGCLDA
01697      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELUGCLDA
01698      THEN                                                         ELUGCLDA
01699         PERFORM 841-ALLOC-IBGR-TBL                                ELUGCLDA
01700      ELSE                                                         ELUGCLDA
01701         CONTINUE.                                                 ELUGCLDA
01702                                                                   ELUGCLDA
01703      SET SLOT-NOT-FOUND TO TRUE.                                  ELUGCLDA
01704      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01705         VARYING IBGR-IDX FROM 1 BY 1                              ELUGCLDA
01706           UNTIL    IBGR-IDX > IBGR-TBL-CNT                        ELUGCLDA
01707                 OR SLOT-FOUND                                     ELUGCLDA
01708         IF WS-SAVE-SLOT-NBR = IBGR-SLOT-NUMBER (IBGR-IDX)         ELUGCLDA
01709         THEN                                                      ELUGCLDA
01710            SET SLOT-FOUND TO TRUE                                 ELUGCLDA
01711         ELSE                                                      ELUGCLDA
01712            CONTINUE                                               ELUGCLDA
01713         END-IF                                                    ELUGCLDA
01714         END-PERFORM.                                              ELUGCLDA
01715                                                                   ELUGCLDA
01716      IF SLOT-FOUND                                                ELUGCLDA
01717      THEN                                                         ELUGCLDA
01718         CONTINUE                                                  ELUGCLDA
01719      ELSE                                                         ELUGCLDA
01720         IF NOT IBGR-TBL-FULL                                      ELUGCLDA
01721         THEN                                                      ELUGCLDA
01722            ADD 1 TO IBGR-TBL-CNT                                  ELUGCLDA
01723            SET IBGR-IDX TO IBGR-TBL-CNT                           ELUGCLDA
01724            MOVE WS-SAVE-SLOT-NBR TO IBGR-SLOT-NUMBER (IBGR-IDX)   ELUGCLDA
01725            SET IBGR-TABULAR-PTR (IBGR-IDX) TO NULL                ELUGCLDA
01726            INITIALIZE IBGR-CONFIDENCE-FACTORS (IBGR-IDX)          ELUGCLDA
01727         ELSE                                                      ELUGCLDA
01728            PERFORM 994-SIGNAL-INCR-TBL-SIZE.                      ELUGCLDA
01729                                                                   ELUGCLDA
01730 /*****************************************************************ELUGCLDA
01731 *                                                                 ELUGCLDA
01732 *    ADD DIAGNOSIS INTERNAL TABULAR SLOT NUMBER TO TABLE          ELUGCLDA
01733 *                                                                 ELUGCLDA
01734 ******************************************************************ELUGCLDA
01735                                                                   ELUGCLDA
01736  832-ADD-IDGD-TO-TBL.                                             ELUGCLDA
01737      IF ADDRESS OF IDGD-INTERNAL-TABS-TABLE = NULL                ELUGCLDA
01738      THEN                                                         ELUGCLDA
01739         PERFORM 842-ALLOC-IDGD-TBL                                ELUGCLDA
01740      ELSE                                                         ELUGCLDA
01741         CONTINUE.                                                 ELUGCLDA
01742                                                                   ELUGCLDA
01743      SET SLOT-NOT-FOUND TO TRUE.                                  ELUGCLDA
01744      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01745         VARYING IDGD-IDX FROM 1 BY 1                              ELUGCLDA
01746           UNTIL    IDGD-IDX > IDGD-TBL-CNT                        ELUGCLDA
01747                 OR SLOT-FOUND                                     ELUGCLDA
01748         IF WS-SAVE-SLOT-NBR = IDGD-SLOT-NUMBER (IDGD-IDX)         ELUGCLDA
01749         THEN                                                      ELUGCLDA
01750            SET SLOT-FOUND TO TRUE                                 ELUGCLDA
01751         ELSE                                                      ELUGCLDA
01752            CONTINUE                                               ELUGCLDA
01753         END-IF                                                    ELUGCLDA
01754         END-PERFORM.                                              ELUGCLDA
01755                                                                   ELUGCLDA
01756      IF SLOT-FOUND                                                ELUGCLDA
01757      THEN                                                         ELUGCLDA
01758         CONTINUE                                                  ELUGCLDA
01759      ELSE                                                         ELUGCLDA
01760         IF NOT IDGD-TBL-FULL                                      ELUGCLDA
01761         THEN                                                      ELUGCLDA
01762            ADD 1 TO IDGD-TBL-CNT                                  ELUGCLDA
01763            SET IDGD-IDX TO IDGD-TBL-CNT                           ELUGCLDA
01764            MOVE WS-SAVE-SLOT-NBR TO IDGD-SLOT-NUMBER (IDGD-IDX)   ELUGCLDA
01765            SET IDGD-TABULAR-PTR (IDGD-IDX) TO NULL                ELUGCLDA
01766            INITIALIZE IDGD-CONFIDENCE-FACTORS (IDGD-IDX)          ELUGCLDA
01767         ELSE                                                      ELUGCLDA
01768            PERFORM 994-SIGNAL-INCR-TBL-SIZE.                      ELUGCLDA
01769                                                                   ELUGCLDA
01770 /*****************************************************************ELUGCLDA
01771 *                                                                *ELUGCLDA
01772 *    ADD PROVIDER NUMBER INTERNAL TABULAR SLOT NUMBER TO TABLE   *ELUGCLDA
01773 *                                                                *ELUGCLDA
01774 ******************************************************************ELUGCLDA
01775                                                                   ELUGCLDA
01776  833-ADD-IPGN-TO-TBL.                                             ELUGCLDA
01777      IF ADDRESS OF IPGN-INTERNAL-TABS-TABLE = NULL                ELUGCLDA
01778      THEN                                                         ELUGCLDA
01779         PERFORM 843-ALLOC-IPGN-TBL                                ELUGCLDA
01780      ELSE                                                         ELUGCLDA
01781         CONTINUE.                                                 ELUGCLDA
01782                                                                   ELUGCLDA
01783      SET SLOT-NOT-FOUND TO TRUE.                                  ELUGCLDA
01784      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01785         VARYING IPGN-X-IDX FROM 1 BY 1                            ELUGCLDA
01786           UNTIL    IPGN-X-IDX > IPGN-TBL-CNT                      ELUGCLDA
01787                 OR SLOT-FOUND                                     ELUGCLDA
01788         IF WS-SAVE-SLOT-NBR = IPGN-SLOT-NUMBER (IPGN-X-IDX)       ELUGCLDA
01789         THEN                                                      ELUGCLDA
01790            SET SLOT-FOUND TO TRUE                                 ELUGCLDA
01791         ELSE                                                      ELUGCLDA
01792            CONTINUE                                               ELUGCLDA
01793         END-IF                                                    ELUGCLDA
01794         END-PERFORM.                                              ELUGCLDA
01795                                                                   ELUGCLDA
01796      IF SLOT-FOUND                                                ELUGCLDA
01797      THEN                                                         ELUGCLDA
01798         CONTINUE                                                  ELUGCLDA
01799      ELSE                                                         ELUGCLDA
01800         IF NOT IPGN-TBL-FULL                                      ELUGCLDA
01801         THEN                                                      ELUGCLDA
01802            ADD 1 TO IPGN-TBL-CNT                                  ELUGCLDA
01803            SET IPGN-X-IDX TO IPGN-TBL-CNT                         ELUGCLDA
01804            MOVE WS-SAVE-SLOT-NBR TO IPGN-SLOT-NUMBER (IPGN-X-IDX) ELUGCLDA
01805            SET IPGN-TABULAR-PTR (IPGN-X-IDX) TO NULL              ELUGCLDA
01806            INITIALIZE IPGN-CONFIDENCE-FACTORS (IPGN-X-IDX)        ELUGCLDA
01807         ELSE                                                      ELUGCLDA
01808            PERFORM 994-SIGNAL-INCR-TBL-SIZE.                      ELUGCLDA
01809                                                                   ELUGCLDA
01810 /*****************************************************************ELUGCLDA
01811 *                                                                *ELUGCLDA
01812 *    ADD PROCEDURE INTERNAL TABULAR SLOT NUMBER TO TABLE         *ELUGCLDA
01813 *                                                                *ELUGCLDA
01814 ******************************************************************ELUGCLDA
01815                                                                   ELUGCLDA
01816  834-ADD-IPGP-TO-TBL.                                             ELUGCLDA
01817      IF ADDRESS OF IPGP-INTERNAL-TABS-TABLE = NULL                ELUGCLDA
01818      THEN                                                         ELUGCLDA
01819         PERFORM 844-ALLOC-IPGP-TBL                                ELUGCLDA
01820      ELSE                                                         ELUGCLDA
01821         CONTINUE.                                                 ELUGCLDA
01822                                                                   ELUGCLDA
01823      SET SLOT-NOT-FOUND TO TRUE.                                  ELUGCLDA
01824      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01825         VARYING IPGP-IDX FROM 1 BY 1                              ELUGCLDA
01826           UNTIL    IPGP-IDX > IPGP-TBL-CNT                        ELUGCLDA
01827                 OR SLOT-FOUND                                     ELUGCLDA
01828         IF WS-SAVE-SLOT-NBR = IPGP-SLOT-NUMBER (IPGP-IDX)         ELUGCLDA
01829         THEN                                                      ELUGCLDA
01830            SET SLOT-FOUND TO TRUE                                 ELUGCLDA
01831         ELSE                                                      ELUGCLDA
01832            CONTINUE                                               ELUGCLDA
01833         END-IF                                                    ELUGCLDA
01834         END-PERFORM.                                              ELUGCLDA
01835                                                                   ELUGCLDA
01836      IF SLOT-FOUND                                                ELUGCLDA
01837      THEN                                                         ELUGCLDA
01838         CONTINUE                                                  ELUGCLDA
01839      ELSE                                                         ELUGCLDA
01840         IF NOT IPGP-TBL-FULL                                      ELUGCLDA
01841         THEN                                                      ELUGCLDA
01842            ADD 1 TO IPGP-TBL-CNT                                  ELUGCLDA
01843            SET IPGP-IDX TO IPGP-TBL-CNT                           ELUGCLDA
01844            MOVE WS-SAVE-SLOT-NBR TO IPGP-SLOT-NUMBER (IPGP-IDX)   ELUGCLDA
01845            SET IPGP-TABULAR-PTR (IPGP-IDX) TO NULL                ELUGCLDA
01846            INITIALIZE IPGP-CONFIDENCE-FACTORS (IPGP-IDX)          ELUGCLDA
01847         ELSE                                                      ELUGCLDA
01848            PERFORM 994-SIGNAL-INCR-TBL-SIZE.                      ELUGCLDA
01849      CONTINUE.                                                    ELUGCLDA
01850                                                                   ELUGCLDA
01851 /*****************************************************************ELUGCLDA
01852 *                                                                *ELUGCLDA
01853 *   ADD PROVIDER TYPE INTERNAL TABULAR SLOT NUMBER TO TABLE      *ELUGCLDA
01854 *                                                                *ELUGCLDA
01855 ******************************************************************ELUGCLDA
01856                                                                   ELUGCLDA
01857  835-ADD-IPGT-TO-TBL.                                             ELUGCLDA
01858      IF ADDRESS OF IPGT-INTERNAL-TABS-TABLE = NULL                ELUGCLDA
01859      THEN                                                         ELUGCLDA
01860         PERFORM 845-ALLOC-IPGT-TBL                                ELUGCLDA
01861      ELSE                                                         ELUGCLDA
01862         CONTINUE.                                                 ELUGCLDA
01863                                                                   ELUGCLDA
01864      SET SLOT-NOT-FOUND TO TRUE.                                  ELUGCLDA
01865      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01866         VARYING IPGT-X-IDX FROM 1 BY 1                            ELUGCLDA
01867           UNTIL    IPGT-X-IDX > IPGT-TBL-CNT                      ELUGCLDA
01868                 OR SLOT-FOUND                                     ELUGCLDA
01869         IF WS-SAVE-SLOT-NBR = IPGT-SLOT-NUMBER (IPGT-X-IDX)       ELUGCLDA
01870         THEN                                                      ELUGCLDA
01871            SET SLOT-FOUND TO TRUE                                 ELUGCLDA
01872         ELSE                                                      ELUGCLDA
01873            CONTINUE                                               ELUGCLDA
01874         END-IF                                                    ELUGCLDA
01875         END-PERFORM.                                              ELUGCLDA
01876                                                                   ELUGCLDA
01877      IF SLOT-FOUND                                                ELUGCLDA
01878      THEN                                                         ELUGCLDA
01879         CONTINUE                                                  ELUGCLDA
01880      ELSE                                                         ELUGCLDA
01881         IF NOT IPGT-TBL-FULL                                      ELUGCLDA
01882         THEN                                                      ELUGCLDA
01883            ADD 1 TO IPGT-TBL-CNT                                  ELUGCLDA
01884            SET IPGT-X-IDX TO IPGT-TBL-CNT                         ELUGCLDA
01885            MOVE WS-SAVE-SLOT-NBR TO IPGT-SLOT-NUMBER (IPGT-X-IDX) ELUGCLDA
01886            SET IPGT-TABULAR-PTR (IPGT-X-IDX) TO NULL              ELUGCLDA
01887            INITIALIZE IPGT-CONFIDENCE-FACTORS (IPGT-X-IDX)        ELUGCLDA
01888         ELSE                                                      ELUGCLDA
01889            PERFORM 994-SIGNAL-INCR-TBL-SIZE.                      ELUGCLDA
01890                                                                   ELUGCLDA
01891                                                                   ELUGCLDA
01892 /*****************************************************************ELUGCLDA
01893 *                                                                *ELUGCLDA
01894 *   ADD PROVIDER SPEC INTERNAL TABULAR SLOT NUMBER TO TABLE      *ELUGCLDA
01895 *                                                                *ELUGCLDA
01896 ******************************************************************ELUGCLDA
01897                                                                   ELUGCLDA
01898  836-ADD-IPGS-TO-TBL.                                             ELUGCLDA
01899      IF ADDRESS OF IPGS-INTERNAL-TABS-TABLE = NULL                ELUGCLDA
01900         PERFORM 846-ALLOC-IPGS-TBL                                ELUGCLDA
01901      ELSE                                                         ELUGCLDA
01902         CONTINUE.                                                 ELUGCLDA
01903                                                                   ELUGCLDA
01904      SET SLOT-NOT-FOUND TO TRUE.                                  ELUGCLDA
01905      PERFORM WITH TEST BEFORE                                     ELUGCLDA
01906         VARYING IPGS-X-IDX FROM 1 BY 1                            ELUGCLDA
01907           UNTIL    IPGS-X-IDX > IPGS-TBL-CNT                      ELUGCLDA
01908                 OR SLOT-FOUND                                     ELUGCLDA
01909         IF WS-SAVE-SLOT-NBR = IPGS-SLOT-NUMBER (IPGS-X-IDX)       ELUGCLDA
01910            SET SLOT-FOUND TO TRUE                                 ELUGCLDA
01911         ELSE                                                      ELUGCLDA
01912            CONTINUE                                               ELUGCLDA
01913         END-IF                                                    ELUGCLDA
01914         END-PERFORM.                                              ELUGCLDA
01915                                                                   ELUGCLDA
01916      IF SLOT-FOUND                                                ELUGCLDA
01917         CONTINUE                                                  ELUGCLDA
01918      ELSE                                                         ELUGCLDA
01919         IF NOT IPGS-TBL-FULL                                      ELUGCLDA
01920            ADD 1 TO IPGS-TBL-CNT                                  ELUGCLDA
01921            SET IPGS-X-IDX TO IPGS-TBL-CNT                         ELUGCLDA
01922            MOVE WS-SAVE-SLOT-NBR TO IPGS-SLOT-NUMBER (IPGS-X-IDX) ELUGCLDA
01923            SET IPGS-TABULAR-PTR (IPGS-X-IDX) TO NULL              ELUGCLDA
01924            INITIALIZE IPGS-CONFIDENCE-FACTORS (IPGS-X-IDX)        ELUGCLDA
01925         ELSE                                                      ELUGCLDA
01926            PERFORM 994-SIGNAL-INCR-TBL-SIZE.                      ELUGCLDA
01927                                                                   ELUGCLDA
01928                                                                   ELUGCLDA
01929 /*****************************************************************ELUGCLDA
01930 *                                                                *ELUGCLDA
01931 *    ALLOCATE THE BENEFIT PROVISION INTERNAL TABULAR TABLE       *ELUGCLDA
01932 *                                                                *ELUGCLDA
01933 ******************************************************************ELUGCLDA
01934                                                                   ELUGCLDA
01935  841-ALLOC-IBGR-TBL.                                              ELUGCLDA
01936      SET CIA-ELSIBGR-DDN TO TRUE.                                 ELUGCLDA
01937      SET ADDRESS OF IBGR-INTERNAL-TABS-TABLE TO NULL.             ELUGCLDA
01938      CALL 'ELUSAVAD'                                              ELUGCLDA
01939         USING DFHCOMMAREA                                         ELUGCLDA
01940               ADDRESS OF IBGR-INTERNAL-TABS-TABLE.                ELUGCLDA
01941                                                                   ELUGCLDA
01942      CALL 'ELUSETAD'                                              ELUGCLDA
01943         USING DFHCOMMAREA                                         ELUGCLDA
01944               ADDRESS OF IBGR-INTERNAL-TABS-TABLE.                ELUGCLDA
01945                                                                   ELUGCLDA
01946      COMPUTE CIA-AREA-LEN =                                       ELUGCLDA
01947                LENGTH OF IBGR-TBL-CNT                             ELUGCLDA
01948              + (CIA-MVO * LENGTH OF IBGR-INTERNAL-TABS)           ELUGCLDA
01949                                                                   ELUGCLDA
01950      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGCLDA
01951      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUGCLDA
01952                                                                   ELUGCLDA
01953      IF NOT CIA-RC-OK                                             ELUGCLDA
01954      THEN                                                         ELUGCLDA
01955         PERFORM 991-SIGNAL-UNALLOC-AREA                           ELUGCLDA
01956      ELSE                                                         ELUGCLDA
01957         CALL 'ELUSETAD'                                           ELUGCLDA
01958            USING DFHCOMMAREA                                      ELUGCLDA
01959               ADDRESS OF IBGR-INTERNAL-TABS-TABLE                 ELUGCLDA
01960         IF CIA-RC-PTR-NULL                                        ELUGCLDA
01961         THEN                                                      ELUGCLDA
01962            PERFORM 991-SIGNAL-UNALLOC-AREA                        ELUGCLDA
01963         ELSE                                                      ELUGCLDA
01964            CONTINUE.                                              ELUGCLDA
01965                                                                   ELUGCLDA
01966 /*****************************************************************ELUGCLDA
01967 *                                                                *ELUGCLDA
01968 *    ALLOCATE THE DIAGNOSIS INTERNAL TABULAR TABLE               *ELUGCLDA
01969 *                                                                *ELUGCLDA
01970 ******************************************************************ELUGCLDA
01971                                                                   ELUGCLDA
01972  842-ALLOC-IDGD-TBL.                                              ELUGCLDA
01973      SET CIA-ELSIDGD-DDN TO TRUE.                                 ELUGCLDA
01974      SET ADDRESS OF IDGD-INTERNAL-TABS-TABLE TO NULL.             ELUGCLDA
01975      CALL 'ELUSAVAD'                                              ELUGCLDA
01976         USING DFHCOMMAREA                                         ELUGCLDA
01977               ADDRESS OF IDGD-INTERNAL-TABS-TABLE.                ELUGCLDA
01978                                                                   ELUGCLDA
01979      CALL 'ELUSETAD'                                              ELUGCLDA
01980         USING DFHCOMMAREA                                         ELUGCLDA
01981               ADDRESS OF IDGD-INTERNAL-TABS-TABLE.                ELUGCLDA
01982                                                                   ELUGCLDA
01983      COMPUTE CIA-AREA-LEN =                                       ELUGCLDA
01984                LENGTH OF IDGD-TBL-CNT                             ELUGCLDA
01985              + (CIA-MVO * LENGTH OF IDGD-INTERNAL-TABS)           ELUGCLDA
01986                                                                   ELUGCLDA
01987      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGCLDA
01988      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUGCLDA
01989                                                                   ELUGCLDA
01990      IF NOT CIA-RC-OK                                             ELUGCLDA
01991      THEN                                                         ELUGCLDA
01992         PERFORM 991-SIGNAL-UNALLOC-AREA                           ELUGCLDA
01993      ELSE                                                         ELUGCLDA
01994         CALL 'ELUSETAD'                                           ELUGCLDA
01995            USING DFHCOMMAREA                                      ELUGCLDA
01996               ADDRESS OF IDGD-INTERNAL-TABS-TABLE                 ELUGCLDA
01997         IF CIA-RC-PTR-NULL                                        ELUGCLDA
01998         THEN                                                      ELUGCLDA
01999            PERFORM 991-SIGNAL-UNALLOC-AREA                        ELUGCLDA
02000         ELSE                                                      ELUGCLDA
02001            CONTINUE.                                              ELUGCLDA
02002                                                                   ELUGCLDA
02003 /*****************************************************************ELUGCLDA
02004 *                                                                *ELUGCLDA
02005 *    ALLOCATE THE PROVIDER NUMBER INTERNAL TABULAR TABLE         *ELUGCLDA
02006 *                                                                *ELUGCLDA
02007 ******************************************************************ELUGCLDA
02008                                                                   ELUGCLDA
02009  843-ALLOC-IPGN-TBL.                                              ELUGCLDA
02010      SET CIA-ELSIPGN-DDN TO TRUE.                                 ELUGCLDA
02011      SET ADDRESS OF IPGN-INTERNAL-TABS-TABLE TO NULL.             ELUGCLDA
02012      CALL 'ELUSAVAD'                                              ELUGCLDA
02013         USING DFHCOMMAREA                                         ELUGCLDA
02014               ADDRESS OF IPGN-INTERNAL-TABS-TABLE.                ELUGCLDA
02015                                                                   ELUGCLDA
02016      CALL 'ELUSETAD'                                              ELUGCLDA
02017         USING DFHCOMMAREA                                         ELUGCLDA
02018               ADDRESS OF IPGN-INTERNAL-TABS-TABLE.                ELUGCLDA
02019                                                                   ELUGCLDA
02020      COMPUTE CIA-AREA-LEN =                                       ELUGCLDA
02021                LENGTH OF IPGN-TBL-CNT                             ELUGCLDA
02022              + (CIA-MVO * LENGTH OF IPGN-INTERNAL-TABS)           ELUGCLDA
02023                                                                   ELUGCLDA
02024      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGCLDA
02025      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUGCLDA
02026                                                                   ELUGCLDA
02027      IF NOT CIA-RC-OK                                             ELUGCLDA
02028      THEN                                                         ELUGCLDA
02029         PERFORM 991-SIGNAL-UNALLOC-AREA                           ELUGCLDA
02030      ELSE                                                         ELUGCLDA
02031         CALL 'ELUSETAD'                                           ELUGCLDA
02032            USING DFHCOMMAREA                                      ELUGCLDA
02033               ADDRESS OF IPGN-INTERNAL-TABS-TABLE                 ELUGCLDA
02034         IF CIA-RC-PTR-NULL                                        ELUGCLDA
02035         THEN                                                      ELUGCLDA
02036            PERFORM 991-SIGNAL-UNALLOC-AREA                        ELUGCLDA
02037         ELSE                                                      ELUGCLDA
02038            CONTINUE.                                              ELUGCLDA
02039                                                                   ELUGCLDA
02040 /*****************************************************************ELUGCLDA
02041 *                                                                *ELUGCLDA
02042 *    ALLOCATE THE PROCEDURE INTERNAL TABULAR TABLE               *ELUGCLDA
02043 *                                                                *ELUGCLDA
02044 ******************************************************************ELUGCLDA
02045                                                                   ELUGCLDA
02046  844-ALLOC-IPGP-TBL.                                              ELUGCLDA
02047      SET CIA-ELSIPGP-DDN TO TRUE.                                 ELUGCLDA
02048      SET ADDRESS OF IPGP-INTERNAL-TABS-TABLE TO NULL.             ELUGCLDA
02049      CALL 'ELUSAVAD'                                              ELUGCLDA
02050         USING DFHCOMMAREA                                         ELUGCLDA
02051               ADDRESS OF IPGP-INTERNAL-TABS-TABLE.                ELUGCLDA
02052                                                                   ELUGCLDA
02053      CALL 'ELUSETAD'                                              ELUGCLDA
02054         USING DFHCOMMAREA                                         ELUGCLDA
02055               ADDRESS OF IPGP-INTERNAL-TABS-TABLE.                ELUGCLDA
02056                                                                   ELUGCLDA
02057      COMPUTE CIA-AREA-LEN =                                       ELUGCLDA
02058                LENGTH OF IPGP-TBL-CNT                             ELUGCLDA
02059              + (CIA-MVO * LENGTH OF IPGP-INTERNAL-TABS)           ELUGCLDA
02060                                                                   ELUGCLDA
02061      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGCLDA
02062      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUGCLDA
02063                                                                   ELUGCLDA
02064      IF NOT CIA-RC-OK                                             ELUGCLDA
02065      THEN                                                         ELUGCLDA
02066         PERFORM 991-SIGNAL-UNALLOC-AREA                           ELUGCLDA
02067      ELSE                                                         ELUGCLDA
02068         CALL 'ELUSETAD'                                           ELUGCLDA
02069            USING DFHCOMMAREA                                      ELUGCLDA
02070               ADDRESS OF IPGP-INTERNAL-TABS-TABLE                 ELUGCLDA
02071         IF CIA-RC-PTR-NULL                                        ELUGCLDA
02072         THEN                                                      ELUGCLDA
02073            PERFORM 991-SIGNAL-UNALLOC-AREA                        ELUGCLDA
02074         ELSE                                                      ELUGCLDA
02075            CONTINUE.                                              ELUGCLDA
02076      CONTINUE.                                                    ELUGCLDA
02077                                                                   ELUGCLDA
02078 /*****************************************************************ELUGCLDA
02079 *                                                                *ELUGCLDA
02080 *    ALLOCATE THE PROVIDER TYPE INTERNAL TABULAR TABLE           *ELUGCLDA
02081 *                                                                *ELUGCLDA
02082 ******************************************************************ELUGCLDA
02083                                                                   ELUGCLDA
02084  845-ALLOC-IPGT-TBL.                                              ELUGCLDA
02085      SET CIA-ELSIPGT-DDN TO TRUE.                                 ELUGCLDA
02086      SET ADDRESS OF IPGT-INTERNAL-TABS-TABLE TO NULL.             ELUGCLDA
02087      CALL 'ELUSAVAD'                                              ELUGCLDA
02088         USING DFHCOMMAREA                                         ELUGCLDA
02089               ADDRESS OF IPGT-INTERNAL-TABS-TABLE.                ELUGCLDA
02090                                                                   ELUGCLDA
02091      CALL 'ELUSETAD'                                              ELUGCLDA
02092         USING DFHCOMMAREA                                         ELUGCLDA
02093               ADDRESS OF IPGT-INTERNAL-TABS-TABLE.                ELUGCLDA
02094                                                                   ELUGCLDA
02095      COMPUTE CIA-AREA-LEN =                                       ELUGCLDA
02096                LENGTH OF IPGT-TBL-CNT                             ELUGCLDA
02097              + (CIA-MVO * LENGTH OF IPGT-INTERNAL-TABS)           ELUGCLDA
02098                                                                   ELUGCLDA
02099      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGCLDA
02100      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUGCLDA
02101                                                                   ELUGCLDA
02102      IF NOT CIA-RC-OK                                             ELUGCLDA
02103      THEN                                                         ELUGCLDA
02104         PERFORM 991-SIGNAL-UNALLOC-AREA                           ELUGCLDA
02105      ELSE                                                         ELUGCLDA
02106         CALL 'ELUSETAD'                                           ELUGCLDA
02107            USING DFHCOMMAREA                                      ELUGCLDA
02108               ADDRESS OF IPGT-INTERNAL-TABS-TABLE                 ELUGCLDA
02109         IF CIA-RC-PTR-NULL                                        ELUGCLDA
02110         THEN                                                      ELUGCLDA
02111            PERFORM 991-SIGNAL-UNALLOC-AREA                        ELUGCLDA
02112         ELSE                                                      ELUGCLDA
02113            CONTINUE.                                              ELUGCLDA
02114                                                                   ELUGCLDA
02115 /*****************************************************************ELUGCLDA
02116 *                                                                *ELUGCLDA
02117 *    ALLOCATE THE PROVIDER SPEC INTERNAL TABULAR TABLE           *ELUGCLDA
02118 *                                                                *ELUGCLDA
02119 ******************************************************************ELUGCLDA
02120                                                                   ELUGCLDA
02121  846-ALLOC-IPGS-TBL.                                              ELUGCLDA
02122      SET CIA-ELSIPGS-DDN TO TRUE.                                 ELUGCLDA
02123      SET ADDRESS OF IPGS-INTERNAL-TABS-TABLE TO NULL.             ELUGCLDA
02124      CALL 'ELUSAVAD'                                              ELUGCLDA
02125         USING DFHCOMMAREA                                         ELUGCLDA
02126               ADDRESS OF IPGS-INTERNAL-TABS-TABLE.                ELUGCLDA
02127                                                                   ELUGCLDA
02128      CALL 'ELUSETAD'                                              ELUGCLDA
02129         USING DFHCOMMAREA                                         ELUGCLDA
02130               ADDRESS OF IPGS-INTERNAL-TABS-TABLE.                ELUGCLDA
02131                                                                   ELUGCLDA
02132      COMPUTE CIA-AREA-LEN =                                       ELUGCLDA
02133                LENGTH OF IPGS-TBL-CNT                             ELUGCLDA
02134              + (CIA-MVO * LENGTH OF IPGS-INTERNAL-TABS)           ELUGCLDA
02135                                                                   ELUGCLDA
02136      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGCLDA
02137      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUGCLDA
02138                                                                   ELUGCLDA
02139      IF NOT CIA-RC-OK                                             ELUGCLDA
02140      THEN                                                         ELUGCLDA
02141         PERFORM 991-SIGNAL-UNALLOC-AREA                           ELUGCLDA
02142      ELSE                                                         ELUGCLDA
02143         CALL 'ELUSETAD'                                           ELUGCLDA
02144            USING DFHCOMMAREA                                      ELUGCLDA
02145               ADDRESS OF IPGS-INTERNAL-TABS-TABLE                 ELUGCLDA
02146         IF CIA-RC-PTR-NULL                                        ELUGCLDA
02147         THEN                                                      ELUGCLDA
02148            PERFORM 991-SIGNAL-UNALLOC-AREA                        ELUGCLDA
02149         ELSE                                                      ELUGCLDA
02150            CONTINUE.                                              ELUGCLDA
02151                                                                   ELUGCLDA
02152 ******************************************************************ELUGCLDA
02153 *                                                                *ELUGCLDA
02154 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA                   *ELUGCLDA
02155 *                                                                *ELUGCLDA
02156 ******************************************************************ELUGCLDA
02157                                                                   ELUGCLDA
02158  902-ESTAB-ADDR-ELSKEYS.                                          ELUGCLDA
02159      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELUGCLDA
02160      CALL 'ELUSETAD'                                              ELUGCLDA
02161         USING DFHCOMMAREA                                         ELUGCLDA
02162               ADDRESS OF KWA-FILE-KEY-WORK-AREA.                  ELUGCLDA
02163      IF CIA-RC-PTR-NULL                                           ELUGCLDA
02164      THEN                                                         ELUGCLDA
02165         PERFORM 991-SIGNAL-UNALLOC-AREA.                          ELUGCLDA
02166                                                                   ELUGCLDA
02167 ******************************************************************ELUGCLDA
02168 *                                                                *ELUGCLDA
02169 *    ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTROL         *ELUGCLDA
02170 *    BLOCK                                                       *ELUGCLDA
02171 *                                                                *ELUGCLDA
02172 ******************************************************************ELUGCLDA
02173                                                                   ELUGCLDA
02174  904-ESTAB-ADDR-ELSSSCB.                                          ELUGCLDA
02175      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELUGCLDA
02176      CALL 'ELUSETAD'                                              ELUGCLDA
02177          USING DFHCOMMAREA                                        ELUGCLDA
02178                ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.            ELUGCLDA
02179      IF CIA-RC-PTR-NULL                                           ELUGCLDA
02180          PERFORM 991-SIGNAL-UNALLOC-AREA.                         ELUGCLDA
02181                                                                   ELUGCLDA
02182 ******************************************************************ELUGCLDA
02183 *                                                                *ELUGCLDA
02184 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC                  *ELUGCLDA
02185 *                                                                *ELUGCLDA
02186 ******************************************************************ELUGCLDA
02187                                                                   ELUGCLDA
02188  905-ESTAB-ADDR-ELSGRPSP.                                         ELUGCLDA
02189      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELUGCLDA
02190      CALL 'ELUSETAD'                                              ELUGCLDA
02191         USING DFHCOMMAREA                                         ELUGCLDA
02192               ADDRESS OF GROUP-SPECIFIC-RECORD.                   ELUGCLDA
02193      IF CIA-RC-PTR-NULL                                           ELUGCLDA
02194      THEN                                                         ELUGCLDA
02195         PERFORM 991-SIGNAL-UNALLOC-AREA.                          ELUGCLDA
02196                                                                   ELUGCLDA
02197 ******************************************************************ELUGCLDA
02198 *                                                                *ELUGCLDA
02199 *    ESTABLISH ADDRESSABILITY OF CONTRACT RECORDS                *ELUGCLDA
02200 *    (INST BAS, INST SUP, PROF BAS, PROF SUP)                    *ELUGCLDA
02201 *                                                                *ELUGCLDA
02202 *    NOT THAT ANY GIVEN CONTRACT POINTER MAY BE NULL.            *ELUGCLDA
02203 *    TEST THE CIA-RC-PTR-NULL VALUE IMMEDIATELY AFTER            *ELUGCLDA
02204 *    'PERFORM'ING ONE OF THESE PARAGRAPHS TO DETERMINE           *ELUGCLDA
02205 *    WHETHER CONTRACT IS PRESENT AND CAN BE PROCESSED.           *ELUGCLDA
02206 *                                                                *ELUGCLDA
02207 ******************************************************************ELUGCLDA
02208                                                                   ELUGCLDA
02209  906-ESTAB-ADDR-ELSCONIB.                                         ELUGCLDA
02210      SET CIA-ELSCONIB-DDN TO TRUE.                                ELUGCLDA
02211      CALL 'ELUSETAD'                                              ELUGCLDA
02212         USING DFHCOMMAREA                                         ELUGCLDA
02213               ADDRESS OF CONTRACT-RECORD.                         ELUGCLDA
02214                                                                   ELUGCLDA
02215  906-ESTAB-ADDR-ELSCONIS.                                         ELUGCLDA
02216      SET CIA-ELSCONIS-DDN TO TRUE.                                ELUGCLDA
02217      CALL 'ELUSETAD'                                              ELUGCLDA
02218         USING DFHCOMMAREA                                         ELUGCLDA
02219               ADDRESS OF CONTRACT-RECORD.                         ELUGCLDA
02220                                                                   ELUGCLDA
02221  906-ESTAB-ADDR-ELSCONPB.                                         ELUGCLDA
02222      SET CIA-ELSCONPB-DDN TO TRUE.                                ELUGCLDA
02223      CALL 'ELUSETAD'                                              ELUGCLDA
02224         USING DFHCOMMAREA                                         ELUGCLDA
02225               ADDRESS OF CONTRACT-RECORD.                         ELUGCLDA
02226                                                                   ELUGCLDA
02227  906-ESTAB-ADDR-ELSCONPS.                                         ELUGCLDA
02228      SET CIA-ELSCONPS-DDN TO TRUE.                                ELUGCLDA
02229      CALL 'ELUSETAD'                                              ELUGCLDA
02230         USING DFHCOMMAREA                                         ELUGCLDA
02231               ADDRESS OF CONTRACT-RECORD.                         ELUGCLDA
02232                                                                   ELUGCLDA
02233 /*****************************************************************ELUGCLDA
02234 *                                                                *ELUGCLDA
02235 *    CALL STORAGE MANAGER                                        *ELUGCLDA
02236 *                                                                *ELUGCLDA
02237 ******************************************************************ELUGCLDA
02238                                                                   ELUGCLDA
02239  951-CALL-STORAGE-MANAGER.                                        ELUGCLDA
02240      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA.                  ELUGCLDA
02241                                                                   ELUGCLDA
02242 ******************************************************************ELUGCLDA
02243 *                                                                *ELUGCLDA
02244 *    CALL I-O MODULE                                             *ELUGCLDA
02245 *                                                                *ELUGCLDA
02246 ******************************************************************ELUGCLDA
02247                                                                   ELUGCLDA
02248  952-CALL-I-O-MODULE.                                             ELUGCLDA
02249      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA.                  ELUGCLDA
02250                                                                   ELUGCLDA
02251 /*****************************************************************ELUGCLDA
02252 *                                                                *ELUGCLDA
02253 *    ESTABLISH THE ENGLISH CONTRACT INQUIRY STORAGE              *ELUGCLDA
02254 *    MANAGEMENT ENVIRONMENT                                      *ELUGCLDA
02255 *                                                                *ELUGCLDA
02256 *  - CHECK VALIDITY OF COMMAREA, ABEND IF NOT VALID              *ELUGCLDA
02257 *  - ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA,          *ELUGCLDA
02258 *    ABEND IF THE POINTER IS NULL                                *ELUGCLDA
02259 *  - INITIALIZE THE STORAGE MANAGEMENT SYSTEM                    *ELUGCLDA
02260 *                                                                *ELUGCLDA
02261 ******************************************************************ELUGCLDA
02262                                                                   ELUGCLDA
02263  990-ESTAB-ECI-STG-ENVIRON.                                       ELUGCLDA
02264                                                                   ELUGCLDA
02265      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELUGCLDA
02266      THEN                                                         ELUGCLDA
02267         EXEC CICS ABEND ABCODE ('EL01') END-EXEC                  ELUGCLDA
02268      ELSE                                                         ELUGCLDA
02269         IF ECA-CIA-PTR = NULL                                     ELUGCLDA
02270         THEN                                                      ELUGCLDA
02271            EXEC CICS ABEND ABCODE ('EL02') END-EXEC               ELUGCLDA
02272         ELSE                                                      ELUGCLDA
02273            SET ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA           ELUGCLDA
02274             TO ECA-CIA-PTR                                        ELUGCLDA
02275            CALL 'ELUINISM'                                        ELUGCLDA
02276               USING DFHCOMMAREA                                   ELUGCLDA
02277                     ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.     ELUGCLDA
02278                                                                   ELUGCLDA
02279 /*****************************************************************ELUGCLDA
02280 *                                                                *ELUGCLDA
02281 *    SIGNAL UNALLOCATED AREA ERROR                               *ELUGCLDA
02282 *                                                                *ELUGCLDA
02283 ******************************************************************ELUGCLDA
02284                                                                   ELUGCLDA
02285  991-SIGNAL-UNALLOC-AREA.                                         ELUGCLDA
02286      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUGCLDA
02287      PERFORM 999-SIGNAL-ABEND.                                    ELUGCLDA
02288                                                                   ELUGCLDA
02289 ******************************************************************ELUGCLDA
02290 *                                                                *ELUGCLDA
02291 *    SIGNAL TABULAR NOT FOUND                                    *ELUGCLDA
02292 *                                                                *ELUGCLDA
02293 ******************************************************************ELUGCLDA
02294                                                                   ELUGCLDA
02295  992-SIGNAL-TABULAR-NOT-FOUND.                                    ELUGCLDA
02296      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELUGCLDA
02297      PERFORM 999-SIGNAL-ABEND.                                    ELUGCLDA
02298                                                                   ELUGCLDA
02299 ******************************************************************ELUGCLDA
02300 *                                                                *ELUGCLDA
02301 *    SIGNAL CRITICAL I-O ERROR                                   *ELUGCLDA
02302 *                                                                *ELUGCLDA
02303 ******************************************************************ELUGCLDA
02304                                                                   ELUGCLDA
02305  993-SIGNAL-CRITICAL-I-O-ERROR.                                   ELUGCLDA
02306      SET CIA-AB-CRITIO TO TRUE.                                   ELUGCLDA
02307      PERFORM 999-SIGNAL-ABEND.                                    ELUGCLDA
02308                                                                   ELUGCLDA
02309 ******************************************************************ELUGCLDA
02310 *                                                                *ELUGCLDA
02311 *    SIGNAL INCREASE IN TABLE SIZE NEEDED                        *ELUGCLDA
02312 *                                                                *ELUGCLDA
02313 ******************************************************************ELUGCLDA
02314                                                                   ELUGCLDA
02315  994-SIGNAL-INCR-TBL-SIZE.                                        ELUGCLDA
02316      SET CIA-AB-INCR-TBL-SIZE TO TRUE.                            ELUGCLDA
02317      PERFORM 999-SIGNAL-ABEND.                                    ELUGCLDA
02318                                                                   ELUGCLDA
02319 ******************************************************************ELUGCLDA
02320 *                                                                *ELUGCLDA
02321 *    SIGNAL ABEND                                                *ELUGCLDA
02322 *                                                                *ELUGCLDA
02323 ******************************************************************ELUGCLDA
02324                                                                   ELUGCLDA
02325  999-SIGNAL-ABEND.                                                ELUGCLDA
02326      EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.                ELUGCLDA
