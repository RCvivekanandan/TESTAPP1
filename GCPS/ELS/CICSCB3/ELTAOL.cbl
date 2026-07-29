00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELTAOL  
00003  PROGRAM-ID.        ELTAOL.                                          LV002
00004                                                                   ELTAOL  
00005  AUTHOR.            LUCY TORRES.                                  ELTAOL  
00006                     RICHARD J. LUKETICH (RE-WRITE).               ELTAOL  
00007                                                                   ELTAOL  
00008  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELTAOL  
00009                     A MUTUAL LEGAL RESERVE COMPANY                ELTAOL  
00010                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELTAOL  
00011                     233 N. MICHIGAN AVE                           ELTAOL  
00012                     CHICAGO, ILLINOIS 60601                       ELTAOL  
00013                                                                   ELTAOL  
00014  DATE-WRITTEN.      03-JUN-1987.                                  ELTAOL  
00015                     03-JAN-1992 (RE-WRITE).                       ELTAOL  
00016                                                                   ELTAOL  
00017  DATE-COMPILED.                                                   ELTAOL  
00018                                                                   ELTAOL  
00019  SECURITY.          COPYRIGHT 1986, 1992,                         ELTAOL  
00020                     HEALTH CARE SERVICE CORPORATION               ELTAOL  
00021                                                                   ELTAOL  
00022  ENVIRONMENT DIVISION.                                            ELTAOL  
00023                                                                   ELTAOL  
00024  CONFIGURATION SECTION.                                           ELTAOL  
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELTAOL  
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELTAOL  
00027                                                                   ELTAOL  
00028 /*****************************************************************ELTAOL  
00029 *                                                                *ELTAOL  
00030 *  ELTAOL - ELS:    SELECTS #AOL (OUT OF POCKET) ACCUMULATORS AND*ELTAOL  
00031 *                   SETUPS THE INFORMATION TO BE PROCESSED BY    *ELTAOL  
00032 *                   THE OUT OF POCKET GENERATOR MODULE. THE ACCUMSELTAOL  
00033 *                   ARE SELECTED FROM THE GROUP SPECIFIC AND     *ELTAOL  
00034 *                   CONTRACT LEVEL PROCESSING.                   *ELTAOL  
00035 *                                                                *ELTAOL  
00036 ******************************************************************ELTAOL  
00037 *                                                                *ELTAOL  
00038 *                      MAINTENANCE HISTORY                       *ELTAOL  
00039 *                                                                *ELTAOL  
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELTAOL  
00041 * ----- ----------- --- ----- ---------------------------------- *ELTAOL  
00042 * 01.00 03-JUN-1987 LET       CREATED                            *ELTAOL  
00043 * 01.01 25-SEP-1987 LET       ADDED DEFINITION DATA FIELD        *ELTAOL  
00044 *                                                                *ELTAOL  
00045 * 01.02 17-NOV-1987 REB       MADE CHANGES TO CORRESPOND TO NEW  *ELTAOL  
00046 *                             VERSION OF COPYBOOK ELSACUMC.      *ELTAOL  
00047 *                                                                *ELTAOL  
00048 * 01.07    SEP-1991 RKH    1. ADDED LOGIC FOR:                   *ELTAOL  
00049 *    ISSR #12010                A.  NEW PATIENT AGE FIELDS       *ELTAOL  
00050 *                               B.  RELATIONSHIP IND VALUE       *ELTAOL  
00051 *                          2. REVISE LOGIC TO LOAD INT ACCUMS    *ELTAOL  
00052 *                             INTO VARIABLE LEVEL TABLE          *ELTAOL  
00053 *                          3. ADDED COPYBOOKS :                  *ELTAOL  
00054 *                               A. GCTIBGR   - IBGR TAB          *ELTAOL  
00055 *                               B. GCTIPGT   - IPGT TAB          *ELTAOL  
00056 *                               C. ELSCFTB2  - PROVIDER TYPE     *ELTAOL  
00057 *                                         COMPARE TABLE          *ELTAOL  
00058 *                          4. ADD LOGIC TO INSPECT #IPGT AND     *ELTAOL  
00059 *                             #IBGR INT TABS TO DETERMINE IF     *ELTAOL  
00060 *                             AN OCCURRANCE IS THE SELECTED      *ELTAOL  
00061 *                             PROVIDER CLASS.                    *ELTAOL  
00062 *                                                                *ELTAOL  
00063 * 02.01 04-FEB-1992 JPB       CLONED FROM ELTABM, MADE CHANGES   *ELTAOL  
00064 *                             FOR OUT OF POCKET ASC/DES LOGIC.   *ELTAOL  
00065 *                                                                *ELTAOL  
00066 * 03.00 02-DEC-1998 AKK       ADD SUPPORT FOR NEW TAB #ACP       *ELTAOL  
00067 *                                                                *ELTAOL  
00068 * 03.01 25-AUG-2000 AKK       ADD SUPPORT FOR NEW TAB #IPGS      *ELTAOL  
00069 *                                                                *ELTAOL  
00070 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST    *ELTAOL  
00069 *                                                                *ELTAOL  
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00056 *                                                                *ELGABMCC
00071 ******************************************************************ELTAOL  
00072      TITLE  'ELTAOL          WORKING STORAGE'.                    ELTAOL  
00073  DATA DIVISION.                                                   ELTAOL  
00074                                                                   ELTAOL  
00075  WORKING-STORAGE SECTION.                                         ELTAOL  
00076                                                                   ELTAOL  
00077  01  SWITCHES.                                                    ELTAOL  
00078      02                                      PICTURE  X(01).      ELTAOL  
00079         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELTAOL  
00080         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELTAOL  
00081                                                                   ELTAOL  
00082      02 OCCURRENCE-APPLIES                   PICTURE  X(01).      ELTAOL  
00083         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELTAOL  
00084         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELTAOL  
00085      02                                      PICTURE  X(01).      ELTAOL  
00086         88 SW-HAS-IBGR                       VALUE 'Y'.           ELTAOL  
00087         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELTAOL  
00088      02                                      PICTURE  X(01).      ELTAOL  
00089         88 SW-HAS-IDGD                       VALUE 'Y'.           ELTAOL  
00090         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELTAOL  
00091      02                                      PICTURE  X(01).      ELTAOL  
00092         88 SW-HAS-IPGN                       VALUE 'Y'.           ELTAOL  
00093         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELTAOL  
00094      02                                      PICTURE  X(01).      ELTAOL  
00095         88 SW-HAS-IPGP                       VALUE 'Y'.           ELTAOL  
00096         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELTAOL  
00097      02                                      PICTURE  X(01).      ELTAOL  
00098         88 SW-HAS-IPGT                       VALUE 'Y'.           ELTAOL  
00099         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELTAOL  
00100      02                                      PICTURE  X(01).      ELTAOL  
00101         88 SW-HAS-IPGS                       VALUE 'Y'.           ELTAOL  
00102         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELTAOL  
00103      02                                      PICTURE  X(01).      ELTAOL  
00104         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELTAOL  
00105         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELTAOL  
00106      02                                      PICTURE  X(01).      ELTAOL  
00107         88 SW-INTRNL-INST-PROV-CL            VALUE 'Y'.           ELTAOL  
00108         88 SW-INTRNL-NOT-INST-PROV-CL        VALUE 'N'.           ELTAOL  
00109         88 SW-INTRNL-INST-PROV-CL-NOT-DET VALUE 'X'.              ELTAOL  
00110      02                                      PICTURE  X(01).      ELTAOL  
00111         88 SW-INTRNL-PROF-PROV-CL            VALUE 'Y'.           ELTAOL  
00112         88 SW-INTRNL-NOT-PROF-PROV-CL        VALUE 'N'.           ELTAOL  
00113         88 SW-INTRNL-PROF-PROV-CL-NOT-DET VALUE 'X'.              ELTAOL  
00114      02                                      PICTURE  X(01).      ELTAOL  
00115         88 SW-INTRNL-PROF-PROV-SP            VALUE 'Y'.           ELTAOL  
00116         88 SW-INTRNL-NOT-PROF-PROV-SP        VALUE 'N'.           ELTAOL  
00117         88 SW-INTRNL-PROF-PROV-SP-NOT-DET VALUE 'X'.              ELTAOL  
00118      02                                      PICTURE  X(01).      ELTAOL  
00119         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELTAOL  
00120         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELTAOL  
00121      02                                      PICTURE  X(01).      ELTAOL  
00122         88 SW-MATCHING-ENTRY-FOUND           VALUE 'Y'.           ELTAOL  
00123         88 SW-MATCHING-ENTRY-NOT-FOUND       VALUE 'N'.           ELTAOL  
00124      02                                      PICTURE  X(01).      ELTAOL  
00125         88 SW-SORT-COMPLETED                 VALUE 'Y'.           ELTAOL  
00126         88 SW-SORT-NOT-COMPLETED             VALUE 'N'.           ELTAOL  
00127                                                                   ELTAOL  
00128  01  WS-PROVISION-ARGUMENT.                                       ELTAOL  
00129      02                          PICTURE  X(05).                  ELTAOL  
00130      02 WS-PROVISION-CL          PICTURE  X(01).                  ELTAOL  
00131         88 INST-CL               VALUE 'A', 'B', 'W'.             ELTAOL  
00132         88 PROF-CL               VALUE 'C', 'D', 'E'.             ELTAOL  
00133                                                                   ELTAOL  
00134  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELTAOL  
00135      88 WS-LOB-INST              VALUE '1'.                       ELTAOL  
00136      88 WS-LOB-PROF              VALUE '2'.                       ELTAOL  
00137      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELTAOL  
00138      88 WS-LOB-BOTH              VALUE '3', '4', '5', '6', '7'.   ELTAOL  
00139                                                                   ELTAOL  
00140  01  PROGRAM-CONSTANTS.                                           ELTAOL  
00141      02 PC-AOL                   PICTURE  X(06) VALUE '#AOL  '.   ELTAOL  
00142      02 PC-GCT-MAX-SUB           PICTURE S9(04) COMP.             ELTAOL  
00143      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELTAOL  
00144      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELTAOL  
00145      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELTAOL  
00146      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELTAOL  
00147      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELTAOL  
00148      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGS '.   ELTAOL  
00149      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELTAOL  
00150                                                                   ELTAOL  
00151  01  WS-WORK-FIELDS.                                              ELTAOL  
00152      02 WS-AOL-SUB               PICTURE S9(04) COMP.             ELTAOL  
00153      02 WS-AOL-ACCUM-CNT         PICTURE S9(04) COMP.             ELTAOL  
00154      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELTAOL  
00155      02 WS-OCCURRENCE-SUB        PICTURE S9(04) COMP.             ELTAOL  
00156      02 WS-SAVE-SUB              PICTURE S9(04) COMP.             ELTAOL  
00157      02 WS-SAVE-INDEX            USAGE IS INDEX.                  ELTAOL  
00158      02 SORT-SUB                 PICTURE S9(04) COMP.             ELTAOL  
00159      02 TEST-SUB                 PICTURE S9(04) COMP.             ELTAOL  
00160                                                                   ELTAOL  
00161  01  WS-MAX-INDEX-VALUES.                                         ELTAOL  
00162      02  WS-MAX-GX1-INDEX        USAGE IS INDEX.                  ELTAOL  
00163      02  WS-MAX-GX3-INDEX        USAGE IS INDEX.                  ELTAOL  
00164      02  WS-MAX-GXS-INDEX        USAGE IS INDEX.                  ELTAOL  
00165      02  WS-MAX-GAD-INDEX        USAGE IS INDEX.                  ELTAOL  
00166      02  WS-MAX-GAD-INT-INDEX    USAGE IS INDEX.                  ELTAOL  
00167      02  WS-MAX-GCT-INDEX        USAGE IS INDEX.                  ELTAOL  
00168      02  WS-MAX-GCG-INDEX        USAGE IS INDEX.                  ELTAOL  
00169                                                                   ELTAOL  
00170  01  WS-POINTERS.                                                 ELTAOL  
00171      02  WS-INST-CNTRCT-PTR      POINTER.                         ELTAOL  
00172      02  WS-PROF-CNTRCT-PTR      POINTER.                         ELTAOL  
00173                                                                   ELTAOL  
00174  01  ACCUM-HOLD-TBL.                                              ELTAOL  
00175      02  ACCUM-SLOT-NBR          PICTURE S9(07) COMP-3            ELTAOL  
00176                                  OCCURS 5 TIMES.                  ELTAOL  
00177                                                                   ELTAOL  
00178  01  WS-OCCURRENCE-PROCESSED-TBL.                                 ELTAOL  
00179      02                          PICTURE X                        ELTAOL  
00180                                  OCCURS 44 TIMES                  ELTAOL  
00181                                  INDEXED BY WS-OCCURRENCE-INDEX.  ELTAOL  
00182          88  WS-OCCURRENCE-PROCESSED           VALUE 'P'.         ELTAOL  
00183          88  WS-OCCURRENCE-NOT-PROCESSED       VALUE ' '.         ELTAOL  
00184                                                                   ELTAOL  
00185  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELTAOL  
00186      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTAOL  
00187      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTAOL  
00188      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTAOL  
00189      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTAOL  
00190      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTAOL  
00191      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTAOL  
00192                                                                   ELTAOL  
00193  01  WS-ASCEND-DESCEND-ENTRY-HOLD PICTURE X(28).                  ELTAOL  
00194                                                                   ELTAOL  
00195 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELTAOL  
00196      COPY ELSCFTB2.                                               ELTAOL  
00197                                                                   ELTAOL  
00198 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELTAOL  
00199      COPY ELSCFTB9.                                               ELTAOL  
00200                                                                   ELTAOL  
00201      TITLE  'ELTAOL          LINKAGE SECTION'                     ELTAOL  
00202  LINKAGE SECTION.                                                 ELTAOL  
00203  01  DFHCOMMAREA.                                                 ELTAOL  
00204      COPY ELSCOMMC.                                               ELTAOL  
00205 /                                                                 ELTAOL  
00206      COPY ELSCIA2C.                                               ELTAOL  
00207 /                                                                 ELTAOL  
00208      COPY ELSIOPMC.                                               ELTAOL  
00209 /                                                                 ELTAOL  
00210      COPY ELSKEYSC.                                               ELTAOL  
00211 /                                                                 ELTAOL  
00212      COPY ELSSRTPC.                                               ELTAOL  
00213 /                                                                 ELTAOL  
00214      COPY ELSSSCBC.                                               ELTAOL  
00215 /                                                                 ELTAOL  
00216  01  GCG-GRP-SPEC-RECORD-AREA.                                    ELTAOL  
00217      COPY GCGROUPC.                                               ELTAOL  
00218 /                                                                 ELTAOL  
00219  01  GCT-CONTRACT-RECORD-AREA.                                    ELTAOL  
00220      COPY GCCONTRC.                                               ELTAOL  
00221 /                                                                 ELTAOL  
00222  01  GAD-RECORD-AREA.                                             ELTAOL  
00223      COPY GCTAOLC.                                                ELTAOL  
00224 /                                                                 ELTAOL  
00225      COPY ELSACUMC.                                               ELTAOL  
00226 /                                                                 ELTAOL  
00227  01  GX1-RECORD-AREA.                                             ELTAOL  
00228      COPY GCTIBGRC.                                               ELTAOL  
00229 /                                                                 ELTAOL  
00230  01  GX3-RECORD-AREA.                                             ELTAOL  
00231      COPY GCTIPGTC.                                               ELTAOL  
00232 /                                                                 ELTAOL  
00233  01  GXS-RECORD-AREA.                                             ELTAOL  
00234      COPY GCTIPGSC.                                               ELTAOL  
00235      TITLE  'ELTAOL          PROCEDURE DIVISION'.                 ELTAOL  
00236 ************************************************************      ELTAOL  
00237 *                                                          *      ELTAOL  
00238 *    ELTAOL MAINLINE                                       *      ELTAOL  
00239 *                                                          *      ELTAOL  
00240 ************************************************************      ELTAOL  
00241                                                                   ELTAOL  
00242  PROCEDURE DIVISION.                                              ELTAOL  
00243                                                                   ELTAOL  
00244      PERFORM 0010-INITIALIZATION.                                 ELTAOL  
00245      PERFORM 0100-PROCESS.                                        ELTAOL  
00246      GOBACK.                                                      ELTAOL  
00247                                                                   ELTAOL  
00248 ************************************************************      ELTAOL  
00249 *                                                          *      ELTAOL  
00250 *    INITIALIZATION                                        *      ELTAOL  
00251 *                                                          *      ELTAOL  
00252 ************************************************************      ELTAOL  
00253                                                                   ELTAOL  
00254  0010-INITIALIZATION.                                             ELTAOL  
00255      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELTAOL  
00256      PERFORM 0060-EST-ADR-KEY-WK-AREA.                            ELTAOL  
00257      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELTAOL  
00258      PERFORM 0120-EST-ADR-GRP-SPC.                                ELTAOL  
00259      PERFORM 0190-INIT-DATA.                                      ELTAOL  
00260                                                                   ELTAOL  
00261 ************************************************************      ELTAOL  
00262 *                                                          *      ELTAOL  
00263 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELTAOL  
00264 *                                                          *      ELTAOL  
00265 ************************************************************      ELTAOL  
00266                                                                   ELTAOL  
00267  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELTAOL  
00268      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTAOL  
00269      THEN                                                         ELTAOL  
00270         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELTAOL  
00271      ELSE                                                         ELTAOL  
00272         IF ECA-CIA-PTR = NULL                                     ELTAOL  
00273         THEN                                                      ELTAOL  
00274            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELTAOL  
00275         ELSE                                                      ELTAOL  
00276            CALL 'ELUINISM' USING DFHCOMMAREA                      ELTAOL  
00277               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELTAOL  
00278               END-CALL                                            ELTAOL  
00279            SET CIA-ELSSSCB-DDN TO TRUE                            ELTAOL  
00280            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELTAOL  
00281               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELTAOL  
00282               END-CALL                                            ELTAOL  
00283            IF CIA-RC-PTR-NULL                                     ELTAOL  
00284            THEN                                                   ELTAOL  
00285               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELTAOL  
00286               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELTAOL  
00287            ELSE                                                   ELTAOL  
00288               CONTINUE                                            ELTAOL  
00289            END-IF                                                 ELTAOL  
00290         END-IF                                                    ELTAOL  
00291      END-IF.                                                      ELTAOL  
00292                                                                   ELTAOL  
00293 /***********************************************************      ELTAOL  
00294 *                                                          *      ELTAOL  
00295 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELTAOL  
00296 *                                                          *      ELTAOL  
00297 ************************************************************      ELTAOL  
00298                                                                   ELTAOL  
00299  0060-EST-ADR-KEY-WK-AREA.                                        ELTAOL  
00300      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTAOL  
00301      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAOL  
00302         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELTAOL  
00303         END-CALL.                                                 ELTAOL  
00304      IF CIA-RC-PTR-NULL                                           ELTAOL  
00305         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTAOL  
00306         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTAOL  
00307      END-IF.                                                      ELTAOL  
00308                                                                   ELTAOL  
00309 ************************************************************      ELTAOL  
00310 *                                                          *      ELTAOL  
00311 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTAOL  
00312 *                                                          *      ELTAOL  
00313 ************************************************************      ELTAOL  
00314                                                                   ELTAOL  
00315  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELTAOL  
00316      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTAOL  
00317      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAOL  
00318         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELTAOL  
00319         END-CALL.                                                 ELTAOL  
00320      IF CIA-RC-PTR-NULL                                           ELTAOL  
00321         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTAOL  
00322         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTAOL  
00323      END-IF.                                                      ELTAOL  
00324                                                                   ELTAOL  
00325 ************************************************************      ELTAOL  
00326 *                                                          *      ELTAOL  
00327 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD     *      ELTAOL  
00328 *                                                          *      ELTAOL  
00329 ************************************************************      ELTAOL  
00330                                                                   ELTAOL  
00331  0120-EST-ADR-GRP-SPC.                                            ELTAOL  
00332      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTAOL  
00333      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAOL  
00334         ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                       ELTAOL  
00335         END-CALL.                                                 ELTAOL  
00336      IF CIA-RC-PTR-NULL                                           ELTAOL  
00337         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTAOL  
00338         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTAOL  
00339      END-IF.                                                      ELTAOL  
00340                                                                   ELTAOL  
00341 /***********************************************************      ELTAOL  
00342 *                                                          *      ELTAOL  
00343 *    INITIALIZE DATA AREAS                                 *      ELTAOL  
00344 *                                                          *      ELTAOL  
00345 ************************************************************      ELTAOL  
00346                                                                   ELTAOL  
00347  0190-INIT-DATA.                                                  ELTAOL  
00348      COMPUTE PC-GCT-MAX-SUB =   LENGTH OF GCT-CONT-TAB-PTRS       ELTAOL  
00349                               / LENGTH OF GCT-CON-TAB-ID-SLOT.    ELTAOL  
00350      SET GCT-INDEX                TO PC-GCT-MAX-SUB.              ELTAOL  
00351      SET WS-MAX-GCT-INDEX         TO GCT-INDEX.                   ELTAOL  
00352      SET GCG-INDEX                TO GCG-COUNT-TAB-PROVN-POINTERS.ELTAOL  
00353      SET WS-MAX-GCG-INDEX         TO GCG-INDEX.                   ELTAOL  
00354      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELTAOL  
00355      INITIALIZE WS-AOL-ACCUM-CNT.                                 ELTAOL  
00356                                                                   ELTAOL  
00357 /***********************************************************      ELTAOL  
00358 *                                                          *      ELTAOL  
00359 *        PROCESS                                           *      ELTAOL  
00360 *                                                          *      ELTAOL  
00361 ************************************************************      ELTAOL  
00362                                                                   ELTAOL  
00363  0100-PROCESS.                                                    ELTAOL  
00364      PERFORM 0110-SCAN-GRP-SPC-FOR-ACCUMS.                        ELTAOL  
00365      PERFORM 0120-SCAN-CONTRACTS-FOR-ACCUMS.                      ELTAOL  
00366                                                                   ELTAOL  
00367      IF WS-AOL-ACCUM-CNT >  0                                     ELTAOL  
00368      THEN                                                         ELTAOL  
00369          PERFORM 0210-SCAN-FOR-APPLIC-OCCRNCS                     ELTAOL  
00370      END-IF.                                                      ELTAOL  
00371                                                                   ELTAOL  
00372      IF SW-NO-APPLIC-ACCUM-FOUND                                  ELTAOL  
00373      THEN                                                         ELTAOL  
00374         EVALUATE TRUE                                             ELTAOL  
00375            WHEN SSB-PROV-CLASS-INST                               ELTAOL  
00376               SET SRP-INST-NOT-APPLICABLE TO TRUE                 ELTAOL  
00377            WHEN SSB-PROV-CLASS-PROF                               ELTAOL  
00378               SET SRP-PROF-NOT-APPLICABLE TO TRUE                 ELTAOL  
00379            WHEN SSB-PROV-CLASS-BOTH                               ELTAOL  
00380               SET SRP-NO-ACCUMS-FOUND TO TRUE                     ELTAOL  
00381            WHEN OTHER                                             ELTAOL  
00382               SET CIA-AB-PGM-LOGIC TO TRUE                        ELTAOL  
00383               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELTAOL  
00384            END-EVALUATE                                           ELTAOL  
00385      END-IF.                                                      ELTAOL  
00386                                                                   ELTAOL  
00387      SET SRP-TOPIC-ACCUM TO TRUE.                                 ELTAOL  
00388                                                                   ELTAOL  
00389 * -- LINK TO THE OUTPUT GENERATOR                                 ELTAOL  
00390      EXEC CICS LINK PROGRAM ('ELGAOL') COMMAREA (DFHCOMMAREA)     ELTAOL  
00391         END-EXEC.                                                 ELTAOL  
00392                                                                   ELTAOL  
00393 /***********************************************************      ELTAOL  
00394 *                                                          *      ELTAOL  
00395 *    SCAN GROUP SPECIFIC RECORD FOR ACCUMULATORS           *      ELTAOL  
00396 *                                                          *      ELTAOL  
00397 ************************************************************      ELTAOL  
00398                                                                   ELTAOL  
00399  0110-SCAN-GRP-SPC-FOR-ACCUMS.                                    ELTAOL  
00400      PERFORM WITH TEST BEFORE                                     ELTAOL  
00401         VARYING GCG-INDEX FROM 1 BY 1                             ELTAOL  
00402           UNTIL GCG-INDEX = WS-MAX-GCG-INDEX                      ELTAOL  
00403                 OR GCG-TAB-ID (GCG-INDEX) > PC-AOL                ELTAOL  
00404 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELTAOL  
00405         IF     GCG-TAB-ID (GCG-INDEX)  =  PC-AOL                  ELTAOL  
00406            AND GCG-TAB-SLOT-NO (GCG-INDEX)  >  ZERO               ELTAOL  
00407         THEN                                                      ELTAOL  
00408 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELTAOL  
00409            MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO WS-SLOT-NBR        ELTAOL  
00410            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELTAOL  
00411         END-IF                                                    ELTAOL  
00412         END-PERFORM.                                              ELTAOL  
00413                                                                   ELTAOL  
00414 /***********************************************************      ELTAOL  
00415 *                                                          *      ELTAOL  
00416 *    SCAN CONTRACT RECORDS FOR ACCUMULATORS                *      ELTAOL  
00417 *                                                          *      ELTAOL  
00418 ************************************************************      ELTAOL  
00419                                                                   ELTAOL  
00420  0120-SCAN-CONTRACTS-FOR-ACCUMS.                                  ELTAOL  
00421      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELTAOL  
00422      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELTAOL  
00423                                                                   ELTAOL  
00424      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTAOL  
00425      THEN                                                         ELTAOL  
00426          PERFORM 0130-SCAN-INST-BAS                               ELTAOL  
00427      END-IF.                                                      ELTAOL  
00428                                                                   ELTAOL  
00429      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTAOL  
00430      THEN                                                         ELTAOL  
00431          PERFORM 0140-SCAN-PROF-BAS                               ELTAOL  
00432      END-IF.                                                      ELTAOL  
00433                                                                   ELTAOL  
00434      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELTAOL  
00435      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELTAOL  
00436                                                                   ELTAOL  
00437      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTAOL  
00438      THEN                                                         ELTAOL  
00439          PERFORM 0150-SCAN-INST-SUP                               ELTAOL  
00440      END-IF.                                                      ELTAOL  
00441                                                                   ELTAOL  
00442      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTAOL  
00443      THEN                                                         ELTAOL  
00444          PERFORM 0160-SCAN-PROF-SUP                               ELTAOL  
00445      END-IF.                                                      ELTAOL  
00446                                                                   ELTAOL  
00447 /***********************************************************      ELTAOL  
00448 *                                                          *      ELTAOL  
00449 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELTAOL  
00450 *                                                          *      ELTAOL  
00451 ************************************************************      ELTAOL  
00452                                                                   ELTAOL  
00453  0130-SCAN-INST-BAS.                                              ELTAOL  
00454      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTAOL  
00455      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAOL  
00456         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTAOL  
00457         END-CALL.                                                 ELTAOL  
00458      SET WS-INST-CNTRCT-PTR                                       ELTAOL  
00459       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTAOL  
00460                                                                   ELTAOL  
00461      IF CIA-RC-PTR-NULL                                           ELTAOL  
00462      THEN                                                         ELTAOL  
00463         CONTINUE                                                  ELTAOL  
00464      ELSE                                                         ELTAOL  
00465         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTAOL  
00466      END-IF.                                                      ELTAOL  
00467                                                                   ELTAOL  
00468 ************************************************************      ELTAOL  
00469 *                                                          *      ELTAOL  
00470 *    SCAN PROFESSIONAL BASIC CONTRACT RECORD               *      ELTAOL  
00471 *                                                          *      ELTAOL  
00472 ************************************************************      ELTAOL  
00473                                                                   ELTAOL  
00474  0140-SCAN-PROF-BAS.                                              ELTAOL  
00475      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTAOL  
00476      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAOL  
00477         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTAOL  
00478         END-CALL.                                                 ELTAOL  
00479      SET WS-PROF-CNTRCT-PTR                                       ELTAOL  
00480       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTAOL  
00481                                                                   ELTAOL  
00482      IF    CIA-RC-PTR-NULL                                        ELTAOL  
00483         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELTAOL  
00484      THEN                                                         ELTAOL  
00485         CONTINUE                                                  ELTAOL  
00486      ELSE                                                         ELTAOL  
00487         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTAOL  
00488      END-IF.                                                      ELTAOL  
00489                                                                   ELTAOL  
00490 /***********************************************************      ELTAOL  
00491 *                                                          *      ELTAOL  
00492 *    SCAN INSTITUTIONAL SUPPLEMENTAL CONTRACT RECORD       *      ELTAOL  
00493 *                                                          *      ELTAOL  
00494 ************************************************************      ELTAOL  
00495                                                                   ELTAOL  
00496  0150-SCAN-INST-SUP.                                              ELTAOL  
00497      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTAOL  
00498      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAOL  
00499         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTAOL  
00500         END-CALL.                                                 ELTAOL  
00501      SET WS-INST-CNTRCT-PTR                                       ELTAOL  
00502       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTAOL  
00503                                                                   ELTAOL  
00504      IF CIA-RC-PTR-NULL                                           ELTAOL  
00505      THEN                                                         ELTAOL  
00506         CONTINUE                                                  ELTAOL  
00507      ELSE                                                         ELTAOL  
00508         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTAOL  
00509      END-IF.                                                      ELTAOL  
00510                                                                   ELTAOL  
00511 ************************************************************      ELTAOL  
00512 *                                                          *      ELTAOL  
00513 *    SCAN PROFESSIONAL SUPPLEMENTAL CONTRACT RECORD        *      ELTAOL  
00514 *                                                          *      ELTAOL  
00515 ************************************************************      ELTAOL  
00516                                                                   ELTAOL  
00517  0160-SCAN-PROF-SUP.                                              ELTAOL  
00518      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTAOL  
00519      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAOL  
00520         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTAOL  
00521         END-CALL.                                                 ELTAOL  
00522      SET WS-PROF-CNTRCT-PTR                                       ELTAOL  
00523       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTAOL  
00524                                                                   ELTAOL  
00525      IF    CIA-RC-PTR-NULL                                        ELTAOL  
00526         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELTAOL  
00527      THEN                                                         ELTAOL  
00528         CONTINUE                                                  ELTAOL  
00529      ELSE                                                         ELTAOL  
00530         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTAOL  
00531      END-IF.                                                      ELTAOL  
00532                                                                   ELTAOL  
00533 /***********************************************************      ELTAOL  
00534 *                                                          *      ELTAOL  
00535 *    SCAN A CONTRACT RECORD FOR ACCUMULATORS               *      ELTAOL  
00536 *                                                          *      ELTAOL  
00537 ************************************************************      ELTAOL  
00538                                                                   ELTAOL  
00539  0170-SCAN-CONTRACT-FOR-ACCUMS.                                   ELTAOL  
00540      PERFORM WITH TEST BEFORE                                     ELTAOL  
00541         VARYING GCT-TAB-INDEX FROM 1 BY 1                         ELTAOL  
00542           UNTIL GCT-TAB-INDEX > WS-MAX-GCT-INDEX                  ELTAOL  
00543                 OR GCT-CON-TAB-ID (GCT-TAB-INDEX) > PC-AOL        ELTAOL  
00544 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELTAOL  
00545         IF     GCT-CON-TAB-ID (GCT-TAB-INDEX) = PC-AOL            ELTAOL  
00546            AND GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  > ZEROS          ELTAOL  
00547         THEN                                                      ELTAOL  
00548 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELTAOL  
00549            MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  TO  WS-SLOT-NBR ELTAOL  
00550            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELTAOL  
00551         END-IF                                                    ELTAOL  
00552         END-PERFORM.                                              ELTAOL  
00553                                                                   ELTAOL  
00554 /***********************************************************      ELTAOL  
00555 *                                                          *      ELTAOL  
00556 *    SAVE UNIQUE ACCUMULATOR SLOT NUMBER                   *      ELTAOL  
00557 *                                                          *      ELTAOL  
00558 ************************************************************      ELTAOL  
00559                                                                   ELTAOL  
00560  0200-SAVE-UNQ-ACCUM-SLOT-NBR.                                    ELTAOL  
00561                                                                   ELTAOL  
00562 * -- SCAN TABLE OF ACCUM SLOT NUMBERS FOR DUPLICATE               ELTAOL  
00563      SET SW-UNQ-SLOT-NBR TO TRUE.                                 ELTAOL  
00564      PERFORM WITH TEST BEFORE                                     ELTAOL  
00565         VARYING WS-AOL-SUB FROM 1 BY 1                            ELTAOL  
00566           UNTIL    WS-AOL-SUB > WS-AOL-ACCUM-CNT                  ELTAOL  
00567                 OR SW-DUP-SLOT-NBR                                ELTAOL  
00568         IF WS-SLOT-NBR = ACCUM-SLOT-NBR (WS-AOL-SUB)              ELTAOL  
00569         THEN                                                      ELTAOL  
00570            SET SW-DUP-SLOT-NBR TO TRUE                            ELTAOL  
00571         END-IF                                                    ELTAOL  
00572         END-PERFORM.                                              ELTAOL  
00573                                                                   ELTAOL  
00574 * -- IF SLOT NUMBER IS UNIQUE, ADD IT TO THE HOLD TABLE           ELTAOL  
00575      IF SW-UNQ-SLOT-NBR                                           ELTAOL  
00576      THEN                                                         ELTAOL  
00577         ADD 1 TO  WS-AOL-ACCUM-CNT                                ELTAOL  
00578         MOVE WS-SLOT-NBR TO ACCUM-SLOT-NBR(WS-AOL-ACCUM-CNT)      ELTAOL  
00579      END-IF.                                                      ELTAOL  
00580                                                                   ELTAOL  
00581 /***********************************************************      ELTAOL  
00582 *                                                          *      ELTAOL  
00583 *    SCAN AOL ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELTAOL  
00584 *                                                          *      ELTAOL  
00585 ************************************************************      ELTAOL  
00586                                                                   ELTAOL  
00587  0210-SCAN-FOR-APPLIC-OCCRNCS.                                    ELTAOL  
00588      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELTAOL  
00589      PERFORM 0220-DELETE-AOL-SUMMARY-FILE.                        ELTAOL  
00590      PERFORM 0230-ALLOC-WORKFILE-REC-AREA.                        ELTAOL  
00591                                                                   ELTAOL  
00592 * -- READ AND SCAN EACH ACCUMULATOR TABULAR                       ELTAOL  
00593      PERFORM WITH TEST BEFORE                                     ELTAOL  
00594         VARYING WS-AOL-SUB FROM 1 BY 1                            ELTAOL  
00595           UNTIL WS-AOL-SUB > WS-AOL-ACCUM-CNT                     ELTAOL  
00596 *    -- OBTAIN ACCUMULATOR TABULAR RECORD                         ELTAOL  
00597         MOVE PC-AOL TO KWA-PROVISION-ID                           ELTAOL  
00598         MOVE ACCUM-SLOT-NBR (WS-AOL-SUB) TO KWA-PROVISION-SLOT-NO ELTAOL  
00599         MOVE SPACES TO WS-OCCURRENCE-PROCESSED-TBL                ELTAOL  
00600         PERFORM 0240-READ-TABULAR-REC                             ELTAOL  
00601 *    -- SCAN ACCUMULATOR TABULAR                                  ELTAOL  
00602         PERFORM WITH TEST BEFORE                                  ELTAOL  
00603            VARYING WS-OCCURRENCE-SUB FROM 1 BY 1                  ELTAOL  
00604            UNTIL WS-OCCURRENCE-SUB >= GAD-ENTRY-COUNT             ELTAOL  
00605             IF WS-OCCURRENCE-NOT-PROCESSED (WS-OCCURRENCE-SUB)    ELTAOL  
00606              THEN                                                 ELTAOL  
00607              SET GAD-INDEX                                        ELTAOL  
00608                  TO WS-OCCURRENCE-SUB                             ELTAOL  
00609              SET WS-OCCURRENCE-INDEX                              ELTAOL  
00610                  TO GAD-INDEX                                     ELTAOL  
00611               PERFORM 0300-TEST-AOL-OCCURRENCE                    ELTAOL  
00612             END-IF                                                ELTAOL  
00613         END-PERFORM                                               ELTAOL  
00614      END-PERFORM.                                                 ELTAOL  
00615                                                                   ELTAOL  
00616                                                                   ELTAOL  
00617 /***********************************************************      ELTAOL  
00618 *                                                          *      ELTAOL  
00619 *        DELETE AOL SUMMARY FILE                           *      ELTAOL  
00620 *                                                          *      ELTAOL  
00621 ************************************************************      ELTAOL  
00622                                                                   ELTAOL  
00623  0220-DELETE-AOL-SUMMARY-FILE.                                    ELTAOL  
00624      SET IOP-DEL TO TRUE.                                         ELTAOL  
00625      SET IOP-FCQ-NONE TO TRUE.                                    ELTAOL  
00626      SET IOP-KVQ-NONE TO TRUE.                                    ELTAOL  
00627      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTAOL  
00628                                                                   ELTAOL  
00629 /***********************************************************      ELTAOL  
00630 *                                                          *      ELTAOL  
00631 *    ALLOCATE WORKFILE RECORD AREA                         *      ELTAOL  
00632 *                                                          *      ELTAOL  
00633 ************************************************************      ELTAOL  
00634                                                                   ELTAOL  
00635  0230-ALLOC-WORKFILE-REC-AREA.                                    ELTAOL  
00636      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELTAOL  
00637      SET CIA-STG-GETMAIN TO TRUE.                                 ELTAOL  
00638      SET IOP-GETMAIN-REC TO TRUE.                                 ELTAOL  
00639      COMPUTE IOP-MAX-REC-LEN =                                    ELTAOL  
00640              LENGTH OF ACCUM-FIXED-AREA                           ELTAOL  
00641 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELTAOL  
00642            + LENGTH OF ACCUM-VARIABLE-AREA                        ELTAOL  
00643            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELTAOL  
00644 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELTAOL  
00645 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELTAOL  
00646                                                                   ELTAOL  
00647      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTAOL  
00648      IF IOP-REC-PTR = NULLS                                       ELTAOL  
00649      THEN                                                         ELTAOL  
00650         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTAOL  
00651         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTAOL  
00652      ELSE                                                         ELTAOL  
00653         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELTAOL  
00654      END-IF.                                                      ELTAOL  
00655                                                                   ELTAOL  
00656 /***********************************************************      ELTAOL  
00657 *                                                          *      ELTAOL  
00658 *    READ TABULAR RECORD                                   *      ELTAOL  
00659 *                                                          *      ELTAOL  
00660 ************************************************************      ELTAOL  
00661                                                                   ELTAOL  
00662  0240-READ-TABULAR-REC.                                           ELTAOL  
00663      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELTAOL  
00664      SET IOP-RD TO TRUE.                                          ELTAOL  
00665      SET IOP-FCQ-NONE TO TRUE.                                    ELTAOL  
00666      SET IOP-KVQ-EQ TO TRUE.                                      ELTAOL  
00667      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTAOL  
00668      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTAOL  
00669      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTAOL  
00670      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTAOL  
00671                                                                   ELTAOL  
00672      EVALUATE TRUE                                                ELTAOL  
00673        WHEN IOP-RC-OK                                             ELTAOL  
00674           SET ADDRESS OF GAD-RECORD-AREA TO IOP-REC-PTR           ELTAOL  
00675           SET IOP-REC-PTR TO NULLS                                ELTAOL  
00676           SET GAD-INDEX   TO GAD-ENTRY-COUNT                      ELTAOL  
00677           SET WS-MAX-GAD-INDEX TO GAD-INDEX                       ELTAOL  
00678        WHEN IOP-RC-NOTFND                                         ELTAOL  
00679           SET CIA-AB-NOTFND-GCTABULR TO TRUE                      ELTAOL  
00680           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELTAOL  
00681        WHEN OTHER                                                 ELTAOL  
00682           SET CIA-AB-CRITIO TO TRUE                               ELTAOL  
00683           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELTAOL  
00684        END-EVALUATE.                                              ELTAOL  
00685                                                                   ELTAOL  
00686 /***********************************************************      ELTAOL  
00687 *                                                          *      ELTAOL  
00688 *        TEST AOL OCCURS                                   *      ELTAOL  
00689 *                                                          *      ELTAOL  
00690 ************************************************************      ELTAOL  
00691                                                                   ELTAOL  
00692  0300-TEST-AOL-OCCURRENCE.                                        ELTAOL  
00693                                                                   ELTAOL  
00694      MOVE GAD-O-P-X-L-O-B (GAD-INDEX) TO WS-LOB-ACCUM-OCCRNC.     ELTAOL  
00695      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELTAOL  
00696      PERFORM 0320-SCAN-FOR-INTERNALS.                             ELTAOL  
00697      IF SW-OCCRNC-APPLIES                                         ELTAOL  
00698      THEN                                                         ELTAOL  
00699 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELTAOL  
00700         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELTAOL  
00701         PERFORM 0340-INIT-ACCUM-EXTRACT                           ELTAOL  
00702         PERFORM 0350-EXTRACT-ACCUM                                ELTAOL  
00703         PERFORM 0490-CHK-EXTRACT-DATA-INTGRTY                     ELTAOL  
00704         PERFORM 0360-ACCUM-VBL-PORTION                            ELTAOL  
00705         PERFORM 0710-WRITE-EXTRACT-RECORD                         ELTAOL  
00706      END-IF.                                                      ELTAOL  
00707      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-INDEX)            ELTAOL  
00708          TO TRUE.                                                 ELTAOL  
00709                                                                   ELTAOL  
00710                                                                   ELTAOL  
00711 ************************************************************      ELTAOL  
00712 *                                                          *      ELTAOL  
00713 *        INITIALIZE OCCURRENCE                             *      ELTAOL  
00714 *                                                          *      ELTAOL  
00715 ************************************************************      ELTAOL  
00716                                                                   ELTAOL  
00717  0310-INITIALIZE-OCCURRENCE.                                      ELTAOL  
00718      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELTAOL  
00719      SET SW-HAS-NO-IBGR                                           ELTAOL  
00720          SW-HAS-NO-IDGD                                           ELTAOL  
00721          SW-HAS-NO-IPGN                                           ELTAOL  
00722          SW-HAS-NO-IPGP                                           ELTAOL  
00723          SW-HAS-NO-IPGT                                           ELTAOL  
00724          SW-HAS-NO-IPGS                                           ELTAOL  
00725       TO TRUE.                                                    ELTAOL  
00726      INITIALIZE WS-IBGR-SLOT-NBR                                  ELTAOL  
00727                 WS-IDGD-SLOT-NBR                                  ELTAOL  
00728                 WS-IPGN-SLOT-NBR                                  ELTAOL  
00729                 WS-IPGP-SLOT-NBR                                  ELTAOL  
00730                 WS-IPGT-SLOT-NBR                                  ELTAOL  
00731                 WS-IPGS-SLOT-NBR.                                 ELTAOL  
00732      SET SW-INTRNL-INST-PROV-CL-NOT-DET                           ELTAOL  
00733          SW-INTRNL-PROF-PROV-CL-NOT-DET                           ELTAOL  
00734          SW-INTRNL-PROF-PROV-SP-NOT-DET                           ELTAOL  
00735       TO TRUE.                                                    ELTAOL  
00736                                                                   ELTAOL  
00737                                                                   ELTAOL  
00738 ************************************************************      ELTAOL  
00739 *                                                          *      ELTAOL  
00740 *        SCAN FOR INTERNAL TABULARS                        *      ELTAOL  
00741 *                                                          *      ELTAOL  
00742 ************************************************************      ELTAOL  
00743                                                                   ELTAOL  
00744  0320-SCAN-FOR-INTERNALS.                                         ELTAOL  
00745 *    (THIS IS DONE NOW IN CASE IPGT OR IBGR IS NEEDED TO DETERMINEELTAOL  
00746 *     WHETHER OCCURRENCE IS INSTITUTIONAL OR PROFESSIONAL.)       ELTAOL  
00747      PERFORM 0330-SCAN-THE-INTERNAL-TABULAR                       ELTAOL  
00748         VARYING GAD-INT-INDEX FROM 1 BY 1                         ELTAOL  
00749           UNTIL    GAD-INT-INDEX                                  ELTAOL  
00750                 >= GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX).        ELTAOL  
00751                                                                   ELTAOL  
00752      EVALUATE TRUE ALSO TRUE                                      ELTAOL  
00753         WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                        ELTAOL  
00754            SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE                  ELTAOL  
00755            SET SW-OCCRNC-APPLIES TO TRUE                          ELTAOL  
00756         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH                 ELTAOL  
00757            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELTAOL  
00758            PERFORM 0550-CHK-INTRNL-TAB-PROV-CL                    ELTAOL  
00759         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST                 ELTAOL  
00760            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELTAOL  
00761            SET SW-OCCRNC-APPLIES TO TRUE                          ELTAOL  
00762         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH                 ELTAOL  
00763            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELTAOL  
00764            PERFORM 0550-CHK-INTRNL-TAB-PROV-CL                    ELTAOL  
00765            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELTAOL  
00766            PERFORM 0551-CHK-INTRNL-TAB-PROV-SP                    ELTAOL  
00767         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF                 ELTAOL  
00768            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELTAOL  
00769            SET SW-OCCRNC-APPLIES TO TRUE                          ELTAOL  
00770         WHEN OTHER                                                ELTAOL  
00771            CONTINUE                                               ELTAOL  
00772         END-EVALUATE.                                             ELTAOL  
00773                                                                   ELTAOL  
00774 /***********************************************************      ELTAOL  
00775 *                                                          *      ELTAOL  
00776 *        SCAN THE INTERNAL TABULARS                        *      ELTAOL  
00777 *                                                          *      ELTAOL  
00778 ************************************************************      ELTAOL  
00779                                                                   ELTAOL  
00780  0330-SCAN-THE-INTERNAL-TABULAR.                                  ELTAOL  
00781      IF GAD-INT-SLOT (GAD-INDEX, GAD-INT-INDEX) > 0               ELTAOL  
00782      THEN                                                         ELTAOL  
00783         MOVE GAD-INT-SLOT (GAD-INDEX, GAD-INT-INDEX)              ELTAOL  
00784           TO WS-SLOT-NBR                                          ELTAOL  
00785         EVALUATE GAD-INT-ID (GAD-INDEX, GAD-INT-INDEX)            ELTAOL  
00786            WHEN PC-IBGR                                           ELTAOL  
00787               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELTAOL  
00788               SET SW-HAS-IBGR                                     ELTAOL  
00789                TO TRUE                                            ELTAOL  
00790            WHEN PC-IDGD                                           ELTAOL  
00791               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELTAOL  
00792               SET SW-HAS-IDGD                                     ELTAOL  
00793                TO TRUE                                            ELTAOL  
00794            WHEN PC-IPGP                                           ELTAOL  
00795               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELTAOL  
00796               SET SW-HAS-IPGP                                     ELTAOL  
00797                TO TRUE                                            ELTAOL  
00798            WHEN PC-IPGN                                           ELTAOL  
00799               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELTAOL  
00800               SET SW-HAS-IPGN                                     ELTAOL  
00801                TO TRUE                                            ELTAOL  
00802            WHEN PC-IPGT                                           ELTAOL  
00803               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELTAOL  
00804               SET SW-HAS-IPGT                                     ELTAOL  
00805                TO TRUE                                            ELTAOL  
00806            WHEN PC-IPGS                                           ELTAOL  
00807               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELTAOL  
00808               SET SW-HAS-IPGS                                     ELTAOL  
00809                TO TRUE                                            ELTAOL  
00810            WHEN OTHER                                             ELTAOL  
00811               CONTINUE                                            ELTAOL  
00812            END-EVALUATE                                           ELTAOL  
00813      END-IF.                                                      ELTAOL  
00814                                                                   ELTAOL  
00815 /***********************************************************      ELTAOL  
00816 *                                                          *      ELTAOL  
00817 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELTAOL  
00818 *                                                          *      ELTAOL  
00819 ************************************************************      ELTAOL  
00820                                                                   ELTAOL  
00821  0340-INIT-ACCUM-EXTRACT.                                         ELTAOL  
00822      INITIALIZE ACCUM-FIXED-AREA.                                 ELTAOL  
00823      SET ACCUM-AOL TO TRUE.                                       ELTAOL  
00824      MOVE +1 TO  ACCUM-ASCEND-DESCEND-COUNT.                      ELTAOL  
00825      SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELTAOL  
00826      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELTAOL  
00827      INITIALIZE ACCUM-COPAY-ENTRY (1).                            ELTAOL  
00828                                                                   ELTAOL  
00829 /***********************************************************      ELTAOL  
00830 *                                                          *      ELTAOL  
00831 *        SUMMARIZE AOL TOPIC LEVEL DATA ELEMENTS           *      ELTAOL  
00832 *                                                          *      ELTAOL  
00833 ************************************************************      ELTAOL  
00834                                                                   ELTAOL  
00835  0350-EXTRACT-ACCUM.                                              ELTAOL  
00836                                                                   ELTAOL  
00837 * -- SET FIXED PORTION DATA ELEMENTS                              ELTAOL  
00838      MOVE GAD-O-P-X-FYI-VALUE (GAD-INDEX) TO ACCUM-FYI-VALUE.     ELTAOL  
00839      MOVE GAD-O-P-X-COST-CONTAIN-IND (GAD-INDEX)                  ELTAOL  
00840        TO     ACCUM-COST-CONTAIN-IND.                             ELTAOL  
00841      MOVE GAD-O-P-X-PLACE-OF-TREATMENT (GAD-INDEX)                ELTAOL  
00842        TO     ACCUM-PLACE-OF-TREATMENT.                           ELTAOL  
00843      MOVE GAD-O-P-X-SERVICE-GROUP (GAD-INDEX)                     ELTAOL  
00844        TO     ACCUM-SERVICE-GROUP.                                ELTAOL  
00845      MOVE GAD-O-P-X-CO-PAY-IND (GAD-INDEX)                        ELTAOL  
00846        TO     ACCUM-CO-PAY-IND (1).                               ELTAOL  
00847      MOVE GAD-O-P-X-DAY-FACTOR-IND (GAD-INDEX)                    ELTAOL  
00848        TO     ACCUM-DAY-FACTOR-IND.                               ELTAOL  
00849      MOVE GAD-O-P-X-CLAIM-LVL-ACCUM-IND (GAD-INDEX)               ELTAOL  
00850        TO     ACCUM-CLAIM-LVL-ACCUM-IND.                          ELTAOL  
00851      MOVE GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX)               ELTAOL  
00852        TO     ACCUM-INTERNAL-DESCRIPTOR.                          ELTAOL  
00853      MOVE GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)                    ELTAOL  
00854        TO     ACCUM-BENEFIT-PERIOD.                               ELTAOL  
00855      MOVE GAD-O-P-X-BEN-PER-TIME-FCTR (GAD-INDEX)                 ELTAOL  
00856        TO     ACCUM-BEN-PER-TIME-FCTR.                            ELTAOL  
00857      MOVE GAD-O-P-X-BEN-PER-TIME-QUAL (GAD-INDEX)                 ELTAOL  
00858        TO     ACCUM-BEN-PER-TIME-QUAL.                            ELTAOL  
00859      MOVE GAD-O-P-X-INTERVAL-TIME-FCTR (GAD-INDEX)                ELTAOL  
00860        TO     ACCUM-INTERVAL-TIME-FCTR.                           ELTAOL  
00861      MOVE GAD-O-P-X-INTERVAL-TYPE (GAD-INDEX)                     ELTAOL  
00862        TO     ACCUM-INTERVAL-TYPE.                                ELTAOL  
00863      MOVE GAD-O-P-X-INTERVAL-OVRD-IND (GAD-INDEX)                 ELTAOL  
00864        TO     ACCUM-INTERVAL-OVRD-IND.                            ELTAOL  
00865      MOVE GAD-O-P-X-INTERVAL-OVRD-VALUE (GAD-INDEX)               ELTAOL  
00866        TO     ACCUM-INTERVAL-OVRD-VALUE.                          ELTAOL  
00867      MOVE GAD-O-P-X-L-O-B (GAD-INDEX) TO ACCUM-L-O-B.             ELTAOL  
00868      MOVE GAD-O-P-X-DEFINITION (GAD-INDEX) TO ACCUM-DEFINITION.   ELTAOL  
00869      SET REINSTATEMENT-IND-NA TO TRUE.                            ELTAOL  
00870      MOVE   GAD-CARRY-OVER-CREDIT-IND (GAD-INDEX)                 ELTAOL  
00871      TO   ACCUM-CARRY-OVER-CREDIT-IND.                            ELTAOL  
00872      MOVE GAD-O-P-X-ASCEND-DESCEND-IND (GAD-INDEX)                ELTAOL  
00873        TO     ACCUM-ASCEND-DESCEND-IND.                           ELTAOL  
00874      MOVE GAD-O-P-X-CONDITION (GAD-INDEX) TO ACCUM-CONDITION.     ELTAOL  
00875      MOVE GAD-O-P-X-FAM-OR-INDIV (GAD-INDEX)                      ELTAOL  
00876        TO     ACCUM-FAM-OR-INDIV.                                 ELTAOL  
00877      SET DED-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELTAOL  
00878      SET MAX-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELTAOL  
00879      MOVE GCG-OUTPKT-BASE-AMT-SOURCE-IN                           ELTAOL  
00880        TO ACCUM-OPX-BASE-AMT-SOURCE-IND.                          ELTAOL  
00881      MOVE GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)                   ELTAOL  
00882        TO     ACCUM-VALUE-QUALIFIER.                              ELTAOL  
00883      MOVE GAD-O-P-X-RELATIONSHIP-IND (GAD-INDEX)                  ELTAOL  
00884        TO     ACCUM-RELATIONSHIP-IND.                             ELTAOL  
00885      MOVE GAD-O-P-X-AGE-LIMIT-FROM (GAD-INDEX)                    ELTAOL  
00886        TO     ACCUM-AGE-LIMIT-FROM-VAL.                           ELTAOL  
00887      MOVE GAD-O-P-X-AGE-LIMIT-TO (GAD-INDEX)                      ELTAOL  
00888        TO     ACCUM-AGE-LIMIT-TO-VAL.                             ELTAOL  
00889      MOVE GAD-O-P-X-AGE-QUAL-IND-FROM (GAD-INDEX)                 ELTAOL  
00890        TO     ACCUM-AGE-LIMIT-FROM-IND.                           ELTAOL  
00891      MOVE GAD-O-P-X-AGE-QUAL-IND-TO (GAD-INDEX)                   ELTAOL  
00892        TO     ACCUM-AGE-LIMIT-TO-IND.                             ELTAOL  
00893                                                                   ELTAOL  
00894 * -- SET OCCURRENCE PROVIDER CLASS INFORMATION                    ELTAOL  
00895      EVALUATE TRUE ALSO TRUE                                      ELTAOL  
00896         WHEN      SW-INTRNL-INST-PROV-CL                          ELTAOL  
00897              ALSO SW-INTRNL-NOT-PROF-PROV-CL                      ELTAOL  
00898            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELTAOL  
00899         WHEN      SW-INTRNL-NOT-INST-PROV-CL                      ELTAOL  
00900              ALSO SW-INTRNL-PROF-PROV-CL                          ELTAOL  
00901            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELTAOL  
00902         WHEN OTHER                                                ELTAOL  
00903            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELTAOL  
00904         END-EVALUATE.                                             ELTAOL  
00905                                                                   ELTAOL  
00906 * -- SET OCCURRENCE PROVIDER SPEC INFORMATION                     ELTAOL  
00907      IF SW-INTRNL-PROF-PROV-CL                                    ELTAOL  
00908         SET ACCUM-PRVDR-CLS-PROF TO TRUE                          ELTAOL  
00909      ELSE                                                         ELTAOL  
00910         SET ACCUM-PRVDR-CLS-ALL TO TRUE                           ELTAOL  
00911      END-IF.                                                      ELTAOL  
00912                                                                   ELTAOL  
00913 /***********************************************************      ELTAOL  
00914 *                                                          *      ELTAOL  
00915 *        ACCUM VBL PORTION                                 *      ELTAOL  
00916 *                                                          *      ELTAOL  
00917 ************************************************************      ELTAOL  
00918                                                                   ELTAOL  
00919  0360-ACCUM-VBL-PORTION.                                          ELTAOL  
00920      SET ASC-DES-INDEX TO 1.                                      ELTAOL  
00921      PERFORM 0370-EXTRACT-VARIABLE-PORTION.                       ELTAOL  
00922      IF ACCUM-VARIABLE-TYPE                                       ELTAOL  
00923         THEN                                                      ELTAOL  
00924             PERFORM 0380-EXTRACT-ADDL-OCCURNCS                    ELTAOL  
00925       END-IF.                                                     ELTAOL  
00926                                                                   ELTAOL  
00927 /***********************************************************      ELTAOL  
00928 *                                                          *      ELTAOL  
00929 *        EXTRACT VARIABLE PORTION                          *      ELTAOL  
00930 *                                                          *      ELTAOL  
00931 ************************************************************      ELTAOL  
00932                                                                   ELTAOL  
00933  0370-EXTRACT-VARIABLE-PORTION.                                   ELTAOL  
00934      MOVE GAD-O-P-X-BISCENDING-IND (GAD-INDEX)                    ELTAOL  
00935        TO ACCUM-BISCEND-IND (ASC-DES-INDEX).                      ELTAOL  
00936      MOVE GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX)                     ELTAOL  
00937        TO ACCUM-PERCENT-LEVEL (ASC-DES-INDEX).                    ELTAOL  
00938      MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)                       ELTAOL  
00939        TO ACCUM-VALUE-LIMIT (ASC-DES-INDEX).                      ELTAOL  
00940      MOVE WS-IBGR-SLOT-NBR                                        ELTAOL  
00941        TO ACCUM-IBGR-SLOT-NBR (ASC-DES-INDEX).                    ELTAOL  
00942      MOVE WS-IDGD-SLOT-NBR                                        ELTAOL  
00943        TO ACCUM-IDGD-SLOT-NBR (ASC-DES-INDEX).                    ELTAOL  
00944      MOVE WS-IPGN-SLOT-NBR                                        ELTAOL  
00945        TO ACCUM-IPGN-SLOT-NBR (ASC-DES-INDEX).                    ELTAOL  
00946      MOVE WS-IPGP-SLOT-NBR                                        ELTAOL  
00947        TO ACCUM-IPGP-SLOT-NBR (ASC-DES-INDEX).                    ELTAOL  
00948      MOVE WS-IPGT-SLOT-NBR                                        ELTAOL  
00949        TO ACCUM-IPGT-SLOT-NBR (ASC-DES-INDEX).                    ELTAOL  
00950      MOVE WS-IPGS-SLOT-NBR                                        ELTAOL  
00951        TO ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX).                    ELTAOL  
00952      IF ACCUM-BISCEND-IND (ASC-DES-INDEX)                         ELTAOL  
00953         = ZERO OR SPACES OR LOW-VALUES                            ELTAOL  
00954         SET BISCEND-IND-NA (ASC-DES-INDEX) TO TRUE.               ELTAOL  
00955      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB) TO TRUE.     ELTAOL  
00956                                                                   ELTAOL  
00957 /***********************************************************      ELTAOL  
00958 *                                                          *      ELTAOL  
00959 *        EXTRACT ADDITIONAL OCCURRENCES                    *      ELTAOL  
00960 *                                                          *      ELTAOL  
00961 ************************************************************      ELTAOL  
00962                                                                   ELTAOL  
00963  0380-EXTRACT-ADDL-OCCURNCS.                                      ELTAOL  
00964       MOVE WS-OCCURRENCE-SUB TO WS-SAVE-SUB                       ELTAOL  
00965       SET  WS-SAVE-INDEX    TO GAD-INDEX.                         ELTAOL  
00966       ADD 1 TO WS-OCCURRENCE-SUB.                                 ELTAOL  
00967       PERFORM 0390-TEST-SUBSEQ-OCCRNCES                           ELTAOL  
00968          VARYING WS-OCCURRENCE-SUB                                ELTAOL  
00969             FROM WS-OCCURRENCE-SUB BY 1                           ELTAOL  
00970          UNTIL WS-OCCURRENCE-INDEX >= GAD-ENTRY-COUNT.            ELTAOL  
00971       MOVE WS-SAVE-SUB TO WS-OCCURRENCE-SUB.                      ELTAOL  
00972       SET  GAD-INDEX   TO WS-SAVE-INDEX.                          ELTAOL  
00973                                                                   ELTAOL  
00974 /***********************************************************      ELTAOL  
00975 *                                                          *      ELTAOL  
00976 *        TEST SUBSEQUENT OCCURRENCES                       *      ELTAOL  
00977 *                                                          *      ELTAOL  
00978 ************************************************************      ELTAOL  
00979                                                                   ELTAOL  
00980  0390-TEST-SUBSEQ-OCCRNCES.                                       ELTAOL  
00981       SET GAD-INDEX                                               ELTAOL  
00982        TO WS-OCCURRENCE-SUB.                                      ELTAOL  
00983       SET WS-OCCURRENCE-INDEX                                     ELTAOL  
00984        TO WS-OCCURRENCE-SUB.                                      ELTAOL  
00985       IF WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB)              ELTAOL  
00986          CONTINUE                                                 ELTAOL  
00987       ELSE PERFORM 0400-TEST-OCCURRENCE.                          ELTAOL  
00988                                                                   ELTAOL  
00989 /***********************************************************      ELTAOL  
00990 *                                                          *      ELTAOL  
00991 *        TEST OCCURRENCE                                   *      ELTAOL  
00992 *                                                          *      ELTAOL  
00993 ************************************************************      ELTAOL  
00994                                                                   ELTAOL  
00995  0400-TEST-OCCURRENCE.                                            ELTAOL  
00996       SET SW-MATCHING-ENTRY-NOT-FOUND TO TRUE.                    ELTAOL  
00997       PERFORM 0410-TEST-KEYS-FOR-MATCH.                           ELTAOL  
00998       IF SW-MATCHING-ENTRY-FOUND                                  ELTAOL  
00999          PERFORM 0420-COMPLETE-TEST-OF-OCCURNCE.                  ELTAOL  
01000                                                                   ELTAOL  
01001 /***********************************************************      ELTAOL  
01002 *                                                          *      ELTAOL  
01003 *        TEST KEYS FOR MATCH                               *      ELTAOL  
01004 *                                                          *      ELTAOL  
01005 ************************************************************      ELTAOL  
01006                                                                   ELTAOL  
01007  0410-TEST-KEYS-FOR-MATCH.                                        ELTAOL  
01008      IF GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX) =                    ELTAOL  
01009              ACCUM-BENEFIT-PERIOD                                 ELTAOL  
01010                        AND                                        ELTAOL  
01011              GAD-O-P-X-FAM-OR-INDIV (GAD-INDEX) =                 ELTAOL  
01012              ACCUM-FAM-OR-INDIV                                   ELTAOL  
01013                        AND                                        ELTAOL  
01014              GAD-O-P-X-L-O-B (GAD-INDEX) =                        ELTAOL  
01015              ACCUM-L-O-B                                          ELTAOL  
01016                        AND                                        ELTAOL  
01017              GAD-O-P-X-FYI-VALUE (GAD-INDEX) =                    ELTAOL  
01018              ACCUM-FYI-VALUE                                      ELTAOL  
01019                        AND                                        ELTAOL  
01020              GAD-O-P-X-SERVICE-GROUP (GAD-INDEX) =                ELTAOL  
01021              ACCUM-SERVICE-GROUP                                  ELTAOL  
01022                        AND                                        ELTAOL  
01023              GAD-O-P-X-PLACE-OF-TREATMENT (GAD-INDEX) =           ELTAOL  
01024              ACCUM-PLACE-OF-TREATMENT                             ELTAOL  
01025                        AND                                        ELTAOL  
01026              GAD-COND-ALL-BIT (GAD-INDEX) =                       ELTAOL  
01027              ACCUM-COND-ALL-BIT                                   ELTAOL  
01028                        AND                                        ELTAOL  
01029              GAD-COND-EXCLUSION-BIT (GAD-INDEX) =                 ELTAOL  
01030              ACCUM-COND-EXCLUSION-BIT                             ELTAOL  
01031                        AND                                        ELTAOL  
01032              GAD-COND-ICD-BIT (GAD-INDEX) =                       ELTAOL  
01033              ACCUM-COND-ICD-BIT                                   ELTAOL  
01034                        AND                                        ELTAOL  
01035              GAD-COND-TB-BIT (GAD-INDEX) =                        ELTAOL  
01036              ACCUM-COND-TB-BIT                                    ELTAOL  
01037                        AND                                        ELTAOL  
01038              GAD-COND-MENTAL-BIT (GAD-INDEX) =                    ELTAOL  
01039              ACCUM-COND-MENTAL-BIT                                ELTAOL  
01040                        AND                                        ELTAOL  
01041              GAD-COND-DRUG-BIT (GAD-INDEX) =                      ELTAOL  
01042              ACCUM-COND-DRUG-BIT                                  ELTAOL  
01043                        AND                                        ELTAOL  
01044              GAD-COND-ALCOHOL-BIT (GAD-INDEX) =                   ELTAOL  
01045              ACCUM-COND-ALCOHOL-BIT                               ELTAOL  
01046                        AND                                        ELTAOL  
01047              GAD-COND-OB-COMP-BIT (GAD-INDEX) =                   ELTAOL  
01048              ACCUM-COND-OB-COMP-BIT                               ELTAOL  
01049                        AND                                        ELTAOL  
01050              GAD-COND-OB-NORM-BIT (GAD-INDEX) =                   ELTAOL  
01051              ACCUM-COND-OB-NORM-BIT                               ELTAOL  
01052                        AND                                        ELTAOL  
01053              GAD-COND-MALIGNANCY-BIT (GAD-INDEX) =                ELTAOL  
01054              ACCUM-COND-MALIGNANCY-BIT                            ELTAOL  
01055                        AND                                        ELTAOL  
01056              GAD-COND-CARDIAC-DISEASE-BIT (GAD-INDEX) =           ELTAOL  
01057              ACCUM-COND-CARDIAC-DISEASE-BIT                       ELTAOL  
01058                        AND                                        ELTAOL  
01059              GAD-COND-OBESITY-BIT (GAD-INDEX) =                   ELTAOL  
01060              ACCUM-COND-OBESITY-BIT                               ELTAOL  
01061                        AND                                        ELTAOL  
01062              GAD-COND-KIDNEY-DISEASE-BIT (GAD-INDEX) =            ELTAOL  
01063              ACCUM-COND-KIDNEY-DISEASE-BIT                        ELTAOL  
01064                        AND                                        ELTAOL  
01065              GAD-COND-ACCIDENT-BIT (GAD-INDEX) =                  ELTAOL  
01066              ACCUM-COND-ACCIDENT-BIT                              ELTAOL  
01067                        AND                                        ELTAOL  
01068              GAD-COND-PRE-EXIST-BIT (GAD-INDEX) =                 ELTAOL  
01069              ACCUM-COND-PRE-EXIST-BIT                             ELTAOL  
01070                        AND                                        ELTAOL  
01071              GAD-COND-NON-EMER-BIT (GAD-INDEX) =                  ELTAOL  
01072              ACCUM-COND-NON-EMER-BIT                              ELTAOL  
01073                        AND                                        ELTAOL  
01074              GAD-COND-SUICIDE-BIT (GAD-INDEX) =                   ELTAOL  
01075              ACCUM-COND-SUICIDE-BIT                               ELTAOL  
01076                        AND                                        ELTAOL  
01077              GAD-COND-TMJ-BIT (GAD-INDEX) =                       ELTAOL  
01078              ACCUM-COND-TMJ-BIT                                   ELTAOL  
01079                        AND                                        ELTAOL  
01080              GAD-COND-INF-BIT (GAD-INDEX) =                       ELTAOL  
01081              ACCUM-COND-INF-BIT                                   ELTAOL  
01082                        AND                                        ELTAOL  
01083              GAD-COND-LIFE-THREAT-BIT (GAD-INDEX) =               ELTAOL  
01084              ACCUM-COND-LIFE-THREAT-BIT                           ELTAOL  
01085                        AND                                        ELTAOL  
01086              GAD-O-P-X-CO-PAY-IND (GAD-INDEX) =                   ELTAOL  
01087              ACCUM-CO-PAY-IND (1)                                 ELTAOL  
01088                        AND                                        ELTAOL  
01089              GAD-O-P-X-COST-CONTAIN-IND (GAD-INDEX) =             ELTAOL  
01090              ACCUM-COST-CONTAIN-IND                               ELTAOL  
01091                        AND                                        ELTAOL  
01092              GAD-O-P-X-ASCEND-DESCEND-IND (GAD-INDEX) =           ELTAOL  
01093              ACCUM-ASCEND-DESCEND-IND                             ELTAOL  
01094                        AND                                        ELTAOL  
01095              GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX) =          ELTAOL  
01096              ACCUM-INTERNAL-DESCRIPTOR                            ELTAOL  
01097                        AND                                        ELTAOL  
01098              GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) =              ELTAOL  
01099              ACCUM-VALUE-QUALIFIER                                ELTAOL  
01100          THEN                                                     ELTAOL  
01101          SET SW-MATCHING-ENTRY-FOUND TO TRUE.                     ELTAOL  
01102                                                                   ELTAOL  
01103                                                                   ELTAOL  
01104 /***********************************************************      ELTAOL  
01105 *                                                          *      ELTAOL  
01106 *        COMPLETE TEST OF OCCURRENCE                       *      ELTAOL  
01107 *                                                          *      ELTAOL  
01108 ************************************************************      ELTAOL  
01109                                                                   ELTAOL  
01110  0420-COMPLETE-TEST-OF-OCCURNCE.                                  ELTAOL  
01111      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELTAOL  
01112      PERFORM 0320-SCAN-FOR-INTERNALS.                             ELTAOL  
01113      IF SW-OCCRNC-APPLIES                                         ELTAOL  
01114         PERFORM 0430-EXTRACT-NEXT-OCCURRENCE                      ELTAOL  
01115      ELSE SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB) TO TRUE.ELTAOL  
01116                                                                   ELTAOL  
01117 /***********************************************************      ELTAOL  
01118 *                                                          *      ELTAOL  
01119 *        EXTRACT NEXT OCCURRENCE                           *      ELTAOL  
01120 *                                                          *      ELTAOL  
01121 ************************************************************      ELTAOL  
01122                                                                   ELTAOL  
01123  0430-EXTRACT-NEXT-OCCURRENCE.                                    ELTAOL  
01124      ADD +1 TO ACCUM-ASCEND-DESCEND-COUNT.                        ELTAOL  
01125      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELTAOL  
01126      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (ASC-DES-INDEX).       ELTAOL  
01127      PERFORM 0370-EXTRACT-VARIABLE-PORTION.                       ELTAOL  
01128      PERFORM 0440-INSERT-NEW-ENTRY.                               ELTAOL  
01129                                                                   ELTAOL  
01130 /***********************************************************      ELTAOL  
01131 *                                                          *      ELTAOL  
01132 *        INSERT NEW ENTRY                                  *      ELTAOL  
01133 *                                                          *      ELTAOL  
01134 ************************************************************      ELTAOL  
01135                                                                   ELTAOL  
01136  0440-INSERT-NEW-ENTRY.                                           ELTAOL  
01137      MOVE ACCUM-ASCEND-DESCEND-COUNT TO SORT-SUB.                 ELTAOL  
01138      SET SW-SORT-NOT-COMPLETED TO TRUE.                           ELTAOL  
01139      IF SORT-SUB = 1                                              ELTAOL  
01140         CONTINUE                                                  ELTAOL  
01141      ELSE IF ACCUM-ASCEND-ORDER                                   ELTAOL  
01142              PERFORM 0450-ASCEND-INSERT                           ELTAOL  
01143                UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1            ELTAOL  
01144           ELSE IF ACCUM-DESCEND-ORDER                             ELTAOL  
01145                   PERFORM 0460-DESCEND-INSERT                     ELTAOL  
01146                     UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1       ELTAOL  
01147                ELSE IF ACCUM-BISCEND-ORDER                        ELTAOL  
01148                        PERFORM 0470-BISCEND-INSERT                ELTAOL  
01149                          UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1. ELTAOL  
01150                                                                   ELTAOL  
01151                                                                   ELTAOL  
01152 /***********************************************************      ELTAOL  
01153 *                                                          *      ELTAOL  
01154 *        ASCEND INSERT                                     *      ELTAOL  
01155 *                                                          *      ELTAOL  
01156 ************************************************************      ELTAOL  
01157                                                                   ELTAOL  
01158  0450-ASCEND-INSERT.                                              ELTAOL  
01159      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELTAOL  
01160      IF ACCUM-PERCENT-LEVEL (SORT-SUB) <                          ELTAOL  
01161         ACCUM-PERCENT-LEVEL (TEST-SUB)                            ELTAOL  
01162         PERFORM 0480-SWAP-ENTRIES                                 ELTAOL  
01163      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELTAOL  
01164                                                                   ELTAOL  
01165 /***********************************************************      ELTAOL  
01166 *                                                          *      ELTAOL  
01167 *        DESCEND INSERT                                    *      ELTAOL  
01168 *                                                          *      ELTAOL  
01169 ************************************************************      ELTAOL  
01170                                                                   ELTAOL  
01171  0460-DESCEND-INSERT.                                             ELTAOL  
01172      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELTAOL  
01173      IF ACCUM-PERCENT-LEVEL (SORT-SUB) >                          ELTAOL  
01174         ACCUM-PERCENT-LEVEL (TEST-SUB)                            ELTAOL  
01175         PERFORM 0480-SWAP-ENTRIES                                 ELTAOL  
01176      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELTAOL  
01177                                                                   ELTAOL  
01178 /***********************************************************      ELTAOL  
01179 *                                                          *      ELTAOL  
01180 *        BISCEND INSERT                                    *      ELTAOL  
01181 *                                                          *      ELTAOL  
01182 ************************************************************      ELTAOL  
01183                                                                   ELTAOL  
01184  0470-BISCEND-INSERT.                                             ELTAOL  
01185      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELTAOL  
01186      IF ACCUM-BISCEND-IND (SORT-SUB) <                            ELTAOL  
01187         ACCUM-BISCEND-IND (TEST-SUB)                              ELTAOL  
01188         PERFORM 0480-SWAP-ENTRIES                                 ELTAOL  
01189      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELTAOL  
01190                                                                   ELTAOL  
01191 /***********************************************************      ELTAOL  
01192 *                                                          *      ELTAOL  
01193 *        SWAP ENTRIES                                      *      ELTAOL  
01194 *                                                          *      ELTAOL  
01195 ************************************************************      ELTAOL  
01196                                                                   ELTAOL  
01197  0480-SWAP-ENTRIES.                                               ELTAOL  
01198      MOVE ACCUM-ASCEND-DESCEND-ENTRY (SORT-SUB)                   ELTAOL  
01199        TO WS-ASCEND-DESCEND-ENTRY-HOLD.                           ELTAOL  
01200      MOVE ACCUM-ASCEND-DESCEND-ENTRY (TEST-SUB)                   ELTAOL  
01201        TO ACCUM-ASCEND-DESCEND-ENTRY (SORT-SUB).                  ELTAOL  
01202      MOVE WS-ASCEND-DESCEND-ENTRY-HOLD                            ELTAOL  
01203        TO ACCUM-ASCEND-DESCEND-ENTRY (TEST-SUB).                  ELTAOL  
01204      MOVE TEST-SUB TO SORT-SUB.                                   ELTAOL  
01205                                                                   ELTAOL  
01206                                                                   ELTAOL  
01207 /***********************************************************      ELTAOL  
01208 *                                                          *      ELTAOL  
01209 *    CHECK EXTRACT DATA INTEGRITY                          *      ELTAOL  
01210 *                                                          *      ELTAOL  
01211 ************************************************************      ELTAOL  
01212                                                                   ELTAOL  
01213  0490-CHK-EXTRACT-DATA-INTGRTY.                                   ELTAOL  
01214      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELTAOL  
01215      THEN                                                         ELTAOL  
01216         SET FYI-VALUE-NA TO TRUE                                  ELTAOL  
01217      END-IF.                                                      ELTAOL  
01218                                                                   ELTAOL  
01219      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELTAOL  
01220      THEN                                                         ELTAOL  
01221         SET COST-CONTAIN-IND-NA TO TRUE                           ELTAOL  
01222      END-IF.                                                      ELTAOL  
01223                                                                   ELTAOL  
01224      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELTAOL  
01225      THEN                                                         ELTAOL  
01226         SET PLACE-OF-TREATMENT-NA TO TRUE                         ELTAOL  
01227      END-IF.                                                      ELTAOL  
01228                                                                   ELTAOL  
01229      IF ACCUM-BENEFIT-PERIOD = ZEROS OR SPACES OR LOW-VALUES      ELTAOL  
01230      THEN                                                         ELTAOL  
01231         SET BENEFIT-PERIOD-NA TO TRUE                             ELTAOL  
01232      END-IF.                                                      ELTAOL  
01233                                                                   ELTAOL  
01234      IF ACCUM-BEN-PER-TIME-QUAL = ZEROS OR SPACES OR LOW-VALUES   ELTAOL  
01235      THEN                                                         ELTAOL  
01236         SET BEN-PER-TIME-QUAL-NA TO TRUE                          ELTAOL  
01237      END-IF.                                                      ELTAOL  
01238                                                                   ELTAOL  
01239      IF ACCUM-INTERVAL-TYPE = ZEROS OR SPACES OR LOW-VALUES       ELTAOL  
01240      THEN                                                         ELTAOL  
01241         SET INTERVAL-TYPE-NA TO TRUE                              ELTAOL  
01242      END-IF.                                                      ELTAOL  
01243                                                                   ELTAOL  
01244      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELTAOL  
01245      THEN                                                         ELTAOL  
01246         SET INTERVAL-OVRD-IND-NA TO TRUE                          ELTAOL  
01247      END-IF.                                                      ELTAOL  
01248                                                                   ELTAOL  
01249      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELTAOL  
01250      THEN                                                         ELTAOL  
01251         SET L-O-B-NA TO TRUE                                      ELTAOL  
01252      END-IF.                                                      ELTAOL  
01253                                                                   ELTAOL  
01254      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELTAOL  
01255      THEN                                                         ELTAOL  
01256         SET REINSTATEMENT-IND-NA TO TRUE                          ELTAOL  
01257      END-IF.                                                      ELTAOL  
01258                                                                   ELTAOL  
01259      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELTAOL  
01260      THEN                                                         ELTAOL  
01261         SET DEFINITION-NA TO TRUE                                 ELTAOL  
01262      END-IF.                                                      ELTAOL  
01263                                                                   ELTAOL  
01264      IF   ACCUM-CARRY-OVER-CREDIT-IND                             ELTAOL  
01265         = ZEROS OR SPACES OR LOW-VALUES                           ELTAOL  
01266      THEN                                                         ELTAOL  
01267         SET CARRY-OVER-CREDIT-IND-NA TO TRUE                      ELTAOL  
01268      END-IF.                                                      ELTAOL  
01269                                                                   ELTAOL  
01270      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELTAOL  
01271      THEN                                                         ELTAOL  
01272         SET ASCEND-DESCEND-IND-NA TO TRUE                         ELTAOL  
01273      END-IF.                                                      ELTAOL  
01274                                                                   ELTAOL  
01275      IF ACCUM-FAM-OR-INDIV = ZEROS OR SPACES OR LOW-VALUES        ELTAOL  
01276      THEN                                                         ELTAOL  
01277         SET FAM-OR-INDIV-NA TO TRUE                               ELTAOL  
01278      END-IF.                                                      ELTAOL  
01279                                                                   ELTAOL  
01280      IF ACCUM-OPX-BASE-AMT-SOURCE-IND =                           ELTAOL  
01281          ZEROS OR SPACES OR LOW-VALUES                            ELTAOL  
01282      THEN                                                         ELTAOL  
01283         SET OPX-BASE-AMT-SOURCE-IND-NA TO TRUE                    ELTAOL  
01284      END-IF.                                                      ELTAOL  
01285                                                                   ELTAOL  
01286      IF ACCUM-VALUE-QUALIFIER = ZEROS OR SPACES OR LOW-VALUES     ELTAOL  
01287      THEN                                                         ELTAOL  
01288         SET VALUE-QUALIFIER-NA TO TRUE                            ELTAOL  
01289      END-IF.                                                      ELTAOL  
01290                                                                   ELTAOL  
01291      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELTAOL  
01292      THEN                                                         ELTAOL  
01293         SET RELATIONSHIP-IND-NA TO TRUE                           ELTAOL  
01294      END-IF.                                                      ELTAOL  
01295                                                                   ELTAOL  
01296      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELTAOL  
01297      THEN                                                         ELTAOL  
01298         SET AGE-LMT-TO-IND-NA TO TRUE                             ELTAOL  
01299      END-IF.                                                      ELTAOL  
01300                                                                   ELTAOL  
01301      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELTAOL  
01302      THEN                                                         ELTAOL  
01303         SET AGE-LMT-FROM-IND-NA TO TRUE                           ELTAOL  
01304      END-IF.                                                      ELTAOL  
01305                                                                   ELTAOL  
01306      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELTAOL  
01307      THEN                                                         ELTAOL  
01308         SET LMT-MANDATORY-IND-NA TO TRUE                          ELTAOL  
01309      END-IF.                                                      ELTAOL  
01310                                                                   ELTAOL  
01311      IF ACCUM-CO-PAY-IND (1) = ZEROS OR SPACES OR LOW-VALUES      ELTAOL  
01312      THEN                                                         ELTAOL  
01313         SET CO-PAY-IND-NA (1) TO TRUE                             ELTAOL  
01314      END-IF.                                                      ELTAOL  
01315                                                                   ELTAOL  
01316      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELTAOL  
01317      THEN                                                         ELTAOL  
01318         SET SERVICE-GROUP-NA TO TRUE                              ELTAOL  
01319      END-IF.                                                      ELTAOL  
01320                                                                   ELTAOL  
01321      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELTAOL  
01322      THEN                                                         ELTAOL  
01323         SET INTERNAL-DESCRIPTOR-NA TO TRUE                        ELTAOL  
01324      END-IF.                                                      ELTAOL  
01325                                                                   ELTAOL  
01326      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELTAOL  
01327      THEN                                                         ELTAOL  
01328         SET DAY-FACTOR-IND-NA TO TRUE                             ELTAOL  
01329      END-IF.                                                      ELTAOL  
01330                                                                   ELTAOL  
01331      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELTAOL  
01332      THEN                                                         ELTAOL  
01333         SET CLAIM-LVL-ACCUM-IND-NA TO TRUE                        ELTAOL  
01334      END-IF.                                                      ELTAOL  
01335                                                                   ELTAOL  
01336      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELTAOL  
01337      THEN                                                         ELTAOL  
01338         SET BEN-PER-MAX-OVRD-IND-NA TO TRUE                       ELTAOL  
01339      END-IF.                                                      ELTAOL  
01340                                                                   ELTAOL  
01341      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELTAOL  
01342      THEN                                                         ELTAOL  
01343         SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE                        ELTAOL  
01344      END-IF.                                                      ELTAOL  
01345                                                                   ELTAOL  
01346 /***********************************************************      ELTAOL  
01347 *                                                          *      ELTAOL  
01348 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER CLASS   *      ELTAOL  
01349 *                                                          *      ELTAOL  
01350 ************************************************************      ELTAOL  
01351                                                                   ELTAOL  
01352  0550-CHK-INTRNL-TAB-PROV-CL.                                     ELTAOL  
01353      IF SW-HAS-IPGT                                               ELTAOL  
01354      THEN                                                         ELTAOL  
01355         PERFORM 0560-CHK-IPGT-PROV-CL                             ELTAOL  
01356      ELSE                                                         ELTAOL  
01357         IF SW-HAS-IBGR                                            ELTAOL  
01358         THEN                                                      ELTAOL  
01359            PERFORM 0640-CHK-IBGR-PROV-CL                          ELTAOL  
01360         ELSE                                                      ELTAOL  
01361            SET SW-OCCRNC-APPLIES TO TRUE                          ELTAOL  
01362         END-IF                                                    ELTAOL  
01363      END-IF.                                                      ELTAOL  
01364                                                                   ELTAOL  
01365 /***********************************************************      ELTAOL  
01366 *                                                          *      ELTAOL  
01367 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER SPEC    *      ELTAOL  
01368 *                                                          *      ELTAOL  
01369 ************************************************************      ELTAOL  
01370                                                                   ELTAOL  
01371  0551-CHK-INTRNL-TAB-PROV-SP.                                     ELTAOL  
01372      IF SW-HAS-IPGS                                               ELTAOL  
01373         PERFORM 0561-CHK-IPGS-PROV-SP                             ELTAOL  
01374      END-IF.                                                      ELTAOL  
01375                                                                   ELTAOL  
01376 /*****************************************************************ELTAOL  
01377 *                                                                *ELTAOL  
01378 *    CHECK IPGT INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELTAOL  
01379 *                                                                *ELTAOL  
01380 ******************************************************************ELTAOL  
01381                                                                   ELTAOL  
01382  0560-CHK-IPGT-PROV-CL.                                           ELTAOL  
01383      MOVE PC-IPGT TO KWA-PROVISION-ID.                            ELTAOL  
01384      MOVE WS-IPGT-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTAOL  
01385      PERFORM 0660-READ-INTRLN-TAB.                                ELTAOL  
01386      SET ADDRESS OF GX3-RECORD-AREA TO IOP-REC-PTR.               ELTAOL  
01387      SET GX3-INDEX                  TO GX3-ENTRY-COUNT.           ELTAOL  
01388      SET WS-MAX-GX3-INDEX           TO GX3-INDEX.                 ELTAOL  
01389                                                                   ELTAOL  
01390      IF GX3-ID-ARGUMENT-INCLUDED                                  ELTAOL  
01391      THEN                                                         ELTAOL  
01392         PERFORM 0570-CHK-INCLD-TYPE-IPGT                          ELTAOL  
01393      ELSE                                                         ELTAOL  
01394          PERFORM 0600-CHK-EXCLD-TYPE-IPGT                         ELTAOL  
01395      END-IF.                                                      ELTAOL  
01396                                                                   ELTAOL  
01397 /*****************************************************************ELTAOL  
01398 *                                                                *ELTAOL  
01399 *    CHECK IPGS INTERNAL TABULAR TO DETERMINE PROVIDER SPEC      *ELTAOL  
01400 *                                                                *ELTAOL  
01401 ******************************************************************ELTAOL  
01402                                                                   ELTAOL  
01403  0561-CHK-IPGS-PROV-SP.                                           ELTAOL  
01404      MOVE PC-IPGS TO KWA-PROVISION-ID.                            ELTAOL  
01405      MOVE WS-IPGS-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTAOL  
01406      PERFORM 0660-READ-INTRLN-TAB.                                ELTAOL  
01407      SET ADDRESS OF GXS-RECORD-AREA TO IOP-REC-PTR.               ELTAOL  
01408      SET GXS-INDEX                  TO GXS-ENTRY-COUNT.           ELTAOL  
01409      SET WS-MAX-GXS-INDEX           TO GXS-INDEX.                 ELTAOL  
01410                                                                   ELTAOL  
01411      IF GXS-ID-ARGUMENT-INCLUDED                                  ELTAOL  
01412         PERFORM 0571-CHK-INCLD-TYPE-IPGS                          ELTAOL  
01413      ELSE                                                         ELTAOL  
01414          PERFORM 0601-CHK-EXCLD-TYPE-IPGS                         ELTAOL  
01415      END-IF.                                                      ELTAOL  
01416                                                                   ELTAOL  
01417 ************************************************************      ELTAOL  
01418 *                                                          *      ELTAOL  
01419 *    CHECK INCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELTAOL  
01420 *                                                          *      ELTAOL  
01421 ************************************************************      ELTAOL  
01422                                                                   ELTAOL  
01423  0570-CHK-INCLD-TYPE-IPGT.                                        ELTAOL  
01424      SET CFT2-IDX TO 1.                                           ELTAOL  
01425      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTAOL  
01426          SW-INTRNL-NOT-PROF-PROV-CL                               ELTAOL  
01427       TO TRUE.                                                    ELTAOL  
01428      PERFORM 0580-TEST-IPGT-INCLD-ENTRIES                         ELTAOL  
01429         VARYING GX3-INDEX  FROM 1 BY 1                            ELTAOL  
01430           UNTIL    GX3-INDEX = WS-MAX-GX3-INDEX                   ELTAOL  
01431                 OR (    SW-INTRNL-INST-PROV-CL                    ELTAOL  
01432                     AND SW-INTRNL-PROF-PROV-CL ).                 ELTAOL  
01433                                                                   ELTAOL  
01434 ************************************************************      ELTAOL  
01435 *                                                          *      ELTAOL  
01436 *    CHECK INCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELTAOL  
01437 *                                                          *      ELTAOL  
01438 ************************************************************      ELTAOL  
01439                                                                   ELTAOL  
01440  0571-CHK-INCLD-TYPE-IPGS.                                        ELTAOL  
01441      SET CFT9-IDX TO 1.                                           ELTAOL  
01442      SET SW-INTRNL-NOT-PROF-PROV-SP                               ELTAOL  
01443       TO TRUE.                                                    ELTAOL  
01444      PERFORM 0581-TEST-IPGS-INCLD-ENTRIES                         ELTAOL  
01445         VARYING GXS-INDEX  FROM 1 BY 1                            ELTAOL  
01446           UNTIL    GXS-INDEX = WS-MAX-GXS-INDEX                   ELTAOL  
01447                 OR (    SW-INTRNL-PROF-PROV-CL).                  ELTAOL  
01448                                                                   ELTAOL  
01449 ************************************************************      ELTAOL  
01450 *                                                          *      ELTAOL  
01451 *    TEST IPGT INCLUDE ENTRIES TO DETERMINE PROVIDER CLASS *      ELTAOL  
01452 *                                                          *      ELTAOL  
01453 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELTAOL  
01454 *          CFT2 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELTAOL  
01455 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELTAOL  
01456 *          OF THE IPGT TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELTAOL  
01457 *                                                          *      ELTAOL  
01458 ************************************************************      ELTAOL  
01459                                                                   ELTAOL  
01460  0580-TEST-IPGT-INCLD-ENTRIES.                                    ELTAOL  
01461      PERFORM WITH TEST BEFORE                                     ELTAOL  
01462         UNTIL    SW-OCCRNC-APPLIES                                ELTAOL  
01463               OR   CFT2-PT (CFT2-IDX)                             ELTAOL  
01464                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELTAOL  
01465               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELTAOL  
01466         IF   GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELTAOL  
01467            = CFT2-PT (CFT2-IDX)                                   ELTAOL  
01468         THEN                                                      ELTAOL  
01469 *    -- TEST PROVIDER CLASS                                       ELTAOL  
01470            EVALUATE TRUE                                          ELTAOL  
01471               WHEN CFT2-PT-INST (CFT2-IDX)                        ELTAOL  
01472                  SET SW-INTRNL-INST-PROV-CL TO TRUE               ELTAOL  
01473                  IF SRP-ACCUM-PROV-CLASS-INST                     ELTAOL  
01474                  THEN                                             ELTAOL  
01475                     SET SW-OCCRNC-APPLIES TO TRUE                 ELTAOL  
01476                  END-IF                                           ELTAOL  
01477               WHEN CFT2-PT-PROF (CFT2-IDX)                        ELTAOL  
01478                  SET SW-INTRNL-PROF-PROV-CL TO TRUE               ELTAOL  
01479                  IF SRP-ACCUM-PROV-CLASS-PROF                     ELTAOL  
01480                  THEN                                             ELTAOL  
01481                     SET SW-OCCRNC-APPLIES TO TRUE                 ELTAOL  
01482                  END-IF                                           ELTAOL  
01483               END-EVALUATE                                        ELTAOL  
01484         ELSE                                                      ELTAOL  
01485            CONTINUE                                               ELTAOL  
01486         END-IF                                                    ELTAOL  
01487 *    -- BUMP TO NEXT CFT2 TABLE ENTRY                             ELTAOL  
01488         SET CFT2-IDX UP BY 1                                      ELTAOL  
01489         END-PERFORM.                                              ELTAOL  
01490                                                                   ELTAOL  
01491 ************************************************************      ELTAOL  
01492 *                                                          *      ELTAOL  
01493 *    TEST IPGS INCLUDE ENTRIES TO DETERMINE PROVIDER SPEC  *      ELTAOL  
01494 *                                                          *      ELTAOL  
01495 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELTAOL  
01496 *          CFT9 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELTAOL  
01497 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELTAOL  
01498 *          OF THE IPGS TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELTAOL  
01499 *                                                          *      ELTAOL  
01500 ************************************************************      ELTAOL  
01501                                                                   ELTAOL  
01502  0581-TEST-IPGS-INCLD-ENTRIES.                                    ELTAOL  
01503      PERFORM WITH TEST BEFORE                                     ELTAOL  
01504         UNTIL    SW-OCCRNC-APPLIES                                ELTAOL  
01505               OR   CFT9-PT (CFT9-IDX)                             ELTAOL  
01506                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELTAOL  
01507               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELTAOL  
01508         IF   GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELTAOL  
01509            = CFT9-PT (CFT9-IDX)                                   ELTAOL  
01510 *    -- TEST PROVIDER SPEC                                        ELTAOL  
01511         IF CFT9-PT-PROF (CFT9-IDX)                                ELTAOL  
01512           SET SW-INTRNL-PROF-PROV-SP TO TRUE                      ELTAOL  
01513           IF SRP-ACCUM-PROV-SPEC-PROF                             ELTAOL  
01514              SET SW-OCCRNC-APPLIES TO TRUE                        ELTAOL  
01515           END-IF                                                  ELTAOL  
01516         ELSE                                                      ELTAOL  
01517            CONTINUE                                               ELTAOL  
01518         END-IF                                                    ELTAOL  
01519         END-IF                                                    ELTAOL  
01520 *    -- BUMP TO NEXT CFT9 TABLE ENTRY                             ELTAOL  
01521         SET CFT9-IDX UP BY 1                                      ELTAOL  
01522         END-PERFORM.                                              ELTAOL  
01523                                                                   ELTAOL  
01524 /***********************************************************      ELTAOL  
01525 *                                                          *      ELTAOL  
01526 *    CHECK EXCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELTAOL  
01527 *                                                          *      ELTAOL  
01528 ************************************************************      ELTAOL  
01529                                                                   ELTAOL  
01530  0600-CHK-EXCLD-TYPE-IPGT.                                        ELTAOL  
01531                                                                   ELTAOL  
01532 * -- INITIALIZE CFT2 TABLE TO INCLUDE ALL PROVIDER TYPES          ELTAOL  
01533      PERFORM WITH TEST BEFORE                                     ELTAOL  
01534         VARYING CFT2-IDX FROM 1 BY 1                              ELTAOL  
01535           UNTIL CFT2-IDX > CFT2-NBR-TBL-ENTRIES                   ELTAOL  
01536         SET  CFT2-PT-INCLUDE (CFT2-IDX) TO TRUE                   ELTAOL  
01537         END-PERFORM.                                              ELTAOL  
01538                                                                   ELTAOL  
01539 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGT                 ELTAOL  
01540      SET  CFT2-IDX TO 1.                                          ELTAOL  
01541      PERFORM 0610-TAG-EXCLD-IPGT-ENTRIES                          ELTAOL  
01542         VARYING GX3-INDEX FROM 1 BY 1                             ELTAOL  
01543           UNTIL GX3-INDEX = WS-MAX-GX3-INDEX.                     ELTAOL  
01544                                                                   ELTAOL  
01545 * -- CHECK CFT2 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELTAOL  
01546      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTAOL  
01547          SW-INTRNL-NOT-PROF-PROV-CL                               ELTAOL  
01548       TO TRUE.                                                    ELTAOL  
01549      PERFORM 0630-CHK-CFT2-NOT-EXCLD                              ELTAOL  
01550         VARYING CFT2-IDX FROM 1 BY 1                              ELTAOL  
01551           UNTIL    (    SW-INTRNL-INST-PROV-CL                    ELTAOL  
01552                     AND SW-INTRNL-PROF-PROV-CL )                  ELTAOL  
01553                 OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES.               ELTAOL  
01554                                                                   ELTAOL  
01555 /***********************************************************      ELTAOL  
01556 *                                                          *      ELTAOL  
01557 *    CHECK EXCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELTAOL  
01558 *                                                          *      ELTAOL  
01559 ************************************************************      ELTAOL  
01560                                                                   ELTAOL  
01561  0601-CHK-EXCLD-TYPE-IPGS.                                        ELTAOL  
01562                                                                   ELTAOL  
01563 * -- INITIALIZE CFT9 TABLE TO INCLUDE ALL PROVIDER SPEC           ELTAOL  
01564      PERFORM WITH TEST BEFORE                                     ELTAOL  
01565         VARYING CFT9-IDX FROM 1 BY 1                              ELTAOL  
01566           UNTIL CFT9-IDX > CFT9-NBR-TBL-ENTRIES                   ELTAOL  
01567         SET  CFT9-PT-INCLUDE (CFT9-IDX) TO TRUE                   ELTAOL  
01568         END-PERFORM.                                              ELTAOL  
01569                                                                   ELTAOL  
01570 * -- TAG ALL PROVIDER SPEC EXCLUDED BY THIS IPGS                  ELTAOL  
01571      SET  CFT9-IDX TO 1.                                          ELTAOL  
01572      PERFORM 0611-TAG-EXCLD-IPGS-ENTRIES                          ELTAOL  
01573         VARYING GXS-INDEX FROM 1 BY 1                             ELTAOL  
01574           UNTIL GXS-INDEX = WS-MAX-GXS-INDEX.                     ELTAOL  
01575                                                                   ELTAOL  
01576 * -- CHECK CFT9 TABLE FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED ELTAOL  
01577      SET SW-INTRNL-NOT-PROF-PROV-SP                               ELTAOL  
01578       TO TRUE.                                                    ELTAOL  
01579      PERFORM 0631-CHK-CFT9-NOT-EXCLD                              ELTAOL  
01580         VARYING CFT9-IDX FROM 1 BY 1                              ELTAOL  
01581           UNTIL    (    SW-INTRNL-PROF-PROV-SP)                   ELTAOL  
01582                 OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES.               ELTAOL  
01583                                                                   ELTAOL  
01584 ************************************************************      ELTAOL  
01585 *                                                          *      ELTAOL  
01586 *    TAG EXCLUDED IPGT ENTRIES IN CFT2                     *      ELTAOL  
01587 *                                                          *      ELTAOL  
01588 ************************************************************      ELTAOL  
01589                                                                   ELTAOL  
01590  0610-TAG-EXCLD-IPGT-ENTRIES.                                     ELTAOL  
01591      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELTAOL  
01592      PERFORM WITH TEST BEFORE                                     ELTAOL  
01593         UNTIL    SW-ENTRY-FOUND                                   ELTAOL  
01594               OR   CFT2-PT (CFT2-IDX)                             ELTAOL  
01595                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELTAOL  
01596               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELTAOL  
01597         IF   CFT2-PT(CFT2-IDX)                                    ELTAOL  
01598            = GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELTAOL  
01599         THEN                                                      ELTAOL  
01600            SET SW-ENTRY-FOUND TO TRUE                             ELTAOL  
01601            SET CFT2-PT-EXCLUDE (CFT2-IDX) TO TRUE                 ELTAOL  
01602         END-IF                                                    ELTAOL  
01603         SET CFT2-IDX UP BY 1                                      ELTAOL  
01604      END-PERFORM.                                                 ELTAOL  
01605                                                                   ELTAOL  
01606 ************************************************************      ELTAOL  
01607 *                                                          *      ELTAOL  
01608 *    TAG EXCLUDED IPGS ENTRIES IN CFT9                     *      ELTAOL  
01609 *                                                          *      ELTAOL  
01610 ************************************************************      ELTAOL  
01611                                                                   ELTAOL  
01612  0611-TAG-EXCLD-IPGS-ENTRIES.                                     ELTAOL  
01613      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELTAOL  
01614      PERFORM WITH TEST BEFORE                                     ELTAOL  
01615         UNTIL    SW-ENTRY-FOUND                                   ELTAOL  
01616               OR   CFT9-PT (CFT9-IDX)                             ELTAOL  
01617                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELTAOL  
01618               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELTAOL  
01619         IF   CFT9-PT(CFT9-IDX)                                    ELTAOL  
01620            = GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELTAOL  
01621         THEN                                                      ELTAOL  
01622            SET SW-ENTRY-FOUND TO TRUE                             ELTAOL  
01623            SET CFT9-PT-EXCLUDE (CFT9-IDX) TO TRUE                 ELTAOL  
01624         END-IF                                                    ELTAOL  
01625         SET CFT9-IDX UP BY 1                                      ELTAOL  
01626      END-PERFORM.                                                 ELTAOL  
01627                                                                   ELTAOL  
01628 ******************************************************************ELTAOL  
01629 *                                                                *ELTAOL  
01630 *    CHECK CFT2 FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDED     *ELTAOL  
01631 *                                                                *ELTAOL  
01632 ******************************************************************ELTAOL  
01633                                                                   ELTAOL  
01634  0630-CHK-CFT2-NOT-EXCLD.                                         ELTAOL  
01635      IF CFT2-PT-INCLUDE (CFT2-IDX)                                ELTAOL  
01636      THEN                                                         ELTAOL  
01637         EVALUATE TRUE                                             ELTAOL  
01638            WHEN CFT2-PT-INST (CFT2-IDX)                           ELTAOL  
01639               SET SW-INTRNL-INST-PROV-CL TO TRUE                  ELTAOL  
01640               IF SRP-ACCUM-PROV-CLASS-INST                        ELTAOL  
01641               THEN                                                ELTAOL  
01642                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTAOL  
01643               END-IF                                              ELTAOL  
01644            WHEN CFT2-PT-PROF (CFT2-IDX)                           ELTAOL  
01645               SET SW-INTRNL-PROF-PROV-CL TO TRUE                  ELTAOL  
01646               IF SRP-ACCUM-PROV-CLASS-PROF                        ELTAOL  
01647               THEN                                                ELTAOL  
01648                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTAOL  
01649               END-IF                                              ELTAOL  
01650            END-EVALUATE                                           ELTAOL  
01651      END-IF.                                                      ELTAOL  
01652                                                                   ELTAOL  
01653 ******************************************************************ELTAOL  
01654 *                                                                *ELTAOL  
01655 *    CHECK CFT9 FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED     * ELTAOL  
01656 *                                                                *ELTAOL  
01657 ******************************************************************ELTAOL  
01658                                                                   ELTAOL  
01659  0631-CHK-CFT9-NOT-EXCLD.                                         ELTAOL  
01660      IF CFT9-PT-INCLUDE (CFT9-IDX)                                ELTAOL  
01661         IF CFT9-PT-PROF (CFT9-IDX)                                ELTAOL  
01662           SET SW-INTRNL-PROF-PROV-SP TO TRUE                      ELTAOL  
01663           IF SRP-ACCUM-PROV-SPEC-PROF                             ELTAOL  
01664              SET SW-OCCRNC-APPLIES TO TRUE                        ELTAOL  
01665           END-IF                                                  ELTAOL  
01666      END-IF.                                                      ELTAOL  
01667                                                                   ELTAOL  
01668 /*****************************************************************ELTAOL  
01669 *                                                                *ELTAOL  
01670 *    CHECK IBGR INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELTAOL  
01671 *                                                                *ELTAOL  
01672 ******************************************************************ELTAOL  
01673                                                                   ELTAOL  
01674  0640-CHK-IBGR-PROV-CL.                                           ELTAOL  
01675      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTAOL  
01676          SW-INTRNL-NOT-PROF-PROV-CL TO TRUE.                      ELTAOL  
01677      MOVE PC-IBGR TO KWA-PROVISION-ID.                            ELTAOL  
01678      MOVE WS-IBGR-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTAOL  
01679      PERFORM 0660-READ-INTRLN-TAB.                                ELTAOL  
01680      SET ADDRESS OF GX1-RECORD-AREA TO IOP-REC-PTR.               ELTAOL  
01681      SET GX1-INDEX                  TO GX1-ENTRY-COUNT.           ELTAOL  
01682      SET WS-MAX-GX1-INDEX           TO GX1-INDEX.                 ELTAOL  
01683                                                                   ELTAOL  
01684      IF GX1-ID-ARGUMENT-EXCLUDED                                  ELTAOL  
01685      THEN                                                         ELTAOL  
01686 *    -- ASSUME THAT IBGR WOULD NOT EXCLUDE ALL OF ANY PROVIDER    ELTAOL  
01687 *       CLASS (I.E., BOTH TYPES APPLY).                           ELTAOL  
01688         SET SW-OCCRNC-APPLIES                                     ELTAOL  
01689             SW-INTRNL-INST-PROV-CL                                ELTAOL  
01690             SW-INTRNL-PROF-PROV-CL                                ELTAOL  
01691          TO TRUE                                                  ELTAOL  
01692      ELSE                                                         ELTAOL  
01693         PERFORM 0650-CHK-INCLD-TYPE-IBGR                          ELTAOL  
01694      END-IF.                                                      ELTAOL  
01695                                                                   ELTAOL  
01696 /*****************************************************************ELTAOL  
01697 *                                                                *ELTAOL  
01698 *    CHECK INCLUDE TYPE IBGR TO DETERMINE PROVIDER CLASS         *ELTAOL  
01699 *                                                                *ELTAOL  
01700 ******************************************************************ELTAOL  
01701                                                                   ELTAOL  
01702  0650-CHK-INCLD-TYPE-IBGR.                                        ELTAOL  
01703      PERFORM WITH TEST BEFORE                                     ELTAOL  
01704         VARYING GX1-INDEX FROM 1 BY 1                             ELTAOL  
01705           UNTIL    GX1-INDEX = WS-MAX-GX1-INDEX                   ELTAOL  
01706                 OR (    SW-INTRNL-INST-PROV-CL                    ELTAOL  
01707                     AND SW-INTRNL-PROF-PROV-CL )                  ELTAOL  
01708         MOVE GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)                ELTAOL  
01709           TO WS-PROVISION-ARGUMENT                                ELTAOL  
01710         EVALUATE TRUE                                             ELTAOL  
01711            WHEN INST-CL                                           ELTAOL  
01712               SET SW-INTRNL-INST-PROV-CL TO TRUE                  ELTAOL  
01713               IF SRP-ACCUM-PROV-CLASS-INST                        ELTAOL  
01714               THEN                                                ELTAOL  
01715                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTAOL  
01716               END-IF                                              ELTAOL  
01717            WHEN PROF-CL                                           ELTAOL  
01718               SET SW-INTRNL-PROF-PROV-CL TO TRUE                  ELTAOL  
01719               IF SRP-ACCUM-PROV-CLASS-PROF                        ELTAOL  
01720               THEN                                                ELTAOL  
01721                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTAOL  
01722               END-IF                                              ELTAOL  
01723            END-EVALUATE                                           ELTAOL  
01724         END-PERFORM.                                              ELTAOL  
01725                                                                   ELTAOL  
01726 /***********************************************************      ELTAOL  
01727 *                                                          *      ELTAOL  
01728 *    READ THE INTERNAL TABULAR RECORD                      *      ELTAOL  
01729 *                                                          *      ELTAOL  
01730 ************************************************************      ELTAOL  
01731                                                                   ELTAOL  
01732  0660-READ-INTRLN-TAB.                                            ELTAOL  
01733      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELTAOL  
01734      SET IOP-RD TO TRUE.                                          ELTAOL  
01735      SET IOP-FCQ-NONE TO TRUE.                                    ELTAOL  
01736      SET IOP-KVQ-EQ TO TRUE.                                      ELTAOL  
01737      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELTAOL  
01738      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTAOL  
01739      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTAOL  
01740      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTAOL  
01741                                                                   ELTAOL  
01742      EVALUATE TRUE                                                ELTAOL  
01743         WHEN IOP-RC-OK                                            ELTAOL  
01744            CONTINUE                                               ELTAOL  
01745         WHEN IOP-RC-NOTFND                                        ELTAOL  
01746            SET CIA-AB-NOTFND-GCTABULR TO TRUE                     ELTAOL  
01747            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELTAOL  
01748         WHEN OTHER                                                ELTAOL  
01749             SET CIA-AB-CRITIO TO TRUE                             ELTAOL  
01750             EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC           ELTAOL  
01751         END-EVALUATE.                                             ELTAOL  
01752                                                                   ELTAOL  
01753 /***********************************************************      ELTAOL  
01754 *                                                          *      ELTAOL  
01755 *        ADD ACCUM OCCURRENCE TO FILE                      *      ELTAOL  
01756 *                                                          *      ELTAOL  
01757 ************************************************************      ELTAOL  
01758                                                                   ELTAOL  
01759  0710-WRITE-EXTRACT-RECORD.                                       ELTAOL  
01760      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELTAOL  
01761      SET  IOP-ADD TO TRUE.                                        ELTAOL  
01762      SET  IOP-FCQ-NONE TO TRUE.                                   ELTAOL  
01763      SET  IOP-KVQ-NONE TO TRUE.                                   ELTAOL  
01764      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTAOL  
01765                                                                   ELTAOL  
01766 /***********************************************************      ELTAOL  
01767 *                                                          *      ELTAOL  
01768 *    ESTABLISH ADDRESSABILITY OF THE TABULAR FILE          *      ELTAOL  
01769 *                                                          *      ELTAOL  
01770 ************************************************************      ELTAOL  
01771                                                                   ELTAOL  
01772  9060-EST-ADR-TABULAR-FILE.                                       ELTAOL  
01773      SET  CIA-GCTABULR-DDN TO TRUE.                               ELTAOL  
01774      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAOL  
01775         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELTAOL  
01776         END-CALL.                                                 ELTAOL  
01777      IF CIA-RC-PTR-NULL                                           ELTAOL  
01778      THEN                                                         ELTAOL  
01779         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTAOL  
01780         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTAOL  
01781      END-IF.                                                      ELTAOL  
01782                                                                   ELTAOL  
01783 /***********************************************************      ELTAOL  
01784 *                                                          *      ELTAOL  
01785 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELTAOL  
01786 *                                                          *      ELTAOL  
01787 ************************************************************      ELTAOL  
01788                                                                   ELTAOL  
01789  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELTAOL  
01790      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELTAOL  
01791      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAOL  
01792         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELTAOL  
01793         END-CALL.                                                 ELTAOL  
01794      IF CIA-RC-PTR-NULL                                           ELTAOL  
01795         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTAOL  
01796         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTAOL  
01797      END-IF.                                                      ELTAOL  
