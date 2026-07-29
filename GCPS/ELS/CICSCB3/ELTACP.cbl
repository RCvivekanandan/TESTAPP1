00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELTACP  
00003  PROGRAM-ID.        ELTACP.                                          LV001
00004                                                                   ELTACP  
00005  AUTHOR.            ANNE KEFFER KING.                             ELTACP  
00006                                                                   ELTACP  
00007  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELTACP  
00008                     A MUTUAL LEGAL RESERVE COMPANY                ELTACP  
00009                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELTACP  
00010                     233 N. MICHIGAN AVE                           ELTACP  
00011                     CHICAGO, ILLINOIS 60601                       ELTACP  
00012                                                                   ELTACP  
00013  DATE-WRITTEN.      20-OCT-1998.                                  ELTACP  
00014  DATE-COMPILED.                                                   ELTACP  
00015                                                                   ELTACP  
00016  SECURITY.          COPYRIGHT 1986,1993,                          ELTACP  
00017                     HEALTH CARE SERVICE CORPORATION               ELTACP  
00018                                                                   ELTACP  
00019  ENVIRONMENT DIVISION.                                            ELTACP  
00020                                                                   ELTACP  
00021  CONFIGURATION SECTION.                                           ELTACP  
00022  SOURCE-COMPUTER.    IBM-3090.                                    ELTACP  
00023  OBJECT-COMPUTER.    IBM-3090.                                    ELTACP  
00024                                                                   ELTACP  
00025 /*****************************************************************ELTACP  
00026 *                                                                *ELTACP  
00027 *  ELTACP - ELS:    SELECTS #ACP (CO-PAY) ACCUMULATORS AND       *ELTACP  
00028 *                   SETUPS THE INFORMATION TO BE PROCESSED BY    *ELTACP  
00029 *                   THE CO-PAY     GENERATOR MODULE. THE ACCUMS  *ELTACP  
00030 *                   ARE SELECTED FROM THE GROUP SPECIFIC AND     *ELTACP  
00031 *                   CONTRACT LEVEL PROCESSING.                   *ELTACP  
00032 *                                                                *ELTACP  
00033 ******************************************************************ELTACP  
00034 *                                                                *ELTACP  
00035 *                      MAINTENANCE HISTORY                       *ELTACP  
00036 *                                                                *ELTACP  
00037 *  MOD     DATE     BY  DRPT                ACTION               *ELTACP  
00038 * ----- ----------- --- ----- ---------------------------------- *ELTACP  
00039 * 01.00 20-OCT-1998 AKK       CLONED FROM VERSION OF ELTADL,     *ELTACP  
00040 *                             THAT WAS WRITTEN BY RJL.           *ELTACP  
00041 *                                                                *ELTACP  
00042 * 01.01 25-SEP-2000 AKK       ADDED SUPPORT FOR #IPGS            *ELTACP  
00043 *                                                                *ELTACP  
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00056 *                                                                *ELGABMCC
00044 ******************************************************************ELTACP  
00045 /                                                                 ELTACP  
00046  DATA DIVISION.                                                   ELTACP  
00047                                                                   ELTACP  
00048  WORKING-STORAGE SECTION.                                         ELTACP  
00049                                                                   ELTACP  
00050  01  SWITCHES.                                                    ELTACP  
00051      02                                      PICTURE  X(01).      ELTACP  
00052         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELTACP  
00053         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELTACP  
00054                                                                   ELTACP  
00055      02 WS-STOP-SW                           PICTURE  X(01).      ELTACP  
00056         88 WS-STOP                           VALUE 'Y'.           ELTACP  
00057                                                                   ELTACP  
00058      02 WS-PAIR-FOUND-SW                     PICTURE  X(01).      ELTACP  
00059         88 WS-PAIR-FOUND                     VALUE 'Y'.           ELTACP  
00060                                                                   ELTACP  
00061      02 WS-MATCH-SW                          PICTURE  X(01).      ELTACP  
00062         88 WS-MATCH-FOUND                    VALUE 'Y'.           ELTACP  
00063                                                                   ELTACP  
00064      02 FIRST-TIME-SW                        PICTURE  X(01).      ELTACP  
00065         88 FIRST-ACP                         VALUE 'Y'.           ELTACP  
00066         88 NOT-FIRST-ACP                     VALUE 'N'.           ELTACP  
00067      02 OCCURRENCE-APPLIES                   PICTURE  X(01).      ELTACP  
00068         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELTACP  
00069         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELTACP  
00070      02                                      PICTURE  X(01).      ELTACP  
00071         88 SW-HAS-IBGR                       VALUE 'Y'.           ELTACP  
00072         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELTACP  
00073      02                                      PICTURE  X(01).      ELTACP  
00074         88 SW-HAS-IDGD                       VALUE 'Y'.           ELTACP  
00075         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELTACP  
00076      02                                      PICTURE  X(01).      ELTACP  
00077         88 SW-HAS-IPGN                       VALUE 'Y'.           ELTACP  
00078         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELTACP  
00079      02                                      PICTURE  X(01).      ELTACP  
00080         88 SW-HAS-IPGP                       VALUE 'Y'.           ELTACP  
00081         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELTACP  
00082      02                                      PICTURE  X(01).      ELTACP  
00083         88 SW-HAS-IPGT                       VALUE 'Y'.           ELTACP  
00084         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELTACP  
00085      02                                      PICTURE  X(01).      ELTACP  
00086         88 SW-HAS-IPGS                       VALUE 'Y'.           ELTACP  
00087         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELTACP  
00088      02                                      PICTURE  X(01).      ELTACP  
00089         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELTACP  
00090         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELTACP  
00091      02                                      PICTURE  X(01).      ELTACP  
00092         88 SW-INTRNL-INST-PROV-CL            VALUE 'Y'.           ELTACP  
00093         88 SW-INTRNL-NOT-INST-PROV-CL        VALUE 'N'.           ELTACP  
00094         88 SW-INTRNL-INST-PROV-CL-NOT-DET VALUE 'X'.              ELTACP  
00095      02                                      PICTURE  X(01).      ELTACP  
00096         88 SW-INTRNL-PROF-PROV-CL            VALUE 'Y'.           ELTACP  
00097         88 SW-INTRNL-NOT-PROF-PROV-CL        VALUE 'N'.           ELTACP  
00098         88 SW-INTRNL-PROF-PROV-CL-NOT-DET VALUE 'X'.              ELTACP  
00099      02                                      PICTURE  X(01).      ELTACP  
00100         88 SW-INTRNL-INST-PROV-SP            VALUE 'Y'.           ELTACP  
00101         88 SW-INTRNL-NOT-INST-PROV-SP        VALUE 'N'.           ELTACP  
00102         88 SW-INTRNL-INST-PROV-SP-NOT-DET VALUE 'X'.              ELTACP  
00103      02                                      PICTURE  X(01).      ELTACP  
00104         88 SW-INTRNL-PROF-PROV-SP            VALUE 'Y'.           ELTACP  
00105         88 SW-INTRNL-NOT-PROF-PROV-SP        VALUE 'N'.           ELTACP  
00106         88 SW-INTRNL-PROF-PROV-SP-NOT-DET VALUE 'X'.              ELTACP  
00107      02                                      PICTURE  X(01).      ELTACP  
00108         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELTACP  
00109         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELTACP  
00110                                                                   ELTACP  
00111  01  WS-PROVISION-ARGUMENT.                                       ELTACP  
00112      02                          PICTURE  X(05).                  ELTACP  
00113      02 WS-PROVISION-CL          PICTURE  X(01).                  ELTACP  
00114         88 INST-CL               VALUE 'A', 'B', 'W'.             ELTACP  
00115         88 PROF-CL               VALUE 'C', 'D', 'E'.             ELTACP  
00116                                                                   ELTACP  
00117  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELTACP  
00118      88 WS-LOB-INST              VALUE '1'.                       ELTACP  
00119      88 WS-LOB-PROF              VALUE '2'.                       ELTACP  
00120      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELTACP  
00121      88 WS-LOB-BOTH              VALUE '3', '4', '5', '6', '7'.   ELTACP  
00122                                                                   ELTACP  
00123  01  PROGRAM-CONSTANTS.                                           ELTACP  
00124      02 PC-ACP                   PICTURE  X(06) VALUE '#ACP  '.   ELTACP  
00125      02 PC-GCT-MAX-SUB           PICTURE S9(04) COMP.             ELTACP  
00126      02 WS-HOLD-SUB              PICTURE S9(04) COMP.             ELTACP  
00127      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELTACP  
00128      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELTACP  
00129      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELTACP  
00130      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELTACP  
00131      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELTACP  
00132      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGS '.   ELTACP  
00133      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELTACP  
00134                                                                   ELTACP  
00135  01  WS-WORK-FIELDS.                                              ELTACP  
00136      02 WS-OCCURRENCE-SUB        PICTURE S9(04) COMP.             ELTACP  
00137      02 WS-ACP-SUB               PICTURE S9(04) COMP.             ELTACP  
00138      02 WS-ACP-ACCUM-CNT         PICTURE S9(04) COMP.             ELTACP  
00139      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELTACP  
00140      02 HOLD-DEFINITION          PICTURE X(02).                   ELTACP  
00141                                                                   ELTACP  
00142  01  WS-OCCURRENCE-PROCESSED-TBL.                                 ELTACP  
00143      02                          PICTURE X                        ELTACP  
00144                                  OCCURS 44 TIMES                  ELTACP  
00145                                  INDEXED BY WS-OCCURRENCE-INDEX.  ELTACP  
00146          88  WS-OCCURRENCE-PROCESSED           VALUE 'P'.         ELTACP  
00147          88  WS-OCCURRENCE-NOT-PROCESSED       VALUE ' '.         ELTACP  
00148                                                                   ELTACP  
00149  01  WS-POINTERS.                                                 ELTACP  
00150      02  WS-INST-CNTRCT-PTR      POINTER.                         ELTACP  
00151      02  WS-PROF-CNTRCT-PTR      POINTER.                         ELTACP  
00152                                                                   ELTACP  
00153  01  WS-MAX-INDEX-VALUES.                                         ELTACP  
00154      02 WS-HOLD-IDX              USAGE IS INDEX.                  ELTACP  
00155      02 WS-MAX-GX1-INDEX         USAGE IS INDEX.                  ELTACP  
00156      02 WS-MAX-GX3-INDEX         USAGE IS INDEX.                  ELTACP  
00157      02 WS-MAX-GXS-INDEX         USAGE IS INDEX.                  ELTACP  
00158      02 WS-MAX-GAF-INDEX         USAGE IS INDEX.                  ELTACP  
00159      02 WS-MAX-GCT-INDEX         USAGE IS INDEX.                  ELTACP  
00160      02 WS-MAX-GCG-INDEX         USAGE IS INDEX.                  ELTACP  
00161                                                                   ELTACP  
00162  01  ACCUM-HOLD-TBL.                                              ELTACP  
00163      02  ACCUM-SLOT-NBR          PICTURE S9(07) COMP-3            ELTACP  
00164                                  OCCURS 5 TIMES.                  ELTACP  
00165                                                                   ELTACP  
00166  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELTACP  
00167      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTACP  
00168      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTACP  
00169      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTACP  
00170      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTACP  
00171      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTACP  
00172      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTACP  
00173                                                                   ELTACP  
00174 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELTACP  
00175      COPY ELSCFTB2.                                               ELTACP  
00176                                                                   ELTACP  
00177 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELTACP  
00178      COPY ELSCFTB9.                                               ELTACP  
00179                                                                   ELTACP  
00180      TITLE  'ELTACP          LINKAGE SECTION'                     ELTACP  
00181  LINKAGE SECTION.                                                 ELTACP  
00182  01  DFHCOMMAREA.                                                 ELTACP  
00183      COPY ELSCOMMC.                                               ELTACP  
00184 /                                                                 ELTACP  
00185      COPY ELSCIA2C.                                               ELTACP  
00186 /                                                                 ELTACP  
00187      COPY ELSIOPMC.                                               ELTACP  
00188 /                                                                 ELTACP  
00189      COPY ELSKEYSC.                                               ELTACP  
00190 /                                                                 ELTACP  
00191      COPY ELSSRTPC.                                               ELTACP  
00192 /                                                                 ELTACP  
00193      COPY ELSSSCBC.                                               ELTACP  
00194 /                                                                 ELTACP  
00195  01  GCG-GRP-SPEC-RECORD-AREA.                                    ELTACP  
00196      COPY GCGROUPC.                                               ELTACP  
00197 /                                                                 ELTACP  
00198  01  GCT-CONTRACT-RECORD-AREA.                                    ELTACP  
00199      COPY GCCONTRC.                                               ELTACP  
00200 /                                                                 ELTACP  
00201  01  GAF-RECORD-AREA.                                             ELTACP  
00202      COPY GCTACPC.                                                ELTACP  
00203 /                                                                 ELTACP  
00204      COPY ELSACUMC.                                               ELTACP  
00205 /                                                                 ELTACP  
00206  01  GX1-RECORD-AREA.                                             ELTACP  
00207      COPY GCTIBGRC.                                               ELTACP  
00208                                                                   ELTACP  
00209  01  GXS-RECORD-AREA.                                             ELTACP  
00210      COPY GCTIPGSC.                                               ELTACP  
00211 /                                                                 ELTACP  
00212  01  GX3-RECORD-AREA.                                             ELTACP  
00213      COPY GCTIPGTC.                                               ELTACP  
00214 /    TITLE  'ELTACP          PROCEDURE DIVISION'.                 ELTACP  
00215 ************************************************************      ELTACP  
00216 *                                                          *      ELTACP  
00217 *    ELTACP MAINLINE                                       *      ELTACP  
00218 *                                                          *      ELTACP  
00219 ************************************************************      ELTACP  
00220                                                                   ELTACP  
00221  PROCEDURE DIVISION.                                              ELTACP  
00222                                                                   ELTACP  
00223      PERFORM 0010-INITIALIZATION.                                 ELTACP  
00224      SET FIRST-ACP TO TRUE.                                       ELTACP  
00225      PERFORM 0100-PROCESS.                                        ELTACP  
00226      GOBACK.                                                      ELTACP  
00227                                                                   ELTACP  
00228 ************************************************************      ELTACP  
00229 *                                                          *      ELTACP  
00230 *    INITIALIZATION                                        *      ELTACP  
00231 *                                                          *      ELTACP  
00232 ************************************************************      ELTACP  
00233                                                                   ELTACP  
00234  0010-INITIALIZATION.                                             ELTACP  
00235      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELTACP  
00236      PERFORM 0030-EST-ADR-KEY-WK-AREA.                            ELTACP  
00237      PERFORM 0040-EST-ADR-OF-SUBROUTINE-PAR.                      ELTACP  
00238      PERFORM 0050-EST-ADR-GRP-SPC.                                ELTACP  
00239      PERFORM 0060-INIT-DATA.                                      ELTACP  
00240                                                                   ELTACP  
00241 /***********************************************************      ELTACP  
00242 *                                                          *      ELTACP  
00243 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELTACP  
00244 *                                                          *      ELTACP  
00245 ************************************************************      ELTACP  
00246                                                                   ELTACP  
00247  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELTACP  
00248      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTACP  
00249      THEN                                                         ELTACP  
00250         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELTACP  
00251      ELSE                                                         ELTACP  
00252         IF ECA-CIA-PTR = NULL                                     ELTACP  
00253         THEN                                                      ELTACP  
00254            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELTACP  
00255         ELSE                                                      ELTACP  
00256            CALL 'ELUINISM' USING DFHCOMMAREA                      ELTACP  
00257               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELTACP  
00258               END-CALL                                            ELTACP  
00259            SET CIA-ELSSSCB-DDN TO TRUE                            ELTACP  
00260            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELTACP  
00261               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELTACP  
00262               END-CALL                                            ELTACP  
00263            IF CIA-RC-PTR-NULL                                     ELTACP  
00264            THEN                                                   ELTACP  
00265               PERFORM 0099-CIA-AB-UNALLOC-AREA                    ELTACP  
00266            ELSE                                                   ELTACP  
00267               CONTINUE                                            ELTACP  
00268            END-IF                                                 ELTACP  
00269         END-IF                                                    ELTACP  
00270      END-IF.                                                      ELTACP  
00271                                                                   ELTACP  
00272 /***********************************************************      ELTACP  
00273 *                                                          *      ELTACP  
00274 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELTACP  
00275 *                                                          *      ELTACP  
00276 ************************************************************      ELTACP  
00277                                                                   ELTACP  
00278  0030-EST-ADR-KEY-WK-AREA.                                        ELTACP  
00279      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTACP  
00280      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACP  
00281         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELTACP  
00282         END-CALL.                                                 ELTACP  
00283      IF CIA-RC-PTR-NULL                                           ELTACP  
00284         PERFORM 0099-CIA-AB-UNALLOC-AREA.                         ELTACP  
00285                                                                   ELTACP  
00286 ************************************************************      ELTACP  
00287 *                                                          *      ELTACP  
00288 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTACP  
00289 *                                                          *      ELTACP  
00290 ************************************************************      ELTACP  
00291                                                                   ELTACP  
00292  0040-EST-ADR-OF-SUBROUTINE-PAR.                                  ELTACP  
00293      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTACP  
00294      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACP  
00295         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELTACP  
00296         END-CALL.                                                 ELTACP  
00297      IF CIA-RC-PTR-NULL                                           ELTACP  
00298         PERFORM 0099-CIA-AB-UNALLOC-AREA.                         ELTACP  
00299                                                                   ELTACP  
00300 /***********************************************************      ELTACP  
00301 *                                                          *      ELTACP  
00302 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD     *      ELTACP  
00303 *                                                          *      ELTACP  
00304 ************************************************************      ELTACP  
00305                                                                   ELTACP  
00306  0050-EST-ADR-GRP-SPC.                                            ELTACP  
00307      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTACP  
00308      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACP  
00309         ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                       ELTACP  
00310         END-CALL.                                                 ELTACP  
00311      IF CIA-RC-PTR-NULL                                           ELTACP  
00312         PERFORM 0099-CIA-AB-UNALLOC-AREA.                         ELTACP  
00313                                                                   ELTACP  
00314 ************************************************************      ELTACP  
00315 *                                                          *      ELTACP  
00316 *    INITIALIZE DATA AREAS                                 *      ELTACP  
00317 *                                                          *      ELTACP  
00318 ************************************************************      ELTACP  
00319                                                                   ELTACP  
00320  0060-INIT-DATA.                                                  ELTACP  
00321      COMPUTE PC-GCT-MAX-SUB =   LENGTH OF GCT-CONT-TAB-PTRS       ELTACP  
00322                               / LENGTH OF GCT-CON-TAB-ID-SLOT.    ELTACP  
00323      SET GCT-INDEX        TO PC-GCT-MAX-SUB.                      ELTACP  
00324      SET WS-MAX-GCT-INDEX TO GCT-INDEX.                           ELTACP  
00325      SET GCG-INDEX        TO GCG-COUNT-TAB-PROVN-POINTERS.        ELTACP  
00326      SET WS-MAX-GCG-INDEX TO GCG-INDEX.                           ELTACP  
00327      INITIALIZE WS-ACP-ACCUM-CNT.                                 ELTACP  
00328      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELTACP  
00329                                                                   ELTACP  
00330 /***********************************************************      ELTACP  
00331 *                                                          *      ELTACP  
00332 *    ABEND CIA UNALLOCATED AREA                            *      ELTACP  
00333 *                                                          *      ELTACP  
00334 ************************************************************      ELTACP  
00335                                                                   ELTACP  
00336  0099-CIA-AB-UNALLOC-AREA.                                        ELTACP  
00337      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTACP  
00338      EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.               ELTACP  
00339                                                                   ELTACP  
00340 ************************************************************      ELTACP  
00341 *                                                          *      ELTACP  
00342 *        PROCESS                                           *      ELTACP  
00343 *                                                          *      ELTACP  
00344 ************************************************************      ELTACP  
00345                                                                   ELTACP  
00346  0100-PROCESS.                                                    ELTACP  
00347      PERFORM 0110-SCAN-GRP-SPC-FOR-ACCUMS.                        ELTACP  
00348      PERFORM 0120-SCAN-CONTRACTS-FOR-ACCUMS.                      ELTACP  
00349                                                                   ELTACP  
00350      IF WS-ACP-ACCUM-CNT >  0                                     ELTACP  
00351      THEN                                                         ELTACP  
00352          PERFORM 0210-SCAN-FOR-APPLIC-OCCRNCS                     ELTACP  
00353      END-IF.                                                      ELTACP  
00354                                                                   ELTACP  
00355      IF SW-NO-APPLIC-ACCUM-FOUND                                  ELTACP  
00356      THEN                                                         ELTACP  
00357         EVALUATE TRUE                                             ELTACP  
00358            WHEN SSB-PROV-CLASS-INST                               ELTACP  
00359               SET SRP-INST-NOT-APPLICABLE TO TRUE                 ELTACP  
00360            WHEN SSB-PROV-CLASS-PROF                               ELTACP  
00361               SET SRP-PROF-NOT-APPLICABLE TO TRUE                 ELTACP  
00362            WHEN SSB-PROV-CLASS-BOTH                               ELTACP  
00363               SET SRP-NO-ACCUMS-FOUND TO TRUE                     ELTACP  
00364            WHEN OTHER                                             ELTACP  
00365               SET CIA-AB-PGM-LOGIC TO TRUE                        ELTACP  
00366               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELTACP  
00367            END-EVALUATE                                           ELTACP  
00368      END-IF.                                                      ELTACP  
00369                                                                   ELTACP  
00370      SET SRP-TOPIC-ACCUM TO TRUE.                                 ELTACP  
00371                                                                   ELTACP  
00372 * -- LINK TO THE OUTPUT GENERATOR                                 ELTACP  
00373      EXEC CICS LINK PROGRAM ('ELGACP') COMMAREA (DFHCOMMAREA)     ELTACP  
00374         END-EXEC.                                                 ELTACP  
00375                                                                   ELTACP  
00376 /***********************************************************      ELTACP  
00377 *                                                          *      ELTACP  
00378 *    SCAN GROUP SPECIFIC RECORD FOR ACCUMULATORS           *      ELTACP  
00379 *                                                          *      ELTACP  
00380 ************************************************************      ELTACP  
00381                                                                   ELTACP  
00382  0110-SCAN-GRP-SPC-FOR-ACCUMS.                                    ELTACP  
00383      PERFORM WITH TEST BEFORE                                     ELTACP  
00384         VARYING GCG-INDEX FROM 1 BY 1                             ELTACP  
00385           UNTIL GCG-INDEX = WS-MAX-GCG-INDEX                      ELTACP  
00386                 OR GCG-TAB-ID (GCG-INDEX) > PC-ACP                ELTACP  
00387 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELTACP  
00388         IF     GCG-TAB-ID (GCG-INDEX)  =  PC-ACP                  ELTACP  
00389            AND GCG-TAB-SLOT-NO (GCG-INDEX)  >  ZERO               ELTACP  
00390         THEN                                                      ELTACP  
00391 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELTACP  
00392            MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO WS-SLOT-NBR        ELTACP  
00393            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELTACP  
00394         END-IF                                                    ELTACP  
00395         END-PERFORM.                                              ELTACP  
00396                                                                   ELTACP  
00397 /***********************************************************      ELTACP  
00398 *                                                          *      ELTACP  
00399 *    SCAN CONTRACT RECORDS FOR ACCUMULATORS                *      ELTACP  
00400 *                                                          *      ELTACP  
00401 ************************************************************      ELTACP  
00402                                                                   ELTACP  
00403  0120-SCAN-CONTRACTS-FOR-ACCUMS.                                  ELTACP  
00404      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELTACP  
00405      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELTACP  
00406                                                                   ELTACP  
00407      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTACP  
00408      THEN                                                         ELTACP  
00409          PERFORM 0130-SCAN-INST-BAS                               ELTACP  
00410      END-IF.                                                      ELTACP  
00411                                                                   ELTACP  
00412      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTACP  
00413      THEN                                                         ELTACP  
00414          PERFORM 0140-SCAN-PROF-BAS                               ELTACP  
00415      END-IF.                                                      ELTACP  
00416                                                                   ELTACP  
00417      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELTACP  
00418      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELTACP  
00419                                                                   ELTACP  
00420      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTACP  
00421      THEN                                                         ELTACP  
00422          PERFORM 0150-SCAN-INST-SUP                               ELTACP  
00423      END-IF.                                                      ELTACP  
00424                                                                   ELTACP  
00425      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTACP  
00426      THEN                                                         ELTACP  
00427          PERFORM 0160-SCAN-PROF-SUP                               ELTACP  
00428      END-IF.                                                      ELTACP  
00429                                                                   ELTACP  
00430 /***********************************************************      ELTACP  
00431 *                                                          *      ELTACP  
00432 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELTACP  
00433 *                                                          *      ELTACP  
00434 ************************************************************      ELTACP  
00435                                                                   ELTACP  
00436  0130-SCAN-INST-BAS.                                              ELTACP  
00437      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTACP  
00438      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACP  
00439         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTACP  
00440         END-CALL.                                                 ELTACP  
00441      SET WS-INST-CNTRCT-PTR                                       ELTACP  
00442       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTACP  
00443                                                                   ELTACP  
00444      IF CIA-RC-PTR-NULL                                           ELTACP  
00445      THEN                                                         ELTACP  
00446         CONTINUE                                                  ELTACP  
00447      ELSE                                                         ELTACP  
00448         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTACP  
00449      END-IF.                                                      ELTACP  
00450                                                                   ELTACP  
00451 ************************************************************      ELTACP  
00452 *                                                          *      ELTACP  
00453 *    SCAN PROFESSIONAL BASIC CONTRACT RECORD               *      ELTACP  
00454 *                                                          *      ELTACP  
00455 ************************************************************      ELTACP  
00456                                                                   ELTACP  
00457  0140-SCAN-PROF-BAS.                                              ELTACP  
00458      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTACP  
00459      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACP  
00460         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTACP  
00461         END-CALL.                                                 ELTACP  
00462      SET WS-PROF-CNTRCT-PTR                                       ELTACP  
00463       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTACP  
00464                                                                   ELTACP  
00465      IF    CIA-RC-PTR-NULL                                        ELTACP  
00466         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELTACP  
00467      THEN                                                         ELTACP  
00468         CONTINUE                                                  ELTACP  
00469      ELSE                                                         ELTACP  
00470         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTACP  
00471      END-IF.                                                      ELTACP  
00472                                                                   ELTACP  
00473 /***********************************************************      ELTACP  
00474 *                                                          *      ELTACP  
00475 *    SCAN INSTITUTIONAL SUPPLEMENTAL CONTRACT RECORD       *      ELTACP  
00476 *                                                          *      ELTACP  
00477 ************************************************************      ELTACP  
00478                                                                   ELTACP  
00479  0150-SCAN-INST-SUP.                                              ELTACP  
00480      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTACP  
00481      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACP  
00482         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTACP  
00483         END-CALL.                                                 ELTACP  
00484      SET WS-INST-CNTRCT-PTR                                       ELTACP  
00485       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTACP  
00486                                                                   ELTACP  
00487      IF CIA-RC-PTR-NULL                                           ELTACP  
00488      THEN                                                         ELTACP  
00489         CONTINUE                                                  ELTACP  
00490      ELSE                                                         ELTACP  
00491         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTACP  
00492      END-IF.                                                      ELTACP  
00493                                                                   ELTACP  
00494 ************************************************************      ELTACP  
00495 *                                                          *      ELTACP  
00496 *    SCAN PROFESSIONAL SUPPLEMENTAL CONTRACT RECORD        *      ELTACP  
00497 *                                                          *      ELTACP  
00498 ************************************************************      ELTACP  
00499                                                                   ELTACP  
00500  0160-SCAN-PROF-SUP.                                              ELTACP  
00501      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTACP  
00502      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACP  
00503         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTACP  
00504         END-CALL.                                                 ELTACP  
00505      SET WS-PROF-CNTRCT-PTR                                       ELTACP  
00506       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTACP  
00507                                                                   ELTACP  
00508      IF    CIA-RC-PTR-NULL                                        ELTACP  
00509         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELTACP  
00510      THEN                                                         ELTACP  
00511         CONTINUE                                                  ELTACP  
00512      ELSE                                                         ELTACP  
00513         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTACP  
00514      END-IF.                                                      ELTACP  
00515                                                                   ELTACP  
00516 /***********************************************************      ELTACP  
00517 *                                                          *      ELTACP  
00518 *    SCAN A CONTRACT RECORD FOR ACCUMULATORS               *      ELTACP  
00519 *                                                          *      ELTACP  
00520 ************************************************************      ELTACP  
00521                                                                   ELTACP  
00522  0170-SCAN-CONTRACT-FOR-ACCUMS.                                   ELTACP  
00523      PERFORM WITH TEST BEFORE                                     ELTACP  
00524         VARYING GCT-TAB-INDEX FROM 1 BY 1                         ELTACP  
00525           UNTIL GCT-TAB-INDEX > WS-MAX-GCT-INDEX                  ELTACP  
00526                 OR GCT-CON-TAB-ID (GCT-TAB-INDEX) > PC-ACP        ELTACP  
00527 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELTACP  
00528         IF     GCT-CON-TAB-ID (GCT-TAB-INDEX) = PC-ACP            ELTACP  
00529            AND GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  > ZEROS          ELTACP  
00530         THEN                                                      ELTACP  
00531 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELTACP  
00532            MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  TO  WS-SLOT-NBR ELTACP  
00533            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELTACP  
00534         END-IF                                                    ELTACP  
00535         END-PERFORM.                                              ELTACP  
00536                                                                   ELTACP  
00537 ************************************************************      ELTACP  
00538 *                                                          *      ELTACP  
00539 *    SAVE UNIQUE ACCUMULATOR SLOT NUMBER                   *      ELTACP  
00540 *                                                          *      ELTACP  
00541 ************************************************************      ELTACP  
00542                                                                   ELTACP  
00543  0200-SAVE-UNQ-ACCUM-SLOT-NBR.                                    ELTACP  
00544                                                                   ELTACP  
00545 * -- SCAN TABLE OF ACCUM SLOT NUMBERS FOR DUPLICATE               ELTACP  
00546      SET SW-UNQ-SLOT-NBR TO TRUE.                                 ELTACP  
00547      PERFORM WITH TEST BEFORE                                     ELTACP  
00548         VARYING WS-ACP-SUB FROM 1 BY 1                            ELTACP  
00549           UNTIL    WS-ACP-SUB > WS-ACP-ACCUM-CNT                  ELTACP  
00550                 OR SW-DUP-SLOT-NBR                                ELTACP  
00551         IF WS-SLOT-NBR = ACCUM-SLOT-NBR (WS-ACP-SUB)              ELTACP  
00552         THEN                                                      ELTACP  
00553            SET SW-DUP-SLOT-NBR TO TRUE                            ELTACP  
00554         END-IF                                                    ELTACP  
00555         END-PERFORM.                                              ELTACP  
00556                                                                   ELTACP  
00557 * -- IF SLOT NUMBER IS UNIQUE, ADD IT TO THE HOLD TABLE           ELTACP  
00558      IF SW-UNQ-SLOT-NBR                                           ELTACP  
00559      THEN                                                         ELTACP  
00560         ADD 1 TO  WS-ACP-ACCUM-CNT                                ELTACP  
00561         MOVE WS-SLOT-NBR TO ACCUM-SLOT-NBR(WS-ACP-ACCUM-CNT)      ELTACP  
00562      END-IF.                                                      ELTACP  
00563                                                                   ELTACP  
00564 /***********************************************************      ELTACP  
00565 *                                                          *      ELTACP  
00566 *    SCAN ACP ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELTACP  
00567 *                                                          *      ELTACP  
00568 ************************************************************      ELTACP  
00569                                                                   ELTACP  
00570  0210-SCAN-FOR-APPLIC-OCCRNCS.                                    ELTACP  
00571      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELTACP  
00572      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELTACP  
00573      PERFORM 0220-DELETE-ACP-SUMMARY-FILE.                        ELTACP  
00574      PERFORM 0230-ALLOC-WORKFILE-REC-AREA.                        ELTACP  
00575                                                                   ELTACP  
00576 * -- READ AND SCAN EACH ACCUMULATOR TABULAR                       ELTACP  
00577      SET GAF-INDEX TO 1.                                          ELTACP  
00578      PERFORM WITH TEST BEFORE                                     ELTACP  
00579         VARYING WS-ACP-SUB FROM 1 BY 1                            ELTACP  
00580           UNTIL WS-ACP-SUB > WS-ACP-ACCUM-CNT                     ELTACP  
00581 *    -- OBTAIN ACCUMULATOR TABULAR RECORD                         ELTACP  
00582         MOVE PC-ACP TO KWA-PROVISION-ID                           ELTACP  
00583         MOVE ACCUM-SLOT-NBR (WS-ACP-SUB) TO KWA-PROVISION-SLOT-NO ELTACP  
00584         PERFORM 0240-READ-TABULAR-REC                             ELTACP  
00585         MOVE SPACES TO WS-OCCURRENCE-PROCESSED-TBL                ELTACP  
00586 *    -- SCAN ACCUMULATOR TABULAR                                  ELTACP  
00587         PERFORM WITH TEST BEFORE                                  ELTACP  
00588            VARYING WS-OCCURRENCE-SUB FROM 1 BY 1                  ELTACP  
00589               UNTIL WS-OCCURRENCE-SUB >= GAF-ENTRY-COUNT          ELTACP  
00590               IF WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB)      ELTACP  
00591                  CONTINUE                                         ELTACP  
00592               ELSE                                                ELTACP  
00593                  SET GAF-INDEX TO WS-OCCURRENCE-SUB               ELTACP  
00594                  PERFORM 0300-TEST-ACP-OCCURRENCE                 ELTACP  
00595               END-IF                                              ELTACP  
00596         END-PERFORM                                               ELTACP  
00597      END-PERFORM.                                                 ELTACP  
00598                                                                   ELTACP  
00599 *       PERFORM WITH TEST BEFORE                                  ELTACP  
00600 *          VARYING WS-OCCURRENCE-SUB FROM 1 BY 1                  ELTACP  
00601 *          UNTIL WS-OCCURRENCE-SUB >= GAF-ENTRY-COUNT             ELTACP  
00602 *           IF WS-OCCURRENCE-NOT-PROCESSED (WS-OCCURRENCE-SUB)    ELTACP  
00603 *            SET WS-OCCURRENCE-INDEX                              ELTACP  
00604 *                         GAF-INDEX                               ELTACP  
00605 *             TO WS-OCCURRENCE-SUB                                ELTACP  
00606 *             PERFORM 0300-TEST-ACP-OCCURRENCE                    ELTACP  
00607 *           END-IF                                                ELTACP  
00608 *       END-PERFORM                                               ELTACP  
00609 *    END-PERFORM.                                                 ELTACP  
00610                                                                   ELTACP  
00611                                                                   ELTACP  
00612 ************************************************************      ELTACP  
00613 *                                                          *      ELTACP  
00614 *        DELETE ACP SUMMARY FILE                           *      ELTACP  
00615 *                                                          *      ELTACP  
00616 ************************************************************      ELTACP  
00617                                                                   ELTACP  
00618  0220-DELETE-ACP-SUMMARY-FILE.                                    ELTACP  
00619      SET IOP-DEL TO TRUE.                                         ELTACP  
00620      SET IOP-FCQ-NONE TO TRUE.                                    ELTACP  
00621      SET IOP-KVQ-NONE TO TRUE.                                    ELTACP  
00622      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTACP  
00623                                                                   ELTACP  
00624 /***********************************************************      ELTACP  
00625 *                                                          *      ELTACP  
00626 *    ALLOCATE WORKFILE RECORD AREA                         *      ELTACP  
00627 *                                                          *      ELTACP  
00628 ************************************************************      ELTACP  
00629                                                                   ELTACP  
00630  0230-ALLOC-WORKFILE-REC-AREA.                                    ELTACP  
00631      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELTACP  
00632      SET CIA-STG-GETMAIN TO TRUE.                                 ELTACP  
00633      SET IOP-GETMAIN-REC TO TRUE.                                 ELTACP  
00634      COMPUTE IOP-MAX-REC-LEN =                                    ELTACP  
00635              LENGTH OF ACCUM-FIXED-AREA                           ELTACP  
00636 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELTACP  
00637            + LENGTH OF ACCUM-VARIABLE-AREA                        ELTACP  
00638            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA.                 ELTACP  
00639 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELTACP  
00640 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELTACP  
00641                                                                   ELTACP  
00642      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTACP  
00643      IF IOP-REC-PTR = NULLS                                       ELTACP  
00644      THEN                                                         ELTACP  
00645         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTACP  
00646         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTACP  
00647      ELSE                                                         ELTACP  
00648         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELTACP  
00649      END-IF.                                                      ELTACP  
00650                                                                   ELTACP  
00651 ************************************************************      ELTACP  
00652 *                                                          *      ELTACP  
00653 *    READ TABULAR RECORD                                   *      ELTACP  
00654 *                                                          *      ELTACP  
00655 ************************************************************      ELTACP  
00656                                                                   ELTACP  
00657  0240-READ-TABULAR-REC.                                           ELTACP  
00658      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELTACP  
00659      SET IOP-RD TO TRUE.                                          ELTACP  
00660      SET IOP-FCQ-NONE TO TRUE.                                    ELTACP  
00661      SET IOP-KVQ-EQ TO TRUE.                                      ELTACP  
00662      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTACP  
00663      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTACP  
00664      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTACP  
00665      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTACP  
00666                                                                   ELTACP  
00667      EVALUATE TRUE                                                ELTACP  
00668        WHEN IOP-RC-OK                                             ELTACP  
00669           SET ADDRESS OF GAF-RECORD-AREA TO IOP-REC-PTR           ELTACP  
00670           SET IOP-REC-PTR TO NULLS                                ELTACP  
00671           SET GAF-INDEX   TO GAF-ENTRY-COUNT                      ELTACP  
00672           SET WS-MAX-GAF-INDEX TO GAF-INDEX                       ELTACP  
00673        WHEN IOP-RC-NOTFND                                         ELTACP  
00674           SET CIA-AB-NOTFND-GCTABULR TO TRUE                      ELTACP  
00675           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELTACP  
00676        WHEN OTHER                                                 ELTACP  
00677           SET CIA-AB-CRITIO TO TRUE                               ELTACP  
00678           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELTACP  
00679        END-EVALUATE.                                              ELTACP  
00680                                                                   ELTACP  
00681 /***********************************************************      ELTACP  
00682 *                                                          *      ELTACP  
00683 *        TEST ACP OCCURS                                   *      ELTACP  
00684 *                                                          *      ELTACP  
00685 ************************************************************      ELTACP  
00686                                                                   ELTACP  
00687  0300-TEST-ACP-OCCURRENCE.                                        ELTACP  
00688                                                                   ELTACP  
00689      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELTACP  
00690      MOVE GAF-COPAY-L-O-B (GAF-INDEX) TO WS-LOB-ACCUM-OCCRNC.     ELTACP  
00691      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELTACP  
00692      PERFORM 0320-SCAN-FOR-INTERNALS.                             ELTACP  
00693      IF SW-OCCRNC-APPLIES                                         ELTACP  
00694      THEN                                                         ELTACP  
00695 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELTACP  
00696         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELTACP  
00697         PERFORM 0340-INIT-ACCUM-EXTRACT                           ELTACP  
00698         PERFORM 0350-EXTRACT-ACCUM                                ELTACP  
00699         PERFORM 0490-CHK-EXTRACT-DATA-INTGRTY                     ELTACP  
00700         PERFORM 0375-VARIABLE-DATA                                ELTACP  
00701         IF GAF-COPAY-DEFINITION (GAF-INDEX) = 'AA' OR 'BB' OR     ELTACP  
00702            'FF'                                                   ELTACP  
00703           ADD 1 TO WS-OCCURRENCE-SUB                              ELTACP  
00704         ELSE                                                      ELTACP  
00705           PERFORM 0710-WRITE-EXTRACT-RECORD                       ELTACP  
00706           MOVE 1 TO WS-OCCURRENCE-SUB                             ELTACP  
00707         END-IF                                                    ELTACP  
00708      END-IF.                                                      ELTACP  
00709                                                                   ELTACP  
00710 ************************************************************      ELTACP  
00711 *                                                          *      ELTACP  
00712 *        INITIALIZE OCCURRENCE                             *      ELTACP  
00713 *                                                          *      ELTACP  
00714 ************************************************************      ELTACP  
00715                                                                   ELTACP  
00716  0310-INITIALIZE-OCCURRENCE.                                      ELTACP  
00717      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELTACP  
00718      SET SW-HAS-NO-IBGR                                           ELTACP  
00719          SW-HAS-NO-IDGD                                           ELTACP  
00720          SW-HAS-NO-IPGN                                           ELTACP  
00721          SW-HAS-NO-IPGP                                           ELTACP  
00722          SW-HAS-NO-IPGT                                           ELTACP  
00723          SW-HAS-NO-IPGS                                           ELTACP  
00724       TO TRUE.                                                    ELTACP  
00725      INITIALIZE WS-IBGR-SLOT-NBR                                  ELTACP  
00726                 WS-IDGD-SLOT-NBR                                  ELTACP  
00727                 WS-IPGN-SLOT-NBR                                  ELTACP  
00728                 WS-IPGP-SLOT-NBR                                  ELTACP  
00729                 WS-IPGT-SLOT-NBR                                  ELTACP  
00730                 WS-IPGS-SLOT-NBR.                                 ELTACP  
00731      SET SW-INTRNL-INST-PROV-CL-NOT-DET                           ELTACP  
00732          SW-INTRNL-PROF-PROV-CL-NOT-DET                           ELTACP  
00733          SW-INTRNL-PROF-PROV-SP-NOT-DET                           ELTACP  
00734       TO TRUE.                                                    ELTACP  
00735                                                                   ELTACP  
00736 /***********************************************************      ELTACP  
00737 *                                                          *      ELTACP  
00738 *        SCAN FOR INTERNAL TABULARS                        *      ELTACP  
00739 *                                                          *      ELTACP  
00740 ************************************************************      ELTACP  
00741                                                                   ELTACP  
00742  0320-SCAN-FOR-INTERNALS.                                         ELTACP  
00743 *    (THIS IS DONE NOW IN CASE IPGT OR IBGR IS NEEDED TO DETERMINEELTACP  
00744 *     WHETHER OCCURRENCE IS INSTITUTIONAL OR PROFESSIONAL.)       ELTACP  
00745      PERFORM 0330-SCAN-THE-INTERNAL-TABULAR                       ELTACP  
00746         VARYING GAF-INT-INDEX FROM 1 BY 1                         ELTACP  
00747           UNTIL    GAF-INT-INDEX                                  ELTACP  
00748                 >= GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX).        ELTACP  
00749                                                                   ELTACP  
00750      EVALUATE TRUE ALSO TRUE                                      ELTACP  
00751         WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                        ELTACP  
00752            SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE                  ELTACP  
00753            SET SW-OCCRNC-APPLIES TO TRUE                          ELTACP  
00754         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH                 ELTACP  
00755            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELTACP  
00756            PERFORM 0550-CHK-INTRNL-TAB-PROV-CL                    ELTACP  
00757         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST                 ELTACP  
00758            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELTACP  
00759            SET SW-OCCRNC-APPLIES TO TRUE                          ELTACP  
00760         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH                 ELTACP  
00761            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELTACP  
00762            PERFORM 0550-CHK-INTRNL-TAB-PROV-CL                    ELTACP  
00763            PERFORM 0551-CHK-INTRNL-TAB-PROV-SP                    ELTACP  
00764         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF                 ELTACP  
00765            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELTACP  
00766            SET SW-OCCRNC-APPLIES TO TRUE                          ELTACP  
00767         WHEN OTHER                                                ELTACP  
00768            CONTINUE                                               ELTACP  
00769         END-EVALUATE.                                             ELTACP  
00770                                                                   ELTACP  
00771 /***********************************************************      ELTACP  
00772 *                                                          *      ELTACP  
00773 *        SCAN THE INTERNAL TABULARS                        *      ELTACP  
00774 *                                                          *      ELTACP  
00775 ************************************************************      ELTACP  
00776                                                                   ELTACP  
00777  0330-SCAN-THE-INTERNAL-TABULAR.                                  ELTACP  
00778      IF GAF-INT-SLOT (GAF-INDEX, GAF-INT-INDEX) > 0               ELTACP  
00779      THEN                                                         ELTACP  
00780         MOVE GAF-INT-SLOT (GAF-INDEX, GAF-INT-INDEX)              ELTACP  
00781           TO WS-SLOT-NBR                                          ELTACP  
00782         EVALUATE GAF-INT-ID (GAF-INDEX, GAF-INT-INDEX)            ELTACP  
00783            WHEN PC-IBGR                                           ELTACP  
00784               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELTACP  
00785               SET SW-HAS-IBGR                                     ELTACP  
00786                TO TRUE                                            ELTACP  
00787            WHEN PC-IDGD                                           ELTACP  
00788               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELTACP  
00789               SET SW-HAS-IDGD                                     ELTACP  
00790                TO TRUE                                            ELTACP  
00791            WHEN PC-IPGP                                           ELTACP  
00792               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELTACP  
00793               SET SW-HAS-IPGP                                     ELTACP  
00794                TO TRUE                                            ELTACP  
00795            WHEN PC-IPGN                                           ELTACP  
00796               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELTACP  
00797               SET SW-HAS-IPGN                                     ELTACP  
00798                TO TRUE                                            ELTACP  
00799            WHEN PC-IPGT                                           ELTACP  
00800               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELTACP  
00801               SET SW-HAS-IPGT                                     ELTACP  
00802                TO TRUE                                            ELTACP  
00803            WHEN PC-IPGS                                           ELTACP  
00804               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELTACP  
00805               SET SW-HAS-IPGS                                     ELTACP  
00806                TO TRUE                                            ELTACP  
00807            WHEN OTHER                                             ELTACP  
00808               CONTINUE                                            ELTACP  
00809            END-EVALUATE                                           ELTACP  
00810      END-IF.                                                      ELTACP  
00811                                                                   ELTACP  
00812 ************************************************************      ELTACP  
00813 *                                                          *      ELTACP  
00814 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELTACP  
00815 *                                                          *      ELTACP  
00816 ************************************************************      ELTACP  
00817                                                                   ELTACP  
00818  0340-INIT-ACCUM-EXTRACT.                                         ELTACP  
00819      INITIALIZE ACCUM-COPAY-COUNT.                                ELTACP  
00820      INITIALIZE ACCUM-FIXED-AREA.                                 ELTACP  
00821      SET ACCUM-ACP TO TRUE.                                       ELTACP  
00822 *    MOVE +1 TO  ACCUM-COPAY-COUNT.                               ELTACP  
00823 *    SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELTACP  
00824      SET  COPAY-INDEX TO ACCUM-COPAY-COUNT.                       ELTACP  
00825      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELTACP  
00826 *    INITIALIZE ACCUM-COPAY-VARIABLE-AREA.                        ELTACP  
00827 *    INITIALIZE ACCUM-COPAY-ENTRY (1).                            ELTACP  
00828      MOVE +1 TO  ACCUM-ASCEND-DESCEND-COUNT.                      ELTACP  
00829                                                                   ELTACP  
00830 /***********************************************************      ELTACP  
00831 *                                                          *      ELTACP  
00832 *        SUMMARIZE ACP TOPIC LEVEL DATA ELEMENTS           *      ELTACP  
00833 *                                                          *      ELTACP  
00834 ************************************************************      ELTACP  
00835                                                                   ELTACP  
00836  0350-EXTRACT-ACCUM.                                              ELTACP  
00837                                                                   ELTACP  
00838 * -- SET FIXED PORTION DATA ELEMENTS                              ELTACP  
00839      MOVE GAF-COPAY-MANDATORY-IND (GAF-INDEX)                     ELTACP  
00840        TO ACCUM-LMT-MANDATORY-IND.                                ELTACP  
00841 *    MOVE GAF-COPAY-BENEFIT-PERIOD (GAF-INDEX)                    ELTACP  
00842 *      TO ACCUM-BENEFIT-PERIOD.                                   ELTACP  
00843      MOVE GAF-COPAY-FAM-OR-INDIV (GAF-INDEX)                      ELTACP  
00844        TO ACCUM-FAM-OR-INDIV.                                     ELTACP  
00845      MOVE GAF-COPAY-L-O-B (GAF-INDEX)                             ELTACP  
00846        TO ACCUM-L-O-B.                                            ELTACP  
00847 *    MOVE GAF-COPAY-DEFINITION (GAF-INDEX)                        ELTACP  
00848 *      TO ACCUM-DEFINITION.                                       ELTACP  
00849      MOVE GAF-COPAY-DAY-FACTOR-IND (GAF-INDEX)                    ELTACP  
00850        TO ACCUM-DAY-FACTOR-IND.                                   ELTACP  
00851      MOVE GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX)               ELTACP  
00852           TO ACCUM-INTERNAL-DESCRIPTOR.                           ELTACP  
00853      MOVE GAF-COPAY-SERVICE-GROUP (GAF-INDEX)                     ELTACP  
00854           TO ACCUM-SERVICE-GROUP.                                 ELTACP  
00855      MOVE GAF-COPAY-PLACE-OF-TREATMENT (GAF-INDEX)                ELTACP  
00856        TO ACCUM-PLACE-OF-TREATMENT.                               ELTACP  
00857      MOVE GAF-COPAY-CONDITION (GAF-INDEX)                         ELTACP  
00858        TO ACCUM-CONDITION.                                        ELTACP  
00859      MOVE GAF-COPAY-CLAIM-LVL-ACCUM-IND (GAF-INDEX)               ELTACP  
00860        TO ACCUM-CLAIM-LVL-ACCUM-IND.                              ELTACP  
00861 *    MOVE GAF-COPAY-CO-PAY-IND (GAF-INDEX)                        ELTACP  
00862 *      TO ACCUM-CO-PAY-IND.                                       ELTACP  
00863      MOVE GAF-COPAY-COST-CONTAIN-IND (GAF-INDEX)                  ELTACP  
00864        TO ACCUM-COST-CONTAIN-IND.                                 ELTACP  
00865      MOVE GAF-COPAY-BEN-PER-TIME-QUAL (GAF-INDEX)                 ELTACP  
00866        TO ACCUM-BEN-PER-TIME-QUAL.                                ELTACP  
00867      MOVE GAF-COPAY-BEN-PER-TIME-FCTR (GAF-INDEX)                 ELTACP  
00868        TO ACCUM-BEN-PER-TIME-FCTR.                                ELTACP  
00869      MOVE GAF-COPAY-INTERVAL-TYPE (GAF-INDEX)                     ELTACP  
00870        TO ACCUM-INTERVAL-TYPE.                                    ELTACP  
00871      MOVE GAF-COPAY-INTERVAL-TIME-FCTR (GAF-INDEX)                ELTACP  
00872        TO ACCUM-INTERVAL-TIME-FCTR.                               ELTACP  
00873      MOVE GAF-COPAY-INTERVAL-OVRD-IND (GAF-INDEX)                 ELTACP  
00874        TO ACCUM-INTERVAL-OVRD-IND.                                ELTACP  
00875      MOVE GAF-COPAY-INTERVAL-OVRD-VALUE (GAF-INDEX)               ELTACP  
00876        TO ACCUM-INTERVAL-OVRD-VALUE.                              ELTACP  
00877      MOVE GAF-COPAY-FYI-VALUE (GAF-INDEX)                         ELTACP  
00878        TO ACCUM-FYI-VALUE.                                        ELTACP  
00879      MOVE GCG-DED-BASE-AMT-SOURCE-IND                             ELTACP  
00880        TO ACCUM-DED-BASE-AMT-SOURCE-IND.                          ELTACP  
00881 *    MOVE GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX)                   ELTACP  
00882 *      TO ACCUM-VALUE-QUALIFIER.                                  ELTACP  
00883      MOVE GAF-COPAY-RELATIONSHIP-IND (GAF-INDEX)                  ELTACP  
00884        TO ACCUM-RELATIONSHIP-IND.                                 ELTACP  
00885      MOVE GAF-COPAY-AGE-LIMIT-FROM (GAF-INDEX)                    ELTACP  
00886        TO ACCUM-AGE-LIMIT-FROM-VAL.                               ELTACP  
00887      MOVE GAF-COPAY-AGE-LIMIT-TO (GAF-INDEX)                      ELTACP  
00888        TO ACCUM-AGE-LIMIT-TO-VAL.                                 ELTACP  
00889      MOVE GAF-COPAY-AGE-QUAL-IND-FROM (GAF-INDEX)                 ELTACP  
00890        TO ACCUM-AGE-LIMIT-FROM-IND.                               ELTACP  
00891      MOVE GAF-COPAY-AGE-QUAL-IND-TO (GAF-INDEX)                   ELTACP  
00892        TO ACCUM-AGE-LIMIT-TO-IND.                                 ELTACP  
00893 *    MOVE GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)                   ELTACP  
00894 *     TO ACCUM-COPAY-TIME-DOLLAR-IND.                             ELTACP  
00895      SET ASCEND-DESCEND-IND-NA                                    ELTACP  
00896          1ST-DOLR-COVRGE-LMT-NA                                   ELTACP  
00897          CARRY-OVER-CREDIT-IND-NA                                 ELTACP  
00898          BEN-PER-MAX-OVRD-IND-NA                                  ELTACP  
00899          DEFINITION-NA                                            ELTACP  
00900          BENEFIT-PERIOD-NA                                        ELTACP  
00901          VALUE-QUALIFIER-NA TO TRUE.                              ELTACP  
00902      MOVE ZEROS TO ACCUM-MAX-BASE-AMT-SOURCE-IND.                 ELTACP  
00903      MOVE ZEROS TO ACCUM-OPX-BASE-AMT-SOURCE-IND.                 ELTACP  
00904                                                                   ELTACP  
00905 * -- SET OCCURRENCE PROVIDER CLASS INFORMATION                    ELTACP  
00906      EVALUATE TRUE ALSO TRUE                                      ELTACP  
00907         WHEN    SW-INTRNL-INST-PROV-CL                            ELTACP  
00908            ALSO SW-INTRNL-NOT-PROF-PROV-CL                        ELTACP  
00909               SET ACCUM-PRVDR-CLS-INST TO TRUE                    ELTACP  
00910         WHEN    SW-INTRNL-NOT-INST-PROV-CL                        ELTACP  
00911            ALSO SW-INTRNL-PROF-PROV-CL                            ELTACP  
00912               SET ACCUM-PRVDR-CLS-PROF TO TRUE                    ELTACP  
00913         WHEN    SW-INTRNL-PROF-PROV-SP                            ELTACP  
00914            ALSO SW-INTRNL-NOT-INST-PROV-SP                        ELTACP  
00915               SET ACCUM-PRVDR-SPC-PROF TO TRUE                    ELTACP  
00916         WHEN OTHER                                                ELTACP  
00917               SET ACCUM-PRVDR-CLS-ALL TO TRUE                     ELTACP  
00918               SET ACCUM-PRVDR-SPC-PROF TO TRUE                    ELTACP  
00919         END-EVALUATE.                                             ELTACP  
00920                                                                   ELTACP  
00921 * -- SET VARIABLE PORTION DATA ELEMENTS                           ELTACP  
00922 *    MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                       ELTACP  
00923 *      TO ACCUM-VALUE-LIMIT (1).                                  ELTACP  
00924 *    INITIALIZE ACCUM-VARIABLE-AREA.                              ELTACP  
00925      MOVE WS-IBGR-SLOT-NBR TO ACCUM-IBGR-SLOT-NBR (1).            ELTACP  
00926      MOVE WS-IDGD-SLOT-NBR TO ACCUM-IDGD-SLOT-NBR (1).            ELTACP  
00927      MOVE WS-IPGN-SLOT-NBR TO ACCUM-IPGN-SLOT-NBR (1).            ELTACP  
00928      MOVE WS-IPGP-SLOT-NBR TO ACCUM-IPGP-SLOT-NBR (1).            ELTACP  
00929      MOVE WS-IPGT-SLOT-NBR TO ACCUM-IPGT-SLOT-NBR (1).            ELTACP  
00930      MOVE WS-IPGS-SLOT-NBR TO ACCUM-IPGS-SLOT-NBR (1).            ELTACP  
00931                                                                   ELTACP  
00932 /***********************************************************      ELTACP  
00933 *                                                          *      ELTACP  
00934 *    LOAD VARIABLE DATA FOR RELATED PAIRS                  *      ELTACP  
00935 *                                                          *      ELTACP  
00936 *COPAY INDEX IS RESET WHERE THER ARE NO PAIRS, IT IS NOT   *      ELTACP  
00937 * INCREMENTED.                                             *      ELTACP  
00938 ************************************************************      ELTACP  
00939  0375-VARIABLE-DATA.                                              ELTACP  
00940 *     MOVE GAF-ENTRY-COUNT TO ACCUM-COPAY-COUNT                   ELTACP  
00941       IF FIRST-ACP                                                ELTACP  
00942          SET COPAY-INDEX TO 1                                     ELTACP  
00943          SET NOT-FIRST-ACP TO TRUE                                ELTACP  
00944       END-IF.                                                     ELTACP  
00945       EVALUATE TRUE                                               ELTACP  
00946       WHEN GAF-COPAY-DEFINITION (GAF-INDEX) = '0C' OR '0D'        ELTACP  
00947          OR '0E'                                                  ELTACP  
00948            SET COPAY-INDEX TO 1                                   ELTACP  
00949            PERFORM 0480-LOAD-VARIABLE-FIELDS                      ELTACP  
00950 *          PERFORM 9900-SET-INDICES                               ELTACP  
00951            SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)         ELTACP  
00952              TO TRUE                                              ELTACP  
00953 *            ADD 1 TO WS-OCCURRENCE-SUB                           ELTACP  
00954       WHEN GAF-COPAY-DEFINITION (GAF-INDEX) = '00'                ELTACP  
00955            IF GAF-COPAY-TIME-DOLLAR-IND(GAF-INDEX) = '00'         ELTACP  
00956               SET COPAY-INDEX TO 1                                ELTACP  
00957               PERFORM 0480-LOAD-VARIABLE-FIELDS                   ELTACP  
00958               SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)      ELTACP  
00959                 TO TRUE                                           ELTACP  
00960 *            ADD 1 TO WS-OCCURRENCE-SUB                           ELTACP  
00961               SET WS-HOLD-IDX TO GAF-INDEX                        ELTACP  
00962 *             PERFORM 9900-SET-INDICES                            ELTACP  
00963            ELSE                                                   ELTACP  
00964               PERFORM 0480-LOAD-VARIABLE-FIELDS                   ELTACP  
00965               SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)      ELTACP  
00966                 TO TRUE                                           ELTACP  
00967 *             ADD 1 TO WS-OCCURRENCE-SUB                          ELTACP  
00968 *             PERFORM 9900-SET-INDICES                            ELTACP  
00969               PERFORM 0400-LOAD-CHAINED-OCCURS                    ELTACP  
00970            END-IF                                                 ELTACP  
00971       WHEN GAF-COPAY-DEFINITION (GAF-INDEX) = '0A' OR             ELTACP  
00972            '0B' OR '0F'                                           ELTACP  
00973            IF GAF-COPAY-TIME-DOLLAR-IND(GAF-INDEX) = '00'         ELTACP  
00974               PERFORM 0480-LOAD-VARIABLE-FIELDS                   ELTACP  
00975               SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)      ELTACP  
00976                 TO TRUE                                           ELTACP  
00977               ADD 1 TO WS-OCCURRENCE-SUB                          ELTACP  
00978 *             SET WS-HOLD-SUB TO GAF-INDEX                        ELTACP  
00979 *             PERFORM 9900-SET-INDICES                            ELTACP  
00980               MOVE GAF-COPAY-DEFINITION (GAF-INDEX)               ELTACP  
00981                  TO HOLD-DEFINITION                               ELTACP  
00982               PERFORM 0498-COMPLETE-PAIR                          ELTACP  
00983               SET GAF-INDEX TO WS-HOLD-SUB                        ELTACP  
00984            ELSE                                                   ELTACP  
00985               PERFORM 9999-CODING-ERROR                           ELTACP  
00986            END-IF                                                 ELTACP  
00987       WHEN OTHER                                                  ELTACP  
00988 *MAY BE AN ERROR ROUTINE                                          ELTACP  
00989             CONTINUE                                              ELTACP  
00990       END-EVALUATE.                                               ELTACP  
00991       INITIALIZE WS-PAIR-FOUND-SW.                                ELTACP  
00992                                                                   ELTACP  
00993 /***********************************************************      ELTACP  
00994 *                                                          *      ELTACP  
00995 *    LOAD CHAINED OCCURS                                   *      ELTACP  
00996 *                                                          *      ELTACP  
00997 ************************************************************      ELTACP  
00998  0400-LOAD-CHAINED-OCCURS.                                        ELTACP  
00999      SET GAF-INDEX UP BY 1.                                       ELTACP  
01000      INITIALIZE WS-STOP-SW.                                       ELTACP  
01001      PERFORM UNTIL WS-STOP                                        ELTACP  
01002 *       IF GAF-COPAY-TIME-DOLLAR-IND(GAF-INDEX) = 'A1'            ELTACP  
01003              ADD 1 TO WS-OCCURRENCE-SUB                           ELTACP  
01004              PERFORM 0480-LOAD-VARIABLE-FIELDS                    ELTACP  
01005              SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)       ELTACP  
01006                TO TRUE                                            ELTACP  
01007 *            PERFORM 9900-SET-INDICES                             ELTACP  
01008              SET GAF-INDEX UP BY 1                                ELTACP  
01009              PERFORM 0500-FIND-RELATED-PAIR                       ELTACP  
01010 *       END-IF                                                    ELTACP  
01011      END-PERFORM.                                                 ELTACP  
01012                                                                   ELTACP  
01013 /***********************************************************      ELTACP  
01014 *                                                          *      ELTACP  
01015 *    LOAD VARIABLE-FIELDS                                  *      ELTACP  
01016 *                                                          *      ELTACP  
01017 ************************************************************      ELTACP  
01018  0480-LOAD-VARIABLE-FIELDS.                                       ELTACP  
01019       MOVE GAF-COPAY-BENEFIT-PERIOD(GAF-INDEX) TO                 ELTACP  
01020          ACCUM-COPAY-BEN-PER(COPAY-INDEX).                        ELTACP  
01021       MOVE GAF-COPAY-DEFINITION(GAF-INDEX) TO                     ELTACP  
01022          ACCUM-COPAY-DEFINITION(COPAY-INDEX).                     ELTACP  
01023       MOVE GAF-COPAY-VALUE-QUALIFIER(GAF-INDEX) TO                ELTACP  
01024          ACCUM-COPAY-VALUE-QUALIFIER(COPAY-INDEX).                ELTACP  
01025       MOVE GAF-COPAY-VALUE-LIMIT(GAF-INDEX) TO                    ELTACP  
01026          ACCUM-COPAY-VALUE-LIMIT(COPAY-INDEX).                    ELTACP  
01027       MOVE GAF-COPAY-TIME-DOLLAR-IND(GAF-INDEX) TO                ELTACP  
01028          ACCUM-COPAY-TAD-IND(COPAY-INDEX).                        ELTACP  
01029       MOVE GAF-COPAY-CO-PAY-IND(GAF-INDEX) TO                     ELTACP  
01030            ACCUM-CO-PAY-IND(COPAY-INDEX).                         ELTACP  
01031       SET COPAY-INDEX UP BY 1.                                    ELTACP  
01032       ADD +1 TO ACCUM-COPAY-COUNT.                                ELTACP  
01033                                                                   ELTACP  
01034 /***********************************************************      ELTACP  
01035 *                                                          *      ELTACP  
01036 *    CHECK EXTRACT DATA INTEGRITY                          *      ELTACP  
01037 *                                                          *      ELTACP  
01038 ************************************************************      ELTACP  
01039                                                                   ELTACP  
01040  0490-CHK-EXTRACT-DATA-INTGRTY.                                   ELTACP  
01041      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELTACP  
01042         SET FYI-VALUE-NA TO TRUE.                                 ELTACP  
01043                                                                   ELTACP  
01044      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELTACP  
01045         SET COST-CONTAIN-IND-NA TO TRUE.                          ELTACP  
01046                                                                   ELTACP  
01047      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELTACP  
01048         SET PLACE-OF-TREATMENT-NA TO TRUE.                        ELTACP  
01049                                                                   ELTACP  
01050 *    IF ACCUM-BENEFIT-PERIOD = ZEROS OR SPACES OR LOW-VALUES      ELTACP  
01051 *       SET BENEFIT-PERIOD-NA TO TRUE.                            ELTACP  
01052                                                                   ELTACP  
01053      IF ACCUM-BEN-PER-TIME-QUAL = ZEROS OR SPACES OR LOW-VALUES   ELTACP  
01054         SET BEN-PER-TIME-QUAL-NA TO TRUE.                         ELTACP  
01055                                                                   ELTACP  
01056      IF ACCUM-INTERVAL-TYPE = ZEROS OR SPACES OR LOW-VALUES       ELTACP  
01057         SET INTERVAL-TYPE-NA TO TRUE.                             ELTACP  
01058                                                                   ELTACP  
01059      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELTACP  
01060         SET INTERVAL-OVRD-IND-NA TO TRUE.                         ELTACP  
01061                                                                   ELTACP  
01062      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELTACP  
01063         SET L-O-B-NA TO TRUE.                                     ELTACP  
01064                                                                   ELTACP  
01065      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELTACP  
01066         SET REINSTATEMENT-IND-NA TO TRUE.                         ELTACP  
01067                                                                   ELTACP  
01068 *    IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELTACP  
01069 *       SET DEFINITION-NA TO TRUE.                                ELTACP  
01070                                                                   ELTACP  
01071      IF ACCUM-CARRY-OVER-CREDIT-IND =                             ELTACP  
01072                          ZEROS OR SPACES OR LOW-VALUES            ELTACP  
01073         SET CARRY-OVER-CREDIT-IND-NA TO TRUE.                     ELTACP  
01074                                                                   ELTACP  
01075      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELTACP  
01076         SET ASCEND-DESCEND-IND-NA TO TRUE.                        ELTACP  
01077                                                                   ELTACP  
01078      IF ACCUM-FAM-OR-INDIV = ZEROS OR SPACES OR LOW-VALUES        ELTACP  
01079         SET FAM-OR-INDIV-NA TO TRUE.                              ELTACP  
01080                                                                   ELTACP  
01081      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELTACP  
01082         SET RELATIONSHIP-IND-NA TO TRUE.                          ELTACP  
01083                                                                   ELTACP  
01084      IF ACCUM-DED-BASE-AMT-SOURCE-IND =                           ELTACP  
01085                            ZEROS OR SPACES OR LOW-VALUES          ELTACP  
01086         SET DED-BASE-AMT-SOURCE-IND-NA TO TRUE.                   ELTACP  
01087                                                                   ELTACP  
01088 *    IF ACCUM-VALUE-QUALIFIER = ZEROS OR SPACES OR LOW-VALUES     ELTACP  
01089 *       SET VALUE-QUALIFIER-NA TO TRUE.                           ELTACP  
01090                                                                   ELTACP  
01091      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELTACP  
01092         SET AGE-LMT-TO-IND-NA TO TRUE.                            ELTACP  
01093                                                                   ELTACP  
01094      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELTACP  
01095         SET AGE-LMT-FROM-IND-NA TO TRUE.                          ELTACP  
01096                                                                   ELTACP  
01097      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELTACP  
01098         SET LMT-MANDATORY-IND-NA TO TRUE.                         ELTACP  
01099                                                                   ELTACP  
01100      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELTACP  
01101         SET DAY-FACTOR-IND-NA TO TRUE.                            ELTACP  
01102                                                                   ELTACP  
01103      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELTACP  
01104         SET CLAIM-LVL-ACCUM-IND-NA TO TRUE.                       ELTACP  
01105                                                                   ELTACP  
01106 *    IF ACCUM-CO-PAY-IND = ZEROS OR SPACES OR LOW-VALUES          ELTACP  
01107 *       SET CO-PAY-IND-NA TO TRUE.                                ELTACP  
01108                                                                   ELTACP  
01109      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELTACP  
01110         SET SERVICE-GROUP-NA TO TRUE.                             ELTACP  
01111                                                                   ELTACP  
01112      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELTACP  
01113         SET INTERNAL-DESCRIPTOR-NA TO TRUE.                       ELTACP  
01114                                                                   ELTACP  
01115 /***********************************************************      ELTACP  
01116 *                                                          *      ELTACP  
01117 *    COMPLETE  PAIR                                        *      ELTACP  
01118 * CHECK-VALUE OF INDEX WHEN COMING OUT OF THIS PARA        *      ELTACP  
01119 ************************************************************      ELTACP  
01120  0498-COMPLETE-PAIR.                                              ELTACP  
01121      IF HOLD-DEFINITION = '0A'                                    ELTACP  
01122         PERFORM WITH TEST AFTER                                   ELTACP  
01123               VARYING GAF-INDEX FROM 1 BY 1 UNTIL                 ELTACP  
01124                    WS-MATCH-FOUND                                 ELTACP  
01125 *        GAF-COPAY-DEFINITION (GAF-INDEX) = 'AA'                  ELTACP  
01126             IF GAF-COPAY-DEFINITION (GAF-INDEX) = 'AA'            ELTACP  
01127                PERFORM 0510-MATCH-REST-OF-OCCUR                   ELTACP  
01128                IF WS-MATCH-FOUND                                  ELTACP  
01129                   PERFORM 0480-LOAD-VARIABLE-FIELDS               ELTACP  
01130                END-IF                                             ELTACP  
01131             END-IF                                                ELTACP  
01132         END-PERFORM                                               ELTACP  
01133      END-IF.                                                      ELTACP  
01134      INITIALIZE WS-MATCH-SW.                                      ELTACP  
01135      IF HOLD-DEFINITION = '0B'                                    ELTACP  
01136         PERFORM VARYING GAF-INDEX FROM GAF-INDEX BY 1 UNTIL       ELTACP  
01137          GAF-COPAY-DEFINITION (GAF-INDEX) = 'BB'                  ELTACP  
01138             IF GAF-COPAY-DEFINITION (GAF-INDEX) = 'BB'            ELTACP  
01139                PERFORM 0510-MATCH-REST-OF-OCCUR                   ELTACP  
01140                IF WS-MATCH-FOUND                                  ELTACP  
01141                   PERFORM 0480-LOAD-VARIABLE-FIELDS               ELTACP  
01142                END-IF                                             ELTACP  
01143             END-IF                                                ELTACP  
01144         END-PERFORM                                               ELTACP  
01145      END-IF.                                                      ELTACP  
01146      IF HOLD-DEFINITION  = '0F'                                   ELTACP  
01147         PERFORM VARYING GAF-INDEX FROM GAF-INDEX BY 1 UNTIL       ELTACP  
01148          GAF-COPAY-DEFINITION (GAF-INDEX) = 'FF'                  ELTACP  
01149             IF GAF-COPAY-DEFINITION (GAF-INDEX) = 'FF'            ELTACP  
01150                PERFORM 0510-MATCH-REST-OF-OCCUR                   ELTACP  
01151                IF WS-MATCH-FOUND                                  ELTACP  
01152                   PERFORM 0480-LOAD-VARIABLE-FIELDS               ELTACP  
01153                END-IF                                             ELTACP  
01154             END-IF                                                ELTACP  
01155         END-PERFORM                                               ELTACP  
01156      END-IF.                                                      ELTACP  
01157      IF WS-MATCH-FOUND                                            ELTACP  
01158         ADD 1 TO WS-OCCURRENCE-SUB                                ELTACP  
01159         SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)            ELTACP  
01160             TO TRUE                                               ELTACP  
01161      END-IF.                                                      ELTACP  
01162 /***********************************************************      ELTACP  
01163 *                                                          *      ELTACP  
01164 *    FIND RELATED PAIR                                     *      ELTACP  
01165 *                                                          *      ELTACP  
01166 ************************************************************      ELTACP  
01167  0500-FIND-RELATED-PAIR.                                          ELTACP  
01168 *    PERFORM VARYING GAF-INDEX FROM GAF-INDEX BY 1                ELTACP  
01169      PERFORM WITH TEST AFTER VARYING GAF-INDEX FROM GAF-INDEX BY 1ELTACP  
01170         UNTIL (GAF-INDEX) > GAF-ENTRY-COUNT OR NOT WS-MATCH-FOUND ELTACP  
01171 *CHANGED FROM WS-HOLD-IDX TO - 1*****                             ELTACP  
01172            IF GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX - 1)       ELTACP  
01173            =  GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX)           ELTACP  
01174               INITIALIZE WS-MATCH-SW                              ELTACP  
01175               PERFORM 0510-MATCH-REST-OF-OCCUR                    ELTACP  
01176               IF WS-MATCH-FOUND                                   ELTACP  
01177                  SET WS-PAIR-FOUND TO TRUE                        ELTACP  
01178                  PERFORM 0480-LOAD-VARIABLE-FIELDS                ELTACP  
01179                 ADD 1 TO WS-OCCURRENCE-SUB                        ELTACP  
01180                  SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)   ELTACP  
01181                    TO TRUE                                        ELTACP  
01182 *               ADD 1 TO WS-OCCURRENCE-SUB                        ELTACP  
01183 *                SET WS-HOLD-IDX UP BY 1                          ELTACP  
01184 *                SET COPAY-INDEX UP BY 1                          ELTACP  
01185               ELSE                                                ELTACP  
01186                  SET WS-STOP TO TRUE                              ELTACP  
01187               END-IF                                              ELTACP  
01188            ELSE                                                   ELTACP  
01189               SET WS-STOP TO TRUE                                 ELTACP  
01190 *THIS ASSUMES THE ENTRIES WITH TAD INDICATORS ARE PAIRED BY       ELTACP  
01191 *THE SORT SO THEY ARE NEXT TO EACH OTHER.                         ELTACP  
01192            END-IF                                                 ELTACP  
01193      END-PERFORM.                                                 ELTACP  
01194                                                                   ELTACP  
01195 /***********************************************************      ELTACP  
01196 *                                                          *      ELTACP  
01197 *    MATCH REST OF PARIED OCCURANCE                        *      ELTACP  
01198 *                                                          *      ELTACP  
01199 ************************************************************      ELTACP  
01200  0510-MATCH-REST-OF-OCCUR.                                        ELTACP  
01201      IF GAF-COPAY-MANDATORY-IND (GAF-INDEX) =                     ELTACP  
01202         ACCUM-LMT-MANDATORY-IND                                   ELTACP  
01203 *       GAF-COPAY-MANDATORY-IND (WS-HOLD-IDX)                     ELTACP  
01204      AND                                                          ELTACP  
01205         GAF-COPAY-FAM-OR-INDIV  (GAF-INDEX) =                     ELTACP  
01206         ACCUM-FAM-OR-INDIV                                        ELTACP  
01207 *       GAF-COPAY-FAM-OR-INDIV  (WS-HOLD-IDX)                     ELTACP  
01208      AND                                                          ELTACP  
01209         GAF-COPAY-L-O-B (GAF-INDEX) =                             ELTACP  
01210         ACCUM-L-O-B                                               ELTACP  
01211 *       GAF-COPAY-L-O-B (GAF-INDEX - 1)                           ELTACP  
01212      AND                                                          ELTACP  
01213         GAF-COPAY-DAY-FACTOR-IND (GAF-INDEX) =                    ELTACP  
01214         ACCUM-DAY-FACTOR-IND                                      ELTACP  
01215 *       GAF-COPAY-DAY-FACTOR-IND (WS-HOLD-IDX)                    ELTACP  
01216      AND                                                          ELTACP  
01217        GAF-COPAY-SERVICE-GROUP (GAF-INDEX) =                      ELTACP  
01218        ACCUM-SERVICE-GROUP                                        ELTACP  
01219 *      GAF-COPAY-SERVICE-GROUP (WS-HOLD-IDX)                      ELTACP  
01220      AND                                                          ELTACP  
01221        GAF-COPAY-PLACE-OF-TREATMENT (GAF-INDEX) =                 ELTACP  
01222        ACCUM-PLACE-OF-TREATMENT                                   ELTACP  
01223 *      GAF-COPAY-PLACE-OF-TREATMENT (WS-HOLD-IDX)                 ELTACP  
01224      AND                                                          ELTACP  
01225        GAF-COPAY-CONDITION (GAF-INDEX) =                          ELTACP  
01226        ACCUM-CONDITION                                            ELTACP  
01227 *      GAF-COPAY-CONDITION (WS-HOLD-IDX)                          ELTACP  
01228      AND                                                          ELTACP  
01229        GAF-COPAY-CLAIM-LVL-ACCUM-IND (GAF-INDEX) =                ELTACP  
01230        ACCUM-CLAIM-LVL-ACCUM-IND                                  ELTACP  
01231 *      GAF-COPAY-CLAIM-LVL-ACCUM-IND (WS-HOLD-IDX)                ELTACP  
01232 *    AND                                                          ELTACP  
01233 *      GAF-COPAY-CO-PAY-IND  (GAF-INDEX) =                        ELTACP  
01234 *      ACCUM-CO-PAY-IND                                           ELTACP  
01235 *      GAF-COPAY-CO-PAY-IND  (WS-HOLD-IDX)                        ELTACP  
01236      AND                                                          ELTACP  
01237        GAF-COPAY-COST-CONTAIN-IND (GAF-INDEX) =                   ELTACP  
01238        ACCUM-COST-CONTAIN-IND                                     ELTACP  
01239 *      GAF-COPAY-COST-CONTAIN-IND (WS-HOLD-IDX)                   ELTACP  
01240           SET WS-MATCH-FOUND TO TRUE                              ELTACP  
01241      END-IF.                                                      ELTACP  
01242 *    AND                                                          ELTACP  
01243 *      GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX) =                   ELTACP  
01244 *      GAF-INTERNAL-TABULAR-COUNT (WS-HOLD-IDX)                   ELTACP  
01245 *    AND                                                          ELTACP  
01246 *      GAF-INTERNAL-TAB-SLOT (GAF-INDEX) =                        ELTACP  
01247 *      GAF-INTERNAL-TAB-SLOT (WS-HOLD-IDX)                        ELTACP  
01248 *    AND                                                          ELTACP  
01249 *      END-OF-REDEFINES (GAF-INDEX) =                             ELTACP  
01250 *      END-OF-REDEFINES (WS-HOLD-IDX)                             ELTACP  
01251 *         SET WS-MATCH-FOUND TO TRUE                              ELTACP  
01252 *    END-IF.                                                      ELTACP  
01253                                                                   ELTACP  
01254 /***********************************************************      ELTACP  
01255 *                                                          *      ELTACP  
01256 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER CLASS   *      ELTACP  
01257 *                                                          *      ELTACP  
01258 ************************************************************      ELTACP  
01259                                                                   ELTACP  
01260  0550-CHK-INTRNL-TAB-PROV-CL.                                     ELTACP  
01261      IF SW-HAS-IPGT                                               ELTACP  
01262      THEN                                                         ELTACP  
01263         PERFORM 0560-CHK-IPGT-PROV-CL                             ELTACP  
01264      ELSE                                                         ELTACP  
01265         IF SW-HAS-IBGR                                            ELTACP  
01266         THEN                                                      ELTACP  
01267            PERFORM 0640-CHK-IBGR-PROV-CL                          ELTACP  
01268         ELSE                                                      ELTACP  
01269            SET SW-OCCRNC-APPLIES TO TRUE                          ELTACP  
01270         END-IF                                                    ELTACP  
01271      END-IF.                                                      ELTACP  
01272                                                                   ELTACP  
01273 /***********************************************************      ELTACP  
01274 *                                                          *      ELTACP  
01275 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER SPEC    *      ELTACP  
01276 *                                                          *      ELTACP  
01277 ************************************************************      ELTACP  
01278                                                                   ELTACP  
01279  0551-CHK-INTRNL-TAB-PROV-SP.                                     ELTACP  
01280      IF SW-HAS-IPGT                                               ELTACP  
01281         PERFORM 0561-CHK-IPGS-PROV-SP                             ELTACP  
01282      ELSE                                                         ELTACP  
01283         CONTINUE                                                  ELTACP  
01284      END-IF.                                                      ELTACP  
01285 *       IF SW-HAS-IBGR                                            ELTACP  
01286 *       THEN                                                      ELTACP  
01287 *          PERFORM 0640-CHK-IBGR-PROV-CL                          ELTACP  
01288 *       ELSE                                                      ELTACP  
01289 *          SET SW-OCCRNC-APPLIES TO TRUE                          ELTACP  
01290 *       END-IF                                                    ELTACP  
01291 *    END-IF.                                                      ELTACP  
01292                                                                   ELTACP  
01293 ******************************************************************ELTACP  
01294 *                                                                *ELTACP  
01295 *    CHECK IPGT INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELTACP  
01296 *                                                                *ELTACP  
01297 ******************************************************************ELTACP  
01298                                                                   ELTACP  
01299  0560-CHK-IPGT-PROV-CL.                                           ELTACP  
01300      MOVE PC-IPGT TO KWA-PROVISION-ID.                            ELTACP  
01301      MOVE WS-IPGT-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTACP  
01302      PERFORM 0660-READ-INTRLN-TAB.                                ELTACP  
01303      SET ADDRESS OF GX3-RECORD-AREA TO IOP-REC-PTR.               ELTACP  
01304      SET IOP-REC-PTR                TO NULLS.                     ELTACP  
01305      SET GX3-INDEX                  TO GX3-ENTRY-COUNT.           ELTACP  
01306      SET WS-MAX-GX3-INDEX           TO GX3-INDEX.                 ELTACP  
01307                                                                   ELTACP  
01308      IF GX3-ID-ARGUMENT-INCLUDED                                  ELTACP  
01309      THEN                                                         ELTACP  
01310         PERFORM 0570-CHK-INCLD-TYPE-IPGT                          ELTACP  
01311      ELSE                                                         ELTACP  
01312          PERFORM 0600-CHK-EXCLD-TYPE-IPGT                         ELTACP  
01313      END-IF.                                                      ELTACP  
01314                                                                   ELTACP  
01315 ******************************************************************ELTACP  
01316 *                                                                *ELTACP  
01317 *    CHECK IPGS INTERNAL TABULAR TO DETERMINE PROVIDER SPEC      *ELTACP  
01318 *                                                                *ELTACP  
01319 ******************************************************************ELTACP  
01320                                                                   ELTACP  
01321  0561-CHK-IPGS-PROV-SP.                                           ELTACP  
01322      MOVE PC-IPGS TO KWA-PROVISION-ID.                            ELTACP  
01323      MOVE WS-IPGS-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTACP  
01324      PERFORM 0660-READ-INTRLN-TAB.                                ELTACP  
01325      SET ADDRESS OF GXS-RECORD-AREA TO IOP-REC-PTR.               ELTACP  
01326      SET IOP-REC-PTR                TO NULLS.                     ELTACP  
01327      SET GXS-INDEX                  TO GXS-ENTRY-COUNT.           ELTACP  
01328      SET WS-MAX-GXS-INDEX           TO GXS-INDEX.                 ELTACP  
01329                                                                   ELTACP  
01330      IF GXS-ID-ARGUMENT-INCLUDED                                  ELTACP  
01331         PERFORM 0571-CHK-INCLD-SPEC-IPGS                          ELTACP  
01332      ELSE                                                         ELTACP  
01333          PERFORM 0601-CHK-EXCLD-SPEC-IPGS                         ELTACP  
01334      END-IF.                                                      ELTACP  
01335                                                                   ELTACP  
01336 /***********************************************************      ELTACP  
01337 *                                                          *      ELTACP  
01338 *    CHECK INCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELTACP  
01339 *                                                          *      ELTACP  
01340 ************************************************************      ELTACP  
01341                                                                   ELTACP  
01342  0571-CHK-INCLD-SPEC-IPGS.                                        ELTACP  
01343      SET CFT9-IDX TO 1.                                           ELTACP  
01344      SET SW-INTRNL-NOT-PROF-PROV-SP                               ELTACP  
01345       TO TRUE.                                                    ELTACP  
01346      PERFORM 0581-TEST-IPGS-INCLD-ENTRIES                         ELTACP  
01347         VARYING GXS-INDEX  FROM 1 BY 1                            ELTACP  
01348           UNTIL    GXS-INDEX = WS-MAX-GXS-INDEX                   ELTACP  
01349                 OR (    SW-INTRNL-PROF-PROV-CL).                  ELTACP  
01350                                                                   ELTACP  
01351 /***********************************************************      ELTACP  
01352 *                                                          *      ELTACP  
01353 *    CHECK INCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELTACP  
01354 *                                                          *      ELTACP  
01355 ************************************************************      ELTACP  
01356                                                                   ELTACP  
01357  0570-CHK-INCLD-TYPE-IPGT.                                        ELTACP  
01358      SET CFT2-IDX TO 1.                                           ELTACP  
01359      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTACP  
01360          SW-INTRNL-NOT-PROF-PROV-CL                               ELTACP  
01361       TO TRUE.                                                    ELTACP  
01362      PERFORM 0580-TEST-IPGT-INCLD-ENTRIES                         ELTACP  
01363         VARYING GX3-INDEX  FROM 1 BY 1                            ELTACP  
01364           UNTIL    GX3-INDEX = WS-MAX-GX3-INDEX                   ELTACP  
01365                 OR (    SW-INTRNL-INST-PROV-CL                    ELTACP  
01366                     AND SW-INTRNL-PROF-PROV-CL ).                 ELTACP  
01367                                                                   ELTACP  
01368 ************************************************************      ELTACP  
01369 *                                                          *      ELTACP  
01370 *    TEST IPGT INCLUDE ENTRIES TO DETERMINE PROVIDER CLASS *      ELTACP  
01371 *                                                          *      ELTACP  
01372 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELTACP  
01373 *          CFT2 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELTACP  
01374 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELTACP  
01375 *          OF THE IPGT TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELTACP  
01376 *                                                          *      ELTACP  
01377 ************************************************************      ELTACP  
01378                                                                   ELTACP  
01379  0580-TEST-IPGT-INCLD-ENTRIES.                                    ELTACP  
01380      PERFORM WITH TEST BEFORE                                     ELTACP  
01381         UNTIL    SW-OCCRNC-APPLIES                                ELTACP  
01382               OR   CFT2-PT (CFT2-IDX)                             ELTACP  
01383                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELTACP  
01384               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELTACP  
01385         IF   GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELTACP  
01386            = CFT2-PT (CFT2-IDX)                                   ELTACP  
01387         THEN                                                      ELTACP  
01388 *    -- TEST PROVIDER CLASS                                       ELTACP  
01389            EVALUATE TRUE                                          ELTACP  
01390               WHEN CFT2-PT-INST (CFT2-IDX)                        ELTACP  
01391                  SET SW-INTRNL-INST-PROV-CL TO TRUE               ELTACP  
01392                  IF SRP-ACCUM-PROV-CLASS-INST                     ELTACP  
01393                  THEN                                             ELTACP  
01394                     SET SW-OCCRNC-APPLIES TO TRUE                 ELTACP  
01395                  END-IF                                           ELTACP  
01396               WHEN CFT2-PT-PROF (CFT2-IDX)                        ELTACP  
01397                  SET SW-INTRNL-PROF-PROV-CL TO TRUE               ELTACP  
01398                  IF SRP-ACCUM-PROV-CLASS-PROF                     ELTACP  
01399                  THEN                                             ELTACP  
01400                     SET SW-OCCRNC-APPLIES TO TRUE                 ELTACP  
01401                  END-IF                                           ELTACP  
01402               END-EVALUATE                                        ELTACP  
01403         ELSE                                                      ELTACP  
01404            CONTINUE                                               ELTACP  
01405         END-IF                                                    ELTACP  
01406 *    -- BUMP TO NEXT CFT2 TABLE ENTRY                             ELTACP  
01407         SET CFT2-IDX UP BY 1                                      ELTACP  
01408         END-PERFORM.                                              ELTACP  
01409                                                                   ELTACP  
01410 ************************************************************      ELTACP  
01411 *                                                          *      ELTACP  
01412 *    TEST IPGS INCLUDE ENTRIES TO DETERMINE PROVIDER SPEC  *      ELTACP  
01413 *                                                          *      ELTACP  
01414 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELTACP  
01415 *          CFT9 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELTACP  
01416 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELTACP  
01417 *          OF THE IPGS TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELTACP  
01418 *                                                          *      ELTACP  
01419 ************************************************************      ELTACP  
01420                                                                   ELTACP  
01421  0581-TEST-IPGS-INCLD-ENTRIES.                                    ELTACP  
01422      PERFORM WITH TEST BEFORE                                     ELTACP  
01423         UNTIL    SW-OCCRNC-APPLIES                                ELTACP  
01424               OR   CFT9-PT (CFT9-IDX)                             ELTACP  
01425                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELTACP  
01426               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELTACP  
01427         IF   GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELTACP  
01428            = CFT9-PT (CFT9-IDX)                                   ELTACP  
01429 *    -- TEST PROVIDER CLASS                                       ELTACP  
01430            EVALUATE TRUE                                          ELTACP  
01431               WHEN CFT9-PT-PROF (CFT9-IDX)                        ELTACP  
01432                  SET SW-INTRNL-PROF-PROV-SP TO TRUE               ELTACP  
01433                  IF SRP-ACCUM-PROV-SPEC-PROF                      ELTACP  
01434                     SET SW-OCCRNC-APPLIES TO TRUE                 ELTACP  
01435                  END-IF                                           ELTACP  
01436               END-EVALUATE                                        ELTACP  
01437         ELSE                                                      ELTACP  
01438            CONTINUE                                               ELTACP  
01439         END-IF                                                    ELTACP  
01440 *    -- BUMP TO NEXT CFT9 TABLE ENTRY                             ELTACP  
01441         SET CFT9-IDX UP BY 1                                      ELTACP  
01442         END-PERFORM.                                              ELTACP  
01443                                                                   ELTACP  
01444 /***********************************************************      ELTACP  
01445 *                                                          *      ELTACP  
01446 *    CHECK EXCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELTACP  
01447 *                                                          *      ELTACP  
01448 ************************************************************      ELTACP  
01449                                                                   ELTACP  
01450  0600-CHK-EXCLD-TYPE-IPGT.                                        ELTACP  
01451                                                                   ELTACP  
01452 * -- INITIALIZE CFT2 TABLE TO INCLUDE ALL PROVIDER TYPES          ELTACP  
01453      PERFORM WITH TEST BEFORE                                     ELTACP  
01454         VARYING CFT2-IDX FROM 1 BY 1                              ELTACP  
01455           UNTIL CFT2-IDX > CFT2-NBR-TBL-ENTRIES                   ELTACP  
01456         SET  CFT2-PT-INCLUDE (CFT2-IDX) TO TRUE                   ELTACP  
01457         END-PERFORM.                                              ELTACP  
01458                                                                   ELTACP  
01459 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGT                 ELTACP  
01460      SET  CFT2-IDX TO 1.                                          ELTACP  
01461      PERFORM 0610-TAG-EXCLD-IPGT-ENTRIES                          ELTACP  
01462         VARYING GX3-INDEX FROM 1 BY 1                             ELTACP  
01463           UNTIL GX3-INDEX = WS-MAX-GX3-INDEX.                     ELTACP  
01464                                                                   ELTACP  
01465 * -- CHECK CFT2 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELTACP  
01466      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTACP  
01467          SW-INTRNL-NOT-PROF-PROV-CL                               ELTACP  
01468       TO TRUE.                                                    ELTACP  
01469      PERFORM 0630-CHK-CFT2-NOT-EXCLD                              ELTACP  
01470         VARYING CFT2-IDX FROM 1 BY 1                              ELTACP  
01471           UNTIL    (    SW-INTRNL-INST-PROV-CL                    ELTACP  
01472                     AND SW-INTRNL-PROF-PROV-CL )                  ELTACP  
01473                 OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES.               ELTACP  
01474                                                                   ELTACP  
01475 /***********************************************************      ELTACP  
01476 *                                                          *      ELTACP  
01477 *    CHECK EXCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELTACP  
01478 *                                                          *      ELTACP  
01479 ************************************************************      ELTACP  
01480                                                                   ELTACP  
01481  0601-CHK-EXCLD-SPEC-IPGS.                                        ELTACP  
01482                                                                   ELTACP  
01483 * -- INITIALIZE CFT9 TABLE TO INCLUDE ALL PROVIDER SPEC           ELTACP  
01484      PERFORM WITH TEST BEFORE                                     ELTACP  
01485         VARYING CFT9-IDX FROM 1 BY 1                              ELTACP  
01486           UNTIL CFT9-IDX > CFT9-NBR-TBL-ENTRIES                   ELTACP  
01487         SET  CFT9-PT-INCLUDE (CFT9-IDX) TO TRUE                   ELTACP  
01488         END-PERFORM.                                              ELTACP  
01489                                                                   ELTACP  
01490 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGS                 ELTACP  
01491      SET  CFT9-IDX TO 1.                                          ELTACP  
01492      PERFORM 0611-TAG-EXCLD-IPGS-ENTRIES                          ELTACP  
01493         VARYING GXS-INDEX FROM 1 BY 1                             ELTACP  
01494           UNTIL GXS-INDEX = WS-MAX-GXS-INDEX.                     ELTACP  
01495                                                                   ELTACP  
01496 * -- CHECK CFT9 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELTACP  
01497      SET SW-INTRNL-NOT-PROF-PROV-SP                               ELTACP  
01498       TO TRUE.                                                    ELTACP  
01499      PERFORM 0631-CHK-CFT9-NOT-EXCLD                              ELTACP  
01500         VARYING CFT9-IDX FROM 1 BY 1                              ELTACP  
01501           UNTIL    (    SW-INTRNL-PROF-PROV-SP )                  ELTACP  
01502                 OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES.               ELTACP  
01503                                                                   ELTACP  
01504 ************************************************************      ELTACP  
01505 *                                                          *      ELTACP  
01506 *    TAG EXCLUDED IPGT ENTRIES IN CFT2                     *      ELTACP  
01507 *                                                          *      ELTACP  
01508 ************************************************************      ELTACP  
01509                                                                   ELTACP  
01510  0610-TAG-EXCLD-IPGT-ENTRIES.                                     ELTACP  
01511      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELTACP  
01512      PERFORM WITH TEST BEFORE                                     ELTACP  
01513         UNTIL    SW-ENTRY-FOUND                                   ELTACP  
01514               OR   CFT2-PT (CFT2-IDX)                             ELTACP  
01515                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELTACP  
01516               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELTACP  
01517         IF   CFT2-PT(CFT2-IDX)                                    ELTACP  
01518            = GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELTACP  
01519         THEN                                                      ELTACP  
01520            SET SW-ENTRY-FOUND TO TRUE                             ELTACP  
01521            SET CFT2-PT-EXCLUDE (CFT2-IDX) TO TRUE                 ELTACP  
01522            SET CFT2-IDX UP BY 1                                   ELTACP  
01523         ELSE                                                      ELTACP  
01524            SET CFT2-IDX UP BY 1                                   ELTACP  
01525         END-IF                                                    ELTACP  
01526         END-PERFORM.                                              ELTACP  
01527                                                                   ELTACP  
01528 ************************************************************      ELTACP  
01529 *                                                          *      ELTACP  
01530 *    TAG EXCLUDED IPGS ENTRIES IN CFT9                     *      ELTACP  
01531 *                                                          *      ELTACP  
01532 ************************************************************      ELTACP  
01533                                                                   ELTACP  
01534  0611-TAG-EXCLD-IPGS-ENTRIES.                                     ELTACP  
01535      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELTACP  
01536      PERFORM WITH TEST BEFORE                                     ELTACP  
01537         UNTIL    SW-ENTRY-FOUND                                   ELTACP  
01538               OR   CFT9-PT (CFT9-IDX)                             ELTACP  
01539                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELTACP  
01540               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELTACP  
01541         IF   CFT9-PT(CFT9-IDX)                                    ELTACP  
01542            = GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELTACP  
01543            SET SW-ENTRY-FOUND TO TRUE                             ELTACP  
01544            SET CFT9-PT-EXCLUDE (CFT9-IDX) TO TRUE                 ELTACP  
01545            SET CFT9-IDX UP BY 1                                   ELTACP  
01546         ELSE                                                      ELTACP  
01547            SET CFT9-IDX UP BY 1                                   ELTACP  
01548         END-IF                                                    ELTACP  
01549         END-PERFORM.                                              ELTACP  
01550                                                                   ELTACP  
01551 /*****************************************************************ELTACP  
01552 *                                                                *ELTACP  
01553 *    CHECK CFT2 FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDED     *ELTACP  
01554 *                                                                *ELTACP  
01555 ******************************************************************ELTACP  
01556                                                                   ELTACP  
01557  0630-CHK-CFT2-NOT-EXCLD.                                         ELTACP  
01558      IF CFT2-PT-INCLUDE (CFT2-IDX)                                ELTACP  
01559      THEN                                                         ELTACP  
01560         EVALUATE TRUE                                             ELTACP  
01561            WHEN CFT2-PT-INST (CFT2-IDX)                           ELTACP  
01562               SET SW-INTRNL-INST-PROV-CL TO TRUE                  ELTACP  
01563               IF SRP-ACCUM-PROV-CLASS-INST                        ELTACP  
01564               THEN                                                ELTACP  
01565                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTACP  
01566               END-IF                                              ELTACP  
01567            WHEN CFT2-PT-PROF (CFT2-IDX)                           ELTACP  
01568               SET SW-INTRNL-PROF-PROV-CL TO TRUE                  ELTACP  
01569               IF SRP-ACCUM-PROV-CLASS-PROF                        ELTACP  
01570               THEN                                                ELTACP  
01571                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTACP  
01572               END-IF                                              ELTACP  
01573            END-EVALUATE                                           ELTACP  
01574      END-IF.                                                      ELTACP  
01575                                                                   ELTACP  
01576 /*****************************************************************ELTACP  
01577 *                                                                *ELTACP  
01578 *    CHECK CFT9 FOR CLASS(ES) OF PROVIDER SPEC  NOT EXCLUDED     *ELTACP  
01579 *                                                                *ELTACP  
01580 ******************************************************************ELTACP  
01581                                                                   ELTACP  
01582  0631-CHK-CFT9-NOT-EXCLD.                                         ELTACP  
01583      IF CFT9-PT-INCLUDE (CFT9-IDX)                                ELTACP  
01584         EVALUATE TRUE                                             ELTACP  
01585            WHEN CFT9-PT-PROF (CFT9-IDX)                           ELTACP  
01586               SET SW-INTRNL-PROF-PROV-SP TO TRUE                  ELTACP  
01587               IF SRP-ACCUM-PROV-SPEC-PROF                         ELTACP  
01588                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTACP  
01589               END-IF                                              ELTACP  
01590            END-EVALUATE                                           ELTACP  
01591      END-IF.                                                      ELTACP  
01592                                                                   ELTACP  
01593 ******************************************************************ELTACP  
01594 *                                                                *ELTACP  
01595 *    CHECK IBGR INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELTACP  
01596 *                                                                *ELTACP  
01597 ******************************************************************ELTACP  
01598                                                                   ELTACP  
01599  0640-CHK-IBGR-PROV-CL.                                           ELTACP  
01600      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTACP  
01601          SW-INTRNL-NOT-PROF-PROV-CL TO TRUE.                      ELTACP  
01602      MOVE PC-IBGR TO KWA-PROVISION-ID.                            ELTACP  
01603      MOVE WS-IBGR-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTACP  
01604      PERFORM 0660-READ-INTRLN-TAB.                                ELTACP  
01605      SET ADDRESS OF GX1-RECORD-AREA TO IOP-REC-PTR.               ELTACP  
01606      SET IOP-REC-PTR                TO NULLS.                     ELTACP  
01607      SET GX1-INDEX                  TO GX1-ENTRY-COUNT.           ELTACP  
01608      SET WS-MAX-GX1-INDEX           TO GX1-INDEX.                 ELTACP  
01609                                                                   ELTACP  
01610      IF GX1-ID-ARGUMENT-EXCLUDED                                  ELTACP  
01611      THEN                                                         ELTACP  
01612 *    -- ASSUME THAT IBGR WOULD NOT EXCLUDE ALL OF ANY PROVIDER    ELTACP  
01613 *       CLASS (I.E., BOTH TYPES APPLY).                           ELTACP  
01614         SET SW-OCCRNC-APPLIES                                     ELTACP  
01615             SW-INTRNL-INST-PROV-CL                                ELTACP  
01616             SW-INTRNL-PROF-PROV-CL                                ELTACP  
01617          TO TRUE                                                  ELTACP  
01618      ELSE                                                         ELTACP  
01619         PERFORM 0650-CHK-INCLD-TYPE-IBGR                          ELTACP  
01620      END-IF.                                                      ELTACP  
01621                                                                   ELTACP  
01622 /*****************************************************************ELTACP  
01623 *                                                                *ELTACP  
01624 *    CHECK INCLUDE TYPE IBGR TO DETERMINE PROVIDER CLASS         *ELTACP  
01625 *                                                                *ELTACP  
01626 ******************************************************************ELTACP  
01627                                                                   ELTACP  
01628  0650-CHK-INCLD-TYPE-IBGR.                                        ELTACP  
01629      PERFORM WITH TEST BEFORE                                     ELTACP  
01630         VARYING GX1-INDEX FROM 1 BY 1                             ELTACP  
01631           UNTIL    GX1-INDEX = WS-MAX-GX1-INDEX                   ELTACP  
01632                 OR (    SW-INTRNL-INST-PROV-CL                    ELTACP  
01633                     AND SW-INTRNL-PROF-PROV-CL )                  ELTACP  
01634         MOVE GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)                ELTACP  
01635           TO WS-PROVISION-ARGUMENT                                ELTACP  
01636         EVALUATE TRUE                                             ELTACP  
01637            WHEN INST-CL                                           ELTACP  
01638               SET SW-INTRNL-INST-PROV-CL TO TRUE                  ELTACP  
01639               IF SRP-ACCUM-PROV-CLASS-INST                        ELTACP  
01640               THEN                                                ELTACP  
01641                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTACP  
01642               END-IF                                              ELTACP  
01643            WHEN PROF-CL                                           ELTACP  
01644               SET SW-INTRNL-PROF-PROV-CL TO TRUE                  ELTACP  
01645               IF SRP-ACCUM-PROV-CLASS-PROF                        ELTACP  
01646               THEN                                                ELTACP  
01647                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTACP  
01648               END-IF                                              ELTACP  
01649            END-EVALUATE                                           ELTACP  
01650         END-PERFORM.                                              ELTACP  
01651                                                                   ELTACP  
01652 /***********************************************************      ELTACP  
01653 *                                                          *      ELTACP  
01654 *    READ THE INTERNAL TABULAR RECORD                      *      ELTACP  
01655 *                                                          *      ELTACP  
01656 ************************************************************      ELTACP  
01657                                                                   ELTACP  
01658  0660-READ-INTRLN-TAB.                                            ELTACP  
01659      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELTACP  
01660      SET IOP-RD TO TRUE.                                          ELTACP  
01661      SET IOP-FCQ-NONE TO TRUE.                                    ELTACP  
01662      SET IOP-KVQ-EQ TO TRUE.                                      ELTACP  
01663      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELTACP  
01664      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTACP  
01665      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTACP  
01666      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTACP  
01667                                                                   ELTACP  
01668      EVALUATE TRUE                                                ELTACP  
01669         WHEN IOP-RC-OK                                            ELTACP  
01670            CONTINUE                                               ELTACP  
01671         WHEN IOP-RC-NOTFND                                        ELTACP  
01672            SET CIA-AB-NOTFND-GCTABULR TO TRUE                     ELTACP  
01673            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELTACP  
01674         WHEN OTHER                                                ELTACP  
01675             SET CIA-AB-CRITIO TO TRUE                             ELTACP  
01676             EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC           ELTACP  
01677         END-EVALUATE.                                             ELTACP  
01678                                                                   ELTACP  
01679 ************************************************************      ELTACP  
01680 *                                                          *      ELTACP  
01681 *        ADD ACCUM OCCURRENCE TO FILE                      *      ELTACP  
01682 *                                                          *      ELTACP  
01683 ************************************************************      ELTACP  
01684                                                                   ELTACP  
01685  0710-WRITE-EXTRACT-RECORD.                                       ELTACP  
01686      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELTACP  
01687      SET  IOP-ADD TO TRUE.                                        ELTACP  
01688      SET  IOP-FCQ-NONE TO TRUE.                                   ELTACP  
01689      SET  IOP-KVQ-NONE TO TRUE.                                   ELTACP  
01690      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTACP  
01691                                                                   ELTACP  
01692 /***********************************************************      ELTACP  
01693 *                                                          *      ELTACP  
01694 *    ESTABLISH ADDRESSABILITY OF THE TABULAR FILE          *      ELTACP  
01695 *                                                          *      ELTACP  
01696 ************************************************************      ELTACP  
01697                                                                   ELTACP  
01698  9060-EST-ADR-TABULAR-FILE.                                       ELTACP  
01699      SET  CIA-GCTABULR-DDN TO TRUE.                               ELTACP  
01700      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACP  
01701         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELTACP  
01702         END-CALL.                                                 ELTACP  
01703      IF CIA-RC-PTR-NULL                                           ELTACP  
01704      THEN                                                         ELTACP  
01705         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTACP  
01706         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTACP  
01707      END-IF.                                                      ELTACP  
01708                                                                   ELTACP  
01709 ************************************************************      ELTACP  
01710 *                                                          *      ELTACP  
01711 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELTACP  
01712 *                                                          *      ELTACP  
01713 ************************************************************      ELTACP  
01714                                                                   ELTACP  
01715  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELTACP  
01716      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELTACP  
01717      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACP  
01718         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELTACP  
01719         END-CALL.                                                 ELTACP  
01720      IF CIA-RC-PTR-NULL                                           ELTACP  
01721         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTACP  
01722         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTACP  
01723      END-IF.                                                      ELTACP  
01724                                                                   ELTACP  
01725 *9900-SET-INDICES.                                                ELTACP  
01726 *    SET GAF-INDEX UP BY 1.                                       ELTACP  
01727 *    SET COPAY-INDEX UP BY 1.                                     ELTACP  
01728                                                                   ELTACP  
01729  9999-CODING-ERROR.                                               ELTACP  
01730        CONTINUE.                                                  ELTACP  
