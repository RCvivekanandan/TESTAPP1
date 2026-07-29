00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELGACPCC
00003  PROGRAM-ID.        ELGACPCC.                                        LV001
00004                                                                   ELGACPCC
00005  AUTHOR.            ANNE KING.                                    ELGACPCC
00006                                                                   ELGACPCC
00007  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELGACPCC
00008                     A MUTUAL LEGAL RESERVE COMPANY                ELGACPCC
00009                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELGACPCC
00010                     233 N. MICHIGAN AVE                           ELGACPCC
00011                     CHICAGO, ILLINOIS 60601                       ELGACPCC
00012                                                                   ELGACPCC
00013  DATE-WRITTEN.      29-OCT-1998.                                  ELGACPCC
00014                                                                   ELGACPCC
00015  DATE-COMPILED.                                                   ELGACPCC
00016                                                                   ELGACPCC
00017  SECURITY.          COPYRIGHT 1986, 1992,                         ELGACPCC
00018                     HEALTH CARE SERVICE CORPORATION               ELGACPCC
00019                                                                   ELGACPCC
00020  ENVIRONMENT DIVISION.                                            ELGACPCC
00021                                                                   ELGACPCC
00022  CONFIGURATION SECTION.                                           ELGACPCC
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELGACPCC
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELGACPCC
00025                                                                   ELGACPCC
00026 /*****************************************************************ELGACPCC
00027 *                                                                *ELGACPCC
00028 *  ELGACPCC-ELS:    SELECTS #ACP (CO-PAY) ACCUMULATORS AND       *ELGACPCC
00029 *                   SETUPS THE INFORMATION TO BE PROCESSED BY    *ELGACPCC
00030 *                   THE CO-PAY GENERATOR MODULE.  THE ACCUMS     *ELGACPCC
00031 *                   ARE SELECTED FROM THE GROUP SPECIFIC AND     *ELGACPCC
00032 *                   CONTRACT LEVEL PROCESSING.                   *ELGACPCC
00033 *                                                                *ELGACPCC
00034 ******************************************************************ELGACPCC
00035 *                                                                *ELGACPCC
00036 *                      MAINTENANCE HISTORY                       *ELGACPCC
00037 *                                                                *ELGACPCC
00038 *  MOD     DATE     BY                DESCRIPTION                *ELGACPCC
00039 * ----- ----------- --- ---------------------------------------- *ELGACPCC
00040 * 01.00 29-OCT-1998 AKK CLONED FROM ELGADLCC.                    *ELGACPCC
00041 *                                                                *ELGACPCC
00042 * 01.01 11-MAR-1999 AKK ADDED BAE.                               *ELGACPCC
00043 *                                                                *ELGACPCC
00044 * 01.02 24-AUG-2000 AKK ADDED SUPPORT FOR #IPGS TABULAR          *ELGACPCC
00045 *                                                                *ELGACPCC
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00046 ******************************************************************ELGACPCC
00047      TITLE  'ELGACPCC        WORKING STORAGE'.                    ELGACPCC
00048  DATA DIVISION.                                                   ELGACPCC
00049                                                                   ELGACPCC
00050  WORKING-STORAGE SECTION.                                         ELGACPCC
00051                                                                   ELGACPCC
00052  01  SWITCHES.                                                    ELGACPCC
00053      02 FIRST-TIME-SW                        PICTURE  X(01).      ELGACPCC
00054         88 FIRST-ACP                         VALUE 'Y'.           ELGACPCC
00055         88 NOT-FIRST-ACP                     VALUE 'N'.           ELGACPCC
00056      02                                      PICTURE  X(01).      ELGACPCC
00057         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELGACPCC
00058         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELGACPCC
00059      02                                      PICTURE  X(01).      ELGACPCC
00060         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELGACPCC
00061         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELGACPCC
00062      02                                      PICTURE  X(01).      ELGACPCC
00063         88 SW-CC-IND-APPLIES                 VALUE 'Y'.           ELGACPCC
00064         88 SW-CC-IND-DOES-NOT-APPLY          VALUE 'N'.           ELGACPCC
00065      02                                      PICTURE  X(01).      ELGACPCC
00066         88 SW-HAS-IBGR                       VALUE 'Y'.           ELGACPCC
00067         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELGACPCC
00068      02                                      PICTURE  X(01).      ELGACPCC
00069         88 SW-HAS-IDGD                       VALUE 'Y'.           ELGACPCC
00070         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELGACPCC
00071      02                                      PICTURE  X(01).      ELGACPCC
00072         88 SW-HAS-IPGN                       VALUE 'Y'.           ELGACPCC
00073         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELGACPCC
00074      02                                      PICTURE  X(01).      ELGACPCC
00075         88 SW-HAS-IPGP                       VALUE 'Y'.           ELGACPCC
00076         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELGACPCC
00077      02                                      PICTURE  X(01).      ELGACPCC
00078         88 SW-HAS-IPGT                       VALUE 'Y'.           ELGACPCC
00079         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELGACPCC
00080      02                                      PICTURE  X(01).      ELGACPCC
00081         88 SW-HAS-IPGS                       VALUE 'Y'.           ELGACPCC
00082         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELGACPCC
00083      02                                      PICTURE  X(01).      ELGACPCC
00084         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELGACPCC
00085         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELGACPCC
00086      02                                      PICTURE  X(01).      ELGACPCC
00087         88 SW-INTRNL-INST-PROV-CLASS         VALUE 'Y'.           ELGACPCC
00088         88 SW-INTRNL-NOT-INST-PROV-CLASS     VALUE 'N'.           ELGACPCC
00089      02                                      PICTURE  X(01).      ELGACPCC
00090         88 SW-INTRNL-PROF-PROV-CLASS         VALUE 'Y'.           ELGACPCC
00091         88 SW-INTRNL-NOT-PROF-PROV-CLASS     VALUE 'N'.           ELGACPCC
00092      02                                      PICTURE  X(01).      ELGACPCC
00093         88 SW-INTRNL-INST-PROV-SPEC          VALUE 'Y'.           ELGACPCC
00094         88 SW-INTRNL-NOT-INST-PROV-SPEC      VALUE 'N'.           ELGACPCC
00095      02                                      PICTURE  X(01).      ELGACPCC
00096         88 SW-INTRNL-PROF-PROV-SPEC          VALUE 'Y'.           ELGACPCC
00097         88 SW-INTRNL-NOT-PROF-PROV-SPEC      VALUE 'N'.           ELGACPCC
00098      02                                      PICTURE  X(01).      ELGACPCC
00099         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELGACPCC
00100         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELGACPCC
00101 / -- GCPS DATA ELEMENT TEST AREAS                                 ELGACPCC
00102                                                                   ELGACPCC
00103  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELGACPCC
00104      88 WS-LOB-INST              VALUE '1', '4', '5', '6', '8'.   ELGACPCC
00105      88 WS-LOB-PROF              VALUE '2', '4', '5', '7', '8'.   ELGACPCC
00106      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELGACPCC
00107      88 WS-LOB-BOTH              VALUE '4', '5', '6', '7', '8'.   ELGACPCC
00108                                                                   ELGACPCC
00109  01  WS-PROVISION-ARGUMENT.                                       ELGACPCC
00110      02                          PICTURE  X(05).                  ELGACPCC
00111      02 WS-PROVISION-CLASS       PICTURE  X(01).                  ELGACPCC
00112         88 INST-CLASS            VALUE 'A', 'B', 'W'.             ELGACPCC
00113         88 PROF-CLASS            VALUE 'C', 'D', 'E'.             ELGACPCC
00114                                                                   ELGACPCC
00115  01  WS-SUBTOPIC                 PICTURE  X(16).                  ELGACPCC
00116      88 WS-SUBTOPIC-ATCP         VALUE 'ATCP            '.        ELGACPCC
00117      88 WS-SUBTOPIC-BAE          VALUE 'BAE             '.        ELGACPCC
00118      88 WS-SUBTOPIC-CPO          VALUE 'CPO             '.        ELGACPCC
00119      88 WS-SUBTOPIC-CBL          VALUE 'CBL             '.        ELGACPCC
00120      88 WS-SUBTOPIC-EMH          VALUE 'EMH             '.        ELGACPCC
00121      88 WS-SUBTOPIC-HOSP         VALUE 'HOSP            '.        ELGACPCC
00122      88 WS-SUBTOPIC-IOB          VALUE 'IOB             '.        ELGACPCC
00123      88 WS-SUBTOPIC-MASOP        VALUE 'MASOP           '.        ELGACPCC
00124      88 WS-SUBTOPIC-MCN          VALUE 'MCN             '.        ELGACPCC
00125      88 WS-SUBTOPIC-MEDNEC       VALUE 'MEDNEC          '.        ELGACPCC
00126      88 WS-SUBTOPIC-MHSC         VALUE 'MHSC            '.        ELGACPCC
00127      88 WS-SUBTOPIC-MOND         VALUE 'MOND            '.        ELGACPCC
00128      88 WS-SUBTOPIC-MOPS         VALUE 'MOPS            '.        ELGACPCC
00129      88 WS-SUBTOPIC-MSA          VALUE 'MSA             '.        ELGACPCC
00130      88 WS-SUBTOPIC-PAN          VALUE 'PAN             '.        ELGACPCC
00131      88 WS-SUBTOPIC-PAR          VALUE 'PAR             '.        ELGACPCC
00132      88 WS-SUBTOPIC-PAT          VALUE 'PAT             '.        ELGACPCC
00133      88 WS-SUBTOPIC-POS          VALUE 'POS             '.        ELGACPCC
00134      88 WS-SUBTOPIC-PPO          VALUE 'PPO             '.        ELGACPCC
00135      88 WS-SUBTOPIC-REIMB        VALUE 'REIMB           '.        ELGACPCC
00136      88 WS-SUBTOPIC-RPO          VALUE 'RPO             '.        ELGACPCC
00137      88 WS-SUBTOPIC-WEEK         VALUE 'WEEK            '.        ELGACPCC
00138                                                                   ELGACPCC
00139  01  WS-COST-CONTAIN-IND.                                         ELGACPCC
00140      02                          PICTURE  X(01).                  ELGACPCC
00141         88 WS-CCI-ATCP           VALUE 'A', 'H'.                  ELGACPCC
00142         88 WS-CCI-BAE            VALUE 'N'.                       ELGACPCC
00143         88 WS-CCI-CPO            VALUE 'K'.                       ELGACPCC
00144         88 WS-CCI-CBL            VALUE 'J'.                       ELGACPCC
00145         88 WS-CCI-EMH            VALUE 'I'.                       ELGACPCC
00146         88 WS-CCI-HOSP           VALUE 'B', 'F'.                  ELGACPCC
00147         88 WS-CCI-IOB            VALUE '5'.                       ELGACPCC
00148         88 WS-CCI-MASOP          VALUE '2', 'E', 'G'.             ELGACPCC
00149         88 WS-CCI-MCN            VALUE 'M'.                       ELGACPCC
00150         88 WS-CCI-MEDNEC         VALUE '7'.                       ELGACPCC
00151         88 WS-CCI-MHSC           VALUE 'S'.                       ELGACPCC
00152         88 WS-CCI-MOND           VALUE '4'.                       ELGACPCC
00153         88 WS-CCI-MOPS           VALUE '1', 'F'.                  ELGACPCC
00154         88 WS-CCI-MSA            VALUE '8'.                       ELGACPCC
00155         88 WS-CCI-PAR            VALUE '6', 'E', 'G'.             ELGACPCC
00156         88 WS-CCI-PAT            VALUE 'D'.                       ELGACPCC
00157         88 WS-CCI-PAN            VALUE 'L'.                       ELGACPCC
00158         88 WS-CCI-POS            VALUE 'P'.                       ELGACPCC
00159         88 WS-CCI-PPO            VALUE '9'.                       ELGACPCC
00160         88 WS-CCI-REIMB          VALUE '*'.                       ELGACPCC
00161         88 WS-CCI-RPO            VALUE 'R'.                       ELGACPCC
00162         88 WS-CCI-WEEK           VALUE '3'.                       ELGACPCC
00163      02                          PICTURE  X(01).                  ELGACPCC
00164 / -- CONSTANTS AND WORK FIELDS                                    ELGACPCC
00165                                                                   ELGACPCC
00166  01  PROGRAM-CONSTANTS.                                           ELGACPCC
00167      02 PC-ACP                   PICTURE  X(06) VALUE '#ACP  '.   ELGACPCC
00168      02 PC-GCT-MAX-SUB           PICTURE S9(04) COMP.             ELGACPCC
00169      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELGACPCC
00170      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELGACPCC
00171      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELGACPCC
00172      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELGACPCC
00173      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELGACPCC
00174      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGT '.   ELGACPCC
00175      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELGACPCC
00176                                                                   ELGACPCC
00177  01  WS-WORK-FIELDS.                                              ELGACPCC
00178      02 WS-OCCURRENCE-SUB        PICTURE S9(04) COMP.             ELGACPCC
00179      02 WS-ACP-SUB               PICTURE S9(04) COMP.             ELGACPCC
00180      02 WS-ACP-ACCUM-CNT         PICTURE S9(04) COMP.             ELGACPCC
00181      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELGACPCC
00182                                                                   ELGACPCC
00183  01  WS-MAX-INDEX-VALUES.                                         ELGACPCC
00184      02 WS-HOLD-IDX              USAGE IS INDEX.                  ELGACPCC
00185      02 WS-MAX-GAF-INDEX         INDEX.                           ELGACPCC
00186      02 WS-MAX-GAF-INT-INDEX     INDEX.                           ELGACPCC
00187      02 WS-MAX-GCT-INDEX         INDEX.                           ELGACPCC
00188      02 WS-MAX-GCG-INDEX         INDEX.                           ELGACPCC
00189      02 WS-MAX-GX1-INDEX         INDEX.                           ELGACPCC
00190      02 WS-MAX-GX3-INDEX         INDEX.                           ELGACPCC
00191      02 WS-MAX-GXS-INDEX         INDEX.                           ELGACPCC
00192                                                                   ELGACPCC
00193  01  WS-POINTERS.                                                 ELGACPCC
00194      02  WS-POINTER1             POINTER.                         ELGACPCC
00195      02  WS-POINTER2             POINTER.                         ELGACPCC
00196                                                                   ELGACPCC
00197  01  ACCUM-HOLD-TBL.                                              ELGACPCC
00198      02  ACCUM-SLOT-NBR          PICTURE S9(07) COMP-3            ELGACPCC
00199                                  OCCURS 5 TIMES.                  ELGACPCC
00200                                                                   ELGACPCC
00201  01  WS-OCCURRENCE-PROCESSED-TBL.                                 ELGACPCC
00202      02                          PICTURE X                        ELGACPCC
00203                                  OCCURS 44 TIMES                  ELGACPCC
00204                                  INDEXED BY WS-OCCURRENCE-INDEX.  ELGACPCC
00205          88  WS-OCCURRENCE-PROCESSED           VALUE 'P'.         ELGACPCC
00206          88  WS-OCCURRENCE-NOT-PROCESSED       VALUE ' '.         ELGACPCC
00207                                                                   ELGACPCC
00208                                                                   ELGACPCC
00209  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELGACPCC
00210      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGACPCC
00211      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGACPCC
00212      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGACPCC
00213      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGACPCC
00214      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGACPCC
00215      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGACPCC
00216 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELGACPCC
00217      COPY ELSCFTB2.                                               ELGACPCC
00218                                                                   ELGACPCC
00219 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELGACPCC
00220      COPY ELSCFTB9.                                               ELGACPCC
00221                                                                   ELGACPCC
00222      TITLE  'ELGACPCC        LINKAGE SECTION'                     ELGACPCC
00223  LINKAGE SECTION.                                                 ELGACPCC
00224  01  DFHCOMMAREA.                                                 ELGACPCC
00225      COPY ELSCOMMC.                                               ELGACPCC
00226 /                                                                 ELGACPCC
00227      COPY ELSCIA2C.                                               ELGACPCC
00228 /                                                                 ELGACPCC
00229      COPY ELSIOPMC.                                               ELGACPCC
00230 /                                                                 ELGACPCC
00231      COPY ELSKEYSC.                                               ELGACPCC
00232 /                                                                 ELGACPCC
00233      COPY ELSSRTPC.                                               ELGACPCC
00234 /                                                                 ELGACPCC
00235      COPY ELSSSCBC.                                               ELGACPCC
00236 /                                                                 ELGACPCC
00237  01  GCG-GRP-SPEC-RECORD-AREA.                                    ELGACPCC
00238      COPY GCGROUPC.                                               ELGACPCC
00239 /                                                                 ELGACPCC
00240  01  GCT-CONTRACT-RECORD-AREA.                                    ELGACPCC
00241      COPY GCCONTRC.                                               ELGACPCC
00242 /                                                                 ELGACPCC
00243  01  GAF-RECORD-AREA.                                             ELGACPCC
00244      COPY GCTACPC.                                                ELGACPCC
00245 /                                                                 ELGACPCC
00246      COPY ELSACUMC.                                               ELGACPCC
00247 /                                                                 ELGACPCC
00248  01  GX1-RECORD-AREA.                                             ELGACPCC
00249      COPY GCTIBGRC.                                               ELGACPCC
00250 /                                                                 ELGACPCC
00251  01  GX3-RECORD-AREA.                                             ELGACPCC
00252      COPY GCTIPGTC.                                               ELGACPCC
00253 /                                                                 ELGACPCC
00254  01  GXS-RECORD-AREA.                                             ELGACPCC
00255      COPY GCTIPGSC.                                               ELGACPCC
00256      TITLE  'ELGACPCC        PROCEDURE DIVISION'.                 ELGACPCC
00257 ************************************************************      ELGACPCC
00258 *                                                          *      ELGACPCC
00259 *    ELTACP MAINLINE                                       *      ELGACPCC
00260 *                                                          *      ELGACPCC
00261 ************************************************************      ELGACPCC
00262                                                                   ELGACPCC
00263  PROCEDURE DIVISION.                                              ELGACPCC
00264      PERFORM 0010-INITIALIZATION.                                 ELGACPCC
00265      PERFORM 0100-PROCESS.                                        ELGACPCC
00266      GOBACK.                                                      ELGACPCC
00267                                                                   ELGACPCC
00268 ************************************************************      ELGACPCC
00269 *                                                          *      ELGACPCC
00270 *    INITIALIZATION                                        *      ELGACPCC
00271 *                                                          *      ELGACPCC
00272 ************************************************************      ELGACPCC
00273                                                                   ELGACPCC
00274  0010-INITIALIZATION.                                             ELGACPCC
00275      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELGACPCC
00276      PERFORM 0060-EST-ADR-KEY-WK-AREA.                            ELGACPCC
00277      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELGACPCC
00278      PERFORM 0120-EST-ADR-GRP-SPC.                                ELGACPCC
00279      PERFORM 0190-INIT-DATA.                                      ELGACPCC
00280                                                                   ELGACPCC
00281 ************************************************************      ELGACPCC
00282 *                                                          *      ELGACPCC
00283 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGACPCC
00284 *                                                          *      ELGACPCC
00285 ************************************************************      ELGACPCC
00286                                                                   ELGACPCC
00287  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELGACPCC
00288      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGACPCC
00289      THEN                                                         ELGACPCC
00290         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGACPCC
00291      ELSE                                                         ELGACPCC
00292         IF ECA-CIA-PTR = NULL                                     ELGACPCC
00293         THEN                                                      ELGACPCC
00294            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGACPCC
00295         ELSE                                                      ELGACPCC
00296            CALL 'ELUINISM' USING DFHCOMMAREA                      ELGACPCC
00297               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELGACPCC
00298               END-CALL                                            ELGACPCC
00299            SET CIA-ELSSSCB-DDN TO TRUE                            ELGACPCC
00300            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELGACPCC
00301               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELGACPCC
00302               END-CALL                                            ELGACPCC
00303            IF CIA-RC-PTR-NULL                                     ELGACPCC
00304            THEN                                                   ELGACPCC
00305               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELGACPCC
00306               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGACPCC
00307            ELSE                                                   ELGACPCC
00308               CONTINUE                                            ELGACPCC
00309            END-IF                                                 ELGACPCC
00310         END-IF                                                    ELGACPCC
00311      END-IF.                                                      ELGACPCC
00312                                                                   ELGACPCC
00313 /***********************************************************      ELGACPCC
00314 *                                                          *      ELGACPCC
00315 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELGACPCC
00316 *                                                          *      ELGACPCC
00317 ************************************************************      ELGACPCC
00318                                                                   ELGACPCC
00319  0060-EST-ADR-KEY-WK-AREA.                                        ELGACPCC
00320      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGACPCC
00321      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACPCC
00322         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELGACPCC
00323         END-CALL.                                                 ELGACPCC
00324      IF CIA-RC-PTR-NULL                                           ELGACPCC
00325      THEN                                                         ELGACPCC
00326         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACPCC
00327         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGACPCC
00328      END-IF.                                                      ELGACPCC
00329                                                                   ELGACPCC
00330 ************************************************************      ELGACPCC
00331 *                                                          *      ELGACPCC
00332 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGACPCC
00333 *                                                          *      ELGACPCC
00334 ************************************************************      ELGACPCC
00335                                                                   ELGACPCC
00336  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGACPCC
00337      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGACPCC
00338      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACPCC
00339         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELGACPCC
00340         END-CALL.                                                 ELGACPCC
00341      IF CIA-RC-PTR-NULL                                           ELGACPCC
00342      THEN                                                         ELGACPCC
00343         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACPCC
00344         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGACPCC
00345      END-IF.                                                      ELGACPCC
00346                                                                   ELGACPCC
00347 ************************************************************      ELGACPCC
00348 *                                                          *      ELGACPCC
00349 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD     *      ELGACPCC
00350 *                                                          *      ELGACPCC
00351 ************************************************************      ELGACPCC
00352                                                                   ELGACPCC
00353  0120-EST-ADR-GRP-SPC.                                            ELGACPCC
00354      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELGACPCC
00355      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACPCC
00356         ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                       ELGACPCC
00357         END-CALL.                                                 ELGACPCC
00358      IF CIA-RC-PTR-NULL                                           ELGACPCC
00359      THEN                                                         ELGACPCC
00360         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACPCC
00361         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGACPCC
00362      END-IF.                                                      ELGACPCC
00363                                                                   ELGACPCC
00364 /***********************************************************      ELGACPCC
00365 *                                                          *      ELGACPCC
00366 *    INITIALIZE DATA AREAS                                 *      ELGACPCC
00367 *                                                          *      ELGACPCC
00368 ************************************************************      ELGACPCC
00369                                                                   ELGACPCC
00370  0190-INIT-DATA.                                                  ELGACPCC
00371      COMPUTE PC-GCT-MAX-SUB =   LENGTH OF GCT-CONT-TAB-PTRS       ELGACPCC
00372                               / LENGTH OF GCT-CON-TAB-ID-SLOT.    ELGACPCC
00373      SET GCT-INDEX TO PC-GCT-MAX-SUB.                             ELGACPCC
00374      SET WS-MAX-GCT-INDEX TO GCT-INDEX.                           ELGACPCC
00375      SET GCG-INDEX TO GCG-COUNT-TAB-PROVN-POINTERS.               ELGACPCC
00376      SET WS-MAX-GCG-INDEX TO GCG-INDEX.                           ELGACPCC
00377      INITIALIZE WS-ACP-ACCUM-CNT.                                 ELGACPCC
00378      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGACPCC
00379                                                                   ELGACPCC
00380 /***********************************************************      ELGACPCC
00381 *                                                          *      ELGACPCC
00382 *        PROCESS                                           *      ELGACPCC
00383 *                                                          *      ELGACPCC
00384 ************************************************************      ELGACPCC
00385                                                                   ELGACPCC
00386  0100-PROCESS.                                                    ELGACPCC
00387      PERFORM 0110-SCAN-GRP-SPC-FOR-ACCUMS.                        ELGACPCC
00388      PERFORM 0120-SCAN-CONTRACTS-FOR-ACCUMS.                      ELGACPCC
00389                                                                   ELGACPCC
00390      IF WS-ACP-ACCUM-CNT >  0                                     ELGACPCC
00391      THEN                                                         ELGACPCC
00392          PERFORM 0240-SCAN-FOR-APPLIC-OCCRNCS                     ELGACPCC
00393      END-IF.                                                      ELGACPCC
00394                                                                   ELGACPCC
00395 *    -- LINK TO THE OUTPUT GENERATOR                              ELGACPCC
00396      IF SW-APPLIC-ACCUM-FOUND                                     ELGACPCC
00397         SET SRP-COST-CONT-ACCUM TO TRUE                           ELGACPCC
00398         EXEC CICS LINK PROGRAM ('ELGACP')                         ELGACPCC
00399                        COMMAREA (DFHCOMMAREA)                     ELGACPCC
00400         END-EXEC                                                  ELGACPCC
00401      END-IF.                                                      ELGACPCC
00402                                                                   ELGACPCC
00403                                                                   ELGACPCC
00404 /***********************************************************      ELGACPCC
00405 *                                                          *      ELGACPCC
00406 *    SCAN GROUP SPECIFIC RECORD FOR ACCUMULATORS           *      ELGACPCC
00407 *                                                          *      ELGACPCC
00408 ************************************************************      ELGACPCC
00409                                                                   ELGACPCC
00410  0110-SCAN-GRP-SPC-FOR-ACCUMS.                                    ELGACPCC
00411      PERFORM WITH TEST BEFORE                                     ELGACPCC
00412         VARYING GCG-INDEX FROM 1 BY 1                             ELGACPCC
00413           UNTIL GCG-INDEX = WS-MAX-GCG-INDEX                      ELGACPCC
00414                 OR GCG-TAB-ID (GCG-INDEX) > PC-ACP                ELGACPCC
00415 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELGACPCC
00416         IF     GCG-TAB-ID (GCG-INDEX)  =  PC-ACP                  ELGACPCC
00417            AND GCG-TAB-SLOT-NO (GCG-INDEX)  >  ZERO               ELGACPCC
00418         THEN                                                      ELGACPCC
00419 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELGACPCC
00420            MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO WS-SLOT-NBR        ELGACPCC
00421            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELGACPCC
00422         END-IF                                                    ELGACPCC
00423         END-PERFORM.                                              ELGACPCC
00424                                                                   ELGACPCC
00425 /***********************************************************      ELGACPCC
00426 *                                                          *      ELGACPCC
00427 *    SCAN CONTRACT RECORDS FOR ACCUMULATORS                *      ELGACPCC
00428 *                                                          *      ELGACPCC
00429 ************************************************************      ELGACPCC
00430                                                                   ELGACPCC
00431  0120-SCAN-CONTRACTS-FOR-ACCUMS.                                  ELGACPCC
00432      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELGACPCC
00433      THEN                                                         ELGACPCC
00434          PERFORM 0130-SCAN-INST-BAS                               ELGACPCC
00435      END-IF.                                                      ELGACPCC
00436                                                                   ELGACPCC
00437      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELGACPCC
00438      THEN                                                         ELGACPCC
00439          PERFORM 0140-SCAN-PROF-BAS                               ELGACPCC
00440      END-IF.                                                      ELGACPCC
00441                                                                   ELGACPCC
00442      SET WS-POINTER1 TO NULLS.                                    ELGACPCC
00443      SET WS-POINTER2 TO NULLS.                                    ELGACPCC
00444                                                                   ELGACPCC
00445      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELGACPCC
00446      THEN                                                         ELGACPCC
00447          PERFORM 0150-SCAN-INST-SUP                               ELGACPCC
00448      END-IF.                                                      ELGACPCC
00449                                                                   ELGACPCC
00450      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELGACPCC
00451      THEN                                                         ELGACPCC
00452          PERFORM 0170-SCAN-PROF-SUP                               ELGACPCC
00453      END-IF.                                                      ELGACPCC
00454                                                                   ELGACPCC
00455 /***********************************************************      ELGACPCC
00456 *                                                          *      ELGACPCC
00457 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELGACPCC
00458 *                                                          *      ELGACPCC
00459 ************************************************************      ELGACPCC
00460                                                                   ELGACPCC
00461  0130-SCAN-INST-BAS.                                              ELGACPCC
00462      SET CIA-ELSCONIB-DDN TO TRUE.                                ELGACPCC
00463      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACPCC
00464         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGACPCC
00465         END-CALL.                                                 ELGACPCC
00466      SET WS-POINTER1 TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.      ELGACPCC
00467                                                                   ELGACPCC
00468      IF CIA-RC-PTR-NULL                                           ELGACPCC
00469      THEN                                                         ELGACPCC
00470         CONTINUE                                                  ELGACPCC
00471      ELSE                                                         ELGACPCC
00472         PERFORM 0180-SCAN-CONTRACT-FOR-ACCUMS                     ELGACPCC
00473      END-IF.                                                      ELGACPCC
00474                                                                   ELGACPCC
00475 ************************************************************      ELGACPCC
00476 *                                                          *      ELGACPCC
00477 *    SCAN PROFESSIONAL BASIC CONTRACT RECORD               *      ELGACPCC
00478 *                                                          *      ELGACPCC
00479 ************************************************************      ELGACPCC
00480                                                                   ELGACPCC
00481  0140-SCAN-PROF-BAS.                                              ELGACPCC
00482      SET CIA-ELSCONPB-DDN TO TRUE.                                ELGACPCC
00483      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACPCC
00484         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGACPCC
00485         END-CALL.                                                 ELGACPCC
00486      SET WS-POINTER2 TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.      ELGACPCC
00487                                                                   ELGACPCC
00488      IF CIA-RC-PTR-NULL OR (WS-POINTER1 = WS-POINTER2)            ELGACPCC
00489      THEN                                                         ELGACPCC
00490         CONTINUE                                                  ELGACPCC
00491      ELSE                                                         ELGACPCC
00492         PERFORM 0180-SCAN-CONTRACT-FOR-ACCUMS                     ELGACPCC
00493      END-IF.                                                      ELGACPCC
00494                                                                   ELGACPCC
00495 /***********************************************************      ELGACPCC
00496 *                                                          *      ELGACPCC
00497 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELGACPCC
00498 *                                                          *      ELGACPCC
00499 ************************************************************      ELGACPCC
00500                                                                   ELGACPCC
00501  0150-SCAN-INST-SUP.                                              ELGACPCC
00502      SET CIA-ELSCONIS-DDN TO TRUE.                                ELGACPCC
00503      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACPCC
00504         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGACPCC
00505         END-CALL.                                                 ELGACPCC
00506      SET WS-POINTER1 TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.      ELGACPCC
00507                                                                   ELGACPCC
00508      IF CIA-RC-PTR-NULL                                           ELGACPCC
00509      THEN                                                         ELGACPCC
00510         CONTINUE                                                  ELGACPCC
00511      ELSE                                                         ELGACPCC
00512         PERFORM 0180-SCAN-CONTRACT-FOR-ACCUMS                     ELGACPCC
00513      END-IF.                                                      ELGACPCC
00514                                                                   ELGACPCC
00515 ************************************************************      ELGACPCC
00516 *                                                          *      ELGACPCC
00517 *    SCAN PROFESSIONAL SUPPLEMENTAL CONTRACT RECORD        *      ELGACPCC
00518 *                                                          *      ELGACPCC
00519 ************************************************************      ELGACPCC
00520                                                                   ELGACPCC
00521  0170-SCAN-PROF-SUP.                                              ELGACPCC
00522      SET CIA-ELSCONPS-DDN TO TRUE.                                ELGACPCC
00523      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACPCC
00524         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGACPCC
00525         END-CALL.                                                 ELGACPCC
00526      SET WS-POINTER2 TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.      ELGACPCC
00527                                                                   ELGACPCC
00528      IF CIA-RC-PTR-NULL AND (WS-POINTER1 = WS-POINTER2)           ELGACPCC
00529      THEN                                                         ELGACPCC
00530         CONTINUE                                                  ELGACPCC
00531      ELSE                                                         ELGACPCC
00532         PERFORM 0180-SCAN-CONTRACT-FOR-ACCUMS                     ELGACPCC
00533      END-IF.                                                      ELGACPCC
00534                                                                   ELGACPCC
00535 /***********************************************************      ELGACPCC
00536 *                                                          *      ELGACPCC
00537 *    SCAN A CONTRACT RECORD FOR ACCUMULATORS               *      ELGACPCC
00538 *                                                          *      ELGACPCC
00539 ************************************************************      ELGACPCC
00540                                                                   ELGACPCC
00541  0180-SCAN-CONTRACT-FOR-ACCUMS.                                   ELGACPCC
00542      PERFORM WITH TEST BEFORE                                     ELGACPCC
00543         VARYING GCT-TAB-INDEX FROM 1 BY 1                         ELGACPCC
00544           UNTIL    GCT-TAB-INDEX > WS-MAX-GCT-INDEX               ELGACPCC
00545                 OR GCT-CON-TAB-ID (GCT-TAB-INDEX) > PC-ACP        ELGACPCC
00546 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELGACPCC
00547         IF     GCT-CON-TAB-ID (GCT-TAB-INDEX) = PC-ACP            ELGACPCC
00548            AND GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  > ZEROS          ELGACPCC
00549         THEN                                                      ELGACPCC
00550 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELGACPCC
00551            MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  TO  WS-SLOT-NBR ELGACPCC
00552            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELGACPCC
00553         END-IF                                                    ELGACPCC
00554         END-PERFORM.                                              ELGACPCC
00555                                                                   ELGACPCC
00556 /***********************************************************      ELGACPCC
00557 *                                                          *      ELGACPCC
00558 *    SAVE UNIQUE ACCUMULATOR SLOT NUMBER                   *      ELGACPCC
00559 *                                                          *      ELGACPCC
00560 ************************************************************      ELGACPCC
00561                                                                   ELGACPCC
00562  0200-SAVE-UNQ-ACCUM-SLOT-NBR.                                    ELGACPCC
00563                                                                   ELGACPCC
00564 * -- SCAN TABLE OF ACCUM SLOT NUMBERS FOR DUPLICATE               ELGACPCC
00565      SET SW-UNQ-SLOT-NBR TO TRUE.                                 ELGACPCC
00566      PERFORM WITH TEST BEFORE                                     ELGACPCC
00567         VARYING WS-ACP-SUB FROM 1 BY 1                            ELGACPCC
00568           UNTIL    WS-ACP-SUB > WS-ACP-ACCUM-CNT                  ELGACPCC
00569                 OR SW-DUP-SLOT-NBR                                ELGACPCC
00570         IF WS-SLOT-NBR = ACCUM-SLOT-NBR (WS-ACP-SUB)              ELGACPCC
00571         THEN                                                      ELGACPCC
00572            SET SW-DUP-SLOT-NBR TO TRUE                            ELGACPCC
00573         END-IF                                                    ELGACPCC
00574         END-PERFORM.                                              ELGACPCC
00575                                                                   ELGACPCC
00576 * -- IF SLOT NUMBER IS UNIQUE, ADD IT TO THE HOLD TABLE           ELGACPCC
00577      IF SW-UNQ-SLOT-NBR                                           ELGACPCC
00578      THEN                                                         ELGACPCC
00579         ADD 1 TO  WS-ACP-ACCUM-CNT                                ELGACPCC
00580         MOVE WS-SLOT-NBR TO ACCUM-SLOT-NBR(WS-ACP-ACCUM-CNT)      ELGACPCC
00581      END-IF.                                                      ELGACPCC
00582                                                                   ELGACPCC
00583 /***********************************************************      ELGACPCC
00584 *                                                          *      ELGACPCC
00585 *    SCAN ADL ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELGACPCC
00586 *                                                          *      ELGACPCC
00587 ************************************************************      ELGACPCC
00588                                                                   ELGACPCC
00589  0240-SCAN-FOR-APPLIC-OCCRNCS.                                    ELGACPCC
00590      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGACPCC
00591      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGACPCC
00592      PERFORM 0250-DELETE-ACP-SUMMARY-FILE.                        ELGACPCC
00593      PERFORM 0260-ALLOC-WORKFILE-REC-AREA.                        ELGACPCC
00594                                                                   ELGACPCC
00595 * -- READ AND SCAN EACH ACCUMULATOR TABULAR                       ELGACPCC
00596      PERFORM WITH TEST BEFORE                                     ELGACPCC
00597         VARYING WS-ACP-SUB FROM 1 BY 1                            ELGACPCC
00598           UNTIL WS-ACP-SUB > WS-ACP-ACCUM-CNT                     ELGACPCC
00599 *    -- OBTAIN ACCUMULATOR TABULAR RECORD                         ELGACPCC
00600         MOVE PC-ACP TO KWA-PROVISION-ID                           ELGACPCC
00601         MOVE ACCUM-SLOT-NBR (WS-ACP-SUB) TO KWA-PROVISION-SLOT-NO ELGACPCC
00602         PERFORM 0280-READ-TABULAR-REC                             ELGACPCC
00603 *    -- SCAN ACCUMULATOR TABULAR                                  ELGACPCC
00604        PERFORM WITH TEST BEFORE                                   ELGACPCC
00605           VARYING WS-OCCURRENCE-SUB FROM 1 BY 1                   ELGACPCC
00606              UNTIL WS-OCCURRENCE-SUB >= GAF-ENTRY-COUNT           ELGACPCC
00607              IF WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB)       ELGACPCC
00608                 CONTINUE                                          ELGACPCC
00609              ELSE                                                 ELGACPCC
00610                 SET GAF-INDEX TO WS-OCCURRENCE-SUB                ELGACPCC
00611                 PERFORM 0300-TEST-ACP-OCCURRENCE                  ELGACPCC
00612              END-IF                                               ELGACPCC
00613        END-PERFORM                                                ELGACPCC
00614      END-PERFORM.                                                 ELGACPCC
00615                                                                   ELGACPCC
00616 *       PERFORM 0300-TEST-ACP-OCCURENCE                           ELGACPCC
00617 *          VARYING GAF-INDEX FROM 1 BY 1                          ELGACPCC
00618 *            UNTIL GAF-INDEX = WS-MAX-GAF-INDEX                   ELGACPCC
00619 *       END-PERFORM.                                              ELGACPCC
00620                                                                   ELGACPCC
00621 /***********************************************************      ELGACPCC
00622 *                                                          *      ELGACPCC
00623 *        DELETE ACP SUMMARY FILE                           *      ELGACPCC
00624 *                                                          *      ELGACPCC
00625 ************************************************************      ELGACPCC
00626                                                                   ELGACPCC
00627  0250-DELETE-ACP-SUMMARY-FILE.                                    ELGACPCC
00628      SET IOP-DEL TO TRUE.                                         ELGACPCC
00629      SET IOP-FCQ-NONE TO TRUE.                                    ELGACPCC
00630      SET IOP-KVQ-NONE TO TRUE.                                    ELGACPCC
00631      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACPCC
00632                                                                   ELGACPCC
00633 /***********************************************************      ELGACPCC
00634 *                                                          *      ELGACPCC
00635 *    ALLOCATE WORKFILE RECORD AREA                         *      ELGACPCC
00636 *                                                          *      ELGACPCC
00637 ************************************************************      ELGACPCC
00638                                                                   ELGACPCC
00639  0260-ALLOC-WORKFILE-REC-AREA.                                    ELGACPCC
00640      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGACPCC
00641      SET CIA-STG-GETMAIN TO TRUE.                                 ELGACPCC
00642      SET IOP-GETMAIN-REC TO TRUE.                                 ELGACPCC
00643      COMPUTE IOP-MAX-REC-LEN =                                    ELGACPCC
00644              LENGTH OF ACCUM-FIXED-AREA                           ELGACPCC
00645 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELGACPCC
00646            + LENGTH OF ACCUM-VARIABLE-AREA                        ELGACPCC
00647            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELGACPCC
00648 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELGACPCC
00649 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELGACPCC
00650                                                                   ELGACPCC
00651      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACPCC
00652      IF IOP-REC-PTR = NULLS                                       ELGACPCC
00653      THEN                                                         ELGACPCC
00654         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACPCC
00655         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGACPCC
00656      ELSE                                                         ELGACPCC
00657         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELGACPCC
00658      END-IF.                                                      ELGACPCC
00659                                                                   ELGACPCC
00660 /***********************************************************      ELGACPCC
00661 *                                                          *      ELGACPCC
00662 *    READ TABULAR RECORD                                   *      ELGACPCC
00663 *                                                          *      ELGACPCC
00664 ************************************************************      ELGACPCC
00665                                                                   ELGACPCC
00666  0280-READ-TABULAR-REC.                                           ELGACPCC
00667      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELGACPCC
00668      SET IOP-RD TO TRUE.                                          ELGACPCC
00669      SET IOP-FCQ-NONE TO TRUE.                                    ELGACPCC
00670      SET IOP-KVQ-EQ TO TRUE.                                      ELGACPCC
00671      SET IOP-STG-MODE-MOVE TO TRUE.                               ELGACPCC
00672      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGACPCC
00673      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGACPCC
00674      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACPCC
00675                                                                   ELGACPCC
00676      EVALUATE TRUE                                                ELGACPCC
00677        WHEN IOP-RC-OK                                             ELGACPCC
00678           SET ADDRESS OF GAF-RECORD-AREA TO IOP-REC-PTR           ELGACPCC
00679           SET IOP-REC-PTR TO NULLS                                ELGACPCC
00680           SET GAF-INDEX TO GAF-ENTRY-COUNT                        ELGACPCC
00681           SET WS-MAX-GAF-INDEX TO GAF-INDEX                       ELGACPCC
00682        WHEN IOP-RC-NOTFND                                         ELGACPCC
00683           SET CIA-AB-NOTFND-GCTABULR TO TRUE                      ELGACPCC
00684           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELGACPCC
00685        WHEN OTHER                                                 ELGACPCC
00686           SET CIA-AB-CRITIO TO TRUE                               ELGACPCC
00687           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELGACPCC
00688        END-EVALUATE.                                              ELGACPCC
00689                                                                   ELGACPCC
00690 /***********************************************************      ELGACPCC
00691 *                                                          *      ELGACPCC
00692 *        TEST ACP OCCURS                                   *      ELGACPCC
00693 *                                                          *      ELGACPCC
00694 ************************************************************      ELGACPCC
00695  0300-TEST-ACP-OCCURRENCE.                                        ELGACPCC
00696      MOVE GAF-COPAY-L-O-B (GAF-INDEX) TO WS-LOB-ACCUM-OCCRNC.     ELGACPCC
00697      MOVE GAF-COPAY-COST-CONTAIN-IND (GAF-INDEX)                  ELGACPCC
00698        TO WS-COST-CONTAIN-IND.                                    ELGACPCC
00699      SET SW-CC-IND-DOES-NOT-APPLY TO TRUE.                        ELGACPCC
00700      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELGACPCC
00701                                                                   ELGACPCC
00702      PERFORM 0500-CHK-CC-IND.                                     ELGACPCC
00703                                                                   ELGACPCC
00704      IF SW-CC-IND-APPLIES                                         ELGACPCC
00705      THEN                                                         ELGACPCC
00706 *    -- SCAN FOR INTERNAL TABULARS                                ELGACPCC
00707 *       (THIS IS DONE NOW IN CASE IPGT OR IBGR IS NEEDED TO       ELGACPCC
00708 *        DETERMINE WHETHER OCCURRENCE IS INSTITUTIONAL OR         ELGACPCC
00709 *        PROFESSIONAL.)                                           ELGACPCC
00710         PERFORM 0310-SCAN-INTRNL-TAB                              ELGACPCC
00711         EVALUATE TRUE ALSO TRUE                                   ELGACPCC
00712            WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                     ELGACPCC
00713               SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE               ELGACPCC
00714               SET SW-OCCRNC-APPLIES TO TRUE                       ELGACPCC
00715            WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH              ELGACPCC
00716               SET SRP-ACCUM-PROV-CLASS-INST TO TRUE               ELGACPCC
00717               PERFORM 0550-CHK-INTRNL-TAB-PROV-CLASS              ELGACPCC
00718            WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST              ELGACPCC
00719               SET SRP-ACCUM-PROV-CLASS-INST TO TRUE               ELGACPCC
00720               SET SW-OCCRNC-APPLIES TO TRUE                       ELGACPCC
00721            WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH              ELGACPCC
00722               SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE               ELGACPCC
00723               PERFORM 0550-CHK-INTRNL-TAB-PROV-CLASS              ELGACPCC
00724               SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                ELGACPCC
00725               PERFORM 0551-CHK-INTRNL-TAB-PROV-SPEC               ELGACPCC
00726            WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF              ELGACPCC
00727               SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE               ELGACPCC
00728               SET SW-OCCRNC-APPLIES TO TRUE                       ELGACPCC
00729            WHEN OTHER                                             ELGACPCC
00730               CONTINUE                                            ELGACPCC
00731            END-EVALUATE                                           ELGACPCC
00732      ELSE                                                         ELGACPCC
00733         CONTINUE                                                  ELGACPCC
00734      END-IF.                                                      ELGACPCC
00735                                                                   ELGACPCC
00736      IF SW-OCCRNC-APPLIES                                         ELGACPCC
00737      THEN                                                         ELGACPCC
00738 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELGACPCC
00739         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELGACPCC
00740         PERFORM 0330-INIT-ACCUM-EXTRACT                           ELGACPCC
00741         PERFORM 0340-EXTRACT-ACCUM                                ELGACPCC
00742         PERFORM 0410-CHK-EXTRACT-DATA-INTGRTY                     ELGACPCC
00743         PERFORM 0375-VARIABLE-DATA                                ELGACPCC
00744         PERFORM 0710-WRITE-EXTRACT-RECORD                         ELGACPCC
00745          MOVE 1 TO WS-OCCURRENCE-SUB                              ELGACPCC
00746      END-IF.                                                      ELGACPCC
00747                                                                   ELGACPCC
00748 /***********************************************************      ELGACPCC
00749 *                                                          *      ELGACPCC
00750 *    SCAN INTERNAL TABULARS                                *      ELGACPCC
00751 *                                                          *      ELGACPCC
00752 ************************************************************      ELGACPCC
00753                                                                   ELGACPCC
00754  0310-SCAN-INTRNL-TAB.                                            ELGACPCC
00755                                                                   ELGACPCC
00756 * -- INITIALIZE SCAN PROCESS                                      ELGACPCC
00757      SET SW-OCCRNC-DOES-NOT-APPLY                                 ELGACPCC
00758          SW-HAS-NO-IBGR                                           ELGACPCC
00759          SW-HAS-NO-IDGD                                           ELGACPCC
00760          SW-HAS-NO-IPGN                                           ELGACPCC
00761          SW-HAS-NO-IPGP                                           ELGACPCC
00762          SW-HAS-NO-IPGT                                           ELGACPCC
00763          SW-HAS-NO-IPGS                                           ELGACPCC
00764       TO TRUE.                                                    ELGACPCC
00765      INITIALIZE WS-IBGR-SLOT-NBR                                  ELGACPCC
00766                 WS-IDGD-SLOT-NBR                                  ELGACPCC
00767                 WS-IPGN-SLOT-NBR                                  ELGACPCC
00768                 WS-IPGP-SLOT-NBR                                  ELGACPCC
00769                 WS-IPGT-SLOT-NBR                                  ELGACPCC
00770                 WS-IPGS-SLOT-NBR.                                 ELGACPCC
00771      SET GAF-INT-INDEX TO GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX). ELGACPCC
00772      SET WS-MAX-GAF-INT-INDEX TO GAF-INT-INDEX.                   ELGACPCC
00773                                                                   ELGACPCC
00774 * -- SCAN THE LIST OF INTERNAL TABULARS                           ELGACPCC
00775      PERFORM 0320-SCAN-THE-INTERNAL-TABULAR                       ELGACPCC
00776         VARYING GAF-INT-INDEX FROM 1 BY 1                         ELGACPCC
00777           UNTIL GAF-INT-INDEX >= WS-MAX-GAF-INT-INDEX.            ELGACPCC
00778                                                                   ELGACPCC
00779 /***********************************************************      ELGACPCC
00780 *                                                          *      ELGACPCC
00781 *    SCAN INTERNAL TABULAR LIST                            *      ELGACPCC
00782 *                                                          *      ELGACPCC
00783 ************************************************************      ELGACPCC
00784                                                                   ELGACPCC
00785  0320-SCAN-THE-INTERNAL-TABULAR.                                  ELGACPCC
00786      IF GAF-INT-SLOT (GAF-INDEX, GAF-INT-INDEX) > 0               ELGACPCC
00787      THEN                                                         ELGACPCC
00788         MOVE GAF-INT-SLOT (GAF-INDEX, GAF-INT-INDEX)              ELGACPCC
00789           TO WS-SLOT-NBR                                          ELGACPCC
00790         EVALUATE GAF-INT-ID (GAF-INDEX, GAF-INT-INDEX)            ELGACPCC
00791            WHEN PC-IBGR                                           ELGACPCC
00792               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELGACPCC
00793               SET SW-HAS-IBGR                                     ELGACPCC
00794                TO TRUE                                            ELGACPCC
00795            WHEN PC-IDGD                                           ELGACPCC
00796               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELGACPCC
00797               SET SW-HAS-IDGD                                     ELGACPCC
00798                TO TRUE                                            ELGACPCC
00799            WHEN PC-IPGP                                           ELGACPCC
00800               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELGACPCC
00801               SET SW-HAS-IPGP                                     ELGACPCC
00802                TO TRUE                                            ELGACPCC
00803            WHEN PC-IPGN                                           ELGACPCC
00804               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELGACPCC
00805               SET SW-HAS-IPGN                                     ELGACPCC
00806                TO TRUE                                            ELGACPCC
00807            WHEN PC-IPGT                                           ELGACPCC
00808               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELGACPCC
00809               SET SW-HAS-IPGT                                     ELGACPCC
00810                TO TRUE                                            ELGACPCC
00811            WHEN PC-IPGS                                           ELGACPCC
00812               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELGACPCC
00813               SET SW-HAS-IPGS                                     ELGACPCC
00814                TO TRUE                                            ELGACPCC
00815            WHEN OTHER                                             ELGACPCC
00816               CONTINUE                                            ELGACPCC
00817            END-EVALUATE                                           ELGACPCC
00818      END-IF.                                                      ELGACPCC
00819                                                                   ELGACPCC
00820 /***********************************************************      ELGACPCC
00821 *                                                          *      ELGACPCC
00822 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELGACPCC
00823 *                                                          *      ELGACPCC
00824 ************************************************************      ELGACPCC
00825                                                                   ELGACPCC
00826  0330-INIT-ACCUM-EXTRACT.                                         ELGACPCC
00827      INITIALIZE ACCUM-FIXED-AREA.                                 ELGACPCC
00828      SET ACCUM-ACP TO TRUE.                                       ELGACPCC
00829      MOVE 1 TO  ACCUM-ASCEND-DESCEND-COUNT.                       ELGACPCC
00830      SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELGACPCC
00831      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELGACPCC
00832                                                                   ELGACPCC
00833 /***********************************************************      ELGACPCC
00834 *                                                          *      ELGACPCC
00835 *        SUMMARIZE ADL TOPIC LEVEL DATA ELEMENTS           *      ELGACPCC
00836 *                                                          *      ELGACPCC
00837 ************************************************************      ELGACPCC
00838                                                                   ELGACPCC
00839  0340-EXTRACT-ACCUM.                                              ELGACPCC
00840      MOVE GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)                   ELGACPCC
00841        TO ACCUM-COPAY-TAD-IND (COPAY-INDEX)                       ELGACPCC
00842      MOVE GAF-COPAY-FYI-VALUE (GAF-INDEX) TO ACCUM-FYI-VALUE.     ELGACPCC
00843      MOVE GAF-COPAY-COST-CONTAIN-IND (GAF-INDEX)                  ELGACPCC
00844        TO ACCUM-COST-CONTAIN-IND.                                 ELGACPCC
00845      MOVE GAF-COPAY-PLACE-OF-TREATMENT (GAF-INDEX)                ELGACPCC
00846        TO ACCUM-PLACE-OF-TREATMENT.                               ELGACPCC
00847      MOVE GAF-COPAY-BENEFIT-PERIOD (GAF-INDEX)                    ELGACPCC
00848        TO ACCUM-BENEFIT-PERIOD.                                   ELGACPCC
00849      MOVE GAF-COPAY-BEN-PER-TIME-FCTR (GAF-INDEX)                 ELGACPCC
00850        TO ACCUM-BEN-PER-TIME-FCTR.                                ELGACPCC
00851      MOVE GAF-COPAY-BEN-PER-TIME-QUAL (GAF-INDEX)                 ELGACPCC
00852        TO ACCUM-BEN-PER-TIME-QUAL.                                ELGACPCC
00853      MOVE GAF-COPAY-INTERVAL-TIME-FCTR (GAF-INDEX)                ELGACPCC
00854        TO ACCUM-INTERVAL-TIME-FCTR.                               ELGACPCC
00855      MOVE GAF-COPAY-INTERVAL-TYPE (GAF-INDEX)                     ELGACPCC
00856        TO ACCUM-INTERVAL-TYPE.                                    ELGACPCC
00857      MOVE GAF-COPAY-INTERVAL-OVRD-IND (GAF-INDEX)                 ELGACPCC
00858        TO ACCUM-INTERVAL-OVRD-IND.                                ELGACPCC
00859      MOVE GAF-COPAY-INTERVAL-OVRD-VALUE (GAF-INDEX)               ELGACPCC
00860        TO ACCUM-INTERVAL-OVRD-VALUE.                              ELGACPCC
00861      MOVE GAF-COPAY-L-O-B (GAF-INDEX) TO ACCUM-L-O-B.             ELGACPCC
00862      EVALUATE TRUE ALSO TRUE                                      ELGACPCC
00863         WHEN      SW-INTRNL-INST-PROV-CLASS                       ELGACPCC
00864              ALSO SW-INTRNL-PROF-PROV-CLASS                       ELGACPCC
00865            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELGACPCC
00866         WHEN      SW-INTRNL-INST-PROV-CLASS                       ELGACPCC
00867              ALSO SW-INTRNL-NOT-PROF-PROV-CLASS                   ELGACPCC
00868            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELGACPCC
00869         WHEN      SW-INTRNL-NOT-INST-PROV-CLASS                   ELGACPCC
00870              ALSO SW-INTRNL-PROF-PROV-CLASS                       ELGACPCC
00871            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELGACPCC
00872         WHEN OTHER                                                ELGACPCC
00873            CONTINUE                                               ELGACPCC
00874         END-EVALUATE.                                             ELGACPCC
00875      EVALUATE TRUE ALSO TRUE                                      ELGACPCC
00876         WHEN      SW-INTRNL-INST-PROV-SPEC                        ELGACPCC
00877              ALSO SW-INTRNL-PROF-PROV-SPEC                        ELGACPCC
00878            SET ACCUM-PRVDR-SPC-ALL TO TRUE                        ELGACPCC
00879         WHEN      SW-INTRNL-INST-PROV-SPEC                        ELGACPCC
00880              ALSO SW-INTRNL-NOT-PROF-PROV-SPEC                    ELGACPCC
00881            SET ACCUM-PRVDR-SPC-INST TO TRUE                       ELGACPCC
00882         WHEN      SW-INTRNL-NOT-INST-PROV-SPEC                    ELGACPCC
00883              ALSO SW-INTRNL-PROF-PROV-SPEC                        ELGACPCC
00884            SET ACCUM-PRVDR-SPC-PROF TO TRUE                       ELGACPCC
00885         WHEN OTHER                                                ELGACPCC
00886            CONTINUE                                               ELGACPCC
00887         END-EVALUATE.                                             ELGACPCC
00888      MOVE GAF-COPAY-DEFINITION (GAF-INDEX) TO ACCUM-DEFINITION.   ELGACPCC
00889      SET CARRY-OVER-CREDIT-IND-NA                                 ELGACPCC
00890          ASCEND-DESCEND-IND-NA                                    ELGACPCC
00891       TO TRUE.                                                    ELGACPCC
00892      MOVE GAF-COPAY-CONDITION (GAF-INDEX) TO ACCUM-CONDITION.     ELGACPCC
00893      MOVE GAF-COPAY-FAM-OR-INDIV (GAF-INDEX)                      ELGACPCC
00894        TO ACCUM-FAM-OR-INDIV.                                     ELGACPCC
00895      MOVE ZEROS TO ACCUM-DED-BASE-AMT-SOURCE-IND.                 ELGACPCC
00896      MOVE ZEROS TO ACCUM-OPX-BASE-AMT-SOURCE-IND.                 ELGACPCC
00897      MOVE GCG-MAX-BASE-AMT-SOURCE-IND                             ELGACPCC
00898        TO ACCUM-MAX-BASE-AMT-SOURCE-IND.                          ELGACPCC
00899      MOVE GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX)                   ELGACPCC
00900        TO ACCUM-VALUE-QUALIFIER.                                  ELGACPCC
00901      MOVE GAF-COPAY-RELATIONSHIP-IND (GAF-INDEX)                  ELGACPCC
00902        TO ACCUM-RELATIONSHIP-IND.                                 ELGACPCC
00903      MOVE GAF-COPAY-AGE-LIMIT-FROM (GAF-INDEX)                    ELGACPCC
00904        TO ACCUM-AGE-LIMIT-FROM-VAL.                               ELGACPCC
00905      MOVE GAF-COPAY-AGE-QUAL-IND-FROM (GAF-INDEX)                 ELGACPCC
00906        TO ACCUM-AGE-LIMIT-FROM-IND.                               ELGACPCC
00907      MOVE GAF-COPAY-AGE-LIMIT-TO (GAF-INDEX)                      ELGACPCC
00908        TO ACCUM-AGE-LIMIT-TO-VAL.                                 ELGACPCC
00909      MOVE GAF-COPAY-AGE-QUAL-IND-TO (GAF-INDEX)                   ELGACPCC
00910        TO ACCUM-AGE-LIMIT-TO-IND.                                 ELGACPCC
00911      SET LMT-MANDATORY-IND-NA TO TRUE.                            ELGACPCC
00912      MOVE GAF-COPAY-CO-PAY-IND (GAF-INDEX)                        ELGACPCC
00913        TO ACCUM-CO-PAY-IND (COPAY-INDEX)                          ELGACPCC
00914      MOVE GAF-COPAY-SERVICE-GROUP (GAF-INDEX)                     ELGACPCC
00915        TO ACCUM-SERVICE-GROUP.                                    ELGACPCC
00916      MOVE GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX)               ELGACPCC
00917        TO ACCUM-INTERNAL-DESCRIPTOR.                              ELGACPCC
00918      MOVE GAF-COPAY-DAY-FACTOR-IND (GAF-INDEX)                    ELGACPCC
00919        TO ACCUM-DAY-FACTOR-IND.                                   ELGACPCC
00920      MOVE GAF-COPAY-CLAIM-LVL-ACCUM-IND (GAF-INDEX)               ELGACPCC
00921        TO ACCUM-CLAIM-LVL-ACCUM-IND.                              ELGACPCC
00922      SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE.                          ELGACPCC
00923                                                                   ELGACPCC
00924      MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                       ELGACPCC
00925        TO ACCUM-VALUE-LIMIT (1).                                  ELGACPCC
00926      MOVE WS-IBGR-SLOT-NBR TO ACCUM-IBGR-SLOT-NBR (1).            ELGACPCC
00927      MOVE WS-IDGD-SLOT-NBR TO ACCUM-IDGD-SLOT-NBR (1).            ELGACPCC
00928      MOVE WS-IPGN-SLOT-NBR TO ACCUM-IPGN-SLOT-NBR (1).            ELGACPCC
00929      MOVE WS-IPGP-SLOT-NBR TO ACCUM-IPGP-SLOT-NBR (1).            ELGACPCC
00930      MOVE WS-IPGT-SLOT-NBR TO ACCUM-IPGT-SLOT-NBR (1).            ELGACPCC
00931      MOVE WS-IPGS-SLOT-NBR TO ACCUM-IPGS-SLOT-NBR (1).            ELGACPCC
00932                                                                   ELGACPCC
00933 /***********************************************************      ELGACPCC
00934 *                                                          *      ELGACPCC
00935 *    LOAD VARIABLE DATA FOR RELATED PAIRS                  *      ELGACPCC
00936 *12/98 ONLY ADDING 00 DEF AND 00 TAD FOR FIRST INSTALLATION*      ELGACPCC
00937 *COPAY INDEX IS RESET WHERE THERE ARE NO PAIRS,IT IS NOT   *      ELGACPCC
00938 * INCREMENTED.                                             *      ELGACPCC
00939 ************************************************************      ELGACPCC
00940  0375-VARIABLE-DATA.                                              ELGACPCC
00941 *     MOVE GAF-ENTRY-COUNT TO ACCUM-COPAY-COUNT                   ELGACPCC
00942       IF FIRST-ACP                                                ELGACPCC
00943          SET COPAY-INDEX TO 1                                     ELGACPCC
00944          SET NOT-FIRST-ACP TO TRUE                                ELGACPCC
00945       END-IF.                                                     ELGACPCC
00946       EVALUATE TRUE                                               ELGACPCC
00947         WHEN GAF-COPAY-DEFINITION (GAF-INDEX) = '00'              ELGACPCC
00948              IF GAF-COPAY-TIME-DOLLAR-IND(GAF-INDEX) = '00'       ELGACPCC
00949                 SET COPAY-INDEX TO 1                              ELGACPCC
00950                 PERFORM 0480-LOAD-VARIABLE-FIELDS                 ELGACPCC
00951                 SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)    ELGACPCC
00952                   TO TRUE                                         ELGACPCC
00953                 SET WS-HOLD-IDX TO GAF-INDEX                      ELGACPCC
00954              ELSE                                                 ELGACPCC
00955                 CONTINUE                                          ELGACPCC
00956 *               PERFORM 0480-LOAD-VARIABLE-FIELDS                 ELGACPCC
00957 *               SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)    ELGACPCC
00958 *                 TO TRUE                                         ELGACPCC
00959 *               PERFORM 0400-LOAD-CHAINED-OCCURS                  ELGACPCC
00960              END-IF                                               ELGACPCC
00961       END-EVALUATE.                                               ELGACPCC
00962                                                                   ELGACPCC
00963 /***********************************************************      ELGACPCC
00964 *                                                          *      ELGACPCC
00965 *    CHECK EXTRACT DATA INTEGRITY                          *      ELGACPCC
00966 *                                                          *      ELGACPCC
00967 ************************************************************      ELGACPCC
00968                                                                   ELGACPCC
00969  0410-CHK-EXTRACT-DATA-INTGRTY.                                   ELGACPCC
00970      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELGACPCC
00971      THEN                                                         ELGACPCC
00972         SET FYI-VALUE-NA TO TRUE                                  ELGACPCC
00973      END-IF.                                                      ELGACPCC
00974                                                                   ELGACPCC
00975      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELGACPCC
00976      THEN                                                         ELGACPCC
00977         SET COST-CONTAIN-IND-NA TO TRUE                           ELGACPCC
00978      END-IF.                                                      ELGACPCC
00979                                                                   ELGACPCC
00980      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELGACPCC
00981      THEN                                                         ELGACPCC
00982         SET PLACE-OF-TREATMENT-NA TO TRUE                         ELGACPCC
00983      END-IF.                                                      ELGACPCC
00984                                                                   ELGACPCC
00985      IF ACCUM-BENEFIT-PERIOD = ZEROS OR SPACES OR LOW-VALUES      ELGACPCC
00986      THEN                                                         ELGACPCC
00987         SET BENEFIT-PERIOD-NA TO TRUE                             ELGACPCC
00988      END-IF.                                                      ELGACPCC
00989                                                                   ELGACPCC
00990      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELGACPCC
00991      THEN                                                         ELGACPCC
00992         SET INTERVAL-OVRD-IND-NA TO TRUE                          ELGACPCC
00993      END-IF.                                                      ELGACPCC
00994                                                                   ELGACPCC
00995      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELGACPCC
00996      THEN                                                         ELGACPCC
00997         SET L-O-B-NA TO TRUE                                      ELGACPCC
00998      END-IF.                                                      ELGACPCC
00999                                                                   ELGACPCC
01000      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELGACPCC
01001      THEN                                                         ELGACPCC
01002         SET REINSTATEMENT-IND-NA TO TRUE                          ELGACPCC
01003      END-IF.                                                      ELGACPCC
01004                                                                   ELGACPCC
01005      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELGACPCC
01006      THEN                                                         ELGACPCC
01007         SET DEFINITION-NA TO TRUE                                 ELGACPCC
01008      END-IF.                                                      ELGACPCC
01009                                                                   ELGACPCC
01010      IF   ACCUM-CARRY-OVER-CREDIT-IND                             ELGACPCC
01011         = ZEROS OR SPACES OR LOW-VALUES                           ELGACPCC
01012      THEN                                                         ELGACPCC
01013         SET CARRY-OVER-CREDIT-IND-NA TO TRUE                      ELGACPCC
01014      END-IF.                                                      ELGACPCC
01015                                                                   ELGACPCC
01016      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELGACPCC
01017      THEN                                                         ELGACPCC
01018         SET ASCEND-DESCEND-IND-NA TO TRUE                         ELGACPCC
01019      END-IF.                                                      ELGACPCC
01020                                                                   ELGACPCC
01021      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELGACPCC
01022      THEN                                                         ELGACPCC
01023         SET RELATIONSHIP-IND-NA TO TRUE                           ELGACPCC
01024      END-IF.                                                      ELGACPCC
01025                                                                   ELGACPCC
01026      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELGACPCC
01027      THEN                                                         ELGACPCC
01028         SET AGE-LMT-TO-IND-NA TO TRUE                             ELGACPCC
01029      END-IF.                                                      ELGACPCC
01030                                                                   ELGACPCC
01031      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELGACPCC
01032      THEN                                                         ELGACPCC
01033         SET AGE-LMT-FROM-IND-NA TO TRUE                           ELGACPCC
01034      END-IF.                                                      ELGACPCC
01035                                                                   ELGACPCC
01036      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELGACPCC
01037      THEN                                                         ELGACPCC
01038         SET LMT-MANDATORY-IND-NA TO TRUE                          ELGACPCC
01039      END-IF.                                                      ELGACPCC
01040                                                                   ELGACPCC
01041      IF ACCUM-CO-PAY-IND (COPAY-INDEX)                            ELGACPCC
01042                          = ZEROS OR SPACES OR LOW-VALUES          ELGACPCC
01043         SET CO-PAY-IND-NA (COPAY-INDEX) TO TRUE                   ELGACPCC
01044      END-IF.                                                      ELGACPCC
01045                                                                   ELGACPCC
01046      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELGACPCC
01047      THEN                                                         ELGACPCC
01048         SET SERVICE-GROUP-NA TO TRUE                              ELGACPCC
01049      END-IF.                                                      ELGACPCC
01050                                                                   ELGACPCC
01051      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELGACPCC
01052      THEN                                                         ELGACPCC
01053         SET INTERNAL-DESCRIPTOR-NA TO TRUE                        ELGACPCC
01054      END-IF.                                                      ELGACPCC
01055                                                                   ELGACPCC
01056      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELGACPCC
01057      THEN                                                         ELGACPCC
01058         SET DAY-FACTOR-IND-NA TO TRUE                             ELGACPCC
01059      END-IF.                                                      ELGACPCC
01060                                                                   ELGACPCC
01061      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELGACPCC
01062      THEN                                                         ELGACPCC
01063         SET CLAIM-LVL-ACCUM-IND-NA TO TRUE                        ELGACPCC
01064      END-IF.                                                      ELGACPCC
01065                                                                   ELGACPCC
01066      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELGACPCC
01067      THEN                                                         ELGACPCC
01068         SET BEN-PER-MAX-OVRD-IND-NA TO TRUE                       ELGACPCC
01069      END-IF.                                                      ELGACPCC
01070                                                                   ELGACPCC
01071      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELGACPCC
01072      THEN                                                         ELGACPCC
01073         SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE                        ELGACPCC
01074      END-IF.                                                      ELGACPCC
01075                                                                   ELGACPCC
01076 /***********************************************************      ELGACPCC
01077 *                                                          *      ELGACPCC
01078 *    LOAD VARIABLE-FIELDS                                  *      ELGACPCC
01079 *                                                          *      ELGACPCC
01080 ************************************************************      ELGACPCC
01081    0480-LOAD-VARIABLE-FIELDS.                                     ELGACPCC
01082         MOVE GAF-COPAY-BENEFIT-PERIOD(GAF-INDEX) TO               ELGACPCC
01083            ACCUM-COPAY-BEN-PER(COPAY-INDEX).                      ELGACPCC
01084         MOVE GAF-COPAY-DEFINITION(GAF-INDEX) TO                   ELGACPCC
01085            ACCUM-COPAY-DEFINITION(COPAY-INDEX).                   ELGACPCC
01086         MOVE GAF-COPAY-VALUE-QUALIFIER(GAF-INDEX) TO              ELGACPCC
01087            ACCUM-COPAY-VALUE-QUALIFIER(COPAY-INDEX).              ELGACPCC
01088         MOVE GAF-COPAY-VALUE-LIMIT(GAF-INDEX) TO                  ELGACPCC
01089            ACCUM-COPAY-VALUE-LIMIT(COPAY-INDEX).                  ELGACPCC
01090         MOVE GAF-COPAY-TIME-DOLLAR-IND(GAF-INDEX) TO              ELGACPCC
01091            ACCUM-COPAY-TAD-IND(COPAY-INDEX).                      ELGACPCC
01092         MOVE GAF-COPAY-CO-PAY-IND(GAF-INDEX) TO                   ELGACPCC
01093              ACCUM-CO-PAY-IND(COPAY-INDEX).                       ELGACPCC
01094         SET COPAY-INDEX UP BY 1.                                  ELGACPCC
01095         ADD +1 TO ACCUM-COPAY-COUNT.                              ELGACPCC
01096                                                                   ELGACPCC
01097 /*****************************************************************ELGACPCC
01098 *                                                                *ELGACPCC
01099 *    CHECK COST CONTAINMENT INDICATOR TO DETERMINE APPLICABILITY *ELGACPCC
01100 *                                                                *ELGACPCC
01101 ******************************************************************ELGACPCC
01102                                                                   ELGACPCC
01103  0500-CHK-CC-IND.                                                 ELGACPCC
01104      MOVE SSB-MODIFIER-1 TO WS-SUBTOPIC.                          ELGACPCC
01105      EVALUATE TRUE              ALSO TRUE                         ELGACPCC
01106         WHEN WS-SUBTOPIC-ATCP   ALSO WS-CCI-ATCP                  ELGACPCC
01107            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01108         WHEN WS-SUBTOPIC-BAE   ALSO WS-CCI-BAE                    ELGACPCC
01109            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01110         WHEN WS-SUBTOPIC-EMH    ALSO WS-CCI-EMH                   ELGACPCC
01111            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01112         WHEN WS-SUBTOPIC-HOSP   ALSO WS-CCI-HOSP                  ELGACPCC
01113            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01114         WHEN WS-SUBTOPIC-IOB    ALSO WS-CCI-IOB                   ELGACPCC
01115            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01116         WHEN WS-SUBTOPIC-MASOP  ALSO WS-CCI-MASOP                 ELGACPCC
01117            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01118         WHEN WS-SUBTOPIC-MCN    ALSO WS-CCI-MCN                   ELGACPCC
01119            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01120         WHEN WS-SUBTOPIC-MEDNEC ALSO WS-CCI-MEDNEC                ELGACPCC
01121            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01122         WHEN WS-SUBTOPIC-MHSC   ALSO WS-CCI-MHSC                  ELGACPCC
01123            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01124         WHEN WS-SUBTOPIC-MOND   ALSO WS-CCI-MOND                  ELGACPCC
01125            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01126         WHEN WS-SUBTOPIC-MOPS   ALSO WS-CCI-MOPS                  ELGACPCC
01127            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01128         WHEN WS-SUBTOPIC-MSA    ALSO WS-CCI-MSA                   ELGACPCC
01129            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01130         WHEN WS-SUBTOPIC-PAR    ALSO WS-CCI-PAR                   ELGACPCC
01131            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01132         WHEN WS-SUBTOPIC-PAT    ALSO WS-CCI-PAT                   ELGACPCC
01133            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01134         WHEN WS-SUBTOPIC-POS    ALSO WS-CCI-POS                   ELGACPCC
01135            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01136         WHEN WS-SUBTOPIC-PPO    ALSO WS-CCI-PPO                   ELGACPCC
01137            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01138         WHEN WS-SUBTOPIC-REIMB  ALSO WS-CCI-REIMB                 ELGACPCC
01139            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01140         WHEN WS-SUBTOPIC-RPO  ALSO WS-CCI-RPO                     ELGACPCC
01141            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01142         WHEN WS-SUBTOPIC-CPO  ALSO WS-CCI-CPO                     ELGACPCC
01143            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01144         WHEN WS-SUBTOPIC-CBL  ALSO WS-CCI-CBL                     ELGACPCC
01145            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01146         WHEN WS-SUBTOPIC-PAN  ALSO WS-CCI-PAN                     ELGACPCC
01147            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01148         WHEN WS-SUBTOPIC-WEEK   ALSO WS-CCI-WEEK                  ELGACPCC
01149            SET SW-CC-IND-APPLIES TO TRUE                          ELGACPCC
01150         WHEN OTHER                                                ELGACPCC
01151            CONTINUE                                               ELGACPCC
01152         END-EVALUATE.                                             ELGACPCC
01153                                                                   ELGACPCC
01154 /***********************************************************      ELGACPCC
01155 *                                                          *      ELGACPCC
01156 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER CLASS   *      ELGACPCC
01157 *                                                          *      ELGACPCC
01158 ************************************************************      ELGACPCC
01159                                                                   ELGACPCC
01160  0550-CHK-INTRNL-TAB-PROV-CLASS.                                  ELGACPCC
01161      IF SW-HAS-IPGT                                               ELGACPCC
01162      THEN                                                         ELGACPCC
01163         PERFORM 0560-CHK-IPGT-PROV-CLASS                          ELGACPCC
01164      ELSE                                                         ELGACPCC
01165         IF SW-HAS-IBGR                                            ELGACPCC
01166         THEN                                                      ELGACPCC
01167            PERFORM 0640-CHK-IBGR-PROV-CLASS                       ELGACPCC
01168         ELSE                                                      ELGACPCC
01169            SET SW-OCCRNC-APPLIES TO TRUE                          ELGACPCC
01170         END-IF                                                    ELGACPCC
01171      END-IF.                                                      ELGACPCC
01172 /***********************************************************      ELGACPCC
01173 *                                                          *      ELGACPCC
01174 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER SPEC    *      ELGACPCC
01175 *                                                          *      ELGACPCC
01176 ************************************************************      ELGACPCC
01177                                                                   ELGACPCC
01178  0551-CHK-INTRNL-TAB-PROV-SPEC.                                   ELGACPCC
01179      IF SW-HAS-IPGS                                               ELGACPCC
01180         PERFORM 0561-CHK-IPGS-PROV-SPEC                           ELGACPCC
01181 *    ELSE                                                         ELGACPCC
01182 *       IF SW-HAS-IBGR                                            ELGACPCC
01183 *          PERFORM 0640-CHK-IBGR-PROV-CLASS                       ELGACPCC
01184 *       ELSE                                                      ELGACPCC
01185 *          SET SW-OCCRNC-APPLIES TO TRUE                          ELGACPCC
01186 *       END-IF                                                    ELGACPCC
01187      END-IF.                                                      ELGACPCC
01188                                                                   ELGACPCC
01189 /*****************************************************************ELGACPCC
01190 *                                                                *ELGACPCC
01191 *    CHECK IPGT INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELGACPCC
01192 *                                                                *ELGACPCC
01193 ******************************************************************ELGACPCC
01194                                                                   ELGACPCC
01195  0560-CHK-IPGT-PROV-CLASS.                                        ELGACPCC
01196      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELGACPCC
01197      MOVE PC-IPGT TO KWA-PROVISION-ID.                            ELGACPCC
01198      MOVE WS-IPGT-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGACPCC
01199      PERFORM 0700-READ-INTRNL-TAB.                                ELGACPCC
01200      SET ADDRESS OF GX3-RECORD-AREA TO IOP-REC-PTR.               ELGACPCC
01201      SET IOP-REC-PTR TO NULLS.                                    ELGACPCC
01202      SET GX3-INDEX TO GX3-ENTRY-COUNT.                            ELGACPCC
01203      SET WS-MAX-GX3-INDEX TO GX3-INDEX.                           ELGACPCC
01204                                                                   ELGACPCC
01205      IF GX3-ID-ARGUMENT-INCLUDED                                  ELGACPCC
01206      THEN                                                         ELGACPCC
01207         PERFORM 0570-CHK-INCLD-TYPE-IPGT                          ELGACPCC
01208      ELSE                                                         ELGACPCC
01209          PERFORM 0600-CHK-EXCLD-TYPE-IPGT                         ELGACPCC
01210      END-IF.                                                      ELGACPCC
01211                                                                   ELGACPCC
01212 /*****************************************************************ELGACPCC
01213 *                                                                *ELGACPCC
01214 *    CHECK IPGS INTERNAL TABULAR TO DETERMINE PROVIDER SPEC      *ELGACPCC
01215 *                                                                *ELGACPCC
01216 ******************************************************************ELGACPCC
01217                                                                   ELGACPCC
01218  0561-CHK-IPGS-PROV-SPEC.                                         ELGACPCC
01219      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELGACPCC
01220      MOVE PC-IPGS TO KWA-PROVISION-ID.                            ELGACPCC
01221      MOVE WS-IPGS-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGACPCC
01222      PERFORM 0700-READ-INTRNL-TAB.                                ELGACPCC
01223      SET ADDRESS OF GXS-RECORD-AREA TO IOP-REC-PTR.               ELGACPCC
01224      SET IOP-REC-PTR TO NULLS.                                    ELGACPCC
01225      SET GXS-INDEX TO GX3-ENTRY-COUNT.                            ELGACPCC
01226      SET WS-MAX-GXS-INDEX TO GXS-INDEX.                           ELGACPCC
01227                                                                   ELGACPCC
01228      IF GXS-ID-ARGUMENT-INCLUDED                                  ELGACPCC
01229      THEN                                                         ELGACPCC
01230         PERFORM 0571-CHK-INCLD-TYPE-IPGS                          ELGACPCC
01231      ELSE                                                         ELGACPCC
01232          PERFORM 0601-CHK-EXCLD-TYPE-IPGS                         ELGACPCC
01233      END-IF.                                                      ELGACPCC
01234                                                                   ELGACPCC
01235 ************************************************************      ELGACPCC
01236 *                                                          *      ELGACPCC
01237 *    CHECK INCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGACPCC
01238 *                                                          *      ELGACPCC
01239 ************************************************************      ELGACPCC
01240                                                                   ELGACPCC
01241  0570-CHK-INCLD-TYPE-IPGT.                                        ELGACPCC
01242      SET CFT2-IDX TO 1.                                           ELGACPCC
01243      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGACPCC
01244          SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGACPCC
01245       TO TRUE.                                                    ELGACPCC
01246      PERFORM 0580-TEST-IPGT-INCLD-ENTRIES                         ELGACPCC
01247         VARYING GX3-INDEX  FROM 1 BY 1                            ELGACPCC
01248           UNTIL    GX3-INDEX = WS-MAX-GX3-INDEX                   ELGACPCC
01249                 OR (    SW-INTRNL-INST-PROV-CLASS                 ELGACPCC
01250                     AND SW-INTRNL-PROF-PROV-CLASS ).              ELGACPCC
01251                                                                   ELGACPCC
01252 ************************************************************      ELGACPCC
01253 *                                                          *      ELGACPCC
01254 *    CHECK INCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELGACPCC
01255 *                                                          *      ELGACPCC
01256 ************************************************************      ELGACPCC
01257                                                                   ELGACPCC
01258  0571-CHK-INCLD-TYPE-IPGS.                                        ELGACPCC
01259      SET CFT2-IDX TO 1.                                           ELGACPCC
01260      SET SW-INTRNL-NOT-INST-PROV-SPEC                             ELGACPCC
01261          SW-INTRNL-NOT-PROF-PROV-SPEC                             ELGACPCC
01262       TO TRUE.                                                    ELGACPCC
01263      PERFORM 0581-TEST-IPGS-INCLD-ENTRIES                         ELGACPCC
01264         VARYING GXS-INDEX  FROM 1 BY 1                            ELGACPCC
01265           UNTIL    GXS-INDEX = WS-MAX-GXS-INDEX                   ELGACPCC
01266                 OR (    SW-INTRNL-INST-PROV-SPEC                  ELGACPCC
01267                     AND SW-INTRNL-PROF-PROV-SPEC ).               ELGACPCC
01268                                                                   ELGACPCC
01269 ************************************************************      ELGACPCC
01270 *                                                          *      ELGACPCC
01271 *    TEST IPGT INCLUDE ENTRIES TO DETERMINE PROVIDER CLASS *      ELGACPCC
01272 *                                                          *      ELGACPCC
01273 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELGACPCC
01274 *          CFT2 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELGACPCC
01275 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELGACPCC
01276 *          OF THE IPGT TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELGACPCC
01277 *                                                          *      ELGACPCC
01278 ************************************************************      ELGACPCC
01279                                                                   ELGACPCC
01280  0580-TEST-IPGT-INCLD-ENTRIES.                                    ELGACPCC
01281      PERFORM WITH TEST BEFORE                                     ELGACPCC
01282         UNTIL    SW-OCCRNC-APPLIES                                ELGACPCC
01283               OR   CFT2-PT (CFT2-IDX)                             ELGACPCC
01284                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELGACPCC
01285               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELGACPCC
01286         IF   GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELGACPCC
01287            = CFT2-PT (CFT2-IDX)                                   ELGACPCC
01288         THEN                                                      ELGACPCC
01289 *    -- TEST PROVIDER CLASS                                       ELGACPCC
01290            EVALUATE TRUE                                          ELGACPCC
01291               WHEN CFT2-PT-INST (CFT2-IDX)                        ELGACPCC
01292                  SET SW-INTRNL-INST-PROV-CLASS TO TRUE            ELGACPCC
01293                  IF SRP-ACCUM-PROV-CLASS-INST                     ELGACPCC
01294                  THEN                                             ELGACPCC
01295                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGACPCC
01296                  END-IF                                           ELGACPCC
01297               WHEN CFT2-PT-PROF (CFT2-IDX)                        ELGACPCC
01298                  SET SW-INTRNL-PROF-PROV-CLASS TO TRUE            ELGACPCC
01299                  IF SRP-ACCUM-PROV-CLASS-PROF                     ELGACPCC
01300                  THEN                                             ELGACPCC
01301                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGACPCC
01302                  END-IF                                           ELGACPCC
01303               END-EVALUATE                                        ELGACPCC
01304         END-IF                                                    ELGACPCC
01305         SET CFT2-IDX UP BY 1                                      ELGACPCC
01306         END-PERFORM.                                              ELGACPCC
01307                                                                   ELGACPCC
01308 ************************************************************      ELGACPCC
01309 *                                                          *      ELGACPCC
01310 *    TEST IPGS INCLUDE ENTRIES TO DETERMINE PROVIDER SPEC  *      ELGACPCC
01311 *                                                          *      ELGACPCC
01312 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELGACPCC
01313 *          CFT9 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELGACPCC
01314 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELGACPCC
01315 *          OF THE IPGS TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELGACPCC
01316 *                                                          *      ELGACPCC
01317 ************************************************************      ELGACPCC
01318                                                                   ELGACPCC
01319  0581-TEST-IPGS-INCLD-ENTRIES.                                    ELGACPCC
01320      PERFORM WITH TEST BEFORE                                     ELGACPCC
01321         UNTIL    SW-OCCRNC-APPLIES                                ELGACPCC
01322               OR   CFT9-PT (CFT2-IDX)                             ELGACPCC
01323                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELGACPCC
01324               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELGACPCC
01325         IF   GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELGACPCC
01326            = CFT9-PT (CFT9-IDX)                                   ELGACPCC
01327 *    -- TEST PROVIDER SPEC                                        ELGACPCC
01328            EVALUATE TRUE                                          ELGACPCC
01329               WHEN CFT9-PT-INST (CFT9-IDX)                        ELGACPCC
01330                  SET SW-INTRNL-INST-PROV-SPEC TO TRUE             ELGACPCC
01331                  IF SRP-ACCUM-PROV-SPEC-INST                      ELGACPCC
01332                  THEN                                             ELGACPCC
01333                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGACPCC
01334                  END-IF                                           ELGACPCC
01335               WHEN CFT9-PT-PROF (CFT9-IDX)                        ELGACPCC
01336                  SET SW-INTRNL-PROF-PROV-SPEC TO TRUE             ELGACPCC
01337                  IF SRP-ACCUM-PROV-SPEC-PROF                      ELGACPCC
01338                  THEN                                             ELGACPCC
01339                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGACPCC
01340                  END-IF                                           ELGACPCC
01341               END-EVALUATE                                        ELGACPCC
01342         END-IF                                                    ELGACPCC
01343         SET CFT9-IDX UP BY 1                                      ELGACPCC
01344         END-PERFORM.                                              ELGACPCC
01345                                                                   ELGACPCC
01346 /***********************************************************      ELGACPCC
01347 *                                                          *      ELGACPCC
01348 *    CHECK EXCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGACPCC
01349 *                                                          *      ELGACPCC
01350 ************************************************************      ELGACPCC
01351                                                                   ELGACPCC
01352  0600-CHK-EXCLD-TYPE-IPGT.                                        ELGACPCC
01353                                                                   ELGACPCC
01354 * -- INITIALIZE CFT2 TABLE TO INCLUDE ALL PROVIDER TYPES          ELGACPCC
01355      PERFORM WITH TEST BEFORE                                     ELGACPCC
01356         VARYING CFT2-IDX FROM 1 BY 1                              ELGACPCC
01357           UNTIL CFT2-IDX > CFT2-NBR-TBL-ENTRIES                   ELGACPCC
01358         SET  CFT2-PT-INCLUDE (CFT2-IDX) TO TRUE                   ELGACPCC
01359         END-PERFORM.                                              ELGACPCC
01360                                                                   ELGACPCC
01361 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGT                 ELGACPCC
01362      SET  CFT2-IDX TO 1.                                          ELGACPCC
01363      PERFORM 0610-TAG-EXCLD-IPGT-ENTRIES                          ELGACPCC
01364         VARYING GX3-INDEX FROM 1 BY 1                             ELGACPCC
01365           UNTIL GX3-INDEX = WS-MAX-GX3-INDEX.                     ELGACPCC
01366                                                                   ELGACPCC
01367 * -- CHECK CFT2 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELGACPCC
01368      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGACPCC
01369          SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGACPCC
01370       TO TRUE.                                                    ELGACPCC
01371      PERFORM 0630-CHK-CFT2-NOT-EXCLD                              ELGACPCC
01372         VARYING CFT2-IDX FROM 1 BY 1                              ELGACPCC
01373           UNTIL    (    SW-INTRNL-INST-PROV-CLASS                 ELGACPCC
01374                     AND SW-INTRNL-PROF-PROV-CLASS )               ELGACPCC
01375                 OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES.               ELGACPCC
01376                                                                   ELGACPCC
01377 /***********************************************************      ELGACPCC
01378 *                                                          *      ELGACPCC
01379 *    CHECK EXCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELGACPCC
01380 *                                                          *      ELGACPCC
01381 ************************************************************      ELGACPCC
01382                                                                   ELGACPCC
01383  0601-CHK-EXCLD-TYPE-IPGS.                                        ELGACPCC
01384                                                                   ELGACPCC
01385 * -- INITIALIZE CFT9 TABLE TO INCLUDE ALL PROVIDER SPEC           ELGACPCC
01386      PERFORM WITH TEST BEFORE                                     ELGACPCC
01387         VARYING CFT9-IDX FROM 1 BY 1                              ELGACPCC
01388           UNTIL CFT9-IDX > CFT9-NBR-TBL-ENTRIES                   ELGACPCC
01389         SET  CFT9-PT-INCLUDE (CFT9-IDX) TO TRUE                   ELGACPCC
01390         END-PERFORM.                                              ELGACPCC
01391                                                                   ELGACPCC
01392 * -- TAG ALL PROVIDER                                             ELGACPCC
01393      SET  CFT9-IDX TO 1.                                          ELGACPCC
01394      PERFORM 0611-TAG-EXCLD-IPGS-ENTRIES                          ELGACPCC
01395         VARYING GXS-INDEX FROM 1 BY 1                             ELGACPCC
01396           UNTIL GXS-INDEX = WS-MAX-GXS-INDEX.                     ELGACPCC
01397                                                                   ELGACPCC
01398 * -- CHECK CFT9 TABLE FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED ELGACPCC
01399      SET SW-INTRNL-NOT-INST-PROV-SPEC                             ELGACPCC
01400          SW-INTRNL-NOT-PROF-PROV-SPEC                             ELGACPCC
01401       TO TRUE.                                                    ELGACPCC
01402      PERFORM 0631-CHK-CFT9-NOT-EXCLD                              ELGACPCC
01403         VARYING CFT9-IDX FROM 1 BY 1                              ELGACPCC
01404           UNTIL    (    SW-INTRNL-INST-PROV-SPEC                  ELGACPCC
01405                     AND SW-INTRNL-PROF-PROV-SPEC )                ELGACPCC
01406                 OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES.               ELGACPCC
01407                                                                   ELGACPCC
01408 ************************************************************      ELGACPCC
01409 *                                                          *      ELGACPCC
01410 *    TAG EXCLUDED IPGT ENTRIES IN CFT2                     *      ELGACPCC
01411 *                                                          *      ELGACPCC
01412 ************************************************************      ELGACPCC
01413                                                                   ELGACPCC
01414  0610-TAG-EXCLD-IPGT-ENTRIES.                                     ELGACPCC
01415      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELGACPCC
01416      PERFORM WITH TEST BEFORE                                     ELGACPCC
01417         UNTIL    SW-ENTRY-FOUND                                   ELGACPCC
01418               OR   CFT2-PT (CFT2-IDX)                             ELGACPCC
01419                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELGACPCC
01420               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELGACPCC
01421         IF   CFT2-PT(CFT2-IDX)                                    ELGACPCC
01422            = GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELGACPCC
01423         THEN                                                      ELGACPCC
01424            SET SW-ENTRY-FOUND TO TRUE                             ELGACPCC
01425            SET CFT2-PT-EXCLUDE (CFT2-IDX) TO TRUE                 ELGACPCC
01426         END-IF                                                    ELGACPCC
01427         SET CFT2-IDX UP BY 1                                      ELGACPCC
01428         END-PERFORM.                                              ELGACPCC
01429                                                                   ELGACPCC
01430 ************************************************************      ELGACPCC
01431 *                                                          *      ELGACPCC
01432 *    TAG EXCLUDED IPGS ENTRIES IN CFT9                     *      ELGACPCC
01433 *                                                          *      ELGACPCC
01434 ************************************************************      ELGACPCC
01435                                                                   ELGACPCC
01436  0611-TAG-EXCLD-IPGS-ENTRIES.                                     ELGACPCC
01437      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELGACPCC
01438      PERFORM WITH TEST BEFORE                                     ELGACPCC
01439         UNTIL    SW-ENTRY-FOUND                                   ELGACPCC
01440               OR   CFT9-PT (CFT2-IDX)                             ELGACPCC
01441                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELGACPCC
01442               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELGACPCC
01443         IF   CFT9-PT(CFT9-IDX)                                    ELGACPCC
01444            = GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELGACPCC
01445         THEN                                                      ELGACPCC
01446            SET SW-ENTRY-FOUND TO TRUE                             ELGACPCC
01447            SET CFT9-PT-EXCLUDE (CFT9-IDX) TO TRUE                 ELGACPCC
01448         END-IF                                                    ELGACPCC
01449         SET CFT9-IDX UP BY 1                                      ELGACPCC
01450         END-PERFORM.                                              ELGACPCC
01451                                                                   ELGACPCC
01452 ******************************************************************ELGACPCC
01453 *                                                                *ELGACPCC
01454 *    CHECK CFT2 FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDED     *ELGACPCC
01455 *                                                                *ELGACPCC
01456 ******************************************************************ELGACPCC
01457                                                                   ELGACPCC
01458  0630-CHK-CFT2-NOT-EXCLD.                                         ELGACPCC
01459      IF CFT2-PT-INCLUDE (CFT2-IDX)                                ELGACPCC
01460      THEN                                                         ELGACPCC
01461         EVALUATE TRUE                                             ELGACPCC
01462            WHEN CFT2-PT-INST (CFT2-IDX)                           ELGACPCC
01463               SET SW-INTRNL-INST-PROV-CLASS TO TRUE               ELGACPCC
01464               IF SRP-ACCUM-PROV-CLASS-INST                        ELGACPCC
01465               THEN                                                ELGACPCC
01466                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGACPCC
01467               END-IF                                              ELGACPCC
01468            WHEN CFT2-PT-PROF (CFT2-IDX)                           ELGACPCC
01469               SET SW-INTRNL-PROF-PROV-CLASS TO TRUE               ELGACPCC
01470               IF SRP-ACCUM-PROV-CLASS-PROF                        ELGACPCC
01471               THEN                                                ELGACPCC
01472                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGACPCC
01473               END-IF                                              ELGACPCC
01474            END-EVALUATE                                           ELGACPCC
01475      END-IF.                                                      ELGACPCC
01476                                                                   ELGACPCC
01477 ******************************************************************ELGACPCC
01478 *                                                                *ELGACPCC
01479 *    CHECK CFT9 FOR CLASS(ES) OF PROVIDER SPEC  NOT EXCLUDED     *ELGACPCC
01480 *                                                                *ELGACPCC
01481 ******************************************************************ELGACPCC
01482                                                                   ELGACPCC
01483  0631-CHK-CFT9-NOT-EXCLD.                                         ELGACPCC
01484      IF CFT9-PT-INCLUDE (CFT9-IDX)                                ELGACPCC
01485         EVALUATE TRUE                                             ELGACPCC
01486            WHEN CFT9-PT-INST (CFT9-IDX)                           ELGACPCC
01487               SET SW-INTRNL-INST-PROV-SPEC TO TRUE                ELGACPCC
01488               IF SRP-ACCUM-PROV-SPEC-INST                         ELGACPCC
01489                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGACPCC
01490               END-IF                                              ELGACPCC
01491            WHEN CFT9-PT-PROF (CFT9-IDX)                           ELGACPCC
01492               SET SW-INTRNL-PROF-PROV-SPEC TO TRUE                ELGACPCC
01493               IF SRP-ACCUM-PROV-SPEC-PROF                         ELGACPCC
01494                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGACPCC
01495               END-IF                                              ELGACPCC
01496            END-EVALUATE                                           ELGACPCC
01497      END-IF.                                                      ELGACPCC
01498                                                                   ELGACPCC
01499 /*****************************************************************ELGACPCC
01500 *                                                                *ELGACPCC
01501 *    CHECK IBGR INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELGACPCC
01502 *                                                                *ELGACPCC
01503 ******************************************************************ELGACPCC
01504                                                                   ELGACPCC
01505  0640-CHK-IBGR-PROV-CLASS.                                        ELGACPCC
01506      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGACPCC
01507          SW-INTRNL-NOT-PROF-PROV-CLASS TO TRUE.                   ELGACPCC
01508      MOVE PC-IBGR TO KWA-PROVISION-ID.                            ELGACPCC
01509      MOVE WS-IBGR-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGACPCC
01510      PERFORM 0700-READ-INTRNL-TAB.                                ELGACPCC
01511      SET ADDRESS OF GX1-RECORD-AREA TO IOP-REC-PTR.               ELGACPCC
01512      SET IOP-REC-PTR TO NULLS.                                    ELGACPCC
01513      SET GX1-INDEX TO GX1-ENTRY-COUNT.                            ELGACPCC
01514      SET WS-MAX-GX1-INDEX TO GX1-INDEX.                           ELGACPCC
01515                                                                   ELGACPCC
01516      IF GX1-ID-ARGUMENT-EXCLUDED                                  ELGACPCC
01517      THEN                                                         ELGACPCC
01518 *    -- ASSUME THAT IBGR WOULD NOT EXCLUDE ALL OF ANY PROVIDER    ELGACPCC
01519 *       CLASS (I.E., BOTH TYPES APPLY).                           ELGACPCC
01520         SET SW-OCCRNC-APPLIES                                     ELGACPCC
01521             SW-INTRNL-INST-PROV-CLASS                             ELGACPCC
01522             SW-INTRNL-PROF-PROV-CLASS                             ELGACPCC
01523          TO TRUE                                                  ELGACPCC
01524      ELSE                                                         ELGACPCC
01525         PERFORM 0690-CHK-INCLD-TYPE-IBGR                          ELGACPCC
01526      END-IF.                                                      ELGACPCC
01527                                                                   ELGACPCC
01528 /*****************************************************************ELGACPCC
01529 *                                                                *ELGACPCC
01530 *    CHECK INCLUDE TYPE IBGR TO DETERMINE PROVIDER CLASS         *ELGACPCC
01531 *                                                                *ELGACPCC
01532 ******************************************************************ELGACPCC
01533                                                                   ELGACPCC
01534  0690-CHK-INCLD-TYPE-IBGR.                                        ELGACPCC
01535      PERFORM WITH TEST BEFORE                                     ELGACPCC
01536         VARYING GX1-INDEX FROM 1 BY 1                             ELGACPCC
01537           UNTIL    GX1-INDEX = WS-MAX-GX1-INDEX                   ELGACPCC
01538                 OR (    SW-INTRNL-INST-PROV-CLASS                 ELGACPCC
01539                     AND SW-INTRNL-PROF-PROV-CLASS )               ELGACPCC
01540         MOVE GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)                ELGACPCC
01541           TO WS-PROVISION-ARGUMENT                                ELGACPCC
01542         EVALUATE TRUE                                             ELGACPCC
01543            WHEN INST-CLASS                                        ELGACPCC
01544               SET SW-INTRNL-INST-PROV-CLASS TO TRUE               ELGACPCC
01545               IF SRP-ACCUM-PROV-CLASS-INST                        ELGACPCC
01546               THEN                                                ELGACPCC
01547                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGACPCC
01548               END-IF                                              ELGACPCC
01549            WHEN PROF-CLASS                                        ELGACPCC
01550               SET SW-INTRNL-PROF-PROV-CLASS TO TRUE               ELGACPCC
01551               IF SRP-ACCUM-PROV-CLASS-PROF                        ELGACPCC
01552               THEN                                                ELGACPCC
01553                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGACPCC
01554               END-IF                                              ELGACPCC
01555            END-EVALUATE                                           ELGACPCC
01556         END-PERFORM.                                              ELGACPCC
01557                                                                   ELGACPCC
01558 /***********************************************************      ELGACPCC
01559 *                                                          *      ELGACPCC
01560 *    READ THE INTERNAL TABULAR RECORD                      *      ELGACPCC
01561 *                                                          *      ELGACPCC
01562 ************************************************************      ELGACPCC
01563                                                                   ELGACPCC
01564  0700-READ-INTRNL-TAB.                                            ELGACPCC
01565      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELGACPCC
01566      SET IOP-RD TO TRUE.                                          ELGACPCC
01567      SET IOP-FCQ-NONE TO TRUE.                                    ELGACPCC
01568      SET IOP-KVQ-EQ TO TRUE.                                      ELGACPCC
01569      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELGACPCC
01570      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGACPCC
01571      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGACPCC
01572      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACPCC
01573                                                                   ELGACPCC
01574      EVALUATE TRUE                                                ELGACPCC
01575         WHEN IOP-RC-OK                                            ELGACPCC
01576            CONTINUE                                               ELGACPCC
01577         WHEN IOP-RC-NOTFND                                        ELGACPCC
01578            SET CIA-AB-NOTFND-GCTABULR TO TRUE                     ELGACPCC
01579            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELGACPCC
01580         WHEN OTHER                                                ELGACPCC
01581             SET CIA-AB-CRITIO TO TRUE                             ELGACPCC
01582             EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC           ELGACPCC
01583         END-EVALUATE.                                             ELGACPCC
01584                                                                   ELGACPCC
01585 /***********************************************************      ELGACPCC
01586 *                                                          *      ELGACPCC
01587 *        ADD ACCUM OCCURENCE TO FILE                       *      ELGACPCC
01588 *                                                          *      ELGACPCC
01589 ************************************************************      ELGACPCC
01590                                                                   ELGACPCC
01591  0710-WRITE-EXTRACT-RECORD.                                       ELGACPCC
01592      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGACPCC
01593      SET  IOP-ADD TO TRUE.                                        ELGACPCC
01594      SET  IOP-FCQ-NONE TO TRUE.                                   ELGACPCC
01595      SET  IOP-KVQ-NONE TO TRUE.                                   ELGACPCC
01596      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACPCC
01597                                                                   ELGACPCC
01598 /***********************************************************      ELGACPCC
01599 *                                                          *      ELGACPCC
01600 *    ESTABLISH ADDRESSABILITY OF THE TABULAR FILE          *      ELGACPCC
01601 *                                                          *      ELGACPCC
01602 ************************************************************      ELGACPCC
01603                                                                   ELGACPCC
01604  9060-EST-ADR-TABULAR-FILE.                                       ELGACPCC
01605      SET  CIA-GCTABULR-DDN TO TRUE.                               ELGACPCC
01606      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACPCC
01607         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGACPCC
01608         END-CALL.                                                 ELGACPCC
01609      IF CIA-RC-PTR-NULL                                           ELGACPCC
01610      THEN                                                         ELGACPCC
01611         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACPCC
01612         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGACPCC
01613      END-IF.                                                      ELGACPCC
01614                                                                   ELGACPCC
01615 /***********************************************************      ELGACPCC
01616 *                                                          *      ELGACPCC
01617 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELGACPCC
01618 *                                                          *      ELGACPCC
01619 ************************************************************      ELGACPCC
01620                                                                   ELGACPCC
01621  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELGACPCC
01622      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGACPCC
01623      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACPCC
01624         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGACPCC
01625         END-CALL.                                                 ELGACPCC
01626      IF CIA-RC-PTR-NULL                                           ELGACPCC
01627      THEN                                                         ELGACPCC
01628         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACPCC
01629         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGACPCC
01630      END-IF.                                                      ELGACPCC
