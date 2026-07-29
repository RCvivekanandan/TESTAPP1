00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGABMCC
00003  PROGRAM-ID.        ELGABMCC.                                        LV002
00004                                                                   ELGABMCC
00005  AUTHOR.            LUCY TORRES.                                  ELGABMCC
00006                     RICHARD J. LUKETICH (RE-WRITE).               ELGABMCC
00007                                                                   ELGABMCC
00008  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELGABMCC
00009                     A MUTUAL LEGAL RESERVE COMPANY                ELGABMCC
00010                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELGABMCC
00011                     233 N. MICHIGAN AVE                           ELGABMCC
00012                     CHICAGO, ILLINOIS 60601                       ELGABMCC
00013                                                                   ELGABMCC
00014  DATE-WRITTEN.      03-JUN-1987.                                  ELGABMCC
00015                     07-JAN-1992 (RE-WRITE).                       ELGABMCC
00016                                                                   ELGABMCC
00017  DATE-COMPILED.                                                   ELGABMCC
00018                                                                   ELGABMCC
00019  SECURITY.          COPYRIGHT 1986, 1992,                         ELGABMCC
00020                     HEALTH CARE SERVICE CORPORATION               ELGABMCC
00021                                                                   ELGABMCC
00022  ENVIRONMENT DIVISION.                                            ELGABMCC
00023                                                                   ELGABMCC
00024  CONFIGURATION SECTION.                                           ELGABMCC
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELGABMCC
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELGABMCC
00027                                                                   ELGABMCC
00028 /*****************************************************************ELGABMCC
00029 *                                                                *ELGABMCC
00030 *  ELTABM - ELS:    SELECTS #ABM (MAXIMUM) ACCUMULATORS AND      *ELGABMCC
00031 *                   SETUPS THE INFORMATION TO BE PROCESSED BY    *ELGABMCC
00032 *                   THE MAXIMUM GENERATOR MODULE.  THE ACCUMS    *ELGABMCC
00033 *                   ARE SELECTED FROM THE GROUP SPECIFIC AND     *ELGABMCC
00034 *                   CONTRACT LEVEL PROCESSING.                   *ELGABMCC
00035 *                                                                *ELGABMCC
00036 ******************************************************************ELGABMCC
00037 *                                                                *ELGABMCC
00038 *                      MAINTENANCE HISTORY                       *ELGABMCC
00039 *                                                                *ELGABMCC
00040 *  MOD     DATE     BY                DESCRIPTION                *ELGABMCC
00041 * ----- ----------- --- ---------------------------------------- *ELGABMCC
00042 * 01.00 03-JUN-1987 LET CREATED                                  *ELGABMCC
00043 * 02.00 07-JAN-1992 RJL REBUILT BY CLONING FROM ELTABM AND       *ELGABMCC
00044 *                       ADDING NECESSARY ADDITIONAL LOGIC TO     *ELGABMCC
00045 *                       SELECT ACCORDING TO COST CONTAINMENT     *ELGABMCC
00046 *                       CRITERIA.                                *ELGABMCC
00047 * 02.01 01-SEP-1994 AKK ADDED LOGIC TO HANDLE RPO, LEFT OUT WHEN *ELGABMCC
00048 *                       RPO ADDED.                               *ELGABMCC
00049 * 02.02 22-FEB-1995 AKK ADDED LOGIC TO HANDLE CPO.               *ELGABMCC
00050 *                                                                *ELGABMCC
00051 * 02.03 16-FEB-1996 AKK ADDED LOGIC TO CBL AND PAN.              *ELGABMCC
00052 *                                                                *ELGABMCC
00053 * 02.04 02-DEC-1998 AKK ADDED LOGIC TO SUPPORT ACP.              *ELGABMCC
00054 *                                                                *ELGABMCC
00055 * 02.05 11-MAR-1999 AKK ADDED LOGIC TO SUPPORT BAE.              *ELGABMCC
00056 *                                                                *ELGABMCC
00057 *       12-AUG-2003 AKK GEN IN QE TO TEST ORDER OF COMPILE       *ELGABMCC
00058 *                                                                *ELGABMCC
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00056 *                                                                *ELGABMCC
00059 ******************************************************************ELGABMCC
00060      TITLE  'ELGABMCC        WORKING STORAGE'.                    ELGABMCC
00061  DATA DIVISION.                                                   ELGABMCC
00062                                                                   ELGABMCC
00063  WORKING-STORAGE SECTION.                                         ELGABMCC
00064                                                                   ELGABMCC
00065  01  SWITCHES.                                                    ELGABMCC
00066      02                                      PICTURE  X(01).      ELGABMCC
00067         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELGABMCC
00068         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELGABMCC
00069      02                                      PICTURE  X(01).      ELGABMCC
00070         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELGABMCC
00071         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELGABMCC
00072      02                                      PICTURE  X(01).      ELGABMCC
00073         88 SW-CC-IND-APPLIES                 VALUE 'Y'.           ELGABMCC
00074         88 SW-CC-IND-DOES-NOT-APPLY          VALUE 'N'.           ELGABMCC
00075      02                                      PICTURE  X(01).      ELGABMCC
00076         88 SW-HAS-IBGR                       VALUE 'Y'.           ELGABMCC
00077         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELGABMCC
00078      02                                      PICTURE  X(01).      ELGABMCC
00079         88 SW-HAS-IDGD                       VALUE 'Y'.           ELGABMCC
00080         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELGABMCC
00081      02                                      PICTURE  X(01).      ELGABMCC
00082         88 SW-HAS-IPGN                       VALUE 'Y'.           ELGABMCC
00083         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELGABMCC
00084      02                                      PICTURE  X(01).      ELGABMCC
00085         88 SW-HAS-IPGP                       VALUE 'Y'.           ELGABMCC
00086         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELGABMCC
00087      02                                      PICTURE  X(01).      ELGABMCC
00088         88 SW-HAS-IPGT                       VALUE 'Y'.           ELGABMCC
00089         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELGABMCC
00090      02                                      PICTURE  X(01).      ELGABMCC
00091         88 SW-HAS-IPGS                       VALUE 'Y'.           ELGABMCC
00092         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELGABMCC
00093      02                                      PICTURE  X(01).      ELGABMCC
00094         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELGABMCC
00095         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELGABMCC
00096      02                                      PICTURE  X(01).      ELGABMCC
00097         88 SW-INTRNL-INST-PROV-CLASS         VALUE 'Y'.           ELGABMCC
00098         88 SW-INTRNL-NOT-INST-PROV-CLASS     VALUE 'N'.           ELGABMCC
00099      02                                      PICTURE  X(01).      ELGABMCC
00100         88 SW-INTRNL-PROF-PROV-CLASS         VALUE 'Y'.           ELGABMCC
00101         88 SW-INTRNL-NOT-PROF-PROV-CLASS     VALUE 'N'.           ELGABMCC
00102      02                                      PICTURE  X(01).      ELGABMCC
00103         88 SW-INTRNL-PROF-PROV-SPEC          VALUE 'Y'.           ELGABMCC
00104         88 SW-INTRNL-NOT-PROF-PROV-SPEC      VALUE 'N'.           ELGABMCC
00105      02                                      PICTURE  X(01).      ELGABMCC
00106         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELGABMCC
00107         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELGABMCC
00108 / -- GCPS DATA ELEMENT TEST AREAS                                 ELGABMCC
00109                                                                   ELGABMCC
00110  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELGABMCC
00111      88 WS-LOB-INST              VALUE '1', '4', '5', '6', '8'.   ELGABMCC
00112      88 WS-LOB-PROF              VALUE '2', '4', '5', '7', '8'.   ELGABMCC
00113      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELGABMCC
00114      88 WS-LOB-BOTH              VALUE '4', '5', '6', '7', '8'.   ELGABMCC
00115                                                                   ELGABMCC
00116  01  WS-PROVISION-ARGUMENT.                                       ELGABMCC
00117      02                          PICTURE  X(05).                  ELGABMCC
00118      02 WS-PROVISION-CLASS       PICTURE  X(01).                  ELGABMCC
00119         88 INST-CLASS            VALUE 'A', 'B', 'W'.             ELGABMCC
00120         88 PROF-CLASS            VALUE 'C', 'D', 'E'.             ELGABMCC
00121                                                                   ELGABMCC
00122  01  WS-SUBTOPIC                 PICTURE  X(16).                  ELGABMCC
00123      88 WS-SUBTOPIC-ATCP         VALUE 'ATCP            '.        ELGABMCC
00124      88 WS-SUBTOPIC-BAE          VALUE 'BAE             '.        ELGABMCC
00125      88 WS-SUBTOPIC-CPO          VALUE 'CPO             '.        ELGABMCC
00126      88 WS-SUBTOPIC-CBL          VALUE 'CBL             '.        ELGABMCC
00127      88 WS-SUBTOPIC-EMH          VALUE 'EMH             '.        ELGABMCC
00128      88 WS-SUBTOPIC-HOSP         VALUE 'HOSP            '.        ELGABMCC
00129      88 WS-SUBTOPIC-IOB          VALUE 'IOB             '.        ELGABMCC
00130      88 WS-SUBTOPIC-MASOP        VALUE 'MASOP           '.        ELGABMCC
00131      88 WS-SUBTOPIC-MCN          VALUE 'MCN             '.        ELGABMCC
00132      88 WS-SUBTOPIC-MEDNEC       VALUE 'MEDNEC          '.        ELGABMCC
00133      88 WS-SUBTOPIC-MHSC         VALUE 'MHSC            '.        ELGABMCC
00134      88 WS-SUBTOPIC-MOND         VALUE 'MOND            '.        ELGABMCC
00135      88 WS-SUBTOPIC-MOPS         VALUE 'MOPS            '.        ELGABMCC
00136      88 WS-SUBTOPIC-MSA          VALUE 'MSA             '.        ELGABMCC
00137      88 WS-SUBTOPIC-PAN          VALUE 'PAN             '.        ELGABMCC
00138      88 WS-SUBTOPIC-PAR          VALUE 'PAR             '.        ELGABMCC
00139      88 WS-SUBTOPIC-PAT          VALUE 'PAT             '.        ELGABMCC
00140      88 WS-SUBTOPIC-POS          VALUE 'POS             '.        ELGABMCC
00141      88 WS-SUBTOPIC-PPO          VALUE 'PPO             '.        ELGABMCC
00142      88 WS-SUBTOPIC-REIMB        VALUE 'REIMB           '.        ELGABMCC
00143      88 WS-SUBTOPIC-RPO          VALUE 'RPO             '.        ELGABMCC
00144      88 WS-SUBTOPIC-WEEK         VALUE 'WEEK            '.        ELGABMCC
00145                                                                   ELGABMCC
00146  01  WS-COST-CONTAIN-IND.                                         ELGABMCC
00147      02                          PICTURE  X(01).                  ELGABMCC
00148         88 WS-CCI-ATCP           VALUE 'A', 'H'.                  ELGABMCC
00149         88 WS-CCI-BAE            VALUE 'N'.                       ELGABMCC
00150         88 WS-CCI-CPO            VALUE 'K'.                       ELGABMCC
00151         88 WS-CCI-CBL            VALUE 'J'.                       ELGABMCC
00152         88 WS-CCI-EMH            VALUE 'I'.                       ELGABMCC
00153         88 WS-CCI-HOSP           VALUE 'B', 'F'.                  ELGABMCC
00154         88 WS-CCI-IOB            VALUE '5'.                       ELGABMCC
00155         88 WS-CCI-MASOP          VALUE '2', 'E', 'G'.             ELGABMCC
00156         88 WS-CCI-MCN            VALUE 'M'.                       ELGABMCC
00157         88 WS-CCI-MEDNEC         VALUE '7'.                       ELGABMCC
00158         88 WS-CCI-MHSC           VALUE 'S'.                       ELGABMCC
00159         88 WS-CCI-MOND           VALUE '4'.                       ELGABMCC
00160         88 WS-CCI-MOPS           VALUE '1', 'F'.                  ELGABMCC
00161         88 WS-CCI-MSA            VALUE '8'.                       ELGABMCC
00162         88 WS-CCI-PAR            VALUE '6', 'E', 'G'.             ELGABMCC
00163         88 WS-CCI-PAN            VALUE 'L'.                       ELGABMCC
00164         88 WS-CCI-PAT            VALUE 'D'.                       ELGABMCC
00165         88 WS-CCI-POS            VALUE 'P'.                       ELGABMCC
00166         88 WS-CCI-PPO            VALUE '9'.                       ELGABMCC
00167         88 WS-CCI-REIMB          VALUE '*'.                       ELGABMCC
00168         88 WS-CCI-RPO            VALUE 'R'.                       ELGABMCC
00169         88 WS-CCI-WEEK           VALUE '3'.                       ELGABMCC
00170      02                          PICTURE  X(01).                  ELGABMCC
00171 / -- CONSTANTS AND WORK FIELDS                                    ELGABMCC
00172                                                                   ELGABMCC
00173  01  PROGRAM-CONSTANTS.                                           ELGABMCC
00174      02 PC-ABM                   PICTURE  X(06) VALUE '#ABM  '.   ELGABMCC
00175      02 PC-GCT-MAX-SUB           PICTURE S9(04) COMP.             ELGABMCC
00176      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELGABMCC
00177      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELGABMCC
00178      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELGABMCC
00179      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELGABMCC
00180      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELGABMCC
00181      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGT '.   ELGABMCC
00182      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELGABMCC
00183                                                                   ELGABMCC
00184  01  WS-WORK-FIELDS.                                              ELGABMCC
00185      02 WS-ABM-SUB               PICTURE S9(04) COMP.             ELGABMCC
00186      02 WS-ABM-ACCUM-CNT         PICTURE S9(04) COMP.             ELGABMCC
00187      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELGABMCC
00188                                                                   ELGABMCC
00189  01  WS-MAX-INDEX-VALUES.                                         ELGABMCC
00190      02 WS-MAX-GAA-INDEX         INDEX.                           ELGABMCC
00191      02 WS-MAX-GAA-INT-INDEX     INDEX.                           ELGABMCC
00192      02 WS-MAX-GCT-INDEX         INDEX.                           ELGABMCC
00193      02 WS-MAX-GCG-INDEX         INDEX.                           ELGABMCC
00194      02 WS-MAX-GX1-INDEX         INDEX.                           ELGABMCC
00195      02 WS-MAX-GX3-INDEX         INDEX.                           ELGABMCC
00196      02 WS-MAX-GXS-INDEX         INDEX.                           ELGABMCC
00197                                                                   ELGABMCC
00198  01  WS-POINTERS.                                                 ELGABMCC
00199      02  WS-POINTER1             POINTER.                         ELGABMCC
00200      02  WS-POINTER2             POINTER.                         ELGABMCC
00201                                                                   ELGABMCC
00202  01  ACCUM-HOLD-TBL.                                              ELGABMCC
00203      02  ACCUM-SLOT-NBR          PICTURE S9(07) COMP-3            ELGABMCC
00204                                  OCCURS 5 TIMES.                  ELGABMCC
00205                                                                   ELGABMCC
00206  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELGABMCC
00207      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGABMCC
00208      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGABMCC
00209      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGABMCC
00210      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGABMCC
00211      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGABMCC
00212      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGABMCC
00213                                                                   ELGABMCC
00214 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELGABMCC
00215      COPY ELSCFTB2.                                               ELGABMCC
00216                                                                   ELGABMCC
00217 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELGABMCC
00218      COPY ELSCFTB9.                                               ELGABMCC
00219                                                                   ELGABMCC
00220      TITLE  'ELGABMCC        LINKAGE SECTION'                     ELGABMCC
00221  LINKAGE SECTION.                                                 ELGABMCC
00222  01  DFHCOMMAREA.                                                 ELGABMCC
00223      COPY ELSCOMMC.                                               ELGABMCC
00224 /                                                                 ELGABMCC
00225      COPY ELSCIA2C.                                               ELGABMCC
00226 /                                                                 ELGABMCC
00227      COPY ELSIOPMC.                                               ELGABMCC
00228 /                                                                 ELGABMCC
00229      COPY ELSKEYSC.                                               ELGABMCC
00230 /                                                                 ELGABMCC
00231      COPY ELSSRTPC.                                               ELGABMCC
00232 /                                                                 ELGABMCC
00233      COPY ELSSSCBC.                                               ELGABMCC
00234 /                                                                 ELGABMCC
00235  01  GCG-GRP-SPEC-RECORD-AREA.                                    ELGABMCC
00236      COPY GCGROUPC.                                               ELGABMCC
00237 /                                                                 ELGABMCC
00238  01  GCT-CONTRACT-RECORD-AREA.                                    ELGABMCC
00239      COPY GCCONTRC.                                               ELGABMCC
00240 /                                                                 ELGABMCC
00241  01  GAA-RECORD-AREA.                                             ELGABMCC
00242      COPY GCTABMC.                                                ELGABMCC
00243 /                                                                 ELGABMCC
00244      COPY ELSACUMC.                                               ELGABMCC
00245 /                                                                 ELGABMCC
00246  01  GX1-RECORD-AREA.                                             ELGABMCC
00247      COPY GCTIBGRC.                                               ELGABMCC
00248 /                                                                 ELGABMCC
00249  01  GX3-RECORD-AREA.                                             ELGABMCC
00250      COPY GCTIPGTC.                                               ELGABMCC
00251 /                                                                 ELGABMCC
00252  01  GXS-RECORD-AREA.                                             ELGABMCC
00253      COPY GCTIPGSC.                                               ELGABMCC
00254      TITLE  'ELGABMCC        PROCEDURE DIVISION'.                 ELGABMCC
00255 ************************************************************      ELGABMCC
00256 *                                                          *      ELGABMCC
00257 *    ELTABM MAINLINE                                       *      ELGABMCC
00258 *                                                          *      ELGABMCC
00259 ************************************************************      ELGABMCC
00260                                                                   ELGABMCC
00261  PROCEDURE DIVISION.                                              ELGABMCC
00262      PERFORM 0010-INITIALIZATION.                                 ELGABMCC
00263      PERFORM 0100-PROCESS.                                        ELGABMCC
00264      GOBACK.                                                      ELGABMCC
00265                                                                   ELGABMCC
00266 ************************************************************      ELGABMCC
00267 *                                                          *      ELGABMCC
00268 *    INITIALIZATION                                        *      ELGABMCC
00269 *                                                          *      ELGABMCC
00270 ************************************************************      ELGABMCC
00271                                                                   ELGABMCC
00272  0010-INITIALIZATION.                                             ELGABMCC
00273      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELGABMCC
00274      PERFORM 0060-EST-ADR-KEY-WK-AREA.                            ELGABMCC
00275      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELGABMCC
00276      PERFORM 0120-EST-ADR-GRP-SPC.                                ELGABMCC
00277      PERFORM 0190-INIT-DATA.                                      ELGABMCC
00278                                                                   ELGABMCC
00279 ************************************************************      ELGABMCC
00280 *                                                          *      ELGABMCC
00281 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGABMCC
00282 *                                                          *      ELGABMCC
00283 ************************************************************      ELGABMCC
00284                                                                   ELGABMCC
00285  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELGABMCC
00286      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGABMCC
00287      THEN                                                         ELGABMCC
00288         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGABMCC
00289      ELSE                                                         ELGABMCC
00290         IF ECA-CIA-PTR = NULL                                     ELGABMCC
00291         THEN                                                      ELGABMCC
00292            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGABMCC
00293         ELSE                                                      ELGABMCC
00294            CALL 'ELUINISM' USING DFHCOMMAREA                      ELGABMCC
00295               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELGABMCC
00296               END-CALL                                            ELGABMCC
00297            SET CIA-ELSSSCB-DDN TO TRUE                            ELGABMCC
00298            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELGABMCC
00299               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELGABMCC
00300               END-CALL                                            ELGABMCC
00301            IF CIA-RC-PTR-NULL                                     ELGABMCC
00302            THEN                                                   ELGABMCC
00303               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELGABMCC
00304               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGABMCC
00305            ELSE                                                   ELGABMCC
00306               CONTINUE                                            ELGABMCC
00307            END-IF                                                 ELGABMCC
00308         END-IF                                                    ELGABMCC
00309      END-IF.                                                      ELGABMCC
00310                                                                   ELGABMCC
00311 /***********************************************************      ELGABMCC
00312 *                                                          *      ELGABMCC
00313 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELGABMCC
00314 *                                                          *      ELGABMCC
00315 ************************************************************      ELGABMCC
00316                                                                   ELGABMCC
00317  0060-EST-ADR-KEY-WK-AREA.                                        ELGABMCC
00318      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGABMCC
00319      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGABMCC
00320         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELGABMCC
00321         END-CALL.                                                 ELGABMCC
00322      IF CIA-RC-PTR-NULL                                           ELGABMCC
00323      THEN                                                         ELGABMCC
00324         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGABMCC
00325         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGABMCC
00326      END-IF.                                                      ELGABMCC
00327                                                                   ELGABMCC
00328 ************************************************************      ELGABMCC
00329 *                                                          *      ELGABMCC
00330 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGABMCC
00331 *                                                          *      ELGABMCC
00332 ************************************************************      ELGABMCC
00333                                                                   ELGABMCC
00334  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGABMCC
00335      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGABMCC
00336      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGABMCC
00337         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELGABMCC
00338         END-CALL.                                                 ELGABMCC
00339      IF CIA-RC-PTR-NULL                                           ELGABMCC
00340      THEN                                                         ELGABMCC
00341         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGABMCC
00342         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGABMCC
00343      END-IF.                                                      ELGABMCC
00344                                                                   ELGABMCC
00345 ************************************************************      ELGABMCC
00346 *                                                          *      ELGABMCC
00347 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD     *      ELGABMCC
00348 *                                                          *      ELGABMCC
00349 ************************************************************      ELGABMCC
00350                                                                   ELGABMCC
00351  0120-EST-ADR-GRP-SPC.                                            ELGABMCC
00352      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELGABMCC
00353      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGABMCC
00354         ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                       ELGABMCC
00355         END-CALL.                                                 ELGABMCC
00356      IF CIA-RC-PTR-NULL                                           ELGABMCC
00357      THEN                                                         ELGABMCC
00358         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGABMCC
00359         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGABMCC
00360      END-IF.                                                      ELGABMCC
00361                                                                   ELGABMCC
00362 /***********************************************************      ELGABMCC
00363 *                                                          *      ELGABMCC
00364 *    INITIALIZE DATA AREAS                                 *      ELGABMCC
00365 *                                                          *      ELGABMCC
00366 ************************************************************      ELGABMCC
00367                                                                   ELGABMCC
00368  0190-INIT-DATA.                                                  ELGABMCC
00369      COMPUTE PC-GCT-MAX-SUB =   LENGTH OF GCT-CONT-TAB-PTRS       ELGABMCC
00370                               / LENGTH OF GCT-CON-TAB-ID-SLOT.    ELGABMCC
00371      SET GCT-INDEX TO PC-GCT-MAX-SUB.                             ELGABMCC
00372      SET WS-MAX-GCT-INDEX TO GCT-INDEX.                           ELGABMCC
00373      SET GCG-INDEX TO GCG-COUNT-TAB-PROVN-POINTERS.               ELGABMCC
00374      SET WS-MAX-GCG-INDEX TO GCG-INDEX.                           ELGABMCC
00375      INITIALIZE WS-ABM-ACCUM-CNT.                                 ELGABMCC
00376      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGABMCC
00377                                                                   ELGABMCC
00378 /***********************************************************      ELGABMCC
00379 *                                                          *      ELGABMCC
00380 *        PROCESS                                           *      ELGABMCC
00381 *                                                          *      ELGABMCC
00382 ************************************************************      ELGABMCC
00383                                                                   ELGABMCC
00384  0100-PROCESS.                                                    ELGABMCC
00385      PERFORM 0110-SCAN-GRP-SPC-FOR-ACCUMS.                        ELGABMCC
00386      PERFORM 0120-SCAN-CONTRACTS-FOR-ACCUMS.                      ELGABMCC
00387                                                                   ELGABMCC
00388      IF WS-ABM-ACCUM-CNT >  0                                     ELGABMCC
00389      THEN                                                         ELGABMCC
00390          PERFORM 0240-SCAN-FOR-APPLIC-OCCRNCS                     ELGABMCC
00391      END-IF.                                                      ELGABMCC
00392                                                                   ELGABMCC
00393 *    -- LINK TO THE OUTPUT GENERATOR                              ELGABMCC
00394      IF SW-APPLIC-ACCUM-FOUND                                     ELGABMCC
00395         SET SRP-COST-CONT-ACCUM TO TRUE                           ELGABMCC
00396         EXEC CICS LINK PROGRAM ('ELGABM')                         ELGABMCC
00397                        COMMAREA (DFHCOMMAREA)                     ELGABMCC
00398         END-EXEC                                                  ELGABMCC
00399      END-IF.                                                      ELGABMCC
00400                                                                   ELGABMCC
00401                                                                   ELGABMCC
00402 /***********************************************************      ELGABMCC
00403 *                                                          *      ELGABMCC
00404 *    SCAN GROUP SPECIFIC RECORD FOR ACCUMULATORS           *      ELGABMCC
00405 *                                                          *      ELGABMCC
00406 ************************************************************      ELGABMCC
00407                                                                   ELGABMCC
00408  0110-SCAN-GRP-SPC-FOR-ACCUMS.                                    ELGABMCC
00409      PERFORM WITH TEST BEFORE                                     ELGABMCC
00410         VARYING GCG-INDEX FROM 1 BY 1                             ELGABMCC
00411           UNTIL GCG-INDEX = WS-MAX-GCG-INDEX                      ELGABMCC
00412                 OR GCG-TAB-ID (GCG-INDEX) > PC-ABM                ELGABMCC
00413 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELGABMCC
00414         IF     GCG-TAB-ID (GCG-INDEX)  =  PC-ABM                  ELGABMCC
00415            AND GCG-TAB-SLOT-NO (GCG-INDEX)  >  ZERO               ELGABMCC
00416         THEN                                                      ELGABMCC
00417 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELGABMCC
00418            MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO WS-SLOT-NBR        ELGABMCC
00419            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELGABMCC
00420         END-IF                                                    ELGABMCC
00421         END-PERFORM.                                              ELGABMCC
00422                                                                   ELGABMCC
00423 /***********************************************************      ELGABMCC
00424 *                                                          *      ELGABMCC
00425 *    SCAN CONTRACT RECORDS FOR ACCUMULATORS                *      ELGABMCC
00426 *                                                          *      ELGABMCC
00427 ************************************************************      ELGABMCC
00428                                                                   ELGABMCC
00429  0120-SCAN-CONTRACTS-FOR-ACCUMS.                                  ELGABMCC
00430      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELGABMCC
00431      THEN                                                         ELGABMCC
00432          PERFORM 0130-SCAN-INST-BAS                               ELGABMCC
00433      END-IF.                                                      ELGABMCC
00434                                                                   ELGABMCC
00435      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELGABMCC
00436      THEN                                                         ELGABMCC
00437          PERFORM 0140-SCAN-PROF-BAS                               ELGABMCC
00438      END-IF.                                                      ELGABMCC
00439                                                                   ELGABMCC
00440      SET WS-POINTER1 TO NULLS.                                    ELGABMCC
00441      SET WS-POINTER2 TO NULLS.                                    ELGABMCC
00442                                                                   ELGABMCC
00443      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELGABMCC
00444      THEN                                                         ELGABMCC
00445          PERFORM 0150-SCAN-INST-SUP                               ELGABMCC
00446      END-IF.                                                      ELGABMCC
00447                                                                   ELGABMCC
00448      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELGABMCC
00449      THEN                                                         ELGABMCC
00450          PERFORM 0170-SCAN-PROF-SUP                               ELGABMCC
00451      END-IF.                                                      ELGABMCC
00452                                                                   ELGABMCC
00453 /***********************************************************      ELGABMCC
00454 *                                                          *      ELGABMCC
00455 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELGABMCC
00456 *                                                          *      ELGABMCC
00457 ************************************************************      ELGABMCC
00458                                                                   ELGABMCC
00459  0130-SCAN-INST-BAS.                                              ELGABMCC
00460      SET CIA-ELSCONIB-DDN TO TRUE.                                ELGABMCC
00461      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGABMCC
00462         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGABMCC
00463         END-CALL.                                                 ELGABMCC
00464      SET WS-POINTER1 TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.      ELGABMCC
00465                                                                   ELGABMCC
00466      IF CIA-RC-PTR-NULL                                           ELGABMCC
00467      THEN                                                         ELGABMCC
00468         CONTINUE                                                  ELGABMCC
00469      ELSE                                                         ELGABMCC
00470         PERFORM 0180-SCAN-CONTRACT-FOR-ACCUMS                     ELGABMCC
00471      END-IF.                                                      ELGABMCC
00472                                                                   ELGABMCC
00473 ************************************************************      ELGABMCC
00474 *                                                          *      ELGABMCC
00475 *    SCAN PROFESSIONAL BASIC CONTRACT RECORD               *      ELGABMCC
00476 *                                                          *      ELGABMCC
00477 ************************************************************      ELGABMCC
00478                                                                   ELGABMCC
00479  0140-SCAN-PROF-BAS.                                              ELGABMCC
00480      SET CIA-ELSCONPB-DDN TO TRUE.                                ELGABMCC
00481      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGABMCC
00482         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGABMCC
00483         END-CALL.                                                 ELGABMCC
00484      SET WS-POINTER2 TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.      ELGABMCC
00485                                                                   ELGABMCC
00486      IF CIA-RC-PTR-NULL OR (WS-POINTER1 = WS-POINTER2)            ELGABMCC
00487      THEN                                                         ELGABMCC
00488         CONTINUE                                                  ELGABMCC
00489      ELSE                                                         ELGABMCC
00490         PERFORM 0180-SCAN-CONTRACT-FOR-ACCUMS                     ELGABMCC
00491      END-IF.                                                      ELGABMCC
00492                                                                   ELGABMCC
00493 /***********************************************************      ELGABMCC
00494 *                                                          *      ELGABMCC
00495 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELGABMCC
00496 *                                                          *      ELGABMCC
00497 ************************************************************      ELGABMCC
00498                                                                   ELGABMCC
00499  0150-SCAN-INST-SUP.                                              ELGABMCC
00500      SET CIA-ELSCONIS-DDN TO TRUE.                                ELGABMCC
00501      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGABMCC
00502         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGABMCC
00503         END-CALL.                                                 ELGABMCC
00504      SET WS-POINTER1 TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.      ELGABMCC
00505                                                                   ELGABMCC
00506      IF CIA-RC-PTR-NULL                                           ELGABMCC
00507      THEN                                                         ELGABMCC
00508         CONTINUE                                                  ELGABMCC
00509      ELSE                                                         ELGABMCC
00510         PERFORM 0180-SCAN-CONTRACT-FOR-ACCUMS                     ELGABMCC
00511      END-IF.                                                      ELGABMCC
00512                                                                   ELGABMCC
00513 ************************************************************      ELGABMCC
00514 *                                                          *      ELGABMCC
00515 *    SCAN PROFESSIONAL SUPPLEMENTAL CONTRACT RECORD        *      ELGABMCC
00516 *                                                          *      ELGABMCC
00517 ************************************************************      ELGABMCC
00518                                                                   ELGABMCC
00519  0170-SCAN-PROF-SUP.                                              ELGABMCC
00520      SET CIA-ELSCONPS-DDN TO TRUE.                                ELGABMCC
00521      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGABMCC
00522         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGABMCC
00523         END-CALL.                                                 ELGABMCC
00524      SET WS-POINTER2 TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.      ELGABMCC
00525                                                                   ELGABMCC
00526      IF CIA-RC-PTR-NULL AND (WS-POINTER1 = WS-POINTER2)           ELGABMCC
00527      THEN                                                         ELGABMCC
00528         CONTINUE                                                  ELGABMCC
00529      ELSE                                                         ELGABMCC
00530         PERFORM 0180-SCAN-CONTRACT-FOR-ACCUMS                     ELGABMCC
00531      END-IF.                                                      ELGABMCC
00532                                                                   ELGABMCC
00533 /***********************************************************      ELGABMCC
00534 *                                                          *      ELGABMCC
00535 *    SCAN A CONTRACT RECORD FOR ACCUMULATORS               *      ELGABMCC
00536 *                                                          *      ELGABMCC
00537 ************************************************************      ELGABMCC
00538                                                                   ELGABMCC
00539  0180-SCAN-CONTRACT-FOR-ACCUMS.                                   ELGABMCC
00540      PERFORM WITH TEST BEFORE                                     ELGABMCC
00541         VARYING GCT-TAB-INDEX FROM 1 BY 1                         ELGABMCC
00542           UNTIL    GCT-TAB-INDEX > WS-MAX-GCT-INDEX               ELGABMCC
00543                 OR GCT-CON-TAB-ID (GCT-TAB-INDEX) > PC-ABM        ELGABMCC
00544 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELGABMCC
00545         IF     GCT-CON-TAB-ID (GCT-TAB-INDEX) = PC-ABM            ELGABMCC
00546            AND GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  > ZEROS          ELGABMCC
00547         THEN                                                      ELGABMCC
00548 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELGABMCC
00549            MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  TO  WS-SLOT-NBR ELGABMCC
00550            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELGABMCC
00551         END-IF                                                    ELGABMCC
00552         END-PERFORM.                                              ELGABMCC
00553                                                                   ELGABMCC
00554 /***********************************************************      ELGABMCC
00555 *                                                          *      ELGABMCC
00556 *    SAVE UNIQUE ACCUMULATOR SLOT NUMBER                   *      ELGABMCC
00557 *                                                          *      ELGABMCC
00558 ************************************************************      ELGABMCC
00559                                                                   ELGABMCC
00560  0200-SAVE-UNQ-ACCUM-SLOT-NBR.                                    ELGABMCC
00561                                                                   ELGABMCC
00562 * -- SCAN TABLE OF ACCUM SLOT NUMBERS FOR DUPLICATE               ELGABMCC
00563      SET SW-UNQ-SLOT-NBR TO TRUE.                                 ELGABMCC
00564      PERFORM WITH TEST BEFORE                                     ELGABMCC
00565         VARYING WS-ABM-SUB FROM 1 BY 1                            ELGABMCC
00566           UNTIL    WS-ABM-SUB > WS-ABM-ACCUM-CNT                  ELGABMCC
00567                 OR SW-DUP-SLOT-NBR                                ELGABMCC
00568         IF WS-SLOT-NBR = ACCUM-SLOT-NBR (WS-ABM-SUB)              ELGABMCC
00569         THEN                                                      ELGABMCC
00570            SET SW-DUP-SLOT-NBR TO TRUE                            ELGABMCC
00571         END-IF                                                    ELGABMCC
00572         END-PERFORM.                                              ELGABMCC
00573                                                                   ELGABMCC
00574 * -- IF SLOT NUMBER IS UNIQUE, ADD IT TO THE HOLD TABLE           ELGABMCC
00575      IF SW-UNQ-SLOT-NBR                                           ELGABMCC
00576      THEN                                                         ELGABMCC
00577         ADD 1 TO  WS-ABM-ACCUM-CNT                                ELGABMCC
00578         MOVE WS-SLOT-NBR TO ACCUM-SLOT-NBR(WS-ABM-ACCUM-CNT)      ELGABMCC
00579      END-IF.                                                      ELGABMCC
00580                                                                   ELGABMCC
00581 /***********************************************************      ELGABMCC
00582 *                                                          *      ELGABMCC
00583 *    SCAN ABM ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELGABMCC
00584 *                                                          *      ELGABMCC
00585 ************************************************************      ELGABMCC
00586                                                                   ELGABMCC
00587  0240-SCAN-FOR-APPLIC-OCCRNCS.                                    ELGABMCC
00588      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGABMCC
00589      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGABMCC
00590      PERFORM 0250-DELETE-ABM-SUMMARY-FILE.                        ELGABMCC
00591      PERFORM 0260-ALLOC-WORKFILE-REC-AREA.                        ELGABMCC
00592                                                                   ELGABMCC
00593 * -- READ AND SCAN EACH ACCUMULATOR TABULAR                       ELGABMCC
00594      PERFORM WITH TEST BEFORE                                     ELGABMCC
00595         VARYING WS-ABM-SUB FROM 1 BY 1                            ELGABMCC
00596           UNTIL WS-ABM-SUB > WS-ABM-ACCUM-CNT                     ELGABMCC
00597 *    -- OBTAIN ACCUMULATOR TABULAR RECORD                         ELGABMCC
00598         MOVE PC-ABM TO KWA-PROVISION-ID                           ELGABMCC
00599         MOVE ACCUM-SLOT-NBR (WS-ABM-SUB) TO KWA-PROVISION-SLOT-NO ELGABMCC
00600         PERFORM 0280-READ-TABULAR-REC                             ELGABMCC
00601 *    -- SCAN ACCUMULATOR TABULAR                                  ELGABMCC
00602         PERFORM 0300-TEST-ABM-OCCURENCE                           ELGABMCC
00603            VARYING GAA-INDEX FROM 1 BY 1                          ELGABMCC
00604              UNTIL GAA-INDEX = WS-MAX-GAA-INDEX                   ELGABMCC
00605         END-PERFORM.                                              ELGABMCC
00606                                                                   ELGABMCC
00607 /***********************************************************      ELGABMCC
00608 *                                                          *      ELGABMCC
00609 *        DELETE ABM SUMMARY FILE                           *      ELGABMCC
00610 *                                                          *      ELGABMCC
00611 ************************************************************      ELGABMCC
00612                                                                   ELGABMCC
00613  0250-DELETE-ABM-SUMMARY-FILE.                                    ELGABMCC
00614      SET IOP-DEL TO TRUE.                                         ELGABMCC
00615      SET IOP-FCQ-NONE TO TRUE.                                    ELGABMCC
00616      SET IOP-KVQ-NONE TO TRUE.                                    ELGABMCC
00617      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGABMCC
00618                                                                   ELGABMCC
00619 /***********************************************************      ELGABMCC
00620 *                                                          *      ELGABMCC
00621 *    ALLOCATE WORKFILE RECORD AREA                         *      ELGABMCC
00622 *                                                          *      ELGABMCC
00623 ************************************************************      ELGABMCC
00624                                                                   ELGABMCC
00625  0260-ALLOC-WORKFILE-REC-AREA.                                    ELGABMCC
00626      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGABMCC
00627      SET CIA-STG-GETMAIN TO TRUE.                                 ELGABMCC
00628      SET IOP-GETMAIN-REC TO TRUE.                                 ELGABMCC
00629      COMPUTE IOP-MAX-REC-LEN =                                    ELGABMCC
00630              LENGTH OF ACCUM-FIXED-AREA                           ELGABMCC
00631 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELGABMCC
00632            + LENGTH OF ACCUM-VARIABLE-AREA                        ELGABMCC
00633            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELGABMCC
00634 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELGABMCC
00635 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELGABMCC
00636                                                                   ELGABMCC
00637      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGABMCC
00638      IF IOP-REC-PTR = NULLS                                       ELGABMCC
00639      THEN                                                         ELGABMCC
00640         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGABMCC
00641         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGABMCC
00642      ELSE                                                         ELGABMCC
00643         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELGABMCC
00644      END-IF.                                                      ELGABMCC
00645                                                                   ELGABMCC
00646 /***********************************************************      ELGABMCC
00647 *                                                          *      ELGABMCC
00648 *    READ TABULAR RECORD                                   *      ELGABMCC
00649 *                                                          *      ELGABMCC
00650 ************************************************************      ELGABMCC
00651                                                                   ELGABMCC
00652  0280-READ-TABULAR-REC.                                           ELGABMCC
00653      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELGABMCC
00654      SET IOP-RD TO TRUE.                                          ELGABMCC
00655      SET IOP-FCQ-NONE TO TRUE.                                    ELGABMCC
00656      SET IOP-KVQ-EQ TO TRUE.                                      ELGABMCC
00657      SET IOP-STG-MODE-MOVE TO TRUE.                               ELGABMCC
00658      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGABMCC
00659      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGABMCC
00660      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGABMCC
00661                                                                   ELGABMCC
00662      EVALUATE TRUE                                                ELGABMCC
00663        WHEN IOP-RC-OK                                             ELGABMCC
00664           SET ADDRESS OF GAA-RECORD-AREA TO IOP-REC-PTR           ELGABMCC
00665           SET IOP-REC-PTR TO NULLS                                ELGABMCC
00666           SET GAA-INDEX TO GAA-ENTRY-COUNT                        ELGABMCC
00667           SET WS-MAX-GAA-INDEX TO GAA-INDEX                       ELGABMCC
00668        WHEN IOP-RC-NOTFND                                         ELGABMCC
00669           SET CIA-AB-NOTFND-GCTABULR TO TRUE                      ELGABMCC
00670           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELGABMCC
00671        WHEN OTHER                                                 ELGABMCC
00672           SET CIA-AB-CRITIO TO TRUE                               ELGABMCC
00673           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELGABMCC
00674        END-EVALUATE.                                              ELGABMCC
00675                                                                   ELGABMCC
00676 /***********************************************************      ELGABMCC
00677 *                                                          *      ELGABMCC
00678 *        TEST ABM OCCURS                                   *      ELGABMCC
00679 *                                                          *      ELGABMCC
00680 ************************************************************      ELGABMCC
00681                                                                   ELGABMCC
00682  0300-TEST-ABM-OCCURENCE.                                         ELGABMCC
00683      MOVE GAA-BAMA-L-O-B (GAA-INDEX) TO WS-LOB-ACCUM-OCCRNC.      ELGABMCC
00684      MOVE GAA-BAMA-COST-CONTAIN-IND (GAA-INDEX)                   ELGABMCC
00685        TO WS-COST-CONTAIN-IND.                                    ELGABMCC
00686      SET SW-CC-IND-DOES-NOT-APPLY TO TRUE.                        ELGABMCC
00687      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELGABMCC
00688                                                                   ELGABMCC
00689      PERFORM 0500-CHK-CC-IND.                                     ELGABMCC
00690                                                                   ELGABMCC
00691      IF SW-CC-IND-APPLIES                                         ELGABMCC
00692      THEN                                                         ELGABMCC
00693 *    -- SCAN FOR INTERNAL TABULARS                                ELGABMCC
00694 *       (THIS IS DONE NOW IN CASE IPGT OR IBGR IS NEEDED TO       ELGABMCC
00695 *        DETERMINE WHETHER OCCURRENCE IS INSTITUTIONAL OR         ELGABMCC
00696 *        PROFESSIONAL.)                                           ELGABMCC
00697         PERFORM 0310-SCAN-INTRNL-TAB                              ELGABMCC
00698         EVALUATE TRUE ALSO TRUE                                   ELGABMCC
00699            WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                     ELGABMCC
00700               SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE               ELGABMCC
00701               SET SW-OCCRNC-APPLIES TO TRUE                       ELGABMCC
00702            WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH              ELGABMCC
00703               SET SRP-ACCUM-PROV-CLASS-INST TO TRUE               ELGABMCC
00704               PERFORM 0550-CHK-INTRNL-TAB-PROV-CLASS              ELGABMCC
00705            WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST              ELGABMCC
00706               SET SRP-ACCUM-PROV-CLASS-INST TO TRUE               ELGABMCC
00707               SET SW-OCCRNC-APPLIES TO TRUE                       ELGABMCC
00708            WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH              ELGABMCC
00709               SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE               ELGABMCC
00710               PERFORM 0550-CHK-INTRNL-TAB-PROV-CLASS              ELGABMCC
00711               SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                ELGABMCC
00712               PERFORM 0551-CHK-INTRNL-TAB-PROV-SPEC               ELGABMCC
00713            WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF              ELGABMCC
00714               SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE               ELGABMCC
00715               SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                ELGABMCC
00716               SET SW-OCCRNC-APPLIES TO TRUE                       ELGABMCC
00717            WHEN OTHER                                             ELGABMCC
00718               CONTINUE                                            ELGABMCC
00719            END-EVALUATE                                           ELGABMCC
00720      ELSE                                                         ELGABMCC
00721         CONTINUE                                                  ELGABMCC
00722      END-IF.                                                      ELGABMCC
00723                                                                   ELGABMCC
00724      IF SW-OCCRNC-APPLIES                                         ELGABMCC
00725      THEN                                                         ELGABMCC
00726 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELGABMCC
00727         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELGABMCC
00728         PERFORM 0330-INIT-ACCUM-EXTRACT                           ELGABMCC
00729         PERFORM 0340-EXTRACT-ACCUM                                ELGABMCC
00730         PERFORM 0410-CHK-EXTRACT-DATA-INTGRTY                     ELGABMCC
00731         PERFORM 0710-WRITE-EXTRACT-RECORD                         ELGABMCC
00732      END-IF.                                                      ELGABMCC
00733                                                                   ELGABMCC
00734 /***********************************************************      ELGABMCC
00735 *                                                          *      ELGABMCC
00736 *    SCAN INTERNAL TABULARS                                *      ELGABMCC
00737 *                                                          *      ELGABMCC
00738 ************************************************************      ELGABMCC
00739                                                                   ELGABMCC
00740  0310-SCAN-INTRNL-TAB.                                            ELGABMCC
00741                                                                   ELGABMCC
00742 * -- INITIALIZE SCAN PROCESS                                      ELGABMCC
00743      SET SW-OCCRNC-DOES-NOT-APPLY                                 ELGABMCC
00744          SW-HAS-NO-IBGR                                           ELGABMCC
00745          SW-HAS-NO-IDGD                                           ELGABMCC
00746          SW-HAS-NO-IPGN                                           ELGABMCC
00747          SW-HAS-NO-IPGP                                           ELGABMCC
00748          SW-HAS-NO-IPGT                                           ELGABMCC
00749          SW-HAS-NO-IPGS                                           ELGABMCC
00750       TO TRUE.                                                    ELGABMCC
00751      INITIALIZE WS-IBGR-SLOT-NBR                                  ELGABMCC
00752                 WS-IDGD-SLOT-NBR                                  ELGABMCC
00753                 WS-IPGN-SLOT-NBR                                  ELGABMCC
00754                 WS-IPGP-SLOT-NBR                                  ELGABMCC
00755                 WS-IPGT-SLOT-NBR                                  ELGABMCC
00756                 WS-IPGS-SLOT-NBR.                                 ELGABMCC
00757      SET GAA-INT-INDEX TO GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX). ELGABMCC
00758      SET WS-MAX-GAA-INT-INDEX TO GAA-INT-INDEX.                   ELGABMCC
00759                                                                   ELGABMCC
00760 * -- SCAN THE LIST OF INTERNAL TABULARS                           ELGABMCC
00761      PERFORM 0320-SCAN-THE-INTERNAL-TABULAR                       ELGABMCC
00762         VARYING GAA-INT-INDEX FROM 1 BY 1                         ELGABMCC
00763           UNTIL GAA-INT-INDEX >= WS-MAX-GAA-INT-INDEX.            ELGABMCC
00764                                                                   ELGABMCC
00765 /***********************************************************      ELGABMCC
00766 *                                                          *      ELGABMCC
00767 *    SCAN INTERNAL TABULAR LIST                            *      ELGABMCC
00768 *                                                          *      ELGABMCC
00769 ************************************************************      ELGABMCC
00770                                                                   ELGABMCC
00771  0320-SCAN-THE-INTERNAL-TABULAR.                                  ELGABMCC
00772      IF GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX) > 0               ELGABMCC
00773      THEN                                                         ELGABMCC
00774         MOVE GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX)              ELGABMCC
00775           TO WS-SLOT-NBR                                          ELGABMCC
00776         EVALUATE GAA-INT-ID (GAA-INDEX, GAA-INT-INDEX)            ELGABMCC
00777            WHEN PC-IBGR                                           ELGABMCC
00778               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELGABMCC
00779               SET SW-HAS-IBGR                                     ELGABMCC
00780                TO TRUE                                            ELGABMCC
00781            WHEN PC-IDGD                                           ELGABMCC
00782               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELGABMCC
00783               SET SW-HAS-IDGD                                     ELGABMCC
00784                TO TRUE                                            ELGABMCC
00785            WHEN PC-IPGP                                           ELGABMCC
00786               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELGABMCC
00787               SET SW-HAS-IPGP                                     ELGABMCC
00788                TO TRUE                                            ELGABMCC
00789            WHEN PC-IPGN                                           ELGABMCC
00790               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELGABMCC
00791               SET SW-HAS-IPGN                                     ELGABMCC
00792                TO TRUE                                            ELGABMCC
00793            WHEN PC-IPGT                                           ELGABMCC
00794               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELGABMCC
00795               SET SW-HAS-IPGT                                     ELGABMCC
00796                TO TRUE                                            ELGABMCC
00797            WHEN PC-IPGS                                           ELGABMCC
00798               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELGABMCC
00799               SET SW-HAS-IPGS                                     ELGABMCC
00800                TO TRUE                                            ELGABMCC
00801            WHEN OTHER                                             ELGABMCC
00802               CONTINUE                                            ELGABMCC
00803            END-EVALUATE                                           ELGABMCC
00804      END-IF.                                                      ELGABMCC
00805                                                                   ELGABMCC
00806 /***********************************************************      ELGABMCC
00807 *                                                          *      ELGABMCC
00808 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELGABMCC
00809 *                                                          *      ELGABMCC
00810 ************************************************************      ELGABMCC
00811                                                                   ELGABMCC
00812  0330-INIT-ACCUM-EXTRACT.                                         ELGABMCC
00813      INITIALIZE ACCUM-FIXED-AREA.                                 ELGABMCC
00814      SET ACCUM-ABM TO TRUE.                                       ELGABMCC
00815      MOVE 1 TO  ACCUM-ASCEND-DESCEND-COUNT.                       ELGABMCC
00816      SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELGABMCC
00817      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELGABMCC
00818      INITIALIZE ACCUM-COPAY-ENTRY (1).                            ELGABMCC
00819                                                                   ELGABMCC
00820 /***********************************************************      ELGABMCC
00821 *                                                          *      ELGABMCC
00822 *        SUMMARIZE ABM TOPIC LEVEL DATA ELEMENTS           *      ELGABMCC
00823 *                                                          *      ELGABMCC
00824 ************************************************************      ELGABMCC
00825                                                                   ELGABMCC
00826  0340-EXTRACT-ACCUM.                                              ELGABMCC
00827      MOVE GAA-BAMA-FYI-VALUE (GAA-INDEX) TO ACCUM-FYI-VALUE.      ELGABMCC
00828      MOVE GAA-BAMA-COST-CONTAIN-IND (GAA-INDEX)                   ELGABMCC
00829        TO ACCUM-COST-CONTAIN-IND.                                 ELGABMCC
00830      MOVE GAA-BAMA-PLACE-OF-TREATMENT (GAA-INDEX)                 ELGABMCC
00831        TO ACCUM-PLACE-OF-TREATMENT.                               ELGABMCC
00832      MOVE GAA-BAMA-BENEFIT-PERIOD (GAA-INDEX)                     ELGABMCC
00833        TO ACCUM-BENEFIT-PERIOD.                                   ELGABMCC
00834      MOVE GAA-BAMA-BEN-PER-TIME-FCTR (GAA-INDEX)                  ELGABMCC
00835        TO ACCUM-BEN-PER-TIME-FCTR.                                ELGABMCC
00836      MOVE GAA-BAMA-BEN-PER-TIME-QUAL (GAA-INDEX)                  ELGABMCC
00837        TO ACCUM-BEN-PER-TIME-QUAL.                                ELGABMCC
00838      MOVE GAA-BAMA-INTERVAL-TIME-FCTR (GAA-INDEX)                 ELGABMCC
00839        TO ACCUM-INTERVAL-TIME-FCTR.                               ELGABMCC
00840      MOVE GAA-BAMA-INTERVAL-TYPE (GAA-INDEX)                      ELGABMCC
00841        TO ACCUM-INTERVAL-TYPE.                                    ELGABMCC
00842      MOVE GAA-BAMA-INTERVAL-OVRD-IND (GAA-INDEX)                  ELGABMCC
00843        TO ACCUM-INTERVAL-OVRD-IND.                                ELGABMCC
00844      MOVE GAA-BAMA-INTERVAL-OVRD-VALUE (GAA-INDEX)                ELGABMCC
00845        TO ACCUM-INTERVAL-OVRD-VALUE.                              ELGABMCC
00846      MOVE GAA-BAMA-L-O-B (GAA-INDEX) TO ACCUM-L-O-B.              ELGABMCC
00847      EVALUATE TRUE ALSO TRUE                                      ELGABMCC
00848         WHEN      SW-INTRNL-INST-PROV-CLASS                       ELGABMCC
00849              ALSO SW-INTRNL-PROF-PROV-CLASS                       ELGABMCC
00850            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELGABMCC
00851         WHEN      SW-INTRNL-INST-PROV-CLASS                       ELGABMCC
00852              ALSO SW-INTRNL-NOT-PROF-PROV-CLASS                   ELGABMCC
00853            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELGABMCC
00854         WHEN      SW-INTRNL-NOT-INST-PROV-CLASS                   ELGABMCC
00855              ALSO SW-INTRNL-PROF-PROV-CLASS                       ELGABMCC
00856            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELGABMCC
00857         WHEN OTHER                                                ELGABMCC
00858            CONTINUE                                               ELGABMCC
00859         END-EVALUATE.                                             ELGABMCC
00860       IF SW-INTRNL-PROF-PROV-SPEC                                 ELGABMCC
00861            SET ACCUM-PRVDR-SPC-PROF TO TRUE                       ELGABMCC
00862       END-IF.                                                     ELGABMCC
00863      MOVE GAA-BAMA-REINSTATEMENT-IND (GAA-INDEX)                  ELGABMCC
00864        TO ACCUM-REINSTATEMENT-IND.                                ELGABMCC
00865      MOVE GAA-BAMA-DEFINITION (GAA-INDEX) TO ACCUM-DEFINITION.    ELGABMCC
00866      SET CARRY-OVER-CREDIT-IND-NA                                 ELGABMCC
00867          ASCEND-DESCEND-IND-NA                                    ELGABMCC
00868       TO TRUE.                                                    ELGABMCC
00869      MOVE GAA-BAMA-CONDITION (GAA-INDEX) TO  ACCUM-CONDITION.     ELGABMCC
00870      MOVE GAA-BAMA-FAM-OR-INDIV (GAA-INDEX)                       ELGABMCC
00871        TO ACCUM-FAM-OR-INDIV.                                     ELGABMCC
00872      MOVE ZEROS TO ACCUM-DED-BASE-AMT-SOURCE-IND.                 ELGABMCC
00873      MOVE ZEROS TO ACCUM-OPX-BASE-AMT-SOURCE-IND.                 ELGABMCC
00874      MOVE GCG-MAX-BASE-AMT-SOURCE-IND                             ELGABMCC
00875        TO ACCUM-MAX-BASE-AMT-SOURCE-IND.                          ELGABMCC
00876      MOVE GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX)                    ELGABMCC
00877        TO ACCUM-VALUE-QUALIFIER.                                  ELGABMCC
00878      MOVE GAA-BAMA-RELATIONSHIP-IND (GAA-INDEX)                   ELGABMCC
00879        TO ACCUM-RELATIONSHIP-IND.                                 ELGABMCC
00880      MOVE GAA-BAMA-AGE-LIMIT-FROM (GAA-INDEX)                     ELGABMCC
00881        TO ACCUM-AGE-LIMIT-FROM-VAL.                               ELGABMCC
00882      MOVE GAA-BAMA-AGE-QUAL-IND-FROM (GAA-INDEX)                  ELGABMCC
00883        TO ACCUM-AGE-LIMIT-FROM-IND.                               ELGABMCC
00884      MOVE GAA-BAMA-AGE-LIMIT-TO (GAA-INDEX)                       ELGABMCC
00885        TO ACCUM-AGE-LIMIT-TO-VAL.                                 ELGABMCC
00886      MOVE GAA-BAMA-AGE-QUAL-IND-TO (GAA-INDEX)                    ELGABMCC
00887        TO ACCUM-AGE-LIMIT-TO-IND.                                 ELGABMCC
00888      SET LMT-MANDATORY-IND-NA TO TRUE.                            ELGABMCC
00889      MOVE GAA-BAMA-CO-PAY-IND (GAA-INDEX)                         ELGABMCC
00890        TO ACCUM-CO-PAY-IND (COPAY-INDEX).                         ELGABMCC
00891 *THE ABOVE LINE WILL CHANGE                                       ELGABMCC
00892      MOVE GAA-BAMA-SERVICE-GROUP (GAA-INDEX)                      ELGABMCC
00893        TO ACCUM-SERVICE-GROUP.                                    ELGABMCC
00894      MOVE GAA-BAMA-INTERNAL-DESCRIPTOR (GAA-INDEX)                ELGABMCC
00895        TO ACCUM-INTERNAL-DESCRIPTOR.                              ELGABMCC
00896      MOVE GAA-BAMA-DAY-FACTOR-IND (GAA-INDEX)                     ELGABMCC
00897        TO ACCUM-DAY-FACTOR-IND.                                   ELGABMCC
00898      MOVE GAA-BAMA-CLAIM-LVL-ACCUM-IND (GAA-INDEX)                ELGABMCC
00899        TO ACCUM-CLAIM-LVL-ACCUM-IND.                              ELGABMCC
00900      MOVE GAA-BAMA-BEN-PER-MAX-OVRD-IND (GAA-INDEX)               ELGABMCC
00901        TO ACCUM-BEN-PER-MAX-OVRD-IND.                             ELGABMCC
00902      SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE.                          ELGABMCC
00903                                                                   ELGABMCC
00904      MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                        ELGABMCC
00905        TO ACCUM-VALUE-LIMIT (1).                                  ELGABMCC
00906      MOVE WS-IBGR-SLOT-NBR TO ACCUM-IBGR-SLOT-NBR (1).            ELGABMCC
00907      MOVE WS-IDGD-SLOT-NBR TO ACCUM-IDGD-SLOT-NBR (1).            ELGABMCC
00908      MOVE WS-IPGN-SLOT-NBR TO ACCUM-IPGN-SLOT-NBR (1).            ELGABMCC
00909      MOVE WS-IPGP-SLOT-NBR TO ACCUM-IPGP-SLOT-NBR (1).            ELGABMCC
00910      MOVE WS-IPGT-SLOT-NBR TO ACCUM-IPGT-SLOT-NBR (1).            ELGABMCC
00911      MOVE WS-IPGS-SLOT-NBR TO ACCUM-IPGS-SLOT-NBR (1).            ELGABMCC
00912                                                                   ELGABMCC
00913 /***********************************************************      ELGABMCC
00914 *                                                          *      ELGABMCC
00915 *    CHECK EXTRACT DATA INTEGRITY                          *      ELGABMCC
00916 *                                                          *      ELGABMCC
00917 ************************************************************      ELGABMCC
00918                                                                   ELGABMCC
00919  0410-CHK-EXTRACT-DATA-INTGRTY.                                   ELGABMCC
00920      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELGABMCC
00921      THEN                                                         ELGABMCC
00922         SET FYI-VALUE-NA TO TRUE                                  ELGABMCC
00923      END-IF.                                                      ELGABMCC
00924                                                                   ELGABMCC
00925      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELGABMCC
00926      THEN                                                         ELGABMCC
00927         SET COST-CONTAIN-IND-NA TO TRUE                           ELGABMCC
00928      END-IF.                                                      ELGABMCC
00929                                                                   ELGABMCC
00930      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELGABMCC
00931      THEN                                                         ELGABMCC
00932         SET PLACE-OF-TREATMENT-NA TO TRUE                         ELGABMCC
00933      END-IF.                                                      ELGABMCC
00934                                                                   ELGABMCC
00935      IF ACCUM-BENEFIT-PERIOD = ZEROS OR SPACES OR LOW-VALUES      ELGABMCC
00936      THEN                                                         ELGABMCC
00937         SET BENEFIT-PERIOD-NA TO TRUE                             ELGABMCC
00938      END-IF.                                                      ELGABMCC
00939                                                                   ELGABMCC
00940      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELGABMCC
00941      THEN                                                         ELGABMCC
00942         SET INTERVAL-OVRD-IND-NA TO TRUE                          ELGABMCC
00943      END-IF.                                                      ELGABMCC
00944                                                                   ELGABMCC
00945      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELGABMCC
00946      THEN                                                         ELGABMCC
00947         SET L-O-B-NA TO TRUE                                      ELGABMCC
00948      END-IF.                                                      ELGABMCC
00949                                                                   ELGABMCC
00950      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELGABMCC
00951      THEN                                                         ELGABMCC
00952         SET REINSTATEMENT-IND-NA TO TRUE                          ELGABMCC
00953      END-IF.                                                      ELGABMCC
00954                                                                   ELGABMCC
00955      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELGABMCC
00956      THEN                                                         ELGABMCC
00957         SET DEFINITION-NA TO TRUE                                 ELGABMCC
00958      END-IF.                                                      ELGABMCC
00959                                                                   ELGABMCC
00960      IF   ACCUM-CARRY-OVER-CREDIT-IND                             ELGABMCC
00961         = ZEROS OR SPACES OR LOW-VALUES                           ELGABMCC
00962      THEN                                                         ELGABMCC
00963         SET CARRY-OVER-CREDIT-IND-NA TO TRUE                      ELGABMCC
00964      END-IF.                                                      ELGABMCC
00965                                                                   ELGABMCC
00966      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELGABMCC
00967      THEN                                                         ELGABMCC
00968         SET ASCEND-DESCEND-IND-NA TO TRUE                         ELGABMCC
00969      END-IF.                                                      ELGABMCC
00970                                                                   ELGABMCC
00971      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELGABMCC
00972      THEN                                                         ELGABMCC
00973         SET RELATIONSHIP-IND-NA TO TRUE                           ELGABMCC
00974      END-IF.                                                      ELGABMCC
00975                                                                   ELGABMCC
00976      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELGABMCC
00977      THEN                                                         ELGABMCC
00978         SET AGE-LMT-TO-IND-NA TO TRUE                             ELGABMCC
00979      END-IF.                                                      ELGABMCC
00980                                                                   ELGABMCC
00981      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELGABMCC
00982      THEN                                                         ELGABMCC
00983         SET AGE-LMT-FROM-IND-NA TO TRUE                           ELGABMCC
00984      END-IF.                                                      ELGABMCC
00985                                                                   ELGABMCC
00986      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELGABMCC
00987      THEN                                                         ELGABMCC
00988         SET LMT-MANDATORY-IND-NA TO TRUE                          ELGABMCC
00989      END-IF.                                                      ELGABMCC
00990                                                                   ELGABMCC
00991      IF ACCUM-CO-PAY-IND (COPAY-INDEX)                            ELGABMCC
00992                     = ZEROS OR SPACES OR LOW-VALUES               ELGABMCC
00993      THEN                                                         ELGABMCC
00994         SET CO-PAY-IND-NA (COPAY-INDEX) TO TRUE                   ELGABMCC
00995      END-IF.                                                      ELGABMCC
00996                                                                   ELGABMCC
00997      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELGABMCC
00998      THEN                                                         ELGABMCC
00999         SET SERVICE-GROUP-NA TO TRUE                              ELGABMCC
01000      END-IF.                                                      ELGABMCC
01001                                                                   ELGABMCC
01002      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELGABMCC
01003      THEN                                                         ELGABMCC
01004         SET INTERNAL-DESCRIPTOR-NA TO TRUE                        ELGABMCC
01005      END-IF.                                                      ELGABMCC
01006                                                                   ELGABMCC
01007      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELGABMCC
01008      THEN                                                         ELGABMCC
01009         SET DAY-FACTOR-IND-NA TO TRUE                             ELGABMCC
01010      END-IF.                                                      ELGABMCC
01011                                                                   ELGABMCC
01012      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELGABMCC
01013      THEN                                                         ELGABMCC
01014         SET CLAIM-LVL-ACCUM-IND-NA TO TRUE                        ELGABMCC
01015      END-IF.                                                      ELGABMCC
01016                                                                   ELGABMCC
01017      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELGABMCC
01018      THEN                                                         ELGABMCC
01019         SET BEN-PER-MAX-OVRD-IND-NA TO TRUE                       ELGABMCC
01020      END-IF.                                                      ELGABMCC
01021                                                                   ELGABMCC
01022      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELGABMCC
01023      THEN                                                         ELGABMCC
01024         SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE                        ELGABMCC
01025      END-IF.                                                      ELGABMCC
01026                                                                   ELGABMCC
01027 /*****************************************************************ELGABMCC
01028 *                                                                *ELGABMCC
01029 *    CHECK COST CONTAINMENT INDICATOR TO DETERMINE APPLICABILITY *ELGABMCC
01030 *                                                                *ELGABMCC
01031 ******************************************************************ELGABMCC
01032                                                                   ELGABMCC
01033  0500-CHK-CC-IND.                                                 ELGABMCC
01034      MOVE SSB-MODIFIER-1 TO WS-SUBTOPIC.                          ELGABMCC
01035      EVALUATE TRUE              ALSO TRUE                         ELGABMCC
01036         WHEN WS-SUBTOPIC-ATCP   ALSO WS-CCI-ATCP                  ELGABMCC
01037            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01038         WHEN WS-SUBTOPIC-BAE    ALSO WS-CCI-BAE                   ELGABMCC
01039            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01040         WHEN WS-SUBTOPIC-EMH    ALSO WS-CCI-EMH                   ELGABMCC
01041            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01042         WHEN WS-SUBTOPIC-HOSP   ALSO WS-CCI-HOSP                  ELGABMCC
01043            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01044         WHEN WS-SUBTOPIC-IOB    ALSO WS-CCI-IOB                   ELGABMCC
01045            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01046         WHEN WS-SUBTOPIC-MASOP  ALSO WS-CCI-MASOP                 ELGABMCC
01047            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01048         WHEN WS-SUBTOPIC-MCN    ALSO WS-CCI-MCN                   ELGABMCC
01049            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01050         WHEN WS-SUBTOPIC-MEDNEC ALSO WS-CCI-MEDNEC                ELGABMCC
01051            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01052         WHEN WS-SUBTOPIC-MHSC   ALSO WS-CCI-MHSC                  ELGABMCC
01053            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01054         WHEN WS-SUBTOPIC-MOND   ALSO WS-CCI-MOND                  ELGABMCC
01055            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01056         WHEN WS-SUBTOPIC-MOPS   ALSO WS-CCI-MOPS                  ELGABMCC
01057            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01058         WHEN WS-SUBTOPIC-MSA    ALSO WS-CCI-MSA                   ELGABMCC
01059            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01060         WHEN WS-SUBTOPIC-PAR    ALSO WS-CCI-PAR                   ELGABMCC
01061            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01062         WHEN WS-SUBTOPIC-PAT    ALSO WS-CCI-PAT                   ELGABMCC
01063            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01064         WHEN WS-SUBTOPIC-POS    ALSO WS-CCI-POS                   ELGABMCC
01065            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01066         WHEN WS-SUBTOPIC-PPO    ALSO WS-CCI-PPO                   ELGABMCC
01067            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01068         WHEN WS-SUBTOPIC-REIMB  ALSO WS-CCI-REIMB                 ELGABMCC
01069            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01070         WHEN WS-SUBTOPIC-RPO   ALSO WS-CCI-RPO                    ELGABMCC
01071            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01072         WHEN WS-SUBTOPIC-CPO   ALSO WS-CCI-CPO                    ELGABMCC
01073            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01074         WHEN WS-SUBTOPIC-CBL   ALSO WS-CCI-CBL                    ELGABMCC
01075            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01076         WHEN WS-SUBTOPIC-PAN   ALSO WS-CCI-PAN                    ELGABMCC
01077            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01078         WHEN WS-SUBTOPIC-WEEK   ALSO WS-CCI-WEEK                  ELGABMCC
01079            SET SW-CC-IND-APPLIES TO TRUE                          ELGABMCC
01080         WHEN OTHER                                                ELGABMCC
01081            CONTINUE                                               ELGABMCC
01082         END-EVALUATE.                                             ELGABMCC
01083                                                                   ELGABMCC
01084 /***********************************************************      ELGABMCC
01085 *                                                          *      ELGABMCC
01086 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER CLASS   *      ELGABMCC
01087 *                                                          *      ELGABMCC
01088 ************************************************************      ELGABMCC
01089                                                                   ELGABMCC
01090  0550-CHK-INTRNL-TAB-PROV-CLASS.                                  ELGABMCC
01091      IF SW-HAS-IPGT                                               ELGABMCC
01092      THEN                                                         ELGABMCC
01093         PERFORM 0560-CHK-IPGT-PROV-CLASS                          ELGABMCC
01094      ELSE                                                         ELGABMCC
01095         IF SW-HAS-IBGR                                            ELGABMCC
01096         THEN                                                      ELGABMCC
01097            PERFORM 0640-CHK-IBGR-PROV-CLASS                       ELGABMCC
01098         ELSE                                                      ELGABMCC
01099            SET SW-OCCRNC-APPLIES TO TRUE                          ELGABMCC
01100         END-IF                                                    ELGABMCC
01101      END-IF.                                                      ELGABMCC
01102                                                                   ELGABMCC
01103 /***********************************************************      ELGABMCC
01104 *                                                          *      ELGABMCC
01105 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER SPECS   *      ELGABMCC
01106 *                                                          *      ELGABMCC
01107 ************************************************************      ELGABMCC
01108                                                                   ELGABMCC
01109  0551-CHK-INTRNL-TAB-PROV-SPEC.                                   ELGABMCC
01110      IF SW-HAS-IPGS                                               ELGABMCC
01111         PERFORM 0561-CHK-IPGS-PROV-SPEC                           ELGABMCC
01112      END-IF.                                                      ELGABMCC
01113                                                                   ELGABMCC
01114 /*****************************************************************ELGABMCC
01115 *                                                                *ELGABMCC
01116 *    CHECK IPGT INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELGABMCC
01117 *                                                                *ELGABMCC
01118 ******************************************************************ELGABMCC
01119                                                                   ELGABMCC
01120  0561-CHK-IPGS-PROV-SPEC.                                         ELGABMCC
01121      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELGABMCC
01122      MOVE PC-IPGS TO KWA-PROVISION-ID.                            ELGABMCC
01123      MOVE WS-IPGS-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGABMCC
01124      PERFORM 0700-READ-INTRNL-TAB.                                ELGABMCC
01125      SET ADDRESS OF GXS-RECORD-AREA TO IOP-REC-PTR.               ELGABMCC
01126      SET IOP-REC-PTR TO NULLS.                                    ELGABMCC
01127      SET GXS-INDEX TO GXS-ENTRY-COUNT.                            ELGABMCC
01128      SET WS-MAX-GXS-INDEX TO GXS-INDEX.                           ELGABMCC
01129                                                                   ELGABMCC
01130      IF GXS-ID-ARGUMENT-INCLUDED                                  ELGABMCC
01131         PERFORM 0571-CHK-INCLD-TYPE-IPGS                          ELGABMCC
01132      ELSE                                                         ELGABMCC
01133          PERFORM 0601-CHK-EXCLD-TYPE-IPGS                         ELGABMCC
01134      END-IF.                                                      ELGABMCC
01135                                                                   ELGABMCC
01136 /*****************************************************************ELGABMCC
01137 *                                                                *ELGABMCC
01138 *    CHECK IPGT INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELGABMCC
01139 *                                                                *ELGABMCC
01140 ******************************************************************ELGABMCC
01141                                                                   ELGABMCC
01142  0560-CHK-IPGT-PROV-CLASS.                                        ELGABMCC
01143      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELGABMCC
01144      MOVE PC-IPGT TO KWA-PROVISION-ID.                            ELGABMCC
01145      MOVE WS-IPGT-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGABMCC
01146      PERFORM 0700-READ-INTRNL-TAB.                                ELGABMCC
01147      SET ADDRESS OF GX3-RECORD-AREA TO IOP-REC-PTR.               ELGABMCC
01148      SET IOP-REC-PTR TO NULLS.                                    ELGABMCC
01149      SET GX3-INDEX TO GX3-ENTRY-COUNT.                            ELGABMCC
01150      SET WS-MAX-GX3-INDEX TO GX3-INDEX.                           ELGABMCC
01151                                                                   ELGABMCC
01152      IF GX3-ID-ARGUMENT-INCLUDED                                  ELGABMCC
01153      THEN                                                         ELGABMCC
01154         PERFORM 0571-CHK-INCLD-TYPE-IPGS                          ELGABMCC
01155      ELSE                                                         ELGABMCC
01156          PERFORM 0601-CHK-EXCLD-TYPE-IPGS                         ELGABMCC
01157      END-IF.                                                      ELGABMCC
01158                                                                   ELGABMCC
01159 ************************************************************      ELGABMCC
01160 *                                                          *      ELGABMCC
01161 *    CHECK INCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGABMCC
01162 *                                                          *      ELGABMCC
01163 ************************************************************      ELGABMCC
01164                                                                   ELGABMCC
01165  0571-CHK-INCLD-TYPE-IPGS.                                        ELGABMCC
01166      SET CFT2-IDX TO 1.                                           ELGABMCC
01167      SET SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGABMCC
01168       TO TRUE.                                                    ELGABMCC
01169      PERFORM 0581-TEST-IPGS-INCLD-ENTRIES                         ELGABMCC
01170         VARYING GXS-INDEX  FROM 1 BY 1                            ELGABMCC
01171           UNTIL    GXS-INDEX = WS-MAX-GXS-INDEX                   ELGABMCC
01172                 OR (    SW-INTRNL-PROF-PROV-CLASS).               ELGABMCC
01173                                                                   ELGABMCC
01174 ************************************************************      ELGABMCC
01175 *                                                          *      ELGABMCC
01176 *    CHECK INCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGABMCC
01177 *                                                          *      ELGABMCC
01178 ************************************************************      ELGABMCC
01179                                                                   ELGABMCC
01180  0570-CHK-INCLD-TYPE-IPGT.                                        ELGABMCC
01181      SET CFT2-IDX TO 1.                                           ELGABMCC
01182      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGABMCC
01183          SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGABMCC
01184       TO TRUE.                                                    ELGABMCC
01185      PERFORM 0580-TEST-IPGT-INCLD-ENTRIES                         ELGABMCC
01186         VARYING GX3-INDEX  FROM 1 BY 1                            ELGABMCC
01187           UNTIL    GX3-INDEX = WS-MAX-GX3-INDEX                   ELGABMCC
01188                 OR (    SW-INTRNL-INST-PROV-CLASS                 ELGABMCC
01189                     AND SW-INTRNL-PROF-PROV-CLASS ).              ELGABMCC
01190                                                                   ELGABMCC
01191 ************************************************************      ELGABMCC
01192 *                                                          *      ELGABMCC
01193 *    TEST IPGT INCLUDE ENTRIES TO DETERMINE PROVIDER CLASS *      ELGABMCC
01194 *                                                          *      ELGABMCC
01195 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELGABMCC
01196 *          CFT2 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELGABMCC
01197 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELGABMCC
01198 *          OF THE IPGT TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELGABMCC
01199 *                                                          *      ELGABMCC
01200 ************************************************************      ELGABMCC
01201                                                                   ELGABMCC
01202  0580-TEST-IPGT-INCLD-ENTRIES.                                    ELGABMCC
01203      PERFORM WITH TEST BEFORE                                     ELGABMCC
01204         UNTIL    SW-OCCRNC-APPLIES                                ELGABMCC
01205               OR   CFT2-PT (CFT2-IDX)                             ELGABMCC
01206                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELGABMCC
01207               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELGABMCC
01208         IF   GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELGABMCC
01209            = CFT2-PT (CFT2-IDX)                                   ELGABMCC
01210         THEN                                                      ELGABMCC
01211 *    -- TEST PROVIDER CLASS                                       ELGABMCC
01212            EVALUATE TRUE                                          ELGABMCC
01213               WHEN CFT2-PT-INST (CFT2-IDX)                        ELGABMCC
01214                  SET SW-INTRNL-INST-PROV-CLASS TO TRUE            ELGABMCC
01215                  IF SRP-ACCUM-PROV-CLASS-INST                     ELGABMCC
01216                  THEN                                             ELGABMCC
01217                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGABMCC
01218                  END-IF                                           ELGABMCC
01219               WHEN CFT2-PT-PROF (CFT2-IDX)                        ELGABMCC
01220                  SET SW-INTRNL-PROF-PROV-CLASS TO TRUE            ELGABMCC
01221                  IF SRP-ACCUM-PROV-CLASS-PROF                     ELGABMCC
01222                  THEN                                             ELGABMCC
01223                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGABMCC
01224                  END-IF                                           ELGABMCC
01225               END-EVALUATE                                        ELGABMCC
01226         END-IF                                                    ELGABMCC
01227         SET CFT2-IDX UP BY 1                                      ELGABMCC
01228         END-PERFORM.                                              ELGABMCC
01229                                                                   ELGABMCC
01230 ************************************************************      ELGABMCC
01231 *                                                          *      ELGABMCC
01232 *    TEST IPGS INCLUDE ENTRIES TO DETERMINE PROVIDER SPEC  *      ELGABMCC
01233 *                                                          *      ELGABMCC
01234 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELGABMCC
01235 *          CFT9 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELGABMCC
01236 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELGABMCC
01237 *          OF THE IPGS TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELGABMCC
01238 *                                                          *      ELGABMCC
01239 ************************************************************      ELGABMCC
01240                                                                   ELGABMCC
01241  0581-TEST-IPGS-INCLD-ENTRIES.                                    ELGABMCC
01242      PERFORM WITH TEST BEFORE                                     ELGABMCC
01243         UNTIL    SW-OCCRNC-APPLIES                                ELGABMCC
01244               OR   CFT9-PT (CFT9-IDX)                             ELGABMCC
01245                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELGABMCC
01246               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELGABMCC
01247         IF   GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELGABMCC
01248            = CFT9-PT (CFT9-IDX)                                   ELGABMCC
01249 *    -- TEST PROVIDER CLASS                                       ELGABMCC
01250            EVALUATE TRUE                                          ELGABMCC
01251               WHEN CFT9-PT-PROF (CFT9-IDX)                        ELGABMCC
01252                  SET SW-INTRNL-PROF-PROV-CLASS TO TRUE            ELGABMCC
01253                  IF SRP-ACCUM-PROV-CLASS-PROF                     ELGABMCC
01254                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGABMCC
01255                  END-IF                                           ELGABMCC
01256               END-EVALUATE                                        ELGABMCC
01257         END-IF                                                    ELGABMCC
01258         SET CFT9-IDX UP BY 1                                      ELGABMCC
01259         END-PERFORM.                                              ELGABMCC
01260                                                                   ELGABMCC
01261 /***********************************************************      ELGABMCC
01262 *                                                          *      ELGABMCC
01263 *    CHECK EXCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGABMCC
01264 *                                                          *      ELGABMCC
01265 ************************************************************      ELGABMCC
01266                                                                   ELGABMCC
01267  0600-CHK-EXCLD-TYPE-IPGT.                                        ELGABMCC
01268                                                                   ELGABMCC
01269 * -- INITIALIZE CFT2 TABLE TO INCLUDE ALL PROVIDER TYPES          ELGABMCC
01270      PERFORM WITH TEST BEFORE                                     ELGABMCC
01271         VARYING CFT2-IDX FROM 1 BY 1                              ELGABMCC
01272           UNTIL CFT2-IDX > CFT2-NBR-TBL-ENTRIES                   ELGABMCC
01273         SET  CFT2-PT-INCLUDE (CFT2-IDX) TO TRUE                   ELGABMCC
01274         END-PERFORM.                                              ELGABMCC
01275                                                                   ELGABMCC
01276 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGT                 ELGABMCC
01277      SET  CFT2-IDX TO 1.                                          ELGABMCC
01278      PERFORM 0610-TAG-EXCLD-IPGT-ENTRIES                          ELGABMCC
01279         VARYING GX3-INDEX FROM 1 BY 1                             ELGABMCC
01280           UNTIL GX3-INDEX = WS-MAX-GX3-INDEX.                     ELGABMCC
01281                                                                   ELGABMCC
01282 * -- CHECK CFT2 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELGABMCC
01283      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGABMCC
01284          SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGABMCC
01285       TO TRUE.                                                    ELGABMCC
01286      PERFORM 0630-CHK-CFT2-NOT-EXCLD                              ELGABMCC
01287         VARYING CFT2-IDX FROM 1 BY 1                              ELGABMCC
01288           UNTIL    (    SW-INTRNL-INST-PROV-CLASS                 ELGABMCC
01289                     AND SW-INTRNL-PROF-PROV-CLASS )               ELGABMCC
01290                 OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES.               ELGABMCC
01291                                                                   ELGABMCC
01292 /***********************************************************      ELGABMCC
01293 *                                                          *      ELGABMCC
01294 *    CHECK EXCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGABMCC
01295 *                                                          *      ELGABMCC
01296 ************************************************************      ELGABMCC
01297  0601-CHK-EXCLD-TYPE-IPGS.                                        ELGABMCC
01298                                                                   ELGABMCC
01299 * -- INITIALIZE CFT9 TABLE TO INCLUDE ALL PROVIDER SPEC           ELGABMCC
01300      PERFORM WITH TEST BEFORE                                     ELGABMCC
01301         VARYING CFT9-IDX FROM 1 BY 1                              ELGABMCC
01302           UNTIL CFT9-IDX > CFT9-NBR-TBL-ENTRIES                   ELGABMCC
01303         SET  CFT9-PT-INCLUDE (CFT9-IDX) TO TRUE                   ELGABMCC
01304         END-PERFORM.                                              ELGABMCC
01305                                                                   ELGABMCC
01306 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGS                 ELGABMCC
01307      SET  CFT9-IDX TO 1.                                          ELGABMCC
01308      PERFORM 0611-TAG-EXCLD-IPGS-ENTRIES                          ELGABMCC
01309         VARYING GXS-INDEX FROM 1 BY 1                             ELGABMCC
01310           UNTIL GXS-INDEX = WS-MAX-GXS-INDEX.                     ELGABMCC
01311                                                                   ELGABMCC
01312 * -- CHECK CFT9 TABLE FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED ELGABMCC
01313      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGABMCC
01314          SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGABMCC
01315       TO TRUE.                                                    ELGABMCC
01316      PERFORM 0631-CHK-CFT9-NOT-EXCLD                              ELGABMCC
01317         VARYING CFT9-IDX FROM 1 BY 1                              ELGABMCC
01318           UNTIL    (    SW-INTRNL-INST-PROV-CLASS                 ELGABMCC
01319                     AND SW-INTRNL-PROF-PROV-CLASS )               ELGABMCC
01320                 OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES.               ELGABMCC
01321                                                                   ELGABMCC
01322 ************************************************************      ELGABMCC
01323 *                                                          *      ELGABMCC
01324 *    TAG EXCLUDED IPGT ENTRIES IN CFT2                     *      ELGABMCC
01325 *                                                          *      ELGABMCC
01326 ************************************************************      ELGABMCC
01327                                                                   ELGABMCC
01328  0610-TAG-EXCLD-IPGT-ENTRIES.                                     ELGABMCC
01329      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELGABMCC
01330      PERFORM WITH TEST BEFORE                                     ELGABMCC
01331         UNTIL    SW-ENTRY-FOUND                                   ELGABMCC
01332               OR   CFT2-PT (CFT2-IDX)                             ELGABMCC
01333                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELGABMCC
01334               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELGABMCC
01335         IF   CFT2-PT(CFT2-IDX)                                    ELGABMCC
01336            = GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELGABMCC
01337         THEN                                                      ELGABMCC
01338            SET SW-ENTRY-FOUND TO TRUE                             ELGABMCC
01339            SET CFT2-PT-EXCLUDE (CFT2-IDX) TO TRUE                 ELGABMCC
01340         END-IF                                                    ELGABMCC
01341         SET CFT2-IDX UP BY 1                                      ELGABMCC
01342         END-PERFORM.                                              ELGABMCC
01343                                                                   ELGABMCC
01344 ************************************************************      ELGABMCC
01345 *                                                          *      ELGABMCC
01346 *    TAG EXCLUDED IPGS ENTRIES IN CFT9                     *      ELGABMCC
01347 *                                                          *      ELGABMCC
01348 ************************************************************      ELGABMCC
01349                                                                   ELGABMCC
01350  0611-TAG-EXCLD-IPGS-ENTRIES.                                     ELGABMCC
01351      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELGABMCC
01352      PERFORM WITH TEST BEFORE                                     ELGABMCC
01353         UNTIL    SW-ENTRY-FOUND                                   ELGABMCC
01354               OR   CFT9-PT (CFT2-IDX)                             ELGABMCC
01355                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELGABMCC
01356               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELGABMCC
01357         IF   CFT9-PT(CFT2-IDX)                                    ELGABMCC
01358            = GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELGABMCC
01359         THEN                                                      ELGABMCC
01360            SET SW-ENTRY-FOUND TO TRUE                             ELGABMCC
01361            SET CFT9-PT-EXCLUDE (CFT9-IDX) TO TRUE                 ELGABMCC
01362         END-IF                                                    ELGABMCC
01363         SET CFT9-IDX UP BY 1                                      ELGABMCC
01364         END-PERFORM.                                              ELGABMCC
01365                                                                   ELGABMCC
01366 ******************************************************************ELGABMCC
01367 *                                                                *ELGABMCC
01368 *    CHECK CFT2 FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDED     *ELGABMCC
01369 *                                                                *ELGABMCC
01370 ******************************************************************ELGABMCC
01371                                                                   ELGABMCC
01372  0630-CHK-CFT2-NOT-EXCLD.                                         ELGABMCC
01373      IF CFT2-PT-INCLUDE (CFT2-IDX)                                ELGABMCC
01374      THEN                                                         ELGABMCC
01375         EVALUATE TRUE                                             ELGABMCC
01376            WHEN CFT2-PT-INST (CFT2-IDX)                           ELGABMCC
01377               SET SW-INTRNL-INST-PROV-CLASS TO TRUE               ELGABMCC
01378               IF SRP-ACCUM-PROV-CLASS-INST                        ELGABMCC
01379               THEN                                                ELGABMCC
01380                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGABMCC
01381               END-IF                                              ELGABMCC
01382            WHEN CFT2-PT-PROF (CFT2-IDX)                           ELGABMCC
01383               SET SW-INTRNL-PROF-PROV-CLASS TO TRUE               ELGABMCC
01384               IF SRP-ACCUM-PROV-CLASS-PROF                        ELGABMCC
01385               THEN                                                ELGABMCC
01386                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGABMCC
01387               END-IF                                              ELGABMCC
01388            END-EVALUATE                                           ELGABMCC
01389      END-IF.                                                      ELGABMCC
01390                                                                   ELGABMCC
01391 ******************************************************************ELGABMCC
01392 *                                                                *ELGABMCC
01393 *    CHECK CFT9 FOR CLASS(ES) OF PROVIDER SPEC  NOT EXCLUDED     *ELGABMCC
01394 *                                                                *ELGABMCC
01395 ******************************************************************ELGABMCC
01396                                                                   ELGABMCC
01397  0631-CHK-CFT9-NOT-EXCLD.                                         ELGABMCC
01398      IF CFT9-PT-INCLUDE (CFT9-IDX)                                ELGABMCC
01399      THEN                                                         ELGABMCC
01400         EVALUATE TRUE                                             ELGABMCC
01401            WHEN CFT9-PT-INST (CFT9-IDX)                           ELGABMCC
01402               SET SW-INTRNL-INST-PROV-CLASS TO TRUE               ELGABMCC
01403               IF SRP-ACCUM-PROV-CLASS-INST                        ELGABMCC
01404               THEN                                                ELGABMCC
01405                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGABMCC
01406               END-IF                                              ELGABMCC
01407            WHEN CFT9-PT-PROF (CFT9-IDX)                           ELGABMCC
01408               SET SW-INTRNL-PROF-PROV-CLASS TO TRUE               ELGABMCC
01409               IF SRP-ACCUM-PROV-CLASS-PROF                        ELGABMCC
01410               THEN                                                ELGABMCC
01411                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGABMCC
01412               END-IF                                              ELGABMCC
01413            END-EVALUATE                                           ELGABMCC
01414      END-IF.                                                      ELGABMCC
01415                                                                   ELGABMCC
01416 /*****************************************************************ELGABMCC
01417 *                                                                *ELGABMCC
01418 *    CHECK IBGR INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELGABMCC
01419 *                                                                *ELGABMCC
01420 ******************************************************************ELGABMCC
01421                                                                   ELGABMCC
01422  0640-CHK-IBGR-PROV-CLASS.                                        ELGABMCC
01423      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGABMCC
01424          SW-INTRNL-NOT-PROF-PROV-CLASS TO TRUE.                   ELGABMCC
01425      MOVE PC-IBGR TO KWA-PROVISION-ID.                            ELGABMCC
01426      MOVE WS-IBGR-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGABMCC
01427      PERFORM 0700-READ-INTRNL-TAB.                                ELGABMCC
01428      SET ADDRESS OF GX1-RECORD-AREA TO IOP-REC-PTR.               ELGABMCC
01429      SET IOP-REC-PTR TO NULLS.                                    ELGABMCC
01430      SET GX1-INDEX TO GX1-ENTRY-COUNT.                            ELGABMCC
01431      SET WS-MAX-GX1-INDEX TO GX1-INDEX.                           ELGABMCC
01432                                                                   ELGABMCC
01433      IF GX1-ID-ARGUMENT-EXCLUDED                                  ELGABMCC
01434      THEN                                                         ELGABMCC
01435 *    -- ASSUME THAT IBGR WOULD NOT EXCLUDE ALL OF ANY PROVIDER    ELGABMCC
01436 *       CLASS (I.E., BOTH TYPES APPLY).                           ELGABMCC
01437         SET SW-OCCRNC-APPLIES                                     ELGABMCC
01438             SW-INTRNL-INST-PROV-CLASS                             ELGABMCC
01439             SW-INTRNL-PROF-PROV-CLASS                             ELGABMCC
01440          TO TRUE                                                  ELGABMCC
01441      ELSE                                                         ELGABMCC
01442         PERFORM 0690-CHK-INCLD-TYPE-IBGR                          ELGABMCC
01443      END-IF.                                                      ELGABMCC
01444                                                                   ELGABMCC
01445 /*****************************************************************ELGABMCC
01446 *                                                                *ELGABMCC
01447 *    CHECK INCLUDE TYPE IBGR TO DETERMINE PROVIDER CLASS         *ELGABMCC
01448 *                                                                *ELGABMCC
01449 ******************************************************************ELGABMCC
01450                                                                   ELGABMCC
01451  0690-CHK-INCLD-TYPE-IBGR.                                        ELGABMCC
01452      PERFORM WITH TEST BEFORE                                     ELGABMCC
01453         VARYING GX1-INDEX FROM 1 BY 1                             ELGABMCC
01454           UNTIL    GX1-INDEX = WS-MAX-GX1-INDEX                   ELGABMCC
01455                 OR (    SW-INTRNL-INST-PROV-CLASS                 ELGABMCC
01456                     AND SW-INTRNL-PROF-PROV-CLASS )               ELGABMCC
01457         MOVE GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)                ELGABMCC
01458           TO WS-PROVISION-ARGUMENT                                ELGABMCC
01459         EVALUATE TRUE                                             ELGABMCC
01460            WHEN INST-CLASS                                        ELGABMCC
01461               SET SW-INTRNL-INST-PROV-CLASS TO TRUE               ELGABMCC
01462               IF SRP-ACCUM-PROV-CLASS-INST                        ELGABMCC
01463               THEN                                                ELGABMCC
01464                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGABMCC
01465               END-IF                                              ELGABMCC
01466            WHEN PROF-CLASS                                        ELGABMCC
01467               SET SW-INTRNL-PROF-PROV-CLASS TO TRUE               ELGABMCC
01468               IF SRP-ACCUM-PROV-CLASS-PROF                        ELGABMCC
01469               THEN                                                ELGABMCC
01470                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGABMCC
01471               END-IF                                              ELGABMCC
01472            END-EVALUATE                                           ELGABMCC
01473         END-PERFORM.                                              ELGABMCC
01474                                                                   ELGABMCC
01475 /***********************************************************      ELGABMCC
01476 *                                                          *      ELGABMCC
01477 *    READ THE INTERNAL TABULAR RECORD                      *      ELGABMCC
01478 *                                                          *      ELGABMCC
01479 ************************************************************      ELGABMCC
01480                                                                   ELGABMCC
01481  0700-READ-INTRNL-TAB.                                            ELGABMCC
01482      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELGABMCC
01483      SET IOP-RD TO TRUE.                                          ELGABMCC
01484      SET IOP-FCQ-NONE TO TRUE.                                    ELGABMCC
01485      SET IOP-KVQ-EQ TO TRUE.                                      ELGABMCC
01486      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELGABMCC
01487      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGABMCC
01488      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGABMCC
01489      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGABMCC
01490                                                                   ELGABMCC
01491      EVALUATE TRUE                                                ELGABMCC
01492         WHEN IOP-RC-OK                                            ELGABMCC
01493            CONTINUE                                               ELGABMCC
01494         WHEN IOP-RC-NOTFND                                        ELGABMCC
01495            SET CIA-AB-NOTFND-GCTABULR TO TRUE                     ELGABMCC
01496            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELGABMCC
01497         WHEN OTHER                                                ELGABMCC
01498             SET CIA-AB-CRITIO TO TRUE                             ELGABMCC
01499             EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC           ELGABMCC
01500         END-EVALUATE.                                             ELGABMCC
01501                                                                   ELGABMCC
01502 /***********************************************************      ELGABMCC
01503 *                                                          *      ELGABMCC
01504 *        ADD ACCUM OCCURENCE TO FILE                       *      ELGABMCC
01505 *                                                          *      ELGABMCC
01506 ************************************************************      ELGABMCC
01507                                                                   ELGABMCC
01508  0710-WRITE-EXTRACT-RECORD.                                       ELGABMCC
01509      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGABMCC
01510      SET  IOP-ADD TO TRUE.                                        ELGABMCC
01511      SET  IOP-FCQ-NONE TO TRUE.                                   ELGABMCC
01512      SET  IOP-KVQ-NONE TO TRUE.                                   ELGABMCC
01513      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGABMCC
01514                                                                   ELGABMCC
01515 /***********************************************************      ELGABMCC
01516 *                                                          *      ELGABMCC
01517 *    ESTABLISH ADDRESSABILITY OF THE TABULAR FILE          *      ELGABMCC
01518 *                                                          *      ELGABMCC
01519 ************************************************************      ELGABMCC
01520                                                                   ELGABMCC
01521  9060-EST-ADR-TABULAR-FILE.                                       ELGABMCC
01522      SET  CIA-GCTABULR-DDN TO TRUE.                               ELGABMCC
01523      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGABMCC
01524         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGABMCC
01525         END-CALL.                                                 ELGABMCC
01526      IF CIA-RC-PTR-NULL                                           ELGABMCC
01527      THEN                                                         ELGABMCC
01528         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGABMCC
01529         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGABMCC
01530      END-IF.                                                      ELGABMCC
01531                                                                   ELGABMCC
01532 /***********************************************************      ELGABMCC
01533 *                                                          *      ELGABMCC
01534 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELGABMCC
01535 *                                                          *      ELGABMCC
01536 ************************************************************      ELGABMCC
01537                                                                   ELGABMCC
01538  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELGABMCC
01539      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGABMCC
01540      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGABMCC
01541         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGABMCC
01542         END-CALL.                                                 ELGABMCC
01543      IF CIA-RC-PTR-NULL                                           ELGABMCC
01544      THEN                                                         ELGABMCC
01545         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGABMCC
01546         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGABMCC
01547      END-IF.                                                      ELGABMCC
