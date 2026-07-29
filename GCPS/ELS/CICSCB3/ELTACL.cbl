00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELTACL  
00003  PROGRAM-ID.        ELTACL.                                          LV002
00004                                                                   ELTACL  
00005  AUTHOR.            LUCY TORRES.                                  ELTACL  
00006                     RICHARD J. LUKETICH (RE-WRITE).               ELTACL  
00007                                                                   ELTACL  
00008  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELTACL  
00009                     A MUTUAL LEGAL RESERVE COMPANY                ELTACL  
00010                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELTACL  
00011                     233 N. MICHIGAN AVE                           ELTACL  
00012                     CHICAGO, ILLINOIS 60601                       ELTACL  
00013                                                                   ELTACL  
00014  DATE-WRITTEN.      03-JUN-1987.                                  ELTACL  
00015                     03-JAN-1992 (RE-WRITE).                       ELTACL  
00016                                                                   ELTACL  
00017  DATE-COMPILED.                                                   ELTACL  
00018                                                                   ELTACL  
00019  SECURITY.          COPYRIGHT 1986, 1992,                         ELTACL  
00020                     HEALTH CARE SERVICE CORPORATION               ELTACL  
00021                                                                   ELTACL  
00022  ENVIRONMENT DIVISION.                                            ELTACL  
00023                                                                   ELTACL  
00024  CONFIGURATION SECTION.                                           ELTACL  
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELTACL  
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELTACL  
00027                                                                   ELTACL  
00028 /*****************************************************************ELTACL  
00029 *                                                                *ELTACL  
00030 *  ELTACL - ELS:    SELECTS #ACL (COINSURANCE) ACCUMULATORS AND  *ELTACL  
00031 *                   SETUPS THE INFORMATION TO BE PROCESSED BY    *ELTACL  
00032 *                   THE COINSURANCE GENERATOR MODULE.  THE ACCUMS*ELTACL  
00033 *                   ARE SELECTED FROM THE GROUP SPECIFIC AND     *ELTACL  
00034 *                   CONTRACT LEVEL PROCESSING.                   *ELTACL  
00035 *                                                                *ELTACL  
00036 ******************************************************************ELTACL  
00037 *                                                                *ELTACL  
00038 *                      MAINTENANCE HISTORY                       *ELTACL  
00039 *                                                                *ELTACL  
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELTACL  
00041 * ----- ----------- --- ----- ---------------------------------- *ELTACL  
00042 * 01.00 03-JUN-1987 LET       CREATED                            *ELTACL  
00043 * 01.01 25-SEP-1987 LET       ADDED DEFINITION DATA FIELD        *ELTACL  
00044 *                                                                *ELTACL  
00045 * 01.02 17-NOV-1987 REB       MADE CHANGES TO CORRESPOND TO NEW  *ELTACL  
00046 *                             VERSION OF COPYBOOK ELSACUMC.      *ELTACL  
00047 *                                                                *ELTACL  
00048 * 01.07    SEP-1991 RKH    1. ADDED LOGIC FOR:                   *ELTACL  
00049 *    ISSR #12010                A.  NEW PATIENT AGE FIELDS       *ELTACL  
00050 *                               B.  RELATIONSHIP IND VALUE       *ELTACL  
00051 *                          2. REVISE LOGIC TO LOAD INT ACCUMS    *ELTACL  
00052 *                             INTO VARIABLE LEVEL TABLE          *ELTACL  
00053 *                          3. ADDED COPYBOOKS :                  *ELTACL  
00054 *                               A. GCTIBGR   - IBGR TAB          *ELTACL  
00055 *                               B. GCTIPGT   - IPGT TAB          *ELTACL  
00056 *                               C. ELSCFTB2  - PROVIDER TYPE     *ELTACL  
00057 *                                         COMPARE TABLE          *ELTACL  
00058 *                          4. ADD LOGIC TO INSPECT #IPGT AND     *ELTACL  
00059 *                             #IBGR INT TABS TO DETERMINE IF     *ELTACL  
00060 *                             AN OCCURRANCE IS THE SELECTED      *ELTACL  
00061 *                             PROVIDER CLASS.                    *ELTACL  
00062 *                                                                *ELTACL  
00063 * 02.01 04-FEB-1992 JPB       CLONED FROM ELTABM, MADE CHANGES   *ELTACL  
00064 *                             FOR COINSURANCE ASC/DES LOGIC.     *ELTACL  
00065 *                                                                *ELTACL  
00066 * 02.02 25-AUG-2000 AKK       ADDED SUPPORT FOR #IPGS             ELTACL  
00067 *                                                                *ELTACL  
00068 * 02.03 28-AUG-2000 AKK       ADDED FIX FOR ASRAS OCCURRING,      ELTACL  
00069 *                             FOR THIS FIX I INITIALIZED         *ELTACL  
00070 *                             THE WS IPGS SLOT TO ZEROES AS      *ELTACL  
00071 *                             THERE ARE NO IPGS' YET.           * ELTACL  
00072 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST     ELTACL  
00073 *                                                                *ELTACL  
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00056 *                                                                *ELGABMCC
00074 ******************************************************************ELTACL  
00075      TITLE  'ELTACL          WORKING STORAGE'.                    ELTACL  
00076  DATA DIVISION.                                                   ELTACL  
00077                                                                   ELTACL  
00078  WORKING-STORAGE SECTION.                                         ELTACL  
00079                                                                   ELTACL  
00080  01  SWITCHES.                                                    ELTACL  
00081      02                                      PICTURE  X(01).      ELTACL  
00082         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELTACL  
00083         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELTACL  
00084                                                                   ELTACL  
00085      02 OCCURRENCE-APPLIES                   PICTURE  X(01).      ELTACL  
00086         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELTACL  
00087         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELTACL  
00088      02                                      PICTURE  X(01).      ELTACL  
00089         88 SW-HAS-IBGR                       VALUE 'Y'.           ELTACL  
00090         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELTACL  
00091      02                                      PICTURE  X(01).      ELTACL  
00092         88 SW-HAS-IDGD                       VALUE 'Y'.           ELTACL  
00093         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELTACL  
00094      02                                      PICTURE  X(01).      ELTACL  
00095         88 SW-HAS-IPGN                       VALUE 'Y'.           ELTACL  
00096         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELTACL  
00097      02                                      PICTURE  X(01).      ELTACL  
00098         88 SW-HAS-IPGP                       VALUE 'Y'.           ELTACL  
00099         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELTACL  
00100      02                                      PICTURE  X(01).      ELTACL  
00101         88 SW-HAS-IPGT                       VALUE 'Y'.           ELTACL  
00102         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELTACL  
00103      02                                      PICTURE  X(01).      ELTACL  
00104         88 SW-HAS-IPGS                       VALUE 'Y'.           ELTACL  
00105         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELTACL  
00106      02                                      PICTURE  X(01).      ELTACL  
00107         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELTACL  
00108         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELTACL  
00109      02                                      PICTURE  X(01).      ELTACL  
00110         88 SW-INTRNL-INST-PROV-CL            VALUE 'Y'.           ELTACL  
00111         88 SW-INTRNL-NOT-INST-PROV-CL        VALUE 'N'.           ELTACL  
00112         88 SW-INTRNL-INST-PROV-CL-NOT-DET VALUE 'X'.              ELTACL  
00113      02                                      PICTURE  X(01).      ELTACL  
00114         88 SW-INTRNL-PROF-PROV-CL            VALUE 'Y'.           ELTACL  
00115         88 SW-INTRNL-NOT-PROF-PROV-CL        VALUE 'N'.           ELTACL  
00116         88 SW-INTRNL-PROF-PROV-CL-NOT-DET VALUE 'X'.              ELTACL  
00117      02                                      PICTURE  X(01).      ELTACL  
00118         88 SW-INTRNL-PROF-PROV-SP            VALUE 'Y'.           ELTACL  
00119         88 SW-INTRNL-NOT-PROF-PROV-SP        VALUE 'N'.           ELTACL  
00120         88 SW-INTRNL-PROF-PROV-SP-NOT-DET VALUE 'X'.              ELTACL  
00121      02                                      PICTURE  X(01).      ELTACL  
00122         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELTACL  
00123         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELTACL  
00124      02                                      PICTURE  X(01).      ELTACL  
00125         88 SW-MATCHING-ENTRY-FOUND           VALUE 'Y'.           ELTACL  
00126         88 SW-MATCHING-ENTRY-NOT-FOUND       VALUE 'N'.           ELTACL  
00127      02                                      PICTURE  X(01).      ELTACL  
00128         88 SW-SORT-COMPLETED                 VALUE 'Y'.           ELTACL  
00129         88 SW-SORT-NOT-COMPLETED             VALUE 'N'.           ELTACL  
00130                                                                   ELTACL  
00131  01  WS-PROVISION-ARGUMENT.                                       ELTACL  
00132      02                          PICTURE  X(05).                  ELTACL  
00133      02 WS-PROVISION-CL          PICTURE  X(01).                  ELTACL  
00134         88 INST-CL               VALUE 'A', 'B', 'W'.             ELTACL  
00135         88 PROF-CL               VALUE 'C', 'D', 'E'.             ELTACL  
00136                                                                   ELTACL  
00137  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELTACL  
00138      88 WS-LOB-INST              VALUE '1'.                       ELTACL  
00139      88 WS-LOB-PROF              VALUE '2'.                       ELTACL  
00140      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELTACL  
00141      88 WS-LOB-BOTH              VALUE '3', '4', '5', '6', '7'.   ELTACL  
00142                                                                   ELTACL  
00143  01  PROGRAM-CONSTANTS.                                           ELTACL  
00144      02 PC-ACL                   PICTURE  X(06) VALUE '#ACL  '.   ELTACL  
00145      02 PC-GCT-MAX-SUB           PICTURE S9(04) COMP.             ELTACL  
00146      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELTACL  
00147      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELTACL  
00148      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELTACL  
00149      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELTACL  
00150      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELTACL  
00151      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGS '.   ELTACL  
00152      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELTACL  
00153                                                                   ELTACL  
00154  01  WS-WORK-FIELDS.                                              ELTACL  
00155      02 WS-ACL-SUB               PICTURE S9(04) COMP.             ELTACL  
00156      02 WS-ACL-ACCUM-CNT         PICTURE S9(04) COMP.             ELTACL  
00157      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELTACL  
00158      02 WS-OCCURRENCE-SUB        PICTURE S9(04) COMP.             ELTACL  
00159      02 WS-SAVE-SUB              PICTURE S9(04) COMP.             ELTACL  
00160      02 WS-SAVE-INDEX            USAGE IS INDEX.                  ELTACL  
00161      02 SORT-SUB                 PICTURE S9(04) COMP.             ELTACL  
00162      02 TEST-SUB                 PICTURE S9(04) COMP.             ELTACL  
00163                                                                   ELTACL  
00164  01  WS-MAX-INDEX-VALUES.                                         ELTACL  
00165      02 WS-MAX-GX1-INDEX         USAGE IS INDEX.                  ELTACL  
00166      02 WS-MAX-GX3-INDEX         USAGE IS INDEX.                  ELTACL  
00167      02 WS-MAX-GXS-INDEX         USAGE IS INDEX.                  ELTACL  
00168      02 WS-MAX-GAB-INDEX         USAGE IS INDEX.                  ELTACL  
00169      02 WS-MAX-GAB-INT-INDEX     USAGE IS INDEX.                  ELTACL  
00170      02 WS-MAX-GCT-INDEX         USAGE IS INDEX.                  ELTACL  
00171      02 WS-MAX-GCG-INDEX         USAGE IS INDEX.                  ELTACL  
00172                                                                   ELTACL  
00173  01  WS-POINTERS.                                                 ELTACL  
00174      02  WS-INST-CNTRCT-PTR      POINTER.                         ELTACL  
00175      02  WS-PROF-CNTRCT-PTR      POINTER.                         ELTACL  
00176                                                                   ELTACL  
00177  01  ACCUM-HOLD-TBL.                                              ELTACL  
00178      02  ACCUM-SLOT-NBR          PICTURE S9(07) COMP-3            ELTACL  
00179                                  OCCURS 5 TIMES.                  ELTACL  
00180                                                                   ELTACL  
00181  01  WS-OCCURRENCE-PROCESSED-TBL.                                 ELTACL  
00182      02                          PICTURE X                        ELTACL  
00183                                  OCCURS 44 TIMES                  ELTACL  
00184                                  INDEXED BY WS-OCCURRENCE-INDEX.  ELTACL  
00185          88  WS-OCCURRENCE-PROCESSED           VALUE 'P'.         ELTACL  
00186          88  WS-OCCURRENCE-NOT-PROCESSED       VALUE ' '.         ELTACL  
00187                                                                   ELTACL  
00188  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELTACL  
00189      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTACL  
00190      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTACL  
00191      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTACL  
00192      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTACL  
00193      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTACL  
00194      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3 VALUE ZERO. ELTACL  
00195                                                                   ELTACL  
00196  01  WS-ASCEND-DESCEND-ENTRY-HOLD PICTURE X(28).                  ELTACL  
00197                                                                   ELTACL  
00198 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELTACL  
00199      COPY ELSCFTB2.                                               ELTACL  
00200                                                                   ELTACL  
00201 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELTACL  
00202      COPY ELSCFTB9.                                               ELTACL  
00203                                                                   ELTACL  
00204      TITLE  'ELTACL          LINKAGE SECTION'                     ELTACL  
00205  LINKAGE SECTION.                                                 ELTACL  
00206  01  DFHCOMMAREA.                                                 ELTACL  
00207      COPY ELSCOMMC.                                               ELTACL  
00208 /                                                                 ELTACL  
00209      COPY ELSCIA2C.                                               ELTACL  
00210 /                                                                 ELTACL  
00211      COPY ELSIOPMC.                                               ELTACL  
00212 /                                                                 ELTACL  
00213      COPY ELSKEYSC.                                               ELTACL  
00214 /                                                                 ELTACL  
00215      COPY ELSSRTPC.                                               ELTACL  
00216 /                                                                 ELTACL  
00217      COPY ELSSSCBC.                                               ELTACL  
00218 /                                                                 ELTACL  
00219  01  GCG-GRP-SPEC-RECORD-AREA.                                    ELTACL  
00220      COPY GCGROUPC.                                               ELTACL  
00221 /                                                                 ELTACL  
00222  01  GCT-CONTRACT-RECORD-AREA.                                    ELTACL  
00223      COPY GCCONTRC.                                               ELTACL  
00224 /                                                                 ELTACL  
00225  01  GAB-RECORD-AREA.                                             ELTACL  
00226      COPY GCTACLC.                                                ELTACL  
00227 /                                                                 ELTACL  
00228      COPY ELSACUMC.                                               ELTACL  
00229 /                                                                 ELTACL  
00230  01  GX1-RECORD-AREA.                                             ELTACL  
00231      COPY GCTIBGRC.                                               ELTACL  
00232 /                                                                 ELTACL  
00233  01  GX3-RECORD-AREA.                                             ELTACL  
00234      COPY GCTIPGTC.                                               ELTACL  
00235 /                                                                 ELTACL  
00236  01  GXS-RECORD-AREA.                                             ELTACL  
00237      COPY GCTIPGSC.                                               ELTACL  
00238      TITLE  'ELTACL          PROCEDURE DIVISION'.                 ELTACL  
00239 ************************************************************      ELTACL  
00240 *                                                          *      ELTACL  
00241 *    ELTACL MAINLINE                                       *      ELTACL  
00242 *                                                          *      ELTACL  
00243 ************************************************************      ELTACL  
00244                                                                   ELTACL  
00245  PROCEDURE DIVISION.                                              ELTACL  
00246                                                                   ELTACL  
00247      PERFORM 0010-INITIALIZATION.                                 ELTACL  
00248      PERFORM 0100-PROCESS.                                        ELTACL  
00249      GOBACK.                                                      ELTACL  
00250                                                                   ELTACL  
00251 ************************************************************      ELTACL  
00252 *                                                          *      ELTACL  
00253 *    INITIALIZATION                                        *      ELTACL  
00254 *                                                          *      ELTACL  
00255 ************************************************************      ELTACL  
00256                                                                   ELTACL  
00257  0010-INITIALIZATION.                                             ELTACL  
00258      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELTACL  
00259      PERFORM 0060-EST-ADR-KEY-WK-AREA.                            ELTACL  
00260      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELTACL  
00261      PERFORM 0120-EST-ADR-GRP-SPC.                                ELTACL  
00262      PERFORM 0190-INIT-DATA.                                      ELTACL  
00263                                                                   ELTACL  
00264 ************************************************************      ELTACL  
00265 *                                                          *      ELTACL  
00266 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELTACL  
00267 *                                                          *      ELTACL  
00268 ************************************************************      ELTACL  
00269                                                                   ELTACL  
00270  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELTACL  
00271      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTACL  
00272      THEN                                                         ELTACL  
00273         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELTACL  
00274      ELSE                                                         ELTACL  
00275         IF ECA-CIA-PTR = NULL                                     ELTACL  
00276         THEN                                                      ELTACL  
00277            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELTACL  
00278         ELSE                                                      ELTACL  
00279            CALL 'ELUINISM' USING DFHCOMMAREA                      ELTACL  
00280               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELTACL  
00281               END-CALL                                            ELTACL  
00282            SET CIA-ELSSSCB-DDN TO TRUE                            ELTACL  
00283            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELTACL  
00284               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELTACL  
00285               END-CALL                                            ELTACL  
00286            IF CIA-RC-PTR-NULL                                     ELTACL  
00287            THEN                                                   ELTACL  
00288               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELTACL  
00289               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELTACL  
00290            ELSE                                                   ELTACL  
00291               CONTINUE                                            ELTACL  
00292            END-IF                                                 ELTACL  
00293         END-IF                                                    ELTACL  
00294      END-IF.                                                      ELTACL  
00295                                                                   ELTACL  
00296 /***********************************************************      ELTACL  
00297 *                                                          *      ELTACL  
00298 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELTACL  
00299 *                                                          *      ELTACL  
00300 ************************************************************      ELTACL  
00301                                                                   ELTACL  
00302  0060-EST-ADR-KEY-WK-AREA.                                        ELTACL  
00303      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTACL  
00304      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACL  
00305         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELTACL  
00306         END-CALL.                                                 ELTACL  
00307      IF CIA-RC-PTR-NULL                                           ELTACL  
00308         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTACL  
00309         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTACL  
00310      END-IF.                                                      ELTACL  
00311                                                                   ELTACL  
00312 ************************************************************      ELTACL  
00313 *                                                          *      ELTACL  
00314 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTACL  
00315 *                                                          *      ELTACL  
00316 ************************************************************      ELTACL  
00317                                                                   ELTACL  
00318  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELTACL  
00319      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTACL  
00320      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACL  
00321         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELTACL  
00322         END-CALL.                                                 ELTACL  
00323      IF CIA-RC-PTR-NULL                                           ELTACL  
00324         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTACL  
00325         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTACL  
00326      END-IF.                                                      ELTACL  
00327                                                                   ELTACL  
00328 ************************************************************      ELTACL  
00329 *                                                          *      ELTACL  
00330 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD     *      ELTACL  
00331 *                                                          *      ELTACL  
00332 ************************************************************      ELTACL  
00333                                                                   ELTACL  
00334  0120-EST-ADR-GRP-SPC.                                            ELTACL  
00335      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTACL  
00336      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACL  
00337         ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                       ELTACL  
00338         END-CALL.                                                 ELTACL  
00339      IF CIA-RC-PTR-NULL                                           ELTACL  
00340         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTACL  
00341         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTACL  
00342      END-IF.                                                      ELTACL  
00343                                                                   ELTACL  
00344 /***********************************************************      ELTACL  
00345 *                                                          *      ELTACL  
00346 *    INITIALIZE DATA AREAS                                 *      ELTACL  
00347 *                                                          *      ELTACL  
00348 ************************************************************      ELTACL  
00349                                                                   ELTACL  
00350  0190-INIT-DATA.                                                  ELTACL  
00351      COMPUTE PC-GCT-MAX-SUB =   LENGTH OF GCT-CONT-TAB-PTRS       ELTACL  
00352                               / LENGTH OF GCT-CON-TAB-ID-SLOT.    ELTACL  
00353      SET GCT-INDEX        TO PC-GCT-MAX-SUB.                      ELTACL  
00354      SET WS-MAX-GCT-INDEX TO GCT-INDEX.                           ELTACL  
00355      SET GCG-INDEX        TO GCG-COUNT-TAB-PROVN-POINTERS.        ELTACL  
00356      SET WS-MAX-GCG-INDEX TO GCG-INDEX.                           ELTACL  
00357      INITIALIZE WS-ACL-ACCUM-CNT.                                 ELTACL  
00358      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELTACL  
00359                                                                   ELTACL  
00360 /***********************************************************      ELTACL  
00361 *                                                          *      ELTACL  
00362 *        PROCESS                                           *      ELTACL  
00363 *                                                          *      ELTACL  
00364 ************************************************************      ELTACL  
00365                                                                   ELTACL  
00366  0100-PROCESS.                                                    ELTACL  
00367      PERFORM 0110-SCAN-GRP-SPC-FOR-ACCUMS.                        ELTACL  
00368      PERFORM 0120-SCAN-CONTRACTS-FOR-ACCUMS.                      ELTACL  
00369                                                                   ELTACL  
00370      IF WS-ACL-ACCUM-CNT >  0                                     ELTACL  
00371      THEN                                                         ELTACL  
00372          PERFORM 0210-SCAN-FOR-APPLIC-OCCRNCS                     ELTACL  
00373      END-IF.                                                      ELTACL  
00374                                                                   ELTACL  
00375      IF SW-NO-APPLIC-ACCUM-FOUND                                  ELTACL  
00376      THEN                                                         ELTACL  
00377         EVALUATE TRUE                                             ELTACL  
00378            WHEN SSB-PROV-CLASS-INST                               ELTACL  
00379               SET SRP-INST-NOT-APPLICABLE TO TRUE                 ELTACL  
00380            WHEN SSB-PROV-CLASS-PROF                               ELTACL  
00381               SET SRP-PROF-NOT-APPLICABLE TO TRUE                 ELTACL  
00382            WHEN SSB-PROV-CLASS-BOTH                               ELTACL  
00383               SET SRP-NO-ACCUMS-FOUND TO TRUE                     ELTACL  
00384            WHEN OTHER                                             ELTACL  
00385               SET CIA-AB-PGM-LOGIC TO TRUE                        ELTACL  
00386               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELTACL  
00387            END-EVALUATE                                           ELTACL  
00388      END-IF.                                                      ELTACL  
00389                                                                   ELTACL  
00390      SET SRP-TOPIC-ACCUM TO TRUE.                                 ELTACL  
00391                                                                   ELTACL  
00392 * -- LINK TO THE OUTPUT GENERATOR                                 ELTACL  
00393      EXEC CICS LINK PROGRAM ('ELGACL') COMMAREA (DFHCOMMAREA)     ELTACL  
00394         END-EXEC.                                                 ELTACL  
00395                                                                   ELTACL  
00396 /***********************************************************      ELTACL  
00397 *                                                          *      ELTACL  
00398 *    SCAN GROUP SPECIFIC RECORD FOR ACCUMULATORS           *      ELTACL  
00399 *                                                          *      ELTACL  
00400 ************************************************************      ELTACL  
00401                                                                   ELTACL  
00402  0110-SCAN-GRP-SPC-FOR-ACCUMS.                                    ELTACL  
00403      PERFORM WITH TEST BEFORE                                     ELTACL  
00404         VARYING GCG-INDEX FROM 1 BY 1                             ELTACL  
00405           UNTIL GCG-INDEX = WS-MAX-GCG-INDEX                      ELTACL  
00406                 OR GCG-TAB-ID (GCG-INDEX) > PC-ACL                ELTACL  
00407 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELTACL  
00408         IF     GCG-TAB-ID (GCG-INDEX)  =  PC-ACL                  ELTACL  
00409            AND GCG-TAB-SLOT-NO (GCG-INDEX)  >  ZERO               ELTACL  
00410         THEN                                                      ELTACL  
00411 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELTACL  
00412            MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO WS-SLOT-NBR        ELTACL  
00413            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELTACL  
00414         END-IF                                                    ELTACL  
00415         END-PERFORM.                                              ELTACL  
00416                                                                   ELTACL  
00417 /***********************************************************      ELTACL  
00418 *                                                          *      ELTACL  
00419 *    SCAN CONTRACT RECORDS FOR ACCUMULATORS                *      ELTACL  
00420 *                                                          *      ELTACL  
00421 ************************************************************      ELTACL  
00422                                                                   ELTACL  
00423  0120-SCAN-CONTRACTS-FOR-ACCUMS.                                  ELTACL  
00424      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELTACL  
00425      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELTACL  
00426                                                                   ELTACL  
00427      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTACL  
00428      THEN                                                         ELTACL  
00429          PERFORM 0130-SCAN-INST-BAS                               ELTACL  
00430      END-IF.                                                      ELTACL  
00431                                                                   ELTACL  
00432      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTACL  
00433      THEN                                                         ELTACL  
00434          PERFORM 0140-SCAN-PROF-BAS                               ELTACL  
00435      END-IF.                                                      ELTACL  
00436                                                                   ELTACL  
00437      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELTACL  
00438      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELTACL  
00439                                                                   ELTACL  
00440      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTACL  
00441      THEN                                                         ELTACL  
00442          PERFORM 0150-SCAN-INST-SUP                               ELTACL  
00443      END-IF.                                                      ELTACL  
00444                                                                   ELTACL  
00445      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTACL  
00446      THEN                                                         ELTACL  
00447          PERFORM 0160-SCAN-PROF-SUP                               ELTACL  
00448      END-IF.                                                      ELTACL  
00449                                                                   ELTACL  
00450 /***********************************************************      ELTACL  
00451 *                                                          *      ELTACL  
00452 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELTACL  
00453 *                                                          *      ELTACL  
00454 ************************************************************      ELTACL  
00455                                                                   ELTACL  
00456  0130-SCAN-INST-BAS.                                              ELTACL  
00457      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTACL  
00458      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACL  
00459         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTACL  
00460         END-CALL.                                                 ELTACL  
00461      SET WS-INST-CNTRCT-PTR                                       ELTACL  
00462       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTACL  
00463                                                                   ELTACL  
00464      IF CIA-RC-PTR-NULL                                           ELTACL  
00465      THEN                                                         ELTACL  
00466         CONTINUE                                                  ELTACL  
00467      ELSE                                                         ELTACL  
00468         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTACL  
00469      END-IF.                                                      ELTACL  
00470                                                                   ELTACL  
00471 ************************************************************      ELTACL  
00472 *                                                          *      ELTACL  
00473 *    SCAN PROFESSIONAL BASIC CONTRACT RECORD               *      ELTACL  
00474 *                                                          *      ELTACL  
00475 ************************************************************      ELTACL  
00476                                                                   ELTACL  
00477  0140-SCAN-PROF-BAS.                                              ELTACL  
00478      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTACL  
00479      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACL  
00480         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTACL  
00481         END-CALL.                                                 ELTACL  
00482      SET WS-PROF-CNTRCT-PTR                                       ELTACL  
00483       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTACL  
00484                                                                   ELTACL  
00485      IF    CIA-RC-PTR-NULL                                        ELTACL  
00486         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELTACL  
00487      THEN                                                         ELTACL  
00488         CONTINUE                                                  ELTACL  
00489      ELSE                                                         ELTACL  
00490         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTACL  
00491      END-IF.                                                      ELTACL  
00492                                                                   ELTACL  
00493 /***********************************************************      ELTACL  
00494 *                                                          *      ELTACL  
00495 *    SCAN INSTITUTIONAL SUPPLEMENTAL CONTRACT RECORD       *      ELTACL  
00496 *                                                          *      ELTACL  
00497 ************************************************************      ELTACL  
00498                                                                   ELTACL  
00499  0150-SCAN-INST-SUP.                                              ELTACL  
00500      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTACL  
00501      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACL  
00502         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTACL  
00503         END-CALL.                                                 ELTACL  
00504      SET WS-INST-CNTRCT-PTR                                       ELTACL  
00505       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTACL  
00506                                                                   ELTACL  
00507      IF CIA-RC-PTR-NULL                                           ELTACL  
00508      THEN                                                         ELTACL  
00509         CONTINUE                                                  ELTACL  
00510      ELSE                                                         ELTACL  
00511         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTACL  
00512      END-IF.                                                      ELTACL  
00513                                                                   ELTACL  
00514 ************************************************************      ELTACL  
00515 *                                                          *      ELTACL  
00516 *    SCAN PROFESSIONAL SUPPLEMENTAL CONTRACT RECORD        *      ELTACL  
00517 *                                                          *      ELTACL  
00518 ************************************************************      ELTACL  
00519                                                                   ELTACL  
00520  0160-SCAN-PROF-SUP.                                              ELTACL  
00521      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTACL  
00522      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACL  
00523         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTACL  
00524         END-CALL.                                                 ELTACL  
00525      SET WS-PROF-CNTRCT-PTR                                       ELTACL  
00526       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTACL  
00527                                                                   ELTACL  
00528      IF    CIA-RC-PTR-NULL                                        ELTACL  
00529         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELTACL  
00530      THEN                                                         ELTACL  
00531         CONTINUE                                                  ELTACL  
00532      ELSE                                                         ELTACL  
00533         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTACL  
00534      END-IF.                                                      ELTACL  
00535                                                                   ELTACL  
00536 /***********************************************************      ELTACL  
00537 *                                                          *      ELTACL  
00538 *    SCAN A CONTRACT RECORD FOR ACCUMULATORS               *      ELTACL  
00539 *                                                          *      ELTACL  
00540 ************************************************************      ELTACL  
00541                                                                   ELTACL  
00542  0170-SCAN-CONTRACT-FOR-ACCUMS.                                   ELTACL  
00543      PERFORM WITH TEST BEFORE                                     ELTACL  
00544         VARYING GCT-TAB-INDEX FROM 1 BY 1                         ELTACL  
00545           UNTIL GCT-TAB-INDEX > WS-MAX-GCT-INDEX                  ELTACL  
00546                 OR GCT-CON-TAB-ID (GCT-TAB-INDEX) > PC-ACL        ELTACL  
00547 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELTACL  
00548         IF     GCT-CON-TAB-ID (GCT-TAB-INDEX) = PC-ACL            ELTACL  
00549            AND GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  > ZEROS          ELTACL  
00550         THEN                                                      ELTACL  
00551 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELTACL  
00552            MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  TO  WS-SLOT-NBR ELTACL  
00553            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELTACL  
00554         END-IF                                                    ELTACL  
00555         END-PERFORM.                                              ELTACL  
00556                                                                   ELTACL  
00557 /***********************************************************      ELTACL  
00558 *                                                          *      ELTACL  
00559 *    SAVE UNIQUE ACCUMULATOR SLOT NUMBER                   *      ELTACL  
00560 *                                                          *      ELTACL  
00561 ************************************************************      ELTACL  
00562                                                                   ELTACL  
00563  0200-SAVE-UNQ-ACCUM-SLOT-NBR.                                    ELTACL  
00564                                                                   ELTACL  
00565 * -- SCAN TABLE OF ACCUM SLOT NUMBERS FOR DUPLICATE               ELTACL  
00566      SET SW-UNQ-SLOT-NBR TO TRUE.                                 ELTACL  
00567      PERFORM WITH TEST BEFORE                                     ELTACL  
00568         VARYING WS-ACL-SUB FROM 1 BY 1                            ELTACL  
00569           UNTIL    WS-ACL-SUB > WS-ACL-ACCUM-CNT                  ELTACL  
00570                 OR SW-DUP-SLOT-NBR                                ELTACL  
00571         IF WS-SLOT-NBR = ACCUM-SLOT-NBR (WS-ACL-SUB)              ELTACL  
00572         THEN                                                      ELTACL  
00573            SET SW-DUP-SLOT-NBR TO TRUE                            ELTACL  
00574         END-IF                                                    ELTACL  
00575         END-PERFORM.                                              ELTACL  
00576                                                                   ELTACL  
00577 * -- IF SLOT NUMBER IS UNIQUE, ADD IT TO THE HOLD TABLE           ELTACL  
00578      IF SW-UNQ-SLOT-NBR                                           ELTACL  
00579      THEN                                                         ELTACL  
00580         ADD 1 TO  WS-ACL-ACCUM-CNT                                ELTACL  
00581         MOVE WS-SLOT-NBR TO ACCUM-SLOT-NBR(WS-ACL-ACCUM-CNT)      ELTACL  
00582      END-IF.                                                      ELTACL  
00583                                                                   ELTACL  
00584 /***********************************************************      ELTACL  
00585 *                                                          *      ELTACL  
00586 *    SCAN ACL ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELTACL  
00587 *                                                          *      ELTACL  
00588 ************************************************************      ELTACL  
00589                                                                   ELTACL  
00590  0210-SCAN-FOR-APPLIC-OCCRNCS.                                    ELTACL  
00591      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELTACL  
00592      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELTACL  
00593      PERFORM 0220-DELETE-ACL-SUMMARY-FILE.                        ELTACL  
00594      PERFORM 0230-ALLOC-WORKFILE-REC-AREA.                        ELTACL  
00595                                                                   ELTACL  
00596 * -- READ AND SCAN EACH ACCUMULATOR TABULAR                       ELTACL  
00597      PERFORM WITH TEST BEFORE                                     ELTACL  
00598         VARYING WS-ACL-SUB FROM 1 BY 1                            ELTACL  
00599           UNTIL WS-ACL-SUB > WS-ACL-ACCUM-CNT                     ELTACL  
00600 *    -- OBTAIN ACCUMULATOR TABULAR RECORD                         ELTACL  
00601         MOVE PC-ACL TO KWA-PROVISION-ID                           ELTACL  
00602         MOVE ACCUM-SLOT-NBR (WS-ACL-SUB) TO KWA-PROVISION-SLOT-NO ELTACL  
00603         MOVE SPACES TO WS-OCCURRENCE-PROCESSED-TBL                ELTACL  
00604         PERFORM 0240-READ-TABULAR-REC                             ELTACL  
00605 *    -- SCAN ACCUMULATOR TABULAR                                  ELTACL  
00606         PERFORM WITH TEST BEFORE                                  ELTACL  
00607            VARYING WS-OCCURRENCE-SUB FROM 1 BY 1                  ELTACL  
00608            UNTIL WS-OCCURRENCE-SUB >= GAB-ENTRY-COUNT             ELTACL  
00609             IF WS-OCCURRENCE-NOT-PROCESSED (WS-OCCURRENCE-SUB)    ELTACL  
00610              THEN                                                 ELTACL  
00611              SET WS-OCCURRENCE-INDEX                              ELTACL  
00612                           GAB-INDEX                               ELTACL  
00613               TO WS-OCCURRENCE-SUB                                ELTACL  
00614               PERFORM 0300-TEST-ACL-OCCURRENCE                    ELTACL  
00615             END-IF                                                ELTACL  
00616         END-PERFORM                                               ELTACL  
00617      END-PERFORM.                                                 ELTACL  
00618                                                                   ELTACL  
00619                                                                   ELTACL  
00620 /***********************************************************      ELTACL  
00621 *                                                          *      ELTACL  
00622 *        DELETE ACL SUMMARY FILE                           *      ELTACL  
00623 *                                                          *      ELTACL  
00624 ************************************************************      ELTACL  
00625                                                                   ELTACL  
00626  0220-DELETE-ACL-SUMMARY-FILE.                                    ELTACL  
00627      SET IOP-DEL TO TRUE.                                         ELTACL  
00628      SET IOP-FCQ-NONE TO TRUE.                                    ELTACL  
00629      SET IOP-KVQ-NONE TO TRUE.                                    ELTACL  
00630      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTACL  
00631                                                                   ELTACL  
00632 /***********************************************************      ELTACL  
00633 *                                                          *      ELTACL  
00634 *    ALLOCATE WORKFILE RECORD AREA                         *      ELTACL  
00635 *                                                          *      ELTACL  
00636 ************************************************************      ELTACL  
00637                                                                   ELTACL  
00638  0230-ALLOC-WORKFILE-REC-AREA.                                    ELTACL  
00639      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELTACL  
00640      SET CIA-STG-GETMAIN TO TRUE.                                 ELTACL  
00641      SET IOP-GETMAIN-REC TO TRUE.                                 ELTACL  
00642      COMPUTE IOP-MAX-REC-LEN =                                    ELTACL  
00643              LENGTH OF ACCUM-FIXED-AREA                           ELTACL  
00644 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELTACL  
00645            + LENGTH OF ACCUM-VARIABLE-AREA                        ELTACL  
00646            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELTACL  
00647 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELTACL  
00648 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELTACL  
00649                                                                   ELTACL  
00650      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTACL  
00651      IF IOP-REC-PTR = NULLS                                       ELTACL  
00652      THEN                                                         ELTACL  
00653         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTACL  
00654         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTACL  
00655      ELSE                                                         ELTACL  
00656         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELTACL  
00657      END-IF.                                                      ELTACL  
00658                                                                   ELTACL  
00659 /***********************************************************      ELTACL  
00660 *                                                          *      ELTACL  
00661 *    READ TABULAR RECORD                                   *      ELTACL  
00662 *                                                          *      ELTACL  
00663 ************************************************************      ELTACL  
00664                                                                   ELTACL  
00665  0240-READ-TABULAR-REC.                                           ELTACL  
00666      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELTACL  
00667      SET IOP-RD TO TRUE.                                          ELTACL  
00668      SET IOP-FCQ-NONE TO TRUE.                                    ELTACL  
00669      SET IOP-KVQ-EQ TO TRUE.                                      ELTACL  
00670      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTACL  
00671      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTACL  
00672      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTACL  
00673      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTACL  
00674                                                                   ELTACL  
00675      EVALUATE TRUE                                                ELTACL  
00676        WHEN IOP-RC-OK                                             ELTACL  
00677           SET ADDRESS OF GAB-RECORD-AREA TO IOP-REC-PTR           ELTACL  
00678           SET IOP-REC-PTR TO NULLS                                ELTACL  
00679           SET GAB-INDEX   TO GAB-ENTRY-COUNT                      ELTACL  
00680           SET WS-MAX-GAB-INDEX TO GAB-INDEX                       ELTACL  
00681        WHEN IOP-RC-NOTFND                                         ELTACL  
00682           SET CIA-AB-NOTFND-GCTABULR TO TRUE                      ELTACL  
00683           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELTACL  
00684        WHEN OTHER                                                 ELTACL  
00685           SET CIA-AB-CRITIO TO TRUE                               ELTACL  
00686           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELTACL  
00687        END-EVALUATE.                                              ELTACL  
00688                                                                   ELTACL  
00689 /***********************************************************      ELTACL  
00690 *                                                          *      ELTACL  
00691 *        TEST ACL OCCURS                                   *      ELTACL  
00692 *                                                          *      ELTACL  
00693 ************************************************************      ELTACL  
00694                                                                   ELTACL  
00695  0300-TEST-ACL-OCCURRENCE.                                        ELTACL  
00696                                                                   ELTACL  
00697      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELTACL  
00698      MOVE GAB-COINS-L-O-B (GAB-INDEX) TO WS-LOB-ACCUM-OCCRNC.     ELTACL  
00699      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELTACL  
00700      PERFORM 0320-SCAN-FOR-INTERNALS.                             ELTACL  
00701      IF SW-OCCRNC-APPLIES                                         ELTACL  
00702      THEN                                                         ELTACL  
00703 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELTACL  
00704         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELTACL  
00705         PERFORM 0340-INIT-ACCUM-EXTRACT                           ELTACL  
00706         PERFORM 0350-EXTRACT-ACCUM                                ELTACL  
00707         PERFORM 0490-CHK-EXTRACT-DATA-INTGRTY                     ELTACL  
00708         PERFORM 0360-ACCUM-VBL-PORTION                            ELTACL  
00709         PERFORM 0710-WRITE-EXTRACT-RECORD                         ELTACL  
00710      END-IF.                                                      ELTACL  
00711      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-INDEX)            ELTACL  
00712          TO TRUE.                                                 ELTACL  
00713                                                                   ELTACL  
00714                                                                   ELTACL  
00715 ************************************************************      ELTACL  
00716 *                                                          *      ELTACL  
00717 *        INITIALIZE OCCURRENCE                             *      ELTACL  
00718 *                                                          *      ELTACL  
00719 ************************************************************      ELTACL  
00720                                                                   ELTACL  
00721  0310-INITIALIZE-OCCURRENCE.                                      ELTACL  
00722      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELTACL  
00723      SET SW-HAS-NO-IBGR                                           ELTACL  
00724          SW-HAS-NO-IDGD                                           ELTACL  
00725          SW-HAS-NO-IPGN                                           ELTACL  
00726          SW-HAS-NO-IPGP                                           ELTACL  
00727          SW-HAS-NO-IPGT                                           ELTACL  
00728          SW-HAS-NO-IPGS                                           ELTACL  
00729       TO TRUE.                                                    ELTACL  
00730      INITIALIZE WS-IBGR-SLOT-NBR                                  ELTACL  
00731                 WS-IDGD-SLOT-NBR                                  ELTACL  
00732                 WS-IPGN-SLOT-NBR                                  ELTACL  
00733                 WS-IPGP-SLOT-NBR                                  ELTACL  
00734                 WS-IPGT-SLOT-NBR                                  ELTACL  
00735                 WS-IPGS-SLOT-NBR.                                 ELTACL  
00736      SET SW-INTRNL-INST-PROV-CL-NOT-DET                           ELTACL  
00737          SW-INTRNL-PROF-PROV-CL-NOT-DET                           ELTACL  
00738          SW-INTRNL-PROF-PROV-SP-NOT-DET                           ELTACL  
00739       TO TRUE.                                                    ELTACL  
00740                                                                   ELTACL  
00741                                                                   ELTACL  
00742 ************************************************************      ELTACL  
00743 *                                                          *      ELTACL  
00744 *        SCAN FOR INTERNAL TABULARS                        *      ELTACL  
00745 *                                                          *      ELTACL  
00746 ************************************************************      ELTACL  
00747                                                                   ELTACL  
00748  0320-SCAN-FOR-INTERNALS.                                         ELTACL  
00749 *    (THIS IS DONE NOW IN CASE IPGT OR IBGR IS NEEDED TO DETERMINEELTACL  
00750 *     WHETHER OCCURRENCE IS INSTITUTIONAL OR PROFESSIONAL.)       ELTACL  
00751      PERFORM 0330-SCAN-THE-INTERNAL-TABULAR                       ELTACL  
00752         VARYING GAB-INT-INDEX FROM 1 BY 1                         ELTACL  
00753           UNTIL    GAB-INT-INDEX                                  ELTACL  
00754                 >= GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX).        ELTACL  
00755                                                                   ELTACL  
00756      EVALUATE TRUE ALSO TRUE                                      ELTACL  
00757         WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                        ELTACL  
00758            SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE                  ELTACL  
00759            SET SW-OCCRNC-APPLIES TO TRUE                          ELTACL  
00760         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH                 ELTACL  
00761            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELTACL  
00762            PERFORM 0550-CHK-INTRNL-TAB-PROV-CL                    ELTACL  
00763         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST                 ELTACL  
00764            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELTACL  
00765            SET SW-OCCRNC-APPLIES TO TRUE                          ELTACL  
00766         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH                 ELTACL  
00767            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELTACL  
00768            PERFORM 0550-CHK-INTRNL-TAB-PROV-CL                    ELTACL  
00769            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELTACL  
00770            PERFORM 0551-CHK-INTRNL-TAB-PROV-SP                    ELTACL  
00771         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF                 ELTACL  
00772            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELTACL  
00773            SET SW-OCCRNC-APPLIES TO TRUE                          ELTACL  
00774         WHEN OTHER                                                ELTACL  
00775            CONTINUE                                               ELTACL  
00776         END-EVALUATE.                                             ELTACL  
00777                                                                   ELTACL  
00778 /***********************************************************      ELTACL  
00779 *                                                          *      ELTACL  
00780 *        SCAN THE INTERNAL TABULARS                        *      ELTACL  
00781 *                                                          *      ELTACL  
00782 ************************************************************      ELTACL  
00783                                                                   ELTACL  
00784  0330-SCAN-THE-INTERNAL-TABULAR.                                  ELTACL  
00785      MOVE ZEROES TO WS-IPGS-SLOT-NBR.                             ELTACL  
00786      INITIALIZE WS-SLOT-NBR                                       ELTACL  
00787      IF GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX) > 0               ELTACL  
00788      THEN                                                         ELTACL  
00789         MOVE GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX)              ELTACL  
00790           TO WS-SLOT-NBR                                          ELTACL  
00791         EVALUATE GAB-INT-ID (GAB-INDEX, GAB-INT-INDEX)            ELTACL  
00792            WHEN PC-IBGR                                           ELTACL  
00793               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELTACL  
00794               SET SW-HAS-IBGR                                     ELTACL  
00795                TO TRUE                                            ELTACL  
00796            WHEN PC-IDGD                                           ELTACL  
00797               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELTACL  
00798               SET SW-HAS-IDGD                                     ELTACL  
00799                TO TRUE                                            ELTACL  
00800            WHEN PC-IPGP                                           ELTACL  
00801               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELTACL  
00802               SET SW-HAS-IPGP                                     ELTACL  
00803                TO TRUE                                            ELTACL  
00804            WHEN PC-IPGN                                           ELTACL  
00805               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELTACL  
00806               SET SW-HAS-IPGN                                     ELTACL  
00807                TO TRUE                                            ELTACL  
00808            WHEN PC-IPGT                                           ELTACL  
00809               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELTACL  
00810               SET SW-HAS-IPGT                                     ELTACL  
00811                TO TRUE                                            ELTACL  
00812            WHEN PC-IPGS                                           ELTACL  
00813               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELTACL  
00814               SET SW-HAS-IPGS                                     ELTACL  
00815                TO TRUE                                            ELTACL  
00816            WHEN OTHER                                             ELTACL  
00817               CONTINUE                                            ELTACL  
00818            END-EVALUATE                                           ELTACL  
00819      END-IF.                                                      ELTACL  
00820                                                                   ELTACL  
00821 /***********************************************************      ELTACL  
00822 *                                                          *      ELTACL  
00823 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELTACL  
00824 *                                                          *      ELTACL  
00825 ************************************************************      ELTACL  
00826                                                                   ELTACL  
00827  0340-INIT-ACCUM-EXTRACT.                                         ELTACL  
00828      INITIALIZE ACCUM-FIXED-AREA.                                 ELTACL  
00829      SET ACCUM-ACL TO TRUE.                                       ELTACL  
00830      MOVE +1 TO  ACCUM-ASCEND-DESCEND-COUNT.                      ELTACL  
00831      SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELTACL  
00832      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELTACL  
00833      INITIALIZE ACCUM-COPAY-ENTRY(1).                             ELTACL  
00834                                                                   ELTACL  
00835 /***********************************************************      ELTACL  
00836 *                                                          *      ELTACL  
00837 *        SUMMARIZE ACL TOPIC LEVEL DATA ELEMENTS           *      ELTACL  
00838 *                                                          *      ELTACL  
00839 ************************************************************      ELTACL  
00840                                                                   ELTACL  
00841  0350-EXTRACT-ACCUM.                                              ELTACL  
00842                                                                   ELTACL  
00843 * -- SET FIXED PORTION DATA ELEMENTS                              ELTACL  
00844      MOVE GAB-COINS-FYI-VALUE (GAB-INDEX) TO ACCUM-FYI-VALUE.     ELTACL  
00845      MOVE GAB-COINS-COST-CONTAIN-IND (GAB-INDEX)                  ELTACL  
00846        TO ACCUM-COST-CONTAIN-IND.                                 ELTACL  
00847      MOVE GAB-COINS-SERVICE-GROUP (GAB-INDEX)                     ELTACL  
00848        TO ACCUM-SERVICE-GROUP.                                    ELTACL  
00849      MOVE GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)                ELTACL  
00850        TO ACCUM-PLACE-OF-TREATMENT.                               ELTACL  
00851      MOVE GAB-COINS-CO-PAY-IND (GAB-INDEX)                        ELTACL  
00852        TO ACCUM-CO-PAY-IND (1).                                   ELTACL  
00853      MOVE GAB-COINS-LMT-MANDATORY-IND (GAB-INDEX)                 ELTACL  
00854        TO ACCUM-LMT-MANDATORY-IND.                                ELTACL  
00855      MOVE GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX)               ELTACL  
00856        TO ACCUM-INTERNAL-DESCRIPTOR.                              ELTACL  
00857      MOVE GAB-COINS-DAY-FACTOR-IND (GAB-INDEX)                    ELTACL  
00858        TO ACCUM-DAY-FACTOR-IND.                                   ELTACL  
00859      MOVE GAB-COINS-CLAIM-LVL-ACCUM-IND (GAB-INDEX)               ELTACL  
00860        TO ACCUM-CLAIM-LVL-ACCUM-IND.                              ELTACL  
00861      MOVE GAB-COINS-1ST-DOLR-COVRGE-LMT (GAB-INDEX)               ELTACL  
00862        TO ACCUM-1ST-DOLR-COVRGE-LMT.                              ELTACL  
00863      MOVE GAB-COINS-BENEFIT-PERIOD (GAB-INDEX)                    ELTACL  
00864        TO ACCUM-BENEFIT-PERIOD.                                   ELTACL  
00865      MOVE GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX)                ELTACL  
00866        TO ACCUM-ASCEND-DESCEND-IND.                               ELTACL  
00867      MOVE GAB-COINS-BEN-PER-TIME-FCTR (GAB-INDEX)                 ELTACL  
00868        TO ACCUM-BEN-PER-TIME-FCTR.                                ELTACL  
00869      MOVE GAB-COINS-BEN-PER-TIME-QUAL (GAB-INDEX)                 ELTACL  
00870        TO ACCUM-BEN-PER-TIME-QUAL.                                ELTACL  
00871      MOVE GAB-COINS-INTERVAL-TIME-FCTR (GAB-INDEX)                ELTACL  
00872        TO ACCUM-INTERVAL-TIME-FCTR.                               ELTACL  
00873      MOVE GAB-COINS-INTERVAL-TYPE (GAB-INDEX)                     ELTACL  
00874        TO ACCUM-INTERVAL-TYPE.                                    ELTACL  
00875      MOVE GAB-COINS-INTERVAL-OVRD-IND (GAB-INDEX)                 ELTACL  
00876        TO ACCUM-INTERVAL-OVRD-IND.                                ELTACL  
00877      MOVE GAB-COINS-INTERVAL-OVRD-VALUE (GAB-INDEX)               ELTACL  
00878        TO ACCUM-INTERVAL-OVRD-VALUE.                              ELTACL  
00879      MOVE GAB-COINS-L-O-B (GAB-INDEX) TO ACCUM-L-O-B.             ELTACL  
00880      MOVE GAB-COINS-REINSTATEMENT-IND (GAB-INDEX)                 ELTACL  
00881        TO ACCUM-REINSTATEMENT-IND.                                ELTACL  
00882      MOVE GAB-COINS-DEFINITION (GAB-INDEX) TO ACCUM-DEFINITION.   ELTACL  
00883      SET CARRY-OVER-CREDIT-IND-NA                                 ELTACL  
00884       TO TRUE.                                                    ELTACL  
00885      MOVE GAB-COINS-CONDITION (GAB-INDEX) TO ACCUM-CONDITION.     ELTACL  
00886      MOVE GAB-COINS-FAM-OR-INDIV (GAB-INDEX)                      ELTACL  
00887        TO ACCUM-FAM-OR-INDIV.                                     ELTACL  
00888      SET DED-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELTACL  
00889      SET OPX-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELTACL  
00890      SET MAX-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELTACL  
00891      MOVE GAB-COINS-VALUE-QUALIFIER (GAB-INDEX)                   ELTACL  
00892        TO ACCUM-VALUE-QUALIFIER.                                  ELTACL  
00893      MOVE GAB-COINS-RELATIONSHIP-IND (GAB-INDEX)                  ELTACL  
00894        TO ACCUM-RELATIONSHIP-IND.                                 ELTACL  
00895      MOVE GAB-COINS-AGE-LIMIT-FROM (GAB-INDEX)                    ELTACL  
00896        TO ACCUM-AGE-LIMIT-FROM-VAL.                               ELTACL  
00897      MOVE GAB-COINS-AGE-QUAL-IND-FROM (GAB-INDEX)                 ELTACL  
00898        TO ACCUM-AGE-LIMIT-FROM-IND.                               ELTACL  
00899      MOVE GAB-COINS-AGE-LIMIT-TO (GAB-INDEX)                      ELTACL  
00900        TO ACCUM-AGE-LIMIT-TO-VAL.                                 ELTACL  
00901      MOVE GAB-COINS-AGE-QUAL-IND-TO (GAB-INDEX)                   ELTACL  
00902        TO ACCUM-AGE-LIMIT-TO-IND.                                 ELTACL  
00903                                                                   ELTACL  
00904 * -- SET OCCURRENCE PROVIDER CLASS INFORMATION                    ELTACL  
00905      EVALUATE TRUE ALSO TRUE                                      ELTACL  
00906         WHEN      SW-INTRNL-INST-PROV-CL                          ELTACL  
00907              ALSO SW-INTRNL-NOT-PROF-PROV-CL                      ELTACL  
00908            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELTACL  
00909         WHEN      SW-INTRNL-NOT-INST-PROV-CL                      ELTACL  
00910              ALSO SW-INTRNL-PROF-PROV-CL                          ELTACL  
00911            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELTACL  
00912         WHEN OTHER                                                ELTACL  
00913            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELTACL  
00914         END-EVALUATE.                                             ELTACL  
00915                                                                   ELTACL  
00916 * -- SET OCCURRENCE PROVIDER SPEC INFORMATION                     ELTACL  
00917         IF SW-INTRNL-PROF-PROV-SP                                 ELTACL  
00918            SET ACCUM-PRVDR-SPC-PROF TO TRUE                       ELTACL  
00919         ELSE                                                      ELTACL  
00920            SET ACCUM-PRVDR-SPC-ALL TO TRUE                        ELTACL  
00921         END-IF.                                                   ELTACL  
00922                                                                   ELTACL  
00923 /***********************************************************      ELTACL  
00924 *                                                          *      ELTACL  
00925 *        ACCUM VBL PORTION                                 *      ELTACL  
00926 *                                                          *      ELTACL  
00927 ************************************************************      ELTACL  
00928                                                                   ELTACL  
00929  0360-ACCUM-VBL-PORTION.                                          ELTACL  
00930      SET ASC-DES-INDEX TO 1.                                      ELTACL  
00931      PERFORM 0370-EXTRACT-VARIABLE-PORTION.                       ELTACL  
00932      IF ACCUM-VARIABLE-TYPE                                       ELTACL  
00933         THEN                                                      ELTACL  
00934             PERFORM 0380-EXTRACT-ADDL-OCCURNCS                    ELTACL  
00935       END-IF.                                                     ELTACL  
00936                                                                   ELTACL  
00937 /***********************************************************      ELTACL  
00938 *                                                          *      ELTACL  
00939 *        EXTRACT VARIABLE PORTION                          *      ELTACL  
00940 *                                                          *      ELTACL  
00941 ************************************************************      ELTACL  
00942                                                                   ELTACL  
00943  0370-EXTRACT-VARIABLE-PORTION.                                   ELTACL  
00944      MOVE GAB-COINS-BISCENDING-IND (GAB-INDEX)                    ELTACL  
00945        TO ACCUM-BISCEND-IND (ASC-DES-INDEX).                      ELTACL  
00946      MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)                     ELTACL  
00947        TO ACCUM-PERCENT-LEVEL (ASC-DES-INDEX).                    ELTACL  
00948      MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                       ELTACL  
00949        TO ACCUM-VALUE-LIMIT (ASC-DES-INDEX).                      ELTACL  
00950      MOVE WS-IBGR-SLOT-NBR                                        ELTACL  
00951        TO ACCUM-IBGR-SLOT-NBR (ASC-DES-INDEX).                    ELTACL  
00952      MOVE WS-IDGD-SLOT-NBR                                        ELTACL  
00953        TO ACCUM-IDGD-SLOT-NBR (ASC-DES-INDEX).                    ELTACL  
00954      MOVE WS-IPGN-SLOT-NBR                                        ELTACL  
00955        TO ACCUM-IPGN-SLOT-NBR (ASC-DES-INDEX).                    ELTACL  
00956      MOVE WS-IPGP-SLOT-NBR                                        ELTACL  
00957        TO ACCUM-IPGP-SLOT-NBR (ASC-DES-INDEX).                    ELTACL  
00958      MOVE WS-IPGT-SLOT-NBR                                        ELTACL  
00959        TO ACCUM-IPGT-SLOT-NBR (ASC-DES-INDEX).                    ELTACL  
00960      MOVE WS-IPGS-SLOT-NBR                                        ELTACL  
00961        TO ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX).                    ELTACL  
00962      IF ACCUM-BISCEND-IND (ASC-DES-INDEX)                         ELTACL  
00963         = ZERO OR SPACES OR LOW-VALUES                            ELTACL  
00964         SET BISCEND-IND-NA (ASC-DES-INDEX) TO TRUE.               ELTACL  
00965      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB) TO TRUE.     ELTACL  
00966                                                                   ELTACL  
00967 /***********************************************************      ELTACL  
00968 *                                                          *      ELTACL  
00969 *        EXTRACT ADDITIONAL OCCURRENCES                    *      ELTACL  
00970 *                                                          *      ELTACL  
00971 ************************************************************      ELTACL  
00972                                                                   ELTACL  
00973  0380-EXTRACT-ADDL-OCCURNCS.                                      ELTACL  
00974       MOVE WS-OCCURRENCE-SUB TO WS-SAVE-SUB                       ELTACL  
00975       SET  WS-SAVE-INDEX    TO GAB-INDEX.                         ELTACL  
00976       ADD 1 TO WS-OCCURRENCE-SUB.                                 ELTACL  
00977       PERFORM 0390-TEST-SUBSEQ-OCCRNCES                           ELTACL  
00978          VARYING WS-OCCURRENCE-SUB                                ELTACL  
00979             FROM WS-OCCURRENCE-SUB BY 1                           ELTACL  
00980          UNTIL WS-OCCURRENCE-INDEX >= GAB-ENTRY-COUNT.            ELTACL  
00981       MOVE WS-SAVE-SUB TO WS-OCCURRENCE-SUB.                      ELTACL  
00982       SET  GAB-INDEX   TO WS-SAVE-INDEX.                          ELTACL  
00983                                                                   ELTACL  
00984 /***********************************************************      ELTACL  
00985 *                                                          *      ELTACL  
00986 *        TEST SUBSEQUENT OCCURRENCES                       *      ELTACL  
00987 *                                                          *      ELTACL  
00988 ************************************************************      ELTACL  
00989                                                                   ELTACL  
00990  0390-TEST-SUBSEQ-OCCRNCES.                                       ELTACL  
00991       SET GAB-INDEX                                               ELTACL  
00992           WS-OCCURRENCE-INDEX TO WS-OCCURRENCE-SUB.               ELTACL  
00993       IF WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB)              ELTACL  
00994          CONTINUE                                                 ELTACL  
00995       ELSE PERFORM 0400-TEST-OCCURRENCE.                          ELTACL  
00996                                                                   ELTACL  
00997 /***********************************************************      ELTACL  
00998 *                                                          *      ELTACL  
00999 *        TEST OCCURRENCE                                   *      ELTACL  
01000 *                                                          *      ELTACL  
01001 ************************************************************      ELTACL  
01002                                                                   ELTACL  
01003  0400-TEST-OCCURRENCE.                                            ELTACL  
01004       SET SW-MATCHING-ENTRY-NOT-FOUND TO TRUE.                    ELTACL  
01005       PERFORM 0410-TEST-KEYS-FOR-MATCH.                           ELTACL  
01006       IF SW-MATCHING-ENTRY-FOUND                                  ELTACL  
01007          PERFORM 0420-COMPLETE-TEST-OF-OCCURNCE.                  ELTACL  
01008                                                                   ELTACL  
01009 /***********************************************************      ELTACL  
01010 *                                                          *      ELTACL  
01011 *        TEST KEYS FOR MATCH                               *      ELTACL  
01012 *                                                          *      ELTACL  
01013 ************************************************************      ELTACL  
01014                                                                   ELTACL  
01015  0410-TEST-KEYS-FOR-MATCH.                                        ELTACL  
01016      IF GAB-COINS-BENEFIT-PERIOD (GAB-INDEX) =                    ELTACL  
01017              ACCUM-BENEFIT-PERIOD                                 ELTACL  
01018                        AND                                        ELTACL  
01019              GAB-COINS-FAM-OR-INDIV (GAB-INDEX) =                 ELTACL  
01020              ACCUM-FAM-OR-INDIV                                   ELTACL  
01021                        AND                                        ELTACL  
01022              GAB-COINS-L-O-B (GAB-INDEX) =                        ELTACL  
01023              ACCUM-L-O-B                                          ELTACL  
01024                        AND                                        ELTACL  
01025              GAB-COINS-FYI-VALUE (GAB-INDEX) =                    ELTACL  
01026              ACCUM-FYI-VALUE                                      ELTACL  
01027                        AND                                        ELTACL  
01028              GAB-COINS-SERVICE-GROUP (GAB-INDEX) =                ELTACL  
01029              ACCUM-SERVICE-GROUP                                  ELTACL  
01030                        AND                                        ELTACL  
01031              GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX) =           ELTACL  
01032              ACCUM-PLACE-OF-TREATMENT                             ELTACL  
01033                        AND                                        ELTACL  
01034              GAB-COND-ALL-BIT (GAB-INDEX) =                       ELTACL  
01035              ACCUM-COND-ALL-BIT                                   ELTACL  
01036                        AND                                        ELTACL  
01037              GAB-COND-EXCLUSION-BIT (GAB-INDEX) =                 ELTACL  
01038              ACCUM-COND-EXCLUSION-BIT                             ELTACL  
01039                        AND                                        ELTACL  
01040              GAB-COND-ICD-BIT (GAB-INDEX) =                       ELTACL  
01041              ACCUM-COND-ICD-BIT                                   ELTACL  
01042                        AND                                        ELTACL  
01043              GAB-COND-TB-BIT (GAB-INDEX) =                        ELTACL  
01044              ACCUM-COND-TB-BIT                                    ELTACL  
01045                        AND                                        ELTACL  
01046              GAB-COND-MENTAL-BIT (GAB-INDEX) =                    ELTACL  
01047              ACCUM-COND-MENTAL-BIT                                ELTACL  
01048                        AND                                        ELTACL  
01049              GAB-COND-DRUG-BIT (GAB-INDEX) =                      ELTACL  
01050              ACCUM-COND-DRUG-BIT                                  ELTACL  
01051                        AND                                        ELTACL  
01052              GAB-COND-ALCOHOL-BIT (GAB-INDEX) =                   ELTACL  
01053              ACCUM-COND-ALCOHOL-BIT                               ELTACL  
01054                        AND                                        ELTACL  
01055              GAB-COND-OB-COMP-BIT (GAB-INDEX) =                   ELTACL  
01056              ACCUM-COND-OB-COMP-BIT                               ELTACL  
01057                        AND                                        ELTACL  
01058              GAB-COND-OB-NORM-BIT (GAB-INDEX) =                   ELTACL  
01059              ACCUM-COND-OB-NORM-BIT                               ELTACL  
01060                        AND                                        ELTACL  
01061              GAB-COND-MALIGNANCY-BIT (GAB-INDEX) =                ELTACL  
01062              ACCUM-COND-MALIGNANCY-BIT                            ELTACL  
01063                        AND                                        ELTACL  
01064              GAB-COND-CARDIAC-DISEASE-BIT (GAB-INDEX) =           ELTACL  
01065              ACCUM-COND-CARDIAC-DISEASE-BIT                       ELTACL  
01066                        AND                                        ELTACL  
01067              GAB-COND-LIFE-THREAT-BIT (GAB-INDEX) =               ELTACL  
01068              ACCUM-COND-LIFE-THREAT-BIT                           ELTACL  
01069                        AND                                        ELTACL  
01070              GAB-COND-TMJ-BIT (GAB-INDEX) =                       ELTACL  
01071              ACCUM-COND-TMJ-BIT                                   ELTACL  
01072                        AND                                        ELTACL  
01073              GAB-COND-INF-BIT (GAB-INDEX) =                       ELTACL  
01074              ACCUM-COND-INF-BIT                                   ELTACL  
01075                        AND                                        ELTACL  
01076              GAB-COND-OBESITY-BIT (GAB-INDEX) =                   ELTACL  
01077              ACCUM-COND-OBESITY-BIT                               ELTACL  
01078                        AND                                        ELTACL  
01079              GAB-COND-KIDNEY-DISEASE-BIT (GAB-INDEX) =            ELTACL  
01080              ACCUM-COND-KIDNEY-DISEASE-BIT                        ELTACL  
01081                        AND                                        ELTACL  
01082              GAB-COND-ACCIDENT-BIT (GAB-INDEX) =                  ELTACL  
01083              ACCUM-COND-ACCIDENT-BIT                              ELTACL  
01084                        AND                                        ELTACL  
01085              GAB-COND-PRE-EXIST-BIT (GAB-INDEX) =                 ELTACL  
01086              ACCUM-COND-PRE-EXIST-BIT                             ELTACL  
01087                        AND                                        ELTACL  
01088              GAB-COND-NON-EMER-BIT (GAB-INDEX) =                  ELTACL  
01089              ACCUM-COND-NON-EMER-BIT                              ELTACL  
01090                        AND                                        ELTACL  
01091              GAB-COND-SUICIDE-BIT (GAB-INDEX) =                   ELTACL  
01092              ACCUM-COND-SUICIDE-BIT                               ELTACL  
01093                        AND                                        ELTACL  
01094              GAB-COINS-CO-PAY-IND (GAB-INDEX) =                   ELTACL  
01095              ACCUM-CO-PAY-IND (1)                                 ELTACL  
01096                        AND                                        ELTACL  
01097              GAB-COINS-COST-CONTAIN-IND (GAB-INDEX) =             ELTACL  
01098              ACCUM-COST-CONTAIN-IND                               ELTACL  
01099                        AND                                        ELTACL  
01100              GAB-COINS-LMT-MANDATORY-IND (GAB-INDEX) =            ELTACL  
01101              ACCUM-LMT-MANDATORY-IND                              ELTACL  
01102                        AND                                        ELTACL  
01103              GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX) =           ELTACL  
01104              ACCUM-ASCEND-DESCEND-IND                             ELTACL  
01105                        AND                                        ELTACL  
01106              GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX) =          ELTACL  
01107              ACCUM-INTERNAL-DESCRIPTOR                            ELTACL  
01108                        AND                                        ELTACL  
01109              GAB-COINS-VALUE-QUALIFIER (GAB-INDEX) =              ELTACL  
01110              ACCUM-VALUE-QUALIFIER                                ELTACL  
01111          THEN                                                     ELTACL  
01112          SET SW-MATCHING-ENTRY-FOUND TO TRUE.                     ELTACL  
01113                                                                   ELTACL  
01114                                                                   ELTACL  
01115 /***********************************************************      ELTACL  
01116 *                                                          *      ELTACL  
01117 *        COMPLETE TEST OF OCCURRENCE                       *      ELTACL  
01118 *                                                          *      ELTACL  
01119 ************************************************************      ELTACL  
01120                                                                   ELTACL  
01121  0420-COMPLETE-TEST-OF-OCCURNCE.                                  ELTACL  
01122      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELTACL  
01123      PERFORM 0320-SCAN-FOR-INTERNALS.                             ELTACL  
01124      IF SW-OCCRNC-APPLIES                                         ELTACL  
01125         PERFORM 0430-EXTRACT-NEXT-OCCURRENCE                      ELTACL  
01126      ELSE SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB) TO TRUE.ELTACL  
01127                                                                   ELTACL  
01128 /***********************************************************      ELTACL  
01129 *                                                          *      ELTACL  
01130 *        EXTRACT NEXT OCCURRENCE                           *      ELTACL  
01131 *                                                          *      ELTACL  
01132 ************************************************************      ELTACL  
01133                                                                   ELTACL  
01134  0430-EXTRACT-NEXT-OCCURRENCE.                                    ELTACL  
01135      ADD +1 TO ACCUM-ASCEND-DESCEND-COUNT.                        ELTACL  
01136      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELTACL  
01137      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (ASC-DES-INDEX).       ELTACL  
01138      PERFORM 0370-EXTRACT-VARIABLE-PORTION.                       ELTACL  
01139      PERFORM 0440-INSERT-NEW-ENTRY.                               ELTACL  
01140                                                                   ELTACL  
01141 /***********************************************************      ELTACL  
01142 *                                                          *      ELTACL  
01143 *        INSERT NEW ENTRY                                  *      ELTACL  
01144 *                                                          *      ELTACL  
01145 ************************************************************      ELTACL  
01146                                                                   ELTACL  
01147  0440-INSERT-NEW-ENTRY.                                           ELTACL  
01148      MOVE ACCUM-ASCEND-DESCEND-COUNT TO SORT-SUB.                 ELTACL  
01149      SET SW-SORT-NOT-COMPLETED TO TRUE.                           ELTACL  
01150      IF SORT-SUB = 1                                              ELTACL  
01151         CONTINUE                                                  ELTACL  
01152      ELSE IF ACCUM-ASCEND-ORDER                                   ELTACL  
01153              PERFORM 0450-ASCEND-INSERT                           ELTACL  
01154                UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1            ELTACL  
01155           ELSE IF ACCUM-DESCEND-ORDER                             ELTACL  
01156                   PERFORM 0460-DESCEND-INSERT                     ELTACL  
01157                     UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1       ELTACL  
01158                ELSE IF ACCUM-BISCEND-ORDER                        ELTACL  
01159                        PERFORM 0470-BISCEND-INSERT                ELTACL  
01160                          UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1. ELTACL  
01161                                                                   ELTACL  
01162                                                                   ELTACL  
01163 /***********************************************************      ELTACL  
01164 *                                                          *      ELTACL  
01165 *        ASCEND INSERT                                     *      ELTACL  
01166 *                                                          *      ELTACL  
01167 ************************************************************      ELTACL  
01168                                                                   ELTACL  
01169  0450-ASCEND-INSERT.                                              ELTACL  
01170      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELTACL  
01171      IF ACCUM-PERCENT-LEVEL (SORT-SUB) <                          ELTACL  
01172         ACCUM-PERCENT-LEVEL (TEST-SUB)                            ELTACL  
01173         PERFORM 0480-SWAP-ENTRIES                                 ELTACL  
01174      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELTACL  
01175                                                                   ELTACL  
01176 /***********************************************************      ELTACL  
01177 *                                                          *      ELTACL  
01178 *        DESCEND INSERT                                    *      ELTACL  
01179 *                                                          *      ELTACL  
01180 ************************************************************      ELTACL  
01181                                                                   ELTACL  
01182  0460-DESCEND-INSERT.                                             ELTACL  
01183      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELTACL  
01184      IF ACCUM-PERCENT-LEVEL (SORT-SUB) >                          ELTACL  
01185         ACCUM-PERCENT-LEVEL (TEST-SUB)                            ELTACL  
01186         PERFORM 0480-SWAP-ENTRIES                                 ELTACL  
01187      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELTACL  
01188                                                                   ELTACL  
01189 /***********************************************************      ELTACL  
01190 *                                                          *      ELTACL  
01191 *        BISCEND INSERT                                    *      ELTACL  
01192 *                                                          *      ELTACL  
01193 ************************************************************      ELTACL  
01194                                                                   ELTACL  
01195  0470-BISCEND-INSERT.                                             ELTACL  
01196      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELTACL  
01197      IF ACCUM-BISCEND-IND (SORT-SUB) <                            ELTACL  
01198         ACCUM-BISCEND-IND (TEST-SUB)                              ELTACL  
01199         PERFORM 0480-SWAP-ENTRIES                                 ELTACL  
01200      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELTACL  
01201                                                                   ELTACL  
01202 /***********************************************************      ELTACL  
01203 *                                                          *      ELTACL  
01204 *        SWAP ENTRIES                                      *      ELTACL  
01205 *                                                          *      ELTACL  
01206 ************************************************************      ELTACL  
01207                                                                   ELTACL  
01208  0480-SWAP-ENTRIES.                                               ELTACL  
01209      MOVE ACCUM-ASCEND-DESCEND-ENTRY (SORT-SUB)                   ELTACL  
01210        TO WS-ASCEND-DESCEND-ENTRY-HOLD.                           ELTACL  
01211      MOVE ACCUM-ASCEND-DESCEND-ENTRY (TEST-SUB)                   ELTACL  
01212        TO ACCUM-ASCEND-DESCEND-ENTRY (SORT-SUB).                  ELTACL  
01213      MOVE WS-ASCEND-DESCEND-ENTRY-HOLD                            ELTACL  
01214        TO ACCUM-ASCEND-DESCEND-ENTRY (TEST-SUB).                  ELTACL  
01215      MOVE TEST-SUB TO SORT-SUB.                                   ELTACL  
01216                                                                   ELTACL  
01217                                                                   ELTACL  
01218 /***********************************************************      ELTACL  
01219 *                                                          *      ELTACL  
01220 *    CHECK EXTRACT DATA INTEGRITY                          *      ELTACL  
01221 *                                                          *      ELTACL  
01222 ************************************************************      ELTACL  
01223                                                                   ELTACL  
01224  0490-CHK-EXTRACT-DATA-INTGRTY.                                   ELTACL  
01225      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELTACL  
01226      THEN                                                         ELTACL  
01227         SET FYI-VALUE-NA TO TRUE                                  ELTACL  
01228      END-IF.                                                      ELTACL  
01229                                                                   ELTACL  
01230      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELTACL  
01231      THEN                                                         ELTACL  
01232         SET COST-CONTAIN-IND-NA TO TRUE                           ELTACL  
01233      END-IF.                                                      ELTACL  
01234                                                                   ELTACL  
01235      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELTACL  
01236      THEN                                                         ELTACL  
01237         SET PLACE-OF-TREATMENT-NA TO TRUE                         ELTACL  
01238      END-IF.                                                      ELTACL  
01239                                                                   ELTACL  
01240      IF ACCUM-BEN-PER-TIME-QUAL = ZEROS OR SPACES OR LOW-VALUES   ELTACL  
01241      THEN                                                         ELTACL  
01242         SET BEN-PER-TIME-QUAL-NA TO TRUE                          ELTACL  
01243      END-IF.                                                      ELTACL  
01244                                                                   ELTACL  
01245      IF ACCUM-INTERVAL-TYPE = ZEROS OR SPACES OR LOW-VALUES       ELTACL  
01246      THEN                                                         ELTACL  
01247         SET INTERVAL-TYPE-NA TO TRUE                              ELTACL  
01248      END-IF.                                                      ELTACL  
01249                                                                   ELTACL  
01250      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELTACL  
01251      THEN                                                         ELTACL  
01252         SET INTERVAL-OVRD-IND-NA TO TRUE                          ELTACL  
01253      END-IF.                                                      ELTACL  
01254                                                                   ELTACL  
01255      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELTACL  
01256      THEN                                                         ELTACL  
01257         SET L-O-B-NA TO TRUE                                      ELTACL  
01258      END-IF.                                                      ELTACL  
01259                                                                   ELTACL  
01260      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELTACL  
01261      THEN                                                         ELTACL  
01262         SET REINSTATEMENT-IND-NA TO TRUE                          ELTACL  
01263      END-IF.                                                      ELTACL  
01264                                                                   ELTACL  
01265      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELTACL  
01266      THEN                                                         ELTACL  
01267         SET DEFINITION-NA TO TRUE                                 ELTACL  
01268      END-IF.                                                      ELTACL  
01269                                                                   ELTACL  
01270      IF   ACCUM-CARRY-OVER-CREDIT-IND                             ELTACL  
01271         = ZEROS OR SPACES OR LOW-VALUES                           ELTACL  
01272      THEN                                                         ELTACL  
01273         SET CARRY-OVER-CREDIT-IND-NA TO TRUE                      ELTACL  
01274      END-IF.                                                      ELTACL  
01275                                                                   ELTACL  
01276      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELTACL  
01277      THEN                                                         ELTACL  
01278         SET ASCEND-DESCEND-IND-NA TO TRUE                         ELTACL  
01279      END-IF.                                                      ELTACL  
01280                                                                   ELTACL  
01281      IF ACCUM-FAM-OR-INDIV = ZEROS OR SPACES OR LOW-VALUES        ELTACL  
01282      THEN                                                         ELTACL  
01283         SET FAM-OR-INDIV-NA TO TRUE                               ELTACL  
01284      END-IF.                                                      ELTACL  
01285                                                                   ELTACL  
01286      IF ACCUM-VALUE-QUALIFIER = ZEROS OR SPACES OR LOW-VALUES     ELTACL  
01287      THEN                                                         ELTACL  
01288         SET VALUE-QUALIFIER-NA TO TRUE                            ELTACL  
01289      END-IF.                                                      ELTACL  
01290                                                                   ELTACL  
01291      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELTACL  
01292      THEN                                                         ELTACL  
01293         SET RELATIONSHIP-IND-NA TO TRUE                           ELTACL  
01294      END-IF.                                                      ELTACL  
01295                                                                   ELTACL  
01296      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELTACL  
01297      THEN                                                         ELTACL  
01298         SET AGE-LMT-TO-IND-NA TO TRUE                             ELTACL  
01299      END-IF.                                                      ELTACL  
01300                                                                   ELTACL  
01301      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELTACL  
01302      THEN                                                         ELTACL  
01303         SET AGE-LMT-FROM-IND-NA TO TRUE                           ELTACL  
01304      END-IF.                                                      ELTACL  
01305                                                                   ELTACL  
01306      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELTACL  
01307      THEN                                                         ELTACL  
01308         SET LMT-MANDATORY-IND-NA TO TRUE                          ELTACL  
01309      END-IF.                                                      ELTACL  
01310                                                                   ELTACL  
01311      IF ACCUM-CO-PAY-IND (1) = ZEROS OR SPACES OR LOW-VALUES      ELTACL  
01312      THEN                                                         ELTACL  
01313         SET CO-PAY-IND-NA (1) TO TRUE                             ELTACL  
01314      END-IF.                                                      ELTACL  
01315                                                                   ELTACL  
01316      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELTACL  
01317      THEN                                                         ELTACL  
01318         SET SERVICE-GROUP-NA TO TRUE                              ELTACL  
01319      END-IF.                                                      ELTACL  
01320                                                                   ELTACL  
01321      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELTACL  
01322      THEN                                                         ELTACL  
01323         SET INTERNAL-DESCRIPTOR-NA TO TRUE                        ELTACL  
01324      END-IF.                                                      ELTACL  
01325                                                                   ELTACL  
01326      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELTACL  
01327      THEN                                                         ELTACL  
01328         SET DAY-FACTOR-IND-NA TO TRUE                             ELTACL  
01329      END-IF.                                                      ELTACL  
01330                                                                   ELTACL  
01331      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELTACL  
01332      THEN                                                         ELTACL  
01333         SET CLAIM-LVL-ACCUM-IND-NA TO TRUE                        ELTACL  
01334      END-IF.                                                      ELTACL  
01335                                                                   ELTACL  
01336      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELTACL  
01337      THEN                                                         ELTACL  
01338         SET BEN-PER-MAX-OVRD-IND-NA TO TRUE                       ELTACL  
01339      END-IF.                                                      ELTACL  
01340                                                                   ELTACL  
01341      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELTACL  
01342      THEN                                                         ELTACL  
01343         SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE                        ELTACL  
01344      END-IF.                                                      ELTACL  
01345                                                                   ELTACL  
01346 /***********************************************************      ELTACL  
01347 *                                                          *      ELTACL  
01348 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER CLASS   *      ELTACL  
01349 *                                                          *      ELTACL  
01350 ************************************************************      ELTACL  
01351                                                                   ELTACL  
01352  0550-CHK-INTRNL-TAB-PROV-CL.                                     ELTACL  
01353      IF SW-HAS-IPGT                                               ELTACL  
01354      THEN                                                         ELTACL  
01355         PERFORM 0560-CHK-IPGT-PROV-CL                             ELTACL  
01356      ELSE                                                         ELTACL  
01357         IF SW-HAS-IBGR                                            ELTACL  
01358         THEN                                                      ELTACL  
01359            PERFORM 0640-CHK-IBGR-PROV-CL                          ELTACL  
01360         ELSE                                                      ELTACL  
01361            SET SW-OCCRNC-APPLIES TO TRUE                          ELTACL  
01362         END-IF                                                    ELTACL  
01363      END-IF.                                                      ELTACL  
01364                                                                   ELTACL  
01365 /***********************************************************      ELTACL  
01366 *                                                          *      ELTACL  
01367 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER SPEC    *      ELTACL  
01368 *                                                          *      ELTACL  
01369 ************************************************************      ELTACL  
01370                                                                   ELTACL  
01371  0551-CHK-INTRNL-TAB-PROV-SP.                                     ELTACL  
01372      IF SW-HAS-IPGS                                               ELTACL  
01373         PERFORM 0561-CHK-IPGS-PROV-SP                             ELTACL  
01374      END-IF.                                                      ELTACL  
01375                                                                   ELTACL  
01376 /*****************************************************************ELTACL  
01377 *                                                                *ELTACL  
01378 *    CHECK IPGS INTERNAL TABULAR TO DETERMINE PROVIDER SPEC      *ELTACL  
01379 *                                                                *ELTACL  
01380 ******************************************************************ELTACL  
01381                                                                   ELTACL  
01382  0561-CHK-IPGS-PROV-SP.                                           ELTACL  
01383      MOVE PC-IPGS TO KWA-PROVISION-ID.                            ELTACL  
01384      MOVE WS-IPGS-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTACL  
01385      PERFORM 0660-READ-INTRLN-TAB.                                ELTACL  
01386      SET ADDRESS OF GXS-RECORD-AREA TO IOP-REC-PTR.               ELTACL  
01387      SET IOP-REC-PTR                TO NULLS.                     ELTACL  
01388      SET GXS-INDEX                  TO GXS-ENTRY-COUNT.           ELTACL  
01389      SET WS-MAX-GXS-INDEX           TO GXS-INDEX.                 ELTACL  
01390                                                                   ELTACL  
01391      IF GXS-ID-ARGUMENT-INCLUDED                                  ELTACL  
01392         PERFORM 0571-CHK-INCLD-IPGS                               ELTACL  
01393      ELSE                                                         ELTACL  
01394          PERFORM 0601-CHK-EXCLD-IPGS                              ELTACL  
01395      END-IF.                                                      ELTACL  
01396                                                                   ELTACL  
01397 /*****************************************************************ELTACL  
01398 *                                                                *ELTACL  
01399 *    CHECK IPGT INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELTACL  
01400 *                                                                *ELTACL  
01401 ******************************************************************ELTACL  
01402                                                                   ELTACL  
01403  0560-CHK-IPGT-PROV-CL.                                           ELTACL  
01404      MOVE PC-IPGT TO KWA-PROVISION-ID.                            ELTACL  
01405      MOVE WS-IPGT-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTACL  
01406      PERFORM 0660-READ-INTRLN-TAB.                                ELTACL  
01407      SET ADDRESS OF GX3-RECORD-AREA TO IOP-REC-PTR.               ELTACL  
01408      SET IOP-REC-PTR                TO NULLS.                     ELTACL  
01409      SET GX3-INDEX                  TO GX3-ENTRY-COUNT.           ELTACL  
01410      SET WS-MAX-GX3-INDEX           TO GX3-INDEX.                 ELTACL  
01411                                                                   ELTACL  
01412      IF GX3-ID-ARGUMENT-INCLUDED                                  ELTACL  
01413      THEN                                                         ELTACL  
01414         PERFORM 0570-CHK-INCLD-TYPE-IPGT                          ELTACL  
01415      ELSE                                                         ELTACL  
01416          PERFORM 0600-CHK-EXCLD-TYPE-IPGT                         ELTACL  
01417      END-IF.                                                      ELTACL  
01418                                                                   ELTACL  
01419 /*****************************************************************ELTACL  
01420 *                                                                *ELTACL  
01421 *    CHECK IPGS INTERNAL TABULAR TO DETERMINE PROVIDER SPEC      *ELTACL  
01422 *                                                                *ELTACL  
01423 ******************************************************************ELTACL  
01424                                                                   ELTACL  
01425 ************************************************************      ELTACL  
01426 *                                                          *      ELTACL  
01427 *    CHECK INCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELTACL  
01428 *                                                          *      ELTACL  
01429 ************************************************************      ELTACL  
01430                                                                   ELTACL  
01431  0570-CHK-INCLD-TYPE-IPGT.                                        ELTACL  
01432      SET CFT2-IDX TO 1.                                           ELTACL  
01433      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTACL  
01434          SW-INTRNL-NOT-PROF-PROV-CL                               ELTACL  
01435       TO TRUE.                                                    ELTACL  
01436      PERFORM 0580-TEST-IPGT-INCLD-ENTRIES                         ELTACL  
01437         VARYING GX3-INDEX  FROM 1 BY 1                            ELTACL  
01438           UNTIL    GX3-INDEX = WS-MAX-GX3-INDEX                   ELTACL  
01439                 OR (    SW-INTRNL-INST-PROV-CL                    ELTACL  
01440                     AND SW-INTRNL-PROF-PROV-CL ).                 ELTACL  
01441                                                                   ELTACL  
01442 ************************************************************      ELTACL  
01443 *                                                          *      ELTACL  
01444 *    CHECK INCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELTACL  
01445 *                                                          *      ELTACL  
01446 ************************************************************      ELTACL  
01447                                                                   ELTACL  
01448  0571-CHK-INCLD-IPGS.                                             ELTACL  
01449      SET CFT2-IDX TO 1.                                           ELTACL  
01450      SET SW-INTRNL-NOT-PROF-PROV-CL                               ELTACL  
01451       TO TRUE.                                                    ELTACL  
01452      PERFORM 0581-TEST-IPGS-INCLD-ENTRIES                         ELTACL  
01453         VARYING GXS-INDEX  FROM 1 BY 1                            ELTACL  
01454           UNTIL    GXS-INDEX = WS-MAX-GXS-INDEX                   ELTACL  
01455                 OR (    SW-INTRNL-PROF-PROV-CL).                  ELTACL  
01456                                                                   ELTACL  
01457 ************************************************************      ELTACL  
01458 *                                                          *      ELTACL  
01459 *    TEST IPGT INCLUDE ENTRIES TO DETERMINE PROVIDER CLASS *      ELTACL  
01460 *                                                          *      ELTACL  
01461 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELTACL  
01462 *          CFT2 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELTACL  
01463 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELTACL  
01464 *          OF THE IPGT TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELTACL  
01465 *                                                          *      ELTACL  
01466 ************************************************************      ELTACL  
01467                                                                   ELTACL  
01468  0580-TEST-IPGT-INCLD-ENTRIES.                                    ELTACL  
01469      PERFORM WITH TEST BEFORE                                     ELTACL  
01470         UNTIL    SW-OCCRNC-APPLIES                                ELTACL  
01471               OR   CFT2-PT (CFT2-IDX)                             ELTACL  
01472                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELTACL  
01473               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELTACL  
01474         IF   GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELTACL  
01475            = CFT2-PT (CFT2-IDX)                                   ELTACL  
01476         THEN                                                      ELTACL  
01477 *    -- TEST PROVIDER CLASS                                       ELTACL  
01478            EVALUATE TRUE                                          ELTACL  
01479               WHEN CFT2-PT-INST (CFT2-IDX)                        ELTACL  
01480                  SET SW-INTRNL-INST-PROV-CL TO TRUE               ELTACL  
01481                  IF SRP-ACCUM-PROV-CLASS-INST                     ELTACL  
01482                  THEN                                             ELTACL  
01483                     SET SW-OCCRNC-APPLIES TO TRUE                 ELTACL  
01484                  END-IF                                           ELTACL  
01485               WHEN CFT2-PT-PROF (CFT2-IDX)                        ELTACL  
01486                  SET SW-INTRNL-PROF-PROV-CL TO TRUE               ELTACL  
01487                  IF SRP-ACCUM-PROV-CLASS-PROF                     ELTACL  
01488                  THEN                                             ELTACL  
01489                     SET SW-OCCRNC-APPLIES TO TRUE                 ELTACL  
01490                  END-IF                                           ELTACL  
01491               END-EVALUATE                                        ELTACL  
01492         ELSE                                                      ELTACL  
01493            CONTINUE                                               ELTACL  
01494         END-IF                                                    ELTACL  
01495 *    -- BUMP TO NEXT CFT2 TABLE ENTRY                             ELTACL  
01496         SET CFT2-IDX UP BY 1                                      ELTACL  
01497         END-PERFORM.                                              ELTACL  
01498                                                                   ELTACL  
01499 ************************************************************      ELTACL  
01500 *                                                          *      ELTACL  
01501 *    TEST IPGS INCLUDE ENTRIES TO DETERMINE PROVIDER SPEC  *      ELTACL  
01502 *                                                          *      ELTACL  
01503 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELTACL  
01504 *          CFT9 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELTACL  
01505 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELTACL  
01506 *          OF THE IPGS TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELTACL  
01507 *                                                          *      ELTACL  
01508 ************************************************************      ELTACL  
01509                                                                   ELTACL  
01510  0581-TEST-IPGS-INCLD-ENTRIES.                                    ELTACL  
01511      PERFORM WITH TEST BEFORE                                     ELTACL  
01512         UNTIL    SW-OCCRNC-APPLIES                                ELTACL  
01513               OR   CFT9-PT (CFT9-IDX)                             ELTACL  
01514                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELTACL  
01515               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELTACL  
01516         IF   GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELTACL  
01517            = CFT9-PT (CFT9-IDX)                                   ELTACL  
01518 *    -- TEST PROVIDER SPEC                                        ELTACL  
01519          IF CFT9-PT-PROF (CFT9-IDX)                               ELTACL  
01520             SET SW-INTRNL-PROF-PROV-SP TO TRUE                    ELTACL  
01521             IF SRP-ACCUM-PROV-SPEC-PROF                           ELTACL  
01522                SET SW-OCCRNC-APPLIES TO TRUE                      ELTACL  
01523             END-IF                                                ELTACL  
01524         ELSE                                                      ELTACL  
01525            CONTINUE                                               ELTACL  
01526         END-IF                                                    ELTACL  
01527         END-IF                                                    ELTACL  
01528 *    -- BUMP TO NEXT CFT9 TABLE ENTRY                             ELTACL  
01529         SET CFT9-IDX UP BY 1                                      ELTACL  
01530         END-PERFORM.                                              ELTACL  
01531                                                                   ELTACL  
01532 /***********************************************************      ELTACL  
01533 *                                                          *      ELTACL  
01534 *    CHECK EXCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELTACL  
01535 *                                                          *      ELTACL  
01536 ************************************************************      ELTACL  
01537                                                                   ELTACL  
01538  0600-CHK-EXCLD-TYPE-IPGT.                                        ELTACL  
01539                                                                   ELTACL  
01540 * -- INITIALIZE CFT2 TABLE TO INCLUDE ALL PROVIDER TYPES          ELTACL  
01541      PERFORM WITH TEST BEFORE                                     ELTACL  
01542         VARYING CFT2-IDX FROM 1 BY 1                              ELTACL  
01543           UNTIL CFT2-IDX > CFT2-NBR-TBL-ENTRIES                   ELTACL  
01544         SET  CFT2-PT-INCLUDE (CFT2-IDX) TO TRUE                   ELTACL  
01545         END-PERFORM.                                              ELTACL  
01546                                                                   ELTACL  
01547 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGT                 ELTACL  
01548      SET  CFT2-IDX TO 1.                                          ELTACL  
01549      PERFORM 0610-TAG-EXCLD-IPGT-ENTRIES                          ELTACL  
01550         VARYING GX3-INDEX FROM 1 BY 1                             ELTACL  
01551           UNTIL GX3-INDEX = WS-MAX-GX3-INDEX.                     ELTACL  
01552                                                                   ELTACL  
01553 * -- CHECK CFT2 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELTACL  
01554      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTACL  
01555          SW-INTRNL-NOT-PROF-PROV-CL                               ELTACL  
01556       TO TRUE.                                                    ELTACL  
01557      PERFORM 0630-CHK-CFT2-NOT-EXCLD                              ELTACL  
01558         VARYING CFT2-IDX FROM 1 BY 1                              ELTACL  
01559           UNTIL    (    SW-INTRNL-INST-PROV-CL                    ELTACL  
01560                     AND SW-INTRNL-PROF-PROV-CL )                  ELTACL  
01561                 OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES.               ELTACL  
01562                                                                   ELTACL  
01563 /***********************************************************      ELTACL  
01564 *                                                          *      ELTACL  
01565 *    CHECK EXCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELTACL  
01566 *                                                          *      ELTACL  
01567 ************************************************************      ELTACL  
01568                                                                   ELTACL  
01569  0601-CHK-EXCLD-IPGS.                                             ELTACL  
01570                                                                   ELTACL  
01571 * -- INITIALIZE CFT9 TABLE TO INCLUDE ALL PROVIDER SPEC           ELTACL  
01572      PERFORM WITH TEST BEFORE                                     ELTACL  
01573         VARYING CFT9-IDX FROM 1 BY 1                              ELTACL  
01574           UNTIL CFT9-IDX > CFT9-NBR-TBL-ENTRIES                   ELTACL  
01575         SET  CFT9-PT-INCLUDE (CFT9-IDX) TO TRUE                   ELTACL  
01576         END-PERFORM.                                              ELTACL  
01577                                                                   ELTACL  
01578 * -- TAG ALL PROVIDER SPEC EXCLUDED BY THIS IPGS                  ELTACL  
01579      SET  CFT9-IDX TO 1.                                          ELTACL  
01580      PERFORM 0611-TAG-EXCLD-IPGS-ENTRIES                          ELTACL  
01581         VARYING GXS-INDEX FROM 1 BY 1                             ELTACL  
01582           UNTIL GXS-INDEX = WS-MAX-GXS-INDEX.                     ELTACL  
01583                                                                   ELTACL  
01584 * -- CHECK CFT9 TABLE FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED ELTACL  
01585      SET SW-INTRNL-NOT-PROF-PROV-SP                               ELTACL  
01586       TO TRUE.                                                    ELTACL  
01587      PERFORM 0631-CHK-CFT9-NOT-EXCLD                              ELTACL  
01588         VARYING CFT9-IDX FROM 1 BY 1                              ELTACL  
01589           UNTIL    (    SW-INTRNL-PROF-PROV-CL )                  ELTACL  
01590                 OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES.               ELTACL  
01591                                                                   ELTACL  
01592 ************************************************************      ELTACL  
01593 *                                                          *      ELTACL  
01594 *    TAG EXCLUDED IPGT ENTRIES IN CFT2                     *      ELTACL  
01595 *                                                          *      ELTACL  
01596 ************************************************************      ELTACL  
01597                                                                   ELTACL  
01598  0610-TAG-EXCLD-IPGT-ENTRIES.                                     ELTACL  
01599      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELTACL  
01600      PERFORM WITH TEST BEFORE                                     ELTACL  
01601         UNTIL    SW-ENTRY-FOUND                                   ELTACL  
01602               OR   CFT2-PT (CFT2-IDX)                             ELTACL  
01603                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELTACL  
01604               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELTACL  
01605         IF   CFT2-PT(CFT2-IDX)                                    ELTACL  
01606            = GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELTACL  
01607         THEN                                                      ELTACL  
01608            SET SW-ENTRY-FOUND TO TRUE                             ELTACL  
01609            SET CFT2-PT-EXCLUDE (CFT2-IDX) TO TRUE                 ELTACL  
01610            SET CFT2-IDX UP BY 1                                   ELTACL  
01611         ELSE                                                      ELTACL  
01612            SET CFT2-IDX UP BY 1                                   ELTACL  
01613         END-IF                                                    ELTACL  
01614         END-PERFORM.                                              ELTACL  
01615                                                                   ELTACL  
01616 ************************************************************      ELTACL  
01617 *                                                          *      ELTACL  
01618 *    TAG EXCLUDED IPGS ENTRIES IN CFT9                     *      ELTACL  
01619 *                                                          *      ELTACL  
01620 ************************************************************      ELTACL  
01621                                                                   ELTACL  
01622  0611-TAG-EXCLD-IPGS-ENTRIES.                                     ELTACL  
01623      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELTACL  
01624      PERFORM WITH TEST BEFORE                                     ELTACL  
01625         UNTIL    SW-ENTRY-FOUND                                   ELTACL  
01626               OR   CFT9-PT (CFT9-IDX)                             ELTACL  
01627                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELTACL  
01628               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELTACL  
01629         IF   CFT9-PT(CFT9-IDX)                                    ELTACL  
01630            = GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELTACL  
01631            SET SW-ENTRY-FOUND TO TRUE                             ELTACL  
01632            SET CFT9-PT-EXCLUDE (CFT9-IDX) TO TRUE                 ELTACL  
01633            SET CFT9-IDX UP BY 1                                   ELTACL  
01634         ELSE                                                      ELTACL  
01635            SET CFT9-IDX UP BY 1                                   ELTACL  
01636         END-IF                                                    ELTACL  
01637         END-PERFORM.                                              ELTACL  
01638                                                                   ELTACL  
01639 ******************************************************************ELTACL  
01640 *                                                                *ELTACL  
01641 *    CHECK CFT2 FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDED     *ELTACL  
01642 *                                                                *ELTACL  
01643 ******************************************************************ELTACL  
01644                                                                   ELTACL  
01645  0630-CHK-CFT2-NOT-EXCLD.                                         ELTACL  
01646      IF CFT2-PT-INCLUDE (CFT2-IDX)                                ELTACL  
01647      THEN                                                         ELTACL  
01648         EVALUATE TRUE                                             ELTACL  
01649            WHEN CFT2-PT-INST (CFT2-IDX)                           ELTACL  
01650               SET SW-INTRNL-INST-PROV-CL TO TRUE                  ELTACL  
01651               IF SRP-ACCUM-PROV-CLASS-INST                        ELTACL  
01652               THEN                                                ELTACL  
01653                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTACL  
01654               END-IF                                              ELTACL  
01655            WHEN CFT2-PT-PROF (CFT2-IDX)                           ELTACL  
01656               SET SW-INTRNL-PROF-PROV-CL TO TRUE                  ELTACL  
01657               IF SRP-ACCUM-PROV-CLASS-PROF                        ELTACL  
01658               THEN                                                ELTACL  
01659                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTACL  
01660               END-IF                                              ELTACL  
01661            END-EVALUATE                                           ELTACL  
01662      END-IF.                                                      ELTACL  
01663                                                                   ELTACL  
01664 ******************************************************************ELTACL  
01665 *                                                                *ELTACL  
01666 *    CHECK CFT9 FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED     * ELTACL  
01667 *                                                                *ELTACL  
01668 ******************************************************************ELTACL  
01669                                                                   ELTACL  
01670  0631-CHK-CFT9-NOT-EXCLD.                                         ELTACL  
01671      IF CFT9-PT-INCLUDE (CFT9-IDX)                                ELTACL  
01672        IF CFT9-PT-PROF (CFT9-IDX)                                 ELTACL  
01673          SET SW-INTRNL-PROF-PROV-CL TO TRUE                       ELTACL  
01674          IF SRP-ACCUM-PROV-SPEC-PROF                              ELTACL  
01675             SET SW-OCCRNC-APPLIES TO TRUE                         ELTACL  
01676          END-IF                                                   ELTACL  
01677      END-IF.                                                      ELTACL  
01678                                                                   ELTACL  
01679 /*****************************************************************ELTACL  
01680 *                                                                *ELTACL  
01681 *    CHECK IBGR INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELTACL  
01682 *                                                                *ELTACL  
01683 ******************************************************************ELTACL  
01684                                                                   ELTACL  
01685  0640-CHK-IBGR-PROV-CL.                                           ELTACL  
01686      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTACL  
01687          SW-INTRNL-NOT-PROF-PROV-CL TO TRUE.                      ELTACL  
01688      MOVE PC-IBGR TO KWA-PROVISION-ID.                            ELTACL  
01689      MOVE WS-IBGR-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTACL  
01690      PERFORM 0660-READ-INTRLN-TAB.                                ELTACL  
01691      SET ADDRESS OF GX1-RECORD-AREA TO IOP-REC-PTR.               ELTACL  
01692      SET IOP-REC-PTR                TO NULLS.                     ELTACL  
01693      SET GX1-INDEX                  TO GX1-ENTRY-COUNT.           ELTACL  
01694      SET WS-MAX-GX1-INDEX           TO GX1-INDEX.                 ELTACL  
01695                                                                   ELTACL  
01696      IF GX1-ID-ARGUMENT-EXCLUDED                                  ELTACL  
01697      THEN                                                         ELTACL  
01698 *    -- ASSUME THAT IBGR WOULD NOT EXCLUDE ALL OF ANY PROVIDER    ELTACL  
01699 *       CLASS (I.E., BOTH TYPES APPLY).                           ELTACL  
01700         SET SW-OCCRNC-APPLIES                                     ELTACL  
01701             SW-INTRNL-INST-PROV-CL                                ELTACL  
01702             SW-INTRNL-PROF-PROV-CL                                ELTACL  
01703          TO TRUE                                                  ELTACL  
01704      ELSE                                                         ELTACL  
01705         PERFORM 0650-CHK-INCLD-TYPE-IBGR                          ELTACL  
01706      END-IF.                                                      ELTACL  
01707                                                                   ELTACL  
01708 /*****************************************************************ELTACL  
01709 *                                                                *ELTACL  
01710 *    CHECK INCLUDE TYPE IBGR TO DETERMINE PROVIDER CLASS         *ELTACL  
01711 *                                                                *ELTACL  
01712 ******************************************************************ELTACL  
01713                                                                   ELTACL  
01714  0650-CHK-INCLD-TYPE-IBGR.                                        ELTACL  
01715      PERFORM WITH TEST BEFORE                                     ELTACL  
01716         VARYING GX1-INDEX FROM 1 BY 1                             ELTACL  
01717           UNTIL    GX1-INDEX = WS-MAX-GX1-INDEX                   ELTACL  
01718                 OR (    SW-INTRNL-INST-PROV-CL                    ELTACL  
01719                     AND SW-INTRNL-PROF-PROV-CL )                  ELTACL  
01720         MOVE GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)                ELTACL  
01721           TO WS-PROVISION-ARGUMENT                                ELTACL  
01722         EVALUATE TRUE                                             ELTACL  
01723            WHEN INST-CL                                           ELTACL  
01724               SET SW-INTRNL-INST-PROV-CL TO TRUE                  ELTACL  
01725               IF SRP-ACCUM-PROV-CLASS-INST                        ELTACL  
01726               THEN                                                ELTACL  
01727                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTACL  
01728               END-IF                                              ELTACL  
01729            WHEN PROF-CL                                           ELTACL  
01730               SET SW-INTRNL-PROF-PROV-CL TO TRUE                  ELTACL  
01731               IF SRP-ACCUM-PROV-CLASS-PROF                        ELTACL  
01732               THEN                                                ELTACL  
01733                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTACL  
01734               END-IF                                              ELTACL  
01735            END-EVALUATE                                           ELTACL  
01736         END-PERFORM.                                              ELTACL  
01737                                                                   ELTACL  
01738 /***********************************************************      ELTACL  
01739 *                                                          *      ELTACL  
01740 *    READ THE INTERNAL TABULAR RECORD                      *      ELTACL  
01741 *                                                          *      ELTACL  
01742 ************************************************************      ELTACL  
01743                                                                   ELTACL  
01744  0660-READ-INTRLN-TAB.                                            ELTACL  
01745      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELTACL  
01746      SET IOP-RD TO TRUE.                                          ELTACL  
01747      SET IOP-FCQ-NONE TO TRUE.                                    ELTACL  
01748      SET IOP-KVQ-EQ TO TRUE.                                      ELTACL  
01749      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELTACL  
01750      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTACL  
01751      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTACL  
01752      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTACL  
01753                                                                   ELTACL  
01754      EVALUATE TRUE                                                ELTACL  
01755         WHEN IOP-RC-OK                                            ELTACL  
01756            CONTINUE                                               ELTACL  
01757         WHEN IOP-RC-NOTFND                                        ELTACL  
01758            SET CIA-AB-NOTFND-GCTABULR TO TRUE                     ELTACL  
01759            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELTACL  
01760         WHEN OTHER                                                ELTACL  
01761             SET CIA-AB-CRITIO TO TRUE                             ELTACL  
01762             EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC           ELTACL  
01763         END-EVALUATE.                                             ELTACL  
01764                                                                   ELTACL  
01765 /***********************************************************      ELTACL  
01766 *                                                          *      ELTACL  
01767 *        ADD ACCUM OCCURRENCE TO FILE                      *      ELTACL  
01768 *                                                          *      ELTACL  
01769 ************************************************************      ELTACL  
01770                                                                   ELTACL  
01771  0710-WRITE-EXTRACT-RECORD.                                       ELTACL  
01772      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELTACL  
01773      SET  IOP-ADD TO TRUE.                                        ELTACL  
01774      SET  IOP-FCQ-NONE TO TRUE.                                   ELTACL  
01775      SET  IOP-KVQ-NONE TO TRUE.                                   ELTACL  
01776      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTACL  
01777                                                                   ELTACL  
01778 /***********************************************************      ELTACL  
01779 *                                                          *      ELTACL  
01780 *    ESTABLISH ADDRESSABILITY OF THE TABULAR FILE          *      ELTACL  
01781 *                                                          *      ELTACL  
01782 ************************************************************      ELTACL  
01783                                                                   ELTACL  
01784  9060-EST-ADR-TABULAR-FILE.                                       ELTACL  
01785      SET  CIA-GCTABULR-DDN TO TRUE.                               ELTACL  
01786      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACL  
01787         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELTACL  
01788         END-CALL.                                                 ELTACL  
01789      IF CIA-RC-PTR-NULL                                           ELTACL  
01790      THEN                                                         ELTACL  
01791         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTACL  
01792         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTACL  
01793      END-IF.                                                      ELTACL  
01794                                                                   ELTACL  
01795 /***********************************************************      ELTACL  
01796 *                                                          *      ELTACL  
01797 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELTACL  
01798 *                                                          *      ELTACL  
01799 ************************************************************      ELTACL  
01800                                                                   ELTACL  
01801  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELTACL  
01802      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELTACL  
01803      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTACL  
01804         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELTACL  
01805         END-CALL.                                                 ELTACL  
01806      IF CIA-RC-PTR-NULL                                           ELTACL  
01807         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTACL  
01808         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTACL  
01809      END-IF.                                                      ELTACL  
