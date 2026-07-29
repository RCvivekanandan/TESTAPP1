00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGOUTPX
00003  PROGRAM-ID.        ELGOUTPX.                                        LV002
00004                                                                   ELGOUTPX
00005  AUTHOR.            LUCY TORRES.                                  ELGOUTPX
00006                     RICHARD J. LUKETICH (RE-WRITE).               ELGOUTPX
00007                                                                   ELGOUTPX
00008  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELGOUTPX
00009                     A MUTUAL LEGAL RESERVE COMPANY                ELGOUTPX
00010                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELGOUTPX
00011                     233 N. MICHIGAN AVE                           ELGOUTPX
00012                     CHICAGO, ILLINOIS 60601                       ELGOUTPX
00013                                                                   ELGOUTPX
00014  DATE-WRITTEN.      03-JUN-1987.                                  ELGOUTPX
00015                     03-JAN-1992 (RE-WRITE).                       ELGOUTPX
00016                                                                   ELGOUTPX
00017  DATE-COMPILED.                                                   ELGOUTPX
00018                                                                   ELGOUTPX
00019  SECURITY.          COPYRIGHT 1986, 1992,                         ELGOUTPX
00020                     HEALTH CARE SERVICE CORPORATION               ELGOUTPX
00021                                                                   ELGOUTPX
00022  ENVIRONMENT DIVISION.                                            ELGOUTPX
00023                                                                   ELGOUTPX
00024  CONFIGURATION SECTION.                                           ELGOUTPX
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELGOUTPX
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELGOUTPX
00027                                                                   ELGOUTPX
00028 /*****************************************************************ELGOUTPX
00029 *                                                                *ELGOUTPX
00030 *  ELGOUTPX - ELS:  SELECTS #AOL (OUT OF POCKET) ACCUMULATORS AND*ELGOUTPX
00031 *                   SETUPS THE INFORMATION TO BE PROCESSED BY    *ELGOUTPX
00032 *                   THE OUT OF POCKET GENERATOR MODULE. THE ACCUMSELGOUTPX
00033 *                   ARE SELECTED FROM THE GROUP SPECIFIC AND     *ELGOUTPX
00034 *                   CONTRACT LEVEL PROCESSING.                   *ELGOUTPX
00035 *                                                                *ELGOUTPX
00036 ******************************************************************ELGOUTPX
00037 *                                                                *ELGOUTPX
00038 *                      MAINTENANCE HISTORY                       *ELGOUTPX
00039 *                                                                *ELGOUTPX
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELGOUTPX
00041 * ----- ----------- --- ----- ---------------------------------- *ELGOUTPX
00042 * 01.00 03-JUN-1987 LET       CREATED                            *ELGOUTPX
00043 * 01.01 25-SEP-1987 LET       ADDED DEFINITION DATA FIELD        *ELGOUTPX
00044 *                                                                *ELGOUTPX
00045 * 01.02 17-NOV-1987 REB       MADE CHANGES TO CORRESPOND TO NEW  *ELGOUTPX
00046 *                             VERSION OF COPYBOOK ELSACUMC.      *ELGOUTPX
00047 *                                                                *ELGOUTPX
00048 * 01.07    SEP-1991 RKH    1. ADDED LOGIC FOR:                   *ELGOUTPX
00049 *    ISSR #12010                A.  NEW PATIENT AGE FIELDS       *ELGOUTPX
00050 *                               B.  RELATIONSHIP IND VALUE       *ELGOUTPX
00051 *                          2. REVISE LOGIC TO LOAD INT ACCUMS    *ELGOUTPX
00052 *                             INTO VARIABLE LEVEL TABLE          *ELGOUTPX
00053 *                          3. ADDED COPYBOOKS :                  *ELGOUTPX
00054 *                               A. GCTIBGR   - IBGR TAB          *ELGOUTPX
00055 *                               B. GCTIPGT   - IPGT TAB          *ELGOUTPX
00056 *                               C. ELSCFTB2  - PROVIDER TYPE     *ELGOUTPX
00057 *                                         COMPARE TABLE          *ELGOUTPX
00058 *                          4. ADD LOGIC TO INSPECT #IPGT AND     *ELGOUTPX
00059 *                             #IBGR INT TABS TO DETERMINE IF     *ELGOUTPX
00060 *                             AN OCCURRANCE IS THE SELECTED      *ELGOUTPX
00061 *                             PROVIDER CLASS.                    *ELGOUTPX
00062 *                                                                *ELGOUTPX
00063 * 02.02 17-MAR-1992 JPB       CLONED FROM ELTAOL, MADE CHANGES   *ELGOUTPX
00064 *                             TO ELIMINATE MULTIPLE READS OF     *ELGOUTPX
00065 *                             THE GAD-RECORD AND ELIMINATED      *ELGOUTPX
00066 *                             CODE USED TO CHECK INTERNALS.      *ELGOUTPX
00067 *                                                                *ELGOUTPX
00068 * 02.03 02-DEC-1998 AKK      ADDED SUPPORT FOR ACP.              *ELGOUTPX
00069 *                                                                *ELGOUTPX
00070 * 02.04 25-AUG-2000 AKK      ADDED SUPPORT FOR #IPGS             *ELGOUTPX
00071 *                                                                *ELGOUTPX
00072 *            08/12/03  AKK  REGEN'D TO CHECK ORDER OF THE COMPILE*ELGOUTPX
00071 *                                                                *ELGOUTPX
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00056 *                                                                *ELGABMCC
00073 ******************************************************************ELGOUTPX
00074      TITLE  'ELGOUTPX          WORKING STORAGE'.                  ELGOUTPX
00075  DATA DIVISION.                                                   ELGOUTPX
00076                                                                   ELGOUTPX
00077  WORKING-STORAGE SECTION.                                         ELGOUTPX
00078                                                                   ELGOUTPX
00079  01  SWITCHES.                                                    ELGOUTPX
00080      02                                      PICTURE  X(01).      ELGOUTPX
00081         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELGOUTPX
00082         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELGOUTPX
00083                                                                   ELGOUTPX
00084      02 OCCURRENCE-APPLIES                   PICTURE  X(01).      ELGOUTPX
00085         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELGOUTPX
00086         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELGOUTPX
00087      02                                      PICTURE  X(01).      ELGOUTPX
00088         88 SW-HAS-IBGR                       VALUE 'Y'.           ELGOUTPX
00089         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELGOUTPX
00090      02                                      PICTURE  X(01).      ELGOUTPX
00091         88 SW-HAS-IDGD                       VALUE 'Y'.           ELGOUTPX
00092         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELGOUTPX
00093      02                                      PICTURE  X(01).      ELGOUTPX
00094         88 SW-HAS-IPGN                       VALUE 'Y'.           ELGOUTPX
00095         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELGOUTPX
00096      02                                      PICTURE  X(01).      ELGOUTPX
00097         88 SW-HAS-IPGP                       VALUE 'Y'.           ELGOUTPX
00098         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELGOUTPX
00099      02                                      PICTURE  X(01).      ELGOUTPX
00100         88 SW-HAS-IPGT                       VALUE 'Y'.           ELGOUTPX
00101         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELGOUTPX
00102      02                                      PICTURE  X(01).      ELGOUTPX
00103         88 SW-HAS-IPGS                       VALUE 'Y'.           ELGOUTPX
00104         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELGOUTPX
00105      02                                      PICTURE  X(01).      ELGOUTPX
00106         88 SW-INTRNL-INST-PROV-CL            VALUE 'Y'.           ELGOUTPX
00107         88 SW-INTRNL-NOT-INST-PROV-CL        VALUE 'N'.           ELGOUTPX
00108         88 SW-INTRNL-INST-PROV-CL-NOT-DET VALUE 'X'.              ELGOUTPX
00109      02                                      PICTURE  X(01).      ELGOUTPX
00110         88 SW-INTRNL-PROF-PROV-CL            VALUE 'Y'.           ELGOUTPX
00111         88 SW-INTRNL-NOT-PROF-PROV-CL        VALUE 'N'.           ELGOUTPX
00112         88 SW-INTRNL-PROF-PROV-CL-NOT-DET VALUE 'X'.              ELGOUTPX
00113      02                                      PICTURE  X(01).      ELGOUTPX
00114         88 SW-INTRNL-PROF-PROV-SP            VALUE 'Y'.           ELGOUTPX
00115         88 SW-INTRNL-NOT-PROF-PROV-SP        VALUE 'N'.           ELGOUTPX
00116         88 SW-INTRNL-PROF-PROV-SP-NOT-DET VALUE 'X'.              ELGOUTPX
00117      02                                      PICTURE  X(01).      ELGOUTPX
00118         88 SW-MATCHING-ENTRY-FOUND           VALUE 'Y'.           ELGOUTPX
00119         88 SW-MATCHING-ENTRY-NOT-FOUND       VALUE 'N'.           ELGOUTPX
00120      02                                      PICTURE  X(01).      ELGOUTPX
00121         88 SW-SORT-COMPLETED                 VALUE 'Y'.           ELGOUTPX
00122         88 SW-SORT-NOT-COMPLETED             VALUE 'N'.           ELGOUTPX
00123                                                                   ELGOUTPX
00124  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELGOUTPX
00125      88 WS-LOB-INST              VALUE '1'.                       ELGOUTPX
00126      88 WS-LOB-PROF              VALUE '2'.                       ELGOUTPX
00127      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELGOUTPX
00128      88 WS-LOB-BOTH              VALUE '3', '4', '5', '6', '7'.   ELGOUTPX
00129                                                                   ELGOUTPX
00130  01  PROGRAM-CONSTANTS.                                           ELGOUTPX
00131      02 PC-AOL                   PICTURE  X(06) VALUE '#AOL  '.   ELGOUTPX
00132      02 PC-GCT-MAX-SUB           PICTURE S9(04) COMP.             ELGOUTPX
00133      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELGOUTPX
00134      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELGOUTPX
00135      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELGOUTPX
00136      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELGOUTPX
00137      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELGOUTPX
00138      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGS '.   ELGOUTPX
00139      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELGOUTPX
00140                                                                   ELGOUTPX
00141  01  WS-WORK-FIELDS.                                              ELGOUTPX
00142      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELGOUTPX
00143      02 WS-OCCURRENCE-SUB        PICTURE S9(04) COMP.             ELGOUTPX
00144      02 WS-SAVE-SUB              PICTURE S9(04) COMP.             ELGOUTPX
00145      02 WS-SAVE-INDEX            USAGE IS INDEX.                  ELGOUTPX
00146      02 SORT-SUB                 PICTURE S9(04) COMP.             ELGOUTPX
00147      02 TEST-SUB                 PICTURE S9(04) COMP.             ELGOUTPX
00148                                                                   ELGOUTPX
00149  01  ACCUM-HOLD-TBL.                                              ELGOUTPX
00150      02  ACCUM-SLOT-NBR          PICTURE S9(07) COMP-3            ELGOUTPX
00151                                  OCCURS 5 TIMES.                  ELGOUTPX
00152                                                                   ELGOUTPX
00153  01  WS-OCCURRENCE-PROCESSED-TBL.                                 ELGOUTPX
00154      02                          PICTURE X                        ELGOUTPX
00155                                  OCCURS 44 TIMES                  ELGOUTPX
00156                                  INDEXED BY WS-OCCURRENCE-INDEX.  ELGOUTPX
00157          88  WS-OCCURRENCE-PROCESSED           VALUE 'P'.         ELGOUTPX
00158          88  WS-OCCURRENCE-NOT-PROCESSED       VALUE ' '.         ELGOUTPX
00159                                                                   ELGOUTPX
00160  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELGOUTPX
00161      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGOUTPX
00162      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGOUTPX
00163      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGOUTPX
00164      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGOUTPX
00165      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGOUTPX
00166      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGOUTPX
00167                                                                   ELGOUTPX
00168  01  WS-ASCEND-DESCEND-ENTRY-HOLD PICTURE X(28).                  ELGOUTPX
00169                                                                   ELGOUTPX
00170 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELGOUTPX
00171      COPY ELSCFTB2.                                               ELGOUTPX
00172                                                                   ELGOUTPX
00173 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELGOUTPX
00174      COPY ELSCFTB9.                                               ELGOUTPX
00175                                                                   ELGOUTPX
00176      TITLE  'ELGOUTPX          LINKAGE SECTION'                   ELGOUTPX
00177  LINKAGE SECTION.                                                 ELGOUTPX
00178  01  DFHCOMMAREA.                                                 ELGOUTPX
00179      COPY ELSCOMMC.                                               ELGOUTPX
00180 /                                                                 ELGOUTPX
00181      COPY ELSCIA2C.                                               ELGOUTPX
00182 /                                                                 ELGOUTPX
00183      COPY ELSIOPMC.                                               ELGOUTPX
00184 /                                                                 ELGOUTPX
00185      COPY ELSKEYSC.                                               ELGOUTPX
00186 /                                                                 ELGOUTPX
00187      COPY ELSSRTPC.                                               ELGOUTPX
00188 /                                                                 ELGOUTPX
00189      COPY ELSSSCBC.                                               ELGOUTPX
00190 /                                                                 ELGOUTPX
00191  01  GCG-RECORD-AREA.                                             ELGOUTPX
00192      COPY GCGROUPC.                                               ELGOUTPX
00193 /                                                                 ELGOUTPX
00194  01  GAD-RECORD-AREA.                                             ELGOUTPX
00195      COPY GCTAOLC.                                                ELGOUTPX
00196 /                                                                 ELGOUTPX
00197      COPY ELSACUMC.                                               ELGOUTPX
00198      TITLE  'ELGOUTPX          PROCEDURE DIVISION'.               ELGOUTPX
00199 ************************************************************      ELGOUTPX
00200 *                                                          *      ELGOUTPX
00201 *    ELGOUTPX MAINLINE                                     *      ELGOUTPX
00202 *                                                          *      ELGOUTPX
00203 ************************************************************      ELGOUTPX
00204                                                                   ELGOUTPX
00205  PROCEDURE DIVISION.                                              ELGOUTPX
00206                                                                   ELGOUTPX
00207      PERFORM 0010-INITIALIZATION.                                 ELGOUTPX
00208      PERFORM 0100-PROCESS.                                        ELGOUTPX
00209      GOBACK.                                                      ELGOUTPX
00210                                                                   ELGOUTPX
00211 ************************************************************      ELGOUTPX
00212 *                                                          *      ELGOUTPX
00213 *    INITIALIZATION                                        *      ELGOUTPX
00214 *                                                          *      ELGOUTPX
00215 ************************************************************      ELGOUTPX
00216                                                                   ELGOUTPX
00217  0010-INITIALIZATION.                                             ELGOUTPX
00218      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELGOUTPX
00219      PERFORM 0060-EST-ADR-KEY-WK-AREA.                            ELGOUTPX
00220      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELGOUTPX
00221      PERFORM 0120-EST-ADR-BEN-PRVN-ACCUM.                         ELGOUTPX
00222      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGOUTPX
00223                                                                   ELGOUTPX
00224 ************************************************************      ELGOUTPX
00225 *                                                          *      ELGOUTPX
00226 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGOUTPX
00227 *                                                          *      ELGOUTPX
00228 ************************************************************      ELGOUTPX
00229                                                                   ELGOUTPX
00230  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELGOUTPX
00231      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGOUTPX
00232      THEN                                                         ELGOUTPX
00233         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGOUTPX
00234      ELSE                                                         ELGOUTPX
00235         IF ECA-CIA-PTR = NULL                                     ELGOUTPX
00236         THEN                                                      ELGOUTPX
00237            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGOUTPX
00238         ELSE                                                      ELGOUTPX
00239            CALL 'ELUINISM' USING DFHCOMMAREA                      ELGOUTPX
00240               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELGOUTPX
00241               END-CALL                                            ELGOUTPX
00242            SET CIA-ELSSSCB-DDN TO TRUE                            ELGOUTPX
00243            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELGOUTPX
00244               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELGOUTPX
00245               END-CALL                                            ELGOUTPX
00246            IF CIA-RC-PTR-NULL                                     ELGOUTPX
00247            THEN                                                   ELGOUTPX
00248               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELGOUTPX
00249               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGOUTPX
00250            ELSE                                                   ELGOUTPX
00251               CONTINUE                                            ELGOUTPX
00252            END-IF                                                 ELGOUTPX
00253         END-IF                                                    ELGOUTPX
00254      END-IF.                                                      ELGOUTPX
00255                                                                   ELGOUTPX
00256 /***********************************************************      ELGOUTPX
00257 *                                                          *      ELGOUTPX
00258 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELGOUTPX
00259 *                                                          *      ELGOUTPX
00260 ************************************************************      ELGOUTPX
00261                                                                   ELGOUTPX
00262  0060-EST-ADR-KEY-WK-AREA.                                        ELGOUTPX
00263      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGOUTPX
00264      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGOUTPX
00265         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELGOUTPX
00266         END-CALL.                                                 ELGOUTPX
00267      IF CIA-RC-PTR-NULL                                           ELGOUTPX
00268         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGOUTPX
00269         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGOUTPX
00270      END-IF.                                                      ELGOUTPX
00271                                                                   ELGOUTPX
00272 ************************************************************      ELGOUTPX
00273 *                                                          *      ELGOUTPX
00274 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGOUTPX
00275 *                                                          *      ELGOUTPX
00276 ************************************************************      ELGOUTPX
00277                                                                   ELGOUTPX
00278  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGOUTPX
00279      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGOUTPX
00280      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGOUTPX
00281         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELGOUTPX
00282         END-CALL.                                                 ELGOUTPX
00283      IF CIA-RC-PTR-NULL                                           ELGOUTPX
00284         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGOUTPX
00285         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGOUTPX
00286      END-IF.                                                      ELGOUTPX
00287                                                                   ELGOUTPX
00288 ************************************************************      ELGOUTPX
00289 *                                                          *      ELGOUTPX
00290 *    ESTABLISH ADDRESSABILITY OF BENEFIT PROVSION LEVEL    *      ELGOUTPX
00291 *    ACCUMULATOR RECORD                                    *      ELGOUTPX
00292 *                                                          *      ELGOUTPX
00293 ************************************************************      ELGOUTPX
00294                                                                   ELGOUTPX
00295  0120-EST-ADR-BEN-PRVN-ACCUM.                                     ELGOUTPX
00296      SET CIA-GCTABULR-DDN TO TRUE.                                ELGOUTPX
00297      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGOUTPX
00298         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGOUTPX
00299         END-CALL.                                                 ELGOUTPX
00300      IF CIA-RC-PTR-NULL                                           ELGOUTPX
00301         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGOUTPX
00302         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGOUTPX
00303      ELSE                                                         ELGOUTPX
00304         IF IOP-REC-PTR = NULLS                                    ELGOUTPX
00305            SET CIA-AB-UNALLOC-AREA TO TRUE                        ELGOUTPX
00306            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELGOUTPX
00307         ELSE                                                      ELGOUTPX
00308            SET ADDRESS OF GAD-RECORD-AREA TO IOP-REC-PTR          ELGOUTPX
00309         END-IF                                                    ELGOUTPX
00310      END-IF.                                                      ELGOUTPX
00311                                                                   ELGOUTPX
00312 /***********************************************************      ELGOUTPX
00313 *                                                          *      ELGOUTPX
00314 *        PROCESS                                           *      ELGOUTPX
00315 *                                                          *      ELGOUTPX
00316 ************************************************************      ELGOUTPX
00317                                                                   ELGOUTPX
00318  0100-PROCESS.                                                    ELGOUTPX
00319      PERFORM 0210-SCAN-FOR-APPLIC-OCCRNCS.                        ELGOUTPX
00320                                                                   ELGOUTPX
00321      IF SW-NO-APPLIC-ACCUM-FOUND                                  ELGOUTPX
00322      THEN                                                         ELGOUTPX
00323         EVALUATE TRUE                                             ELGOUTPX
00324            WHEN SSB-PROV-CLASS-INST                               ELGOUTPX
00325               SET SRP-INST-NOT-APPLICABLE TO TRUE                 ELGOUTPX
00326            WHEN SSB-PROV-CLASS-PROF                               ELGOUTPX
00327               SET SRP-PROF-NOT-APPLICABLE TO TRUE                 ELGOUTPX
00328            WHEN SSB-PROV-CLASS-BOTH                               ELGOUTPX
00329               SET SRP-NO-ACCUMS-FOUND TO TRUE                     ELGOUTPX
00330            WHEN OTHER                                             ELGOUTPX
00331               SET CIA-AB-PGM-LOGIC TO TRUE                        ELGOUTPX
00332               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGOUTPX
00333            END-EVALUATE                                           ELGOUTPX
00334      END-IF.                                                      ELGOUTPX
00335                                                                   ELGOUTPX
00336      SET SRP-BEN-PROV-ACCUM TO TRUE.                              ELGOUTPX
00337                                                                   ELGOUTPX
00338 * -- LINK TO THE OUTPUT GENERATOR                                 ELGOUTPX
00339      EXEC CICS LINK PROGRAM ('ELGAOL') COMMAREA (DFHCOMMAREA)     ELGOUTPX
00340         END-EXEC.                                                 ELGOUTPX
00341                                                                   ELGOUTPX
00342 /***********************************************************      ELGOUTPX
00343 *                                                          *      ELGOUTPX
00344 *    SCAN AOL ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELGOUTPX
00345 *                                                          *      ELGOUTPX
00346 ************************************************************      ELGOUTPX
00347                                                                   ELGOUTPX
00348  0210-SCAN-FOR-APPLIC-OCCRNCS.                                    ELGOUTPX
00349      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGOUTPX
00350      PERFORM 0220-DELETE-AOL-SUMMARY-FILE.                        ELGOUTPX
00351      PERFORM 0230-ALLOC-WORKFILE-REC-AREA.                        ELGOUTPX
00352                                                                   ELGOUTPX
00353      MOVE SPACES TO WS-OCCURRENCE-PROCESSED-TBL.                  ELGOUTPX
00354 * -- SCAN ACCUMULATOR TABULAR                                     ELGOUTPX
00355      PERFORM WITH TEST BEFORE                                     ELGOUTPX
00356         VARYING WS-OCCURRENCE-SUB FROM 1 BY 1                     ELGOUTPX
00357         UNTIL WS-OCCURRENCE-SUB >= GAD-ENTRY-COUNT                ELGOUTPX
00358          IF WS-OCCURRENCE-NOT-PROCESSED (WS-OCCURRENCE-SUB)       ELGOUTPX
00359           THEN                                                    ELGOUTPX
00360           SET GAD-INDEX           TO WS-OCCURRENCE-SUB            ELGOUTPX
00361           SET WS-OCCURRENCE-INDEX TO WS-OCCURRENCE-SUB            ELGOUTPX
00362           PERFORM 0300-TEST-AOL-OCCURRENCE                        ELGOUTPX
00363          END-IF                                                   ELGOUTPX
00364      END-PERFORM.                                                 ELGOUTPX
00365                                                                   ELGOUTPX
00366                                                                   ELGOUTPX
00367 /***********************************************************      ELGOUTPX
00368 *                                                          *      ELGOUTPX
00369 *        DELETE AOL SUMMARY FILE                           *      ELGOUTPX
00370 *                                                          *      ELGOUTPX
00371 ************************************************************      ELGOUTPX
00372                                                                   ELGOUTPX
00373  0220-DELETE-AOL-SUMMARY-FILE.                                    ELGOUTPX
00374      SET IOP-DEL TO TRUE.                                         ELGOUTPX
00375      SET IOP-FCQ-NONE TO TRUE.                                    ELGOUTPX
00376      SET IOP-KVQ-NONE TO TRUE.                                    ELGOUTPX
00377      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGOUTPX
00378                                                                   ELGOUTPX
00379 /***********************************************************      ELGOUTPX
00380 *                                                          *      ELGOUTPX
00381 *    ALLOCATE WORKFILE RECORD AREA                         *      ELGOUTPX
00382 *                                                          *      ELGOUTPX
00383 ************************************************************      ELGOUTPX
00384                                                                   ELGOUTPX
00385  0230-ALLOC-WORKFILE-REC-AREA.                                    ELGOUTPX
00386      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGOUTPX
00387      SET CIA-STG-GETMAIN TO TRUE.                                 ELGOUTPX
00388      SET IOP-GETMAIN-REC TO TRUE.                                 ELGOUTPX
00389      COMPUTE IOP-MAX-REC-LEN =                                    ELGOUTPX
00390              LENGTH OF ACCUM-FIXED-AREA                           ELGOUTPX
00391 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELGOUTPX
00392            + LENGTH OF ACCUM-VARIABLE-AREA                        ELGOUTPX
00393            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELGOUTPX
00394 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELGOUTPX
00395 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELGOUTPX
00396                                                                   ELGOUTPX
00397      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGOUTPX
00398      IF IOP-REC-PTR = NULLS                                       ELGOUTPX
00399      THEN                                                         ELGOUTPX
00400         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGOUTPX
00401         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGOUTPX
00402      ELSE                                                         ELGOUTPX
00403         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELGOUTPX
00404      END-IF.                                                      ELGOUTPX
00405                                                                   ELGOUTPX
00406 /***********************************************************      ELGOUTPX
00407 *                                                          *      ELGOUTPX
00408 *        TEST AOL OCCURS                                   *      ELGOUTPX
00409 *                                                          *      ELGOUTPX
00410 ************************************************************      ELGOUTPX
00411                                                                   ELGOUTPX
00412  0300-TEST-AOL-OCCURRENCE.                                        ELGOUTPX
00413                                                                   ELGOUTPX
00414      MOVE GAD-O-P-X-L-O-B (GAD-INDEX) TO WS-LOB-ACCUM-OCCRNC.     ELGOUTPX
00415      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELGOUTPX
00416      PERFORM 0320-SCAN-FOR-INTERNALS.                             ELGOUTPX
00417      IF SW-OCCRNC-APPLIES                                         ELGOUTPX
00418      THEN                                                         ELGOUTPX
00419 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELGOUTPX
00420         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELGOUTPX
00421         PERFORM 0340-INIT-ACCUM-EXTRACT                           ELGOUTPX
00422         PERFORM 0350-EXTRACT-ACCUM                                ELGOUTPX
00423         PERFORM 0490-CHK-EXTRACT-DATA-INTGRTY                     ELGOUTPX
00424         PERFORM 0360-ACCUM-VBL-PORTION                            ELGOUTPX
00425         PERFORM 0710-WRITE-EXTRACT-RECORD                         ELGOUTPX
00426      END-IF.                                                      ELGOUTPX
00427      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-INDEX)            ELGOUTPX
00428          TO TRUE.                                                 ELGOUTPX
00429                                                                   ELGOUTPX
00430                                                                   ELGOUTPX
00431 ************************************************************      ELGOUTPX
00432 *                                                          *      ELGOUTPX
00433 *        INITIALIZE OCCURRENCE                             *      ELGOUTPX
00434 *                                                          *      ELGOUTPX
00435 ************************************************************      ELGOUTPX
00436                                                                   ELGOUTPX
00437  0310-INITIALIZE-OCCURRENCE.                                      ELGOUTPX
00438      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELGOUTPX
00439      SET SW-HAS-NO-IBGR                                           ELGOUTPX
00440          SW-HAS-NO-IDGD                                           ELGOUTPX
00441          SW-HAS-NO-IPGN                                           ELGOUTPX
00442          SW-HAS-NO-IPGP                                           ELGOUTPX
00443          SW-HAS-NO-IPGT                                           ELGOUTPX
00444          SW-HAS-NO-IPGS                                           ELGOUTPX
00445       TO TRUE.                                                    ELGOUTPX
00446      INITIALIZE WS-IBGR-SLOT-NBR                                  ELGOUTPX
00447                 WS-IDGD-SLOT-NBR                                  ELGOUTPX
00448                 WS-IPGN-SLOT-NBR                                  ELGOUTPX
00449                 WS-IPGP-SLOT-NBR                                  ELGOUTPX
00450                 WS-IPGS-SLOT-NBR                                  ELGOUTPX
00451                 WS-IPGT-SLOT-NBR.                                 ELGOUTPX
00452      SET SW-INTRNL-INST-PROV-CL-NOT-DET                           ELGOUTPX
00453          SW-INTRNL-PROF-PROV-CL-NOT-DET                           ELGOUTPX
00454          SW-INTRNL-PROF-PROV-SP-NOT-DET                           ELGOUTPX
00455       TO TRUE.                                                    ELGOUTPX
00456                                                                   ELGOUTPX
00457                                                                   ELGOUTPX
00458 ************************************************************      ELGOUTPX
00459 *                                                          *      ELGOUTPX
00460 *        SCAN FOR INTERNAL TABULARS                        *      ELGOUTPX
00461 *                                                          *      ELGOUTPX
00462 ************************************************************      ELGOUTPX
00463                                                                   ELGOUTPX
00464  0320-SCAN-FOR-INTERNALS.                                         ELGOUTPX
00465 *    (THIS IS DONE NOW IN CASE IPGT OR IBGR IS NEEDED TO DETERMINEELGOUTPX
00466 *     WHETHER OCCURRENCE IS INSTITUTIONAL OR PROFESSIONAL.)       ELGOUTPX
00467      PERFORM 0330-SCAN-THE-INTERNAL-TABULAR                       ELGOUTPX
00468         VARYING GAD-INT-INDEX FROM 1 BY 1                         ELGOUTPX
00469           UNTIL    GAD-INT-INDEX                                  ELGOUTPX
00470                 >= GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX).        ELGOUTPX
00471                                                                   ELGOUTPX
00472      EVALUATE TRUE ALSO TRUE                                      ELGOUTPX
00473         WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                        ELGOUTPX
00474            SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE                  ELGOUTPX
00475            SET SW-OCCRNC-APPLIES TO TRUE                          ELGOUTPX
00476         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH                 ELGOUTPX
00477            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGOUTPX
00478            SET SW-OCCRNC-APPLIES TO TRUE                          ELGOUTPX
00479         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST                 ELGOUTPX
00480            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGOUTPX
00481            SET SW-OCCRNC-APPLIES TO TRUE                          ELGOUTPX
00482         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH                 ELGOUTPX
00483            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGOUTPX
00484            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGOUTPX
00485            SET SW-OCCRNC-APPLIES TO TRUE                          ELGOUTPX
00486         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF                 ELGOUTPX
00487            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGOUTPX
00488            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGOUTPX
00489            SET SW-OCCRNC-APPLIES TO TRUE                          ELGOUTPX
00490         WHEN OTHER                                                ELGOUTPX
00491            CONTINUE                                               ELGOUTPX
00492      END-EVALUATE.                                                ELGOUTPX
00493                                                                   ELGOUTPX
00494 /***********************************************************      ELGOUTPX
00495 *                                                          *      ELGOUTPX
00496 *        SCAN THE INTERNAL TABULARS                        *      ELGOUTPX
00497 *                                                          *      ELGOUTPX
00498 ************************************************************      ELGOUTPX
00499                                                                   ELGOUTPX
00500  0330-SCAN-THE-INTERNAL-TABULAR.                                  ELGOUTPX
00501      IF GAD-INT-SLOT (GAD-INDEX, GAD-INT-INDEX) > 0               ELGOUTPX
00502      THEN                                                         ELGOUTPX
00503         MOVE GAD-INT-SLOT (GAD-INDEX, GAD-INT-INDEX)              ELGOUTPX
00504           TO WS-SLOT-NBR                                          ELGOUTPX
00505         EVALUATE GAD-INT-ID (GAD-INDEX, GAD-INT-INDEX)            ELGOUTPX
00506            WHEN PC-IBGR                                           ELGOUTPX
00507               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELGOUTPX
00508               SET SW-HAS-IBGR                                     ELGOUTPX
00509                TO TRUE                                            ELGOUTPX
00510            WHEN PC-IDGD                                           ELGOUTPX
00511               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELGOUTPX
00512               SET SW-HAS-IDGD                                     ELGOUTPX
00513                TO TRUE                                            ELGOUTPX
00514            WHEN PC-IPGP                                           ELGOUTPX
00515               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELGOUTPX
00516               SET SW-HAS-IPGP                                     ELGOUTPX
00517                TO TRUE                                            ELGOUTPX
00518            WHEN PC-IPGN                                           ELGOUTPX
00519               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELGOUTPX
00520               SET SW-HAS-IPGN                                     ELGOUTPX
00521                TO TRUE                                            ELGOUTPX
00522            WHEN PC-IPGT                                           ELGOUTPX
00523               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELGOUTPX
00524               SET SW-HAS-IPGT                                     ELGOUTPX
00525                TO TRUE                                            ELGOUTPX
00526            WHEN PC-IPGS                                           ELGOUTPX
00527               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELGOUTPX
00528               SET SW-HAS-IPGS                                     ELGOUTPX
00529                TO TRUE                                            ELGOUTPX
00530            WHEN OTHER                                             ELGOUTPX
00531               CONTINUE                                            ELGOUTPX
00532         END-EVALUATE                                              ELGOUTPX
00533      END-IF.                                                      ELGOUTPX
00534                                                                   ELGOUTPX
00535 /***********************************************************      ELGOUTPX
00536 *                                                          *      ELGOUTPX
00537 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELGOUTPX
00538 *                                                          *      ELGOUTPX
00539 ************************************************************      ELGOUTPX
00540                                                                   ELGOUTPX
00541  0340-INIT-ACCUM-EXTRACT.                                         ELGOUTPX
00542      INITIALIZE ACCUM-FIXED-AREA.                                 ELGOUTPX
00543      SET ACCUM-AOL TO TRUE.                                       ELGOUTPX
00544      MOVE +1 TO  ACCUM-ASCEND-DESCEND-COUNT.                      ELGOUTPX
00545      SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELGOUTPX
00546      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELGOUTPX
00547      INITIALIZE ACCUM-COPAY-ENTRY (1).                            ELGOUTPX
00548                                                                   ELGOUTPX
00549 /***********************************************************      ELGOUTPX
00550 *                                                          *      ELGOUTPX
00551 *          EXTRACT AOL TOPIC LEVEL DATA ELEMENTS           *      ELGOUTPX
00552 *                                                          *      ELGOUTPX
00553 ************************************************************      ELGOUTPX
00554                                                                   ELGOUTPX
00555  0350-EXTRACT-ACCUM.                                              ELGOUTPX
00556                                                                   ELGOUTPX
00557 * -- SET FIXED PORTION DATA ELEMENTS                              ELGOUTPX
00558      MOVE GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)                    ELGOUTPX
00559        TO     ACCUM-BENEFIT-PERIOD.                               ELGOUTPX
00560                                                                   ELGOUTPX
00561      MOVE GAD-O-P-X-FAM-OR-INDIV (GAD-INDEX)                      ELGOUTPX
00562        TO     ACCUM-FAM-OR-INDIV.                                 ELGOUTPX
00563                                                                   ELGOUTPX
00564      MOVE GAD-O-P-X-L-O-B (GAD-INDEX) TO ACCUM-L-O-B.             ELGOUTPX
00565                                                                   ELGOUTPX
00566      MOVE GAD-O-P-X-DEFINITION (GAD-INDEX) TO ACCUM-DEFINITION.   ELGOUTPX
00567                                                                   ELGOUTPX
00568      SET REINSTATEMENT-IND-NA TO TRUE.                            ELGOUTPX
00569                                                                   ELGOUTPX
00570      MOVE GAD-O-P-X-DAY-FACTOR-IND (GAD-INDEX)                    ELGOUTPX
00571        TO     ACCUM-DAY-FACTOR-IND.                               ELGOUTPX
00572                                                                   ELGOUTPX
00573      MOVE GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX)               ELGOUTPX
00574        TO     ACCUM-INTERNAL-DESCRIPTOR.                          ELGOUTPX
00575                                                                   ELGOUTPX
00576      MOVE GAD-O-P-X-SERVICE-GROUP (GAD-INDEX)                     ELGOUTPX
00577        TO     ACCUM-SERVICE-GROUP.                                ELGOUTPX
00578                                                                   ELGOUTPX
00579      MOVE GAD-O-P-X-PLACE-OF-TREATMENT (GAD-INDEX)                ELGOUTPX
00580        TO     ACCUM-PLACE-OF-TREATMENT.                           ELGOUTPX
00581                                                                   ELGOUTPX
00582      MOVE GAD-O-P-X-CONDITION (GAD-INDEX) TO ACCUM-CONDITION.     ELGOUTPX
00583                                                                   ELGOUTPX
00584      MOVE GAD-O-P-X-CLAIM-LVL-ACCUM-IND (GAD-INDEX)               ELGOUTPX
00585        TO     ACCUM-CLAIM-LVL-ACCUM-IND.                          ELGOUTPX
00586                                                                   ELGOUTPX
00587      MOVE GAD-O-P-X-CO-PAY-IND (GAD-INDEX)                        ELGOUTPX
00588        TO     ACCUM-CO-PAY-IND (1).                               ELGOUTPX
00589                                                                   ELGOUTPX
00590      MOVE GAD-O-P-X-COST-CONTAIN-IND (GAD-INDEX)                  ELGOUTPX
00591        TO     ACCUM-COST-CONTAIN-IND.                             ELGOUTPX
00592                                                                   ELGOUTPX
00593      MOVE GAD-O-P-X-AGE-LIMIT-FROM (GAD-INDEX)                    ELGOUTPX
00594        TO     ACCUM-AGE-LIMIT-FROM-VAL.                           ELGOUTPX
00595                                                                   ELGOUTPX
00596      MOVE GAD-O-P-X-AGE-LIMIT-TO (GAD-INDEX)                      ELGOUTPX
00597        TO     ACCUM-AGE-LIMIT-TO-VAL.                             ELGOUTPX
00598                                                                   ELGOUTPX
00599      MOVE GAD-O-P-X-AGE-QUAL-IND-FROM (GAD-INDEX)                 ELGOUTPX
00600        TO     ACCUM-AGE-LIMIT-FROM-IND.                           ELGOUTPX
00601                                                                   ELGOUTPX
00602      MOVE GAD-O-P-X-AGE-QUAL-IND-TO (GAD-INDEX)                   ELGOUTPX
00603        TO     ACCUM-AGE-LIMIT-TO-IND.                             ELGOUTPX
00604                                                                   ELGOUTPX
00605      MOVE GAD-O-P-X-RELATIONSHIP-IND (GAD-INDEX)                  ELGOUTPX
00606        TO     ACCUM-RELATIONSHIP-IND.                             ELGOUTPX
00607                                                                   ELGOUTPX
00608      MOVE  GAD-CARRY-OVER-CREDIT-IND (GAD-INDEX)                  ELGOUTPX
00609       TO ACCUM-CARRY-OVER-CREDIT-IND.                             ELGOUTPX
00610                                                                   ELGOUTPX
00611      MOVE GAD-O-P-X-ASCEND-DESCEND-IND (GAD-INDEX)                ELGOUTPX
00612        TO     ACCUM-ASCEND-DESCEND-IND.                           ELGOUTPX
00613                                                                   ELGOUTPX
00614      MOVE GAD-O-P-X-BEN-PER-TIME-QUAL (GAD-INDEX)                 ELGOUTPX
00615        TO     ACCUM-BEN-PER-TIME-QUAL.                            ELGOUTPX
00616                                                                   ELGOUTPX
00617      MOVE GAD-O-P-X-BEN-PER-TIME-FCTR (GAD-INDEX)                 ELGOUTPX
00618        TO     ACCUM-BEN-PER-TIME-FCTR.                            ELGOUTPX
00619                                                                   ELGOUTPX
00620      MOVE GAD-O-P-X-INTERVAL-TIME-FCTR (GAD-INDEX)                ELGOUTPX
00621        TO     ACCUM-INTERVAL-TIME-FCTR.                           ELGOUTPX
00622                                                                   ELGOUTPX
00623      MOVE GAD-O-P-X-INTERVAL-TYPE (GAD-INDEX)                     ELGOUTPX
00624        TO     ACCUM-INTERVAL-TYPE.                                ELGOUTPX
00625                                                                   ELGOUTPX
00626      MOVE GAD-O-P-X-INTERVAL-OVRD-IND (GAD-INDEX)                 ELGOUTPX
00627        TO     ACCUM-INTERVAL-OVRD-IND.                            ELGOUTPX
00628                                                                   ELGOUTPX
00629      MOVE GAD-O-P-X-INTERVAL-OVRD-VALUE (GAD-INDEX)               ELGOUTPX
00630        TO     ACCUM-INTERVAL-OVRD-VALUE.                          ELGOUTPX
00631                                                                   ELGOUTPX
00632      MOVE GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)                   ELGOUTPX
00633        TO     ACCUM-VALUE-QUALIFIER.                              ELGOUTPX
00634                                                                   ELGOUTPX
00635      MOVE GAD-O-P-X-FYI-VALUE (GAD-INDEX) TO ACCUM-FYI-VALUE.     ELGOUTPX
00636                                                                   ELGOUTPX
00637      SET DED-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELGOUTPX
00638      SET MAX-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELGOUTPX
00639                                                                   ELGOUTPX
00640      MOVE GCG-OUTPKT-BASE-AMT-SOURCE-IN TO                        ELGOUTPX
00641           ACCUM-OPX-BASE-AMT-SOURCE-IND.                          ELGOUTPX
00642                                                                   ELGOUTPX
00643 * -- SET OCCURRENCE PROVIDER CLASS INFORMATION                    ELGOUTPX
00644      EVALUATE TRUE ALSO TRUE                                      ELGOUTPX
00645         WHEN      SW-INTRNL-INST-PROV-CL                          ELGOUTPX
00646              ALSO SW-INTRNL-NOT-PROF-PROV-CL                      ELGOUTPX
00647            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELGOUTPX
00648         WHEN      SW-INTRNL-NOT-INST-PROV-CL                      ELGOUTPX
00649              ALSO SW-INTRNL-PROF-PROV-CL                          ELGOUTPX
00650            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELGOUTPX
00651         WHEN OTHER                                                ELGOUTPX
00652            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELGOUTPX
00653      END-EVALUATE.                                                ELGOUTPX
00654                                                                   ELGOUTPX
00655 * -- SET OCCURRENCE PROVIDER SPEC INFORMATION                     ELGOUTPX
00656         IF SW-INTRNL-PROF-PROV-CL                                 ELGOUTPX
00657            SET ACCUM-PRVDR-SPC-PROF TO TRUE                       ELGOUTPX
00658         ELSE                                                      ELGOUTPX
00659            SET ACCUM-PRVDR-SPC-ALL TO TRUE                        ELGOUTPX
00660         END-IF.                                                   ELGOUTPX
00661                                                                   ELGOUTPX
00662 /***********************************************************      ELGOUTPX
00663 *                                                          *      ELGOUTPX
00664 *        ACCUM VBL PORTION                                 *      ELGOUTPX
00665 *                                                          *      ELGOUTPX
00666 ************************************************************      ELGOUTPX
00667                                                                   ELGOUTPX
00668  0360-ACCUM-VBL-PORTION.                                          ELGOUTPX
00669      SET ASC-DES-INDEX TO 1.                                      ELGOUTPX
00670      PERFORM 0370-EXTRACT-VARIABLE-PORTION.                       ELGOUTPX
00671      IF ACCUM-VARIABLE-TYPE                                       ELGOUTPX
00672         THEN                                                      ELGOUTPX
00673             PERFORM 0380-EXTRACT-ADDL-OCCURNCS                    ELGOUTPX
00674      END-IF.                                                      ELGOUTPX
00675                                                                   ELGOUTPX
00676 /***********************************************************      ELGOUTPX
00677 *                                                          *      ELGOUTPX
00678 *        EXTRACT VARIABLE PORTION                          *      ELGOUTPX
00679 *                                                          *      ELGOUTPX
00680 ************************************************************      ELGOUTPX
00681                                                                   ELGOUTPX
00682  0370-EXTRACT-VARIABLE-PORTION.                                   ELGOUTPX
00683      MOVE GAD-O-P-X-BISCENDING-IND (GAD-INDEX)                    ELGOUTPX
00684        TO     ACCUM-BISCEND-IND (ASC-DES-INDEX).                  ELGOUTPX
00685      MOVE GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX)                     ELGOUTPX
00686        TO     ACCUM-PERCENT-LEVEL (ASC-DES-INDEX).                ELGOUTPX
00687      MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)                       ELGOUTPX
00688        TO     ACCUM-VALUE-LIMIT (ASC-DES-INDEX).                  ELGOUTPX
00689      MOVE    WS-IBGR-SLOT-NBR                                     ELGOUTPX
00690        TO ACCUM-IBGR-SLOT-NBR (ASC-DES-INDEX).                    ELGOUTPX
00691      MOVE    WS-IDGD-SLOT-NBR                                     ELGOUTPX
00692        TO ACCUM-IDGD-SLOT-NBR (ASC-DES-INDEX).                    ELGOUTPX
00693      MOVE    WS-IPGN-SLOT-NBR                                     ELGOUTPX
00694        TO ACCUM-IPGN-SLOT-NBR (ASC-DES-INDEX).                    ELGOUTPX
00695      MOVE    WS-IPGP-SLOT-NBR                                     ELGOUTPX
00696        TO ACCUM-IPGP-SLOT-NBR (ASC-DES-INDEX).                    ELGOUTPX
00697      MOVE    WS-IPGT-SLOT-NBR                                     ELGOUTPX
00698        TO ACCUM-IPGT-SLOT-NBR (ASC-DES-INDEX).                    ELGOUTPX
00699      MOVE    WS-IPGS-SLOT-NBR                                     ELGOUTPX
00700        TO ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX).                    ELGOUTPX
00701      IF ACCUM-BISCEND-IND (ASC-DES-INDEX)                         ELGOUTPX
00702         = ZERO OR SPACES OR LOW-VALUES                            ELGOUTPX
00703         SET   BISCEND-IND-NA (ASC-DES-INDEX) TO TRUE.             ELGOUTPX
00704                                                                   ELGOUTPX
00705      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-INDEX) TO TRUE.   ELGOUTPX
00706                                                                   ELGOUTPX
00707 /***********************************************************      ELGOUTPX
00708 *                                                          *      ELGOUTPX
00709 *        EXTRACT ADDITIONAL OCCURRENCES                    *      ELGOUTPX
00710 *                                                          *      ELGOUTPX
00711 ************************************************************      ELGOUTPX
00712                                                                   ELGOUTPX
00713  0380-EXTRACT-ADDL-OCCURNCS.                                      ELGOUTPX
00714      MOVE WS-OCCURRENCE-SUB TO WS-SAVE-SUB                        ELGOUTPX
00715      SET  WS-SAVE-INDEX    TO GAD-INDEX.                          ELGOUTPX
00716      ADD 1 TO WS-OCCURRENCE-SUB.                                  ELGOUTPX
00717      PERFORM 0390-TEST-SUBSEQ-OCCRNCES                            ELGOUTPX
00718          VARYING WS-OCCURRENCE-SUB                                ELGOUTPX
00719             FROM WS-OCCURRENCE-SUB BY 1                           ELGOUTPX
00720          UNTIL WS-OCCURRENCE-INDEX >= GAD-ENTRY-COUNT.            ELGOUTPX
00721      MOVE WS-SAVE-SUB TO WS-OCCURRENCE-SUB.                       ELGOUTPX
00722      SET  GAD-INDEX   TO WS-SAVE-INDEX.                           ELGOUTPX
00723                                                                   ELGOUTPX
00724 /***********************************************************      ELGOUTPX
00725 *                                                          *      ELGOUTPX
00726 *        TEST SUBSEQUENT OCCURRENCES                       *      ELGOUTPX
00727 *                                                          *      ELGOUTPX
00728 ************************************************************      ELGOUTPX
00729                                                                   ELGOUTPX
00730  0390-TEST-SUBSEQ-OCCRNCES.                                       ELGOUTPX
00731      SET GAD-INDEX TO WS-OCCURRENCE-SUB.                          ELGOUTPX
00732      SET WS-OCCURRENCE-INDEX TO WS-OCCURRENCE-SUB.                ELGOUTPX
00733      IF WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-INDEX)             ELGOUTPX
00734          CONTINUE                                                 ELGOUTPX
00735      ELSE PERFORM 0400-TEST-OCCURRENCE.                           ELGOUTPX
00736                                                                   ELGOUTPX
00737 /***********************************************************      ELGOUTPX
00738 *                                                          *      ELGOUTPX
00739 *        TEST OCCURRENCE                                   *      ELGOUTPX
00740 *                                                          *      ELGOUTPX
00741 ************************************************************      ELGOUTPX
00742                                                                   ELGOUTPX
00743  0400-TEST-OCCURRENCE.                                            ELGOUTPX
00744      SET SW-MATCHING-ENTRY-NOT-FOUND TO TRUE.                     ELGOUTPX
00745      PERFORM 0410-TEST-KEYS-FOR-MATCH.                            ELGOUTPX
00746      IF SW-MATCHING-ENTRY-FOUND                                   ELGOUTPX
00747         PERFORM 0420-COMPLETE-TEST-OF-OCCURNCE.                   ELGOUTPX
00748                                                                   ELGOUTPX
00749 /***********************************************************      ELGOUTPX
00750 *                                                          *      ELGOUTPX
00751 *        TEST KEYS FOR MATCH                               *      ELGOUTPX
00752 *                                                          *      ELGOUTPX
00753 ************************************************************      ELGOUTPX
00754                                                                   ELGOUTPX
00755  0410-TEST-KEYS-FOR-MATCH.                                        ELGOUTPX
00756      IF      GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX) =               ELGOUTPX
00757                  ACCUM-BENEFIT-PERIOD                             ELGOUTPX
00758                        AND                                        ELGOUTPX
00759              GAD-O-P-X-FAM-OR-INDIV (GAD-INDEX) =                 ELGOUTPX
00760                  ACCUM-FAM-OR-INDIV                               ELGOUTPX
00761                        AND                                        ELGOUTPX
00762              GAD-O-P-X-L-O-B (GAD-INDEX) =                        ELGOUTPX
00763                  ACCUM-L-O-B                                      ELGOUTPX
00764                        AND                                        ELGOUTPX
00765              GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX) =          ELGOUTPX
00766                  ACCUM-INTERNAL-DESCRIPTOR                        ELGOUTPX
00767                        AND                                        ELGOUTPX
00768              GAD-O-P-X-SERVICE-GROUP (GAD-INDEX) =                ELGOUTPX
00769                  ACCUM-SERVICE-GROUP                              ELGOUTPX
00770                        AND                                        ELGOUTPX
00771              GAD-O-P-X-PLACE-OF-TREATMENT (GAD-INDEX) =           ELGOUTPX
00772                  ACCUM-PLACE-OF-TREATMENT                         ELGOUTPX
00773                        AND                                        ELGOUTPX
00774                    GAD-COND-ALL-BIT (GAD-INDEX) =                 ELGOUTPX
00775                  ACCUM-COND-ALL-BIT                               ELGOUTPX
00776                        AND                                        ELGOUTPX
00777                    GAD-COND-EXCLUSION-BIT (GAD-INDEX) =           ELGOUTPX
00778                  ACCUM-COND-EXCLUSION-BIT                         ELGOUTPX
00779                        AND                                        ELGOUTPX
00780                    GAD-COND-ICD-BIT (GAD-INDEX) =                 ELGOUTPX
00781                  ACCUM-COND-ICD-BIT                               ELGOUTPX
00782                        AND                                        ELGOUTPX
00783                    GAD-COND-TB-BIT (GAD-INDEX) =                  ELGOUTPX
00784                  ACCUM-COND-TB-BIT                                ELGOUTPX
00785                        AND                                        ELGOUTPX
00786                    GAD-COND-MENTAL-BIT (GAD-INDEX) =              ELGOUTPX
00787                  ACCUM-COND-MENTAL-BIT                            ELGOUTPX
00788                        AND                                        ELGOUTPX
00789                    GAD-COND-DRUG-BIT (GAD-INDEX) =                ELGOUTPX
00790                  ACCUM-COND-DRUG-BIT                              ELGOUTPX
00791                        AND                                        ELGOUTPX
00792                    GAD-COND-ALCOHOL-BIT (GAD-INDEX) =             ELGOUTPX
00793                  ACCUM-COND-ALCOHOL-BIT                           ELGOUTPX
00794                        AND                                        ELGOUTPX
00795                    GAD-COND-OB-COMP-BIT (GAD-INDEX) =             ELGOUTPX
00796                  ACCUM-COND-OB-COMP-BIT                           ELGOUTPX
00797                        AND                                        ELGOUTPX
00798                    GAD-COND-OB-NORM-BIT (GAD-INDEX) =             ELGOUTPX
00799                  ACCUM-COND-OB-NORM-BIT                           ELGOUTPX
00800                        AND                                        ELGOUTPX
00801                    GAD-COND-MALIGNANCY-BIT (GAD-INDEX) =          ELGOUTPX
00802                  ACCUM-COND-MALIGNANCY-BIT                        ELGOUTPX
00803                        AND                                        ELGOUTPX
00804                    GAD-COND-CARDIAC-DISEASE-BIT (GAD-INDEX) =     ELGOUTPX
00805                  ACCUM-COND-CARDIAC-DISEASE-BIT                   ELGOUTPX
00806                        AND                                        ELGOUTPX
00807                    GAD-COND-LIFE-THREAT-BIT (GAD-INDEX) =         ELGOUTPX
00808                  ACCUM-COND-LIFE-THREAT-BIT                       ELGOUTPX
00809                        AND                                        ELGOUTPX
00810                    GAD-COND-TMJ-BIT (GAD-INDEX) =                 ELGOUTPX
00811                  ACCUM-COND-TMJ-BIT                               ELGOUTPX
00812                        AND                                        ELGOUTPX
00813                    GAD-COND-INF-BIT (GAD-INDEX) =                 ELGOUTPX
00814                  ACCUM-COND-INF-BIT                               ELGOUTPX
00815                        AND                                        ELGOUTPX
00816                    GAD-COND-OBESITY-BIT (GAD-INDEX) =             ELGOUTPX
00817                  ACCUM-COND-OBESITY-BIT                           ELGOUTPX
00818                        AND                                        ELGOUTPX
00819                    GAD-COND-KIDNEY-DISEASE-BIT (GAD-INDEX) =      ELGOUTPX
00820                  ACCUM-COND-KIDNEY-DISEASE-BIT                    ELGOUTPX
00821                        AND                                        ELGOUTPX
00822                    GAD-COND-ACCIDENT-BIT (GAD-INDEX) =            ELGOUTPX
00823                  ACCUM-COND-ACCIDENT-BIT                          ELGOUTPX
00824                        AND                                        ELGOUTPX
00825                    GAD-COND-PRE-EXIST-BIT (GAD-INDEX) =           ELGOUTPX
00826                  ACCUM-COND-PRE-EXIST-BIT                         ELGOUTPX
00827                        AND                                        ELGOUTPX
00828                    GAD-COND-NON-EMER-BIT (GAD-INDEX) =            ELGOUTPX
00829                  ACCUM-COND-NON-EMER-BIT                          ELGOUTPX
00830                        AND                                        ELGOUTPX
00831                    GAD-COND-SUICIDE-BIT (GAD-INDEX) =             ELGOUTPX
00832                  ACCUM-COND-SUICIDE-BIT                           ELGOUTPX
00833                        AND                                        ELGOUTPX
00834              GAD-O-P-X-CO-PAY-IND (GAD-INDEX) =                   ELGOUTPX
00835                  ACCUM-CO-PAY-IND (1)                             ELGOUTPX
00836                        AND                                        ELGOUTPX
00837              GAD-O-P-X-COST-CONTAIN-IND (GAD-INDEX) =             ELGOUTPX
00838                  ACCUM-COST-CONTAIN-IND                           ELGOUTPX
00839                        AND                                        ELGOUTPX
00840              GAD-O-P-X-ASCEND-DESCEND-IND (GAD-INDEX) =           ELGOUTPX
00841                  ACCUM-ASCEND-DESCEND-IND                         ELGOUTPX
00842                        AND                                        ELGOUTPX
00843              GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) =              ELGOUTPX
00844                  ACCUM-VALUE-QUALIFIER                            ELGOUTPX
00845                        AND                                        ELGOUTPX
00846              GAD-O-P-X-AGE-LIMIT-FROM (GAD-INDEX) =               ELGOUTPX
00847                   ACCUM-AGE-LIMIT-FROM-VAL                        ELGOUTPX
00848                        AND                                        ELGOUTPX
00849              GAD-O-P-X-AGE-LIMIT-TO (GAD-INDEX) =                 ELGOUTPX
00850                   ACCUM-AGE-LIMIT-TO-VAL                          ELGOUTPX
00851                        AND                                        ELGOUTPX
00852              GAD-O-P-X-AGE-QUAL-IND-FROM (GAD-INDEX) =            ELGOUTPX
00853                   ACCUM-AGE-LIMIT-FROM-IND                        ELGOUTPX
00854                        AND                                        ELGOUTPX
00855              GAD-O-P-X-AGE-QUAL-IND-TO (GAD-INDEX) =              ELGOUTPX
00856                   ACCUM-AGE-LIMIT-TO-IND                          ELGOUTPX
00857                        AND                                        ELGOUTPX
00858              GAD-O-P-X-RELATIONSHIP-IND (GAD-INDEX) =             ELGOUTPX
00859                   ACCUM-RELATIONSHIP-IND                          ELGOUTPX
00860                        AND                                        ELGOUTPX
00861              GAD-O-P-X-FYI-VALUE (GAD-INDEX) =                    ELGOUTPX
00862                  ACCUM-FYI-VALUE                                  ELGOUTPX
00863      THEN                                                         ELGOUTPX
00864          SET SW-MATCHING-ENTRY-FOUND TO TRUE.                     ELGOUTPX
00865                                                                   ELGOUTPX
00866                                                                   ELGOUTPX
00867 /***********************************************************      ELGOUTPX
00868 *                                                          *      ELGOUTPX
00869 *        COMPLETE TEST OF OCCURRENCE                       *      ELGOUTPX
00870 *                                                          *      ELGOUTPX
00871 ************************************************************      ELGOUTPX
00872                                                                   ELGOUTPX
00873  0420-COMPLETE-TEST-OF-OCCURNCE.                                  ELGOUTPX
00874      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELGOUTPX
00875      PERFORM 0320-SCAN-FOR-INTERNALS.                             ELGOUTPX
00876      IF SW-OCCRNC-APPLIES                                         ELGOUTPX
00877         PERFORM 0430-EXTRACT-NEXT-OCCURRENCE                      ELGOUTPX
00878      ELSE                                                         ELGOUTPX
00879         SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB) TO TRUE.  ELGOUTPX
00880                                                                   ELGOUTPX
00881 /***********************************************************      ELGOUTPX
00882 *                                                          *      ELGOUTPX
00883 *        EXTRACT NEXT OCCURRENCE                           *      ELGOUTPX
00884 *                                                          *      ELGOUTPX
00885 ************************************************************      ELGOUTPX
00886                                                                   ELGOUTPX
00887  0430-EXTRACT-NEXT-OCCURRENCE.                                    ELGOUTPX
00888      ADD +1 TO ACCUM-ASCEND-DESCEND-COUNT.                        ELGOUTPX
00889      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELGOUTPX
00890      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (ASC-DES-INDEX).       ELGOUTPX
00891      PERFORM 0370-EXTRACT-VARIABLE-PORTION.                       ELGOUTPX
00892      PERFORM 0440-INSERT-NEW-ENTRY.                               ELGOUTPX
00893                                                                   ELGOUTPX
00894 /***********************************************************      ELGOUTPX
00895 *                                                          *      ELGOUTPX
00896 *        INSERT NEW ENTRY                                  *      ELGOUTPX
00897 *                                                          *      ELGOUTPX
00898 ************************************************************      ELGOUTPX
00899                                                                   ELGOUTPX
00900  0440-INSERT-NEW-ENTRY.                                           ELGOUTPX
00901      MOVE ACCUM-ASCEND-DESCEND-COUNT TO SORT-SUB.                 ELGOUTPX
00902      SET SW-SORT-NOT-COMPLETED TO TRUE.                           ELGOUTPX
00903      IF SORT-SUB = 1                                              ELGOUTPX
00904         CONTINUE                                                  ELGOUTPX
00905      ELSE IF ACCUM-ASCEND-ORDER                                   ELGOUTPX
00906              PERFORM 0450-ASCEND-INSERT                           ELGOUTPX
00907                UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1            ELGOUTPX
00908           ELSE IF ACCUM-DESCEND-ORDER                             ELGOUTPX
00909                   PERFORM 0460-DESCEND-INSERT                     ELGOUTPX
00910                     UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1       ELGOUTPX
00911                ELSE IF ACCUM-BISCEND-ORDER                        ELGOUTPX
00912                        PERFORM 0470-BISCEND-INSERT                ELGOUTPX
00913                          UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1. ELGOUTPX
00914                                                                   ELGOUTPX
00915                                                                   ELGOUTPX
00916 /***********************************************************      ELGOUTPX
00917 *                                                          *      ELGOUTPX
00918 *        ASCEND INSERT                                     *      ELGOUTPX
00919 *                                                          *      ELGOUTPX
00920 ************************************************************      ELGOUTPX
00921                                                                   ELGOUTPX
00922  0450-ASCEND-INSERT.                                              ELGOUTPX
00923      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELGOUTPX
00924      IF ACCUM-PERCENT-LEVEL (SORT-SUB) <                          ELGOUTPX
00925         ACCUM-PERCENT-LEVEL (TEST-SUB)                            ELGOUTPX
00926         PERFORM 0480-SWAP-ENTRIES                                 ELGOUTPX
00927      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELGOUTPX
00928                                                                   ELGOUTPX
00929 /***********************************************************      ELGOUTPX
00930 *                                                          *      ELGOUTPX
00931 *        DESCEND INSERT                                    *      ELGOUTPX
00932 *                                                          *      ELGOUTPX
00933 ************************************************************      ELGOUTPX
00934                                                                   ELGOUTPX
00935  0460-DESCEND-INSERT.                                             ELGOUTPX
00936      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELGOUTPX
00937      IF ACCUM-PERCENT-LEVEL (SORT-SUB) >                          ELGOUTPX
00938         ACCUM-PERCENT-LEVEL (TEST-SUB)                            ELGOUTPX
00939         PERFORM 0480-SWAP-ENTRIES                                 ELGOUTPX
00940      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELGOUTPX
00941                                                                   ELGOUTPX
00942 /***********************************************************      ELGOUTPX
00943 *                                                          *      ELGOUTPX
00944 *        BISCEND INSERT                                    *      ELGOUTPX
00945 *                                                          *      ELGOUTPX
00946 ************************************************************      ELGOUTPX
00947                                                                   ELGOUTPX
00948  0470-BISCEND-INSERT.                                             ELGOUTPX
00949      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELGOUTPX
00950      IF ACCUM-BISCEND-IND (SORT-SUB) <                            ELGOUTPX
00951         ACCUM-BISCEND-IND (TEST-SUB)                              ELGOUTPX
00952         PERFORM 0480-SWAP-ENTRIES                                 ELGOUTPX
00953      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELGOUTPX
00954                                                                   ELGOUTPX
00955 /***********************************************************      ELGOUTPX
00956 *                                                          *      ELGOUTPX
00957 *        SWAP ENTRIES                                      *      ELGOUTPX
00958 *                                                          *      ELGOUTPX
00959 ************************************************************      ELGOUTPX
00960                                                                   ELGOUTPX
00961  0480-SWAP-ENTRIES.                                               ELGOUTPX
00962      MOVE ACCUM-ASCEND-DESCEND-ENTRY (SORT-SUB)                   ELGOUTPX
00963        TO    WS-ASCEND-DESCEND-ENTRY-HOLD.                        ELGOUTPX
00964      MOVE ACCUM-ASCEND-DESCEND-ENTRY (TEST-SUB)                   ELGOUTPX
00965        TO ACCUM-ASCEND-DESCEND-ENTRY (SORT-SUB).                  ELGOUTPX
00966      MOVE    WS-ASCEND-DESCEND-ENTRY-HOLD                         ELGOUTPX
00967        TO ACCUM-ASCEND-DESCEND-ENTRY (TEST-SUB).                  ELGOUTPX
00968      MOVE TEST-SUB TO SORT-SUB.                                   ELGOUTPX
00969                                                                   ELGOUTPX
00970                                                                   ELGOUTPX
00971 /***********************************************************      ELGOUTPX
00972 *                                                          *      ELGOUTPX
00973 *    CHECK EXTRACT DATA INTEGRITY                          *      ELGOUTPX
00974 *                                                          *      ELGOUTPX
00975 ************************************************************      ELGOUTPX
00976                                                                   ELGOUTPX
00977  0490-CHK-EXTRACT-DATA-INTGRTY.                                   ELGOUTPX
00978      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELGOUTPX
00979      THEN                                                         ELGOUTPX
00980         SET   FYI-VALUE-NA TO TRUE                                ELGOUTPX
00981      END-IF.                                                      ELGOUTPX
00982                                                                   ELGOUTPX
00983      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELGOUTPX
00984      THEN                                                         ELGOUTPX
00985         SET   COST-CONTAIN-IND-NA TO TRUE                         ELGOUTPX
00986      END-IF.                                                      ELGOUTPX
00987                                                                   ELGOUTPX
00988      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELGOUTPX
00989      THEN                                                         ELGOUTPX
00990         SET   PLACE-OF-TREATMENT-NA TO TRUE                       ELGOUTPX
00991      END-IF.                                                      ELGOUTPX
00992                                                                   ELGOUTPX
00993      IF ACCUM-BENEFIT-PERIOD = ZEROS OR SPACES OR LOW-VALUES      ELGOUTPX
00994      THEN                                                         ELGOUTPX
00995         SET   BENEFIT-PERIOD-NA TO TRUE                           ELGOUTPX
00996      END-IF.                                                      ELGOUTPX
00997                                                                   ELGOUTPX
00998      IF ACCUM-BEN-PER-TIME-QUAL = ZEROS OR SPACES OR LOW-VALUES   ELGOUTPX
00999      THEN                                                         ELGOUTPX
01000         SET   BEN-PER-TIME-QUAL-NA TO TRUE                        ELGOUTPX
01001      END-IF.                                                      ELGOUTPX
01002                                                                   ELGOUTPX
01003      IF ACCUM-INTERVAL-TYPE = ZEROS OR SPACES OR LOW-VALUES       ELGOUTPX
01004      THEN                                                         ELGOUTPX
01005         SET   INTERVAL-TYPE-NA TO TRUE                            ELGOUTPX
01006      END-IF.                                                      ELGOUTPX
01007                                                                   ELGOUTPX
01008      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELGOUTPX
01009      THEN                                                         ELGOUTPX
01010         SET   INTERVAL-OVRD-IND-NA TO TRUE                        ELGOUTPX
01011      END-IF.                                                      ELGOUTPX
01012                                                                   ELGOUTPX
01013      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELGOUTPX
01014      THEN                                                         ELGOUTPX
01015         SET   L-O-B-NA TO TRUE                                    ELGOUTPX
01016      END-IF.                                                      ELGOUTPX
01017                                                                   ELGOUTPX
01018      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELGOUTPX
01019      THEN                                                         ELGOUTPX
01020         SET   REINSTATEMENT-IND-NA TO TRUE                        ELGOUTPX
01021      END-IF.                                                      ELGOUTPX
01022                                                                   ELGOUTPX
01023      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELGOUTPX
01024      THEN                                                         ELGOUTPX
01025         SET   DEFINITION-NA TO TRUE                               ELGOUTPX
01026      END-IF.                                                      ELGOUTPX
01027                                                                   ELGOUTPX
01028      IF   ACCUM-CARRY-OVER-CREDIT-IND                             ELGOUTPX
01029         = ZEROS OR SPACES OR LOW-VALUES                           ELGOUTPX
01030      THEN                                                         ELGOUTPX
01031         SET     CARRY-OVER-CREDIT-IND-NA TO TRUE                  ELGOUTPX
01032      END-IF.                                                      ELGOUTPX
01033                                                                   ELGOUTPX
01034      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELGOUTPX
01035      THEN                                                         ELGOUTPX
01036         SET   ASCEND-DESCEND-IND-NA TO TRUE                       ELGOUTPX
01037      END-IF.                                                      ELGOUTPX
01038                                                                   ELGOUTPX
01039      IF ACCUM-FAM-OR-INDIV = ZEROS OR SPACES OR LOW-VALUES        ELGOUTPX
01040      THEN                                                         ELGOUTPX
01041         SET   FAM-OR-INDIV-NA TO TRUE                             ELGOUTPX
01042      END-IF.                                                      ELGOUTPX
01043                                                                   ELGOUTPX
01044      IF ACCUM-OPX-BASE-AMT-SOURCE-IND =                           ELGOUTPX
01045          ZEROS OR SPACES OR LOW-VALUES                            ELGOUTPX
01046      THEN                                                         ELGOUTPX
01047         SET   OPX-BASE-AMT-SOURCE-IND-NA TO TRUE                  ELGOUTPX
01048      END-IF.                                                      ELGOUTPX
01049                                                                   ELGOUTPX
01050      IF ACCUM-VALUE-QUALIFIER = ZEROS OR SPACES OR LOW-VALUES     ELGOUTPX
01051      THEN                                                         ELGOUTPX
01052         SET   VALUE-QUALIFIER-NA TO TRUE                          ELGOUTPX
01053      END-IF.                                                      ELGOUTPX
01054                                                                   ELGOUTPX
01055      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELGOUTPX
01056      THEN                                                         ELGOUTPX
01057         SET   RELATIONSHIP-IND-NA TO TRUE                         ELGOUTPX
01058      END-IF.                                                      ELGOUTPX
01059                                                                   ELGOUTPX
01060      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELGOUTPX
01061      THEN                                                         ELGOUTPX
01062         SET   AGE-LMT-FROM-IND-NA TO TRUE                         ELGOUTPX
01063      END-IF.                                                      ELGOUTPX
01064                                                                   ELGOUTPX
01065      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELGOUTPX
01066      THEN                                                         ELGOUTPX
01067         SET   AGE-LMT-TO-IND-NA TO TRUE                           ELGOUTPX
01068      END-IF.                                                      ELGOUTPX
01069                                                                   ELGOUTPX
01070      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELGOUTPX
01071      THEN                                                         ELGOUTPX
01072         SET   LMT-MANDATORY-IND-NA TO TRUE                        ELGOUTPX
01073      END-IF.                                                      ELGOUTPX
01074                                                                   ELGOUTPX
01075      IF ACCUM-CO-PAY-IND (1) = ZEROS OR SPACES OR LOW-VALUES      ELGOUTPX
01076      THEN                                                         ELGOUTPX
01077         SET   CO-PAY-IND-NA (1) TO TRUE                           ELGOUTPX
01078      END-IF.                                                      ELGOUTPX
01079                                                                   ELGOUTPX
01080      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELGOUTPX
01081      THEN                                                         ELGOUTPX
01082         SET   SERVICE-GROUP-NA TO TRUE                            ELGOUTPX
01083      END-IF.                                                      ELGOUTPX
01084                                                                   ELGOUTPX
01085      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELGOUTPX
01086      THEN                                                         ELGOUTPX
01087         SET   INTERNAL-DESCRIPTOR-NA TO TRUE                      ELGOUTPX
01088      END-IF.                                                      ELGOUTPX
01089                                                                   ELGOUTPX
01090      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELGOUTPX
01091      THEN                                                         ELGOUTPX
01092         SET   DAY-FACTOR-IND-NA TO TRUE                           ELGOUTPX
01093      END-IF.                                                      ELGOUTPX
01094                                                                   ELGOUTPX
01095      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELGOUTPX
01096      THEN                                                         ELGOUTPX
01097         SET   CLAIM-LVL-ACCUM-IND-NA TO TRUE                      ELGOUTPX
01098      END-IF.                                                      ELGOUTPX
01099                                                                   ELGOUTPX
01100      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELGOUTPX
01101      THEN                                                         ELGOUTPX
01102         SET   BEN-PER-MAX-OVRD-IND-NA TO TRUE                     ELGOUTPX
01103      END-IF.                                                      ELGOUTPX
01104                                                                   ELGOUTPX
01105      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELGOUTPX
01106      THEN                                                         ELGOUTPX
01107         SET   1ST-DOLR-COVRGE-LMT-NA TO TRUE                      ELGOUTPX
01108      END-IF.                                                      ELGOUTPX
01109                                                                   ELGOUTPX
01110 /***********************************************************      ELGOUTPX
01111 *                                                          *      ELGOUTPX
01112 *        ADD ACCUM OCCURRENCE TO FILE                      *      ELGOUTPX
01113 *                                                          *      ELGOUTPX
01114 ************************************************************      ELGOUTPX
01115                                                                   ELGOUTPX
01116  0710-WRITE-EXTRACT-RECORD.                                       ELGOUTPX
01117      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGOUTPX
01118      SET  IOP-ADD TO TRUE.                                        ELGOUTPX
01119      SET  IOP-FCQ-NONE TO TRUE.                                   ELGOUTPX
01120      SET  IOP-KVQ-NONE TO TRUE.                                   ELGOUTPX
01121      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGOUTPX
01122                                                                   ELGOUTPX
01123 /***********************************************************      ELGOUTPX
01124 *                                                          *      ELGOUTPX
01125 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELGOUTPX
01126 *                                                          *      ELGOUTPX
01127 ************************************************************      ELGOUTPX
01128                                                                   ELGOUTPX
01129  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELGOUTPX
01130      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGOUTPX
01131      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGOUTPX
01132         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGOUTPX
01133         END-CALL.                                                 ELGOUTPX
01134      IF CIA-RC-PTR-NULL                                           ELGOUTPX
01135         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGOUTPX
01136         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGOUTPX
01137      END-IF.                                                      ELGOUTPX
