00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELGCOPAY
00003  PROGRAM-ID.        ELGCOPAY.                                        LV001
00004                                                                   ELGCOPAY
00005  AUTHOR.            ANNE KING.                                    ELGCOPAY
00006                                                                   ELGCOPAY
00007  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELGCOPAY
00008                     A MUTUAL LEGAL RESERVE COMPANY                ELGCOPAY
00009                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELGCOPAY
00010                     233 N. MICHIGAN AVE                           ELGCOPAY
00011                     CHICAGO, ILLINOIS 60601                       ELGCOPAY
00012                                                                   ELGCOPAY
00013  DATE-WRITTEN.      30-OCT-1998.                                  ELGCOPAY
00014                                                                   ELGCOPAY
00015  DATE-COMPILED.                                                   ELGCOPAY
00016                                                                   ELGCOPAY
00017  SECURITY.          COPYRIGHT 1986, 1992,                         ELGCOPAY
00018                     HEALTH CARE SERVICE CORPORATION               ELGCOPAY
00019                                                                   ELGCOPAY
00020  ENVIRONMENT DIVISION.                                            ELGCOPAY
00021                                                                   ELGCOPAY
00022  CONFIGURATION SECTION.                                           ELGCOPAY
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELGCOPAY
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELGCOPAY
00025                                                                   ELGCOPAY
00026 /*****************************************************************ELGCOPAY
00027 *                                                                *ELGCOPAY
00028 *  ELGCOPAY:    SELECTS #ACP COPAY ACCUMS FRON THE               *ELGCOPAY
00029 *               BENEFIT PROVISION LEVEL, PROCESS THE ACCUM,      *ELGCOPAY
00030 *               AND LINK TO THE COPAY      GENERATOR MODULE      *ELGCOPAY
00031 *               TO DISPLAY THE ACCUM INFORMATION.                *ELGCOPAY
00032 *                                                                *ELGCOPAY
00033 ******************************************************************ELGCOPAY
00034 *                                                                *ELGCOPAY
00035 *                      MAINTENANCE HISTORY                       *ELGCOPAY
00036 *                                                                *ELGCOPAY
00037 *  MOD     DATE     BY  DRPT                ACTION               *ELGCOPAY
00038 * ----- ----------- --- ----- ---------------------------------- *ELGCOPAY
00039 * 01.00 30-OCT-1998 AKK       CLONED FROM ELGDEDBL.              *ELGCOPAY
00040 *                                                                *ELGCOPAY
00041 * 01.01 12-SEP-2000 AKK       ADDED SUPPORT FOR #IPGS.           *ELGCOPAY
00042 *                                                                *ELGCOPAY
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00042 *                                                                *ELGCOPAY
00043 ***************************************************************** ELGCOPAY
00044      TITLE  'ELGCOPAY          WORKING STORAGE'.                  ELGCOPAY
00045  DATA DIVISION.                                                   ELGCOPAY
00046                                                                   ELGCOPAY
00047  WORKING-STORAGE SECTION.                                         ELGCOPAY
00048                                                                   ELGCOPAY
00049  01  SWITCHES.                                                    ELGCOPAY
00050      02 WS-PAIR-FOUND-SW                      PICTURE  X(01).     ELGCOPAY
00051         88 WS-PAIR-FOUND                      VALUE 'Y'.          ELGCOPAY
00052                                                                   ELGCOPAY
00053      02 WS-MATCH-SW                          PICTURE  X(01).      ELGCOPAY
00054         88 WS-MATCH-FOUND                    VALUE 'Y'.           ELGCOPAY
00055                                                                   ELGCOPAY
00056      02 WS-STOP-SW                           PICTURE  X(01).      ELGCOPAY
00057         88 WS-STOP                           VALUE 'Y'.           ELGCOPAY
00058                                                                   ELGCOPAY
00059      02 FIRST-TIME-SW                          PICTURE  X(01).    ELGCOPAY
00060         88 FIRST-ACP                           VALUE 'Y'.         ELGCOPAY
00061         88 NOT-FIRST-ACP                       VALUE 'N'.         ELGCOPAY
00062                                                                   ELGCOPAY
00063      02                                      PICTURE  X(01).      ELGCOPAY
00064         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELGCOPAY
00065         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELGCOPAY
00066      02                                      PICTURE  X(01).      ELGCOPAY
00067         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELGCOPAY
00068         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELGCOPAY
00069      02                                      PICTURE  X(01).      ELGCOPAY
00070         88 SW-HAS-IBGR                       VALUE 'Y'.           ELGCOPAY
00071         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELGCOPAY
00072      02                                      PICTURE  X(01).      ELGCOPAY
00073         88 SW-HAS-IDGD                       VALUE 'Y'.           ELGCOPAY
00074         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELGCOPAY
00075      02                                      PICTURE  X(01).      ELGCOPAY
00076         88 SW-HAS-IPGN                       VALUE 'Y'.           ELGCOPAY
00077         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELGCOPAY
00078      02                                      PICTURE  X(01).      ELGCOPAY
00079         88 SW-HAS-IPGP                       VALUE 'Y'.           ELGCOPAY
00080         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELGCOPAY
00081      02                                      PICTURE  X(01).      ELGCOPAY
00082         88 SW-HAS-IPGT                       VALUE 'Y'.           ELGCOPAY
00083         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELGCOPAY
00084      02                                      PICTURE  X(01).      ELGCOPAY
00085         88 SW-HAS-IPGS                       VALUE 'Y'.           ELGCOPAY
00086         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELGCOPAY
00087      02                                      PICTURE  X(01).      ELGCOPAY
00088         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELGCOPAY
00089         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELGCOPAY
00090      02                                      PICTURE  X(01).      ELGCOPAY
00091         88 SW-INTRNL-INST-PROV-CLASS         VALUE 'Y'.           ELGCOPAY
00092         88 SW-INTRNL-NOT-INST-PROV-CLASS     VALUE 'N'.           ELGCOPAY
00093         88 SW-INTRNL-INST-PROV-CLS-NOT-DT    VALUE 'X'.           ELGCOPAY
00094      02                                      PICTURE  X(01).      ELGCOPAY
00095         88 SW-INTRNL-PROF-PROV-CLASS         VALUE 'Y'.           ELGCOPAY
00096         88 SW-INTRNL-NOT-PROF-PROV-CLASS     VALUE 'N'.           ELGCOPAY
00097         88 SW-INTRNL-PROF-PROV-CLS-NOT-DT    VALUE 'X'.           ELGCOPAY
00098      02                                      PICTURE  X(01).      ELGCOPAY
00099         88 SW-INTRNL-PROF-PROV-SPEC          VALUE 'Y'.           ELGCOPAY
00100         88 SW-INTRNL-NOT-PROF-PROV-SPEC      VALUE 'N'.           ELGCOPAY
00101         88 SW-INTRNL-PROF-PROV-SPC-NOT-DT    VALUE 'X'.           ELGCOPAY
00102      02                                      PICTURE  X(01).      ELGCOPAY
00103         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELGCOPAY
00104         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELGCOPAY
00105                                                                   ELGCOPAY
00106  01  WS-PROVISION-ARGUMENT.                                       ELGCOPAY
00107      02                                      PICTURE X(05).       ELGCOPAY
00108      02 WS-PROVISION-CL                      PICTURE X(01).       ELGCOPAY
00109         88  INST-CLASS                       VALUE 'A', 'B', 'W'. ELGCOPAY
00110         88  PROF-CLASS                       VALUE 'C', 'D', 'E'. ELGCOPAY
00111                                                                   ELGCOPAY
00112  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELGCOPAY
00113      88 WS-LOB-INST              VALUE '1'.                       ELGCOPAY
00114      88 WS-LOB-PROF              VALUE '2'.                       ELGCOPAY
00115      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELGCOPAY
00116      88 WS-LOB-BOTH              VALUE '3', '4', '5', '6', '7'.   ELGCOPAY
00117                                                                   ELGCOPAY
00118  01  PROGRAM-CONSTANTS.                                           ELGCOPAY
00119      02 WS-HOLD-SUB              PICTURE S9(04) COMP.             ELGCOPAY
00120      02 PC-ACP                   PICTURE  X(06) VALUE '#ADL  '.   ELGCOPAY
00121      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELGCOPAY
00122      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELGCOPAY
00123      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELGCOPAY
00124      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELGCOPAY
00125      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELGCOPAY
00126      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGS '.   ELGCOPAY
00127      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELGCOPAY
00128                                                                   ELGCOPAY
00129  01  WS-WORK-FIELDS.                                              ELGCOPAY
00130      02 WS-OCCURRENCE-SUB        PICTURE S9(04) COMP.             ELGCOPAY
00131      02 WS-ACP-SUB               PICTURE S9(04) COMP.             ELGCOPAY
00132      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELGCOPAY
00133      02 WS-DED-GAF-IDX           INDEX.                           ELGCOPAY
00134      02 WS-HOLD-IDX              USAGE IS INDEX.                  ELGCOPAY
00135      02 HOLD-DEFINITION          PICTURE X(02).                   ELGCOPAY
00136                                                                   ELGCOPAY
00137  01  WS-OCCURRENCE-PROCESSED-TBL.                                 ELGCOPAY
00138      02                          PICTURE X                        ELGCOPAY
00139                                  OCCURS 44 TIMES                  ELGCOPAY
00140                                  INDEXED BY WS-OCCURRENCE-INDEX.  ELGCOPAY
00141          88  WS-OCCURRENCE-PROCESSED           VALUE 'P'.         ELGCOPAY
00142          88  WS-OCCURRENCE-NOT-PROCESSED       VALUE ' '.         ELGCOPAY
00143                                                                   ELGCOPAY
00144  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELGCOPAY
00145      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGCOPAY
00146      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGCOPAY
00147      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGCOPAY
00148      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGCOPAY
00149      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGCOPAY
00150      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGCOPAY
00151 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELGCOPAY
00152      COPY ELSCFTB2.                                               ELGCOPAY
00153                                                                   ELGCOPAY
00154 / -- PROVIDER TSPECCONFIDENCE FACTORS TABLE                       ELGCOPAY
00155      COPY ELSCFTB9.                                               ELGCOPAY
00156                                                                   ELGCOPAY
00157      TITLE  'ELGCOPAY          LINKAGE SECTION'                   ELGCOPAY
00158  LINKAGE SECTION.                                                 ELGCOPAY
00159  01  DFHCOMMAREA.                                                 ELGCOPAY
00160      COPY ELSCOMMC.                                               ELGCOPAY
00161 /                                                                 ELGCOPAY
00162      COPY ELSCIA2C.                                               ELGCOPAY
00163 /                                                                 ELGCOPAY
00164      COPY ELSIOPMC.                                               ELGCOPAY
00165 /                                                                 ELGCOPAY
00166      COPY ELSKEYSC.                                               ELGCOPAY
00167 /                                                                 ELGCOPAY
00168      COPY ELSSRTPC.                                               ELGCOPAY
00169 /                                                                 ELGCOPAY
00170      COPY ELSSSCBC.                                               ELGCOPAY
00171 /                                                                 ELGCOPAY
00172  01  GAF-RECORD-AREA.                                             ELGCOPAY
00173      COPY GCTACPC.                                                ELGCOPAY
00174 /                                                                 ELGCOPAY
00175  01 GCG-GRP-SPEC-RECORD-AREA.                                     ELGCOPAY
00176      COPY GCGROUPC.                                               ELGCOPAY
00177 /                                                                 ELGCOPAY
00178      COPY ELSACUMC.                                               ELGCOPAY
00179      TITLE  'ELGCOPAY  PROCEDURE DIVISION'.                       ELGCOPAY
00180  PROCEDURE DIVISION.                                              ELGCOPAY
00181 ************************************************************      ELGCOPAY
00182 *                                                          *      ELGCOPAY
00183 *    E L G C O P A Y    M A I N L I N E                    *      ELGCOPAY
00184 *                                                          *      ELGCOPAY
00185 ************************************************************      ELGCOPAY
00186                                                                   ELGCOPAY
00187  0000-MANILINE.                                                   ELGCOPAY
00188      PERFORM 0010-INITIALIZATION.                                 ELGCOPAY
00189      PERFORM 0100-PROCESS.                                        ELGCOPAY
00190      GOBACK.                                                      ELGCOPAY
00191                                                                   ELGCOPAY
00192 ************************************************************      ELGCOPAY
00193 *                                                          *      ELGCOPAY
00194 *    I N I T I A L I Z A T I O N                           *      ELGCOPAY
00195 *                                                          *      ELGCOPAY
00196 ************************************************************      ELGCOPAY
00197  0010-INITIALIZATION.                                             ELGCOPAY
00198      PERFORM 0015-EST-ADR-OF-CNTRL-BLKS.                          ELGCOPAY
00199      PERFORM 0030-EST-ADR-KEY-WK-AREA.                            ELGCOPAY
00200      PERFORM 0045-EST-ADR-OF-SUBROUTINE-PAR.                      ELGCOPAY
00201      PERFORM 0060-EST-ADR-BEN-PRVN-ACCUM.                         ELGCOPAY
00202      PERFORM 0075-EST-ADRSABLTY-GRP-SPC.                          ELGCOPAY
00203      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGCOPAY
00204                                                                   ELGCOPAY
00205 ************************************************************      ELGCOPAY
00206 *                                                          *      ELGCOPAY
00207 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGCOPAY
00208 *                                                          *      ELGCOPAY
00209 ************************************************************      ELGCOPAY
00210  0015-EST-ADR-OF-CNTRL-BLKS.                                      ELGCOPAY
00211 *->  EXECUTED BY 0010-INITIALIZATION                              ELGCOPAY
00212      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGCOPAY
00213         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGCOPAY
00214      ELSE                                                         ELGCOPAY
00215         IF ECA-CIA-PTR = NULL                                     ELGCOPAY
00216            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGCOPAY
00217         ELSE                                                      ELGCOPAY
00218            CALL 'ELUINISM' USING DFHCOMMAREA                      ELGCOPAY
00219               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELGCOPAY
00220                  END-CALL                                         ELGCOPAY
00221            SET CIA-ELSSSCB-DDN TO TRUE                            ELGCOPAY
00222            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELGCOPAY
00223               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELGCOPAY
00224                  END-CALL                                         ELGCOPAY
00225            IF CIA-RC-PTR-NULL                                     ELGCOPAY
00226               PERFORM 0900-CIA-AB-UNALLOC-AREA                    ELGCOPAY
00227            ELSE                                                   ELGCOPAY
00228               CONTINUE                                            ELGCOPAY
00229            END-IF                                                 ELGCOPAY
00230         END-IF                                                    ELGCOPAY
00231      END-IF.                                                      ELGCOPAY
00232                                                                   ELGCOPAY
00233 ************************************************************      ELGCOPAY
00234 *                                                          *      ELGCOPAY
00235 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELGCOPAY
00236 *                                                          *      ELGCOPAY
00237 ************************************************************      ELGCOPAY
00238  0030-EST-ADR-KEY-WK-AREA.                                        ELGCOPAY
00239 *->  EXECUTED BY 0010-INITIALIZATION                              ELGCOPAY
00240      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGCOPAY
00241      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOPAY
00242         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELGCOPAY
00243            END-CALL.                                              ELGCOPAY
00244      IF CIA-RC-PTR-NULL                                           ELGCOPAY
00245         PERFORM 0900-CIA-AB-UNALLOC-AREA.                         ELGCOPAY
00246                                                                   ELGCOPAY
00247 ************************************************************      ELGCOPAY
00248 *                                                          *      ELGCOPAY
00249 *    ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS     *      ELGCOPAY
00250 *                                                          *      ELGCOPAY
00251 ************************************************************      ELGCOPAY
00252  0045-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGCOPAY
00253 *->  EXECUTED BY 0010-INITIALIZATION                              ELGCOPAY
00254      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGCOPAY
00255      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOPAY
00256         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELGCOPAY
00257            END-CALL.                                              ELGCOPAY
00258      IF CIA-RC-PTR-NULL                                           ELGCOPAY
00259         PERFORM 0900-CIA-AB-UNALLOC-AREA.                         ELGCOPAY
00260                                                                   ELGCOPAY
00261 ************************************************************      ELGCOPAY
00262 *                                                          *      ELGCOPAY
00263 *    ESTABLISH ADDRESSABILITY OF BENEFIT PROVISION LEVEL   *      ELGCOPAY
00264 *    ACCUMULATOR RECORD                                    *      ELGCOPAY
00265 *                                                          *      ELGCOPAY
00266 ************************************************************      ELGCOPAY
00267  0060-EST-ADR-BEN-PRVN-ACCUM.                                     ELGCOPAY
00268 *->  EXECUTED BY 0010-INITIALIZATION                              ELGCOPAY
00269      SET CIA-GCTABULR-DDN TO TRUE.                                ELGCOPAY
00270      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOPAY
00271         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGCOPAY
00272            END-CALL.                                              ELGCOPAY
00273      IF CIA-RC-PTR-NULL                                           ELGCOPAY
00274         PERFORM 0900-CIA-AB-UNALLOC-AREA                          ELGCOPAY
00275      ELSE                                                         ELGCOPAY
00276         IF IOP-REC-PTR = NULLS                                    ELGCOPAY
00277            SET CIA-AB-UNALLOC-AREA TO TRUE                        ELGCOPAY
00278            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELGCOPAY
00279         ELSE                                                      ELGCOPAY
00280            SET ADDRESS OF GAF-RECORD-AREA TO IOP-REC-PTR          ELGCOPAY
00281         END-IF                                                    ELGCOPAY
00282      END-IF.                                                      ELGCOPAY
00283                                                                   ELGCOPAY
00284 ************************************************************      ELGCOPAY
00285 *                                                          *      ELGCOPAY
00286 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD     *      ELGCOPAY
00287 *                                                          *      ELGCOPAY
00288 ************************************************************      ELGCOPAY
00289 *->  EXECUTED BY 0010-INITIALIZATION                              ELGCOPAY
00290  0075-EST-ADRSABLTY-GRP-SPC.                                      ELGCOPAY
00291      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELGCOPAY
00292      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOPAY
00293         ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                       ELGCOPAY
00294            END-CALL.                                              ELGCOPAY
00295      IF CIA-RC-PTR-NULL                                           ELGCOPAY
00296         PERFORM 0900-CIA-AB-UNALLOC-AREA.                         ELGCOPAY
00297                                                                   ELGCOPAY
00298 /***********************************************************      ELGCOPAY
00299 *                                                          *      ELGCOPAY
00300 *        PROCESS                                           *      ELGCOPAY
00301 *                                                          *      ELGCOPAY
00302 ************************************************************      ELGCOPAY
00303  0100-PROCESS.                                                    ELGCOPAY
00304 *->  EXECUTED BY 0000-MANILINE                                    ELGCOPAY
00305      PERFORM 0240-SCAN-FOR-APPLIC-OCCRNCS.                        ELGCOPAY
00306      IF SW-NO-APPLIC-ACCUM-FOUND                                  ELGCOPAY
00307         EVALUATE TRUE                                             ELGCOPAY
00308            WHEN SSB-PROV-CLASS-INST                               ELGCOPAY
00309               SET SRP-INST-NOT-APPLICABLE TO TRUE                 ELGCOPAY
00310            WHEN SSB-PROV-CLASS-PROF                               ELGCOPAY
00311               SET SRP-PROF-NOT-APPLICABLE TO TRUE                 ELGCOPAY
00312            WHEN SSB-PROV-CLASS-BOTH                               ELGCOPAY
00313               SET SRP-NO-ACCUMS-FOUND TO TRUE                     ELGCOPAY
00314            WHEN OTHER                                             ELGCOPAY
00315               SET CIA-AB-PGM-LOGIC TO TRUE                        ELGCOPAY
00316               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGCOPAY
00317         END-EVALUATE                                              ELGCOPAY
00318      END-IF.                                                      ELGCOPAY
00319                                                                   ELGCOPAY
00320      SET SRP-TOPIC-ACCUM TO TRUE.                                 ELGCOPAY
00321                                                                   ELGCOPAY
00322 *->  LINK TO THE OUTPUT GENERATOR                                 ELGCOPAY
00323      EXEC CICS LINK  PROGRAM ('ELGACP')                           ELGCOPAY
00324                      COMMAREA (DFHCOMMAREA)   END-EXEC.           ELGCOPAY
00325                                                                   ELGCOPAY
00326 ************************************************************      ELGCOPAY
00327 *                                                          *      ELGCOPAY
00328 *    SCAN ADL ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELGCOPAY
00329 *                                                          *      ELGCOPAY
00330 ************************************************************      ELGCOPAY
00331  0240-SCAN-FOR-APPLIC-OCCRNCS.                                    ELGCOPAY
00332 *->  EXECUTED BY 0100-PROCESS                                     ELGCOPAY
00333      PERFORM 0800-EST-ADR-OF-TEMPORARY-FILE.                      ELGCOPAY
00334      PERFORM 0250-DELETE-ACP-SUMMARY-FILE.                        ELGCOPAY
00335      PERFORM 0260-ALLOC-WORKFILE-REC-AREA.                        ELGCOPAY
00336                                                                   ELGCOPAY
00337 *->  SET UPPER LIMIT ON TABULAR SCAN                              ELGCOPAY
00338      SET GAF-INDEX TO GAF-ENTRY-COUNT.                            ELGCOPAY
00339      SET WS-DED-GAF-IDX TO GAF-INDEX.                             ELGCOPAY
00340 *->  SCAN ACCUMULATOR TABULAR                                     ELGCOPAY
00341      PERFORM WITH TEST BEFORE                                     ELGCOPAY
00342         VARYING WS-OCCURRENCE-SUB FROM 1 BY 1                     ELGCOPAY
00343            UNTIL WS-OCCURRENCE-SUB >= GAF-ENTRY-COUNT             ELGCOPAY
00344            IF WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB)         ELGCOPAY
00345               CONTINUE                                            ELGCOPAY
00346            ELSE                                                   ELGCOPAY
00347               SET GAF-INDEX TO WS-OCCURRENCE-SUB                  ELGCOPAY
00348               PERFORM 0300-TEST-ACP-OCCURRENCE                    ELGCOPAY
00349            END-IF                                                 ELGCOPAY
00350      END-PERFORM.                                                 ELGCOPAY
00351                                                                   ELGCOPAY
00352 *    PERFORM 0300-TEST-ACP-OCCURENCE                              ELGCOPAY
00353 *       VARYING GAF-INDEX FROM 1 BY 1                             ELGCOPAY
00354 *          UNTIL GAF-INDEX = WS-DED-GAF-IDX.                      ELGCOPAY
00355                                                                   ELGCOPAY
00356 ************************************************************      ELGCOPAY
00357 *                                                          *      ELGCOPAY
00358 *        DELETE ADL SUMMARY FILE                           *      ELGCOPAY
00359 *                                                          *      ELGCOPAY
00360 ************************************************************      ELGCOPAY
00361  0250-DELETE-ACP-SUMMARY-FILE.                                    ELGCOPAY
00362 *->  EXECUTED BY 0240-SCAN-FOR-APPLIC-OCCRNCS                     ELGCOPAY
00363      SET IOP-DEL TO TRUE.                                         ELGCOPAY
00364      SET IOP-FCQ-NONE TO TRUE.                                    ELGCOPAY
00365      SET IOP-KVQ-NONE TO TRUE.                                    ELGCOPAY
00366      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGCOPAY
00367                                                                   ELGCOPAY
00368 ************************************************************      ELGCOPAY
00369 *                                                          *      ELGCOPAY
00370 *    ALLOCATE WORKFILE RECORD AREA                         *      ELGCOPAY
00371 *                                                          *      ELGCOPAY
00372 ************************************************************      ELGCOPAY
00373  0260-ALLOC-WORKFILE-REC-AREA.                                    ELGCOPAY
00374 *->  EXECUTED BY 0240-SCAN-FOR-APPLIC-OCCRNCS                     ELGCOPAY
00375      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGCOPAY
00376      SET CIA-STG-GETMAIN TO TRUE.                                 ELGCOPAY
00377      SET IOP-GETMAIN-REC TO TRUE.                                 ELGCOPAY
00378      COMPUTE IOP-MAX-REC-LEN =                                    ELGCOPAY
00379              LENGTH OF ACCUM-FIXED-AREA +                         ELGCOPAY
00380 *            LENGTH OF ACCUM-ASCEND-DESCEND-COUNT +               ELGCOPAY
00381              LENGTH OF ACCUM-VARIABLE-AREA +                      ELGCOPAY
00382              LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELGCOPAY
00383 *           (PC-MAXIMUM-NBR-OCCURS *                              ELGCOPAY
00384 *            LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).              ELGCOPAY
00385                                                                   ELGCOPAY
00386      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGCOPAY
00387      IF IOP-REC-PTR = NULLS                                       ELGCOPAY
00388         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGCOPAY
00389         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGCOPAY
00390      ELSE                                                         ELGCOPAY
00391         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELGCOPAY
00392      END-IF.                                                      ELGCOPAY
00393                                                                   ELGCOPAY
00394 /***********************************************************      ELGCOPAY
00395 *                                                          *      ELGCOPAY
00396 *        TEST ADL OCCURS                                   *      ELGCOPAY
00397 *                                                          *      ELGCOPAY
00398 ************************************************************      ELGCOPAY
00399  0300-TEST-ACP-OCCURRENCE.                                        ELGCOPAY
00400 *->  EXECUTED BY 0240-SCAN-FOR-APPLIC-OCCRNCS                     ELGCOPAY
00401      MOVE GAF-COPAY-L-O-B (GAF-INDEX) TO WS-LOB-ACCUM-OCCRNC.     ELGCOPAY
00402      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELGCOPAY
00403      SET SW-HAS-NO-IBGR                                           ELGCOPAY
00404          SW-HAS-NO-IDGD                                           ELGCOPAY
00405          SW-HAS-NO-IPGN                                           ELGCOPAY
00406          SW-HAS-NO-IPGP                                           ELGCOPAY
00407          SW-HAS-NO-IPGS                                           ELGCOPAY
00408          SW-HAS-NO-IPGT TO TRUE.                                  ELGCOPAY
00409                                                                   ELGCOPAY
00410      INITIALIZE WS-IBGR-SLOT-NBR                                  ELGCOPAY
00411                 WS-IDGD-SLOT-NBR                                  ELGCOPAY
00412                 WS-IPGN-SLOT-NBR                                  ELGCOPAY
00413                 WS-IPGP-SLOT-NBR                                  ELGCOPAY
00414                 WS-IPGS-SLOT-NBR                                  ELGCOPAY
00415                 WS-IPGT-SLOT-NBR.                                 ELGCOPAY
00416      SET SW-INTRNL-INST-PROV-CLS-NOT-DT                           ELGCOPAY
00417          SW-INTRNL-PROF-PROV-CLS-NOT-DT TO TRUE.                  ELGCOPAY
00418                                                                   ELGCOPAY
00419 *->  SCAN FOR INTERNAL TABULARS                                   ELGCOPAY
00420 *    (THIS IS DONE NOW IN CASE IPGT IS NEEDED TO DETERMINE        ELGCOPAY
00421 *     WHETHER OCCURRENCE IS INSTITUTIONAL OR PROFESSIONAL.        ELGCOPAY
00422 *     THE IBGR IS IGNORED, SINCE THE ACCUMULATOR IS DIRECTLY      ELGCOPAY
00423 *     ATTACHED TO THE PROVISION.)                                 ELGCOPAY
00424                                                                   ELGCOPAY
00425      PERFORM 0310-SCAN-THE-INTERNAL-TABULAR                       ELGCOPAY
00426         VARYING GAF-INT-INDEX FROM 1 BY 1                         ELGCOPAY
00427           UNTIL GAF-INT-INDEX  >=                                 ELGCOPAY
00428                 GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX).           ELGCOPAY
00429                                                                   ELGCOPAY
00430      EVALUATE TRUE ALSO TRUE                                      ELGCOPAY
00431         WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                        ELGCOPAY
00432            SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE                  ELGCOPAY
00433            SET SW-OCCRNC-APPLIES TO TRUE                          ELGCOPAY
00434         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH                 ELGCOPAY
00435            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGCOPAY
00436            SET SW-OCCRNC-APPLIES TO TRUE                          ELGCOPAY
00437         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST                 ELGCOPAY
00438            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGCOPAY
00439            SET SW-OCCRNC-APPLIES TO TRUE                          ELGCOPAY
00440         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH                 ELGCOPAY
00441            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGCOPAY
00442            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGCOPAY
00443            SET SW-OCCRNC-APPLIES TO TRUE                          ELGCOPAY
00444         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF                 ELGCOPAY
00445            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGCOPAY
00446            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGCOPAY
00447            SET SW-OCCRNC-APPLIES TO TRUE                          ELGCOPAY
00448         WHEN OTHER                                                ELGCOPAY
00449            CONTINUE                                               ELGCOPAY
00450      END-EVALUATE.                                                ELGCOPAY
00451                                                                   ELGCOPAY
00452 *->  SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD               ELGCOPAY
00453      IF SW-OCCRNC-APPLIES                                         ELGCOPAY
00454         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELGCOPAY
00455         PERFORM 0320-INIT-ACCUM-EXTRACT                           ELGCOPAY
00456         PERFORM 0330-EXTRACT-ACCUM                                ELGCOPAY
00457         PERFORM 0375-VARIABLE-DATA                                ELGCOPAY
00458         PERFORM 0410-CHK-EXTRACT-DATA-INTGRTY                     ELGCOPAY
00459         PERFORM 0700-WRITE-EXTRACT-RECORD                         ELGCOPAY
00460      END-IF.                                                      ELGCOPAY
00461                                                                   ELGCOPAY
00462 /***********************************************************      ELGCOPAY
00463 *                                                          *      ELGCOPAY
00464 *        SCAN THE INTERNAL TABULARS                        *      ELGCOPAY
00465 *                                                          *      ELGCOPAY
00466 ************************************************************      ELGCOPAY
00467  0310-SCAN-THE-INTERNAL-TABULAR.                                  ELGCOPAY
00468 *->  EXECUTED BY 0300-TEST-ACP-OCCURENCE                          ELGCOPAY
00469      IF GAF-INT-SLOT (GAF-INDEX, GAF-INT-INDEX) > 0               ELGCOPAY
00470         MOVE GAF-INT-SLOT (GAF-INDEX, GAF-INT-INDEX)              ELGCOPAY
00471           TO WS-SLOT-NBR                                          ELGCOPAY
00472         EVALUATE GAF-INT-ID (GAF-INDEX, GAF-INT-INDEX)            ELGCOPAY
00473            WHEN PC-IBGR                                           ELGCOPAY
00474               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELGCOPAY
00475               SET SW-HAS-IBGR                                     ELGCOPAY
00476                TO TRUE                                            ELGCOPAY
00477            WHEN PC-IDGD                                           ELGCOPAY
00478               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELGCOPAY
00479               SET SW-HAS-IDGD                                     ELGCOPAY
00480                TO TRUE                                            ELGCOPAY
00481            WHEN PC-IPGP                                           ELGCOPAY
00482               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELGCOPAY
00483               SET SW-HAS-IPGP                                     ELGCOPAY
00484                TO TRUE                                            ELGCOPAY
00485            WHEN PC-IPGN                                           ELGCOPAY
00486               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELGCOPAY
00487               SET SW-HAS-IPGN                                     ELGCOPAY
00488                TO TRUE                                            ELGCOPAY
00489            WHEN PC-IPGT                                           ELGCOPAY
00490               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELGCOPAY
00491               SET SW-HAS-IPGT                                     ELGCOPAY
00492                TO TRUE                                            ELGCOPAY
00493            WHEN PC-IPGS                                           ELGCOPAY
00494               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELGCOPAY
00495               SET SW-HAS-IPGS                                     ELGCOPAY
00496                TO TRUE                                            ELGCOPAY
00497            WHEN OTHER                                             ELGCOPAY
00498                 CONTINUE                                          ELGCOPAY
00499         END-EVALUATE                                              ELGCOPAY
00500      END-IF.                                                      ELGCOPAY
00501                                                                   ELGCOPAY
00502 ************************************************************      ELGCOPAY
00503 *                                                          *      ELGCOPAY
00504 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELGCOPAY
00505 *                                                          *      ELGCOPAY
00506 ************************************************************      ELGCOPAY
00507  0320-INIT-ACCUM-EXTRACT.                                         ELGCOPAY
00508 *->  EXECUTED BY 0300-TEST-ACP-OCCURENCE                          ELGCOPAY
00509      INITIALIZE ACCUM-FIXED-AREA.                                 ELGCOPAY
00510      SET ACCUM-ACP TO TRUE.                                       ELGCOPAY
00511      MOVE 1 TO  ACCUM-ASCEND-DESCEND-COUNT.                       ELGCOPAY
00512      SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELGCOPAY
00513      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELGCOPAY
00514                                                                   ELGCOPAY
00515 /***********************************************************      ELGCOPAY
00516 *                                                          *      ELGCOPAY
00517 *        SUMMARIZE ADL TOPIC LEVEL DATA ELEMENTS           *      ELGCOPAY
00518 *                                                          *      ELGCOPAY
00519 ************************************************************      ELGCOPAY
00520  0330-EXTRACT-ACCUM.                                              ELGCOPAY
00521 *->  EXECUTED BY 0300-TEST-ACP-OCCURENCE                          ELGCOPAY
00522 *    SET FIXED PORTION DATA ELEMENTS                              ELGCOPAY
00523      MOVE GAF-COPAY-MANDATORY-IND (GAF-INDEX)                     ELGCOPAY
00524        TO ACCUM-LMT-MANDATORY-IND.                                ELGCOPAY
00525      MOVE GAF-COPAY-FYI-VALUE (GAF-INDEX) TO ACCUM-FYI-VALUE.     ELGCOPAY
00526      MOVE GAF-COPAY-COST-CONTAIN-IND (GAF-INDEX)                  ELGCOPAY
00527        TO ACCUM-COST-CONTAIN-IND.                                 ELGCOPAY
00528      MOVE GAF-COPAY-PLACE-OF-TREATMENT (GAF-INDEX)                ELGCOPAY
00529        TO ACCUM-PLACE-OF-TREATMENT.                               ELGCOPAY
00530 *    MOVE GAF-COPAY-BENEFIT-PERIOD (GAF-INDEX)                    ELGCOPAY
00531 *      TO ACCUM-BENEFIT-PERIOD.                                   ELGCOPAY
00532      MOVE GAF-COPAY-BEN-PER-TIME-FCTR (GAF-INDEX)                 ELGCOPAY
00533        TO ACCUM-BEN-PER-TIME-FCTR.                                ELGCOPAY
00534      MOVE GAF-COPAY-BEN-PER-TIME-QUAL (GAF-INDEX)                 ELGCOPAY
00535        TO ACCUM-BEN-PER-TIME-QUAL.                                ELGCOPAY
00536      MOVE GAF-COPAY-INTERVAL-TIME-FCTR (GAF-INDEX)                ELGCOPAY
00537        TO ACCUM-INTERVAL-TIME-FCTR.                               ELGCOPAY
00538      MOVE GAF-COPAY-INTERVAL-TYPE (GAF-INDEX)                     ELGCOPAY
00539        TO ACCUM-INTERVAL-TYPE.                                    ELGCOPAY
00540      MOVE GAF-COPAY-INTERVAL-OVRD-IND (GAF-INDEX)                 ELGCOPAY
00541        TO ACCUM-INTERVAL-OVRD-IND.                                ELGCOPAY
00542      MOVE GAF-COPAY-INTERVAL-OVRD-VALUE (GAF-INDEX)               ELGCOPAY
00543        TO ACCUM-INTERVAL-OVRD-VALUE.                              ELGCOPAY
00544      MOVE GAF-COPAY-L-O-B (GAF-INDEX) TO ACCUM-L-O-B.             ELGCOPAY
00545 *    MOVE GAF-COPAY-DEFINITION (GAF-INDEX) TO ACCUM-DEFINITION.   ELGCOPAY
00546      SET CARRY-OVER-CREDIT-IND-NA                                 ELGCOPAY
00547             ASCEND-DESCEND-IND-NA TO TRUE.                        ELGCOPAY
00548      MOVE GAF-COPAY-CONDITION (GAF-INDEX) TO ACCUM-CONDITION.     ELGCOPAY
00549      MOVE GAF-COPAY-FAM-OR-INDIV (GAF-INDEX)                      ELGCOPAY
00550        TO ACCUM-FAM-OR-INDIV.                                     ELGCOPAY
00551      MOVE ZEROS TO ACCUM-MAX-BASE-AMT-SOURCE-IND.                 ELGCOPAY
00552      MOVE ZEROS TO ACCUM-OPX-BASE-AMT-SOURCE-IND.                 ELGCOPAY
00553      MOVE GCG-DED-BASE-AMT-SOURCE-IND                             ELGCOPAY
00554        TO ACCUM-DED-BASE-AMT-SOURCE-IND.                          ELGCOPAY
00555 *    MOVE GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX)                   ELGCOPAY
00556 *      TO ACCUM-VALUE-QUALIFIER.                                  ELGCOPAY
00557      MOVE GAF-COPAY-RELATIONSHIP-IND (GAF-INDEX)                  ELGCOPAY
00558        TO ACCUM-RELATIONSHIP-IND.                                 ELGCOPAY
00559      MOVE GAF-COPAY-AGE-LIMIT-FROM (GAF-INDEX)                    ELGCOPAY
00560        TO ACCUM-AGE-LIMIT-FROM-VAL.                               ELGCOPAY
00561      MOVE GAF-COPAY-AGE-QUAL-IND-FROM (GAF-INDEX)                 ELGCOPAY
00562        TO ACCUM-AGE-LIMIT-FROM-IND.                               ELGCOPAY
00563      MOVE GAF-COPAY-AGE-LIMIT-TO (GAF-INDEX)                      ELGCOPAY
00564        TO ACCUM-AGE-LIMIT-TO-VAL.                                 ELGCOPAY
00565      MOVE GAF-COPAY-AGE-QUAL-IND-TO (GAF-INDEX)                   ELGCOPAY
00566        TO ACCUM-AGE-LIMIT-TO-IND.                                 ELGCOPAY
00567      SET LMT-MANDATORY-IND-NA TO TRUE.                            ELGCOPAY
00568 *    MOVE GAF-COPAY-CO-PAY-IND (GAF-INDEX)                        ELGCOPAY
00569 *      TO ACCUM-CO-PAY-IND.                                       ELGCOPAY
00570      MOVE GAF-COPAY-SERVICE-GROUP (GAF-INDEX)                     ELGCOPAY
00571        TO ACCUM-SERVICE-GROUP.                                    ELGCOPAY
00572      MOVE GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX)               ELGCOPAY
00573        TO ACCUM-INTERNAL-DESCRIPTOR.                              ELGCOPAY
00574      MOVE GAF-COPAY-DAY-FACTOR-IND (GAF-INDEX)                    ELGCOPAY
00575        TO ACCUM-DAY-FACTOR-IND.                                   ELGCOPAY
00576      MOVE GAF-COPAY-CLAIM-LVL-ACCUM-IND (GAF-INDEX)               ELGCOPAY
00577        TO ACCUM-CLAIM-LVL-ACCUM-IND.                              ELGCOPAY
00578      SET 1ST-DOLR-COVRGE-LMT-NA                                   ELGCOPAY
00579 *        CARRY-OVER-CREDIT-NA                                     ELGCOPAY
00580          BEN-PER-MAX-OVRD-IND-NA TO TRUE.                         ELGCOPAY
00581 *    MOVE GAF-TIME-DOLLAR-IND(GAF-INDEX) TO                       ELGCOPAY
00582 *        ACCUM-TIME-DOLLAR-IND.                                   ELGCOPAY
00583                                                                   ELGCOPAY
00584 *->  SET OCCURRENCE PROVIDER CLASS INFORMATION                    ELGCOPAY
00585      EVALUATE TRUE ALSO TRUE                                      ELGCOPAY
00586         WHEN      SW-INTRNL-INST-PROV-CLASS                       ELGCOPAY
00587              ALSO SW-INTRNL-NOT-PROF-PROV-CLASS                   ELGCOPAY
00588            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELGCOPAY
00589         WHEN      SW-INTRNL-NOT-INST-PROV-CLASS                   ELGCOPAY
00590              ALSO SW-INTRNL-PROF-PROV-CLASS                       ELGCOPAY
00591            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELGCOPAY
00592         WHEN      SW-INTRNL-NOT-INST-PROV-CLASS                   ELGCOPAY
00593              ALSO SW-INTRNL-PROF-PROV-SPEC                        ELGCOPAY
00594            SET ACCUM-PRVDR-SPC-PROF TO TRUE                       ELGCOPAY
00595         WHEN OTHER                                                ELGCOPAY
00596            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELGCOPAY
00597      END-EVALUATE.                                                ELGCOPAY
00598                                                                   ELGCOPAY
00599 *->  SET VARIABLE PORTION DATA ELEMENTS                           ELGCOPAY
00600      MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                       ELGCOPAY
00601        TO ACCUM-VALUE-LIMIT (1).                                  ELGCOPAY
00602      MOVE WS-IBGR-SLOT-NBR TO ACCUM-IBGR-SLOT-NBR (1).            ELGCOPAY
00603      MOVE WS-IDGD-SLOT-NBR TO ACCUM-IDGD-SLOT-NBR (1).            ELGCOPAY
00604      MOVE WS-IPGN-SLOT-NBR TO ACCUM-IPGN-SLOT-NBR (1).            ELGCOPAY
00605      MOVE WS-IPGP-SLOT-NBR TO ACCUM-IPGP-SLOT-NBR (1).            ELGCOPAY
00606      MOVE WS-IPGT-SLOT-NBR TO ACCUM-IPGT-SLOT-NBR (1).            ELGCOPAY
00607      MOVE WS-IPGS-SLOT-NBR TO ACCUM-IPGS-SLOT-NBR (1).            ELGCOPAY
00608                                                                   ELGCOPAY
00609 /***********************************************************      ELGCOPAY
00610 *                                                          *      ELGCOPAY
00611 *    VARIABLE DATA                                         *      ELGCOPAY
00612 *                                                          *      ELGCOPAY
00613 ************************************************************      ELGCOPAY
00614  0375-VARIABLE-DATA.                                              ELGCOPAY
00615       IF FIRST-ACP                                                ELGCOPAY
00616          SET COPAY-INDEX TO 1                                     ELGCOPAY
00617          SET NOT-FIRST-ACP TO TRUE                                ELGCOPAY
00618       END-IF.                                                     ELGCOPAY
00619       EVALUATE TRUE                                               ELGCOPAY
00620       WHEN GAF-COPAY-DEFINITION (GAF-INDEX) = '0C' OR '0D'        ELGCOPAY
00621          OR '0E'                                                  ELGCOPAY
00622            SET COPAY-INDEX TO 1                                   ELGCOPAY
00623            PERFORM 0480-LOAD-VARIABLE-FIELDS                      ELGCOPAY
00624            SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)         ELGCOPAY
00625              TO TRUE                                              ELGCOPAY
00626       WHEN GAF-COPAY-DEFINITION (GAF-INDEX) = '00'                ELGCOPAY
00627            IF GAF-COPAY-TIME-DOLLAR-IND(GAF-INDEX) = '00'         ELGCOPAY
00628               SET COPAY-INDEX TO 1                                ELGCOPAY
00629               PERFORM 0480-LOAD-VARIABLE-FIELDS                   ELGCOPAY
00630               SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)      ELGCOPAY
00631                 TO TRUE                                           ELGCOPAY
00632               SET WS-HOLD-IDX TO GAF-INDEX                        ELGCOPAY
00633            ELSE                                                   ELGCOPAY
00634               PERFORM 0480-LOAD-VARIABLE-FIELDS                   ELGCOPAY
00635               SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)      ELGCOPAY
00636                 TO TRUE                                           ELGCOPAY
00637               PERFORM 0400-LOAD-CHAINED-OCCURS                    ELGCOPAY
00638            END-IF                                                 ELGCOPAY
00639       WHEN GAF-COPAY-DEFINITION (GAF-INDEX) = '0A' OR             ELGCOPAY
00640            '0B' OR '0F'                                           ELGCOPAY
00641            IF GAF-COPAY-TIME-DOLLAR-IND(GAF-INDEX) = '00'         ELGCOPAY
00642               PERFORM 0480-LOAD-VARIABLE-FIELDS                   ELGCOPAY
00643               SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)      ELGCOPAY
00644                 TO TRUE                                           ELGCOPAY
00645               ADD 1 TO WS-OCCURRENCE-SUB                          ELGCOPAY
00646               MOVE GAF-COPAY-DEFINITION (GAF-INDEX)               ELGCOPAY
00647                  TO HOLD-DEFINITION                               ELGCOPAY
00648               PERFORM 0498-COMPLETE-PAIR                          ELGCOPAY
00649               SET GAF-INDEX TO WS-HOLD-SUB                        ELGCOPAY
00650            ELSE                                                   ELGCOPAY
00651               CONTINUE                                            ELGCOPAY
00652 *             PERFORM 9999-CODING-ERROR                           ELGCOPAY
00653            END-IF                                                 ELGCOPAY
00654       WHEN OTHER                                                  ELGCOPAY
00655 *MAY BE AN ERROR ROUTINE                                          ELGCOPAY
00656             CONTINUE                                              ELGCOPAY
00657       END-EVALUATE.                                               ELGCOPAY
00658       INITIALIZE WS-PAIR-FOUND-SW.                                ELGCOPAY
00659                                                                   ELGCOPAY
00660 /***********************************************************      ELGCOPAY
00661 *                                                          *      ELGCOPAY
00662 *    CHECK EXTRACT DATA INTEGRITY                          *      ELGCOPAY
00663 *                                                          *      ELGCOPAY
00664 ************************************************************      ELGCOPAY
00665  0400-LOAD-CHAINED-OCCURS.                                        ELGCOPAY
00666 *     INITIALIZE WS-STOP-SW.                                      ELGCOPAY
00667       PERFORM UNTIL WS-STOP                                       ELGCOPAY
00668          IF GAF-COPAY-TIME-DOLLAR-IND(GAF-INDEX) = 'A1'           ELGCOPAY
00669               PERFORM 0480-LOAD-VARIABLE-FIELDS                   ELGCOPAY
00670               SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)      ELGCOPAY
00671                 TO TRUE                                           ELGCOPAY
00672               ADD 1 TO WS-OCCURRENCE-SUB                          ELGCOPAY
00673 *             PERFORM 9900-SET-INDICES                            ELGCOPAY
00674               SET GAF-INDEX UP BY 1                               ELGCOPAY
00675               PERFORM 0500-FIND-RELATED-PAIR                      ELGCOPAY
00676          END-IF                                                   ELGCOPAY
00677       END-PERFORM.                                                ELGCOPAY
00678                                                                   ELGCOPAY
00679 /***********************************************************      ELGCOPAY
00680 *                                                          *      ELGCOPAY
00681 *    CHECK EXTRACT DATA INTEGRITY                          *      ELGCOPAY
00682 *                                                          *      ELGCOPAY
00683 ************************************************************      ELGCOPAY
00684                                                                   ELGCOPAY
00685  0410-CHK-EXTRACT-DATA-INTGRTY.                                   ELGCOPAY
00686 *->  EXECUTED BY 0300-TEST-ACP-OCCURENCE                          ELGCOPAY
00687      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELGCOPAY
00688         SET FYI-VALUE-NA TO TRUE.                                 ELGCOPAY
00689                                                                   ELGCOPAY
00690      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELGCOPAY
00691         SET COST-CONTAIN-IND-NA TO TRUE.                          ELGCOPAY
00692                                                                   ELGCOPAY
00693      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELGCOPAY
00694         SET PLACE-OF-TREATMENT-NA TO TRUE.                        ELGCOPAY
00695                                                                   ELGCOPAY
00696      IF ACCUM-BENEFIT-PERIOD = ZEROS OR SPACES OR LOW-VALUES      ELGCOPAY
00697         SET BENEFIT-PERIOD-NA TO TRUE.                            ELGCOPAY
00698                                                                   ELGCOPAY
00699      IF ACCUM-BEN-PER-TIME-QUAL = ZEROS OR SPACES OR LOW-VALUES   ELGCOPAY
00700         SET BEN-PER-TIME-QUAL-NA TO TRUE.                         ELGCOPAY
00701                                                                   ELGCOPAY
00702      IF ACCUM-INTERVAL-TYPE = ZEROS OR SPACES OR LOW-VALUES       ELGCOPAY
00703         SET INTERVAL-OVRD-IND-NA TO TRUE.                         ELGCOPAY
00704                                                                   ELGCOPAY
00705      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELGCOPAY
00706         SET INTERVAL-OVRD-IND-NA TO TRUE.                         ELGCOPAY
00707                                                                   ELGCOPAY
00708      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELGCOPAY
00709         SET L-O-B-NA TO TRUE.                                     ELGCOPAY
00710                                                                   ELGCOPAY
00711      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELGCOPAY
00712         SET REINSTATEMENT-IND-NA TO TRUE.                         ELGCOPAY
00713                                                                   ELGCOPAY
00714      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELGCOPAY
00715         SET DEFINITION-NA TO TRUE.                                ELGCOPAY
00716                                                                   ELGCOPAY
00717      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELGCOPAY
00718         SET ASCEND-DESCEND-IND-NA TO TRUE.                        ELGCOPAY
00719                                                                   ELGCOPAY
00720      IF ACCUM-FAM-OR-INDIV = ZEROS OR SPACES OR LOW-VALUES        ELGCOPAY
00721         SET FAM-OR-INDIV-NA TO TRUE.                              ELGCOPAY
00722                                                                   ELGCOPAY
00723      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELGCOPAY
00724         SET RELATIONSHIP-IND-NA TO TRUE.                          ELGCOPAY
00725                                                                   ELGCOPAY
00726      IF ACCUM-DED-BASE-AMT-SOURCE-IND =                           ELGCOPAY
00727                            ZEROS OR SPACES OR LOW-VALUES          ELGCOPAY
00728         SET DED-BASE-AMT-SOURCE-IND-NA TO TRUE.                   ELGCOPAY
00729                                                                   ELGCOPAY
00730      IF ACCUM-VALUE-QUALIFIER = ZEROS OR SPACES OR LOW-VALUES     ELGCOPAY
00731         SET VALUE-QUALIFIER-NA TO TRUE.                           ELGCOPAY
00732                                                                   ELGCOPAY
00733      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELGCOPAY
00734         SET AGE-LMT-TO-IND-NA TO TRUE.                            ELGCOPAY
00735                                                                   ELGCOPAY
00736      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELGCOPAY
00737         SET AGE-LMT-FROM-IND-NA TO TRUE.                          ELGCOPAY
00738                                                                   ELGCOPAY
00739      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELGCOPAY
00740         SET LMT-MANDATORY-IND-NA TO TRUE.                         ELGCOPAY
00741                                                                   ELGCOPAY
00742      IF ACCUM-CO-PAY-IND (COPAY-INDEX)                            ELGCOPAY
00743                     = ZEROS OR SPACES OR LOW-VALUES               ELGCOPAY
00744         SET CO-PAY-IND-NA (COPAY-INDEX) TO TRUE                   ELGCOPAY
00745                                                                   ELGCOPAY
00746      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELGCOPAY
00747         SET SERVICE-GROUP-NA TO TRUE.                             ELGCOPAY
00748                                                                   ELGCOPAY
00749      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELGCOPAY
00750         SET INTERNAL-DESCRIPTOR-NA TO TRUE.                       ELGCOPAY
00751                                                                   ELGCOPAY
00752      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELGCOPAY
00753         SET DAY-FACTOR-IND-NA TO TRUE.                            ELGCOPAY
00754                                                                   ELGCOPAY
00755      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELGCOPAY
00756         SET CLAIM-LVL-ACCUM-IND-NA TO TRUE.                       ELGCOPAY
00757                                                                   ELGCOPAY
00758      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELGCOPAY
00759         SET BEN-PER-MAX-OVRD-IND-NA TO TRUE.                      ELGCOPAY
00760                                                                   ELGCOPAY
00761      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELGCOPAY
00762         SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE.                       ELGCOPAY
00763                                                                   ELGCOPAY
00764 /***********************************************************      ELGCOPAY
00765 *                                                          *      ELGCOPAY
00766 *    LOAD VARIABLE FIELDS                                  *      ELGCOPAY
00767 *                                                          *      ELGCOPAY
00768 ************************************************************      ELGCOPAY
00769  0480-LOAD-VARIABLE-FIELDS.                                       ELGCOPAY
00770       MOVE GAF-COPAY-BENEFIT-PERIOD(GAF-INDEX) TO                 ELGCOPAY
00771          ACCUM-COPAY-BEN-PER(COPAY-INDEX).                        ELGCOPAY
00772       MOVE GAF-COPAY-DEFINITION(GAF-INDEX) TO                     ELGCOPAY
00773          ACCUM-COPAY-DEFINITION(COPAY-INDEX).                     ELGCOPAY
00774       MOVE GAF-COPAY-VALUE-QUALIFIER(GAF-INDEX) TO                ELGCOPAY
00775          ACCUM-COPAY-VALUE-QUALIFIER(COPAY-INDEX).                ELGCOPAY
00776       MOVE GAF-COPAY-VALUE-LIMIT(GAF-INDEX) TO                    ELGCOPAY
00777          ACCUM-COPAY-VALUE-LIMIT(COPAY-INDEX).                    ELGCOPAY
00778       MOVE GAF-COPAY-TIME-DOLLAR-IND(GAF-INDEX) TO                ELGCOPAY
00779          ACCUM-COPAY-TAD-IND(COPAY-INDEX).                        ELGCOPAY
00780       MOVE GAF-COPAY-CO-PAY-IND(GAF-INDEX) TO                     ELGCOPAY
00781            ACCUM-CO-PAY-IND(COPAY-INDEX).                         ELGCOPAY
00782       SET COPAY-INDEX UP BY 1.                                    ELGCOPAY
00783       ADD +1 TO ACCUM-COPAY-COUNT.                                ELGCOPAY
00784                                                                   ELGCOPAY
00785 /***********************************************************      ELGCOPAY
00786 *                                                          *      ELGCOPAY
00787 *        ADD ACCUM OCCURENCE TO FILE                       *      ELGCOPAY
00788 *                                                          *      ELGCOPAY
00789 ************************************************************      ELGCOPAY
00790  0498-COMPLETE-PAIR.                                              ELGCOPAY
00791       IF HOLD-DEFINITION = '0A'                                   ELGCOPAY
00792          PERFORM WITH TEST AFTER                                  ELGCOPAY
00793                VARYING GAF-INDEX FROM GAF-INDEX BY 1 UNTIL        ELGCOPAY
00794           GAF-COPAY-DEFINITION (GAF-INDEX) = 'AA'                 ELGCOPAY
00795              IF GAF-COPAY-DEFINITION (GAF-INDEX) = 'AA'           ELGCOPAY
00796                 PERFORM 0510-MATCH-REST-OF-OCCUR                  ELGCOPAY
00797                 IF WS-MATCH-FOUND                                 ELGCOPAY
00798                    PERFORM 0480-LOAD-VARIABLE-FIELDS              ELGCOPAY
00799                 END-IF                                            ELGCOPAY
00800              END-IF                                               ELGCOPAY
00801          END-PERFORM                                              ELGCOPAY
00802       END-IF.                                                     ELGCOPAY
00803      IF HOLD-DEFINITION = '0B'                                    ELGCOPAY
00804         PERFORM VARYING GAF-INDEX FROM GAF-INDEX BY 1 UNTIL       ELGCOPAY
00805          GAF-COPAY-DEFINITION (GAF-INDEX) = 'BB'                  ELGCOPAY
00806             IF GAF-COPAY-DEFINITION (GAF-INDEX) = 'BB'            ELGCOPAY
00807                PERFORM 0510-MATCH-REST-OF-OCCUR                   ELGCOPAY
00808                IF WS-MATCH-FOUND                                  ELGCOPAY
00809                   PERFORM 0480-LOAD-VARIABLE-FIELDS               ELGCOPAY
00810                END-IF                                             ELGCOPAY
00811             END-IF                                                ELGCOPAY
00812         END-PERFORM                                               ELGCOPAY
00813      END-IF.                                                      ELGCOPAY
00814      IF HOLD-DEFINITION  = '0F'                                   ELGCOPAY
00815         PERFORM VARYING GAF-INDEX FROM GAF-INDEX BY 1 UNTIL       ELGCOPAY
00816          GAF-COPAY-DEFINITION (GAF-INDEX) = 'FF'                  ELGCOPAY
00817             IF GAF-COPAY-DEFINITION (GAF-INDEX) = 'FF'            ELGCOPAY
00818                PERFORM 0510-MATCH-REST-OF-OCCUR                   ELGCOPAY
00819                IF WS-MATCH-FOUND                                  ELGCOPAY
00820                   PERFORM 0480-LOAD-VARIABLE-FIELDS               ELGCOPAY
00821                END-IF                                             ELGCOPAY
00822             END-IF                                                ELGCOPAY
00823         END-PERFORM                                               ELGCOPAY
00824      END-IF.                                                      ELGCOPAY
00825      IF WS-MATCH-FOUND                                            ELGCOPAY
00826         ADD 1 TO WS-OCCURRENCE-SUB                                ELGCOPAY
00827         SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)            ELGCOPAY
00828             TO TRUE                                               ELGCOPAY
00829      END-IF.                                                      ELGCOPAY
00830 /***********************************************************      ELGCOPAY
00831 *                                                          *      ELGCOPAY
00832 *    FIND RELATED PAIR                                     *      ELGCOPAY
00833 *                                                          *      ELGCOPAY
00834 ************************************************************      ELGCOPAY
00835  0500-FIND-RELATED-PAIR.                                          ELGCOPAY
00836      PERFORM WITH TEST AFTER VARYING GAF-INDEX FROM GAF-INDEX BY 1ELGCOPAY
00837         UNTIL (GAF-INDEX) > GAF-ENTRY-COUNT OR NOT WS-MATCH-FOUND ELGCOPAY
00838 *CHANGED FROM WS-HOLD-IDX TO - 1*****                             ELGCOPAY
00839            IF GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX - 1)       ELGCOPAY
00840            =  GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX)           ELGCOPAY
00841               INITIALIZE WS-MATCH-SW                              ELGCOPAY
00842               PERFORM 0510-MATCH-REST-OF-OCCUR                    ELGCOPAY
00843               IF WS-MATCH-FOUND                                   ELGCOPAY
00844                  SET WS-PAIR-FOUND TO TRUE                        ELGCOPAY
00845                  PERFORM 0480-LOAD-VARIABLE-FIELDS                ELGCOPAY
00846                   SET WS-OCCURRENCE-PROCESSED(WS-OCCURRENCE-SUB)  ELGCOPAY
00847                     TO TRUE                                       ELGCOPAY
00848                  ADD 1 TO WS-OCCURRENCE-SUB                       ELGCOPAY
00849 **                SET WS-HOLD-IDX UP BY 1                         ELGCOPAY
00850 **                SET COPAY-INDEX UP BY 1                         ELGCOPAY
00851                ELSE                                               ELGCOPAY
00852                   SET WS-STOP TO TRUE                             ELGCOPAY
00853                END-IF                                             ELGCOPAY
00854             ELSE                                                  ELGCOPAY
00855                SET WS-STOP TO TRUE                                ELGCOPAY
00856 **THIS ASSUMES THE ENTRIES WITH TAD INDICATORS ARE PAIRED BY      ELGCOPAY
00857 **THE SORT SO THEY ARE NEXT TO EACH OTHER.                        ELGCOPAY
00858             END-IF                                                ELGCOPAY
00859       END-PERFORM.                                                ELGCOPAY
00860                                                                   ELGCOPAY
00861 /***********************************************************      ELGCOPAY
00862 *                                                          *      ELGCOPAY
00863 *    MATCH REST OF PARIED OCCURANCE                        *      ELGCOPAY
00864 *                                                          *      ELGCOPAY
00865 ************************************************************      ELGCOPAY
00866  0510-MATCH-REST-OF-OCCUR.                                        ELGCOPAY
00867       IF GAF-COPAY-MANDATORY-IND (GAF-INDEX) =                    ELGCOPAY
00868          ACCUM-LMT-MANDATORY-IND                                  ELGCOPAY
00869 *        GAF-COPAY-MANDATORY-IND (WS-HOLD-IDX)                    ELGCOPAY
00870       AND                                                         ELGCOPAY
00871          GAF-COPAY-FAM-OR-INDIV  (GAF-INDEX) =                    ELGCOPAY
00872          ACCUM-FAM-OR-INDIV                                       ELGCOPAY
00873 *        GAF-COPAY-FAM-OR-INDIV  (WS-HOLD-IDX)                    ELGCOPAY
00874       AND                                                         ELGCOPAY
00875          GAF-COPAY-L-O-B (GAF-INDEX) =                            ELGCOPAY
00876          ACCUM-L-O-B                                              ELGCOPAY
00877 *          GAF-COPAY-L-O-B (GAF-INDEX - 1)                        ELGCOPAY
00878      AND                                                          ELGCOPAY
00879         GAF-COPAY-DAY-FACTOR-IND (GAF-INDEX) =                    ELGCOPAY
00880         ACCUM-DAY-FACTOR-IND                                      ELGCOPAY
00881 *       GAF-COPAY-DAY-FACTOR-IND (WS-HOLD-IDX)                    ELGCOPAY
00882      AND                                                          ELGCOPAY
00883        GAF-COPAY-SERVICE-GROUP (GAF-INDEX) =                      ELGCOPAY
00884        ACCUM-SERVICE-GROUP                                        ELGCOPAY
00885 *      GAF-COPAY-SERVICE-GROUP (WS-HOLD-IDX)                      ELGCOPAY
00886      AND                                                          ELGCOPAY
00887        GAF-COPAY-PLACE-OF-TREATMENT (GAF-INDEX) =                 ELGCOPAY
00888        ACCUM-PLACE-OF-TREATMENT                                   ELGCOPAY
00889 *      GAF-COPAY-PLACE-OF-TREATMENT (WS-HOLD-IDX)                 ELGCOPAY
00890      AND                                                          ELGCOPAY
00891        GAF-COPAY-CONDITION (GAF-INDEX) =                          ELGCOPAY
00892        ACCUM-CONDITION                                            ELGCOPAY
00893 *      GAF-COPAY-CONDITION (WS-HOLD-IDX)                          ELGCOPAY
00894      AND                                                          ELGCOPAY
00895        GAF-COPAY-CLAIM-LVL-ACCUM-IND (GAF-INDEX) =                ELGCOPAY
00896        ACCUM-CLAIM-LVL-ACCUM-IND                                  ELGCOPAY
00897      AND                                                          ELGCOPAY
00898        GAF-COPAY-COST-CONTAIN-IND (GAF-INDEX) =                   ELGCOPAY
00899        ACCUM-COST-CONTAIN-IND                                     ELGCOPAY
00900 *      GAF-COPAY-COST-CONTAIN-IND (WS-HOLD-IDX)                   ELGCOPAY
00901           SET WS-MATCH-FOUND TO TRUE                              ELGCOPAY
00902      END-IF.                                                      ELGCOPAY
00903                                                                   ELGCOPAY
00904 /***********************************************************      ELGCOPAY
00905 *                                                          *      ELGCOPAY
00906 *        ADD ACCUM OCCURENCE TO FILE                       *      ELGCOPAY
00907 *                                                          *      ELGCOPAY
00908 ************************************************************      ELGCOPAY
00909  0700-WRITE-EXTRACT-RECORD.                                       ELGCOPAY
00910 *->  EXECUTED BY 0300-TEST-ACP-OCCURENCE                          ELGCOPAY
00911      PERFORM 0800-EST-ADR-OF-TEMPORARY-FILE.                      ELGCOPAY
00912      SET  IOP-ADD TO TRUE.                                        ELGCOPAY
00913      SET  IOP-FCQ-NONE TO TRUE.                                   ELGCOPAY
00914      SET  IOP-KVQ-NONE TO TRUE.                                   ELGCOPAY
00915      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGCOPAY
00916                                                                   ELGCOPAY
00917 *->  ESTABLISH ADDRESSABILITY OF THE TABULAR FILE                 ELGCOPAY
00918      SET  CIA-GCTABULR-DDN TO TRUE.                               ELGCOPAY
00919      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOPAY
00920         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGCOPAY
00921      END-CALL.                                                    ELGCOPAY
00922      IF CIA-RC-PTR-NULL                                           ELGCOPAY
00923         PERFORM 0900-CIA-AB-UNALLOC-AREA.                         ELGCOPAY
00924                                                                   ELGCOPAY
00925 ************************************************************      ELGCOPAY
00926 *                                                          *      ELGCOPAY
00927 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELGCOPAY
00928 *                                                          *      ELGCOPAY
00929 ************************************************************      ELGCOPAY
00930  0800-EST-ADR-OF-TEMPORARY-FILE.                                  ELGCOPAY
00931 *-> EXECUTED BY 0240-SCAN-FOR-APPLIC-OCCRNCS                      ELGCOPAY
00932 *               0700-WRITE-EXTRACT-RECORD                         ELGCOPAY
00933      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGCOPAY
00934      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOPAY
00935         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGCOPAY
00936         END-CALL.                                                 ELGCOPAY
00937      IF CIA-RC-PTR-NULL                                           ELGCOPAY
00938         PERFORM 0900-CIA-AB-UNALLOC-AREA.                         ELGCOPAY
00939                                                                   ELGCOPAY
00940 ************************************************************      ELGCOPAY
00941 *                                                          *      ELGCOPAY
00942 *    CIA ABEND UNALLOCATED AREA                            *      ELGCOPAY
00943 *                                                          *      ELGCOPAY
00944 ************************************************************      ELGCOPAY
00945  0900-CIA-AB-UNALLOC-AREA.                                        ELGCOPAY
00946 *-> EXECUTED BY 0015-EST-ADR-OF-CNTRL-BLKS                        ELGCOPAY
00947 *               0030-EST-ADR-KEY-WK-AREA                          ELGCOPAY
00948 *               0045-EST-ADR-OF-SUBROUTINE-PAR                    ELGCOPAY
00949 *               0060-EST-ADR-BEN-PRVN-ACCUM                       ELGCOPAY
00950 *               0075-EST-ADRSABLTY-GRP-SPC                        ELGCOPAY
00951      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGCOPAY
00952      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELGCOPAY
00953                                                                   ELGCOPAY
