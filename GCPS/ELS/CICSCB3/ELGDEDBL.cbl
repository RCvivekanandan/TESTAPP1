00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGDEDBL
00003  PROGRAM-ID.        ELGDEDBL.                                        LV002
00004                                                                   ELGDEDBL
00005  AUTHOR.            GEORGE E. MOORE.                              ELGDEDBL
00006                                                                   ELGDEDBL
00007  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELGDEDBL
00008                     A MUTUAL LEGAL RESERVE COMPANY                ELGDEDBL
00009                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELGDEDBL
00010                     233 N. MICHIGAN AVE                           ELGDEDBL
00011                     CHICAGO, ILLINOIS 60601                       ELGDEDBL
00012                                                                   ELGDEDBL
00013  DATE-WRITTEN.      19-MAY-1992  (CLONED).                        ELGDEDBL
00014                                                                   ELGDEDBL
00015  DATE-COMPILED.                                                   ELGDEDBL
00016                                                                   ELGDEDBL
00017  SECURITY.          COPYRIGHT 1986, 1992,                         ELGDEDBL
00018                     HEALTH CARE SERVICE CORPORATION               ELGDEDBL
00019                                                                   ELGDEDBL
00020  ENVIRONMENT DIVISION.                                            ELGDEDBL
00021                                                                   ELGDEDBL
00022  CONFIGURATION SECTION.                                           ELGDEDBL
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELGDEDBL
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELGDEDBL
00025                                                                   ELGDEDBL
00026 /*****************************************************************ELGDEDBL
00027 *                                                                *ELGDEDBL
00028 *  ELGDEDBL:    SELECTS #ADL (DEDUCTIBLE) ACCUMS FROM THE        *ELGDEDBL
00029 *               BENEFIT PROVISION LEVEL, PROCESS THE ACCUM,      *ELGDEDBL
00030 *               AND LINK TO THE DEDUCTIBLE GENERATOR MODULE      *ELGDEDBL
00031 *               TO DISPLAY THE ACCUM INFORMATION.                *ELGDEDBL
00032 *                                                                *ELGDEDBL
00033 ******************************************************************ELGDEDBL
00034 *                                                                *ELGDEDBL
00035 *                      MAINTENANCE HISTORY                       *ELGDEDBL
00036 *                                                                *ELGDEDBL
00037 *  MOD     DATE     BY  DRPT                ACTION               *ELGDEDBL
00038 * ----- ----------- --- ----- ---------------------------------- *ELGDEDBL
00039 * 01.00 19-MAY-1992 GEM       CLONED FROM ELGMAXIM.              *ELGDEDBL
00040 *                                                                *ELGDEDBL
00041 * 02.00 02-DEC-1998 AKK       ADDED SUPPORT FOR ACP.             *ELGDEDBL
00042 *                                                                *ELGDEDBL
00043 * 02.01 25-AUG-2000 AKK       ADDED SUPPORT FOR #IPGS            *ELGDEDBL
00044 *                                                                *ELGDEDBL
00045 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGDEDBL
00046 *                                                                *ELGDEDBL
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00046 *                                                                *ELGDEDBL
00047 ******************************************************************ELGDEDBL
00048      TITLE  'ELGDEDBL          WORKING STORAGE'.                  ELGDEDBL
00049  DATA DIVISION.                                                   ELGDEDBL
00050                                                                   ELGDEDBL
00051  WORKING-STORAGE SECTION.                                         ELGDEDBL
00052                                                                   ELGDEDBL
00053  01  SWITCHES.                                                    ELGDEDBL
00054      02                                      PICTURE  X(01).      ELGDEDBL
00055         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELGDEDBL
00056         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELGDEDBL
00057      02                                      PICTURE  X(01).      ELGDEDBL
00058         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELGDEDBL
00059         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELGDEDBL
00060      02                                      PICTURE  X(01).      ELGDEDBL
00061         88 SW-HAS-IBGR                       VALUE 'Y'.           ELGDEDBL
00062         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELGDEDBL
00063      02                                      PICTURE  X(01).      ELGDEDBL
00064         88 SW-HAS-IDGD                       VALUE 'Y'.           ELGDEDBL
00065         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELGDEDBL
00066      02                                      PICTURE  X(01).      ELGDEDBL
00067         88 SW-HAS-IPGN                       VALUE 'Y'.           ELGDEDBL
00068         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELGDEDBL
00069      02                                      PICTURE  X(01).      ELGDEDBL
00070         88 SW-HAS-IPGP                       VALUE 'Y'.           ELGDEDBL
00071         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELGDEDBL
00072      02                                      PICTURE  X(01).      ELGDEDBL
00073         88 SW-HAS-IPGT                       VALUE 'Y'.           ELGDEDBL
00074         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELGDEDBL
00075      02                                      PICTURE  X(01).      ELGDEDBL
00076         88 SW-HAS-IPGS                       VALUE 'Y'.           ELGDEDBL
00077         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELGDEDBL
00078      02                                      PICTURE  X(01).      ELGDEDBL
00079         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELGDEDBL
00080         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELGDEDBL
00081      02                                      PICTURE  X(01).      ELGDEDBL
00082         88 SW-INTRNL-INST-PROV-CLASS         VALUE 'Y'.           ELGDEDBL
00083         88 SW-INTRNL-NOT-INST-PROV-CLASS     VALUE 'N'.           ELGDEDBL
00084         88 SW-INTRNL-INST-PROV-CLS-NOT-DT    VALUE 'X'.           ELGDEDBL
00085      02                                      PICTURE  X(01).      ELGDEDBL
00086         88 SW-INTRNL-PROF-PROV-CLASS         VALUE 'Y'.           ELGDEDBL
00087         88 SW-INTRNL-NOT-PROF-PROV-CLASS     VALUE 'N'.           ELGDEDBL
00088         88 SW-INTRNL-PROF-PROV-CLS-NOT-DT    VALUE 'X'.           ELGDEDBL
00089      02                                      PICTURE  X(01).      ELGDEDBL
00090         88 SW-INTRNL-PROF-PROV-SPEC          VALUE 'Y'.           ELGDEDBL
00091         88 SW-INTRNL-NOT-PROF-PROV-SPEC      VALUE 'N'.           ELGDEDBL
00092         88 SW-INTRNL-PROF-PROV-SPC-NOT-DT    VALUE 'X'.           ELGDEDBL
00093      02                                      PICTURE  X(01).      ELGDEDBL
00094         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELGDEDBL
00095         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELGDEDBL
00096                                                                   ELGDEDBL
00097  01  WS-PROVISION-ARGUMENT.                                       ELGDEDBL
00098      02                                      PICTURE X(05).       ELGDEDBL
00099      02 WS-PROVISION-CL                      PICTURE X(01).       ELGDEDBL
00100         88  INST-CLASS                       VALUE 'A', 'B', 'W'. ELGDEDBL
00101         88  PROF-CLASS                       VALUE 'C', 'D', 'E'. ELGDEDBL
00102                                                                   ELGDEDBL
00103  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELGDEDBL
00104      88 WS-LOB-INST              VALUE '1'.                       ELGDEDBL
00105      88 WS-LOB-PROF              VALUE '2'.                       ELGDEDBL
00106      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELGDEDBL
00107      88 WS-LOB-BOTH              VALUE '3', '4', '5', '6', '7'.   ELGDEDBL
00108                                                                   ELGDEDBL
00109  01  PROGRAM-CONSTANTS.                                           ELGDEDBL
00110      02 PC-ADL                   PICTURE  X(06) VALUE '#ADL  '.   ELGDEDBL
00111      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELGDEDBL
00112      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELGDEDBL
00113      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELGDEDBL
00114      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELGDEDBL
00115      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELGDEDBL
00116      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGS '.   ELGDEDBL
00117      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELGDEDBL
00118                                                                   ELGDEDBL
00119  01  WS-WORK-FIELDS.                                              ELGDEDBL
00120      02 WS-ADL-SUB               PICTURE S9(04) COMP.             ELGDEDBL
00121      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELGDEDBL
00122      02 WS-DED-GAC-IDX           INDEX.                           ELGDEDBL
00123                                                                   ELGDEDBL
00124  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELGDEDBL
00125      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGDEDBL
00126      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGDEDBL
00127      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGDEDBL
00128      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGDEDBL
00129      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGDEDBL
00130      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGDEDBL
00131 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELGDEDBL
00132      COPY ELSCFTB2.                                               ELGDEDBL
00133                                                                   ELGDEDBL
00134 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELGDEDBL
00135      COPY ELSCFTB9.                                               ELGDEDBL
00136                                                                   ELGDEDBL
00137      TITLE  'ELGDEDBL          LINKAGE SECTION'                   ELGDEDBL
00138  LINKAGE SECTION.                                                 ELGDEDBL
00139  01  DFHCOMMAREA.                                                 ELGDEDBL
00140      COPY ELSCOMMC.                                               ELGDEDBL
00141 /                                                                 ELGDEDBL
00142      COPY ELSCIA2C.                                               ELGDEDBL
00143 /                                                                 ELGDEDBL
00144      COPY ELSIOPMC.                                               ELGDEDBL
00145 /                                                                 ELGDEDBL
00146      COPY ELSKEYSC.                                               ELGDEDBL
00147 /                                                                 ELGDEDBL
00148      COPY ELSSRTPC.                                               ELGDEDBL
00149 /                                                                 ELGDEDBL
00150      COPY ELSSSCBC.                                               ELGDEDBL
00151 /                                                                 ELGDEDBL
00152  01  GAC-RECORD-AREA.                                             ELGDEDBL
00153      COPY GCTADLC.                                                ELGDEDBL
00154 /                                                                 ELGDEDBL
00155  01 GCG-GRP-SPEC-RECORD-AREA.                                     ELGDEDBL
00156      COPY GCGROUPC.                                               ELGDEDBL
00157 /                                                                 ELGDEDBL
00158      COPY ELSACUMC.                                               ELGDEDBL
00159      TITLE  'ELGDEDBL  PROCEDURE DIVISION'.                       ELGDEDBL
00160  PROCEDURE DIVISION.                                              ELGDEDBL
00161 ************************************************************      ELGDEDBL
00162 *                                                          *      ELGDEDBL
00163 *    E L G D E D B L    M A I N L I N E                    *      ELGDEDBL
00164 *                                                          *      ELGDEDBL
00165 ************************************************************      ELGDEDBL
00166                                                                   ELGDEDBL
00167  0000-MANILINE.                                                   ELGDEDBL
00168      PERFORM 0010-INITIALIZATION.                                 ELGDEDBL
00169      PERFORM 0100-PROCESS.                                        ELGDEDBL
00170      GOBACK.                                                      ELGDEDBL
00171                                                                   ELGDEDBL
00172 ************************************************************      ELGDEDBL
00173 *                                                          *      ELGDEDBL
00174 *    I N I T I A L I Z A T I O N                           *      ELGDEDBL
00175 *                                                          *      ELGDEDBL
00176 ************************************************************      ELGDEDBL
00177  0010-INITIALIZATION.                                             ELGDEDBL
00178      PERFORM 0015-EST-ADR-OF-CNTRL-BLKS.                          ELGDEDBL
00179      PERFORM 0030-EST-ADR-KEY-WK-AREA.                            ELGDEDBL
00180      PERFORM 0045-EST-ADR-OF-SUBROUTINE-PAR.                      ELGDEDBL
00181      PERFORM 0060-EST-ADR-BEN-PRVN-ACCUM.                         ELGDEDBL
00182      PERFORM 0075-EST-ADRSABLTY-GRP-SPC.                          ELGDEDBL
00183      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGDEDBL
00184                                                                   ELGDEDBL
00185 ************************************************************      ELGDEDBL
00186 *                                                          *      ELGDEDBL
00187 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGDEDBL
00188 *                                                          *      ELGDEDBL
00189 ************************************************************      ELGDEDBL
00190  0015-EST-ADR-OF-CNTRL-BLKS.                                      ELGDEDBL
00191 *->  EXECUTED BY 0010-INITIALIZATION                              ELGDEDBL
00192      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGDEDBL
00193         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGDEDBL
00194      ELSE                                                         ELGDEDBL
00195         IF ECA-CIA-PTR = NULL                                     ELGDEDBL
00196            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGDEDBL
00197         ELSE                                                      ELGDEDBL
00198            CALL 'ELUINISM' USING DFHCOMMAREA                      ELGDEDBL
00199               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELGDEDBL
00200                  END-CALL                                         ELGDEDBL
00201            SET CIA-ELSSSCB-DDN TO TRUE                            ELGDEDBL
00202            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELGDEDBL
00203               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELGDEDBL
00204                  END-CALL                                         ELGDEDBL
00205            IF CIA-RC-PTR-NULL                                     ELGDEDBL
00206               PERFORM 0900-CIA-AB-UNALLOC-AREA                    ELGDEDBL
00207            ELSE                                                   ELGDEDBL
00208               CONTINUE                                            ELGDEDBL
00209            END-IF                                                 ELGDEDBL
00210         END-IF                                                    ELGDEDBL
00211      END-IF.                                                      ELGDEDBL
00212                                                                   ELGDEDBL
00213 ************************************************************      ELGDEDBL
00214 *                                                          *      ELGDEDBL
00215 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELGDEDBL
00216 *                                                          *      ELGDEDBL
00217 ************************************************************      ELGDEDBL
00218  0030-EST-ADR-KEY-WK-AREA.                                        ELGDEDBL
00219 *->  EXECUTED BY 0010-INITIALIZATION                              ELGDEDBL
00220      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGDEDBL
00221      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGDEDBL
00222         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELGDEDBL
00223            END-CALL.                                              ELGDEDBL
00224      IF CIA-RC-PTR-NULL                                           ELGDEDBL
00225         PERFORM 0900-CIA-AB-UNALLOC-AREA.                         ELGDEDBL
00226                                                                   ELGDEDBL
00227 ************************************************************      ELGDEDBL
00228 *                                                          *      ELGDEDBL
00229 *    ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS     *      ELGDEDBL
00230 *                                                          *      ELGDEDBL
00231 ************************************************************      ELGDEDBL
00232  0045-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGDEDBL
00233 *->  EXECUTED BY 0010-INITIALIZATION                              ELGDEDBL
00234      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGDEDBL
00235      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGDEDBL
00236         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELGDEDBL
00237            END-CALL.                                              ELGDEDBL
00238      IF CIA-RC-PTR-NULL                                           ELGDEDBL
00239         PERFORM 0900-CIA-AB-UNALLOC-AREA.                         ELGDEDBL
00240                                                                   ELGDEDBL
00241 ************************************************************      ELGDEDBL
00242 *                                                          *      ELGDEDBL
00243 *    ESTABLISH ADDRESSABILITY OF BENEFIT PROVISION LEVEL   *      ELGDEDBL
00244 *    ACCUMULATOR RECORD                                    *      ELGDEDBL
00245 *                                                          *      ELGDEDBL
00246 ************************************************************      ELGDEDBL
00247  0060-EST-ADR-BEN-PRVN-ACCUM.                                     ELGDEDBL
00248 *->  EXECUTED BY 0010-INITIALIZATION                              ELGDEDBL
00249      SET CIA-GCTABULR-DDN TO TRUE.                                ELGDEDBL
00250      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGDEDBL
00251         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGDEDBL
00252            END-CALL.                                              ELGDEDBL
00253      IF CIA-RC-PTR-NULL                                           ELGDEDBL
00254         PERFORM 0900-CIA-AB-UNALLOC-AREA                          ELGDEDBL
00255      ELSE                                                         ELGDEDBL
00256         IF IOP-REC-PTR = NULLS                                    ELGDEDBL
00257            SET CIA-AB-UNALLOC-AREA TO TRUE                        ELGDEDBL
00258            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELGDEDBL
00259         ELSE                                                      ELGDEDBL
00260            SET ADDRESS OF GAC-RECORD-AREA TO IOP-REC-PTR          ELGDEDBL
00261         END-IF                                                    ELGDEDBL
00262      END-IF.                                                      ELGDEDBL
00263                                                                   ELGDEDBL
00264 ************************************************************      ELGDEDBL
00265 *                                                          *      ELGDEDBL
00266 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD     *      ELGDEDBL
00267 *                                                          *      ELGDEDBL
00268 ************************************************************      ELGDEDBL
00269 *->  EXECUTED BY 0010-INITIALIZATION                              ELGDEDBL
00270  0075-EST-ADRSABLTY-GRP-SPC.                                      ELGDEDBL
00271      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELGDEDBL
00272      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGDEDBL
00273         ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                       ELGDEDBL
00274            END-CALL.                                              ELGDEDBL
00275      IF CIA-RC-PTR-NULL                                           ELGDEDBL
00276         PERFORM 0900-CIA-AB-UNALLOC-AREA.                         ELGDEDBL
00277                                                                   ELGDEDBL
00278 /***********************************************************      ELGDEDBL
00279 *                                                          *      ELGDEDBL
00280 *        PROCESS                                           *      ELGDEDBL
00281 *                                                          *      ELGDEDBL
00282 ************************************************************      ELGDEDBL
00283  0100-PROCESS.                                                    ELGDEDBL
00284 *->  EXECUTED BY 0000-MANILINE                                    ELGDEDBL
00285      PERFORM 0240-SCAN-FOR-APPLIC-OCCRNCS.                        ELGDEDBL
00286      IF SW-NO-APPLIC-ACCUM-FOUND                                  ELGDEDBL
00287         EVALUATE TRUE                                             ELGDEDBL
00288            WHEN SSB-PROV-CLASS-INST                               ELGDEDBL
00289               SET SRP-INST-NOT-APPLICABLE TO TRUE                 ELGDEDBL
00290            WHEN SSB-PROV-CLASS-PROF                               ELGDEDBL
00291               SET SRP-PROF-NOT-APPLICABLE TO TRUE                 ELGDEDBL
00292            WHEN SSB-PROV-CLASS-BOTH                               ELGDEDBL
00293               SET SRP-NO-ACCUMS-FOUND TO TRUE                     ELGDEDBL
00294            WHEN OTHER                                             ELGDEDBL
00295               SET CIA-AB-PGM-LOGIC TO TRUE                        ELGDEDBL
00296               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGDEDBL
00297         END-EVALUATE                                              ELGDEDBL
00298      END-IF.                                                      ELGDEDBL
00299                                                                   ELGDEDBL
00300      SET SRP-TOPIC-ACCUM TO TRUE.                                 ELGDEDBL
00301                                                                   ELGDEDBL
00302 *->  LINK TO THE OUTPUT GENERATOR                                 ELGDEDBL
00303      EXEC CICS LINK  PROGRAM ('ELGADL')                           ELGDEDBL
00304                      COMMAREA (DFHCOMMAREA)   END-EXEC.           ELGDEDBL
00305                                                                   ELGDEDBL
00306 ************************************************************      ELGDEDBL
00307 *                                                          *      ELGDEDBL
00308 *    SCAN ADL ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELGDEDBL
00309 *                                                          *      ELGDEDBL
00310 ************************************************************      ELGDEDBL
00311  0240-SCAN-FOR-APPLIC-OCCRNCS.                                    ELGDEDBL
00312 *->  EXECUTED BY 0100-PROCESS                                     ELGDEDBL
00313      PERFORM 0800-EST-ADR-OF-TEMPORARY-FILE.                      ELGDEDBL
00314      PERFORM 0250-DELETE-ADL-SUMMARY-FILE.                        ELGDEDBL
00315      PERFORM 0260-ALLOC-WORKFILE-REC-AREA.                        ELGDEDBL
00316                                                                   ELGDEDBL
00317 *->  SET UPPER LIMIT ON TABULAR SCAN                              ELGDEDBL
00318      SET GAC-INDEX TO GAC-ENTRY-COUNT.                            ELGDEDBL
00319      SET WS-DED-GAC-IDX TO GAC-INDEX.                             ELGDEDBL
00320 *->  SCAN ACCUMULATOR TABULAR                                     ELGDEDBL
00321      PERFORM 0300-TEST-ADL-OCCURENCE                              ELGDEDBL
00322         VARYING GAC-INDEX FROM 1 BY 1                             ELGDEDBL
00323            UNTIL GAC-INDEX = WS-DED-GAC-IDX.                      ELGDEDBL
00324                                                                   ELGDEDBL
00325 ************************************************************      ELGDEDBL
00326 *                                                          *      ELGDEDBL
00327 *        DELETE ADL SUMMARY FILE                           *      ELGDEDBL
00328 *                                                          *      ELGDEDBL
00329 ************************************************************      ELGDEDBL
00330  0250-DELETE-ADL-SUMMARY-FILE.                                    ELGDEDBL
00331 *->  EXECUTED BY 0240-SCAN-FOR-APPLIC-OCCRNCS                     ELGDEDBL
00332      SET IOP-DEL TO TRUE.                                         ELGDEDBL
00333      SET IOP-FCQ-NONE TO TRUE.                                    ELGDEDBL
00334      SET IOP-KVQ-NONE TO TRUE.                                    ELGDEDBL
00335      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGDEDBL
00336                                                                   ELGDEDBL
00337 ************************************************************      ELGDEDBL
00338 *                                                          *      ELGDEDBL
00339 *    ALLOCATE WORKFILE RECORD AREA                         *      ELGDEDBL
00340 *                                                          *      ELGDEDBL
00341 ************************************************************      ELGDEDBL
00342  0260-ALLOC-WORKFILE-REC-AREA.                                    ELGDEDBL
00343 *->  EXECUTED BY 0240-SCAN-FOR-APPLIC-OCCRNCS                     ELGDEDBL
00344      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGDEDBL
00345      SET CIA-STG-GETMAIN TO TRUE.                                 ELGDEDBL
00346      SET IOP-GETMAIN-REC TO TRUE.                                 ELGDEDBL
00347      COMPUTE IOP-MAX-REC-LEN =                                    ELGDEDBL
00348              LENGTH OF ACCUM-FIXED-AREA +                         ELGDEDBL
00349 *            LENGTH OF ACCUM-ASCEND-DESCEND-COUNT +               ELGDEDBL
00350              LENGTH OF ACCUM-VARIABLE-AREA       +                ELGDEDBL
00351              LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELGDEDBL
00352 *           (PC-MAXIMUM-NBR-OCCURS *                              ELGDEDBL
00353 *            LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).              ELGDEDBL
00354                                                                   ELGDEDBL
00355      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGDEDBL
00356      IF IOP-REC-PTR = NULLS                                       ELGDEDBL
00357         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGDEDBL
00358         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGDEDBL
00359      ELSE                                                         ELGDEDBL
00360         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELGDEDBL
00361      END-IF.                                                      ELGDEDBL
00362                                                                   ELGDEDBL
00363 /***********************************************************      ELGDEDBL
00364 *                                                          *      ELGDEDBL
00365 *        TEST ADL OCCURS                                   *      ELGDEDBL
00366 *                                                          *      ELGDEDBL
00367 ************************************************************      ELGDEDBL
00368  0300-TEST-ADL-OCCURENCE.                                         ELGDEDBL
00369 *->  EXECUTED BY 0240-SCAN-FOR-APPLIC-OCCRNCS                     ELGDEDBL
00370      MOVE GAC-DEDL-L-O-B (GAC-INDEX) TO WS-LOB-ACCUM-OCCRNC.      ELGDEDBL
00371      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELGDEDBL
00372      SET SW-HAS-NO-IBGR                                           ELGDEDBL
00373          SW-HAS-NO-IDGD                                           ELGDEDBL
00374          SW-HAS-NO-IPGN                                           ELGDEDBL
00375          SW-HAS-NO-IPGP                                           ELGDEDBL
00376          SW-HAS-NO-IPGS                                           ELGDEDBL
00377          SW-HAS-NO-IPGT TO TRUE.                                  ELGDEDBL
00378                                                                   ELGDEDBL
00379      INITIALIZE WS-IBGR-SLOT-NBR                                  ELGDEDBL
00380                 WS-IDGD-SLOT-NBR                                  ELGDEDBL
00381                 WS-IPGN-SLOT-NBR                                  ELGDEDBL
00382                 WS-IPGP-SLOT-NBR                                  ELGDEDBL
00383                 WS-IPGT-SLOT-NBR                                  ELGDEDBL
00384                 WS-IPGS-SLOT-NBR.                                 ELGDEDBL
00385      SET SW-INTRNL-INST-PROV-CLS-NOT-DT                           ELGDEDBL
00386          SW-INTRNL-PROF-PROV-CLS-NOT-DT                           ELGDEDBL
00387          SW-INTRNL-PROF-PROV-SPC-NOT-DT TO TRUE.                  ELGDEDBL
00388                                                                   ELGDEDBL
00389 *->  SCAN FOR INTERNAL TABULARS                                   ELGDEDBL
00390 *    (THIS IS DONE NOW IN CASE IPGT IS NEEDED TO DETERMINE        ELGDEDBL
00391 *     WHETHER OCCURRENCE IS INSTITUTIONAL OR PROFESSIONAL.        ELGDEDBL
00392 *     THE IBGR IS IGNORED, SINCE THE ACCUMULATOR IS DIRECTLY      ELGDEDBL
00393 *     ATTACHED TO THE PROVISION.)                                 ELGDEDBL
00394                                                                   ELGDEDBL
00395      PERFORM 0310-SCAN-THE-INTERNAL-TABULAR                       ELGDEDBL
00396         VARYING GAC-INT-INDEX FROM 1 BY 1                         ELGDEDBL
00397           UNTIL GAC-INT-INDEX  >=                                 ELGDEDBL
00398                 GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX).           ELGDEDBL
00399                                                                   ELGDEDBL
00400      EVALUATE TRUE ALSO TRUE                                      ELGDEDBL
00401         WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                        ELGDEDBL
00402            SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE                  ELGDEDBL
00403            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGDEDBL
00404            SET SW-OCCRNC-APPLIES TO TRUE                          ELGDEDBL
00405         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH                 ELGDEDBL
00406            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGDEDBL
00407            SET SW-OCCRNC-APPLIES TO TRUE                          ELGDEDBL
00408         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST                 ELGDEDBL
00409            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGDEDBL
00410            SET SW-OCCRNC-APPLIES TO TRUE                          ELGDEDBL
00411         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH                 ELGDEDBL
00412            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGDEDBL
00413            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGDEDBL
00414            SET SW-OCCRNC-APPLIES TO TRUE                          ELGDEDBL
00415         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF                 ELGDEDBL
00416            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGDEDBL
00417            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGDEDBL
00418            SET SW-OCCRNC-APPLIES TO TRUE                          ELGDEDBL
00419         WHEN OTHER                                                ELGDEDBL
00420            CONTINUE                                               ELGDEDBL
00421      END-EVALUATE.                                                ELGDEDBL
00422                                                                   ELGDEDBL
00423 *->  SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD               ELGDEDBL
00424      IF SW-OCCRNC-APPLIES                                         ELGDEDBL
00425         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELGDEDBL
00426         PERFORM 0320-INIT-ACCUM-EXTRACT                           ELGDEDBL
00427         PERFORM 0330-EXTRACT-ACCUM                                ELGDEDBL
00428         PERFORM 0410-CHK-EXTRACT-DATA-INTGRTY                     ELGDEDBL
00429         PERFORM 0700-WRITE-EXTRACT-RECORD                         ELGDEDBL
00430      END-IF.                                                      ELGDEDBL
00431                                                                   ELGDEDBL
00432 /***********************************************************      ELGDEDBL
00433 *                                                          *      ELGDEDBL
00434 *        SCAN THE INTERNAL TABULARS                        *      ELGDEDBL
00435 *                                                          *      ELGDEDBL
00436 ************************************************************      ELGDEDBL
00437  0310-SCAN-THE-INTERNAL-TABULAR.                                  ELGDEDBL
00438 *->  EXECUTED BY 0300-TEST-ADL-OCCURENCE                          ELGDEDBL
00439      IF GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX) > 0               ELGDEDBL
00440         MOVE GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX)              ELGDEDBL
00441           TO WS-SLOT-NBR                                          ELGDEDBL
00442         EVALUATE GAC-INT-ID (GAC-INDEX, GAC-INT-INDEX)            ELGDEDBL
00443            WHEN PC-IBGR                                           ELGDEDBL
00444               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELGDEDBL
00445               SET SW-HAS-IBGR                                     ELGDEDBL
00446                TO TRUE                                            ELGDEDBL
00447            WHEN PC-IDGD                                           ELGDEDBL
00448               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELGDEDBL
00449               SET SW-HAS-IDGD                                     ELGDEDBL
00450                TO TRUE                                            ELGDEDBL
00451            WHEN PC-IPGP                                           ELGDEDBL
00452               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELGDEDBL
00453               SET SW-HAS-IPGP                                     ELGDEDBL
00454                TO TRUE                                            ELGDEDBL
00455            WHEN PC-IPGN                                           ELGDEDBL
00456               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELGDEDBL
00457               SET SW-HAS-IPGN                                     ELGDEDBL
00458                TO TRUE                                            ELGDEDBL
00459            WHEN PC-IPGT                                           ELGDEDBL
00460               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELGDEDBL
00461               SET SW-HAS-IPGT                                     ELGDEDBL
00462                TO TRUE                                            ELGDEDBL
00463            WHEN PC-IPGS                                           ELGDEDBL
00464               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELGDEDBL
00465               SET SW-HAS-IPGS                                     ELGDEDBL
00466                TO TRUE                                            ELGDEDBL
00467            WHEN OTHER                                             ELGDEDBL
00468                 CONTINUE                                          ELGDEDBL
00469         END-EVALUATE                                              ELGDEDBL
00470      END-IF.                                                      ELGDEDBL
00471                                                                   ELGDEDBL
00472 ************************************************************      ELGDEDBL
00473 *                                                          *      ELGDEDBL
00474 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELGDEDBL
00475 *                                                          *      ELGDEDBL
00476 ************************************************************      ELGDEDBL
00477  0320-INIT-ACCUM-EXTRACT.                                         ELGDEDBL
00478 *->  EXECUTED BY 0300-TEST-ADL-OCCURENCE                          ELGDEDBL
00479      INITIALIZE ACCUM-FIXED-AREA.                                 ELGDEDBL
00480      SET ACCUM-ADL TO TRUE.                                       ELGDEDBL
00481      MOVE 1 TO  ACCUM-ASCEND-DESCEND-COUNT.                       ELGDEDBL
00482      SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELGDEDBL
00483      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELGDEDBL
00484      INITIALIZE ACCUM-COPAY-ENTRY (1).                            ELGDEDBL
00485                                                                   ELGDEDBL
00486 /***********************************************************      ELGDEDBL
00487 *                                                          *      ELGDEDBL
00488 *        SUMMARIZE ADL TOPIC LEVEL DATA ELEMENTS           *      ELGDEDBL
00489 *                                                          *      ELGDEDBL
00490 ************************************************************      ELGDEDBL
00491  0330-EXTRACT-ACCUM.                                              ELGDEDBL
00492 *->  EXECUTED BY 0300-TEST-ADL-OCCURENCE                          ELGDEDBL
00493 *    SET FIXED PORTION DATA ELEMENTS                              ELGDEDBL
00494      MOVE GAC-DEDL-MANDATORY-IND (GAC-INDEX)                      ELGDEDBL
00495        TO ACCUM-LMT-MANDATORY-IND.                                ELGDEDBL
00496      MOVE GAC-DEDL-FYI-VALUE (GAC-INDEX) TO ACCUM-FYI-VALUE.      ELGDEDBL
00497      MOVE GAC-DEDL-COST-CONTAIN-IND (GAC-INDEX)                   ELGDEDBL
00498        TO ACCUM-COST-CONTAIN-IND.                                 ELGDEDBL
00499      MOVE GAC-DEDL-PLACE-OF-TREATMENT (GAC-INDEX)                 ELGDEDBL
00500        TO ACCUM-PLACE-OF-TREATMENT.                               ELGDEDBL
00501      MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)                     ELGDEDBL
00502        TO ACCUM-BENEFIT-PERIOD.                                   ELGDEDBL
00503      MOVE GAC-DEDL-BEN-PER-TIME-FCTR (GAC-INDEX)                  ELGDEDBL
00504        TO ACCUM-BEN-PER-TIME-FCTR.                                ELGDEDBL
00505      MOVE GAC-DEDL-BEN-PER-TIME-QUAL (GAC-INDEX)                  ELGDEDBL
00506        TO ACCUM-BEN-PER-TIME-QUAL.                                ELGDEDBL
00507      MOVE GAC-DEDL-INTERVAL-TIME-FCTR (GAC-INDEX)                 ELGDEDBL
00508        TO ACCUM-INTERVAL-TIME-FCTR.                               ELGDEDBL
00509      MOVE GAC-DEDL-INTERVAL-TYPE (GAC-INDEX)                      ELGDEDBL
00510        TO ACCUM-INTERVAL-TYPE.                                    ELGDEDBL
00511      MOVE GAC-DEDL-INTERVAL-OVRD-IND (GAC-INDEX)                  ELGDEDBL
00512        TO ACCUM-INTERVAL-OVRD-IND.                                ELGDEDBL
00513      MOVE GAC-DEDL-INTERVAL-OVRD-VALUE (GAC-INDEX)                ELGDEDBL
00514        TO ACCUM-INTERVAL-OVRD-VALUE.                              ELGDEDBL
00515      MOVE GAC-DEDL-L-O-B (GAC-INDEX) TO ACCUM-L-O-B.              ELGDEDBL
00516      MOVE GAC-DEDL-DEFINITION (GAC-INDEX) TO ACCUM-DEFINITION.    ELGDEDBL
00517      SET CARRY-OVER-CREDIT-IND-NA                                 ELGDEDBL
00518             ASCEND-DESCEND-IND-NA TO TRUE.                        ELGDEDBL
00519      MOVE GAC-DEDL-CONDITION (GAC-INDEX) TO  ACCUM-CONDITION.     ELGDEDBL
00520      MOVE GAC-DEDL-FAM-OR-INDIV (GAC-INDEX)                       ELGDEDBL
00521        TO ACCUM-FAM-OR-INDIV.                                     ELGDEDBL
00522      MOVE ZEROS TO ACCUM-MAX-BASE-AMT-SOURCE-IND.                 ELGDEDBL
00523      MOVE ZEROS TO ACCUM-OPX-BASE-AMT-SOURCE-IND.                 ELGDEDBL
00524      MOVE GCG-DED-BASE-AMT-SOURCE-IND                             ELGDEDBL
00525        TO ACCUM-DED-BASE-AMT-SOURCE-IND.                          ELGDEDBL
00526      MOVE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)                    ELGDEDBL
00527        TO ACCUM-VALUE-QUALIFIER.                                  ELGDEDBL
00528      MOVE GAC-DEDL-RELATIONSHIP-IND (GAC-INDEX)                   ELGDEDBL
00529        TO ACCUM-RELATIONSHIP-IND.                                 ELGDEDBL
00530      MOVE GAC-CARRY-OVER-CREDIT-IND (GAC-INDEX)                   ELGDEDBL
00531        TO ACCUM-CARRY-OVER-CREDIT-IND.                            ELGDEDBL
00532      MOVE GAC-DEDL-AGE-LIMIT-FROM (GAC-INDEX)                     ELGDEDBL
00533        TO ACCUM-AGE-LIMIT-FROM-VAL.                               ELGDEDBL
00534      MOVE GAC-DEDL-AGE-QUAL-IND-FROM (GAC-INDEX)                  ELGDEDBL
00535        TO ACCUM-AGE-LIMIT-FROM-IND.                               ELGDEDBL
00536      MOVE GAC-DEDL-AGE-LIMIT-TO (GAC-INDEX)                       ELGDEDBL
00537        TO ACCUM-AGE-LIMIT-TO-VAL.                                 ELGDEDBL
00538      MOVE GAC-DEDL-AGE-QUAL-IND-TO (GAC-INDEX)                    ELGDEDBL
00539        TO ACCUM-AGE-LIMIT-TO-IND.                                 ELGDEDBL
00540      SET LMT-MANDATORY-IND-NA TO TRUE.                            ELGDEDBL
00541      MOVE GAC-DEDL-CO-PAY-IND (GAC-INDEX)                         ELGDEDBL
00542        TO ACCUM-CO-PAY-IND (COPAY-INDEX)                          ELGDEDBL
00543      MOVE GAC-DEDL-SERVICE-GROUP (GAC-INDEX)                      ELGDEDBL
00544        TO ACCUM-SERVICE-GROUP.                                    ELGDEDBL
00545      MOVE GAC-DEDL-INTERNAL-DESCRIPTOR (GAC-INDEX)                ELGDEDBL
00546        TO ACCUM-INTERNAL-DESCRIPTOR.                              ELGDEDBL
00547      MOVE GAC-DEDL-DAY-FACTOR-IND (GAC-INDEX)                     ELGDEDBL
00548        TO ACCUM-DAY-FACTOR-IND.                                   ELGDEDBL
00549      MOVE GAC-DEDL-CLAIM-LVL-ACCUM-IND (GAC-INDEX)                ELGDEDBL
00550        TO ACCUM-CLAIM-LVL-ACCUM-IND.                              ELGDEDBL
00551      SET 1ST-DOLR-COVRGE-LMT-NA                                   ELGDEDBL
00552          BEN-PER-MAX-OVRD-IND-NA TO TRUE.                         ELGDEDBL
00553                                                                   ELGDEDBL
00554 *->  SET OCCURRENCE PROVIDER CLASS INFORMATION                    ELGDEDBL
00555      EVALUATE TRUE ALSO TRUE                                      ELGDEDBL
00556         WHEN      SW-INTRNL-INST-PROV-CLASS                       ELGDEDBL
00557              ALSO SW-INTRNL-NOT-PROF-PROV-CLASS                   ELGDEDBL
00558            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELGDEDBL
00559         WHEN      SW-INTRNL-NOT-INST-PROV-CLASS                   ELGDEDBL
00560              ALSO SW-INTRNL-PROF-PROV-CLASS                       ELGDEDBL
00561            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELGDEDBL
00562         WHEN OTHER                                                ELGDEDBL
00563            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELGDEDBL
00564      END-EVALUATE.                                                ELGDEDBL
00565                                                                   ELGDEDBL
00566 *->  SET OCCURRENCE PROVIDER SPEC INFORMATION                     ELGDEDBL
00567        IF SW-INTRNL-PROF-PROV-SPEC                                ELGDEDBL
00568            SET ACCUM-PRVDR-SPC-PROF TO TRUE                       ELGDEDBL
00569       ELSE                                                        ELGDEDBL
00570            SET ACCUM-PRVDR-SPC-ALL TO TRUE                        ELGDEDBL
00571       END-IF.                                                     ELGDEDBL
00572                                                                   ELGDEDBL
00573 *->  SET VARIABLE PORTION DATA ELEMENTS                           ELGDEDBL
00574      MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                        ELGDEDBL
00575        TO ACCUM-VALUE-LIMIT (1).                                  ELGDEDBL
00576      MOVE WS-IBGR-SLOT-NBR TO ACCUM-IBGR-SLOT-NBR (1).            ELGDEDBL
00577      MOVE WS-IDGD-SLOT-NBR TO ACCUM-IDGD-SLOT-NBR (1).            ELGDEDBL
00578      MOVE WS-IPGN-SLOT-NBR TO ACCUM-IPGN-SLOT-NBR (1).            ELGDEDBL
00579      MOVE WS-IPGP-SLOT-NBR TO ACCUM-IPGP-SLOT-NBR (1).            ELGDEDBL
00580      MOVE WS-IPGT-SLOT-NBR TO ACCUM-IPGT-SLOT-NBR (1).            ELGDEDBL
00581      MOVE WS-IPGS-SLOT-NBR TO ACCUM-IPGS-SLOT-NBR (1).            ELGDEDBL
00582                                                                   ELGDEDBL
00583 /***********************************************************      ELGDEDBL
00584 *                                                          *      ELGDEDBL
00585 *    CHECK EXTRACT DATA INTEGRITY                          *      ELGDEDBL
00586 *                                                          *      ELGDEDBL
00587 ************************************************************      ELGDEDBL
00588                                                                   ELGDEDBL
00589  0410-CHK-EXTRACT-DATA-INTGRTY.                                   ELGDEDBL
00590 *->  EXECUTED BY 0300-TEST-ADL-OCCURENCE                          ELGDEDBL
00591      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELGDEDBL
00592         SET FYI-VALUE-NA TO TRUE.                                 ELGDEDBL
00593                                                                   ELGDEDBL
00594      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELGDEDBL
00595         SET COST-CONTAIN-IND-NA TO TRUE.                          ELGDEDBL
00596                                                                   ELGDEDBL
00597      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELGDEDBL
00598         SET PLACE-OF-TREATMENT-NA TO TRUE.                        ELGDEDBL
00599                                                                   ELGDEDBL
00600      IF ACCUM-BENEFIT-PERIOD = ZEROS OR SPACES OR LOW-VALUES      ELGDEDBL
00601         SET BENEFIT-PERIOD-NA TO TRUE.                            ELGDEDBL
00602                                                                   ELGDEDBL
00603      IF ACCUM-BEN-PER-TIME-QUAL = ZEROS OR SPACES OR LOW-VALUES   ELGDEDBL
00604         SET BEN-PER-TIME-QUAL-NA TO TRUE.                         ELGDEDBL
00605                                                                   ELGDEDBL
00606      IF ACCUM-INTERVAL-TYPE = ZEROS OR SPACES OR LOW-VALUES       ELGDEDBL
00607         SET INTERVAL-OVRD-IND-NA TO TRUE.                         ELGDEDBL
00608                                                                   ELGDEDBL
00609      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELGDEDBL
00610         SET INTERVAL-OVRD-IND-NA TO TRUE.                         ELGDEDBL
00611                                                                   ELGDEDBL
00612      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELGDEDBL
00613         SET L-O-B-NA TO TRUE.                                     ELGDEDBL
00614                                                                   ELGDEDBL
00615      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELGDEDBL
00616         SET REINSTATEMENT-IND-NA TO TRUE.                         ELGDEDBL
00617                                                                   ELGDEDBL
00618      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELGDEDBL
00619         SET DEFINITION-NA TO TRUE.                                ELGDEDBL
00620                                                                   ELGDEDBL
00621      IF ACCUM-CARRY-OVER-CREDIT-IND =                             ELGDEDBL
00622                          ZEROS OR SPACES OR LOW-VALUES            ELGDEDBL
00623               SET CARRY-OVER-CREDIT-IND-NA TO TRUE.               ELGDEDBL
00624                                                                   ELGDEDBL
00625      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELGDEDBL
00626         SET ASCEND-DESCEND-IND-NA TO TRUE.                        ELGDEDBL
00627                                                                   ELGDEDBL
00628      IF ACCUM-FAM-OR-INDIV = ZEROS OR SPACES OR LOW-VALUES        ELGDEDBL
00629         SET FAM-OR-INDIV-NA TO TRUE.                              ELGDEDBL
00630                                                                   ELGDEDBL
00631      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELGDEDBL
00632         SET RELATIONSHIP-IND-NA TO TRUE.                          ELGDEDBL
00633                                                                   ELGDEDBL
00634      IF ACCUM-DED-BASE-AMT-SOURCE-IND =                           ELGDEDBL
00635                            ZEROS OR SPACES OR LOW-VALUES          ELGDEDBL
00636         SET DED-BASE-AMT-SOURCE-IND-NA TO TRUE.                   ELGDEDBL
00637                                                                   ELGDEDBL
00638      IF ACCUM-VALUE-QUALIFIER = ZEROS OR SPACES OR LOW-VALUES     ELGDEDBL
00639         SET VALUE-QUALIFIER-NA TO TRUE.                           ELGDEDBL
00640                                                                   ELGDEDBL
00641      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELGDEDBL
00642         SET AGE-LMT-TO-IND-NA TO TRUE.                            ELGDEDBL
00643                                                                   ELGDEDBL
00644      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELGDEDBL
00645         SET AGE-LMT-FROM-IND-NA TO TRUE.                          ELGDEDBL
00646                                                                   ELGDEDBL
00647      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELGDEDBL
00648         SET LMT-MANDATORY-IND-NA TO TRUE.                         ELGDEDBL
00649                                                                   ELGDEDBL
00650      IF ACCUM-CO-PAY-IND (COPAY-INDEX)                            ELGDEDBL
00651                  = ZEROS OR SPACES OR LOW-VALUES                  ELGDEDBL
00652         SET CO-PAY-IND-NA (COPAY-INDEX) TO TRUE                   ELGDEDBL
00653                                                                   ELGDEDBL
00654      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELGDEDBL
00655         SET SERVICE-GROUP-NA TO TRUE.                             ELGDEDBL
00656                                                                   ELGDEDBL
00657      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELGDEDBL
00658         SET INTERNAL-DESCRIPTOR-NA TO TRUE.                       ELGDEDBL
00659                                                                   ELGDEDBL
00660      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELGDEDBL
00661         SET DAY-FACTOR-IND-NA TO TRUE.                            ELGDEDBL
00662                                                                   ELGDEDBL
00663      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELGDEDBL
00664         SET CLAIM-LVL-ACCUM-IND-NA TO TRUE.                       ELGDEDBL
00665                                                                   ELGDEDBL
00666      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELGDEDBL
00667         SET BEN-PER-MAX-OVRD-IND-NA TO TRUE.                      ELGDEDBL
00668                                                                   ELGDEDBL
00669      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELGDEDBL
00670         SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE.                       ELGDEDBL
00671                                                                   ELGDEDBL
00672 /***********************************************************      ELGDEDBL
00673 *                                                          *      ELGDEDBL
00674 *        ADD ACCUM OCCURENCE TO FILE                       *      ELGDEDBL
00675 *                                                          *      ELGDEDBL
00676 ************************************************************      ELGDEDBL
00677  0700-WRITE-EXTRACT-RECORD.                                       ELGDEDBL
00678 *->  EXECUTED BY 0300-TEST-ADL-OCCURENCE                          ELGDEDBL
00679      PERFORM 0800-EST-ADR-OF-TEMPORARY-FILE.                      ELGDEDBL
00680      SET  IOP-ADD TO TRUE.                                        ELGDEDBL
00681      SET  IOP-FCQ-NONE TO TRUE.                                   ELGDEDBL
00682      SET  IOP-KVQ-NONE TO TRUE.                                   ELGDEDBL
00683      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGDEDBL
00684                                                                   ELGDEDBL
00685 *->  ESTABLISH ADDRESSABILITY OF THE TABULAR FILE                 ELGDEDBL
00686      SET  CIA-GCTABULR-DDN TO TRUE.                               ELGDEDBL
00687      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGDEDBL
00688         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGDEDBL
00689      END-CALL.                                                    ELGDEDBL
00690      IF CIA-RC-PTR-NULL                                           ELGDEDBL
00691         PERFORM 0900-CIA-AB-UNALLOC-AREA.                         ELGDEDBL
00692                                                                   ELGDEDBL
00693 ************************************************************      ELGDEDBL
00694 *                                                          *      ELGDEDBL
00695 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELGDEDBL
00696 *                                                          *      ELGDEDBL
00697 ************************************************************      ELGDEDBL
00698  0800-EST-ADR-OF-TEMPORARY-FILE.                                  ELGDEDBL
00699 *-> EXECUTED BY 0240-SCAN-FOR-APPLIC-OCCRNCS                      ELGDEDBL
00700 *               0700-WRITE-EXTRACT-RECORD                         ELGDEDBL
00701      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGDEDBL
00702      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGDEDBL
00703         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGDEDBL
00704         END-CALL.                                                 ELGDEDBL
00705      IF CIA-RC-PTR-NULL                                           ELGDEDBL
00706         PERFORM 0900-CIA-AB-UNALLOC-AREA.                         ELGDEDBL
00707                                                                   ELGDEDBL
00708 ************************************************************      ELGDEDBL
00709 *                                                          *      ELGDEDBL
00710 *    CIA ABEND UNALLOCATED AREA                            *      ELGDEDBL
00711 *                                                          *      ELGDEDBL
00712 ************************************************************      ELGDEDBL
00713  0900-CIA-AB-UNALLOC-AREA.                                        ELGDEDBL
00714 *-> EXECUTED BY 0015-EST-ADR-OF-CNTRL-BLKS                        ELGDEDBL
00715 *               0030-EST-ADR-KEY-WK-AREA                          ELGDEDBL
00716 *               0045-EST-ADR-OF-SUBROUTINE-PAR                    ELGDEDBL
00717 *               0060-EST-ADR-BEN-PRVN-ACCUM                       ELGDEDBL
00718 *               0075-EST-ADRSABLTY-GRP-SPC                        ELGDEDBL
00719      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGDEDBL
00720      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELGDEDBL
00721                                                                   ELGDEDBL
