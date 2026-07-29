00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGCOINS
00003  PROGRAM-ID.        ELGCOINS.                                        LV002
00004                                                                   ELGCOINS
00005  AUTHOR.            LUCY TORRES.                                  ELGCOINS
00006                     RICHARD J. LUKETICH (RE-WRITE).               ELGCOINS
00007                                                                   ELGCOINS
00008  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELGCOINS
00009                     A MUTUAL LEGAL RESERVE COMPANY                ELGCOINS
00010                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELGCOINS
00011                     233 N. MICHIGAN AVE                           ELGCOINS
00012                     CHICAGO, ILLINOIS 60601                       ELGCOINS
00013                                                                   ELGCOINS
00014  DATE-WRITTEN.      03-JUN-1987.                                  ELGCOINS
00015                     03-JAN-1992 (RE-WRITE).                       ELGCOINS
00016                                                                   ELGCOINS
00017  DATE-COMPILED.                                                   ELGCOINS
00018                                                                   ELGCOINS
00019  SECURITY.          COPYRIGHT 1986, 1992,                         ELGCOINS
00020                     HEALTH CARE SERVICE CORPORATION               ELGCOINS
00021                                                                   ELGCOINS
00022  ENVIRONMENT DIVISION.                                            ELGCOINS
00023                                                                   ELGCOINS
00024  CONFIGURATION SECTION.                                           ELGCOINS
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELGCOINS
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELGCOINS
00027                                                                   ELGCOINS
00028 /*****************************************************************ELGCOINS
00029 *                                                                *ELGCOINS
00030 *  ELGCOINS - ELS:  SELECTS #ACL (COINSURANCE) ACCUMULATORS AND  *ELGCOINS
00031 *                   SETUPS THE INFORMATION TO BE PROCESSED BY    *ELGCOINS
00032 *                   THE COINSURANCE GENERATOR MODULE.  THE ACCUMS*ELGCOINS
00033 *                   ARE SELECTED FROM THE GROUP SPECIFIC AND     *ELGCOINS
00034 *                   CONTRACT LEVEL PROCESSING.                   *ELGCOINS
00035 *                                                                *ELGCOINS
00036 ******************************************************************ELGCOINS
00037 *                                                                *ELGCOINS
00038 *                      MAINTENANCE HISTORY                       *ELGCOINS
00039 *                                                                *ELGCOINS
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELGCOINS
00041 * ----- ----------- --- ----- ---------------------------------- *ELGCOINS
00042 * 01.00 03-JUN-1987 LET       CREATED                            *ELGCOINS
00043 * 01.01 25-SEP-1987 LET       ADDED DEFINITION DATA FIELD        *ELGCOINS
00044 *                                                                *ELGCOINS
00045 * 01.02 17-NOV-1987 REB       MADE CHANGES TO CORRESPOND TO NEW  *ELGCOINS
00046 *                             VERSION OF COPYBOOK ELSACUMC.      *ELGCOINS
00047 *                                                                *ELGCOINS
00048 * 01.07    SEP-1991 RKH    1. ADDED LOGIC FOR:                   *ELGCOINS
00049 *    ISSR #12010                A.  NEW PATIENT AGE FIELDS       *ELGCOINS
00050 *                               B.  RELATIONSHIP IND VALUE       *ELGCOINS
00051 *                          2. REVISE LOGIC TO LOAD INT ACCUMS    *ELGCOINS
00052 *                             INTO VARIABLE LEVEL TABLE          *ELGCOINS
00053 *                          3. ADDED COPYBOOKS :                  *ELGCOINS
00054 *                               A. GCTIBGR   - IBGR TAB          *ELGCOINS
00055 *                               B. GCTIPGT   - IPGT TAB          *ELGCOINS
00056 *                               C. ELSCFTB2  - PROVIDER TYPE     *ELGCOINS
00057 *                                         COMPARE TABLE          *ELGCOINS
00058 *                          4. ADD LOGIC TO INSPECT #IPGT AND     *ELGCOINS
00059 *                             #IBGR INT TABS TO DETERMINE IF     *ELGCOINS
00060 *                             AN OCCURRANCE IS THE SELECTED      *ELGCOINS
00061 *                             PROVIDER CLASS.                    *ELGCOINS
00062 *                                                                *ELGCOINS
00063 * 02.02 17-MAR-1992 JPB       CLONED FROM ELTACL, MADE CHANGES   *ELGCOINS
00064 *                             TO ELIMINATE MULTIPLE READS OF     *ELGCOINS
00065 *                             THE GAB-RECORD AND ELIMINATED      *ELGCOINS
00066 *                             CODE USED TO CHECK INTERNALS.      *ELGCOINS
00067 *                                                                *ELGCOINS
00068 * 02.03 02-DEC-1998 AKK      ADDED SUPPORT FOR ACP               *ELGCOINS
00069 *                                                                *ELGCOINS
00070 * 02.04 25-AUG-2000 AKK      ADDED SUPPORT FOR #IPGS             *ELGCOINS
00071 *                                                                *ELGCOINS
00072 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGCOINS
00071 *                                                                *ELGCOINS
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00071 *                                                                *ELGCOINS
00073 ******************************************************************ELGCOINS
00074      TITLE  'ELGCOINS          WORKING STORAGE'.                  ELGCOINS
00075  DATA DIVISION.                                                   ELGCOINS
00076                                                                   ELGCOINS
00077  WORKING-STORAGE SECTION.                                         ELGCOINS
00078                                                                   ELGCOINS
00079  01  SWITCHES.                                                    ELGCOINS
00080      02                                      PICTURE  X(01).      ELGCOINS
00081         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELGCOINS
00082         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELGCOINS
00083                                                                   ELGCOINS
00084      02 OCCURRENCE-APPLIES                   PICTURE  X(01).      ELGCOINS
00085         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELGCOINS
00086         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELGCOINS
00087      02                                      PICTURE  X(01).      ELGCOINS
00088         88 SW-HAS-IBGR                       VALUE 'Y'.           ELGCOINS
00089         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELGCOINS
00090      02                                      PICTURE  X(01).      ELGCOINS
00091         88 SW-HAS-IDGD                       VALUE 'Y'.           ELGCOINS
00092         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELGCOINS
00093      02                                      PICTURE  X(01).      ELGCOINS
00094         88 SW-HAS-IPGN                       VALUE 'Y'.           ELGCOINS
00095         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELGCOINS
00096      02                                      PICTURE  X(01).      ELGCOINS
00097         88 SW-HAS-IPGP                       VALUE 'Y'.           ELGCOINS
00098         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELGCOINS
00099      02                                      PICTURE  X(01).      ELGCOINS
00100         88 SW-HAS-IPGT                       VALUE 'Y'.           ELGCOINS
00101         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELGCOINS
00102      02                                      PICTURE  X(01).      ELGCOINS
00103         88 SW-HAS-IPGS                       VALUE 'Y'.           ELGCOINS
00104         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELGCOINS
00105      02                                      PICTURE  X(01).      ELGCOINS
00106         88 SW-INTRNL-INST-PROV-CL            VALUE 'Y'.           ELGCOINS
00107         88 SW-INTRNL-NOT-INST-PROV-CL        VALUE 'N'.           ELGCOINS
00108         88 SW-INTRNL-INST-PROV-CL-NOT-DET VALUE 'X'.              ELGCOINS
00109      02                                      PICTURE  X(01).      ELGCOINS
00110         88 SW-INTRNL-PROF-PROV-CL            VALUE 'Y'.           ELGCOINS
00111         88 SW-INTRNL-NOT-PROF-PROV-CL        VALUE 'N'.           ELGCOINS
00112         88 SW-INTRNL-PROF-PROV-CL-NOT-DET VALUE 'X'.              ELGCOINS
00113      02                                      PICTURE  X(01).      ELGCOINS
00114         88 SW-INTRNL-PROF-PROV-SP            VALUE 'Y'.           ELGCOINS
00115         88 SW-INTRNL-NOT-PROF-PROV-SP        VALUE 'N'.           ELGCOINS
00116         88 SW-INTRNL-PROF-PROV-SP-NOT-DET VALUE 'X'.              ELGCOINS
00117      02                                      PICTURE  X(01).      ELGCOINS
00118         88 SW-MATCHING-ENTRY-FOUND           VALUE 'Y'.           ELGCOINS
00119         88 SW-MATCHING-ENTRY-NOT-FOUND       VALUE 'N'.           ELGCOINS
00120      02                                      PICTURE  X(01).      ELGCOINS
00121         88 SW-SORT-COMPLETED                 VALUE 'Y'.           ELGCOINS
00122         88 SW-SORT-NOT-COMPLETED             VALUE 'N'.           ELGCOINS
00123                                                                   ELGCOINS
00124  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELGCOINS
00125      88 WS-LOB-INST              VALUE '1'.                       ELGCOINS
00126      88 WS-LOB-PROF              VALUE '2'.                       ELGCOINS
00127      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELGCOINS
00128      88 WS-LOB-BOTH              VALUE '3', '4', '5', '6', '7'.   ELGCOINS
00129                                                                   ELGCOINS
00130  01  PROGRAM-CONSTANTS.                                           ELGCOINS
00131      02 PC-ACL                   PICTURE  X(06) VALUE '#ACL  '.   ELGCOINS
00132      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELGCOINS
00133      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELGCOINS
00134      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELGCOINS
00135      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELGCOINS
00136      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELGCOINS
00137      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGS '.   ELGCOINS
00138      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELGCOINS
00139                                                                   ELGCOINS
00140  01  WS-WORK-FIELDS.                                              ELGCOINS
00141      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELGCOINS
00142      02 WS-OCCURRENCE-SUB        PICTURE S9(04) COMP.             ELGCOINS
00143      02 WS-SAVE-SUB              PICTURE S9(04) COMP.             ELGCOINS
00144      02 WS-SAVE-INDEX            USAGE IS INDEX.                  ELGCOINS
00145      02 SORT-SUB                 PICTURE S9(04) COMP.             ELGCOINS
00146      02 TEST-SUB                 PICTURE S9(04) COMP.             ELGCOINS
00147                                                                   ELGCOINS
00148  01  ACCUM-HOLD-TBL.                                              ELGCOINS
00149      02  ACCUM-SLOT-NBR          PICTURE S9(07) COMP-3            ELGCOINS
00150                                  OCCURS 5 TIMES.                  ELGCOINS
00151                                                                   ELGCOINS
00152  01  WS-OCCURRENCE-PROCESSED-TBL.                                 ELGCOINS
00153      02                          PICTURE X                        ELGCOINS
00154                                  OCCURS 44 TIMES                  ELGCOINS
00155                                  INDEXED BY WS-OCCURRENCE-INDEX.  ELGCOINS
00156          88  WS-OCCURRENCE-PROCESSED           VALUE 'P'.         ELGCOINS
00157          88  WS-OCCURRENCE-NOT-PROCESSED       VALUE ' '.         ELGCOINS
00158                                                                   ELGCOINS
00159  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELGCOINS
00160      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGCOINS
00161      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGCOINS
00162      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGCOINS
00163      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGCOINS
00164      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGCOINS
00165      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGCOINS
00166                                                                   ELGCOINS
00167  01  WS-ASCEND-DESCEND-ENTRY-HOLD PICTURE X(28).                  ELGCOINS
00168                                                                   ELGCOINS
00169 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELGCOINS
00170      COPY ELSCFTB2.                                               ELGCOINS
00171                                                                   ELGCOINS
00172 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELGCOINS
00173      COPY ELSCFTB9.                                               ELGCOINS
00174                                                                   ELGCOINS
00175      TITLE  'ELGCOINS          LINKAGE SECTION'                   ELGCOINS
00176  LINKAGE SECTION.                                                 ELGCOINS
00177  01  DFHCOMMAREA.                                                 ELGCOINS
00178      COPY ELSCOMMC.                                               ELGCOINS
00179 /                                                                 ELGCOINS
00180      COPY ELSCIA2C.                                               ELGCOINS
00181 /                                                                 ELGCOINS
00182      COPY ELSIOPMC.                                               ELGCOINS
00183 /                                                                 ELGCOINS
00184      COPY ELSKEYSC.                                               ELGCOINS
00185 /                                                                 ELGCOINS
00186      COPY ELSSRTPC.                                               ELGCOINS
00187 /                                                                 ELGCOINS
00188      COPY ELSSSCBC.                                               ELGCOINS
00189 /                                                                 ELGCOINS
00190  01  GAB-RECORD-AREA.                                             ELGCOINS
00191      COPY GCTACLC.                                                ELGCOINS
00192 /                                                                 ELGCOINS
00193      COPY ELSACUMC.                                               ELGCOINS
00194      TITLE  'ELGCOINS          PROCEDURE DIVISION'.               ELGCOINS
00195 ************************************************************      ELGCOINS
00196 *                                                          *      ELGCOINS
00197 *    ELGCOINS MAINLINE                                     *      ELGCOINS
00198 *                                                          *      ELGCOINS
00199 ************************************************************      ELGCOINS
00200                                                                   ELGCOINS
00201  PROCEDURE DIVISION.                                              ELGCOINS
00202                                                                   ELGCOINS
00203      PERFORM 0010-INITIALIZATION.                                 ELGCOINS
00204      PERFORM 0100-PROCESS.                                        ELGCOINS
00205      GOBACK.                                                      ELGCOINS
00206                                                                   ELGCOINS
00207 ************************************************************      ELGCOINS
00208 *                                                          *      ELGCOINS
00209 *    INITIALIZATION                                        *      ELGCOINS
00210 *                                                          *      ELGCOINS
00211 ************************************************************      ELGCOINS
00212                                                                   ELGCOINS
00213  0010-INITIALIZATION.                                             ELGCOINS
00214      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELGCOINS
00215      PERFORM 0060-EST-ADR-KEY-WK-AREA.                            ELGCOINS
00216      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELGCOINS
00217      PERFORM 0120-EST-ADR-BEN-PRVN-ACCUM.                         ELGCOINS
00218      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGCOINS
00219                                                                   ELGCOINS
00220 ************************************************************      ELGCOINS
00221 *                                                          *      ELGCOINS
00222 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGCOINS
00223 *                                                          *      ELGCOINS
00224 ************************************************************      ELGCOINS
00225                                                                   ELGCOINS
00226  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELGCOINS
00227      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGCOINS
00228      THEN                                                         ELGCOINS
00229         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGCOINS
00230      ELSE                                                         ELGCOINS
00231         IF ECA-CIA-PTR = NULL                                     ELGCOINS
00232         THEN                                                      ELGCOINS
00233            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGCOINS
00234         ELSE                                                      ELGCOINS
00235            CALL 'ELUINISM' USING DFHCOMMAREA                      ELGCOINS
00236               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELGCOINS
00237               END-CALL                                            ELGCOINS
00238            SET CIA-ELSSSCB-DDN TO TRUE                            ELGCOINS
00239            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELGCOINS
00240               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELGCOINS
00241               END-CALL                                            ELGCOINS
00242            IF CIA-RC-PTR-NULL                                     ELGCOINS
00243            THEN                                                   ELGCOINS
00244               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELGCOINS
00245               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGCOINS
00246            ELSE                                                   ELGCOINS
00247               CONTINUE                                            ELGCOINS
00248            END-IF                                                 ELGCOINS
00249         END-IF                                                    ELGCOINS
00250      END-IF.                                                      ELGCOINS
00251                                                                   ELGCOINS
00252 /***********************************************************      ELGCOINS
00253 *                                                          *      ELGCOINS
00254 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELGCOINS
00255 *                                                          *      ELGCOINS
00256 ************************************************************      ELGCOINS
00257                                                                   ELGCOINS
00258  0060-EST-ADR-KEY-WK-AREA.                                        ELGCOINS
00259      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGCOINS
00260      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOINS
00261         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELGCOINS
00262         END-CALL.                                                 ELGCOINS
00263      IF CIA-RC-PTR-NULL                                           ELGCOINS
00264         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGCOINS
00265         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGCOINS
00266      END-IF.                                                      ELGCOINS
00267                                                                   ELGCOINS
00268 ************************************************************      ELGCOINS
00269 *                                                          *      ELGCOINS
00270 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGCOINS
00271 *                                                          *      ELGCOINS
00272 ************************************************************      ELGCOINS
00273                                                                   ELGCOINS
00274  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGCOINS
00275      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGCOINS
00276      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOINS
00277         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELGCOINS
00278         END-CALL.                                                 ELGCOINS
00279      IF CIA-RC-PTR-NULL                                           ELGCOINS
00280         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGCOINS
00281         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGCOINS
00282      END-IF.                                                      ELGCOINS
00283                                                                   ELGCOINS
00284 ************************************************************      ELGCOINS
00285 *                                                          *      ELGCOINS
00286 *    ESTABLISH ADDRESSABILITY OF BENEFIT PROVSION LEVEL    *      ELGCOINS
00287 *    ACCUMULATOR RECORD                                    *      ELGCOINS
00288 *                                                          *      ELGCOINS
00289 ************************************************************      ELGCOINS
00290                                                                   ELGCOINS
00291  0120-EST-ADR-BEN-PRVN-ACCUM.                                     ELGCOINS
00292      SET CIA-GCTABULR-DDN TO TRUE.                                ELGCOINS
00293      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOINS
00294         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGCOINS
00295         END-CALL.                                                 ELGCOINS
00296      IF CIA-RC-PTR-NULL                                           ELGCOINS
00297         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGCOINS
00298         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGCOINS
00299      ELSE                                                         ELGCOINS
00300         IF IOP-REC-PTR = NULLS                                    ELGCOINS
00301            SET CIA-AB-UNALLOC-AREA TO TRUE                        ELGCOINS
00302            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELGCOINS
00303         ELSE                                                      ELGCOINS
00304            SET ADDRESS OF GAB-RECORD-AREA TO IOP-REC-PTR          ELGCOINS
00305         END-IF                                                    ELGCOINS
00306      END-IF.                                                      ELGCOINS
00307                                                                   ELGCOINS
00308                                                                   ELGCOINS
00309 /***********************************************************      ELGCOINS
00310 *                                                          *      ELGCOINS
00311 *        PROCESS                                           *      ELGCOINS
00312 *                                                          *      ELGCOINS
00313 ************************************************************      ELGCOINS
00314                                                                   ELGCOINS
00315  0100-PROCESS.                                                    ELGCOINS
00316      PERFORM 0210-SCAN-FOR-APPLIC-OCCRNCS.                        ELGCOINS
00317                                                                   ELGCOINS
00318      IF SW-NO-APPLIC-ACCUM-FOUND                                  ELGCOINS
00319         EVALUATE TRUE                                             ELGCOINS
00320            WHEN SSB-PROV-CLASS-INST                               ELGCOINS
00321               SET SRP-INST-NOT-APPLICABLE TO TRUE                 ELGCOINS
00322            WHEN SSB-PROV-CLASS-PROF                               ELGCOINS
00323               SET SRP-PROF-NOT-APPLICABLE TO TRUE                 ELGCOINS
00324            WHEN SSB-PROV-CLASS-BOTH                               ELGCOINS
00325               SET SRP-NO-ACCUMS-FOUND TO TRUE                     ELGCOINS
00326            WHEN OTHER                                             ELGCOINS
00327               SET CIA-AB-PGM-LOGIC TO TRUE                        ELGCOINS
00328               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGCOINS
00329            END-EVALUATE                                           ELGCOINS
00330      END-IF.                                                      ELGCOINS
00331                                                                   ELGCOINS
00332      SET SRP-BEN-PROV-ACCUM TO TRUE.                              ELGCOINS
00333                                                                   ELGCOINS
00334 * -- LINK TO THE OUTPUT GENERATOR                                 ELGCOINS
00335      EXEC CICS LINK PROGRAM ('ELGACL') COMMAREA (DFHCOMMAREA)     ELGCOINS
00336         END-EXEC.                                                 ELGCOINS
00337                                                                   ELGCOINS
00338 /***********************************************************      ELGCOINS
00339 *                                                          *      ELGCOINS
00340 *    SCAN ACL ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELGCOINS
00341 *                                                          *      ELGCOINS
00342 ************************************************************      ELGCOINS
00343                                                                   ELGCOINS
00344  0210-SCAN-FOR-APPLIC-OCCRNCS.                                    ELGCOINS
00345      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGCOINS
00346      PERFORM 0220-DELETE-ACL-SUMMARY-FILE.                        ELGCOINS
00347      PERFORM 0230-ALLOC-WORKFILE-REC-AREA.                        ELGCOINS
00348                                                                   ELGCOINS
00349 * -- OBTAIN ACCUMULATOR TABULAR RECORD                            ELGCOINS
00350      MOVE SPACES TO WS-OCCURRENCE-PROCESSED-TBL                   ELGCOINS
00351 * -- SCAN ACCUMULATOR TABULAR                                     ELGCOINS
00352      PERFORM WITH TEST BEFORE                                     ELGCOINS
00353         VARYING WS-OCCURRENCE-SUB FROM 1 BY 1                     ELGCOINS
00354            UNTIL WS-OCCURRENCE-SUB >= GAB-ENTRY-COUNT             ELGCOINS
00355             IF WS-OCCURRENCE-NOT-PROCESSED (WS-OCCURRENCE-SUB)    ELGCOINS
00356              THEN                                                 ELGCOINS
00357              SET GAB-INDEX  TO WS-OCCURRENCE-SUB                  ELGCOINS
00358              SET WS-OCCURRENCE-INDEX TO GAB-INDEX                 ELGCOINS
00359              PERFORM 0300-TEST-ACL-OCCURRENCE                     ELGCOINS
00360             END-IF                                                ELGCOINS
00361      END-PERFORM.                                                 ELGCOINS
00362                                                                   ELGCOINS
00363                                                                   ELGCOINS
00364 /***********************************************************      ELGCOINS
00365 *                                                          *      ELGCOINS
00366 *        DELETE ACL SUMMARY FILE                           *      ELGCOINS
00367 *                                                          *      ELGCOINS
00368 ************************************************************      ELGCOINS
00369                                                                   ELGCOINS
00370  0220-DELETE-ACL-SUMMARY-FILE.                                    ELGCOINS
00371      SET IOP-DEL TO TRUE.                                         ELGCOINS
00372      SET IOP-FCQ-NONE TO TRUE.                                    ELGCOINS
00373      SET IOP-KVQ-NONE TO TRUE.                                    ELGCOINS
00374      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGCOINS
00375                                                                   ELGCOINS
00376 /***********************************************************      ELGCOINS
00377 *                                                          *      ELGCOINS
00378 *    ALLOCATE WORKFILE RECORD AREA                         *      ELGCOINS
00379 *                                                          *      ELGCOINS
00380 ************************************************************      ELGCOINS
00381                                                                   ELGCOINS
00382  0230-ALLOC-WORKFILE-REC-AREA.                                    ELGCOINS
00383      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGCOINS
00384      SET CIA-STG-GETMAIN TO TRUE.                                 ELGCOINS
00385      SET IOP-GETMAIN-REC TO TRUE.                                 ELGCOINS
00386      COMPUTE IOP-MAX-REC-LEN =                                    ELGCOINS
00387              LENGTH OF ACCUM-FIXED-AREA                           ELGCOINS
00388 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELGCOINS
00389            + LENGTH OF ACCUM-VARIABLE-AREA                        ELGCOINS
00390            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELGCOINS
00391 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELGCOINS
00392 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELGCOINS
00393                                                                   ELGCOINS
00394      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGCOINS
00395      IF IOP-REC-PTR = NULLS                                       ELGCOINS
00396      THEN                                                         ELGCOINS
00397         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGCOINS
00398         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGCOINS
00399      ELSE                                                         ELGCOINS
00400         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELGCOINS
00401      END-IF.                                                      ELGCOINS
00402                                                                   ELGCOINS
00403 /***********************************************************      ELGCOINS
00404 *                                                          *      ELGCOINS
00405 *        TEST ACL OCCURS                                   *      ELGCOINS
00406 *                                                          *      ELGCOINS
00407 ************************************************************      ELGCOINS
00408                                                                   ELGCOINS
00409  0300-TEST-ACL-OCCURRENCE.                                        ELGCOINS
00410                                                                   ELGCOINS
00411      MOVE GAB-COINS-L-O-B (GAB-INDEX) TO WS-LOB-ACCUM-OCCRNC.     ELGCOINS
00412      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELGCOINS
00413      PERFORM 0320-SCAN-FOR-INTERNALS.                             ELGCOINS
00414      IF SW-OCCRNC-APPLIES                                         ELGCOINS
00415      THEN                                                         ELGCOINS
00416 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELGCOINS
00417         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELGCOINS
00418         PERFORM 0340-INIT-ACCUM-EXTRACT                           ELGCOINS
00419         PERFORM 0350-EXTRACT-ACCUM                                ELGCOINS
00420         PERFORM 0490-CHK-EXTRACT-DATA-INTGRTY                     ELGCOINS
00421         PERFORM 0360-ACCUM-VBL-PORTION                            ELGCOINS
00422         PERFORM 0710-WRITE-EXTRACT-RECORD                         ELGCOINS
00423      END-IF.                                                      ELGCOINS
00424      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-INDEX)            ELGCOINS
00425          TO TRUE.                                                 ELGCOINS
00426                                                                   ELGCOINS
00427                                                                   ELGCOINS
00428 /***********************************************************      ELGCOINS
00429 *                                                          *      ELGCOINS
00430 *        INITIALIZE OCCURRENCE                             *      ELGCOINS
00431 *                                                          *      ELGCOINS
00432 ************************************************************      ELGCOINS
00433                                                                   ELGCOINS
00434  0310-INITIALIZE-OCCURRENCE.                                      ELGCOINS
00435      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELGCOINS
00436      SET SW-HAS-NO-IBGR                                           ELGCOINS
00437          SW-HAS-NO-IDGD                                           ELGCOINS
00438          SW-HAS-NO-IPGN                                           ELGCOINS
00439          SW-HAS-NO-IPGP                                           ELGCOINS
00440          SW-HAS-NO-IPGT                                           ELGCOINS
00441          SW-HAS-NO-IPGS                                           ELGCOINS
00442       TO TRUE.                                                    ELGCOINS
00443      INITIALIZE WS-IBGR-SLOT-NBR                                  ELGCOINS
00444                 WS-IDGD-SLOT-NBR                                  ELGCOINS
00445                 WS-IPGN-SLOT-NBR                                  ELGCOINS
00446                 WS-IPGP-SLOT-NBR                                  ELGCOINS
00447                 WS-IPGT-SLOT-NBR                                  ELGCOINS
00448                 WS-IPGS-SLOT-NBR.                                 ELGCOINS
00449      SET SW-INTRNL-INST-PROV-CL-NOT-DET                           ELGCOINS
00450          SW-INTRNL-PROF-PROV-CL-NOT-DET                           ELGCOINS
00451          SW-INTRNL-PROF-PROV-SP-NOT-DET                           ELGCOINS
00452       TO TRUE.                                                    ELGCOINS
00453                                                                   ELGCOINS
00454                                                                   ELGCOINS
00455 /***********************************************************      ELGCOINS
00456 *                                                          *      ELGCOINS
00457 *        SCAN FOR INTERNAL TABULARS                        *      ELGCOINS
00458 *                                                          *      ELGCOINS
00459 ************************************************************      ELGCOINS
00460                                                                   ELGCOINS
00461  0320-SCAN-FOR-INTERNALS.                                         ELGCOINS
00462 *    (THIS IS DONE NOW IN CASE IPGT OR IBGR IS NEEDED TO DETERMINEELGCOINS
00463 *     WHETHER OCCURRENCE IS INSTITUTIONAL OR PROFESSIONAL.)       ELGCOINS
00464      PERFORM 0330-SCAN-THE-INTERNAL-TABULAR                       ELGCOINS
00465         VARYING GAB-INT-INDEX FROM 1 BY 1                         ELGCOINS
00466           UNTIL    GAB-INT-INDEX                                  ELGCOINS
00467                 >= GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX).        ELGCOINS
00468                                                                   ELGCOINS
00469      EVALUATE TRUE ALSO TRUE                                      ELGCOINS
00470         WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                        ELGCOINS
00471            SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE                  ELGCOINS
00472            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGCOINS
00473            SET SW-OCCRNC-APPLIES TO TRUE                          ELGCOINS
00474         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH                 ELGCOINS
00475            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGCOINS
00476            SET SW-OCCRNC-APPLIES TO TRUE                          ELGCOINS
00477         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST                 ELGCOINS
00478            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGCOINS
00479            SET SW-OCCRNC-APPLIES TO TRUE                          ELGCOINS
00480         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH                 ELGCOINS
00481            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGCOINS
00482            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGCOINS
00483            SET SW-OCCRNC-APPLIES TO TRUE                          ELGCOINS
00484         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF                 ELGCOINS
00485            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGCOINS
00486            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGCOINS
00487            SET SW-OCCRNC-APPLIES TO TRUE                          ELGCOINS
00488         WHEN OTHER                                                ELGCOINS
00489            CONTINUE                                               ELGCOINS
00490       END-EVALUATE.                                               ELGCOINS
00491                                                                   ELGCOINS
00492 /***********************************************************      ELGCOINS
00493 *                                                          *      ELGCOINS
00494 *        SCAN THE INTERNAL TABULARS                        *      ELGCOINS
00495 *                                                          *      ELGCOINS
00496 ************************************************************      ELGCOINS
00497                                                                   ELGCOINS
00498  0330-SCAN-THE-INTERNAL-TABULAR.                                  ELGCOINS
00499      IF GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX) > 0               ELGCOINS
00500      THEN                                                         ELGCOINS
00501         MOVE GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX)              ELGCOINS
00502           TO WS-SLOT-NBR                                          ELGCOINS
00503         EVALUATE GAB-INT-ID (GAB-INDEX, GAB-INT-INDEX)            ELGCOINS
00504            WHEN PC-IBGR                                           ELGCOINS
00505               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELGCOINS
00506               SET SW-HAS-IBGR                                     ELGCOINS
00507                TO TRUE                                            ELGCOINS
00508            WHEN PC-IDGD                                           ELGCOINS
00509               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELGCOINS
00510               SET SW-HAS-IDGD                                     ELGCOINS
00511                TO TRUE                                            ELGCOINS
00512            WHEN PC-IPGP                                           ELGCOINS
00513               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELGCOINS
00514               SET SW-HAS-IPGP                                     ELGCOINS
00515                TO TRUE                                            ELGCOINS
00516            WHEN PC-IPGN                                           ELGCOINS
00517               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELGCOINS
00518               SET SW-HAS-IPGN                                     ELGCOINS
00519                TO TRUE                                            ELGCOINS
00520            WHEN PC-IPGT                                           ELGCOINS
00521               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELGCOINS
00522               SET SW-HAS-IPGT                                     ELGCOINS
00523                TO TRUE                                            ELGCOINS
00524            WHEN PC-IPGS                                           ELGCOINS
00525               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELGCOINS
00526               SET SW-HAS-IPGS                                     ELGCOINS
00527                TO TRUE                                            ELGCOINS
00528            WHEN OTHER                                             ELGCOINS
00529               CONTINUE                                            ELGCOINS
00530         END-EVALUATE                                              ELGCOINS
00531      END-IF.                                                      ELGCOINS
00532                                                                   ELGCOINS
00533 /***********************************************************      ELGCOINS
00534 *                                                          *      ELGCOINS
00535 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELGCOINS
00536 *                                                          *      ELGCOINS
00537 ************************************************************      ELGCOINS
00538                                                                   ELGCOINS
00539  0340-INIT-ACCUM-EXTRACT.                                         ELGCOINS
00540      INITIALIZE ACCUM-FIXED-AREA.                                 ELGCOINS
00541      SET ACCUM-ACL TO TRUE.                                       ELGCOINS
00542      MOVE +1 TO  ACCUM-ASCEND-DESCEND-COUNT.                      ELGCOINS
00543      SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELGCOINS
00544      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELGCOINS
00545      INITIALIZE ACCUM-COPAY-ENTRY (1).                            ELGCOINS
00546                                                                   ELGCOINS
00547 /***********************************************************      ELGCOINS
00548 *                                                          *      ELGCOINS
00549 *        EXTRACT ACL TOPIC LEVEL DATA ELEMENTS             *      ELGCOINS
00550 *                                                          *      ELGCOINS
00551 ************************************************************      ELGCOINS
00552                                                                   ELGCOINS
00553  0350-EXTRACT-ACCUM.                                              ELGCOINS
00554                                                                   ELGCOINS
00555 * -- SET FIXED PORTION DATA ELEMENTS                              ELGCOINS
00556                                                                   ELGCOINS
00557      MOVE GAB-COINS-BENEFIT-PERIOD (GAB-INDEX)                    ELGCOINS
00558        TO     ACCUM-BENEFIT-PERIOD.                               ELGCOINS
00559                                                                   ELGCOINS
00560      MOVE GAB-COINS-FAM-OR-INDIV (GAB-INDEX)                      ELGCOINS
00561        TO     ACCUM-FAM-OR-INDIV.                                 ELGCOINS
00562                                                                   ELGCOINS
00563      MOVE GAB-COINS-L-O-B (GAB-INDEX) TO ACCUM-L-O-B.             ELGCOINS
00564                                                                   ELGCOINS
00565      MOVE GAB-COINS-DEFINITION (GAB-INDEX) TO ACCUM-DEFINITION.   ELGCOINS
00566                                                                   ELGCOINS
00567      MOVE GAB-COINS-DAY-FACTOR-IND (GAB-INDEX)                    ELGCOINS
00568        TO     ACCUM-DAY-FACTOR-IND.                               ELGCOINS
00569                                                                   ELGCOINS
00570      MOVE GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX)               ELGCOINS
00571        TO     ACCUM-INTERNAL-DESCRIPTOR.                          ELGCOINS
00572                                                                   ELGCOINS
00573      MOVE GAB-COINS-SERVICE-GROUP (GAB-INDEX)                     ELGCOINS
00574        TO     ACCUM-SERVICE-GROUP.                                ELGCOINS
00575                                                                   ELGCOINS
00576      MOVE GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)                ELGCOINS
00577        TO     ACCUM-PLACE-OF-TREATMENT.                           ELGCOINS
00578                                                                   ELGCOINS
00579      MOVE GAB-COINS-CONDITION (GAB-INDEX) TO ACCUM-CONDITION.     ELGCOINS
00580                                                                   ELGCOINS
00581      MOVE GAB-COINS-REINSTATEMENT-IND (GAB-INDEX)                 ELGCOINS
00582        TO     ACCUM-REINSTATEMENT-IND.                            ELGCOINS
00583                                                                   ELGCOINS
00584      MOVE GAB-COINS-CLAIM-LVL-ACCUM-IND (GAB-INDEX)               ELGCOINS
00585        TO     ACCUM-CLAIM-LVL-ACCUM-IND.                          ELGCOINS
00586                                                                   ELGCOINS
00587      MOVE GAB-COINS-1ST-DOLR-COVRGE-LMT (GAB-INDEX)               ELGCOINS
00588        TO     ACCUM-1ST-DOLR-COVRGE-LMT.                          ELGCOINS
00589                                                                   ELGCOINS
00590      MOVE GAB-COINS-CO-PAY-IND (GAB-INDEX)                        ELGCOINS
00591        TO     ACCUM-CO-PAY-IND (COPAY-INDEX).                     ELGCOINS
00592                                                                   ELGCOINS
00593      MOVE GAB-COINS-COST-CONTAIN-IND (GAB-INDEX)                  ELGCOINS
00594        TO     ACCUM-COST-CONTAIN-IND.                             ELGCOINS
00595                                                                   ELGCOINS
00596      MOVE GAB-COINS-AGE-LIMIT-FROM (GAB-INDEX)                    ELGCOINS
00597        TO     ACCUM-AGE-LIMIT-FROM-VAL.                           ELGCOINS
00598                                                                   ELGCOINS
00599      MOVE GAB-COINS-AGE-LIMIT-TO (GAB-INDEX)                      ELGCOINS
00600        TO     ACCUM-AGE-LIMIT-TO-VAL.                             ELGCOINS
00601                                                                   ELGCOINS
00602      MOVE GAB-COINS-AGE-QUAL-IND-FROM (GAB-INDEX)                 ELGCOINS
00603        TO     ACCUM-AGE-LIMIT-FROM-IND.                           ELGCOINS
00604                                                                   ELGCOINS
00605      MOVE GAB-COINS-AGE-QUAL-IND-TO (GAB-INDEX)                   ELGCOINS
00606        TO     ACCUM-AGE-LIMIT-TO-IND.                             ELGCOINS
00607                                                                   ELGCOINS
00608      MOVE GAB-COINS-RELATIONSHIP-IND (GAB-INDEX)                  ELGCOINS
00609        TO     ACCUM-RELATIONSHIP-IND.                             ELGCOINS
00610                                                                   ELGCOINS
00611      MOVE GAB-COINS-LMT-MANDATORY-IND (GAB-INDEX)                 ELGCOINS
00612        TO     ACCUM-LMT-MANDATORY-IND.                            ELGCOINS
00613                                                                   ELGCOINS
00614      MOVE GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX)                ELGCOINS
00615        TO     ACCUM-ASCEND-DESCEND-IND.                           ELGCOINS
00616                                                                   ELGCOINS
00617      MOVE GAB-COINS-BEN-PER-TIME-QUAL (GAB-INDEX)                 ELGCOINS
00618        TO     ACCUM-BEN-PER-TIME-QUAL.                            ELGCOINS
00619                                                                   ELGCOINS
00620      MOVE GAB-COINS-BEN-PER-TIME-FCTR (GAB-INDEX)                 ELGCOINS
00621        TO     ACCUM-BEN-PER-TIME-FCTR.                            ELGCOINS
00622                                                                   ELGCOINS
00623      MOVE GAB-COINS-INTERVAL-TIME-FCTR (GAB-INDEX)                ELGCOINS
00624        TO     ACCUM-INTERVAL-TIME-FCTR.                           ELGCOINS
00625                                                                   ELGCOINS
00626      MOVE GAB-COINS-INTERVAL-TYPE (GAB-INDEX)                     ELGCOINS
00627        TO     ACCUM-INTERVAL-TYPE.                                ELGCOINS
00628                                                                   ELGCOINS
00629      MOVE GAB-COINS-INTERVAL-OVRD-IND (GAB-INDEX)                 ELGCOINS
00630        TO     ACCUM-INTERVAL-OVRD-IND.                            ELGCOINS
00631                                                                   ELGCOINS
00632      MOVE GAB-COINS-INTERVAL-OVRD-VALUE (GAB-INDEX)               ELGCOINS
00633        TO     ACCUM-INTERVAL-OVRD-VALUE.                          ELGCOINS
00634                                                                   ELGCOINS
00635      MOVE GAB-COINS-VALUE-QUALIFIER (GAB-INDEX)                   ELGCOINS
00636        TO     ACCUM-VALUE-QUALIFIER.                              ELGCOINS
00637                                                                   ELGCOINS
00638      MOVE GAB-COINS-FYI-VALUE (GAB-INDEX) TO ACCUM-FYI-VALUE.     ELGCOINS
00639                                                                   ELGCOINS
00640 * -- SET OCCURRENCE PROVIDER CLASS INFORMATION                    ELGCOINS
00641      EVALUATE TRUE ALSO TRUE                                      ELGCOINS
00642         WHEN      SW-INTRNL-INST-PROV-CL                          ELGCOINS
00643              ALSO SW-INTRNL-NOT-PROF-PROV-CL                      ELGCOINS
00644            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELGCOINS
00645         WHEN      SW-INTRNL-NOT-INST-PROV-CL                      ELGCOINS
00646              ALSO SW-INTRNL-PROF-PROV-CL                          ELGCOINS
00647            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELGCOINS
00648         WHEN OTHER                                                ELGCOINS
00649            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELGCOINS
00650      END-EVALUATE.                                                ELGCOINS
00651                                                                   ELGCOINS
00652 * -- SET OCCURRENCE PROVIDER SPEC INFORMATION                     ELGCOINS
00653         IF SW-INTRNL-NOT-INST-PROV-CL                             ELGCOINS
00654              AND SW-INTRNL-PROF-PROV-CL                           ELGCOINS
00655            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELGCOINS
00656         ELSE                                                      ELGCOINS
00657            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELGCOINS
00658         END-IF.                                                   ELGCOINS
00659                                                                   ELGCOINS
00660      SET CARRY-OVER-CREDIT-IND-NA                                 ELGCOINS
00661       TO TRUE.                                                    ELGCOINS
00662                                                                   ELGCOINS
00663      SET DED-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELGCOINS
00664      SET OPX-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELGCOINS
00665      SET MAX-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELGCOINS
00666                                                                   ELGCOINS
00667 ************************************************************      ELGCOINS
00668 *                                                          *      ELGCOINS
00669 *        ACCUM VARIABLE PORTION                            *      ELGCOINS
00670 *                                                          *      ELGCOINS
00671 ************************************************************      ELGCOINS
00672                                                                   ELGCOINS
00673  0360-ACCUM-VBL-PORTION.                                          ELGCOINS
00674      SET ASC-DES-INDEX TO 1.                                      ELGCOINS
00675      PERFORM 0370-EXTRACT-VARIABLE-PORTION.                       ELGCOINS
00676      IF ACCUM-VARIABLE-TYPE                                       ELGCOINS
00677         THEN                                                      ELGCOINS
00678             PERFORM 0380-EXTRACT-ADDL-OCCURNCS                    ELGCOINS
00679       END-IF.                                                     ELGCOINS
00680                                                                   ELGCOINS
00681 /***********************************************************      ELGCOINS
00682 *                                                          *      ELGCOINS
00683 *        EXTRACT VARIABLE PORTION                          *      ELGCOINS
00684 *                                                          *      ELGCOINS
00685 ************************************************************      ELGCOINS
00686                                                                   ELGCOINS
00687  0370-EXTRACT-VARIABLE-PORTION.                                   ELGCOINS
00688      MOVE GAB-COINS-BISCENDING-IND (GAB-INDEX)                    ELGCOINS
00689        TO ACCUM-BISCEND-IND (ASC-DES-INDEX).                      ELGCOINS
00690      MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)                     ELGCOINS
00691        TO ACCUM-PERCENT-LEVEL (ASC-DES-INDEX).                    ELGCOINS
00692      MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                       ELGCOINS
00693        TO ACCUM-VALUE-LIMIT (ASC-DES-INDEX).                      ELGCOINS
00694      MOVE WS-IBGR-SLOT-NBR                                        ELGCOINS
00695        TO ACCUM-IBGR-SLOT-NBR (ASC-DES-INDEX).                    ELGCOINS
00696      MOVE WS-IDGD-SLOT-NBR                                        ELGCOINS
00697        TO ACCUM-IDGD-SLOT-NBR (ASC-DES-INDEX).                    ELGCOINS
00698      MOVE WS-IPGN-SLOT-NBR                                        ELGCOINS
00699        TO ACCUM-IPGN-SLOT-NBR (ASC-DES-INDEX).                    ELGCOINS
00700      MOVE WS-IPGP-SLOT-NBR                                        ELGCOINS
00701        TO ACCUM-IPGP-SLOT-NBR (ASC-DES-INDEX).                    ELGCOINS
00702      MOVE WS-IPGT-SLOT-NBR                                        ELGCOINS
00703        TO ACCUM-IPGT-SLOT-NBR (ASC-DES-INDEX).                    ELGCOINS
00704      MOVE WS-IPGS-SLOT-NBR                                        ELGCOINS
00705        TO ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX).                    ELGCOINS
00706      IF ACCUM-BISCEND-IND (ASC-DES-INDEX)                         ELGCOINS
00707         = ZERO OR SPACES OR LOW-VALUES                            ELGCOINS
00708         SET BISCEND-IND-NA (ASC-DES-INDEX) TO TRUE.               ELGCOINS
00709      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB) TO TRUE.     ELGCOINS
00710                                                                   ELGCOINS
00711 ************************************************************      ELGCOINS
00712 *                                                          *      ELGCOINS
00713 *        EXTRACT ADDITIONAL OCCURRENCES                    *      ELGCOINS
00714 *                                                          *      ELGCOINS
00715 ************************************************************      ELGCOINS
00716                                                                   ELGCOINS
00717  0380-EXTRACT-ADDL-OCCURNCS.                                      ELGCOINS
00718      MOVE WS-OCCURRENCE-SUB TO WS-SAVE-SUB                        ELGCOINS
00719      SET  WS-SAVE-INDEX    TO GAB-INDEX.                          ELGCOINS
00720      ADD 1 TO WS-OCCURRENCE-SUB.                                  ELGCOINS
00721      PERFORM 0390-TEST-SUBSEQ-OCCRNCES                            ELGCOINS
00722          VARYING WS-OCCURRENCE-SUB                                ELGCOINS
00723             FROM WS-OCCURRENCE-SUB BY 1                           ELGCOINS
00724          UNTIL WS-OCCURRENCE-INDEX >= GAB-ENTRY-COUNT.            ELGCOINS
00725      MOVE WS-SAVE-SUB TO WS-OCCURRENCE-SUB.                       ELGCOINS
00726      SET  GAB-INDEX   TO WS-SAVE-INDEX.                           ELGCOINS
00727                                                                   ELGCOINS
00728 ************************************************************      ELGCOINS
00729 *                                                          *      ELGCOINS
00730 *        TEST SUBSEQUENT OCCURRENCES                       *      ELGCOINS
00731 *                                                          *      ELGCOINS
00732 ************************************************************      ELGCOINS
00733                                                                   ELGCOINS
00734  0390-TEST-SUBSEQ-OCCRNCES.                                       ELGCOINS
00735      SET GAB-INDEX TO WS-OCCURRENCE-SUB.                          ELGCOINS
00736      SET WS-OCCURRENCE-INDEX TO WS-OCCURRENCE-SUB.                ELGCOINS
00737      IF WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB)               ELGCOINS
00738          CONTINUE                                                 ELGCOINS
00739      ELSE PERFORM 0400-TEST-OCCURRENCE.                           ELGCOINS
00740                                                                   ELGCOINS
00741 /***********************************************************      ELGCOINS
00742 *                                                          *      ELGCOINS
00743 *        TEST OCCURRENCE                                   *      ELGCOINS
00744 *                                                          *      ELGCOINS
00745 ************************************************************      ELGCOINS
00746                                                                   ELGCOINS
00747  0400-TEST-OCCURRENCE.                                            ELGCOINS
00748      SET SW-MATCHING-ENTRY-NOT-FOUND TO TRUE.                     ELGCOINS
00749      PERFORM 0410-TEST-KEYS-FOR-MATCH.                            ELGCOINS
00750      IF SW-MATCHING-ENTRY-FOUND                                   ELGCOINS
00751          PERFORM 0420-COMPLETE-TEST-OF-OCCURNCE.                  ELGCOINS
00752                                                                   ELGCOINS
00753 /***********************************************************      ELGCOINS
00754 *                                                          *      ELGCOINS
00755 *        TEST KEYS FOR MATCH                               *      ELGCOINS
00756 *                                                          *      ELGCOINS
00757 ************************************************************      ELGCOINS
00758                                                                   ELGCOINS
00759  0410-TEST-KEYS-FOR-MATCH.                                        ELGCOINS
00760      IF GAB-COINS-FYI-VALUE (GAB-INDEX) =                         ELGCOINS
00761             ACCUM-FYI-VALUE                                       ELGCOINS
00762                    AND                                            ELGCOINS
00763         GAB-COINS-COST-CONTAIN-IND (GAB-INDEX) =                  ELGCOINS
00764             ACCUM-COST-CONTAIN-IND                                ELGCOINS
00765                    AND                                            ELGCOINS
00766         GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX) =                ELGCOINS
00767             ACCUM-PLACE-OF-TREATMENT                              ELGCOINS
00768                    AND                                            ELGCOINS
00769         GAB-COINS-BENEFIT-PERIOD (GAB-INDEX) =                    ELGCOINS
00770              ACCUM-BENEFIT-PERIOD                                 ELGCOINS
00771                    AND                                            ELGCOINS
00772         GAB-COINS-L-O-B (GAB-INDEX) =                             ELGCOINS
00773             ACCUM-L-O-B                                           ELGCOINS
00774                    AND                                            ELGCOINS
00775         GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX) =                ELGCOINS
00776             ACCUM-ASCEND-DESCEND-IND                              ELGCOINS
00777                    AND                                            ELGCOINS
00778               GAB-COND-ALL-BIT (GAB-INDEX) =                      ELGCOINS
00779             ACCUM-COND-ALL-BIT                                    ELGCOINS
00780                    AND                                            ELGCOINS
00781               GAB-COND-EXCLUSION-BIT (GAB-INDEX) =                ELGCOINS
00782             ACCUM-COND-EXCLUSION-BIT                              ELGCOINS
00783                    AND                                            ELGCOINS
00784               GAB-COND-ICD-BIT (GAB-INDEX) =                      ELGCOINS
00785             ACCUM-COND-ICD-BIT                                    ELGCOINS
00786                    AND                                            ELGCOINS
00787               GAB-COND-TB-BIT (GAB-INDEX) =                       ELGCOINS
00788             ACCUM-COND-TB-BIT                                     ELGCOINS
00789                    AND                                            ELGCOINS
00790               GAB-COND-MENTAL-BIT (GAB-INDEX) =                   ELGCOINS
00791             ACCUM-COND-MENTAL-BIT                                 ELGCOINS
00792                    AND                                            ELGCOINS
00793               GAB-COND-DRUG-BIT (GAB-INDEX) =                     ELGCOINS
00794             ACCUM-COND-DRUG-BIT                                   ELGCOINS
00795                    AND                                            ELGCOINS
00796               GAB-COND-ALCOHOL-BIT (GAB-INDEX) =                  ELGCOINS
00797             ACCUM-COND-ALCOHOL-BIT                                ELGCOINS
00798                    AND                                            ELGCOINS
00799               GAB-COND-OB-COMP-BIT (GAB-INDEX) =                  ELGCOINS
00800             ACCUM-COND-OB-COMP-BIT                                ELGCOINS
00801                    AND                                            ELGCOINS
00802               GAB-COND-OB-NORM-BIT (GAB-INDEX) =                  ELGCOINS
00803             ACCUM-COND-OB-NORM-BIT                                ELGCOINS
00804                    AND                                            ELGCOINS
00805               GAB-COND-MALIGNANCY-BIT (GAB-INDEX) =               ELGCOINS
00806             ACCUM-COND-MALIGNANCY-BIT                             ELGCOINS
00807                    AND                                            ELGCOINS
00808               GAB-COND-CARDIAC-DISEASE-BIT (GAB-INDEX) =          ELGCOINS
00809             ACCUM-COND-CARDIAC-DISEASE-BIT                        ELGCOINS
00810                    AND                                            ELGCOINS
00811               GAB-COND-OBESITY-BIT (GAB-INDEX) =                  ELGCOINS
00812             ACCUM-COND-OBESITY-BIT                                ELGCOINS
00813                    AND                                            ELGCOINS
00814               GAB-COND-KIDNEY-DISEASE-BIT (GAB-INDEX) =           ELGCOINS
00815             ACCUM-COND-KIDNEY-DISEASE-BIT                         ELGCOINS
00816                    AND                                            ELGCOINS
00817               GAB-COND-ACCIDENT-BIT (GAB-INDEX) =                 ELGCOINS
00818             ACCUM-COND-ACCIDENT-BIT                               ELGCOINS
00819                    AND                                            ELGCOINS
00820               GAB-COND-PRE-EXIST-BIT (GAB-INDEX) =                ELGCOINS
00821             ACCUM-COND-PRE-EXIST-BIT                              ELGCOINS
00822                    AND                                            ELGCOINS
00823               GAB-COND-NON-EMER-BIT (GAB-INDEX) =                 ELGCOINS
00824             ACCUM-COND-NON-EMER-BIT                               ELGCOINS
00825                    AND                                            ELGCOINS
00826               GAB-COND-SUICIDE-BIT (GAB-INDEX) =                  ELGCOINS
00827             ACCUM-COND-SUICIDE-BIT                                ELGCOINS
00828                    AND                                            ELGCOINS
00829               GAB-COND-TMJ-BIT (GAB-INDEX) =                      ELGCOINS
00830             ACCUM-COND-TMJ-BIT                                    ELGCOINS
00831                    AND                                            ELGCOINS
00832               GAB-COND-INF-BIT (GAB-INDEX) =                      ELGCOINS
00833             ACCUM-COND-INF-BIT                                    ELGCOINS
00834                    AND                                            ELGCOINS
00835               GAB-COND-LIFE-THREAT-BIT (GAB-INDEX) =              ELGCOINS
00836             ACCUM-COND-LIFE-THREAT-BIT                            ELGCOINS
00837                    AND                                            ELGCOINS
00838         GAB-COINS-FAM-OR-INDIV (GAB-INDEX) =                      ELGCOINS
00839             ACCUM-FAM-OR-INDIV                                    ELGCOINS
00840                    AND                                            ELGCOINS
00841          GAB-COINS-VALUE-QUALIFIER (GAB-INDEX) =                  ELGCOINS
00842              ACCUM-VALUE-QUALIFIER                                ELGCOINS
00843                    AND                                            ELGCOINS
00844          GAB-COINS-RELATIONSHIP-IND (GAB-INDEX) =                 ELGCOINS
00845              ACCUM-RELATIONSHIP-IND                               ELGCOINS
00846                    AND                                            ELGCOINS
00847          GAB-COINS-AGE-LIMIT-FROM (GAB-INDEX) =                   ELGCOINS
00848              ACCUM-AGE-LIMIT-FROM-VAL                             ELGCOINS
00849                    AND                                            ELGCOINS
00850          GAB-COINS-AGE-LIMIT-TO (GAB-INDEX) =                     ELGCOINS
00851              ACCUM-AGE-LIMIT-TO-VAL                               ELGCOINS
00852                    AND                                            ELGCOINS
00853          GAB-COINS-AGE-QUAL-IND-FROM (GAB-INDEX) =                ELGCOINS
00854              ACCUM-AGE-LIMIT-FROM-IND                             ELGCOINS
00855                    AND                                            ELGCOINS
00856          GAB-COINS-AGE-QUAL-IND-TO (GAB-INDEX) =                  ELGCOINS
00857              ACCUM-AGE-LIMIT-TO-IND                               ELGCOINS
00858                    AND                                            ELGCOINS
00859          GAB-COINS-LMT-MANDATORY-IND (GAB-INDEX) =                ELGCOINS
00860              ACCUM-LMT-MANDATORY-IND                              ELGCOINS
00861                    AND                                            ELGCOINS
00862          GAB-COINS-CO-PAY-IND (GAB-INDEX) =                       ELGCOINS
00863              ACCUM-CO-PAY-IND (COPAY-INDEX)                       ELGCOINS
00864                    AND                                            ELGCOINS
00865          GAB-COINS-SERVICE-GROUP (GAB-INDEX) =                    ELGCOINS
00866              ACCUM-SERVICE-GROUP                                  ELGCOINS
00867                    AND                                            ELGCOINS
00868          GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX) =              ELGCOINS
00869              ACCUM-INTERNAL-DESCRIPTOR                            ELGCOINS
00870       THEN                                                        ELGCOINS
00871          SET SW-MATCHING-ENTRY-FOUND TO TRUE.                     ELGCOINS
00872                                                                   ELGCOINS
00873                                                                   ELGCOINS
00874 /***********************************************************      ELGCOINS
00875 *                                                          *      ELGCOINS
00876 *        COMPLETE TEST OF OCCURRENCE                       *      ELGCOINS
00877 *                                                          *      ELGCOINS
00878 ************************************************************      ELGCOINS
00879                                                                   ELGCOINS
00880  0420-COMPLETE-TEST-OF-OCCURNCE.                                  ELGCOINS
00881      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELGCOINS
00882      PERFORM 0320-SCAN-FOR-INTERNALS.                             ELGCOINS
00883      IF SW-OCCRNC-APPLIES                                         ELGCOINS
00884         PERFORM 0430-EXTRACT-NEXT-OCCURRENCE                      ELGCOINS
00885      ELSE SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB) TO TRUE.ELGCOINS
00886                                                                   ELGCOINS
00887                                                                   ELGCOINS
00888 ************************************************************      ELGCOINS
00889 *                                                          *      ELGCOINS
00890 *        EXTRACT NEXT OCCURRENCE                           *      ELGCOINS
00891 *                                                          *      ELGCOINS
00892 ************************************************************      ELGCOINS
00893                                                                   ELGCOINS
00894  0430-EXTRACT-NEXT-OCCURRENCE.                                    ELGCOINS
00895      ADD +1 TO ACCUM-ASCEND-DESCEND-COUNT.                        ELGCOINS
00896      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELGCOINS
00897      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (ASC-DES-INDEX).       ELGCOINS
00898      PERFORM 0370-EXTRACT-VARIABLE-PORTION.                       ELGCOINS
00899      PERFORM 0440-INSERT-NEW-ENTRY.                               ELGCOINS
00900                                                                   ELGCOINS
00901                                                                   ELGCOINS
00902 /***********************************************************      ELGCOINS
00903 *                                                          *      ELGCOINS
00904 *        INSERT NEW ENTRY                                  *      ELGCOINS
00905 *                                                          *      ELGCOINS
00906 ************************************************************      ELGCOINS
00907                                                                   ELGCOINS
00908  0440-INSERT-NEW-ENTRY.                                           ELGCOINS
00909      MOVE ACCUM-ASCEND-DESCEND-COUNT TO SORT-SUB.                 ELGCOINS
00910      SET SW-SORT-NOT-COMPLETED TO TRUE.                           ELGCOINS
00911      IF SORT-SUB = 1                                              ELGCOINS
00912         CONTINUE                                                  ELGCOINS
00913      ELSE IF ACCUM-ASCEND-ORDER                                   ELGCOINS
00914              PERFORM 0450-ASCEND-INSERT                           ELGCOINS
00915                UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1            ELGCOINS
00916           ELSE IF ACCUM-DESCEND-ORDER                             ELGCOINS
00917                   PERFORM 0460-DESCEND-INSERT                     ELGCOINS
00918                     UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1       ELGCOINS
00919                ELSE IF ACCUM-BISCEND-ORDER                        ELGCOINS
00920                        PERFORM 0470-BISCEND-INSERT                ELGCOINS
00921                          UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1. ELGCOINS
00922                                                                   ELGCOINS
00923                                                                   ELGCOINS
00924 /***********************************************************      ELGCOINS
00925 *                                                          *      ELGCOINS
00926 *        ASCEND INSERT                                     *      ELGCOINS
00927 *                                                          *      ELGCOINS
00928 ************************************************************      ELGCOINS
00929                                                                   ELGCOINS
00930  0450-ASCEND-INSERT.                                              ELGCOINS
00931      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELGCOINS
00932      IF ACCUM-PERCENT-LEVEL (SORT-SUB) <                          ELGCOINS
00933         ACCUM-PERCENT-LEVEL (TEST-SUB)                            ELGCOINS
00934         PERFORM 0480-SWAP-ENTRIES                                 ELGCOINS
00935      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELGCOINS
00936                                                                   ELGCOINS
00937                                                                   ELGCOINS
00938 ************************************************************      ELGCOINS
00939 *                                                          *      ELGCOINS
00940 *        DESCEND INSERT                                    *      ELGCOINS
00941 *                                                          *      ELGCOINS
00942 ************************************************************      ELGCOINS
00943                                                                   ELGCOINS
00944  0460-DESCEND-INSERT.                                             ELGCOINS
00945      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELGCOINS
00946      IF ACCUM-PERCENT-LEVEL (SORT-SUB) >                          ELGCOINS
00947         ACCUM-PERCENT-LEVEL (TEST-SUB)                            ELGCOINS
00948         PERFORM 0480-SWAP-ENTRIES                                 ELGCOINS
00949      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELGCOINS
00950                                                                   ELGCOINS
00951                                                                   ELGCOINS
00952 /***********************************************************      ELGCOINS
00953 *                                                          *      ELGCOINS
00954 *        BISCEND INSERT                                    *      ELGCOINS
00955 *                                                          *      ELGCOINS
00956 ************************************************************      ELGCOINS
00957                                                                   ELGCOINS
00958  0470-BISCEND-INSERT.                                             ELGCOINS
00959      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELGCOINS
00960      IF ACCUM-BISCEND-IND (SORT-SUB) <                            ELGCOINS
00961         ACCUM-BISCEND-IND (TEST-SUB)                              ELGCOINS
00962         PERFORM 0480-SWAP-ENTRIES                                 ELGCOINS
00963      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELGCOINS
00964                                                                   ELGCOINS
00965                                                                   ELGCOINS
00966 ************************************************************      ELGCOINS
00967 *                                                          *      ELGCOINS
00968 *        SWAP ENTRIES                                      *      ELGCOINS
00969 *                                                          *      ELGCOINS
00970 ************************************************************      ELGCOINS
00971                                                                   ELGCOINS
00972  0480-SWAP-ENTRIES.                                               ELGCOINS
00973      MOVE ACCUM-ASCEND-DESCEND-ENTRY (SORT-SUB)                   ELGCOINS
00974        TO WS-ASCEND-DESCEND-ENTRY-HOLD.                           ELGCOINS
00975      MOVE ACCUM-ASCEND-DESCEND-ENTRY (TEST-SUB)                   ELGCOINS
00976        TO ACCUM-ASCEND-DESCEND-ENTRY (SORT-SUB).                  ELGCOINS
00977      MOVE WS-ASCEND-DESCEND-ENTRY-HOLD                            ELGCOINS
00978        TO ACCUM-ASCEND-DESCEND-ENTRY (TEST-SUB).                  ELGCOINS
00979      MOVE TEST-SUB TO SORT-SUB.                                   ELGCOINS
00980                                                                   ELGCOINS
00981                                                                   ELGCOINS
00982 /***********************************************************      ELGCOINS
00983 *                                                          *      ELGCOINS
00984 *    CHECK EXTRACT DATA INTEGRITY                          *      ELGCOINS
00985 *                                                          *      ELGCOINS
00986 ************************************************************      ELGCOINS
00987                                                                   ELGCOINS
00988  0490-CHK-EXTRACT-DATA-INTGRTY.                                   ELGCOINS
00989      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELGCOINS
00990      THEN                                                         ELGCOINS
00991           SET FYI-VALUE-NA TO TRUE                                ELGCOINS
00992      END-IF.                                                      ELGCOINS
00993                                                                   ELGCOINS
00994      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELGCOINS
00995      THEN                                                         ELGCOINS
00996           SET COST-CONTAIN-IND-NA TO TRUE                         ELGCOINS
00997      END-IF.                                                      ELGCOINS
00998                                                                   ELGCOINS
00999      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELGCOINS
01000      THEN                                                         ELGCOINS
01001           SET PLACE-OF-TREATMENT-NA TO TRUE                       ELGCOINS
01002      END-IF.                                                      ELGCOINS
01003                                                                   ELGCOINS
01004      IF ACCUM-BENEFIT-PERIOD = ZEROS OR SPACES OR LOW-VALUES      ELGCOINS
01005      THEN                                                         ELGCOINS
01006         SET   BENEFIT-PERIOD-NA TO TRUE                           ELGCOINS
01007      END-IF.                                                      ELGCOINS
01008                                                                   ELGCOINS
01009      IF ACCUM-BEN-PER-TIME-QUAL = ZEROS OR SPACES OR LOW-VALUES   ELGCOINS
01010      THEN                                                         ELGCOINS
01011         SET   BEN-PER-TIME-QUAL-NA TO TRUE                        ELGCOINS
01012      END-IF.                                                      ELGCOINS
01013                                                                   ELGCOINS
01014      IF ACCUM-INTERVAL-TYPE = ZEROS OR SPACES OR LOW-VALUES       ELGCOINS
01015      THEN                                                         ELGCOINS
01016         SET   INTERVAL-TYPE-NA TO TRUE                            ELGCOINS
01017      END-IF.                                                      ELGCOINS
01018                                                                   ELGCOINS
01019      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELGCOINS
01020      THEN                                                         ELGCOINS
01021         SET   INTERVAL-OVRD-IND-NA TO TRUE                        ELGCOINS
01022      END-IF.                                                      ELGCOINS
01023                                                                   ELGCOINS
01024      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELGCOINS
01025      THEN                                                         ELGCOINS
01026         SET   L-O-B-NA TO TRUE                                    ELGCOINS
01027      END-IF.                                                      ELGCOINS
01028                                                                   ELGCOINS
01029      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELGCOINS
01030      THEN                                                         ELGCOINS
01031         SET   REINSTATEMENT-IND-NA TO TRUE                        ELGCOINS
01032      END-IF.                                                      ELGCOINS
01033                                                                   ELGCOINS
01034      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELGCOINS
01035      THEN                                                         ELGCOINS
01036         SET   DEFINITION-NA TO TRUE                               ELGCOINS
01037      END-IF.                                                      ELGCOINS
01038                                                                   ELGCOINS
01039      IF   ACCUM-CARRY-OVER-CREDIT-IND                             ELGCOINS
01040         = ZEROS OR SPACES OR LOW-VALUES                           ELGCOINS
01041      THEN                                                         ELGCOINS
01042         SET     CARRY-OVER-CREDIT-IND-NA TO TRUE                  ELGCOINS
01043      END-IF.                                                      ELGCOINS
01044                                                                   ELGCOINS
01045      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELGCOINS
01046      THEN                                                         ELGCOINS
01047         SET   ASCEND-DESCEND-IND-NA TO TRUE                       ELGCOINS
01048      END-IF.                                                      ELGCOINS
01049                                                                   ELGCOINS
01050      IF ACCUM-FAM-OR-INDIV = ZEROS OR SPACES OR LOW-VALUES        ELGCOINS
01051      THEN                                                         ELGCOINS
01052         SET   FAM-OR-INDIV-NA TO TRUE                             ELGCOINS
01053      END-IF.                                                      ELGCOINS
01054                                                                   ELGCOINS
01055      IF ACCUM-VALUE-QUALIFIER = ZEROS OR SPACES OR LOW-VALUES     ELGCOINS
01056      THEN                                                         ELGCOINS
01057         SET   VALUE-QUALIFIER-NA TO TRUE                          ELGCOINS
01058      END-IF.                                                      ELGCOINS
01059                                                                   ELGCOINS
01060      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELGCOINS
01061      THEN                                                         ELGCOINS
01062         SET   RELATIONSHIP-IND-NA TO TRUE                         ELGCOINS
01063      END-IF.                                                      ELGCOINS
01064                                                                   ELGCOINS
01065      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELGCOINS
01066      THEN                                                         ELGCOINS
01067         SET   AGE-LMT-FROM-IND-NA TO TRUE                         ELGCOINS
01068      END-IF.                                                      ELGCOINS
01069                                                                   ELGCOINS
01070      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELGCOINS
01071      THEN                                                         ELGCOINS
01072         SET   AGE-LMT-TO-IND-NA TO TRUE                           ELGCOINS
01073      END-IF.                                                      ELGCOINS
01074                                                                   ELGCOINS
01075      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELGCOINS
01076      THEN                                                         ELGCOINS
01077         SET   LMT-MANDATORY-IND-NA TO TRUE                        ELGCOINS
01078      END-IF.                                                      ELGCOINS
01079                                                                   ELGCOINS
01080      IF ACCUM-CO-PAY-IND (COPAY-INDEX)                            ELGCOINS
01081                      = ZEROS OR SPACES OR LOW-VALUES              ELGCOINS
01082      THEN                                                         ELGCOINS
01083         SET   CO-PAY-IND-NA (COPAY-INDEX) TO TRUE                 ELGCOINS
01084      END-IF.                                                      ELGCOINS
01085                                                                   ELGCOINS
01086      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELGCOINS
01087      THEN                                                         ELGCOINS
01088         SET   SERVICE-GROUP-NA TO TRUE                            ELGCOINS
01089      END-IF.                                                      ELGCOINS
01090                                                                   ELGCOINS
01091      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELGCOINS
01092      THEN                                                         ELGCOINS
01093         SET   INTERNAL-DESCRIPTOR-NA TO TRUE                      ELGCOINS
01094      END-IF.                                                      ELGCOINS
01095                                                                   ELGCOINS
01096      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELGCOINS
01097      THEN                                                         ELGCOINS
01098         SET   DAY-FACTOR-IND-NA TO TRUE                           ELGCOINS
01099      END-IF.                                                      ELGCOINS
01100                                                                   ELGCOINS
01101      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELGCOINS
01102      THEN                                                         ELGCOINS
01103         SET   CLAIM-LVL-ACCUM-IND-NA TO TRUE                      ELGCOINS
01104      END-IF.                                                      ELGCOINS
01105                                                                   ELGCOINS
01106      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELGCOINS
01107      THEN                                                         ELGCOINS
01108         SET   BEN-PER-MAX-OVRD-IND-NA TO TRUE                     ELGCOINS
01109      END-IF.                                                      ELGCOINS
01110                                                                   ELGCOINS
01111      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELGCOINS
01112      THEN                                                         ELGCOINS
01113         SET   1ST-DOLR-COVRGE-LMT-NA TO TRUE                      ELGCOINS
01114      END-IF.                                                      ELGCOINS
01115                                                                   ELGCOINS
01116 /***********************************************************      ELGCOINS
01117 *                                                          *      ELGCOINS
01118 *        ADD ACCUM OCCURRENCE TO FILE                      *      ELGCOINS
01119 *                                                          *      ELGCOINS
01120 ************************************************************      ELGCOINS
01121                                                                   ELGCOINS
01122  0710-WRITE-EXTRACT-RECORD.                                       ELGCOINS
01123      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGCOINS
01124      SET  IOP-ADD TO TRUE.                                        ELGCOINS
01125      SET  IOP-FCQ-NONE TO TRUE.                                   ELGCOINS
01126      SET  IOP-KVQ-NONE TO TRUE.                                   ELGCOINS
01127      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGCOINS
01128                                                                   ELGCOINS
01129 /***********************************************************      ELGCOINS
01130 *                                                          *      ELGCOINS
01131 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELGCOINS
01132 *                                                          *      ELGCOINS
01133 ************************************************************      ELGCOINS
01134                                                                   ELGCOINS
01135  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELGCOINS
01136      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGCOINS
01137      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOINS
01138         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGCOINS
01139         END-CALL.                                                 ELGCOINS
01140      IF CIA-RC-PTR-NULL                                           ELGCOINS
01141         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGCOINS
01142         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGCOINS
01143      END-IF.                                                      ELGCOINS
