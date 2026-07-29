00001  IDENTIFICATION DIVISION.                                         09/04/03
00002                                                                   ELTADL  
00003  PROGRAM-ID.        ELTADL.                                          LV002
00004                                                                   ELTADL  
00005  AUTHOR.            GEORGE E MOORE.                               ELTADL  
00006                                                                   ELTADL  
00007  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELTADL  
00008                     A MUTUAL LEGAL RESERVE COMPANY                ELTADL  
00009                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELTADL  
00010                     233 N. MICHIGAN AVE                           ELTADL  
00011                     CHICAGO, ILLINOIS 60601                       ELTADL  
00012                                                                   ELTADL  
00013  DATE-WRITTEN.      26-FEB-1992.                                  ELTADL  
00014  DATE-COMPILED.                                                   ELTADL  
00015                                                                   ELTADL  
00016  SECURITY.          COPYRIGHT 1986, 1992,                         ELTADL  
00017                     HEALTH CARE SERVICE CORPORATION               ELTADL  
00018                                                                   ELTADL  
00019  ENVIRONMENT DIVISION.                                            ELTADL  
00020                                                                   ELTADL  
00021  CONFIGURATION SECTION.                                           ELTADL  
00022  SOURCE-COMPUTER.    IBM-3090.                                    ELTADL  
00023  OBJECT-COMPUTER.    IBM-3090.                                    ELTADL  
00024                                                                   ELTADL  
00025 /*****************************************************************ELTADL  
00026 *                                                                *ELTADL  
00027 *  ELTADL - ELS:    SELECTS #ADL (DEDUCTIBLE) ACCUMULATORS AND   *ELTADL  
00028 *                   SETUPS THE INFORMATION TO BE PROCESSED BY    *ELTADL  
00029 *                   THE DEDUCTIBLE GENERATOR MODULE. THE ACCUMS  *ELTADL  
00030 *                   ARE SELECTED FROM THE GROUP SPECIFIC AND     *ELTADL  
00031 *                   CONTRACT LEVEL PROCESSING.                   *ELTADL  
00032 *                                                                *ELTADL  
00033 ******************************************************************ELTADL  
00034 *                                                                *ELTADL  
00035 *                      MAINTENANCE HISTORY                       *ELTADL  
00036 *                                                                *ELTADL  
00037 *  MOD     DATE     BY  DRPT                ACTION               *ELTADL  
00038 * ----- ----------- --- ----- ---------------------------------- *ELTADL  
00039 * 01.00 26-FEB-1992 GEM       CLONED FROM VERSION OF ELTABM,     *ELTADL  
00040 *                             THAT WAS WRITTEN BY RJL.           *ELTADL  
00041 * 02.00 02-DEC-1998 AKK       ADD SUPPORT FOR #ACP TABULAR.      *ELTADL  
00042 *                                                                *ELTADL  
00043 * 02.01 25-AUG-2000 AKK       ADD SUPPORT FOR #IPGS TABULAR.      ELTADL  
00044 *                                                                *ELTADL  
00045 *       19-AUG-2003 AKK       RECOMPILING TO REMOVE RC 12 FROM    ELTADL  
00046 *                             MASERLIST, NEED NEW LEVEL          *ELTADL  
00044 *                                                                *ELTADL  
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00056 *                                                                *ELGABMCC
00047 ******************************************************************ELTADL  
00048 /                                                                 ELTADL  
00049  DATA DIVISION.                                                   ELTADL  
00050                                                                   ELTADL  
00051  WORKING-STORAGE SECTION.                                         ELTADL  
00052                                                                   ELTADL  
00053  01  SWITCHES.                                                    ELTADL  
00054      02                                      PICTURE  X(01).      ELTADL  
00055         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELTADL  
00056         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELTADL  
00057                                                                   ELTADL  
00058      02 OCCURRENCE-APPLIES                   PICTURE  X(01).      ELTADL  
00059         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELTADL  
00060         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELTADL  
00061      02                                      PICTURE  X(01).      ELTADL  
00062         88 SW-HAS-IBGR                       VALUE 'Y'.           ELTADL  
00063         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELTADL  
00064      02                                      PICTURE  X(01).      ELTADL  
00065         88 SW-HAS-IDGD                       VALUE 'Y'.           ELTADL  
00066         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELTADL  
00067      02                                      PICTURE  X(01).      ELTADL  
00068         88 SW-HAS-IPGN                       VALUE 'Y'.           ELTADL  
00069         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELTADL  
00070      02                                      PICTURE  X(01).      ELTADL  
00071         88 SW-HAS-IPGP                       VALUE 'Y'.           ELTADL  
00072         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELTADL  
00073      02                                      PICTURE  X(01).      ELTADL  
00074         88 SW-HAS-IPGT                       VALUE 'Y'.           ELTADL  
00075         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELTADL  
00076      02                                      PICTURE  X(01).      ELTADL  
00077         88 SW-HAS-IPGS                       VALUE 'Y'.           ELTADL  
00078         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELTADL  
00079      02                                      PICTURE  X(01).      ELTADL  
00080         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELTADL  
00081         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELTADL  
00082      02                                      PICTURE  X(01).      ELTADL  
00083         88 SW-INTRNL-INST-PROV-CL            VALUE 'Y'.           ELTADL  
00084         88 SW-INTRNL-NOT-INST-PROV-CL        VALUE 'N'.           ELTADL  
00085         88 SW-INTRNL-INST-PROV-CL-NOT-DET VALUE 'X'.              ELTADL  
00086      02                                      PICTURE  X(01).      ELTADL  
00087         88 SW-INTRNL-PROF-PROV-CL            VALUE 'Y'.           ELTADL  
00088         88 SW-INTRNL-NOT-PROF-PROV-CL        VALUE 'N'.           ELTADL  
00089         88 SW-INTRNL-PROF-PROV-CL-NOT-DET VALUE 'X'.              ELTADL  
00090      02                                      PICTURE  X(01).      ELTADL  
00091         88 SW-INTRNL-PROF-PROV-SP            VALUE 'Y'.           ELTADL  
00092         88 SW-INTRNL-NOT-PROF-PROV-SP        VALUE 'N'.           ELTADL  
00093         88 SW-INTRNL-PROF-PROV-SP-NOT-DET VALUE 'X'.              ELTADL  
00094      02                                      PICTURE  X(01).      ELTADL  
00095         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELTADL  
00096         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELTADL  
00097                                                                   ELTADL  
00098  01  WS-PROVISION-ARGUMENT.                                       ELTADL  
00099      02                          PICTURE  X(05).                  ELTADL  
00100      02 WS-PROVISION-CL          PICTURE  X(01).                  ELTADL  
00101         88 INST-CL               VALUE 'A', 'B', 'W'.             ELTADL  
00102         88 PROF-CL               VALUE 'C', 'D', 'E'.             ELTADL  
00103                                                                   ELTADL  
00104  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELTADL  
00105      88 WS-LOB-INST              VALUE '1'.                       ELTADL  
00106      88 WS-LOB-PROF              VALUE '2'.                       ELTADL  
00107      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELTADL  
00108      88 WS-LOB-BOTH              VALUE '3', '4', '5', '6', '7'.   ELTADL  
00109                                                                   ELTADL  
00110  01  PROGRAM-CONSTANTS.                                           ELTADL  
00111      02 PC-ADL                   PICTURE  X(06) VALUE '#ADL  '.   ELTADL  
00112      02 PC-GCT-MAX-SUB           PICTURE S9(04) COMP.             ELTADL  
00113      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELTADL  
00114      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELTADL  
00115      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELTADL  
00116      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELTADL  
00117      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELTADL  
00118      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGS '.   ELTADL  
00119      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELTADL  
00120                                                                   ELTADL  
00121  01  WS-WORK-FIELDS.                                              ELTADL  
00122      02 WS-ADL-SUB               PICTURE S9(04) COMP.             ELTADL  
00123      02 WS-ADL-ACCUM-CNT         PICTURE S9(04) COMP.             ELTADL  
00124      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELTADL  
00125                                                                   ELTADL  
00126  01  WS-POINTERS.                                                 ELTADL  
00127      02  WS-INST-CNTRCT-PTR      POINTER.                         ELTADL  
00128      02  WS-PROF-CNTRCT-PTR      POINTER.                         ELTADL  
00129                                                                   ELTADL  
00130  01  WS-MAX-INDEX-VALUES.                                         ELTADL  
00131      02 WS-MAX-GX1-INDEX         USAGE IS INDEX.                  ELTADL  
00132      02 WS-MAX-GX3-INDEX         USAGE IS INDEX.                  ELTADL  
00133      02 WS-MAX-GXS-INDEX         USAGE IS INDEX.                  ELTADL  
00134      02 WS-MAX-GAC-INDEX         USAGE IS INDEX.                  ELTADL  
00135      02 WS-MAX-GCT-INDEX         USAGE IS INDEX.                  ELTADL  
00136      02 WS-MAX-GCG-INDEX         USAGE IS INDEX.                  ELTADL  
00137                                                                   ELTADL  
00138  01  ACCUM-HOLD-TBL.                                              ELTADL  
00139      02  ACCUM-SLOT-NBR          PICTURE S9(07) COMP-3            ELTADL  
00140                                  OCCURS 5 TIMES.                  ELTADL  
00141                                                                   ELTADL  
00142  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELTADL  
00143      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTADL  
00144      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTADL  
00145      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTADL  
00146      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTADL  
00147      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTADL  
00148      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTADL  
00149                                                                   ELTADL  
00150 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELTADL  
00151      COPY ELSCFTB2.                                               ELTADL  
00152                                                                   ELTADL  
00153 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELTADL  
00154      COPY ELSCFTB9.                                               ELTADL  
00155                                                                   ELTADL  
00156      TITLE  'ELTADL          LINKAGE SECTION'                     ELTADL  
00157  LINKAGE SECTION.                                                 ELTADL  
00158  01  DFHCOMMAREA.                                                 ELTADL  
00159      COPY ELSCOMMC.                                               ELTADL  
00160 /                                                                 ELTADL  
00161      COPY ELSCIA2C.                                               ELTADL  
00162 /                                                                 ELTADL  
00163      COPY ELSIOPMC.                                               ELTADL  
00164 /                                                                 ELTADL  
00165      COPY ELSKEYSC.                                               ELTADL  
00166 /                                                                 ELTADL  
00167      COPY ELSSRTPC.                                               ELTADL  
00168 /                                                                 ELTADL  
00169      COPY ELSSSCBC.                                               ELTADL  
00170 /                                                                 ELTADL  
00171  01  GCG-GRP-SPEC-RECORD-AREA.                                    ELTADL  
00172      COPY GCGROUPC.                                               ELTADL  
00173 /                                                                 ELTADL  
00174  01  GCT-CONTRACT-RECORD-AREA.                                    ELTADL  
00175      COPY GCCONTRC.                                               ELTADL  
00176 /                                                                 ELTADL  
00177  01  GAC-RECORD-AREA.                                             ELTADL  
00178      COPY GCTADLC.                                                ELTADL  
00179 /                                                                 ELTADL  
00180      COPY ELSACUMC.                                               ELTADL  
00181 /                                                                 ELTADL  
00182  01  GX1-RECORD-AREA.                                             ELTADL  
00183      COPY GCTIBGRC.                                               ELTADL  
00184 /                                                                 ELTADL  
00185  01  GX3-RECORD-AREA.                                             ELTADL  
00186      COPY GCTIPGTC.                                               ELTADL  
00187 /                                                                 ELTADL  
00188  01  GXS-RECORD-AREA.                                             ELTADL  
00189      COPY GCTIPGSC.                                               ELTADL  
00190 /    TITLE  'ELTADL          PROCEDURE DIVISION'.                 ELTADL  
00191 ************************************************************      ELTADL  
00192 *                                                          *      ELTADL  
00193 *    ELTADL MAINLINE                                       *      ELTADL  
00194 *                                                          *      ELTADL  
00195 ************************************************************      ELTADL  
00196                                                                   ELTADL  
00197  PROCEDURE DIVISION.                                              ELTADL  
00198                                                                   ELTADL  
00199      PERFORM 0010-INITIALIZATION.                                 ELTADL  
00200      PERFORM 0100-PROCESS.                                        ELTADL  
00201      GOBACK.                                                      ELTADL  
00202                                                                   ELTADL  
00203 ************************************************************      ELTADL  
00204 *                                                          *      ELTADL  
00205 *    INITIALIZATION                                        *      ELTADL  
00206 *                                                          *      ELTADL  
00207 ************************************************************      ELTADL  
00208                                                                   ELTADL  
00209  0010-INITIALIZATION.                                             ELTADL  
00210      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELTADL  
00211      PERFORM 0030-EST-ADR-KEY-WK-AREA.                            ELTADL  
00212      PERFORM 0040-EST-ADR-OF-SUBROUTINE-PAR.                      ELTADL  
00213      PERFORM 0050-EST-ADR-GRP-SPC.                                ELTADL  
00214      PERFORM 0060-INIT-DATA.                                      ELTADL  
00215                                                                   ELTADL  
00216 /***********************************************************      ELTADL  
00217 *                                                          *      ELTADL  
00218 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELTADL  
00219 *                                                          *      ELTADL  
00220 ************************************************************      ELTADL  
00221                                                                   ELTADL  
00222  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELTADL  
00223      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTADL  
00224      THEN                                                         ELTADL  
00225         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELTADL  
00226      ELSE                                                         ELTADL  
00227         IF ECA-CIA-PTR = NULL                                     ELTADL  
00228         THEN                                                      ELTADL  
00229            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELTADL  
00230         ELSE                                                      ELTADL  
00231            CALL 'ELUINISM' USING DFHCOMMAREA                      ELTADL  
00232               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELTADL  
00233               END-CALL                                            ELTADL  
00234            SET CIA-ELSSSCB-DDN TO TRUE                            ELTADL  
00235            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELTADL  
00236               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELTADL  
00237               END-CALL                                            ELTADL  
00238            IF CIA-RC-PTR-NULL                                     ELTADL  
00239            THEN                                                   ELTADL  
00240               PERFORM 0099-CIA-AB-UNALLOC-AREA                    ELTADL  
00241            ELSE                                                   ELTADL  
00242               CONTINUE                                            ELTADL  
00243            END-IF                                                 ELTADL  
00244         END-IF                                                    ELTADL  
00245      END-IF.                                                      ELTADL  
00246                                                                   ELTADL  
00247 /***********************************************************      ELTADL  
00248 *                                                          *      ELTADL  
00249 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELTADL  
00250 *                                                          *      ELTADL  
00251 ************************************************************      ELTADL  
00252                                                                   ELTADL  
00253  0030-EST-ADR-KEY-WK-AREA.                                        ELTADL  
00254      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTADL  
00255      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTADL  
00256         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELTADL  
00257         END-CALL.                                                 ELTADL  
00258      IF CIA-RC-PTR-NULL                                           ELTADL  
00259         PERFORM 0099-CIA-AB-UNALLOC-AREA.                         ELTADL  
00260                                                                   ELTADL  
00261 ************************************************************      ELTADL  
00262 *                                                          *      ELTADL  
00263 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTADL  
00264 *                                                          *      ELTADL  
00265 ************************************************************      ELTADL  
00266                                                                   ELTADL  
00267  0040-EST-ADR-OF-SUBROUTINE-PAR.                                  ELTADL  
00268      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTADL  
00269      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTADL  
00270         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELTADL  
00271         END-CALL.                                                 ELTADL  
00272      IF CIA-RC-PTR-NULL                                           ELTADL  
00273         PERFORM 0099-CIA-AB-UNALLOC-AREA.                         ELTADL  
00274                                                                   ELTADL  
00275 /***********************************************************      ELTADL  
00276 *                                                          *      ELTADL  
00277 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD     *      ELTADL  
00278 *                                                          *      ELTADL  
00279 ************************************************************      ELTADL  
00280                                                                   ELTADL  
00281  0050-EST-ADR-GRP-SPC.                                            ELTADL  
00282      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTADL  
00283      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTADL  
00284         ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                       ELTADL  
00285         END-CALL.                                                 ELTADL  
00286      IF CIA-RC-PTR-NULL                                           ELTADL  
00287         PERFORM 0099-CIA-AB-UNALLOC-AREA.                         ELTADL  
00288                                                                   ELTADL  
00289 ************************************************************      ELTADL  
00290 *                                                          *      ELTADL  
00291 *    INITIALIZE DATA AREAS                                 *      ELTADL  
00292 *                                                          *      ELTADL  
00293 ************************************************************      ELTADL  
00294                                                                   ELTADL  
00295  0060-INIT-DATA.                                                  ELTADL  
00296      COMPUTE PC-GCT-MAX-SUB =   LENGTH OF GCT-CONT-TAB-PTRS       ELTADL  
00297                               / LENGTH OF GCT-CON-TAB-ID-SLOT.    ELTADL  
00298      SET GCT-INDEX        TO PC-GCT-MAX-SUB.                      ELTADL  
00299      SET WS-MAX-GCT-INDEX TO GCT-INDEX.                           ELTADL  
00300      SET GCG-INDEX        TO GCG-COUNT-TAB-PROVN-POINTERS.        ELTADL  
00301      SET WS-MAX-GCG-INDEX TO GCG-INDEX.                           ELTADL  
00302      INITIALIZE WS-ADL-ACCUM-CNT.                                 ELTADL  
00303      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELTADL  
00304                                                                   ELTADL  
00305 /***********************************************************      ELTADL  
00306 *                                                          *      ELTADL  
00307 *    ABEND CIA UNALLOCATED AREA                            *      ELTADL  
00308 *                                                          *      ELTADL  
00309 ************************************************************      ELTADL  
00310                                                                   ELTADL  
00311  0099-CIA-AB-UNALLOC-AREA.                                        ELTADL  
00312      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTADL  
00313      EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.               ELTADL  
00314                                                                   ELTADL  
00315 ************************************************************      ELTADL  
00316 *                                                          *      ELTADL  
00317 *        PROCESS                                           *      ELTADL  
00318 *                                                          *      ELTADL  
00319 ************************************************************      ELTADL  
00320                                                                   ELTADL  
00321  0100-PROCESS.                                                    ELTADL  
00322      PERFORM 0110-SCAN-GRP-SPC-FOR-ACCUMS.                        ELTADL  
00323      PERFORM 0120-SCAN-CONTRACTS-FOR-ACCUMS.                      ELTADL  
00324                                                                   ELTADL  
00325      IF WS-ADL-ACCUM-CNT >  0                                     ELTADL  
00326      THEN                                                         ELTADL  
00327          PERFORM 0210-SCAN-FOR-APPLIC-OCCRNCS                     ELTADL  
00328      END-IF.                                                      ELTADL  
00329                                                                   ELTADL  
00330      IF SW-NO-APPLIC-ACCUM-FOUND                                  ELTADL  
00331      THEN                                                         ELTADL  
00332         EVALUATE TRUE                                             ELTADL  
00333            WHEN SSB-PROV-CLASS-INST                               ELTADL  
00334               SET SRP-INST-NOT-APPLICABLE TO TRUE                 ELTADL  
00335            WHEN SSB-PROV-CLASS-PROF                               ELTADL  
00336               SET SRP-PROF-NOT-APPLICABLE TO TRUE                 ELTADL  
00337            WHEN SSB-PROV-CLASS-BOTH                               ELTADL  
00338               SET SRP-NO-ACCUMS-FOUND TO TRUE                     ELTADL  
00339            WHEN OTHER                                             ELTADL  
00340               SET CIA-AB-PGM-LOGIC TO TRUE                        ELTADL  
00341               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELTADL  
00342            END-EVALUATE                                           ELTADL  
00343      END-IF.                                                      ELTADL  
00344                                                                   ELTADL  
00345      SET SRP-TOPIC-ACCUM TO TRUE.                                 ELTADL  
00346                                                                   ELTADL  
00347 * -- LINK TO THE OUTPUT GENERATOR                                 ELTADL  
00348      EXEC CICS LINK PROGRAM ('ELGADL') COMMAREA (DFHCOMMAREA)     ELTADL  
00349         END-EXEC.                                                 ELTADL  
00350                                                                   ELTADL  
00351 /***********************************************************      ELTADL  
00352 *                                                          *      ELTADL  
00353 *    SCAN GROUP SPECIFIC RECORD FOR ACCUMULATORS           *      ELTADL  
00354 *                                                          *      ELTADL  
00355 ************************************************************      ELTADL  
00356                                                                   ELTADL  
00357  0110-SCAN-GRP-SPC-FOR-ACCUMS.                                    ELTADL  
00358      PERFORM WITH TEST BEFORE                                     ELTADL  
00359         VARYING GCG-INDEX FROM 1 BY 1                             ELTADL  
00360           UNTIL GCG-INDEX = WS-MAX-GCG-INDEX                      ELTADL  
00361                 OR GCG-TAB-ID (GCG-INDEX) > PC-ADL                ELTADL  
00362 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELTADL  
00363         IF     GCG-TAB-ID (GCG-INDEX)  =  PC-ADL                  ELTADL  
00364            AND GCG-TAB-SLOT-NO (GCG-INDEX)  >  ZERO               ELTADL  
00365         THEN                                                      ELTADL  
00366 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELTADL  
00367            MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO WS-SLOT-NBR        ELTADL  
00368            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELTADL  
00369         END-IF                                                    ELTADL  
00370         END-PERFORM.                                              ELTADL  
00371                                                                   ELTADL  
00372 /***********************************************************      ELTADL  
00373 *                                                          *      ELTADL  
00374 *    SCAN CONTRACT RECORDS FOR ACCUMULATORS                *      ELTADL  
00375 *                                                          *      ELTADL  
00376 ************************************************************      ELTADL  
00377                                                                   ELTADL  
00378  0120-SCAN-CONTRACTS-FOR-ACCUMS.                                  ELTADL  
00379      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELTADL  
00380      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELTADL  
00381                                                                   ELTADL  
00382      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTADL  
00383      THEN                                                         ELTADL  
00384          PERFORM 0130-SCAN-INST-BAS                               ELTADL  
00385      END-IF.                                                      ELTADL  
00386                                                                   ELTADL  
00387      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTADL  
00388      THEN                                                         ELTADL  
00389          PERFORM 0140-SCAN-PROF-BAS                               ELTADL  
00390      END-IF.                                                      ELTADL  
00391                                                                   ELTADL  
00392      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELTADL  
00393      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELTADL  
00394                                                                   ELTADL  
00395      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTADL  
00396      THEN                                                         ELTADL  
00397          PERFORM 0150-SCAN-INST-SUP                               ELTADL  
00398      END-IF.                                                      ELTADL  
00399                                                                   ELTADL  
00400      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTADL  
00401      THEN                                                         ELTADL  
00402          PERFORM 0160-SCAN-PROF-SUP                               ELTADL  
00403      END-IF.                                                      ELTADL  
00404                                                                   ELTADL  
00405 /***********************************************************      ELTADL  
00406 *                                                          *      ELTADL  
00407 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELTADL  
00408 *                                                          *      ELTADL  
00409 ************************************************************      ELTADL  
00410                                                                   ELTADL  
00411  0130-SCAN-INST-BAS.                                              ELTADL  
00412      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTADL  
00413      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTADL  
00414         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTADL  
00415         END-CALL.                                                 ELTADL  
00416      SET WS-INST-CNTRCT-PTR                                       ELTADL  
00417       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTADL  
00418                                                                   ELTADL  
00419      IF CIA-RC-PTR-NULL                                           ELTADL  
00420      THEN                                                         ELTADL  
00421         CONTINUE                                                  ELTADL  
00422      ELSE                                                         ELTADL  
00423         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTADL  
00424      END-IF.                                                      ELTADL  
00425                                                                   ELTADL  
00426 ************************************************************      ELTADL  
00427 *                                                          *      ELTADL  
00428 *    SCAN PROFESSIONAL BASIC CONTRACT RECORD               *      ELTADL  
00429 *                                                          *      ELTADL  
00430 ************************************************************      ELTADL  
00431                                                                   ELTADL  
00432  0140-SCAN-PROF-BAS.                                              ELTADL  
00433      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTADL  
00434      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTADL  
00435         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTADL  
00436         END-CALL.                                                 ELTADL  
00437      SET WS-PROF-CNTRCT-PTR                                       ELTADL  
00438       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTADL  
00439                                                                   ELTADL  
00440      IF    CIA-RC-PTR-NULL                                        ELTADL  
00441         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELTADL  
00442      THEN                                                         ELTADL  
00443         CONTINUE                                                  ELTADL  
00444      ELSE                                                         ELTADL  
00445         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTADL  
00446      END-IF.                                                      ELTADL  
00447                                                                   ELTADL  
00448 /***********************************************************      ELTADL  
00449 *                                                          *      ELTADL  
00450 *    SCAN INSTITUTIONAL SUPPLEMENTAL CONTRACT RECORD       *      ELTADL  
00451 *                                                          *      ELTADL  
00452 ************************************************************      ELTADL  
00453                                                                   ELTADL  
00454  0150-SCAN-INST-SUP.                                              ELTADL  
00455      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTADL  
00456      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTADL  
00457         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTADL  
00458         END-CALL.                                                 ELTADL  
00459      SET WS-INST-CNTRCT-PTR                                       ELTADL  
00460       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTADL  
00461                                                                   ELTADL  
00462      IF CIA-RC-PTR-NULL                                           ELTADL  
00463      THEN                                                         ELTADL  
00464         CONTINUE                                                  ELTADL  
00465      ELSE                                                         ELTADL  
00466         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTADL  
00467      END-IF.                                                      ELTADL  
00468                                                                   ELTADL  
00469 ************************************************************      ELTADL  
00470 *                                                          *      ELTADL  
00471 *    SCAN PROFESSIONAL SUPPLEMENTAL CONTRACT RECORD        *      ELTADL  
00472 *                                                          *      ELTADL  
00473 ************************************************************      ELTADL  
00474                                                                   ELTADL  
00475  0160-SCAN-PROF-SUP.                                              ELTADL  
00476      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTADL  
00477      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTADL  
00478         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTADL  
00479         END-CALL.                                                 ELTADL  
00480      SET WS-PROF-CNTRCT-PTR                                       ELTADL  
00481       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTADL  
00482                                                                   ELTADL  
00483      IF    CIA-RC-PTR-NULL                                        ELTADL  
00484         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELTADL  
00485      THEN                                                         ELTADL  
00486         CONTINUE                                                  ELTADL  
00487      ELSE                                                         ELTADL  
00488         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTADL  
00489      END-IF.                                                      ELTADL  
00490                                                                   ELTADL  
00491 /***********************************************************      ELTADL  
00492 *                                                          *      ELTADL  
00493 *    SCAN A CONTRACT RECORD FOR ACCUMULATORS               *      ELTADL  
00494 *                                                          *      ELTADL  
00495 ************************************************************      ELTADL  
00496                                                                   ELTADL  
00497  0170-SCAN-CONTRACT-FOR-ACCUMS.                                   ELTADL  
00498      PERFORM WITH TEST BEFORE                                     ELTADL  
00499         VARYING GCT-TAB-INDEX FROM 1 BY 1                         ELTADL  
00500           UNTIL GCT-TAB-INDEX > WS-MAX-GCT-INDEX                  ELTADL  
00501                 OR GCT-CON-TAB-ID (GCT-TAB-INDEX) > PC-ADL        ELTADL  
00502 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELTADL  
00503         IF     GCT-CON-TAB-ID (GCT-TAB-INDEX) = PC-ADL            ELTADL  
00504            AND GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  > ZEROS          ELTADL  
00505         THEN                                                      ELTADL  
00506 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELTADL  
00507            MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  TO  WS-SLOT-NBR ELTADL  
00508            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELTADL  
00509         END-IF                                                    ELTADL  
00510         END-PERFORM.                                              ELTADL  
00511                                                                   ELTADL  
00512 ************************************************************      ELTADL  
00513 *                                                          *      ELTADL  
00514 *    SAVE UNIQUE ACCUMULATOR SLOT NUMBER                   *      ELTADL  
00515 *                                                          *      ELTADL  
00516 ************************************************************      ELTADL  
00517                                                                   ELTADL  
00518  0200-SAVE-UNQ-ACCUM-SLOT-NBR.                                    ELTADL  
00519                                                                   ELTADL  
00520 * -- SCAN TABLE OF ACCUM SLOT NUMBERS FOR DUPLICATE               ELTADL  
00521      SET SW-UNQ-SLOT-NBR TO TRUE.                                 ELTADL  
00522      PERFORM WITH TEST BEFORE                                     ELTADL  
00523         VARYING WS-ADL-SUB FROM 1 BY 1                            ELTADL  
00524           UNTIL    WS-ADL-SUB > WS-ADL-ACCUM-CNT                  ELTADL  
00525                 OR SW-DUP-SLOT-NBR                                ELTADL  
00526         IF WS-SLOT-NBR = ACCUM-SLOT-NBR (WS-ADL-SUB)              ELTADL  
00527         THEN                                                      ELTADL  
00528            SET SW-DUP-SLOT-NBR TO TRUE                            ELTADL  
00529         END-IF                                                    ELTADL  
00530         END-PERFORM.                                              ELTADL  
00531                                                                   ELTADL  
00532 * -- IF SLOT NUMBER IS UNIQUE, ADD IT TO THE HOLD TABLE           ELTADL  
00533      IF SW-UNQ-SLOT-NBR                                           ELTADL  
00534      THEN                                                         ELTADL  
00535         ADD 1 TO  WS-ADL-ACCUM-CNT                                ELTADL  
00536         MOVE WS-SLOT-NBR TO ACCUM-SLOT-NBR(WS-ADL-ACCUM-CNT)      ELTADL  
00537      END-IF.                                                      ELTADL  
00538                                                                   ELTADL  
00539 /***********************************************************      ELTADL  
00540 *                                                          *      ELTADL  
00541 *    SCAN ADL ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELTADL  
00542 *                                                          *      ELTADL  
00543 ************************************************************      ELTADL  
00544                                                                   ELTADL  
00545  0210-SCAN-FOR-APPLIC-OCCRNCS.                                    ELTADL  
00546      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELTADL  
00547      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELTADL  
00548      PERFORM 0220-DELETE-ADL-SUMMARY-FILE.                        ELTADL  
00549      PERFORM 0230-ALLOC-WORKFILE-REC-AREA.                        ELTADL  
00550                                                                   ELTADL  
00551 * -- READ AND SCAN EACH ACCUMULATOR TABULAR                       ELTADL  
00552      PERFORM WITH TEST BEFORE                                     ELTADL  
00553         VARYING WS-ADL-SUB FROM 1 BY 1                            ELTADL  
00554           UNTIL WS-ADL-SUB > WS-ADL-ACCUM-CNT                     ELTADL  
00555 *    -- OBTAIN ACCUMULATOR TABULAR RECORD                         ELTADL  
00556         MOVE PC-ADL TO KWA-PROVISION-ID                           ELTADL  
00557         MOVE ACCUM-SLOT-NBR (WS-ADL-SUB) TO KWA-PROVISION-SLOT-NO ELTADL  
00558         PERFORM 0240-READ-TABULAR-REC                             ELTADL  
00559 *    -- SCAN ACCUMULATOR TABULAR                                  ELTADL  
00560         PERFORM 0300-TEST-ADL-OCCURRENCE                          ELTADL  
00561            VARYING GAC-INDEX FROM 1 BY 1                          ELTADL  
00562              UNTIL GAC-INDEX = GAC-ENTRY-COUNT                    ELTADL  
00563      END-PERFORM.                                                 ELTADL  
00564                                                                   ELTADL  
00565                                                                   ELTADL  
00566 ************************************************************      ELTADL  
00567 *                                                          *      ELTADL  
00568 *        DELETE ADL SUMMARY FILE                           *      ELTADL  
00569 *                                                          *      ELTADL  
00570 ************************************************************      ELTADL  
00571                                                                   ELTADL  
00572  0220-DELETE-ADL-SUMMARY-FILE.                                    ELTADL  
00573      SET IOP-DEL TO TRUE.                                         ELTADL  
00574      SET IOP-FCQ-NONE TO TRUE.                                    ELTADL  
00575      SET IOP-KVQ-NONE TO TRUE.                                    ELTADL  
00576      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTADL  
00577                                                                   ELTADL  
00578 /***********************************************************      ELTADL  
00579 *                                                          *      ELTADL  
00580 *    ALLOCATE WORKFILE RECORD AREA                         *      ELTADL  
00581 *                                                          *      ELTADL  
00582 ************************************************************      ELTADL  
00583                                                                   ELTADL  
00584  0230-ALLOC-WORKFILE-REC-AREA.                                    ELTADL  
00585      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELTADL  
00586      SET CIA-STG-GETMAIN TO TRUE.                                 ELTADL  
00587      SET IOP-GETMAIN-REC TO TRUE.                                 ELTADL  
00588      COMPUTE IOP-MAX-REC-LEN =                                    ELTADL  
00589              LENGTH OF ACCUM-FIXED-AREA                           ELTADL  
00590 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELTADL  
00591            + LENGTH OF ACCUM-VARIABLE-AREA                        ELTADL  
00592            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELTADL  
00593 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELTADL  
00594 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELTADL  
00595                                                                   ELTADL  
00596      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTADL  
00597      IF IOP-REC-PTR = NULLS                                       ELTADL  
00598      THEN                                                         ELTADL  
00599         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTADL  
00600         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTADL  
00601      ELSE                                                         ELTADL  
00602         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELTADL  
00603      END-IF.                                                      ELTADL  
00604                                                                   ELTADL  
00605 ************************************************************      ELTADL  
00606 *                                                          *      ELTADL  
00607 *    READ TABULAR RECORD                                   *      ELTADL  
00608 *                                                          *      ELTADL  
00609 ************************************************************      ELTADL  
00610                                                                   ELTADL  
00611  0240-READ-TABULAR-REC.                                           ELTADL  
00612      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELTADL  
00613      SET IOP-RD TO TRUE.                                          ELTADL  
00614      SET IOP-FCQ-NONE TO TRUE.                                    ELTADL  
00615      SET IOP-KVQ-EQ TO TRUE.                                      ELTADL  
00616      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTADL  
00617      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTADL  
00618      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTADL  
00619      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTADL  
00620                                                                   ELTADL  
00621      EVALUATE TRUE                                                ELTADL  
00622        WHEN IOP-RC-OK                                             ELTADL  
00623           SET ADDRESS OF GAC-RECORD-AREA TO IOP-REC-PTR           ELTADL  
00624           SET IOP-REC-PTR TO NULLS                                ELTADL  
00625           SET GAC-INDEX   TO GAC-ENTRY-COUNT                      ELTADL  
00626           SET WS-MAX-GAC-INDEX TO GAC-INDEX                       ELTADL  
00627        WHEN IOP-RC-NOTFND                                         ELTADL  
00628           SET CIA-AB-NOTFND-GCTABULR TO TRUE                      ELTADL  
00629           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELTADL  
00630        WHEN OTHER                                                 ELTADL  
00631           SET CIA-AB-CRITIO TO TRUE                               ELTADL  
00632           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELTADL  
00633        END-EVALUATE.                                              ELTADL  
00634                                                                   ELTADL  
00635 /***********************************************************      ELTADL  
00636 *                                                          *      ELTADL  
00637 *        TEST ADL OCCURS                                   *      ELTADL  
00638 *                                                          *      ELTADL  
00639 ************************************************************      ELTADL  
00640                                                                   ELTADL  
00641  0300-TEST-ADL-OCCURRENCE.                                        ELTADL  
00642                                                                   ELTADL  
00643      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELTADL  
00644      MOVE GAC-DEDL-L-O-B (GAC-INDEX) TO WS-LOB-ACCUM-OCCRNC.      ELTADL  
00645      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELTADL  
00646      PERFORM 0320-SCAN-FOR-INTERNALS.                             ELTADL  
00647      IF SW-OCCRNC-APPLIES                                         ELTADL  
00648      THEN                                                         ELTADL  
00649 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELTADL  
00650         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELTADL  
00651         PERFORM 0340-INIT-ACCUM-EXTRACT                           ELTADL  
00652         PERFORM 0350-EXTRACT-ACCUM                                ELTADL  
00653         PERFORM 0490-CHK-EXTRACT-DATA-INTGRTY                     ELTADL  
00654         PERFORM 0710-WRITE-EXTRACT-RECORD                         ELTADL  
00655      END-IF.                                                      ELTADL  
00656                                                                   ELTADL  
00657 ************************************************************      ELTADL  
00658 *                                                          *      ELTADL  
00659 *        INITIALIZE OCCURRENCE                             *      ELTADL  
00660 *                                                          *      ELTADL  
00661 ************************************************************      ELTADL  
00662                                                                   ELTADL  
00663  0310-INITIALIZE-OCCURRENCE.                                      ELTADL  
00664      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELTADL  
00665      SET SW-HAS-NO-IBGR                                           ELTADL  
00666          SW-HAS-NO-IDGD                                           ELTADL  
00667          SW-HAS-NO-IPGN                                           ELTADL  
00668          SW-HAS-NO-IPGP                                           ELTADL  
00669          SW-HAS-NO-IPGT                                           ELTADL  
00670          SW-HAS-NO-IPGS                                           ELTADL  
00671       TO TRUE.                                                    ELTADL  
00672      INITIALIZE WS-IBGR-SLOT-NBR                                  ELTADL  
00673                 WS-IDGD-SLOT-NBR                                  ELTADL  
00674                 WS-IPGN-SLOT-NBR                                  ELTADL  
00675                 WS-IPGP-SLOT-NBR                                  ELTADL  
00676                 WS-IPGT-SLOT-NBR                                  ELTADL  
00677                 WS-IPGS-SLOT-NBR.                                 ELTADL  
00678      SET SW-INTRNL-INST-PROV-CL-NOT-DET                           ELTADL  
00679          SW-INTRNL-PROF-PROV-CL-NOT-DET                           ELTADL  
00680          SW-INTRNL-PROF-PROV-SP-NOT-DET                           ELTADL  
00681       TO TRUE.                                                    ELTADL  
00682                                                                   ELTADL  
00683 /***********************************************************      ELTADL  
00684 *                                                          *      ELTADL  
00685 *        SCAN FOR INTERNAL TABULARS                        *      ELTADL  
00686 *                                                          *      ELTADL  
00687 ************************************************************      ELTADL  
00688                                                                   ELTADL  
00689  0320-SCAN-FOR-INTERNALS.                                         ELTADL  
00690 *    (THIS IS DONE NOW IN CASE IPGT OR IBGR IS NEEDED TO DETERMINEELTADL  
00691 *     WHETHER OCCURRENCE IS INSTITUTIONAL OR PROFESSIONAL.)       ELTADL  
00692      PERFORM 0330-SCAN-THE-INTERNAL-TABULAR                       ELTADL  
00693         VARYING GAC-INT-INDEX FROM 1 BY 1                         ELTADL  
00694           UNTIL    GAC-INT-INDEX                                  ELTADL  
00695                 >= GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX).        ELTADL  
00696                                                                   ELTADL  
00697      EVALUATE TRUE ALSO TRUE                                      ELTADL  
00698         WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                        ELTADL  
00699            SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE                  ELTADL  
00700            SET SW-OCCRNC-APPLIES TO TRUE                          ELTADL  
00701         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH                 ELTADL  
00702            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELTADL  
00703            PERFORM 0550-CHK-INTRNL-TAB-PROV-CL                    ELTADL  
00704         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST                 ELTADL  
00705            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELTADL  
00706            SET SW-OCCRNC-APPLIES TO TRUE                          ELTADL  
00707         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH                 ELTADL  
00708            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELTADL  
00709            PERFORM 0550-CHK-INTRNL-TAB-PROV-CL                    ELTADL  
00710            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELTADL  
00711            PERFORM 0551-CHK-INTRNL-TAB-PROV-SP                    ELTADL  
00712         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF                 ELTADL  
00713            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELTADL  
00714            SET SW-OCCRNC-APPLIES TO TRUE                          ELTADL  
00715         WHEN OTHER                                                ELTADL  
00716            CONTINUE                                               ELTADL  
00717         END-EVALUATE.                                             ELTADL  
00718                                                                   ELTADL  
00719 /***********************************************************      ELTADL  
00720 *                                                          *      ELTADL  
00721 *        SCAN THE INTERNAL TABULARS                        *      ELTADL  
00722 *                                                          *      ELTADL  
00723 ************************************************************      ELTADL  
00724                                                                   ELTADL  
00725  0330-SCAN-THE-INTERNAL-TABULAR.                                  ELTADL  
00726      IF GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX) > 0               ELTADL  
00727      THEN                                                         ELTADL  
00728         MOVE GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX)              ELTADL  
00729           TO WS-SLOT-NBR                                          ELTADL  
00730         EVALUATE GAC-INT-ID (GAC-INDEX, GAC-INT-INDEX)            ELTADL  
00731            WHEN PC-IBGR                                           ELTADL  
00732               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELTADL  
00733               SET SW-HAS-IBGR                                     ELTADL  
00734                TO TRUE                                            ELTADL  
00735            WHEN PC-IDGD                                           ELTADL  
00736               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELTADL  
00737               SET SW-HAS-IDGD                                     ELTADL  
00738                TO TRUE                                            ELTADL  
00739            WHEN PC-IPGP                                           ELTADL  
00740               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELTADL  
00741               SET SW-HAS-IPGP                                     ELTADL  
00742                TO TRUE                                            ELTADL  
00743            WHEN PC-IPGN                                           ELTADL  
00744               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELTADL  
00745               SET SW-HAS-IPGN                                     ELTADL  
00746                TO TRUE                                            ELTADL  
00747            WHEN PC-IPGT                                           ELTADL  
00748               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELTADL  
00749               SET SW-HAS-IPGT                                     ELTADL  
00750                TO TRUE                                            ELTADL  
00751            WHEN PC-IPGS                                           ELTADL  
00752               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELTADL  
00753               SET SW-HAS-IPGS                                     ELTADL  
00754                TO TRUE                                            ELTADL  
00755            WHEN OTHER                                             ELTADL  
00756               CONTINUE                                            ELTADL  
00757            END-EVALUATE                                           ELTADL  
00758      END-IF.                                                      ELTADL  
00759                                                                   ELTADL  
00760 ************************************************************      ELTADL  
00761 *                                                          *      ELTADL  
00762 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELTADL  
00763 *                                                          *      ELTADL  
00764 ************************************************************      ELTADL  
00765                                                                   ELTADL  
00766  0340-INIT-ACCUM-EXTRACT.                                         ELTADL  
00767      INITIALIZE ACCUM-FIXED-AREA.                                 ELTADL  
00768      SET ACCUM-ADL TO TRUE.                                       ELTADL  
00769      MOVE +1 TO  ACCUM-ASCEND-DESCEND-COUNT.                      ELTADL  
00770      SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELTADL  
00771      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELTADL  
00772      INITIALIZE ACCUM-COPAY-ENTRY (1).                            ELTADL  
00773                                                                   ELTADL  
00774 /***********************************************************      ELTADL  
00775 *                                                          *      ELTADL  
00776 *        SUMMARIZE ADL TOPIC LEVEL DATA ELEMENTS           *      ELTADL  
00777 *                                                          *      ELTADL  
00778 ************************************************************      ELTADL  
00779                                                                   ELTADL  
00780  0350-EXTRACT-ACCUM.                                              ELTADL  
00781                                                                   ELTADL  
00782 * -- SET FIXED PORTION DATA ELEMENTS                              ELTADL  
00783      MOVE GAC-DEDL-MANDATORY-IND (GAC-INDEX)                      ELTADL  
00784        TO ACCUM-LMT-MANDATORY-IND.                                ELTADL  
00785      MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)                     ELTADL  
00786        TO ACCUM-BENEFIT-PERIOD.                                   ELTADL  
00787      MOVE GAC-DEDL-FAM-OR-INDIV (GAC-INDEX)                       ELTADL  
00788        TO ACCUM-FAM-OR-INDIV.                                     ELTADL  
00789      MOVE GAC-DEDL-L-O-B (GAC-INDEX)                              ELTADL  
00790        TO ACCUM-L-O-B.                                            ELTADL  
00791      MOVE GAC-DEDL-DEFINITION (GAC-INDEX)                         ELTADL  
00792        TO ACCUM-DEFINITION.                                       ELTADL  
00793      MOVE GAC-DEDL-DAY-FACTOR-IND (GAC-INDEX)                     ELTADL  
00794        TO ACCUM-DAY-FACTOR-IND.                                   ELTADL  
00795      MOVE GAC-DEDL-INTERNAL-DESCRIPTOR (GAC-INDEX)                ELTADL  
00796           TO ACCUM-INTERNAL-DESCRIPTOR.                           ELTADL  
00797      MOVE GAC-DEDL-SERVICE-GROUP (GAC-INDEX)                      ELTADL  
00798           TO ACCUM-SERVICE-GROUP.                                 ELTADL  
00799      MOVE GAC-DEDL-PLACE-OF-TREATMENT (GAC-INDEX)                 ELTADL  
00800        TO ACCUM-PLACE-OF-TREATMENT.                               ELTADL  
00801      MOVE GAC-DEDL-CONDITION (GAC-INDEX)                          ELTADL  
00802        TO ACCUM-CONDITION.                                        ELTADL  
00803      MOVE GAC-DEDL-CLAIM-LVL-ACCUM-IND (GAC-INDEX)                ELTADL  
00804        TO ACCUM-CLAIM-LVL-ACCUM-IND.                              ELTADL  
00805      MOVE GAC-DEDL-CO-PAY-IND (GAC-INDEX)                         ELTADL  
00806        TO ACCUM-CO-PAY-IND (1).                                   ELTADL  
00807      MOVE GAC-DEDL-COST-CONTAIN-IND (GAC-INDEX)                   ELTADL  
00808        TO ACCUM-COST-CONTAIN-IND.                                 ELTADL  
00809      MOVE GAC-DEDL-BEN-PER-TIME-QUAL (GAC-INDEX)                  ELTADL  
00810        TO ACCUM-BEN-PER-TIME-QUAL.                                ELTADL  
00811      MOVE GAC-DEDL-BEN-PER-TIME-FCTR (GAC-INDEX)                  ELTADL  
00812        TO ACCUM-BEN-PER-TIME-FCTR.                                ELTADL  
00813      MOVE GAC-DEDL-INTERVAL-TYPE (GAC-INDEX)                      ELTADL  
00814        TO ACCUM-INTERVAL-TYPE.                                    ELTADL  
00815      MOVE GAC-DEDL-INTERVAL-TIME-FCTR (GAC-INDEX)                 ELTADL  
00816        TO ACCUM-INTERVAL-TIME-FCTR.                               ELTADL  
00817      MOVE GAC-DEDL-INTERVAL-OVRD-IND (GAC-INDEX)                  ELTADL  
00818        TO ACCUM-INTERVAL-OVRD-IND.                                ELTADL  
00819      MOVE GAC-DEDL-INTERVAL-OVRD-VALUE (GAC-INDEX)                ELTADL  
00820        TO ACCUM-INTERVAL-OVRD-VALUE.                              ELTADL  
00821      MOVE GAC-DEDL-FYI-VALUE (GAC-INDEX)                          ELTADL  
00822        TO ACCUM-FYI-VALUE.                                        ELTADL  
00823      MOVE GCG-DED-BASE-AMT-SOURCE-IND                             ELTADL  
00824        TO ACCUM-DED-BASE-AMT-SOURCE-IND.                          ELTADL  
00825      MOVE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)                    ELTADL  
00826        TO ACCUM-VALUE-QUALIFIER.                                  ELTADL  
00827      MOVE GAC-DEDL-RELATIONSHIP-IND (GAC-INDEX)                   ELTADL  
00828        TO ACCUM-RELATIONSHIP-IND.                                 ELTADL  
00829      MOVE GAC-CARRY-OVER-CREDIT-IND (GAC-INDEX)                   ELTADL  
00830        TO ACCUM-CARRY-OVER-CREDIT-IND.                            ELTADL  
00831      MOVE GAC-DEDL-AGE-LIMIT-FROM (GAC-INDEX)                     ELTADL  
00832        TO ACCUM-AGE-LIMIT-FROM-VAL.                               ELTADL  
00833      MOVE GAC-DEDL-AGE-LIMIT-TO (GAC-INDEX)                       ELTADL  
00834        TO ACCUM-AGE-LIMIT-TO-VAL.                                 ELTADL  
00835      MOVE GAC-DEDL-AGE-QUAL-IND-FROM (GAC-INDEX)                  ELTADL  
00836        TO ACCUM-AGE-LIMIT-FROM-IND.                               ELTADL  
00837      MOVE GAC-DEDL-AGE-QUAL-IND-TO (GAC-INDEX)                    ELTADL  
00838        TO ACCUM-AGE-LIMIT-TO-IND.                                 ELTADL  
00839      SET ASCEND-DESCEND-IND-NA                                    ELTADL  
00840          1ST-DOLR-COVRGE-LMT-NA                                   ELTADL  
00841          BEN-PER-MAX-OVRD-IND-NA TO TRUE.                         ELTADL  
00842      MOVE ZEROS TO ACCUM-MAX-BASE-AMT-SOURCE-IND.                 ELTADL  
00843      MOVE ZEROS TO ACCUM-OPX-BASE-AMT-SOURCE-IND.                 ELTADL  
00844                                                                   ELTADL  
00845 * -- SET OCCURRENCE PROVIDER CLASS INFORMATION                    ELTADL  
00846      EVALUATE TRUE ALSO TRUE                                      ELTADL  
00847         WHEN    SW-INTRNL-INST-PROV-CL                            ELTADL  
00848            ALSO SW-INTRNL-NOT-PROF-PROV-CL                        ELTADL  
00849               SET ACCUM-PRVDR-CLS-INST TO TRUE                    ELTADL  
00850         WHEN    SW-INTRNL-NOT-INST-PROV-CL                        ELTADL  
00851            ALSO SW-INTRNL-PROF-PROV-CL                            ELTADL  
00852               SET ACCUM-PRVDR-CLS-PROF TO TRUE                    ELTADL  
00853         WHEN OTHER                                                ELTADL  
00854               SET ACCUM-PRVDR-CLS-ALL TO TRUE                     ELTADL  
00855         END-EVALUATE.                                             ELTADL  
00856                                                                   ELTADL  
00857 * -- SET OCCURRENCE PROVIDER SPEC INFORMATION                     ELTADL  
00858         IF SW-INTRNL-PROF-PROV-SP                                 ELTADL  
00859           SET ACCUM-PRVDR-SPC-PROF TO TRUE                        ELTADL  
00860         ELSE                                                      ELTADL  
00861           SET ACCUM-PRVDR-CLS-ALL TO TRUE                         ELTADL  
00862         END-IF.                                                   ELTADL  
00863                                                                   ELTADL  
00864 * -- SET VARIABLE PORTION DATA ELEMENTS                           ELTADL  
00865      MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                        ELTADL  
00866        TO ACCUM-VALUE-LIMIT (1).                                  ELTADL  
00867      MOVE WS-IBGR-SLOT-NBR TO ACCUM-IBGR-SLOT-NBR (1).            ELTADL  
00868      MOVE WS-IDGD-SLOT-NBR TO ACCUM-IDGD-SLOT-NBR (1).            ELTADL  
00869      MOVE WS-IPGN-SLOT-NBR TO ACCUM-IPGN-SLOT-NBR (1).            ELTADL  
00870      MOVE WS-IPGP-SLOT-NBR TO ACCUM-IPGP-SLOT-NBR (1).            ELTADL  
00871      MOVE WS-IPGT-SLOT-NBR TO ACCUM-IPGT-SLOT-NBR (1).            ELTADL  
00872      MOVE WS-IPGS-SLOT-NBR TO ACCUM-IPGS-SLOT-NBR (1).            ELTADL  
00873                                                                   ELTADL  
00874 /***********************************************************      ELTADL  
00875 *                                                          *      ELTADL  
00876 *    CHECK EXTRACT DATA INTEGRITY                          *      ELTADL  
00877 *                                                          *      ELTADL  
00878 ************************************************************      ELTADL  
00879                                                                   ELTADL  
00880  0490-CHK-EXTRACT-DATA-INTGRTY.                                   ELTADL  
00881      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELTADL  
00882         SET FYI-VALUE-NA TO TRUE.                                 ELTADL  
00883                                                                   ELTADL  
00884      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELTADL  
00885         SET COST-CONTAIN-IND-NA TO TRUE.                          ELTADL  
00886                                                                   ELTADL  
00887      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELTADL  
00888         SET PLACE-OF-TREATMENT-NA TO TRUE.                        ELTADL  
00889                                                                   ELTADL  
00890      IF ACCUM-BENEFIT-PERIOD = ZEROS OR SPACES OR LOW-VALUES      ELTADL  
00891         SET BENEFIT-PERIOD-NA TO TRUE.                            ELTADL  
00892                                                                   ELTADL  
00893      IF ACCUM-BEN-PER-TIME-QUAL = ZEROS OR SPACES OR LOW-VALUES   ELTADL  
00894         SET BEN-PER-TIME-QUAL-NA TO TRUE.                         ELTADL  
00895                                                                   ELTADL  
00896      IF ACCUM-INTERVAL-TYPE = ZEROS OR SPACES OR LOW-VALUES       ELTADL  
00897         SET INTERVAL-TYPE-NA TO TRUE.                             ELTADL  
00898                                                                   ELTADL  
00899      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELTADL  
00900         SET INTERVAL-OVRD-IND-NA TO TRUE.                         ELTADL  
00901                                                                   ELTADL  
00902      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELTADL  
00903         SET L-O-B-NA TO TRUE.                                     ELTADL  
00904                                                                   ELTADL  
00905      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELTADL  
00906         SET REINSTATEMENT-IND-NA TO TRUE.                         ELTADL  
00907                                                                   ELTADL  
00908      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELTADL  
00909         SET DEFINITION-NA TO TRUE.                                ELTADL  
00910                                                                   ELTADL  
00911      IF ACCUM-CARRY-OVER-CREDIT-IND =                             ELTADL  
00912                          ZEROS OR SPACES OR LOW-VALUES            ELTADL  
00913         SET CARRY-OVER-CREDIT-IND-NA TO TRUE.                     ELTADL  
00914                                                                   ELTADL  
00915      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELTADL  
00916         SET ASCEND-DESCEND-IND-NA TO TRUE.                        ELTADL  
00917                                                                   ELTADL  
00918      IF ACCUM-FAM-OR-INDIV = ZEROS OR SPACES OR LOW-VALUES        ELTADL  
00919         SET FAM-OR-INDIV-NA TO TRUE.                              ELTADL  
00920                                                                   ELTADL  
00921      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELTADL  
00922         SET RELATIONSHIP-IND-NA TO TRUE.                          ELTADL  
00923                                                                   ELTADL  
00924      IF ACCUM-DED-BASE-AMT-SOURCE-IND =                           ELTADL  
00925                            ZEROS OR SPACES OR LOW-VALUES          ELTADL  
00926         SET DED-BASE-AMT-SOURCE-IND-NA TO TRUE.                   ELTADL  
00927                                                                   ELTADL  
00928      IF ACCUM-VALUE-QUALIFIER = ZEROS OR SPACES OR LOW-VALUES     ELTADL  
00929         SET VALUE-QUALIFIER-NA TO TRUE.                           ELTADL  
00930                                                                   ELTADL  
00931      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELTADL  
00932         SET AGE-LMT-TO-IND-NA TO TRUE.                            ELTADL  
00933                                                                   ELTADL  
00934      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELTADL  
00935         SET AGE-LMT-FROM-IND-NA TO TRUE.                          ELTADL  
00936                                                                   ELTADL  
00937      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELTADL  
00938         SET LMT-MANDATORY-IND-NA TO TRUE.                         ELTADL  
00939                                                                   ELTADL  
00940      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELTADL  
00941         SET DAY-FACTOR-IND-NA TO TRUE.                            ELTADL  
00942                                                                   ELTADL  
00943      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELTADL  
00944         SET CLAIM-LVL-ACCUM-IND-NA TO TRUE.                       ELTADL  
00945                                                                   ELTADL  
00946      IF ACCUM-CO-PAY-IND (1) = ZEROS OR SPACES OR LOW-VALUES      ELTADL  
00947         SET CO-PAY-IND-NA (1) TO TRUE.                            ELTADL  
00948                                                                   ELTADL  
00949      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELTADL  
00950         SET SERVICE-GROUP-NA TO TRUE.                             ELTADL  
00951                                                                   ELTADL  
00952      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELTADL  
00953         SET INTERNAL-DESCRIPTOR-NA TO TRUE.                       ELTADL  
00954                                                                   ELTADL  
00955 /***********************************************************      ELTADL  
00956 *                                                          *      ELTADL  
00957 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER CLASS   *      ELTADL  
00958 *                                                          *      ELTADL  
00959 ************************************************************      ELTADL  
00960                                                                   ELTADL  
00961  0550-CHK-INTRNL-TAB-PROV-CL.                                     ELTADL  
00962      IF SW-HAS-IPGT                                               ELTADL  
00963      THEN                                                         ELTADL  
00964         PERFORM 0560-CHK-IPGT-PROV-CL                             ELTADL  
00965      ELSE                                                         ELTADL  
00966         IF SW-HAS-IBGR                                            ELTADL  
00967         THEN                                                      ELTADL  
00968            PERFORM 0640-CHK-IBGR-PROV-CL                          ELTADL  
00969         ELSE                                                      ELTADL  
00970            SET SW-OCCRNC-APPLIES TO TRUE                          ELTADL  
00971         END-IF                                                    ELTADL  
00972      END-IF.                                                      ELTADL  
00973                                                                   ELTADL  
00974 /***********************************************************      ELTADL  
00975 *                                                          *      ELTADL  
00976 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER SPEC    *      ELTADL  
00977 *                                                          *      ELTADL  
00978 ************************************************************      ELTADL  
00979                                                                   ELTADL  
00980  0551-CHK-INTRNL-TAB-PROV-SP.                                     ELTADL  
00981      IF SW-HAS-IPGS                                               ELTADL  
00982         PERFORM 0561-CHK-IPGS-PROV-SP                             ELTADL  
00983      END-IF.                                                      ELTADL  
00984                                                                   ELTADL  
00985 ******************************************************************ELTADL  
00986 *                                                                *ELTADL  
00987 *    CHECK IPGT INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELTADL  
00988 *                                                                *ELTADL  
00989 ******************************************************************ELTADL  
00990                                                                   ELTADL  
00991  0560-CHK-IPGT-PROV-CL.                                           ELTADL  
00992      MOVE PC-IPGT TO KWA-PROVISION-ID.                            ELTADL  
00993      MOVE WS-IPGT-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTADL  
00994      PERFORM 0660-READ-INTRLN-TAB.                                ELTADL  
00995      SET ADDRESS OF GX3-RECORD-AREA TO IOP-REC-PTR.               ELTADL  
00996      SET IOP-REC-PTR                TO NULLS.                     ELTADL  
00997      SET GX3-INDEX                  TO GX3-ENTRY-COUNT.           ELTADL  
00998      SET WS-MAX-GX3-INDEX           TO GX3-INDEX.                 ELTADL  
00999                                                                   ELTADL  
01000      IF GX3-ID-ARGUMENT-INCLUDED                                  ELTADL  
01001      THEN                                                         ELTADL  
01002         PERFORM 0570-CHK-INCLD-TYPE-IPGT                          ELTADL  
01003      ELSE                                                         ELTADL  
01004          PERFORM 0600-CHK-EXCLD-TYPE-IPGT                         ELTADL  
01005      END-IF.                                                      ELTADL  
01006                                                                   ELTADL  
01007 ******************************************************************ELTADL  
01008 *                                                                *ELTADL  
01009 *    CHECK IPGS INTERNAL TABULAR TO DETERMINE PROVIDER SPEC      *ELTADL  
01010 *                                                                *ELTADL  
01011 ******************************************************************ELTADL  
01012                                                                   ELTADL  
01013  0561-CHK-IPGS-PROV-SP.                                           ELTADL  
01014      MOVE PC-IPGS TO KWA-PROVISION-ID.                            ELTADL  
01015      MOVE WS-IPGS-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTADL  
01016      PERFORM 0660-READ-INTRLN-TAB.                                ELTADL  
01017      SET ADDRESS OF GXS-RECORD-AREA TO IOP-REC-PTR.               ELTADL  
01018      SET IOP-REC-PTR                TO NULLS.                     ELTADL  
01019      SET GXS-INDEX                  TO GXS-ENTRY-COUNT.           ELTADL  
01020      SET WS-MAX-GXS-INDEX           TO GXS-INDEX.                 ELTADL  
01021                                                                   ELTADL  
01022      IF GXS-ID-ARGUMENT-INCLUDED                                  ELTADL  
01023         PERFORM 0571-CHK-INCLD-TYPE-IPGS                          ELTADL  
01024      ELSE                                                         ELTADL  
01025          PERFORM 0601-CHK-EXCLD-TYPE-IPGS                         ELTADL  
01026      END-IF.                                                      ELTADL  
01027                                                                   ELTADL  
01028 /***********************************************************      ELTADL  
01029 *                                                          *      ELTADL  
01030 *    CHECK INCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELTADL  
01031 *                                                          *      ELTADL  
01032 ************************************************************      ELTADL  
01033                                                                   ELTADL  
01034  0570-CHK-INCLD-TYPE-IPGT.                                        ELTADL  
01035      SET CFT2-IDX TO 1.                                           ELTADL  
01036      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTADL  
01037          SW-INTRNL-NOT-PROF-PROV-CL                               ELTADL  
01038       TO TRUE.                                                    ELTADL  
01039      PERFORM 0580-TEST-IPGT-INCLD-ENTRIES                         ELTADL  
01040         VARYING GX3-INDEX  FROM 1 BY 1                            ELTADL  
01041           UNTIL    GX3-INDEX = WS-MAX-GX3-INDEX                   ELTADL  
01042                 OR (    SW-INTRNL-INST-PROV-CL                    ELTADL  
01043                     AND SW-INTRNL-PROF-PROV-CL ).                 ELTADL  
01044                                                                   ELTADL  
01045 /***********************************************************      ELTADL  
01046 *                                                          *      ELTADL  
01047 *    CHECK INCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELTADL  
01048 *                                                          *      ELTADL  
01049 ************************************************************      ELTADL  
01050                                                                   ELTADL  
01051  0571-CHK-INCLD-TYPE-IPGS.                                        ELTADL  
01052      SET CFT9-IDX TO 1.                                           ELTADL  
01053      SET SW-INTRNL-NOT-PROF-PROV-CL                               ELTADL  
01054       TO TRUE.                                                    ELTADL  
01055      PERFORM 0581-TEST-IPGS-INCLD-ENTRIES                         ELTADL  
01056         VARYING GXS-INDEX  FROM 1 BY 1                            ELTADL  
01057           UNTIL    GXS-INDEX = WS-MAX-GXS-INDEX                   ELTADL  
01058                 OR (    SW-INTRNL-PROF-PROV-SP).                  ELTADL  
01059                                                                   ELTADL  
01060 ************************************************************      ELTADL  
01061 *                                                          *      ELTADL  
01062 *    TEST IPGT INCLUDE ENTRIES TO DETERMINE PROVIDER CLASS *      ELTADL  
01063 *                                                          *      ELTADL  
01064 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELTADL  
01065 *          CFT2 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELTADL  
01066 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELTADL  
01067 *          OF THE IPGT TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELTADL  
01068 *                                                          *      ELTADL  
01069 ************************************************************      ELTADL  
01070                                                                   ELTADL  
01071  0580-TEST-IPGT-INCLD-ENTRIES.                                    ELTADL  
01072      PERFORM WITH TEST BEFORE                                     ELTADL  
01073         UNTIL    SW-OCCRNC-APPLIES                                ELTADL  
01074               OR   CFT2-PT (CFT2-IDX)                             ELTADL  
01075                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELTADL  
01076               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELTADL  
01077         IF   GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELTADL  
01078            = CFT2-PT (CFT2-IDX)                                   ELTADL  
01079         THEN                                                      ELTADL  
01080 *    -- TEST PROVIDER CLASS                                       ELTADL  
01081            EVALUATE TRUE                                          ELTADL  
01082               WHEN CFT2-PT-INST (CFT2-IDX)                        ELTADL  
01083                  SET SW-INTRNL-INST-PROV-CL TO TRUE               ELTADL  
01084                  IF SRP-ACCUM-PROV-CLASS-INST                     ELTADL  
01085                  THEN                                             ELTADL  
01086                     SET SW-OCCRNC-APPLIES TO TRUE                 ELTADL  
01087                  END-IF                                           ELTADL  
01088               WHEN CFT2-PT-PROF (CFT2-IDX)                        ELTADL  
01089                  SET SW-INTRNL-PROF-PROV-CL TO TRUE               ELTADL  
01090                  IF SRP-ACCUM-PROV-CLASS-PROF                     ELTADL  
01091                  THEN                                             ELTADL  
01092                     SET SW-OCCRNC-APPLIES TO TRUE                 ELTADL  
01093                  END-IF                                           ELTADL  
01094               END-EVALUATE                                        ELTADL  
01095         ELSE                                                      ELTADL  
01096            CONTINUE                                               ELTADL  
01097         END-IF                                                    ELTADL  
01098 *    -- BUMP TO NEXT CFT2 TABLE ENTRY                             ELTADL  
01099         SET CFT2-IDX UP BY 1                                      ELTADL  
01100         END-PERFORM.                                              ELTADL  
01101                                                                   ELTADL  
01102 ************************************************************      ELTADL  
01103 *                                                          *      ELTADL  
01104 *    TEST IPGS INCLUDE ENTRIES TO DETERMINE PROVIDER SPEC  *      ELTADL  
01105 *                                                          *      ELTADL  
01106 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELTADL  
01107 *          CFT9 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELTADL  
01108 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELTADL  
01109 *          OF THE IPGS TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELTADL  
01110 *                                                          *      ELTADL  
01111 ************************************************************      ELTADL  
01112                                                                   ELTADL  
01113  0581-TEST-IPGS-INCLD-ENTRIES.                                    ELTADL  
01114      PERFORM WITH TEST BEFORE                                     ELTADL  
01115         UNTIL    SW-OCCRNC-APPLIES                                ELTADL  
01116               OR   CFT9-PT (CFT9-IDX)                             ELTADL  
01117                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELTADL  
01118               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELTADL  
01119         IF   GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELTADL  
01120            = CFT9-PT (CFT9-IDX)                                   ELTADL  
01121         THEN                                                      ELTADL  
01122 *    -- TEST PROVIDER SPEC                                        ELTADL  
01123        IF CFT9-PT-PROF (CFT9-IDX)                                 ELTADL  
01124          SET SW-INTRNL-PROF-PROV-SP TO TRUE                       ELTADL  
01125          IF SRP-ACCUM-PROV-SPEC-PROF                              ELTADL  
01126              SET SW-OCCRNC-APPLIES TO TRUE                        ELTADL  
01127           END-IF                                                  ELTADL  
01128           END-IF                                                  ELTADL  
01129         ELSE                                                      ELTADL  
01130            CONTINUE                                               ELTADL  
01131         END-IF                                                    ELTADL  
01132 *    -- BUMP TO NEXT CFT9 TABLE ENTRY                             ELTADL  
01133         SET CFT9-IDX UP BY 1                                      ELTADL  
01134         END-PERFORM.                                              ELTADL  
01135                                                                   ELTADL  
01136 /***********************************************************      ELTADL  
01137 *                                                          *      ELTADL  
01138 *    CHECK EXCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELTADL  
01139 *                                                          *      ELTADL  
01140 ************************************************************      ELTADL  
01141                                                                   ELTADL  
01142  0600-CHK-EXCLD-TYPE-IPGT.                                        ELTADL  
01143                                                                   ELTADL  
01144 * -- INITIALIZE CFT2 TABLE TO INCLUDE ALL PROVIDER TYPES          ELTADL  
01145      PERFORM WITH TEST BEFORE                                     ELTADL  
01146         VARYING CFT2-IDX FROM 1 BY 1                              ELTADL  
01147           UNTIL CFT2-IDX > CFT2-NBR-TBL-ENTRIES                   ELTADL  
01148         SET  CFT2-PT-INCLUDE (CFT2-IDX) TO TRUE                   ELTADL  
01149         END-PERFORM.                                              ELTADL  
01150                                                                   ELTADL  
01151 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGT                 ELTADL  
01152      SET  CFT2-IDX TO 1.                                          ELTADL  
01153      PERFORM 0610-TAG-EXCLD-IPGT-ENTRIES                          ELTADL  
01154         VARYING GX3-INDEX FROM 1 BY 1                             ELTADL  
01155           UNTIL GX3-INDEX = WS-MAX-GX3-INDEX.                     ELTADL  
01156                                                                   ELTADL  
01157 * -- CHECK CFT2 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELTADL  
01158      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTADL  
01159          SW-INTRNL-NOT-PROF-PROV-CL                               ELTADL  
01160       TO TRUE.                                                    ELTADL  
01161      PERFORM 0630-CHK-CFT2-NOT-EXCLD                              ELTADL  
01162         VARYING CFT2-IDX FROM 1 BY 1                              ELTADL  
01163           UNTIL    (    SW-INTRNL-INST-PROV-CL                    ELTADL  
01164                     AND SW-INTRNL-PROF-PROV-CL )                  ELTADL  
01165                 OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES.               ELTADL  
01166                                                                   ELTADL  
01167 /***********************************************************      ELTADL  
01168 *                                                          *      ELTADL  
01169 *    CHECK EXCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELTADL  
01170 *                                                          *      ELTADL  
01171 ************************************************************      ELTADL  
01172                                                                   ELTADL  
01173  0601-CHK-EXCLD-TYPE-IPGS.                                        ELTADL  
01174                                                                   ELTADL  
01175 * -- INITIALIZE CFT9 TABLE TO INCLUDE ALL PROVIDER SPEC           ELTADL  
01176      PERFORM WITH TEST BEFORE                                     ELTADL  
01177         VARYING CFT9-IDX FROM 1 BY 1                              ELTADL  
01178           UNTIL CFT9-IDX > CFT9-NBR-TBL-ENTRIES                   ELTADL  
01179         SET  CFT9-PT-INCLUDE (CFT9-IDX) TO TRUE                   ELTADL  
01180         END-PERFORM.                                              ELTADL  
01181                                                                   ELTADL  
01182 * -- TAG ALL PROVIDER SPEC EXCLUDED BY THIS IPGS                  ELTADL  
01183      SET  CFT9-IDX TO 1.                                          ELTADL  
01184      PERFORM 0611-TAG-EXCLD-IPGS-ENTRIES                          ELTADL  
01185         VARYING GXS-INDEX FROM 1 BY 1                             ELTADL  
01186           UNTIL GXS-INDEX = WS-MAX-GXS-INDEX.                     ELTADL  
01187                                                                   ELTADL  
01188 * -- CHECK CFT9 TABLE FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED ELTADL  
01189      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTADL  
01190          SW-INTRNL-NOT-PROF-PROV-CL                               ELTADL  
01191          SW-INTRNL-NOT-PROF-PROV-SP                               ELTADL  
01192       TO TRUE.                                                    ELTADL  
01193      PERFORM 0631-CHK-CFT9-NOT-EXCLD                              ELTADL  
01194         VARYING CFT9-IDX FROM 1 BY 1                              ELTADL  
01195           UNTIL    (    SW-INTRNL-PROF-PROV-SP)                   ELTADL  
01196                 OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES.               ELTADL  
01197                                                                   ELTADL  
01198 ************************************************************      ELTADL  
01199 *                                                          *      ELTADL  
01200 *    TAG EXCLUDED IPGT ENTRIES IN CFT2                     *      ELTADL  
01201 *                                                          *      ELTADL  
01202 ************************************************************      ELTADL  
01203                                                                   ELTADL  
01204  0610-TAG-EXCLD-IPGT-ENTRIES.                                     ELTADL  
01205      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELTADL  
01206      PERFORM WITH TEST BEFORE                                     ELTADL  
01207         UNTIL    SW-ENTRY-FOUND                                   ELTADL  
01208               OR   CFT2-PT (CFT2-IDX)                             ELTADL  
01209                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELTADL  
01210               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELTADL  
01211         IF   CFT2-PT(CFT2-IDX)                                    ELTADL  
01212            = GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELTADL  
01213         THEN                                                      ELTADL  
01214            SET SW-ENTRY-FOUND TO TRUE                             ELTADL  
01215            SET CFT2-PT-EXCLUDE (CFT2-IDX) TO TRUE                 ELTADL  
01216            SET CFT2-IDX UP BY 1                                   ELTADL  
01217         ELSE                                                      ELTADL  
01218            SET CFT2-IDX UP BY 1                                   ELTADL  
01219         END-IF                                                    ELTADL  
01220         END-PERFORM.                                              ELTADL  
01221                                                                   ELTADL  
01222 ************************************************************      ELTADL  
01223 *                                                          *      ELTADL  
01224 *    TAG EXCLUDED IPGS ENTRIES IN CFT9                     *      ELTADL  
01225 *                                                          *      ELTADL  
01226 ************************************************************      ELTADL  
01227                                                                   ELTADL  
01228  0611-TAG-EXCLD-IPGS-ENTRIES.                                     ELTADL  
01229      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELTADL  
01230      PERFORM WITH TEST BEFORE                                     ELTADL  
01231         UNTIL    SW-ENTRY-FOUND                                   ELTADL  
01232               OR   CFT9-PT (CFT2-IDX)                             ELTADL  
01233                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELTADL  
01234               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELTADL  
01235         IF   CFT9-PT(CFT9-IDX)                                    ELTADL  
01236            = GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELTADL  
01237            SET SW-ENTRY-FOUND TO TRUE                             ELTADL  
01238            SET CFT9-PT-EXCLUDE (CFT9-IDX) TO TRUE                 ELTADL  
01239            SET CFT9-IDX UP BY 1                                   ELTADL  
01240         ELSE                                                      ELTADL  
01241            SET CFT9-IDX UP BY 1                                   ELTADL  
01242         END-IF                                                    ELTADL  
01243         END-PERFORM.                                              ELTADL  
01244                                                                   ELTADL  
01245 /*****************************************************************ELTADL  
01246 *                                                                *ELTADL  
01247 *    CHECK CFT2 FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDED     *ELTADL  
01248 *                                                                *ELTADL  
01249 ******************************************************************ELTADL  
01250                                                                   ELTADL  
01251  0630-CHK-CFT2-NOT-EXCLD.                                         ELTADL  
01252      IF CFT2-PT-INCLUDE (CFT2-IDX)                                ELTADL  
01253      THEN                                                         ELTADL  
01254         EVALUATE TRUE                                             ELTADL  
01255            WHEN CFT2-PT-INST (CFT2-IDX)                           ELTADL  
01256               SET SW-INTRNL-INST-PROV-CL TO TRUE                  ELTADL  
01257               IF SRP-ACCUM-PROV-CLASS-INST                        ELTADL  
01258               THEN                                                ELTADL  
01259                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTADL  
01260               END-IF                                              ELTADL  
01261            WHEN CFT2-PT-PROF (CFT2-IDX)                           ELTADL  
01262               SET SW-INTRNL-PROF-PROV-CL TO TRUE                  ELTADL  
01263               IF SRP-ACCUM-PROV-CLASS-PROF                        ELTADL  
01264               THEN                                                ELTADL  
01265                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTADL  
01266               END-IF                                              ELTADL  
01267            END-EVALUATE                                           ELTADL  
01268      END-IF.                                                      ELTADL  
01269                                                                   ELTADL  
01270 /*****************************************************************ELTADL  
01271 *                                                                *ELTADL  
01272 *    CHECK CFT9 FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED     * ELTADL  
01273 *                                                                *ELTADL  
01274 ******************************************************************ELTADL  
01275                                                                   ELTADL  
01276  0631-CHK-CFT9-NOT-EXCLD.                                         ELTADL  
01277      IF CFT9-PT-INCLUDE (CFT9-IDX)                                ELTADL  
01278        IF CFT9-PT-PROF (CFT9-IDX)                                 ELTADL  
01279          SET SW-INTRNL-PROF-PROV-CL TO TRUE                       ELTADL  
01280          IF SRP-ACCUM-PROV-SPEC-PROF                              ELTADL  
01281             SET SW-OCCRNC-APPLIES TO TRUE                         ELTADL  
01282          END-IF                                                   ELTADL  
01283      END-IF.                                                      ELTADL  
01284                                                                   ELTADL  
01285 ******************************************************************ELTADL  
01286 *                                                                *ELTADL  
01287 *    CHECK IBGR INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELTADL  
01288 *                                                                *ELTADL  
01289 ******************************************************************ELTADL  
01290                                                                   ELTADL  
01291  0640-CHK-IBGR-PROV-CL.                                           ELTADL  
01292      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTADL  
01293          SW-INTRNL-NOT-PROF-PROV-CL TO TRUE.                      ELTADL  
01294      MOVE PC-IBGR TO KWA-PROVISION-ID.                            ELTADL  
01295      MOVE WS-IBGR-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTADL  
01296      PERFORM 0660-READ-INTRLN-TAB.                                ELTADL  
01297      SET ADDRESS OF GX1-RECORD-AREA TO IOP-REC-PTR.               ELTADL  
01298      SET IOP-REC-PTR                TO NULLS.                     ELTADL  
01299      SET GX1-INDEX                  TO GX1-ENTRY-COUNT.           ELTADL  
01300      SET WS-MAX-GX1-INDEX           TO GX1-INDEX.                 ELTADL  
01301                                                                   ELTADL  
01302      IF GX1-ID-ARGUMENT-EXCLUDED                                  ELTADL  
01303      THEN                                                         ELTADL  
01304 *    -- ASSUME THAT IBGR WOULD NOT EXCLUDE ALL OF ANY PROVIDER    ELTADL  
01305 *       CLASS (I.E., BOTH TYPES APPLY).                           ELTADL  
01306         SET SW-OCCRNC-APPLIES                                     ELTADL  
01307             SW-INTRNL-INST-PROV-CL                                ELTADL  
01308             SW-INTRNL-PROF-PROV-CL                                ELTADL  
01309          TO TRUE                                                  ELTADL  
01310      ELSE                                                         ELTADL  
01311         PERFORM 0650-CHK-INCLD-TYPE-IBGR                          ELTADL  
01312      END-IF.                                                      ELTADL  
01313                                                                   ELTADL  
01314 /*****************************************************************ELTADL  
01315 *                                                                *ELTADL  
01316 *    CHECK INCLUDE TYPE IBGR TO DETERMINE PROVIDER CLASS         *ELTADL  
01317 *                                                                *ELTADL  
01318 ******************************************************************ELTADL  
01319                                                                   ELTADL  
01320  0650-CHK-INCLD-TYPE-IBGR.                                        ELTADL  
01321      PERFORM WITH TEST BEFORE                                     ELTADL  
01322         VARYING GX1-INDEX FROM 1 BY 1                             ELTADL  
01323           UNTIL    GX1-INDEX = WS-MAX-GX1-INDEX                   ELTADL  
01324                 OR (    SW-INTRNL-INST-PROV-CL                    ELTADL  
01325                     AND SW-INTRNL-PROF-PROV-CL )                  ELTADL  
01326         MOVE GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)                ELTADL  
01327           TO WS-PROVISION-ARGUMENT                                ELTADL  
01328         EVALUATE TRUE                                             ELTADL  
01329            WHEN INST-CL                                           ELTADL  
01330               SET SW-INTRNL-INST-PROV-CL TO TRUE                  ELTADL  
01331               IF SRP-ACCUM-PROV-CLASS-INST                        ELTADL  
01332               THEN                                                ELTADL  
01333                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTADL  
01334               END-IF                                              ELTADL  
01335            WHEN PROF-CL                                           ELTADL  
01336               SET SW-INTRNL-PROF-PROV-CL TO TRUE                  ELTADL  
01337               IF SRP-ACCUM-PROV-CLASS-PROF                        ELTADL  
01338               THEN                                                ELTADL  
01339                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTADL  
01340               END-IF                                              ELTADL  
01341            END-EVALUATE                                           ELTADL  
01342         END-PERFORM.                                              ELTADL  
01343                                                                   ELTADL  
01344 /***********************************************************      ELTADL  
01345 *                                                          *      ELTADL  
01346 *    READ THE INTERNAL TABULAR RECORD                      *      ELTADL  
01347 *                                                          *      ELTADL  
01348 ************************************************************      ELTADL  
01349                                                                   ELTADL  
01350  0660-READ-INTRLN-TAB.                                            ELTADL  
01351      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELTADL  
01352      SET IOP-RD TO TRUE.                                          ELTADL  
01353      SET IOP-FCQ-NONE TO TRUE.                                    ELTADL  
01354      SET IOP-KVQ-EQ TO TRUE.                                      ELTADL  
01355      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELTADL  
01356      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTADL  
01357      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTADL  
01358      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTADL  
01359                                                                   ELTADL  
01360      EVALUATE TRUE                                                ELTADL  
01361         WHEN IOP-RC-OK                                            ELTADL  
01362            CONTINUE                                               ELTADL  
01363         WHEN IOP-RC-NOTFND                                        ELTADL  
01364            SET CIA-AB-NOTFND-GCTABULR TO TRUE                     ELTADL  
01365            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELTADL  
01366         WHEN OTHER                                                ELTADL  
01367             SET CIA-AB-CRITIO TO TRUE                             ELTADL  
01368             EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC           ELTADL  
01369         END-EVALUATE.                                             ELTADL  
01370                                                                   ELTADL  
01371 ************************************************************      ELTADL  
01372 *                                                          *      ELTADL  
01373 *        ADD ACCUM OCCURRENCE TO FILE                      *      ELTADL  
01374 *                                                          *      ELTADL  
01375 ************************************************************      ELTADL  
01376                                                                   ELTADL  
01377  0710-WRITE-EXTRACT-RECORD.                                       ELTADL  
01378      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELTADL  
01379      SET  IOP-ADD TO TRUE.                                        ELTADL  
01380      SET  IOP-FCQ-NONE TO TRUE.                                   ELTADL  
01381      SET  IOP-KVQ-NONE TO TRUE.                                   ELTADL  
01382      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTADL  
01383                                                                   ELTADL  
01384 /***********************************************************      ELTADL  
01385 *                                                          *      ELTADL  
01386 *    ESTABLISH ADDRESSABILITY OF THE TABULAR FILE          *      ELTADL  
01387 *                                                          *      ELTADL  
01388 ************************************************************      ELTADL  
01389                                                                   ELTADL  
01390  9060-EST-ADR-TABULAR-FILE.                                       ELTADL  
01391      SET  CIA-GCTABULR-DDN TO TRUE.                               ELTADL  
01392      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTADL  
01393         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELTADL  
01394         END-CALL.                                                 ELTADL  
01395      IF CIA-RC-PTR-NULL                                           ELTADL  
01396      THEN                                                         ELTADL  
01397         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTADL  
01398         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTADL  
01399      END-IF.                                                      ELTADL  
01400                                                                   ELTADL  
01401 ************************************************************      ELTADL  
01402 *                                                          *      ELTADL  
01403 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELTADL  
01404 *                                                          *      ELTADL  
01405 ************************************************************      ELTADL  
01406                                                                   ELTADL  
01407  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELTADL  
01408      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELTADL  
01409      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTADL  
01410         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELTADL  
01411         END-CALL.                                                 ELTADL  
01412      IF CIA-RC-PTR-NULL                                           ELTADL  
01413         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTADL  
01414         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTADL  
01415      END-IF.                                                      ELTADL  
