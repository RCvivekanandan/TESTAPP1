00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELTABM  
00003  PROGRAM-ID.        ELTABM.                                          LV002
00004                                                                   ELTABM  
00005  AUTHOR.            LUCY TORRES.                                  ELTABM  
00006                     RICHARD J. LUKETICH (RE-WRITE).               ELTABM  
00007                                                                   ELTABM  
00008  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELTABM  
00009                     A MUTUAL LEGAL RESERVE COMPANY                ELTABM  
00010                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELTABM  
00011                     233 N. MICHIGAN AVE                           ELTABM  
00012                     CHICAGO, ILLINOIS 60601                       ELTABM  
00013                                                                   ELTABM  
00014  DATE-WRITTEN.      03-JUN-1987.                                  ELTABM  
00015                     03-JAN-1992 (RE-WRITE).                       ELTABM  
00016                                                                   ELTABM  
00017  DATE-COMPILED.                                                   ELTABM  
00018                                                                   ELTABM  
00019  SECURITY.          COPYRIGHT 1986, 1992,                         ELTABM  
00020                     HEALTH CARE SERVICE CORPORATION               ELTABM  
00021                                                                   ELTABM  
00022  ENVIRONMENT DIVISION.                                            ELTABM  
00023                                                                   ELTABM  
00024  CONFIGURATION SECTION.                                           ELTABM  
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELTABM  
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELTABM  
00027                                                                   ELTABM  
00028 /*****************************************************************ELTABM  
00029 *                                                                *ELTABM  
00030 *  ELTABM - ELS:    SELECTS #ABM (MAXIMUM) ACCUMULATORS AND      *ELTABM  
00031 *                   SETUPS THE INFORMATION TO BE PROCESSED BY    *ELTABM  
00032 *                   THE MAXIMUM GENERATOR MODULE.      THE ACCUMS*ELTABM  
00033 *                   ARE SELECTED FROM THE GROUP SPECIFIC AND     *ELTABM  
00034 *                   CONTRACT LEVEL PROCESSING.                   *ELTABM  
00035 *                                                                *ELTABM  
00036 ******************************************************************ELTABM  
00037 *                                                                *ELTABM  
00038 *                      MAINTENANCE HISTORY                       *ELTABM  
00039 *                                                                *ELTABM  
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELTABM  
00041 * ----- ----------- --- ----- ---------------------------------- *ELTABM  
00042 * 01.00 03-JUN-1987 LET       CREATED                            *ELTABM  
00043 * 01.01 25-SEP-1987 LET       ADDED DEFINITION DATA FIELD        *ELTABM  
00044 *                                                                *ELTABM  
00045 * 01.02 17-NOV-1987 REB       MADE CHANGES TO CORRESPOND TO NEW  *ELTABM  
00046 *                             VERSION OF COPYBOOK ELSACUMC.      *ELTABM  
00047 *                                                                *ELTABM  
00048 * 01.07    SEP-1991 RKH    1. ADDED LOGIC FOR:                   *ELTABM  
00049 *    ISSR #12010                A.  NEW PATIENT AGE FIELDS       *ELTABM  
00050 *                               B.  RELATIONSHIP IND VALUE       *ELTABM  
00051 *                          2. REVISE LOGIC TO LOAD INT ACCUMS    *ELTABM  
00052 *                             INTO VARIABLE LEVEL TABLE          *ELTABM  
00053 *                          3. ADDED COPYBOOKS :                  *ELTABM  
00054 *                               A. GCTIBGR   - IBGR TAB          *ELTABM  
00055 *                               B. GCTIPGT   - IPGT TAB          *ELTABM  
00056 *                               C. ELSCFTB2  - PROVIDER TYPE     *ELTABM  
00057 *                                         COMPARE TABLE          *ELTABM  
00058 *                          4. ADD LOGIC TO INSPECT #IPGT AND     *ELTABM  
00059 *                             #IBGR INT TABS TO DETERMINE IF     *ELTABM  
00060 *                             AN OCCURRANCE IS THE SELECTED      *ELTABM  
00061 *                             PROVIDER CLASS.                    *ELTABM  
00062 *                                                                *ELTABM  
00063 * 01.08    NOV-1993 JPB    CHANGED SUBSCRIPT FROM WS-GCG-TAB-SUB *ELTABM  
00064 *                          TO GCG-INDEX BECAUSE THE GROUP CON-   *ELTABM  
00065 *                          TRACT INFORMATION WAS NOT BEING PRO-  *ELTABM  
00066 *                          CESSED.                               *ELTABM  
00067 *                                                                *ELTABM  
00068 * 01.09    DEC-02-98 AKK   MANTENANCE DUE TO ADDITON OF ACP TAB. *ELTABM  
00069 *                                                                *ELTABM  
00070 * 01.10    AUG-25-00 AKK   MANTENANCE DUE TO ADDITON OF #IPGS.   *ELTABM  
00071 *                                                                *ELTABM  
00072 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST    *ELTABM  
00073 *                                                                *ELTABM  
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00056 *                                                                *ELGABMCC
00074 ******************************************************************ELTABM  
00075      TITLE  'ELTABM          WORKING STORAGE'.                    ELTABM  
00076  DATA DIVISION.                                                   ELTABM  
00077                                                                   ELTABM  
00078  WORKING-STORAGE SECTION.                                         ELTABM  
00079                                                                   ELTABM  
00080  01  SWITCHES.                                                    ELTABM  
00081      02                                      PICTURE  X(01).      ELTABM  
00082         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELTABM  
00083         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELTABM  
00084                                                                   ELTABM  
00085      02 OCCURRENCE-APPLIES                   PICTURE  X(01).      ELTABM  
00086         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELTABM  
00087         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELTABM  
00088      02                                      PICTURE  X(01).      ELTABM  
00089         88 SW-HAS-IBGR                       VALUE 'Y'.           ELTABM  
00090         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELTABM  
00091      02                                      PICTURE  X(01).      ELTABM  
00092         88 SW-HAS-IDGD                       VALUE 'Y'.           ELTABM  
00093         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELTABM  
00094      02                                      PICTURE  X(01).      ELTABM  
00095         88 SW-HAS-IPGN                       VALUE 'Y'.           ELTABM  
00096         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELTABM  
00097      02                                      PICTURE  X(01).      ELTABM  
00098         88 SW-HAS-IPGP                       VALUE 'Y'.           ELTABM  
00099         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELTABM  
00100      02                                      PICTURE  X(01).      ELTABM  
00101         88 SW-HAS-IPGT                       VALUE 'Y'.           ELTABM  
00102         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELTABM  
00103      02                                      PICTURE  X(01).      ELTABM  
00104         88 SW-HAS-IPGS                       VALUE 'Y'.           ELTABM  
00105         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELTABM  
00106      02                                      PICTURE  X(01).      ELTABM  
00107         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELTABM  
00108         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELTABM  
00109      02                                      PICTURE  X(01).      ELTABM  
00110         88 SW-INTRNL-INST-PROV-CL            VALUE 'Y'.           ELTABM  
00111         88 SW-INTRNL-NOT-INST-PROV-CL        VALUE 'N'.           ELTABM  
00112         88 SW-INTRNL-INST-PROV-CL-NOT-DET VALUE 'X'.              ELTABM  
00113      02                                      PICTURE  X(01).      ELTABM  
00114         88 SW-INTRNL-PROF-PROV-CL            VALUE 'Y'.           ELTABM  
00115         88 SW-INTRNL-NOT-PROF-PROV-CL        VALUE 'N'.           ELTABM  
00116         88 SW-INTRNL-PROF-PROV-CL-NOT-DET VALUE 'X'.              ELTABM  
00117      02                                      PICTURE  X(01).      ELTABM  
00118         88 SW-INTRNL-PROF-PROV-SP            VALUE 'Y'.           ELTABM  
00119         88 SW-INTRNL-NOT-PROF-PROV-SP        VALUE 'N'.           ELTABM  
00120         88 SW-INTRNL-PROF-PROV-SP-NOT-DET VALUE 'X'.              ELTABM  
00121      02                                      PICTURE  X(01).      ELTABM  
00122         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELTABM  
00123         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELTABM  
00124                                                                   ELTABM  
00125  01  WS-PROVISION-ARGUMENT.                                       ELTABM  
00126      02                          PICTURE  X(05).                  ELTABM  
00127      02 WS-PROVISION-CL          PICTURE  X(01).                  ELTABM  
00128         88 INST-CL               VALUE 'A', 'B', 'W'.             ELTABM  
00129         88 PROF-CL               VALUE 'C', 'D', 'E'.             ELTABM  
00130                                                                   ELTABM  
00131  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELTABM  
00132      88 WS-LOB-INST              VALUE '1'.                       ELTABM  
00133      88 WS-LOB-PROF              VALUE '2'.                       ELTABM  
00134      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELTABM  
00135      88 WS-LOB-BOTH              VALUE '3', '4', '5', '6', '7'.   ELTABM  
00136                                                                   ELTABM  
00137  01  PROGRAM-CONSTANTS.                                           ELTABM  
00138      02 PC-ABM                   PICTURE  X(06) VALUE '#ABM  '.   ELTABM  
00139      02 PC-GCT-MAX-SUB           PICTURE S9(04) COMP.             ELTABM  
00140      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELTABM  
00141      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELTABM  
00142      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELTABM  
00143      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELTABM  
00144      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELTABM  
00145      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGS '.   ELTABM  
00146      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELTABM  
00147                                                                   ELTABM  
00148  01  WS-WORK-FIELDS.                                              ELTABM  
00149      02 WS-ABM-SUB               PICTURE S9(04) COMP.             ELTABM  
00150      02 WS-GCG-TAB-SUB           PICTURE S9(04) COMP.             ELTABM  
00151      02 WS-GCT-TAB-SUB           PICTURE S9(04) COMP.             ELTABM  
00152      02 WS-ABM-ACCUM-CNT         PICTURE S9(04) COMP.             ELTABM  
00153      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELTABM  
00154                                                                   ELTABM  
00155  01  WS-POINTERS.                                                 ELTABM  
00156      02  WS-INST-CNTRCT-PTR      POINTER.                         ELTABM  
00157      02  WS-PROF-CNTRCT-PTR      POINTER.                         ELTABM  
00158                                                                   ELTABM  
00159  01  WS-MAX-INDEX-VALUES.                                         ELTABM  
00160      02 WS-MAX-GX1-INDEX         USAGE IS INDEX.                  ELTABM  
00161      02 WS-MAX-GX3-INDEX         USAGE IS INDEX.                  ELTABM  
00162      02 WS-MAX-GXS-INDEX         USAGE IS INDEX.                  ELTABM  
00163      02 WS-MAX-GAA-INDEX         USAGE IS INDEX.                  ELTABM  
00164      02 WS-MAX-GAA-INT-INDEX     USAGE IS INDEX.                  ELTABM  
00165      02 WS-MAX-GCT-INDEX         USAGE IS INDEX.                  ELTABM  
00166      02 WS-MAX-GCG-INDEX         USAGE IS INDEX.                  ELTABM  
00167                                                                   ELTABM  
00168  01  ACCUM-HOLD-TBL.                                              ELTABM  
00169      02  ACCUM-SLOT-NBR          PICTURE S9(07) COMP-3            ELTABM  
00170                                  OCCURS 5 TIMES.                  ELTABM  
00171                                                                   ELTABM  
00172  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELTABM  
00173      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTABM  
00174      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTABM  
00175      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTABM  
00176      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTABM  
00177      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTABM  
00178      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELTABM  
00179                                                                   ELTABM  
00180 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELTABM  
00181      COPY ELSCFTB2.                                               ELTABM  
00182                                                                   ELTABM  
00183 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELTABM  
00184      COPY ELSCFTB9.                                               ELTABM  
00185                                                                   ELTABM  
00186      TITLE  'ELTABM          LINKAGE SECTION'                     ELTABM  
00187  LINKAGE SECTION.                                                 ELTABM  
00188  01  DFHCOMMAREA.                                                 ELTABM  
00189      COPY ELSCOMMC.                                               ELTABM  
00190 /                                                                 ELTABM  
00191      COPY ELSCIA2C.                                               ELTABM  
00192 /                                                                 ELTABM  
00193      COPY ELSIOPMC.                                               ELTABM  
00194 /                                                                 ELTABM  
00195      COPY ELSKEYSC.                                               ELTABM  
00196 /                                                                 ELTABM  
00197      COPY ELSSRTPC.                                               ELTABM  
00198 /                                                                 ELTABM  
00199      COPY ELSSSCBC.                                               ELTABM  
00200 /                                                                 ELTABM  
00201  01  GCG-GRP-SPEC-RECORD-AREA.                                    ELTABM  
00202      COPY GCGROUPC.                                               ELTABM  
00203 /                                                                 ELTABM  
00204  01  GCT-CONTRACT-RECORD-AREA.                                    ELTABM  
00205      COPY GCCONTRC.                                               ELTABM  
00206 /                                                                 ELTABM  
00207  01  GAA-RECORD-AREA.                                             ELTABM  
00208      COPY GCTABMC.                                                ELTABM  
00209 /                                                                 ELTABM  
00210      COPY ELSACUMC.                                               ELTABM  
00211 /                                                                 ELTABM  
00212  01  GX1-RECORD-AREA.                                             ELTABM  
00213      COPY GCTIBGRC.                                               ELTABM  
00214 /                                                                 ELTABM  
00215  01  GX3-RECORD-AREA.                                             ELTABM  
00216      COPY GCTIPGTC.                                               ELTABM  
00217 /                                                                 ELTABM  
00218  01  GXS-RECORD-AREA.                                             ELTABM  
00219      COPY GCTIPGSC.                                               ELTABM  
00220      TITLE  'ELTABM          PROCEDURE DIVISION'.                 ELTABM  
00221 ************************************************************      ELTABM  
00222 *                                                          *      ELTABM  
00223 *    ELTABM MAINLINE                                       *      ELTABM  
00224 *                                                          *      ELTABM  
00225 ************************************************************      ELTABM  
00226                                                                   ELTABM  
00227  PROCEDURE DIVISION.                                              ELTABM  
00228                                                                   ELTABM  
00229      PERFORM 0010-INITIALIZATION.                                 ELTABM  
00230      PERFORM 0100-PROCESS.                                        ELTABM  
00231      GOBACK.                                                      ELTABM  
00232                                                                   ELTABM  
00233 ************************************************************      ELTABM  
00234 *                                                          *      ELTABM  
00235 *    INITIALIZATION                                        *      ELTABM  
00236 *                                                          *      ELTABM  
00237 ************************************************************      ELTABM  
00238                                                                   ELTABM  
00239  0010-INITIALIZATION.                                             ELTABM  
00240      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELTABM  
00241      PERFORM 0060-EST-ADR-KEY-WK-AREA.                            ELTABM  
00242      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELTABM  
00243      PERFORM 0120-EST-ADR-GRP-SPC.                                ELTABM  
00244      PERFORM 0190-INIT-DATA.                                      ELTABM  
00245                                                                   ELTABM  
00246 ************************************************************      ELTABM  
00247 *                                                          *      ELTABM  
00248 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELTABM  
00249 *                                                          *      ELTABM  
00250 ************************************************************      ELTABM  
00251                                                                   ELTABM  
00252  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELTABM  
00253      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTABM  
00254      THEN                                                         ELTABM  
00255         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELTABM  
00256      ELSE                                                         ELTABM  
00257         IF ECA-CIA-PTR = NULL                                     ELTABM  
00258         THEN                                                      ELTABM  
00259            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELTABM  
00260         ELSE                                                      ELTABM  
00261            CALL 'ELUINISM' USING DFHCOMMAREA                      ELTABM  
00262               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELTABM  
00263               END-CALL                                            ELTABM  
00264            SET CIA-ELSSSCB-DDN TO TRUE                            ELTABM  
00265            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELTABM  
00266               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELTABM  
00267               END-CALL                                            ELTABM  
00268            IF CIA-RC-PTR-NULL                                     ELTABM  
00269            THEN                                                   ELTABM  
00270               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELTABM  
00271               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELTABM  
00272            ELSE                                                   ELTABM  
00273               CONTINUE                                            ELTABM  
00274            END-IF                                                 ELTABM  
00275         END-IF                                                    ELTABM  
00276      END-IF.                                                      ELTABM  
00277                                                                   ELTABM  
00278 /***********************************************************      ELTABM  
00279 *                                                          *      ELTABM  
00280 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELTABM  
00281 *                                                          *      ELTABM  
00282 ************************************************************      ELTABM  
00283                                                                   ELTABM  
00284  0060-EST-ADR-KEY-WK-AREA.                                        ELTABM  
00285      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTABM  
00286      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTABM  
00287         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELTABM  
00288         END-CALL.                                                 ELTABM  
00289      IF CIA-RC-PTR-NULL                                           ELTABM  
00290         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTABM  
00291         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTABM  
00292      END-IF.                                                      ELTABM  
00293                                                                   ELTABM  
00294 ************************************************************      ELTABM  
00295 *                                                          *      ELTABM  
00296 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTABM  
00297 *                                                          *      ELTABM  
00298 ************************************************************      ELTABM  
00299                                                                   ELTABM  
00300  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELTABM  
00301      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTABM  
00302      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTABM  
00303         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELTABM  
00304         END-CALL.                                                 ELTABM  
00305      IF CIA-RC-PTR-NULL                                           ELTABM  
00306         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTABM  
00307         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTABM  
00308      END-IF.                                                      ELTABM  
00309                                                                   ELTABM  
00310 ************************************************************      ELTABM  
00311 *                                                          *      ELTABM  
00312 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD     *      ELTABM  
00313 *                                                          *      ELTABM  
00314 ************************************************************      ELTABM  
00315                                                                   ELTABM  
00316  0120-EST-ADR-GRP-SPC.                                            ELTABM  
00317      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTABM  
00318      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTABM  
00319         ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                       ELTABM  
00320         END-CALL.                                                 ELTABM  
00321      IF CIA-RC-PTR-NULL                                           ELTABM  
00322         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTABM  
00323         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTABM  
00324      END-IF.                                                      ELTABM  
00325                                                                   ELTABM  
00326 /***********************************************************      ELTABM  
00327 *                                                          *      ELTABM  
00328 *    INITIALIZE DATA AREAS                                 *      ELTABM  
00329 *                                                          *      ELTABM  
00330 ************************************************************      ELTABM  
00331                                                                   ELTABM  
00332  0190-INIT-DATA.                                                  ELTABM  
00333      COMPUTE PC-GCT-MAX-SUB =   LENGTH OF GCT-CONT-TAB-PTRS       ELTABM  
00334                               / LENGTH OF GCT-CON-TAB-ID-SLOT.    ELTABM  
00335      SET GCT-INDEX        TO PC-GCT-MAX-SUB.                      ELTABM  
00336      SET WS-MAX-GCT-INDEX TO GCT-INDEX.                           ELTABM  
00337      SET GCG-INDEX        TO GCG-COUNT-TAB-PROVN-POINTERS.        ELTABM  
00338      SET WS-MAX-GCG-INDEX TO GCG-INDEX.                           ELTABM  
00339      INITIALIZE WS-ABM-ACCUM-CNT.                                 ELTABM  
00340      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELTABM  
00341                                                                   ELTABM  
00342 /***********************************************************      ELTABM  
00343 *                                                          *      ELTABM  
00344 *        PROCESS                                           *      ELTABM  
00345 *                                                          *      ELTABM  
00346 ************************************************************      ELTABM  
00347                                                                   ELTABM  
00348  0100-PROCESS.                                                    ELTABM  
00349      PERFORM 0110-SCAN-GRP-SPC-FOR-ACCUMS.                        ELTABM  
00350      PERFORM 0120-SCAN-CONTRACTS-FOR-ACCUMS.                      ELTABM  
00351                                                                   ELTABM  
00352      IF WS-ABM-ACCUM-CNT >  0                                     ELTABM  
00353      THEN                                                         ELTABM  
00354          PERFORM 0210-SCAN-FOR-APPLIC-OCCRNCS                     ELTABM  
00355      END-IF.                                                      ELTABM  
00356                                                                   ELTABM  
00357      IF SW-NO-APPLIC-ACCUM-FOUND                                  ELTABM  
00358      THEN                                                         ELTABM  
00359         EVALUATE TRUE                                             ELTABM  
00360            WHEN SSB-PROV-CLASS-INST                               ELTABM  
00361               SET SRP-INST-NOT-APPLICABLE TO TRUE                 ELTABM  
00362            WHEN SSB-PROV-CLASS-PROF                               ELTABM  
00363               SET SRP-PROF-NOT-APPLICABLE TO TRUE                 ELTABM  
00364            WHEN SSB-PROV-CLASS-BOTH                               ELTABM  
00365               SET SRP-NO-ACCUMS-FOUND TO TRUE                     ELTABM  
00366            WHEN OTHER                                             ELTABM  
00367               SET CIA-AB-PGM-LOGIC TO TRUE                        ELTABM  
00368               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELTABM  
00369            END-EVALUATE                                           ELTABM  
00370      END-IF.                                                      ELTABM  
00371                                                                   ELTABM  
00372      SET SRP-TOPIC-ACCUM TO TRUE.                                 ELTABM  
00373                                                                   ELTABM  
00374 * -- LINK TO THE OUTPUT GENERATOR                                 ELTABM  
00375      EXEC CICS LINK PROGRAM ('ELGABM') COMMAREA (DFHCOMMAREA)     ELTABM  
00376         END-EXEC.                                                 ELTABM  
00377                                                                   ELTABM  
00378 /***********************************************************      ELTABM  
00379 *                                                          *      ELTABM  
00380 *    SCAN GROUP SPECIFIC RECORD FOR ACCUMULATORS           *      ELTABM  
00381 *                                                          *      ELTABM  
00382 ************************************************************      ELTABM  
00383                                                                   ELTABM  
00384  0110-SCAN-GRP-SPC-FOR-ACCUMS.                                    ELTABM  
00385      PERFORM WITH TEST BEFORE                                     ELTABM  
00386         VARYING GCG-INDEX FROM 1 BY 1                             ELTABM  
00387           UNTIL GCG-INDEX = WS-MAX-GCG-INDEX                      ELTABM  
00388                 OR GCG-TAB-ID (GCG-INDEX) > PC-ABM                ELTABM  
00389 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELTABM  
00390         IF     GCG-TAB-ID (GCG-INDEX)  =  PC-ABM                  ELTABM  
00391            AND GCG-TAB-SLOT-NO (GCG-INDEX)  >  ZERO               ELTABM  
00392         THEN                                                      ELTABM  
00393 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELTABM  
00394            MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO WS-SLOT-NBR        ELTABM  
00395            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELTABM  
00396         END-IF                                                    ELTABM  
00397         END-PERFORM.                                              ELTABM  
00398                                                                   ELTABM  
00399 /***********************************************************      ELTABM  
00400 *                                                          *      ELTABM  
00401 *    SCAN CONTRACT RECORDS FOR ACCUMULATORS                *      ELTABM  
00402 *                                                          *      ELTABM  
00403 ************************************************************      ELTABM  
00404                                                                   ELTABM  
00405  0120-SCAN-CONTRACTS-FOR-ACCUMS.                                  ELTABM  
00406      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELTABM  
00407      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELTABM  
00408                                                                   ELTABM  
00409      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTABM  
00410      THEN                                                         ELTABM  
00411          PERFORM 0130-SCAN-INST-BAS                               ELTABM  
00412      END-IF.                                                      ELTABM  
00413                                                                   ELTABM  
00414      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTABM  
00415      THEN                                                         ELTABM  
00416          PERFORM 0140-SCAN-PROF-BAS                               ELTABM  
00417      END-IF.                                                      ELTABM  
00418                                                                   ELTABM  
00419      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELTABM  
00420      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELTABM  
00421                                                                   ELTABM  
00422      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTABM  
00423      THEN                                                         ELTABM  
00424          PERFORM 0150-SCAN-INST-SUP                               ELTABM  
00425      END-IF.                                                      ELTABM  
00426                                                                   ELTABM  
00427      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTABM  
00428      THEN                                                         ELTABM  
00429          PERFORM 0160-SCAN-PROF-SUP                               ELTABM  
00430      END-IF.                                                      ELTABM  
00431                                                                   ELTABM  
00432 /***********************************************************      ELTABM  
00433 *                                                          *      ELTABM  
00434 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELTABM  
00435 *                                                          *      ELTABM  
00436 ************************************************************      ELTABM  
00437                                                                   ELTABM  
00438  0130-SCAN-INST-BAS.                                              ELTABM  
00439      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTABM  
00440      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTABM  
00441         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTABM  
00442         END-CALL.                                                 ELTABM  
00443      SET WS-INST-CNTRCT-PTR                                       ELTABM  
00444       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTABM  
00445                                                                   ELTABM  
00446      IF CIA-RC-PTR-NULL                                           ELTABM  
00447      THEN                                                         ELTABM  
00448         CONTINUE                                                  ELTABM  
00449      ELSE                                                         ELTABM  
00450         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTABM  
00451      END-IF.                                                      ELTABM  
00452                                                                   ELTABM  
00453 ************************************************************      ELTABM  
00454 *                                                          *      ELTABM  
00455 *    SCAN PROFESSIONAL BASIC CONTRACT RECORD               *      ELTABM  
00456 *                                                          *      ELTABM  
00457 ************************************************************      ELTABM  
00458                                                                   ELTABM  
00459  0140-SCAN-PROF-BAS.                                              ELTABM  
00460      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTABM  
00461      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTABM  
00462         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTABM  
00463         END-CALL.                                                 ELTABM  
00464      SET WS-PROF-CNTRCT-PTR                                       ELTABM  
00465       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTABM  
00466                                                                   ELTABM  
00467      IF    CIA-RC-PTR-NULL                                        ELTABM  
00468         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELTABM  
00469      THEN                                                         ELTABM  
00470         CONTINUE                                                  ELTABM  
00471      ELSE                                                         ELTABM  
00472         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTABM  
00473      END-IF.                                                      ELTABM  
00474                                                                   ELTABM  
00475 /***********************************************************      ELTABM  
00476 *                                                          *      ELTABM  
00477 *    SCAN INSTITUTIONAL SUPPLEMENTAL CONTRACT RECORD       *      ELTABM  
00478 *                                                          *      ELTABM  
00479 ************************************************************      ELTABM  
00480                                                                   ELTABM  
00481  0150-SCAN-INST-SUP.                                              ELTABM  
00482      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTABM  
00483      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTABM  
00484         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTABM  
00485         END-CALL.                                                 ELTABM  
00486      SET WS-INST-CNTRCT-PTR                                       ELTABM  
00487       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTABM  
00488                                                                   ELTABM  
00489      IF CIA-RC-PTR-NULL                                           ELTABM  
00490      THEN                                                         ELTABM  
00491         CONTINUE                                                  ELTABM  
00492      ELSE                                                         ELTABM  
00493         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTABM  
00494      END-IF.                                                      ELTABM  
00495                                                                   ELTABM  
00496 ************************************************************      ELTABM  
00497 *                                                          *      ELTABM  
00498 *    SCAN PROFESSIONAL SUPPLEMENTAL CONTRACT RECORD        *      ELTABM  
00499 *                                                          *      ELTABM  
00500 ************************************************************      ELTABM  
00501                                                                   ELTABM  
00502  0160-SCAN-PROF-SUP.                                              ELTABM  
00503      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTABM  
00504      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTABM  
00505         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELTABM  
00506         END-CALL.                                                 ELTABM  
00507      SET WS-PROF-CNTRCT-PTR                                       ELTABM  
00508       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELTABM  
00509                                                                   ELTABM  
00510      IF    CIA-RC-PTR-NULL                                        ELTABM  
00511         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELTABM  
00512      THEN                                                         ELTABM  
00513         CONTINUE                                                  ELTABM  
00514      ELSE                                                         ELTABM  
00515         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELTABM  
00516      END-IF.                                                      ELTABM  
00517                                                                   ELTABM  
00518 /***********************************************************      ELTABM  
00519 *                                                          *      ELTABM  
00520 *    SCAN A CONTRACT RECORD FOR ACCUMULATORS               *      ELTABM  
00521 *                                                          *      ELTABM  
00522 ************************************************************      ELTABM  
00523                                                                   ELTABM  
00524  0170-SCAN-CONTRACT-FOR-ACCUMS.                                   ELTABM  
00525      PERFORM WITH TEST BEFORE                                     ELTABM  
00526         VARYING GCT-TAB-INDEX FROM 1 BY 1                         ELTABM  
00527           UNTIL GCT-TAB-INDEX > WS-MAX-GCT-INDEX                  ELTABM  
00528                 OR GCT-CON-TAB-ID (GCT-TAB-INDEX) > PC-ABM        ELTABM  
00529 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELTABM  
00530         IF     GCT-CON-TAB-ID (GCT-TAB-INDEX) = PC-ABM            ELTABM  
00531            AND GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  > ZEROS          ELTABM  
00532         THEN                                                      ELTABM  
00533 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELTABM  
00534            MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  TO  WS-SLOT-NBR ELTABM  
00535            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELTABM  
00536         END-IF                                                    ELTABM  
00537         END-PERFORM.                                              ELTABM  
00538                                                                   ELTABM  
00539 /***********************************************************      ELTABM  
00540 *                                                          *      ELTABM  
00541 *    SAVE UNIQUE ACCUMULATOR SLOT NUMBER                   *      ELTABM  
00542 *                                                          *      ELTABM  
00543 ************************************************************      ELTABM  
00544                                                                   ELTABM  
00545  0200-SAVE-UNQ-ACCUM-SLOT-NBR.                                    ELTABM  
00546                                                                   ELTABM  
00547 * -- SCAN TABLE OF ACCUM SLOT NUMBERS FOR DUPLICATE               ELTABM  
00548      SET SW-UNQ-SLOT-NBR TO TRUE.                                 ELTABM  
00549      PERFORM WITH TEST BEFORE                                     ELTABM  
00550         VARYING WS-ABM-SUB FROM 1 BY 1                            ELTABM  
00551           UNTIL    WS-ABM-SUB > WS-ABM-ACCUM-CNT                  ELTABM  
00552                 OR SW-DUP-SLOT-NBR                                ELTABM  
00553         IF WS-SLOT-NBR = ACCUM-SLOT-NBR (WS-ABM-SUB)              ELTABM  
00554         THEN                                                      ELTABM  
00555            SET SW-DUP-SLOT-NBR TO TRUE                            ELTABM  
00556         END-IF                                                    ELTABM  
00557         END-PERFORM.                                              ELTABM  
00558                                                                   ELTABM  
00559 * -- IF SLOT NUMBER IS UNIQUE, ADD IT TO THE HOLD TABLE           ELTABM  
00560      IF SW-UNQ-SLOT-NBR                                           ELTABM  
00561      THEN                                                         ELTABM  
00562         ADD 1 TO  WS-ABM-ACCUM-CNT                                ELTABM  
00563         MOVE WS-SLOT-NBR TO ACCUM-SLOT-NBR(WS-ABM-ACCUM-CNT)      ELTABM  
00564      END-IF.                                                      ELTABM  
00565                                                                   ELTABM  
00566 /***********************************************************      ELTABM  
00567 *                                                          *      ELTABM  
00568 *    SCAN ABM ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELTABM  
00569 *                                                          *      ELTABM  
00570 ************************************************************      ELTABM  
00571                                                                   ELTABM  
00572  0210-SCAN-FOR-APPLIC-OCCRNCS.                                    ELTABM  
00573      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELTABM  
00574      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELTABM  
00575      PERFORM 0220-DELETE-ABM-SUMMARY-FILE.                        ELTABM  
00576      PERFORM 0230-ALLOC-WORKFILE-REC-AREA.                        ELTABM  
00577                                                                   ELTABM  
00578 * -- READ AND SCAN EACH ACCUMULATOR TABULAR                       ELTABM  
00579      PERFORM WITH TEST BEFORE                                     ELTABM  
00580         VARYING WS-ABM-SUB FROM 1 BY 1                            ELTABM  
00581           UNTIL WS-ABM-SUB > WS-ABM-ACCUM-CNT                     ELTABM  
00582 *    -- OBTAIN ACCUMULATOR TABULAR RECORD                         ELTABM  
00583         MOVE PC-ABM TO KWA-PROVISION-ID                           ELTABM  
00584         MOVE ACCUM-SLOT-NBR (WS-ABM-SUB) TO KWA-PROVISION-SLOT-NO ELTABM  
00585         PERFORM 0240-READ-TABULAR-REC                             ELTABM  
00586 *    -- SCAN ACCUMULATOR TABULAR                                  ELTABM  
00587         PERFORM 0300-TEST-ABM-OCCURRENCE                          ELTABM  
00588            VARYING GAA-INDEX FROM 1 BY 1                          ELTABM  
00589              UNTIL GAA-INDEX = GAA-ENTRY-COUNT                    ELTABM  
00590      END-PERFORM.                                                 ELTABM  
00591                                                                   ELTABM  
00592                                                                   ELTABM  
00593 /***********************************************************      ELTABM  
00594 *                                                          *      ELTABM  
00595 *        DELETE ABM SUMMARY FILE                           *      ELTABM  
00596 *                                                          *      ELTABM  
00597 ************************************************************      ELTABM  
00598                                                                   ELTABM  
00599  0220-DELETE-ABM-SUMMARY-FILE.                                    ELTABM  
00600      SET IOP-DEL TO TRUE.                                         ELTABM  
00601      SET IOP-FCQ-NONE TO TRUE.                                    ELTABM  
00602      SET IOP-KVQ-NONE TO TRUE.                                    ELTABM  
00603      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTABM  
00604                                                                   ELTABM  
00605 /***********************************************************      ELTABM  
00606 *                                                          *      ELTABM  
00607 *    ALLOCATE WORKFILE RECORD AREA                         *      ELTABM  
00608 *                                                          *      ELTABM  
00609 ************************************************************      ELTABM  
00610                                                                   ELTABM  
00611  0230-ALLOC-WORKFILE-REC-AREA.                                    ELTABM  
00612      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELTABM  
00613      SET CIA-STG-GETMAIN TO TRUE.                                 ELTABM  
00614      SET IOP-GETMAIN-REC TO TRUE.                                 ELTABM  
00615      COMPUTE IOP-MAX-REC-LEN =                                    ELTABM  
00616              LENGTH OF ACCUM-FIXED-AREA                           ELTABM  
00617 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELTABM  
00618            + LENGTH OF ACCUM-VARIABLE-AREA                        ELTABM  
00619            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELTABM  
00620 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELTABM  
00621 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELTABM  
00622                                                                   ELTABM  
00623      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTABM  
00624      IF IOP-REC-PTR = NULLS                                       ELTABM  
00625      THEN                                                         ELTABM  
00626         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTABM  
00627         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTABM  
00628      ELSE                                                         ELTABM  
00629         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELTABM  
00630      END-IF.                                                      ELTABM  
00631                                                                   ELTABM  
00632 /***********************************************************      ELTABM  
00633 *                                                          *      ELTABM  
00634 *    READ TABULAR RECORD                                   *      ELTABM  
00635 *                                                          *      ELTABM  
00636 ************************************************************      ELTABM  
00637                                                                   ELTABM  
00638  0240-READ-TABULAR-REC.                                           ELTABM  
00639      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELTABM  
00640      SET IOP-RD TO TRUE.                                          ELTABM  
00641      SET IOP-FCQ-NONE TO TRUE.                                    ELTABM  
00642      SET IOP-KVQ-EQ TO TRUE.                                      ELTABM  
00643      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTABM  
00644      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTABM  
00645      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTABM  
00646      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTABM  
00647                                                                   ELTABM  
00648      EVALUATE TRUE                                                ELTABM  
00649        WHEN IOP-RC-OK                                             ELTABM  
00650           SET ADDRESS OF GAA-RECORD-AREA TO IOP-REC-PTR           ELTABM  
00651           SET IOP-REC-PTR TO NULLS                                ELTABM  
00652           SET GAA-INDEX   TO GAA-ENTRY-COUNT                      ELTABM  
00653           SET WS-MAX-GAA-INDEX TO GAA-INDEX                       ELTABM  
00654        WHEN IOP-RC-NOTFND                                         ELTABM  
00655           SET CIA-AB-NOTFND-GCTABULR TO TRUE                      ELTABM  
00656           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELTABM  
00657        WHEN OTHER                                                 ELTABM  
00658           SET CIA-AB-CRITIO TO TRUE                               ELTABM  
00659           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELTABM  
00660        END-EVALUATE.                                              ELTABM  
00661                                                                   ELTABM  
00662 /***********************************************************      ELTABM  
00663 *                                                          *      ELTABM  
00664 *        TEST ABM OCCURS                                   *      ELTABM  
00665 *                                                          *      ELTABM  
00666 ************************************************************      ELTABM  
00667                                                                   ELTABM  
00668  0300-TEST-ABM-OCCURRENCE.                                        ELTABM  
00669                                                                   ELTABM  
00670      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELTABM  
00671      MOVE GAA-BAMA-L-O-B (GAA-INDEX) TO WS-LOB-ACCUM-OCCRNC.      ELTABM  
00672      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELTABM  
00673      PERFORM 0320-SCAN-FOR-INTERNALS.                             ELTABM  
00674      IF SW-OCCRNC-APPLIES                                         ELTABM  
00675      THEN                                                         ELTABM  
00676 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELTABM  
00677         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELTABM  
00678         PERFORM 0340-INIT-ACCUM-EXTRACT                           ELTABM  
00679         PERFORM 0350-EXTRACT-ACCUM                                ELTABM  
00680         PERFORM 0490-CHK-EXTRACT-DATA-INTGRTY                     ELTABM  
00681         PERFORM 0710-WRITE-EXTRACT-RECORD                         ELTABM  
00682      END-IF.                                                      ELTABM  
00683                                                                   ELTABM  
00684 ************************************************************      ELTABM  
00685 *                                                          *      ELTABM  
00686 *        INITIALIZE OCCURRENCE                             *      ELTABM  
00687 *                                                          *      ELTABM  
00688 ************************************************************      ELTABM  
00689                                                                   ELTABM  
00690  0310-INITIALIZE-OCCURRENCE.                                      ELTABM  
00691      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELTABM  
00692      SET SW-HAS-NO-IBGR                                           ELTABM  
00693          SW-HAS-NO-IDGD                                           ELTABM  
00694          SW-HAS-NO-IPGN                                           ELTABM  
00695          SW-HAS-NO-IPGP                                           ELTABM  
00696          SW-HAS-NO-IPGT                                           ELTABM  
00697          SW-HAS-NO-IPGS                                           ELTABM  
00698       TO TRUE.                                                    ELTABM  
00699      INITIALIZE WS-IBGR-SLOT-NBR                                  ELTABM  
00700                 WS-IDGD-SLOT-NBR                                  ELTABM  
00701                 WS-IPGN-SLOT-NBR                                  ELTABM  
00702                 WS-IPGP-SLOT-NBR                                  ELTABM  
00703                 WS-IPGT-SLOT-NBR                                  ELTABM  
00704                 WS-IPGS-SLOT-NBR.                                 ELTABM  
00705      SET SW-INTRNL-INST-PROV-CL-NOT-DET                           ELTABM  
00706          SW-INTRNL-PROF-PROV-CL-NOT-DET                           ELTABM  
00707          SW-INTRNL-PROF-PROV-SP-NOT-DET                           ELTABM  
00708       TO TRUE.                                                    ELTABM  
00709                                                                   ELTABM  
00710                                                                   ELTABM  
00711 ************************************************************      ELTABM  
00712 *                                                          *      ELTABM  
00713 *        SCAN FOR INTERNAL TABULARS                        *      ELTABM  
00714 *                                                          *      ELTABM  
00715 ************************************************************      ELTABM  
00716                                                                   ELTABM  
00717  0320-SCAN-FOR-INTERNALS.                                         ELTABM  
00718 *    (THIS IS DONE NOW IN CASE IPGT OR IBGR IS NEEDED TO DETERMINEELTABM  
00719 *     WHETHER OCCURRENCE IS INSTITUTIONAL OR PROFESSIONAL.)       ELTABM  
00720      PERFORM 0330-SCAN-THE-INTERNAL-TABULAR                       ELTABM  
00721         VARYING GAA-INT-INDEX FROM 1 BY 1                         ELTABM  
00722           UNTIL    GAA-INT-INDEX                                  ELTABM  
00723                 >= GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX).        ELTABM  
00724                                                                   ELTABM  
00725      EVALUATE TRUE ALSO TRUE                                      ELTABM  
00726         WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                        ELTABM  
00727            SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE                  ELTABM  
00728            SET SW-OCCRNC-APPLIES TO TRUE                          ELTABM  
00729         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH                 ELTABM  
00730            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELTABM  
00731            PERFORM 0550-CHK-INTRNL-TAB-PROV-CL                    ELTABM  
00732         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST                 ELTABM  
00733            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELTABM  
00734            SET SW-OCCRNC-APPLIES TO TRUE                          ELTABM  
00735         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH                 ELTABM  
00736            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELTABM  
00737            PERFORM 0550-CHK-INTRNL-TAB-PROV-CL                    ELTABM  
00738            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELTABM  
00739            PERFORM 0551-CHK-INTRNL-TAB-PROV-SP                    ELTABM  
00740         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF                 ELTABM  
00741            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELTABM  
00742            SET SW-OCCRNC-APPLIES TO TRUE                          ELTABM  
00743         WHEN OTHER                                                ELTABM  
00744            CONTINUE                                               ELTABM  
00745         END-EVALUATE.                                             ELTABM  
00746                                                                   ELTABM  
00747 /***********************************************************      ELTABM  
00748 *                                                          *      ELTABM  
00749 *        SCAN THE INTERNAL TABULARS                        *      ELTABM  
00750 *                                                          *      ELTABM  
00751 ************************************************************      ELTABM  
00752                                                                   ELTABM  
00753  0330-SCAN-THE-INTERNAL-TABULAR.                                  ELTABM  
00754      IF GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX) > 0               ELTABM  
00755      THEN                                                         ELTABM  
00756         MOVE GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX)              ELTABM  
00757           TO WS-SLOT-NBR                                          ELTABM  
00758         EVALUATE GAA-INT-ID (GAA-INDEX, GAA-INT-INDEX)            ELTABM  
00759            WHEN PC-IBGR                                           ELTABM  
00760               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELTABM  
00761               SET SW-HAS-IBGR                                     ELTABM  
00762                TO TRUE                                            ELTABM  
00763            WHEN PC-IDGD                                           ELTABM  
00764               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELTABM  
00765               SET SW-HAS-IDGD                                     ELTABM  
00766                TO TRUE                                            ELTABM  
00767            WHEN PC-IPGP                                           ELTABM  
00768               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELTABM  
00769               SET SW-HAS-IPGP                                     ELTABM  
00770                TO TRUE                                            ELTABM  
00771            WHEN PC-IPGN                                           ELTABM  
00772               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELTABM  
00773               SET SW-HAS-IPGN                                     ELTABM  
00774                TO TRUE                                            ELTABM  
00775            WHEN PC-IPGT                                           ELTABM  
00776               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELTABM  
00777               SET SW-HAS-IPGT                                     ELTABM  
00778                TO TRUE                                            ELTABM  
00779            WHEN PC-IPGS                                           ELTABM  
00780               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELTABM  
00781               SET SW-HAS-IPGS                                     ELTABM  
00782                TO TRUE                                            ELTABM  
00783            WHEN OTHER                                             ELTABM  
00784               CONTINUE                                            ELTABM  
00785            END-EVALUATE                                           ELTABM  
00786      END-IF.                                                      ELTABM  
00787                                                                   ELTABM  
00788 /***********************************************************      ELTABM  
00789 *                                                          *      ELTABM  
00790 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELTABM  
00791 *                                                          *      ELTABM  
00792 ************************************************************      ELTABM  
00793                                                                   ELTABM  
00794  0340-INIT-ACCUM-EXTRACT.                                         ELTABM  
00795      INITIALIZE ACCUM-FIXED-AREA.                                 ELTABM  
00796      SET ACCUM-ABM TO TRUE.                                       ELTABM  
00797      MOVE +1 TO  ACCUM-ASCEND-DESCEND-COUNT.                      ELTABM  
00798      SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELTABM  
00799      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELTABM  
00800      INITIALIZE ACCUM-COPAY-ENTRY (1).                            ELTABM  
00801                                                                   ELTABM  
00802 /***********************************************************      ELTABM  
00803 *                                                          *      ELTABM  
00804 *        SUMMARIZE ABM TOPIC LEVEL DATA ELEMENTS           *      ELTABM  
00805 *                                                          *      ELTABM  
00806 ************************************************************      ELTABM  
00807                                                                   ELTABM  
00808  0350-EXTRACT-ACCUM.                                              ELTABM  
00809                                                                   ELTABM  
00810 * -- SET FIXED PORTION DATA ELEMENTS                              ELTABM  
00811      MOVE GAA-BAMA-FYI-VALUE (GAA-INDEX) TO ACCUM-FYI-VALUE.      ELTABM  
00812      MOVE GAA-BAMA-COST-CONTAIN-IND (GAA-INDEX)                   ELTABM  
00813        TO ACCUM-COST-CONTAIN-IND.                                 ELTABM  
00814      MOVE GAA-BAMA-SERVICE-GROUP (GAA-INDEX)                      ELTABM  
00815        TO ACCUM-SERVICE-GROUP.                                    ELTABM  
00816      MOVE GAA-BAMA-PLACE-OF-TREATMENT (GAA-INDEX)                 ELTABM  
00817        TO ACCUM-PLACE-OF-TREATMENT.                               ELTABM  
00818      MOVE GAA-BAMA-CO-PAY-IND (GAA-INDEX)                         ELTABM  
00819        TO ACCUM-CO-PAY-IND (1).                                   ELTABM  
00820      MOVE GAA-BAMA-INTERNAL-DESCRIPTOR (GAA-INDEX)                ELTABM  
00821        TO ACCUM-INTERNAL-DESCRIPTOR.                              ELTABM  
00822      MOVE GAA-BAMA-DAY-FACTOR-IND (GAA-INDEX)                     ELTABM  
00823        TO ACCUM-DAY-FACTOR-IND.                                   ELTABM  
00824      MOVE GAA-BAMA-CLAIM-LVL-ACCUM-IND (GAA-INDEX)                ELTABM  
00825        TO ACCUM-CLAIM-LVL-ACCUM-IND.                              ELTABM  
00826      MOVE GAA-BAMA-BENEFIT-PERIOD (GAA-INDEX)                     ELTABM  
00827        TO ACCUM-BENEFIT-PERIOD.                                   ELTABM  
00828      MOVE GAA-BAMA-BEN-PER-TIME-FCTR (GAA-INDEX)                  ELTABM  
00829        TO ACCUM-BEN-PER-TIME-FCTR.                                ELTABM  
00830      MOVE GAA-BAMA-BEN-PER-TIME-QUAL (GAA-INDEX)                  ELTABM  
00831        TO ACCUM-BEN-PER-TIME-QUAL.                                ELTABM  
00832      MOVE GAA-BAMA-INTERVAL-TIME-FCTR (GAA-INDEX)                 ELTABM  
00833        TO ACCUM-INTERVAL-TIME-FCTR.                               ELTABM  
00834      MOVE GAA-BAMA-INTERVAL-TYPE (GAA-INDEX)                      ELTABM  
00835        TO ACCUM-INTERVAL-TYPE.                                    ELTABM  
00836      MOVE GAA-BAMA-INTERVAL-OVRD-IND (GAA-INDEX)                  ELTABM  
00837        TO ACCUM-INTERVAL-OVRD-IND.                                ELTABM  
00838      MOVE GAA-BAMA-INTERVAL-OVRD-VALUE (GAA-INDEX)                ELTABM  
00839        TO ACCUM-INTERVAL-OVRD-VALUE.                              ELTABM  
00840      MOVE GAA-BAMA-L-O-B (GAA-INDEX) TO ACCUM-L-O-B.              ELTABM  
00841      MOVE GAA-BAMA-REINSTATEMENT-IND (GAA-INDEX)                  ELTABM  
00842        TO ACCUM-REINSTATEMENT-IND.                                ELTABM  
00843      MOVE GAA-BAMA-DEFINITION (GAA-INDEX) TO ACCUM-DEFINITION.    ELTABM  
00844      SET CARRY-OVER-CREDIT-IND-NA                                 ELTABM  
00845       TO TRUE.                                                    ELTABM  
00846      MOVE GAA-BAMA-CONDITION (GAA-INDEX) TO ACCUM-CONDITION.      ELTABM  
00847      MOVE GAA-BAMA-FAM-OR-INDIV (GAA-INDEX)                       ELTABM  
00848        TO ACCUM-FAM-OR-INDIV.                                     ELTABM  
00849      SET DED-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELTABM  
00850      SET OPX-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELTABM  
00851      MOVE GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX)                    ELTABM  
00852        TO ACCUM-VALUE-QUALIFIER.                                  ELTABM  
00853      MOVE GAA-BAMA-RELATIONSHIP-IND (GAA-INDEX)                   ELTABM  
00854        TO ACCUM-RELATIONSHIP-IND.                                 ELTABM  
00855      MOVE GAA-BAMA-AGE-LIMIT-FROM (GAA-INDEX)                     ELTABM  
00856        TO ACCUM-AGE-LIMIT-FROM-VAL.                               ELTABM  
00857      MOVE GAA-BAMA-AGE-QUAL-IND-FROM (GAA-INDEX)                  ELTABM  
00858        TO ACCUM-AGE-LIMIT-FROM-IND.                               ELTABM  
00859      MOVE GAA-BAMA-AGE-LIMIT-TO (GAA-INDEX)                       ELTABM  
00860        TO ACCUM-AGE-LIMIT-TO-VAL.                                 ELTABM  
00861      MOVE GAA-BAMA-AGE-QUAL-IND-TO (GAA-INDEX)                    ELTABM  
00862        TO ACCUM-AGE-LIMIT-TO-IND.                                 ELTABM  
00863                                                                   ELTABM  
00864 * -- SET OCCURRENCE PROVIDER CLASS INFORMATION                    ELTABM  
00865      EVALUATE TRUE ALSO TRUE                                      ELTABM  
00866         WHEN      SW-INTRNL-INST-PROV-CL                          ELTABM  
00867              ALSO SW-INTRNL-NOT-PROF-PROV-CL                      ELTABM  
00868            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELTABM  
00869         WHEN      SW-INTRNL-NOT-INST-PROV-CL                      ELTABM  
00870              ALSO SW-INTRNL-PROF-PROV-CL                          ELTABM  
00871            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELTABM  
00872         WHEN OTHER                                                ELTABM  
00873            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELTABM  
00874         END-EVALUATE.                                             ELTABM  
00875 * -- SET OCCURRENCE PROVIDER SPEC INFORMATION                     ELTABM  
00876         IF  SW-INTRNL-PROF-PROV-CL                                ELTABM  
00877            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELTABM  
00878         ELSE                                                      ELTABM  
00879            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELTABM  
00880         END-IF.                                                   ELTABM  
00881 * -- SET VARIABLE PORTION DATA ELEMENTS                           ELTABM  
00882      MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                        ELTABM  
00883        TO ACCUM-VALUE-LIMIT (ASC-DES-INDEX).                      ELTABM  
00884      MOVE WS-IBGR-SLOT-NBR                                        ELTABM  
00885        TO ACCUM-IBGR-SLOT-NBR (ASC-DES-INDEX).                    ELTABM  
00886      MOVE WS-IDGD-SLOT-NBR                                        ELTABM  
00887        TO ACCUM-IDGD-SLOT-NBR (ASC-DES-INDEX).                    ELTABM  
00888      MOVE WS-IPGN-SLOT-NBR                                        ELTABM  
00889        TO ACCUM-IPGN-SLOT-NBR (ASC-DES-INDEX).                    ELTABM  
00890      MOVE WS-IPGP-SLOT-NBR                                        ELTABM  
00891        TO ACCUM-IPGP-SLOT-NBR (ASC-DES-INDEX).                    ELTABM  
00892      MOVE WS-IPGT-SLOT-NBR                                        ELTABM  
00893        TO ACCUM-IPGT-SLOT-NBR (ASC-DES-INDEX).                    ELTABM  
00894      MOVE WS-IPGS-SLOT-NBR                                        ELTABM  
00895        TO ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX).                    ELTABM  
00896                                                                   ELTABM  
00897                                                                   ELTABM  
00898                                                                   ELTABM  
00899 /***********************************************************      ELTABM  
00900 *                                                          *      ELTABM  
00901 *    CHECK EXTRACT DATA INTEGRITY                          *      ELTABM  
00902 *                                                          *      ELTABM  
00903 ************************************************************      ELTABM  
00904                                                                   ELTABM  
00905  0490-CHK-EXTRACT-DATA-INTGRTY.                                   ELTABM  
00906      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELTABM  
00907      THEN                                                         ELTABM  
00908         SET FYI-VALUE-NA TO TRUE                                  ELTABM  
00909      END-IF.                                                      ELTABM  
00910                                                                   ELTABM  
00911      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELTABM  
00912      THEN                                                         ELTABM  
00913         SET COST-CONTAIN-IND-NA TO TRUE                           ELTABM  
00914      END-IF.                                                      ELTABM  
00915                                                                   ELTABM  
00916      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELTABM  
00917      THEN                                                         ELTABM  
00918         SET PLACE-OF-TREATMENT-NA TO TRUE                         ELTABM  
00919      END-IF.                                                      ELTABM  
00920                                                                   ELTABM  
00921      IF ACCUM-BEN-PER-TIME-QUAL = ZEROS OR SPACES OR LOW-VALUES   ELTABM  
00922      THEN                                                         ELTABM  
00923         SET BEN-PER-TIME-QUAL-NA TO TRUE                          ELTABM  
00924      END-IF.                                                      ELTABM  
00925                                                                   ELTABM  
00926      IF ACCUM-INTERVAL-TYPE = ZEROS OR SPACES OR LOW-VALUES       ELTABM  
00927      THEN                                                         ELTABM  
00928         SET INTERVAL-TYPE-NA TO TRUE                              ELTABM  
00929      END-IF.                                                      ELTABM  
00930                                                                   ELTABM  
00931      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELTABM  
00932      THEN                                                         ELTABM  
00933         SET INTERVAL-OVRD-IND-NA TO TRUE                          ELTABM  
00934      END-IF.                                                      ELTABM  
00935                                                                   ELTABM  
00936      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELTABM  
00937      THEN                                                         ELTABM  
00938         SET L-O-B-NA TO TRUE                                      ELTABM  
00939      END-IF.                                                      ELTABM  
00940                                                                   ELTABM  
00941      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELTABM  
00942      THEN                                                         ELTABM  
00943         SET REINSTATEMENT-IND-NA TO TRUE                          ELTABM  
00944      END-IF.                                                      ELTABM  
00945                                                                   ELTABM  
00946      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELTABM  
00947      THEN                                                         ELTABM  
00948         SET DEFINITION-NA TO TRUE                                 ELTABM  
00949      END-IF.                                                      ELTABM  
00950                                                                   ELTABM  
00951      IF   ACCUM-CARRY-OVER-CREDIT-IND                             ELTABM  
00952         = ZEROS OR SPACES OR LOW-VALUES                           ELTABM  
00953      THEN                                                         ELTABM  
00954         SET CARRY-OVER-CREDIT-IND-NA TO TRUE                      ELTABM  
00955      END-IF.                                                      ELTABM  
00956                                                                   ELTABM  
00957      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELTABM  
00958      THEN                                                         ELTABM  
00959         SET ASCEND-DESCEND-IND-NA TO TRUE                         ELTABM  
00960      END-IF.                                                      ELTABM  
00961                                                                   ELTABM  
00962      IF ACCUM-FAM-OR-INDIV = ZEROS OR SPACES OR LOW-VALUES        ELTABM  
00963      THEN                                                         ELTABM  
00964         SET FAM-OR-INDIV-NA TO TRUE                               ELTABM  
00965      END-IF.                                                      ELTABM  
00966                                                                   ELTABM  
00967      IF ACCUM-VALUE-QUALIFIER = ZEROS OR SPACES OR LOW-VALUES     ELTABM  
00968      THEN                                                         ELTABM  
00969         SET VALUE-QUALIFIER-NA TO TRUE                            ELTABM  
00970      END-IF.                                                      ELTABM  
00971                                                                   ELTABM  
00972      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELTABM  
00973      THEN                                                         ELTABM  
00974         SET RELATIONSHIP-IND-NA TO TRUE                           ELTABM  
00975      END-IF.                                                      ELTABM  
00976                                                                   ELTABM  
00977      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELTABM  
00978      THEN                                                         ELTABM  
00979         SET AGE-LMT-TO-IND-NA TO TRUE                             ELTABM  
00980      END-IF.                                                      ELTABM  
00981                                                                   ELTABM  
00982      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELTABM  
00983      THEN                                                         ELTABM  
00984         SET AGE-LMT-FROM-IND-NA TO TRUE                           ELTABM  
00985      END-IF.                                                      ELTABM  
00986                                                                   ELTABM  
00987      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELTABM  
00988      THEN                                                         ELTABM  
00989         SET LMT-MANDATORY-IND-NA TO TRUE                          ELTABM  
00990      END-IF.                                                      ELTABM  
00991                                                                   ELTABM  
00992      IF ACCUM-CO-PAY-IND (1) = ZEROS OR SPACES OR LOW-VALUES      ELTABM  
00993      THEN                                                         ELTABM  
00994         SET CO-PAY-IND-NA (1) TO TRUE                             ELTABM  
00995      END-IF.                                                      ELTABM  
00996                                                                   ELTABM  
00997      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELTABM  
00998      THEN                                                         ELTABM  
00999         SET SERVICE-GROUP-NA TO TRUE                              ELTABM  
01000      END-IF.                                                      ELTABM  
01001                                                                   ELTABM  
01002      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELTABM  
01003      THEN                                                         ELTABM  
01004         SET INTERNAL-DESCRIPTOR-NA TO TRUE                        ELTABM  
01005      END-IF.                                                      ELTABM  
01006                                                                   ELTABM  
01007      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELTABM  
01008      THEN                                                         ELTABM  
01009         SET DAY-FACTOR-IND-NA TO TRUE                             ELTABM  
01010      END-IF.                                                      ELTABM  
01011                                                                   ELTABM  
01012      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELTABM  
01013      THEN                                                         ELTABM  
01014         SET CLAIM-LVL-ACCUM-IND-NA TO TRUE                        ELTABM  
01015      END-IF.                                                      ELTABM  
01016                                                                   ELTABM  
01017      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELTABM  
01018      THEN                                                         ELTABM  
01019         SET BEN-PER-MAX-OVRD-IND-NA TO TRUE                       ELTABM  
01020      END-IF.                                                      ELTABM  
01021                                                                   ELTABM  
01022      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELTABM  
01023      THEN                                                         ELTABM  
01024         SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE                        ELTABM  
01025      END-IF.                                                      ELTABM  
01026                                                                   ELTABM  
01027 /***********************************************************      ELTABM  
01028 *                                                          *      ELTABM  
01029 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER CLASS   *      ELTABM  
01030 *                                                          *      ELTABM  
01031 ************************************************************      ELTABM  
01032                                                                   ELTABM  
01033  0550-CHK-INTRNL-TAB-PROV-CL.                                     ELTABM  
01034      IF SW-HAS-IPGT                                               ELTABM  
01035      THEN                                                         ELTABM  
01036         PERFORM 0560-CHK-IPGT-PROV-CL                             ELTABM  
01037      ELSE                                                         ELTABM  
01038         IF SW-HAS-IBGR                                            ELTABM  
01039         THEN                                                      ELTABM  
01040            PERFORM 0640-CHK-IBGR-PROV-CL                          ELTABM  
01041         ELSE                                                      ELTABM  
01042            SET SW-OCCRNC-APPLIES TO TRUE                          ELTABM  
01043         END-IF                                                    ELTABM  
01044      END-IF.                                                      ELTABM  
01045                                                                   ELTABM  
01046 /***********************************************************      ELTABM  
01047 *                                                          *      ELTABM  
01048 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER SPEC    *      ELTABM  
01049 *                                                          *      ELTABM  
01050 ************************************************************      ELTABM  
01051                                                                   ELTABM  
01052  0551-CHK-INTRNL-TAB-PROV-SP.                                     ELTABM  
01053      IF SW-HAS-IPGS                                               ELTABM  
01054      THEN                                                         ELTABM  
01055         PERFORM 0561-CHK-IPGS-PROV-SP                             ELTABM  
01056      END-IF.                                                      ELTABM  
01057                                                                   ELTABM  
01058 /*****************************************************************ELTABM  
01059 *                                                                *ELTABM  
01060 *    CHECK IPGT INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELTABM  
01061 *                                                                *ELTABM  
01062 ******************************************************************ELTABM  
01063                                                                   ELTABM  
01064  0560-CHK-IPGT-PROV-CL.                                           ELTABM  
01065      MOVE PC-IPGT TO KWA-PROVISION-ID.                            ELTABM  
01066      MOVE WS-IPGT-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTABM  
01067      PERFORM 0660-READ-INTRLN-TAB.                                ELTABM  
01068      SET ADDRESS OF GX3-RECORD-AREA TO IOP-REC-PTR.               ELTABM  
01069      SET IOP-REC-PTR                TO NULLS.                     ELTABM  
01070      SET GX3-INDEX                  TO GX3-ENTRY-COUNT.           ELTABM  
01071      SET WS-MAX-GX3-INDEX           TO GX3-INDEX.                 ELTABM  
01072                                                                   ELTABM  
01073      IF GX3-ID-ARGUMENT-INCLUDED                                  ELTABM  
01074      THEN                                                         ELTABM  
01075         PERFORM 0570-CHK-INCLD-TYPE-IPGT                          ELTABM  
01076      ELSE                                                         ELTABM  
01077          PERFORM 0600-CHK-EXCLD-TYPE-IPGT                         ELTABM  
01078      END-IF.                                                      ELTABM  
01079                                                                   ELTABM  
01080 /*****************************************************************ELTABM  
01081 *                                                                *ELTABM  
01082 *    CHECK IPGS INTERNAL TABULAR TO DETERMINE PROVIDER SPEC      *ELTABM  
01083 *                                                                *ELTABM  
01084 ******************************************************************ELTABM  
01085                                                                   ELTABM  
01086  0561-CHK-IPGS-PROV-SP.                                           ELTABM  
01087      MOVE PC-IPGS TO KWA-PROVISION-ID.                            ELTABM  
01088      MOVE WS-IPGS-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTABM  
01089      PERFORM 0660-READ-INTRLN-TAB.                                ELTABM  
01090      SET ADDRESS OF GXS-RECORD-AREA TO IOP-REC-PTR.               ELTABM  
01091      SET IOP-REC-PTR                TO NULLS.                     ELTABM  
01092      SET GXS-INDEX                  TO GXS-ENTRY-COUNT.           ELTABM  
01093      SET WS-MAX-GXS-INDEX           TO GXS-INDEX.                 ELTABM  
01094                                                                   ELTABM  
01095      IF GXS-ID-ARGUMENT-INCLUDED                                  ELTABM  
01096         PERFORM 0571-CHK-INCLD-TYPE-IPGS                          ELTABM  
01097      ELSE                                                         ELTABM  
01098          PERFORM 0601-CHK-EXCLD-TYPE-IPGS                         ELTABM  
01099      END-IF.                                                      ELTABM  
01100                                                                   ELTABM  
01101 ************************************************************      ELTABM  
01102 *                                                          *      ELTABM  
01103 *    CHECK INCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELTABM  
01104 *                                                          *      ELTABM  
01105 ************************************************************      ELTABM  
01106                                                                   ELTABM  
01107  0570-CHK-INCLD-TYPE-IPGT.                                        ELTABM  
01108      SET CFT2-IDX TO 1.                                           ELTABM  
01109      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTABM  
01110          SW-INTRNL-NOT-PROF-PROV-CL                               ELTABM  
01111       TO TRUE.                                                    ELTABM  
01112      PERFORM 0580-TEST-IPGT-INCLD-ENTRIES                         ELTABM  
01113         VARYING GX3-INDEX  FROM 1 BY 1                            ELTABM  
01114           UNTIL    GX3-INDEX = WS-MAX-GX3-INDEX                   ELTABM  
01115                 OR (    SW-INTRNL-INST-PROV-CL                    ELTABM  
01116                     AND SW-INTRNL-PROF-PROV-CL ).                 ELTABM  
01117                                                                   ELTABM  
01118 ************************************************************      ELTABM  
01119 *                                                          *      ELTABM  
01120 *    CHECK INCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELTABM  
01121 *                                                          *      ELTABM  
01122 ************************************************************      ELTABM  
01123                                                                   ELTABM  
01124  0571-CHK-INCLD-TYPE-IPGS.                                        ELTABM  
01125      SET CFT9-IDX TO 1.                                           ELTABM  
01126      SET SW-INTRNL-NOT-PROF-PROV-CL                               ELTABM  
01127       TO TRUE.                                                    ELTABM  
01128      PERFORM 0581-TEST-IPGS-INCLD-ENTRIES                         ELTABM  
01129         VARYING GXS-INDEX  FROM 1 BY 1                            ELTABM  
01130           UNTIL    GXS-INDEX = WS-MAX-GXS-INDEX                   ELTABM  
01131                 OR (    SW-INTRNL-PROF-PROV-CL).                  ELTABM  
01132                                                                   ELTABM  
01133 ************************************************************      ELTABM  
01134 *                                                          *      ELTABM  
01135 *    TEST IPGT INCLUDE ENTRIES TO DETERMINE PROVIDER CLASS *      ELTABM  
01136 *                                                          *      ELTABM  
01137 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELTABM  
01138 *          CFT2 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELTABM  
01139 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELTABM  
01140 *          OF THE IPGT TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELTABM  
01141 *                                                          *      ELTABM  
01142 ************************************************************      ELTABM  
01143                                                                   ELTABM  
01144  0580-TEST-IPGT-INCLD-ENTRIES.                                    ELTABM  
01145      PERFORM WITH TEST BEFORE                                     ELTABM  
01146         UNTIL    SW-OCCRNC-APPLIES                                ELTABM  
01147               OR   CFT2-PT (CFT2-IDX)                             ELTABM  
01148                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELTABM  
01149               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELTABM  
01150         IF   GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELTABM  
01151            = CFT2-PT (CFT2-IDX)                                   ELTABM  
01152         THEN                                                      ELTABM  
01153 *    -- TEST PROVIDER CLASS                                       ELTABM  
01154            EVALUATE TRUE                                          ELTABM  
01155               WHEN CFT2-PT-INST (CFT2-IDX)                        ELTABM  
01156                  SET SW-INTRNL-INST-PROV-CL TO TRUE               ELTABM  
01157                  IF SRP-ACCUM-PROV-CLASS-INST                     ELTABM  
01158                  THEN                                             ELTABM  
01159                     SET SW-OCCRNC-APPLIES TO TRUE                 ELTABM  
01160                  END-IF                                           ELTABM  
01161               WHEN CFT2-PT-PROF (CFT2-IDX)                        ELTABM  
01162                  SET SW-INTRNL-PROF-PROV-CL TO TRUE               ELTABM  
01163                  IF SRP-ACCUM-PROV-CLASS-PROF                     ELTABM  
01164                  THEN                                             ELTABM  
01165                     SET SW-OCCRNC-APPLIES TO TRUE                 ELTABM  
01166                  END-IF                                           ELTABM  
01167               END-EVALUATE                                        ELTABM  
01168         ELSE                                                      ELTABM  
01169            CONTINUE                                               ELTABM  
01170         END-IF                                                    ELTABM  
01171 *    -- BUMP TO NEXT CFT2 TABLE ENTRY                             ELTABM  
01172         SET CFT2-IDX UP BY 1                                      ELTABM  
01173         END-PERFORM.                                              ELTABM  
01174                                                                   ELTABM  
01175 ************************************************************      ELTABM  
01176 *                                                          *      ELTABM  
01177 *    TEST IPGS INCLUDE ENTRIES TO DETERMINE PROVIDER SPEC  *      ELTABM  
01178 *                                                          *      ELTABM  
01179 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELTABM  
01180 *          CFT2 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELTABM  
01181 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELTABM  
01182 *          OF THE IPGS TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELTABM  
01183 *                                                          *      ELTABM  
01184 ************************************************************      ELTABM  
01185                                                                   ELTABM  
01186  0581-TEST-IPGS-INCLD-ENTRIES.                                    ELTABM  
01187      PERFORM WITH TEST BEFORE                                     ELTABM  
01188         UNTIL    SW-OCCRNC-APPLIES                                ELTABM  
01189               OR   CFT9-PT (CFT9-IDX)                             ELTABM  
01190                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELTABM  
01191               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELTABM  
01192         IF   GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELTABM  
01193            = CFT9-PT (CFT9-IDX)                                   ELTABM  
01194         THEN                                                      ELTABM  
01195 *    -- TEST PROVIDER SPEC                                        ELTABM  
01196          IF CFT9-PT-PROF (CFT9-IDX)                               ELTABM  
01197            SET SW-INTRNL-PROF-PROV-SP TO TRUE                     ELTABM  
01198            IF SRP-ACCUM-PROV-SPEC-PROF                            ELTABM  
01199               SET SW-OCCRNC-APPLIES TO TRUE                       ELTABM  
01200            END-IF                                                 ELTABM  
01201         ELSE                                                      ELTABM  
01202            CONTINUE                                               ELTABM  
01203         END-IF                                                    ELTABM  
01204         END-IF                                                    ELTABM  
01205 *    -- BUMP TO NEXT CFT9 TABLE ENTRY                             ELTABM  
01206         SET CFT9-IDX UP BY 1                                      ELTABM  
01207         END-PERFORM.                                              ELTABM  
01208                                                                   ELTABM  
01209 /***********************************************************      ELTABM  
01210 *                                                          *      ELTABM  
01211 *    CHECK EXCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELTABM  
01212 *                                                          *      ELTABM  
01213 ************************************************************      ELTABM  
01214                                                                   ELTABM  
01215  0600-CHK-EXCLD-TYPE-IPGT.                                        ELTABM  
01216                                                                   ELTABM  
01217 * -- INITIALIZE CFT2 TABLE TO INCLUDE ALL PROVIDER TYPES          ELTABM  
01218      PERFORM WITH TEST BEFORE                                     ELTABM  
01219         VARYING CFT2-IDX FROM 1 BY 1                              ELTABM  
01220           UNTIL CFT2-IDX > CFT2-NBR-TBL-ENTRIES                   ELTABM  
01221         SET  CFT2-PT-INCLUDE (CFT2-IDX) TO TRUE                   ELTABM  
01222         END-PERFORM.                                              ELTABM  
01223                                                                   ELTABM  
01224 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGT                 ELTABM  
01225      SET  CFT2-IDX TO 1.                                          ELTABM  
01226      PERFORM 0610-TAG-EXCLD-IPGT-ENTRIES                          ELTABM  
01227         VARYING GX3-INDEX FROM 1 BY 1                             ELTABM  
01228           UNTIL GX3-INDEX = WS-MAX-GX3-INDEX.                     ELTABM  
01229                                                                   ELTABM  
01230 * -- CHECK CFT2 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELTABM  
01231      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTABM  
01232          SW-INTRNL-NOT-PROF-PROV-CL                               ELTABM  
01233       TO TRUE.                                                    ELTABM  
01234      PERFORM 0630-CHK-CFT2-NOT-EXCLD                              ELTABM  
01235         VARYING CFT2-IDX FROM 1 BY 1                              ELTABM  
01236           UNTIL    (    SW-INTRNL-INST-PROV-CL                    ELTABM  
01237                     AND SW-INTRNL-PROF-PROV-CL )                  ELTABM  
01238                 OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES.               ELTABM  
01239                                                                   ELTABM  
01240 /***********************************************************      ELTABM  
01241 *                                                          *      ELTABM  
01242 *    CHECK EXCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELTABM  
01243 *                                                          *      ELTABM  
01244 ************************************************************      ELTABM  
01245                                                                   ELTABM  
01246  0601-CHK-EXCLD-TYPE-IPGS.                                        ELTABM  
01247                                                                   ELTABM  
01248 * -- INITIALIZE CFT9 TABLE TO INCLUDE ALL PROVIDER SPEC           ELTABM  
01249      PERFORM WITH TEST BEFORE                                     ELTABM  
01250         VARYING CFT9-IDX FROM 1 BY 1                              ELTABM  
01251           UNTIL CFT9-IDX > CFT9-NBR-TBL-ENTRIES                   ELTABM  
01252         SET  CFT9-PT-INCLUDE (CFT9-IDX) TO TRUE                   ELTABM  
01253         END-PERFORM.                                              ELTABM  
01254                                                                   ELTABM  
01255 * -- TAG ALL PROVIDER SPEC EXCLUDED BY THIS IPGS                  ELTABM  
01256      SET  CFT9-IDX TO 1.                                          ELTABM  
01257      PERFORM 0611-TAG-EXCLD-IPGS-ENTRIES                          ELTABM  
01258         VARYING GXS-INDEX FROM 1 BY 1                             ELTABM  
01259           UNTIL GXS-INDEX = WS-MAX-GXS-INDEX.                     ELTABM  
01260                                                                   ELTABM  
01261 * -- CHECK CFT9 TABLE FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED ELTABM  
01262      SET SW-INTRNL-NOT-PROF-PROV-CL                               ELTABM  
01263       TO TRUE.                                                    ELTABM  
01264      PERFORM 0631-CHK-CFT9-NOT-EXCLD                              ELTABM  
01265         VARYING CFT9-IDX FROM 1 BY 1                              ELTABM  
01266           UNTIL    (    SW-INTRNL-PROF-PROV-CL)                   ELTABM  
01267                 OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES.               ELTABM  
01268                                                                   ELTABM  
01269 ************************************************************      ELTABM  
01270 *                                                          *      ELTABM  
01271 *    TAG EXCLUDED IPGT ENTRIES IN CFT2                     *      ELTABM  
01272 *                                                          *      ELTABM  
01273 ************************************************************      ELTABM  
01274                                                                   ELTABM  
01275  0610-TAG-EXCLD-IPGT-ENTRIES.                                     ELTABM  
01276      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELTABM  
01277      PERFORM WITH TEST BEFORE                                     ELTABM  
01278         UNTIL    SW-ENTRY-FOUND                                   ELTABM  
01279               OR   CFT2-PT (CFT2-IDX)                             ELTABM  
01280                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELTABM  
01281               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELTABM  
01282         IF   CFT2-PT(CFT2-IDX)                                    ELTABM  
01283            = GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELTABM  
01284         THEN                                                      ELTABM  
01285            SET SW-ENTRY-FOUND TO TRUE                             ELTABM  
01286            SET CFT2-PT-EXCLUDE (CFT2-IDX) TO TRUE                 ELTABM  
01287            SET CFT2-IDX UP BY 1                                   ELTABM  
01288         ELSE                                                      ELTABM  
01289            SET CFT2-IDX UP BY 1                                   ELTABM  
01290         END-IF                                                    ELTABM  
01291         END-PERFORM.                                              ELTABM  
01292                                                                   ELTABM  
01293 ************************************************************      ELTABM  
01294 *                                                          *      ELTABM  
01295 *    TAG EXCLUDED IPGS ENTRIES IN CFT9                     *      ELTABM  
01296 *                                                          *      ELTABM  
01297 ************************************************************      ELTABM  
01298                                                                   ELTABM  
01299  0611-TAG-EXCLD-IPGS-ENTRIES.                                     ELTABM  
01300      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELTABM  
01301      PERFORM WITH TEST BEFORE                                     ELTABM  
01302         UNTIL    SW-ENTRY-FOUND                                   ELTABM  
01303               OR   CFT9-PT (CFT9-IDX)                             ELTABM  
01304                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELTABM  
01305               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELTABM  
01306         IF   CFT9-PT(CFT9-IDX)                                    ELTABM  
01307            = GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELTABM  
01308            SET SW-ENTRY-FOUND TO TRUE                             ELTABM  
01309            SET CFT9-PT-EXCLUDE (CFT9-IDX) TO TRUE                 ELTABM  
01310            SET CFT9-IDX UP BY 1                                   ELTABM  
01311         ELSE                                                      ELTABM  
01312            SET CFT9-IDX UP BY 1                                   ELTABM  
01313         END-IF                                                    ELTABM  
01314         END-PERFORM.                                              ELTABM  
01315                                                                   ELTABM  
01316 ******************************************************************ELTABM  
01317 *                                                                *ELTABM  
01318 *    CHECK CFT2 FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDED     *ELTABM  
01319 *                                                                *ELTABM  
01320 ******************************************************************ELTABM  
01321                                                                   ELTABM  
01322  0630-CHK-CFT2-NOT-EXCLD.                                         ELTABM  
01323      IF CFT2-PT-INCLUDE (CFT2-IDX)                                ELTABM  
01324      THEN                                                         ELTABM  
01325         EVALUATE TRUE                                             ELTABM  
01326            WHEN CFT2-PT-INST (CFT2-IDX)                           ELTABM  
01327               SET SW-INTRNL-INST-PROV-CL TO TRUE                  ELTABM  
01328               IF SRP-ACCUM-PROV-CLASS-INST                        ELTABM  
01329               THEN                                                ELTABM  
01330                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTABM  
01331               END-IF                                              ELTABM  
01332            WHEN CFT2-PT-PROF (CFT2-IDX)                           ELTABM  
01333               SET SW-INTRNL-PROF-PROV-CL TO TRUE                  ELTABM  
01334               IF SRP-ACCUM-PROV-CLASS-PROF                        ELTABM  
01335               THEN                                                ELTABM  
01336                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTABM  
01337               END-IF                                              ELTABM  
01338            END-EVALUATE                                           ELTABM  
01339      END-IF.                                                      ELTABM  
01340                                                                   ELTABM  
01341 ******************************************************************ELTABM  
01342 *                                                                *ELTABM  
01343 *    CHECK CFT9 FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED     * ELTABM  
01344 *                                                                *ELTABM  
01345 ******************************************************************ELTABM  
01346                                                                   ELTABM  
01347  0631-CHK-CFT9-NOT-EXCLD.                                         ELTABM  
01348      IF CFT9-PT-INCLUDE (CFT9-IDX)                                ELTABM  
01349        IF CFT9-PT-PROF (CFT9-IDX)                                 ELTABM  
01350         SET SW-INTRNL-PROF-PROV-SP TO TRUE                        ELTABM  
01351         IF SRP-ACCUM-PROV-SPEC-PROF                               ELTABM  
01352            SET SW-OCCRNC-APPLIES TO TRUE                          ELTABM  
01353         END-IF                                                    ELTABM  
01354      END-IF.                                                      ELTABM  
01355                                                                   ELTABM  
01356 /*****************************************************************ELTABM  
01357 *                                                                *ELTABM  
01358 *    CHECK IBGR INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELTABM  
01359 *                                                                *ELTABM  
01360 ******************************************************************ELTABM  
01361                                                                   ELTABM  
01362  0640-CHK-IBGR-PROV-CL.                                           ELTABM  
01363      SET SW-INTRNL-NOT-INST-PROV-CL                               ELTABM  
01364          SW-INTRNL-NOT-PROF-PROV-CL TO TRUE.                      ELTABM  
01365      MOVE PC-IBGR TO KWA-PROVISION-ID.                            ELTABM  
01366      MOVE WS-IBGR-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELTABM  
01367      PERFORM 0660-READ-INTRLN-TAB.                                ELTABM  
01368      SET ADDRESS OF GX1-RECORD-AREA TO IOP-REC-PTR.               ELTABM  
01369      SET IOP-REC-PTR                TO NULLS.                     ELTABM  
01370      SET GX1-INDEX                  TO GX1-ENTRY-COUNT.           ELTABM  
01371      SET WS-MAX-GX1-INDEX           TO GX1-INDEX.                 ELTABM  
01372                                                                   ELTABM  
01373      IF GX1-ID-ARGUMENT-EXCLUDED                                  ELTABM  
01374      THEN                                                         ELTABM  
01375 *    -- ASSUME THAT IBGR WOULD NOT EXCLUDE ALL OF ANY PROVIDER    ELTABM  
01376 *       CLASS (I.E., BOTH TYPES APPLY).                           ELTABM  
01377         SET SW-OCCRNC-APPLIES                                     ELTABM  
01378             SW-INTRNL-INST-PROV-CL                                ELTABM  
01379             SW-INTRNL-PROF-PROV-CL                                ELTABM  
01380          TO TRUE                                                  ELTABM  
01381      ELSE                                                         ELTABM  
01382         PERFORM 0650-CHK-INCLD-TYPE-IBGR                          ELTABM  
01383      END-IF.                                                      ELTABM  
01384                                                                   ELTABM  
01385 /*****************************************************************ELTABM  
01386 *                                                                *ELTABM  
01387 *    CHECK INCLUDE TYPE IBGR TO DETERMINE PROVIDER CLASS         *ELTABM  
01388 *                                                                *ELTABM  
01389 ******************************************************************ELTABM  
01390                                                                   ELTABM  
01391  0650-CHK-INCLD-TYPE-IBGR.                                        ELTABM  
01392      PERFORM WITH TEST BEFORE                                     ELTABM  
01393         VARYING GX1-INDEX FROM 1 BY 1                             ELTABM  
01394           UNTIL    GX1-INDEX = WS-MAX-GX1-INDEX                   ELTABM  
01395                 OR (    SW-INTRNL-INST-PROV-CL                    ELTABM  
01396                     AND SW-INTRNL-PROF-PROV-CL )                  ELTABM  
01397         MOVE GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)                ELTABM  
01398           TO WS-PROVISION-ARGUMENT                                ELTABM  
01399         EVALUATE TRUE                                             ELTABM  
01400            WHEN INST-CL                                           ELTABM  
01401               SET SW-INTRNL-INST-PROV-CL TO TRUE                  ELTABM  
01402               IF SRP-ACCUM-PROV-CLASS-INST                        ELTABM  
01403               THEN                                                ELTABM  
01404                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTABM  
01405               END-IF                                              ELTABM  
01406            WHEN PROF-CL                                           ELTABM  
01407               SET SW-INTRNL-PROF-PROV-CL TO TRUE                  ELTABM  
01408               IF SRP-ACCUM-PROV-CLASS-PROF                        ELTABM  
01409               THEN                                                ELTABM  
01410                  SET SW-OCCRNC-APPLIES TO TRUE                    ELTABM  
01411               END-IF                                              ELTABM  
01412            END-EVALUATE                                           ELTABM  
01413         END-PERFORM.                                              ELTABM  
01414                                                                   ELTABM  
01415 /***********************************************************      ELTABM  
01416 *                                                          *      ELTABM  
01417 *    READ THE INTERNAL TABULAR RECORD                      *      ELTABM  
01418 *                                                          *      ELTABM  
01419 ************************************************************      ELTABM  
01420                                                                   ELTABM  
01421  0660-READ-INTRLN-TAB.                                            ELTABM  
01422      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELTABM  
01423      SET IOP-RD TO TRUE.                                          ELTABM  
01424      SET IOP-FCQ-NONE TO TRUE.                                    ELTABM  
01425      SET IOP-KVQ-EQ TO TRUE.                                      ELTABM  
01426      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELTABM  
01427      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTABM  
01428      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTABM  
01429      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTABM  
01430                                                                   ELTABM  
01431      EVALUATE TRUE                                                ELTABM  
01432         WHEN IOP-RC-OK                                            ELTABM  
01433            CONTINUE                                               ELTABM  
01434         WHEN IOP-RC-NOTFND                                        ELTABM  
01435            SET CIA-AB-NOTFND-GCTABULR TO TRUE                     ELTABM  
01436            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELTABM  
01437         WHEN OTHER                                                ELTABM  
01438             SET CIA-AB-CRITIO TO TRUE                             ELTABM  
01439             EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC           ELTABM  
01440         END-EVALUATE.                                             ELTABM  
01441                                                                   ELTABM  
01442 /***********************************************************      ELTABM  
01443 *                                                          *      ELTABM  
01444 *        ADD ACCUM OCCURRENCE TO FILE                      *      ELTABM  
01445 *                                                          *      ELTABM  
01446 ************************************************************      ELTABM  
01447                                                                   ELTABM  
01448  0710-WRITE-EXTRACT-RECORD.                                       ELTABM  
01449      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELTABM  
01450      SET  IOP-ADD TO TRUE.                                        ELTABM  
01451      SET  IOP-FCQ-NONE TO TRUE.                                   ELTABM  
01452      SET  IOP-KVQ-NONE TO TRUE.                                   ELTABM  
01453      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELTABM  
01454                                                                   ELTABM  
01455 /***********************************************************      ELTABM  
01456 *                                                          *      ELTABM  
01457 *    ESTABLISH ADDRESSABILITY OF THE TABULAR FILE          *      ELTABM  
01458 *                                                          *      ELTABM  
01459 ************************************************************      ELTABM  
01460                                                                   ELTABM  
01461  9060-EST-ADR-TABULAR-FILE.                                       ELTABM  
01462      SET  CIA-GCTABULR-DDN TO TRUE.                               ELTABM  
01463      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTABM  
01464         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELTABM  
01465         END-CALL.                                                 ELTABM  
01466      IF CIA-RC-PTR-NULL                                           ELTABM  
01467      THEN                                                         ELTABM  
01468         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTABM  
01469         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTABM  
01470      END-IF.                                                      ELTABM  
01471                                                                   ELTABM  
01472 /***********************************************************      ELTABM  
01473 *                                                          *      ELTABM  
01474 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELTABM  
01475 *                                                          *      ELTABM  
01476 ************************************************************      ELTABM  
01477                                                                   ELTABM  
01478  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELTABM  
01479      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELTABM  
01480      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTABM  
01481         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELTABM  
01482         END-CALL.                                                 ELTABM  
01483      IF CIA-RC-PTR-NULL                                           ELTABM  
01484         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTABM  
01485         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELTABM  
01486      END-IF.                                                      ELTABM  
