00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGADLCC
00003  PROGRAM-ID.        ELGADLCC.                                        LV002
00004                                                                   ELGADLCC
00005  AUTHOR.            LUCY TORRES.                                  ELGADLCC
00006                     RICHARD J. LUKETICH (RE-WRITE).               ELGADLCC
00007                                                                   ELGADLCC
00008  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELGADLCC
00009                     A MUTUAL LEGAL RESERVE COMPANY                ELGADLCC
00010                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELGADLCC
00011                     233 N. MICHIGAN AVE                           ELGADLCC
00012                     CHICAGO, ILLINOIS 60601                       ELGADLCC
00013                                                                   ELGADLCC
00014  DATE-WRITTEN.      03-JUN-1987.                                  ELGADLCC
00015                     07-JAN-1992 (RE-WRITE).                       ELGADLCC
00016                                                                   ELGADLCC
00017  DATE-COMPILED.                                                   ELGADLCC
00018                                                                   ELGADLCC
00019  SECURITY.          COPYRIGHT 1986, 1992,                         ELGADLCC
00020                     HEALTH CARE SERVICE CORPORATION               ELGADLCC
00021                                                                   ELGADLCC
00022  ENVIRONMENT DIVISION.                                            ELGADLCC
00023                                                                   ELGADLCC
00024  CONFIGURATION SECTION.                                           ELGADLCC
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELGADLCC
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELGADLCC
00027                                                                   ELGADLCC
00028 /*****************************************************************ELGADLCC
00029 *                                                                *ELGADLCC
00030 *  ELGADLCC-ELS:    SELECTS #ADL (DEDUCTIBLE) ACCUMULATORS AND   *ELGADLCC
00031 *                   SETUPS THE INFORMATION TO BE PROCESSED BY    *ELGADLCC
00032 *                   THE DEDUCTIBLE GENERATOR MODULE.  THE ACCUMS *ELGADLCC
00033 *                   ARE SELECTED FROM THE GROUP SPECIFIC AND     *ELGADLCC
00034 *                   CONTRACT LEVEL PROCESSING.                   *ELGADLCC
00035 *                                                                *ELGADLCC
00036 ******************************************************************ELGADLCC
00037 *                                                                *ELGADLCC
00038 *                      MAINTENANCE HISTORY                       *ELGADLCC
00039 *                                                                *ELGADLCC
00040 *  MOD     DATE     BY                DESCRIPTION                *ELGADLCC
00041 * ----- ----------- --- ---------------------------------------- *ELGADLCC
00042 * 01.00 03-JUN-1987 LET CREATED                                  *ELGADLCC
00043 * 02.00 ??-DEC-1992 JPB REBUILT BY CLONING FROM ELGABMCC         *ELGADLCC
00044 * 02.01 01-SEP-1994 AKK ADDED RPO - MISSED IT AT THE TIME        *ELGADLCC
00045 *                       RPO WAS WRITTEN.                         *ELGADLCC
00046 *                                                                *ELGADLCC
00047 * 02.02 24-FEB-1995 AKK ADDED CPO.                                ELGADLCC
00048 *                                                                *ELGADLCC
00049 * 02.03 16-FEB-1996 AKK ADDED CBL AND PAN.                        ELGADLCC
00050 *                                                                *ELGADLCC
00051 * 02.04 11-MAR-1996 AKK ADDED BAE.                                ELGADLCC
00052 *                                                                *ELGADLCC
00053 * 02.05 24-AUG-2000 AKK ADD SUPPORT FOR #IPGS                     ELGADLCC
00054 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGADLCC
      *                                                                *        
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00055 ******************************************************************ELGADLCC
00056      TITLE  'ELGADLCC        WORKING STORAGE'.                    ELGADLCC
00057  DATA DIVISION.                                                   ELGADLCC
00058                                                                   ELGADLCC
00059  WORKING-STORAGE SECTION.                                         ELGADLCC
00060                                                                   ELGADLCC
00061  01  SWITCHES.                                                    ELGADLCC
00062      02                                      PICTURE  X(01).      ELGADLCC
00063         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELGADLCC
00064         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELGADLCC
00065      02                                      PICTURE  X(01).      ELGADLCC
00066         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELGADLCC
00067         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELGADLCC
00068      02                                      PICTURE  X(01).      ELGADLCC
00069         88 SW-CC-IND-APPLIES                 VALUE 'Y'.           ELGADLCC
00070         88 SW-CC-IND-DOES-NOT-APPLY          VALUE 'N'.           ELGADLCC
00071      02                                      PICTURE  X(01).      ELGADLCC
00072         88 SW-HAS-IBGR                       VALUE 'Y'.           ELGADLCC
00073         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELGADLCC
00074      02                                      PICTURE  X(01).      ELGADLCC
00075         88 SW-HAS-IDGD                       VALUE 'Y'.           ELGADLCC
00076         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELGADLCC
00077      02                                      PICTURE  X(01).      ELGADLCC
00078         88 SW-HAS-IPGN                       VALUE 'Y'.           ELGADLCC
00079         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELGADLCC
00080      02                                      PICTURE  X(01).      ELGADLCC
00081         88 SW-HAS-IPGP                       VALUE 'Y'.           ELGADLCC
00082         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELGADLCC
00083      02                                      PICTURE  X(01).      ELGADLCC
00084         88 SW-HAS-IPGT                       VALUE 'Y'.           ELGADLCC
00085         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELGADLCC
00086      02                                      PICTURE  X(01).      ELGADLCC
00087         88 SW-HAS-IPGS                       VALUE 'Y'.           ELGADLCC
00088         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELGADLCC
00089      02                                      PICTURE  X(01).      ELGADLCC
00090         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELGADLCC
00091         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELGADLCC
00092      02                                      PICTURE  X(01).      ELGADLCC
00093         88 SW-INTRNL-INST-PROV-CLASS         VALUE 'Y'.           ELGADLCC
00094         88 SW-INTRNL-NOT-INST-PROV-CLASS     VALUE 'N'.           ELGADLCC
00095      02                                      PICTURE  X(01).      ELGADLCC
00096         88 SW-INTRNL-PROF-PROV-CLASS         VALUE 'Y'.           ELGADLCC
00097         88 SW-INTRNL-NOT-PROF-PROV-CLASS     VALUE 'N'.           ELGADLCC
00098      02                                      PICTURE  X(01).      ELGADLCC
00099         88 SW-INTRNL-PROF-PROV-SPEC          VALUE 'Y'.           ELGADLCC
00100         88 SW-INTRNL-NOT-PROF-PROV-SPEC      VALUE 'N'.           ELGADLCC
00101      02                                      PICTURE  X(01).      ELGADLCC
00102         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELGADLCC
00103         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELGADLCC
00104 / -- GCPS DATA ELEMENT TEST AREAS                                 ELGADLCC
00105                                                                   ELGADLCC
00106  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELGADLCC
00107      88 WS-LOB-INST              VALUE '1', '4', '5', '6', '8'.   ELGADLCC
00108      88 WS-LOB-PROF              VALUE '2', '4', '5', '7', '8'.   ELGADLCC
00109      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELGADLCC
00110      88 WS-LOB-BOTH              VALUE '4', '5', '6', '7', '8'.   ELGADLCC
00111                                                                   ELGADLCC
00112  01  WS-PROVISION-ARGUMENT.                                       ELGADLCC
00113      02                          PICTURE  X(05).                  ELGADLCC
00114      02 WS-PROVISION-CLASS       PICTURE  X(01).                  ELGADLCC
00115         88 INST-CLASS            VALUE 'A', 'B', 'W'.             ELGADLCC
00116         88 PROF-CLASS            VALUE 'C', 'D', 'E'.             ELGADLCC
00117                                                                   ELGADLCC
00118  01  WS-SUBTOPIC                 PICTURE  X(16).                  ELGADLCC
00119      88 WS-SUBTOPIC-ATCP         VALUE 'ATCP            '.        ELGADLCC
00120      88 WS-SUBTOPIC-BAE          VALUE 'BAE             '.        ELGADLCC
00121      88 WS-SUBTOPIC-CPO          VALUE 'CPO             '.        ELGADLCC
00122      88 WS-SUBTOPIC-CBL          VALUE 'CBL             '.        ELGADLCC
00123      88 WS-SUBTOPIC-EMH          VALUE 'EMH             '.        ELGADLCC
00124      88 WS-SUBTOPIC-HOSP         VALUE 'HOSP            '.        ELGADLCC
00125      88 WS-SUBTOPIC-IOB          VALUE 'IOB             '.        ELGADLCC
00126      88 WS-SUBTOPIC-MASOP        VALUE 'MASOP           '.        ELGADLCC
00127      88 WS-SUBTOPIC-MCN          VALUE 'MCN             '.        ELGADLCC
00128      88 WS-SUBTOPIC-MEDNEC       VALUE 'MEDNEC          '.        ELGADLCC
00129      88 WS-SUBTOPIC-MHSC         VALUE 'MHSC            '.        ELGADLCC
00130      88 WS-SUBTOPIC-MOND         VALUE 'MOND            '.        ELGADLCC
00131      88 WS-SUBTOPIC-MOPS         VALUE 'MOPS            '.        ELGADLCC
00132      88 WS-SUBTOPIC-MSA          VALUE 'MSA             '.        ELGADLCC
00133      88 WS-SUBTOPIC-PAN          VALUE 'PAN             '.        ELGADLCC
00134      88 WS-SUBTOPIC-PAR          VALUE 'PAR             '.        ELGADLCC
00135      88 WS-SUBTOPIC-PAT          VALUE 'PAT             '.        ELGADLCC
00136      88 WS-SUBTOPIC-POS          VALUE 'POS             '.        ELGADLCC
00137      88 WS-SUBTOPIC-PPO          VALUE 'PPO             '.        ELGADLCC
00138      88 WS-SUBTOPIC-REIMB        VALUE 'REIMB           '.        ELGADLCC
00139      88 WS-SUBTOPIC-RPO          VALUE 'RPO             '.        ELGADLCC
00140      88 WS-SUBTOPIC-WEEK         VALUE 'WEEK            '.        ELGADLCC
00141                                                                   ELGADLCC
00142  01  WS-COST-CONTAIN-IND.                                         ELGADLCC
00143      02                          PICTURE  X(01).                  ELGADLCC
00144         88 WS-CCI-ATCP           VALUE 'A', 'H'.                  ELGADLCC
00145         88 WS-CCI-BAE            VALUE 'N'.                       ELGADLCC
00146         88 WS-CCI-CPO            VALUE 'K'.                       ELGADLCC
00147         88 WS-CCI-CBL            VALUE 'J'.                       ELGADLCC
00148         88 WS-CCI-EMH            VALUE 'I'.                       ELGADLCC
00149         88 WS-CCI-HOSP           VALUE 'B', 'F'.                  ELGADLCC
00150         88 WS-CCI-IOB            VALUE '5'.                       ELGADLCC
00151         88 WS-CCI-MASOP          VALUE '2', 'E', 'G'.             ELGADLCC
00152         88 WS-CCI-MCN            VALUE 'M'.                       ELGADLCC
00153         88 WS-CCI-MEDNEC         VALUE '7'.                       ELGADLCC
00154         88 WS-CCI-MHSC           VALUE 'S'.                       ELGADLCC
00155         88 WS-CCI-MOND           VALUE '4'.                       ELGADLCC
00156         88 WS-CCI-MOPS           VALUE '1', 'F'.                  ELGADLCC
00157         88 WS-CCI-MSA            VALUE '8'.                       ELGADLCC
00158         88 WS-CCI-PAR            VALUE '6', 'E', 'G'.             ELGADLCC
00159         88 WS-CCI-PAT            VALUE 'D'.                       ELGADLCC
00160         88 WS-CCI-PAN            VALUE 'L'.                       ELGADLCC
00161         88 WS-CCI-POS            VALUE 'P'.                       ELGADLCC
00162         88 WS-CCI-PPO            VALUE '9'.                       ELGADLCC
00163         88 WS-CCI-REIMB          VALUE '*'.                       ELGADLCC
00164         88 WS-CCI-RPO            VALUE 'R'.                       ELGADLCC
00165         88 WS-CCI-WEEK           VALUE '3'.                       ELGADLCC
00166      02                          PICTURE  X(01).                  ELGADLCC
00167 / -- CONSTANTS AND WORK FIELDS                                    ELGADLCC
00168                                                                   ELGADLCC
00169  01  PROGRAM-CONSTANTS.                                           ELGADLCC
00170      02 PC-ADL                   PICTURE  X(06) VALUE '#ADL  '.   ELGADLCC
00171      02 PC-GCT-MAX-SUB           PICTURE S9(04) COMP.             ELGADLCC
00172      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELGADLCC
00173      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELGADLCC
00174      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELGADLCC
00175      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELGADLCC
00176      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELGADLCC
00177      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGS '.   ELGADLCC
00178      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELGADLCC
00179                                                                   ELGADLCC
00180  01  WS-WORK-FIELDS.                                              ELGADLCC
00181      02 WS-ADL-SUB               PICTURE S9(04) COMP.             ELGADLCC
00182      02 WS-ADL-ACCUM-CNT         PICTURE S9(04) COMP.             ELGADLCC
00183      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELGADLCC
00184                                                                   ELGADLCC
00185  01  WS-MAX-INDEX-VALUES.                                         ELGADLCC
00186      02 WS-MAX-GAC-INDEX         INDEX.                           ELGADLCC
00187      02 WS-MAX-GAC-INT-INDEX     INDEX.                           ELGADLCC
00188      02 WS-MAX-GCT-INDEX         INDEX.                           ELGADLCC
00189      02 WS-MAX-GCG-INDEX         INDEX.                           ELGADLCC
00190      02 WS-MAX-GX1-INDEX         INDEX.                           ELGADLCC
00191      02 WS-MAX-GX3-INDEX         INDEX.                           ELGADLCC
00192      02 WS-MAX-GXS-INDEX         INDEX.                           ELGADLCC
00193                                                                   ELGADLCC
00194  01  WS-POINTERS.                                                 ELGADLCC
00195      02  WS-POINTER1             POINTER.                         ELGADLCC
00196      02  WS-POINTER2             POINTER.                         ELGADLCC
00197                                                                   ELGADLCC
00198  01  ACCUM-HOLD-TBL.                                              ELGADLCC
00199      02  ACCUM-SLOT-NBR          PICTURE S9(07) COMP-3            ELGADLCC
00200                                  OCCURS 5 TIMES.                  ELGADLCC
00201                                                                   ELGADLCC
00202  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELGADLCC
00203      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGADLCC
00204      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGADLCC
00205      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGADLCC
00206      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGADLCC
00207      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGADLCC
00208      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGADLCC
00209 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELGADLCC
00210      COPY ELSCFTB2.                                               ELGADLCC
00211                                                                   ELGADLCC
00212 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELGADLCC
00213      COPY ELSCFTB9.                                               ELGADLCC
00214                                                                   ELGADLCC
00215      TITLE  'ELGADLCC        LINKAGE SECTION'                     ELGADLCC
00216  LINKAGE SECTION.                                                 ELGADLCC
00217  01  DFHCOMMAREA.                                                 ELGADLCC
00218      COPY ELSCOMMC.                                               ELGADLCC
00219 /                                                                 ELGADLCC
00220      COPY ELSCIA2C.                                               ELGADLCC
00221 /                                                                 ELGADLCC
00222      COPY ELSIOPMC.                                               ELGADLCC
00223 /                                                                 ELGADLCC
00224      COPY ELSKEYSC.                                               ELGADLCC
00225 /                                                                 ELGADLCC
00226      COPY ELSSRTPC.                                               ELGADLCC
00227 /                                                                 ELGADLCC
00228      COPY ELSSSCBC.                                               ELGADLCC
00229 /                                                                 ELGADLCC
00230  01  GCG-GRP-SPEC-RECORD-AREA.                                    ELGADLCC
00231      COPY GCGROUPC.                                               ELGADLCC
00232 /                                                                 ELGADLCC
00233  01  GCT-CONTRACT-RECORD-AREA.                                    ELGADLCC
00234      COPY GCCONTRC.                                               ELGADLCC
00235 /                                                                 ELGADLCC
00236  01  GAC-RECORD-AREA.                                             ELGADLCC
00237      COPY GCTADLC.                                                ELGADLCC
00238 /                                                                 ELGADLCC
00239      COPY ELSACUMC.                                               ELGADLCC
00240 /                                                                 ELGADLCC
00241  01  GX1-RECORD-AREA.                                             ELGADLCC
00242      COPY GCTIBGRC.                                               ELGADLCC
00243 /                                                                 ELGADLCC
00244  01  GX3-RECORD-AREA.                                             ELGADLCC
00245      COPY GCTIPGTC.                                               ELGADLCC
00246 /                                                                 ELGADLCC
00247  01  GXS-RECORD-AREA.                                             ELGADLCC
00248      COPY GCTIPGSC.                                               ELGADLCC
00249      TITLE  'ELGADLCC        PROCEDURE DIVISION'.                 ELGADLCC
00250 ************************************************************      ELGADLCC
00251 *                                                          *      ELGADLCC
00252 *    ELTADL MAINLINE                                       *      ELGADLCC
00253 *                                                          *      ELGADLCC
00254 ************************************************************      ELGADLCC
00255                                                                   ELGADLCC
00256  PROCEDURE DIVISION.                                              ELGADLCC
00257      PERFORM 0010-INITIALIZATION.                                 ELGADLCC
00258      PERFORM 0100-PROCESS.                                        ELGADLCC
00259      GOBACK.                                                      ELGADLCC
00260                                                                   ELGADLCC
00261 ************************************************************      ELGADLCC
00262 *                                                          *      ELGADLCC
00263 *    INITIALIZATION                                        *      ELGADLCC
00264 *                                                          *      ELGADLCC
00265 ************************************************************      ELGADLCC
00266                                                                   ELGADLCC
00267  0010-INITIALIZATION.                                             ELGADLCC
00268      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELGADLCC
00269      PERFORM 0060-EST-ADR-KEY-WK-AREA.                            ELGADLCC
00270      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELGADLCC
00271      PERFORM 0120-EST-ADR-GRP-SPC.                                ELGADLCC
00272      PERFORM 0190-INIT-DATA.                                      ELGADLCC
00273                                                                   ELGADLCC
00274 ************************************************************      ELGADLCC
00275 *                                                          *      ELGADLCC
00276 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGADLCC
00277 *                                                          *      ELGADLCC
00278 ************************************************************      ELGADLCC
00279                                                                   ELGADLCC
00280  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELGADLCC
00281      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGADLCC
00282      THEN                                                         ELGADLCC
00283         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGADLCC
00284      ELSE                                                         ELGADLCC
00285         IF ECA-CIA-PTR = NULL                                     ELGADLCC
00286         THEN                                                      ELGADLCC
00287            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGADLCC
00288         ELSE                                                      ELGADLCC
00289            CALL 'ELUINISM' USING DFHCOMMAREA                      ELGADLCC
00290               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELGADLCC
00291               END-CALL                                            ELGADLCC
00292            SET CIA-ELSSSCB-DDN TO TRUE                            ELGADLCC
00293            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELGADLCC
00294               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELGADLCC
00295               END-CALL                                            ELGADLCC
00296            IF CIA-RC-PTR-NULL                                     ELGADLCC
00297            THEN                                                   ELGADLCC
00298               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELGADLCC
00299               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGADLCC
00300            ELSE                                                   ELGADLCC
00301               CONTINUE                                            ELGADLCC
00302            END-IF                                                 ELGADLCC
00303         END-IF                                                    ELGADLCC
00304      END-IF.                                                      ELGADLCC
00305                                                                   ELGADLCC
00306 /***********************************************************      ELGADLCC
00307 *                                                          *      ELGADLCC
00308 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELGADLCC
00309 *                                                          *      ELGADLCC
00310 ************************************************************      ELGADLCC
00311                                                                   ELGADLCC
00312  0060-EST-ADR-KEY-WK-AREA.                                        ELGADLCC
00313      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGADLCC
00314      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGADLCC
00315         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELGADLCC
00316         END-CALL.                                                 ELGADLCC
00317      IF CIA-RC-PTR-NULL                                           ELGADLCC
00318      THEN                                                         ELGADLCC
00319         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGADLCC
00320         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGADLCC
00321      END-IF.                                                      ELGADLCC
00322                                                                   ELGADLCC
00323 ************************************************************      ELGADLCC
00324 *                                                          *      ELGADLCC
00325 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGADLCC
00326 *                                                          *      ELGADLCC
00327 ************************************************************      ELGADLCC
00328                                                                   ELGADLCC
00329  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGADLCC
00330      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGADLCC
00331      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGADLCC
00332         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELGADLCC
00333         END-CALL.                                                 ELGADLCC
00334      IF CIA-RC-PTR-NULL                                           ELGADLCC
00335      THEN                                                         ELGADLCC
00336         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGADLCC
00337         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGADLCC
00338      END-IF.                                                      ELGADLCC
00339                                                                   ELGADLCC
00340 ************************************************************      ELGADLCC
00341 *                                                          *      ELGADLCC
00342 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD     *      ELGADLCC
00343 *                                                          *      ELGADLCC
00344 ************************************************************      ELGADLCC
00345                                                                   ELGADLCC
00346  0120-EST-ADR-GRP-SPC.                                            ELGADLCC
00347      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELGADLCC
00348      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGADLCC
00349         ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                       ELGADLCC
00350         END-CALL.                                                 ELGADLCC
00351      IF CIA-RC-PTR-NULL                                           ELGADLCC
00352      THEN                                                         ELGADLCC
00353         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGADLCC
00354         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGADLCC
00355      END-IF.                                                      ELGADLCC
00356                                                                   ELGADLCC
00357 /***********************************************************      ELGADLCC
00358 *                                                          *      ELGADLCC
00359 *    INITIALIZE DATA AREAS                                 *      ELGADLCC
00360 *                                                          *      ELGADLCC
00361 ************************************************************      ELGADLCC
00362                                                                   ELGADLCC
00363  0190-INIT-DATA.                                                  ELGADLCC
00364      COMPUTE PC-GCT-MAX-SUB =   LENGTH OF GCT-CONT-TAB-PTRS       ELGADLCC
00365                               / LENGTH OF GCT-CON-TAB-ID-SLOT.    ELGADLCC
00366      SET GCT-INDEX TO PC-GCT-MAX-SUB.                             ELGADLCC
00367      SET WS-MAX-GCT-INDEX TO GCT-INDEX.                           ELGADLCC
00368      SET GCG-INDEX TO GCG-COUNT-TAB-PROVN-POINTERS.               ELGADLCC
00369      SET WS-MAX-GCG-INDEX TO GCG-INDEX.                           ELGADLCC
00370      INITIALIZE WS-ADL-ACCUM-CNT.                                 ELGADLCC
00371      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGADLCC
00372                                                                   ELGADLCC
00373 /***********************************************************      ELGADLCC
00374 *                                                          *      ELGADLCC
00375 *        PROCESS                                           *      ELGADLCC
00376 *                                                          *      ELGADLCC
00377 ************************************************************      ELGADLCC
00378                                                                   ELGADLCC
00379  0100-PROCESS.                                                    ELGADLCC
00380      PERFORM 0110-SCAN-GRP-SPC-FOR-ACCUMS.                        ELGADLCC
00381      PERFORM 0120-SCAN-CONTRACTS-FOR-ACCUMS.                      ELGADLCC
00382                                                                   ELGADLCC
00383      IF WS-ADL-ACCUM-CNT >  0                                     ELGADLCC
00384      THEN                                                         ELGADLCC
00385          PERFORM 0240-SCAN-FOR-APPLIC-OCCRNCS                     ELGADLCC
00386      END-IF.                                                      ELGADLCC
00387                                                                   ELGADLCC
00388 *    -- LINK TO THE OUTPUT GENERATOR                              ELGADLCC
00389      IF SW-APPLIC-ACCUM-FOUND                                     ELGADLCC
00390         SET SRP-COST-CONT-ACCUM TO TRUE                           ELGADLCC
00391         EXEC CICS LINK PROGRAM ('ELGADL')                         ELGADLCC
00392                        COMMAREA (DFHCOMMAREA)                     ELGADLCC
00393         END-EXEC                                                  ELGADLCC
00394      END-IF.                                                      ELGADLCC
00395                                                                   ELGADLCC
00396                                                                   ELGADLCC
00397 /***********************************************************      ELGADLCC
00398 *                                                          *      ELGADLCC
00399 *    SCAN GROUP SPECIFIC RECORD FOR ACCUMULATORS           *      ELGADLCC
00400 *                                                          *      ELGADLCC
00401 ************************************************************      ELGADLCC
00402                                                                   ELGADLCC
00403  0110-SCAN-GRP-SPC-FOR-ACCUMS.                                    ELGADLCC
00404      PERFORM WITH TEST BEFORE                                     ELGADLCC
00405         VARYING GCG-INDEX FROM 1 BY 1                             ELGADLCC
00406           UNTIL GCG-INDEX = WS-MAX-GCG-INDEX                      ELGADLCC
00407                 OR GCG-TAB-ID (GCG-INDEX) > PC-ADL                ELGADLCC
00408 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELGADLCC
00409         IF     GCG-TAB-ID (GCG-INDEX)  =  PC-ADL                  ELGADLCC
00410            AND GCG-TAB-SLOT-NO (GCG-INDEX)  >  ZERO               ELGADLCC
00411         THEN                                                      ELGADLCC
00412 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELGADLCC
00413            MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO WS-SLOT-NBR        ELGADLCC
00414            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELGADLCC
00415         END-IF                                                    ELGADLCC
00416         END-PERFORM.                                              ELGADLCC
00417                                                                   ELGADLCC
00418 /***********************************************************      ELGADLCC
00419 *                                                          *      ELGADLCC
00420 *    SCAN CONTRACT RECORDS FOR ACCUMULATORS                *      ELGADLCC
00421 *                                                          *      ELGADLCC
00422 ************************************************************      ELGADLCC
00423                                                                   ELGADLCC
00424  0120-SCAN-CONTRACTS-FOR-ACCUMS.                                  ELGADLCC
00425      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELGADLCC
00426      THEN                                                         ELGADLCC
00427          PERFORM 0130-SCAN-INST-BAS                               ELGADLCC
00428      END-IF.                                                      ELGADLCC
00429                                                                   ELGADLCC
00430      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELGADLCC
00431      THEN                                                         ELGADLCC
00432          PERFORM 0140-SCAN-PROF-BAS                               ELGADLCC
00433      END-IF.                                                      ELGADLCC
00434                                                                   ELGADLCC
00435      SET WS-POINTER1 TO NULLS.                                    ELGADLCC
00436      SET WS-POINTER2 TO NULLS.                                    ELGADLCC
00437                                                                   ELGADLCC
00438      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELGADLCC
00439      THEN                                                         ELGADLCC
00440          PERFORM 0150-SCAN-INST-SUP                               ELGADLCC
00441      END-IF.                                                      ELGADLCC
00442                                                                   ELGADLCC
00443      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELGADLCC
00444      THEN                                                         ELGADLCC
00445          PERFORM 0170-SCAN-PROF-SUP                               ELGADLCC
00446      END-IF.                                                      ELGADLCC
00447                                                                   ELGADLCC
00448 /***********************************************************      ELGADLCC
00449 *                                                          *      ELGADLCC
00450 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELGADLCC
00451 *                                                          *      ELGADLCC
00452 ************************************************************      ELGADLCC
00453                                                                   ELGADLCC
00454  0130-SCAN-INST-BAS.                                              ELGADLCC
00455      SET CIA-ELSCONIB-DDN TO TRUE.                                ELGADLCC
00456      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGADLCC
00457         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGADLCC
00458         END-CALL.                                                 ELGADLCC
00459      SET WS-POINTER1 TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.      ELGADLCC
00460                                                                   ELGADLCC
00461      IF CIA-RC-PTR-NULL                                           ELGADLCC
00462      THEN                                                         ELGADLCC
00463         CONTINUE                                                  ELGADLCC
00464      ELSE                                                         ELGADLCC
00465         PERFORM 0180-SCAN-CONTRACT-FOR-ACCUMS                     ELGADLCC
00466      END-IF.                                                      ELGADLCC
00467                                                                   ELGADLCC
00468 ************************************************************      ELGADLCC
00469 *                                                          *      ELGADLCC
00470 *    SCAN PROFESSIONAL BASIC CONTRACT RECORD               *      ELGADLCC
00471 *                                                          *      ELGADLCC
00472 ************************************************************      ELGADLCC
00473                                                                   ELGADLCC
00474  0140-SCAN-PROF-BAS.                                              ELGADLCC
00475      SET CIA-ELSCONPB-DDN TO TRUE.                                ELGADLCC
00476      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGADLCC
00477         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGADLCC
00478         END-CALL.                                                 ELGADLCC
00479      SET WS-POINTER2 TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.      ELGADLCC
00480                                                                   ELGADLCC
00481      IF CIA-RC-PTR-NULL OR (WS-POINTER1 = WS-POINTER2)            ELGADLCC
00482      THEN                                                         ELGADLCC
00483         CONTINUE                                                  ELGADLCC
00484      ELSE                                                         ELGADLCC
00485         PERFORM 0180-SCAN-CONTRACT-FOR-ACCUMS                     ELGADLCC
00486      END-IF.                                                      ELGADLCC
00487                                                                   ELGADLCC
00488 /***********************************************************      ELGADLCC
00489 *                                                          *      ELGADLCC
00490 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELGADLCC
00491 *                                                          *      ELGADLCC
00492 ************************************************************      ELGADLCC
00493                                                                   ELGADLCC
00494  0150-SCAN-INST-SUP.                                              ELGADLCC
00495      SET CIA-ELSCONIS-DDN TO TRUE.                                ELGADLCC
00496      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGADLCC
00497         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGADLCC
00498         END-CALL.                                                 ELGADLCC
00499      SET WS-POINTER1 TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.      ELGADLCC
00500                                                                   ELGADLCC
00501      IF CIA-RC-PTR-NULL                                           ELGADLCC
00502      THEN                                                         ELGADLCC
00503         CONTINUE                                                  ELGADLCC
00504      ELSE                                                         ELGADLCC
00505         PERFORM 0180-SCAN-CONTRACT-FOR-ACCUMS                     ELGADLCC
00506      END-IF.                                                      ELGADLCC
00507                                                                   ELGADLCC
00508 ************************************************************      ELGADLCC
00509 *                                                          *      ELGADLCC
00510 *    SCAN PROFESSIONAL SUPPLEMENTAL CONTRACT RECORD        *      ELGADLCC
00511 *                                                          *      ELGADLCC
00512 ************************************************************      ELGADLCC
00513                                                                   ELGADLCC
00514  0170-SCAN-PROF-SUP.                                              ELGADLCC
00515      SET CIA-ELSCONPS-DDN TO TRUE.                                ELGADLCC
00516      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGADLCC
00517         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGADLCC
00518         END-CALL.                                                 ELGADLCC
00519      SET WS-POINTER2 TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.      ELGADLCC
00520                                                                   ELGADLCC
00521      IF CIA-RC-PTR-NULL AND (WS-POINTER1 = WS-POINTER2)           ELGADLCC
00522      THEN                                                         ELGADLCC
00523         CONTINUE                                                  ELGADLCC
00524      ELSE                                                         ELGADLCC
00525         PERFORM 0180-SCAN-CONTRACT-FOR-ACCUMS                     ELGADLCC
00526      END-IF.                                                      ELGADLCC
00527                                                                   ELGADLCC
00528 /***********************************************************      ELGADLCC
00529 *                                                          *      ELGADLCC
00530 *    SCAN A CONTRACT RECORD FOR ACCUMULATORS               *      ELGADLCC
00531 *                                                          *      ELGADLCC
00532 ************************************************************      ELGADLCC
00533                                                                   ELGADLCC
00534  0180-SCAN-CONTRACT-FOR-ACCUMS.                                   ELGADLCC
00535      PERFORM WITH TEST BEFORE                                     ELGADLCC
00536         VARYING GCT-TAB-INDEX FROM 1 BY 1                         ELGADLCC
00537           UNTIL    GCT-TAB-INDEX > WS-MAX-GCT-INDEX               ELGADLCC
00538                 OR GCT-CON-TAB-ID (GCT-TAB-INDEX) > PC-ADL        ELGADLCC
00539 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELGADLCC
00540         IF     GCT-CON-TAB-ID (GCT-TAB-INDEX) = PC-ADL            ELGADLCC
00541            AND GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  > ZEROS          ELGADLCC
00542         THEN                                                      ELGADLCC
00543 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELGADLCC
00544            MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  TO  WS-SLOT-NBR ELGADLCC
00545            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELGADLCC
00546         END-IF                                                    ELGADLCC
00547         END-PERFORM.                                              ELGADLCC
00548                                                                   ELGADLCC
00549 /***********************************************************      ELGADLCC
00550 *                                                          *      ELGADLCC
00551 *    SAVE UNIQUE ACCUMULATOR SLOT NUMBER                   *      ELGADLCC
00552 *                                                          *      ELGADLCC
00553 ************************************************************      ELGADLCC
00554                                                                   ELGADLCC
00555  0200-SAVE-UNQ-ACCUM-SLOT-NBR.                                    ELGADLCC
00556                                                                   ELGADLCC
00557 * -- SCAN TABLE OF ACCUM SLOT NUMBERS FOR DUPLICATE               ELGADLCC
00558      SET SW-UNQ-SLOT-NBR TO TRUE.                                 ELGADLCC
00559      PERFORM WITH TEST BEFORE                                     ELGADLCC
00560         VARYING WS-ADL-SUB FROM 1 BY 1                            ELGADLCC
00561           UNTIL    WS-ADL-SUB > WS-ADL-ACCUM-CNT                  ELGADLCC
00562                 OR SW-DUP-SLOT-NBR                                ELGADLCC
00563         IF WS-SLOT-NBR = ACCUM-SLOT-NBR (WS-ADL-SUB)              ELGADLCC
00564         THEN                                                      ELGADLCC
00565            SET SW-DUP-SLOT-NBR TO TRUE                            ELGADLCC
00566         END-IF                                                    ELGADLCC
00567         END-PERFORM.                                              ELGADLCC
00568                                                                   ELGADLCC
00569 * -- IF SLOT NUMBER IS UNIQUE, ADD IT TO THE HOLD TABLE           ELGADLCC
00570      IF SW-UNQ-SLOT-NBR                                           ELGADLCC
00571      THEN                                                         ELGADLCC
00572         ADD 1 TO  WS-ADL-ACCUM-CNT                                ELGADLCC
00573         MOVE WS-SLOT-NBR TO ACCUM-SLOT-NBR(WS-ADL-ACCUM-CNT)      ELGADLCC
00574      END-IF.                                                      ELGADLCC
00575                                                                   ELGADLCC
00576 /***********************************************************      ELGADLCC
00577 *                                                          *      ELGADLCC
00578 *    SCAN ADL ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELGADLCC
00579 *                                                          *      ELGADLCC
00580 ************************************************************      ELGADLCC
00581                                                                   ELGADLCC
00582  0240-SCAN-FOR-APPLIC-OCCRNCS.                                    ELGADLCC
00583      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGADLCC
00584      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGADLCC
00585      PERFORM 0250-DELETE-ADL-SUMMARY-FILE.                        ELGADLCC
00586      PERFORM 0260-ALLOC-WORKFILE-REC-AREA.                        ELGADLCC
00587                                                                   ELGADLCC
00588 * -- READ AND SCAN EACH ACCUMULATOR TABULAR                       ELGADLCC
00589      PERFORM WITH TEST BEFORE                                     ELGADLCC
00590         VARYING WS-ADL-SUB FROM 1 BY 1                            ELGADLCC
00591           UNTIL WS-ADL-SUB > WS-ADL-ACCUM-CNT                     ELGADLCC
00592 *    -- OBTAIN ACCUMULATOR TABULAR RECORD                         ELGADLCC
00593         MOVE PC-ADL TO KWA-PROVISION-ID                           ELGADLCC
00594         MOVE ACCUM-SLOT-NBR (WS-ADL-SUB) TO KWA-PROVISION-SLOT-NO ELGADLCC
00595         PERFORM 0280-READ-TABULAR-REC                             ELGADLCC
00596 *    -- SCAN ACCUMULATOR TABULAR                                  ELGADLCC
00597         PERFORM 0300-TEST-ADL-OCCURENCE                           ELGADLCC
00598            VARYING GAC-INDEX FROM 1 BY 1                          ELGADLCC
00599              UNTIL GAC-INDEX = WS-MAX-GAC-INDEX                   ELGADLCC
00600         END-PERFORM.                                              ELGADLCC
00601                                                                   ELGADLCC
00602 /***********************************************************      ELGADLCC
00603 *                                                          *      ELGADLCC
00604 *        DELETE ADL SUMMARY FILE                           *      ELGADLCC
00605 *                                                          *      ELGADLCC
00606 ************************************************************      ELGADLCC
00607                                                                   ELGADLCC
00608  0250-DELETE-ADL-SUMMARY-FILE.                                    ELGADLCC
00609      SET IOP-DEL TO TRUE.                                         ELGADLCC
00610      SET IOP-FCQ-NONE TO TRUE.                                    ELGADLCC
00611      SET IOP-KVQ-NONE TO TRUE.                                    ELGADLCC
00612      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGADLCC
00613                                                                   ELGADLCC
00614 /***********************************************************      ELGADLCC
00615 *                                                          *      ELGADLCC
00616 *    ALLOCATE WORKFILE RECORD AREA                         *      ELGADLCC
00617 *                                                          *      ELGADLCC
00618 ************************************************************      ELGADLCC
00619                                                                   ELGADLCC
00620  0260-ALLOC-WORKFILE-REC-AREA.                                    ELGADLCC
00621      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGADLCC
00622      SET CIA-STG-GETMAIN TO TRUE.                                 ELGADLCC
00623      SET IOP-GETMAIN-REC TO TRUE.                                 ELGADLCC
00624      COMPUTE IOP-MAX-REC-LEN =                                    ELGADLCC
00625              LENGTH OF ACCUM-FIXED-AREA                           ELGADLCC
00626 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELGADLCC
00627            + LENGTH OF ACCUM-VARIABLE-AREA                        ELGADLCC
00628            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELGADLCC
00629 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELGADLCC
00630 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELGADLCC
00631                                                                   ELGADLCC
00632      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGADLCC
00633      IF IOP-REC-PTR = NULLS                                       ELGADLCC
00634      THEN                                                         ELGADLCC
00635         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGADLCC
00636         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGADLCC
00637      ELSE                                                         ELGADLCC
00638         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELGADLCC
00639      END-IF.                                                      ELGADLCC
00640                                                                   ELGADLCC
00641 /***********************************************************      ELGADLCC
00642 *                                                          *      ELGADLCC
00643 *    READ TABULAR RECORD                                   *      ELGADLCC
00644 *                                                          *      ELGADLCC
00645 ************************************************************      ELGADLCC
00646                                                                   ELGADLCC
00647  0280-READ-TABULAR-REC.                                           ELGADLCC
00648      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELGADLCC
00649      SET IOP-RD TO TRUE.                                          ELGADLCC
00650      SET IOP-FCQ-NONE TO TRUE.                                    ELGADLCC
00651      SET IOP-KVQ-EQ TO TRUE.                                      ELGADLCC
00652      SET IOP-STG-MODE-MOVE TO TRUE.                               ELGADLCC
00653      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGADLCC
00654      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGADLCC
00655      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGADLCC
00656                                                                   ELGADLCC
00657      EVALUATE TRUE                                                ELGADLCC
00658        WHEN IOP-RC-OK                                             ELGADLCC
00659           SET ADDRESS OF GAC-RECORD-AREA TO IOP-REC-PTR           ELGADLCC
00660           SET IOP-REC-PTR TO NULLS                                ELGADLCC
00661           SET GAC-INDEX TO GAC-ENTRY-COUNT                        ELGADLCC
00662           SET WS-MAX-GAC-INDEX TO GAC-INDEX                       ELGADLCC
00663        WHEN IOP-RC-NOTFND                                         ELGADLCC
00664           SET CIA-AB-NOTFND-GCTABULR TO TRUE                      ELGADLCC
00665           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELGADLCC
00666        WHEN OTHER                                                 ELGADLCC
00667           SET CIA-AB-CRITIO TO TRUE                               ELGADLCC
00668           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELGADLCC
00669        END-EVALUATE.                                              ELGADLCC
00670                                                                   ELGADLCC
00671 /***********************************************************      ELGADLCC
00672 *                                                          *      ELGADLCC
00673 *        TEST ADL OCCURS                                   *      ELGADLCC
00674 *                                                          *      ELGADLCC
00675 ************************************************************      ELGADLCC
00676                                                                   ELGADLCC
00677  0300-TEST-ADL-OCCURENCE.                                         ELGADLCC
00678      MOVE GAC-DEDL-L-O-B (GAC-INDEX) TO WS-LOB-ACCUM-OCCRNC.      ELGADLCC
00679      MOVE GAC-DEDL-COST-CONTAIN-IND (GAC-INDEX)                   ELGADLCC
00680        TO WS-COST-CONTAIN-IND.                                    ELGADLCC
00681      SET SW-CC-IND-DOES-NOT-APPLY TO TRUE.                        ELGADLCC
00682      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELGADLCC
00683                                                                   ELGADLCC
00684      PERFORM 0500-CHK-CC-IND.                                     ELGADLCC
00685                                                                   ELGADLCC
00686      IF SW-CC-IND-APPLIES                                         ELGADLCC
00687      THEN                                                         ELGADLCC
00688 *    -- SCAN FOR INTERNAL TABULARS                                ELGADLCC
00689 *       (THIS IS DONE NOW IN CASE IPGT OR IBGR IS NEEDED TO       ELGADLCC
00690 *        DETERMINE WHETHER OCCURRENCE IS INSTITUTIONAL OR         ELGADLCC
00691 *        PROFESSIONAL.)                                           ELGADLCC
00692         PERFORM 0310-SCAN-INTRNL-TAB                              ELGADLCC
00693         EVALUATE TRUE ALSO TRUE                                   ELGADLCC
00694            WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                     ELGADLCC
00695               SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE               ELGADLCC
00696               SET SW-OCCRNC-APPLIES TO TRUE                       ELGADLCC
00697            WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH              ELGADLCC
00698               SET SRP-ACCUM-PROV-CLASS-INST TO TRUE               ELGADLCC
00699               PERFORM 0550-CHK-INTRNL-TAB-PROV-CLASS              ELGADLCC
00700            WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST              ELGADLCC
00701               SET SRP-ACCUM-PROV-CLASS-INST TO TRUE               ELGADLCC
00702               SET SW-OCCRNC-APPLIES TO TRUE                       ELGADLCC
00703            WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH              ELGADLCC
00704               SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE               ELGADLCC
00705               PERFORM 0550-CHK-INTRNL-TAB-PROV-CLASS              ELGADLCC
00706               SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                ELGADLCC
00707               PERFORM 0551-CHK-INTRNL-TAB-PROV-SPEC               ELGADLCC
00708            WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF              ELGADLCC
00709               SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE               ELGADLCC
00710               SET SW-OCCRNC-APPLIES TO TRUE                       ELGADLCC
00711            WHEN OTHER                                             ELGADLCC
00712               CONTINUE                                            ELGADLCC
00713            END-EVALUATE                                           ELGADLCC
00714      ELSE                                                         ELGADLCC
00715         CONTINUE                                                  ELGADLCC
00716      END-IF.                                                      ELGADLCC
00717                                                                   ELGADLCC
00718      IF SW-OCCRNC-APPLIES                                         ELGADLCC
00719      THEN                                                         ELGADLCC
00720 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELGADLCC
00721         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELGADLCC
00722         PERFORM 0330-INIT-ACCUM-EXTRACT                           ELGADLCC
00723         PERFORM 0340-EXTRACT-ACCUM                                ELGADLCC
00724         PERFORM 0410-CHK-EXTRACT-DATA-INTGRTY                     ELGADLCC
00725         PERFORM 0710-WRITE-EXTRACT-RECORD                         ELGADLCC
00726      END-IF.                                                      ELGADLCC
00727                                                                   ELGADLCC
00728 /***********************************************************      ELGADLCC
00729 *                                                          *      ELGADLCC
00730 *    SCAN INTERNAL TABULARS                                *      ELGADLCC
00731 *                                                          *      ELGADLCC
00732 ************************************************************      ELGADLCC
00733                                                                   ELGADLCC
00734  0310-SCAN-INTRNL-TAB.                                            ELGADLCC
00735                                                                   ELGADLCC
00736 * -- INITIALIZE SCAN PROCESS                                      ELGADLCC
00737      SET SW-OCCRNC-DOES-NOT-APPLY                                 ELGADLCC
00738          SW-HAS-NO-IBGR                                           ELGADLCC
00739          SW-HAS-NO-IDGD                                           ELGADLCC
00740          SW-HAS-NO-IPGN                                           ELGADLCC
00741          SW-HAS-NO-IPGP                                           ELGADLCC
00742          SW-HAS-NO-IPGT                                           ELGADLCC
00743          SW-HAS-NO-IPGS                                           ELGADLCC
00744       TO TRUE.                                                    ELGADLCC
00745      INITIALIZE WS-IBGR-SLOT-NBR                                  ELGADLCC
00746                 WS-IDGD-SLOT-NBR                                  ELGADLCC
00747                 WS-IPGN-SLOT-NBR                                  ELGADLCC
00748                 WS-IPGP-SLOT-NBR                                  ELGADLCC
00749                 WS-IPGT-SLOT-NBR                                  ELGADLCC
00750                 WS-IPGS-SLOT-NBR.                                 ELGADLCC
00751      SET GAC-INT-INDEX TO GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX). ELGADLCC
00752      SET WS-MAX-GAC-INT-INDEX TO GAC-INT-INDEX.                   ELGADLCC
00753                                                                   ELGADLCC
00754 * -- SCAN THE LIST OF INTERNAL TABULARS                           ELGADLCC
00755      PERFORM 0320-SCAN-THE-INTERNAL-TABULAR                       ELGADLCC
00756         VARYING GAC-INT-INDEX FROM 1 BY 1                         ELGADLCC
00757           UNTIL GAC-INT-INDEX >= WS-MAX-GAC-INT-INDEX.            ELGADLCC
00758                                                                   ELGADLCC
00759 /***********************************************************      ELGADLCC
00760 *                                                          *      ELGADLCC
00761 *    SCAN INTERNAL TABULAR LIST                            *      ELGADLCC
00762 *                                                          *      ELGADLCC
00763 ************************************************************      ELGADLCC
00764                                                                   ELGADLCC
00765  0320-SCAN-THE-INTERNAL-TABULAR.                                  ELGADLCC
00766      IF GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX) > 0               ELGADLCC
00767      THEN                                                         ELGADLCC
00768         MOVE GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX)              ELGADLCC
00769           TO WS-SLOT-NBR                                          ELGADLCC
00770         EVALUATE GAC-INT-ID (GAC-INDEX, GAC-INT-INDEX)            ELGADLCC
00771            WHEN PC-IBGR                                           ELGADLCC
00772               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELGADLCC
00773               SET SW-HAS-IBGR                                     ELGADLCC
00774                TO TRUE                                            ELGADLCC
00775            WHEN PC-IDGD                                           ELGADLCC
00776               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELGADLCC
00777               SET SW-HAS-IDGD                                     ELGADLCC
00778                TO TRUE                                            ELGADLCC
00779            WHEN PC-IPGP                                           ELGADLCC
00780               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELGADLCC
00781               SET SW-HAS-IPGP                                     ELGADLCC
00782                TO TRUE                                            ELGADLCC
00783            WHEN PC-IPGN                                           ELGADLCC
00784               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELGADLCC
00785               SET SW-HAS-IPGN                                     ELGADLCC
00786                TO TRUE                                            ELGADLCC
00787            WHEN PC-IPGT                                           ELGADLCC
00788               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELGADLCC
00789               SET SW-HAS-IPGT                                     ELGADLCC
00790                TO TRUE                                            ELGADLCC
00791            WHEN PC-IPGS                                           ELGADLCC
00792               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELGADLCC
00793               SET SW-HAS-IPGS                                     ELGADLCC
00794                TO TRUE                                            ELGADLCC
00795            WHEN OTHER                                             ELGADLCC
00796               CONTINUE                                            ELGADLCC
00797            END-EVALUATE                                           ELGADLCC
00798      END-IF.                                                      ELGADLCC
00799                                                                   ELGADLCC
00800 /***********************************************************      ELGADLCC
00801 *                                                          *      ELGADLCC
00802 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELGADLCC
00803 *                                                          *      ELGADLCC
00804 ************************************************************      ELGADLCC
00805                                                                   ELGADLCC
00806  0330-INIT-ACCUM-EXTRACT.                                         ELGADLCC
00807      INITIALIZE ACCUM-FIXED-AREA.                                 ELGADLCC
00808      SET ACCUM-ADL TO TRUE.                                       ELGADLCC
00809      MOVE 1 TO  ACCUM-ASCEND-DESCEND-COUNT.                       ELGADLCC
00810      SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELGADLCC
00811      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELGADLCC
00812                                                                   ELGADLCC
00813 /***********************************************************      ELGADLCC
00814 *                                                          *      ELGADLCC
00815 *        SUMMARIZE ADL TOPIC LEVEL DATA ELEMENTS           *      ELGADLCC
00816 *                                                          *      ELGADLCC
00817 ************************************************************      ELGADLCC
00818                                                                   ELGADLCC
00819  0340-EXTRACT-ACCUM.                                              ELGADLCC
00820      MOVE GAC-DEDL-FYI-VALUE (GAC-INDEX) TO ACCUM-FYI-VALUE.      ELGADLCC
00821      MOVE GAC-DEDL-COST-CONTAIN-IND (GAC-INDEX)                   ELGADLCC
00822        TO ACCUM-COST-CONTAIN-IND.                                 ELGADLCC
00823      MOVE GAC-DEDL-PLACE-OF-TREATMENT (GAC-INDEX)                 ELGADLCC
00824        TO ACCUM-PLACE-OF-TREATMENT.                               ELGADLCC
00825      MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)                     ELGADLCC
00826        TO ACCUM-BENEFIT-PERIOD.                                   ELGADLCC
00827      MOVE GAC-DEDL-BEN-PER-TIME-FCTR (GAC-INDEX)                  ELGADLCC
00828        TO ACCUM-BEN-PER-TIME-FCTR.                                ELGADLCC
00829      MOVE GAC-DEDL-BEN-PER-TIME-QUAL (GAC-INDEX)                  ELGADLCC
00830        TO ACCUM-BEN-PER-TIME-QUAL.                                ELGADLCC
00831      MOVE GAC-DEDL-INTERVAL-TIME-FCTR (GAC-INDEX)                 ELGADLCC
00832        TO ACCUM-INTERVAL-TIME-FCTR.                               ELGADLCC
00833      MOVE GAC-DEDL-INTERVAL-TYPE (GAC-INDEX)                      ELGADLCC
00834        TO ACCUM-INTERVAL-TYPE.                                    ELGADLCC
00835      MOVE GAC-DEDL-INTERVAL-OVRD-IND (GAC-INDEX)                  ELGADLCC
00836        TO ACCUM-INTERVAL-OVRD-IND.                                ELGADLCC
00837      MOVE GAC-DEDL-INTERVAL-OVRD-VALUE (GAC-INDEX)                ELGADLCC
00838        TO ACCUM-INTERVAL-OVRD-VALUE.                              ELGADLCC
00839      MOVE GAC-DEDL-L-O-B (GAC-INDEX) TO ACCUM-L-O-B.              ELGADLCC
00840      EVALUATE TRUE ALSO TRUE                                      ELGADLCC
00841         WHEN      SW-INTRNL-INST-PROV-CLASS                       ELGADLCC
00842              ALSO SW-INTRNL-PROF-PROV-CLASS                       ELGADLCC
00843            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELGADLCC
00844         WHEN      SW-INTRNL-INST-PROV-CLASS                       ELGADLCC
00845              ALSO SW-INTRNL-NOT-PROF-PROV-CLASS                   ELGADLCC
00846            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELGADLCC
00847         WHEN      SW-INTRNL-NOT-INST-PROV-CLASS                   ELGADLCC
00848              ALSO SW-INTRNL-PROF-PROV-CLASS                       ELGADLCC
00849            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELGADLCC
00850         WHEN OTHER                                                ELGADLCC
00851            CONTINUE                                               ELGADLCC
00852         END-EVALUATE.                                             ELGADLCC
00853         IF SW-INTRNL-PROF-PROV-SPEC                               ELGADLCC
00854            SET ACCUM-PRVDR-SPC-PROF TO TRUE                       ELGADLCC
00855         END-IF.                                                   ELGADLCC
00856      MOVE GAC-DEDL-DEFINITION (GAC-INDEX) TO ACCUM-DEFINITION.    ELGADLCC
00857      SET CARRY-OVER-CREDIT-IND-NA                                 ELGADLCC
00858          ASCEND-DESCEND-IND-NA                                    ELGADLCC
00859       TO TRUE.                                                    ELGADLCC
00860      MOVE GAC-DEDL-CONDITION (GAC-INDEX) TO  ACCUM-CONDITION.     ELGADLCC
00861      MOVE GAC-DEDL-FAM-OR-INDIV (GAC-INDEX)                       ELGADLCC
00862        TO ACCUM-FAM-OR-INDIV.                                     ELGADLCC
00863      MOVE ZEROS TO ACCUM-DED-BASE-AMT-SOURCE-IND.                 ELGADLCC
00864      MOVE ZEROS TO ACCUM-OPX-BASE-AMT-SOURCE-IND.                 ELGADLCC
00865      MOVE GCG-MAX-BASE-AMT-SOURCE-IND                             ELGADLCC
00866        TO ACCUM-MAX-BASE-AMT-SOURCE-IND.                          ELGADLCC
00867      MOVE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)                    ELGADLCC
00868        TO ACCUM-VALUE-QUALIFIER.                                  ELGADLCC
00869      MOVE GAC-DEDL-RELATIONSHIP-IND (GAC-INDEX)                   ELGADLCC
00870        TO ACCUM-RELATIONSHIP-IND.                                 ELGADLCC
00871      MOVE GAC-DEDL-AGE-LIMIT-FROM (GAC-INDEX)                     ELGADLCC
00872        TO ACCUM-AGE-LIMIT-FROM-VAL.                               ELGADLCC
00873      MOVE GAC-DEDL-AGE-QUAL-IND-FROM (GAC-INDEX)                  ELGADLCC
00874        TO ACCUM-AGE-LIMIT-FROM-IND.                               ELGADLCC
00875      MOVE GAC-DEDL-AGE-LIMIT-TO (GAC-INDEX)                       ELGADLCC
00876        TO ACCUM-AGE-LIMIT-TO-VAL.                                 ELGADLCC
00877      MOVE GAC-DEDL-AGE-QUAL-IND-TO (GAC-INDEX)                    ELGADLCC
00878        TO ACCUM-AGE-LIMIT-TO-IND.                                 ELGADLCC
00879      SET LMT-MANDATORY-IND-NA TO TRUE.                            ELGADLCC
00880      MOVE GAC-DEDL-CO-PAY-IND (GAC-INDEX)                         ELGADLCC
00881        TO ACCUM-CO-PAY-IND(COPAY-INDEX)                           ELGADLCC
00882      MOVE GAC-DEDL-SERVICE-GROUP (GAC-INDEX)                      ELGADLCC
00883        TO ACCUM-SERVICE-GROUP.                                    ELGADLCC
00884      MOVE GAC-DEDL-INTERNAL-DESCRIPTOR (GAC-INDEX)                ELGADLCC
00885        TO ACCUM-INTERNAL-DESCRIPTOR.                              ELGADLCC
00886      MOVE GAC-DEDL-DAY-FACTOR-IND (GAC-INDEX)                     ELGADLCC
00887        TO ACCUM-DAY-FACTOR-IND.                                   ELGADLCC
00888      MOVE GAC-DEDL-CLAIM-LVL-ACCUM-IND (GAC-INDEX)                ELGADLCC
00889        TO ACCUM-CLAIM-LVL-ACCUM-IND.                              ELGADLCC
00890      SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE.                          ELGADLCC
00891                                                                   ELGADLCC
00892      MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                        ELGADLCC
00893        TO ACCUM-VALUE-LIMIT (1).                                  ELGADLCC
00894      MOVE WS-IBGR-SLOT-NBR TO ACCUM-IBGR-SLOT-NBR (1).            ELGADLCC
00895      MOVE WS-IDGD-SLOT-NBR TO ACCUM-IDGD-SLOT-NBR (1).            ELGADLCC
00896      MOVE WS-IPGN-SLOT-NBR TO ACCUM-IPGN-SLOT-NBR (1).            ELGADLCC
00897      MOVE WS-IPGP-SLOT-NBR TO ACCUM-IPGP-SLOT-NBR (1).            ELGADLCC
00898      MOVE WS-IPGT-SLOT-NBR TO ACCUM-IPGT-SLOT-NBR (1).            ELGADLCC
00899      MOVE WS-IPGS-SLOT-NBR TO ACCUM-IPGS-SLOT-NBR (1).            ELGADLCC
00900                                                                   ELGADLCC
00901 /***********************************************************      ELGADLCC
00902 *                                                          *      ELGADLCC
00903 *    CHECK EXTRACT DATA INTEGRITY                          *      ELGADLCC
00904 *                                                          *      ELGADLCC
00905 ************************************************************      ELGADLCC
00906                                                                   ELGADLCC
00907  0410-CHK-EXTRACT-DATA-INTGRTY.                                   ELGADLCC
00908      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELGADLCC
00909      THEN                                                         ELGADLCC
00910         SET FYI-VALUE-NA TO TRUE                                  ELGADLCC
00911      END-IF.                                                      ELGADLCC
00912                                                                   ELGADLCC
00913      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELGADLCC
00914      THEN                                                         ELGADLCC
00915         SET COST-CONTAIN-IND-NA TO TRUE                           ELGADLCC
00916      END-IF.                                                      ELGADLCC
00917                                                                   ELGADLCC
00918      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELGADLCC
00919      THEN                                                         ELGADLCC
00920         SET PLACE-OF-TREATMENT-NA TO TRUE                         ELGADLCC
00921      END-IF.                                                      ELGADLCC
00922                                                                   ELGADLCC
00923      IF ACCUM-BENEFIT-PERIOD = ZEROS OR SPACES OR LOW-VALUES      ELGADLCC
00924      THEN                                                         ELGADLCC
00925         SET BENEFIT-PERIOD-NA TO TRUE                             ELGADLCC
00926      END-IF.                                                      ELGADLCC
00927                                                                   ELGADLCC
00928      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELGADLCC
00929      THEN                                                         ELGADLCC
00930         SET INTERVAL-OVRD-IND-NA TO TRUE                          ELGADLCC
00931      END-IF.                                                      ELGADLCC
00932                                                                   ELGADLCC
00933      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELGADLCC
00934      THEN                                                         ELGADLCC
00935         SET L-O-B-NA TO TRUE                                      ELGADLCC
00936      END-IF.                                                      ELGADLCC
00937                                                                   ELGADLCC
00938      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELGADLCC
00939      THEN                                                         ELGADLCC
00940         SET REINSTATEMENT-IND-NA TO TRUE                          ELGADLCC
00941      END-IF.                                                      ELGADLCC
00942                                                                   ELGADLCC
00943      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELGADLCC
00944      THEN                                                         ELGADLCC
00945         SET DEFINITION-NA TO TRUE                                 ELGADLCC
00946      END-IF.                                                      ELGADLCC
00947                                                                   ELGADLCC
00948      IF   ACCUM-CARRY-OVER-CREDIT-IND                             ELGADLCC
00949         = ZEROS OR SPACES OR LOW-VALUES                           ELGADLCC
00950      THEN                                                         ELGADLCC
00951         SET CARRY-OVER-CREDIT-IND-NA TO TRUE                      ELGADLCC
00952      END-IF.                                                      ELGADLCC
00953                                                                   ELGADLCC
00954      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELGADLCC
00955      THEN                                                         ELGADLCC
00956         SET ASCEND-DESCEND-IND-NA TO TRUE                         ELGADLCC
00957      END-IF.                                                      ELGADLCC
00958                                                                   ELGADLCC
00959      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELGADLCC
00960      THEN                                                         ELGADLCC
00961         SET RELATIONSHIP-IND-NA TO TRUE                           ELGADLCC
00962      END-IF.                                                      ELGADLCC
00963                                                                   ELGADLCC
00964      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELGADLCC
00965      THEN                                                         ELGADLCC
00966         SET AGE-LMT-TO-IND-NA TO TRUE                             ELGADLCC
00967      END-IF.                                                      ELGADLCC
00968                                                                   ELGADLCC
00969      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELGADLCC
00970      THEN                                                         ELGADLCC
00971         SET AGE-LMT-FROM-IND-NA TO TRUE                           ELGADLCC
00972      END-IF.                                                      ELGADLCC
00973                                                                   ELGADLCC
00974      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELGADLCC
00975      THEN                                                         ELGADLCC
00976         SET LMT-MANDATORY-IND-NA TO TRUE                          ELGADLCC
00977      END-IF.                                                      ELGADLCC
00978                                                                   ELGADLCC
00979      IF ACCUM-CO-PAY-IND (COPAY-INDEX)                            ELGADLCC
00980              = ZEROS OR SPACES OR LOW-VALUES                      ELGADLCC
00981      THEN                                                         ELGADLCC
00982         SET CO-PAY-IND-NA (COPAY-INDEX) TO TRUE                   ELGADLCC
00983      END-IF.                                                      ELGADLCC
00984                                                                   ELGADLCC
00985      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELGADLCC
00986      THEN                                                         ELGADLCC
00987         SET SERVICE-GROUP-NA TO TRUE                              ELGADLCC
00988      END-IF.                                                      ELGADLCC
00989                                                                   ELGADLCC
00990      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELGADLCC
00991      THEN                                                         ELGADLCC
00992         SET INTERNAL-DESCRIPTOR-NA TO TRUE                        ELGADLCC
00993      END-IF.                                                      ELGADLCC
00994                                                                   ELGADLCC
00995      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELGADLCC
00996      THEN                                                         ELGADLCC
00997         SET DAY-FACTOR-IND-NA TO TRUE                             ELGADLCC
00998      END-IF.                                                      ELGADLCC
00999                                                                   ELGADLCC
01000      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELGADLCC
01001      THEN                                                         ELGADLCC
01002         SET CLAIM-LVL-ACCUM-IND-NA TO TRUE                        ELGADLCC
01003      END-IF.                                                      ELGADLCC
01004                                                                   ELGADLCC
01005      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELGADLCC
01006      THEN                                                         ELGADLCC
01007         SET BEN-PER-MAX-OVRD-IND-NA TO TRUE                       ELGADLCC
01008      END-IF.                                                      ELGADLCC
01009                                                                   ELGADLCC
01010      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELGADLCC
01011      THEN                                                         ELGADLCC
01012         SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE                        ELGADLCC
01013      END-IF.                                                      ELGADLCC
01014                                                                   ELGADLCC
01015 /*****************************************************************ELGADLCC
01016 *                                                                *ELGADLCC
01017 *    CHECK COST CONTAINMENT INDICATOR TO DETERMINE APPLICABILITY *ELGADLCC
01018 *                                                                *ELGADLCC
01019 ******************************************************************ELGADLCC
01020                                                                   ELGADLCC
01021  0500-CHK-CC-IND.                                                 ELGADLCC
01022      MOVE SSB-MODIFIER-1 TO WS-SUBTOPIC.                          ELGADLCC
01023      EVALUATE TRUE              ALSO TRUE                         ELGADLCC
01024         WHEN WS-SUBTOPIC-ATCP   ALSO WS-CCI-ATCP                  ELGADLCC
01025            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01026         WHEN WS-SUBTOPIC-BAE   ALSO WS-CCI-BAE                    ELGADLCC
01027            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01028         WHEN WS-SUBTOPIC-EMH    ALSO WS-CCI-EMH                   ELGADLCC
01029            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01030         WHEN WS-SUBTOPIC-HOSP   ALSO WS-CCI-HOSP                  ELGADLCC
01031            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01032         WHEN WS-SUBTOPIC-IOB    ALSO WS-CCI-IOB                   ELGADLCC
01033            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01034         WHEN WS-SUBTOPIC-MASOP  ALSO WS-CCI-MASOP                 ELGADLCC
01035            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01036         WHEN WS-SUBTOPIC-MCN    ALSO WS-CCI-MCN                   ELGADLCC
01037            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01038         WHEN WS-SUBTOPIC-MEDNEC ALSO WS-CCI-MEDNEC                ELGADLCC
01039            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01040         WHEN WS-SUBTOPIC-MHSC   ALSO WS-CCI-MHSC                  ELGADLCC
01041            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01042         WHEN WS-SUBTOPIC-MOND   ALSO WS-CCI-MOND                  ELGADLCC
01043            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01044         WHEN WS-SUBTOPIC-MOPS   ALSO WS-CCI-MOPS                  ELGADLCC
01045            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01046         WHEN WS-SUBTOPIC-MSA    ALSO WS-CCI-MSA                   ELGADLCC
01047            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01048         WHEN WS-SUBTOPIC-PAR    ALSO WS-CCI-PAR                   ELGADLCC
01049            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01050         WHEN WS-SUBTOPIC-PAT    ALSO WS-CCI-PAT                   ELGADLCC
01051            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01052         WHEN WS-SUBTOPIC-POS    ALSO WS-CCI-POS                   ELGADLCC
01053            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01054         WHEN WS-SUBTOPIC-PPO    ALSO WS-CCI-PPO                   ELGADLCC
01055            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01056         WHEN WS-SUBTOPIC-REIMB  ALSO WS-CCI-REIMB                 ELGADLCC
01057            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01058         WHEN WS-SUBTOPIC-RPO  ALSO WS-CCI-RPO                     ELGADLCC
01059            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01060         WHEN WS-SUBTOPIC-CPO  ALSO WS-CCI-CPO                     ELGADLCC
01061            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01062         WHEN WS-SUBTOPIC-CBL  ALSO WS-CCI-CBL                     ELGADLCC
01063            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01064         WHEN WS-SUBTOPIC-PAN  ALSO WS-CCI-PAN                     ELGADLCC
01065            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01066         WHEN WS-SUBTOPIC-WEEK   ALSO WS-CCI-WEEK                  ELGADLCC
01067            SET SW-CC-IND-APPLIES TO TRUE                          ELGADLCC
01068         WHEN OTHER                                                ELGADLCC
01069            CONTINUE                                               ELGADLCC
01070         END-EVALUATE.                                             ELGADLCC
01071                                                                   ELGADLCC
01072 /***********************************************************      ELGADLCC
01073 *                                                          *      ELGADLCC
01074 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER CLASS   *      ELGADLCC
01075 *                                                          *      ELGADLCC
01076 ************************************************************      ELGADLCC
01077                                                                   ELGADLCC
01078  0550-CHK-INTRNL-TAB-PROV-CLASS.                                  ELGADLCC
01079      IF SW-HAS-IPGT                                               ELGADLCC
01080      THEN                                                         ELGADLCC
01081         PERFORM 0560-CHK-IPGT-PROV-CLASS                          ELGADLCC
01082      ELSE                                                         ELGADLCC
01083         IF SW-HAS-IBGR                                            ELGADLCC
01084         THEN                                                      ELGADLCC
01085            PERFORM 0640-CHK-IBGR-PROV-CLASS                       ELGADLCC
01086         ELSE                                                      ELGADLCC
01087            SET SW-OCCRNC-APPLIES TO TRUE                          ELGADLCC
01088         END-IF                                                    ELGADLCC
01089      END-IF.                                                      ELGADLCC
01090                                                                   ELGADLCC
01091 /***********************************************************      ELGADLCC
01092 *                                                          *      ELGADLCC
01093 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER SPEC    *      ELGADLCC
01094 *                                                          *      ELGADLCC
01095 ************************************************************      ELGADLCC
01096                                                                   ELGADLCC
01097  0551-CHK-INTRNL-TAB-PROV-SPEC.                                   ELGADLCC
01098      IF SW-HAS-IPGS                                               ELGADLCC
01099         PERFORM 0561-CHK-IPGS-PROV-SPEC                           ELGADLCC
01100 *    ELSE                                                         ELGADLCC
01101 *       IF SW-HAS-IBGR                                            ELGADLCC
01102 *       WHEN                                                      ELGADLCC
01103 *          PERFORM 0640-CHK-IBGR-PROV-CLASS                       ELGADLCC
01104 *       ELSE                                                      ELGADLCC
01105 *          SET SW-OCCRNC-APPLIES TO TRUE                          ELGADLCC
01106 *       END-IF                                                    ELGADLCC
01107      END-IF.                                                      ELGADLCC
01108                                                                   ELGADLCC
01109 /*****************************************************************ELGADLCC
01110 *                                                                *ELGADLCC
01111 *    CHECK IPGT INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELGADLCC
01112 *                                                                *ELGADLCC
01113 ******************************************************************ELGADLCC
01114                                                                   ELGADLCC
01115  0560-CHK-IPGT-PROV-CLASS.                                        ELGADLCC
01116      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELGADLCC
01117      MOVE PC-IPGT TO KWA-PROVISION-ID.                            ELGADLCC
01118      MOVE WS-IPGT-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGADLCC
01119      PERFORM 0700-READ-INTRNL-TAB.                                ELGADLCC
01120      SET ADDRESS OF GX3-RECORD-AREA TO IOP-REC-PTR.               ELGADLCC
01121      SET IOP-REC-PTR TO NULLS.                                    ELGADLCC
01122      SET GX3-INDEX TO GX3-ENTRY-COUNT.                            ELGADLCC
01123      SET WS-MAX-GX3-INDEX TO GX3-INDEX.                           ELGADLCC
01124                                                                   ELGADLCC
01125      IF GX3-ID-ARGUMENT-INCLUDED                                  ELGADLCC
01126      THEN                                                         ELGADLCC
01127         PERFORM 0570-CHK-INCLD-TYPE-IPGT                          ELGADLCC
01128      ELSE                                                         ELGADLCC
01129          PERFORM 0600-CHK-EXCLD-TYPE-IPGT                         ELGADLCC
01130      END-IF.                                                      ELGADLCC
01131                                                                   ELGADLCC
01132 /*****************************************************************ELGADLCC
01133 *                                                                *ELGADLCC
01134 *    CHECK IPGS INTERNAL TABULAR TO DETERMINE PROVIDER SPECS     *ELGADLCC
01135 *                                                                *ELGADLCC
01136 ******************************************************************ELGADLCC
01137                                                                   ELGADLCC
01138  0561-CHK-IPGS-PROV-SPEC.                                         ELGADLCC
01139      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELGADLCC
01140      MOVE PC-IPGS TO KWA-PROVISION-ID.                            ELGADLCC
01141      MOVE WS-IPGS-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGADLCC
01142      PERFORM 0700-READ-INTRNL-TAB.                                ELGADLCC
01143      SET ADDRESS OF GXS-RECORD-AREA TO IOP-REC-PTR.               ELGADLCC
01144      SET IOP-REC-PTR TO NULLS.                                    ELGADLCC
01145      SET GXS-INDEX TO GXS-ENTRY-COUNT.                            ELGADLCC
01146      SET WS-MAX-GXS-INDEX TO GXS-INDEX.                           ELGADLCC
01147                                                                   ELGADLCC
01148      IF GXS-ID-ARGUMENT-INCLUDED                                  ELGADLCC
01149         PERFORM 0571-CHK-INCLD-TYPE-IPGS                          ELGADLCC
01150      ELSE                                                         ELGADLCC
01151          PERFORM 0601-CHK-EXCLD-TYPE-IPGS                         ELGADLCC
01152      END-IF.                                                      ELGADLCC
01153                                                                   ELGADLCC
01154 ************************************************************      ELGADLCC
01155 *                                                          *      ELGADLCC
01156 *    CHECK INCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGADLCC
01157 *                                                          *      ELGADLCC
01158 ************************************************************      ELGADLCC
01159                                                                   ELGADLCC
01160  0570-CHK-INCLD-TYPE-IPGT.                                        ELGADLCC
01161      SET CFT2-IDX TO 1.                                           ELGADLCC
01162      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGADLCC
01163          SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGADLCC
01164       TO TRUE.                                                    ELGADLCC
01165      PERFORM 0580-TEST-IPGT-INCLD-ENTRIES                         ELGADLCC
01166         VARYING GX3-INDEX  FROM 1 BY 1                            ELGADLCC
01167           UNTIL    GX3-INDEX = WS-MAX-GX3-INDEX                   ELGADLCC
01168                 OR (    SW-INTRNL-INST-PROV-CLASS                 ELGADLCC
01169                     AND SW-INTRNL-PROF-PROV-CLASS ).              ELGADLCC
01170                                                                   ELGADLCC
01171 ************************************************************      ELGADLCC
01172 *                                                          *      ELGADLCC
01173 *    CHECK INCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELGADLCC
01174 *                                                          *      ELGADLCC
01175 ************************************************************      ELGADLCC
01176                                                                   ELGADLCC
01177  0571-CHK-INCLD-TYPE-IPGS.                                        ELGADLCC
01178      SET CFT9-IDX TO 1.                                           ELGADLCC
01179      SET SW-INTRNL-NOT-PROF-PROV-SPEC                             ELGADLCC
01180       TO TRUE.                                                    ELGADLCC
01181      PERFORM 0581-TEST-IPGS-INCLD-ENTRIES                         ELGADLCC
01182         VARYING GXS-INDEX  FROM 1 BY 1                            ELGADLCC
01183           UNTIL    GXS-INDEX = WS-MAX-GXS-INDEX                   ELGADLCC
01184                 OR      SW-INTRNL-PROF-PROV-SPEC.                 ELGADLCC
01185                                                                   ELGADLCC
01186 ************************************************************      ELGADLCC
01187 *                                                          *      ELGADLCC
01188 *    TEST IPGT INCLUDE ENTRIES TO DETERMINE PROVIDER CLASS *      ELGADLCC
01189 *                                                          *      ELGADLCC
01190 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELGADLCC
01191 *          CFT2 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELGADLCC
01192 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELGADLCC
01193 *          OF THE IPGT TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELGADLCC
01194 *                                                          *      ELGADLCC
01195 ************************************************************      ELGADLCC
01196                                                                   ELGADLCC
01197  0580-TEST-IPGT-INCLD-ENTRIES.                                    ELGADLCC
01198      PERFORM WITH TEST BEFORE                                     ELGADLCC
01199         UNTIL    SW-OCCRNC-APPLIES                                ELGADLCC
01200               OR   CFT2-PT (CFT2-IDX)                             ELGADLCC
01201                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELGADLCC
01202               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELGADLCC
01203         IF   GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELGADLCC
01204            = CFT2-PT (CFT2-IDX)                                   ELGADLCC
01205         THEN                                                      ELGADLCC
01206 *    -- TEST PROVIDER CLASS                                       ELGADLCC
01207            EVALUATE TRUE                                          ELGADLCC
01208               WHEN CFT2-PT-INST (CFT2-IDX)                        ELGADLCC
01209                  SET SW-INTRNL-INST-PROV-CLASS TO TRUE            ELGADLCC
01210                  IF SRP-ACCUM-PROV-CLASS-INST                     ELGADLCC
01211                  THEN                                             ELGADLCC
01212                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGADLCC
01213                  END-IF                                           ELGADLCC
01214               WHEN CFT2-PT-PROF (CFT2-IDX)                        ELGADLCC
01215                  SET SW-INTRNL-PROF-PROV-CLASS TO TRUE            ELGADLCC
01216                  IF SRP-ACCUM-PROV-CLASS-PROF                     ELGADLCC
01217                  THEN                                             ELGADLCC
01218                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGADLCC
01219                  END-IF                                           ELGADLCC
01220               END-EVALUATE                                        ELGADLCC
01221         END-IF                                                    ELGADLCC
01222         SET CFT2-IDX UP BY 1                                      ELGADLCC
01223         END-PERFORM.                                              ELGADLCC
01224                                                                   ELGADLCC
01225 ************************************************************      ELGADLCC
01226 *                                                          *      ELGADLCC
01227 *    TEST IPGS INCLUDE ENTRIES TO DETERMINE PROVIDER SPEC  *      ELGADLCC
01228 *                                                          *      ELGADLCC
01229 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELGADLCC
01230 *          CFT9 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELGADLCC
01231 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELGADLCC
01232 *          OF THE IPGT TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELGADLCC
01233 *                                                          *      ELGADLCC
01234 ************************************************************      ELGADLCC
01235                                                                   ELGADLCC
01236  0581-TEST-IPGS-INCLD-ENTRIES.                                    ELGADLCC
01237      PERFORM WITH TEST BEFORE                                     ELGADLCC
01238         UNTIL    SW-OCCRNC-APPLIES                                ELGADLCC
01239               OR   CFT9-PT (CFT9-IDX)                             ELGADLCC
01240                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELGADLCC
01241               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELGADLCC
01242         IF   GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELGADLCC
01243            = CFT9-PT (CFT9-IDX)                                   ELGADLCC
01244 *    -- TEST PROVIDER CLASS                                       ELGADLCC
01245            EVALUATE TRUE                                          ELGADLCC
01246               WHEN CFT9-PT-PROF (CFT9-IDX)                        ELGADLCC
01247                  SET SW-INTRNL-PROF-PROV-SPEC TO TRUE             ELGADLCC
01248                  IF SRP-ACCUM-PROV-SPEC-PROF                      ELGADLCC
01249                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGADLCC
01250                  END-IF                                           ELGADLCC
01251               END-EVALUATE                                        ELGADLCC
01252         END-IF                                                    ELGADLCC
01253         SET CFT9-IDX UP BY 1                                      ELGADLCC
01254         END-PERFORM.                                              ELGADLCC
01255                                                                   ELGADLCC
01256 /***********************************************************      ELGADLCC
01257 *                                                          *      ELGADLCC
01258 *    CHECK EXCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGADLCC
01259 *                                                          *      ELGADLCC
01260 ************************************************************      ELGADLCC
01261                                                                   ELGADLCC
01262  0600-CHK-EXCLD-TYPE-IPGT.                                        ELGADLCC
01263                                                                   ELGADLCC
01264 * -- INITIALIZE CFT2 TABLE TO INCLUDE ALL PROVIDER TYPES          ELGADLCC
01265      PERFORM WITH TEST BEFORE                                     ELGADLCC
01266         VARYING CFT2-IDX FROM 1 BY 1                              ELGADLCC
01267           UNTIL CFT2-IDX > CFT2-NBR-TBL-ENTRIES                   ELGADLCC
01268         SET  CFT2-PT-INCLUDE (CFT2-IDX) TO TRUE                   ELGADLCC
01269         END-PERFORM.                                              ELGADLCC
01270                                                                   ELGADLCC
01271 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGT                 ELGADLCC
01272      SET  CFT2-IDX TO 1.                                          ELGADLCC
01273      PERFORM 0610-TAG-EXCLD-IPGT-ENTRIES                          ELGADLCC
01274         VARYING GX3-INDEX FROM 1 BY 1                             ELGADLCC
01275           UNTIL GX3-INDEX = WS-MAX-GX3-INDEX.                     ELGADLCC
01276                                                                   ELGADLCC
01277 * -- CHECK CFT2 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELGADLCC
01278      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGADLCC
01279          SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGADLCC
01280       TO TRUE.                                                    ELGADLCC
01281      PERFORM 0630-CHK-CFT2-NOT-EXCLD                              ELGADLCC
01282         VARYING CFT2-IDX FROM 1 BY 1                              ELGADLCC
01283           UNTIL    (    SW-INTRNL-INST-PROV-CLASS                 ELGADLCC
01284                     AND SW-INTRNL-PROF-PROV-CLASS )               ELGADLCC
01285                 OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES.               ELGADLCC
01286                                                                   ELGADLCC
01287 /***********************************************************      ELGADLCC
01288 *                                                          *      ELGADLCC
01289 *    CHECK EXCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELGADLCC
01290 *                                                          *      ELGADLCC
01291 ************************************************************      ELGADLCC
01292                                                                   ELGADLCC
01293  0601-CHK-EXCLD-TYPE-IPGS.                                        ELGADLCC
01294                                                                   ELGADLCC
01295 * -- INITIALIZE CFT9 TABLE TO INCLUDE ALL PROVIDER SPEC           ELGADLCC
01296      PERFORM WITH TEST BEFORE                                     ELGADLCC
01297         VARYING CFT9-IDX FROM 1 BY 1                              ELGADLCC
01298           UNTIL CFT9-IDX > CFT9-NBR-TBL-ENTRIES                   ELGADLCC
01299         SET  CFT9-PT-INCLUDE (CFT9-IDX) TO TRUE                   ELGADLCC
01300         END-PERFORM.                                              ELGADLCC
01301                                                                   ELGADLCC
01302 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGS                 ELGADLCC
01303      SET  CFT9-IDX TO 1.                                          ELGADLCC
01304      PERFORM 0611-TAG-EXCLD-IPGS-ENTRIES                          ELGADLCC
01305         VARYING GXS-INDEX FROM 1 BY 1                             ELGADLCC
01306           UNTIL GXS-INDEX = WS-MAX-GXS-INDEX.                     ELGADLCC
01307                                                                   ELGADLCC
01308 * -- CHECK CFT9 TABLE FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED ELGADLCC
01309      SET SW-INTRNL-NOT-PROF-PROV-SPEC                             ELGADLCC
01310       TO TRUE.                                                    ELGADLCC
01311      PERFORM 0631-CHK-CFT9-NOT-EXCLD                              ELGADLCC
01312         VARYING CFT9-IDX FROM 1 BY 1                              ELGADLCC
01313           UNTIL         SW-INTRNL-PROF-PROV-SPEC                  ELGADLCC
01314                 OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES.               ELGADLCC
01315                                                                   ELGADLCC
01316 ************************************************************      ELGADLCC
01317 *                                                          *      ELGADLCC
01318 *    TAG EXCLUDED IPGT ENTRIES IN CFT2                     *      ELGADLCC
01319 *                                                          *      ELGADLCC
01320 ************************************************************      ELGADLCC
01321                                                                   ELGADLCC
01322  0610-TAG-EXCLD-IPGT-ENTRIES.                                     ELGADLCC
01323      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELGADLCC
01324      PERFORM WITH TEST BEFORE                                     ELGADLCC
01325         UNTIL    SW-ENTRY-FOUND                                   ELGADLCC
01326               OR   CFT2-PT (CFT2-IDX)                             ELGADLCC
01327                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELGADLCC
01328               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELGADLCC
01329         IF   CFT2-PT(CFT2-IDX)                                    ELGADLCC
01330            = GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELGADLCC
01331         THEN                                                      ELGADLCC
01332            SET SW-ENTRY-FOUND TO TRUE                             ELGADLCC
01333            SET CFT2-PT-EXCLUDE (CFT2-IDX) TO TRUE                 ELGADLCC
01334         END-IF                                                    ELGADLCC
01335         SET CFT2-IDX UP BY 1                                      ELGADLCC
01336         END-PERFORM.                                              ELGADLCC
01337                                                                   ELGADLCC
01338 ************************************************************      ELGADLCC
01339 *                                                          *      ELGADLCC
01340 *    TAG EXCLUDED IPGS ENTRIES IN CFT9                     *      ELGADLCC
01341 *                                                          *      ELGADLCC
01342 ************************************************************      ELGADLCC
01343                                                                   ELGADLCC
01344  0611-TAG-EXCLD-IPGS-ENTRIES.                                     ELGADLCC
01345      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELGADLCC
01346      PERFORM WITH TEST BEFORE                                     ELGADLCC
01347         UNTIL    SW-ENTRY-FOUND                                   ELGADLCC
01348               OR   CFT9-PT (CFT2-IDX)                             ELGADLCC
01349                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELGADLCC
01350               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELGADLCC
01351         IF   CFT9-PT(CFT9-IDX)                                    ELGADLCC
01352            = GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELGADLCC
01353            SET SW-ENTRY-FOUND TO TRUE                             ELGADLCC
01354            SET CFT9-PT-EXCLUDE (CFT9-IDX) TO TRUE                 ELGADLCC
01355         END-IF                                                    ELGADLCC
01356         SET CFT9-IDX UP BY 1                                      ELGADLCC
01357         END-PERFORM.                                              ELGADLCC
01358                                                                   ELGADLCC
01359 ******************************************************************ELGADLCC
01360 *                                                                *ELGADLCC
01361 *    CHECK CFT2 FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDED     *ELGADLCC
01362 *                                                                *ELGADLCC
01363 ******************************************************************ELGADLCC
01364                                                                   ELGADLCC
01365  0630-CHK-CFT2-NOT-EXCLD.                                         ELGADLCC
01366      IF CFT2-PT-INCLUDE (CFT2-IDX)                                ELGADLCC
01367      THEN                                                         ELGADLCC
01368         EVALUATE TRUE                                             ELGADLCC
01369            WHEN CFT2-PT-INST (CFT2-IDX)                           ELGADLCC
01370               SET SW-INTRNL-INST-PROV-CLASS TO TRUE               ELGADLCC
01371               IF SRP-ACCUM-PROV-CLASS-INST                        ELGADLCC
01372               THEN                                                ELGADLCC
01373                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGADLCC
01374               END-IF                                              ELGADLCC
01375            WHEN CFT2-PT-PROF (CFT2-IDX)                           ELGADLCC
01376               SET SW-INTRNL-PROF-PROV-CLASS TO TRUE               ELGADLCC
01377               IF SRP-ACCUM-PROV-CLASS-PROF                        ELGADLCC
01378               THEN                                                ELGADLCC
01379                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGADLCC
01380               END-IF                                              ELGADLCC
01381            END-EVALUATE                                           ELGADLCC
01382      END-IF.                                                      ELGADLCC
01383                                                                   ELGADLCC
01384 ******************************************************************ELGADLCC
01385 *                                                                *ELGADLCC
01386 *    CHECK CFT9 FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED     * ELGADLCC
01387 *                                                                *ELGADLCC
01388 ******************************************************************ELGADLCC
01389                                                                   ELGADLCC
01390  0631-CHK-CFT9-NOT-EXCLD.                                         ELGADLCC
01391      IF CFT9-PT-INCLUDE (CFT9-IDX)                                ELGADLCC
01392         EVALUATE TRUE                                             ELGADLCC
01393            WHEN CFT9-PT-PROF (CFT9-IDX)                           ELGADLCC
01394               SET SW-INTRNL-PROF-PROV-SPEC TO TRUE                ELGADLCC
01395               IF SRP-ACCUM-PROV-SPEC-PROF                         ELGADLCC
01396                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGADLCC
01397               END-IF                                              ELGADLCC
01398            END-EVALUATE                                           ELGADLCC
01399      END-IF.                                                      ELGADLCC
01400                                                                   ELGADLCC
01401 /*****************************************************************ELGADLCC
01402 *                                                                *ELGADLCC
01403 *    CHECK IBGR INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELGADLCC
01404 *                                                                *ELGADLCC
01405 ******************************************************************ELGADLCC
01406                                                                   ELGADLCC
01407  0640-CHK-IBGR-PROV-CLASS.                                        ELGADLCC
01408      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGADLCC
01409          SW-INTRNL-NOT-PROF-PROV-CLASS TO TRUE.                   ELGADLCC
01410      MOVE PC-IBGR TO KWA-PROVISION-ID.                            ELGADLCC
01411      MOVE WS-IBGR-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGADLCC
01412      PERFORM 0700-READ-INTRNL-TAB.                                ELGADLCC
01413      SET ADDRESS OF GX1-RECORD-AREA TO IOP-REC-PTR.               ELGADLCC
01414      SET IOP-REC-PTR TO NULLS.                                    ELGADLCC
01415      SET GX1-INDEX TO GX1-ENTRY-COUNT.                            ELGADLCC
01416      SET WS-MAX-GX1-INDEX TO GX1-INDEX.                           ELGADLCC
01417                                                                   ELGADLCC
01418      IF GX1-ID-ARGUMENT-EXCLUDED                                  ELGADLCC
01419      THEN                                                         ELGADLCC
01420 *    -- ASSUME THAT IBGR WOULD NOT EXCLUDE ALL OF ANY PROVIDER    ELGADLCC
01421 *       CLASS (I.E., BOTH TYPES APPLY).                           ELGADLCC
01422         SET SW-OCCRNC-APPLIES                                     ELGADLCC
01423             SW-INTRNL-INST-PROV-CLASS                             ELGADLCC
01424             SW-INTRNL-PROF-PROV-CLASS                             ELGADLCC
01425          TO TRUE                                                  ELGADLCC
01426      ELSE                                                         ELGADLCC
01427         PERFORM 0690-CHK-INCLD-TYPE-IBGR                          ELGADLCC
01428      END-IF.                                                      ELGADLCC
01429                                                                   ELGADLCC
01430 /*****************************************************************ELGADLCC
01431 *                                                                *ELGADLCC
01432 *    CHECK INCLUDE TYPE IBGR TO DETERMINE PROVIDER CLASS         *ELGADLCC
01433 *                                                                *ELGADLCC
01434 ******************************************************************ELGADLCC
01435                                                                   ELGADLCC
01436  0690-CHK-INCLD-TYPE-IBGR.                                        ELGADLCC
01437      PERFORM WITH TEST BEFORE                                     ELGADLCC
01438         VARYING GX1-INDEX FROM 1 BY 1                             ELGADLCC
01439           UNTIL    GX1-INDEX = WS-MAX-GX1-INDEX                   ELGADLCC
01440                 OR (    SW-INTRNL-INST-PROV-CLASS                 ELGADLCC
01441                     AND SW-INTRNL-PROF-PROV-CLASS )               ELGADLCC
01442         MOVE GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)                ELGADLCC
01443           TO WS-PROVISION-ARGUMENT                                ELGADLCC
01444         EVALUATE TRUE                                             ELGADLCC
01445            WHEN INST-CLASS                                        ELGADLCC
01446               SET SW-INTRNL-INST-PROV-CLASS TO TRUE               ELGADLCC
01447               IF SRP-ACCUM-PROV-CLASS-INST                        ELGADLCC
01448               THEN                                                ELGADLCC
01449                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGADLCC
01450               END-IF                                              ELGADLCC
01451            WHEN PROF-CLASS                                        ELGADLCC
01452               SET SW-INTRNL-PROF-PROV-CLASS TO TRUE               ELGADLCC
01453               IF SRP-ACCUM-PROV-CLASS-PROF                        ELGADLCC
01454               THEN                                                ELGADLCC
01455                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGADLCC
01456               END-IF                                              ELGADLCC
01457            END-EVALUATE                                           ELGADLCC
01458         END-PERFORM.                                              ELGADLCC
01459                                                                   ELGADLCC
01460 /***********************************************************      ELGADLCC
01461 *                                                          *      ELGADLCC
01462 *    READ THE INTERNAL TABULAR RECORD                      *      ELGADLCC
01463 *                                                          *      ELGADLCC
01464 ************************************************************      ELGADLCC
01465                                                                   ELGADLCC
01466  0700-READ-INTRNL-TAB.                                            ELGADLCC
01467      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELGADLCC
01468      SET IOP-RD TO TRUE.                                          ELGADLCC
01469      SET IOP-FCQ-NONE TO TRUE.                                    ELGADLCC
01470      SET IOP-KVQ-EQ TO TRUE.                                      ELGADLCC
01471      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELGADLCC
01472      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGADLCC
01473      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGADLCC
01474      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGADLCC
01475                                                                   ELGADLCC
01476      EVALUATE TRUE                                                ELGADLCC
01477         WHEN IOP-RC-OK                                            ELGADLCC
01478            CONTINUE                                               ELGADLCC
01479         WHEN IOP-RC-NOTFND                                        ELGADLCC
01480            SET CIA-AB-NOTFND-GCTABULR TO TRUE                     ELGADLCC
01481            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELGADLCC
01482         WHEN OTHER                                                ELGADLCC
01483             SET CIA-AB-CRITIO TO TRUE                             ELGADLCC
01484             EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC           ELGADLCC
01485         END-EVALUATE.                                             ELGADLCC
01486                                                                   ELGADLCC
01487 /***********************************************************      ELGADLCC
01488 *                                                          *      ELGADLCC
01489 *        ADD ACCUM OCCURENCE TO FILE                       *      ELGADLCC
01490 *                                                          *      ELGADLCC
01491 ************************************************************      ELGADLCC
01492                                                                   ELGADLCC
01493  0710-WRITE-EXTRACT-RECORD.                                       ELGADLCC
01494      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGADLCC
01495      SET  IOP-ADD TO TRUE.                                        ELGADLCC
01496      SET  IOP-FCQ-NONE TO TRUE.                                   ELGADLCC
01497      SET  IOP-KVQ-NONE TO TRUE.                                   ELGADLCC
01498      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGADLCC
01499                                                                   ELGADLCC
01500 /***********************************************************      ELGADLCC
01501 *                                                          *      ELGADLCC
01502 *    ESTABLISH ADDRESSABILITY OF THE TABULAR FILE          *      ELGADLCC
01503 *                                                          *      ELGADLCC
01504 ************************************************************      ELGADLCC
01505                                                                   ELGADLCC
01506  9060-EST-ADR-TABULAR-FILE.                                       ELGADLCC
01507      SET  CIA-GCTABULR-DDN TO TRUE.                               ELGADLCC
01508      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGADLCC
01509         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGADLCC
01510         END-CALL.                                                 ELGADLCC
01511      IF CIA-RC-PTR-NULL                                           ELGADLCC
01512      THEN                                                         ELGADLCC
01513         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGADLCC
01514         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGADLCC
01515      END-IF.                                                      ELGADLCC
01516                                                                   ELGADLCC
01517 /***********************************************************      ELGADLCC
01518 *                                                          *      ELGADLCC
01519 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELGADLCC
01520 *                                                          *      ELGADLCC
01521 ************************************************************      ELGADLCC
01522                                                                   ELGADLCC
01523  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELGADLCC
01524      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGADLCC
01525      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGADLCC
01526         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGADLCC
01527         END-CALL.                                                 ELGADLCC
01528      IF CIA-RC-PTR-NULL                                           ELGADLCC
01529      THEN                                                         ELGADLCC
01530         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGADLCC
01531         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGADLCC
01532      END-IF.                                                      ELGADLCC
