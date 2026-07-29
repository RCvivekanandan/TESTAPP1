00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGAOLCC
00003  PROGRAM-ID.        ELGAOLCC.                                        LV002
00004                                                                   ELGAOLCC
00005  AUTHOR.            LUCY TORRES.                                  ELGAOLCC
00006                     RICHARD J. LUKETICH (RE-WRITE).               ELGAOLCC
00007                                                                   ELGAOLCC
00008  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELGAOLCC
00009                     A MUTUAL LEGAL RESERVE COMPANY                ELGAOLCC
00010                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELGAOLCC
00011                     233 N. MICHIGAN AVE                           ELGAOLCC
00012                     CHICAGO, ILLINOIS 60601                       ELGAOLCC
00013                                                                   ELGAOLCC
00014  DATE-WRITTEN.      03-JUN-1987.                                  ELGAOLCC
00015                     03-JAN-1992 (RE-WRITE).                       ELGAOLCC
00016                                                                   ELGAOLCC
00017  DATE-COMPILED.                                                   ELGAOLCC
00018                                                                   ELGAOLCC
00019  SECURITY.          COPYRIGHT 1986, 1992,                         ELGAOLCC
00020                     HEALTH CARE SERVICE CORPORATION               ELGAOLCC
00021                                                                   ELGAOLCC
00022  ENVIRONMENT DIVISION.                                            ELGAOLCC
00023                                                                   ELGAOLCC
00024  CONFIGURATION SECTION.                                           ELGAOLCC
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELGAOLCC
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELGAOLCC
00027                                                                   ELGAOLCC
00028 /*****************************************************************ELGAOLCC
00029 *                                                                *ELGAOLCC
00030 *  ELGAOLCC - ELS:  SELECTS #AOL (OUT OF POCKET) ACCUMULATORS    *ELGAOLCC
00031 *                   AND SETS UP THE INFORMATION TO BE PROCESSED  *ELGAOLCC
00032 *                   BY THE OUT OF POCKET GENERATOR MODULE.  THE  *ELGAOLCC
00033 *                   ACCUMS ARE SELECTED FROM THE GROUP SPECIFIC  *ELGAOLCC
00034 *                   AND CONTRACT LEVEL PROCESSING.               *ELGAOLCC
00035 *                                                                *ELGAOLCC
00036 ******************************************************************ELGAOLCC
00037 *                                                                *ELGAOLCC
00038 *                      MAINTENANCE HISTORY                       *ELGAOLCC
00039 *                                                                *ELGAOLCC
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELGAOLCC
00041 * ----- ----------- --- ----- ---------------------------------- *ELGAOLCC
00042 * 01.00 03-JUN-1987 LET       CREATED                            *ELGAOLCC
00043 * 01.01 25-SEP-1987 LET       ADDED DEFINITION DATA FIELD        *ELGAOLCC
00044 *                                                                *ELGAOLCC
00045 * 01.02 17-NOV-1987 REB       MADE CHANGES TO CORRESPOND TO NEW  *ELGAOLCC
00046 *                             VERSION OF COPYBOOK ELSACUMC.      *ELGAOLCC
00047 *                                                                *ELGAOLCC
00048 * 01.07    SEP-1991 RKH    1. ADDED LOGIC FOR:                   *ELGAOLCC
00049 *    ISSR #12010                A.  NEW PATIENT AGE FIELDS       *ELGAOLCC
00050 *                               B.  RELATIONSHIP IND VALUE       *ELGAOLCC
00051 *                          2. REVISE LOGIC TO LOAD INT ACCUMS    *ELGAOLCC
00052 *                             INTO VARIABLE LEVEL TABLE          *ELGAOLCC
00053 *                          3. ADDED COPYBOOKS :                  *ELGAOLCC
00054 *                               A. GCTIBGR   - IBGR TAB          *ELGAOLCC
00055 *                               B. GCTIPGT   - IPGT TAB          *ELGAOLCC
00056 *                               C. ELSCFTB2  - PROVIDER TYPE     *ELGAOLCC
00057 *                                         COMPARE TABLE          *ELGAOLCC
00058 *                          4. ADD LOGIC TO INSPECT #IPGT AND     *ELGAOLCC
00059 *                             #IBGR INT TABS TO DETERMINE IF     *ELGAOLCC
00060 *                             AN OCCURRANCE IS THE SELECTED      *ELGAOLCC
00061 *                             PROVIDER CLASS.                    *ELGAOLCC
00062 *                                                                *ELGAOLCC
00063 * 02.00 03-JAN-1992 RJL       LOGIC RESTRUCTURED, ADDED          *ELGAOLCC
00064 *                             MAXIMUM BASE AMOUNT SOURCE IND.    *ELGAOLCC
00065 *                                                                *ELGAOLCC
00066 * 02.01 03-APR-1992 JPB       CLONED FROM ELGACLCC, CHANGED LOGIC*ELGAOLCC
00067 *                             FROM COINSURANCE TO OUT-OF-POCKET. *ELGAOLCC
00068 *                                                                *ELGAOLCC
00069 * 02.02 22-SEP-1994 AKK       ADDED RPO AND THEM FOUND THAT      *ELGAOLCC
00070 *                             THIS PROG ALLOWED NON-CCP OPX-     *ELGAOLCC
00071 *                             FIXED                              *ELGAOLCC
00072 *                                                                *ELGAOLCC
00073 * 02.03 24-FEB-1995 AKK       ADDED CPO.                         *ELGAOLCC
00074 *                                                                *ELGAOLCC
00075 * 02.04 16-FEB-1996 AKK       ADDED CBL AND PAN.                 *ELGAOLCC
00076 *                                                                *ELGAOLCC
00077 * 02.05 09-DEC-1998 AKK       ADDED ACP.                         *ELGAOLCC
00078 *                                                                *ELGAOLCC
00079 * 02.06 11-MAR-1999 AKK       ADDED BAE.                         *ELGAOLCC
00080 *                                                                *ELGAOLCC
00081 * 02.07 24-AUG-2000 AKK       ADD SUPPORT FOR #IPGT.             *ELGAOLCC
00082 *                                                                *ELGAOLCC
00083 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGAOLCC
00084 *                                                                *ELGAOLCC
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00085 ******************************************************************ELGAOLCC
00086      TITLE  'ELGAOLCC          WORKING STORAGE'.                  ELGAOLCC
00087  DATA DIVISION.                                                   ELGAOLCC
00088                                                                   ELGAOLCC
00089  WORKING-STORAGE SECTION.                                         ELGAOLCC
00090                                                                   ELGAOLCC
00091  01  SWITCHES.                                                    ELGAOLCC
00092                                                                   ELGAOLCC
00093      02 ACCUM-APPLICABILITY                  PICTURE  X(01).      ELGAOLCC
00094         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELGAOLCC
00095         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELGAOLCC
00096                                                                   ELGAOLCC
00097      02 COST-CONTAINMENT-APPLICABILITY       PICTURE  X(01).      ELGAOLCC
00098         88 SW-CC-IND-APPLIES                 VALUE 'Y'.           ELGAOLCC
00099         88 SW-CC-IND-DOES-NOT-APPLY          VALUE 'N'.           ELGAOLCC
00100                                                                   ELGAOLCC
00101      02 OCCURRENCE-APPLICABILITY             PICTURE  X(01).      ELGAOLCC
00102         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELGAOLCC
00103         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELGAOLCC
00104      02                                      PICTURE  X(01).      ELGAOLCC
00105         88 SW-HAS-IBGR                       VALUE 'Y'.           ELGAOLCC
00106         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELGAOLCC
00107      02                                      PICTURE  X(01).      ELGAOLCC
00108         88 SW-HAS-IDGD                       VALUE 'Y'.           ELGAOLCC
00109         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELGAOLCC
00110      02                                      PICTURE  X(01).      ELGAOLCC
00111         88 SW-HAS-IPGN                       VALUE 'Y'.           ELGAOLCC
00112         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELGAOLCC
00113      02                                      PICTURE  X(01).      ELGAOLCC
00114         88 SW-HAS-IPGP                       VALUE 'Y'.           ELGAOLCC
00115         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELGAOLCC
00116      02                                      PICTURE  X(01).      ELGAOLCC
00117         88 SW-HAS-IPGT                       VALUE 'Y'.           ELGAOLCC
00118         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELGAOLCC
00119      02                                      PICTURE  X(01).      ELGAOLCC
00120         88 SW-HAS-IPGS                       VALUE 'Y'.           ELGAOLCC
00121         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELGAOLCC
00122      02                                      PICTURE  X(01).      ELGAOLCC
00123         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELGAOLCC
00124         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELGAOLCC
00125      02                                      PICTURE  X(01).      ELGAOLCC
00126         88 SW-DUP-IBGR                       VALUE 'D'.           ELGAOLCC
00127         88 SW-UNQ-IBGR                       VALUE 'U'.           ELGAOLCC
00128      02                                      PICTURE  X(01).      ELGAOLCC
00129         88 SW-DUP-IDGD                       VALUE 'D'.           ELGAOLCC
00130         88 SW-UNQ-IDGD                       VALUE 'U'.           ELGAOLCC
00131      02                                      PICTURE  X(01).      ELGAOLCC
00132         88 SW-DUP-IPGN                       VALUE 'D'.           ELGAOLCC
00133         88 SW-UNQ-IPGN                       VALUE 'U'.           ELGAOLCC
00134      02                                      PICTURE  X(01).      ELGAOLCC
00135         88 SW-DUP-IPGP                       VALUE 'D'.           ELGAOLCC
00136         88 SW-UNQ-IPGP                       VALUE 'U'.           ELGAOLCC
00137      02                                      PICTURE  X(01).      ELGAOLCC
00138         88 SW-DUP-IPGT                       VALUE 'D'.           ELGAOLCC
00139         88 SW-UNQ-IPGT                       VALUE 'U'.           ELGAOLCC
00140      02                                      PICTURE  X(01).      ELGAOLCC
00141         88 SW-DUP-IPGS                       VALUE 'D'.           ELGAOLCC
00142         88 SW-UNQ-IPGS                       VALUE 'U'.           ELGAOLCC
00143      02                                      PICTURE  X(01).      ELGAOLCC
00144         88 SW-INTRNL-INST-PROV-CLASS         VALUE 'Y'.           ELGAOLCC
00145         88 SW-INTRNL-NOT-INST-PROV-CLASS     VALUE 'N'.           ELGAOLCC
00146         88 SW-INTRNL-INST-CLASS-NOT-DET      VALUE 'X'.           ELGAOLCC
00147      02                                      PICTURE  X(01).      ELGAOLCC
00148         88 SW-INTRNL-PROF-PROV-CLASS         VALUE 'Y'.           ELGAOLCC
00149         88 SW-INTRNL-NOT-PROF-PROV-CLASS     VALUE 'N'.           ELGAOLCC
00150         88 SW-INTRNL-PROF-CLASS-NOT-DET      VALUE 'X'.           ELGAOLCC
00151      02                                      PICTURE  X(01).      ELGAOLCC
00152         88 SW-INTRNL-PROF-PROV-SPEC          VALUE 'Y'.           ELGAOLCC
00153         88 SW-INTRNL-NOT-PROF-PROV-SPEC      VALUE 'N'.           ELGAOLCC
00154         88 SW-INTRNL-PROF-SPEC-NOT-DET       VALUE 'X'.           ELGAOLCC
00155      02                                      PICTURE  X(01).      ELGAOLCC
00156         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELGAOLCC
00157         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELGAOLCC
00158      02                                      PICTURE  X(01).      ELGAOLCC
00159         88 SW-MATCHING-ENTRY-FOUND           VALUE 'Y'.           ELGAOLCC
00160         88 SW-MATCHING-ENTRY-NOT-FOUND       VALUE 'N'.           ELGAOLCC
00161      02                                      PICTURE  X(01).      ELGAOLCC
00162         88 SW-SORT-COMPLETED                 VALUE 'Y'.           ELGAOLCC
00163         88 SW-SORT-NOT-COMPLETED             VALUE 'N'.           ELGAOLCC
00164                                                                   ELGAOLCC
00165  01  WS-PROVISION-ARGUMENT.                                       ELGAOLCC
00166      02                          PICTURE  X(05).                  ELGAOLCC
00167      02 WS-PROVISION-CLASS       PICTURE  X(01).                  ELGAOLCC
00168         88 INST-CLASS            VALUE 'A', 'B', 'W'.             ELGAOLCC
00169         88 PROF-CLASS            VALUE 'C', 'D', 'E'.             ELGAOLCC
00170                                                                   ELGAOLCC
00171  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELGAOLCC
00172      88 WS-LOB-INST              VALUE '1'.                       ELGAOLCC
00173      88 WS-LOB-PROF              VALUE '2'.                       ELGAOLCC
00174      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELGAOLCC
00175      88 WS-LOB-BOTH              VALUE '3', '4', '5', '6', '7'.   ELGAOLCC
00176                                                                   ELGAOLCC
00177  01  PROGRAM-CONSTANTS.                                           ELGAOLCC
00178      02 PC-AOL                   PICTURE  X(06) VALUE '#AOL  '.   ELGAOLCC
00179      02 PC-GCT-MAX-SUB           PICTURE S9(04) COMP.             ELGAOLCC
00180      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELGAOLCC
00181      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELGAOLCC
00182      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELGAOLCC
00183      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELGAOLCC
00184      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELGAOLCC
00185      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGT '.   ELGAOLCC
00186      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELGAOLCC
00187                                                                   ELGAOLCC
00188  01  WS-SUBTOPIC-TYPE            PIC  X(16) VALUE SPACES.         ELGAOLCC
00189      88  SUBTOPIC-ATCP                      VALUE 'ATCP'.         ELGAOLCC
00190      88  SUBTOPIC-BAE                       VALUE 'BAE'.          ELGAOLCC
00191      88  SUBTOPIC-CPO                       VALUE 'CPO'.          ELGAOLCC
00192      88  SUBTOPIC-CBL                       VALUE 'CBL'.          ELGAOLCC
00193      88  SUBTOPIC-HOSP                      VALUE 'HOSP'.         ELGAOLCC
00194      88  SUBTOPIC-PAT                       VALUE 'PAT'.          ELGAOLCC
00195      88  SUBTOPIC-MCN                       VALUE 'MCN'.          ELGAOLCC
00196      88  SUBTOPIC-MOPS                      VALUE 'MOPS'.         ELGAOLCC
00197      88  SUBTOPIC-MASOP                     VALUE 'MASOP'.        ELGAOLCC
00198      88  SUBTOPIC-WEEKEND                   VALUE 'WEEKEND'.      ELGAOLCC
00199      88  SUBTOPIC-MONDIS                    VALUE 'MONDIS'.       ELGAOLCC
00200      88  SUBTOPIC-IOB                       VALUE 'IOB'.          ELGAOLCC
00201      88  SUBTOPIC-PAR                       VALUE 'PAR'.          ELGAOLCC
00202      88  SUBTOPIC-MEDNEC                    VALUE 'MEDNEC'.       ELGAOLCC
00203      88  SUBTOPIC-MSA                       VALUE 'MSA'.          ELGAOLCC
00204      88  SUBTOPIC-PAN                       VALUE 'PAN'.          ELGAOLCC
00205      88  SUBTOPIC-PPO                       VALUE 'PPO'.          ELGAOLCC
00206      88  SUBTOPIC-EMH                       VALUE 'EMH'.          ELGAOLCC
00207      88  SUBTOPIC-POS                       VALUE 'POS'.          ELGAOLCC
00208      88  SUBTOPIC-MHSC                      VALUE 'MHSC'.         ELGAOLCC
00209      88  SUBTOPIC-RPO                       VALUE 'RPO'.          ELGAOLCC
00210                                                                   ELGAOLCC
00211  01  WS-COST-CONTAIN-IND         PIC  X(01) VALUE SPACES.         ELGAOLCC
00212      88  CC-ATCP                            VALUE  'A' 'H'.       ELGAOLCC
00213      88  CC-BAE                             VALUE  'N'.           ELGAOLCC
00214      88  CC-CPO                             VALUE  'K'.           ELGAOLCC
00215      88  CC-CBL                             VALUE  'J'.           ELGAOLCC
00216      88  CC-HOSP                            VALUE  'B' 'F' .      ELGAOLCC
00217      88  CC-PAT                             VALUE  'D'.           ELGAOLCC
00218      88  CC-MCN                             VALUE  'M'.           ELGAOLCC
00219      88  CC-MOPS                            VALUE  '1' 'F' 'C'.   ELGAOLCC
00220      88  CC-MASOP                           VALUES '2' 'E' 'G'.   ELGAOLCC
00221      88  CC-WEEKEND                         VALUE  '3'.           ELGAOLCC
00222      88  CC-MONDIS                          VALUE  '4'.           ELGAOLCC
00223      88  CC-IOB                             VALUE  '5'.           ELGAOLCC
00224      88  CC-PAR                             VALUE  '6' 'E' 'G'.   ELGAOLCC
00225      88  CC-PAN                             VALUE  'L'.           ELGAOLCC
00226      88  CC-MEDNEC                          VALUE  '7'.           ELGAOLCC
00227      88  CC-MSA                             VALUE  '8'.           ELGAOLCC
00228      88  CC-PPO                             VALUE  '9' 'G'.       ELGAOLCC
00229      88  CC-EMH                             VALUE  'I'.           ELGAOLCC
00230      88  CC-POS                             VALUE  'P'.           ELGAOLCC
00231      88  CC-MHSC                            VALUE  'S'.           ELGAOLCC
00232      88  CC-RPO                             VALUE  'R'.           ELGAOLCC
00233                                                                   ELGAOLCC
00234  01  WS-WORK-FIELDS.                                              ELGAOLCC
00235      02 WS-AOL-SUB               PICTURE S9(04) COMP.             ELGAOLCC
00236      02 WS-GAD-SUB               PICTURE S9(04) COMP.             ELGAOLCC
00237      02 WS-GAD-INT-SUB           PICTURE S9(04) COMP.             ELGAOLCC
00238      02 WS-AOL-ACCUM-CNT         PICTURE S9(04) COMP.             ELGAOLCC
00239      02 WS-INTERNAL-COUNTER      PICTURE S9(04) COMP.             ELGAOLCC
00240      02 WS-OCCURRENCE-SUB        PICTURE S9(04) COMP.             ELGAOLCC
00241      02 WS-SAVE-SUB              PICTURE S9(04) COMP.             ELGAOLCC
00242      02 SORT-SUB                 PICTURE S9(04) COMP.             ELGAOLCC
00243      02 TEST-SUB                 PICTURE S9(04) COMP.             ELGAOLCC
00244      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELGAOLCC
00245      02 WS-SAVE-INDEX            USAGE IS INDEX.                  ELGAOLCC
00246                                                                   ELGAOLCC
00247  01 WS-MAX-INDEX-VALUES.                                          ELGAOLCC
00248      02 WS-MAX-GAD-INDEX         USAGE IS INDEX.                  ELGAOLCC
00249      02 WS-MAX-GAD-INT-INDEX     USAGE IS INDEX.                  ELGAOLCC
00250      02 WS-MAX-GCT-INDEX         USAGE IS INDEX.                  ELGAOLCC
00251      02 WS-MAX-GCG-INDEX         USAGE IS INDEX.                  ELGAOLCC
00252      02 WS-MAX-GX1-INDEX         USAGE IS INDEX.                  ELGAOLCC
00253      02 WS-MAX-GX3-INDEX         USAGE IS INDEX.                  ELGAOLCC
00254      02 WS-MAX-GXS-INDEX         USAGE IS INDEX.                  ELGAOLCC
00255                                                                   ELGAOLCC
00256  01  WS-POINTERS.                                                 ELGAOLCC
00257      02  WS-INST-CNTRCT-PTR      POINTER.                         ELGAOLCC
00258      02  WS-PROF-CNTRCT-PTR      POINTER.                         ELGAOLCC
00259                                                                   ELGAOLCC
00260  01  ACCUM-HOLD-TBL.                                              ELGAOLCC
00261      02  ACCUM-SLOT-NBR          PICTURE S9(07) COMP-3            ELGAOLCC
00262                                  OCCURS 5 TIMES.                  ELGAOLCC
00263                                                                   ELGAOLCC
00264  01  WS-OCCURRENCE-PROCESSED-TBL.                                 ELGAOLCC
00265      02                          PICTURE X                        ELGAOLCC
00266                                  OCCURS 44 TIMES                  ELGAOLCC
00267                                  INDEXED BY WS-OCCURRENCE-INDEX.  ELGAOLCC
00268          88  WS-OCCURRENCE-PROCESSED           VALUE 'P'.         ELGAOLCC
00269          88  WS-OCCURRENCE-NOT-PROCESSED       VALUE ' '.         ELGAOLCC
00270                                                                   ELGAOLCC
00271  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELGAOLCC
00272      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGAOLCC
00273      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGAOLCC
00274      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGAOLCC
00275      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGAOLCC
00276      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGAOLCC
00277      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGAOLCC
00278                                                                   ELGAOLCC
00279  01  WS-ASCEND-DESCEND-ENTRY-HOLD PICTURE X(28).                  ELGAOLCC
00280 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELGAOLCC
00281      COPY ELSCFTB2.                                               ELGAOLCC
00282                                                                   ELGAOLCC
00283 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELGAOLCC
00284      COPY ELSCFTB9.                                               ELGAOLCC
00285                                                                   ELGAOLCC
00286      TITLE  'ELGAOLCC          LINKAGE SECTION'                   ELGAOLCC
00287  LINKAGE SECTION.                                                 ELGAOLCC
00288  01  DFHCOMMAREA.                                                 ELGAOLCC
00289      COPY ELSCOMMC.                                               ELGAOLCC
00290 /                                                                 ELGAOLCC
00291      COPY ELSCIA2C.                                               ELGAOLCC
00292 /                                                                 ELGAOLCC
00293      COPY ELSIOPMC.                                               ELGAOLCC
00294 /                                                                 ELGAOLCC
00295      COPY ELSKEYSC.                                               ELGAOLCC
00296 /                                                                 ELGAOLCC
00297      COPY ELSSRTPC.                                               ELGAOLCC
00298 /                                                                 ELGAOLCC
00299      COPY ELSSSCBC.                                               ELGAOLCC
00300 /                                                                 ELGAOLCC
00301  01  GCG-GRP-SPEC-RECORD-AREA.                                    ELGAOLCC
00302      COPY GCGROUPC.                                               ELGAOLCC
00303 /                                                                 ELGAOLCC
00304  01  GCT-CONTRACT-RECORD-AREA.                                    ELGAOLCC
00305      COPY GCCONTRC.                                               ELGAOLCC
00306 /                                                                 ELGAOLCC
00307  01  GAD-RECORD-AREA.                                             ELGAOLCC
00308      COPY GCTAOLC.                                                ELGAOLCC
00309 /                                                                 ELGAOLCC
00310      COPY ELSACUMC.                                               ELGAOLCC
00311 /                                                                 ELGAOLCC
00312  01  GX1-RECORD-AREA.                                             ELGAOLCC
00313      COPY GCTIBGRC.                                               ELGAOLCC
00314 /                                                                 ELGAOLCC
00315  01  GX3-RECORD-AREA.                                             ELGAOLCC
00316      COPY GCTIPGTC.                                               ELGAOLCC
00317 /                                                                 ELGAOLCC
00318  01  GXS-RECORD-AREA.                                             ELGAOLCC
00319      COPY GCTIPGSC.                                               ELGAOLCC
00320      TITLE  'ELGAOLCC          PROCEDURE DIVISION'.               ELGAOLCC
00321 ************************************************************      ELGAOLCC
00322 *                                                          *      ELGAOLCC
00323 *    ELGAOLCC MAINLINE                                     *      ELGAOLCC
00324 *                                                          *      ELGAOLCC
00325 ************************************************************      ELGAOLCC
00326                                                                   ELGAOLCC
00327  PROCEDURE DIVISION.                                              ELGAOLCC
00328                                                                   ELGAOLCC
00329      PERFORM 0010-INITIALIZATION.                                 ELGAOLCC
00330      PERFORM 0100-PROCESS.                                        ELGAOLCC
00331      GOBACK.                                                      ELGAOLCC
00332                                                                   ELGAOLCC
00333 ************************************************************      ELGAOLCC
00334 *                                                          *      ELGAOLCC
00335 *    INITIALIZATION                                        *      ELGAOLCC
00336 *                                                          *      ELGAOLCC
00337 ************************************************************      ELGAOLCC
00338                                                                   ELGAOLCC
00339  0010-INITIALIZATION.                                             ELGAOLCC
00340      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELGAOLCC
00341      PERFORM 0060-EST-ADR-KEY-WK-AREA.                            ELGAOLCC
00342      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELGAOLCC
00343      PERFORM 0120-EST-ADR-GRP-SPC.                                ELGAOLCC
00344      PERFORM 0190-INIT-DATA.                                      ELGAOLCC
00345                                                                   ELGAOLCC
00346 /***********************************************************      ELGAOLCC
00347 *                                                          *      ELGAOLCC
00348 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGAOLCC
00349 *                                                          *      ELGAOLCC
00350 ************************************************************      ELGAOLCC
00351                                                                   ELGAOLCC
00352  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELGAOLCC
00353      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGAOLCC
00354      THEN                                                         ELGAOLCC
00355         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGAOLCC
00356      ELSE                                                         ELGAOLCC
00357         IF ECA-CIA-PTR = NULL                                     ELGAOLCC
00358         THEN                                                      ELGAOLCC
00359            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGAOLCC
00360         ELSE                                                      ELGAOLCC
00361            CALL 'ELUINISM' USING DFHCOMMAREA                      ELGAOLCC
00362               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELGAOLCC
00363               END-CALL                                            ELGAOLCC
00364            SET CIA-ELSSSCB-DDN TO TRUE                            ELGAOLCC
00365            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELGAOLCC
00366               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELGAOLCC
00367               END-CALL                                            ELGAOLCC
00368            IF CIA-RC-PTR-NULL                                     ELGAOLCC
00369            THEN                                                   ELGAOLCC
00370               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELGAOLCC
00371               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGAOLCC
00372            ELSE                                                   ELGAOLCC
00373               CONTINUE                                            ELGAOLCC
00374            END-IF                                                 ELGAOLCC
00375         END-IF                                                    ELGAOLCC
00376      END-IF.                                                      ELGAOLCC
00377                                                                   ELGAOLCC
00378 /***********************************************************      ELGAOLCC
00379 *                                                          *      ELGAOLCC
00380 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELGAOLCC
00381 *                                                          *      ELGAOLCC
00382 ************************************************************      ELGAOLCC
00383                                                                   ELGAOLCC
00384  0060-EST-ADR-KEY-WK-AREA.                                        ELGAOLCC
00385      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGAOLCC
00386      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOLCC
00387         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELGAOLCC
00388         END-CALL.                                                 ELGAOLCC
00389      IF CIA-RC-PTR-NULL                                           ELGAOLCC
00390         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGAOLCC
00391         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGAOLCC
00392      END-IF.                                                      ELGAOLCC
00393                                                                   ELGAOLCC
00394 ************************************************************      ELGAOLCC
00395 *                                                          *      ELGAOLCC
00396 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGAOLCC
00397 *                                                          *      ELGAOLCC
00398 ************************************************************      ELGAOLCC
00399                                                                   ELGAOLCC
00400  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGAOLCC
00401      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGAOLCC
00402      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOLCC
00403         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELGAOLCC
00404         END-CALL.                                                 ELGAOLCC
00405      IF CIA-RC-PTR-NULL                                           ELGAOLCC
00406         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGAOLCC
00407         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGAOLCC
00408      END-IF.                                                      ELGAOLCC
00409                                                                   ELGAOLCC
00410 /***********************************************************      ELGAOLCC
00411 *                                                          *      ELGAOLCC
00412 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD     *      ELGAOLCC
00413 *                                                          *      ELGAOLCC
00414 ************************************************************      ELGAOLCC
00415                                                                   ELGAOLCC
00416  0120-EST-ADR-GRP-SPC.                                            ELGAOLCC
00417      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELGAOLCC
00418      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOLCC
00419         ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                       ELGAOLCC
00420         END-CALL.                                                 ELGAOLCC
00421      IF CIA-RC-PTR-NULL                                           ELGAOLCC
00422         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGAOLCC
00423         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGAOLCC
00424      END-IF.                                                      ELGAOLCC
00425                                                                   ELGAOLCC
00426 /***********************************************************      ELGAOLCC
00427 *                                                          *      ELGAOLCC
00428 *    INITIALIZE DATA AREAS                                 *      ELGAOLCC
00429 *                                                          *      ELGAOLCC
00430 ************************************************************      ELGAOLCC
00431                                                                   ELGAOLCC
00432  0190-INIT-DATA.                                                  ELGAOLCC
00433      COMPUTE PC-GCT-MAX-SUB =   LENGTH OF GCT-CONT-TAB-PTRS       ELGAOLCC
00434                               / LENGTH OF GCT-CON-TAB-ID-SLOT.    ELGAOLCC
00435      SET GCT-INDEX                TO PC-GCT-MAX-SUB.              ELGAOLCC
00436      SET WS-MAX-GCT-INDEX         TO GCT-INDEX.                   ELGAOLCC
00437      SET GCG-INDEX                TO GCG-COUNT-TAB-PROVN-POINTERS.ELGAOLCC
00438      SET WS-MAX-GCG-INDEX         TO GCG-INDEX.                   ELGAOLCC
00439      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGAOLCC
00440      INITIALIZE WS-AOL-ACCUM-CNT.                                 ELGAOLCC
00441                                                                   ELGAOLCC
00442 /***********************************************************      ELGAOLCC
00443 *                                                          *      ELGAOLCC
00444 *        PROCESS                                           *      ELGAOLCC
00445 *                                                          *      ELGAOLCC
00446 ************************************************************      ELGAOLCC
00447                                                                   ELGAOLCC
00448  0100-PROCESS.                                                    ELGAOLCC
00449      PERFORM 0110-SCAN-GRP-SPC-FOR-ACCUMS.                        ELGAOLCC
00450      PERFORM 0120-SCAN-CONTRACTS-FOR-ACCUMS.                      ELGAOLCC
00451                                                                   ELGAOLCC
00452      IF WS-AOL-ACCUM-CNT >  0                                     ELGAOLCC
00453      THEN                                                         ELGAOLCC
00454          PERFORM 0210-SCAN-FOR-APPLIC-OCCRNCS                     ELGAOLCC
00455      END-IF.                                                      ELGAOLCC
00456                                                                   ELGAOLCC
00457                                                                   ELGAOLCC
00458 *    -- LINK TO THE OUTPUT GENERATOR                              ELGAOLCC
00459      IF SW-APPLIC-ACCUM-FOUND                                     ELGAOLCC
00460         SET SRP-COST-CONT-ACCUM TO TRUE                           ELGAOLCC
00461         EXEC CICS LINK PROGRAM  ('ELGAOL')                        ELGAOLCC
00462                        COMMAREA (DFHCOMMAREA)                     ELGAOLCC
00463         END-EXEC                                                  ELGAOLCC
00464      END-IF.                                                      ELGAOLCC
00465                                                                   ELGAOLCC
00466 /***********************************************************      ELGAOLCC
00467 *                                                          *      ELGAOLCC
00468 *    SCAN GROUP SPECIFIC RECORD FOR ACCUMULATORS           *      ELGAOLCC
00469 *                                                          *      ELGAOLCC
00470 ************************************************************      ELGAOLCC
00471                                                                   ELGAOLCC
00472  0110-SCAN-GRP-SPC-FOR-ACCUMS.                                    ELGAOLCC
00473      PERFORM WITH TEST BEFORE                                     ELGAOLCC
00474         VARYING GCG-INDEX FROM 1 BY 1                             ELGAOLCC
00475           UNTIL GCG-INDEX > WS-MAX-GCG-INDEX                      ELGAOLCC
00476                 OR GCG-TAB-ID (GCG-INDEX) > PC-AOL                ELGAOLCC
00477 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELGAOLCC
00478         IF     GCG-TAB-ID (GCG-INDEX)  =  PC-AOL                  ELGAOLCC
00479            AND GCG-TAB-SLOT-NO (GCG-INDEX)  >  ZERO               ELGAOLCC
00480         THEN                                                      ELGAOLCC
00481 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELGAOLCC
00482            MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO WS-SLOT-NBR        ELGAOLCC
00483            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELGAOLCC
00484         END-IF                                                    ELGAOLCC
00485      END-PERFORM.                                                 ELGAOLCC
00486                                                                   ELGAOLCC
00487 /***********************************************************      ELGAOLCC
00488 *                                                          *      ELGAOLCC
00489 *    SCAN CONTRACT RECORDS FOR ACCUMULATORS                *      ELGAOLCC
00490 *                                                          *      ELGAOLCC
00491 ************************************************************      ELGAOLCC
00492                                                                   ELGAOLCC
00493  0120-SCAN-CONTRACTS-FOR-ACCUMS.                                  ELGAOLCC
00494                                                                   ELGAOLCC
00495      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELGAOLCC
00496      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELGAOLCC
00497                                                                   ELGAOLCC
00498      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELGAOLCC
00499      THEN                                                         ELGAOLCC
00500          PERFORM 0130-SCAN-INST-BAS                               ELGAOLCC
00501      END-IF.                                                      ELGAOLCC
00502                                                                   ELGAOLCC
00503      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELGAOLCC
00504      THEN                                                         ELGAOLCC
00505          PERFORM 0140-SCAN-PROF-BAS                               ELGAOLCC
00506      END-IF.                                                      ELGAOLCC
00507                                                                   ELGAOLCC
00508      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELGAOLCC
00509      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELGAOLCC
00510                                                                   ELGAOLCC
00511      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELGAOLCC
00512      THEN                                                         ELGAOLCC
00513          PERFORM 0150-SCAN-INST-SUP                               ELGAOLCC
00514      END-IF.                                                      ELGAOLCC
00515                                                                   ELGAOLCC
00516      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELGAOLCC
00517      THEN                                                         ELGAOLCC
00518          PERFORM 0160-SCAN-PROF-SUP                               ELGAOLCC
00519      END-IF.                                                      ELGAOLCC
00520                                                                   ELGAOLCC
00521 /***********************************************************      ELGAOLCC
00522 *                                                          *      ELGAOLCC
00523 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELGAOLCC
00524 *                                                          *      ELGAOLCC
00525 ************************************************************      ELGAOLCC
00526                                                                   ELGAOLCC
00527  0130-SCAN-INST-BAS.                                              ELGAOLCC
00528      SET CIA-ELSCONIB-DDN TO TRUE.                                ELGAOLCC
00529      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOLCC
00530         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGAOLCC
00531         END-CALL.                                                 ELGAOLCC
00532      SET WS-INST-CNTRCT-PTR                                       ELGAOLCC
00533       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELGAOLCC
00534                                                                   ELGAOLCC
00535      IF CIA-RC-PTR-NULL                                           ELGAOLCC
00536      THEN                                                         ELGAOLCC
00537         CONTINUE                                                  ELGAOLCC
00538      ELSE                                                         ELGAOLCC
00539         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELGAOLCC
00540      END-IF.                                                      ELGAOLCC
00541                                                                   ELGAOLCC
00542 ************************************************************      ELGAOLCC
00543 *                                                          *      ELGAOLCC
00544 *    SCAN PROFESSIONAL BASIC CONTRACT RECORD               *      ELGAOLCC
00545 *                                                          *      ELGAOLCC
00546 ************************************************************      ELGAOLCC
00547                                                                   ELGAOLCC
00548  0140-SCAN-PROF-BAS.                                              ELGAOLCC
00549      SET CIA-ELSCONPB-DDN TO TRUE.                                ELGAOLCC
00550      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOLCC
00551         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGAOLCC
00552         END-CALL.                                                 ELGAOLCC
00553      SET WS-PROF-CNTRCT-PTR                                       ELGAOLCC
00554       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELGAOLCC
00555                                                                   ELGAOLCC
00556      IF    CIA-RC-PTR-NULL                                        ELGAOLCC
00557         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELGAOLCC
00558      THEN                                                         ELGAOLCC
00559         CONTINUE                                                  ELGAOLCC
00560      ELSE                                                         ELGAOLCC
00561         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELGAOLCC
00562      END-IF.                                                      ELGAOLCC
00563                                                                   ELGAOLCC
00564 /***********************************************************      ELGAOLCC
00565 *                                                          *      ELGAOLCC
00566 *    SCAN INSTITUTIONAL SUPPLEMENTAL CONTRACT RECORD       *      ELGAOLCC
00567 *                                                          *      ELGAOLCC
00568 ************************************************************      ELGAOLCC
00569                                                                   ELGAOLCC
00570  0150-SCAN-INST-SUP.                                              ELGAOLCC
00571      SET CIA-ELSCONIS-DDN TO TRUE.                                ELGAOLCC
00572      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOLCC
00573         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGAOLCC
00574         END-CALL.                                                 ELGAOLCC
00575      SET WS-INST-CNTRCT-PTR                                       ELGAOLCC
00576       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELGAOLCC
00577                                                                   ELGAOLCC
00578      IF CIA-RC-PTR-NULL                                           ELGAOLCC
00579      THEN                                                         ELGAOLCC
00580         CONTINUE                                                  ELGAOLCC
00581      ELSE                                                         ELGAOLCC
00582         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELGAOLCC
00583      END-IF.                                                      ELGAOLCC
00584                                                                   ELGAOLCC
00585 ************************************************************      ELGAOLCC
00586 *                                                          *      ELGAOLCC
00587 *    SCAN PROFESSIONAL SUPPLEMENTAL CONTRACT RECORD        *      ELGAOLCC
00588 *                                                          *      ELGAOLCC
00589 ************************************************************      ELGAOLCC
00590                                                                   ELGAOLCC
00591  0160-SCAN-PROF-SUP.                                              ELGAOLCC
00592      SET CIA-ELSCONPS-DDN TO TRUE.                                ELGAOLCC
00593      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOLCC
00594         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGAOLCC
00595         END-CALL.                                                 ELGAOLCC
00596      SET WS-PROF-CNTRCT-PTR                                       ELGAOLCC
00597       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELGAOLCC
00598                                                                   ELGAOLCC
00599      IF    CIA-RC-PTR-NULL                                        ELGAOLCC
00600         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELGAOLCC
00601      THEN                                                         ELGAOLCC
00602         CONTINUE                                                  ELGAOLCC
00603      ELSE                                                         ELGAOLCC
00604         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELGAOLCC
00605      END-IF.                                                      ELGAOLCC
00606                                                                   ELGAOLCC
00607 /***********************************************************      ELGAOLCC
00608 *                                                          *      ELGAOLCC
00609 *    SCAN A CONTRACT RECORD FOR ACCUMULATORS               *      ELGAOLCC
00610 *                                                          *      ELGAOLCC
00611 ************************************************************      ELGAOLCC
00612                                                                   ELGAOLCC
00613  0170-SCAN-CONTRACT-FOR-ACCUMS.                                   ELGAOLCC
00614      PERFORM WITH TEST BEFORE                                     ELGAOLCC
00615         VARYING GCT-TAB-INDEX FROM 1 BY 1                         ELGAOLCC
00616           UNTIL    GCT-TAB-INDEX > WS-MAX-GCT-INDEX               ELGAOLCC
00617                 OR GCT-CON-TAB-ID (GCT-TAB-INDEX) > PC-AOL        ELGAOLCC
00618 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELGAOLCC
00619         IF     GCT-CON-TAB-ID (GCT-TAB-INDEX) = PC-AOL            ELGAOLCC
00620            AND GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  > ZEROS          ELGAOLCC
00621         THEN                                                      ELGAOLCC
00622 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELGAOLCC
00623            MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  TO  WS-SLOT-NBR ELGAOLCC
00624            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELGAOLCC
00625         END-IF                                                    ELGAOLCC
00626      END-PERFORM.                                                 ELGAOLCC
00627                                                                   ELGAOLCC
00628 /***********************************************************      ELGAOLCC
00629 *                                                          *      ELGAOLCC
00630 *    SAVE UNIQUE ACCUMULATOR SLOT NUMBER                   *      ELGAOLCC
00631 *                                                          *      ELGAOLCC
00632 ************************************************************      ELGAOLCC
00633                                                                   ELGAOLCC
00634  0200-SAVE-UNQ-ACCUM-SLOT-NBR.                                    ELGAOLCC
00635                                                                   ELGAOLCC
00636 * -- SCAN TABLE OF ACCUM SLOT NUMBERS FOR DUPLICATE               ELGAOLCC
00637      SET SW-UNQ-SLOT-NBR TO TRUE.                                 ELGAOLCC
00638      PERFORM WITH TEST BEFORE                                     ELGAOLCC
00639         VARYING WS-AOL-SUB FROM 1 BY 1                            ELGAOLCC
00640           UNTIL    WS-AOL-SUB > WS-AOL-ACCUM-CNT                  ELGAOLCC
00641                 OR SW-DUP-SLOT-NBR                                ELGAOLCC
00642         IF WS-SLOT-NBR = ACCUM-SLOT-NBR (WS-AOL-SUB)              ELGAOLCC
00643         THEN                                                      ELGAOLCC
00644            SET SW-DUP-SLOT-NBR TO TRUE                            ELGAOLCC
00645         END-IF                                                    ELGAOLCC
00646      END-PERFORM.                                                 ELGAOLCC
00647                                                                   ELGAOLCC
00648 * -- IF SLOT NUMBER IS UNIQUE, ADD IT TO THE HOLD TABLE           ELGAOLCC
00649      IF SW-UNQ-SLOT-NBR                                           ELGAOLCC
00650      THEN                                                         ELGAOLCC
00651         ADD 1 TO  WS-AOL-ACCUM-CNT                                ELGAOLCC
00652         MOVE WS-SLOT-NBR TO ACCUM-SLOT-NBR(WS-AOL-ACCUM-CNT)      ELGAOLCC
00653      END-IF.                                                      ELGAOLCC
00654                                                                   ELGAOLCC
00655 /***********************************************************      ELGAOLCC
00656 *                                                          *      ELGAOLCC
00657 *    SCAN AOL ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELGAOLCC
00658 *                                                          *      ELGAOLCC
00659 ************************************************************      ELGAOLCC
00660                                                                   ELGAOLCC
00661  0210-SCAN-FOR-APPLIC-OCCRNCS.                                    ELGAOLCC
00662      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGAOLCC
00663      PERFORM 0220-DELETE-AOL-SUMMARY-FILE.                        ELGAOLCC
00664      PERFORM 0230-ALLOC-WORKFILE-REC-AREA.                        ELGAOLCC
00665                                                                   ELGAOLCC
00666 * -- READ AND SCAN EACH ACCUMULATOR TABULAR                       ELGAOLCC
00667      PERFORM WITH TEST BEFORE                                     ELGAOLCC
00668         VARYING WS-AOL-SUB FROM 1 BY 1                            ELGAOLCC
00669           UNTIL WS-AOL-SUB > WS-AOL-ACCUM-CNT                     ELGAOLCC
00670 *    -- OBTAIN ACCUMULATOR TABULAR RECORD                         ELGAOLCC
00671         MOVE PC-AOL TO KWA-PROVISION-ID                           ELGAOLCC
00672         MOVE ACCUM-SLOT-NBR (WS-AOL-SUB) TO KWA-PROVISION-SLOT-NO ELGAOLCC
00673         MOVE SPACES TO WS-OCCURRENCE-PROCESSED-TBL                ELGAOLCC
00674         PERFORM 0240-READ-TABULAR-REC                             ELGAOLCC
00675 *    -- SCAN ACCUMULATOR TABULAR                                  ELGAOLCC
00676         PERFORM WITH TEST BEFORE                                  ELGAOLCC
00677            VARYING WS-OCCURRENCE-SUB FROM 1 BY 1                  ELGAOLCC
00678            UNTIL WS-OCCURRENCE-SUB >= GAD-ENTRY-COUNT             ELGAOLCC
00679            IF WS-OCCURRENCE-NOT-PROCESSED (WS-OCCURRENCE-SUB)     ELGAOLCC
00680              THEN                                                 ELGAOLCC
00681              SET GAD-INDEX           TO WS-OCCURRENCE-SUB         ELGAOLCC
00682              SET WS-OCCURRENCE-INDEX TO WS-OCCURRENCE-SUB         ELGAOLCC
00683              PERFORM 0300-TEST-AOL-OCCURRENCE                     ELGAOLCC
00684            END-IF                                                 ELGAOLCC
00685         END-PERFORM                                               ELGAOLCC
00686      END-PERFORM.                                                 ELGAOLCC
00687                                                                   ELGAOLCC
00688 /***********************************************************      ELGAOLCC
00689 *                                                          *      ELGAOLCC
00690 *        DELETE AOL SUMMARY FILE                           *      ELGAOLCC
00691 *                                                          *      ELGAOLCC
00692 ************************************************************      ELGAOLCC
00693                                                                   ELGAOLCC
00694  0220-DELETE-AOL-SUMMARY-FILE.                                    ELGAOLCC
00695      SET IOP-DEL TO TRUE.                                         ELGAOLCC
00696      SET IOP-FCQ-NONE TO TRUE.                                    ELGAOLCC
00697      SET IOP-KVQ-NONE TO TRUE.                                    ELGAOLCC
00698      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGAOLCC
00699                                                                   ELGAOLCC
00700 /***********************************************************      ELGAOLCC
00701 *                                                          *      ELGAOLCC
00702 *    ALLOCATE WORKFILE RECORD AREA                         *      ELGAOLCC
00703 *                                                          *      ELGAOLCC
00704 ************************************************************      ELGAOLCC
00705                                                                   ELGAOLCC
00706  0230-ALLOC-WORKFILE-REC-AREA.                                    ELGAOLCC
00707      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGAOLCC
00708      SET CIA-STG-GETMAIN TO TRUE.                                 ELGAOLCC
00709      SET IOP-GETMAIN-REC TO TRUE.                                 ELGAOLCC
00710      COMPUTE IOP-MAX-REC-LEN =                                    ELGAOLCC
00711              LENGTH OF ACCUM-FIXED-AREA                           ELGAOLCC
00712 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELGAOLCC
00713            + LENGTH OF ACCUM-VARIABLE-AREA                        ELGAOLCC
00714            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELGAOLCC
00715 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELGAOLCC
00716 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELGAOLCC
00717                                                                   ELGAOLCC
00718      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGAOLCC
00719      IF IOP-REC-PTR = NULLS                                       ELGAOLCC
00720      THEN                                                         ELGAOLCC
00721         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGAOLCC
00722         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGAOLCC
00723      ELSE                                                         ELGAOLCC
00724         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELGAOLCC
00725      END-IF.                                                      ELGAOLCC
00726                                                                   ELGAOLCC
00727 /***********************************************************      ELGAOLCC
00728 *                                                          *      ELGAOLCC
00729 *    READ TABULAR RECORD                                   *      ELGAOLCC
00730 *                                                          *      ELGAOLCC
00731 ************************************************************      ELGAOLCC
00732                                                                   ELGAOLCC
00733  0240-READ-TABULAR-REC.                                           ELGAOLCC
00734      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELGAOLCC
00735      SET IOP-RD TO TRUE.                                          ELGAOLCC
00736      SET IOP-FCQ-NONE TO TRUE.                                    ELGAOLCC
00737      SET IOP-KVQ-EQ TO TRUE.                                      ELGAOLCC
00738      SET IOP-STG-MODE-MOVE TO TRUE.                               ELGAOLCC
00739      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGAOLCC
00740      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGAOLCC
00741      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGAOLCC
00742                                                                   ELGAOLCC
00743      EVALUATE TRUE                                                ELGAOLCC
00744        WHEN IOP-RC-OK                                             ELGAOLCC
00745           SET ADDRESS OF GAD-RECORD-AREA TO IOP-REC-PTR           ELGAOLCC
00746           SET IOP-REC-PTR                TO NULLS                 ELGAOLCC
00747           SET GAD-INDEX                  TO GAD-ENTRY-COUNT       ELGAOLCC
00748           SET WS-MAX-GAD-INDEX           TO GAD-INDEX             ELGAOLCC
00749        WHEN IOP-RC-NOTFND                                         ELGAOLCC
00750           SET CIA-AB-NOTFND-GCTABULR TO TRUE                      ELGAOLCC
00751           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELGAOLCC
00752        WHEN OTHER                                                 ELGAOLCC
00753           SET CIA-AB-CRITIO TO TRUE                               ELGAOLCC
00754           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELGAOLCC
00755      END-EVALUATE.                                                ELGAOLCC
00756                                                                   ELGAOLCC
00757 /***********************************************************      ELGAOLCC
00758 *                                                          *      ELGAOLCC
00759 *        TEST AOL OCCURS                                   *      ELGAOLCC
00760 *                                                          *      ELGAOLCC
00761 ************************************************************      ELGAOLCC
00762                                                                   ELGAOLCC
00763  0300-TEST-AOL-OCCURRENCE.                                        ELGAOLCC
00764      MOVE GAD-O-P-X-L-O-B (GAD-INDEX) TO WS-LOB-ACCUM-OCCRNC.     ELGAOLCC
00765      MOVE GAD-O-P-X-COST-CONTAIN-IND (GAD-INDEX)                  ELGAOLCC
00766        TO WS-COST-CONTAIN-IND.                                    ELGAOLCC
00767      SET SW-CC-IND-DOES-NOT-APPLY                                 ELGAOLCC
00768          SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELGAOLCC
00769                                                                   ELGAOLCC
00770      PERFORM 0370-CHECK-FOR-CORRECT-CC-TYPE.                      ELGAOLCC
00771      IF SW-CC-IND-APPLIES                                         ELGAOLCC
00772         PERFORM 0310-INITIALIZE-OCCURRENCE                        ELGAOLCC
00773         PERFORM 0320-SCAN-FOR-INTERNALS.                          ELGAOLCC
00774      IF SW-OCCRNC-APPLIES                                         ELGAOLCC
00775      THEN                                                         ELGAOLCC
00776 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELGAOLCC
00777         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELGAOLCC
00778         PERFORM 0360-INIT-ACCUM-EXTRACT                           ELGAOLCC
00779         PERFORM 0380-EXTRACT-ACCUM                                ELGAOLCC
00780         PERFORM 0520-CHK-EXTRACT-DATA-INTGRTY                     ELGAOLCC
00781         PERFORM 0390-EXTRACT-ACCUM-VBL-PORTION                    ELGAOLCC
00782         PERFORM 0710-WRITE-EXTRACT-RECORD                         ELGAOLCC
00783      END-IF.                                                      ELGAOLCC
00784      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-INDEX)            ELGAOLCC
00785          TO TRUE.                                                 ELGAOLCC
00786                                                                   ELGAOLCC
00787 /***********************************************************      ELGAOLCC
00788 *                                                          *      ELGAOLCC
00789 *        INITIALIZE OCCURRENCE                             *      ELGAOLCC
00790 *                                                          *      ELGAOLCC
00791 ************************************************************      ELGAOLCC
00792                                                                   ELGAOLCC
00793  0310-INITIALIZE-OCCURRENCE.                                      ELGAOLCC
00794      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELGAOLCC
00795      SET SW-HAS-NO-IBGR                                           ELGAOLCC
00796          SW-HAS-NO-IDGD                                           ELGAOLCC
00797          SW-HAS-NO-IPGN                                           ELGAOLCC
00798          SW-HAS-NO-IPGP                                           ELGAOLCC
00799          SW-HAS-NO-IPGT                                           ELGAOLCC
00800          SW-HAS-NO-IPGS                                           ELGAOLCC
00801       TO TRUE.                                                    ELGAOLCC
00802      INITIALIZE WS-IBGR-SLOT-NBR                                  ELGAOLCC
00803                 WS-IDGD-SLOT-NBR                                  ELGAOLCC
00804                 WS-IPGN-SLOT-NBR                                  ELGAOLCC
00805                 WS-IPGP-SLOT-NBR                                  ELGAOLCC
00806                 WS-IPGT-SLOT-NBR                                  ELGAOLCC
00807                 WS-IPGS-SLOT-NBR                                  ELGAOLCC
00808                 WS-INTERNAL-COUNTER.                              ELGAOLCC
00809      SET GAD-INT-INDEX TO                                         ELGAOLCC
00810          GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX).                  ELGAOLCC
00811      SET WS-MAX-GAD-INT-INDEX TO GAD-INT-INDEX.                   ELGAOLCC
00812      SET SW-INTRNL-INST-CLASS-NOT-DET                             ELGAOLCC
00813          SW-INTRNL-PROF-CLASS-NOT-DET                             ELGAOLCC
00814          SW-INTRNL-PROF-SPEC-NOT-DET                              ELGAOLCC
00815       TO TRUE.                                                    ELGAOLCC
00816                                                                   ELGAOLCC
00817                                                                   ELGAOLCC
00818 /***********************************************************      ELGAOLCC
00819 *                                                          *      ELGAOLCC
00820 *        SCAN FOR INTERNAL TABULARS                        *      ELGAOLCC
00821 *                                                          *      ELGAOLCC
00822 ************************************************************      ELGAOLCC
00823                                                                   ELGAOLCC
00824  0320-SCAN-FOR-INTERNALS.                                         ELGAOLCC
00825 *    (THIS IS DONE NOW IN CASE IPGT OR IBGR IS NEEDED TO DETERMINEELGAOLCC
00826 *     WHETHER OCCURRENCE IS INSTITUTIONAL OR PROFESSIONAL.)       ELGAOLCC
00827      PERFORM 0330-SCAN-THE-INTERNAL-TABULAR                       ELGAOLCC
00828         VARYING GAD-INT-INDEX FROM 1 BY 1                         ELGAOLCC
00829           UNTIL    GAD-INT-INDEX                                  ELGAOLCC
00830                 >= WS-MAX-GAD-INT-INDEX.                          ELGAOLCC
00831                                                                   ELGAOLCC
00832      EVALUATE TRUE ALSO TRUE                                      ELGAOLCC
00833         WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                        ELGAOLCC
00834            SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE                  ELGAOLCC
00835            SET SW-OCCRNC-APPLIES TO TRUE                          ELGAOLCC
00836         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH                 ELGAOLCC
00837            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGAOLCC
00838            PERFORM 0550-CHK-INTRNL-TAB-PROV-CLASS                 ELGAOLCC
00839         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST                 ELGAOLCC
00840            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGAOLCC
00841            SET SW-OCCRNC-APPLIES TO TRUE                          ELGAOLCC
00842         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH                 ELGAOLCC
00843            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGAOLCC
00844            PERFORM 0550-CHK-INTRNL-TAB-PROV-CLASS                 ELGAOLCC
00845            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGAOLCC
00846            PERFORM 0551-CHK-INTRNL-TAB-PROV-SPEC                  ELGAOLCC
00847         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF                 ELGAOLCC
00848            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGAOLCC
00849            SET SW-OCCRNC-APPLIES TO TRUE                          ELGAOLCC
00850         WHEN OTHER                                                ELGAOLCC
00851            CONTINUE                                               ELGAOLCC
00852      END-EVALUATE.                                                ELGAOLCC
00853                                                                   ELGAOLCC
00854 /***********************************************************      ELGAOLCC
00855 *                                                          *      ELGAOLCC
00856 *        SCAN THE INTERNAL TABULARS                        *      ELGAOLCC
00857 *                                                          *      ELGAOLCC
00858 ************************************************************      ELGAOLCC
00859                                                                   ELGAOLCC
00860  0330-SCAN-THE-INTERNAL-TABULAR.                                  ELGAOLCC
00861      IF GAD-INT-SLOT (GAD-INDEX, GAD-INT-INDEX) > 0               ELGAOLCC
00862         THEN                                                      ELGAOLCC
00863            MOVE GAD-INT-SLOT (GAD-INDEX, GAD-INT-INDEX)           ELGAOLCC
00864              TO WS-SLOT-NBR                                       ELGAOLCC
00865            EVALUATE GAD-INT-ID (GAD-INDEX, GAD-INT-INDEX)         ELGAOLCC
00866               WHEN PC-IBGR                                        ELGAOLCC
00867                 MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR              ELGAOLCC
00868                 SET SW-HAS-IBGR TO TRUE                           ELGAOLCC
00869               WHEN PC-IDGD                                        ELGAOLCC
00870                 MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR              ELGAOLCC
00871                 SET SW-HAS-IDGD TO TRUE                           ELGAOLCC
00872               WHEN PC-IPGP                                        ELGAOLCC
00873                 MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR              ELGAOLCC
00874                 SET SW-HAS-IPGP TO TRUE                           ELGAOLCC
00875               WHEN PC-IPGN                                        ELGAOLCC
00876                 MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR              ELGAOLCC
00877                 SET SW-HAS-IPGN TO TRUE                           ELGAOLCC
00878               WHEN PC-IPGT                                        ELGAOLCC
00879                 MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR              ELGAOLCC
00880                 SET SW-HAS-IPGT TO TRUE                           ELGAOLCC
00881               WHEN PC-IPGS                                        ELGAOLCC
00882                 MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR              ELGAOLCC
00883                 SET SW-HAS-IPGS TO TRUE                           ELGAOLCC
00884               WHEN OTHER                                          ELGAOLCC
00885                  CONTINUE                                         ELGAOLCC
00886            END-EVALUATE                                           ELGAOLCC
00887      END-IF.                                                      ELGAOLCC
00888                                                                   ELGAOLCC
00889                                                                   ELGAOLCC
00890 /***********************************************************      ELGAOLCC
00891 *                                                          *      ELGAOLCC
00892 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELGAOLCC
00893 *                                                          *      ELGAOLCC
00894 ************************************************************      ELGAOLCC
00895                                                                   ELGAOLCC
00896  0360-INIT-ACCUM-EXTRACT.                                         ELGAOLCC
00897      INITIALIZE ACCUM-FIXED-AREA.                                 ELGAOLCC
00898      SET ACCUM-AOL TO TRUE.                                       ELGAOLCC
00899      MOVE +1 TO ACCUM-ASCEND-DESCEND-COUNT.                       ELGAOLCC
00900      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELGAOLCC
00901      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELGAOLCC
00902      INITIALIZE ACCUM-COPAY-ENTRY (1).                            ELGAOLCC
00903                                                                   ELGAOLCC
00904 /***********************************************************      ELGAOLCC
00905 *                                                          *      ELGAOLCC
00906 *        CHECK FOR CORRECT COST CONTAINMENT TYPE           *      ELGAOLCC
00907 *                                                          *      ELGAOLCC
00908 ************************************************************      ELGAOLCC
00909                                                                   ELGAOLCC
00910  0370-CHECK-FOR-CORRECT-CC-TYPE.                                  ELGAOLCC
00911      MOVE SSB-MODIFIER-1 TO WS-SUBTOPIC-TYPE.                     ELGAOLCC
00912      IF    (SUBTOPIC-ATCP    AND CC-ATCP)                         ELGAOLCC
00913         OR (SUBTOPIC-WEEKEND AND CC-WEEKEND)                      ELGAOLCC
00914         OR (SUBTOPIC-HOSP    AND CC-HOSP)                         ELGAOLCC
00915         OR (SUBTOPIC-IOB     AND CC-IOB)                          ELGAOLCC
00916         OR (SUBTOPIC-MASOP   AND CC-MASOP)                        ELGAOLCC
00917         OR (SUBTOPIC-MONDIS  AND CC-MONDIS)                       ELGAOLCC
00918         OR (SUBTOPIC-MEDNEC  AND CC-MEDNEC)                       ELGAOLCC
00919         OR (SUBTOPIC-MOPS    AND CC-MOPS)                         ELGAOLCC
00920         OR (SUBTOPIC-MSA     AND CC-MSA)                          ELGAOLCC
00921         OR (SUBTOPIC-PPO     AND CC-PPO)                          ELGAOLCC
00922         OR (SUBTOPIC-PAR     AND CC-PAR)                          ELGAOLCC
00923         OR (SUBTOPIC-PAT     AND CC-PAT)                          ELGAOLCC
00924         OR (SUBTOPIC-EMH     AND CC-EMH)                          ELGAOLCC
00925         OR (SUBTOPIC-MCN     AND CC-MCN)                          ELGAOLCC
00926         OR (SUBTOPIC-POS     AND CC-POS)                          ELGAOLCC
00927         OR (SUBTOPIC-MHSC    AND CC-MHSC)                         ELGAOLCC
00928         OR (SUBTOPIC-RPO     AND CC-RPO)                          ELGAOLCC
00929         OR (SUBTOPIC-BAE     AND CC-BAE)                          ELGAOLCC
00930         OR (SUBTOPIC-CPO     AND CC-CPO)                          ELGAOLCC
00931         OR (SUBTOPIC-CBL     AND CC-CBL)                          ELGAOLCC
00932         OR (SUBTOPIC-PAN     AND CC-PAN)                          ELGAOLCC
00933      THEN                                                         ELGAOLCC
00934          SET SW-CC-IND-APPLIES TO TRUE.                           ELGAOLCC
00935                                                                   ELGAOLCC
00936                                                                   ELGAOLCC
00937 /***********************************************************      ELGAOLCC
00938 *                                                          *      ELGAOLCC
00939 *        EXTRACT ACCUM                                     *      ELGAOLCC
00940 *                                                          *      ELGAOLCC
00941 ************************************************************      ELGAOLCC
00942                                                                   ELGAOLCC
00943  0380-EXTRACT-ACCUM.                                              ELGAOLCC
00944                                                                   ELGAOLCC
00945 * -- SET FIXED PORTION DATA ELEMENTS                              ELGAOLCC
00946                                                                   ELGAOLCC
00947      MOVE GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)                    ELGAOLCC
00948        TO     ACCUM-BENEFIT-PERIOD.                               ELGAOLCC
00949                                                                   ELGAOLCC
00950      MOVE GAD-O-P-X-FAM-OR-INDIV (GAD-INDEX)                      ELGAOLCC
00951        TO     ACCUM-FAM-OR-INDIV.                                 ELGAOLCC
00952                                                                   ELGAOLCC
00953      MOVE GAD-O-P-X-L-O-B (GAD-INDEX) TO ACCUM-L-O-B.             ELGAOLCC
00954                                                                   ELGAOLCC
00955      MOVE GAD-O-P-X-DEFINITION (GAD-INDEX) TO ACCUM-DEFINITION.   ELGAOLCC
00956                                                                   ELGAOLCC
00957      MOVE GAD-O-P-X-DAY-FACTOR-IND (GAD-INDEX)                    ELGAOLCC
00958        TO     ACCUM-DAY-FACTOR-IND.                               ELGAOLCC
00959                                                                   ELGAOLCC
00960      MOVE GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX)               ELGAOLCC
00961        TO     ACCUM-INTERNAL-DESCRIPTOR.                          ELGAOLCC
00962                                                                   ELGAOLCC
00963      MOVE GAD-O-P-X-SERVICE-GROUP (GAD-INDEX)                     ELGAOLCC
00964        TO     ACCUM-SERVICE-GROUP.                                ELGAOLCC
00965                                                                   ELGAOLCC
00966      MOVE GAD-O-P-X-PLACE-OF-TREATMENT (GAD-INDEX)                ELGAOLCC
00967        TO     ACCUM-PLACE-OF-TREATMENT.                           ELGAOLCC
00968                                                                   ELGAOLCC
00969      MOVE GAD-O-P-X-CONDITION (GAD-INDEX) TO ACCUM-CONDITION.     ELGAOLCC
00970                                                                   ELGAOLCC
00971      MOVE GAD-O-P-X-CLAIM-LVL-ACCUM-IND (GAD-INDEX)               ELGAOLCC
00972        TO     ACCUM-CLAIM-LVL-ACCUM-IND.                          ELGAOLCC
00973                                                                   ELGAOLCC
00974      MOVE GAD-O-P-X-CO-PAY-IND (GAD-INDEX)                        ELGAOLCC
00975        TO     ACCUM-CO-PAY-IND (COPAY-INDEX).                     ELGAOLCC
00976                                                                   ELGAOLCC
00977      MOVE GAD-O-P-X-COST-CONTAIN-IND (GAD-INDEX)                  ELGAOLCC
00978        TO     ACCUM-COST-CONTAIN-IND.                             ELGAOLCC
00979                                                                   ELGAOLCC
00980      MOVE GAD-O-P-X-AGE-LIMIT-FROM (GAD-INDEX)                    ELGAOLCC
00981        TO     ACCUM-AGE-LIMIT-FROM-VAL.                           ELGAOLCC
00982                                                                   ELGAOLCC
00983      MOVE GAD-O-P-X-AGE-LIMIT-TO (GAD-INDEX)                      ELGAOLCC
00984        TO     ACCUM-AGE-LIMIT-TO-VAL.                             ELGAOLCC
00985                                                                   ELGAOLCC
00986      MOVE GAD-O-P-X-AGE-QUAL-IND-FROM (GAD-INDEX)                 ELGAOLCC
00987        TO     ACCUM-AGE-LIMIT-FROM-IND.                           ELGAOLCC
00988                                                                   ELGAOLCC
00989      MOVE GAD-O-P-X-AGE-QUAL-IND-TO (GAD-INDEX)                   ELGAOLCC
00990        TO     ACCUM-AGE-LIMIT-TO-IND.                             ELGAOLCC
00991                                                                   ELGAOLCC
00992      MOVE GAD-O-P-X-RELATIONSHIP-IND (GAD-INDEX)                  ELGAOLCC
00993        TO     ACCUM-RELATIONSHIP-IND.                             ELGAOLCC
00994                                                                   ELGAOLCC
00995      MOVE   GAD-CARRY-OVER-CREDIT-IND (GAD-INDEX)                 ELGAOLCC
00996        TO ACCUM-CARRY-OVER-CREDIT-IND.                            ELGAOLCC
00997                                                                   ELGAOLCC
00998      MOVE GAD-O-P-X-ASCEND-DESCEND-IND (GAD-INDEX)                ELGAOLCC
00999        TO     ACCUM-ASCEND-DESCEND-IND.                           ELGAOLCC
01000                                                                   ELGAOLCC
01001      MOVE GAD-O-P-X-BEN-PER-TIME-QUAL (GAD-INDEX)                 ELGAOLCC
01002        TO     ACCUM-BEN-PER-TIME-QUAL.                            ELGAOLCC
01003                                                                   ELGAOLCC
01004      MOVE GAD-O-P-X-BEN-PER-TIME-FCTR (GAD-INDEX)                 ELGAOLCC
01005        TO     ACCUM-BEN-PER-TIME-FCTR.                            ELGAOLCC
01006                                                                   ELGAOLCC
01007      MOVE GAD-O-P-X-INTERVAL-TIME-FCTR (GAD-INDEX)                ELGAOLCC
01008        TO     ACCUM-INTERVAL-TIME-FCTR.                           ELGAOLCC
01009                                                                   ELGAOLCC
01010      MOVE GAD-O-P-X-INTERVAL-TYPE (GAD-INDEX)                     ELGAOLCC
01011        TO     ACCUM-INTERVAL-TYPE.                                ELGAOLCC
01012                                                                   ELGAOLCC
01013      MOVE GAD-O-P-X-INTERVAL-OVRD-IND (GAD-INDEX)                 ELGAOLCC
01014        TO     ACCUM-INTERVAL-OVRD-IND.                            ELGAOLCC
01015                                                                   ELGAOLCC
01016      MOVE GAD-O-P-X-INTERVAL-OVRD-VALUE (GAD-INDEX)               ELGAOLCC
01017        TO     ACCUM-INTERVAL-OVRD-VALUE.                          ELGAOLCC
01018                                                                   ELGAOLCC
01019      MOVE GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)                   ELGAOLCC
01020        TO     ACCUM-VALUE-QUALIFIER.                              ELGAOLCC
01021                                                                   ELGAOLCC
01022      MOVE GAD-O-P-X-FYI-VALUE (GAD-INDEX) TO ACCUM-FYI-VALUE.     ELGAOLCC
01023                                                                   ELGAOLCC
01024      SET REINSTATEMENT-IND-NA TO TRUE.                            ELGAOLCC
01025      SET DED-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELGAOLCC
01026      SET MAX-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELGAOLCC
01027      MOVE GCG-OUTPKT-BASE-AMT-SOURCE-IN                           ELGAOLCC
01028        TO  ACCUM-OPX-BASE-AMT-SOURCE-IND.                         ELGAOLCC
01029                                                                   ELGAOLCC
01030 * -- SET OCCURRENCE PROVIDER CLASS INFORMATION                    ELGAOLCC
01031      EVALUATE TRUE ALSO TRUE                                      ELGAOLCC
01032         WHEN      SW-INTRNL-INST-PROV-CLASS                       ELGAOLCC
01033              ALSO SW-INTRNL-NOT-PROF-PROV-CLASS                   ELGAOLCC
01034            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELGAOLCC
01035         WHEN      SW-INTRNL-NOT-INST-PROV-CLASS                   ELGAOLCC
01036              ALSO SW-INTRNL-PROF-PROV-CLASS                       ELGAOLCC
01037            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELGAOLCC
01038         WHEN OTHER                                                ELGAOLCC
01039            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELGAOLCC
01040      END-EVALUATE.                                                ELGAOLCC
01041                                                                   ELGAOLCC
01042      IF SW-INTRNL-PROF-PROV-CLASS                                 ELGAOLCC
01043             SET ACCUM-PRVDR-CLS-PROF TO TRUE                      ELGAOLCC
01044      END-IF.                                                      ELGAOLCC
01045                                                                   ELGAOLCC
01046 /***********************************************************      ELGAOLCC
01047 *                                                          *      ELGAOLCC
01048 *        EXTRACT ACCUM VARIABLE PORTION                    *      ELGAOLCC
01049 *                                                          *      ELGAOLCC
01050 ************************************************************      ELGAOLCC
01051                                                                   ELGAOLCC
01052  0390-EXTRACT-ACCUM-VBL-PORTION.                                  ELGAOLCC
01053      SET ASC-DES-INDEX TO 1.                                      ELGAOLCC
01054      PERFORM 0395-EXTRACT-VARIABLE-PORTION.                       ELGAOLCC
01055      IF ACCUM-VARIABLE-TYPE                                       ELGAOLCC
01056             PERFORM 0400-EXTRACT-ADDL-OCCURNCS.                   ELGAOLCC
01057                                                                   ELGAOLCC
01058                                                                   ELGAOLCC
01059 /***********************************************************      ELGAOLCC
01060 *                                                          *      ELGAOLCC
01061 *        EXTRACT VARIABLE PORTION                          *      ELGAOLCC
01062 *                                                          *      ELGAOLCC
01063 ************************************************************      ELGAOLCC
01064                                                                   ELGAOLCC
01065  0395-EXTRACT-VARIABLE-PORTION.                                   ELGAOLCC
01066      MOVE GAD-O-P-X-BISCENDING-IND (GAD-INDEX)                    ELGAOLCC
01067        TO     ACCUM-BISCEND-IND (ASC-DES-INDEX).                  ELGAOLCC
01068      MOVE GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX)                     ELGAOLCC
01069        TO     ACCUM-PERCENT-LEVEL (ASC-DES-INDEX).                ELGAOLCC
01070      MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)                       ELGAOLCC
01071        TO     ACCUM-VALUE-LIMIT (ASC-DES-INDEX).                  ELGAOLCC
01072      MOVE    WS-IBGR-SLOT-NBR                                     ELGAOLCC
01073        TO ACCUM-IBGR-SLOT-NBR (ASC-DES-INDEX).                    ELGAOLCC
01074      MOVE    WS-IDGD-SLOT-NBR                                     ELGAOLCC
01075        TO ACCUM-IDGD-SLOT-NBR (ASC-DES-INDEX).                    ELGAOLCC
01076      MOVE    WS-IPGN-SLOT-NBR                                     ELGAOLCC
01077        TO ACCUM-IPGN-SLOT-NBR (ASC-DES-INDEX).                    ELGAOLCC
01078      MOVE    WS-IPGP-SLOT-NBR                                     ELGAOLCC
01079        TO ACCUM-IPGP-SLOT-NBR (ASC-DES-INDEX).                    ELGAOLCC
01080      MOVE    WS-IPGT-SLOT-NBR                                     ELGAOLCC
01081        TO ACCUM-IPGT-SLOT-NBR (ASC-DES-INDEX).                    ELGAOLCC
01082      MOVE    WS-IPGS-SLOT-NBR                                     ELGAOLCC
01083        TO ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX).                    ELGAOLCC
01084      IF ACCUM-BISCEND-IND (ASC-DES-INDEX)                         ELGAOLCC
01085         = ZERO OR SPACES OR LOW-VALUES                            ELGAOLCC
01086         SET   BISCEND-IND-NA (ASC-DES-INDEX) TO TRUE.             ELGAOLCC
01087                                                                   ELGAOLCC
01088      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB) TO TRUE.     ELGAOLCC
01089                                                                   ELGAOLCC
01090                                                                   ELGAOLCC
01091 /***********************************************************      ELGAOLCC
01092 *                                                          *      ELGAOLCC
01093 *        EXTRACT ADDITIONAL OCCURRENCES                    *      ELGAOLCC
01094 *                                                          *      ELGAOLCC
01095 ************************************************************      ELGAOLCC
01096                                                                   ELGAOLCC
01097  0400-EXTRACT-ADDL-OCCURNCS.                                      ELGAOLCC
01098      MOVE WS-OCCURRENCE-SUB TO WS-SAVE-SUB                        ELGAOLCC
01099      SET  WS-SAVE-INDEX     TO GAD-INDEX.                         ELGAOLCC
01100      ADD 1 TO WS-OCCURRENCE-SUB.                                  ELGAOLCC
01101      PERFORM 0410-TEST-SUBSEQ-OCCRNCES                            ELGAOLCC
01102          VARYING WS-OCCURRENCE-SUB                                ELGAOLCC
01103             FROM WS-OCCURRENCE-SUB BY 1                           ELGAOLCC
01104          UNTIL WS-OCCURRENCE-INDEX >= GAD-ENTRY-COUNT.            ELGAOLCC
01105      MOVE WS-SAVE-SUB TO WS-OCCURRENCE-SUB.                       ELGAOLCC
01106      SET  GAD-INDEX   TO WS-SAVE-INDEX.                           ELGAOLCC
01107                                                                   ELGAOLCC
01108                                                                   ELGAOLCC
01109                                                                   ELGAOLCC
01110 ************************************************************      ELGAOLCC
01111 *                                                          *      ELGAOLCC
01112 *        TEST SUBSEQUENT OCCURRENCES                       *      ELGAOLCC
01113 *                                                          *      ELGAOLCC
01114 ************************************************************      ELGAOLCC
01115                                                                   ELGAOLCC
01116  0410-TEST-SUBSEQ-OCCRNCES.                                       ELGAOLCC
01117      SET GAD-INDEX           TO WS-OCCURRENCE-SUB.                ELGAOLCC
01118      SET WS-OCCURRENCE-INDEX TO WS-OCCURRENCE-SUB.                ELGAOLCC
01119      IF WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-INDEX)             ELGAOLCC
01120          CONTINUE                                                 ELGAOLCC
01121      ELSE PERFORM 0420-TEST-OCCURRENCE.                           ELGAOLCC
01122                                                                   ELGAOLCC
01123                                                                   ELGAOLCC
01124                                                                   ELGAOLCC
01125 /***********************************************************      ELGAOLCC
01126 *                                                          *      ELGAOLCC
01127 *        TEST OCCURRENCE                                   *      ELGAOLCC
01128 *                                                          *      ELGAOLCC
01129 ************************************************************      ELGAOLCC
01130                                                                   ELGAOLCC
01131  0420-TEST-OCCURRENCE.                                            ELGAOLCC
01132      SET SW-MATCHING-ENTRY-NOT-FOUND TO TRUE.                     ELGAOLCC
01133      PERFORM 0430-TEST-KEYS-FOR-MATCH.                            ELGAOLCC
01134      IF SW-MATCHING-ENTRY-FOUND                                   ELGAOLCC
01135          PERFORM 0440-COMPLETE-TEST-OF-OCCURNCE.                  ELGAOLCC
01136                                                                   ELGAOLCC
01137                                                                   ELGAOLCC
01138                                                                   ELGAOLCC
01139 /***********************************************************      ELGAOLCC
01140 *                                                          *      ELGAOLCC
01141 *        TEST KEYS FOR MATCH                               *      ELGAOLCC
01142 *                                                          *      ELGAOLCC
01143 ************************************************************      ELGAOLCC
01144                                                                   ELGAOLCC
01145  0430-TEST-KEYS-FOR-MATCH.                                        ELGAOLCC
01146      IF  GAD-O-P-X-FYI-VALUE (GAD-INDEX) =                        ELGAOLCC
01147              ACCUM-FYI-VALUE                                      ELGAOLCC
01148                        AND                                        ELGAOLCC
01149          GAD-O-P-X-COST-CONTAIN-IND (GAD-INDEX) =                 ELGAOLCC
01150              ACCUM-COST-CONTAIN-IND                               ELGAOLCC
01151                        AND                                        ELGAOLCC
01152          GAD-O-P-X-PLACE-OF-TREATMENT (GAD-INDEX) =               ELGAOLCC
01153              ACCUM-PLACE-OF-TREATMENT                             ELGAOLCC
01154                        AND                                        ELGAOLCC
01155          GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX) =                   ELGAOLCC
01156              ACCUM-BENEFIT-PERIOD                                 ELGAOLCC
01157                        AND                                        ELGAOLCC
01158          GAD-O-P-X-L-O-B (GAD-INDEX) =                            ELGAOLCC
01159              ACCUM-L-O-B                                          ELGAOLCC
01160                        AND                                        ELGAOLCC
01161          GAD-O-P-X-ASCEND-DESCEND-IND (GAD-INDEX) =               ELGAOLCC
01162              ACCUM-ASCEND-DESCEND-IND                             ELGAOLCC
01163                        AND                                        ELGAOLCC
01164                GAD-COND-ALL-BIT (GAD-INDEX) =                     ELGAOLCC
01165              ACCUM-COND-ALL-BIT                                   ELGAOLCC
01166                        AND                                        ELGAOLCC
01167                GAD-COND-EXCLUSION-BIT (GAD-INDEX) =               ELGAOLCC
01168              ACCUM-COND-EXCLUSION-BIT                             ELGAOLCC
01169                        AND                                        ELGAOLCC
01170                GAD-COND-ICD-BIT (GAD-INDEX) =                     ELGAOLCC
01171              ACCUM-COND-ICD-BIT                                   ELGAOLCC
01172                        AND                                        ELGAOLCC
01173                GAD-COND-TB-BIT (GAD-INDEX) =                      ELGAOLCC
01174              ACCUM-COND-TB-BIT                                    ELGAOLCC
01175                        AND                                        ELGAOLCC
01176                GAD-COND-MENTAL-BIT (GAD-INDEX) =                  ELGAOLCC
01177              ACCUM-COND-MENTAL-BIT                                ELGAOLCC
01178                        AND                                        ELGAOLCC
01179                GAD-COND-DRUG-BIT (GAD-INDEX) =                    ELGAOLCC
01180              ACCUM-COND-DRUG-BIT                                  ELGAOLCC
01181                        AND                                        ELGAOLCC
01182                GAD-COND-ALCOHOL-BIT (GAD-INDEX) =                 ELGAOLCC
01183              ACCUM-COND-ALCOHOL-BIT                               ELGAOLCC
01184                        AND                                        ELGAOLCC
01185                GAD-COND-OB-COMP-BIT (GAD-INDEX) =                 ELGAOLCC
01186              ACCUM-COND-OB-COMP-BIT                               ELGAOLCC
01187                        AND                                        ELGAOLCC
01188                GAD-COND-OB-NORM-BIT (GAD-INDEX) =                 ELGAOLCC
01189              ACCUM-COND-OB-NORM-BIT                               ELGAOLCC
01190                        AND                                        ELGAOLCC
01191                GAD-COND-MALIGNANCY-BIT (GAD-INDEX) =              ELGAOLCC
01192              ACCUM-COND-MALIGNANCY-BIT                            ELGAOLCC
01193                        AND                                        ELGAOLCC
01194                GAD-COND-CARDIAC-DISEASE-BIT (GAD-INDEX) =         ELGAOLCC
01195              ACCUM-COND-CARDIAC-DISEASE-BIT                       ELGAOLCC
01196                        AND                                        ELGAOLCC
01197                GAD-COND-OBESITY-BIT (GAD-INDEX) =                 ELGAOLCC
01198              ACCUM-COND-OBESITY-BIT                               ELGAOLCC
01199                        AND                                        ELGAOLCC
01200                GAD-COND-KIDNEY-DISEASE-BIT (GAD-INDEX) =          ELGAOLCC
01201              ACCUM-COND-KIDNEY-DISEASE-BIT                        ELGAOLCC
01202                        AND                                        ELGAOLCC
01203                GAD-COND-ACCIDENT-BIT (GAD-INDEX) =                ELGAOLCC
01204              ACCUM-COND-ACCIDENT-BIT                              ELGAOLCC
01205                        AND                                        ELGAOLCC
01206                GAD-COND-PRE-EXIST-BIT (GAD-INDEX) =               ELGAOLCC
01207              ACCUM-COND-PRE-EXIST-BIT                             ELGAOLCC
01208                        AND                                        ELGAOLCC
01209                GAD-COND-NON-EMER-BIT (GAD-INDEX) =                ELGAOLCC
01210              ACCUM-COND-NON-EMER-BIT                              ELGAOLCC
01211                        AND                                        ELGAOLCC
01212                GAD-COND-SUICIDE-BIT (GAD-INDEX) =                 ELGAOLCC
01213              ACCUM-COND-SUICIDE-BIT                               ELGAOLCC
01214                        AND                                        ELGAOLCC
01215                GAD-COND-TMJ-BIT (GAD-INDEX) =                     ELGAOLCC
01216              ACCUM-COND-TMJ-BIT                                   ELGAOLCC
01217                        AND                                        ELGAOLCC
01218                GAD-COND-INF-BIT (GAD-INDEX) =                     ELGAOLCC
01219              ACCUM-COND-INF-BIT                                   ELGAOLCC
01220                        AND                                        ELGAOLCC
01221                GAD-COND-LIFE-THREAT-BIT (GAD-INDEX) =             ELGAOLCC
01222              ACCUM-COND-LIFE-THREAT-BIT                           ELGAOLCC
01223                        AND                                        ELGAOLCC
01224          GAD-O-P-X-FAM-OR-INDIV (GAD-INDEX) =                     ELGAOLCC
01225              ACCUM-FAM-OR-INDIV                                   ELGAOLCC
01226                        AND                                        ELGAOLCC
01227          GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) =                  ELGAOLCC
01228              ACCUM-VALUE-QUALIFIER                                ELGAOLCC
01229                        AND                                        ELGAOLCC
01230          GAD-O-P-X-RELATIONSHIP-IND (GAD-INDEX) =                 ELGAOLCC
01231              ACCUM-RELATIONSHIP-IND                               ELGAOLCC
01232                        AND                                        ELGAOLCC
01233          GAD-O-P-X-AGE-LIMIT-FROM (GAD-INDEX) =                   ELGAOLCC
01234              ACCUM-AGE-LIMIT-FROM-VAL                             ELGAOLCC
01235                        AND                                        ELGAOLCC
01236          GAD-O-P-X-AGE-LIMIT-TO (GAD-INDEX) =                     ELGAOLCC
01237              ACCUM-AGE-LIMIT-TO-VAL                               ELGAOLCC
01238                        AND                                        ELGAOLCC
01239          GAD-O-P-X-AGE-QUAL-IND-FROM (GAD-INDEX) =                ELGAOLCC
01240              ACCUM-AGE-LIMIT-FROM-IND                             ELGAOLCC
01241                        AND                                        ELGAOLCC
01242          GAD-O-P-X-AGE-QUAL-IND-TO (GAD-INDEX) =                  ELGAOLCC
01243              ACCUM-AGE-LIMIT-TO-IND                               ELGAOLCC
01244                        AND                                        ELGAOLCC
01245          GAD-O-P-X-CO-PAY-IND (GAD-INDEX) =                       ELGAOLCC
01246              ACCUM-CO-PAY-IND (COPAY-INDEX)                       ELGAOLCC
01247                        AND                                        ELGAOLCC
01248          GAD-O-P-X-SERVICE-GROUP (GAD-INDEX) =                    ELGAOLCC
01249              ACCUM-SERVICE-GROUP                                  ELGAOLCC
01250                        AND                                        ELGAOLCC
01251          GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX) =              ELGAOLCC
01252              ACCUM-INTERNAL-DESCRIPTOR                            ELGAOLCC
01253      THEN                                                         ELGAOLCC
01254          SET SW-MATCHING-ENTRY-FOUND TO TRUE.                     ELGAOLCC
01255                                                                   ELGAOLCC
01256                                                                   ELGAOLCC
01257                                                                   ELGAOLCC
01258                                                                   ELGAOLCC
01259 /***********************************************************      ELGAOLCC
01260 *                                                          *      ELGAOLCC
01261 *        COMPLETE TEST OF OCCURRENCE                       *      ELGAOLCC
01262 *                                                          *      ELGAOLCC
01263 ************************************************************      ELGAOLCC
01264                                                                   ELGAOLCC
01265  0440-COMPLETE-TEST-OF-OCCURNCE.                                  ELGAOLCC
01266      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELGAOLCC
01267      PERFORM 0320-SCAN-FOR-INTERNALS                              ELGAOLCC
01268      IF SW-OCCRNC-APPLIES                                         ELGAOLCC
01269         PERFORM 0450-EXTRCT-NXT-OCCURNCE-DATA                     ELGAOLCC
01270      ELSE                                                         ELGAOLCC
01271         SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB) TO TRUE.  ELGAOLCC
01272                                                                   ELGAOLCC
01273                                                                   ELGAOLCC
01274                                                                   ELGAOLCC
01275 ************************************************************      ELGAOLCC
01276 *                                                          *      ELGAOLCC
01277 *        EXTRACT NEXT OCCURRENCE DATA                      *      ELGAOLCC
01278 *                                                          *      ELGAOLCC
01279 ************************************************************      ELGAOLCC
01280                                                                   ELGAOLCC
01281  0450-EXTRCT-NXT-OCCURNCE-DATA.                                   ELGAOLCC
01282      ADD +1 TO ACCUM-ASCEND-DESCEND-COUNT.                        ELGAOLCC
01283      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELGAOLCC
01284      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (ASC-DES-INDEX).       ELGAOLCC
01285      PERFORM 0395-EXTRACT-VARIABLE-PORTION.                       ELGAOLCC
01286      PERFORM 0470-INSERT-NEW-ENTRY.                               ELGAOLCC
01287                                                                   ELGAOLCC
01288                                                                   ELGAOLCC
01289                                                                   ELGAOLCC
01290 /***********************************************************      ELGAOLCC
01291 *                                                          *      ELGAOLCC
01292 *        INSERT NEW ENTRY                                  *      ELGAOLCC
01293 *                                                          *      ELGAOLCC
01294 ************************************************************      ELGAOLCC
01295                                                                   ELGAOLCC
01296  0470-INSERT-NEW-ENTRY.                                           ELGAOLCC
01297      MOVE ACCUM-ASCEND-DESCEND-COUNT TO SORT-SUB.                 ELGAOLCC
01298      SET SW-SORT-NOT-COMPLETED TO TRUE.                           ELGAOLCC
01299      IF SORT-SUB = 1                                              ELGAOLCC
01300         CONTINUE                                                  ELGAOLCC
01301      ELSE IF ACCUM-ASCEND-ORDER                                   ELGAOLCC
01302              PERFORM 0480-ASCEND-INSERT                           ELGAOLCC
01303                UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1            ELGAOLCC
01304           ELSE IF ACCUM-DESCEND-ORDER                             ELGAOLCC
01305                   PERFORM 0490-DESCEND-INSERT                     ELGAOLCC
01306                     UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1       ELGAOLCC
01307                ELSE IF ACCUM-BISCEND-ORDER                        ELGAOLCC
01308                        PERFORM 0500-BISCEND-INSERT                ELGAOLCC
01309                          UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1. ELGAOLCC
01310                                                                   ELGAOLCC
01311                                                                   ELGAOLCC
01312                                                                   ELGAOLCC
01313 /***********************************************************      ELGAOLCC
01314 *                                                          *      ELGAOLCC
01315 *        ASCEND INSERT                                     *      ELGAOLCC
01316 *                                                          *      ELGAOLCC
01317 ************************************************************      ELGAOLCC
01318                                                                   ELGAOLCC
01319  0480-ASCEND-INSERT.                                              ELGAOLCC
01320      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELGAOLCC
01321      IF ACCUM-PERCENT-LEVEL (SORT-SUB) <                          ELGAOLCC
01322         ACCUM-PERCENT-LEVEL (TEST-SUB)                            ELGAOLCC
01323         PERFORM 0510-SWAP-ENTRIES                                 ELGAOLCC
01324      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELGAOLCC
01325                                                                   ELGAOLCC
01326                                                                   ELGAOLCC
01327                                                                   ELGAOLCC
01328 ************************************************************      ELGAOLCC
01329 *                                                          *      ELGAOLCC
01330 *        DESCEND INSERT                                    *      ELGAOLCC
01331 *                                                          *      ELGAOLCC
01332 ************************************************************      ELGAOLCC
01333                                                                   ELGAOLCC
01334  0490-DESCEND-INSERT.                                             ELGAOLCC
01335      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELGAOLCC
01336      IF ACCUM-PERCENT-LEVEL (SORT-SUB) >                          ELGAOLCC
01337         ACCUM-PERCENT-LEVEL (TEST-SUB)                            ELGAOLCC
01338         PERFORM 0510-SWAP-ENTRIES                                 ELGAOLCC
01339      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELGAOLCC
01340                                                                   ELGAOLCC
01341                                                                   ELGAOLCC
01342                                                                   ELGAOLCC
01343 /***********************************************************      ELGAOLCC
01344 *                                                          *      ELGAOLCC
01345 *        BISCEND INSERT                                    *      ELGAOLCC
01346 *                                                          *      ELGAOLCC
01347 ************************************************************      ELGAOLCC
01348                                                                   ELGAOLCC
01349  0500-BISCEND-INSERT.                                             ELGAOLCC
01350      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELGAOLCC
01351      IF ACCUM-BISCEND-IND (SORT-SUB) <                            ELGAOLCC
01352         ACCUM-BISCEND-IND (TEST-SUB)                              ELGAOLCC
01353         PERFORM 0510-SWAP-ENTRIES                                 ELGAOLCC
01354      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELGAOLCC
01355                                                                   ELGAOLCC
01356                                                                   ELGAOLCC
01357                                                                   ELGAOLCC
01358 ************************************************************      ELGAOLCC
01359 *                                                          *      ELGAOLCC
01360 *        SWAP ENTRIES                                      *      ELGAOLCC
01361 *                                                          *      ELGAOLCC
01362 ************************************************************      ELGAOLCC
01363                                                                   ELGAOLCC
01364  0510-SWAP-ENTRIES.                                               ELGAOLCC
01365      MOVE ACCUM-ASCEND-DESCEND-ENTRY (SORT-SUB)                   ELGAOLCC
01366        TO    WS-ASCEND-DESCEND-ENTRY-HOLD.                        ELGAOLCC
01367      MOVE ACCUM-ASCEND-DESCEND-ENTRY (TEST-SUB)                   ELGAOLCC
01368        TO ACCUM-ASCEND-DESCEND-ENTRY (SORT-SUB).                  ELGAOLCC
01369      MOVE    WS-ASCEND-DESCEND-ENTRY-HOLD                         ELGAOLCC
01370        TO ACCUM-ASCEND-DESCEND-ENTRY (TEST-SUB).                  ELGAOLCC
01371      MOVE TEST-SUB TO SORT-SUB.                                   ELGAOLCC
01372                                                                   ELGAOLCC
01373                                                                   ELGAOLCC
01374 /***********************************************************      ELGAOLCC
01375 *                                                          *      ELGAOLCC
01376 *    CHECK EXTRACT DATA INTEGRITY                          *      ELGAOLCC
01377 *                                                          *      ELGAOLCC
01378 ************************************************************      ELGAOLCC
01379                                                                   ELGAOLCC
01380  0520-CHK-EXTRACT-DATA-INTGRTY.                                   ELGAOLCC
01381      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELGAOLCC
01382      THEN                                                         ELGAOLCC
01383           SET FYI-VALUE-NA TO TRUE                                ELGAOLCC
01384      END-IF.                                                      ELGAOLCC
01385                                                                   ELGAOLCC
01386      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELGAOLCC
01387      THEN                                                         ELGAOLCC
01388           SET COST-CONTAIN-IND-NA TO TRUE                         ELGAOLCC
01389      END-IF.                                                      ELGAOLCC
01390                                                                   ELGAOLCC
01391      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELGAOLCC
01392      THEN                                                         ELGAOLCC
01393           SET PLACE-OF-TREATMENT-NA TO TRUE                       ELGAOLCC
01394      END-IF.                                                      ELGAOLCC
01395                                                                   ELGAOLCC
01396      IF ACCUM-BENEFIT-PERIOD = ZEROS OR SPACES OR LOW-VALUES      ELGAOLCC
01397      THEN                                                         ELGAOLCC
01398           SET BENEFIT-PERIOD-NA TO TRUE                           ELGAOLCC
01399      END-IF.                                                      ELGAOLCC
01400                                                                   ELGAOLCC
01401      IF ACCUM-BEN-PER-TIME-QUAL = ZEROS OR SPACES OR LOW-VALUES   ELGAOLCC
01402      THEN                                                         ELGAOLCC
01403           SET BEN-PER-TIME-QUAL-NA TO TRUE                        ELGAOLCC
01404      END-IF.                                                      ELGAOLCC
01405                                                                   ELGAOLCC
01406      IF ACCUM-INTERVAL-TYPE = ZEROS OR SPACES OR LOW-VALUES       ELGAOLCC
01407      THEN                                                         ELGAOLCC
01408           SET INTERVAL-TYPE-NA TO TRUE                            ELGAOLCC
01409      END-IF.                                                      ELGAOLCC
01410                                                                   ELGAOLCC
01411      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELGAOLCC
01412      THEN                                                         ELGAOLCC
01413           SET INTERVAL-OVRD-IND-NA TO TRUE                        ELGAOLCC
01414      END-IF.                                                      ELGAOLCC
01415                                                                   ELGAOLCC
01416      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELGAOLCC
01417      THEN                                                         ELGAOLCC
01418           SET L-O-B-NA TO TRUE                                    ELGAOLCC
01419      END-IF.                                                      ELGAOLCC
01420                                                                   ELGAOLCC
01421      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELGAOLCC
01422      THEN                                                         ELGAOLCC
01423           SET REINSTATEMENT-IND-NA TO TRUE                        ELGAOLCC
01424      END-IF.                                                      ELGAOLCC
01425                                                                   ELGAOLCC
01426      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELGAOLCC
01427      THEN                                                         ELGAOLCC
01428           SET DEFINITION-NA TO TRUE                               ELGAOLCC
01429      END-IF.                                                      ELGAOLCC
01430                                                                   ELGAOLCC
01431      IF   ACCUM-CARRY-OVER-CREDIT-IND                             ELGAOLCC
01432         = ZEROS OR SPACES OR LOW-VALUES                           ELGAOLCC
01433      THEN                                                         ELGAOLCC
01434         SET     CARRY-OVER-CREDIT-IND-NA TO TRUE                  ELGAOLCC
01435      END-IF.                                                      ELGAOLCC
01436                                                                   ELGAOLCC
01437      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELGAOLCC
01438      THEN                                                         ELGAOLCC
01439         SET   ASCEND-DESCEND-IND-NA TO TRUE                       ELGAOLCC
01440      END-IF.                                                      ELGAOLCC
01441                                                                   ELGAOLCC
01442      IF ACCUM-FAM-OR-INDIV = ZEROS OR SPACES OR LOW-VALUES        ELGAOLCC
01443      THEN                                                         ELGAOLCC
01444         SET   FAM-OR-INDIV-NA TO TRUE                             ELGAOLCC
01445      END-IF.                                                      ELGAOLCC
01446                                                                   ELGAOLCC
01447      IF ACCUM-OPX-BASE-AMT-SOURCE-IND =                           ELGAOLCC
01448         ZEROS OR SPACES OR LOW-VALUES                             ELGAOLCC
01449      THEN                                                         ELGAOLCC
01450         SET   OPX-BASE-AMT-SOURCE-IND-NA TO TRUE                  ELGAOLCC
01451      END-IF.                                                      ELGAOLCC
01452                                                                   ELGAOLCC
01453      IF ACCUM-VALUE-QUALIFIER = ZEROS OR SPACES OR LOW-VALUES     ELGAOLCC
01454      THEN                                                         ELGAOLCC
01455         SET   VALUE-QUALIFIER-NA TO TRUE                          ELGAOLCC
01456      END-IF.                                                      ELGAOLCC
01457                                                                   ELGAOLCC
01458      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELGAOLCC
01459      THEN                                                         ELGAOLCC
01460         SET   RELATIONSHIP-IND-NA TO TRUE                         ELGAOLCC
01461      END-IF.                                                      ELGAOLCC
01462                                                                   ELGAOLCC
01463      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELGAOLCC
01464      THEN                                                         ELGAOLCC
01465         SET   AGE-LMT-FROM-IND-NA TO TRUE                         ELGAOLCC
01466      END-IF.                                                      ELGAOLCC
01467                                                                   ELGAOLCC
01468      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELGAOLCC
01469      THEN                                                         ELGAOLCC
01470         SET   AGE-LMT-TO-IND-NA TO TRUE                           ELGAOLCC
01471      END-IF.                                                      ELGAOLCC
01472                                                                   ELGAOLCC
01473      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELGAOLCC
01474      THEN                                                         ELGAOLCC
01475         SET   LMT-MANDATORY-IND-NA TO TRUE                        ELGAOLCC
01476      END-IF.                                                      ELGAOLCC
01477                                                                   ELGAOLCC
01478      IF ACCUM-CO-PAY-IND (COPAY-INDEX)                            ELGAOLCC
01479                          = ZEROS OR SPACES OR LOW-VALUES          ELGAOLCC
01480      THEN                                                         ELGAOLCC
01481         SET   CO-PAY-IND-NA (COPAY-INDEX) TO TRUE                 ELGAOLCC
01482      END-IF.                                                      ELGAOLCC
01483                                                                   ELGAOLCC
01484      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELGAOLCC
01485      THEN                                                         ELGAOLCC
01486         SET   SERVICE-GROUP-NA TO TRUE                            ELGAOLCC
01487      END-IF.                                                      ELGAOLCC
01488                                                                   ELGAOLCC
01489      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELGAOLCC
01490      THEN                                                         ELGAOLCC
01491         SET   INTERNAL-DESCRIPTOR-NA TO TRUE                      ELGAOLCC
01492      END-IF.                                                      ELGAOLCC
01493                                                                   ELGAOLCC
01494      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELGAOLCC
01495      THEN                                                         ELGAOLCC
01496         SET   DAY-FACTOR-IND-NA TO TRUE                           ELGAOLCC
01497      END-IF.                                                      ELGAOLCC
01498                                                                   ELGAOLCC
01499      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELGAOLCC
01500      THEN                                                         ELGAOLCC
01501         SET   CLAIM-LVL-ACCUM-IND-NA TO TRUE                      ELGAOLCC
01502      END-IF.                                                      ELGAOLCC
01503                                                                   ELGAOLCC
01504      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELGAOLCC
01505      THEN                                                         ELGAOLCC
01506         SET   BEN-PER-MAX-OVRD-IND-NA TO TRUE                     ELGAOLCC
01507      END-IF.                                                      ELGAOLCC
01508                                                                   ELGAOLCC
01509      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELGAOLCC
01510      THEN                                                         ELGAOLCC
01511         SET   1ST-DOLR-COVRGE-LMT-NA TO TRUE                      ELGAOLCC
01512      END-IF.                                                      ELGAOLCC
01513                                                                   ELGAOLCC
01514 /***********************************************************      ELGAOLCC
01515 *                                                          *      ELGAOLCC
01516 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER CLASS   *      ELGAOLCC
01517 *                                                          *      ELGAOLCC
01518 ************************************************************      ELGAOLCC
01519                                                                   ELGAOLCC
01520  0550-CHK-INTRNL-TAB-PROV-CLASS.                                  ELGAOLCC
01521      IF SW-HAS-IPGT                                               ELGAOLCC
01522      THEN                                                         ELGAOLCC
01523         PERFORM 0560-CHK-IPGT-PROV-CLASS                          ELGAOLCC
01524      ELSE                                                         ELGAOLCC
01525         IF SW-HAS-IBGR                                            ELGAOLCC
01526         THEN                                                      ELGAOLCC
01527            PERFORM 0640-CHK-IBGR-PROV-CLASS                       ELGAOLCC
01528         ELSE                                                      ELGAOLCC
01529            SET SW-OCCRNC-APPLIES TO TRUE                          ELGAOLCC
01530         END-IF                                                    ELGAOLCC
01531      END-IF.                                                      ELGAOLCC
01532                                                                   ELGAOLCC
01533 /***********************************************************      ELGAOLCC
01534 *                                                          *      ELGAOLCC
01535 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER SPEC    *      ELGAOLCC
01536 *                                                          *      ELGAOLCC
01537 ************************************************************      ELGAOLCC
01538                                                                   ELGAOLCC
01539  0551-CHK-INTRNL-TAB-PROV-SPEC.                                   ELGAOLCC
01540      IF SW-HAS-IPGS                                               ELGAOLCC
01541      THEN                                                         ELGAOLCC
01542         PERFORM 0561-CHK-IPGS-PROV-SPEC                           ELGAOLCC
01543 *    ELSE                                                         ELGAOLCC
01544 *       IF SW-HAS-IBGR                                            ELGAOLCC
01545 *       THEN                                                      ELGAOLCC
01546 *          PERFORM 0640-CHK-IBGR-PROV-CLASS                       ELGAOLCC
01547 *       ELSE                                                      ELGAOLCC
01548 *          SET SW-OCCRNC-APPLIES TO TRUE                          ELGAOLCC
01549 *       END-IF                                                    ELGAOLCC
01550       END-IF.                                                     ELGAOLCC
01551                                                                   ELGAOLCC
01552 /*****************************************************************ELGAOLCC
01553 *                                                                *ELGAOLCC
01554 *    CHECK IPGT INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELGAOLCC
01555 *                                                                *ELGAOLCC
01556 ******************************************************************ELGAOLCC
01557                                                                   ELGAOLCC
01558  0560-CHK-IPGT-PROV-CLASS.                                        ELGAOLCC
01559      MOVE PC-IPGT TO KWA-PROVISION-ID.                            ELGAOLCC
01560      MOVE WS-IPGT-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGAOLCC
01561      PERFORM 0650-READ-INTRNL-TAB.                                ELGAOLCC
01562      SET ADDRESS OF GX3-RECORD-AREA TO IOP-REC-PTR.               ELGAOLCC
01563      SET GX3-INDEX                  TO GX3-ENTRY-COUNT.           ELGAOLCC
01564      SET WS-MAX-GX3-INDEX           TO GX3-INDEX.                 ELGAOLCC
01565                                                                   ELGAOLCC
01566      IF GX3-ID-ARGUMENT-INCLUDED                                  ELGAOLCC
01567      THEN                                                         ELGAOLCC
01568         PERFORM 0570-CHK-INCLD-TYPE-IPGT                          ELGAOLCC
01569      ELSE                                                         ELGAOLCC
01570         PERFORM 0600-CHK-EXCLD-TYPE-IPGT                          ELGAOLCC
01571      END-IF.                                                      ELGAOLCC
01572                                                                   ELGAOLCC
01573 /*****************************************************************ELGAOLCC
01574 *                                                                *ELGAOLCC
01575 *    CHECK IPGS INTERNAL TABULAR TO DETERMINE PROVIDER SPEC      *ELGAOLCC
01576 *                                                                *ELGAOLCC
01577 ******************************************************************ELGAOLCC
01578                                                                   ELGAOLCC
01579  0561-CHK-IPGS-PROV-SPEC.                                         ELGAOLCC
01580      MOVE PC-IPGS TO KWA-PROVISION-ID.                            ELGAOLCC
01581      MOVE WS-IPGS-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGAOLCC
01582      PERFORM 0650-READ-INTRNL-TAB.                                ELGAOLCC
01583      SET ADDRESS OF GXS-RECORD-AREA TO IOP-REC-PTR.               ELGAOLCC
01584      SET GXS-INDEX                  TO GXS-ENTRY-COUNT.           ELGAOLCC
01585      SET WS-MAX-GXS-INDEX           TO GXS-INDEX.                 ELGAOLCC
01586                                                                   ELGAOLCC
01587      IF GXS-ID-ARGUMENT-INCLUDED                                  ELGAOLCC
01588      THEN                                                         ELGAOLCC
01589         PERFORM 0571-CHK-INCLD-TYPE-IPGS                          ELGAOLCC
01590      ELSE                                                         ELGAOLCC
01591         PERFORM 0601-CHK-EXCLD-TYPE-IPGS                          ELGAOLCC
01592      END-IF.                                                      ELGAOLCC
01593                                                                   ELGAOLCC
01594 ************************************************************      ELGAOLCC
01595 *                                                          *      ELGAOLCC
01596 *    CHECK INCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGAOLCC
01597 *                                                          *      ELGAOLCC
01598 ************************************************************      ELGAOLCC
01599                                                                   ELGAOLCC
01600  0570-CHK-INCLD-TYPE-IPGT.                                        ELGAOLCC
01601      SET CFT2-IDX TO 1.                                           ELGAOLCC
01602      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGAOLCC
01603          SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGAOLCC
01604       TO TRUE.                                                    ELGAOLCC
01605      PERFORM 0580-TEST-IPGT-INCLD-ENTRIES                         ELGAOLCC
01606         VARYING GX3-INDEX  FROM 1 BY 1                            ELGAOLCC
01607           UNTIL    GX3-INDEX = WS-MAX-GX3-INDEX                   ELGAOLCC
01608                 OR (    SW-INTRNL-INST-PROV-CLASS                 ELGAOLCC
01609                     AND SW-INTRNL-PROF-PROV-CLASS ).              ELGAOLCC
01610                                                                   ELGAOLCC
01611 ************************************************************      ELGAOLCC
01612 *                                                          *      ELGAOLCC
01613 *    CHECK INCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELGAOLCC
01614 *                                                          *      ELGAOLCC
01615 ************************************************************      ELGAOLCC
01616                                                                   ELGAOLCC
01617  0571-CHK-INCLD-TYPE-IPGS.                                        ELGAOLCC
01618      SET CFT9-IDX TO 1.                                           ELGAOLCC
01619      SET SW-INTRNL-NOT-PROF-PROV-SPEC                             ELGAOLCC
01620       TO TRUE.                                                    ELGAOLCC
01621      PERFORM 0581-TEST-IPGS-INCLD-ENTRIES                         ELGAOLCC
01622         VARYING GXS-INDEX  FROM 1 BY 1                            ELGAOLCC
01623           UNTIL    GXS-INDEX = WS-MAX-GXS-INDEX                   ELGAOLCC
01624                 OR (    SW-INTRNL-PROF-PROV-SPEC).                ELGAOLCC
01625                                                                   ELGAOLCC
01626 /***********************************************************      ELGAOLCC
01627 *                                                          *      ELGAOLCC
01628 *    TEST IPGT INCLUDE ENTRIES TO DETERMINE PROVIDER CLASS *      ELGAOLCC
01629 *                                                          *      ELGAOLCC
01630 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELGAOLCC
01631 *          CFT2 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELGAOLCC
01632 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELGAOLCC
01633 *          OF THE IPGT TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELGAOLCC
01634 *                                                          *      ELGAOLCC
01635 ************************************************************      ELGAOLCC
01636                                                                   ELGAOLCC
01637  0580-TEST-IPGT-INCLD-ENTRIES.                                    ELGAOLCC
01638      PERFORM WITH TEST BEFORE                                     ELGAOLCC
01639         UNTIL    SW-OCCRNC-APPLIES                                ELGAOLCC
01640               OR   CFT2-PT (CFT2-IDX)                             ELGAOLCC
01641                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELGAOLCC
01642               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELGAOLCC
01643         IF   GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELGAOLCC
01644            = CFT2-PT (CFT2-IDX)                                   ELGAOLCC
01645         THEN                                                      ELGAOLCC
01646 *    -- TEST PROVIDER CLASS                                       ELGAOLCC
01647            EVALUATE TRUE                                          ELGAOLCC
01648               WHEN CFT2-PT-INST (CFT2-IDX)                        ELGAOLCC
01649                  SET SW-INTRNL-INST-PROV-CLASS TO TRUE            ELGAOLCC
01650                  IF SRP-ACCUM-PROV-CLASS-INST                     ELGAOLCC
01651                  THEN                                             ELGAOLCC
01652                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGAOLCC
01653                  END-IF                                           ELGAOLCC
01654               WHEN CFT2-PT-PROF (CFT2-IDX)                        ELGAOLCC
01655                  SET SW-INTRNL-PROF-PROV-CLASS TO TRUE            ELGAOLCC
01656                  IF SRP-ACCUM-PROV-CLASS-PROF                     ELGAOLCC
01657                  THEN                                             ELGAOLCC
01658                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGAOLCC
01659                  END-IF                                           ELGAOLCC
01660            END-EVALUATE                                           ELGAOLCC
01661         ELSE                                                      ELGAOLCC
01662            CONTINUE                                               ELGAOLCC
01663         END-IF                                                    ELGAOLCC
01664 *    -- BUMP TO NEXT CFT2 TABLE ENTRY                             ELGAOLCC
01665         SET CFT2-IDX UP BY 1                                      ELGAOLCC
01666      END-PERFORM.                                                 ELGAOLCC
01667                                                                   ELGAOLCC
01668 /***********************************************************      ELGAOLCC
01669 *                                                          *      ELGAOLCC
01670 *    TEST IPGS INCLUDE ENTRIES TO DETERMINE PROVIDER SPEC  *      ELGAOLCC
01671 *                                                          *      ELGAOLCC
01672 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELGAOLCC
01673 *          CFT9 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELGAOLCC
01674 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELGAOLCC
01675 *          OF THE IPGS TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELGAOLCC
01676 *                                                          *      ELGAOLCC
01677 ************************************************************      ELGAOLCC
01678                                                                   ELGAOLCC
01679  0581-TEST-IPGS-INCLD-ENTRIES.                                    ELGAOLCC
01680      PERFORM WITH TEST BEFORE                                     ELGAOLCC
01681         UNTIL    SW-OCCRNC-APPLIES                                ELGAOLCC
01682               OR   CFT9-PT (CFT9-IDX)                             ELGAOLCC
01683                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELGAOLCC
01684               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELGAOLCC
01685         IF   GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELGAOLCC
01686            = CFT9-PT (CFT9-IDX)                                   ELGAOLCC
01687 *    -- TEST PROVIDER SPEC                                        ELGAOLCC
01688          IF CFT9-PT-PROF (CFT9-IDX)                               ELGAOLCC
01689            SET SW-INTRNL-PROF-PROV-SPEC TO TRUE                   ELGAOLCC
01690              IF SRP-ACCUM-PROV-SPEC-PROF                          ELGAOLCC
01691                SET SW-OCCRNC-APPLIES TO TRUE                      ELGAOLCC
01692              END-IF                                               ELGAOLCC
01693         ELSE                                                      ELGAOLCC
01694            CONTINUE                                               ELGAOLCC
01695         END-IF                                                    ELGAOLCC
01696         END-IF                                                    ELGAOLCC
01697 *    -- BUMP TO NEXT CFT9 TABLE ENTRY                             ELGAOLCC
01698         SET CFT9-IDX UP BY 1                                      ELGAOLCC
01699      END-PERFORM.                                                 ELGAOLCC
01700                                                                   ELGAOLCC
01701 /***********************************************************      ELGAOLCC
01702 *                                                          *      ELGAOLCC
01703 *    CHECK EXCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGAOLCC
01704 *                                                          *      ELGAOLCC
01705 ************************************************************      ELGAOLCC
01706                                                                   ELGAOLCC
01707  0600-CHK-EXCLD-TYPE-IPGT.                                        ELGAOLCC
01708                                                                   ELGAOLCC
01709 * -- INITIALIZE CFT2 TABLE TO INCLUDE ALL PROVIDER TYPES          ELGAOLCC
01710      PERFORM WITH TEST BEFORE                                     ELGAOLCC
01711         VARYING CFT2-IDX FROM 1 BY 1                              ELGAOLCC
01712           UNTIL CFT2-IDX > CFT2-NBR-TBL-ENTRIES                   ELGAOLCC
01713         SET  CFT2-PT-INCLUDE (CFT2-IDX) TO TRUE                   ELGAOLCC
01714      END-PERFORM.                                                 ELGAOLCC
01715                                                                   ELGAOLCC
01716 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGT                 ELGAOLCC
01717      SET  CFT2-IDX TO 1.                                          ELGAOLCC
01718      PERFORM 0610-TAG-EXCLD-IPGT-ENTRIES                          ELGAOLCC
01719         VARYING GX3-INDEX FROM 1 BY 1                             ELGAOLCC
01720           UNTIL GX3-INDEX = WS-MAX-GX3-INDEX.                     ELGAOLCC
01721                                                                   ELGAOLCC
01722 * -- CHECK CFT2 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELGAOLCC
01723      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGAOLCC
01724          SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGAOLCC
01725       TO TRUE.                                                    ELGAOLCC
01726      PERFORM 0630-CHK-CFT2-NOT-EXCLD                              ELGAOLCC
01727         VARYING CFT2-IDX FROM 1 BY 1                              ELGAOLCC
01728           UNTIL    (    SW-INTRNL-INST-PROV-CLASS                 ELGAOLCC
01729                     AND SW-INTRNL-PROF-PROV-CLASS )               ELGAOLCC
01730                 OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES.               ELGAOLCC
01731                                                                   ELGAOLCC
01732 /***********************************************************      ELGAOLCC
01733 *                                                          *      ELGAOLCC
01734 *    CHECK EXCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELGAOLCC
01735 *                                                          *      ELGAOLCC
01736 ************************************************************      ELGAOLCC
01737                                                                   ELGAOLCC
01738  0601-CHK-EXCLD-TYPE-IPGS.                                        ELGAOLCC
01739                                                                   ELGAOLCC
01740 * -- INITIALIZE CFT9 TABLE TO INCLUDE ALL PROVIDER SPEC           ELGAOLCC
01741      PERFORM WITH TEST BEFORE                                     ELGAOLCC
01742         VARYING CFT9-IDX FROM 1 BY 1                              ELGAOLCC
01743           UNTIL CFT9-IDX > CFT9-NBR-TBL-ENTRIES                   ELGAOLCC
01744         SET  CFT9-PT-INCLUDE (CFT9-IDX) TO TRUE                   ELGAOLCC
01745      END-PERFORM.                                                 ELGAOLCC
01746                                                                   ELGAOLCC
01747 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGS                 ELGAOLCC
01748      SET  CFT9-IDX TO 1.                                          ELGAOLCC
01749      PERFORM 0611-TAG-EXCLD-IPGS-ENTRIES                          ELGAOLCC
01750         VARYING GXS-INDEX FROM 1 BY 1                             ELGAOLCC
01751           UNTIL GXS-INDEX = WS-MAX-GXS-INDEX.                     ELGAOLCC
01752                                                                   ELGAOLCC
01753 * -- CHECK CFT2 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELGAOLCC
01754      SET SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGAOLCC
01755       TO TRUE.                                                    ELGAOLCC
01756      PERFORM 0631-CHK-CFT9-NOT-EXCLD                              ELGAOLCC
01757         VARYING CFT9-IDX FROM 1 BY 1                              ELGAOLCC
01758           UNTIL    (    SW-INTRNL-PROF-PROV-CLASS )               ELGAOLCC
01759                 OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES.               ELGAOLCC
01760                                                                   ELGAOLCC
01761 /***********************************************************      ELGAOLCC
01762 *                                                          *      ELGAOLCC
01763 *    TAG EXCLUDED IPGT ENTRIES IN CFT2                     *      ELGAOLCC
01764 *                                                          *      ELGAOLCC
01765 ************************************************************      ELGAOLCC
01766                                                                   ELGAOLCC
01767  0610-TAG-EXCLD-IPGT-ENTRIES.                                     ELGAOLCC
01768      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELGAOLCC
01769      PERFORM WITH TEST BEFORE                                     ELGAOLCC
01770         UNTIL    SW-ENTRY-FOUND                                   ELGAOLCC
01771               OR   CFT2-PT (CFT2-IDX)                             ELGAOLCC
01772                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELGAOLCC
01773               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELGAOLCC
01774         IF   CFT2-PT(CFT2-IDX)                                    ELGAOLCC
01775            = GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELGAOLCC
01776         THEN                                                      ELGAOLCC
01777            SET SW-ENTRY-FOUND TO TRUE                             ELGAOLCC
01778            SET CFT2-PT-EXCLUDE (CFT2-IDX) TO TRUE                 ELGAOLCC
01779         END-IF                                                    ELGAOLCC
01780         SET CFT2-IDX UP BY 1                                      ELGAOLCC
01781      END-PERFORM.                                                 ELGAOLCC
01782                                                                   ELGAOLCC
01783 /***********************************************************      ELGAOLCC
01784 *                                                          *      ELGAOLCC
01785 *    TAG EXCLUDED IPGS ENTRIES IN CFT9                     *      ELGAOLCC
01786 *                                                          *      ELGAOLCC
01787 ************************************************************      ELGAOLCC
01788                                                                   ELGAOLCC
01789  0611-TAG-EXCLD-IPGS-ENTRIES.                                     ELGAOLCC
01790      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELGAOLCC
01791      PERFORM WITH TEST BEFORE                                     ELGAOLCC
01792         UNTIL    SW-ENTRY-FOUND                                   ELGAOLCC
01793               OR   CFT9-PT (CFT9-IDX)                             ELGAOLCC
01794                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELGAOLCC
01795               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELGAOLCC
01796         IF   CFT9-PT(CFT9-IDX)                                    ELGAOLCC
01797            = GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELGAOLCC
01798         THEN                                                      ELGAOLCC
01799            SET SW-ENTRY-FOUND TO TRUE                             ELGAOLCC
01800            SET CFT9-PT-EXCLUDE (CFT9-IDX) TO TRUE                 ELGAOLCC
01801         END-IF                                                    ELGAOLCC
01802         SET CFT9-IDX UP BY 1                                      ELGAOLCC
01803      END-PERFORM.                                                 ELGAOLCC
01804                                                                   ELGAOLCC
01805 /*****************************************************************ELGAOLCC
01806 *                                                                *ELGAOLCC
01807 *    CHECK CFT2 FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDED     *ELGAOLCC
01808 *                                                                *ELGAOLCC
01809 ******************************************************************ELGAOLCC
01810                                                                   ELGAOLCC
01811  0630-CHK-CFT2-NOT-EXCLD.                                         ELGAOLCC
01812      IF CFT2-PT-INCLUDE (CFT2-IDX)                                ELGAOLCC
01813      THEN                                                         ELGAOLCC
01814         EVALUATE TRUE                                             ELGAOLCC
01815            WHEN CFT2-PT-INST (CFT2-IDX)                           ELGAOLCC
01816               SET SW-INTRNL-INST-PROV-CLASS TO TRUE               ELGAOLCC
01817               IF SRP-ACCUM-PROV-CLASS-INST                        ELGAOLCC
01818               THEN                                                ELGAOLCC
01819                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGAOLCC
01820               END-IF                                              ELGAOLCC
01821            WHEN CFT2-PT-PROF (CFT2-IDX)                           ELGAOLCC
01822               SET SW-INTRNL-PROF-PROV-CLASS TO TRUE               ELGAOLCC
01823               IF SRP-ACCUM-PROV-CLASS-PROF                        ELGAOLCC
01824               THEN                                                ELGAOLCC
01825                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGAOLCC
01826               END-IF                                              ELGAOLCC
01827            END-EVALUATE                                           ELGAOLCC
01828      END-IF.                                                      ELGAOLCC
01829                                                                   ELGAOLCC
01830 /*****************************************************************ELGAOLCC
01831 *                                                                *ELGAOLCC
01832 *    CHECK CFT9 FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED     * ELGAOLCC
01833 *                                                                *ELGAOLCC
01834 ******************************************************************ELGAOLCC
01835                                                                   ELGAOLCC
01836  0631-CHK-CFT9-NOT-EXCLD.                                         ELGAOLCC
01837      IF CFT9-PT-INCLUDE (CFT9-IDX)                                ELGAOLCC
01838        IF CFT9-PT-PROF (CFT9-IDX)                                 ELGAOLCC
01839           SET SW-INTRNL-PROF-PROV-SPEC  TO TRUE                   ELGAOLCC
01840           IF SRP-ACCUM-PROV-SPEC-PROF                             ELGAOLCC
01841             SET SW-OCCRNC-APPLIES TO TRUE                         ELGAOLCC
01842           END-IF                                                  ELGAOLCC
01843      END-IF.                                                      ELGAOLCC
01844                                                                   ELGAOLCC
01845 /*****************************************************************ELGAOLCC
01846 *                                                                *ELGAOLCC
01847 *    CHECK IBGR INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELGAOLCC
01848 *                                                                *ELGAOLCC
01849 ******************************************************************ELGAOLCC
01850                                                                   ELGAOLCC
01851  0640-CHK-IBGR-PROV-CLASS.                                        ELGAOLCC
01852      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGAOLCC
01853          SW-INTRNL-NOT-PROF-PROV-CLASS TO TRUE.                   ELGAOLCC
01854      MOVE PC-IBGR TO KWA-PROVISION-ID.                            ELGAOLCC
01855      MOVE WS-IBGR-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGAOLCC
01856      PERFORM 0650-READ-INTRNL-TAB.                                ELGAOLCC
01857      SET ADDRESS OF GX1-RECORD-AREA TO IOP-REC-PTR.               ELGAOLCC
01858      SET GX1-INDEX                  TO GX1-ENTRY-COUNT.           ELGAOLCC
01859      SET WS-MAX-GX1-INDEX           TO GX1-INDEX.                 ELGAOLCC
01860                                                                   ELGAOLCC
01861      IF GX1-ID-ARGUMENT-EXCLUDED                                  ELGAOLCC
01862      THEN                                                         ELGAOLCC
01863 *    -- ASSUME THAT IBGR WOULD NOT EXCLUDE ALL OF ANY PROVIDER    ELGAOLCC
01864 *       CLASS (I.E., BOTH TYPES APPLY).                           ELGAOLCC
01865         SET SW-OCCRNC-APPLIES                                     ELGAOLCC
01866             SW-INTRNL-INST-PROV-CLASS                             ELGAOLCC
01867             SW-INTRNL-PROF-PROV-CLASS                             ELGAOLCC
01868          TO TRUE                                                  ELGAOLCC
01869      ELSE                                                         ELGAOLCC
01870         PERFORM 0690-CHK-INCLD-TYPE-IBGR                          ELGAOLCC
01871      END-IF.                                                      ELGAOLCC
01872                                                                   ELGAOLCC
01873 /***********************************************************      ELGAOLCC
01874 *                                                          *      ELGAOLCC
01875 *    READ THE INTERNAL TABULAR RECORD                      *      ELGAOLCC
01876 *                                                          *      ELGAOLCC
01877 ************************************************************      ELGAOLCC
01878                                                                   ELGAOLCC
01879  0650-READ-INTRNL-TAB.                                            ELGAOLCC
01880      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELGAOLCC
01881      SET IOP-RD TO TRUE.                                          ELGAOLCC
01882      SET IOP-FCQ-NONE TO TRUE.                                    ELGAOLCC
01883      SET IOP-KVQ-EQ TO TRUE.                                      ELGAOLCC
01884      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELGAOLCC
01885      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGAOLCC
01886      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGAOLCC
01887      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGAOLCC
01888                                                                   ELGAOLCC
01889      EVALUATE TRUE                                                ELGAOLCC
01890         WHEN IOP-RC-OK                                            ELGAOLCC
01891            CONTINUE                                               ELGAOLCC
01892         WHEN IOP-RC-NOTFND                                        ELGAOLCC
01893            SET CIA-AB-NOTFND-GCTABULR TO TRUE                     ELGAOLCC
01894            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELGAOLCC
01895         WHEN OTHER                                                ELGAOLCC
01896             SET CIA-AB-CRITIO TO TRUE                             ELGAOLCC
01897             EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC           ELGAOLCC
01898      END-EVALUATE.                                                ELGAOLCC
01899                                                                   ELGAOLCC
01900 /*****************************************************************ELGAOLCC
01901 *                                                                *ELGAOLCC
01902 *    CHECK INCLUDE TYPE IBGR TO DETERMINE PROVIDER CLASS         *ELGAOLCC
01903 *                                                                *ELGAOLCC
01904 ******************************************************************ELGAOLCC
01905                                                                   ELGAOLCC
01906  0690-CHK-INCLD-TYPE-IBGR.                                        ELGAOLCC
01907      PERFORM WITH TEST BEFORE                                     ELGAOLCC
01908         VARYING GX1-INDEX FROM 1 BY 1                             ELGAOLCC
01909           UNTIL    GX1-INDEX = WS-MAX-GX1-INDEX                   ELGAOLCC
01910                 OR (    SW-INTRNL-INST-PROV-CLASS                 ELGAOLCC
01911                     AND SW-INTRNL-PROF-PROV-CLASS )               ELGAOLCC
01912         MOVE GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)                ELGAOLCC
01913           TO WS-PROVISION-ARGUMENT                                ELGAOLCC
01914         EVALUATE TRUE                                             ELGAOLCC
01915            WHEN INST-CLASS                                        ELGAOLCC
01916               SET SW-INTRNL-INST-PROV-CLASS TO TRUE               ELGAOLCC
01917               IF SRP-ACCUM-PROV-CLASS-INST                        ELGAOLCC
01918               THEN                                                ELGAOLCC
01919                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGAOLCC
01920               END-IF                                              ELGAOLCC
01921            WHEN PROF-CLASS                                        ELGAOLCC
01922               SET SW-INTRNL-PROF-PROV-CLASS TO TRUE               ELGAOLCC
01923               IF SRP-ACCUM-PROV-CLASS-PROF                        ELGAOLCC
01924               THEN                                                ELGAOLCC
01925                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGAOLCC
01926               END-IF                                              ELGAOLCC
01927         END-EVALUATE                                              ELGAOLCC
01928      END-PERFORM.                                                 ELGAOLCC
01929                                                                   ELGAOLCC
01930 /***********************************************************      ELGAOLCC
01931 *                                                          *      ELGAOLCC
01932 *        ADD ACCUM OCCURRENCE TO FILE                      *      ELGAOLCC
01933 *                                                          *      ELGAOLCC
01934 ************************************************************      ELGAOLCC
01935                                                                   ELGAOLCC
01936  0710-WRITE-EXTRACT-RECORD.                                       ELGAOLCC
01937      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGAOLCC
01938      SET  IOP-ADD TO TRUE.                                        ELGAOLCC
01939      SET  IOP-FCQ-NONE TO TRUE.                                   ELGAOLCC
01940      SET  IOP-KVQ-NONE TO TRUE.                                   ELGAOLCC
01941      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGAOLCC
01942                                                                   ELGAOLCC
01943 /***********************************************************      ELGAOLCC
01944 *                                                          *      ELGAOLCC
01945 *    ESTABLISH ADDRESSABILITY OF THE TABULAR FILE          *      ELGAOLCC
01946 *                                                          *      ELGAOLCC
01947 ************************************************************      ELGAOLCC
01948                                                                   ELGAOLCC
01949  9060-EST-ADR-TABULAR-FILE.                                       ELGAOLCC
01950      SET  CIA-GCTABULR-DDN TO TRUE.                               ELGAOLCC
01951      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOLCC
01952         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGAOLCC
01953         END-CALL.                                                 ELGAOLCC
01954      IF CIA-RC-PTR-NULL                                           ELGAOLCC
01955      THEN                                                         ELGAOLCC
01956         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGAOLCC
01957         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGAOLCC
01958      END-IF.                                                      ELGAOLCC
01959                                                                   ELGAOLCC
01960 /***********************************************************      ELGAOLCC
01961 *                                                          *      ELGAOLCC
01962 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELGAOLCC
01963 *                                                          *      ELGAOLCC
01964 ************************************************************      ELGAOLCC
01965                                                                   ELGAOLCC
01966  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELGAOLCC
01967      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGAOLCC
01968      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOLCC
01969         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGAOLCC
01970         END-CALL.                                                 ELGAOLCC
01971      IF CIA-RC-PTR-NULL                                           ELGAOLCC
01972         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGAOLCC
01973         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGAOLCC
01974      END-IF.                                                      ELGAOLCC
