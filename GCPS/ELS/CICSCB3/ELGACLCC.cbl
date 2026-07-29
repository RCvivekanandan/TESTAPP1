00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGACLCC
00003  PROGRAM-ID.        ELGACLCC.                                        LV002
00004                                                                   ELGACLCC
00005  AUTHOR.            LUCY TORRES.                                  ELGACLCC
00006                     RICHARD J. LUKETICH (RE-WRITE).               ELGACLCC
00007                                                                   ELGACLCC
00008  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELGACLCC
00009                     A MUTUAL LEGAL RESERVE COMPANY                ELGACLCC
00010                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELGACLCC
00011                     233 N. MICHIGAN AVE                           ELGACLCC
00012                     CHICAGO, ILLINOIS 60601                       ELGACLCC
00013                                                                   ELGACLCC
00014  DATE-WRITTEN.      03-JUN-1987.                                  ELGACLCC
00015                     03-JAN-1992 (RE-WRITE).                       ELGACLCC
00016                                                                   ELGACLCC
00017  DATE-COMPILED.                                                   ELGACLCC
00018                                                                   ELGACLCC
00019  SECURITY.          COPYRIGHT 1986, 1992,                         ELGACLCC
00020                     HEALTH CARE SERVICE CORPORATION               ELGACLCC
00021                                                                   ELGACLCC
00022  ENVIRONMENT DIVISION.                                            ELGACLCC
00023                                                                   ELGACLCC
00024  CONFIGURATION SECTION.                                           ELGACLCC
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELGACLCC
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELGACLCC
00027                                                                   ELGACLCC
00028 /*****************************************************************ELGACLCC
00029 *                                                                *ELGACLCC
00030 *  ELGACLCC - ELS:  SELECTS #ACL (COINSURANCE) ACCUMULATORS AND  *ELGACLCC
00031 *                   SETUPS THE INFORMATION TO BE PROCESSED BY    *ELGACLCC
00032 *                   THE COINSURANCE GENERATOR MODULE.  THE       *ELGACLCC
00033 *                   ACCUMS ARE SELECTED FROM THE GROUP SPECIFIC  *ELGACLCC
00034 *                   AND CONTRACT LEVEL PROCESSING.               *ELGACLCC
00035 *                                                                *ELGACLCC
00036 ******************************************************************ELGACLCC
00037 *                                                                *ELGACLCC
00038 *                      MAINTENANCE HISTORY                       *ELGACLCC
00039 *                                                                *ELGACLCC
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELGACLCC
00041 * ----- ----------- --- ----- ---------------------------------- *ELGACLCC
00042 * 01.00 03-JUN-1987 LET       CREATED                            *ELGACLCC
00043 * 01.01 25-SEP-1987 LET       ADDED DEFINITION DATA FIELD        *ELGACLCC
00044 *                                                                *ELGACLCC
00045 * 01.02 17-NOV-1987 REB       MADE CHANGES TO CORRESPOND TO NEW  *ELGACLCC
00046 *                             VERSION OF COPYBOOK ELSACUMC.      *ELGACLCC
00047 *                                                                *ELGACLCC
00048 * 01.07    SEP-1991 RKH    1. ADDED LOGIC FOR:                   *ELGACLCC
00049 *    ISSR #12010                A.  NEW PATIENT AGE FIELDS       *ELGACLCC
00050 *                               B.  RELATIONSHIP IND VALUE       *ELGACLCC
00051 *                          2. REVISE LOGIC TO LOAD INT ACCUMS    *ELGACLCC
00052 *                             INTO VARIABLE LEVEL TABLE          *ELGACLCC
00053 *                          3. ADDED COPYBOOKS :                  *ELGACLCC
00054 *                               A. GCTIBGR   - IBGR TAB          *ELGACLCC
00055 *                               B. GCTIPGT   - IPGT TAB          *ELGACLCC
00056 *                               C. ELSCFTB2  - PROVIDER TYPE     *ELGACLCC
00057 *                                         COMPARE TABLE          *ELGACLCC
00058 *                          4. ADD LOGIC TO INSPECT #IPGT AND     *ELGACLCC
00059 *                             #IBGR INT TABS TO DETERMINE IF     *ELGACLCC
00060 *                             AN OCCURRANCE IS THE SELECTED      *ELGACLCC
00061 *                             PROVIDER CLASS.                    *ELGACLCC
00062 *                                                                *ELGACLCC
00063 * 02.00 03-JAN-1992 RJL       LOGIC RESTRUCTURED, ADDED          *ELGACLCC
00064 *                             MAXIMUM BASE AMOUNT SOURCE IND.    *ELGACLCC
00065 *                                                                *ELGACLCC
00066 * 02.01 23-JAN-1992 JPB       CLONED FROM ELGABM, CHANGED LOGIC  *ELGACLCC
00067 *                             FROM MAXIMUMS TO COINSURANCE.      *ELGACLCC
00068 *                                                                *ELGACLCC
00069 * 02.02 03-MAR-1992 JPB       CLONED FROM ELTACL, ADDED LOGIC FOR*ELGACLCC
00070 *                             COST CONTAINMENT.                  *ELGACLCC
00071 *                                                                *ELGACLCC
00072 * 02.03 01-SEP-1994 AKK       ADDED RPO - MISSED IT WHEN RPO     *ELGACLCC
00073 *                             WAS ADDED.                         *ELGACLCC
00074 *                                                                *ELGACLCC
00075 * 02.04 24-FEB-1995 AKK       ADDED CPO.                         *ELGACLCC
00076 *                                                                *ELGACLCC
00077 * 02.05 16-FEB-1996 AKK       ADDED CBL AND PAN.                 *ELGACLCC
00078 *                                                                *ELGACLCC
00079 * 02.06 02-DEC-1998 AKK       ADDED ACP.                         *ELGACLCC
00080 *                                                                *ELGACLCC
00081 * 02.07 11-MAR-1999 AKK       ADDED BAE.                         *ELGACLCC
00082 *                                                                *ELGACLCC
00083 * 02.08 21-AUG-2000 AKK       ADDED SUPPORT FOR #IPGS.           *ELGACLCC
00084 *                                                                *ELGACLCC
00085 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGACLCC
00086 *                                                                *ELGACLCC
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00087 ******************************************************************ELGACLCC
00088      TITLE  'ELGACLCC          WORKING STORAGE'.                  ELGACLCC
00089  DATA DIVISION.                                                   ELGACLCC
00090                                                                   ELGACLCC
00091  WORKING-STORAGE SECTION.                                         ELGACLCC
00092                                                                   ELGACLCC
00093  01  SWITCHES.                                                    ELGACLCC
00094      02                                      PICTURE  X(01).      ELGACLCC
00095         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELGACLCC
00096         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELGACLCC
00097      02 COST-CONTAINMENT-APPLICABILITY       PICTURE  X(01).      ELGACLCC
00098         88 SW-CC-IND-APPLIES                 VALUE 'Y'.           ELGACLCC
00099         88 SW-CC-IND-DOES-NOT-APPLY          VALUE 'N'.           ELGACLCC
00100      02                                      PICTURE  X(01).      ELGACLCC
00101         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELGACLCC
00102         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELGACLCC
00103      02                                      PICTURE  X(01).      ELGACLCC
00104         88 SW-HAS-IBGR                       VALUE 'Y'.           ELGACLCC
00105         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELGACLCC
00106      02                                      PICTURE  X(01).      ELGACLCC
00107         88 SW-HAS-IDGD                       VALUE 'Y'.           ELGACLCC
00108         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELGACLCC
00109      02                                      PICTURE  X(01).      ELGACLCC
00110         88 SW-HAS-IPGN                       VALUE 'Y'.           ELGACLCC
00111         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELGACLCC
00112      02                                      PICTURE  X(01).      ELGACLCC
00113         88 SW-HAS-IPGP                       VALUE 'Y'.           ELGACLCC
00114         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELGACLCC
00115      02                                      PICTURE  X(01).      ELGACLCC
00116         88 SW-HAS-IPGT                       VALUE 'Y'.           ELGACLCC
00117         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELGACLCC
00118      02                                      PICTURE  X(01).      ELGACLCC
00119         88 SW-HAS-IPGS                       VALUE 'Y'.           ELGACLCC
00120         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELGACLCC
00121      02                                      PICTURE  X(01).      ELGACLCC
00122         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELGACLCC
00123         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELGACLCC
00124      02                                      PICTURE  X(01).      ELGACLCC
00125         88 SW-DUP-IBGR                       VALUE 'D'.           ELGACLCC
00126         88 SW-UNQ-IBGR                       VALUE 'U'.           ELGACLCC
00127      02                                      PICTURE  X(01).      ELGACLCC
00128         88 SW-DUP-IDGD                       VALUE 'D'.           ELGACLCC
00129         88 SW-UNQ-IDGD                       VALUE 'U'.           ELGACLCC
00130      02                                      PICTURE  X(01).      ELGACLCC
00131         88 SW-DUP-IPGN                       VALUE 'D'.           ELGACLCC
00132         88 SW-UNQ-IPGN                       VALUE 'U'.           ELGACLCC
00133      02                                      PICTURE  X(01).      ELGACLCC
00134         88 SW-DUP-IPGP                       VALUE 'D'.           ELGACLCC
00135         88 SW-UNQ-IPGP                       VALUE 'U'.           ELGACLCC
00136      02                                      PICTURE  X(01).      ELGACLCC
00137         88 SW-DUP-IPGT                       VALUE 'D'.           ELGACLCC
00138         88 SW-UNQ-IPGT                       VALUE 'U'.           ELGACLCC
00139      02                                      PICTURE  X(01).      ELGACLCC
00140         88 SW-DUP-IPGS                       VALUE 'D'.           ELGACLCC
00141         88 SW-UNQ-IPGS                       VALUE 'U'.           ELGACLCC
00142      02                                      PICTURE  X(01).      ELGACLCC
00143         88 SW-INTRNL-INST-PROV-CLASS         VALUE 'Y'.           ELGACLCC
00144         88 SW-INTRNL-NOT-INST-PROV-CLASS     VALUE 'N'.           ELGACLCC
00145         88 SW-INTRNL-INST-CLASS-NOT-DET      VALUE 'X'.           ELGACLCC
00146      02                                      PICTURE  X(01).      ELGACLCC
00147         88 SW-INTRNL-PROF-PROV-CLASS         VALUE 'Y'.           ELGACLCC
00148         88 SW-INTRNL-NOT-PROF-PROV-CLASS     VALUE 'N'.           ELGACLCC
00149         88 SW-INTRNL-PROF-CLASS-NOT-DET      VALUE 'X'.           ELGACLCC
00150      02                                      PICTURE  X(01).      ELGACLCC
00151         88 SW-INTRNL-PROF-PROV-SPEC          VALUE 'Y'.           ELGACLCC
00152         88 SW-INTRNL-NOT-PROF-PROV-SPEC      VALUE 'N'.           ELGACLCC
00153         88 SW-INTRNL-PROF-SPEC-NOT-DET      VALUE 'X'.            ELGACLCC
00154      02                                      PICTURE  X(01).      ELGACLCC
00155         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELGACLCC
00156         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELGACLCC
00157      02                                      PICTURE  X(01).      ELGACLCC
00158         88 SW-MATCHING-ENTRY-FOUND           VALUE 'Y'.           ELGACLCC
00159         88 SW-MATCHING-ENTRY-NOT-FOUND       VALUE 'N'.           ELGACLCC
00160      02                                      PICTURE  X(01).      ELGACLCC
00161         88 SW-SORT-COMPLETED                 VALUE 'Y'.           ELGACLCC
00162         88 SW-SORT-NOT-COMPLETED             VALUE 'N'.           ELGACLCC
00163                                                                   ELGACLCC
00164  01  WS-PROVISION-ARGUMENT.                                       ELGACLCC
00165      02                          PICTURE  X(05).                  ELGACLCC
00166      02 WS-PROVISION-CLASS       PICTURE  X(01).                  ELGACLCC
00167         88 INST-CLASS            VALUE 'A', 'B', 'W'.             ELGACLCC
00168         88 PROF-CLASS            VALUE 'C', 'D', 'E'.             ELGACLCC
00169                                                                   ELGACLCC
00170  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELGACLCC
00171      88 WS-LOB-INST              VALUE '1'.                       ELGACLCC
00172      88 WS-LOB-PROF              VALUE '2'.                       ELGACLCC
00173      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELGACLCC
00174      88 WS-LOB-BOTH              VALUE '3', '4', '5', '6', '7'.   ELGACLCC
00175                                                                   ELGACLCC
00176  01  PROGRAM-CONSTANTS.                                           ELGACLCC
00177      02 PC-ACL                   PICTURE  X(06) VALUE '#ACL  '.   ELGACLCC
00178      02 PC-GCT-MAX-SUB           PICTURE S9(04) COMP.             ELGACLCC
00179      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELGACLCC
00180      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELGACLCC
00181      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELGACLCC
00182      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELGACLCC
00183      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELGACLCC
00184      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGT '.   ELGACLCC
00185      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELGACLCC
00186                                                                   ELGACLCC
00187  01  WS-SUBTOPIC-TYPE            PIC  X(16) VALUE SPACES.         ELGACLCC
00188      88  SUBTOPIC-ATCP                      VALUE 'ATCP'.         ELGACLCC
00189      88  SUBTOPIC-BAE                       VALUE 'BAE'.          ELGACLCC
00190      88  SUBTOPIC-CBL                       VALUE 'CBL'.          ELGACLCC
00191      88  SUBTOPIC-CPO                       VALUE 'CPO'.          ELGACLCC
00192      88  SUBTOPIC-HOSP                      VALUE 'HOSP'.         ELGACLCC
00193      88  SUBTOPIC-PAT                       VALUE 'PAT'.          ELGACLCC
00194      88  SUBTOPIC-MCN                       VALUE 'MCN'.          ELGACLCC
00195      88  SUBTOPIC-MOPS                      VALUE 'MOPS'.         ELGACLCC
00196      88  SUBTOPIC-MASOP                     VALUE 'MASOP'.        ELGACLCC
00197      88  SUBTOPIC-WEEKEND                   VALUE 'WEEKEND'.      ELGACLCC
00198      88  SUBTOPIC-MONDIS                    VALUE 'MONDIS'.       ELGACLCC
00199      88  SUBTOPIC-IOB                       VALUE 'IOB'.          ELGACLCC
00200      88  SUBTOPIC-PAR                       VALUE 'PAR'.          ELGACLCC
00201      88  SUBTOPIC-MEDNEC                    VALUE 'MEDNEC'.       ELGACLCC
00202      88  SUBTOPIC-MSA                       VALUE 'MSA'.          ELGACLCC
00203      88  SUBTOPIC-PAN                       VALUE 'PAN'.          ELGACLCC
00204      88  SUBTOPIC-PPO                       VALUE 'PPO'.          ELGACLCC
00205      88  SUBTOPIC-EMH                       VALUE 'EMH'.          ELGACLCC
00206      88  SUBTOPIC-POS                       VALUE 'POS'.          ELGACLCC
00207      88  SUBTOPIC-MHSC                      VALUE 'MHSC'.         ELGACLCC
00208      88  SUBTOPIC-RPO                       VALUE 'RPO'.          ELGACLCC
00209                                                                   ELGACLCC
00210  01  WS-COST-CONTAIN-IND         PIC  X(01) VALUE SPACES.         ELGACLCC
00211      88  CC-ATCP                            VALUE  'A' 'H'.       ELGACLCC
00212      88  CC-BAE                             VALUE  'N'.           ELGACLCC
00213      88  CC-CPO                             VALUE  'K'.           ELGACLCC
00214      88  CC-CBL                             VALUE  'J'.           ELGACLCC
00215      88  CC-HOSP                            VALUE  'B' 'F' .      ELGACLCC
00216      88  CC-PAT                             VALUE  'D'.           ELGACLCC
00217      88  CC-MCN                             VALUE  'M'.           ELGACLCC
00218      88  CC-MOPS                            VALUE  '1' 'F' 'C'.   ELGACLCC
00219      88  CC-MASOP                           VALUES '2' 'E' 'G'.   ELGACLCC
00220      88  CC-WEEKEND                         VALUE  '3'.           ELGACLCC
00221      88  CC-MONDIS                          VALUE  '4'.           ELGACLCC
00222      88  CC-IOB                             VALUE  '5'.           ELGACLCC
00223      88  CC-PAR                             VALUE  '6' 'E' 'G'.   ELGACLCC
00224      88  CC-MEDNEC                          VALUE  '7'.           ELGACLCC
00225      88  CC-MSA                             VALUE  '8'.           ELGACLCC
00226      88  CC-PAN                             VALUE  'L'.           ELGACLCC
00227      88  CC-PPO                             VALUE  '9' 'G'.       ELGACLCC
00228      88  CC-EMH                             VALUE  'I'.           ELGACLCC
00229      88  CC-POS                             VALUE  'P'.           ELGACLCC
00230      88  CC-MHSC                            VALUE  'S'.           ELGACLCC
00231      88  CC-RPO                             VALUE  'R'.           ELGACLCC
00232                                                                   ELGACLCC
00233  01  WS-WORK-FIELDS.                                              ELGACLCC
00234      02 WS-ACL-SUB               PICTURE S9(04) COMP.             ELGACLCC
00235      02 WS-GAB-SUB               PICTURE S9(04) COMP.             ELGACLCC
00236      02 WS-GAB-INT-SUB           PICTURE S9(04) COMP.             ELGACLCC
00237      02 WS-ACL-ACCUM-CNT         PICTURE S9(04) COMP.             ELGACLCC
00238      02 WS-INTERNAL-COUNTER      PICTURE S9(04) COMP.             ELGACLCC
00239      02 WS-OCCURRENCE-SUB        PICTURE S9(04) COMP.             ELGACLCC
00240      02 WS-SAVE-SUB              PICTURE S9(04) COMP.             ELGACLCC
00241      02 SORT-SUB                 PICTURE S9(04) COMP.             ELGACLCC
00242      02 TEST-SUB                 PICTURE S9(04) COMP.             ELGACLCC
00243      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELGACLCC
00244      02 WS-SAVE-INDEX            USAGE IS INDEX.                  ELGACLCC
00245                                                                   ELGACLCC
00246  01 WS-MAX-INDEX-VALUES.                                          ELGACLCC
00247      02 WS-MAX-GAB-INDEX         USAGE IS INDEX.                  ELGACLCC
00248      02 WS-MAX-GAB-INT-INDEX     USAGE IS INDEX.                  ELGACLCC
00249      02 WS-MAX-GCT-INDEX         USAGE IS INDEX.                  ELGACLCC
00250      02 WS-MAX-GCG-INDEX         USAGE IS INDEX.                  ELGACLCC
00251      02 WS-MAX-GX1-INDEX         USAGE IS INDEX.                  ELGACLCC
00252      02 WS-MAX-GX3-INDEX         USAGE IS INDEX.                  ELGACLCC
00253      02 WS-MAX-GXS-INDEX         USAGE IS INDEX.                  ELGACLCC
00254                                                                   ELGACLCC
00255  01  WS-POINTERS.                                                 ELGACLCC
00256      02  WS-INST-CNTRCT-PTR      POINTER.                         ELGACLCC
00257      02  WS-PROF-CNTRCT-PTR      POINTER.                         ELGACLCC
00258                                                                   ELGACLCC
00259  01  ACCUM-HOLD-TBL.                                              ELGACLCC
00260      02  ACCUM-SLOT-NBR          PICTURE S9(07) COMP-3            ELGACLCC
00261                                  OCCURS 5 TIMES.                  ELGACLCC
00262                                                                   ELGACLCC
00263  01  WS-OCCURRENCE-PROCESSED-TBL.                                 ELGACLCC
00264      02                          PICTURE X                        ELGACLCC
00265                                  OCCURS 44 TIMES                  ELGACLCC
00266                                  INDEXED BY WS-OCCURRENCE-INDEX.  ELGACLCC
00267          88  WS-OCCURRENCE-PROCESSED           VALUE 'P'.         ELGACLCC
00268          88  WS-OCCURRENCE-NOT-PROCESSED       VALUE ' '.         ELGACLCC
00269                                                                   ELGACLCC
00270  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELGACLCC
00271      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGACLCC
00272      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGACLCC
00273      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGACLCC
00274      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGACLCC
00275      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGACLCC
00276      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGACLCC
00277                                                                   ELGACLCC
00278  01  WS-ASCEND-DESCEND-ENTRY-HOLD PICTURE X(28).                  ELGACLCC
00279 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELGACLCC
00280      COPY ELSCFTB2.                                               ELGACLCC
00281                                                                   ELGACLCC
00282 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELGACLCC
00283      COPY ELSCFTB9.                                               ELGACLCC
00284                                                                   ELGACLCC
00285      TITLE  'ELGACLCC          LINKAGE SECTION'                   ELGACLCC
00286  LINKAGE SECTION.                                                 ELGACLCC
00287  01  DFHCOMMAREA.                                                 ELGACLCC
00288      COPY ELSCOMMC.                                               ELGACLCC
00289 /                                                                 ELGACLCC
00290      COPY ELSCIA2C.                                               ELGACLCC
00291 /                                                                 ELGACLCC
00292      COPY ELSIOPMC.                                               ELGACLCC
00293 /                                                                 ELGACLCC
00294      COPY ELSKEYSC.                                               ELGACLCC
00295 /                                                                 ELGACLCC
00296      COPY ELSSRTPC.                                               ELGACLCC
00297 /                                                                 ELGACLCC
00298      COPY ELSSSCBC.                                               ELGACLCC
00299 /                                                                 ELGACLCC
00300  01  GCG-GRP-SPEC-RECORD-AREA.                                    ELGACLCC
00301      COPY GCGROUPC.                                               ELGACLCC
00302 /                                                                 ELGACLCC
00303  01  GCT-CONTRACT-RECORD-AREA.                                    ELGACLCC
00304      COPY GCCONTRC.                                               ELGACLCC
00305 /                                                                 ELGACLCC
00306  01  GAB-RECORD-AREA.                                             ELGACLCC
00307      COPY GCTACLC.                                                ELGACLCC
00308 /                                                                 ELGACLCC
00309      COPY ELSACUMC.                                               ELGACLCC
00310 /                                                                 ELGACLCC
00311  01  GX1-RECORD-AREA.                                             ELGACLCC
00312      COPY GCTIBGRC.                                               ELGACLCC
00313 /                                                                 ELGACLCC
00314  01  GX3-RECORD-AREA.                                             ELGACLCC
00315      COPY GCTIPGTC.                                               ELGACLCC
00316                                                                   ELGACLCC
00317  01  GXS-RECORD-AREA.                                             ELGACLCC
00318      COPY GCTIPGSC.                                               ELGACLCC
00319      TITLE  'ELGACLCC          PROCEDURE DIVISION'.               ELGACLCC
00320 /***********************************************************      ELGACLCC
00321 *                                                          *      ELGACLCC
00322 *    ELGACLCC MAINLINE                                     *      ELGACLCC
00323 *                                                          *      ELGACLCC
00324 ************************************************************      ELGACLCC
00325                                                                   ELGACLCC
00326  PROCEDURE DIVISION.                                              ELGACLCC
00327                                                                   ELGACLCC
00328      PERFORM 0010-INITIALIZATION.                                 ELGACLCC
00329      PERFORM 0100-PROCESS.                                        ELGACLCC
00330      GOBACK.                                                      ELGACLCC
00331                                                                   ELGACLCC
00332 ************************************************************      ELGACLCC
00333 *                                                          *      ELGACLCC
00334 *    INITIALIZATION                                        *      ELGACLCC
00335 *                                                          *      ELGACLCC
00336 ************************************************************      ELGACLCC
00337                                                                   ELGACLCC
00338  0010-INITIALIZATION.                                             ELGACLCC
00339      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELGACLCC
00340      PERFORM 0060-EST-ADR-KEY-WK-AREA.                            ELGACLCC
00341      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELGACLCC
00342      PERFORM 0120-EST-ADR-GRP-SPC.                                ELGACLCC
00343      PERFORM 0190-INIT-DATA.                                      ELGACLCC
00344                                                                   ELGACLCC
00345 /***********************************************************      ELGACLCC
00346 *                                                          *      ELGACLCC
00347 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGACLCC
00348 *                                                          *      ELGACLCC
00349 ************************************************************      ELGACLCC
00350                                                                   ELGACLCC
00351  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELGACLCC
00352      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGACLCC
00353      THEN                                                         ELGACLCC
00354         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGACLCC
00355      ELSE                                                         ELGACLCC
00356         IF ECA-CIA-PTR = NULL                                     ELGACLCC
00357         THEN                                                      ELGACLCC
00358            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGACLCC
00359         ELSE                                                      ELGACLCC
00360            CALL 'ELUINISM' USING DFHCOMMAREA                      ELGACLCC
00361               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELGACLCC
00362               END-CALL                                            ELGACLCC
00363            SET CIA-ELSSSCB-DDN TO TRUE                            ELGACLCC
00364            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELGACLCC
00365               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELGACLCC
00366               END-CALL                                            ELGACLCC
00367            IF CIA-RC-PTR-NULL                                     ELGACLCC
00368            THEN                                                   ELGACLCC
00369               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELGACLCC
00370               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGACLCC
00371            ELSE                                                   ELGACLCC
00372               CONTINUE                                            ELGACLCC
00373            END-IF                                                 ELGACLCC
00374         END-IF                                                    ELGACLCC
00375      END-IF.                                                      ELGACLCC
00376                                                                   ELGACLCC
00377 /***********************************************************      ELGACLCC
00378 *                                                          *      ELGACLCC
00379 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELGACLCC
00380 *                                                          *      ELGACLCC
00381 ************************************************************      ELGACLCC
00382                                                                   ELGACLCC
00383  0060-EST-ADR-KEY-WK-AREA.                                        ELGACLCC
00384      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGACLCC
00385      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACLCC
00386         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELGACLCC
00387         END-CALL.                                                 ELGACLCC
00388      IF CIA-RC-PTR-NULL                                           ELGACLCC
00389         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACLCC
00390         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGACLCC
00391      END-IF.                                                      ELGACLCC
00392                                                                   ELGACLCC
00393 ************************************************************      ELGACLCC
00394 *                                                          *      ELGACLCC
00395 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGACLCC
00396 *                                                          *      ELGACLCC
00397 ************************************************************      ELGACLCC
00398                                                                   ELGACLCC
00399  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGACLCC
00400      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGACLCC
00401      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACLCC
00402         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELGACLCC
00403         END-CALL.                                                 ELGACLCC
00404      IF CIA-RC-PTR-NULL                                           ELGACLCC
00405         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACLCC
00406         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGACLCC
00407      END-IF.                                                      ELGACLCC
00408                                                                   ELGACLCC
00409 /***********************************************************      ELGACLCC
00410 *                                                          *      ELGACLCC
00411 *    ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD     *      ELGACLCC
00412 *                                                          *      ELGACLCC
00413 ************************************************************      ELGACLCC
00414                                                                   ELGACLCC
00415  0120-EST-ADR-GRP-SPC.                                            ELGACLCC
00416      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELGACLCC
00417      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACLCC
00418         ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                       ELGACLCC
00419         END-CALL.                                                 ELGACLCC
00420      IF CIA-RC-PTR-NULL                                           ELGACLCC
00421         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACLCC
00422         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGACLCC
00423      END-IF.                                                      ELGACLCC
00424                                                                   ELGACLCC
00425 ************************************************************      ELGACLCC
00426 *                                                          *      ELGACLCC
00427 *    INITIALIZE DATA AREAS                                 *      ELGACLCC
00428 *                                                          *      ELGACLCC
00429 ************************************************************      ELGACLCC
00430                                                                   ELGACLCC
00431  0190-INIT-DATA.                                                  ELGACLCC
00432      COMPUTE PC-GCT-MAX-SUB =   LENGTH OF GCT-CONT-TAB-PTRS       ELGACLCC
00433                               / LENGTH OF GCT-CON-TAB-ID-SLOT.    ELGACLCC
00434      SET GCT-INDEX                TO PC-GCT-MAX-SUB.              ELGACLCC
00435      SET WS-MAX-GCT-INDEX         TO GCT-INDEX.                   ELGACLCC
00436      SET GCG-INDEX                TO GCG-COUNT-TAB-PROVN-POINTERS.ELGACLCC
00437      SET WS-MAX-GCG-INDEX         TO GCG-INDEX.                   ELGACLCC
00438      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGACLCC
00439      INITIALIZE WS-ACL-ACCUM-CNT.                                 ELGACLCC
00440                                                                   ELGACLCC
00441 /***********************************************************      ELGACLCC
00442 *                                                          *      ELGACLCC
00443 *        PROCESS                                           *      ELGACLCC
00444 *                                                          *      ELGACLCC
00445 ************************************************************      ELGACLCC
00446                                                                   ELGACLCC
00447  0100-PROCESS.                                                    ELGACLCC
00448      PERFORM 0110-SCAN-GRP-SPC-FOR-ACCUMS.                        ELGACLCC
00449      PERFORM 0120-SCAN-CONTRACTS-FOR-ACCUMS.                      ELGACLCC
00450                                                                   ELGACLCC
00451      IF WS-ACL-ACCUM-CNT >  0                                     ELGACLCC
00452      THEN                                                         ELGACLCC
00453          PERFORM 0210-SCAN-FOR-APPLIC-OCCRNCS                     ELGACLCC
00454      END-IF.                                                      ELGACLCC
00455                                                                   ELGACLCC
00456                                                                   ELGACLCC
00457 *    -- LINK TO THE OUTPUT GENERATOR                              ELGACLCC
00458      IF SW-APPLIC-ACCUM-FOUND                                     ELGACLCC
00459         SET SRP-COST-CONT-ACCUM TO TRUE                           ELGACLCC
00460         EXEC CICS LINK PROGRAM  ('ELGACL')                        ELGACLCC
00461                        COMMAREA (DFHCOMMAREA)                     ELGACLCC
00462         END-EXEC                                                  ELGACLCC
00463      END-IF.                                                      ELGACLCC
00464                                                                   ELGACLCC
00465 /***********************************************************      ELGACLCC
00466 *                                                          *      ELGACLCC
00467 *    SCAN GROUP SPECIFIC RECORD FOR ACCUMULATORS           *      ELGACLCC
00468 *                                                          *      ELGACLCC
00469 ************************************************************      ELGACLCC
00470                                                                   ELGACLCC
00471  0110-SCAN-GRP-SPC-FOR-ACCUMS.                                    ELGACLCC
00472      PERFORM WITH TEST BEFORE                                     ELGACLCC
00473         VARYING GCG-INDEX FROM 1 BY 1                             ELGACLCC
00474           UNTIL GCG-INDEX = WS-MAX-GCG-INDEX                      ELGACLCC
00475                 OR GCG-TAB-ID (GCG-INDEX) > PC-ACL                ELGACLCC
00476 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELGACLCC
00477         IF     GCG-TAB-ID (GCG-INDEX)  =  PC-ACL                  ELGACLCC
00478            AND GCG-TAB-SLOT-NO (GCG-INDEX)  >  ZERO               ELGACLCC
00479         THEN                                                      ELGACLCC
00480 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELGACLCC
00481            MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO WS-SLOT-NBR        ELGACLCC
00482            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELGACLCC
00483         END-IF                                                    ELGACLCC
00484         END-PERFORM.                                              ELGACLCC
00485                                                                   ELGACLCC
00486 /***********************************************************      ELGACLCC
00487 *                                                          *      ELGACLCC
00488 *    SCAN CONTRACT RECORDS FOR ACCUMULATORS                *      ELGACLCC
00489 *                                                          *      ELGACLCC
00490 ************************************************************      ELGACLCC
00491                                                                   ELGACLCC
00492  0120-SCAN-CONTRACTS-FOR-ACCUMS.                                  ELGACLCC
00493      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELGACLCC
00494      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELGACLCC
00495                                                                   ELGACLCC
00496      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELGACLCC
00497      THEN                                                         ELGACLCC
00498          PERFORM 0130-SCAN-INST-BAS                               ELGACLCC
00499      END-IF.                                                      ELGACLCC
00500                                                                   ELGACLCC
00501      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELGACLCC
00502      THEN                                                         ELGACLCC
00503          PERFORM 0140-SCAN-PROF-BAS                               ELGACLCC
00504      END-IF.                                                      ELGACLCC
00505                                                                   ELGACLCC
00506      SET WS-INST-CNTRCT-PTR TO NULLS.                             ELGACLCC
00507      SET WS-PROF-CNTRCT-PTR TO NULLS.                             ELGACLCC
00508                                                                   ELGACLCC
00509      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELGACLCC
00510      THEN                                                         ELGACLCC
00511          PERFORM 0150-SCAN-INST-SUP                               ELGACLCC
00512      END-IF.                                                      ELGACLCC
00513                                                                   ELGACLCC
00514      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELGACLCC
00515      THEN                                                         ELGACLCC
00516          PERFORM 0160-SCAN-PROF-SUP                               ELGACLCC
00517      END-IF.                                                      ELGACLCC
00518                                                                   ELGACLCC
00519 /***********************************************************      ELGACLCC
00520 *                                                          *      ELGACLCC
00521 *    SCAN INSTITUTIONAL BASIC CONTRACT RECORD              *      ELGACLCC
00522 *                                                          *      ELGACLCC
00523 ************************************************************      ELGACLCC
00524                                                                   ELGACLCC
00525  0130-SCAN-INST-BAS.                                              ELGACLCC
00526      SET CIA-ELSCONIB-DDN TO TRUE.                                ELGACLCC
00527      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACLCC
00528         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGACLCC
00529         END-CALL.                                                 ELGACLCC
00530      SET WS-INST-CNTRCT-PTR                                       ELGACLCC
00531       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELGACLCC
00532                                                                   ELGACLCC
00533      IF CIA-RC-PTR-NULL                                           ELGACLCC
00534      THEN                                                         ELGACLCC
00535         CONTINUE                                                  ELGACLCC
00536      ELSE                                                         ELGACLCC
00537         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELGACLCC
00538      END-IF.                                                      ELGACLCC
00539                                                                   ELGACLCC
00540 ************************************************************      ELGACLCC
00541 *                                                          *      ELGACLCC
00542 *    SCAN PROFESSIONAL BASIC CONTRACT RECORD               *      ELGACLCC
00543 *                                                          *      ELGACLCC
00544 ************************************************************      ELGACLCC
00545                                                                   ELGACLCC
00546  0140-SCAN-PROF-BAS.                                              ELGACLCC
00547      SET CIA-ELSCONPB-DDN TO TRUE.                                ELGACLCC
00548      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACLCC
00549         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGACLCC
00550         END-CALL.                                                 ELGACLCC
00551      SET WS-PROF-CNTRCT-PTR                                       ELGACLCC
00552       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELGACLCC
00553                                                                   ELGACLCC
00554      IF    CIA-RC-PTR-NULL                                        ELGACLCC
00555         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELGACLCC
00556      THEN                                                         ELGACLCC
00557         CONTINUE                                                  ELGACLCC
00558      ELSE                                                         ELGACLCC
00559         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELGACLCC
00560      END-IF.                                                      ELGACLCC
00561                                                                   ELGACLCC
00562 /***********************************************************      ELGACLCC
00563 *                                                          *      ELGACLCC
00564 *    SCAN INSTITUTIONAL SUPPLEMENTAL CONTRACT RECORD       *      ELGACLCC
00565 *                                                          *      ELGACLCC
00566 ************************************************************      ELGACLCC
00567                                                                   ELGACLCC
00568  0150-SCAN-INST-SUP.                                              ELGACLCC
00569      SET CIA-ELSCONIS-DDN TO TRUE.                                ELGACLCC
00570      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACLCC
00571         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGACLCC
00572         END-CALL.                                                 ELGACLCC
00573      SET WS-INST-CNTRCT-PTR                                       ELGACLCC
00574       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELGACLCC
00575                                                                   ELGACLCC
00576      IF CIA-RC-PTR-NULL                                           ELGACLCC
00577      THEN                                                         ELGACLCC
00578         CONTINUE                                                  ELGACLCC
00579      ELSE                                                         ELGACLCC
00580         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELGACLCC
00581      END-IF.                                                      ELGACLCC
00582                                                                   ELGACLCC
00583 ************************************************************      ELGACLCC
00584 *                                                          *      ELGACLCC
00585 *    SCAN PROFESSIONAL SUPPLEMENTAL CONTRACT RECORD        *      ELGACLCC
00586 *                                                          *      ELGACLCC
00587 ************************************************************      ELGACLCC
00588                                                                   ELGACLCC
00589  0160-SCAN-PROF-SUP.                                              ELGACLCC
00590      SET CIA-ELSCONPS-DDN TO TRUE.                                ELGACLCC
00591      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACLCC
00592         ADDRESS OF GCT-CONTRACT-RECORD-AREA                       ELGACLCC
00593         END-CALL.                                                 ELGACLCC
00594      SET WS-PROF-CNTRCT-PTR                                       ELGACLCC
00595       TO ADDRESS OF GCT-CONTRACT-RECORD-AREA.                     ELGACLCC
00596                                                                   ELGACLCC
00597      IF    CIA-RC-PTR-NULL                                        ELGACLCC
00598         OR (WS-INST-CNTRCT-PTR = WS-PROF-CNTRCT-PTR)              ELGACLCC
00599      THEN                                                         ELGACLCC
00600         CONTINUE                                                  ELGACLCC
00601      ELSE                                                         ELGACLCC
00602         PERFORM 0170-SCAN-CONTRACT-FOR-ACCUMS                     ELGACLCC
00603      END-IF.                                                      ELGACLCC
00604                                                                   ELGACLCC
00605 /***********************************************************      ELGACLCC
00606 *                                                          *      ELGACLCC
00607 *    SCAN A CONTRACT RECORD FOR ACCUMULATORS               *      ELGACLCC
00608 *                                                          *      ELGACLCC
00609 ************************************************************      ELGACLCC
00610                                                                   ELGACLCC
00611  0170-SCAN-CONTRACT-FOR-ACCUMS.                                   ELGACLCC
00612      PERFORM WITH TEST BEFORE                                     ELGACLCC
00613         VARYING GCT-TAB-INDEX FROM 1 BY 1                         ELGACLCC
00614           UNTIL    GCT-TAB-INDEX > WS-MAX-GCT-INDEX               ELGACLCC
00615                 OR GCT-CON-TAB-ID (GCT-TAB-INDEX) > PC-ACL        ELGACLCC
00616 *    -- DETERMINE IF ACCUMULATOR FOUND                            ELGACLCC
00617         IF     GCT-CON-TAB-ID (GCT-TAB-INDEX) = PC-ACL            ELGACLCC
00618            AND GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  > ZEROS          ELGACLCC
00619         THEN                                                      ELGACLCC
00620 *       -- SAVE ACCUMULATOR SLOT NUMBER IN HOLD TABLE             ELGACLCC
00621            MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  TO  WS-SLOT-NBR ELGACLCC
00622            PERFORM 0200-SAVE-UNQ-ACCUM-SLOT-NBR                   ELGACLCC
00623         END-IF                                                    ELGACLCC
00624         END-PERFORM.                                              ELGACLCC
00625                                                                   ELGACLCC
00626 /***********************************************************      ELGACLCC
00627 *                                                          *      ELGACLCC
00628 *    SAVE UNIQUE ACCUMULATOR SLOT NUMBER                   *      ELGACLCC
00629 *                                                          *      ELGACLCC
00630 ************************************************************      ELGACLCC
00631                                                                   ELGACLCC
00632  0200-SAVE-UNQ-ACCUM-SLOT-NBR.                                    ELGACLCC
00633                                                                   ELGACLCC
00634 * -- SCAN TABLE OF ACCUM SLOT NUMBERS FOR DUPLICATE               ELGACLCC
00635      SET SW-UNQ-SLOT-NBR TO TRUE.                                 ELGACLCC
00636      PERFORM WITH TEST BEFORE                                     ELGACLCC
00637         VARYING WS-ACL-SUB FROM 1 BY 1                            ELGACLCC
00638           UNTIL    WS-ACL-SUB > WS-ACL-ACCUM-CNT                  ELGACLCC
00639                 OR SW-DUP-SLOT-NBR                                ELGACLCC
00640         IF WS-SLOT-NBR = ACCUM-SLOT-NBR (WS-ACL-SUB)              ELGACLCC
00641         THEN                                                      ELGACLCC
00642            SET SW-DUP-SLOT-NBR TO TRUE                            ELGACLCC
00643         END-IF                                                    ELGACLCC
00644      END-PERFORM.                                                 ELGACLCC
00645                                                                   ELGACLCC
00646 * -- IF SLOT NUMBER IS UNIQUE, ADD IT TO THE HOLD TABLE           ELGACLCC
00647      IF SW-UNQ-SLOT-NBR                                           ELGACLCC
00648      THEN                                                         ELGACLCC
00649         ADD 1 TO  WS-ACL-ACCUM-CNT                                ELGACLCC
00650         MOVE WS-SLOT-NBR TO ACCUM-SLOT-NBR(WS-ACL-ACCUM-CNT)      ELGACLCC
00651      END-IF.                                                      ELGACLCC
00652                                                                   ELGACLCC
00653 /***********************************************************      ELGACLCC
00654 *                                                          *      ELGACLCC
00655 *    SCAN ACL ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELGACLCC
00656 *                                                          *      ELGACLCC
00657 ************************************************************      ELGACLCC
00658                                                                   ELGACLCC
00659  0210-SCAN-FOR-APPLIC-OCCRNCS.                                    ELGACLCC
00660      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGACLCC
00661      PERFORM 0220-DELETE-ACL-SUMMARY-FILE.                        ELGACLCC
00662      PERFORM 0230-ALLOC-WORKFILE-REC-AREA.                        ELGACLCC
00663                                                                   ELGACLCC
00664 * -- READ AND SCAN EACH ACCUMULATOR TABULAR                       ELGACLCC
00665      PERFORM WITH TEST BEFORE                                     ELGACLCC
00666         VARYING WS-ACL-SUB FROM 1 BY 1                            ELGACLCC
00667           UNTIL WS-ACL-SUB > WS-ACL-ACCUM-CNT                     ELGACLCC
00668 *    -- OBTAIN ACCUMULATOR TABULAR RECORD                         ELGACLCC
00669         MOVE PC-ACL TO KWA-PROVISION-ID                           ELGACLCC
00670         MOVE ACCUM-SLOT-NBR (WS-ACL-SUB) TO KWA-PROVISION-SLOT-NO ELGACLCC
00671         MOVE SPACES TO WS-OCCURRENCE-PROCESSED-TBL                ELGACLCC
00672         PERFORM 0240-READ-TABULAR-REC                             ELGACLCC
00673 *    -- SCAN ACCUMULATOR TABULAR                                  ELGACLCC
00674         PERFORM WITH TEST BEFORE                                  ELGACLCC
00675            VARYING WS-OCCURRENCE-SUB FROM 1 BY 1                  ELGACLCC
00676            UNTIL WS-OCCURRENCE-SUB >= GAB-ENTRY-COUNT             ELGACLCC
00677            IF WS-OCCURRENCE-NOT-PROCESSED (WS-OCCURRENCE-SUB)     ELGACLCC
00678              SET WS-OCCURRENCE-INDEX                              ELGACLCC
00679                           GAB-INDEX                               ELGACLCC
00680               TO WS-OCCURRENCE-SUB                                ELGACLCC
00681              PERFORM 0300-TEST-ACL-OCCURRENCE                     ELGACLCC
00682            END-IF                                                 ELGACLCC
00683         END-PERFORM                                               ELGACLCC
00684      END-PERFORM.                                                 ELGACLCC
00685                                                                   ELGACLCC
00686 /***********************************************************      ELGACLCC
00687 *                                                          *      ELGACLCC
00688 *        DELETE ACL SUMMARY FILE                           *      ELGACLCC
00689 *                                                          *      ELGACLCC
00690 ************************************************************      ELGACLCC
00691                                                                   ELGACLCC
00692  0220-DELETE-ACL-SUMMARY-FILE.                                    ELGACLCC
00693      SET IOP-DEL TO TRUE.                                         ELGACLCC
00694      SET IOP-FCQ-NONE TO TRUE.                                    ELGACLCC
00695      SET IOP-KVQ-NONE TO TRUE.                                    ELGACLCC
00696      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACLCC
00697                                                                   ELGACLCC
00698 /***********************************************************      ELGACLCC
00699 *                                                          *      ELGACLCC
00700 *    ALLOCATE WORKFILE RECORD AREA                         *      ELGACLCC
00701 *                                                          *      ELGACLCC
00702 ************************************************************      ELGACLCC
00703                                                                   ELGACLCC
00704  0230-ALLOC-WORKFILE-REC-AREA.                                    ELGACLCC
00705      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGACLCC
00706      SET CIA-STG-GETMAIN TO TRUE.                                 ELGACLCC
00707      SET IOP-GETMAIN-REC TO TRUE.                                 ELGACLCC
00708      COMPUTE IOP-MAX-REC-LEN =                                    ELGACLCC
00709              LENGTH OF ACCUM-FIXED-AREA                           ELGACLCC
00710 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELGACLCC
00711            + LENGTH OF ACCUM-VARIABLE-AREA                        ELGACLCC
00712            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELGACLCC
00713 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELGACLCC
00714 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELGACLCC
00715                                                                   ELGACLCC
00716      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACLCC
00717      IF IOP-REC-PTR = NULLS                                       ELGACLCC
00718      THEN                                                         ELGACLCC
00719         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACLCC
00720         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGACLCC
00721      ELSE                                                         ELGACLCC
00722         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELGACLCC
00723      END-IF.                                                      ELGACLCC
00724                                                                   ELGACLCC
00725 /***********************************************************      ELGACLCC
00726 *                                                          *      ELGACLCC
00727 *    READ TABULAR RECORD                                   *      ELGACLCC
00728 *                                                          *      ELGACLCC
00729 ************************************************************      ELGACLCC
00730                                                                   ELGACLCC
00731  0240-READ-TABULAR-REC.                                           ELGACLCC
00732      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELGACLCC
00733      SET IOP-RD TO TRUE.                                          ELGACLCC
00734      SET IOP-FCQ-NONE TO TRUE.                                    ELGACLCC
00735      SET IOP-KVQ-EQ TO TRUE.                                      ELGACLCC
00736      SET IOP-STG-MODE-MOVE TO TRUE.                               ELGACLCC
00737      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGACLCC
00738      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGACLCC
00739      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACLCC
00740                                                                   ELGACLCC
00741      EVALUATE TRUE                                                ELGACLCC
00742        WHEN IOP-RC-OK                                             ELGACLCC
00743           SET ADDRESS OF GAB-RECORD-AREA TO IOP-REC-PTR           ELGACLCC
00744           SET IOP-REC-PTR                TO NULLS                 ELGACLCC
00745           SET GAB-INDEX                  TO GAB-ENTRY-COUNT       ELGACLCC
00746           SET WS-MAX-GAB-INDEX           TO GAB-INDEX             ELGACLCC
00747        WHEN IOP-RC-NOTFND                                         ELGACLCC
00748           SET CIA-AB-NOTFND-GCTABULR TO TRUE                      ELGACLCC
00749           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELGACLCC
00750        WHEN OTHER                                                 ELGACLCC
00751           SET CIA-AB-CRITIO TO TRUE                               ELGACLCC
00752           EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC             ELGACLCC
00753        END-EVALUATE.                                              ELGACLCC
00754                                                                   ELGACLCC
00755 /***********************************************************      ELGACLCC
00756 *                                                          *      ELGACLCC
00757 *        TEST ACL OCCURS                                   *      ELGACLCC
00758 *                                                          *      ELGACLCC
00759 ************************************************************      ELGACLCC
00760                                                                   ELGACLCC
00761  0300-TEST-ACL-OCCURRENCE.                                        ELGACLCC
00762      MOVE GAB-COINS-L-O-B (GAB-INDEX) TO WS-LOB-ACCUM-OCCRNC.     ELGACLCC
00763      MOVE GAB-COINS-COST-CONTAIN-IND (GAB-INDEX)                  ELGACLCC
00764        TO WS-COST-CONTAIN-IND.                                    ELGACLCC
00765      SET SW-CC-IND-DOES-NOT-APPLY                                 ELGACLCC
00766          SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELGACLCC
00767                                                                   ELGACLCC
00768      PERFORM 0370-CHECK-FOR-CORRECT-CC-TYPE.                      ELGACLCC
00769                                                                   ELGACLCC
00770      IF SW-CC-IND-APPLIES                                         ELGACLCC
00771         PERFORM 0310-INITIALIZE-OCCURRENCE                        ELGACLCC
00772         PERFORM 0320-SCAN-FOR-INTERNALS.                          ELGACLCC
00773      IF SW-OCCRNC-APPLIES                                         ELGACLCC
00774      THEN                                                         ELGACLCC
00775 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELGACLCC
00776         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELGACLCC
00777         PERFORM 0360-INIT-ACCUM-EXTRACT                           ELGACLCC
00778         PERFORM 0380-EXTRACT-ACCUM                                ELGACLCC
00779         PERFORM 0520-CHK-EXTRACT-DATA-INTGRTY                     ELGACLCC
00780         PERFORM 0390-EXTRACT-ACCUM-VBL-PORTION                    ELGACLCC
00781         PERFORM 0710-WRITE-EXTRACT-RECORD                         ELGACLCC
00782      END-IF.                                                      ELGACLCC
00783      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-INDEX)            ELGACLCC
00784          TO TRUE.                                                 ELGACLCC
00785                                                                   ELGACLCC
00786 /***********************************************************      ELGACLCC
00787 *                                                          *      ELGACLCC
00788 *        INITIALIZE OCCURRENCE                             *      ELGACLCC
00789 *                                                          *      ELGACLCC
00790 ************************************************************      ELGACLCC
00791                                                                   ELGACLCC
00792  0310-INITIALIZE-OCCURRENCE.                                      ELGACLCC
00793      SET SW-OCCRNC-DOES-NOT-APPLY                                 ELGACLCC
00794          SW-HAS-NO-IBGR                                           ELGACLCC
00795          SW-HAS-NO-IDGD                                           ELGACLCC
00796          SW-HAS-NO-IPGN                                           ELGACLCC
00797          SW-HAS-NO-IPGP                                           ELGACLCC
00798          SW-HAS-NO-IPGT                                           ELGACLCC
00799          SW-HAS-NO-IPGS                                           ELGACLCC
00800       TO TRUE.                                                    ELGACLCC
00801      INITIALIZE WS-IBGR-SLOT-NBR                                  ELGACLCC
00802                 WS-IDGD-SLOT-NBR                                  ELGACLCC
00803                 WS-IPGN-SLOT-NBR                                  ELGACLCC
00804                 WS-IPGP-SLOT-NBR                                  ELGACLCC
00805                 WS-IPGT-SLOT-NBR                                  ELGACLCC
00806                 WS-IPGS-SLOT-NBR                                  ELGACLCC
00807                 WS-INTERNAL-COUNTER.                              ELGACLCC
00808      SET GAB-INT-INDEX TO                                         ELGACLCC
00809          GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX).                  ELGACLCC
00810      SET WS-MAX-GAB-INT-INDEX TO GAB-INT-INDEX.                   ELGACLCC
00811      SET SW-INTRNL-INST-CLASS-NOT-DET                             ELGACLCC
00812          SW-INTRNL-PROF-CLASS-NOT-DET                             ELGACLCC
00813          SW-INTRNL-PROF-SPEC-NOT-DET                              ELGACLCC
00814       TO TRUE.                                                    ELGACLCC
00815      SET SW-INTRNL-PROF-SPEC-NOT-DET                              ELGACLCC
00816       TO TRUE.                                                    ELGACLCC
00817                                                                   ELGACLCC
00818                                                                   ELGACLCC
00819 /***********************************************************      ELGACLCC
00820 *                                                          *      ELGACLCC
00821 *        SCAN FOR INTERNAL TABULARS                        *      ELGACLCC
00822 *                                                          *      ELGACLCC
00823 ************************************************************      ELGACLCC
00824                                                                   ELGACLCC
00825  0320-SCAN-FOR-INTERNALS.                                         ELGACLCC
00826 *    (THIS IS DONE NOW IN CASE IPGT OR IBGR IS NEEDED TO DETERMINEELGACLCC
00827 *     WHETHER OCCURRENCE IS INSTITUTIONAL OR PROFESSIONAL.)       ELGACLCC
00828      PERFORM 0330-SCAN-THE-INTERNAL-TABULAR                       ELGACLCC
00829         VARYING GAB-INT-INDEX FROM 1 BY 1                         ELGACLCC
00830           UNTIL    GAB-INT-INDEX                                  ELGACLCC
00831                 >= WS-MAX-GAB-INT-INDEX.                          ELGACLCC
00832                                                                   ELGACLCC
00833      EVALUATE TRUE ALSO TRUE                                      ELGACLCC
00834         WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                        ELGACLCC
00835            SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE                  ELGACLCC
00836            SET SW-OCCRNC-APPLIES TO TRUE                          ELGACLCC
00837         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH                 ELGACLCC
00838            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGACLCC
00839            PERFORM 0550-CHK-INTRNL-TAB-PROV-CLASS                 ELGACLCC
00840         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST                 ELGACLCC
00841            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGACLCC
00842            SET SW-OCCRNC-APPLIES TO TRUE                          ELGACLCC
00843         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH                 ELGACLCC
00844            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGACLCC
00845            PERFORM 0550-CHK-INTRNL-TAB-PROV-CLASS                 ELGACLCC
00846            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGACLCC
00847            PERFORM 0551-CHK-INTRNL-TAB-PROV-SPEC                  ELGACLCC
00848         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF                 ELGACLCC
00849            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGACLCC
00850            SET SW-OCCRNC-APPLIES TO TRUE                          ELGACLCC
00851         WHEN OTHER                                                ELGACLCC
00852            CONTINUE                                               ELGACLCC
00853         END-EVALUATE.                                             ELGACLCC
00854                                                                   ELGACLCC
00855 /***********************************************************      ELGACLCC
00856 *                                                          *      ELGACLCC
00857 *        SCAN THE INTERNAL TABULARS                        *      ELGACLCC
00858 *                                                          *      ELGACLCC
00859 ************************************************************      ELGACLCC
00860                                                                   ELGACLCC
00861  0330-SCAN-THE-INTERNAL-TABULAR.                                  ELGACLCC
00862       IF GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX) > 0              ELGACLCC
00863          THEN                                                     ELGACLCC
00864            MOVE GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX)           ELGACLCC
00865              TO WS-SLOT-NBR                                       ELGACLCC
00866            EVALUATE GAB-INT-ID (GAB-INDEX, GAB-INT-INDEX)         ELGACLCC
00867               WHEN PC-IBGR                                        ELGACLCC
00868                 MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR              ELGACLCC
00869                 SET SW-HAS-IBGR                                   ELGACLCC
00870                  TO TRUE                                          ELGACLCC
00871               WHEN PC-IDGD                                        ELGACLCC
00872                 MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR              ELGACLCC
00873                 SET SW-HAS-IDGD                                   ELGACLCC
00874                  TO TRUE                                          ELGACLCC
00875               WHEN PC-IPGP                                        ELGACLCC
00876                 MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR              ELGACLCC
00877                 SET SW-HAS-IPGP                                   ELGACLCC
00878                  TO TRUE                                          ELGACLCC
00879               WHEN PC-IPGN                                        ELGACLCC
00880                 MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR              ELGACLCC
00881                 SET SW-HAS-IPGN                                   ELGACLCC
00882                  TO TRUE                                          ELGACLCC
00883               WHEN PC-IPGT                                        ELGACLCC
00884                 MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR              ELGACLCC
00885                 SET SW-HAS-IPGT                                   ELGACLCC
00886                  TO TRUE                                          ELGACLCC
00887               WHEN PC-IPGS                                        ELGACLCC
00888                 MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR              ELGACLCC
00889                 SET SW-HAS-IPGS                                   ELGACLCC
00890                  TO TRUE                                          ELGACLCC
00891               WHEN OTHER                                          ELGACLCC
00892                  CONTINUE                                         ELGACLCC
00893            END-EVALUATE                                           ELGACLCC
00894       END-IF.                                                     ELGACLCC
00895                                                                   ELGACLCC
00896                                                                   ELGACLCC
00897 /***********************************************************      ELGACLCC
00898 *                                                          *      ELGACLCC
00899 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELGACLCC
00900 *                                                          *      ELGACLCC
00901 ************************************************************      ELGACLCC
00902                                                                   ELGACLCC
00903  0360-INIT-ACCUM-EXTRACT.                                         ELGACLCC
00904      INITIALIZE ACCUM-FIXED-AREA.                                 ELGACLCC
00905      SET ACCUM-ACL TO TRUE.                                       ELGACLCC
00906      MOVE +1 TO ACCUM-ASCEND-DESCEND-COUNT.                       ELGACLCC
00907      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELGACLCC
00908      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELGACLCC
00909      INITIALIZE ACCUM-COPAY-ENTRY (1).                            ELGACLCC
00910                                                                   ELGACLCC
00911 /***********************************************************      ELGACLCC
00912 *                                                          *      ELGACLCC
00913 *        CHECK FOR CORRECT COST CONTAINMENT TYPE           *      ELGACLCC
00914 *                                                          *      ELGACLCC
00915 ************************************************************      ELGACLCC
00916                                                                   ELGACLCC
00917  0370-CHECK-FOR-CORRECT-CC-TYPE.                                  ELGACLCC
00918      MOVE SSB-MODIFIER-1                        TO                ELGACLCC
00919          WS-SUBTOPIC-TYPE.                                        ELGACLCC
00920      IF        (SUBTOPIC-ATCP    AND CC-ATCP)                     ELGACLCC
00921             OR (SUBTOPIC-WEEKEND AND CC-WEEKEND)                  ELGACLCC
00922             OR (SUBTOPIC-HOSP    AND CC-HOSP)                     ELGACLCC
00923             OR (SUBTOPIC-IOB     AND CC-IOB)                      ELGACLCC
00924             OR (SUBTOPIC-MASOP   AND CC-MASOP)                    ELGACLCC
00925             OR (SUBTOPIC-MONDIS  AND CC-MONDIS)                   ELGACLCC
00926             OR (SUBTOPIC-MEDNEC  AND CC-MEDNEC)                   ELGACLCC
00927             OR (SUBTOPIC-MOPS    AND CC-MOPS)                     ELGACLCC
00928             OR (SUBTOPIC-MSA     AND CC-MSA)                      ELGACLCC
00929             OR (SUBTOPIC-PPO     AND CC-PPO)                      ELGACLCC
00930             OR (SUBTOPIC-PAR     AND CC-PAR)                      ELGACLCC
00931             OR (SUBTOPIC-PAT     AND CC-PAT)                      ELGACLCC
00932             OR (SUBTOPIC-PAN     AND CC-PAN)                      ELGACLCC
00933             OR (SUBTOPIC-EMH     AND CC-EMH)                      ELGACLCC
00934             OR (SUBTOPIC-MCN     AND CC-MCN)                      ELGACLCC
00935             OR (SUBTOPIC-POS     AND CC-POS)                      ELGACLCC
00936             OR (SUBTOPIC-MHSC    AND CC-MHSC)                     ELGACLCC
00937             OR (SUBTOPIC-RPO     AND CC-RPO)                      ELGACLCC
00938             OR (SUBTOPIC-CPO     AND CC-CPO)                      ELGACLCC
00939             OR (SUBTOPIC-CBL     AND CC-CBL)                      ELGACLCC
00940             OR (SUBTOPIC-BAE     AND CC-BAE)                      ELGACLCC
00941          SET SW-CC-IND-APPLIES TO TRUE.                           ELGACLCC
00942                                                                   ELGACLCC
00943                                                                   ELGACLCC
00944 /***********************************************************      ELGACLCC
00945 *                                                          *      ELGACLCC
00946 *        EXTRACT ACCUM                                     *      ELGACLCC
00947 *                                                          *      ELGACLCC
00948 ************************************************************      ELGACLCC
00949                                                                   ELGACLCC
00950  0380-EXTRACT-ACCUM.                                              ELGACLCC
00951                                                                   ELGACLCC
00952 * -- SET FIXED PORTION DATA ELEMENTS                              ELGACLCC
00953      MOVE GAB-COINS-FYI-VALUE (GAB-INDEX) TO ACCUM-FYI-VALUE.     ELGACLCC
00954      MOVE GAB-COINS-COST-CONTAIN-IND (GAB-INDEX)                  ELGACLCC
00955        TO     ACCUM-COST-CONTAIN-IND.                             ELGACLCC
00956      MOVE GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)                ELGACLCC
00957        TO     ACCUM-PLACE-OF-TREATMENT.                           ELGACLCC
00958      MOVE GAB-COINS-SERVICE-GROUP (GAB-INDEX)                     ELGACLCC
00959        TO     ACCUM-SERVICE-GROUP.                                ELGACLCC
00960      MOVE GAB-COINS-CO-PAY-IND (GAB-INDEX)                        ELGACLCC
00961        TO     ACCUM-CO-PAY-IND (COPAY-INDEX).                     ELGACLCC
00962      MOVE GAB-COINS-LMT-MANDATORY-IND (GAB-INDEX)                 ELGACLCC
00963        TO     ACCUM-LMT-MANDATORY-IND.                            ELGACLCC
00964      MOVE GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX)               ELGACLCC
00965        TO     ACCUM-INTERNAL-DESCRIPTOR.                          ELGACLCC
00966      MOVE GAB-COINS-DAY-FACTOR-IND (GAB-INDEX)                    ELGACLCC
00967        TO     ACCUM-DAY-FACTOR-IND.                               ELGACLCC
00968      MOVE GAB-COINS-CLAIM-LVL-ACCUM-IND (GAB-INDEX)               ELGACLCC
00969        TO     ACCUM-CLAIM-LVL-ACCUM-IND.                          ELGACLCC
00970      MOVE GAB-COINS-1ST-DOLR-COVRGE-LMT (GAB-INDEX)               ELGACLCC
00971        TO     ACCUM-1ST-DOLR-COVRGE-LMT.                          ELGACLCC
00972      MOVE GAB-COINS-BENEFIT-PERIOD (GAB-INDEX)                    ELGACLCC
00973        TO     ACCUM-BENEFIT-PERIOD.                               ELGACLCC
00974      MOVE GAB-COINS-BEN-PER-TIME-FCTR (GAB-INDEX)                 ELGACLCC
00975        TO     ACCUM-BEN-PER-TIME-FCTR.                            ELGACLCC
00976      MOVE GAB-COINS-BEN-PER-TIME-QUAL (GAB-INDEX)                 ELGACLCC
00977        TO     ACCUM-BEN-PER-TIME-QUAL.                            ELGACLCC
00978      MOVE GAB-COINS-INTERVAL-TIME-FCTR (GAB-INDEX)                ELGACLCC
00979        TO     ACCUM-INTERVAL-TIME-FCTR.                           ELGACLCC
00980      MOVE GAB-COINS-INTERVAL-TYPE (GAB-INDEX)                     ELGACLCC
00981        TO     ACCUM-INTERVAL-TYPE.                                ELGACLCC
00982      MOVE GAB-COINS-INTERVAL-OVRD-IND (GAB-INDEX)                 ELGACLCC
00983        TO     ACCUM-INTERVAL-OVRD-IND.                            ELGACLCC
00984      MOVE GAB-COINS-INTERVAL-OVRD-VALUE (GAB-INDEX)               ELGACLCC
00985        TO     ACCUM-INTERVAL-OVRD-VALUE.                          ELGACLCC
00986      MOVE GAB-COINS-L-O-B (GAB-INDEX) TO ACCUM-L-O-B.             ELGACLCC
00987      MOVE GAB-COINS-REINSTATEMENT-IND (GAB-INDEX)                 ELGACLCC
00988        TO     ACCUM-REINSTATEMENT-IND.                            ELGACLCC
00989      MOVE GAB-COINS-DEFINITION (GAB-INDEX) TO ACCUM-DEFINITION.   ELGACLCC
00990      SET CARRY-OVER-CREDIT-IND-NA                                 ELGACLCC
00991       TO TRUE.                                                    ELGACLCC
00992      MOVE GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX)                ELGACLCC
00993        TO     ACCUM-ASCEND-DESCEND-IND.                           ELGACLCC
00994      MOVE GAB-COINS-CONDITION (GAB-INDEX) TO ACCUM-CONDITION.     ELGACLCC
00995      MOVE GAB-COINS-FAM-OR-INDIV (GAB-INDEX)                      ELGACLCC
00996        TO     ACCUM-FAM-OR-INDIV.                                 ELGACLCC
00997      SET DED-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELGACLCC
00998      SET OPX-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELGACLCC
00999      SET MAX-BASE-AMT-SOURCE-IND-NA TO TRUE.                      ELGACLCC
01000      MOVE GAB-COINS-VALUE-QUALIFIER (GAB-INDEX)                   ELGACLCC
01001        TO     ACCUM-VALUE-QUALIFIER.                              ELGACLCC
01002      MOVE GAB-COINS-RELATIONSHIP-IND (GAB-INDEX)                  ELGACLCC
01003        TO     ACCUM-RELATIONSHIP-IND.                             ELGACLCC
01004      MOVE GAB-COINS-AGE-LIMIT-FROM (GAB-INDEX)                    ELGACLCC
01005        TO     ACCUM-AGE-LIMIT-FROM-VAL.                           ELGACLCC
01006      MOVE GAB-COINS-AGE-QUAL-IND-FROM (GAB-INDEX)                 ELGACLCC
01007        TO     ACCUM-AGE-LIMIT-FROM-IND.                           ELGACLCC
01008      MOVE GAB-COINS-AGE-LIMIT-TO (GAB-INDEX)                      ELGACLCC
01009        TO     ACCUM-AGE-LIMIT-TO-VAL.                             ELGACLCC
01010      MOVE GAB-COINS-AGE-QUAL-IND-TO (GAB-INDEX)                   ELGACLCC
01011        TO     ACCUM-AGE-LIMIT-TO-IND.                             ELGACLCC
01012                                                                   ELGACLCC
01013 * -- SET OCCURRENCE PROVIDER CLASS INFORMATION                    ELGACLCC
01014      EVALUATE TRUE ALSO TRUE                                      ELGACLCC
01015         WHEN      SW-INTRNL-INST-PROV-CLASS                       ELGACLCC
01016              ALSO SW-INTRNL-NOT-PROF-PROV-CLASS                   ELGACLCC
01017            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELGACLCC
01018         WHEN      SW-INTRNL-NOT-INST-PROV-CLASS                   ELGACLCC
01019              ALSO SW-INTRNL-PROF-PROV-CLASS                       ELGACLCC
01020            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELGACLCC
01021         WHEN OTHER                                                ELGACLCC
01022            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELGACLCC
01023         END-EVALUATE.                                             ELGACLCC
01024                                                                   ELGACLCC
01025 * -- SET OCCURRENCE PROVIDER SPEC INFORMATION                     ELGACLCC
01026         IF SW-INTRNL-PROF-PROV-SPEC                               ELGACLCC
01027            SET ACCUM-PRVDR-SPC-PROF TO TRUE                       ELGACLCC
01028         END-IF.                                                   ELGACLCC
01029                                                                   ELGACLCC
01030 ************************************************************      ELGACLCC
01031 *                                                          *      ELGACLCC
01032 *        EXTRACT ACCUM VARIABLE PORTION                    *      ELGACLCC
01033 *                                                          *      ELGACLCC
01034 ************************************************************      ELGACLCC
01035                                                                   ELGACLCC
01036  0390-EXTRACT-ACCUM-VBL-PORTION.                                  ELGACLCC
01037      SET ASC-DES-INDEX TO 1.                                      ELGACLCC
01038      PERFORM 0400-EXTRACT-VARIABLE-PORTION.                       ELGACLCC
01039      IF ACCUM-VARIABLE-TYPE                                       ELGACLCC
01040         THEN                                                      ELGACLCC
01041             PERFORM 0410-EXTRACT-ADDL-OCCURNCS                    ELGACLCC
01042       END-IF.                                                     ELGACLCC
01043                                                                   ELGACLCC
01044                                                                   ELGACLCC
01045 /***********************************************************      ELGACLCC
01046 *                                                          *      ELGACLCC
01047 *        EXTRACT VARIABLE PORTION                          *      ELGACLCC
01048 *                                                          *      ELGACLCC
01049 ************************************************************      ELGACLCC
01050                                                                   ELGACLCC
01051  0400-EXTRACT-VARIABLE-PORTION.                                   ELGACLCC
01052      MOVE GAB-COINS-BISCENDING-IND (GAB-INDEX)                    ELGACLCC
01053        TO ACCUM-BISCEND-IND (ASC-DES-INDEX).                      ELGACLCC
01054      MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)                     ELGACLCC
01055        TO ACCUM-PERCENT-LEVEL (ASC-DES-INDEX).                    ELGACLCC
01056      MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                       ELGACLCC
01057        TO ACCUM-VALUE-LIMIT (ASC-DES-INDEX).                      ELGACLCC
01058      MOVE WS-IBGR-SLOT-NBR                                        ELGACLCC
01059        TO ACCUM-IBGR-SLOT-NBR (ASC-DES-INDEX).                    ELGACLCC
01060      MOVE WS-IDGD-SLOT-NBR                                        ELGACLCC
01061        TO ACCUM-IDGD-SLOT-NBR (ASC-DES-INDEX).                    ELGACLCC
01062      MOVE WS-IPGN-SLOT-NBR                                        ELGACLCC
01063        TO ACCUM-IPGN-SLOT-NBR (ASC-DES-INDEX).                    ELGACLCC
01064      MOVE WS-IPGP-SLOT-NBR                                        ELGACLCC
01065        TO ACCUM-IPGP-SLOT-NBR (ASC-DES-INDEX).                    ELGACLCC
01066      MOVE WS-IPGT-SLOT-NBR                                        ELGACLCC
01067        TO ACCUM-IPGT-SLOT-NBR (ASC-DES-INDEX).                    ELGACLCC
01068      MOVE WS-IPGS-SLOT-NBR                                        ELGACLCC
01069        TO ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX).                    ELGACLCC
01070      IF ACCUM-BISCEND-IND (ASC-DES-INDEX)                         ELGACLCC
01071         = ZERO OR SPACES OR LOW-VALUES                            ELGACLCC
01072         SET BISCEND-IND-NA (ASC-DES-INDEX) TO TRUE.               ELGACLCC
01073      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB) TO TRUE.     ELGACLCC
01074                                                                   ELGACLCC
01075                                                                   ELGACLCC
01076 /***********************************************************      ELGACLCC
01077 *                                                          *      ELGACLCC
01078 *        EXTRACT ADDITIONAL OCCURRENCES                    *      ELGACLCC
01079 *                                                          *      ELGACLCC
01080 ************************************************************      ELGACLCC
01081                                                                   ELGACLCC
01082  0410-EXTRACT-ADDL-OCCURNCS.                                      ELGACLCC
01083       MOVE WS-OCCURRENCE-SUB TO WS-SAVE-SUB                       ELGACLCC
01084       SET  WS-SAVE-INDEX    TO GAB-INDEX.                         ELGACLCC
01085       ADD 1 TO WS-OCCURRENCE-SUB.                                 ELGACLCC
01086       PERFORM 0420-TEST-SUBSEQ-OCCRNCES                           ELGACLCC
01087          VARYING WS-OCCURRENCE-SUB                                ELGACLCC
01088             FROM WS-OCCURRENCE-SUB BY 1                           ELGACLCC
01089          UNTIL WS-OCCURRENCE-INDEX >= GAB-ENTRY-COUNT.            ELGACLCC
01090       MOVE WS-SAVE-SUB TO WS-OCCURRENCE-SUB.                      ELGACLCC
01091       SET  GAB-INDEX   TO WS-SAVE-INDEX.                          ELGACLCC
01092                                                                   ELGACLCC
01093                                                                   ELGACLCC
01094                                                                   ELGACLCC
01095 /***********************************************************      ELGACLCC
01096 *                                                          *      ELGACLCC
01097 *        TEST SUBSEQUENT OCCURRENCES                       *      ELGACLCC
01098 *                                                          *      ELGACLCC
01099 ************************************************************      ELGACLCC
01100                                                                   ELGACLCC
01101  0420-TEST-SUBSEQ-OCCRNCES.                                       ELGACLCC
01102       SET GAB-INDEX                                               ELGACLCC
01103           WS-OCCURRENCE-INDEX TO WS-OCCURRENCE-SUB.               ELGACLCC
01104       IF WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB)              ELGACLCC
01105          CONTINUE                                                 ELGACLCC
01106       ELSE PERFORM 0430-TEST-OCCURRENCE.                          ELGACLCC
01107                                                                   ELGACLCC
01108                                                                   ELGACLCC
01109                                                                   ELGACLCC
01110 ************************************************************      ELGACLCC
01111 *                                                          *      ELGACLCC
01112 *        TEST OCCURRENCE                                   *      ELGACLCC
01113 *                                                          *      ELGACLCC
01114 ************************************************************      ELGACLCC
01115                                                                   ELGACLCC
01116  0430-TEST-OCCURRENCE.                                            ELGACLCC
01117       SET SW-MATCHING-ENTRY-NOT-FOUND TO TRUE.                    ELGACLCC
01118       PERFORM 0440-TEST-KEYS-FOR-MATCH.                           ELGACLCC
01119       IF SW-MATCHING-ENTRY-FOUND                                  ELGACLCC
01120          PERFORM 0450-COMPLETE-TEST-OF-OCCURNCE.                  ELGACLCC
01121                                                                   ELGACLCC
01122                                                                   ELGACLCC
01123                                                                   ELGACLCC
01124 /***********************************************************      ELGACLCC
01125 *                                                          *      ELGACLCC
01126 *        TEST KEYS FOR MATCH                               *      ELGACLCC
01127 *                                                          *      ELGACLCC
01128 ************************************************************      ELGACLCC
01129                                                                   ELGACLCC
01130  0440-TEST-KEYS-FOR-MATCH.                                        ELGACLCC
01131      IF GAB-COINS-BENEFIT-PERIOD (GAB-INDEX) =                    ELGACLCC
01132              ACCUM-BENEFIT-PERIOD                                 ELGACLCC
01133                        AND                                        ELGACLCC
01134              GAB-COINS-FAM-OR-INDIV (GAB-INDEX) =                 ELGACLCC
01135              ACCUM-FAM-OR-INDIV                                   ELGACLCC
01136                        AND                                        ELGACLCC
01137              GAB-COINS-L-O-B (GAB-INDEX) =                        ELGACLCC
01138              ACCUM-L-O-B                                          ELGACLCC
01139                        AND                                        ELGACLCC
01140              GAB-COINS-FYI-VALUE (GAB-INDEX) =                    ELGACLCC
01141              ACCUM-FYI-VALUE                                      ELGACLCC
01142                        AND                                        ELGACLCC
01143              GAB-COINS-SERVICE-GROUP (GAB-INDEX) =                ELGACLCC
01144              ACCUM-SERVICE-GROUP                                  ELGACLCC
01145                        AND                                        ELGACLCC
01146              GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX) =           ELGACLCC
01147              ACCUM-PLACE-OF-TREATMENT                             ELGACLCC
01148                        AND                                        ELGACLCC
01149              GAB-COND-ALL-BIT (GAB-INDEX) =                       ELGACLCC
01150              ACCUM-COND-ALL-BIT                                   ELGACLCC
01151                        AND                                        ELGACLCC
01152              GAB-COND-EXCLUSION-BIT (GAB-INDEX) =                 ELGACLCC
01153              ACCUM-COND-EXCLUSION-BIT                             ELGACLCC
01154                        AND                                        ELGACLCC
01155              GAB-COND-ICD-BIT (GAB-INDEX) =                       ELGACLCC
01156              ACCUM-COND-ICD-BIT                                   ELGACLCC
01157                        AND                                        ELGACLCC
01158              GAB-COND-TB-BIT (GAB-INDEX) =                        ELGACLCC
01159              ACCUM-COND-TB-BIT                                    ELGACLCC
01160                        AND                                        ELGACLCC
01161              GAB-COND-MENTAL-BIT (GAB-INDEX) =                    ELGACLCC
01162              ACCUM-COND-MENTAL-BIT                                ELGACLCC
01163                        AND                                        ELGACLCC
01164              GAB-COND-DRUG-BIT (GAB-INDEX) =                      ELGACLCC
01165              ACCUM-COND-DRUG-BIT                                  ELGACLCC
01166                        AND                                        ELGACLCC
01167              GAB-COND-ALCOHOL-BIT (GAB-INDEX) =                   ELGACLCC
01168              ACCUM-COND-ALCOHOL-BIT                               ELGACLCC
01169                        AND                                        ELGACLCC
01170              GAB-COND-OB-COMP-BIT (GAB-INDEX) =                   ELGACLCC
01171              ACCUM-COND-OB-COMP-BIT                               ELGACLCC
01172                        AND                                        ELGACLCC
01173              GAB-COND-OB-NORM-BIT (GAB-INDEX) =                   ELGACLCC
01174              ACCUM-COND-OB-NORM-BIT                               ELGACLCC
01175                        AND                                        ELGACLCC
01176              GAB-COND-MALIGNANCY-BIT (GAB-INDEX) =                ELGACLCC
01177              ACCUM-COND-MALIGNANCY-BIT                            ELGACLCC
01178                        AND                                        ELGACLCC
01179              GAB-COND-CARDIAC-DISEASE-BIT (GAB-INDEX) =           ELGACLCC
01180              ACCUM-COND-CARDIAC-DISEASE-BIT                       ELGACLCC
01181                        AND                                        ELGACLCC
01182              GAB-COND-LIFE-THREAT-BIT (GAB-INDEX) =               ELGACLCC
01183              ACCUM-COND-LIFE-THREAT-BIT                           ELGACLCC
01184                        AND                                        ELGACLCC
01185              GAB-COND-OBESITY-BIT (GAB-INDEX) =                   ELGACLCC
01186              ACCUM-COND-OBESITY-BIT                               ELGACLCC
01187                        AND                                        ELGACLCC
01188              GAB-COND-KIDNEY-DISEASE-BIT (GAB-INDEX) =            ELGACLCC
01189              ACCUM-COND-KIDNEY-DISEASE-BIT                        ELGACLCC
01190                        AND                                        ELGACLCC
01191              GAB-COND-ACCIDENT-BIT (GAB-INDEX) =                  ELGACLCC
01192              ACCUM-COND-ACCIDENT-BIT                              ELGACLCC
01193                        AND                                        ELGACLCC
01194              GAB-COND-PRE-EXIST-BIT (GAB-INDEX) =                 ELGACLCC
01195              ACCUM-COND-PRE-EXIST-BIT                             ELGACLCC
01196                        AND                                        ELGACLCC
01197              GAB-COND-NON-EMER-BIT (GAB-INDEX) =                  ELGACLCC
01198              ACCUM-COND-NON-EMER-BIT                              ELGACLCC
01199                        AND                                        ELGACLCC
01200              GAB-COND-SUICIDE-BIT (GAB-INDEX) =                   ELGACLCC
01201              ACCUM-COND-SUICIDE-BIT                               ELGACLCC
01202                        AND                                        ELGACLCC
01203              GAB-COND-TMJ-BIT (GAB-INDEX) =                       ELGACLCC
01204              ACCUM-COND-TMJ-BIT                                   ELGACLCC
01205                        AND                                        ELGACLCC
01206              GAB-COND-INF-BIT (GAB-INDEX) =                       ELGACLCC
01207              ACCUM-COND-INF-BIT                                   ELGACLCC
01208                        AND                                        ELGACLCC
01209              GAB-COINS-CO-PAY-IND (GAB-INDEX) =                   ELGACLCC
01210              ACCUM-CO-PAY-IND (COPAY-INDEX)                       ELGACLCC
01211                        AND                                        ELGACLCC
01212              GAB-COINS-COST-CONTAIN-IND (GAB-INDEX) =             ELGACLCC
01213              ACCUM-COST-CONTAIN-IND                               ELGACLCC
01214                        AND                                        ELGACLCC
01215              GAB-COINS-LMT-MANDATORY-IND (GAB-INDEX) =            ELGACLCC
01216              ACCUM-LMT-MANDATORY-IND                              ELGACLCC
01217                        AND                                        ELGACLCC
01218              GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX) =           ELGACLCC
01219              ACCUM-ASCEND-DESCEND-IND                             ELGACLCC
01220                        AND                                        ELGACLCC
01221              GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX) =          ELGACLCC
01222              ACCUM-INTERNAL-DESCRIPTOR                            ELGACLCC
01223                        AND                                        ELGACLCC
01224              GAB-COINS-VALUE-QUALIFIER (GAB-INDEX) =              ELGACLCC
01225              ACCUM-VALUE-QUALIFIER                                ELGACLCC
01226          THEN                                                     ELGACLCC
01227          SET SW-MATCHING-ENTRY-FOUND TO TRUE.                     ELGACLCC
01228                                                                   ELGACLCC
01229                                                                   ELGACLCC
01230                                                                   ELGACLCC
01231                                                                   ELGACLCC
01232 /***********************************************************      ELGACLCC
01233 *                                                          *      ELGACLCC
01234 *        COMPLETE TEST OF OCCURRENCE                       *      ELGACLCC
01235 *                                                          *      ELGACLCC
01236 ************************************************************      ELGACLCC
01237                                                                   ELGACLCC
01238  0450-COMPLETE-TEST-OF-OCCURNCE.                                  ELGACLCC
01239      PERFORM 0310-INITIALIZE-OCCURRENCE.                          ELGACLCC
01240      PERFORM 0320-SCAN-FOR-INTERNALS                              ELGACLCC
01241      IF SW-OCCRNC-APPLIES                                         ELGACLCC
01242         PERFORM 0460-EXTRCT-NXT-OCCURNCE-DATA                     ELGACLCC
01243      ELSE                                                         ELGACLCC
01244      SET WS-OCCURRENCE-PROCESSED (WS-OCCURRENCE-SUB) TO TRUE.     ELGACLCC
01245                                                                   ELGACLCC
01246                                                                   ELGACLCC
01247                                                                   ELGACLCC
01248 ************************************************************      ELGACLCC
01249 *                                                          *      ELGACLCC
01250 *        EXTRACT NEXT OF OCCURRENCE DATA                   *      ELGACLCC
01251 *                                                          *      ELGACLCC
01252 ************************************************************      ELGACLCC
01253                                                                   ELGACLCC
01254  0460-EXTRCT-NXT-OCCURNCE-DATA.                                   ELGACLCC
01255      ADD +1 TO ACCUM-ASCEND-DESCEND-COUNT.                        ELGACLCC
01256      SET ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.             ELGACLCC
01257      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (ASC-DES-INDEX).       ELGACLCC
01258      PERFORM 0400-EXTRACT-VARIABLE-PORTION.                       ELGACLCC
01259      PERFORM 0470-INSERT-NEW-ENTRY.                               ELGACLCC
01260                                                                   ELGACLCC
01261                                                                   ELGACLCC
01262                                                                   ELGACLCC
01263 /***********************************************************      ELGACLCC
01264 *                                                          *      ELGACLCC
01265 *        INSERT NEW ENTRY                                  *      ELGACLCC
01266 *                                                          *      ELGACLCC
01267 ************************************************************      ELGACLCC
01268                                                                   ELGACLCC
01269  0470-INSERT-NEW-ENTRY.                                           ELGACLCC
01270      MOVE ACCUM-ASCEND-DESCEND-COUNT TO SORT-SUB.                 ELGACLCC
01271      SET SW-SORT-NOT-COMPLETED TO TRUE.                           ELGACLCC
01272      IF SORT-SUB = 1                                              ELGACLCC
01273         CONTINUE                                                  ELGACLCC
01274      ELSE IF ACCUM-ASCEND-ORDER                                   ELGACLCC
01275              PERFORM 0480-ASCEND-INSERT                           ELGACLCC
01276                UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1            ELGACLCC
01277           ELSE IF ACCUM-DESCEND-ORDER                             ELGACLCC
01278                   PERFORM 0490-DESCEND-INSERT                     ELGACLCC
01279                     UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1       ELGACLCC
01280                ELSE IF ACCUM-BISCEND-ORDER                        ELGACLCC
01281                        PERFORM 0500-BISCEND-INSERT                ELGACLCC
01282                          UNTIL SW-SORT-COMPLETED OR SORT-SUB = 1. ELGACLCC
01283                                                                   ELGACLCC
01284                                                                   ELGACLCC
01285                                                                   ELGACLCC
01286 /***********************************************************      ELGACLCC
01287 *                                                          *      ELGACLCC
01288 *        ASCEND INSERT                                     *      ELGACLCC
01289 *                                                          *      ELGACLCC
01290 ************************************************************      ELGACLCC
01291                                                                   ELGACLCC
01292  0480-ASCEND-INSERT.                                              ELGACLCC
01293      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELGACLCC
01294      IF ACCUM-PERCENT-LEVEL (SORT-SUB) <                          ELGACLCC
01295         ACCUM-PERCENT-LEVEL (TEST-SUB)                            ELGACLCC
01296         PERFORM 0510-SWAP-ENTRIES                                 ELGACLCC
01297      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELGACLCC
01298                                                                   ELGACLCC
01299                                                                   ELGACLCC
01300                                                                   ELGACLCC
01301 ************************************************************      ELGACLCC
01302 *                                                          *      ELGACLCC
01303 *        DESCEND INSERT                                    *      ELGACLCC
01304 *                                                          *      ELGACLCC
01305 ************************************************************      ELGACLCC
01306                                                                   ELGACLCC
01307  0490-DESCEND-INSERT.                                             ELGACLCC
01308      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELGACLCC
01309      IF ACCUM-PERCENT-LEVEL (SORT-SUB) >                          ELGACLCC
01310         ACCUM-PERCENT-LEVEL (TEST-SUB)                            ELGACLCC
01311         PERFORM 0510-SWAP-ENTRIES                                 ELGACLCC
01312      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELGACLCC
01313                                                                   ELGACLCC
01314                                                                   ELGACLCC
01315                                                                   ELGACLCC
01316 /***********************************************************      ELGACLCC
01317 *                                                          *      ELGACLCC
01318 *        BISCEND INSERT                                    *      ELGACLCC
01319 *                                                          *      ELGACLCC
01320 ************************************************************      ELGACLCC
01321                                                                   ELGACLCC
01322  0500-BISCEND-INSERT.                                             ELGACLCC
01323      COMPUTE TEST-SUB = SORT-SUB - 1.                             ELGACLCC
01324      IF ACCUM-BISCEND-IND (SORT-SUB) <                            ELGACLCC
01325         ACCUM-BISCEND-IND (TEST-SUB)                              ELGACLCC
01326         PERFORM 0510-SWAP-ENTRIES                                 ELGACLCC
01327      ELSE SET SW-SORT-COMPLETED TO TRUE.                          ELGACLCC
01328                                                                   ELGACLCC
01329                                                                   ELGACLCC
01330                                                                   ELGACLCC
01331 ************************************************************      ELGACLCC
01332 *                                                          *      ELGACLCC
01333 *        SWAP ENTRIES                                      *      ELGACLCC
01334 *                                                          *      ELGACLCC
01335 ************************************************************      ELGACLCC
01336                                                                   ELGACLCC
01337  0510-SWAP-ENTRIES.                                               ELGACLCC
01338      MOVE ACCUM-ASCEND-DESCEND-ENTRY (SORT-SUB)                   ELGACLCC
01339        TO WS-ASCEND-DESCEND-ENTRY-HOLD.                           ELGACLCC
01340      MOVE ACCUM-ASCEND-DESCEND-ENTRY (TEST-SUB)                   ELGACLCC
01341        TO ACCUM-ASCEND-DESCEND-ENTRY (SORT-SUB).                  ELGACLCC
01342      MOVE WS-ASCEND-DESCEND-ENTRY-HOLD                            ELGACLCC
01343        TO ACCUM-ASCEND-DESCEND-ENTRY (TEST-SUB).                  ELGACLCC
01344      MOVE TEST-SUB TO SORT-SUB.                                   ELGACLCC
01345                                                                   ELGACLCC
01346                                                                   ELGACLCC
01347 /***********************************************************      ELGACLCC
01348 *                                                          *      ELGACLCC
01349 *    CHECK EXTRACT DATA INTEGRITY                          *      ELGACLCC
01350 *                                                          *      ELGACLCC
01351 ************************************************************      ELGACLCC
01352                                                                   ELGACLCC
01353  0520-CHK-EXTRACT-DATA-INTGRTY.                                   ELGACLCC
01354      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELGACLCC
01355      THEN                                                         ELGACLCC
01356         SET FYI-VALUE-NA TO TRUE                                  ELGACLCC
01357      END-IF.                                                      ELGACLCC
01358                                                                   ELGACLCC
01359      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELGACLCC
01360      THEN                                                         ELGACLCC
01361         SET COST-CONTAIN-IND-NA TO TRUE                           ELGACLCC
01362      END-IF.                                                      ELGACLCC
01363                                                                   ELGACLCC
01364      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELGACLCC
01365      THEN                                                         ELGACLCC
01366         SET PLACE-OF-TREATMENT-NA TO TRUE                         ELGACLCC
01367      END-IF.                                                      ELGACLCC
01368                                                                   ELGACLCC
01369      IF ACCUM-BEN-PER-TIME-QUAL = ZEROS OR SPACES OR LOW-VALUES   ELGACLCC
01370      THEN                                                         ELGACLCC
01371         SET BEN-PER-TIME-QUAL-NA TO TRUE                          ELGACLCC
01372      END-IF.                                                      ELGACLCC
01373                                                                   ELGACLCC
01374      IF ACCUM-INTERVAL-TYPE = ZEROS OR SPACES OR LOW-VALUES       ELGACLCC
01375      THEN                                                         ELGACLCC
01376         SET INTERVAL-TYPE-NA TO TRUE                              ELGACLCC
01377      END-IF.                                                      ELGACLCC
01378                                                                   ELGACLCC
01379      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELGACLCC
01380      THEN                                                         ELGACLCC
01381         SET INTERVAL-OVRD-IND-NA TO TRUE                          ELGACLCC
01382      END-IF.                                                      ELGACLCC
01383                                                                   ELGACLCC
01384      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELGACLCC
01385      THEN                                                         ELGACLCC
01386         SET L-O-B-NA TO TRUE                                      ELGACLCC
01387      END-IF.                                                      ELGACLCC
01388                                                                   ELGACLCC
01389      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELGACLCC
01390      THEN                                                         ELGACLCC
01391         SET REINSTATEMENT-IND-NA TO TRUE                          ELGACLCC
01392      END-IF.                                                      ELGACLCC
01393                                                                   ELGACLCC
01394      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELGACLCC
01395      THEN                                                         ELGACLCC
01396         SET DEFINITION-NA TO TRUE                                 ELGACLCC
01397      END-IF.                                                      ELGACLCC
01398                                                                   ELGACLCC
01399      IF   ACCUM-CARRY-OVER-CREDIT-IND                             ELGACLCC
01400         = ZEROS OR SPACES OR LOW-VALUES                           ELGACLCC
01401      THEN                                                         ELGACLCC
01402         SET CARRY-OVER-CREDIT-IND-NA TO TRUE                      ELGACLCC
01403      END-IF.                                                      ELGACLCC
01404                                                                   ELGACLCC
01405      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELGACLCC
01406      THEN                                                         ELGACLCC
01407         SET ASCEND-DESCEND-IND-NA TO TRUE                         ELGACLCC
01408      END-IF.                                                      ELGACLCC
01409                                                                   ELGACLCC
01410      IF ACCUM-FAM-OR-INDIV = ZEROS OR SPACES OR LOW-VALUES        ELGACLCC
01411      THEN                                                         ELGACLCC
01412         SET FAM-OR-INDIV-NA TO TRUE                               ELGACLCC
01413      END-IF.                                                      ELGACLCC
01414                                                                   ELGACLCC
01415      IF ACCUM-VALUE-QUALIFIER = ZEROS OR SPACES OR LOW-VALUES     ELGACLCC
01416      THEN                                                         ELGACLCC
01417         SET VALUE-QUALIFIER-NA TO TRUE                            ELGACLCC
01418      END-IF.                                                      ELGACLCC
01419                                                                   ELGACLCC
01420      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELGACLCC
01421      THEN                                                         ELGACLCC
01422         SET RELATIONSHIP-IND-NA TO TRUE                           ELGACLCC
01423      END-IF.                                                      ELGACLCC
01424                                                                   ELGACLCC
01425      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELGACLCC
01426      THEN                                                         ELGACLCC
01427         SET AGE-LMT-TO-IND-NA TO TRUE                             ELGACLCC
01428      END-IF.                                                      ELGACLCC
01429                                                                   ELGACLCC
01430      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELGACLCC
01431      THEN                                                         ELGACLCC
01432         SET AGE-LMT-FROM-IND-NA TO TRUE                           ELGACLCC
01433      END-IF.                                                      ELGACLCC
01434                                                                   ELGACLCC
01435      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELGACLCC
01436      THEN                                                         ELGACLCC
01437         SET LMT-MANDATORY-IND-NA TO TRUE                          ELGACLCC
01438      END-IF.                                                      ELGACLCC
01439                                                                   ELGACLCC
01440      IF ACCUM-CO-PAY-IND(COPAY-INDEX)                             ELGACLCC
01441                          = ZEROS OR SPACES OR LOW-VALUES          ELGACLCC
01442         SET CO-PAY-IND-NA (COPAY-INDEX) TO TRUE                   ELGACLCC
01443      END-IF.                                                      ELGACLCC
01444                                                                   ELGACLCC
01445      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELGACLCC
01446         SET SERVICE-GROUP-NA TO TRUE                              ELGACLCC
01447      END-IF.                                                      ELGACLCC
01448                                                                   ELGACLCC
01449      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELGACLCC
01450         SET INTERNAL-DESCRIPTOR-NA TO TRUE                        ELGACLCC
01451      END-IF.                                                      ELGACLCC
01452                                                                   ELGACLCC
01453      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELGACLCC
01454         SET DAY-FACTOR-IND-NA TO TRUE                             ELGACLCC
01455      END-IF.                                                      ELGACLCC
01456                                                                   ELGACLCC
01457      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELGACLCC
01458         SET CLAIM-LVL-ACCUM-IND-NA TO TRUE                        ELGACLCC
01459      END-IF.                                                      ELGACLCC
01460                                                                   ELGACLCC
01461      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELGACLCC
01462      THEN                                                         ELGACLCC
01463         SET BEN-PER-MAX-OVRD-IND-NA TO TRUE                       ELGACLCC
01464      END-IF.                                                      ELGACLCC
01465                                                                   ELGACLCC
01466      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELGACLCC
01467      THEN                                                         ELGACLCC
01468         SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE                        ELGACLCC
01469      END-IF.                                                      ELGACLCC
01470                                                                   ELGACLCC
01471                                                                   ELGACLCC
01472 /***********************************************************      ELGACLCC
01473 *                                                          *      ELGACLCC
01474 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER SPEC    *      ELGACLCC
01475 *                                                          *      ELGACLCC
01476 ************************************************************      ELGACLCC
01477                                                                   ELGACLCC
01478  0551-CHK-INTRNL-TAB-PROV-SPEC.                                   ELGACLCC
01479      IF SW-HAS-IPGS                                               ELGACLCC
01480         PERFORM 0561-CHK-IPGS-PROV-SPEC                           ELGACLCC
01481      END-IF.                                                      ELGACLCC
01482                                                                   ELGACLCC
01483 /***********************************************************      ELGACLCC
01484 *                                                          *      ELGACLCC
01485 *    CHECK INTERNAL TABULARS TO DETERMINE PROVIDER CLASS   *      ELGACLCC
01486 *                                                          *      ELGACLCC
01487 ************************************************************      ELGACLCC
01488                                                                   ELGACLCC
01489  0550-CHK-INTRNL-TAB-PROV-CLASS.                                  ELGACLCC
01490      IF SW-HAS-IPGT                                               ELGACLCC
01491      THEN                                                         ELGACLCC
01492         PERFORM 0560-CHK-IPGT-PROV-CLASS                          ELGACLCC
01493      ELSE                                                         ELGACLCC
01494         IF SW-HAS-IBGR                                            ELGACLCC
01495         THEN                                                      ELGACLCC
01496            PERFORM 0640-CHK-IBGR-PROV-CLASS                       ELGACLCC
01497         ELSE                                                      ELGACLCC
01498            SET SW-OCCRNC-APPLIES TO TRUE                          ELGACLCC
01499         END-IF                                                    ELGACLCC
01500      END-IF.                                                      ELGACLCC
01501                                                                   ELGACLCC
01502 /*****************************************************************ELGACLCC
01503 *                                                                *ELGACLCC
01504 *    CHECK IPGT INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELGACLCC
01505 *                                                                *ELGACLCC
01506 ******************************************************************ELGACLCC
01507                                                                   ELGACLCC
01508  0560-CHK-IPGT-PROV-CLASS.                                        ELGACLCC
01509      MOVE PC-IPGT TO KWA-PROVISION-ID.                            ELGACLCC
01510      MOVE WS-IPGT-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGACLCC
01511      PERFORM 0650-READ-INTRLN-TAB.                                ELGACLCC
01512      SET ADDRESS OF GX3-RECORD-AREA TO IOP-REC-PTR.               ELGACLCC
01513      SET IOP-REC-PTR                TO NULLS.                     ELGACLCC
01514      SET GX3-INDEX                  TO GX3-ENTRY-COUNT.           ELGACLCC
01515      SET WS-MAX-GX3-INDEX           TO GX3-INDEX.                 ELGACLCC
01516                                                                   ELGACLCC
01517      IF GX3-ID-ARGUMENT-INCLUDED                                  ELGACLCC
01518      THEN                                                         ELGACLCC
01519         PERFORM 0570-CHK-INCLD-TYPE-IPGT                          ELGACLCC
01520      ELSE                                                         ELGACLCC
01521          PERFORM 0600-CHK-EXCLD-TYPE-IPGT                         ELGACLCC
01522      END-IF.                                                      ELGACLCC
01523                                                                   ELGACLCC
01524 /*****************************************************************ELGACLCC
01525 *                                                                *ELGACLCC
01526 *    CHECK IPGS INTERNAL TABULAR TO DETERMINE PROVIDER SPEC      *ELGACLCC
01527 *                                                                *ELGACLCC
01528 ******************************************************************ELGACLCC
01529  0561-CHK-IPGS-PROV-SPEC.                                         ELGACLCC
01530      MOVE PC-IPGS TO KWA-PROVISION-ID.                            ELGACLCC
01531      MOVE WS-IPGS-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGACLCC
01532      PERFORM 0650-READ-INTRLN-TAB.                                ELGACLCC
01533      SET ADDRESS OF GXS-RECORD-AREA TO IOP-REC-PTR.               ELGACLCC
01534      SET IOP-REC-PTR                TO NULLS.                     ELGACLCC
01535      SET GXS-INDEX                  TO GXS-ENTRY-COUNT.           ELGACLCC
01536      SET WS-MAX-GXS-INDEX           TO GXS-INDEX.                 ELGACLCC
01537                                                                   ELGACLCC
01538      IF GXS-ID-ARGUMENT-INCLUDED                                  ELGACLCC
01539         PERFORM 0571-CHK-INCLD-TYPE-IPGS                          ELGACLCC
01540      ELSE                                                         ELGACLCC
01541          PERFORM 0601-CHK-EXCLD-TYPE-IPGS                         ELGACLCC
01542      END-IF.                                                      ELGACLCC
01543                                                                   ELGACLCC
01544 ************************************************************      ELGACLCC
01545 *                                                          *      ELGACLCC
01546 *    CHECK INCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGACLCC
01547 *                                                          *      ELGACLCC
01548 ************************************************************      ELGACLCC
01549                                                                   ELGACLCC
01550  0570-CHK-INCLD-TYPE-IPGT.                                        ELGACLCC
01551      SET CFT2-IDX TO 1.                                           ELGACLCC
01552      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGACLCC
01553          SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGACLCC
01554       TO TRUE.                                                    ELGACLCC
01555      PERFORM 0580-TEST-IPGT-INCLD-ENTRIES                         ELGACLCC
01556         VARYING GX3-INDEX  FROM 1 BY 1                            ELGACLCC
01557           UNTIL    GX3-INDEX = WS-MAX-GX3-INDEX                   ELGACLCC
01558                 OR (    SW-INTRNL-INST-PROV-CLASS                 ELGACLCC
01559                     AND SW-INTRNL-PROF-PROV-CLASS ).              ELGACLCC
01560                                                                   ELGACLCC
01561 ************************************************************      ELGACLCC
01562 *                                                          *      ELGACLCC
01563 *    CHECK INCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGACLCC
01564 *                                                          *      ELGACLCC
01565 ************************************************************      ELGACLCC
01566                                                                   ELGACLCC
01567  0571-CHK-INCLD-TYPE-IPGS.                                        ELGACLCC
01568      SET CFT2-IDX TO 1.                                           ELGACLCC
01569      SET SW-INTRNL-NOT-PROF-PROV-SPEC                             ELGACLCC
01570       TO TRUE.                                                    ELGACLCC
01571      PERFORM 0581-TEST-IPGS-INCLD-ENTRIES                         ELGACLCC
01572         VARYING GXS-INDEX  FROM 1 BY 1                            ELGACLCC
01573           UNTIL    GXS-INDEX = WS-MAX-GXS-INDEX                   ELGACLCC
01574                 OR (    SW-INTRNL-PROF-PROV-SPEC).                ELGACLCC
01575                                                                   ELGACLCC
01576 /***********************************************************      ELGACLCC
01577 *                                                          *      ELGACLCC
01578 *    TEST IPGT INCLUDE ENTRIES TO DETERMINE PROVIDER CLASS *      ELGACLCC
01579 *                                                          *      ELGACLCC
01580 *    NOTE: FOR PERFORMANCE IMPROVEMENT, THE SEARCH OF THE  *      ELGACLCC
01581 *          CFT2 TABLE IS RESUMED AT THE NEXT LOGICAL       *      ELGACLCC
01582 *          ENTRY.  THE ASSUMPTION IS THAT THE CONTENT      *      ELGACLCC
01583 *          OF THE IPGT TABULAR ARE IN ALPHNUMERIC ORDER.   *      ELGACLCC
01584 *                                                          *      ELGACLCC
01585 ************************************************************      ELGACLCC
01586                                                                   ELGACLCC
01587  0580-TEST-IPGT-INCLD-ENTRIES.                                    ELGACLCC
01588      PERFORM WITH TEST BEFORE                                     ELGACLCC
01589         UNTIL    SW-OCCRNC-APPLIES                                ELGACLCC
01590               OR   CFT2-PT (CFT2-IDX)                             ELGACLCC
01591                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELGACLCC
01592               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELGACLCC
01593         IF   GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELGACLCC
01594            = CFT2-PT (CFT2-IDX)                                   ELGACLCC
01595         THEN                                                      ELGACLCC
01596 *    -- TEST PROVIDER CLASS                                       ELGACLCC
01597            EVALUATE TRUE                                          ELGACLCC
01598               WHEN CFT2-PT-INST (CFT2-IDX)                        ELGACLCC
01599                  SET SW-INTRNL-INST-PROV-CLASS TO TRUE            ELGACLCC
01600                  IF SRP-ACCUM-PROV-CLASS-INST                     ELGACLCC
01601                  THEN                                             ELGACLCC
01602                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGACLCC
01603                  END-IF                                           ELGACLCC
01604               WHEN CFT2-PT-PROF (CFT2-IDX)                        ELGACLCC
01605                  SET SW-INTRNL-PROF-PROV-CLASS TO TRUE            ELGACLCC
01606                  IF SRP-ACCUM-PROV-CLASS-PROF                     ELGACLCC
01607                  THEN                                             ELGACLCC
01608                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGACLCC
01609                  END-IF                                           ELGACLCC
01610               END-EVALUATE                                        ELGACLCC
01611         ELSE                                                      ELGACLCC
01612            CONTINUE                                               ELGACLCC
01613         END-IF                                                    ELGACLCC
01614 *    -- BUMP TO NEXT CFT2 TABLE ENTRY                             ELGACLCC
01615         SET CFT2-IDX UP BY 1                                      ELGACLCC
01616         END-PERFORM.                                              ELGACLCC
01617                                                                   ELGACLCC
01618  0581-TEST-IPGS-INCLD-ENTRIES.                                    ELGACLCC
01619      PERFORM WITH TEST BEFORE                                     ELGACLCC
01620         UNTIL    SW-OCCRNC-APPLIES                                ELGACLCC
01621               OR   CFT9-PT (CFT2-IDX)                             ELGACLCC
01622                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELGACLCC
01623               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELGACLCC
01624         IF   GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELGACLCC
01625            = CFT9-PT (CFT9-IDX)                                   ELGACLCC
01626         THEN                                                      ELGACLCC
01627 *    -- TEST PROVIDER CLASS                                       ELGACLCC
01628            EVALUATE TRUE                                          ELGACLCC
01629               WHEN CFT9-PT-PROF (CFT9-IDX)                        ELGACLCC
01630                  SET SW-INTRNL-PROF-PROV-SPEC TO TRUE             ELGACLCC
01631                  IF SRP-ACCUM-PROV-CLASS-PROF                     ELGACLCC
01632                     SET SW-OCCRNC-APPLIES TO TRUE                 ELGACLCC
01633                  END-IF                                           ELGACLCC
01634               END-EVALUATE                                        ELGACLCC
01635         ELSE                                                      ELGACLCC
01636            CONTINUE                                               ELGACLCC
01637         END-IF                                                    ELGACLCC
01638 *    -- BUMP TO NEXT CFT9 TABLE ENTRY                             ELGACLCC
01639         SET CFT9-IDX UP BY 1                                      ELGACLCC
01640         END-PERFORM.                                              ELGACLCC
01641                                                                   ELGACLCC
01642 /***********************************************************      ELGACLCC
01643 *                                                          *      ELGACLCC
01644 *    CHECK EXCLUDE TYPE IPGT TO DETERMINE PROVIDER CLASS   *      ELGACLCC
01645 *                                                          *      ELGACLCC
01646 ************************************************************      ELGACLCC
01647                                                                   ELGACLCC
01648  0600-CHK-EXCLD-TYPE-IPGT.                                        ELGACLCC
01649                                                                   ELGACLCC
01650 * -- INITIALIZE CFT2 TABLE TO INCLUDE ALL PROVIDER TYPES          ELGACLCC
01651      PERFORM WITH TEST BEFORE                                     ELGACLCC
01652         VARYING CFT2-IDX FROM 1 BY 1                              ELGACLCC
01653           UNTIL CFT2-IDX > CFT2-NBR-TBL-ENTRIES                   ELGACLCC
01654         SET  CFT2-PT-INCLUDE (CFT2-IDX) TO TRUE                   ELGACLCC
01655         END-PERFORM.                                              ELGACLCC
01656                                                                   ELGACLCC
01657 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGT                 ELGACLCC
01658      SET  CFT2-IDX TO 1.                                          ELGACLCC
01659      PERFORM 0610-TAG-EXCLD-IPGT-ENTRIES                          ELGACLCC
01660         VARYING GX3-INDEX FROM 1 BY 1                             ELGACLCC
01661           UNTIL GX3-INDEX = WS-MAX-GX3-INDEX.                     ELGACLCC
01662                                                                   ELGACLCC
01663 * -- CHECK CFT2 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELGACLCC
01664      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGACLCC
01665          SW-INTRNL-NOT-PROF-PROV-CLASS                            ELGACLCC
01666       TO TRUE.                                                    ELGACLCC
01667      PERFORM 0630-CHK-CFT2-NOT-EXCLD                              ELGACLCC
01668         VARYING CFT2-IDX FROM 1 BY 1                              ELGACLCC
01669           UNTIL    (    SW-INTRNL-INST-PROV-CLASS                 ELGACLCC
01670                     AND SW-INTRNL-PROF-PROV-CLASS )               ELGACLCC
01671                 OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES.               ELGACLCC
01672                                                                   ELGACLCC
01673 /***********************************************************      ELGACLCC
01674 *                                                          *      ELGACLCC
01675 *    CHECK EXCLUDE TYPE IPGS TO DETERMINE PROVIDER SPEC    *      ELGACLCC
01676 *                                                          *      ELGACLCC
01677 ************************************************************      ELGACLCC
01678                                                                   ELGACLCC
01679  0601-CHK-EXCLD-TYPE-IPGS.                                        ELGACLCC
01680                                                                   ELGACLCC
01681 * -- INITIALIZE CFT9 TABLE TO INCLUDE ALL PROVIDER TYPES          ELGACLCC
01682      PERFORM WITH TEST BEFORE                                     ELGACLCC
01683         VARYING CFT9-IDX FROM 1 BY 1                              ELGACLCC
01684           UNTIL CFT9-IDX > CFT9-NBR-TBL-ENTRIES                   ELGACLCC
01685         SET  CFT9-PT-INCLUDE (CFT9-IDX) TO TRUE                   ELGACLCC
01686         END-PERFORM.                                              ELGACLCC
01687                                                                   ELGACLCC
01688 * -- TAG ALL PROVIDER TYPES EXCLUDED BY THIS IPGT                 ELGACLCC
01689      SET  CFT9-IDX TO 1.                                          ELGACLCC
01690      PERFORM 0611-TAG-EXCLD-IPGS-ENTRIES                          ELGACLCC
01691         VARYING GXS-INDEX FROM 1 BY 1                             ELGACLCC
01692           UNTIL GXS-INDEX = WS-MAX-GXS-INDEX.                     ELGACLCC
01693                                                                   ELGACLCC
01694 * -- CHECK CFT9 TABLE FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDEDELGACLCC
01695      SET SW-INTRNL-NOT-PROF-PROV-SPEC                             ELGACLCC
01696       TO TRUE.                                                    ELGACLCC
01697      PERFORM 0631-CHK-CFT9-NOT-EXCLD                              ELGACLCC
01698         VARYING CFT9-IDX FROM 1 BY 1                              ELGACLCC
01699           UNTIL        SW-INTRNL-PROF-PROV-SPEC                   ELGACLCC
01700                 OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES.               ELGACLCC
01701                                                                   ELGACLCC
01702 /***********************************************************      ELGACLCC
01703 *                                                          *      ELGACLCC
01704 *    TAG EXCLUDED IPGT ENTRIES IN CFT2                     *      ELGACLCC
01705 *                                                          *      ELGACLCC
01706 ************************************************************      ELGACLCC
01707                                                                   ELGACLCC
01708  0610-TAG-EXCLD-IPGT-ENTRIES.                                     ELGACLCC
01709      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELGACLCC
01710      PERFORM WITH TEST BEFORE                                     ELGACLCC
01711         UNTIL    SW-ENTRY-FOUND                                   ELGACLCC
01712               OR   CFT2-PT (CFT2-IDX)                             ELGACLCC
01713                  > GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)         ELGACLCC
01714               OR CFT2-IDX > CFT2-NBR-TBL-ENTRIES                  ELGACLCC
01715         IF   CFT2-PT(CFT2-IDX)                                    ELGACLCC
01716            = GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)               ELGACLCC
01717         THEN                                                      ELGACLCC
01718            SET SW-ENTRY-FOUND TO TRUE                             ELGACLCC
01719            SET CFT2-PT-EXCLUDE (CFT2-IDX) TO TRUE                 ELGACLCC
01720            SET CFT2-IDX UP BY 1                                   ELGACLCC
01721         ELSE                                                      ELGACLCC
01722            SET CFT2-IDX UP BY 1                                   ELGACLCC
01723         END-IF                                                    ELGACLCC
01724         END-PERFORM.                                              ELGACLCC
01725                                                                   ELGACLCC
01726 /***********************************************************      ELGACLCC
01727 *                                                          *      ELGACLCC
01728 *    TAG EXCLUDED IPGS ENTRIES IN CFT9                     *      ELGACLCC
01729 *                                                          *      ELGACLCC
01730 ************************************************************      ELGACLCC
01731                                                                   ELGACLCC
01732  0611-TAG-EXCLD-IPGS-ENTRIES.                                     ELGACLCC
01733      SET SW-ENTRY-NOT-FOUND TO TRUE.                              ELGACLCC
01734      PERFORM WITH TEST BEFORE                                     ELGACLCC
01735         UNTIL    SW-ENTRY-FOUND                                   ELGACLCC
01736               OR   CFT9-PT (CFT2-IDX)                             ELGACLCC
01737                  > GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)         ELGACLCC
01738               OR CFT9-IDX > CFT9-NBR-TBL-ENTRIES                  ELGACLCC
01739         IF   CFT9-PT(CFT9-IDX)                                    ELGACLCC
01740            = GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)               ELGACLCC
01741         THEN                                                      ELGACLCC
01742            SET SW-ENTRY-FOUND TO TRUE                             ELGACLCC
01743            SET CFT9-PT-EXCLUDE (CFT9-IDX) TO TRUE                 ELGACLCC
01744            SET CFT9-IDX UP BY 1                                   ELGACLCC
01745         ELSE                                                      ELGACLCC
01746            SET CFT9-IDX UP BY 1                                   ELGACLCC
01747         END-IF                                                    ELGACLCC
01748         END-PERFORM.                                              ELGACLCC
01749                                                                   ELGACLCC
01750 /*****************************************************************ELGACLCC
01751 *                                                                *ELGACLCC
01752 *    CHECK CFT2 FOR CLASS(ES) OF PROVIDER TYPES NOT EXCLUDED     *ELGACLCC
01753 *                                                                *ELGACLCC
01754 ******************************************************************ELGACLCC
01755                                                                   ELGACLCC
01756  0630-CHK-CFT2-NOT-EXCLD.                                         ELGACLCC
01757      IF CFT2-PT-INCLUDE (CFT2-IDX)                                ELGACLCC
01758      THEN                                                         ELGACLCC
01759         EVALUATE TRUE                                             ELGACLCC
01760            WHEN CFT2-PT-INST (CFT2-IDX)                           ELGACLCC
01761               SET SW-INTRNL-INST-PROV-CLASS TO TRUE               ELGACLCC
01762               IF SRP-ACCUM-PROV-CLASS-INST                        ELGACLCC
01763               THEN                                                ELGACLCC
01764                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGACLCC
01765               END-IF                                              ELGACLCC
01766            WHEN CFT2-PT-PROF (CFT2-IDX)                           ELGACLCC
01767               SET SW-INTRNL-PROF-PROV-CLASS TO TRUE               ELGACLCC
01768               IF SRP-ACCUM-PROV-CLASS-PROF                        ELGACLCC
01769               THEN                                                ELGACLCC
01770                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGACLCC
01771               END-IF                                              ELGACLCC
01772            END-EVALUATE                                           ELGACLCC
01773      END-IF.                                                      ELGACLCC
01774                                                                   ELGACLCC
01775 /*****************************************************************ELGACLCC
01776 *                                                                *ELGACLCC
01777 *    CHECK CFT9 FOR CLASS(ES) OF PROVIDER SPEC NOT EXCLUDED     * ELGACLCC
01778 *                                                                *ELGACLCC
01779 ******************************************************************ELGACLCC
01780                                                                   ELGACLCC
01781  0631-CHK-CFT9-NOT-EXCLD.                                         ELGACLCC
01782      IF CFT9-PT-INCLUDE (CFT9-IDX)                                ELGACLCC
01783         EVALUATE TRUE                                             ELGACLCC
01784            WHEN CFT9-PT-PROF (CFT9-IDX)                           ELGACLCC
01785               SET SW-INTRNL-PROF-PROV-SPEC TO TRUE                ELGACLCC
01786               IF SRP-ACCUM-PROV-SPEC-PROF                         ELGACLCC
01787               THEN                                                ELGACLCC
01788                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGACLCC
01789               END-IF                                              ELGACLCC
01790            END-EVALUATE                                           ELGACLCC
01791      END-IF.                                                      ELGACLCC
01792                                                                   ELGACLCC
01793 /*****************************************************************ELGACLCC
01794 *                                                                *ELGACLCC
01795 *    CHECK IBGR INTERNAL TABULAR TO DETERMINE PROVIDER CLASS     *ELGACLCC
01796 *                                                                *ELGACLCC
01797 ******************************************************************ELGACLCC
01798                                                                   ELGACLCC
01799  0640-CHK-IBGR-PROV-CLASS.                                        ELGACLCC
01800      SET SW-INTRNL-NOT-INST-PROV-CLASS                            ELGACLCC
01801          SW-INTRNL-NOT-PROF-PROV-CLASS TO TRUE.                   ELGACLCC
01802      MOVE PC-IBGR TO KWA-PROVISION-ID.                            ELGACLCC
01803      MOVE WS-IBGR-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELGACLCC
01804      PERFORM 0650-READ-INTRLN-TAB.                                ELGACLCC
01805      SET ADDRESS OF GX1-RECORD-AREA TO IOP-REC-PTR.               ELGACLCC
01806      SET IOP-REC-PTR                TO NULLS.                     ELGACLCC
01807      SET GX1-INDEX                  TO GX1-ENTRY-COUNT.           ELGACLCC
01808      SET WS-MAX-GX1-INDEX           TO GX1-INDEX.                 ELGACLCC
01809                                                                   ELGACLCC
01810      IF GX1-ID-ARGUMENT-EXCLUDED                                  ELGACLCC
01811      THEN                                                         ELGACLCC
01812 *    -- ASSUME THAT IBGR WOULD NOT EXCLUDE ALL OF ANY PROVIDER    ELGACLCC
01813 *       CLASS (I.E., BOTH TYPES APPLY).                           ELGACLCC
01814         SET SW-OCCRNC-APPLIES                                     ELGACLCC
01815             SW-INTRNL-INST-PROV-CLASS                             ELGACLCC
01816             SW-INTRNL-PROF-PROV-CLASS                             ELGACLCC
01817          TO TRUE                                                  ELGACLCC
01818      ELSE                                                         ELGACLCC
01819         PERFORM 0690-CHK-INCLD-TYPE-IBGR                          ELGACLCC
01820      END-IF.                                                      ELGACLCC
01821                                                                   ELGACLCC
01822 /***********************************************************      ELGACLCC
01823 *                                                          *      ELGACLCC
01824 *    READ THE INTERNAL TABULAR RECORD                      *      ELGACLCC
01825 *                                                          *      ELGACLCC
01826 ************************************************************      ELGACLCC
01827                                                                   ELGACLCC
01828  0650-READ-INTRLN-TAB.                                            ELGACLCC
01829      PERFORM 9060-EST-ADR-TABULAR-FILE.                           ELGACLCC
01830      SET IOP-RD TO TRUE.                                          ELGACLCC
01831      SET IOP-FCQ-NONE TO TRUE.                                    ELGACLCC
01832      SET IOP-KVQ-EQ TO TRUE.                                      ELGACLCC
01833      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELGACLCC
01834      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGACLCC
01835      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGACLCC
01836      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACLCC
01837                                                                   ELGACLCC
01838      EVALUATE TRUE                                                ELGACLCC
01839         WHEN IOP-RC-OK                                            ELGACLCC
01840            CONTINUE                                               ELGACLCC
01841         WHEN IOP-RC-NOTFND                                        ELGACLCC
01842            SET CIA-AB-NOTFND-GCTABULR TO TRUE                     ELGACLCC
01843            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELGACLCC
01844         WHEN OTHER                                                ELGACLCC
01845             SET CIA-AB-CRITIO TO TRUE                             ELGACLCC
01846             EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC           ELGACLCC
01847         END-EVALUATE.                                             ELGACLCC
01848                                                                   ELGACLCC
01849 /*****************************************************************ELGACLCC
01850 *                                                                *ELGACLCC
01851 *    CHECK INCLUDE TYPE IBGR TO DETERMINE PROVIDER CLASS         *ELGACLCC
01852 *                                                                *ELGACLCC
01853 ******************************************************************ELGACLCC
01854                                                                   ELGACLCC
01855  0690-CHK-INCLD-TYPE-IBGR.                                        ELGACLCC
01856      PERFORM WITH TEST BEFORE                                     ELGACLCC
01857         VARYING GX1-INDEX FROM 1 BY 1                             ELGACLCC
01858           UNTIL    GX1-INDEX = WS-MAX-GX1-INDEX                   ELGACLCC
01859                 OR (    SW-INTRNL-INST-PROV-CLASS                 ELGACLCC
01860                     AND SW-INTRNL-PROF-PROV-CLASS )               ELGACLCC
01861         MOVE GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)                ELGACLCC
01862           TO WS-PROVISION-ARGUMENT                                ELGACLCC
01863         EVALUATE TRUE                                             ELGACLCC
01864            WHEN INST-CLASS                                        ELGACLCC
01865               SET SW-INTRNL-INST-PROV-CLASS TO TRUE               ELGACLCC
01866               IF SRP-ACCUM-PROV-CLASS-INST                        ELGACLCC
01867               THEN                                                ELGACLCC
01868                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGACLCC
01869               END-IF                                              ELGACLCC
01870            WHEN PROF-CLASS                                        ELGACLCC
01871               SET SW-INTRNL-PROF-PROV-CLASS TO TRUE               ELGACLCC
01872               IF SRP-ACCUM-PROV-CLASS-PROF                        ELGACLCC
01873               THEN                                                ELGACLCC
01874                  SET SW-OCCRNC-APPLIES TO TRUE                    ELGACLCC
01875               END-IF                                              ELGACLCC
01876            END-EVALUATE                                           ELGACLCC
01877      END-PERFORM.                                                 ELGACLCC
01878                                                                   ELGACLCC
01879 /***********************************************************      ELGACLCC
01880 *                                                          *      ELGACLCC
01881 *        ADD ACCUM OCCURRENCE TO FILE                      *      ELGACLCC
01882 *                                                          *      ELGACLCC
01883 ************************************************************      ELGACLCC
01884                                                                   ELGACLCC
01885  0710-WRITE-EXTRACT-RECORD.                                       ELGACLCC
01886      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGACLCC
01887      SET  IOP-ADD TO TRUE.                                        ELGACLCC
01888      SET  IOP-FCQ-NONE TO TRUE.                                   ELGACLCC
01889      SET  IOP-KVQ-NONE TO TRUE.                                   ELGACLCC
01890      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACLCC
01891                                                                   ELGACLCC
01892 /***********************************************************      ELGACLCC
01893 *                                                          *      ELGACLCC
01894 *    ESTABLISH ADDRESSABILITY OF THE TABULAR FILE          *      ELGACLCC
01895 *                                                          *      ELGACLCC
01896 ************************************************************      ELGACLCC
01897                                                                   ELGACLCC
01898  9060-EST-ADR-TABULAR-FILE.                                       ELGACLCC
01899      SET  CIA-GCTABULR-DDN TO TRUE.                               ELGACLCC
01900      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACLCC
01901         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGACLCC
01902         END-CALL.                                                 ELGACLCC
01903      IF CIA-RC-PTR-NULL                                           ELGACLCC
01904      THEN                                                         ELGACLCC
01905         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACLCC
01906         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGACLCC
01907      END-IF.                                                      ELGACLCC
01908                                                                   ELGACLCC
01909 /***********************************************************      ELGACLCC
01910 *                                                          *      ELGACLCC
01911 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELGACLCC
01912 *                                                          *      ELGACLCC
01913 ************************************************************      ELGACLCC
01914                                                                   ELGACLCC
01915  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELGACLCC
01916      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGACLCC
01917      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACLCC
01918         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGACLCC
01919         END-CALL.                                                 ELGACLCC
01920      IF CIA-RC-PTR-NULL                                           ELGACLCC
01921         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACLCC
01922         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGACLCC
01923      END-IF.                                                      ELGACLCC
