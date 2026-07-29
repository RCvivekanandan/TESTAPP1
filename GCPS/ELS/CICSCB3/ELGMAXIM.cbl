00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGMAXIM
00003  PROGRAM-ID.        ELGMAXIM.                                        LV002
00004                                                                   ELGMAXIM
00005  AUTHOR.            LUCY TORRES.                                  ELGMAXIM
00006                     RICHARD J. LUKETICH (RE-WRITE).               ELGMAXIM
00007                                                                   ELGMAXIM
00008  INSTALLATION.      HEALTH CARE SERVICE CORPORATION               ELGMAXIM
00009                     A MUTUAL LEGAL RESERVE COMPANY                ELGMAXIM
00010                     BLUE CROSS/BLUE SHIELD OF ILLINOIS            ELGMAXIM
00011                     233 N. MICHIGAN AVE                           ELGMAXIM
00012                     CHICAGO, ILLINOIS 60601                       ELGMAXIM
00013                                                                   ELGMAXIM
00014  DATE-WRITTEN.      03-JUN-1987.                                  ELGMAXIM
00015                     17-FEB-1992 (RE-WRITE).                       ELGMAXIM
00016                                                                   ELGMAXIM
00017  DATE-COMPILED.                                                   ELGMAXIM
00018                                                                   ELGMAXIM
00019  SECURITY.          COPYRIGHT 1986, 1992,                         ELGMAXIM
00020                     HEALTH CARE SERVICE CORPORATION               ELGMAXIM
00021                                                                   ELGMAXIM
00022  ENVIRONMENT DIVISION.                                            ELGMAXIM
00023                                                                   ELGMAXIM
00024  CONFIGURATION SECTION.                                           ELGMAXIM
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELGMAXIM
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELGMAXIM
00027                                                                   ELGMAXIM
00028 /*****************************************************************ELGMAXIM
00029 *                                                                *ELGMAXIM
00030 *  ELGMAXIM:        SELECTS #ABM (MAXIMUM) ACCUMULATORS AND      *ELGMAXIM
00031 *                   SETUPS THE INFORMATION TO BE PROCESSED BY    *ELGMAXIM
00032 *                   THE MAXIMUM GENERATOR MODULE.  THE ACCUMS    *ELGMAXIM
00033 *                   ARE SELECTED FROM THE BENEFIT PROVISION      *ELGMAXIM
00034 *                   LEVEL.                                       *ELGMAXIM
00035 *                                                                *ELGMAXIM
00036 ******************************************************************ELGMAXIM
00037 *                                                                *ELGMAXIM
00038 *                      MAINTENANCE HISTORY                       *ELGMAXIM
00039 *                                                                *ELGMAXIM
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELGMAXIM
00041 * ----- ----------- --- ----- ---------------------------------- *ELGMAXIM
00042 * 01.00 03-JUN-1987 LET       CREATED                            *ELGMAXIM
00043 * 01.01 25-SEP-1987 LET       ADDED DEFINITION DATA FIELD        *ELGMAXIM
00044 *                                                                *ELGMAXIM
00045 * 01.02 17-NOV-1987 REB       MADE CHANGES TO CORRESPOND TO NEW  *ELGMAXIM
00046 *                             VERSION OF COPYBOOK ELSACUMC.      *ELGMAXIM
00047 *                                                                *ELGMAXIM
00048 * 01.07    SEP-1991 RKH    1. ADDED LOGIC FOR:                   *ELGMAXIM
00049 *    ISSR #12010                A.  NEW PATIENT AGE FIELDS       *ELGMAXIM
00050 *                               B.  RELATIONSHIP IND VALUE       *ELGMAXIM
00051 *                          2. REVISE LOGIC TO LOAD INT ACCUMS    *ELGMAXIM
00052 *                             INTO VARIABLE LEVEL TABLE          *ELGMAXIM
00053 *                          3. ADDED COPYBOOKS :                  *ELGMAXIM
00054 *                               A. GCTIBGR   - IBGR TAB          *ELGMAXIM
00055 *                               B. GCTIPGT   - IPGT TAB          *ELGMAXIM
00056 *                               C. ELSCFTB2  - PROVIDER TYPE     *ELGMAXIM
00057 *                                         COMPARE TABLE          *ELGMAXIM
00058 *                          4. ADD LOGIC TO INSPECT #IPGT AND     *ELGMAXIM
00059 *                             #IBGR INT TABS TO DETERMINE IF     *ELGMAXIM
00060 *                             AN OCCURRANCE IS THE SELECTED      *ELGMAXIM
00061 *                             PROVIDER CLASS.                    *ELGMAXIM
00062 *                                                                *ELGMAXIM
00063 * 02.00 03-JAN-1992 RJL       LOGIC RESTRUCTURED, ADDED          *ELGMAXIM
00064 *                             MAXIMUM BASE AMOUNT SOURCE IND.    *ELGMAXIM
00065 *                                                                *ELGMAXIM
00066 * 02.00 17-FEB-1992 RJL    CLONED FROM ELTABM.                   *ELGMAXIM
00067 *                                                                *ELGMAXIM
00068 * 02.01 25-AUG-2000 AKK    ADDED SUPPORT FOR #IPGS.              *ELGMAXIM
00069 *                                                                *ELGMAXIM
00070 *            08/12/03  AKK  REGEN'D TO CHECK ORDER OF THE COMPILE*ELGMAXIM
00069 *                                                                *ELGMAXIM
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00056 *                                                                *ELGABMCC
00071 ******************************************************************ELGMAXIM
00072      TITLE  'ELGMAXIM          WORKING STORAGE'.                  ELGMAXIM
00073  DATA DIVISION.                                                   ELGMAXIM
00074                                                                   ELGMAXIM
00075  WORKING-STORAGE SECTION.                                         ELGMAXIM
00076                                                                   ELGMAXIM
00077  01  SWITCHES.                                                    ELGMAXIM
00078      02                                      PICTURE  X(01).      ELGMAXIM
00079         88 SW-APPLIC-ACCUM-FOUND             VALUE 'Y'.           ELGMAXIM
00080         88 SW-NO-APPLIC-ACCUM-FOUND          VALUE 'N'.           ELGMAXIM
00081      02                                      PICTURE  X(01).      ELGMAXIM
00082         88 SW-OCCRNC-APPLIES                 VALUE 'Y'.           ELGMAXIM
00083         88 SW-OCCRNC-DOES-NOT-APPLY          VALUE 'N'.           ELGMAXIM
00084      02                                      PICTURE  X(01).      ELGMAXIM
00085         88 SW-HAS-IBGR                       VALUE 'Y'.           ELGMAXIM
00086         88 SW-HAS-NO-IBGR                    VALUE 'N'.           ELGMAXIM
00087      02                                      PICTURE  X(01).      ELGMAXIM
00088         88 SW-HAS-IDGD                       VALUE 'Y'.           ELGMAXIM
00089         88 SW-HAS-NO-IDGD                    VALUE 'N'.           ELGMAXIM
00090      02                                      PICTURE  X(01).      ELGMAXIM
00091         88 SW-HAS-IPGN                       VALUE 'Y'.           ELGMAXIM
00092         88 SW-HAS-NO-IPGN                    VALUE 'N'.           ELGMAXIM
00093      02                                      PICTURE  X(01).      ELGMAXIM
00094         88 SW-HAS-IPGP                       VALUE 'Y'.           ELGMAXIM
00095         88 SW-HAS-NO-IPGP                    VALUE 'N'.           ELGMAXIM
00096      02                                      PICTURE  X(01).      ELGMAXIM
00097         88 SW-HAS-IPGT                       VALUE 'Y'.           ELGMAXIM
00098         88 SW-HAS-NO-IPGT                    VALUE 'N'.           ELGMAXIM
00099      02                                      PICTURE  X(01).      ELGMAXIM
00100         88 SW-HAS-IPGS                       VALUE 'Y'.           ELGMAXIM
00101         88 SW-HAS-NO-IPGS                    VALUE 'N'.           ELGMAXIM
00102      02                                      PICTURE  X(01).      ELGMAXIM
00103         88 SW-DUP-SLOT-NBR                   VALUE 'D'.           ELGMAXIM
00104         88 SW-UNQ-SLOT-NBR                   VALUE 'U'.           ELGMAXIM
00105      02                                      PICTURE  X(01).      ELGMAXIM
00106         88 SW-INTRNL-INST-PROV-CLASS         VALUE 'Y'.           ELGMAXIM
00107         88 SW-INTRNL-NOT-INST-PROV-CLASS     VALUE 'N'.           ELGMAXIM
00108         88 SW-INTRNL-INST-PROV-CLS-NOT-DT    VALUE 'X'.           ELGMAXIM
00109      02                                      PICTURE  X(01).      ELGMAXIM
00110         88 SW-INTRNL-PROF-PROV-CLASS         VALUE 'Y'.           ELGMAXIM
00111         88 SW-INTRNL-NOT-PROF-PROV-CLASS     VALUE 'N'.           ELGMAXIM
00112         88 SW-INTRNL-PROF-PROV-CLS-NOT-DT    VALUE 'X'.           ELGMAXIM
00113      02                                      PICTURE  X(01).      ELGMAXIM
00114         88 SW-INTRNL-PROF-PROV-SPEC          VALUE 'Y'.           ELGMAXIM
00115         88 SW-INTRNL-NOT-PROF-PROV-SPEC      VALUE 'N'.           ELGMAXIM
00116         88 SW-INTRNL-PROF-PROV-SPC-NOT-DT    VALUE 'X'.           ELGMAXIM
00117      02                                      PICTURE  X(01).      ELGMAXIM
00118         88 SW-ENTRY-FOUND                    VALUE 'Y'.           ELGMAXIM
00119         88 SW-ENTRY-NOT-FOUND                VALUE 'N'.           ELGMAXIM
00120                                                                   ELGMAXIM
00121  01  WS-PROVISION-ARGUMENT.                                       ELGMAXIM
00122      02                                      PICTURE X(05).       ELGMAXIM
00123      02 WS-PROVISION-CL                      PICTURE X(01).       ELGMAXIM
00124         88  INST-CLASS                       VALUE 'A', 'B', 'W'. ELGMAXIM
00125         88  PROF-CLASS                       VALUE 'C', 'D', 'E'. ELGMAXIM
00126                                                                   ELGMAXIM
00127  01  WS-LOB-ACCUM-OCCRNC         PICTURE  X(01).                  ELGMAXIM
00128      88 WS-LOB-INST              VALUE '1'.                       ELGMAXIM
00129      88 WS-LOB-PROF              VALUE '2'.                       ELGMAXIM
00130      88 WS-LOB-SUPP              VALUE '3', '6', '7', '8'.        ELGMAXIM
00131      88 WS-LOB-BOTH              VALUE '3', '4', '5', '6', '7'.   ELGMAXIM
00132                                                                   ELGMAXIM
00133  01  PROGRAM-CONSTANTS.                                           ELGMAXIM
00134      02 PC-ABM                   PICTURE  X(06) VALUE '#ABM  '.   ELGMAXIM
00135      02 PC-IBGR                  PICTURE  X(06) VALUE '#IBGR '.   ELGMAXIM
00136      02 PC-IDGD                  PICTURE  X(06) VALUE '#IDGD '.   ELGMAXIM
00137      02 PC-IPGN                  PICTURE  X(06) VALUE '#IPGN '.   ELGMAXIM
00138      02 PC-IPGP                  PICTURE  X(06) VALUE '#IPGP '.   ELGMAXIM
00139      02 PC-IPGT                  PICTURE  X(06) VALUE '#IPGT '.   ELGMAXIM
00140      02 PC-IPGS                  PICTURE  X(06) VALUE '#IPGS '.   ELGMAXIM
00141      02 PC-MAXIMUM-NBR-OCCURS    PICTURE  9(02) VALUE 44.         ELGMAXIM
00142                                                                   ELGMAXIM
00143  01  WS-WORK-FIELDS.                                              ELGMAXIM
00144      02 WS-ABM-SUB               PICTURE S9(04) COMP.             ELGMAXIM
00145      02 WS-SLOT-NBR              PICTURE S9(07) COMP-3.           ELGMAXIM
00146      02 WS-MAX-GAA-IDX           INDEX.                           ELGMAXIM
00147                                                                   ELGMAXIM
00148  01  WS-INTRNL-TAB-SLOT-HOLD.                                     ELGMAXIM
00149      02 WS-IBGR-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGMAXIM
00150      02 WS-IDGD-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGMAXIM
00151      02 WS-IPGN-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGMAXIM
00152      02 WS-IPGP-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGMAXIM
00153      02 WS-IPGT-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGMAXIM
00154      02 WS-IPGS-SLOT-NBR         PICTURE S9(7) COMP-3.            ELGMAXIM
00155 / -- PROVIDER TYPE CONFIDENCE FACTORS TABLE                       ELGMAXIM
00156      COPY ELSCFTB2.                                               ELGMAXIM
00157                                                                   ELGMAXIM
00158 / -- PROVIDER SPEC CONFIDENCE FACTORS TABLE                       ELGMAXIM
00159      COPY ELSCFTB9.                                               ELGMAXIM
00160                                                                   ELGMAXIM
00161      TITLE  'ELGMAXIM          LINKAGE SECTION'                   ELGMAXIM
00162  LINKAGE SECTION.                                                 ELGMAXIM
00163  01  DFHCOMMAREA.                                                 ELGMAXIM
00164      COPY ELSCOMMC.                                               ELGMAXIM
00165 /                                                                 ELGMAXIM
00166      COPY ELSCIA2C.                                               ELGMAXIM
00167 /                                                                 ELGMAXIM
00168      COPY ELSIOPMC.                                               ELGMAXIM
00169 /                                                                 ELGMAXIM
00170      COPY ELSKEYSC.                                               ELGMAXIM
00171 /                                                                 ELGMAXIM
00172      COPY ELSSRTPC.                                               ELGMAXIM
00173 /                                                                 ELGMAXIM
00174      COPY ELSSSCBC.                                               ELGMAXIM
00175 /                                                                 ELGMAXIM
00176  01  GAA-RECORD-AREA.                                             ELGMAXIM
00177      COPY GCTABMC.                                                ELGMAXIM
00178 /                                                                 ELGMAXIM
00179      COPY ELSACUMC.                                               ELGMAXIM
00180      TITLE  'ELGMAXIM          PROCEDURE DIVISION'.               ELGMAXIM
00181 ************************************************************      ELGMAXIM
00182 *                                                          *      ELGMAXIM
00183 *    ELGMAXIM MAINLINE                                     *      ELGMAXIM
00184 *                                                          *      ELGMAXIM
00185 ************************************************************      ELGMAXIM
00186                                                                   ELGMAXIM
00187  PROCEDURE DIVISION.                                              ELGMAXIM
00188                                                                   ELGMAXIM
00189      PERFORM 0010-INITIALIZATION.                                 ELGMAXIM
00190      PERFORM 0100-PROCESS.                                        ELGMAXIM
00191      GOBACK.                                                      ELGMAXIM
00192                                                                   ELGMAXIM
00193 ************************************************************      ELGMAXIM
00194 *                                                          *      ELGMAXIM
00195 *    INITIALIZATION                                        *      ELGMAXIM
00196 *                                                          *      ELGMAXIM
00197 ************************************************************      ELGMAXIM
00198                                                                   ELGMAXIM
00199  0010-INITIALIZATION.                                             ELGMAXIM
00200      PERFORM 0020-EST-ADR-OF-CNTRL-BLKS.                          ELGMAXIM
00201      PERFORM 0060-EST-ADR-KEY-WK-AREA.                            ELGMAXIM
00202      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELGMAXIM
00203      PERFORM 0120-EST-ADR-BEN-PRVN-ACCUM.                         ELGMAXIM
00204      PERFORM 0190-INIT-DATA.                                      ELGMAXIM
00205                                                                   ELGMAXIM
00206 ************************************************************      ELGMAXIM
00207 *                                                          *      ELGMAXIM
00208 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGMAXIM
00209 *                                                          *      ELGMAXIM
00210 ************************************************************      ELGMAXIM
00211                                                                   ELGMAXIM
00212  0020-EST-ADR-OF-CNTRL-BLKS.                                      ELGMAXIM
00213      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGMAXIM
00214      THEN                                                         ELGMAXIM
00215         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGMAXIM
00216      ELSE                                                         ELGMAXIM
00217         IF ECA-CIA-PTR = NULL                                     ELGMAXIM
00218         THEN                                                      ELGMAXIM
00219            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGMAXIM
00220         ELSE                                                      ELGMAXIM
00221            CALL 'ELUINISM' USING DFHCOMMAREA                      ELGMAXIM
00222               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELGMAXIM
00223               END-CALL                                            ELGMAXIM
00224            SET CIA-ELSSSCB-DDN TO TRUE                            ELGMAXIM
00225            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELGMAXIM
00226               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELGMAXIM
00227               END-CALL                                            ELGMAXIM
00228            IF CIA-RC-PTR-NULL                                     ELGMAXIM
00229            THEN                                                   ELGMAXIM
00230               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELGMAXIM
00231               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGMAXIM
00232            ELSE                                                   ELGMAXIM
00233               CONTINUE                                            ELGMAXIM
00234            END-IF                                                 ELGMAXIM
00235         END-IF                                                    ELGMAXIM
00236      END-IF.                                                      ELGMAXIM
00237                                                                   ELGMAXIM
00238 /***********************************************************      ELGMAXIM
00239 *                                                          *      ELGMAXIM
00240 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELGMAXIM
00241 *                                                          *      ELGMAXIM
00242 ************************************************************      ELGMAXIM
00243                                                                   ELGMAXIM
00244  0060-EST-ADR-KEY-WK-AREA.                                        ELGMAXIM
00245      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGMAXIM
00246      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGMAXIM
00247         ADDRESS OF KWA-FILE-KEY-WORK-AREA                         ELGMAXIM
00248         END-CALL.                                                 ELGMAXIM
00249      IF CIA-RC-PTR-NULL                                           ELGMAXIM
00250         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGMAXIM
00251         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGMAXIM
00252      END-IF.                                                      ELGMAXIM
00253                                                                   ELGMAXIM
00254 ************************************************************      ELGMAXIM
00255 *                                                          *      ELGMAXIM
00256 *    ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS     *      ELGMAXIM
00257 *                                                          *      ELGMAXIM
00258 ************************************************************      ELGMAXIM
00259                                                                   ELGMAXIM
00260  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGMAXIM
00261      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGMAXIM
00262      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGMAXIM
00263         ADDRESS OF SRP-SUBROUTINE-PARAMETERS                      ELGMAXIM
00264         END-CALL.                                                 ELGMAXIM
00265      IF CIA-RC-PTR-NULL                                           ELGMAXIM
00266         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGMAXIM
00267         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGMAXIM
00268      END-IF.                                                      ELGMAXIM
00269                                                                   ELGMAXIM
00270 ************************************************************      ELGMAXIM
00271 *                                                          *      ELGMAXIM
00272 *    ESTABLISH ADDRESSABILITY OF BENEFIT PROVISION LEVEL   *      ELGMAXIM
00273 *    ACCUMULATOR RECORD                                    *      ELGMAXIM
00274 *                                                          *      ELGMAXIM
00275 ************************************************************      ELGMAXIM
00276                                                                   ELGMAXIM
00277  0120-EST-ADR-BEN-PRVN-ACCUM.                                     ELGMAXIM
00278      SET CIA-GCTABULR-DDN TO TRUE.                                ELGMAXIM
00279      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGMAXIM
00280         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGMAXIM
00281         END-CALL.                                                 ELGMAXIM
00282      IF CIA-RC-PTR-NULL                                           ELGMAXIM
00283      THEN                                                         ELGMAXIM
00284         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGMAXIM
00285         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGMAXIM
00286      ELSE                                                         ELGMAXIM
00287         IF IOP-REC-PTR = NULLS                                    ELGMAXIM
00288         THEN                                                      ELGMAXIM
00289            SET CIA-AB-UNALLOC-AREA TO TRUE                        ELGMAXIM
00290            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELGMAXIM
00291         ELSE                                                      ELGMAXIM
00292            SET ADDRESS OF GAA-RECORD-AREA TO IOP-REC-PTR          ELGMAXIM
00293         END-IF                                                    ELGMAXIM
00294      END-IF.                                                      ELGMAXIM
00295                                                                   ELGMAXIM
00296 /***********************************************************      ELGMAXIM
00297 *                                                          *      ELGMAXIM
00298 *    INITIALIZE DATA AREAS                                 *      ELGMAXIM
00299 *                                                          *      ELGMAXIM
00300 ************************************************************      ELGMAXIM
00301                                                                   ELGMAXIM
00302  0190-INIT-DATA.                                                  ELGMAXIM
00303      SET SW-NO-APPLIC-ACCUM-FOUND TO TRUE.                        ELGMAXIM
00304                                                                   ELGMAXIM
00305 /***********************************************************      ELGMAXIM
00306 *                                                          *      ELGMAXIM
00307 *        PROCESS                                           *      ELGMAXIM
00308 *                                                          *      ELGMAXIM
00309 ************************************************************      ELGMAXIM
00310                                                                   ELGMAXIM
00311  0100-PROCESS.                                                    ELGMAXIM
00312      PERFORM 0240-SCAN-FOR-APPLIC-OCCRNCS.                        ELGMAXIM
00313                                                                   ELGMAXIM
00314      IF SW-NO-APPLIC-ACCUM-FOUND                                  ELGMAXIM
00315      THEN                                                         ELGMAXIM
00316         EVALUATE TRUE                                             ELGMAXIM
00317            WHEN SSB-PROV-CLASS-INST                               ELGMAXIM
00318               SET SRP-INST-NOT-APPLICABLE TO TRUE                 ELGMAXIM
00319            WHEN SSB-PROV-CLASS-PROF                               ELGMAXIM
00320               SET SRP-PROF-NOT-APPLICABLE TO TRUE                 ELGMAXIM
00321            WHEN SSB-PROV-CLASS-BOTH                               ELGMAXIM
00322               SET SRP-NO-ACCUMS-FOUND TO TRUE                     ELGMAXIM
00323            WHEN OTHER                                             ELGMAXIM
00324               SET CIA-AB-PGM-LOGIC TO TRUE                        ELGMAXIM
00325               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGMAXIM
00326            END-EVALUATE                                           ELGMAXIM
00327      END-IF.                                                      ELGMAXIM
00328                                                                   ELGMAXIM
00329      SET SRP-TOPIC-ACCUM TO TRUE.                                 ELGMAXIM
00330                                                                   ELGMAXIM
00331 * -- LINK TO THE OUTPUT GENERATOR                                 ELGMAXIM
00332      EXEC CICS LINK PROGRAM ('ELGABM') COMMAREA (DFHCOMMAREA)     ELGMAXIM
00333         END-EXEC.                                                 ELGMAXIM
00334                                                                   ELGMAXIM
00335 /***********************************************************      ELGMAXIM
00336 *                                                          *      ELGMAXIM
00337 *    SCAN ABM ACCUMULATORS FOR APPLICABLE OCCURRENCES      *      ELGMAXIM
00338 *                                                          *      ELGMAXIM
00339 ************************************************************      ELGMAXIM
00340                                                                   ELGMAXIM
00341  0240-SCAN-FOR-APPLIC-OCCRNCS.                                    ELGMAXIM
00342      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGMAXIM
00343      PERFORM 0250-DELETE-ABM-SUMMARY-FILE.                        ELGMAXIM
00344      PERFORM 0260-ALLOC-WORKFILE-REC-AREA.                        ELGMAXIM
00345                                                                   ELGMAXIM
00346 * -- SET UPPER LIMIT ON TABULAR SCAN                              ELGMAXIM
00347      SET GAA-INDEX TO GAA-ENTRY-COUNT.                            ELGMAXIM
00348      SET WS-MAX-GAA-IDX TO GAA-INDEX.                             ELGMAXIM
00349 * -- SCAN ACCUMULATOR TABULAR                                     ELGMAXIM
00350      PERFORM 0300-TEST-ABM-OCCURENCE                              ELGMAXIM
00351         VARYING GAA-INDEX FROM 1 BY 1                             ELGMAXIM
00352           UNTIL GAA-INDEX = WS-MAX-GAA-IDX.                       ELGMAXIM
00353                                                                   ELGMAXIM
00354 /***********************************************************      ELGMAXIM
00355 *                                                          *      ELGMAXIM
00356 *        DELETE ABM SUMMARY FILE                           *      ELGMAXIM
00357 *                                                          *      ELGMAXIM
00358 ************************************************************      ELGMAXIM
00359                                                                   ELGMAXIM
00360  0250-DELETE-ABM-SUMMARY-FILE.                                    ELGMAXIM
00361      SET IOP-DEL TO TRUE.                                         ELGMAXIM
00362      SET IOP-FCQ-NONE TO TRUE.                                    ELGMAXIM
00363      SET IOP-KVQ-NONE TO TRUE.                                    ELGMAXIM
00364      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGMAXIM
00365                                                                   ELGMAXIM
00366 /***********************************************************      ELGMAXIM
00367 *                                                          *      ELGMAXIM
00368 *    ALLOCATE WORKFILE RECORD AREA                         *      ELGMAXIM
00369 *                                                          *      ELGMAXIM
00370 ************************************************************      ELGMAXIM
00371                                                                   ELGMAXIM
00372  0260-ALLOC-WORKFILE-REC-AREA.                                    ELGMAXIM
00373      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGMAXIM
00374      SET CIA-STG-GETMAIN TO TRUE.                                 ELGMAXIM
00375      SET IOP-GETMAIN-REC TO TRUE.                                 ELGMAXIM
00376      COMPUTE IOP-MAX-REC-LEN =                                    ELGMAXIM
00377              LENGTH OF ACCUM-FIXED-AREA                           ELGMAXIM
00378 *          + LENGTH OF ACCUM-ASCEND-DESCEND-COUNT                 ELGMAXIM
00379            + LENGTH OF ACCUM-VARIABLE-AREA                        ELGMAXIM
00380            + LENGTH OF ACCUM-COPAY-VARIABLE-AREA                  ELGMAXIM
00381 *          + (PC-MAXIMUM-NBR-OCCURS *                             ELGMAXIM
00382 *             LENGTH OF  ACCUM-ASCEND-DESCEND-ENTRY).             ELGMAXIM
00383                                                                   ELGMAXIM
00384      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGMAXIM
00385      IF IOP-REC-PTR = NULLS                                       ELGMAXIM
00386      THEN                                                         ELGMAXIM
00387         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGMAXIM
00388         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGMAXIM
00389      ELSE                                                         ELGMAXIM
00390         SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR     ELGMAXIM
00391      END-IF.                                                      ELGMAXIM
00392                                                                   ELGMAXIM
00393 /***********************************************************      ELGMAXIM
00394 *                                                          *      ELGMAXIM
00395 *        TEST ABM OCCURS                                   *      ELGMAXIM
00396 *                                                          *      ELGMAXIM
00397 ************************************************************      ELGMAXIM
00398                                                                   ELGMAXIM
00399  0300-TEST-ABM-OCCURENCE.                                         ELGMAXIM
00400                                                                   ELGMAXIM
00401 * -- INITIALIZE                                                   ELGMAXIM
00402      MOVE GAA-BAMA-L-O-B (GAA-INDEX) TO WS-LOB-ACCUM-OCCRNC.      ELGMAXIM
00403      SET SW-OCCRNC-DOES-NOT-APPLY TO TRUE.                        ELGMAXIM
00404      SET SW-HAS-NO-IBGR                                           ELGMAXIM
00405          SW-HAS-NO-IDGD                                           ELGMAXIM
00406          SW-HAS-NO-IPGN                                           ELGMAXIM
00407          SW-HAS-NO-IPGP                                           ELGMAXIM
00408          SW-HAS-NO-IPGT                                           ELGMAXIM
00409          SW-HAS-NO-IPGS                                           ELGMAXIM
00410       TO TRUE.                                                    ELGMAXIM
00411      INITIALIZE WS-IBGR-SLOT-NBR                                  ELGMAXIM
00412                 WS-IDGD-SLOT-NBR                                  ELGMAXIM
00413                 WS-IPGN-SLOT-NBR                                  ELGMAXIM
00414                 WS-IPGP-SLOT-NBR                                  ELGMAXIM
00415                 WS-IPGT-SLOT-NBR                                  ELGMAXIM
00416                 WS-IPGS-SLOT-NBR.                                 ELGMAXIM
00417      SET SW-INTRNL-INST-PROV-CLS-NOT-DT                           ELGMAXIM
00418          SW-INTRNL-PROF-PROV-CLS-NOT-DT                           ELGMAXIM
00419          SW-INTRNL-PROF-PROV-SPC-NOT-DT                           ELGMAXIM
00420       TO TRUE.                                                    ELGMAXIM
00421                                                                   ELGMAXIM
00422 * -- SCAN FOR INTERNAL TABULARS                                   ELGMAXIM
00423 *    (THIS IS DONE NOW IN CASE IPGT IS NEEDED TO DETERMINE        ELGMAXIM
00424 *     WHETHER OCCURRENCE IS INSTITUTIONAL OR PROFESSIONAL.        ELGMAXIM
00425 *     THE IBGR IS IGNORED, SINCE THE ACCUMULATOR IS DIRECTLY      ELGMAXIM
00426 *     ATTACHED TO THE PROVISION.)                                 ELGMAXIM
00427      PERFORM 0310-SCAN-THE-INTERNAL-TABULAR                       ELGMAXIM
00428         VARYING GAA-INT-INDEX FROM 1 BY 1                         ELGMAXIM
00429           UNTIL    GAA-INT-INDEX                                  ELGMAXIM
00430                 >= GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX).        ELGMAXIM
00431                                                                   ELGMAXIM
00432      EVALUATE TRUE ALSO TRUE                                      ELGMAXIM
00433         WHEN SSB-PROV-CLASS-BOTH ALSO TRUE                        ELGMAXIM
00434            SET SRP-ACCUM-PROV-CLASS-BOTH TO TRUE                  ELGMAXIM
00435            SET SW-OCCRNC-APPLIES TO TRUE                          ELGMAXIM
00436         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-BOTH                 ELGMAXIM
00437            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGMAXIM
00438            SET SW-OCCRNC-APPLIES TO TRUE                          ELGMAXIM
00439         WHEN SSB-PROV-CLASS-INST ALSO WS-LOB-INST                 ELGMAXIM
00440            SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                  ELGMAXIM
00441            SET SW-OCCRNC-APPLIES TO TRUE                          ELGMAXIM
00442         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-BOTH                 ELGMAXIM
00443            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGMAXIM
00444            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGMAXIM
00445            SET SW-OCCRNC-APPLIES TO TRUE                          ELGMAXIM
00446         WHEN SSB-PROV-CLASS-PROF ALSO WS-LOB-PROF                 ELGMAXIM
00447            SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE                  ELGMAXIM
00448            SET SRP-ACCUM-PROV-SPEC-PROF TO TRUE                   ELGMAXIM
00449            SET SW-OCCRNC-APPLIES TO TRUE                          ELGMAXIM
00450         WHEN OTHER                                                ELGMAXIM
00451            CONTINUE                                               ELGMAXIM
00452         END-EVALUATE.                                             ELGMAXIM
00453                                                                   ELGMAXIM
00454      IF SW-OCCRNC-APPLIES                                         ELGMAXIM
00455      THEN                                                         ELGMAXIM
00456 *    -- SUMMARIZE AND WRITE ACCUMULATOR EXTRACT RECORD            ELGMAXIM
00457         SET SW-APPLIC-ACCUM-FOUND TO TRUE                         ELGMAXIM
00458         PERFORM 0320-INIT-ACCUM-EXTRACT                           ELGMAXIM
00459         PERFORM 0330-EXTRACT-ACCUM                                ELGMAXIM
00460         PERFORM 0410-CHK-EXTRACT-DATA-INTGRTY                     ELGMAXIM
00461         PERFORM 0710-WRITE-EXTRACT-RECORD                         ELGMAXIM
00462      END-IF.                                                      ELGMAXIM
00463                                                                   ELGMAXIM
00464 /***********************************************************      ELGMAXIM
00465 *                                                          *      ELGMAXIM
00466 *        SCAN THE INTERNAL TABULARS                        *      ELGMAXIM
00467 *                                                          *      ELGMAXIM
00468 ************************************************************      ELGMAXIM
00469                                                                   ELGMAXIM
00470  0310-SCAN-THE-INTERNAL-TABULAR.                                  ELGMAXIM
00471      IF GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX) > 0               ELGMAXIM
00472      THEN                                                         ELGMAXIM
00473         MOVE GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX)              ELGMAXIM
00474           TO WS-SLOT-NBR                                          ELGMAXIM
00475         EVALUATE GAA-INT-ID (GAA-INDEX, GAA-INT-INDEX)            ELGMAXIM
00476            WHEN PC-IBGR                                           ELGMAXIM
00477               MOVE WS-SLOT-NBR TO WS-IBGR-SLOT-NBR                ELGMAXIM
00478               SET SW-HAS-IBGR                                     ELGMAXIM
00479                TO TRUE                                            ELGMAXIM
00480            WHEN PC-IDGD                                           ELGMAXIM
00481               MOVE WS-SLOT-NBR TO WS-IDGD-SLOT-NBR                ELGMAXIM
00482               SET SW-HAS-IDGD                                     ELGMAXIM
00483                TO TRUE                                            ELGMAXIM
00484            WHEN PC-IPGP                                           ELGMAXIM
00485               MOVE WS-SLOT-NBR TO WS-IPGP-SLOT-NBR                ELGMAXIM
00486               SET SW-HAS-IPGP                                     ELGMAXIM
00487                TO TRUE                                            ELGMAXIM
00488            WHEN PC-IPGN                                           ELGMAXIM
00489               MOVE WS-SLOT-NBR TO WS-IPGN-SLOT-NBR                ELGMAXIM
00490               SET SW-HAS-IPGN                                     ELGMAXIM
00491                TO TRUE                                            ELGMAXIM
00492            WHEN PC-IPGT                                           ELGMAXIM
00493               MOVE WS-SLOT-NBR TO WS-IPGT-SLOT-NBR                ELGMAXIM
00494               SET SW-HAS-IPGT                                     ELGMAXIM
00495                TO TRUE                                            ELGMAXIM
00496            WHEN PC-IPGS                                           ELGMAXIM
00497               MOVE WS-SLOT-NBR TO WS-IPGS-SLOT-NBR                ELGMAXIM
00498               SET SW-HAS-IPGS                                     ELGMAXIM
00499                TO TRUE                                            ELGMAXIM
00500            WHEN OTHER                                             ELGMAXIM
00501               CONTINUE                                            ELGMAXIM
00502            END-EVALUATE                                           ELGMAXIM
00503      END-IF.                                                      ELGMAXIM
00504                                                                   ELGMAXIM
00505 /***********************************************************      ELGMAXIM
00506 *                                                          *      ELGMAXIM
00507 *    INITIALIZE ACCUMULATOR EXTRACT RECORD                 *      ELGMAXIM
00508 *                                                          *      ELGMAXIM
00509 ************************************************************      ELGMAXIM
00510                                                                   ELGMAXIM
00511  0320-INIT-ACCUM-EXTRACT.                                         ELGMAXIM
00512      INITIALIZE ACCUM-FIXED-AREA.                                 ELGMAXIM
00513      SET ACCUM-ABM TO TRUE.                                       ELGMAXIM
00514      MOVE 1 TO  ACCUM-ASCEND-DESCEND-COUNT.                       ELGMAXIM
00515      SET  ASC-DES-INDEX TO ACCUM-ASCEND-DESCEND-COUNT.            ELGMAXIM
00516      INITIALIZE ACCUM-ASCEND-DESCEND-ENTRY (1).                   ELGMAXIM
00517      INITIALIZE ACCUM-COPAY-ENTRY (1).                            ELGMAXIM
00518                                                                   ELGMAXIM
00519 /***********************************************************      ELGMAXIM
00520 *                                                          *      ELGMAXIM
00521 *        SUMMARIZE ABM TOPIC LEVEL DATA ELEMENTS           *      ELGMAXIM
00522 *                                                          *      ELGMAXIM
00523 ************************************************************      ELGMAXIM
00524                                                                   ELGMAXIM
00525  0330-EXTRACT-ACCUM.                                              ELGMAXIM
00526                                                                   ELGMAXIM
00527 * -- SET FIXED PORTION DATA ELEMENTS                              ELGMAXIM
00528      MOVE GAA-BAMA-FYI-VALUE (GAA-INDEX) TO ACCUM-FYI-VALUE.      ELGMAXIM
00529      MOVE GAA-BAMA-COST-CONTAIN-IND (GAA-INDEX)                   ELGMAXIM
00530        TO ACCUM-COST-CONTAIN-IND.                                 ELGMAXIM
00531      MOVE GAA-BAMA-PLACE-OF-TREATMENT (GAA-INDEX)                 ELGMAXIM
00532        TO ACCUM-PLACE-OF-TREATMENT.                               ELGMAXIM
00533      MOVE GAA-BAMA-BENEFIT-PERIOD (GAA-INDEX)                     ELGMAXIM
00534        TO ACCUM-BENEFIT-PERIOD.                                   ELGMAXIM
00535      MOVE GAA-BAMA-BEN-PER-TIME-FCTR (GAA-INDEX)                  ELGMAXIM
00536        TO ACCUM-BEN-PER-TIME-FCTR.                                ELGMAXIM
00537      MOVE GAA-BAMA-BEN-PER-TIME-QUAL (GAA-INDEX)                  ELGMAXIM
00538        TO ACCUM-BEN-PER-TIME-QUAL.                                ELGMAXIM
00539      MOVE GAA-BAMA-INTERVAL-TIME-FCTR (GAA-INDEX)                 ELGMAXIM
00540        TO ACCUM-INTERVAL-TIME-FCTR.                               ELGMAXIM
00541      MOVE GAA-BAMA-INTERVAL-TYPE (GAA-INDEX)                      ELGMAXIM
00542        TO ACCUM-INTERVAL-TYPE.                                    ELGMAXIM
00543      MOVE GAA-BAMA-INTERVAL-OVRD-IND (GAA-INDEX)                  ELGMAXIM
00544        TO ACCUM-INTERVAL-OVRD-IND.                                ELGMAXIM
00545      MOVE GAA-BAMA-INTERVAL-OVRD-VALUE (GAA-INDEX)                ELGMAXIM
00546        TO ACCUM-INTERVAL-OVRD-VALUE.                              ELGMAXIM
00547      MOVE GAA-BAMA-L-O-B (GAA-INDEX) TO ACCUM-L-O-B.              ELGMAXIM
00548      MOVE GAA-BAMA-REINSTATEMENT-IND (GAA-INDEX)                  ELGMAXIM
00549        TO ACCUM-REINSTATEMENT-IND.                                ELGMAXIM
00550      MOVE GAA-BAMA-DEFINITION (GAA-INDEX) TO ACCUM-DEFINITION.    ELGMAXIM
00551      SET CARRY-OVER-CREDIT-IND-NA                                 ELGMAXIM
00552          ASCEND-DESCEND-IND-NA                                    ELGMAXIM
00553       TO TRUE.                                                    ELGMAXIM
00554      MOVE GAA-BAMA-CONDITION (GAA-INDEX) TO  ACCUM-CONDITION.     ELGMAXIM
00555      MOVE GAA-BAMA-FAM-OR-INDIV (GAA-INDEX)                       ELGMAXIM
00556        TO ACCUM-FAM-OR-INDIV.                                     ELGMAXIM
00557      MOVE ZEROS TO ACCUM-DED-BASE-AMT-SOURCE-IND.                 ELGMAXIM
00558      MOVE ZEROS TO ACCUM-OPX-BASE-AMT-SOURCE-IND.                 ELGMAXIM
00559      MOVE GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX)                    ELGMAXIM
00560        TO ACCUM-VALUE-QUALIFIER.                                  ELGMAXIM
00561      MOVE GAA-BAMA-RELATIONSHIP-IND (GAA-INDEX)                   ELGMAXIM
00562        TO ACCUM-RELATIONSHIP-IND.                                 ELGMAXIM
00563      MOVE GAA-BAMA-AGE-LIMIT-FROM (GAA-INDEX)                     ELGMAXIM
00564        TO ACCUM-AGE-LIMIT-FROM-VAL.                               ELGMAXIM
00565      MOVE GAA-BAMA-AGE-QUAL-IND-FROM (GAA-INDEX)                  ELGMAXIM
00566        TO ACCUM-AGE-LIMIT-FROM-IND.                               ELGMAXIM
00567      MOVE GAA-BAMA-AGE-LIMIT-TO (GAA-INDEX)                       ELGMAXIM
00568        TO ACCUM-AGE-LIMIT-TO-VAL.                                 ELGMAXIM
00569      MOVE GAA-BAMA-AGE-QUAL-IND-TO (GAA-INDEX)                    ELGMAXIM
00570        TO ACCUM-AGE-LIMIT-TO-IND.                                 ELGMAXIM
00571      SET LMT-MANDATORY-IND-NA TO TRUE.                            ELGMAXIM
00572      MOVE GAA-BAMA-CO-PAY-IND (GAA-INDEX)                         ELGMAXIM
00573        TO ACCUM-CO-PAY-IND (1).                                   ELGMAXIM
00574      MOVE GAA-BAMA-SERVICE-GROUP (GAA-INDEX)                      ELGMAXIM
00575        TO ACCUM-SERVICE-GROUP.                                    ELGMAXIM
00576      MOVE GAA-BAMA-INTERNAL-DESCRIPTOR (GAA-INDEX)                ELGMAXIM
00577        TO ACCUM-INTERNAL-DESCRIPTOR.                              ELGMAXIM
00578      MOVE GAA-BAMA-DAY-FACTOR-IND (GAA-INDEX)                     ELGMAXIM
00579        TO ACCUM-DAY-FACTOR-IND.                                   ELGMAXIM
00580      MOVE GAA-BAMA-CLAIM-LVL-ACCUM-IND (GAA-INDEX)                ELGMAXIM
00581        TO ACCUM-CLAIM-LVL-ACCUM-IND.                              ELGMAXIM
00582      MOVE GAA-BAMA-BEN-PER-MAX-OVRD-IND (GAA-INDEX)               ELGMAXIM
00583        TO ACCUM-BEN-PER-MAX-OVRD-IND.                             ELGMAXIM
00584      SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE.                          ELGMAXIM
00585                                                                   ELGMAXIM
00586 * -- SET OCCURRENCE PROVIDER CLASS INFORMATION                    ELGMAXIM
00587      EVALUATE TRUE ALSO TRUE                                      ELGMAXIM
00588         WHEN      SW-INTRNL-INST-PROV-CLASS                       ELGMAXIM
00589              ALSO SW-INTRNL-NOT-PROF-PROV-CLASS                   ELGMAXIM
00590            SET ACCUM-PRVDR-CLS-INST TO TRUE                       ELGMAXIM
00591         WHEN      SW-INTRNL-NOT-INST-PROV-CLASS                   ELGMAXIM
00592              ALSO SW-INTRNL-PROF-PROV-CLASS                       ELGMAXIM
00593            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELGMAXIM
00594         WHEN OTHER                                                ELGMAXIM
00595            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELGMAXIM
00596         END-EVALUATE.                                             ELGMAXIM
00597                                                                   ELGMAXIM
00598 * -- SET OCCURRENCE PROVIDER SPEC INFORMATION                     ELGMAXIM
00599         IF SW-INTRNL-PROF-PROV-CLASS                              ELGMAXIM
00600            SET ACCUM-PRVDR-CLS-PROF TO TRUE                       ELGMAXIM
00601         ELSE                                                      ELGMAXIM
00602            SET ACCUM-PRVDR-CLS-ALL TO TRUE                        ELGMAXIM
00603         END-IF.                                                   ELGMAXIM
00604                                                                   ELGMAXIM
00605 * -- SET VARIABLE PORTION DATA ELEMENTS                           ELGMAXIM
00606      MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                        ELGMAXIM
00607        TO ACCUM-VALUE-LIMIT (1).                                  ELGMAXIM
00608      MOVE WS-IBGR-SLOT-NBR TO ACCUM-IBGR-SLOT-NBR (1).            ELGMAXIM
00609      MOVE WS-IDGD-SLOT-NBR TO ACCUM-IDGD-SLOT-NBR (1).            ELGMAXIM
00610      MOVE WS-IPGN-SLOT-NBR TO ACCUM-IPGN-SLOT-NBR (1).            ELGMAXIM
00611      MOVE WS-IPGP-SLOT-NBR TO ACCUM-IPGP-SLOT-NBR (1).            ELGMAXIM
00612      MOVE WS-IPGT-SLOT-NBR TO ACCUM-IPGT-SLOT-NBR (1).            ELGMAXIM
00613      MOVE WS-IPGS-SLOT-NBR TO ACCUM-IPGS-SLOT-NBR (1).            ELGMAXIM
00614                                                                   ELGMAXIM
00615 /***********************************************************      ELGMAXIM
00616 *                                                          *      ELGMAXIM
00617 *    CHECK EXTRACT DATA INTEGRITY                          *      ELGMAXIM
00618 *                                                          *      ELGMAXIM
00619 ************************************************************      ELGMAXIM
00620                                                                   ELGMAXIM
00621  0410-CHK-EXTRACT-DATA-INTGRTY.                                   ELGMAXIM
00622      IF ACCUM-FYI-VALUE = ZEROS OR SPACES OR LOW-VALUES           ELGMAXIM
00623      THEN                                                         ELGMAXIM
00624         SET FYI-VALUE-NA TO TRUE                                  ELGMAXIM
00625      END-IF.                                                      ELGMAXIM
00626                                                                   ELGMAXIM
00627      IF ACCUM-COST-CONTAIN-IND = ZEROS OR SPACES OR LOW-VALUES    ELGMAXIM
00628      THEN                                                         ELGMAXIM
00629         SET COST-CONTAIN-IND-NA TO TRUE                           ELGMAXIM
00630      END-IF.                                                      ELGMAXIM
00631                                                                   ELGMAXIM
00632      IF ACCUM-PLACE-OF-TREATMENT = ZEROS OR SPACES OR LOW-VALUES  ELGMAXIM
00633      THEN                                                         ELGMAXIM
00634         SET PLACE-OF-TREATMENT-NA TO TRUE                         ELGMAXIM
00635      END-IF.                                                      ELGMAXIM
00636                                                                   ELGMAXIM
00637      IF ACCUM-BENEFIT-PERIOD = ZEROS OR SPACES OR LOW-VALUES      ELGMAXIM
00638      THEN                                                         ELGMAXIM
00639         SET BENEFIT-PERIOD-NA TO TRUE                             ELGMAXIM
00640      END-IF.                                                      ELGMAXIM
00641                                                                   ELGMAXIM
00642      IF ACCUM-INTERVAL-OVRD-IND = ZEROS OR SPACES OR LOW-VALUES   ELGMAXIM
00643      THEN                                                         ELGMAXIM
00644         SET INTERVAL-OVRD-IND-NA TO TRUE                          ELGMAXIM
00645      END-IF.                                                      ELGMAXIM
00646                                                                   ELGMAXIM
00647      IF ACCUM-L-O-B = ZEROS OR SPACES OR LOW-VALUES               ELGMAXIM
00648      THEN                                                         ELGMAXIM
00649         SET L-O-B-NA TO TRUE                                      ELGMAXIM
00650      END-IF.                                                      ELGMAXIM
00651                                                                   ELGMAXIM
00652      IF ACCUM-REINSTATEMENT-IND = ZEROS OR SPACES OR LOW-VALUES   ELGMAXIM
00653      THEN                                                         ELGMAXIM
00654         SET REINSTATEMENT-IND-NA TO TRUE                          ELGMAXIM
00655      END-IF.                                                      ELGMAXIM
00656                                                                   ELGMAXIM
00657      IF ACCUM-DEFINITION = ZEROS OR SPACES OR LOW-VALUES          ELGMAXIM
00658      THEN                                                         ELGMAXIM
00659         SET DEFINITION-NA TO TRUE                                 ELGMAXIM
00660      END-IF.                                                      ELGMAXIM
00661                                                                   ELGMAXIM
00662      IF   ACCUM-CARRY-OVER-CREDIT-IND                             ELGMAXIM
00663         = ZEROS OR SPACES OR LOW-VALUES                           ELGMAXIM
00664      THEN                                                         ELGMAXIM
00665         SET CARRY-OVER-CREDIT-IND-NA TO TRUE                      ELGMAXIM
00666      END-IF.                                                      ELGMAXIM
00667                                                                   ELGMAXIM
00668      IF ACCUM-ASCEND-DESCEND-IND = ZEROS OR SPACES OR LOW-VALUES  ELGMAXIM
00669      THEN                                                         ELGMAXIM
00670         SET ASCEND-DESCEND-IND-NA TO TRUE                         ELGMAXIM
00671      END-IF.                                                      ELGMAXIM
00672                                                                   ELGMAXIM
00673      IF ACCUM-RELATIONSHIP-IND = ZEROS OR SPACES OR LOW-VALUES    ELGMAXIM
00674      THEN                                                         ELGMAXIM
00675         SET RELATIONSHIP-IND-NA TO TRUE                           ELGMAXIM
00676      END-IF.                                                      ELGMAXIM
00677                                                                   ELGMAXIM
00678      IF ACCUM-AGE-LIMIT-TO-IND = ZEROS OR SPACES OR LOW-VALUES    ELGMAXIM
00679      THEN                                                         ELGMAXIM
00680         SET AGE-LMT-TO-IND-NA TO TRUE                             ELGMAXIM
00681      END-IF.                                                      ELGMAXIM
00682                                                                   ELGMAXIM
00683      IF ACCUM-AGE-LIMIT-FROM-IND = ZEROS OR SPACES OR LOW-VALUES  ELGMAXIM
00684      THEN                                                         ELGMAXIM
00685         SET AGE-LMT-FROM-IND-NA TO TRUE                           ELGMAXIM
00686      END-IF.                                                      ELGMAXIM
00687                                                                   ELGMAXIM
00688      IF ACCUM-LMT-MANDATORY-IND = ZEROS OR SPACES OR LOW-VALUES   ELGMAXIM
00689      THEN                                                         ELGMAXIM
00690         SET LMT-MANDATORY-IND-NA TO TRUE                          ELGMAXIM
00691      END-IF.                                                      ELGMAXIM
00692                                                                   ELGMAXIM
00693      IF ACCUM-CO-PAY-IND (1) = ZEROS OR SPACES OR LOW-VALUES      ELGMAXIM
00694      THEN                                                         ELGMAXIM
00695         SET CO-PAY-IND-NA (1) TO TRUE                             ELGMAXIM
00696      END-IF.                                                      ELGMAXIM
00697                                                                   ELGMAXIM
00698      IF ACCUM-SERVICE-GROUP = ZEROS OR SPACES OR LOW-VALUES       ELGMAXIM
00699      THEN                                                         ELGMAXIM
00700         SET SERVICE-GROUP-NA TO TRUE                              ELGMAXIM
00701      END-IF.                                                      ELGMAXIM
00702                                                                   ELGMAXIM
00703      IF ACCUM-INTERNAL-DESCRIPTOR = ZEROS OR SPACES OR LOW-VALUES ELGMAXIM
00704      THEN                                                         ELGMAXIM
00705         SET INTERNAL-DESCRIPTOR-NA TO TRUE                        ELGMAXIM
00706      END-IF.                                                      ELGMAXIM
00707                                                                   ELGMAXIM
00708      IF ACCUM-DAY-FACTOR-IND = ZEROS OR SPACES OR LOW-VALUES      ELGMAXIM
00709      THEN                                                         ELGMAXIM
00710         SET DAY-FACTOR-IND-NA TO TRUE                             ELGMAXIM
00711      END-IF.                                                      ELGMAXIM
00712                                                                   ELGMAXIM
00713      IF ACCUM-CLAIM-LVL-ACCUM-IND = ZEROS OR SPACES OR LOW-VALUES ELGMAXIM
00714      THEN                                                         ELGMAXIM
00715         SET CLAIM-LVL-ACCUM-IND-NA TO TRUE                        ELGMAXIM
00716      END-IF.                                                      ELGMAXIM
00717                                                                   ELGMAXIM
00718      IF ACCUM-BEN-PER-MAX-OVRD-IND = ZEROS OR SPACES OR LOW-VALUESELGMAXIM
00719      THEN                                                         ELGMAXIM
00720         SET BEN-PER-MAX-OVRD-IND-NA TO TRUE                       ELGMAXIM
00721      END-IF.                                                      ELGMAXIM
00722                                                                   ELGMAXIM
00723      IF ACCUM-1ST-DOLR-COVRGE-LMT = ZEROS OR SPACES OR LOW-VALUES ELGMAXIM
00724      THEN                                                         ELGMAXIM
00725         SET 1ST-DOLR-COVRGE-LMT-NA TO TRUE                        ELGMAXIM
00726      END-IF.                                                      ELGMAXIM
00727                                                                   ELGMAXIM
00728 /***********************************************************      ELGMAXIM
00729 *                                                          *      ELGMAXIM
00730 *        ADD ACCUM OCCURENCE TO FILE                       *      ELGMAXIM
00731 *                                                          *      ELGMAXIM
00732 ************************************************************      ELGMAXIM
00733                                                                   ELGMAXIM
00734  0710-WRITE-EXTRACT-RECORD.                                       ELGMAXIM
00735      PERFORM 9070-EST-ADR-OF-TEMPORARY-FILE.                      ELGMAXIM
00736      SET  IOP-ADD TO TRUE.                                        ELGMAXIM
00737      SET  IOP-FCQ-NONE TO TRUE.                                   ELGMAXIM
00738      SET  IOP-KVQ-NONE TO TRUE.                                   ELGMAXIM
00739      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGMAXIM
00740                                                                   ELGMAXIM
00741 /***********************************************************      ELGMAXIM
00742 *                                                          *      ELGMAXIM
00743 *    ESTABLISH ADDRESSABILITY OF THE WORK FILE             *      ELGMAXIM
00744 *                                                          *      ELGMAXIM
00745 ************************************************************      ELGMAXIM
00746                                                                   ELGMAXIM
00747  9070-EST-ADR-OF-TEMPORARY-FILE.                                  ELGMAXIM
00748      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGMAXIM
00749      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGMAXIM
00750         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                    ELGMAXIM
00751         END-CALL.                                                 ELGMAXIM
00752      IF CIA-RC-PTR-NULL                                           ELGMAXIM
00753         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGMAXIM
00754         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELGMAXIM
00755      END-IF.                                                      ELGMAXIM
