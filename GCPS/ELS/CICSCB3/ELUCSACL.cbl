00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUCSACL
00003  PROGRAM-ID.         ELUCSACL.                                       LV001
00004                                                                   ELUCSACL
00005  AUTHOR.             GEORGE E MOORE.                              ELUCSACL
00006                                                                   ELUCSACL
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUCSACL
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUCSACL
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUCSACL
00010                      233 N. MICHIGAN AVE                          ELUCSACL
00011                      CHICAGO, ILLINOIS 60601                      ELUCSACL
00012                                                                   ELUCSACL
00013  DATE-WRITTEN.       20-SEPT-1989.                                ELUCSACL
00014                                                                   ELUCSACL
00015  DATE-COMPILED.                                                   ELUCSACL
00016                                                                   ELUCSACL
00017  SECURITY.           COPYRIGHT 1986,                              ELUCSACL
00018                      HEALTH CARE SERVICE CORPORATION              ELUCSACL
00019      SKIP3                                                        ELUCSACL
00020  TITLE 'ELS CONTRACT SUMMARY COINSURANCE EXTRACT UTILITY      '.  ELUCSACL
00021  ENVIRONMENT DIVISION.                                            ELUCSACL
00022                                                                   ELUCSACL
00023  CONFIGURATION SECTION.                                           ELUCSACL
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELUCSACL
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELUCSACL
00026      EJECT                                                        ELUCSACL
00027 ******************************************************************ELUCSACL
00028 *              MAINTAINANCE HISTORY                              *ELUCSACL
00029 *                                                                *ELUCSACL
00030 *   MOD     DATE     BY     ACTION                               *ELUCSACL
00031 *  01.00  20-SEP-89  GEM    CREATED - CLONED FROM ELUCSABM       *ELUCSACL
00032 *  01.01  30-MAY-91  RKH    UPDATE  - CORRECTED LOGIC ERROR IN   *ELUCSACL
00033 *                           MODULE - EXAMINE-IBGR                *ELUCSACL
00034 *                           MODULE WOULD ABEND IF NO IBGR TAB    *ELUCSACL
00035 *                           WAS CODED IN ACCUM TABULARS/CHANGED  *ELUCSACL
00036 *                           FROM ABEND TO PERFORM THE MODULE     *ELUCSACL
00037 *                           PROCESS-NOT-FOUND-IBGR               *ELUCSACL
00038 *  01.02  16-OCT-92  BAK    CHANGE HOLD FIELDS FOR CONDITION     *ELUCSACL
00039 *                           CODE BITS FROM 20 TO 30 BYTES.       *ELUCSACL
00040 *                                                                *ELUCSACL
00041 ******************************************************************ELUCSACL
00042                                                                   ELUCSACL
00043  DATA DIVISION.                                                   ELUCSACL
00044  WORKING-STORAGE SECTION.                                         ELUCSACL
00045  01  WS-HOLD-AREA.                                                ELUCSACL
00046      03  WS-LINE-OF-BUSINESS       OCCURS 2 TIMES                 ELUCSACL
00047                                    PIC X(01).                     ELUCSACL
00048          88  INSTITUTIONAL-LOB              VALUE  '1' '5'        ELUCSACL
00049                                                    '4' '6' '8'.   ELUCSACL
00050          88  PROFESSIONAL-LOB               VALUE  '2' '4'        ELUCSACL
00051                                                    '5' '7' '8'.   ELUCSACL
00052          88  SUPPLEMENTAL-LOB                VALUE '3' '6'        ELUCSACL
00053                                                    '7' '8'.       ELUCSACL
00054 *                                                                 ELUCSACL
00055     03  WS-OBSTETRICS                  PIC X(04).                 ELUCSACL
00056         88  OB-NORMAL                  VALUE 'OBNM' 'OBNS' 'OBND' ELUCSACL
00057                                              'EABI' 'EABO'.       ELUCSACL
00058         88  OB-COMPLICATED             VALUE 'OBCM' 'OBCS' 'OBCD' ELUCSACL
00059                                              'TABI' 'TABO'.       ELUCSACL
00060     03  WS-EMERGENCY                   PIC X(04).                 ELUCSACL
00061         88  EMER-ACCIDENT                    VALUE 'EAER' ' EAC'. ELUCSACL
00062         88  EMER-MEDICAL                     VALUE 'EMER' ' EMC'. ELUCSACL
00063 *                                                                 ELUCSACL
00064  01  WS-SUBSCRIPTS   USAGE COMP SYNC.                             ELUCSACL
00065      05  WS-BP-SUB                PIC S9(4).                      ELUCSACL
00066      05  WS-CSPT-SUB              PIC S9(4).                      ELUCSACL
00067      05  WS-BPL-X-SUB             PIC S9(4).                      ELUCSACL
00068      05  WS-CSBP-X-SUB            PIC S9(4).                      ELUCSACL
00069      05  WS-ATBL-X-SUB            PIC S9(4).                      ELUCSACL
00070      05  WS-IBGR-X-SUB            PIC S9(4).                      ELUCSACL
00071      05  WS-GX1-SUB               PIC S9(4).                      ELUCSACL
00072 *                                                                 ELUCSACL
00073  01  PRIORITY-SUB-SAVE  USAGE COMP SYNC.                          ELUCSACL
00074      05  IN-PRIORITY-LVL          PIC S9(4) VALUE ZERO.           ELUCSACL
00075      05  PRIORITY-LVL             PIC S9(4) VALUE ZERO.           ELUCSACL
00076 *                                                                 ELUCSACL
00077  01  WS-ACL-KEY.                                                  ELUCSACL
00078      03  WS-ACL-KEY-A-D-IND     PIC X(01).                        ELUCSACL
00079      03  WS-ACL-KEY-TYPE        PIC X(02).                        ELUCSACL
00080      03  WS-ACL-KEY-MAN-IND     PIC X(01).                        ELUCSACL
00081      03  WS-ACL-KEY-F-R-I       PIC X(01).                        ELUCSACL
00082      03  WS-ACL-KEY-L-O-B       PIC X(01).                        ELUCSACL
00083      03  WS-ACL-KEY-INT-DES     PIC X(09).                        ELUCSACL
00084      03  WS-ACL-KEY-SER-GRP     PIC X(02).                        ELUCSACL
00085      03  WS-ACL-KEY-TRT-GRP     PIC X(02).                        ELUCSACL
00086      03  WS-ACL-KEY-COND        PIC X(30).                        ELUCSACL
00087      03  WS-ACL-KEY-COST        PIC X(02).                        ELUCSACL
00088      03  WS-ACL-KEY-COPAY       PIC X(01).                        ELUCSACL
00089 *                                                                 ELUCSACL
00090  01  WS-HOLD-KEY.                                                 ELUCSACL
00091      03  WS-HOLD-KEY-A-D-IND    PIC X(01).                        ELUCSACL
00092      03  WS-HOLD-KEY-TYPE       PIC X(02).                        ELUCSACL
00093      03  WS-HOLD-KEY-MAN-IND    PIC X(01).                        ELUCSACL
00094      03  WS-HOLD-KEY-F-R-I      PIC X(01).                        ELUCSACL
00095      03  WS-HOLD-KEY-L-O-B      PIC X(01).                        ELUCSACL
00096      03  WS-HOLD-KEY-INT-DES    PIC X(09).                        ELUCSACL
00097      03  WS-HOLD-KEY-SER-GRP    PIC X(02).                        ELUCSACL
00098      03  WS-HOLD-KEY-TRT-GRP    PIC X(02).                        ELUCSACL
00099      03  WS-HOLD-KEY-COND       PIC X(30).                        ELUCSACL
00100      03  WS-HOLD-KEY-COST       PIC X(02).                        ELUCSACL
00101      03  WS-HOLD-KEY-COPAY      PIC X(01).                        ELUCSACL
00102 *                                                                 ELUCSACL
00103  01  WS-ACL-ENTRIES.                                              ELUCSACL
00104      03  WS-ACL-BISCEND-IND     PIC X(01).                        ELUCSACL
00105      03  WS-ACL-PCT-LVL         PIC S9(03)      COMP-3.           ELUCSACL
00106      03  WS-ACL-VAL-LMT         PIC S9(07)V99   COMP-3.           ELUCSACL
00107 *                                                                 ELUCSACL
00108  01  WS-SWITCHES.                                                 ELUCSACL
00109      03  WS-APPLICABLE-SWITCH     PIC X(01)  VALUE SPACES.        ELUCSACL
00110          88  APPLICABLE                      VALUE 'Y'.           ELUCSACL
00111          88  NOT-APPLICABLE                  VALUE 'N'.           ELUCSACL
00112      03  WS-IBGR-SWITCH            PIC X(01) VALUE SPACES.        ELUCSACL
00113          88  IBGR-FOUND                      VALUE 'Y'.           ELUCSACL
00114          88  NO-IBGR-FOUND                   VALUE 'N'.           ELUCSACL
00115      03  WS-BP-ID-SWITCH           PIC X(01) VALUE SPACES.        ELUCSACL
00116          88  WS-BP-ID-FOUND                  VALUE 'Y'.           ELUCSACL
00117          88  WS-BP-ID-NOT-FOUND              VALUE 'N'.           ELUCSACL
00118      03  WS-CONCLUSIVE-SWITCH      PIC X(01) VALUE SPACES.        ELUCSACL
00119          88  CONCLUSIVE                      VALUE 'Y'.           ELUCSACL
00120          88  NOT-CONCLUSIVE                  VALUE 'N'.           ELUCSACL
00121      03  WS-QUALIFICATION-STATUS   PIC X(01) VALUE SPACES.        ELUCSACL
00122          88  WS-BP-QUALIFIED                 VALUE 'Y'.           ELUCSACL
00123          88  WS-BP-NOT-QUALIFIED             VALUE 'N'.           ELUCSACL
00124      03  WS-TABULAR-STATUS         PIC X(02) VALUE SPACES.        ELUCSACL
00125          88  TABULAR-INCLUDED                VALUE 'IN'.          ELUCSACL
00126          88  TABULAR-EXCLUDED                VALUE 'EX'.          ELUCSACL
00127      03  WS-LEVEL-STATUS           PIC X(02) VALUE SPACES.        ELUCSACL
00128          88  LEVEL-IS-BP                     VALUE 'BP'.          ELUCSACL
00129          88  LEVEL-IS-GC                     VALUE 'GC'.          ELUCSACL
00130      03  IP-OP-IND                 PIC X(01) VALUE SPACES.        ELUCSACL
00131          88  IN-PATIENT                      VALUE 'I'.           ELUCSACL
00132          88  OUT-PATIENT                     VALUE 'O'.           ELUCSACL
00133          88  BOTH-IP-OP                      VALUE 'B'.           ELUCSACL
00134      03  SEARCH-SWITCH             PIC X(01) VALUE SPACES.        ELUCSACL
00135          88  SEARCH-IS-COMPLETE              VALUE 'Y'.           ELUCSACL
00136      03  WS-BP-ID-FORMAT           PIC X(01) VALUE SPACES.        ELUCSACL
00137          88  INSTITUTIONAL                  VALUE  'A' 'B' 'W'.   ELUCSACL
00138          88  PROFESSIONAL                   VALUE  'C' 'D' 'E'.   ELUCSACL
00139      03  SPECIFIC-SWITCH           PIC X(01) VALUE SPACES.        ELUCSACL
00140          88  SPECIFIC                        VALUE 'Y'.           ELUCSACL
00141          88  NOT-SPECIFIC                    VALUE 'N'.           ELUCSACL
00142      03  IP-OP-SELECT-SWT          PIC X(01) VALUE SPACES.        ELUCSACL
00143          88  IP-OP-IND-SELECTED              VALUE 'Y'.           ELUCSACL
00144      03  SIMPLE-R-VARYING-SWT      PIC X(01) VALUE SPACES.        ELUCSACL
00145          88  SIMPLE-ACL                      VALUE 'S'.           ELUCSACL
00146          88  VARYING-ACL                     VALUE 'V'.           ELUCSACL
00147      03  WS-COINS-FOUND-SW         PIC X(01) VALUE SPACES.        ELUCSACL
00148          88  COINS-FOUND                     VALUE 'Y'.           ELUCSACL
00149          88  COINS-NOT-FOUND                 VALUE 'N'.           ELUCSACL
00150      03  WS-ACCUM-SW               PIC X(01) VALUE SPACES.        ELUCSACL
00151          88  ACCUM-IS-OVERALL                VALUE 'Y'.           ELUCSACL
00152          88  ACCUM-IS-NOT-OVERALL            VALUE 'N'.           ELUCSACL
00153 *                                                                 ELUCSACL
00154  01  CONF-FACTORS.                                                ELUCSACL
00155      03  CF-TWO                    COMP-1 VALUE +0.200000E+00.    ELUCSACL
00156      03  CF-FIVE                   COMP-1 VALUE +0.500000E+00.    ELUCSACL
00157      03  CF-ONE                    COMP-1 VALUE +1.000000E+00.    ELUCSACL
00158 *                                                                 ELUCSACL
00159  01  PROGRAM-CONSTANTS.                                           ELUCSACL
00160      03  PC-ONE                    PIC X(01) VALUE '1'.           ELUCSACL
00161      03  PC-OB                     PIC X(02) VALUE '0B'.          ELUCSACL
00162      03  PC-OC                     PIC X(02) VALUE '0C'.          ELUCSACL
00163      03  PC-OD                     PIC X(02) VALUE '0D'.          ELUCSACL
00164 *                                                                 ELUCSACL
00165      COPY ELSBPTBL.                                               ELUCSACL
00166 *                                                                 ELUCSACL
00167      COPY ELSBPITC.                                               ELUCSACL
00168  LINKAGE SECTION.                                                 ELUCSACL
00169  01  DFHCOMMAREA.                                                 ELUCSACL
00170      COPY ELSCOMMC.                                               ELUCSACL
00171 *                                                                 ELUCSACL
00172      COPY ELSCIA2C.                                               ELUCSACL
00173 /                                                                 ELUCSACL
00174      COPY ELSCSACC.                                               ELUCSACL
00175 /                                                                 ELUCSACL
00176      COPY ELSCSADC.                                               ELUCSACL
00177 /                                                                 ELUCSACL
00178      COPY ELSCSPTC.                                               ELUCSACL
00179 /                                                                 ELUCSACL
00180      COPY ELSIBGRC.                                               ELUCSACL
00181 /                                                                 ELUCSACL
00182      COPY ELSCSBPC.                                               ELUCSACL
00183 /                                                                 ELUCSACL
00184      COPY ELSATBLC.                                               ELUCSACL
00185 /                                                                 ELUCSACL
00186  01  INTERNAL-TABULAR-RECORD.                                     ELUCSACL
00187      COPY GCTIBGRC.                                               ELUCSACL
00188 /                                                                 ELUCSACL
00189  01  BENEFIT-PERIOD-TABLE.                                        ELUCSACL
00190      03  BENEFIT-PERIOD        OCCURS 25 TIMES                    ELUCSACL
00191                                PIC X(02).                         ELUCSACL
00192 /                                                                 ELUCSACL
00193      EJECT                                                        ELUCSACL
00194  PROCEDURE DIVISION.                                              ELUCSACL
00195                                                                   ELUCSACL
00196 ************************************************************      ELUCSACL
00197 *                                                          *      ELUCSACL
00198 *        CONTRACT SUMMARY COINSURANCE                      *      ELUCSACL
00199 *                                                          *      ELUCSACL
00200 ************************************************************      ELUCSACL
00201  CONTRACT-SUMMARY-COINSURANCE.                                    ELUCSACL
00202      PERFORM ESTABLISH-ADDRESS-OF-CNTL-BLK.                       ELUCSACL
00203                                                                   ELUCSACL
00204      PERFORM ESTABLISH-ADDRESS-OF-PTR-LIST.                       ELUCSACL
00205                                                                   ELUCSACL
00206      PERFORM ESTABLISH-ADDRESS-OF-ACCUM-TBL.                      ELUCSACL
00207                                                                   ELUCSACL
00208      PERFORM ESTABLISH-ADDRESS-OF-INTRL-TBL.                      ELUCSACL
00209                                                                   ELUCSACL
00210      PERFORM ESTABLISH-ADDRESS-OF-ATBL-TBL.                       ELUCSACL
00211                                                                   ELUCSACL
00212      PERFORM ESTABLISH-ADDRESS-OF-ACL-TBL.                        ELUCSACL
00213                                                                   ELUCSACL
00214      PERFORM PROCESS-BENEFIT-PROVISION-PTRS                       ELUCSACL
00215              VARYING WS-CSPT-SUB FROM 1 BY 1                      ELUCSACL
00216                UNTIL WS-CSPT-SUB > CSPT-TBL-CNT.                  ELUCSACL
00217      GOBACK.                                                      ELUCSACL
00218      EJECT                                                        ELUCSACL
00219 ************************************************************      ELUCSACL
00220 *                                                          *      ELUCSACL
00221 *        PROCESS BENEFIT PROVISION PTRS                    *      ELUCSACL
00222 *                                                          *      ELUCSACL
00223 ************************************************************      ELUCSACL
00224  PROCESS-BENEFIT-PROVISION-PTRS.                                  ELUCSACL
00225      SET CSPT-IDX TO WS-CSPT-SUB.                                 ELUCSACL
00226      IF CSPT-BP-TBL-PTR (CSPT-IDX) NOT = NULLS                    ELUCSACL
00227          PERFORM PROCESS-COINSURANCE-PER-BEN-PR.                  ELUCSACL
00228                                                                   ELUCSACL
00229 ************************************************************      ELUCSACL
00230 *                                                          *      ELUCSACL
00231 *        PROCESS COINSURANCE PER BENEFIT PROVISION POINTER *      ELUCSACL
00232 *                                                          *      ELUCSACL
00233 ************************************************************      ELUCSACL
00234  PROCESS-COINSURANCE-PER-BEN-PR.                                  ELUCSACL
00235      SET ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE TO               ELUCSACL
00236            CSPT-BP-TBL-PTR (CSPT-IDX).                            ELUCSACL
00237      PERFORM EXTRACT-COIN-INFO-FOR-EACH-BEN                       ELUCSACL
00238          VARYING WS-CSBP-X-SUB FROM 1 BY 1 UNTIL                  ELUCSACL
00239                            WS-CSBP-X-SUB  > CSBP-TBL-CNT.         ELUCSACL
00240                                                                   ELUCSACL
00241 ************************************************************      ELUCSACL
00242 *                                                          *      ELUCSACL
00243 *        EXTRACT COINSURANCE INFO FOR EACH BEN PROVISION   *      ELUCSACL
00244 *                                                          *      ELUCSACL
00245 ************************************************************      ELUCSACL
00246  EXTRACT-COIN-INFO-FOR-EACH-BEN.                                  ELUCSACL
00247      SET CSBP-X-IDX TO WS-CSBP-X-SUB.                             ELUCSACL
00248      IF NOT CSBP-FORMAT-W (CSBP-X-IDX) AND                        ELUCSACL
00249             CSBP-PROVN-PRICING-METHD (CSBP-X-IDX) > ZERO          ELUCSACL
00250          PERFORM PROCESS-ITEMS-WITH-PROVN-PRICI.                  ELUCSACL
00251                                                                   ELUCSACL
00252 ************************************************************      ELUCSACL
00253 *                                                          *      ELUCSACL
00254 *        PROCESS ITEMS WITH PROVN PRICING METHD            *      ELUCSACL
00255 *                                                          *      ELUCSACL
00256 ************************************************************      ELUCSACL
00257  PROCESS-ITEMS-WITH-PROVN-PRICI.                                  ELUCSACL
00258      IF CSBP-COVERED (CSBP-X-IDX) AND                             ELUCSACL
00259       ( CSBP-PAYMENT-REQUESTED (CSBP-X-IDX)                       ELUCSACL
00260          OR  CSBP-USE-SUPP-INFO (CSBP-X-IDX) )                    ELUCSACL
00261            PERFORM EXTRACT-COINSURANCE-INFO                       ELUCSACL
00262      ELSE IF CSBP-COVERED-ON-SUPP (CSBP-X-IDX) AND                ELUCSACL
00263              CSBP-USE-SUPP-INFO (CSBP-X-IDX)                      ELUCSACL
00264                 PERFORM EXTRACT-COINSURANCE-INFO.                 ELUCSACL
00265                                                                   ELUCSACL
00266 ************************************************************      ELUCSACL
00267 *                                                          *      ELUCSACL
00268 *        EXTRACT COINSURANCE INFORMATION                   *      ELUCSACL
00269 *                                                          *      ELUCSACL
00270 ************************************************************      ELUCSACL
00271  EXTRACT-COINSURANCE-INFO.                                        ELUCSACL
00272      PERFORM CLEAR-CSBP-ACL-DATA.                                 ELUCSACL
00273      MOVE '100' TO CSBP-COINS-PERCENT-LEVEL (CSBP-X-IDX).         ELUCSACL
00274      SET COINS-NOT-FOUND TO TRUE.                                 ELUCSACL
00275      IF CSAC-ACL-BP-TBL-PTR NOT = NULL  AND                       ELUCSACL
00276         CSBP-BP-ACL-SLOT (CSBP-X-IDX) NOT = ZERO                  ELUCSACL
00277              PERFORM INTERROGATE-BENEFIT-PROVISIONX.              ELUCSACL
00278      IF CSAC-ACL-GC-TBL-PTR NOT = NULL                            ELUCSACL
00279         PERFORM INTERROGATE-GROUP-CONTRACT-LEV.                   ELUCSACL
00280                                                                   ELUCSACL
00281      PERFORM FINAL-TEST.                                          ELUCSACL
00282                                                                   ELUCSACL
00283 ************************************************************      ELUCSACL
00284 *                                                          *      ELUCSACL
00285 *        FINAL TEST & IDENTICAL VARYING ACL TEST           *      ELUCSACL
00286 *                                                          *      ELUCSACL
00287 ************************************************************      ELUCSACL
00288  FINAL-TEST.                                                      ELUCSACL
00289      IF VARYING-ACL                                               ELUCSACL
00290         PERFORM IDENTICAL-VARYING-ACL-TEST.                       ELUCSACL
00291                                                                   ELUCSACL
00292  IDENTICAL-VARYING-ACL-TEST.                                      ELUCSACL
00293      IF CSAD-ACL-NBR-ENTRIES > 1                                  ELUCSACL
00294         PERFORM SCAN-PRIOR-CSAD-TBL-ENTRIES.                      ELUCSACL
00295                                                                   ELUCSACL
00296      SET CSBP-COINS-AD-SUB (CSBP-X-IDX) TO CSAD-ACL-IDX.          ELUCSACL
00297                                                                   ELUCSACL
00298  SCAN-PRIOR-CSAD-TBL-ENTRIES.                                     ELUCSACL
00299      SET CSAD-PREV-IDX TO 1.                                      ELUCSACL
00300      PERFORM WITH TEST BEFORE                                     ELUCSACL
00301              UNTIL CSAD-ACL-KEY (CSAD-PREV-IDX)                   ELUCSACL
00302                  = CSAD-ACL-KEY (CSAD-ACL-IDX)                    ELUCSACL
00303        SET CSAD-PREV-IDX UP BY 1                                  ELUCSACL
00304      END-PERFORM.                                                 ELUCSACL
00305                                                                   ELUCSACL
00306      IF CSAD-ACL-IDX > CSAD-PREV-IDX                              ELUCSACL
00307         SUBTRACT 1 FROM CSAD-ACL-NBR-ENTRIES                      ELUCSACL
00308         SET CSAD-ACL-IDX TO CSAD-PREV-IDX.                        ELUCSACL
00309                                                                   ELUCSACL
00310 ************************************************************      ELUCSACL
00311 *                                                          *      ELUCSACL
00312 *        INTERROGATE BENEFIT PROVISION LEVEL               *      ELUCSACL
00313 *                                                          *      ELUCSACL
00314 ************************************************************      ELUCSACL
00315  INTERROGATE-BENEFIT-PROVISIONX.                                  ELUCSACL
00316      SET LEVEL-IS-BP TO TRUE.                                     ELUCSACL
00317      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                        ELUCSACL
00318          TO CSAC-ACL-BP-TBL-PTR.                                  ELUCSACL
00319      MOVE CSBP-L-O-B (CSBP-X-IDX) TO WS-LINE-OF-BUSINESS (1).     ELUCSACL
00320      PERFORM SEARCH-FOR-MATCHING-SLOT                             ELUCSACL
00321          VARYING WS-ATBL-X-SUB FROM 1 BY 1                        ELUCSACL
00322                    UNTIL WS-ATBL-X-SUB > ATBL-TBL-CNT.            ELUCSACL
00323                                                                   ELUCSACL
00324 ************************************************************      ELUCSACL
00325 *                                                          *      ELUCSACL
00326 *        SEARCH FOR MATCHING SLOT                          *      ELUCSACL
00327 *                                                          *      ELUCSACL
00328 ************************************************************      ELUCSACL
00329  SEARCH-FOR-MATCHING-SLOT.                                        ELUCSACL
00330      SET ATBL-X-IDX TO WS-ATBL-X-SUB.                             ELUCSACL
00331      IF ATBL-SLOT-NUMBER (ATBL-X-IDX) =                           ELUCSACL
00332         CSBP-BP-ACL-SLOT (CSBP-X-IDX)                             ELUCSACL
00333            PERFORM PROCESS-MATCHING-SLOT.                         ELUCSACL
00334                                                                   ELUCSACL
00335 ************************************************************      ELUCSACL
00336 *                                                          *      ELUCSACL
00337 *        INTERROGATE GROUP CONTRACT LEVEL                  *      ELUCSACL
00338 *                                                          *      ELUCSACL
00339 ************************************************************      ELUCSACL
00340  INTERROGATE-GROUP-CONTRACT-LEV.                                  ELUCSACL
00341      SET LEVEL-IS-GC TO TRUE.                                     ELUCSACL
00342      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                        ELUCSACL
00343          TO CSAC-ACL-GC-TBL-PTR.                                  ELUCSACL
00344      MOVE CSBP-L-O-B (CSBP-X-IDX) TO WS-LINE-OF-BUSINESS (1).     ELUCSACL
00345      PERFORM PROCESS-ALL-MATCHING-SLOTS                           ELUCSACL
00346          VARYING WS-ATBL-X-SUB FROM 1 BY 1                        ELUCSACL
00347                    UNTIL WS-ATBL-X-SUB > ATBL-TBL-CNT.            ELUCSACL
00348                                                                   ELUCSACL
00349 ************************************************************      ELUCSACL
00350 *                                                          *      ELUCSACL
00351 *        PROCESS ALL MATCHING SLOTS                        *      ELUCSACL
00352 *                                                          *      ELUCSACL
00353 ************************************************************      ELUCSACL
00354  PROCESS-ALL-MATCHING-SLOTS.                                      ELUCSACL
00355      SET ATBL-X-IDX TO WS-ATBL-X-SUB.                             ELUCSACL
00356      PERFORM PROCESS-MATCHING-SLOT.                               ELUCSACL
00357                                                                   ELUCSACL
00358 ************************************************************      ELUCSACL
00359 *                                                          *      ELUCSACL
00360 *        PROCESS MATCHING SLOT                             *      ELUCSACL
00361 *                                                          *      ELUCSACL
00362 ************************************************************      ELUCSACL
00363  PROCESS-MATCHING-SLOT.                                           ELUCSACL
00364      SET NOT-SPECIFIC TO TRUE.                                    ELUCSACL
00365      PERFORM EXAMINE-COST-CONTAINMENT-IND.                        ELUCSACL
00366      IF APPLICABLE                                                ELUCSACL
00367          PERFORM EXAMINE-SERVICE-GROUP.                           ELUCSACL
00368      IF APPLICABLE                                                ELUCSACL
00369          PERFORM EXAMINE-COVERAGE.                                ELUCSACL
00370      IF APPLICABLE                                                ELUCSACL
00371          PERFORM EXAMINE-IBGR.                                    ELUCSACL
00372      IF APPLICABLE AND NOT-CONCLUSIVE                             ELUCSACL
00373          PERFORM EXAMINE-PLACE-OF-TREATMENT.                      ELUCSACL
00374      IF APPLICABLE AND NOT-CONCLUSIVE                             ELUCSACL
00375          PERFORM EXAMINE-LINE-OF-BUSINESS.                        ELUCSACL
00376      IF APPLICABLE                                                ELUCSACL
00377          PERFORM EXAMINE-CONDITION-BITS.                          ELUCSACL
00378      IF APPLICABLE                                                ELUCSACL
00379          PERFORM EXAMINE-CONFIDENCE-FACTORS.                      ELUCSACL
00380      IF APPLICABLE                                                ELUCSACL
00381          SET COINS-FOUND TO TRUE                                  ELUCSACL
00382          PERFORM SELECT-BENEFIT-PERIOD-PRIORITY.                  ELUCSACL
00383                                                                   ELUCSACL
00384 ************************************************************      ELUCSACL
00385 *                                                          *      ELUCSACL
00386 *        EXAMINE COST CONTAINMENT IND                      *      ELUCSACL
00387 *                                                          *      ELUCSACL
00388 ************************************************************      ELUCSACL
00389  EXAMINE-COST-CONTAINMENT-IND.                                    ELUCSACL
00390      IF ATBL-COST-CONTAIN-IND (ATBL-X-IDX) = ZERO                 ELUCSACL
00391          SET APPLICABLE TO TRUE                                   ELUCSACL
00392        ELSE                                                       ELUCSACL
00393          SET NOT-APPLICABLE TO TRUE                               ELUCSACL
00394        END-IF.                                                    ELUCSACL
00395                                                                   ELUCSACL
00396 ************************************************************      ELUCSACL
00397 *                                                          *      ELUCSACL
00398 *        EXAMINE SERVICE GROUP                             *      ELUCSACL
00399 *                                                          *      ELUCSACL
00400 ************************************************************      ELUCSACL
00401  EXAMINE-SERVICE-GROUP.                                           ELUCSACL
00402      SET NOT-APPLICABLE TO TRUE.                                  ELUCSACL
00403      IF CSBP-OUTPATIENT-ST                                        ELUCSACL
00404          PERFORM INVESTIGATE-OUTPATIENT-CODES.                    ELUCSACL
00405      IF ATBL-SERVICE-GROUP (ATBL-X-IDX) = ZERO                    ELUCSACL
00406         SET APPLICABLE TO TRUE.                                   ELUCSACL
00407                                                                   ELUCSACL
00408 ************************************************************      ELUCSACL
00409 *                                                          *      ELUCSACL
00410 *        INVESTIGATE OUTPATIENT CODES                      *      ELUCSACL
00411 *                                                          *      ELUCSACL
00412 ************************************************************      ELUCSACL
00413  INVESTIGATE-OUTPATIENT-CODES.                                    ELUCSACL
00414      MOVE CSBP-BP-KEY (CSBP-X-IDX) TO WS-EMERGENCY.               ELUCSACL
00415      IF EMER-ACCIDENT AND                                         ELUCSACL
00416         ATBL-SERVICE-GROUP (ATBL-X-IDX) = PC-OC                   ELUCSACL
00417            SET APPLICABLE TO TRUE.                                ELUCSACL
00418      IF EMER-MEDICAL AND                                          ELUCSACL
00419         ATBL-SERVICE-GROUP (ATBL-X-IDX) = PC-OD                   ELUCSACL
00420            SET APPLICABLE TO TRUE.                                ELUCSACL
00421      IF ATBL-SERVICE-GROUP (ATBL-X-IDX) = ZERO                    ELUCSACL
00422            SET APPLICABLE TO TRUE.                                ELUCSACL
00423                                                                   ELUCSACL
00424 ************************************************************      ELUCSACL
00425 *                                                          *      ELUCSACL
00426 *        EXAMINE COVERAGE                                  *      ELUCSACL
00427 *                                                          *      ELUCSACL
00428 ************************************************************      ELUCSACL
00429  EXAMINE-COVERAGE.                                                ELUCSACL
00430      SET NOT-APPLICABLE TO TRUE.                                  ELUCSACL
00431      IF CSBP-COVERED (CSBP-X-IDX) AND                             ELUCSACL
00432         ATBL-CF-BAS (ATBL-X-IDX) > CF-FIVE                        ELUCSACL
00433            SET APPLICABLE TO TRUE                                 ELUCSACL
00434        ELSE                                                       ELUCSACL
00435        IF CSBP-COVERED-ON-SUPP (CSBP-X-IDX) AND                   ELUCSACL
00436           ATBL-CF-SUP (ATBL-X-IDX) > CF-FIVE                      ELUCSACL
00437              SET APPLICABLE TO TRUE                               ELUCSACL
00438        END-IF.                                                    ELUCSACL
00439                                                                   ELUCSACL
00440 ************************************************************      ELUCSACL
00441 *                                                          *      ELUCSACL
00442 *        EXAMINE IBGR                                      *      ELUCSACL
00443 *                                                          *      ELUCSACL
00444 ************************************************************      ELUCSACL
00445  EXAMINE-IBGR.                                                    ELUCSACL
00446      SET NOT-APPLICABLE TO TRUE.                                  ELUCSACL
00447      SET NO-IBGR-FOUND TO TRUE.                                   ELUCSACL
00448      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELUCSACL
00449          CONTINUE                                                 ELUCSACL
00450      ELSE                                                         ELUCSACL
00451          PERFORM PROCESS-INTERNAL-TABULAR-SLOT.                   ELUCSACL
00452      IF IBGR-FOUND                                                ELUCSACL
00453          PERFORM PROCESS-FOUND-IBGR                               ELUCSACL
00454      ELSE                                                         ELUCSACL
00455          PERFORM PROCESS-NOT-FOUND-IBGR.                          ELUCSACL
00456                                                                   ELUCSACL
00457 ************************************************************      ELUCSACL
00458 *                                                          *      ELUCSACL
00459 *        PROCESS INTERNAL TABULAR SLOT                     *      ELUCSACL
00460 *                                                          *      ELUCSACL
00461 ************************************************************      ELUCSACL
00462  PROCESS-INTERNAL-TABULAR-SLOT.                                   ELUCSACL
00463      PERFORM SEARCH-FOR-INTERNAL-TABULAR-SL                       ELUCSACL
00464          VARYING WS-IBGR-X-SUB FROM 1 BY 1                        ELUCSACL
00465                     UNTIL WS-IBGR-X-SUB > IBGR-TBL-CNT            ELUCSACL
00466                        OR IBGR-FOUND.                             ELUCSACL
00467                                                                   ELUCSACL
00468 ************************************************************      ELUCSACL
00469 *                                                          *      ELUCSACL
00470 *        PROCESS FOUND IBGR                                *      ELUCSACL
00471 *                                                          *      ELUCSACL
00472 ************************************************************      ELUCSACL
00473  PROCESS-FOUND-IBGR.                                              ELUCSACL
00474      IF TABULAR-INCLUDED                                          ELUCSACL
00475          PERFORM PROCESS-INCLUDED                                 ELUCSACL
00476      ELSE                                                         ELUCSACL
00477          PERFORM PROCESS-EXCLUDED.                                ELUCSACL
00478                                                                   ELUCSACL
00479 ************************************************************      ELUCSACL
00480 *                                                          *      ELUCSACL
00481 *        PROCESS NOT FOUND IBGR                            *      ELUCSACL
00482 *                                                          *      ELUCSACL
00483 ************************************************************      ELUCSACL
00484  PROCESS-NOT-FOUND-IBGR.                                          ELUCSACL
00485      IF LEVEL-IS-BP                                               ELUCSACL
00486          SET CONCLUSIVE TO TRUE                                   ELUCSACL
00487          SET APPLICABLE TO TRUE                                   ELUCSACL
00488        ELSE                                                       ELUCSACL
00489          SET APPLICABLE TO TRUE                                   ELUCSACL
00490          SET NOT-CONCLUSIVE TO TRUE                               ELUCSACL
00491        END-IF.                                                    ELUCSACL
00492                                                                   ELUCSACL
00493 ************************************************************      ELUCSACL
00494 *                                                          *      ELUCSACL
00495 *        PROCESS INCLUDED                                  *      ELUCSACL
00496 *                                                          *      ELUCSACL
00497 ************************************************************      ELUCSACL
00498  PROCESS-INCLUDED.                                                ELUCSACL
00499      IF WS-BP-ID-FOUND                                            ELUCSACL
00500          SET CONCLUSIVE TO TRUE                                   ELUCSACL
00501          SET APPLICABLE TO TRUE                                   ELUCSACL
00502          SET SPECIFIC TO TRUE                                     ELUCSACL
00503        ELSE                                                       ELUCSACL
00504          SET NOT-APPLICABLE TO TRUE                               ELUCSACL
00505          SET CONCLUSIVE TO TRUE                                   ELUCSACL
00506        END-IF.                                                    ELUCSACL
00507                                                                   ELUCSACL
00508 ************************************************************      ELUCSACL
00509 *                                                          *      ELUCSACL
00510 *        PROCESS EXCLUDED                                  *      ELUCSACL
00511 *                                                          *      ELUCSACL
00512 ************************************************************      ELUCSACL
00513  PROCESS-EXCLUDED.                                                ELUCSACL
00514      IF WS-BP-ID-FOUND                                            ELUCSACL
00515          SET CONCLUSIVE TO TRUE                                   ELUCSACL
00516          SET NOT-APPLICABLE TO TRUE                               ELUCSACL
00517      ELSE                                                         ELUCSACL
00518          SET APPLICABLE TO TRUE                                   ELUCSACL
00519          SET NOT-CONCLUSIVE TO TRUE                               ELUCSACL
00520      END-IF.                                                      ELUCSACL
00521                                                                   ELUCSACL
00522 ************************************************************      ELUCSACL
00523 *                                                          *      ELUCSACL
00524 *        SEARCH FOR INTERNAL TABULAR SLOT                  *      ELUCSACL
00525 *                                                          *      ELUCSACL
00526 ************************************************************      ELUCSACL
00527  SEARCH-FOR-INTERNAL-TABULAR-SL.                                  ELUCSACL
00528      SET IBGR-X-IDX TO WS-IBGR-X-SUB.                             ELUCSACL
00529      IF IBGR-SLOT-NUMBER (IBGR-X-IDX) =                           ELUCSACL
00530         ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX)                        ELUCSACL
00531            PERFORM INTERNAL-TABULAR-SLOT-FOUND.                   ELUCSACL
00532                                                                   ELUCSACL
00533 ************************************************************      ELUCSACL
00534 *                                                          *      ELUCSACL
00535 *        INTERNAL TABULAR SLOT FOUND                       *      ELUCSACL
00536 *                                                          *      ELUCSACL
00537 ************************************************************      ELUCSACL
00538  INTERNAL-TABULAR-SLOT-FOUND.                                     ELUCSACL
00539      SET IBGR-FOUND TO TRUE.                                      ELUCSACL
00540      SET ADDRESS OF INTERNAL-TABULAR-RECORD                       ELUCSACL
00541          TO IBGR-TABULAR-PTR (IBGR-X-IDX).                        ELUCSACL
00542      PERFORM SEARCH-FOR-BP-ID.                                    ELUCSACL
00543      IF GX1-ID-ARGUMENT-INCLUDED                                  ELUCSACL
00544          PERFORM PROCESS-INCLUDE-TABULAR                          ELUCSACL
00545      ELSE IF GX1-ID-ARGUMENT-EXCLUDED                             ELUCSACL
00546          PERFORM PROCESS-EXCLUDE-TABULAR                          ELUCSACL
00547      ELSE                                                         ELUCSACL
00548          PERFORM SIGNAL-PROGRAM-LOGIC-ERROR.                      ELUCSACL
00549                                                                   ELUCSACL
00550 ************************************************************      ELUCSACL
00551 *                                                          *      ELUCSACL
00552 *        SEARCH FOR BP ID                                  *      ELUCSACL
00553 *                                                          *      ELUCSACL
00554 ************************************************************      ELUCSACL
00555  SEARCH-FOR-BP-ID.                                                ELUCSACL
00556      SET WS-BP-ID-NOT-FOUND TO TRUE.                              ELUCSACL
00557      PERFORM SEARCH-FOR-MATCHING-BP-ID                            ELUCSACL
00558          VARYING WS-GX1-SUB FROM 1 BY 1                           ELUCSACL
00559                    UNTIL WS-GX1-SUB > GX1-ENTRY-COUNT             ELUCSACL
00560                       OR WS-BP-ID-FOUND.                          ELUCSACL
00561                                                                   ELUCSACL
00562 ************************************************************      ELUCSACL
00563 *                                                          *      ELUCSACL
00564 *        SEARCH FOR MATCHING BP ID                         *      ELUCSACL
00565 *                                                          *      ELUCSACL
00566 ************************************************************      ELUCSACL
00567  SEARCH-FOR-MATCHING-BP-ID.                                       ELUCSACL
00568      SET GX1-INDEX TO WS-GX1-SUB.                                 ELUCSACL
00569      IF GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) =                   ELUCSACL
00570         CSBP-BP-KEY (CSBP-X-IDX)                                  ELUCSACL
00571              SET WS-BP-ID-FOUND TO TRUE.                          ELUCSACL
00572                                                                   ELUCSACL
00573 ************************************************************      ELUCSACL
00574 *                                                          *      ELUCSACL
00575 *        PROCESS INCLUDE TABULAR                           *      ELUCSACL
00576 *                                                          *      ELUCSACL
00577 ************************************************************      ELUCSACL
00578  PROCESS-INCLUDE-TABULAR.                                         ELUCSACL
00579      SET TABULAR-INCLUDED TO TRUE.                                ELUCSACL
00580      IF WS-BP-ID-FOUND                                            ELUCSACL
00581         SET WS-BP-QUALIFIED TO TRUE.                              ELUCSACL
00582                                                                   ELUCSACL
00583 ************************************************************      ELUCSACL
00584 *                                                          *      ELUCSACL
00585 *        PROCESS EXCLUDE TABULAR                           *      ELUCSACL
00586 *                                                          *      ELUCSACL
00587 ************************************************************      ELUCSACL
00588  PROCESS-EXCLUDE-TABULAR.                                         ELUCSACL
00589      SET TABULAR-EXCLUDED TO TRUE.                                ELUCSACL
00590      IF WS-BP-ID-NOT-FOUND                                        ELUCSACL
00591         SET WS-BP-QUALIFIED TO TRUE.                              ELUCSACL
00592                                                                   ELUCSACL
00593 ************************************************************      ELUCSACL
00594 *                                                          *      ELUCSACL
00595 *        EXAMINE PLACE OF TREATMENT                        *      ELUCSACL
00596 *                                                          *      ELUCSACL
00597 ************************************************************      ELUCSACL
00598  EXAMINE-PLACE-OF-TREATMENT.                                      ELUCSACL
00599      MOVE SPACE TO IP-OP-SELECT-SWT.                              ELUCSACL
00600      SET NOT-APPLICABLE TO TRUE.                                  ELUCSACL
00601      PERFORM SELECT-IP-OP-IND                                     ELUCSACL
00602              VARYING WS-BPL-X-SUB FROM 1 BY 1                     ELUCSACL
00603                              UNTIL IP-OP-IND-SELECTED.            ELUCSACL
00604      IF IN-PATIENT AND                                            ELUCSACL
00605         ATBL-CF-IP (ATBL-X-IDX) > CF-TWO                          ELUCSACL
00606            SET APPLICABLE TO TRUE.                                ELUCSACL
00607                                                                   ELUCSACL
00608      IF OUT-PATIENT AND                                           ELUCSACL
00609         ATBL-CF-OP (ATBL-X-IDX) > CF-TWO                          ELUCSACL
00610            SET APPLICABLE TO TRUE.                                ELUCSACL
00611                                                                   ELUCSACL
00612      IF BOTH-IP-OP                                                ELUCSACL
00613            SET APPLICABLE TO TRUE.                                ELUCSACL
00614                                                                   ELUCSACL
00615  SELECT-IP-OP-IND.                                                ELUCSACL
00616      SET BPL-IDX TO WS-BPL-X-SUB.                                 ELUCSACL
00617      IF BPL-BP-ID (BPL-IDX) = CSBP-BP-KEY (CSBP-X-IDX)            ELUCSACL
00618           MOVE BPL-IP-OP-IND (BPL-IDX) TO IP-OP-IND               ELUCSACL
00619           SET IP-OP-IND-SELECTED TO TRUE.                         ELUCSACL
00620                                                                   ELUCSACL
00621 ************************************************************      ELUCSACL
00622 *                                                          *      ELUCSACL
00623 *        EXAMINE LINE OF BUSINESS                          *      ELUCSACL
00624 *                                                          *      ELUCSACL
00625 ************************************************************      ELUCSACL
00626  EXAMINE-LINE-OF-BUSINESS.                                        ELUCSACL
00627      SET NOT-APPLICABLE TO TRUE.                                  ELUCSACL
00628      MOVE CSBP-BP-ID-FORMAT (CSBP-X-IDX) TO WS-BP-ID-FORMAT.      ELUCSACL
00629      IF INSTITUTIONAL AND                                         ELUCSACL
00630         ATBL-CF-INST (ATBL-X-IDX) > CF-FIVE                       ELUCSACL
00631            SET APPLICABLE TO TRUE                                 ELUCSACL
00632        ELSE                                                       ELUCSACL
00633        IF PROFESSIONAL AND                                        ELUCSACL
00634           ATBL-CF-PROF (ATBL-X-IDX) > CF-FIVE                     ELUCSACL
00635             SET APPLICABLE TO TRUE                                ELUCSACL
00636       END-IF.                                                     ELUCSACL
00637                                                                   ELUCSACL
00638 ************************************************************      ELUCSACL
00639 *                                                          *      ELUCSACL
00640 *        CLEAR CSBP ACL DATA                               *      ELUCSACL
00641 *                                                          *      ELUCSACL
00642 ************************************************************      ELUCSACL
00643  CLEAR-CSBP-ACL-DATA.                                             ELUCSACL
00644      MOVE 999 TO PRIORITY-LVL.                                    ELUCSACL
00645      MOVE SPACES TO CSBP-COINS-OVERALL-SW (CSBP-X-IDX),           ELUCSACL
00646                     CSBP-COINS-BENEFIT-PERIOD (CSBP-X-IDX),       ELUCSACL
00647                     CSBP-COINS-L-O-B (CSBP-X-IDX),                ELUCSACL
00648                     CSBP-COINS-PLACE-OF-TREATMENT (CSBP-X-IDX),   ELUCSACL
00649                     CSBP-COINS-VALUE-QUALIFIER (CSBP-X-IDX),      ELUCSACL
00650                     CSBP-COINS-VALUE-QUALIFIER (CSBP-X-IDX).      ELUCSACL
00651      MOVE ZEROES TO CSBP-COINS-IBGR-SLOT (CSBP-X-IDX),            ELUCSACL
00652                     CSBP-COINS-VALUE-LIMIT (CSBP-X-IDX),          ELUCSACL
00653                     CSBP-COINS-AD-SUB (CSBP-X-IDX).               ELUCSACL
00654      SET SIMPLE-ACL TO TRUE.                                      ELUCSACL
00655                                                                   ELUCSACL
00656 ************************************************************      ELUCSACL
00657 *                                                          *      ELUCSACL
00658 *        EXAMINE CONDITION BITS                            *      ELUCSACL
00659 *                                                          *      ELUCSACL
00660 ************************************************************      ELUCSACL
00661  EXAMINE-CONDITION-BITS.                                          ELUCSACL
00662      SET NOT-APPLICABLE TO TRUE.                                  ELUCSACL
00663      IF CSBP-PSYCHIATRIC-ST                                       ELUCSACL
00664          PERFORM ALL-OR-ICD-BITS-QUERY                            ELUCSACL
00665      ELSE IF CSBP-OB-STERILIZE-ST                                 ELUCSACL
00666          PERFORM PROCESS-OB-PROVN                                 ELUCSACL
00667      ELSE IF NOT CSBP-PSYCHIATRIC-ST OR                           ELUCSACL
00668                 NOT CSBP-OB-STERILIZE-ST                          ELUCSACL
00669          PERFORM DEFAULT-ALL-OR-ICD-BIT-QUERY.                    ELUCSACL
00670                                                                   ELUCSACL
00671 ************************************************************      ELUCSACL
00672 *                                                          *      ELUCSACL
00673 *        ALL OR ICD BITS QUERY                             *      ELUCSACL
00674 *                                                          *      ELUCSACL
00675 ************************************************************      ELUCSACL
00676  ALL-OR-ICD-BITS-QUERY.                                           ELUCSACL
00677      IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = PC-ONE OR                ELUCSACL
00678                 ATBL-COND-ICD-BIT (ATBL-X-IDX) = PC-ONE           ELUCSACL
00679          PERFORM ALL-OR-ICD-BIT-ON-QUERY-EXCLUS                   ELUCSACL
00680      ELSE                                                         ELUCSACL
00681          PERFORM ALL-AND-ICD-BIT-OFF-QUERY-MENT.                  ELUCSACL
00682                                                                   ELUCSACL
00683 ************************************************************      ELUCSACL
00684 *                                                          *      ELUCSACL
00685 *        PROCESS OB PROVN                                  *      ELUCSACL
00686 *                                                          *      ELUCSACL
00687 ************************************************************      ELUCSACL
00688  PROCESS-OB-PROVN.                                                ELUCSACL
00689      SET NOT-APPLICABLE TO TRUE.                                  ELUCSACL
00690      MOVE CSBP-BP-KEY (CSBP-X-IDX) TO WS-OBSTETRICS.              ELUCSACL
00691      IF OB-NORMAL                                                 ELUCSACL
00692         PERFORM PROCESS-OB-NORM                                   ELUCSACL
00693      ELSE                                                         ELUCSACL
00694         IF OB-COMPLICATED                                         ELUCSACL
00695            PERFORM PROCESS-OB-COMPL                               ELUCSACL
00696         ELSE                                                      ELUCSACL
00697            PERFORM DEFAULT-ALL-OR-ICD-BIT-QUERY                   ELUCSACL
00698         END-IF                                                    ELUCSACL
00699      END-IF.                                                      ELUCSACL
00700                                                                   ELUCSACL
00701 ************************************************************      ELUCSACL
00702 *                                                          *      ELUCSACL
00703 *        DEFAULT ALL OR ICD BIT QUERY                      *      ELUCSACL
00704 *                                                          *      ELUCSACL
00705 ************************************************************      ELUCSACL
00706  DEFAULT-ALL-OR-ICD-BIT-QUERY.                                    ELUCSACL
00707      IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = PC-ONE OR                ELUCSACL
00708         ATBL-COND-ICD-BIT (ATBL-X-IDX) = PC-ONE                   ELUCSACL
00709         SET APPLICABLE TO TRUE.                                   ELUCSACL
00710                                                                   ELUCSACL
00711 ************************************************************      ELUCSACL
00712 *                                                          *      ELUCSACL
00713 *        ALL OR ICD BIT ON QUERY EXCLUSION BIT             *      ELUCSACL
00714 *                                                          *      ELUCSACL
00715 ************************************************************      ELUCSACL
00716  ALL-OR-ICD-BIT-ON-QUERY-EXCLUS.                                  ELUCSACL
00717      IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = PC-ONE             ELUCSACL
00718         IF ATBL-COND-MENTAL-BIT (ATBL-X-IDX) = ZERO               ELUCSACL
00719            SET APPLICABLE TO TRUE                                 ELUCSACL
00720         ELSE                                                      ELUCSACL
00721            CONTINUE                                               ELUCSACL
00722      ELSE                                                         ELUCSACL
00723          SET APPLICABLE TO TRUE                                   ELUCSACL
00724      END-IF.                                                      ELUCSACL
00725                                                                   ELUCSACL
00726 ************************************************************      ELUCSACL
00727 *                                                          *      ELUCSACL
00728 *        ALL AND ICD BIT OFF QUERY MENTAL BIT              *      ELUCSACL
00729 *                                                          *      ELUCSACL
00730 ************************************************************      ELUCSACL
00731  ALL-AND-ICD-BIT-OFF-QUERY-MENT.                                  ELUCSACL
00732      IF ATBL-COND-MENTAL-BIT (ATBL-X-IDX) = PC-ONE                ELUCSACL
00733         SET SPECIFIC TO TRUE                                      ELUCSACL
00734         SET APPLICABLE TO TRUE.                                   ELUCSACL
00735                                                                   ELUCSACL
00736 ************************************************************      ELUCSACL
00737 *                                                          *      ELUCSACL
00738 *        PROCESS OB NORM                                   *      ELUCSACL
00739 *                                                          *      ELUCSACL
00740 ************************************************************      ELUCSACL
00741  PROCESS-OB-NORM.                                                 ELUCSACL
00742      IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = PC-ONE OR                ELUCSACL
00743                  ATBL-COND-ICD-BIT (ATBL-X-IDX) = PC-ONE          ELUCSACL
00744          PERFORM PROCESS-OB-NORM-COND-BIT-ON                      ELUCSACL
00745      ELSE IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = ZERO AND            ELUCSACL
00746                  ATBL-COND-ICD-BIT (ATBL-X-IDX) = ZERO            ELUCSACL
00747          PERFORM PROCESS-OB-NORM-COND-BITS-OFF.                   ELUCSACL
00748                                                                   ELUCSACL
00749 ************************************************************      ELUCSACL
00750 *                                                          *      ELUCSACL
00751 *        PROCESS OB COMPL                                  *      ELUCSACL
00752 *                                                          *      ELUCSACL
00753 ************************************************************      ELUCSACL
00754  PROCESS-OB-COMPL.                                                ELUCSACL
00755      IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = PC-ONE OR                ELUCSACL
00756                  ATBL-COND-ICD-BIT (ATBL-X-IDX) = PC-ONE          ELUCSACL
00757          PERFORM PROCESS-OB-COMPL-COND-BIT-ON                     ELUCSACL
00758      ELSE IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = ZERO AND            ELUCSACL
00759                  ATBL-COND-ICD-BIT (ATBL-X-IDX) = ZERO            ELUCSACL
00760          PERFORM PROCESS-OB-COMPL-COND-BITS-OFF.                  ELUCSACL
00761                                                                   ELUCSACL
00762 ************************************************************      ELUCSACL
00763 *                                                          *      ELUCSACL
00764 *        PROCESS OB NORM COND BIT ON                       *      ELUCSACL
00765 *                                                          *      ELUCSACL
00766 ************************************************************      ELUCSACL
00767  PROCESS-OB-NORM-COND-BIT-ON.                                     ELUCSACL
00768      IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = PC-ONE AND         ELUCSACL
00769         ATBL-COND-OB-NORM-BIT (ATBL-X-IDX) = ZERO                 ELUCSACL
00770           SET APPLICABLE TO TRUE                                  ELUCSACL
00771        ELSE                                                       ELUCSACL
00772        IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = ZERO             ELUCSACL
00773           SET APPLICABLE TO TRUE                                  ELUCSACL
00774        END-IF.                                                    ELUCSACL
00775                                                                   ELUCSACL
00776 ************************************************************      ELUCSACL
00777 *                                                          *      ELUCSACL
00778 *        PROCESS OB NORM COND BITS OFF                     *      ELUCSACL
00779 *                                                          *      ELUCSACL
00780 ************************************************************      ELUCSACL
00781  PROCESS-OB-NORM-COND-BITS-OFF.                                   ELUCSACL
00782      IF ATBL-COND-OB-NORM-BIT (ATBL-X-IDX) = PC-ONE               ELUCSACL
00783         SET SPECIFIC TO TRUE                                      ELUCSACL
00784         SET APPLICABLE TO TRUE.                                   ELUCSACL
00785                                                                   ELUCSACL
00786 ************************************************************      ELUCSACL
00787 *                                                          *      ELUCSACL
00788 *        PROCESS OB COMPL COND BIT ON                      *      ELUCSACL
00789 *                                                          *      ELUCSACL
00790 ************************************************************      ELUCSACL
00791  PROCESS-OB-COMPL-COND-BIT-ON.                                    ELUCSACL
00792      IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = PC-ONE AND         ELUCSACL
00793         ATBL-COND-OB-COMP-BIT (ATBL-X-IDX) = ZERO                 ELUCSACL
00794           SET APPLICABLE TO TRUE                                  ELUCSACL
00795        ELSE                                                       ELUCSACL
00796        IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = ZERO             ELUCSACL
00797           SET APPLICABLE TO TRUE                                  ELUCSACL
00798        END-IF.                                                    ELUCSACL
00799                                                                   ELUCSACL
00800 ************************************************************      ELUCSACL
00801 *                                                          *      ELUCSACL
00802 *        PROCESS OB COMPL COND BITS OFF                    *      ELUCSACL
00803 *                                                          *      ELUCSACL
00804 ************************************************************      ELUCSACL
00805  PROCESS-OB-COMPL-COND-BITS-OFF.                                  ELUCSACL
00806      IF ATBL-COND-OB-COMP-BIT (ATBL-X-IDX) = PC-ONE               ELUCSACL
00807         SET SPECIFIC TO TRUE                                      ELUCSACL
00808         SET APPLICABLE TO TRUE.                                   ELUCSACL
00809                                                                   ELUCSACL
00810 ************************************************************      ELUCSACL
00811 *                                                          *      ELUCSACL
00812 *        EXAMINE CONFIDENCE FACTORS                        *      ELUCSACL
00813 *                                                          *      ELUCSACL
00814 ************************************************************      ELUCSACL
00815  EXAMINE-CONFIDENCE-FACTORS.                                      ELUCSACL
00816      SET NOT-APPLICABLE TO TRUE.                                  ELUCSACL
00817      MOVE CSBP-BP-ID-FORMAT (CSBP-X-IDX) TO WS-BP-ID-FORMAT.      ELUCSACL
00818      IF INSTITUTIONAL AND                                         ELUCSACL
00819         ATBL-CF-INST (ATBL-X-IDX) > CF-TWO                        ELUCSACL
00820           SET APPLICABLE TO TRUE                                  ELUCSACL
00821        ELSE                                                       ELUCSACL
00822        IF PROFESSIONAL AND                                        ELUCSACL
00823           ATBL-CF-PROF (ATBL-X-IDX) > CF-TWO                      ELUCSACL
00824              SET APPLICABLE TO TRUE                               ELUCSACL
00825        END-IF.                                                    ELUCSACL
00826      IF APPLICABLE                                                ELUCSACL
00827          PERFORM EXAMINE-IP-OP-CONFIDENCE-FACTO.                  ELUCSACL
00828                                                                   ELUCSACL
00829 ************************************************************      ELUCSACL
00830 *                                                          *      ELUCSACL
00831 *        EXAMINE IP OP CONFIDENCE FACTORS                  *      ELUCSACL
00832 *                                                          *      ELUCSACL
00833 ************************************************************      ELUCSACL
00834  EXAMINE-IP-OP-CONFIDENCE-FACTO.                                  ELUCSACL
00835      SET NOT-APPLICABLE TO TRUE.                                  ELUCSACL
00836      IF CSBP-INPATIENT (CSBP-X-IDX) AND                           ELUCSACL
00837         ATBL-CF-IP (ATBL-X-IDX) > CF-TWO                          ELUCSACL
00838           SET APPLICABLE TO TRUE.                                 ELUCSACL
00839      IF CSBP-OUTPATIENT (CSBP-X-IDX) AND                          ELUCSACL
00840         ATBL-CF-OP (ATBL-X-IDX) > CF-TWO                          ELUCSACL
00841           SET APPLICABLE TO TRUE.                                 ELUCSACL
00842      IF CSBP-BOTH (CSBP-X-IDX)                                    ELUCSACL
00843         SET APPLICABLE TO TRUE.                                   ELUCSACL
00844                                                                   ELUCSACL
00845 ************************************************************      ELUCSACL
00846 *                                                          *      ELUCSACL
00847 *        SELECT BENEFIT PERIOD PRIORITY                    *      ELUCSACL
00848 *                                                          *      ELUCSACL
00849 ************************************************************      ELUCSACL
00850  SELECT-BENEFIT-PERIOD-PRIORITY.                                  ELUCSACL
00851      IF CSBP-L-O-B (CSBP-X-IDX) = '1'                             ELUCSACL
00852          PERFORM SELECT-BLUE-CROSS-PRIORITY-TAB                   ELUCSACL
00853      ELSE IF CSBP-L-O-B (CSBP-X-IDX) = '2'                        ELUCSACL
00854          PERFORM SELECT-BLUE-SHIELD-PRIORITY-TA                   ELUCSACL
00855      ELSE IF CSBP-L-O-B (CSBP-X-IDX) = '3'                        ELUCSACL
00856          PERFORM SEARCH-TABLE-FIVE                                ELUCSACL
00857      ELSE IF CSBP-L-O-B (CSBP-X-IDX) = '4'                        ELUCSACL
00858          PERFORM SELECT-COMP-MAJ-MED-PRIORITY                     ELUCSACL
00859      END-IF.                                                      ELUCSACL
00860                                                                   ELUCSACL
00861      IF NOT-SPECIFIC                                              ELUCSACL
00862         COMPUTE IN-PRIORITY-LVL = IN-PRIORITY-LVL + 100.          ELUCSACL
00863                                                                   ELUCSACL
00864      IF IN-PRIORITY-LVL < PRIORITY-LVL                            ELUCSACL
00865         PERFORM CAPTURE-ACL-INFO                                  ELUCSACL
00866      ELSE                                                         ELUCSACL
00867         IF IN-PRIORITY-LVL = PRIORITY-LVL                         ELUCSACL
00868            AND VARYING-ACL                                        ELUCSACL
00869                AND ATBL-ASCEND-DESCEND-IND (ATBL-X-IDX)           ELUCSACL
00870                 =  CSAD-ACL-KEY-A-D-IND (CSAD-ACL-IDX)            ELUCSACL
00871                    PERFORM CAPTURE-NEXT-VARYING-ACL-INFO          ELUCSACL
00872         ELSE                                                      ELUCSACL
00873                    CONTINUE.                                      ELUCSACL
00874                                                                   ELUCSACL
00875      MOVE SPACE TO SEARCH-SWITCH.                                 ELUCSACL
00876                                                                   ELUCSACL
00877 ************************************************************      ELUCSACL
00878 *                                                          *      ELUCSACL
00879 *        SELECT BLUE CROSS PRIORITY TABLE                  *      ELUCSACL
00880 *                                                          *      ELUCSACL
00881 ************************************************************      ELUCSACL
00882  SELECT-BLUE-CROSS-PRIORITY-TAB.                                  ELUCSACL
00883      IF CSBP-INPATIENT (CSBP-X-IDX)                               ELUCSACL
00884          PERFORM SEARCH-TABLE-ONE                                 ELUCSACL
00885      ELSE IF CSBP-OUTPATIENT (CSBP-X-IDX)                         ELUCSACL
00886          PERFORM SEARCH-TABLE-TWO                                 ELUCSACL
00887      ELSE IF CSBP-BOTH (CSBP-X-IDX)                               ELUCSACL
00888          PERFORM SEARCH-TABLE-THREE.                              ELUCSACL
00889                                                                   ELUCSACL
00890 ************************************************************      ELUCSACL
00891 *                                                          *      ELUCSACL
00892 *        SELECT BLUE SHIELD PRIORITY TABLE                 *      ELUCSACL
00893 *                                                          *      ELUCSACL
00894 ************************************************************      ELUCSACL
00895  SELECT-BLUE-SHIELD-PRIORITY-TA.                                  ELUCSACL
00896      IF CSBP-INPATIENT (CSBP-X-IDX)                               ELUCSACL
00897          PERFORM SEARCH-TABLE-THREE                               ELUCSACL
00898      ELSE IF CSBP-OUTPATIENT (CSBP-X-IDX)                         ELUCSACL
00899          PERFORM SEARCH-TABLE-FOUR                                ELUCSACL
00900      ELSE IF CSBP-BOTH (CSBP-X-IDX)                               ELUCSACL
00901          PERFORM SEARCH-TABLE-THREE.                              ELUCSACL
00902                                                                   ELUCSACL
00903 ************************************************************      ELUCSACL
00904 *                                                          *      ELUCSACL
00905 *        SELECT COMP MAJ MED PRIORITY                      *      ELUCSACL
00906 *                                                          *      ELUCSACL
00907 ************************************************************      ELUCSACL
00908  SELECT-COMP-MAJ-MED-PRIORITY.                                    ELUCSACL
00909      MOVE CSBP-BP-ID-FORMAT (CSBP-X-IDX) TO WS-BP-ID-FORMAT.      ELUCSACL
00910      IF INSTITUTIONAL                                             ELUCSACL
00911         PERFORM SELECT-BLUE-CROSS-PRIORITY-TAB                    ELUCSACL
00912      ELSE                                                         ELUCSACL
00913      IF PROFESSIONAL                                              ELUCSACL
00914         PERFORM SELECT-BLUE-SHIELD-PRIORITY-TA.                   ELUCSACL
00915                                                                   ELUCSACL
00916 ************************************************************      ELUCSACL
00917 *                                                          *      ELUCSACL
00918 *        SEARCH TABLE ONE                                  *      ELUCSACL
00919 *                                                          *      ELUCSACL
00920 ************************************************************      ELUCSACL
00921  SEARCH-TABLE-ONE.                                                ELUCSACL
00922      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL1                    ELUCSACL
00923           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSACL
00924      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSACL
00925          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSACL
00926                          SEARCH-IS-COMPLETE.                      ELUCSACL
00927                                                                   ELUCSACL
00928 ************************************************************      ELUCSACL
00929 *                                                          *      ELUCSACL
00930 *        SEARCH TABLE TWO                                  *      ELUCSACL
00931 *                                                          *      ELUCSACL
00932 ************************************************************      ELUCSACL
00933  SEARCH-TABLE-TWO.                                                ELUCSACL
00934      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL2                    ELUCSACL
00935           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSACL
00936      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSACL
00937          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSACL
00938                          SEARCH-IS-COMPLETE.                      ELUCSACL
00939                                                                   ELUCSACL
00940 ************************************************************      ELUCSACL
00941 *                                                          *      ELUCSACL
00942 *        SEARCH TABLE THREE                                *      ELUCSACL
00943 *                                                          *      ELUCSACL
00944 ************************************************************      ELUCSACL
00945  SEARCH-TABLE-THREE.                                              ELUCSACL
00946      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL3                    ELUCSACL
00947           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSACL
00948      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSACL
00949          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSACL
00950                          SEARCH-IS-COMPLETE.                      ELUCSACL
00951                                                                   ELUCSACL
00952 ************************************************************      ELUCSACL
00953 *                                                          *      ELUCSACL
00954 *        SEARCH TABLE FOUR                                 *      ELUCSACL
00955 *                                                          *      ELUCSACL
00956 ************************************************************      ELUCSACL
00957  SEARCH-TABLE-FOUR.                                               ELUCSACL
00958      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL4                    ELUCSACL
00959           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSACL
00960      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSACL
00961          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSACL
00962                          SEARCH-IS-COMPLETE.                      ELUCSACL
00963                                                                   ELUCSACL
00964 ************************************************************      ELUCSACL
00965 *                                                          *      ELUCSACL
00966 *        SEARCH TABLE FIVE                                 *      ELUCSACL
00967 *                                                          *      ELUCSACL
00968 ************************************************************      ELUCSACL
00969  SEARCH-TABLE-FIVE.                                               ELUCSACL
00970      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL5                    ELUCSACL
00971           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSACL
00972      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSACL
00973          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSACL
00974                          SEARCH-IS-COMPLETE.                      ELUCSACL
00975                                                                   ELUCSACL
00976 ************************************************************      ELUCSACL
00977 *                                                          *      ELUCSACL
00978 *        SELECT BENEFIT PERIOD                             *      ELUCSACL
00979 *                                                          *      ELUCSACL
00980 ************************************************************      ELUCSACL
00981  SELECT-BENEFIT-PERIOD.                                           ELUCSACL
00982      IF ATBL-BENEFIT-PERIOD (ATBL-X-IDX) =                        ELUCSACL
00983                 BENEFIT-PERIOD (WS-BP-SUB)                        ELUCSACL
00984          PERFORM SET-SEARCH-COMPLETE-SWITCH.                      ELUCSACL
00985      IF WS-BP-SUB > 999                                           ELUCSACL
00986          PERFORM SET-SUBSCRIPT-TO-999.                            ELUCSACL
00987                                                                   ELUCSACL
00988 ************************************************************      ELUCSACL
00989 *                                                          *      ELUCSACL
00990 *        SET SEARCH COMPLETE SWITCH                        *      ELUCSACL
00991 *                                                          *      ELUCSACL
00992 ************************************************************      ELUCSACL
00993  SET-SEARCH-COMPLETE-SWITCH.                                      ELUCSACL
00994      SET SEARCH-IS-COMPLETE TO TRUE.                              ELUCSACL
00995      MOVE WS-BP-SUB TO IN-PRIORITY-LVL.                           ELUCSACL
00996                                                                   ELUCSACL
00997 ************************************************************      ELUCSACL
00998 *                                                          *      ELUCSACL
00999 *        SET SUBSCRIPT TO 999                              *      ELUCSACL
01000 *                                                          *      ELUCSACL
01001 ************************************************************      ELUCSACL
01002  SET-SUBSCRIPT-TO-999.                                            ELUCSACL
01003      MOVE 999 TO WS-BP-SUB.                                       ELUCSACL
01004      PERFORM SET-SEARCH-COMPLETE-SWITCH.                          ELUCSACL
01005                                                                   ELUCSACL
01006 ************************************************************      ELUCSACL
01007 *                                                          *      ELUCSACL
01008 *        CAPTURE ACL INFO                                  *      ELUCSACL
01009 *                                                          *      ELUCSACL
01010 ************************************************************      ELUCSACL
01011  CAPTURE-ACL-INFO.                                                ELUCSACL
01012      IF ATBL-ASCEND-DESCEND-IND (ATBL-X-IDX) = 0                  ELUCSACL
01013         PERFORM CAPTURE-SIMPLE-ACL-INFO                           ELUCSACL
01014      ELSE                                                         ELUCSACL
01015         PERFORM CAPTURE-NEW-VARYING-ACL-INFO                      ELUCSACL
01016      END-IF.                                                      ELUCSACL
01017      MOVE IN-PRIORITY-LVL TO PRIORITY-LVL.                        ELUCSACL
01018                                                                   ELUCSACL
01019 ************************************************************      ELUCSACL
01020 *                                                          *      ELUCSACL
01021 *        CAPTURE SIMPLE ACL INFO                           *      ELUCSACL
01022 *                                                          *      ELUCSACL
01023 ************************************************************      ELUCSACL
01024  CAPTURE-SIMPLE-ACL-INFO.                                         ELUCSACL
01025      IF CSBP-COINS-BENEFIT-PERIOD (CSBP-X-IDX) NOT = SPACES       ELUCSACL
01026         SET CSBP-ADDITIONAL-ACL-TEXT (CSBP-X-IDX) TO TRUE.        ELUCSACL
01027      SET SIMPLE-ACL TO TRUE.                                      ELUCSACL
01028      MOVE 'N' TO CSBP-COINS-OVERALL-SW (CSBP-X-IDX).              ELUCSACL
01029      PERFORM CAPTURE-BASIC-INFO.                                  ELUCSACL
01030      MOVE ATBL-VALUE-LIMIT             (ATBL-X-IDX)               ELUCSACL
01031        TO CSBP-COINS-VALUE-LIMIT       (CSBP-X-IDX).              ELUCSACL
01032      MOVE ATBL-VALUE-QUALIFIER         (ATBL-X-IDX)               ELUCSACL
01033        TO CSBP-COINS-VALUE-QUALIFIER   (CSBP-X-IDX).              ELUCSACL
01034      MOVE ATBL-PERCENT-LEVEL           (ATBL-X-IDX)               ELUCSACL
01035        TO CSBP-COINS-PERCENT-LEVEL     (CSBP-X-IDX).              ELUCSACL
01036                                                                   ELUCSACL
01037 ************************************************************      ELUCSACL
01038 *                                                          *      ELUCSACL
01039 *        CAPTURE NEW VARYING ACL INFO                      *      ELUCSACL
01040 *                                                          *      ELUCSACL
01041 ************************************************************      ELUCSACL
01042  CAPTURE-NEW-VARYING-ACL-INFO.                                    ELUCSACL
01043      PERFORM CAPTURE-BASIC-INFO.                                  ELUCSACL
01044      IF VARYING-ACL                                               ELUCSACL
01045         CONTINUE                                                  ELUCSACL
01046      ELSE                                                         ELUCSACL
01047         ADD 1 TO CSAD-ACL-NBR-ENTRIES                             ELUCSACL
01048         SET CSAD-ACL-IDX TO CSAD-ACL-NBR-ENTRIES                  ELUCSACL
01049         SET VARYING-ACL TO TRUE                                   ELUCSACL
01050         MOVE '1' TO CSAD-ENTRIES-USED (CSAD-ACL-IDX)              ELUCSACL
01051      END-IF.                                                      ELUCSACL
01052                                                                   ELUCSACL
01053      MOVE ATBL-ASCEND-DESCEND-IND      (ATBL-X-IDX)               ELUCSACL
01054        TO CSAD-ACL-KEY-A-D-IND         (CSAD-ACL-IDX).            ELUCSACL
01055      MOVE ATBL-BENEFIT-PERIOD          (ATBL-X-IDX)               ELUCSACL
01056        TO CSAD-ACL-KEY-TYPE            (CSAD-ACL-IDX).            ELUCSACL
01057      MOVE ATBL-MANDATORY-IND           (ATBL-X-IDX)               ELUCSACL
01058        TO CSAD-ACL-KEY-MAN-IND         (CSAD-ACL-IDX).            ELUCSACL
01059      MOVE ATBL-FAM-OR-INDIV            (ATBL-X-IDX)               ELUCSACL
01060        TO CSAD-ACL-KEY-F-R-I           (CSAD-ACL-IDX).            ELUCSACL
01061      MOVE ATBL-L-O-B                   (ATBL-X-IDX)               ELUCSACL
01062        TO CSAD-ACL-KEY-L-O-B           (CSAD-ACL-IDX).            ELUCSACL
01063      MOVE ATBL-INTERNAL-DESCRIPTOR     (ATBL-X-IDX)               ELUCSACL
01064        TO CSAD-ACL-KEY-INT-DES         (CSAD-ACL-IDX).            ELUCSACL
01065      MOVE ATBL-SERVICE-GROUP           (ATBL-X-IDX)               ELUCSACL
01066        TO CSAD-ACL-KEY-SER-GRP         (CSAD-ACL-IDX).            ELUCSACL
01067      MOVE ATBL-PLACE-OF-TREATMENT      (ATBL-X-IDX)               ELUCSACL
01068        TO CSAD-ACL-KEY-TRT-GRP         (CSAD-ACL-IDX).            ELUCSACL
01069      MOVE ATBL-CONDITION               (ATBL-X-IDX)               ELUCSACL
01070        TO CSAD-ACL-KEY-COND            (CSAD-ACL-IDX).            ELUCSACL
01071      MOVE ATBL-COST-CONTAIN-IND        (ATBL-X-IDX)               ELUCSACL
01072        TO CSAD-ACL-KEY-COST            (CSAD-ACL-IDX).            ELUCSACL
01073      MOVE ATBL-CO-PAY-IND              (ATBL-X-IDX)               ELUCSACL
01074        TO CSAD-ACL-KEY-COPAY           (CSAD-ACL-IDX).            ELUCSACL
01075                                                                   ELUCSACL
01076      MOVE ATBL-BISCEND-IND             (ATBL-X-IDX)               ELUCSACL
01077        TO CSAD-ACL-BISCEND-IND (CSAD-ACL-IDX, 1).                 ELUCSACL
01078      MOVE ATBL-PERCENT-LEVEL           (ATBL-X-IDX)               ELUCSACL
01079        TO CSAD-ACL-PCT-LVL     (CSAD-ACL-IDX, 1).                 ELUCSACL
01080      MOVE ATBL-VALUE-LIMIT             (ATBL-X-IDX)               ELUCSACL
01081        TO CSAD-ACL-VAL-LMT     (CSAD-ACL-IDX, 1).                 ELUCSACL
01082      MOVE ATBL-VALUE-QUALIFIER         (ATBL-X-IDX)               ELUCSACL
01083        TO CSBP-COINS-VALUE-QUALIFIER   (CSBP-X-IDX).              ELUCSACL
01084                                                                   ELUCSACL
01085 ************************************************************      ELUCSACL
01086 *                                                          *      ELUCSACL
01087 *        CAPTURE NEXT VARYING ACL INFO                     *      ELUCSACL
01088 *                                                          *      ELUCSACL
01089 ************************************************************      ELUCSACL
01090  CAPTURE-NEXT-VARYING-ACL-INFO.                                   ELUCSACL
01091      PERFORM BUILD-WS-ACL-KEY.                                    ELUCSACL
01092      IF WS-ACL-KEY = CSAD-ACL-KEY-FIELDS (CSAD-ACL-IDX)           ELUCSACL
01093         PERFORM ADD-NEW-ENTRY-TO-CSAD-ENTRY.                      ELUCSACL
01094                                                                   ELUCSACL
01095 ************************************************************      ELUCSACL
01096 *                                                          *      ELUCSACL
01097 *        ADD NEW ENTRY TO CSAD ENTRY                       *      ELUCSACL
01098 *                                                          *      ELUCSACL
01099 ************************************************************      ELUCSACL
01100  ADD-NEW-ENTRY-TO-CSAD-ENTRY.                                     ELUCSACL
01101      SET CSAD-ENTRY-IDX TO CSAD-ENTRIES-USED (CSAD-ACL-IDX).      ELUCSACL
01102      SET CSAD-NEW-IDX   TO CSAD-ENTRY-IDX.                        ELUCSACL
01103      SET CSAD-NEW-IDX   UP BY 1.                                  ELUCSACL
01104      ADD 1 TO CSAD-ENTRIES-USED (CSAD-ACL-IDX).                   ELUCSACL
01105      MOVE ATBL-BISCEND-IND   (ATBL-X-IDX)                         ELUCSACL
01106        TO CSAD-ACL-BISCEND-IND (CSAD-ACL-IDX, CSAD-NEW-IDX).      ELUCSACL
01107      MOVE ATBL-PERCENT-LEVEL (ATBL-X-IDX)                         ELUCSACL
01108        TO CSAD-ACL-PCT-LVL     (CSAD-ACL-IDX, CSAD-NEW-IDX).      ELUCSACL
01109      MOVE ATBL-VALUE-LIMIT   (ATBL-X-IDX)                         ELUCSACL
01110        TO CSAD-ACL-VAL-LMT     (CSAD-ACL-IDX, CSAD-NEW-IDX).      ELUCSACL
01111                                                                   ELUCSACL
01112      EVALUATE CSAD-ACL-KEY-A-D-IND (CSAD-ACL-IDX)                 ELUCSACL
01113        WHEN 1 PERFORM SORT-ASCEND,                                ELUCSACL
01114        WHEN 2 PERFORM SORT-DESCEND,                               ELUCSACL
01115        WHEN 3 PERFORM SORT-BISCEND,                               ELUCSACL
01116        WHEN OTHER CONTINUE,                                       ELUCSACL
01117      END-EVALUATE.                                                ELUCSACL
01118                                                                   ELUCSACL
01119 ************************************************************      ELUCSACL
01120 *                                                          *      ELUCSACL
01121 *        SORT ASCEND                                       *      ELUCSACL
01122 *                                                          *      ELUCSACL
01123 ************************************************************      ELUCSACL
01124  SORT-ASCEND.                                                     ELUCSACL
01125      PERFORM SWAP-ENTRIES                                         ELUCSACL
01126         UNTIL CSAD-NEW-IDX = 1   OR                               ELUCSACL
01127         CSAD-ACL-PCT-LVL (CSAD-ACL-IDX, CSAD-ENTRY-IDX) <         ELUCSACL
01128         CSAD-ACL-PCT-LVL (CSAD-ACL-IDX, CSAD-NEW-IDX).            ELUCSACL
01129                                                                   ELUCSACL
01130 ************************************************************      ELUCSACL
01131 *                                                          *      ELUCSACL
01132 *        SORT DESCEND                                      *      ELUCSACL
01133 *                                                          *      ELUCSACL
01134 ************************************************************      ELUCSACL
01135  SORT-DESCEND.                                                    ELUCSACL
01136      PERFORM SWAP-ENTRIES                                         ELUCSACL
01137         UNTIL CSAD-NEW-IDX = 1   OR                               ELUCSACL
01138         CSAD-ACL-PCT-LVL (CSAD-ACL-IDX, CSAD-ENTRY-IDX) >         ELUCSACL
01139         CSAD-ACL-PCT-LVL (CSAD-ACL-IDX, CSAD-NEW-IDX).            ELUCSACL
01140                                                                   ELUCSACL
01141 ************************************************************      ELUCSACL
01142 *                                                          *      ELUCSACL
01143 *        SORT BISCEND                                      *      ELUCSACL
01144 *                                                          *      ELUCSACL
01145 ************************************************************      ELUCSACL
01146  SORT-BISCEND.                                                    ELUCSACL
01147      PERFORM SWAP-ENTRIES                                         ELUCSACL
01148         UNTIL CSAD-NEW-IDX = 1   OR                               ELUCSACL
01149         CSAD-ACL-BISCEND-IND (CSAD-ACL-IDX, CSAD-ENTRY-IDX) <     ELUCSACL
01150         CSAD-ACL-BISCEND-IND (CSAD-ACL-IDX, CSAD-NEW-IDX).        ELUCSACL
01151                                                                   ELUCSACL
01152 ************************************************************      ELUCSACL
01153 *                                                          *      ELUCSACL
01154 *        SWAP ENTRIES                                      *      ELUCSACL
01155 *                                                          *      ELUCSACL
01156 ************************************************************      ELUCSACL
01157  SWAP-ENTRIES.                                                    ELUCSACL
01158      MOVE CSAD-TBL-ENTRIES (CSAD-ACL-IDX, CSAD-ENTRY-IDX)         ELUCSACL
01159      TO   WS-ACL-ENTRIES.                                         ELUCSACL
01160      MOVE CSAD-TBL-ENTRIES (CSAD-ACL-IDX, CSAD-NEW-IDX)           ELUCSACL
01161      TO   CSAD-TBL-ENTRIES (CSAD-ACL-IDX, CSAD-ENTRY-IDX).        ELUCSACL
01162      MOVE WS-ACL-ENTRIES                                          ELUCSACL
01163      TO   CSAD-TBL-ENTRIES (CSAD-ACL-IDX, CSAD-NEW-IDX).          ELUCSACL
01164      SET CSAD-ENTRY-IDX DOWN BY 1.                                ELUCSACL
01165      SET CSAD-NEW-IDX   DOWN BY 1.                                ELUCSACL
01166                                                                   ELUCSACL
01167 ************************************************************      ELUCSACL
01168 *                                                          *      ELUCSACL
01169 *        BUILD WS ACL KEY                                  *      ELUCSACL
01170 *                                                          *      ELUCSACL
01171 ************************************************************      ELUCSACL
01172  BUILD-WS-ACL-KEY.                                                ELUCSACL
01173      MOVE ATBL-ASCEND-DESCEND-IND (ATBL-X-IDX)                    ELUCSACL
01174                                            TO  WS-ACL-KEY-A-D-IND.ELUCSACL
01175      MOVE ATBL-BENEFIT-PERIOD (ATBL-X-IDX) TO  WS-ACL-KEY-TYPE.   ELUCSACL
01176      MOVE ATBL-MANDATORY-IND (ATBL-X-IDX)  TO  WS-ACL-KEY-MAN-IND.ELUCSACL
01177      MOVE ATBL-FAM-OR-INDIV (ATBL-X-IDX)   TO  WS-ACL-KEY-F-R-I.  ELUCSACL
01178      MOVE ATBL-L-O-B (ATBL-X-IDX)          TO  WS-ACL-KEY-L-O-B.  ELUCSACL
01179      MOVE ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX)                   ELUCSACL
01180                                            TO  WS-ACL-KEY-INT-DES.ELUCSACL
01181      MOVE ATBL-SERVICE-GROUP (ATBL-X-IDX)  TO  WS-ACL-KEY-SER-GRP.ELUCSACL
01182      MOVE ATBL-PLACE-OF-TREATMENT (ATBL-X-IDX)                    ELUCSACL
01183                                            TO  WS-ACL-KEY-TRT-GRP.ELUCSACL
01184      MOVE ATBL-CONDITION (ATBL-X-IDX)      TO  WS-ACL-KEY-COND.   ELUCSACL
01185      MOVE ATBL-COST-CONTAIN-IND (ATBL-X-IDX)                      ELUCSACL
01186                                            TO  WS-ACL-KEY-COST.   ELUCSACL
01187      MOVE ATBL-CO-PAY-IND (ATBL-X-IDX)     TO  WS-ACL-KEY-COPAY.  ELUCSACL
01188                                                                   ELUCSACL
01189 ************************************************************      ELUCSACL
01190 *                                                          *      ELUCSACL
01191 *        CAPTURE-BASIC-INFO                                *      ELUCSACL
01192 *                                                          *      ELUCSACL
01193 ************************************************************      ELUCSACL
01194  CAPTURE-BASIC-INFO.                                              ELUCSACL
01195      MOVE ATBL-IBGR-SLOT-NUMBER        (ATBL-X-IDX)               ELUCSACL
01196        TO CSBP-COINS-IBGR-SLOT         (CSBP-X-IDX).              ELUCSACL
01197      MOVE ATBL-BENEFIT-PERIOD          (ATBL-X-IDX)               ELUCSACL
01198        TO CSBP-COINS-BENEFIT-PERIOD    (CSBP-X-IDX).              ELUCSACL
01199      MOVE ATBL-L-O-B                   (ATBL-X-IDX)               ELUCSACL
01200        TO CSBP-COINS-L-O-B             (CSBP-X-IDX).              ELUCSACL
01201      MOVE ATBL-PLACE-OF-TREATMENT      (ATBL-X-IDX)               ELUCSACL
01202        TO CSBP-COINS-PLACE-OF-TREATMENT (CSBP-X-IDX).             ELUCSACL
01203      MOVE ATBL-VALUE-QUALIFIER         (ATBL-X-IDX)               ELUCSACL
01204        TO CSBP-COINS-VALUE-QUALIFIER   (CSBP-X-IDX).              ELUCSACL
01205                                                                   ELUCSACL
01206 ************************************************************      ELUCSACL
01207 *                                                          *      ELUCSACL
01208 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELUCSACL
01209 *                                                          *      ELUCSACL
01210 ************************************************************      ELUCSACL
01211  ESTABLISH-ADDRESS-OF-CNTL-BLK.                                   ELUCSACL
01212      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELUCSACL
01213          PERFORM SIGNAL-INVALID-COMMAREA.                         ELUCSACL
01214      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUCSACL
01215          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELUCSACL
01216                                                                   ELUCSACL
01217 ************************************************************      ELUCSACL
01218 *                                                          *      ELUCSACL
01219 *        ESTABLISH ADDRESSABILITY OF POINTER LIST          *      ELUCSACL
01220 *                                                          *      ELUCSACL
01221 ************************************************************      ELUCSACL
01222  ESTABLISH-ADDRESS-OF-PTR-LIST.                                   ELUCSACL
01223      SET  CIA-ELSCSPTC-DDN TO TRUE.                               ELUCSACL
01224      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSACL
01225          ADDRESS OF CSPT-POINTER-LIST.                            ELUCSACL
01226      IF CIA-RC-PTR-NULL                                           ELUCSACL
01227          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSACL
01228                                                                   ELUCSACL
01229 ************************************************************      ELUCSACL
01230 *                                                          *      ELUCSACL
01231 *        ESTABLISH ADDRESSABILITY OF ACCUMULATOR TABLE     *      ELUCSACL
01232 *                                                          *      ELUCSACL
01233 ************************************************************      ELUCSACL
01234  ESTABLISH-ADDRESS-OF-ACCUM-TBL.                                  ELUCSACL
01235      SET  CIA-ELSCSAC-DDN TO TRUE.                                ELUCSACL
01236      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSACL
01237          ADDRESS OF CSAC-ACCUMULATOR-TABLE.                       ELUCSACL
01238      IF CIA-RC-PTR-NULL                                           ELUCSACL
01239          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSACL
01240                                                                   ELUCSACL
01241 ************************************************************      ELUCSACL
01242 *                                                          *      ELUCSACL
01243 *        ESTABLISH ADDRESSABILITY OF INTERNALS TABLE       *      ELUCSACL
01244 *                                                          *      ELUCSACL
01245 ************************************************************      ELUCSACL
01246  ESTABLISH-ADDRESS-OF-INTRL-TBL.                                  ELUCSACL
01247      SET  CIA-ELSIBGR-DDN TO TRUE.                                ELUCSACL
01248      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSACL
01249          ADDRESS OF IBGR-INTERNAL-TABS-TABLE.                     ELUCSACL
01250                                                                   ELUCSACL
01251 ************************************************************      ELUCSACL
01252 *                                                          *      ELUCSACL
01253 *        ESTABLISH ATBLC ADDRESSABILITY                    *      ELUCSACL
01254 *                                                          *      ELUCSACL
01255 ************************************************************      ELUCSACL
01256  ESTABLISH-ADDRESS-OF-ATBL-TBL.                                   ELUCSACL
01257      SET  CIA-ELSATBL-DDN TO TRUE.                                ELUCSACL
01258      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSACL
01259          ADDRESS OF ATBL-ACCUMULATOR-TABLE.                       ELUCSACL
01260      IF CIA-RC-PTR-NULL                                           ELUCSACL
01261          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSACL
01262                                                                   ELUCSACL
01263 ************************************************************      ELUCSACL
01264 *                                                          *      ELUCSACL
01265 *        ESTABLISH ACL ADDRESSABILITY                      *      ELUCSACL
01266 *                                                          *      ELUCSACL
01267 ************************************************************      ELUCSACL
01268  ESTABLISH-ADDRESS-OF-ACL-TBL.                                    ELUCSACL
01269      SET  CIA-ELSCSADC-DDN TO TRUE.                               ELUCSACL
01270      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSACL
01271          ADDRESS OF CSAD-ACL-TABLE.                               ELUCSACL
01272      IF CIA-RC-PTR-NULL                                           ELUCSACL
01273         SET CIA-STG-GETMAIN TO TRUE                               ELUCSACL
01274         EXEC CICS LINK                                            ELUCSACL
01275              PROGRAM('ELUSTGMG')                                  ELUCSACL
01276              COMMAREA(DFHCOMMAREA)                                ELUCSACL
01277         END-EXEC.                                                 ELUCSACL
01278      SET  CIA-ELSCSADC-DDN TO TRUE.                               ELUCSACL
01279      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSACL
01280          ADDRESS OF CSAD-ACL-TABLE.                               ELUCSACL
01281      MOVE ZEROES TO CSAD-ACL-NBR-ENTRIES                          ELUCSACL
01282                     CSAD-ENTRIES-USED (CSAD-ACL-IDX).             ELUCSACL
01283                                                                   ELUCSACL
01284 ************************************************************      ELUCSACL
01285 *                                                          *      ELUCSACL
01286 *        SIGNAL INVALID COMMAREA                           *      ELUCSACL
01287 *                                                          *      ELUCSACL
01288 ************************************************************      ELUCSACL
01289  SIGNAL-INVALID-COMMAREA.                                         ELUCSACL
01290      EXEC CICS ABEND                                              ELUCSACL
01291                ABCODE('EL01')                                     ELUCSACL
01292         END-EXEC.                                                 ELUCSACL
01293                                                                   ELUCSACL
01294 ************************************************************      ELUCSACL
01295 *                                                          *      ELUCSACL
01296 *        SIGNAL UNALLOC AREA ERROR                         *      ELUCSACL
01297 *                                                          *      ELUCSACL
01298 ************************************************************      ELUCSACL
01299  SIGNAL-UNALLOC-AREA-ERROR.                                       ELUCSACL
01300      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUCSACL
01301      PERFORM SIGNAL-ABEND.                                        ELUCSACL
01302                                                                   ELUCSACL
01303 ************************************************************      ELUCSACL
01304 *                                                          *      ELUCSACL
01305 *        SIGNAL PROGRAM LOGIC ERROR                        *      ELUCSACL
01306 *                                                          *      ELUCSACL
01307 ************************************************************      ELUCSACL
01308  SIGNAL-PROGRAM-LOGIC-ERROR.                                      ELUCSACL
01309      SET CIA-AB-PGM-LOGIC TO TRUE.                                ELUCSACL
01310      PERFORM SIGNAL-ABEND.                                        ELUCSACL
01311                                                                   ELUCSACL
01312 ************************************************************      ELUCSACL
01313 *                                                          *      ELUCSACL
01314 *        SIGNAL ABEND                                      *      ELUCSACL
01315 *                                                          *      ELUCSACL
01316 ************************************************************      ELUCSACL
01317  SIGNAL-ABEND.                                                    ELUCSACL
01318      EXEC CICS ABEND                                              ELUCSACL
01319                ABCODE(CIA-ABCODE)                                 ELUCSACL
01320         END-EXEC.                                                 ELUCSACL
