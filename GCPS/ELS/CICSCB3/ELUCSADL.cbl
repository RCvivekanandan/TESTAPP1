00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUCSADL
00003  PROGRAM-ID.         ELUCSADL.                                       LV001
00004                                                                   ELUCSADL
00005  AUTHOR.             GEORGE E MOORE.                              ELUCSADL
00006                                                                   ELUCSADL
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUCSADL
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUCSADL
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUCSADL
00010                      233 N. MICHIGAN AVE                          ELUCSADL
00011                      CHICAGO, ILLINOIS 60601                      ELUCSADL
00012                                                                   ELUCSADL
00013  DATE-WRITTEN.       11-OCT-1989.                                 ELUCSADL
00014                                                                   ELUCSADL
00015  DATE-COMPILED.                                                   ELUCSADL
00016                                                                   ELUCSADL
00017  SECURITY.           COPYRIGHT 1986,                              ELUCSADL
00018                      HEALTH CARE SERVICE CORPORATION              ELUCSADL
00019      SKIP3                                                        ELUCSADL
00020  TITLE 'ELS CONTRACT SUMMARY DEDUCTIBLE EXTRACT UTILITY   '.      ELUCSADL
00021  ENVIRONMENT DIVISION.                                            ELUCSADL
00022                                                                   ELUCSADL
00023  CONFIGURATION SECTION.                                           ELUCSADL
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELUCSADL
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELUCSADL
00026      EJECT                                                        ELUCSADL
00027 ******************************************************************ELUCSADL
00028 *              MAINTAINANCE HISTORY                              *ELUCSADL
00029 *                                                                *ELUCSADL
00030 *   MOD     DATE     BY     ACTION                               *ELUCSADL
00031 *  01.00  11-OCT-89  GEM    CREATED                              *ELUCSADL
00032 *                                                                *ELUCSADL
00033 ******************************************************************ELUCSADL
00034                                                                   ELUCSADL
00035  DATA DIVISION.                                                   ELUCSADL
00036  WORKING-STORAGE SECTION.                                         ELUCSADL
00037  01  WS-HOLD-AREA.                                                ELUCSADL
00038      03  WS-LINE-OF-BUSINESS       OCCURS 2 TIMES                 ELUCSADL
00039                                    PIC X(01).                     ELUCSADL
00040          88  INSTITUTIONAL-LOB              VALUE  '1' '5'        ELUCSADL
00041                                                    '4' '6' '8'.   ELUCSADL
00042          88  PROFESSIONAL-LOB               VALUE  '2' '4'        ELUCSADL
00043                                                    '5' '7' '8'.   ELUCSADL
00044          88  SUPPLEMENTAL-LOB                VALUE '3' '6'        ELUCSADL
00045                                                    '7' '8'.       ELUCSADL
00046 *                                                                 ELUCSADL
00047     03  WS-OBSTETRICS                  PIC X(04).                 ELUCSADL
00048         88  OB-NORMAL                  VALUE 'OBNM' 'OBNS'        ELUCSADL
00049                                              'OBND' 'EABI' 'EABO'.ELUCSADL
00050         88  OB-COMPLICATED             VALUE 'OBCM' 'OBCS'        ELUCSADL
00051                                              'OBCD' 'TABI' 'TABO'.ELUCSADL
00052     03  WS-EMERGENCY                   PIC X(04).                 ELUCSADL
00053         88  EMER-ACCIDENT                    VALUE 'EAER' ' EAC'. ELUCSADL
00054         88  EMER-MEDICAL                     VALUE 'EMER' ' EMC'. ELUCSADL
00055 *                                                                 ELUCSADL
00056  01  WS-SUBSCRIPTS   USAGE COMP SYNC.                             ELUCSADL
00057      05  WS-BP-SUB                PIC S9(4).                      ELUCSADL
00058      05  WS-CSPT-SUB              PIC S9(4).                      ELUCSADL
00059      05  WS-BPL-X-SUB             PIC S9(4).                      ELUCSADL
00060      05  WS-CSBP-X-SUB            PIC S9(4).                      ELUCSADL
00061      05  WS-ATBL-X-SUB            PIC S9(4).                      ELUCSADL
00062      05  WS-IBGR-X-SUB            PIC S9(4).                      ELUCSADL
00063      05  WS-GX1-SUB               PIC S9(4).                      ELUCSADL
00064 *                                                                 ELUCSADL
00065  01  PRIORITY-SUB-SAVE  USAGE COMP SYNC.                          ELUCSADL
00066      05  IN-PRIORITY-LVL          PIC S9(4) VALUE ZERO.           ELUCSADL
00067      05  PRIORITY-LVL             PIC S9(4) VALUE ZERO.           ELUCSADL
00068 *                                                                 ELUCSADL
00069  01  WS-SWITCHES.                                                 ELUCSADL
00070      03  WS-APPLICABLE-SWITCH     PIC X(01)  VALUE SPACES.        ELUCSADL
00071          88  APPLICABLE                      VALUE 'Y'.           ELUCSADL
00072          88  NOT-APPLICABLE                  VALUE 'N'.           ELUCSADL
00073      03  WS-IBGR-SWITCH            PIC X(01) VALUE SPACES.        ELUCSADL
00074          88  IBGR-FOUND                      VALUE 'Y'.           ELUCSADL
00075          88  NO-IBGR-FOUND                   VALUE 'N'.           ELUCSADL
00076      03  WS-BP-ID-SWITCH           PIC X(01) VALUE SPACES.        ELUCSADL
00077          88  WS-BP-ID-FOUND                  VALUE 'Y'.           ELUCSADL
00078          88  WS-BP-ID-NOT-FOUND              VALUE 'N'.           ELUCSADL
00079      03  WS-CONCLUSIVE-SWITCH      PIC X(01) VALUE SPACES.        ELUCSADL
00080          88  CONCLUSIVE                      VALUE 'Y'.           ELUCSADL
00081          88  NOT-CONCLUSIVE                  VALUE 'N'.           ELUCSADL
00082      03  WS-QUALIFICATION-STATUS   PIC X(01) VALUE SPACES.        ELUCSADL
00083          88  WS-BP-QUALIFIED                 VALUE 'Y'.           ELUCSADL
00084          88  WS-BP-NOT-QUALIFIED             VALUE 'N'.           ELUCSADL
00085      03  WS-TABULAR-STATUS         PIC X(02) VALUE SPACES.        ELUCSADL
00086          88  TABULAR-INCLUDED                VALUE 'IN'.          ELUCSADL
00087          88  TABULAR-EXCLUDED                VALUE 'EX'.          ELUCSADL
00088      03  WS-LEVEL-STATUS           PIC X(02) VALUE SPACES.        ELUCSADL
00089          88  LEVEL-IS-BP                     VALUE 'BP'.          ELUCSADL
00090          88  LEVEL-IS-GC                     VALUE 'GC'.          ELUCSADL
00091      03  IP-OP-IND                 PIC X(01) VALUE SPACES.        ELUCSADL
00092          88  IN-PATIENT                      VALUE 'I'.           ELUCSADL
00093          88  OUT-PATIENT                     VALUE 'O'.           ELUCSADL
00094          88  BOTH-IP-OP                      VALUE 'B'.           ELUCSADL
00095      03  SEARCH-SWITCH             PIC X(01) VALUE SPACES.        ELUCSADL
00096          88  SEARCH-IS-COMPLETE              VALUE 'Y'.           ELUCSADL
00097      03  WS-BP-ID-FORMAT           PIC X(01) VALUE SPACES.        ELUCSADL
00098          88  INSTITUTIONAL                  VALUE  'A' 'B' 'W'.   ELUCSADL
00099          88  PROFESSIONAL                   VALUE  'C' 'D' 'E'.   ELUCSADL
00100      03  SPECIFIC-SWITCH           PIC X(01) VALUE SPACES.        ELUCSADL
00101          88  SPECIFIC                        VALUE 'Y'.           ELUCSADL
00102          88  NOT-SPECIFIC                    VALUE 'N'.           ELUCSADL
00103      03  IP-OP-SELECT-SWT          PIC X(01) VALUE SPACES.        ELUCSADL
00104          88  IP-OP-IND-SELECTED              VALUE 'Y'.           ELUCSADL
00105 *                                                                 ELUCSADL
00106  01  CONF-FACTORS.                                                ELUCSADL
00107      03  CF-TWO                    COMP-1 VALUE +0.200000E+00.    ELUCSADL
00108      03  CF-FIVE                   COMP-1 VALUE +0.500000E+00.    ELUCSADL
00109 *                                                                 ELUCSADL
00110  01  PROGRAM-CONSTANTS.                                           ELUCSADL
00111      03  PC-ONE                    PIC X(01) VALUE '1'.           ELUCSADL
00112      03  PC-OB                     PIC X(02) VALUE '0B'.          ELUCSADL
00113      03  PC-OC                     PIC X(02) VALUE '0C'.          ELUCSADL
00114      03  PC-OD                     PIC X(02) VALUE '0D'.          ELUCSADL
00115 *                                                                 ELUCSADL
00116      COPY ELSBPTBL.                                               ELUCSADL
00117 *                                                                 ELUCSADL
00118      COPY ELSBPITC.                                               ELUCSADL
00119  LINKAGE SECTION.                                                 ELUCSADL
00120  01  DFHCOMMAREA.                                                 ELUCSADL
00121      COPY ELSCOMMC.                                               ELUCSADL
00122 *                                                                 ELUCSADL
00123      COPY ELSCIA2C.                                               ELUCSADL
00124 /                                                                 ELUCSADL
00125      COPY ELSCSACC.                                               ELUCSADL
00126 /                                                                 ELUCSADL
00127      COPY ELSCSPTC.                                               ELUCSADL
00128 /                                                                 ELUCSADL
00129      COPY ELSIBGRC.                                               ELUCSADL
00130 /                                                                 ELUCSADL
00131      COPY ELSCSBPC.                                               ELUCSADL
00132 /                                                                 ELUCSADL
00133      COPY ELSATBLC.                                               ELUCSADL
00134 /                                                                 ELUCSADL
00135  01  INTERNAL-TABULAR-RECORD.                                     ELUCSADL
00136      COPY GCTIBGRC.                                               ELUCSADL
00137 /                                                                 ELUCSADL
00138  01  BENEFIT-PERIOD-TABLE.                                        ELUCSADL
00139      03  BENEFIT-PERIOD        OCCURS 25 TIMES                    ELUCSADL
00140                                PIC X(02).                         ELUCSADL
00141 /                                                                 ELUCSADL
00142      EJECT                                                        ELUCSADL
00143  PROCEDURE DIVISION.                                              ELUCSADL
00144                                                                   ELUCSADL
00145 ************************************************************      ELUCSADL
00146 *                                                          *      ELUCSADL
00147 *        CONTRACT SUMMARY DEDUCTIBLE                       *      ELUCSADL
00148 *                                                          *      ELUCSADL
00149 ************************************************************      ELUCSADL
00150  CONTRACT-SUMMARY-DEDUCTIBLE.                                     ELUCSADL
00151      PERFORM ESTABLISH-ADDRESS-OF-CNTL-BLK.                       ELUCSADL
00152                                                                   ELUCSADL
00153      PERFORM ESTABLISH-ADDRESS-OF-PTR-LIST.                       ELUCSADL
00154                                                                   ELUCSADL
00155      PERFORM ESTABLISH-ADDRESS-OF-ACCUM-TBL.                      ELUCSADL
00156                                                                   ELUCSADL
00157      PERFORM ESTABLISH-ADDRESS-OF-INTRL-TBL.                      ELUCSADL
00158                                                                   ELUCSADL
00159      PERFORM ESTABLISH-ADDRESS-OF-ATBL-TBL.                       ELUCSADL
00160                                                                   ELUCSADL
00161      PERFORM PROCESS-BENEFIT-PROVISION-PTRS                       ELUCSADL
00162              VARYING WS-CSPT-SUB FROM 1 BY 1                      ELUCSADL
00163                UNTIL WS-CSPT-SUB > CSPT-TBL-CNT.                  ELUCSADL
00164      GOBACK.                                                      ELUCSADL
00165      EJECT                                                        ELUCSADL
00166 ************************************************************      ELUCSADL
00167 *                                                          *      ELUCSADL
00168 *        PROCESS BENEFIT PROVISION PTRS                    *      ELUCSADL
00169 *                                                          *      ELUCSADL
00170 ************************************************************      ELUCSADL
00171  PROCESS-BENEFIT-PROVISION-PTRS.                                  ELUCSADL
00172      SET CSPT-IDX TO WS-CSPT-SUB.                                 ELUCSADL
00173      IF CSPT-BP-TBL-PTR (CSPT-IDX) NOT = NULLS                    ELUCSADL
00174          PERFORM PROCESS-DEDUCTIBLE-PER-BEN-PRV.                  ELUCSADL
00175                                                                   ELUCSADL
00176                                                                   ELUCSADL
00177 ************************************************************      ELUCSADL
00178 *                                                          *      ELUCSADL
00179 *        PROCESS DEDUCTIBLE PER BENEFIT PROVISION POINTER  *      ELUCSADL
00180 *                                                          *      ELUCSADL
00181 ************************************************************      ELUCSADL
00182  PROCESS-DEDUCTIBLE-PER-BEN-PRV.                                  ELUCSADL
00183      SET ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE TO               ELUCSADL
00184            CSPT-BP-TBL-PTR (CSPT-IDX).                            ELUCSADL
00185      PERFORM EXTRACT-DED-INFO-FOR-EACH-BENE                       ELUCSADL
00186          VARYING WS-CSBP-X-SUB FROM 1 BY 1 UNTIL                  ELUCSADL
00187                            WS-CSBP-X-SUB  > CSBP-TBL-CNT.         ELUCSADL
00188                                                                   ELUCSADL
00189                                                                   ELUCSADL
00190 ************************************************************      ELUCSADL
00191 *                                                          *      ELUCSADL
00192 *        EXTRACT DED INFO FOR EACH BENEFIT PROVISION       *      ELUCSADL
00193 *                                                          *      ELUCSADL
00194 ************************************************************      ELUCSADL
00195  EXTRACT-DED-INFO-FOR-EACH-BENE.                                  ELUCSADL
00196      SET CSBP-X-IDX TO WS-CSBP-X-SUB.                             ELUCSADL
00197      IF NOT CSBP-FORMAT-W (CSBP-X-IDX) AND                        ELUCSADL
00198             CSBP-PROVN-PRICING-METHD (CSBP-X-IDX) > ZERO          ELUCSADL
00199          PERFORM PROCESS-ITEMS-WITH-PROVN-PRICI.                  ELUCSADL
00200      EJECT                                                        ELUCSADL
00201                                                                   ELUCSADL
00202                                                                   ELUCSADL
00203 ************************************************************      ELUCSADL
00204 *                                                          *      ELUCSADL
00205 *        PROCESS ITEMS WITH PROVN PRICING METHD            *      ELUCSADL
00206 *                                                          *      ELUCSADL
00207 ************************************************************      ELUCSADL
00208  PROCESS-ITEMS-WITH-PROVN-PRICI.                                  ELUCSADL
00209      IF CSBP-COVERED (CSBP-X-IDX) AND                             ELUCSADL
00210         CSBP-PAYMENT-REQUESTED (CSBP-X-IDX)                       ELUCSADL
00211            PERFORM EXTRACT-DEDUCTIBLES-INFO                       ELUCSADL
00212      ELSE IF CSBP-COVERED-ON-SUPP (CSBP-X-IDX) AND                ELUCSADL
00213              CSBP-USE-SUPP-INFO (CSBP-X-IDX)                      ELUCSADL
00214                 PERFORM EXTRACT-DEDUCTIBLES-INFO.                 ELUCSADL
00215                                                                   ELUCSADL
00216                                                                   ELUCSADL
00217 ************************************************************      ELUCSADL
00218 *                                                          *      ELUCSADL
00219 *        EXTRACT DEDUCTIBLES INFORMATION                   *      ELUCSADL
00220 *                                                          *      ELUCSADL
00221 ************************************************************      ELUCSADL
00222  EXTRACT-DEDUCTIBLES-INFO.                                        ELUCSADL
00223      PERFORM CLEAR-CSBP-ADL-DATA.                                 ELUCSADL
00224      IF CSAC-ADL-BP-TBL-PTR NOT = NULL  AND                       ELUCSADL
00225         CSBP-BP-ADL-SLOT (CSBP-X-IDX) NOT = ZERO                  ELUCSADL
00226              PERFORM INTERROGATE-BENEFIT-PROVISIONX.              ELUCSADL
00227      IF CSAC-ADL-GC-TBL-PTR NOT = NULL                            ELUCSADL
00228            PERFORM INTERROGATE-GROUP-CONTRACT-LEV.                ELUCSADL
00229      EJECT                                                        ELUCSADL
00230                                                                   ELUCSADL
00231                                                                   ELUCSADL
00232 ************************************************************      ELUCSADL
00233 *                                                          *      ELUCSADL
00234 *        INTERROGATE BENEFIT PROVISION LEVEL               *      ELUCSADL
00235 *                                                          *      ELUCSADL
00236 ************************************************************      ELUCSADL
00237  INTERROGATE-BENEFIT-PROVISIONX.                                  ELUCSADL
00238      SET LEVEL-IS-BP TO TRUE.                                     ELUCSADL
00239      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                        ELUCSADL
00240          TO CSAC-ADL-BP-TBL-PTR.                                  ELUCSADL
00241      MOVE CSBP-L-O-B (CSBP-X-IDX) TO WS-LINE-OF-BUSINESS (1).     ELUCSADL
00242      PERFORM SEARCH-FOR-MATCHING-SLOT                             ELUCSADL
00243          VARYING WS-ATBL-X-SUB FROM 1 BY 1                        ELUCSADL
00244                    UNTIL WS-ATBL-X-SUB > ATBL-TBL-CNT.            ELUCSADL
00245                                                                   ELUCSADL
00246                                                                   ELUCSADL
00247 ************************************************************      ELUCSADL
00248 *                                                          *      ELUCSADL
00249 *        SEARCH FOR MATCHING SLOT                          *      ELUCSADL
00250 *                                                          *      ELUCSADL
00251 ************************************************************      ELUCSADL
00252  SEARCH-FOR-MATCHING-SLOT.                                        ELUCSADL
00253      SET ATBL-X-IDX TO WS-ATBL-X-SUB.                             ELUCSADL
00254      IF ATBL-SLOT-NUMBER (ATBL-X-IDX) =                           ELUCSADL
00255         CSBP-BP-ADL-SLOT (CSBP-X-IDX)                             ELUCSADL
00256            PERFORM PROCESS-MATCHING-SLOT.                         ELUCSADL
00257      EJECT                                                        ELUCSADL
00258                                                                   ELUCSADL
00259                                                                   ELUCSADL
00260 ************************************************************      ELUCSADL
00261 *                                                          *      ELUCSADL
00262 *        INTERROGATE GROUP CONTRACT LEVEL                  *      ELUCSADL
00263 *                                                          *      ELUCSADL
00264 ************************************************************      ELUCSADL
00265  INTERROGATE-GROUP-CONTRACT-LEV.                                  ELUCSADL
00266      SET LEVEL-IS-GC TO TRUE.                                     ELUCSADL
00267      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                        ELUCSADL
00268          TO CSAC-ADL-GC-TBL-PTR.                                  ELUCSADL
00269      MOVE CSBP-L-O-B (CSBP-X-IDX) TO WS-LINE-OF-BUSINESS (1).     ELUCSADL
00270      PERFORM PROCESS-ALL-MATCHING-SLOTS                           ELUCSADL
00271          VARYING WS-ATBL-X-SUB FROM 1 BY 1                        ELUCSADL
00272                    UNTIL WS-ATBL-X-SUB > ATBL-TBL-CNT.            ELUCSADL
00273                                                                   ELUCSADL
00274                                                                   ELUCSADL
00275 ************************************************************      ELUCSADL
00276 *                                                          *      ELUCSADL
00277 *        PROCESS ALL MATCHING SLOTS                        *      ELUCSADL
00278 *                                                          *      ELUCSADL
00279 ************************************************************      ELUCSADL
00280  PROCESS-ALL-MATCHING-SLOTS.                                      ELUCSADL
00281      SET ATBL-X-IDX TO WS-ATBL-X-SUB.                             ELUCSADL
00282      PERFORM PROCESS-MATCHING-SLOT.                               ELUCSADL
00283      EJECT                                                        ELUCSADL
00284                                                                   ELUCSADL
00285                                                                   ELUCSADL
00286 ************************************************************      ELUCSADL
00287 *                                                          *      ELUCSADL
00288 *        PROCESS MATCHING SLOT                             *      ELUCSADL
00289 *                                                          *      ELUCSADL
00290 ************************************************************      ELUCSADL
00291  PROCESS-MATCHING-SLOT.                                           ELUCSADL
00292      SET NOT-SPECIFIC TO TRUE.                                    ELUCSADL
00293      PERFORM EXAMINE-COST-CONTAINMENT-IND.                        ELUCSADL
00294      IF APPLICABLE                                                ELUCSADL
00295          PERFORM EXAMINE-SERVICE-GROUP.                           ELUCSADL
00296      IF APPLICABLE                                                ELUCSADL
00297          PERFORM EXAMINE-COVERAGE.                                ELUCSADL
00298      IF APPLICABLE                                                ELUCSADL
00299          PERFORM EXAMINE-IBGR.                                    ELUCSADL
00300      IF APPLICABLE AND NOT-CONCLUSIVE                             ELUCSADL
00301          PERFORM EXAMINE-PLACE-OF-TREATMENT.                      ELUCSADL
00302      IF APPLICABLE AND NOT-CONCLUSIVE                             ELUCSADL
00303          PERFORM EXAMINE-LINE-OF-BUSINESS.                        ELUCSADL
00304      IF APPLICABLE                                                ELUCSADL
00305          PERFORM EXAMINE-CONDITION-BITS.                          ELUCSADL
00306      IF APPLICABLE                                                ELUCSADL
00307          PERFORM EXAMINE-CONFIDENCE-FACTORS.                      ELUCSADL
00308      IF APPLICABLE                                                ELUCSADL
00309          PERFORM CHECK-FOR-ADL-OVERALL.                           ELUCSADL
00310      EJECT                                                        ELUCSADL
00311                                                                   ELUCSADL
00312                                                                   ELUCSADL
00313 ************************************************************      ELUCSADL
00314 *                                                          *      ELUCSADL
00315 *        EXAMINE COST CONTAINMENT IND                      *      ELUCSADL
00316 *                                                          *      ELUCSADL
00317 ************************************************************      ELUCSADL
00318  EXAMINE-COST-CONTAINMENT-IND.                                    ELUCSADL
00319      IF ATBL-COST-CONTAIN-IND (ATBL-X-IDX) = ZERO                 ELUCSADL
00320          SET APPLICABLE TO TRUE                                   ELUCSADL
00321        ELSE                                                       ELUCSADL
00322          SET NOT-APPLICABLE TO TRUE                               ELUCSADL
00323        END-IF.                                                    ELUCSADL
00324      EJECT                                                        ELUCSADL
00325                                                                   ELUCSADL
00326                                                                   ELUCSADL
00327 ************************************************************      ELUCSADL
00328 *                                                          *      ELUCSADL
00329 *        EXAMINE SERVICE GROUP                             *      ELUCSADL
00330 *                                                          *      ELUCSADL
00331 ************************************************************      ELUCSADL
00332  EXAMINE-SERVICE-GROUP.                                           ELUCSADL
00333      SET NOT-APPLICABLE TO TRUE.                                  ELUCSADL
00334      IF CSBP-OUTPATIENT-ST                                        ELUCSADL
00335          PERFORM INVESTIGATE-OUTPATIENT-CODES.                    ELUCSADL
00336      IF ATBL-SERVICE-GROUP (ATBL-X-IDX) = ZERO                    ELUCSADL
00337         SET APPLICABLE TO TRUE.                                   ELUCSADL
00338                                                                   ELUCSADL
00339                                                                   ELUCSADL
00340 ************************************************************      ELUCSADL
00341 *                                                          *      ELUCSADL
00342 *        INVESTIGATE OUTPATIENT CODES                      *      ELUCSADL
00343 *                                                          *      ELUCSADL
00344 ************************************************************      ELUCSADL
00345  INVESTIGATE-OUTPATIENT-CODES.                                    ELUCSADL
00346      MOVE CSBP-BP-KEY (CSBP-X-IDX) TO WS-EMERGENCY.               ELUCSADL
00347      IF EMER-ACCIDENT AND                                         ELUCSADL
00348         ATBL-SERVICE-GROUP (ATBL-X-IDX) = PC-OC                   ELUCSADL
00349            SET APPLICABLE TO TRUE.                                ELUCSADL
00350      IF EMER-MEDICAL AND                                          ELUCSADL
00351         ATBL-SERVICE-GROUP (ATBL-X-IDX) = PC-OD                   ELUCSADL
00352            SET APPLICABLE TO TRUE.                                ELUCSADL
00353      IF ATBL-SERVICE-GROUP (ATBL-X-IDX) = ZERO                    ELUCSADL
00354            SET APPLICABLE TO TRUE.                                ELUCSADL
00355      EJECT                                                        ELUCSADL
00356                                                                   ELUCSADL
00357                                                                   ELUCSADL
00358 ************************************************************      ELUCSADL
00359 *                                                          *      ELUCSADL
00360 *        EXAMINE COVERAGE                                  *      ELUCSADL
00361 *                                                          *      ELUCSADL
00362 ************************************************************      ELUCSADL
00363  EXAMINE-COVERAGE.                                                ELUCSADL
00364      SET NOT-APPLICABLE TO TRUE.                                  ELUCSADL
00365      IF CSBP-COVERED (CSBP-X-IDX) AND                             ELUCSADL
00366         ATBL-CF-BAS (ATBL-X-IDX) > CF-FIVE                        ELUCSADL
00367            SET APPLICABLE TO TRUE                                 ELUCSADL
00368        ELSE                                                       ELUCSADL
00369        IF CSBP-COVERED-ON-SUPP (CSBP-X-IDX) AND                   ELUCSADL
00370           ATBL-CF-SUP (ATBL-X-IDX) > CF-FIVE                      ELUCSADL
00371              SET APPLICABLE TO TRUE                               ELUCSADL
00372        END-IF.                                                    ELUCSADL
00373      EJECT                                                        ELUCSADL
00374                                                                   ELUCSADL
00375                                                                   ELUCSADL
00376 ************************************************************      ELUCSADL
00377 *                                                          *      ELUCSADL
00378 *        EXAMINE IBGR                                      *      ELUCSADL
00379 *                                                          *      ELUCSADL
00380 ************************************************************      ELUCSADL
00381  EXAMINE-IBGR.                                                    ELUCSADL
00382      SET NOT-APPLICABLE TO TRUE.                                  ELUCSADL
00383      SET NO-IBGR-FOUND TO TRUE.                                   ELUCSADL
00384      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELUCSADL
00385          PERFORM SIGNAL-UNALLOC-IBGR-ERROR                        ELUCSADL
00386      ELSE                                                         ELUCSADL
00387          PERFORM PROCESS-INTERNAL-TABULAR-SLOT.                   ELUCSADL
00388      IF IBGR-FOUND                                                ELUCSADL
00389          PERFORM PROCESS-FOUND-IBGR                               ELUCSADL
00390      ELSE                                                         ELUCSADL
00391          PERFORM PROCESS-NOT-FOUND-IBGR.                          ELUCSADL
00392                                                                   ELUCSADL
00393                                                                   ELUCSADL
00394 ************************************************************      ELUCSADL
00395 *                                                          *      ELUCSADL
00396 *        PROCESS INTERNAL TABULAR SLOT                     *      ELUCSADL
00397 *                                                          *      ELUCSADL
00398 ************************************************************      ELUCSADL
00399  PROCESS-INTERNAL-TABULAR-SLOT.                                   ELUCSADL
00400      PERFORM SEARCH-FOR-INTERNAL-TABULAR-SL                       ELUCSADL
00401          VARYING WS-IBGR-X-SUB FROM 1 BY 1                        ELUCSADL
00402                     UNTIL WS-IBGR-X-SUB > IBGR-TBL-CNT            ELUCSADL
00403                        OR IBGR-FOUND.                             ELUCSADL
00404      EJECT                                                        ELUCSADL
00405                                                                   ELUCSADL
00406                                                                   ELUCSADL
00407 ************************************************************      ELUCSADL
00408 *                                                          *      ELUCSADL
00409 *        PROCESS FOUND IBGR                                *      ELUCSADL
00410 *                                                          *      ELUCSADL
00411 ************************************************************      ELUCSADL
00412  PROCESS-FOUND-IBGR.                                              ELUCSADL
00413      IF TABULAR-INCLUDED                                          ELUCSADL
00414          PERFORM PROCESS-INCLUDED                                 ELUCSADL
00415      ELSE                                                         ELUCSADL
00416          PERFORM PROCESS-EXCLUDED.                                ELUCSADL
00417      EJECT                                                        ELUCSADL
00418                                                                   ELUCSADL
00419                                                                   ELUCSADL
00420 ************************************************************      ELUCSADL
00421 *                                                          *      ELUCSADL
00422 *        PROCESS NOT FOUND IBGR                            *      ELUCSADL
00423 *                                                          *      ELUCSADL
00424 ************************************************************      ELUCSADL
00425  PROCESS-NOT-FOUND-IBGR.                                          ELUCSADL
00426      IF LEVEL-IS-BP                                               ELUCSADL
00427          SET CONCLUSIVE TO TRUE                                   ELUCSADL
00428          SET APPLICABLE TO TRUE                                   ELUCSADL
00429        ELSE                                                       ELUCSADL
00430          SET APPLICABLE TO TRUE                                   ELUCSADL
00431          SET NOT-CONCLUSIVE TO TRUE                               ELUCSADL
00432        END-IF.                                                    ELUCSADL
00433                                                                   ELUCSADL
00434                                                                   ELUCSADL
00435 ************************************************************      ELUCSADL
00436 *                                                          *      ELUCSADL
00437 *        PROCESS INCLUDED                                  *      ELUCSADL
00438 *                                                          *      ELUCSADL
00439 ************************************************************      ELUCSADL
00440  PROCESS-INCLUDED.                                                ELUCSADL
00441      IF WS-BP-ID-FOUND                                            ELUCSADL
00442          SET CONCLUSIVE TO TRUE                                   ELUCSADL
00443          SET APPLICABLE TO TRUE                                   ELUCSADL
00444          SET SPECIFIC TO TRUE                                     ELUCSADL
00445        ELSE                                                       ELUCSADL
00446          SET NOT-APPLICABLE TO TRUE                               ELUCSADL
00447          SET CONCLUSIVE TO TRUE                                   ELUCSADL
00448        END-IF.                                                    ELUCSADL
00449      EJECT                                                        ELUCSADL
00450                                                                   ELUCSADL
00451                                                                   ELUCSADL
00452 ************************************************************      ELUCSADL
00453 *                                                          *      ELUCSADL
00454 *        PROCESS EXCLUDED                                  *      ELUCSADL
00455 *                                                          *      ELUCSADL
00456 ************************************************************      ELUCSADL
00457  PROCESS-EXCLUDED.                                                ELUCSADL
00458      IF WS-BP-ID-FOUND                                            ELUCSADL
00459          SET CONCLUSIVE TO TRUE                                   ELUCSADL
00460          SET NOT-APPLICABLE TO TRUE                               ELUCSADL
00461      ELSE                                                         ELUCSADL
00462          SET APPLICABLE TO TRUE                                   ELUCSADL
00463          SET NOT-CONCLUSIVE TO TRUE                               ELUCSADL
00464      END-IF.                                                      ELUCSADL
00465      EJECT                                                        ELUCSADL
00466                                                                   ELUCSADL
00467                                                                   ELUCSADL
00468 ************************************************************      ELUCSADL
00469 *                                                          *      ELUCSADL
00470 *        SEARCH FOR INTERNAL TABULAR SLOT                  *      ELUCSADL
00471 *                                                          *      ELUCSADL
00472 ************************************************************      ELUCSADL
00473  SEARCH-FOR-INTERNAL-TABULAR-SL.                                  ELUCSADL
00474      SET IBGR-X-IDX TO WS-IBGR-X-SUB.                             ELUCSADL
00475      IF IBGR-SLOT-NUMBER (IBGR-X-IDX) =                           ELUCSADL
00476         ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX)                        ELUCSADL
00477            PERFORM INTERNAL-TABULAR-SLOT-FOUND.                   ELUCSADL
00478                                                                   ELUCSADL
00479                                                                   ELUCSADL
00480 ************************************************************      ELUCSADL
00481 *                                                          *      ELUCSADL
00482 *        INTERNAL TABULAR SLOT FOUND                       *      ELUCSADL
00483 *                                                          *      ELUCSADL
00484 ************************************************************      ELUCSADL
00485  INTERNAL-TABULAR-SLOT-FOUND.                                     ELUCSADL
00486      SET IBGR-FOUND TO TRUE.                                      ELUCSADL
00487      SET ADDRESS OF INTERNAL-TABULAR-RECORD                       ELUCSADL
00488          TO IBGR-TABULAR-PTR (IBGR-X-IDX).                        ELUCSADL
00489      PERFORM SEARCH-FOR-BP-ID.                                    ELUCSADL
00490      IF GX1-ID-ARGUMENT-INCLUDED                                  ELUCSADL
00491          PERFORM PROCESS-INCLUDE-TABULAR                          ELUCSADL
00492      ELSE IF GX1-ID-ARGUMENT-EXCLUDED                             ELUCSADL
00493          PERFORM PROCESS-EXCLUDE-TABULAR                          ELUCSADL
00494      ELSE                                                         ELUCSADL
00495          PERFORM SIGNAL-PROGRAM-LOGIC-ERROR.                      ELUCSADL
00496      EJECT                                                        ELUCSADL
00497                                                                   ELUCSADL
00498                                                                   ELUCSADL
00499 ************************************************************      ELUCSADL
00500 *                                                          *      ELUCSADL
00501 *        SEARCH FOR BP ID                                  *      ELUCSADL
00502 *                                                          *      ELUCSADL
00503 ************************************************************      ELUCSADL
00504  SEARCH-FOR-BP-ID.                                                ELUCSADL
00505      SET WS-BP-ID-NOT-FOUND TO TRUE.                              ELUCSADL
00506      PERFORM SEARCH-FOR-MATCHING-BP-ID                            ELUCSADL
00507          VARYING WS-GX1-SUB FROM 1 BY 1                           ELUCSADL
00508                    UNTIL WS-GX1-SUB > GX1-ENTRY-COUNT             ELUCSADL
00509                       OR WS-BP-ID-FOUND.                          ELUCSADL
00510                                                                   ELUCSADL
00511                                                                   ELUCSADL
00512 ************************************************************      ELUCSADL
00513 *                                                          *      ELUCSADL
00514 *        SEARCH FOR MATCHING BP ID                         *      ELUCSADL
00515 *                                                          *      ELUCSADL
00516 ************************************************************      ELUCSADL
00517  SEARCH-FOR-MATCHING-BP-ID.                                       ELUCSADL
00518      SET GX1-INDEX TO WS-GX1-SUB.                                 ELUCSADL
00519      IF GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) =                   ELUCSADL
00520         CSBP-BP-KEY (CSBP-X-IDX)                                  ELUCSADL
00521              SET WS-BP-ID-FOUND TO TRUE.                          ELUCSADL
00522      EJECT                                                        ELUCSADL
00523                                                                   ELUCSADL
00524                                                                   ELUCSADL
00525 ************************************************************      ELUCSADL
00526 *                                                          *      ELUCSADL
00527 *        PROCESS INCLUDE TABULAR                           *      ELUCSADL
00528 *                                                          *      ELUCSADL
00529 ************************************************************      ELUCSADL
00530  PROCESS-INCLUDE-TABULAR.                                         ELUCSADL
00531      SET TABULAR-INCLUDED TO TRUE.                                ELUCSADL
00532      IF WS-BP-ID-FOUND                                            ELUCSADL
00533         SET WS-BP-QUALIFIED TO TRUE.                              ELUCSADL
00534      EJECT                                                        ELUCSADL
00535                                                                   ELUCSADL
00536                                                                   ELUCSADL
00537 ************************************************************      ELUCSADL
00538 *                                                          *      ELUCSADL
00539 *        PROCESS EXCLUDE TABULAR                           *      ELUCSADL
00540 *                                                          *      ELUCSADL
00541 ************************************************************      ELUCSADL
00542  PROCESS-EXCLUDE-TABULAR.                                         ELUCSADL
00543      SET TABULAR-EXCLUDED TO TRUE.                                ELUCSADL
00544      IF WS-BP-ID-NOT-FOUND                                        ELUCSADL
00545         SET WS-BP-QUALIFIED TO TRUE.                              ELUCSADL
00546      EJECT                                                        ELUCSADL
00547                                                                   ELUCSADL
00548 ************************************************************      ELUCSADL
00549 *                                                          *      ELUCSADL
00550 *        EXAMINE PLACE OF TREATMENT                        *      ELUCSADL
00551 *                                                          *      ELUCSADL
00552 ************************************************************      ELUCSADL
00553  EXAMINE-PLACE-OF-TREATMENT.                                      ELUCSADL
00554      MOVE SPACE TO IP-OP-SELECT-SWT.                              ELUCSADL
00555      SET NOT-APPLICABLE TO TRUE.                                  ELUCSADL
00556      PERFORM SELECT-IP-OP-IND                                     ELUCSADL
00557              VARYING WS-BPL-X-SUB FROM 1 BY 1                     ELUCSADL
00558                              UNTIL IP-OP-IND-SELECTED.            ELUCSADL
00559      IF IN-PATIENT AND                                            ELUCSADL
00560         ATBL-CF-IP (ATBL-X-IDX) > CF-TWO                          ELUCSADL
00561            SET APPLICABLE TO TRUE.                                ELUCSADL
00562                                                                   ELUCSADL
00563      IF OUT-PATIENT AND                                           ELUCSADL
00564         ATBL-CF-OP (ATBL-X-IDX) > CF-TWO                          ELUCSADL
00565            SET APPLICABLE TO TRUE.                                ELUCSADL
00566                                                                   ELUCSADL
00567      IF BOTH-IP-OP                                                ELUCSADL
00568            SET APPLICABLE TO TRUE.                                ELUCSADL
00569                                                                   ELUCSADL
00570  SELECT-IP-OP-IND.                                                ELUCSADL
00571      SET BPL-IDX TO WS-BPL-X-SUB.                                 ELUCSADL
00572      IF BPL-BP-ID (BPL-IDX) = CSBP-BP-KEY (CSBP-X-IDX)            ELUCSADL
00573           MOVE BPL-IP-OP-IND (BPL-IDX) TO IP-OP-IND               ELUCSADL
00574           SET IP-OP-IND-SELECTED TO TRUE.                         ELUCSADL
00575      EJECT                                                        ELUCSADL
00576                                                                   ELUCSADL
00577 ************************************************************      ELUCSADL
00578 *                                                          *      ELUCSADL
00579 *        EXAMINE LINE OF BUSINESS                          *      ELUCSADL
00580 *                                                          *      ELUCSADL
00581 ************************************************************      ELUCSADL
00582  EXAMINE-LINE-OF-BUSINESS.                                        ELUCSADL
00583      SET NOT-APPLICABLE TO TRUE.                                  ELUCSADL
00584      MOVE CSBP-BP-ID-FORMAT (CSBP-X-IDX) TO WS-BP-ID-FORMAT.      ELUCSADL
00585      IF INSTITUTIONAL AND                                         ELUCSADL
00586         ATBL-CF-INST (ATBL-X-IDX) > CF-FIVE                       ELUCSADL
00587            SET APPLICABLE TO TRUE                                 ELUCSADL
00588        ELSE                                                       ELUCSADL
00589        IF PROFESSIONAL AND                                        ELUCSADL
00590           ATBL-CF-PROF (ATBL-X-IDX) > CF-FIVE                     ELUCSADL
00591             SET APPLICABLE TO TRUE                                ELUCSADL
00592       END-IF.                                                     ELUCSADL
00593      EJECT                                                        ELUCSADL
00594                                                                   ELUCSADL
00595                                                                   ELUCSADL
00596 ************************************************************      ELUCSADL
00597 *                                                          *      ELUCSADL
00598 *        CLEAR CSBP ADL DATA                               *      ELUCSADL
00599 *                                                          *      ELUCSADL
00600 ************************************************************      ELUCSADL
00601  CLEAR-CSBP-ADL-DATA.                                             ELUCSADL
00602      MOVE 999 TO PRIORITY-LVL.                                    ELUCSADL
00603      MOVE SPACES TO CSBP-DEDL-OVERALL-SW (CSBP-X-IDX),            ELUCSADL
00604                     CSBP-DEDL-BENEFIT-PERIOD (CSBP-X-IDX),        ELUCSADL
00605                     CSBP-DEDL-L-O-B (CSBP-X-IDX),                 ELUCSADL
00606                     CSBP-DEDL-PLACE-OF-TREATMENT (CSBP-X-IDX),    ELUCSADL
00607                     CSBP-DEDL-VALUE-QUALIFIER (CSBP-X-IDX).       ELUCSADL
00608      MOVE ZEROES TO CSBP-DEDL-IBGR-SLOT (CSBP-X-IDX),             ELUCSADL
00609                     CSBP-DEDL-VALUE-LIMIT (CSBP-X-IDX).           ELUCSADL
00610      EJECT                                                        ELUCSADL
00611                                                                   ELUCSADL
00612                                                                   ELUCSADL
00613 ************************************************************      ELUCSADL
00614 *                                                          *      ELUCSADL
00615 *        EXAMINE CONDITION BITS                            *      ELUCSADL
00616 *                                                          *      ELUCSADL
00617 ************************************************************      ELUCSADL
00618  EXAMINE-CONDITION-BITS.                                          ELUCSADL
00619      SET NOT-APPLICABLE TO TRUE.                                  ELUCSADL
00620      IF CSBP-PSYCHIATRIC-ST                                       ELUCSADL
00621          PERFORM ALL-OR-ICD-BITS-QUERY                            ELUCSADL
00622      ELSE IF CSBP-OB-STERILIZE-ST                                 ELUCSADL
00623          PERFORM PROCESS-OB-PROVN                                 ELUCSADL
00624      ELSE IF NOT CSBP-PSYCHIATRIC-ST OR                           ELUCSADL
00625                 NOT CSBP-OB-STERILIZE-ST                          ELUCSADL
00626          PERFORM DEFAULT-ALL-OR-ICD-BIT-QUERY.                    ELUCSADL
00627                                                                   ELUCSADL
00628                                                                   ELUCSADL
00629 ************************************************************      ELUCSADL
00630 *                                                          *      ELUCSADL
00631 *        ALL OR ICD BITS QUERY                             *      ELUCSADL
00632 *                                                          *      ELUCSADL
00633 ************************************************************      ELUCSADL
00634  ALL-OR-ICD-BITS-QUERY.                                           ELUCSADL
00635      IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = PC-ONE OR                ELUCSADL
00636                 ATBL-COND-ICD-BIT (ATBL-X-IDX) = PC-ONE           ELUCSADL
00637          PERFORM ALL-OR-ICD-BIT-ON-QUERY-EXCLUS                   ELUCSADL
00638      ELSE                                                         ELUCSADL
00639          PERFORM ALL-AND-ICD-BIT-OFF-QUERY-MENT.                  ELUCSADL
00640      EJECT                                                        ELUCSADL
00641                                                                   ELUCSADL
00642                                                                   ELUCSADL
00643 ************************************************************      ELUCSADL
00644 *                                                          *      ELUCSADL
00645 *        PROCESS OB PROVN                                  *      ELUCSADL
00646 *                                                          *      ELUCSADL
00647 ************************************************************      ELUCSADL
00648  PROCESS-OB-PROVN.                                                ELUCSADL
00649      SET NOT-APPLICABLE TO TRUE.                                  ELUCSADL
00650      MOVE CSBP-BP-KEY (CSBP-X-IDX) TO WS-OBSTETRICS.              ELUCSADL
00651      IF OB-NORMAL                                                 ELUCSADL
00652         PERFORM PROCESS-OB-NORM                                   ELUCSADL
00653      ELSE                                                         ELUCSADL
00654         IF OB-COMPLICATED                                         ELUCSADL
00655            PERFORM PROCESS-OB-COMPL                               ELUCSADL
00656         ELSE                                                      ELUCSADL
00657            PERFORM DEFAULT-ALL-OR-ICD-BIT-QUERY                   ELUCSADL
00658         END-IF                                                    ELUCSADL
00659      END-IF.                                                      ELUCSADL
00660      EJECT                                                        ELUCSADL
00661                                                                   ELUCSADL
00662                                                                   ELUCSADL
00663 ************************************************************      ELUCSADL
00664 *                                                          *      ELUCSADL
00665 *        DEFAULT ALL OR ICD BIT QUERY                      *      ELUCSADL
00666 *                                                          *      ELUCSADL
00667 ************************************************************      ELUCSADL
00668  DEFAULT-ALL-OR-ICD-BIT-QUERY.                                    ELUCSADL
00669      IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = PC-ONE OR                ELUCSADL
00670         ATBL-COND-ICD-BIT (ATBL-X-IDX) = PC-ONE                   ELUCSADL
00671         SET APPLICABLE TO TRUE.                                   ELUCSADL
00672      EJECT                                                        ELUCSADL
00673                                                                   ELUCSADL
00674                                                                   ELUCSADL
00675 ************************************************************      ELUCSADL
00676 *                                                          *      ELUCSADL
00677 *        ALL OR ICD BIT ON QUERY EXCLUSION BIT             *      ELUCSADL
00678 *                                                          *      ELUCSADL
00679 ************************************************************      ELUCSADL
00680  ALL-OR-ICD-BIT-ON-QUERY-EXCLUS.                                  ELUCSADL
00681      IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = PC-ONE             ELUCSADL
00682         IF ATBL-COND-MENTAL-BIT (ATBL-X-IDX) = ZERO               ELUCSADL
00683            SET APPLICABLE TO TRUE                                 ELUCSADL
00684         ELSE                                                      ELUCSADL
00685            CONTINUE                                               ELUCSADL
00686      ELSE                                                         ELUCSADL
00687          SET APPLICABLE TO TRUE                                   ELUCSADL
00688      END-IF.                                                      ELUCSADL
00689      EJECT                                                        ELUCSADL
00690                                                                   ELUCSADL
00691                                                                   ELUCSADL
00692 ************************************************************      ELUCSADL
00693 *                                                          *      ELUCSADL
00694 *        ALL AND ICD BIT OFF QUERY MENTAL BIT              *      ELUCSADL
00695 *                                                          *      ELUCSADL
00696 ************************************************************      ELUCSADL
00697  ALL-AND-ICD-BIT-OFF-QUERY-MENT.                                  ELUCSADL
00698      IF ATBL-COND-MENTAL-BIT (ATBL-X-IDX) = PC-ONE                ELUCSADL
00699         SET SPECIFIC TO TRUE                                      ELUCSADL
00700         SET APPLICABLE TO TRUE.                                   ELUCSADL
00701                                                                   ELUCSADL
00702                                                                   ELUCSADL
00703 ************************************************************      ELUCSADL
00704 *                                                          *      ELUCSADL
00705 *        PROCESS OB NORM                                   *      ELUCSADL
00706 *                                                          *      ELUCSADL
00707 ************************************************************      ELUCSADL
00708  PROCESS-OB-NORM.                                                 ELUCSADL
00709      IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = PC-ONE OR                ELUCSADL
00710                  ATBL-COND-ICD-BIT (ATBL-X-IDX) = PC-ONE          ELUCSADL
00711          PERFORM PROCESS-OB-NORM-COND-BIT-ON                      ELUCSADL
00712      ELSE IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = ZERO AND            ELUCSADL
00713                  ATBL-COND-ICD-BIT (ATBL-X-IDX) = ZERO            ELUCSADL
00714          PERFORM PROCESS-OB-NORM-COND-BITS-OFF.                   ELUCSADL
00715      EJECT                                                        ELUCSADL
00716                                                                   ELUCSADL
00717                                                                   ELUCSADL
00718 ************************************************************      ELUCSADL
00719 *                                                          *      ELUCSADL
00720 *        PROCESS OB COMPL                                  *      ELUCSADL
00721 *                                                          *      ELUCSADL
00722 ************************************************************      ELUCSADL
00723  PROCESS-OB-COMPL.                                                ELUCSADL
00724      IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = PC-ONE OR                ELUCSADL
00725                  ATBL-COND-ICD-BIT (ATBL-X-IDX) = PC-ONE          ELUCSADL
00726          PERFORM PROCESS-OB-COMPL-COND-BIT-ON                     ELUCSADL
00727      ELSE IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = ZERO AND            ELUCSADL
00728                  ATBL-COND-ICD-BIT (ATBL-X-IDX) = ZERO            ELUCSADL
00729          PERFORM PROCESS-OB-COMPL-COND-BITS-OFF.                  ELUCSADL
00730      EJECT                                                        ELUCSADL
00731                                                                   ELUCSADL
00732                                                                   ELUCSADL
00733 ************************************************************      ELUCSADL
00734 *                                                          *      ELUCSADL
00735 *        PROCESS OB NORM COND BIT ON                       *      ELUCSADL
00736 *                                                          *      ELUCSADL
00737 ************************************************************      ELUCSADL
00738  PROCESS-OB-NORM-COND-BIT-ON.                                     ELUCSADL
00739      IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = PC-ONE AND         ELUCSADL
00740         ATBL-COND-OB-NORM-BIT (ATBL-X-IDX) = ZERO                 ELUCSADL
00741           SET APPLICABLE TO TRUE                                  ELUCSADL
00742        ELSE                                                       ELUCSADL
00743        IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = ZERO             ELUCSADL
00744           SET APPLICABLE TO TRUE                                  ELUCSADL
00745        END-IF.                                                    ELUCSADL
00746                                                                   ELUCSADL
00747                                                                   ELUCSADL
00748 ************************************************************      ELUCSADL
00749 *                                                          *      ELUCSADL
00750 *        PROCESS OB NORM COND BITS OFF                     *      ELUCSADL
00751 *                                                          *      ELUCSADL
00752 ************************************************************      ELUCSADL
00753  PROCESS-OB-NORM-COND-BITS-OFF.                                   ELUCSADL
00754      IF ATBL-COND-OB-NORM-BIT (ATBL-X-IDX) = PC-ONE               ELUCSADL
00755         SET SPECIFIC TO TRUE                                      ELUCSADL
00756         SET APPLICABLE TO TRUE.                                   ELUCSADL
00757                                                                   ELUCSADL
00758                                                                   ELUCSADL
00759 ************************************************************      ELUCSADL
00760 *                                                          *      ELUCSADL
00761 *        PROCESS OB COMPL COND BIT ON                      *      ELUCSADL
00762 *                                                          *      ELUCSADL
00763 ************************************************************      ELUCSADL
00764  PROCESS-OB-COMPL-COND-BIT-ON.                                    ELUCSADL
00765      IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = PC-ONE AND         ELUCSADL
00766         ATBL-COND-OB-COMP-BIT (ATBL-X-IDX) = ZERO                 ELUCSADL
00767           SET APPLICABLE TO TRUE                                  ELUCSADL
00768        ELSE                                                       ELUCSADL
00769        IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = ZERO             ELUCSADL
00770           SET APPLICABLE TO TRUE                                  ELUCSADL
00771        END-IF.                                                    ELUCSADL
00772                                                                   ELUCSADL
00773                                                                   ELUCSADL
00774 ************************************************************      ELUCSADL
00775 *                                                          *      ELUCSADL
00776 *        PROCESS OB COMPL COND BITS OFF                    *      ELUCSADL
00777 *                                                          *      ELUCSADL
00778 ************************************************************      ELUCSADL
00779  PROCESS-OB-COMPL-COND-BITS-OFF.                                  ELUCSADL
00780      IF ATBL-COND-OB-COMP-BIT (ATBL-X-IDX) = PC-ONE               ELUCSADL
00781         SET SPECIFIC TO TRUE                                      ELUCSADL
00782         SET APPLICABLE TO TRUE.                                   ELUCSADL
00783      EJECT                                                        ELUCSADL
00784                                                                   ELUCSADL
00785                                                                   ELUCSADL
00786 ************************************************************      ELUCSADL
00787 *                                                          *      ELUCSADL
00788 *        EXAMINE CONFIDENCE FACTORS                        *      ELUCSADL
00789 *                                                          *      ELUCSADL
00790 ************************************************************      ELUCSADL
00791  EXAMINE-CONFIDENCE-FACTORS.                                      ELUCSADL
00792      SET NOT-APPLICABLE TO TRUE.                                  ELUCSADL
00793      MOVE CSBP-BP-ID-FORMAT (CSBP-X-IDX) TO WS-BP-ID-FORMAT.      ELUCSADL
00794      IF INSTITUTIONAL AND                                         ELUCSADL
00795         ATBL-CF-INST (ATBL-X-IDX) > CF-TWO                        ELUCSADL
00796           SET APPLICABLE TO TRUE                                  ELUCSADL
00797        ELSE                                                       ELUCSADL
00798        IF PROFESSIONAL AND                                        ELUCSADL
00799           ATBL-CF-PROF (ATBL-X-IDX) > CF-TWO                      ELUCSADL
00800              SET APPLICABLE TO TRUE                               ELUCSADL
00801        END-IF.                                                    ELUCSADL
00802      IF APPLICABLE                                                ELUCSADL
00803          PERFORM EXAMINE-IP-OP-CONFIDENCE-FACTO.                  ELUCSADL
00804      EJECT                                                        ELUCSADL
00805                                                                   ELUCSADL
00806                                                                   ELUCSADL
00807 ************************************************************      ELUCSADL
00808 *                                                          *      ELUCSADL
00809 *        EXAMINE IP OP CONFIDENCE FACTORS                  *      ELUCSADL
00810 *                                                          *      ELUCSADL
00811 ************************************************************      ELUCSADL
00812  EXAMINE-IP-OP-CONFIDENCE-FACTO.                                  ELUCSADL
00813      SET NOT-APPLICABLE TO TRUE.                                  ELUCSADL
00814      IF CSBP-INPATIENT (CSBP-X-IDX) AND                           ELUCSADL
00815         ATBL-CF-IP (ATBL-X-IDX) > CF-TWO                          ELUCSADL
00816           SET APPLICABLE TO TRUE.                                 ELUCSADL
00817      IF CSBP-OUTPATIENT (CSBP-X-IDX) AND                          ELUCSADL
00818         ATBL-CF-OP (ATBL-X-IDX) > CF-TWO                          ELUCSADL
00819           SET APPLICABLE TO TRUE.                                 ELUCSADL
00820      IF CSBP-BOTH (CSBP-X-IDX)                                    ELUCSADL
00821         SET APPLICABLE TO TRUE.                                   ELUCSADL
00822      EJECT                                                        ELUCSADL
00823                                                                   ELUCSADL
00824                                                                   ELUCSADL
00825 ************************************************************      ELUCSADL
00826 *                                                          *      ELUCSADL
00827 *        CHECK FOR ADL OVERALL                             *      ELUCSADL
00828 *                                                          *      ELUCSADL
00829 ************************************************************      ELUCSADL
00830  CHECK-FOR-ADL-OVERALL.                                           ELUCSADL
00831      IF ATBL-CF-OV (ATBL-X-IDX) > CF-FIVE                         ELUCSADL
00832         IF CSBP-DEDL-VALUE-LIMIT (CSBP-X-IDX) > ZERO              ELUCSADL
00833            MOVE 'N' TO CSBP-DEDL-OVERALL-SW (CSBP-X-IDX)          ELUCSADL
00834         ELSE                                                      ELUCSADL
00835            MOVE 'Y' TO CSBP-DEDL-OVERALL-SW (CSBP-X-IDX)          ELUCSADL
00836      ELSE                                                         ELUCSADL
00837         PERFORM SELECT-BENEFIT-PERIOD-PRIORITY                    ELUCSADL
00838      END-IF.                                                      ELUCSADL
00839      EJECT                                                        ELUCSADL
00840                                                                   ELUCSADL
00841                                                                   ELUCSADL
00842 ************************************************************      ELUCSADL
00843 *                                                          *      ELUCSADL
00844 *        SELECT BENEFIT PERIOD PRIORITY                    *      ELUCSADL
00845 *                                                          *      ELUCSADL
00846 ************************************************************      ELUCSADL
00847  SELECT-BENEFIT-PERIOD-PRIORITY.                                  ELUCSADL
00848      IF CSBP-L-O-B (CSBP-X-IDX) = '1'                             ELUCSADL
00849          PERFORM SELECT-BLUE-CROSS-PRIORITY-TAB                   ELUCSADL
00850      ELSE IF CSBP-L-O-B (CSBP-X-IDX) = '2'                        ELUCSADL
00851          PERFORM SELECT-BLUE-SHIELD-PRIORITY-TA                   ELUCSADL
00852      ELSE IF CSBP-L-O-B (CSBP-X-IDX) = '3'                        ELUCSADL
00853          PERFORM SEARCH-TABLE-FIVE                                ELUCSADL
00854      ELSE IF CSBP-L-O-B (CSBP-X-IDX) = '4'                        ELUCSADL
00855          PERFORM SELECT-COMP-MAJ-MED-PRIORITY                     ELUCSADL
00856      END-IF.                                                      ELUCSADL
00857                                                                   ELUCSADL
00858      IF NOT-SPECIFIC                                              ELUCSADL
00859         COMPUTE IN-PRIORITY-LVL = IN-PRIORITY-LVL + 100.          ELUCSADL
00860                                                                   ELUCSADL
00861      IF IN-PRIORITY-LVL < PRIORITY-LVL                            ELUCSADL
00862         PERFORM PROCESS-ADL-INFO.                                 ELUCSADL
00863                                                                   ELUCSADL
00864      MOVE SPACE TO SEARCH-SWITCH.                                 ELUCSADL
00865      EJECT                                                        ELUCSADL
00866                                                                   ELUCSADL
00867                                                                   ELUCSADL
00868 ************************************************************      ELUCSADL
00869 *                                                          *      ELUCSADL
00870 *        SELECT BLUE CROSS PRIORITY TABLE                  *      ELUCSADL
00871 *                                                          *      ELUCSADL
00872 ************************************************************      ELUCSADL
00873  SELECT-BLUE-CROSS-PRIORITY-TAB.                                  ELUCSADL
00874      IF CSBP-INPATIENT (CSBP-X-IDX)                               ELUCSADL
00875          PERFORM SEARCH-TABLE-ONE                                 ELUCSADL
00876      ELSE IF CSBP-OUTPATIENT (CSBP-X-IDX)                         ELUCSADL
00877          PERFORM SEARCH-TABLE-TWO                                 ELUCSADL
00878      ELSE IF CSBP-BOTH (CSBP-X-IDX)                               ELUCSADL
00879          PERFORM SEARCH-TABLE-THREE.                              ELUCSADL
00880      EJECT                                                        ELUCSADL
00881                                                                   ELUCSADL
00882                                                                   ELUCSADL
00883 ************************************************************      ELUCSADL
00884 *                                                          *      ELUCSADL
00885 *        SELECT BLUE SHIELD PRIORITY TABLE                 *      ELUCSADL
00886 *                                                          *      ELUCSADL
00887 ************************************************************      ELUCSADL
00888  SELECT-BLUE-SHIELD-PRIORITY-TA.                                  ELUCSADL
00889      IF CSBP-INPATIENT (CSBP-X-IDX)                               ELUCSADL
00890          PERFORM SEARCH-TABLE-THREE                               ELUCSADL
00891      ELSE IF CSBP-OUTPATIENT (CSBP-X-IDX)                         ELUCSADL
00892          PERFORM SEARCH-TABLE-FOUR                                ELUCSADL
00893      ELSE IF CSBP-BOTH (CSBP-X-IDX)                               ELUCSADL
00894          PERFORM SEARCH-TABLE-THREE.                              ELUCSADL
00895      EJECT                                                        ELUCSADL
00896                                                                   ELUCSADL
00897                                                                   ELUCSADL
00898 ************************************************************      ELUCSADL
00899 *                                                          *      ELUCSADL
00900 *        SELECT COMP MAJ MED PRIORITY                      *      ELUCSADL
00901 *                                                          *      ELUCSADL
00902 ************************************************************      ELUCSADL
00903  SELECT-COMP-MAJ-MED-PRIORITY.                                    ELUCSADL
00904      MOVE CSBP-BP-ID-FORMAT (CSBP-X-IDX) TO WS-BP-ID-FORMAT.      ELUCSADL
00905      IF INSTITUTIONAL                                             ELUCSADL
00906         PERFORM SELECT-BLUE-CROSS-PRIORITY-TAB                    ELUCSADL
00907      ELSE                                                         ELUCSADL
00908      IF PROFESSIONAL                                              ELUCSADL
00909         PERFORM SELECT-BLUE-SHIELD-PRIORITY-TA.                   ELUCSADL
00910                                                                   ELUCSADL
00911                                                                   ELUCSADL
00912 ************************************************************      ELUCSADL
00913 *                                                          *      ELUCSADL
00914 *        SEARCH TABLE ONE                                  *      ELUCSADL
00915 *                                                          *      ELUCSADL
00916 ************************************************************      ELUCSADL
00917  SEARCH-TABLE-ONE.                                                ELUCSADL
00918      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL1                    ELUCSADL
00919           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSADL
00920      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSADL
00921          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSADL
00922                          SEARCH-IS-COMPLETE.                      ELUCSADL
00923                                                                   ELUCSADL
00924                                                                   ELUCSADL
00925 ************************************************************      ELUCSADL
00926 *                                                          *      ELUCSADL
00927 *        SEARCH TABLE TWO                                  *      ELUCSADL
00928 *                                                          *      ELUCSADL
00929 ************************************************************      ELUCSADL
00930  SEARCH-TABLE-TWO.                                                ELUCSADL
00931      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL2                    ELUCSADL
00932           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSADL
00933      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSADL
00934          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSADL
00935                          SEARCH-IS-COMPLETE.                      ELUCSADL
00936      EJECT                                                        ELUCSADL
00937                                                                   ELUCSADL
00938                                                                   ELUCSADL
00939 ************************************************************      ELUCSADL
00940 *                                                          *      ELUCSADL
00941 *        SEARCH TABLE THREE                                *      ELUCSADL
00942 *                                                          *      ELUCSADL
00943 ************************************************************      ELUCSADL
00944  SEARCH-TABLE-THREE.                                              ELUCSADL
00945      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL3                    ELUCSADL
00946           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSADL
00947      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSADL
00948          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSADL
00949                          SEARCH-IS-COMPLETE.                      ELUCSADL
00950                                                                   ELUCSADL
00951                                                                   ELUCSADL
00952 ************************************************************      ELUCSADL
00953 *                                                          *      ELUCSADL
00954 *        SEARCH TABLE FOUR                                 *      ELUCSADL
00955 *                                                          *      ELUCSADL
00956 ************************************************************      ELUCSADL
00957  SEARCH-TABLE-FOUR.                                               ELUCSADL
00958      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL4                    ELUCSADL
00959           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSADL
00960      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSADL
00961          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSADL
00962                          SEARCH-IS-COMPLETE.                      ELUCSADL
00963      EJECT                                                        ELUCSADL
00964                                                                   ELUCSADL
00965                                                                   ELUCSADL
00966 ************************************************************      ELUCSADL
00967 *                                                          *      ELUCSADL
00968 *        SEARCH TABLE FIVE                                 *      ELUCSADL
00969 *                                                          *      ELUCSADL
00970 ************************************************************      ELUCSADL
00971  SEARCH-TABLE-FIVE.                                               ELUCSADL
00972      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL5                    ELUCSADL
00973           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSADL
00974      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSADL
00975          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSADL
00976                          SEARCH-IS-COMPLETE.                      ELUCSADL
00977      EJECT                                                        ELUCSADL
00978                                                                   ELUCSADL
00979                                                                   ELUCSADL
00980 ************************************************************      ELUCSADL
00981 *                                                          *      ELUCSADL
00982 *        SELECT BENEFIT PERIOD                             *      ELUCSADL
00983 *                                                          *      ELUCSADL
00984 ************************************************************      ELUCSADL
00985  SELECT-BENEFIT-PERIOD.                                           ELUCSADL
00986      IF ATBL-BENEFIT-PERIOD (ATBL-X-IDX) =                        ELUCSADL
00987                 BENEFIT-PERIOD (WS-BP-SUB)                        ELUCSADL
00988          PERFORM SET-SEARCH-COMPLETE-SWITCH.                      ELUCSADL
00989      IF WS-BP-SUB > 999                                           ELUCSADL
00990          PERFORM SET-SUBSCRIPT-TO-999.                            ELUCSADL
00991                                                                   ELUCSADL
00992                                                                   ELUCSADL
00993 ************************************************************      ELUCSADL
00994 *                                                          *      ELUCSADL
00995 *        SET SEARCH COMPLETE SWITCH                        *      ELUCSADL
00996 *                                                          *      ELUCSADL
00997 ************************************************************      ELUCSADL
00998  SET-SEARCH-COMPLETE-SWITCH.                                      ELUCSADL
00999      SET SEARCH-IS-COMPLETE TO TRUE.                              ELUCSADL
01000      MOVE WS-BP-SUB TO IN-PRIORITY-LVL.                           ELUCSADL
01001                                                                   ELUCSADL
01002                                                                   ELUCSADL
01003 ************************************************************      ELUCSADL
01004 *                                                          *      ELUCSADL
01005 *        SET SUBSCRIPT TO 999                              *      ELUCSADL
01006 *                                                          *      ELUCSADL
01007 ************************************************************      ELUCSADL
01008  SET-SUBSCRIPT-TO-999.                                            ELUCSADL
01009      MOVE 999 TO WS-BP-SUB.                                       ELUCSADL
01010      PERFORM SET-SEARCH-COMPLETE-SWITCH.                          ELUCSADL
01011      EJECT                                                        ELUCSADL
01012                                                                   ELUCSADL
01013                                                                   ELUCSADL
01014 ************************************************************      ELUCSADL
01015 *                                                          *      ELUCSADL
01016 *        PROCESS ADL INFO                                  *      ELUCSADL
01017 *                                                          *      ELUCSADL
01018 ************************************************************      ELUCSADL
01019  PROCESS-ADL-INFO.                                                ELUCSADL
01020      MOVE IN-PRIORITY-LVL TO PRIORITY-LVL.                        ELUCSADL
01021      PERFORM CAPTURE-DEDL-INFO.                                   ELUCSADL
01022      SET CSBP-ADDITIONAL-ADL-TEXT (CSBP-X-IDX) TO TRUE.           ELUCSADL
01023      EJECT                                                        ELUCSADL
01024                                                                   ELUCSADL
01025                                                                   ELUCSADL
01026 ************************************************************      ELUCSADL
01027 *                                                          *      ELUCSADL
01028 *        CAPTURE DEDL INFO                                 *      ELUCSADL
01029 *                                                          *      ELUCSADL
01030 ************************************************************      ELUCSADL
01031  CAPTURE-DEDL-INFO.                                               ELUCSADL
01032      MOVE 'N' TO CSBP-DEDL-OVERALL-SW (CSBP-X-IDX).               ELUCSADL
01033      MOVE ATBL-IBGR-SLOT-NUMBER        (ATBL-X-IDX)               ELUCSADL
01034        TO CSBP-DEDL-IBGR-SLOT          (CSBP-X-IDX).              ELUCSADL
01035      MOVE ATBL-BENEFIT-PERIOD          (ATBL-X-IDX)               ELUCSADL
01036        TO CSBP-DEDL-BENEFIT-PERIOD     (CSBP-X-IDX).              ELUCSADL
01037      MOVE ATBL-L-O-B                   (ATBL-X-IDX)               ELUCSADL
01038        TO CSBP-DEDL-L-O-B              (CSBP-X-IDX).              ELUCSADL
01039      MOVE ATBL-PLACE-OF-TREATMENT      (ATBL-X-IDX)               ELUCSADL
01040        TO CSBP-DEDL-PLACE-OF-TREATMENT (CSBP-X-IDX).              ELUCSADL
01041      MOVE ATBL-VALUE-LIMIT             (ATBL-X-IDX)               ELUCSADL
01042        TO CSBP-DEDL-VALUE-LIMIT        (CSBP-X-IDX).              ELUCSADL
01043      MOVE ATBL-VALUE-QUALIFIER         (ATBL-X-IDX)               ELUCSADL
01044        TO CSBP-DEDL-VALUE-QUALIFIER    (CSBP-X-IDX).              ELUCSADL
01045      EJECT                                                        ELUCSADL
01046                                                                   ELUCSADL
01047                                                                   ELUCSADL
01048 ************************************************************      ELUCSADL
01049 *                                                          *      ELUCSADL
01050 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELUCSADL
01051 *                                                          *      ELUCSADL
01052 ************************************************************      ELUCSADL
01053  ESTABLISH-ADDRESS-OF-CNTL-BLK.                                   ELUCSADL
01054      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELUCSADL
01055          PERFORM SIGNAL-INVALID-COMMAREA.                         ELUCSADL
01056      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUCSADL
01057          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELUCSADL
01058                                                                   ELUCSADL
01059                                                                   ELUCSADL
01060 ************************************************************      ELUCSADL
01061 *                                                          *      ELUCSADL
01062 *        ESTABLISH ADDRESSABILITY OF POINTER LIST          *      ELUCSADL
01063 *                                                          *      ELUCSADL
01064 ************************************************************      ELUCSADL
01065  ESTABLISH-ADDRESS-OF-PTR-LIST.                                   ELUCSADL
01066      SET  CIA-ELSCSPTC-DDN TO TRUE.                               ELUCSADL
01067      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSADL
01068          ADDRESS OF CSPT-POINTER-LIST.                            ELUCSADL
01069      IF CIA-RC-PTR-NULL                                           ELUCSADL
01070          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSADL
01071      EJECT                                                        ELUCSADL
01072                                                                   ELUCSADL
01073                                                                   ELUCSADL
01074 ************************************************************      ELUCSADL
01075 *                                                          *      ELUCSADL
01076 *        ESTABLISH ADDRESSABILITY OF ACCUMULATOR TABLE     *      ELUCSADL
01077 *                                                          *      ELUCSADL
01078 ************************************************************      ELUCSADL
01079  ESTABLISH-ADDRESS-OF-ACCUM-TBL.                                  ELUCSADL
01080      SET  CIA-ELSCSAC-DDN TO TRUE.                                ELUCSADL
01081      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSADL
01082          ADDRESS OF CSAC-ACCUMULATOR-TABLE.                       ELUCSADL
01083      IF CIA-RC-PTR-NULL                                           ELUCSADL
01084          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSADL
01085                                                                   ELUCSADL
01086                                                                   ELUCSADL
01087 ************************************************************      ELUCSADL
01088 *                                                          *      ELUCSADL
01089 *        ESTABLISH ADDRESSABILITY OF INTERNALS TABLE       *      ELUCSADL
01090 *                                                          *      ELUCSADL
01091 ************************************************************      ELUCSADL
01092  ESTABLISH-ADDRESS-OF-INTRL-TBL.                                  ELUCSADL
01093      SET  CIA-ELSIBGR-DDN TO TRUE.                                ELUCSADL
01094      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSADL
01095          ADDRESS OF IBGR-INTERNAL-TABS-TABLE.                     ELUCSADL
01096                                                                   ELUCSADL
01097                                                                   ELUCSADL
01098 ************************************************************      ELUCSADL
01099 *                                                          *      ELUCSADL
01100 *        ESTABLISH ATBLC ADDRESSABILITY                    *      ELUCSADL
01101 *                                                          *      ELUCSADL
01102 ************************************************************      ELUCSADL
01103  ESTABLISH-ADDRESS-OF-ATBL-TBL.                                   ELUCSADL
01104      SET  CIA-ELSATBL-DDN TO TRUE.                                ELUCSADL
01105      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSADL
01106          ADDRESS OF ATBL-ACCUMULATOR-TABLE.                       ELUCSADL
01107      IF CIA-RC-PTR-NULL                                           ELUCSADL
01108          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSADL
01109      EJECT                                                        ELUCSADL
01110                                                                   ELUCSADL
01111                                                                   ELUCSADL
01112                                                                   ELUCSADL
01113 ************************************************************      ELUCSADL
01114 *                                                          *      ELUCSADL
01115 *        SIGNAL INVALID COMMAREA                           *      ELUCSADL
01116 *                                                          *      ELUCSADL
01117 ************************************************************      ELUCSADL
01118  SIGNAL-INVALID-COMMAREA.                                         ELUCSADL
01119      EXEC CICS ABEND                                              ELUCSADL
01120                ABCODE('EL01')                                     ELUCSADL
01121         END-EXEC.                                                 ELUCSADL
01122                                                                   ELUCSADL
01123                                                                   ELUCSADL
01124 ************************************************************      ELUCSADL
01125 *                                                          *      ELUCSADL
01126 *        SIGNAL UNALLOC IBGR ERROR                         *      ELUCSADL
01127 *                                                          *      ELUCSADL
01128 ************************************************************      ELUCSADL
01129  SIGNAL-UNALLOC-IBGR-ERROR.                                       ELUCSADL
01130      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUCSADL
01131      SET CIA-ELSIBGR-DDN TO TRUE.                                 ELUCSADL
01132      PERFORM SIGNAL-ABEND.                                        ELUCSADL
01133                                                                   ELUCSADL
01134                                                                   ELUCSADL
01135 ************************************************************      ELUCSADL
01136 *                                                          *      ELUCSADL
01137 *        SIGNAL UNALLOC AREA ERROR                         *      ELUCSADL
01138 *                                                          *      ELUCSADL
01139 ************************************************************      ELUCSADL
01140  SIGNAL-UNALLOC-AREA-ERROR.                                       ELUCSADL
01141      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUCSADL
01142      PERFORM SIGNAL-ABEND.                                        ELUCSADL
01143      EJECT                                                        ELUCSADL
01144                                                                   ELUCSADL
01145                                                                   ELUCSADL
01146 ************************************************************      ELUCSADL
01147 *                                                          *      ELUCSADL
01148 *        SIGNAL PROGRAM LOGIC ERROR                        *      ELUCSADL
01149 *                                                          *      ELUCSADL
01150 ************************************************************      ELUCSADL
01151  SIGNAL-PROGRAM-LOGIC-ERROR.                                      ELUCSADL
01152      SET CIA-AB-PGM-LOGIC TO TRUE.                                ELUCSADL
01153      PERFORM SIGNAL-ABEND.                                        ELUCSADL
01154                                                                   ELUCSADL
01155                                                                   ELUCSADL
01156 ************************************************************      ELUCSADL
01157 *                                                          *      ELUCSADL
01158 *        SIGNAL ABEND                                      *      ELUCSADL
01159 *                                                          *      ELUCSADL
01160 ************************************************************      ELUCSADL
01161  SIGNAL-ABEND.                                                    ELUCSADL
01162      EXEC CICS ABEND                                              ELUCSADL
01163                ABCODE(CIA-ABCODE)                                 ELUCSADL
01164         END-EXEC.                                                 ELUCSADL
