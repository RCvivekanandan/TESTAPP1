00001 *      LAST MAINTENANCE TIME:  9.34.53  DATE: 07/28/89            06/29/02
00002 * STRUCTURE(S) MEMBER ELUCSABMPL - LEVEL 219 AS OF 11/23/88       ELUCSABM
00003 * FROM PANLIB R360059.STRUCTPL.PANLIB                                LV001
00004 *   ELS CONTRACT SUMMARY MAXIMUM EXTRACT UTILITY                  ELUCSABM
00005  IDENTIFICATION DIVISION.                                         ELUCSABM
00006                                                                   ELUCSABM
00007  PROGRAM-ID.         ELUCSABM.                                    ELUCSABM
00008                                                                   ELUCSABM
00009  AUTHOR.             GEORGE E MOORE.                              ELUCSABM
00010                                                                   ELUCSABM
00011  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUCSABM
00012                      A MUTUAL LEGAL RESERVE COMPANY               ELUCSABM
00013                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUCSABM
00014                      233 N. MICHIGAN AVE                          ELUCSABM
00015                      CHICAGO, ILLINOIS 60601                      ELUCSABM
00016                                                                   ELUCSABM
00017  DATE-WRITTEN.       28-JULY-1989.                                ELUCSABM
00018                                                                   ELUCSABM
00019  DATE-COMPILED.                                                   ELUCSABM
00020                                                                   ELUCSABM
00021  SECURITY.           COPYRIGHT 1986,                              ELUCSABM
00022                      HEALTH CARE SERVICE CORPORATION              ELUCSABM
00023      SKIP3                                                        ELUCSABM
00024  TITLE 'ELS CONTRACT SUMMARY MAXIMUM EXTRACT UTILITY      '.      ELUCSABM
00025  ENVIRONMENT DIVISION.                                            ELUCSABM
00026                                                                   ELUCSABM
00027  CONFIGURATION SECTION.                                           ELUCSABM
00028  SOURCE-COMPUTER.    IBM-3090.                                    ELUCSABM
00029  OBJECT-COMPUTER.    IBM-3090.                                    ELUCSABM
00030      EJECT                                                        ELUCSABM
00031 ******************************************************************ELUCSABM
00032 *ELUCSABM -- ELS:                                                *ELUCSABM
00033 *                                                                *ELUCSABM
00034 *THE PURPOSE OF ELUCSABM IS TO DETERMINE MAXIMUM VALUES FOR A    *ELUCSABM
00035 *PARTICULAR BENEFIT PROVISION.  IT WILL BE LINKED TO BY ELUCSCOV.*ELUCSABM
00036 *MAXIMUMS MAY BE FOUND AT THE TABLUAR LEVEL, CONTRACT LEVEL OR   *ELUCSABM
00037 *GROUP SPECIFIC LEVEL.  SINCE THERE MAY BE MORE THAN ONE MAXIMUM *ELUCSABM
00038 *ALL LEVELS MUST BE SEARCHED TO MAKE SURE THAT ALL APPROPRIATE   *ELUCSABM
00039 *MAXIMUMS HAVE BEEN LOCATED.                                     *ELUCSABM
00040 *PLEASE NOTE THAT THE 'NORMAL' POSTITION FOR THE LIFETIME MAXIMUM*ELUCSABM
00041 *IS IN THE SECOND OCCURANCE OF MAXIMUMS IN THE ELSCSBP TABLE,    *ELUCSABM
00042 *HOWEVER, WHEN THERE IS ONLY A LIFETIME MAXIMUM, THE LIEFETIME   *ELUCSABM
00043 *MAXIMUM MUST BE MOVED TO THE FIRST OCCURANCE.  THE ABOVE IS     *ELUCSABM
00044 *FOR PROCESSING PSYCHIATRIC BENEFITS.                            *ELUCSABM
00045 *                                                                *ELUCSABM
00046 *THERE IS ALSO SPECIAL PROCESSING FOR OBSTETRICS BUT IT IS ONLY  *ELUCSABM
00047 *RELATED TO PROCESSING IBGRS.                                    *ELUCSABM
00048 ******************************************************************ELUCSABM
00049 *              MAINTAINANCE HISTORY                              *ELUCSABM
00050 *                                                                *ELUCSABM
00051 *   MOD     DATE     BY     ACTION                               *ELUCSABM
00052 *  01.00  89-JUL-89  GEM    CREATED                              *ELUCSABM
00053 *  01.01  30-MAY-91  RKH    UPDATE  - CORRECTED LOGIC ERROR IN   *ELUCSABM
00054 *                           MODULE - EXAMINE-IBGR                *ELUCSABM
00055 *                           MODULE WOULD ABEND IF NO IBGR TAB    *ELUCSABM
00056 *                           WAS CODED IN ACCUM TABULARS/CHANGED  *ELUCSABM
00057 *                           FROM ABEND TO PERFORM THE MODULE     *ELUCSABM
00058 *                           PROCESS-NOT-FOUND-IBGR               *ELUCSABM
00059 *                                                                *ELUCSABM
00060 ******************************************************************ELUCSABM
00061                                                                   ELUCSABM
00062  DATA DIVISION.                                                   ELUCSABM
00063  WORKING-STORAGE SECTION.                                         ELUCSABM
00064  01  WS-HOLD-AREA.                                                ELUCSABM
00065      03  WS-LINE-OF-BUSINESS       OCCURS 2 TIMES                 ELUCSABM
00066                                    PIC X(01).                     ELUCSABM
00067          88  INSTITUTIONAL-LOB              VALUE  '1' '5'        ELUCSABM
00068                                                    '4' '6' '8'.   ELUCSABM
00069          88  PROFESSIONAL-LOB               VALUE  '2' '4'        ELUCSABM
00070                                                    '5' '7' '8'.   ELUCSABM
00071          88  SUPPLEMENTAL-LOB                VALUE '3' '6'        ELUCSABM
00072                                                    '7' '8'.       ELUCSABM
00073 *                                                                 ELUCSABM
00074     03  WS-OBSTETRICS                  PIC X(04).                 ELUCSABM
00075         88  OB-NORMAL                  VALUE 'OBNM' 'OBNS'        ELUCSABM
00076                                              'OBND' 'EABI' 'EABO'.ELUCSABM
00077         88  OB-COMPLICATED             VALUE 'OBCM' 'OBCS'        ELUCSABM
00078                                              'OBCD' 'TABI' 'TABO'.ELUCSABM
00079     03  WS-EMERGENCY                   PIC X(04).                 ELUCSABM
00080         88  EMER-ACCIDENT                    VALUE 'EAER' ' EAC'. ELUCSABM
00081         88  EMER-MEDICAL                     VALUE 'EMER' ' EMC'. ELUCSABM
00082 *                                                                 ELUCSABM
00083  01  WS-SUBSCRIPTS   USAGE COMP SYNC.                             ELUCSABM
00084      05  WS-BP-SUB                PIC S9(4).                      ELUCSABM
00085      05  WS-CSPT-SUB              PIC S9(4).                      ELUCSABM
00086      05  WS-BPL-X-SUB             PIC S9(4).                      ELUCSABM
00087      05  WS-CSBP-X-SUB            PIC S9(4).                      ELUCSABM
00088      05  WS-ATBL-X-SUB            PIC S9(4).                      ELUCSABM
00089      05  WS-IBGR-X-SUB            PIC S9(4).                      ELUCSABM
00090      05  WS-GX1-SUB               PIC S9(4).                      ELUCSABM
00091      05  INSERT-SUB               PIC S9(4).                      ELUCSABM
00092      05  CUR-SUB                  PIC S9(4).                      ELUCSABM
00093      05  PREV-SUB                 PIC S9(4).                      ELUCSABM
00094 *                                                                 ELUCSABM
00095  01  PRIORITY-SUB-SAVE  USAGE COMP SYNC.                          ELUCSABM
00096      05  IN-PRIORITY-LVL          PIC S9(4) VALUE ZERO.           ELUCSABM
00097      05  PRIORITY-LVL             OCCURS 5 TIMES                  ELUCSABM
00098                                   PIC S9(4).                      ELUCSABM
00099 *                                                                 ELUCSABM
00100  01  WS-CSBP-BAMA-INFO-AREA.                                      ELUCSABM
00101      05  WS-CSBP-BAMA-INFO        OCCURS 5 TIMES.                 ELUCSABM
00102          07  WS-CSBP-BAMA-BP             PIC X(02).               ELUCSABM
00103          07  WS-CSBP-BAMA-LOB            PIC X(01).               ELUCSABM
00104          07  WS-CSBP-BAMA-POT            PIC X(02).               ELUCSABM
00105          07  WS-CSBP-BAMA-LMT            PIC S9(7)V99 COMP-3.     ELUCSABM
00106          07  WS-CSBP-BAMA-QUAL           PIC X(01).               ELUCSABM
00107 *                                                                 ELUCSABM
00108  01  WS-SWITCHES.                                                 ELUCSABM
00109      03  WS-APPLICABLE-SWITCH     PIC X(01)  VALUE SPACES.        ELUCSABM
00110          88  APPLICABLE                      VALUE 'Y'.           ELUCSABM
00111          88  NOT-APPLICABLE                  VALUE 'N'.           ELUCSABM
00112      03  WS-IBGR-SWITCH            PIC X(01) VALUE SPACES.        ELUCSABM
00113          88  IBGR-FOUND                      VALUE 'Y'.           ELUCSABM
00114          88  NO-IBGR-FOUND                   VALUE 'N'.           ELUCSABM
00115      03  WS-BP-ID-SWITCH           PIC X(01) VALUE SPACES.        ELUCSABM
00116          88  WS-BP-ID-FOUND                  VALUE 'Y'.           ELUCSABM
00117          88  WS-BP-ID-NOT-FOUND              VALUE 'N'.           ELUCSABM
00118      03  WS-CONCLUSIVE-SWITCH      PIC X(01) VALUE SPACES.        ELUCSABM
00119          88  CONCLUSIVE                      VALUE 'Y'.           ELUCSABM
00120          88  NOT-CONCLUSIVE                  VALUE 'N'.           ELUCSABM
00121      03  WS-QUALIFICATION-STATUS   PIC X(01) VALUE SPACES.        ELUCSABM
00122          88  WS-BP-QUALIFIED                 VALUE 'Y'.           ELUCSABM
00123          88  WS-BP-NOT-QUALIFIED             VALUE 'N'.           ELUCSABM
00124      03  WS-TABULAR-STATUS         PIC X(02) VALUE SPACES.        ELUCSABM
00125          88  TABULAR-INCLUDED                VALUE 'IN'.          ELUCSABM
00126          88  TABULAR-EXCLUDED                VALUE 'EX'.          ELUCSABM
00127      03  WS-LEVEL-STATUS           PIC X(02) VALUE SPACES.        ELUCSABM
00128          88  LEVEL-IS-BP                     VALUE 'BP'.          ELUCSABM
00129          88  LEVEL-IS-GC                     VALUE 'GC'.          ELUCSABM
00130      03  IP-OP-IND                 PIC X(01) VALUE SPACES.        ELUCSABM
00131          88  IN-PATIENT                      VALUE 'I'.           ELUCSABM
00132          88  OUT-PATIENT                     VALUE 'O'.           ELUCSABM
00133          88  BOTH-IP-OP                      VALUE 'B'.           ELUCSABM
00134      03  SEARCH-SWITCH             PIC X(01) VALUE SPACES.        ELUCSABM
00135          88  SEARCH-IS-COMPLETE              VALUE 'Y'.           ELUCSABM
00136      03  WS-BP-ID-FORMAT           PIC X(01) VALUE SPACES.        ELUCSABM
00137          88  INSTITUTIONAL                  VALUE  'A' 'B' 'W'.   ELUCSABM
00138          88  PROFESSIONAL                   VALUE  'C' 'D' 'E'.   ELUCSABM
00139      03  SPECIFIC-SWITCH           PIC X(01) VALUE SPACES.        ELUCSABM
00140          88  SPECIFIC                        VALUE 'Y'.           ELUCSABM
00141          88  NOT-SPECIFIC                    VALUE 'N'.           ELUCSABM
00142      03  IP-OP-SELECT-SWT          PIC X(01) VALUE SPACES.        ELUCSABM
00143          88  IP-OP-IND-SELECTED              VALUE 'Y'.           ELUCSABM
00144      03  PRIORITY-SWT              PIC X(01) VALUE SPACES.        ELUCSABM
00145          88  INSERT-NEW-PRIORITY             VALUE 'Y'.           ELUCSABM
00146          88  NO-MORE-PRIORITIES              VALUE 'N'.           ELUCSABM
00147                                                                   ELUCSABM
00148  01  CONF-FACTORS.                                                ELUCSABM
00149      03  CF-TWO                    COMP-1 VALUE +0.200000E+00.    ELUCSABM
00150      03  CF-FIVE                   COMP-1 VALUE +0.500000E+00.    ELUCSABM
00151 *                                                                 ELUCSABM
00152  01  PROGRAM-CONSTANTS.                                           ELUCSABM
00153      03  PC-ONE                    PIC X(01) VALUE '1'.           ELUCSABM
00154      03  PC-OB                     PIC X(02) VALUE '0B'.          ELUCSABM
00155      03  PC-OC                     PIC X(02) VALUE '0C'.          ELUCSABM
00156      03  PC-OD                     PIC X(02) VALUE '0D'.          ELUCSABM
00157 *                                                                 ELUCSABM
00158      COPY ELSBPTBL.                                               ELUCSABM
00159 *                                                                 ELUCSABM
00160      COPY ELSBPITC.                                               ELUCSABM
00161  LINKAGE SECTION.                                                 ELUCSABM
00162  01  DFHCOMMAREA.                                                 ELUCSABM
00163      COPY ELSCOMMC.                                               ELUCSABM
00164 *                                                                 ELUCSABM
00165      COPY ELSCIA2C.                                               ELUCSABM
00166 /                                                                 ELUCSABM
00167      COPY ELSCSACC.                                               ELUCSABM
00168 /                                                                 ELUCSABM
00169      COPY ELSCSPTC.                                               ELUCSABM
00170 /                                                                 ELUCSABM
00171      COPY ELSIBGRC.                                               ELUCSABM
00172 /                                                                 ELUCSABM
00173      COPY ELSCSBPC.                                               ELUCSABM
00174 /                                                                 ELUCSABM
00175      COPY ELSATBLC.                                               ELUCSABM
00176 /                                                                 ELUCSABM
00177  01  INTERNAL-TABULAR-RECORD.                                     ELUCSABM
00178      COPY GCTIBGRC.                                               ELUCSABM
00179 /                                                                 ELUCSABM
00180  01  BENEFIT-PERIOD-TABLE.                                        ELUCSABM
00181      03  BENEFIT-PERIOD        OCCURS 25 TIMES                    ELUCSABM
00182                                PIC X(02).                         ELUCSABM
00183 /                                                                 ELUCSABM
00184      EJECT                                                        ELUCSABM
00185  PROCEDURE DIVISION.                                              ELUCSABM
00186                                                                   ELUCSABM
00187 ************************************************************      ELUCSABM
00188 *                                                          *      ELUCSABM
00189 *        CONTRACT SUMMARY MAXIMUM                          *      ELUCSABM
00190 *                                                          *      ELUCSABM
00191 ************************************************************      ELUCSABM
00192  CONTRACT-SUMMARY-MAXIMUM.                                        ELUCSABM
00193      PERFORM ESTABLISH-ADDRESS-OF-CNTL-BLK.                       ELUCSABM
00194                                                                   ELUCSABM
00195      PERFORM ESTABLISH-ADDRESS-OF-PTR-LIST.                       ELUCSABM
00196                                                                   ELUCSABM
00197      PERFORM ESTABLISH-ADDRESS-OF-ACCUM-TBL.                      ELUCSABM
00198                                                                   ELUCSABM
00199      PERFORM ESTABLISH-ADDRESS-OF-INTRL-TBL.                      ELUCSABM
00200                                                                   ELUCSABM
00201      PERFORM ESTABLISH-ADDRESS-OF-ATBL-TBL.                       ELUCSABM
00202                                                                   ELUCSABM
00203      PERFORM PROCESS-BENEFIT-PROVISION-PTRS                       ELUCSABM
00204              VARYING WS-CSPT-SUB FROM 1 BY 1                      ELUCSABM
00205                UNTIL WS-CSPT-SUB > CSPT-TBL-CNT.                  ELUCSABM
00206      GOBACK.                                                      ELUCSABM
00207      EJECT                                                        ELUCSABM
00208 ************************************************************      ELUCSABM
00209 *                                                          *      ELUCSABM
00210 *        PROCESS BENEFIT PROVISION PTRS                    *      ELUCSABM
00211 *                                                          *      ELUCSABM
00212 ************************************************************      ELUCSABM
00213  PROCESS-BENEFIT-PROVISION-PTRS.                                  ELUCSABM
00214      SET CSPT-IDX TO WS-CSPT-SUB.                                 ELUCSABM
00215      IF CSPT-BP-TBL-PTR (CSPT-IDX) NOT = NULLS                    ELUCSABM
00216          PERFORM PROCESS-MAXIMUM-PER-BENEFIT-PR.                  ELUCSABM
00217                                                                   ELUCSABM
00218                                                                   ELUCSABM
00219 ************************************************************      ELUCSABM
00220 *                                                          *      ELUCSABM
00221 *        PROCESS MAXIMUM PER BENEFIT PROVISION POINTER     *      ELUCSABM
00222 *                                                          *      ELUCSABM
00223 ************************************************************      ELUCSABM
00224  PROCESS-MAXIMUM-PER-BENEFIT-PR.                                  ELUCSABM
00225      SET ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE TO               ELUCSABM
00226            CSPT-BP-TBL-PTR (CSPT-IDX).                            ELUCSABM
00227      PERFORM EXTRACT-MAX-INFO-FOR-EACH-BENE                       ELUCSABM
00228          VARYING WS-CSBP-X-SUB FROM 1 BY 1 UNTIL                  ELUCSABM
00229                            WS-CSBP-X-SUB  > CSBP-TBL-CNT.         ELUCSABM
00230                                                                   ELUCSABM
00231                                                                   ELUCSABM
00232 ************************************************************      ELUCSABM
00233 *                                                          *      ELUCSABM
00234 *        EXTRACT MAX INFO FOR EACH BENEFIT PROVISION       *      ELUCSABM
00235 *                                                          *      ELUCSABM
00236 ************************************************************      ELUCSABM
00237  EXTRACT-MAX-INFO-FOR-EACH-BENE.                                  ELUCSABM
00238      SET CSBP-X-IDX TO WS-CSBP-X-SUB.                             ELUCSABM
00239      IF NOT CSBP-FORMAT-W (CSBP-X-IDX) AND                        ELUCSABM
00240             CSBP-PROVN-PRICING-METHD (CSBP-X-IDX) > ZERO          ELUCSABM
00241          PERFORM PROCESS-ITEMS-WITH-PROVN-PRICI.                  ELUCSABM
00242      EJECT                                                        ELUCSABM
00243                                                                   ELUCSABM
00244                                                                   ELUCSABM
00245 ************************************************************      ELUCSABM
00246 *                                                          *      ELUCSABM
00247 *        PROCESS ITEMS WITH PROVN PRICING METHD            *      ELUCSABM
00248 *                                                          *      ELUCSABM
00249 ************************************************************      ELUCSABM
00250  PROCESS-ITEMS-WITH-PROVN-PRICI.                                  ELUCSABM
00251      IF CSBP-COVERED (CSBP-X-IDX) AND                             ELUCSABM
00252         CSBP-PAYMENT-REQUESTED (CSBP-X-IDX)                       ELUCSABM
00253            PERFORM EXTRACT-MAXIMUMS-INFORMATION                   ELUCSABM
00254      ELSE IF CSBP-COVERED-ON-SUPP (CSBP-X-IDX) AND                ELUCSABM
00255              CSBP-USE-SUPP-INFO (CSBP-X-IDX)                      ELUCSABM
00256                 PERFORM EXTRACT-MAXIMUMS-INFORMATION.             ELUCSABM
00257                                                                   ELUCSABM
00258                                                                   ELUCSABM
00259 ************************************************************      ELUCSABM
00260 *                                                          *      ELUCSABM
00261 *        EXTRACT MAXIMUMS INFORMATION                      *      ELUCSABM
00262 *                                                          *      ELUCSABM
00263 ************************************************************      ELUCSABM
00264  EXTRACT-MAXIMUMS-INFORMATION.                                    ELUCSABM
00265      PERFORM CLEAR-CSBP-ABM-DATA.                                 ELUCSABM
00266      IF CSAC-ABM-BP-TBL-PTR NOT = NULL  AND                       ELUCSABM
00267         CSBP-BP-ABM-SLOT (CSBP-X-IDX) NOT = ZERO                  ELUCSABM
00268              PERFORM INTERROGATE-BENEFIT-PROVISIONX.              ELUCSABM
00269      IF CSAC-ABM-GC-TBL-PTR NOT = NULL                            ELUCSABM
00270            PERFORM INTERROGATE-GROUP-CONTRACT-LEV.                ELUCSABM
00271      EJECT                                                        ELUCSABM
00272                                                                   ELUCSABM
00273                                                                   ELUCSABM
00274 ************************************************************      ELUCSABM
00275 *                                                          *      ELUCSABM
00276 *        INTERROGATE BENEFIT PROVISION LEVEL               *      ELUCSABM
00277 *                                                          *      ELUCSABM
00278 ************************************************************      ELUCSABM
00279  INTERROGATE-BENEFIT-PROVISIONX.                                  ELUCSABM
00280      SET LEVEL-IS-BP TO TRUE.                                     ELUCSABM
00281      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                        ELUCSABM
00282          TO CSAC-ABM-BP-TBL-PTR.                                  ELUCSABM
00283      MOVE CSBP-L-O-B (CSBP-X-IDX) TO WS-LINE-OF-BUSINESS (1).     ELUCSABM
00284      PERFORM SEARCH-FOR-MATCHING-SLOT                             ELUCSABM
00285          VARYING WS-ATBL-X-SUB FROM 1 BY 1                        ELUCSABM
00286                    UNTIL WS-ATBL-X-SUB > ATBL-TBL-CNT.            ELUCSABM
00287                                                                   ELUCSABM
00288                                                                   ELUCSABM
00289 ************************************************************      ELUCSABM
00290 *                                                          *      ELUCSABM
00291 *        SEARCH FOR MATCHING SLOT                          *      ELUCSABM
00292 *                                                          *      ELUCSABM
00293 ************************************************************      ELUCSABM
00294  SEARCH-FOR-MATCHING-SLOT.                                        ELUCSABM
00295      SET ATBL-X-IDX TO WS-ATBL-X-SUB.                             ELUCSABM
00296      IF ATBL-SLOT-NUMBER (ATBL-X-IDX) =                           ELUCSABM
00297         CSBP-BP-ABM-SLOT (CSBP-X-IDX)                             ELUCSABM
00298            PERFORM PROCESS-MATCHING-SLOT.                         ELUCSABM
00299      EJECT                                                        ELUCSABM
00300                                                                   ELUCSABM
00301                                                                   ELUCSABM
00302 ************************************************************      ELUCSABM
00303 *                                                          *      ELUCSABM
00304 *        INTERROGATE GROUP CONTRACT LEVEL                  *      ELUCSABM
00305 *                                                          *      ELUCSABM
00306 ************************************************************      ELUCSABM
00307  INTERROGATE-GROUP-CONTRACT-LEV.                                  ELUCSABM
00308      SET LEVEL-IS-GC TO TRUE.                                     ELUCSABM
00309      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                        ELUCSABM
00310          TO CSAC-ABM-GC-TBL-PTR.                                  ELUCSABM
00311      MOVE CSBP-L-O-B (CSBP-X-IDX) TO WS-LINE-OF-BUSINESS (1).     ELUCSABM
00312      PERFORM PROCESS-ALL-MATCHING-SLOTS                           ELUCSABM
00313          VARYING WS-ATBL-X-SUB FROM 1 BY 1                        ELUCSABM
00314                    UNTIL WS-ATBL-X-SUB > ATBL-TBL-CNT.            ELUCSABM
00315                                                                   ELUCSABM
00316                                                                   ELUCSABM
00317 ************************************************************      ELUCSABM
00318 *                                                          *      ELUCSABM
00319 *        PROCESS ALL MATCHING SLOTS                        *      ELUCSABM
00320 *                                                          *      ELUCSABM
00321 ************************************************************      ELUCSABM
00322  PROCESS-ALL-MATCHING-SLOTS.                                      ELUCSABM
00323      SET ATBL-X-IDX TO WS-ATBL-X-SUB.                             ELUCSABM
00324      PERFORM PROCESS-MATCHING-SLOT.                               ELUCSABM
00325      EJECT                                                        ELUCSABM
00326                                                                   ELUCSABM
00327                                                                   ELUCSABM
00328 ************************************************************      ELUCSABM
00329 *                                                          *      ELUCSABM
00330 *        PROCESS MATCHING SLOT                             *      ELUCSABM
00331 *                                                          *      ELUCSABM
00332 ************************************************************      ELUCSABM
00333  PROCESS-MATCHING-SLOT.                                           ELUCSABM
00334      SET NOT-SPECIFIC TO TRUE.                                    ELUCSABM
00335      PERFORM EXAMINE-COST-CONTAINMENT-IND.                        ELUCSABM
00336      IF APPLICABLE                                                ELUCSABM
00337          PERFORM EXAMINE-SERVICE-GROUP.                           ELUCSABM
00338      IF APPLICABLE                                                ELUCSABM
00339          PERFORM EXAMINE-COVERAGE.                                ELUCSABM
00340      IF APPLICABLE                                                ELUCSABM
00341          PERFORM EXAMINE-IBGR.                                    ELUCSABM
00342      IF APPLICABLE AND NOT-CONCLUSIVE                             ELUCSABM
00343          PERFORM EXAMINE-PLACE-OF-TREATMENT.                      ELUCSABM
00344      IF APPLICABLE AND NOT-CONCLUSIVE                             ELUCSABM
00345          PERFORM EXAMINE-LINE-OF-BUSINESS.                        ELUCSABM
00346      IF APPLICABLE                                                ELUCSABM
00347          PERFORM EXAMINE-CONDITION-BITS.                          ELUCSABM
00348      IF APPLICABLE                                                ELUCSABM
00349          PERFORM EXAMINE-CONFIDENCE-FACTORS.                      ELUCSABM
00350      IF APPLICABLE                                                ELUCSABM
00351          PERFORM CHECK-FOR-ABM-OVERALL.                           ELUCSABM
00352      EJECT                                                        ELUCSABM
00353                                                                   ELUCSABM
00354                                                                   ELUCSABM
00355 ************************************************************      ELUCSABM
00356 *                                                          *      ELUCSABM
00357 *        EXAMINE COST CONTAINMENT IND                      *      ELUCSABM
00358 *                                                          *      ELUCSABM
00359 ************************************************************      ELUCSABM
00360  EXAMINE-COST-CONTAINMENT-IND.                                    ELUCSABM
00361      IF ATBL-COST-CONTAIN-IND (ATBL-X-IDX) = ZERO                 ELUCSABM
00362          SET APPLICABLE TO TRUE                                   ELUCSABM
00363        ELSE                                                       ELUCSABM
00364          SET NOT-APPLICABLE TO TRUE                               ELUCSABM
00365        END-IF.                                                    ELUCSABM
00366      EJECT                                                        ELUCSABM
00367                                                                   ELUCSABM
00368                                                                   ELUCSABM
00369 ************************************************************      ELUCSABM
00370 *                                                          *      ELUCSABM
00371 *        EXAMINE SERVICE GROUP                             *      ELUCSABM
00372 *                                                          *      ELUCSABM
00373 ************************************************************      ELUCSABM
00374  EXAMINE-SERVICE-GROUP.                                           ELUCSABM
00375      SET NOT-APPLICABLE TO TRUE.                                  ELUCSABM
00376      IF CSBP-OUTPATIENT-ST                                        ELUCSABM
00377          PERFORM INVESTIGATE-OUTPATIENT-CODES.                    ELUCSABM
00378      IF ATBL-SERVICE-GROUP (ATBL-X-IDX) = ZERO                    ELUCSABM
00379         SET APPLICABLE TO TRUE.                                   ELUCSABM
00380                                                                   ELUCSABM
00381                                                                   ELUCSABM
00382 ************************************************************      ELUCSABM
00383 *                                                          *      ELUCSABM
00384 *        INVESTIGATE OUTPATIENT CODES                      *      ELUCSABM
00385 *                                                          *      ELUCSABM
00386 ************************************************************      ELUCSABM
00387  INVESTIGATE-OUTPATIENT-CODES.                                    ELUCSABM
00388      MOVE CSBP-BP-KEY (CSBP-X-IDX) TO WS-EMERGENCY.               ELUCSABM
00389      IF EMER-ACCIDENT AND                                         ELUCSABM
00390         ATBL-SERVICE-GROUP (ATBL-X-IDX) = PC-OC                   ELUCSABM
00391            SET APPLICABLE TO TRUE.                                ELUCSABM
00392      IF EMER-MEDICAL AND                                          ELUCSABM
00393         ATBL-SERVICE-GROUP (ATBL-X-IDX) = PC-OD                   ELUCSABM
00394            SET APPLICABLE TO TRUE.                                ELUCSABM
00395      IF ATBL-SERVICE-GROUP (ATBL-X-IDX) = ZERO                    ELUCSABM
00396            SET APPLICABLE TO TRUE.                                ELUCSABM
00397      EJECT                                                        ELUCSABM
00398                                                                   ELUCSABM
00399                                                                   ELUCSABM
00400 ************************************************************      ELUCSABM
00401 *                                                          *      ELUCSABM
00402 *        EXAMINE COVERAGE                                  *      ELUCSABM
00403 *                                                          *      ELUCSABM
00404 ************************************************************      ELUCSABM
00405  EXAMINE-COVERAGE.                                                ELUCSABM
00406      SET NOT-APPLICABLE TO TRUE.                                  ELUCSABM
00407      IF CSBP-COVERED (CSBP-X-IDX) AND                             ELUCSABM
00408         ATBL-CF-BAS (ATBL-X-IDX) > CF-FIVE                        ELUCSABM
00409            SET APPLICABLE TO TRUE                                 ELUCSABM
00410        ELSE                                                       ELUCSABM
00411        IF CSBP-COVERED-ON-SUPP (CSBP-X-IDX) AND                   ELUCSABM
00412           ATBL-CF-SUP (ATBL-X-IDX) > CF-FIVE                      ELUCSABM
00413              SET APPLICABLE TO TRUE                               ELUCSABM
00414        END-IF.                                                    ELUCSABM
00415      EJECT                                                        ELUCSABM
00416                                                                   ELUCSABM
00417                                                                   ELUCSABM
00418 ************************************************************      ELUCSABM
00419 *                                                          *      ELUCSABM
00420 *        EXAMINE IBGR                                      *      ELUCSABM
00421 *                                                          *      ELUCSABM
00422 ************************************************************      ELUCSABM
00423  EXAMINE-IBGR.                                                    ELUCSABM
00424      SET NOT-APPLICABLE TO TRUE.                                  ELUCSABM
00425      SET NO-IBGR-FOUND TO TRUE.                                   ELUCSABM
00426      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELUCSABM
00427          CONTINUE                                                 ELUCSABM
00428      ELSE                                                         ELUCSABM
00429          PERFORM PROCESS-INTERNAL-TABULAR-SLOT.                   ELUCSABM
00430      IF IBGR-FOUND                                                ELUCSABM
00431          PERFORM PROCESS-FOUND-IBGR                               ELUCSABM
00432      ELSE                                                         ELUCSABM
00433          PERFORM PROCESS-NOT-FOUND-IBGR.                          ELUCSABM
00434                                                                   ELUCSABM
00435                                                                   ELUCSABM
00436 ************************************************************      ELUCSABM
00437 *                                                          *      ELUCSABM
00438 *        PROCESS INTERNAL TABULAR SLOT                     *      ELUCSABM
00439 *                                                          *      ELUCSABM
00440 ************************************************************      ELUCSABM
00441  PROCESS-INTERNAL-TABULAR-SLOT.                                   ELUCSABM
00442      PERFORM SEARCH-FOR-INTERNAL-TABULAR-SL                       ELUCSABM
00443          VARYING WS-IBGR-X-SUB FROM 1 BY 1                        ELUCSABM
00444                     UNTIL WS-IBGR-X-SUB > IBGR-TBL-CNT            ELUCSABM
00445                        OR IBGR-FOUND.                             ELUCSABM
00446      EJECT                                                        ELUCSABM
00447                                                                   ELUCSABM
00448                                                                   ELUCSABM
00449 ************************************************************      ELUCSABM
00450 *                                                          *      ELUCSABM
00451 *        PROCESS FOUND IBGR                                *      ELUCSABM
00452 *                                                          *      ELUCSABM
00453 ************************************************************      ELUCSABM
00454  PROCESS-FOUND-IBGR.                                              ELUCSABM
00455      IF TABULAR-INCLUDED                                          ELUCSABM
00456          PERFORM PROCESS-INCLUDED                                 ELUCSABM
00457      ELSE                                                         ELUCSABM
00458          PERFORM PROCESS-EXCLUDED.                                ELUCSABM
00459      EJECT                                                        ELUCSABM
00460                                                                   ELUCSABM
00461                                                                   ELUCSABM
00462 ************************************************************      ELUCSABM
00463 *                                                          *      ELUCSABM
00464 *        PROCESS NOT FOUND IBGR                            *      ELUCSABM
00465 *                                                          *      ELUCSABM
00466 ************************************************************      ELUCSABM
00467  PROCESS-NOT-FOUND-IBGR.                                          ELUCSABM
00468      IF LEVEL-IS-BP                                               ELUCSABM
00469          SET CONCLUSIVE TO TRUE                                   ELUCSABM
00470          SET APPLICABLE TO TRUE                                   ELUCSABM
00471        ELSE                                                       ELUCSABM
00472          SET APPLICABLE TO TRUE                                   ELUCSABM
00473          SET NOT-CONCLUSIVE TO TRUE                               ELUCSABM
00474        END-IF.                                                    ELUCSABM
00475                                                                   ELUCSABM
00476                                                                   ELUCSABM
00477 ************************************************************      ELUCSABM
00478 *                                                          *      ELUCSABM
00479 *        PROCESS INCLUDED                                  *      ELUCSABM
00480 *                                                          *      ELUCSABM
00481 ************************************************************      ELUCSABM
00482  PROCESS-INCLUDED.                                                ELUCSABM
00483      IF WS-BP-ID-FOUND                                            ELUCSABM
00484          SET CONCLUSIVE TO TRUE                                   ELUCSABM
00485          SET APPLICABLE TO TRUE                                   ELUCSABM
00486          SET SPECIFIC TO TRUE                                     ELUCSABM
00487        ELSE                                                       ELUCSABM
00488          SET NOT-APPLICABLE TO TRUE                               ELUCSABM
00489          SET CONCLUSIVE TO TRUE                                   ELUCSABM
00490        END-IF.                                                    ELUCSABM
00491      EJECT                                                        ELUCSABM
00492                                                                   ELUCSABM
00493                                                                   ELUCSABM
00494 ************************************************************      ELUCSABM
00495 *                                                          *      ELUCSABM
00496 *        PROCESS EXCLUDED                                  *      ELUCSABM
00497 *                                                          *      ELUCSABM
00498 ************************************************************      ELUCSABM
00499  PROCESS-EXCLUDED.                                                ELUCSABM
00500      IF WS-BP-ID-FOUND                                            ELUCSABM
00501          SET CONCLUSIVE TO TRUE                                   ELUCSABM
00502          SET NOT-APPLICABLE TO TRUE                               ELUCSABM
00503      ELSE                                                         ELUCSABM
00504          SET APPLICABLE TO TRUE                                   ELUCSABM
00505          SET NOT-CONCLUSIVE TO TRUE                               ELUCSABM
00506      END-IF.                                                      ELUCSABM
00507      EJECT                                                        ELUCSABM
00508                                                                   ELUCSABM
00509                                                                   ELUCSABM
00510 ************************************************************      ELUCSABM
00511 *                                                          *      ELUCSABM
00512 *        SEARCH FOR INTERNAL TABULAR SLOT                  *      ELUCSABM
00513 *                                                          *      ELUCSABM
00514 ************************************************************      ELUCSABM
00515  SEARCH-FOR-INTERNAL-TABULAR-SL.                                  ELUCSABM
00516      SET IBGR-X-IDX TO WS-IBGR-X-SUB.                             ELUCSABM
00517      IF IBGR-SLOT-NUMBER (IBGR-X-IDX) =                           ELUCSABM
00518         ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX)                        ELUCSABM
00519            PERFORM INTERNAL-TABULAR-SLOT-FOUND.                   ELUCSABM
00520                                                                   ELUCSABM
00521                                                                   ELUCSABM
00522 ************************************************************      ELUCSABM
00523 *                                                          *      ELUCSABM
00524 *        INTERNAL TABULAR SLOT FOUND                       *      ELUCSABM
00525 *                                                          *      ELUCSABM
00526 ************************************************************      ELUCSABM
00527  INTERNAL-TABULAR-SLOT-FOUND.                                     ELUCSABM
00528      SET IBGR-FOUND TO TRUE.                                      ELUCSABM
00529      SET ADDRESS OF INTERNAL-TABULAR-RECORD                       ELUCSABM
00530          TO IBGR-TABULAR-PTR (IBGR-X-IDX).                        ELUCSABM
00531      PERFORM SEARCH-FOR-BP-ID.                                    ELUCSABM
00532      IF GX1-ID-ARGUMENT-INCLUDED                                  ELUCSABM
00533          PERFORM PROCESS-INCLUDE-TABULAR                          ELUCSABM
00534      ELSE IF GX1-ID-ARGUMENT-EXCLUDED                             ELUCSABM
00535          PERFORM PROCESS-EXCLUDE-TABULAR                          ELUCSABM
00536      ELSE                                                         ELUCSABM
00537          PERFORM SIGNAL-PROGRAM-LOGIC-ERROR.                      ELUCSABM
00538      EJECT                                                        ELUCSABM
00539                                                                   ELUCSABM
00540                                                                   ELUCSABM
00541 ************************************************************      ELUCSABM
00542 *                                                          *      ELUCSABM
00543 *        SEARCH FOR BP ID                                  *      ELUCSABM
00544 *                                                          *      ELUCSABM
00545 ************************************************************      ELUCSABM
00546  SEARCH-FOR-BP-ID.                                                ELUCSABM
00547      SET WS-BP-ID-NOT-FOUND TO TRUE.                              ELUCSABM
00548      PERFORM SEARCH-FOR-MATCHING-BP-ID                            ELUCSABM
00549          VARYING WS-GX1-SUB FROM 1 BY 1                           ELUCSABM
00550                    UNTIL WS-GX1-SUB > GX1-ENTRY-COUNT             ELUCSABM
00551                       OR WS-BP-ID-FOUND.                          ELUCSABM
00552                                                                   ELUCSABM
00553                                                                   ELUCSABM
00554 ************************************************************      ELUCSABM
00555 *                                                          *      ELUCSABM
00556 *        SEARCH FOR MATCHING BP ID                         *      ELUCSABM
00557 *                                                          *      ELUCSABM
00558 ************************************************************      ELUCSABM
00559  SEARCH-FOR-MATCHING-BP-ID.                                       ELUCSABM
00560      SET GX1-INDEX TO WS-GX1-SUB.                                 ELUCSABM
00561      IF GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) =                   ELUCSABM
00562         CSBP-BP-KEY (CSBP-X-IDX)                                  ELUCSABM
00563              SET WS-BP-ID-FOUND TO TRUE.                          ELUCSABM
00564      EJECT                                                        ELUCSABM
00565                                                                   ELUCSABM
00566                                                                   ELUCSABM
00567 ************************************************************      ELUCSABM
00568 *                                                          *      ELUCSABM
00569 *        PROCESS INCLUDE TABULAR                           *      ELUCSABM
00570 *                                                          *      ELUCSABM
00571 ************************************************************      ELUCSABM
00572  PROCESS-INCLUDE-TABULAR.                                         ELUCSABM
00573      SET TABULAR-INCLUDED TO TRUE.                                ELUCSABM
00574      IF WS-BP-ID-FOUND                                            ELUCSABM
00575         SET WS-BP-QUALIFIED TO TRUE.                              ELUCSABM
00576      EJECT                                                        ELUCSABM
00577                                                                   ELUCSABM
00578                                                                   ELUCSABM
00579 ************************************************************      ELUCSABM
00580 *                                                          *      ELUCSABM
00581 *        PROCESS EXCLUDE TABULAR                           *      ELUCSABM
00582 *                                                          *      ELUCSABM
00583 ************************************************************      ELUCSABM
00584  PROCESS-EXCLUDE-TABULAR.                                         ELUCSABM
00585      SET TABULAR-EXCLUDED TO TRUE.                                ELUCSABM
00586      IF WS-BP-ID-NOT-FOUND                                        ELUCSABM
00587         SET WS-BP-QUALIFIED TO TRUE.                              ELUCSABM
00588      EJECT                                                        ELUCSABM
00589                                                                   ELUCSABM
00590 ************************************************************      ELUCSABM
00591 *                                                          *      ELUCSABM
00592 *        EXAMINE PLACE OF TREATMENT                        *      ELUCSABM
00593 *                                                          *      ELUCSABM
00594 ************************************************************      ELUCSABM
00595  EXAMINE-PLACE-OF-TREATMENT.                                      ELUCSABM
00596      MOVE SPACE TO IP-OP-SELECT-SWT.                              ELUCSABM
00597      SET NOT-APPLICABLE TO TRUE.                                  ELUCSABM
00598      PERFORM SELECT-IP-OP-IND                                     ELUCSABM
00599              VARYING WS-BPL-X-SUB FROM 1 BY 1                     ELUCSABM
00600                              UNTIL IP-OP-IND-SELECTED.            ELUCSABM
00601      IF IN-PATIENT AND                                            ELUCSABM
00602         ATBL-CF-IP (ATBL-X-IDX) > CF-TWO                          ELUCSABM
00603            SET APPLICABLE TO TRUE.                                ELUCSABM
00604                                                                   ELUCSABM
00605      IF OUT-PATIENT AND                                           ELUCSABM
00606         ATBL-CF-OP (ATBL-X-IDX) > CF-TWO                          ELUCSABM
00607            SET APPLICABLE TO TRUE.                                ELUCSABM
00608                                                                   ELUCSABM
00609      IF BOTH-IP-OP                                                ELUCSABM
00610            SET APPLICABLE TO TRUE.                                ELUCSABM
00611                                                                   ELUCSABM
00612  SELECT-IP-OP-IND.                                                ELUCSABM
00613      SET BPL-IDX TO WS-BPL-X-SUB.                                 ELUCSABM
00614      IF BPL-BP-ID (BPL-IDX) = CSBP-BP-KEY (CSBP-X-IDX)            ELUCSABM
00615           MOVE BPL-IP-OP-IND (BPL-IDX) TO IP-OP-IND               ELUCSABM
00616           SET IP-OP-IND-SELECTED TO TRUE.                         ELUCSABM
00617      EJECT                                                        ELUCSABM
00618                                                                   ELUCSABM
00619 ************************************************************      ELUCSABM
00620 *                                                          *      ELUCSABM
00621 *        EXAMINE LINE OF BUSINESS                          *      ELUCSABM
00622 *                                                          *      ELUCSABM
00623 ************************************************************      ELUCSABM
00624  EXAMINE-LINE-OF-BUSINESS.                                        ELUCSABM
00625      SET NOT-APPLICABLE TO TRUE.                                  ELUCSABM
00626      MOVE CSBP-BP-ID-FORMAT (CSBP-X-IDX) TO WS-BP-ID-FORMAT.      ELUCSABM
00627      IF INSTITUTIONAL AND                                         ELUCSABM
00628         ATBL-CF-INST (ATBL-X-IDX) > CF-FIVE                       ELUCSABM
00629            SET APPLICABLE TO TRUE                                 ELUCSABM
00630        ELSE                                                       ELUCSABM
00631        IF PROFESSIONAL AND                                        ELUCSABM
00632           ATBL-CF-PROF (ATBL-X-IDX) > CF-FIVE                     ELUCSABM
00633             SET APPLICABLE TO TRUE                                ELUCSABM
00634       END-IF.                                                     ELUCSABM
00635      EJECT                                                        ELUCSABM
00636                                                                   ELUCSABM
00637                                                                   ELUCSABM
00638 ************************************************************      ELUCSABM
00639 *                                                          *      ELUCSABM
00640 *        CLEAR CSBP ABM DATA                               *      ELUCSABM
00641 *                                                          *      ELUCSABM
00642 ************************************************************      ELUCSABM
00643  CLEAR-CSBP-ABM-DATA.                                             ELUCSABM
00644      MOVE SPACES TO CSBP-BAMA-OVERALL-SW (CSBP-X-IDX).            ELUCSABM
00645      MOVE ZEROES TO CSBP-BAMA-IBGR-SLOT (CSBP-X-IDX).             ELUCSABM
00646                                                                   ELUCSABM
00647      PERFORM VARYING CSBP-Y-IDX FROM 1 BY 1 UNTIL CSBP-Y-IDX = 6  ELUCSABM
00648         MOVE SPACES TO                                            ELUCSABM
00649              CSBP-BAMA-BENEFIT-PERIOD     (CSBP-X-IDX, CSBP-Y-IDX)ELUCSABM
00650              CSBP-BAMA-L-O-B              (CSBP-X-IDX, CSBP-Y-IDX)ELUCSABM
00651              CSBP-BAMA-PLACE-OF-TREATMENT (CSBP-X-IDX, CSBP-Y-IDX)ELUCSABM
00652              CSBP-BAMA-VALUE-QUALIFIER    (CSBP-X-IDX, CSBP-Y-IDX)ELUCSABM
00653         MOVE ZEROES TO                                            ELUCSABM
00654              CSBP-BAMA-VALUE-LIMIT        (CSBP-X-IDX, CSBP-Y-IDX)ELUCSABM
00655      END-PERFORM.                                                 ELUCSABM
00656                                                                   ELUCSABM
00657      PERFORM VARYING CUR-SUB FROM 1 BY 1 UNTIL CUR-SUB = 6        ELUCSABM
00658         MOVE SPACES TO                                            ELUCSABM
00659              WS-CSBP-BAMA-BP   (CUR-SUB)                          ELUCSABM
00660              WS-CSBP-BAMA-LOB  (CUR-SUB)                          ELUCSABM
00661              WS-CSBP-BAMA-POT  (CUR-SUB)                          ELUCSABM
00662              WS-CSBP-BAMA-QUAL (CUR-SUB)                          ELUCSABM
00663         MOVE ZEROES TO                                            ELUCSABM
00664              WS-CSBP-BAMA-LMT  (CUR-SUB)                          ELUCSABM
00665      END-PERFORM.                                                 ELUCSABM
00666                                                                   ELUCSABM
00667      PERFORM VARYING CUR-SUB FROM 1 BY 1 UNTIL CUR-SUB = 6        ELUCSABM
00668         MOVE ZEROES TO                                            ELUCSABM
00669              PRIORITY-LVL      (CUR-SUB)                          ELUCSABM
00670      END-PERFORM.                                                 ELUCSABM
00671                                                                   ELUCSABM
00672      MOVE 999 TO PRIORITY-LVL (1).                                ELUCSABM
00673      EJECT                                                        ELUCSABM
00674                                                                   ELUCSABM
00675                                                                   ELUCSABM
00676 ************************************************************      ELUCSABM
00677 *                                                          *      ELUCSABM
00678 *        EXAMINE CONDITION BITS                            *      ELUCSABM
00679 *                                                          *      ELUCSABM
00680 ************************************************************      ELUCSABM
00681  EXAMINE-CONDITION-BITS.                                          ELUCSABM
00682      SET NOT-APPLICABLE TO TRUE.                                  ELUCSABM
00683      IF CSBP-PSYCHIATRIC-ST                                       ELUCSABM
00684          PERFORM ALL-OR-ICD-BITS-QUERY                            ELUCSABM
00685      ELSE IF CSBP-OB-STERILIZE-ST                                 ELUCSABM
00686          PERFORM PROCESS-OB-PROVN                                 ELUCSABM
00687      ELSE IF NOT CSBP-PSYCHIATRIC-ST OR                           ELUCSABM
00688                 NOT CSBP-OB-STERILIZE-ST                          ELUCSABM
00689          PERFORM DEFAULT-ALL-OR-ICD-BIT-QUERY.                    ELUCSABM
00690                                                                   ELUCSABM
00691                                                                   ELUCSABM
00692 ************************************************************      ELUCSABM
00693 *                                                          *      ELUCSABM
00694 *        ALL OR ICD BITS QUERY                             *      ELUCSABM
00695 *                                                          *      ELUCSABM
00696 ************************************************************      ELUCSABM
00697  ALL-OR-ICD-BITS-QUERY.                                           ELUCSABM
00698      IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = PC-ONE OR                ELUCSABM
00699                 ATBL-COND-ICD-BIT (ATBL-X-IDX) = PC-ONE           ELUCSABM
00700          PERFORM ALL-OR-ICD-BIT-ON-QUERY-EXCLUS                   ELUCSABM
00701      ELSE                                                         ELUCSABM
00702          PERFORM ALL-AND-ICD-BIT-OFF-QUERY-MENT.                  ELUCSABM
00703      EJECT                                                        ELUCSABM
00704                                                                   ELUCSABM
00705                                                                   ELUCSABM
00706 ************************************************************      ELUCSABM
00707 *                                                          *      ELUCSABM
00708 *        PROCESS OB PROVN                                  *      ELUCSABM
00709 *                                                          *      ELUCSABM
00710 ************************************************************      ELUCSABM
00711  PROCESS-OB-PROVN.                                                ELUCSABM
00712      SET NOT-APPLICABLE TO TRUE.                                  ELUCSABM
00713      MOVE CSBP-BP-KEY (CSBP-X-IDX) TO WS-OBSTETRICS.              ELUCSABM
00714      IF OB-NORMAL                                                 ELUCSABM
00715         PERFORM PROCESS-OB-NORM                                   ELUCSABM
00716      ELSE                                                         ELUCSABM
00717         IF OB-COMPLICATED                                         ELUCSABM
00718            PERFORM PROCESS-OB-COMPL                               ELUCSABM
00719         ELSE                                                      ELUCSABM
00720            PERFORM DEFAULT-ALL-OR-ICD-BIT-QUERY                   ELUCSABM
00721         END-IF                                                    ELUCSABM
00722      END-IF.                                                      ELUCSABM
00723      EJECT                                                        ELUCSABM
00724                                                                   ELUCSABM
00725                                                                   ELUCSABM
00726 ************************************************************      ELUCSABM
00727 *                                                          *      ELUCSABM
00728 *        DEFAULT ALL OR ICD BIT QUERY                      *      ELUCSABM
00729 *                                                          *      ELUCSABM
00730 ************************************************************      ELUCSABM
00731  DEFAULT-ALL-OR-ICD-BIT-QUERY.                                    ELUCSABM
00732      IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = PC-ONE OR                ELUCSABM
00733         ATBL-COND-ICD-BIT (ATBL-X-IDX) = PC-ONE                   ELUCSABM
00734         SET APPLICABLE TO TRUE.                                   ELUCSABM
00735      EJECT                                                        ELUCSABM
00736                                                                   ELUCSABM
00737                                                                   ELUCSABM
00738 ************************************************************      ELUCSABM
00739 *                                                          *      ELUCSABM
00740 *        ALL OR ICD BIT ON QUERY EXCLUSION BIT             *      ELUCSABM
00741 *                                                          *      ELUCSABM
00742 ************************************************************      ELUCSABM
00743  ALL-OR-ICD-BIT-ON-QUERY-EXCLUS.                                  ELUCSABM
00744      IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = PC-ONE             ELUCSABM
00745         IF ATBL-COND-MENTAL-BIT (ATBL-X-IDX) = ZERO               ELUCSABM
00746            SET APPLICABLE TO TRUE                                 ELUCSABM
00747         ELSE                                                      ELUCSABM
00748            CONTINUE                                               ELUCSABM
00749      ELSE                                                         ELUCSABM
00750          SET APPLICABLE TO TRUE                                   ELUCSABM
00751      END-IF.                                                      ELUCSABM
00752      EJECT                                                        ELUCSABM
00753                                                                   ELUCSABM
00754                                                                   ELUCSABM
00755 ************************************************************      ELUCSABM
00756 *                                                          *      ELUCSABM
00757 *        ALL AND ICD BIT OFF QUERY MENTAL BIT              *      ELUCSABM
00758 *                                                          *      ELUCSABM
00759 ************************************************************      ELUCSABM
00760  ALL-AND-ICD-BIT-OFF-QUERY-MENT.                                  ELUCSABM
00761      IF ATBL-COND-MENTAL-BIT (ATBL-X-IDX) = PC-ONE                ELUCSABM
00762         SET SPECIFIC TO TRUE                                      ELUCSABM
00763         SET APPLICABLE TO TRUE.                                   ELUCSABM
00764                                                                   ELUCSABM
00765                                                                   ELUCSABM
00766 ************************************************************      ELUCSABM
00767 *                                                          *      ELUCSABM
00768 *        PROCESS OB NORM                                   *      ELUCSABM
00769 *                                                          *      ELUCSABM
00770 ************************************************************      ELUCSABM
00771  PROCESS-OB-NORM.                                                 ELUCSABM
00772      IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = PC-ONE OR                ELUCSABM
00773                  ATBL-COND-ICD-BIT (ATBL-X-IDX) = PC-ONE          ELUCSABM
00774          PERFORM PROCESS-OB-NORM-COND-BIT-ON                      ELUCSABM
00775      ELSE IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = ZERO AND            ELUCSABM
00776                  ATBL-COND-ICD-BIT (ATBL-X-IDX) = ZERO            ELUCSABM
00777          PERFORM PROCESS-OB-NORM-COND-BITS-OFF.                   ELUCSABM
00778      EJECT                                                        ELUCSABM
00779                                                                   ELUCSABM
00780                                                                   ELUCSABM
00781 ************************************************************      ELUCSABM
00782 *                                                          *      ELUCSABM
00783 *        PROCESS OB COMPL                                  *      ELUCSABM
00784 *                                                          *      ELUCSABM
00785 ************************************************************      ELUCSABM
00786  PROCESS-OB-COMPL.                                                ELUCSABM
00787      IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = PC-ONE OR                ELUCSABM
00788                  ATBL-COND-ICD-BIT (ATBL-X-IDX) = PC-ONE          ELUCSABM
00789          PERFORM PROCESS-OB-COMPL-COND-BIT-ON                     ELUCSABM
00790      ELSE IF ATBL-COND-ALL-BIT (ATBL-X-IDX) = ZERO AND            ELUCSABM
00791                  ATBL-COND-ICD-BIT (ATBL-X-IDX) = ZERO            ELUCSABM
00792          PERFORM PROCESS-OB-COMPL-COND-BITS-OFF.                  ELUCSABM
00793      EJECT                                                        ELUCSABM
00794                                                                   ELUCSABM
00795                                                                   ELUCSABM
00796 ************************************************************      ELUCSABM
00797 *                                                          *      ELUCSABM
00798 *        PROCESS OB NORM COND BIT ON                       *      ELUCSABM
00799 *                                                          *      ELUCSABM
00800 ************************************************************      ELUCSABM
00801  PROCESS-OB-NORM-COND-BIT-ON.                                     ELUCSABM
00802      IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = PC-ONE AND         ELUCSABM
00803         ATBL-COND-OB-NORM-BIT (ATBL-X-IDX) = ZERO                 ELUCSABM
00804           SET APPLICABLE TO TRUE                                  ELUCSABM
00805        ELSE                                                       ELUCSABM
00806        IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = ZERO             ELUCSABM
00807           SET APPLICABLE TO TRUE                                  ELUCSABM
00808        END-IF.                                                    ELUCSABM
00809                                                                   ELUCSABM
00810                                                                   ELUCSABM
00811 ************************************************************      ELUCSABM
00812 *                                                          *      ELUCSABM
00813 *        PROCESS OB NORM COND BITS OFF                     *      ELUCSABM
00814 *                                                          *      ELUCSABM
00815 ************************************************************      ELUCSABM
00816  PROCESS-OB-NORM-COND-BITS-OFF.                                   ELUCSABM
00817      IF ATBL-COND-OB-NORM-BIT (ATBL-X-IDX) = PC-ONE               ELUCSABM
00818         SET SPECIFIC TO TRUE                                      ELUCSABM
00819         SET APPLICABLE TO TRUE.                                   ELUCSABM
00820                                                                   ELUCSABM
00821                                                                   ELUCSABM
00822 ************************************************************      ELUCSABM
00823 *                                                          *      ELUCSABM
00824 *        PROCESS OB COMPL COND BIT ON                      *      ELUCSABM
00825 *                                                          *      ELUCSABM
00826 ************************************************************      ELUCSABM
00827  PROCESS-OB-COMPL-COND-BIT-ON.                                    ELUCSABM
00828      IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = PC-ONE AND         ELUCSABM
00829         ATBL-COND-OB-COMP-BIT (ATBL-X-IDX) = ZERO                 ELUCSABM
00830           SET APPLICABLE TO TRUE                                  ELUCSABM
00831        ELSE                                                       ELUCSABM
00832        IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = ZERO             ELUCSABM
00833           SET APPLICABLE TO TRUE                                  ELUCSABM
00834        END-IF.                                                    ELUCSABM
00835                                                                   ELUCSABM
00836                                                                   ELUCSABM
00837 ************************************************************      ELUCSABM
00838 *                                                          *      ELUCSABM
00839 *        PROCESS OB COMPL COND BITS OFF                    *      ELUCSABM
00840 *                                                          *      ELUCSABM
00841 ************************************************************      ELUCSABM
00842  PROCESS-OB-COMPL-COND-BITS-OFF.                                  ELUCSABM
00843      IF ATBL-COND-OB-COMP-BIT (ATBL-X-IDX) = PC-ONE               ELUCSABM
00844         SET SPECIFIC TO TRUE                                      ELUCSABM
00845         SET APPLICABLE TO TRUE.                                   ELUCSABM
00846      EJECT                                                        ELUCSABM
00847                                                                   ELUCSABM
00848                                                                   ELUCSABM
00849 ************************************************************      ELUCSABM
00850 *                                                          *      ELUCSABM
00851 *        EXAMINE CONFIDENCE FACTORS                        *      ELUCSABM
00852 *                                                          *      ELUCSABM
00853 ************************************************************      ELUCSABM
00854  EXAMINE-CONFIDENCE-FACTORS.                                      ELUCSABM
00855      SET NOT-APPLICABLE TO TRUE.                                  ELUCSABM
00856      MOVE CSBP-BP-ID-FORMAT (CSBP-X-IDX) TO WS-BP-ID-FORMAT.      ELUCSABM
00857      IF INSTITUTIONAL AND                                         ELUCSABM
00858         ATBL-CF-INST (ATBL-X-IDX) > CF-TWO                        ELUCSABM
00859           SET APPLICABLE TO TRUE                                  ELUCSABM
00860        ELSE                                                       ELUCSABM
00861        IF PROFESSIONAL AND                                        ELUCSABM
00862           ATBL-CF-PROF (ATBL-X-IDX) > CF-TWO                      ELUCSABM
00863              SET APPLICABLE TO TRUE                               ELUCSABM
00864        END-IF.                                                    ELUCSABM
00865      IF APPLICABLE                                                ELUCSABM
00866          PERFORM EXAMINE-IP-OP-CONFIDENCE-FACTO.                  ELUCSABM
00867      EJECT                                                        ELUCSABM
00868                                                                   ELUCSABM
00869                                                                   ELUCSABM
00870 ************************************************************      ELUCSABM
00871 *                                                          *      ELUCSABM
00872 *        EXAMINE IP OP CONFIDENCE FACTORS                  *      ELUCSABM
00873 *                                                          *      ELUCSABM
00874 ************************************************************      ELUCSABM
00875  EXAMINE-IP-OP-CONFIDENCE-FACTO.                                  ELUCSABM
00876      SET NOT-APPLICABLE TO TRUE.                                  ELUCSABM
00877      IF CSBP-INPATIENT (CSBP-X-IDX) AND                           ELUCSABM
00878         ATBL-CF-IP (ATBL-X-IDX) > CF-TWO                          ELUCSABM
00879           SET APPLICABLE TO TRUE.                                 ELUCSABM
00880      IF CSBP-OUTPATIENT (CSBP-X-IDX) AND                          ELUCSABM
00881         ATBL-CF-OP (ATBL-X-IDX) > CF-TWO                          ELUCSABM
00882           SET APPLICABLE TO TRUE.                                 ELUCSABM
00883      IF CSBP-BOTH (CSBP-X-IDX)                                    ELUCSABM
00884         SET APPLICABLE TO TRUE.                                   ELUCSABM
00885      EJECT                                                        ELUCSABM
00886                                                                   ELUCSABM
00887                                                                   ELUCSABM
00888 ************************************************************      ELUCSABM
00889 *                                                          *      ELUCSABM
00890 *        CHECK FOR ABM OVERALL                             *      ELUCSABM
00891 *                                                          *      ELUCSABM
00892 ************************************************************      ELUCSABM
00893  CHECK-FOR-ABM-OVERALL.                                           ELUCSABM
00894      IF ATBL-CF-OV (ATBL-X-IDX) > CF-FIVE                         ELUCSABM
00895         IF CSBP-BAMA-VALUE-LIMIT (CSBP-X-IDX, 1) > ZERO           ELUCSABM
00896            MOVE 'N' TO CSBP-BAMA-OVERALL-SW (CSBP-X-IDX)          ELUCSABM
00897         ELSE                                                      ELUCSABM
00898            MOVE 'Y' TO CSBP-BAMA-OVERALL-SW (CSBP-X-IDX)          ELUCSABM
00899      ELSE                                                         ELUCSABM
00900         PERFORM SELECT-BENEFIT-PERIOD-PRIORITY                    ELUCSABM
00901      END-IF.                                                      ELUCSABM
00902      EJECT                                                        ELUCSABM
00903                                                                   ELUCSABM
00904                                                                   ELUCSABM
00905 ************************************************************      ELUCSABM
00906 *                                                          *      ELUCSABM
00907 *        SELECT BENEFIT PERIOD PRIORITY                    *      ELUCSABM
00908 *                                                          *      ELUCSABM
00909 ************************************************************      ELUCSABM
00910  SELECT-BENEFIT-PERIOD-PRIORITY.                                  ELUCSABM
00911      IF CSBP-L-O-B (CSBP-X-IDX) = '1'                             ELUCSABM
00912          PERFORM SELECT-BLUE-CROSS-PRIORITY-TAB                   ELUCSABM
00913      ELSE IF CSBP-L-O-B (CSBP-X-IDX) = '2'                        ELUCSABM
00914          PERFORM SELECT-BLUE-SHIELD-PRIORITY-TA                   ELUCSABM
00915      ELSE IF CSBP-L-O-B (CSBP-X-IDX) = '3'                        ELUCSABM
00916          PERFORM SEARCH-TABLE-FIVE                                ELUCSABM
00917      ELSE IF CSBP-L-O-B (CSBP-X-IDX) = '4'                        ELUCSABM
00918          PERFORM SELECT-COMP-MAJ-MED-PRIORITY                     ELUCSABM
00919      END-IF.                                                      ELUCSABM
00920                                                                   ELUCSABM
00921      IF NOT-SPECIFIC                                              ELUCSABM
00922         COMPUTE IN-PRIORITY-LVL = IN-PRIORITY-LVL + 100.          ELUCSABM
00923                                                                   ELUCSABM
00924      MOVE 1 TO INSERT-SUB.                                        ELUCSABM
00925      PERFORM WITH TEST AFTER                                      ELUCSABM
00926              UNTIL INSERT-NEW-PRIORITY OR NO-MORE-PRIORITIES      ELUCSABM
00927         IF IN-PRIORITY-LVL < PRIORITY-LVL (INSERT-SUB)            ELUCSABM
00928            SET INSERT-NEW-PRIORITY TO TRUE                        ELUCSABM
00929         ELSE                                                      ELUCSABM
00930            IF INSERT-SUB = 6                                      ELUCSABM
00931               SET NO-MORE-PRIORITIES TO TRUE                      ELUCSABM
00932            ELSE                                                   ELUCSABM
00933               ADD 1 TO INSERT-SUB                                 ELUCSABM
00934            END-IF                                                 ELUCSABM
00935         END-IF                                                    ELUCSABM
00936      END-PERFORM.                                                 ELUCSABM
00937                                                                   ELUCSABM
00938      IF INSERT-NEW-PRIORITY                                       ELUCSABM
00939         MOVE 6 TO CUR-SUB, PREV-SUB                               ELUCSABM
00940            PERFORM WITH TEST AFTER                                ELUCSABM
00941               UNTIL CUR-SUB = INSERT-SUB                          ELUCSABM
00942               SUBTRACT 1 FROM PREV-SUB                            ELUCSABM
00943               MOVE WS-CSBP-BAMA-INFO (PREV-SUB)                   ELUCSABM
00944               TO   WS-CSBP-BAMA-INFO (CUR-SUB)                    ELUCSABM
00945               SUBTRACT 1 FROM CUR-SUB                             ELUCSABM
00946            END-PERFORM                                            ELUCSABM
00947         PERFORM CAPTURE-ABM-INFO.                                 ELUCSABM
00948                                                                   ELUCSABM
00949      MOVE SPACE TO SEARCH-SWITCH.                                 ELUCSABM
00950                                                                   ELUCSABM
00951                                                                   ELUCSABM
00952 ************************************************************      ELUCSABM
00953 *                                                          *      ELUCSABM
00954 *        SELECT BLUE CROSS PRIORITY TABLE                  *      ELUCSABM
00955 *                                                          *      ELUCSABM
00956 ************************************************************      ELUCSABM
00957  SELECT-BLUE-CROSS-PRIORITY-TAB.                                  ELUCSABM
00958      IF CSBP-INPATIENT (CSBP-X-IDX)                               ELUCSABM
00959          PERFORM SEARCH-TABLE-ONE                                 ELUCSABM
00960      ELSE IF CSBP-OUTPATIENT (CSBP-X-IDX)                         ELUCSABM
00961          PERFORM SEARCH-TABLE-TWO                                 ELUCSABM
00962      ELSE IF CSBP-BOTH (CSBP-X-IDX)                               ELUCSABM
00963          PERFORM SEARCH-TABLE-THREE.                              ELUCSABM
00964      EJECT                                                        ELUCSABM
00965                                                                   ELUCSABM
00966                                                                   ELUCSABM
00967 ************************************************************      ELUCSABM
00968 *                                                          *      ELUCSABM
00969 *        SELECT BLUE SHIELD PRIORITY TABLE                 *      ELUCSABM
00970 *                                                          *      ELUCSABM
00971 ************************************************************      ELUCSABM
00972  SELECT-BLUE-SHIELD-PRIORITY-TA.                                  ELUCSABM
00973      IF CSBP-INPATIENT (CSBP-X-IDX)                               ELUCSABM
00974          PERFORM SEARCH-TABLE-THREE                               ELUCSABM
00975      ELSE IF CSBP-OUTPATIENT (CSBP-X-IDX)                         ELUCSABM
00976          PERFORM SEARCH-TABLE-FOUR                                ELUCSABM
00977      ELSE IF CSBP-BOTH (CSBP-X-IDX)                               ELUCSABM
00978          PERFORM SEARCH-TABLE-THREE.                              ELUCSABM
00979      EJECT                                                        ELUCSABM
00980                                                                   ELUCSABM
00981                                                                   ELUCSABM
00982 ************************************************************      ELUCSABM
00983 *                                                          *      ELUCSABM
00984 *        SELECT COMP MAJ MED PRIORITY                      *      ELUCSABM
00985 *                                                          *      ELUCSABM
00986 ************************************************************      ELUCSABM
00987  SELECT-COMP-MAJ-MED-PRIORITY.                                    ELUCSABM
00988      MOVE CSBP-BP-ID-FORMAT (CSBP-X-IDX) TO WS-BP-ID-FORMAT.      ELUCSABM
00989      IF INSTITUTIONAL                                             ELUCSABM
00990         PERFORM SELECT-BLUE-CROSS-PRIORITY-TAB                    ELUCSABM
00991      ELSE                                                         ELUCSABM
00992      IF PROFESSIONAL                                              ELUCSABM
00993         PERFORM SELECT-BLUE-SHIELD-PRIORITY-TA.                   ELUCSABM
00994                                                                   ELUCSABM
00995                                                                   ELUCSABM
00996 ************************************************************      ELUCSABM
00997 *                                                          *      ELUCSABM
00998 *        SEARCH TABLE ONE                                  *      ELUCSABM
00999 *                                                          *      ELUCSABM
01000 ************************************************************      ELUCSABM
01001  SEARCH-TABLE-ONE.                                                ELUCSABM
01002      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL1                    ELUCSABM
01003           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSABM
01004      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSABM
01005          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSABM
01006                          SEARCH-IS-COMPLETE.                      ELUCSABM
01007                                                                   ELUCSABM
01008                                                                   ELUCSABM
01009 ************************************************************      ELUCSABM
01010 *                                                          *      ELUCSABM
01011 *        SEARCH TABLE TWO                                  *      ELUCSABM
01012 *                                                          *      ELUCSABM
01013 ************************************************************      ELUCSABM
01014  SEARCH-TABLE-TWO.                                                ELUCSABM
01015      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL2                    ELUCSABM
01016           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSABM
01017      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSABM
01018          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSABM
01019                          SEARCH-IS-COMPLETE.                      ELUCSABM
01020      EJECT                                                        ELUCSABM
01021                                                                   ELUCSABM
01022                                                                   ELUCSABM
01023 ************************************************************      ELUCSABM
01024 *                                                          *      ELUCSABM
01025 *        SEARCH TABLE THREE                                *      ELUCSABM
01026 *                                                          *      ELUCSABM
01027 ************************************************************      ELUCSABM
01028  SEARCH-TABLE-THREE.                                              ELUCSABM
01029      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL3                    ELUCSABM
01030           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSABM
01031      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSABM
01032          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSABM
01033                          SEARCH-IS-COMPLETE.                      ELUCSABM
01034                                                                   ELUCSABM
01035                                                                   ELUCSABM
01036 ************************************************************      ELUCSABM
01037 *                                                          *      ELUCSABM
01038 *        SEARCH TABLE FOUR                                 *      ELUCSABM
01039 *                                                          *      ELUCSABM
01040 ************************************************************      ELUCSABM
01041  SEARCH-TABLE-FOUR.                                               ELUCSABM
01042      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL4                    ELUCSABM
01043           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSABM
01044      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSABM
01045          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSABM
01046                          SEARCH-IS-COMPLETE.                      ELUCSABM
01047      EJECT                                                        ELUCSABM
01048                                                                   ELUCSABM
01049                                                                   ELUCSABM
01050 ************************************************************      ELUCSABM
01051 *                                                          *      ELUCSABM
01052 *        SEARCH TABLE FIVE                                 *      ELUCSABM
01053 *                                                          *      ELUCSABM
01054 ************************************************************      ELUCSABM
01055  SEARCH-TABLE-FIVE.                                               ELUCSABM
01056      CALL 'ELUADDRS' USING WS-BEN-PER-IND-TBL5                    ELUCSABM
01057           ADDRESS OF BENEFIT-PERIOD-TABLE.                        ELUCSABM
01058      PERFORM SELECT-BENEFIT-PERIOD                                ELUCSABM
01059          VARYING WS-BP-SUB FROM 1 BY 1 UNTIL                      ELUCSABM
01060                          SEARCH-IS-COMPLETE.                      ELUCSABM
01061      EJECT                                                        ELUCSABM
01062                                                                   ELUCSABM
01063                                                                   ELUCSABM
01064 ************************************************************      ELUCSABM
01065 *                                                          *      ELUCSABM
01066 *        SELECT BENEFIT PERIOD                             *      ELUCSABM
01067 *                                                          *      ELUCSABM
01068 ************************************************************      ELUCSABM
01069  SELECT-BENEFIT-PERIOD.                                           ELUCSABM
01070      IF ATBL-BENEFIT-PERIOD (ATBL-X-IDX) =                        ELUCSABM
01071                 BENEFIT-PERIOD (WS-BP-SUB)                        ELUCSABM
01072          PERFORM SET-SEARCH-COMPLETE-SWITCH.                      ELUCSABM
01073      IF WS-BP-SUB > 999                                           ELUCSABM
01074          PERFORM SET-SUBSCRIPT-TO-999.                            ELUCSABM
01075                                                                   ELUCSABM
01076                                                                   ELUCSABM
01077 ************************************************************      ELUCSABM
01078 *                                                          *      ELUCSABM
01079 *        SET SEARCH COMPLETE SWITCH                        *      ELUCSABM
01080 *                                                          *      ELUCSABM
01081 ************************************************************      ELUCSABM
01082  SET-SEARCH-COMPLETE-SWITCH.                                      ELUCSABM
01083      SET SEARCH-IS-COMPLETE TO TRUE.                              ELUCSABM
01084      MOVE WS-BP-SUB TO IN-PRIORITY-LVL.                           ELUCSABM
01085                                                                   ELUCSABM
01086                                                                   ELUCSABM
01087 ************************************************************      ELUCSABM
01088 *                                                          *      ELUCSABM
01089 *        SET SUBSCRIPT TO 999                              *      ELUCSABM
01090 *                                                          *      ELUCSABM
01091 ************************************************************      ELUCSABM
01092  SET-SUBSCRIPT-TO-999.                                            ELUCSABM
01093      MOVE 999 TO WS-BP-SUB.                                       ELUCSABM
01094      PERFORM SET-SEARCH-COMPLETE-SWITCH.                          ELUCSABM
01095      EJECT                                                        ELUCSABM
01096                                                                   ELUCSABM
01097                                                                   ELUCSABM
01098 ************************************************************      ELUCSABM
01099 *                                                          *      ELUCSABM
01100 *        CAPTURE ABM INFO                                  *      ELUCSABM
01101 *                                                          *      ELUCSABM
01102 ************************************************************      ELUCSABM
01103  CAPTURE-ABM-INFO.                                                ELUCSABM
01104      SET CSBP-ADDITIONAL-ABM-TEXT (CSBP-X-IDX) TO TRUE.           ELUCSABM
01105      MOVE 'N' TO CSBP-BAMA-OVERALL-SW (CSBP-X-IDX).               ELUCSABM
01106      MOVE ATBL-IBGR-SLOT-NUMBER        (ATBL-X-IDX)               ELUCSABM
01107        TO CSBP-BAMA-IBGR-SLOT          (CSBP-X-IDX).              ELUCSABM
01108      MOVE ATBL-BENEFIT-PERIOD (ATBL-X-IDX)                        ELUCSABM
01109        TO WS-CSBP-BAMA-BP (INSERT-SUB).                           ELUCSABM
01110      MOVE ATBL-L-O-B (ATBL-X-IDX)                                 ELUCSABM
01111        TO WS-CSBP-BAMA-LOB (INSERT-SUB).                          ELUCSABM
01112      MOVE ATBL-PLACE-OF-TREATMENT (ATBL-X-IDX)                    ELUCSABM
01113        TO WS-CSBP-BAMA-POT (INSERT-SUB).                          ELUCSABM
01114      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)                           ELUCSABM
01115        TO WS-CSBP-BAMA-LMT (INSERT-SUB).                          ELUCSABM
01116      MOVE ATBL-VALUE-QUALIFIER (ATBL-X-IDX)                       ELUCSABM
01117        TO WS-CSBP-BAMA-QUAL (INSERT-SUB).                         ELUCSABM
01118      SET CSBP-Y-IDX TO 1.                                         ELUCSABM
01119      PERFORM VARYING CUR-SUB FROM 1 BY 1 UNTIL CUR-SUB = 6        ELUCSABM
01120         MOVE WS-CSBP-BAMA-BP (CUR-SUB)                            ELUCSABM
01121          TO CSBP-BAMA-BENEFIT-PERIOD  (CSBP-X-IDX CSBP-Y-IDX)     ELUCSABM
01122         MOVE WS-CSBP-BAMA-LOB (CUR-SUB)                           ELUCSABM
01123          TO CSBP-BAMA-L-O-B           (CSBP-X-IDX CSBP-Y-IDX)     ELUCSABM
01124         MOVE WS-CSBP-BAMA-POT (CUR-SUB)                           ELUCSABM
01125          TO CSBP-BAMA-PLACE-OF-TREATMENT (CSBP-X-IDX CSBP-Y-IDX)  ELUCSABM
01126         MOVE WS-CSBP-BAMA-LMT (CUR-SUB)                           ELUCSABM
01127          TO CSBP-BAMA-VALUE-LIMIT     (CSBP-X-IDX CSBP-Y-IDX)     ELUCSABM
01128         MOVE WS-CSBP-BAMA-QUAL (CUR-SUB)                          ELUCSABM
01129          TO CSBP-BAMA-VALUE-QUALIFIER (CSBP-X-IDX CSBP-Y-IDX)     ELUCSABM
01130         SET CSBP-Y-IDX UP BY 1                                    ELUCSABM
01131      END-PERFORM.                                                 ELUCSABM
01132      EJECT                                                        ELUCSABM
01133                                                                   ELUCSABM
01134                                                                   ELUCSABM
01135 ************************************************************      ELUCSABM
01136 *                                                          *      ELUCSABM
01137 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELUCSABM
01138 *                                                          *      ELUCSABM
01139 ************************************************************      ELUCSABM
01140  ESTABLISH-ADDRESS-OF-CNTL-BLK.                                   ELUCSABM
01141      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELUCSABM
01142          PERFORM SIGNAL-INVALID-COMMAREA.                         ELUCSABM
01143      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUCSABM
01144          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELUCSABM
01145                                                                   ELUCSABM
01146                                                                   ELUCSABM
01147 ************************************************************      ELUCSABM
01148 *                                                          *      ELUCSABM
01149 *        ESTABLISH ADDRESSABILITY OF POINTER LIST          *      ELUCSABM
01150 *                                                          *      ELUCSABM
01151 ************************************************************      ELUCSABM
01152  ESTABLISH-ADDRESS-OF-PTR-LIST.                                   ELUCSABM
01153      SET  CIA-ELSCSPTC-DDN TO TRUE.                               ELUCSABM
01154      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSABM
01155          ADDRESS OF CSPT-POINTER-LIST.                            ELUCSABM
01156      IF CIA-RC-PTR-NULL                                           ELUCSABM
01157          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSABM
01158      EJECT                                                        ELUCSABM
01159                                                                   ELUCSABM
01160                                                                   ELUCSABM
01161 ************************************************************      ELUCSABM
01162 *                                                          *      ELUCSABM
01163 *        ESTABLISH ADDRESSABILITY OF ACCUMULATOR TABLE     *      ELUCSABM
01164 *                                                          *      ELUCSABM
01165 ************************************************************      ELUCSABM
01166  ESTABLISH-ADDRESS-OF-ACCUM-TBL.                                  ELUCSABM
01167      SET  CIA-ELSCSAC-DDN TO TRUE.                                ELUCSABM
01168      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSABM
01169          ADDRESS OF CSAC-ACCUMULATOR-TABLE.                       ELUCSABM
01170      IF CIA-RC-PTR-NULL                                           ELUCSABM
01171          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSABM
01172                                                                   ELUCSABM
01173                                                                   ELUCSABM
01174 ************************************************************      ELUCSABM
01175 *                                                          *      ELUCSABM
01176 *        ESTABLISH ADDRESSABILITY OF INTERNALS TABLE       *      ELUCSABM
01177 *                                                          *      ELUCSABM
01178 ************************************************************      ELUCSABM
01179  ESTABLISH-ADDRESS-OF-INTRL-TBL.                                  ELUCSABM
01180      SET  CIA-ELSIBGR-DDN TO TRUE.                                ELUCSABM
01181      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSABM
01182          ADDRESS OF IBGR-INTERNAL-TABS-TABLE.                     ELUCSABM
01183                                                                   ELUCSABM
01184                                                                   ELUCSABM
01185 ************************************************************      ELUCSABM
01186 *                                                          *      ELUCSABM
01187 *        ESTABLISH ATBLC ADDRESSABILITY                    *      ELUCSABM
01188 *                                                          *      ELUCSABM
01189 ************************************************************      ELUCSABM
01190  ESTABLISH-ADDRESS-OF-ATBL-TBL.                                   ELUCSABM
01191      SET  CIA-ELSATBL-DDN TO TRUE.                                ELUCSABM
01192      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSABM
01193          ADDRESS OF ATBL-ACCUMULATOR-TABLE.                       ELUCSABM
01194      IF CIA-RC-PTR-NULL                                           ELUCSABM
01195          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSABM
01196      EJECT                                                        ELUCSABM
01197                                                                   ELUCSABM
01198                                                                   ELUCSABM
01199                                                                   ELUCSABM
01200 ************************************************************      ELUCSABM
01201 *                                                          *      ELUCSABM
01202 *        SIGNAL INVALID COMMAREA                           *      ELUCSABM
01203 *                                                          *      ELUCSABM
01204 ************************************************************      ELUCSABM
01205  SIGNAL-INVALID-COMMAREA.                                         ELUCSABM
01206      EXEC CICS ABEND                                              ELUCSABM
01207                ABCODE('EL01')                                     ELUCSABM
01208         END-EXEC.                                                 ELUCSABM
01209                                                                   ELUCSABM
01210                                                                   ELUCSABM
01211 ************************************************************      ELUCSABM
01212 *                                                          *      ELUCSABM
01213 *        SIGNAL UNALLOC IBGR ERROR                         *      ELUCSABM
01214 *                                                          *      ELUCSABM
01215 ************************************************************      ELUCSABM
01216  SIGNAL-UNALLOC-IBGR-ERROR.                                       ELUCSABM
01217      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUCSABM
01218      SET CIA-ELSIBGR-DDN TO TRUE.                                 ELUCSABM
01219      PERFORM SIGNAL-ABEND.                                        ELUCSABM
01220                                                                   ELUCSABM
01221                                                                   ELUCSABM
01222 ************************************************************      ELUCSABM
01223 *                                                          *      ELUCSABM
01224 *        SIGNAL UNALLOC AREA ERROR                         *      ELUCSABM
01225 *                                                          *      ELUCSABM
01226 ************************************************************      ELUCSABM
01227  SIGNAL-UNALLOC-AREA-ERROR.                                       ELUCSABM
01228      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUCSABM
01229      PERFORM SIGNAL-ABEND.                                        ELUCSABM
01230      EJECT                                                        ELUCSABM
01231                                                                   ELUCSABM
01232                                                                   ELUCSABM
01233 ************************************************************      ELUCSABM
01234 *                                                          *      ELUCSABM
01235 *        SIGNAL PROGRAM LOGIC ERROR                        *      ELUCSABM
01236 *                                                          *      ELUCSABM
01237 ************************************************************      ELUCSABM
01238  SIGNAL-PROGRAM-LOGIC-ERROR.                                      ELUCSABM
01239      SET CIA-AB-PGM-LOGIC TO TRUE.                                ELUCSABM
01240      PERFORM SIGNAL-ABEND.                                        ELUCSABM
01241                                                                   ELUCSABM
01242                                                                   ELUCSABM
01243 ************************************************************      ELUCSABM
01244 *                                                          *      ELUCSABM
01245 *        SIGNAL ABEND                                      *      ELUCSABM
01246 *                                                          *      ELUCSABM
01247 ************************************************************      ELUCSABM
01248  SIGNAL-ABEND.                                                    ELUCSABM
01249      EXEC CICS ABEND                                              ELUCSABM
01250                ABCODE(CIA-ABCODE)                                 ELUCSABM
01251         END-EXEC.                                                 ELUCSABM
