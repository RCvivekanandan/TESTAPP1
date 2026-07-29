00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELTPOS  
00003  PROGRAM-ID.         ELTPOS.                                         LV001
00004                                                                   ELTPOS  
00005  AUTHOR.             ANNE KEFFER KING.                            ELTPOS  
00006                                                                   ELTPOS  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTPOS  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTPOS  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTPOS  
00010                      233 N. MICHIGAN AVE                          ELTPOS  
00011                      CHICAGO, ILLINOIS 60601                      ELTPOS  
00012                                                                   ELTPOS  
00013  DATE-WRITTEN.       22-MAY-1991.                                 ELTPOS  
00014                                                                   ELTPOS  
00015  DATE-COMPILED.                                                   ELTPOS  
00016                                                                   ELTPOS  
00017  SECURITY.           COPYRIGHT 1986,                              ELTPOS  
00018                      HEALTH CARE SERVICE CORPORATION              ELTPOS  
00019 ******************************************************************ELTPOS  
00020 *  RECORDS                                                       *ELTPOS  
00021 *  ACCESSED: ELCDCIA RECORD                                      *ELTPOS  
00022 *             GROUP SPECIFIC RECORD                              *ELTPOS  
00023 *             #GCCP TABULAR RECORD                               *ELTPOS  
00024 *                                                                *ELTPOS  
00025 *  PROCESSING                                                    *ELTPOS  
00026 *  FUNCTIONS: THIS MODULE PERFORMS THE FOLLOWING FUNCTIONS:      *ELTPOS  
00027 *                                                                *ELTPOS  
00028 *              1. INITIALIZES WORK DATA ELEMENTS.                *ELTPOS  
00029 *                                                                *ELTPOS  
00030 *              2. ACQUIRE NECESSARY RECORDS FOR PROCESSING.      *ELTPOS  
00031 *                                                                *ELTPOS  
00032 *              3. PROCESS POINT OF SERVICE PROGRAM TABULAR       *ELTPOS  
00033 *                 RECORD FOR BLUE CROSS PRODUCING OUTPUT         *ELTPOS  
00034 *                 TEXT AS REQUIRED.                              *ELTPOS  
00035 *                                                                *ELTPOS  
00036 *              4. PROCESS POINT OF SERVICE PROGRAM TABULAR       *ELTPOS  
00037 *                 RECORD FOR BLUE SHIELD PRODUCING OUTPUT        *ELTPOS  
00038 *                 TEXT AS REQUIRED.                              *ELTPOS  
00039 *                                                                *ELTPOS  
00040 *              5. PROCESS POINT OF SERVICE PROGRAM TABULAR       *ELTPOS  
00041 *                 RECORD FOR MAJOR MEDICAL PRODUCING OUTPUT      *ELTPOS  
00042 *                 TEXT AS REQUIRED.                              *ELTPOS  
00043 ******************************************************************ELTPOS  
00044 *                U P D A T E  L O G                              *ELTPOS  
00045 *                                                                *ELTPOS  
00046 *  MOD      DATE      WHO        DESCRIPTION                     *ELTPOS  
00047 *                                                                 ELTPOS  
00048 *  1.00     05/21/91  AKK        CREATED ISSR 11836               ELTPOS  
00049 *                                                                 ELTPOS  
00050 *                                                                 ELTPOS  
00051 *  1.01     03/18/92  BAK        ADD CODE TO PROCESS #GMCS-       ELTPOS  
00052 *                                SPECIAL SERVICES TABULAR.        ELTPOS  
00053 *                                ALSO REMOVE PERFORMS IN THE      ELTPOS  
00054 *                                GENERATE-HEADINGS ROUTINE AND    ELTPOS  
00055 *                                REPLACE WITH HEADING MOVES.      ELTPOS  
00056 ***************************************************************** ELTPOS  
00057      SKIP3                                                        ELTPOS  
00058  ENVIRONMENT DIVISION.                                            ELTPOS  
00059                                                                   ELTPOS  
00060  CONFIGURATION SECTION.                                           ELTPOS  
00061  SOURCE-COMPUTER.    IBM-3090.                                    ELTPOS  
00062  OBJECT-COMPUTER.    IBM-3090.                                    ELTPOS  
00063      EJECT                                                        ELTPOS  
00064                                                                   ELTPOS  
00065  DATA DIVISION.                                                   ELTPOS  
00066 /                                                                 ELTPOS  
00067 *                                                                 ELTPOS  
00068  WORKING-STORAGE SECTION.                                         ELTPOS  
00069  01  WS-BEGIN                            PIC X(24) VALUE          ELTPOS  
00070                                 '** ELTPOS WS BEGINS **'.         ELTPOS  
00071  01  WS-MISC.                                                     ELTPOS  
00072      05  PC-GCCP                       PIC X(06)                  ELTPOS  
00073                                               VALUE '#GCCP'.      ELTPOS  
00074      05  PC-GMCR                       PIC X(06)                  ELTPOS  
00075                                               VALUE '#GMCR'.      ELTPOS  
00076      05  PC-GMCS                       PIC X(06)                  ELTPOS  
00077                                               VALUE '#GMCS'.      ELTPOS  
00078      05  PC-GROUP                      PIC X(06)                  ELTPOS  
00079                                               VALUE 'GROUP'.      ELTPOS  
00080      05  WS-GMCR-PROC-ID               PIC X(06) VALUE SPACE.     ELTPOS  
00081      05  WS-GMCR-PROC-SLOT-NO          PIC S9(04) COMP-3          ELTPOS  
00082                                                  VALUE ZERO.      ELTPOS  
00083      05  WS-GMCS-SPEC-ID               PIC X(06) VALUE SPACE.     ELTPOS  
00084      05  WS-GMCS-SPEC-SLOT-NO          PIC S9(04) COMP-3          ELTPOS  
00085                                                  VALUE ZERO.      ELTPOS  
00086      05  WS-CMF-SUB                    PIC S9(04) COMP.           ELTPOS  
00087      05  SCREEN-TYPE                   PIC X  VALUE SPACES.       ELTPOS  
00088          88  INSTITUTIONAL-SCREEN             VALUE 'I'.          ELTPOS  
00089          88  PROFESSIONAL-SCREEN              VALUE 'P'.          ELTPOS  
00090          88  SUPPLEMENTAL-SCREEN              VALUE 'S'.          ELTPOS  
00091 *                                                                 ELTPOS  
00092  01  WS-SWITCHES.                                                 ELTPOS  
00093      05  UNDEFINED-TABULAR-SW     PIC X      VALUE 'N'.           ELTPOS  
00094          88 TABULAR-IS-UNDEFINED             VALUE 'Y'.           ELTPOS  
00095      05  DEFINED-TABULAR-SW       PIC X      VALUE 'N'.           ELTPOS  
00096          88 TABULAR-IS-DEFINED               VALUE 'Y'.           ELTPOS  
00097 *                                                                 ELTPOS  
00098      05  ADDITIONAL-TEXT-SW       PIC X      VALUE SPACE.         ELTPOS  
00099          88 BLANK-LINE-NEEDED                VALUE 'B'.           ELTPOS  
00100          88 ADDITIONAL-TEXT                  VALUE 'Y'.           ELTPOS  
00101 *                                                                 ELTPOS  
00102      05  CONTINUED-PROCESSING-SW  PIC X      VALUE SPACE.         ELTPOS  
00103          88 PROCESSING-CMF-TEXT              VALUE 'P'.           ELTPOS  
00104          88 DONE-PROCESSING                  VALUE 'D'.           ELTPOS  
00105 *                                                                 ELTPOS  
00106      05  WS-PERIOD-SW             PIC X      VALUE 'N'.           ELTPOS  
00107          88 PERIOD-NEEDED                    VALUE 'Y'.           ELTPOS  
00108 *                                                                 ELTPOS  
00109      05  WS-PAYMENT-LEVEL-SW      PIC X      VALUE SPACE.         ELTPOS  
00110          88 PAYMENT-LVL-TRANSLATED           VALUE 'Y'.           ELTPOS  
00111          88 PAYMENT-LVL-NOT-TRANSLATED       VALUE 'N'.           ELTPOS  
00112 *                                                                 ELTPOS  
00113 ******************************************************************ELTPOS  
00114 *SCREEN BODY LINES                                                ELTPOS  
00115 ******************************************************************ELTPOS  
00116  01  WS-HDR-LN2.                                                  ELTPOS  
00117      05  FILLER            PIC X(20)         VALUE SPACES.        ELTPOS  
00118      05  FILLER            PIC X(25)         VALUE                ELTPOS  
00119          'POINT OF SERVICE PROGRAM '.                             ELTPOS  
00120      05  HDR-TITLE         PIC X(13)         VALUE SPACES.        ELTPOS  
00121      05  FILLER            PIC X(20)         VALUE SPACES.        ELTPOS  
00122                                                                   ELTPOS  
00123  01  WS-PARTICIPATION-IND.                                        ELTPOS  
00124      05  FILLER             PIC X(79)        VALUE                ELTPOS  
00125          'THE POINT OF SERVICE PROGRAM APPLIES TO '.              ELTPOS  
00126 *                                                                 ELTPOS  
00127  01  WS-APPROVAL-SOURCE.                                          ELTPOS  
00128      05  FILLER             PIC X(79)        VALUE                ELTPOS  
00129         'THE POINT OF SERVICE PROGRAM REQUIRES THE APPROVAL OF '. ELTPOS  
00130 *                                                                 ELTPOS  
00131  01  WS-INDICATOR.                                                ELTPOS  
00132      05  FILLER             PIC X(79)        VALUE                ELTPOS  
00133          'THE POINT OF SERVICE PROGRAM '.                         ELTPOS  
00134 *                                                                 ELTPOS  
00135  01  WS-PAYMENT-LEVEL.                                            ELTPOS  
00136      05  FILLER                  PIC X(79)   VALUE                ELTPOS  
00137      'POINT OF SERVICE PAYMENT LEVEL RULES ARE AS FOLLOWS:  '.    ELTPOS  
00138 *                                                                 ELTPOS  
00139  01  WS-ALTERNATE-PRICING.                                        ELTPOS  
00140      05  FILLER                  PIC X(79)   VALUE                ELTPOS  
00141          'THE ALTERNATE PRICING FOR PROFESSIONAL SERVICES IS '.   ELTPOS  
00142 *                                                                 ELTPOS  
00143  01  WS-CALC-METHOD.                                              ELTPOS  
00144      05  FILLER                  PIC X(79)   VALUE  'THE METHOD FOELTPOS  
00145 -    'R CALCULATING POINT OF SERVICE NETWORK BENEFITS IS:'.       ELTPOS  
00146 *                                                                 ELTPOS  
00147  01  WS-BENEFITS-REDUCTION.                                       ELTPOS  
00148      05  FILLER                  PIC X(79)   VALUE                ELTPOS  
00149          'DENIED OR REDUCED BENEFITS DUE TO COST CONTAINMENT:'.   ELTPOS  
00150 *                                                                 ELTPOS  
00151  01  WS-SPILLOVER-SENTENCE.                                       ELTPOS  
00152      05  FILLER                 PIC X(79)    VALUE                ELTPOS  
00153          'UNPAID SERVICES AFTER BASIC BENEFIT REDUCTIONS ARE '.   ELTPOS  
00154 *                                                                 ELTPOS  
00155  01  WS-NOT-APPLICABLE-LOB-BC.                                    ELTPOS  
00156      05  FILLER                    PIC X(79)   VALUE              ELTPOS  
00157          'THE POINT OF SERVICE PROGRAM DOES NOT APPLY TO INSTITUTIELTPOS  
00158 -        'ONAL BENEFITS.'.                                        ELTPOS  
00159 *                                                                 ELTPOS  
00160  01  WS-NOT-APPLICABLE-LOB-BS.                                    ELTPOS  
00161      05  FILLER                    PIC X(79)   VALUE              ELTPOS  
00162          'THE POINT OF SERVICE PROGRAM DOES NOT APPLY TO PROFESSIOELTPOS  
00163 -        'NAL BENEFITS.'.                                         ELTPOS  
00164 *                                                                 ELTPOS  
00165  01  WS-NOT-APPLICABLE-LOB-MM.                                    ELTPOS  
00166      05  FILLER                    PIC X(79)   VALUE              ELTPOS  
00167          'THE POINT OF SERVICE PROGRAM DOES NOT APPLY TO SUPPLEME ELTPOS  
00168 -        'NTAL BENEFITS.'.                                        ELTPOS  
00169 *                                                                 ELTPOS  
00170  01  WS-NOT-APPLICABLE-MSG.                                       ELTPOS  
00171      05  FILLER                    PIC X(79)  VALUE               ELTPOS  
00172          'THE POINT OF SERVICE PROGRAM IS NOT APPLICABLE.'.       ELTPOS  
00173 *                                                                 ELTPOS  
00174  01  SPECIAL-PROCEDURES-MSG.                                      ELTPOS  
00175      05  FILLER                    PIC X(79)  VALUE               ELTPOS  
00176         'THERE ARE SPECIAL RELATED PROCEDURES INCLUDED IN THIS COSELTPOS  
00177 -        'T CONTAINMENT PROGRAM.'.                                ELTPOS  
00178 *                                                                 ELTPOS  
00179  01  SPECIAL-SERVICES-MSG.                                        ELTPOS  
00180      05  FILLER                    PIC X(79)  VALUE               ELTPOS  
00181         'THERE ARE SPECIAL RELATED SERVICES INCLUDED IN THIS COST ELTPOS  
00182 -        'CONTAINMENT PROGRAM.'.                                  ELTPOS  
00183 *                                                                 ELTPOS  
00184  01  WS-DISCLAIMER.                                               ELTPOS  
00185      05  FILLER                    PIC X(79)  VALUE               ELTPOS  
00186         '*** SUBJECT TO CONTRACT LIMITATIONS ***'.                ELTPOS  
00187  01  WS-END                              PIC X(18) VALUE          ELTPOS  
00188                                          '*** END OF W/S ***'.    ELTPOS  
00189  LINKAGE SECTION.                                                 ELTPOS  
00190  01  DFHCOMMAREA.                                                 ELTPOS  
00191      COPY ELSCOMMC.                                               ELTPOS  
00192 /                                                                 ELTPOS  
00193      COPY ELSCIA2C.                                               ELTPOS  
00194 /                                                                 ELTPOS  
00195      COPY ELSCMDSC.                                               ELTPOS  
00196 /                                                                 ELTPOS  
00197      COPY ELSCMIFC.                                               ELTPOS  
00198 /                                                                 ELTPOS  
00199      COPY ELSIOPMC.                                               ELTPOS  
00200 /                                                                 ELTPOS  
00201      COPY ELSKEYSC.                                               ELTPOS  
00202 /                                                                 ELTPOS  
00203      COPY ELSOUTPC.                                               ELTPOS  
00204 /                                                                 ELTPOS  
00205      COPY ELSSRTPC.                                               ELTPOS  
00206 /                                                                 ELTPOS  
00207      COPY ELSTCWAC.                                               ELTPOS  
00208 /                                                                 ELTPOS  
00209      COPY ELSSSCBC.                                               ELTPOS  
00210 /                                                                 ELTPOS  
00211  01  GROUP-SPECIFIC-RECORD.                                       ELTPOS  
00212      COPY GCGROUPC.                                               ELTPOS  
00213 /                                                                 ELTPOS  
00214  01  GCCP-TABULAR-REC.                                            ELTPOS  
00215      COPY GCTGCCPC.                                               ELTPOS  
00216 /                                                                 ELTPOS  
00217      EJECT                                                        ELTPOS  
00218  PROCEDURE DIVISION.                                              ELTPOS  
00219 ************************************************************      ELTPOS  
00220 *                                                          *      ELTPOS  
00221 *                    PROCEDURE DIVISION                    *      ELTPOS  
00222 *                                                          *      ELTPOS  
00223 ************************************************************      ELTPOS  
00224                                                                   ELTPOS  
00225                                                                   ELTPOS  
00226 ************************************************************      ELTPOS  
00227 *                                                          *      ELTPOS  
00228 *        POINT OF SERVICE                                  *      ELTPOS  
00229 *                                                          *      ELTPOS  
00230 ************************************************************      ELTPOS  
00231  POINT-OF-SERVICE.                                                ELTPOS  
00232      PERFORM INITIALIZATION.                                      ELTPOS  
00233      PERFORM PROCESS-POS.                                         ELTPOS  
00234      GOBACK.                                                      ELTPOS  
00235                                                                   ELTPOS  
00236                                                                   ELTPOS  
00237 ************************************************************      ELTPOS  
00238 *                                                          *      ELTPOS  
00239 *        INITIALIZATION                                    *      ELTPOS  
00240 *                                                          *      ELTPOS  
00241 ************************************************************      ELTPOS  
00242  INITIALIZATION.                                                  ELTPOS  
00243      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTPOS  
00244      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTPOS  
00245                                                                   ELTPOS  
00246                                                                   ELTPOS  
00247 ************************************************************      ELTPOS  
00248 *                                                          *      ELTPOS  
00249 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTPOS  
00250 *                                                          *      ELTPOS  
00251 ************************************************************      ELTPOS  
00252  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTPOS  
00253      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTPOS  
00254      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTPOS  
00255      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTPOS  
00256                                                                   ELTPOS  
00257                                                                   ELTPOS  
00258 ************************************************************      ELTPOS  
00259 *                                                          *      ELTPOS  
00260 *        CHECK FOR VALID COMMAREA                          *      ELTPOS  
00261 *                                                          *      ELTPOS  
00262 ************************************************************      ELTPOS  
00263  CHECK-FOR-VALID-COMMAREA.                                        ELTPOS  
00264      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTPOS  
00265          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTPOS  
00266                                                                   ELTPOS  
00267                                                                   ELTPOS  
00268 ************************************************************      ELTPOS  
00269 *                                                          *      ELTPOS  
00270 *        SIGNAL INVALID COMMAREA                           *      ELTPOS  
00271 *                                                          *      ELTPOS  
00272 ************************************************************      ELTPOS  
00273  SIGNAL-INVALID-COMMAREA.                                         ELTPOS  
00274      EXEC CICS ABEND                                              ELTPOS  
00275                ABCODE('EL01')                                     ELTPOS  
00276         END-EXEC.                                                 ELTPOS  
00277      EJECT                                                        ELTPOS  
00278                                                                   ELTPOS  
00279                                                                   ELTPOS  
00280 ************************************************************      ELTPOS  
00281 *                                                          *      ELTPOS  
00282 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTPOS  
00283 *                                                          *      ELTPOS  
00284 ************************************************************      ELTPOS  
00285  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTPOS  
00286      IF ECA-CIA-PTR = NULL                                        ELTPOS  
00287          PERFORM SIGNAL-INVALID-CIA                               ELTPOS  
00288      ELSE                                                         ELTPOS  
00289          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTPOS  
00290                                                                   ELTPOS  
00291                                                                   ELTPOS  
00292 ************************************************************      ELTPOS  
00293 *                                                          *      ELTPOS  
00294 *        SIGNAL INVALID CIA                                *      ELTPOS  
00295 *                                                          *      ELTPOS  
00296 ************************************************************      ELTPOS  
00297  SIGNAL-INVALID-CIA.                                              ELTPOS  
00298      EXEC CICS ABEND                                              ELTPOS  
00299                ABCODE('EL02')                                     ELTPOS  
00300         END-EXEC.                                                 ELTPOS  
00301      EJECT                                                        ELTPOS  
00302                                                                   ELTPOS  
00303                                                                   ELTPOS  
00304 ************************************************************      ELTPOS  
00305 *                                                          *      ELTPOS  
00306 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTPOS  
00307 *                                                          *      ELTPOS  
00308 ************************************************************      ELTPOS  
00309  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTPOS  
00310      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTPOS  
00311      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPOS  
00312          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTPOS  
00313      IF CIA-RC-PTR-NULL                                           ELTPOS  
00314          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPOS  
00315                                                                   ELTPOS  
00316                                                                   ELTPOS  
00317 ************************************************************      ELTPOS  
00318 *                                                          *      ELTPOS  
00319 *        SIGNAL UNALLOC AREA ERROR                         *      ELTPOS  
00320 *                                                          *      ELTPOS  
00321 ************************************************************      ELTPOS  
00322  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTPOS  
00323      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTPOS  
00324      PERFORM SIGNAL-ABEND.                                        ELTPOS  
00325                                                                   ELTPOS  
00326                                                                   ELTPOS  
00327 ************************************************************      ELTPOS  
00328 *                                                          *      ELTPOS  
00329 *        SIGNAL ABEND                                      *      ELTPOS  
00330 *                                                          *      ELTPOS  
00331 ************************************************************      ELTPOS  
00332  SIGNAL-ABEND.                                                    ELTPOS  
00333      EXEC CICS ABEND                                              ELTPOS  
00334                ABCODE(CIA-ABCODE)                                 ELTPOS  
00335         END-EXEC.                                                 ELTPOS  
00336      EJECT                                                        ELTPOS  
00337                                                                   ELTPOS  
00338                                                                   ELTPOS  
00339 ************************************************************      ELTPOS  
00340 *                                                          *      ELTPOS  
00341 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTPOS  
00342 *                                                          *      ELTPOS  
00343 ************************************************************      ELTPOS  
00344  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTPOS  
00345      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTPOS  
00346      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTPOS  
00347      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTPOS  
00348      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTPOS  
00349      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTPOS  
00350      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTPOS  
00351      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTPOS  
00352                                                                   ELTPOS  
00353                                                                   ELTPOS  
00354 ************************************************************      ELTPOS  
00355 *                                                          *      ELTPOS  
00356 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTPOS  
00357 *                                                          *      ELTPOS  
00358 ************************************************************      ELTPOS  
00359  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTPOS  
00360      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTPOS  
00361      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPOS  
00362          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTPOS  
00363      IF CIA-RC-PTR-NULL                                           ELTPOS  
00364          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPOS  
00365      EJECT                                                        ELTPOS  
00366                                                                   ELTPOS  
00367                                                                   ELTPOS  
00368 ************************************************************      ELTPOS  
00369 *                                                          *      ELTPOS  
00370 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTPOS  
00371 *                                                          *      ELTPOS  
00372 ************************************************************      ELTPOS  
00373  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTPOS  
00374      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTPOS  
00375      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPOS  
00376          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTPOS  
00377      IF CIA-RC-PTR-NULL                                           ELTPOS  
00378          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPOS  
00379      EJECT                                                        ELTPOS  
00380                                                                   ELTPOS  
00381                                                                   ELTPOS  
00382 ************************************************************      ELTPOS  
00383 *                                                          *      ELTPOS  
00384 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTPOS  
00385 *                                                          *      ELTPOS  
00386 ************************************************************      ELTPOS  
00387  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTPOS  
00388      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTPOS  
00389      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPOS  
00390          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELTPOS  
00391      IF CIA-RC-PTR-NULL                                           ELTPOS  
00392          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPOS  
00393      EJECT                                                        ELTPOS  
00394                                                                   ELTPOS  
00395                                                                   ELTPOS  
00396 ************************************************************      ELTPOS  
00397 *                                                          *      ELTPOS  
00398 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTPOS  
00399 *                                                          *      ELTPOS  
00400 ************************************************************      ELTPOS  
00401  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTPOS  
00402      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTPOS  
00403      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPOS  
00404          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTPOS  
00405      IF CIA-RC-PTR-NULL                                           ELTPOS  
00406          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPOS  
00407      EJECT                                                        ELTPOS  
00408                                                                   ELTPOS  
00409                                                                   ELTPOS  
00410 ************************************************************      ELTPOS  
00411 *                                                          *      ELTPOS  
00412 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTPOS  
00413 *                                                          *      ELTPOS  
00414 ************************************************************      ELTPOS  
00415  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTPOS  
00416      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTPOS  
00417      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPOS  
00418          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTPOS  
00419      IF CIA-RC-PTR-NULL                                           ELTPOS  
00420          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPOS  
00421      EJECT                                                        ELTPOS  
00422                                                                   ELTPOS  
00423                                                                   ELTPOS  
00424 ************************************************************      ELTPOS  
00425 *                                                          *      ELTPOS  
00426 *        ESTABLISH ADDRESSABILITY OF GRP SPECIFIC          *      ELTPOS  
00427 *                                                          *      ELTPOS  
00428 ************************************************************      ELTPOS  
00429  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTPOS  
00430      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTPOS  
00431      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPOS  
00432          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTPOS  
00433      IF CIA-RC-PTR-NULL                                           ELTPOS  
00434          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPOS  
00435      EJECT                                                        ELTPOS  
00436                                                                   ELTPOS  
00437                                                                   ELTPOS  
00438 ************************************************************      ELTPOS  
00439 *                                                          *      ELTPOS  
00440 *        ESTABLISH ADDRESSABILITY OF COST CONTAINMENT      *      ELTPOS  
00441 *                                                          *      ELTPOS  
00442 ************************************************************      ELTPOS  
00443  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTPOS  
00444      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPOS  
00445      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPOS  
00446          ADDRESS OF GCCP-TABULAR-REC.                             ELTPOS  
00447      IF CIA-RC-PTR-NULL                                           ELTPOS  
00448          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPOS  
00449                                                                   ELTPOS  
00450                                                                   ELTPOS  
00451 ************************************************************      ELTPOS  
00452 *                                                          *      ELTPOS  
00453 *        ESTABLISH ADDRESS OF CIA                          *      ELTPOS  
00454 *                                                          *      ELTPOS  
00455 ************************************************************      ELTPOS  
00456  ESTABLISH-ADDRESS-OF-CIA.                                        ELTPOS  
00457      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTPOS  
00458          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTPOS  
00459      EJECT                                                        ELTPOS  
00460                                                                   ELTPOS  
00461                                                                   ELTPOS  
00462 ************************************************************      ELTPOS  
00463 *                                                          *      ELTPOS  
00464 *        PROCESS POS                                       *      ELTPOS  
00465 *                                                          *      ELTPOS  
00466 ************************************************************      ELTPOS  
00467  PROCESS-POS.                                                     ELTPOS  
00468      IF GCG-NEW-POS-IND EQUAL ZERO                                ELTPOS  
00469          PERFORM GENERATE-NOT-APPLICABLE-SENTEN                   ELTPOS  
00470      ELSE                                                         ELTPOS  
00471          PERFORM GENERATE-POINT-OF-SERVICE-TEXT.                  ELTPOS  
00472      MOVE 'E' TO  COF-FUNCTION.                                   ELTPOS  
00473      MOVE ZEROS TO COF-NBR-DTL-LINES                              ELTPOS  
00474                COF-NBR-HDR-LINES.                                 ELTPOS  
00475      PERFORM LINK-TO-OUTPUT.                                      ELTPOS  
00476      EJECT                                                        ELTPOS  
00477                                                                   ELTPOS  
00478                                                                   ELTPOS  
00479 ************************************************************      ELTPOS  
00480 *                                                          *      ELTPOS  
00481 *        GENERATE POINT OF SERVICE TEXT                    *      ELTPOS  
00482 *                                                          *      ELTPOS  
00483 ************************************************************      ELTPOS  
00484  GENERATE-POINT-OF-SERVICE-TEXT.                                  ELTPOS  
00485      PERFORM VERIFY-POINT-OF-SERVICE-IN-GCC.                      ELTPOS  
00486      PERFORM BUILD-POINT-OF-SERVICE-TEXT.                         ELTPOS  
00487                                                                   ELTPOS  
00488                                                                   ELTPOS  
00489 ************************************************************      ELTPOS  
00490 *                                                          *      ELTPOS  
00491 *        GENERATE NOT APPLICABLE SENTENCE                  *      ELTPOS  
00492 *                                                          *      ELTPOS  
00493 ************************************************************      ELTPOS  
00494  GENERATE-NOT-APPLICABLE-SENTEN.                                  ELTPOS  
00495      PERFORM GENERATE-HEADINGS.                                   ELTPOS  
00496      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPOS  
00497      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTPOS  
00498          (COF-NBR-DTL-LINES).                                     ELTPOS  
00499      PERFORM LINK-TO-OUTPUT.                                      ELTPOS  
00500                                                                   ELTPOS  
00501                                                                   ELTPOS  
00502 ************************************************************      ELTPOS  
00503 *                                                          *      ELTPOS  
00504 *        VERIFY POINT OF SERVICE IN GCCP RECORD            *      ELTPOS  
00505 *                                                          *      ELTPOS  
00506 ************************************************************      ELTPOS  
00507  VERIFY-POINT-OF-SERVICE-IN-GCC.                                  ELTPOS  
00508      PERFORM ACQUIRE-GCCP-RECORD.                                 ELTPOS  
00509      PERFORM OBTAIN-POINT-OF-SERVICE-WITHIN.                      ELTPOS  
00510      EJECT                                                        ELTPOS  
00511                                                                   ELTPOS  
00512                                                                   ELTPOS  
00513 ************************************************************      ELTPOS  
00514 *                                                          *      ELTPOS  
00515 *        ACQUIRE GCCP RECORD                               *      ELTPOS  
00516 *                                                          *      ELTPOS  
00517 ************************************************************      ELTPOS  
00518  ACQUIRE-GCCP-RECORD.                                             ELTPOS  
00519      MOVE SPACES TO KWA-PROVISION-ID.                             ELTPOS  
00520      SET GCG-INDEX TO 1.                                          ELTPOS  
00521      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPOS  
00522          AT END                                                   ELTPOS  
00523             MOVE ZEROES TO KWA-PROVISION-SLOT-NO                  ELTPOS  
00524          WHEN GCG-TAB-ID (GCG-INDEX) = PC-GCCP                    ELTPOS  
00525                 MOVE GCG-TAB-ID (GCG-INDEX) TO                    ELTPOS  
00526          KWA-PROVISION-ID                                         ELTPOS  
00527                 MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                  ELTPOS  
00528                     TO KWA-PROVISION-SLOT-NO                      ELTPOS  
00529          END-SEARCH.                                              ELTPOS  
00530      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTPOS  
00531          PERFORM SIGNAL-UNDEFINED-TABULAR                         ELTPOS  
00532      ELSE                                                         ELTPOS  
00533          PERFORM READ-GCCP-RECORD.                                ELTPOS  
00534                                                                   ELTPOS  
00535                                                                   ELTPOS  
00536 ************************************************************      ELTPOS  
00537 *                                                          *      ELTPOS  
00538 *        SIGNAL UNDEFINED TABULAR                          *      ELTPOS  
00539 *                                                          *      ELTPOS  
00540 ************************************************************      ELTPOS  
00541  SIGNAL-UNDEFINED-TABULAR.                                        ELTPOS  
00542      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTPOS  
00543      PERFORM SIGNAL-ABEND.                                        ELTPOS  
00544                                                                   ELTPOS  
00545                                                                   ELTPOS  
00546 ************************************************************      ELTPOS  
00547 *                                                          *      ELTPOS  
00548 *        OBTAIN POINT OF SERVICE WITHIN GCCP RECORD        *      ELTPOS  
00549 *                                                          *      ELTPOS  
00550 ************************************************************      ELTPOS  
00551  OBTAIN-POINT-OF-SERVICE-WITHIN.                                  ELTPOS  
00552      SET GSS-INDEX TO 1.                                          ELTPOS  
00553      SEARCH GSS-ENTRY                                             ELTPOS  
00554         AT END                                                    ELTPOS  
00555            SET TABULAR-IS-UNDEFINED TO TRUE                       ELTPOS  
00556         WHEN GSS-P1-PROG-CODE-CHR (GSS-INDEX)                     ELTPOS  
00557                 CONTINUE                                          ELTPOS  
00558          END-SEARCH.                                              ELTPOS  
00559      IF TABULAR-IS-UNDEFINED                                      ELTPOS  
00560          PERFORM SIGNAL-UNDEFINED-TABULAR.                        ELTPOS  
00561      EJECT                                                        ELTPOS  
00562                                                                   ELTPOS  
00563                                                                   ELTPOS  
00564 ************************************************************      ELTPOS  
00565 *                                                          *      ELTPOS  
00566 *        BUILD POINT OF SERVICE TEXT                       *      ELTPOS  
00567 *                                                          *      ELTPOS  
00568 ************************************************************      ELTPOS  
00569  BUILD-POINT-OF-SERVICE-TEXT.                                     ELTPOS  
00570      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTPOS  
00571          PERFORM GENERATE-INSTITUTIONAL.                          ELTPOS  
00572      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTPOS  
00573          PERFORM GENERATE-PROFESSIONAL.                           ELTPOS  
00574      IF GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                        ELTPOS  
00575                 '03' OR '04' OR '06' OR '08'                      ELTPOS  
00576          PERFORM PROCESS-SUPPLEMENTAL.                            ELTPOS  
00577                                                                   ELTPOS  
00578                                                                   ELTPOS  
00579 ************************************************************      ELTPOS  
00580 *                                                          *      ELTPOS  
00581 *        GENERATE INSTITUTIONAL                            *      ELTPOS  
00582 *                                                          *      ELTPOS  
00583 ************************************************************      ELTPOS  
00584  GENERATE-INSTITUTIONAL.                                          ELTPOS  
00585      SET INSTITUTIONAL-SCREEN TO TRUE.                            ELTPOS  
00586      MOVE 'INSTITUTIONAL' TO HDR-TITLE.                           ELTPOS  
00587      PERFORM GENERATE-HEADINGS.                                   ELTPOS  
00588      IF GSS-P1-BC-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTPOS  
00589                 OR LOW-VALUES                                     ELTPOS  
00590          PERFORM SIGNAL-NOT-APPLICABLE-FOR-BC-L                   ELTPOS  
00591      ELSE                                                         ELTPOS  
00592          PERFORM CONSTRUCT-BC-TEXT-AND-SCREEN.                    ELTPOS  
00593      EJECT                                                        ELTPOS  
00594                                                                   ELTPOS  
00595                                                                   ELTPOS  
00596 ************************************************************      ELTPOS  
00597 *                                                          *      ELTPOS  
00598 *        GENERATE PROFESSIONAL                             *      ELTPOS  
00599 *                                                          *      ELTPOS  
00600 ************************************************************      ELTPOS  
00601  GENERATE-PROFESSIONAL.                                           ELTPOS  
00602      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTPOS  
00603      MOVE 'PROFESSIONAL' TO HDR-TITLE.                            ELTPOS  
00604      PERFORM GENERATE-HEADINGS.                                   ELTPOS  
00605      IF GSS-P1-BS-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTPOS  
00606                 OR LOW-VALUES                                     ELTPOS  
00607          PERFORM SIGNAL-NOT-APPLICABLE-FOR-BS-L                   ELTPOS  
00608      ELSE                                                         ELTPOS  
00609          PERFORM CONSTRUCT-BS-TEXT-AND-SCREEN.                    ELTPOS  
00610      EJECT                                                        ELTPOS  
00611                                                                   ELTPOS  
00612                                                                   ELTPOS  
00613 ************************************************************      ELTPOS  
00614 *                                                          *      ELTPOS  
00615 *        PROCESS SUPPLEMENTAL                              *      ELTPOS  
00616 *                                                          *      ELTPOS  
00617 ************************************************************      ELTPOS  
00618  PROCESS-SUPPLEMENTAL.                                            ELTPOS  
00619      SET SUPPLEMENTAL-SCREEN TO TRUE.                             ELTPOS  
00620      MOVE 'SUPPLEMENTAL' TO HDR-TITLE.                            ELTPOS  
00621      PERFORM GENERATE-HEADINGS.                                   ELTPOS  
00622      IF GSS-P1-MM-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTPOS  
00623                 OR LOW-VALUES                                     ELTPOS  
00624          PERFORM SIGNAL-NOT-APPLICABLE-FOR-MM-L                   ELTPOS  
00625      ELSE                                                         ELTPOS  
00626          PERFORM CONSTRUCT-MM-TEXT-AND-SCREEN.                    ELTPOS  
00627      EJECT                                                        ELTPOS  
00628                                                                   ELTPOS  
00629                                                                   ELTPOS  
00630 ************************************************************      ELTPOS  
00631 *                                                          *      ELTPOS  
00632 *        GENERATE HEADINGS                                 *      ELTPOS  
00633 *                                                          *      ELTPOS  
00634 ************************************************************      ELTPOS  
00635  GENERATE-HEADINGS.                                               ELTPOS  
00636      SET COF-NEW-PAGE TO TRUE.                                    ELTPOS  
00637      MOVE 2            TO COF-NBR-HDR-LINES.                      ELTPOS  
00638      MOVE WS-HDR-LN2   TO COF-HDR-LINE                            ELTPOS  
00639          (COF-NBR-HDR-LINES).                                     ELTPOS  
00640      MOVE +1           TO COF-NBR-DTL-LINES.                      ELTPOS  
00641      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPOS  
00642      PERFORM LINK-TO-OUTPUT.                                      ELTPOS  
00643                                                                   ELTPOS  
00644                                                                   ELTPOS  
00645 ************************************************************      ELTPOS  
00646 *                                                          *      ELTPOS  
00647 *        SIGNAL NOT APPLICABLE FOR BC LOB                  *      ELTPOS  
00648 *                                                          *      ELTPOS  
00649 ************************************************************      ELTPOS  
00650  SIGNAL-NOT-APPLICABLE-FOR-BC-L.                                  ELTPOS  
00651      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPOS  
00652      MOVE WS-NOT-APPLICABLE-LOB-BC TO COF-DTL-LINE                ELTPOS  
00653          (COF-NBR-DTL-LINES).                                     ELTPOS  
00654      PERFORM LINK-TO-OUTPUT.                                      ELTPOS  
00655                                                                   ELTPOS  
00656                                                                   ELTPOS  
00657 ************************************************************      ELTPOS  
00658 *                                                          *      ELTPOS  
00659 *        SIGNAL NOT APPLICABLE FOR BS LOB                  *      ELTPOS  
00660 *                                                          *      ELTPOS  
00661 ************************************************************      ELTPOS  
00662  SIGNAL-NOT-APPLICABLE-FOR-BS-L.                                  ELTPOS  
00663      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPOS  
00664      MOVE WS-NOT-APPLICABLE-LOB-BS TO COF-DTL-LINE                ELTPOS  
00665          (COF-NBR-DTL-LINES).                                     ELTPOS  
00666      PERFORM LINK-TO-OUTPUT.                                      ELTPOS  
00667                                                                   ELTPOS  
00668                                                                   ELTPOS  
00669 ************************************************************      ELTPOS  
00670 *                                                          *      ELTPOS  
00671 *        SIGNAL NOT APPLICABLE FOR MM LOB                  *      ELTPOS  
00672 *                                                          *      ELTPOS  
00673 ************************************************************      ELTPOS  
00674  SIGNAL-NOT-APPLICABLE-FOR-MM-L.                                  ELTPOS  
00675      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPOS  
00676      MOVE WS-NOT-APPLICABLE-LOB-MM TO COF-DTL-LINE                ELTPOS  
00677          (COF-NBR-DTL-LINES).                                     ELTPOS  
00678      PERFORM LINK-TO-OUTPUT.                                      ELTPOS  
00679      EJECT                                                        ELTPOS  
00680                                                                   ELTPOS  
00681                                                                   ELTPOS  
00682 ************************************************************      ELTPOS  
00683 *                                                          *      ELTPOS  
00684 *        CONSTRUCT BC TEXT AND SCREEN                      *      ELTPOS  
00685 *                                                          *      ELTPOS  
00686 ************************************************************      ELTPOS  
00687  CONSTRUCT-BC-TEXT-AND-SCREEN.                                    ELTPOS  
00688      SET PAYMENT-LVL-NOT-TRANSLATED TO TRUE.                      ELTPOS  
00689      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTPOS  
00690      IF GSS-P1-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTPOS  
00691          ZERO                                                     ELTPOS  
00692                 AND SPACES AND LOW-VALUES                         ELTPOS  
00693          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTPOS  
00694      PERFORM TRANSLATE-BC-INDICATOR.                              ELTPOS  
00695      IF GSS-P1-BC-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTPOS  
00696          ZERO                                                     ELTPOS  
00697               AND SPACES AND LOW-VALUES                           ELTPOS  
00698          PERFORM TRANSLATE-BC-PAYMENT-LEVEL-IND.                  ELTPOS  
00699      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTPOS  
00700      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTPOS  
00701      IF PAYMENT-LVL-NOT-TRANSLATED AND                            ELTPOS  
00702                 (GSS-P1-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL      ELTPOS  
00703          ZEROES                                                   ELTPOS  
00704                     AND SPACES AND LOW-VALUES)                    ELTPOS  
00705          PERFORM GENERATE-BC-CALC-METHOD-SENTEN.                  ELTPOS  
00706      IF GSS-P1-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTPOS  
00707                  ZERO AND SPACES AND LOW-VALUES                   ELTPOS  
00708          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTPOS  
00709      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTPOS  
00710               '03' OR '04' OR '06' OR '08')                       ELTPOS  
00711            AND                                                    ELTPOS  
00712             GSS-P1-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL           ELTPOS  
00713          ZERO                                                     ELTPOS  
00714                         AND SPACES AND LOW-VALUES                 ELTPOS  
00715          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTPOS  
00716      PERFORM GENERATE-RELATED-PROCEDURES-TA.                      ELTPOS  
00717      PERFORM GENERATE-SPECIAL-SERVICES-TA.                        ELTPOS  
00718      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTPOS  
00719      MOVE SPACES TO SCREEN-TYPE.                                  ELTPOS  
00720      INITIALIZE WS-PAYMENT-LEVEL-SW.                              ELTPOS  
00721      EJECT                                                        ELTPOS  
00722                                                                   ELTPOS  
00723                                                                   ELTPOS  
00724 ************************************************************      ELTPOS  
00725 *                                                          *      ELTPOS  
00726 *        CONSTRUCT BS TEXT AND SCREEN                      *      ELTPOS  
00727 *                                                          *      ELTPOS  
00728 ************************************************************      ELTPOS  
00729  CONSTRUCT-BS-TEXT-AND-SCREEN.                                    ELTPOS  
00730      SET PAYMENT-LVL-NOT-TRANSLATED TO TRUE.                      ELTPOS  
00731      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTPOS  
00732      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTPOS  
00733      IF GSS-P1-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTPOS  
00734          ZERO                                                     ELTPOS  
00735                 AND SPACES AND LOW-VALUES                         ELTPOS  
00736          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTPOS  
00737      PERFORM TRANSLATE-BS-INDICATOR.                              ELTPOS  
00738      IF GSS-P1-BS-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTPOS  
00739          ZERO                                                     ELTPOS  
00740               AND SPACES AND LOW-VALUES                           ELTPOS  
00741          PERFORM TRANSLATE-BS-PAYMENT-LEVEL-IND.                  ELTPOS  
00742      IF GSS-P1-BS-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL             ELTPOS  
00743          SPACES                                                   ELTPOS  
00744                  AND ZEROES AND LOW-VALUES                        ELTPOS  
00745          PERFORM GENERATE-BS-ALT-PRIC-TEXT.                       ELTPOS  
00746      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTPOS  
00747      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTPOS  
00748      IF PAYMENT-LVL-NOT-TRANSLATED AND                            ELTPOS  
00749               (GSS-P1-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL        ELTPOS  
00750          ZEROES                                                   ELTPOS  
00751                AND SPACES AND LOW-VALUES)                         ELTPOS  
00752          PERFORM GENERATE-BS-CALC-METHOD-SENTEN.                  ELTPOS  
00753      IF GSS-P1-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTPOS  
00754                  ZERO AND SPACES AND LOW-VALUES                   ELTPOS  
00755          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTPOS  
00756      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTPOS  
00757                   '03' OR '04' OR '06' OR '08')                   ELTPOS  
00758            AND                                                    ELTPOS  
00759             GSS-P1-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL           ELTPOS  
00760          ZERO                                                     ELTPOS  
00761                         AND SPACES AND LOW-VALUES                 ELTPOS  
00762          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTPOS  
00763      PERFORM GENERATE-RELATED-PROCEDURES-TA.                      ELTPOS  
00764      PERFORM GENERATE-SPECIAL-SERVICES-TA.                        ELTPOS  
00765      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTPOS  
00766      MOVE SPACES TO SCREEN-TYPE.                                  ELTPOS  
00767      INITIALIZE WS-PAYMENT-LEVEL-SW.                              ELTPOS  
00768      EJECT                                                        ELTPOS  
00769                                                                   ELTPOS  
00770                                                                   ELTPOS  
00771 ************************************************************      ELTPOS  
00772 *                                                          *      ELTPOS  
00773 *        CONSTRUCT MM TEXT AND SCREEN                      *      ELTPOS  
00774 *                                                          *      ELTPOS  
00775 ************************************************************      ELTPOS  
00776  CONSTRUCT-MM-TEXT-AND-SCREEN.                                    ELTPOS  
00777      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTPOS  
00778      IF GSS-P1-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTPOS  
00779          ZERO                                                     ELTPOS  
00780                 AND SPACES AND LOW-VALUES                         ELTPOS  
00781          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTPOS  
00782      PERFORM TRANSLATE-MM-INDICATOR.                              ELTPOS  
00783      IF GSS-P1-MM-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTPOS  
00784          ZERO                                                     ELTPOS  
00785             AND SPACES AND LOW-VALUES                             ELTPOS  
00786          PERFORM TRANSLATE-MM-PAYMENT-LEVEL-IND.                  ELTPOS  
00787      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTPOS  
00788      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTPOS  
00789      IF PAYMENT-LVL-NOT-TRANSLATED AND                            ELTPOS  
00790                (GSS-P1-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL       ELTPOS  
00791          ZEROES                                                   ELTPOS  
00792                AND SPACES AND LOW-VALUES)                         ELTPOS  
00793          PERFORM GENERATE-MM-CALC-METHOD-SENTEN.                  ELTPOS  
00794      IF GSS-P1-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTPOS  
00795                  ZERO AND SPACES AND LOW-VALUES                   ELTPOS  
00796          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTPOS  
00797      PERFORM GENERATE-RELATED-PROCEDURES-TA.                      ELTPOS  
00798      PERFORM GENERATE-SPECIAL-SERVICES-TA.                        ELTPOS  
00799      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTPOS  
00800      EJECT                                                        ELTPOS  
00801                                                                   ELTPOS  
00802                                                                   ELTPOS  
00803 ************************************************************      ELTPOS  
00804 *                                                          *      ELTPOS  
00805 *        TRANSLATE PARTICIPATION IND                       *      ELTPOS  
00806 *                                                          *      ELTPOS  
00807 ************************************************************      ELTPOS  
00808  TRANSLATE-PARTICIPATION-IND.                                     ELTPOS  
00809      SET PERIOD-NEEDED TO TRUE.                                   ELTPOS  
00810      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPOS  
00811      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPOS  
00812      MOVE WS-PARTICIPATION-IND TO                                 ELTPOS  
00813          TCAR-FROM-LINE(TCAR-FROM-SUB).                           ELTPOS  
00814      ADD +1 TO TCAR-FROM-SUB.                                     ELTPOS  
00815      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPOS  
00816      MOVE PC-GROUP TO CMF-RECORD-PREFIX.                          ELTPOS  
00817      MOVE 'NEW-POS-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTPOS  
00818      MOVE GCG-NEW-POS-IND TO CMF-CODE-VALUE.                      ELTPOS  
00819      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
00820      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPOS  
00821      EJECT                                                        ELTPOS  
00822                                                                   ELTPOS  
00823                                                                   ELTPOS  
00824 ************************************************************      ELTPOS  
00825 *                                                          *      ELTPOS  
00826 *        GENERATE ASSOCIATED ACCUMULATORS                  *      ELTPOS  
00827 *                                                          *      ELTPOS  
00828 ************************************************************      ELTPOS  
00829  GENERATE-ASSOCIATED-ACCUMULATO.                                  ELTPOS  
00830      PERFORM GENERATE-COINSURANCE-TEXT.                           ELTPOS  
00831      PERFORM GENERATE-COPAY-TEXT.                                 ELTPOS  
00832      PERFORM GENERATE-DEDUCTIBLE-TEXT.                            ELTPOS  
00833      PERFORM GENERATE-BENEFIT-MAXIMUMS-TEXT.                      ELTPOS  
00834      PERFORM GENERATE-OUT-OF-POCKET-TEXT.                         ELTPOS  
00835      EJECT                                                        ELTPOS  
00836                                                                   ELTPOS  
00837                                                                   ELTPOS  
00838 ************************************************************      ELTPOS  
00839 *                                                          *      ELTPOS  
00840 *        GENERATE DISCLAIMER SENTENCE                      *      ELTPOS  
00841 *                                                          *      ELTPOS  
00842 ************************************************************      ELTPOS  
00843  GENERATE-DISCLAIMER-SENTENCE.                                    ELTPOS  
00844      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPOS  
00845      MOVE WS-DISCLAIMER TO COF-DTL-LINE (COF-NBR-DTL-LINES).      ELTPOS  
00846      PERFORM LINK-TO-OUTPUT.                                      ELTPOS  
00847      EJECT                                                        ELTPOS  
00848                                                                   ELTPOS  
00849                                                                   ELTPOS  
00850 ************************************************************      ELTPOS  
00851 *                                                          *      ELTPOS  
00852 *        TRANSLATE APPROVAL SOURCE                         *      ELTPOS  
00853 *                                                          *      ELTPOS  
00854 ************************************************************      ELTPOS  
00855  TRANSLATE-APPROVAL-SOURCE.                                       ELTPOS  
00856      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPOS  
00857      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPOS  
00858      SET PERIOD-NEEDED TO TRUE.                                   ELTPOS  
00859      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPOS  
00860      MOVE WS-APPROVAL-SOURCE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTPOS  
00861      ADD 1 TO TCAR-FROM-SUB.                                      ELTPOS  
00862      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPOS  
00863      MOVE 'P1-APPROVAL-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTPOS  
00864      MOVE GSS-P1-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELTPOS  
00865          CMF-CODE-VALUE.                                          ELTPOS  
00866      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
00867      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPOS  
00868                                                                   ELTPOS  
00869                                                                   ELTPOS  
00870 ************************************************************      ELTPOS  
00871 *                                                          *      ELTPOS  
00872 *        GET INDICATOR FIXED TEXT                          *      ELTPOS  
00873 *                                                          *      ELTPOS  
00874 ************************************************************      ELTPOS  
00875  GET-INDICATOR-FIXED-TEXT.                                        ELTPOS  
00876      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPOS  
00877      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPOS  
00878      SET PERIOD-NEEDED TO TRUE.                                   ELTPOS  
00879      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPOS  
00880      MOVE WS-INDICATOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELTPOS  
00881      ADD 1 TO TCAR-FROM-SUB.                                      ELTPOS  
00882      EJECT                                                        ELTPOS  
00883                                                                   ELTPOS  
00884                                                                   ELTPOS  
00885 ************************************************************      ELTPOS  
00886 *                                                          *      ELTPOS  
00887 *        TRANSLATE BC INDICATOR                            *      ELTPOS  
00888 *                                                          *      ELTPOS  
00889 ************************************************************      ELTPOS  
00890  TRANSLATE-BC-INDICATOR.                                          ELTPOS  
00891      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTPOS  
00892      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPOS  
00893      MOVE 'P1-BC-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTPOS  
00894      MOVE GSS-P1-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPOS  
00895      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
00896      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPOS  
00897      EJECT                                                        ELTPOS  
00898                                                                   ELTPOS  
00899                                                                   ELTPOS  
00900 ************************************************************      ELTPOS  
00901 *                                                          *      ELTPOS  
00902 *        TRANSLATE BC PAYMENT LEVEL INDICATOR              *      ELTPOS  
00903 *                                                          *      ELTPOS  
00904 ************************************************************      ELTPOS  
00905  TRANSLATE-BC-PAYMENT-LEVEL-IND.                                  ELTPOS  
00906      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTPOS  
00907      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPOS  
00908      MOVE 'P1-BC-PAYMENT-LEVEL-IND'  TO CMF-ELEMENT-SYSTEM-NAME.  ELTPOS  
00909      MOVE GSS-P1-BC-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTPOS  
00910          CMF-CODE-VALUE.                                          ELTPOS  
00911      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
00912      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTPOS  
00913      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTPOS  
00914                                                                   ELTPOS  
00915                                                                   ELTPOS  
00916 ************************************************************      ELTPOS  
00917 *                                                          *      ELTPOS  
00918 *        MOVE PREFORMATTED TEXT TO OUTPUT                  *      ELTPOS  
00919 *                                                          *      ELTPOS  
00920 ************************************************************      ELTPOS  
00921  MOVE-PREFORMATTED-TEXT-TO-OUTP.                                  ELTPOS  
00922      PERFORM DO-MOVE-OF-TEXT                                      ELTPOS  
00923          VARYING WS-CMF-SUB FROM 1                                ELTPOS  
00924                       BY 1 UNTIL WS-CMF-SUB                       ELTPOS  
00925                         > CMF-NBR-DESCR-LINES.                    ELTPOS  
00926      PERFORM LINK-TO-OUTPUT.                                      ELTPOS  
00927      EJECT                                                        ELTPOS  
00928                                                                   ELTPOS  
00929                                                                   ELTPOS  
00930 ************************************************************      ELTPOS  
00931 *                                                          *      ELTPOS  
00932 *        DO MOVE OF TEXT                                   *      ELTPOS  
00933 *                                                          *      ELTPOS  
00934 ************************************************************      ELTPOS  
00935  DO-MOVE-OF-TEXT.                                                 ELTPOS  
00936      SET CMF-DESCR-IDX TO WS-CMF-SUB.                             ELTPOS  
00937      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                          ELTPOS  
00938          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTPOS  
00939      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPOS  
00940                                                                   ELTPOS  
00941                                                                   ELTPOS  
00942 ************************************************************      ELTPOS  
00943 *                                                          *      ELTPOS  
00944 *        SETUP PAYMENT LEVEL FIXED TEXT                    *      ELTPOS  
00945 *                                                          *      ELTPOS  
00946 ************************************************************      ELTPOS  
00947  SETUP-PAYMENT-LEVEL-FIXED-TEXT.                                  ELTPOS  
00948      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPOS  
00949      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPOS  
00950      STRING WS-PAYMENT-LEVEL                                      ELTPOS  
00951          DELIMITED BY SIZE INTO COF-DTL-LINE                      ELTPOS  
00952          (COF-NBR-DTL-LINES).                                     ELTPOS  
00953      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTPOS  
00954                                                                   ELTPOS  
00955                                                                   ELTPOS  
00956 ************************************************************      ELTPOS  
00957 *                                                          *      ELTPOS  
00958 *        GENERATE COINSURANCE TEXT                         *      ELTPOS  
00959 *                                                          *      ELTPOS  
00960 ************************************************************      ELTPOS  
00961  GENERATE-COINSURANCE-TEXT.                                       ELTPOS  
00962      EXEC CICS LINK                                               ELTPOS  
00963          PROGRAM ('ELGACLCC')                                     ELTPOS  
00964          COMMAREA (DFHCOMMAREA)                                   ELTPOS  
00965          END-EXEC.                                                ELTPOS  
00966                                                                   ELTPOS  
00967                                                                   ELTPOS  
00968 ************************************************************      ELTPOS  
00969 *                                                          *      ELTPOS  
00970 *        GENERATE COPAY      TEXT                          *      ELTPOS  
00971 *                                                          *      ELTPOS  
00972 ************************************************************      ELTPOS  
00973  GENERATE-COPAY-TEXT.                                             ELTPOS  
00974      EXEC CICS LINK                                               ELTPOS  
00975          PROGRAM ('ELGACPCC')                                     ELTPOS  
00976          COMMAREA (DFHCOMMAREA)                                   ELTPOS  
00977          END-EXEC.                                                ELTPOS  
00978                                                                   ELTPOS  
00979                                                                   ELTPOS  
00980 ************************************************************      ELTPOS  
00981 *                                                          *      ELTPOS  
00982 *        GENERATE DEDUCTIBLE TEXT                          *      ELTPOS  
00983 *                                                          *      ELTPOS  
00984 ************************************************************      ELTPOS  
00985  GENERATE-DEDUCTIBLE-TEXT.                                        ELTPOS  
00986      EXEC CICS LINK                                               ELTPOS  
00987          PROGRAM ('ELGADLCC')                                     ELTPOS  
00988          COMMAREA (DFHCOMMAREA)                                   ELTPOS  
00989          END-EXEC.                                                ELTPOS  
00990                                                                   ELTPOS  
00991                                                                   ELTPOS  
00992 ************************************************************      ELTPOS  
00993 *                                                          *      ELTPOS  
00994 *        GENERATE BENEFIT MAXIMUMS TEXT                    *      ELTPOS  
00995 *                                                          *      ELTPOS  
00996 ************************************************************      ELTPOS  
00997  GENERATE-BENEFIT-MAXIMUMS-TEXT.                                  ELTPOS  
00998      EXEC CICS LINK                                               ELTPOS  
00999          PROGRAM ('ELGABMCC')                                     ELTPOS  
01000          COMMAREA (DFHCOMMAREA)                                   ELTPOS  
01001          END-EXEC.                                                ELTPOS  
01002      EJECT                                                        ELTPOS  
01003                                                                   ELTPOS  
01004                                                                   ELTPOS  
01005 ************************************************************      ELTPOS  
01006 *                                                          *      ELTPOS  
01007 *        GENERATE OUT OF POCKET TEXT                       *      ELTPOS  
01008 *                                                          *      ELTPOS  
01009 ************************************************************      ELTPOS  
01010  GENERATE-OUT-OF-POCKET-TEXT.                                     ELTPOS  
01011      EXEC CICS LINK                                               ELTPOS  
01012          PROGRAM ('ELGAOLCC')                                     ELTPOS  
01013          COMMAREA (DFHCOMMAREA)                                   ELTPOS  
01014          END-EXEC.                                                ELTPOS  
01015      EJECT                                                        ELTPOS  
01016                                                                   ELTPOS  
01017                                                                   ELTPOS  
01018 ************************************************************      ELTPOS  
01019 *                                                          *      ELTPOS  
01020 *        GENERATE BC CALC METHOD SENTENCE                  *      ELTPOS  
01021 *                                                          *      ELTPOS  
01022 ************************************************************      ELTPOS  
01023  GENERATE-BC-CALC-METHOD-SENTEN.                                  ELTPOS  
01024      PERFORM CREATE-CALC-METHOD-FIXED-TEXT.                       ELTPOS  
01025      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPOS  
01026      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPOS  
01027      MOVE 'P1-BC-CALC-METHOD'  TO                                 ELTPOS  
01028          CMF-ELEMENT-SYSTEM-NAME.                                 ELTPOS  
01029      MOVE GSS-P1-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPOS  
01030      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
01031      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTPOS  
01032                                                                   ELTPOS  
01033                                                                   ELTPOS  
01034 ************************************************************      ELTPOS  
01035 *                                                          *      ELTPOS  
01036 *        CREATE CALC METHOD FIXED TEXT                     *      ELTPOS  
01037 *                                                          *      ELTPOS  
01038 ************************************************************      ELTPOS  
01039  CREATE-CALC-METHOD-FIXED-TEXT.                                   ELTPOS  
01040      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPOS  
01041      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPOS  
01042      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPOS  
01043      STRING WS-CALC-METHOD                                        ELTPOS  
01044                DELIMITED BY SIZE INTO COF-DTL-LINE                ELTPOS  
01045          (COF-NBR-DTL-LINES).                                     ELTPOS  
01046      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPOS  
01047      EJECT                                                        ELTPOS  
01048                                                                   ELTPOS  
01049                                                                   ELTPOS  
01050 ************************************************************      ELTPOS  
01051 *                                                          *      ELTPOS  
01052 *        GENERATE BS CALC METHOD SENTENCE                  *      ELTPOS  
01053 *                                                          *      ELTPOS  
01054 ************************************************************      ELTPOS  
01055  GENERATE-BS-CALC-METHOD-SENTEN.                                  ELTPOS  
01056      PERFORM CREATE-CALC-METHOD-FIXED-TEXT.                       ELTPOS  
01057      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPOS  
01058      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPOS  
01059      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPOS  
01060      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPOS  
01061      MOVE 'P1-BS-CALC-METHOD'  TO CMF-ELEMENT-SYSTEM-NAME.        ELTPOS  
01062      MOVE GSS-P1-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPOS  
01063      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
01064      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTPOS  
01065      EJECT                                                        ELTPOS  
01066                                                                   ELTPOS  
01067                                                                   ELTPOS  
01068 ************************************************************      ELTPOS  
01069 *                                                          *      ELTPOS  
01070 *        GENERATE MM CALC METHOD SENTENCE                  *      ELTPOS  
01071 *                                                          *      ELTPOS  
01072 ************************************************************      ELTPOS  
01073  GENERATE-MM-CALC-METHOD-SENTEN.                                  ELTPOS  
01074      PERFORM CREATE-CALC-METHOD-FIXED-TEXT.                       ELTPOS  
01075      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPOS  
01076      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPOS  
01077      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPOS  
01078      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPOS  
01079      MOVE 'P1-MM-CALC-METHOD'  TO CMF-ELEMENT-SYSTEM-NAME.        ELTPOS  
01080      MOVE GSS-P1-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPOS  
01081      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
01082      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTPOS  
01083      EJECT                                                        ELTPOS  
01084                                                                   ELTPOS  
01085                                                                   ELTPOS  
01086 ************************************************************      ELTPOS  
01087 *                                                          *      ELTPOS  
01088 *        GENERATE BS ALT PRIC TEXT                         *      ELTPOS  
01089 *                                                          *      ELTPOS  
01090 ************************************************************      ELTPOS  
01091  GENERATE-BS-ALT-PRIC-TEXT.                                       ELTPOS  
01092      INITIALIZE TCAR-FROM-AREA.                                   ELTPOS  
01093      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPOS  
01094      INITIALIZE WS-PERIOD-SW.                                     ELTPOS  
01095      MOVE WS-ALTERNATE-PRICING  TO TCAR-FROM-LINE                 ELTPOS  
01096          (TCAR-FROM-SUB).                                         ELTPOS  
01097      ADD +1 TO TCAR-FROM-SUB.                                     ELTPOS  
01098      SET ADDITIONAL-TEXT TO TRUE.                                 ELTPOS  
01099      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPOS  
01100      MOVE 'P1-BS-ALT-PRIC-METH' TO CMF-ELEMENT-SYSTEM-NAME.       ELTPOS  
01101      MOVE GSS-P1-BS-ALT-PRIC-METH (GSS-INDEX) TO CMF-CODE-VALUE.  ELTPOS  
01102      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
01103      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPOS  
01104      EJECT                                                        ELTPOS  
01105                                                                   ELTPOS  
01106                                                                   ELTPOS  
01107 ************************************************************      ELTPOS  
01108 *                                                          *      ELTPOS  
01109 *        GENERATE COMBINED BENEFITS REDUCTION SENTENCE     *      ELTPOS  
01110 *                                                          *      ELTPOS  
01111 ************************************************************      ELTPOS  
01112  GENERATE-COMBINED-BENEFITS-RED.                                  ELTPOS  
01113      MOVE 'P1' TO SRP-COST-CONT-TYPE.                             ELTPOS  
01114      MOVE 'POINT OF SERVICE PROGRAM' TO SRP-CCP-NAME.             ELTPOS  
01115      MOVE GSS-P1-COMB-BENE-REDUCT-IND (GSS-INDEX) TO              ELTPOS  
01116           SRP-CCP-COMB-BENE-REDUCT-IND.                           ELTPOS  
01117      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTPOS  
01118      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPOS  
01119          ADDRESS OF GCCP-TABULAR-REC.                             ELTPOS  
01120      EXEC CICS LINK                                               ELTPOS  
01121          PROGRAM ('ELGCBRI')                                      ELTPOS  
01122          COMMAREA (DFHCOMMAREA)                                   ELTPOS  
01123          END-EXEC.                                                ELTPOS  
01124                                                                   ELTPOS  
01125                                                                   ELTPOS  
01126 ************************************************************      ELTPOS  
01127 *                                                          *      ELTPOS  
01128 *        DISPLAY RELATED PROCEDURES SENTENCE               *      ELTPOS  
01129 *                                                          *      ELTPOS  
01130 ************************************************************      ELTPOS  
01131  DISPLAY-RELATED-PROCEDURES-SEN.                                  ELTPOS  
01132      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPOS  
01133      MOVE SPECIAL-PROCEDURES-MSG TO                               ELTPOS  
01134          COF-DTL-LINE (COF-NBR-DTL-LINES).                        ELTPOS  
01135      PERFORM LINK-TO-OUTPUT.                                      ELTPOS  
01136      PERFORM GENERATE-RELATED-PROCEDURES-TE.                      ELTPOS  
01137      EJECT                                                        ELTPOS  
01138                                                                   ELTPOS  
01139                                                                   ELTPOS  
01140 ************************************************************      ELTPOS  
01141 *                                                          *      ELTPOS  
01142 *        GENERATE RELATED PROCEDURES TEXT                  *      ELTPOS  
01143 *                                                          *      ELTPOS  
01144 ************************************************************      ELTPOS  
01145  GENERATE-RELATED-PROCEDURES-TE.                                  ELTPOS  
01146      MOVE 'POINT OF SERVICE' TO SRP-CCP-NAME.                     ELTPOS  
01147      MOVE WS-GMCR-PROC-ID TO SRP-TABULAR-ID.                      ELTPOS  
01148      MOVE WS-GMCR-PROC-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTPOS  
01149      PERFORM CALL-SPECIAL-PROCEDURES-GENERA.                      ELTPOS  
01150                                                                   ELTPOS  
01151                                                                   ELTPOS  
01152 ************************************************************      ELTPOS  
01153 *                                                          *      ELTPOS  
01154 *        CALL SPECIAL PROCEDURES GENERATOR                 *      ELTPOS  
01155 *                                                          *      ELTPOS  
01156 ************************************************************      ELTPOS  
01157  CALL-SPECIAL-PROCEDURES-GENERA.                                  ELTPOS  
01158      EXEC CICS LINK                                               ELTPOS  
01159                PROGRAM ('ELGGXXR')                                ELTPOS  
01160                COMMAREA (DFHCOMMAREA)                             ELTPOS  
01161         END-EXEC.                                                 ELTPOS  
01162      EJECT                                                        ELTPOS  
01163                                                                   ELTPOS  
01164                                                                   ELTPOS  
01165 ************************************************************      ELTPOS  
01166 *                                                          *      ELTPOS  
01167 *        GENERATE RELATED PROCEDURES TABULAR               *      ELTPOS  
01168 *                                                          *      ELTPOS  
01169 ************************************************************      ELTPOS  
01170  GENERATE-RELATED-PROCEDURES-TA.                                  ELTPOS  
01171      SET GCG-INDEX TO 1.                                          ELTPOS  
01172      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPOS  
01173            AT END                                                 ELTPOS  
01174               MOVE ZEROES TO WS-GMCR-PROC-SLOT-NO                 ELTPOS  
01175            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GMCR              ELTPOS  
01176               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTPOS  
01177          WS-GMCR-PROC-ID                                          ELTPOS  
01178               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTPOS  
01179                   TO WS-GMCR-PROC-SLOT-NO                         ELTPOS  
01180         END-SEARCH.                                               ELTPOS  
01181      IF WS-GMCR-PROC-SLOT-NO NOT EQUAL ZEROES                     ELTPOS  
01182                 AND WS-GMCR-PROC-ID EQUAL PC-GMCR                 ELTPOS  
01183          PERFORM DISPLAY-RELATED-PROCEDURES-SEN.                  ELTPOS  
01184      EJECT                                                        ELTPOS  
01185                                                                   ELTPOS  
01186                                                                   ELTPOS  
01187                                                                   ELTPOS  
01188                                                                   ELTPOS  
01189 ************************************************************      ELTPOS  
01190 *                                                          *      ELTPOS  
01191 *        DISPLAY SPECIAL SERVICES SENTENCE                 *      ELTPOS  
01192 *                                                          *      ELTPOS  
01193 ************************************************************      ELTPOS  
01194  DISPLAY-SPECIAL-SERVICES-SEN.                                    ELTPOS  
01195      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPOS  
01196      MOVE SPECIAL-SERVICES-MSG TO                                 ELTPOS  
01197          COF-DTL-LINE (COF-NBR-DTL-LINES).                        ELTPOS  
01198      PERFORM LINK-TO-OUTPUT.                                      ELTPOS  
01199      PERFORM GENERATE-SPECIAL-SERVICES-TE.                        ELTPOS  
01200      EJECT                                                        ELTPOS  
01201                                                                   ELTPOS  
01202                                                                   ELTPOS  
01203 ************************************************************      ELTPOS  
01204 *                                                          *      ELTPOS  
01205 *        GENERATE SPECIAL SERVICES TEXT                    *      ELTPOS  
01206 *                                                          *      ELTPOS  
01207 ************************************************************      ELTPOS  
01208  GENERATE-SPECIAL-SERVICES-TE.                                    ELTPOS  
01209      MOVE 'SPECIAL SERVICES' TO SRP-CCP-NAME.                     ELTPOS  
01210      MOVE WS-GMCS-SPEC-ID TO SRP-TABULAR-ID.                      ELTPOS  
01211      MOVE WS-GMCS-SPEC-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTPOS  
01212      PERFORM CALL-SPECIAL-SERVICES-GENERA.                        ELTPOS  
01213                                                                   ELTPOS  
01214                                                                   ELTPOS  
01215 ************************************************************      ELTPOS  
01216 *                                                          *      ELTPOS  
01217 *        CALL SPECIAL SERVICES GENERATOR                   *      ELTPOS  
01218 *                                                          *      ELTPOS  
01219 ************************************************************      ELTPOS  
01220  CALL-SPECIAL-SERVICES-GENERA.                                    ELTPOS  
01221      EXEC CICS LINK                                               ELTPOS  
01222                PROGRAM ('ELGGXXB')                                ELTPOS  
01223                COMMAREA (DFHCOMMAREA)                             ELTPOS  
01224         END-EXEC.                                                 ELTPOS  
01225      EJECT                                                        ELTPOS  
01226                                                                   ELTPOS  
01227                                                                   ELTPOS  
01228 ************************************************************      ELTPOS  
01229 *                                                          *      ELTPOS  
01230 *        GENERATE SPECIAL SERVICES TABULAR                 *      ELTPOS  
01231 *                                                          *      ELTPOS  
01232 ************************************************************      ELTPOS  
01233  GENERATE-SPECIAL-SERVICES-TA.                                    ELTPOS  
01234      SET GCG-INDEX TO 1.                                          ELTPOS  
01235      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPOS  
01236            AT END                                                 ELTPOS  
01237               MOVE ZEROES TO WS-GMCS-SPEC-SLOT-NO                 ELTPOS  
01238            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GMCS              ELTPOS  
01239               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTPOS  
01240          WS-GMCS-SPEC-ID                                          ELTPOS  
01241               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTPOS  
01242                   TO WS-GMCS-SPEC-SLOT-NO                         ELTPOS  
01243         END-SEARCH.                                               ELTPOS  
01244      IF WS-GMCS-SPEC-SLOT-NO NOT EQUAL ZEROES                     ELTPOS  
01245                 AND WS-GMCS-SPEC-ID EQUAL PC-GMCS                 ELTPOS  
01246          PERFORM DISPLAY-SPECIAL-SERVICES-SEN.                    ELTPOS  
01247      EJECT                                                        ELTPOS  
01248                                                                   ELTPOS  
01249                                                                   ELTPOS  
01250 ************************************************************      ELTPOS  
01251 *                                                          *      ELTPOS  
01252 *        TRANSLATE BS INDICATOR                            *      ELTPOS  
01253 *                                                          *      ELTPOS  
01254 ************************************************************      ELTPOS  
01255  TRANSLATE-BS-INDICATOR.                                          ELTPOS  
01256      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTPOS  
01257      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPOS  
01258      MOVE 'P1-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTPOS  
01259      MOVE GSS-P1-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPOS  
01260      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
01261      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPOS  
01262      EJECT                                                        ELTPOS  
01263                                                                   ELTPOS  
01264                                                                   ELTPOS  
01265 ************************************************************      ELTPOS  
01266 *                                                          *      ELTPOS  
01267 *        TRANSLATE BS PAYMENT LEVEL INDICATOR              *      ELTPOS  
01268 *                                                          *      ELTPOS  
01269 ************************************************************      ELTPOS  
01270  TRANSLATE-BS-PAYMENT-LEVEL-IND.                                  ELTPOS  
01271      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTPOS  
01272      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPOS  
01273      MOVE 'P1-BS-PAYMENT-LEVEL-IND' TO                            ELTPOS  
01274          CMF-ELEMENT-SYSTEM-NAME.                                 ELTPOS  
01275      MOVE GSS-P1-BS-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTPOS  
01276          CMF-CODE-VALUE.                                          ELTPOS  
01277      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
01278      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTPOS  
01279      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTPOS  
01280      EJECT                                                        ELTPOS  
01281                                                                   ELTPOS  
01282                                                                   ELTPOS  
01283 ************************************************************      ELTPOS  
01284 *                                                          *      ELTPOS  
01285 *        TRANSLATE MM INDICATOR                            *      ELTPOS  
01286 *                                                          *      ELTPOS  
01287 ************************************************************      ELTPOS  
01288  TRANSLATE-MM-INDICATOR.                                          ELTPOS  
01289      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTPOS  
01290      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPOS  
01291      MOVE 'P1-MM-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTPOS  
01292      MOVE GSS-P1-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPOS  
01293      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
01294      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPOS  
01295      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPOS  
01296      EJECT                                                        ELTPOS  
01297                                                                   ELTPOS  
01298                                                                   ELTPOS  
01299 ************************************************************      ELTPOS  
01300 *                                                          *      ELTPOS  
01301 *        TRANSLATE MM PAYMENT LEVEL INDICATOR              *      ELTPOS  
01302 *                                                          *      ELTPOS  
01303 ************************************************************      ELTPOS  
01304  TRANSLATE-MM-PAYMENT-LEVEL-IND.                                  ELTPOS  
01305      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTPOS  
01306      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPOS  
01307      MOVE 'P1-MM-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPOS  
01308      MOVE GSS-P1-MM-PAYMENT-LEVEL-IND (GSS-INDEX)                 ELTPOS  
01309                                   TO CMF-CODE-VALUE.              ELTPOS  
01310      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
01311      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTPOS  
01312      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTPOS  
01313      EJECT                                                        ELTPOS  
01314                                                                   ELTPOS  
01315                                                                   ELTPOS  
01316 ************************************************************      ELTPOS  
01317 *                                                          *      ELTPOS  
01318 *        GENERATE SPILL OVER INDICATOR                     *      ELTPOS  
01319 *                                                          *      ELTPOS  
01320 ************************************************************      ELTPOS  
01321  GENERATE-SPILL-OVER-INDICATOR.                                   ELTPOS  
01322      INITIALIZE ADDITIONAL-TEXT-SW.                               ELTPOS  
01323      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPOS  
01324      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPOS  
01325      SET PERIOD-NEEDED TO TRUE.                                   ELTPOS  
01326      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPOS  
01327      MOVE WS-SPILLOVER-SENTENCE TO TCAR-FROM-LINE                 ELTPOS  
01328          (TCAR-FROM-SUB).                                         ELTPOS  
01329      ADD 1 TO TCAR-FROM-SUB.                                      ELTPOS  
01330      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPOS  
01331      MOVE 'P1-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTPOS  
01332      MOVE GSS-P1-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPOS  
01333      PERFORM LINK-TO-TRANSLATOR.                                  ELTPOS  
01334      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPOS  
01335                                                                   ELTPOS  
01336                                                                   ELTPOS  
01337 ************************************************************      ELTPOS  
01338 *                                                          *      ELTPOS  
01339 *        LINK TO TRANSLATOR                                *      ELTPOS  
01340 *                                                          *      ELTPOS  
01341 ************************************************************      ELTPOS  
01342  LINK-TO-TRANSLATOR.                                              ELTPOS  
01343      EXEC CICS LINK                                               ELTPOS  
01344           PROGRAM('ELUCMIF')                                      ELTPOS  
01345           COMMAREA(DFHCOMMAREA)                                   ELTPOS  
01346           END-EXEC.                                               ELTPOS  
01347      EJECT                                                        ELTPOS  
01348                                                                   ELTPOS  
01349                                                                   ELTPOS  
01350 ************************************************************      ELTPOS  
01351 *                                                          *      ELTPOS  
01352 *        READ GCCP RECORD                                  *      ELTPOS  
01353 *                                                          *      ELTPOS  
01354 ************************************************************      ELTPOS  
01355  READ-GCCP-RECORD.                                                ELTPOS  
01356      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPOS  
01357      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPOS  
01358           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                 ELTPOS  
01359      SET IOP-RD TO TRUE.                                          ELTPOS  
01360      SET IOP-FCQ-NONE TO TRUE.                                    ELTPOS  
01361      SET IOP-KVQ-EQ TO TRUE.                                      ELTPOS  
01362      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTPOS  
01363      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTPOS  
01364      PERFORM LINK-TO-I-O-PGM.                                     ELTPOS  
01365      EJECT                                                        ELTPOS  
01366                                                                   ELTPOS  
01367                                                                   ELTPOS  
01368 ************************************************************      ELTPOS  
01369 *                                                          *      ELTPOS  
01370 *        LINK TO I O PGM                                   *      ELTPOS  
01371 *                                                          *      ELTPOS  
01372 ************************************************************      ELTPOS  
01373  LINK-TO-I-O-PGM.                                                 ELTPOS  
01374      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELTPOS  
01375           COMMAREA (DFHCOMMAREA)                                  ELTPOS  
01376           END-EXEC.                                               ELTPOS  
01377      IF IOP-RC-OK                                                 ELTPOS  
01378          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTPOS  
01379      ELSE IF IOP-RC-NOTFND                                        ELTPOS  
01380          PERFORM SIGNAL-NOT-FOUND-GCTAB                           ELTPOS  
01381      ELSE                                                         ELTPOS  
01382          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTPOS  
01383                                                                   ELTPOS  
01384                                                                   ELTPOS  
01385 ************************************************************      ELTPOS  
01386 *                                                          *      ELTPOS  
01387 *        SIGNAL CRITICAL IO ERROR                          *      ELTPOS  
01388 *                                                          *      ELTPOS  
01389 ************************************************************      ELTPOS  
01390  SIGNAL-CRITICAL-IO-ERROR.                                        ELTPOS  
01391      SET CIA-AB-CRITIO TO TRUE.                                   ELTPOS  
01392      PERFORM SIGNAL-ABEND.                                        ELTPOS  
01393                                                                   ELTPOS  
01394                                                                   ELTPOS  
01395 ************************************************************      ELTPOS  
01396 *                                                          *      ELTPOS  
01397 *        SIGNAL NOT FOUND GCTAB                            *      ELTPOS  
01398 *                                                          *      ELTPOS  
01399 ************************************************************      ELTPOS  
01400  SIGNAL-NOT-FOUND-GCTAB.                                          ELTPOS  
01401      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTPOS  
01402      PERFORM SIGNAL-ABEND.                                        ELTPOS  
01403                                                                   ELTPOS  
01404                                                                   ELTPOS  
01405 ************************************************************      ELTPOS  
01406 *                                                          *      ELTPOS  
01407 *        ESTABLISH ADDRESSABILITY OF GCCP RECORD           *      ELTPOS  
01408 *                                                          *      ELTPOS  
01409 ************************************************************      ELTPOS  
01410  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTPOS  
01411      SET ADDRESS OF GCCP-TABULAR-REC TO IOP-REC-PTR.              ELTPOS  
01412      SET IOP-REC-PTR TO NULL.                                     ELTPOS  
01413                                                                   ELTPOS  
01414                                                                   ELTPOS  
01415 ************************************************************      ELTPOS  
01416 *                                                          *      ELTPOS  
01417 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTPOS  
01418 *                                                          *      ELTPOS  
01419 ************************************************************      ELTPOS  
01420  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTPOS  
01421      PERFORM INITIALIZE-CMOUT.                                    ELTPOS  
01422      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTPOS  
01423      EJECT                                                        ELTPOS  
01424                                                                   ELTPOS  
01425                                                                   ELTPOS  
01426 ************************************************************      ELTPOS  
01427 *                                                          *      ELTPOS  
01428 *        PREPARE TEXT FOR OUTPUT                           *      ELTPOS  
01429 *                                                          *      ELTPOS  
01430 ************************************************************      ELTPOS  
01431  PREPARE-TEXT-FOR-OUTPUT.                                         ELTPOS  
01432      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTPOS  
01433          UNTIL CMF-DESCR-IDX                                      ELTPOS  
01434                                    GREATER THAN                   ELTPOS  
01435              CMF-NBR-DESCR-LINES.                                 ELTPOS  
01436      EJECT                                                        ELTPOS  
01437                                                                   ELTPOS  
01438                                                                   ELTPOS  
01439 ************************************************************      ELTPOS  
01440 *                                                          *      ELTPOS  
01441 *        INITIALIZE CMOUT                                  *      ELTPOS  
01442 *                                                          *      ELTPOS  
01443 ************************************************************      ELTPOS  
01444  INITIALIZE-CMOUT.                                                ELTPOS  
01445      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPOS  
01446      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPOS  
01447          ADDRESS OF CMF-DESCR.                                    ELTPOS  
01448      SET CMF-DESCR-IDX TO 1.                                      ELTPOS  
01449      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTPOS  
01450                                                                   ELTPOS  
01451                                                                   ELTPOS  
01452 ************************************************************      ELTPOS  
01453 *                                                          *      ELTPOS  
01454 *        MOVE CMF TEXT TO OUTPUT                           *      ELTPOS  
01455 *                                                          *      ELTPOS  
01456 ************************************************************      ELTPOS  
01457  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTPOS  
01458      PERFORM MOVE-A-LINE.                                         ELTPOS  
01459      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTPOS  
01460          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTPOS  
01461      IF TCAR-FROM-SUB GREATER THAN 20                             ELTPOS  
01462               OR CMF-DESCR-IDX GREATER THAN                       ELTPOS  
01463          CMF-NBR-DESCR-LINES                                      ELTPOS  
01464          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTPOS  
01465                                                                   ELTPOS  
01466                                                                   ELTPOS  
01467 ************************************************************      ELTPOS  
01468 *                                                          *      ELTPOS  
01469 *        FINISH CODES MANUAL TEXT                          *      ELTPOS  
01470 *                                                          *      ELTPOS  
01471 ************************************************************      ELTPOS  
01472  FINISH-CODES-MANUAL-TEXT.                                        ELTPOS  
01473      SET DONE-PROCESSING TO TRUE.                                 ELTPOS  
01474      IF PERIOD-NEEDED                                             ELTPOS  
01475          PERFORM GET-AND-MOVE-PERIOD.                             ELTPOS  
01476                                                                   ELTPOS  
01477                                                                   ELTPOS  
01478 ************************************************************      ELTPOS  
01479 *                                                          *      ELTPOS  
01480 *        GET AND MOVE PERIOD                               *      ELTPOS  
01481 *                                                          *      ELTPOS  
01482 ************************************************************      ELTPOS  
01483  GET-AND-MOVE-PERIOD.                                             ELTPOS  
01484      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTPOS  
01485          (TCAR-FROM-SUB).                                         ELTPOS  
01486                                                                   ELTPOS  
01487                                                                   ELTPOS  
01488 ************************************************************      ELTPOS  
01489 *                                                          *      ELTPOS  
01490 *        SAVE LAST LINE                                    *      ELTPOS  
01491 *                                                          *      ELTPOS  
01492 ************************************************************      ELTPOS  
01493  SAVE-LAST-LINE.                                                  ELTPOS  
01494      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPOS  
01495      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTPOS  
01496         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTPOS  
01497      ADD 1 TO TCAR-FROM-SUB.                                      ELTPOS  
01498      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTPOS  
01499                                                                   ELTPOS  
01500                                                                   ELTPOS  
01501 ************************************************************      ELTPOS  
01502 *                                                          *      ELTPOS  
01503 *        OUTPUT LAST LINE                                  *      ELTPOS  
01504 *                                                          *      ELTPOS  
01505 ************************************************************      ELTPOS  
01506  OUTPUT-LAST-LINE.                                                ELTPOS  
01507      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTPOS  
01508          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTPOS  
01509      IF BLANK-LINE-NEEDED                                         ELTPOS  
01510          PERFORM CREATE-A-BLANK-LINE.                             ELTPOS  
01511                                                                   ELTPOS  
01512                                                                   ELTPOS  
01513 ************************************************************      ELTPOS  
01514 *                                                          *      ELTPOS  
01515 *        CREATE A BLANK LINE                               *      ELTPOS  
01516 *                                                          *      ELTPOS  
01517 ************************************************************      ELTPOS  
01518  CREATE-A-BLANK-LINE.                                             ELTPOS  
01519      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTPOS  
01520      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPOS  
01521                                                                   ELTPOS  
01522                                                                   ELTPOS  
01523 ************************************************************      ELTPOS  
01524 *                                                          *      ELTPOS  
01525 *        MOVE A LINE                                       *      ELTPOS  
01526 *                                                          *      ELTPOS  
01527 ************************************************************      ELTPOS  
01528  MOVE-A-LINE.                                                     ELTPOS  
01529      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTPOS  
01530          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTPOS  
01531      SET CMF-DESCR-IDX UP BY 1.                                   ELTPOS  
01532      ADD 1 TO TCAR-FROM-SUB.                                      ELTPOS  
01533      EJECT                                                        ELTPOS  
01534                                                                   ELTPOS  
01535                                                                   ELTPOS  
01536 ************************************************************      ELTPOS  
01537 *                                                          *      ELTPOS  
01538 *        REFORMAT AND WRITE TEXT                           *      ELTPOS  
01539 *                                                          *      ELTPOS  
01540 ************************************************************      ELTPOS  
01541  REFORMAT-AND-WRITE-TEXT.                                         ELTPOS  
01542      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTPOS  
01543      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTPOS  
01544      PERFORM UNSTRING-TEXT.                                       ELTPOS  
01545      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPOS  
01546      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPOS  
01547      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTPOS  
01548          UNTIL COF-NBR-DTL-LINES GREATER                          ELTPOS  
01549                                   TCAR-OUTPUT-FIELDS-USED -       ELTPOS  
01550              1.                                                   ELTPOS  
01551      PERFORM DISPOSE-OF-LAST-LINE.                                ELTPOS  
01552      PERFORM LINK-TO-OUTPUT.                                      ELTPOS  
01553                                                                   ELTPOS  
01554                                                                   ELTPOS  
01555 ************************************************************      ELTPOS  
01556 *                                                          *      ELTPOS  
01557 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTPOS  
01558 *                                                          *      ELTPOS  
01559 ************************************************************      ELTPOS  
01560  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTPOS  
01561      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTPOS  
01562           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTPOS  
01563      ADD +1 TO TCAR-FROM-SUB.                                     ELTPOS  
01564      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPOS  
01565      EJECT                                                        ELTPOS  
01566                                                                   ELTPOS  
01567                                                                   ELTPOS  
01568 ************************************************************      ELTPOS  
01569 *                                                          *      ELTPOS  
01570 *        UNSTRING TEXT                                     *      ELTPOS  
01571 *                                                          *      ELTPOS  
01572 ************************************************************      ELTPOS  
01573  UNSTRING-TEXT.                                                   ELTPOS  
01574      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTPOS  
01575      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTPOS  
01576      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTPOS  
01577      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTPOS  
01578      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTPOS  
01579      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTPOS  
01580      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTPOS  
01581      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTPOS  
01582      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTPOS  
01583      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTPOS  
01584      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTPOS  
01585      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTPOS  
01586      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTPOS  
01587      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTPOS  
01588      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTPOS  
01589      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTPOS  
01590      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTPOS  
01591      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTPOS  
01592      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTPOS  
01593      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTPOS  
01594      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTPOS  
01595      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTPOS  
01596                                                                   ELTPOS  
01597                                                                   ELTPOS  
01598 ************************************************************      ELTPOS  
01599 *                                                          *      ELTPOS  
01600 *        LINK TO OUTPUT                                    *      ELTPOS  
01601 *                                                          *      ELTPOS  
01602 ************************************************************      ELTPOS  
01603  LINK-TO-OUTPUT.                                                  ELTPOS  
01604      EXEC CICS LINK                                               ELTPOS  
01605          PROGRAM ('ELUOUTPT')                                     ELTPOS  
01606          COMMAREA (DFHCOMMAREA)                                   ELTPOS  
01607          END-EXEC.                                                ELTPOS  
01608      EJECT                                                        ELTPOS  
01609                                                                   ELTPOS  
01610                                                                   ELTPOS  
01611 ************************************************************      ELTPOS  
01612 *                                                          *      ELTPOS  
01613 *        DISPOSE OF LAST LINE                              *      ELTPOS  
01614 *                                                          *      ELTPOS  
01615 ************************************************************      ELTPOS  
01616  DISPOSE-OF-LAST-LINE.                                            ELTPOS  
01617      IF NOT ADDITIONAL-TEXT                                       ELTPOS  
01618          PERFORM INITIALIZE-CONTINUED-SW.                         ELTPOS  
01619      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTPOS  
01620          PERFORM SAVE-LAST-LINE                                   ELTPOS  
01621      ELSE                                                         ELTPOS  
01622          PERFORM OUTPUT-LAST-LINE.                                ELTPOS  
01623                                                                   ELTPOS  
01624                                                                   ELTPOS  
01625 ************************************************************      ELTPOS  
01626 *                                                          *      ELTPOS  
01627 *        INITIALIZE CONTINUED SW                           *      ELTPOS  
01628 *                                                          *      ELTPOS  
01629 ************************************************************      ELTPOS  
01630  INITIALIZE-CONTINUED-SW.                                         ELTPOS  
01631      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTPOS  
