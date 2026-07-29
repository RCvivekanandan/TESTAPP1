00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELTHMC  
00003  PROGRAM-ID.         ELTHMC.                                         LV001
00004                                                                   ELTHMC  
00005  AUTHOR.             ANNE KEFFER KING.                            ELTHMC  
00006                                                                   ELTHMC  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTHMC  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTHMC  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTHMC  
00010                      233 N. MICHIGAN AVE                          ELTHMC  
00011                      CHICAGO, ILLINOIS 60601                      ELTHMC  
00012                                                                   ELTHMC  
00013  DATE-WRITTEN.       28-FEB-2001.                                 ELTHMC  
00014                                                                   ELTHMC  
00015  DATE-COMPILED.                                                   ELTHMC  
00016                                                                   ELTHMC  
00017  SECURITY.           COPYRIGHT 1986,                              ELTHMC  
00018                      HEALTH CARE SERVICE CORPORATION              ELTHMC  
00019 ******************************************************************ELTHMC  
00020 *  RECORDS                                                       *ELTHMC  
00021 *  ACCESSED: ELCDCIA RECORD                                      *ELTHMC  
00022 *             GROUP SPECIFIC RECORD                              *ELTHMC  
00023 *             #GCCP TABULAR RECORD                               *ELTHMC  
00024 *  HMO MANAGED CARE PROGRAM                                      *ELTHMC  
00025 *  PROCESSING                                                    *ELTHMC  
00026 *  FUNCTIONS: THIS MODULE PERFORMS THE FOLLOWING FUNCTIONS:      *ELTHMC  
00027 *                                                                *ELTHMC  
00028 *              1. INITIALIZES WORK DATA ELEMENTS.                *ELTHMC  
00029 *                                                                *ELTHMC  
00030 *              2. ACQUIRE NECESSARY RECORDS FOR PROCESSING.      *ELTHMC  
00031 *                                                                *ELTHMC  
00032 *              3. PROCESS HMO MANAGED CARE NET PROGRAM TABULAR   *ELTHMC  
00033 *                 RECORD FOR BLUE CROSS PRODUCING OUTPUT         *ELTHMC  
00034 *                 TEXT AS REQUIRED.                              *ELTHMC  
00035 *                                                                *ELTHMC  
00036 *              4. PROCESS HMO MANAGED CARE NET PROGRAM TABULAR   *ELTHMC  
00037 *                 RECORD FOR BLUE SHIELD PRODUCING OUTPUT        *ELTHMC  
00038 *                 TEXT AS REQUIRED.                              *ELTHMC  
00039 *                                                                *ELTHMC  
00040 *              5. PROCESS HMO MANAGED CARE NET PROGRAM TABULAR   *ELTHMC  
00041 *                 RECORD FOR MAJOR MEDICAL PRODUCING OUTPUT      *ELTHMC  
00042 *                 TEXT AS REQUIRED.                              *ELTHMC  
00043 ******************************************************************ELTHMC  
00044 *                U P D A T E  L O G                              *ELTHMC  
00045 *                                                                *ELTHMC  
00046 *  MOD      DATE      WHO        DESCRIPTION                      ELTHMC  
00047 *                                                                 ELTHMC  
00048 *  1.00     02/28/01  AKK        CREATED                          ELTHMC  
00049 *                                                                 ELTHMC  
00050 ***************************************************************** ELTHMC  
00051      SKIP3                                                        ELTHMC  
00052                                                                   ELTHMC  
00053  ENVIRONMENT DIVISION.                                            ELTHMC  
00054                                                                   ELTHMC  
00055  CONFIGURATION SECTION.                                           ELTHMC  
00056  SOURCE-COMPUTER.    IBM-3090.                                    ELTHMC  
00057  OBJECT-COMPUTER.    IBM-3090.                                    ELTHMC  
00058      EJECT                                                        ELTHMC  
00059                                                                   ELTHMC  
00060                                                                   ELTHMC  
00061  DATA DIVISION.                                                   ELTHMC  
00062 /                                                                 ELTHMC  
00063 *                                                                 ELTHMC  
00064  WORKING-STORAGE SECTION.                                         ELTHMC  
00065  01  WS-BEGIN                            PIC X(24) VALUE          ELTHMC  
00066                                 '** ELTHMC WS BEGINS **'.         ELTHMC  
00067  01  WS-MISC.                                                     ELTHMC  
00068      05  PC-GCCP                       PIC X(06)                  ELTHMC  
00069                                               VALUE '#GCCP'.      ELTHMC  
00070      05  PC-GROUP                      PIC X(06)                  ELTHMC  
00071                                               VALUE 'GROUP '.     ELTHMC  
00072      05  PC-GMCR                       PIC X(06)                  ELTHMC  
00073                                               VALUE '#GMCR'.      ELTHMC  
00074      05  WS-GMCR-PROC-ID               PIC X(06) VALUE SPACE.     ELTHMC  
00075      05  WS-GMCR-PROC-SLOT-NO          PIC S9(04) COMP-3          ELTHMC  
00076                                                  VALUE ZERO.      ELTHMC  
00077      05  PC-GMCS                       PIC X(06)                  ELTHMC  
00078                                               VALUE '#GMCS'.      ELTHMC  
00079      05  WS-GMCS-SPEC-ID               PIC X(06) VALUE SPACE.     ELTHMC  
00080      05  WS-GMCS-SPEC-SLOT-NO          PIC S9(04) COMP-3          ELTHMC  
00081                                                  VALUE ZERO.      ELTHMC  
00082      05  WS-CMF-SUB                    PIC S9(04) COMP.           ELTHMC  
00083      05  SCREEN-TYPE                   PIC X  VALUE SPACES.       ELTHMC  
00084          88  INSTITUTIONAL-SCREEN             VALUE 'I'.          ELTHMC  
00085          88  PROFESSIONAL-SCREEN              VALUE 'P'.          ELTHMC  
00086          88  SUPPLEMENTAL-SCREEN              VALUE 'S'.          ELTHMC  
00087 *                                                                 ELTHMC  
00088  01  WS-SWITCHES.                                                 ELTHMC  
00089      05  UNDEFINED-TABULAR-SW     PIC X      VALUE 'N'.           ELTHMC  
00090          88 TABULAR-IS-UNDEFINED             VALUE 'Y'.           ELTHMC  
00091      05  DEFINED-TABULAR-SW       PIC X      VALUE 'N'.           ELTHMC  
00092          88 TABULAR-IS-DEFINED               VALUE 'Y'.           ELTHMC  
00093 *                                                                 ELTHMC  
00094      05  ADDITIONAL-TEXT-SW       PIC X      VALUE SPACE.         ELTHMC  
00095          88 BLANK-LINE-NEEDED                VALUE 'B'.           ELTHMC  
00096          88 ADDITIONAL-TEXT                  VALUE 'Y'.           ELTHMC  
00097 *                                                                 ELTHMC  
00098      05  CONTINUED-PROCESSING-SW  PIC X      VALUE SPACE.         ELTHMC  
00099          88 PROCESSING-CMF-TEXT              VALUE 'P'.           ELTHMC  
00100          88 DONE-PROCESSING                  VALUE 'D'.           ELTHMC  
00101 *                                                                 ELTHMC  
00102      05  WS-PERIOD-SW             PIC X      VALUE 'N'.           ELTHMC  
00103          88 PERIOD-NEEDED                    VALUE 'Y'.           ELTHMC  
00104 *                                                                 ELTHMC  
00105      05  WS-PAYMENT-LEVEL-SW      PIC X      VALUE SPACE.         ELTHMC  
00106          88 PAYMENT-LVL-TRANSLATED           VALUE 'Y'.           ELTHMC  
00107          88 PAYMENT-LVL-NOT-TRANSLATED       VALUE 'N'.           ELTHMC  
00108 *                                                                 ELTHMC  
00109 ******************************************************************ELTHMC  
00110 *SCREEN BODY LINES                                                ELTHMC  
00111 ******************************************************************ELTHMC  
00112  01  WS-HDR-LN2.                                                  ELTHMC  
00113      05  FILLER            PIC X(17)         VALUE SPACES.        ELTHMC  
00114      05  FILLER            PIC X(33)         VALUE                ELTHMC  
00115          'HMO MANAGED CARE NETWORK PROGRAM '.                     ELTHMC  
00116      05  HDR-TITLE         PIC X(13)         VALUE SPACES.        ELTHMC  
00117      05  FILLER            PIC X(16)         VALUE SPACES.        ELTHMC  
00118                                                                   ELTHMC  
00119  01  WS-PARTICIPATION-IND.                                        ELTHMC  
00120      05  FILLER             PIC X(79)        VALUE                ELTHMC  
00121          'THE HMO MANAGED CARE NETWORK PROGRAM APPLIES TO '.      ELTHMC  
00122 *                                                                 ELTHMC  
00123                                                                   ELTHMC  
00124  01  WS-APPROVAL-SOURCE.                                          ELTHMC  
00125      05  FILLER             PIC X(79)        VALUE                ELTHMC  
00126          'THE HMO MANAGED CARE NETWORK PROGRAM REQUIRES THE APPROVELTHMC  
00127 -        'AL OF '.                                                ELTHMC  
00128 *                                                                 ELTHMC  
00129  01  WS-INDICATOR.                                                ELTHMC  
00130      05  FILLER             PIC X(79)        VALUE                ELTHMC  
00131          'THE HMO MANAGED CARE NETWORK PROGRAM '.                 ELTHMC  
00132 *                                                                 ELTHMC  
00133  01  WS-PAYMENT-LEVEL.                                            ELTHMC  
00134      05  FILLER                  PIC X(79)   VALUE                ELTHMC  
00135      'HMO MANAGED CARE PAYMENT LEVEL RULES ARE AS FOLLOWS: '.     ELTHMC  
00136 *                                                                 ELTHMC  
00137  01  WS-CALC-METHOD.                                              ELTHMC  
00138      05  FILLER                  PIC X(79)   VALUE  'THE METHOD FOELTHMC  
00139 -    'R CALCULATING HMO MANAGED CARE NETWORK BENEFITS IS:'.       ELTHMC  
00140 *                                                                 ELTHMC  
00141  01  WS-BENEFITS-REDUCTION.                                       ELTHMC  
00142      05  FILLER                  PIC X(79)   VALUE                ELTHMC  
00143          'DENIED OR REDUCED BENEFITS DUE TO COST CONTAINMENT:'.   ELTHMC  
00144 *                                                                 ELTHMC  
00145  01  WS-SPILLOVER-SENTENCE.                                       ELTHMC  
00146      05  FILLER                 PIC X(79)    VALUE                ELTHMC  
00147          'UNPAID SERVICES AFTER BASIC BENEFIT REDUCTIONS ARE '.   ELTHMC  
00148 *                                                                 ELTHMC  
00149  01  WS-NOT-APPLICABLE-LOB-BC.                                    ELTHMC  
00150      05  FILLER                    PIC X(79)   VALUE              ELTHMC  
00151          'THE HMO MANAGED CARE PROGRAM DOES NOT APPLY TO INSTITUTIELTHMC  
00152 -        ' BENEFITS.'.                                            ELTHMC  
00153 *                                                                 ELTHMC  
00154  01  WS-NOT-APPLICABLE-LOB-BS.                                    ELTHMC  
00155      05  FILLER                    PIC X(79)   VALUE              ELTHMC  
00156          'THE HMO MANAGED CARE PROGRAM DOES NOT APPLY TO PROFESSIOELTHMC  
00157 -        'BENEFITS.'.                                             ELTHMC  
00158 *                                                                 ELTHMC  
00159  01  WS-NOT-APPLICABLE-LOB-MM.                                    ELTHMC  
00160      05  FILLER                    PIC X(79)   VALUE              ELTHMC  
00161          'THE HMO MANAGED CARE PROGRAM DOES NOT APPLY TO SUPPLEMENELTHMC  
00162 -        'BENEFITS.'.                                             ELTHMC  
00163 *                                                                 ELTHMC  
00164  01  WS-NOT-APPLICABLE-MSG.                                       ELTHMC  
00165      05  FILLER                    PIC X(79)  VALUE               ELTHMC  
00166          'THE HMO MANAGED CARE PROGRAM IS NOT APPLICABLE.'.       ELTHMC  
00167 *                                                                 ELTHMC  
00168  01  SPECIAL-PROCEDURES-MSG.                                      ELTHMC  
00169      05  FILLER                    PIC X(79)  VALUE               ELTHMC  
00170         'THERE ARE SPECIAL RELATED PROCEDURES INCLUDED IN THIS COSELTHMC  
00171 -       'T CONTAINMENT PROGRAM.'.                                 ELTHMC  
00172 *                                                                 ELTHMC  
00173  01  SPECIAL-SERVICES-MSG.                                        ELTHMC  
00174      05  FILLER                    PIC X(79)  VALUE               ELTHMC  
00175         'THERE ARE SPECIAL RELATED SERVICES INCLUDED IN THIS COST ELTHMC  
00176 -       'CONTAINMENT PROGRAM.'.                                   ELTHMC  
00177 *                                                                 ELTHMC  
00178  01  WS-DISCLAIMER.                                               ELTHMC  
00179      05  FILLER                    PIC X(79)  VALUE               ELTHMC  
00180         '*** SUBJECT TO CONTRACT LIMITATIONS ***'.                ELTHMC  
00181  01  WS-END                              PIC X(18) VALUE          ELTHMC  
00182                                          '*** END OF W/S ***'.    ELTHMC  
00183  LINKAGE SECTION.                                                 ELTHMC  
00184  01  DFHCOMMAREA.                                                 ELTHMC  
00185      COPY ELSCOMMC.                                               ELTHMC  
00186 /                                                                 ELTHMC  
00187      COPY ELSCIA2C.                                               ELTHMC  
00188 /                                                                 ELTHMC  
00189      COPY ELSCMDSC.                                               ELTHMC  
00190 /                                                                 ELTHMC  
00191      COPY ELSCMIFC.                                               ELTHMC  
00192 /                                                                 ELTHMC  
00193      COPY ELSIOPMC.                                               ELTHMC  
00194 /                                                                 ELTHMC  
00195      COPY ELSKEYSC.                                               ELTHMC  
00196 /                                                                 ELTHMC  
00197      COPY ELSOUTPC.                                               ELTHMC  
00198 /                                                                 ELTHMC  
00199      COPY ELSSRTPC.                                               ELTHMC  
00200 /                                                                 ELTHMC  
00201      COPY ELSTCWAC.                                               ELTHMC  
00202 /                                                                 ELTHMC  
00203      COPY ELSSSCBC.                                               ELTHMC  
00204 /                                                                 ELTHMC  
00205  01  GROUP-SPECIFIC-RECORD.                                       ELTHMC  
00206      COPY GCGROUPC.                                               ELTHMC  
00207 /                                                                 ELTHMC  
00208  01  GCCP-TABULAR-REC.                                            ELTHMC  
00209      COPY GCTGCCPC.                                               ELTHMC  
00210 /                                                                 ELTHMC  
00211      EJECT                                                        ELTHMC  
00212  PROCEDURE DIVISION.                                              ELTHMC  
00213 ************************************************************      ELTHMC  
00214 *                                                          *      ELTHMC  
00215 *                    PROCEDURE DIVISION                    *      ELTHMC  
00216 *                                                          *      ELTHMC  
00217 ************************************************************      ELTHMC  
00218                                                                   ELTHMC  
00219                                                                   ELTHMC  
00220 ************************************************************      ELTHMC  
00221 *                                                          *      ELTHMC  
00222 *        HMO MANAGED CARE NETWORK                          *      ELTHMC  
00223 *                                                          *      ELTHMC  
00224 ************************************************************      ELTHMC  
00225  MANAGED-CARE-NETWORK.                                            ELTHMC  
00226      PERFORM INITIALIZATION.                                      ELTHMC  
00227      PERFORM PROCESS-HMC.                                         ELTHMC  
00228      GOBACK.                                                      ELTHMC  
00229                                                                   ELTHMC  
00230                                                                   ELTHMC  
00231 ************************************************************      ELTHMC  
00232 *                                                          *      ELTHMC  
00233 *        INITIALIZATION                                    *      ELTHMC  
00234 *                                                          *      ELTHMC  
00235 ************************************************************      ELTHMC  
00236  INITIALIZATION.                                                  ELTHMC  
00237      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTHMC  
00238      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTHMC  
00239                                                                   ELTHMC  
00240                                                                   ELTHMC  
00241 ************************************************************      ELTHMC  
00242 *                                                          *      ELTHMC  
00243 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTHMC  
00244 *                                                          *      ELTHMC  
00245 ************************************************************      ELTHMC  
00246  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTHMC  
00247      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTHMC  
00248      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTHMC  
00249      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTHMC  
00250                                                                   ELTHMC  
00251                                                                   ELTHMC  
00252 ************************************************************      ELTHMC  
00253 *                                                          *      ELTHMC  
00254 *        CHECK FOR VALID COMMAREA                          *      ELTHMC  
00255 *                                                          *      ELTHMC  
00256 ************************************************************      ELTHMC  
00257  CHECK-FOR-VALID-COMMAREA.                                        ELTHMC  
00258      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTHMC  
00259          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTHMC  
00260                                                                   ELTHMC  
00261                                                                   ELTHMC  
00262 ************************************************************      ELTHMC  
00263 *                                                          *      ELTHMC  
00264 *        SIGNAL INVALID COMMAREA                           *      ELTHMC  
00265 *                                                          *      ELTHMC  
00266 ************************************************************      ELTHMC  
00267  SIGNAL-INVALID-COMMAREA.                                         ELTHMC  
00268      EXEC CICS ABEND                                              ELTHMC  
00269                ABCODE('EL01')                                     ELTHMC  
00270         END-EXEC.                                                 ELTHMC  
00271      EJECT                                                        ELTHMC  
00272                                                                   ELTHMC  
00273                                                                   ELTHMC  
00274 ************************************************************      ELTHMC  
00275 *                                                          *      ELTHMC  
00276 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTHMC  
00277 *                                                          *      ELTHMC  
00278 ************************************************************      ELTHMC  
00279  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTHMC  
00280      IF ECA-CIA-PTR = NULL                                        ELTHMC  
00281          PERFORM SIGNAL-INVALID-CIA                               ELTHMC  
00282      ELSE                                                         ELTHMC  
00283          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTHMC  
00284                                                                   ELTHMC  
00285                                                                   ELTHMC  
00286 ************************************************************      ELTHMC  
00287 *                                                          *      ELTHMC  
00288 *        SIGNAL INVALID CIA                                *      ELTHMC  
00289 *                                                          *      ELTHMC  
00290 ************************************************************      ELTHMC  
00291  SIGNAL-INVALID-CIA.                                              ELTHMC  
00292      EXEC CICS ABEND                                              ELTHMC  
00293                ABCODE('EL02')                                     ELTHMC  
00294         END-EXEC.                                                 ELTHMC  
00295      EJECT                                                        ELTHMC  
00296                                                                   ELTHMC  
00297                                                                   ELTHMC  
00298 ************************************************************      ELTHMC  
00299 *                                                          *      ELTHMC  
00300 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTHMC  
00301 *                                                          *      ELTHMC  
00302 ************************************************************      ELTHMC  
00303  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTHMC  
00304      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTHMC  
00305      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHMC  
00306          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTHMC  
00307      IF CIA-RC-PTR-NULL                                           ELTHMC  
00308          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHMC  
00309                                                                   ELTHMC  
00310                                                                   ELTHMC  
00311 ************************************************************      ELTHMC  
00312 *                                                          *      ELTHMC  
00313 *        SIGNAL UNALLOC AREA ERROR                         *      ELTHMC  
00314 *                                                          *      ELTHMC  
00315 ************************************************************      ELTHMC  
00316  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTHMC  
00317      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTHMC  
00318      PERFORM SIGNAL-ABEND.                                        ELTHMC  
00319                                                                   ELTHMC  
00320                                                                   ELTHMC  
00321 ************************************************************      ELTHMC  
00322 *                                                          *      ELTHMC  
00323 *        SIGNAL ABEND                                      *      ELTHMC  
00324 *                                                          *      ELTHMC  
00325 ************************************************************      ELTHMC  
00326  SIGNAL-ABEND.                                                    ELTHMC  
00327      EXEC CICS ABEND                                              ELTHMC  
00328                ABCODE(CIA-ABCODE)                                 ELTHMC  
00329         END-EXEC.                                                 ELTHMC  
00330      EJECT                                                        ELTHMC  
00331                                                                   ELTHMC  
00332                                                                   ELTHMC  
00333 ************************************************************      ELTHMC  
00334 *                                                          *      ELTHMC  
00335 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTHMC  
00336 *                                                          *      ELTHMC  
00337 ************************************************************      ELTHMC  
00338  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTHMC  
00339      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTHMC  
00340      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTHMC  
00341      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTHMC  
00342      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTHMC  
00343      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTHMC  
00344      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTHMC  
00345      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTHMC  
00346                                                                   ELTHMC  
00347                                                                   ELTHMC  
00348 ************************************************************      ELTHMC  
00349 *                                                          *      ELTHMC  
00350 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTHMC  
00351 *                                                          *      ELTHMC  
00352 ************************************************************      ELTHMC  
00353  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTHMC  
00354      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTHMC  
00355      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHMC  
00356          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTHMC  
00357      IF CIA-RC-PTR-NULL                                           ELTHMC  
00358          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHMC  
00359      EJECT                                                        ELTHMC  
00360                                                                   ELTHMC  
00361                                                                   ELTHMC  
00362 ************************************************************      ELTHMC  
00363 *                                                          *      ELTHMC  
00364 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTHMC  
00365 *                                                          *      ELTHMC  
00366 ************************************************************      ELTHMC  
00367  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTHMC  
00368      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTHMC  
00369      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHMC  
00370          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTHMC  
00371      IF CIA-RC-PTR-NULL                                           ELTHMC  
00372          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHMC  
00373      EJECT                                                        ELTHMC  
00374                                                                   ELTHMC  
00375                                                                   ELTHMC  
00376 ************************************************************      ELTHMC  
00377 *                                                          *      ELTHMC  
00378 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTHMC  
00379 *                                                          *      ELTHMC  
00380 ************************************************************      ELTHMC  
00381  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTHMC  
00382      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTHMC  
00383      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHMC  
00384          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELTHMC  
00385      IF CIA-RC-PTR-NULL                                           ELTHMC  
00386          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHMC  
00387      EJECT                                                        ELTHMC  
00388                                                                   ELTHMC  
00389                                                                   ELTHMC  
00390 ************************************************************      ELTHMC  
00391 *                                                          *      ELTHMC  
00392 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTHMC  
00393 *                                                          *      ELTHMC  
00394 ************************************************************      ELTHMC  
00395  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTHMC  
00396      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTHMC  
00397      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHMC  
00398          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTHMC  
00399      IF CIA-RC-PTR-NULL                                           ELTHMC  
00400          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHMC  
00401      EJECT                                                        ELTHMC  
00402                                                                   ELTHMC  
00403                                                                   ELTHMC  
00404 ************************************************************      ELTHMC  
00405 *                                                          *      ELTHMC  
00406 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTHMC  
00407 *                                                          *      ELTHMC  
00408 ************************************************************      ELTHMC  
00409  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTHMC  
00410      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTHMC  
00411      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHMC  
00412          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTHMC  
00413      IF CIA-RC-PTR-NULL                                           ELTHMC  
00414          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHMC  
00415      EJECT                                                        ELTHMC  
00416                                                                   ELTHMC  
00417                                                                   ELTHMC  
00418 ************************************************************      ELTHMC  
00419 *                                                          *      ELTHMC  
00420 *        ESTABLISH ADDRESSABILITY OF GRP SPECIFIC          *      ELTHMC  
00421 *                                                          *      ELTHMC  
00422 ************************************************************      ELTHMC  
00423  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTHMC  
00424      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTHMC  
00425      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHMC  
00426          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTHMC  
00427      IF CIA-RC-PTR-NULL                                           ELTHMC  
00428          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHMC  
00429      EJECT                                                        ELTHMC  
00430                                                                   ELTHMC  
00431                                                                   ELTHMC  
00432 ************************************************************      ELTHMC  
00433 *                                                          *      ELTHMC  
00434 *        ESTABLISH ADDRESSABILITY OF COST CONTAINMENT      *      ELTHMC  
00435 *                                                          *      ELTHMC  
00436 ************************************************************      ELTHMC  
00437  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTHMC  
00438      SET CIA-GCTABULR-DDN TO TRUE.                                ELTHMC  
00439      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHMC  
00440          ADDRESS OF GCCP-TABULAR-REC.                             ELTHMC  
00441      IF CIA-RC-PTR-NULL                                           ELTHMC  
00442          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHMC  
00443                                                                   ELTHMC  
00444                                                                   ELTHMC  
00445 ************************************************************      ELTHMC  
00446 *                                                          *      ELTHMC  
00447 *        ESTABLISH ADDRESS OF CIA                          *      ELTHMC  
00448 *                                                          *      ELTHMC  
00449 ************************************************************      ELTHMC  
00450  ESTABLISH-ADDRESS-OF-CIA.                                        ELTHMC  
00451      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTHMC  
00452          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTHMC  
00453      EJECT                                                        ELTHMC  
00454                                                                   ELTHMC  
00455                                                                   ELTHMC  
00456 ************************************************************      ELTHMC  
00457 *                                                          *      ELTHMC  
00458 *        PROCESS HMC                                       *      ELTHMC  
00459 *                                                          *      ELTHMC  
00460 ************************************************************      ELTHMC  
00461  PROCESS-HMC.                                                     ELTHMC  
00462      IF GCG-HMO-MC-INDICATOR EQUAL ZERO                           ELTHMC  
00463          PERFORM TEST-APPLICABILITY                               ELTHMC  
00464      ELSE                                                         ELTHMC  
00465          PERFORM GENERATE-MANAGED-CARE-NETWORKX.                  ELTHMC  
00466      MOVE 'E' TO  COF-FUNCTION.                                   ELTHMC  
00467      MOVE ZEROS TO COF-NBR-DTL-LINES                              ELTHMC  
00468                COF-NBR-HDR-LINES.                                 ELTHMC  
00469      PERFORM LINK-TO-OUTPUT.                                      ELTHMC  
00470      EJECT                                                        ELTHMC  
00471                                                                   ELTHMC  
00472                                                                   ELTHMC  
00473 ************************************************************      ELTHMC  
00474 *                                                          *      ELTHMC  
00475 *        GENERATE MANAGED CARE NETWORK TEXT                *      ELTHMC  
00476 *                                                          *      ELTHMC  
00477 ************************************************************      ELTHMC  
00478  GENERATE-MANAGED-CARE-NETWORKX.                                  ELTHMC  
00479      PERFORM VERIFY-MANAGED-CARE-NETWORK-IN.                      ELTHMC  
00480      PERFORM BUILD-MANAGED-CARE-NETWORK-TEX.                      ELTHMC  
00481                                                                   ELTHMC  
00482                                                                   ELTHMC  
00483 ************************************************************      ELTHMC  
00484 *                                                          *      ELTHMC  
00485 *        TEST APPLICABILITY                                *      ELTHMC  
00486 *                                                          *      ELTHMC  
00487 ************************************************************      ELTHMC  
00488  TEST-APPLICABILITY.                                              ELTHMC  
00489      PERFORM GENERATE-HEADINGS.                                   ELTHMC  
00490      IF GCG-POS-PARTICP-IND  EQUAL ZERO                           ELTHMC  
00491          PERFORM SIGNAL-NOT-APPLICABLE-MSG.                       ELTHMC  
00492      PERFORM LINK-TO-OUTPUT.                                      ELTHMC  
00493                                                                   ELTHMC  
00494                                                                   ELTHMC  
00495 ************************************************************      ELTHMC  
00496 *                                                          *      ELTHMC  
00497 *        VERIFY MANAGED CARE NETWORK IN GCCP RECORD        *      ELTHMC  
00498 *                                                          *      ELTHMC  
00499 ************************************************************      ELTHMC  
00500  VERIFY-MANAGED-CARE-NETWORK-IN.                                  ELTHMC  
00501      PERFORM ACQUIRE-GCCP-RECORD.                                 ELTHMC  
00502      PERFORM OBTAIN-MANAGED-CARE-NETWORK-WI.                      ELTHMC  
00503      EJECT                                                        ELTHMC  
00504                                                                   ELTHMC  
00505                                                                   ELTHMC  
00506 ************************************************************      ELTHMC  
00507 *                                                          *      ELTHMC  
00508 *        ACQUIRE GCCP RECORD                               *      ELTHMC  
00509 *                                                          *      ELTHMC  
00510 ************************************************************      ELTHMC  
00511  ACQUIRE-GCCP-RECORD.                                             ELTHMC  
00512      MOVE SPACES TO KWA-PROVISION-ID.                             ELTHMC  
00513      SET GCG-INDEX TO 1.                                          ELTHMC  
00514      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTHMC  
00515          AT END                                                   ELTHMC  
00516             MOVE ZEROES TO KWA-PROVISION-SLOT-NO                  ELTHMC  
00517             WHEN GCG-TAB-ID (GCG-INDEX) = PC-GCCP                 ELTHMC  
00518                 MOVE GCG-TAB-ID (GCG-INDEX) TO                    ELTHMC  
00519          KWA-PROVISION-ID                                         ELTHMC  
00520                 MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                  ELTHMC  
00521                     TO KWA-PROVISION-SLOT-NO                      ELTHMC  
00522          END-SEARCH.                                              ELTHMC  
00523      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTHMC  
00524          PERFORM SIGNAL-UNDEFINED-TABULAR                         ELTHMC  
00525      ELSE                                                         ELTHMC  
00526          PERFORM READ-GCCP-RECORD.                                ELTHMC  
00527                                                                   ELTHMC  
00528                                                                   ELTHMC  
00529 ************************************************************      ELTHMC  
00530 *                                                          *      ELTHMC  
00531 *        SIGNAL UNDEFINED TABULAR                          *      ELTHMC  
00532 *                                                          *      ELTHMC  
00533 ************************************************************      ELTHMC  
00534  SIGNAL-UNDEFINED-TABULAR.                                        ELTHMC  
00535      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTHMC  
00536      PERFORM SIGNAL-ABEND.                                        ELTHMC  
00537                                                                   ELTHMC  
00538                                                                   ELTHMC  
00539 ************************************************************      ELTHMC  
00540 *                                                          *      ELTHMC  
00541 *        OBTAIN MANAGED CARE NETWORK WITHIN GCCP RECORD    *      ELTHMC  
00542 *                                                          *      ELTHMC  
00543 ************************************************************      ELTHMC  
00544  OBTAIN-MANAGED-CARE-NETWORK-WI.                                  ELTHMC  
00545      SET GSS-INDEX TO 1.                                          ELTHMC  
00546      SEARCH GSS-ENTRY                                             ELTHMC  
00547         AT END                                                    ELTHMC  
00548            SET TABULAR-IS-UNDEFINED TO TRUE                       ELTHMC  
00549            WHEN GSS-HM-PROG-CODE-CHR (GSS-INDEX)                  ELTHMC  
00550                 CONTINUE                                          ELTHMC  
00551          END-SEARCH.                                              ELTHMC  
00552      IF TABULAR-IS-UNDEFINED                                      ELTHMC  
00553          PERFORM SIGNAL-UNDEFINED-TABULAR.                        ELTHMC  
00554      EJECT                                                        ELTHMC  
00555                                                                   ELTHMC  
00556                                                                   ELTHMC  
00557 ************************************************************      ELTHMC  
00558 *                                                          *      ELTHMC  
00559 *        BUILD MANAGED CARE NETWORK TEXT                   *      ELTHMC  
00560 *                                                          *      ELTHMC  
00561 ************************************************************      ELTHMC  
00562  BUILD-MANAGED-CARE-NETWORK-TEX.                                  ELTHMC  
00563      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTHMC  
00564          PERFORM GENERATE-INSTITUTIONAL.                          ELTHMC  
00565      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTHMC  
00566          PERFORM GENERATE-PROFESSIONAL.                           ELTHMC  
00567      IF GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                        ELTHMC  
00568                 '03' OR '04' OR '06' OR '08'                      ELTHMC  
00569          PERFORM PROCESS-SUPPLEMENTAL.                            ELTHMC  
00570                                                                   ELTHMC  
00571                                                                   ELTHMC  
00572 ************************************************************      ELTHMC  
00573 *                                                          *      ELTHMC  
00574 *        GENERATE INSTITUTIONAL                            *      ELTHMC  
00575 *                                                          *      ELTHMC  
00576 ************************************************************      ELTHMC  
00577  GENERATE-INSTITUTIONAL.                                          ELTHMC  
00578      SET INSTITUTIONAL-SCREEN TO TRUE.                            ELTHMC  
00579      MOVE 'INSTITUTIONAL' TO HDR-TITLE.                           ELTHMC  
00580      PERFORM GENERATE-HEADINGS.                                   ELTHMC  
00581      PERFORM BUILD-INSTITUTIONAL-TEXT.                            ELTHMC  
00582      EJECT                                                        ELTHMC  
00583                                                                   ELTHMC  
00584                                                                   ELTHMC  
00585 ************************************************************      ELTHMC  
00586 *                                                          *      ELTHMC  
00587 *        GENERATE PROFESSIONAL                             *      ELTHMC  
00588 *                                                          *      ELTHMC  
00589 ************************************************************      ELTHMC  
00590  GENERATE-PROFESSIONAL.                                           ELTHMC  
00591      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTHMC  
00592      MOVE 'PROFESSIONAL' TO HDR-TITLE.                            ELTHMC  
00593      PERFORM GENERATE-HEADINGS.                                   ELTHMC  
00594      PERFORM BUILD-PROFESSIONAL-TEXT.                             ELTHMC  
00595      EJECT                                                        ELTHMC  
00596                                                                   ELTHMC  
00597                                                                   ELTHMC  
00598 ************************************************************      ELTHMC  
00599 *                                                          *      ELTHMC  
00600 *        PROCESS SUPPLEMENTAL                              *      ELTHMC  
00601 *                                                          *      ELTHMC  
00602 ************************************************************      ELTHMC  
00603  PROCESS-SUPPLEMENTAL.                                            ELTHMC  
00604      SET SUPPLEMENTAL-SCREEN TO TRUE.                             ELTHMC  
00605      MOVE 'SUPPLEMENTAL' TO HDR-TITLE.                            ELTHMC  
00606      PERFORM GENERATE-HEADINGS.                                   ELTHMC  
00607      PERFORM BUILD-SUPPLEMENTAL-TEXT.                             ELTHMC  
00608      EJECT                                                        ELTHMC  
00609                                                                   ELTHMC  
00610                                                                   ELTHMC  
00611 ************************************************************      ELTHMC  
00612 *                                                          *      ELTHMC  
00613 *        GENERATE HEADINGS                                 *      ELTHMC  
00614 *                                                          *      ELTHMC  
00615 ************************************************************      ELTHMC  
00616  GENERATE-HEADINGS.                                               ELTHMC  
00617      SET COF-NEW-PAGE TO TRUE.                                    ELTHMC  
00618      MOVE 2            TO COF-NBR-HDR-LINES.                      ELTHMC  
00619      MOVE WS-HDR-LN2   TO COF-HDR-LINE                            ELTHMC  
00620          (COF-NBR-HDR-LINES).                                     ELTHMC  
00621      MOVE +1           TO COF-NBR-DTL-LINES.                      ELTHMC  
00622      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTHMC  
00623      PERFORM LINK-TO-OUTPUT.                                      ELTHMC  
00624                                                                   ELTHMC  
00625                                                                   ELTHMC  
00626 ************************************************************      ELTHMC  
00627 *                                                          *      ELTHMC  
00628 *        BUILD INSTITUTIONAL TEXT                          *      ELTHMC  
00629 *                                                          *      ELTHMC  
00630 ************************************************************      ELTHMC  
00631  BUILD-INSTITUTIONAL-TEXT.                                        ELTHMC  
00632      IF GSS-HM-BC-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTHMC  
00633                 OR LOW-VALUES                                     ELTHMC  
00634          PERFORM SIGNAL-NOT-APPLICABLE-FOR-BC-L                   ELTHMC  
00635      ELSE                                                         ELTHMC  
00636          PERFORM CONSTRUCT-BC-TEXT-AND-SCREEN.                    ELTHMC  
00637                                                                   ELTHMC  
00638                                                                   ELTHMC  
00639 ************************************************************      ELTHMC  
00640 *                                                          *      ELTHMC  
00641 *        BUILD PROFESSIONAL TEXT                           *      ELTHMC  
00642 *                                                          *      ELTHMC  
00643 ************************************************************      ELTHMC  
00644  BUILD-PROFESSIONAL-TEXT.                                         ELTHMC  
00645      IF GSS-HM-BS-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTHMC  
00646                 OR LOW-VALUES                                     ELTHMC  
00647          PERFORM SIGNAL-NOT-APPLICABLE-FOR-BS-L                   ELTHMC  
00648      ELSE                                                         ELTHMC  
00649          PERFORM CONSTRUCT-BS-TEXT-AND-SCREEN.                    ELTHMC  
00650                                                                   ELTHMC  
00651                                                                   ELTHMC  
00652 ************************************************************      ELTHMC  
00653 *                                                          *      ELTHMC  
00654 *        BUILD SUPPLEMENTAL TEXT                           *      ELTHMC  
00655 *                                                          *      ELTHMC  
00656 ************************************************************      ELTHMC  
00657  BUILD-SUPPLEMENTAL-TEXT.                                         ELTHMC  
00658      IF GSS-HM-MM-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTHMC  
00659                 OR LOW-VALUES                                     ELTHMC  
00660          PERFORM SIGNAL-NOT-APPLICABLE-FOR-MM-L                   ELTHMC  
00661      ELSE                                                         ELTHMC  
00662          PERFORM CONSTRUCT-MM-TEXT-AND-SCREEN.                    ELTHMC  
00663      EJECT                                                        ELTHMC  
00664                                                                   ELTHMC  
00665                                                                   ELTHMC  
00666 ************************************************************      ELTHMC  
00667 *                                                          *      ELTHMC  
00668 *        SIGNAL NOT APPLICABLE FOR BC LOB                  *      ELTHMC  
00669 *                                                          *      ELTHMC  
00670 ************************************************************      ELTHMC  
00671  SIGNAL-NOT-APPLICABLE-FOR-BC-L.                                  ELTHMC  
00672      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHMC  
00673      MOVE WS-NOT-APPLICABLE-LOB-BC TO COF-DTL-LINE                ELTHMC  
00674          (COF-NBR-DTL-LINES).                                     ELTHMC  
00675      PERFORM LINK-TO-OUTPUT.                                      ELTHMC  
00676                                                                   ELTHMC  
00677                                                                   ELTHMC  
00678 ************************************************************      ELTHMC  
00679 *                                                          *      ELTHMC  
00680 *        SIGNAL NOT APPLICABLE FOR BS LOB                  *      ELTHMC  
00681 *                                                          *      ELTHMC  
00682 ************************************************************      ELTHMC  
00683  SIGNAL-NOT-APPLICABLE-FOR-BS-L.                                  ELTHMC  
00684      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHMC  
00685      MOVE WS-NOT-APPLICABLE-LOB-BS TO COF-DTL-LINE                ELTHMC  
00686          (COF-NBR-DTL-LINES).                                     ELTHMC  
00687      PERFORM LINK-TO-OUTPUT.                                      ELTHMC  
00688                                                                   ELTHMC  
00689                                                                   ELTHMC  
00690 ************************************************************      ELTHMC  
00691 *                                                          *      ELTHMC  
00692 *        SIGNAL NOT APPLICABLE FOR MM LOB                  *      ELTHMC  
00693 *                                                          *      ELTHMC  
00694 ************************************************************      ELTHMC  
00695  SIGNAL-NOT-APPLICABLE-FOR-MM-L.                                  ELTHMC  
00696      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHMC  
00697      MOVE WS-NOT-APPLICABLE-LOB-MM TO COF-DTL-LINE                ELTHMC  
00698          (COF-NBR-DTL-LINES).                                     ELTHMC  
00699      PERFORM LINK-TO-OUTPUT.                                      ELTHMC  
00700      EJECT                                                        ELTHMC  
00701                                                                   ELTHMC  
00702                                                                   ELTHMC  
00703 ************************************************************      ELTHMC  
00704 *                                                          *      ELTHMC  
00705 *        CONSTRUCT BC TEXT AND SCREEN                      *      ELTHMC  
00706 *                                                          *      ELTHMC  
00707 ************************************************************      ELTHMC  
00708  CONSTRUCT-BC-TEXT-AND-SCREEN.                                    ELTHMC  
00709      SET PAYMENT-LVL-NOT-TRANSLATED TO TRUE.                      ELTHMC  
00710      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTHMC  
00711      IF GSS-HM-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTHMC  
00712          ZERO                                                     ELTHMC  
00713                 AND SPACES AND LOW-VALUES                         ELTHMC  
00714          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTHMC  
00715      PERFORM TRANSLATE-BC-INDICATOR.                              ELTHMC  
00716      IF GSS-HM-BC-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTHMC  
00717          ZERO                                                     ELTHMC  
00718               AND SPACES AND LOW-VALUES                           ELTHMC  
00719          PERFORM TRANSLATE-BC-PAYMENT-LEVEL-IND.                  ELTHMC  
00720      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTHMC  
00721      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTHMC  
00722      IF PAYMENT-LVL-NOT-TRANSLATED                                ELTHMC  
00723          PERFORM GENERATE-BC-CALC-METHOD-SENTEN.                  ELTHMC  
00724      PERFORM GENERATE-BC-BENEFITS-REDUCTION.                      ELTHMC  
00725      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTHMC  
00726               '03' OR '04' OR '06' OR '08')                       ELTHMC  
00727            AND                                                    ELTHMC  
00728             GSS-HM-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL           ELTHMC  
00729          ZERO                                                     ELTHMC  
00730                         AND SPACES AND LOW-VALUES                 ELTHMC  
00731          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTHMC  
00732      PERFORM GENERATE-RELATED-TABULAR.                            ELTHMC  
00733      PERFORM GENERATE-SPECIAL-TABULAR.                            ELTHMC  
00734      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTHMC  
00735      MOVE SPACES TO SCREEN-TYPE.                                  ELTHMC  
00736      INITIALIZE WS-PAYMENT-LEVEL-SW.                              ELTHMC  
00737      EJECT                                                        ELTHMC  
00738                                                                   ELTHMC  
00739                                                                   ELTHMC  
00740 ************************************************************      ELTHMC  
00741 *                                                          *      ELTHMC  
00742 *        CONSTRUCT BS TEXT AND SCREEN                      *      ELTHMC  
00743 *                                                          *      ELTHMC  
00744 ************************************************************      ELTHMC  
00745  CONSTRUCT-BS-TEXT-AND-SCREEN.                                    ELTHMC  
00746      SET PAYMENT-LVL-NOT-TRANSLATED TO TRUE.                      ELTHMC  
00747      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTHMC  
00748      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTHMC  
00749      IF GSS-HM-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTHMC  
00750          ZERO                                                     ELTHMC  
00751                 AND SPACES AND LOW-VALUES                         ELTHMC  
00752          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTHMC  
00753      PERFORM TRANSLATE-BS-INDICATOR.                              ELTHMC  
00754      IF GSS-HM-BS-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTHMC  
00755          ZERO                                                     ELTHMC  
00756               AND SPACES AND LOW-VALUES                           ELTHMC  
00757          PERFORM TRANSLATE-BS-PAYMENT-LEVEL-IND.                  ELTHMC  
00758      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTHMC  
00759      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTHMC  
00760      IF PAYMENT-LVL-NOT-TRANSLATED                                ELTHMC  
00761          PERFORM GENERATE-BS-CALC-METHOD-SENTEN.                  ELTHMC  
00762      PERFORM GENERATE-BS-BENEFITS-REDUCTION.                      ELTHMC  
00763      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTHMC  
00764                   '03' OR '04' OR '06' OR '08')                   ELTHMC  
00765            AND                                                    ELTHMC  
00766             GSS-HM-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL           ELTHMC  
00767          ZERO                                                     ELTHMC  
00768                         AND SPACES AND LOW-VALUES                 ELTHMC  
00769          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTHMC  
00770      PERFORM GENERATE-RELATED-TABULAR.                            ELTHMC  
00771      PERFORM GENERATE-SPECIAL-TABULAR.                            ELTHMC  
00772      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTHMC  
00773      MOVE SPACES TO SCREEN-TYPE.                                  ELTHMC  
00774      INITIALIZE WS-PAYMENT-LEVEL-SW.                              ELTHMC  
00775      EJECT                                                        ELTHMC  
00776                                                                   ELTHMC  
00777                                                                   ELTHMC  
00778 ************************************************************      ELTHMC  
00779 *                                                          *      ELTHMC  
00780 *        CONSTRUCT MM TEXT AND SCREEN                      *      ELTHMC  
00781 *                                                          *      ELTHMC  
00782 ************************************************************      ELTHMC  
00783  CONSTRUCT-MM-TEXT-AND-SCREEN.                                    ELTHMC  
00784      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTHMC  
00785      IF GSS-HM-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTHMC  
00786          ZERO                                                     ELTHMC  
00787                 AND SPACES AND LOW-VALUES                         ELTHMC  
00788          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTHMC  
00789      PERFORM TRANSLATE-MM-INDICATOR.                              ELTHMC  
00790      IF GSS-HM-MM-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTHMC  
00791          ZERO                                                     ELTHMC  
00792             AND SPACES AND LOW-VALUES                             ELTHMC  
00793          PERFORM TRANSLATE-MM-PAYMENT-LEVEL-IND.                  ELTHMC  
00794      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTHMC  
00795      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTHMC  
00796      IF PAYMENT-LVL-NOT-TRANSLATED                                ELTHMC  
00797          PERFORM GENERATE-MM-CALC-METHOD-SENTEN.                  ELTHMC  
00798      PERFORM GENERATE-MM-BENEFITS-REDUCTION.                      ELTHMC  
00799      PERFORM GENERATE-RELATED-TABULAR.                            ELTHMC  
00800      PERFORM GENERATE-SPECIAL-TABULAR.                            ELTHMC  
00801      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTHMC  
00802      EJECT                                                        ELTHMC  
00803                                                                   ELTHMC  
00804                                                                   ELTHMC  
00805 ************************************************************      ELTHMC  
00806 *                                                          *      ELTHMC  
00807 *        GENERATE ASSOCIATED ACCUMULATORS                  *      ELTHMC  
00808 *                                                          *      ELTHMC  
00809 ************************************************************      ELTHMC  
00810  GENERATE-ASSOCIATED-ACCUMULATO.                                  ELTHMC  
00811      PERFORM GENERATE-COINSURANCE-TEXT.                           ELTHMC  
00812      PERFORM GENERATE-COPAY-TEXT.                                 ELTHMC  
00813      PERFORM GENERATE-DEDUCTIBLE-TEXT.                            ELTHMC  
00814      PERFORM GENERATE-BENEFIT-MAXIMUMS-TEXT.                      ELTHMC  
00815      EJECT                                                        ELTHMC  
00816                                                                   ELTHMC  
00817                                                                   ELTHMC  
00818 ************************************************************      ELTHMC  
00819 *                                                          *      ELTHMC  
00820 *        GENERATE DISCLAIMER SENTENCE                      *      ELTHMC  
00821 *                                                          *      ELTHMC  
00822 ************************************************************      ELTHMC  
00823  GENERATE-DISCLAIMER-SENTENCE.                                    ELTHMC  
00824      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHMC  
00825      MOVE WS-DISCLAIMER TO COF-DTL-LINE                           ELTHMC  
00826          (COF-NBR-DTL-LINES).                                     ELTHMC  
00827      PERFORM LINK-TO-OUTPUT.                                      ELTHMC  
00828      EJECT                                                        ELTHMC  
00829                                                                   ELTHMC  
00830                                                                   ELTHMC  
00831 ************************************************************      ELTHMC  
00832 *                                                          *      ELTHMC  
00833 *        TRANSLATE PARTICIPATION IND                       *      ELTHMC  
00834 *                                                          *      ELTHMC  
00835 ************************************************************      ELTHMC  
00836  TRANSLATE-PARTICIPATION-IND.                                     ELTHMC  
00837      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHMC  
00838      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHMC  
00839      SET PERIOD-NEEDED TO TRUE.                                   ELTHMC  
00840      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHMC  
00841      MOVE WS-PARTICIPATION-IND TO TCAR-FROM-LINE (TCAR-FROM-SUB). ELTHMC  
00842      ADD 1 TO TCAR-FROM-SUB.                                      ELTHMC  
00843      MOVE PC-GROUP TO CMF-RECORD-PREFIX.                          ELTHMC  
00844      MOVE 'HMO-MC-INDICATOR' TO CMF-ELEMENT-SYSTEM-NAME.          ELTHMC  
00845      MOVE GCG-HMO-MC-INDICATOR TO CMF-CODE-VALUE.                 ELTHMC  
00846      PERFORM CALL-TRANSLATOR.                                     ELTHMC  
00847      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHMC  
00848      EJECT                                                        ELTHMC  
00849                                                                   ELTHMC  
00850                                                                   ELTHMC  
00851 ************************************************************      ELTHMC  
00852 *                                                          *      ELTHMC  
00853 *        TRANSLATE APPROVAL SOURCE                         *      ELTHMC  
00854 *                                                          *      ELTHMC  
00855 ************************************************************      ELTHMC  
00856  TRANSLATE-APPROVAL-SOURCE.                                       ELTHMC  
00857      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHMC  
00858      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHMC  
00859      SET PERIOD-NEEDED TO TRUE.                                   ELTHMC  
00860      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHMC  
00861      MOVE WS-APPROVAL-SOURCE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTHMC  
00862      ADD 1 TO TCAR-FROM-SUB.                                      ELTHMC  
00863      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTHMC  
00864      MOVE 'HM-APPROVAL-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTHMC  
00865      MOVE GSS-HM-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELTHMC  
00866          CMF-CODE-VALUE.                                          ELTHMC  
00867      PERFORM CALL-TRANSLATOR.                                     ELTHMC  
00868      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHMC  
00869                                                                   ELTHMC  
00870                                                                   ELTHMC  
00871 ************************************************************      ELTHMC  
00872 *                                                          *      ELTHMC  
00873 *        GET INDICATOR FIXED TEXT                          *      ELTHMC  
00874 *                                                          *      ELTHMC  
00875 ************************************************************      ELTHMC  
00876  GET-INDICATOR-FIXED-TEXT.                                        ELTHMC  
00877      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHMC  
00878      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHMC  
00879      SET PERIOD-NEEDED TO TRUE.                                   ELTHMC  
00880      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHMC  
00881      MOVE WS-INDICATOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELTHMC  
00882      ADD 1 TO TCAR-FROM-SUB.                                      ELTHMC  
00883      EJECT                                                        ELTHMC  
00884                                                                   ELTHMC  
00885                                                                   ELTHMC  
00886 ************************************************************      ELTHMC  
00887 *                                                          *      ELTHMC  
00888 *        TRANSLATE BC INDICATOR                            *      ELTHMC  
00889 *                                                          *      ELTHMC  
00890 ************************************************************      ELTHMC  
00891  TRANSLATE-BC-INDICATOR.                                          ELTHMC  
00892      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTHMC  
00893      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTHMC  
00894      MOVE 'HM-BC-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTHMC  
00895      MOVE GSS-HM-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTHMC  
00896      PERFORM CALL-TRANSLATOR.                                     ELTHMC  
00897      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHMC  
00898      EJECT                                                        ELTHMC  
00899                                                                   ELTHMC  
00900                                                                   ELTHMC  
00901 ************************************************************      ELTHMC  
00902 *                                                          *      ELTHMC  
00903 *        TRANSLATE BC PAYMENT LEVEL INDICATOR              *      ELTHMC  
00904 *                                                          *      ELTHMC  
00905 ************************************************************      ELTHMC  
00906  TRANSLATE-BC-PAYMENT-LEVEL-IND.                                  ELTHMC  
00907      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTHMC  
00908      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTHMC  
00909      MOVE 'HM-BC-PAYMENT-LEVEL-IND'  TO CMF-ELEMENT-SYSTEM-NAME.  ELTHMC  
00910      MOVE GSS-HM-BC-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTHMC  
00911          CMF-CODE-VALUE.                                          ELTHMC  
00912      PERFORM CALL-TRANSLATOR.                                     ELTHMC  
00913      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTHMC  
00914      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTHMC  
00915                                                                   ELTHMC  
00916                                                                   ELTHMC  
00917 ************************************************************      ELTHMC  
00918 *                                                          *      ELTHMC  
00919 *        MOVE TRANSLATED TEXT TO OUTPUT                    *      ELTHMC  
00920 *                                                          *      ELTHMC  
00921 ************************************************************      ELTHMC  
00922  MOVE-TRANSLATED-TEXT-TO-OUTPUT.                                  ELTHMC  
00923      PERFORM DO-MOVE-OF-TEXT-TEXT                                 ELTHMC  
00924          VARYING WS-CMF-SUB FROM 1                                ELTHMC  
00925                       BY 1 UNTIL WS-CMF-SUB                       ELTHMC  
00926                         > CMF-NBR-DESCR-LINES.                    ELTHMC  
00927      PERFORM LINK-TO-OUTPUT.                                      ELTHMC  
00928      EJECT                                                        ELTHMC  
00929                                                                   ELTHMC  
00930                                                                   ELTHMC  
00931 ************************************************************      ELTHMC  
00932 *                                                          *      ELTHMC  
00933 *        DO MOVE OF TEXT TEXT                              *      ELTHMC  
00934 *                                                          *      ELTHMC  
00935 ************************************************************      ELTHMC  
00936  DO-MOVE-OF-TEXT-TEXT.                                            ELTHMC  
00937      SET CMF-DESCR-IDX TO WS-CMF-SUB.                             ELTHMC  
00938      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                          ELTHMC  
00939          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTHMC  
00940      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTHMC  
00941                                                                   ELTHMC  
00942                                                                   ELTHMC  
00943 ************************************************************      ELTHMC  
00944 *                                                          *      ELTHMC  
00945 *        SETUP PAYMENT LEVEL FIXED TEXT                    *      ELTHMC  
00946 *                                                          *      ELTHMC  
00947 ************************************************************      ELTHMC  
00948  SETUP-PAYMENT-LEVEL-FIXED-TEXT.                                  ELTHMC  
00949      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHMC  
00950      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHMC  
00951      STRING WS-PAYMENT-LEVEL                                      ELTHMC  
00952          DELIMITED BY SIZE INTO COF-DTL-LINE                      ELTHMC  
00953          (COF-NBR-DTL-LINES).                                     ELTHMC  
00954      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTHMC  
00955                                                                   ELTHMC  
00956                                                                   ELTHMC  
00957 ************************************************************      ELTHMC  
00958 *                                                          *      ELTHMC  
00959 *        GENERATE COINSURANCE TEXT                         *      ELTHMC  
00960 *                                                          *      ELTHMC  
00961 ************************************************************      ELTHMC  
00962  GENERATE-COINSURANCE-TEXT.                                       ELTHMC  
00963      EXEC CICS LINK                                               ELTHMC  
00964          PROGRAM ('ELGACLCC')                                     ELTHMC  
00965          COMMAREA (DFHCOMMAREA)                                   ELTHMC  
00966          END-EXEC.                                                ELTHMC  
00967                                                                   ELTHMC  
00968 ************************************************************      ELTHMC  
00969 *                                                          *      ELTHMC  
00970 *        GENERATE COPAY TEXT                               *      ELTHMC  
00971 *                                                          *      ELTHMC  
00972 ************************************************************      ELTHMC  
00973  GENERATE-COPAY-TEXT.                                             ELTHMC  
00974      EXEC CICS LINK                                               ELTHMC  
00975          PROGRAM ('ELGACPCC')                                     ELTHMC  
00976          COMMAREA (DFHCOMMAREA)                                   ELTHMC  
00977          END-EXEC.                                                ELTHMC  
00978                                                                   ELTHMC  
00979                                                                   ELTHMC  
00980 ************************************************************      ELTHMC  
00981 *                                                          *      ELTHMC  
00982 *        GENERATE DEDUCTIBLE TEXT                          *      ELTHMC  
00983 *                                                          *      ELTHMC  
00984 ************************************************************      ELTHMC  
00985  GENERATE-DEDUCTIBLE-TEXT.                                        ELTHMC  
00986      EXEC CICS LINK                                               ELTHMC  
00987          PROGRAM ('ELGADLCC')                                     ELTHMC  
00988          COMMAREA (DFHCOMMAREA)                                   ELTHMC  
00989          END-EXEC.                                                ELTHMC  
00990                                                                   ELTHMC  
00991                                                                   ELTHMC  
00992 ************************************************************      ELTHMC  
00993 *                                                          *      ELTHMC  
00994 *        GENERATE BENEFIT MAXIMUMS TEXT                    *      ELTHMC  
00995 *                                                          *      ELTHMC  
00996 ************************************************************      ELTHMC  
00997  GENERATE-BENEFIT-MAXIMUMS-TEXT.                                  ELTHMC  
00998      EXEC CICS LINK                                               ELTHMC  
00999          PROGRAM ('ELGABMCC')                                     ELTHMC  
01000          COMMAREA (DFHCOMMAREA)                                   ELTHMC  
01001          END-EXEC.                                                ELTHMC  
01002      EJECT                                                        ELTHMC  
01003                                                                   ELTHMC  
01004                                                                   ELTHMC  
01005 ************************************************************      ELTHMC  
01006 *                                                          *      ELTHMC  
01007 *        GENERATE BC CALC METHOD SENTENCE                  *      ELTHMC  
01008 *                                                          *      ELTHMC  
01009 ************************************************************      ELTHMC  
01010  GENERATE-BC-CALC-METHOD-SENTEN.                                  ELTHMC  
01011      IF GSS-HM-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTHMC  
01012                AND SPACES AND LOW-VALUES                          ELTHMC  
01013          PERFORM CREATE-BC-CALC-SENTENCE.                         ELTHMC  
01014                                                                   ELTHMC  
01015                                                                   ELTHMC  
01016 ************************************************************      ELTHMC  
01017 *                                                          *      ELTHMC  
01018 *        CREATE BC CALC SENTENCE                           *      ELTHMC  
01019 *                                                          *      ELTHMC  
01020 ************************************************************      ELTHMC  
01021  CREATE-BC-CALC-SENTENCE.                                         ELTHMC  
01022      PERFORM CREATE-CALC-METHOD-FIXED-TEXT.                       ELTHMC  
01023      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHMC  
01024      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTHMC  
01025      MOVE 'HM-BC-CALC-METHOD'  TO                                 ELTHMC  
01026          CMF-ELEMENT-SYSTEM-NAME.                                 ELTHMC  
01027      MOVE GSS-HM-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTHMC  
01028      PERFORM CALL-TRANSLATOR.                                     ELTHMC  
01029      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTHMC  
01030                                                                   ELTHMC  
01031                                                                   ELTHMC  
01032 ************************************************************      ELTHMC  
01033 *                                                          *      ELTHMC  
01034 *        CREATE CALC METHOD FIXED TEXT                     *      ELTHMC  
01035 *                                                          *      ELTHMC  
01036 ************************************************************      ELTHMC  
01037  CREATE-CALC-METHOD-FIXED-TEXT.                                   ELTHMC  
01038      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHMC  
01039      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTHMC  
01040      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTHMC  
01041      STRING WS-CALC-METHOD                                        ELTHMC  
01042                DELIMITED BY SIZE INTO COF-DTL-LINE                ELTHMC  
01043          (COF-NBR-DTL-LINES).                                     ELTHMC  
01044      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTHMC  
01045      EJECT                                                        ELTHMC  
01046                                                                   ELTHMC  
01047                                                                   ELTHMC  
01048 ************************************************************      ELTHMC  
01049 *                                                          *      ELTHMC  
01050 *        GENERATE BS CALC METHOD SENTENCE                  *      ELTHMC  
01051 *                                                          *      ELTHMC  
01052 ************************************************************      ELTHMC  
01053  GENERATE-BS-CALC-METHOD-SENTEN.                                  ELTHMC  
01054      IF GSS-HM-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTHMC  
01055                AND SPACES AND LOW-VALUES                          ELTHMC  
01056          PERFORM CREATE-BS-CALC-SENTENCE.                         ELTHMC  
01057                                                                   ELTHMC  
01058 ************************************************************      ELTHMC  
01059 *                                                          *      ELTHMC  
01060 *        CREATE BS BENFITS REDUCTION                       *      ELTHMC  
01061 *                                                          *      ELTHMC  
01062 ************************************************************      ELTHMC  
01063  GENERATE-BS-BENEFITS-REDUCTION.                                  ELTHMC  
01064      IF GSS-PS-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTHMC  
01065                  ZERO AND SPACES AND LOW-VALUES                   ELTHMC  
01066          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTHMC  
01067                                                                   ELTHMC  
01068 ************************************************************      ELTHMC  
01069 *                                                          *      ELTHMC  
01070 *        CREATE BS CALC SENTENCE                           *      ELTHMC  
01071 *                                                          *      ELTHMC  
01072 ************************************************************      ELTHMC  
01073  CREATE-BS-CALC-SENTENCE.                                         ELTHMC  
01074      PERFORM CREATE-CALC-METHOD-FIXED-TEXT.                       ELTHMC  
01075      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHMC  
01076      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHMC  
01077      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHMC  
01078      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTHMC  
01079      MOVE 'HM-BS-CALC-METHOD'  TO                                 ELTHMC  
01080          CMF-ELEMENT-SYSTEM-NAME.                                 ELTHMC  
01081      MOVE GSS-HM-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTHMC  
01082      PERFORM CALL-TRANSLATOR.                                     ELTHMC  
01083      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTHMC  
01084      EJECT                                                        ELTHMC  
01085                                                                   ELTHMC  
01086                                                                   ELTHMC  
01087 ************************************************************      ELTHMC  
01088 *                                                          *      ELTHMC  
01089 *        GENERATE MM CALC METHOD SENTENCE                  *      ELTHMC  
01090 *                                                          *      ELTHMC  
01091 ************************************************************      ELTHMC  
01092  GENERATE-MM-CALC-METHOD-SENTEN.                                  ELTHMC  
01093      IF GSS-HM-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTHMC  
01094                AND SPACES AND LOW-VALUES                          ELTHMC  
01095          PERFORM CREATE-MM-CALC-SENTENCE.                         ELTHMC  
01096                                                                   ELTHMC  
01097                                                                   ELTHMC  
01098 ************************************************************      ELTHMC  
01099 *                                                          *      ELTHMC  
01100 *        CREATE MM CALC SENTENCE                           *      ELTHMC  
01101 *                                                          *      ELTHMC  
01102 ************************************************************      ELTHMC  
01103  CREATE-MM-CALC-SENTENCE.                                         ELTHMC  
01104      PERFORM CREATE-CALC-METHOD-FIXED-TEXT.                       ELTHMC  
01105      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHMC  
01106      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHMC  
01107      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHMC  
01108      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTHMC  
01109      MOVE 'HM-MM-CALC-METHOD'  TO                                 ELTHMC  
01110          CMF-ELEMENT-SYSTEM-NAME.                                 ELTHMC  
01111      MOVE GSS-HM-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTHMC  
01112      PERFORM CALL-TRANSLATOR.                                     ELTHMC  
01113      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTHMC  
01114                                                                   ELTHMC  
01115                                                                   ELTHMC  
01116 ************************************************************      ELTHMC  
01117 *                                                          *      ELTHMC  
01118 *        GENERATE COMBINED BENEFITS REDUCTION SENTENCE     *      ELTHMC  
01119 *                                                          *      ELTHMC  
01120 ************************************************************      ELTHMC  
01121  GENERATE-COMBINED-BENEFITS-RED.                                  ELTHMC  
01122      MOVE 'PS' TO SRP-COST-CONT-TYPE.                             ELTHMC  
01123      MOVE 'MANAGED CARE NETWORK PROGRAM' TO SRP-CCP-NAME.         ELTHMC  
01124      MOVE GSS-HM-COMB-BENE-REDUCT-IND (GSS-INDEX) TO              ELTHMC  
01125           SRP-CCP-COMB-BENE-REDUCT-IND.                           ELTHMC  
01126      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTHMC  
01127      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTHMC  
01128          ADDRESS OF GCCP-TABULAR-REC.                             ELTHMC  
01129      EXEC CICS LINK                                               ELTHMC  
01130          PROGRAM ('ELGCBRI')                                      ELTHMC  
01131          COMMAREA (DFHCOMMAREA)                                   ELTHMC  
01132          END-EXEC.                                                ELTHMC  
01133      EJECT                                                        ELTHMC  
01134                                                                   ELTHMC  
01135                                                                   ELTHMC  
01136 ************************************************************      ELTHMC  
01137 *                                                          *      ELTHMC  
01138 *        GENERATE BC BENEFITS REDUCTION SENTENCE           *      ELTHMC  
01139 *                                                          *      ELTHMC  
01140 ************************************************************      ELTHMC  
01141  GENERATE-BC-BENEFITS-REDUCTION.                                  ELTHMC  
01142      IF GSS-HM-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTHMC  
01143                  ZERO AND SPACES AND LOW-VALUES                   ELTHMC  
01144          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTHMC  
01145      EJECT                                                        ELTHMC  
01146                                                                   ELTHMC  
01147                                                                   ELTHMC  
01148 ************************************************************      ELTHMC  
01149 *                                                          *      ELTHMC  
01150 *        GENERATE RELATED TABULAR                          *      ELTHMC  
01151 *                                                          *      ELTHMC  
01152 ************************************************************      ELTHMC  
01153  GENERATE-RELATED-TABULAR.                                        ELTHMC  
01154      SET GCG-INDEX TO 1.                                          ELTHMC  
01155      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTHMC  
01156            AT END                                                 ELTHMC  
01157               MOVE ZEROES TO WS-GMCR-PROC-SLOT-NO                 ELTHMC  
01158            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GMCR              ELTHMC  
01159               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTHMC  
01160          WS-GMCR-PROC-ID                                          ELTHMC  
01161               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTHMC  
01162                   TO WS-GMCR-PROC-SLOT-NO                         ELTHMC  
01163         END-SEARCH.                                               ELTHMC  
01164      IF WS-GMCR-PROC-SLOT-NO NOT EQUAL ZEROES                     ELTHMC  
01165                 AND WS-GMCR-PROC-ID EQUAL PC-GMCR                 ELTHMC  
01166          PERFORM DISPLAY-RELATED-PROCEDURES-SEN.                  ELTHMC  
01167                                                                   ELTHMC  
01168                                                                   ELTHMC  
01169 ************************************************************      ELTHMC  
01170 *                                                          *      ELTHMC  
01171 *        DISPLAY RELATED PROCEDURES SENTENCE               *      ELTHMC  
01172 *                                                          *      ELTHMC  
01173 ************************************************************      ELTHMC  
01174  DISPLAY-RELATED-PROCEDURES-SEN.                                  ELTHMC  
01175      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHMC  
01176      MOVE SPECIAL-PROCEDURES-MSG TO                               ELTHMC  
01177          COF-DTL-LINE (COF-NBR-DTL-LINES).                        ELTHMC  
01178      PERFORM LINK-TO-OUTPUT.                                      ELTHMC  
01179      PERFORM GENERATE-RELATED-PROCEDURES-TE.                      ELTHMC  
01180      EJECT                                                        ELTHMC  
01181                                                                   ELTHMC  
01182                                                                   ELTHMC  
01183 ************************************************************      ELTHMC  
01184 *                                                          *      ELTHMC  
01185 *        GENERATE RELATED PROCEDURES TEXT                  *      ELTHMC  
01186 *                                                          *      ELTHMC  
01187 ************************************************************      ELTHMC  
01188  GENERATE-RELATED-PROCEDURES-TE.                                  ELTHMC  
01189      MOVE 'MANAGED CARE NETWORK' TO SRP-CCP-NAME.                 ELTHMC  
01190      MOVE WS-GMCR-PROC-ID TO SRP-TABULAR-ID.                      ELTHMC  
01191      MOVE WS-GMCR-PROC-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTHMC  
01192      PERFORM CALL-SPECIAL-PROCEDURES-GENERA.                      ELTHMC  
01193                                                                   ELTHMC  
01194                                                                   ELTHMC  
01195 ************************************************************      ELTHMC  
01196 *                                                          *      ELTHMC  
01197 *        CALL SPECIAL PROCEDURES GENERATOR                 *      ELTHMC  
01198 *                                                          *      ELTHMC  
01199 ************************************************************      ELTHMC  
01200  CALL-SPECIAL-PROCEDURES-GENERA.                                  ELTHMC  
01201      EXEC CICS LINK                                               ELTHMC  
01202                PROGRAM ('ELGGXXR')                                ELTHMC  
01203                COMMAREA (DFHCOMMAREA)                             ELTHMC  
01204         END-EXEC.                                                 ELTHMC  
01205      EJECT                                                        ELTHMC  
01206                                                                   ELTHMC  
01207                                                                   ELTHMC  
01208                                                                   ELTHMC  
01209                                                                   ELTHMC  
01210 ************************************************************      ELTHMC  
01211 *                                                          *      ELTHMC  
01212 *        GENERATE SPECIAL SERVICES TABULAR                 *      ELTHMC  
01213 *                                                          *      ELTHMC  
01214 ************************************************************      ELTHMC  
01215  GENERATE-SPECIAL-TABULAR.                                        ELTHMC  
01216      SET GCG-INDEX TO 1.                                          ELTHMC  
01217      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTHMC  
01218            AT END                                                 ELTHMC  
01219               MOVE ZEROES TO WS-GMCS-SPEC-SLOT-NO                 ELTHMC  
01220            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GMCS              ELTHMC  
01221               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTHMC  
01222          WS-GMCS-SPEC-ID                                          ELTHMC  
01223               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTHMC  
01224                   TO WS-GMCS-SPEC-SLOT-NO                         ELTHMC  
01225         END-SEARCH.                                               ELTHMC  
01226      IF WS-GMCS-SPEC-SLOT-NO NOT EQUAL ZEROES                     ELTHMC  
01227                 AND WS-GMCS-SPEC-ID EQUAL PC-GMCS                 ELTHMC  
01228          PERFORM DISPLAY-SPECIAL-SERVICES-SEN.                    ELTHMC  
01229                                                                   ELTHMC  
01230                                                                   ELTHMC  
01231 ************************************************************      ELTHMC  
01232 *                                                          *      ELTHMC  
01233 *        DISPLAY SPECIAL SERVICES SENTENCE SS              *      ELTHMC  
01234 *                                                          *      ELTHMC  
01235 ************************************************************      ELTHMC  
01236  DISPLAY-SPECIAL-SERVICES-SEN.                                    ELTHMC  
01237      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHMC  
01238      MOVE SPECIAL-SERVICES-MSG TO                                 ELTHMC  
01239          COF-DTL-LINE (COF-NBR-DTL-LINES).                        ELTHMC  
01240      PERFORM LINK-TO-OUTPUT.                                      ELTHMC  
01241      PERFORM GENERATE-SPECIAL-SERVICES-TE.                        ELTHMC  
01242      EJECT                                                        ELTHMC  
01243                                                                   ELTHMC  
01244                                                                   ELTHMC  
01245 ************************************************************      ELTHMC  
01246 *                                                          *      ELTHMC  
01247 *        GENERATE SPECIAL SERVICES TEXT                    *      ELTHMC  
01248 *                                                          *      ELTHMC  
01249 ************************************************************      ELTHMC  
01250  GENERATE-SPECIAL-SERVICES-TE.                                    ELTHMC  
01251      MOVE 'SPECIAL SERVICES' TO SRP-CCP-NAME.                     ELTHMC  
01252      MOVE WS-GMCS-SPEC-ID TO SRP-TABULAR-ID.                      ELTHMC  
01253      MOVE WS-GMCS-SPEC-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTHMC  
01254      PERFORM CALL-SPECIAL-SERVICES-GENERA.                        ELTHMC  
01255                                                                   ELTHMC  
01256                                                                   ELTHMC  
01257 ************************************************************      ELTHMC  
01258 *                                                          *      ELTHMC  
01259 *        CALL SPECIAL SERVICES GENERATOR                   *      ELTHMC  
01260 *                                                          *      ELTHMC  
01261 ************************************************************      ELTHMC  
01262  CALL-SPECIAL-SERVICES-GENERA.                                    ELTHMC  
01263      EXEC CICS LINK                                               ELTHMC  
01264                PROGRAM ('ELGGXXB')                                ELTHMC  
01265                COMMAREA (DFHCOMMAREA)                             ELTHMC  
01266         END-EXEC.                                                 ELTHMC  
01267      EJECT                                                        ELTHMC  
01268                                                                   ELTHMC  
01269                                                                   ELTHMC  
01270 ************************************************************      ELTHMC  
01271 *                                                          *      ELTHMC  
01272 *        TRANSLATE BS INDICATOR                            *      ELTHMC  
01273 *                                                          *      ELTHMC  
01274 ************************************************************      ELTHMC  
01275  TRANSLATE-BS-INDICATOR.                                          ELTHMC  
01276      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTHMC  
01277      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTHMC  
01278      MOVE 'HM-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTHMC  
01279      MOVE GSS-HM-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTHMC  
01280      PERFORM CALL-TRANSLATOR.                                     ELTHMC  
01281      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHMC  
01282      EJECT                                                        ELTHMC  
01283                                                                   ELTHMC  
01284                                                                   ELTHMC  
01285 ************************************************************      ELTHMC  
01286 *                                                          *      ELTHMC  
01287 *        TRANSLATE BS PAYMENT LEVEL INDICATOR              *      ELTHMC  
01288 *                                                          *      ELTHMC  
01289 ************************************************************      ELTHMC  
01290  TRANSLATE-BS-PAYMENT-LEVEL-IND.                                  ELTHMC  
01291      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTHMC  
01292      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTHMC  
01293      MOVE 'HM-BS-PAYMENT-LEVEL-IND' TO                            ELTHMC  
01294          CMF-ELEMENT-SYSTEM-NAME.                                 ELTHMC  
01295      MOVE GSS-HM-BS-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTHMC  
01296          CMF-CODE-VALUE.                                          ELTHMC  
01297      PERFORM CALL-TRANSLATOR.                                     ELTHMC  
01298      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTHMC  
01299      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTHMC  
01300      EJECT                                                        ELTHMC  
01301                                                                   ELTHMC  
01302                                                                   ELTHMC  
01303 ************************************************************      ELTHMC  
01304 *                                                          *      ELTHMC  
01305 *        TRANSLATE MM INDICATOR                            *      ELTHMC  
01306 *                                                          *      ELTHMC  
01307 ************************************************************      ELTHMC  
01308  TRANSLATE-MM-INDICATOR.                                          ELTHMC  
01309      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTHMC  
01310      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTHMC  
01311      MOVE 'HM-MM-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTHMC  
01312      MOVE GSS-HM-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTHMC  
01313      PERFORM CALL-TRANSLATOR.                                     ELTHMC  
01314      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHMC  
01315      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHMC  
01316      EJECT                                                        ELTHMC  
01317                                                                   ELTHMC  
01318                                                                   ELTHMC  
01319 ************************************************************      ELTHMC  
01320 *                                                          *      ELTHMC  
01321 *        TRANSLATE MM PAYMENT LEVEL INDICATOR              *      ELTHMC  
01322 *                                                          *      ELTHMC  
01323 ************************************************************      ELTHMC  
01324  TRANSLATE-MM-PAYMENT-LEVEL-IND.                                  ELTHMC  
01325      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTHMC  
01326      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTHMC  
01327      MOVE 'HM-MM-PAYMENT-LEVEL-IND' TO                            ELTHMC  
01328          CMF-ELEMENT-SYSTEM-NAME.                                 ELTHMC  
01329      MOVE GSS-HM-MM-PAYMENT-LEVEL-IND (GSS-INDEX)                 ELTHMC  
01330                                   TO CMF-CODE-VALUE.              ELTHMC  
01331      PERFORM CALL-TRANSLATOR.                                     ELTHMC  
01332      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTHMC  
01333      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTHMC  
01334      EJECT                                                        ELTHMC  
01335                                                                   ELTHMC  
01336                                                                   ELTHMC  
01337 ************************************************************      ELTHMC  
01338 *                                                          *      ELTHMC  
01339 *        GENERATE MM BENEFITS REDUCTION SENTENCE           *      ELTHMC  
01340 *                                                          *      ELTHMC  
01341 ************************************************************      ELTHMC  
01342  GENERATE-MM-BENEFITS-REDUCTION.                                  ELTHMC  
01343      IF GSS-HM-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTHMC  
01344                  ZERO AND SPACES AND LOW-VALUES                   ELTHMC  
01345          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTHMC  
01346                                                                   ELTHMC  
01347 ************************************************************      ELTHMC  
01348 *                                                          *      ELTHMC  
01349 *        GENERATE SPILL OVER INDICATOR                     *      ELTHMC  
01350 *                                                          *      ELTHMC  
01351 ************************************************************      ELTHMC  
01352  GENERATE-SPILL-OVER-INDICATOR.                                   ELTHMC  
01353      INITIALIZE ADDITIONAL-TEXT-SW.                               ELTHMC  
01354      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHMC  
01355      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHMC  
01356      SET PERIOD-NEEDED TO TRUE.                                   ELTHMC  
01357      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHMC  
01358      MOVE WS-SPILLOVER-SENTENCE TO TCAR-FROM-LINE                 ELTHMC  
01359          (TCAR-FROM-SUB).                                         ELTHMC  
01360      ADD 1 TO TCAR-FROM-SUB.                                      ELTHMC  
01361      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTHMC  
01362      MOVE 'HM-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTHMC  
01363      MOVE GSS-HM-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTHMC  
01364      PERFORM CALL-TRANSLATOR.                                     ELTHMC  
01365      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHMC  
01366                                                                   ELTHMC  
01367                                                                   ELTHMC  
01368 ************************************************************      ELTHMC  
01369 *                                                          *      ELTHMC  
01370 *        SIGNAL NOT APPLICABLE MSG                         *      ELTHMC  
01371 *                                                          *      ELTHMC  
01372 ************************************************************      ELTHMC  
01373  SIGNAL-NOT-APPLICABLE-MSG.                                       ELTHMC  
01374      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTHMC  
01375      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTHMC  
01376          (COF-NBR-DTL-LINES).                                     ELTHMC  
01377                                                                   ELTHMC  
01378                                                                   ELTHMC  
01379 ************************************************************      ELTHMC  
01380 *                                                          *      ELTHMC  
01381 *        CALL TRANSLATOR                                   *      ELTHMC  
01382 *                                                          *      ELTHMC  
01383 ************************************************************      ELTHMC  
01384  CALL-TRANSLATOR.                                                 ELTHMC  
01385      EXEC CICS LINK                                               ELTHMC  
01386           PROGRAM ('ELUCMIF')                                     ELTHMC  
01387           COMMAREA (DFHCOMMAREA)                                  ELTHMC  
01388           END-EXEC.                                               ELTHMC  
01389      EJECT                                                        ELTHMC  
01390                                                                   ELTHMC  
01391                                                                   ELTHMC  
01392 ************************************************************      ELTHMC  
01393 *                                                          *      ELTHMC  
01394 *        READ GCCP RECORD                                  *      ELTHMC  
01395 *                                                          *      ELTHMC  
01396 ************************************************************      ELTHMC  
01397  READ-GCCP-RECORD.                                                ELTHMC  
01398      SET CIA-GCTABULR-DDN TO TRUE.                                ELTHMC  
01399      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHMC  
01400           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                 ELTHMC  
01401      SET IOP-RD TO TRUE.                                          ELTHMC  
01402      SET IOP-FCQ-NONE TO TRUE.                                    ELTHMC  
01403      SET IOP-KVQ-EQ TO TRUE.                                      ELTHMC  
01404      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTHMC  
01405      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTHMC  
01406      PERFORM CALL-I-O-PGM.                                        ELTHMC  
01407      EJECT                                                        ELTHMC  
01408                                                                   ELTHMC  
01409                                                                   ELTHMC  
01410 ************************************************************      ELTHMC  
01411 *                                                          *      ELTHMC  
01412 *        CALL I O PGM                                      *      ELTHMC  
01413 *                                                          *      ELTHMC  
01414 ************************************************************      ELTHMC  
01415  CALL-I-O-PGM.                                                    ELTHMC  
01416      EXEC CICS LINK                                               ELTHMC  
01417           PROGRAM ('ELUIOPGM')                                    ELTHMC  
01418           COMMAREA (DFHCOMMAREA)                                  ELTHMC  
01419           END-EXEC.                                               ELTHMC  
01420      IF IOP-RC-OK                                                 ELTHMC  
01421          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTHMC  
01422      ELSE IF IOP-RC-NOTFND                                        ELTHMC  
01423          PERFORM SIGNAL-NOT-FOUND-GCTAB                           ELTHMC  
01424      ELSE                                                         ELTHMC  
01425          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTHMC  
01426      EJECT                                                        ELTHMC  
01427                                                                   ELTHMC  
01428                                                                   ELTHMC  
01429 ************************************************************      ELTHMC  
01430 *                                                          *      ELTHMC  
01431 *        SIGNAL CRITICAL IO ERROR                          *      ELTHMC  
01432 *                                                          *      ELTHMC  
01433 ************************************************************      ELTHMC  
01434  SIGNAL-CRITICAL-IO-ERROR.                                        ELTHMC  
01435      SET CIA-AB-CRITIO TO TRUE.                                   ELTHMC  
01436      PERFORM SIGNAL-ABEND.                                        ELTHMC  
01437                                                                   ELTHMC  
01438                                                                   ELTHMC  
01439 ************************************************************      ELTHMC  
01440 *                                                          *      ELTHMC  
01441 *        SIGNAL NOT FOUND GCTAB                            *      ELTHMC  
01442 *                                                          *      ELTHMC  
01443 ************************************************************      ELTHMC  
01444  SIGNAL-NOT-FOUND-GCTAB.                                          ELTHMC  
01445      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTHMC  
01446      PERFORM SIGNAL-ABEND.                                        ELTHMC  
01447                                                                   ELTHMC  
01448                                                                   ELTHMC  
01449 ************************************************************      ELTHMC  
01450 *                                                          *      ELTHMC  
01451 *        ESTABLISH ADDRESSABILITY OF GCCP RECORD           *      ELTHMC  
01452 *                                                          *      ELTHMC  
01453 ************************************************************      ELTHMC  
01454  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTHMC  
01455      SET ADDRESS OF GCCP-TABULAR-REC TO IOP-REC-PTR.              ELTHMC  
01456      SET IOP-REC-PTR TO NULL.                                     ELTHMC  
01457                                                                   ELTHMC  
01458                                                                   ELTHMC  
01459 ************************************************************      ELTHMC  
01460 *                                                          *      ELTHMC  
01461 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTHMC  
01462 *                                                          *      ELTHMC  
01463 ************************************************************      ELTHMC  
01464  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTHMC  
01465      PERFORM INITIALIZE-CMOUT.                                    ELTHMC  
01466      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTHMC  
01467      EJECT                                                        ELTHMC  
01468                                                                   ELTHMC  
01469                                                                   ELTHMC  
01470 ************************************************************      ELTHMC  
01471 *                                                          *      ELTHMC  
01472 *        PREPARE TEXT FOR OUTPUT                           *      ELTHMC  
01473 *                                                          *      ELTHMC  
01474 ************************************************************      ELTHMC  
01475  PREPARE-TEXT-FOR-OUTPUT.                                         ELTHMC  
01476      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTHMC  
01477          UNTIL CMF-DESCR-IDX                                      ELTHMC  
01478                                    GREATER THAN                   ELTHMC  
01479              CMF-NBR-DESCR-LINES.                                 ELTHMC  
01480      EJECT                                                        ELTHMC  
01481                                                                   ELTHMC  
01482                                                                   ELTHMC  
01483 ************************************************************      ELTHMC  
01484 *                                                          *      ELTHMC  
01485 *        INITIALIZE CMOUT                                  *      ELTHMC  
01486 *                                                          *      ELTHMC  
01487 ************************************************************      ELTHMC  
01488  INITIALIZE-CMOUT.                                                ELTHMC  
01489      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTHMC  
01490      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHMC  
01491          ADDRESS OF CMF-DESCR.                                    ELTHMC  
01492      SET CMF-DESCR-IDX TO 1.                                      ELTHMC  
01493      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTHMC  
01494                                                                   ELTHMC  
01495                                                                   ELTHMC  
01496 ************************************************************      ELTHMC  
01497 *                                                          *      ELTHMC  
01498 *        MOVE CMF TEXT TO OUTPUT                           *      ELTHMC  
01499 *                                                          *      ELTHMC  
01500 ************************************************************      ELTHMC  
01501  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTHMC  
01502      PERFORM MOVE-A-LINE.                                         ELTHMC  
01503      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTHMC  
01504          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTHMC  
01505      IF TCAR-FROM-SUB GREATER THAN 20                             ELTHMC  
01506               OR CMF-DESCR-IDX GREATER THAN                       ELTHMC  
01507          CMF-NBR-DESCR-LINES                                      ELTHMC  
01508          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTHMC  
01509                                                                   ELTHMC  
01510                                                                   ELTHMC  
01511 ************************************************************      ELTHMC  
01512 *                                                          *      ELTHMC  
01513 *        FINISH CODES MANUAL TEXT                          *      ELTHMC  
01514 *                                                          *      ELTHMC  
01515 ************************************************************      ELTHMC  
01516  FINISH-CODES-MANUAL-TEXT.                                        ELTHMC  
01517      SET DONE-PROCESSING TO TRUE.                                 ELTHMC  
01518      IF PERIOD-NEEDED                                             ELTHMC  
01519          PERFORM GET-AND-MOVE-PERIOD.                             ELTHMC  
01520                                                                   ELTHMC  
01521                                                                   ELTHMC  
01522 ************************************************************      ELTHMC  
01523 *                                                          *      ELTHMC  
01524 *        GET AND MOVE PERIOD                               *      ELTHMC  
01525 *                                                          *      ELTHMC  
01526 ************************************************************      ELTHMC  
01527  GET-AND-MOVE-PERIOD.                                             ELTHMC  
01528      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTHMC  
01529          (TCAR-FROM-SUB).                                         ELTHMC  
01530                                                                   ELTHMC  
01531                                                                   ELTHMC  
01532 ************************************************************      ELTHMC  
01533 *                                                          *      ELTHMC  
01534 *        SAVE LAST LINE                                    *      ELTHMC  
01535 *                                                          *      ELTHMC  
01536 ************************************************************      ELTHMC  
01537  SAVE-LAST-LINE.                                                  ELTHMC  
01538      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHMC  
01539      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTHMC  
01540         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTHMC  
01541      ADD 1 TO TCAR-FROM-SUB.                                      ELTHMC  
01542      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTHMC  
01543                                                                   ELTHMC  
01544                                                                   ELTHMC  
01545 ************************************************************      ELTHMC  
01546 *                                                          *      ELTHMC  
01547 *        OUTPUT LAST LINE                                  *      ELTHMC  
01548 *                                                          *      ELTHMC  
01549 ************************************************************      ELTHMC  
01550  OUTPUT-LAST-LINE.                                                ELTHMC  
01551      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTHMC  
01552          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTHMC  
01553      IF BLANK-LINE-NEEDED                                         ELTHMC  
01554          PERFORM CREATE-A-BLANK-LINE.                             ELTHMC  
01555                                                                   ELTHMC  
01556                                                                   ELTHMC  
01557 ************************************************************      ELTHMC  
01558 *                                                          *      ELTHMC  
01559 *        CREATE A BLANK LINE                               *      ELTHMC  
01560 *                                                          *      ELTHMC  
01561 ************************************************************      ELTHMC  
01562  CREATE-A-BLANK-LINE.                                             ELTHMC  
01563      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTHMC  
01564      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTHMC  
01565                                                                   ELTHMC  
01566                                                                   ELTHMC  
01567 ************************************************************      ELTHMC  
01568 *                                                          *      ELTHMC  
01569 *        MOVE A LINE                                       *      ELTHMC  
01570 *                                                          *      ELTHMC  
01571 ************************************************************      ELTHMC  
01572  MOVE-A-LINE.                                                     ELTHMC  
01573      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTHMC  
01574          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTHMC  
01575      SET CMF-DESCR-IDX UP BY 1.                                   ELTHMC  
01576      ADD 1 TO TCAR-FROM-SUB.                                      ELTHMC  
01577      EJECT                                                        ELTHMC  
01578                                                                   ELTHMC  
01579                                                                   ELTHMC  
01580 ************************************************************      ELTHMC  
01581 *                                                          *      ELTHMC  
01582 *        REFORMAT AND WRITE TEXT                           *      ELTHMC  
01583 *                                                          *      ELTHMC  
01584 ************************************************************      ELTHMC  
01585  REFORMAT-AND-WRITE-TEXT.                                         ELTHMC  
01586      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTHMC  
01587      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTHMC  
01588      PERFORM UNSTRING-TEXT.                                       ELTHMC  
01589      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHMC  
01590      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHMC  
01591      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTHMC  
01592          UNTIL COF-NBR-DTL-LINES GREATER                          ELTHMC  
01593                                   TCAR-OUTPUT-FIELDS-USED -       ELTHMC  
01594              1.                                                   ELTHMC  
01595      PERFORM DISPOSE-OF-LAST-LINE.                                ELTHMC  
01596      PERFORM LINK-TO-OUTPUT.                                      ELTHMC  
01597                                                                   ELTHMC  
01598                                                                   ELTHMC  
01599 ************************************************************      ELTHMC  
01600 *                                                          *      ELTHMC  
01601 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTHMC  
01602 *                                                          *      ELTHMC  
01603 ************************************************************      ELTHMC  
01604  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTHMC  
01605      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTHMC  
01606           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTHMC  
01607      ADD +1 TO TCAR-FROM-SUB.                                     ELTHMC  
01608      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTHMC  
01609      EJECT                                                        ELTHMC  
01610                                                                   ELTHMC  
01611                                                                   ELTHMC  
01612 ************************************************************      ELTHMC  
01613 *                                                          *      ELTHMC  
01614 *        UNSTRING TEXT                                     *      ELTHMC  
01615 *                                                          *      ELTHMC  
01616 ************************************************************      ELTHMC  
01617  UNSTRING-TEXT.                                                   ELTHMC  
01618      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTHMC  
01619      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTHMC  
01620      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTHMC  
01621      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTHMC  
01622      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTHMC  
01623      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTHMC  
01624      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTHMC  
01625      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTHMC  
01626      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTHMC  
01627      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTHMC  
01628      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTHMC  
01629      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTHMC  
01630      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTHMC  
01631      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTHMC  
01632      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTHMC  
01633      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTHMC  
01634      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTHMC  
01635      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTHMC  
01636      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTHMC  
01637      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTHMC  
01638      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTHMC  
01639      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTHMC  
01640      EJECT                                                        ELTHMC  
01641                                                                   ELTHMC  
01642                                                                   ELTHMC  
01643 ************************************************************      ELTHMC  
01644 *                                                          *      ELTHMC  
01645 *        LINK TO OUTPUT                                    *      ELTHMC  
01646 *                                                          *      ELTHMC  
01647 ************************************************************      ELTHMC  
01648  LINK-TO-OUTPUT.                                                  ELTHMC  
01649      EXEC CICS LINK                                               ELTHMC  
01650          PROGRAM ('ELUOUTPT')                                     ELTHMC  
01651          COMMAREA (DFHCOMMAREA)                                   ELTHMC  
01652          END-EXEC.                                                ELTHMC  
01653      EJECT                                                        ELTHMC  
01654                                                                   ELTHMC  
01655                                                                   ELTHMC  
01656 ************************************************************      ELTHMC  
01657 *                                                          *      ELTHMC  
01658 *        DISPOSE OF LAST LINE                              *      ELTHMC  
01659 *                                                          *      ELTHMC  
01660 ************************************************************      ELTHMC  
01661  DISPOSE-OF-LAST-LINE.                                            ELTHMC  
01662      IF NOT ADDITIONAL-TEXT                                       ELTHMC  
01663          PERFORM INITIALIZE-CONTINUED-SW.                         ELTHMC  
01664      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTHMC  
01665          PERFORM SAVE-LAST-LINE                                   ELTHMC  
01666      ELSE                                                         ELTHMC  
01667          PERFORM OUTPUT-LAST-LINE.                                ELTHMC  
01668                                                                   ELTHMC  
01669                                                                   ELTHMC  
01670 ************************************************************      ELTHMC  
01671 *                                                          *      ELTHMC  
01672 *        INITIALIZE CONTINUED SW                           *      ELTHMC  
01673 *                                                          *      ELTHMC  
01674 ************************************************************      ELTHMC  
01675  INITIALIZE-CONTINUED-SW.                                         ELTHMC  
01676      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTHMC  
