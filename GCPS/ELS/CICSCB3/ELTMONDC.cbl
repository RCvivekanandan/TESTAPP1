00001 *      LAST MAINTENANCE TIME:  9.11.53  DATE: 06/12/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTMONDC
00003                                                                      LV001
00004  PROGRAM-ID.         ELTMONDC.                                    ELTMONDC
00005                                                                   ELTMONDC
00006  AUTHOR.             ANNE KEFFER KING.                            ELTMONDC
00007                                                                   ELTMONDC
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTMONDC
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTMONDC
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTMONDC
00011                      233 N. MICHIGAN AVE                          ELTMONDC
00012                      CHICAGO, ILLINOIS 60601                      ELTMONDC
00013                                                                   ELTMONDC
00014  DATE-WRITTEN.       99-XXX-9999.                                 ELTMONDC
00015                                                                   ELTMONDC
00016  DATE-COMPILED.                                                   ELTMONDC
00017                                                                   ELTMONDC
00018  SECURITY.           COPYRIGHT 1986,                              ELTMONDC
00019                      HEALTH CARE SERVICE CORPORATION              ELTMONDC
00020 ******************************************************************ELTMONDC
00021 * OVERVIEW:                                                      *ELTMONDC
00022 *                                                                *ELTMONDC
00023 * RECORDS                                                        *ELTMONDC
00024 * ACCESSED: ELCDCIA RECORD                                       *ELTMONDC
00025 *           GROUP SPECIFIC RECORD                                *ELTMONDC
00026 *           #GCCP TABULAR RECORD                                 *ELTMONDC
00027 *                                                                *ELTMONDC
00028 * PROCESSING                                                     *ELTMONDC
00029 * FUNCTIONS: THIS MODULE PERFORMS THE FOLLOWING FUNCTIONS:       *ELTMONDC
00030 *                                                                *ELTMONDC
00031 *            1. INITIALIZES WORK DATA ELEMENTS.                  *ELTMONDC
00032 *                                                                *ELTMONDC
00033 *            2. ACQUIRE NECESSARY RECORDS FOR PROCESSING.        *ELTMONDC
00034 *                                                                *ELTMONDC
00035 *            3. PROCESS MONDAY DISCHARGE PROGRAM TABULAR         *ELTMONDC
00036 *               RECORD FOR BLUE CROSS PRODUCING OUTPUT           *ELTMONDC
00037 *               TEXT AS REQUIRED.                                *ELTMONDC
00038 *                                                                *ELTMONDC
00039 *            4. PROCESS MONDAY DISCHARGE PROGRAM TABULAR         *ELTMONDC
00040 *               RECORD FOR BLUE SHIELD PRODUCING OUTPUT          *ELTMONDC
00041 *               TEXT AS REQUIRED.                                *ELTMONDC
00042 *                                                                *ELTMONDC
00043 *            5. PROCESS MONDAY DISCHARGE PROGRAM TABULAR         *ELTMONDC
00044 *               RECORD FOR MAJOR MEDICAL PRODUCING OUTPUT        *ELTMONDC
00045 *             TEXT AS REQUIRED.                                  *ELTMONDC
00046 *                                                                *ELTMONDC
00047 ******************************************************************ELTMONDC
00048 *                     MAINTENANCE HISTORY                        *ELTMONDC
00049 *                                                                *ELTMONDC
00050 *  MOD     DATE     BY  DPRT       ACTION                        *ELTMONDC
00051 * ----- ----------- --- ----- ---------------------------------- *ELTMONDC
00052 * 01.00 99-XXX-9999 AKK       CREATED                            *ELTMONDC
00053 *                                                                *ELTMONDC
00054 * 01.01 01-NOV-1990 JPB       CHANGED STORAGE MANAGEMENT         *ELTMONDC
00055 *                                                                *ELTMONDC
00056 * 01.02 09-NOV-1990 JPB       CHANGED REFERENCES TO GCG-MONDAY-  *ELTMONDC
00057 *                             DISCHARGE-IND TO ACCOMODATE FOR    *ELTMONDC
00058 *                             CHANGES IN FIELD SIZE.             *ELTMONDC
00059 *                                                                *ELTMONDC
00060 * 01.03 11-JUN-1991 JPB       ADDED TRANSLATION AND DISPLAY OF   *ELTMONDC
00061 *                             PARTICIPATION INDICATOR (GCG-      *ELTMONDC
00062 *                             MONDAY-DISCHARGE-IND).             *ELTMONDC
00063 *                                                                *ELTMONDC
00064 ******************************************************************ELTMONDC
00065      SKIP3                                                        ELTMONDC
00066  ENVIRONMENT DIVISION.                                            ELTMONDC
00067                                                                   ELTMONDC
00068  CONFIGURATION SECTION.                                           ELTMONDC
00069  SOURCE-COMPUTER.    IBM-3090.                                    ELTMONDC
00070  OBJECT-COMPUTER.    IBM-3090.                                    ELTMONDC
00071      EJECT                                                        ELTMONDC
00072                                                                   ELTMONDC
00073  DATA DIVISION.                                                   ELTMONDC
00074  WORKING-STORAGE SECTION.                                         ELTMONDC
00075  01  WS-BEGIN                    PIC X(26) VALUE                  ELTMONDC
00076                                 '*** ELTMONDC WS BEGINS ***'.     ELTMONDC
00077  01  WS-MISC-FIELDS.                                              ELTMONDC
00078      05  SCREEN-TYPE              PIC X     VALUE SPACES.         ELTMONDC
00079          88  INSTITUTIONAL-SCREEN           VALUE 'I'.            ELTMONDC
00080          88  PROFESSIONAL-SCREEN            VALUE 'P' .           ELTMONDC
00081          88  SUPPLEMENTAL-SCREEN            VALUE 'S' .           ELTMONDC
00082 *                                                                 ELTMONDC
00083      05  WS-POINTER2              POINTER.                        ELTMONDC
00084      05  WS-POINTER3              POINTER.                        ELTMONDC
00085 *                                                                 ELTMONDC
00086      05  WS-GMDB-SRVS-ID          PIC X(06) VALUE SPACES.         ELTMONDC
00087      05  WS-GMDB-SRVS-SLOT-NO     PIC S9(04)  COMP-3              ELTMONDC
00088                                             VALUE ZERO.           ELTMONDC
00089 *                                                                 ELTMONDC
00090  01  WS-SWITCHES.                                                 ELTMONDC
00091      05  UNDEFINED-TABULAR-SW     PIC X     VALUE 'N'.            ELTMONDC
00092          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTMONDC
00093 *                                                                 ELTMONDC
00094      05  DEFINED-TABULAR-SW       PIC X     VALUE 'N'.            ELTMONDC
00095          88  TABULAR-IS-DEFINED             VALUE 'Y'.            ELTMONDC
00096 *                                                                 ELTMONDC
00097      05  ADDITIONAL-TEXT-SW       PIC X     VALUE SPACES.         ELTMONDC
00098          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTMONDC
00099          88  ADDITIONAL-TEXT                VALUE 'Y'.            ELTMONDC
00100 *                                                                 ELTMONDC
00101      05  CONTINUED-PROCESSING-SW  PIC X     VALUE SPACES.         ELTMONDC
00102          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTMONDC
00103          88  DONE-PROCESSING                VALUE 'D'.            ELTMONDC
00104 *                                                                 ELTMONDC
00105      05  WS-PERIOD-SW             PIC X     VALUE 'N'.            ELTMONDC
00106          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTMONDC
00107 ******************************************************************ELTMONDC
00108 * SCREEN BODY LINES                                               ELTMONDC
00109 ******************************************************************ELTMONDC
00110  01  WS-HDR-LN2.                                                  ELTMONDC
00111      05  FILLER                  PIC X(25)   VALUE SPACES.        ELTMONDC
00112      05  FILLER                  PIC X(17)   VALUE                ELTMONDC
00113          'MONDAY DISCHARGE'.                                      ELTMONDC
00114      05  WS-HDR-TITLE            PIC X(13)   VALUE SPACES.        ELTMONDC
00115      05  FILLER                  PIC X(24)   VALUE SPACES.        ELTMONDC
00116 *                                                                 ELTMONDC
00117  01  WS-PARTICIPATION-LINE.                                       ELTMONDC
00118      05  FILLER                  PIC X(79)   VALUE                ELTMONDC
00119          'THE MONDAY DISCHARGE PROGRAM APPLIES TO '.              ELTMONDC
00120 *                                                                 ELTMONDC
00121  01  WS-APPROVAL-SOURCE.                                          ELTMONDC
00122      05  FILLER                  PIC X(79)   VALUE                ELTMONDC
00123          'THE MONDAY DISCHARGE APPROVAL SOURCE IS '.              ELTMONDC
00124 *                                                                 ELTMONDC
00125  01  WS-BC-INDICATOR.                                             ELTMONDC
00126      05  FILLER                  PIC X(79)  VALUE                 ELTMONDC
00127          'THE PROGRAM CRITERIA FOR BLUE CROSS '.                  ELTMONDC
00128 *                                                                 ELTMONDC
00129  01  WS-BS-INDICATOR.                                             ELTMONDC
00130      05  FILLER                  PIC X(79)  VALUE                 ELTMONDC
00131          'THE PROGRAM CRITERIA FOR BLUE SHIELD '.                 ELTMONDC
00132 *                                                                 ELTMONDC
00133  01  WS-MM-INDICATOR.                                             ELTMONDC
00134      05  FILLER                  PIC X(79)  VALUE                 ELTMONDC
00135          'THE PROGRAM CRITERIA FOR MAJOR MEDICAL '.               ELTMONDC
00136 *                                                                 ELTMONDC
00137  01  WS-BENEFITS-REDUCTION.                                       ELTMONDC
00138      05  FILLER                 PIC X(79)   VALUE                 ELTMONDC
00139          'DENIED OR REDUCED BENEFITS DUE TO COST CONTAINMENT:'.   ELTMONDC
00140 *                                                                 ELTMONDC
00141  01  WS-SPILL-OVER.                                               ELTMONDC
00142      05  FILLER                PIC X(79)   VALUE                  ELTMONDC
00143          'UNPAID SERVICES AFTER BASIC BENEFIT REDUCTIONS ARE '.   ELTMONDC
00144 *                                                                 ELTMONDC
00145  01  SPECIAL-SERVICES-MSG.                                        ELTMONDC
00146      05  FILLER               PIC X(79)   VALUE                   ELTMONDC
00147          'THERE ARE SPECIAL RELATED SERVICES INCLUDED IN THIS PROGELTMONDC
00148 -        'RAM.'.                                                  ELTMONDC
00149 *                                                                 ELTMONDC
00150  01  WS-NOT-APPLICABLE-LOB-BC.                                    ELTMONDC
00151      05  FILLER              PIC  X(79)   VALUE                   ELTMONDC
00152          'THE MONDAY DISCHARGE PROGRAM DOES NOT APPLY TO INSTITUTIELTMONDC
00153 -        'ONAL BENEFITS.'.                                        ELTMONDC
00154 *                                                                 ELTMONDC
00155  01  WS-NOT-APPLICABLE-LOB-BS.                                    ELTMONDC
00156      05  FILLER              PIC  X(79)   VALUE                   ELTMONDC
00157          'THE MONDAY DISCHARGE PROGRAM DOES NOT APPLY TO PROFESSIOELTMONDC
00158 -        'NAL BENEFITS.'.                                         ELTMONDC
00159 *                                                                 ELTMONDC
00160  01  WS-NOT-APPLICABLE-LOB-MM.                                    ELTMONDC
00161      05  FILLER              PIC  X(79) VALUE                     ELTMONDC
00162          'THE MONDAY DISCHARGE PROGRAM DOES NOT APPLY TO SUPPLEMENELTMONDC
00163 -        'TAL BENEFITS.'.                                         ELTMONDC
00164 *                                                                 ELTMONDC
00165  01  WS-NOT-APPLICABLE-MSG.                                       ELTMONDC
00166      05  FILLER                  PIC  X(79) VALUE                 ELTMONDC
00167          'THE MONDAY DISCHARGE PROGRAM IS NOT APPLICABLE.'.       ELTMONDC
00168 *                                                                 ELTMONDC
00169  01  WS-VOLUNTARY-MSG.                                            ELTMONDC
00170      05  FILLER                  PIC  X(79) VALUE                 ELTMONDC
00171          'THE MONDAY DISCHARGE PROGRAM IS VOLUNTARY.'.            ELTMONDC
00172 *                                                                 ELTMONDC
00173  01  WS-DISCLAIMER.                                               ELTMONDC
00174      05  FILLER                 PIC X(79)  VALUE                  ELTMONDC
00175          '*** SUBJECT TO CONTRACT LIMITATIONS ***'.               ELTMONDC
00176 *                                                                 ELTMONDC
00177  01  WS-END                       PIC X(18) VALUE                 ELTMONDC
00178                                          '*** END OF W/S ***'.    ELTMONDC
00179                                                                   ELTMONDC
00180  01  PROGRAM-CONSTANTS.                                           ELTMONDC
00181      05  PC-GCCP                  PIC X(06)  VALUE '#GCCP '.      ELTMONDC
00182      05  PC-GMDB                  PIC X(06)  VALUE '#GMDB '.      ELTMONDC
00183      05  PC-GROUP                 PIC X(06)  VALUE 'GROUP '.      ELTMONDC
00184 *                                                                 ELTMONDC
00185  LINKAGE SECTION.                                                 ELTMONDC
00186  01  DFHCOMMAREA.                                                 ELTMONDC
00187      COPY ELSCOMMC.                                               ELTMONDC
00188 /                                                                 ELTMONDC
00189      COPY ELSCIA2C.                                               ELTMONDC
00190 /                                                                 ELTMONDC
00191      COPY ELSCMDSC.                                               ELTMONDC
00192 /                                                                 ELTMONDC
00193      COPY ELSCMIFC.                                               ELTMONDC
00194 /                                                                 ELTMONDC
00195      COPY ELSIOPMC.                                               ELTMONDC
00196 /                                                                 ELTMONDC
00197      COPY ELSKEYSC.                                               ELTMONDC
00198 /                                                                 ELTMONDC
00199      COPY ELSOUTPC.                                               ELTMONDC
00200 /                                                                 ELTMONDC
00201      COPY ELSSRTPC.                                               ELTMONDC
00202 /                                                                 ELTMONDC
00203      COPY ELSTCWAC.                                               ELTMONDC
00204 /                                                                 ELTMONDC
00205      COPY ELSSSCBC.                                               ELTMONDC
00206 /                                                                 ELTMONDC
00207  01  GROUP-SPECIFIC-RECORD.                                       ELTMONDC
00208      COPY GCGROUPC.                                               ELTMONDC
00209 /                                                                 ELTMONDC
00210  01  GCCP-TABULAR-REC.                                            ELTMONDC
00211      COPY GCTGCCPC.                                               ELTMONDC
00212 /                                                                 ELTMONDC
00213      EJECT                                                        ELTMONDC
00214  PROCEDURE DIVISION.                                              ELTMONDC
00215 ************************************************************      ELTMONDC
00216 *                                                          *      ELTMONDC
00217 *                    PROCEDURE DIVISION                    *      ELTMONDC
00218 *                                                          *      ELTMONDC
00219 ************************************************************      ELTMONDC
00220                                                                   ELTMONDC
00221                                                                   ELTMONDC
00222 ************************************************************      ELTMONDC
00223 *                                                          *      ELTMONDC
00224 *        MONDAY DISCHARGE PROGRAM                          *      ELTMONDC
00225 *                                                          *      ELTMONDC
00226 ************************************************************      ELTMONDC
00227  MONDAY-DISCHARGE-PROGRAM.                                        ELTMONDC
00228      PERFORM INITIALIZATION.                                      ELTMONDC
00229      PERFORM PROCESS-MONDAY-DISCHARGE.                            ELTMONDC
00230      GOBACK.                                                      ELTMONDC
00231                                                                   ELTMONDC
00232                                                                   ELTMONDC
00233 ************************************************************      ELTMONDC
00234 *                                                          *      ELTMONDC
00235 *        INITIALIZATION                                    *      ELTMONDC
00236 *                                                          *      ELTMONDC
00237 ************************************************************      ELTMONDC
00238  INITIALIZATION.                                                  ELTMONDC
00239      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTMONDC
00240      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTMONDC
00241                                                                   ELTMONDC
00242                                                                   ELTMONDC
00243 ************************************************************      ELTMONDC
00244 *                                                          *      ELTMONDC
00245 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTMONDC
00246 *                                                          *      ELTMONDC
00247 ************************************************************      ELTMONDC
00248  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTMONDC
00249      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTMONDC
00250      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTMONDC
00251      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTMONDC
00252                                                                   ELTMONDC
00253                                                                   ELTMONDC
00254 ************************************************************      ELTMONDC
00255 *                                                          *      ELTMONDC
00256 *        CHECK FOR VALID COMMAREA                          *      ELTMONDC
00257 *                                                          *      ELTMONDC
00258 ************************************************************      ELTMONDC
00259  CHECK-FOR-VALID-COMMAREA.                                        ELTMONDC
00260      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTMONDC
00261          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTMONDC
00262                                                                   ELTMONDC
00263                                                                   ELTMONDC
00264 ************************************************************      ELTMONDC
00265 *                                                          *      ELTMONDC
00266 *        SIGNAL INVALID COMMAREA                           *      ELTMONDC
00267 *                                                          *      ELTMONDC
00268 ************************************************************      ELTMONDC
00269  SIGNAL-INVALID-COMMAREA.                                         ELTMONDC
00270      EXEC CICS ABEND                                              ELTMONDC
00271                ABCODE('EL01')                                     ELTMONDC
00272         END-EXEC.                                                 ELTMONDC
00273      EJECT                                                        ELTMONDC
00274                                                                   ELTMONDC
00275                                                                   ELTMONDC
00276 ************************************************************      ELTMONDC
00277 *                                                          *      ELTMONDC
00278 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTMONDC
00279 *                                                          *      ELTMONDC
00280 ************************************************************      ELTMONDC
00281  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTMONDC
00282      IF ECA-CIA-PTR = NULL                                        ELTMONDC
00283          PERFORM SIGNAL-INVALID-CIA                               ELTMONDC
00284      ELSE                                                         ELTMONDC
00285          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTMONDC
00286                                                                   ELTMONDC
00287                                                                   ELTMONDC
00288 ************************************************************      ELTMONDC
00289 *                                                          *      ELTMONDC
00290 *        SIGNAL INVALID CIA                                *      ELTMONDC
00291 *                                                          *      ELTMONDC
00292 ************************************************************      ELTMONDC
00293  SIGNAL-INVALID-CIA.                                              ELTMONDC
00294      EXEC CICS ABEND                                              ELTMONDC
00295                ABCODE('EL02')                                     ELTMONDC
00296         END-EXEC.                                                 ELTMONDC
00297                                                                   ELTMONDC
00298                                                                   ELTMONDC
00299 ************************************************************      ELTMONDC
00300 *                                                          *      ELTMONDC
00301 *        ESTABLISH ADDRESS OF CIA                          *      ELTMONDC
00302 *                                                          *      ELTMONDC
00303 ************************************************************      ELTMONDC
00304  ESTABLISH-ADDRESS-OF-CIA.                                        ELTMONDC
00305      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTMONDC
00306                            ADDRESS OF                             ELTMONDC
00307          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTMONDC
00308      EJECT                                                        ELTMONDC
00309                                                                   ELTMONDC
00310                                                                   ELTMONDC
00311 ************************************************************      ELTMONDC
00312 *                                                          *      ELTMONDC
00313 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTMONDC
00314 *                                                          *      ELTMONDC
00315 ************************************************************      ELTMONDC
00316  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTMONDC
00317      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTMONDC
00318      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMONDC
00319                            ADDRESS OF                             ELTMONDC
00320          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTMONDC
00321      IF CIA-RC-PTR-NULL                                           ELTMONDC
00322          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMONDC
00323                                                                   ELTMONDC
00324                                                                   ELTMONDC
00325 ************************************************************      ELTMONDC
00326 *                                                          *      ELTMONDC
00327 *        SIGNAL UNALLOC AREA ERROR                         *      ELTMONDC
00328 *                                                          *      ELTMONDC
00329 ************************************************************      ELTMONDC
00330  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTMONDC
00331      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTMONDC
00332      PERFORM SIGNAL-ABEND.                                        ELTMONDC
00333                                                                   ELTMONDC
00334                                                                   ELTMONDC
00335 ************************************************************      ELTMONDC
00336 *                                                          *      ELTMONDC
00337 *        SIGNAL ABEND                                      *      ELTMONDC
00338 *                                                          *      ELTMONDC
00339 ************************************************************      ELTMONDC
00340  SIGNAL-ABEND.                                                    ELTMONDC
00341      EXEC CICS ABEND                                              ELTMONDC
00342                ABCODE(CIA-ABCODE)                                 ELTMONDC
00343         END-EXEC.                                                 ELTMONDC
00344      EJECT                                                        ELTMONDC
00345                                                                   ELTMONDC
00346                                                                   ELTMONDC
00347 ************************************************************      ELTMONDC
00348 *                                                          *      ELTMONDC
00349 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTMONDC
00350 *                                                          *      ELTMONDC
00351 ************************************************************      ELTMONDC
00352  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTMONDC
00353      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTMONDC
00354      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTMONDC
00355      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTMONDC
00356      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTMONDC
00357      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTMONDC
00358      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTMONDC
00359      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTMONDC
00360                                                                   ELTMONDC
00361                                                                   ELTMONDC
00362 ************************************************************      ELTMONDC
00363 *                                                          *      ELTMONDC
00364 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTMONDC
00365 *                                                          *      ELTMONDC
00366 ************************************************************      ELTMONDC
00367  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTMONDC
00368      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTMONDC
00369      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMONDC
00370                            ADDRESS OF                             ELTMONDC
00371          CMF-CODES-MANUAL-INTERFACE.                              ELTMONDC
00372      IF CIA-RC-PTR-NULL                                           ELTMONDC
00373          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMONDC
00374      EJECT                                                        ELTMONDC
00375                                                                   ELTMONDC
00376                                                                   ELTMONDC
00377 ************************************************************      ELTMONDC
00378 *                                                          *      ELTMONDC
00379 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTMONDC
00380 *                                                          *      ELTMONDC
00381 ************************************************************      ELTMONDC
00382  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTMONDC
00383      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTMONDC
00384      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMONDC
00385                            ADDRESS OF                             ELTMONDC
00386          COF-OUTPUT-INTERFACE.                                    ELTMONDC
00387      IF CIA-RC-PTR-NULL                                           ELTMONDC
00388          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMONDC
00389      EJECT                                                        ELTMONDC
00390                                                                   ELTMONDC
00391                                                                   ELTMONDC
00392 ************************************************************      ELTMONDC
00393 *                                                          *      ELTMONDC
00394 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTMONDC
00395 *                                                          *      ELTMONDC
00396 ************************************************************      ELTMONDC
00397  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTMONDC
00398      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTMONDC
00399      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMONDC
00400                            ADDRESS OF                             ELTMONDC
00401          SRP-SUBROUTINE-PARAMETERS.                               ELTMONDC
00402      IF CIA-RC-PTR-NULL                                           ELTMONDC
00403          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMONDC
00404      EJECT                                                        ELTMONDC
00405                                                                   ELTMONDC
00406                                                                   ELTMONDC
00407 ************************************************************      ELTMONDC
00408 *                                                          *      ELTMONDC
00409 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTMONDC
00410 *                                                          *      ELTMONDC
00411 ************************************************************      ELTMONDC
00412  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTMONDC
00413      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTMONDC
00414      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMONDC
00415                            ADDRESS OF                             ELTMONDC
00416          TCAR-COMPRESSION-WORK-AREA.                              ELTMONDC
00417      IF CIA-RC-PTR-NULL                                           ELTMONDC
00418          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMONDC
00419      EJECT                                                        ELTMONDC
00420                                                                   ELTMONDC
00421                                                                   ELTMONDC
00422 ************************************************************      ELTMONDC
00423 *                                                          *      ELTMONDC
00424 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTMONDC
00425 *                                                          *      ELTMONDC
00426 ************************************************************      ELTMONDC
00427  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTMONDC
00428      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTMONDC
00429      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMONDC
00430                            ADDRESS OF                             ELTMONDC
00431          KWA-FILE-KEY-WORK-AREA.                                  ELTMONDC
00432      IF CIA-RC-PTR-NULL                                           ELTMONDC
00433          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMONDC
00434      EJECT                                                        ELTMONDC
00435                                                                   ELTMONDC
00436                                                                   ELTMONDC
00437 ************************************************************      ELTMONDC
00438 *                                                          *      ELTMONDC
00439 *        ESTABLISH ADDRESSABILITY OF GRP SPECIFIC          *      ELTMONDC
00440 *                                                          *      ELTMONDC
00441 ************************************************************      ELTMONDC
00442  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTMONDC
00443      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTMONDC
00444      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMONDC
00445                            ADDRESS OF                             ELTMONDC
00446          GROUP-SPECIFIC-RECORD.                                   ELTMONDC
00447      IF CIA-RC-PTR-NULL                                           ELTMONDC
00448          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMONDC
00449      EJECT                                                        ELTMONDC
00450                                                                   ELTMONDC
00451                                                                   ELTMONDC
00452 ************************************************************      ELTMONDC
00453 *                                                          *      ELTMONDC
00454 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT       *      ELTMONDC
00455 *                                                          *      ELTMONDC
00456 ************************************************************      ELTMONDC
00457  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTMONDC
00458      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMONDC
00459      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMONDC
00460                            ADDRESS OF GCCP-TABULAR-REC.           ELTMONDC
00461      IF CIA-RC-PTR-NULL                                           ELTMONDC
00462          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMONDC
00463      EJECT                                                        ELTMONDC
00464                                                                   ELTMONDC
00465                                                                   ELTMONDC
00466 ************************************************************      ELTMONDC
00467 *                                                          *      ELTMONDC
00468 *        PROCESS MONDAY DISCHARGE                          *      ELTMONDC
00469 *                                                          *      ELTMONDC
00470 ************************************************************      ELTMONDC
00471  PROCESS-MONDAY-DISCHARGE.                                        ELTMONDC
00472      IF GCG-MONDAY-DISCHARGE-IND EQUAL ZERO                       ELTMONDC
00473                  OR '08'                                          ELTMONDC
00474          PERFORM TEST-APPLICABILITY                               ELTMONDC
00475      ELSE                                                         ELTMONDC
00476          PERFORM GENERATE-MONDAY-DISCHARGE-TEXT.                  ELTMONDC
00477      MOVE 'E' TO  COF-FUNCTION.                                   ELTMONDC
00478      MOVE ZEROS TO COF-NBR-DTL-LINES                              ELTMONDC
00479                    COF-NBR-HDR-LINES.                             ELTMONDC
00480      PERFORM LINK-TO-OUTPUT.                                      ELTMONDC
00481      EJECT                                                        ELTMONDC
00482                                                                   ELTMONDC
00483                                                                   ELTMONDC
00484 ************************************************************      ELTMONDC
00485 *                                                          *      ELTMONDC
00486 *        GENERATE MONDAY DISCHARGE TEXT                    *      ELTMONDC
00487 *                                                          *      ELTMONDC
00488 ************************************************************      ELTMONDC
00489  GENERATE-MONDAY-DISCHARGE-TEXT.                                  ELTMONDC
00490      PERFORM VERIFY-MONDAY-DISCHARGE-IN-GCC.                      ELTMONDC
00491      PERFORM BUILD-MONDAY-DISCHARGE-TEXT.                         ELTMONDC
00492                                                                   ELTMONDC
00493                                                                   ELTMONDC
00494 ************************************************************      ELTMONDC
00495 *                                                          *      ELTMONDC
00496 *        TEST APPLICABILITY                                *      ELTMONDC
00497 *                                                          *      ELTMONDC
00498 ************************************************************      ELTMONDC
00499  TEST-APPLICABILITY.                                              ELTMONDC
00500      PERFORM GENERATE-HEADINGS.                                   ELTMONDC
00501      IF GCG-MONDAY-DISCHARGE-IND  EQUAL ZERO                      ELTMONDC
00502          PERFORM SIGNAL-NOT-APPLICABLE-MSG                        ELTMONDC
00503      ELSE IF GCG-MONDAY-DISCHARGE-IND  EQUAL  '08'                ELTMONDC
00504          PERFORM SIGNAL-VOLUNTARY-MSG.                            ELTMONDC
00505                                                                   ELTMONDC
00506                                                                   ELTMONDC
00507 ************************************************************      ELTMONDC
00508 *                                                          *      ELTMONDC
00509 *        VERIFY MONDAY DISCHARGE IN GCCP RECORD            *      ELTMONDC
00510 *                                                          *      ELTMONDC
00511 ************************************************************      ELTMONDC
00512  VERIFY-MONDAY-DISCHARGE-IN-GCC.                                  ELTMONDC
00513      PERFORM ACQUIRE-GCCP-RECORD.                                 ELTMONDC
00514      PERFORM OBTAIN-MONDAY-DISCHARGE-WITHIN.                      ELTMONDC
00515      EJECT                                                        ELTMONDC
00516                                                                   ELTMONDC
00517                                                                   ELTMONDC
00518 ************************************************************      ELTMONDC
00519 *                                                          *      ELTMONDC
00520 *        ACQUIRE GCCP RECORD                               *      ELTMONDC
00521 *                                                          *      ELTMONDC
00522 ************************************************************      ELTMONDC
00523  ACQUIRE-GCCP-RECORD.                                             ELTMONDC
00524      MOVE SPACES TO KWA-PROVISION-ID.                             ELTMONDC
00525      SET GCG-INDEX TO 1.                                          ELTMONDC
00526      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMONDC
00527          AT END                                                   ELTMONDC
00528             MOVE ZEROES TO KWA-PROVISION-SLOT-NO                  ELTMONDC
00529          WHEN GCG-TAB-ID (GCG-INDEX) = PC-GCCP                    ELTMONDC
00530                 MOVE GCG-TAB-ID (GCG-INDEX) TO                    ELTMONDC
00531          KWA-PROVISION-ID                                         ELTMONDC
00532                 MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                  ELTMONDC
00533                     TO KWA-PROVISION-SLOT-NO                      ELTMONDC
00534          END-SEARCH.                                              ELTMONDC
00535      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTMONDC
00536          PERFORM SIGNAL-UNDEFINED-TABULAR                         ELTMONDC
00537      ELSE                                                         ELTMONDC
00538          PERFORM READ-GCCP-RECORD.                                ELTMONDC
00539                                                                   ELTMONDC
00540                                                                   ELTMONDC
00541 ************************************************************      ELTMONDC
00542 *                                                          *      ELTMONDC
00543 *        SIGNAL UNDEFINED TABULAR                          *      ELTMONDC
00544 *                                                          *      ELTMONDC
00545 ************************************************************      ELTMONDC
00546  SIGNAL-UNDEFINED-TABULAR.                                        ELTMONDC
00547      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTMONDC
00548      PERFORM SIGNAL-ABEND.                                        ELTMONDC
00549                                                                   ELTMONDC
00550                                                                   ELTMONDC
00551 ************************************************************      ELTMONDC
00552 *                                                          *      ELTMONDC
00553 *        OBTAIN MONDAY DISCHARGE WITHIN GCCP RECORD        *      ELTMONDC
00554 *                                                          *      ELTMONDC
00555 ************************************************************      ELTMONDC
00556  OBTAIN-MONDAY-DISCHARGE-WITHIN.                                  ELTMONDC
00557      SET GSS-INDEX TO 1.                                          ELTMONDC
00558      SEARCH GSS-ENTRY                                             ELTMONDC
00559         AT END                                                    ELTMONDC
00560            SET TABULAR-IS-UNDEFINED TO TRUE                       ELTMONDC
00561         WHEN GSS-MD-PROG-CODE-CHR (GSS-INDEX)                     ELTMONDC
00562            SET TABULAR-IS-DEFINED TO TRUE                         ELTMONDC
00563          END-SEARCH.                                              ELTMONDC
00564      IF TABULAR-IS-UNDEFINED                                      ELTMONDC
00565          PERFORM SIGNAL-UNDEFINED-TABULAR.                        ELTMONDC
00566      EJECT                                                        ELTMONDC
00567                                                                   ELTMONDC
00568                                                                   ELTMONDC
00569 ************************************************************      ELTMONDC
00570 *                                                          *      ELTMONDC
00571 *        BUILD MONDAY DISCHARGE TEXT                       *      ELTMONDC
00572 *                                                          *      ELTMONDC
00573 ************************************************************      ELTMONDC
00574  BUILD-MONDAY-DISCHARGE-TEXT.                                     ELTMONDC
00575      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTMONDC
00576          PERFORM GENERATE-INSTITUTIONAL.                          ELTMONDC
00577      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTMONDC
00578          PERFORM GENERATE-PROFESSIONAL.                           ELTMONDC
00579      IF GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                        ELTMONDC
00580                 '03' OR '04' OR '06' OR '08'                      ELTMONDC
00581          PERFORM GENERATE-SUPPLEMENTAL.                           ELTMONDC
00582                                                                   ELTMONDC
00583                                                                   ELTMONDC
00584 ************************************************************      ELTMONDC
00585 *                                                          *      ELTMONDC
00586 *        GENERATE INSTITUTIONAL                            *      ELTMONDC
00587 *                                                          *      ELTMONDC
00588 ************************************************************      ELTMONDC
00589  GENERATE-INSTITUTIONAL.                                          ELTMONDC
00590      SET INSTITUTIONAL-SCREEN TO TRUE.                            ELTMONDC
00591      PERFORM GENERATE-HEADINGS.                                   ELTMONDC
00592      PERFORM BUILD-INSTITUTIONAL-TEXT.                            ELTMONDC
00593      EJECT                                                        ELTMONDC
00594                                                                   ELTMONDC
00595                                                                   ELTMONDC
00596 ************************************************************      ELTMONDC
00597 *                                                          *      ELTMONDC
00598 *        GENERATE PROFESSIONAL                             *      ELTMONDC
00599 *                                                          *      ELTMONDC
00600 ************************************************************      ELTMONDC
00601  GENERATE-PROFESSIONAL.                                           ELTMONDC
00602      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTMONDC
00603      PERFORM GENERATE-HEADINGS.                                   ELTMONDC
00604      PERFORM BUILD-PROFESSIONAL-TEXT.                             ELTMONDC
00605      EJECT                                                        ELTMONDC
00606                                                                   ELTMONDC
00607                                                                   ELTMONDC
00608 ************************************************************      ELTMONDC
00609 *                                                          *      ELTMONDC
00610 *        GENERATE SUPPLEMENTAL                             *      ELTMONDC
00611 *                                                          *      ELTMONDC
00612 ************************************************************      ELTMONDC
00613  GENERATE-SUPPLEMENTAL.                                           ELTMONDC
00614      SET SUPPLEMENTAL-SCREEN TO TRUE.                             ELTMONDC
00615      PERFORM GENERATE-HEADINGS.                                   ELTMONDC
00616      PERFORM BUILD-SUPPLEMENTAL-TEXT.                             ELTMONDC
00617      EJECT                                                        ELTMONDC
00618                                                                   ELTMONDC
00619                                                                   ELTMONDC
00620 ************************************************************      ELTMONDC
00621 *                                                          *      ELTMONDC
00622 *        GENERATE HEADINGS                                 *      ELTMONDC
00623 *                                                          *      ELTMONDC
00624 ************************************************************      ELTMONDC
00625  GENERATE-HEADINGS.                                               ELTMONDC
00626      SET COF-NEW-PAGE TO TRUE.                                    ELTMONDC
00627      IF INSTITUTIONAL-SCREEN                                      ELTMONDC
00628          PERFORM MOVE-INST-HEADINGS                               ELTMONDC
00629      ELSE IF PROFESSIONAL-SCREEN                                  ELTMONDC
00630          PERFORM MOVE-PROF-HEADINGS                               ELTMONDC
00631      ELSE IF SUPPLEMENTAL-SCREEN                                  ELTMONDC
00632          PERFORM MOVE-SUPP-HEADINGS.                              ELTMONDC
00633      MOVE 2 TO COF-NBR-HDR-LINES.                                 ELTMONDC
00634      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMONDC
00635      MOVE WS-HDR-LN2 TO COF-HDR-LINE                              ELTMONDC
00636          (COF-NBR-HDR-LINES).                                     ELTMONDC
00637      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMONDC
00638      IF INSTITUTIONAL-SCREEN                                      ELTMONDC
00639             OR PROFESSIONAL-SCREEN                                ELTMONDC
00640             OR SUPPLEMENTAL-SCREEN                                ELTMONDC
00641          PERFORM LINK-TO-OUTPUT.                                  ELTMONDC
00642                                                                   ELTMONDC
00643                                                                   ELTMONDC
00644 ************************************************************      ELTMONDC
00645 *                                                          *      ELTMONDC
00646 *        MOVE INST HEADINGS                                *      ELTMONDC
00647 *                                                          *      ELTMONDC
00648 ************************************************************      ELTMONDC
00649  MOVE-INST-HEADINGS.                                              ELTMONDC
00650      MOVE 'INSTITUTIONAL' TO WS-HDR-TITLE.                        ELTMONDC
00651                                                                   ELTMONDC
00652                                                                   ELTMONDC
00653 ************************************************************      ELTMONDC
00654 *                                                          *      ELTMONDC
00655 *        MOVE PROF HEADINGS                                *      ELTMONDC
00656 *                                                          *      ELTMONDC
00657 ************************************************************      ELTMONDC
00658  MOVE-PROF-HEADINGS.                                              ELTMONDC
00659      MOVE 'PROFESSIONAL' TO WS-HDR-TITLE.                         ELTMONDC
00660                                                                   ELTMONDC
00661                                                                   ELTMONDC
00662 ************************************************************      ELTMONDC
00663 *                                                          *      ELTMONDC
00664 *        MOVE SUPP HEADINGS                                *      ELTMONDC
00665 *                                                          *      ELTMONDC
00666 ************************************************************      ELTMONDC
00667  MOVE-SUPP-HEADINGS.                                              ELTMONDC
00668      MOVE 'SUPPLEMENTAL' TO WS-HDR-TITLE.                         ELTMONDC
00669                                                                   ELTMONDC
00670                                                                   ELTMONDC
00671 ************************************************************      ELTMONDC
00672 *                                                          *      ELTMONDC
00673 *        BUILD INSTITUTIONAL TEXT                          *      ELTMONDC
00674 *                                                          *      ELTMONDC
00675 ************************************************************      ELTMONDC
00676  BUILD-INSTITUTIONAL-TEXT.                                        ELTMONDC
00677      IF GSS-MD-BC-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTMONDC
00678                 OR LOW-VALUES                                     ELTMONDC
00679          PERFORM SIGNAL-NOT-APPLICABLE-FOR-CROS                   ELTMONDC
00680      ELSE                                                         ELTMONDC
00681          PERFORM CONSTRUCT-BC-TEXT-AND-SCREEN.                    ELTMONDC
00682                                                                   ELTMONDC
00683                                                                   ELTMONDC
00684 ************************************************************      ELTMONDC
00685 *                                                          *      ELTMONDC
00686 *        BUILD PROFESSIONAL TEXT                           *      ELTMONDC
00687 *                                                          *      ELTMONDC
00688 ************************************************************      ELTMONDC
00689  BUILD-PROFESSIONAL-TEXT.                                         ELTMONDC
00690      IF GSS-MD-BS-IND (GSS-INDEX)  EQUAL ZEROES OR SPACES         ELTMONDC
00691                 OR LOW-VALUES                                     ELTMONDC
00692          PERFORM SIGNAL-NOT-APPLICABLE-FOR-SHEI                   ELTMONDC
00693      ELSE                                                         ELTMONDC
00694          PERFORM CONSTRUCT-BS-TEXT-AND-SCREEN.                    ELTMONDC
00695                                                                   ELTMONDC
00696                                                                   ELTMONDC
00697 ************************************************************      ELTMONDC
00698 *                                                          *      ELTMONDC
00699 *        BUILD SUPPLEMENTAL TEXT                           *      ELTMONDC
00700 *                                                          *      ELTMONDC
00701 ************************************************************      ELTMONDC
00702  BUILD-SUPPLEMENTAL-TEXT.                                         ELTMONDC
00703      IF GSS-MD-MM-IND (GSS-INDEX)  EQUAL ZEROES OR SPACES         ELTMONDC
00704                  OR LOW-VALUES                                    ELTMONDC
00705          PERFORM SIGNAL-NOT-APPLICABLE-FOR-LOBX                   ELTMONDC
00706      ELSE                                                         ELTMONDC
00707          PERFORM CONSTRUCT-MM-TEXT-AND-SCREEN.                    ELTMONDC
00708      EJECT                                                        ELTMONDC
00709                                                                   ELTMONDC
00710                                                                   ELTMONDC
00711 ************************************************************      ELTMONDC
00712 *                                                          *      ELTMONDC
00713 *        SIGNAL NOT APPLICABLE FOR CROSS                   *      ELTMONDC
00714 *                                                          *      ELTMONDC
00715 ************************************************************      ELTMONDC
00716  SIGNAL-NOT-APPLICABLE-FOR-CROS.                                  ELTMONDC
00717      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMONDC
00718      MOVE WS-NOT-APPLICABLE-LOB-BC TO COF-DTL-LINE                ELTMONDC
00719          (COF-NBR-DTL-LINES).                                     ELTMONDC
00720      PERFORM LINK-TO-OUTPUT.                                      ELTMONDC
00721                                                                   ELTMONDC
00722                                                                   ELTMONDC
00723 ************************************************************      ELTMONDC
00724 *                                                          *      ELTMONDC
00725 *        SIGNAL NOT APPLICABLE FOR SHEILD                  *      ELTMONDC
00726 *                                                          *      ELTMONDC
00727 ************************************************************      ELTMONDC
00728  SIGNAL-NOT-APPLICABLE-FOR-SHEI.                                  ELTMONDC
00729      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMONDC
00730      MOVE WS-NOT-APPLICABLE-LOB-BS TO COF-DTL-LINE                ELTMONDC
00731          (COF-NBR-DTL-LINES).                                     ELTMONDC
00732      PERFORM LINK-TO-OUTPUT.                                      ELTMONDC
00733                                                                   ELTMONDC
00734                                                                   ELTMONDC
00735 ************************************************************      ELTMONDC
00736 *                                                          *      ELTMONDC
00737 *        SIGNAL NOT APPLICABLE FOR LOB MM                  *      ELTMONDC
00738 *                                                          *      ELTMONDC
00739 ************************************************************      ELTMONDC
00740  SIGNAL-NOT-APPLICABLE-FOR-LOBX.                                  ELTMONDC
00741      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMONDC
00742      MOVE WS-NOT-APPLICABLE-LOB-MM TO COF-DTL-LINE                ELTMONDC
00743          (COF-NBR-DTL-LINES).                                     ELTMONDC
00744      PERFORM LINK-TO-OUTPUT.                                      ELTMONDC
00745      EJECT                                                        ELTMONDC
00746                                                                   ELTMONDC
00747                                                                   ELTMONDC
00748 ************************************************************      ELTMONDC
00749 *                                                          *      ELTMONDC
00750 *        CONSTRUCT BC TEXT AND SCREEN                      *      ELTMONDC
00751 *                                                          *      ELTMONDC
00752 ************************************************************      ELTMONDC
00753  CONSTRUCT-BC-TEXT-AND-SCREEN.                                    ELTMONDC
00754      PERFORM TRANSLATE-PARTICIPATION-INDICA.                      ELTMONDC
00755      IF GSS-MD-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMONDC
00756          ZERO                                                     ELTMONDC
00757                 AND SPACES AND LOW-VALUES                         ELTMONDC
00758          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMONDC
00759      PERFORM TRANSLATE-BC-INDICATOR.                              ELTMONDC
00760      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTMONDC
00761      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTMONDC
00762      PERFORM GENERATE-BC-CALC-METHOD-SENTEN.                      ELTMONDC
00763      PERFORM GENERATE-BC-BENEFITS-REDUCTION.                      ELTMONDC
00764      PERFORM GENERATE-GMDB-TABULAR-SENTENCE.                      ELTMONDC
00765      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTMONDC
00766               '03' OR '04' OR '06' OR '08')                       ELTMONDC
00767            AND                                                    ELTMONDC
00768             (GSS-MD-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL          ELTMONDC
00769          ZERO                                                     ELTMONDC
00770                         AND SPACES AND LOW-VALUES)                ELTMONDC
00771          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTMONDC
00772      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMONDC
00773      MOVE SPACES TO SCREEN-TYPE.                                  ELTMONDC
00774      EJECT                                                        ELTMONDC
00775                                                                   ELTMONDC
00776                                                                   ELTMONDC
00777 ************************************************************      ELTMONDC
00778 *                                                          *      ELTMONDC
00779 *        CONSTRUCT BS TEXT AND SCREEN                      *      ELTMONDC
00780 *                                                          *      ELTMONDC
00781 ************************************************************      ELTMONDC
00782  CONSTRUCT-BS-TEXT-AND-SCREEN.                                    ELTMONDC
00783      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTMONDC
00784      PERFORM TRANSLATE-PARTICIPATION-INDICA.                      ELTMONDC
00785      IF GSS-MD-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMONDC
00786          ZERO                                                     ELTMONDC
00787                 AND SPACES AND LOW-VALUES                         ELTMONDC
00788          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMONDC
00789      PERFORM TRANSLATE-BS-INDICATOR.                              ELTMONDC
00790      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTMONDC
00791      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTMONDC
00792      PERFORM GENERATE-BS-CALC-METHOD-SENTEN.                      ELTMONDC
00793      PERFORM GENERATE-BS-BENEFITS-REDUCTION.                      ELTMONDC
00794      PERFORM GENERATE-GMDB-TABULAR-SENTENCE.                      ELTMONDC
00795      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTMONDC
00796                   '03' OR '04' OR '06' OR '08')                   ELTMONDC
00797            AND                                                    ELTMONDC
00798             (GSS-MD-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL          ELTMONDC
00799          ZERO                                                     ELTMONDC
00800                         AND SPACES AND LOW-VALUES)                ELTMONDC
00801          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTMONDC
00802      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMONDC
00803      MOVE SPACES TO SCREEN-TYPE.                                  ELTMONDC
00804      EJECT                                                        ELTMONDC
00805                                                                   ELTMONDC
00806                                                                   ELTMONDC
00807 ************************************************************      ELTMONDC
00808 *                                                          *      ELTMONDC
00809 *        CONSTRUCT MM TEXT AND SCREEN                      *      ELTMONDC
00810 *                                                          *      ELTMONDC
00811 ************************************************************      ELTMONDC
00812  CONSTRUCT-MM-TEXT-AND-SCREEN.                                    ELTMONDC
00813      PERFORM TRANSLATE-PARTICIPATION-INDICA.                      ELTMONDC
00814      IF GSS-MD-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMONDC
00815          ZERO                                                     ELTMONDC
00816                 AND SPACES AND LOW-VALUES                         ELTMONDC
00817          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMONDC
00818      PERFORM TRANSLATE-MM-INDICATOR.                              ELTMONDC
00819      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTMONDC
00820      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTMONDC
00821      PERFORM GENERATE-MM-CALC-METHOD-SENTEN.                      ELTMONDC
00822      PERFORM GENERATE-MM-BENEFITS-REDUCTION.                      ELTMONDC
00823      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMONDC
00824      EJECT                                                        ELTMONDC
00825                                                                   ELTMONDC
00826                                                                   ELTMONDC
00827 ************************************************************      ELTMONDC
00828 *                                                          *      ELTMONDC
00829 *        TRANSLATE APPROVAL SOURCE                         *      ELTMONDC
00830 *                                                          *      ELTMONDC
00831 ************************************************************      ELTMONDC
00832  TRANSLATE-APPROVAL-SOURCE.                                       ELTMONDC
00833      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
00834      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMONDC
00835      SET PERIOD-NEEDED TO TRUE.                                   ELTMONDC
00836      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
00837      MOVE WS-APPROVAL-SOURCE TO TCAR-FROM-LINE                    ELTMONDC
00838          (TCAR-FROM-SUB).                                         ELTMONDC
00839      ADD 1 TO TCAR-FROM-SUB.                                      ELTMONDC
00840      MOVE 'MD-APPROVAL-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTMONDC
00841      MOVE GSS-MD-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELTMONDC
00842          CMF-CODE-VALUE.                                          ELTMONDC
00843      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
00844      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
00845      EJECT                                                        ELTMONDC
00846                                                                   ELTMONDC
00847                                                                   ELTMONDC
00848 ************************************************************      ELTMONDC
00849 *                                                          *      ELTMONDC
00850 *        TRANSLATE PARTICIPATION INDICATOR                 *      ELTMONDC
00851 *                                                          *      ELTMONDC
00852 ************************************************************      ELTMONDC
00853  TRANSLATE-PARTICIPATION-INDICA.                                  ELTMONDC
00854      INITIALIZE TCAR-FROM-AREA.                                   ELTMONDC
00855      SET PERIOD-NEEDED TO TRUE.                                   ELTMONDC
00856      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
00857      MOVE WS-PARTICIPATION-LINE TO TCAR-FROM-LINE                 ELTMONDC
00858          (TCAR-FROM-SUB).                                         ELTMONDC
00859      ADD 1 TO TCAR-FROM-SUB.                                      ELTMONDC
00860      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMONDC
00861      MOVE GCG-MONDAY-DISCHARGE-IND TO CMF-CODE-VALUE.             ELTMONDC
00862      MOVE 'MONDAY-DISCHARGE-IND' TO CMF-ELEMENT-SYSTEM-NAME.      ELTMONDC
00863      PERFORM GROUP-LINK-TO-TRANSLATOR.                            ELTMONDC
00864      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
00865      EJECT                                                        ELTMONDC
00866                                                                   ELTMONDC
00867                                                                   ELTMONDC
00868 ************************************************************      ELTMONDC
00869 *                                                          *      ELTMONDC
00870 *        TRANSLATE BC INDICATOR                            *      ELTMONDC
00871 *                                                          *      ELTMONDC
00872 ************************************************************      ELTMONDC
00873  TRANSLATE-BC-INDICATOR.                                          ELTMONDC
00874      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
00875      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMONDC
00876      SET PERIOD-NEEDED TO TRUE.                                   ELTMONDC
00877      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
00878      MOVE WS-BC-INDICATOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).      ELTMONDC
00879      ADD +1 TO TCAR-FROM-SUB.                                     ELTMONDC
00880      MOVE 'MD-BC-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTMONDC
00881      MOVE GSS-MD-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMONDC
00882      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
00883      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
00884      EJECT                                                        ELTMONDC
00885                                                                   ELTMONDC
00886                                                                   ELTMONDC
00887 ************************************************************      ELTMONDC
00888 *                                                          *      ELTMONDC
00889 *        GENERATE ASSOCIATED ACCUMULATORS                  *      ELTMONDC
00890 *                                                          *      ELTMONDC
00891 ************************************************************      ELTMONDC
00892  GENERATE-ASSOCIATED-ACCUMULATO.                                  ELTMONDC
00893      PERFORM GENERATE-COINSURANCE-TEXT.                           ELTMONDC
00894      PERFORM GENERATE-COPAY-TEXT.                                 ELTMONDC
00895      PERFORM GENERATE-DEDUCTIBLE-TEXT.                            ELTMONDC
00896      PERFORM GENERATE-BENEFIT-MAXIMUMS-TEXT.                      ELTMONDC
00897                                                                   ELTMONDC
00898                                                                   ELTMONDC
00899 ************************************************************      ELTMONDC
00900 *                                                          *      ELTMONDC
00901 *        GENERATE COPAY TEXT                               *      ELTMONDC
00902 *                                                          *      ELTMONDC
00903 ************************************************************      ELTMONDC
00904  GENERATE-COPAY-TEXT.                                             ELTMONDC
00905      EXEC CICS LINK                                               ELTMONDC
00906          PROGRAM ('ELGACPCC')                                     ELTMONDC
00907          COMMAREA (DFHCOMMAREA)                                   ELTMONDC
00908          END-EXEC.                                                ELTMONDC
00909                                                                   ELTMONDC
00910 ************************************************************      ELTMONDC
00911 *                                                          *      ELTMONDC
00912 *        GENERATE COINSURANCE TEXT                         *      ELTMONDC
00913 *                                                          *      ELTMONDC
00914 ************************************************************      ELTMONDC
00915  GENERATE-COINSURANCE-TEXT.                                       ELTMONDC
00916      EXEC CICS LINK                                               ELTMONDC
00917          PROGRAM ('ELGACLCC')                                     ELTMONDC
00918          COMMAREA (DFHCOMMAREA)                                   ELTMONDC
00919          END-EXEC.                                                ELTMONDC
00920                                                                   ELTMONDC
00921                                                                   ELTMONDC
00922 ************************************************************      ELTMONDC
00923 *                                                          *      ELTMONDC
00924 *        GENERATE DEDUCTIBLE TEXT                          *      ELTMONDC
00925 *                                                          *      ELTMONDC
00926 ************************************************************      ELTMONDC
00927  GENERATE-DEDUCTIBLE-TEXT.                                        ELTMONDC
00928      EXEC CICS LINK                                               ELTMONDC
00929          PROGRAM ('ELGADLCC')                                     ELTMONDC
00930          COMMAREA (DFHCOMMAREA)                                   ELTMONDC
00931          END-EXEC.                                                ELTMONDC
00932                                                                   ELTMONDC
00933                                                                   ELTMONDC
00934 ************************************************************      ELTMONDC
00935 *                                                          *      ELTMONDC
00936 *        GENERATE BENEFIT MAXIMUMS TEXT                    *      ELTMONDC
00937 *                                                          *      ELTMONDC
00938 ************************************************************      ELTMONDC
00939  GENERATE-BENEFIT-MAXIMUMS-TEXT.                                  ELTMONDC
00940      EXEC CICS LINK                                               ELTMONDC
00941          PROGRAM ('ELGABMCC')                                     ELTMONDC
00942          COMMAREA (DFHCOMMAREA)                                   ELTMONDC
00943          END-EXEC.                                                ELTMONDC
00944      EJECT                                                        ELTMONDC
00945                                                                   ELTMONDC
00946                                                                   ELTMONDC
00947 ************************************************************      ELTMONDC
00948 *                                                          *      ELTMONDC
00949 *        GENERATE BC CALC METHOD SENTENCE                  *      ELTMONDC
00950 *                                                          *      ELTMONDC
00951 ************************************************************      ELTMONDC
00952  GENERATE-BC-CALC-METHOD-SENTEN.                                  ELTMONDC
00953      IF GSS-MD-BC-CALC-METHOD (GSS-INDEX)  NOT EQUAL ZEROES       ELTMONDC
00954               AND SPACES AND LOW-VALUES                           ELTMONDC
00955          PERFORM CREATE-BC-CALC-SENTENCE.                         ELTMONDC
00956                                                                   ELTMONDC
00957                                                                   ELTMONDC
00958 ************************************************************      ELTMONDC
00959 *                                                          *      ELTMONDC
00960 *        CREATE BC CALC SENTENCE                           *      ELTMONDC
00961 *                                                          *      ELTMONDC
00962 ************************************************************      ELTMONDC
00963  CREATE-BC-CALC-SENTENCE.                                         ELTMONDC
00964      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
00965      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMONDC
00966      SET PERIOD-NEEDED TO TRUE.                                   ELTMONDC
00967      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
00968      MOVE 'MD-BC-CALC-METHOD' TO                                  ELTMONDC
00969          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMONDC
00970      MOVE GSS-MD-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMONDC
00971      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
00972      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
00973      EJECT                                                        ELTMONDC
00974                                                                   ELTMONDC
00975                                                                   ELTMONDC
00976 ************************************************************      ELTMONDC
00977 *                                                          *      ELTMONDC
00978 *        GENERATE BS CALC METHOD SENTENCE                  *      ELTMONDC
00979 *                                                          *      ELTMONDC
00980 ************************************************************      ELTMONDC
00981  GENERATE-BS-CALC-METHOD-SENTEN.                                  ELTMONDC
00982      IF GSS-MD-BS-CALC-METHOD (GSS-INDEX)  NOT EQUAL ZEROES       ELTMONDC
00983               AND SPACES AND LOW-VALUES                           ELTMONDC
00984          PERFORM CREATE-BS-CALC-SENTENCE.                         ELTMONDC
00985                                                                   ELTMONDC
00986                                                                   ELTMONDC
00987 ************************************************************      ELTMONDC
00988 *                                                          *      ELTMONDC
00989 *        CREATE BS CALC SENTENCE                           *      ELTMONDC
00990 *                                                          *      ELTMONDC
00991 ************************************************************      ELTMONDC
00992  CREATE-BS-CALC-SENTENCE.                                         ELTMONDC
00993      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
00994      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMONDC
00995      SET PERIOD-NEEDED TO TRUE.                                   ELTMONDC
00996      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
00997      MOVE 'MD-BS-CALC-METHOD' TO                                  ELTMONDC
00998          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMONDC
00999      MOVE GSS-MD-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMONDC
01000      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01001      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01002                                                                   ELTMONDC
01003                                                                   ELTMONDC
01004 ************************************************************      ELTMONDC
01005 *                                                          *      ELTMONDC
01006 *        GENERATE MM CALC METHOD SENTENCE                  *      ELTMONDC
01007 *                                                          *      ELTMONDC
01008 ************************************************************      ELTMONDC
01009  GENERATE-MM-CALC-METHOD-SENTEN.                                  ELTMONDC
01010      IF GSS-MD-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTMONDC
01011               AND SPACES AND LOW-VALUES                           ELTMONDC
01012          PERFORM CREATE-MM-CALC-SENTENCE.                         ELTMONDC
01013      EJECT                                                        ELTMONDC
01014                                                                   ELTMONDC
01015                                                                   ELTMONDC
01016 ************************************************************      ELTMONDC
01017 *                                                          *      ELTMONDC
01018 *        CREATE MM CALC SENTENCE                           *      ELTMONDC
01019 *                                                          *      ELTMONDC
01020 ************************************************************      ELTMONDC
01021  CREATE-MM-CALC-SENTENCE.                                         ELTMONDC
01022      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01023      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMONDC
01024      SET PERIOD-NEEDED TO TRUE.                                   ELTMONDC
01025      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01026      MOVE 'MD-MM-CALC-METHOD' TO                                  ELTMONDC
01027          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMONDC
01028      MOVE GSS-MD-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMONDC
01029      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01030      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01031                                                                   ELTMONDC
01032                                                                   ELTMONDC
01033 ************************************************************      ELTMONDC
01034 *                                                          *      ELTMONDC
01035 *        GENERATE COMBINED BENEFITS REDUCTION SENTENCE     *      ELTMONDC
01036 *                                                          *      ELTMONDC
01037 ************************************************************      ELTMONDC
01038  GENERATE-COMBINED-BENEFITS-RED.                                  ELTMONDC
01039      MOVE 'MONDAY DISCHARGE' TO SRP-CCP-NAME.                     ELTMONDC
01040      MOVE 'MD' TO SRP-COST-CONT-TYPE.                             ELTMONDC
01041      MOVE GSS-MD-COMB-BENE-REDUCT-IND (GSS-INDEX) TO              ELTMONDC
01042             SRP-CCP-COMB-BENE-REDUCT-IND.                         ELTMONDC
01043      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTMONDC
01044      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMONDC
01045                            ADDRESS OF GCCP-TABULAR-REC.           ELTMONDC
01046      CALL 'ELGCBRI' USING DFHEIBLK                                ELTMONDC
01047                           DFHCOMMAREA.                            ELTMONDC
01048      EJECT                                                        ELTMONDC
01049                                                                   ELTMONDC
01050                                                                   ELTMONDC
01051 ************************************************************      ELTMONDC
01052 *                                                          *      ELTMONDC
01053 *        GENERATE RELATED SERVICES                         *      ELTMONDC
01054 *                                                          *      ELTMONDC
01055 ************************************************************      ELTMONDC
01056  GENERATE-RELATED-SERVICES.                                       ELTMONDC
01057      MOVE 'MONDAY DISCHARGE' TO SRP-CCP-NAME.                     ELTMONDC
01058      MOVE WS-GMDB-SRVS-ID TO SRP-TABULAR-ID.                      ELTMONDC
01059      MOVE WS-GMDB-SRVS-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTMONDC
01060      EXEC CICS LINK                                               ELTMONDC
01061          PROGRAM ('ELGGXXB')                                      ELTMONDC
01062          COMMAREA (DFHCOMMAREA)                                   ELTMONDC
01063          END-EXEC.                                                ELTMONDC
01064      EJECT                                                        ELTMONDC
01065                                                                   ELTMONDC
01066                                                                   ELTMONDC
01067 ************************************************************      ELTMONDC
01068 *                                                          *      ELTMONDC
01069 *        GENERATE DISCLAIMER SENTENCE                      *      ELTMONDC
01070 *                                                          *      ELTMONDC
01071 ************************************************************      ELTMONDC
01072  GENERATE-DISCLAIMER-SENTENCE.                                    ELTMONDC
01073      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMONDC
01074      MOVE WS-DISCLAIMER TO COF-DTL-LINE                           ELTMONDC
01075          (COF-NBR-DTL-LINES).                                     ELTMONDC
01076      PERFORM LINK-TO-OUTPUT.                                      ELTMONDC
01077      EJECT                                                        ELTMONDC
01078                                                                   ELTMONDC
01079                                                                   ELTMONDC
01080 ************************************************************      ELTMONDC
01081 *                                                          *      ELTMONDC
01082 *        GENERATE BC BENEFITS REDUCTION SENTENCE           *      ELTMONDC
01083 *                                                          *      ELTMONDC
01084 ************************************************************      ELTMONDC
01085  GENERATE-BC-BENEFITS-REDUCTION.                                  ELTMONDC
01086      IF GSS-MD-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMONDC
01087          ZERO                                                     ELTMONDC
01088                  AND SPACES AND LOW-VALUES                        ELTMONDC
01089          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMONDC
01090      IF (GSS-MD-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTMONDC
01091          ZERO                                                     ELTMONDC
01092                             AND SPACES AND LOW-VALUES) OR         ELTMONDC
01093                 (GSS-MD-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT        ELTMONDC
01094          EQUAL ZERO                                               ELTMONDC
01095                             AND SPACES AND LOW-VALUES) OR         ELTMONDC
01096                 (GSS-MD-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTMONDC
01097          EQUAL ZERO                                               ELTMONDC
01098                             AND SPACES AND LOW-VALUES)            ELTMONDC
01099          PERFORM GENERATE-BC-BEN-REDUCT-TEXT.                     ELTMONDC
01100      EJECT                                                        ELTMONDC
01101                                                                   ELTMONDC
01102                                                                   ELTMONDC
01103 ************************************************************      ELTMONDC
01104 *                                                          *      ELTMONDC
01105 *        GENERATE BC BEN REDUCT TEXT                       *      ELTMONDC
01106 *                                                          *      ELTMONDC
01107 ************************************************************      ELTMONDC
01108  GENERATE-BC-BEN-REDUCT-TEXT.                                     ELTMONDC
01109      PERFORM GENERATE-BENEFITS-REDUCTION-HE.                      ELTMONDC
01110      IF GSS-MD-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMONDC
01111          ZERO                                                     ELTMONDC
01112                             AND SPACES AND LOW-VALUES             ELTMONDC
01113          PERFORM TRANSLATE-BC-DEDU-APPLIC.                        ELTMONDC
01114      IF GSS-MD-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMONDC
01115          ZERO                                                     ELTMONDC
01116                             AND SPACES AND LOW-VALUES             ELTMONDC
01117          PERFORM TRANSLATE-BC-OPEX-APPLIC.                        ELTMONDC
01118      PERFORM CREATE-A-BLANK-LINE.                                 ELTMONDC
01119      PERFORM LINK-TO-OUTPUT.                                      ELTMONDC
01120      IF GSS-MD-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMONDC
01121          ZERO                                                     ELTMONDC
01122                             AND SPACES AND LOW-VALUES             ELTMONDC
01123          PERFORM TRANSLATE-BC-OPEX-OVERRIDE.                      ELTMONDC
01124      EJECT                                                        ELTMONDC
01125                                                                   ELTMONDC
01126                                                                   ELTMONDC
01127 ************************************************************      ELTMONDC
01128 *                                                          *      ELTMONDC
01129 *        TRANSLATE BS INDICATOR                            *      ELTMONDC
01130 *                                                          *      ELTMONDC
01131 ************************************************************      ELTMONDC
01132  TRANSLATE-BS-INDICATOR.                                          ELTMONDC
01133      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01134      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMONDC
01135      SET PERIOD-NEEDED TO TRUE.                                   ELTMONDC
01136      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01137      MOVE WS-BS-INDICATOR TO TCAR-FROM-LINE                       ELTMONDC
01138          (TCAR-FROM-SUB).                                         ELTMONDC
01139      ADD 1 TO TCAR-FROM-SUB.                                      ELTMONDC
01140      MOVE 'MD-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTMONDC
01141      MOVE GSS-MD-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMONDC
01142      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01143      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01144      EJECT                                                        ELTMONDC
01145                                                                   ELTMONDC
01146                                                                   ELTMONDC
01147 ************************************************************      ELTMONDC
01148 *                                                          *      ELTMONDC
01149 *        GENERATE BS BENEFITS REDUCTION SENTENCE           *      ELTMONDC
01150 *                                                          *      ELTMONDC
01151 ************************************************************      ELTMONDC
01152  GENERATE-BS-BENEFITS-REDUCTION.                                  ELTMONDC
01153      IF GSS-MD-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMONDC
01154          ZERO                                                     ELTMONDC
01155                  AND SPACES AND LOW-VALUES                        ELTMONDC
01156          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMONDC
01157      IF ((GSS-MD-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTMONDC
01158          ZERO                                                     ELTMONDC
01159                             AND SPACES AND LOW-VALUES)) OR        ELTMONDC
01160                 ((GSS-MD-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT       ELTMONDC
01161          EQUAL ZERO                                               ELTMONDC
01162                             AND SPACES AND LOW-VALUES)) OR        ELTMONDC
01163                 ((GSS-MD-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT     ELTMONDC
01164          EQUAL ZERO                                               ELTMONDC
01165                             AND SPACES AND LOW-VALUES))           ELTMONDC
01166          PERFORM GENERATE-BS-BEN-REDUCT-TEXT.                     ELTMONDC
01167      EJECT                                                        ELTMONDC
01168                                                                   ELTMONDC
01169                                                                   ELTMONDC
01170 ************************************************************      ELTMONDC
01171 *                                                          *      ELTMONDC
01172 *        GENERATE BS BEN REDUCT TEXT                       *      ELTMONDC
01173 *                                                          *      ELTMONDC
01174 ************************************************************      ELTMONDC
01175  GENERATE-BS-BEN-REDUCT-TEXT.                                     ELTMONDC
01176      PERFORM GENERATE-BENEFITS-REDUCTION-HE.                      ELTMONDC
01177      IF GSS-MD-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMONDC
01178          ZERO                                                     ELTMONDC
01179                             AND SPACES AND LOW-VALUES             ELTMONDC
01180          PERFORM TRANSLATE-BS-DEDU-APPLIC.                        ELTMONDC
01181      IF GSS-MD-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMONDC
01182          ZERO                                                     ELTMONDC
01183                             AND SPACES AND LOW-VALUES             ELTMONDC
01184          PERFORM TRANSLATE-BS-OPEX-APPLIC.                        ELTMONDC
01185      PERFORM CREATE-A-BLANK-LINE.                                 ELTMONDC
01186      PERFORM LINK-TO-OUTPUT.                                      ELTMONDC
01187      IF GSS-MD-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMONDC
01188          ZERO                                                     ELTMONDC
01189                             AND SPACES AND LOW-VALUES             ELTMONDC
01190          PERFORM TRANSLATE-BS-OPEX-OVERRIDE.                      ELTMONDC
01191      EJECT                                                        ELTMONDC
01192                                                                   ELTMONDC
01193                                                                   ELTMONDC
01194 ************************************************************      ELTMONDC
01195 *                                                          *      ELTMONDC
01196 *        GENERATE BENEFITS REDUCTION HEADING               *      ELTMONDC
01197 *                                                          *      ELTMONDC
01198 ************************************************************      ELTMONDC
01199  GENERATE-BENEFITS-REDUCTION-HE.                                  ELTMONDC
01200      INITIALIZE WS-PERIOD-SW.                                     ELTMONDC
01201      INITIALIZE ADDITIONAL-TEXT-SW.                               ELTMONDC
01202      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMONDC
01203      MOVE WS-BENEFITS-REDUCTION TO COF-DTL-LINE                   ELTMONDC
01204          (COF-NBR-DTL-LINES).                                     ELTMONDC
01205      PERFORM LINK-TO-OUTPUT.                                      ELTMONDC
01206      EJECT                                                        ELTMONDC
01207                                                                   ELTMONDC
01208                                                                   ELTMONDC
01209 ************************************************************      ELTMONDC
01210 *                                                          *      ELTMONDC
01211 *        TRANSLATE MM INDICATOR                            *      ELTMONDC
01212 *                                                          *      ELTMONDC
01213 ************************************************************      ELTMONDC
01214  TRANSLATE-MM-INDICATOR.                                          ELTMONDC
01215      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01216      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMONDC
01217      SET PERIOD-NEEDED TO TRUE.                                   ELTMONDC
01218      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01219      MOVE WS-MM-INDICATOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).      ELTMONDC
01220      ADD 1 TO TCAR-FROM-SUB.                                      ELTMONDC
01221      MOVE 'MD-MM-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTMONDC
01222      MOVE GSS-MD-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMONDC
01223      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01224      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01225      EJECT                                                        ELTMONDC
01226                                                                   ELTMONDC
01227                                                                   ELTMONDC
01228 ************************************************************      ELTMONDC
01229 *                                                          *      ELTMONDC
01230 *        GENERATE MM BENEFITS REDUCTION SENTENCE           *      ELTMONDC
01231 *                                                          *      ELTMONDC
01232 ************************************************************      ELTMONDC
01233  GENERATE-MM-BENEFITS-REDUCTION.                                  ELTMONDC
01234      IF GSS-MD-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMONDC
01235          ZERO                                                     ELTMONDC
01236                  AND SPACES AND LOW-VALUES                        ELTMONDC
01237          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMONDC
01238      IF (GSS-MD-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTMONDC
01239          ZERO                                                     ELTMONDC
01240                             AND SPACES AND LOW-VALUES) OR         ELTMONDC
01241                 (GSS-MD-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT        ELTMONDC
01242          EQUAL ZERO                                               ELTMONDC
01243                             AND SPACES AND LOW-VALUES) OR         ELTMONDC
01244                 (GSS-MD-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTMONDC
01245          EQUAL ZERO                                               ELTMONDC
01246                             AND SPACES AND LOW-VALUES)            ELTMONDC
01247          PERFORM GENERATE-MM-BEN-REDUCT-TEXT.                     ELTMONDC
01248      EJECT                                                        ELTMONDC
01249                                                                   ELTMONDC
01250                                                                   ELTMONDC
01251 ************************************************************      ELTMONDC
01252 *                                                          *      ELTMONDC
01253 *        GENERATE MM BEN REDUCT TEXT                       *      ELTMONDC
01254 *                                                          *      ELTMONDC
01255 ************************************************************      ELTMONDC
01256  GENERATE-MM-BEN-REDUCT-TEXT.                                     ELTMONDC
01257      PERFORM GENERATE-BENEFITS-REDUCTION-HE.                      ELTMONDC
01258      IF GSS-MD-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMONDC
01259          ZERO                                                     ELTMONDC
01260                             AND SPACES AND LOW-VALUES             ELTMONDC
01261          PERFORM TRANSLATE-MM-DEDU-APPLIC.                        ELTMONDC
01262      IF GSS-MD-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMONDC
01263          ZERO                                                     ELTMONDC
01264                             AND SPACES AND LOW-VALUES             ELTMONDC
01265          PERFORM TRANSLATE-MM-OPEX-APPLIC.                        ELTMONDC
01266      PERFORM CREATE-A-BLANK-LINE.                                 ELTMONDC
01267      PERFORM LINK-TO-OUTPUT.                                      ELTMONDC
01268      IF GSS-MD-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMONDC
01269          ZERO                                                     ELTMONDC
01270                             AND SPACES AND LOW-VALUES             ELTMONDC
01271          PERFORM TRANSLATE-MM-OPEX-OVERRIDE.                      ELTMONDC
01272      EJECT                                                        ELTMONDC
01273                                                                   ELTMONDC
01274                                                                   ELTMONDC
01275 ************************************************************      ELTMONDC
01276 *                                                          *      ELTMONDC
01277 *        GENERATE GMDB TABULAR SENTENCE                    *      ELTMONDC
01278 *                                                          *      ELTMONDC
01279 ************************************************************      ELTMONDC
01280  GENERATE-GMDB-TABULAR-SENTENCE.                                  ELTMONDC
01281      SET GCG-INDEX TO 1.                                          ELTMONDC
01282      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMONDC
01283            AT END                                                 ELTMONDC
01284               MOVE ZEROES TO WS-GMDB-SRVS-SLOT-NO                 ELTMONDC
01285            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GMDB              ELTMONDC
01286               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTMONDC
01287          WS-GMDB-SRVS-ID                                          ELTMONDC
01288               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTMONDC
01289                   TO WS-GMDB-SRVS-SLOT-NO                         ELTMONDC
01290         END-SEARCH.                                               ELTMONDC
01291      IF WS-GMDB-SRVS-SLOT-NO NOT EQUAL ZEROES                     ELTMONDC
01292                  AND WS-GMDB-SRVS-ID EQUAL PC-GMDB                ELTMONDC
01293          PERFORM DISPLAY-RELATED-SERVICES-SENTE.                  ELTMONDC
01294      IF WS-GMDB-SRVS-SLOT-NO NOT EQUAL ZEROES                     ELTMONDC
01295                   AND WS-GMDB-SRVS-ID EQUAL PC-GMDB               ELTMONDC
01296          PERFORM GENERATE-RELATED-SERVICES.                       ELTMONDC
01297      EJECT                                                        ELTMONDC
01298                                                                   ELTMONDC
01299                                                                   ELTMONDC
01300 ************************************************************      ELTMONDC
01301 *                                                          *      ELTMONDC
01302 *        DISPLAY RELATED SERVICES SENTENCE                 *      ELTMONDC
01303 *                                                          *      ELTMONDC
01304 ************************************************************      ELTMONDC
01305  DISPLAY-RELATED-SERVICES-SENTE.                                  ELTMONDC
01306      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMONDC
01307      MOVE SPECIAL-SERVICES-MSG  TO COF-DTL-LINE                   ELTMONDC
01308          (COF-NBR-DTL-LINES).                                     ELTMONDC
01309      PERFORM CREATE-A-BLANK-LINE.                                 ELTMONDC
01310      PERFORM LINK-TO-OUTPUT.                                      ELTMONDC
01311      EJECT                                                        ELTMONDC
01312                                                                   ELTMONDC
01313                                                                   ELTMONDC
01314 ************************************************************      ELTMONDC
01315 *                                                          *      ELTMONDC
01316 *        TRANSLATE BC DEDU APPLIC                          *      ELTMONDC
01317 *                                                          *      ELTMONDC
01318 ************************************************************      ELTMONDC
01319  TRANSLATE-BC-DEDU-APPLIC.                                        ELTMONDC
01320      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01321      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01322      MOVE  'MD-BC-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTMONDC
01323      MOVE GSS-MD-BC-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMONDC
01324          CMF-CODE-VALUE.                                          ELTMONDC
01325      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01326      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01327      EJECT                                                        ELTMONDC
01328                                                                   ELTMONDC
01329                                                                   ELTMONDC
01330 ************************************************************      ELTMONDC
01331 *                                                          *      ELTMONDC
01332 *        TRANSLATE BC OPEX APPLIC                          *      ELTMONDC
01333 *                                                          *      ELTMONDC
01334 ************************************************************      ELTMONDC
01335  TRANSLATE-BC-OPEX-APPLIC.                                        ELTMONDC
01336      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01337      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01338      MOVE  'MD-BC-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTMONDC
01339      MOVE GSS-MD-BC-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMONDC
01340          CMF-CODE-VALUE.                                          ELTMONDC
01341      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01342      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01343      EJECT                                                        ELTMONDC
01344                                                                   ELTMONDC
01345                                                                   ELTMONDC
01346 ************************************************************      ELTMONDC
01347 *                                                          *      ELTMONDC
01348 *        TRANSLATE BC OPEX OVERRIDE                        *      ELTMONDC
01349 *                                                          *      ELTMONDC
01350 ************************************************************      ELTMONDC
01351  TRANSLATE-BC-OPEX-OVERRIDE.                                      ELTMONDC
01352      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01353      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMONDC
01354      SET PERIOD-NEEDED TO TRUE.                                   ELTMONDC
01355      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01356      MOVE 'MD-BC-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMONDC
01357      MOVE GSS-MD-BC-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMONDC
01358          CMF-CODE-VALUE.                                          ELTMONDC
01359      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01360      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01361      EJECT                                                        ELTMONDC
01362                                                                   ELTMONDC
01363                                                                   ELTMONDC
01364 ************************************************************      ELTMONDC
01365 *                                                          *      ELTMONDC
01366 *        TRANSLATE BS DEDU APPLIC                          *      ELTMONDC
01367 *                                                          *      ELTMONDC
01368 ************************************************************      ELTMONDC
01369  TRANSLATE-BS-DEDU-APPLIC.                                        ELTMONDC
01370      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01371      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01372      MOVE 'MD-BS-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMONDC
01373      MOVE GSS-MD-BS-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMONDC
01374          CMF-CODE-VALUE.                                          ELTMONDC
01375      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01376      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01377      EJECT                                                        ELTMONDC
01378                                                                   ELTMONDC
01379                                                                   ELTMONDC
01380 ************************************************************      ELTMONDC
01381 *                                                          *      ELTMONDC
01382 *        TRANSLATE BS OPEX APPLIC                          *      ELTMONDC
01383 *                                                          *      ELTMONDC
01384 ************************************************************      ELTMONDC
01385  TRANSLATE-BS-OPEX-APPLIC.                                        ELTMONDC
01386      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01387      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01388      MOVE 'MD-BS-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMONDC
01389      MOVE GSS-MD-BS-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMONDC
01390          CMF-CODE-VALUE.                                          ELTMONDC
01391      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01392      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01393      EJECT                                                        ELTMONDC
01394                                                                   ELTMONDC
01395                                                                   ELTMONDC
01396 ************************************************************      ELTMONDC
01397 *                                                          *      ELTMONDC
01398 *        TRANSLATE BS OPEX OVERRIDE                        *      ELTMONDC
01399 *                                                          *      ELTMONDC
01400 ************************************************************      ELTMONDC
01401  TRANSLATE-BS-OPEX-OVERRIDE.                                      ELTMONDC
01402      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01403      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMONDC
01404      SET PERIOD-NEEDED TO TRUE.                                   ELTMONDC
01405      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01406      MOVE 'MD-BS-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMONDC
01407      MOVE GSS-MD-BS-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMONDC
01408          CMF-CODE-VALUE.                                          ELTMONDC
01409      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01410      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01411      EJECT                                                        ELTMONDC
01412                                                                   ELTMONDC
01413                                                                   ELTMONDC
01414 ************************************************************      ELTMONDC
01415 *                                                          *      ELTMONDC
01416 *        TRANSLATE MM DEDU APPLIC                          *      ELTMONDC
01417 *                                                          *      ELTMONDC
01418 ************************************************************      ELTMONDC
01419  TRANSLATE-MM-DEDU-APPLIC.                                        ELTMONDC
01420      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01421      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01422      MOVE 'MD-MM-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMONDC
01423      MOVE GSS-MD-MM-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMONDC
01424          CMF-CODE-VALUE.                                          ELTMONDC
01425      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01426      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01427      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01428      EJECT                                                        ELTMONDC
01429                                                                   ELTMONDC
01430                                                                   ELTMONDC
01431 ************************************************************      ELTMONDC
01432 *                                                          *      ELTMONDC
01433 *        TRANSLATE MM OPEX APPLIC                          *      ELTMONDC
01434 *                                                          *      ELTMONDC
01435 ************************************************************      ELTMONDC
01436  TRANSLATE-MM-OPEX-APPLIC.                                        ELTMONDC
01437      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01438      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01439      MOVE 'MD-MM-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMONDC
01440      MOVE GSS-MD-MM-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMONDC
01441          CMF-CODE-VALUE.                                          ELTMONDC
01442      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01443      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01444      EJECT                                                        ELTMONDC
01445                                                                   ELTMONDC
01446                                                                   ELTMONDC
01447 ************************************************************      ELTMONDC
01448 *                                                          *      ELTMONDC
01449 *        TRANSLATE MM OPEX OVERRIDE                        *      ELTMONDC
01450 *                                                          *      ELTMONDC
01451 ************************************************************      ELTMONDC
01452  TRANSLATE-MM-OPEX-OVERRIDE.                                      ELTMONDC
01453      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01454      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMONDC
01455      SET PERIOD-NEEDED TO TRUE.                                   ELTMONDC
01456      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01457      MOVE 'MD-MM-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMONDC
01458      MOVE GSS-MD-MM-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMONDC
01459          CMF-CODE-VALUE.                                          ELTMONDC
01460      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01461      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01462      EJECT                                                        ELTMONDC
01463                                                                   ELTMONDC
01464                                                                   ELTMONDC
01465 ************************************************************      ELTMONDC
01466 *                                                          *      ELTMONDC
01467 *        GENERATE SPILL OVER INDICATOR                     *      ELTMONDC
01468 *                                                          *      ELTMONDC
01469 ************************************************************      ELTMONDC
01470  GENERATE-SPILL-OVER-INDICATOR.                                   ELTMONDC
01471      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMONDC
01472      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMONDC
01473      SET PERIOD-NEEDED TO TRUE.                                   ELTMONDC
01474      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01475      MOVE WS-SPILL-OVER TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTMONDC
01476      ADD 1 TO TCAR-FROM-SUB.                                      ELTMONDC
01477      MOVE 'MD-SPILL-OVER-IND'      TO CMF-ELEMENT-SYSTEM-NAME.    ELTMONDC
01478      MOVE GSS-MD-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMONDC
01479      PERFORM LINK-TO-TRANSLATOR.                                  ELTMONDC
01480      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMONDC
01481      EJECT                                                        ELTMONDC
01482                                                                   ELTMONDC
01483                                                                   ELTMONDC
01484 ************************************************************      ELTMONDC
01485 *                                                          *      ELTMONDC
01486 *        SIGNAL NOT APPLICABLE MSG                         *      ELTMONDC
01487 *                                                          *      ELTMONDC
01488 ************************************************************      ELTMONDC
01489  SIGNAL-NOT-APPLICABLE-MSG.                                       ELTMONDC
01490      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMONDC
01491      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTMONDC
01492          (COF-NBR-DTL-LINES).                                     ELTMONDC
01493      EJECT                                                        ELTMONDC
01494                                                                   ELTMONDC
01495                                                                   ELTMONDC
01496 ************************************************************      ELTMONDC
01497 *                                                          *      ELTMONDC
01498 *        SIGNAL VOLUNTARY MSG                              *      ELTMONDC
01499 *                                                          *      ELTMONDC
01500 ************************************************************      ELTMONDC
01501  SIGNAL-VOLUNTARY-MSG.                                            ELTMONDC
01502      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMONDC
01503      MOVE WS-VOLUNTARY-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTMONDC
01504                                                                   ELTMONDC
01505                                                                   ELTMONDC
01506 ************************************************************      ELTMONDC
01507 *                                                          *      ELTMONDC
01508 *        GROUP LINK TO TRANSLATOR                          *      ELTMONDC
01509 *                                                          *      ELTMONDC
01510 ************************************************************      ELTMONDC
01511  GROUP-LINK-TO-TRANSLATOR.                                        ELTMONDC
01512      MOVE PC-GROUP TO CMF-RECORD-PREFIX.                          ELTMONDC
01513      EXEC CICS LINK                                               ELTMONDC
01514           PROGRAM('ELUCMIF')                                      ELTMONDC
01515           COMMAREA(DFHCOMMAREA)                                   ELTMONDC
01516           END-EXEC.                                               ELTMONDC
01517                                                                   ELTMONDC
01518                                                                   ELTMONDC
01519 ************************************************************      ELTMONDC
01520 *                                                          *      ELTMONDC
01521 *        LINK TO TRANSLATOR                                *      ELTMONDC
01522 *                                                          *      ELTMONDC
01523 ************************************************************      ELTMONDC
01524  LINK-TO-TRANSLATOR.                                              ELTMONDC
01525      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMONDC
01526      EXEC CICS LINK                                               ELTMONDC
01527           PROGRAM('ELUCMIF')                                      ELTMONDC
01528           COMMAREA(DFHCOMMAREA)                                   ELTMONDC
01529           END-EXEC.                                               ELTMONDC
01530      EJECT                                                        ELTMONDC
01531                                                                   ELTMONDC
01532                                                                   ELTMONDC
01533 ************************************************************      ELTMONDC
01534 *                                                          *      ELTMONDC
01535 *        READ GCCP RECORD                                  *      ELTMONDC
01536 *                                                          *      ELTMONDC
01537 ************************************************************      ELTMONDC
01538  READ-GCCP-RECORD.                                                ELTMONDC
01539      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMONDC
01540      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMONDC
01541                            ADDRESS OF                             ELTMONDC
01542          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTMONDC
01543      SET IOP-RD TO TRUE.                                          ELTMONDC
01544      SET IOP-FCQ-NONE TO TRUE.                                    ELTMONDC
01545      SET IOP-KVQ-EQ TO TRUE.                                      ELTMONDC
01546      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTMONDC
01547      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTMONDC
01548      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTMONDC
01549      PERFORM LINK-TO-I-O-PGM.                                     ELTMONDC
01550      EJECT                                                        ELTMONDC
01551                                                                   ELTMONDC
01552                                                                   ELTMONDC
01553 ************************************************************      ELTMONDC
01554 *                                                          *      ELTMONDC
01555 *        LINK TO I O PGM                                   *      ELTMONDC
01556 *                                                          *      ELTMONDC
01557 ************************************************************      ELTMONDC
01558  LINK-TO-I-O-PGM.                                                 ELTMONDC
01559      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELTMONDC
01560           COMMAREA (DFHCOMMAREA)                                  ELTMONDC
01561           END-EXEC.                                               ELTMONDC
01562      IF IOP-RC-OK                                                 ELTMONDC
01563          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTMONDC
01564      ELSE IF IOP-RC-NOTFND                                        ELTMONDC
01565          PERFORM SIGNAL-NOT-FOUND-GCTAB                           ELTMONDC
01566      ELSE                                                         ELTMONDC
01567          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTMONDC
01568                                                                   ELTMONDC
01569                                                                   ELTMONDC
01570 ************************************************************      ELTMONDC
01571 *                                                          *      ELTMONDC
01572 *        SIGNAL CRITICAL IO ERROR                          *      ELTMONDC
01573 *                                                          *      ELTMONDC
01574 ************************************************************      ELTMONDC
01575  SIGNAL-CRITICAL-IO-ERROR.                                        ELTMONDC
01576      SET CIA-AB-CRITIO TO TRUE.                                   ELTMONDC
01577      PERFORM SIGNAL-ABEND.                                        ELTMONDC
01578                                                                   ELTMONDC
01579                                                                   ELTMONDC
01580 ************************************************************      ELTMONDC
01581 *                                                          *      ELTMONDC
01582 *        SIGNAL NOT FOUND GCTAB                            *      ELTMONDC
01583 *                                                          *      ELTMONDC
01584 ************************************************************      ELTMONDC
01585  SIGNAL-NOT-FOUND-GCTAB.                                          ELTMONDC
01586      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTMONDC
01587      PERFORM SIGNAL-ABEND.                                        ELTMONDC
01588                                                                   ELTMONDC
01589                                                                   ELTMONDC
01590 ************************************************************      ELTMONDC
01591 *                                                          *      ELTMONDC
01592 *        ESTABLISH ADDRESSABILITY OF GCCP RECORD           *      ELTMONDC
01593 *                                                          *      ELTMONDC
01594 ************************************************************      ELTMONDC
01595  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTMONDC
01596      SET ADDRESS OF GCCP-TABULAR-REC TO IOP-REC-PTR.              ELTMONDC
01597      SET IOP-REC-PTR TO NULL.                                     ELTMONDC
01598                                                                   ELTMONDC
01599                                                                   ELTMONDC
01600 ************************************************************      ELTMONDC
01601 *                                                          *      ELTMONDC
01602 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTMONDC
01603 *                                                          *      ELTMONDC
01604 ************************************************************      ELTMONDC
01605  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTMONDC
01606      PERFORM INITIALIZE-CMOUT.                                    ELTMONDC
01607      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTMONDC
01608      EJECT                                                        ELTMONDC
01609                                                                   ELTMONDC
01610                                                                   ELTMONDC
01611 ************************************************************      ELTMONDC
01612 *                                                          *      ELTMONDC
01613 *        PREPARE TEXT FOR OUTPUT                           *      ELTMONDC
01614 *                                                          *      ELTMONDC
01615 ************************************************************      ELTMONDC
01616  PREPARE-TEXT-FOR-OUTPUT.                                         ELTMONDC
01617      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTMONDC
01618          UNTIL CMF-DESCR-IDX                                      ELTMONDC
01619                                    GREATER THAN                   ELTMONDC
01620              CMF-NBR-DESCR-LINES.                                 ELTMONDC
01621      EJECT                                                        ELTMONDC
01622                                                                   ELTMONDC
01623                                                                   ELTMONDC
01624 ************************************************************      ELTMONDC
01625 *                                                          *      ELTMONDC
01626 *        INITIALIZE CMOUT                                  *      ELTMONDC
01627 *                                                          *      ELTMONDC
01628 ************************************************************      ELTMONDC
01629  INITIALIZE-CMOUT.                                                ELTMONDC
01630      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMONDC
01631      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMONDC
01632          ADDRESS OF CMF-DESCR.                                    ELTMONDC
01633      SET CMF-DESCR-IDX TO 1.                                      ELTMONDC
01634      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTMONDC
01635                                                                   ELTMONDC
01636                                                                   ELTMONDC
01637 ************************************************************      ELTMONDC
01638 *                                                          *      ELTMONDC
01639 *        MOVE CMF TEXT TO OUTPUT                           *      ELTMONDC
01640 *                                                          *      ELTMONDC
01641 ************************************************************      ELTMONDC
01642  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTMONDC
01643      PERFORM MOVE-A-LINE.                                         ELTMONDC
01644      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTMONDC
01645          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTMONDC
01646      IF TCAR-FROM-SUB GREATER THAN 20                             ELTMONDC
01647               OR CMF-DESCR-IDX GREATER THAN                       ELTMONDC
01648          CMF-NBR-DESCR-LINES                                      ELTMONDC
01649          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTMONDC
01650                                                                   ELTMONDC
01651                                                                   ELTMONDC
01652 ************************************************************      ELTMONDC
01653 *                                                          *      ELTMONDC
01654 *        FINISH CODES MANUAL TEXT                          *      ELTMONDC
01655 *                                                          *      ELTMONDC
01656 ************************************************************      ELTMONDC
01657  FINISH-CODES-MANUAL-TEXT.                                        ELTMONDC
01658      SET DONE-PROCESSING TO TRUE.                                 ELTMONDC
01659      IF PERIOD-NEEDED                                             ELTMONDC
01660          PERFORM GET-AND-MOVE-PERIOD.                             ELTMONDC
01661                                                                   ELTMONDC
01662                                                                   ELTMONDC
01663 ************************************************************      ELTMONDC
01664 *                                                          *      ELTMONDC
01665 *        GET AND MOVE PERIOD                               *      ELTMONDC
01666 *                                                          *      ELTMONDC
01667 ************************************************************      ELTMONDC
01668  GET-AND-MOVE-PERIOD.                                             ELTMONDC
01669      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTMONDC
01670          (TCAR-FROM-SUB).                                         ELTMONDC
01671                                                                   ELTMONDC
01672                                                                   ELTMONDC
01673 ************************************************************      ELTMONDC
01674 *                                                          *      ELTMONDC
01675 *        SAVE LAST LINE                                    *      ELTMONDC
01676 *                                                          *      ELTMONDC
01677 ************************************************************      ELTMONDC
01678  SAVE-LAST-LINE.                                                  ELTMONDC
01679      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01680      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMONDC
01681         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTMONDC
01682      ADD 1 TO TCAR-FROM-SUB.                                      ELTMONDC
01683      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTMONDC
01684                                                                   ELTMONDC
01685                                                                   ELTMONDC
01686 ************************************************************      ELTMONDC
01687 *                                                          *      ELTMONDC
01688 *        OUTPUT LAST LINE                                  *      ELTMONDC
01689 *                                                          *      ELTMONDC
01690 ************************************************************      ELTMONDC
01691  OUTPUT-LAST-LINE.                                                ELTMONDC
01692      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMONDC
01693          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTMONDC
01694      IF BLANK-LINE-NEEDED                                         ELTMONDC
01695          PERFORM CREATE-A-BLANK-LINE.                             ELTMONDC
01696                                                                   ELTMONDC
01697                                                                   ELTMONDC
01698 ************************************************************      ELTMONDC
01699 *                                                          *      ELTMONDC
01700 *        CREATE A BLANK LINE                               *      ELTMONDC
01701 *                                                          *      ELTMONDC
01702 ************************************************************      ELTMONDC
01703  CREATE-A-BLANK-LINE.                                             ELTMONDC
01704      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMONDC
01705      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMONDC
01706                                                                   ELTMONDC
01707                                                                   ELTMONDC
01708 ************************************************************      ELTMONDC
01709 *                                                          *      ELTMONDC
01710 *        MOVE A LINE                                       *      ELTMONDC
01711 *                                                          *      ELTMONDC
01712 ************************************************************      ELTMONDC
01713  MOVE-A-LINE.                                                     ELTMONDC
01714      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTMONDC
01715          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTMONDC
01716      SET CMF-DESCR-IDX UP BY 1.                                   ELTMONDC
01717      ADD 1 TO TCAR-FROM-SUB.                                      ELTMONDC
01718      EJECT                                                        ELTMONDC
01719                                                                   ELTMONDC
01720                                                                   ELTMONDC
01721 ************************************************************      ELTMONDC
01722 *                                                          *      ELTMONDC
01723 *        REFORMAT AND WRITE TEXT                           *      ELTMONDC
01724 *                                                          *      ELTMONDC
01725 ************************************************************      ELTMONDC
01726  REFORMAT-AND-WRITE-TEXT.                                         ELTMONDC
01727      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTMONDC
01728      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTMONDC
01729      PERFORM UNSTRING-TEXT.                                       ELTMONDC
01730      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMONDC
01731      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMONDC
01732      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTMONDC
01733          UNTIL COF-NBR-DTL-LINES GREATER                          ELTMONDC
01734                                   TCAR-OUTPUT-FIELDS-USED -       ELTMONDC
01735              1.                                                   ELTMONDC
01736      PERFORM DISPOSE-OF-LAST-LINE.                                ELTMONDC
01737      PERFORM LINK-TO-OUTPUT.                                      ELTMONDC
01738                                                                   ELTMONDC
01739                                                                   ELTMONDC
01740 ************************************************************      ELTMONDC
01741 *                                                          *      ELTMONDC
01742 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTMONDC
01743 *                                                          *      ELTMONDC
01744 ************************************************************      ELTMONDC
01745  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTMONDC
01746      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTMONDC
01747           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTMONDC
01748      ADD +1 TO TCAR-FROM-SUB.                                     ELTMONDC
01749      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMONDC
01750      EJECT                                                        ELTMONDC
01751                                                                   ELTMONDC
01752                                                                   ELTMONDC
01753 ************************************************************      ELTMONDC
01754 *                                                          *      ELTMONDC
01755 *        UNSTRING TEXT                                     *      ELTMONDC
01756 *                                                          *      ELTMONDC
01757 ************************************************************      ELTMONDC
01758  UNSTRING-TEXT.                                                   ELTMONDC
01759      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTMONDC
01760      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTMONDC
01761      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTMONDC
01762      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTMONDC
01763      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTMONDC
01764      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTMONDC
01765      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTMONDC
01766      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTMONDC
01767      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMONDC
01768      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMONDC
01769      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTMONDC
01770      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTMONDC
01771      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTMONDC
01772      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTMONDC
01773      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTMONDC
01774      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTMONDC
01775      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTMONDC
01776      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTMONDC
01777      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTMONDC
01778      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTMONDC
01779      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTMONDC
01780      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTMONDC
01781      EJECT                                                        ELTMONDC
01782                                                                   ELTMONDC
01783                                                                   ELTMONDC
01784 ************************************************************      ELTMONDC
01785 *                                                          *      ELTMONDC
01786 *        LINK TO OUTPUT                                    *      ELTMONDC
01787 *                                                          *      ELTMONDC
01788 ************************************************************      ELTMONDC
01789  LINK-TO-OUTPUT.                                                  ELTMONDC
01790      EXEC CICS LINK                                               ELTMONDC
01791          PROGRAM ('ELUOUTPT')                                     ELTMONDC
01792          COMMAREA (DFHCOMMAREA)                                   ELTMONDC
01793          END-EXEC.                                                ELTMONDC
01794      EJECT                                                        ELTMONDC
01795                                                                   ELTMONDC
01796                                                                   ELTMONDC
01797 ************************************************************      ELTMONDC
01798 *                                                          *      ELTMONDC
01799 *        DISPOSE OF LAST LINE                              *      ELTMONDC
01800 *                                                          *      ELTMONDC
01801 ************************************************************      ELTMONDC
01802  DISPOSE-OF-LAST-LINE.                                            ELTMONDC
01803      IF NOT ADDITIONAL-TEXT                                       ELTMONDC
01804          PERFORM INITIALIZE-CONTINUED-SW.                         ELTMONDC
01805      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTMONDC
01806          PERFORM SAVE-LAST-LINE                                   ELTMONDC
01807      ELSE                                                         ELTMONDC
01808          PERFORM OUTPUT-LAST-LINE.                                ELTMONDC
01809                                                                   ELTMONDC
01810                                                                   ELTMONDC
01811 ************************************************************      ELTMONDC
01812 *                                                          *      ELTMONDC
01813 *        INITIALIZE CONTINUED SW                           *      ELTMONDC
01814 *                                                          *      ELTMONDC
01815 ************************************************************      ELTMONDC
01816  INITIALIZE-CONTINUED-SW.                                         ELTMONDC
01817      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTMONDC
