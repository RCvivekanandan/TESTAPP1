00001 *      LAST MAINTENANCE TIME:  8.07.59  DATE: 06/13/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTHOSP 
00003                                                                      LV001
00004  PROGRAM-ID.         ELTHOSP.                                     ELTHOSP 
00005                                                                   ELTHOSP 
00006  AUTHOR.             ANNE KEFFER KING.                            ELTHOSP 
00007                                                                   ELTHOSP 
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTHOSP 
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTHOSP 
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTHOSP 
00011                      233 N. MICHIGAN AVE                          ELTHOSP 
00012                      CHICAGO, ILLINOIS 60601                      ELTHOSP 
00013                                                                   ELTHOSP 
00014  DATE-WRITTEN.       15-JUN-1987.                                 ELTHOSP 
00015                                                                   ELTHOSP 
00016  DATE-COMPILED.                                                   ELTHOSP 
00017                                                                   ELTHOSP 
00018  SECURITY.           COPYRIGHT 1986,                              ELTHOSP 
00019                      HEALTH CARE SERVICE CORPORATION              ELTHOSP 
00020 ******************************************************************ELTHOSP 
00021 * ENGLISH CONTRACT INQUIRY ON-LINE SYSTEM                        *ELTHOSP 
00022 * THIS PROGRAM WILL CREATE TRANSLATED SCREEN RECORDS FOR         *ELTHOSP 
00023 * DISPLAY OF TOPIC - COST CONTAINMENT - HOSPICE                  *ELTHOSP 
00024 * IN ENGLISH LANGUAGE FORMAT.                                    *ELTHOSP 
00025 *                                                                *ELTHOSP 
00026 * INPUT IS THE GCG GROUP SPECIFIC RECORD AND                     *ELTHOSP 
00027 *          THE HOSPICE (HO) TABULAR RECORD.                      *ELTHOSP 
00028 *          OF THE GCCP TABULAR.                                  *ELTHOSP 
00029 ******************************************************************ELTHOSP 
00030 *                                                                *ELTHOSP 
00031 *                       MAINTENANCE HISTORY                      *ELTHOSP 
00032 *                                                                *ELTHOSP 
00033 *  MOD     DATE     BY  DRPT             ACTION                  *ELTHOSP 
00034 * ----- ----------- --- ----- ---------------------------------- *ELTHOSP 
00035 * 01.00 15-JUN-1987 AKK       CREATED                            *ELTHOSP 
00036 *                                                                *ELTHOSP 
00037 * 01.01 10-OCT-1990 JPB       CHANGED STORAGE MANAGEMENT         *ELTHOSP 
00038 *                                                                *ELTHOSP 
00039 * 01.02 12-JUN-1991 GEM       ADD CCP PARTIC IND TO HOSP.        *ELTHOSP 
00040 ******************************************************************ELTHOSP 
00041      SKIP3                                                        ELTHOSP 
00042  ENVIRONMENT DIVISION.                                            ELTHOSP 
00043                                                                   ELTHOSP 
00044  CONFIGURATION SECTION.                                           ELTHOSP 
00045  SOURCE-COMPUTER.    IBM-3090.                                    ELTHOSP 
00046  OBJECT-COMPUTER.    IBM-3090.                                    ELTHOSP 
00047      EJECT                                                        ELTHOSP 
00048                                                                   ELTHOSP 
00049  DATA DIVISION.                                                   ELTHOSP 
00050 /                                                                 ELTHOSP 
00051  WORKING-STORAGE SECTION.                                         ELTHOSP 
00052  77  WORK-STOR                    PIC X(24) VALUE                 ELTHOSP 
00053      'ELTHOSP WORKING STORAGE*'.                                  ELTHOSP 
00054  01  WS-MISC.                                                     ELTHOSP 
00055      05  SCREEN-TYPE                  PIC X     VALUE SPACES.     ELTHOSP 
00056          88 INSTITUTIONAL-SCREEN                VALUE 'I'.        ELTHOSP 
00057          88 PROFESSIONAL-SCREEN                 VALUE 'P'.        ELTHOSP 
00058          88 SUPPLEMENTAL-SCREEN                 VALUE 'B'.        ELTHOSP 
00059 *                                                                 ELTHOSP 
00060  01  WS-POINTER2                      POINTER.                    ELTHOSP 
00061  01  WS-POINTER3                      POINTER.                    ELTHOSP 
00062 *                                                                 ELTHOSP 
00063  01  PROGRAM-CONSTANTS.                                           ELTHOSP 
00064      05  PC-GRP                     PIC X(06)  VALUE 'GROUP'.     ELTHOSP 
00065      05  PC-GCCP                    PIC X(06)  VALUE '#GCCP '.    ELTHOSP 
00066 *                                                                 ELTHOSP 
00067 **** WS-SWITCHES.                                                 ELTHOSP 
00068      05  APPROVAL-SOURCE-SWITCH       PIC X     VALUE SPACE.      ELTHOSP 
00069          88  NOT-HOLDING-APPROVAL-SRCE          VALUE 'N'.        ELTHOSP 
00070          88  HOLDING-APPROVAL-SOURCE            VALUE 'Y'.        ELTHOSP 
00071                                                                   ELTHOSP 
00072      05  UNDEFINED-TABULAR-SW         PIC X     VALUE 'N'.        ELTHOSP 
00073          88 TABULAR-IS-UNDEFINED                VALUE 'Y'.        ELTHOSP 
00074      05  DEFINED-TABULAR-SW           PIC X     VALUE 'N'.        ELTHOSP 
00075          88 TABULAR-IS-DEFINED                  VALUE 'Y'.        ELTHOSP 
00076 *                                                                 ELTHOSP 
00077      05  ADDITIONAL-TEXT-SW          PIC X      VALUE SPACES.     ELTHOSP 
00078          88 ADDITIONAL-TEXT                     VALUE 'Y'.        ELTHOSP 
00079          88 BLANK-LINE-NEEDED                   VALUE 'B'.        ELTHOSP 
00080 *                                                                 ELTHOSP 
00081      05  CONTINUED-PROCESSING-SW     PIC X      VALUE SPACES.     ELTHOSP 
00082          88 PROCESSING-CMF-TEXT                 VALUE 'P'.        ELTHOSP 
00083          88 DONE-PROCESSING                     VALUE 'D'.        ELTHOSP 
00084 *                                                                 ELTHOSP 
00085      05  WS-PERIOD-SW                PIC X      VALUE 'N'.        ELTHOSP 
00086          88 PERIOD-NEEDED                       VALUE 'Y'.        ELTHOSP 
00087 /                                                                 ELTHOSP 
00088  01  WS-HDR-LN2.                                                  ELTHOSP 
00089      05  FILLER                       PIC X(30) VALUE SPACES.     ELTHOSP 
00090      05  FILLER                       PIC X(16) VALUE             ELTHOSP 
00091          'HOSPICE PROGRAM '.                                      ELTHOSP 
00092      05  WS-HDR-TITLE                 PIC X(13) VALUE SPACES.     ELTHOSP 
00093      05  FILLER                       PIC X(20) VALUE SPACES.     ELTHOSP 
00094 **************************************************************    ELTHOSP 
00095 *** SCREEN BODY LINES                                             ELTHOSP 
00096 **************************************************************    ELTHOSP 
00097  01  WS-APPROVAL-SOURCE.                                          ELTHOSP 
00098         05  FILLER                   PIC X(45) VALUE              ELTHOSP 
00099            'HOSPICE PROGRAM SERVICES MUST BE APPROVED BY '.       ELTHOSP 
00100         05  FILLER                   PIC X(34) VALUE SPACES.      ELTHOSP 
00101 *                                                                 ELTHOSP 
00102  01  WS-BC-ALT-PRICING.                                           ELTHOSP 
00103      05  FILLER                      PIC X(51)  VALUE             ELTHOSP 
00104          'THE ALTERNATE PRICING FOR INSTITUTIONAL SERVICE IS '.   ELTHOSP 
00105      05  FILLER                      PIC X(28)  VALUE SPACES.     ELTHOSP 
00106 *                                                                 ELTHOSP 
00107  01  WS-BS-ALT-PRICING.                                           ELTHOSP 
00108      05  FILLER                      PIC X(50)  VALUE             ELTHOSP 
00109          'THE ALTERNATE PRICING FOR PROFESSIONAL SERVICE IS '.    ELTHOSP 
00110      05  FILLER                      PIC X(29)  VALUE SPACES.     ELTHOSP 
00111 *                                                                 ELTHOSP 
00112  01  WS-MM-ALT-PRICING.                                           ELTHOSP 
00113      05  FILLER                      PIC X(50)  VALUE             ELTHOSP 
00114          'THE ALTERNATE PRICING FOR SUPPLEMENTAL SERVICE IS '.    ELTHOSP 
00115      05  FILLER                      PIC X(29)  VALUE SPACES.     ELTHOSP 
00116 *                                                                 ELTHOSP 
00117  01  WS-INDICATOR.                                                ELTHOSP 
00118      05  FILLER                      PIC X(08) VALUE              ELTHOSP 
00119          'HOSPICE '.                                              ELTHOSP 
00120      05  FILLER                      PIC X(71) VALUE SPACES.      ELTHOSP 
00121 *                                                                 ELTHOSP 
00122  01  WS-BENEFITS-REDUCTION.                                       ELTHOSP 
00123      05  FILLER                      PIC X(50) VALUE              ELTHOSP 
00124             'DENIED OR REDUCED CHARGES DUE TO COST CONTAINMENT:'. ELTHOSP 
00125      05  FILLER                      PIC X(29) VALUE SPACES.      ELTHOSP 
00126 ******************************************************************ELTHOSP 
00127 **** SPECIAL MESSAGES ****                                        ELTHOSP 
00128 ******************************************************************ELTHOSP 
00129  01  WS-HOSPICE-APPLIES.                                          ELTHOSP 
00130      05  FILLER                      PIC  X(30) VALUE             ELTHOSP 
00131          'THE HOSPICE PROGRAM APPLIES TO'.                        ELTHOSP 
00132      05  FILLER                      PIC  X(49) VALUE SPACES.     ELTHOSP 
00133                                                                   ELTHOSP 
00134  01  WS-NOT-APPLICABLE-MSG.                                       ELTHOSP 
00135      05  FILLER                      PIC  X(38) VALUE             ELTHOSP 
00136          'THE HOSPICE PROGRAM IS NOT APPLICABLE.'.                ELTHOSP 
00137      05  FILLER                      PIC  X(41) VALUE SPACES.     ELTHOSP 
00138                                                                   ELTHOSP 
00139  01  WS-VOLUNTARY-MSG.                                            ELTHOSP 
00140      05  FILLER                      PIC  X(33) VALUE             ELTHOSP 
00141          'THE HOSPICE PROGRAM IS VOLUNTARY.'.                     ELTHOSP 
00142      05  FILLER                      PIC  X(46) VALUE SPACES.     ELTHOSP 
00143 *                                                                 ELTHOSP 
00144  01  WS-NOT-INSTITUTIONAL-MSG.                                    ELTHOSP 
00145      05  FILLER                      PIC X(61)  VALUE             ELTHOSP 
00146      'THE HOSPICE PROGRAM DOES NOT APPLY TO INSTITUTIONAL BENEFITSELTHOSP 
00147 -         '.'.                                                    ELTHOSP 
00148      05  FILLER                     PIC X(18)  VALUE SPACES.      ELTHOSP 
00149 *                                                                 ELTHOSP 
00150  01  WS-NOT-PROFESSIONAL-MSG.                                     ELTHOSP 
00151      05  FILLER                     PIC X(60)  VALUE              ELTHOSP 
00152       'THE HOSPICE PROGRAM DOES NOT APPLY TO PROFESSIONAL BENEFITSELTHOSP 
00153 -     '.'.                                                        ELTHOSP 
00154      05  FILLER                     PIC X(19)  VALUE SPACES.      ELTHOSP 
00155 *                                                                 ELTHOSP 
00156  01  WS-NOT-SUPPLEMENTAL-MSG.                                     ELTHOSP 
00157      05  FILLER                     PIC X(60)  VALUE              ELTHOSP 
00158       'THE HOSPICE PROGRAM DOES NOT APPLY TO SUPPLEMENTAL BENEFITSELTHOSP 
00159 -    '.'.                                                         ELTHOSP 
00160      05  FILLER                     PIC X(19)  VALUE SPACES.      ELTHOSP 
00161 *                                                                 ELTHOSP 
00162  01  WS-DISCLAIMER.                                               ELTHOSP 
00163      05  FILLER                      PIC X(79)  VALUE             ELTHOSP 
00164      '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.             ELTHOSP 
00165 *                                                                 ELTHOSP 
00166 *                                                                 ELTHOSP 
00167 /                                                                 ELTHOSP 
00168  LINKAGE SECTION.                                                 ELTHOSP 
00169  01  DFHCOMMAREA.                                                 ELTHOSP 
00170      COPY ELSCOMMC.                                               ELTHOSP 
00171 /                                                                 ELTHOSP 
00172      COPY ELSCIA2C.                                               ELTHOSP 
00173 /                                                                 ELTHOSP 
00174      COPY ELSCMDSC.                                               ELTHOSP 
00175 /                                                                 ELTHOSP 
00176      COPY ELSCMIFC.                                               ELTHOSP 
00177 /                                                                 ELTHOSP 
00178      COPY ELSIOPMC.                                               ELTHOSP 
00179 /                                                                 ELTHOSP 
00180      COPY ELSKEYSC.                                               ELTHOSP 
00181 /                                                                 ELTHOSP 
00182      COPY ELSOUTPC.                                               ELTHOSP 
00183 /                                                                 ELTHOSP 
00184      COPY ELSSRTPC.                                               ELTHOSP 
00185 /                                                                 ELTHOSP 
00186      COPY ELSTCWAC.                                               ELTHOSP 
00187 /                                                                 ELTHOSP 
00188      COPY ELSSSCBC.                                               ELTHOSP 
00189 /                                                                 ELTHOSP 
00190  01  GROUP-SPECIFIC-REC.                                          ELTHOSP 
00191  COPY GCGROUPC.                                                   ELTHOSP 
00192 /                                                                 ELTHOSP 
00193  01  GCCP-TABULAR-REC-AREA.                                       ELTHOSP 
00194  COPY GCTGCCPC.                                                   ELTHOSP 
00195      EJECT                                                        ELTHOSP 
00196  PROCEDURE DIVISION.                                              ELTHOSP 
00197 ************************************************************      ELTHOSP 
00198 *                                                          *      ELTHOSP 
00199 *                    PROCEDURE DIVISION                    *      ELTHOSP 
00200 *                                                          *      ELTHOSP 
00201 ************************************************************      ELTHOSP 
00202                                                                   ELTHOSP 
00203                                                                   ELTHOSP 
00204 ************************************************************      ELTHOSP 
00205 *                                                          *      ELTHOSP 
00206 *        PERFORM HOSPICE                                   *      ELTHOSP 
00207 *                                                          *      ELTHOSP 
00208 ************************************************************      ELTHOSP 
00209  PERFORM-HOSPICE.                                                 ELTHOSP 
00210      PERFORM INITIALIZATION.                                      ELTHOSP 
00211      PERFORM PROCESS-HOSPICE-INQUIRY.                             ELTHOSP 
00212      GOBACK.                                                      ELTHOSP 
00213                                                                   ELTHOSP 
00214                                                                   ELTHOSP 
00215 ************************************************************      ELTHOSP 
00216 *                                                          *      ELTHOSP 
00217 *        INITIALIZATION                                    *      ELTHOSP 
00218 *                                                          *      ELTHOSP 
00219 ************************************************************      ELTHOSP 
00220  INITIALIZATION.                                                  ELTHOSP 
00221      SET NOT-HOLDING-APPROVAL-SRCE TO TRUE.                       ELTHOSP 
00222      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTHOSP 
00223      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTHOSP 
00224                                                                   ELTHOSP 
00225                                                                   ELTHOSP 
00226 ************************************************************      ELTHOSP 
00227 *                                                          *      ELTHOSP 
00228 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTHOSP 
00229 *                                                          *      ELTHOSP 
00230 ************************************************************      ELTHOSP 
00231  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTHOSP 
00232      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTHOSP 
00233      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTHOSP 
00234      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTHOSP 
00235                                                                   ELTHOSP 
00236                                                                   ELTHOSP 
00237 ************************************************************      ELTHOSP 
00238 *                                                          *      ELTHOSP 
00239 *        CHECK FOR VALID COMMAREA                          *      ELTHOSP 
00240 *                                                          *      ELTHOSP 
00241 ************************************************************      ELTHOSP 
00242  CHECK-FOR-VALID-COMMAREA.                                        ELTHOSP 
00243      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTHOSP 
00244          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTHOSP 
00245                                                                   ELTHOSP 
00246                                                                   ELTHOSP 
00247 ************************************************************      ELTHOSP 
00248 *                                                          *      ELTHOSP 
00249 *        SIGNAL INVALID COMMAREA                           *      ELTHOSP 
00250 *                                                          *      ELTHOSP 
00251 ************************************************************      ELTHOSP 
00252  SIGNAL-INVALID-COMMAREA.                                         ELTHOSP 
00253      EXEC CICS ABEND                                              ELTHOSP 
00254                ABCODE('EL01')                                     ELTHOSP 
00255         END-EXEC.                                                 ELTHOSP 
00256      EJECT                                                        ELTHOSP 
00257                                                                   ELTHOSP 
00258                                                                   ELTHOSP 
00259 ************************************************************      ELTHOSP 
00260 *                                                          *      ELTHOSP 
00261 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTHOSP 
00262 *                                                          *      ELTHOSP 
00263 ************************************************************      ELTHOSP 
00264  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTHOSP 
00265      IF ECA-CIA-PTR = NULL                                        ELTHOSP 
00266          PERFORM SIGNAL-INVALID-CIA                               ELTHOSP 
00267      ELSE                                                         ELTHOSP 
00268          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTHOSP 
00269                                                                   ELTHOSP 
00270                                                                   ELTHOSP 
00271 ************************************************************      ELTHOSP 
00272 *                                                          *      ELTHOSP 
00273 *        SIGNAL INVALID CIA                                *      ELTHOSP 
00274 *                                                          *      ELTHOSP 
00275 ************************************************************      ELTHOSP 
00276  SIGNAL-INVALID-CIA.                                              ELTHOSP 
00277      EXEC CICS ABEND                                              ELTHOSP 
00278                ABCODE('EL02')                                     ELTHOSP 
00279         END-EXEC.                                                 ELTHOSP 
00280      EJECT                                                        ELTHOSP 
00281                                                                   ELTHOSP 
00282                                                                   ELTHOSP 
00283 ************************************************************      ELTHOSP 
00284 *                                                          *      ELTHOSP 
00285 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTHOSP 
00286 *                                                          *      ELTHOSP 
00287 ************************************************************      ELTHOSP 
00288  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTHOSP 
00289      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTHOSP 
00290      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHOSP 
00291                            ADDRESS OF                             ELTHOSP 
00292          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTHOSP 
00293      IF CIA-RC-PTR-NULL                                           ELTHOSP 
00294          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHOSP 
00295                                                                   ELTHOSP 
00296                                                                   ELTHOSP 
00297 ************************************************************      ELTHOSP 
00298 *                                                          *      ELTHOSP 
00299 *        SIGNAL UNALLOC AREA ERROR                         *      ELTHOSP 
00300 *                                                          *      ELTHOSP 
00301 ************************************************************      ELTHOSP 
00302  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTHOSP 
00303      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTHOSP 
00304      PERFORM SIGNAL-ABEND.                                        ELTHOSP 
00305                                                                   ELTHOSP 
00306                                                                   ELTHOSP 
00307 ************************************************************      ELTHOSP 
00308 *                                                          *      ELTHOSP 
00309 *        SIGNAL ABEND                                      *      ELTHOSP 
00310 *                                                          *      ELTHOSP 
00311 ************************************************************      ELTHOSP 
00312  SIGNAL-ABEND.                                                    ELTHOSP 
00313      EXEC CICS ABEND                                              ELTHOSP 
00314                ABCODE(CIA-ABCODE)                                 ELTHOSP 
00315         END-EXEC.                                                 ELTHOSP 
00316      EJECT                                                        ELTHOSP 
00317                                                                   ELTHOSP 
00318                                                                   ELTHOSP 
00319 ************************************************************      ELTHOSP 
00320 *                                                          *      ELTHOSP 
00321 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTHOSP 
00322 *                                                          *      ELTHOSP 
00323 ************************************************************      ELTHOSP 
00324  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTHOSP 
00325      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTHOSP 
00326      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTHOSP 
00327      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTHOSP 
00328      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTHOSP 
00329      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTHOSP 
00330      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTHOSP 
00331      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTHOSP 
00332                                                                   ELTHOSP 
00333                                                                   ELTHOSP 
00334 ************************************************************      ELTHOSP 
00335 *                                                          *      ELTHOSP 
00336 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTHOSP 
00337 *                                                          *      ELTHOSP 
00338 ************************************************************      ELTHOSP 
00339  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTHOSP 
00340      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTHOSP 
00341      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHOSP 
00342                            ADDRESS OF                             ELTHOSP 
00343          CMF-CODES-MANUAL-INTERFACE.                              ELTHOSP 
00344      IF CIA-RC-PTR-NULL                                           ELTHOSP 
00345          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHOSP 
00346      EJECT                                                        ELTHOSP 
00347                                                                   ELTHOSP 
00348                                                                   ELTHOSP 
00349 ************************************************************      ELTHOSP 
00350 *                                                          *      ELTHOSP 
00351 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTHOSP 
00352 *                                                          *      ELTHOSP 
00353 ************************************************************      ELTHOSP 
00354  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTHOSP 
00355      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTHOSP 
00356      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHOSP 
00357                            ADDRESS OF                             ELTHOSP 
00358          COF-OUTPUT-INTERFACE.                                    ELTHOSP 
00359      IF CIA-RC-PTR-NULL                                           ELTHOSP 
00360          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHOSP 
00361      EJECT                                                        ELTHOSP 
00362                                                                   ELTHOSP 
00363                                                                   ELTHOSP 
00364 ************************************************************      ELTHOSP 
00365 *                                                          *      ELTHOSP 
00366 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTHOSP 
00367 *                                                          *      ELTHOSP 
00368 ************************************************************      ELTHOSP 
00369  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTHOSP 
00370      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTHOSP 
00371      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHOSP 
00372                            ADDRESS OF                             ELTHOSP 
00373          SRP-SUBROUTINE-PARAMETERS.                               ELTHOSP 
00374      IF CIA-RC-PTR-NULL                                           ELTHOSP 
00375          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHOSP 
00376      EJECT                                                        ELTHOSP 
00377                                                                   ELTHOSP 
00378                                                                   ELTHOSP 
00379 ************************************************************      ELTHOSP 
00380 *                                                          *      ELTHOSP 
00381 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTHOSP 
00382 *                                                          *      ELTHOSP 
00383 ************************************************************      ELTHOSP 
00384  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTHOSP 
00385      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTHOSP 
00386      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHOSP 
00387                            ADDRESS OF                             ELTHOSP 
00388          TCAR-COMPRESSION-WORK-AREA.                              ELTHOSP 
00389      IF CIA-RC-PTR-NULL                                           ELTHOSP 
00390          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHOSP 
00391      EJECT                                                        ELTHOSP 
00392                                                                   ELTHOSP 
00393                                                                   ELTHOSP 
00394 ************************************************************      ELTHOSP 
00395 *                                                          *      ELTHOSP 
00396 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTHOSP 
00397 *                                                          *      ELTHOSP 
00398 ************************************************************      ELTHOSP 
00399  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTHOSP 
00400      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTHOSP 
00401      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHOSP 
00402                             ADDRESS OF                            ELTHOSP 
00403          KWA-FILE-KEY-WORK-AREA.                                  ELTHOSP 
00404      IF CIA-RC-PTR-NULL                                           ELTHOSP 
00405          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHOSP 
00406      EJECT                                                        ELTHOSP 
00407                                                                   ELTHOSP 
00408                                                                   ELTHOSP 
00409 ************************************************************      ELTHOSP 
00410 *                                                          *      ELTHOSP 
00411 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD *      ELTHOSP 
00412 *                                                          *      ELTHOSP 
00413 ************************************************************      ELTHOSP 
00414  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTHOSP 
00415      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTHOSP 
00416      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHOSP 
00417                            ADDRESS OF                             ELTHOSP 
00418          GROUP-SPECIFIC-REC.                                      ELTHOSP 
00419      IF CIA-RC-PTR-NULL                                           ELTHOSP 
00420          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHOSP 
00421      EJECT                                                        ELTHOSP 
00422                                                                   ELTHOSP 
00423                                                                   ELTHOSP 
00424 ************************************************************      ELTHOSP 
00425 *                                                          *      ELTHOSP 
00426 *        ESTABLISH ADDRESSABILITY OF COST CONTAINMENT      *      ELTHOSP 
00427 *                                                          *      ELTHOSP 
00428 ************************************************************      ELTHOSP 
00429  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTHOSP 
00430      SET CIA-GCTABULR-DDN TO TRUE.                                ELTHOSP 
00431      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHOSP 
00432                            ADDRESS OF                             ELTHOSP 
00433          GCCP-TABULAR-REC-AREA.                                   ELTHOSP 
00434      IF CIA-RC-PTR-NULL                                           ELTHOSP 
00435          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTHOSP 
00436                                                                   ELTHOSP 
00437                                                                   ELTHOSP 
00438 ************************************************************      ELTHOSP 
00439 *                                                          *      ELTHOSP 
00440 *        ESTABLISH ADDRESS OF CIA                          *      ELTHOSP 
00441 *                                                          *      ELTHOSP 
00442 ************************************************************      ELTHOSP 
00443  ESTABLISH-ADDRESS-OF-CIA.                                        ELTHOSP 
00444      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTHOSP 
00445                            ADDRESS OF                             ELTHOSP 
00446          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTHOSP 
00447      EJECT                                                        ELTHOSP 
00448                                                                   ELTHOSP 
00449                                                                   ELTHOSP 
00450 ************************************************************      ELTHOSP 
00451 *                                                          *      ELTHOSP 
00452 *        PROCESS HOSPICE INQUIRY                           *      ELTHOSP 
00453 *                                                          *      ELTHOSP 
00454 ************************************************************      ELTHOSP 
00455  PROCESS-HOSPICE-INQUIRY.                                         ELTHOSP 
00456      IF GCG-HOSPICE-IND EQUAL ZERO OR                             ELTHOSP 
00457               GCG-HOSPICE-IND EQUAL '08'                          ELTHOSP 
00458          PERFORM TEST-APPLICABILITY                               ELTHOSP 
00459      ELSE                                                         ELTHOSP 
00460          PERFORM GENERATE-HOSPICE-TEXT.                           ELTHOSP 
00461      MOVE 'E' TO COF-FUNCTION.                                    ELTHOSP 
00462      MOVE ZEROS TO COF-NBR-HDR-LINES                              ELTHOSP 
00463         COF-NBR-DTL-LINES.                                        ELTHOSP 
00464      PERFORM LINK-TO-OUTPUT.                                      ELTHOSP 
00465                                                                   ELTHOSP 
00466                                                                   ELTHOSP 
00467 ************************************************************      ELTHOSP 
00468 *                                                          *      ELTHOSP 
00469 *        GENERATE HOSPICE TEXT                             *      ELTHOSP 
00470 *                                                          *      ELTHOSP 
00471 ************************************************************      ELTHOSP 
00472  GENERATE-HOSPICE-TEXT.                                           ELTHOSP 
00473      PERFORM VERIFY-HOSPICE-IN-GCCP-RECORD.                       ELTHOSP 
00474      PERFORM BUILD-HOSPICE-TEXT.                                  ELTHOSP 
00475      EJECT                                                        ELTHOSP 
00476                                                                   ELTHOSP 
00477                                                                   ELTHOSP 
00478 ************************************************************      ELTHOSP 
00479 *                                                          *      ELTHOSP 
00480 *        BUILD HOSPICE TEXT                                *      ELTHOSP 
00481 *                                                          *      ELTHOSP 
00482 ************************************************************      ELTHOSP 
00483  BUILD-HOSPICE-TEXT.                                              ELTHOSP 
00484      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTHOSP 
00485          PERFORM GENERATE-INSTITUTIONAL.                          ELTHOSP 
00486      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTHOSP 
00487          PERFORM GENERATE-PROFESSIONAL.                           ELTHOSP 
00488      IF GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                        ELTHOSP 
00489             '03' OR '04' OR '06' OR '08'                          ELTHOSP 
00490          PERFORM GENERATE-SUPPLEMENTAL.                           ELTHOSP 
00491      EJECT                                                        ELTHOSP 
00492                                                                   ELTHOSP 
00493                                                                   ELTHOSP 
00494 ************************************************************      ELTHOSP 
00495 *                                                          *      ELTHOSP 
00496 *        TEST APPLICABILITY                                *      ELTHOSP 
00497 *                                                          *      ELTHOSP 
00498 ************************************************************      ELTHOSP 
00499  TEST-APPLICABILITY.                                              ELTHOSP 
00500      PERFORM GENERATE-HEADINGS.                                   ELTHOSP 
00501      IF GCG-HOSPICE-IND EQUAL ZERO                                ELTHOSP 
00502          PERFORM SIGNAL-NOT-APPLICABLE-MSG                        ELTHOSP 
00503      ELSE IF GCG-HOSPICE-IND EQUAL '08'                           ELTHOSP 
00504          PERFORM SIGNAL-VOLUNTARY-MSG.                            ELTHOSP 
00505      PERFORM LINK-TO-OUTPUT.                                      ELTHOSP 
00506                                                                   ELTHOSP 
00507                                                                   ELTHOSP 
00508 ************************************************************      ELTHOSP 
00509 *                                                          *      ELTHOSP 
00510 *        SIGNAL NOT APPLICABLE MSG                         *      ELTHOSP 
00511 *                                                          *      ELTHOSP 
00512 ************************************************************      ELTHOSP 
00513  SIGNAL-NOT-APPLICABLE-MSG.                                       ELTHOSP 
00514      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTHOSP 
00515      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTHOSP 
00516          (COF-NBR-DTL-LINES).                                     ELTHOSP 
00517                                                                   ELTHOSP 
00518                                                                   ELTHOSP 
00519 ************************************************************      ELTHOSP 
00520 *                                                          *      ELTHOSP 
00521 *        SIGNAL VOLUNTARY MSG                              *      ELTHOSP 
00522 *                                                          *      ELTHOSP 
00523 ************************************************************      ELTHOSP 
00524  SIGNAL-VOLUNTARY-MSG.                                            ELTHOSP 
00525      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTHOSP 
00526      MOVE WS-VOLUNTARY-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTHOSP 
00527      EJECT                                                        ELTHOSP 
00528                                                                   ELTHOSP 
00529                                                                   ELTHOSP 
00530 ************************************************************      ELTHOSP 
00531 *                                                          *      ELTHOSP 
00532 *        VERIFY HOSPICE IN GCCP RECORD                     *      ELTHOSP 
00533 *                                                          *      ELTHOSP 
00534 ************************************************************      ELTHOSP 
00535  VERIFY-HOSPICE-IN-GCCP-RECORD.                                   ELTHOSP 
00536      SET WS-POINTER2 TO NULLS.                                    ELTHOSP 
00537      SET WS-POINTER3 TO NULLS.                                    ELTHOSP 
00538      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTHOSP 
00539      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTHOSP 
00540                            WS-POINTER2.                           ELTHOSP 
00541      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTHOSP 
00542      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTHOSP 
00543                            WS-POINTER3.                           ELTHOSP 
00544      PERFORM ACQUIRE-GCCP-RECORD.                                 ELTHOSP 
00545      PERFORM GET-HOSPICE-WITHIN-GCCP-RECORD.                      ELTHOSP 
00546      EJECT                                                        ELTHOSP 
00547                                                                   ELTHOSP 
00548                                                                   ELTHOSP 
00549 ************************************************************      ELTHOSP 
00550 *                                                          *      ELTHOSP 
00551 *        ACQUIRE GCCP RECORD                               *      ELTHOSP 
00552 *                                                          *      ELTHOSP 
00553 ************************************************************      ELTHOSP 
00554  ACQUIRE-GCCP-RECORD.                                             ELTHOSP 
00555      MOVE SPACES TO KWA-PROVISION-ID.                             ELTHOSP 
00556      SET GCG-INDEX TO 1.                                          ELTHOSP 
00557      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTHOSP 
00558           AT END                                                  ELTHOSP 
00559             MOVE ZERO TO KWA-PROVISION-SLOT-NO                    ELTHOSP 
00560           WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GCCP               ELTHOSP 
00561              MOVE GCG-TAB-ID (GCG-INDEX) TO                       ELTHOSP 
00562          KWA-PROVISION-ID                                         ELTHOSP 
00563              MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                  ELTHOSP 
00564          KWA-PROVISION-SLOT-NO                                    ELTHOSP 
00565           END-SEARCH.                                             ELTHOSP 
00566      IF KWA-PROVISION-SLOT-NO EQUAL ZERO                          ELTHOSP 
00567          PERFORM SIGNAL-UNDEFINED-TABULAR                         ELTHOSP 
00568      ELSE                                                         ELTHOSP 
00569          PERFORM READ-GCCP-RECORD.                                ELTHOSP 
00570                                                                   ELTHOSP 
00571                                                                   ELTHOSP 
00572 ************************************************************      ELTHOSP 
00573 *                                                          *      ELTHOSP 
00574 *        SIGNAL UNDEFINED TABULAR                          *      ELTHOSP 
00575 *                                                          *      ELTHOSP 
00576 ************************************************************      ELTHOSP 
00577  SIGNAL-UNDEFINED-TABULAR.                                        ELTHOSP 
00578      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTHOSP 
00579      PERFORM SIGNAL-ABEND.                                        ELTHOSP 
00580      EJECT                                                        ELTHOSP 
00581                                                                   ELTHOSP 
00582                                                                   ELTHOSP 
00583 ************************************************************      ELTHOSP 
00584 *                                                          *      ELTHOSP 
00585 *        GET HOSPICE WITHIN GCCP RECORD                    *      ELTHOSP 
00586 *                                                          *      ELTHOSP 
00587 ************************************************************      ELTHOSP 
00588  GET-HOSPICE-WITHIN-GCCP-RECORD.                                  ELTHOSP 
00589      SET GSS-INDEX TO 1.                                          ELTHOSP 
00590      SEARCH GSS-ENTRY                                             ELTHOSP 
00591          AT END                                                   ELTHOSP 
00592            SET TABULAR-IS-UNDEFINED TO TRUE                       ELTHOSP 
00593         WHEN GSS-HO-PROG-CODE-CHR (GSS-INDEX)                     ELTHOSP 
00594              SET TABULAR-IS-DEFINED TO TRUE                       ELTHOSP 
00595         END-SEARCH.                                               ELTHOSP 
00596      IF TABULAR-IS-UNDEFINED                                      ELTHOSP 
00597          PERFORM SIGNAL-UNDEFINED-TABULAR.                        ELTHOSP 
00598                                                                   ELTHOSP 
00599                                                                   ELTHOSP 
00600 ************************************************************      ELTHOSP 
00601 *                                                          *      ELTHOSP 
00602 *        GENERATE INSTITUTIONAL                            *      ELTHOSP 
00603 *                                                          *      ELTHOSP 
00604 ************************************************************      ELTHOSP 
00605  GENERATE-INSTITUTIONAL.                                          ELTHOSP 
00606      SET INSTITUTIONAL-SCREEN TO TRUE.                            ELTHOSP 
00607      PERFORM GENERATE-HEADINGS.                                   ELTHOSP 
00608      PERFORM BUILD-INSTITUTIONAL-TEXT.                            ELTHOSP 
00609      EJECT                                                        ELTHOSP 
00610                                                                   ELTHOSP 
00611                                                                   ELTHOSP 
00612 ************************************************************      ELTHOSP 
00613 *                                                          *      ELTHOSP 
00614 *        GENERATE PROFESSIONAL                             *      ELTHOSP 
00615 *                                                          *      ELTHOSP 
00616 ************************************************************      ELTHOSP 
00617  GENERATE-PROFESSIONAL.                                           ELTHOSP 
00618      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTHOSP 
00619      PERFORM GENERATE-HEADINGS.                                   ELTHOSP 
00620      PERFORM BUILD-PROFESSIONAL-TEXT.                             ELTHOSP 
00621      EJECT                                                        ELTHOSP 
00622                                                                   ELTHOSP 
00623                                                                   ELTHOSP 
00624 ************************************************************      ELTHOSP 
00625 *                                                          *      ELTHOSP 
00626 *        GENERATE SUPPLEMENTAL                             *      ELTHOSP 
00627 *                                                          *      ELTHOSP 
00628 ************************************************************      ELTHOSP 
00629  GENERATE-SUPPLEMENTAL.                                           ELTHOSP 
00630      SET SUPPLEMENTAL-SCREEN TO TRUE.                             ELTHOSP 
00631      PERFORM GENERATE-HEADINGS.                                   ELTHOSP 
00632      PERFORM BUILD-SUPPLEMENTAL-TEXT.                             ELTHOSP 
00633                                                                   ELTHOSP 
00634                                                                   ELTHOSP 
00635 ************************************************************      ELTHOSP 
00636 *                                                          *      ELTHOSP 
00637 *        BUILD INSTITUTIONAL TEXT                          *      ELTHOSP 
00638 *                                                          *      ELTHOSP 
00639 ************************************************************      ELTHOSP 
00640  BUILD-INSTITUTIONAL-TEXT.                                        ELTHOSP 
00641      IF GSS-HO-BC-IND (GSS-INDEX) EQUAL ZEROES                    ELTHOSP 
00642                OR SPACES OR LOW-VALUES                            ELTHOSP 
00643          PERFORM SIGNAL-NOT-APPLICABLE-FOR-BC-L                   ELTHOSP 
00644      ELSE                                                         ELTHOSP 
00645          PERFORM CREATE-INSTITUTIONAL-TEXT.                       ELTHOSP 
00646                                                                   ELTHOSP 
00647                                                                   ELTHOSP 
00648 ************************************************************      ELTHOSP 
00649 *                                                          *      ELTHOSP 
00650 *        BUILD PROFESSIONAL TEXT                           *      ELTHOSP 
00651 *                                                          *      ELTHOSP 
00652 ************************************************************      ELTHOSP 
00653  BUILD-PROFESSIONAL-TEXT.                                         ELTHOSP 
00654      IF GSS-HO-BS-IND (GSS-INDEX)  EQUAL ZEROES                   ELTHOSP 
00655                OR SPACES OR LOW-VALUES                            ELTHOSP 
00656          PERFORM SIGNAL-NOT-APPLICABLE-FOR-BS-L                   ELTHOSP 
00657      ELSE                                                         ELTHOSP 
00658          PERFORM CREATE-PROFESSIONAL-TEXT.                        ELTHOSP 
00659                                                                   ELTHOSP 
00660                                                                   ELTHOSP 
00661 ************************************************************      ELTHOSP 
00662 *                                                          *      ELTHOSP 
00663 *        BUILD SUPPLEMENTAL TEXT                           *      ELTHOSP 
00664 *                                                          *      ELTHOSP 
00665 ************************************************************      ELTHOSP 
00666  BUILD-SUPPLEMENTAL-TEXT.                                         ELTHOSP 
00667      IF GSS-HO-MM-IND (GSS-INDEX)  EQUAL ZEROES                   ELTHOSP 
00668                OR SPACES OR LOW-VALUES                            ELTHOSP 
00669          PERFORM SIGNAL-NOT-APPLICABLE-FOR-MM-L                   ELTHOSP 
00670      ELSE                                                         ELTHOSP 
00671          PERFORM CREATE-SUPPLEMENTAL-TEXT.                        ELTHOSP 
00672      EJECT                                                        ELTHOSP 
00673                                                                   ELTHOSP 
00674                                                                   ELTHOSP 
00675 ************************************************************      ELTHOSP 
00676 *                                                          *      ELTHOSP 
00677 *        CREATE INSTITUTIONAL TEXT                         *      ELTHOSP 
00678 *                                                          *      ELTHOSP 
00679 ************************************************************      ELTHOSP 
00680  CREATE-INSTITUTIONAL-TEXT.                                       ELTHOSP 
00681      PERFORM TRANSLATE-DISPLAY-HOSPICE-IND.                       ELTHOSP 
00682      IF GSS-HO-APPROVAL-SOURCE-IND (GSS-INDEX) NOT  EQUAL         ELTHOSP 
00683          ZEROES                                                   ELTHOSP 
00684              AND SPACES AND LOW-VALUES                            ELTHOSP 
00685          PERFORM TRANSLATE-APPROVAL-SOURCE-IND.                   ELTHOSP 
00686      PERFORM TRANSLATE-BC-INDICATOR.                              ELTHOSP 
00687      IF GSS-HO-BC-IP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTHOSP 
00688          ZEROES AND                                               ELTHOSP 
00689             SPACES AND LOW-VALUES                                 ELTHOSP 
00690          PERFORM TRANSLATE-IP-BC-ALT-PRICING-ME.                  ELTHOSP 
00691      IF GSS-HO-BC-OP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTHOSP 
00692          ZEROES AND                                               ELTHOSP 
00693             SPACES AND LOW-VALUES                                 ELTHOSP 
00694          PERFORM TRANSLATE-OP-BC-ALT-PRICING-ME.                  ELTHOSP 
00695      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTHOSP 
00696      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTHOSP 
00697      PERFORM GENERATE-CALC-METHOD-SENTENCE.                       ELTHOSP 
00698      PERFORM GENERATE-BC-BENEFITS-REDUCTION.                      ELTHOSP 
00699      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTHOSP 
00700      MOVE SPACES TO SCREEN-TYPE.                                  ELTHOSP 
00701      EJECT                                                        ELTHOSP 
00702                                                                   ELTHOSP 
00703                                                                   ELTHOSP 
00704 ************************************************************      ELTHOSP 
00705 *                                                          *      ELTHOSP 
00706 *        CREATE PROFESSIONAL TEXT                          *      ELTHOSP 
00707 *                                                          *      ELTHOSP 
00708 ************************************************************      ELTHOSP 
00709  CREATE-PROFESSIONAL-TEXT.                                        ELTHOSP 
00710      PERFORM TRANSLATE-DISPLAY-HOSPICE-IND.                       ELTHOSP 
00711      IF GSS-HO-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTHOSP 
00712          ZEROES                                                   ELTHOSP 
00713              AND SPACES AND LOW-VALUES                            ELTHOSP 
00714          PERFORM TRANSLATE-APPROVAL-SOURCE-IND.                   ELTHOSP 
00715      PERFORM TRANSLATE-BS-INDICATOR.                              ELTHOSP 
00716      IF GSS-HO-BS-IP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTHOSP 
00717          ZEROES                                                   ELTHOSP 
00718               AND SPACES AND LOW-VALUES                           ELTHOSP 
00719          PERFORM TRANSLATE-IP-BS-ALT-PRICING-ME.                  ELTHOSP 
00720      IF GSS-HO-BS-OP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTHOSP 
00721          ZEROES AND                                               ELTHOSP 
00722              SPACES AND LOW-VALUES                                ELTHOSP 
00723          PERFORM TRANSLATE-OP-BS-ALT-PRICING-ME.                  ELTHOSP 
00724      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTHOSP 
00725      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTHOSP 
00726      PERFORM GENERATE-CALC-METHOD-SENTENCE.                       ELTHOSP 
00727      PERFORM GENERATE-BS-BENEFITS-REDUCTION.                      ELTHOSP 
00728      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTHOSP 
00729      MOVE SPACES TO SCREEN-TYPE.                                  ELTHOSP 
00730      EJECT                                                        ELTHOSP 
00731                                                                   ELTHOSP 
00732                                                                   ELTHOSP 
00733 ************************************************************      ELTHOSP 
00734 *                                                          *      ELTHOSP 
00735 *        CREATE SUPPLEMENTAL TEXT                          *      ELTHOSP 
00736 *                                                          *      ELTHOSP 
00737 ************************************************************      ELTHOSP 
00738  CREATE-SUPPLEMENTAL-TEXT.                                        ELTHOSP 
00739      PERFORM TRANSLATE-DISPLAY-HOSPICE-IND.                       ELTHOSP 
00740      IF GSS-HO-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTHOSP 
00741          ZEROES                                                   ELTHOSP 
00742              AND SPACES AND LOW-VALUES                            ELTHOSP 
00743          PERFORM TRANSLATE-APPROVAL-SOURCE-IND.                   ELTHOSP 
00744      PERFORM TRANSLATE-MM-INDICATOR.                              ELTHOSP 
00745      IF GSS-HO-MM-IP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTHOSP 
00746          ZEROES                                                   ELTHOSP 
00747              AND SPACES AND LOW-VALUES                            ELTHOSP 
00748          PERFORM TRANSLATE-IP-MM-ALT-PRICING-ME.                  ELTHOSP 
00749      IF GSS-HO-MM-OP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTHOSP 
00750          ZEROES                                                   ELTHOSP 
00751              AND SPACES AND LOW-VALUES                            ELTHOSP 
00752          PERFORM TRANSLATE-OP-MM-ALT-PRICING-ME.                  ELTHOSP 
00753      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTHOSP 
00754      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTHOSP 
00755      PERFORM GENERATE-CALC-METHOD-SENTENCE.                       ELTHOSP 
00756      PERFORM GENERATE-MM-BENEFITS-REDUCTION.                      ELTHOSP 
00757      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTHOSP 
00758      EJECT                                                        ELTHOSP 
00759                                                                   ELTHOSP 
00760                                                                   ELTHOSP 
00761 ************************************************************      ELTHOSP 
00762 *                                                          *      ELTHOSP 
00763 *        TRANSLATE DISPLAY HOSPICE IND                     *      ELTHOSP 
00764 *                                                          *      ELTHOSP 
00765 ************************************************************      ELTHOSP 
00766  TRANSLATE-DISPLAY-HOSPICE-IND.                                   ELTHOSP 
00767      INITIALIZE TCAR-FROM-AREA.                                   ELTHOSP 
00768      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
00769      MOVE WS-HOSPICE-APPLIES TO TCAR-FROM-LINE                    ELTHOSP 
00770          (TCAR-FROM-SUB).                                         ELTHOSP 
00771      ADD  +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
00772      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHOSP 
00773      SET PERIOD-NEEDED TO TRUE.                                   ELTHOSP 
00774      MOVE GCG-HOSPICE-IND TO CMF-CODE-VALUE.                      ELTHOSP 
00775      MOVE 'HOSPICE-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTHOSP 
00776      MOVE PC-GRP TO CMF-RECORD-PREFIX.                            ELTHOSP 
00777      EXEC CICS LINK                                               ELTHOSP 
00778                PROGRAM ('ELUCMIF')                                ELTHOSP 
00779                COMMAREA (DFHCOMMAREA)                             ELTHOSP 
00780         END-EXEC.                                                 ELTHOSP 
00781      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
00782      MOVE SPACE TO ADDITIONAL-TEXT-SW.                            ELTHOSP 
00783      EJECT                                                        ELTHOSP 
00784                                                                   ELTHOSP 
00785                                                                   ELTHOSP 
00786 ************************************************************      ELTHOSP 
00787 *                                                          *      ELTHOSP 
00788 *        TRANSLATE BC INDICATOR                            *      ELTHOSP 
00789 *                                                          *      ELTHOSP 
00790 ************************************************************      ELTHOSP 
00791  TRANSLATE-BC-INDICATOR.                                          ELTHOSP 
00792      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTHOSP 
00793      MOVE GSS-HO-BC-IND (GSS-INDEX) TO  CMF-CODE-VALUE.           ELTHOSP 
00794      MOVE 'HO-BC-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTHOSP 
00795      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
00796      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
00797                                                                   ELTHOSP 
00798                                                                   ELTHOSP 
00799 ************************************************************      ELTHOSP 
00800 *                                                          *      ELTHOSP 
00801 *        GET INDICATOR FIXED TEXT                          *      ELTHOSP 
00802 *                                                          *      ELTHOSP 
00803 ************************************************************      ELTHOSP 
00804  GET-INDICATOR-FIXED-TEXT.                                        ELTHOSP 
00805      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
00806      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
00807      MOVE WS-INDICATOR TO TCAR-FROM-LINE                          ELTHOSP 
00808          (TCAR-FROM-SUB).                                         ELTHOSP 
00809      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHOSP 
00810      SET PERIOD-NEEDED TO TRUE.                                   ELTHOSP 
00811      ADD 1 TO TCAR-FROM-SUB.                                      ELTHOSP 
00812      EJECT                                                        ELTHOSP 
00813                                                                   ELTHOSP 
00814                                                                   ELTHOSP 
00815 ************************************************************      ELTHOSP 
00816 *                                                          *      ELTHOSP 
00817 *        SIGNAL NOT APPLICABLE FOR BC LOB                  *      ELTHOSP 
00818 *                                                          *      ELTHOSP 
00819 ************************************************************      ELTHOSP 
00820  SIGNAL-NOT-APPLICABLE-FOR-BC-L.                                  ELTHOSP 
00821      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHOSP 
00822      MOVE WS-NOT-INSTITUTIONAL-MSG TO COF-DTL-LINE                ELTHOSP 
00823          (COF-NBR-DTL-LINES).                                     ELTHOSP 
00824      PERFORM LINK-TO-OUTPUT.                                      ELTHOSP 
00825      EJECT                                                        ELTHOSP 
00826                                                                   ELTHOSP 
00827                                                                   ELTHOSP 
00828 ************************************************************      ELTHOSP 
00829 *                                                          *      ELTHOSP 
00830 *        TRANSLATE APPROVAL SOURCE IND                     *      ELTHOSP 
00831 *                                                          *      ELTHOSP 
00832 ************************************************************      ELTHOSP 
00833  TRANSLATE-APPROVAL-SOURCE-IND.                                   ELTHOSP 
00834      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
00835      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHOSP 
00836      SET PERIOD-NEEDED TO TRUE.                                   ELTHOSP 
00837      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
00838      MOVE WS-APPROVAL-SOURCE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTHOSP 
00839      ADD 1 TO TCAR-FROM-SUB.                                      ELTHOSP 
00840      IF NOT-HOLDING-APPROVAL-SRCE                                 ELTHOSP 
00841          PERFORM GET-APPROVAL-SOURCE-CODE-TRANS                   ELTHOSP 
00842      ELSE                                                         ELTHOSP 
00843          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTHOSP 
00844      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
00845      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTHOSP 
00846      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTHOSP 
00847                            WS-POINTER3.                           ELTHOSP 
00848      EJECT                                                        ELTHOSP 
00849                                                                   ELTHOSP 
00850                                                                   ELTHOSP 
00851 ************************************************************      ELTHOSP 
00852 *                                                          *      ELTHOSP 
00853 *        GET APPROVAL SOURCE CODE TRANSLATION              *      ELTHOSP 
00854 *                                                          *      ELTHOSP 
00855 ************************************************************      ELTHOSP 
00856  GET-APPROVAL-SOURCE-CODE-TRANS.                                  ELTHOSP 
00857      MOVE GSS-HO-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELTHOSP 
00858          CMF-CODE-VALUE.                                          ELTHOSP 
00859      MOVE 'HO-APPROVAL-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTHOSP 
00860      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
00861      SET HOLDING-APPROVAL-SOURCE TO TRUE.                         ELTHOSP 
00862      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTHOSP 
00863      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHOSP 
00864                            ADDRESS OF CMF-DESCR.                  ELTHOSP 
00865      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTHOSP 
00866      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTHOSP 
00867                            ADDRESS OF CMF-DESCR.                  ELTHOSP 
00868      SET WS-POINTER2 TO ADDRESS OF CMF-DESCR.                     ELTHOSP 
00869      EJECT                                                        ELTHOSP 
00870                                                                   ELTHOSP 
00871                                                                   ELTHOSP 
00872 ************************************************************      ELTHOSP 
00873 *                                                          *      ELTHOSP 
00874 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTHOSP 
00875 *                                                          *      ELTHOSP 
00876 ************************************************************      ELTHOSP 
00877  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTHOSP 
00878      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTHOSP 
00879      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTHOSP 
00880                            ADDRESS OF CMF-DESCR.                  ELTHOSP 
00881      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTHOSP 
00882      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTHOSP 
00883                            WS-POINTER2.                           ELTHOSP 
00884      EJECT                                                        ELTHOSP 
00885                                                                   ELTHOSP 
00886                                                                   ELTHOSP 
00887 ************************************************************      ELTHOSP 
00888 *                                                          *      ELTHOSP 
00889 *        TRANSLATE IP BC ALT PRICING METHOD                *      ELTHOSP 
00890 *                                                          *      ELTHOSP 
00891 ************************************************************      ELTHOSP 
00892  TRANSLATE-IP-BC-ALT-PRICING-ME.                                  ELTHOSP 
00893      PERFORM GET-BC-ALT-PRICING-FIXED-TEXT.                       ELTHOSP 
00894      MOVE GSS-HO-BC-IP-ALT-PRICING-METH (GSS-INDEX)  TO           ELTHOSP 
00895          CMF-CODE-VALUE.                                          ELTHOSP 
00896      MOVE 'HO-BC-IP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTHOSP 
00897      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
00898      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
00899      PERFORM WRITE-FINAL-IP-ALT-FIXED-TEXT.                       ELTHOSP 
00900                                                                   ELTHOSP 
00901                                                                   ELTHOSP 
00902 ************************************************************      ELTHOSP 
00903 *                                                          *      ELTHOSP 
00904 *        WRITE FINAL IP ALT FIXED TEXT                     *      ELTHOSP 
00905 *                                                          *      ELTHOSP 
00906 ************************************************************      ELTHOSP 
00907  WRITE-FINAL-IP-ALT-FIXED-TEXT.                                   ELTHOSP 
00908      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHOSP 
00909      SET PERIOD-NEEDED TO TRUE.                                   ELTHOSP 
00910      MOVE ' INPATIENT.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTHOSP 
00911      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTHOSP 
00912      EJECT                                                        ELTHOSP 
00913                                                                   ELTHOSP 
00914                                                                   ELTHOSP 
00915 ************************************************************      ELTHOSP 
00916 *                                                          *      ELTHOSP 
00917 *        TRANSLATE OP BC ALT PRICING METHOD                *      ELTHOSP 
00918 *                                                          *      ELTHOSP 
00919 ************************************************************      ELTHOSP 
00920  TRANSLATE-OP-BC-ALT-PRICING-ME.                                  ELTHOSP 
00921      PERFORM GET-BC-ALT-PRICING-FIXED-TEXT.                       ELTHOSP 
00922      MOVE GSS-HO-BC-OP-ALT-PRICING-METH (GSS-INDEX) TO            ELTHOSP 
00923          CMF-CODE-VALUE.                                          ELTHOSP 
00924      MOVE 'HO-BC-OP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTHOSP 
00925      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
00926      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
00927      PERFORM WRITE-FINAL-OP-ALT-FIXED-TEXT.                       ELTHOSP 
00928                                                                   ELTHOSP 
00929                                                                   ELTHOSP 
00930 ************************************************************      ELTHOSP 
00931 *                                                          *      ELTHOSP 
00932 *        GET BC ALT PRICING FIXED TEXT                     *      ELTHOSP 
00933 *                                                          *      ELTHOSP 
00934 ************************************************************      ELTHOSP 
00935  GET-BC-ALT-PRICING-FIXED-TEXT.                                   ELTHOSP 
00936      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
00937      INITIALIZE WS-PERIOD-SW.                                     ELTHOSP 
00938      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
00939      SET ADDITIONAL-TEXT TO TRUE.                                 ELTHOSP 
00940      MOVE WS-BC-ALT-PRICING TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELTHOSP 
00941      ADD 1 TO TCAR-FROM-SUB.                                      ELTHOSP 
00942                                                                   ELTHOSP 
00943                                                                   ELTHOSP 
00944 ************************************************************      ELTHOSP 
00945 *                                                          *      ELTHOSP 
00946 *        WRITE FINAL OP ALT FIXED TEXT                     *      ELTHOSP 
00947 *                                                          *      ELTHOSP 
00948 ************************************************************      ELTHOSP 
00949  WRITE-FINAL-OP-ALT-FIXED-TEXT.                                   ELTHOSP 
00950      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHOSP 
00951      SET PERIOD-NEEDED TO TRUE.                                   ELTHOSP 
00952      MOVE ' OUTPATIENT.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTHOSP 
00953      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTHOSP 
00954      EJECT                                                        ELTHOSP 
00955                                                                   ELTHOSP 
00956                                                                   ELTHOSP 
00957 ************************************************************      ELTHOSP 
00958 *                                                          *      ELTHOSP 
00959 *        GENERATE ASSOCIATED ACCUMULATORS                  *      ELTHOSP 
00960 *                                                          *      ELTHOSP 
00961 ************************************************************      ELTHOSP 
00962  GENERATE-ASSOCIATED-ACCUMULATO.                                  ELTHOSP 
00963      PERFORM GENERATE-COINSURANCE-TEXT.                           ELTHOSP 
00964      PERFORM GENERATE-COPAY-TEXT.                                 ELTHOSP 
00965      PERFORM GENERATE-DEDUCTIBLE-TEXT.                            ELTHOSP 
00966      PERFORM GENERATE-BENEFIT-MAXIMUMS-TEXT.                      ELTHOSP 
00967                                                                   ELTHOSP 
00968                                                                   ELTHOSP 
00969 ************************************************************      ELTHOSP 
00970 *                                                          *      ELTHOSP 
00971 *        GENERATE COPAY TEXT                               *      ELTHOSP 
00972 *                                                          *      ELTHOSP 
00973 ************************************************************      ELTHOSP 
00974  GENERATE-COPAY-TEXT.                                             ELTHOSP 
00975      EXEC CICS LINK                                               ELTHOSP 
00976          PROGRAM ('ELGACPCC')                                     ELTHOSP 
00977          COMMAREA (DFHCOMMAREA)                                   ELTHOSP 
00978          END-EXEC.                                                ELTHOSP 
00979                                                                   ELTHOSP 
00980 ************************************************************      ELTHOSP 
00981 *                                                          *      ELTHOSP 
00982 *        GENERATE COINSURANCE TEXT                         *      ELTHOSP 
00983 *                                                          *      ELTHOSP 
00984 ************************************************************      ELTHOSP 
00985  GENERATE-COINSURANCE-TEXT.                                       ELTHOSP 
00986      EXEC CICS LINK                                               ELTHOSP 
00987          PROGRAM ('ELGACLCC')                                     ELTHOSP 
00988          COMMAREA (DFHCOMMAREA)                                   ELTHOSP 
00989          END-EXEC.                                                ELTHOSP 
00990                                                                   ELTHOSP 
00991                                                                   ELTHOSP 
00992 ************************************************************      ELTHOSP 
00993 *                                                          *      ELTHOSP 
00994 *        GENERATE DEDUCTIBLE TEXT                          *      ELTHOSP 
00995 *                                                          *      ELTHOSP 
00996 ************************************************************      ELTHOSP 
00997  GENERATE-DEDUCTIBLE-TEXT.                                        ELTHOSP 
00998      EXEC CICS LINK                                               ELTHOSP 
00999          PROGRAM ('ELGADLCC')                                     ELTHOSP 
01000          COMMAREA (DFHCOMMAREA)                                   ELTHOSP 
01001          END-EXEC.                                                ELTHOSP 
01002                                                                   ELTHOSP 
01003                                                                   ELTHOSP 
01004 ************************************************************      ELTHOSP 
01005 *                                                          *      ELTHOSP 
01006 *        GENERATE BENEFIT MAXIMUMS TEXT                    *      ELTHOSP 
01007 *                                                          *      ELTHOSP 
01008 ************************************************************      ELTHOSP 
01009  GENERATE-BENEFIT-MAXIMUMS-TEXT.                                  ELTHOSP 
01010      EXEC CICS LINK                                               ELTHOSP 
01011          PROGRAM ('ELGABMCC')                                     ELTHOSP 
01012          COMMAREA (DFHCOMMAREA)                                   ELTHOSP 
01013          END-EXEC.                                                ELTHOSP 
01014      EJECT                                                        ELTHOSP 
01015                                                                   ELTHOSP 
01016                                                                   ELTHOSP 
01017 ************************************************************      ELTHOSP 
01018 *                                                          *      ELTHOSP 
01019 *        GENERATE CALC METHOD SENTENCE                     *      ELTHOSP 
01020 *                                                          *      ELTHOSP 
01021 ************************************************************      ELTHOSP 
01022  GENERATE-CALC-METHOD-SENTENCE.                                   ELTHOSP 
01023      IF GSS-HO-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES AND       ELTHOSP 
01024               SPACES AND LOW-VALUES                               ELTHOSP 
01025          PERFORM CREATE-CALC-SENTENCE.                            ELTHOSP 
01026                                                                   ELTHOSP 
01027                                                                   ELTHOSP 
01028 ************************************************************      ELTHOSP 
01029 *                                                          *      ELTHOSP 
01030 *        CREATE CALC SENTENCE                              *      ELTHOSP 
01031 *                                                          *      ELTHOSP 
01032 ************************************************************      ELTHOSP 
01033  CREATE-CALC-SENTENCE.                                            ELTHOSP 
01034      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01035      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01036      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHOSP 
01037      SET PERIOD-NEEDED TO TRUE.                                   ELTHOSP 
01038      MOVE GSS-HO-CALC-METHOD (GSS-INDEX) TO                       ELTHOSP 
01039          CMF-CODE-VALUE.                                          ELTHOSP 
01040      MOVE 'HO-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.            ELTHOSP 
01041      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01042      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01043                                                                   ELTHOSP 
01044                                                                   ELTHOSP 
01045 ************************************************************      ELTHOSP 
01046 *                                                          *      ELTHOSP 
01047 *        GENERATE COMBINED BENEFITS REDUCTION TEXT         *      ELTHOSP 
01048 *                                                          *      ELTHOSP 
01049 ************************************************************      ELTHOSP 
01050  GENERATE-COMBINED-BENEFITS-RED.                                  ELTHOSP 
01051      MOVE 'HO' TO SRP-COST-CONT-TYPE.                             ELTHOSP 
01052      MOVE 'HOSPICE PROGRAM' TO SRP-CCP-NAME.                      ELTHOSP 
01053      MOVE GSS-HO-COMB-BENE-REDUCT-IND (GSS-INDEX)                 ELTHOSP 
01054             TO SRP-CCP-COMB-BENE-REDUCT-IND.                      ELTHOSP 
01055      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTHOSP 
01056      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTHOSP 
01057                            ADDRESS OF                             ELTHOSP 
01058          GCCP-TABULAR-REC-AREA.                                   ELTHOSP 
01059      CALL 'ELGCBRI' USING DFHEIBLK                                ELTHOSP 
01060                           DFHCOMMAREA.                            ELTHOSP 
01061      EJECT                                                        ELTHOSP 
01062                                                                   ELTHOSP 
01063                                                                   ELTHOSP 
01064 ************************************************************      ELTHOSP 
01065 *                                                          *      ELTHOSP 
01066 *        GENERATE BC BENEFITS REDUCTION SENTENCE           *      ELTHOSP 
01067 *                                                          *      ELTHOSP 
01068 ************************************************************      ELTHOSP 
01069  GENERATE-BC-BENEFITS-REDUCTION.                                  ELTHOSP 
01070      IF GSS-HO-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTHOSP 
01071          ZEROES                                                   ELTHOSP 
01072                AND SPACES AND LOW-VALUES                          ELTHOSP 
01073          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTHOSP 
01074      IF (GSS-HO-BC-DEDU-APPLIC-IND (GSS-INDEX)  NOT EQUAL         ELTHOSP 
01075          ZEROES                                                   ELTHOSP 
01076                     AND SPACES  AND LOW-VALUES) OR                ELTHOSP 
01077               (GSS-HO-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL    ELTHOSP 
01078          ZEROES                                                   ELTHOSP 
01079                     AND SPACES AND LOW-VALUES) OR                 ELTHOSP 
01080               (GSS-HO-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT        ELTHOSP 
01081          EQUAL ZEROES                                             ELTHOSP 
01082                     AND SPACES AND LOW-VALUES)                    ELTHOSP 
01083          PERFORM GENERATE-BC-BEN-REDUCT-TEXT.                     ELTHOSP 
01084      EJECT                                                        ELTHOSP 
01085                                                                   ELTHOSP 
01086                                                                   ELTHOSP 
01087 ************************************************************      ELTHOSP 
01088 *                                                          *      ELTHOSP 
01089 *        GENERATE BC BEN REDUCT TEXT                       *      ELTHOSP 
01090 *                                                          *      ELTHOSP 
01091 ************************************************************      ELTHOSP 
01092  GENERATE-BC-BEN-REDUCT-TEXT.                                     ELTHOSP 
01093      PERFORM GENERATE-REDUCTIONS-HEADING.                         ELTHOSP 
01094      IF   GSS-HO-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTHOSP 
01095          ZEROES                                                   ELTHOSP 
01096                     AND SPACES AND LOW-VALUES                     ELTHOSP 
01097          PERFORM TRANSLATE-BC-DEDU-APLIC.                         ELTHOSP 
01098      IF   GSS-HO-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTHOSP 
01099          ZEROES                                                   ELTHOSP 
01100                     AND SPACES AND LOW-VALUES                     ELTHOSP 
01101          PERFORM TRANSLATE-BC-OPEX-APPLIC.                        ELTHOSP 
01102      PERFORM CREATE-A-BLANK-LINE.                                 ELTHOSP 
01103      PERFORM LINK-TO-OUTPUT.                                      ELTHOSP 
01104      IF   GSS-HO-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL       ELTHOSP 
01105          ZEROES                                                   ELTHOSP 
01106                     AND SPACES AND LOW-VALUES                     ELTHOSP 
01107          PERFORM TRANSLATE-BC-OPEX-OVERRIDE.                      ELTHOSP 
01108                                                                   ELTHOSP 
01109                                                                   ELTHOSP 
01110 ************************************************************      ELTHOSP 
01111 *                                                          *      ELTHOSP 
01112 *        GENERATE REDUCTIONS HEADING                       *      ELTHOSP 
01113 *                                                          *      ELTHOSP 
01114 ************************************************************      ELTHOSP 
01115  GENERATE-REDUCTIONS-HEADING.                                     ELTHOSP 
01116      INITIALIZE WS-PERIOD-SW.                                     ELTHOSP 
01117      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHOSP 
01118      MOVE WS-BENEFITS-REDUCTION TO COF-DTL-LINE                   ELTHOSP 
01119          (COF-NBR-DTL-LINES).                                     ELTHOSP 
01120      PERFORM LINK-TO-OUTPUT.                                      ELTHOSP 
01121      EJECT                                                        ELTHOSP 
01122                                                                   ELTHOSP 
01123                                                                   ELTHOSP 
01124 ************************************************************      ELTHOSP 
01125 *                                                          *      ELTHOSP 
01126 *        TRANSLATE BC DEDU APLIC                           *      ELTHOSP 
01127 *                                                          *      ELTHOSP 
01128 ************************************************************      ELTHOSP 
01129  TRANSLATE-BC-DEDU-APLIC.                                         ELTHOSP 
01130      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01131      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01132      MOVE GSS-HO-BC-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTHOSP 
01133          CMF-CODE-VALUE.                                          ELTHOSP 
01134      MOVE 'HO-BC-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTHOSP 
01135      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01136      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01137      EJECT                                                        ELTHOSP 
01138                                                                   ELTHOSP 
01139                                                                   ELTHOSP 
01140 ************************************************************      ELTHOSP 
01141 *                                                          *      ELTHOSP 
01142 *        TRANSLATE BC OPEX APPLIC                          *      ELTHOSP 
01143 *                                                          *      ELTHOSP 
01144 ************************************************************      ELTHOSP 
01145  TRANSLATE-BC-OPEX-APPLIC.                                        ELTHOSP 
01146      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01147      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTHOSP 
01148      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01149      MOVE GSS-HO-BC-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTHOSP 
01150          CMF-CODE-VALUE.                                          ELTHOSP 
01151      MOVE 'HO-BC-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTHOSP 
01152      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01153      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01154      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01155      EJECT                                                        ELTHOSP 
01156                                                                   ELTHOSP 
01157                                                                   ELTHOSP 
01158 ************************************************************      ELTHOSP 
01159 *                                                          *      ELTHOSP 
01160 *        TRANSLATE BC OPEX OVERRIDE                        *      ELTHOSP 
01161 *                                                          *      ELTHOSP 
01162 ************************************************************      ELTHOSP 
01163  TRANSLATE-BC-OPEX-OVERRIDE.                                      ELTHOSP 
01164      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01165      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHOSP 
01166      SET PERIOD-NEEDED TO TRUE.                                   ELTHOSP 
01167      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01168      MOVE GSS-HO-BC-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTHOSP 
01169          CMF-CODE-VALUE.                                          ELTHOSP 
01170      MOVE 'HO-BC-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTHOSP 
01171      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01172      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01173      EJECT                                                        ELTHOSP 
01174                                                                   ELTHOSP 
01175                                                                   ELTHOSP 
01176 ************************************************************      ELTHOSP 
01177 *                                                          *      ELTHOSP 
01178 *        TRANSLATE BS INDICATOR                            *      ELTHOSP 
01179 *                                                          *      ELTHOSP 
01180 ************************************************************      ELTHOSP 
01181  TRANSLATE-BS-INDICATOR.                                          ELTHOSP 
01182      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTHOSP 
01183      MOVE GSS-HO-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTHOSP 
01184      MOVE 'HO-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTHOSP 
01185      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01186      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01187      EJECT                                                        ELTHOSP 
01188                                                                   ELTHOSP 
01189                                                                   ELTHOSP 
01190 ************************************************************      ELTHOSP 
01191 *                                                          *      ELTHOSP 
01192 *        TRANSLATE IP BS ALT PRICING METHOD                *      ELTHOSP 
01193 *                                                          *      ELTHOSP 
01194 ************************************************************      ELTHOSP 
01195  TRANSLATE-IP-BS-ALT-PRICING-ME.                                  ELTHOSP 
01196      PERFORM GET-BS-ALT-PRICING-FIXED-TEXT.                       ELTHOSP 
01197      MOVE GSS-HO-BS-IP-ALT-PRICING-METH (GSS-INDEX) TO            ELTHOSP 
01198          CMF-CODE-VALUE.                                          ELTHOSP 
01199      MOVE 'HO-BS-IP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTHOSP 
01200      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01201      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01202      PERFORM WRITE-FINAL-IP-ALT-FIXED-TEXT.                       ELTHOSP 
01203      EJECT                                                        ELTHOSP 
01204                                                                   ELTHOSP 
01205                                                                   ELTHOSP 
01206 ************************************************************      ELTHOSP 
01207 *                                                          *      ELTHOSP 
01208 *        TRANSLATE OP BS ALT PRICING METHOD                *      ELTHOSP 
01209 *                                                          *      ELTHOSP 
01210 ************************************************************      ELTHOSP 
01211  TRANSLATE-OP-BS-ALT-PRICING-ME.                                  ELTHOSP 
01212      PERFORM GET-BS-ALT-PRICING-FIXED-TEXT.                       ELTHOSP 
01213      MOVE GSS-HO-BS-OP-ALT-PRICING-METH (GSS-INDEX)  TO           ELTHOSP 
01214          CMF-CODE-VALUE.                                          ELTHOSP 
01215      MOVE 'HO-BS-OP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTHOSP 
01216      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01217      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01218      PERFORM WRITE-FINAL-OP-ALT-FIXED-TEXT.                       ELTHOSP 
01219                                                                   ELTHOSP 
01220                                                                   ELTHOSP 
01221 ************************************************************      ELTHOSP 
01222 *                                                          *      ELTHOSP 
01223 *        GET BS ALT PRICING FIXED TEXT                     *      ELTHOSP 
01224 *                                                          *      ELTHOSP 
01225 ************************************************************      ELTHOSP 
01226  GET-BS-ALT-PRICING-FIXED-TEXT.                                   ELTHOSP 
01227      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01228      INITIALIZE WS-PERIOD-SW.                                     ELTHOSP 
01229      SET ADDITIONAL-TEXT TO TRUE.                                 ELTHOSP 
01230      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01231      MOVE WS-BS-ALT-PRICING TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELTHOSP 
01232      ADD 1 TO TCAR-FROM-SUB.                                      ELTHOSP 
01233                                                                   ELTHOSP 
01234                                                                   ELTHOSP 
01235 ************************************************************      ELTHOSP 
01236 *                                                          *      ELTHOSP 
01237 *        SIGNAL NOT APPLICABLE FOR BS LOB                  *      ELTHOSP 
01238 *                                                          *      ELTHOSP 
01239 ************************************************************      ELTHOSP 
01240  SIGNAL-NOT-APPLICABLE-FOR-BS-L.                                  ELTHOSP 
01241      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHOSP 
01242      MOVE WS-NOT-PROFESSIONAL-MSG                                 ELTHOSP 
01243              TO COF-DTL-LINE (COF-NBR-DTL-LINES).                 ELTHOSP 
01244      PERFORM LINK-TO-OUTPUT.                                      ELTHOSP 
01245      EJECT                                                        ELTHOSP 
01246                                                                   ELTHOSP 
01247                                                                   ELTHOSP 
01248 ************************************************************      ELTHOSP 
01249 *                                                          *      ELTHOSP 
01250 *        GENERATE BS BENEFITS REDUCTION SENTENCE           *      ELTHOSP 
01251 *                                                          *      ELTHOSP 
01252 ************************************************************      ELTHOSP 
01253  GENERATE-BS-BENEFITS-REDUCTION.                                  ELTHOSP 
01254      IF GSS-HO-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTHOSP 
01255          ZEROES                                                   ELTHOSP 
01256                AND SPACES AND LOW-VALUES                          ELTHOSP 
01257          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTHOSP 
01258      IF (GSS-HO-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTHOSP 
01259               ZEROES AND SPACES AND LOW-VALUES) OR                ELTHOSP 
01260               (GSS-HO-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT          ELTHOSP 
01261          EQUAL                                                    ELTHOSP 
01262               ZEROES AND SPACES AND LOW-VALUES) OR                ELTHOSP 
01263               (GSS-HO-BS-OPEX-OVERRIDE-IND  (GSS-INDEX) NOT       ELTHOSP 
01264          EQUAL                                                    ELTHOSP 
01265               ZEROES AND SPACES AND LOW-VALUES)                   ELTHOSP 
01266          PERFORM GENERATE-BS-BEN-REDUCT-TEXT.                     ELTHOSP 
01267      EJECT                                                        ELTHOSP 
01268                                                                   ELTHOSP 
01269                                                                   ELTHOSP 
01270 ************************************************************      ELTHOSP 
01271 *                                                          *      ELTHOSP 
01272 *        GENERATE BS BEN REDUCT TEXT                       *      ELTHOSP 
01273 *                                                          *      ELTHOSP 
01274 ************************************************************      ELTHOSP 
01275  GENERATE-BS-BEN-REDUCT-TEXT.                                     ELTHOSP 
01276      PERFORM GENERATE-REDUCTIONS-HEADING.                         ELTHOSP 
01277      IF GSS-HO-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTHOSP 
01278               ZEROES AND SPACES AND LOW-VALUES                    ELTHOSP 
01279          PERFORM TRANSLATE-BS-DEDU-APPLIC-IND.                    ELTHOSP 
01280      IF GSS-HO-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTHOSP 
01281               ZEROES AND SPACES AND LOW-VALUES                    ELTHOSP 
01282          PERFORM TRANSLATE-BS-OPEX-APPLIC.                        ELTHOSP 
01283      PERFORM CREATE-A-BLANK-LINE.                                 ELTHOSP 
01284      PERFORM LINK-TO-OUTPUT.                                      ELTHOSP 
01285      IF GSS-HO-BS-OPEX-OVERRIDE-IND  (GSS-INDEX) NOT EQUAL        ELTHOSP 
01286               ZEROES AND SPACES AND LOW-VALUES                    ELTHOSP 
01287          PERFORM TRANSLATE-BS-OPEX-OVERRIDE.                      ELTHOSP 
01288      EJECT                                                        ELTHOSP 
01289                                                                   ELTHOSP 
01290                                                                   ELTHOSP 
01291 ************************************************************      ELTHOSP 
01292 *                                                          *      ELTHOSP 
01293 *        TRANSLATE BS DEDU APPLIC IND                      *      ELTHOSP 
01294 *                                                          *      ELTHOSP 
01295 ************************************************************      ELTHOSP 
01296  TRANSLATE-BS-DEDU-APPLIC-IND.                                    ELTHOSP 
01297      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01298      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01299      MOVE GSS-HO-BS-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTHOSP 
01300          CMF-CODE-VALUE.                                          ELTHOSP 
01301      MOVE 'HO-BS-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTHOSP 
01302      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01303      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01304      EJECT                                                        ELTHOSP 
01305                                                                   ELTHOSP 
01306                                                                   ELTHOSP 
01307 ************************************************************      ELTHOSP 
01308 *                                                          *      ELTHOSP 
01309 *        TRANSLATE BS OPEX APPLIC                          *      ELTHOSP 
01310 *                                                          *      ELTHOSP 
01311 ************************************************************      ELTHOSP 
01312  TRANSLATE-BS-OPEX-APPLIC.                                        ELTHOSP 
01313      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01314      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01315      MOVE GSS-HO-BS-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTHOSP 
01316          CMF-CODE-VALUE.                                          ELTHOSP 
01317      MOVE 'HO-BS-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTHOSP 
01318      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01319      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01320      EJECT                                                        ELTHOSP 
01321                                                                   ELTHOSP 
01322                                                                   ELTHOSP 
01323 ************************************************************      ELTHOSP 
01324 *                                                          *      ELTHOSP 
01325 *        TRANSLATE BS OPEX OVERRIDE                        *      ELTHOSP 
01326 *                                                          *      ELTHOSP 
01327 ************************************************************      ELTHOSP 
01328  TRANSLATE-BS-OPEX-OVERRIDE.                                      ELTHOSP 
01329      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01330      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHOSP 
01331      SET PERIOD-NEEDED TO TRUE.                                   ELTHOSP 
01332      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01333      MOVE GSS-HO-BS-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTHOSP 
01334          CMF-CODE-VALUE.                                          ELTHOSP 
01335      MOVE  'HO-BS-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.  ELTHOSP 
01336      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01337      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01338      EJECT                                                        ELTHOSP 
01339                                                                   ELTHOSP 
01340                                                                   ELTHOSP 
01341 ************************************************************      ELTHOSP 
01342 *                                                          *      ELTHOSP 
01343 *        TRANSLATE MM INDICATOR                            *      ELTHOSP 
01344 *                                                          *      ELTHOSP 
01345 ************************************************************      ELTHOSP 
01346  TRANSLATE-MM-INDICATOR.                                          ELTHOSP 
01347      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01348      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTHOSP 
01349      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01350      MOVE GSS-HO-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTHOSP 
01351      MOVE 'HO-MM-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTHOSP 
01352      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01353      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01354                                                                   ELTHOSP 
01355                                                                   ELTHOSP 
01356 ************************************************************      ELTHOSP 
01357 *                                                          *      ELTHOSP 
01358 *        SIGNAL NOT APPLICABLE FOR MM LOB                  *      ELTHOSP 
01359 *                                                          *      ELTHOSP 
01360 ************************************************************      ELTHOSP 
01361  SIGNAL-NOT-APPLICABLE-FOR-MM-L.                                  ELTHOSP 
01362      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHOSP 
01363      MOVE WS-NOT-SUPPLEMENTAL-MSG TO COF-DTL-LINE                 ELTHOSP 
01364          (COF-NBR-DTL-LINES).                                     ELTHOSP 
01365      PERFORM LINK-TO-OUTPUT.                                      ELTHOSP 
01366      EJECT                                                        ELTHOSP 
01367                                                                   ELTHOSP 
01368                                                                   ELTHOSP 
01369 ************************************************************      ELTHOSP 
01370 *                                                          *      ELTHOSP 
01371 *        TRANSLATE IP MM ALT PRICING METHOD                *      ELTHOSP 
01372 *                                                          *      ELTHOSP 
01373 ************************************************************      ELTHOSP 
01374  TRANSLATE-IP-MM-ALT-PRICING-ME.                                  ELTHOSP 
01375      PERFORM GET-MM-ALT-PRICING-FIXED-TEXT.                       ELTHOSP 
01376      MOVE GSS-HO-MM-IP-ALT-PRICING-METH (GSS-INDEX)  TO           ELTHOSP 
01377          CMF-CODE-VALUE.                                          ELTHOSP 
01378      MOVE 'HO-MM-IP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTHOSP 
01379      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01380      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01381      PERFORM WRITE-FINAL-IP-ALT-FIXED-TEXT.                       ELTHOSP 
01382      EJECT                                                        ELTHOSP 
01383                                                                   ELTHOSP 
01384                                                                   ELTHOSP 
01385 ************************************************************      ELTHOSP 
01386 *                                                          *      ELTHOSP 
01387 *        TRANSLATE OP MM ALT PRICING METHOD                *      ELTHOSP 
01388 *                                                          *      ELTHOSP 
01389 ************************************************************      ELTHOSP 
01390  TRANSLATE-OP-MM-ALT-PRICING-ME.                                  ELTHOSP 
01391      PERFORM GET-MM-ALT-PRICING-FIXED-TEXT.                       ELTHOSP 
01392      MOVE GSS-HO-MM-OP-ALT-PRICING-METH (GSS-INDEX)  TO           ELTHOSP 
01393          CMF-CODE-VALUE.                                          ELTHOSP 
01394      MOVE 'HO-MM-OP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTHOSP 
01395      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01396      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01397      PERFORM WRITE-FINAL-OP-ALT-FIXED-TEXT.                       ELTHOSP 
01398                                                                   ELTHOSP 
01399                                                                   ELTHOSP 
01400 ************************************************************      ELTHOSP 
01401 *                                                          *      ELTHOSP 
01402 *        GET MM ALT PRICING FIXED TEXT                     *      ELTHOSP 
01403 *                                                          *      ELTHOSP 
01404 ************************************************************      ELTHOSP 
01405  GET-MM-ALT-PRICING-FIXED-TEXT.                                   ELTHOSP 
01406      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01407      INITIALIZE WS-PERIOD-SW.                                     ELTHOSP 
01408      SET ADDITIONAL-TEXT TO TRUE.                                 ELTHOSP 
01409      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01410      MOVE WS-MM-ALT-PRICING TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELTHOSP 
01411      ADD 1 TO TCAR-FROM-SUB.                                      ELTHOSP 
01412      EJECT                                                        ELTHOSP 
01413                                                                   ELTHOSP 
01414                                                                   ELTHOSP 
01415 ************************************************************      ELTHOSP 
01416 *                                                          *      ELTHOSP 
01417 *        GENERATE MM BENEFITS REDUCTION SENTENCE           *      ELTHOSP 
01418 *                                                          *      ELTHOSP 
01419 ************************************************************      ELTHOSP 
01420  GENERATE-MM-BENEFITS-REDUCTION.                                  ELTHOSP 
01421      IF GSS-HO-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTHOSP 
01422          ZEROES                                                   ELTHOSP 
01423                AND SPACES AND LOW-VALUES                          ELTHOSP 
01424          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTHOSP 
01425      IF (GSS-HO-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTHOSP 
01426                ZEROES AND SPACE AND LOW-VALUES) OR                ELTHOSP 
01427               (GSS-HO-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT          ELTHOSP 
01428          EQUAL                                                    ELTHOSP 
01429               ZEROES AND SPACE AND LOW-VALUES ) OR                ELTHOSP 
01430               (GSS-HO-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT        ELTHOSP 
01431          EQUAL                                                    ELTHOSP 
01432               ZEROES AND SPACE AND LOW-VALUES)                    ELTHOSP 
01433          PERFORM GENERATE-MM-BEN-REDUCT-TEXT.                     ELTHOSP 
01434      EJECT                                                        ELTHOSP 
01435                                                                   ELTHOSP 
01436                                                                   ELTHOSP 
01437 ************************************************************      ELTHOSP 
01438 *                                                          *      ELTHOSP 
01439 *        GENERATE MM BEN REDUCT TEXT                       *      ELTHOSP 
01440 *                                                          *      ELTHOSP 
01441 ************************************************************      ELTHOSP 
01442  GENERATE-MM-BEN-REDUCT-TEXT.                                     ELTHOSP 
01443      PERFORM GENERATE-REDUCTIONS-HEADING.                         ELTHOSP 
01444      IF   GSS-HO-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTHOSP 
01445                ZEROES AND SPACE AND LOW-VALUES                    ELTHOSP 
01446          PERFORM TRANSLATE-MM-DEDU-APLIC.                         ELTHOSP 
01447      IF   GSS-HO-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTHOSP 
01448               ZEROES AND SPACE AND LOW-VALUES                     ELTHOSP 
01449          PERFORM TRANSLATE-MM-OPEX-APPLIC.                        ELTHOSP 
01450      PERFORM CREATE-A-BLANK-LINE.                                 ELTHOSP 
01451      PERFORM LINK-TO-OUTPUT.                                      ELTHOSP 
01452      IF   GSS-HO-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL       ELTHOSP 
01453               ZEROES AND SPACE AND LOW-VALUES                     ELTHOSP 
01454          PERFORM TRANSLATE-MM-OPEX-OVERRIDE.                      ELTHOSP 
01455      EJECT                                                        ELTHOSP 
01456                                                                   ELTHOSP 
01457                                                                   ELTHOSP 
01458 ************************************************************      ELTHOSP 
01459 *                                                          *      ELTHOSP 
01460 *        TRANSLATE MM DEDU APLIC                           *      ELTHOSP 
01461 *                                                          *      ELTHOSP 
01462 ************************************************************      ELTHOSP 
01463  TRANSLATE-MM-DEDU-APLIC.                                         ELTHOSP 
01464      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01465      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01466      MOVE GSS-HO-MM-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTHOSP 
01467          CMF-CODE-VALUE.                                          ELTHOSP 
01468      MOVE   'HO-MM-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTHOSP 
01469      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01470      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01471      EJECT                                                        ELTHOSP 
01472                                                                   ELTHOSP 
01473                                                                   ELTHOSP 
01474 ************************************************************      ELTHOSP 
01475 *                                                          *      ELTHOSP 
01476 *        TRANSLATE MM OPEX APPLIC                          *      ELTHOSP 
01477 *                                                          *      ELTHOSP 
01478 ************************************************************      ELTHOSP 
01479  TRANSLATE-MM-OPEX-APPLIC.                                        ELTHOSP 
01480      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01481      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01482      MOVE GSS-HO-MM-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTHOSP 
01483          CMF-CODE-VALUE.                                          ELTHOSP 
01484      MOVE 'HO-MM-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTHOSP 
01485      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01486      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01487      EJECT                                                        ELTHOSP 
01488                                                                   ELTHOSP 
01489                                                                   ELTHOSP 
01490 ************************************************************      ELTHOSP 
01491 *                                                          *      ELTHOSP 
01492 *        TRANSLATE MM OPEX OVERRIDE                        *      ELTHOSP 
01493 *                                                          *      ELTHOSP 
01494 ************************************************************      ELTHOSP 
01495  TRANSLATE-MM-OPEX-OVERRIDE.                                      ELTHOSP 
01496      MOVE SPACES TO TCAR-FROM-AREA.                               ELTHOSP 
01497      SET BLANK-LINE-NEEDED TO TRUE.                               ELTHOSP 
01498      SET PERIOD-NEEDED TO TRUE.                                   ELTHOSP 
01499      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01500      MOVE GSS-HO-MM-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTHOSP 
01501          CMF-CODE-VALUE.                                          ELTHOSP 
01502      MOVE 'HO-MM-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTHOSP 
01503      PERFORM LINK-TO-TRANSLATOR.                                  ELTHOSP 
01504      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTHOSP 
01505      EJECT                                                        ELTHOSP 
01506                                                                   ELTHOSP 
01507                                                                   ELTHOSP 
01508 ************************************************************      ELTHOSP 
01509 *                                                          *      ELTHOSP 
01510 *        GENERATE HEADINGS                                 *      ELTHOSP 
01511 *                                                          *      ELTHOSP 
01512 ************************************************************      ELTHOSP 
01513  GENERATE-HEADINGS.                                               ELTHOSP 
01514      SET COF-NEW-PAGE TO TRUE.                                    ELTHOSP 
01515      IF INSTITUTIONAL-SCREEN                                      ELTHOSP 
01516          PERFORM MOVE-INST-HEADING                                ELTHOSP 
01517      ELSE IF PROFESSIONAL-SCREEN                                  ELTHOSP 
01518          PERFORM MOVE-PROF-HEADING                                ELTHOSP 
01519      ELSE IF SUPPLEMENTAL-SCREEN                                  ELTHOSP 
01520          PERFORM MOVE-SUPP-HEADING.                               ELTHOSP 
01521      MOVE 2  TO COF-NBR-HDR-LINES.                                ELTHOSP 
01522      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHOSP 
01523      MOVE WS-HDR-LN2 TO COF-HDR-LINE (2).                         ELTHOSP 
01524      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTHOSP 
01525      IF INSTITUTIONAL-SCREEN                                      ELTHOSP 
01526            OR PROFESSIONAL-SCREEN                                 ELTHOSP 
01527            OR SUPPLEMENTAL-SCREEN                                 ELTHOSP 
01528          PERFORM LINK-TO-OUTPUT.                                  ELTHOSP 
01529      EJECT                                                        ELTHOSP 
01530                                                                   ELTHOSP 
01531                                                                   ELTHOSP 
01532 ************************************************************      ELTHOSP 
01533 *                                                          *      ELTHOSP 
01534 *        GENERATE DISCLAIMER SENTENCE                      *      ELTHOSP 
01535 *                                                          *      ELTHOSP 
01536 ************************************************************      ELTHOSP 
01537  GENERATE-DISCLAIMER-SENTENCE.                                    ELTHOSP 
01538      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTHOSP 
01539      MOVE WS-DISCLAIMER TO COF-DTL-LINE                           ELTHOSP 
01540          (COF-NBR-DTL-LINES).                                     ELTHOSP 
01541      PERFORM LINK-TO-OUTPUT.                                      ELTHOSP 
01542                                                                   ELTHOSP 
01543                                                                   ELTHOSP 
01544 ************************************************************      ELTHOSP 
01545 *                                                          *      ELTHOSP 
01546 *        MOVE INST HEADING                                 *      ELTHOSP 
01547 *                                                          *      ELTHOSP 
01548 ************************************************************      ELTHOSP 
01549  MOVE-INST-HEADING.                                               ELTHOSP 
01550      MOVE 'INSTITUTIONAL' TO WS-HDR-TITLE.                        ELTHOSP 
01551      EJECT                                                        ELTHOSP 
01552                                                                   ELTHOSP 
01553                                                                   ELTHOSP 
01554 ************************************************************      ELTHOSP 
01555 *                                                          *      ELTHOSP 
01556 *        MOVE PROF HEADING                                 *      ELTHOSP 
01557 *                                                          *      ELTHOSP 
01558 ************************************************************      ELTHOSP 
01559  MOVE-PROF-HEADING.                                               ELTHOSP 
01560      MOVE 'PROFESSIONAL' TO WS-HDR-TITLE.                         ELTHOSP 
01561      EJECT                                                        ELTHOSP 
01562                                                                   ELTHOSP 
01563                                                                   ELTHOSP 
01564 ************************************************************      ELTHOSP 
01565 *                                                          *      ELTHOSP 
01566 *        MOVE SUPP HEADING                                 *      ELTHOSP 
01567 *                                                          *      ELTHOSP 
01568 ************************************************************      ELTHOSP 
01569  MOVE-SUPP-HEADING.                                               ELTHOSP 
01570      MOVE 'SUPPLEMENTAL' TO WS-HDR-TITLE.                         ELTHOSP 
01571                                                                   ELTHOSP 
01572                                                                   ELTHOSP 
01573 ************************************************************      ELTHOSP 
01574 *                                                          *      ELTHOSP 
01575 *        LINK TO TRANSLATOR                                *      ELTHOSP 
01576 *                                                          *      ELTHOSP 
01577 ************************************************************      ELTHOSP 
01578  LINK-TO-TRANSLATOR.                                              ELTHOSP 
01579      MOVE PC-GCCP  TO CMF-RECORD-PREFIX.                          ELTHOSP 
01580      EXEC CICS LINK                                               ELTHOSP 
01581          PROGRAM ('ELUCMIF')                                      ELTHOSP 
01582          COMMAREA (DFHCOMMAREA)                                   ELTHOSP 
01583          LENGTH  (LENGTH OF DFHCOMMAREA)                          ELTHOSP 
01584          END-EXEC.                                                ELTHOSP 
01585      EJECT                                                        ELTHOSP 
01586                                                                   ELTHOSP 
01587                                                                   ELTHOSP 
01588 ************************************************************      ELTHOSP 
01589 *                                                          *      ELTHOSP 
01590 *        READ GCCP RECORD                                  *      ELTHOSP 
01591 *                                                          *      ELTHOSP 
01592 ************************************************************      ELTHOSP 
01593  READ-GCCP-RECORD.                                                ELTHOSP 
01594      SET CIA-GCTABULR-DDN TO TRUE.                                ELTHOSP 
01595      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHOSP 
01596                            ADDRESS OF                             ELTHOSP 
01597          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTHOSP 
01598      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTHOSP 
01599      SET IOP-RD TO TRUE.                                          ELTHOSP 
01600      SET IOP-FCQ-NONE TO TRUE.                                    ELTHOSP 
01601      SET IOP-KVQ-EQ TO TRUE.                                      ELTHOSP 
01602      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTHOSP 
01603      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTHOSP 
01604      PERFORM LINK-TO-I-O-PGM.                                     ELTHOSP 
01605      EJECT                                                        ELTHOSP 
01606                                                                   ELTHOSP 
01607                                                                   ELTHOSP 
01608 ************************************************************      ELTHOSP 
01609 *                                                          *      ELTHOSP 
01610 *        LINK TO I O PGM                                   *      ELTHOSP 
01611 *                                                          *      ELTHOSP 
01612 ************************************************************      ELTHOSP 
01613  LINK-TO-I-O-PGM.                                                 ELTHOSP 
01614      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELTHOSP 
01615                COMMAREA (DFHCOMMAREA)                             ELTHOSP 
01616                LENGTH (LENGTH OF DFHCOMMAREA)                     ELTHOSP 
01617                END-EXEC.                                          ELTHOSP 
01618      IF IOP-RC-OK                                                 ELTHOSP 
01619          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTHOSP 
01620      ELSE IF IOP-RC-NOTFND                                        ELTHOSP 
01621          PERFORM SIGNAL-NOT-FOUND-GCTAB                           ELTHOSP 
01622      ELSE                                                         ELTHOSP 
01623          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTHOSP 
01624                                                                   ELTHOSP 
01625                                                                   ELTHOSP 
01626 ************************************************************      ELTHOSP 
01627 *                                                          *      ELTHOSP 
01628 *        ESTABLISH ADDRESSABILITY OF GCCP RECORD           *      ELTHOSP 
01629 *                                                          *      ELTHOSP 
01630 ************************************************************      ELTHOSP 
01631  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTHOSP 
01632      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTHOSP 
01633          IOP-REC-PTR.                                             ELTHOSP 
01634      SET IOP-REC-PTR TO NULL.                                     ELTHOSP 
01635      EJECT                                                        ELTHOSP 
01636                                                                   ELTHOSP 
01637                                                                   ELTHOSP 
01638 ************************************************************      ELTHOSP 
01639 *                                                          *      ELTHOSP 
01640 *        SIGNAL CRITICAL IO ERROR                          *      ELTHOSP 
01641 *                                                          *      ELTHOSP 
01642 ************************************************************      ELTHOSP 
01643  SIGNAL-CRITICAL-IO-ERROR.                                        ELTHOSP 
01644      SET CIA-AB-CRITIO TO TRUE.                                   ELTHOSP 
01645      PERFORM SIGNAL-ABEND.                                        ELTHOSP 
01646                                                                   ELTHOSP 
01647                                                                   ELTHOSP 
01648 ************************************************************      ELTHOSP 
01649 *                                                          *      ELTHOSP 
01650 *        SIGNAL NOT FOUND GCTAB                            *      ELTHOSP 
01651 *                                                          *      ELTHOSP 
01652 ************************************************************      ELTHOSP 
01653  SIGNAL-NOT-FOUND-GCTAB.                                          ELTHOSP 
01654      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTHOSP 
01655      PERFORM SIGNAL-ABEND.                                        ELTHOSP 
01656                                                                   ELTHOSP 
01657                                                                   ELTHOSP 
01658 ************************************************************      ELTHOSP 
01659 *                                                          *      ELTHOSP 
01660 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTHOSP 
01661 *                                                          *      ELTHOSP 
01662 ************************************************************      ELTHOSP 
01663  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTHOSP 
01664      PERFORM INITIALIZE-CMOUT.                                    ELTHOSP 
01665      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTHOSP 
01666      EJECT                                                        ELTHOSP 
01667                                                                   ELTHOSP 
01668                                                                   ELTHOSP 
01669 ************************************************************      ELTHOSP 
01670 *                                                          *      ELTHOSP 
01671 *        PREPARE TEXT FOR OUTPUT                           *      ELTHOSP 
01672 *                                                          *      ELTHOSP 
01673 ************************************************************      ELTHOSP 
01674  PREPARE-TEXT-FOR-OUTPUT.                                         ELTHOSP 
01675      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTHOSP 
01676          UNTIL CMF-DESCR-IDX                                      ELTHOSP 
01677                                    GREATER THAN                   ELTHOSP 
01678              CMF-NBR-DESCR-LINES.                                 ELTHOSP 
01679      EJECT                                                        ELTHOSP 
01680                                                                   ELTHOSP 
01681                                                                   ELTHOSP 
01682 ************************************************************      ELTHOSP 
01683 *                                                          *      ELTHOSP 
01684 *        INITIALIZE CMOUT                                  *      ELTHOSP 
01685 *                                                          *      ELTHOSP 
01686 ************************************************************      ELTHOSP 
01687  INITIALIZE-CMOUT.                                                ELTHOSP 
01688      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTHOSP 
01689      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHOSP 
01690          ADDRESS OF CMF-DESCR.                                    ELTHOSP 
01691      SET CMF-DESCR-IDX TO 1.                                      ELTHOSP 
01692      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTHOSP 
01693                                                                   ELTHOSP 
01694                                                                   ELTHOSP 
01695 ************************************************************      ELTHOSP 
01696 *                                                          *      ELTHOSP 
01697 *        MOVE CMF TEXT TO OUTPUT                           *      ELTHOSP 
01698 *                                                          *      ELTHOSP 
01699 ************************************************************      ELTHOSP 
01700  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTHOSP 
01701      PERFORM MOVE-A-LINE.                                         ELTHOSP 
01702      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTHOSP 
01703          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTHOSP 
01704      IF TCAR-FROM-SUB GREATER THAN 20                             ELTHOSP 
01705               OR CMF-DESCR-IDX GREATER THAN                       ELTHOSP 
01706          CMF-NBR-DESCR-LINES                                      ELTHOSP 
01707          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTHOSP 
01708                                                                   ELTHOSP 
01709                                                                   ELTHOSP 
01710 ************************************************************      ELTHOSP 
01711 *                                                          *      ELTHOSP 
01712 *        FINISH CODES MANUAL TEXT                          *      ELTHOSP 
01713 *                                                          *      ELTHOSP 
01714 ************************************************************      ELTHOSP 
01715  FINISH-CODES-MANUAL-TEXT.                                        ELTHOSP 
01716      SET DONE-PROCESSING TO TRUE.                                 ELTHOSP 
01717      IF PERIOD-NEEDED                                             ELTHOSP 
01718          PERFORM GET-AND-MOVE-PERIOD.                             ELTHOSP 
01719                                                                   ELTHOSP 
01720                                                                   ELTHOSP 
01721 ************************************************************      ELTHOSP 
01722 *                                                          *      ELTHOSP 
01723 *        GET AND MOVE PERIOD                               *      ELTHOSP 
01724 *                                                          *      ELTHOSP 
01725 ************************************************************      ELTHOSP 
01726  GET-AND-MOVE-PERIOD.                                             ELTHOSP 
01727      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTHOSP 
01728          (TCAR-FROM-SUB).                                         ELTHOSP 
01729                                                                   ELTHOSP 
01730                                                                   ELTHOSP 
01731 ************************************************************      ELTHOSP 
01732 *                                                          *      ELTHOSP 
01733 *        SAVE LAST LINE                                    *      ELTHOSP 
01734 *                                                          *      ELTHOSP 
01735 ************************************************************      ELTHOSP 
01736  SAVE-LAST-LINE.                                                  ELTHOSP 
01737      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01738      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTHOSP 
01739         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTHOSP 
01740      ADD 1 TO TCAR-FROM-SUB.                                      ELTHOSP 
01741      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTHOSP 
01742      EJECT                                                        ELTHOSP 
01743                                                                   ELTHOSP 
01744                                                                   ELTHOSP 
01745 ************************************************************      ELTHOSP 
01746 *                                                          *      ELTHOSP 
01747 *        OUTPUT LAST LINE                                  *      ELTHOSP 
01748 *                                                          *      ELTHOSP 
01749 ************************************************************      ELTHOSP 
01750  OUTPUT-LAST-LINE.                                                ELTHOSP 
01751      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTHOSP 
01752          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTHOSP 
01753      IF BLANK-LINE-NEEDED                                         ELTHOSP 
01754          PERFORM CREATE-A-BLANK-LINE.                             ELTHOSP 
01755      EJECT                                                        ELTHOSP 
01756                                                                   ELTHOSP 
01757                                                                   ELTHOSP 
01758 ************************************************************      ELTHOSP 
01759 *                                                          *      ELTHOSP 
01760 *        CREATE A BLANK LINE                               *      ELTHOSP 
01761 *                                                          *      ELTHOSP 
01762 ************************************************************      ELTHOSP 
01763  CREATE-A-BLANK-LINE.                                             ELTHOSP 
01764      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTHOSP 
01765      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTHOSP 
01766                                                                   ELTHOSP 
01767                                                                   ELTHOSP 
01768 ************************************************************      ELTHOSP 
01769 *                                                          *      ELTHOSP 
01770 *        MOVE A LINE                                       *      ELTHOSP 
01771 *                                                          *      ELTHOSP 
01772 ************************************************************      ELTHOSP 
01773  MOVE-A-LINE.                                                     ELTHOSP 
01774      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTHOSP 
01775          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTHOSP 
01776      SET CMF-DESCR-IDX UP BY 1.                                   ELTHOSP 
01777      ADD 1 TO TCAR-FROM-SUB.                                      ELTHOSP 
01778      EJECT                                                        ELTHOSP 
01779                                                                   ELTHOSP 
01780                                                                   ELTHOSP 
01781 ************************************************************      ELTHOSP 
01782 *                                                          *      ELTHOSP 
01783 *        REFORMAT AND WRITE TEXT                           *      ELTHOSP 
01784 *                                                          *      ELTHOSP 
01785 ************************************************************      ELTHOSP 
01786  REFORMAT-AND-WRITE-TEXT.                                         ELTHOSP 
01787      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTHOSP 
01788      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTHOSP 
01789      PERFORM UNSTRING-TEXT.                                       ELTHOSP 
01790      MOVE +1 TO TCAR-FROM-SUB.                                    ELTHOSP 
01791      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTHOSP 
01792      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTHOSP 
01793          UNTIL COF-NBR-DTL-LINES GREATER                          ELTHOSP 
01794                                   TCAR-OUTPUT-FIELDS-USED -       ELTHOSP 
01795              1.                                                   ELTHOSP 
01796      PERFORM DISPOSE-OF-LAST-LINE.                                ELTHOSP 
01797      PERFORM LINK-TO-OUTPUT.                                      ELTHOSP 
01798                                                                   ELTHOSP 
01799                                                                   ELTHOSP 
01800 ************************************************************      ELTHOSP 
01801 *                                                          *      ELTHOSP 
01802 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTHOSP 
01803 *                                                          *      ELTHOSP 
01804 ************************************************************      ELTHOSP 
01805  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTHOSP 
01806      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTHOSP 
01807           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTHOSP 
01808      ADD +1 TO TCAR-FROM-SUB.                                     ELTHOSP 
01809      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTHOSP 
01810      EJECT                                                        ELTHOSP 
01811                                                                   ELTHOSP 
01812                                                                   ELTHOSP 
01813 ************************************************************      ELTHOSP 
01814 *                                                          *      ELTHOSP 
01815 *        UNSTRING TEXT                                     *      ELTHOSP 
01816 *                                                          *      ELTHOSP 
01817 ************************************************************      ELTHOSP 
01818  UNSTRING-TEXT.                                                   ELTHOSP 
01819      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTHOSP 
01820      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTHOSP 
01821      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTHOSP 
01822      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTHOSP 
01823      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTHOSP 
01824      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTHOSP 
01825      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTHOSP 
01826      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTHOSP 
01827      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTHOSP 
01828      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTHOSP 
01829      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTHOSP 
01830      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTHOSP 
01831      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTHOSP 
01832      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTHOSP 
01833      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTHOSP 
01834      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTHOSP 
01835      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTHOSP 
01836      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTHOSP 
01837      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTHOSP 
01838      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTHOSP 
01839      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTHOSP 
01840      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTHOSP 
01841                                                                   ELTHOSP 
01842                                                                   ELTHOSP 
01843 ************************************************************      ELTHOSP 
01844 *                                                          *      ELTHOSP 
01845 *        LINK TO OUTPUT                                    *      ELTHOSP 
01846 *                                                          *      ELTHOSP 
01847 ************************************************************      ELTHOSP 
01848  LINK-TO-OUTPUT.                                                  ELTHOSP 
01849      EXEC CICS LINK                                               ELTHOSP 
01850          PROGRAM ('ELUOUTPT')                                     ELTHOSP 
01851          COMMAREA (DFHCOMMAREA)                                   ELTHOSP 
01852          END-EXEC.                                                ELTHOSP 
01853      EJECT                                                        ELTHOSP 
01854                                                                   ELTHOSP 
01855                                                                   ELTHOSP 
01856 ************************************************************      ELTHOSP 
01857 *                                                          *      ELTHOSP 
01858 *        DISPOSE OF LAST LINE                              *      ELTHOSP 
01859 *                                                          *      ELTHOSP 
01860 ************************************************************      ELTHOSP 
01861  DISPOSE-OF-LAST-LINE.                                            ELTHOSP 
01862      IF NOT ADDITIONAL-TEXT                                       ELTHOSP 
01863          PERFORM INITIALIZE-CONTINUED-SW.                         ELTHOSP 
01864      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTHOSP 
01865          PERFORM SAVE-LAST-LINE                                   ELTHOSP 
01866      ELSE                                                         ELTHOSP 
01867          PERFORM OUTPUT-LAST-LINE.                                ELTHOSP 
01868                                                                   ELTHOSP 
01869                                                                   ELTHOSP 
01870 ************************************************************      ELTHOSP 
01871 *                                                          *      ELTHOSP 
01872 *        INITIALIZE CONTINUED SW                           *      ELTHOSP 
01873 *                                                          *      ELTHOSP 
01874 ************************************************************      ELTHOSP 
01875  INITIALIZE-CONTINUED-SW.                                         ELTHOSP 
01876      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTHOSP 
