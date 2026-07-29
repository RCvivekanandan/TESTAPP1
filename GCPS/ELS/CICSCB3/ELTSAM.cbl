00001 *      LAST MAINTENANCE TIME: 11.01.15  DATE: 06/13/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTSAM  
00003                                                                      LV001
00004  PROGRAM-ID.         ELTSAM.                                      ELTSAM  
00005                                                                   ELTSAM  
00006  AUTHOR.             ANNE KEFFER KING.                            ELTSAM  
00007                                                                   ELTSAM  
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTSAM  
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTSAM  
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTSAM  
00011                      233 N. MICHIGAN AVE                          ELTSAM  
00012                      CHICAGO, ILLINOIS 60601                      ELTSAM  
00013                                                                   ELTSAM  
00014  DATE-WRITTEN.       10-SEP-1990.                                 ELTSAM  
00015                                                                   ELTSAM  
00016  DATE-COMPILED.                                                   ELTSAM  
00017                                                                   ELTSAM  
00018  SECURITY.           COPYRIGHT 1986,                              ELTSAM  
00019                      HEALTH CARE SERVICE CORPORATION              ELTSAM  
00020                                                                   ELTSAM  
00021  ENVIRONMENT DIVISION.                                            ELTSAM  
00022                                                                   ELTSAM  
00023  CONFIGURATION SECTION.                                           ELTSAM  
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELTSAM  
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELTSAM  
00026      EJECT                                                        ELTSAM  
00027                                                                   ELTSAM  
00028 ******************************************************************ELTSAM  
00029 *                       N O T E                                  *ELTSAM  
00030 * THE NAME OF THIS CCP HAS BEEN CHANGED TO EXTENDED MENTAL HEALTH*ELTSAM  
00031 * ALSO EVEN THOUGH OTHER CODES EXIST ON CODES MANUAL ONLY THE    *ELTSAM  
00032 * GROUP SPECIFIC PARTICPATION CODES OF 04 (MANDATORY) AND 08     *ELTSAM  
00033 * (VOLUNTARY) ARE BEING USED.  IF THE 04 AND 08 ARE ENCOUNTERED  *ELTSAM  
00034 * 'NORMAL' GCCP TABULAR PROCESSING IS BYPASSED.  IF ANY OTHER    *ELTSAM  
00035 * CODE IS FOUND NORMAL GCCP TABULAR PROCESSING CONTINUES.        *ELTSAM  
00036 ******************************************************************ELTSAM  
00037 *                       MAINTENANCE HISTORY                      *ELTSAM  
00038 *                                                                *ELTSAM  
00039 * CHANGE ISSR     DATE     BY                ACTION              *ELTSAM  
00040 * ------ ----- ----------- --- --------------------------------- *ELTSAM  
00041 * 01.00        10-SEP-1990 AKK CREATED                           *ELTSAM  
00042 *                                                                *ELTSAM  
00043 * 01.01  11836 14-MAY-1991 JPB CHANGED REFERENCES TO SUBSTANCE   *ELTSAM  
00044 *                          AKK ABUSE MENTAL (OR SAM) TO EXTENDED *ELTSAM  
00045 *                              MENTAL HEALTH (OR EMH).           *ELTSAM  
00046 *                              CHANGE DONE TWICE BECAUSE FOR     *ELTSAM  
00047 *                              SOME RESAON THE ORGINAL CHANGE    *ELTSAM  
00048 *                              GOT LOST.                         *ELTSAM  
00049 * 01.01        21-MAY-1991 AKK CHANGED CODE TO ADD DIRECT TRANS- *ELTSAM  
00050 *                               LATION OF GRP SPEC PARTIC IND    *ELTSAM  
00051 *                               THIS PROGRAM HAS ONLY 2 CODES    *ELTSAM  
00052 *                               THAT ARE CURRENTLY BEING USED    *ELTSAM  
00053 *                               (4 AND 8). SEE NOTE ABOVE.       *ELTSAM  
00054 ******************************************************************ELTSAM  
00055  DATA DIVISION.                                                   ELTSAM  
00056 /                                                                 ELTSAM  
00057 *                                                                 ELTSAM  
00058  WORKING-STORAGE SECTION.                                         ELTSAM  
00059  01  WS-BEGIN                            PIC X(24) VALUE          ELTSAM  
00060                                 '** ELTSAM WS BEGINS **'.         ELTSAM  
00061  01  WS-MISC-FIELDS.                                              ELTSAM  
00062      05  WS-CMF-SUB                    PIC S9(04) COMP.           ELTSAM  
00063      05  SCREEN-TYPE                   PIC X  VALUE SPACES.       ELTSAM  
00064          88  INSTITUTIONAL-SCREEN             VALUE 'I'.          ELTSAM  
00065          88  PROFESSIONAL-SCREEN              VALUE 'P'.          ELTSAM  
00066          88  SUPPLEMENTAL-SCREEN              VALUE 'S'.          ELTSAM  
00067 *                                                                 ELTSAM  
00068  01  PROGRAM-CONSTANTS.                                           ELTSAM  
00069      05  PC-GCCP                 PIC X(06)  VALUE '#GCCP '.       ELTSAM  
00070      05  PC-GROUP                PIC X(06)  VALUE 'GROUP '.       ELTSAM  
00071 *                                                                 ELTSAM  
00072  01  WS-SWITCHES.                                                 ELTSAM  
00073      05  UNDEFINED-TABULAR-SW     PIC X      VALUE 'N'.           ELTSAM  
00074          88 TABULAR-IS-UNDEFINED             VALUE 'Y'.           ELTSAM  
00075      05  DEFINED-TABULAR-SW       PIC X      VALUE 'N'.           ELTSAM  
00076          88 TABULAR-IS-DEFINED               VALUE 'Y'.           ELTSAM  
00077 *                                                                 ELTSAM  
00078      05  ADDITIONAL-TEXT-SW       PIC X      VALUE SPACE.         ELTSAM  
00079          88 BLANK-LINE-NEEDED                VALUE 'B'.           ELTSAM  
00080          88 ADDITIONAL-TEXT                  VALUE 'Y'.           ELTSAM  
00081 *                                                                 ELTSAM  
00082      05  CONTINUED-PROCESSING-SW  PIC X      VALUE SPACE.         ELTSAM  
00083          88 PROCESSING-CMF-TEXT              VALUE 'P'.           ELTSAM  
00084          88 DONE-PROCESSING                  VALUE 'D'.           ELTSAM  
00085 *                                                                 ELTSAM  
00086      05  WS-PERIOD-SW             PIC X      VALUE 'N'.           ELTSAM  
00087          88 PERIOD-NEEDED                    VALUE 'Y'.           ELTSAM  
00088 *                                                                 ELTSAM  
00089      05  WS-PAYMENT-LEVEL-SW      PIC X      VALUE SPACE.         ELTSAM  
00090          88 PAYMENT-LVL-TRANSLATED           VALUE 'N'.           ELTSAM  
00091          88 PAYMENT-LVL-NOT-TRANSLATED       VALUE 'Y'.           ELTSAM  
00092 *                                                                 ELTSAM  
00093 ******************************************************************ELTSAM  
00094 *SCREEN BODY LINES                                                ELTSAM  
00095 ******************************************************************ELTSAM  
00096  01  WS-HDR-LN2.                                                  ELTSAM  
00097      05  FILLER            PIC X(18)         VALUE SPACES.        ELTSAM  
00098      05  FILLER            PIC X(31)         VALUE                ELTSAM  
00099          'EXTENDED MENTAL HEALTH PROGRAM '.                       ELTSAM  
00100      05  HDR-TITLE         PIC X(13)         VALUE SPACES.        ELTSAM  
00101      05  FILLER            PIC X(17)         VALUE SPACES.        ELTSAM  
00102 *                                                                 ELTSAM  
00103 *WS-HDR-LN2A FOR USE WITH PARTICIPATION CODES '04' AND '08'       ELTSAM  
00104 *                                                                 ELTSAM  
00105  01  WS-HDR-LN2A.                                                 ELTSAM  
00106      05  FILLER            PIC X(25)         VALUE SPACES.        ELTSAM  
00107      05  FILLER            PIC X(30)         VALUE                ELTSAM  
00108          'EXTENDED MENTAL HEALTH PROGRAM'.                        ELTSAM  
00109      05  FILLER            PIC X(24)         VALUE SPACES.        ELTSAM  
00110                                                                   ELTSAM  
00111  01  WS-APPROVAL-SOURCE.                                          ELTSAM  
00112      05  FILLER             PIC X(79)        VALUE                ELTSAM  
00113          'THE EXTENDED MENTAL HEALTH PROGRAM REQUIRES THE APPROVALELTSAM  
00114 -        ' OF '.                                                  ELTSAM  
00115 *                                                                 ELTSAM  
00116  01  WS-PART-IND.                                                 ELTSAM  
00117      05  FILLER             PIC X(79)        VALUE                ELTSAM  
00118          'THE EXTENDED MENTAL HEALTH PROGRAM APPLIES TO '.        ELTSAM  
00119 *                                                                 ELTSAM  
00120  01  WS-INDICATOR.                                                ELTSAM  
00121      05  FILLER             PIC X(79)        VALUE                ELTSAM  
00122          'THE EXTENDED MENTAL HEALTH PROGRAM '.                   ELTSAM  
00123 *                                                                 ELTSAM  
00124  01  WS-PAYMENT-LEVEL.                                            ELTSAM  
00125      05  FILLER                  PIC X(79)   VALUE                ELTSAM  
00126      'EXTENDED MENTAL HEALTH PAYMENT LEVEL RULES ARE AS FOLLOWS:'.ELTSAM  
00127 *                                                                 ELTSAM  
00128  01  WS-CALC-METHOD.                                              ELTSAM  
00129      05  FILLER                  PIC X(79)   VALUE                ELTSAM  
00130      'THE METHOD(S) FOR CALCULATING EXTENDED MENTAL HEALTH BENEFITELTSAM  
00131 -    'S IS AS FOLLOWS:'.                                          ELTSAM  
00132 *                                                                 ELTSAM  
00133  01  WS-ALTERNATE-PRICING.                                        ELTSAM  
00134      05  FILLER                  PIC X(79)   VALUE                ELTSAM  
00135          'THE ALTERNATE PRICING FOR PROFESSIONAL SERVICES IS '.   ELTSAM  
00136 *                                                                 ELTSAM  
00137  01  WS-BENEFITS-REDUCTION.                                       ELTSAM  
00138      05  FILLER                  PIC X(79)   VALUE                ELTSAM  
00139          'DENIED OR REDUCED BENEFITS DUE TO COST CONTAINMENT:'.   ELTSAM  
00140 *                                                                 ELTSAM  
00141  01  WS-SPILLOVER-SENTENCE.                                       ELTSAM  
00142      05  FILLER                 PIC X(79)    VALUE                ELTSAM  
00143          'UNPAID SERVICES AFTER BASIC BENEFIT REDUCTIONS ARE '.   ELTSAM  
00144 *                                                                 ELTSAM  
00145  01  WS-NOT-APPLICABLE-LOB-BC.                                    ELTSAM  
00146      05  FILLER                    PIC X(79)   VALUE              ELTSAM  
00147          'THE EXTENDED MENTAL HEALTH PROGRAM DOES NOT APPLY TO INSELTSAM  
00148 -        'TITUTIONAL BENEFITS.'.                                  ELTSAM  
00149 *                                                                 ELTSAM  
00150  01  WS-NOT-APPLICABLE-LOB-BS.                                    ELTSAM  
00151      05  FILLER                    PIC X(79)   VALUE              ELTSAM  
00152          'THE EXTENDED MENTAL HEALTH PROGRAM DOES NOT APPLY TO PROELTSAM  
00153 -        'FESSIONAL BENEFITS.'.                                   ELTSAM  
00154 *                                                                 ELTSAM  
00155  01  WS-NOT-APPLICABLE-LOB-MM.                                    ELTSAM  
00156      05  FILLER                    PIC X(79)   VALUE              ELTSAM  
00157          'THE EXTENDED MENTAL HEALTH PROGRAM DOES NOT APPLY TO SUPELTSAM  
00158 -        'PLEMENTAL BENEFITS.'.                                   ELTSAM  
00159 *                                                                 ELTSAM  
00160  01  WS-VOLUNTARY-MSG.                                            ELTSAM  
00161      05  FILLER                    PIC  X(79) VALUE               ELTSAM  
00162          'THE EXTENDED MENTAL HEALTH PROGRAM IS VOLUNTARY.'.      ELTSAM  
00163 *                                                                 ELTSAM  
00164  01  WS-NOT-APPLICABLE-MSG.                                       ELTSAM  
00165      05  FILLER                    PIC X(79)  VALUE               ELTSAM  
00166          'THE EXTENDED MENTAL HEALTH PROGRAM IS NOT APPLICABLE.'. ELTSAM  
00167 *                                                                 ELTSAM  
00168  01  WS-DISCLAIMER.                                               ELTSAM  
00169      05  FILLER                    PIC X(79)  VALUE               ELTSAM  
00170         '*** SUBJECT TO CONTRACT LIMITATIONS ***'.                ELTSAM  
00171  01  WS-END                              PIC X(18) VALUE          ELTSAM  
00172                                          '*** END OF W/S ***'.    ELTSAM  
00173  LINKAGE SECTION.                                                 ELTSAM  
00174  01  DFHCOMMAREA.                                                 ELTSAM  
00175      COPY ELSCOMMC.                                               ELTSAM  
00176 /                                                                 ELTSAM  
00177      COPY ELSCIA2C.                                               ELTSAM  
00178 /                                                                 ELTSAM  
00179      COPY ELSCMDSC.                                               ELTSAM  
00180 /                                                                 ELTSAM  
00181      COPY ELSCMIFC.                                               ELTSAM  
00182 /                                                                 ELTSAM  
00183      COPY ELSIOPMC.                                               ELTSAM  
00184 /                                                                 ELTSAM  
00185      COPY ELSKEYSC.                                               ELTSAM  
00186 /                                                                 ELTSAM  
00187      COPY ELSOUTPC.                                               ELTSAM  
00188 /                                                                 ELTSAM  
00189      COPY ELSSRTPC.                                               ELTSAM  
00190 /                                                                 ELTSAM  
00191      COPY ELSTCWAC.                                               ELTSAM  
00192 /                                                                 ELTSAM  
00193      COPY ELSSSCBC.                                               ELTSAM  
00194 /                                                                 ELTSAM  
00195  01  GROUP-SPECIFIC-RECORD.                                       ELTSAM  
00196      COPY GCGROUPC.                                               ELTSAM  
00197 /                                                                 ELTSAM  
00198  01  GCCP-TABULAR-REC.                                            ELTSAM  
00199      COPY GCTGCCPC.                                               ELTSAM  
00200 /                                                                 ELTSAM  
00201      EJECT                                                        ELTSAM  
00202  PROCEDURE DIVISION.                                              ELTSAM  
00203 ************************************************************      ELTSAM  
00204 *                                                          *      ELTSAM  
00205 *                    PROCEDURE DIVISION                    *      ELTSAM  
00206 *                                                          *      ELTSAM  
00207 ************************************************************      ELTSAM  
00208                                                                   ELTSAM  
00209                                                                   ELTSAM  
00210 ************************************************************      ELTSAM  
00211 *                                                          *      ELTSAM  
00212 *        EXTENDED MENTAL HEALTH                            *      ELTSAM  
00213 *                                                          *      ELTSAM  
00214 ************************************************************      ELTSAM  
00215  EXTENDED-MENTAL-HEALTH.                                          ELTSAM  
00216      PERFORM INITIALIZATION.                                      ELTSAM  
00217      PERFORM PROCESS-EMH.                                         ELTSAM  
00218      GOBACK.                                                      ELTSAM  
00219                                                                   ELTSAM  
00220                                                                   ELTSAM  
00221 ************************************************************      ELTSAM  
00222 *                                                          *      ELTSAM  
00223 *        INITIALIZATION                                    *      ELTSAM  
00224 *                                                          *      ELTSAM  
00225 ************************************************************      ELTSAM  
00226  INITIALIZATION.                                                  ELTSAM  
00227      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTSAM  
00228      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTSAM  
00229                                                                   ELTSAM  
00230                                                                   ELTSAM  
00231 ************************************************************      ELTSAM  
00232 *                                                          *      ELTSAM  
00233 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTSAM  
00234 *                                                          *      ELTSAM  
00235 ************************************************************      ELTSAM  
00236  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTSAM  
00237      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTSAM  
00238      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTSAM  
00239      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTSAM  
00240                                                                   ELTSAM  
00241                                                                   ELTSAM  
00242 ************************************************************      ELTSAM  
00243 *                                                          *      ELTSAM  
00244 *        CHECK FOR VALID COMMAREA                          *      ELTSAM  
00245 *                                                          *      ELTSAM  
00246 ************************************************************      ELTSAM  
00247  CHECK-FOR-VALID-COMMAREA.                                        ELTSAM  
00248      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTSAM  
00249          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTSAM  
00250                                                                   ELTSAM  
00251                                                                   ELTSAM  
00252 ************************************************************      ELTSAM  
00253 *                                                          *      ELTSAM  
00254 *        SIGNAL INVALID COMMAREA                           *      ELTSAM  
00255 *                                                          *      ELTSAM  
00256 ************************************************************      ELTSAM  
00257  SIGNAL-INVALID-COMMAREA.                                         ELTSAM  
00258      EXEC CICS ABEND                                              ELTSAM  
00259                ABCODE('EL01')                                     ELTSAM  
00260         END-EXEC.                                                 ELTSAM  
00261      EJECT                                                        ELTSAM  
00262                                                                   ELTSAM  
00263                                                                   ELTSAM  
00264 ************************************************************      ELTSAM  
00265 *                                                          *      ELTSAM  
00266 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTSAM  
00267 *                                                          *      ELTSAM  
00268 ************************************************************      ELTSAM  
00269  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTSAM  
00270      IF ECA-CIA-PTR = NULL                                        ELTSAM  
00271          PERFORM SIGNAL-INVALID-CIA                               ELTSAM  
00272      ELSE                                                         ELTSAM  
00273          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTSAM  
00274                                                                   ELTSAM  
00275                                                                   ELTSAM  
00276 ************************************************************      ELTSAM  
00277 *                                                          *      ELTSAM  
00278 *        SIGNAL INVALID CIA                                *      ELTSAM  
00279 *                                                          *      ELTSAM  
00280 ************************************************************      ELTSAM  
00281  SIGNAL-INVALID-CIA.                                              ELTSAM  
00282      EXEC CICS ABEND                                              ELTSAM  
00283                ABCODE('EL02')                                     ELTSAM  
00284         END-EXEC.                                                 ELTSAM  
00285      EJECT                                                        ELTSAM  
00286                                                                   ELTSAM  
00287                                                                   ELTSAM  
00288 ************************************************************      ELTSAM  
00289 *                                                          *      ELTSAM  
00290 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTSAM  
00291 *                                                          *      ELTSAM  
00292 ************************************************************      ELTSAM  
00293  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTSAM  
00294      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTSAM  
00295      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSAM  
00296          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTSAM  
00297      IF CIA-RC-PTR-NULL                                           ELTSAM  
00298          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTSAM  
00299                                                                   ELTSAM  
00300                                                                   ELTSAM  
00301 ************************************************************      ELTSAM  
00302 *                                                          *      ELTSAM  
00303 *        SIGNAL UNALLOC AREA ERROR                         *      ELTSAM  
00304 *                                                          *      ELTSAM  
00305 ************************************************************      ELTSAM  
00306  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTSAM  
00307      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTSAM  
00308      PERFORM SIGNAL-ABEND.                                        ELTSAM  
00309                                                                   ELTSAM  
00310                                                                   ELTSAM  
00311 ************************************************************      ELTSAM  
00312 *                                                          *      ELTSAM  
00313 *        SIGNAL ABEND                                      *      ELTSAM  
00314 *                                                          *      ELTSAM  
00315 ************************************************************      ELTSAM  
00316  SIGNAL-ABEND.                                                    ELTSAM  
00317      EXEC CICS ABEND                                              ELTSAM  
00318                ABCODE(CIA-ABCODE)                                 ELTSAM  
00319         END-EXEC.                                                 ELTSAM  
00320      EJECT                                                        ELTSAM  
00321                                                                   ELTSAM  
00322                                                                   ELTSAM  
00323 ************************************************************      ELTSAM  
00324 *                                                          *      ELTSAM  
00325 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTSAM  
00326 *                                                          *      ELTSAM  
00327 ************************************************************      ELTSAM  
00328  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTSAM  
00329      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTSAM  
00330      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTSAM  
00331      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTSAM  
00332      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTSAM  
00333      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTSAM  
00334      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTSAM  
00335      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTSAM  
00336                                                                   ELTSAM  
00337                                                                   ELTSAM  
00338 ************************************************************      ELTSAM  
00339 *                                                          *      ELTSAM  
00340 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTSAM  
00341 *                                                          *      ELTSAM  
00342 ************************************************************      ELTSAM  
00343  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTSAM  
00344      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTSAM  
00345      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSAM  
00346          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTSAM  
00347      IF CIA-RC-PTR-NULL                                           ELTSAM  
00348          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTSAM  
00349      EJECT                                                        ELTSAM  
00350                                                                   ELTSAM  
00351                                                                   ELTSAM  
00352 ************************************************************      ELTSAM  
00353 *                                                          *      ELTSAM  
00354 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTSAM  
00355 *                                                          *      ELTSAM  
00356 ************************************************************      ELTSAM  
00357  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTSAM  
00358      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTSAM  
00359      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSAM  
00360          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTSAM  
00361      IF CIA-RC-PTR-NULL                                           ELTSAM  
00362          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTSAM  
00363      EJECT                                                        ELTSAM  
00364                                                                   ELTSAM  
00365                                                                   ELTSAM  
00366 ************************************************************      ELTSAM  
00367 *                                                          *      ELTSAM  
00368 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTSAM  
00369 *                                                          *      ELTSAM  
00370 ************************************************************      ELTSAM  
00371  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTSAM  
00372      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTSAM  
00373      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSAM  
00374          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELTSAM  
00375      IF CIA-RC-PTR-NULL                                           ELTSAM  
00376          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTSAM  
00377      EJECT                                                        ELTSAM  
00378                                                                   ELTSAM  
00379                                                                   ELTSAM  
00380 ************************************************************      ELTSAM  
00381 *                                                          *      ELTSAM  
00382 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTSAM  
00383 *                                                          *      ELTSAM  
00384 ************************************************************      ELTSAM  
00385  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTSAM  
00386      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTSAM  
00387      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSAM  
00388          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTSAM  
00389      IF CIA-RC-PTR-NULL                                           ELTSAM  
00390          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTSAM  
00391      EJECT                                                        ELTSAM  
00392                                                                   ELTSAM  
00393                                                                   ELTSAM  
00394 ************************************************************      ELTSAM  
00395 *                                                          *      ELTSAM  
00396 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTSAM  
00397 *                                                          *      ELTSAM  
00398 ************************************************************      ELTSAM  
00399  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTSAM  
00400      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTSAM  
00401      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSAM  
00402          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTSAM  
00403      IF CIA-RC-PTR-NULL                                           ELTSAM  
00404          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTSAM  
00405      EJECT                                                        ELTSAM  
00406                                                                   ELTSAM  
00407                                                                   ELTSAM  
00408 ************************************************************      ELTSAM  
00409 *                                                          *      ELTSAM  
00410 *        ESTABLISH ADDRESSABILITY OF GRP SPECIFIC          *      ELTSAM  
00411 *                                                          *      ELTSAM  
00412 ************************************************************      ELTSAM  
00413  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTSAM  
00414      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTSAM  
00415      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSAM  
00416          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTSAM  
00417      IF CIA-RC-PTR-NULL                                           ELTSAM  
00418          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTSAM  
00419      EJECT                                                        ELTSAM  
00420                                                                   ELTSAM  
00421                                                                   ELTSAM  
00422 ************************************************************      ELTSAM  
00423 *                                                          *      ELTSAM  
00424 *        ESTABLISH ADDRESSABILITY OF COST CONTAINMENT      *      ELTSAM  
00425 *                                                          *      ELTSAM  
00426 ************************************************************      ELTSAM  
00427  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTSAM  
00428      SET CIA-GCTABULR-DDN TO TRUE.                                ELTSAM  
00429      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSAM  
00430          ADDRESS OF GCCP-TABULAR-REC.                             ELTSAM  
00431      IF CIA-RC-PTR-NULL                                           ELTSAM  
00432          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTSAM  
00433                                                                   ELTSAM  
00434                                                                   ELTSAM  
00435 ************************************************************      ELTSAM  
00436 *                                                          *      ELTSAM  
00437 *        ESTABLISH ADDRESS OF CIA                          *      ELTSAM  
00438 *                                                          *      ELTSAM  
00439 ************************************************************      ELTSAM  
00440  ESTABLISH-ADDRESS-OF-CIA.                                        ELTSAM  
00441      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTSAM  
00442          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTSAM  
00443      EJECT                                                        ELTSAM  
00444                                                                   ELTSAM  
00445                                                                   ELTSAM  
00446 ************************************************************      ELTSAM  
00447 *                                                          *      ELTSAM  
00448 *        PROCESS EMH                                       *      ELTSAM  
00449 *                                                          *      ELTSAM  
00450 ************************************************************      ELTSAM  
00451  PROCESS-EMH.                                                     ELTSAM  
00452      IF GCG-SUBS-ABUSE-MENTAL-IND EQUAL ZERO                      ELTSAM  
00453          PERFORM TEST-APPLICABILITY                               ELTSAM  
00454      ELSE IF GCG-SUBS-ABUSE-MENTAL-IND EQUAL                      ELTSAM  
00455                  '04' OR '08'                                     ELTSAM  
00456          PERFORM GENERATE-VOL-MAND-PARTICIPATIO                   ELTSAM  
00457      ELSE                                                         ELTSAM  
00458          PERFORM GENERATE-EXTENDED-MENTAL-HEALT.                  ELTSAM  
00459      MOVE 'E' TO  COF-FUNCTION.                                   ELTSAM  
00460      MOVE ZEROS TO COF-NBR-DTL-LINES                              ELTSAM  
00461                COF-NBR-HDR-LINES.                                 ELTSAM  
00462      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
00463                                                                   ELTSAM  
00464                                                                   ELTSAM  
00465 ************************************************************      ELTSAM  
00466 *                                                          *      ELTSAM  
00467 *        GENERATE EXTENDED MENTAL HEALTH TEXT              *      ELTSAM  
00468 *                                                          *      ELTSAM  
00469 ************************************************************      ELTSAM  
00470  GENERATE-EXTENDED-MENTAL-HEALT.                                  ELTSAM  
00471      PERFORM VERIFY-EXTENDED-MENTAL-HEALTHX.                      ELTSAM  
00472      PERFORM BUILD-EXTENDED-MENTAL-HEALTH-T.                      ELTSAM  
00473      EJECT                                                        ELTSAM  
00474                                                                   ELTSAM  
00475                                                                   ELTSAM  
00476 ************************************************************      ELTSAM  
00477 *                                                          *      ELTSAM  
00478 *        GENERATE VOL MAND PARTICIPATION                   *      ELTSAM  
00479 *                                                          *      ELTSAM  
00480 ************************************************************      ELTSAM  
00481  GENERATE-VOL-MAND-PARTICIPATIO.                                  ELTSAM  
00482      PERFORM GENERATE-VOL-MAND-HEADINGS.                          ELTSAM  
00483      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
00484      SET BLANK-LINE-NEEDED TO TRUE.                               ELTSAM  
00485      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
00486      MOVE PC-GROUP TO CMF-RECORD-PREFIX.                          ELTSAM  
00487      MOVE 'SUBS-ABUSE-MENTAL-IND'  TO                             ELTSAM  
00488          CMF-ELEMENT-SYSTEM-NAME.                                 ELTSAM  
00489      MOVE GCG-SUBS-ABUSE-MENTAL-IND TO CMF-CODE-VALUE.            ELTSAM  
00490      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
00491      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
00492      EJECT                                                        ELTSAM  
00493                                                                   ELTSAM  
00494                                                                   ELTSAM  
00495 ************************************************************      ELTSAM  
00496 *                                                          *      ELTSAM  
00497 *        TEST APPLICABILITY                                *      ELTSAM  
00498 *                                                          *      ELTSAM  
00499 ************************************************************      ELTSAM  
00500  TEST-APPLICABILITY.                                              ELTSAM  
00501      PERFORM GENERATE-HEADINGS.                                   ELTSAM  
00502      IF GCG-SUBS-ABUSE-MENTAL-IND  EQUAL ZERO                     ELTSAM  
00503          PERFORM SIGNAL-NOT-APPLICABLE-MSG.                       ELTSAM  
00504      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
00505      EJECT                                                        ELTSAM  
00506                                                                   ELTSAM  
00507                                                                   ELTSAM  
00508 ************************************************************      ELTSAM  
00509 *                                                          *      ELTSAM  
00510 *        VERIFY EXTENDED MENTAL HEALTH IN GCCP RECORD      *      ELTSAM  
00511 *                                                          *      ELTSAM  
00512 ************************************************************      ELTSAM  
00513  VERIFY-EXTENDED-MENTAL-HEALTHX.                                  ELTSAM  
00514      PERFORM ACQUIRE-GCCP-RECORD.                                 ELTSAM  
00515      PERFORM OBTAIN-EXTENDED-MENTAL-HEALTHX.                      ELTSAM  
00516                                                                   ELTSAM  
00517                                                                   ELTSAM  
00518 ************************************************************      ELTSAM  
00519 *                                                          *      ELTSAM  
00520 *        ACQUIRE GCCP RECORD                               *      ELTSAM  
00521 *                                                          *      ELTSAM  
00522 ************************************************************      ELTSAM  
00523  ACQUIRE-GCCP-RECORD.                                             ELTSAM  
00524      MOVE SPACES TO KWA-PROVISION-ID.                             ELTSAM  
00525      SET GCG-INDEX TO 1.                                          ELTSAM  
00526      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTSAM  
00527          AT END                                                   ELTSAM  
00528             MOVE ZEROES TO KWA-PROVISION-SLOT-NO                  ELTSAM  
00529             WHEN GCG-TAB-ID (GCG-INDEX) = PC-GCCP                 ELTSAM  
00530                 MOVE GCG-TAB-ID (GCG-INDEX) TO                    ELTSAM  
00531          KWA-PROVISION-ID                                         ELTSAM  
00532                 MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                  ELTSAM  
00533                     TO KWA-PROVISION-SLOT-NO                      ELTSAM  
00534          END-SEARCH.                                              ELTSAM  
00535      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTSAM  
00536          PERFORM SIGNAL-UNDEFINED-TABULAR                         ELTSAM  
00537      ELSE                                                         ELTSAM  
00538          PERFORM READ-GCCP-RECORD.                                ELTSAM  
00539      EJECT                                                        ELTSAM  
00540                                                                   ELTSAM  
00541                                                                   ELTSAM  
00542 ************************************************************      ELTSAM  
00543 *                                                          *      ELTSAM  
00544 *        SIGNAL UNDEFINED TABULAR                          *      ELTSAM  
00545 *                                                          *      ELTSAM  
00546 ************************************************************      ELTSAM  
00547  SIGNAL-UNDEFINED-TABULAR.                                        ELTSAM  
00548      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTSAM  
00549      PERFORM SIGNAL-ABEND.                                        ELTSAM  
00550      EJECT                                                        ELTSAM  
00551                                                                   ELTSAM  
00552                                                                   ELTSAM  
00553 ************************************************************      ELTSAM  
00554 *                                                          *      ELTSAM  
00555 *        OBTAIN EXTENDED MENTAL HEALTH WITHIN GCCP RECORD  *      ELTSAM  
00556 *                                                          *      ELTSAM  
00557 ************************************************************      ELTSAM  
00558  OBTAIN-EXTENDED-MENTAL-HEALTHX.                                  ELTSAM  
00559      SET GSS-INDEX TO 1.                                          ELTSAM  
00560      SEARCH GSS-ENTRY                                             ELTSAM  
00561         AT END                                                    ELTSAM  
00562            SET TABULAR-IS-UNDEFINED TO TRUE                       ELTSAM  
00563            WHEN GSS-SA-PROG-CODE-CHR (GSS-INDEX)                  ELTSAM  
00564                 CONTINUE                                          ELTSAM  
00565          END-SEARCH.                                              ELTSAM  
00566      IF TABULAR-IS-UNDEFINED                                      ELTSAM  
00567          PERFORM SIGNAL-UNDEFINED-TABULAR.                        ELTSAM  
00568      EJECT                                                        ELTSAM  
00569                                                                   ELTSAM  
00570                                                                   ELTSAM  
00571 ************************************************************      ELTSAM  
00572 *                                                          *      ELTSAM  
00573 *        BUILD EXTENDED MENTAL HEALTH TEXT                 *      ELTSAM  
00574 *                                                          *      ELTSAM  
00575 ************************************************************      ELTSAM  
00576  BUILD-EXTENDED-MENTAL-HEALTH-T.                                  ELTSAM  
00577      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTSAM  
00578          PERFORM GENERATE-INSTITUTIONAL.                          ELTSAM  
00579      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTSAM  
00580          PERFORM GENERATE-PROFESSIONAL.                           ELTSAM  
00581      IF GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                        ELTSAM  
00582                 '03' OR '04' OR '06' OR '08'                      ELTSAM  
00583          PERFORM PROCESS-SUPPLEMENTAL.                            ELTSAM  
00584                                                                   ELTSAM  
00585                                                                   ELTSAM  
00586 ************************************************************      ELTSAM  
00587 *                                                          *      ELTSAM  
00588 *        GENERATE INSTITUTIONAL                            *      ELTSAM  
00589 *                                                          *      ELTSAM  
00590 ************************************************************      ELTSAM  
00591  GENERATE-INSTITUTIONAL.                                          ELTSAM  
00592      SET INSTITUTIONAL-SCREEN TO TRUE.                            ELTSAM  
00593      PERFORM GENERATE-HEADINGS.                                   ELTSAM  
00594      PERFORM BUILD-INSTITUTIONAL-TEXT.                            ELTSAM  
00595      EJECT                                                        ELTSAM  
00596                                                                   ELTSAM  
00597                                                                   ELTSAM  
00598 ************************************************************      ELTSAM  
00599 *                                                          *      ELTSAM  
00600 *        GENERATE PROFESSIONAL                             *      ELTSAM  
00601 *                                                          *      ELTSAM  
00602 ************************************************************      ELTSAM  
00603  GENERATE-PROFESSIONAL.                                           ELTSAM  
00604      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTSAM  
00605      PERFORM GENERATE-HEADINGS.                                   ELTSAM  
00606      PERFORM BUILD-PROFESSIONAL-TEXT.                             ELTSAM  
00607      EJECT                                                        ELTSAM  
00608                                                                   ELTSAM  
00609                                                                   ELTSAM  
00610 ************************************************************      ELTSAM  
00611 *                                                          *      ELTSAM  
00612 *        PROCESS SUPPLEMENTAL                              *      ELTSAM  
00613 *                                                          *      ELTSAM  
00614 ************************************************************      ELTSAM  
00615  PROCESS-SUPPLEMENTAL.                                            ELTSAM  
00616      SET SUPPLEMENTAL-SCREEN TO TRUE.                             ELTSAM  
00617      PERFORM GENERATE-HEADINGS.                                   ELTSAM  
00618      PERFORM BUILD-SUPPLEMENTAL-TEXT.                             ELTSAM  
00619      EJECT                                                        ELTSAM  
00620                                                                   ELTSAM  
00621                                                                   ELTSAM  
00622 ************************************************************      ELTSAM  
00623 *                                                          *      ELTSAM  
00624 *        GENERATE HEADINGS                                 *      ELTSAM  
00625 *                                                          *      ELTSAM  
00626 ************************************************************      ELTSAM  
00627  GENERATE-HEADINGS.                                               ELTSAM  
00628      SET COF-NEW-PAGE TO TRUE.                                    ELTSAM  
00629      IF INSTITUTIONAL-SCREEN                                      ELTSAM  
00630          PERFORM MOVE-INST-HEADINGS                               ELTSAM  
00631      ELSE IF PROFESSIONAL-SCREEN                                  ELTSAM  
00632          PERFORM MOVE-PROF-HEADINGS                               ELTSAM  
00633      ELSE IF SUPPLEMENTAL-SCREEN                                  ELTSAM  
00634          PERFORM MOVE-SUPP-HEADINGS.                              ELTSAM  
00635      MOVE 2            TO COF-NBR-HDR-LINES.                      ELTSAM  
00636      MOVE WS-HDR-LN2   TO COF-HDR-LINE                            ELTSAM  
00637          (COF-NBR-HDR-LINES).                                     ELTSAM  
00638      MOVE +1           TO COF-NBR-DTL-LINES.                      ELTSAM  
00639      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTSAM  
00640      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
00641                                                                   ELTSAM  
00642                                                                   ELTSAM  
00643 ************************************************************      ELTSAM  
00644 *                                                          *      ELTSAM  
00645 *        GENERATE VOL MAND HEADINGS                        *      ELTSAM  
00646 *                                                          *      ELTSAM  
00647 ************************************************************      ELTSAM  
00648  GENERATE-VOL-MAND-HEADINGS.                                      ELTSAM  
00649      SET COF-NEW-PAGE TO TRUE.                                    ELTSAM  
00650      MOVE 2            TO COF-NBR-HDR-LINES.                      ELTSAM  
00651      MOVE WS-HDR-LN2A  TO COF-HDR-LINE (COF-NBR-HDR-LINES).       ELTSAM  
00652      MOVE +1           TO COF-NBR-DTL-LINES.                      ELTSAM  
00653      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTSAM  
00654      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
00655                                                                   ELTSAM  
00656                                                                   ELTSAM  
00657 ************************************************************      ELTSAM  
00658 *                                                          *      ELTSAM  
00659 *        MOVE INST HEADINGS                                *      ELTSAM  
00660 *                                                          *      ELTSAM  
00661 ************************************************************      ELTSAM  
00662  MOVE-INST-HEADINGS.                                              ELTSAM  
00663      MOVE 'INSTITUTIONAL' TO HDR-TITLE.                           ELTSAM  
00664                                                                   ELTSAM  
00665                                                                   ELTSAM  
00666 ************************************************************      ELTSAM  
00667 *                                                          *      ELTSAM  
00668 *        MOVE PROF HEADINGS                                *      ELTSAM  
00669 *                                                          *      ELTSAM  
00670 ************************************************************      ELTSAM  
00671  MOVE-PROF-HEADINGS.                                              ELTSAM  
00672      MOVE 'PROFESSIONAL' TO HDR-TITLE.                            ELTSAM  
00673                                                                   ELTSAM  
00674                                                                   ELTSAM  
00675 ************************************************************      ELTSAM  
00676 *                                                          *      ELTSAM  
00677 *        MOVE SUPP HEADINGS                                *      ELTSAM  
00678 *                                                          *      ELTSAM  
00679 ************************************************************      ELTSAM  
00680  MOVE-SUPP-HEADINGS.                                              ELTSAM  
00681      MOVE 'SUPPLEMENTAL' TO HDR-TITLE.                            ELTSAM  
00682                                                                   ELTSAM  
00683                                                                   ELTSAM  
00684 ************************************************************      ELTSAM  
00685 *                                                          *      ELTSAM  
00686 *        BUILD INSTITUTIONAL TEXT                          *      ELTSAM  
00687 *                                                          *      ELTSAM  
00688 ************************************************************      ELTSAM  
00689  BUILD-INSTITUTIONAL-TEXT.                                        ELTSAM  
00690      IF GSS-SA-BC-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTSAM  
00691                 OR LOW-VALUES                                     ELTSAM  
00692          PERFORM SIGNAL-NOT-APPLICABLE-FOR-BC-L                   ELTSAM  
00693      ELSE                                                         ELTSAM  
00694          PERFORM CONSTRUCT-BC-TEXT-AND-SCREEN.                    ELTSAM  
00695                                                                   ELTSAM  
00696                                                                   ELTSAM  
00697 ************************************************************      ELTSAM  
00698 *                                                          *      ELTSAM  
00699 *        BUILD PROFESSIONAL TEXT                           *      ELTSAM  
00700 *                                                          *      ELTSAM  
00701 ************************************************************      ELTSAM  
00702  BUILD-PROFESSIONAL-TEXT.                                         ELTSAM  
00703      IF GSS-SA-BS-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTSAM  
00704                 OR LOW-VALUES                                     ELTSAM  
00705          PERFORM SIGNAL-NOT-APPLICABLE-FOR-BS-L                   ELTSAM  
00706      ELSE                                                         ELTSAM  
00707          PERFORM CONSTRUCT-BS-TEXT-AND-SCREEN.                    ELTSAM  
00708                                                                   ELTSAM  
00709                                                                   ELTSAM  
00710 ************************************************************      ELTSAM  
00711 *                                                          *      ELTSAM  
00712 *        BUILD SUPPLEMENTAL TEXT                           *      ELTSAM  
00713 *                                                          *      ELTSAM  
00714 ************************************************************      ELTSAM  
00715  BUILD-SUPPLEMENTAL-TEXT.                                         ELTSAM  
00716      IF GSS-SA-MM-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTSAM  
00717                 OR LOW-VALUES                                     ELTSAM  
00718          PERFORM SIGNAL-NOT-APPLICABLE-FOR-MM-L                   ELTSAM  
00719      ELSE                                                         ELTSAM  
00720          PERFORM CONSTRUCT-MM-TEXT-AND-SCREEN.                    ELTSAM  
00721      EJECT                                                        ELTSAM  
00722                                                                   ELTSAM  
00723                                                                   ELTSAM  
00724 ************************************************************      ELTSAM  
00725 *                                                          *      ELTSAM  
00726 *        SIGNAL NOT APPLICABLE FOR BC LOB                  *      ELTSAM  
00727 *                                                          *      ELTSAM  
00728 ************************************************************      ELTSAM  
00729  SIGNAL-NOT-APPLICABLE-FOR-BC-L.                                  ELTSAM  
00730      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTSAM  
00731      MOVE WS-NOT-APPLICABLE-LOB-BC TO COF-DTL-LINE                ELTSAM  
00732          (COF-NBR-DTL-LINES).                                     ELTSAM  
00733      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
00734                                                                   ELTSAM  
00735                                                                   ELTSAM  
00736 ************************************************************      ELTSAM  
00737 *                                                          *      ELTSAM  
00738 *        SIGNAL NOT APPLICABLE FOR BS LOB                  *      ELTSAM  
00739 *                                                          *      ELTSAM  
00740 ************************************************************      ELTSAM  
00741  SIGNAL-NOT-APPLICABLE-FOR-BS-L.                                  ELTSAM  
00742      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTSAM  
00743      MOVE WS-NOT-APPLICABLE-LOB-BS TO COF-DTL-LINE                ELTSAM  
00744          (COF-NBR-DTL-LINES).                                     ELTSAM  
00745      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
00746                                                                   ELTSAM  
00747                                                                   ELTSAM  
00748 ************************************************************      ELTSAM  
00749 *                                                          *      ELTSAM  
00750 *        SIGNAL NOT APPLICABLE FOR MM LOB                  *      ELTSAM  
00751 *                                                          *      ELTSAM  
00752 ************************************************************      ELTSAM  
00753  SIGNAL-NOT-APPLICABLE-FOR-MM-L.                                  ELTSAM  
00754      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTSAM  
00755      MOVE WS-NOT-APPLICABLE-LOB-MM TO COF-DTL-LINE                ELTSAM  
00756          (COF-NBR-DTL-LINES).                                     ELTSAM  
00757      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
00758      EJECT                                                        ELTSAM  
00759                                                                   ELTSAM  
00760                                                                   ELTSAM  
00761 ************************************************************      ELTSAM  
00762 *                                                          *      ELTSAM  
00763 *        CONSTRUCT BC TEXT AND SCREEN                      *      ELTSAM  
00764 *                                                          *      ELTSAM  
00765 ************************************************************      ELTSAM  
00766  CONSTRUCT-BC-TEXT-AND-SCREEN.                                    ELTSAM  
00767      SET PAYMENT-LVL-NOT-TRANSLATED TO TRUE.                      ELTSAM  
00768      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTSAM  
00769      IF GSS-SA-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTSAM  
00770          ZERO                                                     ELTSAM  
00771                 AND SPACES AND LOW-VALUES                         ELTSAM  
00772          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTSAM  
00773      PERFORM TRANSLATE-BC-INDICATOR.                              ELTSAM  
00774      IF GSS-SA-BC-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTSAM  
00775          ZERO                                                     ELTSAM  
00776               AND SPACES AND LOW-VALUES                           ELTSAM  
00777          PERFORM TRANSLATE-BC-PAYMENT-LEVEL-IND.                  ELTSAM  
00778      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTSAM  
00779      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTSAM  
00780      IF (GSS-SA-CALCULATION-METHOD (GSS-INDEX) NOT EQUAL          ELTSAM  
00781          ZERO                                                     ELTSAM  
00782               AND SPACES AND LOW-VALUES)                          ELTSAM  
00783               AND PAYMENT-LVL-NOT-TRANSLATED                      ELTSAM  
00784          PERFORM TRANSLATE-CALC-METHOD.                           ELTSAM  
00785      PERFORM GENERATE-BC-BENEFITS-REDUCTION.                      ELTSAM  
00786      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTSAM  
00787               '03' OR '04' OR '06' OR '08')                       ELTSAM  
00788            AND                                                    ELTSAM  
00789             GSS-SA-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL           ELTSAM  
00790          ZERO                                                     ELTSAM  
00791                         AND SPACES AND LOW-VALUES                 ELTSAM  
00792          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTSAM  
00793      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTSAM  
00794      MOVE SPACES TO SCREEN-TYPE.                                  ELTSAM  
00795      EJECT                                                        ELTSAM  
00796                                                                   ELTSAM  
00797                                                                   ELTSAM  
00798 ************************************************************      ELTSAM  
00799 *                                                          *      ELTSAM  
00800 *        CONSTRUCT BS TEXT AND SCREEN                      *      ELTSAM  
00801 *                                                          *      ELTSAM  
00802 ************************************************************      ELTSAM  
00803  CONSTRUCT-BS-TEXT-AND-SCREEN.                                    ELTSAM  
00804      SET PAYMENT-LVL-NOT-TRANSLATED TO TRUE.                      ELTSAM  
00805      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTSAM  
00806      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTSAM  
00807      IF GSS-SA-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTSAM  
00808          ZERO                                                     ELTSAM  
00809                 AND SPACES AND LOW-VALUES                         ELTSAM  
00810          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTSAM  
00811      PERFORM TRANSLATE-BS-INDICATOR.                              ELTSAM  
00812      IF GSS-SA-BS-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTSAM  
00813          ZERO                                                     ELTSAM  
00814               AND SPACES AND LOW-VALUES                           ELTSAM  
00815          PERFORM TRANSLATE-BS-PAYMENT-LEVEL-IND.                  ELTSAM  
00816      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTSAM  
00817      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTSAM  
00818      IF (GSS-SA-CALCULATION-METHOD (GSS-INDEX) NOT EQUAL          ELTSAM  
00819          ZERO                                                     ELTSAM  
00820               AND SPACES AND LOW-VALUES)                          ELTSAM  
00821               AND PAYMENT-LVL-NOT-TRANSLATED                      ELTSAM  
00822          PERFORM TRANSLATE-CALC-METHOD.                           ELTSAM  
00823      PERFORM GENERATE-BS-BENEFITS-REDUCTION.                      ELTSAM  
00824      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTSAM  
00825                   '03' OR '04' OR '06' OR '08')                   ELTSAM  
00826            AND                                                    ELTSAM  
00827             GSS-SA-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL           ELTSAM  
00828          ZERO                                                     ELTSAM  
00829                         AND SPACES AND LOW-VALUES                 ELTSAM  
00830          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTSAM  
00831      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTSAM  
00832      MOVE SPACES TO SCREEN-TYPE.                                  ELTSAM  
00833      EJECT                                                        ELTSAM  
00834                                                                   ELTSAM  
00835                                                                   ELTSAM  
00836 ************************************************************      ELTSAM  
00837 *                                                          *      ELTSAM  
00838 *        CONSTRUCT MM TEXT AND SCREEN                      *      ELTSAM  
00839 *                                                          *      ELTSAM  
00840 ************************************************************      ELTSAM  
00841  CONSTRUCT-MM-TEXT-AND-SCREEN.                                    ELTSAM  
00842      SET PAYMENT-LVL-NOT-TRANSLATED TO TRUE.                      ELTSAM  
00843      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTSAM  
00844      IF GSS-SA-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTSAM  
00845          ZERO                                                     ELTSAM  
00846                 AND SPACES AND LOW-VALUES                         ELTSAM  
00847          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTSAM  
00848      PERFORM TRANSLATE-MM-INDICATOR.                              ELTSAM  
00849      IF GSS-SA-MM-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTSAM  
00850          ZERO                                                     ELTSAM  
00851             AND SPACES AND LOW-VALUES                             ELTSAM  
00852          PERFORM TRANSLATE-MM-PAYMENT-LEVEL-IND.                  ELTSAM  
00853      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTSAM  
00854      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTSAM  
00855      IF (GSS-SA-CALCULATION-METHOD (GSS-INDEX) NOT EQUAL          ELTSAM  
00856          ZERO                                                     ELTSAM  
00857               AND SPACES AND LOW-VALUES)                          ELTSAM  
00858               AND PAYMENT-LVL-NOT-TRANSLATED                      ELTSAM  
00859          PERFORM TRANSLATE-CALC-METHOD.                           ELTSAM  
00860      PERFORM GENERATE-MM-BENEFITS-REDUCTION.                      ELTSAM  
00861      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTSAM  
00862      EJECT                                                        ELTSAM  
00863                                                                   ELTSAM  
00864                                                                   ELTSAM  
00865 ************************************************************      ELTSAM  
00866 *                                                          *      ELTSAM  
00867 *        TRANSLATE PARTICIPATION IND                       *      ELTSAM  
00868 *                                                          *      ELTSAM  
00869 ************************************************************      ELTSAM  
00870  TRANSLATE-PARTICIPATION-IND.                                     ELTSAM  
00871      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
00872      SET BLANK-LINE-NEEDED TO TRUE.                               ELTSAM  
00873      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
00874      MOVE WS-PART-IND TO TCAR-FROM-LINE                           ELTSAM  
00875          (TCAR-FROM-SUB).                                         ELTSAM  
00876      ADD +1 TO TCAR-FROM-SUB.                                     ELTSAM  
00877      MOVE PC-GROUP TO CMF-RECORD-PREFIX.                          ELTSAM  
00878      MOVE 'SUBS-ABUSE-MENTAL-IND'  TO CMF-ELEMENT-SYSTEM-NAME.    ELTSAM  
00879      MOVE GCG-SUBS-ABUSE-MENTAL-IND TO CMF-CODE-VALUE.            ELTSAM  
00880      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
00881      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
00882      EJECT                                                        ELTSAM  
00883                                                                   ELTSAM  
00884                                                                   ELTSAM  
00885 ************************************************************      ELTSAM  
00886 *                                                          *      ELTSAM  
00887 *        GENERATE ASSOCIATED ACCUMULATORS                  *      ELTSAM  
00888 *                                                          *      ELTSAM  
00889 ************************************************************      ELTSAM  
00890  GENERATE-ASSOCIATED-ACCUMULATO.                                  ELTSAM  
00891      PERFORM GENERATE-COINSURANCE-TEXT.                           ELTSAM  
00892      PERFORM GENERATE-COPAY-TEXT.                                 ELTSAM  
00893      PERFORM GENERATE-DEDUCTIBLE-TEXT.                            ELTSAM  
00894      PERFORM GENERATE-BENEFIT-MAXIMUMS-TEXT.                      ELTSAM  
00895      EJECT                                                        ELTSAM  
00896                                                                   ELTSAM  
00897                                                                   ELTSAM  
00898 ************************************************************      ELTSAM  
00899 *                                                          *      ELTSAM  
00900 *        GENERATE DISCLAIMER SENTENCE                      *      ELTSAM  
00901 *                                                          *      ELTSAM  
00902 ************************************************************      ELTSAM  
00903  GENERATE-DISCLAIMER-SENTENCE.                                    ELTSAM  
00904      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTSAM  
00905      MOVE WS-DISCLAIMER TO COF-DTL-LINE (COF-NBR-DTL-LINES).      ELTSAM  
00906      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
00907      EJECT                                                        ELTSAM  
00908                                                                   ELTSAM  
00909                                                                   ELTSAM  
00910 ************************************************************      ELTSAM  
00911 *                                                          *      ELTSAM  
00912 *        TRANSLATE APPROVAL SOURCE                         *      ELTSAM  
00913 *                                                          *      ELTSAM  
00914 ************************************************************      ELTSAM  
00915  TRANSLATE-APPROVAL-SOURCE.                                       ELTSAM  
00916      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
00917      SET BLANK-LINE-NEEDED TO TRUE.                               ELTSAM  
00918      SET PERIOD-NEEDED TO TRUE.                                   ELTSAM  
00919      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
00920      MOVE WS-APPROVAL-SOURCE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTSAM  
00921      ADD 1 TO TCAR-FROM-SUB.                                      ELTSAM  
00922      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
00923      MOVE 'SA-APPROVAL-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTSAM  
00924      MOVE GSS-SA-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELTSAM  
00925          CMF-CODE-VALUE.                                          ELTSAM  
00926      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
00927      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
00928                                                                   ELTSAM  
00929                                                                   ELTSAM  
00930 ************************************************************      ELTSAM  
00931 *                                                          *      ELTSAM  
00932 *        GET INDICATOR FIXED TEXT                          *      ELTSAM  
00933 *                                                          *      ELTSAM  
00934 ************************************************************      ELTSAM  
00935  GET-INDICATOR-FIXED-TEXT.                                        ELTSAM  
00936      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
00937      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
00938      MOVE WS-INDICATOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELTSAM  
00939      ADD 1 TO TCAR-FROM-SUB.                                      ELTSAM  
00940      EJECT                                                        ELTSAM  
00941                                                                   ELTSAM  
00942                                                                   ELTSAM  
00943 ************************************************************      ELTSAM  
00944 *                                                          *      ELTSAM  
00945 *        TRANSLATE BC INDICATOR                            *      ELTSAM  
00946 *                                                          *      ELTSAM  
00947 ************************************************************      ELTSAM  
00948  TRANSLATE-BC-INDICATOR.                                          ELTSAM  
00949      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTSAM  
00950      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
00951      MOVE 'SA-BC-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTSAM  
00952      MOVE GSS-SA-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTSAM  
00953      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
00954      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
00955      EJECT                                                        ELTSAM  
00956                                                                   ELTSAM  
00957                                                                   ELTSAM  
00958 ************************************************************      ELTSAM  
00959 *                                                          *      ELTSAM  
00960 *        TRANSLATE BC PAYMENT LEVEL INDICATOR              *      ELTSAM  
00961 *                                                          *      ELTSAM  
00962 ************************************************************      ELTSAM  
00963  TRANSLATE-BC-PAYMENT-LEVEL-IND.                                  ELTSAM  
00964      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTSAM  
00965      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
00966      MOVE 'SA-BC-PAYMENT-LEVEL-IND'  TO CMF-ELEMENT-SYSTEM-NAME.  ELTSAM  
00967      MOVE GSS-SA-BC-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTSAM  
00968          CMF-CODE-VALUE.                                          ELTSAM  
00969      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
00970      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTSAM  
00971      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTSAM  
00972                                                                   ELTSAM  
00973                                                                   ELTSAM  
00974 ************************************************************      ELTSAM  
00975 *                                                          *      ELTSAM  
00976 *        MOVE TRANSLATED TEXT TO OUTPUT                    *      ELTSAM  
00977 *                                                          *      ELTSAM  
00978 ************************************************************      ELTSAM  
00979  MOVE-TRANSLATED-TEXT-TO-OUTPUT.                                  ELTSAM  
00980      PERFORM DO-MOVE-OF-TEXT                                      ELTSAM  
00981          VARYING WS-CMF-SUB FROM 1 BY 1 UNTIL WS-CMF-SUB          ELTSAM  
00982                    > CMF-NBR-DESCR-LINES.                         ELTSAM  
00983      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
00984      EJECT                                                        ELTSAM  
00985                                                                   ELTSAM  
00986                                                                   ELTSAM  
00987 ************************************************************      ELTSAM  
00988 *                                                          *      ELTSAM  
00989 *        DO MOVE OF TEXT                                   *      ELTSAM  
00990 *                                                          *      ELTSAM  
00991 ************************************************************      ELTSAM  
00992  DO-MOVE-OF-TEXT.                                                 ELTSAM  
00993      SET CMF-DESCR-IDX TO WS-CMF-SUB.                             ELTSAM  
00994      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTSAM  
00995          COF-DTL-LINE (COF-NBR-DTL-LINES).                        ELTSAM  
00996      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTSAM  
00997                                                                   ELTSAM  
00998                                                                   ELTSAM  
00999 ************************************************************      ELTSAM  
01000 *                                                          *      ELTSAM  
01001 *        SETUP PAYMENT LEVEL FIXED TEXT                    *      ELTSAM  
01002 *                                                          *      ELTSAM  
01003 ************************************************************      ELTSAM  
01004  SETUP-PAYMENT-LEVEL-FIXED-TEXT.                                  ELTSAM  
01005      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTSAM  
01006      STRING WS-PAYMENT-LEVEL                                      ELTSAM  
01007          DELIMITED BY SIZE INTO COF-DTL-LINE                      ELTSAM  
01008          (COF-NBR-DTL-LINES).                                     ELTSAM  
01009      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTSAM  
01010                                                                   ELTSAM  
01011                                                                   ELTSAM  
01012 ************************************************************      ELTSAM  
01013 *                                                          *      ELTSAM  
01014 *        GENERATE COINSURANCE TEXT                         *      ELTSAM  
01015 *                                                          *      ELTSAM  
01016 ************************************************************      ELTSAM  
01017  GENERATE-COINSURANCE-TEXT.                                       ELTSAM  
01018      EXEC CICS LINK                                               ELTSAM  
01019          PROGRAM ('ELGACLCC')                                     ELTSAM  
01020          COMMAREA (DFHCOMMAREA)                                   ELTSAM  
01021          END-EXEC.                                                ELTSAM  
01022                                                                   ELTSAM  
01023                                                                   ELTSAM  
01024 ************************************************************      ELTSAM  
01025 *                                                          *      ELTSAM  
01026 *        GENERATE COPAY      TEXT                          *      ELTSAM  
01027 *                                                          *      ELTSAM  
01028 ************************************************************      ELTSAM  
01029  GENERATE-COPAY-TEXT.                                             ELTSAM  
01030      EXEC CICS LINK                                               ELTSAM  
01031          PROGRAM ('ELGACPCC')                                     ELTSAM  
01032          COMMAREA (DFHCOMMAREA)                                   ELTSAM  
01033          END-EXEC.                                                ELTSAM  
01034                                                                   ELTSAM  
01035 ************************************************************      ELTSAM  
01036 *                                                          *      ELTSAM  
01037 *        GENERATE DEDUCTIBLE TEXT                          *      ELTSAM  
01038 *                                                          *      ELTSAM  
01039 ************************************************************      ELTSAM  
01040  GENERATE-DEDUCTIBLE-TEXT.                                        ELTSAM  
01041      EXEC CICS LINK                                               ELTSAM  
01042          PROGRAM ('ELGADLCC')                                     ELTSAM  
01043          COMMAREA (DFHCOMMAREA)                                   ELTSAM  
01044          END-EXEC.                                                ELTSAM  
01045                                                                   ELTSAM  
01046                                                                   ELTSAM  
01047 ************************************************************      ELTSAM  
01048 *                                                          *      ELTSAM  
01049 *        GENERATE BENEFIT MAXIMUMS TEXT                    *      ELTSAM  
01050 *                                                          *      ELTSAM  
01051 ************************************************************      ELTSAM  
01052  GENERATE-BENEFIT-MAXIMUMS-TEXT.                                  ELTSAM  
01053      EXEC CICS LINK                                               ELTSAM  
01054          PROGRAM ('ELGABMCC')                                     ELTSAM  
01055          COMMAREA (DFHCOMMAREA)                                   ELTSAM  
01056          END-EXEC.                                                ELTSAM  
01057      EJECT                                                        ELTSAM  
01058                                                                   ELTSAM  
01059                                                                   ELTSAM  
01060 ************************************************************      ELTSAM  
01061 *                                                          *      ELTSAM  
01062 *        TRANSLATE CALC METHOD                             *      ELTSAM  
01063 *                                                          *      ELTSAM  
01064 ************************************************************      ELTSAM  
01065  TRANSLATE-CALC-METHOD.                                           ELTSAM  
01066      PERFORM GET-CALC-METHOD-FIXED-TEXT.                          ELTSAM  
01067      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01068      MOVE 'SA-CALCULATION-METHOD'  TO                             ELTSAM  
01069          CMF-ELEMENT-SYSTEM-NAME.                                 ELTSAM  
01070      MOVE GSS-SA-CALCULATION-METHOD (GSS-INDEX) TO                ELTSAM  
01071          CMF-CODE-VALUE.                                          ELTSAM  
01072      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01073      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTSAM  
01074                                                                   ELTSAM  
01075                                                                   ELTSAM  
01076 ************************************************************      ELTSAM  
01077 *                                                          *      ELTSAM  
01078 *        GET CALC METHOD FIXED TEXT                        *      ELTSAM  
01079 *                                                          *      ELTSAM  
01080 ************************************************************      ELTSAM  
01081  GET-CALC-METHOD-FIXED-TEXT.                                      ELTSAM  
01082      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTSAM  
01083      STRING WS-CALC-METHOD                                        ELTSAM  
01084          DELIMITED BY SIZE INTO COF-DTL-LINE                      ELTSAM  
01085          (COF-NBR-DTL-LINES).                                     ELTSAM  
01086      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTSAM  
01087                                                                   ELTSAM  
01088                                                                   ELTSAM  
01089 ************************************************************      ELTSAM  
01090 *                                                          *      ELTSAM  
01091 *        GENERATE COMBINED BENEFITS REDUCTION SENTENCE     *      ELTSAM  
01092 *                                                          *      ELTSAM  
01093 ************************************************************      ELTSAM  
01094  GENERATE-COMBINED-BENEFITS-RED.                                  ELTSAM  
01095      MOVE 'SA' TO SRP-COST-CONT-TYPE.                             ELTSAM  
01096      MOVE 'EXTENDED MENTAL HEALTH PROGRAM' TO SRP-CCP-NAME.       ELTSAM  
01097      MOVE GSS-SA-COMB-BENE-REDUCT-IND (GSS-INDEX) TO              ELTSAM  
01098           SRP-CCP-COMB-BENE-REDUCT-IND.                           ELTSAM  
01099      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTSAM  
01100      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTSAM  
01101          ADDRESS OF GCCP-TABULAR-REC.                             ELTSAM  
01102      EXEC CICS LINK                                               ELTSAM  
01103          PROGRAM ('ELGCBRI')                                      ELTSAM  
01104          COMMAREA (DFHCOMMAREA)                                   ELTSAM  
01105          END-EXEC.                                                ELTSAM  
01106      EJECT                                                        ELTSAM  
01107                                                                   ELTSAM  
01108                                                                   ELTSAM  
01109 ************************************************************      ELTSAM  
01110 *                                                          *      ELTSAM  
01111 *        GENERATE BC BENEFITS REDUCTION SENTENCE           *      ELTSAM  
01112 *                                                          *      ELTSAM  
01113 ************************************************************      ELTSAM  
01114  GENERATE-BC-BENEFITS-REDUCTION.                                  ELTSAM  
01115      IF GSS-SA-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTSAM  
01116                  ZERO AND SPACES AND LOW-VALUES                   ELTSAM  
01117          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTSAM  
01118      IF (GSS-SA-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTSAM  
01119          ZERO                                                     ELTSAM  
01120                             AND SPACES AND LOW-VALUES)  OR        ELTSAM  
01121                (GSS-SA-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL   ELTSAM  
01122          ZERO                                                     ELTSAM  
01123                             AND SPACES AND LOW-VALUES)  OR        ELTSAM  
01124                (GSS-SA-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT       ELTSAM  
01125          EQUAL ZERO                                               ELTSAM  
01126                             AND SPACES AND LOW-VALUES)            ELTSAM  
01127          PERFORM GENERATE-BC-BENEFIT-REDUCTIONX.                  ELTSAM  
01128      EJECT                                                        ELTSAM  
01129                                                                   ELTSAM  
01130                                                                   ELTSAM  
01131 ************************************************************      ELTSAM  
01132 *                                                          *      ELTSAM  
01133 *        GENERATE BC BENEFIT REDUCTION TEXT                *      ELTSAM  
01134 *                                                          *      ELTSAM  
01135 ************************************************************      ELTSAM  
01136  GENERATE-BC-BENEFIT-REDUCTIONX.                                  ELTSAM  
01137      PERFORM GENERATE-REDUCTIONS-HEADINGS.                        ELTSAM  
01138      IF GSS-SA-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTSAM  
01139          ZERO                                                     ELTSAM  
01140                             AND SPACES AND LOW-VALUES             ELTSAM  
01141          PERFORM TRANSLATE-BC-DEDU-APPLIC.                        ELTSAM  
01142      IF GSS-SA-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTSAM  
01143          ZERO                                                     ELTSAM  
01144                             AND SPACES AND LOW-VALUES             ELTSAM  
01145          PERFORM TRANSLATE-BC-OPEX-APPLIC.                        ELTSAM  
01146      PERFORM CREATE-A-BLANK-LINE.                                 ELTSAM  
01147      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
01148      IF GSS-SA-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTSAM  
01149          ZERO                                                     ELTSAM  
01150                             AND SPACES AND LOW-VALUES             ELTSAM  
01151          PERFORM TRANSLATE-BC-OPEX-OVERRIDE.                      ELTSAM  
01152      EJECT                                                        ELTSAM  
01153                                                                   ELTSAM  
01154                                                                   ELTSAM  
01155 ************************************************************      ELTSAM  
01156 *                                                          *      ELTSAM  
01157 *        GENERATE REDUCTIONS HEADINGS                      *      ELTSAM  
01158 *                                                          *      ELTSAM  
01159 ************************************************************      ELTSAM  
01160  GENERATE-REDUCTIONS-HEADINGS.                                    ELTSAM  
01161      INITIALIZE WS-PERIOD-SW.                                     ELTSAM  
01162      INITIALIZE ADDITIONAL-TEXT-SW.                               ELTSAM  
01163      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTSAM  
01164      MOVE WS-BENEFITS-REDUCTION TO COF-DTL-LINE                   ELTSAM  
01165          (COF-NBR-DTL-LINES).                                     ELTSAM  
01166      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
01167      EJECT                                                        ELTSAM  
01168                                                                   ELTSAM  
01169                                                                   ELTSAM  
01170 ************************************************************      ELTSAM  
01171 *                                                          *      ELTSAM  
01172 *        TRANSLATE BS INDICATOR                            *      ELTSAM  
01173 *                                                          *      ELTSAM  
01174 ************************************************************      ELTSAM  
01175  TRANSLATE-BS-INDICATOR.                                          ELTSAM  
01176      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTSAM  
01177      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01178      MOVE 'SA-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTSAM  
01179      MOVE GSS-SA-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTSAM  
01180      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01181      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
01182      EJECT                                                        ELTSAM  
01183                                                                   ELTSAM  
01184                                                                   ELTSAM  
01185 ************************************************************      ELTSAM  
01186 *                                                          *      ELTSAM  
01187 *        TRANSLATE BS PAYMENT LEVEL INDICATOR              *      ELTSAM  
01188 *                                                          *      ELTSAM  
01189 ************************************************************      ELTSAM  
01190  TRANSLATE-BS-PAYMENT-LEVEL-IND.                                  ELTSAM  
01191      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTSAM  
01192      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01193      MOVE 'SA-BS-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTSAM  
01194      MOVE GSS-SA-BS-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTSAM  
01195          CMF-CODE-VALUE.                                          ELTSAM  
01196      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01197      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTSAM  
01198      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTSAM  
01199      EJECT                                                        ELTSAM  
01200                                                                   ELTSAM  
01201                                                                   ELTSAM  
01202 ************************************************************      ELTSAM  
01203 *                                                          *      ELTSAM  
01204 *        GENERATE BS BENEFITS REDUCTION SENTENCE           *      ELTSAM  
01205 *                                                          *      ELTSAM  
01206 ************************************************************      ELTSAM  
01207  GENERATE-BS-BENEFITS-REDUCTION.                                  ELTSAM  
01208      IF GSS-SA-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTSAM  
01209                  ZERO AND SPACES AND LOW-VALUES                   ELTSAM  
01210          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTSAM  
01211      IF (GSS-SA-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTSAM  
01212          ZERO                                                     ELTSAM  
01213                             AND SPACES AND LOW-VALUES)  OR        ELTSAM  
01214               (GSS-SA-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL    ELTSAM  
01215          ZERO                                                     ELTSAM  
01216                             AND SPACES AND LOW-VALUES)  OR        ELTSAM  
01217                (GSS-SA-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT       ELTSAM  
01218          EQUAL ZERO                                               ELTSAM  
01219                             AND SPACES AND LOW-VALUES)            ELTSAM  
01220          PERFORM GENERATE-BS-BENEFIT-REDUCTIONX.                  ELTSAM  
01221      EJECT                                                        ELTSAM  
01222                                                                   ELTSAM  
01223                                                                   ELTSAM  
01224 ************************************************************      ELTSAM  
01225 *                                                          *      ELTSAM  
01226 *        GENERATE BS BENEFIT REDUCTION TEXT                *      ELTSAM  
01227 *                                                          *      ELTSAM  
01228 ************************************************************      ELTSAM  
01229  GENERATE-BS-BENEFIT-REDUCTIONX.                                  ELTSAM  
01230      PERFORM GENERATE-REDUCTIONS-HEADINGS.                        ELTSAM  
01231      IF GSS-SA-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTSAM  
01232          ZERO                                                     ELTSAM  
01233                             AND SPACES AND LOW-VALUES             ELTSAM  
01234          PERFORM TRANSLATE-BS-DEDU-APPLIC.                        ELTSAM  
01235      IF GSS-SA-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTSAM  
01236          ZERO                                                     ELTSAM  
01237                             AND SPACES AND LOW-VALUES             ELTSAM  
01238          PERFORM TRANSLATE-BS-OPEX-APPLIC.                        ELTSAM  
01239      PERFORM CREATE-A-BLANK-LINE.                                 ELTSAM  
01240      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
01241      IF GSS-SA-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTSAM  
01242          ZERO                                                     ELTSAM  
01243                             AND SPACES AND LOW-VALUES             ELTSAM  
01244          PERFORM TRANSLATE-BS-OPEX-OVERRIDE.                      ELTSAM  
01245      EJECT                                                        ELTSAM  
01246                                                                   ELTSAM  
01247                                                                   ELTSAM  
01248 ************************************************************      ELTSAM  
01249 *                                                          *      ELTSAM  
01250 *        TRANSLATE MM INDICATOR                            *      ELTSAM  
01251 *                                                          *      ELTSAM  
01252 ************************************************************      ELTSAM  
01253  TRANSLATE-MM-INDICATOR.                                          ELTSAM  
01254      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTSAM  
01255      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01256      MOVE 'SA-MM-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTSAM  
01257      MOVE GSS-SA-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTSAM  
01258      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01259      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
01260      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
01261      EJECT                                                        ELTSAM  
01262                                                                   ELTSAM  
01263                                                                   ELTSAM  
01264 ************************************************************      ELTSAM  
01265 *                                                          *      ELTSAM  
01266 *        TRANSLATE MM PAYMENT LEVEL INDICATOR              *      ELTSAM  
01267 *                                                          *      ELTSAM  
01268 ************************************************************      ELTSAM  
01269  TRANSLATE-MM-PAYMENT-LEVEL-IND.                                  ELTSAM  
01270      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTSAM  
01271      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01272      MOVE 'SA-MM-PAYMENT-LEVEL-IND' TO                            ELTSAM  
01273          CMF-ELEMENT-SYSTEM-NAME.                                 ELTSAM  
01274      MOVE GSS-SA-MM-PAYMENT-LEVEL-IND (GSS-INDEX)                 ELTSAM  
01275                                   TO CMF-CODE-VALUE.              ELTSAM  
01276      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01277      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTSAM  
01278      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTSAM  
01279      EJECT                                                        ELTSAM  
01280                                                                   ELTSAM  
01281                                                                   ELTSAM  
01282 ************************************************************      ELTSAM  
01283 *                                                          *      ELTSAM  
01284 *        GENERATE MM BENEFITS REDUCTION SENTENCE           *      ELTSAM  
01285 *                                                          *      ELTSAM  
01286 ************************************************************      ELTSAM  
01287  GENERATE-MM-BENEFITS-REDUCTION.                                  ELTSAM  
01288      IF GSS-SA-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTSAM  
01289                  ZERO AND SPACES AND LOW-VALUES                   ELTSAM  
01290          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTSAM  
01291      IF (GSS-SA-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTSAM  
01292          ZERO                                                     ELTSAM  
01293                             AND SPACES AND LOW-VALUES)  OR        ELTSAM  
01294                (GSS-SA-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL   ELTSAM  
01295          ZERO                                                     ELTSAM  
01296                             AND SPACES AND LOW-VALUES)  OR        ELTSAM  
01297                (GSS-SA-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT       ELTSAM  
01298          EQUAL ZERO                                               ELTSAM  
01299                             AND SPACES AND LOW-VALUES)            ELTSAM  
01300          PERFORM GENERATE-MM-BENEFIT-REDUCTIONX.                  ELTSAM  
01301      EJECT                                                        ELTSAM  
01302                                                                   ELTSAM  
01303                                                                   ELTSAM  
01304 ************************************************************      ELTSAM  
01305 *                                                          *      ELTSAM  
01306 *        GENERATE MM BENEFIT REDUCTION TEXT                *      ELTSAM  
01307 *                                                          *      ELTSAM  
01308 ************************************************************      ELTSAM  
01309  GENERATE-MM-BENEFIT-REDUCTIONX.                                  ELTSAM  
01310      PERFORM GENERATE-REDUCTIONS-HEADINGS.                        ELTSAM  
01311      IF GSS-SA-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTSAM  
01312          ZERO                                                     ELTSAM  
01313                             AND SPACES AND LOW-VALUES             ELTSAM  
01314          PERFORM TRANSLATE-MM-DEDU-APPLIC.                        ELTSAM  
01315      IF GSS-SA-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTSAM  
01316          ZERO                                                     ELTSAM  
01317                             AND SPACES AND LOW-VALUES             ELTSAM  
01318          PERFORM TRANSLATE-MM-OPEX-APPLIC.                        ELTSAM  
01319      PERFORM CREATE-A-BLANK-LINE.                                 ELTSAM  
01320      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
01321      IF GSS-SA-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTSAM  
01322          ZERO                                                     ELTSAM  
01323                             AND SPACES AND LOW-VALUES             ELTSAM  
01324          PERFORM TRANSLATE-MM-OPEX-OVERRIDE.                      ELTSAM  
01325      EJECT                                                        ELTSAM  
01326                                                                   ELTSAM  
01327                                                                   ELTSAM  
01328 ************************************************************      ELTSAM  
01329 *                                                          *      ELTSAM  
01330 *        TRANSLATE BC DEDU APPLIC                          *      ELTSAM  
01331 *                                                          *      ELTSAM  
01332 ************************************************************      ELTSAM  
01333  TRANSLATE-BC-DEDU-APPLIC.                                        ELTSAM  
01334      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
01335      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
01336      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01337      MOVE  'SA-BC-DEDU-APPLIC-IND' TO                             ELTSAM  
01338          CMF-ELEMENT-SYSTEM-NAME.                                 ELTSAM  
01339      MOVE GSS-SA-BC-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTSAM  
01340          CMF-CODE-VALUE.                                          ELTSAM  
01341      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01342      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
01343      EJECT                                                        ELTSAM  
01344                                                                   ELTSAM  
01345                                                                   ELTSAM  
01346 ************************************************************      ELTSAM  
01347 *                                                          *      ELTSAM  
01348 *        TRANSLATE BC OPEX APPLIC                          *      ELTSAM  
01349 *                                                          *      ELTSAM  
01350 ************************************************************      ELTSAM  
01351  TRANSLATE-BC-OPEX-APPLIC.                                        ELTSAM  
01352      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
01353      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
01354      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01355      MOVE 'SA-BC-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTSAM  
01356      MOVE GSS-SA-BC-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTSAM  
01357          CMF-CODE-VALUE.                                          ELTSAM  
01358      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01359      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
01360      EJECT                                                        ELTSAM  
01361                                                                   ELTSAM  
01362                                                                   ELTSAM  
01363 ************************************************************      ELTSAM  
01364 *                                                          *      ELTSAM  
01365 *        TRANSLATE BC OPEX OVERRIDE                        *      ELTSAM  
01366 *                                                          *      ELTSAM  
01367 ************************************************************      ELTSAM  
01368  TRANSLATE-BC-OPEX-OVERRIDE.                                      ELTSAM  
01369      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
01370      SET BLANK-LINE-NEEDED TO TRUE.                               ELTSAM  
01371      SET PERIOD-NEEDED TO TRUE.                                   ELTSAM  
01372      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
01373      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01374      MOVE 'SA-BC-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTSAM  
01375      MOVE GSS-SA-BC-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTSAM  
01376          CMF-CODE-VALUE.                                          ELTSAM  
01377      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01378      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
01379      EJECT                                                        ELTSAM  
01380                                                                   ELTSAM  
01381                                                                   ELTSAM  
01382 ************************************************************      ELTSAM  
01383 *                                                          *      ELTSAM  
01384 *        TRANSLATE BS DEDU APPLIC                          *      ELTSAM  
01385 *                                                          *      ELTSAM  
01386 ************************************************************      ELTSAM  
01387  TRANSLATE-BS-DEDU-APPLIC.                                        ELTSAM  
01388      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
01389      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
01390      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01391      MOVE 'SA-BS-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTSAM  
01392      MOVE GSS-SA-BS-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTSAM  
01393          CMF-CODE-VALUE.                                          ELTSAM  
01394      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01395      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
01396      EJECT                                                        ELTSAM  
01397                                                                   ELTSAM  
01398                                                                   ELTSAM  
01399 ************************************************************      ELTSAM  
01400 *                                                          *      ELTSAM  
01401 *        TRANSLATE BS OPEX APPLIC                          *      ELTSAM  
01402 *                                                          *      ELTSAM  
01403 ************************************************************      ELTSAM  
01404  TRANSLATE-BS-OPEX-APPLIC.                                        ELTSAM  
01405      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
01406      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
01407      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01408      MOVE 'SA-BS-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTSAM  
01409      MOVE GSS-SA-BS-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTSAM  
01410          CMF-CODE-VALUE.                                          ELTSAM  
01411      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01412      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
01413      EJECT                                                        ELTSAM  
01414                                                                   ELTSAM  
01415                                                                   ELTSAM  
01416 ************************************************************      ELTSAM  
01417 *                                                          *      ELTSAM  
01418 *        TRANSLATE BS OPEX OVERRIDE                        *      ELTSAM  
01419 *                                                          *      ELTSAM  
01420 ************************************************************      ELTSAM  
01421  TRANSLATE-BS-OPEX-OVERRIDE.                                      ELTSAM  
01422      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
01423      SET BLANK-LINE-NEEDED TO TRUE.                               ELTSAM  
01424      SET PERIOD-NEEDED TO TRUE.                                   ELTSAM  
01425      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
01426      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01427      MOVE 'SA-BS-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTSAM  
01428      MOVE GSS-SA-BS-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTSAM  
01429          CMF-CODE-VALUE.                                          ELTSAM  
01430      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01431      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
01432      EJECT                                                        ELTSAM  
01433                                                                   ELTSAM  
01434                                                                   ELTSAM  
01435 ************************************************************      ELTSAM  
01436 *                                                          *      ELTSAM  
01437 *        TRANSLATE MM DEDU APPLIC                          *      ELTSAM  
01438 *                                                          *      ELTSAM  
01439 ************************************************************      ELTSAM  
01440  TRANSLATE-MM-DEDU-APPLIC.                                        ELTSAM  
01441      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
01442      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
01443      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01444      MOVE 'SA-MM-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTSAM  
01445      MOVE GSS-SA-MM-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTSAM  
01446          CMF-CODE-VALUE.                                          ELTSAM  
01447      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01448      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
01449      EJECT                                                        ELTSAM  
01450                                                                   ELTSAM  
01451                                                                   ELTSAM  
01452 ************************************************************      ELTSAM  
01453 *                                                          *      ELTSAM  
01454 *        TRANSLATE MM OPEX APPLIC                          *      ELTSAM  
01455 *                                                          *      ELTSAM  
01456 ************************************************************      ELTSAM  
01457  TRANSLATE-MM-OPEX-APPLIC.                                        ELTSAM  
01458      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
01459      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
01460      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01461      MOVE 'SA-MM-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTSAM  
01462      MOVE GSS-SA-MM-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTSAM  
01463          CMF-CODE-VALUE.                                          ELTSAM  
01464      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01465      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
01466      EJECT                                                        ELTSAM  
01467                                                                   ELTSAM  
01468                                                                   ELTSAM  
01469 ************************************************************      ELTSAM  
01470 *                                                          *      ELTSAM  
01471 *        TRANSLATE MM OPEX OVERRIDE                        *      ELTSAM  
01472 *                                                          *      ELTSAM  
01473 ************************************************************      ELTSAM  
01474  TRANSLATE-MM-OPEX-OVERRIDE.                                      ELTSAM  
01475      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
01476      SET BLANK-LINE-NEEDED TO TRUE.                               ELTSAM  
01477      SET PERIOD-NEEDED TO TRUE.                                   ELTSAM  
01478      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
01479      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01480      MOVE 'SA-MM-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTSAM  
01481      MOVE GSS-SA-MM-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTSAM  
01482          CMF-CODE-VALUE.                                          ELTSAM  
01483      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01484      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
01485      EJECT                                                        ELTSAM  
01486                                                                   ELTSAM  
01487                                                                   ELTSAM  
01488 ************************************************************      ELTSAM  
01489 *                                                          *      ELTSAM  
01490 *        GENERATE SPILL OVER INDICATOR                     *      ELTSAM  
01491 *                                                          *      ELTSAM  
01492 ************************************************************      ELTSAM  
01493  GENERATE-SPILL-OVER-INDICATOR.                                   ELTSAM  
01494      INITIALIZE ADDITIONAL-TEXT-SW.                               ELTSAM  
01495      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSAM  
01496      SET BLANK-LINE-NEEDED TO TRUE.                               ELTSAM  
01497      SET PERIOD-NEEDED TO TRUE.                                   ELTSAM  
01498      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
01499      MOVE WS-SPILLOVER-SENTENCE TO TCAR-FROM-LINE                 ELTSAM  
01500          (TCAR-FROM-SUB).                                         ELTSAM  
01501      ADD 1 TO TCAR-FROM-SUB.                                      ELTSAM  
01502      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTSAM  
01503      MOVE 'SA-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTSAM  
01504      MOVE GSS-SA-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTSAM  
01505      PERFORM LINK-TO-TRANSLATOR.                                  ELTSAM  
01506      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTSAM  
01507                                                                   ELTSAM  
01508                                                                   ELTSAM  
01509 ************************************************************      ELTSAM  
01510 *                                                          *      ELTSAM  
01511 *        SIGNAL NOT APPLICABLE MSG                         *      ELTSAM  
01512 *                                                          *      ELTSAM  
01513 ************************************************************      ELTSAM  
01514  SIGNAL-NOT-APPLICABLE-MSG.                                       ELTSAM  
01515      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTSAM  
01516      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTSAM  
01517          (COF-NBR-DTL-LINES).                                     ELTSAM  
01518      EJECT                                                        ELTSAM  
01519                                                                   ELTSAM  
01520                                                                   ELTSAM  
01521 ************************************************************      ELTSAM  
01522 *                                                          *      ELTSAM  
01523 *        LINK TO TRANSLATOR                                *      ELTSAM  
01524 *                                                          *      ELTSAM  
01525 ************************************************************      ELTSAM  
01526  LINK-TO-TRANSLATOR.                                              ELTSAM  
01527      EXEC CICS LINK                                               ELTSAM  
01528          PROGRAM ('ELUCMIF')                                      ELTSAM  
01529          COMMAREA (DFHCOMMAREA)                                   ELTSAM  
01530          END-EXEC.                                                ELTSAM  
01531      EJECT                                                        ELTSAM  
01532                                                                   ELTSAM  
01533                                                                   ELTSAM  
01534 ************************************************************      ELTSAM  
01535 *                                                          *      ELTSAM  
01536 *        READ GCCP RECORD                                  *      ELTSAM  
01537 *                                                          *      ELTSAM  
01538 ************************************************************      ELTSAM  
01539  READ-GCCP-RECORD.                                                ELTSAM  
01540      SET CIA-GCTABULR-DDN TO TRUE.                                ELTSAM  
01541      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSAM  
01542          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTSAM  
01543      SET IOP-RD TO TRUE.                                          ELTSAM  
01544      SET IOP-FCQ-NONE TO TRUE.                                    ELTSAM  
01545      SET IOP-KVQ-EQ TO TRUE.                                      ELTSAM  
01546      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTSAM  
01547      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTSAM  
01548      PERFORM CHANGE-I-O-PGM.                                      ELTSAM  
01549      EJECT                                                        ELTSAM  
01550                                                                   ELTSAM  
01551                                                                   ELTSAM  
01552 ************************************************************      ELTSAM  
01553 *                                                          *      ELTSAM  
01554 *        CHANGE I O PGM                                    *      ELTSAM  
01555 *                                                          *      ELTSAM  
01556 ************************************************************      ELTSAM  
01557  CHANGE-I-O-PGM.                                                  ELTSAM  
01558      EXEC CICS LINK                                               ELTSAM  
01559          PROGRAM ('ELUIOPGM')                                     ELTSAM  
01560          COMMAREA (DFHCOMMAREA)                                   ELTSAM  
01561          END-EXEC.                                                ELTSAM  
01562      IF IOP-RC-OK                                                 ELTSAM  
01563          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTSAM  
01564      ELSE IF IOP-RC-NOTFND                                        ELTSAM  
01565          PERFORM SIGNAL-NOT-FOUND-GCTAB                           ELTSAM  
01566      ELSE                                                         ELTSAM  
01567          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTSAM  
01568      EJECT                                                        ELTSAM  
01569                                                                   ELTSAM  
01570                                                                   ELTSAM  
01571 ************************************************************      ELTSAM  
01572 *                                                          *      ELTSAM  
01573 *        SIGNAL CRITICAL IO ERROR                          *      ELTSAM  
01574 *                                                          *      ELTSAM  
01575 ************************************************************      ELTSAM  
01576  SIGNAL-CRITICAL-IO-ERROR.                                        ELTSAM  
01577      SET CIA-AB-CRITIO TO TRUE.                                   ELTSAM  
01578      PERFORM SIGNAL-ABEND.                                        ELTSAM  
01579                                                                   ELTSAM  
01580                                                                   ELTSAM  
01581 ************************************************************      ELTSAM  
01582 *                                                          *      ELTSAM  
01583 *        SIGNAL NOT FOUND GCTAB                            *      ELTSAM  
01584 *                                                          *      ELTSAM  
01585 ************************************************************      ELTSAM  
01586  SIGNAL-NOT-FOUND-GCTAB.                                          ELTSAM  
01587      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTSAM  
01588      PERFORM SIGNAL-ABEND.                                        ELTSAM  
01589                                                                   ELTSAM  
01590                                                                   ELTSAM  
01591 ************************************************************      ELTSAM  
01592 *                                                          *      ELTSAM  
01593 *        ESTABLISH ADDRESSABILITY OF GCCP RECORD           *      ELTSAM  
01594 *                                                          *      ELTSAM  
01595 ************************************************************      ELTSAM  
01596  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTSAM  
01597      SET ADDRESS OF GCCP-TABULAR-REC TO IOP-REC-PTR.              ELTSAM  
01598      SET IOP-REC-PTR TO NULL.                                     ELTSAM  
01599      EJECT                                                        ELTSAM  
01600                                                                   ELTSAM  
01601                                                                   ELTSAM  
01602 ************************************************************      ELTSAM  
01603 *                                                          *      ELTSAM  
01604 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTSAM  
01605 *                                                          *      ELTSAM  
01606 ************************************************************      ELTSAM  
01607  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTSAM  
01608      PERFORM INITIALIZE-CMOUT.                                    ELTSAM  
01609      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTSAM  
01610                                                                   ELTSAM  
01611                                                                   ELTSAM  
01612 ************************************************************      ELTSAM  
01613 *                                                          *      ELTSAM  
01614 *        PREPARE TEXT FOR OUTPUT                           *      ELTSAM  
01615 *                                                          *      ELTSAM  
01616 ************************************************************      ELTSAM  
01617  PREPARE-TEXT-FOR-OUTPUT.                                         ELTSAM  
01618      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTSAM  
01619          UNTIL CMF-DESCR-IDX                                      ELTSAM  
01620                                    GREATER THAN                   ELTSAM  
01621              CMF-NBR-DESCR-LINES.                                 ELTSAM  
01622                                                                   ELTSAM  
01623                                                                   ELTSAM  
01624 ************************************************************      ELTSAM  
01625 *                                                          *      ELTSAM  
01626 *        INITIALIZE CMOUT                                  *      ELTSAM  
01627 *                                                          *      ELTSAM  
01628 ************************************************************      ELTSAM  
01629  INITIALIZE-CMOUT.                                                ELTSAM  
01630      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTSAM  
01631      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSAM  
01632          ADDRESS OF CMF-DESCR.                                    ELTSAM  
01633      SET CMF-DESCR-IDX TO 1.                                      ELTSAM  
01634      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTSAM  
01635                                                                   ELTSAM  
01636                                                                   ELTSAM  
01637 ************************************************************      ELTSAM  
01638 *                                                          *      ELTSAM  
01639 *        MOVE CMF TEXT TO OUTPUT                           *      ELTSAM  
01640 *                                                          *      ELTSAM  
01641 ************************************************************      ELTSAM  
01642  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTSAM  
01643      PERFORM MOVE-A-LINE.                                         ELTSAM  
01644      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTSAM  
01645          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTSAM  
01646      IF TCAR-FROM-SUB GREATER THAN 20                             ELTSAM  
01647               OR CMF-DESCR-IDX GREATER THAN                       ELTSAM  
01648          CMF-NBR-DESCR-LINES                                      ELTSAM  
01649          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTSAM  
01650      EJECT                                                        ELTSAM  
01651                                                                   ELTSAM  
01652                                                                   ELTSAM  
01653 ************************************************************      ELTSAM  
01654 *                                                          *      ELTSAM  
01655 *        FINISH CODES MANUAL TEXT                          *      ELTSAM  
01656 *                                                          *      ELTSAM  
01657 ************************************************************      ELTSAM  
01658  FINISH-CODES-MANUAL-TEXT.                                        ELTSAM  
01659      SET DONE-PROCESSING TO TRUE.                                 ELTSAM  
01660      IF PERIOD-NEEDED                                             ELTSAM  
01661          PERFORM GET-AND-MOVE-PERIOD.                             ELTSAM  
01662                                                                   ELTSAM  
01663                                                                   ELTSAM  
01664 ************************************************************      ELTSAM  
01665 *                                                          *      ELTSAM  
01666 *        GET AND MOVE PERIOD                               *      ELTSAM  
01667 *                                                          *      ELTSAM  
01668 ************************************************************      ELTSAM  
01669  GET-AND-MOVE-PERIOD.                                             ELTSAM  
01670      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTSAM  
01671          (TCAR-FROM-SUB).                                         ELTSAM  
01672                                                                   ELTSAM  
01673                                                                   ELTSAM  
01674 ************************************************************      ELTSAM  
01675 *                                                          *      ELTSAM  
01676 *        SAVE LAST LINE                                    *      ELTSAM  
01677 *                                                          *      ELTSAM  
01678 ************************************************************      ELTSAM  
01679  SAVE-LAST-LINE.                                                  ELTSAM  
01680      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
01681      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTSAM  
01682         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTSAM  
01683      ADD 1 TO TCAR-FROM-SUB.                                      ELTSAM  
01684      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTSAM  
01685                                                                   ELTSAM  
01686                                                                   ELTSAM  
01687 ************************************************************      ELTSAM  
01688 *                                                          *      ELTSAM  
01689 *        OUTPUT LAST LINE                                  *      ELTSAM  
01690 *                                                          *      ELTSAM  
01691 ************************************************************      ELTSAM  
01692  OUTPUT-LAST-LINE.                                                ELTSAM  
01693      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTSAM  
01694          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTSAM  
01695      IF BLANK-LINE-NEEDED                                         ELTSAM  
01696          PERFORM CREATE-A-BLANK-LINE.                             ELTSAM  
01697                                                                   ELTSAM  
01698                                                                   ELTSAM  
01699 ************************************************************      ELTSAM  
01700 *                                                          *      ELTSAM  
01701 *        CREATE A BLANK LINE                               *      ELTSAM  
01702 *                                                          *      ELTSAM  
01703 ************************************************************      ELTSAM  
01704  CREATE-A-BLANK-LINE.                                             ELTSAM  
01705      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTSAM  
01706      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTSAM  
01707      EJECT                                                        ELTSAM  
01708                                                                   ELTSAM  
01709                                                                   ELTSAM  
01710 ************************************************************      ELTSAM  
01711 *                                                          *      ELTSAM  
01712 *        MOVE A LINE                                       *      ELTSAM  
01713 *                                                          *      ELTSAM  
01714 ************************************************************      ELTSAM  
01715  MOVE-A-LINE.                                                     ELTSAM  
01716      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTSAM  
01717          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTSAM  
01718      SET CMF-DESCR-IDX UP BY 1.                                   ELTSAM  
01719      ADD 1 TO TCAR-FROM-SUB.                                      ELTSAM  
01720      EJECT                                                        ELTSAM  
01721                                                                   ELTSAM  
01722                                                                   ELTSAM  
01723 ************************************************************      ELTSAM  
01724 *                                                          *      ELTSAM  
01725 *        REFORMAT AND WRITE TEXT                           *      ELTSAM  
01726 *                                                          *      ELTSAM  
01727 ************************************************************      ELTSAM  
01728  REFORMAT-AND-WRITE-TEXT.                                         ELTSAM  
01729      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTSAM  
01730      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTSAM  
01731      PERFORM UNSTRING-TEXT.                                       ELTSAM  
01732      MOVE +1 TO TCAR-FROM-SUB.                                    ELTSAM  
01733      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTSAM  
01734      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTSAM  
01735          UNTIL COF-NBR-DTL-LINES GREATER                          ELTSAM  
01736                                   TCAR-OUTPUT-FIELDS-USED -       ELTSAM  
01737              1.                                                   ELTSAM  
01738      PERFORM DISPOSE-OF-LAST-LINE.                                ELTSAM  
01739      PERFORM LINK-TO-OUTPUT.                                      ELTSAM  
01740                                                                   ELTSAM  
01741                                                                   ELTSAM  
01742 ************************************************************      ELTSAM  
01743 *                                                          *      ELTSAM  
01744 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTSAM  
01745 *                                                          *      ELTSAM  
01746 ************************************************************      ELTSAM  
01747  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTSAM  
01748      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTSAM  
01749           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTSAM  
01750      ADD +1 TO TCAR-FROM-SUB.                                     ELTSAM  
01751      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTSAM  
01752      EJECT                                                        ELTSAM  
01753                                                                   ELTSAM  
01754                                                                   ELTSAM  
01755 ************************************************************      ELTSAM  
01756 *                                                          *      ELTSAM  
01757 *        UNSTRING TEXT                                     *      ELTSAM  
01758 *                                                          *      ELTSAM  
01759 ************************************************************      ELTSAM  
01760  UNSTRING-TEXT.                                                   ELTSAM  
01761      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTSAM  
01762      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTSAM  
01763      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTSAM  
01764      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTSAM  
01765      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTSAM  
01766      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTSAM  
01767      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTSAM  
01768      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTSAM  
01769      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTSAM  
01770      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTSAM  
01771      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTSAM  
01772      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTSAM  
01773      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTSAM  
01774      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTSAM  
01775      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTSAM  
01776      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTSAM  
01777      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTSAM  
01778      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTSAM  
01779      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTSAM  
01780      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTSAM  
01781      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTSAM  
01782      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTSAM  
01783      EJECT                                                        ELTSAM  
01784                                                                   ELTSAM  
01785                                                                   ELTSAM  
01786 ************************************************************      ELTSAM  
01787 *                                                          *      ELTSAM  
01788 *        LINK TO OUTPUT                                    *      ELTSAM  
01789 *                                                          *      ELTSAM  
01790 ************************************************************      ELTSAM  
01791  LINK-TO-OUTPUT.                                                  ELTSAM  
01792      EXEC CICS LINK                                               ELTSAM  
01793          PROGRAM ('ELUOUTPT')                                     ELTSAM  
01794          COMMAREA (DFHCOMMAREA)                                   ELTSAM  
01795          END-EXEC.                                                ELTSAM  
01796      EJECT                                                        ELTSAM  
01797                                                                   ELTSAM  
01798                                                                   ELTSAM  
01799 ************************************************************      ELTSAM  
01800 *                                                          *      ELTSAM  
01801 *        DISPOSE OF LAST LINE                              *      ELTSAM  
01802 *                                                          *      ELTSAM  
01803 ************************************************************      ELTSAM  
01804  DISPOSE-OF-LAST-LINE.                                            ELTSAM  
01805      IF NOT ADDITIONAL-TEXT                                       ELTSAM  
01806          PERFORM INITIALIZE-CONTINUED-SW.                         ELTSAM  
01807      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTSAM  
01808          PERFORM SAVE-LAST-LINE                                   ELTSAM  
01809      ELSE                                                         ELTSAM  
01810          PERFORM OUTPUT-LAST-LINE.                                ELTSAM  
01811                                                                   ELTSAM  
01812                                                                   ELTSAM  
01813 ************************************************************      ELTSAM  
01814 *                                                          *      ELTSAM  
01815 *        INITIALIZE CONTINUED SW                           *      ELTSAM  
01816 *                                                          *      ELTSAM  
01817 ************************************************************      ELTSAM  
01818  INITIALIZE-CONTINUED-SW.                                         ELTSAM  
01819      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTSAM  
