00001 *      LAST MAINTENANCE TIME:  7.54.00  DATE: 06/13/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTRPO  
00003                                                                      LV001
00004  PROGRAM-ID.         ELTRPO.                                      ELTRPO  
00005                                                                   ELTRPO  
00006  AUTHOR.             ANNE KEFFER KING.                            ELTRPO  
00007                      CLONED FORM ELTPPO.                          ELTRPO  
00008                                                                   ELTRPO  
00009  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTRPO  
00010                      A MUTUAL LEGAL RESERVE COMPANY               ELTRPO  
00011                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTRPO  
00012                      233 N. MICHIGAN AVE                          ELTRPO  
00013                      CHICAGO, ILLINOIS 60601                      ELTRPO  
00014                                                                   ELTRPO  
00015  DATE-WRITTEN.       05-JAN-1993.                                 ELTRPO  
00016                                                                   ELTRPO  
00017  DATE-COMPILED.                                                   ELTRPO  
00018                                                                   ELTRPO  
00019  SECURITY.           COPYRIGHT 1986,                              ELTRPO  
00020                      HEALTH CARE SERVICE CORPORATION              ELTRPO  
00021      SKIP3                                                        ELTRPO  
00022  ENVIRONMENT DIVISION.                                            ELTRPO  
00023                                                                   ELTRPO  
00024  CONFIGURATION SECTION.                                           ELTRPO  
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELTRPO  
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELTRPO  
00027      EJECT                                                        ELTRPO  
00028 ******************************************************************ELTRPO  
00029 *                                                                *ELTRPO  
00030 *    COPYBOOK:   ELTRPO                                          *ELTRPO  
00031 *    DATE:       04-JUN-1993                                     *ELTRPO  
00032 *    AUTHOR:     RICK BARILEAU                                   *ELTRPO  
00033 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTRPO  
00034 *                WITH RESTRICTED PROVIDER OPTION PROGRAM.        *ELTRPO  
00035 *    NOTES:      X---                                            *ELTRPO  
00036 *                                                                *ELTRPO  
00037 ******************************************************************ELTRPO  
00038 *                                                                *ELTRPO  
00039 *                      MAINTENANCE HISTORY                       *ELTRPO  
00040 *                                                                *ELTRPO  
00041 *  MOD     DATE     BY  DRPT                ACTION               *ELTRPO  
00042 * ----- ----------- --- ----- ---------------------------------- *ELTRPO  
00043 * 01.00 05-JAN-1993 AKK       CREATED.                           *ELTRPO  
00044 *                                                                *ELTRPO  
00045 * 01.01 17-FEB-1993 AKK       REMOVED SENTENCE TO SEND USER TO   *ELTRPO  
00046 *                             SPECIAL PROVIDER CONSIDERATIONS    *ELTRPO  
00047 *                             TOPICS FOR RELATED GVLS            *ELTRPO  
00048 *                                                                *ELTRPO  
00049 * 01.02 14-FEB-1994 AKK       CORRECTED ERROR IN RESETTING TCAR  *ELTRPO  
00050 *                             AREA THAT RESULTED IN DUPLICATE    *ELTRPO  
00051 *                             DISPLAY.                           *ELTRPO  
00052 ******************************************************************ELTRPO  
00053                                                                   ELTRPO  
00054  DATA DIVISION.                                                   ELTRPO  
00055                                                                   ELTRPO  
00056  WORKING-STORAGE SECTION.                                         ELTRPO  
00057  01  WS-MISC.                                                     ELTRPO  
00058      05  WS-BEGIN                        PIC X(26) VALUE          ELTRPO  
00059      '*** ELTRPO WS BEGINS ***'.                                  ELTRPO  
00060      05  WS-POINTER2                     POINTER.                 ELTRPO  
00061      05  WS-POINTER3                     POINTER.                 ELTRPO  
00062                                                                   ELTRPO  
00063  01  WS-SWITCHES.                                                 ELTRPO  
00064      05  ADDITIONAL-TEXT-SWITCH   PIC X(01) VALUE SPACE.          ELTRPO  
00065          88  ADDITIONAL-TEXT                VALUE 'A'.            ELTRPO  
00066          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTRPO  
00067      05  CONTINUED-PROCESSING-SW  PIC X(01) VALUE SPACE.          ELTRPO  
00068          88  DONE-PROCESSING                VALUE 'D'.            ELTRPO  
00069          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTRPO  
00070      05  WS-TABULAR-SWITCH        PIC X(01) VALUE 'N'.            ELTRPO  
00071          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTRPO  
00072      05  WS-PERIOD-SWITCH         PIC X(01) VALUE 'N'.            ELTRPO  
00073          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTRPO  
00074      05  WS-APPROVAL-SOURCE-SW    PIC X     VALUE SPACE.          ELTRPO  
00075          88  NOT-HOLDING-APPROVAL-SRCE      VALUE 'N'.            ELTRPO  
00076          88  HOLDING-APPROVAL-SOURCE        VALUE 'H'.            ELTRPO  
00077                                                                   ELTRPO  
00078  01  WS-HOLD-AREA.                                                ELTRPO  
00079      05  WS-GRPO-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTRPO  
00080                                                                   ELTRPO  
00081 **************************************************************    ELTRPO  
00082 ***                   PROGRAM CONSTANTS                           ELTRPO  
00083 **************************************************************    ELTRPO  
00084      05  WS-GRP                   PIC X(06) VALUE 'GROUP'.        ELTRPO  
00085      05  WS-GCCP                  PIC X(06) VALUE '#GCCP '.       ELTRPO  
00086      05  WS-GRPO                  PIC X(06) VALUE '#GRPO '.       ELTRPO  
00087      05  WS-INST                  PIC X(13) VALUE 'INSTITUTIONAL'.ELTRPO  
00088      05  WS-PROF                  PIC X(13) VALUE 'PROFESSIONAL '.ELTRPO  
00089      05  WS-SUPP                  PIC X(13) VALUE 'SUPPLEMENTAL '.ELTRPO  
00090      05  WS-APPROVAL              PIC X(09) VALUE 'APPROVAL.'.    ELTRPO  
00091                                                                   ELTRPO  
00092 **************************************************************    ELTRPO  
00093 ***                   HEADER  LINE                                ELTRPO  
00094 **************************************************************    ELTRPO  
00095      05  WS-HEADER-LINE.                                          ELTRPO  
00096          10  FILLER               PIC X(14) VALUE SPACES.         ELTRPO  
00097          10  FILLER               PIC X(35) VALUE                 ELTRPO  
00098          'RESTRICTED PROVIDER OPTION PROGRAM '.                   ELTRPO  
00099          10  WS-HDR-LINE-BCBSMM   PIC X(13) VALUE SPACES.         ELTRPO  
00100          10  FILLER               PIC X(17) VALUE SPACES.         ELTRPO  
00101                                                                   ELTRPO  
00102 **************************************************************    ELTRPO  
00103 ***                   SCREEN BODY LINES                           ELTRPO  
00104 **************************************************************    ELTRPO  
00105  01  WS-SCREEN-LINE-AREA.                                         ELTRPO  
00106      05  WS-APPRVL-LINE.                                          ELTRPO  
00107          10  FILLER               PIC X(44) VALUE                 ELTRPO  
00108          'RESTRICTED PROVIDER OPTION PROGRAM REQUIRES '.          ELTRPO  
00109          10  FILLER               PIC X(35) VALUE SPACES.         ELTRPO  
00110                                                                   ELTRPO  
00111      05  WS-ALT-PRICING-LINE-BC.                                  ELTRPO  
00112          10  FILLER               PIC X(52) VALUE                 ELTRPO  
00113          'THE ALTERNATE PRICING FOR INSTITUTIONAL SERVICES IS '.  ELTRPO  
00114          10  FILLER               PIC X(27) VALUE SPACES.         ELTRPO  
00115                                                                   ELTRPO  
00116      05  WS-ALT-PRICING-LINE-BS.                                  ELTRPO  
00117          10  FILLER               PIC X(51) VALUE                 ELTRPO  
00118          'THE ALTERNATE PRICING FOR PROFESSIONAL SERVICES IS '.   ELTRPO  
00119          10  FILLER               PIC X(28) VALUE SPACES.         ELTRPO  
00120                                                                   ELTRPO  
00121      05  WS-BENE-REDUCT-LINE.                                     ELTRPO  
00122          10  FILLER               PIC X(47) VALUE                 ELTRPO  
00123          'DENIED OR REDUCED BENEFITS DUE TO THIS PROGRAM:'.       ELTRPO  
00124          10  FILLER               PIC X(32) VALUE SPACES.         ELTRPO  
00125                                                                   ELTRPO  
00126      05  WS-SPILL-OVER-LINE.                                      ELTRPO  
00127          10  FILLER               PIC X(51) VALUE                 ELTRPO  
00128          'UNPAID SERVICES AFTER BASIC BENEFITS REDUCTION ARE '.   ELTRPO  
00129          10  FILLER               PIC X(28) VALUE SPACES.         ELTRPO  
00130                                                                   ELTRPO  
00131 **************************************************************    ELTRPO  
00132 ** SPECIAL MESSAGE FOR THE NOT APPLICABLE                         ELTRPO  
00133 ** ALSO THE FIXED TEXT FOR TABULAR GRPO                           ELTRPO  
00134 **************************************************************    ELTRPO  
00135      05  WS-RPO-APPLIES.                                          ELTRPO  
00136          10  FILLER               PIC X(79) VALUE                 ELTRPO  
00137          'THE RESTRICTED PROVIDER OPTION PROGRAM APPLIES TO'.     ELTRPO  
00138                                                                   ELTRPO  
00139      05  WS-NOT-APPLICABLE-MSG.                                   ELTRPO  
00140          10  FILLER               PIC X(39) VALUE                 ELTRPO  
00141          'THE RESTRICTED PROVIDER OPTION PROGRAM '.               ELTRPO  
00142          10  FILLER               PIC X(18) VALUE                 ELTRPO  
00143          'IS NOT APPLICABLE.'.                                    ELTRPO  
00144          10  FILLER               PIC X(22) VALUE SPACES.         ELTRPO  
00145                                                                   ELTRPO  
00146      05  WS-SPEC-PROV-MSG.                                        ELTRPO  
00147          10  FILLER               PIC X(79) VALUE                 ELTRPO  
00148          'THERE ARE SPECIAL PROVIDERS INCLUDED IN THIS COST CONTAIELTRPO  
00149 -        'NMENT PROGRAM.'.                                        ELTRPO  
00150                                                                   ELTRPO  
00151      05  WS-DISCLAIMER-MSG.                                       ELTRPO  
00152          10  FILLER               PIC X(79) VALUE                 ELTRPO  
00153          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTRPO  
00154                                                                   ELTRPO  
00155      05  WS-NOT-APPLY-TO-LOB-BC.                                  ELTRPO  
00156          10  FILLER               PIC X(50) VALUE                 ELTRPO  
00157          'RESTRICTED PROVIDER OPTION PROGRAM DOES NOT APPLY '.    ELTRPO  
00158          10  FILLER               PIC X(29) VALUE                 ELTRPO  
00159          'FOR INSTITUTIONAL BENEFITS'.                            ELTRPO  
00160                                                                   ELTRPO  
00161      05  WS-NOT-APPLY-TO-LOB-BS.                                  ELTRPO  
00162          10  FILLER               PIC X(50) VALUE                 ELTRPO  
00163          'RESTRICTED PROVIDER OPTION PROGRAM DOES NOT APPLY '.    ELTRPO  
00164          10  FILLER               PIC X(29) VALUE                 ELTRPO  
00165          'FOR PROFESSIONAL BENEFITS.'.                            ELTRPO  
00166                                                                   ELTRPO  
00167      05  WS-NOT-APPLY-TO-LOB-MM.                                  ELTRPO  
00168          10  FILLER               PIC X(50) VALUE                 ELTRPO  
00169          'RESTRICTED PROVIDER OPTION PROGRAM DOES NOT APPLY '.    ELTRPO  
00170          10  FILLER               PIC X(29) VALUE                 ELTRPO  
00171          'FOR SUPPLEMENTAL BENEFITS.'.                            ELTRPO  
00172                                                                   ELTRPO  
00173  LINKAGE SECTION.                                                 ELTRPO  
00174  01  DFHCOMMAREA.                                                 ELTRPO  
00175      COPY ELSCOMMC.                                               ELTRPO  
00176 /                                                                 ELTRPO  
00177      COPY ELSCIA2C.                                               ELTRPO  
00178 /                                                                 ELTRPO  
00179      COPY ELSCMDSC.                                               ELTRPO  
00180 /                                                                 ELTRPO  
00181      COPY ELSCMIFC.                                               ELTRPO  
00182 /                                                                 ELTRPO  
00183      COPY ELSIOPMC.                                               ELTRPO  
00184 /                                                                 ELTRPO  
00185      COPY ELSKEYSC.                                               ELTRPO  
00186 /                                                                 ELTRPO  
00187      COPY ELSOUTPC.                                               ELTRPO  
00188 /                                                                 ELTRPO  
00189      COPY ELSSRTPC.                                               ELTRPO  
00190 /                                                                 ELTRPO  
00191      COPY ELSTCWAC.                                               ELTRPO  
00192 /                                                                 ELTRPO  
00193      COPY ELSSSCBC.                                               ELTRPO  
00194 /                                                                 ELTRPO  
00195  01  GROUP-SPECIFIC-REC.                                          ELTRPO  
00196      COPY GCGROUPC.                                               ELTRPO  
00197 /                                                                 ELTRPO  
00198  01  GCCP-TABULAR-REC-AREA.                                       ELTRPO  
00199      COPY GCTGCCPC.                                               ELTRPO  
00200      EJECT                                                        ELTRPO  
00201  PROCEDURE DIVISION.                                              ELTRPO  
00202 ************************************************************      ELTRPO  
00203 *                                                          *      ELTRPO  
00204 *                    PROCEDURE DIVISION                    *      ELTRPO  
00205 *                                                          *      ELTRPO  
00206 ************************************************************      ELTRPO  
00207                                                                   ELTRPO  
00208                                                                   ELTRPO  
00209 ************************************************************      ELTRPO  
00210 *                                                          *      ELTRPO  
00211 *        RESTRICTED PROVIDER OPTION                        *      ELTRPO  
00212 *                                                          *      ELTRPO  
00213 ************************************************************      ELTRPO  
00214  RESTRICTED-PROVIDER-OPTION.                                      ELTRPO  
00215      PERFORM INITIALIZATION.                                      ELTRPO  
00216      PERFORM PROCESS.                                             ELTRPO  
00217      GOBACK.                                                      ELTRPO  
00218                                                                   ELTRPO  
00219 ************************************************************      ELTRPO  
00220 *                                                          *      ELTRPO  
00221 *        INITIALIZATION                                    *      ELTRPO  
00222 *                                                          *      ELTRPO  
00223 ************************************************************      ELTRPO  
00224  INITIALIZATION.                                                  ELTRPO  
00225      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTRPO  
00226      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTRPO  
00227                                                                   ELTRPO  
00228 ************************************************************      ELTRPO  
00229 *                                                          *      ELTRPO  
00230 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTRPO  
00231 *                                                          *      ELTRPO  
00232 ************************************************************      ELTRPO  
00233  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTRPO  
00234      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTRPO  
00235      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTRPO  
00236      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTRPO  
00237                                                                   ELTRPO  
00238 ************************************************************      ELTRPO  
00239 *                                                          *      ELTRPO  
00240 *        CHECK FOR VALID COMMAREA                          *      ELTRPO  
00241 *                                                          *      ELTRPO  
00242 ************************************************************      ELTRPO  
00243  CHECK-FOR-VALID-COMMAREA.                                        ELTRPO  
00244      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTRPO  
00245          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTRPO  
00246                                                                   ELTRPO  
00247 ************************************************************      ELTRPO  
00248 *                                                          *      ELTRPO  
00249 *        SIGNAL INVALID COMMAREA                           *      ELTRPO  
00250 *                                                          *      ELTRPO  
00251 ************************************************************      ELTRPO  
00252  SIGNAL-INVALID-COMMAREA.                                         ELTRPO  
00253      EXEC CICS ABEND                                              ELTRPO  
00254                ABCODE('EL01')                                     ELTRPO  
00255         END-EXEC.                                                 ELTRPO  
00256      EJECT                                                        ELTRPO  
00257                                                                   ELTRPO  
00258 ************************************************************      ELTRPO  
00259 *                                                          *      ELTRPO  
00260 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTRPO  
00261 *                                                          *      ELTRPO  
00262 ************************************************************      ELTRPO  
00263  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTRPO  
00264      IF ECA-CIA-PTR = NULL                                        ELTRPO  
00265          PERFORM SIGNAL-INVALID-CIA                               ELTRPO  
00266      ELSE                                                         ELTRPO  
00267          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTRPO  
00268                                                                   ELTRPO  
00269                                                                   ELTRPO  
00270 ************************************************************      ELTRPO  
00271 *                                                          *      ELTRPO  
00272 *        SIGNAL INVALID CIA                                *      ELTRPO  
00273 *                                                          *      ELTRPO  
00274 ************************************************************      ELTRPO  
00275  SIGNAL-INVALID-CIA.                                              ELTRPO  
00276      EXEC CICS ABEND                                              ELTRPO  
00277                ABCODE('EL02')                                     ELTRPO  
00278         END-EXEC.                                                 ELTRPO  
00279                                                                   ELTRPO  
00280 ************************************************************      ELTRPO  
00281 *                                                          *      ELTRPO  
00282 *        ESTABLISH ADDRESS OF CIA                          *      ELTRPO  
00283 *                                                          *      ELTRPO  
00284 ************************************************************      ELTRPO  
00285  ESTABLISH-ADDRESS-OF-CIA.                                        ELTRPO  
00286      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTRPO  
00287                            ADDRESS OF                             ELTRPO  
00288          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTRPO  
00289      EJECT                                                        ELTRPO  
00290                                                                   ELTRPO  
00291 ************************************************************      ELTRPO  
00292 *                                                          *      ELTRPO  
00293 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTRPO  
00294 *                                                          *      ELTRPO  
00295 ************************************************************      ELTRPO  
00296  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTRPO  
00297      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTRPO  
00298      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPO  
00299                            ADDRESS OF                             ELTRPO  
00300          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTRPO  
00301      IF CIA-RC-PTR-NULL                                           ELTRPO  
00302          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTRPO  
00303                                                                   ELTRPO  
00304 ************************************************************      ELTRPO  
00305 *                                                          *      ELTRPO  
00306 *        SIGNAL UNALLOC AREA ERROR                         *      ELTRPO  
00307 *                                                          *      ELTRPO  
00308 ************************************************************      ELTRPO  
00309  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTRPO  
00310      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTRPO  
00311      PERFORM SIGNAL-ABEND.                                        ELTRPO  
00312                                                                   ELTRPO  
00313 ************************************************************      ELTRPO  
00314 *                                                          *      ELTRPO  
00315 *        SIGNAL ABEND                                      *      ELTRPO  
00316 *                                                          *      ELTRPO  
00317 ************************************************************      ELTRPO  
00318  SIGNAL-ABEND.                                                    ELTRPO  
00319      EXEC CICS ABEND                                              ELTRPO  
00320                ABCODE(CIA-ABCODE)                                 ELTRPO  
00321         END-EXEC.                                                 ELTRPO  
00322      EJECT                                                        ELTRPO  
00323                                                                   ELTRPO  
00324 ************************************************************      ELTRPO  
00325 *                                                          *      ELTRPO  
00326 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTRPO  
00327 *                                                          *      ELTRPO  
00328 ************************************************************      ELTRPO  
00329  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTRPO  
00330      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTRPO  
00331      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTRPO  
00332      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTRPO  
00333      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTRPO  
00334      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTRPO  
00335      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTRPO  
00336      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTRPO  
00337                                                                   ELTRPO  
00338 ************************************************************      ELTRPO  
00339 *                                                          *      ELTRPO  
00340 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTRPO  
00341 *                                                          *      ELTRPO  
00342 ************************************************************      ELTRPO  
00343  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTRPO  
00344      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTRPO  
00345      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPO  
00346                            ADDRESS OF                             ELTRPO  
00347          CMF-CODES-MANUAL-INTERFACE.                              ELTRPO  
00348      IF CIA-RC-PTR-NULL                                           ELTRPO  
00349          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTRPO  
00350      EJECT                                                        ELTRPO  
00351                                                                   ELTRPO  
00352 ************************************************************      ELTRPO  
00353 *                                                          *      ELTRPO  
00354 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTRPO  
00355 *                                                          *      ELTRPO  
00356 ************************************************************      ELTRPO  
00357  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTRPO  
00358      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTRPO  
00359      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPO  
00360                            ADDRESS OF                             ELTRPO  
00361          COF-OUTPUT-INTERFACE.                                    ELTRPO  
00362      IF CIA-RC-PTR-NULL                                           ELTRPO  
00363          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTRPO  
00364      EJECT                                                        ELTRPO  
00365                                                                   ELTRPO  
00366 ************************************************************      ELTRPO  
00367 *                                                          *      ELTRPO  
00368 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTRPO  
00369 *                                                          *      ELTRPO  
00370 ************************************************************      ELTRPO  
00371  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTRPO  
00372      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTRPO  
00373      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPO  
00374                            ADDRESS OF                             ELTRPO  
00375          SRP-SUBROUTINE-PARAMETERS.                               ELTRPO  
00376      IF CIA-RC-PTR-NULL                                           ELTRPO  
00377          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTRPO  
00378      EJECT                                                        ELTRPO  
00379                                                                   ELTRPO  
00380 ************************************************************      ELTRPO  
00381 *                                                          *      ELTRPO  
00382 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTRPO  
00383 *                                                          *      ELTRPO  
00384 ************************************************************      ELTRPO  
00385  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTRPO  
00386      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTRPO  
00387      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPO  
00388                            ADDRESS OF                             ELTRPO  
00389          TCAR-COMPRESSION-WORK-AREA.                              ELTRPO  
00390      IF CIA-RC-PTR-NULL                                           ELTRPO  
00391          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTRPO  
00392      EJECT                                                        ELTRPO  
00393                                                                   ELTRPO  
00394 ************************************************************      ELTRPO  
00395 *                                                          *      ELTRPO  
00396 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTRPO  
00397 *                                                          *      ELTRPO  
00398 ************************************************************      ELTRPO  
00399  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTRPO  
00400      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTRPO  
00401      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPO  
00402                            ADDRESS OF                             ELTRPO  
00403          KWA-FILE-KEY-WORK-AREA.                                  ELTRPO  
00404      IF CIA-RC-PTR-NULL                                           ELTRPO  
00405          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTRPO  
00406      EJECT                                                        ELTRPO  
00407                                                                   ELTRPO  
00408 ************************************************************      ELTRPO  
00409 *                                                          *      ELTRPO  
00410 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTRPO  
00411 *                                                          *      ELTRPO  
00412 ************************************************************      ELTRPO  
00413  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTRPO  
00414      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTRPO  
00415      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPO  
00416                            ADDRESS OF                             ELTRPO  
00417          GROUP-SPECIFIC-REC.                                      ELTRPO  
00418      IF CIA-RC-PTR-NULL                                           ELTRPO  
00419          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTRPO  
00420      EJECT                                                        ELTRPO  
00421                                                                   ELTRPO  
00422 ************************************************************      ELTRPO  
00423 *                                                          *      ELTRPO  
00424 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELTRPO  
00425 *                                                          *      ELTRPO  
00426 ************************************************************      ELTRPO  
00427  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTRPO  
00428      SET CIA-GCTABULR-DDN TO TRUE.                                ELTRPO  
00429      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPO  
00430                            ADDRESS OF                             ELTRPO  
00431          GCCP-TABULAR-REC-AREA.                                   ELTRPO  
00432      IF CIA-RC-PTR-NULL                                           ELTRPO  
00433          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTRPO  
00434      EJECT                                                        ELTRPO  
00435                                                                   ELTRPO  
00436 ************************************************************      ELTRPO  
00437 *                                                          *      ELTRPO  
00438 *        PROCESS                                           *      ELTRPO  
00439 *                                                          *      ELTRPO  
00440 ************************************************************      ELTRPO  
00441  PROCESS.                                                         ELTRPO  
00442      IF GCG-RPO-INDICATOR = ZERO                                  ELTRPO  
00443          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTRPO  
00444      ELSE                                                         ELTRPO  
00445          PERFORM GENERATE-RPO-TEXT.                               ELTRPO  
00446      PERFORM TERMINATE-OUTPUT.                                    ELTRPO  
00447                                                                   ELTRPO  
00448 ************************************************************      ELTRPO  
00449 *                                                          *      ELTRPO  
00450 *        EJECT NEW PAGE                                    *      ELTRPO  
00451 *                                                          *      ELTRPO  
00452 ************************************************************      ELTRPO  
00453  EJECT-NEW-PAGE.                                                  ELTRPO  
00454      SET COF-NEW-PAGE      TO TRUE.                               ELTRPO  
00455      MOVE WS-HEADER-LINE   TO COF-HDR-LINE                        ELTRPO  
00456          (COF-NBR-HDR-LINES).                                     ELTRPO  
00457      PERFORM LINK-TO-OUTPUT.                                      ELTRPO  
00458      EJECT                                                        ELTRPO  
00459                                                                   ELTRPO  
00460 ************************************************************      ELTRPO  
00461 *                                                          *      ELTRPO  
00462 *        TRANSLATE AND DISPLAY RPO IND                     *      ELTRPO  
00463 *                                                          *      ELTRPO  
00464 ************************************************************      ELTRPO  
00465  TRANSLATE-AND-DISPLAY-RPO-IND.                                   ELTRPO  
00466      INITIALIZE TCAR-FROM-AREA.                                   ELTRPO  
00467      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
00468      MOVE WS-RPO-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTRPO  
00469      ADD  +1 TO TCAR-FROM-SUB.                                    ELTRPO  
00470      SET BLANK-LINE-NEEDED TO TRUE.                               ELTRPO  
00471      SET PERIOD-NEEDED TO TRUE.                                   ELTRPO  
00472      MOVE GCG-RPO-INDICATOR TO CMF-CODE-VALUE.                    ELTRPO  
00473      MOVE 'RPO-INDICATOR' TO CMF-ELEMENT-SYSTEM-NAME.             ELTRPO  
00474      MOVE WS-GRP TO CMF-RECORD-PREFIX.                            ELTRPO  
00475      EXEC CICS LINK                                               ELTRPO  
00476                PROGRAM ('ELUCMIF')                                ELTRPO  
00477                COMMAREA (DFHCOMMAREA)                             ELTRPO  
00478         END-EXEC.                                                 ELTRPO  
00479      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTRPO  
00480      MOVE SPACE TO ADDITIONAL-TEXT-SWITCH.                        ELTRPO  
00481                                                                   ELTRPO  
00482 ************************************************************      ELTRPO  
00483 *                                                          *      ELTRPO  
00484 *        SET UP OUTPUT SUBSCRIPTS                          *      ELTRPO  
00485 *                                                          *      ELTRPO  
00486 ************************************************************      ELTRPO  
00487  SET-UP-OUTPUT-SUBSCRIPTS.                                        ELTRPO  
00488      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTRPO  
00489      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTRPO  
00490                                                                   ELTRPO  
00491 ************************************************************      ELTRPO  
00492 *                                                          *      ELTRPO  
00493 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTRPO  
00494 *                                                          *      ELTRPO  
00495 ************************************************************      ELTRPO  
00496  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTRPO  
00497      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTRPO  
00498      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTRPO  
00499      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTRPO  
00500      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTRPO  
00501          (COF-NBR-DTL-LINES).                                     ELTRPO  
00502      PERFORM EJECT-NEW-PAGE.                                      ELTRPO  
00503      EJECT                                                        ELTRPO  
00504                                                                   ELTRPO  
00505 ************************************************************      ELTRPO  
00506 *                                                          *      ELTRPO  
00507 *        GENERATE RPO TEXT                                 *      ELTRPO  
00508 *                                                          *      ELTRPO  
00509 ************************************************************      ELTRPO  
00510  GENERATE-RPO-TEXT.                                               ELTRPO  
00511      SET WS-POINTER2 TO NULLS.                                    ELTRPO  
00512      SET WS-POINTER3 TO NULLS.                                    ELTRPO  
00513      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTRPO  
00514      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTRPO  
00515                            WS-POINTER2.                           ELTRPO  
00516      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTRPO  
00517      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTRPO  
00518                            WS-POINTER3.                           ELTRPO  
00519      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELTRPO  
00520      PERFORM DETERMINE-SELECTION.                                 ELTRPO  
00521      EJECT                                                        ELTRPO  
00522                                                                   ELTRPO  
00523 ************************************************************      ELTRPO  
00524 *                                                          *      ELTRPO  
00525 *        TERMINATE OUTPUT                                  *      ELTRPO  
00526 *                                                          *      ELTRPO  
00527 ************************************************************      ELTRPO  
00528  TERMINATE-OUTPUT.                                                ELTRPO  
00529      SET COF-END TO TRUE.                                         ELTRPO  
00530      PERFORM LINK-TO-OUTPUT.                                      ELTRPO  
00531      EJECT                                                        ELTRPO  
00532                                                                   ELTRPO  
00533 ************************************************************      ELTRPO  
00534 *                                                          *      ELTRPO  
00535 *        SEARCH FOR GCCP TABULAR                           *      ELTRPO  
00536 *                                                          *      ELTRPO  
00537 ************************************************************      ELTRPO  
00538  SEARCH-FOR-GCCP-TABULAR.                                         ELTRPO  
00539      SET GCG-INDEX TO +1.                                         ELTRPO  
00540      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTRPO  
00541         AT END                                                    ELTRPO  
00542              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELTRPO  
00543         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCCP                     ELTRPO  
00544              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTRPO  
00545                  TO KWA-PROVISION-ID                              ELTRPO  
00546              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTRPO  
00547                  TO KWA-PROVISION-SLOT-NO                         ELTRPO  
00548         END-SEARCH.                                               ELTRPO  
00549      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTRPO  
00550          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTRPO  
00551      PERFORM GET-GCCP-TABULAR.                                    ELTRPO  
00552      PERFORM SEARCH-THE-GSS-ENTRY.                                ELTRPO  
00553      EJECT                                                        ELTRPO  
00554                                                                   ELTRPO  
00555 ************************************************************      ELTRPO  
00556 *                                                          *      ELTRPO  
00557 *        TRANSLATE APPROVAL SOURCE                         *      ELTRPO  
00558 *                                                          *      ELTRPO  
00559 ************************************************************      ELTRPO  
00560  TRANSLATE-APPROVAL-SOURCE.                                       ELTRPO  
00561      INITIALIZE WS-PERIOD-SWITCH.                                 ELTRPO  
00562      INITIALIZE TCAR-FROM-AREA.                                   ELTRPO  
00563      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
00564      MOVE WS-APPRVL-LINE TO TCAR-FROM-LINE                        ELTRPO  
00565          (TCAR-FROM-SUB).                                         ELTRPO  
00566      ADD  +1 TO TCAR-FROM-SUB.                                    ELTRPO  
00567      SET ADDITIONAL-TEXT TO TRUE.                                 ELTRPO  
00568      IF NOT-HOLDING-APPROVAL-SRCE                                 ELTRPO  
00569          PERFORM GET-APPROVAL-TRANSLATION                         ELTRPO  
00570      ELSE                                                         ELTRPO  
00571          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTRPO  
00572      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTRPO  
00573      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTRPO  
00574      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTRPO  
00575                            WS-POINTER3.                           ELTRPO  
00576      MOVE WS-APPROVAL TO TCAR-FROM-LINE                           ELTRPO  
00577          (TCAR-FROM-SUB).                                         ELTRPO  
00578      PERFORM FINISH-SENTENCE.                                     ELTRPO  
00579      EJECT                                                        ELTRPO  
00580                                                                   ELTRPO  
00581                                                                   ELTRPO  
00582 ************************************************************      ELTRPO  
00583 *                                                          *      ELTRPO  
00584 *        GET APPROVAL TRANSLATION                          *      ELTRPO  
00585 *                                                          *      ELTRPO  
00586 ************************************************************      ELTRPO  
00587  GET-APPROVAL-TRANSLATION.                                        ELTRPO  
00588      MOVE GSS-RP-APPROVAL-SRC-IND (GSS-INDEX) TO CMF-CODE-VALUE.  ELTRPO  
00589      MOVE   'RP-APPROVAL-SRC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTRPO  
00590      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTRPO  
00591      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTRPO  
00592      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTRPO  
00593                            ADDRESS OF CMF-DESCR.                  ELTRPO  
00594      EJECT                                                        ELTRPO  
00595                                                                   ELTRPO  
00596 ************************************************************      ELTRPO  
00597 *                                                          *      ELTRPO  
00598 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTRPO  
00599 *                                                          *      ELTRPO  
00600 ************************************************************      ELTRPO  
00601  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTRPO  
00602      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTRPO  
00603      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTRPO  
00604                            ADDRESS OF CMF-DESCR.                  ELTRPO  
00605      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTRPO  
00606      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTRPO  
00607                            WS-POINTER2.                           ELTRPO  
00608      EJECT                                                        ELTRPO  
00609                                                                   ELTRPO  
00610 ************************************************************      ELTRPO  
00611 *                                                          *      ELTRPO  
00612 *        FINISH SENTENCE                                   *      ELTRPO  
00613 *                                                          *      ELTRPO  
00614 ************************************************************      ELTRPO  
00615  FINISH-SENTENCE.                                                 ELTRPO  
00616      SET BLANK-LINE-NEEDED TO TRUE.                               ELTRPO  
00617      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTRPO  
00618                                                                   ELTRPO  
00619 ************************************************************      ELTRPO  
00620 *                                                          *      ELTRPO  
00621 *        TRANSLATE AND DISPLAY CODE VALUE                  *      ELTRPO  
00622 *                                                          *      ELTRPO  
00623 ************************************************************      ELTRPO  
00624  TRANSLATE-AND-DISPLAY-CODE-VAL.                                  ELTRPO  
00625      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTRPO  
00626      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTRPO  
00627                                                                   ELTRPO  
00628 ************************************************************      ELTRPO  
00629 *                                                          *      ELTRPO  
00630 *        SIGNAL UNDEFINED TABULAR ERROR                    *      ELTRPO  
00631 *                                                          *      ELTRPO  
00632 ************************************************************      ELTRPO  
00633  SIGNAL-UNDEFINED-TABULAR-ERROR.                                  ELTRPO  
00634      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTRPO  
00635      PERFORM SIGNAL-ABEND.                                        ELTRPO  
00636      EJECT                                                        ELTRPO  
00637                                                                   ELTRPO  
00638 ************************************************************      ELTRPO  
00639 *                                                          *      ELTRPO  
00640 *        DETERMINE SELECTION                               *      ELTRPO  
00641 *                                                          *      ELTRPO  
00642 ************************************************************      ELTRPO  
00643  DETERMINE-SELECTION.                                             ELTRPO  
00644      SET NOT-HOLDING-APPROVAL-SRCE TO TRUE.                       ELTRPO  
00645      IF SSB-PROV-CLASS-INST OR                                    ELTRPO  
00646                   SSB-PROV-CLASS-BOTH                             ELTRPO  
00647          PERFORM CREATE-INSTITUTIONAL-SCREEN.                     ELTRPO  
00648      IF SSB-PROV-CLASS-PROF OR                                    ELTRPO  
00649                   SSB-PROV-CLASS-BOTH                             ELTRPO  
00650          PERFORM CREATE-PROFESSIONAL-SCREEN.                      ELTRPO  
00651      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL '03' OR '04' OR       ELTRPO  
00652                    '06' OR '08')                                  ELTRPO  
00653          PERFORM CREATE-SUPPLEMENTAL-SCREEN.                      ELTRPO  
00654                                                                   ELTRPO  
00655 ************************************************************      ELTRPO  
00656 *                                                          *      ELTRPO  
00657 *        CREATE INSTITUTIONAL SCREEN                       *      ELTRPO  
00658 *                                                          *      ELTRPO  
00659 ************************************************************      ELTRPO  
00660  CREATE-INSTITUTIONAL-SCREEN.                                     ELTRPO  
00661      MOVE WS-INST TO WS-HDR-LINE-BCBSMM.                          ELTRPO  
00662      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTRPO  
00663      IF GSS-RP-BC-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTRPO  
00664          ZEROES                                                   ELTRPO  
00665                 AND LOW-VALUES                                    ELTRPO  
00666          PERFORM GENERATE-INSTITUTIONAL-TEXT                      ELTRPO  
00667      ELSE                                                         ELTRPO  
00668          PERFORM GENERATE-INSTITUTIONAL-NOT-APP.                  ELTRPO  
00669      EJECT                                                        ELTRPO  
00670                                                                   ELTRPO  
00671 ************************************************************      ELTRPO  
00672 *                                                          *      ELTRPO  
00673 *        GENERATE INSTITUTIONAL TEXT                       *      ELTRPO  
00674 *                                                          *      ELTRPO  
00675 ************************************************************      ELTRPO  
00676  GENERATE-INSTITUTIONAL-TEXT.                                     ELTRPO  
00677      PERFORM EJECT-NEW-PAGE.                                      ELTRPO  
00678      PERFORM TRANSLATE-AND-DISPLAY-RPO-IND.                       ELTRPO  
00679      IF GSS-RP-APPROVAL-SRC-IND (GSS-INDEX) NOT EQUAL SPACES      ELTRPO  
00680          AND                                                      ELTRPO  
00681                 ZEROES AND LOW-VALUES                             ELTRPO  
00682          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTRPO  
00683      PERFORM TRANSLATE-BC-IND.                                    ELTRPO  
00684      IF GSS-RP-BC-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTRPO  
00685           AND ZEROES AND LOW-VALUES                               ELTRPO  
00686            PERFORM TRANSLATE-BC-PAYMENT-LEVEL-IND.                ELTRPO  
00687      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTRPO  
00688      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTRPO  
00689      IF GSS-RP-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTRPO  
00690          ZEROES                                                   ELTRPO  
00691                 AND LOW-VALUES                                    ELTRPO  
00692          PERFORM TRANSLATE-BC-CALC-METHOD.                        ELTRPO  
00693      IF GSS-RP-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTRPO  
00694          SPACES AND                                               ELTRPO  
00695                 ZEROES AND LOW-VALUES                             ELTRPO  
00696          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTRPO  
00697      IF GSS-RP-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTRPO  
00698          ZEROES                                                   ELTRPO  
00699                 AND LOW-VALUES                                    ELTRPO  
00700          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTRPO  
00701      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTRPO  
00702      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTRPO  
00703      EJECT                                                        ELTRPO  
00704                                                                   ELTRPO  
00705 ************************************************************      ELTRPO  
00706 *                                                          *      ELTRPO  
00707 *        GENERATE INSTITUTIONAL NOT APPLICABLE             *      ELTRPO  
00708 *                                                          *      ELTRPO  
00709 ************************************************************      ELTRPO  
00710  GENERATE-INSTITUTIONAL-NOT-APP.                                  ELTRPO  
00711      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTRPO  
00712      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTRPO  
00713      MOVE WS-NOT-APPLY-TO-LOB-BC TO COF-DTL-LINE                  ELTRPO  
00714          (COF-NBR-DTL-LINES).                                     ELTRPO  
00715      PERFORM EJECT-NEW-PAGE.                                      ELTRPO  
00716      EJECT                                                        ELTRPO  
00717                                                                   ELTRPO  
00718                                                                   ELTRPO  
00719 ************************************************************      ELTRPO  
00720 *                                                          *      ELTRPO  
00721 *        GENERATE DISCLAIMER MESSAGE                       *      ELTRPO  
00722 *                                                          *      ELTRPO  
00723 ************************************************************      ELTRPO  
00724  GENERATE-DISCLAIMER-MESSAGE.                                     ELTRPO  
00725      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTRPO  
00726      MOVE WS-DISCLAIMER-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).  ELTRPO  
00727      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTRPO  
00728      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTRPO  
00729      PERFORM LINK-TO-OUTPUT.                                      ELTRPO  
00730      EJECT                                                        ELTRPO  
00731                                                                   ELTRPO  
00732 ************************************************************      ELTRPO  
00733 *                                                          *      ELTRPO  
00734 *        TRANSLATE BC IND                                  *      ELTRPO  
00735 *                                                          *      ELTRPO  
00736 ************************************************************      ELTRPO  
00737  TRANSLATE-BC-IND.                                                ELTRPO  
00738      INITIALIZE TCAR-FROM-AREA.                                   ELTRPO  
00739      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
00740      SET BLANK-LINE-NEEDED TO TRUE.                               ELTRPO  
00741      SET PERIOD-NEEDED TO TRUE.                                   ELTRPO  
00742      MOVE GSS-RP-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTRPO  
00743      MOVE   'RP-BC-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTRPO  
00744      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTRPO  
00745      EJECT                                                        ELTRPO  
00746                                                                   ELTRPO  
00747 ************************************************************      ELTRPO  
00748 *                                                          *      ELTRPO  
00749 *        TRANSLATE BC PAYMENT LEVEL INDICATOR              *      ELTRPO  
00750 *                                                          *      ELTRPO  
00751 ************************************************************      ELTRPO  
00752  TRANSLATE-BC-PAYMENT-LEVEL-IND.                                  ELTRPO  
00753      INITIALIZE TCAR-FROM-AREA.                                   ELTRPO  
00754      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
00755      MOVE 'RP-BC-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTRPO  
00756      MOVE GSS-RP-BC-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTRPO  
00757         CMF-CODE-VALUE.                                           ELTRPO  
00758      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTRPO  
00759      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTRPO  
00760                                                                   ELTRPO  
00761 ************************************************************      ELTRPO  
00762 *                                                          *      ELTRPO  
00763 *        TRANSLATE BC CALC METHOD                          *      ELTRPO  
00764 *                                                          *      ELTRPO  
00765 ************************************************************      ELTRPO  
00766  TRANSLATE-BC-CALC-METHOD.                                        ELTRPO  
00767      INITIALIZE TCAR-FROM-AREA.                                   ELTRPO  
00768      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
00769      SET BLANK-LINE-NEEDED TO TRUE.                               ELTRPO  
00770      SET PERIOD-NEEDED TO TRUE.                                   ELTRPO  
00771      MOVE GSS-RP-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTRPO  
00772      MOVE   'RP-BC-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTRPO  
00773      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTRPO  
00774      EJECT                                                        ELTRPO  
00775                                                                   ELTRPO  
00776 ************************************************************      ELTRPO  
00777 *                                                          *      ELTRPO  
00778 *        GENERATE ACCUM TABULAR DATA                       *      ELTRPO  
00779 *                                                          *      ELTRPO  
00780 ************************************************************      ELTRPO  
00781  GENERATE-ACCUM-TABULAR-DATA.                                     ELTRPO  
00782      PERFORM GENERATE-COINSURANCE.                                ELTRPO  
00783      PERFORM GENERATE-COPAY.                                      ELTRPO  
00784      PERFORM GENERATE-DEDUCTIBLE.                                 ELTRPO  
00785      PERFORM GENERATE-MAXIMUM.                                    ELTRPO  
00786      PERFORM GENERATE-OUT-OF-POCKET.                              ELTRPO  
00787      EJECT                                                        ELTRPO  
00788                                                                   ELTRPO  
00789 ************************************************************      ELTRPO  
00790 *                                                          *      ELTRPO  
00791 *        GENERATE COMBINED BENEFITS REDUCTION TEXT         *      ELTRPO  
00792 *                                                          *      ELTRPO  
00793 ************************************************************      ELTRPO  
00794  GENERATE-COMBINED-BENEFITS-RED.                                  ELTRPO  
00795      MOVE 'RP' TO SRP-COST-CONT-TYPE.                             ELTRPO  
00796      MOVE 'RESTRICTED PROVIDER OPTION PROGRAM' TO SRP-CCP-NAME.   ELTRPO  
00797      MOVE GSS-RP-COMB-BENE-REDUCT-IND (GSS-INDEX)                 ELTRPO  
00798                  TO SRP-CCP-COMB-BENE-REDUCT-IND.                 ELTRPO  
00799      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTRPO  
00800      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTRPO  
00801                            ADDRESS OF                             ELTRPO  
00802          GCCP-TABULAR-REC-AREA.                                   ELTRPO  
00803      PERFORM CALL-CBRI-INTERFACE.                                 ELTRPO  
00804      EJECT                                                        ELTRPO  
00805                                                                   ELTRPO  
00806 ************************************************************      ELTRPO  
00807 *                                                          *      ELTRPO  
00808 *        CREATE PROFESSIONAL SCREEN                        *      ELTRPO  
00809 *                                                          *      ELTRPO  
00810 ************************************************************      ELTRPO  
00811  CREATE-PROFESSIONAL-SCREEN.                                      ELTRPO  
00812      MOVE WS-PROF TO WS-HDR-LINE-BCBSMM.                          ELTRPO  
00813      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTRPO  
00814      IF GSS-RP-BS-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTRPO  
00815          ZEROES                                                   ELTRPO  
00816                 AND LOW-VALUES                                    ELTRPO  
00817          PERFORM GENERATE-PROFESSIONAL-TEXT                       ELTRPO  
00818      ELSE                                                         ELTRPO  
00819          PERFORM GENERATE-PROFESSIONAL-NOT-APPL.                  ELTRPO  
00820      EJECT                                                        ELTRPO  
00821                                                                   ELTRPO  
00822 ************************************************************      ELTRPO  
00823 *                                                          *      ELTRPO  
00824 *        GENERATE PROFESSIONAL TEXT                        *      ELTRPO  
00825 *                                                          *      ELTRPO  
00826 ************************************************************      ELTRPO  
00827  GENERATE-PROFESSIONAL-TEXT.                                      ELTRPO  
00828      PERFORM EJECT-NEW-PAGE.                                      ELTRPO  
00829      PERFORM TRANSLATE-AND-DISPLAY-RPO-IND.                       ELTRPO  
00830      IF GSS-RP-APPROVAL-SRC-IND (GSS-INDEX) NOT EQUAL             ELTRPO  
00831          SPACES                                                   ELTRPO  
00832                 AND ZEROES AND LOW-VALUES                         ELTRPO  
00833          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTRPO  
00834      PERFORM TRANSLATE-BS-IND.                                    ELTRPO  
00835      IF GSS-RP-BS-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTRPO  
00836            AND ZEROES AND LOW-VALUES                              ELTRPO  
00837            PERFORM TRANSLATE-BS-PAYMENT-LEVEL-IND.                ELTRPO  
00838      IF GSS-RP-BS-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTRPO  
00839          SPACES AND                                               ELTRPO  
00840                 ZEROES AND LOW-VALUES                             ELTRPO  
00841          PERFORM TRANSLATE-BS-ALT-PRIC.                           ELTRPO  
00842      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTRPO  
00843      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTRPO  
00844      IF GSS-RP-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTRPO  
00845          ZEROES                                                   ELTRPO  
00846                 AND LOW-VALUES                                    ELTRPO  
00847          PERFORM TRANSLATE-BS-CALC-METHOD.                        ELTRPO  
00848      IF GSS-RP-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTRPO  
00849          SPACES                                                   ELTRPO  
00850                 AND ZEROES AND LOW-VALUES                         ELTRPO  
00851          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTRPO  
00852      IF GSS-RP-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTRPO  
00853          ZEROES                                                   ELTRPO  
00854                 AND LOW-VALUES                                    ELTRPO  
00855          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTRPO  
00856      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTRPO  
00857      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTRPO  
00858                                                                   ELTRPO  
00859 ************************************************************      ELTRPO  
00860 *                                                          *      ELTRPO  
00861 *        GENERATE PROFESSIONAL NOT APPLICABLE              *      ELTRPO  
00862 *                                                          *      ELTRPO  
00863 ************************************************************      ELTRPO  
00864  GENERATE-PROFESSIONAL-NOT-APPL.                                  ELTRPO  
00865      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTRPO  
00866      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTRPO  
00867      MOVE WS-NOT-APPLY-TO-LOB-BS TO COF-DTL-LINE                  ELTRPO  
00868          (COF-NBR-DTL-LINES).                                     ELTRPO  
00869      PERFORM EJECT-NEW-PAGE.                                      ELTRPO  
00870      EJECT                                                        ELTRPO  
00871                                                                   ELTRPO  
00872 ************************************************************      ELTRPO  
00873 *                                                          *      ELTRPO  
00874 *        TRANSLATE BS IND                                  *      ELTRPO  
00875 *                                                          *      ELTRPO  
00876 ************************************************************      ELTRPO  
00877  TRANSLATE-BS-IND.                                                ELTRPO  
00878      INITIALIZE TCAR-FROM-AREA.                                   ELTRPO  
00879      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
00880      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTRPO  
00881      SET PERIOD-NEEDED TO TRUE.                                   ELTRPO  
00882      MOVE GSS-RP-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTRPO  
00883      MOVE   'RP-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTRPO  
00884      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTRPO  
00885      EJECT                                                        ELTRPO  
00886                                                                   ELTRPO  
00887 ************************************************************      ELTRPO  
00888 *                                                          *      ELTRPO  
00889 *        TRANSLATE BS PAYMENT LEVEL INDICATOR              *      ELTRPO  
00890 *                                                          *      ELTRPO  
00891 ************************************************************      ELTRPO  
00892  TRANSLATE-BS-PAYMENT-LEVEL-IND.                                  ELTRPO  
00893      MOVE 'RP-BS-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTRPO  
00894      MOVE GSS-RP-BS-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTRPO  
00895         CMF-CODE-VALUE.                                           ELTRPO  
00896      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTRPO  
00897      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTRPO  
00898                                                                   ELTRPO  
00899 ************************************************************      ELTRPO  
00900 *                                                          *      ELTRPO  
00901 *        TRANSLATE BS CALC METHOD                          *      ELTRPO  
00902 *                                                          *      ELTRPO  
00903 ************************************************************      ELTRPO  
00904  TRANSLATE-BS-CALC-METHOD.                                        ELTRPO  
00905      INITIALIZE TCAR-FROM-AREA.                                   ELTRPO  
00906      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
00907      SET BLANK-LINE-NEEDED TO TRUE.                               ELTRPO  
00908      SET PERIOD-NEEDED TO TRUE.                                   ELTRPO  
00909      MOVE GSS-RP-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTRPO  
00910      MOVE   'RP-BS-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTRPO  
00911      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTRPO  
00912      EJECT                                                        ELTRPO  
00913                                                                   ELTRPO  
00914 ************************************************************      ELTRPO  
00915 *                                                          *      ELTRPO  
00916 *        TRANSLATE BS ALT PRIC                             *      ELTRPO  
00917 *                                                          *      ELTRPO  
00918 ************************************************************      ELTRPO  
00919  TRANSLATE-BS-ALT-PRIC.                                           ELTRPO  
00920      INITIALIZE TCAR-FROM-AREA.                                   ELTRPO  
00921      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
00922      MOVE WS-ALT-PRICING-LINE-BS TO TCAR-FROM-LINE                ELTRPO  
00923          (TCAR-FROM-SUB).                                         ELTRPO  
00924      ADD  +1 TO TCAR-FROM-SUB.                                    ELTRPO  
00925      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTRPO  
00926      SET PERIOD-NEEDED TO TRUE.                                   ELTRPO  
00927      MOVE GSS-RP-BS-ALT-PRICING-METHOD (GSS-INDEX) TO             ELTRPO  
00928          CMF-CODE-VALUE.                                          ELTRPO  
00929      MOVE  'RP-BS-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME. ELTRPO  
00930      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTRPO  
00931      EJECT                                                        ELTRPO  
00932                                                                   ELTRPO  
00933 ************************************************************      ELTRPO  
00934 *                                                          *      ELTRPO  
00935 *        CREATE SUPPLEMENTAL SCREEN                        *      ELTRPO  
00936 *                                                          *      ELTRPO  
00937 ************************************************************      ELTRPO  
00938  CREATE-SUPPLEMENTAL-SCREEN.                                      ELTRPO  
00939      MOVE WS-SUPP TO WS-HDR-LINE-BCBSMM.                          ELTRPO  
00940      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTRPO  
00941      IF GSS-RP-MM-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTRPO  
00942          ZEROES                                                   ELTRPO  
00943                 AND LOW-VALUES                                    ELTRPO  
00944          PERFORM GENERATE-SUPPLEMENTAL-TEXT                       ELTRPO  
00945      ELSE                                                         ELTRPO  
00946          PERFORM GENERATE-SUPPLEMENTAL-NOT-APPL.                  ELTRPO  
00947      EJECT                                                        ELTRPO  
00948                                                                   ELTRPO  
00949 ************************************************************      ELTRPO  
00950 *                                                          *      ELTRPO  
00951 *        GENERATE SUPPLEMENTAL TEXT                        *      ELTRPO  
00952 *                                                          *      ELTRPO  
00953 ************************************************************      ELTRPO  
00954  GENERATE-SUPPLEMENTAL-TEXT.                                      ELTRPO  
00955      PERFORM EJECT-NEW-PAGE.                                      ELTRPO  
00956      PERFORM TRANSLATE-AND-DISPLAY-RPO-IND.                       ELTRPO  
00957      IF GSS-RP-APPROVAL-SRC-IND (GSS-INDEX) NOT EQUAL             ELTRPO  
00958          SPACES                                                   ELTRPO  
00959                 AND ZEROES AND LOW-VALUES                         ELTRPO  
00960          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTRPO  
00961      PERFORM TRANSLATE-MM-IND.                                    ELTRPO  
00962      IF GSS-RP-MM-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTRPO  
00963         AND SPACES AND LOW-VALUES                                 ELTRPO  
00964            PERFORM TRANSLATE-MM-PAYMENT-LEVEL-IND.                ELTRPO  
00965      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTRPO  
00966      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTRPO  
00967      IF GSS-RP-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTRPO  
00968          ZEROES                                                   ELTRPO  
00969                 AND LOW-VALUES                                    ELTRPO  
00970          PERFORM TRANSLATE-MM-CALC-METHOD.                        ELTRPO  
00971      IF GSS-RP-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTRPO  
00972          SPACES                                                   ELTRPO  
00973                 AND ZEROES AND LOW-VALUES                         ELTRPO  
00974          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTRPO  
00975      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTRPO  
00976      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTRPO  
00977                                                                   ELTRPO  
00978                                                                   ELTRPO  
00979 ************************************************************      ELTRPO  
00980 *                                                          *      ELTRPO  
00981 *        GENERATE SUPPLEMENTAL NOT APPLICABLE              *      ELTRPO  
00982 *                                                          *      ELTRPO  
00983 ************************************************************      ELTRPO  
00984  GENERATE-SUPPLEMENTAL-NOT-APPL.                                  ELTRPO  
00985      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTRPO  
00986      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTRPO  
00987      MOVE WS-NOT-APPLY-TO-LOB-MM TO COF-DTL-LINE                  ELTRPO  
00988          (COF-NBR-DTL-LINES).                                     ELTRPO  
00989      PERFORM EJECT-NEW-PAGE.                                      ELTRPO  
00990      EJECT                                                        ELTRPO  
00991                                                                   ELTRPO  
00992 ************************************************************      ELTRPO  
00993 *                                                          *      ELTRPO  
00994 *        TRANSLATE MM IND                                  *      ELTRPO  
00995 *                                                          *      ELTRPO  
00996 ************************************************************      ELTRPO  
00997  TRANSLATE-MM-IND.                                                ELTRPO  
00998      INITIALIZE TCAR-FROM-AREA.                                   ELTRPO  
00999      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
01000      SET BLANK-LINE-NEEDED TO TRUE.                               ELTRPO  
01001      SET PERIOD-NEEDED TO TRUE.                                   ELTRPO  
01002      MOVE GSS-RP-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTRPO  
01003      MOVE   'RP-MM-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTRPO  
01004      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTRPO  
01005      EJECT                                                        ELTRPO  
01006                                                                   ELTRPO  
01007 ************************************************************      ELTRPO  
01008 *                                                          *      ELTRPO  
01009 *        TRANSLATE MM PAYMENT LEVEL INDICATOR              *      ELTRPO  
01010 *                                                          *      ELTRPO  
01011 ************************************************************      ELTRPO  
01012  TRANSLATE-MM-PAYMENT-LEVEL-IND.                                  ELTRPO  
01013      MOVE 'RP-MM-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTRPO  
01014      MOVE GSS-RP-MM-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTRPO  
01015         CMF-CODE-VALUE.                                           ELTRPO  
01016      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTRPO  
01017      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTRPO  
01018                                                                   ELTRPO  
01019 ************************************************************      ELTRPO  
01020 *                                                          *      ELTRPO  
01021 *        TRANSLATE MM CALC METHOD                          *      ELTRPO  
01022 *                                                          *      ELTRPO  
01023 ************************************************************      ELTRPO  
01024  TRANSLATE-MM-CALC-METHOD.                                        ELTRPO  
01025      INITIALIZE TCAR-FROM-AREA.                                   ELTRPO  
01026      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
01027      SET BLANK-LINE-NEEDED TO TRUE.                               ELTRPO  
01028      SET PERIOD-NEEDED TO TRUE.                                   ELTRPO  
01029      MOVE GSS-RP-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTRPO  
01030      MOVE   'RP-MM-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTRPO  
01031      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTRPO  
01032      EJECT                                                        ELTRPO  
01033                                                                   ELTRPO  
01034                                                                   ELTRPO  
01035 ************************************************************      ELTRPO  
01036 *                                                          *      ELTRPO  
01037 *        GENERATE SPILL OVER TEXT                          *      ELTRPO  
01038 *                                                          *      ELTRPO  
01039 ************************************************************      ELTRPO  
01040  GENERATE-SPILL-OVER-TEXT.                                        ELTRPO  
01041      INITIALIZE TCAR-FROM-AREA.                                   ELTRPO  
01042      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
01043      MOVE WS-SPILL-OVER-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTRPO  
01044      ADD  +1 TO TCAR-FROM-SUB.                                    ELTRPO  
01045      SET BLANK-LINE-NEEDED TO TRUE.                               ELTRPO  
01046      SET PERIOD-NEEDED TO TRUE.                                   ELTRPO  
01047      MOVE GSS-RP-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTRPO  
01048      MOVE 'RP-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTRPO  
01049      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTRPO  
01050      EJECT                                                        ELTRPO  
01051                                                                   ELTRPO  
01052 ************************************************************      ELTRPO  
01053 *                                                          *      ELTRPO  
01054 *        GENERATE SPECIAL PROVIDERS SENTENCE               *      ELTRPO  
01055 *                                                          *      ELTRPO  
01056 ************************************************************      ELTRPO  
01057  GENERATE-SPECIAL-PROVIDERS-SEN.                                  ELTRPO  
01058      SET GCG-INDEX TO +1.                                         ELTRPO  
01059      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTRPO  
01060         AT END                                                    ELTRPO  
01061              MOVE ZEROES TO WS-GRPO-PROV-SLOT-NO                  ELTRPO  
01062         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GRPO                     ELTRPO  
01063              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTRPO  
01064                  TO WS-GRPO-PROV-SLOT-NO                          ELTRPO  
01065         END-SEARCH.                                               ELTRPO  
01066      IF WS-GRPO-PROV-SLOT-NO NOT EQUAL ZEROES                     ELTRPO  
01067          PERFORM DISPLAY-SPECIAL-PROVIDERS-SENT                   ELTRPO  
01068          PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                  ELTRPO  
01069                                                                   ELTRPO  
01070 ************************************************************      ELTRPO  
01071 *                                                          *      ELTRPO  
01072 *        DISPLAY SPECIAL PROVIDERS SENTENCE                *      ELTRPO  
01073 *                                                          *      ELTRPO  
01074 ************************************************************      ELTRPO  
01075  DISPLAY-SPECIAL-PROVIDERS-SENT.                                  ELTRPO  
01076      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTRPO  
01077      MOVE WS-SPEC-PROV-MSG  TO COF-DTL-LINE                       ELTRPO  
01078          (COF-NBR-DTL-LINES).                                     ELTRPO  
01079      PERFORM LINK-TO-OUTPUT.                                      ELTRPO  
01080      EJECT                                                        ELTRPO  
01081                                                                   ELTRPO  
01082 ************************************************************      ELTRPO  
01083 *                                                          *      ELTRPO  
01084 *        SEARCH THE GSS ENTRY                              *      ELTRPO  
01085 *                                                          *      ELTRPO  
01086 ************************************************************      ELTRPO  
01087  SEARCH-THE-GSS-ENTRY.                                            ELTRPO  
01088      SET GSS-INDEX TO 1.                                          ELTRPO  
01089      SEARCH GSS-ENTRY                                             ELTRPO  
01090          AT END                                                   ELTRPO  
01091               SET TABULAR-IS-UNDEFINED TO TRUE                    ELTRPO  
01092          WHEN GSS-RP-PROG-CODE-CHR (GSS-INDEX)                    ELTRPO  
01093               CONTINUE                                            ELTRPO  
01094         END-SEARCH.                                               ELTRPO  
01095      IF TABULAR-IS-UNDEFINED                                      ELTRPO  
01096          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTRPO  
01097                                                                   ELTRPO  
01098 ************************************************************      ELTRPO  
01099 *                                                          *      ELTRPO  
01100 *        CALL CODES MANUAL INTERFACE                       *      ELTRPO  
01101 *                                                          *      ELTRPO  
01102 ************************************************************      ELTRPO  
01103  CALL-CODES-MANUAL-INTERFACE.                                     ELTRPO  
01104      MOVE WS-GCCP TO CMF-RECORD-PREFIX.                           ELTRPO  
01105      EXEC CICS LINK                                               ELTRPO  
01106                PROGRAM ('ELUCMIF')                                ELTRPO  
01107                COMMAREA (DFHCOMMAREA)                             ELTRPO  
01108        END-EXEC.                                                  ELTRPO  
01109      EJECT                                                        ELTRPO  
01110                                                                   ELTRPO  
01111 ************************************************************      ELTRPO  
01112 *                                                          *      ELTRPO  
01113 *        GET GCCP TABULAR                                  *      ELTRPO  
01114 *                                                          *      ELTRPO  
01115 ************************************************************      ELTRPO  
01116  GET-GCCP-TABULAR.                                                ELTRPO  
01117      PERFORM ESTABLISH-ADDRESSABILITY-OF-GC.                      ELTRPO  
01118      SET IOP-RD              TO TRUE.                             ELTRPO  
01119      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELTRPO  
01120      SET IOP-FCQ-NONE        TO TRUE.                             ELTRPO  
01121      SET IOP-KVQ-EQ          TO TRUE.                             ELTRPO  
01122      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTRPO  
01123      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELTRPO  
01124      IF IOP-RC-OK                                                 ELTRPO  
01125          PERFORM ESTABLISH-ADDRESSY-OF-GCCP-TAB                   ELTRPO  
01126      ELSE IF IOP-RC-NOTFND                                        ELTRPO  
01127          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR                     ELTRPO  
01128      ELSE                                                         ELTRPO  
01129          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTRPO  
01130      EJECT                                                        ELTRPO  
01131                                                                   ELTRPO  
01132 ************************************************************      ELTRPO  
01133 *                                                          *      ELTRPO  
01134 *        ESTABLISH ADDRESSABILITY OF GCTABULAR IO PARAMETER*      ELTRPO  
01135 *                                                          *      ELTRPO  
01136 ************************************************************      ELTRPO  
01137  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTRPO  
01138      SET CIA-GCTABULR-DDN TO TRUE.                                ELTRPO  
01139      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPO  
01140                            ADDRESS OF                             ELTRPO  
01141          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTRPO  
01142      EJECT                                                        ELTRPO  
01143                                                                   ELTRPO  
01144 ************************************************************      ELTRPO  
01145 *                                                          *      ELTRPO  
01146 *        CALL INPUT OUTPUT MODULE                          *      ELTRPO  
01147 *                                                          *      ELTRPO  
01148 ************************************************************      ELTRPO  
01149  CALL-INPUT-OUTPUT-MODULE.                                        ELTRPO  
01150      EXEC CICS LINK                                               ELTRPO  
01151                PROGRAM ('ELUIOPGM')                               ELTRPO  
01152                COMMAREA (DFHCOMMAREA)                             ELTRPO  
01153        END-EXEC.                                                  ELTRPO  
01154      EJECT                                                        ELTRPO  
01155                                                                   ELTRPO  
01156 ************************************************************      ELTRPO  
01157 *                                                          *      ELTRPO  
01158 *        ESTABLISH ADDRESSY OF GCCP TABULAR                *      ELTRPO  
01159 *                                                          *      ELTRPO  
01160 ************************************************************      ELTRPO  
01161  ESTABLISH-ADDRESSY-OF-GCCP-TAB.                                  ELTRPO  
01162      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTRPO  
01163          IOP-REC-PTR.                                             ELTRPO  
01164      SET IOP-REC-PTR TO NULL.                                     ELTRPO  
01165      EJECT                                                        ELTRPO  
01166                                                                   ELTRPO  
01167 ************************************************************      ELTRPO  
01168 *                                                          *      ELTRPO  
01169 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELTRPO  
01170 *                                                          *      ELTRPO  
01171 ************************************************************      ELTRPO  
01172  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELTRPO  
01173      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTRPO  
01174      PERFORM SIGNAL-ABEND.                                        ELTRPO  
01175      EJECT                                                        ELTRPO  
01176                                                                   ELTRPO  
01177 ************************************************************      ELTRPO  
01178 *                                                          *      ELTRPO  
01179 *        SIGNAL CRITICAL IO ERROR                          *      ELTRPO  
01180 *                                                          *      ELTRPO  
01181 ************************************************************      ELTRPO  
01182  SIGNAL-CRITICAL-IO-ERROR.                                        ELTRPO  
01183      SET CIA-AB-CRITIO TO TRUE.                                   ELTRPO  
01184      PERFORM SIGNAL-ABEND.                                        ELTRPO  
01185      EJECT                                                        ELTRPO  
01186                                                                   ELTRPO  
01187 ************************************************************      ELTRPO  
01188 *                                                          *      ELTRPO  
01189 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTRPO  
01190 *                                                          *      ELTRPO  
01191 ************************************************************      ELTRPO  
01192  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTRPO  
01193      MOVE 'RESTRICTED PROVIDER OPTION' TO SRP-CCP-NAME.           ELTRPO  
01194      MOVE  WS-GRPO                        TO SRP-TABULAR-ID.      ELTRPO  
01195      MOVE  WS-GRPO-PROV-SLOT-NO           TO SRP-TABULAR-SLOT-NO. ELTRPO  
01196      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTRPO  
01197                                                                   ELTRPO  
01198                                                                   ELTRPO  
01199 ************************************************************      ELTRPO  
01200 *                                                          *      ELTRPO  
01201 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTRPO  
01202 *                                                          *      ELTRPO  
01203 ************************************************************      ELTRPO  
01204  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTRPO  
01205      EXEC CICS LINK                                               ELTRPO  
01206                PROGRAM ('ELGGXXC')                                ELTRPO  
01207                COMMAREA (DFHCOMMAREA)                             ELTRPO  
01208         END-EXEC.                                                 ELTRPO  
01209                                                                   ELTRPO  
01210 ************************************************************      ELTRPO  
01211 *                                                          *      ELTRPO  
01212 *        GENERATE COINSURANCE                              *      ELTRPO  
01213 *                                                          *      ELTRPO  
01214 ************************************************************      ELTRPO  
01215  GENERATE-COINSURANCE.                                            ELTRPO  
01216      EXEC CICS LINK                                               ELTRPO  
01217                PROGRAM ('ELGACLCC')                               ELTRPO  
01218                COMMAREA (DFHCOMMAREA)                             ELTRPO  
01219         END-EXEC.                                                 ELTRPO  
01220                                                                   ELTRPO  
01221 ************************************************************      ELTRPO  
01222 *                                                          *      ELTRPO  
01223 *        GENERATE COPAY                                    *      ELTRPO  
01224 *                                                          *      ELTRPO  
01225 ************************************************************      ELTRPO  
01226  GENERATE-COPAY.                                                  ELTRPO  
01227      EXEC CICS LINK                                               ELTRPO  
01228                PROGRAM ('ELGACPCC')                               ELTRPO  
01229                COMMAREA (DFHCOMMAREA)                             ELTRPO  
01230         END-EXEC.                                                 ELTRPO  
01231                                                                   ELTRPO  
01232 ************************************************************      ELTRPO  
01233 *                                                          *      ELTRPO  
01234 *        GENERATE DEDUCTIBLE                               *      ELTRPO  
01235 *                                                          *      ELTRPO  
01236 ************************************************************      ELTRPO  
01237  GENERATE-DEDUCTIBLE.                                             ELTRPO  
01238      EXEC CICS LINK                                               ELTRPO  
01239                PROGRAM ('ELGADLCC')                               ELTRPO  
01240                COMMAREA (DFHCOMMAREA)                             ELTRPO  
01241         END-EXEC.                                                 ELTRPO  
01242                                                                   ELTRPO  
01243 ************************************************************      ELTRPO  
01244 *                                                          *      ELTRPO  
01245 *        GENERATE MAXIMUM                                  *      ELTRPO  
01246 *                                                          *      ELTRPO  
01247 ************************************************************      ELTRPO  
01248  GENERATE-MAXIMUM.                                                ELTRPO  
01249      EXEC CICS LINK                                               ELTRPO  
01250                PROGRAM ('ELGABMCC')                               ELTRPO  
01251                COMMAREA (DFHCOMMAREA)                             ELTRPO  
01252         END-EXEC.                                                 ELTRPO  
01253      EJECT                                                        ELTRPO  
01254                                                                   ELTRPO  
01255 ************************************************************      ELTRPO  
01256 *                                                          *      ELTRPO  
01257 *        GENERATE OUT OF POCKET                            *      ELTRPO  
01258 *                                                          *      ELTRPO  
01259 ************************************************************      ELTRPO  
01260  GENERATE-OUT-OF-POCKET.                                          ELTRPO  
01261      EXEC CICS LINK                                               ELTRPO  
01262                PROGRAM ('ELGAOLCC')                               ELTRPO  
01263                COMMAREA (DFHCOMMAREA)                             ELTRPO  
01264         END-EXEC.                                                 ELTRPO  
01265                                                                   ELTRPO  
01266 ************************************************************      ELTRPO  
01267 *                                                          *      ELTRPO  
01268 *        CALL CBRI INTERFACE                               *      ELTRPO  
01269 *                                                          *      ELTRPO  
01270 ************************************************************      ELTRPO  
01271  CALL-CBRI-INTERFACE.                                             ELTRPO  
01272      CALL 'ELGCBRI' USING DFHEIBLK                                ELTRPO  
01273                           DFHCOMMAREA.                            ELTRPO  
01274                                                                   ELTRPO  
01275 ************************************************************      ELTRPO  
01276 *                                                          *      ELTRPO  
01277 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTRPO  
01278 *                                                          *      ELTRPO  
01279 ************************************************************      ELTRPO  
01280  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTRPO  
01281      PERFORM INITIALIZE-CMOUT.                                    ELTRPO  
01282      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTRPO  
01283      EJECT                                                        ELTRPO  
01284                                                                   ELTRPO  
01285 ************************************************************      ELTRPO  
01286 *                                                          *      ELTRPO  
01287 *        PREPARE TEXT FOR OUTPUT                           *      ELTRPO  
01288 *                                                          *      ELTRPO  
01289 ************************************************************      ELTRPO  
01290  PREPARE-TEXT-FOR-OUTPUT.                                         ELTRPO  
01291      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTRPO  
01292          UNTIL CMF-DESCR-IDX                                      ELTRPO  
01293                                    GREATER THAN                   ELTRPO  
01294              CMF-NBR-DESCR-LINES.                                 ELTRPO  
01295      EJECT                                                        ELTRPO  
01296                                                                   ELTRPO  
01297 ************************************************************      ELTRPO  
01298 *                                                          *      ELTRPO  
01299 *        INITIALIZE CMOUT                                  *      ELTRPO  
01300 *                                                          *      ELTRPO  
01301 ************************************************************      ELTRPO  
01302  INITIALIZE-CMOUT.                                                ELTRPO  
01303      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTRPO  
01304      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPO  
01305          ADDRESS OF CMF-DESCR.                                    ELTRPO  
01306      SET CMF-DESCR-IDX TO 1.                                      ELTRPO  
01307      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTRPO  
01308                                                                   ELTRPO  
01309                                                                   ELTRPO  
01310 ************************************************************      ELTRPO  
01311 *                                                          *      ELTRPO  
01312 *        MOVE CMF TEXT TO OUTPUT                           *      ELTRPO  
01313 *                                                          *      ELTRPO  
01314 ************************************************************      ELTRPO  
01315  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTRPO  
01316      PERFORM MOVE-A-LINE.                                         ELTRPO  
01317      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTRPO  
01318          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTRPO  
01319      IF TCAR-FROM-SUB GREATER THAN 20                             ELTRPO  
01320               OR CMF-DESCR-IDX GREATER THAN                       ELTRPO  
01321          CMF-NBR-DESCR-LINES                                      ELTRPO  
01322          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTRPO  
01323                                                                   ELTRPO  
01324                                                                   ELTRPO  
01325 ************************************************************      ELTRPO  
01326 *                                                          *      ELTRPO  
01327 *        FINISH CODES MANUAL TEXT                          *      ELTRPO  
01328 *                                                          *      ELTRPO  
01329 ************************************************************      ELTRPO  
01330  FINISH-CODES-MANUAL-TEXT.                                        ELTRPO  
01331      SET DONE-PROCESSING TO TRUE.                                 ELTRPO  
01332      IF PERIOD-NEEDED                                             ELTRPO  
01333          PERFORM GET-AND-MOVE-PERIOD.                             ELTRPO  
01334      EJECT                                                        ELTRPO  
01335                                                                   ELTRPO  
01336                                                                   ELTRPO  
01337 ************************************************************      ELTRPO  
01338 *                                                          *      ELTRPO  
01339 *        GET AND MOVE PERIOD                               *      ELTRPO  
01340 *                                                          *      ELTRPO  
01341 ************************************************************      ELTRPO  
01342  GET-AND-MOVE-PERIOD.                                             ELTRPO  
01343      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTRPO  
01344          (TCAR-FROM-SUB).                                         ELTRPO  
01345                                                                   ELTRPO  
01346                                                                   ELTRPO  
01347 ************************************************************      ELTRPO  
01348 *                                                          *      ELTRPO  
01349 *        SAVE LAST LINE                                    *      ELTRPO  
01350 *                                                          *      ELTRPO  
01351 ************************************************************      ELTRPO  
01352  SAVE-LAST-LINE.                                                  ELTRPO  
01353      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
01354      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTRPO  
01355         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTRPO  
01356      ADD 1 TO TCAR-FROM-SUB.                                      ELTRPO  
01357      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTRPO  
01358                                                                   ELTRPO  
01359                                                                   ELTRPO  
01360 ************************************************************      ELTRPO  
01361 *                                                          *      ELTRPO  
01362 *        OUTPUT LAST LINE                                  *      ELTRPO  
01363 *                                                          *      ELTRPO  
01364 ************************************************************      ELTRPO  
01365  OUTPUT-LAST-LINE.                                                ELTRPO  
01366      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTRPO  
01367          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTRPO  
01368      IF BLANK-LINE-NEEDED                                         ELTRPO  
01369          PERFORM CREATE-A-BLANK-LINE.                             ELTRPO  
01370                                                                   ELTRPO  
01371                                                                   ELTRPO  
01372 ************************************************************      ELTRPO  
01373 *                                                          *      ELTRPO  
01374 *        CREATE A BLANK LINE                               *      ELTRPO  
01375 *                                                          *      ELTRPO  
01376 ************************************************************      ELTRPO  
01377  CREATE-A-BLANK-LINE.                                             ELTRPO  
01378      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTRPO  
01379      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTRPO  
01380                                                                   ELTRPO  
01381                                                                   ELTRPO  
01382 ************************************************************      ELTRPO  
01383 *                                                          *      ELTRPO  
01384 *        MOVE A LINE                                       *      ELTRPO  
01385 *                                                          *      ELTRPO  
01386 ************************************************************      ELTRPO  
01387  MOVE-A-LINE.                                                     ELTRPO  
01388      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTRPO  
01389          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTRPO  
01390      SET CMF-DESCR-IDX UP BY 1.                                   ELTRPO  
01391      ADD 1 TO TCAR-FROM-SUB.                                      ELTRPO  
01392      EJECT                                                        ELTRPO  
01393                                                                   ELTRPO  
01394                                                                   ELTRPO  
01395 ************************************************************      ELTRPO  
01396 *                                                          *      ELTRPO  
01397 *        REFORMAT AND WRITE TEXT                           *      ELTRPO  
01398 *                                                          *      ELTRPO  
01399 ************************************************************      ELTRPO  
01400  REFORMAT-AND-WRITE-TEXT.                                         ELTRPO  
01401      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTRPO  
01402      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTRPO  
01403      PERFORM UNSTRING-TEXT.                                       ELTRPO  
01404      MOVE +1 TO TCAR-FROM-SUB.                                    ELTRPO  
01405      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTRPO  
01406      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTRPO  
01407          UNTIL COF-NBR-DTL-LINES GREATER                          ELTRPO  
01408                                   TCAR-OUTPUT-FIELDS-USED -       ELTRPO  
01409              1.                                                   ELTRPO  
01410      PERFORM DISPOSE-OF-LAST-LINE.                                ELTRPO  
01411      PERFORM LINK-TO-OUTPUT.                                      ELTRPO  
01412                                                                   ELTRPO  
01413                                                                   ELTRPO  
01414 ************************************************************      ELTRPO  
01415 *                                                          *      ELTRPO  
01416 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTRPO  
01417 *                                                          *      ELTRPO  
01418 ************************************************************      ELTRPO  
01419  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTRPO  
01420      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTRPO  
01421           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTRPO  
01422      ADD +1 TO TCAR-FROM-SUB.                                     ELTRPO  
01423      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTRPO  
01424      EJECT                                                        ELTRPO  
01425                                                                   ELTRPO  
01426                                                                   ELTRPO  
01427 ************************************************************      ELTRPO  
01428 *                                                          *      ELTRPO  
01429 *        UNSTRING TEXT                                     *      ELTRPO  
01430 *                                                          *      ELTRPO  
01431 ************************************************************      ELTRPO  
01432  UNSTRING-TEXT.                                                   ELTRPO  
01433      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTRPO  
01434      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTRPO  
01435      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTRPO  
01436      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTRPO  
01437      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTRPO  
01438      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTRPO  
01439      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTRPO  
01440      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTRPO  
01441      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTRPO  
01442      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTRPO  
01443      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTRPO  
01444      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTRPO  
01445      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTRPO  
01446      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTRPO  
01447      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTRPO  
01448      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTRPO  
01449      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTRPO  
01450      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTRPO  
01451      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTRPO  
01452      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTRPO  
01453      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTRPO  
01454      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTRPO  
01455      EJECT                                                        ELTRPO  
01456                                                                   ELTRPO  
01457                                                                   ELTRPO  
01458 ************************************************************      ELTRPO  
01459 *                                                          *      ELTRPO  
01460 *        LINK TO OUTPUT                                    *      ELTRPO  
01461 *                                                          *      ELTRPO  
01462 ************************************************************      ELTRPO  
01463  LINK-TO-OUTPUT.                                                  ELTRPO  
01464      EXEC CICS LINK                                               ELTRPO  
01465          PROGRAM ('ELUOUTPT')                                     ELTRPO  
01466          COMMAREA (DFHCOMMAREA)                                   ELTRPO  
01467          END-EXEC.                                                ELTRPO  
01468      EJECT                                                        ELTRPO  
01469                                                                   ELTRPO  
01470                                                                   ELTRPO  
01471 ************************************************************      ELTRPO  
01472 *                                                          *      ELTRPO  
01473 *        DISPOSE OF LAST LINE                              *      ELTRPO  
01474 *                                                          *      ELTRPO  
01475 ************************************************************      ELTRPO  
01476  DISPOSE-OF-LAST-LINE.                                            ELTRPO  
01477      IF NOT ADDITIONAL-TEXT                                       ELTRPO  
01478          PERFORM INITIALIZE-CONTINUED-SW.                         ELTRPO  
01479      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTRPO  
01480          PERFORM SAVE-LAST-LINE                                   ELTRPO  
01481      ELSE                                                         ELTRPO  
01482          PERFORM OUTPUT-LAST-LINE.                                ELTRPO  
01483                                                                   ELTRPO  
01484                                                                   ELTRPO  
01485 ************************************************************      ELTRPO  
01486 *                                                          *      ELTRPO  
01487 *        INITIALIZE CONTINUED SW                           *      ELTRPO  
01488 *                                                          *      ELTRPO  
01489 ************************************************************      ELTRPO  
01490  INITIALIZE-CONTINUED-SW.                                         ELTRPO  
01491      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTRPO  
