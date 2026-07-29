00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELTBANET
00003  PROGRAM-ID.         ELTBANET.                                       LV002
00004                                                                   ELTBANET
00005  AUTHOR.             ANNE KEFFER-KING.                            ELTBANET
00006                                                                   ELTBANET
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTBANET
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTBANET
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTBANET
00010                      233 N. MICHIGAN AVE                          ELTBANET
00011                      CHICAGO, ILLINOIS 60601                      ELTBANET
00012                                                                   ELTBANET
00013  DATE-WRITTEN.       11-JAN-1992.                                 ELTBANET
00014                                                                   ELTBANET
00015  DATE-COMPILED.                                                   ELTBANET
00016                                                                   ELTBANET
00017  SECURITY.           COPYRIGHT 1986,                              ELTBANET
00018                      HEALTH CARE SERVICE CORPORATION              ELTBANET
00019      SKIP3                                                        ELTBANET
00020  ENVIRONMENT DIVISION.                                            ELTBANET
00021                                                                   ELTBANET
00022  CONFIGURATION SECTION.                                           ELTBANET
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELTBANET
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELTBANET
00025      EJECT                                                        ELTBANET
00026 ******************************************************************ELTBANET
00027 *                                                                *ELTBANET
00028 *    COPYBOOK:   ELTBANET                                        *ELTBANET
00029 *    DATE:       11-JAN-1993                                     *ELTBANET
00030 *    AUTHOR:     ANNE KEFFER-KING                                *ELTBANET
00031 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTBANET
00032 *                WITH PARTICIPATING PROVIDER NETWORK PROGRAM.    *ELTBANET
00033 *    NOTES:      X---                                            *ELTBANET
00034 *                                                                *ELTBANET
00035 ******************************************************************ELTBANET
00036 *                                                                *ELTBANET
00037 *                      MAINTENANCE HISTORY                       *ELTBANET
00038 *                                                                *ELTBANET
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELTBANET
00040 * ----- ----------- --- ----- ---------------------------------- *ELTBANET
00041 * 01.00 23-MAR-1999 AKK       CREATED                            *ELTBANET
00042 *                                                                *ELTBANET
00043 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTBANET
00044 *                                                                *ELTBANET
00045 ******************************************************************ELTBANET
00046                                                                   ELTBANET
00047  DATA DIVISION.                                                   ELTBANET
00048                                                                   ELTBANET
00049  WORKING-STORAGE SECTION.                                         ELTBANET
00050  01  WS-MISC.                                                     ELTBANET
00051      05  WS-BEGIN                      PIC X(26)  VALUE           ELTBANET
00052      '*** ELTBANET WS BEGINS ***'.                                ELTBANET
00053                                                                   ELTBANET
00054  01  PROGRAM-CONSTANTS.                                           ELTBANET
00055      05  WS-GBAE                       PIC X(06)  VALUE '#GBAE '. ELTBANET
00056                                                                   ELTBANET
00057  01  WS-HOLD-AREA.                                                ELTBANET
00058      05  WS-GBAE-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTBANET
00059                                                                   ELTBANET
00060  01  WS-FIXED-TEXT-AREA.                                          ELTBANET
00061 **************************************************************    ELTBANET
00062 ***                   HEADER  LINE                                ELTBANET
00063 **************************************************************    ELTBANET
00064      05  WS-HEADER-LINE.                                          ELTBANET
00065          10  FILLER               PIC X(14) VALUE SPACES.         ELTBANET
00066          10  FILLER               PIC X(49) VALUE                 ELTBANET
00067          'BLUE ADVANTAGE ENTRPRENUER NETWORK INSTITUTIONAL'.      ELTBANET
00068          10  FILLER               PIC X(16) VALUE SPACES.         ELTBANET
00069                                                                   ELTBANET
00070 **************************************************************    ELTBANET
00071 ** A MESSAGE WHERE THE GROUP SPECIFIC DOES NOT HAVE BAE.          ELTBANET
00072 **************************************************************    ELTBANET
00073      05  WS-NOT-APPLICABLE-MSG.                                   ELTBANET
00074          10  FILLER               PIC X(79) VALUE                 ELTBANET
00075       'THE BLUE ADVANTAGE ENTRPRENUER NETWORK IS NOT APPLICABLE.'.ELTBANET
00076                                                                   ELTBANET
00077  LINKAGE SECTION.                                                 ELTBANET
00078  01  DFHCOMMAREA.                                                 ELTBANET
00079      COPY ELSCOMMC.                                               ELTBANET
00080 /                                                                 ELTBANET
00081      COPY ELSCIA2C.                                               ELTBANET
00082 /                                                                 ELTBANET
00083      COPY ELSCMDSC.                                               ELTBANET
00084 /                                                                 ELTBANET
00085      COPY ELSCMIFC.                                               ELTBANET
00086 /                                                                 ELTBANET
00087      COPY ELSIOPMC.                                               ELTBANET
00088 /                                                                 ELTBANET
00089      COPY ELSKEYSC.                                               ELTBANET
00090 /                                                                 ELTBANET
00091      COPY ELSOUTPC.                                               ELTBANET
00092 /                                                                 ELTBANET
00093      COPY ELSSRTPC.                                               ELTBANET
00094 /                                                                 ELTBANET
00095      COPY ELSTCWAC.                                               ELTBANET
00096 /                                                                 ELTBANET
00097      COPY ELSSSCBC.                                               ELTBANET
00098 /                                                                 ELTBANET
00099  01  GROUP-SPECIFIC-REC.                                          ELTBANET
00100      COPY GCGROUPC.                                               ELTBANET
00101 /                                                                 ELTBANET
00102      EJECT                                                        ELTBANET
00103  PROCEDURE DIVISION.                                              ELTBANET
00104 ************************************************************      ELTBANET
00105 *                                                          *      ELTBANET
00106 *                    PROCEDURE DIVISION                    *      ELTBANET
00107 *                                                          *      ELTBANET
00108 ************************************************************      ELTBANET
00109                                                                   ELTBANET
00110                                                                   ELTBANET
00111 ************************************************************      ELTBANET
00112 *                                                          *      ELTBANET
00113 *        BLUE ADVANTAGE ENTRPRENUER NETWORK                *      ELTBANET
00114 *                                                          *      ELTBANET
00115 ************************************************************      ELTBANET
00116  BLUE-ADVANTAGE-NETWORK.                                          ELTBANET
00117      PERFORM INITIALIZATION.                                      ELTBANET
00118      PERFORM PROCESS.                                             ELTBANET
00119      GOBACK.                                                      ELTBANET
00120                                                                   ELTBANET
00121                                                                   ELTBANET
00122 ************************************************************      ELTBANET
00123 *                                                          *      ELTBANET
00124 *        INITIALIZATION.                                   *      ELTBANET
00125 *                                                          *      ELTBANET
00126 ************************************************************      ELTBANET
00127  INITIALIZATION.                                                  ELTBANET
00128      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTBANET
00129      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTBANET
00130                                                                   ELTBANET
00131                                                                   ELTBANET
00132 ************************************************************      ELTBANET
00133 *                                                          *      ELTBANET
00134 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTBANET
00135 *                                                          *      ELTBANET
00136 ************************************************************      ELTBANET
00137  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTBANET
00138      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTBANET
00139      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTBANET
00140      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTBANET
00141                                                                   ELTBANET
00142                                                                   ELTBANET
00143 ************************************************************      ELTBANET
00144 *                                                          *      ELTBANET
00145 *        CHECK FOR VALID COMMAREA                          *      ELTBANET
00146 *                                                          *      ELTBANET
00147 ************************************************************      ELTBANET
00148  CHECK-FOR-VALID-COMMAREA.                                        ELTBANET
00149      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTBANET
00150          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTBANET
00151                                                                   ELTBANET
00152                                                                   ELTBANET
00153 ************************************************************      ELTBANET
00154 *                                                          *      ELTBANET
00155 *        SIGNAL INVALID COMMAREA                           *      ELTBANET
00156 *                                                          *      ELTBANET
00157 ************************************************************      ELTBANET
00158  SIGNAL-INVALID-COMMAREA.                                         ELTBANET
00159      EXEC CICS ABEND                                              ELTBANET
00160                ABCODE('EL01')                                     ELTBANET
00161         END-EXEC.                                                 ELTBANET
00162      EJECT                                                        ELTBANET
00163                                                                   ELTBANET
00164                                                                   ELTBANET
00165 ************************************************************      ELTBANET
00166 *                                                          *      ELTBANET
00167 *        ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA *      ELTBANET
00168 *                                                          *      ELTBANET
00169 ************************************************************      ELTBANET
00170  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTBANET
00171      IF ECA-CIA-PTR = NULL                                        ELTBANET
00172          PERFORM SIGNAL-INVALID-CIA                               ELTBANET
00173      ELSE                                                         ELTBANET
00174          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTBANET
00175                                                                   ELTBANET
00176                                                                   ELTBANET
00177 ************************************************************      ELTBANET
00178 *                                                          *      ELTBANET
00179 *        SIGNAL INVALID CIA                                *      ELTBANET
00180 *                                                          *      ELTBANET
00181 ************************************************************      ELTBANET
00182  SIGNAL-INVALID-CIA.                                              ELTBANET
00183      EXEC CICS ABEND                                              ELTBANET
00184                ABCODE('EL02')                                     ELTBANET
00185         END-EXEC.                                                 ELTBANET
00186      EJECT                                                        ELTBANET
00187                                                                   ELTBANET
00188                                                                   ELTBANET
00189 ************************************************************      ELTBANET
00190 *                                                          *      ELTBANET
00191 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTBANET
00192 *                                                          *      ELTBANET
00193 ************************************************************      ELTBANET
00194  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTBANET
00195      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTBANET
00196      IF CIA-RC-PTR-NULL                                           ELTBANET
00197          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTBANET
00198      ELSE                                                         ELTBANET
00199          PERFORM ESTABLISH-ADDRESS-OF-SSCB.                       ELTBANET
00200                                                                   ELTBANET
00201                                                                   ELTBANET
00202 ************************************************************      ELTBANET
00203 *                                                          *      ELTBANET
00204 *        SIGNAL UNALLOC AREA ERROR                         *      ELTBANET
00205 *                                                          *      ELTBANET
00206 ************************************************************      ELTBANET
00207  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTBANET
00208      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTBANET
00209      PERFORM SIGNAL-ABEND.                                        ELTBANET
00210                                                                   ELTBANET
00211                                                                   ELTBANET
00212 ************************************************************      ELTBANET
00213 *                                                          *      ELTBANET
00214 *        SIGNAL ABEND                                      *      ELTBANET
00215 *                                                          *      ELTBANET
00216 ************************************************************      ELTBANET
00217  SIGNAL-ABEND.                                                    ELTBANET
00218      EXEC CICS ABEND                                              ELTBANET
00219                ABCODE(CIA-ABCODE)                                 ELTBANET
00220         END-EXEC.                                                 ELTBANET
00221      EJECT                                                        ELTBANET
00222                                                                   ELTBANET
00223 ************************************************************      ELTBANET
00224 *                                                          *      ELTBANET
00225 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTBANET
00226 *                                                          *      ELTBANET
00227 ************************************************************      ELTBANET
00228  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTBANET
00229      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTBANET
00230      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTBANET
00231      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTBANET
00232      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTBANET
00233      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTBANET
00234      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTBANET
00235                                                                   ELTBANET
00236 ************************************************************      ELTBANET
00237 *                                                          *      ELTBANET
00238 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTBANET
00239 *                                                          *      ELTBANET
00240 ************************************************************      ELTBANET
00241  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTBANET
00242      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTBANET
00243      IF CIA-RC-PTR-NULL                                           ELTBANET
00244          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTBANET
00245      ELSE                                                         ELTBANET
00246          PERFORM ESTABLISH-ADDRESS-OF-CMIF.                       ELTBANET
00247      EJECT                                                        ELTBANET
00248                                                                   ELTBANET
00249                                                                   ELTBANET
00250 ************************************************************      ELTBANET
00251 *                                                          *      ELTBANET
00252 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTBANET
00253 *                                                          *      ELTBANET
00254 ************************************************************      ELTBANET
00255  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTBANET
00256      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTBANET
00257      IF CIA-RC-PTR-NULL                                           ELTBANET
00258          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTBANET
00259      ELSE                                                         ELTBANET
00260          PERFORM ESTABLISH-ADDRESS-OF-OUTP.                       ELTBANET
00261      EJECT                                                        ELTBANET
00262                                                                   ELTBANET
00263                                                                   ELTBANET
00264 ************************************************************      ELTBANET
00265 *                                                          *      ELTBANET
00266 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTBANET
00267 *                                                          *      ELTBANET
00268 ************************************************************      ELTBANET
00269  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTBANET
00270      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTBANET
00271      IF CIA-RC-PTR-NULL                                           ELTBANET
00272          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTBANET
00273      ELSE                                                         ELTBANET
00274          PERFORM ESTABLISH-ADDRESS-OF-SRTP.                       ELTBANET
00275      EJECT                                                        ELTBANET
00276                                                                   ELTBANET
00277                                                                   ELTBANET
00278 ************************************************************      ELTBANET
00279 *                                                          *      ELTBANET
00280 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTBANET
00281 *                                                          *      ELTBANET
00282 ************************************************************      ELTBANET
00283  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTBANET
00284      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTBANET
00285      IF CIA-RC-PTR-NULL                                           ELTBANET
00286          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTBANET
00287      ELSE                                                         ELTBANET
00288          PERFORM ESTABLISH-ADDRESS-OF-TCWA.                       ELTBANET
00289      EJECT                                                        ELTBANET
00290                                                                   ELTBANET
00291                                                                   ELTBANET
00292 ************************************************************      ELTBANET
00293 *                                                          *      ELTBANET
00294 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTBANET
00295 *                                                          *      ELTBANET
00296 ************************************************************      ELTBANET
00297  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTBANET
00298      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTBANET
00299      IF CIA-RC-PTR-NULL                                           ELTBANET
00300          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTBANET
00301      ELSE                                                         ELTBANET
00302          PERFORM ESTABLISH-ADDRESS-OF-KEYS.                       ELTBANET
00303      EJECT                                                        ELTBANET
00304                                                                   ELTBANET
00305                                                                   ELTBANET
00306 ************************************************************      ELTBANET
00307 *                                                          *      ELTBANET
00308 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTBANET
00309 *                                                          *      ELTBANET
00310 ************************************************************      ELTBANET
00311  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTBANET
00312      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTBANET
00313      IF CIA-RC-PTR-NULL                                           ELTBANET
00314          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTBANET
00315      ELSE                                                         ELTBANET
00316          PERFORM ESTABLISH-ADDRESS-OF-GRPSP.                      ELTBANET
00317                                                                   ELTBANET
00318                                                                   ELTBANET
00319 ************************************************************      ELTBANET
00320 *                                                          *      ELTBANET
00321 *        ESTABLISH ADDRESS OF CIA                          *      ELTBANET
00322 *                                                          *      ELTBANET
00323 ************************************************************      ELTBANET
00324  ESTABLISH-ADDRESS-OF-CIA.                                        ELTBANET
00325      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTBANET
00326          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTBANET
00327                                                                   ELTBANET
00328 ************************************************************      ELTBANET
00329 *                                                          *      ELTBANET
00330 *        ESTABLISH ADDRESS OF SSCB                         *      ELTBANET
00331 *                                                          *      ELTBANET
00332 ************************************************************      ELTBANET
00333  ESTABLISH-ADDRESS-OF-SSCB.                                       ELTBANET
00334      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBANET
00335          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTBANET
00336                                                                   ELTBANET
00337 ************************************************************      ELTBANET
00338 *                                                          *      ELTBANET
00339 *        ESTABLISH ADDRESS OF CMIF                         *      ELTBANET
00340 *                                                          *      ELTBANET
00341 ************************************************************      ELTBANET
00342  ESTABLISH-ADDRESS-OF-CMIF.                                       ELTBANET
00343      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBANET
00344          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTBANET
00345                                                                   ELTBANET
00346 ************************************************************      ELTBANET
00347 *                                                          *      ELTBANET
00348 *        ESTABLISH ADDRESS OF OUTP                         *      ELTBANET
00349 *                                                          *      ELTBANET
00350 ************************************************************      ELTBANET
00351  ESTABLISH-ADDRESS-OF-OUTP.                                       ELTBANET
00352      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBANET
00353          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTBANET
00354                                                                   ELTBANET
00355                                                                   ELTBANET
00356 ************************************************************      ELTBANET
00357 *                                                          *      ELTBANET
00358 *        ESTABLISH ADDRESS OF SRTP                         *      ELTBANET
00359 *                                                          *      ELTBANET
00360 ************************************************************      ELTBANET
00361  ESTABLISH-ADDRESS-OF-SRTP.                                       ELTBANET
00362      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBANET
00363          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELTBANET
00364                                                                   ELTBANET
00365                                                                   ELTBANET
00366 ************************************************************      ELTBANET
00367 *                                                          *      ELTBANET
00368 *        ESTABLISH ADDRESS OF TCWA                         *      ELTBANET
00369 *                                                          *      ELTBANET
00370 ************************************************************      ELTBANET
00371  ESTABLISH-ADDRESS-OF-TCWA.                                       ELTBANET
00372      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBANET
00373              ADDRESS OF TCAR-COMPRESSION-WORK-AREA.               ELTBANET
00374                                                                   ELTBANET
00375 ************************************************************      ELTBANET
00376 *                                                          *      ELTBANET
00377 *        ESTABLISH ADDRESS OF KEYS                         *      ELTBANET
00378 *                                                          *      ELTBANET
00379 ************************************************************      ELTBANET
00380  ESTABLISH-ADDRESS-OF-KEYS.                                       ELTBANET
00381      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBANET
00382           ADDRESS OF KWA-FILE-KEY-WORK-AREA.                      ELTBANET
00383                                                                   ELTBANET
00384                                                                   ELTBANET
00385 ************************************************************      ELTBANET
00386 *                                                          *      ELTBANET
00387 *        ESTABLISH ADDRESS OF GRPSP                        *      ELTBANET
00388 *                                                          *      ELTBANET
00389 ************************************************************      ELTBANET
00390  ESTABLISH-ADDRESS-OF-GRPSP.                                      ELTBANET
00391      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBANET
00392             ADDRESS OF GROUP-SPECIFIC-REC.                        ELTBANET
00393      EJECT                                                        ELTBANET
00394                                                                   ELTBANET
00395 ************************************************************      ELTBANET
00396 *                                                          *      ELTBANET
00397 *        PROCESS                                           *      ELTBANET
00398 *                                                          *      ELTBANET
00399 ************************************************************      ELTBANET
00400  PROCESS.                                                         ELTBANET
00401      PERFORM EJECT-NEW-PAGE.                                      ELTBANET
00402      IF GCG-BAE-INDICATOR = ZERO                                  ELTBANET
00403          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTBANET
00404      ELSE                                                         ELTBANET
00405          PERFORM DETERMINE-IF-GBAE-TABULAR-EXIS.                  ELTBANET
00406      PERFORM TERMINATE-OUTPUT.                                    ELTBANET
00407                                                                   ELTBANET
00408                                                                   ELTBANET
00409 ************************************************************      ELTBANET
00410 *                                                          *      ELTBANET
00411 *        EJECT NEW PAGE                                    *      ELTBANET
00412 *                                                          *      ELTBANET
00413 ************************************************************      ELTBANET
00414  EJECT-NEW-PAGE.                                                  ELTBANET
00415      SET COF-NEW-PAGE           TO TRUE.                          ELTBANET
00416      MOVE +0                    TO COF-NBR-DTL-LINES.             ELTBANET
00417      MOVE +2                    TO COF-NBR-HDR-LINES.             ELTBANET
00418      MOVE WS-HEADER-LINE        TO COF-HDR-LINE                   ELTBANET
00419          (COF-NBR-HDR-LINES).                                     ELTBANET
00420      PERFORM LINK-TO-OUTPUT.                                      ELTBANET
00421      EJECT                                                        ELTBANET
00422                                                                   ELTBANET
00423                                                                   ELTBANET
00424 ************************************************************      ELTBANET
00425 *                                                          *      ELTBANET
00426 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTBANET
00427 *                                                          *      ELTBANET
00428 ************************************************************      ELTBANET
00429  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTBANET
00430      MOVE +1                    TO COF-NBR-DTL-LINES.             ELTBANET
00431      MOVE SPACES                TO COF-DTL-LINE                   ELTBANET
00432          (COF-NBR-DTL-LINES).                                     ELTBANET
00433      ADD +1                     TO COF-NBR-DTL-LINES.             ELTBANET
00434      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTBANET
00435          (COF-NBR-DTL-LINES).                                     ELTBANET
00436      PERFORM LINK-TO-OUTPUT.                                      ELTBANET
00437      EJECT                                                        ELTBANET
00438                                                                   ELTBANET
00439                                                                   ELTBANET
00440 ************************************************************      ELTBANET
00441 *                                                          *      ELTBANET
00442 *        DETERMINE IF GBAE TABULAR EXISTS                  *      ELTBANET
00443 *                                                          *      ELTBANET
00444 ************************************************************      ELTBANET
00445  DETERMINE-IF-GBAE-TABULAR-EXIS.                                  ELTBANET
00446      SET GCG-INDEX TO +1.                                         ELTBANET
00447      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTBANET
00448         AT END                                                    ELTBANET
00449              MOVE ZEROES TO WS-GBAE-PROV-SLOT-NO                  ELTBANET
00450         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GBAE                     ELTBANET
00451              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTBANET
00452                   TO WS-GBAE-PROV-SLOT-NO                         ELTBANET
00453         END-SEARCH.                                               ELTBANET
00454      PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                      ELTBANET
00455                                                                   ELTBANET
00456                                                                   ELTBANET
00457 ************************************************************      ELTBANET
00458 *                                                          *      ELTBANET
00459 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTBANET
00460 *                                                          *      ELTBANET
00461 ************************************************************      ELTBANET
00462  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTBANET
00463      MOVE 'BLUE ADVANTAGE ENTREPRENUER NETWORK' TO                ELTBANET
00464          SRP-CCP-NAME.                                            ELTBANET
00465      MOVE  WS-GBAE                           TO SRP-TABULAR-ID.   ELTBANET
00466      MOVE  WS-GBAE-PROV-SLOT-NO              TO                   ELTBANET
00467          SRP-TABULAR-SLOT-NO.                                     ELTBANET
00468      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTBANET
00469                                                                   ELTBANET
00470                                                                   ELTBANET
00471 ************************************************************      ELTBANET
00472 *                                                          *      ELTBANET
00473 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTBANET
00474 *                                                          *      ELTBANET
00475 ************************************************************      ELTBANET
00476  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTBANET
00477      EXEC CICS LINK                                               ELTBANET
00478                PROGRAM ('ELGGXXC')                                ELTBANET
00479                COMMAREA (DFHCOMMAREA)                             ELTBANET
00480         END-EXEC.                                                 ELTBANET
00481                                                                   ELTBANET
00482                                                                   ELTBANET
00483 ************************************************************      ELTBANET
00484 *                                                          *      ELTBANET
00485 *        LINK TO OUTPUT                                    *      ELTBANET
00486 *                                                          *      ELTBANET
00487 ************************************************************      ELTBANET
00488  LINK-TO-OUTPUT.                                                  ELTBANET
00489      EXEC CICS LINK                                               ELTBANET
00490                PROGRAM ('ELUOUTPT')                               ELTBANET
00491                COMMAREA (DFHCOMMAREA)                             ELTBANET
00492         END-EXEC.                                                 ELTBANET
00493      EJECT                                                        ELTBANET
00494                                                                   ELTBANET
00495 ************************************************************      ELTBANET
00496 *                                                          *      ELTBANET
00497 *        TERMINATE OUTPUT                                  *      ELTBANET
00498 *                                                          *      ELTBANET
00499 ************************************************************      ELTBANET
00500  TERMINATE-OUTPUT.                                                ELTBANET
00501      SET COF-END TO TRUE.                                         ELTBANET
00502      PERFORM LINK-TO-OUTPUT.                                      ELTBANET
