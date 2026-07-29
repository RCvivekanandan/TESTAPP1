00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELTRPNET
00003  PROGRAM-ID.         ELTRPNET.                                       LV001
00004                                                                   ELTRPNET
00005  AUTHOR.             ANNE KEFFER-KING.                            ELTRPNET
00006                                                                   ELTRPNET
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTRPNET
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTRPNET
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTRPNET
00010                      233 N. MICHIGAN AVE                          ELTRPNET
00011                      CHICAGO, ILLINOIS 60601                      ELTRPNET
00012                                                                   ELTRPNET
00013  DATE-WRITTEN.       11-JAN-1992.                                 ELTRPNET
00014                                                                   ELTRPNET
00015  DATE-COMPILED.                                                   ELTRPNET
00016                                                                   ELTRPNET
00017  SECURITY.           COPYRIGHT 1986,                              ELTRPNET
00018                      HEALTH CARE SERVICE CORPORATION              ELTRPNET
00019      SKIP3                                                        ELTRPNET
00020  ENVIRONMENT DIVISION.                                            ELTRPNET
00021                                                                   ELTRPNET
00022  CONFIGURATION SECTION.                                           ELTRPNET
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELTRPNET
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELTRPNET
00025      EJECT                                                        ELTRPNET
00026 ******************************************************************ELTRPNET
00027 *                                                                *ELTRPNET
00028 *    COPYBOOK:   ELTRPNET                                        *ELTRPNET
00029 *    DATE:       11-JAN-1993                                     *ELTRPNET
00030 *    AUTHOR:     ANNE KEFFER-KING                                *ELTRPNET
00031 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTRPNET
00032 *                WITH PARTICIPATING PROVIDER NETWORK PROGRAM.    *ELTRPNET
00033 *    NOTES:      X---                                            *ELTRPNET
00034 *                                                                *ELTRPNET
00035 ******************************************************************ELTRPNET
00036 *                                                                *ELTRPNET
00037 *                      MAINTENANCE HISTORY                       *ELTRPNET
00038 *                                                                *ELTRPNET
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELTRPNET
00040 * ----- ----------- --- ----- ---------------------------------- *ELTRPNET
00041 * 01.00 11-JAN-1993 AKK       CREATED                            *ELTRPNET
00042 *                                                                *ELTRPNET
00043 ******************************************************************ELTRPNET
00044                                                                   ELTRPNET
00045  DATA DIVISION.                                                   ELTRPNET
00046                                                                   ELTRPNET
00047  WORKING-STORAGE SECTION.                                         ELTRPNET
00048  01  WS-MISC.                                                     ELTRPNET
00049      05  WS-BEGIN                      PIC X(26)  VALUE           ELTRPNET
00050      '*** ELTRPNET WS BEGINS ***'.                                ELTRPNET
00051                                                                   ELTRPNET
00052  01  PROGRAM-CONSTANTS.                                           ELTRPNET
00053      05  WS-GRPO                       PIC X(06)  VALUE '#GRPO '. ELTRPNET
00054                                                                   ELTRPNET
00055  01  WS-HOLD-AREA.                                                ELTRPNET
00056      05  WS-GRPO-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTRPNET
00057                                                                   ELTRPNET
00058  01  WS-FIXED-TEXT-AREA.                                          ELTRPNET
00059 **************************************************************    ELTRPNET
00060 ***                   HEADER  LINE                                ELTRPNET
00061 **************************************************************    ELTRPNET
00062      05  WS-HEADER-LINE.                                          ELTRPNET
00063          10  FILLER               PIC X(14) VALUE SPACES.         ELTRPNET
00064          10  FILLER               PIC X(49) VALUE                 ELTRPNET
00065          'RESTRICTED PROVIDER NETWORK PROGRAM INSTITUTIONAL'.     ELTRPNET
00066          10  FILLER               PIC X(16) VALUE SPACES.         ELTRPNET
00067                                                                   ELTRPNET
00068 **************************************************************    ELTRPNET
00069 ** A MESSAGE WHERE THE GROUP SPECIFIC DOES NOT HAVE RPO.          ELTRPNET
00070 **************************************************************    ELTRPNET
00071      05  WS-NOT-APPLICABLE-MSG.                                   ELTRPNET
00072          10  FILLER               PIC X(79) VALUE                 ELTRPNET
00073       'THE RESTRICTED PROVIDER OPTION PROGRAM IS NOT APPLICABLE.'.ELTRPNET
00074                                                                   ELTRPNET
00075  LINKAGE SECTION.                                                 ELTRPNET
00076  01  DFHCOMMAREA.                                                 ELTRPNET
00077      COPY ELSCOMMC.                                               ELTRPNET
00078 /                                                                 ELTRPNET
00079      COPY ELSCIA2C.                                               ELTRPNET
00080 /                                                                 ELTRPNET
00081      COPY ELSCMDSC.                                               ELTRPNET
00082 /                                                                 ELTRPNET
00083      COPY ELSCMIFC.                                               ELTRPNET
00084 /                                                                 ELTRPNET
00085      COPY ELSIOPMC.                                               ELTRPNET
00086 /                                                                 ELTRPNET
00087      COPY ELSKEYSC.                                               ELTRPNET
00088 /                                                                 ELTRPNET
00089      COPY ELSOUTPC.                                               ELTRPNET
00090 /                                                                 ELTRPNET
00091      COPY ELSSRTPC.                                               ELTRPNET
00092 /                                                                 ELTRPNET
00093      COPY ELSTCWAC.                                               ELTRPNET
00094 /                                                                 ELTRPNET
00095      COPY ELSSSCBC.                                               ELTRPNET
00096 /                                                                 ELTRPNET
00097  01  GROUP-SPECIFIC-REC.                                          ELTRPNET
00098      COPY GCGROUPC.                                               ELTRPNET
00099 /                                                                 ELTRPNET
00100      EJECT                                                        ELTRPNET
00101  PROCEDURE DIVISION.                                              ELTRPNET
00102 ************************************************************      ELTRPNET
00103 *                                                          *      ELTRPNET
00104 *                    PROCEDURE DIVISION                    *      ELTRPNET
00105 *                                                          *      ELTRPNET
00106 ************************************************************      ELTRPNET
00107                                                                   ELTRPNET
00108                                                                   ELTRPNET
00109 ************************************************************      ELTRPNET
00110 *                                                          *      ELTRPNET
00111 *        RESTRICTED PROVIDER OPTION NETWORK                *      ELTRPNET
00112 *                                                          *      ELTRPNET
00113 ************************************************************      ELTRPNET
00114  RESTRICTED-PROVIDER-NETWORK.                                     ELTRPNET
00115      PERFORM INITIALIZATION.                                      ELTRPNET
00116      PERFORM PROCESS.                                             ELTRPNET
00117      GOBACK.                                                      ELTRPNET
00118                                                                   ELTRPNET
00119                                                                   ELTRPNET
00120 ************************************************************      ELTRPNET
00121 *                                                          *      ELTRPNET
00122 *        INITIALIZATION.                                   *      ELTRPNET
00123 *                                                          *      ELTRPNET
00124 ************************************************************      ELTRPNET
00125  INITIALIZATION.                                                  ELTRPNET
00126      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTRPNET
00127      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTRPNET
00128                                                                   ELTRPNET
00129                                                                   ELTRPNET
00130 ************************************************************      ELTRPNET
00131 *                                                          *      ELTRPNET
00132 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTRPNET
00133 *                                                          *      ELTRPNET
00134 ************************************************************      ELTRPNET
00135  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTRPNET
00136      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTRPNET
00137      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTRPNET
00138      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTRPNET
00139                                                                   ELTRPNET
00140                                                                   ELTRPNET
00141 ************************************************************      ELTRPNET
00142 *                                                          *      ELTRPNET
00143 *        CHECK FOR VALID COMMAREA                          *      ELTRPNET
00144 *                                                          *      ELTRPNET
00145 ************************************************************      ELTRPNET
00146  CHECK-FOR-VALID-COMMAREA.                                        ELTRPNET
00147      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTRPNET
00148          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTRPNET
00149                                                                   ELTRPNET
00150                                                                   ELTRPNET
00151 ************************************************************      ELTRPNET
00152 *                                                          *      ELTRPNET
00153 *        SIGNAL INVALID COMMAREA                           *      ELTRPNET
00154 *                                                          *      ELTRPNET
00155 ************************************************************      ELTRPNET
00156  SIGNAL-INVALID-COMMAREA.                                         ELTRPNET
00157      EXEC CICS ABEND                                              ELTRPNET
00158                ABCODE('EL01')                                     ELTRPNET
00159         END-EXEC.                                                 ELTRPNET
00160      EJECT                                                        ELTRPNET
00161                                                                   ELTRPNET
00162                                                                   ELTRPNET
00163 ************************************************************      ELTRPNET
00164 *                                                          *      ELTRPNET
00165 *        ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA *      ELTRPNET
00166 *                                                          *      ELTRPNET
00167 ************************************************************      ELTRPNET
00168  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTRPNET
00169      IF ECA-CIA-PTR = NULL                                        ELTRPNET
00170          PERFORM SIGNAL-INVALID-CIA                               ELTRPNET
00171      ELSE                                                         ELTRPNET
00172          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTRPNET
00173                                                                   ELTRPNET
00174                                                                   ELTRPNET
00175 ************************************************************      ELTRPNET
00176 *                                                          *      ELTRPNET
00177 *        SIGNAL INVALID CIA                                *      ELTRPNET
00178 *                                                          *      ELTRPNET
00179 ************************************************************      ELTRPNET
00180  SIGNAL-INVALID-CIA.                                              ELTRPNET
00181      EXEC CICS ABEND                                              ELTRPNET
00182                ABCODE('EL02')                                     ELTRPNET
00183         END-EXEC.                                                 ELTRPNET
00184      EJECT                                                        ELTRPNET
00185                                                                   ELTRPNET
00186                                                                   ELTRPNET
00187 ************************************************************      ELTRPNET
00188 *                                                          *      ELTRPNET
00189 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTRPNET
00190 *                                                          *      ELTRPNET
00191 ************************************************************      ELTRPNET
00192  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTRPNET
00193      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTRPNET
00194      IF CIA-RC-PTR-NULL                                           ELTRPNET
00195          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTRPNET
00196      ELSE                                                         ELTRPNET
00197          PERFORM ESTABLISH-ADDRESS-OF-SSCB.                       ELTRPNET
00198                                                                   ELTRPNET
00199                                                                   ELTRPNET
00200 ************************************************************      ELTRPNET
00201 *                                                          *      ELTRPNET
00202 *        SIGNAL UNALLOC AREA ERROR                         *      ELTRPNET
00203 *                                                          *      ELTRPNET
00204 ************************************************************      ELTRPNET
00205  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTRPNET
00206      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTRPNET
00207      PERFORM SIGNAL-ABEND.                                        ELTRPNET
00208                                                                   ELTRPNET
00209                                                                   ELTRPNET
00210 ************************************************************      ELTRPNET
00211 *                                                          *      ELTRPNET
00212 *        SIGNAL ABEND                                      *      ELTRPNET
00213 *                                                          *      ELTRPNET
00214 ************************************************************      ELTRPNET
00215  SIGNAL-ABEND.                                                    ELTRPNET
00216      EXEC CICS ABEND                                              ELTRPNET
00217                ABCODE(CIA-ABCODE)                                 ELTRPNET
00218         END-EXEC.                                                 ELTRPNET
00219      EJECT                                                        ELTRPNET
00220                                                                   ELTRPNET
00221 ************************************************************      ELTRPNET
00222 *                                                          *      ELTRPNET
00223 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTRPNET
00224 *                                                          *      ELTRPNET
00225 ************************************************************      ELTRPNET
00226  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTRPNET
00227      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTRPNET
00228      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTRPNET
00229      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTRPNET
00230      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTRPNET
00231      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTRPNET
00232      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTRPNET
00233                                                                   ELTRPNET
00234 ************************************************************      ELTRPNET
00235 *                                                          *      ELTRPNET
00236 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTRPNET
00237 *                                                          *      ELTRPNET
00238 ************************************************************      ELTRPNET
00239  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTRPNET
00240      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTRPNET
00241      IF CIA-RC-PTR-NULL                                           ELTRPNET
00242          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTRPNET
00243      ELSE                                                         ELTRPNET
00244          PERFORM ESTABLISH-ADDRESS-OF-CMIF.                       ELTRPNET
00245      EJECT                                                        ELTRPNET
00246                                                                   ELTRPNET
00247                                                                   ELTRPNET
00248 ************************************************************      ELTRPNET
00249 *                                                          *      ELTRPNET
00250 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTRPNET
00251 *                                                          *      ELTRPNET
00252 ************************************************************      ELTRPNET
00253  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTRPNET
00254      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTRPNET
00255      IF CIA-RC-PTR-NULL                                           ELTRPNET
00256          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTRPNET
00257      ELSE                                                         ELTRPNET
00258          PERFORM ESTABLISH-ADDRESS-OF-OUTP.                       ELTRPNET
00259      EJECT                                                        ELTRPNET
00260                                                                   ELTRPNET
00261                                                                   ELTRPNET
00262 ************************************************************      ELTRPNET
00263 *                                                          *      ELTRPNET
00264 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTRPNET
00265 *                                                          *      ELTRPNET
00266 ************************************************************      ELTRPNET
00267  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTRPNET
00268      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTRPNET
00269      IF CIA-RC-PTR-NULL                                           ELTRPNET
00270          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTRPNET
00271      ELSE                                                         ELTRPNET
00272          PERFORM ESTABLISH-ADDRESS-OF-SRTP.                       ELTRPNET
00273      EJECT                                                        ELTRPNET
00274                                                                   ELTRPNET
00275                                                                   ELTRPNET
00276 ************************************************************      ELTRPNET
00277 *                                                          *      ELTRPNET
00278 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTRPNET
00279 *                                                          *      ELTRPNET
00280 ************************************************************      ELTRPNET
00281  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTRPNET
00282      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTRPNET
00283      IF CIA-RC-PTR-NULL                                           ELTRPNET
00284          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTRPNET
00285      ELSE                                                         ELTRPNET
00286          PERFORM ESTABLISH-ADDRESS-OF-TCWA.                       ELTRPNET
00287      EJECT                                                        ELTRPNET
00288                                                                   ELTRPNET
00289                                                                   ELTRPNET
00290 ************************************************************      ELTRPNET
00291 *                                                          *      ELTRPNET
00292 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTRPNET
00293 *                                                          *      ELTRPNET
00294 ************************************************************      ELTRPNET
00295  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTRPNET
00296      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTRPNET
00297      IF CIA-RC-PTR-NULL                                           ELTRPNET
00298          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTRPNET
00299      ELSE                                                         ELTRPNET
00300          PERFORM ESTABLISH-ADDRESS-OF-KEYS.                       ELTRPNET
00301      EJECT                                                        ELTRPNET
00302                                                                   ELTRPNET
00303                                                                   ELTRPNET
00304 ************************************************************      ELTRPNET
00305 *                                                          *      ELTRPNET
00306 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTRPNET
00307 *                                                          *      ELTRPNET
00308 ************************************************************      ELTRPNET
00309  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTRPNET
00310      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTRPNET
00311      IF CIA-RC-PTR-NULL                                           ELTRPNET
00312          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTRPNET
00313      ELSE                                                         ELTRPNET
00314          PERFORM ESTABLISH-ADDRESS-OF-GRPSP.                      ELTRPNET
00315                                                                   ELTRPNET
00316                                                                   ELTRPNET
00317 ************************************************************      ELTRPNET
00318 *                                                          *      ELTRPNET
00319 *        ESTABLISH ADDRESS OF CIA                          *      ELTRPNET
00320 *                                                          *      ELTRPNET
00321 ************************************************************      ELTRPNET
00322  ESTABLISH-ADDRESS-OF-CIA.                                        ELTRPNET
00323      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTRPNET
00324          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTRPNET
00325                                                                   ELTRPNET
00326 ************************************************************      ELTRPNET
00327 *                                                          *      ELTRPNET
00328 *        ESTABLISH ADDRESS OF SSCB                         *      ELTRPNET
00329 *                                                          *      ELTRPNET
00330 ************************************************************      ELTRPNET
00331  ESTABLISH-ADDRESS-OF-SSCB.                                       ELTRPNET
00332      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPNET
00333          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTRPNET
00334                                                                   ELTRPNET
00335 ************************************************************      ELTRPNET
00336 *                                                          *      ELTRPNET
00337 *        ESTABLISH ADDRESS OF CMIF                         *      ELTRPNET
00338 *                                                          *      ELTRPNET
00339 ************************************************************      ELTRPNET
00340  ESTABLISH-ADDRESS-OF-CMIF.                                       ELTRPNET
00341      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPNET
00342          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTRPNET
00343                                                                   ELTRPNET
00344 ************************************************************      ELTRPNET
00345 *                                                          *      ELTRPNET
00346 *        ESTABLISH ADDRESS OF OUTP                         *      ELTRPNET
00347 *                                                          *      ELTRPNET
00348 ************************************************************      ELTRPNET
00349  ESTABLISH-ADDRESS-OF-OUTP.                                       ELTRPNET
00350      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPNET
00351          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTRPNET
00352                                                                   ELTRPNET
00353                                                                   ELTRPNET
00354 ************************************************************      ELTRPNET
00355 *                                                          *      ELTRPNET
00356 *        ESTABLISH ADDRESS OF SRTP                         *      ELTRPNET
00357 *                                                          *      ELTRPNET
00358 ************************************************************      ELTRPNET
00359  ESTABLISH-ADDRESS-OF-SRTP.                                       ELTRPNET
00360      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPNET
00361          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELTRPNET
00362                                                                   ELTRPNET
00363                                                                   ELTRPNET
00364 ************************************************************      ELTRPNET
00365 *                                                          *      ELTRPNET
00366 *        ESTABLISH ADDRESS OF TCWA                         *      ELTRPNET
00367 *                                                          *      ELTRPNET
00368 ************************************************************      ELTRPNET
00369  ESTABLISH-ADDRESS-OF-TCWA.                                       ELTRPNET
00370      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPNET
00371              ADDRESS OF TCAR-COMPRESSION-WORK-AREA.               ELTRPNET
00372                                                                   ELTRPNET
00373 ************************************************************      ELTRPNET
00374 *                                                          *      ELTRPNET
00375 *        ESTABLISH ADDRESS OF KEYS                         *      ELTRPNET
00376 *                                                          *      ELTRPNET
00377 ************************************************************      ELTRPNET
00378  ESTABLISH-ADDRESS-OF-KEYS.                                       ELTRPNET
00379      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPNET
00380           ADDRESS OF KWA-FILE-KEY-WORK-AREA.                      ELTRPNET
00381                                                                   ELTRPNET
00382                                                                   ELTRPNET
00383 ************************************************************      ELTRPNET
00384 *                                                          *      ELTRPNET
00385 *        ESTABLISH ADDRESS OF GRPSP                        *      ELTRPNET
00386 *                                                          *      ELTRPNET
00387 ************************************************************      ELTRPNET
00388  ESTABLISH-ADDRESS-OF-GRPSP.                                      ELTRPNET
00389      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTRPNET
00390             ADDRESS OF GROUP-SPECIFIC-REC.                        ELTRPNET
00391      EJECT                                                        ELTRPNET
00392                                                                   ELTRPNET
00393 ************************************************************      ELTRPNET
00394 *                                                          *      ELTRPNET
00395 *        PROCESS                                           *      ELTRPNET
00396 *                                                          *      ELTRPNET
00397 ************************************************************      ELTRPNET
00398  PROCESS.                                                         ELTRPNET
00399      PERFORM EJECT-NEW-PAGE.                                      ELTRPNET
00400      IF GCG-RPO-INDICATOR = ZERO                                  ELTRPNET
00401          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTRPNET
00402      ELSE                                                         ELTRPNET
00403          PERFORM DETERMINE-IF-GRPO-TABULAR-EXIS.                  ELTRPNET
00404      PERFORM TERMINATE-OUTPUT.                                    ELTRPNET
00405                                                                   ELTRPNET
00406                                                                   ELTRPNET
00407 ************************************************************      ELTRPNET
00408 *                                                          *      ELTRPNET
00409 *        EJECT NEW PAGE                                    *      ELTRPNET
00410 *                                                          *      ELTRPNET
00411 ************************************************************      ELTRPNET
00412  EJECT-NEW-PAGE.                                                  ELTRPNET
00413      SET COF-NEW-PAGE           TO TRUE.                          ELTRPNET
00414      MOVE +0                    TO COF-NBR-DTL-LINES.             ELTRPNET
00415      MOVE +2                    TO COF-NBR-HDR-LINES.             ELTRPNET
00416      MOVE WS-HEADER-LINE        TO COF-HDR-LINE                   ELTRPNET
00417          (COF-NBR-HDR-LINES).                                     ELTRPNET
00418      PERFORM LINK-TO-OUTPUT.                                      ELTRPNET
00419      EJECT                                                        ELTRPNET
00420                                                                   ELTRPNET
00421                                                                   ELTRPNET
00422 ************************************************************      ELTRPNET
00423 *                                                          *      ELTRPNET
00424 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTRPNET
00425 *                                                          *      ELTRPNET
00426 ************************************************************      ELTRPNET
00427  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTRPNET
00428      MOVE +1                    TO COF-NBR-DTL-LINES.             ELTRPNET
00429      MOVE SPACES                TO COF-DTL-LINE                   ELTRPNET
00430          (COF-NBR-DTL-LINES).                                     ELTRPNET
00431      ADD +1                     TO COF-NBR-DTL-LINES.             ELTRPNET
00432      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTRPNET
00433          (COF-NBR-DTL-LINES).                                     ELTRPNET
00434      PERFORM LINK-TO-OUTPUT.                                      ELTRPNET
00435      EJECT                                                        ELTRPNET
00436                                                                   ELTRPNET
00437                                                                   ELTRPNET
00438 ************************************************************      ELTRPNET
00439 *                                                          *      ELTRPNET
00440 *        DETERMINE IF GRPO TABULAR EXISTS                  *      ELTRPNET
00441 *                                                          *      ELTRPNET
00442 ************************************************************      ELTRPNET
00443  DETERMINE-IF-GRPO-TABULAR-EXIS.                                  ELTRPNET
00444      SET GCG-INDEX TO +1.                                         ELTRPNET
00445      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTRPNET
00446         AT END                                                    ELTRPNET
00447              MOVE ZEROES TO WS-GRPO-PROV-SLOT-NO                  ELTRPNET
00448         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GRPO                     ELTRPNET
00449              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTRPNET
00450                   TO WS-GRPO-PROV-SLOT-NO                         ELTRPNET
00451         END-SEARCH.                                               ELTRPNET
00452      PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                      ELTRPNET
00453                                                                   ELTRPNET
00454                                                                   ELTRPNET
00455 ************************************************************      ELTRPNET
00456 *                                                          *      ELTRPNET
00457 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTRPNET
00458 *                                                          *      ELTRPNET
00459 ************************************************************      ELTRPNET
00460  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTRPNET
00461      MOVE 'RESTRICTED PROVIDER NETWORK'       TO                  ELTRPNET
00462          SRP-CCP-NAME.                                            ELTRPNET
00463      MOVE  WS-GRPO                           TO SRP-TABULAR-ID.   ELTRPNET
00464      MOVE  WS-GRPO-PROV-SLOT-NO              TO                   ELTRPNET
00465          SRP-TABULAR-SLOT-NO.                                     ELTRPNET
00466      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTRPNET
00467                                                                   ELTRPNET
00468                                                                   ELTRPNET
00469 ************************************************************      ELTRPNET
00470 *                                                          *      ELTRPNET
00471 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTRPNET
00472 *                                                          *      ELTRPNET
00473 ************************************************************      ELTRPNET
00474  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTRPNET
00475      EXEC CICS LINK                                               ELTRPNET
00476                PROGRAM ('ELGGXXC')                                ELTRPNET
00477                COMMAREA (DFHCOMMAREA)                             ELTRPNET
00478         END-EXEC.                                                 ELTRPNET
00479                                                                   ELTRPNET
00480                                                                   ELTRPNET
00481 ************************************************************      ELTRPNET
00482 *                                                          *      ELTRPNET
00483 *        LINK TO OUTPUT                                    *      ELTRPNET
00484 *                                                          *      ELTRPNET
00485 ************************************************************      ELTRPNET
00486  LINK-TO-OUTPUT.                                                  ELTRPNET
00487      EXEC CICS LINK                                               ELTRPNET
00488                PROGRAM ('ELUOUTPT')                               ELTRPNET
00489                COMMAREA (DFHCOMMAREA)                             ELTRPNET
00490         END-EXEC.                                                 ELTRPNET
00491      EJECT                                                        ELTRPNET
00492                                                                   ELTRPNET
00493 ************************************************************      ELTRPNET
00494 *                                                          *      ELTRPNET
00495 *        TERMINATE OUTPUT                                  *      ELTRPNET
00496 *                                                          *      ELTRPNET
00497 ************************************************************      ELTRPNET
00498  TERMINATE-OUTPUT.                                                ELTRPNET
00499      SET COF-END TO TRUE.                                         ELTRPNET
00500      PERFORM LINK-TO-OUTPUT.                                      ELTRPNET
