00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELTPANET
00003  PROGRAM-ID.         ELTPANET.                                       LV001
00004                                                                   ELTPANET
00005  AUTHOR.             ANNE KEFFER-KING.                            ELTPANET
00006                                                                   ELTPANET
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTPANET
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTPANET
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTPANET
00010                      233 N. MICHIGAN AVE                          ELTPANET
00011                      CHICAGO, ILLINOIS 60601                      ELTPANET
00012                                                                   ELTPANET
00013  DATE-WRITTEN.       19-FEB-1996.                                 ELTPANET
00014                                                                   ELTPANET
00015  DATE-COMPILED.                                                   ELTPANET
00016                                                                   ELTPANET
00017  SECURITY.           COPYRIGHT 1986,                              ELTPANET
00018                      HEALTH CARE SERVICE CORPORATION              ELTPANET
00019      SKIP3                                                        ELTPANET
00020  ENVIRONMENT DIVISION.                                            ELTPANET
00021                                                                   ELTPANET
00022  CONFIGURATION SECTION.                                           ELTPANET
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELTPANET
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELTPANET
00025      EJECT                                                        ELTPANET
00026 ******************************************************************ELTPANET
00027 *                                                                *ELTPANET
00028 *    COPYBOOK:   ELTPANET                                        *ELTPANET
00029 *    DATE:       19-FEB-1996                                     *ELTPANET
00030 *    AUTHOR:     ANNE KEFFER-KING                                *ELTPANET
00031 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTPANET
00032 *                WITH PREFERRED ANCILARY NETWORK PROGRAM.        *ELTPANET
00033 *    NOTES:      X---                                            *ELTPANET
00034 *                                                                *ELTPANET
00035 ******************************************************************ELTPANET
00036 *                                                                *ELTPANET
00037 *                      MAINTENANCE HISTORY                       *ELTPANET
00038 *                                                                *ELTPANET
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELTPANET
00040 * ----- ----------- --- ----- ---------------------------------- *ELTPANET
00041 * 01.00 19-JAN-1996 AKK       CREATED CLONED FROM ELTRPNET.      *ELTPANET
00042 *                                                                *ELTPANET
00043 ******************************************************************ELTPANET
00044                                                                   ELTPANET
00045  DATA DIVISION.                                                   ELTPANET
00046                                                                   ELTPANET
00047  WORKING-STORAGE SECTION.                                         ELTPANET
00048  01  WS-MISC.                                                     ELTPANET
00049      05  WS-BEGIN                      PIC X(26)  VALUE           ELTPANET
00050      '*** ELTPANET WS BEGINS ***'.                                ELTPANET
00051                                                                   ELTPANET
00052  01  PROGRAM-CONSTANTS.                                           ELTPANET
00053      05  WS-GPAN                       PIC X(06)  VALUE '#GPAN '. ELTPANET
00054                                                                   ELTPANET
00055  01  WS-HOLD-AREA.                                                ELTPANET
00056      05  WS-GPAN-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTPANET
00057                                                                   ELTPANET
00058  01  WS-FIXED-TEXT-AREA.                                          ELTPANET
00059 **************************************************************    ELTPANET
00060 ***                   HEADER  LINE                                ELTPANET
00061 **************************************************************    ELTPANET
00062      05  WS-HEADER-LINE.                                          ELTPANET
00063          10  FILLER               PIC X(14) VALUE SPACES.         ELTPANET
00064          10  FILLER               PIC X(49) VALUE                 ELTPANET
00065          'PREFERRED ANCILLARY NETWORK PROGRAM INSTITUTIONAL'.     ELTPANET
00066          10  FILLER               PIC X(16) VALUE SPACES.         ELTPANET
00067                                                                   ELTPANET
00068 **************************************************************    ELTPANET
00069 ** A MESSAGE WHERE THE GROUP SPECIFIC DOES NOT HAVE PAN.          ELTPANET
00070 **************************************************************    ELTPANET
00071      05  WS-NOT-APPLICABLE-MSG.                                   ELTPANET
00072          10  FILLER               PIC X(79) VALUE                 ELTPANET
00073      'THE PREFERRED ANCILLARY NETWORK PROGRAM IS NOT APPLICABLE.'.ELTPANET
00074                                                                   ELTPANET
00075  LINKAGE SECTION.                                                 ELTPANET
00076  01  DFHCOMMAREA.                                                 ELTPANET
00077      COPY ELSCOMMC.                                               ELTPANET
00078 /                                                                 ELTPANET
00079      COPY ELSCIA2C.                                               ELTPANET
00080 /                                                                 ELTPANET
00081      COPY ELSCMDSC.                                               ELTPANET
00082 /                                                                 ELTPANET
00083      COPY ELSCMIFC.                                               ELTPANET
00084 /                                                                 ELTPANET
00085      COPY ELSIOPMC.                                               ELTPANET
00086 /                                                                 ELTPANET
00087      COPY ELSKEYSC.                                               ELTPANET
00088 /                                                                 ELTPANET
00089      COPY ELSOUTPC.                                               ELTPANET
00090 /                                                                 ELTPANET
00091      COPY ELSSRTPC.                                               ELTPANET
00092 /                                                                 ELTPANET
00093      COPY ELSTCWAC.                                               ELTPANET
00094 /                                                                 ELTPANET
00095      COPY ELSSSCBC.                                               ELTPANET
00096 /                                                                 ELTPANET
00097  01  GROUP-SPECIFIC-REC.                                          ELTPANET
00098      COPY GCGROUPC.                                               ELTPANET
00099 /                                                                 ELTPANET
00100      EJECT                                                        ELTPANET
00101  PROCEDURE DIVISION.                                              ELTPANET
00102 ************************************************************      ELTPANET
00103 *                                                          *      ELTPANET
00104 *                    PROCEDURE DIVISION                    *      ELTPANET
00105 *                                                          *      ELTPANET
00106 ************************************************************      ELTPANET
00107                                                                   ELTPANET
00108                                                                   ELTPANET
00109 ************************************************************      ELTPANET
00110 *                                                          *      ELTPANET
00111 *        PREFERRED ANCILLARY NETWORK PROGRAM               *      ELTPANET
00112 *                                                          *      ELTPANET
00113 ************************************************************      ELTPANET
00114  PREFERRED-ANCIALLRY-NETWORK.                                     ELTPANET
00115      PERFORM INITIALIZATION.                                      ELTPANET
00116      PERFORM PROCESS.                                             ELTPANET
00117      GOBACK.                                                      ELTPANET
00118                                                                   ELTPANET
00119                                                                   ELTPANET
00120 ************************************************************      ELTPANET
00121 *                                                          *      ELTPANET
00122 *        INITIALIZATION.                                   *      ELTPANET
00123 *                                                          *      ELTPANET
00124 ************************************************************      ELTPANET
00125  INITIALIZATION.                                                  ELTPANET
00126      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTPANET
00127      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTPANET
00128                                                                   ELTPANET
00129                                                                   ELTPANET
00130 ************************************************************      ELTPANET
00131 *                                                          *      ELTPANET
00132 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTPANET
00133 *                                                          *      ELTPANET
00134 ************************************************************      ELTPANET
00135  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTPANET
00136      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTPANET
00137      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTPANET
00138      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTPANET
00139                                                                   ELTPANET
00140                                                                   ELTPANET
00141 ************************************************************      ELTPANET
00142 *                                                          *      ELTPANET
00143 *        CHECK FOR VALID COMMAREA                          *      ELTPANET
00144 *                                                          *      ELTPANET
00145 ************************************************************      ELTPANET
00146  CHECK-FOR-VALID-COMMAREA.                                        ELTPANET
00147      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTPANET
00148          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTPANET
00149                                                                   ELTPANET
00150                                                                   ELTPANET
00151 ************************************************************      ELTPANET
00152 *                                                          *      ELTPANET
00153 *        SIGNAL INVALID COMMAREA                           *      ELTPANET
00154 *                                                          *      ELTPANET
00155 ************************************************************      ELTPANET
00156  SIGNAL-INVALID-COMMAREA.                                         ELTPANET
00157      EXEC CICS ABEND                                              ELTPANET
00158                ABCODE('EL01')                                     ELTPANET
00159         END-EXEC.                                                 ELTPANET
00160      EJECT                                                        ELTPANET
00161                                                                   ELTPANET
00162                                                                   ELTPANET
00163 ************************************************************      ELTPANET
00164 *                                                          *      ELTPANET
00165 *        ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA *      ELTPANET
00166 *                                                          *      ELTPANET
00167 ************************************************************      ELTPANET
00168  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTPANET
00169      IF ECA-CIA-PTR = NULL                                        ELTPANET
00170          PERFORM SIGNAL-INVALID-CIA                               ELTPANET
00171      ELSE                                                         ELTPANET
00172          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTPANET
00173                                                                   ELTPANET
00174                                                                   ELTPANET
00175 ************************************************************      ELTPANET
00176 *                                                          *      ELTPANET
00177 *        SIGNAL INVALID CIA                                *      ELTPANET
00178 *                                                          *      ELTPANET
00179 ************************************************************      ELTPANET
00180  SIGNAL-INVALID-CIA.                                              ELTPANET
00181      EXEC CICS ABEND                                              ELTPANET
00182                ABCODE('EL02')                                     ELTPANET
00183         END-EXEC.                                                 ELTPANET
00184      EJECT                                                        ELTPANET
00185                                                                   ELTPANET
00186                                                                   ELTPANET
00187 ************************************************************      ELTPANET
00188 *                                                          *      ELTPANET
00189 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTPANET
00190 *                                                          *      ELTPANET
00191 ************************************************************      ELTPANET
00192  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTPANET
00193      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTPANET
00194      IF CIA-RC-PTR-NULL                                           ELTPANET
00195          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPANET
00196      ELSE                                                         ELTPANET
00197          PERFORM ESTABLISH-ADDRESS-OF-SSCB.                       ELTPANET
00198                                                                   ELTPANET
00199                                                                   ELTPANET
00200 ************************************************************      ELTPANET
00201 *                                                          *      ELTPANET
00202 *        SIGNAL UNALLOC AREA ERROR                         *      ELTPANET
00203 *                                                          *      ELTPANET
00204 ************************************************************      ELTPANET
00205  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTPANET
00206      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTPANET
00207      PERFORM SIGNAL-ABEND.                                        ELTPANET
00208                                                                   ELTPANET
00209                                                                   ELTPANET
00210 ************************************************************      ELTPANET
00211 *                                                          *      ELTPANET
00212 *        SIGNAL ABEND                                      *      ELTPANET
00213 *                                                          *      ELTPANET
00214 ************************************************************      ELTPANET
00215  SIGNAL-ABEND.                                                    ELTPANET
00216      EXEC CICS ABEND                                              ELTPANET
00217                ABCODE(CIA-ABCODE)                                 ELTPANET
00218         END-EXEC.                                                 ELTPANET
00219      EJECT                                                        ELTPANET
00220                                                                   ELTPANET
00221 ************************************************************      ELTPANET
00222 *                                                          *      ELTPANET
00223 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTPANET
00224 *                                                          *      ELTPANET
00225 ************************************************************      ELTPANET
00226  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTPANET
00227      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTPANET
00228      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTPANET
00229      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTPANET
00230      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTPANET
00231      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTPANET
00232      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTPANET
00233                                                                   ELTPANET
00234 ************************************************************      ELTPANET
00235 *                                                          *      ELTPANET
00236 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTPANET
00237 *                                                          *      ELTPANET
00238 ************************************************************      ELTPANET
00239  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTPANET
00240      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTPANET
00241      IF CIA-RC-PTR-NULL                                           ELTPANET
00242          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPANET
00243      ELSE                                                         ELTPANET
00244          PERFORM ESTABLISH-ADDRESS-OF-CMIF.                       ELTPANET
00245      EJECT                                                        ELTPANET
00246                                                                   ELTPANET
00247                                                                   ELTPANET
00248 ************************************************************      ELTPANET
00249 *                                                          *      ELTPANET
00250 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTPANET
00251 *                                                          *      ELTPANET
00252 ************************************************************      ELTPANET
00253  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTPANET
00254      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTPANET
00255      IF CIA-RC-PTR-NULL                                           ELTPANET
00256          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPANET
00257      ELSE                                                         ELTPANET
00258          PERFORM ESTABLISH-ADDRESS-OF-OUTP.                       ELTPANET
00259      EJECT                                                        ELTPANET
00260                                                                   ELTPANET
00261                                                                   ELTPANET
00262 ************************************************************      ELTPANET
00263 *                                                          *      ELTPANET
00264 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTPANET
00265 *                                                          *      ELTPANET
00266 ************************************************************      ELTPANET
00267  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTPANET
00268      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTPANET
00269      IF CIA-RC-PTR-NULL                                           ELTPANET
00270          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPANET
00271      ELSE                                                         ELTPANET
00272          PERFORM ESTABLISH-ADDRESS-OF-SRTP.                       ELTPANET
00273      EJECT                                                        ELTPANET
00274                                                                   ELTPANET
00275                                                                   ELTPANET
00276 ************************************************************      ELTPANET
00277 *                                                          *      ELTPANET
00278 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTPANET
00279 *                                                          *      ELTPANET
00280 ************************************************************      ELTPANET
00281  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTPANET
00282      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTPANET
00283      IF CIA-RC-PTR-NULL                                           ELTPANET
00284          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPANET
00285      ELSE                                                         ELTPANET
00286          PERFORM ESTABLISH-ADDRESS-OF-TCWA.                       ELTPANET
00287      EJECT                                                        ELTPANET
00288                                                                   ELTPANET
00289                                                                   ELTPANET
00290 ************************************************************      ELTPANET
00291 *                                                          *      ELTPANET
00292 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTPANET
00293 *                                                          *      ELTPANET
00294 ************************************************************      ELTPANET
00295  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTPANET
00296      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTPANET
00297      IF CIA-RC-PTR-NULL                                           ELTPANET
00298          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPANET
00299      ELSE                                                         ELTPANET
00300          PERFORM ESTABLISH-ADDRESS-OF-KEYS.                       ELTPANET
00301      EJECT                                                        ELTPANET
00302                                                                   ELTPANET
00303                                                                   ELTPANET
00304 ************************************************************      ELTPANET
00305 *                                                          *      ELTPANET
00306 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTPANET
00307 *                                                          *      ELTPANET
00308 ************************************************************      ELTPANET
00309  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTPANET
00310      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTPANET
00311      IF CIA-RC-PTR-NULL                                           ELTPANET
00312          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPANET
00313      ELSE                                                         ELTPANET
00314          PERFORM ESTABLISH-ADDRESS-OF-GRPSP.                      ELTPANET
00315                                                                   ELTPANET
00316                                                                   ELTPANET
00317 ************************************************************      ELTPANET
00318 *                                                          *      ELTPANET
00319 *        ESTABLISH ADDRESS OF CIA                          *      ELTPANET
00320 *                                                          *      ELTPANET
00321 ************************************************************      ELTPANET
00322  ESTABLISH-ADDRESS-OF-CIA.                                        ELTPANET
00323      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTPANET
00324          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTPANET
00325                                                                   ELTPANET
00326 ************************************************************      ELTPANET
00327 *                                                          *      ELTPANET
00328 *        ESTABLISH ADDRESS OF SSCB                         *      ELTPANET
00329 *                                                          *      ELTPANET
00330 ************************************************************      ELTPANET
00331  ESTABLISH-ADDRESS-OF-SSCB.                                       ELTPANET
00332      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPANET
00333          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTPANET
00334                                                                   ELTPANET
00335 ************************************************************      ELTPANET
00336 *                                                          *      ELTPANET
00337 *        ESTABLISH ADDRESS OF CMIF                         *      ELTPANET
00338 *                                                          *      ELTPANET
00339 ************************************************************      ELTPANET
00340  ESTABLISH-ADDRESS-OF-CMIF.                                       ELTPANET
00341      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPANET
00342          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTPANET
00343                                                                   ELTPANET
00344 ************************************************************      ELTPANET
00345 *                                                          *      ELTPANET
00346 *        ESTABLISH ADDRESS OF OUTP                         *      ELTPANET
00347 *                                                          *      ELTPANET
00348 ************************************************************      ELTPANET
00349  ESTABLISH-ADDRESS-OF-OUTP.                                       ELTPANET
00350      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPANET
00351          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTPANET
00352                                                                   ELTPANET
00353                                                                   ELTPANET
00354 ************************************************************      ELTPANET
00355 *                                                          *      ELTPANET
00356 *        ESTABLISH ADDRESS OF SRTP                         *      ELTPANET
00357 *                                                          *      ELTPANET
00358 ************************************************************      ELTPANET
00359  ESTABLISH-ADDRESS-OF-SRTP.                                       ELTPANET
00360      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPANET
00361          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELTPANET
00362                                                                   ELTPANET
00363                                                                   ELTPANET
00364 ************************************************************      ELTPANET
00365 *                                                          *      ELTPANET
00366 *        ESTABLISH ADDRESS OF TCWA                         *      ELTPANET
00367 *                                                          *      ELTPANET
00368 ************************************************************      ELTPANET
00369  ESTABLISH-ADDRESS-OF-TCWA.                                       ELTPANET
00370      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPANET
00371              ADDRESS OF TCAR-COMPRESSION-WORK-AREA.               ELTPANET
00372                                                                   ELTPANET
00373 ************************************************************      ELTPANET
00374 *                                                          *      ELTPANET
00375 *        ESTABLISH ADDRESS OF KEYS                         *      ELTPANET
00376 *                                                          *      ELTPANET
00377 ************************************************************      ELTPANET
00378  ESTABLISH-ADDRESS-OF-KEYS.                                       ELTPANET
00379      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPANET
00380           ADDRESS OF KWA-FILE-KEY-WORK-AREA.                      ELTPANET
00381                                                                   ELTPANET
00382                                                                   ELTPANET
00383 ************************************************************      ELTPANET
00384 *                                                          *      ELTPANET
00385 *        ESTABLISH ADDRESS OF GRPSP                        *      ELTPANET
00386 *                                                          *      ELTPANET
00387 ************************************************************      ELTPANET
00388  ESTABLISH-ADDRESS-OF-GRPSP.                                      ELTPANET
00389      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPANET
00390             ADDRESS OF GROUP-SPECIFIC-REC.                        ELTPANET
00391      EJECT                                                        ELTPANET
00392                                                                   ELTPANET
00393 ************************************************************      ELTPANET
00394 *                                                          *      ELTPANET
00395 *        PROCESS                                           *      ELTPANET
00396 *                                                          *      ELTPANET
00397 ************************************************************      ELTPANET
00398  PROCESS.                                                         ELTPANET
00399      PERFORM EJECT-NEW-PAGE.                                      ELTPANET
00400      IF GCG-PAN-PARTICIPATION-IND = ZERO                          ELTPANET
00401          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTPANET
00402      ELSE                                                         ELTPANET
00403          PERFORM DETERMINE-IF-GPAN-TABULAR-EXIS.                  ELTPANET
00404      PERFORM TERMINATE-OUTPUT.                                    ELTPANET
00405                                                                   ELTPANET
00406                                                                   ELTPANET
00407 ************************************************************      ELTPANET
00408 *                                                          *      ELTPANET
00409 *        EJECT NEW PAGE                                    *      ELTPANET
00410 *                                                          *      ELTPANET
00411 ************************************************************      ELTPANET
00412  EJECT-NEW-PAGE.                                                  ELTPANET
00413      SET COF-NEW-PAGE           TO TRUE.                          ELTPANET
00414      MOVE +0                    TO COF-NBR-DTL-LINES.             ELTPANET
00415      MOVE +2                    TO COF-NBR-HDR-LINES.             ELTPANET
00416      MOVE WS-HEADER-LINE        TO COF-HDR-LINE                   ELTPANET
00417          (COF-NBR-HDR-LINES).                                     ELTPANET
00418      PERFORM LINK-TO-OUTPUT.                                      ELTPANET
00419      EJECT                                                        ELTPANET
00420                                                                   ELTPANET
00421                                                                   ELTPANET
00422 ************************************************************      ELTPANET
00423 *                                                          *      ELTPANET
00424 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTPANET
00425 *                                                          *      ELTPANET
00426 ************************************************************      ELTPANET
00427  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTPANET
00428      MOVE +1                    TO COF-NBR-DTL-LINES.             ELTPANET
00429      MOVE SPACES                TO COF-DTL-LINE                   ELTPANET
00430          (COF-NBR-DTL-LINES).                                     ELTPANET
00431      ADD +1                     TO COF-NBR-DTL-LINES.             ELTPANET
00432      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTPANET
00433          (COF-NBR-DTL-LINES).                                     ELTPANET
00434      PERFORM LINK-TO-OUTPUT.                                      ELTPANET
00435      EJECT                                                        ELTPANET
00436                                                                   ELTPANET
00437                                                                   ELTPANET
00438 ************************************************************      ELTPANET
00439 *                                                          *      ELTPANET
00440 *        DETERMINE IF GPAN TABULAR EXISTS                  *      ELTPANET
00441 *                                                          *      ELTPANET
00442 ************************************************************      ELTPANET
00443  DETERMINE-IF-GPAN-TABULAR-EXIS.                                  ELTPANET
00444      SET GCG-INDEX TO +1.                                         ELTPANET
00445      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPANET
00446         AT END                                                    ELTPANET
00447              MOVE ZEROES TO WS-GPAN-PROV-SLOT-NO                  ELTPANET
00448         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GPAN                     ELTPANET
00449              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTPANET
00450                   TO WS-GPAN-PROV-SLOT-NO                         ELTPANET
00451         END-SEARCH.                                               ELTPANET
00452      PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                      ELTPANET
00453                                                                   ELTPANET
00454                                                                   ELTPANET
00455 ************************************************************      ELTPANET
00456 *                                                          *      ELTPANET
00457 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTPANET
00458 *                                                          *      ELTPANET
00459 ************************************************************      ELTPANET
00460  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTPANET
00461      MOVE 'PREFERRED ANCILLARY NETWORK'       TO                  ELTPANET
00462          SRP-CCP-NAME.                                            ELTPANET
00463      MOVE  WS-GPAN                           TO SRP-TABULAR-ID.   ELTPANET
00464      MOVE  WS-GPAN-PROV-SLOT-NO              TO                   ELTPANET
00465          SRP-TABULAR-SLOT-NO.                                     ELTPANET
00466      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTPANET
00467                                                                   ELTPANET
00468                                                                   ELTPANET
00469 ************************************************************      ELTPANET
00470 *                                                          *      ELTPANET
00471 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTPANET
00472 *                                                          *      ELTPANET
00473 ************************************************************      ELTPANET
00474  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTPANET
00475      EXEC CICS LINK                                               ELTPANET
00476                PROGRAM ('ELGGXXC')                                ELTPANET
00477                COMMAREA (DFHCOMMAREA)                             ELTPANET
00478         END-EXEC.                                                 ELTPANET
00479                                                                   ELTPANET
00480                                                                   ELTPANET
00481 ************************************************************      ELTPANET
00482 *                                                          *      ELTPANET
00483 *        LINK TO OUTPUT                                    *      ELTPANET
00484 *                                                          *      ELTPANET
00485 ************************************************************      ELTPANET
00486  LINK-TO-OUTPUT.                                                  ELTPANET
00487      EXEC CICS LINK                                               ELTPANET
00488                PROGRAM ('ELUOUTPT')                               ELTPANET
00489                COMMAREA (DFHCOMMAREA)                             ELTPANET
00490         END-EXEC.                                                 ELTPANET
00491      EJECT                                                        ELTPANET
00492                                                                   ELTPANET
00493 ************************************************************      ELTPANET
00494 *                                                          *      ELTPANET
00495 *        TERMINATE OUTPUT                                  *      ELTPANET
00496 *                                                          *      ELTPANET
00497 ************************************************************      ELTPANET
00498  TERMINATE-OUTPUT.                                                ELTPANET
00499      SET COF-END TO TRUE.                                         ELTPANET
00500      PERFORM LINK-TO-OUTPUT.                                      ELTPANET
