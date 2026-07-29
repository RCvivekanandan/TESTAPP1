00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELTCBNET
00003  PROGRAM-ID.         ELTCBNET.                                       LV001
00004                                                                   ELTCBNET
00005  AUTHOR.             ANNE KEFFER-KING.                            ELTCBNET
00006                                                                   ELTCBNET
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTCBNET
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTCBNET
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTCBNET
00010                      233 N. MICHIGAN AVE                          ELTCBNET
00011                      CHICAGO, ILLINOIS 60601                      ELTCBNET
00012                                                                   ELTCBNET
00013  DATE-WRITTEN.       19-FEB-1996.                                 ELTCBNET
00014                                                                   ELTCBNET
00015  DATE-COMPILED.                                                   ELTCBNET
00016                                                                   ELTCBNET
00017  SECURITY.           COPYRIGHT 1986,                              ELTCBNET
00018                      HEALTH CARE SERVICE CORPORATION              ELTCBNET
00019      SKIP3                                                        ELTCBNET
00020  ENVIRONMENT DIVISION.                                            ELTCBNET
00021                                                                   ELTCBNET
00022  CONFIGURATION SECTION.                                           ELTCBNET
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELTCBNET
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELTCBNET
00025      EJECT                                                        ELTCBNET
00026 ******************************************************************ELTCBNET
00027 *                                                                *ELTCBNET
00028 *    COPYBOOK:   ELTCBNET                                        *ELTCBNET
00029 *    DATE:       19-JAN-1996                                     *ELTCBNET
00030 *    AUTHOR:     ANNE KEFFER-KING                                *ELTCBNET
00031 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTCBNET
00032 *                WITH COMMUNITY BLUE PROGRAM.                    *ELTCBNET
00033 *    NOTES:      X---                                            *ELTCBNET
00034 *                                                                *ELTCBNET
00035 ******************************************************************ELTCBNET
00036 *                                                                *ELTCBNET
00037 *                      MAINTENANCE HISTORY                       *ELTCBNET
00038 *                                                                *ELTCBNET
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELTCBNET
00040 * ----- ----------- --- ----- ---------------------------------- *ELTCBNET
00041 * 01.00 19-FEB-1996 AKK       CREATED                            *ELTCBNET
00042 *                                                                *ELTCBNET
00043 ******************************************************************ELTCBNET
00044                                                                   ELTCBNET
00045  DATA DIVISION.                                                   ELTCBNET
00046                                                                   ELTCBNET
00047  WORKING-STORAGE SECTION.                                         ELTCBNET
00048  01  WS-MISC.                                                     ELTCBNET
00049      05  WS-BEGIN                      PIC X(26)  VALUE           ELTCBNET
00050      '*** ELTCBNET WS BEGINS ***'.                                ELTCBNET
00051                                                                   ELTCBNET
00052  01  PROGRAM-CONSTANTS.                                           ELTCBNET
00053      05  WS-GCBL                       PIC X(06)  VALUE '#GCBL '. ELTCBNET
00054                                                                   ELTCBNET
00055  01  WS-HOLD-AREA.                                                ELTCBNET
00056      05  WS-GCBL-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTCBNET
00057                                                                   ELTCBNET
00058  01  WS-FIXED-TEXT-AREA.                                          ELTCBNET
00059 **************************************************************    ELTCBNET
00060 ***                   HEADER  LINE                                ELTCBNET
00061 **************************************************************    ELTCBNET
00062      05  WS-HEADER-LINE.                                          ELTCBNET
00063          10  FILLER               PIC X(21) VALUE SPACES.         ELTCBNET
00064          10  FILLER               PIC X(36) VALUE                 ELTCBNET
00065          'COMMUNITY BLUE PROGRAM INSTITUTIONAL'.                  ELTCBNET
00066          10  FILLER               PIC X(22) VALUE SPACES.         ELTCBNET
00067                                                                   ELTCBNET
00068 **************************************************************    ELTCBNET
00069 ** A MESSAGE WHERE THE GROUP SPECIFIC DOES NOT HAVE CBL.          ELTCBNET
00070 **************************************************************    ELTCBNET
00071      05  WS-NOT-APPLICABLE-MSG.                                   ELTCBNET
00072          10  FILLER               PIC X(79) VALUE                 ELTCBNET
00073       'THE COMMUNITY BLUE PROGRAM IS NOT APPLICABLE.'.            ELTCBNET
00074                                                                   ELTCBNET
00075  LINKAGE SECTION.                                                 ELTCBNET
00076  01  DFHCOMMAREA.                                                 ELTCBNET
00077      COPY ELSCOMMC.                                               ELTCBNET
00078 /                                                                 ELTCBNET
00079      COPY ELSCIA2C.                                               ELTCBNET
00080 /                                                                 ELTCBNET
00081      COPY ELSCMDSC.                                               ELTCBNET
00082 /                                                                 ELTCBNET
00083      COPY ELSCMIFC.                                               ELTCBNET
00084 /                                                                 ELTCBNET
00085      COPY ELSIOPMC.                                               ELTCBNET
00086 /                                                                 ELTCBNET
00087      COPY ELSKEYSC.                                               ELTCBNET
00088 /                                                                 ELTCBNET
00089      COPY ELSOUTPC.                                               ELTCBNET
00090 /                                                                 ELTCBNET
00091      COPY ELSSRTPC.                                               ELTCBNET
00092 /                                                                 ELTCBNET
00093      COPY ELSTCWAC.                                               ELTCBNET
00094 /                                                                 ELTCBNET
00095      COPY ELSSSCBC.                                               ELTCBNET
00096 /                                                                 ELTCBNET
00097  01  GROUP-SPECIFIC-REC.                                          ELTCBNET
00098      COPY GCGROUPC.                                               ELTCBNET
00099 /                                                                 ELTCBNET
00100      EJECT                                                        ELTCBNET
00101  PROCEDURE DIVISION.                                              ELTCBNET
00102 ************************************************************      ELTCBNET
00103 *                                                          *      ELTCBNET
00104 *                    PROCEDURE DIVISION                    *      ELTCBNET
00105 *                                                          *      ELTCBNET
00106 ************************************************************      ELTCBNET
00107                                                                   ELTCBNET
00108                                                                   ELTCBNET
00109 ************************************************************      ELTCBNET
00110 *                                                          *      ELTCBNET
00111 *        COMMUNITY BLUE NETWORK                            *      ELTCBNET
00112 *                                                          *      ELTCBNET
00113 ************************************************************      ELTCBNET
00114  COMMUNITY-BLUE-NETWORK.                                          ELTCBNET
00115      PERFORM INITIALIZATION.                                      ELTCBNET
00116      PERFORM PROCESS.                                             ELTCBNET
00117      GOBACK.                                                      ELTCBNET
00118                                                                   ELTCBNET
00119                                                                   ELTCBNET
00120 ************************************************************      ELTCBNET
00121 *                                                          *      ELTCBNET
00122 *        INITIALIZATION.                                   *      ELTCBNET
00123 *                                                          *      ELTCBNET
00124 ************************************************************      ELTCBNET
00125  INITIALIZATION.                                                  ELTCBNET
00126      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTCBNET
00127      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTCBNET
00128                                                                   ELTCBNET
00129                                                                   ELTCBNET
00130 ************************************************************      ELTCBNET
00131 *                                                          *      ELTCBNET
00132 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTCBNET
00133 *                                                          *      ELTCBNET
00134 ************************************************************      ELTCBNET
00135  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTCBNET
00136      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTCBNET
00137      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTCBNET
00138      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTCBNET
00139                                                                   ELTCBNET
00140                                                                   ELTCBNET
00141 ************************************************************      ELTCBNET
00142 *                                                          *      ELTCBNET
00143 *        CHECK FOR VALID COMMAREA                          *      ELTCBNET
00144 *                                                          *      ELTCBNET
00145 ************************************************************      ELTCBNET
00146  CHECK-FOR-VALID-COMMAREA.                                        ELTCBNET
00147      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTCBNET
00148          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTCBNET
00149                                                                   ELTCBNET
00150                                                                   ELTCBNET
00151 ************************************************************      ELTCBNET
00152 *                                                          *      ELTCBNET
00153 *        SIGNAL INVALID COMMAREA                           *      ELTCBNET
00154 *                                                          *      ELTCBNET
00155 ************************************************************      ELTCBNET
00156  SIGNAL-INVALID-COMMAREA.                                         ELTCBNET
00157      EXEC CICS ABEND                                              ELTCBNET
00158                ABCODE('EL01')                                     ELTCBNET
00159         END-EXEC.                                                 ELTCBNET
00160      EJECT                                                        ELTCBNET
00161                                                                   ELTCBNET
00162                                                                   ELTCBNET
00163 ************************************************************      ELTCBNET
00164 *                                                          *      ELTCBNET
00165 *        ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA *      ELTCBNET
00166 *                                                          *      ELTCBNET
00167 ************************************************************      ELTCBNET
00168  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTCBNET
00169      IF ECA-CIA-PTR = NULL                                        ELTCBNET
00170          PERFORM SIGNAL-INVALID-CIA                               ELTCBNET
00171      ELSE                                                         ELTCBNET
00172          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTCBNET
00173                                                                   ELTCBNET
00174                                                                   ELTCBNET
00175 ************************************************************      ELTCBNET
00176 *                                                          *      ELTCBNET
00177 *        SIGNAL INVALID CIA                                *      ELTCBNET
00178 *                                                          *      ELTCBNET
00179 ************************************************************      ELTCBNET
00180  SIGNAL-INVALID-CIA.                                              ELTCBNET
00181      EXEC CICS ABEND                                              ELTCBNET
00182                ABCODE('EL02')                                     ELTCBNET
00183         END-EXEC.                                                 ELTCBNET
00184      EJECT                                                        ELTCBNET
00185                                                                   ELTCBNET
00186                                                                   ELTCBNET
00187 ************************************************************      ELTCBNET
00188 *                                                          *      ELTCBNET
00189 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTCBNET
00190 *                                                          *      ELTCBNET
00191 ************************************************************      ELTCBNET
00192  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTCBNET
00193      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTCBNET
00194      IF CIA-RC-PTR-NULL                                           ELTCBNET
00195          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCBNET
00196      ELSE                                                         ELTCBNET
00197          PERFORM ESTABLISH-ADDRESS-OF-SSCB.                       ELTCBNET
00198                                                                   ELTCBNET
00199                                                                   ELTCBNET
00200 ************************************************************      ELTCBNET
00201 *                                                          *      ELTCBNET
00202 *        SIGNAL UNALLOC AREA ERROR                         *      ELTCBNET
00203 *                                                          *      ELTCBNET
00204 ************************************************************      ELTCBNET
00205  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTCBNET
00206      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTCBNET
00207      PERFORM SIGNAL-ABEND.                                        ELTCBNET
00208                                                                   ELTCBNET
00209                                                                   ELTCBNET
00210 ************************************************************      ELTCBNET
00211 *                                                          *      ELTCBNET
00212 *        SIGNAL ABEND                                      *      ELTCBNET
00213 *                                                          *      ELTCBNET
00214 ************************************************************      ELTCBNET
00215  SIGNAL-ABEND.                                                    ELTCBNET
00216      EXEC CICS ABEND                                              ELTCBNET
00217                ABCODE(CIA-ABCODE)                                 ELTCBNET
00218         END-EXEC.                                                 ELTCBNET
00219      EJECT                                                        ELTCBNET
00220                                                                   ELTCBNET
00221 ************************************************************      ELTCBNET
00222 *                                                          *      ELTCBNET
00223 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTCBNET
00224 *                                                          *      ELTCBNET
00225 ************************************************************      ELTCBNET
00226  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTCBNET
00227      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTCBNET
00228      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTCBNET
00229      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTCBNET
00230      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTCBNET
00231      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTCBNET
00232      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTCBNET
00233                                                                   ELTCBNET
00234 ************************************************************      ELTCBNET
00235 *                                                          *      ELTCBNET
00236 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTCBNET
00237 *                                                          *      ELTCBNET
00238 ************************************************************      ELTCBNET
00239  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTCBNET
00240      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTCBNET
00241      IF CIA-RC-PTR-NULL                                           ELTCBNET
00242          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCBNET
00243      ELSE                                                         ELTCBNET
00244          PERFORM ESTABLISH-ADDRESS-OF-CMIF.                       ELTCBNET
00245      EJECT                                                        ELTCBNET
00246                                                                   ELTCBNET
00247                                                                   ELTCBNET
00248 ************************************************************      ELTCBNET
00249 *                                                          *      ELTCBNET
00250 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTCBNET
00251 *                                                          *      ELTCBNET
00252 ************************************************************      ELTCBNET
00253  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTCBNET
00254      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTCBNET
00255      IF CIA-RC-PTR-NULL                                           ELTCBNET
00256          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCBNET
00257      ELSE                                                         ELTCBNET
00258          PERFORM ESTABLISH-ADDRESS-OF-OUTP.                       ELTCBNET
00259      EJECT                                                        ELTCBNET
00260                                                                   ELTCBNET
00261                                                                   ELTCBNET
00262 ************************************************************      ELTCBNET
00263 *                                                          *      ELTCBNET
00264 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTCBNET
00265 *                                                          *      ELTCBNET
00266 ************************************************************      ELTCBNET
00267  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTCBNET
00268      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTCBNET
00269      IF CIA-RC-PTR-NULL                                           ELTCBNET
00270          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCBNET
00271      ELSE                                                         ELTCBNET
00272          PERFORM ESTABLISH-ADDRESS-OF-SRTP.                       ELTCBNET
00273      EJECT                                                        ELTCBNET
00274                                                                   ELTCBNET
00275                                                                   ELTCBNET
00276 ************************************************************      ELTCBNET
00277 *                                                          *      ELTCBNET
00278 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTCBNET
00279 *                                                          *      ELTCBNET
00280 ************************************************************      ELTCBNET
00281  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTCBNET
00282      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTCBNET
00283      IF CIA-RC-PTR-NULL                                           ELTCBNET
00284          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCBNET
00285      ELSE                                                         ELTCBNET
00286          PERFORM ESTABLISH-ADDRESS-OF-TCWA.                       ELTCBNET
00287      EJECT                                                        ELTCBNET
00288                                                                   ELTCBNET
00289                                                                   ELTCBNET
00290 ************************************************************      ELTCBNET
00291 *                                                          *      ELTCBNET
00292 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTCBNET
00293 *                                                          *      ELTCBNET
00294 ************************************************************      ELTCBNET
00295  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTCBNET
00296      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTCBNET
00297      IF CIA-RC-PTR-NULL                                           ELTCBNET
00298          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCBNET
00299      ELSE                                                         ELTCBNET
00300          PERFORM ESTABLISH-ADDRESS-OF-KEYS.                       ELTCBNET
00301      EJECT                                                        ELTCBNET
00302                                                                   ELTCBNET
00303                                                                   ELTCBNET
00304 ************************************************************      ELTCBNET
00305 *                                                          *      ELTCBNET
00306 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTCBNET
00307 *                                                          *      ELTCBNET
00308 ************************************************************      ELTCBNET
00309  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTCBNET
00310      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTCBNET
00311      IF CIA-RC-PTR-NULL                                           ELTCBNET
00312          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCBNET
00313      ELSE                                                         ELTCBNET
00314          PERFORM ESTABLISH-ADDRESS-OF-GRPSP.                      ELTCBNET
00315                                                                   ELTCBNET
00316                                                                   ELTCBNET
00317 ************************************************************      ELTCBNET
00318 *                                                          *      ELTCBNET
00319 *        ESTABLISH ADDRESS OF CIA                          *      ELTCBNET
00320 *                                                          *      ELTCBNET
00321 ************************************************************      ELTCBNET
00322  ESTABLISH-ADDRESS-OF-CIA.                                        ELTCBNET
00323      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTCBNET
00324          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTCBNET
00325                                                                   ELTCBNET
00326 ************************************************************      ELTCBNET
00327 *                                                          *      ELTCBNET
00328 *        ESTABLISH ADDRESS OF SSCB                         *      ELTCBNET
00329 *                                                          *      ELTCBNET
00330 ************************************************************      ELTCBNET
00331  ESTABLISH-ADDRESS-OF-SSCB.                                       ELTCBNET
00332      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBNET
00333          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTCBNET
00334                                                                   ELTCBNET
00335 ************************************************************      ELTCBNET
00336 *                                                          *      ELTCBNET
00337 *        ESTABLISH ADDRESS OF CMIF                         *      ELTCBNET
00338 *                                                          *      ELTCBNET
00339 ************************************************************      ELTCBNET
00340  ESTABLISH-ADDRESS-OF-CMIF.                                       ELTCBNET
00341      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBNET
00342          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTCBNET
00343                                                                   ELTCBNET
00344 ************************************************************      ELTCBNET
00345 *                                                          *      ELTCBNET
00346 *        ESTABLISH ADDRESS OF OUTP                         *      ELTCBNET
00347 *                                                          *      ELTCBNET
00348 ************************************************************      ELTCBNET
00349  ESTABLISH-ADDRESS-OF-OUTP.                                       ELTCBNET
00350      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBNET
00351          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTCBNET
00352                                                                   ELTCBNET
00353                                                                   ELTCBNET
00354 ************************************************************      ELTCBNET
00355 *                                                          *      ELTCBNET
00356 *        ESTABLISH ADDRESS OF SRTP                         *      ELTCBNET
00357 *                                                          *      ELTCBNET
00358 ************************************************************      ELTCBNET
00359  ESTABLISH-ADDRESS-OF-SRTP.                                       ELTCBNET
00360      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBNET
00361          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELTCBNET
00362                                                                   ELTCBNET
00363                                                                   ELTCBNET
00364 ************************************************************      ELTCBNET
00365 *                                                          *      ELTCBNET
00366 *        ESTABLISH ADDRESS OF TCWA                         *      ELTCBNET
00367 *                                                          *      ELTCBNET
00368 ************************************************************      ELTCBNET
00369  ESTABLISH-ADDRESS-OF-TCWA.                                       ELTCBNET
00370      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBNET
00371              ADDRESS OF TCAR-COMPRESSION-WORK-AREA.               ELTCBNET
00372                                                                   ELTCBNET
00373 ************************************************************      ELTCBNET
00374 *                                                          *      ELTCBNET
00375 *        ESTABLISH ADDRESS OF KEYS                         *      ELTCBNET
00376 *                                                          *      ELTCBNET
00377 ************************************************************      ELTCBNET
00378  ESTABLISH-ADDRESS-OF-KEYS.                                       ELTCBNET
00379      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBNET
00380           ADDRESS OF KWA-FILE-KEY-WORK-AREA.                      ELTCBNET
00381                                                                   ELTCBNET
00382                                                                   ELTCBNET
00383 ************************************************************      ELTCBNET
00384 *                                                          *      ELTCBNET
00385 *        ESTABLISH ADDRESS OF GRPSP                        *      ELTCBNET
00386 *                                                          *      ELTCBNET
00387 ************************************************************      ELTCBNET
00388  ESTABLISH-ADDRESS-OF-GRPSP.                                      ELTCBNET
00389      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBNET
00390             ADDRESS OF GROUP-SPECIFIC-REC.                        ELTCBNET
00391      EJECT                                                        ELTCBNET
00392                                                                   ELTCBNET
00393 ************************************************************      ELTCBNET
00394 *                                                          *      ELTCBNET
00395 *        PROCESS                                           *      ELTCBNET
00396 *                                                          *      ELTCBNET
00397 ************************************************************      ELTCBNET
00398  PROCESS.                                                         ELTCBNET
00399      PERFORM EJECT-NEW-PAGE.                                      ELTCBNET
00400      IF GCG-CBL-PARTICIPATION-IND = ZERO                          ELTCBNET
00401          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTCBNET
00402      ELSE                                                         ELTCBNET
00403          PERFORM DETERMINE-IF-GCBL-TABULAR-EXIS.                  ELTCBNET
00404      PERFORM TERMINATE-OUTPUT.                                    ELTCBNET
00405                                                                   ELTCBNET
00406                                                                   ELTCBNET
00407 ************************************************************      ELTCBNET
00408 *                                                          *      ELTCBNET
00409 *        EJECT NEW PAGE                                    *      ELTCBNET
00410 *                                                          *      ELTCBNET
00411 ************************************************************      ELTCBNET
00412  EJECT-NEW-PAGE.                                                  ELTCBNET
00413      SET COF-NEW-PAGE           TO TRUE.                          ELTCBNET
00414      MOVE +0                    TO COF-NBR-DTL-LINES.             ELTCBNET
00415      MOVE +2                    TO COF-NBR-HDR-LINES.             ELTCBNET
00416      MOVE WS-HEADER-LINE        TO COF-HDR-LINE                   ELTCBNET
00417          (COF-NBR-HDR-LINES).                                     ELTCBNET
00418      PERFORM LINK-TO-OUTPUT.                                      ELTCBNET
00419      EJECT                                                        ELTCBNET
00420                                                                   ELTCBNET
00421                                                                   ELTCBNET
00422 ************************************************************      ELTCBNET
00423 *                                                          *      ELTCBNET
00424 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTCBNET
00425 *                                                          *      ELTCBNET
00426 ************************************************************      ELTCBNET
00427  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTCBNET
00428      MOVE +1                    TO COF-NBR-DTL-LINES.             ELTCBNET
00429      MOVE SPACES                TO COF-DTL-LINE                   ELTCBNET
00430          (COF-NBR-DTL-LINES).                                     ELTCBNET
00431      ADD +1                     TO COF-NBR-DTL-LINES.             ELTCBNET
00432      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTCBNET
00433          (COF-NBR-DTL-LINES).                                     ELTCBNET
00434      PERFORM LINK-TO-OUTPUT.                                      ELTCBNET
00435      EJECT                                                        ELTCBNET
00436                                                                   ELTCBNET
00437                                                                   ELTCBNET
00438 ************************************************************      ELTCBNET
00439 *                                                          *      ELTCBNET
00440 *        DETERMINE IF GCBL TABULAR EXISTS                  *      ELTCBNET
00441 *                                                          *      ELTCBNET
00442 ************************************************************      ELTCBNET
00443  DETERMINE-IF-GCBL-TABULAR-EXIS.                                  ELTCBNET
00444      SET GCG-INDEX TO +1.                                         ELTCBNET
00445      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTCBNET
00446         AT END                                                    ELTCBNET
00447              MOVE ZEROES TO WS-GCBL-PROV-SLOT-NO                  ELTCBNET
00448         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCBL                     ELTCBNET
00449              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTCBNET
00450                   TO WS-GCBL-PROV-SLOT-NO                         ELTCBNET
00451         END-SEARCH.                                               ELTCBNET
00452      PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                      ELTCBNET
00453                                                                   ELTCBNET
00454                                                                   ELTCBNET
00455 ************************************************************      ELTCBNET
00456 *                                                          *      ELTCBNET
00457 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTCBNET
00458 *                                                          *      ELTCBNET
00459 ************************************************************      ELTCBNET
00460  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTCBNET
00461      MOVE 'COMMUNITY BLUE NETWORK'            TO                  ELTCBNET
00462          SRP-CCP-NAME.                                            ELTCBNET
00463      MOVE  WS-GCBL                           TO SRP-TABULAR-ID.   ELTCBNET
00464      MOVE  WS-GCBL-PROV-SLOT-NO              TO                   ELTCBNET
00465          SRP-TABULAR-SLOT-NO.                                     ELTCBNET
00466      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTCBNET
00467                                                                   ELTCBNET
00468                                                                   ELTCBNET
00469 ************************************************************      ELTCBNET
00470 *                                                          *      ELTCBNET
00471 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTCBNET
00472 *                                                          *      ELTCBNET
00473 ************************************************************      ELTCBNET
00474  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTCBNET
00475      EXEC CICS LINK                                               ELTCBNET
00476                PROGRAM ('ELGGXXC')                                ELTCBNET
00477                COMMAREA (DFHCOMMAREA)                             ELTCBNET
00478         END-EXEC.                                                 ELTCBNET
00479                                                                   ELTCBNET
00480                                                                   ELTCBNET
00481 ************************************************************      ELTCBNET
00482 *                                                          *      ELTCBNET
00483 *        LINK TO OUTPUT                                    *      ELTCBNET
00484 *                                                          *      ELTCBNET
00485 ************************************************************      ELTCBNET
00486  LINK-TO-OUTPUT.                                                  ELTCBNET
00487      EXEC CICS LINK                                               ELTCBNET
00488                PROGRAM ('ELUOUTPT')                               ELTCBNET
00489                COMMAREA (DFHCOMMAREA)                             ELTCBNET
00490         END-EXEC.                                                 ELTCBNET
00491      EJECT                                                        ELTCBNET
00492                                                                   ELTCBNET
00493 ************************************************************      ELTCBNET
00494 *                                                          *      ELTCBNET
00495 *        TERMINATE OUTPUT                                  *      ELTCBNET
00496 *                                                          *      ELTCBNET
00497 ************************************************************      ELTCBNET
00498  TERMINATE-OUTPUT.                                                ELTCBNET
00499      SET COF-END TO TRUE.                                         ELTCBNET
00500      PERFORM LINK-TO-OUTPUT.                                      ELTCBNET
