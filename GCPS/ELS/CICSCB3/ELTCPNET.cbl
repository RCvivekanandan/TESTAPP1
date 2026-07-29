00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELTCPNET
00003  PROGRAM-ID.         ELTCPNET.                                       LV001
00004                                                                   ELTCPNET
00005  AUTHOR.             ANNE KEFFER-KING.                            ELTCPNET
00006                                                                   ELTCPNET
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTCPNET
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTCPNET
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTCPNET
00010                      233 N. MICHIGAN AVE                          ELTCPNET
00011                      CHICAGO, ILLINOIS 60601                      ELTCPNET
00012                                                                   ELTCPNET
00013  DATE-WRITTEN.       22-FEB-1995.                                 ELTCPNET
00014                                                                   ELTCPNET
00015  DATE-COMPILED.                                                   ELTCPNET
00016                                                                   ELTCPNET
00017  SECURITY.           COPYRIGHT 1986,                              ELTCPNET
00018                      HEALTH CARE SERVICE CORPORATION              ELTCPNET
00019      SKIP3                                                        ELTCPNET
00020  ENVIRONMENT DIVISION.                                            ELTCPNET
00021                                                                   ELTCPNET
00022  CONFIGURATION SECTION.                                           ELTCPNET
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELTCPNET
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELTCPNET
00025      EJECT                                                        ELTCPNET
00026 ******************************************************************ELTCPNET
00027 *                                                                *ELTCPNET
00028 *    COPYBOOK:   ELTCPNET                                        *ELTCPNET
00029 *    DATE:       22-FEB-1995                                     *ELTCPNET
00030 *    AUTHOR:     ANNE KEFFER-KING                                *ELTCPNET
00031 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTCPNET
00032 *                WITH COMMUNITY PARTICIPATING NETWORK PROGRAM.   *ELTCPNET
00033 *    NOTES:      X---                                            *ELTCPNET
00034 *                                                                *ELTCPNET
00035 ******************************************************************ELTCPNET
00036 *                                                                *ELTCPNET
00037 *                      MAINTENANCE HISTORY                       *ELTCPNET
00038 *                                                                *ELTCPNET
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELTCPNET
00040 * ----- ----------- --- ----- ---------------------------------- *ELTCPNET
00041 * 01.00 22-FEB-1995 AKK       CREATED                            *ELTCPNET
00042 *                                                                *ELTCPNET
00043 ******************************************************************ELTCPNET
00044                                                                   ELTCPNET
00045  DATA DIVISION.                                                   ELTCPNET
00046                                                                   ELTCPNET
00047  WORKING-STORAGE SECTION.                                         ELTCPNET
00048  01  WS-MISC.                                                     ELTCPNET
00049      05  WS-BEGIN                      PIC X(26)  VALUE           ELTCPNET
00050      '*** ELTCPNET WS BEGINS ***'.                                ELTCPNET
00051                                                                   ELTCPNET
00052  01  PROGRAM-CONSTANTS.                                           ELTCPNET
00053      05  WS-GCPO                       PIC X(06)  VALUE '#GCPO '. ELTCPNET
00054                                                                   ELTCPNET
00055  01  WS-HOLD-AREA.                                                ELTCPNET
00056      05  WS-GCPO-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTCPNET
00057                                                                   ELTCPNET
00058  01  WS-FIXED-TEXT-AREA.                                          ELTCPNET
00059 **************************************************************    ELTCPNET
00060 ***                   HEADER  LINE                                ELTCPNET
00061 **************************************************************    ELTCPNET
00062      05  WS-HEADER-LINE.                                          ELTCPNET
00063          10  FILLER               PIC X(18) VALUE SPACES.         ELTCPNET
00064          10  FILLER               PIC X(42) VALUE                 ELTCPNET
00065          'COMMUNITY PARTICIPATING NETWORK PROGRAM '.              ELTCPNET
00066          10  FILLER               PIC X(19) VALUE SPACES.         ELTCPNET
00067                                                                   ELTCPNET
00068 **************************************************************    ELTCPNET
00069 ** A MESSAGE WHERE THE GROUP SPECIFIC DOES NOT HAVE CPO.          ELTCPNET
00070 **************************************************************    ELTCPNET
00071      05  WS-NOT-APPLICABLE-MSG.                                   ELTCPNET
00072          10  FILLER               PIC X(79) VALUE                 ELTCPNET
00073       'THE COMMUNITY PARTICIPATING OPTION PROGRAM IS NOT APPLICABLELTCPNET
00074 -     'E.'.                                                       ELTCPNET
00075                                                                   ELTCPNET
00076  LINKAGE SECTION.                                                 ELTCPNET
00077  01  DFHCOMMAREA.                                                 ELTCPNET
00078      COPY ELSCOMMC.                                               ELTCPNET
00079 /                                                                 ELTCPNET
00080      COPY ELSCIA2C.                                               ELTCPNET
00081 /                                                                 ELTCPNET
00082      COPY ELSCMDSC.                                               ELTCPNET
00083 /                                                                 ELTCPNET
00084      COPY ELSCMIFC.                                               ELTCPNET
00085 /                                                                 ELTCPNET
00086      COPY ELSIOPMC.                                               ELTCPNET
00087 /                                                                 ELTCPNET
00088      COPY ELSKEYSC.                                               ELTCPNET
00089 /                                                                 ELTCPNET
00090      COPY ELSOUTPC.                                               ELTCPNET
00091 /                                                                 ELTCPNET
00092      COPY ELSSRTPC.                                               ELTCPNET
00093 /                                                                 ELTCPNET
00094      COPY ELSTCWAC.                                               ELTCPNET
00095 /                                                                 ELTCPNET
00096      COPY ELSSSCBC.                                               ELTCPNET
00097 /                                                                 ELTCPNET
00098  01  GROUP-SPECIFIC-REC.                                          ELTCPNET
00099      COPY GCGROUPC.                                               ELTCPNET
00100 /                                                                 ELTCPNET
00101      EJECT                                                        ELTCPNET
00102  PROCEDURE DIVISION.                                              ELTCPNET
00103 ************************************************************      ELTCPNET
00104 *                                                          *      ELTCPNET
00105 *                    PROCEDURE DIVISION                    *      ELTCPNET
00106 *                                                          *      ELTCPNET
00107 ************************************************************      ELTCPNET
00108                                                                   ELTCPNET
00109                                                                   ELTCPNET
00110 ************************************************************      ELTCPNET
00111 *                                                          *      ELTCPNET
00112 *        COMMUNITY PROVIDER OPTION NETWORK                 *      ELTCPNET
00113 *                                                          *      ELTCPNET
00114 ************************************************************      ELTCPNET
00115  COMMUNITY-PROVIDER-NETWORK.                                      ELTCPNET
00116      PERFORM INITIALIZATION.                                      ELTCPNET
00117      PERFORM PROCESS.                                             ELTCPNET
00118      GOBACK.                                                      ELTCPNET
00119                                                                   ELTCPNET
00120                                                                   ELTCPNET
00121 ************************************************************      ELTCPNET
00122 *                                                          *      ELTCPNET
00123 *        INITIALIZATION.                                   *      ELTCPNET
00124 *                                                          *      ELTCPNET
00125 ************************************************************      ELTCPNET
00126  INITIALIZATION.                                                  ELTCPNET
00127      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTCPNET
00128      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTCPNET
00129                                                                   ELTCPNET
00130                                                                   ELTCPNET
00131 ************************************************************      ELTCPNET
00132 *                                                          *      ELTCPNET
00133 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTCPNET
00134 *                                                          *      ELTCPNET
00135 ************************************************************      ELTCPNET
00136  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTCPNET
00137      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTCPNET
00138      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTCPNET
00139      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTCPNET
00140                                                                   ELTCPNET
00141                                                                   ELTCPNET
00142 ************************************************************      ELTCPNET
00143 *                                                          *      ELTCPNET
00144 *        CHECK FOR VALID COMMAREA                          *      ELTCPNET
00145 *                                                          *      ELTCPNET
00146 ************************************************************      ELTCPNET
00147  CHECK-FOR-VALID-COMMAREA.                                        ELTCPNET
00148      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTCPNET
00149          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTCPNET
00150                                                                   ELTCPNET
00151                                                                   ELTCPNET
00152 ************************************************************      ELTCPNET
00153 *                                                          *      ELTCPNET
00154 *        SIGNAL INVALID COMMAREA                           *      ELTCPNET
00155 *                                                          *      ELTCPNET
00156 ************************************************************      ELTCPNET
00157  SIGNAL-INVALID-COMMAREA.                                         ELTCPNET
00158      EXEC CICS ABEND                                              ELTCPNET
00159                ABCODE('EL01')                                     ELTCPNET
00160         END-EXEC.                                                 ELTCPNET
00161      EJECT                                                        ELTCPNET
00162                                                                   ELTCPNET
00163                                                                   ELTCPNET
00164 ************************************************************      ELTCPNET
00165 *                                                          *      ELTCPNET
00166 *        ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA *      ELTCPNET
00167 *                                                          *      ELTCPNET
00168 ************************************************************      ELTCPNET
00169  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTCPNET
00170      IF ECA-CIA-PTR = NULL                                        ELTCPNET
00171          PERFORM SIGNAL-INVALID-CIA                               ELTCPNET
00172      ELSE                                                         ELTCPNET
00173          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTCPNET
00174                                                                   ELTCPNET
00175                                                                   ELTCPNET
00176 ************************************************************      ELTCPNET
00177 *                                                          *      ELTCPNET
00178 *        SIGNAL INVALID CIA                                *      ELTCPNET
00179 *                                                          *      ELTCPNET
00180 ************************************************************      ELTCPNET
00181  SIGNAL-INVALID-CIA.                                              ELTCPNET
00182      EXEC CICS ABEND                                              ELTCPNET
00183                ABCODE('EL02')                                     ELTCPNET
00184         END-EXEC.                                                 ELTCPNET
00185      EJECT                                                        ELTCPNET
00186                                                                   ELTCPNET
00187                                                                   ELTCPNET
00188 ************************************************************      ELTCPNET
00189 *                                                          *      ELTCPNET
00190 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTCPNET
00191 *                                                          *      ELTCPNET
00192 ************************************************************      ELTCPNET
00193  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTCPNET
00194      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTCPNET
00195      IF CIA-RC-PTR-NULL                                           ELTCPNET
00196          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCPNET
00197      ELSE                                                         ELTCPNET
00198          PERFORM ESTABLISH-ADDRESS-OF-SSCB.                       ELTCPNET
00199                                                                   ELTCPNET
00200                                                                   ELTCPNET
00201 ************************************************************      ELTCPNET
00202 *                                                          *      ELTCPNET
00203 *        SIGNAL UNALLOC AREA ERROR                         *      ELTCPNET
00204 *                                                          *      ELTCPNET
00205 ************************************************************      ELTCPNET
00206  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTCPNET
00207      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTCPNET
00208      PERFORM SIGNAL-ABEND.                                        ELTCPNET
00209                                                                   ELTCPNET
00210                                                                   ELTCPNET
00211 ************************************************************      ELTCPNET
00212 *                                                          *      ELTCPNET
00213 *        SIGNAL ABEND                                      *      ELTCPNET
00214 *                                                          *      ELTCPNET
00215 ************************************************************      ELTCPNET
00216  SIGNAL-ABEND.                                                    ELTCPNET
00217      EXEC CICS ABEND                                              ELTCPNET
00218                ABCODE(CIA-ABCODE)                                 ELTCPNET
00219         END-EXEC.                                                 ELTCPNET
00220      EJECT                                                        ELTCPNET
00221                                                                   ELTCPNET
00222 ************************************************************      ELTCPNET
00223 *                                                          *      ELTCPNET
00224 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTCPNET
00225 *                                                          *      ELTCPNET
00226 ************************************************************      ELTCPNET
00227  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTCPNET
00228      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTCPNET
00229      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTCPNET
00230      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTCPNET
00231      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTCPNET
00232      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTCPNET
00233      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTCPNET
00234                                                                   ELTCPNET
00235 ************************************************************      ELTCPNET
00236 *                                                          *      ELTCPNET
00237 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTCPNET
00238 *                                                          *      ELTCPNET
00239 ************************************************************      ELTCPNET
00240  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTCPNET
00241      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTCPNET
00242      IF CIA-RC-PTR-NULL                                           ELTCPNET
00243          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCPNET
00244      ELSE                                                         ELTCPNET
00245          PERFORM ESTABLISH-ADDRESS-OF-CMIF.                       ELTCPNET
00246      EJECT                                                        ELTCPNET
00247                                                                   ELTCPNET
00248                                                                   ELTCPNET
00249 ************************************************************      ELTCPNET
00250 *                                                          *      ELTCPNET
00251 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTCPNET
00252 *                                                          *      ELTCPNET
00253 ************************************************************      ELTCPNET
00254  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTCPNET
00255      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTCPNET
00256      IF CIA-RC-PTR-NULL                                           ELTCPNET
00257          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCPNET
00258      ELSE                                                         ELTCPNET
00259          PERFORM ESTABLISH-ADDRESS-OF-OUTP.                       ELTCPNET
00260      EJECT                                                        ELTCPNET
00261                                                                   ELTCPNET
00262                                                                   ELTCPNET
00263 ************************************************************      ELTCPNET
00264 *                                                          *      ELTCPNET
00265 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTCPNET
00266 *                                                          *      ELTCPNET
00267 ************************************************************      ELTCPNET
00268  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTCPNET
00269      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTCPNET
00270      IF CIA-RC-PTR-NULL                                           ELTCPNET
00271          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCPNET
00272      ELSE                                                         ELTCPNET
00273          PERFORM ESTABLISH-ADDRESS-OF-SRTP.                       ELTCPNET
00274      EJECT                                                        ELTCPNET
00275                                                                   ELTCPNET
00276                                                                   ELTCPNET
00277 ************************************************************      ELTCPNET
00278 *                                                          *      ELTCPNET
00279 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTCPNET
00280 *                                                          *      ELTCPNET
00281 ************************************************************      ELTCPNET
00282  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTCPNET
00283      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTCPNET
00284      IF CIA-RC-PTR-NULL                                           ELTCPNET
00285          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCPNET
00286      ELSE                                                         ELTCPNET
00287          PERFORM ESTABLISH-ADDRESS-OF-TCWA.                       ELTCPNET
00288      EJECT                                                        ELTCPNET
00289                                                                   ELTCPNET
00290                                                                   ELTCPNET
00291 ************************************************************      ELTCPNET
00292 *                                                          *      ELTCPNET
00293 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTCPNET
00294 *                                                          *      ELTCPNET
00295 ************************************************************      ELTCPNET
00296  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTCPNET
00297      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTCPNET
00298      IF CIA-RC-PTR-NULL                                           ELTCPNET
00299          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCPNET
00300      ELSE                                                         ELTCPNET
00301          PERFORM ESTABLISH-ADDRESS-OF-KEYS.                       ELTCPNET
00302      EJECT                                                        ELTCPNET
00303                                                                   ELTCPNET
00304                                                                   ELTCPNET
00305 ************************************************************      ELTCPNET
00306 *                                                          *      ELTCPNET
00307 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTCPNET
00308 *                                                          *      ELTCPNET
00309 ************************************************************      ELTCPNET
00310  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTCPNET
00311      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTCPNET
00312      IF CIA-RC-PTR-NULL                                           ELTCPNET
00313          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTCPNET
00314      ELSE                                                         ELTCPNET
00315          PERFORM ESTABLISH-ADDRESS-OF-GRPSP.                      ELTCPNET
00316                                                                   ELTCPNET
00317                                                                   ELTCPNET
00318 ************************************************************      ELTCPNET
00319 *                                                          *      ELTCPNET
00320 *        ESTABLISH ADDRESS OF CIA                          *      ELTCPNET
00321 *                                                          *      ELTCPNET
00322 ************************************************************      ELTCPNET
00323  ESTABLISH-ADDRESS-OF-CIA.                                        ELTCPNET
00324      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTCPNET
00325          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTCPNET
00326                                                                   ELTCPNET
00327 ************************************************************      ELTCPNET
00328 *                                                          *      ELTCPNET
00329 *        ESTABLISH ADDRESS OF SSCB                         *      ELTCPNET
00330 *                                                          *      ELTCPNET
00331 ************************************************************      ELTCPNET
00332  ESTABLISH-ADDRESS-OF-SSCB.                                       ELTCPNET
00333      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPNET
00334          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTCPNET
00335                                                                   ELTCPNET
00336 ************************************************************      ELTCPNET
00337 *                                                          *      ELTCPNET
00338 *        ESTABLISH ADDRESS OF CMIF                         *      ELTCPNET
00339 *                                                          *      ELTCPNET
00340 ************************************************************      ELTCPNET
00341  ESTABLISH-ADDRESS-OF-CMIF.                                       ELTCPNET
00342      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPNET
00343          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTCPNET
00344                                                                   ELTCPNET
00345 ************************************************************      ELTCPNET
00346 *                                                          *      ELTCPNET
00347 *        ESTABLISH ADDRESS OF OUTP                         *      ELTCPNET
00348 *                                                          *      ELTCPNET
00349 ************************************************************      ELTCPNET
00350  ESTABLISH-ADDRESS-OF-OUTP.                                       ELTCPNET
00351      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPNET
00352          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTCPNET
00353                                                                   ELTCPNET
00354                                                                   ELTCPNET
00355 ************************************************************      ELTCPNET
00356 *                                                          *      ELTCPNET
00357 *        ESTABLISH ADDRESS OF SRTP                         *      ELTCPNET
00358 *                                                          *      ELTCPNET
00359 ************************************************************      ELTCPNET
00360  ESTABLISH-ADDRESS-OF-SRTP.                                       ELTCPNET
00361      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPNET
00362          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELTCPNET
00363                                                                   ELTCPNET
00364                                                                   ELTCPNET
00365 ************************************************************      ELTCPNET
00366 *                                                          *      ELTCPNET
00367 *        ESTABLISH ADDRESS OF TCWA                         *      ELTCPNET
00368 *                                                          *      ELTCPNET
00369 ************************************************************      ELTCPNET
00370  ESTABLISH-ADDRESS-OF-TCWA.                                       ELTCPNET
00371      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPNET
00372              ADDRESS OF TCAR-COMPRESSION-WORK-AREA.               ELTCPNET
00373                                                                   ELTCPNET
00374 ************************************************************      ELTCPNET
00375 *                                                          *      ELTCPNET
00376 *        ESTABLISH ADDRESS OF KEYS                         *      ELTCPNET
00377 *                                                          *      ELTCPNET
00378 ************************************************************      ELTCPNET
00379  ESTABLISH-ADDRESS-OF-KEYS.                                       ELTCPNET
00380      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPNET
00381           ADDRESS OF KWA-FILE-KEY-WORK-AREA.                      ELTCPNET
00382                                                                   ELTCPNET
00383                                                                   ELTCPNET
00384 ************************************************************      ELTCPNET
00385 *                                                          *      ELTCPNET
00386 *        ESTABLISH ADDRESS OF GRPSP                        *      ELTCPNET
00387 *                                                          *      ELTCPNET
00388 ************************************************************      ELTCPNET
00389  ESTABLISH-ADDRESS-OF-GRPSP.                                      ELTCPNET
00390      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPNET
00391             ADDRESS OF GROUP-SPECIFIC-REC.                        ELTCPNET
00392      EJECT                                                        ELTCPNET
00393                                                                   ELTCPNET
00394 ************************************************************      ELTCPNET
00395 *                                                          *      ELTCPNET
00396 *        PROCESS                                           *      ELTCPNET
00397 *                                                          *      ELTCPNET
00398 ************************************************************      ELTCPNET
00399  PROCESS.                                                         ELTCPNET
00400      PERFORM EJECT-NEW-PAGE.                                      ELTCPNET
00401      IF GCG-CPO-PARTICIPATION-IND = ZERO                          ELTCPNET
00402          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTCPNET
00403      ELSE                                                         ELTCPNET
00404          PERFORM DETERMINE-IF-GCPO-TABULAR-EXIS.                  ELTCPNET
00405      PERFORM TERMINATE-OUTPUT.                                    ELTCPNET
00406                                                                   ELTCPNET
00407                                                                   ELTCPNET
00408 ************************************************************      ELTCPNET
00409 *                                                          *      ELTCPNET
00410 *        EJECT NEW PAGE                                    *      ELTCPNET
00411 *                                                          *      ELTCPNET
00412 ************************************************************      ELTCPNET
00413  EJECT-NEW-PAGE.                                                  ELTCPNET
00414      SET COF-NEW-PAGE           TO TRUE.                          ELTCPNET
00415      MOVE +0                    TO COF-NBR-DTL-LINES.             ELTCPNET
00416      MOVE +2                    TO COF-NBR-HDR-LINES.             ELTCPNET
00417      MOVE WS-HEADER-LINE        TO COF-HDR-LINE                   ELTCPNET
00418          (COF-NBR-HDR-LINES).                                     ELTCPNET
00419      PERFORM LINK-TO-OUTPUT.                                      ELTCPNET
00420      EJECT                                                        ELTCPNET
00421                                                                   ELTCPNET
00422                                                                   ELTCPNET
00423 ************************************************************      ELTCPNET
00424 *                                                          *      ELTCPNET
00425 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTCPNET
00426 *                                                          *      ELTCPNET
00427 ************************************************************      ELTCPNET
00428  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTCPNET
00429      MOVE +1                    TO COF-NBR-DTL-LINES.             ELTCPNET
00430      MOVE SPACES                TO COF-DTL-LINE                   ELTCPNET
00431          (COF-NBR-DTL-LINES).                                     ELTCPNET
00432      ADD +1                     TO COF-NBR-DTL-LINES.             ELTCPNET
00433      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTCPNET
00434          (COF-NBR-DTL-LINES).                                     ELTCPNET
00435      PERFORM LINK-TO-OUTPUT.                                      ELTCPNET
00436      EJECT                                                        ELTCPNET
00437                                                                   ELTCPNET
00438                                                                   ELTCPNET
00439 ************************************************************      ELTCPNET
00440 *                                                          *      ELTCPNET
00441 *        DETERMINE IF GCPO TABULAR EXISTS                  *      ELTCPNET
00442 *                                                          *      ELTCPNET
00443 ************************************************************      ELTCPNET
00444  DETERMINE-IF-GCPO-TABULAR-EXIS.                                  ELTCPNET
00445      SET GCG-INDEX TO +1.                                         ELTCPNET
00446      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTCPNET
00447         AT END                                                    ELTCPNET
00448              MOVE ZEROES TO WS-GCPO-PROV-SLOT-NO                  ELTCPNET
00449         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCPO                     ELTCPNET
00450              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTCPNET
00451                   TO WS-GCPO-PROV-SLOT-NO                         ELTCPNET
00452         END-SEARCH.                                               ELTCPNET
00453      PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                      ELTCPNET
00454                                                                   ELTCPNET
00455                                                                   ELTCPNET
00456 ************************************************************      ELTCPNET
00457 *                                                          *      ELTCPNET
00458 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTCPNET
00459 *                                                          *      ELTCPNET
00460 ************************************************************      ELTCPNET
00461  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTCPNET
00462      MOVE 'COMMUNITY PROVIDER NETWORK'       TO                   ELTCPNET
00463            SRP-CCP-NAME.                                          ELTCPNET
00464      MOVE  WS-GCPO                           TO SRP-TABULAR-ID.   ELTCPNET
00465      MOVE  WS-GCPO-PROV-SLOT-NO              TO                   ELTCPNET
00466          SRP-TABULAR-SLOT-NO.                                     ELTCPNET
00467      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTCPNET
00468                                                                   ELTCPNET
00469                                                                   ELTCPNET
00470 ************************************************************      ELTCPNET
00471 *                                                          *      ELTCPNET
00472 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTCPNET
00473 *                                                          *      ELTCPNET
00474 ************************************************************      ELTCPNET
00475  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTCPNET
00476      EXEC CICS LINK                                               ELTCPNET
00477                PROGRAM ('ELGGXXC')                                ELTCPNET
00478                COMMAREA (DFHCOMMAREA)                             ELTCPNET
00479         END-EXEC.                                                 ELTCPNET
00480                                                                   ELTCPNET
00481                                                                   ELTCPNET
00482 ************************************************************      ELTCPNET
00483 *                                                          *      ELTCPNET
00484 *        LINK TO OUTPUT                                    *      ELTCPNET
00485 *                                                          *      ELTCPNET
00486 ************************************************************      ELTCPNET
00487  LINK-TO-OUTPUT.                                                  ELTCPNET
00488      EXEC CICS LINK                                               ELTCPNET
00489                PROGRAM ('ELUOUTPT')                               ELTCPNET
00490                COMMAREA (DFHCOMMAREA)                             ELTCPNET
00491         END-EXEC.                                                 ELTCPNET
00492      EJECT                                                        ELTCPNET
00493                                                                   ELTCPNET
00494 ************************************************************      ELTCPNET
00495 *                                                          *      ELTCPNET
00496 *        TERMINATE OUTPUT                                  *      ELTCPNET
00497 *                                                          *      ELTCPNET
00498 ************************************************************      ELTCPNET
00499  TERMINATE-OUTPUT.                                                ELTCPNET
00500      SET COF-END TO TRUE.                                         ELTCPNET
00501      PERFORM LINK-TO-OUTPUT.                                      ELTCPNET
