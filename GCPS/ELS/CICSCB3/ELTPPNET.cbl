00001 *      LAST MAINTENANCE TIME: 14.09.18  DATE: 04/28/89            06/29/02
00002 * STRUCTURE(S) MEMBER ELTPPNETPL - LEVEL 009 AS OF 04/07/88       ELTPPNET
00003 * FROM PANLIB R360059.STRUCTPL.PANLIB                                LV001
00004 *                                                                 ELTPPNET
00005  IDENTIFICATION DIVISION.                                         ELTPPNET
00006                                                                   ELTPPNET
00007  PROGRAM-ID.         ELTPPNET.                                    ELTPPNET
00008                                                                   ELTPPNET
00009  AUTHOR.             RICK BARILEAU.                               ELTPPNET
00010                                                                   ELTPPNET
00011  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTPPNET
00012                      A MUTUAL LEGAL RESERVE COMPANY               ELTPPNET
00013                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTPPNET
00014                      233 N. MICHIGAN AVE                          ELTPPNET
00015                      CHICAGO, ILLINOIS 60601                      ELTPPNET
00016                                                                   ELTPPNET
00017  DATE-WRITTEN.       16-DEC-1987.                                 ELTPPNET
00018                                                                   ELTPPNET
00019  DATE-COMPILED.                                                   ELTPPNET
00020                                                                   ELTPPNET
00021  SECURITY.           COPYRIGHT 1986,                              ELTPPNET
00022                      HEALTH CARE SERVICE CORPORATION              ELTPPNET
00023      SKIP3                                                        ELTPPNET
00024  ENVIRONMENT DIVISION.                                            ELTPPNET
00025                                                                   ELTPPNET
00026  CONFIGURATION SECTION.                                           ELTPPNET
00027  SOURCE-COMPUTER.    IBM-3090.                                    ELTPPNET
00028  OBJECT-COMPUTER.    IBM-3090.                                    ELTPPNET
00029      EJECT                                                        ELTPPNET
00030 ******************************************************************ELTPPNET
00031 *                                                                *ELTPPNET
00032 *    COPYBOOK:   ELTPPNET                                        *ELTPPNET
00033 *    DATE:       16-DEC-1987                                     *ELTPPNET
00034 *    AUTHOR:     RICK BARILEAU                                   *ELTPPNET
00035 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTPPNET
00036 *                WITH PARTICIPATING PROVIDER NETWORK PROGRAM.    *ELTPPNET
00037 *    NOTES:      X---                                            *ELTPPNET
00038 *                                                                *ELTPPNET
00039 ******************************************************************ELTPPNET
00040 *                                                                *ELTPPNET
00041 *                      MAINTENANCE HISTORY                       *ELTPPNET
00042 *                                                                *ELTPPNET
00043 *  MOD     DATE     BY  DRPT                ACTION               *ELTPPNET
00044 * ----- ----------- --- ----- ---------------------------------- *ELTPPNET
00045 * 01.00 16-DEC-1987 REB       CREATED                            *ELTPPNET
00046 *                                                                *ELTPPNET
00047 * 01.01 16-DEC-1987 REB       FIX SYNTAX ON CONDITIONAL.         *ELTPPNET
00048 *                                                                *ELTPPNET
00049 * 01.02 12-JAN-1988 REB       THE SPECS WERE CHANGED TO DISPLAY  *ELTPPNET
00050 *                             VARIOUS TEXT DEPENDING ON HOW IT   *ELTPPNET
00051 *                             PARTICIPATES IN PPO IF AT ALL.     *ELTPPNET
00052 *                                                                *ELTPPNET
00053 * 01.03 14-JAN-1988 REB       RICH DECIDED TO ISOLATE THE ABOVE  *ELTPPNET
00054 *                             CHANGES INTO 'ELGGXXC' INSTEAD.    *ELTPPNET
00055 *                                                                *ELTPPNET
00056 * 01.04 07-APR-1988 AKK       CHANGED PARTICIPATING TO PREFERRED *ELTPPNET
00057 *                                                                *ELTPPNET
00058 * 01.05 28-APR-1989 AKK       DESTRUCT PROGRAM AMD DO STORAGE    *ELTPPNET
00059 *                             MANAGEMENT CHANGES.                *ELTPPNET
00060 *                                                                *ELTPPNET
00061 * 01.06 16-NOV-1990 JPB       CHANGED REFERENCE TO GCG-          *ELTPPNET
00062 *                             PARTICIPAT-PROV-OPTION TO REFLECT  *ELTPPNET
00063 *                             NEW FIELD SIZE.                    *ELTPPNET
00064 *                                                                *ELTPPNET
00065 ******************************************************************ELTPPNET
00066                                                                   ELTPPNET
00067  DATA DIVISION.                                                   ELTPPNET
00068                                                                   ELTPPNET
00069  WORKING-STORAGE SECTION.                                         ELTPPNET
00070  01  WS-MISC.                                                     ELTPPNET
00071      05  WS-BEGIN                      PIC X(26)  VALUE           ELTPPNET
00072      '*** ELTPPNET WS BEGINS ***'.                                ELTPPNET
00073                                                                   ELTPPNET
00074  01  PROGRAM-CONSTANTS.                                           ELTPPNET
00075      05  WS-GPPO                       PIC X(06)  VALUE '#GPPO '. ELTPPNET
00076                                                                   ELTPPNET
00077  01  WS-HOLD-AREA.                                                ELTPPNET
00078      05  WS-GPPO-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTPPNET
00079                                                                   ELTPPNET
00080  01  WS-FIXED-TEXT-AREA.                                          ELTPPNET
00081 **************************************************************    ELTPPNET
00082 ***                   HEADER  LINE                                ELTPPNET
00083 **************************************************************    ELTPPNET
00084      05  WS-HEADER-LINE.                                          ELTPPNET
00085          10  FILLER               PIC X(14) VALUE SPACES.         ELTPPNET
00086          10  FILLER               PIC X(48) VALUE                 ELTPPNET
00087          'PREFERRED PROVIDER NETWORK PROGRAM INSTITUTIONAL'.      ELTPPNET
00088          10  FILLER               PIC X(17) VALUE SPACES.         ELTPPNET
00089                                                                   ELTPPNET
00090 **************************************************************    ELTPPNET
00091 ** A MESSAGE WHERE THE GROUP SPECIFIC DOES NOT HAVE PPO.          ELTPPNET
00092 **************************************************************    ELTPPNET
00093      05  WS-NOT-APPLICABLE-MSG.                                   ELTPPNET
00094          10  FILLER               PIC X(79) VALUE                 ELTPPNET
00095        'THE PREFERRED PROVIDER OPTION PROGRAM IS NOT APPLICABLE.'.ELTPPNET
00096                                                                   ELTPPNET
00097  LINKAGE SECTION.                                                 ELTPPNET
00098  01  DFHCOMMAREA.                                                 ELTPPNET
00099      COPY ELSCOMMC.                                               ELTPPNET
00100 /                                                                 ELTPPNET
00101      COPY ELSCIA2C.                                               ELTPPNET
00102 /                                                                 ELTPPNET
00103      COPY ELSCMDSC.                                               ELTPPNET
00104 /                                                                 ELTPPNET
00105      COPY ELSCMIFC.                                               ELTPPNET
00106 /                                                                 ELTPPNET
00107      COPY ELSIOPMC.                                               ELTPPNET
00108 /                                                                 ELTPPNET
00109      COPY ELSKEYSC.                                               ELTPPNET
00110 /                                                                 ELTPPNET
00111      COPY ELSOUTPC.                                               ELTPPNET
00112 /                                                                 ELTPPNET
00113      COPY ELSSRTPC.                                               ELTPPNET
00114 /                                                                 ELTPPNET
00115      COPY ELSTCWAC.                                               ELTPPNET
00116 /                                                                 ELTPPNET
00117      COPY ELSSSCBC.                                               ELTPPNET
00118 /                                                                 ELTPPNET
00119  01  GROUP-SPECIFIC-REC.                                          ELTPPNET
00120      COPY GCGROUPC.                                               ELTPPNET
00121 /                                                                 ELTPPNET
00122      EJECT                                                        ELTPPNET
00123  PROCEDURE DIVISION.                                              ELTPPNET
00124 ************************************************************      ELTPPNET
00125 *                                                          *      ELTPPNET
00126 *                    PROCEDURE DIVISION                    *      ELTPPNET
00127 *                                                          *      ELTPPNET
00128 ************************************************************      ELTPPNET
00129                                                                   ELTPPNET
00130                                                                   ELTPPNET
00131 ************************************************************      ELTPPNET
00132 *                                                          *      ELTPPNET
00133 *        PARTICIPATING PROVIDER NETWORK                    *      ELTPPNET
00134 *                                                          *      ELTPPNET
00135 ************************************************************      ELTPPNET
00136  PARTICIPATING-PROVIDER-NETWORK.                                  ELTPPNET
00137      PERFORM INITIALIZATION.                                      ELTPPNET
00138      PERFORM PROCESS.                                             ELTPPNET
00139      GOBACK.                                                      ELTPPNET
00140                                                                   ELTPPNET
00141                                                                   ELTPPNET
00142 ************************************************************      ELTPPNET
00143 *                                                          *      ELTPPNET
00144 *        INITIALIZATION.                                   *      ELTPPNET
00145 *                                                          *      ELTPPNET
00146 ************************************************************      ELTPPNET
00147  INITIALIZATION.                                                  ELTPPNET
00148      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTPPNET
00149      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTPPNET
00150                                                                   ELTPPNET
00151                                                                   ELTPPNET
00152 ************************************************************      ELTPPNET
00153 *                                                          *      ELTPPNET
00154 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTPPNET
00155 *                                                          *      ELTPPNET
00156 ************************************************************      ELTPPNET
00157  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTPPNET
00158      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTPPNET
00159      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTPPNET
00160      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTPPNET
00161                                                                   ELTPPNET
00162                                                                   ELTPPNET
00163 ************************************************************      ELTPPNET
00164 *                                                          *      ELTPPNET
00165 *        CHECK FOR VALID COMMAREA                          *      ELTPPNET
00166 *                                                          *      ELTPPNET
00167 ************************************************************      ELTPPNET
00168  CHECK-FOR-VALID-COMMAREA.                                        ELTPPNET
00169      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTPPNET
00170          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTPPNET
00171                                                                   ELTPPNET
00172                                                                   ELTPPNET
00173 ************************************************************      ELTPPNET
00174 *                                                          *      ELTPPNET
00175 *        SIGNAL INVALID COMMAREA                           *      ELTPPNET
00176 *                                                          *      ELTPPNET
00177 ************************************************************      ELTPPNET
00178  SIGNAL-INVALID-COMMAREA.                                         ELTPPNET
00179      EXEC CICS ABEND                                              ELTPPNET
00180                ABCODE('EL01')                                     ELTPPNET
00181         END-EXEC.                                                 ELTPPNET
00182      EJECT                                                        ELTPPNET
00183                                                                   ELTPPNET
00184                                                                   ELTPPNET
00185 ************************************************************      ELTPPNET
00186 *                                                          *      ELTPPNET
00187 *        ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA *      ELTPPNET
00188 *                                                          *      ELTPPNET
00189 ************************************************************      ELTPPNET
00190  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTPPNET
00191      IF ECA-CIA-PTR = NULL                                        ELTPPNET
00192          PERFORM SIGNAL-INVALID-CIA                               ELTPPNET
00193      ELSE                                                         ELTPPNET
00194          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTPPNET
00195                                                                   ELTPPNET
00196                                                                   ELTPPNET
00197 ************************************************************      ELTPPNET
00198 *                                                          *      ELTPPNET
00199 *        SIGNAL INVALID CIA                                *      ELTPPNET
00200 *                                                          *      ELTPPNET
00201 ************************************************************      ELTPPNET
00202  SIGNAL-INVALID-CIA.                                              ELTPPNET
00203      EXEC CICS ABEND                                              ELTPPNET
00204                ABCODE('EL02')                                     ELTPPNET
00205         END-EXEC.                                                 ELTPPNET
00206      EJECT                                                        ELTPPNET
00207                                                                   ELTPPNET
00208                                                                   ELTPPNET
00209 ************************************************************      ELTPPNET
00210 *                                                          *      ELTPPNET
00211 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTPPNET
00212 *                                                          *      ELTPPNET
00213 ************************************************************      ELTPPNET
00214  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTPPNET
00215      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTPPNET
00216      IF CIA-RC-PTR-NULL                                           ELTPPNET
00217          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPPNET
00218      ELSE                                                         ELTPPNET
00219          PERFORM ESTABLISH-ADDRESS-OF-SSCB.                       ELTPPNET
00220                                                                   ELTPPNET
00221                                                                   ELTPPNET
00222 ************************************************************      ELTPPNET
00223 *                                                          *      ELTPPNET
00224 *        SIGNAL UNALLOC AREA ERROR                         *      ELTPPNET
00225 *                                                          *      ELTPPNET
00226 ************************************************************      ELTPPNET
00227  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTPPNET
00228      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTPPNET
00229      PERFORM SIGNAL-ABEND.                                        ELTPPNET
00230                                                                   ELTPPNET
00231                                                                   ELTPPNET
00232 ************************************************************      ELTPPNET
00233 *                                                          *      ELTPPNET
00234 *        SIGNAL ABEND                                      *      ELTPPNET
00235 *                                                          *      ELTPPNET
00236 ************************************************************      ELTPPNET
00237  SIGNAL-ABEND.                                                    ELTPPNET
00238      EXEC CICS ABEND                                              ELTPPNET
00239                ABCODE(CIA-ABCODE)                                 ELTPPNET
00240         END-EXEC.                                                 ELTPPNET
00241      EJECT                                                        ELTPPNET
00242                                                                   ELTPPNET
00243 ************************************************************      ELTPPNET
00244 *                                                          *      ELTPPNET
00245 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTPPNET
00246 *                                                          *      ELTPPNET
00247 ************************************************************      ELTPPNET
00248  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTPPNET
00249      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTPPNET
00250      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTPPNET
00251      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTPPNET
00252      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTPPNET
00253      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTPPNET
00254      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTPPNET
00255                                                                   ELTPPNET
00256 ************************************************************      ELTPPNET
00257 *                                                          *      ELTPPNET
00258 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTPPNET
00259 *                                                          *      ELTPPNET
00260 ************************************************************      ELTPPNET
00261  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTPPNET
00262      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTPPNET
00263      IF CIA-RC-PTR-NULL                                           ELTPPNET
00264          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPPNET
00265      ELSE                                                         ELTPPNET
00266          PERFORM ESTABLISH-ADDRESS-OF-CMIF.                       ELTPPNET
00267      EJECT                                                        ELTPPNET
00268                                                                   ELTPPNET
00269                                                                   ELTPPNET
00270 ************************************************************      ELTPPNET
00271 *                                                          *      ELTPPNET
00272 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTPPNET
00273 *                                                          *      ELTPPNET
00274 ************************************************************      ELTPPNET
00275  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTPPNET
00276      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTPPNET
00277      IF CIA-RC-PTR-NULL                                           ELTPPNET
00278          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPPNET
00279      ELSE                                                         ELTPPNET
00280          PERFORM ESTABLISH-ADDRESS-OF-OUTP.                       ELTPPNET
00281      EJECT                                                        ELTPPNET
00282                                                                   ELTPPNET
00283                                                                   ELTPPNET
00284 ************************************************************      ELTPPNET
00285 *                                                          *      ELTPPNET
00286 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTPPNET
00287 *                                                          *      ELTPPNET
00288 ************************************************************      ELTPPNET
00289  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTPPNET
00290      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTPPNET
00291      IF CIA-RC-PTR-NULL                                           ELTPPNET
00292          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPPNET
00293      ELSE                                                         ELTPPNET
00294          PERFORM ESTABLISH-ADDRESS-OF-SRTP.                       ELTPPNET
00295      EJECT                                                        ELTPPNET
00296                                                                   ELTPPNET
00297                                                                   ELTPPNET
00298 ************************************************************      ELTPPNET
00299 *                                                          *      ELTPPNET
00300 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTPPNET
00301 *                                                          *      ELTPPNET
00302 ************************************************************      ELTPPNET
00303  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTPPNET
00304      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTPPNET
00305      IF CIA-RC-PTR-NULL                                           ELTPPNET
00306          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPPNET
00307      ELSE                                                         ELTPPNET
00308          PERFORM ESTABLISH-ADDRESS-OF-TCWA.                       ELTPPNET
00309      EJECT                                                        ELTPPNET
00310                                                                   ELTPPNET
00311                                                                   ELTPPNET
00312 ************************************************************      ELTPPNET
00313 *                                                          *      ELTPPNET
00314 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTPPNET
00315 *                                                          *      ELTPPNET
00316 ************************************************************      ELTPPNET
00317  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTPPNET
00318      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTPPNET
00319      IF CIA-RC-PTR-NULL                                           ELTPPNET
00320          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPPNET
00321      ELSE                                                         ELTPPNET
00322          PERFORM ESTABLISH-ADDRESS-OF-KEYS.                       ELTPPNET
00323      EJECT                                                        ELTPPNET
00324                                                                   ELTPPNET
00325                                                                   ELTPPNET
00326 ************************************************************      ELTPPNET
00327 *                                                          *      ELTPPNET
00328 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTPPNET
00329 *                                                          *      ELTPPNET
00330 ************************************************************      ELTPPNET
00331  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTPPNET
00332      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTPPNET
00333      IF CIA-RC-PTR-NULL                                           ELTPPNET
00334          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELTPPNET
00335      ELSE                                                         ELTPPNET
00336          PERFORM ESTABLISH-ADDRESS-OF-GRPSP.                      ELTPPNET
00337                                                                   ELTPPNET
00338                                                                   ELTPPNET
00339 ************************************************************      ELTPPNET
00340 *                                                          *      ELTPPNET
00341 *        ESTABLISH ADDRESS OF CIA                          *      ELTPPNET
00342 *                                                          *      ELTPPNET
00343 ************************************************************      ELTPPNET
00344  ESTABLISH-ADDRESS-OF-CIA.                                        ELTPPNET
00345      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTPPNET
00346          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTPPNET
00347                                                                   ELTPPNET
00348 ************************************************************      ELTPPNET
00349 *                                                          *      ELTPPNET
00350 *        ESTABLISH ADDRESS OF SSCB                         *      ELTPPNET
00351 *                                                          *      ELTPPNET
00352 ************************************************************      ELTPPNET
00353  ESTABLISH-ADDRESS-OF-SSCB.                                       ELTPPNET
00354      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPNET
00355          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTPPNET
00356                                                                   ELTPPNET
00357 ************************************************************      ELTPPNET
00358 *                                                          *      ELTPPNET
00359 *        ESTABLISH ADDRESS OF CMIF                         *      ELTPPNET
00360 *                                                          *      ELTPPNET
00361 ************************************************************      ELTPPNET
00362  ESTABLISH-ADDRESS-OF-CMIF.                                       ELTPPNET
00363      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPNET
00364          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTPPNET
00365                                                                   ELTPPNET
00366 ************************************************************      ELTPPNET
00367 *                                                          *      ELTPPNET
00368 *        ESTABLISH ADDRESS OF OUTP                         *      ELTPPNET
00369 *                                                          *      ELTPPNET
00370 ************************************************************      ELTPPNET
00371  ESTABLISH-ADDRESS-OF-OUTP.                                       ELTPPNET
00372      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPNET
00373          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTPPNET
00374                                                                   ELTPPNET
00375                                                                   ELTPPNET
00376 ************************************************************      ELTPPNET
00377 *                                                          *      ELTPPNET
00378 *        ESTABLISH ADDRESS OF SRTP                         *      ELTPPNET
00379 *                                                          *      ELTPPNET
00380 ************************************************************      ELTPPNET
00381  ESTABLISH-ADDRESS-OF-SRTP.                                       ELTPPNET
00382      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPNET
00383          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELTPPNET
00384                                                                   ELTPPNET
00385                                                                   ELTPPNET
00386 ************************************************************      ELTPPNET
00387 *                                                          *      ELTPPNET
00388 *        ESTABLISH ADDRESS OF TCWA                         *      ELTPPNET
00389 *                                                          *      ELTPPNET
00390 ************************************************************      ELTPPNET
00391  ESTABLISH-ADDRESS-OF-TCWA.                                       ELTPPNET
00392      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPNET
00393              ADDRESS OF TCAR-COMPRESSION-WORK-AREA.               ELTPPNET
00394                                                                   ELTPPNET
00395 ************************************************************      ELTPPNET
00396 *                                                          *      ELTPPNET
00397 *        ESTABLISH ADDRESS OF KEYS                         *      ELTPPNET
00398 *                                                          *      ELTPPNET
00399 ************************************************************      ELTPPNET
00400  ESTABLISH-ADDRESS-OF-KEYS.                                       ELTPPNET
00401      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPNET
00402           ADDRESS OF KWA-FILE-KEY-WORK-AREA.                      ELTPPNET
00403                                                                   ELTPPNET
00404                                                                   ELTPPNET
00405 ************************************************************      ELTPPNET
00406 *                                                          *      ELTPPNET
00407 *        ESTABLISH ADDRESS OF GRPSP                        *      ELTPPNET
00408 *                                                          *      ELTPPNET
00409 ************************************************************      ELTPPNET
00410  ESTABLISH-ADDRESS-OF-GRPSP.                                      ELTPPNET
00411      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPNET
00412             ADDRESS OF GROUP-SPECIFIC-REC.                        ELTPPNET
00413      EJECT                                                        ELTPPNET
00414                                                                   ELTPPNET
00415 ************************************************************      ELTPPNET
00416 *                                                          *      ELTPPNET
00417 *        PROCESS                                           *      ELTPPNET
00418 *                                                          *      ELTPPNET
00419 ************************************************************      ELTPPNET
00420  PROCESS.                                                         ELTPPNET
00421      PERFORM EJECT-NEW-PAGE.                                      ELTPPNET
00422      IF GCG-PARTICIPAT-PROV-OPTION = ZERO                         ELTPPNET
00423          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTPPNET
00424      ELSE                                                         ELTPPNET
00425          PERFORM DETERMINE-IF-GPPO-TABULAR-EXIS.                  ELTPPNET
00426      PERFORM TERMINATE-OUTPUT.                                    ELTPPNET
00427                                                                   ELTPPNET
00428                                                                   ELTPPNET
00429 ************************************************************      ELTPPNET
00430 *                                                          *      ELTPPNET
00431 *        EJECT NEW PAGE                                    *      ELTPPNET
00432 *                                                          *      ELTPPNET
00433 ************************************************************      ELTPPNET
00434  EJECT-NEW-PAGE.                                                  ELTPPNET
00435      SET COF-NEW-PAGE           TO TRUE.                          ELTPPNET
00436      MOVE +0                    TO COF-NBR-DTL-LINES.             ELTPPNET
00437      MOVE +2                    TO COF-NBR-HDR-LINES.             ELTPPNET
00438      MOVE WS-HEADER-LINE        TO COF-HDR-LINE                   ELTPPNET
00439          (COF-NBR-HDR-LINES).                                     ELTPPNET
00440      PERFORM LINK-TO-OUTPUT.                                      ELTPPNET
00441      EJECT                                                        ELTPPNET
00442                                                                   ELTPPNET
00443                                                                   ELTPPNET
00444 ************************************************************      ELTPPNET
00445 *                                                          *      ELTPPNET
00446 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTPPNET
00447 *                                                          *      ELTPPNET
00448 ************************************************************      ELTPPNET
00449  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTPPNET
00450      MOVE +1                    TO COF-NBR-DTL-LINES.             ELTPPNET
00451      MOVE SPACES                TO COF-DTL-LINE                   ELTPPNET
00452          (COF-NBR-DTL-LINES).                                     ELTPPNET
00453      ADD +1                     TO COF-NBR-DTL-LINES.             ELTPPNET
00454      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTPPNET
00455          (COF-NBR-DTL-LINES).                                     ELTPPNET
00456      PERFORM LINK-TO-OUTPUT.                                      ELTPPNET
00457      EJECT                                                        ELTPPNET
00458                                                                   ELTPPNET
00459                                                                   ELTPPNET
00460 ************************************************************      ELTPPNET
00461 *                                                          *      ELTPPNET
00462 *        DETERMINE IF GPPO TABULAR EXISTS                  *      ELTPPNET
00463 *                                                          *      ELTPPNET
00464 ************************************************************      ELTPPNET
00465  DETERMINE-IF-GPPO-TABULAR-EXIS.                                  ELTPPNET
00466      SET GCG-INDEX TO +1.                                         ELTPPNET
00467      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPPNET
00468         AT END                                                    ELTPPNET
00469              MOVE ZEROES TO WS-GPPO-PROV-SLOT-NO                  ELTPPNET
00470         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GPPO                     ELTPPNET
00471              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTPPNET
00472                   TO WS-GPPO-PROV-SLOT-NO                         ELTPPNET
00473         END-SEARCH.                                               ELTPPNET
00474      PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                      ELTPPNET
00475                                                                   ELTPPNET
00476                                                                   ELTPPNET
00477 ************************************************************      ELTPPNET
00478 *                                                          *      ELTPPNET
00479 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTPPNET
00480 *                                                          *      ELTPPNET
00481 ************************************************************      ELTPPNET
00482  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTPPNET
00483      MOVE 'PREFERRED PROVIDER NETWORK'       TO                   ELTPPNET
00484          SRP-CCP-NAME.                                            ELTPPNET
00485      MOVE  WS-GPPO                           TO SRP-TABULAR-ID.   ELTPPNET
00486      MOVE  WS-GPPO-PROV-SLOT-NO              TO                   ELTPPNET
00487          SRP-TABULAR-SLOT-NO.                                     ELTPPNET
00488      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTPPNET
00489                                                                   ELTPPNET
00490                                                                   ELTPPNET
00491 ************************************************************      ELTPPNET
00492 *                                                          *      ELTPPNET
00493 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTPPNET
00494 *                                                          *      ELTPPNET
00495 ************************************************************      ELTPPNET
00496  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTPPNET
00497      EXEC CICS LINK                                               ELTPPNET
00498                PROGRAM ('ELGGXXC')                                ELTPPNET
00499                COMMAREA (DFHCOMMAREA)                             ELTPPNET
00500         END-EXEC.                                                 ELTPPNET
00501                                                                   ELTPPNET
00502                                                                   ELTPPNET
00503 ************************************************************      ELTPPNET
00504 *                                                          *      ELTPPNET
00505 *        LINK TO OUTPUT                                    *      ELTPPNET
00506 *                                                          *      ELTPPNET
00507 ************************************************************      ELTPPNET
00508  LINK-TO-OUTPUT.                                                  ELTPPNET
00509      EXEC CICS LINK                                               ELTPPNET
00510                PROGRAM ('ELUOUTPT')                               ELTPPNET
00511                COMMAREA (DFHCOMMAREA)                             ELTPPNET
00512         END-EXEC.                                                 ELTPPNET
00513      EJECT                                                        ELTPPNET
00514                                                                   ELTPPNET
00515 ************************************************************      ELTPPNET
00516 *                                                          *      ELTPPNET
00517 *        TERMINATE OUTPUT                                  *      ELTPPNET
00518 *                                                          *      ELTPPNET
00519 ************************************************************      ELTPPNET
00520  TERMINATE-OUTPUT.                                                ELTPPNET
00521      SET COF-END TO TRUE.                                         ELTPPNET
00522      PERFORM LINK-TO-OUTPUT.                                      ELTPPNET
