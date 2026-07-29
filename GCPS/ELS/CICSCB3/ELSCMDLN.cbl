00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSCMDLN
00003  PROGRAM-ID.         ELSCMDLN.                                       LV002
00004                                                                   ELSCMDLN
00005  AUTHOR.             EDWARD G LISS                                ELSCMDLN
00006                                                                   ELSCMDLN
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSCMDLN
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSCMDLN
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSCMDLN
00010                      233 N. MICHIGAN AVE                          ELSCMDLN
00011                      CHICAGO, ILLINOIS 60601                      ELSCMDLN
00012                                                                   ELSCMDLN
00013  DATE-WRITTEN.       18-NOV-1986.                                 ELSCMDLN
00014                                                                   ELSCMDLN
00015  DATE-COMPILED.                                                   ELSCMDLN
00016                                                                   ELSCMDLN
00017  SECURITY.           COPYRIGHT 1986,                              ELSCMDLN
00018                      HEALTH CARE SERVICE CORPORATION              ELSCMDLN
00019      SKIP3                                                        ELSCMDLN
00020  TITLE 'ELS COMMAND LINE PROCESSOR                        '.      ELSCMDLN
00021  ENVIRONMENT DIVISION.                                            ELSCMDLN
00022                                                                   ELSCMDLN
00023  CONFIGURATION SECTION.                                           ELSCMDLN
00024  SOURCE-COMPUTER.    IBM-3033.                                    ELSCMDLN
00025  OBJECT-COMPUTER.    IBM-3033.                                    ELSCMDLN
00026      EJECT                                                        ELSCMDLN
00027 ******************************************************************ELSCMDLN
00028 *                                                                *ELSCMDLN
00029 *    PROGRAM:    ELSCMDLN                                        *ELSCMDLN
00030 *    DATE:       18-NOV-1986                                     *ELSCMDLN
00031 *    AUTHOR:     EDWARD G LISS                                   *ELSCMDLN
00032 *    FUNCTION:                                                   *ELSCMDLN
00033 *       THIS MODULE WILL EXAMINE THE CICS COMMAND LINE AND       *ELSCMDLN
00034 *       DETERMINE IF THERE IS VALID ARGUMENTS TO ELIQ.           *ELSCMDLN
00035 *       IF THERE ARE VALID ARGUMENTS, THEY WILL BE PARSED I      *ELSCMDLN
00036 *       INTO THE SSB AND PASSED TO THE GROUP SELECTOR FOR        *ELSCMDLN
00037 *       VALIDATION.                                              *ELSCMDLN
00038 *                                                                *ELSCMDLN
00039 *    NOTES:                                                      *ELSCMDLN
00040 *                                                                *ELSCMDLN
00041 ******************************************************************ELSCMDLN
00042 *                                                                *ELSCMDLN
00043 *                      MAINTENANCE HISTORY                       *ELSCMDLN
00044 *                                                                *ELSCMDLN
00045 *  MOD     DATE     BY  DRPT                ACTION               *ELSCMDLN
00046 * ----- ----------- --- ----- ---------------------------------- *ELSCMDLN
00047 * 01.00 18-NOV-1986 EGL       CREATED                            *ELSCMDLN
00048 * 01.01 18-JUL-1988 EGL       ADDED NEW STORAGE MANAGEMENT       *ELSCMDLN
00049 *                             ROUTINE CALLS.                     *ELSCMDLN
00050 * 01.02 22-AUG-1989 EGL       DESTRUCTED PROGRAM.                *ELSCMDLN
00051 *                                                                *ELSCMDLN
00052 * 01.03 30-SEP-1997 AKK       ADD SUPPORT FOR YR 2000 AND        *ELSCMDLN
00053 *                             TX MERGE.                          *ELSCMDLN
00054 *                                                                *ELSCMDLN
00055 *       12-AUG-2003 AKK       GEN'D FOR TEST OF ORDER OF COMPILE *ELSCMDLN
00056 *                                                                *ELSCMDLN
00057 ******************************************************************ELSCMDLN
00058      EJECT                                                        ELSCMDLN
00059  DATA DIVISION.                                                   ELSCMDLN
00060                                                                   ELSCMDLN
00061  WORKING-STORAGE SECTION.                                         ELSCMDLN
00062                                                                   ELSCMDLN
00063  77  WS-MOVE-POS                     PICTURE S9(4) COMP SYNC.     ELSCMDLN
00064                                                                   ELSCMDLN
00065  77  WS-NUMERIC-FIELD-SW             PICTURE X.                   ELSCMDLN
00066      88  WS-NUMERIC-FIELD                    VALUE 'Y'.           ELSCMDLN
00067      88  WS-NUMERIC-INITIAL                  VALUE 'Y'.           ELSCMDLN
00068      88  WS-NON-NUMERIC-FIELD                VALUE 'N'.           ELSCMDLN
00069                                                                   ELSCMDLN
00070  77  WS-SCR-MSG-SW                   PICTURE X VALUE 'N'.         ELSCMDLN
00071      88  WS-SCR-MSG-USED-UP                    VALUE 'Y'.         ELSCMDLN
00072      88  WS-SCR-MSG-NOT-USED-UP                VALUE 'N'.         ELSCMDLN
00073                                                                   ELSCMDLN
00074  01  WS-SCR-MSG-LENGTH               PICTURE S9(4) COMP.          ELSCMDLN
00075  01  WS-SCR-MSG-POINTER              POINTER.                     ELSCMDLN
00076                                                                   ELSCMDLN
00077 *                                                                 ELSCMDLN
00078 *     THE FOLLOWING 3 AREAS (WS-GROUP-NO, WS-SECTION-NO AND       ELSCMDLN
00079 *     WS-SUBSCR-NO) MUST BE 1 BYTE LARGER THAN THE MAXIMUM        ELSCMDLN
00080 *     LENGTH.  THIS SIMPLIFIES THE CHECK FOR FIELD TOO LONG.      ELSCMDLN
00081 *     IF SOMETHING GETS MOVED INTO THIS EXTRA BYTE, THE DATA      ELSCMDLN
00082 *     IS TOO LONG.                                                ELSCMDLN
00083 *                                                                 ELSCMDLN
00084  01  WS-GROUP-NO.                                                 ELSCMDLN
00085      05  WS-GROUP-CHAR               OCCURS 10 TIMES              ELSCMDLN
00086                                      INDEXED BY WS-GROUP-IDX      ELSCMDLN
00087                                      PICTURE X.                   ELSCMDLN
00088                                                                   ELSCMDLN
00089  01  WS-SECTION-NO.                                               ELSCMDLN
00090      05  WS-SECTION-CHAR             OCCURS 6 TIMES               ELSCMDLN
00091                                      INDEXED BY WS-SECTION-IDX    ELSCMDLN
00092                                      PICTURE X.                   ELSCMDLN
00093                                                                   ELSCMDLN
00094  01  WS-SUBSCR-NO.                                                ELSCMDLN
00095      05  WS-SUBSCR-CHAR              OCCURS 13 TIMES              ELSCMDLN
00096                                      INDEXED BY WS-SUBSCR-IDX     ELSCMDLN
00097                                      PICTURE X.                   ELSCMDLN
00098      EJECT                                                        ELSCMDLN
00099  COPY HEXCOBOL.                                                   ELSCMDLN
00100      EJECT                                                        ELSCMDLN
00101  LINKAGE SECTION.                                                 ELSCMDLN
00102  01  DFHCOMMAREA.                                                 ELSCMDLN
00103  COPY ELSCOMMC.                                                   ELSCMDLN
00104      EJECT                                                        ELSCMDLN
00105  COPY ELSCIA2C.                                                   ELSCMDLN
00106      EJECT                                                        ELSCMDLN
00107  COPY ELSSSCBC.                                                   ELSCMDLN
00108      EJECT                                                        ELSCMDLN
00109  01  SCR-MSG.                                                     ELSCMDLN
00110      05  SCR-MSG-AREA                PICTURE X(2048).             ELSCMDLN
00111      05  SCR-MSG-CHAR     REDEFINES SCR-MSG-AREA                  ELSCMDLN
00112                                      OCCURS 2048 TIMES            ELSCMDLN
00113                                      INDEXED BY SCR-IDX           ELSCMDLN
00114                                      PICTURE X.                   ELSCMDLN
00115      05  SCR-CLEAR-SCREEN REDEFINES SCR-MSG-AREA.                 ELSCMDLN
00116          10  SCR-TRAN-ID-1           PICTURE X(4).                ELSCMDLN
00117          10  FILLER                  PICTURE X(2044).             ELSCMDLN
00118      05  SCR-SIGN-SCREEN  REDEFINES SCR-MSG-AREA.                 ELSCMDLN
00119          10  FILLER                  PICTURE X(3).                ELSCMDLN
00120          10  SCR-TRAN-ID-2           PICTURE X(4).                ELSCMDLN
00121          10  FILLER                  PICTURE X(2041).             ELSCMDLN
00122      EJECT                                                        ELSCMDLN
00123  PROCEDURE DIVISION.                                              ELSCMDLN
00124 ************************************************************      ELSCMDLN
00125 *                                                          *      ELSCMDLN
00126 *        PARSE COMMAND LINE                                *      ELSCMDLN
00127 *                                                          *      ELSCMDLN
00128 ************************************************************      ELSCMDLN
00129  PARSE-COMMAND-LINE.                                              ELSCMDLN
00130      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELSCMDLN
00131          EXEC CICS ABEND                                          ELSCMDLN
00132                    ABCODE('EL01')                                 ELSCMDLN
00133                    END-EXEC.                                      ELSCMDLN
00134      PERFORM INITIALIZATION.                                      ELSCMDLN
00135      PERFORM PROCESS-COMMAND-LINE.                                ELSCMDLN
00136      EXEC CICS RETURN                                             ELSCMDLN
00137                END-EXEC.                                          ELSCMDLN
00138                                                                   ELSCMDLN
00139                                                                   ELSCMDLN
00140 ************************************************************      ELSCMDLN
00141 *                                                          *      ELSCMDLN
00142 *        INITIALIZATION                                    *      ELSCMDLN
00143 *                                                          *      ELSCMDLN
00144 ************************************************************      ELSCMDLN
00145  INITIALIZATION.                                                  ELSCMDLN
00146      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSCMDLN
00147          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSCMDLN
00148      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSCMDLN
00149      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSCMDLN
00150          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSCMDLN
00151      MOVE LOW-VALUES TO SSB-CMDLN-DATA.                           ELSCMDLN
00152 /***********************************************************      ELSCMDLN
00153 *                                                          *      ELSCMDLN
00154 *        PROCESS COMMAND LINE                              *      ELSCMDLN
00155 *                                                          *      ELSCMDLN
00156 ************************************************************      ELSCMDLN
00157  PROCESS-COMMAND-LINE.                                            ELSCMDLN
00158      PERFORM RECEIVE-UNFORMATTED-SCREEN.                          ELSCMDLN
00159 * HEX-11 IS AN SET BUFFER ADDRESS                                 ELSCMDLN
00160 * IT WILL BE RECEIVED IF ELIQ ENTERED IMMEDIATELY                 ELSCMDLN
00161 * AFTER SIGN WITHOUT CLEARING THE SCREEN                          ELSCMDLN
00162      IF WS-SCR-MSG-LENGTH < 5 OR                                  ELSCMDLN
00163                 (SCR-MSG-CHAR (1) =  HEX-11 AND                   ELSCMDLN
00164                  WS-SCR-MSG-LENGTH < 8)                           ELSCMDLN
00165          PERFORM IGNORE-TRANSACTION-LINE                          ELSCMDLN
00166      ELSE                                                         ELSCMDLN
00167          PERFORM PROCESS-UNFORMATTED-SCREEN.                      ELSCMDLN
00168                                                                   ELSCMDLN
00169                                                                   ELSCMDLN
00170 ************************************************************      ELSCMDLN
00171 *                                                          *      ELSCMDLN
00172 *        RECEIVE UNFORMATTED SCREEN                        *      ELSCMDLN
00173 *                                                          *      ELSCMDLN
00174 ************************************************************      ELSCMDLN
00175  RECEIVE-UNFORMATTED-SCREEN.                                      ELSCMDLN
00176      EXEC CICS RECEIVE                                            ELSCMDLN
00177                SET(WS-SCR-MSG-POINTER)                            ELSCMDLN
00178                LENGTH(WS-SCR-MSG-LENGTH)                          ELSCMDLN
00179                END-EXEC.                                          ELSCMDLN
00180      SET ADDRESS OF SCR-MSG  TO  WS-SCR-MSG-POINTER.              ELSCMDLN
00181                                                                   ELSCMDLN
00182      PERFORM DROP-TRAILING-SPACES                                 ELSCMDLN
00183          VARYING SCR-IDX                                          ELSCMDLN
00184                  FROM WS-SCR-MSG-LENGTH BY -1                     ELSCMDLN
00185                  UNTIL SCR-MSG-CHAR (SCR-IDX) > SPACE             ELSCMDLN
00186                     OR WS-SCR-MSG-LENGTH = ZERO.                  ELSCMDLN
00187                                                                   ELSCMDLN
00188  DROP-TRAILING-SPACES.                                            ELSCMDLN
00189      SUBTRACT 1 FROM WS-SCR-MSG-LENGTH.                           ELSCMDLN
00190                                                                   ELSCMDLN
00191                                                                   ELSCMDLN
00192 ************************************************************      ELSCMDLN
00193 *                                                          *      ELSCMDLN
00194 *        PROCESS UNFORMATTED SCREEN                        *      ELSCMDLN
00195 *                                                          *      ELSCMDLN
00196 ************************************************************      ELSCMDLN
00197  PROCESS-UNFORMATTED-SCREEN.                                      ELSCMDLN
00198      PERFORM FIND-PARSE-STARTING-POS.                             ELSCMDLN
00199      IF WS-SCR-MSG-NOT-USED-UP                                    ELSCMDLN
00200              AND SCR-MSG-CHAR (SCR-IDX) = SPACE OR                ELSCMDLN
00201          LOW-VALUE                                                ELSCMDLN
00202          PERFORM EXTRACT-THE-DATA                                 ELSCMDLN
00203      ELSE                                                         ELSCMDLN
00204          PERFORM IGNORE-TRANSACTION-LINE.                         ELSCMDLN
00205 /***********************************************************      ELSCMDLN
00206 *                                                          *      ELSCMDLN
00207 *        FIND PARSE STARTING POS                           *      ELSCMDLN
00208 *                                                          *      ELSCMDLN
00209 ************************************************************      ELSCMDLN
00210  FIND-PARSE-STARTING-POS.                                         ELSCMDLN
00211      IF SCR-TRAN-ID-1 = EIBTRNID                                  ELSCMDLN
00212          SET SCR-IDX TO 5                                         ELSCMDLN
00213      ELSE IF SCR-MSG-CHAR (1) = HEX-11                            ELSCMDLN
00214              AND SCR-TRAN-ID-2 = EIBTRNID                         ELSCMDLN
00215          SET SCR-IDX TO 8                                         ELSCMDLN
00216      ELSE                                                         ELSCMDLN
00217          PERFORM IGNORE-TRANSACTION-LINE.                         ELSCMDLN
00218 /***********************************************************      ELSCMDLN
00219 *                                                          *      ELSCMDLN
00220 *        EXTRACT THE DATA                                  *      ELSCMDLN
00221 *                                                          *      ELSCMDLN
00222 ************************************************************      ELSCMDLN
00223  EXTRACT-THE-DATA.                                                ELSCMDLN
00224      PERFORM EXTRACT-GROUP-NUMBER.                                ELSCMDLN
00225      IF WS-SCR-MSG-NOT-USED-UP                                    ELSCMDLN
00226              AND SCR-MSG-CHAR (SCR-IDX) = '/'                     ELSCMDLN
00227          PERFORM EXTRACT-SECTION-NUMBER.                          ELSCMDLN
00228      IF WS-SCR-MSG-NOT-USED-UP                                    ELSCMDLN
00229              AND SCR-MSG-CHAR (SCR-IDX) = ' '                     ELSCMDLN
00230          PERFORM EXTRACT-SUBSCRIBER-NUMBER.                       ELSCMDLN
00231      EJECT                                                        ELSCMDLN
00232                                                                   ELSCMDLN
00233                                                                   ELSCMDLN
00234 ************************************************************      ELSCMDLN
00235 *                                                          *      ELSCMDLN
00236 *        EXTRACT GROUP NUMBER                              *      ELSCMDLN
00237 *                                                          *      ELSCMDLN
00238 ************************************************************      ELSCMDLN
00239  EXTRACT-GROUP-NUMBER.                                            ELSCMDLN
00240      SET WS-GROUP-IDX TO 1.                                       ELSCMDLN
00241      PERFORM INCREMENT-SCREEN-INDEX.                              ELSCMDLN
00242      SET WS-NUMERIC-INITIAL TO TRUE.                              ELSCMDLN
00243      MOVE SPACES TO WS-GROUP-NO.                                  ELSCMDLN
00244      PERFORM COPY-GROUP-CHARS                                     ELSCMDLN
00245          UNTIL (SCR-MSG-CHAR (SCR-IDX) = ' ' OR '/')              ELSCMDLN
00246                  OR WS-GROUP-IDX > LENGTH OF WS-GROUP-NO          ELSCMDLN
00247                  OR WS-SCR-MSG-USED-UP.                           ELSCMDLN
00248      IF WS-GROUP-IDX > LENGTH OF WS-GROUP-NO                      ELSCMDLN
00249          PERFORM IGNORE-PARAMETER                                 ELSCMDLN
00250      ELSE                                                         ELSCMDLN
00251          PERFORM JUSTIFY-GROUP-NUMBER.                            ELSCMDLN
00252 /***********************************************************      ELSCMDLN
00253 *                                                          *      ELSCMDLN
00254 *        COPY GROUP CHARS                                  *      ELSCMDLN
00255 *                                                          *      ELSCMDLN
00256 ************************************************************      ELSCMDLN
00257  COPY-GROUP-CHARS.                                                ELSCMDLN
00258      MOVE SCR-MSG-CHAR (SCR-IDX) TO                               ELSCMDLN
00259           WS-GROUP-CHAR (WS-GROUP-IDX).                           ELSCMDLN
00260      PERFORM CHECK-NUMERIC.                                       ELSCMDLN
00261      PERFORM INCREMENT-SCREEN-INDEX.                              ELSCMDLN
00262      SET WS-GROUP-IDX UP BY 1.                                    ELSCMDLN
00263                                                                   ELSCMDLN
00264                                                                   ELSCMDLN
00265 ************************************************************      ELSCMDLN
00266 *                                                          *      ELSCMDLN
00267 *        JUSTIFY GROUP NUMBER                              *      ELSCMDLN
00268 *                                                          *      ELSCMDLN
00269 ************************************************************      ELSCMDLN
00270  JUSTIFY-GROUP-NUMBER.                                            ELSCMDLN
00271 *    IF WS-NUMERIC-FIELD                                          ELSCMDLN
00272          PERFORM RIGHT-JUSTIFY-GROUP-NUMBER.                      ELSCMDLN
00273 *    ELSE                                                         ELSCMDLN
00274 *        MOVE WS-GROUP-NO TO SSB-CMDLN-GROUP-NUMBER.              ELSCMDLN
00275                                                                   ELSCMDLN
00276                                                                   ELSCMDLN
00277 ************************************************************      ELSCMDLN
00278 *                                                          *      ELSCMDLN
00279 *        RIGHT JUSTIFY GROUP NUMBER                        *      ELSCMDLN
00280 *                                                          *      ELSCMDLN
00281 ************************************************************      ELSCMDLN
00282  RIGHT-JUSTIFY-GROUP-NUMBER.                                      ELSCMDLN
00283      SET WS-MOVE-POS TO WS-GROUP-IDX.                             ELSCMDLN
00284      COMPUTE WS-MOVE-POS = LENGTH OF SSB-CMDLN-GROUP-NUMBER       ELSCMDLN
00285                       - WS-MOVE-POS + 2.                          ELSCMDLN
00286      MOVE ZEROS  TO  SSB-CMDLN-GROUP-NUMBER                       ELSCMDLN
00287      STRING WS-GROUP-NO    DELIMITED BY SIZE                      ELSCMDLN
00288         INTO SSB-CMDLN-GROUP-NUMBER                               ELSCMDLN
00289         POINTER WS-MOVE-POS.                                      ELSCMDLN
00290 /***********************************************************      ELSCMDLN
00291 *                                                          *      ELSCMDLN
00292 *        EXTRACT SECTION NUMBER                            *      ELSCMDLN
00293 *                                                          *      ELSCMDLN
00294 ************************************************************      ELSCMDLN
00295  EXTRACT-SECTION-NUMBER.                                          ELSCMDLN
00296      SET WS-SECTION-IDX TO 1.                                     ELSCMDLN
00297      PERFORM INCREMENT-SCREEN-INDEX.                              ELSCMDLN
00298      SET WS-NUMERIC-INITIAL TO TRUE.                              ELSCMDLN
00299      MOVE SPACES TO WS-SECTION-NO.                                ELSCMDLN
00300      PERFORM COPY-SECTION-CHARS                                   ELSCMDLN
00301          UNTIL SCR-MSG-CHAR (SCR-IDX) = ' '                       ELSCMDLN
00302                  OR WS-SECTION-IDX > LENGTH OF WS-SECTION-NO      ELSCMDLN
00303                  OR WS-SCR-MSG-USED-UP.                           ELSCMDLN
00304      IF WS-SECTION-IDX > LENGTH OF WS-SECTION-NO                  ELSCMDLN
00305          PERFORM IGNORE-PARAMETER                                 ELSCMDLN
00306      ELSE                                                         ELSCMDLN
00307          PERFORM JUSTIFY-SECTION-NUMBER.                          ELSCMDLN
00308                                                                   ELSCMDLN
00309                                                                   ELSCMDLN
00310 ************************************************************      ELSCMDLN
00311 *                                                          *      ELSCMDLN
00312 *        COPY SECTION CHARS                                *      ELSCMDLN
00313 *                                                          *      ELSCMDLN
00314 ************************************************************      ELSCMDLN
00315  COPY-SECTION-CHARS.                                              ELSCMDLN
00316      MOVE SCR-MSG-CHAR (SCR-IDX) TO                               ELSCMDLN
00317           WS-SECTION-CHAR (WS-SECTION-IDX).                       ELSCMDLN
00318      PERFORM CHECK-NUMERIC.                                       ELSCMDLN
00319      PERFORM INCREMENT-SCREEN-INDEX.                              ELSCMDLN
00320      SET WS-SECTION-IDX UP BY 1.                                  ELSCMDLN
00321                                                                   ELSCMDLN
00322                                                                   ELSCMDLN
00323 ************************************************************      ELSCMDLN
00324 *                                                          *      ELSCMDLN
00325 *        JUSTIFY SECTION NUMBER                            *      ELSCMDLN
00326 *                                                          *      ELSCMDLN
00327 ************************************************************      ELSCMDLN
00328  JUSTIFY-SECTION-NUMBER.                                          ELSCMDLN
00329      IF WS-NUMERIC-FIELD                                          ELSCMDLN
00330          PERFORM RIGHT-JUSTIFY-SECTION-NUMBER                     ELSCMDLN
00331      ELSE                                                         ELSCMDLN
00332          MOVE WS-SECTION-NO TO SSB-CMDLN-SECTN-NO.                ELSCMDLN
00333 /***********************************************************      ELSCMDLN
00334 *                                                          *      ELSCMDLN
00335 *        RIGHT JUSTIFY SECTION NUMBER                      *      ELSCMDLN
00336 *                                                          *      ELSCMDLN
00337 ************************************************************      ELSCMDLN
00338  RIGHT-JUSTIFY-SECTION-NUMBER.                                    ELSCMDLN
00339      SET WS-MOVE-POS TO WS-SECTION-IDX.                           ELSCMDLN
00340      COMPUTE WS-MOVE-POS = LENGTH OF SSB-SECTN-NO                 ELSCMDLN
00341                       - WS-MOVE-POS + 2.                          ELSCMDLN
00342      MOVE ZEROS  TO  SSB-CMDLN-SECTN-NO.                          ELSCMDLN
00343      STRING WS-SECTION-NO  DELIMITED BY SIZE                      ELSCMDLN
00344         INTO SSB-CMDLN-SECTN-NO                                   ELSCMDLN
00345         POINTER WS-MOVE-POS.                                      ELSCMDLN
00346 /***********************************************************      ELSCMDLN
00347 *                                                          *      ELSCMDLN
00348 *        EXTRACT SUBSCRIBER NUMBER                         *      ELSCMDLN
00349 *                                                          *      ELSCMDLN
00350 ************************************************************      ELSCMDLN
00351  EXTRACT-SUBSCRIBER-NUMBER.                                       ELSCMDLN
00352      SET WS-SUBSCR-IDX TO 1.                                      ELSCMDLN
00353      PERFORM INCREMENT-SCREEN-INDEX.                              ELSCMDLN
00354      SET WS-NUMERIC-INITIAL TO TRUE.                              ELSCMDLN
00355      MOVE SPACES TO WS-SUBSCR-NO.                                 ELSCMDLN
00356      PERFORM COPY-SUBSCR-CHARS                                    ELSCMDLN
00357          UNTIL SCR-MSG-CHAR (SCR-IDX) = ' '                       ELSCMDLN
00358                  OR WS-SUBSCR-IDX > LENGTH OF WS-SUBSCR-NO        ELSCMDLN
00359                  OR WS-SCR-MSG-USED-UP.                           ELSCMDLN
00360      IF WS-SUBSCR-IDX > LENGTH OF WS-SUBSCR-NO                    ELSCMDLN
00361          PERFORM IGNORE-PARAMETER                                 ELSCMDLN
00362      ELSE                                                         ELSCMDLN
00363          MOVE WS-SUBSCR-NO TO SSB-CMDLN-SUBSCRIBER-NBR.           ELSCMDLN
00364                                                                   ELSCMDLN
00365                                                                   ELSCMDLN
00366 ************************************************************      ELSCMDLN
00367 *                                                          *      ELSCMDLN
00368 *        COPY SUBSCR CHARS                                 *      ELSCMDLN
00369 *                                                          *      ELSCMDLN
00370 ************************************************************      ELSCMDLN
00371  COPY-SUBSCR-CHARS.                                               ELSCMDLN
00372      MOVE SCR-MSG-CHAR (SCR-IDX) TO                               ELSCMDLN
00373           WS-SUBSCR-CHAR (WS-SUBSCR-IDX).                         ELSCMDLN
00374      PERFORM CHECK-NUMERIC.                                       ELSCMDLN
00375      PERFORM INCREMENT-SCREEN-INDEX.                              ELSCMDLN
00376      SET WS-SUBSCR-IDX UP BY 1.                                   ELSCMDLN
00377 /***********************************************************      ELSCMDLN
00378 *                                                          *      ELSCMDLN
00379 *        CHECK NUMERIC                                     *      ELSCMDLN
00380 *                                                          *      ELSCMDLN
00381 ************************************************************      ELSCMDLN
00382  CHECK-NUMERIC.                                                   ELSCMDLN
00383      IF SCR-MSG-CHAR (SCR-IDX) < '0' OR > '9'                     ELSCMDLN
00384          SET WS-NON-NUMERIC-FIELD TO TRUE.                        ELSCMDLN
00385                                                                   ELSCMDLN
00386                                                                   ELSCMDLN
00387 ************************************************************      ELSCMDLN
00388 *                                                          *      ELSCMDLN
00389 *        INCREMENT SCREEN INDEX                            *      ELSCMDLN
00390 *                                                          *      ELSCMDLN
00391 ************************************************************      ELSCMDLN
00392  INCREMENT-SCREEN-INDEX.                                          ELSCMDLN
00393      SET SCR-IDX UP BY 1.                                         ELSCMDLN
00394      IF SCR-IDX > WS-SCR-MSG-LENGTH                               ELSCMDLN
00395          SET WS-SCR-MSG-USED-UP TO TRUE.                          ELSCMDLN
00396                                                                   ELSCMDLN
00397                                                                   ELSCMDLN
00398 ************************************************************      ELSCMDLN
00399 *                                                          *      ELSCMDLN
00400 *        IGNORE PARAMETER                                  *      ELSCMDLN
00401 *                                                          *      ELSCMDLN
00402 ************************************************************      ELSCMDLN
00403  IGNORE-PARAMETER.                                                ELSCMDLN
00404      CONTINUE.                                                    ELSCMDLN
00405                                                                   ELSCMDLN
00406                                                                   ELSCMDLN
00407 ************************************************************      ELSCMDLN
00408 *                                                          *      ELSCMDLN
00409 *        IGNORE TRANSACTION LINE                           *      ELSCMDLN
00410 *                                                          *      ELSCMDLN
00411 ************************************************************      ELSCMDLN
00412  IGNORE-TRANSACTION-LINE.                                         ELSCMDLN
00413      MOVE LOW-VALUES  TO  SSB-CMDLN-DATA.                         ELSCMDLN
