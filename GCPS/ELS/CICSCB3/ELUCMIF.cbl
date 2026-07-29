00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUCMIF 
00003  PROGRAM-ID.         ELUCMIF.                                        LV001
00004                                                                   ELUCMIF 
00005  AUTHOR.             EDWARD G LISS                                ELUCMIF 
00006                                                                   ELUCMIF 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUCMIF 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUCMIF 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUCMIF 
00010                      233 N. MICHIGAN AVE                          ELUCMIF 
00011                      CHICAGO, ILLINOIS 60601                      ELUCMIF 
00012                                                                   ELUCMIF 
00013  DATE-WRITTEN.       01-AUG-1988.                                 ELUCMIF 
00014                                                                   ELUCMIF 
00015  DATE-COMPILED.                                                   ELUCMIF 
00016                                                                   ELUCMIF 
00017  SECURITY.           COPYRIGHT 1988,                              ELUCMIF 
00018                      HEALTH CARE SERVICE CORPORATION              ELUCMIF 
00019      SKIP3                                                        ELUCMIF 
00020  TITLE 'ELS CODES MANUAL INTERFACE MODULE                 '.      ELUCMIF 
00021  ENVIRONMENT DIVISION.                                            ELUCMIF 
00022                                                                   ELUCMIF 
00023  CONFIGURATION SECTION.                                           ELUCMIF 
00024  SOURCE-COMPUTER.    IBM-3033.                                    ELUCMIF 
00025  OBJECT-COMPUTER.    IBM-3033.                                    ELUCMIF 
00026      EJECT                                                        ELUCMIF 
00027 ******************************************************************ELUCMIF 
00028 *                                                                *ELUCMIF 
00029 *    PROGRAM:    ELUCMIF                                         *ELUCMIF 
00030 *    DATE:       01-AUG-1986                                     *ELUCMIF 
00031 *    AUTHOR:     EDWARD G LISS                                   *ELUCMIF 
00032 *    FUNCTION:                                                   *ELUCMIF 
00033 *      THIS MODULE ACCESS THE CODES MANUAL TO TRANSLATE THE      *ELUCMIF 
00034 *      GIVEN CODE TO A ENGLISH PHASE.                            *ELUCMIF 
00035 *                                                                *ELUCMIF 
00036 *    NOTES:                                                      *ELUCMIF 
00037 *          THIS IS A REWRITE OF THE ORIGINAL WHICH WAS           *ELUCMIF 
00038 *    WRITTEN VERY POORLY AND FULL OF LOGIC HOLES.                *ELUCMIF 
00039 *                                                                *ELUCMIF 
00040 ******************************************************************ELUCMIF 
00041 *                                                                *ELUCMIF 
00042 *                      MAINTENANCE HISTORY                       *ELUCMIF 
00043 *                                                                *ELUCMIF 
00044 *  MOD     DATE     BY  DRPT                ACTION               *ELUCMIF 
00045 * ----- ----------- --- ----- ---------------------------------- *ELUCMIF 
00046 * 01.00 01-AUG-1988 EGL       RECREATED                          *ELUCMIF 
00047 * 01.01 17-OCT-1988 EGL       CORRECTED ASRA WHICH RESULTED      *ELUCMIF 
00048 *                             FROM ATTEMPTING TO USE A RECORD    *ELUCMIF 
00049 *                             AFTER AN END BROWSE WAS EXECUTED.  *ELUCMIF 
00050 * 02.00 16-NOV-1988 NAC       PAD OUT PREVIOUS USED LINES WITHIN *ELUCMIF 
00051 *                             ELSCMDSC TO SPACES PRIOR TO TRANS- *ELUCMIF 
00052 *                             LATION.                            *ELUCMIF 
00053 *                                                                *ELUCMIF 
00054 * 02.01 14-DEC-1988 LET       REMOVED THE LINE THAT CHECKS IF    *ELUCMIF 
00055 *                             CMF-CODE-VALUE FIELD IS BLANKS OR  *ELUCMIF 
00056 *                             LOW-VALUES TO COMPENSATE FOR ERROR *ELUCMIF 
00057 *                             IN GCPS FILES.                     *ELUCMIF 
00058 *                                                                *ELUCMIF 
00059 * 02.02 09-MAR-1990 AKK       ADDED CODE TO LOAD NOT FOUND       *ELUCMIF 
00060 *                             ELEMENTS OR VALUES. ALSO ADDED     *ELUCMIF 
00061 *                             CALL TO ELKLOG SUBROUTINE TO       *ELUCMIF 
00062 *                             WRITE THE ABOVE ELEMS/VALUES TO    *ELUCMIF 
00063 *                             THE SNAPSHOT FILE.                 *ELUCMIF 
00064 *                                                                *ELUCMIF 
00065 * 02.03 07-JUL-1990 AKK       CHANGED LINK TO ELUIOPGM WITH      *ELUCMIF 
00066 *                             A CALL.                            *ELUCMIF 
00067 *                                                                *ELUCMIF 
00068 * 02.04 17-JUL-1991 AKK       ADDED CODE TO BYPASS LOGGING OF    *ELUCMIF 
00069 *                             CONTRACT SUMMARY GROUPS NOT LOCKED *ELUCMIF 
00070 *                             OUT AS NOT FOUND ELEMENTS/VALUES   *ELUCMIF 
00071 *                                                                *ELUCMIF 
00072 * 02.05 26-OCT-1994 AKK       ADDED CODE TO BYPASS LOGGING OF    *ELUCMIF 
00073 *                             GROUPS LOCKED OUT OF ELIQ ALL      *ELUCMIF 
00074 *                             TOGETHER AS NOT FOUND CODE VALUES  *ELUCMIF 
00075 ******************************************************************ELUCMIF 
00076      EJECT                                                        ELUCMIF 
00077  DATA DIVISION.                                                   ELUCMIF 
00078                                                                   ELUCMIF 
00079  WORKING-STORAGE SECTION.                                         ELUCMIF 
00080                                                                   ELUCMIF 
00081  01  MISC-WORKING-STORAGE.                                        ELUCMIF 
00082      05  WS-MAX-DESCR-LINES         PICTURE S9(4) COMP.           ELUCMIF 
00083      05  WS-TRANS-COMPLETE-SW       PICTURE X     VALUE 'N'.      ELUCMIF 
00084          88  WS-TRANSLATION-COMPLETE              VALUE 'Y'.      ELUCMIF 
00085          88  WS-TRANSLATION-NOT-COMPLETE          VALUE 'N'.      ELUCMIF 
00086      05  WS-DUMMY-PTR               POINTER.                      ELUCMIF 
00087      05  WS-DATA-ELEMENT-MSG        PICTURE X(52) VALUE           ELUCMIF 
00088          'DATA ELEMENT IS NOT DEFINED; CONTACT E.L.S. SUPPORT'.   ELUCMIF 
00089      05  WS-CODE-VALUE-MSG          PICTURE X(52) VALUE           ELUCMIF 
00090          'CODE VALUE   IS NOT DEFINED; CONTACT E.L.S. SUPPORT'.   ELUCMIF 
00091      05  WS-LINE-COUNT              PICTURE S9(4) COMP.           ELUCMIF 
00092      05  WS-TRUNC-TEST.                                           ELUCMIF 
00093          10 WS-LOGICAL-END-TEST     PICTURE XX.                   ELUCMIF 
00094             88 WS-LOGICAL-END               VALUE '..'.           ELUCMIF 
00095          10 FILLER                  PICTURE X(73).                ELUCMIF 
00096          10 WS-SHORT-RECORD-FIX     PICTURE X(4).                 ELUCMIF 
00097      05  WS-RECORD-STATUS-SW        PICTURE X.                    ELUCMIF 
00098          88  WS-SHORT-RECORD                VALUE 'S'.            ELUCMIF 
00099          88  WS-NORMAL-RECORD               VALUE 'N'.            ELUCMIF 
00100      05  WS-PROCSSING-UNDEFINED     PICTURE X.                    ELUCMIF 
00101          88  WS-PROCESSING-ELEMENT          VALUE 'E'.            ELUCMIF 
00102          88  WS-PROCESSING-VALUE            VALUE 'V'.            ELUCMIF 
00103 * ABOVE BLOCK OF CODE FOR RECORD IO PATCH ROUTINE                 ELUCMIF 
00104 *                                                                 ELUCMIF 
00105 *                                                                 ELUCMIF 
00106      EJECT                                                        ELUCMIF 
00107  LINKAGE SECTION.                                                 ELUCMIF 
00108                                                                   ELUCMIF 
00109  01  DFHCOMMAREA.                                                 ELUCMIF 
00110  COPY ELSCOMMC.                                                   ELUCMIF 
00111      EJECT                                                        ELUCMIF 
00112  COPY ELSCIA2C.                                                   ELUCMIF 
00113      EJECT                                                        ELUCMIF 
00114  COPY ELSCMIFC.                                                   ELUCMIF 
00115      EJECT                                                        ELUCMIF 
00116  COPY ELSCMDSC.                                                   ELUCMIF 
00117      EJECT                                                        ELUCMIF 
00118  COPY ELSELOGC.                                                   ELUCMIF 
00119      EJECT                                                        ELUCMIF 
00120  COPY ELSIOPMC.                                                   ELUCMIF 
00121      EJECT                                                        ELUCMIF 
00122  COPY ELSKEYSC.                                                   ELUCMIF 
00123      EJECT                                                        ELUCMIF 
00124  01  CN-COBOL-NAME-RECORD.                                        ELUCMIF 
00125  COPY ELPCNC.                                                     ELUCMIF 
00126      EJECT                                                        ELUCMIF 
00127  01  CV-CODE-VALUE-RECORD.                                        ELUCMIF 
00128  COPY ELPCVC.                                                     ELUCMIF 
00129      EJECT                                                        ELUCMIF 
00130      EJECT                                                        ELUCMIF 
00131      EJECT                                                        ELUCMIF 
00132  PROCEDURE DIVISION.                                              ELUCMIF 
00133 ************************************************************      ELUCMIF 
00134 *                                                          *      ELUCMIF 
00135 *                    PROCEDURE DIVISION                    *      ELUCMIF 
00136 *                                                          *      ELUCMIF 
00137 ************************************************************      ELUCMIF 
00138                                                                   ELUCMIF 
00139                                                                   ELUCMIF 
00140 ************************************************************      ELUCMIF 
00141 *                                                          *      ELUCMIF 
00142 *        CODES MANUAL INTERFACE                            *      ELUCMIF 
00143 *                                                          *      ELUCMIF 
00144 ************************************************************      ELUCMIF 
00145  CODES-MANUAL-INTERFACE.                                          ELUCMIF 
00146      PERFORM INITIALIZATION.                                      ELUCMIF 
00147      PERFORM PROCESS-CODES-MANUAL.                                ELUCMIF 
00148      PERFORM TERMINATION.                                         ELUCMIF 
00149                                                                   ELUCMIF 
00150                                                                   ELUCMIF 
00151 ************************************************************      ELUCMIF 
00152 *                                                          *      ELUCMIF 
00153 *        INITIALIZATION                                    *      ELUCMIF 
00154 *                                                          *      ELUCMIF 
00155 ************************************************************      ELUCMIF 
00156  INITIALIZATION.                                                  ELUCMIF 
00157      IF LENGTH OF DFHCOMMAREA NOT = EIBCALEN                      ELUCMIF 
00158          PERFORM SIGNAL-COMMAREA-ERROR.                           ELUCMIF 
00159      PERFORM ESTABLISH-ADDRESS-OF-CIA.                            ELUCMIF 
00160      PERFORM ESTABLISH-ADDRESS-OF-CODES-REQ.                      ELUCMIF 
00161      PERFORM ESTABLISH-ADDRESS-OF-CODES-MAN.                      ELUCMIF 
00162      PERFORM ESTABLISH-ADDRESS-OF-KEY-WORKX.                      ELUCMIF 
00163      PERFORM ESTABLISH-ADDRESS-OF-LOG-AREA.                       ELUCMIF 
00164      PERFORM ESTABLISH-IO-BLOCKS.                                 ELUCMIF 
00165      SET WS-TRANSLATION-NOT-COMPLETE TO TRUE.                     ELUCMIF 
00166                                                                   ELUCMIF 
00167                                                                   ELUCMIF 
00168 ************************************************************      ELUCMIF 
00169 *                                                          *      ELUCMIF 
00170 *        ESTABLISH ADDRESS OF CIA                          *      ELUCMIF 
00171 *                                                          *      ELUCMIF 
00172 ************************************************************      ELUCMIF 
00173  ESTABLISH-ADDRESS-OF-CIA.                                        ELUCMIF 
00174      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUCMIF 
00175                 ADDRESS OF                                        ELUCMIF 
00176          CIA-ELS-COMMON-INTERFACE-AREA.                           ELUCMIF 
00177      EJECT                                                        ELUCMIF 
00178                                                                   ELUCMIF 
00179                                                                   ELUCMIF 
00180 ************************************************************      ELUCMIF 
00181 *                                                          *      ELUCMIF 
00182 *        ESTABLISH ADDRESS OF CODES MANUAL REQUEST BLOCK   *      ELUCMIF 
00183 *                                                          *      ELUCMIF 
00184 ************************************************************      ELUCMIF 
00185  ESTABLISH-ADDRESS-OF-CODES-REQ.                                  ELUCMIF 
00186      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELUCMIF 
00187      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCMIF 
00188                 ADDRESS OF CMF-CODES-MANUAL-INTERFACE.            ELUCMIF 
00189      IF NOT CIA-RC-OK                                             ELUCMIF 
00190          PERFORM SIGNAL-MISSING-AREA.                             ELUCMIF 
00191      EJECT                                                        ELUCMIF 
00192                                                                   ELUCMIF 
00193                                                                   ELUCMIF 
00194 ************************************************************      ELUCMIF 
00195 *                                                          *      ELUCMIF 
00196 *        ESTABLISH ADDRESS OF CODES MANUAL OUTPUT BLOCK    *      ELUCMIF 
00197 *                                                          *      ELUCMIF 
00198 ************************************************************      ELUCMIF 
00199  ESTABLISH-ADDRESS-OF-CODES-MAN.                                  ELUCMIF 
00200      PERFORM SET-ADDRESS-OF-CODES-MANUAL-OU.                      ELUCMIF 
00201      IF CIA-RC-PTR-NULL                                           ELUCMIF 
00202          PERFORM ALLOCATE-CODES-MANUAL-OUTPUT-B                   ELUCMIF 
00203      ELSE IF NOT CIA-RC-OK                                        ELUCMIF 
00204          PERFORM SIGNAL-LOGIC-ERROR.                              ELUCMIF 
00205      PERFORM PAD-ELSCMDSC-AREA                                    ELUCMIF 
00206          VARYING WS-LINE-COUNT FROM 1 BY 1                        ELUCMIF 
00207                   UNTIL WS-LINE-COUNT >                           ELUCMIF 
00208              CMF-NBR-DESCR-LINES.                                 ELUCMIF 
00209      MOVE ZEROS TO CMF-NBR-DESCR-LINES.                           ELUCMIF 
00210                                                                   ELUCMIF 
00211                                                                   ELUCMIF 
00212 ************************************************************      ELUCMIF 
00213 *                                                          *      ELUCMIF 
00214 *        PAD ELSCMDSC AREA                                 *      ELUCMIF 
00215 *                                                          *      ELUCMIF 
00216 ************************************************************      ELUCMIF 
00217  PAD-ELSCMDSC-AREA.                                               ELUCMIF 
00218      MOVE SPACES TO CMF-DESCR-LINE (WS-LINE-COUNT).               ELUCMIF 
00219      EJECT                                                        ELUCMIF 
00220                                                                   ELUCMIF 
00221                                                                   ELUCMIF 
00222 ************************************************************      ELUCMIF 
00223 *                                                          *      ELUCMIF 
00224 *        ALLOCATE CODES MANUAL OUTPUT BLOCK                *      ELUCMIF 
00225 *                                                          *      ELUCMIF 
00226 ************************************************************      ELUCMIF 
00227  ALLOCATE-CODES-MANUAL-OUTPUT-B.                                  ELUCMIF 
00228      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELUCMIF 
00229      SET CIA-STG-GETMAIN TO TRUE.                                 ELUCMIF 
00230      COMPUTE CIA-AREA-LEN = 2 +                                   ELUCMIF 
00231           (LENGTH OF CMF-DESCR-LINE *                             ELUCMIF 
00232          WS-MAX-DESCR-LINES).                                     ELUCMIF 
00233      PERFORM CALL-STORAGE-MANAGER.                                ELUCMIF 
00234      PERFORM SET-ADDRESS-OF-CODES-MANUAL-OU.                      ELUCMIF 
00235      MOVE ZEROS TO CMF-NBR-DESCR-LINES.                           ELUCMIF 
00236                                                                   ELUCMIF 
00237                                                                   ELUCMIF 
00238 ************************************************************      ELUCMIF 
00239 *                                                          *      ELUCMIF 
00240 *        SET ADDRESS OF CODES MANUAL OUTPUT BLOCK          *      ELUCMIF 
00241 *                                                          *      ELUCMIF 
00242 ************************************************************      ELUCMIF 
00243  SET-ADDRESS-OF-CODES-MANUAL-OU.                                  ELUCMIF 
00244      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELUCMIF 
00245      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCMIF 
00246                 ADDRESS OF CMF-DESCR.                             ELUCMIF 
00247      MOVE CIA-MVO TO WS-MAX-DESCR-LINES.                          ELUCMIF 
00248      EJECT                                                        ELUCMIF 
00249                                                                   ELUCMIF 
00250                                                                   ELUCMIF 
00251 ************************************************************      ELUCMIF 
00252 *                                                          *      ELUCMIF 
00253 *        ESTABLISH ADDRESS OF KEY WORK AREA                *      ELUCMIF 
00254 *                                                          *      ELUCMIF 
00255 ************************************************************      ELUCMIF 
00256  ESTABLISH-ADDRESS-OF-KEY-WORKX.                                  ELUCMIF 
00257      PERFORM SET-ADDRESS-OF-KEY-WORK-AREA.                        ELUCMIF 
00258      IF CIA-RC-PTR-NULL                                           ELUCMIF 
00259          PERFORM ALLOCATE-KEY-WORK-AREA                           ELUCMIF 
00260      ELSE IF NOT CIA-RC-OK                                        ELUCMIF 
00261          PERFORM SIGNAL-LOGIC-ERROR.                              ELUCMIF 
00262                                                                   ELUCMIF 
00263                                                                   ELUCMIF 
00264 ************************************************************      ELUCMIF 
00265 *                                                          *      ELUCMIF 
00266 *        ESTABLISH ADDRESS OF LOG RECORD AREA              *      ELUCMIF 
00267 *                                                          *      ELUCMIF 
00268 ************************************************************      ELUCMIF 
00269  ESTABLISH-ADDRESS-OF-LOG-AREA.                                   ELUCMIF 
00270      SET CIA-ELSELOG-DDN TO TRUE.                                 ELUCMIF 
00271      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCMIF 
00272                 ADDRESS OF LG-LOG-RECORD.                         ELUCMIF 
00273      IF CIA-RC-PTR-NULL                                           ELUCMIF 
00274          PERFORM ALLOCATE-LOG-REC-AREA                            ELUCMIF 
00275      ELSE IF NOT CIA-RC-OK                                        ELUCMIF 
00276          PERFORM SIGNAL-LOGIC-ERROR.                              ELUCMIF 
00277                                                                   ELUCMIF 
00278                                                                   ELUCMIF 
00279 ************************************************************      ELUCMIF 
00280 *                                                          *      ELUCMIF 
00281 *        ALLOCATE KEY WORK AREA                            *      ELUCMIF 
00282 *                                                          *      ELUCMIF 
00283 ************************************************************      ELUCMIF 
00284  ALLOCATE-KEY-WORK-AREA.                                          ELUCMIF 
00285      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELUCMIF 
00286      SET CIA-STG-GETMAIN TO TRUE.                                 ELUCMIF 
00287      PERFORM CALL-STORAGE-MANAGER.                                ELUCMIF 
00288      PERFORM SET-ADDRESS-OF-KEY-WORK-AREA.                        ELUCMIF 
00289                                                                   ELUCMIF 
00290                                                                   ELUCMIF 
00291 ************************************************************      ELUCMIF 
00292 *                                                          *      ELUCMIF 
00293 *        ALLOCATE LOG REC AREA                             *      ELUCMIF 
00294 *                                                          *      ELUCMIF 
00295 ************************************************************      ELUCMIF 
00296  ALLOCATE-LOG-REC-AREA.                                           ELUCMIF 
00297      COMPUTE CIA-AREA-LEN = LENGTH OF LG-LOG-RECORD               ELUCMIF 
00298      SET CIA-STG-GETMAIN TO TRUE.                                 ELUCMIF 
00299      PERFORM CALL-STORAGE-MANAGER.                                ELUCMIF 
00300      SET CIA-ELSELOG-DDN TO TRUE.                                 ELUCMIF 
00301      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCMIF 
00302                 ADDRESS OF LG-LOG-RECORD.                         ELUCMIF 
00303                                                                   ELUCMIF 
00304                                                                   ELUCMIF 
00305 ************************************************************      ELUCMIF 
00306 *                                                          *      ELUCMIF 
00307 *        SET ADDRESS OF KEY WORK AREA                      *      ELUCMIF 
00308 *                                                          *      ELUCMIF 
00309 ************************************************************      ELUCMIF 
00310  SET-ADDRESS-OF-KEY-WORK-AREA.                                    ELUCMIF 
00311      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELUCMIF 
00312      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCMIF 
00313                 ADDRESS OF KWA-FILE-KEY-WORK-AREA.                ELUCMIF 
00314      EJECT                                                        ELUCMIF 
00315                                                                   ELUCMIF 
00316                                                                   ELUCMIF 
00317 ************************************************************      ELUCMIF 
00318 *                                                          *      ELUCMIF 
00319 *        ESTABLISH IO BLOCKS                               *      ELUCMIF 
00320 *                                                          *      ELUCMIF 
00321 ************************************************************      ELUCMIF 
00322  ESTABLISH-IO-BLOCKS.                                             ELUCMIF 
00323      PERFORM ESTABLISH-ELPCN-BLOCK.                               ELUCMIF 
00324      PERFORM ESTABLISH-ELPCV-BLOCK.                               ELUCMIF 
00325                                                                   ELUCMIF 
00326                                                                   ELUCMIF 
00327 ************************************************************      ELUCMIF 
00328 *                                                          *      ELUCMIF 
00329 *        ESTABLISH ELPCN BLOCK                             *      ELUCMIF 
00330 *                                                          *      ELUCMIF 
00331 ************************************************************      ELUCMIF 
00332  ESTABLISH-ELPCN-BLOCK.                                           ELUCMIF 
00333      SET CIA-ELPCN-DDN TO TRUE.                                   ELUCMIF 
00334      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCMIF 
00335                            WS-DUMMY-PTR.                          ELUCMIF 
00336      IF CIA-RC-PTR-NULL                                           ELUCMIF 
00337          PERFORM ALLOCATE-IO-BLOCK                                ELUCMIF 
00338      ELSE IF NOT CIA-RC-OK                                        ELUCMIF 
00339          PERFORM SIGNAL-LOGIC-ERROR.                              ELUCMIF 
00340      EJECT                                                        ELUCMIF 
00341                                                                   ELUCMIF 
00342                                                                   ELUCMIF 
00343 ************************************************************      ELUCMIF 
00344 *                                                          *      ELUCMIF 
00345 *        ESTABLISH ELPCV BLOCK                             *      ELUCMIF 
00346 *                                                          *      ELUCMIF 
00347 ************************************************************      ELUCMIF 
00348  ESTABLISH-ELPCV-BLOCK.                                           ELUCMIF 
00349      SET CIA-ELPCV-DDN TO TRUE.                                   ELUCMIF 
00350      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCMIF 
00351                            WS-DUMMY-PTR.                          ELUCMIF 
00352      IF CIA-RC-PTR-NULL                                           ELUCMIF 
00353          PERFORM ALLOCATE-IO-BLOCK                                ELUCMIF 
00354      ELSE IF NOT CIA-RC-OK                                        ELUCMIF 
00355          PERFORM SIGNAL-LOGIC-ERROR.                              ELUCMIF 
00356                                                                   ELUCMIF 
00357                                                                   ELUCMIF 
00358                                                                   ELUCMIF 
00359                                                                   ELUCMIF 
00360 ************************************************************      ELUCMIF 
00361 *                                                          *      ELUCMIF 
00362 *        ALLOCATE IO BLOCK                                 *      ELUCMIF 
00363 *                                                          *      ELUCMIF 
00364 ************************************************************      ELUCMIF 
00365  ALLOCATE-IO-BLOCK.                                               ELUCMIF 
00366      SET CIA-STG-GETMAIN TO TRUE.                                 ELUCMIF 
00367      PERFORM CALL-STORAGE-MANAGER.                                ELUCMIF 
00368      EJECT                                                        ELUCMIF 
00369                                                                   ELUCMIF 
00370                                                                   ELUCMIF 
00371 ************************************************************      ELUCMIF 
00372 *                                                          *      ELUCMIF 
00373 *        PROCESS CODES MANUAL                              *      ELUCMIF 
00374 *                                                          *      ELUCMIF 
00375 ************************************************************      ELUCMIF 
00376  PROCESS-CODES-MANUAL.                                            ELUCMIF 
00377      PERFORM VALIDATE-REQUEST.                                    ELUCMIF 
00378      PERFORM OBTAIN-COBOL-NAME-RECORD.                            ELUCMIF 
00379      IF CMF-RC-OK                                                 ELUCMIF 
00380          PERFORM OBTAIN-CODE-TRANSLATION.                         ELUCMIF 
00381                                                                   ELUCMIF 
00382                                                                   ELUCMIF 
00383 ************************************************************      ELUCMIF 
00384 *                                                          *      ELUCMIF 
00385 *        VALIDATE REQUEST                                  *      ELUCMIF 
00386 *                                                          *      ELUCMIF 
00387 ************************************************************      ELUCMIF 
00388  VALIDATE-REQUEST.                                                ELUCMIF 
00389 *** CMF-CODE-VALUE CHECK REMOVED FROM HERE ***                    ELUCMIF 
00390      IF CMF-RECORD-PREFIX = SPACES OR LOW-VALUES                  ELUCMIF 
00391          PERFORM SIGNAL-PARAMETER-ERROR.                          ELUCMIF 
00392      IF CMF-ELEMENT-SYSTEM-NAME = SPACES OR LOW-VALUES            ELUCMIF 
00393          PERFORM SIGNAL-PARAMETER-ERROR.                          ELUCMIF 
00394      SET CMF-RC-OK TO TRUE.                                       ELUCMIF 
00395      EJECT                                                        ELUCMIF 
00396                                                                   ELUCMIF 
00397                                                                   ELUCMIF 
00398 ************************************************************      ELUCMIF 
00399 *                                                          *      ELUCMIF 
00400 *        OBTAIN COBOL NAME RECORD                          *      ELUCMIF 
00401 *                                                          *      ELUCMIF 
00402 ************************************************************      ELUCMIF 
00403  OBTAIN-COBOL-NAME-RECORD.                                        ELUCMIF 
00404      PERFORM CREATE-COBOL-NAME-RECORD-KEY.                        ELUCMIF 
00405      PERFORM READ-THE-COBOL-NAME-RECORD.                          ELUCMIF 
00406      IF IOP-RC-OK                                                 ELUCMIF 
00407          PERFORM ESTABLISH-ADDRESS-OF-COBOL-NAM                   ELUCMIF 
00408      ELSE                                                         ELUCMIF 
00409          PERFORM ISSUE-UNDEFINED-DATA-ELEMENT.                    ELUCMIF 
00410                                                                   ELUCMIF 
00411                                                                   ELUCMIF 
00412 ************************************************************      ELUCMIF 
00413 *                                                          *      ELUCMIF 
00414 *        CREATE COBOL NAME RECORD KEY                      *      ELUCMIF 
00415 *                                                          *      ELUCMIF 
00416 ************************************************************      ELUCMIF 
00417  CREATE-COBOL-NAME-RECORD-KEY.                                    ELUCMIF 
00418      MOVE LOW-VALUES              TO KWA-ELPCN-KEY.               ELUCMIF 
00419      MOVE CMF-RECORD-PREFIX       TO                              ELUCMIF 
00420          KWA-CN-RECORD-PREFIX.                                    ELUCMIF 
00421      MOVE CMF-ELEMENT-SYSTEM-NAME TO KWA-CN-COBOL-NAME.           ELUCMIF 
00422      EJECT                                                        ELUCMIF 
00423                                                                   ELUCMIF 
00424                                                                   ELUCMIF 
00425 ************************************************************      ELUCMIF 
00426 *                                                          *      ELUCMIF 
00427 *        READ THE COBOL NAME RECORD                        *      ELUCMIF 
00428 *                                                          *      ELUCMIF 
00429 ************************************************************      ELUCMIF 
00430  READ-THE-COBOL-NAME-RECORD.                                      ELUCMIF 
00431      SET CIA-ELPCN-DDN TO TRUE.                                   ELUCMIF 
00432      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCMIF 
00433              ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.              ELUCMIF 
00434      MOVE KWA-ELPCN-KEY TO IOP-FILE-KEY.                          ELUCMIF 
00435      MOVE SPACES        TO IOP-AIX-DDNAME.                        ELUCMIF 
00436      SET IOP-RD         TO TRUE.                                  ELUCMIF 
00437      SET IOP-FCQ-NONE   TO TRUE.                                  ELUCMIF 
00438      SET IOP-KVQ-EQ     TO TRUE.                                  ELUCMIF 
00439      PERFORM CALL-IO-PROGRAM.                                     ELUCMIF 
00440                                                                   ELUCMIF 
00441                                                                   ELUCMIF 
00442 ************************************************************      ELUCMIF 
00443 *                                                          *      ELUCMIF 
00444 *        ESTABLISH ADDRESS OF COBOL NAME RECORD            *      ELUCMIF 
00445 *                                                          *      ELUCMIF 
00446 ************************************************************      ELUCMIF 
00447  ESTABLISH-ADDRESS-OF-COBOL-NAM.                                  ELUCMIF 
00448      SET ADDRESS OF CN-COBOL-NAME-RECORD TO                       ELUCMIF 
00449          IOP-REC-PTR.                                             ELUCMIF 
00450                                                                   ELUCMIF 
00451                                                                   ELUCMIF 
00452 ************************************************************      ELUCMIF 
00453 *                                                          *      ELUCMIF 
00454 *        ISSUE UNDEFINED DATA ELEMENT                      *      ELUCMIF 
00455 *                                                          *      ELUCMIF 
00456 ************************************************************      ELUCMIF 
00457  ISSUE-UNDEFINED-DATA-ELEMENT.                                    ELUCMIF 
00458      MOVE 8 TO CMF-RETURN-CODE.                                   ELUCMIF 
00459      MOVE 1 TO CMF-NBR-DESCR-LINES.                               ELUCMIF 
00460      MOVE WS-DATA-ELEMENT-MSG TO CMF-DESCR-LINE (1).              ELUCMIF 
00461      SET WS-PROCESSING-ELEMENT TO TRUE.                           ELUCMIF 
00462      EVALUATE TRUE                                                ELUCMIF 
00463         WHEN CMF-RECORD-PREFIX = '@ELS'  AND                      ELUCMIF 
00464             CMF-ELEMENT-SYSTEM-NAME = 'LOCKOUT-GRP-SCTN-CS'       ELUCMIF 
00465              CONTINUE                                             ELUCMIF 
00466         WHEN CMF-RECORD-PREFIX = '@ELS' AND                       ELUCMIF 
00467             (CMF-ELEMENT-SYSTEM-NAME = 'LOCKOUT-GRP-SCTN-ALL')    ELUCMIF 
00468              CONTINUE                                             ELUCMIF 
00469         WHEN OTHER                                                ELUCMIF 
00470              PERFORM LOAD-INFO-FOR-SNAPSHOT-FILE                  ELUCMIF 
00471           END-EVALUATE.                                           ELUCMIF 
00472      EJECT                                                        ELUCMIF 
00473                                                                   ELUCMIF 
00474                                                                   ELUCMIF 
00475 ************************************************************      ELUCMIF 
00476 *                                                          *      ELUCMIF 
00477 *        OBTAIN CODE TRANSLATION                           *      ELUCMIF 
00478 *                                                          *      ELUCMIF 
00479 ************************************************************      ELUCMIF 
00480  OBTAIN-CODE-TRANSLATION.                                         ELUCMIF 
00481      PERFORM POSITION-CODE-VALUE-FILE.                            ELUCMIF 
00482      PERFORM PROCESS-CODE-VALUE-RECORDS.                          ELUCMIF 
00483      PERFORM RELEASE-CODE-VALUE-FILE-POSITI.                      ELUCMIF 
00484                                                                   ELUCMIF 
00485                                                                   ELUCMIF 
00486 ************************************************************      ELUCMIF 
00487 *                                                          *      ELUCMIF 
00488 *        POSITION CODE VALUE FILE                          *      ELUCMIF 
00489 *                                                          *      ELUCMIF 
00490 ************************************************************      ELUCMIF 
00491  POSITION-CODE-VALUE-FILE.                                        ELUCMIF 
00492      PERFORM ESTABLISH-CODE-VALUE-FILE-BLOC.                      ELUCMIF 
00493      PERFORM CREATE-CODE-VALUE-RECORD-KEY.                        ELUCMIF 
00494      PERFORM START-BROWSING-CODE-VALUE-FILE.                      ELUCMIF 
00495                                                                   ELUCMIF 
00496                                                                   ELUCMIF 
00497 ************************************************************      ELUCMIF 
00498 *                                                          *      ELUCMIF 
00499 *        ESTABLISH CODE VALUE FILE BLOCK                   *      ELUCMIF 
00500 *                                                          *      ELUCMIF 
00501 ************************************************************      ELUCMIF 
00502  ESTABLISH-CODE-VALUE-FILE-BLOC.                                  ELUCMIF 
00503      SET CIA-ELPCV-DDN TO TRUE.                                   ELUCMIF 
00504      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCMIF 
00505                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELUCMIF 
00506                                                                   ELUCMIF 
00507                                                                   ELUCMIF 
00508 ************************************************************      ELUCMIF 
00509 *                                                          *      ELUCMIF 
00510 *        CREATE CODE VALUE RECORD KEY                      *      ELUCMIF 
00511 *                                                          *      ELUCMIF 
00512 ************************************************************      ELUCMIF 
00513  CREATE-CODE-VALUE-RECORD-KEY.                                    ELUCMIF 
00514      MOVE LOW-VALUES              TO KWA-ELPCV-KEY.               ELUCMIF 
00515      MOVE CMF-RECORD-PREFIX       TO                              ELUCMIF 
00516          KWA-CV-RECORD-PREFIX.                                    ELUCMIF 
00517      MOVE CN-ELEMENT-NBR          TO KWA-CV-ELEMENT-NBR.          ELUCMIF 
00518      MOVE CMF-CODE-VALUE          TO KWA-CV-CODE-VALUE.           ELUCMIF 
00519      MOVE 1                       TO KWA-CV-CODE-DESC-SEQ.        ELUCMIF 
00520      EJECT                                                        ELUCMIF 
00521                                                                   ELUCMIF 
00522                                                                   ELUCMIF 
00523 ************************************************************      ELUCMIF 
00524 *                                                          *      ELUCMIF 
00525 *        START BROWSING CODE VALUE FILE                    *      ELUCMIF 
00526 *                                                          *      ELUCMIF 
00527 ************************************************************      ELUCMIF 
00528  START-BROWSING-CODE-VALUE-FILE.                                  ELUCMIF 
00529      MOVE KWA-ELPCV-KEY TO IOP-FILE-KEY.                          ELUCMIF 
00530      SET CIA-ELPCV-DDN  TO TRUE.                                  ELUCMIF 
00531      MOVE SPACES        TO IOP-AIX-DDNAME.                        ELUCMIF 
00532      SET IOP-ST-BR      TO TRUE.                                  ELUCMIF 
00533      SET IOP-FCQ-NONE   TO TRUE.                                  ELUCMIF 
00534      SET IOP-KVQ-GTE    TO TRUE.                                  ELUCMIF 
00535      PERFORM CALL-IO-PROGRAM.                                     ELUCMIF 
00536      IF NOT IOP-RC-OK                                             ELUCMIF 
00537          PERFORM SIGNAL-BROWSE-ERROR.                             ELUCMIF 
00538      EJECT                                                        ELUCMIF 
00539                                                                   ELUCMIF 
00540                                                                   ELUCMIF 
00541 ************************************************************      ELUCMIF 
00542 *                                                          *      ELUCMIF 
00543 *        PROCESS CODE VALUE RECORDS                        *      ELUCMIF 
00544 *                                                          *      ELUCMIF 
00545 ************************************************************      ELUCMIF 
00546  PROCESS-CODE-VALUE-RECORDS.                                      ELUCMIF 
00547      PERFORM READ-TRANSLATION-RECORD.                             ELUCMIF 
00548      PERFORM MOVE-TRANSLATION-TO-OUTPUT-BLO                       ELUCMIF 
00549          UNTIL WS-TRANSLATION-COMPLETE.                           ELUCMIF 
00550      IF CMF-NBR-DESCR-LINES = ZERO                                ELUCMIF 
00551          PERFORM INDICATE-NO-TRANSLATION.                         ELUCMIF 
00552                                                                   ELUCMIF 
00553                                                                   ELUCMIF 
00554 ************************************************************      ELUCMIF 
00555 *                                                          *      ELUCMIF 
00556 *        MOVE TRANSLATION TO OUTPUT BLOCK                  *      ELUCMIF 
00557 *                                                          *      ELUCMIF 
00558 ************************************************************      ELUCMIF 
00559  MOVE-TRANSLATION-TO-OUTPUT-BLO.                                  ELUCMIF 
00560      PERFORM MOVE-TRANSLATION-LINES                               ELUCMIF 
00561          VARYING WS-LINE-COUNT FROM 1 BY 1                        ELUCMIF 
00562                     UNTIL WS-LINE-COUNT >                         ELUCMIF 
00563              CV-NBR-VALUE-DESC-LINES                              ELUCMIF 
00564                        OR WS-TRANSLATION-COMPLETE.                ELUCMIF 
00565      IF WS-TRANSLATION-NOT-COMPLETE                               ELUCMIF 
00566          PERFORM READ-TRANSLATION-RECORD.                         ELUCMIF 
00567      EJECT                                                        ELUCMIF 
00568                                                                   ELUCMIF 
00569                                                                   ELUCMIF 
00570 ************************************************************      ELUCMIF 
00571 *                                                          *      ELUCMIF 
00572 *        MOVE TRANSLATION LINES                            *      ELUCMIF 
00573 *                                                          *      ELUCMIF 
00574 ************************************************************      ELUCMIF 
00575  MOVE-TRANSLATION-LINES.                                          ELUCMIF 
00576      MOVE CV-VALUE-DESC-LINE (WS-LINE-COUNT) TO                   ELUCMIF 
00577          WS-TRUNC-TEST.                                           ELUCMIF 
00578      IF WS-LINE-COUNT = CV-NBR-VALUE-DESC-LINES                   ELUCMIF 
00579          PERFORM PROCESS-SHORT-RECORD.                            ELUCMIF 
00580      IF WS-LOGICAL-END                                            ELUCMIF 
00581          PERFORM INDICATE-TRANSLATION-COMPLETE                    ELUCMIF 
00582      ELSE                                                         ELUCMIF 
00583          PERFORM APPEND-THE-TRANSLATION-LINE.                     ELUCMIF 
00584                                                                   ELUCMIF 
00585                                                                   ELUCMIF 
00586 ************************************************************      ELUCMIF 
00587 *                                                          *      ELUCMIF 
00588 *        APPEND THE TRANSLATION LINE                       *      ELUCMIF 
00589 *                                                          *      ELUCMIF 
00590 ************************************************************      ELUCMIF 
00591  APPEND-THE-TRANSLATION-LINE.                                     ELUCMIF 
00592      ADD 1 TO CMF-NBR-DESCR-LINES.                                ELUCMIF 
00593      MOVE WS-TRUNC-TEST TO CMF-DESCR-LINE                         ELUCMIF 
00594          (CMF-NBR-DESCR-LINES).                                   ELUCMIF 
00595      IF CMF-NBR-DESCR-LINES = WS-MAX-DESCR-LINES                  ELUCMIF 
00596          PERFORM INDICATE-TRANSLATION-COMPLETE.                   ELUCMIF 
00597                                                                   ELUCMIF 
00598                                                                   ELUCMIF 
00599 ************************************************************      ELUCMIF 
00600 *                                                          *      ELUCMIF 
00601 *        READ TRANSLATION RECORD                           *      ELUCMIF 
00602 *                                                          *      ELUCMIF 
00603 ************************************************************      ELUCMIF 
00604  READ-TRANSLATION-RECORD.                                         ELUCMIF 
00605      PERFORM READ-TRANSLATION-INITIALIZATIO.                      ELUCMIF 
00606      PERFORM READ-TRANSLATION-FILE.                               ELUCMIF 
00607                                                                   ELUCMIF 
00608                                                                   ELUCMIF 
00609 ************************************************************      ELUCMIF 
00610 *                                                          *      ELUCMIF 
00611 *        READ TRANSLATION INITIALIZATION                   *      ELUCMIF 
00612 *                                                          *      ELUCMIF 
00613 ************************************************************      ELUCMIF 
00614  READ-TRANSLATION-INITIALIZATIO.                                  ELUCMIF 
00615      SET CIA-ELPCV-DDN TO TRUE.                                   ELUCMIF 
00616      SET IOP-RD-NXT TO TRUE.                                      ELUCMIF 
00617      SET IOP-FCQ-NONE TO TRUE.                                    ELUCMIF 
00618      SET IOP-KVQ-NONE TO TRUE.                                    ELUCMIF 
00619      EJECT                                                        ELUCMIF 
00620                                                                   ELUCMIF 
00621                                                                   ELUCMIF 
00622 ************************************************************      ELUCMIF 
00623 *                                                          *      ELUCMIF 
00624 *        READ TRANSLATION FILE                             *      ELUCMIF 
00625 *                                                          *      ELUCMIF 
00626 ************************************************************      ELUCMIF 
00627  READ-TRANSLATION-FILE.                                           ELUCMIF 
00628      PERFORM CALL-IO-PROGRAM.                                     ELUCMIF 
00629      IF IOP-RC-OK                                                 ELUCMIF 
00630          PERFORM TEST-FOR-END-OF-TRANSLATION                      ELUCMIF 
00631      ELSE IF IOP-RC-ENDFILE                                       ELUCMIF 
00632          PERFORM INDICATE-TRANSLATION-COMPLETE                    ELUCMIF 
00633      ELSE                                                         ELUCMIF 
00634          PERFORM SIGNAL-LOGIC-ERROR.                              ELUCMIF 
00635      PERFORM EXECUTE-IO-PATCH.                                    ELUCMIF 
00636      EJECT                                                        ELUCMIF 
00637                                                                   ELUCMIF 
00638                                                                   ELUCMIF 
00639 ************************************************************      ELUCMIF 
00640 *                                                          *      ELUCMIF 
00641 *        EXECUTE IO PATCH                                  *      ELUCMIF 
00642 *                                                          *      ELUCMIF 
00643 ************************************************************      ELUCMIF 
00644  EXECUTE-IO-PATCH.                                                ELUCMIF 
00645      IF IOP-REC-LEN < 76                                          ELUCMIF 
00646          PERFORM SIGNAL-IO-ERROR.                                 ELUCMIF 
00647      IF IOP-REC-LEN <                                             ELUCMIF 
00648              (76 + CV-NBR-VALUE-DESC-LINES * LENGTH OF            ELUCMIF 
00649          CV-VALUE-DESC-LINE)                                      ELUCMIF 
00650          PERFORM INDICATE-SHORT-RECORD                            ELUCMIF 
00651      ELSE                                                         ELUCMIF 
00652          PERFORM INDICATE-NORMAL-RECORD.                          ELUCMIF 
00653                                                                   ELUCMIF 
00654                                                                   ELUCMIF 
00655 ************************************************************      ELUCMIF 
00656 *                                                          *      ELUCMIF 
00657 *        SIGNAL IO ERROR                                   *      ELUCMIF 
00658 *                                                          *      ELUCMIF 
00659 ************************************************************      ELUCMIF 
00660  SIGNAL-IO-ERROR.                                                 ELUCMIF 
00661      SET CIA-AB-CRITIO TO TRUE.                                   ELUCMIF 
00662      EXEC CICS ABEND                                              ELUCMIF 
00663                ABCODE(CIA-ABCODE)                                 ELUCMIF 
00664         END-EXEC.                                                 ELUCMIF 
00665                                                                   ELUCMIF 
00666                                                                   ELUCMIF 
00667 ************************************************************      ELUCMIF 
00668 *                                                          *      ELUCMIF 
00669 *        INDICATE SHORT RECORD                             *      ELUCMIF 
00670 *                                                          *      ELUCMIF 
00671 ************************************************************      ELUCMIF 
00672  INDICATE-SHORT-RECORD.                                           ELUCMIF 
00673      SET WS-SHORT-RECORD TO TRUE.                                 ELUCMIF 
00674                                                                   ELUCMIF 
00675                                                                   ELUCMIF 
00676 ************************************************************      ELUCMIF 
00677 *                                                          *      ELUCMIF 
00678 *        INDICATE NORMAL RECORD                            *      ELUCMIF 
00679 *                                                          *      ELUCMIF 
00680 ************************************************************      ELUCMIF 
00681  INDICATE-NORMAL-RECORD.                                          ELUCMIF 
00682      SET WS-NORMAL-RECORD TO TRUE.                                ELUCMIF 
00683                                                                   ELUCMIF 
00684                                                                   ELUCMIF 
00685 ************************************************************      ELUCMIF 
00686 *                                                          *      ELUCMIF 
00687 *        PROCESS SHORT RECORD                              *      ELUCMIF 
00688 *                                                          *      ELUCMIF 
00689 ************************************************************      ELUCMIF 
00690  PROCESS-SHORT-RECORD.                                            ELUCMIF 
00691      IF WS-SHORT-RECORD                                           ELUCMIF 
00692           MOVE SPACES TO WS-SHORT-RECORD-FIX                      ELUCMIF 
00693        END-IF.                                                    ELUCMIF 
00694                                                                   ELUCMIF 
00695                                                                   ELUCMIF 
00696 ************************************************************      ELUCMIF 
00697 *                                                          *      ELUCMIF 
00698 *        TEST FOR END OF TRANSLATION                       *      ELUCMIF 
00699 *                                                          *      ELUCMIF 
00700 ************************************************************      ELUCMIF 
00701  TEST-FOR-END-OF-TRANSLATION.                                     ELUCMIF 
00702      SET ADDRESS OF CV-CODE-VALUE-RECORD TO                       ELUCMIF 
00703          IOP-REC-PTR.                                             ELUCMIF 
00704      IF CV-RECORD-PREFIX NOT = CMF-RECORD-PREFIX                  ELUCMIF 
00705               OR CV-ELEMENT-NBR NOT = CN-ELEMENT-NBR              ELUCMIF 
00706               OR CV-CODE-VALUE NOT = CMF-CODE-VALUE               ELUCMIF 
00707          PERFORM INDICATE-TRANSLATION-COMPLETE.                   ELUCMIF 
00708                                                                   ELUCMIF 
00709                                                                   ELUCMIF 
00710 ************************************************************      ELUCMIF 
00711 *                                                          *      ELUCMIF 
00712 *        INDICATE TRANSLATION COMPLETE                     *      ELUCMIF 
00713 *                                                          *      ELUCMIF 
00714 ************************************************************      ELUCMIF 
00715  INDICATE-TRANSLATION-COMPLETE.                                   ELUCMIF 
00716      SET WS-TRANSLATION-COMPLETE TO TRUE.                         ELUCMIF 
00717      EJECT                                                        ELUCMIF 
00718                                                                   ELUCMIF 
00719                                                                   ELUCMIF 
00720 ************************************************************      ELUCMIF 
00721 *                                                          *      ELUCMIF 
00722 *        INDICATE NO TRANSLATION                           *      ELUCMIF 
00723 *                                                          *      ELUCMIF 
00724 ************************************************************      ELUCMIF 
00725  INDICATE-NO-TRANSLATION.                                         ELUCMIF 
00726      MOVE 8 TO CMF-RETURN-CODE.                                   ELUCMIF 
00727      MOVE 1 TO CMF-NBR-DESCR-LINES.                               ELUCMIF 
00728      MOVE WS-CODE-VALUE-MSG TO CMF-DESCR-LINE (1).                ELUCMIF 
00729      SET WS-PROCESSING-VALUE TO TRUE.                             ELUCMIF 
00730      EVALUATE TRUE                                                ELUCMIF 
00731         WHEN CMF-RECORD-PREFIX  = '@ELS' AND                      ELUCMIF 
00732             CMF-ELEMENT-SYSTEM-NAME = 'LOCKOUT-GRP-SCTN-CS'       ELUCMIF 
00733              CONTINUE                                             ELUCMIF 
00734         WHEN CMF-RECORD-PREFIX = '@ELS' AND                       ELUCMIF 
00735             (CMF-ELEMENT-SYSTEM-NAME = 'LOCKOUT-GRP-SCTN-ALL')    ELUCMIF 
00736              CONTINUE                                             ELUCMIF 
00737         WHEN OTHER                                                ELUCMIF 
00738              PERFORM LOAD-INFO-FOR-SNAPSHOT-FILE                  ELUCMIF 
00739           END-EVALUATE.                                           ELUCMIF 
00740      EJECT                                                        ELUCMIF 
00741                                                                   ELUCMIF 
00742                                                                   ELUCMIF 
00743 ************************************************************      ELUCMIF 
00744 *                                                          *      ELUCMIF 
00745 *        RELEASE CODE VALUE FILE POSITION                  *      ELUCMIF 
00746 *                                                          *      ELUCMIF 
00747 ************************************************************      ELUCMIF 
00748  RELEASE-CODE-VALUE-FILE-POSITI.                                  ELUCMIF 
00749      SET CIA-ELPCV-DDN TO TRUE.                                   ELUCMIF 
00750      SET IOP-END-BR TO TRUE.                                      ELUCMIF 
00751      PERFORM CALL-IO-PROGRAM.                                     ELUCMIF 
00752                                                                   ELUCMIF 
00753                                                                   ELUCMIF 
00754 ************************************************************      ELUCMIF 
00755 *                                                          *      ELUCMIF 
00756 *        CALL STORAGE MANAGER                              *      ELUCMIF 
00757 *                                                          *      ELUCMIF 
00758 ************************************************************      ELUCMIF 
00759  CALL-STORAGE-MANAGER.                                            ELUCMIF 
00760      EXEC CICS LINK PROGRAM('ELUSTGMG')                           ELUCMIF 
00761                    COMMAREA(DFHCOMMAREA)                          ELUCMIF 
00762         END-EXEC.                                                 ELUCMIF 
00763                                                                   ELUCMIF 
00764                                                                   ELUCMIF 
00765 ************************************************************      ELUCMIF 
00766 *                                                          *      ELUCMIF 
00767 *        CALL IO PROGRAM                                   *      ELUCMIF 
00768 *                                                          *      ELUCMIF 
00769 ************************************************************      ELUCMIF 
00770  CALL-IO-PROGRAM.                                                 ELUCMIF 
00771      CALL 'ELUIOPGM' USING DFHEIBLK,                              ELUCMIF 
00772                            DFHCOMMAREA.                           ELUCMIF 
00773                                                                   ELUCMIF 
00774                                                                   ELUCMIF 
00775 ************************************************************      ELUCMIF 
00776 *                                                          *      ELUCMIF 
00777 *        TERMINATION                                       *      ELUCMIF 
00778 *                                                          *      ELUCMIF 
00779 ************************************************************      ELUCMIF 
00780  TERMINATION.                                                     ELUCMIF 
00781      GOBACK.                                                      ELUCMIF 
00782      EJECT                                                        ELUCMIF 
00783                                                                   ELUCMIF 
00784                                                                   ELUCMIF 
00785 ************************************************************      ELUCMIF 
00786 *                                                          *      ELUCMIF 
00787 *        SIGNAL COMMAREA ERROR                             *      ELUCMIF 
00788 *                                                          *      ELUCMIF 
00789 ************************************************************      ELUCMIF 
00790  SIGNAL-COMMAREA-ERROR.                                           ELUCMIF 
00791      EXEC CICS ABEND                                              ELUCMIF 
00792                ABCODE('EL01')                                     ELUCMIF 
00793                END-EXEC.                                          ELUCMIF 
00794                                                                   ELUCMIF 
00795                                                                   ELUCMIF 
00796 ************************************************************      ELUCMIF 
00797 *                                                          *      ELUCMIF 
00798 *        SIGNAL PARAMETER ERROR                            *      ELUCMIF 
00799 *                                                          *      ELUCMIF 
00800 ************************************************************      ELUCMIF 
00801  SIGNAL-PARAMETER-ERROR.                                          ELUCMIF 
00802      SET CIA-AB-PARM-ERR TO TRUE.                                 ELUCMIF 
00803      EXEC CICS ABEND                                              ELUCMIF 
00804                ABCODE(CIA-ABCODE)                                 ELUCMIF 
00805                END-EXEC.                                          ELUCMIF 
00806                                                                   ELUCMIF 
00807                                                                   ELUCMIF 
00808 ************************************************************      ELUCMIF 
00809 *                                                          *      ELUCMIF 
00810 *        SIGNAL MISSING AREA                               *      ELUCMIF 
00811 *                                                          *      ELUCMIF 
00812 ************************************************************      ELUCMIF 
00813  SIGNAL-MISSING-AREA.                                             ELUCMIF 
00814      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUCMIF 
00815      EXEC CICS ABEND                                              ELUCMIF 
00816                ABCODE(CIA-ABCODE)                                 ELUCMIF 
00817                END-EXEC.                                          ELUCMIF 
00818                                                                   ELUCMIF 
00819                                                                   ELUCMIF 
00820 ************************************************************      ELUCMIF 
00821 *                                                          *      ELUCMIF 
00822 *        SIGNAL LOGIC ERROR                                *      ELUCMIF 
00823 *                                                          *      ELUCMIF 
00824 ************************************************************      ELUCMIF 
00825  SIGNAL-LOGIC-ERROR.                                              ELUCMIF 
00826      SET CIA-AB-UNDEF TO TRUE.                                    ELUCMIF 
00827      EXEC CICS ABEND                                              ELUCMIF 
00828                ABCODE(CIA-ABCODE)                                 ELUCMIF 
00829                END-EXEC.                                          ELUCMIF 
00830                                                                   ELUCMIF 
00831                                                                   ELUCMIF 
00832 ************************************************************      ELUCMIF 
00833 *                                                          *      ELUCMIF 
00834 *        SIGNAL BROWSE ERROR                               *      ELUCMIF 
00835 *                                                          *      ELUCMIF 
00836 ************************************************************      ELUCMIF 
00837  SIGNAL-BROWSE-ERROR.                                             ELUCMIF 
00838      SET CIA-AB-NOTFND-ELPCV TO TRUE.                             ELUCMIF 
00839      EXEC CICS ABEND                                              ELUCMIF 
00840                ABCODE(CIA-ABCODE)                                 ELUCMIF 
00841                END-EXEC.                                          ELUCMIF 
00842      EJECT                                                        ELUCMIF 
00843 ************************************************************      ELUCMIF 
00844 *                                                          *      ELUCMIF 
00845 *        LOAD-INFO-FOR-SNAPSHOT-FILE                       *      ELUCMIF 
00846 *                                                          *      ELUCMIF 
00847 ************************************************************      ELUCMIF 
00848  LOAD-INFO-FOR-SNAPSHOT-FILE.                                     ELUCMIF 
00849      INITIALIZE LG-LOG-RECORD.                                    ELUCMIF 
00850      IF WS-PROCESSING-ELEMENT                                     ELUCMIF 
00851         SET LG-C-M-ELEMENT TO TRUE                                ELUCMIF 
00852         MOVE CMF-RECORD-PREFIX TO LG-RECORD-PREFIX                ELUCMIF 
00853         MOVE CMF-ELEMENT-SYSTEM-NAME TO LG-ELEMENT-NAME           ELUCMIF 
00854      ELSE                                                         ELUCMIF 
00855        IF WS-PROCESSING-VALUE                                     ELUCMIF 
00856           SET LG-C-M-VALUE TO TRUE                                ELUCMIF 
00857           MOVE CMF-RECORD-PREFIX TO LG-RECORD-PREFIX              ELUCMIF 
00858           MOVE CMF-ELEMENT-SYSTEM-NAME TO LG-ELEMENT-NAME         ELUCMIF 
00859           MOVE CN-ELEMENT-NBR TO LG-DE-NUMBER                     ELUCMIF 
00860           MOVE CMF-CODE-VALUE TO LG-CODE-VALUE.                   ELUCMIF 
00861      MOVE LENGTH OF LG-LOG-RECORD TO LG-LOG-LENGTH.               ELUCMIF 
00862      CALL 'ELKLOG' USING DFHEIBLK                                 ELUCMIF 
00863                          DFHCOMMAREA.                             ELUCMIF 
00864 *                                                                 ELUCMIF 
00865  GOBACK-PARAGRAPH.                                                ELUCMIF 
00866 ************************************************************      ELUCMIF 
00867 *                                                          *      ELUCMIF 
00868 *                         STOP RUN                         *      ELUCMIF 
00869 *                                                          *      ELUCMIF 
00870 ************************************************************      ELUCMIF 
00871      GOBACK.                                                      ELUCMIF 
