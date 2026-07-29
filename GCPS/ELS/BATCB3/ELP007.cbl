00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.    ELP007.                                           ELP007  
00003  AUTHOR.        ALIDA JATICH OF T. M. FLOYD, INC.                    LV001
00004  INSTALLATION.  HCSC - BLUE CROSS AND BLUE SHIELD OF ILLINOIS.    ELP007  
00005  DATE-WRITTEN.  NOV. 6, 1985.                                     ELP007  
00006  DATE-COMPILED.                                                   ELP007  
00007 ***************************************************************** ELP007  
00008 * THIS PROGRAM EXTRACTS RECORDS FROM THE RECORD LIST, DATA      * ELP007  
00009 * ELEMENT, AND CODE VALUE FILES AND PLACES THEM ON A WORK       * ELP007  
00010 * FILE FOR MASS UPDATE.  DEPENDING ON THE FIRST PARAMETER       * ELP007  
00011 * CARD, THIS PROGRAM CAN BE RUN IN EITHER OF THREE MODES:       * ELP007  
00012 *                                                               * ELP007  
00013 * 'A', ALL RECORDS ON FILE: IN THIS MODE, THE PROGRAM WILL      * ELP007  
00014 *    GO THROUGH THE ENTIRE RECORD LIST FILE.  IT WILL COPY      * ELP007  
00015 *    THE RECORD LIST RECORD AND ALL DATA ELEMENT AND CODE       * ELP007  
00016 *    VALUE RECORDS, IN HIERARCHICAL ORDER, TO THE OUTPUT        * ELP007  
00017 *    FILE.  THE JOB STREAM WILL DELETE AND REDEFINE THE         * ELP007  
00018 *    VSAM FILES, AND ELP010 WILL RELOAD THE VSAM FILES          * ELP007  
00019 *    FROM THE WORK FILE, RENUMBERING THE DATA ELEMENTS.         * ELP007  
00020 *                                                               * ELP007  
00021 * 'N', DO NOT EXTRACT: IN THIS MODE, THE PROGRAM WILL           * ELP007  
00022 *    SIMPLY SET THE RETURN CODE TO 12 AND TERMINATE.            * ELP007  
00023 *                                                               * ELP007  
00024 * 'S', SELECTED RECORDS ONLY: IN THIS MODE, THE PROGRAM WILL    * ELP007  
00025 *    READ THE PARAMETER FILE.  EACH PARM RECORD AFTER THE       * ELP007  
00026 *    FIRST ONE SHOULD BE A RECORD LIST PREFIX KEY.  THE         * ELP007  
00027 *    PROGRAM WILL COPY THAT RECORD LIST RECORD AND ALL DATA     * ELP007  
00028 *    ELEMENT AND CODE VALUE RECORDS ASSOCIATED WITH IT, IN      * ELP007  
00029 *    HIERARCHICAL ORDER, TO THE OUTPUT FILE.  THE JOB STREAM    * ELP007  
00030 *    WILL INVOKE ELP010, WHICH WILL DELETE THESE RECORDS        * ELP007  
00031 *    FROM THE VSAM FILES AND RELOAD THEM FROM THE WORK          * ELP007  
00032 *    FILE, WITH THE DATA ELEMENTS RENUMBERED.                   * ELP007  
00033 *                                                               * ELP007  
00034 *    *** MEANING OF RETURN CODES: ***                           * ELP007  
00035 *                                                               * ELP007  
00036 *    0: OUTPUT LOAD FILE SHOULD BE ROUTED DIRECTLY INTO         * ELP007  
00037 *       ELP010 (RECORD MODE).                                   * ELP007  
00038 *                                                               * ELP007  
00039 *    4: IDCAMS SHOULD BE USED TO DELETE AND REDEFINE THE        * ELP007  
00040 *       ELPRL, ELPDE, AND ELPCV FILES.  THEN, ROUTE THE         * ELP007  
00041 *       OUTPUT LOAD FILE INTO ELP010 (FILE MODE).               * ELP007  
00042 *                                                               * ELP007  
00043 *    8: THE OUTPUT LOAD FILE NEEDS TO BE SORTED BECAUSE         * ELP007  
00044 *       THE INPUT PARMS WERE IN THE WRONG ORDER.  THEN,         * ELP007  
00045 *       ROUTE THE OUTPUT LOAD FILE INTO ELP010 (RECORD          * ELP007  
00046 *       MODE).                                                  * ELP007  
00047 *                                                               * ELP007  
00048 *    12: NO RENUMBERING HAS BEEN REQUESTED.  SKIP THE LOAD      * ELP007  
00049 *       STEP AND CONTINUE DIRECTLY TO THE PRINT STEP.           * ELP007  
00050 *                                                               * ELP007  
00051 *    16: DISASTER-LEVEL ERROR: DO NOT USE THE OUTPUT FILE.      * ELP007  
00052 *                                                               * ELP007  
00053 ***************************************************************** ELP007  
00054 *                                                               * ELP007  
00055 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * ELP007  
00056 *    *-*         U P D A T E   H I S T O R Y         *-*        * ELP007  
00057 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * ELP007  
00058 *                                                               * ELP007  
00059 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* ELP007  
00060 *                                                               * ELP007  
00061 *  XXXX      03/05/86  LET  REPLACED ALL THE COBLVSAM CALL WITH * ELP007  
00062 *                           CALLS TO AN IO INTERFACE 'ELBIOPGM' * ELP007  
00063 *                           WHICH WILL DO THOSE CALLS FOR US.   * ELP007  
00064 *  0001      06/18/86  JTC  CHANGED THE INITIAL OF THE LOAD     * ELP007  
00065 *                           RECORDS FOR DATA ELEMENTS AND CODE  * ELP007  
00066 *                           VALUES SO THAT THE DESCRIPTION LINES* ELP007  
00067 *                           WILL CONTAIN SPACES INSTEAD OF      * ELP007  
00068 *                           LOW-VALUES.                         * ELP007  
00069 *  0002      02/17/94  RJL  CORRECTED BRANCH THAT CAUSED EARLY  * ELP007  
00070 *                           TERMINATION OF LOOP THAT WRITES     * ELP007  
00071 *                           CODE VALUE DESCRIPTIONS TO WORK     * ELP007  
00072 *                           FILE.                               * ELP007  
00073 ***************************************************************** ELP007  
00074                                                                   ELP007  
00075  ENVIRONMENT DIVISION.                                            ELP007  
00076  CONFIGURATION SECTION.                                           ELP007  
00077  SOURCE-COMPUTER.  IBM-370.                                       ELP007  
00078  OBJECT-COMPUTER.  IBM-370.                                       ELP007  
00079                                                                   ELP007  
00080  SPECIAL-NAMES.                                                   ELP007  
00081         C01 IS TOP-OF-FORM.                                       ELP007  
00082  INPUT-OUTPUT SECTION.                                            ELP007  
00083  FILE-CONTROL.                                                    ELP007  
00084                                                                   ELP007  
00085      SELECT TEMP-WORK-FILE                                        ELP007  
00086          ASSIGN TO DA-S-TEMPFILE.                                 ELP007  
00087                                                                   ELP007  
00088      SELECT PARM-FILE                                             ELP007  
00089          ASSIGN TO PARM.                                          ELP007  
00090                                                                   ELP007  
00091  DATA DIVISION.                                                   ELP007  
00092  FILE SECTION.                                                    ELP007  
00093                                                                   ELP007  
00094  FD  PARM-FILE                                                    ELP007  
00095      BLOCK CONTAINS  0  RECORDS                                   ELP007  
00096      LABEL RECORDS ARE STANDARD                                   ELP007  
00097      RECORD CONTAINS 80 CHARACTERS                                ELP007  
00098      DATA RECORDS ARE PARM-REC.                                   ELP007  
00099  01  PARM-REC.                                                    ELP007  
00100      05  PARM-1-REQUEST-TYPE    PIC X.                            ELP007  
00101          88  EXTRACT-ALL        VALUE 'A'.                        ELP007  
00102          88  EXTRACT-NOTHING    VALUE 'N'.                        ELP007  
00103          88  EXTRACT-SELECTED   VALUE 'S'.                        ELP007  
00104      05  PARM-PREFIX-KEY        PIC X(8).                         ELP007  
00105      05  FILLER                 PIC X(71).                        ELP007  
00106                                                                   ELP007  
00107 ******************************************************************ELP007  
00108 ** THIS FILE GOES INTO ELP010 FOR RELOADING INTO THE VSAM FILES.**ELP007  
00109 ******************************************************************ELP007  
00110  FD  TEMP-WORK-FILE                                               ELP007  
00111      BLOCK CONTAINS  0  RECORDS                                   ELP007  
00112      LABEL RECORDS ARE STANDARD                                   ELP007  
00113      RECORDING V.                                                 ELP007  
00114  COPY ELPMUC.                                                     ELP007  
00115                                                                   ELP007  
00116  WORKING-STORAGE SECTION.                                         ELP007  
00117  77  FILLER                         PIC X(30)                     ELP007  
00118        VALUE 'ELP007 WORKING STORAGE BEGINS'.                     ELP007  
00119                                                                   ELP007  
00120  01  IO-STATUS.                                                   ELP007  
00121      05  ELPRL-OPEN-INDICATOR       PIC X  VALUE 'N'.             ELP007  
00122          88 ELPRL-OPEN                     VALUE 'Y'.             ELP007  
00123      05  ELPDE-OPEN-INDICATOR       PIC X  VALUE 'N'.             ELP007  
00124          88 ELPDE-OPEN                     VALUE 'Y'.             ELP007  
00125      05  ELPCV-OPEN-INDICATOR       PIC X  VALUE 'N'.             ELP007  
00126          88 ELPCV-OPEN                     VALUE 'Y'.             ELP007  
00127      05  PARM-EOF-INDICATOR         PIC X  VALUE 'N'.             ELP007  
00128          88 PARM-EOF                       VALUE 'Y'.             ELP007  
00129      05  EXTRACT-TYPE-INDICATOR     PIC X  VALUE SPACES.          ELP007  
00130          88 SELECT-BY-RECORD               VALUE 'S'.             ELP007  
00131          88 ALL-RECORDS                    VALUE 'A'.             ELP007  
00132      05  PARM-ORDER-INDICATOR       PIC X  VALUE 'Y'.             ELP007  
00133          88 PARMS-OUT-OF-ORDER             VALUE 'N'.             ELP007  
00134          88 PARMS-IN-ORDER                 VALUE 'Y'.             ELP007  
00135                                                                   ELP007  
00136  01  MISC-WORK.                                                   ELP007  
00137      05  DESC-SUB                   PIC S9(8) COMP VALUE +0.      ELP007  
00138      05  TEMP-HDR-COUNT             PIC 9(8)  VALUE ZEROES.       ELP007  
00139      05  TEMP-RL-COUNT              PIC 9(8)  VALUE ZEROES.       ELP007  
00140      05  TEMP-DE-COUNT              PIC 9(8)  VALUE ZEROES.       ELP007  
00141      05  TEMP-CV-COUNT              PIC 9(8)  VALUE ZEROES.       ELP007  
00142      05  LAST-PARM-PREFIX           PIC X(8)  VALUE LOW-VALUES.   ELP007  
00143      05  SWITCH-ON-STATUS           PIC X     VALUE 'Y'.          ELP007  
00144      05  SWITCH-OFF-STATUS          PIC X     VALUE 'N'.          ELP007  
00145      05  HOLD-RL-FEEDBACK           PIC S9(04) COMP VALUE +0.     ELP007  
00146      05  HOLD-DE-FEEDBACK           PIC S9(04) COMP VALUE +0.     ELP007  
00147      05  HOLD-RL-REQUEST            PIC  X(01)      VALUE SPACE.  ELP007  
00148      05  HOLD-DE-REQUEST            PIC  X(01)      VALUE SPACE.  ELP007  
00149      05  WS-BLANK-LINE              PIC  X(79)      VALUE SPACE.  ELP007  
00150                                                                   ELP007  
00151 ******************************************************************ELP007  
00152 **       PARAMETERS FOR THE 'ELBIOPGM' IO INTERFACE             **ELP007  
00153 ******************************************************************ELP007  
00154  01  IO-INFO.                                                     ELP007  
00155      COPY ELBHIOPM.                                               ELP007  
00156 /                                                                 ELP007  
00157 ******************************************************************ELP007  
00158 **       HOLD AREA FOR THE VSAM RECORDS                         **ELP007  
00159 ******************************************************************ELP007  
00160  01  HOLD-RL-RECORD.                                              ELP007  
00161      COPY ELPRLC.                                                 ELP007  
00162      SKIP3                                                        ELP007  
00163  01  HOLD-DE-RECORD.                                              ELP007  
00164      COPY ELPDEC.                                                 ELP007  
00165      SKIP3                                                        ELP007  
00166  01  HOLD-CV-RECORD.                                              ELP007  
00167      COPY ELPCVC.                                                 ELP007  
00168 /                                                                 ELP007  
00169  01  MISSING-PARMS.                                               ELP007  
00170      05  FILLER             PIC X(35) VALUE                       ELP007  
00171      'NO PARAMETER FILE RECORDS          '.                       ELP007  
00172  01  PARMS-OUT-OF-ORDER-MSG.                                      ELP007  
00173      05  FILLER             PIC X(35) VALUE                       ELP007  
00174      'PARAMETER FILE RECORD OUT OF ORDER '.                       ELP007  
00175  01  MISSING-SELECT.                                              ELP007  
00176      05  FILLER             PIC X(35) VALUE                       ELP007  
00177      'NO SELECT RECORDS ON PARAMETER FILE'.                       ELP007  
00178  01  BAD-OPTION.                                                  ELP007  
00179      05  FILLER             PIC X(35) VALUE                       ELP007  
00180      'EXTRACT OPTION MUST BE A OR S      '.                       ELP007  
00181  01  BAD-PREFIX.                                                  ELP007  
00182      05  FILLER             PIC X(35) VALUE                       ELP007  
00183      'RECORD LIST FILE KEY MISSING       '.                       ELP007  
00184                                                                   ELP007  
00185  01  PARM-ERROR-MSG.                                              ELP007  
00186      05  FILLER             PIC X(8)  VALUE 'ELP007  '.           ELP007  
00187      05  FILLER             PIC X(15) VALUE ' INVALID PARM: '.    ELP007  
00188      05  MSG-PARM-ERROR     PIC X(35) VALUE SPACES.               ELP007  
00189      05  FILLER             PIC X(7)  VALUE ' INPUT='.            ELP007  
00190      05  MSG-PARM-VALUE     PIC X(15) VALUE SPACES.               ELP007  
00191                                                                   ELP007  
00192  01  PARM-ABEND-MSG.                                              ELP007  
00193      05  FILLER             PIC X(8)  VALUE 'ELP007  '.           ELP007  
00194      05  FILLER             PIC X(15) VALUE ' ABNORMAL EOJ: '.    ELP007  
00195      05  MSG-PARM-ABEND     PIC X(35) VALUE SPACES.               ELP007  
00196                                                                   ELP007  
00197  01  VSAM-ERR-MSG.                                                ELP007  
00198      05  FILLER                     PIC X(8)  VALUE 'ELP007  '.   ELP007  
00199      05  FILLER                     PIC X     VALUE SPACES.       ELP007  
00200      05  MSG-FILE-NAME              PIC X(08) VALUE SPACES.       ELP007  
00201      05  FILLER                     PIC X(6)  VALUE ' VSAM '.     ELP007  
00202      05  MSG-ACTION                 PIC X(8)  VALUE SPACES.       ELP007  
00203      05  FILLER                     PIC X(10) VALUE ' FEEDBACK='. ELP007  
00204      05  MSG-FEEDBACK               PIC 9(4)  VALUE ZEROES.       ELP007  
00205      05  FILLER                     PIC X(10) VALUE ' REQ-TYPE='. ELP007  
00206      05  MSG-REQ-TYPE               PIC X     VALUE SPACES.       ELP007  
00207      05  FILLER                     PIC X     VALUE SPACES.       ELP007  
00208      05  MSG-COMMENT                PIC X(12) VALUE SPACES.       ELP007  
00209                                                                   ELP007  
00210  01  VSAM-KEY-MSG.                                                ELP007  
00211      05  FILLER                     PIC X(15) VALUE               ELP007  
00212          'TRANSLATED KEY='.                                       ELP007  
00213      05  MSG-KEY-PREFIX             PIC X(8)  VALUE SPACES.       ELP007  
00214      05  MSG-KEY-ELEMENT            PIC 999.99                    ELP007  
00215                                               VALUE ZEROES        ELP007  
00216                                               BLANK WHEN ZEROES.  ELP007  
00217      05  MSG-KEY-CODE-VALUE         PIC X(10) VALUE SPACES.       ELP007  
00218      05  MSG-KEY-SEQUENCE           PIC 99    VALUE ZEROES        ELP007  
00219                                               BLANK WHEN ZEROES.  ELP007  
00220      05  MSG-LENGTH                 PIC X(9)  VALUE '  LENGTH='.  ELP007  
00221      05  MSG-RDW-LENGTH             PIC ZZZ9  VALUE ZEROES.       ELP007  
00222                                                                   ELP007  
00223  01  ABEND-CODE                     PIC 9(04) COMP VALUE ZEROS.   ELP007  
00224 /                                                                 ELP007  
00225  PROCEDURE DIVISION.                                              ELP007  
00226                                                                   ELP007  
00227  0000-MAINLINE.                                                   ELP007  
00228                                                                   ELP007  
00229      PERFORM 0500-OPEN-FILES                                      ELP007  
00230         THRU 0500-EXIT.                                           ELP007  
00231                                                                   ELP007  
00232      PERFORM 8100-READ-PARM THRU 8100-EXIT.                       ELP007  
00233                                                                   ELP007  
00234      IF PARM-EOF                                                  ELP007  
00235          PERFORM 8010-CLOSE-FILES THRU 8010-EXIT                  ELP007  
00236          MOVE 12 TO RETURN-CODE                                   ELP007  
00237          GO TO 0000-EXIT.                                         ELP007  
00238                                                                   ELP007  
00239      IF EXTRACT-NOTHING                                           ELP007  
00240          PERFORM 8010-CLOSE-FILES THRU 8010-EXIT                  ELP007  
00241          MOVE 12 TO RETURN-CODE                                   ELP007  
00242          GO TO 0000-EXIT.                                         ELP007  
00243                                                                   ELP007  
00244      IF EXTRACT-ALL OR EXTRACT-SELECTED                           ELP007  
00245          MOVE PARM-1-REQUEST-TYPE TO EXTRACT-TYPE-INDICATOR       ELP007  
00246      ELSE                                                         ELP007  
00247          MOVE BAD-OPTION TO MSG-PARM-ABEND                        ELP007  
00248          DISPLAY PARM-ABEND-MSG                                   ELP007  
00249          MOVE 16  TO  RETURN-CODE,                                ELP007  
00250                       ABEND-CODE                                  ELP007  
00251          GO TO 9999-ABEND-RTN.                                    ELP007  
00252                                                                   ELP007  
00253 ******************************************************************ELP007  
00254 **  IF EXTRACT OPTION IS FOR SELECTED ITEMS, THEN THE PARM      **ELP007  
00255 **  INPUT FILE SHOULD CONTAIN AT LEAST ONE SELECTION RECORD.    **ELP007  
00256 **  WITH THIS EXTRACT OPTION, PROGRAM LOGIC IS DRIVEN BY THE    **ELP007  
00257 **  PARM FILE.  EACH PARM RECORD POINTS AT A RECORD LIST RECORD.**ELP007  
00258 ******************************************************************ELP007  
00259      IF SELECT-BY-RECORD                                          ELP007  
00260          MOVE LOW-VALUES TO HEADER-RECORD                         ELP007  
00261          MOVE 'R' TO HEADER-TYPE-RUN                              ELP007  
00262          PERFORM 8110-WRITE-TEMP-HDR THRU 8110-EXIT               ELP007  
00263          PERFORM 8100-READ-PARM THRU 8100-EXIT                    ELP007  
00264          IF PARM-EOF                                              ELP007  
00265              MOVE MISSING-SELECT TO MSG-PARM-ABEND                ELP007  
00266              DISPLAY PARM-ABEND-MSG                               ELP007  
00267              MOVE 16  TO  RETURN-CODE,                            ELP007  
00268                           ABEND-CODE                              ELP007  
00269              GO TO 9999-ABEND-RTN                                 ELP007  
00270          ELSE                                                     ELP007  
00271              MOVE PARM-PREFIX-KEY TO LAST-PARM-PREFIX             ELP007  
00272              PERFORM 3000-EXTRACT-BY-PARM THRU 3000-EXIT          ELP007  
00273                  UNTIL PARM-EOF                                   ELP007  
00274      ELSE                                                         ELP007  
00275          MOVE LOW-VALUES TO HEADER-RECORD                         ELP007  
00276          MOVE 'F' TO HEADER-TYPE-RUN                              ELP007  
00277          PERFORM 8110-WRITE-TEMP-HDR THRU 8110-EXIT               ELP007  
00278 ******************************************************************ELP007  
00279 **  FOR THE OTHER EXTRACT OPTION, PROGRAM LOGIC IS DRIVEN BY    **ELP007  
00280 **  SEQUENTIAL ACCESS TO THE ELPRL FILE.                        **ELP007  
00281 ******************************************************************ELP007  
00282          PERFORM 1000-SEQ-EXTRACT-ELPRL-SET THRU 1000-EXIT        ELP007  
00283              UNTIL HOLD-RL-REQUEST   =  '2'                       ELP007  
00284                           OR                                      ELP007  
00285                    HOLD-RL-FEEDBACK  =  16.                       ELP007  
00286                                                                   ELP007  
00287      PERFORM 8010-CLOSE-FILES THRU 8010-EXIT.                     ELP007  
00288                                                                   ELP007  
00289 ******************************************************************ELP007  
00290 ** RETURN CODE 4 IS NOT AN ERROR.  IT JUST TELLS US THAT WE     **ELP007  
00291 ** WANT TO RUN A SPECIAL DELETE/DEFINE STEP FOR THE THREE       **ELP007  
00292 ** VSAM FILES BEFORE RUNNING ELP010.                            **ELP007  
00293 ******************************************************************ELP007  
00294 ** RETURN CODE 12 MEANS THAT THE SELECTION PARMS WERE OUT OF    **ELP007  
00295 ** ORDER.  THIS MEANS WE HAVE TO SORT THE OUTPUT LOAD FILE      **ELP007  
00296 ** BEFORE RUNNING ELP010, BUT WE DON'T HAVE TO DELETE/DEFINE.   **ELP007  
00297 ******************************************************************ELP007  
00298                                                                   ELP007  
00299      IF ALL-RECORDS                                               ELP007  
00300          MOVE 4 TO RETURN-CODE                                    ELP007  
00301      ELSE                                                         ELP007  
00302          IF PARMS-OUT-OF-ORDER                                    ELP007  
00303              DISPLAY                                              ELP007  
00304              '****WARNING: OUTPUT FILE MUST BE SORTED****'        ELP007  
00305              MOVE 8 TO RETURN-CODE                                ELP007  
00306          ELSE                                                     ELP007  
00307              IF SELECT-BY-RECORD                                  ELP007  
00308              THEN                                                 ELP007  
00309                  MOVE 8       TO RETURN-CODE                      ELP007  
00310              ELSE                                                 ELP007  
00311                  MOVE 0 TO RETURN-CODE.                           ELP007  
00312                                                                   ELP007  
00313  0000-EXIT.                                                       ELP007  
00314      GOBACK.                                                      ELP007  
00315 /                                                                 ELP007  
00316  0500-OPEN-FILES.                                                 ELP007  
00317                                                                   ELP007  
00318 ******************************************************************ELP007  
00319 **    ONLY ONE OPEN IS ISSUED FOR THE VSAM FILES (RL, DE, CV).  **ELP007  
00320 ** THE IO INTERFACE WILL OPEN ALL THE VSAM FILES NEEDED.        **ELP007  
00321 ******************************************************************ELP007  
00322                                                                   ELP007  
00323      OPEN INPUT PARM-FILE                                         ELP007  
00324           OUTPUT TEMP-WORK-FILE.                                  ELP007  
00325                                                                   ELP007  
00326      MOVE 'RL'  TO  ELBHIO-FILE-ID.                               ELP007  
00327      MOVE 'O'   TO  ELBHIO-REQUEST-TYPE.                          ELP007  
00328                                                                   ELP007  
00329      CALL 'ELBIOPGM'  USING IO-INFO.                              ELP007  
00330                                                                   ELP007  
00331      IF ELBHIO-GOOD-RETURN                                        ELP007  
00332          NEXT SENTENCE                                            ELP007  
00333      ELSE                                                         ELP007  
00334          MOVE ELBHIO-FEEDBACK  TO  ABEND-CODE                     ELP007  
00335          GO TO 9999-ABEND-RTN.                                    ELP007  
00336                                                                   ELP007  
00337  0500-EXIT.  EXIT.                                                ELP007  
00338 ******************************************************************ELP007  
00339 ** PROCESSING STEPS:                                            **ELP007  
00340 ** SEQUENTIAL READ OF ELPRL (RECORD LIST FILE)                  **ELP007  
00341 ** CHECK FOR EOF                                                **ELP007  
00342 ** GENERATE AN EXTRACT RECORD FOR THE RECORD LIST RECORD AND    **ELP007  
00343 ** FOR ALL DATA ELEMENTS AND CODE VALUES ASSOCIATED WITH THE    **ELP007  
00344 ** ELPRL RECORD                                                 **ELP007  
00345 ******************************************************************ELP007  
00346                                                                   ELP007  
00347  1000-SEQ-EXTRACT-ELPRL-SET.                                      ELP007  
00348                                                                   ELP007  
00349      PERFORM 8030-READ-NEXT-ELPRL THRU 8030-EXIT.                 ELP007  
00350                                                                   ELP007  
00351      IF HOLD-RL-REQUEST   =  '2'                                  ELP007  
00352                OR                                                 ELP007  
00353         HOLD-RL-FEEDBACK  =  16                                   ELP007  
00354          GO TO 1000-EXIT.                                         ELP007  
00355                                                                   ELP007  
00356      IF RL-RECORD-PREFIX = LOW-VALUES  OR  SPACES                 ELP007  
00357          DISPLAY ' RL REC PREFIX IS LOW VALUES OR SPACES '        ELP007  
00358          GO TO 1000-EXIT.                                         ELP007  
00359                                                                   ELP007  
00360      IF RL-DELETE                                                 ELP007  
00361          DISPLAY ' RL REC IS FOR DELETE '                         ELP007  
00362          GO TO 1000-EXIT.                                         ELP007  
00363      PERFORM 1500-GENERATE-EXT-FOR-ELPRL THRU 1500-EXIT.          ELP007  
00364                                                                   ELP007  
00365  1000-EXIT.  EXIT.                                                ELP007  
00366                                                                   ELP007  
00367 ******************************************************************ELP007  
00368 ** GENERATE AN EXTRACT FOR THE RECORD LIST RECORD.              **ELP007  
00369 ** THEN, SET UP TO READ ALL OF THE ASSOCIATED DATA ELEMENT      **ELP007  
00370 ** RECORDS.                                                     **ELP007  
00371 ******************************************************************ELP007  
00372                                                                   ELP007  
00373  1500-GENERATE-EXT-FOR-ELPRL.                                     ELP007  
00374                                                                   ELP007  
00375      MOVE LOW-VALUES TO RECORD-LIST-LOAD.                         ELP007  
00376      MOVE 'A' TO RECORD-TYPE-RL.                                  ELP007  
00377 *    THE ELEMENT NAME AND CODE VALUE FIELDS IN THE RECORD LIST    ELP007  
00378 *    RECORD SORT KEY SHOULD BE LOW-VALUES.                        ELP007  
00379      MOVE RL-RECORD-PREFIX TO PREFIX-RL.                          ELP007  
00380      MOVE RL-RECORD-NAME TO RECORD-NAME-RL.                       ELP007  
00381      MOVE RL-FILE TO RECORD-FILE-TYPE-RL.                         ELP007  
00382      MOVE SPACES TO DELETE-RECORD-FLAG-RL.                        ELP007  
00383      IF ALL-RECORDS                                               ELP007  
00384          MOVE SPACES TO AUTO-REPRINT-FLAG-RL                      ELP007  
00385      ELSE                                                         ELP007  
00386          MOVE 'Y' TO AUTO-REPRINT-FLAG-RL.                        ELP007  
00387      MOVE ZEROES TO CODE-DESC-SEQ-RL.                             ELP007  
00388      PERFORM 8120-WRITE-TEMP-RL THRU 8120-EXIT.                   ELP007  
00389                                                                   ELP007  
00390      MOVE RL-RECORD-PREFIX  TO  ELBHIO-DE-RECORD-PREFIX,          ELP007  
00391                                 DE-RECORD-PREFIX.                 ELP007  
00392      PERFORM 8060-POINT-ELPDE THRU 8060-EXIT.                     ELP007  
00393                                                                   ELP007  
00394      IF ELBHIO-GOOD-RETURN                                        ELP007  
00395          NEXT SENTENCE                                            ELP007  
00396      ELSE                                                         ELP007  
00397          GO TO 1500-EXIT.                                         ELP007  
00398                                                                   ELP007  
00399      MOVE 'G'  TO  ELBHIO-REQUEST-TYPE,                           ELP007  
00400                    HOLD-DE-REQUEST.                               ELP007  
00401      PERFORM 1800-GENERATE-EXT-FOR-ELPDE THRU 1800-EXIT           ELP007  
00402          UNTIL HOLD-DE-REQUEST  NOT  =  'G'                       ELP007  
00403                       OR                                          ELP007  
00404                DE-RECORD-PREFIX  NOT  =  RL-RECORD-PREFIX.        ELP007  
00405                                                                   ELP007  
00406  1500-EXIT.                                                       ELP007  
00407      EXIT.                                                        ELP007  
00408                                                                   ELP007  
00409  1800-GENERATE-EXT-FOR-ELPDE.                                     ELP007  
00410                                                                   ELP007  
00411      PERFORM 8050-READ-NEXT-ELPDE THRU 8050-EXIT.                 ELP007  
00412                                                                   ELP007  
00413      IF ELBHIO-REQUEST-TYPE NOT = 'G'                             ELP007  
00414                      OR                                           ELP007  
00415         DE-RECORD-PREFIX  NOT  =  RL-RECORD-PREFIX                ELP007  
00416          GO TO 1800-EXIT.                                         ELP007  
00417                                                                   ELP007  
00418      IF DE-RECORD-PREFIX = LOW-VALUES  OR  SPACES                 ELP007  
00419          GO TO 1800-EXIT.                                         ELP007  
00420                                                                   ELP007  
00421      IF DE-DELETE                                                 ELP007  
00422          GO TO 1800-EXIT.                                         ELP007  
00423                                                                   ELP007  
00424      MOVE DE-NBR-DESC-LINES TO NBR-DESC-LINES-DE.                 ELP007  
00425      MOVE LOW-VALUES TO DATA-ELEMENT-LOAD.                        ELP007  
00426      PERFORM 1850-SPACE-DE-DESC THRU 1850-EXIT                    ELP007  
00427          VARYING DESC-SUB FROM 1 BY 1                             ELP007  
00428          UNTIL DESC-SUB IS GREATER THAN                           ELP007  
00429          DE-NBR-DESC-LINES.                                       ELP007  
00430      MOVE 'B' TO RECORD-TYPE-DE.                                  ELP007  
00431      MOVE DE-RECORD-PREFIX TO PREFIX-DE.                          ELP007  
00432 *    WE DON'T COPY THE ELEMENT# BECAUSE ELP010 ASSIGNS NEW ONES.  ELP007  
00433 *    WE DON'T COPY DE-RECORD-PREFIX-N BECAUSE IT IS THE SAME AS   ELP007  
00434 *    DE-RECORD-PREFIX.                                            ELP007  
00435 *    THE CODE VALUE FIELD WILL HAVE LOW-VALUES IN IT AT THIS LEVELELP007  
00436      MOVE DE-ELEMENT-NAME TO NAME-DE.                             ELP007  
00437      MOVE DE-ELEMENT-FORMAT TO ELEMENT-FORMAT-DE.                 ELP007  
00438      MOVE DE-ELEMENT-LENGTH TO ELEMENT-LENGTH-DE.                 ELP007  
00439      MOVE DE-FORMAT-COMMENT TO FORMAT-COMMENT-DE.                 ELP007  
00440      MOVE SPACES TO DELETE-ELEMENT-FLAG-DE.                       ELP007  
00441      MOVE SPACES TO AUTO-REPRINT-FLAG-DE.                         ELP007  
00442      MOVE DE-RECORD-POS TO RECORD-POS-DE.                         ELP007  
00443      MOVE DE-STORED-LENGTH TO STORED-LENGTH-DE.                   ELP007  
00444      MOVE DE-STORED-DECIMALS TO STORED-DECIMALS-DE.               ELP007  
00445      MOVE DE-STORED-FORMAT TO STORED-FORMAT-DE.                   ELP007  
00446      MOVE DE-CODES-FLAG TO CODES-FLAG-DE.                         ELP007  
00447      MOVE DE-COBOL-NAME TO COBOL-NAME-DE.                         ELP007  
00448      MOVE DE-BAL-NAME TO BAL-NAME-DE.                             ELP007  
00449      MOVE DE-NBR-DESC-LINES TO NBR-DESC-LINES-DE.                 ELP007  
00450      MOVE ZEROES TO CODE-DESC-SEQ-DE.                             ELP007  
00451      PERFORM 1900-MOVE-DE-DESC THRU 1900-EXIT                     ELP007  
00452          VARYING DESC-SUB FROM 1 BY 1                             ELP007  
00453          UNTIL DESC-SUB IS GREATER THAN                           ELP007  
00454          DE-NBR-DESC-LINES.                                       ELP007  
00455                                                                   ELP007  
00456      PERFORM 8130-WRITE-TEMP-DE THRU 8130-EXIT.                   ELP007  
00457                                                                   ELP007  
00458      MOVE DE-RECORD-PREFIX  TO  ELBHIO-CV-RECORD-PREFIX.          ELP007  
00459      MOVE DE-ELEMENT-NBR    TO  ELBHIO-CV-ELEMENT-NBR.            ELP007  
00460                                                                   ELP007  
00461      PERFORM 8090-POINT-ELPCV THRU 8090-EXIT.                     ELP007  
00462                                                                   ELP007  
00463      IF ELBHIO-REQUEST-TYPE NOT = 'P'                             ELP007  
00464          GO TO 1800-EXIT.                                         ELP007  
00465                                                                   ELP007  
00466      PERFORM 8080-READ-NEXT-ELPCV THRU 8080-EXIT.                 ELP007  
00467                                                                   ELP007  
00468      IF ELBHIO-REQUEST-TYPE  NOT  =  'G'                          ELP007  
00469                           OR                                      ELP007  
00470         CV-RECORD-PREFIX     NOT  =  DE-RECORD-PREFIX             ELP007  
00471                           OR                                      ELP007  
00472         CV-ELEMENT-NBR       NOT  =  DE-ELEMENT-NBR               ELP007  
00473          GO TO 1800-EXIT.                                         ELP007  
00474                                                                   ELP007  
00475      IF CV-RECORD-PREFIX = LOW-VALUES  OR  SPACES                 ELP007  
00476          GO TO 1800-EXIT.                                         ELP007  
00477                                                                   ELP007  
00478      PERFORM 2000-GENERATE-EXT-FOR-ELPCV THRU 2000-EXIT           ELP007  
00479          UNTIL ELBHIO-REQUEST-TYPE NOT = 'G'                      ELP007  
00480                         OR                                        ELP007  
00481                ELBHIO-CV-RECORD-PREFIX  NOT  =  DE-RECORD-PREFIX  ELP007  
00482                         OR                                        ELP007  
00483                ELBHIO-CV-ELEMENT-NBR    NOT  =  DE-ELEMENT-NBR.   ELP007  
00484                                                                   ELP007  
00485  1800-EXIT.                                                       ELP007  
00486      EXIT.                                                        ELP007  
00487                                                                   ELP007  
00488  1850-SPACE-DE-DESC.                                              ELP007  
00489                                                                   ELP007  
00490      MOVE WS-BLANK-LINE TO DESC-LINE-DE (DESC-SUB).               ELP007  
00491                                                                   ELP007  
00492  1850-EXIT.                                                       ELP007  
00493      EXIT.                                                        ELP007  
00494                                                                   ELP007  
00495  1900-MOVE-DE-DESC.                                               ELP007  
00496                                                                   ELP007  
00497      MOVE DE-DESC-LINE (DESC-SUB) TO DESC-LINE-DE (DESC-SUB).     ELP007  
00498      IF DESC-SUB = DE-NBR-DESC-LINES                              ELP007  
00499          INSPECT DESC-LINE-DE (DESC-SUB) REPLACING ALL LOW-VALUES ELP007  
00500             BY SPACE.                                             ELP007  
00501                                                                   ELP007  
00502  1900-EXIT.                                                       ELP007  
00503      EXIT.                                                        ELP007  
00504                                                                   ELP007  
00505 ******************************************************************ELP007  
00506 ** NOTICE THAT THERE CAN BE MORE THAN ONE CODE VALUE RECORD     **ELP007  
00507 ** PER CODE VALUE.  THIS IS THE CASE IF THE DESCRIPTION         **ELP007  
00508 ** TAKES UP MORE THAN 12 LINES.  THERE IS A RECORD SEQUENCE NO  **ELP007  
00509 ** IN THE CODE VALUE VSAM KEY.  THIS IS NO PROBLEM FOR US; WE   **ELP007  
00510 ** JUST DO A SEQUENTIAL BROWSE AND COPY ALL OF THE CODE VALUE   **ELP007  
00511 ** RECORDS THAT HAVE TO DO WITH THE CURRENT DATA ELEMENT.       **ELP007  
00512 ******************************************************************ELP007  
00513                                                                   ELP007  
00514  2000-GENERATE-EXT-FOR-ELPCV.                                     ELP007  
00515                                                                   ELP007  
00516      IF CV-RECORD-PREFIX = LOW-VALUES  OR  SPACES                 ELP007  
00517          PERFORM 8080-READ-NEXT-ELPCV THRU 8080-EXIT              ELP007  
00518          GO TO 2000-EXIT.                                         ELP007  
00519                                                                   ELP007  
00520      IF CV-DELETE                                                 ELP007  
00521          PERFORM 8080-READ-NEXT-ELPCV THRU 8080-EXIT              ELP007  
00522          GO TO 2000-EXIT.                                         ELP007  
00523                                                                   ELP007  
00524      MOVE CV-NBR-VALUE-DESC-LINES TO NBR-VALUE-DESC-LINES-CV.     ELP007  
00525      MOVE LOW-VALUES TO CODE-VALUE-LOAD.                          ELP007  
00526      PERFORM 2050-SPACE-CV-DESC THRU 2050-EXIT                    ELP007  
00527          VARYING DESC-SUB FROM 1 BY 1                             ELP007  
00528          UNTIL DESC-SUB IS GREATER THAN                           ELP007  
00529          CV-NBR-VALUE-DESC-LINES.                                 ELP007  
00530      MOVE 'C' TO RECORD-TYPE-CV.                                  ELP007  
00531      MOVE CV-RECORD-PREFIX TO PREFIX-CV.                          ELP007  
00532 *    FOR SORTING PURPOSES, THE ELEMENT NAME IN THE CODE VALUE     ELP007  
00533 *    LOAD RECORD MATCHES THE ELEMENT NAME IN THE DATA ELEMENT     ELP007  
00534 *    LOAD RECORD ASSOCIATED WITH IT.                              ELP007  
00535      MOVE DE-ELEMENT-NAME TO NAME-CV.                             ELP007  
00536      MOVE CV-CODE-VALUE TO CODE-VALUE-CV.                         ELP007  
00537      MOVE CV-CODE-DESC-SEQ TO CODE-DESC-SEQ-CV.                   ELP007  
00538      MOVE CV-CODE-NAME TO CODE-NAME-CV.                           ELP007  
00539      MOVE SPACES TO DELETE-CODE-FLAG-CV.                          ELP007  
00540                                                                   ELP007  
00541      MOVE CV-NBR-VALUE-DESC-LINES TO NBR-VALUE-DESC-LINES-CV.     ELP007  
00542      PERFORM 2100-MOVE-CV-DESC THRU 2100-EXIT                     ELP007  
00543          VARYING DESC-SUB FROM 1 BY 1                             ELP007  
00544          UNTIL DESC-SUB IS GREATER THAN                           ELP007  
00545          CV-NBR-VALUE-DESC-LINES.                                 ELP007  
00546                                                                   ELP007  
00547      PERFORM 8140-WRITE-TEMP-CV THRU 8140-EXIT.                   ELP007  
00548                                                                   ELP007  
00549      PERFORM 8080-READ-NEXT-ELPCV THRU 8080-EXIT.                 ELP007  
00550                                                                   ELP007  
00551  2000-EXIT.                                                       ELP007  
00552      EXIT.                                                        ELP007  
00553                                                                   ELP007  
00554  2050-SPACE-CV-DESC.                                              ELP007  
00555                                                                   ELP007  
00556      MOVE WS-BLANK-LINE                                           ELP007  
00557          TO VALUE-DESC-LINE-CV (DESC-SUB).                        ELP007  
00558                                                                   ELP007  
00559  2050-EXIT.                                                       ELP007  
00560      EXIT.                                                        ELP007  
00561                                                                   ELP007  
00562                                                                   ELP007  
00563  2100-MOVE-CV-DESC.                                               ELP007  
00564                                                                   ELP007  
00565      MOVE CV-VALUE-DESC-LINE (DESC-SUB)                           ELP007  
00566          TO VALUE-DESC-LINE-CV (DESC-SUB).                        ELP007  
00567      IF DESC-SUB = CV-NBR-VALUE-DESC-LINES                        ELP007  
00568          INSPECT VALUE-DESC-LINE-CV (DESC-SUB) REPLACING ALL      ELP007  
00569             LOW-VALUES BY SPACE.                                  ELP007  
00570                                                                   ELP007  
00571  2100-EXIT.                                                       ELP007  
00572      EXIT.                                                        ELP007  
00573                                                                   ELP007  
00574 ******************************************************************ELP007  
00575 ** PROCESSING STEPS:                                            **ELP007  
00576 ** A. EDIT PARMS - WE ALLOW ONE TYPE OF SELECTION PARM,         **ELP007  
00577 **    WHICH MUST CONTAIN A PREFIX FIELD.                        **ELP007  
00578 ** B. READ THE VSAM FILES - WE USE THE PARM INPUT TO BUILD      **ELP007  
00579 **    THE KEY AND READ THE ELPRL (RECORD LIST) FILE.  IF NOT    **ELP007  
00580 **    FOUND, THE READ ROUTINE WILL PUT OUT AN ERROR MESSAGE.    **ELP007  
00581 ** C. EXTRACT THE RESULTS, USING THE APPROPRIATE ROUTINE.       **ELP007  
00582 ** D. OBTAIN THE NEXT PARM RECORD.                              **ELP007  
00583 ******************************************************************ELP007  
00584                                                                   ELP007  
00585  3000-EXTRACT-BY-PARM.                                            ELP007  
00586                                                                   ELP007  
00587      IF NOT EXTRACT-SELECTED                                      ELP007  
00588          MOVE BAD-OPTION TO MSG-PARM-ERROR                        ELP007  
00589          PERFORM 9300-PARM-ERROR-MSG THRU 9300-EXIT               ELP007  
00590          PERFORM 3100-GET-NEXT-SELECT-PARM THRU 3100-EXIT         ELP007  
00591          GO TO 3000-EXIT.                                         ELP007  
00592                                                                   ELP007  
00593      IF PARM-PREFIX-KEY = SPACES OR LOW-VALUES OR ZEROES          ELP007  
00594          MOVE BAD-PREFIX TO MSG-PARM-ERROR                        ELP007  
00595          PERFORM 9300-PARM-ERROR-MSG THRU 9300-EXIT               ELP007  
00596          PERFORM 3100-GET-NEXT-SELECT-PARM THRU 3100-EXIT         ELP007  
00597          GO TO 3000-EXIT.                                         ELP007  
00598                                                                   ELP007  
00599      MOVE PARM-PREFIX-KEY TO ELBHIO-ELPRL-KEY.                    ELP007  
00600      PERFORM 8020-READ-ELPRL THRU 8020-EXIT.                      ELP007  
00601      IF ELBHIO-REQUEST-TYPE  =  'R'                               ELP007  
00602          PERFORM 1500-GENERATE-EXT-FOR-ELPRL                      ELP007  
00603              THRU 1500-EXIT.                                      ELP007  
00604                                                                   ELP007  
00605      PERFORM 3100-GET-NEXT-SELECT-PARM THRU 3100-EXIT.            ELP007  
00606                                                                   ELP007  
00607  3000-EXIT.                                                       ELP007  
00608      EXIT.                                                        ELP007  
00609                                                                   ELP007  
00610  3100-GET-NEXT-SELECT-PARM.                                       ELP007  
00611                                                                   ELP007  
00612      PERFORM 8100-READ-PARM THRU 8100-EXIT.                       ELP007  
00613      IF PARM-EOF                                                  ELP007  
00614          GO TO 3100-EXIT.                                         ELP007  
00615                                                                   ELP007  
00616      IF PARM-PREFIX-KEY NOT GREATER THAN LAST-PARM-PREFIX         ELP007  
00617          MOVE PARMS-OUT-OF-ORDER-MSG TO MSG-PARM-ERROR            ELP007  
00618          PERFORM 9300-PARM-ERROR-MSG THRU 9300-EXIT               ELP007  
00619          MOVE SWITCH-OFF-STATUS TO PARM-ORDER-INDICATOR.          ELP007  
00620                                                                   ELP007  
00621      MOVE PARM-PREFIX-KEY TO LAST-PARM-PREFIX.                    ELP007  
00622                                                                   ELP007  
00623  3100-EXIT.                                                       ELP007  
00624      EXIT.                                                        ELP007  
00625                                                                   ELP007  
00626 ******************************************************************ELP007  
00627 ** THIS ROUTINE IS USED BOTH FOR NORMAL AND FOR ABNORMAL EOJ.   **ELP007  
00628 ** IN EITHER CASE, WE TRY TO CLOSE ALL OPENED FILES.            **ELP007  
00629 ******************************************************************ELP007  
00630  8010-CLOSE-FILES.                                                ELP007  
00631                                                                   ELP007  
00632      CLOSE PARM-FILE TEMP-WORK-FILE.                              ELP007  
00633      MOVE 'CLOSE' TO  MSG-ACTION.                                 ELP007  
00634      MOVE 'RL'    TO  ELBHIO-FILE-ID.                             ELP007  
00635      MOVE 'C'     TO  ELBHIO-REQUEST-TYPE.                        ELP007  
00636                                                                   ELP007  
00637      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP007  
00638                                                                   ELP007  
00639      IF  ELBHIO-GOOD-RETURN                                       ELP007  
00640          NEXT SENTENCE                                            ELP007  
00641      ELSE                                                         ELP007  
00642          MOVE ELBHIO-FEEDBACK  TO  ABEND-CODE                     ELP007  
00643          GO TO 9999-ABEND-RTN.                                    ELP007  
00644                                                                   ELP007  
00645      DISPLAY 'TEMP HDR COUNT=' TEMP-HDR-COUNT                     ELP007  
00646              '    TEMP RL COUNT=' TEMP-RL-COUNT.                  ELP007  
00647      DISPLAY 'TEMP DE COUNT=' TEMP-DE-COUNT                       ELP007  
00648              '     TEMP CV COUNT=' TEMP-CV-COUNT.                 ELP007  
00649                                                                   ELP007  
00650  8010-EXIT.                                                       ELP007  
00651      EXIT.                                                        ELP007  
00652                                                                   ELP007  
00653 ******************************************************************ELP007  
00654 ** THIS ROUTINE DOES A RANDOM ACCESS KEY READ ON THE RECORD     **ELP007  
00655 ** LIST FILE.  IF END OF FILE, A STATUS CODE IS SET BY COBLVSAM.**ELP007  
00656 ** THIS MEANS ELP007 IS READY TO TERMINATE NORMALLY.            **ELP007  
00657 ** IF A SERIOUS ERROR OCCURS, A MESSAGE IS PRODUCED             **ELP007  
00658 ** AND PROCESSING IS TERMINATED.                                **ELP007  
00659 ** KEY LENGTH INCLUDES THE 4-BYTE RDW FIELD AND THE FULL KEY    **ELP007  
00660 ** CONSISTING OF THE PREFIX.                                    **ELP007  
00661 ******************************************************************ELP007  
00662                                                                   ELP007  
00663  8020-READ-ELPRL.                                                 ELP007  
00664                                                                   ELP007  
00665      MOVE 'READ'   TO  MSG-ACTION.                                ELP007  
00666      MOVE 'ELPRL'  TO  MSG-FILE-NAME.                             ELP007  
00667      MOVE 'RL'     TO  ELBHIO-FILE-ID.                            ELP007  
00668      MOVE 'R'      TO  ELBHIO-REQUEST-TYPE.                       ELP007  
00669      MOVE 11       TO  ELBHIO-RECORD-LENGTH.                      ELP007  
00670      PERFORM 9210-TRANSLATE-TSGVSAM1-KEY THRU 9210-EXIT.          ELP007  
00671                                                                   ELP007  
00672      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP007  
00673                                                                   ELP007  
00674      IF ELBHIO-GOOD-RETURN                                        ELP007  
00675          MOVE ELBHIO-ELPRL     TO  RECORD-LIST                    ELP007  
00676      ELSE                                                         ELP007  
00677          IF ELBHIO-FEEDBACK  =  16                                ELP007  
00678              PERFORM 9000-VSAM-NOT-ON-FILE THRU 9000-EXIT         ELP007  
00679          ELSE                                                     ELP007  
00680              PERFORM 9100-VSAM-ABEND THRU 9100-EXIT.              ELP007  
00681                                                                   ELP007  
00682                                                                   ELP007  
00683  8020-EXIT.                                                       ELP007  
00684      EXIT.                                                        ELP007  
00685                                                                   ELP007  
00686 ******************************************************************ELP007  
00687 ** THIS ROUTINE DOES A SEQUENTIAL READ NEXT ON THE RECORD       **ELP007  
00688 ** LIST FILE.  IF END OF FILE, A STATUS CODE IS SET BY COBLVSAM.**ELP007  
00689 ** THIS MEANS ELP007 IS READY TO TERMINATE NORMALLY.            **ELP007  
00690 ** IF A SERIOUS ERROR OCCURS, A MESSAGE IS PRODUCED             **ELP007  
00691 ** AND PROCESSING IS TERMINATED.                                **ELP007  
00692 ******************************************************************ELP007  
00693  8030-READ-NEXT-ELPRL.                                            ELP007  
00694                                                                   ELP007  
00695      MOVE 'READNEXT' TO  MSG-ACTION.                              ELP007  
00696      MOVE 'ELPRL'    TO  MSG-FILE-NAME.                           ELP007  
00697      MOVE 'RL'       TO  ELBHIO-FILE-ID.                          ELP007  
00698      MOVE 'G'        TO  ELBHIO-REQUEST-TYPE.                     ELP007  
00699      PERFORM 9210-TRANSLATE-TSGVSAM1-KEY THRU 9210-EXIT.          ELP007  
00700                                                                   ELP007  
00701      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP007  
00702                                                                   ELP007  
00703      MOVE ELBHIO-REQUEST-TYPE  TO  HOLD-RL-REQUEST.               ELP007  
00704      MOVE ELBHIO-FEEDBACK      TO  HOLD-RL-FEEDBACK.              ELP007  
00705                                                                   ELP007  
00706      IF ELBHIO-GOOD-RETURN                                        ELP007  
00707          MOVE ELBHIO-ELPRL     TO  RECORD-LIST                    ELP007  
00708      ELSE                                                         ELP007  
00709          IF ELBHIO-REQUEST-TYPE  NOT  =  '2'                      ELP007  
00710                      AND                                          ELP007  
00711             ELBHIO-FEEDBACK      NOT  =  16                       ELP007  
00712              PERFORM 9100-VSAM-ABEND THRU 9100-EXIT.              ELP007  
00713                                                                   ELP007  
00714                                                                   ELP007  
00715  8030-EXIT.                                                       ELP007  
00716      EXIT.                                                        ELP007  
00717                                                                   ELP007  
00718 ******************************************************************ELP007  
00719 ** THIS ROUTINE DOES A RANDOM ACCESS KEY READ ON THE DATA ELE-  **ELP007  
00720 ** MENT FILE.  IT EXPECTS THE DESIRED KEY TO BE IN THE          **ELP007  
00721 ** SEARCH FIELD.  IF NOT FOUND, AN ERROR MESSAGE IS PRODUCED.   **ELP007  
00722 ** IF A MORE SERIOUS ERROR OCCURS, A MESSAGE IS PRODUCED        **ELP007  
00723 ** AND PROCESSING IS TERMINATED.                                **ELP007  
00724 ** KEY LENGTH INCLUDES THE 4-BYTE RDW FIELD AND THE FULL KEY    **ELP007  
00725 ** CONSISTING OF THE PREFIX AND THE DATA ELEMENT NUMBER.        **ELP007  
00726 ******************************************************************ELP007  
00727  8040-READ-ELPDE.                                                 ELP007  
00728                                                                   ELP007  
00729      MOVE 'READ'   TO  MSG-ACTION.                                ELP007  
00730      MOVE 'ELPDE'  TO  MSG-FILE-NAME.                             ELP007  
00731      MOVE 'DE'     TO  ELBHIO-FILE-ID.                            ELP007  
00732      MOVE 'R'      TO  ELBHIO-REQUEST-TYPE.                       ELP007  
00733      MOVE 15       TO  ELBHIO-RECORD-LENGTH.                      ELP007  
00734      PERFORM 9220-TRANSLATE-TSGVSAM2-KEY THRU 9220-EXIT.          ELP007  
00735                                                                   ELP007  
00736      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP007  
00737                                                                   ELP007  
00738      IF ELBHIO-GOOD-RETURN                                        ELP007  
00739          MOVE +11               TO  DE-NBR-DESC-LINES             ELP007  
00740          MOVE ELBHIO-ELPRL      TO  DATA-ELEMENT                  ELP007  
00741      ELSE                                                         ELP007  
00742          IF ELBHIO-FEEDBACK  =  16                                ELP007  
00743              PERFORM 9000-VSAM-NOT-ON-FILE THRU 9000-EXIT         ELP007  
00744          ELSE                                                     ELP007  
00745              PERFORM 9100-VSAM-ABEND THRU 9100-EXIT.              ELP007  
00746                                                                   ELP007  
00747                                                                   ELP007  
00748  8040-EXIT.                                                       ELP007  
00749      EXIT.                                                        ELP007  
00750                                                                   ELP007  
00751 ******************************************************************ELP007  
00752 ** THIS ROUTINE DOES A SEQUENTIAL READ NEXT ON THE DATA ELEMENT **ELP007  
00753 ** FILE.  IF AT END OF FILE, A STATUS CODE IS SET.              **ELP007  
00754 ** END OF FILE IS NOT NECESSARILY AN ERROR IN ELP007.           **ELP007  
00755 ** IF A SERIOUS ERROR OCCURS, A MESSAGE IS PRODUCED             **ELP007  
00756 ** AND PROCESSING IS TERMINATED.                                **ELP007  
00757 ******************************************************************ELP007  
00758                                                                   ELP007  
00759  8050-READ-NEXT-ELPDE.                                            ELP007  
00760                                                                   ELP007  
00761      MOVE 'READNEXT'  TO  MSG-ACTION.                             ELP007  
00762      MOVE 'ELPDE'     TO  MSG-FILE-NAME.                          ELP007  
00763      MOVE 'DE'        TO  ELBHIO-FILE-ID.                         ELP007  
00764      MOVE 'G'         TO  ELBHIO-REQUEST-TYPE.                    ELP007  
00765      PERFORM 9220-TRANSLATE-TSGVSAM2-KEY THRU 9220-EXIT.          ELP007  
00766                                                                   ELP007  
00767      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP007  
00768                                                                   ELP007  
00769      MOVE ELBHIO-REQUEST-TYPE  TO  HOLD-DE-REQUEST.               ELP007  
00770      MOVE ELBHIO-FEEDBACK      TO  HOLD-DE-FEEDBACK.              ELP007  
00771                                                                   ELP007  
00772      IF ELBHIO-GOOD-RETURN                                        ELP007  
00773          MOVE +11              TO  DE-NBR-DESC-LINES              ELP007  
00774          MOVE ELBHIO-ELPDE     TO  DATA-ELEMENT                   ELP007  
00775      ELSE                                                         ELP007  
00776          IF ELBHIO-REQUEST-TYPE NOT = '2'                         ELP007  
00777              IF ELBHIO-FEEDBACK  NOT  =  16                       ELP007  
00778                  PERFORM 9100-VSAM-ABEND THRU 9100-EXIT.          ELP007  
00779                                                                   ELP007  
00780                                                                   ELP007  
00781  8050-EXIT.                                                       ELP007  
00782      EXIT.                                                        ELP007  
00783                                                                   ELP007  
00784 ******************************************************************ELP007  
00785 ** THIS ROUTINE DOES A POINT (START BROWSE) ON THE DATA ELEMENT **ELP007  
00786 ** FILE.  IF AT END OF FILE, A STATUS CODE IS SET.              **ELP007  
00787 ** END OF FILE IS NOT NECESSARILY AN ERROR IN ELP007.           **ELP007  
00788 ** IF A SERIOUS ERROR OCCURS, A MESSAGE IS PRODUCED             **ELP007  
00789 ** AND PROCESSING IS TERMINATED.                                **ELP007  
00790 ** KEY LENGTH INCLUDES THE 4-BYTE RDW FIELD AND THE GENERIC     **ELP007  
00791 ** KEY CONSISTING OF THE PREFIX ONLY.                           **ELP007  
00792 ******************************************************************ELP007  
00793  8060-POINT-ELPDE.                                                ELP007  
00794                                                                   ELP007  
00795      MOVE 'POINT' TO  MSG-ACTION.                                 ELP007  
00796      MOVE 'ELPDE' TO  MSG-FILE-NAME.                              ELP007  
00797      MOVE 'DE'    TO  ELBHIO-FILE-ID.                             ELP007  
00798      MOVE 'P'     TO  ELBHIO-REQUEST-TYPE.                        ELP007  
00799      MOVE  12     TO  ELBHIO-RECORD-LENGTH.                       ELP007  
00800      PERFORM 9220-TRANSLATE-TSGVSAM2-KEY THRU 9220-EXIT.          ELP007  
00801                                                                   ELP007  
00802      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP007  
00803                                                                   ELP007  
00804      IF ELBHIO-GOOD-RETURN                                        ELP007  
00805          NEXT SENTENCE                                            ELP007  
00806      ELSE                                                         ELP007  
00807          IF ELBHIO-REQUEST-TYPE = '2'                             ELP007  
00808                     OR                                            ELP007  
00809             ELBHIO-FEEDBACK  =  4                                 ELP007  
00810              PERFORM 9000-VSAM-NOT-ON-FILE THRU 9000-EXIT         ELP007  
00811          ELSE                                                     ELP007  
00812              PERFORM 9100-VSAM-ABEND THRU 9100-EXIT.              ELP007  
00813                                                                   ELP007  
00814  8060-EXIT.                                                       ELP007  
00815      EXIT.                                                        ELP007  
00816                                                                   ELP007  
00817 ******************************************************************ELP007  
00818 ** THIS ROUTINE DOES A SEQUENTIAL READ NEXT ON THE CODE VALUE   **ELP007  
00819 ** FILE.  IF AT END OF FILE, A 'NOT ON FILE' MESSAGE IS BUILT.  **ELP007  
00820 ** IF A SERIOUS ERROR OCCURS, A MESSAGE IS PRODUCED             **ELP007  
00821 ** AND PROCESSING IS TERMINATED.                                **ELP007  
00822 ******************************************************************ELP007  
00823                                                                   ELP007  
00824  8080-READ-NEXT-ELPCV.                                            ELP007  
00825                                                                   ELP007  
00826                                                                   ELP007  
00827      MOVE 'READNEXT'  TO  MSG-ACTION.                             ELP007  
00828      MOVE 'ELPCV'     TO  MSG-FILE-NAME.                          ELP007  
00829      MOVE 'CV'        TO  ELBHIO-FILE-ID.                         ELP007  
00830      MOVE 'G'         TO  ELBHIO-REQUEST-TYPE.                    ELP007  
00831      PERFORM 9230-TRANSLATE-TSGVSAM3-KEY THRU 9230-EXIT.          ELP007  
00832                                                                   ELP007  
00833      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP007  
00834                                                                   ELP007  
00835      IF ELBHIO-GOOD-RETURN                                        ELP007  
00836          MOVE +12               TO  CV-NBR-VALUE-DESC-LINES       ELP007  
00837          MOVE ELBHIO-ELPCV      TO  ELEMENT-CODE-VALUE            ELP007  
00838      ELSE                                                         ELP007  
00839          IF ELBHIO-REQUEST-TYPE  NOT  =  '2'                      ELP007  
00840              IF ELBHIO-FEEDBACK  NOT  =  16                       ELP007  
00841                  PERFORM 9100-VSAM-ABEND THRU 9100-EXIT.          ELP007  
00842                                                                   ELP007  
00843                                                                   ELP007  
00844  8080-EXIT.                                                       ELP007  
00845      EXIT.                                                        ELP007  
00846                                                                   ELP007  
00847 ******************************************************************ELP007  
00848 ** THIS ROUTINE DOES A POINT (START BROWSE) ON THE CODE VALUE   **ELP007  
00849 ** FILE.  IF AT END OF FILE, A 'NOT ON FILE' MESSAGE IS BUILT.  **ELP007  
00850 ** IF A SERIOUS ERROR OCCURS, A MESSAGE IS PRODUCED             **ELP007  
00851 ** AND PROCESSING IS TERMINATED.                                **ELP007  
00852 ** KEY LENGTH INCLUDES THE 4-BYTE RDW FIELD AND THE             **ELP007  
00853 ** GENERIC KEY CONSISTING OF THE PREFIX AND DATA ELEMENT NUMBER.**ELP007  
00854 ******************************************************************ELP007  
00855  8090-POINT-ELPCV.                                                ELP007  
00856                                                                   ELP007  
00857      MOVE 'POINT' TO  MSG-ACTION.                                 ELP007  
00858      MOVE 'ELPCV' TO  MSG-FILE-NAME.                              ELP007  
00859      MOVE 'CV'    TO  ELBHIO-FILE-ID.                             ELP007  
00860      MOVE 'P'     TO  ELBHIO-REQUEST-TYPE.                        ELP007  
00861      MOVE 15      TO  ELBHIO-RECORD-LENGTH.                       ELP007  
00862      PERFORM 9230-TRANSLATE-TSGVSAM3-KEY THRU 9230-EXIT.          ELP007  
00863                                                                   ELP007  
00864      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP007  
00865                                                                   ELP007  
00866      IF ELBHIO-GOOD-RETURN                                        ELP007  
00867          NEXT SENTENCE                                            ELP007  
00868      ELSE                                                         ELP007  
00869          IF ELBHIO-REQUEST-TYPE = '2'                             ELP007  
00870                     OR                                            ELP007  
00871             ELBHIO-FEEDBACK = 4                                   ELP007  
00872              PERFORM 9000-VSAM-NOT-ON-FILE THRU 9000-EXIT         ELP007  
00873          ELSE                                                     ELP007  
00874              PERFORM 9100-VSAM-ABEND THRU 9100-EXIT.              ELP007  
00875                                                                   ELP007  
00876  8090-EXIT.                                                       ELP007  
00877      EXIT.                                                        ELP007  
00878                                                                   ELP007  
00879  8100-READ-PARM.                                                  ELP007  
00880                                                                   ELP007  
00881      READ PARM-FILE                                               ELP007  
00882          AT END                                                   ELP007  
00883              MOVE SWITCH-ON-STATUS TO PARM-EOF-INDICATOR.         ELP007  
00884                                                                   ELP007  
00885  8100-EXIT.                                                       ELP007  
00886      EXIT.                                                        ELP007  
00887                                                                   ELP007  
00888  8110-WRITE-TEMP-HDR.                                             ELP007  
00889                                                                   ELP007  
00890      WRITE HEADER-RECORD.                                         ELP007  
00891      ADD +1 TO TEMP-HDR-COUNT.                                    ELP007  
00892                                                                   ELP007  
00893  8110-EXIT.                                                       ELP007  
00894      EXIT.                                                        ELP007  
00895                                                                   ELP007  
00896  8120-WRITE-TEMP-RL.                                              ELP007  
00897      WRITE RECORD-LIST-LOAD.                                      ELP007  
00898      ADD +1 TO TEMP-RL-COUNT.                                     ELP007  
00899                                                                   ELP007  
00900  8120-EXIT.                                                       ELP007  
00901      EXIT.                                                        ELP007  
00902                                                                   ELP007  
00903  8130-WRITE-TEMP-DE.                                              ELP007  
00904                                                                   ELP007  
00905      WRITE DATA-ELEMENT-LOAD.                                     ELP007  
00906      ADD +1 TO TEMP-DE-COUNT.                                     ELP007  
00907                                                                   ELP007  
00908  8130-EXIT.                                                       ELP007  
00909      EXIT.                                                        ELP007  
00910                                                                   ELP007  
00911  8140-WRITE-TEMP-CV.                                              ELP007  
00912                                                                   ELP007  
00913      WRITE CODE-VALUE-LOAD.                                       ELP007  
00914      ADD +1 TO TEMP-CV-COUNT.                                     ELP007  
00915                                                                   ELP007  
00916  8140-EXIT.                                                       ELP007  
00917      EXIT.                                                        ELP007  
00918                                                                   ELP007  
00919 ******************************************************************ELP007  
00920 ** THIS ROUTINE IS USED FOR VSAM RANDOM ACCESS NOT FOUND (STATUS**ELP007  
00921 ** CODE 23) OR FOR A START AND READ NEXT THAT REACHES END OF    **ELP007  
00922 ** FILE (STATUS CODE 10) OR THAT IMMEDIATELY PASSES BEYOND THE  **ELP007  
00923 ** DESIRED KEY RANGE (STATUS CODE 00; CAN ONLY BE CHECKED BY    **ELP007  
00924 ** LOOKING AT THE KEY IN QUESTION).                             **ELP007  
00925 ******************************************************************ELP007  
00926  9000-VSAM-NOT-ON-FILE.                                           ELP007  
00927                                                                   ELP007  
00928      PERFORM 9200-VSAM-MSG-SETUP THRU 9200-EXIT.                  ELP007  
00929      MOVE 'NOT FOUND' TO MSG-COMMENT.                             ELP007  
00930      DISPLAY VSAM-ERR-MSG.                                        ELP007  
00931      DISPLAY VSAM-KEY-MSG.                                        ELP007  
00932                                                                   ELP007  
00933  9000-EXIT.                                                       ELP007  
00934      EXIT.                                                        ELP007  
00935                                                                   ELP007  
00936 ******************************************************************ELP007  
00937 ** THIS ROUTINE IS USED FOR VSAM ERRORS WHICH INDICATE SOMETHING**ELP007  
00938 ** SERIOUSLY WRONG WITH THE FILE.                               **ELP007  
00939 ******************************************************************ELP007  
00940  9100-VSAM-ABEND.                                                 ELP007  
00941                                                                   ELP007  
00942      PERFORM 9200-VSAM-MSG-SETUP THRU 9200-EXIT.                  ELP007  
00943      MOVE 'ABNORMAL EOJ' TO MSG-COMMENT.                          ELP007  
00944      DISPLAY VSAM-ERR-MSG.                                        ELP007  
00945      MOVE 16 TO RETURN-CODE.                                      ELP007  
00946      IF MSG-ACTION NOT = 'OPEN' AND MSG-ACTION NOT = 'CLOSE'      ELP007  
00947          DISPLAY VSAM-KEY-MSG.                                    ELP007  
00948                                                                   ELP007  
00949      PERFORM 8010-CLOSE-FILES THRU 8010-EXIT.                     ELP007  
00950                                                                   ELP007  
00951  9100-EXIT.                                                       ELP007  
00952      EXIT.                                                        ELP007  
00953                                                                   ELP007  
00954 ******************************************************************ELP007  
00955 ** MOVE VARIABLE FIELDS INTO VSAM ERROR MESSAGE.                **ELP007  
00956 ******************************************************************ELP007  
00957                                                                   ELP007  
00958  9200-VSAM-MSG-SETUP.                                             ELP007  
00959                                                                   ELP007  
00960      IF MSG-FILE-NAME = 'ELPRL'  OR  'ELPDE'  OR  'ELPCV'         ELP007  
00961          MOVE ELBHIO-FEEDBACK       TO  MSG-FEEDBACK              ELP007  
00962          MOVE ELBHIO-REQUEST-TYPE   TO  MSG-REQ-TYPE              ELP007  
00963          MOVE ELBHIO-RECORD-LENGTH  TO  MSG-RDW-LENGTH            ELP007  
00964      ELSE                                                         ELP007  
00965          MOVE ZEROES TO MSG-FEEDBACK                              ELP007  
00966          MOVE SPACES TO MSG-REQ-TYPE                              ELP007  
00967          MOVE ZEROES TO MSG-RDW-LENGTH.                           ELP007  
00968                                                                   ELP007  
00969  9200-EXIT.                                                       ELP007  
00970      EXIT.                                                        ELP007  
00971                                                                   ELP007  
00972 ******************************************************************ELP007  
00973 ** SINCE PART OF THE KEY FOR THE DATA ELEMENT AND CODE VALUE    **ELP007  
00974 ** FILES IS PACKED, THE KEY HAS TO BE BROKEN UP AND REFORMATTED **ELP007  
00975 ** WHENEVER IT IS BEING DISPLAYED IN AN ERROR MESSAGE.          **ELP007  
00976 ******************************************************************ELP007  
00977                                                                   ELP007  
00978  9210-TRANSLATE-TSGVSAM1-KEY.                                     ELP007  
00979                                                                   ELP007  
00980      MOVE ELBHIO-ELPRL-KEY      TO  MSG-KEY-PREFIX.               ELP007  
00981      MOVE ZEROES                TO  MSG-KEY-ELEMENT.              ELP007  
00982      MOVE SPACES                TO  MSG-KEY-CODE-VALUE.           ELP007  
00983      MOVE ZEROES                TO  MSG-KEY-SEQUENCE.             ELP007  
00984      MOVE ELBHIO-RECORD-LENGTH  TO  MSG-RDW-LENGTH.               ELP007  
00985                                                                   ELP007  
00986  9210-EXIT.                                                       ELP007  
00987      EXIT.                                                        ELP007  
00988                                                                   ELP007  
00989  9220-TRANSLATE-TSGVSAM2-KEY.                                     ELP007  
00990                                                                   ELP007  
00991      MOVE ELBHIO-DE-RECORD-PREFIX  TO  MSG-KEY-PREFIX.            ELP007  
00992                                                                   ELP007  
00993      IF ELBHIO-DE-ELEMENT-NBR  IS NUMERIC                         ELP007  
00994          MOVE ELBHIO-ELPDE-KEY  TO  MSG-KEY-ELEMENT               ELP007  
00995      ELSE                                                         ELP007  
00996          MOVE ZEROES TO MSG-KEY-ELEMENT.                          ELP007  
00997                                                                   ELP007  
00998      MOVE SPACES                TO  MSG-KEY-CODE-VALUE.           ELP007  
00999      MOVE ZEROES                TO  MSG-KEY-SEQUENCE.             ELP007  
01000      MOVE ELBHIO-RECORD-LENGTH  TO  MSG-RDW-LENGTH.               ELP007  
01001                                                                   ELP007  
01002  9220-EXIT.                                                       ELP007  
01003      EXIT.                                                        ELP007  
01004                                                                   ELP007  
01005  9230-TRANSLATE-TSGVSAM3-KEY.                                     ELP007  
01006                                                                   ELP007  
01007      MOVE ELBHIO-CV-RECORD-PREFIX  TO  MSG-KEY-PREFIX.            ELP007  
01008      IF ELBHIO-CV-ELEMENT-NBR  IS  NUMERIC                        ELP007  
01009          MOVE ELBHIO-CV-ELEMENT-NBR  TO  MSG-KEY-ELEMENT          ELP007  
01010      ELSE                                                         ELP007  
01011          MOVE ZEROES TO MSG-KEY-ELEMENT.                          ELP007  
01012                                                                   ELP007  
01013      MOVE ELBHIO-CV-CODE-VALUE  TO  MSG-KEY-CODE-VALUE.           ELP007  
01014                                                                   ELP007  
01015      IF ELBHIO-CV-CODE-DESC-SEQ  IS  NUMERIC                      ELP007  
01016         MOVE ELBHIO-CV-CODE-DESC-SEQ  TO  MSG-KEY-SEQUENCE        ELP007  
01017      ELSE                                                         ELP007  
01018          MOVE ZEROES TO MSG-KEY-SEQUENCE.                         ELP007  
01019                                                                   ELP007  
01020      MOVE ELBHIO-RECORD-LENGTH  TO  MSG-RDW-LENGTH.               ELP007  
01021                                                                   ELP007  
01022  9230-EXIT.                                                       ELP007  
01023      EXIT.                                                        ELP007  
01024                                                                   ELP007  
01025 ******************************************************************ELP007  
01026 ** ERROR MESSAGE FOR IMPROPERLY FORMATTED SELECTION PARMS.      **ELP007  
01027 ******************************************************************ELP007  
01028  9300-PARM-ERROR-MSG.                                             ELP007  
01029                                                                   ELP007  
01030      MOVE PARM-REC TO MSG-PARM-VALUE.                             ELP007  
01031      DISPLAY PARM-ERROR-MSG.                                      ELP007  
01032                                                                   ELP007  
01033  9300-EXIT.                                                       ELP007  
01034      EXIT.                                                        ELP007  
01035                                                                   ELP007  
01036  9999-ABEND-RTN.                                                  ELP007  
01037                                                                   ELP007  
01038      CALL  'TSGEND'  USING  ABEND-CODE.                           ELP007  
01039                                                                   ELP007  
01040  9999-EXIT.  EXIT.                                                ELP007  
