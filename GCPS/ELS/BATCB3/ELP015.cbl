00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELP015  
00003  PROGRAM-ID.         ELP015.                                         LV001
00004                                                                   ELP015  
00005  AUTHOR.             EDWARD G LISS.                               ELP015  
00006                                                                   ELP015  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELP015  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELP015  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELP015  
00010                      233 N. MICHIGAN AVE                          ELP015  
00011                      CHICAGO, ILLINOIS 60601                      ELP015  
00012                                                                   ELP015  
00013  DATE-WRITTEN.       28-NOV-1989.                                 ELP015  
00014                      ORIGINAL WRITTEN 18-OCT-1985 BY CAT LADY.    ELP015  
00015                                                                   ELP015  
00016  DATE-COMPILED.                                                   ELP015  
00017                                                                   ELP015  
00018  SECURITY.           COPYRIGHT 1989,                              ELP015  
00019                      HEALTH CARE SERVICE CORPORATION              ELP015  
00020                                                                   ELP015  
00021 ***************************************************************** ELP015  
00022 * THIS PROGRAM PRINTS THE TABLE OF CONTENTS FOR THE CODES       * ELP015  
00023 * MANUAL FOR THE ENGLISH LANGUAGE SUPPORT PROTOTYPE SYSTEM.     * ELP015  
00024 *                                                               * ELP015  
00025 * ELP015 IS DRIVEN BY SEQUENTIAL ACCESS TO THE RECORD LIST      * ELP015  
00026 * FILE.  FOR EACH RECORD ON THE RECORD LIST FILE, A START       * ELP015  
00027 * BROWSE IS PERFORMED AGAINST THE DATA ELEMENT FILE TO POINT    * ELP015  
00028 * AT THE FIRST DATA ELEMENT ASSOCIATED WITH THE RECORD LIST     * ELP015  
00029 * RECORD.  EACH OF THESE ARE READ SEQUENTIALLY AND LISTED.      * ELP015  
00030 * RECORD LIST OR DATA ELEMENT RECORDS THAT HAVE BEEN FLAGGED    * ELP015  
00031 * FOR DELETION ARE IGNORED FOR THE PURPOSES OF THIS REPORT.     * ELP015  
00032 *                                                               * ELP015  
00033 ***************************************************************** ELP015  
00034      TITLE 'PRINT CODES MANUAL TABLE OF CONTENTS'.                ELP015  
00035 ***************************************************************** ELP015  
00036 *                                                               * ELP015  
00037 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * ELP015  
00038 *    *-*         U P D A T E   H I S T O R Y         *-*        * ELP015  
00039 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * ELP015  
00040 *                                                               * ELP015  
00041 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* ELP015  
00042 *                                                               * ELP015  
00043 *  01.01     11/28/89  EGL  REWROTE PROGRAM TO SUPPORT AFP      * ELP015  
00044 *                                                               * ELP015  
00045 ***************************************************************** ELP015  
00046                                                                   ELP015  
00047  ENVIRONMENT DIVISION.                                            ELP015  
00048  CONFIGURATION SECTION.                                           ELP015  
00049  SOURCE-COMPUTER.  IBM-370.                                       ELP015  
00050  OBJECT-COMPUTER.  IBM-370.                                       ELP015  
00051                                                                   ELP015  
00052  SPECIAL-NAMES.                                                   ELP015  
00053         C01 IS TOP-OF-FORM.                                       ELP015  
00054  INPUT-OUTPUT SECTION.                                            ELP015  
00055  FILE-CONTROL.                                                    ELP015  
00056                                                                   ELP015  
00057      SELECT PRINT-FILE                                            ELP015  
00058          ASSIGN TO PRTFILE.                                       ELP015  
00059 /                                                                 ELP015  
00060  DATA DIVISION.                                                   ELP015  
00061  FILE SECTION.                                                    ELP015  
00062                                                                   ELP015  
00063  FD  PRINT-FILE                                                   ELP015  
00064      BLOCK CONTAINS  0  RECORDS                                   ELP015  
00065      LABEL RECORDS ARE STANDARD                                   ELP015  
00066      RECORDING MODE IS F                                          ELP015  
00067      RECORD CONTAINS 133 CHARACTERS                               ELP015  
00068      DATA RECORD IS PRT-REC.                                      ELP015  
00069  01  PRT-REC.                                                     ELP015  
00070      05  FILLER                 PIC X.                            ELP015  
00071      05  PT-AFP                 PIC X.                            ELP015  
00072      05  PT-DATA                PIC X(131).                       ELP015  
00073 /                                                                 ELP015  
00074  WORKING-STORAGE SECTION.                                         ELP015  
00075  77  FILLER                         PIC X(30)                     ELP015  
00076        VALUE 'ELP015 WORKING STORAGE BEGINS'.                     ELP015  
00077                                                                   ELP015  
00078  01  MISC-WORK.                                                   ELP015  
00079      05  WS-LINE-NO                 PIC S9999 COMP VALUE +0.      ELP015  
00080      05  WS-RL-EOF-IND              PIC  X(1)  VALUE 'N'.         ELP015  
00081          88  WS-RL-EOF                         VALUE 'Y'.         ELP015  
00082          88  WS-RL-NOT-EOF                     VALUE 'N'.         ELP015  
00083      05  WS-DE-EOF-IND              PIC  X(1)  VALUE 'N'.         ELP015  
00084          88  WS-DE-EOF                         VALUE 'Y'.         ELP015  
00085          88  WS-DE-NOT-EOF                     VALUE 'N'.         ELP015  
00086      05  MAX-LINES                  PIC S9999 COMP VALUE +54.     ELP015  
00087      05  DOT-FILL                   PIC XX    VALUE '. '.         ELP015  
00088                                                                   ELP015  
00089  01  WS-AFP-DATA.                                                 ELP015  
00090      05  WS-NORMAL-FONT             PIC X     VALUE SPACE.        ELP015  
00091                                                                   ELP015  
00092  01  WS-YYMMDD                      PIC 9(6)  VALUE ZEROES.       ELP015  
00093  01  WS-YYMMDD-RDF REDEFINES WS-YYMMDD.                           ELP015  
00094      05  WS-YY                      PIC 99.                       ELP015  
00095      05  WS-MM                      PIC 99.                       ELP015  
00096      05  WS-DD                      PIC 99.                       ELP015  
00097 /                                                                 ELP015  
00098 ******************************************************************ELP015  
00099 *         CALLING PARAMETERS FOR THE IO INTERFACE 'ELBIOPGM'     *ELP015  
00100 ******************************************************************ELP015  
00101  01  IO-INFO.                                                     ELP015  
00102      COPY ELBHIOPM.                                               ELP015  
00103 /                                                                 ELP015  
00104  01  COVER1.                                                      ELP015  
00105      05  FILLER        PIC X(20) VALUE '                    '.    ELP015  
00106      05  FILLER        PIC X(20) VALUE '           ENGLISH L'.    ELP015  
00107      05  FILLER        PIC X(20) VALUE 'ANGUAGE SUPPORT     '.    ELP015  
00108      05  FILLER        PIC X(20) VALUE '                    '.    ELP015  
00109      05  FILLER        PIC X(5)  VALUE '     '.                   ELP015  
00110  01  COVER2.                                                      ELP015  
00111      05  FILLER        PIC X(20) VALUE '                    '.    ELP015  
00112      05  FILLER        PIC X(20) VALUE '        CODES MANUAL'.    ELP015  
00113      05  FILLER        PIC X(20) VALUE ' TABLE OF CONTENTS  '.    ELP015  
00114      05  FILLER        PIC X(20) VALUE '                    '.    ELP015  
00115      05  FILLER        PIC X(5)  VALUE '     '.                   ELP015  
00116  01  COVER3.                                                      ELP015  
00117      05  FILLER        PIC X(20) VALUE '                    '.    ELP015  
00118      05  FILLER        PIC X(20) VALUE '       HEALTH CARE S'.    ELP015  
00119      05  FILLER        PIC X(20) VALUE 'ERVICE CORPORATION  '.    ELP015  
00120      05  FILLER        PIC X(20) VALUE '                    '.    ELP015  
00121      05  FILLER        PIC X(5)  VALUE '     '.                   ELP015  
00122  01  COVER4.                                                      ELP015  
00123      05  FILLER        PIC X(39) VALUE SPACES.                    ELP015  
00124      05  COVER4-DATE.                                             ELP015  
00125          10  COVER4-MM PIC 99    VALUE ZEROES.                    ELP015  
00126          10  FILLER    PIC X     VALUE '/'.                       ELP015  
00127          10  COVER4-DD PIC 99    VALUE ZEROES.                    ELP015  
00128          10  FILLER    PIC X     VALUE '/'.                       ELP015  
00129          10  COVER4-YY PIC 99    VALUE ZEROES.                    ELP015  
00130      05  FILLER        PIC X(38) VALUE SPACES.                    ELP015  
00131  01  HDR1.                                                        ELP015  
00132      05  FILLER        PIC X(20) VALUE 'ENGLISH LANGUAGE SUP'.    ELP015  
00133      05  FILLER        PIC X(20) VALUE 'PORT - CODES MANUAL '.    ELP015  
00134      05  FILLER        PIC X(20) VALUE 'TABLE OF CONTENTS   '.    ELP015  
00135      05  FILLER        PIC X(17) VALUE '                 '.       ELP015  
00136      05  HDR1-DATE     PIC X(8)  VALUE SPACES.                    ELP015  
00137  01  HDR2.                                                        ELP015  
00138      05  HDR2-PREFIX        PIC X(8)  VALUE SPACES.               ELP015  
00139      05  FILLER             PIC XXX   VALUE ' - '.                ELP015  
00140      05  HDR2-RECORD-NAME   PIC X(50) VALUE SPACES.               ELP015  
00141      05  FILLER             PIC X     VALUE SPACES.               ELP015  
00142      05  HDR2-CONTINUED     PIC X(11) VALUE SPACES.               ELP015  
00143      05  FILLER             PIC X(12) VALUE SPACES.               ELP015  
00144  01  HDR3.                                                        ELP015  
00145      05  FILLER             PIC X(17) VALUE 'DATA ELEMENT NAME'.  ELP015  
00146      05  FILLER             PIC X(62) VALUE SPACES.               ELP015  
00147      05  FILLER             PIC X(6)  VALUE 'NUMBER'.             ELP015  
00148 ******************************************************************ELP015  
00149 ** AFTER WE MOVE THE DATA ELEMENT NAME TO THE ARRAY, WE PAD     **ELP015  
00150 ** THE REST OF THE LINE WITH SPACED DOTS TO PROVIDE A VISUAL    **ELP015  
00151 ** LEAD TO THE ELEMENT NUMBER.                                  **ELP015  
00152 ******************************************************************ELP015  
00153  01  DET1.                                                        ELP015  
00154      05  DET1-ELEMENT-NAME  PIC X(75) VALUE SPACES.               ELP015  
00155      05  CUTE-DOTS REDEFINES DET1-ELEMENT-NAME.                   ELP015  
00156          10  FILLER         PIC X.                                ELP015  
00157          10  DET1-ITEM      OCCURS 37 TIMES                       ELP015  
00158                             INDEXED BY DET1-IX                    ELP015  
00159                             PIC XX.                               ELP015  
00160      05  FILLER             PIC X(4)  VALUE '. . '.               ELP015  
00161      05  DET1-ELEMENT-NO    PIC 999.99 VALUE ZEROES.              ELP015  
00162  01  ABEND-CODE                     PIC   9(04) COMP VALUE ZERO.  ELP015  
00163 /                                                                 ELP015  
00164 ******************************************************************ELP015  
00165 **            HOLD AREA FOR THE VSAM RECORD.                    **ELP015  
00166 ******************************************************************ELP015  
00167  01  HOLD-RL-RECORD.                                              ELP015  
00168      COPY ELPRLC.                                                 ELP015  
00169      SKIP3                                                        ELP015  
00170  01  HOLD-DE-RECORD.                                              ELP015  
00171      COPY ELPDEC.                                                 ELP015  
00172 /                                                                 ELP015  
00173  PROCEDURE DIVISION.                                              ELP015  
00174                                                                   ELP015  
00175  0000-MAINLINE.                                                   ELP015  
00176                                                                   ELP015  
00177      PERFORM 0500-OPEN-FILES.                                     ELP015  
00178                                                                   ELP015  
00179      PERFORM 4100-PRINT-COVER-SHEET.                              ELP015  
00180                                                                   ELP015  
00181      PERFORM 8030-READ-NEXT-ELPRL.                                ELP015  
00182      PERFORM 1000-EXAMINE-ELPRL-RECORD                            ELP015  
00183          UNTIL WS-RL-EOF.                                         ELP015  
00184                                                                   ELP015  
00185      PERFORM 8000-CLOSE-FILES.                                    ELP015  
00186                                                                   ELP015  
00187      MOVE ZERO TO RETURN-CODE.                                    ELP015  
00188      GOBACK.                                                      ELP015  
00189                                                                   ELP015  
00190  0500-OPEN-FILES.                                                 ELP015  
00191 ******************************************************************ELP015  
00192 **    ONLY ONE OPEN IS ISSUED FOR THE VSAM FILES (RL,DE).  THE  **ELP015  
00193 ** IO INTERFACE WILL OPEN ALL THE VSAM FILES NEEDED.            **ELP015  
00194 ******************************************************************ELP015  
00195                                                                   ELP015  
00196      OPEN OUTPUT PRINT-FILE.                                      ELP015  
00197                                                                   ELP015  
00198      SET RECORD-LIST-FILE TO TRUE.                                ELP015  
00199      SET ELBHIO-OPEN TO TRUE.                                     ELP015  
00200                                                                   ELP015  
00201      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP015  
00202                                                                   ELP015  
00203      IF ELBHIO-GOOD-RETURN                                        ELP015  
00204          NEXT SENTENCE                                            ELP015  
00205      ELSE                                                         ELP015  
00206          MOVE ELBHIO-FEEDBACK  TO  ABEND-CODE                     ELP015  
00207          PERFORM 9999-ABEND-RTN.                                  ELP015  
00208 /                                                                 ELP015  
00209  1000-EXAMINE-ELPRL-RECORD.                                       ELP015  
00210      IF RL-DELETE    OR                                           ELP015  
00211         RL-RECORD-PREFIX  =  SPACES  OR  LOW-VALUES               ELP015  
00212            CONTINUE                                               ELP015  
00213      ELSE                                                         ELP015  
00214          PERFORM 1700-REPORT-DE-WITHIN-RL.                        ELP015  
00215                                                                   ELP015  
00216      PERFORM 8030-READ-NEXT-ELPRL.                                ELP015  
00217                                                                   ELP015  
00218  1700-REPORT-DE-WITHIN-RL.                                        ELP015  
00219      MOVE SPACES TO HDR2-CONTINUED.                               ELP015  
00220      MOVE RL-RECORD-PREFIX TO HDR2-PREFIX.                        ELP015  
00221      MOVE RL-RECORD-NAME TO HDR2-RECORD-NAME.                     ELP015  
00222      PERFORM 4000-TOP-PAGE.                                       ELP015  
00223      MOVE '(CONTINUED)' TO HDR2-CONTINUED.                        ELP015  
00224                                                                   ELP015  
00225      MOVE RL-RECORD-PREFIX  TO  ELBHIO-DE-RECORD-PREFIX.          ELP015  
00226      PERFORM 8060-POINT-ELPDE.                                    ELP015  
00227      IF ELBHIO-POINT                                              ELP015  
00228          PERFORM 1800-EXAMINE-ELPDE-RECORD                        ELP015  
00229             WITH TEST AFTER                                       ELP015  
00230             UNTIL WS-DE-EOF.                                      ELP015  
00231 /                                                                 ELP015  
00232  1800-EXAMINE-ELPDE-RECORD.                                       ELP015  
00233                                                                   ELP015  
00234      PERFORM 8050-READ-NEXT-ELPDE.                                ELP015  
00235                                                                   ELP015  
00236      IF WS-DE-EOF OR DE-DELETE                                    ELP015  
00237           CONTINUE                                                ELP015  
00238      ELSE                                                         ELP015  
00239           PERFORM 1810-PRINT-DATA-ELEMENT.                        ELP015  
00240                                                                   ELP015  
00241  1810-PRINT-DATA-ELEMENT.                                         ELP015  
00242                                                                   ELP015  
00243      IF WS-LINE-NO GREATER THAN MAX-LINES                         ELP015  
00244          PERFORM 4000-TOP-PAGE.                                   ELP015  
00245                                                                   ELP015  
00246      MOVE DE-ELEMENT-NBR  TO DET1-ELEMENT-NO.                     ELP015  
00247      MOVE DE-ELEMENT-NAME TO DET1-ELEMENT-NAME.                   ELP015  
00248      SET DET1-IX TO 37.                                           ELP015  
00249      PERFORM  UNTIL DET1-IX = 1                                   ELP015  
00250          IF DET1-ITEM (DET1-IX) = SPACES                          ELP015  
00251              MOVE DOT-FILL TO DET1-ITEM (DET1-IX)                 ELP015  
00252              SET DET1-IX DOWN BY 1                                ELP015  
00253          ELSE                                                     ELP015  
00254              SET DET1-IX TO 1                                     ELP015  
00255          END-IF                                                   ELP015  
00256      END-PERFORM.                                                 ELP015  
00257                                                                   ELP015  
00258      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP015  
00259      MOVE DET1 TO PT-DATA.                                        ELP015  
00260      WRITE PRT-REC AFTER ADVANCING 2 LINES.                       ELP015  
00261                                                                   ELP015  
00262      ADD +2 TO WS-LINE-NO.                                        ELP015  
00263 /                                                                 ELP015  
00264  4000-TOP-PAGE.                                                   ELP015  
00265                                                                   ELP015  
00266      MOVE SPACES TO PRT-REC.                                      ELP015  
00267      WRITE PRT-REC AFTER ADVANCING TOP-OF-FORM.                   ELP015  
00268                                                                   ELP015  
00269      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP015  
00270      MOVE HDR1 TO PT-DATA.                                        ELP015  
00271      WRITE PRT-REC AFTER ADVANCING 3 LINES.                       ELP015  
00272                                                                   ELP015  
00273      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP015  
00274      MOVE HDR2 TO PT-DATA.                                        ELP015  
00275      WRITE PRT-REC AFTER ADVANCING 2 LINES.                       ELP015  
00276                                                                   ELP015  
00277      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP015  
00278      MOVE HDR3 TO PT-DATA.                                        ELP015  
00279      WRITE PRT-REC AFTER ADVANCING 2 LINES.                       ELP015  
00280                                                                   ELP015  
00281      MOVE SPACES TO PRT-REC.                                      ELP015  
00282      WRITE PRT-REC AFTER ADVANCING 1 LINE.                        ELP015  
00283                                                                   ELP015  
00284      MOVE +9 TO WS-LINE-NO.                                       ELP015  
00285 /                                                                 ELP015  
00286  4100-PRINT-COVER-SHEET.                                          ELP015  
00287                                                                   ELP015  
00288      MOVE SPACES TO PRT-REC.                                      ELP015  
00289      WRITE PRT-REC AFTER ADVANCING TOP-OF-FORM.                   ELP015  
00290                                                                   ELP015  
00291      MOVE SPACES TO PRT-REC.                                      ELP015  
00292      WRITE PRT-REC AFTER ADVANCING 6 LINES.                       ELP015  
00293                                                                   ELP015  
00294      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP015  
00295      MOVE COVER1 TO PT-DATA.                                      ELP015  
00296      WRITE PRT-REC AFTER ADVANCING 3 LINES.                       ELP015  
00297                                                                   ELP015  
00298      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP015  
00299      MOVE COVER2 TO PT-DATA.                                      ELP015  
00300      WRITE PRT-REC AFTER ADVANCING 3 LINES.                       ELP015  
00301                                                                   ELP015  
00302      MOVE SPACES TO PRT-REC.                                      ELP015  
00303      WRITE PRT-REC AFTER ADVANCING 12 LINES.                      ELP015  
00304                                                                   ELP015  
00305      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP015  
00306      MOVE COVER3 TO PT-DATA.                                      ELP015  
00307      WRITE PRT-REC AFTER ADVANCING 3 LINES.                       ELP015  
00308                                                                   ELP015  
00309      ACCEPT WS-YYMMDD FROM DATE                                   ELP015  
00310      MOVE WS-MM TO COVER4-MM                                      ELP015  
00311      MOVE WS-DD TO COVER4-DD                                      ELP015  
00312      MOVE WS-YY TO COVER4-YY                                      ELP015  
00313                                                                   ELP015  
00314      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP015  
00315      MOVE COVER4 TO PT-DATA.                                      ELP015  
00316      WRITE PRT-REC AFTER ADVANCING 3 LINES.                       ELP015  
00317      MOVE COVER4-DATE TO HDR1-DATE.                               ELP015  
00318 /                                                                 ELP015  
00319  8000-CLOSE-FILES.                                                ELP015  
00320                                                                   ELP015  
00321      CLOSE PRINT-FILE.                                            ELP015  
00322      SET RECORD-LIST-FILE TO TRUE.                                ELP015  
00323      SET ELBHIO-CLOSE TO TRUE.                                    ELP015  
00324                                                                   ELP015  
00325      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP015  
00326                                                                   ELP015  
00327      IF ELBHIO-GOOD-RETURN                                        ELP015  
00328          NEXT SENTENCE                                            ELP015  
00329      ELSE                                                         ELP015  
00330          MOVE ELBHIO-FEEDBACK TO ABEND-CODE                       ELP015  
00331          PERFORM 9999-ABEND-RTN.                                  ELP015  
00332                                                                   ELP015  
00333  8030-READ-NEXT-ELPRL.                                            ELP015  
00334                                                                   ELP015  
00335      SET RECORD-LIST-FILE TO TRUE.                                ELP015  
00336      SET ELBHIO-SEQUENTIAL-GET TO TRUE.                           ELP015  
00337                                                                   ELP015  
00338      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP015  
00339                                                                   ELP015  
00340      IF ELBHIO-GOOD-RETURN                                        ELP015  
00341          MOVE ELBHIO-ELPRL  TO  RECORD-LIST                       ELP015  
00342      ELSE                                                         ELP015  
00343          IF ELBHIO-REQUEST-TYPE = '2' AND                         ELP015  
00344             ELBHIO-FEEDBACK = ZERO                                ELP015  
00345                  SET WS-RL-EOF TO TRUE                            ELP015  
00346          ELSE                                                     ELP015  
00347                  MOVE ELBHIO-FEEDBACK TO ABEND-CODE               ELP015  
00348                  PERFORM 9999-ABEND-RTN                           ELP015  
00349          END-IF                                                   ELP015  
00350      END-IF.                                                      ELP015  
00351 /                                                                 ELP015  
00352  8050-READ-NEXT-ELPDE.                                            ELP015  
00353                                                                   ELP015  
00354      SET DATA-ELEMENT-FILE TO TRUE.                               ELP015  
00355      SET ELBHIO-SEQUENTIAL-GET TO TRUE.                           ELP015  
00356                                                                   ELP015  
00357      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP015  
00358                                                                   ELP015  
00359      IF ELBHIO-GOOD-RETURN                                        ELP015  
00360          MOVE +11               TO  DE-NBR-DESC-LINES             ELP015  
00361          MOVE ELBHIO-ELPDE      TO  DATA-ELEMENT                  ELP015  
00362          IF DE-RECORD-PREFIX = RL-RECORD-PREFIX                   ELP015  
00363              SET WS-DE-NOT-EOF TO TRUE                            ELP015  
00364          ELSE                                                     ELP015  
00365              SET WS-DE-EOF TO TRUE                                ELP015  
00366          END-IF                                                   ELP015  
00367      ELSE                                                         ELP015  
00368          IF ELBHIO-REQUEST-TYPE  =  '2' AND                       ELP015  
00369             ELBHIO-FEEDBACK  = ZERO                               ELP015  
00370                 SET WS-DE-EOF TO TRUE                             ELP015  
00371          ELSE                                                     ELP015  
00372               MOVE ELBHIO-FEEDBACK TO ABEND-CODE                  ELP015  
00373               PERFORM 9999-ABEND-RTN                              ELP015  
00374          END-IF                                                   ELP015  
00375      END-IF.                                                      ELP015  
00376 /                                                                 ELP015  
00377  8060-POINT-ELPDE.                                                ELP015  
00378                                                                   ELP015  
00379      SET DATA-ELEMENT-FILE TO TRUE.                               ELP015  
00380      SET ELBHIO-POINT TO TRUE.                                    ELP015  
00381                                                                   ELP015  
00382      MOVE 11       TO  ELBHIO-RECORD-LENGTH.                      ELP015  
00383                                                                   ELP015  
00384      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP015  
00385                                                                   ELP015  
00386      IF ELBHIO-GOOD-RETURN                                        ELP015  
00387          NEXT SENTENCE                                            ELP015  
00388      ELSE                                                         ELP015  
00389          MOVE ELBHIO-FEEDBACK  TO ABEND-CODE                      ELP015  
00390          PERFORM 9999-ABEND-RTN.                                  ELP015  
00391                                                                   ELP015  
00392                                                                   ELP015  
00393  9999-ABEND-RTN.                                                  ELP015  
00394                                                                   ELP015  
00395      CALL 'TSGEND'  USING  ABEND-CODE.                            ELP015  
