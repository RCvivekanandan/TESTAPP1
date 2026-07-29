00001   IDENTIFICATION DIVISION.                                        06/29/02
00002   PROGRAM-ID.  ELP010.                                            ELP010  
00003   AUTHOR.   JOHN CURIN  --- KEANE,INC.                               LV001
00004   INSTALLATION.   HEALTH CARE SERVICE CORPORATION.                ELP010  
00005   DATE-WRITTEN.   OCTOBER, 1985.                                  ELP010  
00006   DATE-COMPILED.                                                  ELP010  
00007 ****** THIS PROGRAM WILL LOAD THE VSAM FILES OF THE TEMPORARY     ELP010  
00008 ******  ELS SYSTEM. THE FILES ARE RECORD LIST, DATA ELEMENT AND   ELP010  
00009 ******  CODE VALUE.  THE PROGRAM HAS TWO MODES OF OPERATION. THE  ELP010  
00010 ******  FIRST MODE WILL LOAD ALL THREE FILES.  THE SECOND MODE WILELP010  
00011 ******  LOAD OR REPLACE AND LOAD A SINGLE RECORD(STRUCTURE, IE ALLELP010  
00012 ******  ASSOCIATED DATA ELEMENTS AND CODE VALUES).                ELP010  
00013 ******   THE INPUT FILE IS THE TEMPORARY WORK FILE.               ELP010  
00014 ******   THE OUTPUT FILES ARE THE VSAM FILES.                     ELP010  
00015 ******   AN ERROR REPORT IS PRINTED WHEN ERRORS OCCUR.            ELP010  
00016 ******   THERE ARE 2 ERRORS WHICH WILL TERMINATE THE RUN ABNORMALLELP010  
00017 ******   THE FIRST ERROR IS IF THERE IS NO HEADER RECORD ENCOUNTERELP010  
00018 ******    IN THE WORK FILE.                                       ELP010  
00019 ******   THE SECOND ERROR IS IF AN INVALID RECORD TYPE IS ENCOUNTEELP010  
00020 ******    IN THE WORK FILE.                                       ELP010  
00021 ***************************************************************** ELP010  
00022 *****    DATE    PGMER   UPDATES (MOST CURRENT AT TOP)            ELP010  
00023 ***** ----------------------------------------------------------  ELP010  
00024 ***** 03/03/86   LET    CHANGED FROM COBLVSAM TO AN ELS IO        ELP010  
00025 *****                   INTERFACE                                 ELP010  
00026 *****                                                             ELP010  
00027 *****  2/21/86  CHANGE FILE ACCESS TO USE COBLVSAM SUBROUTINE.    ELP010  
00028 *****                                                             ELP010  
00029 *****  4/19/02  CHANGING CURRENT-DATE TO CONFORM TO OS390.        ELP010  
00030 ***************************************************************** ELP010  
00031 /                                                                 ELP010  
00032   ENVIRONMENT DIVISION.                                           ELP010  
00033   CONFIGURATION SECTION.                                          ELP010  
00034   SOURCE-COMPUTER.  IBM-3081.                                     ELP010  
00035   OBJECT-COMPUTER.  IBM-3081.                                     ELP010  
00036   SPECIAL-NAMES.                                                  ELP010  
00037          C01 IS TOP-OF-FORM.                                      ELP010  
00038   INPUT-OUTPUT SECTION.                                           ELP010  
00039   FILE-CONTROL.                                                   ELP010  
00040          SELECT   PRINT-FILE     ASSIGN TO PRINT.                 ELP010  
00041                                                                   ELP010  
00042          SELECT   TEMP-WORK-FILE ASSIGN TO DA-S-TEMPFILE.         ELP010  
00043                                                                   ELP010  
00044   DATA DIVISION.                                                  ELP010  
00045   FILE SECTION.                                                   ELP010  
00046   FD  PRINT-FILE                                                  ELP010  
00047       BLOCK 0                                                     ELP010  
00048       LABEL RECORD STANDARD                                       ELP010  
00049       RECORD CONTAINS 133 CHARACTERS                              ELP010  
00050       DATA RECORD IS PRT-REC.                                     ELP010  
00051   01  PRT-REC.                                                    ELP010  
00052       05  PO-CTL                 PIC X.                           ELP010  
00053       05  PO-REST                PIC X(132).                      ELP010  
00054                                                                   ELP010  
00055   FD  TEMP-WORK-FILE                                              ELP010  
00056       RECORDING V                                                 ELP010  
00057       BLOCK 0                                                     ELP010  
00058       LABEL RECORD STANDARD.                                      ELP010  
00059   COPY ELPMUC.                                                    ELP010  
00060 /                                                                 ELP010  
00061   WORKING-STORAGE SECTION.                                        ELP010  
00062   01  BEGIN-WS                   PIC X(40)     VALUE              ELP010  
00063       '**** WORKING STORAGE BEGINS HERE ****   '.                 ELP010  
00064                                                                   ELP010  
00065 ****** HOLD AREA FOR DATA ELEMENT                                 ELP010  
00066   01 HOLD-DE.                                                     ELP010  
00067      05  HOLD-DATA-AREA1        PIC X(130).                       ELP010  
00068      05  HOLD-DE-CODE-VALUES-CT PIC S9(04)     COMP.              ELP010  
00069      05  HOLD-DATE-AREA2        PIC X(38).                        ELP010  
00070      05  HOLD-DE-NBR-DESC-LINES PIC S9(03)    COMP-3.             ELP010  
00071      05  HOLD-DE-DESC-LINE      PIC X(79)                         ELP010  
00072                                 OCCURS 1 TO 11 TIMES              ELP010  
00073                                 DEPENDING ON                      ELP010  
00074                                 HOLD-DE-NBR-DESC-LINES.           ELP010  
00075                                                                   ELP010  
00076   01  RL-COUNT                   PIC 99        VALUE ZEROS.       ELP010  
00077   01  DE-COUNT                   PIC 99        VALUE ZEROS.       ELP010  
00078   01  CV-COUNT                   PIC 99        VALUE ZEROS.       ELP010  
00079   01  CODE-VALUES-CT             PIC S9(04)    COMP.              ELP010  
00080                                                                   ELP010  
00081 ***** STATUS-CODES ******                                         ELP010  
00082   01  TEST-ERR-STATUS            PIC XX        VALUE SPACES.      ELP010  
00083       88  DUP-REC-RL-CV          VALUE '22'.                      ELP010  
00084       88  DUP-REC-DE             VALUE '92'.                      ELP010  
00085                                                                   ELP010  
00086   01  ABEND-CODE                 PIC 9(04)     COMP VALUE ZEROS.  ELP010  
00087                                                                   ELP010  
00088   01  MAXIMUM-COUNTS.                                             ELP010  
00089       05  MAX-CV-DESC            PIC 9(02)     VALUE 12.          ELP010  
00090       05  MAX-DE-DESC            PIC 9(02)     VALUE 11.          ELP010  
00091                                                                   ELP010  
00092   01  LITERALS.                                                   ELP010  
00093       05  R-LIST                 PIC X(12)     VALUE              ELP010  
00094           'RECORD LIST'.                                          ELP010  
00095       05  D-ELEMENT              PIC X(12)     VALUE              ELP010  
00096           'DATA ELEMENT'.                                         ELP010  
00097       05  C-VALUE                PIC X(12)     VALUE              ELP010  
00098           'CODE VALUE'.                                           ELP010  
00099       05  STATUS-EQUAL           PIC X(10)     VALUE              ELP010  
00100           ' STATUS = '.                                           ELP010  
00101 /                                                                 ELP010  
00102   01  CV-WORK-AREA.                                               ELP010  
00103       COPY ELPCVC.                                                ELP010  
00104       SKIP3                                                       ELP010  
00105   01  DE-WORK-AREA.                                               ELP010  
00106       COPY ELPDEC.                                                ELP010  
00107       SKIP3                                                       ELP010  
00108   01  RL-WORK-AREA.                                               ELP010  
00109       COPY ELPRLC.                                                ELP010  
00110       SKIP3                                                       ELP010  
00111   01  WORK-AREAS.                                                 ELP010  
00112       05  RECORD-TYPE-TEST       PIC X      VALUE SPACE.          ELP010  
00113           88   RL-RECORD         VALUE 'A'.                       ELP010  
00114           88   DE-RECORD         VALUE 'B'.                       ELP010  
00115           88   CV-RECORD         VALUE 'C'.                       ELP010  
00116       05  RUN-TYPE               PIC X      VALUE SPACE.          ELP010  
00117           88   FILE-RUN          VALUE 'F'.                       ELP010  
00118           88   RECORD-RUN        VALUE 'R'.                       ELP010  
00119                                                                   ELP010  
00120   01  PROGRAM-ERROR-MESSSAGES    PIC X(43)  VALUE                 ELP010  
00121       '**** PROGRAM ERROR MESSAGES START HERE ****'.              ELP010  
00122   01  ERROR-MESSAGES.                                             ELP010  
00123       05  CODE-VALUE-NOT-LOADED   PIC X(42)  VALUE                ELP010  
00124       '      CODE VALUE NOT LOADED               '.               ELP010  
00125       05  DATA-ELEM-NOT-LOADED    PIC X(42)  VALUE                ELP010  
00126       '      DATA ELEMENT NOT LOADED             '.               ELP010  
00127       05  REC-LIST-NOT-LOADED     PIC X(42)  VALUE                ELP010  
00128       '      RECORD LIST NOT LOADED              '.               ELP010  
00129       05  REC-LIST-DELETE-FAIL    PIC X(45)  VALUE                ELP010  
00130       'ELP010RL LOAD PROGRAM REC LIST DELETE FAILED '.            ELP010  
00131       05  CODE-VAL-DELETE-FAIL    PIC X(45)  VALUE                ELP010  
00132       'ELP010RL LOAD PROGRAM CODE VAL DELETE FAILED '.            ELP010  
00133       05  DATA-ELEM-DELETE-FAIL   PIC X(45)  VALUE                ELP010  
00134       'ELP010RL LOAD PROGRAM DATA ELEM DELETE FAILED'.            ELP010  
00135       05  NO-HEADER-REC           PIC X(42)  VALUE                ELP010  
00136       'NO HEADER RECORD IN FILE.  RUN TERMINATED.'.               ELP010  
00137       05  REC-LIST-NL-DE-REJECT   PIC X(57) VALUE                 ELP010  
00138       'RECORD LIST RECORD MISMATCH. DATA ELEMENT RECORD REJECTED'.ELP010  
00139       05  DE-NL-CV-REJECT         PIC X(53)  VALUE                ELP010  
00140       'DATA ELEMENT MISMATCH.    CODE VALUE RECORD REJECTED'.     ELP010  
00141       05  CODE-VALUE-IN-WRONG-REC PIC X(62)  VALUE                ELP010  
00142       'CODE VALUE IS IN WRONG SEQUENCE, DOES NOT MATCH CURRENT PREELP010  
00143 -     'FIX'.                                                      ELP010  
00144       05  INVALID-RECORD-TYPE     PIC X(48)  VALUE                ELP010  
00145       'INVALID RECORD TYPE ENCOUNTERED.  RUN TERMINATED'.         ELP010  
00146       05  DUP-REC-RECORD-LIST     PIC X(44)  VALUE                ELP010  
00147       'DUPLICATE RECORD ENCOUNTERED FOR RECORD LIST'.             ELP010  
00148       05  DUP-REC-DATA-ELEMENT    PIC X(45)  VALUE                ELP010  
00149       'DUPLICATE RECORD ENCOUNTERED FOR DATA ELEMENT'.            ELP010  
00150       05  DUP-REC-CODE-VALUE      PIC X(43)  VALUE                ELP010  
00151       'DUPLICATE RECORD ENCOUNTERED FOR CODE VALUE'.              ELP010  
00152                                                                   ELP010  
00153   01  PROGRAM-SWITCHES-HERE      PIC X(40)  VALUE                 ELP010  
00154       '**** PROGRAM SWITCHES START HERE ****   '.                 ELP010  
00155                                                                   ELP010  
00156   01  PROGRAM-SWITCHES.                                           ELP010  
00157       05  DONE-SW                PIC X      VALUE 'N'.            ELP010  
00158           88  D-ONE              VALUE 'Y'.                       ELP010  
00159       05  ERROR-SW               PIC X      VALUE 'N'.            ELP010  
00160       05  PROCESS-PREFIX         PIC X(08)  VALUE SPACES.         ELP010  
00161       05  PROCESS-NAME           PIC X(75)  VALUE SPACES.         ELP010  
00162                                                                   ELP010  
00163   01  PROGRAM-COUNTERS-HERE      PIC X(40)  VALUE                 ELP010  
00164       '**** PROGRAM COUNTERS STARTS HERE ****  '.                 ELP010  
00165                                                                   ELP010  
00166   01  PROGRAM-COUNTERS.                                           ELP010  
00167       05  RL-RUN-TOTAL-READ      PIC S9(7)   COMP-3  VALUE ZEROS. ELP010  
00168       05  DE-RUN-TOTAL-READ      PIC S9(7)   COMP-3  VALUE ZEROS. ELP010  
00169       05  CV-RUN-TOTAL-READ      PIC S9(7)   COMP-3  VALUE ZEROS. ELP010  
00170       05  RL-RUN-TOTAL-WRITTEN   PIC S9(7)   COMP-3  VALUE ZEROS. ELP010  
00171       05  DE-RUN-TOTAL-WRITTEN   PIC S9(7)   COMP-3  VALUE ZEROS. ELP010  
00172       05  CV-RUN-TOTAL-WRITTEN   PIC S9(7)   COMP-3  VALUE ZEROS. ELP010  
00173       05  DESC-CTR               PIC S9(2)   COMP-3  VALUE ZEROS. ELP010  
00174       05  LINE-CTR               PIC S9(3)   COMP-3  VALUE +66.   ELP010  
00175       05  PAGE-CTR               PIC S9(4)   COMP-3  VALUE ZEROS. ELP010  
00176       05  DE-COUNTER             PIC S9(3)V99 COMP-3 VALUE ZEROS. ELP010  
00177                                                                   ELP010  
00178 /                                                                 ELP010  
00179   01  IO-INFO.                                                    ELP010  
00180       COPY ELBHIOPM.                                              ELP010  
00181 /                                                                 ELP010  
00182   01  PRINT-WORK-AREAS-HERE      PIC X(40)  VALUE                 ELP010  
00183       '**** PRINT WORK AREAS START HERE  ****  '.                 ELP010  
00184                                                                   ELP010  
00185   01  HEADING1.                                                   ELP010  
00186       05  S-HEADING1-CTL         PIC X      VALUE '1'.            ELP010  
00187       05  FILLER                 PIC X(12)  VALUE 'REPORT ID.'.   ELP010  
00188       05  FILLER                 PIC X(07)  VALUE ' ELP010'.      ELP010  
00189       05  FILLER                 PIC X(33)  VALUE SPACES.         ELP010  
00190       05  FILLER                 PIC X(26)  VALUE                 ELP010  
00191           'ELP VSAM LOAD ERROR REPORT'.                           ELP010  
00192       05  FILLER                 PIC X(36)  VALUE SPACES.         ELP010  
00193       05  FILLER                 PIC X(05)  VALUE 'PAGE'.         ELP010  
00194       05  S-HEADING1-PAGE        PIC ZZZ9.                        ELP010  
00195                                                                   ELP010  
00196   01  HEADING2.                                                   ELP010  
00197       05  S-HEADING2-CTL         PIC X      VALUE SPACE.          ELP010  
00198       05  FILLER                 PIC X(09)  VALUE 'RUN DATE'.     ELP010  
00199       05  S-HEADING2-RUN-DATE    PIC X(08)  VALUE SPACE.          ELP010  
00200       05  FILLER                 PIC X(115) VALUE SPACES.         ELP010  
00201                                                                   ELP010  
00202   01  HEADING3.                                                   ELP010  
00203       05  FILLER                 PIC X(01)  VALUE '0'.            ELP010  
00204       05  FILLER                 PIC X(07)  VALUE                 ELP010  
00205           ' PREFIX'.                                              ELP010  
00206       05  FILLER                 PIC X(09)  VALUE SPACES.         ELP010  
00207       05  FILLER                 PIC X(17)  VALUE                 ELP010  
00208           'FILE/ELEMENT NAME'.                                    ELP010  
00209       05  FILLER                 PIC X(58)  VALUE SPACES.         ELP010  
00210       05  FILLER                 PIC X(10)  VALUE                 ELP010  
00211           'CODE VALUE'.                                           ELP010  
00212       05  FILLER                 PIC X(07)  VALUE SPACES.         ELP010  
00213       05  FILLER                 PIC X(12)  VALUE                 ELP010  
00214           ' RECORD TYPE'.                                         ELP010  
00215       05  FILLER                 PIC X(12)  VALUE SPACES.         ELP010  
00216                                                                   ELP010  
00217   01  PRT-DTL.                                                    ELP010  
00218       05  PD-CTL                 PIC X.                           ELP010  
00219       05  PD-PREFIX              PIC X(08).                       ELP010  
00220       05  FILLER                 PIC X(05)  VALUE SPACES.         ELP010  
00221       05  PD-NAME                PIC X(75).                       ELP010  
00222       05  FILLER                 PIC X(05)  VALUE SPACES.         ELP010  
00223       05  PD-CODE-VALUE          PIC X(10).                       ELP010  
00224       05  FILLER                 PIC X(05)  VALUE SPACES.         ELP010  
00225       05  PD-RECORD-TYPE         PIC X(12).                       ELP010  
00226       05  FILLER                 PIC X(12)  VALUE SPACES.         ELP010  
00227                                                                   ELP010  
00228   01  PRT-ERROR-LINE.                                             ELP010  
00229       05  PRT-ERR-CTL            PIC X.                           ELP010  
00230       05  FILLER                 PIC X(14)  VALUE                 ELP010  
00231           'ERROR MSG===> '.                                       ELP010  
00232       05  PRT-ERROR-MESSAGE      PIC X(100).                      ELP010  
00233       05  PRT-STATUS-FIELDS.                                      ELP010  
00234          10  PRT-STATUS-EQUAL    PIC X(10)  VALUE ' STATUS = '.   ELP010  
00235          10  PRT-ERR-STATUS      PIC X(03).                       ELP010  
00236          10  PRT-ELBHIO-RETURN   PIC BXX VALUE SPACES.            ELP010  
00237          10  PRT-ELBHIO-REQUEST  PIC BX  VALUE SPACES.            ELP010  
00238                                                                   ELP010  
00239   01  TOTAL-LINE1.                                                ELP010  
00240       05  FILLER                        PIC X.                    ELP010  
00241       05  FILLER                        PIC X(26)  VALUE          ELP010  
00242           'TOTAL # REC LIST READ   - '.                           ELP010  
00243       05  PRT-REC-LIST-READ-CTR         PIC Z,ZZZ,ZZ9.            ELP010  
00244       05  FILLER                        PIC X(32)  VALUE          ELP010  
00245           '  TOTAL # DATA ELEMENT READ   - '.                     ELP010  
00246       05  PRT-DATA-ELEMENT-READ-CTR     PIC Z,ZZZ,ZZ9.            ELP010  
00247       05  FILLER                        PIC X(30)  VALUE          ELP010  
00248           '  TOTAL # CODE VALUE READ   - '.                       ELP010  
00249       05  PRT-CODE-VALUE-READ-CTR       PIC Z,ZZZ,ZZ9.            ELP010  
00250                                                                   ELP010  
00251   01  TOTAL-LINE2.                                                ELP010  
00252       05  FILLER                        PIC X.                    ELP010  
00253       05  FILLER                        PIC X(26)  VALUE          ELP010  
00254           'TOTAL # REC LIST LOADED - '.                           ELP010  
00255       05  PRT-REC-LIST-LOAD-CTR         PIC Z,ZZZ,ZZ9.            ELP010  
00256       05  FILLER                        PIC X(32)  VALUE          ELP010  
00257           '  TOTAL # DATA ELEMENT LOADED - '.                     ELP010  
00258       05  PRT-DATA-ELEMENT-LOAD-CTR     PIC Z,ZZZ,ZZ9.            ELP010  
00259       05  FILLER                        PIC X(30)  VALUE          ELP010  
00260           '  TOTAL # CODE VALUE LOADED - '.                       ELP010  
00261       05  PRT-CODE-VALUE-LOAD-CTR       PIC Z,ZZZ,ZZ9.            ELP010  
00262 /                                                                 ELP010  
00263   PROCEDURE DIVISION.                                             ELP010  
00264   MAINLINE.                                                       ELP010  
00265                                                                   ELP010  
00266      PERFORM 0500-OPEN-FILES                                      ELP010  
00267         THRU 0500-EXIT.                                           ELP010  
00268                                                                   ELP010  
00269      PERFORM 1000-INITIALIZE THRU 1000-EXIT.                      ELP010  
00270      PERFORM 5000-LOAD-FILES THRU 5000-EXIT                       ELP010  
00271           UNTIL D-ONE.                                            ELP010  
00272      PERFORM 9000-WINDUP THRU 9000-EXIT.                          ELP010  
00273                                                                   ELP010  
00274      PERFORM 9500-CLOSE-FILES                                     ELP010  
00275         THRU 9500-EXIT.                                           ELP010  
00276                                                                   ELP010  
00277      STOP RUN.                                                    ELP010  
00278                                                                   ELP010  
00279   MAINLINE-EXIT.  EXIT.                                           ELP010  
00280 /                                                                 ELP010  
00281 *************************************************************     ELP010  
00282 *    ONLY ONE OPEN IS ISSUED FOR THE VSAM FILES.  THE IO    *     ELP010  
00283 * MODULE 'ELBIOPGM' WILL OPEN ALL THE VSAM FILES NEEDED TO  *     ELP010  
00284 * COMPLETE PROCESSING.                                      *     ELP010  
00285 *************************************************************     ELP010  
00286  0500-OPEN-FILES.                                                 ELP010  
00287                                                                   ELP010  
00288      OPEN INPUT TEMP-WORK-FILE.                                   ELP010  
00289      OPEN OUTPUT PRINT-FILE.                                      ELP010  
00290                                                                   ELP010  
00291      MOVE 'RL'  TO  ELBHIO-FILE-ID.                               ELP010  
00292      MOVE 'O'   TO  ELBHIO-REQUEST-TYPE.                          ELP010  
00293                                                                   ELP010  
00294      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP010  
00295                                                                   ELP010  
00296      IF ELBHIO-GOOD-RETURN                                        ELP010  
00297          NEXT SENTENCE                                            ELP010  
00298      ELSE                                                         ELP010  
00299          CLOSE TEMP-WORK-FILE                                     ELP010  
00300                PRINT-FILE                                         ELP010  
00301          DISPLAY 'OPENS FAILED REQUEST CD = '                     ELP010  
00302                ELBHIO-REQUEST-TYPE                                ELP010  
00303          MOVE 1075 TO ABEND-CODE                                  ELP010  
00304          CALL 'TSGEND' USING ABEND-CODE.                          ELP010  
00305                                                                   ELP010  
00306  0500-EXIT.  EXIT.                                                ELP010  
00307 /                                                                 ELP010  
00308   1000-INITIALIZE.                                                ELP010  
00309      ACCEPT S-HEADING2-RUN-DATE FROM DATE.                        ELP010  
00310 *    MOVE CURRENT-DATE TO S-HEADING2-RUN-DATE.                    ELP010  
00311      PERFORM 2000-READ-TEMPFILE THRU 2000-EXIT.                   ELP010  
00312      IF D-ONE                                                     ELP010  
00313          CLOSE TEMP-WORK-FILE                                     ELP010  
00314                PRINT-FILE                                         ELP010  
00315          MOVE 1000 TO ABEND-CODE                                  ELP010  
00316          CALL 'TSGEND' USING ABEND-CODE.                          ELP010  
00317      IF HEADER-SORT-KEY = LOW-VALUES                              ELP010  
00318           NEXT SENTENCE                                           ELP010  
00319      ELSE                                                         ELP010  
00320        MOVE NO-HEADER-REC TO PRT-ERROR-MESSAGE                    ELP010  
00321        PERFORM 8000-PRINT-HEADINGS THRU 8000-EXIT                 ELP010  
00322        WRITE PRT-REC FROM PRT-ERROR-LINE AFTER ADVANCING 2 LINES  ELP010  
00323        CLOSE TEMP-WORK-FILE                                       ELP010  
00324              PRINT-FILE                                           ELP010  
00325        MOVE 1050 TO ABEND-CODE                                    ELP010  
00326        CALL 'TSGEND' USING ABEND-CODE.                            ELP010  
00327                                                                   ELP010  
00328      MOVE HEADER-TYPE-RUN TO RUN-TYPE.                            ELP010  
00329 **** NEED TO SET HOLD-DE-NBR-DESC-LINES TO A VALID NUMBER         ELP010  
00330 ****  TO CONTROL THE LENGTH OF THE MOVE SPACES.                   ELP010  
00331      MOVE 11              TO HOLD-DE-NBR-DESC-LINES.              ELP010  
00332      MOVE SPACES          TO HOLD-DE.                             ELP010  
00333 **** NEED TO SET HOLD-DE-NBR-DESC-LINES TO ZEROS NOW FOR          ELP010  
00334 ****  USE AS A FIRST TIME SW.                                     ELP010  
00335      MOVE ZEROS        TO HOLD-DE-NBR-DESC-LINES.                 ELP010  
00336      MOVE 'N'          TO ERROR-SW.                               ELP010  
00337      PERFORM 2000-READ-TEMPFILE THRU 2000-EXIT.                   ELP010  
00338      IF D-ONE                                                     ELP010  
00339         MOVE SPACE TO RUN-TYPE.                                   ELP010  
00340  1000-EXIT.   EXIT.                                               ELP010  
00341 /                                                                 ELP010  
00342  2000-READ-TEMPFILE.                                              ELP010  
00343      READ TEMP-WORK-FILE                                          ELP010  
00344           AT END                                                  ELP010  
00345              MOVE 'Y' TO DONE-SW.                                 ELP010  
00346  2000-EXIT.   EXIT.                                               ELP010  
00347 /                                                                 ELP010  
00348  5000-LOAD-FILES.                                                 ELP010  
00349      MOVE RECORD-TYPE-RL TO RECORD-TYPE-TEST.                     ELP010  
00350      IF RECORD-RUN                                                ELP010  
00351         IF RL-RECORD                                              ELP010  
00352             PERFORM 6000-DELETE-REC-STRUCT THRU 6000-EXIT.        ELP010  
00353      IF RL-RECORD                                                 ELP010  
00354          MOVE PREFIX-RL TO PROCESS-PREFIX                         ELP010  
00355          ADD 1 TO RL-RUN-TOTAL-READ                               ELP010  
00356          MOVE ZEROS TO DE-COUNTER                                 ELP010  
00357          PERFORM 7000-WRITE-RL-RECORD THRU 7000-EXIT              ELP010  
00358          GO TO 5000-NEXT-READ.                                    ELP010  
00359                                                                   ELP010  
00360      IF DE-RECORD                                                 ELP010  
00361          ADD 1 TO DE-RUN-TOTAL-READ                               ELP010  
00362          IF PREFIX-DE = PROCESS-PREFIX                            ELP010  
00363               MOVE NAME-DE TO PROCESS-NAME                        ELP010  
00364               PERFORM 7300-STORE-DE-RECORD THRU 7300-EXIT         ELP010  
00365               GO TO 5000-NEXT-READ                                ELP010  
00366          ELSE                                                     ELP010  
00367            MOVE SPACES TO PRT-STATUS-FIELDS                       ELP010  
00368            MOVE REC-LIST-NL-DE-REJECT TO PRT-ERROR-MESSAGE        ELP010  
00369            PERFORM 8500-PRINT-ERR-LINES THRU 8500-EXIT            ELP010  
00370            GO TO 5000-NEXT-READ.                                  ELP010  
00371                                                                   ELP010  
00372      IF CV-RECORD                                                 ELP010  
00373          ADD 1 TO CV-RUN-TOTAL-READ                               ELP010  
00374          IF PREFIX-CV = PROCESS-PREFIX                            ELP010  
00375               IF NAME-CV = PROCESS-NAME                           ELP010  
00376                    PERFORM 7500-WRITE-CV-RECORD THRU 7500-EXIT    ELP010  
00377                    GO TO 5000-NEXT-READ                           ELP010  
00378               ELSE                                                ELP010  
00379                MOVE SPACES TO PRT-STATUS-FIELDS                   ELP010  
00380                MOVE DE-NL-CV-REJECT TO PRT-ERROR-MESSAGE          ELP010  
00381                PERFORM 8500-PRINT-ERR-LINES THRU 8500-EXIT        ELP010  
00382                GO TO 5000-NEXT-READ                               ELP010  
00383          ELSE                                                     ELP010  
00384           MOVE SPACES TO PRT-STATUS-FIELDS                        ELP010  
00385           MOVE CODE-VALUE-IN-WRONG-REC TO PRT-ERROR-MESSAGE       ELP010  
00386           PERFORM 8500-PRINT-ERR-LINES THRU 8500-EXIT             ELP010  
00387           GO TO 5000-NEXT-READ.                                   ELP010  
00388      MOVE SPACES TO PRT-STATUS-FIELDS.                            ELP010  
00389      MOVE INVALID-RECORD-TYPE TO PRT-ERROR-MESSAGE.               ELP010  
00390      PERFORM 8500-PRINT-ERR-LINES THRU 8500-EXIT.                 ELP010  
00391                                                                   ELP010  
00392      PERFORM 9500-CLOSE-FILES                                     ELP010  
00393         THRU 9500-EXIT.                                           ELP010  
00394                                                                   ELP010  
00395      MOVE 4010 TO ABEND-CODE.                                     ELP010  
00396      DISPLAY '************************************'.              ELP010  
00397      DISPLAY '     ELP010 ERROR MESSAGE           '.              ELP010  
00398      DISPLAY ' INVALID RECORD TYPE ENCOUNTERED    '.              ELP010  
00399      DISPLAY ' RECORD TYPE = ' RECORD-TYPE-TEST.                  ELP010  
00400      DISPLAY '       RUN ABORTED                  '.              ELP010  
00401      DISPLAY '************************************'.              ELP010  
00402      CALL 'TSGEND' USING ABEND-CODE.                              ELP010  
00403  5000-NEXT-READ.                                                  ELP010  
00404      PERFORM 2000-READ-TEMPFILE THRU 2000-EXIT.                   ELP010  
00405  5000-EXIT.  EXIT.                                                ELP010  
00406 /                                                                 ELP010  
00407  6000-DELETE-REC-STRUCT.                                          ELP010  
00408      MOVE 'RL'       TO  ELBHIO-FILE-ID.                          ELP010  
00409      MOVE 'R'        TO  ELBHIO-REQUEST-TYPE.                     ELP010  
00410      MOVE 12         TO  ELBHIO-RECORD-LENGTH.                    ELP010  
00411      MOVE PREFIX-RL  TO  ELBHIO-ELPRL-KEY.                        ELP010  
00412                                                                   ELP010  
00413      CALL 'ELBIOPGM'   USING IO-INFO.                             ELP010  
00414                                                                   ELP010  
00415      IF ELBHIO-GOOD-RETURN                                        ELP010  
00416          NEXT SENTENCE                                            ELP010  
00417      ELSE                                                         ELP010  
00418          GO TO 6000-EXIT.                                         ELP010  
00419                                                                   ELP010  
00420      PERFORM 6100-DELETE-CODE-VALUES THRU 6100-EXIT.              ELP010  
00421      PERFORM 6200-DELETE-DATA-ELEMENTS THRU 6200-EXIT.            ELP010  
00422                                                                   ELP010  
00423      MOVE 'RL'       TO  ELBHIO-FILE-ID.                          ELP010  
00424      MOVE 'D'        TO  ELBHIO-REQUEST-TYPE.                     ELP010  
00425      MOVE 12         TO  ELBHIO-RECORD-LENGTH.                    ELP010  
00426      MOVE PREFIX-RL  TO  ELBHIO-ELPRL-KEY.                        ELP010  
00427                                                                   ELP010  
00428      CALL 'ELBIOPGM'   USING IO-INFO.                             ELP010  
00429                                                                   ELP010  
00430      IF ELBHIO-GOOD-RETURN                                        ELP010  
00431          NEXT SENTENCE                                            ELP010  
00432      ELSE                                                         ELP010  
00433          MOVE REC-LIST-DELETE-FAIL TO PRT-ERROR-MESSAGE           ELP010  
00434          MOVE STATUS-EQUAL     TO PRT-STATUS-EQUAL                ELP010  
00435          MOVE ELBHIO-FEEDBACK  TO PRT-ERR-STATUS                  ELP010  
00436          PERFORM 8500-PRINT-ERR-LINES THRU 8500-EXIT              ELP010  
00437          CLOSE TEMP-WORK-FILE,                                    ELP010  
00438                PRINT-FILE                                         ELP010  
00439          MOVE 4020 TO ABEND-CODE                                  ELP010  
00440          CALL 'TSGEND' USING ABEND-CODE.                          ELP010  
00441  6000-EXIT.  EXIT.                                                ELP010  
00442 /                                                                 ELP010  
00443  6100-DELETE-CODE-VALUES.                                         ELP010  
00444      MOVE 'CV'        TO  ELBHIO-FILE-ID.                         ELP010  
00445      MOVE 'P'         TO  ELBHIO-REQUEST-TYPE.                    ELP010  
00446      MOVE 27          TO  ELBHIO-RECORD-LENGTH.                   ELP010  
00447      MOVE PREFIX-RL   TO  ELBHIO-CV-RECORD-PREFIX.                ELP010  
00448      MOVE ZEROS       TO  ELBHIO-CV-ELEMENT-NBR,                  ELP010  
00449                           ELBHIO-CV-CODE-DESC-SEQ.                ELP010  
00450      MOVE LOW-VALUES  TO  ELBHIO-CV-CODE-VALUE.                   ELP010  
00451                                                                   ELP010  
00452      CALL 'ELBIOPGM'   USING IO-INFO.                             ELP010  
00453                                                                   ELP010  
00454      IF ELBHIO-GOOD-RETURN                                        ELP010  
00455          NEXT SENTENCE                                            ELP010  
00456      ELSE                                                         ELP010  
00457          GO TO 6100-EXIT.                                         ELP010  
00458                                                                   ELP010  
00459  6100-READ.                                                       ELP010  
00460      MOVE 'G'  TO  ELBHIO-REQUEST-TYPE.                           ELP010  
00461                                                                   ELP010  
00462      CALL 'ELBIOPGM'   USING IO-INFO.                             ELP010  
00463                                                                   ELP010  
00464      IF ELBHIO-GOOD-RETURN                                        ELP010  
00465          NEXT SENTENCE                                            ELP010  
00466      ELSE                                                         ELP010  
00467          GO TO 6100-EXIT.                                         ELP010  
00468                                                                   ELP010  
00469      IF PREFIX-RL  NOT  =  ELBHIO-CV-RECORD-PREFIX                ELP010  
00470          GO TO 6100-EXIT.                                         ELP010  
00471                                                                   ELP010  
00472      MOVE 'D'  TO  ELBHIO-REQUEST-TYPE.                           ELP010  
00473                                                                   ELP010  
00474      CALL 'ELBIOPGM'   USING IO-INFO.                             ELP010  
00475                                                                   ELP010  
00476      IF ELBHIO-GOOD-RETURN                                        ELP010  
00477          NEXT SENTENCE                                            ELP010  
00478      ELSE                                                         ELP010  
00479          MOVE CODE-VAL-DELETE-FAIL TO PRT-ERROR-MESSAGE           ELP010  
00480          MOVE STATUS-EQUAL     TO PRT-STATUS-EQUAL                ELP010  
00481          MOVE ELBHIO-FEEDBACK  TO PRT-ERR-STATUS                  ELP010  
00482          PERFORM 8500-PRINT-ERR-LINES THRU 8500-EXIT              ELP010  
00483          CLOSE TEMP-WORK-FILE,                                    ELP010  
00484                PRINT-FILE                                         ELP010  
00485          MOVE 4030 TO ABEND-CODE                                  ELP010  
00486          CALL 'TSGEND' USING ABEND-CODE.                          ELP010  
00487                                                                   ELP010  
00488      GO TO 6100-READ.                                             ELP010  
00489  6100-EXIT.  EXIT.                                                ELP010  
00490 /                                                                 ELP010  
00491  6200-DELETE-DATA-ELEMENTS.                                       ELP010  
00492      MOVE 'DE'       TO  ELBHIO-FILE-ID.                          ELP010  
00493      MOVE 'P'        TO  ELBHIO-REQUEST-TYPE.                     ELP010  
00494      MOVE 15         TO  ELBHIO-RECORD-LENGTH.                    ELP010  
00495      MOVE PREFIX-RL  TO  ELBHIO-DE-RECORD-PREFIX.                 ELP010  
00496      MOVE ZEROS      TO  ELBHIO-DE-ELEMENT-NBR.                   ELP010  
00497                                                                   ELP010  
00498      CALL 'ELBIOPGM'   USING IO-INFO.                             ELP010  
00499                                                                   ELP010  
00500      IF ELBHIO-GOOD-RETURN                                        ELP010  
00501          NEXT SENTENCE                                            ELP010  
00502      ELSE                                                         ELP010  
00503          GO TO 6200-EXIT.                                         ELP010  
00504  6200-READ.                                                       ELP010  
00505      MOVE 'G'  TO  ELBHIO-REQUEST-TYPE.                           ELP010  
00506                                                                   ELP010  
00507      CALL 'ELBIOPGM'   USING IO-INFO.                             ELP010  
00508                                                                   ELP010  
00509      IF ELBHIO-GOOD-RETURN                                        ELP010  
00510          NEXT SENTENCE                                            ELP010  
00511      ELSE                                                         ELP010  
00512          GO TO 6200-EXIT.                                         ELP010  
00513                                                                   ELP010  
00514      IF PREFIX-RL  NOT  =  ELBHIO-DE-RECORD-PREFIX                ELP010  
00515          GO TO 6200-EXIT.                                         ELP010  
00516                                                                   ELP010  
00517      MOVE 'D'  TO  ELBHIO-REQUEST-TYPE.                           ELP010  
00518                                                                   ELP010  
00519      CALL 'ELBIOPGM'   USING IO-INFO.                             ELP010  
00520                                                                   ELP010  
00521      IF ELBHIO-GOOD-RETURN                                        ELP010  
00522          NEXT SENTENCE                                            ELP010  
00523      ELSE                                                         ELP010  
00524          MOVE DATA-ELEM-DELETE-FAIL TO PRT-ERROR-MESSAGE          ELP010  
00525          MOVE STATUS-EQUAL     TO PRT-STATUS-EQUAL                ELP010  
00526          MOVE ELBHIO-FEEDBACK  TO PRT-ERR-STATUS                  ELP010  
00527          PERFORM 8500-PRINT-ERR-LINES THRU 8500-EXIT              ELP010  
00528          CLOSE TEMP-WORK-FILE,                                    ELP010  
00529                PRINT-FILE                                         ELP010  
00530          MOVE 4030 TO ABEND-CODE                                  ELP010  
00531          CALL 'TSGEND' USING ABEND-CODE.                          ELP010  
00532                                                                   ELP010  
00533      GO TO 6200-READ.                                             ELP010  
00534  6200-EXIT.  EXIT.                                                ELP010  
00535 /                                                                 ELP010  
00536  7000-WRITE-RL-RECORD.                                            ELP010  
00537      MOVE PREFIX-RL           TO RL-RECORD-PREFIX.                ELP010  
00538      MOVE RECORD-NAME-RL      TO RL-RECORD-NAME.                  ELP010  
00539      MOVE RECORD-FILE-TYPE-RL TO RL-FILE.                         ELP010  
00540      MOVE SPACE               TO RL-DELETE-RECORD-FLAG,           ELP010  
00541      IF RECORD-RUN                                                ELP010  
00542           MOVE 'Y' TO RL-AUTO-REPRINT-FLAG                        ELP010  
00543      ELSE                                                         ELP010  
00544           MOVE SPACE TO RL-AUTO-REPRINT-FLAG.                     ELP010  
00545                                                                   ELP010  
00546      MOVE 'RL'          TO  ELBHIO-FILE-ID.                       ELP010  
00547      IF FILE-RUN                                                  ELP010  
00548      THEN                                                         ELP010  
00549          MOVE 'A'       TO  ELBHIO-REQUEST-TYPE                   ELP010  
00550      ELSE                                                         ELP010  
00551          MOVE 'I'       TO  ELBHIO-REQUEST-TYPE.                  ELP010  
00552      MOVE  65           TO  ELBHIO-RECORD-LENGTH.                 ELP010  
00553      MOVE RL-WORK-AREA  TO  ELBHIO-RECORD-AREA.                   ELP010  
00554                                                                   ELP010  
00555      CALL 'ELBIOPGM'   USING IO-INFO.                             ELP010  
00556                                                                   ELP010  
00557      IF ELBHIO-GOOD-RETURN                                        ELP010  
00558          NEXT SENTENCE                                            ELP010  
00559      ELSE                                                         ELP010  
00560          PERFORM 7900-SET-UP-ERROR-MSG THRU 7900-EXIT             ELP010  
00561          MOVE ELBHIO-FEEDBACK     TO  PRT-ERR-STATUS              ELP010  
00562          MOVE ELBHIO-RETURN-CODE  TO  PRT-ELBHIO-RETURN           ELP010  
00563          MOVE ELBHIO-REQUEST-TYPE TO  PRT-ELBHIO-REQUEST          ELP010  
00564          MOVE SPACES              TO  PROCESS-PREFIX              ELP010  
00565          PERFORM 8500-PRINT-ERR-LINES THRU 8500-EXIT              ELP010  
00566          GO TO 7000-EXIT.                                         ELP010  
00567                                                                   ELP010  
00568      ADD 1 TO RL-RUN-TOTAL-WRITTEN.                               ELP010  
00569  7000-EXIT.  EXIT.                                                ELP010  
00570 /                                                                 ELP010  
00571  7300-STORE-DE-RECORD.                                            ELP010  
00572      IF HOLD-DE-NBR-DESC-LINES = ZEROS                            ELP010  
00573          NEXT SENTENCE                                            ELP010  
00574      ELSE                                                         ELP010  
00575 **** NEED TO SET UP VARIABLE LENGTH TO THE PROPER COUNTER         ELP010  
00576 ****  PRIOR TO THE GROUP LEVEL MOVE                               ELP010  
00577        MOVE HOLD-DE-NBR-DESC-LINES  TO  DE-NBR-DESC-LINES         ELP010  
00578        MOVE HOLD-DE                 TO  DATA-ELEMENT              ELP010  
00579        MOVE CODE-VALUES-CT          TO  DE-CODE-VALUES-CT         ELP010  
00580        PERFORM 7350-WRITE-DATA-ELEMENT THRU 7350-EXIT.            ELP010  
00581      MOVE SPACES TO DATA-ELEMENT.                                 ELP010  
00582      ADD 1 TO DE-COUNTER.                                         ELP010  
00583      MOVE PREFIX-DE          TO DE-RECORD-PREFIX,                 ELP010  
00584                                 DE-RECORD-PREFIX-N.               ELP010  
00585      MOVE DE-COUNTER         TO DE-ELEMENT-NBR.                   ELP010  
00586      MOVE NAME-DE            TO DE-ELEMENT-NAME.                  ELP010  
00587      MOVE ELEMENT-FORMAT-DE  TO DE-ELEMENT-FORMAT.                ELP010  
00588      MOVE ELEMENT-LENGTH-DE  TO DE-ELEMENT-LENGTH.                ELP010  
00589      MOVE FORMAT-COMMENT-DE  TO DE-FORMAT-COMMENT.                ELP010  
00590      MOVE RECORD-POS-DE      TO DE-RECORD-POS.                    ELP010  
00591      MOVE STORED-LENGTH-DE   TO DE-STORED-LENGTH.                 ELP010  
00592      MOVE STORED-DECIMALS-DE TO DE-STORED-DECIMALS.               ELP010  
00593      MOVE STORED-FORMAT-DE   TO DE-STORED-FORMAT.                 ELP010  
00594      MOVE CODES-FLAG-DE      TO DE-CODES-FLAG.                    ELP010  
00595      MOVE COBOL-NAME-DE      TO DE-COBOL-NAME.                    ELP010  
00596      MOVE BAL-NAME-DE        TO DE-BAL-NAME.                      ELP010  
00597      MOVE NBR-DESC-LINES-DE  TO DE-NBR-DESC-LINES.                ELP010  
00598      MOVE ZEROS              TO DESC-CTR.                         ELP010  
00599      PERFORM 7400-LOAD-DESC-LINES THRU 7400-EXIT                  ELP010  
00600                        DE-NBR-DESC-LINES  TIMES.                  ELP010  
00601 **** NEED TO SET UP VARIABLE LENGTH TO THE PROPER COUNTER         ELP010  
00602 ****  PRIOR TO THE GROUP LEVEL MOVE                               ELP010  
00603      MOVE DE-NBR-DESC-LINES TO HOLD-DE-NBR-DESC-LINES.            ELP010  
00604      MOVE DATA-ELEMENT TO HOLD-DE.                                ELP010  
00605      MOVE ZEROS        TO CODE-VALUES-CT.                         ELP010  
00606  7300-EXIT.  EXIT.                                                ELP010  
00607                                                                   ELP010  
00608  7350-WRITE-DATA-ELEMENT.                                         ELP010  
00609      MOVE 'DE'          TO  ELBHIO-FILE-ID.                       ELP010  
00610      IF FILE-RUN                                                  ELP010  
00611      THEN                                                         ELP010  
00612          MOVE 'A'       TO  ELBHIO-REQUEST-TYPE                   ELP010  
00613      ELSE                                                         ELP010  
00614          MOVE 'I'       TO  ELBHIO-REQUEST-TYPE.                  ELP010  
00615      MOVE 1045          TO  ELBHIO-RECORD-LENGTH.                 ELP010  
00616      MOVE DE-WORK-AREA  TO  ELBHIO-RECORD-AREA.                   ELP010  
00617                                                                   ELP010  
00618      CALL 'ELBIOPGM'   USING IO-INFO.                             ELP010  
00619                                                                   ELP010  
00620      IF ELBHIO-GOOD-RETURN                                        ELP010  
00621          NEXT SENTENCE                                            ELP010  
00622      ELSE                                                         ELP010  
00623          PERFORM 7900-SET-UP-ERROR-MSG THRU 7900-EXIT             ELP010  
00624          MOVE ELBHIO-FEEDBACK      TO PRT-ERR-STATUS              ELP010  
00625          MOVE ELBHIO-RETURN-CODE   TO PRT-ELBHIO-RETURN           ELP010  
00626          MOVE ELBHIO-REQUEST-TYPE  TO PRT-ELBHIO-REQUEST          ELP010  
00627          MOVE SPACES               TO PROCESS-NAME                ELP010  
00628          PERFORM 8500-PRINT-ERR-LINES THRU 8500-EXIT              ELP010  
00629          IF DE-COUNTER > 0                                        ELP010  
00630             SUBTRACT 1 FROM DE-COUNTER                            ELP010  
00631             GO TO 7350-EXIT                                       ELP010  
00632          ELSE                                                     ELP010  
00633             GO TO 7350-EXIT.                                      ELP010  
00634                                                                   ELP010  
00635      ADD 1 TO DE-RUN-TOTAL-WRITTEN.                               ELP010  
00636  7350-EXIT.   EXIT.                                               ELP010  
00637 /                                                                 ELP010  
00638  7400-LOAD-DESC-LINES.                                            ELP010  
00639      ADD 1 TO DESC-CTR.                                           ELP010  
00640      IF DESC-CTR > MAX-DE-DESC                                    ELP010  
00641          GO TO 7400-EXIT.                                         ELP010  
00642      MOVE DESC-LINE-DE (DESC-CTR) TO DE-DESC-LINE (DESC-CTR).     ELP010  
00643  7400-EXIT.  EXIT.                                                ELP010  
00644 /                                                                 ELP010  
00645  7500-WRITE-CV-RECORD.                                            ELP010  
00646      MOVE SPACES                  TO ELEMENT-CODE-VALUE.          ELP010  
00647      MOVE PREFIX-CV               TO CV-RECORD-PREFIX.            ELP010  
00648      MOVE DE-COUNTER              TO CV-ELEMENT-NBR.              ELP010  
00649      MOVE CODE-VALUE-CV           TO CV-CODE-VALUE.               ELP010  
00650      MOVE CODE-DESC-SEQ-CV        TO CV-CODE-DESC-SEQ.            ELP010  
00651      MOVE CODE-NAME-CV            TO CV-CODE-NAME.                ELP010  
00652      MOVE NBR-VALUE-DESC-LINES-CV TO CV-NBR-VALUE-DESC-LINES.     ELP010  
00653      MOVE SPACE                   TO CV-DELETE-CODE-FLAG.         ELP010  
00654      MOVE ZEROS                   TO DESC-CTR.                    ELP010  
00655      PERFORM 7600-LOAD-CV-DESC THRU 7600-EXIT                     ELP010  
00656           CV-NBR-VALUE-DESC-LINES TIMES.                          ELP010  
00657      MOVE 'CV'          TO  ELBHIO-FILE-ID.                       ELP010  
00658      IF FILE-RUN                                                  ELP010  
00659      THEN                                                         ELP010  
00660          MOVE 'A'       TO  ELBHIO-REQUEST-TYPE                   ELP010  
00661      ELSE                                                         ELP010  
00662          MOVE 'I'       TO  ELBHIO-REQUEST-TYPE.                  ELP010  
00663      MOVE 1028          TO  ELBHIO-RECORD-LENGTH.                 ELP010  
00664      MOVE CV-WORK-AREA  TO  ELBHIO-RECORD-AREA.                   ELP010  
00665                                                                   ELP010  
00666      CALL 'ELBIOPGM'   USING IO-INFO.                             ELP010  
00667                                                                   ELP010  
00668      IF ELBHIO-GOOD-RETURN                                        ELP010  
00669          NEXT SENTENCE                                            ELP010  
00670      ELSE                                                         ELP010  
00671          PERFORM 7900-SET-UP-ERROR-MSG THRU 7900-EXIT             ELP010  
00672          MOVE ELBHIO-FEEDBACK  TO PRT-ERR-STATUS                  ELP010  
00673          MOVE ELBHIO-RETURN-CODE   TO PRT-ELBHIO-RETURN           ELP010  
00674          MOVE ELBHIO-REQUEST-TYPE  TO PRT-ELBHIO-REQUEST          ELP010  
00675          PERFORM 8500-PRINT-ERR-LINES THRU 8500-EXIT              ELP010  
00676          GO TO 7500-EXIT.                                         ELP010  
00677                                                                   ELP010  
00678      ADD 1 TO CV-RUN-TOTAL-WRITTEN,                               ELP010  
00679               CODE-VALUES-CT.                                     ELP010  
00680  7500-EXIT.  EXIT.                                                ELP010  
00681 /                                                                 ELP010  
00682  7600-LOAD-CV-DESC.                                               ELP010  
00683      ADD 1 TO DESC-CTR.                                           ELP010  
00684      IF DESC-CTR > MAX-CV-DESC                                    ELP010  
00685          GO TO 7600-EXIT.                                         ELP010  
00686      MOVE VALUE-DESC-LINE-CV (DESC-CTR) TO                        ELP010  
00687                CV-VALUE-DESC-LINE (DESC-CTR).                     ELP010  
00688  7600-EXIT.                                                       ELP010  
00689 /                                                                 ELP010  
00690  7900-SET-UP-ERROR-MSG.                                           ELP010  
00691      MOVE SPACES TO TEST-ERR-STATUS,                              ELP010  
00692                     PRT-STATUS-FIELDS.                            ELP010  
00693      MOVE STATUS-EQUAL TO PRT-STATUS-EQUAL.                       ELP010  
00694      MOVE ELBHIO-FEEDBACK  TO TEST-ERR-STATUS.                    ELP010  
00695                                                                   ELP010  
00696      IF CV-RECORD                                                 ELP010  
00697          IF DUP-REC-RL-CV                                         ELP010  
00698               MOVE DUP-REC-CODE-VALUE TO PRT-ERROR-MESSAGE        ELP010  
00699          ELSE                                                     ELP010  
00700            MOVE CODE-VALUE-NOT-LOADED TO PRT-ERROR-MESSAGE.       ELP010  
00701                                                                   ELP010  
00702      IF DE-RECORD                                                 ELP010  
00703          IF DUP-REC-DE                                            ELP010  
00704               MOVE DUP-REC-DATA-ELEMENT TO PRT-ERROR-MESSAGE      ELP010  
00705          ELSE                                                     ELP010  
00706            MOVE DATA-ELEM-NOT-LOADED TO PRT-ERROR-MESSAGE.        ELP010  
00707                                                                   ELP010  
00708      IF RL-RECORD                                                 ELP010  
00709          IF DUP-REC-RL-CV                                         ELP010  
00710               MOVE DUP-REC-RECORD-LIST TO PRT-ERROR-MESSAGE       ELP010  
00711          ELSE                                                     ELP010  
00712            MOVE REC-LIST-NOT-LOADED TO PRT-ERROR-MESSAGE.         ELP010  
00713                                                                   ELP010  
00714  7900-EXIT.   EXIT.                                               ELP010  
00715 /                                                                 ELP010  
00716  8000-PRINT-HEADINGS.                                             ELP010  
00717      ADD 1 TO PAGE-CTR.                                           ELP010  
00718      MOVE PAGE-CTR TO S-HEADING1-PAGE.                            ELP010  
00719      MOVE SPACES   TO PRT-REC.                                    ELP010  
00720      WRITE PRT-REC FROM HEADING1 AFTER ADVANCING TOP-OF-FORM.     ELP010  
00721      MOVE SPACES   TO PRT-REC.                                    ELP010  
00722      WRITE PRT-REC FROM HEADING2 AFTER ADVANCING 1 LINE.          ELP010  
00723      MOVE SPACES   TO PRT-REC.                                    ELP010  
00724      WRITE PRT-REC FROM HEADING3 AFTER ADVANCING 2 LINES.         ELP010  
00725      MOVE 7 TO LINE-CTR.                                          ELP010  
00726  8000-EXIT.  EXIT.                                                ELP010  
00727 /                                                                 ELP010  
00728  8500-PRINT-ERR-LINES.                                            ELP010  
00729      IF LINE-CTR > +60                                            ELP010  
00730          PERFORM 8000-PRINT-HEADINGS THRU 8000-EXIT.              ELP010  
00731      MOVE PREFIX-RL      TO PD-PREFIX.                            ELP010  
00732      IF RL-RECORD                                                 ELP010  
00733          MOVE RECORD-NAME-RL TO PD-NAME                           ELP010  
00734      ELSE                                                         ELP010  
00735        MOVE NAME-RL TO PD-NAME.                                   ELP010  
00736      MOVE CODE-VALUE-RL  TO PD-CODE-VALUE.                        ELP010  
00737      IF RL-RECORD                                                 ELP010  
00738        MOVE R-LIST    TO PD-RECORD-TYPE.                          ELP010  
00739      IF DE-RECORD                                                 ELP010  
00740        MOVE D-ELEMENT TO PD-RECORD-TYPE.                          ELP010  
00741      IF CV-RECORD                                                 ELP010  
00742        MOVE C-VALUE   TO PD-RECORD-TYPE.                          ELP010  
00743      MOVE SPACE TO PRT-REC.                                       ELP010  
00744      WRITE PRT-REC FROM PRT-DTL AFTER ADVANCING  2 LINES.         ELP010  
00745      MOVE SPACE TO PRT-REC.                                       ELP010  
00746      WRITE PRT-REC FROM PRT-ERROR-LINE AFTER ADVANCING  1 LINE.   ELP010  
00747      ADD +3 TO LINE-CTR.                                          ELP010  
00748      MOVE 'Y' TO ERROR-SW.                                        ELP010  
00749  8500-EXIT.  EXIT.                                                ELP010  
00750 /                                                                 ELP010  
00751  8900-PRINT-TOTALS.                                               ELP010  
00752      IF LINE-CTR > +50                                            ELP010  
00753           PERFORM 8000-PRINT-HEADINGS THRU 8000-EXIT.             ELP010  
00754      MOVE SPACE TO PRT-REC.                                       ELP010  
00755      MOVE ALL '*' TO PO-REST.                                     ELP010  
00756      WRITE PRT-REC AFTER ADVANCING 2 LINES.                       ELP010  
00757      MOVE RL-RUN-TOTAL-READ TO PRT-REC-LIST-READ-CTR.             ELP010  
00758      MOVE DE-RUN-TOTAL-READ TO PRT-DATA-ELEMENT-READ-CTR.         ELP010  
00759      MOVE CV-RUN-TOTAL-READ TO PRT-CODE-VALUE-READ-CTR.           ELP010  
00760      MOVE SPACE TO PRT-REC.                                       ELP010  
00761      WRITE PRT-REC FROM TOTAL-LINE1 AFTER ADVANCING 2 LINES.      ELP010  
00762                                                                   ELP010  
00763      MOVE RL-RUN-TOTAL-WRITTEN TO PRT-REC-LIST-LOAD-CTR.          ELP010  
00764      MOVE DE-RUN-TOTAL-WRITTEN TO PRT-DATA-ELEMENT-LOAD-CTR.      ELP010  
00765      MOVE CV-RUN-TOTAL-WRITTEN TO PRT-CODE-VALUE-LOAD-CTR.        ELP010  
00766      MOVE SPACE TO PRT-REC.                                       ELP010  
00767      WRITE PRT-REC FROM TOTAL-LINE2 AFTER ADVANCING 1 LINE.       ELP010  
00768      MOVE SPACE TO PRT-REC.                                       ELP010  
00769      MOVE ALL '*' TO PO-REST.                                     ELP010  
00770  8900-EXIT.  EXIT.                                                ELP010  
00771 /                                                                 ELP010  
00772  9000-WINDUP.                                                     ELP010  
00773 ****** WRITE OUT LAST DATA ELEMENT *******                        ELP010  
00774      IF HOLD-DE NOT = SPACES                                      ELP010  
00775        MOVE HOLD-DE TO DATA-ELEMENT                               ELP010  
00776        PERFORM 7350-WRITE-DATA-ELEMENT THRU 7350-EXIT.            ELP010  
00777      IF ERROR-SW = 'N'                                            ELP010  
00778          PERFORM 8000-PRINT-HEADINGS THRU 8000-EXIT               ELP010  
00779          MOVE '  N O  E R R O R S   D E T E C T E D  ' TO         ELP010  
00780             PO-REST                                               ELP010  
00781          WRITE PRT-REC AFTER ADVANCING 3 LINES.                   ELP010  
00782      PERFORM 8900-PRINT-TOTALS THRU 8900-EXIT.                    ELP010  
00783      IF RECORD-RUN                                                ELP010  
00784          MOVE '04' TO RETURN-CODE                                 ELP010  
00785      ELSE                                                         ELP010  
00786        MOVE '00' TO RETURN-CODE.                                  ELP010  
00787                                                                   ELP010  
00788  9000-EXIT. EXIT.                                                 ELP010  
00789 /                                                                 ELP010  
00790 *************************************************************     ELP010  
00791 *    ONLY ONE CLOSE IS ISSUED FOR THE VSAM FILES.  THEY WILL*     ELP010  
00792 * ALL BE CLOSED BY THE IO INTERFACE 'ELBIOPGM'.             *     ELP010  
00793 *************************************************************     ELP010  
00794  9500-CLOSE-FILES.                                                ELP010  
00795                                                                   ELP010  
00796      CLOSE TEMP-WORK-FILE,                                        ELP010  
00797            PRINT-FILE.                                            ELP010  
00798                                                                   ELP010  
00799      MOVE 'RL'  TO  ELBHIO-FILE-ID.                               ELP010  
00800      MOVE 'C'   TO  ELBHIO-REQUEST-TYPE.                          ELP010  
00801                                                                   ELP010  
00802      CALL 'ELBIOPGM'   USING IO-INFO.                             ELP010  
00803                                                                   ELP010  
00804      IF ELBHIO-GOOD-RETURN                                        ELP010  
00805          NEXT SENTENCE                                            ELP010  
00806      ELSE                                                         ELP010  
00807          MOVE 4004 TO ABEND-CODE                                  ELP010  
00808          DISPLAY '*******************************'                ELP010  
00809          DISPLAY ' CLOSE CODE VALUE FAILED '                      ELP010  
00810          DISPLAY ' REQUEST CODE IS ' ELBHIO-REQUEST-TYPE          ELP010  
00811          DISPLAY '*******************************'                ELP010  
00812          CALL 'TSGEND'  USING ABEND-CODE.                         ELP010  
00813                                                                   ELP010  
00814  9500-EXIT.  EXIT.                                                ELP010  
