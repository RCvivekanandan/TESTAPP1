00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELP011  
00003  PROGRAM-ID.         ELP011.                                         LV001
00004                                                                   ELP011  
00005  AUTHOR.             EDWARD G LISS.                               ELP011  
00006                                                                   ELP011  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELP011  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELP011  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELP011  
00010                      233 N. MICHIGAN AVE                          ELP011  
00011                      CHICAGO, ILLINOIS 60601                      ELP011  
00012                                                                   ELP011  
00013  DATE-WRITTEN.       27-NOV-1989.                                 ELP011  
00014                      ORIGINAL WRITTEN 18-OCT-1985 BY CAT LADY.    ELP011  
00015                                                                   ELP011  
00016  DATE-COMPILED.                                                   ELP011  
00017                                                                   ELP011  
00018  SECURITY.           COPYRIGHT 1988,                              ELP011  
00019                      HEALTH CARE SERVICE CORPORATION              ELP011  
00020                                                                   ELP011  
00021 ***************************************************************** ELP011  
00022 *  THIS PROGRAM PRINTS THE INDEX TO THE CODES MANUAL FOR        * ELP011  
00023 *  THE ENGLISH LANGUAGE SUPPORT PROTOTYPE SYSTEM.               * ELP011  
00024 ***************************************************************** ELP011  
00025      TITLE 'PRINT CODES MANUAL INDEX'.                            ELP011  
00026 ***************************************************************** ELP011  
00027 *                                                               * ELP011  
00028 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * ELP011  
00029 *    *-*         U P D A T E   H I S T O R Y         *-*        * ELP011  
00030 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * ELP011  
00031 *                                                               * ELP011  
00032 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* ELP011  
00033 *                                                               * ELP011  
00034 *  01.00     11/27/89  EGL  REWROTE THIS PROGRAM SINCE THE      * ELP011  
00035 *                           ORIGINAL WAS DIFFICULT TO           * ELP011  
00036 *                           MAINTAIN AS WELL AS TO ADD THE      * ELP011  
00037 *                           AFP CAPABILITIES.                   * ELP011  
00038 *                                                               * ELP011  
00039 ***************************************************************** ELP011  
00040                                                                   ELP011  
00041  ENVIRONMENT DIVISION.                                            ELP011  
00042  CONFIGURATION SECTION.                                           ELP011  
00043  SOURCE-COMPUTER.  IBM-370.                                       ELP011  
00044  OBJECT-COMPUTER.  IBM-370.                                       ELP011  
00045                                                                   ELP011  
00046  SPECIAL-NAMES.                                                   ELP011  
00047         C01 IS TOP-OF-FORM.                                       ELP011  
00048  INPUT-OUTPUT SECTION.                                            ELP011  
00049  FILE-CONTROL.                                                    ELP011  
00050                                                                   ELP011  
00051      SELECT PRINT-FILE                                            ELP011  
00052          ASSIGN TO LSTINDX.                                       ELP011  
00053                                                                   ELP011  
00054      SELECT ENTRY-FILE                                            ELP011  
00055          ASSIGN TO CODINDX.                                       ELP011  
00056 /                                                                 ELP011  
00057  DATA DIVISION.                                                   ELP011  
00058  FILE SECTION.                                                    ELP011  
00059                                                                   ELP011  
00060  FD  ENTRY-FILE                                                   ELP011  
00061      BLOCK CONTAINS  0  RECORDS                                   ELP011  
00062      LABEL RECORDS ARE STANDARD                                   ELP011  
00063      RECORDING MODE IS F                                          ELP011  
00064      DATA RECORD IS ENTRY-REC.                                    ELP011  
00065  01  ENTRY-REC.                                                   ELP011  
00066  COPY ELPIXC.                                                     ELP011  
00067 /                                                                 ELP011  
00068  FD  PRINT-FILE                                                   ELP011  
00069      BLOCK CONTAINS  0  RECORDS                                   ELP011  
00070      LABEL RECORDS ARE STANDARD                                   ELP011  
00071      RECORD CONTAINS 133 CHARACTERS                               ELP011  
00072      RECORDING MODE IS F                                          ELP011  
00073      DATA RECORD IS PRT-REC.                                      ELP011  
00074  01  PRT-REC.                                                     ELP011  
00075      05  FILLER                 PIC X.                            ELP011  
00076      05  PT-AFP                 PIC X.                            ELP011  
00077      05  PT-DATA                PIC X(131).                       ELP011  
00078 /                                                                 ELP011  
00079  WORKING-STORAGE SECTION.                                         ELP011  
00080  77  FILLER                         PIC X(30)                     ELP011  
00081        VALUE 'ELP011 WORKING STORAGE BEGINS'.                     ELP011  
00082                                                                   ELP011  
00083  01  WS-MISC-STUFF.                                               ELP011  
00084      05  ENTRY-EOF-INDICATOR        PIC X     VALUE 'N'.          ELP011  
00085          88 ENTRY-NOT-EOF                     VALUE 'N'.          ELP011  
00086          88 ENTRY-EOF                         VALUE 'Y'.          ELP011  
00087      05  CURR-BREAK                 PIC X(20) VALUE SPACES.       ELP011  
00088      05  LAST-BREAK                 PIC X(20) VALUE SPACES.       ELP011  
00089      05  WS-LINE-NO                 PIC S9999 COMP VALUE +0.      ELP011  
00090      05  WS-PAGE-NO                 PIC S9999 COMP VALUE +0.      ELP011  
00091      05  MAX-LINES                  PIC S9999 COMP VALUE +54.     ELP011  
00092                                                                   ELP011  
00093  01  WS-YYMMDD                      PIC 9(6)  VALUE ZEROES.       ELP011  
00094  01  WS-YYMMDD-RDF REDEFINES WS-YYMMDD.                           ELP011  
00095      05  WS-YY                      PIC 99.                       ELP011  
00096      05  WS-MM                      PIC 99.                       ELP011  
00097      05  WS-DD                      PIC 99.                       ELP011  
00098                                                                   ELP011  
00099  01  WS-AFP-DATA.                                                 ELP011  
00100      05  WS-NORMAL-FONT             PIC X VALUE SPACE.            ELP011  
00101 /                                                                 ELP011  
00102  01  COVER1.                                                      ELP011  
00103      05  FILLER        PIC X(20) VALUE '                    '.    ELP011  
00104      05  FILLER        PIC X(20) VALUE '           ENGLISH L'.    ELP011  
00105      05  FILLER        PIC X(20) VALUE 'ANGUAGE SUPPORT     '.    ELP011  
00106      05  FILLER        PIC X(20) VALUE '                    '.    ELP011  
00107      05  FILLER        PIC X(5)  VALUE '     '.                   ELP011  
00108  01  COVER2.                                                      ELP011  
00109      05  FILLER        PIC X(20) VALUE '                    '.    ELP011  
00110      05  FILLER        PIC X(20) VALUE '             INDEX T'.    ELP011  
00111      05  FILLER        PIC X(20) VALUE 'O CODES MANUAL      '.    ELP011  
00112      05  FILLER        PIC X(20) VALUE '                    '.    ELP011  
00113      05  FILLER        PIC X(5)  VALUE '     '.                   ELP011  
00114  01  COVER3.                                                      ELP011  
00115      05  FILLER        PIC X(20) VALUE '                    '.    ELP011  
00116      05  FILLER        PIC X(20) VALUE '       HEALTH CARE S'.    ELP011  
00117      05  FILLER        PIC X(20) VALUE 'ERVICE CORPORATION  '.    ELP011  
00118      05  FILLER        PIC X(20) VALUE '                    '.    ELP011  
00119      05  FILLER        PIC X(5)  VALUE '     '.                   ELP011  
00120  01  COVER4.                                                      ELP011  
00121      05  FILLER        PIC X(39) VALUE SPACES.                    ELP011  
00122      05  COVER4-DATE.                                             ELP011  
00123          10  COVER4-MM PIC 99    VALUE ZEROES.                    ELP011  
00124          10  FILLER    PIC X     VALUE '/'.                       ELP011  
00125          10  COVER4-DD PIC 99    VALUE ZEROES.                    ELP011  
00126          10  FILLER    PIC X     VALUE '/'.                       ELP011  
00127          10  COVER4-YY PIC 99    VALUE ZEROES.                    ELP011  
00128      05  FILLER        PIC X(38) VALUE SPACES.                    ELP011  
00129  01  HDR1.                                                        ELP011  
00130      05  FILLER        PIC X(20) VALUE 'ENGLISH LANGUAGE SUP'.    ELP011  
00131      05  FILLER        PIC X(20) VALUE 'PORT SYSTEM - INDEX '.    ELP011  
00132      05  FILLER        PIC X(20) VALUE 'TO CODES MANUAL     '.    ELP011  
00133      05  FILLER        PIC X(5)  VALUE '     '.                   ELP011  
00134      05  HDR1-DATE.                                               ELP011  
00135          10  HDR1-MM   PIC 99    VALUE ZEROES.                    ELP011  
00136          10  FILLER    PIC X     VALUE '/'.                       ELP011  
00137          10  HDR1-DD   PIC 99    VALUE ZEROES.                    ELP011  
00138          10  FILLER    PIC X     VALUE '/'.                       ELP011  
00139          10  HDR1-YY   PIC 99    VALUE ZEROES.                    ELP011  
00140      05  FILLER        PIC X(8)  VALUE '   PAGE '.                ELP011  
00141      05  HDR1-PG-NO    PIC ZZZ9  VALUE ZEROES.                    ELP011  
00142  01  HDR2.                                                        ELP011  
00143      05  FILLER        PIC X(17) VALUE 'DATA ELEMENT NAME'.       ELP011  
00144      05  FILLER        PIC X(53) VALUE SPACES.                    ELP011  
00145      05  FILLER        PIC X(15) VALUE ' RECORD/ELEMENT'.         ELP011  
00146  01  DET1.                                                        ELP011  
00147      05  DET1-NAME     PIC X(69) VALUE SPACES.                    ELP011  
00148      05  FILLER        PIC X     VALUE SPACES.                    ELP011  
00149      05  DET1-PREFIX   PIC X(8)  VALUE SPACES.                    ELP011  
00150      05  FILLER        PIC X     VALUE '-'.                       ELP011  
00151      05  DET1-ELEMENT-NO                                          ELP011  
00152                        PIC 999.99 VALUE ZEROES.                   ELP011  
00153  01  DET1A.                                                       ELP011  
00154      05  FILLER        PIC X(69) VALUE SPACES.                    ELP011  
00155      05  DET1A-LONG    PIC X(5)  VALUE SPACES.                    ELP011  
00156          88  DET1A-SHORT         VALUE SPACES.                    ELP011  
00157 /                                                                 ELP011  
00158  PROCEDURE DIVISION.                                              ELP011  
00159                                                                   ELP011  
00160  0000-MAINLINE.                                                   ELP011  
00161                                                                   ELP011  
00162      OPEN INPUT ENTRY-FILE                                        ELP011  
00163           OUTPUT PRINT-FILE.                                      ELP011  
00164                                                                   ELP011  
00165      PERFORM 1000-PRINT-COVER-SHEET.                              ELP011  
00166                                                                   ELP011  
00167      PERFORM 4000-READ-ENTRY.                                     ELP011  
00168      MOVE CURR-BREAK TO LAST-BREAK.                               ELP011  
00169      PERFORM 3000-PAGE-HEADER.                                    ELP011  
00170      PERFORM 2000-PRINT-REPORT                                    ELP011  
00171          UNTIL ENTRY-EOF.                                         ELP011  
00172                                                                   ELP011  
00173      CLOSE ENTRY-FILE PRINT-FILE.                                 ELP011  
00174                                                                   ELP011  
00175      MOVE ZERO TO RETURN-CODE.                                    ELP011  
00176      GOBACK.                                                      ELP011  
00177 /                                                                 ELP011  
00178  1000-PRINT-COVER-SHEET.                                          ELP011  
00179                                                                   ELP011  
00180      MOVE SPACES TO PRT-REC.                                      ELP011  
00181      WRITE PRT-REC AFTER ADVANCING TOP-OF-FORM.                   ELP011  
00182      MOVE SPACES TO PRT-REC.                                      ELP011  
00183      WRITE PRT-REC AFTER ADVANCING 6 LINES.                       ELP011  
00184                                                                   ELP011  
00185      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP011  
00186      MOVE COVER1 TO PT-DATA.                                      ELP011  
00187      WRITE PRT-REC AFTER ADVANCING 3 LINES.                       ELP011  
00188                                                                   ELP011  
00189      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP011  
00190      MOVE COVER2 TO PT-DATA.                                      ELP011  
00191      WRITE PRT-REC AFTER ADVANCING 3 LINES.                       ELP011  
00192                                                                   ELP011  
00193      MOVE SPACES TO PRT-REC.                                      ELP011  
00194      WRITE PRT-REC AFTER ADVANCING 12 LINES.                      ELP011  
00195                                                                   ELP011  
00196      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP011  
00197      MOVE COVER3 TO PT-DATA.                                      ELP011  
00198      WRITE PRT-REC AFTER ADVANCING 3 LINES.                       ELP011  
00199                                                                   ELP011  
00200      ACCEPT WS-YYMMDD FROM DATE.                                  ELP011  
00201      MOVE WS-MM TO COVER4-MM.                                     ELP011  
00202      MOVE WS-DD TO COVER4-DD.                                     ELP011  
00203      MOVE WS-YY TO COVER4-YY.                                     ELP011  
00204                                                                   ELP011  
00205      MOVE COVER4-DATE TO HDR1-DATE.                               ELP011  
00206      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP011  
00207      MOVE COVER4 TO PT-DATA.                                      ELP011  
00208      WRITE PRT-REC AFTER ADVANCING 3 LINES.                       ELP011  
00209 /                                                                 ELP011  
00210  2000-PRINT-REPORT.                                               ELP011  
00211                                                                   ELP011  
00212      IF WS-LINE-NO GREATER THAN MAX-LINES                         ELP011  
00213          PERFORM 3000-PAGE-HEADER                                 ELP011  
00214          MOVE CURR-BREAK TO LAST-BREAK                            ELP011  
00215      ELSE                                                         ELP011  
00216          IF CURR-BREAK NOT = LAST-BREAK                           ELP011  
00217              MOVE SPACES TO PRT-REC                               ELP011  
00218              WRITE PRT-REC AFTER ADVANCING 1 LINE                 ELP011  
00219              ADD +1 TO WS-LINE-NO                                 ELP011  
00220              MOVE CURR-BREAK TO LAST-BREAK                        ELP011  
00221          END-IF                                                   ELP011  
00222      END-IF.                                                      ELP011  
00223                                                                   ELP011  
00224      MOVE ENTRY-PREFIX TO DET1-PREFIX.                            ELP011  
00225      MOVE ENTRY-ELEMENT-NBR TO DET1-ELEMENT-NO.                   ELP011  
00226      MOVE ENTRY-TAB TO DET1A.                                     ELP011  
00227      IF DET1A-SHORT                                               ELP011  
00228          MOVE ENTRY-TAB TO DET1-NAME                              ELP011  
00229          MOVE WS-NORMAL-FONT TO PT-AFP                            ELP011  
00230          MOVE DET1 TO PT-DATA                                     ELP011  
00231          WRITE PRT-REC AFTER ADVANCING 1 LINE                     ELP011  
00232          ADD 1 TO WS-LINE-NO                                      ELP011  
00233      ELSE                                                         ELP011  
00234          MOVE WS-NORMAL-FONT TO PT-AFP                            ELP011  
00235          MOVE DET1A TO PT-DATA                                    ELP011  
00236          WRITE PRT-REC AFTER ADVANCING 1 LINE                     ELP011  
00237          MOVE SPACES TO DET1-NAME                                 ELP011  
00238          MOVE WS-NORMAL-FONT TO PT-AFP                            ELP011  
00239          MOVE DET1 TO PT-DATA                                     ELP011  
00240          WRITE PRT-REC AFTER ADVANCING 1 LINE                     ELP011  
00241          ADD 2 TO WS-LINE-NO                                      ELP011  
00242      END-IF.                                                      ELP011  
00243                                                                   ELP011  
00244      PERFORM 4000-READ-ENTRY.                                     ELP011  
00245 /                                                                 ELP011  
00246  3000-PAGE-HEADER.                                                ELP011  
00247                                                                   ELP011  
00248      MOVE SPACES TO PRT-REC.                                      ELP011  
00249      WRITE PRT-REC AFTER ADVANCING TOP-OF-FORM.                   ELP011  
00250                                                                   ELP011  
00251      ADD 1 TO WS-PAGE-NO.                                         ELP011  
00252      MOVE WS-PAGE-NO TO HDR1-PG-NO.                               ELP011  
00253      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP011  
00254      MOVE HDR1 TO PT-DATA.                                        ELP011  
00255      WRITE PRT-REC AFTER ADVANCING 3 LINES.                       ELP011  
00256                                                                   ELP011  
00257      MOVE WS-NORMAL-FONT TO PT-AFP.                               ELP011  
00258      MOVE HDR2 TO PT-DATA.                                        ELP011  
00259      WRITE PRT-REC AFTER ADVANCING 3 LINES.                       ELP011  
00260                                                                   ELP011  
00261      MOVE SPACES TO PRT-REC.                                      ELP011  
00262      WRITE PRT-REC AFTER ADVANCING 1 LINE.                        ELP011  
00263      MOVE +7 TO WS-LINE-NO.                                       ELP011  
00264 /*****************************************************************ELP011  
00265 ** THIS PROGRAM RECOGNIZES A CONTROL BREAK WHENEVER THE FIRST   **ELP011  
00266 ** WORD CHANGES IN THE ELEMENT NAME FIELD.  CERTAIN             **ELP011  
00267 ** TWO-WORD COMBINATIONS, SUCH AS 'BLUE CROSS', ARE TREATED     **ELP011  
00268 ** AS SINGLE WORDS IN THIS PARTICULAR CONTEXT.  ELP006 PLACES   **ELP011  
00269 ** A LOW-VALUE BETWEEN THE WORDS SO THE COMPLICATED TEST NEED   **ELP011  
00270 ** NOT BE REPEATED IN MULTIPLE PROGRAMS.                        **ELP011  
00271 ******************************************************************ELP011  
00272                                                                   ELP011  
00273  4000-READ-ENTRY.                                                 ELP011  
00274                                                                   ELP011  
00275      READ ENTRY-FILE                                              ELP011  
00276          AT END                                                   ELP011  
00277              SET ENTRY-EOF TO TRUE.                               ELP011  
00278      IF ENTRY-NOT-EOF                                             ELP011  
00279          MOVE SPACES TO CURR-BREAK                                ELP011  
00280          UNSTRING ENTRY-DATA DELIMITED BY SPACE                   ELP011  
00281              INTO CURR-BREAK                                      ELP011  
00282      END-IF.                                                      ELP011  
