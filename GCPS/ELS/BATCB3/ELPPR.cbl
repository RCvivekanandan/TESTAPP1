00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPPR   
00003  PROGRAM-ID.    ELPPR.                                               LV001
00004                                                                   ELPPR   
00005  AUTHOR.        ANNE KEFFER KING.                                 ELPPR   
00006                                                                   ELPPR   
00007  INSTALLATION.  HEALTH CARE SERVICE CORPORATION                   ELPPR   
00008                 A MUTUAL LEGAL RESERVE COMPANY                    ELPPR   
00009                 BLUE CROSS BLUE SHIELD OF ILLINOIS                ELPPR   
00010                 233 NORTH MICHIGAN AVENUE                         ELPPR   
00011                 CHICAGO, ILLINOIS 60601                           ELPPR   
00012                                                                   ELPPR   
00013  DATE-WRITTEN.  08-JUN-1989.                                      ELPPR   
00014                                                                   ELPPR   
00015  DATE-COMPILED.                                                   ELPPR   
00016                                                                   ELPPR   
00017  SECURITY.      COPYRIGHT 1988.                                   ELPPR   
00018                 HEALTH CARE SERVICE CORPORATION.                  ELPPR   
00019                                                                   ELPPR   
00020  ENVIRONMENT DIVISION.                                            ELPPR   
00021                                                                   ELPPR   
00022  CONFIGURATION SECTION.                                           ELPPR   
00023  SOURCE-COMPUTER.  IBM-3090.                                      ELPPR   
00024  OBJECT-COMPUTER.  IBM-3090.                                      ELPPR   
00025                                                                   ELPPR   
00026 **************************************************************    ELPPR   
00027 *              ELS ABEND PROCESSING                          *    ELPPR   
00028 *                                                            *    ELPPR   
00029 *  ELPPR - THIS SUBROUTINE PRINTS THE ABEND REPORT PAGES     *    ELPPR   
00030 *          NECESSARY FOR PRINTING THE ABEND REPORT.          *    ELPPR   
00031 *                                                            *    ELPPR   
00032 *************************************************************     ELPPR   
00033 *                                                            *    ELPPR   
00034 *                      MAINTENANCE HISTORY                   *    ELPPR   
00035 *                                                            *    ELPPR   
00036 *  MOD     DATE     BY  DRPT                ACTION           *    ELPPR   
00037 * ----- ----------- --- ----- -------------------------------*    ELPPR   
00038 * 01.00 08-JUN-1989 AKK       CREATED                        *    ELPPR   
00039 * 01.01 27-SEP-1989 EGL     - CHANGED TO USE STANDARD I/O    *    ELPPR   
00040 *                             RATHER THAN RIP.               *    ELPPR   
00041 *                           - ADDED OPTION TO SUPPRESS CICS  *    ELPPR   
00042 *                             IDENTITY FROM THE PAGE HEADING *    ELPPR   
00043 *                             IF THE REGION DATE AND TIME    *    ELPPR   
00044 *                             ZERO.                          *    ELPPR   
00045 * 01.02 03-JAN-1990 EGL     - SHORTENED HEADER LINE TO 132   *    ELPPR   
00046 *                             CHARACTERS TO AVOID TRUNCATION *    ELPPR   
00047 *                             OF LAST DIGIT OF PAGE NUMBER.  *    ELPPR   
00048 *                                                            *    ELPPR   
00049 * 01.03 03-NOV-1997 AKK     - ADDED SUPPORT FOR YEAR 2000    *    ELPPR   
00050 *                             AND TX MERGER.                 *    ELPPR   
00051 *                                                            *    ELPPR   
00052 **************************************************************    ELPPR   
00053                                                                   ELPPR   
00054  INPUT-OUTPUT SECTION.                                            ELPPR   
00055  FILE-CONTROL.                                                    ELPPR   
00056      SELECT PRINTER  ASSIGN TO UT-S-PRINTER.                      ELPPR   
00057 /                                                                 ELPPR   
00058 *                                                                 ELPPR   
00059  DATA DIVISION.                                                   ELPPR   
00060  FILE SECTION.                                                    ELPPR   
00061  FD  PRINTER                                                      ELPPR   
00062      BLOCK CONTAINS 0 RECORDS                                     ELPPR   
00063      RECORDING MODE IS F                                          ELPPR   
00064      LABEL RECORDS STANDARD.                                      ELPPR   
00065  01  PRT-PRINT-LINE        PIC X(133).                            ELPPR   
00066 /                                                                 ELPPR   
00067  WORKING-STORAGE SECTION.                                         ELPPR   
00068  01  FILLER                PIC X(16)   VALUE '*START OF ELPPR*'.  ELPPR   
00069 *                                                                 ELPPR   
00070  01  WS-WORK-AREAS.                                               ELPPR   
00071      05  WS-JUL-DATE       PIC 9(05).                             ELPPR   
00072      05  WS-GREG-DATE      PIC 9(06).                             ELPPR   
00073 *                                                                 ELPPR   
00074  01  WS-OPEN-CLOSE-SWITCH  PIC X       VALUE 'C'.                 ELPPR   
00075      88  FILE-OPENED                   VALUE 'O'.                 ELPPR   
00076      88  FILE-CLOSED                   VALUE 'C'.                 ELPPR   
00077 *                                                                 ELPPR   
00078      COPY HSCDATES.                                               ELPPR   
00079 /                                                                 ELPPR   
00080 * PAGE HEADER                                                     ELPPR   
00081  01  ABEND-REPORT-HEADING-LINES.                                  ELPPR   
00082      03  ABEND-REPORT-HDG-LINE1.                                  ELPPR   
00083          05  HGD1-LINE-SPACING     PIC X(01)  VALUE '1'.          ELPPR   
00084          05  HGD1-PGM-ID           PIC X(6)   VALUE ALL '?'.      ELPPR   
00085          05  FILLER                PIC X      VALUE SPACE.        ELPPR   
00086          05  HDG1-PRINT-DATE.                                     ELPPR   
00087              10  HDG1-PRINTED      PIC X(08)  VALUE 'PRINTED '.   ELPPR   
00088              10  HDG1-DATE         PIC 99/99/99.                  ELPPR   
00089          05  FILLER                PIC X(27)  VALUE SPACES.       ELPPR   
00090          05  FILLER                PIC X(37)  VALUE               ELPPR   
00091              'ENGLISH LANGUAGE SUPPORT ABEND REPORT'.             ELPPR   
00092          05  FILLER                PIC X(34)  VALUE SPACES.       ELPPR   
00093          05  FILLER                PIC X(05)  VALUE 'PAGE '.      ELPPR   
00094          05  HDG1-PAGE-NUMBER      PIC ZZZZ9.                     ELPPR   
00095          05  FILLER                PIC X(1)   VALUE SPACE.        ELPPR   
00096      03  ABEND-REPORT-HDG-LINE2.                                  ELPPR   
00097          05  HDG2-LINE-SPACING     PIC X(01)  VALUE '0'.          ELPPR   
00098          05  FILLER                PIC X(34)  VALUE SPACES.       ELPPR   
00099          05  FILLER                PIC X(11)  VALUE 'ABEND CODE '.ELPPR   
00100          05  HDG2-ABEND-CODE       PIC X(04)  VALUE SPACES.       ELPPR   
00101          05  HDG2-DATE-TIME-TERM.                                 ELPPR   
00102              10  FILLER            PIC X(13)  VALUE               ELPPR   
00103              ' OCCURRED ON '.                                     ELPPR   
00104              10 HDG2-DATE          PIC 99/99/99.                  ELPPR   
00105              10 HDG2-DATE-A REDEFINES HDG2-DATE                   ELPPR   
00106                                    PIC X(08).                     ELPPR   
00107              10 FILLER             PIC X(04)  VALUE ' AT '.       ELPPR   
00108              10 HDG2-TIME          PIC 99B99B99.                  ELPPR   
00109              10  HDG2-TIME-A REDEFINES HDG2-TIME.                 ELPPR   
00110                  15 HDG2-HOURS     PIC 9(02).                     ELPPR   
00111                  15 COLON-1        PIC X.                         ELPPR   
00112                  15 HDG2-MINUTES   PIC 9(02).                     ELPPR   
00113                  15 COLON-2        PIC X.                         ELPPR   
00114                  15 HDG2-SECONDS   PIC 9(02).                     ELPPR   
00115              10 FILLER             PIC X(13)  VALUE               ELPPR   
00116              ' ON TERMINAL '.                                     ELPPR   
00117              10 HDG2-TERMINAL-ID   PIC X(04)  VALUE SPACES.       ELPPR   
00118          05  FILLER                PIC X(33)  VALUE SPACES.       ELPPR   
00119      03 ABEND-REPORT-HDG-LINE3.                                   ELPPR   
00120          05  HDG3-LINE-SPACING     PIC X(01)  VALUE ' '.          ELPPR   
00121          05  FILLER                PIC X(49)  VALUE SPACES.       ELPPR   
00122          05  FILLER                PIC X(12)  VALUE               ELPPR   
00123              'CICS SYSTEM '.                                      ELPPR   
00124          05  HDG3-CICS-SYSTEM-ID   PIC X(04)  VALUE SPACES.       ELPPR   
00125          05  FILLER                PIC X(15)  VALUE               ELPPR   
00126              ' APPLID REGION '.                                   ELPPR   
00127          05  HDG3-APPLID-ID        PIC X(08)  VALUE SPACES.       ELPPR   
00128          05  FILLER                PIC X(44)  VALUE SPACES.       ELPPR   
00129 *                                                                 ELPPR   
00130  01  FILLER                PIC X(14)   VALUE '*END OF ELPPR*'.    ELPPR   
00131 /                                                                 ELPPR   
00132  LINKAGE SECTION.                                                 ELPPR   
00133      COPY ELSPRCBC.                                               ELPPR   
00134 /                                                                 ELPPR   
00135  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK.                ELPPR   
00136      PERFORM INTIALIZATION.                                       ELPPR   
00137      PERFORM PRINT-REPORT-LINE.                                   ELPPR   
00138      GOBACK.                                                      ELPPR   
00139 *                                                                 ELPPR   
00140  INTIALIZATION.                                                   ELPPR   
00141      SET PCB-OK TO TRUE.                                          ELPPR   
00142      IF PCB-EJECT                                                 ELPPR   
00143         ADD +1 TO PCB-PAGE-NUMBER                                 ELPPR   
00144         MOVE ZERO TO PCB-CURRENT-LINE.                            ELPPR   
00145 *                                                                 ELPPR   
00146  PRINT-REPORT-LINE.                                               ELPPR   
00147      IF PCB-PRINT-LINE AND FILE-OPENED                            ELPPR   
00148         PERFORM PRINT-A-LINE                                      ELPPR   
00149      ELSE                                                         ELPPR   
00150          IF PCB-OPEN-PRINTER AND FILE-CLOSED                      ELPPR   
00151             PERFORM OPEN-FILE                                     ELPPR   
00152          ELSE                                                     ELPPR   
00153             IF PCB-CLOSE-PRINTER AND FILE-OPENED                  ELPPR   
00154                PERFORM CLOSE-FILE                                 ELPPR   
00155             ELSE                                                  ELPPR   
00156                PERFORM HANDLE-INVALID-PARAMETER.                  ELPPR   
00157 *                                                                 ELPPR   
00158  PRINT-A-LINE.                                                    ELPPR   
00159      IF PCB-VALID-SPACING                                         ELPPR   
00160          CONTINUE                                                 ELPPR   
00161      ELSE                                                         ELPPR   
00162          SET PCB-SINGLE-SPACE TO TRUE.                            ELPPR   
00163      IF PCB-EJECT                                                 ELPPR   
00164          PERFORM PRINT-PAGE-HEADING.                              ELPPR   
00165      PERFORM DO-PRINT-LINE.                                       ELPPR   
00166 *                                                                 ELPPR   
00167  PRINT-PAGE-HEADING.                                              ELPPR   
00168      MOVE PCB-PRINT-DATE TO HDG1-DATE.                            ELPPR   
00169      MOVE PCB-PAGE-NUMBER TO HDG1-PAGE-NUMBER.                    ELPPR   
00170      MOVE ABEND-REPORT-HDG-LINE1 TO                               ELPPR   
00171            PRT-PRINT-LINE.                                        ELPPR   
00172      PERFORM WRITE-A-LINE.                                        ELPPR   
00173      MOVE 1 TO PCB-CURRENT-LINE.                                  ELPPR   
00174                                                                   ELPPR   
00175      IF PCB-ABEND-DATE NOT NUMERIC AND                            ELPPR   
00176                        PCB-ABEND-TIME NOT NUMERIC                 ELPPR   
00177          MOVE ZEROES TO PCB-ABEND-DATE                            ELPPR   
00178                         PCB-ABEND-TIME                            ELPPR   
00179      END-IF.                                                      ELPPR   
00180      IF PCB-ABEND-DATE = ZERO AND PCB-ABEND-TIME = ZERO           ELPPR   
00181          CONTINUE                                                 ELPPR   
00182      ELSE                                                         ELPPR   
00183          PERFORM PRINT-CONDITIONAL-HEADING.                       ELPPR   
00184                                                                   ELPPR   
00185      MOVE SPACES TO PRT-PRINT-LINE.                               ELPPR   
00186      PERFORM WRITE-A-LINE.                                        ELPPR   
00187      SET PCB-SINGLE-SPACE TO TRUE.                                ELPPR   
00188      ADD 1 TO PCB-CURRENT-LINE.                                   ELPPR   
00189 *                                                                 ELPPR   
00190  PRINT-CONDITIONAL-HEADING.                                       ELPPR   
00191      MOVE PCB-ABEND-CODE TO HDG2-ABEND-CODE.                      ELPPR   
00192      PERFORM CALL-TO-TSGGREG.                                     ELPPR   
00193      MOVE WS-GREG-DATE TO HDG2-DATE.                              ELPPR   
00194      MOVE PCB-ABEND-TIME TO HDG2-TIME.                            ELPPR   
00195      MOVE ':' TO COLON-1                                          ELPPR   
00196                  COLON-2.                                         ELPPR   
00197      MOVE PCB-TERMINAL-ID TO HDG2-TERMINAL-ID.                    ELPPR   
00198      MOVE ABEND-REPORT-HDG-LINE2 TO                               ELPPR   
00199            PRT-PRINT-LINE.                                        ELPPR   
00200      PERFORM WRITE-A-LINE.                                        ELPPR   
00201                                                                   ELPPR   
00202      MOVE PCB-CICS-SYSTEM-ID TO HDG3-CICS-SYSTEM-ID.              ELPPR   
00203      MOVE PCB-CICS-APPL-ID TO HDG3-APPLID-ID.                     ELPPR   
00204      MOVE ABEND-REPORT-HDG-LINE3 TO                               ELPPR   
00205            PRT-PRINT-LINE.                                        ELPPR   
00206      PERFORM WRITE-A-LINE.                                        ELPPR   
00207      ADD 3 TO PCB-CURRENT-LINE.                                   ELPPR   
00208 *                                                                 ELPPR   
00209  CALL-TO-TSGGREG.                                                 ELPPR   
00210      MOVE PCB-ABEND-DATE TO WS-JUL-DATE.                          ELPPR   
00211      CALL 'TSGGREG' USING WS-JUL-DATE                             ELPPR   
00212                           WS-GREG-DATE.                           ELPPR   
00213 *                                                                 ELPPR   
00214  DO-PRINT-LINE.                                                   ELPPR   
00215      MOVE PCB-PRINT-AREA TO PRT-PRINT-LINE.                       ELPPR   
00216      PERFORM WRITE-A-LINE.                                        ELPPR   
00217      PERFORM INCREMENT-NUMBER-OF-LINES.                           ELPPR   
00218 *                                                                 ELPPR   
00219  INCREMENT-NUMBER-OF-LINES.                                       ELPPR   
00220      IF PCB-SINGLE-SPACE                                          ELPPR   
00221         ADD +1 TO PCB-CURRENT-LINE                                ELPPR   
00222      ELSE                                                         ELPPR   
00223         IF PCB-DOUBLE-SPACE                                       ELPPR   
00224            ADD +2 TO PCB-CURRENT-LINE                             ELPPR   
00225         ELSE                                                      ELPPR   
00226            IF PCB-TRIPLE-SPACE                                    ELPPR   
00227               ADD +3 TO PCB-CURRENT-LINE.                         ELPPR   
00228                                                                   ELPPR   
00229  OPEN-FILE.                                                       ELPPR   
00230      MOVE PCB-PRINT-TEXT TO HGD1-PGM-ID.                          ELPPR   
00231      OPEN OUTPUT PRINTER.                                         ELPPR   
00232      MOVE ZERO TO PCB-PAGE-NUMBER                                 ELPPR   
00233                 PCB-CURRENT-LINE.                                 ELPPR   
00234      SET PCB-DEFAULT-MAX-LINES TO TRUE.                           ELPPR   
00235      SET FILE-OPENED TO TRUE.                                     ELPPR   
00236 *                                                                 ELPPR   
00237  WRITE-A-LINE.                                                    ELPPR   
00238      WRITE PRT-PRINT-LINE.                                        ELPPR   
00239 *                                                                 ELPPR   
00240  CLOSE-FILE.                                                      ELPPR   
00241      CLOSE PRINTER.                                               ELPPR   
00242      SET FILE-CLOSED TO TRUE.                                     ELPPR   
00243 *                                                                 ELPPR   
00244  HANDLE-INVALID-PARAMETER.                                        ELPPR   
00245      SET PCB-INVALID-REQUEST TO TRUE.                             ELPPR   
00246 *                                                                 ELPPR   
