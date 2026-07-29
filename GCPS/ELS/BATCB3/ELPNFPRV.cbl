00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPNFPRV
00003  PROGRAM-ID.         ELPNFPRV.                                       LV001
00004                                                                   ELPNFPRV
00005  AUTHOR.             ANNE KEFFER KING.                            ELPNFPRV
00006                                                                   ELPNFPRV
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPNFPRV
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPNFPRV
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPNFPRV
00010                      233 N. MICHIGAN AVE                          ELPNFPRV
00011                      CHICAGO, ILLINOIS 60601                      ELPNFPRV
00012                                                                   ELPNFPRV
00013  DATE-WRITTEN.       20-MAR-1990.                                 ELPNFPRV
00014                                                                   ELPNFPRV
00015  DATE-COMPILED.                                                   ELPNFPRV
00016                                                                   ELPNFPRV
00017  SECURITY.           COPYRIGHT 1989,                              ELPNFPRV
00018                      HEALTH CARE SERVICE CORPORATION              ELPNFPRV
00019  TITLE 'ELS ABEND PROCESSING - PRINT CODE VALUE EXCEPTION REPORT'.ELPNFPRV
00020 ******************************************************************ELPNFPRV
00021 *                                                                *ELPNFPRV
00022 *                    ELS ABEND PROCESSING                        *ELPNFPRV
00023 *                                                                *ELPNFPRV
00024 *   THIS SUBROUTINE PRINTS THE CODE VALUE EXCEPTION REPORT.      *ELPNFPRV
00025 *   ONE CALL WILL PRINT THE ENTIRE EXCEPTION REPORT.             *ELPNFPRV
00026 *                                                                *ELPNFPRV
00027 *   THE INPUT CONSISTS OF ONLY RECORDS EXTRACTED FROM THE        *ELPNFPRV
00028 *   ELS SNAPSHOT FILE. THE VALUES IN THIS FILE ARE IN CODES      *ELPNFPRV
00029 *   MANUAL, BUT NOT HANDLED IN ELS PROGRAMS.  THE ONLY VALUES    *ELPNFPRV
00030 *   THAT WOULD EVER BE ON THIS REPORT ARE THOSE THAT SHOULD      *ELPNFPRV
00031 *   BE HARD CODED IN AN ELS PROGRAM.                             *ELPNFPRV
00032 *                                                                *ELPNFPRV
00033 ******************************************************************ELPNFPRV
00034 *                                                                *ELPNFPRV
00035 *                      MAINTENANCE HISTORY                       *ELPNFPRV
00036 *                                                                *ELPNFPRV
00037 *  MOD     DATE     BY  DRPT                ACTION               *ELPNFPRV
00038 * ----- ----------- --- ----- ---------------------------------- *ELPNFPRV
00039 * 01.00 20-APR-1990 AKK       CREATED.                           *ELPNFPRV
00040 *                                                                *ELPNFPRV
00041 ******************************************************************ELPNFPRV
00042                                                                   ELPNFPRV
00043  ENVIRONMENT DIVISION.                                            ELPNFPRV
00044                                                                   ELPNFPRV
00045  CONFIGURATION SECTION.                                           ELPNFPRV
00046  SOURCE-COMPUTER.    IBM-3090.                                    ELPNFPRV
00047  OBJECT-COMPUTER.    IBM-3090.                                    ELPNFPRV
00048                                                                   ELPNFPRV
00049  INPUT-OUTPUT SECTION.                                            ELPNFPRV
00050  FILE-CONTROL.                                                    ELPNFPRV
00051       SELECT CV-PROGRAM-FILE  ASSIGN TO UT-S-VALPROG.             ELPNFPRV
00052     EJECT                                                         ELPNFPRV
00053  DATA DIVISION.                                                   ELPNFPRV
00054                                                                   ELPNFPRV
00055  FILE SECTION.                                                    ELPNFPRV
00056  FD  CV-PROGRAM-FILE                                              ELPNFPRV
00057      BLOCK CONTAINS 0 RECORDS                                     ELPNFPRV
00058      LABEL RECORDS ARE STANDARD                                   ELPNFPRV
00059      RECORDING MODE IS V.                                         ELPNFPRV
00060      COPY ELSNAPSC.                                               ELPNFPRV
00061 /                                                                 ELPNFPRV
00062  WORKING-STORAGE SECTION.                                         ELPNFPRV
00063  01  FILLER           PIC X(19)   VALUE '*START OF ELPNFPRV*'.    ELPNFPRV
00064  01  WS-MISC.                                                     ELPNFPRV
00065      05  WS-EOF-SW                PIC X       VALUE 'N'.          ELPNFPRV
00066          88  WS-EOF                           VALUE 'Y'.          ELPNFPRV
00067 *                                                                 ELPNFPRV
00068  01  WORK-AREA.                                                   ELPNFPRV
00069      05  HOLD-PREFIX.                                             ELPNFPRV
00070          10  HOLD-RECORD-PREFIX       PIC X(08)   VALUE SPACES.   ELPNFPRV
00071          10  HOLD-SYSTEM-NAME         PIC X(30)   VALUE SPACES.   ELPNFPRV
00072      05  HOLD-CODE-VALUE              PIC X(10)   VALUE SPACES.   ELPNFPRV
00073 /                                                                 ELPNFPRV
00074  01  PR-OUTPUT-LINES.                                             ELPNFPRV
00075      03  PR-HEADING-LINE-1.                                       ELPNFPRV
00076          05  LINE-01-CC            PIC X(01)  VALUE '1'.          ELPNFPRV
00077          05  FILLER                PIC X(47)  VALUE SPACES.       ELPNFPRV
00078          05  FILLER                PIC X(13)  VALUE               ELPNFPRV
00079              'E N G L I S H'.                                     ELPNFPRV
00080          05  FILLER                PIC X(02)  VALUE SPACES.       ELPNFPRV
00081          05  FILLER                PIC X(15)  VALUE               ELPNFPRV
00082              'L A N G U A G E'.                                   ELPNFPRV
00083          05  FILLER                PIC X(02)  VALUE SPACES.       ELPNFPRV
00084          05  FILLER                PIC X(11)  VALUE               ELPNFPRV
00085              'S Y S T E M'.                                       ELPNFPRV
00086          05  FILLER                PIC X(49)  VALUE SPACES.       ELPNFPRV
00087      03  PR-HEADING-LINE-2.                                       ELPNFPRV
00088          05  LINE-02-CC            PIC X(01)  VALUE ' '.          ELPNFPRV
00089          05  FILLER                PIC X(44)  VALUE SPACES.       ELPNFPRV
00090          05  FILLER                PIC X(07)  VALUE 'C O D E'.    ELPNFPRV
00091          05  FILLER                PIC X(02)  VALUE SPACES.       ELPNFPRV
00092          05  FILLER                PIC X(09)  VALUE 'V A L U E'.  ELPNFPRV
00093          05  FILLER                PIC X(02)  VALUE SPACES.       ELPNFPRV
00094          05  FILLER                PIC X(17)  VALUE               ELPNFPRV
00095              'E X C E P T I O N'.                                 ELPNFPRV
00096          05  FILLER                PIC X(02)  VALUE SPACES.       ELPNFPRV
00097          05  FILLER                PIC X(11)  VALUE               ELPNFPRV
00098              'R E P O R T'.                                       ELPNFPRV
00099          05  FILLER                PIC X(38)  VALUE SPACES.       ELPNFPRV
00100      03  PR-HEADING-LINE-3.                                       ELPNFPRV
00101          05  LINE-03-CC            PIC X(01)  VALUE '0'.          ELPNFPRV
00102          05  FILLER                PIC X(49)  VALUE               ELPNFPRV
00103              'THE FOLLOWING CODE VALUES(S) CANNOT BE PROCESSED:'. ELPNFPRV
00104          05  FILLER                PIC X(83)  VALUE SPACES.       ELPNFPRV
00105      03  PR-HEADING-LINE-4.                                       ELPNFPRV
00106          05  LINE-04-CC            PIC X(01)  VALUE '0'.          ELPNFPRV
00107          05  FILLER                PIC X(02)  VALUE SPACES.       ELPNFPRV
00108          05  FILLER                PIC X(07)  VALUE 'PROGRAM'.    ELPNFPRV
00109          05  FILLER                PIC X(20)  VALUE SPACES.       ELPNFPRV
00110          05  FILLER                PIC X(06)  VALUE 'SYSTEM'.     ELPNFPRV
00111          05  FILLER                PIC X(22)  VALUE SPACES.       ELPNFPRV
00112          05  FILLER                PIC X(04)  VALUE 'CODE'.       ELPNFPRV
00113          05  FILLER                PIC X(71)  VALUE SPACES.       ELPNFPRV
00114      03  PR-HEADING-LINE-5.                                       ELPNFPRV
00115          05  LINE-05-CC            PIC X(01)  VALUE ' '.          ELPNFPRV
00116          05  FILLER                PIC X(04)  VALUE SPACES.       ELPNFPRV
00117          05  FILLER                PIC X(04)  VALUE 'NAME'.       ELPNFPRV
00118          05  FILLER                PIC X(22)  VALUE SPACES.       ELPNFPRV
00119          05  FILLER                PIC X(04)  VALUE 'NAME'.       ELPNFPRV
00120          05  FILLER                PIC X(23)  VALUE SPACES.       ELPNFPRV
00121          05  FILLER                PIC X(05)  VALUE 'VALUE'.      ELPNFPRV
00122          05  FILLER                PIC X(70)  VALUE SPACES.       ELPNFPRV
00123      03  PR-HEADING-LINE-6.                                       ELPNFPRV
00124          05  LINE-06-CC            PIC X(01)  VALUE ' '.          ELPNFPRV
00125          05  FILLER                PIC X(02)  VALUE SPACES.       ELPNFPRV
00126          05  FILLER                PIC X(08)  VALUE '========'.   ELPNFPRV
00127          05  FILLER                PIC X(07)  VALUE SPACES.       ELPNFPRV
00128          05  FILLER                PIC X(30)  VALUE               ELPNFPRV
00129              '=============================='.                    ELPNFPRV
00130          05  FILLER                PIC X(08)  VALUE SPACES.       ELPNFPRV
00131          05  FILLER                PIC X(10)  VALUE '=========='. ELPNFPRV
00132          05  FILLER                PIC X(67)  VALUE SPACES.       ELPNFPRV
00133      03  PR-DETAIL-LINE-1.                                        ELPNFPRV
00134          05  LINE-07-CC            PIC X(01)  VALUE ' '.          ELPNFPRV
00135          05  FILLER                PIC X(02)  VALUE SPACES.       ELPNFPRV
00136          05  PR-PROGRAM-NAME       PIC X(08)  VALUE SPACES.       ELPNFPRV
00137          05  FILLER                PIC X(07)  VALUE SPACES.       ELPNFPRV
00138          05  PR-COBOL-NAME         PIC X(30)  VALUE SPACES.       ELPNFPRV
00139          05  FILLER                PIC X(12)  VALUE SPACES.       ELPNFPRV
00140          05  PR-CODE-VALUE         PIC X(04)  VALUE SPACES.       ELPNFPRV
00141          05  FILLER                PIC X(69)  VALUE SPACES.       ELPNFPRV
00142 *                                                                 ELPNFPRV
00143  01  FILLER           PIC X(17)   VALUE '*END OF ELPNFPRV*'.      ELPNFPRV
00144 /                                                                 ELPNFPRV
00145  LINKAGE SECTION.                                                 ELPNFPRV
00146  COPY ELSPRCBC.                                                   ELPNFPRV
00147 /                                                                 ELPNFPRV
00148  COPY ELSELOGC.                                                   ELPNFPRV
00149 /                                                                 ELPNFPRV
00150  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK.                ELPNFPRV
00151      PERFORM 0000-INITIALIZATION.                                 ELPNFPRV
00152      PERFORM 1000-PRINT-REPORT                                    ELPNFPRV
00153           UNTIL WS-EOF.                                           ELPNFPRV
00154      CLOSE CV-PROGRAM-FILE.                                       ELPNFPRV
00155      MOVE ZERO TO RETURN-CODE.                                    ELPNFPRV
00156      GOBACK.                                                      ELPNFPRV
00157                                                                   ELPNFPRV
00158  0000-INITIALIZATION.                                             ELPNFPRV
00159      OPEN INPUT CV-PROGRAM-FILE.                                  ELPNFPRV
00160      PERFORM 8500-DO-READ.                                        ELPNFPRV
00161      IF WS-EOF                                                    ELPNFPRV
00162         CONTINUE                                                  ELPNFPRV
00163      ELSE                                                         ELPNFPRV
00164         PERFORM 8000-PRINT-PAGE-HEADINGS.                         ELPNFPRV
00165 /                                                                 ELPNFPRV
00166  1000-PRINT-REPORT.                                               ELPNFPRV
00167      IF PCB-CURRENT-LINE > PCB-MAX-LINES                          ELPNFPRV
00168           PERFORM 8000-PRINT-PAGE-HEADINGS.                       ELPNFPRV
00169      MOVE SPACES TO PR-DETAIL-LINE-1.                             ELPNFPRV
00170      IF LG-LINE-PREFIX = HOLD-PREFIX                              ELPNFPRV
00171         AND                                                       ELPNFPRV
00172         LG-CODE-VALUE = HOLD-CODE-VALUE                           ELPNFPRV
00173            CONTINUE                                               ELPNFPRV
00174      ELSE                                                         ELPNFPRV
00175         PERFORM 1060-DO-RECORD-MOVES                              ELPNFPRV
00176         MOVE PR-DETAIL-LINE-1 TO PCB-PRINT-AREA                   ELPNFPRV
00177         PERFORM 9000-PRINT-LINE.                                  ELPNFPRV
00178      PERFORM 8500-DO-READ.                                        ELPNFPRV
00179 *                                                                 ELPNFPRV
00180  1060-DO-RECORD-MOVES.                                            ELPNFPRV
00181      MOVE LG-RECORD-PREFIX TO PR-PROGRAM-NAME                     ELPNFPRV
00182                               HOLD-RECORD-PREFIX.                 ELPNFPRV
00183      MOVE LG-ELEMENT-NAME TO PR-COBOL-NAME                        ELPNFPRV
00184                              HOLD-SYSTEM-NAME.                    ELPNFPRV
00185      MOVE LG-CODE-VALUE TO PR-CODE-VALUE                          ELPNFPRV
00186                            HOLD-CODE-VALUE.                       ELPNFPRV
00187 *                                                                 ELPNFPRV
00188 /                                                                 ELPNFPRV
00189  8000-PRINT-PAGE-HEADINGS.                                        ELPNFPRV
00190      MOVE PR-HEADING-LINE-1 TO PCB-PRINT-AREA.                    ELPNFPRV
00191      PERFORM 9000-PRINT-LINE.                                     ELPNFPRV
00192      MOVE PR-HEADING-LINE-2 TO PCB-PRINT-AREA.                    ELPNFPRV
00193      PERFORM 9000-PRINT-LINE.                                     ELPNFPRV
00194      MOVE PR-HEADING-LINE-3 TO PCB-PRINT-AREA.                    ELPNFPRV
00195      PERFORM 9000-PRINT-LINE.                                     ELPNFPRV
00196      MOVE PR-HEADING-LINE-4 TO PCB-PRINT-AREA.                    ELPNFPRV
00197      PERFORM 9000-PRINT-LINE.                                     ELPNFPRV
00198      MOVE PR-HEADING-LINE-5 TO PCB-PRINT-AREA.                    ELPNFPRV
00199      PERFORM 9000-PRINT-LINE.                                     ELPNFPRV
00200      MOVE PR-HEADING-LINE-6 TO PCB-PRINT-AREA.                    ELPNFPRV
00201      PERFORM 9000-PRINT-LINE.                                     ELPNFPRV
00202      MOVE SPACES         TO PCB-PRINT-AREA.                       ELPNFPRV
00203      PERFORM 9000-PRINT-LINE.                                     ELPNFPRV
00204                                                                   ELPNFPRV
00205  8500-DO-READ.                                                    ELPNFPRV
00206      READ CV-PROGRAM-FILE                                         ELPNFPRV
00207          AT END                                                   ELPNFPRV
00208               SET WS-EOF TO TRUE.                                 ELPNFPRV
00209      IF WS-EOF                                                    ELPNFPRV
00210         CONTINUE                                                  ELPNFPRV
00211      ELSE                                                         ELPNFPRV
00212         CALL 'ELUADDRS' USING SSR-SUB-REC (1)                     ELPNFPRV
00213                    ADDRESS OF LG-LOG-RECORD.                      ELPNFPRV
00214                                                                   ELPNFPRV
00215  9000-PRINT-LINE.                                                 ELPNFPRV
00216      SET PCB-PRINT-LINE             TO TRUE.                      ELPNFPRV
00217      INSPECT PCB-PRINT-AREA REPLACING ALL LOW-VALUE BY '_'.       ELPNFPRV
00218      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPNFPRV
