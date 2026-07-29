00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPPRKTS
00003  PROGRAM-ID.         ELPPRKTS.                                       LV001
00004                                                                   ELPPRKTS
00005  AUTHOR.             ANNE KEFFER KING.                            ELPPRKTS
00006                                                                   ELPPRKTS
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPPRKTS
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPPRKTS
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPPRKTS
00010                      233 N. MICHIGAN AVE                          ELPPRKTS
00011                      CHICAGO, ILLINOIS 60601                      ELPPRKTS
00012                                                                   ELPPRKTS
00013  DATE-WRITTEN.       05-JUL-1989.                                 ELPPRKTS
00014                                                                   ELPPRKTS
00015  DATE-COMPILED.                                                   ELPPRKTS
00016                                                                   ELPPRKTS
00017  SECURITY.           COPYRIGHT 1988,                              ELPPRKTS
00018                      HEALTH CARE SERVICE CORPORATION              ELPPRKTS
00019      SKIP3                                                        ELPPRKTS
00020  ENVIRONMENT DIVISION.                                            ELPPRKTS
00021                                                                   ELPPRKTS
00022  CONFIGURATION SECTION.                                           ELPPRKTS
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELPPRKTS
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELPPRKTS
00025      EJECT                                                        ELPPRKTS
00026 ******************************************************************ELPPRKTS
00027 *                                                                *ELPPRKTS
00028 *                    ELS ABEND PROCESSING                        *ELPPRKTS
00029 *                                                                *ELPPRKTS
00030 *   ELPPRKTS - THIS SUBROUTINE FORMATS AND PRINTS 'REPORT        *ELPPRKTS
00031 *             PAGE 4' OF THE ELS ABEND REPORTS.                  *ELPPRKTS
00032 *             ELUABEND.                                          *ELPPRKTS
00033 *                                                                *ELPPRKTS
00034 ******************************************************************ELPPRKTS
00035 *                                                                *ELPPRKTS
00036 *                      MAINTENANCE HISTORY                       *ELPPRKTS
00037 *                                                                *ELPPRKTS
00038 *  MOD     DATE     BY  DRPT                ACTION               *ELPPRKTS
00039 * ----- ----------- --- ----- ---------------------------------- *ELPPRKTS
00040 * 01.00 05-JUL-1989 AKK       CREATED                            *ELPPRKTS
00041 *                                                                *ELPPRKTS
00042 * 01.01 03-NOV-1997 AKK       ADDED SUPPORT FOR YEAR 2000 AND    *ELPPRKTS
00043 *                             TX MERGE.                          *ELPPRKTS
00044 *                             I HAVE NOT ADDED PKG TO THE OUTPUT *ELPPRKTS
00045 *                             TABLE AT THIS TIME.                *ELPPRKTS
00046 ******************************************************************ELPPRKTS
00047                                                                   ELPPRKTS
00048  INPUT-OUTPUT SECTION.                                            ELPPRKTS
00049  FILE-CONTROL.                                                    ELPPRKTS
00050                                                                   ELPPRKTS
00051  DATA DIVISION.                                                   ELPPRKTS
00052  FILE SECTION.                                                    ELPPRKTS
00053 /                                                                 ELPPRKTS
00054  WORKING-STORAGE SECTION.                                         ELPPRKTS
00055  01  FILLER                 PIC X(16)   VALUE '*START ELPPRKTS*'. ELPPRKTS
00056 *                                                                 ELPPRKTS
00057 *                                                                 ELPPRKTS
00058  01  WS-WORK-AREAS.                                               ELPPRKTS
00059      05 WS-KTS-SUB          PIC S9(04)  COMP.                     ELPPRKTS
00060      05 WS-COL-SUB          PIC S9(04)  COMP.                     ELPPRKTS
00061      05 WS-MAX-PER-LINE     PIC S9(04)  COMP  VALUE +13.          ELPPRKTS
00062 *                                                                 ELPPRKTS
00063 *KTS SECTION KEY TABLE IN DOCUMENTATION REFERRED TO AS REPORT     ELPPRKTS
00064 *PAGE4.                                                           ELPPRKTS
00065  01  SKT-OUTPUT-LINES.                                            ELPPRKTS
00066      03  SKT-OUTPUT-LINE1.                                        ELPPRKTS
00067          05  SKT-OUTPUT-LINE1-CC   PIC X(01)  VALUE '1'.          ELPPRKTS
00068          05  FILLER                PIC X(33)  VALUE               ELPPRKTS
00069              'GROUP SECTION KEY TABLE FOR GROUP'.                 ELPPRKTS
00070          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKTS
00071          05  SKT-PLAN-CODE         PIC X(03)  VALUE SPACES.       ELPPRKTS
00072          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTS
00073          05  SKT-GROUP             PIC X(09)  VALUE SPACES.       ELPPRKTS
00074          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTS
00075          05  FILLER                PIC X(01)  VALUE '('.          ELPPRKTS
00076          05  SKT-NUM-OCCURANCES    PIC 9(03)  VALUE ZEROES.       ELPPRKTS
00077          05  FILLER                PIC X(11)  VALUE ' OCCURANCES'.ELPPRKTS
00078          05  FILLER                PIC X(01)  VALUE ')'.          ELPPRKTS
00079          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTS
00080          05  SKT-CONTINUE-PHRASE   PIC X(12)  VALUE SPACES.       ELPPRKTS
00081              88  SKT-FIRST-TIME               VALUE SPACES.       ELPPRKTS
00082              88  SKT-CONTINUE                 VALUE               ELPPRKTS
00083                                               '  *CONTINUE*'.     ELPPRKTS
00084          05  FILLER                PIC X(54)  VALUE SPACES.       ELPPRKTS
00085      03  SKT-OUTPUT-LINE2.                                        ELPPRKTS
00086          05  SKT-OUTPUT-LINE2-CC   PIC X(01)  VALUE '0'.          ELPPRKTS
00087          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKTS
00088          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00089          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00090          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00091          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00092          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00093          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00094          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00095          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00096          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00097          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00098          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00099          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00100          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00101          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00102          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00103          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00104          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00105          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00106          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00107          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00108          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00109          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00110          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00111          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00112          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKTS
00113          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKTS
00114      03  SKT-OUTPUT-LINE3.                                        ELPPRKTS
00115          05  SKT-OUTPUT-LINE3-CC   PIC X(01)  VALUE ' '.          ELPPRKTS
00116          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKTS
00117          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00118          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00119          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00120          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00121          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00122          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00123          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00124          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00125          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00126          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00127          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00128          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00129          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00130          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00131          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00132          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00133          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00134          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00135          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00136          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00137          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00138          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00139          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00140          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTS
00141          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKTS
00142          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKTS
00143      03  SKT-OUTPUT-LINE4.                                        ELPPRKTS
00144          05  SKT-OUTPUT-LINE4-CC   PIC X(01)  VALUE ' '.          ELPPRKTS
00145          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTS
00146          05  SKT-SECTIONS-TABLE.                                  ELPPRKTS
00147              10 SKT-SECTIONS OCCURS 13 TIMES                      ELPPRKTS
00148                  INDEXED BY SKT-SECTN-IDX.                        ELPPRKTS
00149                  15  SKT-SECTION-NUMBER    PIC X(05).             ELPPRKTS
00150                  15  FILLER                PIC X(06).             ELPPRKTS
00151                                                                   ELPPRKTS
00152  01  FILLER                 PIC X(14)   VALUE '*END ELPPRKTS*'.   ELPPRKTS
00153 /                                                                 ELPPRKTS
00154  LINKAGE SECTION.                                                 ELPPRKTS
00155      COPY ELSPRCBC.                                               ELPPRKTS
00156 /                                                                 ELPPRKTS
00157      COPY ELSKTBSC.                                               ELPPRKTS
00158 /                                                                 ELPPRKTS
00159  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK                 ELPPRKTS
00160                           KTS-SECTIONS-KEY-TABLE.                 ELPPRKTS
00161      PERFORM 0000-INITIALIZATION.                                 ELPPRKTS
00162      PERFORM 1000-DO-PRINT-KTS-LINES                              ELPPRKTS
00163         VARYING WS-KTS-SUB FROM 1 BY 1                            ELPPRKTS
00164            UNTIL WS-KTS-SUB > KTS-NBR-KEYS.                       ELPPRKTS
00165      PERFORM 9000-TERMINATION.                                    ELPPRKTS
00166      GOBACK.                                                      ELPPRKTS
00167 *                                                                 ELPPRKTS
00168  0000-INITIALIZATION.                                             ELPPRKTS
00169      MOVE PCB-PLAN-CODE TO SKT-PLAN-CODE.                         ELPPRKTS
00170      MOVE PCB-GROUP-NUM TO SKT-GROUP.                             ELPPRKTS
00171      MOVE KTS-NBR-KEYS TO SKT-NUM-OCCURANCES.                     ELPPRKTS
00172      SET SKT-FIRST-TIME TO TRUE.                                  ELPPRKTS
00173      MOVE SPACES TO SKT-OUTPUT-LINE4.                             ELPPRKTS
00174      MOVE 1 TO WS-COL-SUB.                                        ELPPRKTS
00175 *                                                                 ELPPRKTS
00176  1000-DO-PRINT-KTS-LINES.                                         ELPPRKTS
00177      SET KTS-IDX TO WS-KTS-SUB.                                   ELPPRKTS
00178      SET SKT-SECTN-IDX TO WS-COL-SUB.                             ELPPRKTS
00179      MOVE KTS-SECTION-NUMBER  (KTS-IDX) TO                        ELPPRKTS
00180                 SKT-SECTION-NUMBER (SKT-SECTN-IDX).               ELPPRKTS
00181      IF WS-COL-SUB = WS-MAX-PER-LINE                              ELPPRKTS
00182         PERFORM 1050-PRINT-A-LINE                                 ELPPRKTS
00183         MOVE SPACES TO SKT-OUTPUT-LINE4.                          ELPPRKTS
00184      ADD 1 TO WS-COL-SUB.                                         ELPPRKTS
00185 *                                                                 ELPPRKTS
00186  1050-PRINT-A-LINE.                                               ELPPRKTS
00187      IF SKT-FIRST-TIME OR                                         ELPPRKTS
00188           (PCB-CURRENT-LINE > PCB-MAX-LINES)                      ELPPRKTS
00189         PERFORM 1175-PRINT-PAGE4-HEADINGS.                        ELPPRKTS
00190      MOVE SKT-OUTPUT-LINE4 TO PCB-PRINT-TEXT.                     ELPPRKTS
00191      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKTS
00192      INITIALIZE WS-COL-SUB.                                       ELPPRKTS
00193 *                                                                 ELPPRKTS
00194  1175-PRINT-PAGE4-HEADINGS.                                       ELPPRKTS
00195       SET PCB-EJECT TO TRUE.                                      ELPPRKTS
00196       MOVE SKT-OUTPUT-LINE1 TO PCB-PRINT-AREA.                    ELPPRKTS
00197       PERFORM 8000-DO-THE-PRINT.                                  ELPPRKTS
00198       IF SKT-FIRST-TIME                                           ELPPRKTS
00199           SET SKT-CONTINUE TO TRUE.                               ELPPRKTS
00200       MOVE SKT-OUTPUT-LINE2 TO PCB-PRINT-AREA.                    ELPPRKTS
00201       PERFORM 8000-DO-THE-PRINT.                                  ELPPRKTS
00202       MOVE SKT-OUTPUT-LINE3 TO PCB-PRINT-AREA.                    ELPPRKTS
00203       PERFORM 8000-DO-THE-PRINT.                                  ELPPRKTS
00204 *                                                                 ELPPRKTS
00205  8000-DO-THE-PRINT.                                               ELPPRKTS
00206      SET PCB-PRINT-LINE TO TRUE.                                  ELPPRKTS
00207      INSPECT PCB-PRINT-AREA REPLACING ALL LOW-VALUES BY '_'.      ELPPRKTS
00208      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRKTS
00209 *                                                                 ELPPRKTS
00210  9000-TERMINATION.                                                ELPPRKTS
00211      IF SKT-OUTPUT-LINE4 NOT = SPACES                             ELPPRKTS
00212         MOVE SKT-OUTPUT-LINE4 TO PCB-PRINT-TEXT                   ELPPRKTS
00213         PERFORM 8000-DO-THE-PRINT.                                ELPPRKTS
