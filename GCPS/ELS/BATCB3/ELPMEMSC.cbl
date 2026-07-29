00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPMEMSC
00003  PROGRAM-ID.         ELPMEMSC.                                       LV001
00004                                                                   ELPMEMSC
00005  AUTHOR.             ANNE KEFFER KING.                            ELPMEMSC
00006                                                                   ELPMEMSC
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPMEMSC
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPMEMSC
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPMEMSC
00010                      233 N. MICHIGAN AVE                          ELPMEMSC
00011                      CHICAGO, ILLINOIS 60601                      ELPMEMSC
00012                                                                   ELPMEMSC
00013  DATE-WRITTEN.       10-JUL-1989.                                 ELPMEMSC
00014                                                                   ELPMEMSC
00015  DATE-COMPILED.                                                   ELPMEMSC
00016                                                                   ELPMEMSC
00017  SECURITY.           COPYRIGHT 1988,                              ELPMEMSC
00018                      HEALTH CARE SERVICE CORPORATION              ELPMEMSC
00019      SKIP3                                                        ELPMEMSC
00020  ENVIRONMENT DIVISION.                                            ELPMEMSC
00021                                                                   ELPMEMSC
00022  CONFIGURATION SECTION.                                           ELPMEMSC
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELPMEMSC
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELPMEMSC
00025      EJECT                                                        ELPMEMSC
00026 ******************************************************************ELPMEMSC
00027 *                                                                *ELPMEMSC
00028 *                    ELS ABEND PROCESSING                        *ELPMEMSC
00029 *                                                                *ELPMEMSC
00030 *   ELPMEMSC - THIS SUBROUTINE FORMATS AND PRINTS 'REPORT        *ELPMEMSC
00031 *             PAGE 5' OF THE ELS ABEND REPORTS.                  *ELPMEMSC
00032 *                                                                *ELPMEMSC
00033 ******************************************************************ELPMEMSC
00034 *                                                                *ELPMEMSC
00035 *                      MAINTENANCE HISTORY                       *ELPMEMSC
00036 *                                                                *ELPMEMSC
00037 *  MOD     DATE     BY  DRPT                ACTION               *ELPMEMSC
00038 * ----- ----------- --- ----- ---------------------------------- *ELPMEMSC
00039 * 01.00 10-JUL-1989 AKK       CREATED                            *ELPMEMSC
00040 *                                                                *ELPMEMSC
00041 * 01.01 03-NOV-1997 AKK       ADDED SUPPORT FOR YEAR 2000 AND    *ELPMEMSC
00042 *                             TX SUPPORT.                        *ELPMEMSC
00043 *                                                                *ELPMEMSC
00044 ******************************************************************ELPMEMSC
00045                                                                   ELPMEMSC
00046  INPUT-OUTPUT SECTION.                                            ELPMEMSC
00047  FILE-CONTROL.                                                    ELPMEMSC
00048                                                                   ELPMEMSC
00049  DATA DIVISION.                                                   ELPMEMSC
00050  FILE SECTION.                                                    ELPMEMSC
00051 /                                                                 ELPMEMSC
00052  WORKING-STORAGE SECTION.                                         ELPMEMSC
00053  01  FILLER                 PIC X(16)   VALUE '*START ELPMEMSC*'. ELPMEMSC
00054 *                                                                 ELPMEMSC
00055 *                                                                 ELPMEMSC
00056  01  WS-WORK-AREAS.                                               ELPMEMSC
00057      05 WS-MEMSC-SUB        PIC S9(04)  COMP.                     ELPMEMSC
00058      05 WS-COL-SUB          PIC S9(04)  COMP.                     ELPMEMSC
00059      05 WS-MAX-PER-LINE     PIC S9(04)  COMP  VALUE +05.          ELPMEMSC
00060      05 WS-JUL-DATE         PIC  9(05).                           ELPMEMSC
00061      05 WS-GREG-DATE        PIC  9(06).                           ELPMEMSC
00062 *                                                                 ELPMEMSC
00063 *THE FOLLOWING IS USED TO PRINT WHAT IS REFERRED TO IN THE        ELPMEMSC
00064 *DOCUMENTATION AS REPORT PAGE 5.                                  ELPMEMSC
00065  01  MEMSC-OUTPUT-LINES.                                          ELPMEMSC
00066      03  MEMSC-DETAIL-LINE1.                                      ELPMEMSC
00067          05  MEMSC-OUTPUT-LINE1-CC PIC X(01)  VALUE '1'.          ELPMEMSC
00068          05  FILLER                PIC X(45)  VALUE               ELPMEMSC
00069              'MEMBERSHIP INFORMATION FOR SUBSCRIBER'.             ELPMEMSC
00070          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00071          05  MEMSC-MEMBER-NAME.                                   ELPMEMSC
00072              10  MEMSC-FIRST-NAME  PIC X(10)   VALUE SPACE.       ELPMEMSC
00073              10  FILLER            PIC X       VALUE SPACE.       ELPMEMSC
00074              10  MEMSC-MID-INITIAL PIC X(01)   VALUE SPACE.       ELPMEMSC
00075              10  FILLER            PIC X       VALUE SPACE.       ELPMEMSC
00076              10  MEMSC-LAST-NAME   PIC X(20)   VALUE SPACE.       ELPMEMSC
00077          05  FILLER                PIC X(02)   VALUE SPACES.      ELPMEMSC
00078          05  FILLER                PIC X(20)   VALUE              ELPMEMSC
00079              '(NUMBER OF SECTIONS '.                              ELPMEMSC
00080          05  MEMSC-NUM-SECTIONS    PIC X(04)   VALUE SPACES.      ELPMEMSC
00081          05  FILLER                PIC X(01)   VALUE ')'.         ELPMEMSC
00082          05  FILLER                PIC X(01)   VALUE SPACES.      ELPMEMSC
00083          05  MEMSC-CONTINUE-PHRASE PIC X(10)   VALUE SPACES.      ELPMEMSC
00084              88  MEMSC-FIRST-TIME              VALUE SPACES.      ELPMEMSC
00085              88  MEMSC-CONTINUE                VALUE '*CONTINUE*'.ELPMEMSC
00086          05  FILLER                PIC X(14)   VALUE SPACES.      ELPMEMSC
00087      03  MEMSC-DETAIL-LINE2.                                      ELPMEMSC
00088          05  MEMSC-OUTPUT-LINE2-CC PIC X(01)  VALUE ' '.          ELPMEMSC
00089          05  FILLER                PIC X(11)  VALUE SPACES.       ELPMEMSC
00090          05  FILLER                PIC X(14)  VALUE               ELPMEMSC
00091              'EFFECTIVE DATE'.                                    ELPMEMSC
00092          05  FILLER                PIC X(01)  VALUE SPACES.       ELPMEMSC
00093          05  MEMSC-EFF-DATE        PIC 99/99/99.                  ELPMEMSC
00094          05  MEMSC-EFF-DATE-A REDEFINES MEMSC-EFF-DATE            ELPMEMSC
00095                                    PIC X(08).                     ELPMEMSC
00096          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00097          05  FILLER                PIC X(17)  VALUE               ELPMEMSC
00098              'TERMINATION DATE '.                                 ELPMEMSC
00099          05  MEMSC-TERM-DATE       PIC 99/99/99.                  ELPMEMSC
00100          05  MEMSC-TERM-DATE-A REDEFINES MEMSC-TERM-DATE          ELPMEMSC
00101                                    PIC X(08).                     ELPMEMSC
00102          05  FILLER                PIC X(69)  VALUE SPACES.       ELPMEMSC
00103      03  MEMSC-HEADING-LINE3.                                     ELPMEMSC
00104          05  MEMSC-OUTPUT-LINE3-CC PIC X(01)  VALUE '0'.          ELPMEMSC
00105          05  FILLER                PIC X(09)  VALUE 'PREVIOUS:'.  ELPMEMSC
00106          05  FILLER                PIC X(05)  VALUE SPACES.       ELPMEMSC
00107          05  FILLER                PIC X(13)                      ELPMEMSC
00108                                    VALUE 'GROUP NUMBER '.         ELPMEMSC
00109          05  MEMSC-PREVIOUS-GRP    PIC X(09)  VALUE SPACES.       ELPMEMSC
00110          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00111          05  FILLER                PIC X(18)  VALUE               ELPMEMSC
00112              'MEMBERSHIP NUMBER '.                                ELPMEMSC
00113          05  MEMSC-PREVIOUS-MEM  PIC X(10) VALUE SPACES.          ELPMEMSC
00114          05  FILLER                PIC X(75)  VALUE SPACES.       ELPMEMSC
00115      03  MEMSC-HEADING-LINE4.                                     ELPMEMSC
00116          05  MEMSC-OUTPUT-LINE4-CC PIC X(01)  VALUE ' '.          ELPMEMSC
00117          05  FILLER                PIC X(05)  VALUE 'NEXT:'.      ELPMEMSC
00118          05  FILLER                PIC X(09)  VALUE SPACES.       ELPMEMSC
00119          05  FILLER                PIC X(13)                      ELPMEMSC
00120                                     VALUE 'GROUP NUMBER'.         ELPMEMSC
00121          05  MEMSC-NEXT-GRP-NUM    PIC X(09)  VALUE SPACES.       ELPMEMSC
00122          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00123          05  FILLER                PIC X(17)  VALUE               ELPMEMSC
00124              'MEMBERSHIP NUMBER'.                                 ELPMEMSC
00125          05  FILLER                PIC X(01)  VALUE SPACES.       ELPMEMSC
00126          05  MEMSC-NEXT-MEM-NUM    PIC X(10) VALUE SPACES.        ELPMEMSC
00127          05  FILLER                PIC X(69)  VALUE SPACES.       ELPMEMSC
00128      03  MEMSC-HEADING-LINE5.                                     ELPMEMSC
00129          05  MEMSC-OUTPUT-LINE5-CC PIC X(01)  VALUE '0'.          ELPMEMSC
00130          05  FILLER                PIC X(17)  VALUE SPACES.       ELPMEMSC
00131          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPMEMSC
00132          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00133          05  FILLER                PIC X(04)  VALUE 'TERM'.       ELPMEMSC
00134          05  FILLER                PIC X(15)  VALUE SPACES.       ELPMEMSC
00135          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPMEMSC
00136          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00137          05  FILLER                PIC X(04)  VALUE 'TERM'.       ELPMEMSC
00138          05  FILLER                PIC X(15)  VALUE SPACES.       ELPMEMSC
00139          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPMEMSC
00140          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00141          05  FILLER                PIC X(04)  VALUE 'TERM'.       ELPMEMSC
00142          05  FILLER                PIC X(15)  VALUE SPACES.       ELPMEMSC
00143          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPMEMSC
00144          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00145          05  FILLER                PIC X(04)  VALUE 'TERM'.       ELPMEMSC
00146          05  FILLER                PIC X(15)  VALUE SPACES.       ELPMEMSC
00147          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPMEMSC
00148          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00149          05  FILLER                PIC X(04)  VALUE 'TERM'.       ELPMEMSC
00150          05  FILLER                PIC X(05)  VALUE SPACES.       ELPMEMSC
00151      03  MEMSC-HEADING-LINE6.                                     ELPMEMSC
00152          05  MEMSC-OUTPUT-LINE6-CC PIC X(01)  VALUE ' '.          ELPMEMSC
00153          05  FILLER                PIC X(06)  VALUE SPACES.       ELPMEMSC
00154          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPMEMSC
00155          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00156          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPMEMSC
00157          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00158          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPMEMSC
00159          05  FILLER                PIC X(04)  VALUE SPACES.       ELPMEMSC
00160          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPMEMSC
00161          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00162          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPMEMSC
00163          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00164          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPMEMSC
00165          05  FILLER                PIC X(04)  VALUE SPACES.       ELPMEMSC
00166          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPMEMSC
00167          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00168          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPMEMSC
00169          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00170          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPMEMSC
00171          05  FILLER                PIC X(04)  VALUE SPACES.       ELPMEMSC
00172          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPMEMSC
00173          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00174          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPMEMSC
00175          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00176          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPMEMSC
00177          05  FILLER                PIC X(04)  VALUE SPACES.       ELPMEMSC
00178          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPMEMSC
00179          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00180          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPMEMSC
00181          05  FILLER                PIC X(03)  VALUE SPACES.       ELPMEMSC
00182          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPMEMSC
00183          05  FILLER                PIC X(05)  VALUE SPACES.       ELPMEMSC
00184      03  MEMSC-HEADING-LINE7.                                     ELPMEMSC
00185          05  MEMSC-OUTPUT-LINE7-CC PIC X(01)  VALUE ' '.          ELPMEMSC
00186          05  FILLER                PIC X(06)  VALUE SPACES.       ELPMEMSC
00187          05  FILLER                PIC X(07)  VALUE '-------'.    ELPMEMSC
00188          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00189          05  FILLER                PIC X(05)  VALUE '-----'.      ELPMEMSC
00190          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00191          05  FILLER                PIC X(05)  VALUE '-----'.      ELPMEMSC
00192          05  FILLER                PIC X(04)  VALUE SPACES.       ELPMEMSC
00193          05  FILLER                PIC X(07)  VALUE '-------'.    ELPMEMSC
00194          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00195          05  FILLER                PIC X(05)  VALUE '-----'.      ELPMEMSC
00196          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00197          05  FILLER                PIC X(05)  VALUE '-----'.      ELPMEMSC
00198          05  FILLER                PIC X(04)  VALUE SPACES.       ELPMEMSC
00199          05  FILLER                PIC X(07)  VALUE '-------'.    ELPMEMSC
00200          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00201          05  FILLER                PIC X(05)  VALUE '-----'.      ELPMEMSC
00202          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00203          05  FILLER                PIC X(05)  VALUE '-----'.      ELPMEMSC
00204          05  FILLER                PIC X(04)  VALUE SPACES.       ELPMEMSC
00205          05  FILLER                PIC X(07)  VALUE '-------'.    ELPMEMSC
00206          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00207          05  FILLER                PIC X(05)  VALUE '-----'.      ELPMEMSC
00208          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00209          05  FILLER                PIC X(05)  VALUE '-----'.      ELPMEMSC
00210          05  FILLER                PIC X(04)  VALUE SPACES.       ELPMEMSC
00211          05  FILLER                PIC X(07)  VALUE '-------'.    ELPMEMSC
00212          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00213          05  FILLER                PIC X(05)  VALUE '-----'.      ELPMEMSC
00214          05  FILLER                PIC X(02)  VALUE SPACES.       ELPMEMSC
00215          05  FILLER                PIC X(05)  VALUE '-----'.      ELPMEMSC
00216          05  FILLER                PIC X(05)  VALUE SPACES.       ELPMEMSC
00217      03  MEMSC-DETAIL-LINE8.                                      ELPMEMSC
00218          05  MEMSC-OUTPUT-LINE8-CC PIC X(01)  VALUE ' '.          ELPMEMSC
00219          05  FILLER                PIC X(07)  VALUE SPACES.       ELPMEMSC
00220          05  MEMSC-SECTION-INFO-TABLE.                            ELPMEMSC
00221              10 MEMSC-SECTION-INFO OCCURS 5 TIMES                 ELPMEMSC
00222                  INDEXED BY MEMSC-IDX.                            ELPMEMSC
00223                  15  MEMSC-SECTION    PIC X(05).                  ELPMEMSC
00224                  15  FILLER           PIC X(03).                  ELPMEMSC
00225                  15  MEMSC-SECT-EFF   PIC 9(07).                  ELPMEMSC
00226                  15  MEMSC-SECT-EFF-A REDEFINES MEMSC-SECT-EFF    ELPMEMSC
00227                                       PIC X(07).                  ELPMEMSC
00228                  15  FILLER           PIC X(02).                  ELPMEMSC
00229                  15  MEMSC-SECT-TERM  PIC 9(07).                  ELPMEMSC
00230                  15  MEMSC-SECT-TERM-A REDEFINES MEMSC-SECT-TERM  ELPMEMSC
00231                                       PIC X(07).                  ELPMEMSC
00232                  15  FILLER           PIC X(05).                  ELPMEMSC
00233                                                                   ELPMEMSC
00234  01  FILLER                 PIC X(14)   VALUE '*END ELPMEMSC*'.   ELPMEMSC
00235 /                                                                 ELPMEMSC
00236  LINKAGE SECTION.                                                 ELPMEMSC
00237      COPY ELSPRCBC.                                               ELPMEMSC
00238 /                                                                 ELPMEMSC
00239      COPY ELSMEMSC.                                               ELPMEMSC
00240 /                                                                 ELPMEMSC
00241  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK                 ELPMEMSC
00242                           MSI-MEMBERSHIP-INTERFACE.               ELPMEMSC
00243      PERFORM 0000-INITIALIZATION.                                 ELPMEMSC
00244      PERFORM 1175-PRINT-GRP-INFO.                                 ELPMEMSC
00245      PERFORM 1000-DO-PRINT-MEMSC-LINES                            ELPMEMSC
00246         VARYING WS-MEMSC-SUB FROM 1 BY 1                          ELPMEMSC
00247            UNTIL WS-MEMSC-SUB > MSI-NBR-MBR-SECTNS.               ELPMEMSC
00248      PERFORM 9000-TERMINATION.                                    ELPMEMSC
00249      GOBACK.                                                      ELPMEMSC
00250 *                                                                 ELPMEMSC
00251  0000-INITIALIZATION.                                             ELPMEMSC
00252      MOVE SPACES TO MEMSC-DETAIL-LINE8.                           ELPMEMSC
00253      MOVE SPACES TO PCB-PRINT-TEXT.                               ELPMEMSC
00254      MOVE 1 TO WS-COL-SUB.                                        ELPMEMSC
00255 *                                                                 ELPMEMSC
00256  1000-DO-PRINT-MEMSC-LINES.                                       ELPMEMSC
00257      SET MSI-IDX TO WS-MEMSC-SUB.                                 ELPMEMSC
00258      SET MEMSC-IDX TO WS-COL-SUB.                                 ELPMEMSC
00259      PERFORM 1025-VERIFY-DATES.                                   ELPMEMSC
00260      MOVE MSI-MBR-SECTN (MSI-IDX) TO MEMSC-SECTION (MEMSC-IDX).   ELPMEMSC
00261      IF WS-COL-SUB = WS-MAX-PER-LINE                              ELPMEMSC
00262         PERFORM 1050-PRINT-A-LINE                                 ELPMEMSC
00263         MOVE SPACES TO MEMSC-DETAIL-LINE8.                        ELPMEMSC
00264      ADD 1 TO WS-COL-SUB.                                         ELPMEMSC
00265 *                                                                 ELPMEMSC
00266  1025-VERIFY-DATES.                                               ELPMEMSC
00267      IF MSI-EFF-DATE-CENTURY (MSI-IDX) IS NUMERIC                 ELPMEMSC
00268          MOVE MSI-EFF-DT  (MSI-IDX) TO                            ELPMEMSC
00269                    MEMSC-SECT-EFF (MEMSC-IDX)                     ELPMEMSC
00270      ELSE                                                         ELPMEMSC
00271         MOVE ALL '?' TO MEMSC-SECT-EFF-A (MEMSC-IDX)              ELPMEMSC
00272         SET PCB-INVALID-DATA-FOUND TO TRUE.                       ELPMEMSC
00273      IF MSI-TERMIN-DATE-CC (MSI-IDX) IS NUMERIC                   ELPMEMSC
00274          MOVE MSI-TERM-DT  (MSI-IDX) TO                           ELPMEMSC
00275                    MEMSC-SECT-TERM (MEMSC-IDX)                    ELPMEMSC
00276      ELSE                                                         ELPMEMSC
00277         MOVE ALL '?' TO MEMSC-SECT-TERM-A (MEMSC-IDX)             ELPMEMSC
00278         SET PCB-INVALID-DATA-FOUND TO TRUE.                       ELPMEMSC
00279 *                                                                 ELPMEMSC
00280  1050-PRINT-A-LINE.                                               ELPMEMSC
00281      IF PCB-CURRENT-LINE > PCB-MAX-LINES                          ELPMEMSC
00282         PERFORM 1179-PRINT-COLUMN-HEADINGS.                       ELPMEMSC
00283      MOVE MEMSC-DETAIL-LINE8 TO PCB-PRINT-AREA.                   ELPMEMSC
00284      PERFORM 8000-DO-THE-PRINT.                                   ELPMEMSC
00285      INITIALIZE WS-COL-SUB.                                       ELPMEMSC
00286 *                                                                 ELPMEMSC
00287  1179-PRINT-COLUMN-HEADINGS.                                      ELPMEMSC
00288       MOVE MEMSC-HEADING-LINE5 TO PCB-PRINT-AREA.                 ELPMEMSC
00289       PERFORM 8000-DO-THE-PRINT.                                  ELPMEMSC
00290       MOVE MEMSC-HEADING-LINE6 TO PCB-PRINT-AREA.                 ELPMEMSC
00291       PERFORM 8000-DO-THE-PRINT.                                  ELPMEMSC
00292       MOVE MEMSC-HEADING-LINE7 TO PCB-PRINT-AREA.                 ELPMEMSC
00293       PERFORM 8000-DO-THE-PRINT.                                  ELPMEMSC
00294 *                                                                 ELPMEMSC
00295  1175-PRINT-GRP-INFO.                                             ELPMEMSC
00296       SET PCB-EJECT TO TRUE.                                      ELPMEMSC
00297       PERFORM 1180-DO-DATA-MOVES.                                 ELPMEMSC
00298       MOVE MEMSC-DETAIL-LINE1 TO PCB-PRINT-AREA.                  ELPMEMSC
00299       PERFORM 8000-DO-THE-PRINT.                                  ELPMEMSC
00300       MOVE MEMSC-DETAIL-LINE2 TO PCB-PRINT-AREA.                  ELPMEMSC
00301       PERFORM 8000-DO-THE-PRINT.                                  ELPMEMSC
00302       MOVE MEMSC-HEADING-LINE3 TO PCB-PRINT-AREA.                 ELPMEMSC
00303       PERFORM 8000-DO-THE-PRINT.                                  ELPMEMSC
00304       MOVE MEMSC-HEADING-LINE4 TO PCB-PRINT-AREA.                 ELPMEMSC
00305       PERFORM 8000-DO-THE-PRINT.                                  ELPMEMSC
00306       PERFORM 1179-PRINT-COLUMN-HEADINGS.                         ELPMEMSC
00307       SET MEMSC-CONTINUE TO TRUE.                                 ELPMEMSC
00308 *                                                                 ELPMEMSC
00309  1180-DO-DATA-MOVES.                                              ELPMEMSC
00310       MOVE MSI-FIRST-NAME TO MEMSC-FIRST-NAME.                    ELPMEMSC
00311       MOVE MSI-MIDDLE-INITIAL TO MEMSC-MID-INITIAL.               ELPMEMSC
00312       MOVE MSI-LAST-NAME TO MEMSC-LAST-NAME.                      ELPMEMSC
00313       MOVE MSI-NBR-MBR-SECTNS TO MEMSC-NUM-SECTIONS.              ELPMEMSC
00314       MOVE MSI-PREV-GROUP-NUMBER TO MEMSC-PREVIOUS-GRP.           ELPMEMSC
00315       MOVE MSI-PREV-MEMBR-NBR TO MEMSC-PREVIOUS-MEM.              ELPMEMSC
00316       MOVE MSI-NEXT-GROUP-NUMBER TO MEMSC-NEXT-GRP-NUM.           ELPMEMSC
00317       MOVE MSI-NEXT-MEMBR-NBR TO MEMSC-NEXT-MEM-NUM.              ELPMEMSC
00318       PERFORM 1190-VERIFY-EFF-TERM-DATES.                         ELPMEMSC
00319 *                                                                 ELPMEMSC
00320  1190-VERIFY-EFF-TERM-DATES.                                      ELPMEMSC
00321       IF MSI-GRP-SUB-EFF-DATE-CC IS NUMERIC                       ELPMEMSC
00322          MOVE MSI-GRP-SUB-EFF-DT TO WS-JUL-DATE                   ELPMEMSC
00323          CALL 'TSGGREG' USING WS-JUL-DATE                         ELPMEMSC
00324                              WS-GREG-DATE                         ELPMEMSC
00325          MOVE WS-GREG-DATE TO MEMSC-EFF-DATE                      ELPMEMSC
00326       ELSE                                                        ELPMEMSC
00327          MOVE ALL '?' TO MEMSC-EFF-DATE-A                         ELPMEMSC
00328          SET PCB-INVALID-DATA-FOUND TO TRUE.                      ELPMEMSC
00329       IF MSI-GRP-SUB-TERM-DATE-CENTURY IS NUMERIC                 ELPMEMSC
00330          MOVE MSI-GRP-SUB-TERM-DT TO WS-JUL-DATE                  ELPMEMSC
00331          CALL 'TSGGREG' USING WS-JUL-DATE                         ELPMEMSC
00332                              WS-GREG-DATE                         ELPMEMSC
00333          MOVE WS-GREG-DATE TO MEMSC-TERM-DATE                     ELPMEMSC
00334       ELSE                                                        ELPMEMSC
00335          MOVE ALL '?' TO MEMSC-TERM-DATE-A                        ELPMEMSC
00336          SET PCB-INVALID-DATA-FOUND TO TRUE.                      ELPMEMSC
00337 *                                                                 ELPMEMSC
00338  8000-DO-THE-PRINT.                                               ELPMEMSC
00339      SET PCB-PRINT-LINE TO TRUE.                                  ELPMEMSC
00340      INSPECT PCB-PRINT-AREA REPLACING ALL LOW-VALUES BY '_'.      ELPMEMSC
00341      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPMEMSC
00342 *                                                                 ELPMEMSC
00343  9000-TERMINATION.                                                ELPMEMSC
00344      IF MEMSC-DETAIL-LINE8 NOT = SPACES                           ELPMEMSC
00345         MOVE MEMSC-DETAIL-LINE8 TO PCB-PRINT-TEXT                 ELPMEMSC
00346         PERFORM 8000-DO-THE-PRINT.                                ELPMEMSC
