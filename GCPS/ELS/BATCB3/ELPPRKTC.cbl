00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPPRKTC
00003  PROGRAM-ID.         ELPPRKTC.                                       LV001
00004                                                                   ELPPRKTC
00005  AUTHOR.             EDWARD G LISS.                               ELPPRKTC
00006                                                                   ELPPRKTC
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPPRKTC
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPPRKTC
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPPRKTC
00010                      233 N. MICHIGAN AVE                          ELPPRKTC
00011                      CHICAGO, ILLINOIS 60601                      ELPPRKTC
00012                                                                   ELPPRKTC
00013  DATE-WRITTEN.       05-JUL-1989.                                 ELPPRKTC
00014                                                                   ELPPRKTC
00015  DATE-COMPILED.                                                   ELPPRKTC
00016                                                                   ELPPRKTC
00017  SECURITY.           COPYRIGHT 1989,                              ELPPRKTC
00018                      HEALTH CARE SERVICE CORPORATION              ELPPRKTC
00019      SKIP3                                                        ELPPRKTC
00020  ENVIRONMENT DIVISION.                                            ELPPRKTC
00021                                                                   ELPPRKTC
00022  CONFIGURATION SECTION.                                           ELPPRKTC
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELPPRKTC
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELPPRKTC
00025     SKIP3                                                         ELPPRKTC
00026 ******************************************************************ELPPRKTC
00027 *                                                                *ELPPRKTC
00028 *                    ELS ABEND PROCESSING                        *ELPPRKTC
00029 *                                                                *ELPPRKTC
00030 *   THIS SUBROUTINE PRINTS THE KTC (CONTRACT KEY TABLE) FOR ELS  *ELPPRKTC
00031 *   ABEND PROCESSING.  THE ENTIRE KTBC IS PRINTED BY THIS        *ELPPRKTC
00032 *   MODULE.                                                      *ELPPRKTC
00033 *                                                                *ELPPRKTC
00034 ******************************************************************ELPPRKTC
00035 *                                                                *ELPPRKTC
00036 *                      MAINTENANCE HISTORY                       *ELPPRKTC
00037 *                                                                *ELPPRKTC
00038 *  MOD     DATE     BY  DRPT                ACTION               *ELPPRKTC
00039 * ----- ----------- --- ----- ---------------------------------- *ELPPRKTC
00040 * 01.00 05-JUL-1989 EGL       CREATED.                           *ELPPRKTC
00041 *                                                                *ELPPRKTC
00042 * 01.01 01-OCT-1991 JPB       CHANGED CKT-FAM-REL-LEVEL FROM     *ELPPRKTC
00043 *                             PIC X(01) TO PIC X(02) FOR         *ELPPRKTC
00044 *                             EXPANSION OF FAMILY-RELATIONSHIP   *ELPPRKTC
00045 *                             INDICATOR.                         *ELPPRKTC
00046 *                                                                *ELPPRKTC
00047 * 01.02 03-NOV-1997 AKK       ADDED SUPPORT FOR YEAR 2000 AND    *ELPPRKTC
00048 *                             TEXAS MERGE.                       *ELPPRKTC
00049 ******************************************************************ELPPRKTC
00050  TITLE 'ELS ABEND PROCESSING - PRINT KTC SUBROUTINE'.             ELPPRKTC
00051  DATA DIVISION.                                                   ELPPRKTC
00052  WORKING-STORAGE SECTION.                                         ELPPRKTC
00053  01  FILLER           PIC X(20)   VALUE '*START OF ELPPRKTC*'.    ELPPRKTC
00054 *                                                                 ELPPRKTC
00055  01  WS-MISC-STUFF.                                               ELPPRKTC
00056      05  WS-JULIAN-DATE            PIC 9(5).                      ELPPRKTC
00057      05  WS-GREGORIAN-DATE         PIC 9(6).                      ELPPRKTC
00058      05  WS-INVALID-DATA-SW        PIC X      VALUE 'N'.          ELPPRKTC
00059          88  WS-INVALID-DATA-FOUND            VALUE 'Y'.          ELPPRKTC
00060      05  WS-KTC-SUB                PIC S9(4)  COMP SYNC.          ELPPRKTC
00061      05  WS-PRINT-COL              PIC S9(4)  COMP SYNC.          ELPPRKTC
00062                                                                   ELPPRKTC
00063                                                                   ELPPRKTC
00064  01  CKT-OUTPUT-LINES.                                            ELPPRKTC
00065      03  CKT-DETAIL-LINE1.                                        ELPPRKTC
00066          05  CKT-DETAIL-LINE1-CC   PIC X(01)  VALUE '1'.          ELPPRKTC
00067          05  FILLER                PIC X(28)  VALUE               ELPPRKTC
00068              'CONTRACT KEY TABLE FOR GROUP'.                      ELPPRKTC
00069          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKTC
00070          05  CKT-PLAN-CODE         PIC X(03)  VALUE SPACES.       ELPPRKTC
00071          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00072          05  CKT-GROUP-NUMBER      PIC X(09)  VALUE SPACES.       ELPPRKTC
00073          05  FILLER                PIC X(09)  VALUE ' SECTION '.  ELPPRKTC
00074          05  CKT-SECTION-NUMBER    PIC X(05)  VALUE SPACES.       ELPPRKTC
00075          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00076          05  FILLER                PIC X(01)  VALUE '('.          ELPPRKTC
00077          05  CKT-NUM-OCCURANCES    PIC ZZ9    VALUE ZEROES.       ELPPRKTC
00078          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKTC
00079          05  FILLER                PIC X(10)  VALUE 'OCCURENCES'. ELPPRKTC
00080          05  FILLER                PIC X(01)  VALUE ')'.          ELPPRKTC
00081          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00082          05  FILLER                PIC X(09)  VALUE 'PKG CODE '.  ELPPRKTC
00083          05  CKT-PKG-CODE          PIC X(03)  VALUE SPACE.        ELPPRKTC
00084          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00085          05  CKT-TYPE              PIC X(11)  VALUE SPACES.       ELPPRKTC
00086              88  CKT-FIRST-LINE               VALUE SPACES.       ELPPRKTC
00087              88  CKT-CONT-LINE                VALUE '*CONTINUED*'.ELPPRKTC
00088          05  FILLER                PIC X(30)  VALUE SPACES.       ELPPRKTC
00089      03  CKT-DETAIL-LINE2.                                        ELPPRKTC
00090          05  CKT-DETAIL-LINE2-CC   PIC X(01)  VALUE '0'.          ELPPRKTC
00091          05  FILLER                PIC X(13)  VALUE SPACES.       ELPPRKTC
00092          05  FILLER                PIC X(03)  VALUE 'FAM'.        ELPPRKTC
00093          05  FILLER                PIC X(42)  VALUE SPACES.       ELPPRKTC
00094          05  FILLER                PIC X(03)  VALUE 'FAM'.        ELPPRKTC
00095          05  FILLER                PIC X(42)  VALUE SPACES.       ELPPRKTC
00096          05  FILLER                PIC X(03)  VALUE 'FAM'.        ELPPRKTC
00097          05  FILLER                PIC X(26)  VALUE SPACES.       ELPPRKTC
00098      03  CKT-DETAIL-LINE3.                                        ELPPRKTC
00099          05  CKT-DETAIL-LINE3-CC   PIC X(01)  VALUE ' '.          ELPPRKTC
00100          05  FILLER                PIC X(07)  VALUE SPACES.       ELPPRKTC
00101          05  FILLER                PIC X(04)  VALUE 'PROV'.       ELPPRKTC
00102          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00103          05  FILLER                PIC X(03)  VALUE 'REL'.        ELPPRKTC
00104          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKTC
00105          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPPRKTC
00106          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00107          05  FILLER                PIC X(04)  VALUE 'TERM'.       ELPPRKTC
00108          05  FILLER                PIC X(22)  VALUE SPACES.       ELPPRKTC
00109          05  FILLER                PIC X(04)  VALUE 'PROV'.       ELPPRKTC
00110          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00111          05  FILLER                PIC X(03)  VALUE 'REL'.        ELPPRKTC
00112          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKTC
00113          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPPRKTC
00114          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00115          05  FILLER                PIC X(04)  VALUE 'TERM'.       ELPPRKTC
00116          05  FILLER                PIC X(22)  VALUE SPACES.       ELPPRKTC
00117          05  FILLER                PIC X(04)  VALUE 'PROV'.       ELPPRKTC
00118          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00119          05  FILLER                PIC X(03)  VALUE 'REL'.        ELPPRKTC
00120          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKTC
00121          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPPRKTC
00122          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00123          05  FILLER                PIC X(04)  VALUE 'TERM'.       ELPPRKTC
00124          05  FILLER                PIC X(12)  VALUE SPACES.       ELPPRKTC
00125      03  CKT-DETAIL-LINE4.                                        ELPPRKTC
00126          05  CKT-DETAIL-LINE4-CC   PIC X(01)  VALUE ' '.          ELPPRKTC
00127          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00128          05  FILLER                PIC X(03)  VALUE 'LOB'.        ELPPRKTC
00129          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00130          05  FILLER                PIC X(04)  VALUE 'CNTL'.       ELPPRKTC
00131          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00132          05  FILLER                PIC X(03)  VALUE 'LVL'.        ELPPRKTC
00133          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00134          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRKTC
00135          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00136          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRKTC
00137          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00138          05  FILLER                PIC X(06)  VALUE 'STATUS'.     ELPPRKTC
00139          05  FILLER                PIC X(08)  VALUE SPACES.       ELPPRKTC
00140          05  FILLER                PIC X(03)  VALUE 'LOB'.        ELPPRKTC
00141          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00142          05  FILLER                PIC X(04)  VALUE 'CNTL'.       ELPPRKTC
00143          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00144          05  FILLER                PIC X(03)  VALUE 'LVL'.        ELPPRKTC
00145          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00146          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRKTC
00147          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00148          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRKTC
00149          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00150          05  FILLER                PIC X(06)  VALUE 'STATUS'.     ELPPRKTC
00151          05  FILLER                PIC X(08)  VALUE SPACES.       ELPPRKTC
00152          05  FILLER                PIC X(03)  VALUE 'LOB'.        ELPPRKTC
00153          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00154          05  FILLER                PIC X(04)  VALUE 'CNTL'.       ELPPRKTC
00155          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00156          05  FILLER                PIC X(03)  VALUE 'LVL'.        ELPPRKTC
00157          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00158          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRKTC
00159          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00160          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRKTC
00161          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00162          05  FILLER                PIC X(06)  VALUE 'STATUS'.     ELPPRKTC
00163          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00164      03  CKT-DETAIL-LINE5.                                        ELPPRKTC
00165          05  CKT-DETAIL-LINE5-CC   PIC X(01)  VALUE ' '.          ELPPRKTC
00166          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00167          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKTC
00168          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00169          05  FILLER                PIC X(04)  VALUE '----'.       ELPPRKTC
00170          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00171          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKTC
00172          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00173          05  FILLER                PIC X(05)  VALUE '-----'.      ELPPRKTC
00174          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00175          05  FILLER                PIC X(05)  VALUE '-----'.      ELPPRKTC
00176          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00177          05  FILLER                PIC X(06)  VALUE '------'.     ELPPRKTC
00178          05  FILLER                PIC X(08)  VALUE SPACES.       ELPPRKTC
00179          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKTC
00180          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00181          05  FILLER                PIC X(04)  VALUE '----'.       ELPPRKTC
00182          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00183          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKTC
00184          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00185          05  FILLER                PIC X(05)  VALUE '-----'.      ELPPRKTC
00186          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00187          05  FILLER                PIC X(05)  VALUE '-----'.      ELPPRKTC
00188          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00189          05  FILLER                PIC X(06)  VALUE '------'.     ELPPRKTC
00190          05  FILLER                PIC X(08)  VALUE SPACES.       ELPPRKTC
00191          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKTC
00192          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00193          05  FILLER                PIC X(04)  VALUE '----'.       ELPPRKTC
00194          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00195          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKTC
00196          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00197          05  FILLER                PIC X(05)  VALUE '-----'.      ELPPRKTC
00198          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTC
00199          05  FILLER                PIC X(05)  VALUE '-----'.      ELPPRKTC
00200          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00201          05  FILLER                PIC X(06)  VALUE '------'.     ELPPRKTC
00202          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTC
00203      03  CKT-DETAIL-LINE6.                                        ELPPRKTC
00204          05  CKT-DETAIL-LINE6-CC   PIC X(01).                     ELPPRKTC
00205          05  CKT-COLUMNS           OCCURS 3 TIMES                 ELPPRKTC
00206                                    INDEXED BY CKT-COL-IDX.        ELPPRKTC
00207              10  FILLER                PIC X(03).                 ELPPRKTC
00208              10  CKT-LOB               PIC X(01).                 ELPPRKTC
00209              10  FILLER                PIC X(04).                 ELPPRKTC
00210              10  CKT-PROV-CTL          PIC X(02).                 ELPPRKTC
00211              10  FILLER                PIC X(04).                 ELPPRKTC
00212              10  CKT-FAM-REL-LEVEL     PIC X(02).                 ELPPRKTC
00213              10  FILLER                PIC X(01).                 ELPPRKTC
00214              10  CKT-EFF-DATE          PIC 9(05).                 ELPPRKTC
00215              10  CKT-EFF-DATE-A  REDEFINES CKT-EFF-DATE           ELPPRKTC
00216                                        PIC X(05).                 ELPPRKTC
00217              10  FILLER                PIC X(01).                 ELPPRKTC
00218              10  CKT-TERM-DATE         PIC 9(05).                 ELPPRKTC
00219              10  CKT-TERM-DATE-A REDEFINES CKT-TERM-DATE          ELPPRKTC
00220                                        PIC X(05).                 ELPPRKTC
00221              10  FILLER                PIC X(02).                 ELPPRKTC
00222              10  CKT-STATUS            PIC X(04).                 ELPPRKTC
00223              10  FILLER                PIC X(07).                 ELPPRKTC
00224                                                                   ELPPRKTC
00225  01  FILLER           PIC X(18)   VALUE '*END OF ELPPRKTC*'.      ELPPRKTC
00226 /                                                                 ELPPRKTC
00227  LINKAGE SECTION.                                                 ELPPRKTC
00228  COPY ELSPRCBC.                                                   ELPPRKTC
00229 /                                                                 ELPPRKTC
00230  COPY ELSKTBCC.                                                   ELPPRKTC
00231 /                                                                 ELPPRKTC
00232  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK                 ELPPRKTC
00233                           KTC-GCCONTR-KEY-TABLE.                  ELPPRKTC
00234      PERFORM 0000-INITIALIZATION.                                 ELPPRKTC
00235      PERFORM 1000-PRINT-KTC                                       ELPPRKTC
00236          VARYING WS-KTC-SUB FROM 1 BY 1                           ELPPRKTC
00237            UNTIL WS-KTC-SUB > KTC-NBR-KEYS.                       ELPPRKTC
00238      IF CKT-DETAIL-LINE6 NOT = SPACES                             ELPPRKTC
00239          PERFORM 1010-PRINT-DETAIL-LINE6.                         ELPPRKTC
00240      IF WS-INVALID-DATA-FOUND                                     ELPPRKTC
00241          SET PCB-INVALID-DATA-FOUND TO TRUE.                      ELPPRKTC
00242      GOBACK.                                                      ELPPRKTC
00243      SKIP3                                                        ELPPRKTC
00244  0000-INITIALIZATION.                                             ELPPRKTC
00245      MOVE SPACES TO CKT-DETAIL-LINE6.                             ELPPRKTC
00246      MOVE 1 TO WS-KTC-SUB.                                        ELPPRKTC
00247      MOVE 1 TO WS-PRINT-COL.                                      ELPPRKTC
00248                                                                   ELPPRKTC
00249      MOVE PCB-GROUP-NUM       TO CKT-GROUP-NUMBER.                ELPPRKTC
00250      MOVE PCB-SECT-NUMBER     TO CKT-SECTION-NUMBER.              ELPPRKTC
00251      MOVE KTC-NBR-KEYS        TO CKT-NUM-OCCURANCES.              ELPPRKTC
00252      SET CKT-FIRST-LINE       TO TRUE.                            ELPPRKTC
00253      PERFORM 8000-PRINT-HEADINGS.                                 ELPPRKTC
00254      SET CKT-CONT-LINE        TO TRUE.                            ELPPRKTC
00255 /                                                                 ELPPRKTC
00256  1000-PRINT-KTC.                                                  ELPPRKTC
00257      IF WS-PRINT-COL > 3                                          ELPPRKTC
00258          PERFORM 1010-PRINT-DETAIL-LINE6.                         ELPPRKTC
00259      SET KTC-IDX TO WS-KTC-SUB.                                   ELPPRKTC
00260      SET CKT-COL-IDX TO WS-PRINT-COL.                             ELPPRKTC
00261                                                                   ELPPRKTC
00262      MOVE KTC-L-O-B          (KTC-IDX) TO                         ELPPRKTC
00263                     CKT-LOB           (CKT-COL-IDX).              ELPPRKTC
00264      MOVE KTC-PROVDR-CONTROL (KTC-IDX) TO                         ELPPRKTC
00265                     CKT-PROV-CTL      (CKT-COL-IDX).              ELPPRKTC
00266      MOVE KTC-FAM-REL-LVL    (KTC-IDX) TO                         ELPPRKTC
00267                     CKT-FAM-REL-LEVEL (CKT-COL-IDX).              ELPPRKTC
00268                                                                   ELPPRKTC
00269      IF KTC-EFF-DT-CENTURY (KTC-IDX) IS NUMERIC                   ELPPRKTC
00270          MOVE KTC-EFF-DT    (KTC-IDX) TO                          ELPPRKTC
00271                     CKT-EFF-DATE      (CKT-COL-IDX)               ELPPRKTC
00272      ELSE                                                         ELPPRKTC
00273          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRKTC
00274          MOVE ALL '?' TO                                          ELPPRKTC
00275                     CKT-EFF-DATE-A    (CKT-COL-IDX).              ELPPRKTC
00276                                                                   ELPPRKTC
00277      IF KTC-TERM-DT-CENTURY (KTC-IDX) IS NUMERIC                  ELPPRKTC
00278          MOVE KTC-TERMN-DT   (KTC-IDX) TO                         ELPPRKTC
00279                     CKT-TERM-DATE     (CKT-COL-IDX)               ELPPRKTC
00280      ELSE                                                         ELPPRKTC
00281          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRKTC
00282          MOVE ALL '?' TO                                          ELPPRKTC
00283                     CKT-TERM-DATE-A   (CKT-COL-IDX).              ELPPRKTC
00284                                                                   ELPPRKTC
00285      MOVE KTC-SEL-IND-LST    (KTC-IDX) TO                         ELPPRKTC
00286                     CKT-STATUS        (CKT-COL-IDX).              ELPPRKTC
00287      ADD 1 TO WS-PRINT-COL.                                       ELPPRKTC
00288      SKIP3                                                        ELPPRKTC
00289  1010-PRINT-DETAIL-LINE6.                                         ELPPRKTC
00290      IF PCB-CURRENT-LINE > PCB-MAX-LINES                          ELPPRKTC
00291         PERFORM 8000-PRINT-HEADINGS.                              ELPPRKTC
00292      MOVE CKT-DETAIL-LINE6 TO PCB-PRINT-AREA.                     ELPPRKTC
00293      PERFORM 9000-PRINT-LINE.                                     ELPPRKTC
00294      MOVE SPACES TO CKT-DETAIL-LINE6.                             ELPPRKTC
00295      MOVE 1 TO WS-PRINT-COL.                                      ELPPRKTC
00296 /                                                                 ELPPRKTC
00297  8000-PRINT-HEADINGS.                                             ELPPRKTC
00298      MOVE CKT-DETAIL-LINE1    TO PCB-PRINT-AREA.                  ELPPRKTC
00299      PERFORM 9000-PRINT-LINE.                                     ELPPRKTC
00300      MOVE CKT-DETAIL-LINE2    TO PCB-PRINT-AREA.                  ELPPRKTC
00301      PERFORM 9000-PRINT-LINE.                                     ELPPRKTC
00302      MOVE CKT-DETAIL-LINE3    TO PCB-PRINT-AREA.                  ELPPRKTC
00303      PERFORM 9000-PRINT-LINE.                                     ELPPRKTC
00304      MOVE CKT-DETAIL-LINE4    TO PCB-PRINT-AREA.                  ELPPRKTC
00305      PERFORM 9000-PRINT-LINE.                                     ELPPRKTC
00306      MOVE CKT-DETAIL-LINE5    TO PCB-PRINT-AREA.                  ELPPRKTC
00307      PERFORM 9000-PRINT-LINE.                                     ELPPRKTC
00308      SKIP3                                                        ELPPRKTC
00309  9000-PRINT-LINE.                                                 ELPPRKTC
00310      SET PCB-PRINT-LINE             TO TRUE.                      ELPPRKTC
00311      INSPECT PCB-PRINT-AREA REPLACING ALL LOW-VALUE BY '_'.       ELPPRKTC
00312      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRKTC
