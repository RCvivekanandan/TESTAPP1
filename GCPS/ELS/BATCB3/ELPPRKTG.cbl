00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPPRKTG
00003  PROGRAM-ID.         ELPPRKTG.                                       LV001
00004                                                                   ELPPRKTG
00005  AUTHOR.             EDWARD G LISS.                               ELPPRKTG
00006                                                                   ELPPRKTG
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPPRKTG
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPPRKTG
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPPRKTG
00010                      233 N. MICHIGAN AVE                          ELPPRKTG
00011                      CHICAGO, ILLINOIS 60601                      ELPPRKTG
00012                                                                   ELPPRKTG
00013  DATE-WRITTEN.       11-JUL-1989.                                 ELPPRKTG
00014                                                                   ELPPRKTG
00015  DATE-COMPILED.                                                   ELPPRKTG
00016                                                                   ELPPRKTG
00017  SECURITY.           COPYRIGHT 1989,                              ELPPRKTG
00018                      HEALTH CARE SERVICE CORPORATION              ELPPRKTG
00019      SKIP3                                                        ELPPRKTG
00020  ENVIRONMENT DIVISION.                                            ELPPRKTG
00021                                                                   ELPPRKTG
00022  CONFIGURATION SECTION.                                           ELPPRKTG
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELPPRKTG
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELPPRKTG
00025     SKIP3                                                         ELPPRKTG
00026 ******************************************************************ELPPRKTG
00027 *                                                                *ELPPRKTG
00028 *                    ELS ABEND PROCESSING                        *ELPPRKTG
00029 *                                                                *ELPPRKTG
00030 *   THIS SUBROUTINE PRINTS THE KTG (GROUP KEY TABLE) FOR ELS     *ELPPRKTG
00031 *   ABEND PROCESSING.  THE ENTIRE KTBG IS PRINTED BY THIS        *ELPPRKTG
00032 *   MODULE.                                                      *ELPPRKTG
00033 *                                                                *ELPPRKTG
00034 ******************************************************************ELPPRKTG
00035 *                                                                *ELPPRKTG
00036 *                      MAINTENANCE HISTORY                       *ELPPRKTG
00037 *                                                                *ELPPRKTG
00038 *  MOD     DATE     BY  DRPT                ACTION               *ELPPRKTG
00039 * ----- ----------- --- ----- ---------------------------------- *ELPPRKTG
00040 * 01.00 11-JUL-1989 EGL       CREATED.                           *ELPPRKTG
00041 *                                                                *ELPPRKTG
00042 * 01.01 01-OCT-1991 JPB       CHANGED GKT-FAM-REL-LEVEL FROM     *ELPPRKTG
00043 *                             PIC X(01) TO PIC X(02) FOR FAMILY- *ELPPRKTG
00044 *                             RELATIONSHIP-INDICATOR EXPANSION.  *ELPPRKTG
00045 *                                                                *ELPPRKTG
00046 * 01.02 03-NOV-1997 AKK       ADDED SUPPORT FOR YEAR 2000 AND    *ELPPRKTG
00047 *                             TEXAS MERGE.                       *ELPPRKTG
00048 *                                                                *ELPPRKTG
00049 ******************************************************************ELPPRKTG
00050  TITLE 'ELS ABEND PROCESSING - PRINT KTG SUBROUTINE'.             ELPPRKTG
00051  DATA DIVISION.                                                   ELPPRKTG
00052  WORKING-STORAGE SECTION.                                         ELPPRKTG
00053  01  FILLER           PIC X(20)   VALUE '*START OF ELPPRKTG*'.    ELPPRKTG
00054 *                                                                 ELPPRKTG
00055  01  WS-MISC-STUFF.                                               ELPPRKTG
00056      05  WS-JULIAN-DATE            PIC 9(5).                      ELPPRKTG
00057      05  WS-GREGORIAN-DATE         PIC 9(6).                      ELPPRKTG
00058      05  WS-INVALID-DATA-SW        PIC X      VALUE 'N'.          ELPPRKTG
00059          88  WS-INVALID-DATA-FOUND            VALUE 'Y'.          ELPPRKTG
00060      05  WS-KTG-SUB                PIC S9(4)  COMP SYNC.          ELPPRKTG
00061      05  WS-PRINT-COL              PIC S9(4)  COMP SYNC.          ELPPRKTG
00062                                                                   ELPPRKTG
00063                                                                   ELPPRKTG
00064  01  GKT-OUTPUT-LINES.                                            ELPPRKTG
00065      03  GKT-DETAIL-LINE1.                                        ELPPRKTG
00066          05  GKT-DETAIL-LINE1-CC   PIC X(01)  VALUE '1'.          ELPPRKTG
00067          05  FILLER                PIC X(34)  VALUE               ELPPRKTG
00068              'GROUP SPECIFIC KEY TABLE FOR GROUP'.                ELPPRKTG
00069          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKTG
00070          05  GKT-PLAN-CODE         PIC X(03)  VALUE SPACES.       ELPPRKTG
00071          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00072          05  GKT-GROUP-NUM         PIC X(09)  VALUE SPACES.       ELPPRKTG
00073          05  FILLER                PIC X(09)  VALUE ' SECTION '.  ELPPRKTG
00074          05  GKT-SECTION-NUM       PIC X(05)  VALUE SPACES.       ELPPRKTG
00075          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00076          05  FILLER                PIC X(01)  VALUE '('.          ELPPRKTG
00077          05  GKT-NUM-OCCURANCES    PIC 9(03)  VALUE ZEROES.       ELPPRKTG
00078          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKTG
00079          05  FILLER                PIC X(10)  VALUE 'OCCURENCES'. ELPPRKTG
00080          05  FILLER                PIC X(01)  VALUE ')'.          ELPPRKTG
00081          05  FILLER                PIC X(2)   VALUE SPACES.       ELPPRKTG
00082          05  FILLER                PIC X(09)  VALUE 'PKG CODE '.  ELPPRKTG
00083          05  GKT-PKG-CODE          PIC X(03)  VALUE SPACE.        ELPPRKTG
00084          05  GKT-TYPE              PIC X(11)  VALUE SPACES.       ELPPRKTG
00085              88  GKT-FIRST-LINE              VALUE SPACES.        ELPPRKTG
00086              88  GKT-CONT-LINE               VALUE '*CONTINUED*'. ELPPRKTG
00087      03  GKT-DETAIL-LINE2.                                        ELPPRKTG
00088          05  GKT-DEATIL-LINE2-CC   PIC X(01)  VALUE '0'.          ELPPRKTG
00089          05  FILLER                PIC X(06)  VALUE SPACES.       ELPPRKTG
00090          05  FILLER                PIC X(03)  VALUE 'FAM'.        ELPPRKTG
00091          05  FILLER                PIC X(16)  VALUE SPACES.       ELPPRKTG
00092          05  FILLER                PIC X(04)  VALUE 'PART'.       ELPPRKTG
00093          05  FILLER                PIC X(11)  VALUE SPACES.       ELPPRKTG
00094          05  FILLER                PIC X(09)  VALUE 'PROV CNTL'.  ELPPRKTG
00095          05  FILLER                PIC X(23)  VALUE SPACES.       ELPPRKTG
00096          05  FILLER                PIC X(03)  VALUE 'FAM'.        ELPPRKTG
00097          05  FILLER                PIC X(16)  VALUE SPACES.       ELPPRKTG
00098          05  FILLER                PIC X(04)  VALUE 'PART'.       ELPPRKTG
00099          05  FILLER                PIC X(11)  VALUE SPACES.       ELPPRKTG
00100          05  FILLER                PIC X(09)  VALUE 'PROV CNTL'.  ELPPRKTG
00101          05  FILLER                PIC X(17)  VALUE SPACES.       ELPPRKTG
00102      03  GKT-DETAIL-LINE3.                                        ELPPRKTG
00103          05  GKT-DETAIL-LINE3-CC   PIC X(01)  VALUE ' '.          ELPPRKTG
00104          05  FILLER                PIC X(06)  VALUE SPACES.       ELPPRKTG
00105          05  FILLER                PIC X(03)  VALUE 'REL'.        ELPPRKTG
00106          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKTG
00107          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPPRKTG
00108          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00109          05  FILLER                PIC X(04)  VALUE 'TERM'.       ELPPRKTG
00110          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTG
00111          05  FILLER                PIC X(04)  VALUE 'PROV'.       ELPPRKTG
00112          05  FILLER                PIC X(11)  VALUE SPACES.       ELPPRKTG
00113          05  FILLER                PIC X(08)  VALUE 'LVL. IND'.   ELPPRKTG
00114          05  FILLER                PIC X(24)  VALUE SPACES.       ELPPRKTG
00115          05  FILLER                PIC X(03)  VALUE 'REL'.        ELPPRKTG
00116          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKTG
00117          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPPRKTG
00118          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00119          05  FILLER                PIC X(04)  VALUE 'TERM'.       ELPPRKTG
00120          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTG
00121          05  FILLER                PIC X(04)  VALUE 'PROV'.       ELPPRKTG
00122          05  FILLER                PIC X(11)  VALUE SPACES.       ELPPRKTG
00123          05  FILLER                PIC X(08)  VALUE 'LVL. IND'.   ELPPRKTG
00124          05  FILLER                PIC X(18)  VALUE SPACES.       ELPPRKTG
00125      03  GKT-DETAIL-LINE4.                                        ELPPRKTG
00126          05  GKT-DETAIL-LINE-CC    PIC X(01)  VALUE ' '.          ELPPRKTG
00127          05  FILLER                PIC X(06)  VALUE SPACES.       ELPPRKTG
00128          05  FILLER                PIC X(03)  VALUE 'LVL'.        ELPPRKTG
00129          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00130          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRKTG
00131          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00132          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRKTG
00133          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTG
00134          05  FILLER                PIC X(04)  VALUE 'IND.'.       ELPPRKTG
00135          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00136          05  FILLER                PIC X(03)  VALUE 'LOB'.        ELPPRKTG
00137          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKTG
00138          05  FILLER                PIC X(11)  VALUE               ELPPRKTG
00139              'IB IS PB PS'.                                       ELPPRKTG
00140          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00141          05  FILLER                PIC X(06)  VALUE 'STATUS'.     ELPPRKTG
00142          05  FILLER                PIC X(13)  VALUE SPACES.       ELPPRKTG
00143          05  FILLER                PIC X(03)  VALUE 'LVL'.        ELPPRKTG
00144          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00145          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRKTG
00146          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00147          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRKTG
00148          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTG
00149          05  FILLER                PIC X(04)  VALUE 'IND.'.       ELPPRKTG
00150          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00151          05  FILLER                PIC X(03)  VALUE 'LOB'.        ELPPRKTG
00152          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKTG
00153          05  FILLER                PIC X(11)  VALUE               ELPPRKTG
00154              'IB IS PB PS'.                                       ELPPRKTG
00155          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00156          05  FILLER                PIC X(06)  VALUE 'STATUS'.     ELPPRKTG
00157          05  FILLER                PIC X(07)  VALUE SPACES.       ELPPRKTG
00158      03  GKT-DETAIL-LINE5.                                        ELPPRKTG
00159          05  GKT-DETAIL-LINE5-CC   PIC X(01)  VALUE ' '.          ELPPRKTG
00160          05  FILLER                PIC X(06)  VALUE SPACES.       ELPPRKTG
00161          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKTG
00162          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTG
00163          05  FILLER                PIC X(05)  VALUE '-----'.      ELPPRKTG
00164          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTG
00165          05  FILLER                PIC X(05)  VALUE '-----'.      ELPPRKTG
00166          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTG
00167          05  FILLER                PIC X(04)  VALUE '----'.       ELPPRKTG
00168          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00169          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKTG
00170          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKTG
00171          05  FILLER                PIC X(11)  VALUE               ELPPRKTG
00172              '-- -- -- --'.                                       ELPPRKTG
00173          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00174          05  FILLER                PIC X(06)  VALUE '------'.     ELPPRKTG
00175          05  FILLER                PIC X(13)  VALUE SPACES.       ELPPRKTG
00176          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKTG
00177          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTG
00178          05  FILLER                PIC X(05)  VALUE '-----'.      ELPPRKTG
00179          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTG
00180          05  FILLER                PIC X(05)  VALUE '-----'.      ELPPRKTG
00181          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRKTG
00182          05  FILLER                PIC X(04)  VALUE '----'.       ELPPRKTG
00183          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00184          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKTG
00185          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKTG
00186          05  FILLER                PIC X(11)  VALUE               ELPPRKTG
00187              '-- -- -- --'.                                       ELPPRKTG
00188          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKTG
00189          05  FILLER                PIC X(06)  VALUE '------'.     ELPPRKTG
00190          05  FILLER                PIC X(07)  VALUE SPACES.       ELPPRKTG
00191      03  GKT-DETAIL-LINE6.                                        ELPPRKTG
00192          05  GKT-DETAIL-LINE6-CC   PIC X(01)  VALUE ' '.          ELPPRKTG
00193          05  GKT-DETAIL-COLS       OCCURS 2 TIMES                 ELPPRKTG
00194                                    INDEXED BY GKT-COL-IDX.        ELPPRKTG
00195              10  FILLER                PIC X(07).                 ELPPRKTG
00196              10  GKT-FAM-REL-LEVEL     PIC X(02).                 ELPPRKTG
00197              10  FILLER                PIC X(02).                 ELPPRKTG
00198              10  GKT-EFF-DATE          PIC 9(05).                 ELPPRKTG
00199              10  GKT-EFF-DATE-A REDEFINES GKT-EFF-DATE            ELPPRKTG
00200                                        PIC X(05).                 ELPPRKTG
00201              10  FILLER                PIC X(02).                 ELPPRKTG
00202              10  GKT-TERM-DATE         PIC 9(05).                 ELPPRKTG
00203              10  GKT-TERM-DATE-A REDEFINES GKT-TERM-DATE          ELPPRKTG
00204                                        PIC X(05).                 ELPPRKTG
00205              10  FILLER                PIC X(04).                 ELPPRKTG
00206              10  GKT-PART-PROV-IND     PIC X(02).                 ELPPRKTG
00207              10  FILLER                PIC X(04).                 ELPPRKTG
00208              10  GKT-LOB               PIC X(03).                 ELPPRKTG
00209              10  FILLER                PIC X(04).                 ELPPRKTG
00210              10  GKT-PROV-CTL-IB       PIC X(02).                 ELPPRKTG
00211              10  FILLER                PIC X(01).                 ELPPRKTG
00212              10  GKT-PROV-CTL-IS       PIC X(02).                 ELPPRKTG
00213              10  FILLER                PIC X(01).                 ELPPRKTG
00214              10  GKT-PROV-CTL-PB       PIC X(02).                 ELPPRKTG
00215              10  FILLER                PIC X(01).                 ELPPRKTG
00216              10  GKT-PROV-CTL-PS       PIC X(02).                 ELPPRKTG
00217              10  FILLER                PIC X(06).                 ELPPRKTG
00218              10  GKT-STATUS            PIC X(01).                 ELPPRKTG
00219              10  FILLER                PIC X(08).                 ELPPRKTG
00220                                                                   ELPPRKTG
00221  01  FILLER           PIC X(18)   VALUE '*END OF ELPPRKTG*'.      ELPPRKTG
00222 /                                                                 ELPPRKTG
00223  LINKAGE SECTION.                                                 ELPPRKTG
00224  COPY ELSPRCBC.                                                   ELPPRKTG
00225 /                                                                 ELPPRKTG
00226  COPY ELSKTBGC.                                                   ELPPRKTG
00227 /                                                                 ELPPRKTG
00228  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK                 ELPPRKTG
00229                           KTG-GCGRPSPC-KEY-TABLE.                 ELPPRKTG
00230      PERFORM 0000-INITIALIZATION.                                 ELPPRKTG
00231      PERFORM 1000-PRINT-KTG                                       ELPPRKTG
00232          VARYING WS-KTG-SUB FROM 1 BY 1                           ELPPRKTG
00233            UNTIL WS-KTG-SUB > KTG-NBR-KEYS.                       ELPPRKTG
00234      IF GKT-DETAIL-LINE6 NOT = SPACES                             ELPPRKTG
00235          PERFORM 1010-PRINT-DETAIL-LINE6.                         ELPPRKTG
00236      IF WS-INVALID-DATA-FOUND                                     ELPPRKTG
00237          SET PCB-INVALID-DATA-FOUND TO TRUE.                      ELPPRKTG
00238      GOBACK.                                                      ELPPRKTG
00239      SKIP3                                                        ELPPRKTG
00240  0000-INITIALIZATION.                                             ELPPRKTG
00241      MOVE SPACES TO GKT-DETAIL-LINE6.                             ELPPRKTG
00242      MOVE 1 TO WS-KTG-SUB.                                        ELPPRKTG
00243      MOVE 1 TO WS-PRINT-COL.                                      ELPPRKTG
00244                                                                   ELPPRKTG
00245      MOVE PCB-PLAN-CODE       TO GKT-PLAN-CODE.                   ELPPRKTG
00246      MOVE PCB-GROUP-NUM       TO GKT-GROUP-NUM.                   ELPPRKTG
00247      MOVE PCB-SECT-NUMBER     TO GKT-SECTION-NUM.                 ELPPRKTG
00248      MOVE PCB-PKG-CODE        TO GKT-PKG-CODE.                    ELPPRKTG
00249      MOVE KTG-NBR-KEYS        TO GKT-NUM-OCCURANCES.              ELPPRKTG
00250      SET GKT-FIRST-LINE       TO TRUE.                            ELPPRKTG
00251      PERFORM 8000-PRINT-HEADINGS.                                 ELPPRKTG
00252      SET GKT-CONT-LINE        TO TRUE.                            ELPPRKTG
00253 /                                                                 ELPPRKTG
00254  1000-PRINT-KTG.                                                  ELPPRKTG
00255      IF WS-PRINT-COL > 2                                          ELPPRKTG
00256          PERFORM 1010-PRINT-DETAIL-LINE6.                         ELPPRKTG
00257      SET KTG-IDX TO WS-KTG-SUB.                                   ELPPRKTG
00258      SET GKT-COL-IDX TO WS-PRINT-COL.                             ELPPRKTG
00259                                                                   ELPPRKTG
00260      MOVE KTG-FAM-REL-LVL    (KTG-IDX) TO                         ELPPRKTG
00261                     GKT-FAM-REL-LEVEL (GKT-COL-IDX).              ELPPRKTG
00262                                                                   ELPPRKTG
00263      IF KTG-EFF-DT-CENTURY (KTG-IDX) IS NUMERIC                   ELPPRKTG
00264          MOVE KTG-EFF-DT (KTG-IDX) TO                             ELPPRKTG
00265                     GKT-EFF-DATE      (GKT-COL-IDX)               ELPPRKTG
00266      ELSE                                                         ELPPRKTG
00267          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRKTG
00268          MOVE ALL '?' TO                                          ELPPRKTG
00269                     GKT-EFF-DATE-A    (GKT-COL-IDX).              ELPPRKTG
00270                                                                   ELPPRKTG
00271      IF KTG-TERM-DT-CENTURY (KTG-IDX) IS NUMERIC                  ELPPRKTG
00272          MOVE KTG-TERMN-DT (KTG-IDX) TO                           ELPPRKTG
00273                     GKT-TERM-DATE     (GKT-COL-IDX)               ELPPRKTG
00274      ELSE                                                         ELPPRKTG
00275          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRKTG
00276          MOVE ALL '?' TO                                          ELPPRKTG
00277                     GKT-TERM-DATE-A   (GKT-COL-IDX).              ELPPRKTG
00278                                                                   ELPPRKTG
00279      MOVE KTG-PARTICIPAT-PROV-OPTION (KTG-IDX) TO                 ELPPRKTG
00280                     GKT-PART-PROV-IND (GKT-COL-IDX).              ELPPRKTG
00281      MOVE KTG-L-O-B-CONTRACT-LEVEL-IND (KTG-IDX) TO               ELPPRKTG
00282                     GKT-LOB           (GKT-COL-IDX).              ELPPRKTG
00283      MOVE KTG-PC-LVL-INST-BAS-IND (KTG-IDX) TO                    ELPPRKTG
00284                     GKT-PROV-CTL-IB   (GKT-COL-IDX).              ELPPRKTG
00285      MOVE KTG-PC-LVL-INST-SUP-IND (KTG-IDX) TO                    ELPPRKTG
00286                     GKT-PROV-CTL-IS   (GKT-COL-IDX).              ELPPRKTG
00287      MOVE KTG-PC-LVL-PROF-BAS-IND (KTG-IDX) TO                    ELPPRKTG
00288                     GKT-PROV-CTL-PB   (GKT-COL-IDX).              ELPPRKTG
00289      MOVE KTG-PC-LVL-PROF-SUP-IND (KTG-IDX) TO                    ELPPRKTG
00290                     GKT-PROV-CTL-PS   (GKT-COL-IDX).              ELPPRKTG
00291      MOVE KTG-IND            (KTG-IDX) TO                         ELPPRKTG
00292                     GKT-STATUS        (GKT-COL-IDX).              ELPPRKTG
00293      ADD 1 TO WS-PRINT-COL.                                       ELPPRKTG
00294      SKIP3                                                        ELPPRKTG
00295  1010-PRINT-DETAIL-LINE6.                                         ELPPRKTG
00296      IF PCB-CURRENT-LINE > PCB-MAX-LINES                          ELPPRKTG
00297         PERFORM 8000-PRINT-HEADINGS.                              ELPPRKTG
00298      MOVE GKT-DETAIL-LINE6 TO PCB-PRINT-AREA.                     ELPPRKTG
00299      PERFORM 9000-PRINT-LINE.                                     ELPPRKTG
00300      MOVE SPACES TO GKT-DETAIL-LINE6.                             ELPPRKTG
00301      MOVE 1 TO WS-PRINT-COL.                                      ELPPRKTG
00302 /                                                                 ELPPRKTG
00303  8000-PRINT-HEADINGS.                                             ELPPRKTG
00304      MOVE GKT-DETAIL-LINE1    TO PCB-PRINT-AREA.                  ELPPRKTG
00305      PERFORM 9000-PRINT-LINE.                                     ELPPRKTG
00306      MOVE GKT-DETAIL-LINE2    TO PCB-PRINT-AREA.                  ELPPRKTG
00307      PERFORM 9000-PRINT-LINE.                                     ELPPRKTG
00308      MOVE GKT-DETAIL-LINE3    TO PCB-PRINT-AREA.                  ELPPRKTG
00309      PERFORM 9000-PRINT-LINE.                                     ELPPRKTG
00310      MOVE GKT-DETAIL-LINE4    TO PCB-PRINT-AREA.                  ELPPRKTG
00311      PERFORM 9000-PRINT-LINE.                                     ELPPRKTG
00312      MOVE GKT-DETAIL-LINE5    TO PCB-PRINT-AREA.                  ELPPRKTG
00313      PERFORM 9000-PRINT-LINE.                                     ELPPRKTG
00314      SKIP3                                                        ELPPRKTG
00315  9000-PRINT-LINE.                                                 ELPPRKTG
00316      SET PCB-PRINT-LINE             TO TRUE.                      ELPPRKTG
00317      INSPECT PCB-PRINT-AREA REPLACING ALL LOW-VALUE BY '_'.       ELPPRKTG
00318      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRKTG
