00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPPRKYP
00003  PROGRAM-ID.         ELPPRKY.                                        LV001
00004                                                                   ELPPRKYP
00005  AUTHOR.             ANNE KEFFER KING.                            ELPPRKYP
00006                                                                   ELPPRKYP
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPPRKYP
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPPRKYP
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPPRKYP
00010                      233 N. MICHIGAN AVE                          ELPPRKYP
00011                      CHICAGO, ILLINOIS 60601                      ELPPRKYP
00012                                                                   ELPPRKYP
00013  DATE-WRITTEN.       13-JUL-1989.                                 ELPPRKYP
00014                                                                   ELPPRKYP
00015  DATE-COMPILED.                                                   ELPPRKYP
00016                                                                   ELPPRKYP
00017  SECURITY.           COPYRIGHT 1988,                              ELPPRKYP
00018                      HEALTH CARE SERVICE CORPORATION              ELPPRKYP
00019      SKIP3                                                        ELPPRKYP
00020  ENVIRONMENT DIVISION.                                            ELPPRKYP
00021                                                                   ELPPRKYP
00022  CONFIGURATION SECTION.                                           ELPPRKYP
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELPPRKYP
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELPPRKYP
00025      EJECT                                                        ELPPRKYP
00026 ******************************************************************ELPPRKYP
00027 *                                                                *ELPPRKYP
00028 *                    ELS ABEND PROCESSING                        *ELPPRKYP
00029 *                                                                *ELPPRKYP
00030 *   ELPPRKY - THIS SUBROUTINE FORMATS AND PRINTS 'REPORT         *ELPPRKYP
00031 *             PAGE 6' OF THE ELS ABEND REPORTS.                  *ELPPRKYP
00032 *                                                                *ELPPRKYP
00033 ******************************************************************ELPPRKYP
00034 *                                                                *ELPPRKYP
00035 *                      MAINTENANCE HISTORY                       *ELPPRKYP
00036 *                                                                *ELPPRKYP
00037 *  MOD     DATE     BY  DRPT                ACTION               *ELPPRKYP
00038 * ----- ----------- --- ----- ---------------------------------- *ELPPRKYP
00039 * 01.00 13-JUL-1989 AKK       CREATED                            *ELPPRKYP
00040 *                                                                *ELPPRKYP
00041 * 01.01 01-OCT-1991 JPB       CHANGED GCK-FAM-REL-LVL AND GCK-   *ELPPRKYP
00042 *                             CTR-FAMREL FROM PIC X(01) TO       *ELPPRKYP
00043 *                             PIC X(02) FOR EXPANSION OF FAMILY  *ELPPRKYP
00044 *                             RELATIONSHIP INDICATOR.            *ELPPRKYP
00045 *                                                                *ELPPRKYP
00046 * 01.02 03-NOV-1997 AKK       ADDED SUPPORT FOR YEAR 2000 AND    *ELPPRKYP
00047 *                             TX MERGE.                          *ELPPRKYP
00048 *                                                                *ELPPRKYP
00049 ******************************************************************ELPPRKYP
00050                                                                   ELPPRKYP
00051  INPUT-OUTPUT SECTION.                                            ELPPRKYP
00052  FILE-CONTROL.                                                    ELPPRKYP
00053                                                                   ELPPRKYP
00054  DATA DIVISION.                                                   ELPPRKYP
00055  FILE SECTION.                                                    ELPPRKYP
00056 /                                                                 ELPPRKYP
00057  WORKING-STORAGE SECTION.                                         ELPPRKYP
00058  01  FILLER                 PIC X(16)   VALUE '*START ELPPRKY*'.  ELPPRKYP
00059 *                                                                 ELPPRKYP
00060  01  WS-WORK-AREA.                                                ELPPRKYP
00061      05  WS-CONTRACT-SUB           PIC S9(04) COMP.               ELPPRKYP
00062      05  WS-PRINT-SWITCH           PIC X(02)  VALUE 'NO'.         ELPPRKYP
00063          88 OK-TO-PRINT                       VALUE 'OK'.         ELPPRKYP
00064          88 NOT-OK-TO-PRINT                   VALUE 'NO'.         ELPPRKYP
00065 *                                                                 ELPPRKYP
00066 *IF RECORD ID IS ELSCINPB, PS, IB, IS OR ELSGRPSP AND IS USED TO  ELPPRKYP
00067 *PRINT WHAT IS REFERRED TO IN REPORT PAGE 6 IN THE DOCUMENTATION. ELPPRKYP
00068  01  GCK-OUTPUT-LINES.                                            ELPPRKYP
00069      03  GCK-HEADING-LINE1.                                       ELPPRKYP
00070          05  GCK-OUTPUT-LINE1-CC   PIC X(01)  VALUE '1'.          ELPPRKYP
00071          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKYP
00072          05  FILLER                PIC X(16)  VALUE               ELPPRKYP
00073              'GROUP RECORD KEY'.                                  ELPPRKYP
00074          05  FILLER                PIC X(115) VALUE SPACES.       ELPPRKYP
00075      03  GCK-DETAIL-LINE1.                                        ELPPRKYP
00076          05  GCK-OUTPUT-LINE2-CC   PIC X(01)  VALUE '0'.          ELPPRKYP
00077          05  FILLER                PIC X(07)  VALUE SPACES.       ELPPRKYP
00078          05  FILLER                PIC X(10)  VALUE 'PLAN CODE '. ELPPRKYP
00079          05  GCK-PLAN-CODE         PIC X(03)  VALUE SPACES.       ELPPRKYP
00080          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKYP
00081          05  FILLER                PIC X(12)  VALUE               ELPPRKYP
00082              'GROUP NUMBER'.                                      ELPPRKYP
00083          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKYP
00084          05  GROUP-SECTION-INFO.                                  ELPPRKYP
00085             10  GCK-GROUP-NUMBER      PIC X(09)  VALUE SPACES.    ELPPRKYP
00086             10  FILLER                PIC X(01)  VALUE SPACES.    ELPPRKYP
00087             10  FILLER                PIC X(07)  VALUE 'SECTION'. ELPPRKYP
00088             10  FILLER                PIC X(01)  VALUE SPACES.    ELPPRKYP
00089             10  GCK-SECTION-NUMBER    PIC X(05)  VALUE SPACES.    ELPPRKYP
00090             10  FILLER                PIC X(03)  VALUE SPACES.    ELPPRKYP
00091             10  FILLER             PIC X(09)  VALUE 'PKG CODE '.  ELPPRKYP
00092             10  GCK-PKG-CODE          PIC X(03)  VALUE SPACES.    ELPPRKYP
00093             10  FILLER                PIC X(02)  VALUE SPACES.    ELPPRKYP
00094             10  FILLER                PIC X(21)  VALUE            ELPPRKYP
00095                 'FAMILY RELATION LEVEL'.                          ELPPRKYP
00096             10  FILLER                PIC X(01)  VALUE SPACES.    ELPPRKYP
00097             10  GCK-FAM-REL-LVL       PIC X(02)  VALUE SPACES.    ELPPRKYP
00098             10  FILLER                PIC X(02)  VALUE SPACES.    ELPPRKYP
00099             10  FILLER                PIC X(14)  VALUE            ELPPRKYP
00100                 'EFFECTIVE DATE'.                                 ELPPRKYP
00101             10  FILLER                PIC X(01)  VALUE SPACES.    ELPPRKYP
00102             10  GCK-EFF-DATE          PIC 9(07)  VALUE ZEROES.    ELPPRKYP
00103             10  GCK-EFF-DATE-A REDEFINES GCK-EFF-DATE             ELPPRKYP
00104                                    PIC X(07).                     ELPPRKYP
00105              10 FILLER                PIC X(11)  VALUE SPACES.    ELPPRKYP
00106      03  GCK-HEADING-LINE3.                                       ELPPRKYP
00107          05  GCK-OUTPUT-LINE3-CC   PIC X(01)  VALUE '0'.          ELPPRKYP
00108          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKYP
00109          05  FILLER                PIC X(20)  VALUE               ELPPRKYP
00110              'CONTRACT RECORD KEYS'.                              ELPPRKYP
00111          05  FILLER                PIC X(111) VALUE SPACES.       ELPPRKYP
00112      03  GCK-HEADING-LINE4.                                       ELPPRKYP
00113          05  GCK-OUTPUT-LINE5-CC   PIC X(01)  VALUE ' '.          ELPPRKYP
00114          05  FILLER                PIC X(54)  VALUE SPACES.       ELPPRKYP
00115          05  FILLER                PIC X(03)  VALUE 'FAM'.        ELPPRKYP
00116          05  FILLER                PIC X(75)  VALUE SPACES.       ELPPRKYP
00117      03  GCK-HEADING-LINE5.                                       ELPPRKYP
00118          05  GCK-OUTPUT-LINE5-CC   PIC X(01)  VALUE ' '.          ELPPRKYP
00119          05  FILLER                PIC X(47)  VALUE SPACES.       ELPPRKYP
00120          05  FILLER                PIC X(04)  VALUE 'PROV'.       ELPPRKYP
00121          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKYP
00122          05  FILLER                PIC X(03)  VALUE 'REL'.        ELPPRKYP
00123          05  FILLER                PIC X(05)  VALUE SPACES.       ELPPRKYP
00124          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPPRKYP
00125          05  FILLER                PIC X(67)  VALUE SPACES.       ELPPRKYP
00126      03  GCK-HEADING-LINE6.                                       ELPPRKYP
00127          05  GCK-OUTPUT-LINE6-CC   PIC X(01)  VALUE ' '.          ELPPRKYP
00128          05  FILLER                PIC X(21)  VALUE SPACES.       ELPPRKYP
00129          05  FILLER                PIC X(09)  VALUE 'PLAN CODE'.  ELPPRKYP
00130          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKYP
00131          05  FILLER                PIC X(05)  VALUE 'GROUP'.      ELPPRKYP
00132          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKYP
00133          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKYP
00134          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKYP
00135          05  FILLER                PIC X(08)  VALUE 'PKG CODE'.   ELPPRKYP
00136          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKYP
00137          05  FILLER                PIC X(03)  VALUE 'LOB'.        ELPPRKYP
00138          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKYP
00139          05  FILLER                PIC X(04)  VALUE 'CNTL'.       ELPPRKYP
00140          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKYP
00141          05  FILLER                PIC X(03)  VALUE 'LVL'.        ELPPRKYP
00142          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKYP
00143          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRKYP
00144          05  FILLER                PIC X(43)  VALUE SPACES.       ELPPRKYP
00145      03  GCK-HEADING-LINE7.                                       ELPPRKYP
00146          05  GCK-OUTPUT-LINE7-CC   PIC X(01)  VALUE ' '.          ELPPRKYP
00147          05  FILLER                PIC X(21)  VALUE SPACES.       ELPPRKYP
00148          05  FILLER                PIC X(09)  VALUE '---------'.  ELPPRKYP
00149          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKYP
00150          05  FILLER                PIC X(06)  VALUE '------'.     ELPPRKYP
00151          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKYP
00152          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKYP
00153          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKYP
00154          05  FILLER                PIC X(08)  VALUE '--------'.   ELPPRKYP
00155          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKYP
00156          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKYP
00157          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKYP
00158          05  FILLER                PIC X(04)  VALUE '----'.       ELPPRKYP
00159          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKYP
00160          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKYP
00161          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKYP
00162          05  FILLER                PIC X(05)  VALUE '-----'.      ELPPRKYP
00163          05  FILLER                PIC X(67)  VALUE SPACES.       ELPPRKYP
00164      03  GCK-DETAIL-LINE2.                                        ELPPRKYP
00165          05  GCK-OUTPUT-LINE8-CC   PIC X(01)  VALUE ' '.          ELPPRKYP
00166          05  FILLER                PIC X(07)  VALUE SPACES.       ELPPRKYP
00167          05  FILLER                PIC X(11).                     ELPPRKYP
00168              88 INST-BASIC         VALUE 'INST BASIC'.            ELPPRKYP
00169              88 INST-SUPPL         VALUE 'INST SUPPL'.            ELPPRKYP
00170              88 PROF-BASIC         VALUE 'PROF BASIC'.            ELPPRKYP
00171              88 PROF-SUPPL         VALUE 'PROF SUPPL'.            ELPPRKYP
00172          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKYP
00173          05  GCK-CTR-KEY-INFO.                                    ELPPRKYP
00174              10  GCK-CTR-PLAN-CODE     PIC X(03)  VALUE SPACES.   ELPPRKYP
00175              10  FILLER                PIC X(05)  VALUE SPACES.   ELPPRKYP
00176              10  GCK-CTR-GROUP         PIC X(09)  VALUE SPACES.   ELPPRKYP
00177              10  FILLER                PIC X(02)  VALUE SPACES.   ELPPRKYP
00178              10  GCK-CTR-SECTION       PIC X(05)  VALUE SPACES.   ELPPRKYP
00179              10  FILLER                PIC X(07)  VALUE SPACES.   ELPPRKYP
00180              10  GCK-CTR-PKG-CODE      PIC X(03)  VALUE SPACES.   ELPPRKYP
00181              10  FILLER                PIC X(07)  VALUE SPACES.   ELPPRKYP
00182              10  GCK-CTR-LOB           PIC X(01)  VALUE SPACES.   ELPPRKYP
00183              10  FILLER                PIC X(04)  VALUE SPACES.   ELPPRKYP
00184              10  GCK-CTR-PROVCTL       PIC X(02)  VALUE SPACES.   ELPPRKYP
00185              10  FILLER                PIC X(05)  VALUE SPACES.   ELPPRKYP
00186              10  GCK-CTR-FAMREL        PIC X(02)  VALUE SPACES.   ELPPRKYP
00187              10  FILLER                PIC X(03)  VALUE SPACES.   ELPPRKYP
00188              10  GCK-CTR-EFF-DATE      PIC 9(07)  VALUE ZEROES.   ELPPRKYP
00189              10  GCK-CTR-EFF-DATE-A REDEFINES GCK-CTR-EFF-DATE    ELPPRKYP
00190                                    PIC X(07).                     ELPPRKYP
00191          05  FILLER                PIC X(45)  VALUE SPACES.       ELPPRKYP
00192                                                                   ELPPRKYP
00193  01  FILLER                 PIC X(14)   VALUE '*END ELPPRKY*'.    ELPPRKYP
00194 /                                                                 ELPPRKYP
00195  LINKAGE SECTION.                                                 ELPPRKYP
00196      COPY ELSPRCBC.                                               ELPPRKYP
00197 /                                                                 ELPPRKYP
00198      COPY ELSPRKYC.                                               ELPPRKYP
00199 /                                                                 ELPPRKYP
00200  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK                 ELPPRKYP
00201                           KYA-KEY-HOLD-AREA.                      ELPPRKYP
00202      PERFORM 0000-INITIALIZATION.                                 ELPPRKYP
00203      PERFORM 1000-DO-PRINT-GRPCONTR-LINES.                        ELPPRKYP
00204      GOBACK.                                                      ELPPRKYP
00205 *                                                                 ELPPRKYP
00206  0000-INITIALIZATION.                                             ELPPRKYP
00207      MOVE SPACES TO PCB-PRINT-TEXT.                               ELPPRKYP
00208      MOVE ZERO TO GCK-EFF-DATE.                                   ELPPRKYP
00209 *                                                                 ELPPRKYP
00210  1000-DO-PRINT-GRPCONTR-LINES.                                    ELPPRKYP
00211      IF KYA-GSK-PRINT                                             ELPPRKYP
00212         PERFORM 1025-PRINT-GRPKEY                                 ELPPRKYP
00213         PERFORM 1075-PRINT-CONTRACT                               ELPPRKYP
00214            VARYING WS-CONTRACT-SUB FROM 1 BY 1                    ELPPRKYP
00215             UNTIL WS-CONTRACT-SUB > 4                             ELPPRKYP
00216      ELSE                                                         ELPPRKYP
00217         PERFORM VARYING WS-CONTRACT-SUB                           ELPPRKYP
00218            FROM 1 BY 1 UNTIL WS-CONTRACT-SUB > 4                  ELPPRKYP
00219              OR (KYA-CK-PRINT (WS-CONTRACT-SUB))                  ELPPRKYP
00220              IF KYA-CK-PRINT (WS-CONTRACT-SUB)                    ELPPRKYP
00221                PERFORM 1025-PRINT-GRPKEY                          ELPPRKYP
00222                PERFORM 1075-PRINT-CONTRACT                        ELPPRKYP
00223                   VARYING WS-CONTRACT-SUB FROM 1 BY 1             ELPPRKYP
00224                       UNTIL WS-CONTRACT-SUB > 4                   ELPPRKYP
00225              END-IF                                               ELPPRKYP
00226           END-PERFORM.                                            ELPPRKYP
00227 *                                                                 ELPPRKYP
00228  1025-PRINT-GRPKEY.                                               ELPPRKYP
00229      MOVE GCK-HEADING-LINE1 TO PCB-PRINT-AREA.                    ELPPRKYP
00230      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKYP
00231      MOVE KYA-GSK-PKG-CODE TO GCK-PKG-CODE.                       ELPPRKYP
00232      MOVE KYA-GSK-GROUP TO GCK-GROUP-NUMBER.                      ELPPRKYP
00233      MOVE KYA-GSK-SECT TO GCK-SECTION-NUMBER.                     ELPPRKYP
00234      MOVE KYA-GSK-PKG-CODE TO GCK-PKG-CODE.                       ELPPRKYP
00235      MOVE KYA-GSK-FAM-REL-LVL TO GCK-FAM-REL-LVL.                 ELPPRKYP
00236      PERFORM 1035-VALIDATE-GRP-EFF-DATE.                          ELPPRKYP
00237      MOVE GCK-DETAIL-LINE1 TO PCB-PRINT-AREA.                     ELPPRKYP
00238      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKYP
00239 *                                                                 ELPPRKYP
00240  1035-VALIDATE-GRP-EFF-DATE.                                      ELPPRKYP
00241      IF KYA-GSK-EFF-DT-CENTURY IS NUMERIC                         ELPPRKYP
00242          MOVE KYA-GSK-EFF-DT-CENTURY            TO GCK-EFF-DATE   ELPPRKYP
00243      ELSE                                                         ELPPRKYP
00244          MOVE ALL '?' TO GCK-EFF-DATE-A                           ELPPRKYP
00245          SET PCB-INVALID-DATA-FOUND TO TRUE.                      ELPPRKYP
00246 *                                                                 ELPPRKYP
00247  1075-PRINT-CONTRACT.                                             ELPPRKYP
00248      MOVE SPACES TO GCK-CTR-KEY-INFO.                             ELPPRKYP
00249      IF NOT KYA-GSK-PRINT                                         ELPPRKYP
00250          PERFORM 1025-PRINT-GRPKEY.                               ELPPRKYP
00251      SET KYA-IDX TO WS-CONTRACT-SUB.                              ELPPRKYP
00252      PERFORM 1080-SELECT-LOB.                                     ELPPRKYP
00253      IF INST-BASIC OR (PCB-CURRENT-LINE > PCB-MAX-LINES)          ELPPRKYP
00254         PERFORM 1079-PRINT-CONTRACT-HDG.                          ELPPRKYP
00255      IF KYA-CONTRACT-KEYS (KYA-IDX) = SPACES                      ELPPRKYP
00256         MOVE SPACES TO GCK-CTR-KEY-INFO                           ELPPRKYP
00257      ELSE                                                         ELPPRKYP
00258         MOVE KYA-CK-PLAN-CODE (KYA-IDX) TO GCK-CTR-PLAN-CODE      ELPPRKYP
00259         MOVE KYA-CK-GRP (KYA-IDX) TO GCK-CTR-GROUP                ELPPRKYP
00260         MOVE KYA-CK-SECT (KYA-IDX) TO GCK-CTR-SECTION             ELPPRKYP
00261         MOVE KYA-CK-PKG-CODE (KYA-IDX) TO GCK-CTR-PKG-CODE        ELPPRKYP
00262         MOVE KYA-CK-L-O-B (KYA-IDX) TO GCK-CTR-LOB                ELPPRKYP
00263         MOVE KYA-CK-PROVDR-CONTROL (KYA-IDX)                      ELPPRKYP
00264                          TO GCK-CTR-PROVCTL                       ELPPRKYP
00265         MOVE KYA-CK-FAM-REL-LVL (KYA-IDX) TO GCK-CTR-FAMREL       ELPPRKYP
00266         PERFORM 1085-VALIDATE-CONTR-EFF-DATE.                     ELPPRKYP
00267      MOVE GCK-DETAIL-LINE2 TO PCB-PRINT-AREA.                     ELPPRKYP
00268      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKYP
00269 *                                                                 ELPPRKYP
00270  1079-PRINT-CONTRACT-HDG.                                         ELPPRKYP
00271      MOVE GCK-HEADING-LINE3 TO PCB-PRINT-AREA.                    ELPPRKYP
00272      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKYP
00273      MOVE GCK-HEADING-LINE4 TO PCB-PRINT-AREA.                    ELPPRKYP
00274      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKYP
00275      MOVE GCK-HEADING-LINE5 TO PCB-PRINT-AREA.                    ELPPRKYP
00276      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKYP
00277      MOVE GCK-HEADING-LINE6 TO PCB-PRINT-AREA.                    ELPPRKYP
00278      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKYP
00279      MOVE GCK-HEADING-LINE7 TO PCB-PRINT-AREA.                    ELPPRKYP
00280      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKYP
00281                                                                   ELPPRKYP
00282  1080-SELECT-LOB.                                                 ELPPRKYP
00283       IF WS-CONTRACT-SUB = 1                                      ELPPRKYP
00284          SET INST-BASIC TO TRUE                                   ELPPRKYP
00285       ELSE                                                        ELPPRKYP
00286          IF WS-CONTRACT-SUB = 2                                   ELPPRKYP
00287             SET INST-SUPPL TO TRUE                                ELPPRKYP
00288          ELSE                                                     ELPPRKYP
00289             IF WS-CONTRACT-SUB = 3                                ELPPRKYP
00290                SET PROF-BASIC TO TRUE                             ELPPRKYP
00291             ELSE                                                  ELPPRKYP
00292                IF WS-CONTRACT-SUB = 4                             ELPPRKYP
00293                   SET PROF-SUPPL TO TRUE.                         ELPPRKYP
00294 *                                                                 ELPPRKYP
00295  1085-VALIDATE-CONTR-EFF-DATE.                                    ELPPRKYP
00296      IF KYA-CK-EFF-DATE (KYA-IDX) IS NUMERIC                      ELPPRKYP
00297          MOVE KYA-CK-EFF-DT-CENTURY(KYA-IDX) TO GCK-CTR-EFF-DATE  ELPPRKYP
00298      ELSE                                                         ELPPRKYP
00299          MOVE ALL '?' TO GCK-CTR-EFF-DATE-A                       ELPPRKYP
00300          SET PCB-INVALID-DATA-FOUND TO TRUE.                      ELPPRKYP
00301 *                                                                 ELPPRKYP
00302  8000-DO-THE-PRINT.                                               ELPPRKYP
00303      SET PCB-PRINT-LINE TO TRUE.                                  ELPPRKYP
00304      INSPECT PCB-PRINT-AREA REPLACING ALL LOW-VALUES BY '_'.      ELPPRKYP
00305      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRKYP
00306 *                                                                 ELPPRKYP
