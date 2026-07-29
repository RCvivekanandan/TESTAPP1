00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPPRKY 
00003  PROGRAM-ID.         ELPPRKY.                                        LV001
00004                                                                   ELPPRKY 
00005  AUTHOR.             ANNE KEFFER KING.                            ELPPRKY 
00006                                                                   ELPPRKY 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPPRKY 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPPRKY 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPPRKY 
00010                      233 N. MICHIGAN AVE                          ELPPRKY 
00011                      CHICAGO, ILLINOIS 60601                      ELPPRKY 
00012                                                                   ELPPRKY 
00013  DATE-WRITTEN.       13-JUL-1989.                                 ELPPRKY 
00014                                                                   ELPPRKY 
00015  DATE-COMPILED.                                                   ELPPRKY 
00016                                                                   ELPPRKY 
00017  SECURITY.           COPYRIGHT 1988,                              ELPPRKY 
00018                      HEALTH CARE SERVICE CORPORATION              ELPPRKY 
00019      SKIP3                                                        ELPPRKY 
00020  ENVIRONMENT DIVISION.                                            ELPPRKY 
00021                                                                   ELPPRKY 
00022  CONFIGURATION SECTION.                                           ELPPRKY 
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELPPRKY 
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELPPRKY 
00025      EJECT                                                        ELPPRKY 
00026 ******************************************************************ELPPRKY 
00027 *                                                                *ELPPRKY 
00028 *                    ELS ABEND PROCESSING                        *ELPPRKY 
00029 *                                                                *ELPPRKY 
00030 *   ELPPRKY - THIS SUBROUTINE FORMATS AND PRINTS 'REPORT         *ELPPRKY 
00031 *             PAGE 6' OF THE ELS ABEND REPORTS.                  *ELPPRKY 
00032 *                                                                *ELPPRKY 
00033 ******************************************************************ELPPRKY 
00034 *                                                                *ELPPRKY 
00035 *                      MAINTENANCE HISTORY                       *ELPPRKY 
00036 *                                                                *ELPPRKY 
00037 *  MOD     DATE     BY  DRPT                ACTION               *ELPPRKY 
00038 * ----- ----------- --- ----- ---------------------------------- *ELPPRKY 
00039 * 01.00 13-JUL-1989 AKK       CREATED                            *ELPPRKY 
00040 *                                                                *ELPPRKY 
00041 * 01.01 01-OCT-1991 JPB       CHANGED GCK-FAM-REL-LVL AND GCK-   *ELPPRKY 
00042 *                             CTR-FAMREL FROM PIC X(01) TO       *ELPPRKY 
00043 *                             PIC X(02) FOR EXPANSION OF FAMILY  *ELPPRKY 
00044 *                             RELATIONSHIP INDICATOR.            *ELPPRKY 
00045 *                                                                *ELPPRKY 
00046 * 01.02 03-NOV-1997 AKK       ADDED SUPPORT FOR YEAR 2000 AND    *ELPPRKY 
00047 *                             TX MERGE.                          *ELPPRKY 
00048 *                                                                *ELPPRKY 
00049 ******************************************************************ELPPRKY 
00050                                                                   ELPPRKY 
00051  INPUT-OUTPUT SECTION.                                            ELPPRKY 
00052  FILE-CONTROL.                                                    ELPPRKY 
00053                                                                   ELPPRKY 
00054  DATA DIVISION.                                                   ELPPRKY 
00055  FILE SECTION.                                                    ELPPRKY 
00056 /                                                                 ELPPRKY 
00057  WORKING-STORAGE SECTION.                                         ELPPRKY 
00058  01  FILLER                 PIC X(16)   VALUE '*START ELPPRKY*'.  ELPPRKY 
00059 *                                                                 ELPPRKY 
00060  01  WS-WORK-AREA.                                                ELPPRKY 
00061      05  WS-CONTRACT-SUB           PIC S9(04) COMP.               ELPPRKY 
00062      05  WS-PRINT-SWITCH           PIC X(02)  VALUE 'NO'.         ELPPRKY 
00063          88 OK-TO-PRINT                       VALUE 'OK'.         ELPPRKY 
00064          88 NOT-OK-TO-PRINT                   VALUE 'NO'.         ELPPRKY 
00065 *                                                                 ELPPRKY 
00066 *IF RECORD ID IS ELSCINPB, PS, IB, IS OR ELSGRPSP AND IS USED TO  ELPPRKY 
00067 *PRINT WHAT IS REFERRED TO IN REPORT PAGE 6 IN THE DOCUMENTATION. ELPPRKY 
00068  01  GCK-OUTPUT-LINES.                                            ELPPRKY 
00069      03  GCK-HEADING-LINE1.                                       ELPPRKY 
00070          05  GCK-OUTPUT-LINE1-CC   PIC X(01)  VALUE '1'.          ELPPRKY 
00071          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKY 
00072          05  FILLER                PIC X(16)  VALUE               ELPPRKY 
00073              'GROUP RECORD KEY'.                                  ELPPRKY 
00074          05  FILLER                PIC X(115) VALUE SPACES.       ELPPRKY 
00075      03  GCK-DETAIL-LINE1.                                        ELPPRKY 
00076          05  GCK-OUTPUT-LINE2-CC   PIC X(01)  VALUE '0'.          ELPPRKY 
00077          05  FILLER                PIC X(07)  VALUE SPACES.       ELPPRKY 
00078          05  FILLER                PIC X(10)  VALUE 'PLAN CODE '. ELPPRKY 
00079          05  GCK-PLAN-CODE         PIC X(03)  VALUE SPACES.       ELPPRKY 
00080          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKY 
00081          05  FILLER                PIC X(12)  VALUE               ELPPRKY 
00082              'GROUP NUMBER'.                                      ELPPRKY 
00083          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKY 
00084          05  GROUP-SECTION-INFO.                                  ELPPRKY 
00085             10  GCK-GROUP-NUMBER      PIC X(09)  VALUE SPACES.    ELPPRKY 
00086             10  FILLER                PIC X(01)  VALUE SPACES.    ELPPRKY 
00087             10  FILLER                PIC X(07)  VALUE 'SECTION'. ELPPRKY 
00088             10  FILLER                PIC X(01)  VALUE SPACES.    ELPPRKY 
00089             10  GCK-SECTION-NUMBER    PIC X(05)  VALUE SPACES.    ELPPRKY 
00090             10  FILLER                PIC X(03)  VALUE SPACES.    ELPPRKY 
00091             10  FILLER             PIC X(09)  VALUE 'PKG CODE '.  ELPPRKY 
00092             10  GCK-PKG-CODE          PIC X(03)  VALUE SPACES.    ELPPRKY 
00093             10  FILLER                PIC X(02)  VALUE SPACES.    ELPPRKY 
00094             10  FILLER                PIC X(21)  VALUE            ELPPRKY 
00095                 'FAMILY RELATION LEVEL'.                          ELPPRKY 
00096             10  FILLER                PIC X(01)  VALUE SPACES.    ELPPRKY 
00097             10  GCK-FAM-REL-LVL       PIC X(02)  VALUE SPACES.    ELPPRKY 
00098             10  FILLER                PIC X(02)  VALUE SPACES.    ELPPRKY 
00099             10  FILLER                PIC X(14)  VALUE            ELPPRKY 
00100                 'EFFECTIVE DATE'.                                 ELPPRKY 
00101             10  FILLER                PIC X(01)  VALUE SPACES.    ELPPRKY 
00102             10  GCK-EFF-DATE          PIC 9(07)  VALUE ZEROES.    ELPPRKY 
00103             10  GCK-EFF-DATE-A REDEFINES GCK-EFF-DATE             ELPPRKY 
00104                                    PIC X(07).                     ELPPRKY 
00105              10 FILLER                PIC X(11)  VALUE SPACES.    ELPPRKY 
00106      03  GCK-HEADING-LINE3.                                       ELPPRKY 
00107          05  GCK-OUTPUT-LINE3-CC   PIC X(01)  VALUE '0'.          ELPPRKY 
00108          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRKY 
00109          05  FILLER                PIC X(20)  VALUE               ELPPRKY 
00110              'CONTRACT RECORD KEYS'.                              ELPPRKY 
00111          05  FILLER                PIC X(111) VALUE SPACES.       ELPPRKY 
00112      03  GCK-HEADING-LINE4.                                       ELPPRKY 
00113          05  GCK-OUTPUT-LINE5-CC   PIC X(01)  VALUE ' '.          ELPPRKY 
00114          05  FILLER                PIC X(54)  VALUE SPACES.       ELPPRKY 
00115          05  FILLER                PIC X(03)  VALUE 'FAM'.        ELPPRKY 
00116          05  FILLER                PIC X(75)  VALUE SPACES.       ELPPRKY 
00117      03  GCK-HEADING-LINE5.                                       ELPPRKY 
00118          05  GCK-OUTPUT-LINE5-CC   PIC X(01)  VALUE ' '.          ELPPRKY 
00119          05  FILLER                PIC X(47)  VALUE SPACES.       ELPPRKY 
00120          05  FILLER                PIC X(04)  VALUE 'PROV'.       ELPPRKY 
00121          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKY 
00122          05  FILLER                PIC X(03)  VALUE 'REL'.        ELPPRKY 
00123          05  FILLER                PIC X(05)  VALUE SPACES.       ELPPRKY 
00124          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPPRKY 
00125          05  FILLER                PIC X(67)  VALUE SPACES.       ELPPRKY 
00126      03  GCK-HEADING-LINE6.                                       ELPPRKY 
00127          05  GCK-OUTPUT-LINE6-CC   PIC X(01)  VALUE ' '.          ELPPRKY 
00128          05  FILLER                PIC X(21)  VALUE SPACES.       ELPPRKY 
00129          05  FILLER                PIC X(09)  VALUE 'PLAN CODE'.  ELPPRKY 
00130          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKY 
00131          05  FILLER                PIC X(05)  VALUE 'GROUP'.      ELPPRKY 
00132          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKY 
00133          05  FILLER                PIC X(07)  VALUE 'SECTION'.    ELPPRKY 
00134          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKY 
00135          05  FILLER                PIC X(08)  VALUE 'PKG CODE'.   ELPPRKY 
00136          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKY 
00137          05  FILLER                PIC X(03)  VALUE 'LOB'.        ELPPRKY 
00138          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKY 
00139          05  FILLER                PIC X(04)  VALUE 'CNTL'.       ELPPRKY 
00140          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKY 
00141          05  FILLER                PIC X(03)  VALUE 'LVL'.        ELPPRKY 
00142          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKY 
00143          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRKY 
00144          05  FILLER                PIC X(43)  VALUE SPACES.       ELPPRKY 
00145      03  GCK-HEADING-LINE7.                                       ELPPRKY 
00146          05  GCK-OUTPUT-LINE7-CC   PIC X(01)  VALUE ' '.          ELPPRKY 
00147          05  FILLER                PIC X(21)  VALUE SPACES.       ELPPRKY 
00148          05  FILLER                PIC X(09)  VALUE '---------'.  ELPPRKY 
00149          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKY 
00150          05  FILLER                PIC X(06)  VALUE '------'.     ELPPRKY 
00151          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKY 
00152          05  FILLER                PIC X(07)  VALUE '-------'.    ELPPRKY 
00153          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKY 
00154          05  FILLER                PIC X(08)  VALUE '--------'.   ELPPRKY 
00155          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKY 
00156          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKY 
00157          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKY 
00158          05  FILLER                PIC X(04)  VALUE '----'.       ELPPRKY 
00159          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKY 
00160          05  FILLER                PIC X(03)  VALUE '---'.        ELPPRKY 
00161          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRKY 
00162          05  FILLER                PIC X(05)  VALUE '-----'.      ELPPRKY 
00163          05  FILLER                PIC X(67)  VALUE SPACES.       ELPPRKY 
00164      03  GCK-DETAIL-LINE2.                                        ELPPRKY 
00165          05  GCK-OUTPUT-LINE8-CC   PIC X(01)  VALUE ' '.          ELPPRKY 
00166          05  FILLER                PIC X(07)  VALUE SPACES.       ELPPRKY 
00167          05  FILLER                PIC X(11).                     ELPPRKY 
00168              88 INST-BASIC         VALUE 'INST BASIC'.            ELPPRKY 
00169              88 INST-SUPPL         VALUE 'INST SUPPL'.            ELPPRKY 
00170              88 PROF-BASIC         VALUE 'PROF BASIC'.            ELPPRKY 
00171              88 PROF-SUPPL         VALUE 'PROF SUPPL'.            ELPPRKY 
00172          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRKY 
00173          05  GCK-CTR-KEY-INFO.                                    ELPPRKY 
00174              10  GCK-CTR-PLAN-CODE     PIC X(03)  VALUE SPACES.   ELPPRKY 
00175              10  FILLER                PIC X(05)  VALUE SPACES.   ELPPRKY 
00176              10  GCK-CTR-GROUP         PIC X(09)  VALUE SPACES.   ELPPRKY 
00177              10  FILLER                PIC X(02)  VALUE SPACES.   ELPPRKY 
00178              10  GCK-CTR-SECTION       PIC X(05)  VALUE SPACES.   ELPPRKY 
00179              10  FILLER                PIC X(07)  VALUE SPACES.   ELPPRKY 
00180              10  GCK-CTR-PKG-CODE      PIC X(03)  VALUE SPACES.   ELPPRKY 
00181              10  FILLER                PIC X(07)  VALUE SPACES.   ELPPRKY 
00182              10  GCK-CTR-LOB           PIC X(01)  VALUE SPACES.   ELPPRKY 
00183              10  FILLER                PIC X(04)  VALUE SPACES.   ELPPRKY 
00184              10  GCK-CTR-PROVCTL       PIC X(02)  VALUE SPACES.   ELPPRKY 
00185              10  FILLER                PIC X(05)  VALUE SPACES.   ELPPRKY 
00186              10  GCK-CTR-FAMREL        PIC X(02)  VALUE SPACES.   ELPPRKY 
00187              10  FILLER                PIC X(03)  VALUE SPACES.   ELPPRKY 
00188              10  GCK-CTR-EFF-DATE      PIC 9(07)  VALUE ZEROES.   ELPPRKY 
00189              10  GCK-CTR-EFF-DATE-A REDEFINES GCK-CTR-EFF-DATE    ELPPRKY 
00190                                    PIC X(07).                     ELPPRKY 
00191          05  FILLER                PIC X(45)  VALUE SPACES.       ELPPRKY 
00192                                                                   ELPPRKY 
00193  01  FILLER                 PIC X(14)   VALUE '*END ELPPRKY*'.    ELPPRKY 
00194 /                                                                 ELPPRKY 
00195  LINKAGE SECTION.                                                 ELPPRKY 
00196      COPY ELSPRCBC.                                               ELPPRKY 
00197 /                                                                 ELPPRKY 
00198      COPY ELSPRKYC.                                               ELPPRKY 
00199 /                                                                 ELPPRKY 
00200  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK                 ELPPRKY 
00201                           KYA-KEY-HOLD-AREA.                      ELPPRKY 
00202      PERFORM 0000-INITIALIZATION.                                 ELPPRKY 
00203      PERFORM 1000-DO-PRINT-GRPCONTR-LINES.                        ELPPRKY 
00204      GOBACK.                                                      ELPPRKY 
00205 *                                                                 ELPPRKY 
00206  0000-INITIALIZATION.                                             ELPPRKY 
00207      MOVE SPACES TO PCB-PRINT-TEXT.                               ELPPRKY 
00208      MOVE ZERO TO GCK-EFF-DATE.                                   ELPPRKY 
00209 *                                                                 ELPPRKY 
00210  1000-DO-PRINT-GRPCONTR-LINES.                                    ELPPRKY 
00211      IF KYA-GSK-PRINT                                             ELPPRKY 
00212         PERFORM 1025-PRINT-GRPKEY                                 ELPPRKY 
00213         PERFORM 1075-PRINT-CONTRACT                               ELPPRKY 
00214            VARYING WS-CONTRACT-SUB FROM 1 BY 1                    ELPPRKY 
00215             UNTIL WS-CONTRACT-SUB > 4                             ELPPRKY 
00216      ELSE                                                         ELPPRKY 
00217         PERFORM VARYING WS-CONTRACT-SUB                           ELPPRKY 
00218            FROM 1 BY 1 UNTIL WS-CONTRACT-SUB > 4                  ELPPRKY 
00219              OR (KYA-CK-PRINT (WS-CONTRACT-SUB))                  ELPPRKY 
00220              IF KYA-CK-PRINT (WS-CONTRACT-SUB)                    ELPPRKY 
00221                PERFORM 1025-PRINT-GRPKEY                          ELPPRKY 
00222                PERFORM 1075-PRINT-CONTRACT                        ELPPRKY 
00223                   VARYING WS-CONTRACT-SUB FROM 1 BY 1             ELPPRKY 
00224                       UNTIL WS-CONTRACT-SUB > 4                   ELPPRKY 
00225              END-IF                                               ELPPRKY 
00226           END-PERFORM.                                            ELPPRKY 
00227 *                                                                 ELPPRKY 
00228  1025-PRINT-GRPKEY.                                               ELPPRKY 
00229      MOVE GCK-HEADING-LINE1 TO PCB-PRINT-AREA.                    ELPPRKY 
00230      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKY 
00231      MOVE KYA-GSK-PKG-CODE TO GCK-PKG-CODE.                       ELPPRKY 
00232      MOVE KYA-GSK-GROUP TO GCK-GROUP-NUMBER.                      ELPPRKY 
00233      MOVE KYA-GSK-SECT TO GCK-SECTION-NUMBER.                     ELPPRKY 
00234      MOVE KYA-GSK-PKG-CODE TO GCK-PKG-CODE.                       ELPPRKY 
00235      MOVE KYA-GSK-FAM-REL-LVL TO GCK-FAM-REL-LVL.                 ELPPRKY 
00236      PERFORM 1035-VALIDATE-GRP-EFF-DATE.                          ELPPRKY 
00237      MOVE GCK-DETAIL-LINE1 TO PCB-PRINT-AREA.                     ELPPRKY 
00238      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKY 
00239 *                                                                 ELPPRKY 
00240  1035-VALIDATE-GRP-EFF-DATE.                                      ELPPRKY 
00241      IF KYA-GSK-EFF-DT-CENTURY IS NUMERIC                         ELPPRKY 
00242          MOVE KYA-GSK-EFF-DT-CENTURY            TO GCK-EFF-DATE   ELPPRKY 
00243      ELSE                                                         ELPPRKY 
00244          MOVE ALL '?' TO GCK-EFF-DATE-A                           ELPPRKY 
00245          SET PCB-INVALID-DATA-FOUND TO TRUE.                      ELPPRKY 
00246 *                                                                 ELPPRKY 
00247  1075-PRINT-CONTRACT.                                             ELPPRKY 
00248      MOVE SPACES TO GCK-CTR-KEY-INFO.                             ELPPRKY 
00249      IF NOT KYA-GSK-PRINT                                         ELPPRKY 
00250          PERFORM 1025-PRINT-GRPKEY.                               ELPPRKY 
00251      SET KYA-IDX TO WS-CONTRACT-SUB.                              ELPPRKY 
00252      PERFORM 1080-SELECT-LOB.                                     ELPPRKY 
00253      IF INST-BASIC OR (PCB-CURRENT-LINE > PCB-MAX-LINES)          ELPPRKY 
00254         PERFORM 1079-PRINT-CONTRACT-HDG.                          ELPPRKY 
00255      IF KYA-CONTRACT-KEYS (KYA-IDX) = SPACES                      ELPPRKY 
00256         MOVE SPACES TO GCK-CTR-KEY-INFO                           ELPPRKY 
00257      ELSE                                                         ELPPRKY 
00258         MOVE KYA-CK-PLAN-CODE (KYA-IDX) TO GCK-CTR-PLAN-CODE      ELPPRKY 
00259         MOVE KYA-CK-GRP (KYA-IDX) TO GCK-CTR-GROUP                ELPPRKY 
00260         MOVE KYA-CK-SECT (KYA-IDX) TO GCK-CTR-SECTION             ELPPRKY 
00261         MOVE KYA-CK-PKG-CODE (KYA-IDX) TO GCK-CTR-PKG-CODE        ELPPRKY 
00262         MOVE KYA-CK-L-O-B (KYA-IDX) TO GCK-CTR-LOB                ELPPRKY 
00263         MOVE KYA-CK-PROVDR-CONTROL (KYA-IDX)                      ELPPRKY 
00264                          TO GCK-CTR-PROVCTL                       ELPPRKY 
00265         MOVE KYA-CK-FAM-REL-LVL (KYA-IDX) TO GCK-CTR-FAMREL       ELPPRKY 
00266         PERFORM 1085-VALIDATE-CONTR-EFF-DATE.                     ELPPRKY 
00267      MOVE GCK-DETAIL-LINE2 TO PCB-PRINT-AREA.                     ELPPRKY 
00268      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKY 
00269 *                                                                 ELPPRKY 
00270  1079-PRINT-CONTRACT-HDG.                                         ELPPRKY 
00271      MOVE GCK-HEADING-LINE3 TO PCB-PRINT-AREA.                    ELPPRKY 
00272      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKY 
00273      MOVE GCK-HEADING-LINE4 TO PCB-PRINT-AREA.                    ELPPRKY 
00274      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKY 
00275      MOVE GCK-HEADING-LINE5 TO PCB-PRINT-AREA.                    ELPPRKY 
00276      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKY 
00277      MOVE GCK-HEADING-LINE6 TO PCB-PRINT-AREA.                    ELPPRKY 
00278      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKY 
00279      MOVE GCK-HEADING-LINE7 TO PCB-PRINT-AREA.                    ELPPRKY 
00280      PERFORM 8000-DO-THE-PRINT.                                   ELPPRKY 
00281                                                                   ELPPRKY 
00282  1080-SELECT-LOB.                                                 ELPPRKY 
00283       IF WS-CONTRACT-SUB = 1                                      ELPPRKY 
00284          SET INST-BASIC TO TRUE                                   ELPPRKY 
00285       ELSE                                                        ELPPRKY 
00286          IF WS-CONTRACT-SUB = 2                                   ELPPRKY 
00287             SET INST-SUPPL TO TRUE                                ELPPRKY 
00288          ELSE                                                     ELPPRKY 
00289             IF WS-CONTRACT-SUB = 3                                ELPPRKY 
00290                SET PROF-BASIC TO TRUE                             ELPPRKY 
00291             ELSE                                                  ELPPRKY 
00292                IF WS-CONTRACT-SUB = 4                             ELPPRKY 
00293                   SET PROF-SUPPL TO TRUE.                         ELPPRKY 
00294 *                                                                 ELPPRKY 
00295  1085-VALIDATE-CONTR-EFF-DATE.                                    ELPPRKY 
00296      IF KYA-CK-EFF-DATE (KYA-IDX) IS NUMERIC                      ELPPRKY 
00297          MOVE KYA-CK-EFF-DT-CENTURY(KYA-IDX) TO GCK-CTR-EFF-DATE  ELPPRKY 
00298      ELSE                                                         ELPPRKY 
00299          MOVE ALL '?' TO GCK-CTR-EFF-DATE-A                       ELPPRKY 
00300          SET PCB-INVALID-DATA-FOUND TO TRUE.                      ELPPRKY 
00301 *                                                                 ELPPRKY 
00302  8000-DO-THE-PRINT.                                               ELPPRKY 
00303      SET PCB-PRINT-LINE TO TRUE.                                  ELPPRKY 
00304      INSPECT PCB-PRINT-AREA REPLACING ALL LOW-VALUES BY '_'.      ELPPRKY 
00305      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRKY 
00306 *                                                                 ELPPRKY 
