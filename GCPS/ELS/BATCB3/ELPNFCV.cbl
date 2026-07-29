00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPNFCV 
00003  PROGRAM-ID.         ELPNFCV.                                        LV001
00004                                                                   ELPNFCV 
00005  AUTHOR.             ANNE KEFFER KING.                            ELPNFCV 
00006                                                                   ELPNFCV 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPNFCV 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPNFCV 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPNFCV 
00010                      233 N. MICHIGAN AVE                          ELPNFCV 
00011                      CHICAGO, ILLINOIS 60601                      ELPNFCV 
00012                                                                   ELPNFCV 
00013  DATE-WRITTEN.       20-MAR-1990.                                 ELPNFCV 
00014                                                                   ELPNFCV 
00015  DATE-COMPILED.                                                   ELPNFCV 
00016                                                                   ELPNFCV 
00017  SECURITY.           COPYRIGHT 1989,                              ELPNFCV 
00018                      HEALTH CARE SERVICE CORPORATION              ELPNFCV 
00019  TITLE 'ELS ABEND PROCESSING - PRINT CODE VALUE EXCEPTION REPORT'.ELPNFCV 
00020 ******************************************************************ELPNFCV 
00021 *                                                                *ELPNFCV 
00022 *                    ELS ABEND PROCESSING                        *ELPNFCV 
00023 *                                                                *ELPNFCV 
00024 *   THIS SUBROUTINE PRINTS THE CODE VALUE EXCEPTION REPORT.      *ELPNFCV 
00025 *   ONE CALL WILL PRINT THE ENTIRE EXCEPTION REPORT.             *ELPNFCV 
00026 *                                                                *ELPNFCV 
00027 *   THE INPUT CONSISTS OF ONLY RECORDS EXTRACTED FROM THE        *ELPNFCV 
00028 *   ELS SNAPSHOT FILE.                                           *ELPNFCV 
00029 *                                                                *ELPNFCV 
00030 ******************************************************************ELPNFCV 
00031 *                                                                *ELPNFCV 
00032 *                      MAINTENANCE HISTORY                       *ELPNFCV 
00033 *                                                                *ELPNFCV 
00034 *  MOD     DATE     BY  DRPT                ACTION               *ELPNFCV 
00035 * ----- ----------- --- ----- ---------------------------------- *ELPNFCV 
00036 * 01.00 20-MAR-1990 AKK       CREATED.                           *ELPNFCV 
00037 *                                                                *ELPNFCV 
00038 * 01.01 24-APR-1990 AKK       ADDED SUB HEADING 'CODES MANUAL'   *ELPNFCV 
00039 *                                                                *ELPNFCV 
00040 ******************************************************************ELPNFCV 
00041                                                                   ELPNFCV 
00042  ENVIRONMENT DIVISION.                                            ELPNFCV 
00043                                                                   ELPNFCV 
00044  CONFIGURATION SECTION.                                           ELPNFCV 
00045  SOURCE-COMPUTER.    IBM-3090.                                    ELPNFCV 
00046  OBJECT-COMPUTER.    IBM-3090.                                    ELPNFCV 
00047                                                                   ELPNFCV 
00048  INPUT-OUTPUT SECTION.                                            ELPNFCV 
00049  FILE-CONTROL.                                                    ELPNFCV 
00050       SELECT CODE-VALUE-FILE  ASSIGN TO UT-S-CODEVAL.             ELPNFCV 
00051     EJECT                                                         ELPNFCV 
00052  DATA DIVISION.                                                   ELPNFCV 
00053                                                                   ELPNFCV 
00054  FILE SECTION.                                                    ELPNFCV 
00055  FD  CODE-VALUE-FILE                                              ELPNFCV 
00056      BLOCK CONTAINS 0 RECORDS                                     ELPNFCV 
00057      LABEL RECORDS ARE STANDARD                                   ELPNFCV 
00058      RECORDING MODE IS V.                                         ELPNFCV 
00059      COPY ELSNAPSC.                                               ELPNFCV 
00060 /                                                                 ELPNFCV 
00061  WORKING-STORAGE SECTION.                                         ELPNFCV 
00062  01  FILLER           PIC X(18)   VALUE '*START OF ELPNFCV'.      ELPNFCV 
00063  01  WS-MISC.                                                     ELPNFCV 
00064      05  WS-EOF-SW                PIC X       VALUE 'N'.          ELPNFCV 
00065          88  WS-EOF                           VALUE 'Y'.          ELPNFCV 
00066 *                                                                 ELPNFCV 
00067  01  WORK-AREA.                                                   ELPNFCV 
00068      05  HOLD-PREFIX.                                             ELPNFCV 
00069          10  HOLD-RECORD-PREFIX       PIC X(08)   VALUE SPACES.   ELPNFCV 
00070          10  HOLD-SYSTEM-NAME         PIC X(30)   VALUE SPACES.   ELPNFCV 
00071      05  HOLD-ELEMENT-NUMBER          PIC S999V99 COMP-3          ELPNFCV 
00072                                                   VALUE ZERO.     ELPNFCV 
00073      05  HOLD-CODE-VALUE              PIC X(10)   VALUE SPACES.   ELPNFCV 
00074 /                                                                 ELPNFCV 
00075  01  CV-CODE-VALUE-LINES.                                         ELPNFCV 
00076      03  CV-HEADING-LINE-1.                                       ELPNFCV 
00077          05  CV-HEADING-LINE1-CC   PIC X(01)  VALUE '1'.          ELPNFCV 
00078          05  FILLER                PIC X(58)  VALUE SPACES.       ELPNFCV 
00079          05  FILLER                PIC X(30)  VALUE               ELPNFCV 
00080       'C O D E S  M A N U A L'.                                   ELPNFCV 
00081          05  FILLER                PIC X(44)  VALUE SPACES.       ELPNFCV 
00082      03  CV-HEADING-LINE-1A.                                      ELPNFCV 
00083          05  CV-HEADING-LINE1A-CC  PIC X(01)  VALUE ' '.          ELPNFCV 
00084          05  FILLER                PIC X(43)  VALUE SPACES.       ELPNFCV 
00085          05  FILLER                PIC X(50)  VALUE               ELPNFCV 
00086       'C O D E  V A L U E  E X C E P T I O N  R E P O R T'.       ELPNFCV 
00087          05  FILLER                PIC X(37)  VALUE SPACES.       ELPNFCV 
00088      03  CV-HEADING-LINE-2.                                       ELPNFCV 
00089          05  CV-HEADING-LINE2-CC   PIC X(01)  VALUE '-'.          ELPNFCV 
00090          05  FILLER                PIC X(01)  VALUE SPACES.       ELPNFCV 
00091          05  FILLER                PIC X(48)  VALUE               ELPNFCV 
00092              'THE FOLLOWING CODE VALUE(S) HAVE NOT BEEN FOUND:'.  ELPNFCV 
00093          05  FILLER                PIC X(83)  VALUE SPACES.       ELPNFCV 
00094      03  CV-HEADING-LINE-3.                                       ELPNFCV 
00095          05  CV-HEADING-LINE3-CC   PIC X(01)  VALUE '0'.          ELPNFCV 
00096          05  FILLER                PIC X(04)  VALUE SPACES.       ELPNFCV 
00097          05  FILLER                PIC X(06)  VALUE 'RECORD'.     ELPNFCV 
00098          05  FILLER                PIC X(19)  VALUE SPACES.       ELPNFCV 
00099          05  FILLER                PIC X(06)  VALUE 'SYSTEM'.     ELPNFCV 
00100          05  FILLER                PIC X(19)  VALUE SPACES.       ELPNFCV 
00101          05  FILLER                PIC X(12)  VALUE               ELPNFCV 
00102              'DATA ELEMENT'.                                      ELPNFCV 
00103          05  FILLER                PIC X(07)  VALUE SPACES.       ELPNFCV 
00104          05  FILLER                PIC X(04)  VALUE 'CODE'.       ELPNFCV 
00105          05  FILLER                PIC X(10)  VALUE SPACES.       ELPNFCV 
00106          05  FILLER                PIC X(10)  VALUE 'CODE VALUE'. ELPNFCV 
00107          05  FILLER                PIC X(05)  VALUE SPACES.       ELPNFCV 
00108          05  FILLER                PIC X(06)  VALUE 'SYSTEM'.     ELPNFCV 
00109          05  FILLER                PIC X(04)  VALUE SPACES.       ELPNFCV 
00110          05  FILLER                PIC X(06)  VALUE 'APPLID'.     ELPNFCV 
00111          05  FILLER                PIC X(14)  VALUE SPACES.       ELPNFCV 
00112      03  CV-HEADING-LINE-4.                                       ELPNFCV 
00113          05  CV-HEADING-LINE4-CC   PIC X(01)  VALUE ' '.          ELPNFCV 
00114          05  FILLER                PIC X(05)  VALUE SPACES.       ELPNFCV 
00115          05  FILLER                PIC X(04)  VALUE 'LIST'.       ELPNFCV 
00116          05  FILLER                PIC X(21)  VALUE SPACES.       ELPNFCV 
00117          05  FILLER                PIC X(04)  VALUE 'NAME'.       ELPNFCV 
00118          05  FILLER                PIC X(23)  VALUE SPACES.       ELPNFCV 
00119          05  FILLER                PIC X(06)  VALUE 'NUMBER'.     ELPNFCV 
00120          05  FILLER                PIC X(10)  VALUE SPACES.       ELPNFCV 
00121          05  FILLER                PIC X(05)  VALUE 'VALUE'.      ELPNFCV 
00122          05  FILLER                PIC X(09)  VALUE SPACES.       ELPNFCV 
00123          05  FILLER                PIC X(10)  VALUE 'HEX FORMAT'. ELPNFCV 
00124          05  FILLER                PIC X(07)  VALUE SPACES.       ELPNFCV 
00125          05  FILLER                PIC X(02)  VALUE 'ID'.         ELPNFCV 
00126          05  FILLER                PIC X(06)  VALUE SPACES.       ELPNFCV 
00127          05  FILLER                PIC X(06)  VALUE 'REGION'.     ELPNFCV 
00128          05  FILLER                PIC X(14)  VALUE SPACES.       ELPNFCV 
00129      03  CV-HEADING-LINE-5.                                       ELPNFCV 
00130          05  CV-HEADING-LINE5-CC   PIC X(01)  VALUE ' '.          ELPNFCV 
00131          05  FILLER                PIC X(03)  VALUE SPACES.       ELPNFCV 
00132          05  FILLER                PIC X(08)  VALUE '========'.   ELPNFCV 
00133          05  FILLER                PIC X(07)  VALUE SPACES.       ELPNFCV 
00134          05  FILLER                PIC X(31)  VALUE               ELPNFCV 
00135              '==============================='.                   ELPNFCV 
00136          05  FILLER                PIC X(05)  VALUE SPACES.       ELPNFCV 
00137          05  FILLER                PIC X(12)  VALUE               ELPNFCV 
00138              '============'.                                      ELPNFCV 
00139          05  FILLER                PIC X(05)  VALUE SPACES.       ELPNFCV 
00140          05  FILLER                PIC X(10)  VALUE '=========='. ELPNFCV 
00141          05  FILLER                PIC X(06)  VALUE SPACES.       ELPNFCV 
00142          05  FILLER                PIC X(10)  VALUE '=========='. ELPNFCV 
00143          05  FILLER                PIC X(05)  VALUE SPACES.       ELPNFCV 
00144          05  FILLER                PIC X(06)  VALUE '======'.     ELPNFCV 
00145          05  FILLER                PIC X(04)  VALUE SPACES.       ELPNFCV 
00146          05  FILLER                PIC X(07)  VALUE '======='.    ELPNFCV 
00147          05  FILLER                PIC X(13)  VALUE SPACES.       ELPNFCV 
00148      03  CV-DETAIL-LINE-1.                                        ELPNFCV 
00149          05  CV-DETAIL-LINE1-CC    PIC X(01)  VALUE ' '.          ELPNFCV 
00150          05  FILLER                PIC X(03)  VALUE SPACES.       ELPNFCV 
00151          05  CV-RECORD-LIST        PIC X(08)  VALUE SPACES.       ELPNFCV 
00152          05  FILLER                PIC X(07)  VALUE SPACES.       ELPNFCV 
00153          05  CV-SYSTEM-NAME        PIC X(31)  VALUE SPACES.       ELPNFCV 
00154          05  FILLER                PIC X(08)  VALUE SPACES.       ELPNFCV 
00155          05  CV-DATA-ELEMENT-NBR   PIC ZZ9.99.                    ELPNFCV 
00156          05  CV-DATA-ELEMENT-NBRA REDEFINES CV-DATA-ELEMENT-NBR   ELPNFCV 
00157                                    PIC X(06).                     ELPNFCV 
00158          05  FILLER                PIC X(10)  VALUE SPACES.       ELPNFCV 
00159          05  CV-CODE-VALUE         PIC X(10)  VALUE SPACES.       ELPNFCV 
00160          05  FILLER                PIC X(04)  VALUE SPACES.       ELPNFCV 
00161          05  CV-CODE-VALUE-HEX     PIC X(10)  VALUE SPACES.       ELPNFCV 
00162          05  FILLER                PIC X(06)  VALUE SPACES.       ELPNFCV 
00163          05  CV-SYSTEM-ID          PIC X(04)  VALUE SPACES.       ELPNFCV 
00164          05  FILLER                PIC X(05)  VALUE SPACES.       ELPNFCV 
00165          05  CV-APPLID-REGION      PIC X(08)  VALUE SPACES.       ELPNFCV 
00166          05  FILLER                PIC X(12)  VALUE SPACES.       ELPNFCV 
00167      03  CV-DETAIL-LINE-2.                                        ELPNFCV 
00168          05  CV-DETAIL-LINE2-CC    PIC X(01)  VALUE ' '.          ELPNFCV 
00169          05  FILLER                PIC X(87)  VALUE SPACES.       ELPNFCV 
00170          05  CV-CODE-VALUE-HEX2    PIC X(10)  VALUE SPACES.       ELPNFCV 
00171          05  FILLER                PIC X(35)  VALUE SPACES.       ELPNFCV 
00172      03  CV-DETAIL-LINE-3.                                        ELPNFCV 
00173          05  CV-DETAIL-LINE3-CC    PIC X(01)  VALUE ' '.          ELPNFCV 
00174          05  FILLER                PIC X(87)  VALUE SPACES.       ELPNFCV 
00175          05  CV-CODE-VALUE-HEX3    PIC X(10)  VALUE SPACES.       ELPNFCV 
00176          05  FILLER                PIC X(35)  VALUE SPACES.       ELPNFCV 
00177 /                                                                 ELPNFCV 
00178 *                                                                 ELPNFCV 
00179  COPY ELSUNPKC.                                                   ELPNFCV 
00180 *                                                                 ELPNFCV 
00181  01  FILLER           PIC X(16)   VALUE '*END OF ELPNFCV'.        ELPNFCV 
00182 /                                                                 ELPNFCV 
00183  LINKAGE SECTION.                                                 ELPNFCV 
00184  COPY ELSPRCBC.                                                   ELPNFCV 
00185 /                                                                 ELPNFCV 
00186  COPY ELSELOGC.                                                   ELPNFCV 
00187 /                                                                 ELPNFCV 
00188  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK.                ELPNFCV 
00189      PERFORM 0000-INITIALIZATION.                                 ELPNFCV 
00190      PERFORM 1000-PRINT-REPORT                                    ELPNFCV 
00191           UNTIL WS-EOF.                                           ELPNFCV 
00192      CLOSE CODE-VALUE-FILE.                                       ELPNFCV 
00193      MOVE ZERO TO RETURN-CODE.                                    ELPNFCV 
00194      GOBACK.                                                      ELPNFCV 
00195                                                                   ELPNFCV 
00196  0000-INITIALIZATION.                                             ELPNFCV 
00197      OPEN INPUT CODE-VALUE-FILE.                                  ELPNFCV 
00198      PERFORM 8500-DO-READ.                                        ELPNFCV 
00199      IF WS-EOF                                                    ELPNFCV 
00200         CONTINUE                                                  ELPNFCV 
00201      ELSE                                                         ELPNFCV 
00202         PERFORM 8000-PRINT-PAGE-HEADINGS.                         ELPNFCV 
00203 /                                                                 ELPNFCV 
00204  1000-PRINT-REPORT.                                               ELPNFCV 
00205      IF PCB-CURRENT-LINE > PCB-MAX-LINES                          ELPNFCV 
00206           PERFORM 8000-PRINT-PAGE-HEADINGS.                       ELPNFCV 
00207      MOVE SPACES TO CV-DETAIL-LINE-1                              ELPNFCV 
00208                     CV-DETAIL-LINE-2.                             ELPNFCV 
00209      IF LG-LINE-PREFIX = HOLD-PREFIX                              ELPNFCV 
00210         AND                                                       ELPNFCV 
00211         LG-DE-NUMBER = HOLD-ELEMENT-NUMBER                        ELPNFCV 
00212         AND                                                       ELPNFCV 
00213         LG-CODE-VALUE = HOLD-CODE-VALUE                           ELPNFCV 
00214            CONTINUE                                               ELPNFCV 
00215      ELSE                                                         ELPNFCV 
00216         PERFORM 1050-DO-PREFIX-MOVES                              ELPNFCV 
00217         PERFORM 1060-DO-RECORD-MOVES                              ELPNFCV 
00218         PERFORM 1075-GET-HEX-DATA                                 ELPNFCV 
00219         MOVE CV-DETAIL-LINE-1 TO PCB-PRINT-AREA                   ELPNFCV 
00220         PERFORM 9000-PRINT-LINE                                   ELPNFCV 
00221         MOVE CV-DETAIL-LINE-2 TO PCB-PRINT-AREA                   ELPNFCV 
00222         PERFORM 9000-PRINT-LINE.                                  ELPNFCV 
00223      PERFORM 8500-DO-READ.                                        ELPNFCV 
00224 *                                                                 ELPNFCV 
00225  1050-DO-PREFIX-MOVES.                                            ELPNFCV 
00226      MOVE SSR-CICS-SYSTEM-ID TO CV-SYSTEM-ID.                     ELPNFCV 
00227      MOVE SSR-CICS-APPL-ID TO CV-APPLID-REGION.                   ELPNFCV 
00228 *                                                                 ELPNFCV 
00229  1060-DO-RECORD-MOVES.                                            ELPNFCV 
00230      MOVE LG-RECORD-PREFIX TO CV-RECORD-LIST                      ELPNFCV 
00231                               HOLD-RECORD-PREFIX.                 ELPNFCV 
00232      MOVE LG-ELEMENT-NAME TO CV-SYSTEM-NAME                       ELPNFCV 
00233                              HOLD-SYSTEM-NAME.                    ELPNFCV 
00234      DISPLAY 'LOG DE NUM '  LG-DE-NUMBER  ' '                     ELPNFCV 
00235      DISPLAY 'LOG DE NUM2'  CV-DATA-ELEMENT-NBR                   ELPNFCV 
00236      DISPLAY 'LOG DE NUM3'  HOLD-ELEMENT-NUMBER                   ELPNFCV 
00237      MOVE LG-DE-NUMBER TO CV-DATA-ELEMENT-NBR                     ELPNFCV 
00238                           HOLD-ELEMENT-NUMBER.                    ELPNFCV 
00239      MOVE LG-CODE-VALUE TO CV-CODE-VALUE                          ELPNFCV 
00240                            HOLD-CODE-VALUE.                       ELPNFCV 
00241 *                                                                 ELPNFCV 
00242  1075-GET-HEX-DATA.                                               ELPNFCV 
00243      MOVE ZERO TO EP-START-OFFSET.                                ELPNFCV 
00244      SET EP-CICS-REC-PTR TO NULLS.                                ELPNFCV 
00245      MOVE LENGTH OF CV-CODE-VALUE TO EP-LENGTH.                   ELPNFCV 
00246      CALL 'ELPUNPKR' USING ELPUNPKR-PARM                          ELPNFCV 
00247                            CV-CODE-VALUE.                         ELPNFCV 
00248      MOVE EP-LINE-2-TEXT TO CV-CODE-VALUE-HEX.                    ELPNFCV 
00249      MOVE EP-LINE-3-TEXT TO CV-CODE-VALUE-HEX2.                   ELPNFCV 
00250 /                                                                 ELPNFCV 
00251  8000-PRINT-PAGE-HEADINGS.                                        ELPNFCV 
00252      MOVE CV-HEADING-LINE-1 TO PCB-PRINT-AREA.                    ELPNFCV 
00253      PERFORM 9000-PRINT-LINE.                                     ELPNFCV 
00254      MOVE CV-HEADING-LINE-1A TO PCB-PRINT-AREA.                   ELPNFCV 
00255      PERFORM 9000-PRINT-LINE.                                     ELPNFCV 
00256      MOVE CV-HEADING-LINE-2 TO PCB-PRINT-AREA.                    ELPNFCV 
00257      PERFORM 9000-PRINT-LINE.                                     ELPNFCV 
00258      MOVE CV-HEADING-LINE-3 TO PCB-PRINT-AREA.                    ELPNFCV 
00259      PERFORM 9000-PRINT-LINE.                                     ELPNFCV 
00260      MOVE CV-HEADING-LINE-4 TO PCB-PRINT-AREA.                    ELPNFCV 
00261      PERFORM 9000-PRINT-LINE.                                     ELPNFCV 
00262      MOVE CV-HEADING-LINE-5 TO PCB-PRINT-AREA.                    ELPNFCV 
00263      PERFORM 9000-PRINT-LINE.                                     ELPNFCV 
00264      MOVE SPACES         TO PCB-PRINT-AREA.                       ELPNFCV 
00265      PERFORM 9000-PRINT-LINE.                                     ELPNFCV 
00266                                                                   ELPNFCV 
00267  8500-DO-READ.                                                    ELPNFCV 
00268      READ CODE-VALUE-FILE                                         ELPNFCV 
00269          AT END                                                   ELPNFCV 
00270               SET WS-EOF TO TRUE.                                 ELPNFCV 
00271      IF WS-EOF                                                    ELPNFCV 
00272         CONTINUE                                                  ELPNFCV 
00273      ELSE                                                         ELPNFCV 
00274         CALL 'ELUADDRS' USING SSR-SUB-REC (1)                     ELPNFCV 
00275                    ADDRESS OF LG-LOG-RECORD.                      ELPNFCV 
00276                                                                   ELPNFCV 
00277  9000-PRINT-LINE.                                                 ELPNFCV 
00278      SET PCB-PRINT-LINE             TO TRUE.                      ELPNFCV 
00279      INSPECT PCB-PRINT-AREA REPLACING ALL LOW-VALUE BY '_'.       ELPNFCV 
00280      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPNFCV 
