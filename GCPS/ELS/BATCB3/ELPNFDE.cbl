00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPNFDE 
00003  PROGRAM-ID.         ELPNFDE.                                        LV001
00004                                                                   ELPNFDE 
00005  AUTHOR.             ANNE KEFFER KING.                            ELPNFDE 
00006                                                                   ELPNFDE 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPNFDE 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPNFDE 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPNFDE 
00010                      233 N. MICHIGAN AVE                          ELPNFDE 
00011                      CHICAGO, ILLINOIS 60601                      ELPNFDE 
00012                                                                   ELPNFDE 
00013  DATE-WRITTEN.       20-MAR-1990.                                 ELPNFDE 
00014                                                                   ELPNFDE 
00015  DATE-COMPILED.                                                   ELPNFDE 
00016                                                                   ELPNFDE 
00017  SECURITY.           COPYRIGHT 1989,                              ELPNFDE 
00018                      HEALTH CARE SERVICE CORPORATION              ELPNFDE 
00019  TITLE 'ELS ABEND PROCESSING-PRINT DATA ELEMENT EXCEPTION REPORT' ELPNFDE 
00020 ******************************************************************ELPNFDE 
00021 *                                                                *ELPNFDE 
00022 *                    ELS ABEND PROCESSING                        *ELPNFDE 
00023 *                                                                *ELPNFDE 
00024 *   THIS SUBROUTINE PRINTS THE CODE ELEMENT EXCEPTION REPORT.    *ELPNFDE 
00025 *   ONE CALL WILL PRINT THE ENTIRE EXCEPTION REPORT.             *ELPNFDE 
00026 *                                                                *ELPNFDE 
00027 *   THE INPUT CONSISTS OF ONLY THE RECORDS EXTRACTED FROM THE    *ELPNFDE 
00028 *   ELS SNAPSHOT FILE.                                           *ELPNFDE 
00029 *                                                                *ELPNFDE 
00030 ******************************************************************ELPNFDE 
00031 *                                                                *ELPNFDE 
00032 *                      MAINTENANCE HISTORY                       *ELPNFDE 
00033 *                                                                *ELPNFDE 
00034 *  MOD     DATE     BY  DRPT                ACTION               *ELPNFDE 
00035 * ----- ----------- --- ----- ---------------------------------- *ELPNFDE 
00036 * 01.00 20-MAR-1990 AKK       CREATED.                           *ELPNFDE 
00037 *                                                                *ELPNFDE 
00038 * 01.01 24-APR-1990 AKK       ADDED SUB HEADING - 'CODES MANUAL' *ELPNFDE 
00039 *                                                                *ELPNFDE 
00040 ******************************************************************ELPNFDE 
00041                                                                   ELPNFDE 
00042  ENVIRONMENT DIVISION.                                            ELPNFDE 
00043                                                                   ELPNFDE 
00044  CONFIGURATION SECTION.                                           ELPNFDE 
00045  SOURCE-COMPUTER.    IBM-3090.                                    ELPNFDE 
00046  OBJECT-COMPUTER.    IBM-3090.                                    ELPNFDE 
00047                                                                   ELPNFDE 
00048  INPUT-OUTPUT SECTION.                                            ELPNFDE 
00049  FILE-CONTROL.                                                    ELPNFDE 
00050       SELECT DATA-ELEMENT-FILE  ASSIGN TO UT-S-DATAELE.           ELPNFDE 
00051     EJECT                                                         ELPNFDE 
00052  DATA DIVISION.                                                   ELPNFDE 
00053                                                                   ELPNFDE 
00054  FILE SECTION.                                                    ELPNFDE 
00055  FD  DATA-ELEMENT-FILE                                            ELPNFDE 
00056      BLOCK CONTAINS 0 RECORDS                                     ELPNFDE 
00057      LABEL RECORDS ARE STANDARD                                   ELPNFDE 
00058      RECORDING MODE IS V.                                         ELPNFDE 
00059      COPY ELSNAPSC.                                               ELPNFDE 
00060 /                                                                 ELPNFDE 
00061  WORKING-STORAGE SECTION.                                         ELPNFDE 
00062  01  FILLER           PIC X(18)   VALUE '*START OF ELPNFDE'.      ELPNFDE 
00063  01  WS-MISC.                                                     ELPNFDE 
00064      05  WS-EOF-SW    PIC X       VALUE 'N'.                      ELPNFDE 
00065          88  WS-EOF               VALUE 'Y'.                      ELPNFDE 
00066 *                                                                 ELPNFDE 
00067  01  HOLD-PREFIX.                                                 ELPNFDE 
00068      05  HOLD-RECORD-PREV         PIC X(08) VALUE SPACE.          ELPNFDE 
00069      05  HOLD-SYSTEM-NAME         PIC X(30) VALUE SPACE.          ELPNFDE 
00070 /                                                                 ELPNFDE 
00071  01  DE-DATA-ELEMENT-OUTPUT-LINES.                                ELPNFDE 
00072      03  DE-HEADING-LINE-1.                                       ELPNFDE 
00073          05  DE-DATA-ELEMENT-CC-1  PIC X(01)  VALUE '1'.          ELPNFDE 
00074          05  FILLER                PIC X(56)  VALUE SPACES.       ELPNFDE 
00075          05  FILLER                PIC X(30)  VALUE               ELPNFDE 
00076       'C O D E S  M A N U A L'.                                   ELPNFDE 
00077          05  FILLER                PIC X(46)  VALUE SPACES.       ELPNFDE 
00078      03  DE-HEADING-LINE-1A.                                      ELPNFDE 
00079          05  DE-DATA-ELEMENT-CC1A  PIC X(01)  VALUE ' '.          ELPNFDE 
00080          05  FILLER                PIC X(41)  VALUE SPACES.       ELPNFDE 
00081          05  FILLER                PIC X(54)  VALUE               ELPNFDE 
00082       'D A T A  E L E M E N T  E X C E P T I O N  R E P O R T'.   ELPNFDE 
00083          05  FILLER                PIC X(36)  VALUE SPACES.       ELPNFDE 
00084      03  DE-HEADING-LINE-2.                                       ELPNFDE 
00085          05  DE-DATA-ELEMENT-CC-2  PIC X(01)  VALUE '-'.          ELPNFDE 
00086          05  FILLER                PIC X(50)  VALUE               ELPNFDE 
00087              'THE FOLLOWING DATA ELEMENT(S) HAVE NOT BEEN FOUND:'.ELPNFDE 
00088          05  FILLER                PIC X(82)  VALUE SPACES.       ELPNFDE 
00089      03  DE-HEADING-LINE-3.                                       ELPNFDE 
00090          05  DE-DATA-ELEMENT-CC-3  PIC X(01)  VALUE '0'.          ELPNFDE 
00091          05  FILLER                PIC X(03)  VALUE SPACES.       ELPNFDE 
00092          05  FILLER                PIC X(06)  VALUE 'RECORD'.     ELPNFDE 
00093          05  FILLER                PIC X(20)  VALUE SPACES.       ELPNFDE 
00094          05  FILLER                PIC X(06)  VALUE 'SYSTEM'.     ELPNFDE 
00095          05  FILLER                PIC X(19)  VALUE SPACES.       ELPNFDE 
00096          05  FILLER                PIC X(06)  VALUE 'SYSTEM'.     ELPNFDE 
00097          05  FILLER                PIC X(06)  VALUE SPACES.       ELPNFDE 
00098          05  FILLER                PIC X(06)  VALUE 'APPLID'.     ELPNFDE 
00099          05  FILLER                PIC X(60)  VALUE SPACES.       ELPNFDE 
00100      03  DE-HEADING-LINE-4.                                       ELPNFDE 
00101          05  DE-DATA-ELEMENT-CC-4  PIC X(01)  VALUE ' '.          ELPNFDE 
00102          05  FILLER                PIC X(04)  VALUE SPACES.       ELPNFDE 
00103          05  FILLER                PIC X(04)  VALUE 'LIST'.       ELPNFDE 
00104          05  FILLER                PIC X(22)  VALUE SPACES.       ELPNFDE 
00105          05  FILLER                PIC X(04)  VALUE 'NAME'.       ELPNFDE 
00106          05  FILLER                PIC X(22)  VALUE SPACES.       ELPNFDE 
00107          05  FILLER                PIC X(02)  VALUE 'ID'.         ELPNFDE 
00108          05  FILLER                PIC X(08)  VALUE SPACES.       ELPNFDE 
00109          05  FILLER                PIC X(06)  VALUE 'REGION'.     ELPNFDE 
00110          05  FILLER                PIC X(60)  VALUE SPACES.       ELPNFDE 
00111      03  DE-HEADING-LINE-5.                                       ELPNFDE 
00112          05  DE-DATA-ELEMENT-CC-5  PIC X(01)  VALUE ' '.          ELPNFDE 
00113          05  FILLER                PIC X(02)  VALUE SPACES.       ELPNFDE 
00114          05  FILLER                PIC X(08)  VALUE '========'.   ELPNFDE 
00115          05  FILLER                PIC X(07)  VALUE SPACES.       ELPNFDE 
00116          05  FILLER                PIC X(30)  VALUE               ELPNFDE 
00117              '=============================='.                    ELPNFDE 
00118          05  FILLER                PIC X(07)  VALUE SPACES.       ELPNFDE 
00119          05  FILLER                PIC X(06)  VALUE '======'.     ELPNFDE 
00120          05  FILLER                PIC X(05)  VALUE SPACES.       ELPNFDE 
00121          05  FILLER                PIC X(08)  VALUE '========'.   ELPNFDE 
00122          05  FILLER                PIC X(59)  VALUE SPACES.       ELPNFDE 
00123      03  DE-DETAIL-LINE-1.                                        ELPNFDE 
00124          05  DE-DE-DETAIL-CC-1     PIC X(01)  VALUE ' '.          ELPNFDE 
00125          05  FILLER                PIC X(02)  VALUE SPACES.       ELPNFDE 
00126          05  DE-RECORD-PREFIX      PIC X(08)  VALUE SPACES.       ELPNFDE 
00127          05  FILLER                PIC X(07)  VALUE SPACES.       ELPNFDE 
00128          05  DE-ELEMENT-NAME       PIC X(30)  VALUE SPACES.       ELPNFDE 
00129          05  FILLER                PIC X(08)  VALUE SPACES.       ELPNFDE 
00130          05  DE-SYSTEM-ID          PIC X(04)  VALUE SPACES.       ELPNFDE 
00131          05  FILLER                PIC X(06)  VALUE SPACES.       ELPNFDE 
00132          05  DE-APPLID-REGION      PIC X(08)  VALUE SPACES.       ELPNFDE 
00133          05  FILLER                PIC X(59)  VALUE SPACES.       ELPNFDE 
00134 *                                                                 ELPNFDE 
00135  01  FILLER           PIC X(16)   VALUE '*END OF ELPNFDE'.        ELPNFDE 
00136 /                                                                 ELPNFDE 
00137  LINKAGE SECTION.                                                 ELPNFDE 
00138  COPY ELSPRCBC.                                                   ELPNFDE 
00139 /                                                                 ELPNFDE 
00140  COPY ELSELOGC.                                                   ELPNFDE 
00141 /                                                                 ELPNFDE 
00142  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK.                ELPNFDE 
00143      PERFORM 0000-INITIALIZATION.                                 ELPNFDE 
00144      PERFORM 1000-PRINT-REPORT                                    ELPNFDE 
00145           UNTIL WS-EOF.                                           ELPNFDE 
00146      CLOSE DATA-ELEMENT-FILE.                                     ELPNFDE 
00147      MOVE ZERO TO RETURN-CODE.                                    ELPNFDE 
00148      GOBACK.                                                      ELPNFDE 
00149                                                                   ELPNFDE 
00150  0000-INITIALIZATION.                                             ELPNFDE 
00151      OPEN INPUT DATA-ELEMENT-FILE.                                ELPNFDE 
00152      PERFORM 8500-DO-READ.                                        ELPNFDE 
00153      IF WS-EOF                                                    ELPNFDE 
00154         CONTINUE                                                  ELPNFDE 
00155      ELSE                                                         ELPNFDE 
00156         PERFORM 8000-PRINT-PAGE-HEADINGS.                         ELPNFDE 
00157 /                                                                 ELPNFDE 
00158  1000-PRINT-REPORT.                                               ELPNFDE 
00159      IF PCB-CURRENT-LINE > PCB-MAX-LINES                          ELPNFDE 
00160           PERFORM 8000-PRINT-PAGE-HEADINGS.                       ELPNFDE 
00161      MOVE SPACES TO DE-DETAIL-LINE-1.                             ELPNFDE 
00162      IF LG-LINE-PREFIX = HOLD-PREFIX                              ELPNFDE 
00163           CONTINUE                                                ELPNFDE 
00164      ELSE                                                         ELPNFDE 
00165         PERFORM 1050-DO-PREFIX-MOVES                              ELPNFDE 
00166         PERFORM 1060-DO-RECORD-MOVES                              ELPNFDE 
00167         MOVE DE-DETAIL-LINE-1 TO PCB-PRINT-AREA                   ELPNFDE 
00168         PERFORM 9000-PRINT-LINE.                                  ELPNFDE 
00169      PERFORM 8500-DO-READ.                                        ELPNFDE 
00170 *                                                                 ELPNFDE 
00171  1050-DO-PREFIX-MOVES.                                            ELPNFDE 
00172      MOVE SSR-CICS-SYSTEM-ID TO DE-SYSTEM-ID.                     ELPNFDE 
00173      MOVE SSR-CICS-APPL-ID TO DE-APPLID-REGION.                   ELPNFDE 
00174 *                                                                 ELPNFDE 
00175  1060-DO-RECORD-MOVES.                                            ELPNFDE 
00176      MOVE LG-RECORD-PREFIX TO DE-RECORD-PREFIX.                   ELPNFDE 
00177      MOVE LG-ELEMENT-NAME TO DE-ELEMENT-NAME.                     ELPNFDE 
00178      MOVE LG-LINE-PREFIX TO HOLD-PREFIX.                          ELPNFDE 
00179 /                                                                 ELPNFDE 
00180  8000-PRINT-PAGE-HEADINGS.                                        ELPNFDE 
00181      MOVE DE-HEADING-LINE-1 TO PCB-PRINT-AREA.                    ELPNFDE 
00182      PERFORM 9000-PRINT-LINE.                                     ELPNFDE 
00183      MOVE DE-HEADING-LINE-1A TO PCB-PRINT-AREA.                   ELPNFDE 
00184      PERFORM 9000-PRINT-LINE.                                     ELPNFDE 
00185      MOVE DE-HEADING-LINE-2 TO PCB-PRINT-AREA.                    ELPNFDE 
00186      PERFORM 9000-PRINT-LINE.                                     ELPNFDE 
00187      MOVE DE-HEADING-LINE-3 TO PCB-PRINT-AREA.                    ELPNFDE 
00188      PERFORM 9000-PRINT-LINE.                                     ELPNFDE 
00189      MOVE DE-HEADING-LINE-4 TO PCB-PRINT-AREA.                    ELPNFDE 
00190      PERFORM 9000-PRINT-LINE.                                     ELPNFDE 
00191      MOVE DE-HEADING-LINE-5 TO PCB-PRINT-AREA.                    ELPNFDE 
00192      PERFORM 9000-PRINT-LINE.                                     ELPNFDE 
00193      MOVE SPACES         TO PCB-PRINT-AREA.                       ELPNFDE 
00194      PERFORM 9000-PRINT-LINE.                                     ELPNFDE 
00195                                                                   ELPNFDE 
00196  8500-DO-READ.                                                    ELPNFDE 
00197      READ DATA-ELEMENT-FILE                                       ELPNFDE 
00198          AT END                                                   ELPNFDE 
00199               SET WS-EOF TO TRUE.                                 ELPNFDE 
00200      IF WS-EOF                                                    ELPNFDE 
00201         CONTINUE                                                  ELPNFDE 
00202      ELSE                                                         ELPNFDE 
00203         CALL 'ELUADDRS' USING SSR-SUB-REC (1)                     ELPNFDE 
00204                    ADDRESS OF LG-LOG-RECORD.                      ELPNFDE 
00205                                                                   ELPNFDE 
00206  9000-PRINT-LINE.                                                 ELPNFDE 
00207      SET PCB-PRINT-LINE             TO TRUE.                      ELPNFDE 
00208      INSPECT PCB-PRINT-AREA REPLACING ALL LOW-VALUE BY '_'.       ELPNFDE 
00209      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPNFDE 
