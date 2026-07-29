00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPPRDMP
00003  PROGRAM-ID.         ELPPRDMP.                                       LV001
00004                                                                   ELPPRDMP
00005  AUTHOR.             EDWARD G LISS.                               ELPPRDMP
00006                                                                   ELPPRDMP
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPPRDMP
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPPRDMP
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPPRDMP
00010                      233 N. MICHIGAN AVE                          ELPPRDMP
00011                      CHICAGO, ILLINOIS 60601                      ELPPRDMP
00012                                                                   ELPPRDMP
00013  DATE-WRITTEN.       20-JUL-1989.                                 ELPPRDMP
00014                                                                   ELPPRDMP
00015  DATE-COMPILED.                                                   ELPPRDMP
00016                                                                   ELPPRDMP
00017  SECURITY.           COPYRIGHT 1989,                              ELPPRDMP
00018                      HEALTH CARE SERVICE CORPORATION              ELPPRDMP
00019      SKIP3                                                        ELPPRDMP
00020  ENVIRONMENT DIVISION.                                            ELPPRDMP
00021                                                                   ELPPRDMP
00022  CONFIGURATION SECTION.                                           ELPPRDMP
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELPPRDMP
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELPPRDMP
00025     SKIP3                                                         ELPPRDMP
00026 ******************************************************************ELPPRDMP
00027 *                                                                *ELPPRDMP
00028 *                    ELS ABEND PROCESSING                        *ELPPRDMP
00029 *                                                                *ELPPRDMP
00030 *   THIS SUBROUTINE PRINTS A DUMP OF THE DATA CONTAINS IN THE    *ELPPRDMP
00031 *   RECORD PORTION OF THE REAP PARAMETER LIST.                   *ELPPRDMP
00032 *                                                                *ELPPRDMP
00033 ******************************************************************ELPPRDMP
00034 *                                                                *ELPPRDMP
00035 *                      MAINTENANCE HISTORY                       *ELPPRDMP
00036 *                                                                *ELPPRDMP
00037 *  MOD     DATE     BY  DRPT                ACTION               *ELPPRDMP
00038 * ----- ----------- --- ----- ---------------------------------- *ELPPRDMP
00039 * 01.00 20-JUL-1989 EGL       CREATED.                           *ELPPRDMP
00040 *                                                                *ELPPRDMP
00041 ******************************************************************ELPPRDMP
00042  TITLE 'ELS ABEND PROCESSING - PRINT RECORD DUMP'.                ELPPRDMP
00043  DATA DIVISION.                                                   ELPPRDMP
00044  WORKING-STORAGE SECTION.                                         ELPPRDMP
00045  01  MISC-WS.                                                     ELPPRDMP
00046      05  WS-DUMP-LEN            PIC S9(8) COMP.                   ELPPRDMP
00047      05  WS-MAX-LINE-SIZE       PIC S9(8) COMP VALUE 120.         ELPPRDMP
00048 *                                                                 ELPPRDMP
00049  01  WS-DUMP-SUB-HEADINGS.                                        ELPPRDMP
00050      03  WS-DSH-1.                                                ELPPRDMP
00051          05  LINE-01-CC            PIC X(01)  VALUE '1'.          ELPPRDMP
00052          05  FILLER                PIC X(10)  VALUE 'ELS DDNAME'. ELPPRDMP
00053          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRDMP
00054          05  WS-DSH-DDNAME         PIC X(08)  VALUE SPACES.       ELPPRDMP
00055          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRDMP
00056          05  FILLER                PIC X(06)  VALUE 'LENGTH'.     ELPPRDMP
00057          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRDMP
00058          05  WS-DSH-LENGTH         PIC ZZZZZ9.                    ELPPRDMP
00059          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRDMP
00060          05  WS-DSH-CONT-MSG       PIC X(11)  VALUE SPACES.       ELPPRDMP
00061              88  WS-DSH-CONT                  VALUE '(CONTINUED)'.ELPPRDMP
00062          05  FILLER                PIC X(87)  VALUE SPACES.       ELPPRDMP
00063      03  WS-DSH-2.                                                ELPPRDMP
00064          05  LINE-02-CC            PIC X(01)  VALUE '0'.          ELPPRDMP
00065          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRDMP
00066          05  FILLER                PIC X(07)  VALUE 'ADDRESS'.    ELPPRDMP
00067          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRDMP
00068          05  FILLER                PIC X(120) VALUE               ELPPRDMP
00069              '----+----1----+----2----+----3----+----4----+----5--ELPPRDMP
00070 -            '--+----6----+----7----+----8----+----9----+----0----ELPPRDMP
00071 -            '+----1----+----2'.                                  ELPPRDMP
00072          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRDMP
00073 /                                                                 ELPPRDMP
00074      COPY ELSUNPKC.                                               ELPPRDMP
00075 /                                                                 ELPPRDMP
00076  01  WS-PREV-PRINT-AREA.                                          ELPPRDMP
00077      05  WS-PREV-LINE-1            PIC X(121).                    ELPPRDMP
00078      05  WS-PREV-LINE-2            PIC X(121).                    ELPPRDMP
00079      05  WS-PREV-LINE-3            PIC X(121).                    ELPPRDMP
00080                                                                   ELPPRDMP
00081  01  WS-COMPRESSED-PRINT.                                         ELPPRDMP
00082      05  WS-COMPRESSED-LINE-1.                                    ELPPRDMP
00083          10  WS-FROM-ID-1          PIC X(11).                     ELPPRDMP
00084          10  FILLER                PIC X(6)    VALUE ' THRU '.    ELPPRDMP
00085          10  WS-THRU-ID-1          PIC X(11).                     ELPPRDMP
00086          10  FILLER                PIC X(22)   VALUE              ELPPRDMP
00087                                    ' SAME AS PREVIOUS LINE'.      ELPPRDMP
00088      05  WS-COMPRESSED-LINE-2.                                    ELPPRDMP
00089          10  WS-FROM-ID-2          PIC X(11).                     ELPPRDMP
00090          10  FILLER                PIC X(6)    VALUE SPACES.      ELPPRDMP
00091          10  WS-THRU-ID-2          PIC X(11).                     ELPPRDMP
00092 /                                                                 ELPPRDMP
00093  LINKAGE SECTION.                                                 ELPPRDMP
00094      COPY ELSPRCBC.                                               ELPPRDMP
00095      EJECT                                                        ELPPRDMP
00096      COPY ELSRDPAC.                                               ELPPRDMP
00097 /                                                                 ELPPRDMP
00098  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK                 ELPPRDMP
00099                           RDP-READ-PARAMETERS.                    ELPPRDMP
00100                                                                   ELPPRDMP
00101      PERFORM 0000-INITIALIZE-MODULE.                              ELPPRDMP
00102      PERFORM 1000-DUMP-AREA                                       ELPPRDMP
00103          UNTIL WS-DUMP-LEN = ZERO.                                ELPPRDMP
00104      IF WS-FROM-ID-1 NOT = WS-THRU-ID-1                           ELPPRDMP
00105          PERFORM 1010-PRINT-COMPRESSED                            ELPPRDMP
00106      END-IF.                                                      ELPPRDMP
00107      PERFORM 3000-PRINT-PAGE-FOOTING.                             ELPPRDMP
00108      GOBACK.                                                      ELPPRDMP
00109                                                                   ELPPRDMP
00110  0000-INITIALIZE-MODULE.                                          ELPPRDMP
00111      MOVE RDP-DDNAME      TO WS-DSH-DDNAME.                       ELPPRDMP
00112      MOVE RDP-AREA-LENGTH TO WS-DSH-LENGTH.                       ELPPRDMP
00113      MOVE SPACES          TO WS-DSH-CONT-MSG.                     ELPPRDMP
00114      PERFORM 2000-PRINT-PAGE-HEADING.                             ELPPRDMP
00115      MOVE ZERO TO EP-START-OFFSET.                                ELPPRDMP
00116      SET EP-CICS-REC-PTR TO RDP-CICS-AREA-PTR.                    ELPPRDMP
00117      MOVE RDP-AREA-LENGTH TO WS-DUMP-LEN.                         ELPPRDMP
00118      INITIALIZE WS-PREV-PRINT-AREA.                               ELPPRDMP
00119      INITIALIZE WS-COMPRESSED-PRINT.                              ELPPRDMP
00120                                                                   ELPPRDMP
00121  1000-DUMP-AREA.                                                  ELPPRDMP
00122      IF WS-DUMP-LEN > WS-MAX-LINE-SIZE                            ELPPRDMP
00123           MOVE WS-MAX-LINE-SIZE TO EP-LENGTH                      ELPPRDMP
00124      ELSE                                                         ELPPRDMP
00125           MOVE WS-DUMP-LEN TO EP-LENGTH.                          ELPPRDMP
00126                                                                   ELPPRDMP
00127      CALL 'ELPUNPKR' USING      ELPUNPKR-PARM                     ELPPRDMP
00128                                 RDP-AREA.                         ELPPRDMP
00129                                                                   ELPPRDMP
00130      IF EP-LINE-1-TEXT = WS-PREV-LINE-1 AND                       ELPPRDMP
00131         EP-LINE-2-TEXT = WS-PREV-LINE-2 AND                       ELPPRDMP
00132         EP-LINE-3-TEXT = WS-PREV-LINE-3                           ELPPRDMP
00133             MOVE EP-LINE-1-ID TO WS-THRU-ID-1                     ELPPRDMP
00134             MOVE EP-LINE-2-ID TO WS-THRU-ID-2                     ELPPRDMP
00135      ELSE                                                         ELPPRDMP
00136         IF EP-START-OFFSET > ZERO        AND                      ELPPRDMP
00137            WS-FROM-ID-1 NOT = WS-THRU-ID-1                        ELPPRDMP
00138             PERFORM 1010-PRINT-COMPRESSED                         ELPPRDMP
00139         END-IF                                                    ELPPRDMP
00140         PERFORM 1020-PRINT-LINE                                   ELPPRDMP
00141      END-IF.                                                      ELPPRDMP
00142                                                                   ELPPRDMP
00143      SUBTRACT EP-LENGTH FROM WS-DUMP-LEN.                         ELPPRDMP
00144      ADD EP-LENGTH TO EP-START-OFFSET.                            ELPPRDMP
00145                                                                   ELPPRDMP
00146  1010-PRINT-COMPRESSED.                                           ELPPRDMP
00147      PERFORM 1030-TEST-FOR-FULL-PAGE.                             ELPPRDMP
00148                                                                   ELPPRDMP
00149      MOVE WS-COMPRESSED-LINE-1 TO PCB-PRINT-TEXT.                 ELPPRDMP
00150      SET PCB-PRINT-LINE TO TRUE.                                  ELPPRDMP
00151      SET PCB-DOUBLE-SPACE TO TRUE.                                ELPPRDMP
00152      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRDMP
00153                                                                   ELPPRDMP
00154      MOVE WS-COMPRESSED-LINE-2 TO PCB-PRINT-TEXT.                 ELPPRDMP
00155      SET PCB-PRINT-LINE TO TRUE.                                  ELPPRDMP
00156      SET PCB-SINGLE-SPACE TO TRUE.                                ELPPRDMP
00157      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRDMP
00158                                                                   ELPPRDMP
00159      MOVE SPACES TO PCB-PRINT-TEXT.                               ELPPRDMP
00160      SET PCB-PRINT-LINE TO TRUE.                                  ELPPRDMP
00161      SET PCB-SINGLE-SPACE TO TRUE.                                ELPPRDMP
00162      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRDMP
00163 /                                                                 ELPPRDMP
00164  1020-PRINT-LINE.                                                 ELPPRDMP
00165      PERFORM 1030-TEST-FOR-FULL-PAGE.                             ELPPRDMP
00166                                                                   ELPPRDMP
00167      MOVE EP-LINE-1 TO PCB-PRINT-TEXT.                            ELPPRDMP
00168      SET PCB-PRINT-LINE TO TRUE.                                  ELPPRDMP
00169      SET PCB-DOUBLE-SPACE TO TRUE.                                ELPPRDMP
00170      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRDMP
00171                                                                   ELPPRDMP
00172      MOVE EP-LINE-2 TO PCB-PRINT-TEXT.                            ELPPRDMP
00173      SET PCB-PRINT-LINE TO TRUE.                                  ELPPRDMP
00174      SET PCB-SINGLE-SPACE TO TRUE.                                ELPPRDMP
00175      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRDMP
00176                                                                   ELPPRDMP
00177      MOVE EP-LINE-3 TO PCB-PRINT-TEXT.                            ELPPRDMP
00178      SET PCB-PRINT-LINE TO TRUE.                                  ELPPRDMP
00179      SET PCB-SINGLE-SPACE TO TRUE.                                ELPPRDMP
00180      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRDMP
00181                                                                   ELPPRDMP
00182      MOVE EP-LINE-1-TEXT TO WS-PREV-LINE-1.                       ELPPRDMP
00183      MOVE EP-LINE-2-TEXT TO WS-PREV-LINE-2.                       ELPPRDMP
00184      MOVE EP-LINE-3-TEXT TO WS-PREV-LINE-3.                       ELPPRDMP
00185                                                                   ELPPRDMP
00186      MOVE EP-LINE-1-ID   TO WS-FROM-ID-1                          ELPPRDMP
00187                             WS-THRU-ID-1.                         ELPPRDMP
00188                                                                   ELPPRDMP
00189      MOVE EP-LINE-2-ID   TO WS-FROM-ID-2                          ELPPRDMP
00190                             WS-THRU-ID-2.                         ELPPRDMP
00191 /                                                                 ELPPRDMP
00192  1030-TEST-FOR-FULL-PAGE.                                         ELPPRDMP
00193      IF PCB-CURRENT-LINE + 4 > PCB-MAX-LINES                      ELPPRDMP
00194          SET WS-DSH-CONT TO TRUE                                  ELPPRDMP
00195          PERFORM 3000-PRINT-PAGE-FOOTING                          ELPPRDMP
00196          PERFORM 2000-PRINT-PAGE-HEADING.                         ELPPRDMP
00197                                                                   ELPPRDMP
00198  2000-PRINT-PAGE-HEADING.                                         ELPPRDMP
00199                                                                   ELPPRDMP
00200      SET PCB-PRINT-LINE TO TRUE.                                  ELPPRDMP
00201      MOVE WS-DSH-1 TO PCB-PRINT-AREA.                             ELPPRDMP
00202      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRDMP
00203                                                                   ELPPRDMP
00204      SET PCB-PRINT-LINE TO TRUE.                                  ELPPRDMP
00205      MOVE WS-DSH-2 TO PCB-PRINT-AREA.                             ELPPRDMP
00206      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRDMP
00207                                                                   ELPPRDMP
00208  3000-PRINT-PAGE-FOOTING.                                         ELPPRDMP
00209                                                                   ELPPRDMP
00210      SET PCB-PRINT-LINE TO TRUE.                                  ELPPRDMP
00211      MOVE WS-DSH-2 TO PCB-PRINT-AREA.                             ELPPRDMP
00212      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRDMP
