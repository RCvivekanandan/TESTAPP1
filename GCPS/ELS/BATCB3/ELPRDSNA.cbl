00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPRDSNA
00003  PROGRAM-ID.         ELPRDSNA.                                       LV001
00004                                                                   ELPRDSNA
00005  AUTHOR.             ANNE KEFFER KING.                            ELPRDSNA
00006                                                                   ELPRDSNA
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPRDSNA
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPRDSNA
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPRDSNA
00010                      233 N. MICHIGAN AVE                          ELPRDSNA
00011                      CHICAGO, ILLINOIS 60601                      ELPRDSNA
00012                                                                   ELPRDSNA
00013  DATE-WRITTEN.       02-JUN-1989.                                 ELPRDSNA
00014                                                                   ELPRDSNA
00015  DATE-COMPILED.                                                   ELPRDSNA
00016                                                                   ELPRDSNA
00017  SECURITY.           COPYRIGHT 1988,                              ELPRDSNA
00018                      HEALTH CARE SERVICE CORPORATION              ELPRDSNA
00019      SKIP3                                                        ELPRDSNA
00020  ENVIRONMENT DIVISION.                                            ELPRDSNA
00021                                                                   ELPRDSNA
00022  CONFIGURATION SECTION.                                           ELPRDSNA
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELPRDSNA
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELPRDSNA
00025      EJECT                                                        ELPRDSNA
00026 ***************************************************************** ELPRDSNA
00027 *                                                                *ELPRDSNA
00028 *                    ELS ABEND PROCESSING                        *ELPRDSNA
00029 *                                                                *ELPRDSNA
00030 *   ELPRDSNA - THIS SUBROUTINE READS THE SORTED SNAPSHOT FILE    *ELPRDSNA
00031 *              AND MOVES THE DATA AREA TO THE READ SUBROUTINE    *ELPRDSNA
00032 *              PARAMETER LIST. ELPRDSNA IS CALLED BY BOTH        *ELPRDSNA
00033 *              ABEND PROGRAMS (ELP030 AND ELP031).               *ELPRDSNA
00034 *                                                                *ELPRDSNA
00035 ******************************************************************ELPRDSNA
00036 *                                                                *ELPRDSNA
00037 *                      MAINTENANCE HISTORY                       *ELPRDSNA
00038 *                                                                *ELPRDSNA
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELPRDSNA
00040 * ----- ----------- --- ----- ---------------------------------- *ELPRDSNA
00041 * 01.00 03-JUN-1989 AKK       CREATED                            *ELPRDSNA
00042 * 01.01 16-MAR-1990 EGL       CHANGED PROGRAM TO NOT REBLOCK     *ELPRDSNA
00043 *                             RECORDS WHICH ARE DETECTED TO BE   *ELPRDSNA
00044 *                             LONGER THAN 65K                    *ELPRDSNA
00045 *                                                                *ELPRDSNA
00046 ******************************************************************ELPRDSNA
00047                                                                   ELPRDSNA
00048  INPUT-OUTPUT SECTION.                                            ELPRDSNA
00049  FILE-CONTROL.                                                    ELPRDSNA
00050      SELECT SNAP-SHOT-FILE ASSIGN TO SNAPSHTO.                    ELPRDSNA
00051 /                                                                 ELPRDSNA
00052 *                                                                 ELPRDSNA
00053  DATA DIVISION.                                                   ELPRDSNA
00054  FILE SECTION.                                                    ELPRDSNA
00055  FD  SNAP-SHOT-FILE                                               ELPRDSNA
00056      LABEL RECORDS ARE STANDARD                                   ELPRDSNA
00057      BLOCK CONTAINS 0 RECORDS                                     ELPRDSNA
00058      RECORD CONTAINS 51 TO 4089 CHARACTERS                        ELPRDSNA
00059      RECORDING MODE V.                                            ELPRDSNA
00060      COPY ELSNAPSC.                                               ELPRDSNA
00061 /                                                                 ELPRDSNA
00062  WORKING-STORAGE SECTION.                                         ELPRDSNA
00063  01  FILLER              PIC X(19)   VALUE '*START OF ELPRDSNA*'. ELPRDSNA
00064 *                                                                 ELPRDSNA
00065  01  WS-WORK-AREA.                                                ELPRDSNA
00066      05  WS-BLOCK-SIZE-ACCUM  PIC  S9(08)  COMP VALUE +0.         ELPRDSNA
00067      05  WS-PREV-SEQ-NUM      PIC  S9(04)  COMP VALUE +0.         ELPRDSNA
00068      05  WS-MOVE-LONG-FIELDS.                                     ELPRDSNA
00069          10  WS-MOVE-LEN      PIC S9(08)  COMP  VALUE +0.         ELPRDSNA
00070          10  WS-MOVE-POS      PIC S9(08)  COMP  VALUE +0.         ELPRDSNA
00071          10  WS-PAD-CHAR      PIC X             VALUE             ELPRDSNA
00072                                                  LOW-VALUES.      ELPRDSNA
00073 *                                                                 ELPRDSNA
00074  01  WS-SWITCHES.                                                 ELPRDSNA
00075      05  OPEN-CLOSED-SWITCH   PIC X             VALUE 'C'.        ELPRDSNA
00076          88  FILE-OPENED                        VALUE 'O'.        ELPRDSNA
00077          88  FILE-CLOSED                        VALUE 'C'.        ELPRDSNA
00078      05  WS-END-OF-FILE-IND       PIC X         VALUE 'M'.        ELPRDSNA
00079          88  MORE-RECORDS                       VALUE 'M'.        ELPRDSNA
00080          88  END-OF-FILE                        VALUE 'E'.        ELPRDSNA
00081 *                                                                 ELPRDSNA
00082  01  FILLER              PIC X(17)   VALUE '*END OF ELPRDSNA*'.   ELPRDSNA
00083 /                                                                 ELPRDSNA
00084  LINKAGE SECTION.                                                 ELPRDSNA
00085  COPY ELSRDPAC.                                                   ELPRDSNA
00086 /                                                                 ELPRDSNA
00087  PROCEDURE DIVISION USING RDP-READ-PARAMETERS.                    ELPRDSNA
00088      PERFORM INITIALIZATION.                                      ELPRDSNA
00089      PERFORM PROCESS-READ-SNAPSHOT.                               ELPRDSNA
00090      GOBACK.                                                      ELPRDSNA
00091 *                                                                 ELPRDSNA
00092  INITIALIZATION.                                                  ELPRDSNA
00093      MOVE 1 TO WS-MOVE-POS.                                       ELPRDSNA
00094      INITIALIZE WS-BLOCK-SIZE-ACCUM.                              ELPRDSNA
00095 *                                                                 ELPRDSNA
00096  PROCESS-READ-SNAPSHOT.                                           ELPRDSNA
00097      IF RDP-READ-FILE AND FILE-OPENED                             ELPRDSNA
00098         PERFORM READ-DATA-BLOCK                                   ELPRDSNA
00099      ELSE                                                         ELPRDSNA
00100         IF RDP-OPEN-FILE AND FILE-CLOSED                          ELPRDSNA
00101            PERFORM OPEN-FILE-ROUTINE                              ELPRDSNA
00102         ELSE                                                      ELPRDSNA
00103            IF RDP-CLOSE-FILE AND FILE-OPENED                      ELPRDSNA
00104                PERFORM CLOSE-FILE-ROUTINE                         ELPRDSNA
00105            ELSE                                                   ELPRDSNA
00106               PERFORM HANDLE-INVALID-READ-PARM.                   ELPRDSNA
00107 *                                                                 ELPRDSNA
00108  READ-DATA-BLOCK.                                                 ELPRDSNA
00109      READ SNAP-SHOT-FILE                                          ELPRDSNA
00110         AT END                                                    ELPRDSNA
00111             SET END-OF-FILE TO TRUE                               ELPRDSNA
00112             SET RDP-EOF TO TRUE.                                  ELPRDSNA
00113      IF MORE-RECORDS                                              ELPRDSNA
00114         ADD SSR-SUB-SIZE TO WS-BLOCK-SIZE-ACCUM                   ELPRDSNA
00115         MOVE SSR-SEQUENCE-NUM TO WS-PREV-SEQ-NUM                  ELPRDSNA
00116         SET RDP-CALL-OK TO TRUE                                   ELPRDSNA
00117         INITIALIZE RDP-RECORD                                     ELPRDSNA
00118         PERFORM DO-SSR-TO-RDP-MOVES                               ELPRDSNA
00119         PERFORM CALL-TO-ELUMVCL                                   ELPRDSNA
00120         IF SSR-AREA-LENGTH = WS-BLOCK-SIZE-ACCUM                  ELPRDSNA
00121             OR (SSR-AREA-LENGTH = 0)                              ELPRDSNA
00122                CONTINUE                                           ELPRDSNA
00123         ELSE                                                      ELPRDSNA
00124             IF SSR-AREA-LENGTH > LENGTH OF RDP-AREA               ELPRDSNA
00125                 MOVE SSR-SUB-SIZE TO RDP-AREA-LENGTH              ELPRDSNA
00126             ELSE                                                  ELPRDSNA
00127                 PERFORM READ-ROUTINE                              ELPRDSNA
00128                     UNTIL (SSR-AREA-LENGTH =                      ELPRDSNA
00129                        WS-BLOCK-SIZE-ACCUM) OR RDP-UNEXPECTED-EOF ELPRDSNA
00130                          OR RDP-SHORT-RECORD.                     ELPRDSNA
00131 *                                                                 ELPRDSNA
00132  READ-ROUTINE.                                                    ELPRDSNA
00133      READ SNAP-SHOT-FILE                                          ELPRDSNA
00134          AT END                                                   ELPRDSNA
00135             SET END-OF-FILE TO TRUE                               ELPRDSNA
00136             SET RDP-UNEXPECTED-EOF TO TRUE.                       ELPRDSNA
00137      IF MORE-RECORDS                                              ELPRDSNA
00138          ADD SSR-SUB-SIZE TO WS-BLOCK-SIZE-ACCUM                  ELPRDSNA
00139          PERFORM CONTINUE-BUILDING-DATA-BLOCK.                    ELPRDSNA
00140 *                                                                 ELPRDSNA
00141  CONTINUE-BUILDING-DATA-BLOCK.                                    ELPRDSNA
00142      IF SSR-SEQUENCE-NUM = WS-PREV-SEQ-NUM + 1                    ELPRDSNA
00143           AND ((SSR-ABEND-CODE = RDP-ABEND-CODE) AND              ELPRDSNA
00144               (SSR-TERMINAL-ID = RDP-TERMINAL-ID) AND             ELPRDSNA
00145               (SSR-ABEND-DATE = RDP-ABEND-DATE) AND               ELPRDSNA
00146                (SSR-ABEND-TIME = RDP-ABEND-TIME))                 ELPRDSNA
00147           MOVE SSR-SEQUENCE-NUM TO WS-PREV-SEQ-NUM                ELPRDSNA
00148           PERFORM CALL-TO-ELUMVCL                                 ELPRDSNA
00149      ELSE                                                         ELPRDSNA
00150           PERFORM HANDLE-SHORT-RECORD-FOUND.                      ELPRDSNA
00151 *                                                                 ELPRDSNA
00152  DO-SSR-TO-RDP-MOVES.                                             ELPRDSNA
00153      MOVE SSR-RECORD-TYPE TO RDP-RECORD-TYPE.                     ELPRDSNA
00154      MOVE SSR-CICS-SYSTEM-ID TO RDP-CICS-SYSTEM-ID.               ELPRDSNA
00155      MOVE SSR-CICS-APPL-ID TO RDP-CICS-APPL-ID.                   ELPRDSNA
00156      MOVE SSR-ABEND-DATE TO RDP-ABEND-DATE.                       ELPRDSNA
00157      MOVE SSR-ABEND-TIME TO RDP-ABEND-TIME.                       ELPRDSNA
00158      MOVE SSR-TERMINAL-ID TO RDP-TERMINAL-ID.                     ELPRDSNA
00159      MOVE SSR-ABEND-CODE TO RDP-ABEND-CODE.                       ELPRDSNA
00160      MOVE SSR-DDNAME TO RDP-DDNAME.                               ELPRDSNA
00161      MOVE SSR-SORT-ID TO RDP-SORT-ID.                             ELPRDSNA
00162      SET RDP-CICS-AREA-PTR TO SSR-AREA-PTR.                       ELPRDSNA
00163      MOVE SSR-AREA-LENGTH TO RDP-AREA-LENGTH.                     ELPRDSNA
00164 *                                                                 ELPRDSNA
00165  CALL-TO-ELUMVCL.                                                 ELPRDSNA
00166      MOVE SSR-SUB-SIZE TO WS-MOVE-LEN.                            ELPRDSNA
00167      CALL 'ELUMVCL' USING SSR-SUB-REC(1)                          ELPRDSNA
00168                           WS-MOVE-LEN                             ELPRDSNA
00169                           RDP-AREA-CHAR(WS-MOVE-POS)              ELPRDSNA
00170                           WS-MOVE-LEN                             ELPRDSNA
00171                           WS-PAD-CHAR.                            ELPRDSNA
00172      ADD WS-MOVE-LEN TO WS-MOVE-POS.                              ELPRDSNA
00173 *                                                                 ELPRDSNA
00174  OPEN-FILE-ROUTINE.                                               ELPRDSNA
00175      SET FILE-OPENED TO TRUE.                                     ELPRDSNA
00176      OPEN INPUT SNAP-SHOT-FILE.                                   ELPRDSNA
00177 *                                                                 ELPRDSNA
00178  CLOSE-FILE-ROUTINE.                                              ELPRDSNA
00179      SET FILE-CLOSED TO TRUE.                                     ELPRDSNA
00180      CLOSE SNAP-SHOT-FILE.                                        ELPRDSNA
00181 *                                                                 ELPRDSNA
00182  HANDLE-INVALID-READ-PARM.                                        ELPRDSNA
00183      SET RDP-INVALID-DATA TO TRUE.                                ELPRDSNA
00184 *                                                                 ELPRDSNA
00185  HANDLE-SHORT-RECORD-FOUND.                                       ELPRDSNA
00186      SET RDP-SHORT-RECORD TO TRUE.                                ELPRDSNA
