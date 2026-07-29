00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPCNV  
00003  PROGRAM-ID.         ELPCNV.                                         LV001
00004                                                                   ELPCNV  
00005  AUTHOR.             LUCY TORRES                                  ELPCNV  
00006                                                                   ELPCNV  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPCNV  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPCNV  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPCNV  
00010                      233 N. MICHIGAN AVE                          ELPCNV  
00011                      CHICAGO, ILLINOIS 60601                      ELPCNV  
00012                                                                   ELPCNV  
00013  DATE-WRITTEN.       06-OCT-1988.                                 ELPCNV  
00014                                                                   ELPCNV  
00015  DATE-COMPILED.                                                   ELPCNV  
00016                                                                   ELPCNV  
00017  SECURITY.           COPYRIGHT 1988,                              ELPCNV  
00018                      HEALTH CARE SERVICE CORPORATION              ELPCNV  
00019      SKIP3                                                        ELPCNV  
00020  ENVIRONMENT DIVISION.                                            ELPCNV  
00021                                                                   ELPCNV  
00022  CONFIGURATION SECTION.                                           ELPCNV  
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELPCNV  
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELPCNV  
00025      EJECT                                                        ELPCNV  
00026 ******************************************************************ELPCNV  
00027 *                                                                *ELPCNV  
00028 *                 SPECIAL BENEFIT MESSAGES SUBSYSTEM             *ELPCNV  
00029 *                                                                *ELPCNV  
00030 *   ELPCNV - THIS MODULE IS A ONE SHOT THAT WILL LOAD   THE      *ELPCNV  
00031 *             UNIQUE KEY GENERATOR RECORD INTO THE GROUP KEY     *ELPCNV  
00032 *             FILE.  IT WILL ALSO LOAD A RECORD OF LOW-VALUES    *ELPCNV  
00033 *             INTO THE MESSAGE KEY FILE.                         *ELPCNV  
00034 *                                                                *ELPCNV  
00035 ******************************************************************ELPCNV  
00036 *                                                                *ELPCNV  
00037 *                      MAINTENANCE HISTORY                       *ELPCNV  
00038 *                                                                *ELPCNV  
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELPCNV  
00040 * ----- ----------- --- ----- ---------------------------------- *ELPCNV  
00041 * 01.00 06-OCT-1988 LET       CREATED                            *ELPCNV  
00042 *                                                                *ELPCNV  
00043 ******************************************************************ELPCNV  
00044                                                                   ELPCNV  
00045  INPUT-OUTPUT SECTION.                                            ELPCNV  
00046  FILE-CONTROL.                                                    ELPCNV  
00047      SELECT OLD-MESSAGE-FILE ASSIGN TO ELPMSGO                    ELPCNV  
00048                            ORGANIZATION IS INDEXED                ELPCNV  
00049                            ACCESS IS SEQUENTIAL                   ELPCNV  
00050                            RECORD KEY IS MESSAGE-KEY              ELPCNV  
00051                            FILE STATUS IS OLD-MSG-STATUS-KEY.     ELPCNV  
00052                                                                   ELPCNV  
00053      SELECT NEW-MESSAGE-FILE ASSIGN TO ELPMSGN                    ELPCNV  
00054                              ORGANIZATION IS INDEXED              ELPCNV  
00055                              ACCESS IS RANDOM                     ELPCNV  
00056                              RECORD KEY IS MESSAGE2-KEY           ELPCNV  
00057                              FILE STATUS IS NEW-MSG-STATUS-KEY.   ELPCNV  
00058                                                                   ELPCNV  
00059  DATA DIVISION.                                                   ELPCNV  
00060  FILE SECTION.                                                    ELPCNV  
00061  FD  OLD-MESSAGE-FILE                                             ELPCNV  
00062      LABEL RECORDS ARE STANDARD.                                  ELPCNV  
00063  01  OLD-MESSAGE-RECORD.                                          ELPCNV  
00064      05  MESSAGE-KEY.                                             ELPCNV  
00065          10  MESSAGE-TYPE                PIC  X(01).              ELPCNV  
00066              88  MESSAGE-COMMON                     VALUE 'C'.    ELPCNV  
00067              88  MESSAGE-UNIQUE                     VALUE 'X'.    ELPCNV  
00068          10  MESSAGE-NUMBER              PIC  X(04).              ELPCNV  
00069      05  MESSAGE-RETRIEVED-IND           PIC  X(01).              ELPCNV  
00070          88  MSG-WAS-RETRIEVED                      VALUE 'R'.    ELPCNV  
00071          88  MSG-WAS-NOT-RETRIEVED                  VALUE ' '.    ELPCNV  
00072      05  MESSAGE-REFERENCE-COUNT         PIC S9(04)   COMP.       ELPCNV  
00073      05  MESSAGE-LINE-COUNT              PIC S9(03)   COMP.       ELPCNV  
00074          88  MAXIMUM-MESSAGE-LINES                    VALUE +015. ELPCNV  
00075      05  MESSAGE-LINE-ENTRY              PIC  X(72)               ELPCNV  
00076                                          OCCURS 1 TO 15 TIMES     ELPCNV  
00077                                          DEPENDING ON             ELPCNV  
00078                                          MESSAGE-LINE-COUNT       ELPCNV  
00079                                          INDEXED BY MSG-LINE-IDX. ELPCNV  
00080  FD  NEW-MESSAGE-FILE                                             ELPCNV  
00081      LABEL RECORDS ARE STANDARD.                                  ELPCNV  
00082  01  SPECIAL-MESSAGE2-RECORD.                                     ELPCNV  
00083      05  MESSAGE2-KEY.                                            ELPCNV  
00084          10  MESSAGE2-TYPE               PIC  X(01).              ELPCNV  
00085              88  MESSAGE2-COMMON                    VALUE 'C'.    ELPCNV  
00086              88  MESSAGE2-UNIQUE                    VALUE 'X'.    ELPCNV  
00087          10  MESSAGE2-NUMBER             PIC  X(04).              ELPCNV  
00088      05  MESSAGE2-RETRIEVED-IND          PIC  X(01).              ELPCNV  
00089          88  MSG2-RETRIEVED                         VALUE 'R'.    ELPCNV  
00090          88  MSG2-NOT-RETRIEVED                     VALUE ' '.    ELPCNV  
00091      05  FILLER                          PIC  X(10) VALUE SPACES. ELPCNV  
00092      05  MESSAGE2-REFERENCE-COUNT        PIC S9(04)   COMP.       ELPCNV  
00093      05  MESSAGE2-LINE-COUNT             PIC S9(03)   COMP.       ELPCNV  
00094          88  MAXIMUM-MESSAGE2-LINES                   VALUE +015. ELPCNV  
00095      05  MESSAGE2-TEXT-AREA.                                      ELPCNV  
00096          10  MESSAGE2-LINE-ENTRY         PIC  X(72)               ELPCNV  
00097                                          OCCURS 1 TO 15 TIMES     ELPCNV  
00098                                          DEPENDING ON             ELPCNV  
00099                                          MESSAGE2-LINE-COUNT      ELPCNV  
00100                                          INDEXED BY MSG2-LINE-IDX.ELPCNV  
00101 /                                                                 ELPCNV  
00102  WORKING-STORAGE SECTION.                                         ELPCNV  
00103  01  WS-ELPCNV-BEGIN             PIC  X(28) VALUE                 ELPCNV  
00104          '** ELPCNV WS BEGINS HERE **'.                           ELPCNV  
00105                                                                   ELPCNV  
00106  01  SWITCHES.                                                    ELPCNV  
00107      05  WS-OLD-MSG-EOF-SW       PIC  X(01) VALUE SPACE.          ELPCNV  
00108          88  OLD-MSG-EOF                    VALUE '1'.            ELPCNV  
00109                                                                   ELPCNV  
00110  01  WS-STATUS-KEYS.                                              ELPCNV  
00111      05  OLD-MSG-STATUS-KEY      PIC  X(02).                      ELPCNV  
00112      05  NEW-MSG-STATUS-KEY      PIC  X(02).                      ELPCNV  
00113                                                                   ELPCNV  
00114  01  ABEND-CODE                  PIC  9(04) COMP VALUE 0.         ELPCNV  
00115                                                                   ELPCNV  
00116  01  WS-ELPCNV-FINISH            PIC  X(28) VALUE                 ELPCNV  
00117          '** ELPCNV WS  ENDS  HERE **'.                           ELPCNV  
00118 /                                                                 ELPCNV  
00119  PROCEDURE DIVISION.                                              ELPCNV  
00120      PERFORM 100-INITIALIZE.                                      ELPCNV  
00121      PERFORM 500-PROCESS.                                         ELPCNV  
00122      PERFORM 900-TERMINATE.                                       ELPCNV  
00123 /                                                                 ELPCNV  
00124  100-INITIALIZE.                                                  ELPCNV  
00125      INITIALIZE OLD-MSG-STATUS-KEY                                ELPCNV  
00126                 NEW-MSG-STATUS-KEY                                ELPCNV  
00127                 WS-OLD-MSG-EOF-SW.                                ELPCNV  
00128                                                                   ELPCNV  
00129      OPEN INPUT  OLD-MESSAGE-FILE.                                ELPCNV  
00130      DISPLAY 'OLD KEY STATUS ' OLD-MSG-STATUS-KEY.                ELPCNV  
00131      IF OLD-MSG-STATUS-KEY  NOT = '00'                            ELPCNV  
00132         PERFORM 999-ABEND-THE-PROGRAM.                            ELPCNV  
00133      OPEN I-O    NEW-MESSAGE-FILE.                                ELPCNV  
00134      DISPLAY 'NEW KEY STATUS ' NEW-MSG-STATUS-KEY.                ELPCNV  
00135      IF NEW-MSG-STATUS-KEY  NOT = '00'                            ELPCNV  
00136         PERFORM 999-ABEND-THE-PROGRAM.                            ELPCNV  
00137                                                                   ELPCNV  
00138  500-PROCESS.                                                     ELPCNV  
00139      PERFORM 600-READ-OLD-MSG-FILE 2 TIMES.                       ELPCNV  
00140      PERFORM 700-PROCESS-OLD-MSG-FILE                             ELPCNV  
00141        UNTIL OLD-MSG-EOF.                                         ELPCNV  
00142      DISPLAY 'CLOSING FILES'.                                     ELPCNV  
00143      PERFORM 800-CLOSE-ALL-OPENED-FILES.                          ELPCNV  
00144                                                                   ELPCNV  
00145  600-READ-OLD-MSG-FILE.                                           ELPCNV  
00146      READ OLD-MESSAGE-FILE                                        ELPCNV  
00147           AT END SET OLD-MSG-EOF  TO  TRUE.                       ELPCNV  
00148                                                                   ELPCNV  
00149  700-PROCESS-OLD-MSG-FILE.                                        ELPCNV  
00150      IF NOT OLD-MSG-EOF                                           ELPCNV  
00151         PERFORM 710-REFORMAT-MESSAGE-RECORD.                      ELPCNV  
00152                                                                   ELPCNV  
00153  710-REFORMAT-MESSAGE-RECORD.                                     ELPCNV  
00154      MOVE MESSAGE-LINE-COUNT         TO  MESSAGE2-LINE-COUNT.     ELPCNV  
00155      MOVE MESSAGE-KEY                TO  MESSAGE2-KEY.            ELPCNV  
00156      MOVE MESSAGE-RETRIEVED-IND      TO  MESSAGE2-RETRIEVED-IND.  ELPCNV  
00157      MOVE MESSAGE-REFERENCE-COUNT    TO  MESSAGE2-REFERENCE-COUNT.ELPCNV  
00158      PERFORM 712-MOVE-IN-MESSAGE-TEXT                             ELPCNV  
00159              VARYING MSG-LINE-IDX FROM +1 BY +1                   ELPCNV  
00160              UNTIL   MSG-LINE-IDX > MESSAGE-LINE-COUNT.           ELPCNV  
00161                                                                   ELPCNV  
00162      PERFORM 714-WRITE-NEW-MESSAGE-RECORD.                        ELPCNV  
00163      PERFORM 600-READ-OLD-MSG-FILE.                               ELPCNV  
00164                                                                   ELPCNV  
00165  712-MOVE-IN-MESSAGE-TEXT.                                        ELPCNV  
00166      SET MSG2-LINE-IDX  TO  MSG-LINE-IDX.                         ELPCNV  
00167      MOVE MESSAGE-LINE-ENTRY (MSG-LINE-IDX)                       ELPCNV  
00168                        TO  MESSAGE2-LINE-ENTRY (MSG2-LINE-IDX).   ELPCNV  
00169                                                                   ELPCNV  
00170  714-WRITE-NEW-MESSAGE-RECORD.                                    ELPCNV  
00171      WRITE SPECIAL-MESSAGE2-RECORD                                ELPCNV  
00172         INVALID KEY PERFORM 999-ABEND-THE-PROGRAM.                ELPCNV  
00173                                                                   ELPCNV  
00174  800-CLOSE-ALL-OPENED-FILES.                                      ELPCNV  
00175      CLOSE OLD-MESSAGE-FILE, NEW-MESSAGE-FILE.                    ELPCNV  
00176                                                                   ELPCNV  
00177  900-TERMINATE.                                                   ELPCNV  
00178      STOP RUN.                                                    ELPCNV  
00179                                                                   ELPCNV  
00180  999-ABEND-THE-PROGRAM.                                           ELPCNV  
00181      DISPLAY 'O STATUS KEY INFO IS  ' OLD-MSG-STATUS-KEY.         ELPCNV  
00182      DISPLAY 'N STATUS KEY INFO IS  ' NEW-MSG-STATUS-KEY.         ELPCNV  
00183      DISPLAY 'RECORD KEY IS '  MESSAGE-KEY.                       ELPCNV  
00184      CALL 'TSGEND' USING ABEND-CODE.                              ELPCNV  
