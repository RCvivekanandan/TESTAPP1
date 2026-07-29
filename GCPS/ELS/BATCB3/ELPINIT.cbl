00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPINIT 
00003  PROGRAM-ID.         ELPINIT.                                        LV001
00004                                                                   ELPINIT 
00005  AUTHOR.             LUCY TORRES                                  ELPINIT 
00006                                                                   ELPINIT 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPINIT 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPINIT 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPINIT 
00010                      233 N. MICHIGAN AVE                          ELPINIT 
00011                      CHICAGO, ILLINOIS 60601                      ELPINIT 
00012                                                                   ELPINIT 
00013  DATE-WRITTEN.       12-JUL-1988.                                 ELPINIT 
00014                                                                   ELPINIT 
00015  DATE-COMPILED.                                                   ELPINIT 
00016                                                                   ELPINIT 
00017  SECURITY.           COPYRIGHT 1988,                              ELPINIT 
00018                      HEALTH CARE SERVICE CORPORATION              ELPINIT 
00019      SKIP3                                                        ELPINIT 
00020  ENVIRONMENT DIVISION.                                            ELPINIT 
00021                                                                   ELPINIT 
00022  CONFIGURATION SECTION.                                           ELPINIT 
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELPINIT 
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELPINIT 
00025      EJECT                                                        ELPINIT 
00026 ******************************************************************ELPINIT 
00027 *                                                                *ELPINIT 
00028 *                 SPECIAL BENEFIT MESSAGES SUBSYSTEM             *ELPINIT 
00029 *                                                                *ELPINIT 
00030 *   ELPINIT - THIS MODULE IS A ONE SHOT THAT WILL LOAD  THE      *ELPINIT 
00031 *             UNIQUE KEY GENERATOR RECORD INTO THE GROUP KEY     *ELPINIT 
00032 *             FILE.  IT WILL ALSO LOAD A RECORD OF LOW-VALUES    *ELPINIT 
00033 *             INTO THE MESSAGE KEY FILE.                         *ELPINIT 
00034 *                                                                *ELPINIT 
00035 ******************************************************************ELPINIT 
00036 *                                                                *ELPINIT 
00037 *                      MAINTENANCE HISTORY                       *ELPINIT 
00038 *                                                                *ELPINIT 
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELPINIT 
00040 * ----- ----------- --- ----- ---------------------------------- *ELPINIT 
00041 * 01.00 12-JUL-1988 LET       CREATED                            *ELPINIT 
00042 *                                                                *ELPINIT 
00043 ******************************************************************ELPINIT 
00044                                                                   ELPINIT 
00045  INPUT-OUTPUT SECTION.                                            ELPINIT 
00046  FILE-CONTROL.                                                    ELPINIT 
00047      SELECT GROUP-KEY-FILE ASSIGN TO ELPGRPKY                     ELPINIT 
00048                            ORGANIZATION IS INDEXED                ELPINIT 
00049                            ACCESS IS SEQUENTIAL                   ELPINIT 
00050                            RECORD KEY IS UNIQUE-KEY-ID            ELPINIT 
00051                            FILE STATUS IS GRP-FILE-STATUS-KEY.    ELPINIT 
00052                                                                   ELPINIT 
00053      SELECT MESSAGE-KEY-FILE ASSIGN TO ELPMSGKY                   ELPINIT 
00054                              ORGANIZATION IS INDEXED              ELPINIT 
00055                              ACCESS IS SEQUENTIAL                 ELPINIT 
00056                              RECORD KEY IS MESSAGE-KEY            ELPINIT 
00057                              FILE STATUS IS MSG-FILE-STATUS-KEY   ELPINIT 
00058                                             MSG-OTHER-RET.        ELPINIT 
00059                                                                   ELPINIT 
00060  DATA DIVISION.                                                   ELPINIT 
00061  FILE SECTION.                                                    ELPINIT 
00062  FD  GROUP-KEY-FILE                                               ELPINIT 
00063      LABEL RECORDS ARE STANDARD.                                  ELPINIT 
00064  01  GROUP-KEY-FILE-RECORD.                                       ELPINIT 
00065      05  UNIQUE-KEY-ID           PIC  X(56).                      ELPINIT 
00066      05  FILLER                  PIC  X(105).                     ELPINIT 
00067                                                                   ELPINIT 
00068  FD  MESSAGE-KEY-FILE                                             ELPINIT 
00069      LABEL RECORDS ARE STANDARD.                                  ELPINIT 
00070      COPY ELSMSGRC.                                               ELPINIT 
00071 /                                                                 ELPINIT 
00072  WORKING-STORAGE SECTION.                                         ELPINIT 
00073  01  WS-ELPINIT-BEGIN            PIC  X(28) VALUE                 ELPINIT 
00074          '** ELPINIT WS BEGINS HERE **'.                          ELPINIT 
00075                                                                   ELPINIT 
00076  01  GRP-FILE-STATUS-KEY.                                         ELPINIT 
00077      05  GRP-STATUS-KEY1         PIC  X(01) VALUE ZERO.           ELPINIT 
00078      05  GRP-STATUS-KEY2         PIC  X(01) VALUE ZERO.           ELPINIT 
00079                                                                   ELPINIT 
00080  01  MSG-FILE-STATUS-KEY.                                         ELPINIT 
00081      05  MSG-STATUS-KEY1         PIC  X(01).                      ELPINIT 
00082      05  MSG-STATUS-KEY2         PIC  X(01).                      ELPINIT 
00083                                                                   ELPINIT 
00084  01  MSG-OTHER-RET.                                               ELPINIT 
00085      05  MSG-RETURN              PIC  9(02) COMP VALUE 0.         ELPINIT 
00086      05  MSG-FUNCTION            PIC  9(02) COMP VALUE 0.         ELPINIT 
00087      05  MSG-FEEDBACK            PIC  9(02) COMP VALUE 0.         ELPINIT 
00088                                                                   ELPINIT 
00089  01  ABEND-CODE                  PIC  9(04) COMP VALUE 0.         ELPINIT 
00090                                                                   ELPINIT 
00091 *******************************************************           ELPINIT 
00092 *      UNIQUE MESSAGE KEY GENERATOR RECORD LAYOUT     *           ELPINIT 
00093 *******************************************************           ELPINIT 
00094      COPY ELSKYGEN.                                               ELPINIT 
00095 /                                                                 ELPINIT 
00096 *******************************************************           ELPINIT 
00097 *      MESSAGE FILE RECORD LAYOUT                     *           ELPINIT 
00098 *******************************************************           ELPINIT 
00099      COPY ELSMSGR2.                                               ELPINIT 
00100                                                                   ELPINIT 
00101  01  WS-ELPINIT-FINISH           PIC  X(28) VALUE                 ELPINIT 
00102          '** ELPINIT WS  ENDS  HERE **'.                          ELPINIT 
00103 /                                                                 ELPINIT 
00104  PROCEDURE DIVISION.                                              ELPINIT 
00105      PERFORM 100-INITIALIZE.                                      ELPINIT 
00106      PERFORM 500-PROCESS.                                         ELPINIT 
00107      PERFORM 900-TERMINATE.                                       ELPINIT 
00108 /                                                                 ELPINIT 
00109  100-INITIALIZE.                                                  ELPINIT 
00110      DISPLAY 'OPENING FILES'.                                     ELPINIT 
00111      OPEN OUTPUT GROUP-KEY-FILE, MESSAGE-KEY-FILE.                ELPINIT 
00112      IF MSG-FILE-STATUS-KEY NOT = '00'                            ELPINIT 
00113         DISPLAY 'MSG KEY STATUS ' MSG-FILE-STATUS-KEY             ELPINIT 
00114         PERFORM 999-ABEND-THE-PROGRAM.                            ELPINIT 
00115      IF GRP-FILE-STATUS-KEY NOT = '00'                            ELPINIT 
00116         DISPLAY 'GRP KEY STATUS ' GRP-FILE-STATUS-KEY             ELPINIT 
00117         PERFORM 999-ABEND-THE-PROGRAM.                            ELPINIT 
00118                                                                   ELPINIT 
00119  500-PROCESS.                                                     ELPINIT 
00120      DISPLAY 'CREATING KEY GENERATOR'.                            ELPINIT 
00121      PERFORM 505-CREATE-KEY-GENERATOR-REC.                        ELPINIT 
00122      DISPLAY 'WRITING TO GROUP FILE'.                             ELPINIT 
00123      PERFORM 510-WRITE-GROUP-FILE-RECORD.                         ELPINIT 
00124      DISPLAY 'CREATING MESSAGE RECORD'.                           ELPINIT 
00125      PERFORM 515-CREATE-INITIAL-MSG-REC.                          ELPINIT 
00126      DISPLAY 'WRITING TO MESSAGE FILE'.                           ELPINIT 
00127      PERFORM 520-WRITE-MESSAGE-RECORD.                            ELPINIT 
00128      DISPLAY 'CLOSING FILES'.                                     ELPINIT 
00129      PERFORM 599-CLOSE-ALL-OPENED-FILES.                          ELPINIT 
00130                                                                   ELPINIT 
00131  505-CREATE-KEY-GENERATOR-REC.                                    ELPINIT 
00132      MOVE LOW-VALUES          TO  GENERATOR-KEY.                  ELPINIT 
00133      MOVE ZEROS               TO  GENERATED-UNIQUE-NUMBER         ELPINIT 
00134                                   LAST-COMMON-MESSAGE-USED.       ELPINIT 
00135      MOVE KEY-GENERATOR-AREA  TO  GROUP-KEY-FILE-RECORD.          ELPINIT 
00136                                                                   ELPINIT 
00137  510-WRITE-GROUP-FILE-RECORD.                                     ELPINIT 
00138      WRITE GROUP-KEY-FILE-RECORD                                  ELPINIT 
00139         INVALID KEY PERFORM 999-ABEND-THE-PROGRAM.                ELPINIT 
00140                                                                   ELPINIT 
00141  515-CREATE-INITIAL-MSG-REC.                                      ELPINIT 
00142      INITIALIZE MESSAGE2-KEY,                                     ELPINIT 
00143                 MESSAGE2-REFERENCE-COUNT.                         ELPINIT 
00144      MOVE +1      TO  MESSAGE2-LINE-COUNT.                        ELPINIT 
00145      MOVE SPACES  TO  MESSAGE2-LINE-ENTRY (1).                    ELPINIT 
00146                                                                   ELPINIT 
00147      MOVE MESSAGE2-LINE-COUNT      TO  MESSAGE-LINE-COUNT.        ELPINIT 
00148      MOVE SPECIAL-MESSAGE2-RECORD  TO  SPECIAL-MESSAGE-RECORD.    ELPINIT 
00149                                                                   ELPINIT 
00150  520-WRITE-MESSAGE-RECORD.                                        ELPINIT 
00151      WRITE SPECIAL-MESSAGE-RECORD                                 ELPINIT 
00152         INVALID KEY PERFORM 999-ABEND-THE-PROGRAM.                ELPINIT 
00153                                                                   ELPINIT 
00154  599-CLOSE-ALL-OPENED-FILES.                                      ELPINIT 
00155      CLOSE GROUP-KEY-FILE, MESSAGE-KEY-FILE.                      ELPINIT 
00156 *    CLOSE MESSAGE-KEY-FILE.                                      ELPINIT 
00157                                                                   ELPINIT 
00158  900-TERMINATE.                                                   ELPINIT 
00159      STOP RUN.                                                    ELPINIT 
00160                                                                   ELPINIT 
00161  999-ABEND-THE-PROGRAM.                                           ELPINIT 
00162      DISPLAY 'STATUS KEY INFO IS  ' GRP-FILE-STATUS-KEY.          ELPINIT 
00163      DISPLAY 'STATUS KEY INFO IS  ' MSG-FILE-STATUS-KEY.          ELPINIT 
00164      CALL 'TSGEND' USING ABEND-CODE.                              ELPINIT 
