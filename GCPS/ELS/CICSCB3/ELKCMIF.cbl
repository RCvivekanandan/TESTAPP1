00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELKCMIF 
00003  PROGRAM-ID.         ELKCMIF.                                        LV002
00004                                                                   ELKCMIF 
00005  AUTHOR.             EDWARD G LISS                                ELKCMIF 
00006                                                                   ELKCMIF 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELKCMIF 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELKCMIF 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELKCMIF 
00010                      233 N. MICHIGAN AVE                          ELKCMIF 
00011                      CHICAGO, ILLINOIS 60601                      ELKCMIF 
00012                                                                   ELKCMIF 
00013  DATE-WRITTEN.       16-JAN-1990.                                 ELKCMIF 
00014                                                                   ELKCMIF 
00015  DATE-COMPILED.                                                   ELKCMIF 
00016                                                                   ELKCMIF 
00017  SECURITY.           COPYRIGHT 1990,                              ELKCMIF 
00018                      HEALTH CARE SERVICE CORPORATION              ELKCMIF 
00019      SKIP3                                                        ELKCMIF 
00020 ******************************************************************ELKCMIF 
00021 *                                                                *ELKCMIF 
00022 *    PROGRAM:    ELKCMIF                                         *ELKCMIF 
00023 *    DATE:       16-JAN-1990                                     *ELKCMIF 
00024 *    AUTHOR:     EDWARD G LISS                                   *ELKCMIF 
00025 *    FUNCTION:                                                   *ELKCMIF 
00026 *      THIS MODULE ACCESS THE CODES MANUAL TO TRANSLATE THE      *ELKCMIF 
00027 *      GIVEN CODE TO A ENGLISH PHASE.                            *ELKCMIF 
00028 *                                                                *ELKCMIF 
00029 ******************************************************************ELKCMIF 
00030 *                                                                *ELKCMIF 
00031 *                      MAINTENANCE HISTORY                       *ELKCMIF 
00032 *                                                                *ELKCMIF 
00033 *  MOD     DATE     BY  DRPT                ACTION               *ELKCMIF 
00034 * ----- ----------- --- ----- ---------------------------------- *ELKCMIF 
00035 * 01.00 16-JAN-1990 EGL       CREATED                            *ELKCMIF 
00036 * 02.00 23-JAN-1990 RKH       CHANGED PARA 6040-NEW-CODE-VALUE   *ELKCMIF 
00037 *                             CODE-VALUE DELIMINTED BY SPACE     *ELKCMIF 
00038 *                CHANGED TO : CODE-VALUE DELIMINTED BY SIZE      *ELKCMIF 
00039 *    08/12/03 AKK TESTING ORDER OF COMPILE                    *   ELKCMIF 
00040 ******************************************************************ELKCMIF 
00041                                                                   ELKCMIF 
00042  ENVIRONMENT DIVISION.                                            ELKCMIF 
00043                                                                   ELKCMIF 
00044  CONFIGURATION SECTION.                                           ELKCMIF 
00045  SOURCE-COMPUTER.    IBM-3033.                                    ELKCMIF 
00046  OBJECT-COMPUTER.    IBM-3033.                                    ELKCMIF 
00047  TITLE 'GENERIC CODES MANUAL INTERFACE MODULE'.                   ELKCMIF 
00048  DATA DIVISION.                                                   ELKCMIF 
00049                                                                   ELKCMIF 
00050  WORKING-STORAGE SECTION.                                         ELKCMIF 
00051                                                                   ELKCMIF 
00052  01  PC-CONSTANTS.                                                ELKCMIF 
00053      05  PC-ELPCN                   PICTURE X(8) VALUE 'ELPCN'.   ELKCMIF 
00054      05  PC-ELPCV                   PICTURE X(8) VALUE 'ELPCV'.   ELKCMIF 
00055      05  PC-ELPDE                   PICTURE X(8) VALUE 'ELPDE'.   ELKCMIF 
00056      05  PC-ELPEN                   PICTURE X(8) VALUE 'ELPEN'.   ELKCMIF 
00057                                                                   ELKCMIF 
00058      05  PC-ELKDESCC                PICTURE X(8) VALUE 'ELKDESCC'.ELKCMIF 
00059      05  PC-ELKTRANC                PICTURE X(8) VALUE 'ELKTRANC'.ELKCMIF 
00060                                                                   ELKCMIF 
00061      05  PC-MAX-TRANS-LINE          PICTURE S9(4) COMP VALUE +400.ELKCMIF 
00062                                                                   ELKCMIF 
00063      05  PC-ABEND-CODE-ELKA         PICTURE X(4) VALUE 'ELKA'.    ELKCMIF 
00064      05  PC-ABEND-CODE-ELKB         PICTURE X(4) VALUE 'ELKB'.    ELKCMIF 
00065      05  PC-ABEND-CODE-ELKC         PICTURE X(4) VALUE 'ELKC'.    ELKCMIF 
00066      05  PC-ABEND-CODE-ELKD         PICTURE X(4) VALUE 'ELKD'.    ELKCMIF 
00067      05  PC-ABEND-CODE-ELKE         PICTURE X(4) VALUE 'ELKE'.    ELKCMIF 
00068      05  PC-ABEND-CODE-ELKF         PICTURE X(4) VALUE 'ELKF'.    ELKCMIF 
00069                                                                   ELKCMIF 
00070  01  MISC-WORKING-STORAGE.                                        ELKCMIF 
00071      05  WS-MAX-DESCR-LINES         PICTURE S9(4) COMP SYNC.      ELKCMIF 
00072      05  WS-ELPCV-REC-LEN           PICTURE S9(4) COMP SYNC.      ELKCMIF 
00073      05  WS-DESC-SUB                PICTURE S9(4) COMP SYNC.      ELKCMIF 
00074      05  WS-GET-MAIN-LENGTH         PICTURE S9(4) COMP SYNC.      ELKCMIF 
00075      05  WS-LINE-COUNT              PICTURE S9(4) COMP SYNC.      ELKCMIF 
00076      05  WS-LOW-VALUE               PICTURE X     VALUE LOW-VALUE.ELKCMIF 
00077      05  WS-TRANS-COMPLETE-SW       PICTURE X     VALUE 'N'.      ELKCMIF 
00078          88  WS-TRANSLATION-COMPLETE              VALUE 'Y'.      ELKCMIF 
00079          88  WS-TRANSLATION-NOT-COMPLETE          VALUE 'N'.      ELKCMIF 
00080      05  WS-CV-ENDBR-SW             PICTURE X     VALUE 'N'.      ELKCMIF 
00081          88  WS-CV-ENDBR-REQ                      VALUE 'Y'.      ELKCMIF 
00082          88  WS-CV-ENDBR-NOT-REQ                  VALUE 'N'.      ELKCMIF 
00083      05  WS-ELPCN-SW                PICTURE X     VALUE 'N'.      ELKCMIF 
00084          88  WS-ELPCN-FOUND                       VALUE 'Y'.      ELKCMIF 
00085          88  WS-ELPCN-NOT-FOUND                   VALUE 'N'.      ELKCMIF 
00086      05  WS-ELPEN-SW                PICTURE X     VALUE 'N'.      ELKCMIF 
00087          88  WS-ELPEN-FOUND                       VALUE 'Y'.      ELKCMIF 
00088          88  WS-ELPEN-NOT-FOUND                   VALUE 'N'.      ELKCMIF 
00089      05  WS-ELPDE-SW                PICTURE X     VALUE 'N'.      ELKCMIF 
00090          88  WS-ELPDE-FOUND                       VALUE 'Y'.      ELKCMIF 
00091          88  WS-ELPDE-NOT-FOUND                   VALUE 'N'.      ELKCMIF 
00092      05  WS-ELPCV-SW                PICTURE X     VALUE 'N'.      ELKCMIF 
00093          88  WS-ELPCV-FOUND                       VALUE 'Y'.      ELKCMIF 
00094          88  WS-ELPCV-NOT-FOUND                   VALUE 'N', 'E'. ELKCMIF 
00095          88  WS-ELPCV-EOF                         VALUE 'E'.      ELKCMIF 
00096      05  WS-CENTER-LINE.                                          ELKCMIF 
00097          10  FILLER                 PICTURE X(4)  VALUE SPACES.   ELKCMIF 
00098          10  WS-TRUNC-TEST.                                       ELKCMIF 
00099              15 WS-LOGICAL-END-TEST PICTURE XX.                   ELKCMIF 
00100                 88 WS-LOGICAL-END           VALUE '..'.           ELKCMIF 
00101              15 FILLER              PICTURE X(73).                ELKCMIF 
00102              15 WS-SHORT-RECORD-FIX PICTURE X(4).                 ELKCMIF 
00103      05  WS-RECORD-STATUS-SW        PICTURE X.                    ELKCMIF 
00104          88  WS-SHORT-RECORD                VALUE 'S'.            ELKCMIF 
00105          88  WS-NORMAL-RECORD               VALUE 'N'.            ELKCMIF 
00106      05  WS-SUPPRESS-SW             PICTURE X.                    ELKCMIF 
00107          88  WS-BYPASS-TRANS                VALUE 'Y'.            ELKCMIF 
00108          88  WS-ACCEPT-TRANS                VALUE 'N'.            ELKCMIF 
00109      05  WS-RECORD-KEYS.                                          ELKCMIF 
00110          10  WS-ELPCN-KEY.                                        ELKCMIF 
00111              15  WS-CN-COBOL-PREFIX      PIC X(8).                ELKCMIF 
00112              15  WS-CN-COBOL-NAME        PIC X(30).               ELKCMIF 
00113          10  WS-ELPEN-KEY.                                        ELKCMIF 
00114              15  WS-EN-ENGLISH-PREFIX    PIC X(8).                ELKCMIF 
00115              15  WS-EN-ENGLISH-NAME      PIC X(75).               ELKCMIF 
00116          10  WS-ELPDE-KEY.                                        ELKCMIF 
00117              15  WS-DE-RECORD-PREFIX     PIC X(8).                ELKCMIF 
00118              15  WS-DE-ELEMENT-NUM       PIC S9(3)V99   COMP-3.   ELKCMIF 
00119          10  WS-ELPCV-KEY.                                        ELKCMIF 
00120              15  WS-CV-RECORD-PREFIX     PIC X(8).                ELKCMIF 
00121              15  WS-CV-ELEMENT-NUM       PIC S9(3)V99   COMP-3.   ELKCMIF 
00122              15  WS-CV-CODE-VALUE        PIC X(10).               ELKCMIF 
00123              15  WS-CV-DESC-SEQ          PIC 99.                  ELKCMIF 
00124      05  WS-HIGH-CODE-VALUE              PIC X(10).               ELKCMIF 
00125      05  WS-GENERIC-KEY-LEN              PIC S9(4)      COMP.     ELKCMIF 
00126 /                                                                 ELKCMIF 
00127  LINKAGE SECTION.                                                 ELKCMIF 
00128                                                                   ELKCMIF 
00129  01  DFHCOMMAREA.                                                 ELKCMIF 
00130  COPY ELKCMIFC.                                                   ELKCMIF 
00131 /                                                                 ELKCMIF 
00132  COPY ELKDESCC.                                                   ELKCMIF 
00133 /                                                                 ELKCMIF 
00134  COPY ELKTRANC.                                                   ELKCMIF 
00135 /                                                                 ELKCMIF 
00136  01  CN-COBOL-NAME-RECORD.                                        ELKCMIF 
00137  COPY ELPCNC.                                                     ELKCMIF 
00138 /                                                                 ELKCMIF 
00139  01  EN-ENGLISH-NAME-RECORD.                                      ELKCMIF 
00140  COPY ELPENC.                                                     ELKCMIF 
00141 /                                                                 ELKCMIF 
00142  01  DE-DATA-ELEMENT-RECORD.                                      ELKCMIF 
00143  COPY ELPDEC.                                                     ELKCMIF 
00144 /                                                                 ELKCMIF 
00145  01  CV-CODE-VALUE-RECORD.                                        ELKCMIF 
00146  COPY ELPCVC.                                                     ELKCMIF 
00147 /                                                                 ELKCMIF 
00148  01  WORK-AREA.                                                   ELKCMIF 
00149      05  WA-RECORD-KEYS.                                          ELKCMIF 
00150          10  WA-ELPCN-KEY.                                        ELKCMIF 
00151              15  WA-CN-COBOL-PREFIX      PIC X(8).                ELKCMIF 
00152              15  WA-CN-COBOL-NAME        PIC X(30).               ELKCMIF 
00153          10  WA-ELPEN-KEY.                                        ELKCMIF 
00154              15  WA-EN-ENGLISH-PREFIX    PIC X(8).                ELKCMIF 
00155              15  WA-EN-ENGLISH-NAME      PIC X(75).               ELKCMIF 
00156          10  WA-ELPDE-KEY.                                        ELKCMIF 
00157              15  WA-DE-RECORD-PREFIX     PIC X(8).                ELKCMIF 
00158              15  WA-DE-ELEMENT-NUM       PIC S9(3)V99   COMP-3.   ELKCMIF 
00159          10  WA-ELPCV-KEY.                                        ELKCMIF 
00160              15  WA-CV-RECORD-PREFIX     PIC X(8).                ELKCMIF 
00161              15  WA-CV-ELEMENT-NUM       PIC S9(3)V99   COMP-3.   ELKCMIF 
00162              15  WA-CV-CODE-VALUE        PIC X(10).               ELKCMIF 
00163              15  WA-CV-DESC-SEQ          PIC 99.                  ELKCMIF 
00164      05  WA-RECORD-PTRS.                                          ELKCMIF 
00165          10  WA-ELPCN-PTR                POINTER.                 ELKCMIF 
00166          10  WA-ELPEN-PTR                POINTER.                 ELKCMIF 
00167          10  WA-ELPDE-PTR                POINTER.                 ELKCMIF 
00168          10  WA-ELPCV-PTR                POINTER.                 ELKCMIF 
00169 /                                                                 ELKCMIF 
00170  PROCEDURE DIVISION.                                              ELKCMIF 
00171      PERFORM 0000-INITIALIZATION.                                 ELKCMIF 
00172      PERFORM 1000-PROCESS-REQUEST.                                ELKCMIF 
00173      PERFORM 0500-TERMINATION.                                    ELKCMIF 
00174      EXEC CICS RETURN END-EXEC.                                   ELKCMIF 
00175      GOBACK.                                                      ELKCMIF 
00176                                                                   ELKCMIF 
00177  0000-INITIALIZATION.                                             ELKCMIF 
00178      PERFORM 0010-VALIDATE-COMMAREA.                              ELKCMIF 
00179      PERFORM 0020-IGNORE-CONDITIONS.                              ELKCMIF 
00180      PERFORM 8030-ESTABLISH-WORK-AREA.                            ELKCMIF 
00181                                                                   ELKCMIF 
00182  0010-VALIDATE-COMMAREA.                                          ELKCMIF 
00183      IF LENGTH OF DFHCOMMAREA NOT = EIBCALEN                      ELKCMIF 
00184          PERFORM 9000-SIGNAL-INVALID-COMMAREA.                    ELKCMIF 
00185      IF CMIF-FREE-STORAGE                                         ELKCMIF 
00186          CONTINUE                                                 ELKCMIF 
00187      ELSE                                                         ELKCMIF 
00188          IF NOT CMIF-VALID-ARG                                    ELKCMIF 
00189              PERFORM 9010-SIGNAL-INVALID-ARGUMENT                 ELKCMIF 
00190          END-IF                                                   ELKCMIF 
00191          IF CMIF-VALID-TRAN-REQ AND CMIF-VALID-DESC-REQ           ELKCMIF 
00192              CONTINUE                                             ELKCMIF 
00193          ELSE                                                     ELKCMIF 
00194              PERFORM 9020-SIGNAL-INVALID-REQUEST                  ELKCMIF 
00195          END-IF                                                   ELKCMIF 
00196      END-IF.                                                      ELKCMIF 
00197                                                                   ELKCMIF 
00198  0020-IGNORE-CONDITIONS.                                          ELKCMIF 
00199      IF NOT CMIF-FREE-STORAGE                                     ELKCMIF 
00200          EXEC CICS IGNORE CONDITION                               ELKCMIF 
00201              NOTFND                                               ELKCMIF 
00202              ENDFILE                                              ELKCMIF 
00203          END-EXEC                                                 ELKCMIF 
00204      END-IF.                                                      ELKCMIF 
00205                                                                   ELKCMIF 
00206  0500-TERMINATION.                                                ELKCMIF 
00207      IF WS-CV-ENDBR-REQ                                           ELKCMIF 
00208          EXEC CICS ENDBR                                          ELKCMIF 
00209               DATASET(PC-ELPCV)                                   ELKCMIF 
00210          END-EXEC                                                 ELKCMIF 
00211      END-IF.                                                      ELKCMIF 
00212 /                                                                 ELKCMIF 
00213  1000-PROCESS-REQUEST.                                            ELKCMIF 
00214      EVALUATE TRUE                                                ELKCMIF 
00215      WHEN CMIF-USE-SYSTEM-ARG                                     ELKCMIF 
00216           PERFORM 2000-PROCESS-SYSTEM-NAME-REQ                    ELKCMIF 
00217      WHEN CMIF-USE-ENGLISH-ARG                                    ELKCMIF 
00218           PERFORM 3000-PROCESS-ENGLISH-NAME-REQ                   ELKCMIF 
00219      WHEN CMIF-FREE-STORAGE                                       ELKCMIF 
00220           PERFORM 8100-FREE-STORAGE                               ELKCMIF 
00221      WHEN OTHER                                                   ELKCMIF 
00222           PERFORM 9010-SIGNAL-INVALID-ARGUMENT                    ELKCMIF 
00223      END-EVALUATE.                                                ELKCMIF 
00224                                                                   ELKCMIF 
00225  2000-PROCESS-SYSTEM-NAME-REQ.                                    ELKCMIF 
00226      IF (CMIF-SYSTEM-PREFIX = SPACES OR LOW-VALUES) OR            ELKCMIF 
00227         (CMIF-SYSTEM-NAME = SPACES OR LOW-VALUE)                  ELKCMIF 
00228           SET CMIF-INVALID-ARG TO TRUE                            ELKCMIF 
00229      ELSE                                                         ELKCMIF 
00230           PERFORM 2010-READ-COBOL-NAME-REC.                       ELKCMIF 
00231                                                                   ELKCMIF 
00232  2010-READ-COBOL-NAME-REC.                                        ELKCMIF 
00233      MOVE CMIF-SYSTEM-PREFIX      TO WS-CN-COBOL-PREFIX.          ELKCMIF 
00234      MOVE CMIF-SYSTEM-NAME        TO WS-CN-COBOL-NAME.            ELKCMIF 
00235                                                                   ELKCMIF 
00236      IF WS-ELPCN-KEY = WA-ELPCN-KEY                               ELKCMIF 
00237          SET WS-ELPCN-FOUND TO TRUE                               ELKCMIF 
00238      ELSE                                                         ELKCMIF 
00239          PERFORM 7010-READ-CN-FILE.                               ELKCMIF 
00240                                                                   ELKCMIF 
00241      IF WS-ELPCN-FOUND                                            ELKCMIF 
00242          MOVE WS-ELPCN-KEY TO WA-ELPCN-KEY                        ELKCMIF 
00243          PERFORM 2020-PROCESS-COBOL-NAME-REC                      ELKCMIF 
00244      ELSE                                                         ELKCMIF 
00245          SET CMIF-ARGUMENT-NOT-FOUND TO TRUE                      ELKCMIF 
00246          MOVE LOW-VALUES TO WA-ELPCN-KEY.                         ELKCMIF 
00247                                                                   ELKCMIF 
00248  2020-PROCESS-COBOL-NAME-REC.                                     ELKCMIF 
00249      SET ADDRESS OF CN-COBOL-NAME-RECORD TO WA-ELPCN-PTR.         ELKCMIF 
00250      IF CN-DELETE                                                 ELKCMIF 
00251          SET CMIF-ARGUMENT-NOT-FOUND TO TRUE                      ELKCMIF 
00252      ELSE                                                         ELKCMIF 
00253          MOVE CN-RECORD-PREFIX        TO WS-DE-RECORD-PREFIX      ELKCMIF 
00254          MOVE CN-ELEMENT-NBR          TO WS-DE-ELEMENT-NUM        ELKCMIF 
00255          MOVE CN-RECORD-PREFIX        TO WS-CV-RECORD-PREFIX      ELKCMIF 
00256          MOVE CN-ELEMENT-NBR          TO WS-CV-ELEMENT-NUM        ELKCMIF 
00257          PERFORM 4000-PROCESS-REQ                                 ELKCMIF 
00258      END-IF.                                                      ELKCMIF 
00259 /                                                                 ELKCMIF 
00260  3000-PROCESS-ENGLISH-NAME-REQ.                                   ELKCMIF 
00261      IF (CMIF-ENGLISH-PREFIX = SPACES OR LOW-VALUES) OR           ELKCMIF 
00262         (CMIF-ENGLISH-NAME = SPACES OR LOW-VALUE)                 ELKCMIF 
00263           SET CMIF-INVALID-ARG TO TRUE                            ELKCMIF 
00264      ELSE                                                         ELKCMIF 
00265           PERFORM 3010-READ-ENGLISH-NAME-REC.                     ELKCMIF 
00266                                                                   ELKCMIF 
00267  3010-READ-ENGLISH-NAME-REC.                                      ELKCMIF 
00268      MOVE CMIF-ENGLISH-PREFIX      TO WS-EN-ENGLISH-PREFIX.       ELKCMIF 
00269      MOVE CMIF-ENGLISH-NAME        TO WS-EN-ENGLISH-NAME.         ELKCMIF 
00270                                                                   ELKCMIF 
00271      IF WS-ELPEN-KEY = WA-ELPEN-KEY                               ELKCMIF 
00272          SET WS-ELPCN-FOUND TO TRUE                               ELKCMIF 
00273      ELSE                                                         ELKCMIF 
00274          PERFORM 7020-READ-EN-FILE.                               ELKCMIF 
00275                                                                   ELKCMIF 
00276      IF WS-ELPEN-FOUND                                            ELKCMIF 
00277          MOVE WS-ELPEN-KEY TO WA-ELPEN-KEY                        ELKCMIF 
00278          PERFORM 3020-PROCESS-ENGLISH-NAME-REC                    ELKCMIF 
00279      ELSE                                                         ELKCMIF 
00280          SET CMIF-ARGUMENT-NOT-FOUND TO TRUE                      ELKCMIF 
00281          MOVE LOW-VALUES TO WA-ELPEN-KEY.                         ELKCMIF 
00282                                                                   ELKCMIF 
00283  3020-PROCESS-ENGLISH-NAME-REC.                                   ELKCMIF 
00284      SET ADDRESS OF EN-ENGLISH-NAME-RECORD TO WA-ELPEN-PTR.       ELKCMIF 
00285      IF EN-DELETE                                                 ELKCMIF 
00286          SET CMIF-ARGUMENT-NOT-FOUND  TO TRUE                     ELKCMIF 
00287      ELSE                                                         ELKCMIF 
00288          MOVE EN-RECORD-PREFIX        TO WS-DE-RECORD-PREFIX      ELKCMIF 
00289          MOVE EN-ELEMENT-NBR          TO WS-DE-ELEMENT-NUM        ELKCMIF 
00290          MOVE EN-RECORD-PREFIX        TO WS-CV-RECORD-PREFIX      ELKCMIF 
00291          MOVE EN-ELEMENT-NBR          TO WS-CV-ELEMENT-NUM        ELKCMIF 
00292          PERFORM 4000-PROCESS-REQ                                 ELKCMIF 
00293      END-IF.                                                      ELKCMIF 
00294 /                                                                 ELKCMIF 
00295  4000-PROCESS-REQ.                                                ELKCMIF 
00296      IF CMIF-GENERIC-TRANS-REQ                                    ELKCMIF 
00297             PERFORM 6000-GENERIC-TRANSLATION                      ELKCMIF 
00298      ELSE                                                         ELKCMIF 
00299          IF CMIF-FULL-TRANS-REQ OR                                ELKCMIF 
00300             CMIF-SHORT-TRANS-REQ OR                               ELKCMIF 
00301             CMIF-CODE-DESC-REQ                                    ELKCMIF 
00302                 PERFORM 5000-PROCESS-CODE-VALUE                   ELKCMIF 
00303          END-IF                                                   ELKCMIF 
00304      END-IF.                                                      ELKCMIF 
00305      IF CMIF-FIELD-DESC-REQ AND                                   ELKCMIF 
00306         CMIF-OK                                                   ELKCMIF 
00307             PERFORM 4100-OBTAIN-FIELD-DESC                        ELKCMIF 
00308      END-IF.                                                      ELKCMIF 
00309                                                                   ELKCMIF 
00310                                                                   ELKCMIF 
00311  4100-OBTAIN-FIELD-DESC.                                          ELKCMIF 
00312      PERFORM 8000-ESTABLISH-DESC-AREA.                            ELKCMIF 
00313                                                                   ELKCMIF 
00314      IF WS-ELPDE-KEY = WA-ELPDE-KEY                               ELKCMIF 
00315          SET WS-ELPDE-FOUND TO TRUE                               ELKCMIF 
00316      ELSE                                                         ELKCMIF 
00317          PERFORM 7030-READ-DE-FILE                                ELKCMIF 
00318      END-IF.                                                      ELKCMIF 
00319                                                                   ELKCMIF 
00320      IF WS-ELPDE-FOUND                                            ELKCMIF 
00321          MOVE WS-ELPDE-KEY TO WA-ELPDE-KEY                        ELKCMIF 
00322          PERFORM 4110-COPY-DESCRIPTION                            ELKCMIF 
00323      ELSE                                                         ELKCMIF 
00324          SET CMIF-ARGUMENT-NOT-FOUND TO TRUE                      ELKCMIF 
00325          MOVE LOW-VALUES TO WA-ELPDE-KEY.                         ELKCMIF 
00326                                                                   ELKCMIF 
00327  4110-COPY-DESCRIPTION.                                           ELKCMIF 
00328      SET ADDRESS OF DE-DATA-ELEMENT-RECORD TO WA-ELPDE-PTR.       ELKCMIF 
00329      IF DE-DELETE                                                 ELKCMIF 
00330          SET CMIF-ARGUMENT-NOT-FOUND TO TRUE                      ELKCMIF 
00331      ELSE                                                         ELKCMIF 
00332          MOVE DE-NBR-DESC-LINES TO CMFD-NBR-DESCR-LINES           ELKCMIF 
00333          PERFORM VARYING WS-DESC-SUB FROM 1 BY 1                  ELKCMIF 
00334              UNTIL WS-DESC-SUB > DE-NBR-DESC-LINES                ELKCMIF 
00335              MOVE DE-DESC-LINE (WS-DESC-SUB)                      ELKCMIF 
00336                TO CMFD-DESCR-LINE (WS-DESC-SUB)                   ELKCMIF 
00337          END-PERFORM                                              ELKCMIF 
00338      END-IF.                                                      ELKCMIF 
00339 /                                                                 ELKCMIF 
00340  5000-PROCESS-CODE-VALUE.                                         ELKCMIF 
00341      IF CMIF-CODE-VALUE = SPACES OR LOW-VALUES                    ELKCMIF 
00342          SET CMIF-INVALID-ARG TO TRUE                             ELKCMIF 
00343      ELSE                                                         ELKCMIF 
00344          PERFORM 5010-PROCESS-TRAN-REQ.                           ELKCMIF 
00345                                                                   ELKCMIF 
00346  5010-PROCESS-TRAN-REQ.                                           ELKCMIF 
00347      MOVE CMIF-CODE-VALUE TO WS-CV-CODE-VALUE                     ELKCMIF 
00348                              WS-HIGH-CODE-VALUE.                  ELKCMIF 
00349      COMPUTE WS-GENERIC-KEY-LEN = LENGTH OF WS-CV-RECORD-PREFIX   ELKCMIF 
00350                                 + LENGTH OF WS-CV-ELEMENT-NUM     ELKCMIF 
00351                                 + LENGTH OF WS-CV-CODE-VALUE.     ELKCMIF 
00352                                                                   ELKCMIF 
00353      EXEC CICS STARTBR                                            ELKCMIF 
00354           DATASET(PC-ELPCV)                                       ELKCMIF 
00355           RIDFLD(WS-ELPCV-KEY)                                    ELKCMIF 
00356           KEYLENGTH(WS-GENERIC-KEY-LEN) GENERIC                   ELKCMIF 
00357           EQUAL                                                   ELKCMIF 
00358      END-EXEC.                                                    ELKCMIF 
00359                                                                   ELKCMIF 
00360      IF EIBRCODE = LOW-VALUES                                     ELKCMIF 
00361           SET WS-CV-ENDBR-REQ TO TRUE                             ELKCMIF 
00362           MOVE WS-ELPCV-KEY   TO WA-ELPCV-KEY                     ELKCMIF 
00363           PERFORM 7000-READ-CODE-VALUE-RECORD                     ELKCMIF 
00364      ELSE                                                         ELKCMIF 
00365           SET WS-CV-ENDBR-NOT-REQ TO TRUE                         ELKCMIF 
00366           MOVE LOW-VALUES     TO WA-ELPCV-KEY                     ELKCMIF 
00367           SET WS-ELPCV-NOT-FOUND TO TRUE                          ELKCMIF 
00368      END-IF.                                                      ELKCMIF 
00369                                                                   ELKCMIF 
00370      IF WS-ELPCV-FOUND                                            ELKCMIF 
00371           IF CMIF-CODE-DESC-REQ                                   ELKCMIF 
00372                  MOVE CV-CODE-NAME  TO  CMIF-CODE-DESC            ELKCMIF 
00373                  SET CMIF-OK TO TRUE                              ELKCMIF 
00374           END-IF                                                  ELKCMIF 
00375           IF CMIF-FULL-TRANS-REQ OR                               ELKCMIF 
00376              CMIF-SHORT-TRANS-REQ                                 ELKCMIF 
00377                PERFORM 5020-TRANSLATE-REQ                         ELKCMIF 
00378           END-IF                                                  ELKCMIF 
00379      ELSE                                                         ELKCMIF 
00380           SET CMIF-CODE-VALUE-NOT-FOUND TO TRUE                   ELKCMIF 
00381      END-IF.                                                      ELKCMIF 
00382 /                                                                 ELKCMIF 
00383  5020-TRANSLATE-REQ.                                              ELKCMIF 
00384      PERFORM 8010-ESTABLISH-TRAN-AREA.                            ELKCMIF 
00385      SET WS-TRANSLATION-NOT-COMPLETE TO TRUE.                     ELKCMIF 
00386      MOVE ZERO TO CMTR-NBR-TRANS-LINES.                           ELKCMIF 
00387      PERFORM 5030-MOVE-TRANSLATION                                ELKCMIF 
00388          UNTIL WS-TRANSLATION-COMPLETE.                           ELKCMIF 
00389                                                                   ELKCMIF 
00390  5030-MOVE-TRANSLATION.                                           ELKCMIF 
00391      PERFORM 5040-MOVE-TRANSLATION-LINES                          ELKCMIF 
00392          VARYING WS-DESC-SUB FROM 1 BY 1                          ELKCMIF 
00393            UNTIL WS-DESC-SUB > CV-NBR-VALUE-DESC-LINES            ELKCMIF 
00394               OR WS-TRANSLATION-COMPLETE.                         ELKCMIF 
00395      IF WS-TRANSLATION-NOT-COMPLETE                               ELKCMIF 
00396          PERFORM 7000-READ-CODE-VALUE-RECORD                      ELKCMIF 
00397          IF WS-ELPCV-NOT-FOUND                                    ELKCMIF 
00398              SET WS-TRANSLATION-COMPLETE TO TRUE                  ELKCMIF 
00399          END-IF                                                   ELKCMIF 
00400      END-IF.                                                      ELKCMIF 
00401                                                                   ELKCMIF 
00402  5040-MOVE-TRANSLATION-LINES.                                     ELKCMIF 
00403      MOVE CV-VALUE-DESC-LINE (WS-DESC-SUB) TO WS-TRUNC-TEST.      ELKCMIF 
00404      IF  WS-SHORT-RECORD AND                                      ELKCMIF 
00405                 WS-DESC-SUB = CV-NBR-VALUE-DESC-LINES             ELKCMIF 
00406           MOVE SPACES TO WS-SHORT-RECORD-FIX                      ELKCMIF 
00407      END-IF.                                                      ELKCMIF 
00408                                                                   ELKCMIF 
00409      IF WS-LOGICAL-END AND CMIF-SHORT-TRANS-REQ                   ELKCMIF 
00410          SET WS-TRANSLATION-COMPLETE TO TRUE                      ELKCMIF 
00411      ELSE                                                         ELKCMIF 
00412          ADD 1 TO CMTR-NBR-TRANS-LINES                            ELKCMIF 
00413          MOVE WS-CENTER-LINE TO CMTR-TRANS-LINE                   ELKCMIF 
00414                                (CMTR-NBR-TRANS-LINES)             ELKCMIF 
00415          IF CMTR-NBR-TRANS-LINES = PC-MAX-TRANS-LINE              ELKCMIF 
00416              SET WS-TRANSLATION-COMPLETE TO TRUE                  ELKCMIF 
00417          END-IF                                                   ELKCMIF 
00418      END-IF.                                                      ELKCMIF 
00419 /                                                                 ELKCMIF 
00420  6000-GENERIC-TRANSLATION.                                        ELKCMIF 
00421      MOVE HIGH-VALUES TO WS-HIGH-CODE-VALUE.                      ELKCMIF 
00422      IF CMIF-CODE-VALUE = SPACES OR LOW-VALUES                    ELKCMIF 
00423          MOVE ZERO TO WS-GENERIC-KEY-LEN                          ELKCMIF 
00424      ELSE                                                         ELKCMIF 
00425          MOVE 1 TO WS-GENERIC-KEY-LEN                             ELKCMIF 
00426          STRING CMIF-CODE-VALUE     DELIMITED BY SPACE            ELKCMIF 
00427            INTO WS-HIGH-CODE-VALUE                                ELKCMIF 
00428            POINTER WS-GENERIC-KEY-LEN                             ELKCMIF 
00429          END-STRING                                               ELKCMIF 
00430          SUBTRACT 1 FROM WS-GENERIC-KEY-LEN                       ELKCMIF 
00431      END-IF.                                                      ELKCMIF 
00432                                                                   ELKCMIF 
00433      COMPUTE WS-GENERIC-KEY-LEN = LENGTH OF WS-CV-RECORD-PREFIX   ELKCMIF 
00434                                 + LENGTH OF WS-CV-ELEMENT-NUM     ELKCMIF 
00435                                 + WS-GENERIC-KEY-LEN.             ELKCMIF 
00436                                                                   ELKCMIF 
00437      MOVE CMIF-CODE-VALUE TO WS-CV-CODE-VALUE.                    ELKCMIF 
00438      EXEC CICS STARTBR                                            ELKCMIF 
00439           DATASET(PC-ELPCV)                                       ELKCMIF 
00440           RIDFLD(WS-ELPCV-KEY)                                    ELKCMIF 
00441           KEYLENGTH(WS-GENERIC-KEY-LEN) GENERIC                   ELKCMIF 
00442           EQUAL                                                   ELKCMIF 
00443      END-EXEC.                                                    ELKCMIF 
00444                                                                   ELKCMIF 
00445      IF EIBRCODE = LOW-VALUES                                     ELKCMIF 
00446           SET WS-CV-ENDBR-REQ TO TRUE                             ELKCMIF 
00447           MOVE WS-ELPCV-KEY   TO WA-ELPCV-KEY                     ELKCMIF 
00448           PERFORM 7000-READ-CODE-VALUE-RECORD                     ELKCMIF 
00449      ELSE                                                         ELKCMIF 
00450           SET WS-CV-ENDBR-NOT-REQ TO TRUE                         ELKCMIF 
00451           MOVE LOW-VALUES     TO WA-ELPCV-KEY                     ELKCMIF 
00452           SET WS-ELPCV-NOT-FOUND TO TRUE                          ELKCMIF 
00453      END-IF.                                                      ELKCMIF 
00454                                                                   ELKCMIF 
00455      IF WS-ELPCV-FOUND                                            ELKCMIF 
00456           PERFORM 6010-TRANSLATE-REQ                              ELKCMIF 
00457      ELSE                                                         ELKCMIF 
00458           SET CMIF-CODE-VALUE-NOT-FOUND TO TRUE                   ELKCMIF 
00459      END-IF.                                                      ELKCMIF 
00460 /                                                                 ELKCMIF 
00461  6010-TRANSLATE-REQ.                                              ELKCMIF 
00462      PERFORM 8010-ESTABLISH-TRAN-AREA.                            ELKCMIF 
00463      SET WS-TRANSLATION-NOT-COMPLETE TO TRUE.                     ELKCMIF 
00464      SET WS-ACCEPT-TRANS TO TRUE.                                 ELKCMIF 
00465      MOVE ZERO TO CMTR-NBR-TRANS-LINES.                           ELKCMIF 
00466      MOVE LOW-VALUES TO WA-CV-CODE-VALUE.                         ELKCMIF 
00467                                                                   ELKCMIF 
00468      PERFORM 6020-MOVE-TRANSLATION                                ELKCMIF 
00469          UNTIL WS-TRANSLATION-COMPLETE.                           ELKCMIF 
00470                                                                   ELKCMIF 
00471  6020-MOVE-TRANSLATION.                                           ELKCMIF 
00472      IF CV-CODE-VALUE NOT = WA-CV-CODE-VALUE                      ELKCMIF 
00473          PERFORM 6040-NEW-CODE-VALUE                              ELKCMIF 
00474      END-IF.                                                      ELKCMIF 
00475      PERFORM 6030-MOVE-TRANSLATION-LINES                          ELKCMIF 
00476          VARYING WS-DESC-SUB FROM 1 BY 1                          ELKCMIF 
00477            UNTIL WS-DESC-SUB > CV-NBR-VALUE-DESC-LINES            ELKCMIF 
00478               OR WS-BYPASS-TRANS                                  ELKCMIF 
00479               OR WS-TRANSLATION-COMPLETE.                         ELKCMIF 
00480      IF WS-BYPASS-TRANS                                           ELKCMIF 
00481          PERFORM 7000-READ-CODE-VALUE-RECORD                      ELKCMIF 
00482             UNTIL WS-ELPCV-NOT-FOUND                              ELKCMIF 
00483          IF WS-ELPCV-EOF                                          ELKCMIF 
00484              SET WS-TRANSLATION-COMPLETE TO TRUE                  ELKCMIF 
00485          ELSE                                                     ELKCMIF 
00486              IF (CV-RECORD-PREFIX = WA-CV-RECORD-PREFIX    AND    ELKCMIF 
00487                  CV-ELEMENT-NBR   = WA-CV-ELEMENT-NUM      AND    ELKCMIF 
00488                  CV-CODE-VALUE    < WS-HIGH-CODE-VALUE)           ELKCMIF 
00489                     CONTINUE                                      ELKCMIF 
00490              ELSE                                                 ELKCMIF 
00491                     SET WS-TRANSLATION-COMPLETE TO TRUE           ELKCMIF 
00492              END-IF                                               ELKCMIF 
00493          END-IF                                                   ELKCMIF 
00494      ELSE                                                         ELKCMIF 
00495          IF WS-TRANSLATION-NOT-COMPLETE                           ELKCMIF 
00496              PERFORM 7000-READ-CODE-VALUE-RECORD                  ELKCMIF 
00497              IF WS-ELPCV-NOT-FOUND                                ELKCMIF 
00498                  SET WS-TRANSLATION-COMPLETE TO TRUE              ELKCMIF 
00499              END-IF                                               ELKCMIF 
00500          END-IF                                                   ELKCMIF 
00501      END-IF.                                                      ELKCMIF 
00502                                                                   ELKCMIF 
00503  6030-MOVE-TRANSLATION-LINES.                                     ELKCMIF 
00504      MOVE CV-VALUE-DESC-LINE (WS-DESC-SUB) TO WS-TRUNC-TEST.      ELKCMIF 
00505      IF  WS-SHORT-RECORD AND                                      ELKCMIF 
00506                 WS-DESC-SUB = CV-NBR-VALUE-DESC-LINES             ELKCMIF 
00507           MOVE SPACES TO WS-SHORT-RECORD-FIX                      ELKCMIF 
00508      END-IF.                                                      ELKCMIF 
00509                                                                   ELKCMIF 
00510      IF WS-LOGICAL-END AND CMIF-SHORT-TRANS-REQ                   ELKCMIF 
00511          SET WS-BYPASS-TRANS TO TRUE                              ELKCMIF 
00512      ELSE                                                         ELKCMIF 
00513          ADD 1 TO CMTR-NBR-TRANS-LINES                            ELKCMIF 
00514          MOVE WS-CENTER-LINE TO CMTR-TRANS-LINE                   ELKCMIF 
00515                                (CMTR-NBR-TRANS-LINES)             ELKCMIF 
00516          IF CMTR-NBR-TRANS-LINES = PC-MAX-TRANS-LINE              ELKCMIF 
00517              SET WS-TRANSLATION-COMPLETE TO TRUE                  ELKCMIF 
00518          END-IF                                                   ELKCMIF 
00519      END-IF.                                                      ELKCMIF 
00520 /                                                                 ELKCMIF 
00521  6040-NEW-CODE-VALUE.                                             ELKCMIF 
00522      ADD 1 TO CMTR-NBR-TRANS-LINES.                               ELKCMIF 
00523      MOVE SPACES TO CMTR-TRANS-LINE (CMTR-NBR-TRANS-LINES).       ELKCMIF 
00524      STRING 'CODE: '         DELIMITED BY SIZE                    ELKCMIF 
00525             CV-CODE-VALUE    DELIMITED BY SIZE                    ELKCMIF 
00526        INTO CMTR-TRANS-LINE (CMTR-NBR-TRANS-LINES)                ELKCMIF 
00527      END-STRING.                                                  ELKCMIF 
00528      IF CMTR-NBR-TRANS-LINES = PC-MAX-TRANS-LINE                  ELKCMIF 
00529          SET WS-TRANSLATION-COMPLETE TO TRUE                      ELKCMIF 
00530      END-IF.                                                      ELKCMIF 
00531      MOVE CV-CODE-VALUE TO WA-CV-CODE-VALUE.                      ELKCMIF 
00532 /                                                                 ELKCMIF 
00533  7000-READ-CODE-VALUE-RECORD.                                     ELKCMIF 
00534      EXEC CICS READNEXT                                           ELKCMIF 
00535           DATASET(PC-ELPCV)                                       ELKCMIF 
00536           SET(WA-ELPCV-PTR)                                       ELKCMIF 
00537           LENGTH(WS-ELPCV-REC-LEN)                                ELKCMIF 
00538           RIDFLD(WS-ELPCV-KEY)                                    ELKCMIF 
00539      END-EXEC.                                                    ELKCMIF 
00540                                                                   ELKCMIF 
00541      SET ADDRESS OF CV-CODE-VALUE-RECORD TO WA-ELPCV-PTR.         ELKCMIF 
00542                                                                   ELKCMIF 
00543      IF EIBRCODE = LOW-VALUES                                     ELKCMIF 
00544          IF NOT CV-DELETE AND                                     ELKCMIF 
00545             (CV-RECORD-PREFIX = WA-CV-RECORD-PREFIX    AND        ELKCMIF 
00546              CV-ELEMENT-NBR   = WA-CV-ELEMENT-NUM      AND        ELKCMIF 
00547              CV-CODE-VALUE NOT > WS-HIGH-CODE-VALUE)              ELKCMIF 
00548                  SET WS-ELPCV-FOUND TO TRUE                       ELKCMIF 
00549                  PERFORM 7001-EXECUTE-IO-PATCH                    ELKCMIF 
00550          ELSE                                                     ELKCMIF 
00551              SET WS-ELPCV-NOT-FOUND TO TRUE                       ELKCMIF 
00552          END-IF                                                   ELKCMIF 
00553      ELSE                                                         ELKCMIF 
00554          SET WS-ELPCV-EOF TO TRUE                                 ELKCMIF 
00555      END-IF.                                                      ELKCMIF 
00556                                                                   ELKCMIF 
00557 *                                                                 ELKCMIF 
00558 *  THIS IO PATCH TAKES CARE OF A SITUATION IN THE CODE VALUE FILE ELKCMIF 
00559 *  IN WHICH THE RECORD COULD BE FOUR BYTES TOO SHORT.  AS A       ELKCMIF 
00560 *  RESULT, THOSE FOUR BYTE CONTAIN GARBAGE WHICH IS INCLUDED IN   ELKCMIF 
00561 *  THE TRANSLATION AND CAUSED WIERD TEXT TO APPEAR IN TRANSLATIONSELKCMIF 
00562 *  AS WELL AS OCCASIONAL PROG407 TERMINAL ERRORS.                 ELKCMIF 
00563 *                                                                 ELKCMIF 
00564                                                                   ELKCMIF 
00565  7001-EXECUTE-IO-PATCH.                                           ELKCMIF 
00566      IF WS-ELPCV-REC-LEN < 76                                     ELKCMIF 
00567          PERFORM 9060-SIGNAL-ELPCV-ERROR                          ELKCMIF 
00568      END-IF.                                                      ELKCMIF 
00569      IF WS-ELPCV-REC-LEN <                                        ELKCMIF 
00570              (76 + CV-NBR-VALUE-DESC-LINES                        ELKCMIF 
00571                  * LENGTH OF CV-VALUE-DESC-LINE)                  ELKCMIF 
00572          SET WS-SHORT-RECORD TO TRUE                              ELKCMIF 
00573      ELSE                                                         ELKCMIF 
00574          SET WS-NORMAL-RECORD TO TRUE                             ELKCMIF 
00575      END-IF.                                                      ELKCMIF 
00576 /                                                                 ELKCMIF 
00577  7010-READ-CN-FILE.                                               ELKCMIF 
00578      EXEC CICS READ                                               ELKCMIF 
00579           DATASET(PC-ELPCN)                                       ELKCMIF 
00580           SET(WA-ELPCN-PTR)                                       ELKCMIF 
00581           RIDFLD(WS-ELPCN-KEY)                                    ELKCMIF 
00582      END-EXEC.                                                    ELKCMIF 
00583                                                                   ELKCMIF 
00584      IF EIBRCODE = LOW-VALUES                                     ELKCMIF 
00585          SET WS-ELPCN-FOUND TO TRUE                               ELKCMIF 
00586      ELSE                                                         ELKCMIF 
00587          SET WS-ELPCN-NOT-FOUND TO TRUE                           ELKCMIF 
00588      END-IF.                                                      ELKCMIF 
00589                                                                   ELKCMIF 
00590                                                                   ELKCMIF 
00591  7020-READ-EN-FILE.                                               ELKCMIF 
00592      EXEC CICS READ                                               ELKCMIF 
00593           DATASET(PC-ELPEN)                                       ELKCMIF 
00594           SET(WA-ELPEN-PTR)                                       ELKCMIF 
00595           RIDFLD(WS-ELPEN-KEY)                                    ELKCMIF 
00596      END-EXEC.                                                    ELKCMIF 
00597                                                                   ELKCMIF 
00598      IF EIBRCODE = LOW-VALUES                                     ELKCMIF 
00599          SET WS-ELPEN-FOUND TO TRUE                               ELKCMIF 
00600      ELSE                                                         ELKCMIF 
00601          SET WS-ELPEN-NOT-FOUND TO TRUE                           ELKCMIF 
00602      END-IF.                                                      ELKCMIF 
00603                                                                   ELKCMIF 
00604  7030-READ-DE-FILE.                                               ELKCMIF 
00605      EXEC CICS READ                                               ELKCMIF 
00606           DATASET(PC-ELPDE)                                       ELKCMIF 
00607           SET(WA-ELPDE-PTR)                                       ELKCMIF 
00608           RIDFLD(WS-ELPDE-KEY)                                    ELKCMIF 
00609      END-EXEC.                                                    ELKCMIF 
00610                                                                   ELKCMIF 
00611      IF EIBRCODE = LOW-VALUES                                     ELKCMIF 
00612          SET WS-ELPDE-FOUND TO TRUE                               ELKCMIF 
00613      ELSE                                                         ELKCMIF 
00614          SET WS-ELPDE-NOT-FOUND TO TRUE                           ELKCMIF 
00615      END-IF.                                                      ELKCMIF 
00616 /                                                                 ELKCMIF 
00617  8000-ESTABLISH-DESC-AREA.                                        ELKCMIF 
00618      IF CMIF-DESC-PTR = NULL                                      ELKCMIF 
00619          COMPUTE WS-GET-MAIN-LENGTH =                             ELKCMIF 
00620             LENGTH OF CMFD-FIXED-PART +                           ELKCMIF 
00621            (LENGTH OF CMFD-DESCR-LINE * 12)                       ELKCMIF 
00622          EXEC CICS GETMAIN                                        ELKCMIF 
00623                SET(CMIF-DESC-PTR)                                 ELKCMIF 
00624                LENGTH(WS-GET-MAIN-LENGTH)                         ELKCMIF 
00625                INITIMG(WS-LOW-VALUE)                              ELKCMIF 
00626          END-EXEC                                                 ELKCMIF 
00627          SET ADDRESS OF CMFD-FIELD-DESCRIPTION                    ELKCMIF 
00628              TO CMIF-DESC-PTR                                     ELKCMIF 
00629          MOVE PC-ELKDESCC TO CMFD-INTERNAL-ID                     ELKCMIF 
00630      ELSE                                                         ELKCMIF 
00631          SET ADDRESS OF CMFD-FIELD-DESCRIPTION                    ELKCMIF 
00632              TO CMIF-DESC-PTR                                     ELKCMIF 
00633          IF CMFD-INTERNAL-ID NOT = PC-ELKDESCC                    ELKCMIF 
00634              PERFORM 9040-SIGNAL-INVALID-DESC-AREA                ELKCMIF 
00635          END-IF                                                   ELKCMIF 
00636      END-IF.                                                      ELKCMIF 
00637                                                                   ELKCMIF 
00638  8010-ESTABLISH-TRAN-AREA.                                        ELKCMIF 
00639      IF CMIF-TRAN-PTR = NULL                                      ELKCMIF 
00640          COMPUTE WS-GET-MAIN-LENGTH =                             ELKCMIF 
00641             LENGTH OF CMTR-FIXED-PART +                           ELKCMIF 
00642            (LENGTH OF CMTR-TRANS-LINE * PC-MAX-TRANS-LINE)        ELKCMIF 
00643          EXEC CICS GETMAIN                                        ELKCMIF 
00644                SET(CMIF-TRAN-PTR)                                 ELKCMIF 
00645                LENGTH(WS-GET-MAIN-LENGTH)                         ELKCMIF 
00646                INITIMG(WS-LOW-VALUE)                              ELKCMIF 
00647          END-EXEC                                                 ELKCMIF 
00648          SET ADDRESS OF CMTR-TRANSLATION                          ELKCMIF 
00649              TO CMIF-TRAN-PTR                                     ELKCMIF 
00650          MOVE PC-ELKTRANC TO CMTR-INTERNAL-ID                     ELKCMIF 
00651      ELSE                                                         ELKCMIF 
00652          SET ADDRESS OF CMTR-TRANSLATION                          ELKCMIF 
00653              TO CMIF-TRAN-PTR                                     ELKCMIF 
00654          IF CMTR-INTERNAL-ID  NOT = PC-ELKTRANC                   ELKCMIF 
00655              PERFORM 9050-SIGNAL-INVALID-TRAN-AREA                ELKCMIF 
00656          END-IF                                                   ELKCMIF 
00657      END-IF.                                                      ELKCMIF 
00658 /                                                                 ELKCMIF 
00659  8030-ESTABLISH-WORK-AREA.                                        ELKCMIF 
00660      IF CMIF-WORK-AREA-PTR = NULL                                 ELKCMIF 
00661          COMPUTE WS-GET-MAIN-LENGTH = LENGTH OF WORK-AREA         ELKCMIF 
00662          EXEC CICS GETMAIN                                        ELKCMIF 
00663                SET(CMIF-WORK-AREA-PTR)                            ELKCMIF 
00664                LENGTH(WS-GET-MAIN-LENGTH)                         ELKCMIF 
00665                INITIMG(WS-LOW-VALUE)                              ELKCMIF 
00666          END-EXEC                                                 ELKCMIF 
00667      END-IF.                                                      ELKCMIF 
00668      SET ADDRESS OF WORK-AREA TO CMIF-WORK-AREA-PTR.              ELKCMIF 
00669 /                                                                 ELKCMIF 
00670  8100-FREE-STORAGE.                                               ELKCMIF 
00671      IF CMIF-TRAN-PTR NOT = NULL                                  ELKCMIF 
00672          SET ADDRESS OF CMTR-TRANSLATION TO CMIF-TRAN-PTR         ELKCMIF 
00673          EXEC CICS FREEMAIN                                       ELKCMIF 
00674                    DATA(CMTR-TRANSLATION)                         ELKCMIF 
00675          END-EXEC                                                 ELKCMIF 
00676          SET CMIF-TRAN-PTR TO NULL                                ELKCMIF 
00677      END-IF.                                                      ELKCMIF 
00678                                                                   ELKCMIF 
00679      IF CMIF-DESC-PTR NOT = NULL                                  ELKCMIF 
00680          SET ADDRESS OF CMFD-FIELD-DESCRIPTION TO CMIF-DESC-PTR   ELKCMIF 
00681          EXEC CICS FREEMAIN                                       ELKCMIF 
00682                    DATA(CMFD-FIELD-DESCRIPTION)                   ELKCMIF 
00683          END-EXEC                                                 ELKCMIF 
00684          SET CMIF-DESC-PTR TO NULL                                ELKCMIF 
00685      END-IF.                                                      ELKCMIF 
00686                                                                   ELKCMIF 
00687      IF CMIF-WORK-AREA-PTR NOT = NULL                             ELKCMIF 
00688          SET ADDRESS OF WORK-AREA TO CMIF-WORK-AREA-PTR           ELKCMIF 
00689          EXEC CICS FREEMAIN                                       ELKCMIF 
00690                    DATA(WORK-AREA)                                ELKCMIF 
00691          END-EXEC                                                 ELKCMIF 
00692          SET CMIF-WORK-AREA-PTR TO NULL                           ELKCMIF 
00693      END-IF.                                                      ELKCMIF 
00694                                                                   ELKCMIF 
00695 /                                                                 ELKCMIF 
00696  9000-SIGNAL-INVALID-COMMAREA.                                    ELKCMIF 
00697      EXEC CICS ABEND                                              ELKCMIF 
00698                ABCODE(PC-ABEND-CODE-ELKA)                         ELKCMIF 
00699                END-EXEC.                                          ELKCMIF 
00700  9010-SIGNAL-INVALID-ARGUMENT.                                    ELKCMIF 
00701      EXEC CICS ABEND                                              ELKCMIF 
00702                ABCODE(PC-ABEND-CODE-ELKB)                         ELKCMIF 
00703                END-EXEC.                                          ELKCMIF 
00704  9020-SIGNAL-INVALID-REQUEST.                                     ELKCMIF 
00705      EXEC CICS ABEND                                              ELKCMIF 
00706                ABCODE(PC-ABEND-CODE-ELKC)                         ELKCMIF 
00707                END-EXEC.                                          ELKCMIF 
00708  9040-SIGNAL-INVALID-DESC-AREA.                                   ELKCMIF 
00709      EXEC CICS ABEND                                              ELKCMIF 
00710                ABCODE(PC-ABEND-CODE-ELKD)                         ELKCMIF 
00711                END-EXEC.                                          ELKCMIF 
00712  9050-SIGNAL-INVALID-TRAN-AREA.                                   ELKCMIF 
00713      EXEC CICS ABEND                                              ELKCMIF 
00714                ABCODE(PC-ABEND-CODE-ELKE)                         ELKCMIF 
00715                END-EXEC.                                          ELKCMIF 
00716  9060-SIGNAL-ELPCV-ERROR.                                         ELKCMIF 
00717      EXEC CICS ABEND                                              ELKCMIF 
00718                ABCODE(PC-ABEND-CODE-ELKF)                         ELKCMIF 
00719                END-EXEC.                                          ELKCMIF 
