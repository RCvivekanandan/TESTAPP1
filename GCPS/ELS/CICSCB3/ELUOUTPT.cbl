00001 *      LAST MAINTENANCE TIME:  8.55.12  DATE: 07/19/91            09/03/03
00002  IDENTIFICATION DIVISION.                                         ELUOUTPT
00003                                                                      LV002
00004  PROGRAM-ID.         ELUOUTPT.                                    ELUOUTPT
00005                                                                   ELUOUTPT
00006  AUTHOR.             EDWARD G LISS                                ELUOUTPT
00007                                                                   ELUOUTPT
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUOUTPT
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELUOUTPT
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUOUTPT
00011                      233 N. MICHIGAN AVE                          ELUOUTPT
00012                      CHICAGO, ILLINOIS 60601                      ELUOUTPT
00013                                                                   ELUOUTPT
00014  DATE-WRITTEN.       15-JUN-1987.                                 ELUOUTPT
00015                                                                   ELUOUTPT
00016  DATE-COMPILED.                                                   ELUOUTPT
00017                                                                   ELUOUTPT
00018  SECURITY.           COPYRIGHT 1987,                              ELUOUTPT
00019                      HEALTH CARE SERVICE CORPORATION              ELUOUTPT
00020                                                                   ELUOUTPT
00021  ENVIRONMENT DIVISION.                                            ELUOUTPT
00022                                                                   ELUOUTPT
00023  CONFIGURATION SECTION.                                           ELUOUTPT
00024  SOURCE-COMPUTER.    IBM-3033.                                    ELUOUTPT
00025  OBJECT-COMPUTER.    IBM-3033.                                    ELUOUTPT
00026 /*****************************************************************ELUOUTPT
00027 *                                                                *ELUOUTPT
00028 *    PROGRAM:    ELUOUTPT                                        *ELUOUTPT
00029 *    DATE:       15-JUN-1987                                     *ELUOUTPT
00030 *    AUTHOR:     EDWARD G LISS                                   *ELUOUTPT
00031 *    FUNCTION:                                                   *ELUOUTPT
00032 *        THIS UTILITY MODULE WILL ACCUMULATE OUTPUT REQUESTS     *ELUOUTPT
00033 *    FROM A TOPIC UNTIL A PAGE HAS BEEN FILLED.  THE PAGE WILL   *ELUOUTPT
00034 *    THEN BE WRITTEN AND A NEW PAGE STARTED.  THIS CONTINUES     *ELUOUTPT
00035 *    UNTIL THE CALLING MODULE SIGNALS THE END OF THE OUTPUT.     *ELUOUTPT
00036 *                                                                *ELUOUTPT
00037 ******************************************************************ELUOUTPT
00038 *                                                                *ELUOUTPT
00039 *                      MAINTENANCE HISTORY                       *ELUOUTPT
00040 *                                                                *ELUOUTPT
00041 *  MOD     DATE     BY  DRPT                ACTION               *ELUOUTPT
00042 * ----- ----------- --- ----- ---------------------------------- *ELUOUTPT
00043 * 01.00 15-JUN-1987 EGL       REWRITTEN IN STRUCTURES            *ELUOUTPT
00044 * 01.01 18-FEB-1988 NAC       MODIFY TO ACCOMODATE A MAXIMUM     *ELUOUTPT
00045 *                             OF 5 HEADER LINES; USE A MASK      *ELUOUTPT
00046 *                             LINE TO PAD OUT UNUSED LINES OF    *ELUOUTPT
00047 *                             OUTPUT BLOCK; CHANGE LOGIC TO TEST *ELUOUTPT
00048 *                             MAXIMUM LINES OF OUTPUT WRITTEN AT *ELUOUTPT
00049 *                             A TIME.                            *ELUOUTPT
00050 * 01.02 08-JUL-1988 EGL       MODIFIED TO ACCOMODATE NEW STORAGE *ELUOUTPT
00051 *                             MANAGEMENT SCHEME.                 *ELUOUTPT
00052 * 01.03 08-JUL-1991 RJL       REVISED TO PROVIDE IMPROVED PAGE   *ELUOUTPT
00053 *                             OVERFLOW PROCESSING AND IMPROVE    *ELUOUTPT
00054 *                             LOGIC FLOW.                        *ELUOUTPT
00055 *       12-AUG-2003 AKK GEN IN QE TO TEST ORDER OF COMPILE       *ELUOUTPT
00056 ******************************************************************ELUOUTPT
00057 /                                                                 ELUOUTPT
00058  DATA DIVISION.                                                   ELUOUTPT
00059                                                                   ELUOUTPT
00060  WORKING-STORAGE SECTION.                                         ELUOUTPT
00061                                                                   ELUOUTPT
00062  01  WS-WORK-FIELDS.                                              ELUOUTPT
00063      05 WS-COF-SUB                PICTURE S9(04) COMP.            ELUOUTPT
00064      05 WS-MAX-LINE-CNT           PICTURE S9(04) COMP  VALUE +22. ELUOUTPT
00065      05 WS-NBR-MASK-LINES         PICTURE S9(04) COMP.            ELUOUTPT
00066      05 WS-PQ-FILL-LINES          PICTURE S9(04) COMP  VALUE +4.  ELUOUTPT
00067      05 WS-PQ-MAX-BLK-LINES       PICTURE S9(04) COMP  VALUE +4.  ELUOUTPT
00068      05 WS-PQ-MAX-OCCURS          PICTURE S9(04) COMP.            ELUOUTPT
00069      05 WS-PQ-REM-LINES           PICTURE S9(04) COMP.            ELUOUTPT
00070      05 WS-SUB                    PICTURE S9(04) COMP.            ELUOUTPT
00071                                                                   ELUOUTPT
00072  01  WS-CONSTANT-FIELDS.                                          ELUOUTPT
00073      05 WS-CONTD-LINE             PICTURE  X(79)                  ELUOUTPT
00074         VALUE '                              --- (CONTINUED) ---  ELUOUTPT
00075 -             '                            '.                     ELUOUTPT
00076 /                                                                 ELUOUTPT
00077  LINKAGE SECTION.                                                 ELUOUTPT
00078  01  DFHCOMMAREA.                                                 ELUOUTPT
00079      COPY ELSCOMMC.                                               ELUOUTPT
00080 /  *** CIA  AREA ***                                              ELUOUTPT
00081      COPY ELSCIA2C.                                               ELUOUTPT
00082 /  *** IO PARM AREA ***                                           ELUOUTPT
00083      COPY ELSIOPMC.                                               ELUOUTPT
00084 /  *** PAGE AREA ***                                              ELUOUTPT
00085      COPY ELSPAGQC.                                               ELUOUTPT
00086 /  *** OUTPUT TEXT AREA ***                                       ELUOUTPT
00087      COPY ELSOUTPC.                                               ELUOUTPT
00088  PROCEDURE DIVISION.                                              ELUOUTPT
00089 ************************************************************      ELUOUTPT
00090 *                                                          *      ELUOUTPT
00091 *                    PROCEDURE DIVISION                    *      ELUOUTPT
00092 *                                                          *      ELUOUTPT
00093 ************************************************************      ELUOUTPT
00094                                                                   ELUOUTPT
00095                                                                   ELUOUTPT
00096 ************************************************************      ELUOUTPT
00097 *                                                          *      ELUOUTPT
00098 *        ASSEMBLE OUTPUT PAGES                             *      ELUOUTPT
00099 *                                                          *      ELUOUTPT
00100 ************************************************************      ELUOUTPT
00101  ASM-OUTPT-PG.                                                    ELUOUTPT
00102      PERFORM 000-INITIALIZE.                                      ELUOUTPT
00103      PERFORM 100-PROCESS.                                         ELUOUTPT
00104      PERFORM 900-TERMINATE.                                       ELUOUTPT
00105      GOBACK.                                                      ELUOUTPT
00106                                                                   ELUOUTPT
00107                                                                   ELUOUTPT
00108 ************************************************************      ELUOUTPT
00109 *                                                          *      ELUOUTPT
00110 *        INITIALIZE                                        *      ELUOUTPT
00111 *                                                          *      ELUOUTPT
00112 ************************************************************      ELUOUTPT
00113  000-INITIALIZE.                                                  ELUOUTPT
00114      PERFORM 010-ESTAB-COMMAREA-ADDR.                             ELUOUTPT
00115      PERFORM 020-ESTAB-ELSOUTP-ADDR.                              ELUOUTPT
00116      PERFORM 030-ESTAB-ELSPAGE-ADDR.                              ELUOUTPT
00117                                                                   ELUOUTPT
00118                                                                   ELUOUTPT
00119 ************************************************************      ELUOUTPT
00120 *                                                          *      ELUOUTPT
00121 *        ESTABLISH COMMAREA ADDRESSING                     *      ELUOUTPT
00122 *                                                          *      ELUOUTPT
00123 ************************************************************      ELUOUTPT
00124  010-ESTAB-COMMAREA-ADDR.                                         ELUOUTPT
00125      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELUOUTPT
00126          PERFORM AB01-INVALID-COMMAREA.                           ELUOUTPT
00127      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUOUTPT
00128                            ADDRESS OF                             ELUOUTPT
00129          CIA-ELS-COMMON-INTERFACE-AREA.                           ELUOUTPT
00130                                                                   ELUOUTPT
00131                                                                   ELUOUTPT
00132 ************************************************************      ELUOUTPT
00133 *                                                          *      ELUOUTPT
00134 *        ESTABLISH PARAMETER ADDRESSING                    *      ELUOUTPT
00135 *                                                          *      ELUOUTPT
00136 ************************************************************      ELUOUTPT
00137  020-ESTAB-ELSOUTP-ADDR.                                          ELUOUTPT
00138      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELUOUTPT
00139      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUOUTPT
00140                      ADDRESS OF COF-OUTPUT-INTERFACE.             ELUOUTPT
00141      IF NOT CIA-RC-OK                                             ELUOUTPT
00142          PERFORM AB03-INVALID-PARM.                               ELUOUTPT
00143      IF    COF-NBR-HDR-LINES < 0                                  ELUOUTPT
00144         OR COF-NBR-DTL-LINES < 0                                  ELUOUTPT
00145         OR COF-NBR-TRL-LINES < 0                                  ELUOUTPT
00146          PERFORM AB02-PGM-LOGIC-ERR.                              ELUOUTPT
00147                                                                   ELUOUTPT
00148                                                                   ELUOUTPT
00149 ************************************************************      ELUOUTPT
00150 *                                                          *      ELUOUTPT
00151 *        ESTABLISH PAGE FILE ADDRESSING                    *      ELUOUTPT
00152 *                                                          *      ELUOUTPT
00153 ************************************************************      ELUOUTPT
00154  030-ESTAB-ELSPAGE-ADDR.                                          ELUOUTPT
00155      PERFORM 031-SET-ADDR-ELSPAGE-IOPM.                           ELUOUTPT
00156      MOVE CIA-MVO TO WS-PQ-MAX-OCCURS.                            ELUOUTPT
00157      IF CIA-RC-PTR-NULL                                           ELUOUTPT
00158          PERFORM 031-ALLOC-ELSPAGE-IOPM.                          ELUOUTPT
00159      IF IOP-REC-PTR = NULL                                        ELUOUTPT
00160          PERFORM 032-ALLOC-ELSPAGE-RECAREA.                       ELUOUTPT
00161      SET ADDRESS OF PQ-PAGE-QUEUE TO IOP-REC-PTR.                 ELUOUTPT
00162                                                                   ELUOUTPT
00163                                                                   ELUOUTPT
00164 ************************************************************      ELUOUTPT
00165 *                                                          *      ELUOUTPT
00166 *        ALLOCATE PAGE FILE IO BLOCK                       *      ELUOUTPT
00167 *                                                          *      ELUOUTPT
00168 ************************************************************      ELUOUTPT
00169  031-ALLOC-ELSPAGE-IOPM.                                          ELUOUTPT
00170      SET CIA-ELSPAGE-DDN TO TRUE.                                 ELUOUTPT
00171      SET CIA-STG-GETMAIN TO TRUE.                                 ELUOUTPT
00172      EXEC CICS LINK PROGRAM('ELUSTGMG') COMMAREA(DFHCOMMAREA)     ELUOUTPT
00173          END-EXEC.                                                ELUOUTPT
00174      PERFORM 031-SET-ADDR-ELSPAGE-IOPM.                           ELUOUTPT
00175                                                                   ELUOUTPT
00176                                                                   ELUOUTPT
00177 ************************************************************      ELUOUTPT
00178 *                                                          *      ELUOUTPT
00179 *        SET ADDRESS OF ELSPAGE AREA                       *      ELUOUTPT
00180 *                                                          *      ELUOUTPT
00181 ************************************************************      ELUOUTPT
00182  031-SET-ADDR-ELSPAGE-IOPM.                                       ELUOUTPT
00183      SET CIA-ELSPAGE-DDN TO TRUE.                                 ELUOUTPT
00184      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUOUTPT
00185                            ADDRESS OF                             ELUOUTPT
00186          IOP-INPUT-OUTPUT-PARAMETERS.                             ELUOUTPT
00187                                                                   ELUOUTPT
00188                                                                   ELUOUTPT
00189 ************************************************************      ELUOUTPT
00190 *                                                          *      ELUOUTPT
00191 *        ALLOCATE PAGE BUILD AREA                          *      ELUOUTPT
00192 *                                                          *      ELUOUTPT
00193 ************************************************************      ELUOUTPT
00194  032-ALLOC-ELSPAGE-RECAREA.                                       ELUOUTPT
00195      PERFORM 032-GETMAIN-ELSPAGE-RECAREA.                         ELUOUTPT
00196      PERFORM 032-INIT-ELSPAGE-RECAREA.                            ELUOUTPT
00197      PERFORM 032-DEL-ELSPAGE-OLD.                                 ELUOUTPT
00198                                                                   ELUOUTPT
00199                                                                   ELUOUTPT
00200 ************************************************************      ELUOUTPT
00201 *                                                          *      ELUOUTPT
00202 *        GET STORAGE FOR PAGE BUILD AREA                   *      ELUOUTPT
00203 *                                                          *      ELUOUTPT
00204 ************************************************************      ELUOUTPT
00205  032-GETMAIN-ELSPAGE-RECAREA.                                     ELUOUTPT
00206      SET CIA-ELSPAGE-DDN TO TRUE.                                 ELUOUTPT
00207      SET CIA-STG-GETMAIN TO TRUE.                                 ELUOUTPT
00208      SET IOP-GETMAIN-REC TO TRUE.                                 ELUOUTPT
00209      COMPUTE IOP-MAX-REC-LEN = LENGTH OF PQ-FIXED-PART +          ELUOUTPT
00210            (LENGTH OF PQ-PAGE-LINE * CIA-MVO).                    ELUOUTPT
00211      EXEC CICS LINK PROGRAM('ELUSTGMG') COMMAREA(DFHCOMMAREA)     ELUOUTPT
00212          END-EXEC.                                                ELUOUTPT
00213      SET ADDRESS OF PQ-PAGE-QUEUE TO IOP-REC-PTR.                 ELUOUTPT
00214                                                                   ELUOUTPT
00215                                                                   ELUOUTPT
00216 ************************************************************      ELUOUTPT
00217 *                                                          *      ELUOUTPT
00218 *        INITIALIZE PAGE BUILD AREA                        *      ELUOUTPT
00219 *                                                          *      ELUOUTPT
00220 ************************************************************      ELUOUTPT
00221  032-INIT-ELSPAGE-RECAREA.                                        ELUOUTPT
00222      MOVE +0  TO  PQ-PAGE-NO.                                     ELUOUTPT
00223      MOVE +0  TO  PQ-OCCURRENCE-COUNT.                            ELUOUTPT
00224      MOVE +0  TO  PQ-HEADER-LINE-COUNT.                           ELUOUTPT
00225                                                                   ELUOUTPT
00226                                                                   ELUOUTPT
00227 ************************************************************      ELUOUTPT
00228 *                                                          *      ELUOUTPT
00229 *        DELETE OLD PAGE FILE                              *      ELUOUTPT
00230 *                                                          *      ELUOUTPT
00231 ************************************************************      ELUOUTPT
00232  032-DEL-ELSPAGE-OLD.                                             ELUOUTPT
00233      SET CIA-ELSPAGE-DDN TO TRUE.                                 ELUOUTPT
00234      SET IOP-DEL TO TRUE.                                         ELUOUTPT
00235      PERFORM 811-CALL-IO.                                         ELUOUTPT
00236                                                                   ELUOUTPT
00237                                                                   ELUOUTPT
00238 ************************************************************      ELUOUTPT
00239 *                                                          *      ELUOUTPT
00240 *        PROCESS                                           *      ELUOUTPT
00241 *                                                          *      ELUOUTPT
00242 ************************************************************      ELUOUTPT
00243  100-PROCESS.                                                     ELUOUTPT
00244      IF COF-NEW-PAGE OR COF-NBR-HDR-LINES > 0                     ELUOUTPT
00245          PERFORM 200-BEG-NEW-PG.                                  ELUOUTPT
00246      PERFORM 300-ADD-TXT-PG.                                      ELUOUTPT
00247      IF COF-END                                                   ELUOUTPT
00248          PERFORM 400-WRITE-FINAL-PG.                              ELUOUTPT
00249                                                                   ELUOUTPT
00250                                                                   ELUOUTPT
00251 ************************************************************      ELUOUTPT
00252 *                                                          *      ELUOUTPT
00253 *        BEGIN NEW PAGE                                    *      ELUOUTPT
00254 *                                                          *      ELUOUTPT
00255 ************************************************************      ELUOUTPT
00256  200-BEG-NEW-PG.                                                  ELUOUTPT
00257      SET PQ-MORE-RECORDS TO TRUE.                                 ELUOUTPT
00258      IF PQ-OCCURRENCE-COUNT > PQ-HEADER-LINE-COUNT                ELUOUTPT
00259          PERFORM 810-WRITE-PG                                     ELUOUTPT
00260      ELSE                                                         ELUOUTPT
00261          PERFORM 210-INIT-NXT-PG.                                 ELUOUTPT
00262                                                                   ELUOUTPT
00263                                                                   ELUOUTPT
00264 ************************************************************      ELUOUTPT
00265 *                                                          *      ELUOUTPT
00266 *        INITIALIZE NEXT PAGE                              *      ELUOUTPT
00267 *                                                          *      ELUOUTPT
00268 ************************************************************      ELUOUTPT
00269  210-INIT-NXT-PG.                                                 ELUOUTPT
00270      IF COF-NBR-HDR-LINES = 0                                     ELUOUTPT
00271          PERFORM 211-RESV-PG-HDR                                  ELUOUTPT
00272      ELSE                                                         ELUOUTPT
00273          PERFORM 212-SET-NEW-PG-HDR.                              ELUOUTPT
00274                                                                   ELUOUTPT
00275                                                                   ELUOUTPT
00276 ************************************************************      ELUOUTPT
00277 *                                                          *      ELUOUTPT
00278 *        PRESERVE PAGE HEADINGS                            *      ELUOUTPT
00279 *                                                          *      ELUOUTPT
00280 ************************************************************      ELUOUTPT
00281  211-RESV-PG-HDR.                                                 ELUOUTPT
00282      MOVE PQ-HEADER-LINE-COUNT TO PQ-OCCURRENCE-COUNT.            ELUOUTPT
00283                                                                   ELUOUTPT
00284                                                                   ELUOUTPT
00285 ************************************************************      ELUOUTPT
00286 *                                                          *      ELUOUTPT
00287 *        SET NEW PAGE HEADINGS                             *      ELUOUTPT
00288 *                                                          *      ELUOUTPT
00289 ************************************************************      ELUOUTPT
00290  212-SET-NEW-PG-HDR.                                              ELUOUTPT
00291      PERFORM 213-MOVE-HDR-LINES                                   ELUOUTPT
00292          VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB >                ELUOUTPT
00293              COF-NBR-HDR-LINES.                                   ELUOUTPT
00294      MOVE COF-NBR-HDR-LINES TO PQ-HEADER-LINE-COUNT               ELUOUTPT
00295                                PQ-OCCURRENCE-COUNT.               ELUOUTPT
00296                                                                   ELUOUTPT
00297                                                                   ELUOUTPT
00298 ************************************************************      ELUOUTPT
00299 *                                                          *      ELUOUTPT
00300 *        MOVE HEADING LINES                                *      ELUOUTPT
00301 *                                                          *      ELUOUTPT
00302 ************************************************************      ELUOUTPT
00303  213-MOVE-HDR-LINES.                                              ELUOUTPT
00304      MOVE COF-HDR-LINE (WS-SUB) TO PQ-PAGE-LINE                   ELUOUTPT
00305          (WS-SUB).                                                ELUOUTPT
00306      INSPECT PQ-PAGE-LINE (WS-SUB) REPLACING                      ELUOUTPT
00307           ALL LOW-VALUES BY SPACES.                               ELUOUTPT
00308                                                                   ELUOUTPT
00309                                                                   ELUOUTPT
00310 ************************************************************      ELUOUTPT
00311 *                                                          *      ELUOUTPT
00312 *        ADD TEXT TO PAGE                                  *      ELUOUTPT
00313 *                                                          *      ELUOUTPT
00314 ************************************************************      ELUOUTPT
00315  300-ADD-TXT-PG.                                                  ELUOUTPT
00316      COMPUTE WS-PQ-REM-LINES = WS-MAX-LINE-CNT -                  ELUOUTPT
00317          PQ-OCCURRENCE-COUNT.                                     ELUOUTPT
00318      IF COF-NBR-DTL-LINES > WS-PQ-REM-LINES                       ELUOUTPT
00319          PERFORM 310-DET-PG-OFLOW-PROC                            ELUOUTPT
00320      ELSE                                                         ELUOUTPT
00321          PERFORM 340-ADD-TXT-PG.                                  ELUOUTPT
00322                                                                   ELUOUTPT
00323                                                                   ELUOUTPT
00324 ************************************************************      ELUOUTPT
00325 *                                                          *      ELUOUTPT
00326 *        DETERMINE PAGE OVERFLOW ACTION                    *      ELUOUTPT
00327 *                                                          *      ELUOUTPT
00328 ************************************************************      ELUOUTPT
00329  310-DET-PG-OFLOW-PROC.                                           ELUOUTPT
00330      IF WS-PQ-REM-LINES <= WS-PQ-MAX-BLK-LINES                    ELUOUTPT
00331          PERFORM 320-START-NEW-PG-OFLOW                           ELUOUTPT
00332      ELSE                                                         ELUOUTPT
00333          PERFORM 330-FILL-PG-THEN-OFLOW.                          ELUOUTPT
00334                                                                   ELUOUTPT
00335                                                                   ELUOUTPT
00336 ************************************************************      ELUOUTPT
00337 *                                                          *      ELUOUTPT
00338 *        START NEW PAGE FOR OVERFLOW                       *      ELUOUTPT
00339 *                                                          *      ELUOUTPT
00340 ************************************************************      ELUOUTPT
00341  320-START-NEW-PG-OFLOW.                                          ELUOUTPT
00342      IF NOT COF-DEFAULT-MASK                                      ELUOUTPT
00343          PERFORM 321-FILL-PG-MASK.                                ELUOUTPT
00344      PERFORM 200-BEG-NEW-PG.                                      ELUOUTPT
00345      PERFORM 399-MOVE-DTL-LINE                                    ELUOUTPT
00346          VARYING WS-COF-SUB FROM 1 BY 1                           ELUOUTPT
00347            UNTIL WS-COF-SUB > COF-NBR-DTL-LINES.                  ELUOUTPT
00348                                                                   ELUOUTPT
00349                                                                   ELUOUTPT
00350 ************************************************************      ELUOUTPT
00351 *                                                          *      ELUOUTPT
00352 *        FILL PAGE WITH BLANK LINE MASK                    *      ELUOUTPT
00353 *                                                          *      ELUOUTPT
00354 ************************************************************      ELUOUTPT
00355  321-FILL-PG-MASK.                                                ELUOUTPT
00356      PERFORM 322-MOVE-MASK                                        ELUOUTPT
00357          WS-PQ-REM-LINES TIMES.                                   ELUOUTPT
00358                                                                   ELUOUTPT
00359                                                                   ELUOUTPT
00360 ************************************************************      ELUOUTPT
00361 *                                                          *      ELUOUTPT
00362 *        MOVE BLANK LINE MASK TO OUTPUT                    *      ELUOUTPT
00363 *                                                          *      ELUOUTPT
00364 ************************************************************      ELUOUTPT
00365  322-MOVE-MASK.                                                   ELUOUTPT
00366      ADD 1 TO PQ-OCCURRENCE-COUNT.                                ELUOUTPT
00367      MOVE COF-MASK-LINE TO PQ-PAGE-LINE                           ELUOUTPT
00368          (PQ-OCCURRENCE-COUNT).                                   ELUOUTPT
00369                                                                   ELUOUTPT
00370                                                                   ELUOUTPT
00371 ************************************************************      ELUOUTPT
00372 *                                                          *      ELUOUTPT
00373 *        FILL CURRENT PAGE THEN OVERFLOW ONTO NEW PAGE     *      ELUOUTPT
00374 *                                                          *      ELUOUTPT
00375 ************************************************************      ELUOUTPT
00376  330-FILL-PG-THEN-OFLOW.                                          ELUOUTPT
00377      COMPUTE WS-PQ-FILL-LINES = WS-PQ-REM-LINES - 1.              ELUOUTPT
00378      PERFORM 399-MOVE-DTL-LINE                                    ELUOUTPT
00379          VARYING WS-COF-SUB FROM 1 BY 1                           ELUOUTPT
00380            UNTIL WS-COF-SUB > WS-PQ-FILL-LINES.                   ELUOUTPT
00381      PERFORM 331-INS-CONTD-MSG.                                   ELUOUTPT
00382      PERFORM 200-BEG-NEW-PG.                                      ELUOUTPT
00383      PERFORM 399-MOVE-DTL-LINE                                    ELUOUTPT
00384          VARYING WS-COF-SUB FROM WS-COF-SUB BY 1                  ELUOUTPT
00385            UNTIL WS-COF-SUB > COF-NBR-DTL-LINES.                  ELUOUTPT
00386                                                                   ELUOUTPT
00387                                                                   ELUOUTPT
00388 ************************************************************      ELUOUTPT
00389 *                                                          *      ELUOUTPT
00390 *        INSERT CONTINUED MESSAGE                          *      ELUOUTPT
00391 *                                                          *      ELUOUTPT
00392 ************************************************************      ELUOUTPT
00393  331-INS-CONTD-MSG.                                               ELUOUTPT
00394      ADD 1 TO PQ-OCCURRENCE-COUNT.                                ELUOUTPT
00395      MOVE WS-CONTD-LINE TO PQ-PAGE-LINE                           ELUOUTPT
00396          (PQ-OCCURRENCE-COUNT).                                   ELUOUTPT
00397                                                                   ELUOUTPT
00398                                                                   ELUOUTPT
00399 ************************************************************      ELUOUTPT
00400 *                                                          *      ELUOUTPT
00401 *        ADD TEXT TO CURRENT PAGE                          *      ELUOUTPT
00402 *                                                          *      ELUOUTPT
00403 ************************************************************      ELUOUTPT
00404  340-ADD-TXT-PG.                                                  ELUOUTPT
00405      PERFORM 399-MOVE-DTL-LINE                                    ELUOUTPT
00406          VARYING WS-COF-SUB FROM 1 BY 1                           ELUOUTPT
00407            UNTIL WS-COF-SUB > COF-NBR-DTL-LINES.                  ELUOUTPT
00408                                                                   ELUOUTPT
00409                                                                   ELUOUTPT
00410 ************************************************************      ELUOUTPT
00411 *                                                          *      ELUOUTPT
00412 *        MOVE DETAIL LINES                                 *      ELUOUTPT
00413 *                                                          *      ELUOUTPT
00414 ************************************************************      ELUOUTPT
00415  399-MOVE-DTL-LINE.                                               ELUOUTPT
00416      ADD 1 TO PQ-OCCURRENCE-COUNT.                                ELUOUTPT
00417      IF PQ-OCCURRENCE-COUNT > WS-PQ-MAX-OCCURS                    ELUOUTPT
00418          PERFORM AB99-MISC-ERROR.                                 ELUOUTPT
00419      MOVE COF-DTL-LINE (WS-COF-SUB) TO PQ-PAGE-LINE               ELUOUTPT
00420          (PQ-OCCURRENCE-COUNT).                                   ELUOUTPT
00421      INSPECT PQ-PAGE-LINE (PQ-OCCURRENCE-COUNT) REPLACING         ELUOUTPT
00422           ALL LOW-VALUES BY SPACES.                               ELUOUTPT
00423                                                                   ELUOUTPT
00424                                                                   ELUOUTPT
00425 ************************************************************      ELUOUTPT
00426 *                                                          *      ELUOUTPT
00427 *        WRITE FINAL PAGE                                  *      ELUOUTPT
00428 *                                                          *      ELUOUTPT
00429 ************************************************************      ELUOUTPT
00430  400-WRITE-FINAL-PG.                                              ELUOUTPT
00431      SET PQ-LAST-RECORD TO TRUE.                                  ELUOUTPT
00432      IF    PQ-OCCURRENCE-COUNT > PQ-HEADER-LINE-COUNT             ELUOUTPT
00433         OR PQ-PAGE-NO <= 1                                        ELUOUTPT
00434          PERFORM 810-WRITE-PG.                                    ELUOUTPT
00435                                                                   ELUOUTPT
00436                                                                   ELUOUTPT
00437 ************************************************************      ELUOUTPT
00438 *                                                          *      ELUOUTPT
00439 *        WRITE CURRENT PAGE                                *      ELUOUTPT
00440 *                                                          *      ELUOUTPT
00441 ************************************************************      ELUOUTPT
00442  810-WRITE-PG.                                                    ELUOUTPT
00443      ADD +1 TO PQ-PAGE-NO.                                        ELUOUTPT
00444      COMPUTE IOP-REC-LEN                                          ELUOUTPT
00445              =   LENGTH OF PQ-FIXED-PART                          ELUOUTPT
00446                + LENGTH OF PQ-PAGE-LINE *                         ELUOUTPT
00447          PQ-OCCURRENCE-COUNT.                                     ELUOUTPT
00448      SET CIA-ELSPAGE-DDN  TO TRUE.                                ELUOUTPT
00449      SET IOP-ADD          TO TRUE.                                ELUOUTPT
00450      SET IOP-FCQ-NONE     TO TRUE.                                ELUOUTPT
00451      SET IOP-KVQ-NONE     TO TRUE.                                ELUOUTPT
00452      PERFORM 811-CALL-IO.                                         ELUOUTPT
00453      PERFORM 210-INIT-NXT-PG.                                     ELUOUTPT
00454                                                                   ELUOUTPT
00455                                                                   ELUOUTPT
00456 ************************************************************      ELUOUTPT
00457 *                                                          *      ELUOUTPT
00458 *        CALL INPUT OUTPUT ROUTINE                         *      ELUOUTPT
00459 *                                                          *      ELUOUTPT
00460 ************************************************************      ELUOUTPT
00461  811-CALL-IO.                                                     ELUOUTPT
00462      EXEC CICS LINK PROGRAM('ELUIOPGM') COMMAREA(DFHCOMMAREA)     ELUOUTPT
00463          END-EXEC.                                                ELUOUTPT
00464                                                                   ELUOUTPT
00465                                                                   ELUOUTPT
00466 ************************************************************      ELUOUTPT
00467 *                                                          *      ELUOUTPT
00468 *        TERMINATE                                         *      ELUOUTPT
00469 *                                                          *      ELUOUTPT
00470 ************************************************************      ELUOUTPT
00471  900-TERMINATE.                                                   ELUOUTPT
00472      INITIALIZE COF-DTL                                           ELUOUTPT
00473                 COF-NBR-HDR-LINES                                 ELUOUTPT
00474                 COF-NBR-DTL-LINES                                 ELUOUTPT
00475                 COF-NBR-TRL-LINES                                 ELUOUTPT
00476                 COF-FUNCTION.                                     ELUOUTPT
00477                                                                   ELUOUTPT
00478                                                                   ELUOUTPT
00479 ************************************************************      ELUOUTPT
00480 *                                                          *      ELUOUTPT
00481 *        INVALID COMMAREA                                  *      ELUOUTPT
00482 *                                                          *      ELUOUTPT
00483 ************************************************************      ELUOUTPT
00484  AB01-INVALID-COMMAREA.                                           ELUOUTPT
00485      EXEC CICS ABEND ABCODE('EL01') END-EXEC.                     ELUOUTPT
00486                                                                   ELUOUTPT
00487                                                                   ELUOUTPT
00488 ************************************************************      ELUOUTPT
00489 *                                                          *      ELUOUTPT
00490 *        PROGRAM LOGIC ERROR                               *      ELUOUTPT
00491 *                                                          *      ELUOUTPT
00492 ************************************************************      ELUOUTPT
00493  AB02-PGM-LOGIC-ERR.                                              ELUOUTPT
00494      SET CIA-AB-PGM-LOGIC TO TRUE.                                ELUOUTPT
00495      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUOUTPT
00496                                                                   ELUOUTPT
00497                                                                   ELUOUTPT
00498 ************************************************************      ELUOUTPT
00499 *                                                          *      ELUOUTPT
00500 *        INVALID PARAMETER                                 *      ELUOUTPT
00501 *                                                          *      ELUOUTPT
00502 ************************************************************      ELUOUTPT
00503  AB03-INVALID-PARM.                                               ELUOUTPT
00504      SET CIA-AB-PARM-ERR TO TRUE.                                 ELUOUTPT
00505      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUOUTPT
00506                                                                   ELUOUTPT
00507                                                                   ELUOUTPT
00508 ************************************************************      ELUOUTPT
00509 *                                                          *      ELUOUTPT
00510 *        SIGNAL SUBSCRIPT ERROR                            *      ELUOUTPT
00511 *                                                          *      ELUOUTPT
00512 ************************************************************      ELUOUTPT
00513  AB99-MISC-ERROR.                                                 ELUOUTPT
00514      SET CIA-AB-UNDEF TO TRUE.                                    ELUOUTPT
00515      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUOUTPT
