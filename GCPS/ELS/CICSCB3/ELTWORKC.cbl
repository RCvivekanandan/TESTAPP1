00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELTWORKC
00003  PROGRAM-ID.         ELTWORKC.                                       LV001
00004                                                                   ELTWORKC
00005  AUTHOR.             LUCY TORRES.                                 ELTWORKC
00006                                                                   ELTWORKC
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTWORKC
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTWORKC
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTWORKC
00010                      233 N. MICHIGAN AVE                          ELTWORKC
00011                      CHICAGO, ILLINOIS 60601                      ELTWORKC
00012                                                                   ELTWORKC
00013  DATE-WRITTEN.       15-APR-1987.                                 ELTWORKC
00014                                                                   ELTWORKC
00015  DATE-COMPILED.                                                   ELTWORKC
00016                                                                   ELTWORKC
00017  SECURITY.           COPYRIGHT 1986,                              ELTWORKC
00018                      HEALTH CARE SERVICE CORPORATION              ELTWORKC
00019      SKIP3                                                        ELTWORKC
00020  ENVIRONMENT DIVISION.                                            ELTWORKC
00021                                                                   ELTWORKC
00022  CONFIGURATION SECTION.                                           ELTWORKC
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELTWORKC
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELTWORKC
00025      EJECT                                                        ELTWORKC
00026 ******************************************************************ELTWORKC
00027 *                                                                *ELTWORKC
00028 *    DATE:       15-APR-1987                                     *ELTWORKC
00029 *    AUTHOR:     LUCY TORRES                                     *ELTWORKC
00030 *    FUNCTION:   TOPIC DISPLAY FOR WORKMENS COMPENSATION.        *ELTWORKC
00031 *                                                                *ELTWORKC
00032 *    NOTES:      X---                                            *ELTWORKC
00033 *                                                                *ELTWORKC
00034 ******************************************************************ELTWORKC
00035 *                                                                *ELTWORKC
00036 *                      MAINTENANCE HISTORY                       *ELTWORKC
00037 *                                                                *ELTWORKC
00038 *  MOD     DATE     BY  DRPT                ACTION               *ELTWORKC
00039 * ----- ----------- --- ----- ---------------------------------- *ELTWORKC
00040 * 01.00 14-APR-1987 LET ----- CREATED                            *ELTWORKC
00041 *                                                                *ELTWORKC
00042 * 01.01 04-MAY-1988 REB       USER REQUESTED OUTPUT TO BE        *ELTWORKC
00043 *                             DISPLAYED IN 2-COLUMN FORMAT.      *ELTWORKC
00044 * 02.00 01-NOV-1989 EGL       DESTRUCTED PROGRAM AND ADDED THE   *ELTWORKC
00045 *                             STORAGE MANAGEMENT ENHANCEMENTS.   *ELTWORKC
00046 *                                                                *ELTWORKC
00047 ******************************************************************ELTWORKC
00048                                                                   ELTWORKC
00049  DATA DIVISION.                                                   ELTWORKC
00050  WORKING-STORAGE SECTION.                                         ELTWORKC
00051 ****************************************************************  ELTWORKC
00052 *      CONSTANTS, SWITCHES, HOLD-AREA, WORK-AREA               *  ELTWORKC
00053 ****************************************************************  ELTWORKC
00054  01  WORK-FIELDS.                                                 ELTWORKC
00055      05  WS-BEGIN                PIC  X(24) VALUE                 ELTWORKC
00056          '** ELTWORKC WS BEGINS **'.                              ELTWORKC
00057      05  WS-UNPACK-AGE           PIC S9(3)  VALUE ZEROS.          ELTWORKC
00058      05  WS-MASK-LINE.                                            ELTWORKC
00059          10  FILLER              PIC  X(30) VALUE SPACES.         ELTWORKC
00060          10  FILLER              PIC  X(01) VALUE '|'.            ELTWORKC
00061      05  WS-MAX-AGE-AMT.                                          ELTWORKC
00062          10  WS-MAX-AGE-NUM      PIC  ZZ9.                        ELTWORKC
00063          10  FILLER              PIC  X(01).                      ELTWORKC
00064      05  WS-MIN-AGE-AMT.                                          ELTWORKC
00065          10  WS-MIN-AGE-NUM      PIC  ZZ9.                        ELTWORKC
00066          10  FILLER              PIC  X(01).                      ELTWORKC
00067      05  WS-BAR                  PIC  X(01) VALUE '|'.            ELTWORKC
00068 /                                                                 ELTWORKC
00069 ****************************************************************  ELTWORKC
00070 *                    LITERAL TEXT AREA                         *  ELTWORKC
00071 ****************************************************************  ELTWORKC
00072  01  HEADER-LINE-3.                                               ELTWORKC
00073      05  FILLER                  PIC  X(43) VALUE                 ELTWORKC
00074          'THE FOLLOWING DISPLAYED BENEFITS ARE FOR:'.             ELTWORKC
00075      05  FILLER                  PIC  X(7)  VALUE 'WORKERS'.      ELTWORKC
00076      05  FILLER                  PIC  X     VALUE QUOTE.          ELTWORKC
00077      05  FILLER                  PIC  X(13) VALUE                 ELTWORKC
00078              ' COMPENSATION'.                                     ELTWORKC
00079      05  FILLER                  PIC  X(15) VALUE SPACES.         ELTWORKC
00080                                                                   ELTWORKC
00081 **   HEADING AREA.                                                ELTWORKC
00082  01  WS-LOB-1.                                                    ELTWORKC
00083      05  FILLER                  PIC  X(07) VALUE                 ELTWORKC
00084          'WORKERS'.                                               ELTWORKC
00085      05  FILLER                  PIC  X     VALUE QUOTE.          ELTWORKC
00086      05  FILLER                  PIC  X(22) VALUE                 ELTWORKC
00087          ' COMPENSATION LINE '.                                   ELTWORKC
00088                                                                   ELTWORKC
00089  01  WS-LOB-2.                                                    ELTWORKC
00090      05  FILLER                  PIC  X(30) VALUE                 ELTWORKC
00091          'OF BUSINESS APPLICATION '.                              ELTWORKC
00092                                                                   ELTWORKC
00093  01  WS-INVEST-1.                                                 ELTWORKC
00094      05  FILLER                  PIC  X(07) VALUE                 ELTWORKC
00095          'WORKERS'.                                               ELTWORKC
00096      05  FILLER                  PIC  X     VALUE QUOTE.          ELTWORKC
00097      05  FILLER                  PIC  X(22) VALUE                 ELTWORKC
00098          ' COMPENSATION '.                                        ELTWORKC
00099                                                                   ELTWORKC
00100  01  WS-INVEST-2.                                                 ELTWORKC
00101      05  FILLER                  PIC  X(30) VALUE                 ELTWORKC
00102          'INVESTIGATION'.                                         ELTWORKC
00103                                                                   ELTWORKC
00104  01  WS-INVEST-PAT.                                               ELTWORKC
00105      05  FILLER                  PIC  X(30) VALUE                 ELTWORKC
00106          'INVESTIGATION OF PATIENTS '.                            ELTWORKC
00107                                                                   ELTWORKC
00108  01  WS-MAX-AGE-LITERAL-1.                                        ELTWORKC
00109      05  FILLER                  PIC  X(30) VALUE                 ELTWORKC
00110          'MAXIMUM AGE FOR PATIENT TO'.                            ELTWORKC
00111                                                                   ELTWORKC
00112  01  WS-MAX-AGE-LITERAL-2.                                        ELTWORKC
00113      05  FILLER                  PIC  X(25) VALUE                 ELTWORKC
00114          'BE CONSIDERED FOR WORKERS'.                             ELTWORKC
00115      05  FILLER                  PIC  X     VALUE QUOTE.          ELTWORKC
00116      05  FILLER                  PIC  X(04) VALUE SPACES.         ELTWORKC
00117                                                                   ELTWORKC
00118  01  WS-MAX-AGE-LITERAL-3.                                        ELTWORKC
00119      05  FILLER                  PIC  X(30) VALUE                 ELTWORKC
00120          'COMPENSATION INVESTIGATION'.                            ELTWORKC
00121                                                                   ELTWORKC
00122  01  WS-MIN-AGE-LITERAL-1.                                        ELTWORKC
00123      05  FILLER                  PIC  X(30) VALUE                 ELTWORKC
00124          'MINIMUM AGE FOR PATIENT TO'.                            ELTWORKC
00125                                                                   ELTWORKC
00126  01  WS-MIN-AGE-LITERAL-2.                                        ELTWORKC
00127      05  FILLER                  PIC  X(25) VALUE                 ELTWORKC
00128          'BE CONSIDERED FOR WORKERS'.                             ELTWORKC
00129      05  FILLER                  PIC  X     VALUE QUOTE.          ELTWORKC
00130      05  FILLER                  PIC  X(04) VALUE SPACES.         ELTWORKC
00131                                                                   ELTWORKC
00132  01  WS-MIN-AGE-LITERAL-3.                                        ELTWORKC
00133      05  FILLER                  PIC  X(30) VALUE                 ELTWORKC
00134          'COMPENSATION INVESTIGATION'.                            ELTWORKC
00135                                                                   ELTWORKC
00136  01  WS-OUTPUT-AREA.                                              ELTWORKC
00137      05  WS-HEADINGS-CNT          PIC S9(04)  VALUE +0  COMP-3.   ELTWORKC
00138      05  WS-LINE-CNT              PIC S9(04)  VALUE +0  COMP-3.   ELTWORKC
00139      05  WS-OUTPUT                PIC X(1580) VALUE SPACES.       ELTWORKC
00140      05  WS-OUTPUT-ENTRY REDEFINES WS-OUTPUT                      ELTWORKC
00141                                   OCCURS 20 TIMES                 ELTWORKC
00142                                   INDEXED BY WS-OUTPUT-IDX.       ELTWORKC
00143          10  WS-OUTPUT-LINE.                                      ELTWORKC
00144              15  WS-HEADING       PIC X(30).                      ELTWORKC
00145              15  WS-DIVIDER       PIC X(01).                      ELTWORKC
00146              15  FILLER           PIC X(02).                      ELTWORKC
00147              15  WS-TEXT          PIC X(46).                      ELTWORKC
00148                                                                   ELTWORKC
00149 /                                                                 ELTWORKC
00150  LINKAGE SECTION.                                                 ELTWORKC
00151  01  DFHCOMMAREA.                                                 ELTWORKC
00152      COPY ELSCOMMC.                                               ELTWORKC
00153 /                                                                 ELTWORKC
00154      COPY ELSCIA2C.                                               ELTWORKC
00155 /                                                                 ELTWORKC
00156      COPY ELSCMDSC.                                               ELTWORKC
00157 /                                                                 ELTWORKC
00158      COPY ELSCMIFC.                                               ELTWORKC
00159 /                                                                 ELTWORKC
00160      COPY ELSIOPMC.                                               ELTWORKC
00161 /                                                                 ELTWORKC
00162      COPY ELSKEYSC.                                               ELTWORKC
00163 /                                                                 ELTWORKC
00164      COPY ELSOUTPC.                                               ELTWORKC
00165 /                                                                 ELTWORKC
00166      COPY ELSSRTPC.                                               ELTWORKC
00167 /                                                                 ELTWORKC
00168      COPY ELSTCWAC.                                               ELTWORKC
00169 /                                                                 ELTWORKC
00170      COPY ELSSSCBC.                                               ELTWORKC
00171 /                                                                 ELTWORKC
00172  01  ELR-GRP-REC-AREA.                                            ELTWORKC
00173      COPY GCGROUPC.                                               ELTWORKC
00174 /                                                                 ELTWORKC
00175      EJECT                                                        ELTWORKC
00176  PROCEDURE DIVISION.                                              ELTWORKC
00177                                                                   ELTWORKC
00178      PERFORM ELTWORKC-INITIALIZE.                                 ELTWORKC
00179      PERFORM ELTWORKC-PROCESS.                                    ELTWORKC
00180      GOBACK.                                                      ELTWORKC
00181                                                                   ELTWORKC
00182                                                                   ELTWORKC
00183 ************************************************************      ELTWORKC
00184 *                                                          *      ELTWORKC
00185 *        ELTWORKC.INITIALIZE                               *      ELTWORKC
00186 *                                                          *      ELTWORKC
00187 ************************************************************      ELTWORKC
00188  ELTWORKC-INITIALIZE.                                             ELTWORKC
00189      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTWORKC
00190      PERFORM SET-ADDR-TO-CIA.                                     ELTWORKC
00191      PERFORM SET-ADDR-TO-SSCB.                                    ELTWORKC
00192      PERFORM SET-ADDR-TO-CODES-MANUAL-INT.                        ELTWORKC
00193      PERFORM SET-ADDR-TO-OUTPUT-INTERFACE.                        ELTWORKC
00194      PERFORM SET-ADDR-TO-FILE-KEY-AREA.                           ELTWORKC
00195      PERFORM SET-ADDR-TO-COMPRESSION-AREA.                        ELTWORKC
00196      PERFORM SET-ADDR-TO-GROUP-SPEC-RECORD.                       ELTWORKC
00197                                                                   ELTWORKC
00198                                                                   ELTWORKC
00199 ************************************************************      ELTWORKC
00200 *                                                          *      ELTWORKC
00201 *        CHECK FOR VALID COMMAREA                          *      ELTWORKC
00202 *                                                          *      ELTWORKC
00203 ************************************************************      ELTWORKC
00204  CHECK-FOR-VALID-COMMAREA.                                        ELTWORKC
00205      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTWORKC
00206          EXEC CICS ABEND                                          ELTWORKC
00207                    ABCODE('EL01')                                 ELTWORKC
00208          END-EXEC.                                                ELTWORKC
00209                                                                   ELTWORKC
00210                                                                   ELTWORKC
00211 ************************************************************      ELTWORKC
00212 *                                                          *      ELTWORKC
00213 *        SET ADDR TO CIA                                   *      ELTWORKC
00214 *                                                          *      ELTWORKC
00215 ************************************************************      ELTWORKC
00216  SET-ADDR-TO-CIA.                                                 ELTWORKC
00217      IF ECA-CIA-PTR = NULL                                        ELTWORKC
00218          EXEC CICS ABEND                                          ELTWORKC
00219                    ABCODE('EL02')                                 ELTWORKC
00220          END-EXEC                                                 ELTWORKC
00221      ELSE                                                         ELTWORKC
00222         CALL 'ELUINISM' USING DFHCOMMAREA                         ELTWORKC
00223              ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.            ELTWORKC
00224                                                                   ELTWORKC
00225                                                                   ELTWORKC
00226                                                                   ELTWORKC
00227                                                                   ELTWORKC
00228 ************************************************************      ELTWORKC
00229 *                                                          *      ELTWORKC
00230 *        SET ADDR TO SSCB                                  *      ELTWORKC
00231 *                                                          *      ELTWORKC
00232 ************************************************************      ELTWORKC
00233  SET-ADDR-TO-SSCB.                                                ELTWORKC
00234      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTWORKC
00235      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWORKC
00236                 ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.           ELTWORKC
00237      IF CIA-RC-PTR-NULL                                           ELTWORKC
00238          SET CIA-AB-PARM-MISSING  TO  TRUE                        ELTWORKC
00239          PERFORM ABEND-THE-PROGRAM.                               ELTWORKC
00240                                                                   ELTWORKC
00241                                                                   ELTWORKC
00242 ************************************************************      ELTWORKC
00243 *                                                          *      ELTWORKC
00244 *        SET ADDR TO CODES MANUAL INT                      *      ELTWORKC
00245 *                                                          *      ELTWORKC
00246 ************************************************************      ELTWORKC
00247  SET-ADDR-TO-CODES-MANUAL-INT.                                    ELTWORKC
00248      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTWORKC
00249      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWORKC
00250                 ADDRESS OF CMF-CODES-MANUAL-INTERFACE.            ELTWORKC
00251                                                                   ELTWORKC
00252                                                                   ELTWORKC
00253 ************************************************************      ELTWORKC
00254 *                                                          *      ELTWORKC
00255 *        SET ADDR TO OUTPUT INTERFACE                      *      ELTWORKC
00256 *                                                          *      ELTWORKC
00257 ************************************************************      ELTWORKC
00258  SET-ADDR-TO-OUTPUT-INTERFACE.                                    ELTWORKC
00259      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTWORKC
00260      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWORKC
00261                 ADDRESS OF COF-OUTPUT-INTERFACE.                  ELTWORKC
00262                                                                   ELTWORKC
00263                                                                   ELTWORKC
00264 ************************************************************      ELTWORKC
00265 *                                                          *      ELTWORKC
00266 *        SET ADDR TO FILE KEY AREA                         *      ELTWORKC
00267 *                                                          *      ELTWORKC
00268 ************************************************************      ELTWORKC
00269  SET-ADDR-TO-FILE-KEY-AREA.                                       ELTWORKC
00270      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTWORKC
00271      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWORKC
00272                 ADDRESS OF KWA-FILE-KEY-WORK-AREA.                ELTWORKC
00273                                                                   ELTWORKC
00274                                                                   ELTWORKC
00275 ************************************************************      ELTWORKC
00276 *                                                          *      ELTWORKC
00277 *        SET ADDR TO COMPRESSION AREA                      *      ELTWORKC
00278 *                                                          *      ELTWORKC
00279 ************************************************************      ELTWORKC
00280  SET-ADDR-TO-COMPRESSION-AREA.                                    ELTWORKC
00281      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTWORKC
00282      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWORKC
00283                 ADDRESS OF TCAR-COMPRESSION-WORK-AREA.            ELTWORKC
00284                                                                   ELTWORKC
00285                                                                   ELTWORKC
00286 ************************************************************      ELTWORKC
00287 *                                                          *      ELTWORKC
00288 *        SET ADDR TO GROUP SPEC RECORD                     *      ELTWORKC
00289 *                                                          *      ELTWORKC
00290 ************************************************************      ELTWORKC
00291  SET-ADDR-TO-GROUP-SPEC-RECORD.                                   ELTWORKC
00292      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTWORKC
00293      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWORKC
00294                 ADDRESS OF ELR-GRP-REC-AREA.                      ELTWORKC
00295 /***********************************************************      ELTWORKC
00296 *                                                          *      ELTWORKC
00297 *        ELTWORKC.PROCESS                                  *      ELTWORKC
00298 *                                                          *      ELTWORKC
00299 ************************************************************      ELTWORKC
00300  ELTWORKC-PROCESS.                                                ELTWORKC
00301      MOVE 'GROUP'        TO  CMF-RECORD-PREFIX.                   ELTWORKC
00302      MOVE HEADER-LINE-3  TO  COF-HDR-LINE (2).                    ELTWORKC
00303      MOVE ALL '-'        TO  COF-HDR-LINE (3).                    ELTWORKC
00304      MOVE WS-MASK-LINE   TO COF-MASK-LINE.                        ELTWORKC
00305      PERFORM DISPLAY-THE-HEADER.                                  ELTWORKC
00306      SET  WS-OUTPUT-IDX  TO +1.                                   ELTWORKC
00307      IF GCG-WKR-COMP-L-O-B  NOT  =  ZEROS                         ELTWORKC
00308          PERFORM SETUP-LINE-OF-BUSINESS-SENTENC.                  ELTWORKC
00309      IF GCG-WC-INVESN-MODE  NOT  =  ZEROS                         ELTWORKC
00310          PERFORM SETUP-INVESTIGATION-SENTENCE.                    ELTWORKC
00311      IF GCG-WC-MAR-STAT-IND  NOT  =  ZEROS                        ELTWORKC
00312          PERFORM SETUP-MARITAL-STATUS-SENTENCE.                   ELTWORKC
00313      IF GCG-WC-MAX-AGE  NOT  =  ZEROS                             ELTWORKC
00314          PERFORM SETUP-MAXIMUM-AGE-SENTENCE.                      ELTWORKC
00315      IF GCG-WC-MIN-AGE  NOT  = ZEROS                              ELTWORKC
00316          PERFORM SETUP-MINIMUM-AGE-SENTENCE.                      ELTWORKC
00317      PERFORM DISPLAY-END-OF-TEXT.                                 ELTWORKC
00318      PERFORM END-THE-DISPLAY.                                     ELTWORKC
00319                                                                   ELTWORKC
00320                                                                   ELTWORKC
00321 ************************************************************      ELTWORKC
00322 *                                                          *      ELTWORKC
00323 *        DISPLAY THE HEADER                                *      ELTWORKC
00324 *                                                          *      ELTWORKC
00325 ************************************************************      ELTWORKC
00326  DISPLAY-THE-HEADER.                                              ELTWORKC
00327      MOVE +3   TO  COF-NBR-HDR-LINES.                             ELTWORKC
00328      MOVE 'P'  TO  COF-FUNCTION.                                  ELTWORKC
00329      PERFORM CALL-OUTPUT-INTERFACE.                               ELTWORKC
00330 /***********************************************************      ELTWORKC
00331 *                                                          *      ELTWORKC
00332 *        SETUP LINE OF BUSINESS SENTENCE                   *      ELTWORKC
00333 *                                                          *      ELTWORKC
00334 ************************************************************      ELTWORKC
00335  SETUP-LINE-OF-BUSINESS-SENTENC.                                  ELTWORKC
00336      MOVE GCG-WKR-COMP-L-O-B  TO  CMF-CODE-VALUE.                 ELTWORKC
00337      MOVE 'WKR-COMP-L-O-B'    TO  CMF-ELEMENT-SYSTEM-NAME.        ELTWORKC
00338      PERFORM TRANSLATE-THE-DATA-FIELD.                            ELTWORKC
00339      MOVE WS-LOB-1            TO  WS-HEADING (1).                 ELTWORKC
00340      MOVE WS-LOB-2            TO  WS-HEADING (2).                 ELTWORKC
00341      MOVE +2                  TO  WS-HEADINGS-CNT.                ELTWORKC
00342      PERFORM FORMAT-SENTENCE.                                     ELTWORKC
00343                                                                   ELTWORKC
00344                                                                   ELTWORKC
00345 ************************************************************      ELTWORKC
00346 *                                                          *      ELTWORKC
00347 *        SETUP INVESTIGATION SENTENCE                      *      ELTWORKC
00348 *                                                          *      ELTWORKC
00349 ************************************************************      ELTWORKC
00350  SETUP-INVESTIGATION-SENTENCE.                                    ELTWORKC
00351      MOVE GCG-WC-INVESN-MODE  TO  CMF-CODE-VALUE.                 ELTWORKC
00352      MOVE 'WC-INVESN-MODE'    TO  CMF-ELEMENT-SYSTEM-NAME.        ELTWORKC
00353      PERFORM TRANSLATE-THE-DATA-FIELD.                            ELTWORKC
00354      MOVE WS-INVEST-1         TO  WS-HEADING (1).                 ELTWORKC
00355      MOVE WS-INVEST-2         TO  WS-HEADING (2).                 ELTWORKC
00356      MOVE +2                  TO  WS-HEADINGS-CNT.                ELTWORKC
00357      PERFORM FORMAT-SENTENCE.                                     ELTWORKC
00358                                                                   ELTWORKC
00359                                                                   ELTWORKC
00360 ************************************************************      ELTWORKC
00361 *                                                          *      ELTWORKC
00362 *        SETUP MARITAL STATUS SENTENCE                     *      ELTWORKC
00363 *                                                          *      ELTWORKC
00364 ************************************************************      ELTWORKC
00365  SETUP-MARITAL-STATUS-SENTENCE.                                   ELTWORKC
00366      MOVE GCG-WC-MAR-STAT-IND  TO  CMF-CODE-VALUE.                ELTWORKC
00367      MOVE 'WC-MAR-STAT-IND'    TO  CMF-ELEMENT-SYSTEM-NAME.       ELTWORKC
00368      PERFORM TRANSLATE-THE-DATA-FIELD.                            ELTWORKC
00369      MOVE WS-INVEST-PAT        TO  WS-HEADING (1).                ELTWORKC
00370      MOVE +1                   TO  WS-HEADINGS-CNT.               ELTWORKC
00371      PERFORM FORMAT-SENTENCE.                                     ELTWORKC
00372 /***********************************************************      ELTWORKC
00373 *                                                          *      ELTWORKC
00374 *        SETUP MAXIMUM AGE SENTENCE                        *      ELTWORKC
00375 *                                                          *      ELTWORKC
00376 ************************************************************      ELTWORKC
00377  SETUP-MAXIMUM-AGE-SENTENCE.                                      ELTWORKC
00378      MOVE GCG-WC-MAX-AGE        TO  WS-MAX-AGE-NUM.               ELTWORKC
00379      MOVE +3                    TO  WS-LINE-CNT.                  ELTWORKC
00380      MOVE WS-MAX-AGE-LITERAL-1  TO  WS-HEADING (1).               ELTWORKC
00381      MOVE WS-MAX-AGE-LITERAL-2  TO  WS-HEADING (2).               ELTWORKC
00382      MOVE WS-MAX-AGE-LITERAL-3  TO  WS-HEADING (3).               ELTWORKC
00383      MOVE WS-MAX-AGE-AMT        TO  WS-TEXT (1).                  ELTWORKC
00384      PERFORM MOVE-WS-OUTPUT-INTO-OUTPUT-INT                       ELTWORKC
00385          VARYING WS-OUTPUT-IDX FROM 1 BY 1                        ELTWORKC
00386                  UNTIL   WS-OUTPUT-IDX > WS-LINE-CNT + 1.         ELTWORKC
00387      PERFORM CALL-OUTPUT-INTERFACE.                               ELTWORKC
00388      PERFORM INITIALIZE-WS-OUTPUT-AREA.                           ELTWORKC
00389                                                                   ELTWORKC
00390                                                                   ELTWORKC
00391 ************************************************************      ELTWORKC
00392 *                                                          *      ELTWORKC
00393 *        SETUP MINIMUM AGE SENTENCE                        *      ELTWORKC
00394 *                                                          *      ELTWORKC
00395 ************************************************************      ELTWORKC
00396  SETUP-MINIMUM-AGE-SENTENCE.                                      ELTWORKC
00397      MOVE GCG-WC-MIN-AGE        TO  WS-MIN-AGE-NUM.               ELTWORKC
00398      MOVE +3                    TO  WS-LINE-CNT.                  ELTWORKC
00399      MOVE WS-MIN-AGE-LITERAL-1  TO  WS-HEADING (1).               ELTWORKC
00400      MOVE WS-MIN-AGE-LITERAL-2  TO  WS-HEADING (2).               ELTWORKC
00401      MOVE WS-MIN-AGE-LITERAL-3  TO  WS-HEADING (3).               ELTWORKC
00402      MOVE WS-MIN-AGE-AMT        TO  WS-TEXT (1).                  ELTWORKC
00403      PERFORM MOVE-WS-OUTPUT-INTO-OUTPUT-INT                       ELTWORKC
00404          VARYING WS-OUTPUT-IDX FROM 1 BY 1                        ELTWORKC
00405                  UNTIL   WS-OUTPUT-IDX > WS-LINE-CNT + 1.         ELTWORKC
00406      PERFORM CALL-OUTPUT-INTERFACE.                               ELTWORKC
00407      PERFORM INITIALIZE-WS-OUTPUT-AREA.                           ELTWORKC
00408 /***********************************************************      ELTWORKC
00409 *                                                          *      ELTWORKC
00410 *        END THE DISPLAY                                   *      ELTWORKC
00411 *                                                          *      ELTWORKC
00412 ************************************************************      ELTWORKC
00413  END-THE-DISPLAY.                                                 ELTWORKC
00414      MOVE 'E'  TO  COF-FUNCTION.                                  ELTWORKC
00415      PERFORM CALL-OUTPUT-INTERFACE.                               ELTWORKC
00416                                                                   ELTWORKC
00417                                                                   ELTWORKC
00418 ************************************************************      ELTWORKC
00419 *                                                          *      ELTWORKC
00420 *        TRANSLATE THE DATA FIELD                          *      ELTWORKC
00421 *                                                          *      ELTWORKC
00422 ************************************************************      ELTWORKC
00423  TRANSLATE-THE-DATA-FIELD.                                        ELTWORKC
00424      EXEC CICS LINK PROGRAM ('ELUCMIF')                           ELTWORKC
00425                     COMMAREA (DFHCOMMAREA)                        ELTWORKC
00426                     END-EXEC.                                     ELTWORKC
00427      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTWORKC
00428      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWORKC
00429                 ADDRESS OF CMF-DESCR.                             ELTWORKC
00430                                                                   ELTWORKC
00431                                                                   ELTWORKC
00432 ************************************************************      ELTWORKC
00433 *                                                          *      ELTWORKC
00434 *        CALL OUTPUT INTERFACE                             *      ELTWORKC
00435 *                                                          *      ELTWORKC
00436 ************************************************************      ELTWORKC
00437  CALL-OUTPUT-INTERFACE.                                           ELTWORKC
00438      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTWORKC
00439                     COMMAREA (DFHCOMMAREA)                        ELTWORKC
00440                     END-EXEC.                                     ELTWORKC
00441 /***********************************************************      ELTWORKC
00442 *                                                          *      ELTWORKC
00443 *        FORMAT SENTENCE                                   *      ELTWORKC
00444 *                                                          *      ELTWORKC
00445 ************************************************************      ELTWORKC
00446  FORMAT-SENTENCE.                                                 ELTWORKC
00447      INITIALIZE TCAR-FROM-SUB                                     ELTWORKC
00448                 TCAR-FROM-AREA                                    ELTWORKC
00449                 TCAR-FROM-LENGTH                                  ELTWORKC
00450                 TCAR-X.                                           ELTWORKC
00451      PERFORM                                                      ELTWORKC
00452          VARYING CMF-DESCR-IDX FROM 1 BY 1                        ELTWORKC
00453                   UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES       ELTWORKC
00454              ADD +1 TO TCAR-FROM-SUB                              ELTWORKC
00455              MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO               ELTWORKC
00456                   TCAR-FROM-LINE (TCAR-FROM-SUB)                  ELTWORKC
00457      END-PERFORM.                                                 ELTWORKC
00458      ADD  +1  TO TCAR-FROM-SUB.                                   ELTWORKC
00459      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELTWORKC
00460      PERFORM FORMAT-THE-RAW-TEXT.                                 ELTWORKC
00461      PERFORM MOVE-TEXT-TO-DISPLAY.                                ELTWORKC
00462      PERFORM DISPLAY-OUTPUT-AREA.                                 ELTWORKC
00463                                                                   ELTWORKC
00464                                                                   ELTWORKC
00465 ************************************************************      ELTWORKC
00466 *                                                          *      ELTWORKC
00467 *        FORMAT THE RAW TEXT                               *      ELTWORKC
00468 *                                                          *      ELTWORKC
00469 ************************************************************      ELTWORKC
00470  FORMAT-THE-RAW-TEXT.                                             ELTWORKC
00471      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTWORKC
00472      MOVE +10 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTWORKC
00473      MOVE +46 TO TCAR-OUTPUT-FIELD-1-LEN                          ELTWORKC
00474                  TCAR-OUTPUT-FIELD-2-LEN                          ELTWORKC
00475                  TCAR-OUTPUT-FIELD-3-LEN                          ELTWORKC
00476                  TCAR-OUTPUT-FIELD-4-LEN                          ELTWORKC
00477                  TCAR-OUTPUT-FIELD-5-LEN                          ELTWORKC
00478                  TCAR-OUTPUT-FIELD-6-LEN                          ELTWORKC
00479                  TCAR-OUTPUT-FIELD-7-LEN                          ELTWORKC
00480                  TCAR-OUTPUT-FIELD-8-LEN                          ELTWORKC
00481                  TCAR-OUTPUT-FIELD-9-LEN                          ELTWORKC
00482                  TCAR-OUTPUT-FIELD-10-LEN.                        ELTWORKC
00483      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTWORKC
00484 /***********************************************************      ELTWORKC
00485 *                                                          *      ELTWORKC
00486 *        MOVE TEXT TO DISPLAY                              *      ELTWORKC
00487 *                                                          *      ELTWORKC
00488 ************************************************************      ELTWORKC
00489  MOVE-TEXT-TO-DISPLAY.                                            ELTWORKC
00490      PERFORM                                                      ELTWORKC
00491          VARYING TCAR-X FROM 1 BY 1                               ELTWORKC
00492                   UNTIL TCAR-X > TCAR-OUTPUT-FIELDS-USED          ELTWORKC
00493            ADD  1                         TO WS-LINE-CNT          ELTWORKC
00494            MOVE TCAR-OPF-DATA (TCAR-X)    TO                      ELTWORKC
00495                 WS-TEXT (WS-OUTPUT-IDX)                           ELTWORKC
00496            SET  WS-OUTPUT-IDX UP BY +1                            ELTWORKC
00497      END-PERFORM.                                                 ELTWORKC
00498                                                                   ELTWORKC
00499                                                                   ELTWORKC
00500 ************************************************************      ELTWORKC
00501 *                                                          *      ELTWORKC
00502 *        DISPLAY OUTPUT AREA                               *      ELTWORKC
00503 *                                                          *      ELTWORKC
00504 ************************************************************      ELTWORKC
00505  DISPLAY-OUTPUT-AREA.                                             ELTWORKC
00506      IF WS-LINE-CNT < WS-HEADINGS-CNT                             ELTWORKC
00507          ADD  +1 TO WS-LINE-CNT.                                  ELTWORKC
00508      PERFORM MOVE-WS-OUTPUT-INTO-OUTPUT-INT                       ELTWORKC
00509          VARYING WS-OUTPUT-IDX FROM 1 BY 1                        ELTWORKC
00510                  UNTIL   WS-OUTPUT-IDX > WS-LINE-CNT + 1.         ELTWORKC
00511      PERFORM CALL-OUTPUT-INTERFACE.                               ELTWORKC
00512      PERFORM INITIALIZE-WS-OUTPUT-AREA.                           ELTWORKC
00513                                                                   ELTWORKC
00514                                                                   ELTWORKC
00515 ************************************************************      ELTWORKC
00516 *                                                          *      ELTWORKC
00517 *        MOVE WS OUTPUT INTO OUTPUT INTERFACE AREA         *      ELTWORKC
00518 *                                                          *      ELTWORKC
00519 ************************************************************      ELTWORKC
00520  MOVE-WS-OUTPUT-INTO-OUTPUT-INT.                                  ELTWORKC
00521      ADD  +1                 TO COF-NBR-DTL-LINES.                ELTWORKC
00522      MOVE WS-BAR             TO WS-DIVIDER                        ELTWORKC
00523          (WS-OUTPUT-IDX).                                         ELTWORKC
00524      MOVE WS-OUTPUT-LINE (WS-OUTPUT-IDX)                          ELTWORKC
00525                              TO COF-DTL-LINE                      ELTWORKC
00526          (COF-NBR-DTL-LINES).                                     ELTWORKC
00527 /***********************************************************      ELTWORKC
00528 *                                                          *      ELTWORKC
00529 *        DISPLAY END OF TEXT                               *      ELTWORKC
00530 *                                                          *      ELTWORKC
00531 ************************************************************      ELTWORKC
00532  DISPLAY-END-OF-TEXT.                                             ELTWORKC
00533      MOVE +1                 TO COF-NBR-DTL-LINES.                ELTWORKC
00534      MOVE ALL '-'            TO COF-DTL-LINE (1).                 ELTWORKC
00535      PERFORM CALL-OUTPUT-INTERFACE.                               ELTWORKC
00536                                                                   ELTWORKC
00537                                                                   ELTWORKC
00538 ************************************************************      ELTWORKC
00539 *                                                          *      ELTWORKC
00540 *        INITIALIZE WS OUTPUT AREA                         *      ELTWORKC
00541 *                                                          *      ELTWORKC
00542 ************************************************************      ELTWORKC
00543  INITIALIZE-WS-OUTPUT-AREA.                                       ELTWORKC
00544      INITIALIZE WS-HEADINGS-CNT                                   ELTWORKC
00545                 WS-LINE-CNT                                       ELTWORKC
00546                 WS-OUTPUT.                                        ELTWORKC
00547      SET  WS-OUTPUT-IDX TO +1.                                    ELTWORKC
00548                                                                   ELTWORKC
00549                                                                   ELTWORKC
00550 ************************************************************      ELTWORKC
00551 *                                                          *      ELTWORKC
00552 *        ABEND THE PROGRAM                                 *      ELTWORKC
00553 *                                                          *      ELTWORKC
00554 ************************************************************      ELTWORKC
00555  ABEND-THE-PROGRAM.                                               ELTWORKC
00556      EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.                ELTWORKC
00557 /                                                                 ELTWORKC
00558      COPY ELSTCOMP.                                               ELTWORKC
