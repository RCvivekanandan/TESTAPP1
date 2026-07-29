00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSPGDSP
00003  PROGRAM-ID.         ELSPGDSP.                                       LV002
00004 *                            (FORMERLY ELELPGML)                  ELSPGDSP
00005  AUTHOR.             RICHARD J. LUKETICH.                         ELSPGDSP
00006                                                                   ELSPGDSP
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSPGDSP
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSPGDSP
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSPGDSP
00010                      233 N. MICHIGAN AVE                          ELSPGDSP
00011                      CHICAGO, ILLINOIS 60601                      ELSPGDSP
00012                                                                   ELSPGDSP
00013  DATE-WRITTEN.       05-DEC-1986.                                 ELSPGDSP
00014                                                                   ELSPGDSP
00015  DATE-COMPILED.                                                   ELSPGDSP
00016                                                                   ELSPGDSP
00017  SECURITY.           COPYRIGHT 1986,                              ELSPGDSP
00018                      HEALTH CARE SERVICE CORPORATION              ELSPGDSP
00019      SKIP3                                                        ELSPGDSP
00020 *    CONVERTED 7/87 TO USE TEMP STORAGE RATHER THAN A VSAM        ELSPGDSP
00021 *    KSDS.           ED LISS  7/87                                ELSPGDSP
00022 *                                                                 ELSPGDSP
00023 *    SHIFTED THE FUNCTION OF PF3 TO PF9 AND REASSIGNED PF3        ELSPGDSP
00024 *    AS THE MENU BACKOUT KEY                                      ELSPGDSP
00025 *                                                                 ELSPGDSP
00026 *    CONVERT PROGRAM FROM STRUCTURES  04/10/89   ED LISS          ELSPGDSP
00027 *                                                                 ELSPGDSP
00028 *    STORAGE MANAGEMENT ENHANCEMENTS  04/12/89   GEM              ELSPGDSP
00029 *                                                                 ELSPGDSP
00030 *    CORRECTED ASRA DUE TO SMA ERROR 07/10/89  EGL                ELSPGDSP
00031 *                                                                 ELSPGDSP
00032 *       13-APR-2003 AKK       REGEN TO TEST ORDER OF COMPILES    *ELSPGDSP
00033                                                                   ELSPGDSP
00034  ENVIRONMENT DIVISION.                                            ELSPGDSP
00035                                                                   ELSPGDSP
00036  CONFIGURATION SECTION.                                           ELSPGDSP
00037  SOURCE-COMPUTER.    IBM-3033.                                    ELSPGDSP
00038  OBJECT-COMPUTER.    IBM-3033.                                    ELSPGDSP
00039      EJECT                                                        ELSPGDSP
00040  DATA DIVISION.                                                   ELSPGDSP
00041                                                                   ELSPGDSP
00042  FILE SECTION.                                                    ELSPGDSP
00043                                                                   ELSPGDSP
00044  WORKING-STORAGE SECTION.                                         ELSPGDSP
00045                                                                   ELSPGDSP
00046  01  WS-MISC-VALUES.                                              ELSPGDSP
00047      02 WS-PRTR-ID               PICTURE  X(04).                  ELSPGDSP
00048      02 WS-PG-SUBSCR             PICTURE S9(04)          COMP.    ELSPGDSP
00049      02 WS-TWA-LEN               PICTURE S9(04)          COMP.    ELSPGDSP
00050      02 WS-MAX-PAGE-LINES        PICTURE S9(03)          COMP-3   ELSPGDSP
00051                                  VALUE +23.                       ELSPGDSP
00052                                                                   ELSPGDSP
00053  01  WS-EIBRCODE.                                                 ELSPGDSP
00054      02 WS-EIBRCODE-0.                                            ELSPGDSP
00055         03 WS-EIBRCODE-0-HI      PICTURE  X(01).                  ELSPGDSP
00056         03 WS-EIBRCODE-0-LO      PICTURE  X(01).                  ELSPGDSP
00057      02 FILLER                   REDEFINES WS-EIBRCODE-0          ELSPGDSP
00058                                  PICTURE S9(04)          COMP.    ELSPGDSP
00059         88 WS-EIBRCODE-NOTFND    VALUE +129.                      ELSPGDSP
00060         88 WS-EIBRCODE-ENDFILE   VALUE +015.                      ELSPGDSP
00061                                                                   ELSPGDSP
00062  01  WS-MESSAGES.                                                 ELSPGDSP
00063      02 WS-PFK-MSG               PICTURE  X(79)                   ELSPGDSP
00064                                  VALUE '<PF2>=PRINT ALL <PF3-4-5-9ELSPGDSP
00065 -                                      '>=INQ PGM <PF7/8>=PREV/NEXELSPGDSP
00066 -                                      'T <PF10/11>=FIRST/LAST'.  ELSPGDSP
00067      02 WS-HC-REQ-UNDEF-MSG      PICTURE  X(79)                   ELSPGDSP
00068                                  VALUE 'UNDEFINED ERROR IN HARDCOPELSPGDSP
00069 -                                      'Y REQUEST.  CONTACT SSD TEELSPGDSP
00070 -                                      'CHNICAL SUPPORT.'.        ELSPGDSP
00071      02 WS-HC-REQ-PRTR-MSG       PICTURE  X(79)                   ELSPGDSP
00072                                  VALUE 'NO PRINTER HAS BEEN ASSIGNELSPGDSP
00073 -                                      'ED TO THIS TERMINAL FOR HAELSPGDSP
00074 -                                      'RDCOPY PRINTING.'.        ELSPGDSP
00075      02 WS-HC-REQ-TS-MSG         PICTURE  X(79)                   ELSPGDSP
00076                                  VALUE 'INSUFFICIENT TEMPORARY STOELSPGDSP
00077 -                                      'RAGE FOR HARDCOPY PRINT.  ELSPGDSP
00078 -                                      'TRY AGAIN LATER.'.        ELSPGDSP
00079      02 WS-HC-REQ-OUT-MSG        PICTURE  X(79)                   ELSPGDSP
00080                                  VALUE 'YOUR HARDCOPY PRINTER IS OELSPGDSP
00081 -                                      'UT OF SERVICE.  CONTACT THELSPGDSP
00082 -                                      'THE HELP DESK, X6675.'.   ELSPGDSP
00083      02 WS-HC-REQ-CMPLT-MSG      PICTURE  X(79)                   ELSPGDSP
00084                                  VALUE 'HARDCOPY REQUEST COMPLETEDELSPGDSP
00085 -                                      '.'.                       ELSPGDSP
00086      02 WS-HC-REQ-IN-PROC-MSG    PICTURE  X(79)                   ELSPGDSP
00087                                  VALUE 'PROCESSING HARDCOPY REQUESELSPGDSP
00088 -                                      'T.'.                      ELSPGDSP
00089      EJECT                                                        ELSPGDSP
00090      COPY EL00SETC.                                               ELSPGDSP
00091      EJECT                                                        ELSPGDSP
00092      COPY EL03SETC.                                               ELSPGDSP
00093      EJECT                                                        ELSPGDSP
00094 *01  PRINT-AREA.                                                  ELSPGDSP
00095      COPY PRNCOBOL.                                               ELSPGDSP
00096      EJECT                                                        ELSPGDSP
00097      COPY DFHAID.                                                 ELSPGDSP
00098      EJECT                                                        ELSPGDSP
00099      COPY DFHBMSCA.                                               ELSPGDSP
00100      EJECT                                                        ELSPGDSP
00101  LINKAGE SECTION.                                                 ELSPGDSP
00102  01  DFHCOMMAREA.                                                 ELSPGDSP
00103      COPY ELSCOMMC.                                               ELSPGDSP
00104      EJECT                                                        ELSPGDSP
00105      COPY ELSCIA2C.                                               ELSPGDSP
00106      EJECT                                                        ELSPGDSP
00107      COPY ELSSSCBC.                                               ELSPGDSP
00108      EJECT                                                        ELSPGDSP
00109      COPY ELSIOPMC.                                               ELSPGDSP
00110      EJECT                                                        ELSPGDSP
00111      COPY ELSPAGQC.                                               ELSPGDSP
00112      EJECT                                                        ELSPGDSP
00113      COPY TUACOBOL.                                               ELSPGDSP
00114      EJECT                                                        ELSPGDSP
00115  PROCEDURE DIVISION.                                              ELSPGDSP
00116 ************************************************************      ELSPGDSP
00117 *                                                          *      ELSPGDSP
00118 *                    PROCEDURE DIVISION                    *      ELSPGDSP
00119 *                                                          *      ELSPGDSP
00120 ************************************************************      ELSPGDSP
00121                                                                   ELSPGDSP
00122                                                                   ELSPGDSP
00123 ************************************************************      ELSPGDSP
00124 *                                                          *      ELSPGDSP
00125 *        DISPLAY OUTPUT PAGES                              *      ELSPGDSP
00126 *                                                          *      ELSPGDSP
00127 ************************************************************      ELSPGDSP
00128  DISPLAY-OUTPUT-PAGES.                                            ELSPGDSP
00129      PERFORM DISPLAY-OUTPUT-PAGES-INITIALIZ.                      ELSPGDSP
00130      PERFORM DISPLAY-OUTPUT-PAGES-PROCESS.                        ELSPGDSP
00131      PERFORM DISPLAY-OUTPUT-PAGES-TERMINATE.                      ELSPGDSP
00132                                                                   ELSPGDSP
00133                                                                   ELSPGDSP
00134 ************************************************************      ELSPGDSP
00135 *                                                          *      ELSPGDSP
00136 *        DISPLAY OUTPUT PAGES.INITIALIZE                   *      ELSPGDSP
00137 *                                                          *      ELSPGDSP
00138 ************************************************************      ELSPGDSP
00139  DISPLAY-OUTPUT-PAGES-INITIALIZ.                                  ELSPGDSP
00140      PERFORM ESTABLISH-ADDRESSING-TO-COMMUN.                      ELSPGDSP
00141      PERFORM ESTABLISH-ADDRESSING-TO-STATUS.                      ELSPGDSP
00142      PERFORM ESTABLISH-ADDRESSING-TO-IO-PAR.                      ELSPGDSP
00143                                                                   ELSPGDSP
00144                                                                   ELSPGDSP
00145 ************************************************************      ELSPGDSP
00146 *                                                          *      ELSPGDSP
00147 *        ESTABLISH ADDRESSING TO COMMUNICATION AREA        *      ELSPGDSP
00148 *                                                          *      ELSPGDSP
00149 ************************************************************      ELSPGDSP
00150  ESTABLISH-ADDRESSING-TO-COMMUN.                                  ELSPGDSP
00151      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELSPGDSP
00152          PERFORM SIGNAL-INVALID-COMMAREA-LENGTH.                  ELSPGDSP
00153      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSPGDSP
00154          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSPGDSP
00155                                                                   ELSPGDSP
00156                                                                   ELSPGDSP
00157 ************************************************************      ELSPGDSP
00158 *                                                          *      ELSPGDSP
00159 *        SIGNAL INVALID COMMAREA LENGTH                    *      ELSPGDSP
00160 *                                                          *      ELSPGDSP
00161 ************************************************************      ELSPGDSP
00162  SIGNAL-INVALID-COMMAREA-LENGTH.                                  ELSPGDSP
00163      EXEC CICS ABEND                                              ELSPGDSP
00164                ABCODE('EL01')                                     ELSPGDSP
00165           END-EXEC.                                               ELSPGDSP
00166      EJECT                                                        ELSPGDSP
00167                                                                   ELSPGDSP
00168                                                                   ELSPGDSP
00169 ************************************************************      ELSPGDSP
00170 *                                                          *      ELSPGDSP
00171 *        ESTABLISH ADDRESSING TO STATUS BLOCK AREA         *      ELSPGDSP
00172 *                                                          *      ELSPGDSP
00173 ************************************************************      ELSPGDSP
00174  ESTABLISH-ADDRESSING-TO-STATUS.                                  ELSPGDSP
00175      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSPGDSP
00176      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPGDSP
00177          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSPGDSP
00178      IF CIA-RC-PTR-NULL                                           ELSPGDSP
00179          PERFORM SIGNAL-INVALID-STATUS-BLOCK.                     ELSPGDSP
00180                                                                   ELSPGDSP
00181                                                                   ELSPGDSP
00182 ************************************************************      ELSPGDSP
00183 *                                                          *      ELSPGDSP
00184 *        SIGNAL INVALID STATUS BLOCK                       *      ELSPGDSP
00185 *                                                          *      ELSPGDSP
00186 ************************************************************      ELSPGDSP
00187  SIGNAL-INVALID-STATUS-BLOCK.                                     ELSPGDSP
00188      SET CIA-AB-PARM-MISSING TO TRUE.                             ELSPGDSP
00189      EXEC CICS ABEND                                              ELSPGDSP
00190                ABCODE(CIA-ABCODE)                                 ELSPGDSP
00191           END-EXEC.                                               ELSPGDSP
00192      EJECT                                                        ELSPGDSP
00193                                                                   ELSPGDSP
00194                                                                   ELSPGDSP
00195 ************************************************************      ELSPGDSP
00196 *                                                          *      ELSPGDSP
00197 *        ESTABLISH ADDRESSING TO IO PARM AREA              *      ELSPGDSP
00198 *                                                          *      ELSPGDSP
00199 ************************************************************      ELSPGDSP
00200  ESTABLISH-ADDRESSING-TO-IO-PAR.                                  ELSPGDSP
00201      SET CIA-ELSPAGE-DDN TO TRUE.                                 ELSPGDSP
00202      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPGDSP
00203          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSPGDSP
00204      IF CIA-RC-PTR-NULL                                           ELSPGDSP
00205         PERFORM ALLOCATE-IO-PARM-AREA.                            ELSPGDSP
00206      SET CIA-ELSPAGE-DDN TO TRUE.                                 ELSPGDSP
00207      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSPGDSP
00208          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSPGDSP
00209      SET IOP-REC-PTR TO NULL.                                     ELSPGDSP
00210                                                                   ELSPGDSP
00211                                                                   ELSPGDSP
00212 ************************************************************      ELSPGDSP
00213 *                                                          *      ELSPGDSP
00214 *        ALLOCATE IO PARM AREA                             *      ELSPGDSP
00215 *                                                          *      ELSPGDSP
00216 ************************************************************      ELSPGDSP
00217  ALLOCATE-IO-PARM-AREA.                                           ELSPGDSP
00218      SET CIA-STG-GETMAIN   TO TRUE.                               ELSPGDSP
00219      EXEC CICS LINK                                               ELSPGDSP
00220                PROGRAM('ELUSTGMG')                                ELSPGDSP
00221                COMMAREA(DFHCOMMAREA)                              ELSPGDSP
00222           END-EXEC.                                               ELSPGDSP
00223      EJECT                                                        ELSPGDSP
00224                                                                   ELSPGDSP
00225                                                                   ELSPGDSP
00226 ************************************************************      ELSPGDSP
00227 *                                                          *      ELSPGDSP
00228 *        DISPLAY OUTPUT PAGES.PROCESS                      *      ELSPGDSP
00229 *                                                          *      ELSPGDSP
00230 ************************************************************      ELSPGDSP
00231  DISPLAY-OUTPUT-PAGES-PROCESS.                                    ELSPGDSP
00232      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSPGDSP
00233                OR SSB-COMPLETED    (SSB-SELECTOR-STATE)           ELSPGDSP
00234          PERFORM DISPLAY-FIRST-OUTPUT-PAGE                        ELSPGDSP
00235      ELSE                                                         ELSPGDSP
00236          PERFORM PERFORM-OPERATION-REQUESTED-BY.                  ELSPGDSP
00237                                                                   ELSPGDSP
00238                                                                   ELSPGDSP
00239 ************************************************************      ELSPGDSP
00240 *                                                          *      ELSPGDSP
00241 *        DISPLAY FIRST OUTPUT PAGE                         *      ELSPGDSP
00242 *                                                          *      ELSPGDSP
00243 ************************************************************      ELSPGDSP
00244  DISPLAY-FIRST-OUTPUT-PAGE.                                       ELSPGDSP
00245      PERFORM INITIALIZE-WORK-AREAS.                               ELSPGDSP
00246      PERFORM READ-PAGE-FILE-RECORD.                               ELSPGDSP
00247      PERFORM SEND-PAGE-MAP.                                       ELSPGDSP
00248      PERFORM DISPLAY-PAGE.                                        ELSPGDSP
00249                                                                   ELSPGDSP
00250                                                                   ELSPGDSP
00251 ************************************************************      ELSPGDSP
00252 *                                                          *      ELSPGDSP
00253 *        INITIALIZE WORK AREAS                             *      ELSPGDSP
00254 *                                                          *      ELSPGDSP
00255 ************************************************************      ELSPGDSP
00256  INITIALIZE-WORK-AREAS.                                           ELSPGDSP
00257      MOVE 1            TO  SSB-PD-CURRENT-PAGE.                   ELSPGDSP
00258      MOVE 0            TO  SSB-PD-LAST-PAGE.                      ELSPGDSP
00259      SET SSB-CONT-TASK TO  TRUE.                                  ELSPGDSP
00260      SET SSB-PRIMARY-SCREEN-OUT (SSB-SELECTOR-STATE) TO           ELSPGDSP
00261          TRUE.                                                    ELSPGDSP
00262      EJECT                                                        ELSPGDSP
00263                                                                   ELSPGDSP
00264                                                                   ELSPGDSP
00265 ************************************************************      ELSPGDSP
00266 *                                                          *      ELSPGDSP
00267 *        PERFORM OPERATION REQUESTED BY TERMINAL OPERATOR  *      ELSPGDSP
00268 *                                                          *      ELSPGDSP
00269 ************************************************************      ELSPGDSP
00270  PERFORM-OPERATION-REQUESTED-BY.                                  ELSPGDSP
00271      IF EIBAID =    DFHENTER                                      ELSPGDSP
00272                            OR DFHPF8  OR DFHPF20                  ELSPGDSP
00273          PERFORM DISPLAY-NEXT-PAGE                                ELSPGDSP
00274      ELSE IF EIBAID =    DFHPF7  OR DFHPF19                       ELSPGDSP
00275          PERFORM DISPLAY-PREVIOUS-PAGE                            ELSPGDSP
00276      ELSE IF EIBAID =    DFHPF10 OR DFHPF22                       ELSPGDSP
00277          PERFORM DISPLAY-FIRST-PAGE                               ELSPGDSP
00278      ELSE IF EIBAID =    DFHPF11 OR DFHPF23                       ELSPGDSP
00279          PERFORM DISPLAY-LAST-PAGE                                ELSPGDSP
00280      ELSE IF EIBAID =    DFHPF2  OR DFHPF14                       ELSPGDSP
00281          PERFORM PRINT-ALL-PAGES                                  ELSPGDSP
00282      ELSE IF EIBAID =    DFHPF3  OR DFHPF15                       ELSPGDSP
00283                            OR DFHPF4  OR DFHPF16                  ELSPGDSP
00284                            OR DFHPF5  OR DFHPF17                  ELSPGDSP
00285                            OR DFHPF9  OR DFHPF21                  ELSPGDSP
00286          PERFORM INDICATE-DISPLAY-PROCESS-COMPL                   ELSPGDSP
00287      ELSE IF EIBAID =    DFHCLEAR                                 ELSPGDSP
00288          PERFORM INDICATE-DISPLAY-PROCESS-TERMI                   ELSPGDSP
00289      ELSE                                                         ELSPGDSP
00290          PERFORM INDICATE-INVALID-FUNCTION-KEY.                   ELSPGDSP
00291      EJECT                                                        ELSPGDSP
00292                                                                   ELSPGDSP
00293                                                                   ELSPGDSP
00294 ************************************************************      ELSPGDSP
00295 *                                                          *      ELSPGDSP
00296 *        DISPLAY NEXT PAGE                                 *      ELSPGDSP
00297 *                                                          *      ELSPGDSP
00298 ************************************************************      ELSPGDSP
00299  DISPLAY-NEXT-PAGE.                                               ELSPGDSP
00300      PERFORM DETERMINE-NEXT-PAGE-NUMBER.                          ELSPGDSP
00301      PERFORM READ-PAGE-FILE-RECORD.                               ELSPGDSP
00302      PERFORM DISPLAY-PAGE.                                        ELSPGDSP
00303                                                                   ELSPGDSP
00304                                                                   ELSPGDSP
00305 ************************************************************      ELSPGDSP
00306 *                                                          *      ELSPGDSP
00307 *        DETERMINE NEXT PAGE NUMBER                        *      ELSPGDSP
00308 *                                                          *      ELSPGDSP
00309 ************************************************************      ELSPGDSP
00310  DETERMINE-NEXT-PAGE-NUMBER.                                      ELSPGDSP
00311      IF SSB-PD-LAST-PAGE = ZERO                                   ELSPGDSP
00312                OR SSB-PD-CURRENT-PAGE <                           ELSPGDSP
00313          SSB-PD-LAST-PAGE                                         ELSPGDSP
00314          PERFORM INCREMENT-PAGE-NUMBER                            ELSPGDSP
00315      ELSE                                                         ELSPGDSP
00316          PERFORM RESET-PAGE-NUMBER.                               ELSPGDSP
00317                                                                   ELSPGDSP
00318                                                                   ELSPGDSP
00319 ************************************************************      ELSPGDSP
00320 *                                                          *      ELSPGDSP
00321 *        INCREMENT PAGE NUMBER                             *      ELSPGDSP
00322 *                                                          *      ELSPGDSP
00323 ************************************************************      ELSPGDSP
00324  INCREMENT-PAGE-NUMBER.                                           ELSPGDSP
00325      ADD 1 TO SSB-PD-CURRENT-PAGE.                                ELSPGDSP
00326      EJECT                                                        ELSPGDSP
00327                                                                   ELSPGDSP
00328                                                                   ELSPGDSP
00329 ************************************************************      ELSPGDSP
00330 *                                                          *      ELSPGDSP
00331 *        DISPLAY PREVIOUS PAGE                             *      ELSPGDSP
00332 *                                                          *      ELSPGDSP
00333 ************************************************************      ELSPGDSP
00334  DISPLAY-PREVIOUS-PAGE.                                           ELSPGDSP
00335      PERFORM DETERMINE-PREVIOUS-PAGE-NUMBER.                      ELSPGDSP
00336      PERFORM READ-PAGE-FILE-RECORD.                               ELSPGDSP
00337      PERFORM DISPLAY-PAGE.                                        ELSPGDSP
00338                                                                   ELSPGDSP
00339                                                                   ELSPGDSP
00340 ************************************************************      ELSPGDSP
00341 *                                                          *      ELSPGDSP
00342 *        DETERMINE PREVIOUS PAGE NUMBER                    *      ELSPGDSP
00343 *                                                          *      ELSPGDSP
00344 ************************************************************      ELSPGDSP
00345  DETERMINE-PREVIOUS-PAGE-NUMBER.                                  ELSPGDSP
00346      IF SSB-PD-CURRENT-PAGE > 1                                   ELSPGDSP
00347          PERFORM DECREMENT-PAGE-NUMBER                            ELSPGDSP
00348      ELSE                                                         ELSPGDSP
00349          PERFORM FIND-LAST-PAGE.                                  ELSPGDSP
00350                                                                   ELSPGDSP
00351                                                                   ELSPGDSP
00352 ************************************************************      ELSPGDSP
00353 *                                                          *      ELSPGDSP
00354 *        DECREMENT PAGE NUMBER                             *      ELSPGDSP
00355 *                                                          *      ELSPGDSP
00356 ************************************************************      ELSPGDSP
00357  DECREMENT-PAGE-NUMBER.                                           ELSPGDSP
00358      SUBTRACT 1 FROM SSB-PD-CURRENT-PAGE.                         ELSPGDSP
00359      EJECT                                                        ELSPGDSP
00360                                                                   ELSPGDSP
00361                                                                   ELSPGDSP
00362 ************************************************************      ELSPGDSP
00363 *                                                          *      ELSPGDSP
00364 *        DISPLAY FIRST PAGE                                *      ELSPGDSP
00365 *                                                          *      ELSPGDSP
00366 ************************************************************      ELSPGDSP
00367  DISPLAY-FIRST-PAGE.                                              ELSPGDSP
00368      PERFORM RESET-PAGE-NUMBER.                                   ELSPGDSP
00369      PERFORM READ-PAGE-FILE-RECORD.                               ELSPGDSP
00370      PERFORM DISPLAY-PAGE.                                        ELSPGDSP
00371                                                                   ELSPGDSP
00372                                                                   ELSPGDSP
00373 ************************************************************      ELSPGDSP
00374 *                                                          *      ELSPGDSP
00375 *        RESET PAGE NUMBER                                 *      ELSPGDSP
00376 *                                                          *      ELSPGDSP
00377 ************************************************************      ELSPGDSP
00378  RESET-PAGE-NUMBER.                                               ELSPGDSP
00379      MOVE +1 TO SSB-PD-CURRENT-PAGE.                              ELSPGDSP
00380      EJECT                                                        ELSPGDSP
00381                                                                   ELSPGDSP
00382                                                                   ELSPGDSP
00383 ************************************************************      ELSPGDSP
00384 *                                                          *      ELSPGDSP
00385 *        DISPLAY LAST PAGE                                 *      ELSPGDSP
00386 *                                                          *      ELSPGDSP
00387 ************************************************************      ELSPGDSP
00388  DISPLAY-LAST-PAGE.                                               ELSPGDSP
00389      PERFORM FIND-LAST-PAGE.                                      ELSPGDSP
00390      PERFORM READ-PAGE-FILE-RECORD.                               ELSPGDSP
00391      PERFORM DISPLAY-PAGE.                                        ELSPGDSP
00392                                                                   ELSPGDSP
00393                                                                   ELSPGDSP
00394 ************************************************************      ELSPGDSP
00395 *                                                          *      ELSPGDSP
00396 *        FIND LAST PAGE                                    *      ELSPGDSP
00397 *                                                          *      ELSPGDSP
00398 ************************************************************      ELSPGDSP
00399  FIND-LAST-PAGE.                                                  ELSPGDSP
00400      IF SSB-PD-LAST-PAGE > 0                                      ELSPGDSP
00401          PERFORM SELECT-KNOWN-LAST-PAGE                           ELSPGDSP
00402      ELSE                                                         ELSPGDSP
00403          PERFORM LOCATE-LAST-PAGE.                                ELSPGDSP
00404                                                                   ELSPGDSP
00405                                                                   ELSPGDSP
00406 ************************************************************      ELSPGDSP
00407 *                                                          *      ELSPGDSP
00408 *        SELECT KNOWN LAST PAGE                            *      ELSPGDSP
00409 *                                                          *      ELSPGDSP
00410 ************************************************************      ELSPGDSP
00411  SELECT-KNOWN-LAST-PAGE.                                          ELSPGDSP
00412      MOVE SSB-PD-LAST-PAGE TO SSB-PD-CURRENT-PAGE.                ELSPGDSP
00413      EJECT                                                        ELSPGDSP
00414                                                                   ELSPGDSP
00415                                                                   ELSPGDSP
00416 ************************************************************      ELSPGDSP
00417 *                                                          *      ELSPGDSP
00418 *        LOCATE LAST PAGE                                  *      ELSPGDSP
00419 *                                                          *      ELSPGDSP
00420 ************************************************************      ELSPGDSP
00421  LOCATE-LAST-PAGE.                                                ELSPGDSP
00422      PERFORM READ-PAGE-FILE-RECORD                                ELSPGDSP
00423          VARYING SSB-PD-CURRENT-PAGE FROM SSB-PD-CURRENT-PAGE     ELSPGDSP
00424              BY 1                                                 ELSPGDSP
00425                   UNTIL SSB-PD-LAST-PAGE > ZERO.                  ELSPGDSP
00426      MOVE SSB-PD-LAST-PAGE TO SSB-PD-CURRENT-PAGE.                ELSPGDSP
00427      EJECT                                                        ELSPGDSP
00428                                                                   ELSPGDSP
00429                                                                   ELSPGDSP
00430 ************************************************************      ELSPGDSP
00431 *                                                          *      ELSPGDSP
00432 *        PRINT ALL PAGES                                   *      ELSPGDSP
00433 *                                                          *      ELSPGDSP
00434 ************************************************************      ELSPGDSP
00435  PRINT-ALL-PAGES.                                                 ELSPGDSP
00436      PERFORM OBTAIN-ASSIGNED-PRINTER-TERMIN.                      ELSPGDSP
00437      IF WS-PRTR-ID IS NOT EQUAL TO SPACES OR ZEROS OR             ELSPGDSP
00438          LOW-VALUES                                               ELSPGDSP
00439          PERFORM COMPLETE-PRINT-ALL-PAGES-REQUE                   ELSPGDSP
00440      ELSE                                                         ELSPGDSP
00441          PERFORM DISPLAY-NO-PRINTER-ASSIGNED-ME.                  ELSPGDSP
00442                                                                   ELSPGDSP
00443                                                                   ELSPGDSP
00444 ************************************************************      ELSPGDSP
00445 *                                                          *      ELSPGDSP
00446 *        DISPLAY NO PRINTER ASSIGNED MESSAGE               *      ELSPGDSP
00447 *                                                          *      ELSPGDSP
00448 ************************************************************      ELSPGDSP
00449  DISPLAY-NO-PRINTER-ASSIGNED-ME.                                  ELSPGDSP
00450      MOVE WS-HC-REQ-PRTR-MSG    TO ERRMSGO.                       ELSPGDSP
00451      PERFORM SEND-ERROR-MAP-AND-DATA.                             ELSPGDSP
00452                                                                   ELSPGDSP
00453                                                                   ELSPGDSP
00454 ************************************************************      ELSPGDSP
00455 *                                                          *      ELSPGDSP
00456 *        OBTAIN ASSIGNED PRINTER TERMINAL ID               *      ELSPGDSP
00457 *                                                          *      ELSPGDSP
00458 ************************************************************      ELSPGDSP
00459  OBTAIN-ASSIGNED-PRINTER-TERMIN.                                  ELSPGDSP
00460      PERFORM ESTABLISH-ADDRESSING-TO-TERMIN.                      ELSPGDSP
00461      IF TUA0PRID IS NOT EQUAL TO SPACES OR ZEROS OR               ELSPGDSP
00462          LOW-VALUES                                               ELSPGDSP
00463          PERFORM SAVE-ASSIGNED-PRINTER-TERMINAL                   ELSPGDSP
00464      ELSE                                                         ELSPGDSP
00465          PERFORM INDICATE-NO-PRINTER-ASSIGNED-F.                  ELSPGDSP
00466                                                                   ELSPGDSP
00467                                                                   ELSPGDSP
00468 ************************************************************      ELSPGDSP
00469 *                                                          *      ELSPGDSP
00470 *        ESTABLISH ADDRESSING TO TERMINAL CONTROL TABLE USE*      ELSPGDSP
00471 *                                                          *      ELSPGDSP
00472 ************************************************************      ELSPGDSP
00473  ESTABLISH-ADDRESSING-TO-TERMIN.                                  ELSPGDSP
00474      EXEC CICS ADDRESS TCTUA(ADDRESS OF TUACOBOL)                 ELSPGDSP
00475          END-EXEC.                                                ELSPGDSP
00476                                                                   ELSPGDSP
00477                                                                   ELSPGDSP
00478 ************************************************************      ELSPGDSP
00479 *                                                          *      ELSPGDSP
00480 *        SAVE ASSIGNED PRINTER TERMINAL ID                 *      ELSPGDSP
00481 *                                                          *      ELSPGDSP
00482 ************************************************************      ELSPGDSP
00483  SAVE-ASSIGNED-PRINTER-TERMINAL.                                  ELSPGDSP
00484      MOVE TUA0PRID TO WS-PRTR-ID.                                 ELSPGDSP
00485                                                                   ELSPGDSP
00486                                                                   ELSPGDSP
00487 ************************************************************      ELSPGDSP
00488 *                                                          *      ELSPGDSP
00489 *        INDICATE NO PRINTER ASSIGNED FOR THIS TERMINAL    *      ELSPGDSP
00490 *                                                          *      ELSPGDSP
00491 ************************************************************      ELSPGDSP
00492  INDICATE-NO-PRINTER-ASSIGNED-F.                                  ELSPGDSP
00493      MOVE SPACES TO WS-PRTR-ID.                                   ELSPGDSP
00494      EJECT                                                        ELSPGDSP
00495                                                                   ELSPGDSP
00496                                                                   ELSPGDSP
00497 ************************************************************      ELSPGDSP
00498 *                                                          *      ELSPGDSP
00499 *        COMPLETE PRINT ALL PAGES REQUEST                  *      ELSPGDSP
00500 *                                                          *      ELSPGDSP
00501 ************************************************************      ELSPGDSP
00502  COMPLETE-PRINT-ALL-PAGES-REQUE.                                  ELSPGDSP
00503      PERFORM DISPLAY-HARDCOPY-REQUEST-IN-PR.                      ELSPGDSP
00504      PERFORM BUILD-PRINT-PARAMETER-LIST.                          ELSPGDSP
00505      PERFORM CALL-PRINT-PROGRAM.                                  ELSPGDSP
00506      PERFORM INDICATE-PRINT-RETURN-STATUS.                        ELSPGDSP
00507                                                                   ELSPGDSP
00508                                                                   ELSPGDSP
00509 ************************************************************      ELSPGDSP
00510 *                                                          *      ELSPGDSP
00511 *        DISPLAY HARDCOPY REQUEST IN PROGRESS MESSAGE      *      ELSPGDSP
00512 *                                                          *      ELSPGDSP
00513 ************************************************************      ELSPGDSP
00514  DISPLAY-HARDCOPY-REQUEST-IN-PR.                                  ELSPGDSP
00515      MOVE WS-HC-REQ-IN-PROC-MSG TO ERRMSGO.                       ELSPGDSP
00516      PERFORM SEND-ERROR-MAP-AND-DATA.                             ELSPGDSP
00517                                                                   ELSPGDSP
00518                                                                   ELSPGDSP
00519 ************************************************************      ELSPGDSP
00520 *                                                          *      ELSPGDSP
00521 *        BUILD PRINT PARAMETER LIST                        *      ELSPGDSP
00522 *                                                          *      ELSPGDSP
00523 ************************************************************      ELSPGDSP
00524  BUILD-PRINT-PARAMETER-LIST.                                      ELSPGDSP
00525      SET IOP-PRINT-TSQ TO TRUE.                                   ELSPGDSP
00526      MOVE ZEROS  TO  IOP-TSQ-FIRST-PRINT                          ELSPGDSP
00527                      IOP-TSQ-LAST-PRINT.                          ELSPGDSP
00528      MOVE WS-PRTR-ID   TO IOP-TSQ-PRINTER-ID.                     ELSPGDSP
00529                                                                   ELSPGDSP
00530                                                                   ELSPGDSP
00531 ************************************************************      ELSPGDSP
00532 *                                                          *      ELSPGDSP
00533 *        CALL PRINT PROGRAM                                *      ELSPGDSP
00534 *                                                          *      ELSPGDSP
00535 ************************************************************      ELSPGDSP
00536  CALL-PRINT-PROGRAM.                                              ELSPGDSP
00537      EXEC CICS LINK                                               ELSPGDSP
00538                PROGRAM('ELUIOPGM')                                ELSPGDSP
00539                COMMAREA(DFHCOMMAREA)                              ELSPGDSP
00540          END-EXEC.                                                ELSPGDSP
00541                                                                   ELSPGDSP
00542                                                                   ELSPGDSP
00543 ************************************************************      ELSPGDSP
00544 *                                                          *      ELSPGDSP
00545 *        INDICATE PRINT RETURN STATUS                      *      ELSPGDSP
00546 *                                                          *      ELSPGDSP
00547 ************************************************************      ELSPGDSP
00548  INDICATE-PRINT-RETURN-STATUS.                                    ELSPGDSP
00549      MOVE IOP-TSQ-PRINT-MSG TO ERRMSGO.                           ELSPGDSP
00550      PERFORM SEND-ERROR-MAP-AND-DATA.                             ELSPGDSP
00551      EJECT                                                        ELSPGDSP
00552                                                                   ELSPGDSP
00553                                                                   ELSPGDSP
00554 ************************************************************      ELSPGDSP
00555 *                                                          *      ELSPGDSP
00556 *        INDICATE DISPLAY PROCESS COMPLETED                *      ELSPGDSP
00557 *                                                          *      ELSPGDSP
00558 ************************************************************      ELSPGDSP
00559  INDICATE-DISPLAY-PROCESS-COMPL.                                  ELSPGDSP
00560      SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.              ELSPGDSP
00561      EJECT                                                        ELSPGDSP
00562                                                                   ELSPGDSP
00563                                                                   ELSPGDSP
00564 ************************************************************      ELSPGDSP
00565 *                                                          *      ELSPGDSP
00566 *        INDICATE DISPLAY PROCESS TERMINATED               *      ELSPGDSP
00567 *                                                          *      ELSPGDSP
00568 ************************************************************      ELSPGDSP
00569  INDICATE-DISPLAY-PROCESS-TERMI.                                  ELSPGDSP
00570      SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.              ELSPGDSP
00571      SET SSB-TERM-TASK TO TRUE.                                   ELSPGDSP
00572      EJECT                                                        ELSPGDSP
00573                                                                   ELSPGDSP
00574                                                                   ELSPGDSP
00575 ************************************************************      ELSPGDSP
00576 *                                                          *      ELSPGDSP
00577 *        INDICATE INVALID FUNCTION KEY                     *      ELSPGDSP
00578 *                                                          *      ELSPGDSP
00579 ************************************************************      ELSPGDSP
00580  INDICATE-INVALID-FUNCTION-KEY.                                   ELSPGDSP
00581      SET CIA-AB-PGM-LOGIC TO TRUE.                                ELSPGDSP
00582      EXEC CICS ABEND                                              ELSPGDSP
00583                ABCODE(CIA-ABCODE)                                 ELSPGDSP
00584           END-EXEC.                                               ELSPGDSP
00585      EJECT                                                        ELSPGDSP
00586                                                                   ELSPGDSP
00587                                                                   ELSPGDSP
00588 ************************************************************      ELSPGDSP
00589 *                                                          *      ELSPGDSP
00590 *        DISPLAY PAGE                                      *      ELSPGDSP
00591 *                                                          *      ELSPGDSP
00592 ************************************************************      ELSPGDSP
00593  DISPLAY-PAGE.                                                    ELSPGDSP
00594      PERFORM BUILD-PAGE.                                          ELSPGDSP
00595      PERFORM SEND-PAGE-DATA.                                      ELSPGDSP
00596      PERFORM SEND-STANDARD-PFK-USAGE-MESSAG.                      ELSPGDSP
00597                                                                   ELSPGDSP
00598                                                                   ELSPGDSP
00599 ************************************************************      ELSPGDSP
00600 *                                                          *      ELSPGDSP
00601 *        BUILD PAGE                                        *      ELSPGDSP
00602 *                                                          *      ELSPGDSP
00603 ************************************************************      ELSPGDSP
00604  BUILD-PAGE.                                                      ELSPGDSP
00605      PERFORM BUILD-PAGE-HEADING.                                  ELSPGDSP
00606      PERFORM BUILD-PAGE-BODY.                                     ELSPGDSP
00607                                                                   ELSPGDSP
00608                                                                   ELSPGDSP
00609 ************************************************************      ELSPGDSP
00610 *                                                          *      ELSPGDSP
00611 *        BUILD PAGE HEADING                                *      ELSPGDSP
00612 *                                                          *      ELSPGDSP
00613 ************************************************************      ELSPGDSP
00614  BUILD-PAGE-HEADING.                                              ELSPGDSP
00615      MOVE EIBTRNID    TO MAPID3O.                                 ELSPGDSP
00616      MOVE PQ-PAGE-NO  TO PGNBRO.                                  ELSPGDSP
00617      MOVE SPACES TO DATE3O.                                       ELSPGDSP
00618      MOVE DFHBMASB TO MAPID3A                                     ELSPGDSP
00619                       PGNBRA                                      ELSPGDSP
00620                       PGMSGA                                      ELSPGDSP
00621                       DATE3A.                                     ELSPGDSP
00622      PERFORM FILL-IN-PAGE-MESSAGE.                                ELSPGDSP
00623      EJECT                                                        ELSPGDSP
00624                                                                   ELSPGDSP
00625                                                                   ELSPGDSP
00626 ************************************************************      ELSPGDSP
00627 *                                                          *      ELSPGDSP
00628 *        FILL IN PAGE MESSAGE                              *      ELSPGDSP
00629 *                                                          *      ELSPGDSP
00630 ************************************************************      ELSPGDSP
00631  FILL-IN-PAGE-MESSAGE.                                            ELSPGDSP
00632      IF PQ-LAST-RECORD                                            ELSPGDSP
00633          PERFORM INDICATE-LAST-PAGE                               ELSPGDSP
00634      ELSE                                                         ELSPGDSP
00635          PERFORM INDICATE-MORE-PAGES.                             ELSPGDSP
00636                                                                   ELSPGDSP
00637                                                                   ELSPGDSP
00638 ************************************************************      ELSPGDSP
00639 *                                                          *      ELSPGDSP
00640 *        INDICATE LAST PAGE                                *      ELSPGDSP
00641 *                                                          *      ELSPGDSP
00642 ************************************************************      ELSPGDSP
00643  INDICATE-LAST-PAGE.                                              ELSPGDSP
00644      MOVE 'LAST' TO PGMSGO.                                       ELSPGDSP
00645                                                                   ELSPGDSP
00646                                                                   ELSPGDSP
00647 ************************************************************      ELSPGDSP
00648 *                                                          *      ELSPGDSP
00649 *        INDICATE MORE PAGES                               *      ELSPGDSP
00650 *                                                          *      ELSPGDSP
00651 ************************************************************      ELSPGDSP
00652  INDICATE-MORE-PAGES.                                             ELSPGDSP
00653      MOVE 'MORE' TO PGMSGO.                                       ELSPGDSP
00654                                                                   ELSPGDSP
00655                                                                   ELSPGDSP
00656 ************************************************************      ELSPGDSP
00657 *                                                          *      ELSPGDSP
00658 *        BUILD PAGE BODY                                   *      ELSPGDSP
00659 *                                                          *      ELSPGDSP
00660 ************************************************************      ELSPGDSP
00661  BUILD-PAGE-BODY.                                                 ELSPGDSP
00662      PERFORM MOVE-OR-CLEAR-PAGE-BODY-LINES                        ELSPGDSP
00663          VARYING WS-PG-SUBSCR                                     ELSPGDSP
00664             FROM 1                                                ELSPGDSP
00665               BY 1                                                ELSPGDSP
00666            UNTIL WS-PG-SUBSCR IS GREATER THAN 22.                 ELSPGDSP
00667      EJECT                                                        ELSPGDSP
00668                                                                   ELSPGDSP
00669                                                                   ELSPGDSP
00670 ************************************************************      ELSPGDSP
00671 *                                                          *      ELSPGDSP
00672 *        MOVE OR CLEAR PAGE BODY LINES                     *      ELSPGDSP
00673 *                                                          *      ELSPGDSP
00674 ************************************************************      ELSPGDSP
00675  MOVE-OR-CLEAR-PAGE-BODY-LINES.                                   ELSPGDSP
00676      PERFORM SET-PAGE-BODY-LINE-ATTRIBUTE.                        ELSPGDSP
00677      IF WS-PG-SUBSCR IS NOT GREATER THAN                          ELSPGDSP
00678          PQ-OCCURRENCE-COUNT                                      ELSPGDSP
00679          PERFORM MOVE-PAGE-BODY-LINE                              ELSPGDSP
00680      ELSE                                                         ELSPGDSP
00681          PERFORM CLEAR-PAGE-BODY-LINE.                            ELSPGDSP
00682                                                                   ELSPGDSP
00683                                                                   ELSPGDSP
00684 ************************************************************      ELSPGDSP
00685 *                                                          *      ELSPGDSP
00686 *        SET PAGE BODY LINE ATTRIBUTE                      *      ELSPGDSP
00687 *                                                          *      ELSPGDSP
00688 ************************************************************      ELSPGDSP
00689  SET-PAGE-BODY-LINE-ATTRIBUTE.                                    ELSPGDSP
00690      MOVE DFHBMASK TO TEXTA (WS-PG-SUBSCR).                       ELSPGDSP
00691                                                                   ELSPGDSP
00692                                                                   ELSPGDSP
00693 ************************************************************      ELSPGDSP
00694 *                                                          *      ELSPGDSP
00695 *        MOVE PAGE BODY LINE                               *      ELSPGDSP
00696 *                                                          *      ELSPGDSP
00697 ************************************************************      ELSPGDSP
00698  MOVE-PAGE-BODY-LINE.                                             ELSPGDSP
00699      MOVE PQ-PAGE-LINE (WS-PG-SUBSCR) TO TEXTO                    ELSPGDSP
00700          (WS-PG-SUBSCR).                                          ELSPGDSP
00701                                                                   ELSPGDSP
00702                                                                   ELSPGDSP
00703 ************************************************************      ELSPGDSP
00704 *                                                          *      ELSPGDSP
00705 *        CLEAR PAGE BODY LINE                              *      ELSPGDSP
00706 *                                                          *      ELSPGDSP
00707 ************************************************************      ELSPGDSP
00708  CLEAR-PAGE-BODY-LINE.                                            ELSPGDSP
00709      MOVE SPACES TO TEXTO (WS-PG-SUBSCR).                         ELSPGDSP
00710      EJECT                                                        ELSPGDSP
00711                                                                   ELSPGDSP
00712                                                                   ELSPGDSP
00713 ************************************************************      ELSPGDSP
00714 *                                                          *      ELSPGDSP
00715 *        SEND STANDARD PFK USAGE MESSAGE                   *      ELSPGDSP
00716 *                                                          *      ELSPGDSP
00717 ************************************************************      ELSPGDSP
00718  SEND-STANDARD-PFK-USAGE-MESSAG.                                  ELSPGDSP
00719      MOVE WS-PFK-MSG TO ERRMSGO.                                  ELSPGDSP
00720      PERFORM SEND-ERROR-MAP-AND-DATA.                             ELSPGDSP
00721      EJECT                                                        ELSPGDSP
00722                                                                   ELSPGDSP
00723                                                                   ELSPGDSP
00724 ************************************************************      ELSPGDSP
00725 *                                                          *      ELSPGDSP
00726 *        SEND PAGE MAP                                     *      ELSPGDSP
00727 *                                                          *      ELSPGDSP
00728 ************************************************************      ELSPGDSP
00729  SEND-PAGE-MAP.                                                   ELSPGDSP
00730      EXEC CICS SEND MAP    ('EL03MAP')                            ELSPGDSP
00731                     MAPSET ('EL03SET')                            ELSPGDSP
00732                     MAPONLY                                       ELSPGDSP
00733                     CURSOR(1)                                     ELSPGDSP
00734                     ERASE                                         ELSPGDSP
00735                     END-EXEC.                                     ELSPGDSP
00736      EJECT                                                        ELSPGDSP
00737                                                                   ELSPGDSP
00738                                                                   ELSPGDSP
00739 ************************************************************      ELSPGDSP
00740 *                                                          *      ELSPGDSP
00741 *        SEND PAGE DATA                                    *      ELSPGDSP
00742 *                                                          *      ELSPGDSP
00743 ************************************************************      ELSPGDSP
00744  SEND-PAGE-DATA.                                                  ELSPGDSP
00745      EXEC CICS SEND MAP     ('EL03MAP')                           ELSPGDSP
00746                     MAPSET  ('EL03SET')                           ELSPGDSP
00747                     FROM    (EL03MAPO)                            ELSPGDSP
00748                     DATAONLY                                      ELSPGDSP
00749                     CURSOR  (1)                                   ELSPGDSP
00750                     FREEKB                                        ELSPGDSP
00751                     END-EXEC.                                     ELSPGDSP
00752                                                                   ELSPGDSP
00753                                                                   ELSPGDSP
00754 ************************************************************      ELSPGDSP
00755 *                                                          *      ELSPGDSP
00756 *        SEND ERROR MAP AND DATA                           *      ELSPGDSP
00757 *                                                          *      ELSPGDSP
00758 ************************************************************      ELSPGDSP
00759  SEND-ERROR-MAP-AND-DATA.                                         ELSPGDSP
00760      MOVE DFHBMASB TO ERRMSGA.                                    ELSPGDSP
00761      EXEC CICS SEND MAP      ('EL00MAP')                          ELSPGDSP
00762                     MAPSET   ('EL00SET')                          ELSPGDSP
00763                     FROM     (EL00MAPO)                           ELSPGDSP
00764                     CURSOR   (1)                                  ELSPGDSP
00765                     END-EXEC.                                     ELSPGDSP
00766      EJECT                                                        ELSPGDSP
00767                                                                   ELSPGDSP
00768                                                                   ELSPGDSP
00769 ************************************************************      ELSPGDSP
00770 *                                                          *      ELSPGDSP
00771 *        READ PAGE FILE RECORD                             *      ELSPGDSP
00772 *                                                          *      ELSPGDSP
00773 ************************************************************      ELSPGDSP
00774  READ-PAGE-FILE-RECORD.                                           ELSPGDSP
00775      SET IOP-RD  TO TRUE.                                         ELSPGDSP
00776      MOVE SSB-PD-CURRENT-PAGE TO IOP-TSQ-ITEM-NBR.                ELSPGDSP
00777      EXEC CICS LINK                                               ELSPGDSP
00778                PROGRAM('ELUIOPGM')                                ELSPGDSP
00779                COMMAREA(DFHCOMMAREA)                              ELSPGDSP
00780           END-EXEC.                                               ELSPGDSP
00781      SET ADDRESS OF PQ-PAGE-QUEUE TO IOP-REC-PTR.                 ELSPGDSP
00782      PERFORM DETERMINE-IF-THIS-IS-THE-LASTX.                      ELSPGDSP
00783                                                                   ELSPGDSP
00784                                                                   ELSPGDSP
00785 ************************************************************      ELSPGDSP
00786 *                                                          *      ELSPGDSP
00787 *        DETERMINE IF THIS IS THE LAST PAGE                *      ELSPGDSP
00788 *                                                          *      ELSPGDSP
00789 ************************************************************      ELSPGDSP
00790  DETERMINE-IF-THIS-IS-THE-LASTX.                                  ELSPGDSP
00791      IF PQ-LAST-RECORD                                            ELSPGDSP
00792          PERFORM SET-LAST-PAGE-VALUE.                             ELSPGDSP
00793      IF IOP-RC-NOTFND                                             ELSPGDSP
00794          PERFORM SIGNAL-IMPROPER-PAGE-FILE.                       ELSPGDSP
00795                                                                   ELSPGDSP
00796                                                                   ELSPGDSP
00797 ************************************************************      ELSPGDSP
00798 *                                                          *      ELSPGDSP
00799 *        SET LAST PAGE VALUE                               *      ELSPGDSP
00800 *                                                          *      ELSPGDSP
00801 ************************************************************      ELSPGDSP
00802  SET-LAST-PAGE-VALUE.                                             ELSPGDSP
00803      MOVE SSB-PD-CURRENT-PAGE TO SSB-PD-LAST-PAGE.                ELSPGDSP
00804                                                                   ELSPGDSP
00805                                                                   ELSPGDSP
00806 ************************************************************      ELSPGDSP
00807 *                                                          *      ELSPGDSP
00808 *        SIGNAL IMPROPER PAGE FILE                         *      ELSPGDSP
00809 *                                                          *      ELSPGDSP
00810 ************************************************************      ELSPGDSP
00811  SIGNAL-IMPROPER-PAGE-FILE.                                       ELSPGDSP
00812      SET CIA-AB-UNDEF TO TRUE.                                    ELSPGDSP
00813      EXEC CICS ABEND                                              ELSPGDSP
00814                ABCODE(CIA-ABCODE)                                 ELSPGDSP
00815           END-EXEC.                                               ELSPGDSP
00816                                                                   ELSPGDSP
00817                                                                   ELSPGDSP
00818 ************************************************************      ELSPGDSP
00819 *                                                          *      ELSPGDSP
00820 *        DISPLAY OUTPUT PAGES.TERMINATE                    *      ELSPGDSP
00821 *                                                          *      ELSPGDSP
00822 ************************************************************      ELSPGDSP
00823  DISPLAY-OUTPUT-PAGES-TERMINATE.                                  ELSPGDSP
00824      EXEC CICS RETURN                                             ELSPGDSP
00825           END-EXEC.                                               ELSPGDSP
00826      EJECT                                                        ELSPGDSP
00827  GOBACK-PARAGRAPH.                                                ELSPGDSP
00828 ************************************************************      ELSPGDSP
00829 *                                                          *      ELSPGDSP
00830 *                         STOP RUN                         *      ELSPGDSP
00831 *                                                          *      ELSPGDSP
00832 ************************************************************      ELSPGDSP
00833      GOBACK.                                                      ELSPGDSP
