00001  IDENTIFICATION DIVISION.                                         12/09/02
00002  PROGRAM-ID.         GC0220.                                      GC0220  
00003  AUTHOR.             DELORES FRY.                                    LV002
00004  INSTALLATION.       HCSC.                                        GC0220  
00005  DATE-WRITTEN.       JANUARY 1988.                                GC0220  
00006  DATE-COMPILED.                                                   GC0220  
00007 ******************************************************************GC0220  
00008 *                                                                *GC0220  
00009 *          GENERIC CONTRACT PROCESSING SYSTEM  (GCPS)            *GC0220  
00010 *                                                                *GC0220  
00011 *                                                                *GC0220  
00012 *     GC0220   - UPDATES THE VSAM BENEFIT PROVISION FILE WITH    *GC0220  
00013 *                NEW BENEFIT PROVISION RECORDS.                  *GC0220  
00014 *                                                                *GC0220  
00015 *                                                                *GC0220  
00016 *  INPUT FILE:   INPUT-SLOT-UPDATE-FILE            -GC0220A      *GC0220  
00017 *                                                  (SEQUENTIAL)  *GC0220  
00018 *                                                                *GC0220  
00019 *   I/O  FILE:   BENEFIT PROVISION FILE  -  TSGVSAM1             *GC0220  
00020 *                                           (VSAM KEY SEQUENCE)  *GC0220  
00021 *                                                                *GC0220  
00022 *                                                                *GC0220  
00023 ******************************************************************GC0220  
00024 ******************************************************************GC0220  
00025 ******************************************************************GC0220  
00026 *                                                                *GC0220  
00027 *       ***-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0220  
00028 *       *-*         U P D A T E   H I S T O R Y         *-*      *GC0220  
00029 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0220  
00030 *                                                                *GC0220  
00031 *                                                                *GC0220  
00032 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION--------------- *GC0220  
00033 *                                                                *GC0220  
00034 *   D1009    1/19/88   FRY    CREATED THIS PROGRAM......         *GC0220  
00035 *                                                                *GC0220  
00036 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC0220  
00037 *                                                                *GC0220  
00038 *                                                                *GC0220  
00039 ******************************************************************GC0220  
00040 ******************************************************************GC0220  
00041  ENVIRONMENT DIVISION.                                            GC0220  
00042                                                                   GC0220  
00043  CONFIGURATION SECTION.                                           GC0220  
00044  SOURCE-COMPUTER.  IBM-370.                                       GC0220  
00045  OBJECT-COMPUTER.  IBM-370.                                       GC0220  
00046                                                                   GC0220  
00047  INPUT-OUTPUT SECTION.                                            GC0220  
00048                                                                   GC0220  
00049  FILE-CONTROL.                                                    GC0220  
00050                                                                   GC0220  
00051      SELECT   INPUT-SLOT-UPDATE-FILE    ASSIGN TO   UT-S-GC0220A. GC0220  
00052                                                                   GC0220  
00053                                                                   GC0220  
00054  DATA DIVISION.                                                   GC0220  
00055  FILE SECTION.                                                    GC0220  
00056                                                                   GC0220  
00057  FD  INPUT-SLOT-UPDATE-FILE                                       GC0220  
00058      LABEL RECORDS ARE STANDARD                                   GC0220  
00059      RECORDING MODE IS V                                          GC0220  
00060      BLOCK CONTAINS  0  RECORDS.                                  GC0220  
00061                                                                   GC0220  
00062  01  INPUT-SLOT-UPDATE-RECORD.                                    GC0220  
00063 *    05  INPUT-WORK-KEY                    PIC  X(64).            GC0220  
00064          COPY GCWRKDCC.                                           GC0220  
00065      05  INPUT-BENEFIT-RECORD.                                    GC0220  
00066          10  INPUT-BENEFIT-RECORD-KEY.                            GC0220  
00067              15  INPUT-BENEFIT-ID          PIC  X(06).            GC0220  
00068              15  INPUT-BENEFIT-SLOT        PIC S9(07)    COMP-3.  GC0220  
00069              15  FILLER                    PIC  X(385).           GC0220  
00070 /                                                                 GC0220  
00071                                                                   GC0220  
00072  WORKING-STORAGE SECTION.                                         GC0220  
00073                                                                   GC0220  
00074  01  WS-PROGRAM-ID                 PIC  X(27)  VALUE              GC0220  
00075                                     '* GC0220 WORKING STORAGE *'. GC0220  
00076                                                                   GC0220  
00077  01  WS-HOLD-AREAS.                                               GC0220  
00078      05  FILLER                    PIC  X(18)  VALUE              GC0220  
00079                                    '**  ABEND CODE  **'.          GC0220  
00080      05  WS-ABEND-CODE             PIC  9(04)  VALUE 0    COMP.   GC0220  
00081      05  FILLER                    PIC  X(19)  VALUE              GC0220  
00082                                    '**  BENEFIT KEY  **'.         GC0220  
00083      05  WS-DISPLAY-KEY.                                          GC0220  
00084          10  WS-BENEFIT-ID          PIC  X(06)  VALUE SPACES.     GC0220  
00085          10  WS-BENEFIT-SLOT        PIC  ZZZZZZ9.                 GC0220  
00086                                                                   GC0220  
00087                                                                   GC0220  
00088  01  WS-COUNT-AREAS.                                              GC0220  
00089      05  FILLER                    PIC  X(21)  VALUE              GC0220  
00090                                    '**  RECORD COUNTS  **'.       GC0220  
00091      05  WS-RECORDS-READ           PIC  9(07)  VALUE ZEROES.      GC0220  
00092      05  WS-RECORDS-ADDED          PIC  9(07)  VALUE ZEROES.      GC0220  
00093                                                                   GC0220  
00094  COPY HSCDATES.                                                   GC0220  
00095                                                                   GC0220  
00096  01  WS-DATE-AREA.                                                GC0220  
00097      05  WS-JUL-DATE               PIC  9(05)  VALUE ZEROS.       GC0220  
00098      05  FILLER REDEFINES WS-JUL-DATE.                            GC0220  
00099          10  WS-YY                 PIC  9(02).                    GC0220  
00100          10  WS-DDD                PIC  9(03).                    GC0220  
00101      05  WS-CURR-JUL-DATE          PIC S9(05) COMP-3 VALUE ZEROS. GC0220  
00102                                                                   GC0220  
00103  01  WS-SWITCHES.                                                 GC0220  
00104      05  FILLER                    PIC  X(16)  VALUE              GC0220  
00105                                    '*** SWITCHES ***'.            GC0220  
00106      05  WS-END-OF-FILE-SW         PIC  X(01)  VALUE '0'.         GC0220  
00107          88  WS-END-OF-FILE-SWITCH-ON          VALUE '1'.         GC0220  
00108 /                                                                 GC0220  
00109  01  WS-GCPS-RECORD-LENGTHS.                                      GC0220  
00110      05  FILLER                    PIC  X(27)  VALUE              GC0220  
00111                                    '*** GCPS RECORD LENGTHS ***'. GC0220  
00112      COPY GCCDRLEN.                                               GC0220  
00113 /                                                                 GC0220  
00114                                                                   GC0220  
00115 ******************************************************************GC0220  
00116 **                                                                GC0220  
00117 **     PARAMETERS FOR THE BENEFIT PROVISION FILE     TSGVSAM1     GC0220  
00118 **                                                                GC0220  
00119 ******************************************************************GC0220  
00120  01  FILLER                            PIC  X(32)  VALUE          GC0220  
00121                                '***  BENEFIT PROVISION FILE  ***'.GC0220  
00122                                                                   GC0220  
00123  01  PARM-SET.                                                    GC0220  
00124      05  SET-VSAM-RDW.                                            GC0220  
00125         10 SET-VSAM-RECORD-LENGTH      PIC 9(04)     COMP.        GC0220  
00126         10 SET-VSAM-FEEDBACK-CODE      PIC 9(04)     COMP.        GC0220  
00127      05  SET-VSAM-VALUE                PIC 9(08)     COMP.        GC0220  
00128                                                                   GC0220  
00129                                                                   GC0220  
00130  01  PARM-BEN-ONE-A.                                              GC0220  
00131      05  RESERVED-FLDS                 PIC  9(08) VALUE 0   COMP. GC0220  
00132      05  RESERVED-ONE     REDEFINES     RESERVED-FLDS.            GC0220  
00133         10  VSAM-REQUEST-TYPE          PIC  X(01).                GC0220  
00134         10  FILLER                     PIC  X(03).                GC0220  
00135                                                                   GC0220  
00136                                                                   GC0220  
00137  01  PARM-BEN-ONE-B.                                              GC0220  
00138      05  VSAM-RDW.                                                GC0220  
00139         10  VSAM-RECORD-LENGTH         PIC  9(04)    COMP.        GC0220  
00140         10  VSAM-FEEDBACK-CODE         PIC  9(04)    COMP.        GC0220  
00141      05  VSAM-RECORD-AREA              PIC  X(395).               GC0220  
00142      05  VSAM-RECORD-A       REDEFINES     VSAM-RECORD-AREA.      GC0220  
00143          10  VSAM-KEY-ID               PIC  X(10).                GC0220  
00144          10  VSAM-KEY-DATA.                                       GC0220  
00145             15  VSAM-DT-OF-LAST-CHANGE PIC S9(05)    COMP-3.      GC0220  
00146             15  VSAM-ENTRY-COUNT       PIC S9(03)    COMP-3.      GC0220  
00147             15  FILLER                 PIC  X(380).               GC0220  
00148                                                                   GC0220  
00149 /                                                                 GC0220  
00150  PROCEDURE DIVISION.                                              GC0220  
00151                                                                   GC0220  
00152 ******************************************************************GC0220  
00153 ****                                                              GC0220  
00154 **               P R O C E S S    C O N T R O L                   GC0220  
00155 ****                                                              GC0220  
00156 ******************************************************************GC0220  
00157  0000-MAINLINE.                                                   GC0220  
00158                                                                   GC0220  
00159      PERFORM 1000-OPEN-THE-FILES  THRU  1000-EXIT.                GC0220  
00160                                                                   GC0220  
00161      IF WS-END-OF-FILE-SWITCH-ON                                  GC0220  
00162          DISPLAY '  '                                             GC0220  
00163          DISPLAY ' GC0220--   NO INPUT RECORDS RECEIVED'          GC0220  
00164          PERFORM 9500-CLOSE-THE-FILES  THRU  9500-EXIT            GC0220  
00165          STOP RUN.                                                GC0220  
00166                                                                   GC0220  
00167      PERFORM 2500-PROCESS-ALL-INPUT-RECORDS THRU 2500-EXIT        GC0220  
00168          UNTIL  WS-END-OF-FILE-SWITCH-ON.                         GC0220  
00169                                                                   GC0220  
00170      PERFORM 9500-CLOSE-THE-FILES  THRU  9500-EXIT.               GC0220  
00171                                                                   GC0220  
00172      STOP RUN.                                                    GC0220  
00173                                                                   GC0220  
00174  0000-EXIT.                                                       GC0220  
00175      EXIT.                                                        GC0220  
00176 /                                                                 GC0220  
00177 ******************************************************************GC0220  
00178 ***                                                               GC0220  
00179 **       OPEN SEQUENTIAL AND VSAM FILES                           GC0220  
00180 **       READ THE FIRST RECORD FROM SEQUENTIAL INPUT FILE         GC0220  
00181 ***                                                               GC0220  
00182 ******************************************************************GC0220  
00183  1000-OPEN-THE-FILES.                                             GC0220  
00184                                                                   GC0220  
00185      OPEN INPUT  INPUT-SLOT-UPDATE-FILE.                          GC0220  
00186                                                                   GC0220  
00187                                                                   GC0220  
00188 **--- SET VSAM FILE FOR SPECIAL PROCESSING                        GC0220  
00189 **                                                                GC0220  
00190      MOVE  'S'          TO  VSAM-REQUEST-TYPE.                    GC0220  
00191      MOVE   8           TO  SET-VSAM-RECORD-LENGTH.               GC0220  
00192      MOVE   3           TO  SET-VSAM-VALUE.                       GC0220  
00193      CALL  'TSGVSAM1'  USING  PARM-BEN-ONE-A    PARM-SET.         GC0220  
00194                                                                   GC0220  
00195      IF  VSAM-REQUEST-TYPE    NOT EQUAL   'S'                     GC0220  
00196          DISPLAY  '  '                                            GC0220  
00197          DISPLAY  ' BAD SET    GC0220    1000-OPEN-THE-FILES'     GC0220  
00198          MOVE  SET-VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE         GC0220  
00199          GO TO  9999-ERROR-RTN.                                   GC0220  
00200                                                                   GC0220  
00201                                                                   GC0220  
00202                                                                   GC0220  
00203 **--- AN \
00204 **                                                                GC0220  
00205      MOVE  'O'          TO  VSAM-REQUEST-TYPE.                    GC0220  
00206      CALL  'TSGVSAM1'  USING  PARM-BEN-ONE-A   PARM-BEN-ONE-B.    GC0220  
00207                                                                   GC0220  
00208                                                                   GC0220  
00209      IF  VSAM-REQUEST-TYPE    NOT EQUAL   'O'                     GC0220  
00210          DISPLAY  '  '                                            GC0220  
00211          DISPLAY  ' BAD OPEN     GC0220    1000-OPEN-THE-FILES'   GC0220  
00212          MOVE  SET-VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE         GC0220  
00213          GO TO  9999-ERROR-RTN.                                   GC0220  
00214                                                                   GC0220  
00215      CALL 'TCDTES' USING HSCDATES.                                GC0220  
00216                                                                   GC0220  
00217      MOVE JYR  TO WS-YY.                                          GC0220  
00218      MOVE JDA  TO WS-DDD.                                         GC0220  
00219      MOVE WS-JUL-DATE TO WS-CURR-JUL-DATE.                        GC0220  
00220                                                                   GC0220  
00221 **--- READ THE FIRST INPUT RECORD.                                GC0220  
00222 **                                                                GC0220  
00223      PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT.                 GC0220  
00224                                                                   GC0220  
00225  1000-EXIT.                                                       GC0220  
00226      EXIT.                                                        GC0220  
00227 /                                                                 GC0220  
00228 ******************************************************************GC0220  
00229 ****                                                              GC0220  
00230 **      SEQUENTIAL BENEFIT PROVISION SLOT UPDATED FILE            GC0220  
00231 ****                                                              GC0220  
00232 ******************************************************************GC0220  
00233  2000-READ-INPUT-FILE.                                            GC0220  
00234                                                                   GC0220  
00235      READ INPUT-SLOT-UPDATE-FILE                                  GC0220  
00236          AT END                                                   GC0220  
00237              MOVE  '1'   TO   WS-END-OF-FILE-SW                   GC0220  
00238              DISPLAY ' '                                          GC0220  
00239              DISPLAY ' RECORDS READ FROM INPUT FILE    =   '      GC0220  
00240                                                 WS-RECORDS-READ   GC0220  
00241              DISPLAY ' RECORDS ADDED TO VSAM FILE      =   '      GC0220  
00242                                                 WS-RECORDS-ADDED  GC0220  
00243              GO TO 2000-EXIT.                                     GC0220  
00244                                                                   GC0220  
00245                                                                   GC0220  
00246      ADD  1  TO    WS-RECORDS-READ.                               GC0220  
00247                                                                   GC0220  
00248  2000-EXIT.                                                       GC0220  
00249      EXIT.                                                        GC0220  
00250 /                                                                 GC0220  
00251 ******************************************************************GC0220  
00252 ***                                                               GC0220  
00253 **   'N'  NEW BENEFIT PROVISION RECORDS ARE PROCESSED             GC0220  
00254 **   'E'  EXISTING BENEFIT PROVISION RECORDS ARE NOT PROCESSED    GC0220  
00255 ***                                                               GC0220  
00256 ******************************************************************GC0220  
00257  2500-PROCESS-ALL-INPUT-RECORDS.                                  GC0220  
00258                                                                   GC0220  
00259 **--- CHECK FIELD IN WORK RECORD.                                 GC0220  
00260 **                                                                GC0220  
00261      IF WRK-SIG-B-NEW-SLOT                                        GC0220  
00262          PERFORM 3000-PROCESS-BENEFIT-RECORD  THRU 3000-EXIT.     GC0220  
00263                                                                   GC0220  
00264                                                                   GC0220  
00265 **--- READ ANOTHER INPUT RECORD.                                  GC0220  
00266 **                                                                GC0220  
00267      PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT.                 GC0220  
00268                                                                   GC0220  
00269  2500-EXIT.                                                       GC0220  
00270      EXIT.                                                        GC0220  
00271 /                                                                 GC0220  
00272 ******************************************************************GC0220  
00273 ****                                                              GC0220  
00274 ***      INITIALIZE THE RECORD AREA AND COMPUTE THE LENGTH        GC0220  
00275 ****                                                              GC0220  
00276 ******************************************************************GC0220  
00277  3000-PROCESS-BENEFIT-RECORD.                                     GC0220  
00278                                                                   GC0220  
00279      MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        GC0220  
00280      MOVE  INPUT-BENEFIT-RECORD      TO  VSAM-RECORD-AREA.        GC0220  
00281                                                                   GC0220  
00282      COMPUTE VSAM-RECORD-LENGTH        =                          GC0220  
00283                               4        +                          GC0220  
00284          GC-GCBENPRV-FIXED-LEN         +                          GC0220  
00285          VSAM-ENTRY-COUNT              *                          GC0220  
00286          GC-GCBENPRV-VARY-LEN.                                    GC0220  
00287                                                                   GC0220  
00288      PERFORM 9000-INSERT-NEW-BENEFIT-PROVN  THRU 9000-EXIT.       GC0220  
00289                                                                   GC0220  
00290  3000-EXIT.                                                       GC0220  
00291      EXIT.                                                        GC0220  
00292 /                                                                 GC0220  
00293 ******************************************************************GC0220  
00294 ****                                                              GC0220  
00295 ***       INSERT NEW BENEFIT PROVISION RECORD IN VSAM FILE        GC0220  
00296 ****                                                              GC0220  
00297 ******************************************************************GC0220  
00298  9000-INSERT-NEW-BENEFIT-PROVN.                                   GC0220  
00299                                                                   GC0220  
00300 ***  MOVE CURRENT DATE TO DATE OF LAST CHANGE.                    GC0220  
00301      MOVE WS-CURR-JUL-DATE TO VSAM-DT-OF-LAST-CHANGE.             GC0220  
00302                                                                   GC0220  
00303      MOVE     'I'           TO   VSAM-REQUEST-TYPE.               GC0220  
00304      CALL 'TSGVSAM1'  USING  PARM-BEN-ONE-A    PARM-BEN-ONE-B.    GC0220  
00305                                                                   GC0220  
00306      IF  VSAM-REQUEST-TYPE    EQUAL    '3'                        GC0220  
00307          MOVE  INPUT-BENEFIT-ID     TO  WS-BENEFIT-ID             GC0220  
00308          MOVE  INPUT-BENEFIT-SLOT   TO  WS-BENEFIT-SLOT           GC0220  
00309          MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             GC0220  
00310          DISPLAY  '  '                                            GC0220  
00311          DISPLAY  ' GC0220 DUPLICATE KEY  9000-INSERT-NEW-BENEFIT'GC0220  
00312          DISPLAY  '  '                                            GC0220  
00313          DISPLAY  ' DUPLICATE KEY   =  '    WS-DISPLAY-KEY        GC0220  
00314          DISPLAY  '  '                                            GC0220  
00315          GO TO  9999-ERROR-RTN.                                   GC0220  
00316                                                                   GC0220  
00317                                                                   GC0220  
00318                                                                   GC0220  
00319      IF  VSAM-REQUEST-TYPE    NOT EQUAL   'I'                     GC0220  
00320          DISPLAY  '  '                                            GC0220  
00321          DISPLAY  ' GC0220 BAD INSERT  9000-INSERT-NEW-BENEFIT'   GC0220  
00322          MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             GC0220  
00323          GO TO  9999-ERROR-RTN.                                   GC0220  
00324                                                                   GC0220  
00325      ADD  1  TO   WS-RECORDS-ADDED.                               GC0220  
00326                                                                   GC0220  
00327  9000-EXIT.                                                       GC0220  
00328      EXIT.                                                        GC0220  
00329 /                                                                 GC0220  
00330 ******************************************************************GC0220  
00331 ****                                                              GC0220  
00332 **             CLOSE SEQUENTIAL AND VSAM FILES                    GC0220  
00333 ****                                                              GC0220  
00334 ******************************************************************GC0220  
00335  9500-CLOSE-THE-FILES.                                            GC0220  
00336                                                                   GC0220  
00337      CLOSE INPUT-SLOT-UPDATE-FILE.                                GC0220  
00338                                                                   GC0220  
00339                                                                   GC0220  
00340      MOVE     'C'           TO   VSAM-REQUEST-TYPE.               GC0220  
00341      CALL  'TSGVSAM1'  USING  PARM-BEN-ONE-A   PARM-BEN-ONE-B.    GC0220  
00342                                                                   GC0220  
00343                                                                   GC0220  
00344      IF  VSAM-REQUEST-TYPE    NOT EQUAL   'C'                     GC0220  
00345          DISPLAY  '  '                                            GC0220  
00346          DISPLAY  ' BAD CLOSE    GC0220    9500-CLOSE-THE-FILES'  GC0220  
00347          MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             GC0220  
00348          GO TO  9999-ERROR-RTN.                                   GC0220  
00349                                                                   GC0220  
00350  9500-EXIT.                                                       GC0220  
00351      EXIT.                                                        GC0220  
00352 /                                                                 GC0220  
00353 ******************************************************************GC0220  
00354 ****                                                              GC0220  
00355 **                       A B E N D                                GC0220  
00356 ****                                                              GC0220  
00357 ******************************************************************GC0220  
00358  9999-ERROR-RTN.                                                  GC0220  
00359                                                                   GC0220  
00360      CALL  'TSGEND' USING  WS-ABEND-CODE.                         GC0220  
00361                                                                   GC0220  
00362  9999-EXIT.                                                       GC0220  
00363      EXIT.                                                        GC0220  
00364 /                                                                 GC0220  
