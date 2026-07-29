00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID.    GC0025.                                           GC0025  
00003  AUTHOR.        FRANK GARRETT.                                       LV003
00004  INSTALLATION.  HCSC.                                             GC0025  
00005  DATE-WRITTEN.  09/87.                                            GC0025  
00006                                                                   GC0025  
00007 ******************************************************************GC0025  
00008 *   THIS  PROGRAM PRODUCES THE DCC TABLE MAINTENANCE ACTIVITY     GC0025  
00009 *   REPORT. IT USES THE RELEASED SYSTEM MASTER/TABULAR SEQUENTIAL GC0025  
00010 *   FILE CREATED IN GC0010 AND THE SYSTEM MASTER/TABULAR VSAM FILEGC0025  
00011 *                                                                 GC0025  
00012 *   RSMT     - RELEASED SYSTEM MASTER/TABULAR FILE    (INPUT)     GC0025  
00013 *   TSGVSAM1 - SYSTEM MASTER TABULAR FILE             (INPUT)     GC0025  
00014 *                                                                 GC0025  
00015 ******************************************************************GC0025  
00016 ******************************************************************GC0025  
00017 *               U P D A T E   H I S T O R Y                      *GC0025  
00018 *    DATE    PROGRAMMER   MAINTENANCE                            *GC0025  
00019 *  --------  ----------   ---------------------------------------*GC0025  
00020 *  09-24-87     FCG       NEW PROGRAM.                           *GC0025  
00021 *                                                                *GC0025  
00022 *  12-10-87     FCG       ADD NEW REPORT NUMBER TO PROGRAM.      *GC0025  
00023 *                                                                *GC0025  
00024 *  01-10-95     GDM       CONVERT TO COBOL II                    *GC0025  
00025 *                                                                *GC0025  
00026 *  08-14-02     GTF       EXPAND OPERATOR ID FROM 5 TO 8 CHARS.  *GC0025  
00027 *                                                                *GC0025  
00028 *  09-12-02     AKK       ADDING 9 CHARAC TO FILLER AREAS IN  .  *GC0025  
00029 *                         RSMT-RECORD AND PARM-TWO DUE TO OPID   *GC0025  
00030 *                         EXPANSION.                             *GC0025  
00031 *  08-05-03     AKK       REGEN'D FOR FILE CONVERSION, FILE SIZE *GC0025  
00032 *  08-05-03     AKK       ADD 05 FILLER FOR 12 BYTES TO FIX      *GC0025  
00033 *                         FILE MISMATCH                          *GC0025  
00034 *  08-07-03     AKK       ADDING 1 BYTE OF FILLER TO RSMT-       *GC0025  
00035 *                         FILE MISMATCH                          *GC0025  
00036 ******************************************************************GC0025  
00037                                                                   GC0025  
00038  ENVIRONMENT DIVISION.                                            GC0025  
00039                                                                   GC0025  
00040  CONFIGURATION SECTION.                                           GC0025  
00041  SOURCE-COMPUTER.  IBM-370.                                       GC0025  
00042  OBJECT-COMPUTER.  IBM-370.                                       GC0025  
00043  INPUT-OUTPUT SECTION.                                            GC0025  
00044                                                                   GC0025  
00045  FILE-CONTROL.                                                    GC0025  
00046      SELECT RSMT-FILE                 ASSIGN  TO UT-S-GC0025A.    GC0025  
00047  DATA DIVISION.                                                   GC0025  
00048                                                                   GC0025  
00049  FILE SECTION.                                                    GC0025  
00050                                                                   GC0025  
00051  FD  RSMT-FILE                                                    GC0025  
00052      LABEL RECORDS ARE STANDARD                                   GC0025  
00053      RECORDING MODE IS V                                          GC0025  
00054      BLOCK CONTAINS 0 RECORDS.                                    GC0025  
00055                                                                   GC0025  
00056  01  RSMT-RECORD.                                                 GC0025  
00057      COPY GCSYSDCC.                                               GC0025  
00058      05  RSMT-TAB-REC.                                            GC0025  
00059          10  RSMT-KEY-FIELD.                                      GC0025  
00060              15  RSMT-TAB-ID.                                     GC0025  
00061                  20 POSITION-1-RSMT PIC XX.                       GC0025  
00062                     88 DCC-TABLE-RECORD         VALUE '#T'.       GC0025  
00063                  20 POSITION-2-RSMT PIC X(4).                     GC0025  
00064              15  RSMT-TAB-SLOT      PIC S9(7)   COMP-3.           GC0025  
00065          10  FILLER                 PIC X(36).                    GC0025  
00066 *        10  FILLER                 PIC X(27).                    GC0025  
00067          10  RSMT-ENTRY-COUNT       PIC S9(5)   COMP-3.           GC0025  
00068          10  FILLER                 PIC X(3960).                  GC0025  
00069      05  FILLER                     PIC X(03).                    GC0025  
00070 /                                                                 GC0025  
00071  WORKING-STORAGE SECTION.                                         GC0025  
00072  01  FILLER                        PIC  X(24)                     GC0025  
00073              VALUE 'GC0025 WORKING STORAGE'.                      GC0025  
00074  01  GC-TCON-REC.                                                 GC0025  
00075      COPY  GCTTCONC.                                              GC0025  
00076 /                                                                 GC0025  
00077  01  PARM-ONE.                                                    GC0025  
00078      05 RESERVED-FLDS              PIC 9(8)   VALUE ZEROS COMP.   GC0025  
00079      05 RESERVED-ONE  REDEFINES  RESERVED-FLDS.                   GC0025  
00080          10 REQUEST-TYPE           PIC X.                         GC0025  
00081          10 FILLER                 PIC X(3).                      GC0025  
00082                                                                   GC0025  
00083  01  PARM-TWO.                                                    GC0025  
00084      05 RDW.                                                      GC0025  
00085          10 RECORD-LENGTH          PIC 9(4)   COMP.               GC0025  
00086          10 FEEDBACK-CODE          PIC 9(4)   COMP.               GC0025  
00087      05  REC-AREA                  PIC X(4009).                   GC0025  
00088      05  VSAM-RECORD  REDEFINES  REC-AREA.                        GC0025  
00089          10 VSAM-KEY-FIELD.                                       GC0025  
00090              15  VSAM-ID           PIC X(6).                      GC0025  
00091              15  VSAM-SLOT         PIC S9(7)  COMP-3.             GC0025  
00092          10 VSAM-DATA.                                            GC0025  
00093 *EXPANDED DUE TO 8 BYTES PER OPID.                                GC0025  
00094              15 VSAM-FIXED-PART    PIC X(36).                     GC0025  
00095 *            15 VSAM-FIXED-PART    PIC X(27).                     GC0025  
00096              15 VSAM-ENTRY-COUNT   PIC S9(5)  COMP-3.             GC0025  
00097              15 VSAM-VAR-PART      PIC X(3960).                   GC0025  
00098                                                                   GC0025  
00099  01  PARM-SET.                                                    GC0025  
00100      05 SET-RDW.                                                  GC0025  
00101          10 SET-RECORD-LENGTH      PIC 9(4)       COMP.           GC0025  
00102          10 SET-FEEDBACK-CODE      PIC 9(4)       COMP.           GC0025  
00103      05  SET-VALUE                 PIC 9(8)       COMP.           GC0025  
00104 /                                                                 GC0025  
00105  01  GC-TCON-REC2.                                                GC0025  
00106      COPY  GCTTCON2.                                              GC0025  
00107 /                                                                 GC0025  
00108  01  PROGRAM-VARIABLES.                                           GC0025  
00109      05  HOLD-TAB-ID                   PIC X(11)  VALUE SPACES.   GC0025  
00110      05  SEARCH-KEY.                                              GC0025  
00111          10  SEARCH-ID                 PIC X(6).                  GC0025  
00112          10  SEARCH-SLOT               PIC S9(7)  COMP-3.         GC0025  
00113      05  EOF-SW                        PIC XXX    VALUE SPACES.   GC0025  
00114          88  RSMT-EOF                             VALUE 'END'.    GC0025  
00115                                                                   GC0025  
00116  01  WRK-SUBSCRIPTS.                                              GC0025  
00117      05 I              VALUE +0        PIC S9(4)      COMP-3.     GC0025  
00118      05 J              VALUE +0        PIC S9(4)      COMP-3.     GC0025  
00119      05 K              VALUE +0        PIC S9(4)      COMP-3.     GC0025  
00120      05 SORT-VALUE     VALUE +0        PIC S9(4)      COMP-3.     GC0025  
00121      05 CHECK-VALUE    VALUE +0        PIC S9(4)      COMP-3.     GC0025  
00122 *                                                                 GC0025  
00123  01  WRK-DELETE-TABLE-AREA.                                       GC0025  
00124      05 WRK-DELETE-TABLE     OCCURS 660 TIMES INDEXED BY          GC0025  
00125          DEL-INDEX.                                               GC0025  
00126          10  DEL-ELIG-CODE             PIC X(2).                  GC0025  
00127          10  DEL-SLOT-NUMBER           PIC 9(7).                  GC0025  
00128 *                                                                 GC0025  
00129  01  WRK-ADD-TABLE-AREA.                                          GC0025  
00130      05 WRK-ADD-TABLE        OCCURS 660 TIMES INDEXED BY          GC0025  
00131          ADD-INDEX.                                               GC0025  
00132          10  ADD-ELIG-CODE             PIC X(2).                  GC0025  
00133          10  ADD-SLOT-NUMBER           PIC 9(7).                  GC0025  
00134 /                                                                 GC0025  
00135  01  RPT-IND    VALUE '1128'  PIC X(4).                           GC0025  
00136                                                                   GC0025  
00137  01  LINE-COUNT VALUE  ZEROES PIC 99.                             GC0025  
00138                                                                   GC0025  
00139  01  GC040050-IND             PIC X.                              GC0025  
00140      88  HEADER-TIME          VALUE 'H'.                          GC0025  
00141      88  RPT-TIME             VALUE 'R'.                          GC0025  
00142      88  TRAIL-TIME           VALUE 'T'.                          GC0025  
00143      88  DETAIL-TIME          VALUE 'D'.                          GC0025  
00144      88  LOAD-TIME            VALUE 'L'.                          GC0025  
00145                                                                   GC0025  
00146  01  PRINT-LINE-CC.                                               GC0025  
00147      05  CONTROL-CHARACTERS         PIC X.                        GC0025  
00148      05  PRINT-LINE-132             PIC X(132).                   GC0025  
00149                                                                   GC0025  
00150  01  COUNT-AMT                      PIC S9(5)  COMP-3.            GC0025  
00151                                                                   GC0025  
00152  01  WS-MESSAGE-LINE.                                             GC0025  
00153      05  FILLER                     PIC X(06)  VALUE SPACES.      GC0025  
00154      05  FILLER                     PIC X(07)  VALUE              GC0025  
00155      '**  NO '.                                                   GC0025  
00156      05  WS-TAB-ID                  PIC X(04)  VALUE SPACE.       GC0025  
00157      05  FILLER                     PIC X(45)  VALUE              GC0025  
00158      ' DCC TABLE ELIGIBILITY CODES CHANGED FOR THIS'.             GC0025  
00159      05  FILLER                     PIC X(20)  VALUE              GC0025  
00160      ' PROCESSING DATE  **'.                                      GC0025  
00161      05  FILLER                     PIC X(51)  VALUE SPACES.      GC0025  
00162                                                                   GC0025  
00163  01  PRINT-SUB                      PIC 9(1).                     GC0025  
00164      COPY HSCDATES.                                               GC0025  
00165 /                                                                 GC0025  
00166  01  JUL-DATE.                                                    GC0025  
00167      05  JUL-DT.                                                  GC0025  
00168          10  JUL-YY                 PIC 99.                       GC0025  
00169          10  JUL-DD                 PIC 999.                      GC0025  
00170      05  TODAYS-DATE REDEFINES  JUL-DT PIC 9(5).                  GC0025  
00171                                                                   GC0025  
00172  01  DATE-AREA.                                                   GC0025  
00173      05  GREG-DATE                  PIC 9(6).                     GC0025  
00174      05  JULIAN-DATE                PIC 9(5).                     GC0025  
00175      05  WS-DATE.                                                 GC0025  
00176          10  WS-DATE-MO             PIC 9(02).                    GC0025  
00177          10  WS-DATE-DAY            PIC 9(02).                    GC0025  
00178          10  WS-DATE-YR             PIC 9(02).                    GC0025  
00179      05  WS-DATE-FIELDS.                                          GC0025  
00180          10  WS-MO                  PIC 9(02).                    GC0025  
00181          10  FILLER                 PIC X(01)  VALUE '-'.         GC0025  
00182          10  WS-DAY                 PIC 9(02).                    GC0025  
00183          10  FILLER                 PIC X(01)  VALUE '-'.         GC0025  
00184          10  WS-YR                  PIC 9(02).                    GC0025  
00185                                                                   GC0025  
00186  01  WS-SWITCHES.                                                 GC0025  
00187    05  END-OF-FILE-SW               PIC XXX  VALUE SPACES.        GC0025  
00188      88  END-OF-RSMT-FILE                  VALUE 'END'.           GC0025  
00189    05  TABLE-LOAD-END-SW            PIC X(01)  VALUE  '0'.        GC0025  
00190      88  END-OF-TABLE-LOAD                     VALUE  '1'.        GC0025  
00191    05  TABLE-PRINT-END-SW           PIC X(01)  VALUE  '0'.        GC0025  
00192      88  END-OF-TABLE-PRINT                    VALUE  '1'.        GC0025  
00193    05  WS-TXDIP-SWITCH              PIC X(01)  VALUE  '0'.        GC0025  
00194      88  WS-TXDIP-SWITCH-ON                    VALUE  '1'.        GC0025  
00195    05  WS-TXDOP-SWITCH              PIC X(01)  VALUE  '0'.        GC0025  
00196      88  WS-TXDOP-SWITCH-ON                    VALUE  '1'.        GC0025  
00197    05  WS-TXCON-SWITCH              PIC X(01)  VALUE  '0'.        GC0025  
00198      88  WS-TXCON-SWITCH-ON                    VALUE  '1'.        GC0025  
00199    05  WS-TXCOS-SWITCH              PIC X(01)  VALUE  '0'.        GC0025  
00200      88  WS-TXCOS-SWITCH-ON                    VALUE  '1'.        GC0025  
00201                                                                   GC0025  
00202  01  ABEND-CODE                       PIC 9(4)    COMP.           GC0025  
00203                                                                   GC0025  
00204 /                                                                 GC0025  
00205  01  HEADINGS.                                                    GC0025  
00206 * 8/14/02 EXPANDED OPID TO 8 BYTES FROM 5. SUBTRACTED 15 BYTES    GC0025  
00207 * FROM RIGHTMOST FILLER. GTF                                      GC0025  
00208                                                                   GC0025  
00209 *    02  HEAD-1.                                                  GC0025  
00210      05  FILLER     PIC X(02) VALUE SPACES.                       GC0025  
00211      05  FILLER     PIC X(20) VALUE 'CURRENT OPERATOR ID '.       GC0025  
00212      05  HD1-OPER-ID-1       PIC X(8).                            GC0025  
00213      05  FILLER     PIC X(10) VALUE SPACES.                       GC0025  
00214      05  FILLER     PIC X(25) VALUE '1ST PREVIOUS OPERATOR ID '.  GC0025  
00215      05  HD1-OPER-ID-2       PIC X(8).                            GC0025  
00216      05  FILLER     PIC X(10) VALUE SPACES.                       GC0025  
00217      05  FILLER     PIC X(25) VALUE '2ND PREVIOUS OPERATOR ID '.  GC0025  
00218      05  HD1-OPER-ID-3       PIC X(8).                            GC0025  
00219      05  FILLER     PIC X(11) VALUE SPACE.                        GC0025  
00220                                                                   GC0025  
00221 *    02  HEAD-2.                                                  GC0025  
00222      05  FILLER              PIC X(133) VALUE ' '.                GC0025  
00223                                                                   GC0025  
00224 *    02  HEAD-3.                                                  GC0025  
00225      05  FILLER     PIC X(02)   VALUE SPACES.                     GC0025  
00226      05  FILLER     PIC X(13)  VALUE '* * * * * *  '.             GC0025  
00227      05  HD3-TABLE-ID1         PIC X(04).                         GC0025  
00228      05  FILLER     PIC X(02)  VALUE SPACES.                      GC0025  
00229      05  FILLER     PIC X(26)  VALUE 'ELIGIBILITY CODES DELETED '.GC0025  
00230      05  FILLER     PIC X(26)  VALUE ' * * * * * *    * * * * * '.GC0025  
00231      05  FILLER     PIC X(04)  VALUE '*   '.                      GC0025  
00232      05  HD3-TABLE-ID2         PIC X(04).                         GC0025  
00233      05  FILLER     PIC X(03)  VALUE SPACES.                      GC0025  
00234      05  FILLER     PIC X(26)  VALUE 'ELIGIBILITY CODES ADDED  *'.GC0025  
00235      05  FILLER     PIC X(10)  VALUE ' * * * * *'.                GC0025  
00236      05  FILLER     PIC X(18)  VALUE SPACES.                      GC0025  
00237                                                                   GC0025  
00238 *    02  HEAD-4.                                                  GC0025  
00239      05  FILLER              PIC X(133) VALUE ' '.                GC0025  
00240                                                                   GC0025  
00241  01  HEADS  REDEFINES  HEADINGS.                                  GC0025  
00242      02  HEAD-LINE  OCCURS 4  TIMES  INDEXED  BY                  GC0025  
00243                     HEAD-INDEX   PIC X(133).                      GC0025  
00244  01  DETAIL-1.                                                    GC0025  
00245      05  FILLER              PIC X(6)   VALUE SPACES.             GC0025  
00246      05  DT1-ID-SLOT1     OCCURS   3  TIMES                       GC0025  
00247                           INDEXED BY SLOT1-INDEX.                 GC0025  
00248          10  DT1-ID1         PIC X(2).                            GC0025  
00249          10  FILLER          PIC X(2).                            GC0025  
00250          10  DT1-SLOT1       PIC 9(7).                            GC0025  
00251          10  FILLER          PIC X(8).                            GC0025  
00252      05  FILLER              PIC X(4)   VALUE SPACES.             GC0025  
00253      05  DT1-ID-SLOT2     OCCURS   3  TIMES                       GC0025  
00254                           INDEXED BY SLOT2-INDEX.                 GC0025  
00255          10  DT1-ID2         PIC X(2).                            GC0025  
00256          10  FILLER          PIC X(2).                            GC0025  
00257          10  DT1-SLOT2       PIC 9(7).                            GC0025  
00258          10  FILLER          PIC X(8).                            GC0025  
00259      05  FILLER              PIC X(9)   VALUE SPACES.             GC0025  
00260                                                                   GC0025  
00261  PROCEDURE DIVISION.                                              GC0025  
00262  0000-MAINLINE.                                                   GC0025  
00263      OPEN   INPUT      RSMT-FILE.                                 GC0025  
00264                                                                   GC0025  
00265      MOVE   'S'                  TO REQUEST-TYPE.                 GC0025  
00266      MOVE    8                   TO SET-RECORD-LENGTH.            GC0025  
00267      MOVE    3                   TO SET-VALUE.                    GC0025  
00268                                                                   GC0025  
00269      CALL   'TSGVSAM1'  USING PARM-ONE PARM-SET.                  GC0025  
00270      IF  REQUEST-TYPE NOT EQUAL 'S'                               GC0025  
00271          MOVE SET-FEEDBACK-CODE  TO ABEND-CODE                    GC0025  
00272          GO TO 9999-ERROR-RTN.                                    GC0025  
00273                                                                   GC0025  
00274 *    MOVE   'O'                  TO REQUEST-TYPE.                 GC0025  
00275 *    CALL   'TSGVSAM1'  USING PARM-ONE  PARM-TWO.                 GC0025  
00276 *    IF  REQUEST-TYPE NOT EQUAL 'O'                               GC0025  
00277 *        MOVE FEEDBACK-CODE      TO ABEND-CODE                    GC0025  
00278 *        GO TO 9999-ERROR-RTN.                                    GC0025  
00279 *                                                                 GC0025  
00280      CALL 'TCDTES'    USING HSCDATES.                             GC0025  
00281                                                                   GC0025  
00282      MOVE JYR                    TO JUL-YY.                       GC0025  
00283      MOVE JDA                    TO JUL-DD.                       GC0025  
00284                                                                   GC0025  
00285      MOVE 'H'                    TO GC040050-IND.                 GC0025  
00286                                                                   GC0025  
00287      PERFORM 0120-CALL-PRINT THRU 0129-EXIT.                      GC0025  
00288                                                                   GC0025  
00289      PERFORM 0010-PROCESS-RTN  THRU 0019-EXIT                     GC0025  
00290          UNTIL END-OF-RSMT-FILE.                                  GC0025  
00291                                                                   GC0025  
00292      MOVE SPACE              TO HD1-OPER-ID-1.                    GC0025  
00293      MOVE SPACE              TO HD1-OPER-ID-2.                    GC0025  
00294      MOVE SPACE              TO HD1-OPER-ID-3.                    GC0025  
00295                                                                   GC0025  
00296      IF  NOT  WS-TXCON-SWITCH-ON                                  GC0025  
00297        MOVE 'XCON'           TO  WS-TAB-ID                        GC0025  
00298        MOVE '#TXCON'         TO   RSMT-TAB-ID                     GC0025  
00299        PERFORM 0100-HEADING-RTN THRU 0109-EXIT                    GC0025  
00300        MOVE WS-MESSAGE-LINE  TO  PRINT-LINE-CC                    GC0025  
00301        MOVE '-'              TO  CONTROL-CHARACTERS               GC0025  
00302        PERFORM 0120-CALL-PRINT THRU 0129-EXIT.                    GC0025  
00303                                                                   GC0025  
00304      IF  NOT  WS-TXCOS-SWITCH-ON                                  GC0025  
00305        MOVE 'XCOS'           TO  WS-TAB-ID                        GC0025  
00306        MOVE '#TXCOS'         TO   RSMT-TAB-ID                     GC0025  
00307        PERFORM 0100-HEADING-RTN THRU 0109-EXIT                    GC0025  
00308        MOVE WS-MESSAGE-LINE  TO  PRINT-LINE-CC                    GC0025  
00309        MOVE '-'              TO  CONTROL-CHARACTERS               GC0025  
00310        PERFORM 0120-CALL-PRINT THRU 0129-EXIT.                    GC0025  
00311                                                                   GC0025  
00312      IF  NOT  WS-TXDIP-SWITCH-ON                                  GC0025  
00313        MOVE 'XDIP'           TO  WS-TAB-ID                        GC0025  
00314        MOVE '#TXDIP'         TO   RSMT-TAB-ID                     GC0025  
00315        PERFORM 0100-HEADING-RTN THRU 0109-EXIT                    GC0025  
00316        MOVE WS-MESSAGE-LINE  TO  PRINT-LINE-CC                    GC0025  
00317        MOVE '-'              TO  CONTROL-CHARACTERS               GC0025  
00318        PERFORM 0120-CALL-PRINT THRU 0129-EXIT.                    GC0025  
00319                                                                   GC0025  
00320      IF  NOT  WS-TXDOP-SWITCH-ON                                  GC0025  
00321        MOVE 'XDOP'           TO  WS-TAB-ID                        GC0025  
00322        MOVE '#TXDOP'         TO   RSMT-TAB-ID                     GC0025  
00323        PERFORM 0100-HEADING-RTN THRU 0109-EXIT                    GC0025  
00324        MOVE WS-MESSAGE-LINE  TO  PRINT-LINE-CC                    GC0025  
00325        MOVE '-'              TO  CONTROL-CHARACTERS               GC0025  
00326        PERFORM 0120-CALL-PRINT THRU 0129-EXIT.                    GC0025  
00327                                                                   GC0025  
00328      MOVE   'C'                  TO REQUEST-TYPE.                 GC0025  
00329      CALL   'TSGVSAM1'  USING PARM-ONE PARM-TWO.                  GC0025  
00330      IF  REQUEST-TYPE NOT EQUAL 'C'                               GC0025  
00331          MOVE FEEDBACK-CODE      TO ABEND-CODE                    GC0025  
00332          GO TO 9999-ERROR-RTN.                                    GC0025  
00333                                                                   GC0025  
00334      CLOSE  RSMT-FILE.                                            GC0025  
00335                                                                   GC0025  
00336      MOVE 'T'                    TO GC040050-IND.                 GC0025  
00337                                                                   GC0025  
00338      PERFORM 0120-CALL-PRINT THRU 0129-EXIT.                      GC0025  
00339                                                                   GC0025  
00340      GOBACK.                                                      GC0025  
00341                                                                   GC0025  
00342  0000-EXIT.                                                       GC0025  
00343       EXIT.                                                       GC0025  
00344       EJECT                                                       GC0025  
00345  0010-PROCESS-RTN.                                                GC0025  
00346                                                                   GC0025  
00347      READ RSMT-FILE                                               GC0025  
00348           AT END                                                  GC0025  
00349               MOVE 'END'         TO END-OF-FILE-SW                GC0025  
00350               GO TO 0019-EXIT.                                    GC0025  
00351 **                                                                GC0025  
00352      IF  DCC-TABLE-RECORD                                         GC0025  
00353         NEXT SENTENCE                                             GC0025  
00354      ELSE                                                         GC0025  
00355         GO TO 0019-EXIT.                                          GC0025  
00356                                                                   GC0025  
00357      MOVE ZERO                   TO TABLE-LOAD-END-SW.            GC0025  
00358      MOVE ZERO                   TO TABLE-PRINT-END-SW.           GC0025  
00359                                                                   GC0025  
00360      MOVE RSMT-ENTRY-COUNT       TO GT1-ENTRY-COUNT.              GC0025  
00361      MOVE RSMT-TAB-REC           TO GT1-RECORD.                   GC0025  
00362                                                                   GC0025  
00363      MOVE GT1-OPID-CURRENT       TO HD1-OPER-ID-1.                GC0025  
00364      MOVE GT1-OPID-1ST-PREVIOUS  TO HD1-OPER-ID-2.                GC0025  
00365      MOVE GT1-OPID-2ND-PREVIOUS  TO HD1-OPER-ID-3.                GC0025  
00366      PERFORM 0100-HEADING-RTN THRU 0109-EXIT.                     GC0025  
00367                                                                   GC0025  
00368      MOVE GT1-TABULAR-PROVISION-ID   TO VSAM-KEY-FIELD.           GC0025  
00369                                                                   GC0025  
00370      PERFORM 0310-READ-TPF-FILE THRU 0310-EXIT.                   GC0025  
00371                                                                   GC0025  
00372      SUBTRACT 1 FROM GT1-ENTRY-COUNT GIVING COUNT-AMT.            GC0025  
00373      SET ADD-INDEX   TO 1.                                        GC0025  
00374      SET DEL-INDEX   TO 1.                                        GC0025  
00375      SET GT1-INDEX   TO 1.                                        GC0025  
00376      SET GT1X-INDEX  TO 1.                                        GC0025  
00377      SET SLOT1-INDEX   TO 1.                                      GC0025  
00378      SET SLOT2-INDEX   TO 1.                                      GC0025  
00379                                                                   GC0025  
00380      MOVE SPACE TO WRK-ADD-TABLE-AREA.                            GC0025  
00381      MOVE SPACE TO WRK-DELETE-TABLE-AREA.                         GC0025  
00382                                                                   GC0025  
00383      PERFORM 0300-LOAD-TABLES THRU 0300-EXIT                      GC0025  
00384          UNTIL END-OF-TABLE-LOAD.                                 GC0025  
00385                                                                   GC0025  
00386      SET ADD-INDEX   TO 1.                                        GC0025  
00387      SET DEL-INDEX   TO 1.                                        GC0025  
00388      SET GT1-INDEX   TO 1.                                        GC0025  
00389      SET GT1X-INDEX  TO 1.                                        GC0025  
00390      SET SLOT1-INDEX   TO 1.                                      GC0025  
00391      SET SLOT2-INDEX   TO 1.                                      GC0025  
00392      PERFORM 0400-PRINT-TABLES THRU 0400-EXIT                     GC0025  
00393          UNTIL END-OF-TABLE-PRINT.                                GC0025  
00394                                                                   GC0025  
00395  0019-EXIT.                                                       GC0025  
00396       EXIT.                                                       GC0025  
00397 /                                                                 GC0025  
00398  0100-HEADING-RTN.                                                GC0025  
00399                                                                   GC0025  
00400      MOVE ZERO TO LINE-COUNT.                                     GC0025  
00401      IF  RSMT-TAB-ID = '#TXCON'                                   GC0025  
00402          MOVE 'XCON'         TO  HD3-TABLE-ID1                    GC0025  
00403          MOVE 'XCON'         TO  HD3-TABLE-ID2                    GC0025  
00404          MOVE '1'            TO  WS-TXCON-SWITCH                  GC0025  
00405      ELSE                                                         GC0025  
00406      IF  RSMT-TAB-ID = '#TXCOS'                                   GC0025  
00407          MOVE 'XCOS'         TO  HD3-TABLE-ID1                    GC0025  
00408          MOVE 'XCOS'         TO  HD3-TABLE-ID2                    GC0025  
00409          MOVE '1'            TO  WS-TXCOS-SWITCH                  GC0025  
00410      ELSE                                                         GC0025  
00411      IF  RSMT-TAB-ID = '#TXDIP'                                   GC0025  
00412          MOVE 'XDIP'         TO  HD3-TABLE-ID1                    GC0025  
00413          MOVE 'XDIP'         TO  HD3-TABLE-ID2                    GC0025  
00414          MOVE '1'            TO  WS-TXDIP-SWITCH                  GC0025  
00415      ELSE                                                         GC0025  
00416      IF  RSMT-TAB-ID = '#TXDOP'                                   GC0025  
00417          MOVE 'XDOP'         TO  HD3-TABLE-ID1                    GC0025  
00418          MOVE 'XDOP'         TO  HD3-TABLE-ID2                    GC0025  
00419          MOVE '1'            TO  WS-TXDOP-SWITCH.                 GC0025  
00420                                                                   GC0025  
00421      MOVE 'R'                        TO GC040050-IND.             GC0025  
00422                                                                   GC0025  
00423      PERFORM 0110-HEAD-PRINT THRU 0119-EXIT                       GC0025  
00424          VARYING HEAD-INDEX FROM 1 BY 1                           GC0025  
00425          UNTIL HEAD-INDEX IS GREATER THAN 4.                      GC0025  
00426                                                                   GC0025  
00427      ADD 8 TO LINE-COUNT.                                         GC0025  
00428                                                                   GC0025  
00429  0109-EXIT.                                                       GC0025  
00430       EXIT.                                                       GC0025  
00431 /                                                                 GC0025  
00432  0110-HEAD-PRINT.                                                 GC0025  
00433                                                                   GC0025  
00434      MOVE HEAD-LINE (HEAD-INDEX)     TO PRINT-LINE-CC.            GC0025  
00435                                                                   GC0025  
00436      PERFORM 0120-CALL-PRINT THRU 0129-EXIT.                      GC0025  
00437                                                                   GC0025  
00438      MOVE 'D'                        TO GC040050-IND.             GC0025  
00439                                                                   GC0025  
00440  0119-EXIT.                                                       GC0025  
00441       EXIT.                                                       GC0025  
00442 /                                                                 GC0025  
00443  0120-CALL-PRINT.                                                 GC0025  
00444                                                                   GC0025  
00445      CALL  'GC040050'  USING  RPT-IND, LINE-COUNT, GC040050-IND,  GC0025  
00446        PRINT-LINE-CC.                                             GC0025  
00447                                                                   GC0025  
00448  0129-EXIT.                                                       GC0025  
00449       EXIT.                                                       GC0025  
00450 /                                                                 GC0025  
00451  0220-PRINT-LINE.                                                 GC0025  
00452                                                                   GC0025  
00453      IF  LINE-COUNT GREATER THAN 58                               GC0025  
00454          PERFORM 0100-HEADING-RTN THRU 0109-EXIT.                 GC0025  
00455                                                                   GC0025  
00456      ADD 1 TO LINE-COUNT.                                         GC0025  
00457                                                                   GC0025  
00458      MOVE DETAIL-1                   TO PRINT-LINE-CC.            GC0025  
00459      MOVE 'D'                        TO GC040050-IND.             GC0025  
00460                                                                   GC0025  
00461      PERFORM 0120-CALL-PRINT THRU 0129-EXIT.                      GC0025  
00462                                                                   GC0025  
00463  0220-EXIT.                                                       GC0025  
00464       EXIT.                                                       GC0025  
00465 /                                                                 GC0025  
00466  0300-LOAD-TABLES.                                                GC0025  
00467                                                                   GC0025  
00468      IF  END-OF-TABLE-LOAD                                        GC0025  
00469          GO TO 0300-EXIT.                                         GC0025  
00470                                                                   GC0025  
00471      IF  GT1-ELIGIBILITY-CODE (GT1-INDEX) = HIGH-VALUE            GC0025  
00472      AND GT1X-ELIGIBILITY-CODE (GT1X-INDEX) = HIGH-VALUE          GC0025  
00473          MOVE GT1-ELIGIBILITY-CODE (GT1-INDEX)                    GC0025  
00474            TO ADD-ELIG-CODE (ADD-INDEX)                           GC0025  
00475          MOVE GT1-SLOT-NUMBER (GT1-INDEX)                         GC0025  
00476            TO ADD-SLOT-NUMBER (ADD-INDEX)                         GC0025  
00477          MOVE GT1X-ELIGIBILITY-CODE (GT1X-INDEX)                  GC0025  
00478            TO DEL-ELIG-CODE (DEL-INDEX)                           GC0025  
00479          MOVE GT1X-SLOT-NUMBER (GT1X-INDEX)                       GC0025  
00480            TO DEL-SLOT-NUMBER (DEL-INDEX)                         GC0025  
00481          MOVE '1' TO TABLE-LOAD-END-SW                            GC0025  
00482          GO TO 0300-EXIT.                                         GC0025  
00483                                                                   GC0025  
00484      IF  GT1-ELIGIBILITY-CODE (GT1-INDEX) =                       GC0025  
00485          GT1X-ELIGIBILITY-CODE (GT1X-INDEX)                       GC0025  
00486      AND                                                          GC0025  
00487          GT1-SLOT-NUMBER (GT1-INDEX) =                            GC0025  
00488          GT1X-SLOT-NUMBER (GT1X-INDEX)                            GC0025  
00489          SET GT1-INDEX  UP BY +1                                  GC0025  
00490          SET GT1X-INDEX UP BY +1                                  GC0025  
00491          GO TO 0300-EXIT.                                         GC0025  
00492                                                                   GC0025  
00493      IF  GT1-ELIGIBILITY-CODE (GT1-INDEX) =                       GC0025  
00494          GT1X-ELIGIBILITY-CODE (GT1X-INDEX)                       GC0025  
00495      AND                                                          GC0025  
00496          GT1-SLOT-NUMBER (GT1-INDEX) NOT =                        GC0025  
00497          GT1X-SLOT-NUMBER (GT1X-INDEX)                            GC0025  
00498          MOVE GT1-ELIGIBILITY-CODE (GT1-INDEX)                    GC0025  
00499            TO ADD-ELIG-CODE (ADD-INDEX)                           GC0025  
00500          MOVE GT1-SLOT-NUMBER (GT1-INDEX)                         GC0025  
00501            TO ADD-SLOT-NUMBER (ADD-INDEX)                         GC0025  
00502          MOVE GT1X-ELIGIBILITY-CODE (GT1X-INDEX)                  GC0025  
00503            TO DEL-ELIG-CODE (DEL-INDEX)                           GC0025  
00504          MOVE GT1X-SLOT-NUMBER (GT1X-INDEX)                       GC0025  
00505            TO DEL-SLOT-NUMBER (DEL-INDEX)                         GC0025  
00506          SET ADD-INDEX  UP BY +1                                  GC0025  
00507          SET DEL-INDEX  UP BY +1                                  GC0025  
00508          SET GT1-INDEX  UP BY +1                                  GC0025  
00509          SET GT1X-INDEX UP BY +1                                  GC0025  
00510          GO TO 0300-EXIT.                                         GC0025  
00511                                                                   GC0025  
00512      IF  GT1-ELIGIBILITY-CODE (GT1-INDEX) <                       GC0025  
00513          GT1X-ELIGIBILITY-CODE (GT1X-INDEX)                       GC0025  
00514          MOVE GT1-ELIGIBILITY-CODE (GT1-INDEX)                    GC0025  
00515            TO ADD-ELIG-CODE (ADD-INDEX)                           GC0025  
00516          MOVE GT1-SLOT-NUMBER (GT1-INDEX)                         GC0025  
00517            TO ADD-SLOT-NUMBER (ADD-INDEX)                         GC0025  
00518          SET ADD-INDEX  UP BY +1                                  GC0025  
00519          SET GT1-INDEX  UP BY +1                                  GC0025  
00520          GO TO 0300-EXIT.                                         GC0025  
00521                                                                   GC0025  
00522      IF  GT1-ELIGIBILITY-CODE (GT1-INDEX) >                       GC0025  
00523          GT1X-ELIGIBILITY-CODE (GT1X-INDEX)                       GC0025  
00524          MOVE GT1X-ELIGIBILITY-CODE (GT1X-INDEX)                  GC0025  
00525            TO DEL-ELIG-CODE (DEL-INDEX)                           GC0025  
00526          MOVE GT1X-SLOT-NUMBER (GT1X-INDEX)                       GC0025  
00527            TO DEL-SLOT-NUMBER (DEL-INDEX)                         GC0025  
00528          SET DEL-INDEX   UP BY +1                                 GC0025  
00529          SET GT1X-INDEX  UP BY +1                                 GC0025  
00530          GO TO 0300-EXIT.                                         GC0025  
00531                                                                   GC0025  
00532  0300-EXIT.                                                       GC0025  
00533       EXIT.                                                       GC0025  
00534 /                                                                 GC0025  
00535  0310-READ-TPF-FILE.                                              GC0025  
00536                                                                   GC0025  
00537      MOVE 'R'                        TO REQUEST-TYPE.             GC0025  
00538      MOVE 14                         TO RECORD-LENGTH.            GC0025  
00539      CALL 'TSGVSAM1' USING PARM-ONE PARM-TWO.                     GC0025  
00540      IF  REQUEST-TYPE EQUAL '3'                                   GC0025  
00541          MOVE 1050                   TO ABEND-CODE                GC0025  
00542          GO TO 9999-ERROR-RTN.                                    GC0025  
00543      IF  REQUEST-TYPE NOT EQUAL 'R'                               GC0025  
00544          MOVE FEEDBACK-CODE          TO ABEND-CODE                GC0025  
00545          GO TO 9999-ERROR-RTN.                                    GC0025  
00546                                                                   GC0025  
00547      MOVE VSAM-ENTRY-COUNT           TO GT1X-ENTRY-COUNT.         GC0025  
00548      MOVE REC-AREA                   TO GT1X-RECORD.              GC0025  
00549  0310-EXIT.                                                       GC0025  
00550      EXIT.                                                        GC0025  
00551 /                                                                 GC0025  
00552  0400-PRINT-TABLES.                                               GC0025  
00553                                                                   GC0025  
00554      IF  END-OF-TABLE-PRINT                                       GC0025  
00555          GO TO 0400-EXIT.                                         GC0025  
00556                                                                   GC0025  
00557      IF  DEL-ELIG-CODE (DEL-INDEX) = HIGH-VALUE                   GC0025  
00558      AND ADD-ELIG-CODE (ADD-INDEX) = HIGH-VALUE                   GC0025  
00559          MOVE '1' TO TABLE-PRINT-END-SW                           GC0025  
00560          GO TO 0400-EXIT.                                         GC0025  
00561                                                                   GC0025  
00562      SET SLOT1-INDEX TO +1.                                       GC0025  
00563      SET SLOT2-INDEX TO +1.                                       GC0025  
00564      MOVE SPACE TO DETAIL-1.                                      GC0025  
00565      PERFORM 0410-LOAD-PRINT THRU 0410-EXIT 3 TIMES.              GC0025  
00566      PERFORM 0220-PRINT-LINE THRU 0220-EXIT.                      GC0025  
00567                                                                   GC0025  
00568  0400-EXIT.                                                       GC0025  
00569       EXIT.                                                       GC0025  
00570 /                                                                 GC0025  
00571  0410-LOAD-PRINT.                                                 GC0025  
00572                                                                   GC0025  
00573      IF  DEL-ELIG-CODE (DEL-INDEX) NOT = HIGH-VALUE               GC0025  
00574          MOVE DEL-ELIG-CODE (DEL-INDEX)                           GC0025  
00575            TO DT1-ID1 (SLOT1-INDEX)                               GC0025  
00576          MOVE DEL-SLOT-NUMBER (DEL-INDEX)                         GC0025  
00577            TO DT1-SLOT1 (SLOT1-INDEX)                             GC0025  
00578          SET DEL-INDEX UP BY +1                                   GC0025  
00579          SET SLOT1-INDEX UP BY +1.                                GC0025  
00580                                                                   GC0025  
00581      IF  ADD-ELIG-CODE (ADD-INDEX) NOT = HIGH-VALUE               GC0025  
00582          MOVE ADD-ELIG-CODE (ADD-INDEX)                           GC0025  
00583            TO DT1-ID2 (SLOT2-INDEX)                               GC0025  
00584          MOVE ADD-SLOT-NUMBER (ADD-INDEX)                         GC0025  
00585            TO DT1-SLOT2 (SLOT2-INDEX)                             GC0025  
00586          SET ADD-INDEX UP BY +1                                   GC0025  
00587          SET SLOT2-INDEX UP BY +1.                                GC0025  
00588                                                                   GC0025  
00589  0410-EXIT.                                                       GC0025  
00590       EXIT.                                                       GC0025  
00591 /                                                                 GC0025  
00592  9999-ERROR-RTN.                                                  GC0025  
00593                                                                   GC0025  
00594       CALL    'TSGEND'     USING      ABEND-CODE.                 GC0025  
00595                                                                   GC0025  
00596  9999-EXIT.                                                       GC0025  
00597       EXIT.                                                       GC0025  
