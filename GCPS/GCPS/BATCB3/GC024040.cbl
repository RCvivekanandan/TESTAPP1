000100 IDENTIFICATION DIVISION.                                         12/09/02
000200 PROGRAM-ID.    GC024040.                                         GC024040
000300 AUTHOR.        ED WITKUS.                                           LV002
000400 INSTALLATION.  HEALTH CARE SERVICE CORPORATION.                  GC024040
000500 DATE-WRITTEN.  SEPTEMBER, 1987.                                  GC024040
000600 DATE-COMPILED.                                                   GC024040
000700******************************************************************GC024040
000800*** PROGRAM LOGIC FLOW.                                           GC024040
000900***                                                               GC024040
001000*** READ CONTRACT SLOT UPDATE FILE.                               GC024040
001100*** IF INPUT RECORD HAS AN #AOL TABULAR CODED                     GC024040
001200***     READ THE TABULAR FILE USING THAT KEY                      GC024040
001300***     IF ANY BENEFIT-PERIOD-IND = '0D'                          GC024040
001400***         THEN FOR EACH #ADL RECORD THAT IS ATTACHED TO         GC024040
001500***         THE CURRENT CONTRACT RECORD OR TO ANY GROUP           GC024040
001600***         SPECIFIC RECORD WHICH HAS THE SAME GROUP, SECTION     GC024040
001700***         ALL BENEFIT-PERIOD-IND MUST = '0D'.                   GC024040
001800***                                                               GC024040
001900*** PERFORM THE SAME EDIT FOR THE VALUE '0E'.                     GC024040
002000*** ALSO PERFORM THE VICE-VERSA OF THIS EDIT, THAT IS:            GC024040
002100***                                                               GC024040
002200*** IF INPUT RECORD HAS AN #ADL CODED                             GC024040
002300***     READ THE TABULAR FILE USING THAT KEY                      GC024040
002400***     IF ANY BENEFIT-PERIOD-IND = '0D'                          GC024040
002500***         THEN FOR EACH #AOL RECORD THAT IS ATTACHED TO         GC024040
002600***         THE CURRENT CONTRACT RECORD OR TO ANY GROUP           GC024040
002700***         SPECIFIC RECORD WHICH HAS THE SAME GROUP, SECTION     GC024040
002800***         ALL BENEFIT-PERIOD-IND MUST = '0D'.                   GC024040
002900***                                                               GC024040
003000******************************************************************GC024040
003100***************************************************************** GC024040
003200*    GC24040A IS THE CONTRACT SLOT UPDATE FILE.                   GC024040
003300*    TSGVSAM1 IS THE TABULAR FILE.                                GC024040
003400*    TSGVSAM2 IS THE GROUP SPECIFIC FILE.                         GC024040
003500*                                                                 GC024040
003600******************************************************************GC024040
003700***************************************************************** GC024040
003800*                                                                 GC024040
003900*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       GC024040
004000*       *-*         U P D A T E   H I S T O R Y         *-*       GC024040
004100*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       GC024040
004200*                                                                 GC024040
004300* LOG-NBR    DATE    WHO  ----------   DESCRIPTION   -------------GC024040
004400* -------  --------  ---  ----------------------------------------GC024040
004500* D146     09/01/87  ENW  CREATED.                                GC024040
004600* 11154     3/20/91  FRY  INCREASE RECORD AREA IN WORKING STORAGE*GC024040
004700*                  TSGVSAM1  -TABULAR FILE.                      *GC024040
004800*                  ONEA-REC-FILLER  PIC X(3846) CHANGED TO  7765.*GC024040
004900*                                                                *GC024040
005000* D12009   09/10/91  GDM  INCREASE DET1-FRL1                     *GC024040
005100*                                  DET1-FRL2 TO 2 POSITIONS      *GC024040
005200*                                                                *GC024040
005300*           1/17/95  EMS   CONVERTED TO COBOL II.                *GC024040
005400*                                                                *GC024040
005500* 14726/15057                                                    *GC024040
005600*         10/23/97 AB   ADDED CHANGES      TO SUPPORT THE YEAR   *GC024040
005700*                       2000 AND THE EXPANSION OF THE GROUP      *GC024040
005800*                       SPECIFIC AND CONTRACT KEY TO SUPPORT THE *GC024040
005900*                       TEXAS MERGER.                            *GC024040
006000*                                                                *GC024040
006100* 15182    12/02/98  FRY  REFERENCE THE CORRECT POSITION ON THE  *GC024040
006200*                         CONTRACT RECORD FOR THE ACCUMS.        *GC024040
006300*                            CHANGE POSITION OF #ADL TABULAR     *GC024040
006400*                            FROM:  7      TO:  8                *GC024040
006500*                            CHANGE POSITION OF #AOL TABULAR     *GC024040
006600*                            FROM:  9      TO:  10               *GC024040
006700*                                                                *GC024040
006800* P FIX    04/14/99  KIKI DECREASED THE LENGTH OF WS-HDR1,       *GC024040
006900*                         WS-HDR2 AND WS-HDR3 TO PRINT THE       *GC024040
007000*                         FULL CENTURY/YEAR, PREVIOUSLY THE      *GC024040
007100*                         YEAR WAS DROPPED.                      *GC024040
007200*                                                                *GC024040
007300*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC024040
007400*                                                                *GC024040
007410*  DM09441   09-09-09   DNK   ADJUSTED THE LENGTH OF THE         *GC024040
007420*                             TABULAR RECORD AREA FOR EXPANSION. *GC024040
007430*                                                                *GC024040
ED0624* BBDA-58217 06/04/24   ED    RECOMPILE FOR PEAQ COPYBOOK        *        
ED0624*                             EXPANSION:                         *        
ED0624*                                   COPYBKS - GCTABM*, GCTACL*,  *        
ED0624*                                   GCTACP*,  GCTADL*, GCTADL*   *        
007500***************************************************************** GC024040
007600*                                                                 GC024040
007700 ENVIRONMENT DIVISION.                                            GC024040
007800 CONFIGURATION SECTION.                                           GC024040
007900 SOURCE-COMPUTER. IBM-370.                                        GC024040
008000 OBJECT-COMPUTER. IBM-370.                                        GC024040
008100 INPUT-OUTPUT SECTION.                                            GC024040
008200 FILE-CONTROL.                                                    GC024040
008300     SELECT CONTRACT-FILE ASSIGN TO UT-S-GC24040A.                GC024040
008400                                                                  GC024040
008500 DATA DIVISION.                                                   GC024040
008600 FILE SECTION.                                                    GC024040
008700 FD  CONTRACT-FILE                                                GC024040
008800     BLOCK CONTAINS  0  RECORDS                                   GC024040
008900     RECORDING MODE IS V                                          GC024040
009000     LABEL RECORDS ARE STANDARD.                                  GC024040
009100 01  CONTRACT-REC.                                                GC024040
009200     COPY GCWRKDCC.                                               GC024040
009300     COPY GCCONTRC.                                               GC024040
009400                                                                  GC024040
009500/                                                                 GC024040
009600 WORKING-STORAGE SECTION.                                         GC024040
009700                                                                  GC024040
009800 01  FILLER            PIC X(24) VALUE                            GC024040
009900     'GC024040 WORKING STORAGE'.                                  GC024040
010000                                                                  GC024040
010100 01  MISC-WORK.                                                   GC024040
010200     05  ABEND-CODE           PIC S9999 COMP VALUE ZERO.          GC024040
010300     05  WS-CHECK-VALUE       PIC X(02) VALUE SPACES.             GC024040
010400     05  H-BEN-PER            PIC X(02) VALUE SPACES.             GC024040
010500     05  WS-LINE-CNT          PIC 9(3)  VALUE 80.                 GC024040
010600     05  WS-PAGE-CNT          PIC 9(4)  VALUE ZERO.               GC024040
010700     05  WS-DISP-EFF-DT       PIC 9(05) VALUE ZEROS.              GC024040
010800     05  WS-UNPK-SLOT         PIC 9(07) VALUE ZEROS.              GC024040
010900     05  WS-DISP-TAB-KEY.                                         GC024040
011000         10  WS-DISP-TAB-ID   PIC X(06) VALUE SPACES.             GC024040
011100         10  WS-DISP-TAB-SLOT PIC 9(07) VALUE ZEROS.              GC024040
011200     05  WS-JUL-DATE          PIC 9(05) VALUE ZEROS.              GC024040
011300     05  WS-GREG-DATE.                                            GC024040
011400         10  WS-GREG-DD       PIC 9(02) VALUE ZEROS.              GC024040
011500         10  WS-GREG-MM       PIC 9(02) VALUE ZEROS.              GC024040
011600         10  WS-GREG-YY       PIC 9(02) VALUE ZEROS.              GC024040
011700     05  WS-DATE-AREA.                                            GC024040
011800         10  WS-MDY.                                              GC024040
011900             15  WS-M            PIC 99    VALUE ZERO.            GC024040
012000             15  WS-D            PIC 99    VALUE ZERO.            GC024040
012100             15  WS-Y            PIC 9(4)  VALUE ZERO.            GC024040
012200     05  WS-TIME.                                                 GC024040
012300         10  HDR3-HH         PIC 99      VALUE ZEROS.             GC024040
012400         10  HDR3-MM         PIC 99      VALUE ZEROS.             GC024040
012500         10  HDR3-SS         PIC 99      VALUE ZEROS.             GC024040
012600         10  FILLER          PIC 99      VALUE ZEROS.             GC024040
012700/                                                                 GC024040
012800 01  RIP-AREA.                                                    GC024040
012900     05  RIP-STACKER          PIC X(8)   VALUE 'RIP001'.          GC024040
013000     05  PRINT-AREA.                                              GC024040
013100         10  PRINT-CC         PIC X      VALUE SPACES.            GC024040
013200         10  PRINT-LINE       PIC X(132) VALUE SPACES.            GC024040
013300                                                                  GC024040
013400     COPY RIPHDTR.                                                GC024040
013500/                                                                 GC024040
013600     COPY MLDATE01.                                               GC024040
013700                                                                  GC024040
013800/                                                                 GC024040
013900 01  WS-SWITCHES.                                                 GC024040
014000     05  WS-ADL-SW             PIC 9    VALUE ZERO.               GC024040
014100         88  ADL-NOT-FOUND              VALUE 0.                  GC024040
014200         88  ADL-FOUND                  VALUE 1.                  GC024040
014300     05  WS-AOL-SW             PIC 9    VALUE ZERO.               GC024040
014400         88  AOL-NOT-FOUND              VALUE 0.                  GC024040
014500         88  AOL-FOUND                  VALUE 1.                  GC024040
014600     05  WS-CON-EOF-SW         PIC 9    VALUE ZERO.               GC024040
014700         88  CON-EOF                    VALUE 1.                  GC024040
014800     05  WS-ERROR-SW           PIC 9    VALUE ZERO.               GC024040
014900         88  ERROR-FOUND                VALUE 1.                  GC024040
015000     05  WS-GROUP-EOP-SW       PIC 9    VALUE ZERO.               GC024040
015100         88  GROUP-IN-PROCESS           VALUE 0.                  GC024040
015200         88  GROUP-END-O-PROCESS        VALUE 1.                  GC024040
015300     05  WS-GROUP-PRINT-SW     PIC 9    VALUE ZERO.               GC024040
015400     05  WS-GRP-EOF-SW         PIC 9    VALUE ZERO.               GC024040
015500         88  GRP-EOF                    VALUE 1.                  GC024040
015600     05  WS-PROCESS-SW         PIC 9    VALUE ZERO.               GC024040
015700         88  PROCESS-AOL-VS-ADL         VALUE 1.                  GC024040
015800         88  PROCESS-ADL-VS-AOL         VALUE 2.                  GC024040
015900     05  WS-REPORT-SW          PIC 9    VALUE ZERO.               GC024040
016000         88  NO-REPORT                  VALUE 0.                  GC024040
016100     05  WS-SKIP-SW            PIC 9    VALUE ZERO.               GC024040
016200     05  WS-VALUE-FOUND-SW     PIC 9    VALUE ZERO.               GC024040
016300         88  VALUE-NOT-FOUND            VALUE 0.                  GC024040
016400         88  VALUE-FOUND                VALUE 1.                  GC024040
016500                                                                  GC024040
016600 01  WS-PRINT-HEADINGS.                                           GC024040
016700     05  WS-HDR1.                                                 GC024040
016800         10  FILLER              PIC X       VALUE '1'.           GC024040
016900         10  FILLER              PIC X(14)   VALUE                GC024040
017000             'PGM # GC024040'.                                    GC024040
017100         10  FILLER              PIC X(36)   VALUE SPACES.        GC024040
017200         10  FILLER              PIC X(31)   VALUE                GC024040
017300             'HEALTH CARE SERVICE CORPORATION'.                   GC024040
017400         10  FILLER              PIC X(32)   VALUE SPACES.        GC024040
017500         10  FILLER              PIC X(5)    VALUE 'PAGE:'.       GC024040
017600         10  FILLER              PIC X(6)    VALUE SPACES.        GC024040
017700         10  HDR1-PG-NO          PIC ZZZZ9.                       GC024040
017800                                                                  GC024040
017900     05  WS-HDR2.                                                 GC024040
018000         10  FILLER              PIC X       VALUE SPACES.        GC024040
018100         10  FILLER              PIC X(6)    VALUE 'RPT # '.      GC024040
018200         10  HDR2-REPORT-NO      PIC X(6)    VALUE ' 1176 '.      GC024040
018300         10  FILLER              PIC X(37)   VALUE SPACES.        GC024040
018400         10  FILLER              PIC X(34)   VALUE                GC024040
018500             'GENERIC CONTRACT PROCESSING SYSTEM'.                GC024040
018600         10  FILLER              PIC X(30)   VALUE SPACES.        GC024040
018700         10  FILLER              PIC X(6)    VALUE 'DATE: '.      GC024040
018800         10  HDR2-MM-DD-YY.                                       GC024040
018900             15  HDR2-MM         PIC 99      VALUE ZEROS.         GC024040
019000             15  FILLER          PIC X       VALUE '/'.           GC024040
019100             15  HDR2-DD         PIC 99      VALUE ZEROS.         GC024040
019200             15  FILLER          PIC X       VALUE '/'.           GC024040
019300             15  HDR2-YYYY       PIC 9999    VALUE ZEROS.         GC024040
019400                                                                  GC024040
019500     05  WS-HDR3.                                                 GC024040
019600         10  FILLER              PIC X       VALUE SPACES.        GC024040
019700         10  FILLER              PIC X(34)   VALUE                GC024040
019800             'DEPT: 917  LOCATION: SSD 7TH FLOOR'.                GC024040
019900         10  FILLER              PIC X(14)   VALUE SPACES.        GC024040
020000         10  FILLER              PIC X(36)   VALUE                GC024040
020100             'INCONSISTENT ADL/AOL BENEFIT PERIODS'.              GC024040
020200         10  FILLER              PIC X(17)   VALUE                GC024040
020300             ' - CONTRACT FILE '.                                 GC024040
020400         10  FILLER              PIC X(12)   VALUE SPACES.        GC024040
020500         10  FILLER              PIC X(6)    VALUE 'TIME: '.      GC024040
020600         10  HDR3-HH-MM-SS.                                       GC024040
020700             15  HDR3-HH         PIC 99      VALUE ZEROS.         GC024040
020800             15  FILLER          PIC X       VALUE ':'.           GC024040
020900             15  HDR3-MM         PIC 99      VALUE ZEROS.         GC024040
021000             15  FILLER          PIC X       VALUE ':'.           GC024040
021100             15  HDR3-SS         PIC 99      VALUE ZEROS.         GC024040
021200                                                                  GC024040
021300     05  WS-CON-GRP-COL-HDR1.                                     GC024040
021400         10  FILLER              PIC X(01)   VALUE '0'.           GC024040
021500         10  FILLER              PIC X(42) VALUE                  GC024040
021600             '-------------- CONTRACT KEY --------------'.        GC024040
021700         10  FILLER              PIC X(03) VALUE SPACES.          GC024040
021800         10  FILLER              PIC X(13) VALUE                  GC024040
021900             '-TABULAR KEY-'.                                     GC024040
022000         10  FILLER              PIC X(02) VALUE SPACES.          GC024040
022100         10  FILLER              PIC X(07) VALUE                  GC024040
022200             'BEN-PER'.                                           GC024040
022300         10  FILLER              PIC X(03) VALUE SPACES.          GC024040
022400         10  FILLER              PIC X(34) VALUE                  GC024040
022500             '------- GROUP SPECIFIC KEY -------'.                GC024040
022600         10  FILLER              PIC X(03) VALUE SPACES.          GC024040
022700         10  FILLER              PIC X(13) VALUE                  GC024040
022800             '-TABULAR KEY-'.                                     GC024040
022900         10  FILLER              PIC X(02) VALUE SPACES.          GC024040
023000         10  FILLER              PIC X(07) VALUE                  GC024040
023100             'BEN-PER'.                                           GC024040
023200                                                                  GC024040
023300     05  WS-CON-GRP-COL-HDR2.                                     GC024040
023400         10  FILLER              PIC X(01)   VALUE ' '.           GC024040
023500         10  FILLER              PIC X(42) VALUE                  GC024040
023600             'PLN GROUP     SECTN PKG LOB PRV FRL EFF-DT'.        GC024040
023700         10  FILLER              PIC X(03) VALUE SPACES.          GC024040
023800         10  FILLER              PIC X(13) VALUE                  GC024040
023900             'ID.      SLOT'.                                     GC024040
024000         10  FILLER              PIC X(04) VALUE SPACES.          GC024040
024100         10  FILLER              PIC X(03) VALUE                  GC024040
024200             'IND'.                                               GC024040
024300         10  FILLER              PIC X(05) VALUE SPACES.          GC024040
024400         10  FILLER              PIC X(34) VALUE                  GC024040
024500             'PLN GROUP     SECTN PKG FRL EFF-DT'.                GC024040
024600         10  FILLER              PIC X(03) VALUE SPACES.          GC024040
024700         10  FILLER              PIC X(13) VALUE                  GC024040
024800             'ID.      SLOT'.                                     GC024040
024900         10  FILLER              PIC X(04) VALUE SPACES.          GC024040
025000         10  FILLER              PIC X(03) VALUE                  GC024040
025100             'IND'.                                               GC024040
025200         10  FILLER              PIC X(4) VALUE SPACES.           GC024040
025300                                                                  GC024040
025400     05  WS-CON-GRP-DET1.                                         GC024040
025500         10  FILLER              PIC X(01)   VALUE ' '.           GC024040
025600         10  DET1-PLAN-CODE1     PIC X(3).                        GC024040
025700         10  FILLER              PIC X(01)   VALUE SPACES.        GC024040
025800         10  DET1-GRP1           PIC X(09).                       GC024040
025900         10  FILLER              PIC X(01)   VALUE SPACES.        GC024040
026000         10  DET1-SECTN1         PIC X(05).                       GC024040
026100         10  FILLER              PIC X(01)   VALUE SPACES.        GC024040
026200         10  DET1-PKG-CODE1      PIC X(3).                        GC024040
026300         10  FILLER              PIC X(02)   VALUE SPACES.        GC024040
026400         10  DET1-LOB1           PIC X(01).                       GC024040
026500         10  FILLER              PIC X(02)   VALUE SPACES.        GC024040
026600         10  DET1-PVC1           PIC X(02).                       GC024040
026700         10  FILLER              PIC X(03)   VALUE SPACES.        GC024040
026800         10  DET1-FRL1           PIC X(02).                       GC024040
026900         10  FILLER              PIC X(02)   VALUE SPACES.        GC024040
027000         10  DET1-DATE1.                                          GC024040
027100             15  DET1-MM1        PIC X(02).                       GC024040
027200             15  DET1-DD1        PIC X(02).                       GC024040
027300             15  DET1-YY1        PIC X(02).                       GC024040
027400         10  FILLER              PIC X(03)   VALUE SPACES.        GC024040
027500         10  DET1-TAB1.                                           GC024040
027600             15  DET1-TAB1-ID    PIC X(06).                       GC024040
027700             15  DET1-TAB1-SLOT  PIC Z(07).                       GC024040
027800         10  FILLER              PIC X(04)   VALUE SPACES.        GC024040
027900         10  DET1-BEN-PER1       PIC X(02).                       GC024040
028000         10  FILLER              PIC X(06)   VALUE SPACES.        GC024040
028100         10  DET1-PLAN-CODE2     PIC X(3).                        GC024040
028200         10  FILLER              PIC X(01)   VALUE SPACES.        GC024040
028300         10  DET1-GRP2           PIC X(09).                       GC024040
028400         10  FILLER              PIC X(01)   VALUE SPACES.        GC024040
028500         10  DET1-SECTN2         PIC X(05).                       GC024040
028600         10  FILLER              PIC X(01)   VALUE SPACES.        GC024040
028700         10  DET1-PKG-CODE2      PIC X(3).                        GC024040
028800         10  FILLER              PIC X(02)   VALUE SPACES.        GC024040
028900         10  DET1-FRL2           PIC X(02).                       GC024040
029000         10  FILLER              PIC X(02)   VALUE SPACES.        GC024040
029100         10  DET1-DATE2.                                          GC024040
029200             15  DET1-MM2        PIC X(02).                       GC024040
029300             15  DET1-DD2        PIC X(02).                       GC024040
029400             15  DET1-YY2        PIC X(02).                       GC024040
029500         10  FILLER              PIC X(03)   VALUE SPACES.        GC024040
029600         10  DET1-TAB2.                                           GC024040
029700             15  DET1-TAB2-ID    PIC X(06).                       GC024040
029800             15  DET1-TAB2-SLOT  PIC Z(07).                       GC024040
029900         10  FILLER              PIC X(04)   VALUE SPACES.        GC024040
030000         10  DET1-BEN-PER2       PIC X(02).                       GC024040
030100/                                                                 GC024040
030200     05  WS-NO-RECORD-MSG.                                        GC024040
030300         10  FILLER              PIC X(01)   VALUE '0'.           GC024040
030400         10  FILLER              PIC X(67)   VALUE                GC024040
030500         '*** NO ERROR CONDITIONS ENCOUNTERED WITH CONTRACT ADL/AOGC024040
030600-        'L EDITS ***'.                                           GC024040
030700         10  FILLER              PIC X(65)   VALUE SPACES.        GC024040
030800/                                                                 GC024040
030900 01  PARM-SET.                                                    GC024040
031000     05  SET-RDW.                                                 GC024040
031100         10  SET-REC-LENG    PIC 9(2)  COMP VALUE ZEROS.          GC024040
031200         10  SET-FEEDBACK    PIC 9(2)  COMP VALUE ZEROS.          GC024040
031300     05  SET-VALUE           PIC 9(5)  COMP.                      GC024040
031400                                                                  GC024040
031500/                                                                 GC024040
031600 01  PARM-ONE.                                                    GC024040
031700     05  RESERVED-FLDS-1     PIC 9(5)  COMP  VALUE ZEROS.         GC024040
031800     05  RESERVED-X-1 REDEFINES RESERVED-FLDS-1.                  GC024040
031900         10  REQUEST-TYPE-1  PIC X.                               GC024040
032000                                                                  GC024040
032100 01  PARM-ONEA.                                                   GC024040
032200     02  ONEA-RDW.                                                GC024040
032300         05  ONEA-REC-LEN    PIC 9(2)  COMP VALUE ZEROS.          GC024040
032400         05  ONEA-FEEDBACK   PIC 9(2)  COMP VALUE ZEROS.          GC024040
032500     02  ONEA-REC-AREA.                                           GC024040
032600         05  ONEA-REC-KEY.                                        GC024040
032700             10  ONEA-REC-ID   PIC X(6).                          GC024040
032800             10  ONEA-REC-SLOT PIC S9(7) COMP-3  VALUE ZEROS.     GC024040
032900         05  FILLER            PIC X(27)   VALUE LOW-VALUES.      GC024040
033000         05  ONEA-REC-CNT      PIC S9(5) COMP-3  VALUE ZEROS.     GC024040
033100         05  ONEA-REC-FILLER   PIC X(31330)      VALUE LOW-VALUES.GC024040
033200                                                                  GC024040
033300/                                                                 GC024040
033400 01  PARM-TWO.                                                    GC024040
033500     05  RESERVED-FLDS-2     PIC 9(5)  COMP VALUE ZEROS.          GC024040
033600     05  RESERVED-X-2 REDEFINES RESERVED-FLDS-2.                  GC024040
033700         10  REQUEST-TYPE-2  PIC X.                               GC024040
033800                                                                  GC024040
033900 01  PARM-TWOA.                                                   GC024040
034000     02  TWOA-RDW.                                                GC024040
034100         05  TWOA-REC-LEN    PIC 9(2)  COMP  VALUE ZEROS.         GC024040
034200         05  TWOA-FEEDBACK   PIC 9(2)  COMP  VALUE ZEROS.         GC024040
034300     02  TWOA-REC-AREA.                                           GC024040
034400         COPY GCGROUPC.                                           GC024040
034500/                                                                 GC024040
034600 01  WS-ADL-REC.                                                  GC024040
034700     COPY GCTADLC.                                                GC024040
034800/                                                                 GC024040
034900 01  WS-AOL-REC.                                                  GC024040
035000     COPY GCTAOLC.                                                GC024040
035100/                                                                 GC024040
035200     COPY HSCDATES.                                               GC024040
035300/                                                                 GC024040
035400 PROCEDURE DIVISION.                                              GC024040
035500*    READY TRACE.                                                 GC024040
035600 0100-PROCESS.                                                    GC024040
035700                                                                  GC024040
035800     PERFORM 0100-005-OPEN-FILES THRU 0100-005-EXIT.              GC024040
035900                                                                  GC024040
036000     PERFORM 0100-010-READ THRU 0100-010-EXIT                     GC024040
036100         UNTIL CON-EOF.                                           GC024040
036200                                                                  GC024040
036300     IF WS-REPORT-SW = 0                                          GC024040
036400         PERFORM R0130-PRINT-NO-RECORDS-MSG THRU R0130-EXIT.      GC024040
036500                                                                  GC024040
036600     PERFORM 0900-CLOSE-FILES THRU 0900-EXIT.                     GC024040
036700                                                                  GC024040
036800     STOP RUN.                                                    GC024040
036900                                                                  GC024040
037000 0100-005-OPEN-FILES.                                             GC024040
037100                                                                  GC024040
037200     MOVE 'S'                TO REQUEST-TYPE-1.                   GC024040
037300     MOVE 8                  TO SET-REC-LENG.                     GC024040
037400     MOVE 3                  TO SET-VALUE.                        GC024040
037500     CALL 'TSGVSAM1' USING PARM-ONE PARM-SET.                     GC024040
037600     IF  REQUEST-TYPE-1 NOT EQUAL 'S'                             GC024040
037700         DISPLAY 'TSGVSAM1 SET ERROR'                             GC024040
037800         MOVE SET-FEEDBACK   TO ABEND-CODE                        GC024040
037900         GO TO 99999-ERROR-RTN.                                   GC024040
038000     MOVE 'S'                TO REQUEST-TYPE-2.                   GC024040
038100     MOVE 8                  TO SET-REC-LENG.                     GC024040
038200     MOVE 3                  TO SET-VALUE.                        GC024040
038300     CALL 'TSGVSAM2' USING PARM-TWO PARM-SET.                     GC024040
038400     IF  REQUEST-TYPE-2 NOT EQUAL 'S'                             GC024040
038500         DISPLAY 'TSGVSAM2 SET ERROR'                             GC024040
038600         MOVE SET-FEEDBACK   TO ABEND-CODE                        GC024040
038700         GO TO 99999-ERROR-RTN.                                   GC024040
038800                                                                  GC024040
038900     OPEN INPUT  CONTRACT-FILE.                                   GC024040
039000                                                                  GC024040
039100     MOVE    'TDY'               TO  MLDATE-FUNC.                 GC024040
039200     MOVE    'M'                 TO  MLDATE-FORM1.                GC024040
039300     CALL    'MLDATE' USING MLDATE01.                             GC024040
039400     MOVE    MLDATE-DATE1        TO  WS-MDY.                      GC024040
039500     MOVE WS-M                   TO HDR2-MM.                      GC024040
039600     MOVE WS-D                   TO HDR2-DD.                      GC024040
039700     MOVE WS-Y                   TO HDR2-YYYY.                    GC024040
039800                                                                  GC024040
039900     ACCEPT WS-TIME FROM TIME.                                    GC024040
040000     MOVE CORR WS-TIME TO HDR3-HH-MM-SS.                          GC024040
040100                                                                  GC024040
040200     MOVE '1176' TO TECH-HDR-RPT-NO                               GC024040
040300                    TECH-TRL-RPT-NO.                              GC024040
040400     MOVE 'CONTRACT INCONST ADL/AOL RPT' TO TECH-HDR-RPT-NAME.    GC024040
040500     MOVE HR-TECH-HEADER TO PRINT-AREA.                           GC024040
040600     CALL 'RIPWRITE' USING  PRINT-AREA  RIP-STACKER.              GC024040
040700 0100-005-EXIT.                                                   GC024040
040800     EXIT.                                                        GC024040
040900/                                                                 GC024040
041000 0100-010-READ.                                                   GC024040
041100                                                                  GC024040
041200     READ CONTRACT-FILE AT END                                    GC024040
041300         MOVE 1 TO WS-CON-EOF-SW                                  GC024040
041400         GO TO 0100-010-EXIT.                                     GC024040
041500                                                                  GC024040
041600     MOVE +44    TO GAC-ENTRY-COUNT                               GC024040
041700                    GAD-ENTRY-COUNT.                              GC024040
041800                                                                  GC024040
041900     MOVE SPACES TO WS-ADL-REC                                    GC024040
042000                    WS-AOL-REC                                    GC024040
042100                    PRINT-AREA.                                   GC024040
042200                                                                  GC024040
042300*****  CHECK #AOL VS. #ADL                                        GC024040
042400     MOVE 1 TO WS-PROCESS-SW.                                     GC024040
042500     SET GCT-TAB-INDEX TO +10.                                    GC024040
042600     IF GCT-CON-TAB-ID (GCT-TAB-INDEX) = '#AOL  '                 GC024040
042700       AND                                                        GC024040
042800        GCT-CON-TAB-SLOT (GCT-TAB-INDEX) > ZEROS                  GC024040
042900         MOVE GCT-CON-TAB-ID-SLOT (GCT-TAB-INDEX) TO ONEA-REC-KEY GC024040
043000         PERFORM R0020-READ-TABULAR THRU R0020-EXIT               GC024040
043100         MOVE '0D' TO WS-CHECK-VALUE                              GC024040
043200         PERFORM 0100-020-AOL-RTN THRU 0100-020-EXIT              GC024040
043300         MOVE '0E' TO WS-CHECK-VALUE                              GC024040
043400         PERFORM 0100-020-AOL-RTN THRU 0100-020-EXIT.             GC024040
043500                                                                  GC024040
043600***** NOW THE REVERSE (VICE-VERSA) #ADL VS. #AOL.                 GC024040
043700     MOVE 2 TO WS-PROCESS-SW.                                     GC024040
043800     SET GCT-TAB-INDEX TO +8.                                     GC024040
043900     IF GCT-CON-TAB-ID (GCT-TAB-INDEX) = '#ADL  '                 GC024040
044000       AND                                                        GC024040
044100        GCT-CON-TAB-SLOT (GCT-TAB-INDEX) > ZEROS                  GC024040
044200         MOVE GCT-CON-TAB-ID-SLOT (GCT-TAB-INDEX) TO ONEA-REC-KEY GC024040
044300         PERFORM R0020-READ-TABULAR THRU R0020-EXIT               GC024040
044400         MOVE '0D' TO WS-CHECK-VALUE                              GC024040
044500         PERFORM 0100-030-ADL-RTN THRU 0100-030-EXIT              GC024040
044600         MOVE '0E' TO WS-CHECK-VALUE                              GC024040
044700         PERFORM 0100-030-ADL-RTN THRU 0100-030-EXIT.             GC024040
044800                                                                  GC024040
044900*************************                                         GC024040
045000***** USER WANTS A BLANK LINE AFTER BREAK ON GROUP.               GC024040
045100***** NOTE: AT LEAST TWO LINES WILL BE PRINTED FOR EACH           GC024040
045200*****       ERROR CONDITION, SO CHECK THE LINE-CNT TO BE          GC024040
045300*****       SURE BOTH WILL BE PRINTED TOGETHER. (WS-LINE-CNT < 59)GC024040
045400*************************                                         GC024040
045500     IF WS-SKIP-SW = ZERO                                         GC024040
045600         GO TO 0100-010-EXIT.                                     GC024040
045700     MOVE ZERO TO WS-SKIP-SW.                                     GC024040
045800     MOVE SPACES TO PRINT-AREA.                                   GC024040
045900     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024040
046000     ADD 1 TO WS-LINE-CNT.                                        GC024040
046100     IF WS-LINE-CNT > 59                                          GC024040
046200         MOVE 80 TO WS-LINE-CNT.                                  GC024040
046300                                                                  GC024040
046400 0100-010-EXIT.                                                   GC024040
046500     EXIT.                                                        GC024040
046600                                                                  GC024040
046700 0100-020-AOL-RTN.                                                GC024040
046800                                                                  GC024040
046900*** FIRST SEE IF TABULAR IS CODED.                                GC024040
047000                                                                  GC024040
047100     MOVE 0 TO WS-VALUE-FOUND-SW.                                 GC024040
047200     PERFORM R0030-CHECK-AOL-BEN-PER THRU R0030-EXIT              GC024040
047300         VARYING GAD-INDEX FROM 1 BY 1                            GC024040
047400         UNTIL GAD-INDEX = GAD-ENTRY-COUNT                        GC024040
047500              OR                                                  GC024040
047600               VALUE-FOUND.                                       GC024040
047700                                                                  GC024040
047800     IF VALUE-NOT-FOUND                                           GC024040
047900         GO TO 0100-020-EXIT.                                     GC024040
048000                                                                  GC024040
048100****** CHECK FOR #ADL KEY ON CONTRACT RECORD.                     GC024040
048200     MOVE 0 TO WS-ERROR-SW.                                       GC024040
048300     SET GCT-TAB-INDEX TO +8.                                     GC024040
048400     IF GCT-CON-TAB-SLOT (GCT-TAB-INDEX) > ZEROS                  GC024040
048500         MOVE GCT-CON-TAB-ID-SLOT (GCT-TAB-INDEX) TO ONEA-REC-KEY GC024040
048600         PERFORM R0020-READ-TABULAR THRU R0020-EXIT               GC024040
048700         PERFORM R0040-CHECK-ALL-ADL THRU R0040-EXIT              GC024040
048800             VARYING GAC-INDEX FROM 1 BY 1                        GC024040
048900             UNTIL GAC-INDEX = GAC-ENTRY-COUNT                    GC024040
049000           OR                                                     GC024040
049100             ERROR-FOUND                                          GC024040
049200         IF ERROR-FOUND                                           GC024040
049300             PERFORM R0100-FORMAT-CONTRACT-ERROR THRU R0100-EXIT. GC024040
049400                                                                  GC024040
049500** CHECK ALL GROUP SPECIFIC RECORDS THAT HAVE THE SAME            GC024040
049600** GROUP AND SECTION NUMBERS.                                     GC024040
049700     MOVE ZERO         TO WS-GROUP-PRINT-SW.                      GC024040
049800     MOVE GCT-PLAN-CODE   TO GCG-PLAN-CODE.                       GC024040
049900     MOVE GCT-GROUP-NUM   TO GCG-GROUP-NUM.                       GC024040
050000     MOVE GCT-SECTION-NUM TO GCG-SECTION-NUM.                     GC024040
050100     MOVE GCT-PKG-CODE    TO GCG-PKG-CODE.                        GC024040
050200     PERFORM R0060-POINT THRU R0060-EXIT.                         GC024040
050300     MOVE ZERO TO WS-GROUP-EOP-SW.                                GC024040
050400     PERFORM R0070-PROCESS-GROUP THRU R0070-EXIT                  GC024040
050500         UNTIL GROUP-END-O-PROCESS.                               GC024040
050600 0100-020-EXIT.                                                   GC024040
050700     EXIT.                                                        GC024040
050800                                                                  GC024040
050900 0100-030-ADL-RTN.                                                GC024040
051000                                                                  GC024040
051100*** FIRST SEE IF TABULAR IS CODED.                                GC024040
051200                                                                  GC024040
051300     MOVE 0 TO WS-VALUE-FOUND-SW.                                 GC024040
051400     PERFORM R0035-CHECK-ADL-BEN-PER THRU R0035-EXIT              GC024040
051500         VARYING GAC-INDEX FROM 1 BY 1                            GC024040
051600         UNTIL GAC-INDEX = GAC-ENTRY-COUNT                        GC024040
051700              OR                                                  GC024040
051800               VALUE-FOUND.                                       GC024040
051900                                                                  GC024040
052000     IF VALUE-NOT-FOUND                                           GC024040
052100         GO TO 0100-030-EXIT.                                     GC024040
052200                                                                  GC024040
052300** CHECK FOR #AOL KEY ON CONTRACT RECORD.                         GC024040
052400     MOVE 0 TO WS-ERROR-SW.                                       GC024040
052500     SET GCT-TAB-INDEX TO +10.                                    GC024040
052600     IF GCT-CON-TAB-SLOT (GCT-TAB-INDEX) > ZEROS                  GC024040
052700         MOVE GCT-CON-TAB-ID-SLOT (GCT-TAB-INDEX) TO ONEA-REC-KEY GC024040
052800         PERFORM R0020-READ-TABULAR THRU R0020-EXIT               GC024040
052900         PERFORM R0045-CHECK-ALL-AOL THRU R0045-EXIT              GC024040
053000             VARYING GAD-INDEX FROM 1 BY 1                        GC024040
053100             UNTIL GAD-INDEX = GAD-ENTRY-COUNT                    GC024040
053200           OR                                                     GC024040
053300             ERROR-FOUND                                          GC024040
053400         IF ERROR-FOUND                                           GC024040
053500             PERFORM R0100-FORMAT-CONTRACT-ERROR THRU R0100-EXIT. GC024040
053600                                                                  GC024040
053700** CHECK ALL GROUP SPECIFIC RECORDS THAT HAVE THE SAME            GC024040
053800** GROUP AND SECTION NUMBERS.                                     GC024040
053900     MOVE ZERO         TO WS-GROUP-PRINT-SW.                      GC024040
054000     MOVE GCT-PLAN-CODE   TO GCG-PLAN-CODE.                       GC024040
054100     MOVE GCT-GROUP-NUM   TO GCG-GROUP-NUM.                       GC024040
054200     MOVE GCT-SECTION-NUM TO GCG-SECTION-NUM.                     GC024040
054300     MOVE GCT-PKG-CODE    TO GCG-PKG-CODE.                        GC024040
054400     PERFORM R0060-POINT THRU R0060-EXIT.                         GC024040
054500     MOVE ZERO TO WS-GROUP-EOP-SW.                                GC024040
054600     PERFORM R0070-PROCESS-GROUP THRU R0070-EXIT                  GC024040
054700         UNTIL GROUP-END-O-PROCESS.                               GC024040
054800 0100-030-EXIT.                                                   GC024040
054900     EXIT.                                                        GC024040
055000/                                                                 GC024040
055100 R0020-READ-TABULAR.                                              GC024040
055200                                                                  GC024040
055300     MOVE 'R' TO REQUEST-TYPE-1.                                  GC024040
055400     MOVE +14 TO ONEA-REC-LEN.                                    GC024040
055500                                                                  GC024040
055600     CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC024040
055700                                                                  GC024040
055800     IF REQUEST-TYPE-1 NOT = 'R'                                  GC024040
055900         DISPLAY 'GC024040 BAD READ--R0020-READ-TABULAR    '      GC024040
056000         MOVE ONEA-REC-ID   TO WS-DISP-TAB-ID                     GC024040
056100         MOVE ONEA-REC-SLOT TO WS-DISP-TAB-SLOT                   GC024040
056200         DISPLAY 'TABULAR KEY = ' WS-DISP-TAB-KEY                 GC024040
056300         MOVE ONEA-FEEDBACK TO ABEND-CODE                         GC024040
056400         GO TO 99999-ERROR-RTN.                                   GC024040
056500                                                                  GC024040
056600     IF ONEA-REC-ID = '#ADL  '                                    GC024040
056700         MOVE ONEA-REC-CNT    TO GAC-ENTRY-COUNT                  GC024040
056800         MOVE GAC-ENTRY-COUNT TO GAC-ENTRY-COUNT                  GC024040
056900         MOVE ONEA-REC-AREA TO WS-ADL-REC                         GC024040
057000     ELSE                                                         GC024040
057100         MOVE ONEA-REC-CNT    TO GAD-ENTRY-COUNT                  GC024040
057200         MOVE GAD-ENTRY-COUNT TO GAD-ENTRY-COUNT                  GC024040
057300         MOVE ONEA-REC-AREA TO WS-AOL-REC.                        GC024040
057400                                                                  GC024040
057500 R0020-EXIT.                                                      GC024040
057600     EXIT.                                                        GC024040
057700/                                                                 GC024040
057800 R0030-CHECK-AOL-BEN-PER.                                         GC024040
057900      IF GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX) = WS-CHECK-VALUE    GC024040
058000          MOVE 1 TO WS-VALUE-FOUND-SW.                            GC024040
058100 R0030-EXIT.                                                      GC024040
058200      EXIT.                                                       GC024040
058300                                                                  GC024040
058400 R0035-CHECK-ADL-BEN-PER.                                         GC024040
058500      IF GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX) = WS-CHECK-VALUE     GC024040
058600          MOVE 1 TO WS-VALUE-FOUND-SW.                            GC024040
058700 R0035-EXIT.                                                      GC024040
058800      EXIT.                                                       GC024040
058900                                                                  GC024040
059000 R0040-CHECK-ALL-ADL.                                             GC024040
059100      IF GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX) NOT =                GC024040
059200                                               WS-CHECK-VALUE     GC024040
059300          MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX) TO H-BEN-PER   GC024040
059400          MOVE 1 TO WS-ERROR-SW.                                  GC024040
059500 R0040-EXIT.                                                      GC024040
059600      EXIT.                                                       GC024040
059700                                                                  GC024040
059800 R0045-CHECK-ALL-AOL.                                             GC024040
059900      IF GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX) NOT =               GC024040
060000                                                WS-CHECK-VALUE    GC024040
060100          MOVE GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX) TO H-BEN-PER  GC024040
060200          MOVE 1 TO WS-ERROR-SW.                                  GC024040
060300 R0045-EXIT.                                                      GC024040
060400      EXIT.                                                       GC024040
060500                                                                  GC024040
060600 R0060-POINT.                                                     GC024040
060700     MOVE 'P' TO REQUEST-TYPE-2.                                  GC024040
060800     MOVE +24  TO TWOA-REC-LEN.                                   GC024040
060900     CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC024040
061000     IF REQUEST-TYPE-2 NOT = 'P'                                  GC024040
061100         MOVE GCT-EFF-DT TO WS-DISP-EFF-DT                        GC024040
061200         DISPLAY 'GC024040 BAD POINT--R0060-POINT '               GC024040
061300              GCT-PLAN-CODE ' '                                   GC024040
061400              GCT-GROUP-NUM ' '                                   GC024040
061500              GCT-SECTION-NUM ' '                                 GC024040
061600              GCT-PKG-CODE ' '                                    GC024040
061700              GCT-L-O-B     ' '                                   GC024040
061800              GCT-PROVDR-CONTROL ' '                              GC024040
061900              GCT-FAM-REL-LVL    ' '                              GC024040
062000              WS-DISP-EFF-DT                                      GC024040
062100             MOVE TWOA-FEEDBACK TO ABEND-CODE                     GC024040
062200             GO TO 99999-ERROR-RTN.                               GC024040
062300 R0060-EXIT.                                                      GC024040
062400      EXIT.                                                       GC024040
062500                                                                  GC024040
062600 R0070-PROCESS-GROUP.                                             GC024040
062700     MOVE 1 TO WS-GROUP-PRINT-SW.                                 GC024040
062800     MOVE 'G' TO REQUEST-TYPE-2.                                  GC024040
062900     CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC024040
063000     IF REQUEST-TYPE-2  = '2'                                     GC024040
063100         MOVE 1 TO WS-GROUP-EOP-SW                                GC024040
063200         MOVE 0 TO WS-GROUP-PRINT-SW                              GC024040
063300         GO TO R0070-EXIT.                                        GC024040
063400     IF REQUEST-TYPE-2  NOT = 'G'                                 GC024040
063500         MOVE GCT-EFF-DT TO WS-DISP-EFF-DT                        GC024040
063600         DISPLAY 'GC024040 BAD GET--R0070-PROCESS '               GC024040
063700              GCT-PLAN-CODE ' '                                   GC024040
063800              GCT-GROUP-NUM ' '                                   GC024040
063900              GCT-SECTION-NUM ' '                                 GC024040
064000              GCT-L-O-B     ' '                                   GC024040
064100              GCT-PROVDR-CONTROL ' '                              GC024040
064200              GCT-FAM-REL-LVL    ' '                              GC024040
064300              WS-DISP-EFF-DT                                      GC024040
064400             MOVE TWOA-FEEDBACK TO ABEND-CODE                     GC024040
064500             GO TO 99999-ERROR-RTN.                               GC024040
064600       IF GCG-PLAN-CODE = GCT-PLAN-CODE                           GC024040
064700         AND                                                      GC024040
064800           GCG-GROUP-NUM  = GCT-GROUP-NUM                         GC024040
064900         AND                                                      GC024040
065000           GCG-SECTION-NUM = GCT-SECTION-NUM                      GC024040
065100         AND                                                      GC024040
065200           GCG-PKG-CODE = GCT-PKG-CODE                            GC024040
065300           NEXT SENTENCE                                          GC024040
065400      ELSE                                                        GC024040
065500          MOVE 1 TO WS-GROUP-EOP-SW                               GC024040
065600          MOVE 0 TO WS-GROUP-PRINT-SW                             GC024040
065700          GO TO R0070-EXIT.                                       GC024040
065800*     IF WS-PROCESS-SW = 1                                        GC024040
065900      IF PROCESS-AOL-VS-ADL                                       GC024040
066000          PERFORM R0070-010-GROUP-ADL THRU R0070-010-EXIT.        GC024040
066100*     IF WS-PROCESS-SW = 2                                        GC024040
066200      IF PROCESS-ADL-VS-AOL                                       GC024040
066300          PERFORM R0070-020-GROUP-AOL THRU R0070-020-EXIT.        GC024040
066400                                                                  GC024040
066500 R0070-EXIT.                                                      GC024040
066600      EXIT.                                                       GC024040
066700                                                                  GC024040
066800                                                                  GC024040
066900 R0070-010-GROUP-ADL.                                             GC024040
067000      MOVE 0 TO WS-ADL-SW.                                        GC024040
067100      PERFORM R0080-GROUP-ADL-LOOP THRU R0080-EXIT                GC024040
067200          VARYING GCG-INDEX FROM 1 BY 1                           GC024040
067300          UNTIL ADL-FOUND                                         GC024040
067400               OR                                                 GC024040
067500                GCG-INDEX = GCG-COUNT-TAB-PROVN-POINTERS.         GC024040
067600*    IF WS-ADL-SW = 1                                             GC024040
067700     IF ADL-FOUND                                                 GC024040
067800         MOVE 0 TO WS-ERROR-SW                                    GC024040
067900         PERFORM R0040-CHECK-ALL-ADL THRU R0040-EXIT              GC024040
068000             VARYING GAC-INDEX FROM 1 BY 1                        GC024040
068100             UNTIL GAC-INDEX = GAC-ENTRY-COUNT                    GC024040
068200           OR                                                     GC024040
068300             ERROR-FOUND                                          GC024040
068400*        IF WS-ERROR-SW = 1                                       GC024040
068500         IF ERROR-FOUND                                           GC024040
068600             PERFORM R0110-FORMAT-GROUP-ERROR THRU R0110-EXIT.    GC024040
068700 R0070-010-EXIT.                                                  GC024040
068800     EXIT.                                                        GC024040
068900                                                                  GC024040
069000                                                                  GC024040
069100 R0070-020-GROUP-AOL.                                             GC024040
069200      MOVE 0 TO WS-AOL-SW.                                        GC024040
069300      PERFORM R0085-GROUP-AOL-LOOP THRU R0085-EXIT                GC024040
069400          VARYING GCG-INDEX FROM 1 BY 1                           GC024040
069500          UNTIL AOL-FOUND                                         GC024040
069600               OR                                                 GC024040
069700                GCG-INDEX = GCG-COUNT-TAB-PROVN-POINTERS.         GC024040
069800     IF AOL-FOUND                                                 GC024040
069900         MOVE 0 TO WS-ERROR-SW                                    GC024040
070000         PERFORM R0045-CHECK-ALL-AOL THRU R0045-EXIT              GC024040
070100             VARYING GAD-INDEX FROM 1 BY 1                        GC024040
070200             UNTIL GAD-INDEX = GAD-ENTRY-COUNT                    GC024040
070300           OR                                                     GC024040
070400             ERROR-FOUND                                          GC024040
070500        IF ERROR-FOUND                                            GC024040
070600            PERFORM R0110-FORMAT-GROUP-ERROR THRU R0110-EXIT.     GC024040
070700 R0070-020-EXIT.                                                  GC024040
070800     EXIT.                                                        GC024040
070900                                                                  GC024040
071000 R0080-GROUP-ADL-LOOP.                                            GC024040
071100      IF GCG-TAB-ID (GCG-INDEX) = '#ADL  '                        GC024040
071200        AND                                                       GC024040
071300         GCG-TAB-SLOT-NO (GCG-INDEX) > ZEROS                      GC024040
071400          MOVE GCG-GRP-SPEC-TAB-ID (GCG-INDEX) TO ONEA-REC-KEY    GC024040
071500          PERFORM R0020-READ-TABULAR THRU R0020-EXIT              GC024040
071600          MOVE 1 TO WS-ADL-SW.                                    GC024040
071700 R0080-EXIT.                                                      GC024040
071800      EXIT.                                                       GC024040
071900                                                                  GC024040
072000 R0085-GROUP-AOL-LOOP.                                            GC024040
072100      IF GCG-TAB-ID (GCG-INDEX) = '#AOL  '                        GC024040
072200        AND                                                       GC024040
072300         GCG-TAB-SLOT-NO (GCG-INDEX) > ZEROS                      GC024040
072400          MOVE GCG-GRP-SPEC-TAB-ID (GCG-INDEX) TO ONEA-REC-KEY    GC024040
072500          PERFORM R0020-READ-TABULAR THRU R0020-EXIT              GC024040
072600          MOVE 1 TO WS-AOL-SW.                                    GC024040
072700 R0085-EXIT.                                                      GC024040
072800      EXIT.                                                       GC024040
072900                                                                  GC024040
073000                                                                  GC024040
073100 R0090-CONVERT-JUL-TO-GREG.                                       GC024040
073200                                                                  GC024040
073300     CALL 'TSGGREG' USING WS-JUL-DATE WS-GREG-DATE.               GC024040
073400                                                                  GC024040
073500 R0090-EXIT.                                                      GC024040
073600      EXIT.                                                       GC024040
073700                                                                  GC024040
073800 R0100-FORMAT-CONTRACT-ERROR.                                     GC024040
073900     MOVE GCT-PLAN-CODE           TO DET1-PLAN-CODE1.             GC024040
074000     MOVE GCT-GROUP-NUM           TO DET1-GRP1.                   GC024040
074100     MOVE GCT-SECTION-NUM         TO DET1-SECTN1.                 GC024040
074200     MOVE GCT-PKG-CODE            TO DET1-PKG-CODE1.              GC024040
074300     MOVE GCT-L-O-B               TO DET1-LOB1.                   GC024040
074400     MOVE GCT-PROVDR-CONTROL      TO DET1-PVC1.                   GC024040
074500     MOVE GCT-FAM-REL-LVL         TO DET1-FRL1.                   GC024040
074600     MOVE GCT-EFF-DT              TO WS-JUL-DATE.                 GC024040
074700     PERFORM R0090-CONVERT-JUL-TO-GREG THRU R0090-EXIT.           GC024040
074800     MOVE WS-GREG-DATE            TO DET1-DATE1.                  GC024040
074900*    IF WS-PROCESS-SW = 1                                         GC024040
075000     IF PROCESS-AOL-VS-ADL                                        GC024040
075100         MOVE GAD-PROVISION-ID        TO DET1-TAB1-ID             GC024040
075200         MOVE GAD-PROVISION-SLOT-NO   TO WS-UNPK-SLOT             GC024040
075300         MOVE WS-UNPK-SLOT            TO DET1-TAB1-SLOT           GC024040
075400*        MOVE GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)                GC024040
075500         MOVE WS-CHECK-VALUE          TO DET1-BEN-PER1.           GC024040
075600*    IF WS-PROCESS-SW = 2                                         GC024040
075700     IF PROCESS-ADL-VS-AOL                                        GC024040
075800         MOVE GAC-PROVISION-ID        TO DET1-TAB1-ID             GC024040
075900         MOVE GAC-PROVISION-SLOT-NO   TO WS-UNPK-SLOT             GC024040
076000         MOVE WS-UNPK-SLOT            TO DET1-TAB1-SLOT           GC024040
076100*        MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)                 GC024040
076200         MOVE WS-CHECK-VALUE          TO DET1-BEN-PER1.           GC024040
076300     MOVE SPACES TO DET1-PLAN-CODE2                               GC024040
076400                    DET1-GRP2                                     GC024040
076500                    DET1-SECTN2                                   GC024040
076600                    DET1-PKG-CODE2                                GC024040
076700                    DET1-FRL2                                     GC024040
076800                    DET1-DATE2                                    GC024040
076900                    DET1-TAB2                                     GC024040
077000                    DET1-BEN-PER2.                                GC024040
077100     PERFORM R0120-PRINT-CONTRACT-GROUP THRU R0120-EXIT.          GC024040
077200                                                                  GC024040
077300*    IF WS-PROCESS-SW = 1                                         GC024040
077400     IF PROCESS-AOL-VS-ADL                                        GC024040
077500         MOVE GAC-PROVISION-ID       TO DET1-TAB1-ID              GC024040
077600         MOVE GAC-PROVISION-SLOT-NO  TO WS-UNPK-SLOT              GC024040
077700         MOVE WS-UNPK-SLOT           TO DET1-TAB1-SLOT            GC024040
077800*        MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)                 GC024040
077900         MOVE H-BEN-PER                                           GC024040
078000                                     TO DET1-BEN-PER1.            GC024040
078100                                                                  GC024040
078200*    IF WS-PROCESS-SW = 2                                         GC024040
078300     IF PROCESS-ADL-VS-AOL                                        GC024040
078400         MOVE GAD-PROVISION-ID       TO DET1-TAB1-ID              GC024040
078500         MOVE GAD-PROVISION-SLOT-NO  TO WS-UNPK-SLOT              GC024040
078600         MOVE WS-UNPK-SLOT           TO DET1-TAB1-SLOT            GC024040
078700*        MOVE GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)                GC024040
078800         MOVE H-BEN-PER                                           GC024040
078900                                     TO DET1-BEN-PER1.            GC024040
079000                                                                  GC024040
079100     PERFORM R0120-PRINT-CONTRACT-GROUP THRU R0120-EXIT.          GC024040
079200 R0100-EXIT.                                                      GC024040
079300     EXIT.                                                        GC024040
079400                                                                  GC024040
079500 R0110-FORMAT-GROUP-ERROR.                                        GC024040
079600     MOVE GCT-PLAN-CODE           TO DET1-PLAN-CODE1.             GC024040
079700     MOVE GCT-GROUP-NUM           TO DET1-GRP1.                   GC024040
079800     MOVE GCT-SECTION-NUM         TO DET1-SECTN1.                 GC024040
079900     MOVE GCT-PKG-CODE            TO DET1-PKG-CODE1.              GC024040
080000     MOVE GCT-L-O-B               TO DET1-LOB1.                   GC024040
080100     MOVE GCT-PROVDR-CONTROL      TO DET1-PVC1.                   GC024040
080200     MOVE GCT-FAM-REL-LVL         TO DET1-FRL1.                   GC024040
080300     MOVE GCT-EFF-DT              TO WS-JUL-DATE.                 GC024040
080400     PERFORM R0090-CONVERT-JUL-TO-GREG THRU R0090-EXIT.           GC024040
080500     MOVE WS-GREG-DATE            TO DET1-DATE1.                  GC024040
080600*    IF WS-PROCESS-SW = 1                                         GC024040
080700     IF PROCESS-AOL-VS-ADL                                        GC024040
080800         MOVE GAD-PROVISION-ID        TO DET1-TAB1-ID             GC024040
080900         MOVE GAD-PROVISION-SLOT-NO   TO WS-UNPK-SLOT             GC024040
081000         MOVE WS-UNPK-SLOT            TO DET1-TAB1-SLOT           GC024040
081100*        MOVE GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)                GC024040
081200         MOVE WS-CHECK-VALUE                                      GC024040
081300                                      TO DET1-BEN-PER1.           GC024040
081400*    IF WS-PROCESS-SW = 2                                         GC024040
081500     IF PROCESS-ADL-VS-AOL                                        GC024040
081600         MOVE GAC-PROVISION-ID        TO DET1-TAB1-ID             GC024040
081700         MOVE GAC-PROVISION-SLOT-NO   TO WS-UNPK-SLOT             GC024040
081800         MOVE WS-UNPK-SLOT            TO DET1-TAB1-SLOT           GC024040
081900*        MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)                 GC024040
082000         MOVE WS-CHECK-VALUE                                      GC024040
082100                                      TO DET1-BEN-PER1.           GC024040
082200     MOVE GCG-PLAN-CODE        TO DET1-PLAN-CODE2.                GC024040
082300     MOVE GCG-GROUP-NUM        TO DET1-GRP2.                      GC024040
082400     MOVE GCG-SECTION-NUM      TO DET1-SECTN2.                    GC024040
082500     MOVE GCG-PKG-CODE         TO DET1-PKG-CODE2.                 GC024040
082600     MOVE GCG-FAM-REL-LVL      TO DET1-FRL2.                      GC024040
082700     MOVE GCG-EFF-DT           TO WS-JUL-DATE.                    GC024040
082800     PERFORM R0090-CONVERT-JUL-TO-GREG THRU R0090-EXIT.           GC024040
082900     MOVE WS-GREG-DATE          TO DET1-DATE2.                    GC024040
083000*    IF WS-PROCESS-SW = 1                                         GC024040
083100     IF PROCESS-AOL-VS-ADL                                        GC024040
083200         MOVE GAC-PROVISION-ID      TO DET1-TAB2-ID               GC024040
083300         MOVE GAC-PROVISION-SLOT-NO TO WS-UNPK-SLOT               GC024040
083400         MOVE WS-UNPK-SLOT          TO DET1-TAB2-SLOT             GC024040
083500*        MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)                 GC024040
083600         MOVE H-BEN-PER                                           GC024040
083700                                    TO DET1-BEN-PER2              GC024040
083800         PERFORM R0120-PRINT-CONTRACT-GROUP THRU R0120-EXIT.      GC024040
083900*    IF WS-PROCESS-SW = 2                                         GC024040
084000     IF PROCESS-ADL-VS-AOL                                        GC024040
084100         MOVE GAD-PROVISION-ID      TO DET1-TAB2-ID               GC024040
084200         MOVE GAD-PROVISION-SLOT-NO TO WS-UNPK-SLOT               GC024040
084300         MOVE WS-UNPK-SLOT          TO DET1-TAB2-SLOT             GC024040
084400*        MOVE GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)                GC024040
084500         MOVE H-BEN-PER                                           GC024040
084600                                    TO DET1-BEN-PER2              GC024040
084700         PERFORM R0120-PRINT-CONTRACT-GROUP THRU R0120-EXIT.      GC024040
084800 R0110-EXIT.                                                      GC024040
084900     EXIT.                                                        GC024040
085000                                                                  GC024040
085100 R0120-PRINT-CONTRACT-GROUP.                                      GC024040
085200     IF WS-LINE-CNT > 60                                          GC024040
085300         ADD 1 TO WS-PAGE-CNT                                     GC024040
085400         MOVE WS-PAGE-CNT TO HDR1-PG-NO                           GC024040
085500         MOVE WS-HDR1 TO PRINT-AREA                               GC024040
085600         CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER             GC024040
085700         MOVE WS-HDR2 TO PRINT-AREA                               GC024040
085800         CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER             GC024040
085900         MOVE WS-HDR3 TO PRINT-AREA                               GC024040
086000         CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER             GC024040
086100         MOVE WS-CON-GRP-COL-HDR1 TO PRINT-AREA                   GC024040
086200         CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER             GC024040
086300         MOVE WS-CON-GRP-COL-HDR2 TO PRINT-AREA                   GC024040
086400         CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER             GC024040
086500         MOVE 6 TO WS-LINE-CNT.                                   GC024040
086600     MOVE WS-CON-GRP-DET1 TO PRINT-AREA.                          GC024040
086700     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024040
086800     MOVE 1 TO WS-SKIP-SW,                                        GC024040
086900               WS-REPORT-SW.                                      GC024040
087000     ADD 1 TO WS-LINE-CNT.                                        GC024040
087100 R0120-EXIT.                                                      GC024040
087200     EXIT.                                                        GC024040
087300                                                                  GC024040
087400 R0130-PRINT-NO-RECORDS-MSG.                                      GC024040
087500     ADD 1 TO WS-PAGE-CNT.                                        GC024040
087600     MOVE WS-PAGE-CNT TO HDR1-PG-NO.                              GC024040
087700     MOVE WS-HDR1 TO PRINT-AREA.                                  GC024040
087800     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024040
087900     MOVE WS-HDR2 TO PRINT-AREA.                                  GC024040
088000     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024040
088100     MOVE WS-HDR3 TO PRINT-AREA.                                  GC024040
088200     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024040
088300     MOVE WS-CON-GRP-COL-HDR1 TO PRINT-AREA.                      GC024040
088400     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024040
088500     MOVE WS-CON-GRP-COL-HDR2 TO PRINT-AREA.                      GC024040
088600     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024040
088700     MOVE WS-NO-RECORD-MSG TO PRINT-AREA.                         GC024040
088800     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024040
088900 R0130-EXIT.                                                      GC024040
089000     EXIT.                                                        GC024040
089100                                                                  GC024040
089200/                                                                 GC024040
089300 0900-CLOSE-FILES.                                                GC024040
089400                                                                  GC024040
089500     MOVE 'C' TO REQUEST-TYPE-1.                                  GC024040
089600     CALL 'TSGVSAM1' USING PARM-ONE   PARM-ONEA.                  GC024040
089700     IF  REQUEST-TYPE-1 NOT = 'C'                                 GC024040
089800         DISPLAY 'BAD CLOSE TABULAR  FILE    '                    GC024040
089900         MOVE ONEA-FEEDBACK  TO ABEND-CODE                        GC024040
090000         GO TO 99999-ERROR-RTN.                                   GC024040
090100                                                                  GC024040
090200     MOVE 'C' TO REQUEST-TYPE-2.                                  GC024040
090300     CALL 'TSGVSAM2' USING PARM-TWO   PARM-TWOA.                  GC024040
090400     IF  REQUEST-TYPE-2 NOT = 'C'                                 GC024040
090500         DISPLAY 'BAD CLOSE GROUP SPECIFIC FILE '                 GC024040
090600         MOVE TWOA-FEEDBACK  TO ABEND-CODE                        GC024040
090700         GO TO 99999-ERROR-RTN.                                   GC024040
090800                                                                  GC024040
090900     CLOSE CONTRACT-FILE.                                         GC024040
091000                                                                  GC024040
091100     MOVE HR-TECH-TRAILER   TO PRINT-AREA.                        GC024040
091200     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024040
091300     CALL 'RIPFINIS'.                                             GC024040
091400 0900-EXIT. EXIT.                                                 GC024040
091500                                                                  GC024040
091600 99998-FALL-THROUGH-TRAP.                                         GC024040
091700                                                                  GC024040
091800     DISPLAY 'GC024040 - FALL-THROUGH LOGIC ERROR'.               GC024040
091900     MOVE +0001 TO ABEND-CODE.                                    GC024040
092000                                                                  GC024040
092100 99999-ERROR-RTN.                                                 GC024040
092200                                                                  GC024040
092300     CALL 'TSGEND' USING ABEND-CODE.                              GC024040
092400                                                                  GC024040
092500 99999-EXIT. EXIT.                                                GC024040
