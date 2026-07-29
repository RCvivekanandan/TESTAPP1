000100 IDENTIFICATION DIVISION.                                         12/09/02
000200 PROGRAM-ID.    GC024050.                                         GC024050
000300 AUTHOR.        ED WITKUS.                                           LV002
000400 INSTALLATION.  HEALTH CARE SERVICE CORPORATION.                  GC024050
000500 DATE-WRITTEN.  NOVEMBER 23, 1987.                                GC024050
000600 DATE-COMPILED.                                                   GC024050
000700******************************************************************GC024050
000800*** PROGRAM LOGIC FLOW.                                           GC024050
000900***                                                               GC024050
001000*** READ GROUP SPECIFIC SLOT UPDATE FILE.                         GC024050
001100*** IF INPUT RECORD HAS AN #AOL TABULAR CODED                     GC024050
001200***     READ THE TABULAR FILE USING THAT KEY                      GC024050
001300***     IF ANY BENEFIT-PERIOD-IND = '0D'                          GC024050
001400***         THEN FOR EACH #ADL RECORD THAT IS ATTACHED TO         GC024050
001500***         THE CURRENT GROUP SPECIFIC RECORD OR TO ANY           GC024050
001600***         CONTRACT RECORD WHICH HAS THE SAME GROUP, SECTION     GC024050
001700***         ALL BENEFIT-PERIOD-IND MUST = '0D'.                   GC024050
001800***                                                               GC024050
001900*** PERFORM THE SAME EDIT FOR THE VALUE '0E'.                     GC024050
002000*** ALSO PERFORM THE VICE-VERSA OF THIS EDIT, THAT IS:            GC024050
002100***                                                               GC024050
002200*** IF INPUT RECORD HAS AN #ADL CODED                             GC024050
002300***     READ THE TABULAR FILE USING THAT KEY                      GC024050
002400***     IF ANY BENEFIT-PERIOD-IND = '0D'                          GC024050
002500***         THEN FOR EACH #AOL RECORD THAT IS ATTACHED TO         GC024050
002600***         THE CURRENT GROUP SPECIFIC RECORD OR TO ANY           GC024050
002700***         CONTRACT RECORD WHICH HAS THE SAME GROUP, SECTION     GC024050
002800***         ALL BENEFIT-PERIOD-IND MUST = '0D'.                   GC024050
002900***                                                               GC024050
003000******************************************************************GC024050
003100***************************************************************** GC024050
003200*    GC24050A IS THE GROUP SPECIFIC SLOT UPDATE FILE.             GC024050
003300*    TSGVSAM1 IS THE TABULAR FILE.                                GC024050
003400*    TSGVSAM2 IS THE CONTRACT FILE.                               GC024050
003500*                                                                 GC024050
003600******************************************************************GC024050
003700***************************************************************** GC024050
003800*                                                                 GC024050
003900*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       GC024050
004000*       *-*         U P D A T E   H I S T O R Y         *-*       GC024050
004100*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       GC024050
004200*                                                                 GC024050
004300* LOG-NBR    DATE    WHO  ----------   DESCRIPTION   -------------GC024050
004400* -------  --------  ---  ----------------------------------------GC024050
004500* D146     09/01/87  ENW  CREATED.                                GC024050
004600* 11154     3/21/91  FRY  INCREASE RECORD AREA IN WORKING STORAGE*GC024050
004700*               TSGVSAM1   -TABULAR FILE.                        *GC024050
004800*               ONEA-REC-FILLER   PIC X(3846)  CHANGED TO  7765. *GC024050
004900*                                                                *GC024050
005000* D12009   09/10/91  GDM  INCREASE DET1-FRL1                     *GC024050
005100*                                  DET1-FRL2 TO 2 POSITIONS      *GC024050
005200*                                                                *GC024050
005300*           1/17/94  EMS  CONVERTED TO COBOL II.                 *GC024050
005400*                                                                *GC024050
005500*  14726/                                                         GC024050
005600*  15057     10/24/97  AB   ADDED CODE TO SUPPORT THE YEAR        GC024050
005700*                           2000 AND THE EXPANSION OF THE         GC024050
005800*                           CONTRACT KEY TO SUPPORT THE TX        GC024050
005900*                           MERGER.                               GC024050
006000*                                                                *GC024050
006100* 15182    12/02/98  FRY  REFERENCE THE CORRECT POSITION ON THE  *GC024050
006200*                         CONTRACT RECORD FOR THE ACCUMS.        *GC024050
006300*                            CHANGE POSITION OF #ADL TABULAR     *GC024050
006400*                            FROM:  7      TO:  8                *GC024050
006500*                            CHANGE POSITION OF #AOL TABULAR     *GC024050
006600*                            FROM:  9      TO:  10               *GC024050
006700*                                                                *GC024050
006800* P FIX    04/14/99  KIKI  DECREASED BY A BYTE THE LENGTH OF     *GC024050
006900*                          WS-HDR1, WS-HDR2 AND WS-HDR3 TO       *GC024050
007000*                          REPORT THE COMPLETE CENTURY/YEAR,     *GC024050
007100*                          PRIOR THE YEAR WAS DROPED ON REPORT.  *GC024050
007200*                                                                *GC024050
007300*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC024050
007400*                                                                *GC024050
007410* DM09441    09-09-09   DNK   ADJUSTED THE TABULAR RECORD AREA   *GC024050
007420*                             FOR THE EXPANSION.                 *GC024050
      *                                                                *GC024000
      *            09/11/15   KIKI  RECOMPILE -  EXTEND - GCGROUP*     *GC024000
      *                                                                *GC024000
ED0624* BBDA-58217 06/04/24   ED    RECOMPILE FOR PEAQ COPYBOOK        *        
ED0624*                             EXPANSION:                         *        
ED0624*                                   COPYBKS - GCTABM*, GCTACL*,  *        
ED0624*                                   GCTACP*,  GCTADL*, GCTADL*   *        
      ******************************************************************GC024000
007430*                                                                *GC024050
007600*                                                                 GC024050
007700 ENVIRONMENT DIVISION.                                            GC024050
007800 CONFIGURATION SECTION.                                           GC024050
007900 SOURCE-COMPUTER. IBM-370.                                        GC024050
008000 OBJECT-COMPUTER. IBM-370.                                        GC024050
008100 INPUT-OUTPUT SECTION.                                            GC024050
008200 FILE-CONTROL.                                                    GC024050
008300                                                                  GC024050
008400     SELECT GRPSPC-FILE   ASSIGN TO UT-S-GC24050A.                GC024050
008500                                                                  GC024050
008600 DATA DIVISION.                                                   GC024050
008700 FILE SECTION.                                                    GC024050
008800 FD  GRPSPC-FILE                                                  GC024050
008900     BLOCK CONTAINS  0  RECORDS                                   GC024050
009000     RECORDING MODE IS V                                          GC024050
009100     LABEL RECORDS ARE STANDARD.                                  GC024050
009200 01  GRPSPC-REC.                                                  GC024050
009300     COPY GCWRKDCC.                                               GC024050
009400     COPY GCGROUPC.                                               GC024050
009500                                                                  GC024050
009600/                                                                 GC024050
009700 WORKING-STORAGE SECTION.                                         GC024050
009800                                                                  GC024050
009900 01  FILLER            PIC X(24) VALUE                            GC024050
010000     'GC024050 WORKING STORAGE'.                                  GC024050
010100                                                                  GC024050
010200 01  MISC-WORK.                                                   GC024050
010300     05  ABEND-CODE           PIC S9999 COMP VALUE ZERO.          GC024050
010400     05  WS-ADL-KEY.                                              GC024050
010500         10  WS-ADL-KEY-ID    PIC X(06)  VALUE SPACES.            GC024050
010600         10  WS-ADL-KEY-SLOT  PIC S9(07) VALUE ZEROS COMP-3.      GC024050
010700     05  WS-AOL-KEY.                                              GC024050
010800         10  WS-AOL-KEY-ID    PIC X(06)  VALUE SPACES.            GC024050
010900         10  WS-AOL-KEY-SLOT  PIC S9(07) VALUE ZEROS COMP-3.      GC024050
011000     05  WS-CHECK-VALUE       PIC X(02)  VALUE SPACES.            GC024050
011100     05  H-BEN-PER            PIC X(02) VALUE SPACES.             GC024050
011200     05  WS-LINE-CNT          PIC 9(3)  VALUE 80.                 GC024050
011300     05  WS-PAGE-CNT          PIC 9(4)  VALUE ZERO.               GC024050
011400     05  WS-DISP-EFF-DT       PIC 9(05) VALUE ZEROS.              GC024050
011500     05  WS-UNPK-SLOT         PIC 9(07) VALUE ZEROS.              GC024050
011600     05  WS-DISP-TAB-KEY.                                         GC024050
011700         10  WS-DISP-TAB-ID   PIC X(06) VALUE SPACES.             GC024050
011800         10  WS-DISP-TAB-SLOT PIC 9(07) VALUE ZEROS.              GC024050
011900     05  WS-JUL-DATE          PIC 9(05) VALUE ZEROS.              GC024050
012000     05  WS-GREG-DATE.                                            GC024050
012100         10  WS-GREG-DD       PIC 9(02) VALUE ZEROS.              GC024050
012200         10  WS-GREG-MM       PIC 9(02) VALUE ZEROS.              GC024050
012300         10  WS-GREG-YY       PIC 9(02) VALUE ZEROS.              GC024050
012400     05  WS-DATE-AREA.                                            GC024050
012500         10  WS-MDY.                                              GC024050
012600             15  WS-M            PIC 99    VALUE ZERO.            GC024050
012700             15  WS-D            PIC 99    VALUE ZERO.            GC024050
012800             15  WS-Y            PIC 9(4)  VALUE ZERO.            GC024050
012900     05  WS-TIME.                                                 GC024050
013000         10  HDR3-HH         PIC 99      VALUE ZEROS.             GC024050
013100         10  HDR3-MM         PIC 99      VALUE ZEROS.             GC024050
013200         10  HDR3-SS         PIC 99      VALUE ZEROS.             GC024050
013300         10  FILLER          PIC 99      VALUE ZEROS.             GC024050
013400                                                                  GC024050
013500 01  WS-SWITCHES.                                                 GC024050
013600     05  WS-ADL-SW             PIC 9    VALUE ZERO.               GC024050
013700         88  ADL-NOT-FOUND              VALUE 0.                  GC024050
013800         88  ADL-FOUND                  VALUE 1.                  GC024050
013900     05  WS-AOL-SW             PIC 9    VALUE ZERO.               GC024050
014000         88  AOL-NOT-FOUND              VALUE 0.                  GC024050
014100         88  AOL-FOUND                  VALUE 1.                  GC024050
014200     05  WS-GRP-EOF-SW         PIC 9    VALUE ZERO.               GC024050
014300         88  GRP-EOF                    VALUE 1.                  GC024050
014400     05  WS-ERROR-SW           PIC 9    VALUE ZERO.               GC024050
014500         88  ERROR-FOUND                VALUE 1.                  GC024050
014600     05  WS-CON-EOP-SW         PIC 9    VALUE ZERO.               GC024050
014700         88  CONTRACT-IN-PROCESS        VALUE 0.                  GC024050
014800         88  CONTRACT-END-O-PROCESS     VALUE 1.                  GC024050
014900     05  WS-PRINT-SW           PIC 9    VALUE ZERO.               GC024050
015000     05  WS-CON-EOF-SW         PIC 9    VALUE ZERO.               GC024050
015100         88  CON-EOF                    VALUE 1.                  GC024050
015200     05  WS-PROCESS-SW         PIC 9    VALUE ZERO.               GC024050
015300         88  PROCESS-AOL-VS-ADL         VALUE 1.                  GC024050
015400         88  PROCESS-ADL-VS-AOL         VALUE 2.                  GC024050
015500     05  WS-REPORT-SW          PIC 9    VALUE ZERO.               GC024050
015600         88  NO-REPORT                  VALUE 0.                  GC024050
015700     05  WS-SKIP-SW            PIC 9    VALUE ZERO.               GC024050
015800     05  WS-VALUE-FOUND-SW     PIC 9    VALUE ZERO.               GC024050
015900         88  VALUE-NOT-FOUND            VALUE 0.                  GC024050
016000         88  VALUE-FOUND                VALUE 1.                  GC024050
016100/                                                                 GC024050
016200 01  RIP-AREA.                                                    GC024050
016300     05  RIP-STACKER          PIC X(8)   VALUE 'RIP001'.          GC024050
016400     05  PRINT-AREA.                                              GC024050
016500         10  PRINT-CC         PIC X      VALUE SPACES.            GC024050
016600         10  PRINT-LINE       PIC X(132) VALUE SPACES.            GC024050
016700                                                                  GC024050
016800     COPY RIPHDTR.                                                GC024050
016900                                                                  GC024050
017000     COPY MLDATE01.                                               GC024050
017100                                                                  GC024050
017200/                                                                 GC024050
017300 01  WS-PRINT-HEADINGS.                                           GC024050
017400     05  WS-HDR1.                                                 GC024050
017500         10  FILLER              PIC X       VALUE '1'.           GC024050
017600         10  FILLER              PIC X(14)   VALUE                GC024050
017700             'PGM # GC024050'.                                    GC024050
017800         10  FILLER              PIC X(36)   VALUE SPACES.        GC024050
017900         10  FILLER              PIC X(31)   VALUE                GC024050
018000             'HEALTH CARE SERVICE CORPORATION'.                   GC024050
018100         10  FILLER              PIC X(32)   VALUE SPACES.        GC024050
018200         10  FILLER              PIC X(5)    VALUE 'PAGE:'.       GC024050
018300         10  FILLER              PIC X(6)    VALUE SPACES.        GC024050
018400         10  HDR1-PG-NO          PIC ZZZZ9.                       GC024050
018500                                                                  GC024050
018600     05  WS-HDR2.                                                 GC024050
018700         10  FILLER              PIC X       VALUE SPACES.        GC024050
018800         10  FILLER              PIC X(6)    VALUE 'RPT # '.      GC024050
018900         10  HDR2-REPORT-NO      PIC X(6)    VALUE ' 1129 '.      GC024050
019000         10  FILLER              PIC X(37)   VALUE SPACES.        GC024050
019100         10  FILLER              PIC X(34)   VALUE                GC024050
019200             'GENERIC CONTRACT PROCESSING SYSTEM'.                GC024050
019300         10  FILLER              PIC X(30)   VALUE SPACES.        GC024050
019400         10  FILLER              PIC X(6)    VALUE 'DATE: '.      GC024050
019500         10  HDR2-MM-DD-YY.                                       GC024050
019600             15  HDR2-MM         PIC 99      VALUE ZEROS.         GC024050
019700             15  FILLER          PIC X       VALUE '/'.           GC024050
019800             15  HDR2-DD         PIC 99      VALUE ZEROS.         GC024050
019900             15  FILLER          PIC X       VALUE '/'.           GC024050
020000             15  HDR2-YYYY       PIC 9999    VALUE ZEROS.         GC024050
020100                                                                  GC024050
020200     05  WS-HDR3.                                                 GC024050
020300         10  FILLER              PIC X       VALUE SPACES.        GC024050
020400         10  FILLER              PIC X(34)   VALUE                GC024050
020500             'DEPT: 917  LOCATION: SSD 7TH FLOOR'.                GC024050
020600         10  FILLER              PIC X(14)   VALUE SPACES.        GC024050
020700         10  FILLER              PIC X(36)   VALUE                GC024050
020800             'INCONSISTENT ADL/AOL BENEFIT PERIODS'.              GC024050
020900         10  FILLER              PIC X(23)   VALUE                GC024050
021000             ' - GROUP SPECIFIC FILE '.                           GC024050
021100         10  FILLER              PIC X(6)    VALUE SPACES.        GC024050
021200         10  FILLER              PIC X(6)    VALUE 'TIME: '.      GC024050
021300         10  HDR3-HH-MM-SS.                                       GC024050
021400             15  HDR3-HH         PIC 99      VALUE ZEROS.         GC024050
021500             15  FILLER          PIC X       VALUE ':'.           GC024050
021600             15  HDR3-MM         PIC 99      VALUE ZEROS.         GC024050
021700             15  FILLER          PIC X       VALUE ':'.           GC024050
021800             15  HDR3-SS         PIC 99      VALUE ZEROS.         GC024050
021900                                                                  GC024050
022000     05  WS-GRP-CON-COL-HDR1.                                     GC024050
022100         10  FILLER              PIC X(01)   VALUE '0'.           GC024050
022200         10  FILLER              PIC X(34) VALUE                  GC024050
022300             '------- GROUP SPECIFIC KEY -------'.                GC024050
022400         10  FILLER              PIC X(03) VALUE SPACES.          GC024050
022500         10  FILLER              PIC X(13) VALUE                  GC024050
022600             '-TABULAR KEY-'.                                     GC024050
022700         10  FILLER              PIC X(02) VALUE SPACES.          GC024050
022800         10  FILLER              PIC X(07) VALUE                  GC024050
022900             'BEN-PER'.                                           GC024050
023000         10  FILLER              PIC X(03) VALUE SPACES.          GC024050
023100         10  FILLER              PIC X(42) VALUE                  GC024050
023200             '-------------- CONTRACT KEY --------------'.        GC024050
023300         10  FILLER              PIC X(03) VALUE SPACES.          GC024050
023400         10  FILLER              PIC X(13) VALUE                  GC024050
023500             '-TABULAR KEY-'.                                     GC024050
023600         10  FILLER              PIC X(02) VALUE SPACES.          GC024050
023700         10  FILLER              PIC X(07) VALUE                  GC024050
023800             'BEN-PER'.                                           GC024050
023900                                                                  GC024050
024000     05  WS-GRP-CON-COL-HDR2.                                     GC024050
024100         10  FILLER              PIC X(01)   VALUE ' '.           GC024050
024200         10  FILLER              PIC X(34) VALUE                  GC024050
024300             'PLN GROUP     SECTN PKG FRL EFF-DT'.                GC024050
024400         10  FILLER              PIC X(03) VALUE SPACES.          GC024050
024500         10  FILLER              PIC X(13) VALUE                  GC024050
024600             'ID.      SLOT'.                                     GC024050
024700         10  FILLER              PIC X(04) VALUE SPACES.          GC024050
024800         10  FILLER              PIC X(03) VALUE                  GC024050
024900             'IND'.                                               GC024050
025000         10  FILLER              PIC X(05) VALUE SPACES.          GC024050
025100         10  FILLER              PIC X(42) VALUE                  GC024050
025200             'PLN GROUP     SECTN PKG LOB PRV FRL EFF-DT'.        GC024050
025300         10  FILLER              PIC X(03) VALUE SPACES.          GC024050
025400         10  FILLER              PIC X(13) VALUE                  GC024050
025500             'ID.      SLOT'.                                     GC024050
025600         10  FILLER              PIC X(04) VALUE SPACES.          GC024050
025700         10  FILLER              PIC X(03) VALUE                  GC024050
025800             'IND'.                                               GC024050
025900                                                                  GC024050
026000     05  WS-GRP-CON-DET1.                                         GC024050
026100         10  FILLER              PIC X(01)   VALUE ' '.           GC024050
026200         10  DET1-PLAN-CODE1     PIC X(3).                        GC024050
026300         10  DET1-GRP1           PIC X(09).                       GC024050
026400         10  FILLER              PIC X(01)   VALUE SPACES.        GC024050
026500         10  DET1-SECTN1         PIC X(05).                       GC024050
026600         10  FILLER              PIC X(01)   VALUE SPACES.        GC024050
026700         10  DET1-PKG-CODE1      PIC X(3).                        GC024050
026800         10  FILLER              PIC X(02)   VALUE SPACES.        GC024050
026900         10  DET1-FRL1           PIC X(02).                       GC024050
027000         10  FILLER              PIC X(02)   VALUE SPACES.        GC024050
027100         10  DET1-DATE1.                                          GC024050
027200             15  DET1-MM1        PIC X(02).                       GC024050
027300             15  DET1-DD1        PIC X(02).                       GC024050
027400             15  DET1-YY1        PIC X(02).                       GC024050
027500         10  FILLER              PIC X(03)   VALUE SPACES.        GC024050
027600         10  DET1-TAB1.                                           GC024050
027700             15  DET1-TAB1-ID    PIC X(06).                       GC024050
027800             15  DET1-TAB1-SLOT  PIC Z(07).                       GC024050
027900         10  FILLER              PIC X(04)   VALUE SPACES.        GC024050
028000         10  DET1-BEN-PER1       PIC X(02).                       GC024050
028100         10  FILLER              PIC X(06)   VALUE SPACES.        GC024050
028200         10  DET1-PLAN-CODE2     PIC X(3).                        GC024050
028300         10  DET1-GRP2           PIC X(09).                       GC024050
028400         10  FILLER              PIC X(01)   VALUE SPACES.        GC024050
028500         10  DET1-SECTN2         PIC X(05).                       GC024050
028600         10  FILLER              PIC X(01)   VALUE SPACES.        GC024050
028700         10  DET1-PKG-CODE2      PIC X(3).                        GC024050
028800         10  FILLER              PIC X(02)   VALUE SPACES.        GC024050
028900         10  DET1-LOB2           PIC X(01).                       GC024050
029000         10  FILLER              PIC X(02)   VALUE SPACES.        GC024050
029100         10  DET1-PVC2           PIC X(02).                       GC024050
029200         10  FILLER              PIC X(03)   VALUE SPACES.        GC024050
029300         10  DET1-FRL2           PIC X(02).                       GC024050
029400         10  FILLER              PIC X(02)   VALUE SPACES.        GC024050
029500         10  DET1-DATE2.                                          GC024050
029600             15  DET1-MM2        PIC X(02).                       GC024050
029700             15  DET1-DD2        PIC X(02).                       GC024050
029800             15  DET1-YY2        PIC X(02).                       GC024050
029900         10  FILLER              PIC X(03)   VALUE SPACES.        GC024050
030000         10  DET1-TAB2.                                           GC024050
030100             15  DET1-TAB2-ID    PIC X(06).                       GC024050
030200             15  DET1-TAB2-SLOT  PIC Z(07).                       GC024050
030300         10  FILLER              PIC X(04)   VALUE SPACES.        GC024050
030400         10  DET1-BEN-PER2       PIC X(02).                       GC024050
030500/                                                                 GC024050
030600     05  WS-NO-RECORD-MSG.                                        GC024050
030700         10  FILLER              PIC X(01)  VALUE '0'.            GC024050
030800         10  FILLER              PIC X(74)  VALUE                 GC024050
030900         '*** NO ERROR CONDITIONS ENCOUNTERED WITH GROUP SPECIFIC GC024050
031000-        'ADL/AOL EDITS *** '.                                    GC024050
031100          10  FILLER              PIC X(58)  VALUE SPACES.        GC024050
031200/                                                                 GC024050
031300 01  PARM-SET.                                                    GC024050
031400     05  SET-RDW.                                                 GC024050
031500         10  SET-REC-LENG    PIC 9(2)  COMP VALUE ZEROS.          GC024050
031600         10  SET-FEEDBACK    PIC 9(2)  COMP VALUE ZEROS.          GC024050
031700     05  SET-VALUE           PIC 9(5)  COMP.                      GC024050
031800                                                                  GC024050
031900/                                                                 GC024050
032000 01  PARM-ONE.                                                    GC024050
032100     05  RESERVED-FLDS-1     PIC 9(5)  COMP  VALUE ZEROS.         GC024050
032200     05  RESERVED-X-1 REDEFINES RESERVED-FLDS-1.                  GC024050
032300         10  REQUEST-TYPE-1  PIC X.                               GC024050
032400                                                                  GC024050
032500 01  PARM-ONEA.                                                   GC024050
032600     02  ONEA-RDW.                                                GC024050
032700         05  ONEA-REC-LEN    PIC 9(2)  COMP VALUE ZEROS.          GC024050
032800         05  ONEA-FEEDBACK   PIC 9(2)  COMP VALUE ZEROS.          GC024050
032900     02  ONEA-REC-AREA.                                           GC024050
033000         05  ONEA-REC-KEY.                                        GC024050
033100             10  ONEA-REC-ID   PIC X(6).                          GC024050
033200             10  ONEA-REC-SLOT PIC S9(7) COMP-3  VALUE ZEROS.     GC024050
033300         05  FILLER            PIC X(27)   VALUE LOW-VALUES.      GC024050
033400         05  ONEA-REC-CNT      PIC S9(5) COMP-3  VALUE ZEROS.     GC024050
033500         05  ONEA-REC-FILLER   PIC X(31330)      VALUE LOW-VALUES.GC024050
033600                                                                  GC024050
033700/                                                                 GC024050
033800 01  PARM-TWO.                                                    GC024050
033900     05  RESERVED-FLDS-2     PIC 9(5)  COMP VALUE ZEROS.          GC024050
034000     05  RESERVED-X-2 REDEFINES RESERVED-FLDS-2.                  GC024050
034100         10  REQUEST-TYPE-2  PIC X.                               GC024050
034200                                                                  GC024050
034300 01  PARM-TWOA.                                                   GC024050
034400     02  TWOA-RDW.                                                GC024050
034500         05  TWOA-REC-LEN    PIC 9(2)  COMP  VALUE ZEROS.         GC024050
034600         05  TWOA-FEEDBACK   PIC 9(2)  COMP  VALUE ZEROS.         GC024050
034700     02  TWOA-REC-AREA.                                           GC024050
034800         COPY GCCONTRC.                                           GC024050
034900/                                                                 GC024050
035000 01  WS-ADL-REC.                                                  GC024050
035100     COPY GCTADLC.                                                GC024050
035200/                                                                 GC024050
035300 01  WS-AOL-REC.                                                  GC024050
035400     COPY GCTAOLC.                                                GC024050
035500/                                                                 GC024050
035600     COPY HSCDATES.                                               GC024050
035700/                                                                 GC024050
035800 PROCEDURE DIVISION.                                              GC024050
035900                                                                  GC024050
036000 0100-PROCESS.                                                    GC024050
036100                                                                  GC024050
036200     PERFORM 0100-005-OPEN-FILES    THRU 0100-005-EXIT.           GC024050
036300                                                                  GC024050
036400     PERFORM 0100-010-READ THRU 0100-010-EXIT                     GC024050
036500         UNTIL GRP-EOF.                                           GC024050
036600                                                                  GC024050
036700     IF WS-REPORT-SW = 0                                          GC024050
036800         PERFORM R0140-PRINT THRU R0140-EXIT.                     GC024050
036900                                                                  GC024050
037000     PERFORM 0900-CLOSE-FILES THRU 0900-EXIT.                     GC024050
037100                                                                  GC024050
037200     STOP RUN.                                                    GC024050
037300/                                                                 GC024050
037400 0100-005-OPEN-FILES.                                             GC024050
037500                                                                  GC024050
037600     MOVE 'S'                TO REQUEST-TYPE-1.                   GC024050
037700     MOVE 8                  TO SET-REC-LENG.                     GC024050
037800     MOVE 3                  TO SET-VALUE.                        GC024050
037900     CALL 'TSGVSAM1' USING PARM-ONE PARM-SET.                     GC024050
038000     IF  REQUEST-TYPE-1 NOT EQUAL 'S'                             GC024050
038100         DISPLAY 'TSGVSAM1 SET ERROR'                             GC024050
038200         MOVE SET-FEEDBACK   TO ABEND-CODE                        GC024050
038300         GO TO 99999-ERROR-RTN.                                   GC024050
038400     MOVE 'S'                TO REQUEST-TYPE-2.                   GC024050
038500     MOVE 8                  TO SET-REC-LENG.                     GC024050
038600     MOVE 3                  TO SET-VALUE.                        GC024050
038700     CALL 'TSGVSAM2' USING PARM-TWO PARM-SET.                     GC024050
038800     IF  REQUEST-TYPE-2 NOT EQUAL 'S'                             GC024050
038900         DISPLAY 'TSGVSAM2 SET ERROR'                             GC024050
039000         MOVE SET-FEEDBACK   TO ABEND-CODE                        GC024050
039100         GO TO 99999-ERROR-RTN.                                   GC024050
039200                                                                  GC024050
039300     OPEN INPUT  GRPSPC-FILE.                                     GC024050
039400                                                                  GC024050
039500     MOVE    'TDY'               TO  MLDATE-FUNC.                 GC024050
039600     MOVE    'M'                 TO  MLDATE-FORM1.                GC024050
039700     CALL    'MLDATE' USING MLDATE01.                             GC024050
039800     MOVE    MLDATE-DATE1        TO  WS-MDY.                      GC024050
039900     MOVE WS-M                   TO HDR2-MM.                      GC024050
040000     MOVE WS-D                   TO HDR2-DD.                      GC024050
040100     MOVE WS-Y                   TO HDR2-YYYY.                    GC024050
040200                                                                  GC024050
040300     ACCEPT WS-TIME FROM TIME.                                    GC024050
040400     MOVE CORR WS-TIME TO HDR3-HH-MM-SS.                          GC024050
040500                                                                  GC024050
040600     MOVE '1129'  TO  TECH-HDR-RPT-NO                             GC024050
040700                      TECH-TRL-RPT-NO.                            GC024050
040800     MOVE 'GROUPSPC INCONST ADL/AOL RPT' TO TECH-HDR-RPT-NAME.    GC024050
040900     MOVE HR-TECH-HEADER TO PRINT-AREA.                           GC024050
041000     CALL 'RIPWRITE' USING PRINT-AREA  RIP-STACKER.               GC024050
041100 0100-005-EXIT.                                                   GC024050
041200     EXIT.                                                        GC024050
041300/                                                                 GC024050
041400 0100-010-READ.                                                   GC024050
041500                                                                  GC024050
041600     READ GRPSPC-FILE AT END                                      GC024050
041700         MOVE 1 TO WS-GRP-EOF-SW                                  GC024050
041800         GO TO 0100-010-EXIT.                                     GC024050
041900                                                                  GC024050
042000     IF GCG-COUNT-TAB-PROVN-POINTERS NOT > 1                      GC024050
042100         GO TO 0100-010-EXIT.                                     GC024050
042200                                                                  GC024050
042300     MOVE +44    TO GAC-ENTRY-COUNT                               GC024050
042400                    GAD-ENTRY-COUNT.                              GC024050
042500                                                                  GC024050
042600     MOVE SPACES TO WS-ADL-REC                                    GC024050
042700                    WS-AOL-REC                                    GC024050
042800                    WS-ADL-KEY-ID                                 GC024050
042900                    WS-AOL-KEY-ID                                 GC024050
043000                    PRINT-AREA.                                   GC024050
043100                                                                  GC024050
043200     MOVE ZEROS TO  WS-ADL-KEY-SLOT                               GC024050
043300                    WS-AOL-KEY-SLOT.                              GC024050
043400                                                                  GC024050
043500     PERFORM R0130-MOVE-GRPSPC-TABS-TO-WS THRU R0130-EXIT         GC024050
043600         VARYING GCG-INDEX FROM 1 BY 1                            GC024050
043700         UNTIL GCG-INDEX = GCG-COUNT-TAB-PROVN-POINTERS.          GC024050
043800                                                                  GC024050
043900** CHECK #AOL VS. #ADL                                            GC024050
044000     IF WS-AOL-KEY-SLOT > ZEROS                                   GC024050
044100         MOVE 1 TO WS-PROCESS-SW                                  GC024050
044200         MOVE WS-AOL-KEY TO ONEA-REC-KEY                          GC024050
044300         PERFORM R0020-READ-TABULAR THRU R0020-EXIT               GC024050
044400         MOVE '0D' TO WS-CHECK-VALUE                              GC024050
044500         PERFORM 0100-020-AOL-RTN THRU 0100-020-EXIT              GC024050
044600         MOVE '0E' TO WS-CHECK-VALUE                              GC024050
044700         PERFORM 0100-020-AOL-RTN THRU 0100-020-EXIT.             GC024050
044800                                                                  GC024050
044900** NOW THE REVERSE (VICE-VERSA) #ADL VS. #AOL                     GC024050
045000     IF WS-ADL-KEY-SLOT > ZEROS                                   GC024050
045100         MOVE 2 TO WS-PROCESS-SW                                  GC024050
045200         MOVE WS-ADL-KEY TO ONEA-REC-KEY                          GC024050
045300         PERFORM R0020-READ-TABULAR THRU R0020-EXIT               GC024050
045400         MOVE '0D' TO WS-CHECK-VALUE                              GC024050
045500         PERFORM 0100-030-ADL-RTN THRU 0100-030-EXIT              GC024050
045600         MOVE '0E' TO WS-CHECK-VALUE                              GC024050
045700         PERFORM 0100-030-ADL-RTN THRU 0100-030-EXIT.             GC024050
045800                                                                  GC024050
045900*************************                                         GC024050
046000***** USER WANTS A BLANK LINE AFTER BREAK ON GROUP.               GC024050
046100***** NOTE: AT LEAST TWO LINES WILL BE PRINTED FOR EACH           GC024050
046200*****       ERROR CONDITION, SO CHECK THE LINE-CNT TO BE          GC024050
046300*****       SURE BOTH WILL BE PRINTED TOGETHER. (WS-LINE-CNT < 59)GC024050
046400*************************                                         GC024050
046500     IF WS-SKIP-SW = ZERO                                         GC024050
046600         GO TO 0100-010-EXIT.                                     GC024050
046700     MOVE ZERO TO WS-SKIP-SW.                                     GC024050
046800     MOVE SPACES TO PRINT-AREA.                                   GC024050
046900     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024050
047000     ADD 1 TO WS-LINE-CNT.                                        GC024050
047100     IF WS-LINE-CNT > 59                                          GC024050
047200         MOVE 80 TO WS-LINE-CNT.                                  GC024050
047300                                                                  GC024050
047400 0100-010-EXIT.                                                   GC024050
047500     EXIT.                                                        GC024050
047600/                                                                 GC024050
047700                                                                  GC024050
047800 0100-020-AOL-RTN.                                                GC024050
047900                                                                  GC024050
048000** SEE IF VALUE EXISTS.                                           GC024050
048100     MOVE 0 TO WS-VALUE-FOUND-SW.                                 GC024050
048200     PERFORM R0030-CHECK-AOL-BEN-PER THRU R0030-EXIT              GC024050
048300         VARYING GAD-INDEX FROM 1 BY 1                            GC024050
048400         UNTIL GAD-INDEX = GAD-ENTRY-COUNT                        GC024050
048500              OR                                                  GC024050
048600               VALUE-FOUND.                                       GC024050
048700                                                                  GC024050
048800     IF VALUE-NOT-FOUND                                           GC024050
048900         GO TO 0100-020-EXIT.                                     GC024050
049000                                                                  GC024050
049100** CHECK FOR #ADL KEY ON GROUP SPECIFIC RECORD.                   GC024050
049200     IF WS-ADL-KEY-SLOT > ZEROS                                   GC024050
049300         MOVE 0 TO WS-ERROR-SW                                    GC024050
049400         MOVE WS-ADL-KEY TO ONEA-REC-KEY                          GC024050
049500         PERFORM R0020-READ-TABULAR THRU R0020-EXIT               GC024050
049600         PERFORM R0040-CHECK-ALL-ADL THRU R0040-EXIT              GC024050
049700             VARYING GAC-INDEX FROM 1 BY 1                        GC024050
049800             UNTIL GAC-INDEX = GAC-ENTRY-COUNT                    GC024050
049900           OR                                                     GC024050
050000             ERROR-FOUND                                          GC024050
050100         IF ERROR-FOUND                                           GC024050
050200             PERFORM R0100-FORMAT-GRPSPC-ERROR THRU R0100-EXIT.   GC024050
050300                                                                  GC024050
050400** CHECK ALL CONTRACT RECORDS THAT HAVE THE SAME                  GC024050
050500** GROUP AND SECTION NUMBERS.                                     GC024050
050600     MOVE GCG-PLAN-CODE TO GCT-PLAN-CODE.                         GC024050
050700     MOVE GCG-GROUP-NUM TO GCT-GROUP-NUM.                         GC024050
050800     MOVE GCG-SECTION-NUM TO GCT-SECTION-NUM.                     GC024050
050900     MOVE GCG-PKG-CODE    TO GCT-PKG-CODE.                        GC024050
051000     PERFORM R0060-POINT THRU R0060-EXIT.                         GC024050
051100     MOVE ZEROS TO WS-PRINT-SW                                    GC024050
051200                   WS-CON-EOP-SW.                                 GC024050
051300     PERFORM R0070-PROCESS THRU R0070-EXIT                        GC024050
051400         UNTIL CONTRACT-END-O-PROCESS.                            GC024050
051500 0100-020-EXIT.                                                   GC024050
051600     EXIT.                                                        GC024050
051700                                                                  GC024050
051800 0100-030-ADL-RTN.                                                GC024050
051900                                                                  GC024050
052000** SEE IF VALUE EXISTS.                                           GC024050
052100     MOVE 0 TO WS-VALUE-FOUND-SW.                                 GC024050
052200     PERFORM R0035-CHECK-ADL-BEN-PER THRU R0035-EXIT              GC024050
052300         VARYING GAC-INDEX FROM 1 BY 1                            GC024050
052400         UNTIL GAC-INDEX = GAC-ENTRY-COUNT                        GC024050
052500              OR                                                  GC024050
052600               VALUE-FOUND.                                       GC024050
052700                                                                  GC024050
052800     IF VALUE-NOT-FOUND                                           GC024050
052900         GO TO 0100-030-EXIT.                                     GC024050
053000                                                                  GC024050
053100** CHECK FOR #AOL KEY ON GROUP SPECIFIC RECORD.                   GC024050
053200     IF WS-AOL-KEY-SLOT > ZEROS                                   GC024050
053300         MOVE 0 TO WS-ERROR-SW                                    GC024050
053400         MOVE WS-AOL-KEY TO ONEA-REC-KEY                          GC024050
053500         PERFORM R0020-READ-TABULAR THRU R0020-EXIT               GC024050
053600         PERFORM R0045-CHECK-ALL-AOL THRU R0045-EXIT              GC024050
053700             VARYING GAD-INDEX FROM 1 BY 1                        GC024050
053800             UNTIL GAD-INDEX = GAD-ENTRY-COUNT                    GC024050
053900           OR                                                     GC024050
054000             ERROR-FOUND                                          GC024050
054100         IF ERROR-FOUND                                           GC024050
054200             PERFORM R0100-FORMAT-GRPSPC-ERROR THRU R0100-EXIT.   GC024050
054300                                                                  GC024050
054400** CHECK ALL CONTRACT RECORDS THAT HAVE THE SAME                  GC024050
054500** GROUP AND SECTION NUMBERS.                                     GC024050
054600     MOVE GCG-PLAN-CODE TO GCT-PLAN-CODE.                         GC024050
054700     MOVE GCG-GROUP-NUM TO GCT-GROUP-NUM.                         GC024050
054800     MOVE GCG-SECTION-NUM TO GCT-SECTION-NUM.                     GC024050
054900     MOVE GCG-PKG-CODE  TO GCT-PKG-CODE.                          GC024050
055000     PERFORM R0060-POINT THRU R0060-EXIT.                         GC024050
055100     MOVE ZEROS TO WS-PRINT-SW                                    GC024050
055200                   WS-CON-EOP-SW.                                 GC024050
055300     PERFORM R0070-PROCESS THRU R0070-EXIT                        GC024050
055400         UNTIL CONTRACT-END-O-PROCESS.                            GC024050
055500 0100-030-EXIT.                                                   GC024050
055600     EXIT.                                                        GC024050
055700                                                                  GC024050
055800 R0020-READ-TABULAR.                                              GC024050
055900                                                                  GC024050
056000     MOVE 'R' TO REQUEST-TYPE-1.                                  GC024050
056100     MOVE +14 TO ONEA-REC-LEN.                                    GC024050
056200                                                                  GC024050
056300     CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC024050
056400                                                                  GC024050
056500     IF REQUEST-TYPE-1 NOT = 'R'                                  GC024050
056600         DISPLAY 'GC024050 BAD READ--R0020-READ-TABULAR    '      GC024050
056700         MOVE ONEA-REC-ID   TO WS-DISP-TAB-ID                     GC024050
056800         MOVE ONEA-REC-SLOT TO WS-DISP-TAB-SLOT                   GC024050
056900         DISPLAY 'TABULAR KEY = ' WS-DISP-TAB-KEY                 GC024050
057000         MOVE ONEA-FEEDBACK TO ABEND-CODE                         GC024050
057100         GO TO 99999-ERROR-RTN.                                   GC024050
057200                                                                  GC024050
057300     IF ONEA-REC-ID = '#ADL  '                                    GC024050
057400         MOVE ONEA-REC-CNT    TO GAC-ENTRY-COUNT                  GC024050
057500         MOVE GAC-ENTRY-COUNT TO GAC-ENTRY-COUNT                  GC024050
057600         MOVE ONEA-REC-AREA TO WS-ADL-REC                         GC024050
057700     ELSE                                                         GC024050
057800         MOVE ONEA-REC-CNT    TO GAD-ENTRY-COUNT                  GC024050
057900         MOVE GAD-ENTRY-COUNT TO GAD-ENTRY-COUNT                  GC024050
058000         MOVE ONEA-REC-AREA TO WS-AOL-REC.                        GC024050
058100                                                                  GC024050
058200 R0020-EXIT.                                                      GC024050
058300     EXIT.                                                        GC024050
058400                                                                  GC024050
058500 R0030-CHECK-AOL-BEN-PER.                                         GC024050
058600      IF GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX) = WS-CHECK-VALUE    GC024050
058700          MOVE 1 TO WS-VALUE-FOUND-SW.                            GC024050
058800 R0030-EXIT.                                                      GC024050
058900      EXIT.                                                       GC024050
059000                                                                  GC024050
059100 R0035-CHECK-ADL-BEN-PER.                                         GC024050
059200      IF GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX) = WS-CHECK-VALUE     GC024050
059300          MOVE 1 TO WS-VALUE-FOUND-SW.                            GC024050
059400 R0035-EXIT.                                                      GC024050
059500      EXIT.                                                       GC024050
059600                                                                  GC024050
059700 R0040-CHECK-ALL-ADL.                                             GC024050
059800      IF GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX) NOT =                GC024050
059900                                               WS-CHECK-VALUE     GC024050
060000          MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX) TO H-BEN-PER   GC024050
060100          MOVE 1 TO WS-ERROR-SW.                                  GC024050
060200 R0040-EXIT.                                                      GC024050
060300      EXIT.                                                       GC024050
060400                                                                  GC024050
060500 R0045-CHECK-ALL-AOL.                                             GC024050
060600      IF GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX) NOT =               GC024050
060700                                                WS-CHECK-VALUE    GC024050
060800          MOVE GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX) TO H-BEN-PER  GC024050
060900          MOVE 1 TO WS-ERROR-SW.                                  GC024050
061000 R0045-EXIT.                                                      GC024050
061100      EXIT.                                                       GC024050
061200                                                                  GC024050
061300 R0060-POINT.                                                     GC024050
061400     MOVE 'P' TO REQUEST-TYPE-2.                                  GC024050
061500     MOVE +24  TO TWOA-REC-LEN.                                   GC024050
061600     CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC024050
061700     IF REQUEST-TYPE-2 NOT = 'P'                                  GC024050
061800         MOVE GCG-EFF-DT TO WS-DISP-EFF-DT                        GC024050
061900         DISPLAY 'GC024050 BAD POINT--R0060-POINT '               GC024050
062000              GCG-PLAN-CODE ' '                                   GC024050
062100              GCG-GROUP-NUM ' '                                   GC024050
062200              GCG-SECTION-NUM ' '                                 GC024050
062300              GCG-PKG-CODE ' '                                    GC024050
062400              GCG-FAM-REL-LVL    ' '                              GC024050
062500              WS-DISP-EFF-DT                                      GC024050
062600             MOVE SET-FEEDBACK TO ABEND-CODE                      GC024050
062700             GO TO 99999-ERROR-RTN.                               GC024050
062800 R0060-EXIT.                                                      GC024050
062900      EXIT.                                                       GC024050
063000                                                                  GC024050
063100 R0070-PROCESS.                                                   GC024050
063200     MOVE 1 TO WS-PRINT-SW.                                       GC024050
063300     MOVE 'G' TO REQUEST-TYPE-2.                                  GC024050
063400     CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC024050
063500     IF REQUEST-TYPE-2  = '2'                                     GC024050
063600         MOVE 1 TO WS-CON-EOP-SW                                  GC024050
063700         MOVE 0 TO WS-PRINT-SW                                    GC024050
063800         GO TO R0070-EXIT.                                        GC024050
063900     IF REQUEST-TYPE-2  NOT = 'G'                                 GC024050
064000         MOVE GCG-EFF-DT TO WS-DISP-EFF-DT                        GC024050
064100         DISPLAY 'GC024050 BAD GET--R0070-PROCESS '               GC024050
064200              GCG-PLAN-CODE ' '                                   GC024050
064300              GCG-GROUP-NUM ' '                                   GC024050
064400              GCG-SECTION-NUM ' '                                 GC024050
064500              GCG-PKG-CODE ' '                                    GC024050
064600              GCG-FAM-REL-LVL    ' '                              GC024050
064700              WS-DISP-EFF-DT                                      GC024050
064800             MOVE TWOA-FEEDBACK TO ABEND-CODE                     GC024050
064900             GO TO 99999-ERROR-RTN.                               GC024050
065000      IF GCG-PLAN-CODE = GCT-PLAN-CODE                            GC024050
065100        AND                                                       GC024050
065200          GCG-GROUP-NUM  = GCT-GROUP-NUM                          GC024050
065300        AND                                                       GC024050
065400          GCG-SECTION-NUM = GCT-SECTION-NUM                       GC024050
065500        AND                                                       GC024050
065600          GCG-PKG-CODE = GCT-PKG-CODE                             GC024050
065700          NEXT SENTENCE                                           GC024050
065800      ELSE                                                        GC024050
065900          MOVE 1 TO WS-CON-EOP-SW                                 GC024050
066000          MOVE 0 TO WS-PRINT-SW                                   GC024050
066100          GO TO R0070-EXIT.                                       GC024050
066200      IF PROCESS-AOL-VS-ADL                                       GC024050
066300          PERFORM R0070-010-ADL THRU R0070-010-EXIT.              GC024050
066400      IF PROCESS-ADL-VS-AOL                                       GC024050
066500          PERFORM R0070-020-AOL THRU R0070-020-EXIT.              GC024050
066600                                                                  GC024050
066700 R0070-EXIT.                                                      GC024050
066800      EXIT.                                                       GC024050
066900                                                                  GC024050
067000                                                                  GC024050
067100 R0070-010-ADL.                                                   GC024050
067200     MOVE 0 TO WS-ADL-SW.                                         GC024050
067300     SET GCT-TAB-INDEX TO +8.                                     GC024050
067400     IF GCT-CON-TAB-ID (GCT-TAB-INDEX) = '#ADL  '                 GC024050
067500       AND                                                        GC024050
067600        GCT-CON-TAB-SLOT (GCT-TAB-INDEX) > ZEROS                  GC024050
067700         MOVE 1 TO WS-ADL-SW                                      GC024050
067800     ELSE                                                         GC024050
067900         GO TO R0070-010-EXIT.                                    GC024050
068000     MOVE GCT-CON-TAB-ID-SLOT (GCT-TAB-INDEX) TO ONEA-REC-KEY.    GC024050
068100     PERFORM R0020-READ-TABULAR THRU R0020-EXIT.                  GC024050
068200     MOVE 0 TO WS-ERROR-SW.                                       GC024050
068300     PERFORM R0040-CHECK-ALL-ADL THRU R0040-EXIT                  GC024050
068400         VARYING GAC-INDEX FROM 1 BY 1                            GC024050
068500         UNTIL GAC-INDEX = GAC-ENTRY-COUNT                        GC024050
068600       OR                                                         GC024050
068700         ERROR-FOUND.                                             GC024050
068800     IF ERROR-FOUND                                               GC024050
068900         PERFORM R0110-FORMAT-CONTRACT-ERROR THRU R0110-EXIT.     GC024050
069000 R0070-010-EXIT.                                                  GC024050
069100     EXIT.                                                        GC024050
069200                                                                  GC024050
069300 R0070-020-AOL.                                                   GC024050
069400     MOVE 0 TO WS-AOL-SW.                                         GC024050
069500     SET GCT-TAB-INDEX TO +10.                                    GC024050
069600     IF GCT-CON-TAB-ID (GCT-TAB-INDEX) = '#AOL  '                 GC024050
069700       AND                                                        GC024050
069800        GCT-CON-TAB-SLOT (GCT-TAB-INDEX) > ZEROS                  GC024050
069900         MOVE 1 TO WS-AOL-SW                                      GC024050
070000     ELSE                                                         GC024050
070100         GO TO R0070-020-EXIT.                                    GC024050
070200     MOVE GCT-CON-TAB-ID-SLOT (GCT-TAB-INDEX) TO ONEA-REC-KEY.    GC024050
070300     PERFORM R0020-READ-TABULAR THRU R0020-EXIT.                  GC024050
070400     MOVE 0 TO WS-ERROR-SW.                                       GC024050
070500     PERFORM R0045-CHECK-ALL-AOL THRU R0045-EXIT                  GC024050
070600         VARYING GAD-INDEX FROM 1 BY 1                            GC024050
070700         UNTIL GAD-INDEX = GAD-ENTRY-COUNT                        GC024050
070800       OR                                                         GC024050
070900         ERROR-FOUND.                                             GC024050
071000     IF ERROR-FOUND                                               GC024050
071100         PERFORM R0110-FORMAT-CONTRACT-ERROR THRU R0110-EXIT.     GC024050
071200 R0070-020-EXIT.                                                  GC024050
071300     EXIT.                                                        GC024050
071400                                                                  GC024050
071500                                                                  GC024050
071600 R0090-CONVERT-JUL-TO-GREG.                                       GC024050
071700                                                                  GC024050
071800     CALL 'TSGGREG' USING WS-JUL-DATE WS-GREG-DATE.               GC024050
071900                                                                  GC024050
072000 R0090-EXIT.                                                      GC024050
072100      EXIT.                                                       GC024050
072200                                                                  GC024050
072300 R0100-FORMAT-GRPSPC-ERROR.                                       GC024050
072400     MOVE GCG-PLAN-CODE           TO DET1-PLAN-CODE1.             GC024050
072500     MOVE GCG-GROUP-NUM           TO DET1-GRP1.                   GC024050
072600     MOVE GCG-SECTION-NUM         TO DET1-SECTN1.                 GC024050
072700     MOVE GCG-PKG-CODE            TO DET1-PKG-CODE1.              GC024050
072800     MOVE GCG-FAM-REL-LVL         TO DET1-FRL1.                   GC024050
072900     MOVE GCG-EFF-DT              TO WS-JUL-DATE.                 GC024050
073000     PERFORM R0090-CONVERT-JUL-TO-GREG THRU R0090-EXIT.           GC024050
073100     MOVE WS-GREG-DATE            TO DET1-DATE1.                  GC024050
073200     IF PROCESS-AOL-VS-ADL                                        GC024050
073300         MOVE GAD-PROVISION-ID        TO DET1-TAB1-ID             GC024050
073400         MOVE GAD-PROVISION-SLOT-NO   TO WS-UNPK-SLOT             GC024050
073500         MOVE WS-UNPK-SLOT            TO DET1-TAB1-SLOT           GC024050
073600         MOVE WS-CHECK-VALUE          TO DET1-BEN-PER1.           GC024050
073700     IF PROCESS-ADL-VS-AOL                                        GC024050
073800         MOVE GAC-PROVISION-ID        TO DET1-TAB1-ID             GC024050
073900         MOVE GAC-PROVISION-SLOT-NO   TO WS-UNPK-SLOT             GC024050
074000         MOVE WS-UNPK-SLOT            TO DET1-TAB1-SLOT           GC024050
074100         MOVE WS-CHECK-VALUE          TO DET1-BEN-PER1.           GC024050
074200     MOVE SPACES TO DET1-PLAN-CODE2                               GC024050
074300                    DET1-GRP2                                     GC024050
074400                    DET1-SECTN2                                   GC024050
074500                    DET1-PKG-CODE2                                GC024050
074600                    DET1-LOB2                                     GC024050
074700                    DET1-PVC2                                     GC024050
074800                    DET1-FRL2                                     GC024050
074900                    DET1-DATE2                                    GC024050
075000                    DET1-TAB2                                     GC024050
075100                    DET1-BEN-PER2.                                GC024050
075200     PERFORM R0120-PRINT THRU R0120-EXIT.                         GC024050
075300                                                                  GC024050
075400     IF PROCESS-AOL-VS-ADL                                        GC024050
075500         MOVE GAC-PROVISION-ID       TO DET1-TAB1-ID              GC024050
075600         MOVE GAC-PROVISION-SLOT-NO  TO WS-UNPK-SLOT              GC024050
075700         MOVE WS-UNPK-SLOT           TO DET1-TAB1-SLOT            GC024050
075800         MOVE H-BEN-PER              TO DET1-BEN-PER1.            GC024050
075900                                                                  GC024050
076000     IF PROCESS-ADL-VS-AOL                                        GC024050
076100         MOVE GAD-PROVISION-ID       TO DET1-TAB1-ID              GC024050
076200         MOVE GAD-PROVISION-SLOT-NO  TO WS-UNPK-SLOT              GC024050
076300         MOVE WS-UNPK-SLOT           TO DET1-TAB1-SLOT            GC024050
076400         MOVE H-BEN-PER              TO DET1-BEN-PER1.            GC024050
076500                                                                  GC024050
076600     PERFORM R0120-PRINT THRU R0120-EXIT.                         GC024050
076700 R0100-EXIT.                                                      GC024050
076800     EXIT.                                                        GC024050
076900                                                                  GC024050
077000 R0110-FORMAT-CONTRACT-ERROR.                                     GC024050
077100     MOVE GCG-PLAN-CODE           TO DET1-PLAN-CODE1.             GC024050
077200     MOVE GCG-GROUP-NUM           TO DET1-GRP1.                   GC024050
077300     MOVE GCG-SECTION-NUM         TO DET1-SECTN1.                 GC024050
077400     MOVE GCG-PKG-CODE            TO DET1-PKG-CODE1.              GC024050
077500     MOVE GCG-FAM-REL-LVL         TO DET1-FRL1.                   GC024050
077600     MOVE GCG-EFF-DT              TO WS-JUL-DATE.                 GC024050
077700     PERFORM R0090-CONVERT-JUL-TO-GREG THRU R0090-EXIT.           GC024050
077800     MOVE WS-GREG-DATE            TO DET1-DATE1.                  GC024050
077900     IF PROCESS-AOL-VS-ADL                                        GC024050
078000         MOVE GAD-PROVISION-ID        TO DET1-TAB1-ID             GC024050
078100         MOVE GAD-PROVISION-SLOT-NO   TO WS-UNPK-SLOT             GC024050
078200         MOVE WS-UNPK-SLOT            TO DET1-TAB1-SLOT           GC024050
078300*        MOVE GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)                GC024050
078400         MOVE WS-CHECK-VALUE                                      GC024050
078500                                      TO DET1-BEN-PER1.           GC024050
078600     IF PROCESS-ADL-VS-AOL                                        GC024050
078700         MOVE GAC-PROVISION-ID        TO DET1-TAB1-ID             GC024050
078800         MOVE GAC-PROVISION-SLOT-NO   TO WS-UNPK-SLOT             GC024050
078900         MOVE WS-UNPK-SLOT            TO DET1-TAB1-SLOT           GC024050
079000*        MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)                 GC024050
079100         MOVE WS-CHECK-VALUE                                      GC024050
079200                                      TO DET1-BEN-PER1.           GC024050
079300     MOVE GCT-PLAN-CODE        TO DET1-PLAN-CODE2.                GC024050
079400     MOVE GCT-GROUP-NUM        TO DET1-GRP2.                      GC024050
079500     MOVE GCT-SECTION-NUM      TO DET1-SECTN2.                    GC024050
079600     MOVE GCT-PKG-CODE         TO DET1-PKG-CODE2.                 GC024050
079700     MOVE GCT-L-O-B            TO DET1-LOB2.                      GC024050
079800     MOVE GCT-PROVDR-CONTROL   TO DET1-PVC2.                      GC024050
079900     MOVE GCT-FAM-REL-LVL      TO DET1-FRL2.                      GC024050
080000     MOVE GCT-EFF-DT           TO WS-JUL-DATE.                    GC024050
080100     PERFORM R0090-CONVERT-JUL-TO-GREG THRU R0090-EXIT.           GC024050
080200     MOVE WS-GREG-DATE          TO DET1-DATE2.                    GC024050
080300     IF PROCESS-AOL-VS-ADL                                        GC024050
080400         MOVE GAC-PROVISION-ID      TO DET1-TAB2-ID               GC024050
080500         MOVE GAC-PROVISION-SLOT-NO TO WS-UNPK-SLOT               GC024050
080600         MOVE WS-UNPK-SLOT          TO DET1-TAB2-SLOT             GC024050
080700*        MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)                 GC024050
080800         MOVE H-BEN-PER                                           GC024050
080900                                    TO DET1-BEN-PER2              GC024050
081000         PERFORM R0120-PRINT THRU R0120-EXIT.                     GC024050
081100     IF PROCESS-ADL-VS-AOL                                        GC024050
081200         MOVE GAD-PROVISION-ID      TO DET1-TAB2-ID               GC024050
081300         MOVE GAD-PROVISION-SLOT-NO TO WS-UNPK-SLOT               GC024050
081400         MOVE WS-UNPK-SLOT          TO DET1-TAB2-SLOT             GC024050
081500*        MOVE GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)                GC024050
081600         MOVE H-BEN-PER                                           GC024050
081700                                    TO DET1-BEN-PER2              GC024050
081800         PERFORM R0120-PRINT THRU R0120-EXIT.                     GC024050
081900 R0110-EXIT.                                                      GC024050
082000     EXIT.                                                        GC024050
082100                                                                  GC024050
082200 R0120-PRINT.                                                     GC024050
082300     IF WS-LINE-CNT > 60                                          GC024050
082400         ADD 1 TO WS-PAGE-CNT                                     GC024050
082500         MOVE WS-PAGE-CNT TO HDR1-PG-NO                           GC024050
082600         MOVE WS-HDR1 TO PRINT-AREA                               GC024050
082700         CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER             GC024050
082800         MOVE WS-HDR2 TO PRINT-AREA                               GC024050
082900         CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER             GC024050
083000         MOVE WS-HDR3 TO PRINT-AREA                               GC024050
083100         CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER             GC024050
083200         MOVE WS-GRP-CON-COL-HDR1 TO PRINT-AREA                   GC024050
083300         CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER             GC024050
083400         MOVE WS-GRP-CON-COL-HDR2 TO PRINT-AREA                   GC024050
083500         CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER             GC024050
083600         MOVE 6 TO WS-LINE-CNT.                                   GC024050
083700     MOVE WS-GRP-CON-DET1 TO PRINT-AREA.                          GC024050
083800     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024050
083900     MOVE 1 TO WS-SKIP-SW,                                        GC024050
084000               WS-REPORT-SW.                                      GC024050
084100     ADD 1 TO WS-LINE-CNT.                                        GC024050
084200 R0120-EXIT.                                                      GC024050
084300     EXIT.                                                        GC024050
084400                                                                  GC024050
084500 R0130-MOVE-GRPSPC-TABS-TO-WS.                                    GC024050
084600     IF GCG-TAB-ID (GCG-INDEX) = '#ADL  '                         GC024050
084700         MOVE GCG-GRP-SPEC-TAB-ID (GCG-INDEX) TO WS-ADL-KEY.      GC024050
084800     IF GCG-TAB-ID (GCG-INDEX) = '#AOL  '                         GC024050
084900         MOVE GCG-GRP-SPEC-TAB-ID (GCG-INDEX) TO WS-AOL-KEY.      GC024050
085000 R0130-EXIT.                                                      GC024050
085100     EXIT.                                                        GC024050
085200/                                                                 GC024050
085300 R0140-PRINT.                                                     GC024050
085400     ADD 1 TO WS-PAGE-CNT.                                        GC024050
085500     MOVE WS-PAGE-CNT TO HDR1-PG-NO.                              GC024050
085600     MOVE WS-HDR1 TO PRINT-AREA.                                  GC024050
085700     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024050
085800     MOVE WS-HDR2 TO PRINT-AREA.                                  GC024050
085900     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024050
086000     MOVE WS-HDR3 TO PRINT-AREA.                                  GC024050
086100     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024050
086200     MOVE WS-GRP-CON-COL-HDR1 TO PRINT-AREA.                      GC024050
086300     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024050
086400     MOVE WS-GRP-CON-COL-HDR2 TO PRINT-AREA.                      GC024050
086500     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024050
086600     MOVE WS-NO-RECORD-MSG TO PRINT-AREA.                         GC024050
086700     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024050
086800 R0140-EXIT.                                                      GC024050
086900     EXIT.                                                        GC024050
087000                                                                  GC024050
087100/                                                                 GC024050
087200 0900-CLOSE-FILES.                                                GC024050
087300                                                                  GC024050
087400     MOVE 'C' TO REQUEST-TYPE-1.                                  GC024050
087500     CALL 'TSGVSAM1' USING PARM-ONE   PARM-ONEA.                  GC024050
087600     IF  REQUEST-TYPE-1 NOT = 'C'                                 GC024050
087700         DISPLAY 'BAD CLOSE TABULAR  FILE    '                    GC024050
087800         MOVE ONEA-FEEDBACK  TO ABEND-CODE                        GC024050
087900         GO TO 99999-ERROR-RTN.                                   GC024050
088000                                                                  GC024050
088100     MOVE 'C' TO REQUEST-TYPE-2.                                  GC024050
088200     CALL 'TSGVSAM2' USING PARM-TWO   PARM-TWOA.                  GC024050
088300     IF  REQUEST-TYPE-2 NOT = 'C'                                 GC024050
088400         DISPLAY 'BAD CLOSE CONTRACT FILE    '                    GC024050
088500         MOVE TWOA-FEEDBACK  TO ABEND-CODE                        GC024050
088600         GO TO 99999-ERROR-RTN.                                   GC024050
088700                                                                  GC024050
088800     CLOSE GRPSPC-FILE.                                           GC024050
088900                                                                  GC024050
089000     MOVE HR-TECH-TRAILER TO PRINT-AREA.                          GC024050
089100     CALL 'RIPWRITE' USING PRINT-AREA RIP-STACKER.                GC024050
089200     CALL 'RIPFINIS'.                                             GC024050
089300                                                                  GC024050
089400 0900-EXIT. EXIT.                                                 GC024050
089500                                                                  GC024050
089600 99998-FALL-THROUGH-TRAP.                                         GC024050
089700                                                                  GC024050
089800     DISPLAY 'GC024050 - FALL-THROUGH LOGIC ERROR'.               GC024050
089900     MOVE +0001 TO ABEND-CODE.                                    GC024050
090000                                                                  GC024050
090100 99999-ERROR-RTN.                                                 GC024050
090200                                                                  GC024050
090300     CALL 'TSGEND' USING ABEND-CODE.                              GC024050
090400                                                                  GC024050
090500 99999-EXIT. EXIT.                                                GC024050
