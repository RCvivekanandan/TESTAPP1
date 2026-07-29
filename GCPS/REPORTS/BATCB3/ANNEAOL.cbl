000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    ANNEAOL.                                          00000200
000300 AUTHOR.        ANNE KING.                                        00000300
000400 DATE-WRITTEN.  MAY 2009.                                         00000400
000500 DATE-COMPILED.                                                   00000500
000600***************************************************************   00000600
000700*                                                                 00000700
000800*    R8002  - #ADL - OUT OF POCKET LIMITS                         00000800
000900*    -------------------------------                              00000900
001000*                                                                 00001000
001100*    PROCESSING                                                   00001100
001200*    ----------                                                   00001200
001300*    1. READ CONTRACT FILE SEQ                                    00001300
001400*    2. SELECT ACTIVE GROUPS                                      00001400
001500*    3. LOOK UP #ADL                                              00001500
001600*    4. TEST #ADL FIELDS.                                         00001600
001700*    5. IF THE OUT-OF-POCKET DEFINITION FIELD = '17'              00001700
001800*          PRINT GROUP KEY.                                       00001800
001900*    6. IF PARM='IL' WRITES IL AND OK REPORTS                     00001900
002000*    7. IF PARM='TX' WRITES TX REPORT.                            00002000
002100***************************************************************   00002100
002200******************************************************************00002200
002300*                                                                 00002300
002400*            U P D A T E   H I S T O R Y                          00002400
002500*            ---------------------------                          00002500
002600*                                                                 00002600
002700* CHG #    DATE    BY   DESCRIPTION                               00002700
002800* -----  --------  ---  -------------------------------------     00002800
002900*        05-10-06  JG   NEW PROGRAM,MODIFIED GCM1132 FOR OK.      00002900
003000*                                                                 00003000
003100*        05-27-09  AKK  CHANGE TO READ #ADL                       00003100
003200*                                                                 00003200
003300******************************************************************00003300
003400 ENVIRONMENT DIVISION.                                            00003400
003500 CONFIGURATION SECTION.                                           00003500
003600 SOURCE-COMPUTER.  IBM-370.                                       00003600
003700 OBJECT-COMPUTER.  IBM-370.                                       00003700
003800 SPECIAL-NAMES.                                                   00003800
003900     C01 IS TOP-OF-PAGE.                                          00003900
004000 INPUT-OUTPUT SECTION.                                            00004000
004100 FILE-CONTROL.                                                    00004100
004200                                                                  00004200
004300     SELECT IL-REPORT   ASSIGN TO GCRPTIL.                        00004300
004400     SELECT OK-REPORT   ASSIGN TO GCRPTOK.                        00004400
004500     SELECT TX-REPORT   ASSIGN TO GCRPTTX.                        00004500
004600     SELECT NM-REPORT   ASSIGN TO GCRPTNM.                        00004600
004700                                                                  00004700
004800     SELECT TRSRT-FILE                                            00004800
004900            ASSIGN TO TRSMASTR                                    00004900
005000            ORGANIZATION IS INDEXED                               00005000
005100            ACCESS MODE IS DYNAMIC                                00005100
005200            RECORD KEY IS B113M-MASTER-KEY-AREA                   00005200
005300            FILE STATUS IS FS-TRSRT.                              00005300
005400                                                                  00005400
005500 DATA DIVISION.                                                   00005500
005600 FILE SECTION.                                                    00005600
005700                                                                  00005700
005800 FD  IL-REPORT                                                    00005800
005900     LABEL RECORDS ARE STANDARD                                   00005900
006000     RECORDING MODE IS F                                          00006000
006100     BLOCK CONTAINS 0 RECORDS.                                    00006100
006200 01  IL-RPT-REC        PIC X(80).                                 00006200
006300                                                                  00006300
006400 FD  OK-REPORT                                                    00006400
006500     LABEL RECORDS ARE STANDARD                                   00006500
006600     RECORDING MODE IS F                                          00006600
006700     BLOCK CONTAINS 0 RECORDS.                                    00006700
006800 01  OK-RPT-REC        PIC X(80).                                 00006800
006900                                                                  00006900
007000 FD  TX-REPORT                                                    00007000
007100     LABEL RECORDS ARE STANDARD                                   00007100
007200     RECORDING MODE IS F                                          00007200
007300     BLOCK CONTAINS 0 RECORDS.                                    00007300
007400 01  TX-RPT-REC        PIC X(80).                                 00007400
007500                                                                  00007500
007600 FD  NM-REPORT                                                    00007600
007700     LABEL RECORDS ARE STANDARD                                   00007700
007800     RECORDING MODE IS F                                          00007800
007900     BLOCK CONTAINS 0 RECORDS.                                    00007900
008000 01  NM-RPT-REC        PIC X(80).                                 00008000
008100                                                                  00008100
008200 FD  TRSRT-FILE                                                   00008200
008300     DATA RECORD IS TRSRT-RECORD.                                 00008300
008400                                                                  00008400
008500 01  TRSRT-RECORD.                                                00008500
008600 COPY RDTB113M.                                                   00008600
008700                                                                  00008700
008800 WORKING-STORAGE SECTION.                                         00008800
008900                                                                  00008900
009000 01  JOB-INFO.                                                    00009000
009100     05  PROGRAM-NAME                  PIC X(08) VALUE 'GCR08002'.00009100
009200                                                                  00009200
009300 01  WS-ABEND-CODE                         PIC 9(04)  COMP.       00009300
009400                                                                  00009400
009500 01  WS-AREAS.                                                    00009500
009600     05  WS-CON-EOF-SW             PIC X(01) VALUE SPACES.        00009600
009700         88  CON-EOF                         VALUE 'Y'.           00009700
009800                                                                  00009800
009900     05  WS-ADL-SW                 PIC X(01) VALUE SPACES.        00009900
010000         88  ADL-FOUND                       VALUE 'Y'.           00010000
010100                                                                  00010100
010200     05  WS-CONDITIONS-FOUND-SW    PIC X(01) VALUE SPACES.        00010200
010300         88  CONDITIONS-FOUND                VALUE 'Y'.           00010300
010400                                                                  00010400
010500     05  WS-INVALID-TR-KEY-SW       PIC X(01) VALUE SPACES.       00010500
010600         88  INVALID-TR-KEY                   VALUE 'Y'.          00010600
010700                                                                  00010700
010800     05  WS-1                  PIC X(01) VALUE '1'.               00010800
010900     05  WS-AOL                PIC X(06) VALUE '#AOL  '.          00010900
011000     05  WS-ADL                PIC X(06) VALUE '#ADL  '.          00011000
011100*    05  WS-0B                 PIC X(02) VALUE '0B'.              00011100
011200     05  WS-YES                PIC X(01) VALUE 'Y'.               00011200
011300     05  WS-LINE-50            PIC X(050) VALUE ALL '-'.          00011300
011400     05  WS-LINE-85            PIC X(085) VALUE ALL '-'.          00011400
011500     05  WS-BLANK-LINE         PIC X(100) VALUE ALL ' '.          00011500
011600     05  WS-9999365            PIC S9(07) VALUE +9999365  COMP-3. 00011600
011700     05  WS-5000               PIC S9(07) VALUE +5000     COMP-3. 00011700
011800     05  WS-10000              PIC S9(07) VALUE +10000    COMP-3. 00011800
011900     05  WS-EFF-DATE           PIC 9(08)  VALUE 0.                00011900
012000                                                                  00012000
012100     05  WS-PLAN-SW                PIC XX VALUE SPACES.           00012100
012200         88  ILLINOIS-PLAN                VALUE 'IL'.             00012200
012300         88  OKLAHOMA-PLAN                VALUE 'OK'.             00012300
012400         88  TEXAS-PLAN                   VALUE 'TX'.             00012400
012500                                                                  00012500
012600*    05  WS-CON-UPD                PIC S9(07) VALUE +0 COMP-3.    00012600
012700     05  WS-CON-READ               PIC S9(07) VALUE +0 COMP-3.    00012700
012800*    05  WS-CON-EXC                PIC S9(07) VALUE +0 COMP-3.    00012800
012900     05  WS-DSP-TOT                PIC S9(07) VALUE +0 COMP-3.    00012900
013000     05  WS-RPT-IL                 PIC S9(07) VALUE +0 COMP-3.    00013000
013100     05  WS-RPT-NM                 PIC S9(07) VALUE +0 COMP-3.    00013100
013200     05  WS-RPT-OK                 PIC S9(07) VALUE +0 COMP-3.    00013200
013300     05  WS-RPT-TX                 PIC S9(07) VALUE +0 COMP-3.    00013300
013400     05  WS-RPT-TOT                PIC S9(07) VALUE +0 COMP-3.    00013400
013500     05  WS-ADL-TOT                PIC S9(07) VALUE +0 COMP-3.    00013500
013600     05  WS-ADL-HITS               PIC S9(07) VALUE +0 COMP-3.    00013600
013700     05  WS-ACTV                   PIC S9(07) VALUE +0 COMP-3.    00013700
013800     05  WS-TERM                   PIC S9(07) VALUE +0 COMP-3.    00013800
013900*    05  WS-TVS-TOT                PIC S9(07) VALUE +0 COMP-3.    00013900
014000     05  WS-PROTO              PIC S9(07) VALUE +0 COMP-3.        00014000
014100     05  WS-PRO-IL             PIC S9(07) VALUE +0 COMP-3.        00014100
014200     05  WS-PRO-OK             PIC S9(07) VALUE +0 COMP-3.        00014200
014300     05  WS-TR-TOT             PIC S9(07) VALUE +0 COMP-3.        00014300
014400     05  WS-IL-TOT             PIC S9(07) VALUE +0 COMP-3.        00014400
014500     05  WS-NM-TOT             PIC S9(07) VALUE +0 COMP-3.        00014500
014600     05  WS-TX-TOT             PIC S9(07) VALUE +0 COMP-3.        00014600
014700     05  WS-OK-TOT             PIC S9(07) VALUE +0 COMP-3.        00014700
014800     05  WS-NO-TR              PIC S9(07) VALUE +0 COMP-3.        00014800
014900     05  WS-FOUND-ON-TVS       PIC S9(07) VALUE +0 COMP-3.        00014900
015000/                                                                 00015000
015100     05  WS-PROTOTYPE-SW          PIC X(6) VALUE SPACES.          00015100
015200         88  ILLINOIS-PROTOTYPE            VALUE                  00015200
015300              'BAEHMO'   'BLCHSL'   'CPOOP1'  'RKDCMM'            00015300
015400              'BLUALT'   'BESHCA'   'CPOOP2'  'RKDHOS'            00015400
015500              'BLUEPR'   'BESHSA'   'CPOOP3'  'RKDPPO'            00015500
015600              'BLUEDG'   '0BEHSA'   'CPOPL1'                      00015600
015700              'BLUDEC'   'HALLGP'   'CPOPL2'                      00015700
015800              'BLUECH'   'HALLMK'   'CPOPL3'.                     00015800
015900                                                                  00015900
016000         88  NEW-MEXICO-PROTOTYPE          VALUE                  00016000
016100              'JEMOPT' 'NMOPT1' '0CLCQZ' 'ACCESS'                 00016100
016200              'LANLHM' 'NMOPT2' 'CTYVGS' 'BCHOIC'                 00016200
016300              'LANLPR' 'NMOPT3' '000PPO' 'NEWCON'                 00016300
016400              'LANLWW' 'STJOES' 'NMCHIP' 'HSO100'                 00016400
016500              'MEXICO' '0CLCR5' 'NMALNC' 'HSO250'                 00016500
016600              'HSO500' '0HSO1K' 'HST100' 'HST250'                 00016600
016700              'HST500' 'HST750' '0HST1K' 'CRVOUT'                 00016700
016800              'SELECT' 'NUMONE' '0MAJOR' '0NM104'                 00016800
016900              '0NM105'.                                           00016900
017000                                                                  00017000
017100                                                                  00017100
017200     05  WS-DATE.                                                 00017200
017300         10  WS-DATE-YR        PIC X(02).                         00017300
017400         10  WS-DATE-MO        PIC X(02).                         00017400
017500         10  WS-DATE-DY        PIC X(02).                         00017500
017600                                                                  00017600
017700     COPY MLDATE01.                                               00017700
017800                                                                  00017800
017900******************************************************************00017900
018000*    PARAMETERS FOR TRANSROUTING FILE                             00018000
018100******************************************************************00018100
018200 01  WS-TRSRT-AREA.                                               00018200
018300     05  FS-TRSRT                    PIC 99  VALUE  ZEROES.       00018300
018400                                                                  00018400
018500     05  WS-IO-TRSRT-AREA.                                        00018500
018600         10  WS-GRP-TRSRTE.                                       00018600
018700             15  WS-IO-GROUP-NUM     PIC X(9).                    00018700
018800             15  WS-TRANRTE-ID       PIC XX.                      00018800
018900         10  WS-TRANS-CODE           PIC X(4).                    00018900
019000         10  WS-COV-CD               PIC X.                       00019000
019100         10  WS-SUB-DIGITS.                                       00019100
019200             15 WS-SUB-DIGIT-LOW     PIC XX.                      00019200
019300             15 WS-SUB-DIGIT-HIGH    PIC XX.                      00019300
019400         10  WS-REC-IDENTIFIER       PIC X(4).                    00019400
019500             88  WS-ACTIVE-RECORD               VALUE  '1600'.    00019500
019600             88  WS-ACTIVE-RECORD               VALUE  '1601'.    00019600
019700                                                                  00019700
019800******************************************************************00019800
019900*    SET PARAMETERS VSAM FILES                                    00019900
020000******************************************************************00020000
020100 01  PARM-SET.                                                    00020100
020200     02  SET-RDW.                                                 00020200
020300         10  SET-RECORD-LENGTH        PIC 9(04) VALUE 8  COMP.    00020300
020400         10  SET-FEEDBACK-CODE        PIC 9(04) VALUE ZEROES COMP.00020400
020500     02  SET-VALUE                    PIC 9(08) VALUE 3  COMP.    00020500
020600                                                                  00020600
020700 01  FILLER                                PIC X(12) VALUE        00020700
020800     ' CONTRACT   '.                                              00020800
020900******************************************************************00020900
021000*    PARAMETERS FOR THE CONTRACT FILE             -TSGVSAM4      *00021000
021100******************************************************************00021100
021200 01  CON-PARM-1.                                                  00021200
021300     02  RESERVED                     PIC 9(08)  VALUE 0 COMP.    00021300
021400     02  RESERVED-X    REDEFINES    RESERVED.                     00021400
021500         10  CON-REQUEST-TYPE         PIC X(01) .                 00021500
021600         10  FILLER                   PIC X(03).                  00021600
021700                                                                  00021700
021800 01  CON-PARM-2.                                                  00021800
021900     02  CON-CODE-AREA.                                           00021900
022000         10  CON-RECORD-LENGTH        PIC 9(02)           COMP.   00022000
022100         10  CON-FEEDBACK-CODE        PIC 9(02)  VALUE 0  COMP.   00022100
022200     02  CON-RECORD-AREA.                                         00022200
022300         COPY   GCCONTRC.                                         00022300
022400                                                                  00022400
022500 01  FILLER                                PIC X(12) VALUE        00022500
022600     ' TABULAR    '.                                              00022600
022700******************************************************************00022700
022800*    PARAMETERS FOR THE TABULAR FILE              -TSGVSAM5      *00022800
022900******************************************************************00022900
023000 01  TAB-PARM-ONE.                                                00023000
023100     02  RESERVED                     PIC 9(08)  VALUE 0 COMP.    00023100
023200     02  RESERVED-X      REDEFINES    RESERVED.                   00023200
023300         10  TAB-REQUEST-TYPE         PIC X(01).                  00023300
023400         10  FILLER-2                 PIC X(03).                  00023400
023500                                                                  00023500
023600 01  ADL-PARM-TWO.                                                00023600
023700     04  ADL-CODE-AREA.                                           00023700
023800         10  ADL-RECORD-LENGTH        PIC 9(02)  VALUE 0  COMP.   00023800
023900         10  ADL-FEEDBACK-CODE        PIC 9(02)           COMP.   00023900
024000     04  ADL-REC-AREA.                                            00024000
024100         COPY GCTAOLC.                                            00024100
024200                                                                  00024200
024300******************************************************************00024300
024400*    PRINTER CONTROL AND PRINT LINES.                             00024400
024500******************************************************************00024500
024600 01  PRINTER-CONTROL.                                             00024600
024700     05  LINES-PRINTED         PIC S9(03) VALUE +0 COMP-3.        00024700
024800     05  PAGE-COUNT            PIC S9(03) VALUE +1 COMP-3.        00024800
024900         88  FIRST-PAGE                   VALUE +1.               00024900
025000     05  PAGE-SIZE             PIC S9(03) VALUE +55 COMP-3.       00025000
025100     05  WS-TITLE-IL           PIC X(23)   VALUE                  00025100
025200                                    'ILLINOIS CONTRACTS     '.    00025200
025300     05  WS-TITLE-OK           PIC X(23)   VALUE                  00025300
025400                                    'OKLA CONTRACTS   '.          00025400
025500     05  WS-TITLE-TX           PIC X(23)   VALUE                  00025500
025600                                    'TEXAS CONTRACTS        '.    00025600
025700                                                                  00025700
025800 01  TITLE-1.                                                     00025800
025900     05  FILLER                PIC X     VALUE SPACES.            00025900
026000     05  FILLER                PIC X(32) VALUE                    00026000
026100                               'M-1132 - #ADL '.                  00026100
026200     05  FILLER                PIC X(30) VALUE SPACES.            00026200
026300     05  T-MON                 PIC 99.                            00026300
026400     05  FILLER                PIC X     VALUE '/'.               00026400
026500     05  T-DAY                 PIC 99.                            00026500
026600     05  FILLER                PIC X     VALUE '/'.               00026600
026700     05  T-YEAR                PIC 99.                            00026700
026800     05  FILLER                PIC X(09) VALUE SPACES.            00026800
026900                                                                  00026900
027000 01  TITLE-2.                                                     00027000
027100     05  FILLER                PIC X     VALUE SPACES.            00027100
027200     05  T-PLAN                PIC X(23).                         00027200
027300     05  FILLER                PIC X(56) VALUE SPACES.            00027300
027400                                                                  00027400
027500 01  RPT-HEADER.                                                  00027500
027600     05  FILLER              PIC X     VALUE SPACES.              00027600
027700     05  FILLER              PIC X(04) VALUE SPACES.              00027700
027800     05  FILLER              PIC X(05) VALUE 'GROUP'.             00027800
027900     05  FILLER              PIC X(04) VALUE SPACES.              00027900
028000     05  FILLER              PIC X(05) VALUE 'SECTN'.             00028000
028100     05  FILLER              PIC X(03) VALUE SPACES.              00028100
028200     05  FILLER              PIC X(03) VALUE 'PKG'.               00028200
028300     05  FILLER              PIC X(02) VALUE SPACES.              00028300
028400     05  FILLER              PIC X(03) VALUE 'LOB'.               00028400
028500     05  FILLER              PIC X(02) VALUE SPACES.              00028500
028600     05  FILLER              PIC X(03) VALUE 'PVC'.               00028600
028700     05  FILLER              PIC X(02) VALUE SPACES.              00028700
028800     05  FILLER              PIC X(02) VALUE 'FR'.                00028800
028900     05  FILLER              PIC X(04) VALUE SPACES.              00028900
029000     05  FILLER              PIC X(08) VALUE 'EFF DATE'.          00029000
029100     05  FILLER              PIC X(10) VALUE SPACES.              00029100
029200     05  FILLER              PIC X(04) VALUE SPACES.              00029200
029300     05  FILLER              PIC X(09) VALUE SPACES.              00029300
029400                                                                  00029400
029500 01  UNDER-LINE.                                                  00029500
029600     05  FILLER              PIC X     VALUE SPACES.              00029600
029700     05  FILLER              PIC X(04) VALUE SPACES.              00029700
029800     05  FILLER              PIC X(05) VALUE '-----'.             00029800
029900     05  FILLER              PIC X(04) VALUE SPACES.              00029900
030000     05  FILLER              PIC X(05) VALUE '-----'.             00030000
030100     05  FILLER              PIC X(03) VALUE SPACES.              00030100
030200     05  FILLER              PIC X(03) VALUE '---'.               00030200
030300     05  FILLER              PIC X(02) VALUE SPACES.              00030300
030400     05  FILLER              PIC X(03) VALUE '---'.               00030400
030500     05  FILLER              PIC X(02) VALUE SPACES.              00030500
030600     05  FILLER              PIC X(03) VALUE '---'.               00030600
030700     05  FILLER              PIC X(02) VALUE SPACES.              00030700
030800     05  FILLER              PIC X(02) VALUE '--'.                00030800
030900     05  FILLER              PIC X(04) VALUE SPACES.              00030900
031000     05  FILLER              PIC X(08) VALUE '--------'.          00031000
031100     05  FILLER              PIC X(08) VALUE SPACES.              00031100
031200     05  FILLER              PIC X(04) VALUE SPACES.              00031200
031300     05  FILLER              PIC X(17) VALUE SPACES.              00031300
031400                                                                  00031400
031500 01  DETAIL-LINE.                                                 00031500
031600     05  FILLER              PIC X(01) VALUE SPACE.               00031600
031700     05  FILLER              PIC X(02) VALUE SPACES.              00031700
031800     05  D-GRP               PIC X(09).                           00031800
031900     05  FILLER              PIC X(02) VALUE SPACES.              00031900
032000     05  D-SEC               PIC X(05).                           00032000
032100     05  FILLER              PIC X(03) VALUE SPACES.              00032100
032200     05  D-PKG               PIC X(03).                           00032200
032300     05  FILLER              PIC X(03) VALUE SPACES.              00032300
032400     05  D-LOB               PIC X(01).                           00032400
032500     05  FILLER              PIC X(03) VALUE SPACES.              00032500
032600     05  D-PVC               PIC X(02).                           00032600
032700     05  FILLER              PIC X(03) VALUE SPACES.              00032700
032800     05  D-FRL               PIC X(02).                           00032800
032900     05  FILLER              PIC X(03) VALUE SPACES.              00032900
033000     05  D-EFFDT             PIC 99/99/9999.                      00033000
033100     05  FILLER              PIC X(05) VALUE SPACES.              00033100
033200     05  FILLER              PIC X(03) VALUE SPACES.              00033200
033300     05  FILLER              PIC X(05) VALUE SPACES.              00033300
033400     05  FILLER              PIC X(15) VALUE SPACES.              00033400
033500                                                                  00033500
033600 01  RPT-LINER.                                                   00033600
033700     05  FILLER              PIC X     VALUE SPACES.              00033700
033800     05  FILLER              PIC X(79) VALUE ALL '='.             00033800
033900                                                                  00033900
034000*01  RECORD-LENGTHS.                                              00034000
034100***  COPY GCCDRLEN.                                               00034100
034200                                                                  00034200
034300 LINKAGE SECTION.                                                 00034300
034400                                                                  00034400
034500 01  PARM-AREA.                                                   00034500
034600     05  PARM-LENGTH            PIC S9(4)  COMP.                  00034600
034700     05  PARM-PLAN              PIC XX.                           00034700
034800                                                                  00034800
034900 PROCEDURE DIVISION USING PARM-AREA.                              00034900
035000******************************************************************00035000
035100*                                                                *00035100
035200******************************************************************00035200
035300 1000-JOB-INFO.                                                   00035300
035400     DISPLAY WS-LINE-50.                                          00035400
035500     DISPLAY ' GCR08002 - #ADL - DEDUCTIBLE  LIMITS'.             00035500
035600     DISPLAY WS-LINE-50.                                          00035600
035700                                                                  00035700
035800 1000-PARM-CHECK.                                                 00035800
035900     IF PARM-LENGTH = 0                                           00035900
036000        DISPLAY  ' ABENDED ON PARM LENGTH: ' PARM-LENGTH          00036000
036100        GO TO 9999-ERROR-RTN                                      00036100
036200     END-IF.                                                      00036200
036300                                                                  00036300
036400     IF PARM-PLAN = 'IL' OR 'TX' OR 'OK'                          00036400
036500        MOVE PARM-PLAN TO WS-PLAN-SW                              00036500
036600     ELSE                                                         00036600
036700        DISPLAY  ' INVALID PARM PLAN = ' PARM-PLAN                00036700
036800        GO TO 9999-ERROR-RTN                                      00036800
036900     END-IF.                                                      00036900
037000                                                                  00037000
037100 1000-MAINLINE.                                                   00037100
037200     PERFORM 9000-START-UP THRU 9000-EXIT.                        00037200
037300     PERFORM 8000-READ-CONTRACT THRU 8000-EXIT.                   00037300
037400     PERFORM 2000-PROCESS-CONTRACT THRU 2000-EXIT                 00037400
037500       UNTIL CON-EOF.                                             00037500
037600     PERFORM 9900-CLOSE-FILES THRU 9900-EXIT.                     00037600
037700     STOP RUN.                                                    00037700
037800 1000-EXIT.                                                       00037800
037900     EXIT.                                                        00037900
038000                                                                  00038000
038100******************************************************************00038100
038200*    PROCESS CONTRACT FILE - ACTIVE GROUPS ONLY                   00038200
038300******************************************************************00038300
038400 2000-PROCESS-CONTRACT.                                           00038400
038500                                                                  00038500
038600     IF WS-DSP-TOT = WS-5000                                      00038600
038700        DISPLAY ' GROUPS READ = ' WS-CON-READ ' '                 00038700
038800                  GCT-GROUP-NUM ' '  GCT-SECTION-NUM              00038800
038900        MOVE +0 TO WS-DSP-TOT                                     00038900
039000     END-IF.                                                      00039000
039100                                                                  00039100
039200     IF GCT-TERMDT-CEN = WS-9999365                               00039200
039300        ADD +1 TO WS-ACTV                                         00039300
039400     ELSE                                                         00039400
039500        ADD +1 TO WS-TERM                                         00039500
039600        PERFORM 8000-READ-CONTRACT THRU 8000-EXIT                 00039600
039700        GO TO 2000-EXIT                                           00039700
039800     END-IF.                                                      00039800
039900                                                                  00039900
040000***  CURRENT RECORD PROCESSING                                    00040000
040100                                                                  00040100
040200     MOVE SPACES TO WS-CONDITIONS-FOUND-SW.                       00040200
040300***  PROCESS TEXAS                                                00040300
040400     IF TEXAS-PLAN                                                00040400
040500***     DISPLAY ' TEXAS '                                         00040500
040600        ADD +1 TO WS-TX-TOT                                       00040600
040700        PERFORM 2500-LOOK-UP-ADL THRU 2500-EXIT                   00040700
040800         VARYING GCT-TAB-INDEX FROM 1 BY 1                        00040800
040900           UNTIL GCT-CON-TAB-ID (GCT-TAB-INDEX) > WS-ADL          00040900
041000        IF CONDITIONS-FOUND                                       00041000
041100           PERFORM 4000-OUTPUT THRU 4000-EXIT                     00041100
041200           PERFORM 8000-READ-CONTRACT THRU 8000-EXIT              00041200
041300           GO TO 2000-EXIT                                        00041300
041400        ELSE                                                      00041400
041500           PERFORM 8000-READ-CONTRACT THRU 8000-EXIT              00041500
041600           GO TO 2000-EXIT                                        00041600
041700        END-IF                                                    00041700
041800     END-IF.                                                      00041800
041900                                                                  00041900
042000***  PROCESS PROTOTYPES                                           00042000
042100*    MOVE GCT-GRP-NO TO WS-PROTOTYPE-SW.                          00042100
042200*    IF ILLINOIS-PROTOTYPE                                        00042200
042300*        OR NEW-MEXICO-PROTOTYPE                                  00042300
042400***     DISPLAY ' PROTO SW = ' WS-PROTOTYPE-SW                    00042400
042500*       ADD +1 TO WS-PROTO                                        00042500
042600*       PERFORM 2500-LOOK-UP-ADL THRU 2500-EXIT                   00042600
042700*        VARYING GCT-TAB-INDEX FROM 1 BY 1                        00042700
042800*          UNTIL GCT-CON-TAB-ID (GCT-TAB-INDEX) > WS-ADL          00042800
042900*       IF CONDITIONS-FOUND                                       00042900
043000*          PERFORM 4100-OUTPUT-P THRU 4100-EXIT                   00043000
043100*          PERFORM 8000-READ-CONTRACT THRU 8000-EXIT              00043100
043200*          GO TO 2000-EXIT                                        00043200
043300*       ELSE                                                      00043300
043400*          PERFORM 8000-READ-CONTRACT THRU 8000-EXIT              00043400
043500*          GO TO 2000-EXIT                                        00043500
043600*       END-IF                                                    00043600
043700*    END-IF.                                                      00043700
043800*                                                                 00043800
043900     PERFORM 6000-READ-TRANS  THRU 6000-EXIT.                     00043900
044000     IF WS-IO-GROUP-NUM NOT =  B113M-GRP-NUMBER                   00044000
044100        ADD +1 TO WS-NO-TR                                        00044100
044200***     DISPLAY ' NOT ON TRANS ROUTE FILE '  GCT-GROUP-NUM        00044200
044300        PERFORM 8000-READ-CONTRACT THRU 8000-EXIT                 00044300
044400        GO TO 2000-EXIT                                           00044400
044500     END-IF.                                                      00044500
044600                                                                  00044600
044700     IF INVALID-TR-KEY                                            00044700
044800        PERFORM 8000-READ-CONTRACT THRU 8000-EXIT                 00044800
044900        GO TO 2000-EXIT                                           00044900
045000     END-IF.                                                      00045000
045100                                                                  00045100
045200     PERFORM 6100-CHECK-STATE-CODE THRU 6100-EXIT.                00045200
045300***  DISPLAY ' STATE CODE = '  B113M-TRSRTE-ID                    00045300
045400     PERFORM 2500-LOOK-UP-ADL THRU 2500-EXIT                      00045400
045500      VARYING GCT-TAB-INDEX FROM 1 BY 1                           00045500
045600        UNTIL GCT-CON-TAB-ID (GCT-TAB-INDEX) > WS-ADL             00045600
045700     IF CONDITIONS-FOUND                                          00045700
045800        PERFORM 4000-OUTPUT THRU 4000-EXIT                        00045800
045900     END-IF.                                                      00045900
046000                                                                  00046000
046100     PERFORM 8000-READ-CONTRACT THRU 8000-EXIT.                   00046100
046200 2000-EXIT.                                                       00046200
046300     EXIT.                                                        00046300
046400                                                                  00046400
046500 2500-LOOK-UP-ADL.                                                00046500
046600***  CHECK FOR #ADL                                               00046600
046700     IF GCT-CON-TAB-ID (GCT-TAB-INDEX) = WS-ADL                   00046700
046800        IF GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  >  +0                00046800
046900           ADD +1 TO WS-ADL-TOT                                   00046900
047000           PERFORM 2700-PROCESS-ADL THRU 2700-EXIT                00047000
047100        END-IF                                                    00047100
047200     END-IF.                                                      00047200
047300 2500-EXIT.                                                       00047300
047400     EXIT.                                                        00047400
047500                                                                  00047500
047600 2700-PROCESS-ADL.                                                00047600
047700***  PRIME KEY                                                    00047700
047800     MOVE GCT-CON-TAB-ID   (GCT-TAB-INDEX)                        00047800
047900       TO GAD-PROVISION-ID.                                       00047900
048000     MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)                        00048000
048100       TO GAD-PROVISION-SLOT-NO.                                  00048100
048200                                                                  00048200
048300***  READ FOR #ADL                                                00048300
048400     MOVE 'R' TO TAB-REQUEST-TYPE.                                00048400
048500     MOVE 14  TO ADL-RECORD-LENGTH.                               00048500
048600                                                                  00048600
048700     CALL 'TSGVSAM5' USING  TAB-PARM-ONE  ADL-PARM-TWO.           00048700
048800     IF TAB-REQUEST-TYPE NOT = 'R'                                00048800
048900        DISPLAY  ' TABULAR FILE   READ ERROR  '                   00048900
049000                 GAD-PROVISION-ID ' '   GAD-PROVISION-SLOT-NO     00049000
049100        MOVE ADL-FEEDBACK-CODE TO WS-ABEND-CODE                   00049100
049200        GO TO 9999-ERROR-RTN                                      00049200
049300     END-IF.                                                      00049300
049400                                                                  00049400
049500***  SEARCH #ADL FOR SPRCIFIED VALUES                             00049500
049600***  MOVE SPACES TO WS-CONDITIONS-FOUND-SW.                       00049600
049700     PERFORM 2705-TEST-ADL THRU 2705-EXIT                         00049700
049800       VARYING GAD-INDEX FROM 1 BY 1                              00049800
049900         UNTIL GAD-INDEX = GAD-ENTRY-COUNT                        00049900
050000            OR CONDITIONS-FOUND.                                  00050000
050100 2700-EXIT.                                                       00050100
050200     EXIT.                                                        00050200
050300                                                                  00050300
050400*************M****************************************************00050400
050500*    TEST #ADL -OPX DEFINITION  = 12                              00050500
050600******************************************************************00050600
050700 2705-TEST-ADL.                                                   00050700
050800     IF GAD-O-P-X-DEFINITION (GAD-INDEX) = '17'                   00050800
050900       CONTINUE                                                   00050900
051000     ELSE                                                         00051000
051100        GO TO 2705-EXIT                                           00051100
051200     END-IF.                                                      00051200
051300                                                                  00051300
051400     ADD +1 TO WS-ADL-HITS.                                       00051400
051500     MOVE WS-YES TO WS-CONDITIONS-FOUND-SW.                       00051500
051600 2705-EXIT.                                                       00051600
051700     EXIT.                                                        00051700
051800                                                                  00051800
051900**************************************************************    00051900
052000*    PRINT CONTRACT KEY ON REPORT                                 00052000
052100**************************************************************    00052100
052200 4000-OUTPUT.                                                     00052200
052300     PERFORM 5000-REPORT THRU 5000-EXIT.                          00052300
052400                                                                  00052400
052500 4000-EXIT.                                                       00052500
052600     EXIT.                                                        00052600
052700                                                                  00052700
052800* 4100-OUTPUT-P.                                                  00052800
052900*     PERFORM 5100-REPORT-P THRU 5100-EXIT.                       00052900
053000* 4100-EXIT.                                                      00053000
053100*     EXIT.                                                       00053100
053200                                                                  00053200
053300**************************************************************    00053300
053400*   PRINT CONTRACT RECORD TO REPORT                               00053400
053500**************************************************************    00053500
053600 5000-REPORT.                                                     00053600
053700***  DISPLAY ' 5000 REPORT'                                       00053700
053800     MOVE GCT-GROUP-NUM       TO  D-GRP                           00053800
053900     MOVE GCT-SECTION-NUM     TO  D-SEC                           00053900
054000     MOVE GCT-PKG-CODE        TO  D-PKG                           00054000
054100     MOVE GCT-L-O-B           TO  D-LOB                           00054100
054200     MOVE GCT-PROVDR-CONTROL  TO  D-PVC                           00054200
054300     MOVE GCT-FAM-REL-LVL     TO  D-FRL                           00054300
054400                                                                  00054400
054500     MOVE 'CNV'          TO MLDATE-FUNC.                          00054500
054600     MOVE 'J'            TO MLDATE-FORM1.                         00054600
054700     MOVE GCT-EFFDT-CEN  TO MLDATE-DATE1.                         00054700
054800     MOVE 'M'            TO MLDATE-FORM2.                         00054800
054900     CALL 'MLDATE' USING MLDATE01.                                00054900
055000     MOVE MLDATE-DATE2   TO WS-EFF-DATE.                          00055000
055100     MOVE  WS-EFF-DATE   TO D-EFFDT.                              00055100
055200                                                                  00055200
055300     IF TEXAS-PLAN                                                00055300
055400***     DISPLAY ' TEXAS REPORT '                                  00055400
055500        WRITE TX-RPT-REC FROM DETAIL-LINE                         00055500
055600        ADD +1 TO WS-RPT-TX                                       00055600
055700                  WS-RPT-TOT                                      00055700
055800        MOVE SPACES TO DETAIL-LINE                                00055800
055900        GO TO 5000-EXIT                                           00055900
056000     END-IF.                                                      00056000
056100                                                                  00056100
056200     IF B113M-STATE-IL                                            00056200
056300        WRITE IL-RPT-REC FROM DETAIL-LINE                         00056300
056400        ADD +1 TO WS-RPT-IL                                       00056400
056500     END-IF.                                                      00056500
056600                                                                  00056600
056700     IF B113M-STATE-OK                                            00056700
056800        WRITE OK-RPT-REC FROM DETAIL-LINE                         00056800
056900        ADD +1 TO WS-RPT-OK                                       00056900
057000     END-IF.                                                      00057000
057100                                                                  00057100
057200     IF B113M-STATE-NM                                            00057200
057300        WRITE NM-RPT-REC FROM DETAIL-LINE                         00057300
057400        ADD +1 TO WS-RPT-NM                                       00057400
057500     END-IF.                                                      00057500
057600                                                                  00057600
057700     ADD +1 TO WS-RPT-TOT.                                        00057700
057800     MOVE SPACES TO DETAIL-LINE.                                  00057800
057900 5000-EXIT.                                                       00057900
058000     EXIT.                                                        00058000
058100                                                                  00058100
058200*5100-REPORT-P.                                                   00058200
058300*    MOVE GCT-GROUP-NUM       TO  D-GRP                           00058300
058400*    MOVE GCT-SECTION-NUM     TO  D-SEC                           00058400
058500*    MOVE GCT-PKG-CODE        TO  D-PKG                           00058500
058600*    MOVE GCT-L-O-B           TO  D-LOB                           00058600
058700*    MOVE GCT-PROVDR-CONTROL  TO  D-PVC                           00058700
058800*    MOVE GCT-FAM-REL-LVL     TO  D-FRL                           00058800
058900*                                                                 00058900
059000*    MOVE 'CNV'          TO MLDATE-FUNC.                          00059000
059100*    MOVE 'J'            TO MLDATE-FORM1.                         00059100
059200*    MOVE GCT-EFFDT-CEN  TO MLDATE-DATE1.                         00059200
059300*    MOVE 'M'            TO MLDATE-FORM2.                         00059300
059400*    CALL 'MLDATE' USING MLDATE01.                                00059400
059500*    MOVE MLDATE-DATE2   TO WS-EFF-DATE.                          00059500
059600*    MOVE  WS-EFF-DATE   TO D-EFFDT.                              00059600
059700*                                                                 00059700
059800***  DISPLAY ' -- PROTO SW   = '  WS-PROTOTYPE-SW                 00059800
059900*    IF ILLINOIS-PROTOTYPE                                        00059900
060000*       WRITE IL-RPT-REC FROM DETAIL-LINE                         00060000
060100*       ADD +1 TO WS-RPT-IL                                       00060100
060200*                 WS-PRO-IL                                       00060200
060300*    END-IF.                                                      00060300
060400*                                                                 00060400
060500*    IF NEW-MEXICO-PROTOTYPE                                      00060500
060600*       WRITE OK-RPT-REC FROM DETAIL-LINE                         00060600
060700*       ADD +1 TO WS-RPT-OK                                       00060700
060800*                 WS-PRO-OK                                       00060800
060900*    END-IF.                                                      00060900
061000*                                                                 00061000
061100*    ADD +1 TO WS-RPT-TOT.                                        00061100
061200*    MOVE SPACES TO DETAIL-LINE.                                  00061200
061300*5100-EXIT.                                                       00061300
061400*    EXIT.                                                        00061400
061500                                                                  00061500
061600 6000-READ-TRANS.                                                 00061600
061700***  DISPLAY ' 3000 READ TR '.                                    00061700
061800     MOVE SPACES           TO  WS-INVALID-TR-KEY-SW.              00061800
061900     MOVE LOW-VALUES       TO  WS-IO-TRSRT-AREA                   00061900
062000     MOVE GCT-GROUP-NUM    TO  WS-IO-GROUP-NUM                    00062000
062100     MOVE HIGH-VALUES      TO  WS-TRANS-CODE                      00062100
062200     MOVE '00'             TO  WS-SUB-DIGIT-LOW                   00062200
062300     MOVE '99'             TO  WS-SUB-DIGIT-HIGH                  00062300
062400     MOVE WS-IO-TRSRT-AREA TO  B113M-MASTER-KEY-AREA              00062400
062500                                                                  00062500
062600     START TRSRT-FILE KEY NOT <  B113M-MASTER-KEY-AREA            00062600
062700       INVALID KEY                                                00062700
062800         MOVE WS-YES TO WS-INVALID-TR-KEY-SW                      00062800
062900         GO TO 6000-EXIT.                                         00062900
063000                                                                  00063000
063100     IF FS-TRSRT  =  '00'                                         00063100
063200        READ TRSRT-FILE NEXT RECORD                               00063200
063300        IF FS-TRSRT  =  '00'                                      00063300
063400***        MOVE WS-IO-GROUP-NUM TO WS-LAST-TR-GROUP               00063400
063500           ADD +1 TO WS-TR-TOT                                    00063500
063600        ELSE                                                      00063600
063700          DISPLAY ' '                                             00063700
063800          DISPLAY '****  TRANS ROUTING FILE AT READ  ****'        00063800
063900          DISPLAY '    FILE STATUS = ' FS-TRSRT                   00063900
064000          DISPLAY 'FILE STATUS KEY = ' B113M-MASTER-KEY-AREA      00064000
064100          MOVE SPACES TO  B113M-GRP-NUMBER                        00064100
064200          MOVE '7100' TO  WS-ABEND-CODE                           00064200
064300          CALL 'TSGEND' USING  WS-ABEND-CODE                      00064300
064400        END-IF                                                    00064400
064500     ELSE                                                         00064500
064600        DISPLAY ' '                                               00064600
064700        DISPLAY '****  TRANS ROUTING FILE AT START  ****'         00064700
064800        DISPLAY '    FILE STATUS = ' FS-TRSRT                     00064800
064900        DISPLAY 'FILE STATUS KEY = ' B113M-MASTER-KEY-AREA        00064900
065000        MOVE SPACES    TO  B113M-GRP-NUMBER                       00065000
065100        MOVE '7100'    TO  WS-ABEND-CODE                          00065100
065200        CALL 'TSGEND'  USING  WS-ABEND-CODE                       00065200
065300     END-IF.                                                      00065300
065400 6000-EXIT.                                                       00065400
065500     EXIT.                                                        00065500
065600                                                                  00065600
065700 6100-CHECK-STATE-CODE.                                           00065700
065800     IF B113M-STATE-IL                                            00065800
065900        ADD +1 TO WS-IL-TOT                                       00065900
066000        GO TO 6100-EXIT                                           00066000
066100     END-IF.                                                      00066100
066200                                                                  00066200
066300     IF B113M-STATE-OK                                            00066300
066400        ADD +1 TO WS-OK-TOT                                       00066400
066500     END-IF.                                                      00066500
066600                                                                  00066600
066700     IF B113M-STATE-NM                                            00066700
066800        ADD +1 TO WS-NM-TOT                                       00066800
066900     END-IF.                                                      00066900
067000 6100-EXIT.                                                       00067000
067100     EXIT.                                                        00067100
067200                                                                  00067200
067300                                                                  00067300
067400                                                                  00067400
067500******************************************************************00067500
067600*    CONTRACT READ                                                00067600
067700******************************************************************00067700
067800 8000-READ-CONTRACT.                                              00067800
067900     MOVE  'G' TO  CON-REQUEST-TYPE.                              00067900
068000     CALL 'TSGVSAM4' USING CON-PARM-1                             00068000
068100                           CON-PARM-2.                            00068100
068200                                                                  00068200
068300     IF CON-REQUEST-TYPE = 'G'                                    00068300
068400        ADD +1 TO  WS-CON-READ                                    00068400
068500                   WS-DSP-TOT                                     00068500
068600     ELSE                                                         00068600
068700        IF CON-REQUEST-TYPE         =   '2'                       00068700
068800           MOVE WS-YES TO WS-CON-EOF-SW                           00068800
068900           GO TO 8000-EXIT                                        00068900
069000        ELSE                                                      00069000
069100           DISPLAY ' BAD GET FOR THE CONTRACT FILE  TSGVSAM4'     00069100
069200           MOVE CON-FEEDBACK-CODE TO WS-ABEND-CODE                00069200
069300           GO TO 9999-ERROR-RTN                                   00069300
069400        END-IF                                                    00069400
069500     END-IF.                                                      00069500
069600 8000-EXIT.                                                       00069600
069700     EXIT.                                                        00069700
069800                                                                  00069800
069900 9000-START-UP.                                                   00069900
070000***  SET FOR FILES                                                00070000
070100     MOVE 'S'  TO  CON-REQUEST-TYPE                               00070100
070200                   TAB-REQUEST-TYPE.                              00070200
070300                                                                  00070300
070400     CALL 'TSGVSAM4'  USING CON-PARM-1         PARM-SET.          00070400
070500     IF CON-REQUEST-TYPE        NOT EQUAL   'S'                   00070500
070600        DISPLAY  ' CONTRACT FILE SET ERROR'                       00070600
070700        MOVE SET-FEEDBACK-CODE  TO  WS-ABEND-CODE                 00070700
070800        GO TO 9999-ERROR-RTN                                      00070800
070900     END-IF.                                                      00070900
071000                                                                  00071000
071100     CALL 'TSGVSAM5'  USING TAB-PARM-ONE  PARM-SET.               00071100
071200     IF TAB-REQUEST-TYPE   NOT EQUAL   'S'                        00071200
071300        DISPLAY  ' TABULAR FILE SET ERROR'                        00071300
071400        MOVE SET-FEEDBACK-CODE  TO  WS-ABEND-CODE                 00071400
071500        GO TO 9999-ERROR-RTN                                      00071500
071600     END-IF.                                                      00071600
071700                                                                  00071700
071800     ACCEPT WS-DATE FROM DATE.                                    00071800
071900     MOVE WS-DATE-MO  TO T-MON.                                   00071900
072000     MOVE WS-DATE-DY  TO T-DAY.                                   00072000
072100     MOVE WS-DATE-YR  TO T-YEAR.                                  00072100
072200                                                                  00072200
072300     IF ILLINOIS-PLAN                                             00072300
072400        OPEN INPUT  TRSRT-FILE                                    00072400
072500             OUTPUT IL-REPORT                                     00072500
072600                    OK-REPORT                                     00072600
072700                    NM-REPORT                                     00072700
072800        PERFORM 9100-IL-RPT-HEADINGS THRU 9100-EXIT               00072800
072900        PERFORM 9200-OK-RPT-HEADINGS THRU 9200-EXIT               00072900
073000        DISPLAY ' '  WS-TITLE-IL ' & ' WS-TITLE-OK                00073000
073100     END-IF.                                                      00073100
073200                                                                  00073200
073300     IF TEXAS-PLAN                                                00073300
073400        OPEN OUTPUT TX-REPORT                                     00073400
073500        PERFORM 9300-TX-RPT-HEADINGS THRU 9300-EXIT               00073500
073600        DISPLAY ' '  WS-TITLE-TX                                  00073600
073700     END-IF.                                                      00073700
073800                                                                  00073800
073900     DISPLAY WS-LINE-50.                                          00073900
074000     MOVE SPACES TO DETAIL-LINE.                                  00074000
074100***  MOVE LOW-VALUES TO B113M-MASTER-KEY-AREA.                    00074100
074200     MOVE SPACES TO  WS-IO-TRSRT-AREA.                            00074200
074300 9000-EXIT.                                                       00074300
074400     EXIT.                                                        00074400
074500                                                                  00074500
074600 9100-IL-RPT-HEADINGS.                                            00074600
074700     MOVE  WS-TITLE-IL   TO   T-PLAN.                             00074700
074800     WRITE IL-RPT-REC  FROM RPT-LINER.                            00074800
074900     WRITE IL-RPT-REC  FROM TITLE-1.                              00074900
075000     WRITE IL-RPT-REC  FROM TITLE-2.                              00075000
075100     WRITE IL-RPT-REC  FROM RPT-LINER.                            00075100
075200                                                                  00075200
075300     MOVE SPACES TO IL-RPT-REC.                                   00075300
075400     WRITE IL-RPT-REC.                                            00075400
075500                                                                  00075500
075600     WRITE IL-RPT-REC  FROM RPT-HEADER.                           00075600
075700     WRITE IL-RPT-REC  FROM UNDER-LINE.                           00075700
075800 9100-EXIT.                                                       00075800
075900     EXIT.                                                        00075900
076000                                                                  00076000
076100 9200-OK-RPT-HEADINGS.                                            00076100
076200     MOVE  WS-TITLE-OK   TO   T-PLAN.                             00076200
076300     WRITE OK-RPT-REC  FROM RPT-LINER.                            00076300
076400     WRITE OK-RPT-REC  FROM TITLE-1.                              00076400
076500     WRITE OK-RPT-REC  FROM TITLE-2.                              00076500
076600     WRITE OK-RPT-REC  FROM RPT-LINER.                            00076600
076700                                                                  00076700
076800     MOVE SPACES TO OK-RPT-REC.                                   00076800
076900     WRITE OK-RPT-REC.                                            00076900
077000                                                                  00077000
077100     WRITE OK-RPT-REC  FROM RPT-HEADER.                           00077100
077200     WRITE OK-RPT-REC  FROM UNDER-LINE.                           00077200
077300 9200-EXIT.                                                       00077300
077400     EXIT.                                                        00077400
077500                                                                  00077500
077600 9300-TX-RPT-HEADINGS.                                            00077600
077700     MOVE  WS-TITLE-TX   TO   T-PLAN.                             00077700
077800     WRITE TX-RPT-REC  FROM RPT-LINER.                            00077800
077900     WRITE TX-RPT-REC  FROM TITLE-1.                              00077900
078000     WRITE TX-RPT-REC  FROM TITLE-2.                              00078000
078100     WRITE TX-RPT-REC  FROM RPT-LINER.                            00078100
078200                                                                  00078200
078300     MOVE SPACES TO TX-RPT-REC.                                   00078300
078400     WRITE TX-RPT-REC.                                            00078400
078500                                                                  00078500
078600     WRITE TX-RPT-REC  FROM RPT-HEADER.                           00078600
078700     WRITE TX-RPT-REC  FROM UNDER-LINE.                           00078700
078800 9300-EXIT.                                                       00078800
078900     EXIT.                                                        00078900
079000                                                                  00079000
079100 9700-DISPLAY-CON.                                                00079100
079200     DISPLAY WS-LINE-50.                                          00079200
079300     DISPLAY ' CURRENT RECORD = '                                 00079300
079400              GCT-GROUP-NUM ' '  GCT-SECTION-NUM ' '              00079400
079500              GCT-PKG-CODE ' ' GCT-L-O-B ' '                      00079500
079600              GCT-PROVDR-CONTROL ' ' GCT-FAM-REL-LVL ' '          00079600
079700              GCT-EFFDT-CEN ' ' GCT-TERMDT-CEN.                   00079700
079800 9700-EXIT.                                                       00079800
079900     EXIT.                                                        00079900
080000                                                                  00080000
080100 9900-CLOSE-FILES.                                                00080100
080200     IF ILLINOIS-PLAN                                             00080200
080300        CLOSE IL-REPORT                                           00080300
080400              OK-REPORT                                           00080400
080500              NM-REPORT                                           00080500
080600              TRSRT-FILE                                          00080600
080700     END-IF.                                                      00080700
080800                                                                  00080800
080900     IF TEXAS-PLAN                                                00080900
081000        CLOSE TX-REPORT                                           00081000
081100     END-IF.                                                      00081100
081200                                                                  00081200
081300     MOVE 'C'  TO  CON-REQUEST-TYPE                               00081300
081400                   TAB-REQUEST-TYPE.                              00081400
081500                                                                  00081500
081600     CALL 'TSGVSAM4' USING  CON-PARM-1         PARM-SET.          00081600
081700     IF CON-REQUEST-TYPE        NOT EQUAL   'C'                   00081700
081800        DISPLAY  ' 9500 CONTRACT FILE        CLOSE ERROR'         00081800
081900        MOVE SET-FEEDBACK-CODE TO WS-ABEND-CODE                   00081900
082000        GO TO 9999-ERROR-RTN                                      00082000
082100     END-IF.                                                      00082100
082200                                                                  00082200
082300     CALL 'TSGVSAM5' USING  TAB-PARM-ONE  PARM-SET.               00082300
082400     IF TAB-REQUEST-TYPE   NOT EQUAL   'C'                        00082400
082500        DISPLAY  ' 9500   TABULAR FILE  CLOSE ERROR'              00082500
082600        MOVE SET-FEEDBACK-CODE  TO  WS-ABEND-CODE                 00082600
082700        GO TO 9999-ERROR-RTN                                      00082700
082800     END-IF.                                                      00082800
082900                                                                  00082900
083000                                                                  00083000
083100***  DISPLAY WS-LINE-50.                                          00083100
083200     DISPLAY WS-BLANK-LINE.                                       00083200
083300     DISPLAY ' GCR08002 COUNTS    ------------'.                  00083300
083400                                                                  00083400
083500     DISPLAY WS-BLANK-LINE.                                       00083500
083600     DISPLAY ' CONTRACT TOTALS    ------------'.                  00083600
083700     DISPLAY '   READ             = ' WS-CON-READ.                00083700
083800     DISPLAY '   ACTIVE           = ' WS-ACTV.                    00083800
083900     DISPLAY '   NOT ACTIVE       = ' WS-TERM.                    00083900
084000     DISPLAY '   PROTOS           = ' WS-PROTO.                   00084000
084100     DISPLAY '    IL              = ' WS-PRO-IL.                  00084100
084200     DISPLAY '    OK              = ' WS-PRO-OK.                  00084200
084300     DISPLAY '                    ------------'.                  00084300
084400                                                                  00084400
084500     DISPLAY WS-BLANK-LINE.                                       00084500
084600     DISPLAY ' TRABS ROUTE        ------------'.                  00084600
084700     DISPLAY '   READS            = '  WS-TR-TOT                  00084700
084800     DISPLAY '   ILL              = '  WS-IL-TOT                  00084800
084900     DISPLAY '   OKLA             = '  WS-OK-TOT                  00084900
085000     DISPLAY '   NOT FOUND        = '  WS-NO-TR.                  00085000
085100                                                                  00085100
085200     DISPLAY WS-BLANK-LINE.                                       00085200
085300     DISPLAY ' TABULAR TOTALS     ------------'.                  00085300
085400     DISPLAY '   ADL FOUND        = ' WS-ADL-TOT.                 00085400
085500     DISPLAY '   ADL HITS         = ' WS-ADL-HITS.                00085500
085600     DISPLAY '                    ------------'.                  00085600
085700                                                                  00085700
085800     DISPLAY WS-BLANK-LINE.                                       00085800
085900     DISPLAY ' OUTPUT TOTALS      ------------'.                  00085900
086000*    DISPLAY '   TVS              = ' WS-TVS-TOT.                 00086000
086100*    DISPLAY '   FOUND ON TVS     = ' WS-FOUND-ON-TVS.            00086100
086200     DISPLAY WS-BLANK-LINE.                                       00086200
086300     DISPLAY '   REPORT           = ' WS-RPT-TOT.                 00086300
086400     DISPLAY '    ILLINOIS        = ' WS-RPT-IL.                  00086400
086500     DISPLAY '    OKLA            = ' WS-RPT-OK.                  00086500
086600     DISPLAY '    TEXAS           = ' WS-RPT-TX.                  00086600
086700     DISPLAY '                    ------------'.                  00086700
086800 9900-EXIT.                                                       00086800
086900     EXIT.                                                        00086900
087000                                                                  00087000
087100***************************************************************   00087100
087200*    PROGRAM ABEND                                                00087200
087300***************************************************************   00087300
087400 9999-ERROR-RTN.                                                  00087400
087500      CALL 'TSGEND' USING WS-ABEND-CODE.                          00087500
087600 9999-EXIT.                                                       00087600
087700     EXIT.                                                        00087700
