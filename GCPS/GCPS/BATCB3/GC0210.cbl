000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.         GC0210.                                      00000200
000300 AUTHOR.             DELORES FRY.                                 00000300
000400 INSTALLATION.       HCSC.                                        00000400
000500 DATE-WRITTEN.       JANUARY 1988.                                00000500
000600 DATE-COMPILED.                                                   00000600
000700******************************************************************00000700
000800*                                                                *00000800
000900*          GENERIC CONTRACT PROCESSING SYSTEM  (GCPS)            *00000900
001000*                                                                *00001000
001100*                                                                *00001100
001200*     GC0210 - UPDATES THE VSAM TABULAR FILE WITH NEW TABULARS.  *00001200
001300*              TYPES OF TABULARS:                                *00001300
001400*              INTERNAL - ALL LEVEL ACCUMS - ALL LEVEL           *00001400
001500*              CONTRACT - GROUP SPECIFIC   - PROVISION           *00001500
001600*                                                                *00001600
001700*                                                                *00001700
001800*  INPUT FILE:   INPUT-SLOT-UPDATE-FILES           -GC0210A      *00001800
001900*                THIS FILE IS A CONCATENATION OF THE FOLLOWING   *00001900
002000*                SEQUENTIAL FILES:                               *00002000
002100*                A.  INTERNAL        SLOT UPDATE FILE            *00002100
002200*                B.  ACCUM           SLOT UPDATE FILE            *00002200
002300*                C.  ALL LEVEL       SLOT UPDATE FILE            *00002300
002400*                D.  CONTRACT        SLOT UPDATE FILE            *00002400
002500*                E.  GROUP SPECIFIC  SLOT UPDATE FILE            *00002500
002600*                F.  PROVISION       SLOT UPDATE FILE            *00002600
002700*                                                                *00002700
002800*   I/O  FILE:   TABULAR FILE  -  TSGVSAM1   (VSAM KEY SEQUENCE) *00002800
002900*                                                                *00002900
003000*                                                                *00003000
003100******************************************************************00003100
003200******************************************************************00003200
003300******************************************************************00003300
003400*                                                                *00003400
003500*       ***-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00003500
003600*       *-*         U P D A T E   H I S T O R Y         *-*      *00003600
003700*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00003700
003800*                                                                *00003800
003900*                                                                *00003900
004000**-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION--------------- *00004000
004100*                                                                *00004100
004200*   D1009    1/14/88   FRY     CREATED THIS PROGRAM.......       *00004200
004300*           03/22/88   ENW   ADDED LOGIC TO ADD DATE TO LAST     *00004300
004400*                            DATE OF LAST CHANGE FIELD.          *00004400
004500*   D199     8/14/89   RKH   ADDED 4 NEW TABULARS:                00004500
004600*                            #GVLG - #GVLH -  #GVLQ - #GVLR       00004600
004700*   D184/   01/30/90   ENW   ADDED 2 NEW TABULARS:                00004700
004800*   D185                     #IDGD - #IPGP                        00004800
004900*                                                                 00004900
005000*   D249.01 08/23/90   APH   1. ADDED 2 NEW TABULARS:             00005000
005100*                               #GMCD AND #GMCR                   00005100
005200*                                                                 00005200
005300*   D249    08/31/90   GDM   1. ADDED NEW TABULAR: #GMCG          00005300
005400*                                                                 00005400
005500*                       ----ACCUM TABULAR RECORD MODIFICATION--- *00005500
005600* 11154   11/12/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *00005600
005700* D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *00005700
005800* D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *00005800
005900* D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *00005900
006000*                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *00006000
006100*                       6. CHANGE TABULAR RECORD MAX LENGTH FROM *00006100
006200*                          4000 TO 8157.                         *00006200
006300*                                                                *00006300
006400* 11154    2/21/91  FRY   -DECREASE MAX OCCURS FROM 46 TO 44.    *00006400
006500*                         -DECREASE MAX REC LENGTH FOR ACCUM REC *00006500
006600*                           FROM 8157 TO 7805.                   *00006600
006700*                                                                *00006700
006800* 11154    3/06/91  FRY   -INCREASE RECORD AREA IN FILE SECTION: *00006800
006900*                      INPUT-TABULAR-RECORD.                     *00006900
007000*                       FILLER   PIC  X(3990)  CHANGED TO  7795. *00007000
007100*                                                                *00007100
007200* D11836    05/30/91   BSO   1. ADDED NEW TABULAR: #GMCT         *00007200
007300*                                                                *00007300
007400* D-292  03/02/92  KJD  ADDED NEW TABULAR: #GMCS                 *00007400
007500*                                                                *00007500
007600* 12730  12/05/92  ENW  ADDED NEW TABULAR: #GRPO                 *00007600
007700*                                                                *00007700
007800*D14045  02/14/95  KJD  ADDED NEW TABULAR: #GCPO                 *00007800
007900*                                                                *00007900
008000*D14631   3/11/96  FRY  ADDED NEW TABULARS: #GCBL AND #GPAN      *00008000
008100*                                                                *00008100
008200*D14402   4/11/96  GDM  ADDED NEW TABULAR: #GSUB                 *00008200
008300*                                                                *00008300
008400*  14726     07/28/98  AB   RECOMPILE  TO SUPPORT THE YEAR        00008400
008500*                           2000 GSUB CAPTURE CENTURY IN THE      00008500
008600*                           DATE FIELD(S).                        00008600
008700*                                                                *00008700
008800*D15182  10/12/98  FRY  ADDED #ACP ACCUM TABULAR                 *00008800
008900*D15446                 ADDED #CRS CONTRACT TABULAR              *00008900
009000*                                                                *00009000
009100* D15380     03/24/99  GDM   ADDED NEW TABULAR: #GBAE            *00009100
009200*                                                                *00009200
009300*            07/13/00  GSP   ADDED LOGIC FOR NEW #IPGS INTERNAL  *00009300
009400*                            TABULAR.                            *00009400
009500*                                                                *00009500
009600*            07/12/01  AKK   ADDED LOGIC FOR NEW TABULARS #IRDX, *00009600
009700*                            #IRIC, #IRPR, #IRPV AND #CDRS IN    *00009700
009800*                            SUPPORT OF #CDRS EFFORT.            *00009800
009900*                                                                *00009900
010000*            10/09/02  GTF   RECOMPILE FOR OPID EXPANSION        *00010000
010100*                                                                *00010100
010200*  P02384    09/14/05  GDM   ADD NEW TABULARS: #GFHC, #GFSA,     *00010200
010300*                            #GHCA, #GHSA, #GLPF, #GLPH, #GWHC   *00010300
010400*                                                                *00010400
010500*  DM9400    05/15/07  LR    ADD NEW TABULAR #GMFH               *00010500
010600*                                                                *00010600
TM0526*BBDA-66049  08/10/26  TM    ADD NEW TABULAR #GHPA               *00010610
TM0526*                                                                *00010620
010700******************************************************************00010700
010800******************************************************************00010800
010900 ENVIRONMENT DIVISION.                                            00010900
011000                                                                  00011000
011100 CONFIGURATION SECTION.                                           00011100
011200 SOURCE-COMPUTER.  IBM-370.                                       00011200
011300 OBJECT-COMPUTER.  IBM-370.                                       00011300
011400                                                                  00011400
011500 INPUT-OUTPUT SECTION.                                            00011500
011600                                                                  00011600
011700 FILE-CONTROL.                                                    00011700
011800                                                                  00011800
011900     SELECT   INPUT-SLOT-UPDATE-FILE    ASSIGN TO   UT-S-GC0210A. 00011900
012000                                                                  00012000
012100                                                                  00012100
012200 DATA DIVISION.                                                   00012200
012300 FILE SECTION.                                                    00012300
012400                                                                  00012400
012500 FD  INPUT-SLOT-UPDATE-FILE                                       00012500
012600     LABEL RECORDS ARE STANDARD                                   00012600
012700     RECORDING MODE IS V                                          00012700
012800     BLOCK CONTAINS  0  RECORDS.                                  00012800
012900                                                                  00012900
013000 01  INPUT-SLOT-UPDATE-RECORD.                                    00013000
013100*    05  INPUT-WORK-KEY                    PIC  X(64).            00013100
013200         COPY GCWRKDCC.                                           00013200
013300     05  INPUT-TABULAR-RECORD.                                    00013300
013400         10  INPUT-TABULAR-RECORD-KEY.                            00013400
013500             15  INPUT-TABULAR-ID          PIC  X(06).            00013500
013600             15  INPUT-TABULAR-SLOT        PIC S9(07)     COMP-3. 00013600
013700             15  FILLER                    PIC  X(7795).          00013700
013800/                                                                 00013800
013900                                                                  00013900
014000 WORKING-STORAGE SECTION.                                         00014000
014100                                                                  00014100
014200 01  WS-PROGRAM-ID                 PIC  X(27)  VALUE              00014200
014300                                    '* GC0210 WORKING STORAGE *'. 00014300
014400                                                                  00014400
014500 01  WS-HOLD-AREAS.                                               00014500
014600     05  FILLER                    PIC  X(18)  VALUE              00014600
014700                                   '**  ABEND CODE  **'.          00014700
014800     05  WS-ABEND-CODE             PIC  9(04)  VALUE 0    COMP.   00014800
014900     05  FILLER                    PIC  X(19)  VALUE              00014900
015000                                   '**  TABULAR KEY  **'.         00015000
015100     05  WS-DISPLAY-KEY.                                          00015100
015200         10  WS-TABULAR-ID         PIC  X(06)  VALUE SPACES.      00015200
015300         10  WS-TABULAR-SLOT       PIC  ZZZZZZ9.                  00015300
015400                                                                  00015400
015500                                                                  00015500
015600 01  WS-COUNT-AREAS.                                              00015600
015700     05  FILLER                    PIC  X(21)  VALUE              00015700
015800                                   '**  RECORD COUNTS  **'.       00015800
015900     05  WS-RECORDS-READ           PIC  9(07)  VALUE ZEROES.      00015900
016000     05  WS-RECORDS-ADDED          PIC  9(07)  VALUE ZEROES.      00016000
016100                                                                  00016100
016200 COPY HSCDATES.                                                   00016200
016300                                                                  00016300
016400 01  WS-DATE-AREA.                                                00016400
016500     05  WS-JUL-DATE               PIC  9(05) VALUE ZEROS.        00016500
016600     05  FILLER REDEFINES WS-JUL-DATE.                            00016600
016700         10  WS-YY                 PIC  9(02).                    00016700
016800         10  WS-DDD                PIC  9(03).                    00016800
016900     05  WS-CURR-JUL-DATE          PIC S9(05) VALUE ZEROS COMP-3. 00016900
017000                                                                  00017000
017100                                                                  00017100
017200 01  WS-SWITCHES.                                                 00017200
017300     05  FILLER                    PIC  X(16)  VALUE              00017300
017400                                   '*** SWITCHES ***'.            00017400
017500     05  WS-END-OF-FILE-SW         PIC  X(01)  VALUE '0'.         00017500
017600         88  WS-END-OF-FILE-SWITCH-ON          VALUE '1'.         00017600
017700/                                                                 00017700
017800 01  WS-GCPS-RECORD-LENGTHS.                                      00017800
017900     05  FILLER                    PIC  X(27)  VALUE              00017900
018000                                   '*** GCPS RECORD LENGTHS ***'. 00018000
018100     COPY GCCDRLEN.                                               00018100
018200/                                                                 00018200
018300                                                                  00018300
018400******************************************************************00018400
018500**                                                                00018500
018600**     PARAMETERS FOR THE TABULAR FILE        TSGVSAM1            00018600
018700**                                                                00018700
018800******************************************************************00018800
018900 01  FILLER                            PIC  X(22)  VALUE          00018900
019000                                       '***  TABULAR FILE  ***'.  00019000
019100                                                                  00019100
019200 01  PARM-SET.                                                    00019200
019300     05  SET-VSAM-RDW.                                            00019300
019400        10 SET-VSAM-RECORD-LENGTH      PIC 9(04)     COMP.        00019400
019500        10 SET-VSAM-FEEDBACK-CODE      PIC 9(04)     COMP.        00019500
019600     05  SET-VSAM-VALUE                PIC 9(08)     COMP.        00019600
019700                                                                  00019700
019800                                                                  00019800
019900 01  PARM-TAB-ONE-A.                                              00019900
020000     05  RESERVED-FLDS                 PIC  9(08) VALUE 0   COMP. 00020000
020100     05  RESERVED-ONE     REDEFINES     RESERVED-FLDS.            00020100
020200        10  VSAM-REQUEST-TYPE          PIC  X(01).                00020200
020300        10  FILLER                     PIC  X(03).                00020300
020400                                                                  00020400
020500                                                                  00020500
020600 01  PARM-TAB-ONE-B.                                              00020600
020700     05  VSAM-RDW.                                                00020700
020800        10  VSAM-RECORD-LENGTH         PIC  9(04)    COMP.        00020800
020900        10  VSAM-FEEDBACK-CODE         PIC  9(04)    COMP.        00020900
021000     05  VSAM-RECORD-AREA              PIC  X(7805).              00021000
021100     05  VSAM-RECORD-A       REDEFINES     VSAM-RECORD-AREA.      00021100
021200         10  VSAM-KEY-ID               PIC  X(10).                00021200
021300         10  FILLER                    PIC  X(01).                00021300
021400         10  VSAM-DT-OF-LAST-CHANGE    PIC S9(5) COMP-3.          00021400
021500         10  VSAM-KEY-DATA.                                       00021500
021600            15  FILLER                 PIC  X(23).                00021600
021700            15  VSAM-ENTRY-COUNT       PIC S9(05)    COMP-3.      00021700
021800            15  FILLER                 PIC  X(7765).              00021800
021900                                                                  00021900
022000/                                                                 00022000
022100 PROCEDURE DIVISION.                                              00022100
022200                                                                  00022200
022300******************************************************************00022300
022400**                                                                00022400
022500**               P R O C E S S    C O N T R O L                   00022500
022600**                                                                00022600
022700******************************************************************00022700
022800 0000-MAINLINE.                                                   00022800
022900                                                                  00022900
023000     PERFORM 1000-OPEN-THE-FILES  THRU  1000-EXIT.                00023000
023100                                                                  00023100
023200     IF WS-END-OF-FILE-SWITCH-ON                                  00023200
023300         DISPLAY '  '                                             00023300
023400         DISPLAY ' GC0210   --NO INPUT RECORDS RECEIVED'          00023400
023500         PERFORM 9500-CLOSE-THE-FILES  THRU  9500-EXIT            00023500
023600         STOP RUN.                                                00023600
023700                                                                  00023700
023800     PERFORM 2500-PROCESS-ALL-INPUT-RECORDS THRU 2500-EXIT        00023800
023900         UNTIL  WS-END-OF-FILE-SWITCH-ON.                         00023900
024000                                                                  00024000
024100     PERFORM 9500-CLOSE-THE-FILES  THRU  9500-EXIT.               00024100
024200                                                                  00024200
024300     STOP RUN.                                                    00024300
024400                                                                  00024400
024500 0000-EXIT.                                                       00024500
024600     EXIT.                                                        00024600
024700/                                                                 00024700
024800******************************************************************00024800
024900***                                                               00024900
025000***      OPEN SEQUENTIAL AND VSAM FILES                           00025000
025100***      READ THE FIRST RECORD FROM SEQUENTIAL INPUT FILE         00025100
025200***                                                               00025200
025300******************************************************************00025300
025400 1000-OPEN-THE-FILES.                                             00025400
025500                                                                  00025500
025600     OPEN INPUT  INPUT-SLOT-UPDATE-FILE.                          00025600
025700                                                                  00025700
025800                                                                  00025800
025900**--- SET VSAM FILE FOR SPECIAL PROCESSING                        00025900
026000**                                                                00026000
026100     MOVE  'S'          TO  VSAM-REQUEST-TYPE.                    00026100
026200     MOVE   8           TO  SET-VSAM-RECORD-LENGTH.               00026200
026300     MOVE   3           TO  SET-VSAM-VALUE.                       00026300
026400     CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A    PARM-SET.         00026400
026500                                                                  00026500
026600                                                                  00026600
026700     IF  VSAM-REQUEST-TYPE    NOT EQUAL   'S'                     00026700
026800         DISPLAY '  '                                             00026800
026900         DISPLAY ' BAD SET    GC0210    1000-OPEN-THE-FILES'      00026900
027000         MOVE  SET-VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE         00027000
027100         GO TO  9999-ERROR-RTN.                                   00027100
027200                                                                  00027200
027300                                                                  00027300
027400                                                                  00027400
027500**--- AN \
027600**                                                                00027600
027700     MOVE  'O'          TO  VSAM-REQUEST-TYPE.                    00027700
027800     CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    00027800
027900                                                                  00027900
028000                                                                  00028000
028100     IF  VSAM-REQUEST-TYPE    NOT EQUAL   'O'                     00028100
028200         DISPLAY '  '                                             00028200
028300         DISPLAY ' BAD OPEN     GC0210    1000-OPEN-THE-FILES'    00028300
028400         MOVE  SET-VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE         00028400
028500         GO TO  9999-ERROR-RTN.                                   00028500
028600                                                                  00028600
028700                                                                  00028700
028800     CALL 'TCDTES' USING HSCDATES.                                00028800
028900                                                                  00028900
029000     MOVE JYR  TO WS-YY.                                          00029000
029100     MOVE JDA  TO WS-DDD.                                         00029100
029200     MOVE WS-JUL-DATE TO WS-CURR-JUL-DATE.                        00029200
029300                                                                  00029300
029400**--- READ THE FIRST INPUT RECORD.                                00029400
029500**                                                                00029500
029600     PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT.                 00029600
029700                                                                  00029700
029800 1000-EXIT.                                                       00029800
029900     EXIT.                                                        00029900
030000/                                                                 00030000
030100******************************************************************00030100
030200**         SEQUENTIAL TABULAR SLOT UPDATED FILES                  00030200
030300**         INPUT SEQUENITAL FILES ARE CONCATENATED                00030300
030400******************************************************************00030400
030500 2000-READ-INPUT-FILE.                                            00030500
030600                                                                  00030600
030700     READ INPUT-SLOT-UPDATE-FILE                                  00030700
030800         AT END                                                   00030800
030900             MOVE  '1'   TO   WS-END-OF-FILE-SW                   00030900
031000             DISPLAY '  '                                         00031000
031100             DISPLAY ' RECORDS READ FROM INPUT FILES   =  '       00031100
031200                                         WS-RECORDS-READ          00031200
031300             DISPLAY ' RECORDS ADDED TO VSAM FILE      =  '       00031300
031400                                         WS-RECORDS-ADDED         00031400
031500             GO TO 2000-EXIT.                                     00031500
031600                                                                  00031600
031700                                                                  00031700
031800     ADD  1  TO  WS-RECORDS-READ.                                 00031800
031900                                                                  00031900
032000 2000-EXIT.                                                       00032000
032100     EXIT.                                                        00032100
032200/                                                                 00032200
032300******************************************************************00032300
032400***                                                               00032400
032500***      'N'  NEW TABULAR RECORDS ARE PROCESSED                   00032500
032600***      'E'  EXISTING TABULAR RECORDS ARE NOT PROCESSED          00032600
032700***                                                               00032700
032800******************************************************************00032800
032900 2500-PROCESS-ALL-INPUT-RECORDS.                                  00032900
033000                                                                  00033000
033100**-- CHECK FIELD IN WORK RECORD.                                  00033100
033200**                                                                00033200
033300     IF WRK-SIG-B-NEW-SLOT                                        00033300
033400         PERFORM 3000-PROCESS-THE-TAB-RECORD THRU 3000-EXIT.      00033400
033500                                                                  00033500
033600                                                                  00033600
033700**-- READ ANOTHER TABULAR RECORD.                                 00033700
033800**                                                                00033800
033900     PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT.                 00033900
034000                                                                  00034000
034100 2500-EXIT.                                                       00034100
034200     EXIT.                                                        00034200
034300/                                                                 00034300
034400******************************************************************00034400
034500***                 CHECK TYPE OF TABULAR                         00034500
034600******************************************************************00034600
034700 3000-PROCESS-THE-TAB-RECORD.                                     00034700
034800                                                                  00034800
034900**--CHECK FOR INTERNAL TABULARS.                                  00034900
035000**                                                                00035000
035100     IF INPUT-TABULAR-ID     EQUAL   '#IBGR '                     00035100
035200         PERFORM 4000-PROCESS-IBGR-TABULAR THRU 4000-EXIT         00035200
035300         GO TO 3000-EXIT.                                         00035300
035400                                                                  00035400
035500     IF INPUT-TABULAR-ID     EQUAL   '#IDGD '                     00035500
035600         PERFORM 4050-PROCESS-IDGD-TABULAR THRU 4050-EXIT         00035600
035700         GO TO 3000-EXIT.                                         00035700
035800                                                                  00035800
035900     IF INPUT-TABULAR-ID     EQUAL   '#IPGN '                     00035900
036000         PERFORM 4100-PROCESS-IPGN-TABULAR THRU 4100-EXIT         00036000
036100         GO TO 3000-EXIT.                                         00036100
036200                                                                  00036200
036300     IF INPUT-TABULAR-ID     EQUAL   '#IPGP '                     00036300
036400         PERFORM 4150-PROCESS-IPGP-TABULAR THRU 4150-EXIT         00036400
036500         GO TO 3000-EXIT.                                         00036500
036600                                                                  00036600
036700     IF INPUT-TABULAR-ID     EQUAL   '#IPGT '                     00036700
036800         PERFORM 4200-PROCESS-IPGT-TABULAR THRU 4200-EXIT         00036800
036900         GO TO 3000-EXIT.                                         00036900
037000                                                                  00037000
037100     IF INPUT-TABULAR-ID     EQUAL   '#IPGS '                     00037100
037200         PERFORM 4250-PROCESS-IPGS-TABULAR THRU 4250-EXIT         00037200
037300         GO TO 3000-EXIT.                                         00037300
037400                                                                  00037400
037500     IF INPUT-TABULAR-ID     EQUAL   '#IRDX '                     00037500
037600         PERFORM 4255-PROCESS-IRDX-TABULAR THRU 4255-EXIT         00037600
037700         GO TO 3000-EXIT.                                         00037700
037800                                                                  00037800
037900     IF INPUT-TABULAR-ID     EQUAL   '#IRIC '                     00037900
038000         PERFORM 4260-PROCESS-IRIC-TABULAR THRU 4260-EXIT         00038000
038100         GO TO 3000-EXIT.                                         00038100
038200                                                                  00038200
038300     IF INPUT-TABULAR-ID     EQUAL   '#IRPR '                     00038300
038400         PERFORM 4265-PROCESS-IRPR-TABULAR THRU 4265-EXIT         00038400
038500         GO TO 3000-EXIT.                                         00038500
038600                                                                  00038600
038700     IF INPUT-TABULAR-ID     EQUAL   '#IRPV '                     00038700
038800         PERFORM 4270-PROCESS-IRPV-TABULAR THRU 4270-EXIT         00038800
038900         GO TO 3000-EXIT.                                         00038900
039000                                                                  00039000
039100                                                                  00039100
039200                                                                  00039200
039300**--CHECK FOR ALL LEVEL ACCUM TABULARS.                           00039300
039400**                                                                00039400
039500     IF INPUT-TABULAR-ID     EQUAL   '#ABM  '                     00039500
039600         PERFORM 5000-PROCESS-ABM-TABULAR THRU 5000-EXIT          00039600
039700         GO TO 3000-EXIT.                                         00039700
039800                                                                  00039800
039900     IF INPUT-TABULAR-ID     EQUAL   '#ACL  '                     00039900
040000         PERFORM 5100-PROCESS-ACL-TABULAR THRU 5100-EXIT          00040000
040100         GO TO 3000-EXIT.                                         00040100
040200                                                                  00040200
040300                                                                  00040300
040400     IF INPUT-TABULAR-ID     EQUAL   '#ACP  '                     00040400
040500         PERFORM 5400-PROCESS-ACP-TABULAR THRU 5400-EXIT          00040500
040600         GO TO 3000-EXIT.                                         00040600
040700                                                                  00040700
040800                                                                  00040800
040900     IF INPUT-TABULAR-ID     EQUAL   '#ADL  '                     00040900
041000         PERFORM 5200-PROCESS-ADL-TABULAR THRU 5200-EXIT          00041000
041100         GO TO 3000-EXIT.                                         00041100
041200                                                                  00041200
041300                                                                  00041300
041400     IF INPUT-TABULAR-ID     EQUAL   '#AOL  '                     00041400
041500         PERFORM 5300-PROCESS-AOL-TABULAR THRU 5300-EXIT          00041500
041600         GO TO 3000-EXIT.                                         00041600
041700                                                                  00041700
041800                                                                  00041800
041900                                                                  00041900
042000**--CHECK FOR ALL LEVEL TABULARS.                                 00042000
042100**                                                                00042100
042200     IF INPUT-TABULAR-ID     EQUAL   '#AAR  '                     00042200
042300         PERFORM 6000-PROCESS-AAR-TABULAR THRU 6000-EXIT          00042300
042400         GO TO 3000-EXIT.                                         00042400
042500                                                                  00042500
042600                                                                  00042600
042700     IF INPUT-TABULAR-ID     EQUAL   '#ACON '                     00042700
042800         PERFORM 6100-PROCESS-ACON-TABULAR THRU 6100-EXIT         00042800
042900         GO TO 3000-EXIT.                                         00042900
043000                                                                  00043000
043100                                                                  00043100
043200     IF INPUT-TABULAR-ID     EQUAL   '#ACOS '                     00043200
043300         PERFORM 6200-PROCESS-ACOS-TABULAR THRU 6200-EXIT         00043300
043400         GO TO 3000-EXIT.                                         00043400
043500                                                                  00043500
043600                                                                  00043600
043700     IF INPUT-TABULAR-ID     EQUAL   '#ADIP '                     00043700
043800         PERFORM 6300-PROCESS-ADIP-TABULAR THRU 6300-EXIT         00043800
043900         GO TO 3000-EXIT.                                         00043900
044000                                                                  00044000
044100                                                                  00044100
044200     IF INPUT-TABULAR-ID     EQUAL   '#ADOP '                     00044200
044300         PERFORM 6400-PROCESS-ADOP-TABULAR THRU 6400-EXIT         00044300
044400         GO TO 3000-EXIT.                                         00044400
044500                                                                  00044500
044600                                                                  00044600
044700                                                                  00044700
044800**--CHECK FOR CONTRACT TABULARS.                                  00044800
044900**                                                                00044900
045000     IF INPUT-TABULAR-ID     EQUAL   '#CLDR '                     00045000
045100         PERFORM 7000-PROCESS-CLDR-TABULAR THRU 7000-EXIT         00045100
045200         GO TO 3000-EXIT.                                         00045200
045300                                                                  00045300
045400                                                                  00045400
045500     IF INPUT-TABULAR-ID     EQUAL   '#CRR  '                     00045500
045600         PERFORM 7100-PROCESS-CRR-TABULAR THRU 7100-EXIT          00045600
045700         GO TO 3000-EXIT.                                         00045700
045800                                                                  00045800
045900                                                                  00045900
046000     IF INPUT-TABULAR-ID     EQUAL   '#CRS  '                     00046000
046100         PERFORM 7200-PROCESS-CRS-TABULAR THRU 7200-EXIT          00046100
046200         GO TO 3000-EXIT.                                         00046200
046300                                                                  00046300
046400     IF INPUT-TABULAR-ID     EQUAL   '#CDRS '                     00046400
046500         PERFORM 7250-PROCESS-CDRS-TABULAR THRU 7250-EXIT         00046500
046600         GO TO 3000-EXIT.                                         00046600
046700                                                                  00046700
046800**--CHECK FOR GROUP SPECIFIC TABULARS.                            00046800
046900**                                                                00046900
047000                                                                  00047000
047100     IF INPUT-TABULAR-ID     EQUAL   '#GBAE '                     00047100
047200         PERFORM 8471-PROCESS-GBAE-TABULAR THRU 8471-EXIT         00047200
047300         GO TO 3000-EXIT.                                         00047300
047400                                                                  00047400
047500     IF INPUT-TABULAR-ID     EQUAL   '#GCBL '                     00047500
047600         PERFORM 8100-PROCESS-GCBL-TABULAR THRU 8100-EXIT         00047600
047700         GO TO 3000-EXIT.                                         00047700
047800                                                                  00047800
047900     IF INPUT-TABULAR-ID     EQUAL   '#GCCP '                     00047900
048000         PERFORM 8105-PROCESS-GCCP-TABULAR THRU 8105-EXIT         00048000
048100         GO TO 3000-EXIT.                                         00048100
048200                                                                  00048200
048300     IF INPUT-TABULAR-ID     EQUAL   '#GCPO '                     00048300
048400         PERFORM 8110-PROCESS-GCPO-TABULAR THRU 8110-EXIT         00048400
048500         GO TO 3000-EXIT.                                         00048500
048600                                                                  00048600
048700     IF INPUT-TABULAR-ID     EQUAL   '#GFSB '                     00048700
048800         PERFORM 8120-PROCESS-GFSB-TABULAR THRU 8120-EXIT         00048800
048900         GO TO 3000-EXIT.                                         00048900
049000                                                                  00049000
049100     IF INPUT-TABULAR-ID     EQUAL   '#GHOB '                     00049100
049200         PERFORM 8140-PROCESS-GHOB-TABULAR THRU 8140-EXIT         00049200
049300         GO TO 3000-EXIT.                                         00049300
049400                                                                  00049400
049500     IF INPUT-TABULAR-ID     EQUAL   '#GHOR '                     00049500
049600         PERFORM 8160-PROCESS-GHOR-TABULAR THRU 8160-EXIT         00049600
049700         GO TO 3000-EXIT.                                         00049700
049800                                                                  00049800
049900     IF INPUT-TABULAR-ID     EQUAL   '#GMCD '                     00049900
050000         PERFORM 8170-PROCESS-GMCD-TABULAR THRU 8170-EXIT         00050000
050100         GO TO 3000-EXIT.                                         00050100
050200                                                                  00050200
050300     IF INPUT-TABULAR-ID     EQUAL   '#GMCG '                     00050300
050400         PERFORM 8171-PROCESS-GMCG-TABULAR THRU 8171-EXIT         00050400
050500         GO TO 3000-EXIT.                                         00050500
050600                                                                  00050600
050700     IF INPUT-TABULAR-ID     EQUAL   '#GMCR '                     00050700
050800         PERFORM 8172-PROCESS-GMCR-TABULAR THRU 8172-EXIT         00050800
050900         GO TO 3000-EXIT.                                         00050900
051000                                                                  00051000
051100     IF INPUT-TABULAR-ID     EQUAL   '#GMCS '                     00051100
051200         PERFORM 8174-PROCESS-GMCS-TABULAR THRU 8174-EXIT         00051200
051300         GO TO 3000-EXIT.                                         00051300
051400                                                                  00051400
051500     IF INPUT-TABULAR-ID     EQUAL   '#GMCT '                     00051500
051600         PERFORM 8173-PROCESS-GMCT-TABULAR THRU 8173-EXIT         00051600
051700         GO TO 3000-EXIT.                                         00051700
051800                                                                  00051800
051900     IF INPUT-TABULAR-ID     EQUAL   '#GMDB '                     00051900
052000         PERFORM 8180-PROCESS-GMDB-TABULAR THRU 8180-EXIT         00052000
052100         GO TO 3000-EXIT.                                         00052100
052200                                                                  00052200
052300     IF INPUT-TABULAR-ID     EQUAL   '#GMDN '                     00052300
052400         PERFORM 8200-PROCESS-GMDN-TABULAR THRU 8200-EXIT         00052400
052500         GO TO 3000-EXIT.                                         00052500
052600                                                                  00052600
052700     IF INPUT-TABULAR-ID     EQUAL   '#GMOB '                     00052700
052800         PERFORM 8220-PROCESS-GMOB-TABULAR THRU 8220-EXIT         00052800
052900         GO TO 3000-EXIT.                                         00052900
053000                                                                  00053000
053100     IF INPUT-TABULAR-ID     EQUAL   '#GMOR '                     00053100
053200         PERFORM 8240-PROCESS-GMOR-TABULAR THRU 8240-EXIT         00053200
053300         GO TO 3000-EXIT.                                         00053300
053400                                                                  00053400
053500     IF INPUT-TABULAR-ID     EQUAL   '#GMPB '                     00053500
053600         PERFORM 8260-PROCESS-GMPB-TABULAR THRU 8260-EXIT         00053600
053700         GO TO 3000-EXIT.                                         00053700
053800                                                                  00053800
053900     IF INPUT-TABULAR-ID     EQUAL   '#GMPR '                     00053900
054000         PERFORM 8280-PROCESS-GMPR-TABULAR THRU 8280-EXIT         00054000
054100         GO TO 3000-EXIT.                                         00054100
054200                                                                  00054200
054300     IF INPUT-TABULAR-ID     EQUAL   '#GMSB '                     00054300
054400         PERFORM 8300-PROCESS-GMSB-TABULAR THRU 8300-EXIT         00054400
054500         GO TO 3000-EXIT.                                         00054500
054600                                                                  00054600
054700     IF INPUT-TABULAR-ID     EQUAL   '#GMSC '                     00054700
054800         PERFORM 8320-PROCESS-GMSC-TABULAR THRU 8320-EXIT         00054800
054900         GO TO 3000-EXIT.                                         00054900
055000                                                                  00055000
055100     IF INPUT-TABULAR-ID     EQUAL   '#GMSR '                     00055100
055200         PERFORM 8340-PROCESS-GMSR-TABULAR THRU 8340-EXIT         00055200
055300         GO TO 3000-EXIT.                                         00055300
055400                                                                  00055400
055500     IF INPUT-TABULAR-ID     EQUAL   '#GPAB '                     00055500
055600         PERFORM 8360-PROCESS-GPAB-TABULAR THRU 8360-EXIT         00055600
055700         GO TO 3000-EXIT.                                         00055700
055800                                                                  00055800
055900     IF INPUT-TABULAR-ID     EQUAL   '#GPAC '                     00055900
056000         PERFORM 8380-PROCESS-GPAC-TABULAR THRU 8380-EXIT         00056000
056100         GO TO 3000-EXIT.                                         00056100
056200                                                                  00056200
056300     IF INPUT-TABULAR-ID     EQUAL   '#GPAD '                     00056300
056400         PERFORM 8400-PROCESS-GPAD-TABULAR THRU 8400-EXIT         00056400
056500         GO TO 3000-EXIT.                                         00056500
056600                                                                  00056600
056700     IF INPUT-TABULAR-ID     EQUAL   '#GPAN '                     00056700
056800         PERFORM 8410-PROCESS-GPAN-TABULAR THRU 8410-EXIT         00056800
056900         GO TO 3000-EXIT.                                         00056900
057000                                                                  00057000
057100     IF INPUT-TABULAR-ID     EQUAL   '#GPAR '                     00057100
057200         PERFORM 8420-PROCESS-GPAR-TABULAR THRU 8420-EXIT         00057200
057300         GO TO 3000-EXIT.                                         00057300
057400                                                                  00057400
057500     IF INPUT-TABULAR-ID     EQUAL   '#GPPO '                     00057500
057600         PERFORM 8440-PROCESS-GPPO-TABULAR THRU 8440-EXIT         00057600
057700         GO TO 3000-EXIT.                                         00057700
057800                                                                  00057800
057900     IF INPUT-TABULAR-ID     EQUAL   '#GRID '                     00057900
058000         PERFORM 8460-PROCESS-GRID-TABULAR THRU 8460-EXIT         00058000
058100         GO TO 3000-EXIT.                                         00058100
058200                                                                  00058200
058300     IF INPUT-TABULAR-ID     EQUAL   '#GRPO '                     00058300
058400         PERFORM 8470-PROCESS-GRPO-TABULAR THRU 8470-EXIT         00058400
058500         GO TO 3000-EXIT.                                         00058500
058600                                                                  00058600
058700     IF INPUT-TABULAR-ID     EQUAL   '#GSUB '                     00058700
058800         PERFORM 8475-PROCESS-GSUB-TABULAR THRU 8475-EXIT         00058800
058900         GO TO 3000-EXIT.                                         00058900
059000                                                                  00059000
059100     IF INPUT-TABULAR-ID     EQUAL   '#GVLF '                     00059100
059200         PERFORM 8480-PROCESS-GVLF-TABULAR THRU 8480-EXIT         00059200
059300         GO TO 3000-EXIT.                                         00059300
059400                                                                  00059400
059500     IF INPUT-TABULAR-ID     EQUAL   '#GVLG '                     00059500
059600         PERFORM 8481-PROCESS-GVLG-TABULAR THRU 8481-EXIT         00059600
059700         GO TO 3000-EXIT.                                         00059700
059800                                                                  00059800
059900     IF INPUT-TABULAR-ID     EQUAL   '#GVLH '                     00059900
060000         PERFORM 8482-PROCESS-GVLH-TABULAR THRU 8482-EXIT         00060000
060100         GO TO 3000-EXIT.                                         00060100
060200                                                                  00060200
060300     IF INPUT-TABULAR-ID     EQUAL   '#GVLP '                     00060300
060400         PERFORM 8500-PROCESS-GVLP-TABULAR THRU 8500-EXIT         00060400
060500         GO TO 3000-EXIT.                                         00060500
060600                                                                  00060600
060700     IF INPUT-TABULAR-ID     EQUAL   '#GVLQ '                     00060700
060800         PERFORM 8501-PROCESS-GVLQ-TABULAR THRU 8501-EXIT         00060800
060900         GO TO 3000-EXIT.                                         00060900
061000                                                                  00061000
061100     IF INPUT-TABULAR-ID     EQUAL   '#GVLR '                     00061100
061200         PERFORM 8502-PROCESS-GVLR-TABULAR THRU 8502-EXIT         00061200
061300         GO TO 3000-EXIT.                                         00061300
061400                                                                  00061400
061500                                                                  00061500
061600     IF INPUT-TABULAR-ID     EQUAL   '#GWCD '                     00061600
061700         PERFORM 8520-PROCESS-GWCD-TABULAR THRU 8520-EXIT         00061700
061800         GO TO 3000-EXIT.                                         00061800
061900                                                                  00061900
062000     IF INPUT-TABULAR-ID     EQUAL   '#GFHC '                     00062000
062100         PERFORM 8530-PROCESS-GFHC-TABULAR THRU 8530-EXIT         00062100
062200         GO TO 3000-EXIT.                                         00062200
062300                                                                  00062300
062400     IF INPUT-TABULAR-ID     EQUAL   '#GFSA '                     00062400
062500         PERFORM 8531-PROCESS-GFSA-TABULAR THRU 8531-EXIT         00062500
062600         GO TO 3000-EXIT.                                         00062600
062700                                                                  00062700
062800     IF INPUT-TABULAR-ID     EQUAL   '#GHCA '                     00062800
062900         PERFORM 8532-PROCESS-GHCA-TABULAR THRU 8532-EXIT         00062900
063000         GO TO 3000-EXIT.                                         00063000
063100                                                                  00063100
063200     IF INPUT-TABULAR-ID     EQUAL   '#GHSA '                     00063200
063300         PERFORM 8533-PROCESS-GHSA-TABULAR THRU 8533-EXIT         00063300
063400         GO TO 3000-EXIT.                                         00063400
063500                                                                  00063500
063600     IF INPUT-TABULAR-ID     EQUAL   '#GLPF '                     00063600
063700         PERFORM 8534-PROCESS-GLPF-TABULAR THRU 8534-EXIT         00063700
063800         GO TO 3000-EXIT.                                         00063800
063900                                                                  00063900
064000     IF INPUT-TABULAR-ID     EQUAL   '#GLPH '                     00064000
064100         PERFORM 8535-PROCESS-GLPH-TABULAR THRU 8535-EXIT         00064100
064200         GO TO 3000-EXIT.                                         00064200
064300                                                                  00064300
064400     IF INPUT-TABULAR-ID     EQUAL   '#GWHC '                     00064400
064500         PERFORM 8536-PROCESS-GWHC-TABULAR THRU 8536-EXIT         00064500
064600         GO TO 3000-EXIT.                                         00064600
064700                                                                  00064700
064800     IF INPUT-TABULAR-ID     EQUAL   '#GMFH '                     00064800
064900         PERFORM 8550-PROCESS-GMFH-TABULAR THRU 8550-EXIT         00064900
065000         GO TO 3000-EXIT.                                         00065000
TM0526     IF INPUT-TABULAR-ID     EQUAL   '#GHPA '                     00065010
TM0526         PERFORM 8555-PROCESS-GHPA-TABULAR THRU 8555-EXIT         00065020
TM0526         GO TO 3000-EXIT.                                         00065030
065100                                                                  00065100
065200**--CHECK FOR PROVISION TABULARS.                                 00065200
065300**                                                                00065300
065400     IF INPUT-TABULAR-ID     EQUAL   '#PAQ  '                     00065400
065500         PERFORM 8700-PROCESS-PAQ-TABULAR THRU 8700-EXIT          00065500
065600         GO TO 3000-EXIT.                                         00065600
065700                                                                  00065700
065800                                                                  00065800
065900     IF INPUT-TABULAR-ID     EQUAL   '#PCX  '                     00065900
066000         PERFORM 8720-PROCESS-PCX-TABULAR THRU 8720-EXIT          00066000
066100         GO TO 3000-EXIT.                                         00066100
066200                                                                  00066200
066300                                                                  00066300
066400     IF INPUT-TABULAR-ID     EQUAL   '#PDR  '                     00066400
066500         PERFORM 8740-PROCESS-PDR-TABULAR THRU 8740-EXIT          00066500
066600         GO TO 3000-EXIT.                                         00066600
066700                                                                  00066700
066800                                                                  00066800
066900     IF INPUT-TABULAR-ID     EQUAL   '#PPF  '                     00066900
067000         PERFORM 8760-PROCESS-PPF-TABULAR THRU 8760-EXIT          00067000
067100         GO TO 3000-EXIT.                                         00067100
067200                                                                  00067200
067300                                                                  00067300
067400     IF INPUT-TABULAR-ID     EQUAL   '#PRR  '                     00067400
067500         PERFORM 8780-PROCESS-PRR-TABULAR THRU 8780-EXIT          00067500
067600         GO TO 3000-EXIT.                                         00067600
067700                                                                  00067700
067800                                                                  00067800
067900     IF INPUT-TABULAR-ID     EQUAL   '#PRV  '                     00067900
068000         PERFORM 8800-PROCESS-PRV-TABULAR THRU 8800-EXIT          00068000
068100         GO TO 3000-EXIT.                                         00068100
068200                                                                  00068200
068300                                                                  00068300
068400     IF INPUT-TABULAR-ID     EQUAL   '#PSC  '                     00068400
068500         PERFORM 8820-PROCESS-PSC-TABULAR THRU 8820-EXIT          00068500
068600         GO TO 3000-EXIT.                                         00068600
068700                                                                  00068700
068800                                                                  00068800
068900     IF INPUT-TABULAR-ID     EQUAL   '#PVE  '                     00068900
069000         PERFORM 8840-PROCESS-PVE-TABULAR THRU 8840-EXIT.         00069000
069100                                                                  00069100
069200 3000-EXIT.                                                       00069200
069300     EXIT.                                                        00069300
069400/                                                                 00069400
069500******************************************************************00069500
069600****             INTERNAL  '#IBGR'   TABULAR                      00069600
069700******************************************************************00069700
069800 4000-PROCESS-IBGR-TABULAR.                                       00069800
069900                                                                  00069900
070000     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00070000
070100     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00070100
070200                                                                  00070200
070300     COMPUTE VSAM-RECORD-LENGTH        =                          00070300
070400                              4        +                          00070400
070500         GC-GCTABULR-IBGR-FIXED-LEN    +                          00070500
070600         VSAM-ENTRY-COUNT              *                          00070600
070700         GC-GCTABULR-IBGR-VARY-LEN.                               00070700
070800                                                                  00070800
070900     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00070900
071000                                                                  00071000
071100 4000-EXIT.                                                       00071100
071200     EXIT.                                                        00071200
071300/                                                                 00071300
071400******************************************************************00071400
071500****             INTERNAL  '#IDGD'   TABULAR                      00071500
071600******************************************************************00071600
071700 4050-PROCESS-IDGD-TABULAR.                                       00071700
071800                                                                  00071800
071900     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00071900
072000     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00072000
072100                                                                  00072100
072200     COMPUTE VSAM-RECORD-LENGTH        =                          00072200
072300                              4        +                          00072300
072400         GC-GCTABULR-IDGD-FIXED-LEN    +                          00072400
072500         VSAM-ENTRY-COUNT              *                          00072500
072600         GC-GCTABULR-IDGD-VARY-LEN.                               00072600
072700                                                                  00072700
072800     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00072800
072900                                                                  00072900
073000 4050-EXIT.                                                       00073000
073100     EXIT.                                                        00073100
073200/                                                                 00073200
073300******************************************************************00073300
073400****             INTERNAL  '#IPGN'   TABULAR                      00073400
073500******************************************************************00073500
073600 4100-PROCESS-IPGN-TABULAR.                                       00073600
073700                                                                  00073700
073800     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00073800
073900     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00073900
074000                                                                  00074000
074100     COMPUTE VSAM-RECORD-LENGTH        =                          00074100
074200                              4        +                          00074200
074300         GC-GCTABULR-IPGN-FIXED-LEN    +                          00074300
074400         VSAM-ENTRY-COUNT              *                          00074400
074500         GC-GCTABULR-IPGN-VARY-LEN.                               00074500
074600                                                                  00074600
074700     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00074700
074800                                                                  00074800
074900 4100-EXIT.                                                       00074900
075000     EXIT.                                                        00075000
075100/                                                                 00075100
075200******************************************************************00075200
075300****             INTERNAL  '#IPGP'   TABULAR                      00075300
075400******************************************************************00075400
075500 4150-PROCESS-IPGP-TABULAR.                                       00075500
075600                                                                  00075600
075700     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00075700
075800     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00075800
075900                                                                  00075900
076000     COMPUTE VSAM-RECORD-LENGTH        =                          00076000
076100                              4        +                          00076100
076200         GC-GCTABULR-IPGP-FIXED-LEN    +                          00076200
076300         VSAM-ENTRY-COUNT              *                          00076300
076400         GC-GCTABULR-IPGP-VARY-LEN.                               00076400
076500                                                                  00076500
076600     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00076600
076700                                                                  00076700
076800 4150-EXIT.                                                       00076800
076900     EXIT.                                                        00076900
077000/                                                                 00077000
077100******************************************************************00077100
077200****             INTERNAL  '#IPGT'   TABULAR                      00077200
077300******************************************************************00077300
077400 4200-PROCESS-IPGT-TABULAR.                                       00077400
077500                                                                  00077500
077600     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00077600
077700     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00077700
077800                                                                  00077800
077900     COMPUTE VSAM-RECORD-LENGTH        =                          00077900
078000                              4        +                          00078000
078100         GC-GCTABULR-IPGT-FIXED-LEN    +                          00078100
078200         VSAM-ENTRY-COUNT              *                          00078200
078300         GC-GCTABULR-IPGT-VARY-LEN.                               00078300
078400                                                                  00078400
078500     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00078500
078600                                                                  00078600
078700 4200-EXIT.                                                       00078700
078800     EXIT.                                                        00078800
078900/                                                                 00078900
079000******************************************************************00079000
079100****             INTERNAL  '#IPGS'   TABULAR                      00079100
079200******************************************************************00079200
079300 4250-PROCESS-IPGS-TABULAR.                                       00079300
079400                                                                  00079400
079500     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00079500
079600     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00079600
079700                                                                  00079700
079800     COMPUTE VSAM-RECORD-LENGTH        =                          00079800
079900                              4        +                          00079900
080000         GC-GCTABULR-IPGS-FIXED-LEN    +                          00080000
080100         VSAM-ENTRY-COUNT              *                          00080100
080200         GC-GCTABULR-IPGS-VARY-LEN.                               00080200
080300                                                                  00080300
080400     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00080400
080500                                                                  00080500
080600 4250-EXIT.                                                       00080600
080700     EXIT.                                                        00080700
080800/                                                                 00080800
080900******************************************************************00080900
081000****             INTERNAL  '#IRDX'   TABULAR   FOR CDRS           00081000
081100******************************************************************00081100
081200 4255-PROCESS-IRDX-TABULAR.                                       00081200
081300                                                                  00081300
081400     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00081400
081500     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00081500
081600                                                                  00081600
081700     COMPUTE VSAM-RECORD-LENGTH        =                          00081700
081800                              4        +                          00081800
081900         GC-GCTABULR-IRDX-FIXED-LEN    +                          00081900
082000         VSAM-ENTRY-COUNT              *                          00082000
082100         GC-GCTABULR-IRDX-VARY-LEN.                               00082100
082200                                                                  00082200
082300     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00082300
082400                                                                  00082400
082500 4255-EXIT.                                                       00082500
082600     EXIT.                                                        00082600
082700/                                                                 00082700
082800******************************************************************00082800
082900****             INTERNAL  '#IRIC'   TABULAR   FOR CDRS           00082900
083000******************************************************************00083000
083100 4260-PROCESS-IRIC-TABULAR.                                       00083100
083200                                                                  00083200
083300     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00083300
083400     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00083400
083500                                                                  00083500
083600     COMPUTE VSAM-RECORD-LENGTH        =                          00083600
083700                              4        +                          00083700
083800         GC-GCTABULR-IRIC-FIXED-LEN    +                          00083800
083900         VSAM-ENTRY-COUNT              *                          00083900
084000         GC-GCTABULR-IRIC-VARY-LEN.                               00084000
084100                                                                  00084100
084200     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00084200
084300                                                                  00084300
084400 4260-EXIT.                                                       00084400
084500     EXIT.                                                        00084500
084600/                                                                 00084600
084700******************************************************************00084700
084800****             INTERNAL  '#IRPR'   TABULAR   FOR CDRS           00084800
084900******************************************************************00084900
085000 4265-PROCESS-IRPR-TABULAR.                                       00085000
085100                                                                  00085100
085200     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00085200
085300     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00085300
085400                                                                  00085400
085500     COMPUTE VSAM-RECORD-LENGTH        =                          00085500
085600                              4        +                          00085600
085700         GC-GCTABULR-IRPR-FIXED-LEN    +                          00085700
085800         VSAM-ENTRY-COUNT              *                          00085800
085900         GC-GCTABULR-IRPR-VARY-LEN.                               00085900
086000                                                                  00086000
086100     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00086100
086200                                                                  00086200
086300 4265-EXIT.                                                       00086300
086400     EXIT.                                                        00086400
086500******************************************************************00086500
086600****             INTERNAL  '#IRPV'   TABULAR   FOR CDRS           00086600
086700******************************************************************00086700
086800 4270-PROCESS-IRPV-TABULAR.                                       00086800
086900                                                                  00086900
087000     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00087000
087100     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00087100
087200                                                                  00087200
087300     COMPUTE VSAM-RECORD-LENGTH        =                          00087300
087400                              4        +                          00087400
087500         GC-GCTABULR-IRPV-FIXED-LEN    +                          00087500
087600         VSAM-ENTRY-COUNT              *                          00087600
087700         GC-GCTABULR-IRPV-VARY-LEN.                               00087700
087800                                                                  00087800
087900     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00087900
088000                                                                  00088000
088100 4270-EXIT.                                                       00088100
088200     EXIT.                                                        00088200
088300/                                                                 00088300
088400******************************************************************00088400
088500****           ALL LEVEL ACCUM   '#ABM'   TABULAR                 00088500
088600******************************************************************00088600
088700 5000-PROCESS-ABM-TABULAR.                                        00088700
088800                                                                  00088800
088900     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00088900
089000     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00089000
089100                                                                  00089100
089200     COMPUTE VSAM-RECORD-LENGTH        =                          00089200
089300                              4        +                          00089300
089400         GC-GCTABULR-ABM-FIXED-LEN     +                          00089400
089500         VSAM-ENTRY-COUNT              *                          00089500
089600         GC-GCTABULR-ABM-VARY-LEN.                                00089600
089700                                                                  00089700
089800     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00089800
089900                                                                  00089900
090000 5000-EXIT.                                                       00090000
090100     EXIT.                                                        00090100
090200/                                                                 00090200
090300******************************************************************00090300
090400****           ALL LEVEL ACCUM   '#ACL'   TABULAR                 00090400
090500******************************************************************00090500
090600 5100-PROCESS-ACL-TABULAR.                                        00090600
090700                                                                  00090700
090800     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00090800
090900     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00090900
091000                                                                  00091000
091100     COMPUTE VSAM-RECORD-LENGTH        =                          00091100
091200                              4        +                          00091200
091300         GC-GCTABULR-ACL-FIXED-LEN     +                          00091300
091400         VSAM-ENTRY-COUNT              *                          00091400
091500         GC-GCTABULR-ACL-VARY-LEN.                                00091500
091600                                                                  00091600
091700     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00091700
091800                                                                  00091800
091900 5100-EXIT.                                                       00091900
092000     EXIT.                                                        00092000
092100/                                                                 00092100
092200******************************************************************00092200
092300****           ALL LEVEL ACCUM   '#ADL'   TABULAR                 00092300
092400******************************************************************00092400
092500 5200-PROCESS-ADL-TABULAR.                                        00092500
092600                                                                  00092600
092700     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00092700
092800     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00092800
092900                                                                  00092900
093000     COMPUTE VSAM-RECORD-LENGTH        =                          00093000
093100                              4        +                          00093100
093200         GC-GCTABULR-ADL-FIXED-LEN     +                          00093200
093300         VSAM-ENTRY-COUNT              *                          00093300
093400         GC-GCTABULR-ADL-VARY-LEN.                                00093400
093500                                                                  00093500
093600     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00093600
093700                                                                  00093700
093800 5200-EXIT.                                                       00093800
093900     EXIT.                                                        00093900
094000/                                                                 00094000
094100******************************************************************00094100
094200****           ALL LEVEL ACCUM   '#AOL'   TABULAR                 00094200
094300******************************************************************00094300
094400 5300-PROCESS-AOL-TABULAR.                                        00094400
094500                                                                  00094500
094600     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00094600
094700     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00094700
094800                                                                  00094800
094900     COMPUTE VSAM-RECORD-LENGTH        =                          00094900
095000                              4        +                          00095000
095100         GC-GCTABULR-AOL-FIXED-LEN     +                          00095100
095200         VSAM-ENTRY-COUNT              *                          00095200
095300         GC-GCTABULR-AOL-VARY-LEN.                                00095300
095400                                                                  00095400
095500     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00095500
095600                                                                  00095600
095700 5300-EXIT.                                                       00095700
095800     EXIT.                                                        00095800
095900/                                                                 00095900
096000******************************************************************00096000
096100****           ALL LEVEL ACCUM   '#ACP'   TABULAR                 00096100
096200******************************************************************00096200
096300 5400-PROCESS-ACP-TABULAR.                                        00096300
096400                                                                  00096400
096500     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00096500
096600     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00096600
096700                                                                  00096700
096800     COMPUTE VSAM-RECORD-LENGTH        =                          00096800
096900                              4        +                          00096900
097000         GC-GCTABULR-ACP-FIXED-LEN     +                          00097000
097100         VSAM-ENTRY-COUNT              *                          00097100
097200         GC-GCTABULR-ACP-VARY-LEN.                                00097200
097300                                                                  00097300
097400     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00097400
097500                                                                  00097500
097600 5400-EXIT.                                                       00097600
097700     EXIT.                                                        00097700
097800/                                                                 00097800
097900******************************************************************00097900
098000****           ALL LEVEL  '#AAR'   TABULAR                        00098000
098100******************************************************************00098100
098200 6000-PROCESS-AAR-TABULAR.                                        00098200
098300                                                                  00098300
098400     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00098400
098500     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00098500
098600                                                                  00098600
098700     COMPUTE VSAM-RECORD-LENGTH        =                          00098700
098800                              4        +                          00098800
098900         GC-GCTABULR-AAR-FIXED-LEN     +                          00098900
099000         VSAM-ENTRY-COUNT              *                          00099000
099100         GC-GCTABULR-AAR-VARY-LEN.                                00099100
099200                                                                  00099200
099300     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00099300
099400                                                                  00099400
099500 6000-EXIT.                                                       00099500
099600     EXIT.                                                        00099600
099700/                                                                 00099700
099800******************************************************************00099800
099900****             ALL LEVEL  '#ACON'   TABULAR                     00099900
100000******************************************************************00100000
100100 6100-PROCESS-ACON-TABULAR.                                       00100100
100200                                                                  00100200
100300     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00100300
100400     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00100400
100500                                                                  00100500
100600     COMPUTE VSAM-RECORD-LENGTH         =                         00100600
100700                              4         +                         00100700
100800         GC-GCTABULR-ACON-FIXED-LEN     +                         00100800
100900         VSAM-ENTRY-COUNT               *                         00100900
101000         GC-GCTABULR-ACON-VARY-LEN.                               00101000
101100                                                                  00101100
101200     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00101200
101300                                                                  00101300
101400 6100-EXIT.                                                       00101400
101500     EXIT.                                                        00101500
101600/                                                                 00101600
101700******************************************************************00101700
101800****             ALL LEVEL  '#ACOS'   TABULAR                     00101800
101900******************************************************************00101900
102000 6200-PROCESS-ACOS-TABULAR.                                       00102000
102100                                                                  00102100
102200     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00102200
102300     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00102300
102400                                                                  00102400
102500     COMPUTE VSAM-RECORD-LENGTH         =                         00102500
102600                              4         +                         00102600
102700         GC-GCTABULR-ACOS-FIXED-LEN     +                         00102700
102800         VSAM-ENTRY-COUNT               *                         00102800
102900         GC-GCTABULR-ACOS-VARY-LEN.                               00102900
103000                                                                  00103000
103100     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00103100
103200                                                                  00103200
103300 6200-EXIT.                                                       00103300
103400     EXIT.                                                        00103400
103500/                                                                 00103500
103600******************************************************************00103600
103700****             ALL LEVEL  '#ADIP'   TABULAR                     00103700
103800******************************************************************00103800
103900 6300-PROCESS-ADIP-TABULAR.                                       00103900
104000                                                                  00104000
104100     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00104100
104200     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00104200
104300                                                                  00104300
104400     COMPUTE VSAM-RECORD-LENGTH         =                         00104400
104500                              4         +                         00104500
104600         GC-GCTABULR-ADIP-FIXED-LEN     +                         00104600
104700         VSAM-ENTRY-COUNT               *                         00104700
104800         GC-GCTABULR-ADIP-VARY-LEN.                               00104800
104900                                                                  00104900
105000     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00105000
105100                                                                  00105100
105200 6300-EXIT.                                                       00105200
105300     EXIT.                                                        00105300
105400/                                                                 00105400
105500******************************************************************00105500
105600****             ALL LEVEL  '#ADOP'   TABULAR                     00105600
105700******************************************************************00105700
105800 6400-PROCESS-ADOP-TABULAR.                                       00105800
105900                                                                  00105900
106000     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00106000
106100     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00106100
106200                                                                  00106200
106300     COMPUTE VSAM-RECORD-LENGTH         =                         00106300
106400                              4         +                         00106400
106500         GC-GCTABULR-ADOP-FIXED-LEN     +                         00106500
106600         VSAM-ENTRY-COUNT               *                         00106600
106700         GC-GCTABULR-ADOP-VARY-LEN.                               00106700
106800                                                                  00106800
106900     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00106900
107000                                                                  00107000
107100 6400-EXIT.                                                       00107100
107200     EXIT.                                                        00107200
107300/                                                                 00107300
107400******************************************************************00107400
107500****             CONTRACT  '#CLDR'   TABULAR                      00107500
107600******************************************************************00107600
107700 7000-PROCESS-CLDR-TABULAR.                                       00107700
107800                                                                  00107800
107900     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00107900
108000     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00108000
108100                                                                  00108100
108200     COMPUTE VSAM-RECORD-LENGTH        =                          00108200
108300                              4        +                          00108300
108400         GC-GCTABULR-CLDR-FIXED-LEN    +                          00108400
108500         VSAM-ENTRY-COUNT              *                          00108500
108600         GC-GCTABULR-CLDR-VARY-LEN.                               00108600
108700                                                                  00108700
108800     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00108800
108900                                                                  00108900
109000 7000-EXIT.                                                       00109000
109100     EXIT.                                                        00109100
109200/                                                                 00109200
109300******************************************************************00109300
109400****             CONTRACT  '#CRR'   TABULAR                       00109400
109500******************************************************************00109500
109600 7100-PROCESS-CRR-TABULAR.                                        00109600
109700                                                                  00109700
109800     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00109800
109900     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00109900
110000                                                                  00110000
110100     COMPUTE VSAM-RECORD-LENGTH        =                          00110100
110200                              4        +                          00110200
110300         GC-GCTABULR-CRR-FIXED-LEN     +                          00110300
110400         VSAM-ENTRY-COUNT              *                          00110400
110500         GC-GCTABULR-CRR-VARY-LEN.                                00110500
110600                                                                  00110600
110700     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00110700
110800                                                                  00110800
110900 7100-EXIT.                                                       00110900
111000     EXIT.                                                        00111000
111100/                                                                 00111100
111200******************************************************************00111200
111300****             CONTRACT  '#CRS'   TABULAR                       00111300
111400******************************************************************00111400
111500 7200-PROCESS-CRS-TABULAR.                                        00111500
111600                                                                  00111600
111700     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00111700
111800     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00111800
111900                                                                  00111900
112000     COMPUTE VSAM-RECORD-LENGTH        =                          00112000
112100                              4        +                          00112100
112200         GC-GCTABULR-CRS-FIXED-LEN     +                          00112200
112300         VSAM-ENTRY-COUNT              *                          00112300
112400         GC-GCTABULR-CRS-VARY-LEN.                                00112400
112500                                                                  00112500
112600     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00112600
112700                                                                  00112700
112800 7200-EXIT.                                                       00112800
112900     EXIT.                                                        00112900
113000/                                                                 00113000
113100******************************************************************00113100
113200****             CONTRACT  '#CDRS'   TABULAR                      00113200
113300******************************************************************00113300
113400 7250-PROCESS-CDRS-TABULAR.                                       00113400
113500                                                                  00113500
113600     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00113600
113700     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00113700
113800                                                                  00113800
113900     COMPUTE VSAM-RECORD-LENGTH        =                          00113900
114000                              4        +                          00114000
114100         GC-GCTABULR-CDRS-FIXED-LEN    +                          00114100
114200         VSAM-ENTRY-COUNT              *                          00114200
114300         GC-GCTABULR-CDRS-VARY-LEN.                               00114300
114400                                                                  00114400
114500     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00114500
114600                                                                  00114600
114700 7250-EXIT.                                                       00114700
114800     EXIT.                                                        00114800
114900/                                                                 00114900
115000******************************************************************00115000
115100****           GROUP SPECIFIC   '#GCBL'   TABULAR                 00115100
115200******************************************************************00115200
115300 8100-PROCESS-GCBL-TABULAR.                                       00115300
115400                                                                  00115400
115500     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00115500
115600     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00115600
115700                                                                  00115700
115800     COMPUTE VSAM-RECORD-LENGTH         =                         00115800
115900                              4         +                         00115900
116000         GC-GCTABULR-GCBL-FIXED-LEN     +                         00116000
116100         VSAM-ENTRY-COUNT               *                         00116100
116200         GC-GCTABULR-GCBL-VARY-LEN.                               00116200
116300                                                                  00116300
116400     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00116400
116500                                                                  00116500
116600 8100-EXIT.                                                       00116600
116700     EXIT.                                                        00116700
116800/                                                                 00116800
116900******************************************************************00116900
117000****           GROUP SPECIFIC   '#GCCP'   TABULAR                 00117000
117100******************************************************************00117100
117200 8105-PROCESS-GCCP-TABULAR.                                       00117200
117300                                                                  00117300
117400     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00117400
117500     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00117500
117600                                                                  00117600
117700     COMPUTE VSAM-RECORD-LENGTH         =                         00117700
117800                              4         +                         00117800
117900         GC-GCTABULR-GCCP-FIXED-LEN     +                         00117900
118000         VSAM-ENTRY-COUNT               *                         00118000
118100         GC-GCTABULR-GCCP-VARY-LEN.                               00118100
118200                                                                  00118200
118300     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00118300
118400                                                                  00118400
118500 8105-EXIT.                                                       00118500
118600     EXIT.                                                        00118600
118700/                                                                 00118700
118800******************************************************************00118800
118900****           GROUP SPECIFIC   '#GCPO'   TABULAR                 00118900
119000******************************************************************00119000
119100 8110-PROCESS-GCPO-TABULAR.                                       00119100
119200                                                                  00119200
119300     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00119300
119400     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00119400
119500                                                                  00119500
119600     COMPUTE VSAM-RECORD-LENGTH         =                         00119600
119700                              4         +                         00119700
119800         GC-GCTABULR-GCPO-FIXED-LEN     +                         00119800
119900         VSAM-ENTRY-COUNT               *                         00119900
120000         GC-GCTABULR-GCPO-VARY-LEN.                               00120000
120100                                                                  00120100
120200     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00120200
120300                                                                  00120300
120400 8110-EXIT.                                                       00120400
120500     EXIT.                                                        00120500
120600/                                                                 00120600
120700******************************************************************00120700
120800****          GROUP SPECIFIC   '#GFSB'   TABULAR                  00120800
120900******************************************************************00120900
121000 8120-PROCESS-GFSB-TABULAR.                                       00121000
121100                                                                  00121100
121200     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00121200
121300     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00121300
121400                                                                  00121400
121500     COMPUTE VSAM-RECORD-LENGTH         =                         00121500
121600                              4         +                         00121600
121700         GC-GCTABULR-GFSB-FIXED-LEN     +                         00121700
121800         VSAM-ENTRY-COUNT               *                         00121800
121900         GC-GCTABULR-GFSB-VARY-LEN.                               00121900
122000                                                                  00122000
122100     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00122100
122200                                                                  00122200
122300 8120-EXIT.                                                       00122300
122400     EXIT.                                                        00122400
122500/                                                                 00122500
122600******************************************************************00122600
122700****          GROUP SPECIFIC   '#GHOB'   TABULAR                  00122700
122800******************************************************************00122800
122900 8140-PROCESS-GHOB-TABULAR.                                       00122900
123000                                                                  00123000
123100     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00123100
123200     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00123200
123300                                                                  00123300
123400     COMPUTE VSAM-RECORD-LENGTH         =                         00123400
123500                              4         +                         00123500
123600         GC-GCTABULR-GHOB-FIXED-LEN     +                         00123600
123700         VSAM-ENTRY-COUNT               *                         00123700
123800         GC-GCTABULR-GHOB-VARY-LEN.                               00123800
123900                                                                  00123900
124000     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00124000
124100                                                                  00124100
124200 8140-EXIT.                                                       00124200
124300     EXIT.                                                        00124300
124400/                                                                 00124400
124500******************************************************************00124500
124600****          GROUP SPECIFIC   '#GHOR'   TABULAR                  00124600
124700******************************************************************00124700
124800 8160-PROCESS-GHOR-TABULAR.                                       00124800
124900                                                                  00124900
125000     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00125000
125100     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00125100
125200                                                                  00125200
125300     COMPUTE VSAM-RECORD-LENGTH         =                         00125300
125400                              4         +                         00125400
125500         GC-GCTABULR-GHOR-FIXED-LEN     +                         00125500
125600         VSAM-ENTRY-COUNT               *                         00125600
125700         GC-GCTABULR-GHOR-VARY-LEN.                               00125700
125800                                                                  00125800
125900     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00125900
126000                                                                  00126000
126100 8160-EXIT.                                                       00126100
126200     EXIT.                                                        00126200
126300                                                                  00126300
126400/*****************************************************************00126400
126500****          GROUP SPECIFIC   '#GMCD'   TABULAR                  00126500
126600******************************************************************00126600
126700 8170-PROCESS-GMCD-TABULAR.                                       00126700
126800                                                                  00126800
126900     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00126900
127000     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00127000
127100                                                                  00127100
127200     COMPUTE VSAM-RECORD-LENGTH         =                         00127200
127300                              4         +                         00127300
127400         GC-GCTABULR-GMCD-FIXED-LEN     +                         00127400
127500         VSAM-ENTRY-COUNT               *                         00127500
127600         GC-GCTABULR-GMCD-VARY-LEN.                               00127600
127700                                                                  00127700
127800     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00127800
127900                                                                  00127900
128000 8170-EXIT.                                                       00128000
128100     EXIT.                                                        00128100
128200                                                                  00128200
128300/*****************************************************************00128300
128400****          GROUP SPECIFIC   '#GMCG'   TABULAR                  00128400
128500******************************************************************00128500
128600 8171-PROCESS-GMCG-TABULAR.                                       00128600
128700                                                                  00128700
128800     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00128800
128900     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00128900
129000                                                                  00129000
129100     COMPUTE VSAM-RECORD-LENGTH         =                         00129100
129200                              4         +                         00129200
129300         GC-GCTABULR-GMCG-FIXED-LEN     +                         00129300
129400         VSAM-ENTRY-COUNT               *                         00129400
129500         GC-GCTABULR-GMCG-VARY-LEN.                               00129500
129600                                                                  00129600
129700     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00129700
129800                                                                  00129800
129900 8171-EXIT.                                                       00129900
130000     EXIT.                                                        00130000
130100                                                                  00130100
130200/*****************************************************************00130200
130300****          GROUP SPECIFIC   '#GMCR'   TABULAR                  00130300
130400******************************************************************00130400
130500 8172-PROCESS-GMCR-TABULAR.                                       00130500
130600                                                                  00130600
130700     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00130700
130800     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00130800
130900                                                                  00130900
131000     COMPUTE VSAM-RECORD-LENGTH         =                         00131000
131100                              4         +                         00131100
131200         GC-GCTABULR-GMCR-FIXED-LEN     +                         00131200
131300         VSAM-ENTRY-COUNT               *                         00131300
131400         GC-GCTABULR-GMCR-VARY-LEN.                               00131400
131500                                                                  00131500
131600     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00131600
131700                                                                  00131700
131800 8172-EXIT.                                                       00131800
131900     EXIT.                                                        00131900
132000                                                                  00132000
132100/*****************************************************************00132100
132200****          GROUP SPECIFIC   '#GMCT'   TABULAR                  00132200
132300******************************************************************00132300
132400 8173-PROCESS-GMCT-TABULAR.                                       00132400
132500                                                                  00132500
132600     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00132600
132700     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00132700
132800                                                                  00132800
132900     COMPUTE VSAM-RECORD-LENGTH         =                         00132900
133000                              4         +                         00133000
133100         GC-GCTABULR-GMCT-FIXED-LEN     +                         00133100
133200         VSAM-ENTRY-COUNT               *                         00133200
133300         GC-GCTABULR-GMCT-VARY-LEN.                               00133300
133400                                                                  00133400
133500     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00133500
133600                                                                  00133600
133700 8173-EXIT.                                                       00133700
133800     EXIT.                                                        00133800
133900                                                                  00133900
134000******************************************************************00134000
134100****          GROUP SPECIFIC   '#GMCS'   TABULAR                  00134100
134200******************************************************************00134200
134300 8174-PROCESS-GMCS-TABULAR.                                       00134300
134400                                                                  00134400
134500     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00134500
134600     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00134600
134700                                                                  00134700
134800     COMPUTE VSAM-RECORD-LENGTH         =                         00134800
134900                              4         +                         00134900
135000         GC-GCTABULR-GMCS-FIXED-LEN     +                         00135000
135100         VSAM-ENTRY-COUNT               *                         00135100
135200         GC-GCTABULR-GMCS-VARY-LEN.                               00135200
135300                                                                  00135300
135400     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00135400
135500                                                                  00135500
135600 8174-EXIT.                                                       00135600
135700     EXIT.                                                        00135700
135800/                                                                 00135800
135900/*****************************************************************00135900
136000****           GROUP SPECIFIC   '#GMDB'   TABULAR                 00136000
136100******************************************************************00136100
136200 8180-PROCESS-GMDB-TABULAR.                                       00136200
136300                                                                  00136300
136400     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00136400
136500     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00136500
136600                                                                  00136600
136700     COMPUTE VSAM-RECORD-LENGTH         =                         00136700
136800                              4         +                         00136800
136900         GC-GCTABULR-GMDB-FIXED-LEN     +                         00136900
137000         VSAM-ENTRY-COUNT               *                         00137000
137100         GC-GCTABULR-GMDB-VARY-LEN.                               00137100
137200                                                                  00137200
137300     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00137300
137400                                                                  00137400
137500 8180-EXIT.                                                       00137500
137600     EXIT.                                                        00137600
137700/                                                                 00137700
137800******************************************************************00137800
137900****          GROUP SPECIFIC   '#GMDN'   TABULAR                  00137900
138000******************************************************************00138000
138100 8200-PROCESS-GMDN-TABULAR.                                       00138100
138200                                                                  00138200
138300     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00138300
138400     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00138400
138500                                                                  00138500
138600     COMPUTE VSAM-RECORD-LENGTH         =                         00138600
138700                              4         +                         00138700
138800         GC-GCTABULR-GMDN-FIXED-LEN     +                         00138800
138900         VSAM-ENTRY-COUNT               *                         00138900
139000         GC-GCTABULR-GMDN-VARY-LEN.                               00139000
139100                                                                  00139100
139200     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00139200
139300                                                                  00139300
139400 8200-EXIT.                                                       00139400
139500     EXIT.                                                        00139500
139600/                                                                 00139600
139700******************************************************************00139700
139800****          GROUP SPECIFIC   '#GMOB'   TABULAR                  00139800
139900******************************************************************00139900
140000 8220-PROCESS-GMOB-TABULAR.                                       00140000
140100                                                                  00140100
140200     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00140200
140300     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00140300
140400                                                                  00140400
140500     COMPUTE VSAM-RECORD-LENGTH         =                         00140500
140600                              4         +                         00140600
140700         GC-GCTABULR-GMOB-FIXED-LEN     +                         00140700
140800         VSAM-ENTRY-COUNT               *                         00140800
140900         GC-GCTABULR-GMOB-VARY-LEN.                               00140900
141000                                                                  00141000
141100     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00141100
141200                                                                  00141200
141300 8220-EXIT.                                                       00141300
141400     EXIT.                                                        00141400
141500/                                                                 00141500
141600******************************************************************00141600
141700****          GROUP SPECIFIC   '#GMOR'   TABULAR                  00141700
141800******************************************************************00141800
141900 8240-PROCESS-GMOR-TABULAR.                                       00141900
142000                                                                  00142000
142100     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00142100
142200     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00142200
142300                                                                  00142300
142400     COMPUTE VSAM-RECORD-LENGTH         =                         00142400
142500                              4         +                         00142500
142600         GC-GCTABULR-GMOR-FIXED-LEN     +                         00142600
142700         VSAM-ENTRY-COUNT               *                         00142700
142800         GC-GCTABULR-GMOR-VARY-LEN.                               00142800
142900                                                                  00142900
143000     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00143000
143100                                                                  00143100
143200 8240-EXIT.                                                       00143200
143300     EXIT.                                                        00143300
143400/                                                                 00143400
143500******************************************************************00143500
143600****         GROUP SPECIFIC   '#GMPB'   TABULAR                   00143600
143700******************************************************************00143700
143800 8260-PROCESS-GMPB-TABULAR.                                       00143800
143900                                                                  00143900
144000     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00144000
144100     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00144100
144200                                                                  00144200
144300     COMPUTE VSAM-RECORD-LENGTH         =                         00144300
144400                              4         +                         00144400
144500         GC-GCTABULR-GMPB-FIXED-LEN     +                         00144500
144600         VSAM-ENTRY-COUNT               *                         00144600
144700         GC-GCTABULR-GMPB-VARY-LEN.                               00144700
144800                                                                  00144800
144900     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00144900
145000                                                                  00145000
145100 8260-EXIT.                                                       00145100
145200     EXIT.                                                        00145200
145300/                                                                 00145300
145400******************************************************************00145400
145500****         GROUP SPECIFIC   '#GMPR'   TABULAR                   00145500
145600******************************************************************00145600
145700 8280-PROCESS-GMPR-TABULAR.                                       00145700
145800                                                                  00145800
145900     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00145900
146000     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00146000
146100                                                                  00146100
146200     COMPUTE VSAM-RECORD-LENGTH         =                         00146200
146300                              4         +                         00146300
146400         GC-GCTABULR-GMPR-FIXED-LEN     +                         00146400
146500         VSAM-ENTRY-COUNT               *                         00146500
146600         GC-GCTABULR-GMPR-VARY-LEN.                               00146600
146700                                                                  00146700
146800     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00146800
146900                                                                  00146900
147000 8280-EXIT.                                                       00147000
147100     EXIT.                                                        00147100
147200/                                                                 00147200
147300******************************************************************00147300
147400****           GROUP SPECIFIC   '#GMSB'   TABULAR                 00147400
147500******************************************************************00147500
147600 8300-PROCESS-GMSB-TABULAR.                                       00147600
147700                                                                  00147700
147800     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00147800
147900     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00147900
148000                                                                  00148000
148100     COMPUTE VSAM-RECORD-LENGTH         =                         00148100
148200                              4         +                         00148200
148300         GC-GCTABULR-GMSB-FIXED-LEN     +                         00148300
148400         VSAM-ENTRY-COUNT               *                         00148400
148500         GC-GCTABULR-GMSB-VARY-LEN.                               00148500
148600                                                                  00148600
148700     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00148700
148800                                                                  00148800
148900 8300-EXIT.                                                       00148900
149000     EXIT.                                                        00149000
149100/                                                                 00149100
149200******************************************************************00149200
149300****           GROUP SPECIFIC   '#GMSC'   TABULAR                 00149300
149400******************************************************************00149400
149500 8320-PROCESS-GMSC-TABULAR.                                       00149500
149600                                                                  00149600
149700     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00149700
149800     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00149800
149900                                                                  00149900
150000     COMPUTE VSAM-RECORD-LENGTH         =                         00150000
150100                              4         +                         00150100
150200         GC-GCTABULR-GMSC-FIXED-LEN     +                         00150200
150300         VSAM-ENTRY-COUNT               *                         00150300
150400         GC-GCTABULR-GMSC-VARY-LEN.                               00150400
150500                                                                  00150500
150600     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00150600
150700                                                                  00150700
150800 8320-EXIT.                                                       00150800
150900     EXIT.                                                        00150900
151000/                                                                 00151000
151100******************************************************************00151100
151200****           GROUP SPECIFIC   '#GMSR'   TABULAR                 00151200
151300******************************************************************00151300
151400 8340-PROCESS-GMSR-TABULAR.                                       00151400
151500                                                                  00151500
151600     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00151600
151700     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00151700
151800                                                                  00151800
151900     COMPUTE VSAM-RECORD-LENGTH         =                         00151900
152000                              4         +                         00152000
152100         GC-GCTABULR-GMSR-FIXED-LEN     +                         00152100
152200         VSAM-ENTRY-COUNT               *                         00152200
152300         GC-GCTABULR-GMSR-VARY-LEN.                               00152300
152400                                                                  00152400
152500     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00152500
152600                                                                  00152600
152700 8340-EXIT.                                                       00152700
152800     EXIT.                                                        00152800
152900/                                                                 00152900
153000******************************************************************00153000
153100****           GROUP SPECIFIC   '#GPAB'   TABULAR                 00153100
153200******************************************************************00153200
153300 8360-PROCESS-GPAB-TABULAR.                                       00153300
153400                                                                  00153400
153500     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00153500
153600     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00153600
153700                                                                  00153700
153800     COMPUTE VSAM-RECORD-LENGTH         =                         00153800
153900                              4         +                         00153900
154000         GC-GCTABULR-GPAB-FIXED-LEN     +                         00154000
154100         VSAM-ENTRY-COUNT               *                         00154100
154200         GC-GCTABULR-GPAB-VARY-LEN.                               00154200
154300                                                                  00154300
154400     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00154400
154500                                                                  00154500
154600 8360-EXIT.                                                       00154600
154700     EXIT.                                                        00154700
154800/                                                                 00154800
154900******************************************************************00154900
155000****           GROUP SPECIFIC   '#GPAC'   TABULAR                 00155000
155100******************************************************************00155100
155200 8380-PROCESS-GPAC-TABULAR.                                       00155200
155300                                                                  00155300
155400     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00155400
155500     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00155500
155600                                                                  00155600
155700     COMPUTE VSAM-RECORD-LENGTH         =                         00155700
155800                              4         +                         00155800
155900         GC-GCTABULR-GPAC-FIXED-LEN     +                         00155900
156000         VSAM-ENTRY-COUNT               *                         00156000
156100         GC-GCTABULR-GPAC-VARY-LEN.                               00156100
156200                                                                  00156200
156300     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00156300
156400                                                                  00156400
156500 8380-EXIT.                                                       00156500
156600     EXIT.                                                        00156600
156700/                                                                 00156700
156800******************************************************************00156800
156900****           GROUP SPECIFIC   '#GPAD'   TABULAR                 00156900
157000******************************************************************00157000
157100 8400-PROCESS-GPAD-TABULAR.                                       00157100
157200                                                                  00157200
157300     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00157300
157400     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00157400
157500                                                                  00157500
157600     COMPUTE VSAM-RECORD-LENGTH         =                         00157600
157700                              4         +                         00157700
157800         GC-GCTABULR-GPAD-FIXED-LEN     +                         00157800
157900         VSAM-ENTRY-COUNT               *                         00157900
158000         GC-GCTABULR-GPAD-VARY-LEN.                               00158000
158100                                                                  00158100
158200     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00158200
158300                                                                  00158300
158400 8400-EXIT.                                                       00158400
158500     EXIT.                                                        00158500
158600/                                                                 00158600
158700******************************************************************00158700
158800****           GROUP SPECIFIC   '#GPAN'   TABULAR                 00158800
158900******************************************************************00158900
159000 8410-PROCESS-GPAN-TABULAR.                                       00159000
159100                                                                  00159100
159200     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00159200
159300     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00159300
159400                                                                  00159400
159500     COMPUTE VSAM-RECORD-LENGTH         =                         00159500
159600                              4         +                         00159600
159700         GC-GCTABULR-GPAN-FIXED-LEN     +                         00159700
159800         VSAM-ENTRY-COUNT               *                         00159800
159900         GC-GCTABULR-GPAN-VARY-LEN.                               00159900
160000                                                                  00160000
160100     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00160100
160200                                                                  00160200
160300 8410-EXIT.                                                       00160300
160400     EXIT.                                                        00160400
160500/                                                                 00160500
160600******************************************************************00160600
160700****           GROUP SPECIFIC   '#GPAR'   TABULAR                 00160700
160800******************************************************************00160800
160900 8420-PROCESS-GPAR-TABULAR.                                       00160900
161000                                                                  00161000
161100     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00161100
161200     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00161200
161300                                                                  00161300
161400     COMPUTE VSAM-RECORD-LENGTH         =                         00161400
161500                              4         +                         00161500
161600         GC-GCTABULR-GPAR-FIXED-LEN     +                         00161600
161700         VSAM-ENTRY-COUNT               *                         00161700
161800         GC-GCTABULR-GPAR-VARY-LEN.                               00161800
161900                                                                  00161900
162000     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00162000
162100                                                                  00162100
162200 8420-EXIT.                                                       00162200
162300     EXIT.                                                        00162300
162400/                                                                 00162400
162500******************************************************************00162500
162600****           GROUP SPECIFIC   '#GPPO'   TABULAR                 00162600
162700******************************************************************00162700
162800 8440-PROCESS-GPPO-TABULAR.                                       00162800
162900                                                                  00162900
163000     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00163000
163100     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00163100
163200                                                                  00163200
163300     COMPUTE VSAM-RECORD-LENGTH         =                         00163300
163400                              4         +                         00163400
163500         GC-GCTABULR-GPPO-FIXED-LEN     +                         00163500
163600         VSAM-ENTRY-COUNT               *                         00163600
163700         GC-GCTABULR-GPPO-VARY-LEN.                               00163700
163800                                                                  00163800
163900     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00163900
164000                                                                  00164000
164100 8440-EXIT.                                                       00164100
164200     EXIT.                                                        00164200
164300/                                                                 00164300
164400******************************************************************00164400
164500****           GROUP SPECIFIC   '#GRID'   TABULAR                 00164500
164600******************************************************************00164600
164700 8460-PROCESS-GRID-TABULAR.                                       00164700
164800                                                                  00164800
164900     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00164900
165000     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00165000
165100                                                                  00165100
165200     COMPUTE VSAM-RECORD-LENGTH         =                         00165200
165300                              4         +                         00165300
165400         GC-GCTABULR-GRID-FIXED-LEN     +                         00165400
165500         VSAM-ENTRY-COUNT               *                         00165500
165600         GC-GCTABULR-GRID-VARY-LEN.                               00165600
165700                                                                  00165700
165800     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00165800
165900                                                                  00165900
166000 8460-EXIT.                                                       00166000
166100     EXIT.                                                        00166100
166200/                                                                 00166200
166300******************************************************************00166300
166400****           GROUP SPECIFIC   '#GRPO'   TABULAR                 00166400
166500******************************************************************00166500
166600 8470-PROCESS-GRPO-TABULAR.                                       00166600
166700                                                                  00166700
166800     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00166800
166900     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00166900
167000                                                                  00167000
167100     COMPUTE VSAM-RECORD-LENGTH         =                         00167100
167200                              4         +                         00167200
167300         GC-GCTABULR-GRPO-FIXED-LEN     +                         00167300
167400         VSAM-ENTRY-COUNT               *                         00167400
167500         GC-GCTABULR-GRPO-VARY-LEN.                               00167500
167600                                                                  00167600
167700     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00167700
167800                                                                  00167800
167900 8470-EXIT.                                                       00167900
168000     EXIT.                                                        00168000
168100/                                                                 00168100
168200******************************************************************00168200
168300****           GROUP SPECIFIC   '#GBAE'   TABULAR                 00168300
168400******************************************************************00168400
168500 8471-PROCESS-GBAE-TABULAR.                                       00168500
168600                                                                  00168600
168700     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00168700
168800     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00168800
168900                                                                  00168900
169000     COMPUTE VSAM-RECORD-LENGTH         =                         00169000
169100                              4         +                         00169100
169200         GC-GCTABULR-GBAE-FIXED-LEN     +                         00169200
169300         VSAM-ENTRY-COUNT               *                         00169300
169400         GC-GCTABULR-GBAE-VARY-LEN.                               00169400
169500                                                                  00169500
169600     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00169600
169700                                                                  00169700
169800 8471-EXIT.                                                       00169800
169900     EXIT.                                                        00169900
170000/                                                                 00170000
170100******************************************************************00170100
170200****           GROUP SPECIFIC   '#GSUB'   TABULAR                 00170200
170300******************************************************************00170300
170400 8475-PROCESS-GSUB-TABULAR.                                       00170400
170500                                                                  00170500
170600     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00170600
170700     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00170700
170800                                                                  00170800
170900     COMPUTE VSAM-RECORD-LENGTH         =                         00170900
171000                              4         +                         00171000
171100         GC-GCTABULR-GSUB-FIXED-LEN.                              00171100
171200                                                                  00171200
171300     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00171300
171400                                                                  00171400
171500 8475-EXIT.                                                       00171500
171600     EXIT.                                                        00171600
171700/                                                                 00171700
171800******************************************************************00171800
171900****           GROUP SPECIFIC   '#GVLF'   TABULAR                 00171900
172000******************************************************************00172000
172100 8480-PROCESS-GVLF-TABULAR.                                       00172100
172200                                                                  00172200
172300     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00172300
172400     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00172400
172500                                                                  00172500
172600     COMPUTE VSAM-RECORD-LENGTH         =                         00172600
172700                              4         +                         00172700
172800         GC-GCTABULR-GVLF-FIXED-LEN     +                         00172800
172900         VSAM-ENTRY-COUNT               *                         00172900
173000         GC-GCTABULR-GVLF-VARY-LEN.                               00173000
173100                                                                  00173100
173200     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00173200
173300                                                                  00173300
173400 8480-EXIT.                                                       00173400
173500     EXIT.                                                        00173500
173600/*****************************************************************00173600
173700****           GROUP SPECIFIC   '#GVLG'   TABULAR                 00173700
173800******************************************************************00173800
173900 8481-PROCESS-GVLG-TABULAR.                                       00173900
174000                                                                  00174000
174100     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00174100
174200     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00174200
174300                                                                  00174300
174400     COMPUTE VSAM-RECORD-LENGTH         =                         00174400
174500                              4         +                         00174500
174600         GC-GCTABULR-GVLG-FIXED-LEN     +                         00174600
174700         VSAM-ENTRY-COUNT               *                         00174700
174800         GC-GCTABULR-GVLG-VARY-LEN.                               00174800
174900                                                                  00174900
175000     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00175000
175100                                                                  00175100
175200 8481-EXIT.                                                       00175200
175300     EXIT.                                                        00175300
175400/*****************************************************************00175400
175500****           GROUP SPECIFIC   '#GVLH'   TABULAR                 00175500
175600******************************************************************00175600
175700 8482-PROCESS-GVLH-TABULAR.                                       00175700
175800                                                                  00175800
175900     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00175900
176000     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00176000
176100                                                                  00176100
176200     COMPUTE VSAM-RECORD-LENGTH         =                         00176200
176300                              4         +                         00176300
176400         GC-GCTABULR-GVLH-FIXED-LEN     +                         00176400
176500         VSAM-ENTRY-COUNT               *                         00176500
176600         GC-GCTABULR-GVLH-VARY-LEN.                               00176600
176700                                                                  00176700
176800     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00176800
176900                                                                  00176900
177000 8482-EXIT.                                                       00177000
177100     EXIT.                                                        00177100
177200                                                                  00177200
177300/*****************************************************************00177300
177400****           GROUP SPECIFIC   '#GVLP'   TABULAR                 00177400
177500******************************************************************00177500
177600 8500-PROCESS-GVLP-TABULAR.                                       00177600
177700                                                                  00177700
177800     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00177800
177900     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00177900
178000                                                                  00178000
178100     COMPUTE VSAM-RECORD-LENGTH         =                         00178100
178200                              4         +                         00178200
178300         GC-GCTABULR-GVLP-FIXED-LEN     +                         00178300
178400         VSAM-ENTRY-COUNT               *                         00178400
178500         GC-GCTABULR-GVLP-VARY-LEN.                               00178500
178600                                                                  00178600
178700     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00178700
178800                                                                  00178800
178900 8500-EXIT.                                                       00178900
179000     EXIT.                                                        00179000
179100/*****************************************************************00179100
179200****           GROUP SPECIFIC   '#GVLQ'   TABULAR                 00179200
179300******************************************************************00179300
179400 8501-PROCESS-GVLQ-TABULAR.                                       00179400
179500                                                                  00179500
179600     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00179600
179700     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00179700
179800                                                                  00179800
179900     COMPUTE VSAM-RECORD-LENGTH         =                         00179900
180000                              4         +                         00180000
180100         GC-GCTABULR-GVLQ-FIXED-LEN     +                         00180100
180200         VSAM-ENTRY-COUNT               *                         00180200
180300         GC-GCTABULR-GVLQ-VARY-LEN.                               00180300
180400                                                                  00180400
180500     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00180500
180600                                                                  00180600
180700 8501-EXIT.                                                       00180700
180800     EXIT.                                                        00180800
180900/*****************************************************************00180900
181000****           GROUP SPECIFIC   '#GVLR'   TABULAR                 00181000
181100******************************************************************00181100
181200 8502-PROCESS-GVLR-TABULAR.                                       00181200
181300                                                                  00181300
181400     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00181400
181500     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00181500
181600                                                                  00181600
181700     COMPUTE VSAM-RECORD-LENGTH         =                         00181700
181800                              4         +                         00181800
181900         GC-GCTABULR-GVLR-FIXED-LEN     +                         00181900
182000         VSAM-ENTRY-COUNT               *                         00182000
182100         GC-GCTABULR-GVLR-VARY-LEN.                               00182100
182200                                                                  00182200
182300     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00182300
182400                                                                  00182400
182500 8502-EXIT.                                                       00182500
182600     EXIT.                                                        00182600
182700/                                                                 00182700
182800******************************************************************00182800
182900****           GROUP SPECIFIC   '#GWCD'   TABULAR                 00182900
183000******************************************************************00183000
183100 8520-PROCESS-GWCD-TABULAR.                                       00183100
183200                                                                  00183200
183300     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00183300
183400     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00183400
183500                                                                  00183500
183600     COMPUTE VSAM-RECORD-LENGTH         =                         00183600
183700                              4         +                         00183700
183800         GC-GCTABULR-GWCD-FIXED-LEN     +                         00183800
183900         VSAM-ENTRY-COUNT               *                         00183900
184000         GC-GCTABULR-GWCD-VARY-LEN.                               00184000
184100                                                                  00184100
184200     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00184200
184300                                                                  00184300
184400 8520-EXIT.                                                       00184400
184500     EXIT.                                                        00184500
184600/                                                                 00184600
184700******************************************************************00184700
184800****           GROUP SPECIFIC   '#GFHC'   TABULAR                 00184800
184900******************************************************************00184900
185000 8530-PROCESS-GFHC-TABULAR.                                       00185000
185100                                                                  00185100
185200     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00185200
185300     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00185300
185400                                                                  00185400
185500     COMPUTE VSAM-RECORD-LENGTH         =                         00185500
185600                              4         +                         00185600
185700         GC-GCTABULR-GFHC-FIXED-LEN     +                         00185700
185800         VSAM-ENTRY-COUNT               *                         00185800
185900         GC-GCTABULR-GFHC-VARY-LEN.                               00185900
186000                                                                  00186000
186100     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00186100
186200                                                                  00186200
186300 8530-EXIT.                                                       00186300
186400     EXIT.                                                        00186400
186500/                                                                 00186500
186600******************************************************************00186600
186700****           GROUP SPECIFIC   '#GFSA'   TABULAR                 00186700
186800******************************************************************00186800
186900 8531-PROCESS-GFSA-TABULAR.                                       00186900
187000                                                                  00187000
187100     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00187100
187200     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00187200
187300                                                                  00187300
187400     COMPUTE VSAM-RECORD-LENGTH         =                         00187400
187500                              4         +                         00187500
187600         GC-GCTABULR-GFSA-FIXED-LEN     +                         00187600
187700         VSAM-ENTRY-COUNT               *                         00187700
187800         GC-GCTABULR-GFSA-VARY-LEN.                               00187800
187900                                                                  00187900
188000     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00188000
188100                                                                  00188100
188200 8531-EXIT.                                                       00188200
188300     EXIT.                                                        00188300
188400/                                                                 00188400
188500******************************************************************00188500
188600****           GROUP SPECIFIC   '#GHCA'   TABULAR                 00188600
188700******************************************************************00188700
188800 8532-PROCESS-GHCA-TABULAR.                                       00188800
188900                                                                  00188900
189000     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00189000
189100     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00189100
189200                                                                  00189200
189300     COMPUTE VSAM-RECORD-LENGTH         =                         00189300
189400                              4         +                         00189400
189500         GC-GCTABULR-GHCA-FIXED-LEN     +                         00189500
189600         VSAM-ENTRY-COUNT               *                         00189600
189700         GC-GCTABULR-GHCA-VARY-LEN.                               00189700
189800                                                                  00189800
189900     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00189900
190000                                                                  00190000
190100 8532-EXIT.                                                       00190100
190200     EXIT.                                                        00190200
190300/                                                                 00190300
190400******************************************************************00190400
190500****           GROUP SPECIFIC   '#GHSA'   TABULAR                 00190500
190600******************************************************************00190600
190700 8533-PROCESS-GHSA-TABULAR.                                       00190700
190800                                                                  00190800
190900     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00190900
191000     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00191000
191100                                                                  00191100
191200     COMPUTE VSAM-RECORD-LENGTH         =                         00191200
191300                              4         +                         00191300
191400         GC-GCTABULR-GHSA-FIXED-LEN     +                         00191400
191500         VSAM-ENTRY-COUNT               *                         00191500
191600         GC-GCTABULR-GHSA-VARY-LEN.                               00191600
191700                                                                  00191700
191800     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00191800
191900                                                                  00191900
192000 8533-EXIT.                                                       00192000
192100     EXIT.                                                        00192100
192200/                                                                 00192200
192300******************************************************************00192300
192400****           GROUP SPECIFIC   '#GLPF'   TABULAR                 00192400
192500******************************************************************00192500
192600 8534-PROCESS-GLPF-TABULAR.                                       00192600
192700                                                                  00192700
192800     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00192800
192900     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00192900
193000                                                                  00193000
193100     COMPUTE VSAM-RECORD-LENGTH         =                         00193100
193200                              4         +                         00193200
193300         GC-GCTABULR-GLPF-FIXED-LEN     +                         00193300
193400         VSAM-ENTRY-COUNT               *                         00193400
193500         GC-GCTABULR-GLPF-VARY-LEN.                               00193500
193600                                                                  00193600
193700     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00193700
193800                                                                  00193800
193900 8534-EXIT.                                                       00193900
194000     EXIT.                                                        00194000
194100/                                                                 00194100
194200******************************************************************00194200
194300****           GROUP SPECIFIC   '#GLPH'   TABULAR                 00194300
194400******************************************************************00194400
194500 8535-PROCESS-GLPH-TABULAR.                                       00194500
194600                                                                  00194600
194700     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00194700
194800     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00194800
194900                                                                  00194900
195000     COMPUTE VSAM-RECORD-LENGTH         =                         00195000
195100                              4         +                         00195100
195200         GC-GCTABULR-GLPH-FIXED-LEN     +                         00195200
195300         VSAM-ENTRY-COUNT               *                         00195300
195400         GC-GCTABULR-GLPH-VARY-LEN.                               00195400
195500                                                                  00195500
195600     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00195600
195700                                                                  00195700
195800 8535-EXIT.                                                       00195800
195900     EXIT.                                                        00195900
196000/                                                                 00196000
196100******************************************************************00196100
196200****           GROUP SPECIFIC   '#GWHC'   TABULAR                 00196200
196300******************************************************************00196300
196400 8536-PROCESS-GWHC-TABULAR.                                       00196400
196500                                                                  00196500
196600     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00196600
196700     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00196700
196800                                                                  00196800
196900     COMPUTE VSAM-RECORD-LENGTH         =                         00196900
197000                              4         +                         00197000
197100         GC-GCTABULR-GWHC-FIXED-LEN     +                         00197100
197200         VSAM-ENTRY-COUNT               *                         00197200
197300         GC-GCTABULR-GWHC-VARY-LEN.                               00197300
197400                                                                  00197400
197500     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00197500
197600                                                                  00197600
197700 8536-EXIT.                                                       00197700
197800     EXIT.                                                        00197800
197900/                                                                 00197900
198000******************************************************************00198000
198100****           GROUP SPECIFIC   '#GMFH'   TABULAR                 00198100
198200******************************************************************00198200
198300 8550-PROCESS-GMFH-TABULAR.                                       00198300
198400                                                                  00198400
198500     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00198500
198600     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00198600
198700                                                                  00198700
198800     COMPUTE VSAM-RECORD-LENGTH         =                         00198800
198900                              4         +                         00198900
199000         GC-GCTABULR-GMFH-FIXED-LEN     +                         00199000
199100         VSAM-ENTRY-COUNT               *                         00199100
199200         GC-GCTABULR-GMFH-VARY-LEN.                               00199200
199300                                                                  00199300
199400     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00199400
199500                                                                  00199500
199600 8550-EXIT.                                                       00199600
199700     EXIT.                                                        00199700
TM0526******************************************************************00199710
TM0526****           GROUP SPECIFIC   '#GHPA'   TABULAR                 00199720
TM0526******************************************************************00199730
TM0526 8555-PROCESS-GHPA-TABULAR.                                       00199740
TM0526                                                                  00199750
TM0526     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00199760
TM0526     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00199770
TM0526                                                                  00199780
TM0526     COMPUTE VSAM-RECORD-LENGTH         =                         00199790
TM0526                              4         +                         00199791
TM0526         GC-GCTABULR-GHPA-FIXED-LEN     +                         00199792
TM0526         VSAM-ENTRY-COUNT               *                         00199793
TM0526         GC-GCTABULR-GHPA-VARY-LEN.                               00199794
TM0526                                                                  00199795
TM0526     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00199796
TM0526                                                                  00199797
TM0526 8555-EXIT.                                                       00199798
TM0526     EXIT.                                                        00199799
199800/                                                                 00199800
199900******************************************************************00199900
200000****           PROVISION   '#PAQ'   TABULAR                       00200000
200100******************************************************************00200100
200200 8700-PROCESS-PAQ-TABULAR.                                        00200200
200300                                                                  00200300
200400     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00200400
200500     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00200500
200600                                                                  00200600
200700     COMPUTE VSAM-RECORD-LENGTH         =                         00200700
200800                              4         +                         00200800
200900         GC-GCTABULR-PAQ-FIXED-LEN      +                         00200900
201000         VSAM-ENTRY-COUNT               *                         00201000
201100         GC-GCTABULR-PAQ-VARY-LEN.                                00201100
201200                                                                  00201200
201300     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00201300
201400                                                                  00201400
201500 8700-EXIT.                                                       00201500
201600     EXIT.                                                        00201600
201700/                                                                 00201700
201800******************************************************************00201800
201900****           PROVISION   '#PCX'   TABULAR                       00201900
202000******************************************************************00202000
202100 8720-PROCESS-PCX-TABULAR.                                        00202100
202200                                                                  00202200
202300     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00202300
202400     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00202400
202500                                                                  00202500
202600     COMPUTE VSAM-RECORD-LENGTH         =                         00202600
202700                              4         +                         00202700
202800         GC-GCTABULR-PCX-FIXED-LEN      +                         00202800
202900         VSAM-ENTRY-COUNT               *                         00202900
203000         GC-GCTABULR-PCX-VARY-LEN.                                00203000
203100                                                                  00203100
203200     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00203200
203300                                                                  00203300
203400 8720-EXIT.                                                       00203400
203500     EXIT.                                                        00203500
203600/                                                                 00203600
203700******************************************************************00203700
203800****           PROVISION   '#PDR'   TABULAR                       00203800
203900******************************************************************00203900
204000 8740-PROCESS-PDR-TABULAR.                                        00204000
204100                                                                  00204100
204200     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00204200
204300     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00204300
204400                                                                  00204400
204500     COMPUTE VSAM-RECORD-LENGTH         =                         00204500
204600                              4         +                         00204600
204700         GC-GCTABULR-PDR-FIXED-LEN      +                         00204700
204800         VSAM-ENTRY-COUNT               *                         00204800
204900         GC-GCTABULR-PDR-VARY-LEN.                                00204900
205000                                                                  00205000
205100     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00205100
205200                                                                  00205200
205300 8740-EXIT.                                                       00205300
205400     EXIT.                                                        00205400
205500/                                                                 00205500
205600******************************************************************00205600
205700****           PROVISION   '#PPF'   TABULAR                       00205700
205800******************************************************************00205800
205900 8760-PROCESS-PPF-TABULAR.                                        00205900
206000                                                                  00206000
206100     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00206100
206200     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00206200
206300                                                                  00206300
206400     COMPUTE VSAM-RECORD-LENGTH         =                         00206400
206500                              4         +                         00206500
206600         GC-GCTABULR-PPF-FIXED-LEN      +                         00206600
206700         VSAM-ENTRY-COUNT               *                         00206700
206800         GC-GCTABULR-PPF-VARY-LEN.                                00206800
206900                                                                  00206900
207000     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00207000
207100                                                                  00207100
207200 8760-EXIT.                                                       00207200
207300     EXIT.                                                        00207300
207400/                                                                 00207400
207500******************************************************************00207500
207600****           PROVISION   '#PRR'   TABULAR                       00207600
207700******************************************************************00207700
207800 8780-PROCESS-PRR-TABULAR.                                        00207800
207900                                                                  00207900
208000     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00208000
208100     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00208100
208200                                                                  00208200
208300     COMPUTE VSAM-RECORD-LENGTH         =                         00208300
208400                              4         +                         00208400
208500         GC-GCTABULR-PRR-FIXED-LEN      +                         00208500
208600         VSAM-ENTRY-COUNT               *                         00208600
208700         GC-GCTABULR-PRR-VARY-LEN.                                00208700
208800                                                                  00208800
208900     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00208900
209000                                                                  00209000
209100 8780-EXIT.                                                       00209100
209200     EXIT.                                                        00209200
209300/                                                                 00209300
209400******************************************************************00209400
209500****           PROVISION   '#PRV'   TABULAR                       00209500
209600******************************************************************00209600
209700 8800-PROCESS-PRV-TABULAR.                                        00209700
209800                                                                  00209800
209900     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00209900
210000     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00210000
210100                                                                  00210100
210200     COMPUTE VSAM-RECORD-LENGTH         =                         00210200
210300                              4         +                         00210300
210400         GC-GCTABULR-PRV-FIXED-LEN      +                         00210400
210500         VSAM-ENTRY-COUNT               *                         00210500
210600         GC-GCTABULR-PRV-VARY-LEN.                                00210600
210700                                                                  00210700
210800     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00210800
210900                                                                  00210900
211000 8800-EXIT.                                                       00211000
211100     EXIT.                                                        00211100
211200/                                                                 00211200
211300******************************************************************00211300
211400****           PROVISION   '#PSC'   TABULAR                       00211400
211500******************************************************************00211500
211600 8820-PROCESS-PSC-TABULAR.                                        00211600
211700                                                                  00211700
211800     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00211800
211900     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00211900
212000                                                                  00212000
212100     COMPUTE VSAM-RECORD-LENGTH         =                         00212100
212200                              4         +                         00212200
212300         GC-GCTABULR-PSC-FIXED-LEN      +                         00212300
212400         VSAM-ENTRY-COUNT               *                         00212400
212500         GC-GCTABULR-PSC-VARY-LEN.                                00212500
212600                                                                  00212600
212700     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00212700
212800                                                                  00212800
212900 8820-EXIT.                                                       00212900
213000     EXIT.                                                        00213000
213100/                                                                 00213100
213200******************************************************************00213200
213300****           PROVISION   '#PVE'   TABULAR                       00213300
213400******************************************************************00213400
213500 8840-PROCESS-PVE-TABULAR.                                        00213500
213600                                                                  00213600
213700     MOVE  LOW-VALUES                TO  VSAM-RECORD-AREA.        00213700
213800     MOVE  INPUT-TABULAR-RECORD      TO  VSAM-RECORD-AREA.        00213800
213900                                                                  00213900
214000     COMPUTE VSAM-RECORD-LENGTH         =                         00214000
214100                              4         +                         00214100
214200         GC-GCTABULR-PVE-FIXED-LEN      +                         00214200
214300         VSAM-ENTRY-COUNT               *                         00214300
214400         GC-GCTABULR-PVE-VARY-LEN.                                00214400
214500                                                                  00214500
214600     PERFORM 9000-INSERT-NEW-TABULAR THRU 9000-EXIT.              00214600
214700                                                                  00214700
214800 8840-EXIT.                                                       00214800
214900     EXIT.                                                        00214900
215000/                                                                 00215000
215100******************************************************************00215100
215200****          INSERT NEW TABULAR IN VSAM FILE                     00215200
215300******************************************************************00215300
215400 9000-INSERT-NEW-TABULAR.                                         00215400
215500                                                                  00215500
215600**** MOVE CURRENT DATE INTO DATE OF LAST CHANGE.                  00215600
215700     MOVE WS-CURR-JUL-DATE TO VSAM-DT-OF-LAST-CHANGE.             00215700
215800                                                                  00215800
215900     MOVE     'I'           TO   VSAM-REQUEST-TYPE.               00215900
216000     CALL 'TSGVSAM1'  USING  PARM-TAB-ONE-A    PARM-TAB-ONE-B.    00216000
216100                                                                  00216100
216200                                                                  00216200
216300     IF  VSAM-REQUEST-TYPE     EQUAL    '3'                       00216300
216400         MOVE  INPUT-TABULAR-ID       TO  WS-TABULAR-ID           00216400
216500         MOVE  INPUT-TABULAR-SLOT     TO  WS-TABULAR-SLOT         00216500
216600         MOVE  VSAM-FEEDBACK-CODE     TO  WS-ABEND-CODE           00216600
216700         DISPLAY  '  '                                            00216700
216800         DISPLAY  'GC0210  DUPLICATE KEY  9000-INSERT-NEW-TABULAR'00216800
216900         DISPLAY  '  '                                            00216900
217000         DISPLAY  ' DUPLICATE KEY    =   '        WS-DISPLAY-KEY  00217000
217100         DISPLAY  '  '                                            00217100
217200         GO TO  9999-ERROR-RTN.                                   00217200
217300                                                                  00217300
217400                                                                  00217400
217500                                                                  00217500
217600     IF  VSAM-REQUEST-TYPE   NOT EQUAL   'I'                      00217600
217700         DISPLAY  '  '                                            00217700
217800         DISPLAY  ' GC0210  BAD INSERT  9000-INSERT-NEW-TABULAR'  00217800
217900         MOVE VSAM-FEEDBACK-CODE  TO  WS-ABEND-CODE               00217900
218000         GO TO  9999-ERROR-RTN.                                   00218000
218100                                                                  00218100
218200                                                                  00218200
218300     ADD 1  TO  WS-RECORDS-ADDED.                                 00218300
218400                                                                  00218400
218500 9000-EXIT.                                                       00218500
218600     EXIT.                                                        00218600
218700/                                                                 00218700
218800******************************************************************00218800
218900**             CLOSE SEQUENTIAL AND VSAM FILES                    00218900
219000******************************************************************00219000
219100 9500-CLOSE-THE-FILES.                                            00219100
219200                                                                  00219200
219300     CLOSE INPUT-SLOT-UPDATE-FILE.                                00219300
219400                                                                  00219400
219500                                                                  00219500
219600     MOVE     'C'           TO   VSAM-REQUEST-TYPE.               00219600
219700     CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    00219700
219800                                                                  00219800
219900                                                                  00219900
220000     IF  VSAM-REQUEST-TYPE    NOT EQUAL   'C'                     00220000
220100         DISPLAY '  '                                             00220100
220200         DISPLAY ' BAD CLOSE   GC0210    9500-CLOSE-THE-FILES'    00220200
220300         MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             00220300
220400         GO TO  9999-ERROR-RTN.                                   00220400
220500                                                                  00220500
220600 9500-EXIT.                                                       00220600
220700     EXIT.                                                        00220700
220800/                                                                 00220800
220900******************************************************************00220900
221000**                       A B E N D                                00221000
221100******************************************************************00221100
221200 9999-ERROR-RTN.                                                  00221200
221300                                                                  00221300
221400     CALL  'TSGEND' USING  WS-ABEND-CODE.                         00221400
221500                                                                  00221500
221600 9999-EXIT.                                                       00221600
221700     EXIT.                                                        00221700
221800/                                                                 00221800
