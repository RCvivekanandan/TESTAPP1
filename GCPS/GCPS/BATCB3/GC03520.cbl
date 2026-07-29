000100 IDENTIFICATION DIVISION.                                         00000100
000200                                                                  00000200
000300 PROGRAM-ID. GC03520.                                             00000300
000400 AUTHOR. GARY D MULLINGS.                                         00000400
000500                                                                  00000500
000600 INSTALLATION. HCSC-HCMS.                                         00000600
000700 DATE-WRITTEN.                                                    00000700
000800 DATE-COMPILED.                                                   00000800
000900******************************************************************00000900
001000*                                                                *00001000
001100*    THIS PROGRAM WILL WRITE A REPORT FOR QUALITY CONTROL        *00001100
001200*    GIVING TOTAL NUMBER OF PROCESSING DONE BY AN EMPLOYEE.      *00001200
001300*                                                                *00001300
001400*    INPUT FILES:                                                *00001400
001500*       QCF-FILE - QUALITY CONTROL EXTRACT FILE                  *00001500
001600*       NME-FILE - CODES VSAM FILE WITH THE OPERATOR NAMES       *00001600
001700*                                                                *00001700
001800*    OUTPUT FILES:                                               *00001800
001900*       NONE                                                     *00001900
002000*                                                                *00002000
002100*    OUTPUT REPORTS:                                             *00002100
002200*       RPT-FILE - FINAL REPORT TOTALS                           *00002200
002300*                                                                *00002300
002400******************************************************************00002400
002500*                                                                *00002500
002600*      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *00002600
002700*      *-*         U P D A T E   H I S T O R Y         *-*       *00002700
002800*      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *00002800
002900*                                                                *00002900
003000* CHG #    DATE    BY              DESCRIPTION                   *00003000
003100* _____  ________  ___  ___________________________________      *00003100
003200*        01/14/97  GDM  CODED ORIGINAL                           *00003200
003300*                                                                *00003300
003400* D374   04/01/03  GTF  RECOMPILE FOR COPYBOOK CHANGE            *00003400
003500*                                                                *00003500
003600* P00148 01/28/04  KDM  RECOMPILE FOR PRIME                      *00003600
003700*                                                                *00003700
003800* DM9400 05/30/07  LR  MODIFIED TO SUPPORT #GMFH TABULAR:        *00003800
003900*         IN GROUP SPECIFIC ENTRY CHANGED                        *00003900
004000*         FROM:     WT-02-DESCRIPTION-VALUES    OCCURS 046 TIMES *00004000
004100*           TO:     WT-02-DESCRIPTION-VALUES    OCCURS 057 TIMES *00004100
004200*         IN CONTRACT ENTRY CHANGED                              *00004200
004300*         FROM:     WT-02-DESCRIPTION-VALUES    OCCURS 019 TIMES *00004300
004400*           TO:     WT-02-DESCRIPTION-VALUES    OCCURS 022 TIMES *00004400
004500* P21681 10/23/2017 RECOMPILE FOR CB GCARCHGS*                    00004500
      *                                                                *00004510
      *        06/26/22 TEB  RECOMPILE FOR COPYBOOK GCARCHCT CHANGE    *00004520
      *                                                                *00004530
      *            P00027450 - NSA ASO OPT IN-OUT FIELD                *00004540
      *            08/03/22  TB   RECOMPILE FOR COPYBOOK GCYCNVTC CHG  *00004550
      *                                                                *00004560
      *            P00027450 - CORRECT NSA ASO OPT IN-OUT FIELD        *00004570
      *            11/30/22  TB   RECOMPILE FOR COPYBOOK GCYCNVTC CHG  *00004580
      *                                                                *00004590
      *            P00029516 - BALANCE BILLING PROTECTION OPTION FIELD *00004591
      *            02/22/24  TB   RECOMPILE FOR COPYBOOK GCYCNVTC CHG  *00004592
      *                                                                *00004593
      *            P00029711 - EXTERNAL FINANCE INDICATOR FIELD       * 00004594
      *            03/12/24  SI   RECOMPILE FOR COPYBOOK GCYCNVTC CHG * 00004595
      *                                                               * 00004596
      *            P00028776 - BALANCE BILLING PROTECTION OPTION FIELD *00004597
      *            07/18/24  BB   RECOMPILE FOR COPYBOOK GCARCHCT CHG  *00004598
      *                                                                 00004599
      *            P00030333 - MULTI TIER INDICATOR FIELD             * 00004600
      *            07/14/25  CB   RECOMPILE FOR COPYBOOK GCYCNVTC CHG * 00004601
      *                                                                *00004602
PG1125*            P000XXXXX - RX COUPON PROGRAM INDICATOR             *00004603
PG1125*            11/17/25  PG   RECOMPILE FOR COPYBOOK GCYCNVTC CHG  *00004604
PG1125*                                                                *00004605
TM0526*BBDA-66049 04/10/26 TM    MODIFIED TO SUPPORT #GHPA TABULAR:    *00004606
TM0526*         IN GROUP SPECIFIC ENTRY CHANGED                        *00004607
TM0526*         FROM:     WT-02-DESCRIPTION-VALUES    OCCURS 057 TIMES *00004608
TM0526*           TO:     WT-02-DESCRIPTION-VALUES    OCCURS 058 TIMES *00004609
PG0626* P30337     06/15/26    PG   RECOMPILE -  GCARCHGS.             *00004610
004600******************************************************************00004620
004700/                                                                 00004700
004800 ENVIRONMENT DIVISION.                                            00004800
004900 CONFIGURATION SECTION.                                           00004900
005000 SOURCE-COMPUTER. IBM-370.                                        00005000
005100 OBJECT-COMPUTER. IBM-370.                                        00005100
005200                                                                  00005200
005300 INPUT-OUTPUT SECTION.                                            00005300
005400 FILE-CONTROL.                                                    00005400
005500                                                                  00005500
005600     SELECT QCF-FILE   ASSIGN TO UT-S-GC03520A.                   00005600
005700     SELECT RPT-FILE   ASSIGN TO UT-S-GC03520C.                   00005700
005800/                                                                 00005800
005900 DATA DIVISION.                                                   00005900
006000 FILE SECTION.                                                    00006000
006100                                                                  00006100
006200 FD  QCF-FILE                                                     00006200
006300     LABEL RECORDS ARE STANDARD                                   00006300
006400     RECORDING MODE IS F                                          00006400
006500     BLOCK CONTAINS 0 RECORDS.                                    00006500
006600                                                                  00006600
006700 01  QCF-RECORD.                                                  00006700
006800     COPY GCQCF.                                                  00006800
006900                                                                  00006900
007000 FD  RPT-FILE                                                     00007000
007100     LABEL RECORDS ARE STANDARD                                   00007100
007200     RECORDING MODE IS F                                          00007200
007300     BLOCK CONTAINS 0 RECORDS.                                    00007300
007400                                                                  00007400
007500 01  RPT-RECORD           PIC X(133).                             00007500
007600/                                                                 00007600
007700 WORKING-STORAGE SECTION.                                         00007700
007800 01  FILLER                    PIC X(24)   VALUE                  00007800
007900     'GC03520 WORKING-STORAGE'.                                   00007900
008000                                                                  00008000
008100 01  ABEND-CODE                PIC 9(4)    COMP.                  00008100
008200 01  WS-OPER-ID                PIC X(05) VALUE SPACES.            00008200
008300 01  RPT-IND    VALUE '9999'   PIC X(4).                          00008300
008400                                                                  00008400
008500 COPY HSCDATES.                                                   00008500
008600 01  DATE-TIME-WORK.                                              00008600
008700     05  TIME-AREA.                                               00008700
008800         10  TIME-HH           PIC XX.                            00008800
008900         10  TIME-MM           PIC XX.                            00008900
009000         10  TIME-SS           PIC XX.                            00009000
009100     05  WS-DATE-AREA.                                            00009100
009200         10  WS-MDY.                                              00009200
009300             15  WS-M          PIC 99    VALUE ZERO.              00009300
009400             15  WS-D          PIC 99    VALUE ZERO.              00009400
009500             15  WS-Y          PIC 99    VALUE ZERO.              00009500
009600         10  WS-YYDDD          PIC 9(5)  VALUE ZERO.              00009600
009700         10  FILLER REDEFINES WS-YYDDD.                           00009700
009800             15  WS-YY         PIC 99.                            00009800
009900             15  WS-DDD        PIC 999.                           00009900
010000         10  HOLD-CURR-DATE-JUL  PIC S9(5) COMP-3 VALUE ZEROS.    00010000
010100                                                                  00010100
010200 01  CEN-DATE.                                                    00010200
010300     05  CEN-DT.                                                  00010300
010400         10  CEN-CC            PIC 99.                            00010400
010500         10  CEN-YY            PIC 99.                            00010500
010600                                                                  00010600
010700 01  HLD-DATE.                                                    00010700
010800     05  HLD-DATE-AREA         PIC 9(07).                         00010800
010900     05  HLD-DATE-WORK REDEFINES HLD-DATE-AREA.                   00010900
011000         10  HLD-CEN           PIC 99.                            00011000
011100         10  HLD-JUL.                                             00011100
011200             15  HLD-YY        PIC 99.                            00011200
011300             15  HLD-DAYS      PIC 999.                           00011300
011400                                                                  00011400
011500 01  WS-CV-CODE-VALUE.                                            00011500
011600     05  WS-CD-CODE-VAL        PIC X(10).                         00011600
011700     05  WS-CD-CDE-VAL REDEFINES WS-CD-CODE-VAL.                  00011700
011800         10  WS-CD-CODE        PIC X(08).                         00011800
011900         10  WS-CD-CODE-FLR    PIC X(02).                         00011900
012000                                                                  00012000
012100 01  WS-QCF-FUNCTION-FIELD.                                       00012100
012200     05  WS-QCF-FUNC-FIELD     PIC X(08).                         00012200
012300     05  WS-QCF-FIRST-TWO     REDEFINES WS-QCF-FUNC-FIELD.        00012300
012400         10  WS-QCF-ID-1       PIC X(02).                         00012400
012500         10  WS-QCF-ID-2       PIC X(04).                         00012500
012600         10  WS-QCF-ID-3       PIC X(02).                         00012600
012700     05  WS-QCF-FIRST-FOUR    REDEFINES WS-QCF-FUNC-FIELD.        00012700
012800         10  WS-QCF-FUNC       PIC X(04).                         00012800
012900         10  FILLER            PIC X(04).                         00012900
013000                                                                  00013000
013100 01  DATE-AREA.                                                   00013100
013200     05  GREG-DATE             PIC 9(6)  VALUE ZEROS.             00013200
013300     05  JULIAN-DATE           PIC 9(5)  VALUE ZEROS.             00013300
013400     05  WS-CHNG-DATE          PIC 9(5)  VALUE ZEROS.             00013400
013500**** THE ABOVE DATE FORMAT IS YYDDD ***                           00013500
013600                                                                  00013600
013700 01  END-OF-FILE-RND           PIC XXX   VALUE SPACES.            00013700
013800     88  END-OF-RND-FILE           VALUE 'END'.                   00013800
013900                                                                  00013900
014000 01  END-OF-FILE-QCF           PIC XXX   VALUE SPACES.            00014000
014100     88  END-OF-QCF-FILE           VALUE 'END'.                   00014100
014200                                                                  00014200
014300 01  NAME-SW                   PIC X     VALUE SPACES.            00014300
014400     88  GOOD-NAME                 VALUE 'Y'.                     00014400
014500     88  NT-GOOD-NAME              VALUE 'N'.                     00014500
014600                                                                  00014600
014700 01  NAME-FOUND-SW             PIC X     VALUE SPACES.            00014700
014800     88  NAME-FOUND                VALUE 'Y'.                     00014800
014900     88  NAME-NOT-FOUND            VALUE 'N'.                     00014900
015000                                                                  00015000
015100 01  TAB-SW                    PIC X     VALUE SPACES.            00015100
015200     88  TAB-FND                   VALUE 'Y'.                     00015200
015300     88  TAB-NT-FND                VALUE 'N'.                     00015300
015400                                                                  00015400
015500 01  FIELD-SW                  PIC X     VALUE SPACES.            00015500
015600     88  FIELD-FND                 VALUE 'Y'.                     00015600
015700     88  FIELD-NT-FND              VALUE 'N'.                     00015700
015800                                                                  00015800
015900 01  OPERATOR-SW               PIC X     VALUE SPACES.            00015900
016000     88  OPER-ON                   VALUE 'Y'.                     00016000
016100     88  OPER-OFF                  VALUE 'N'.                     00016100
016200                                                                  00016200
016300 01  GROUP-NO-SW               PIC X     VALUE SPACES.            00016300
016400     88  GROUP-ON                  VALUE 'Y'.                     00016400
016500     88  GROUP-OFF                 VALUE 'N'.                     00016500
016600                                                                  00016600
016700 01  WS-TABLE-SW               PIC X     VALUE SPACES.            00016700
016800     88  FOUND-SW                  VALUE 'Y'.                     00016800
016900     88  NT-FOUND-SW               VALUE 'N'.                     00016900
017000                                                                  00017000
017100 01  WS-TEST-GROUP.                                               00017100
017200     05  WS-TEST-GRP-NO        PIC  X(6) VALUE SPACE.             00017200
017300                                                                  00017300
017400 01  WS-GROUP-TABLE.                                              00017400
017500     05  WS-GROUP-TBL OCCURS 200 TIMES.                           00017500
017600         10  WS-GRP-NO         PIC  X(6).                         00017600
017700                                                                  00017700
017800 01  COUNTER-AREA.                                                00017800
017900     05  WS-QCF-COUNT          PIC S9(9) VALUE ZEROS.             00017900
018000     05  RPT-LINE-COUNT        PIC S9(9) VALUE ZEROS.             00018000
018100     05  RPT-PAGE-COUNT        PIC S9(9) VALUE ZEROS.             00018100
018200     05  DISPLAY-FEEDBACK      PIC S9(04) COMP-3 VALUE +0.        00018200
018300     05  WS-SUB-1              PIC 999   VALUE ZEROS.             00018300
018400     05  WS-SUB-2              PIC 999   VALUE ZEROS.             00018400
018500     05  WS-TBL-CNT            PIC 999   VALUE ZEROS.             00018500
018600     05  WS-RPT-FLD-MNT        PIC 9(05) VALUE ZEROS.             00018600
018700     05  WS-RPT-BEN-PRV        PIC 9(05) VALUE ZEROS.             00018700
018800     05  WS-RPT-TABLRS         PIC 9(05) VALUE ZEROS.             00018800
018900     05  WS-RPT-ACCUMS         PIC 9(05) VALUE ZEROS.             00018900
019000     05  WS-RPT-MAPPING        PIC 9(05) VALUE ZEROS.             00019000
019100     05  WS-RPT-GCCP           PIC 9(05) VALUE ZEROS.             00019100
019200                                                                  00019200
019300 01  HOLD-LAST-AREA.                                              00019300
019400     05  LAST-OPERATOR         PIC X(08).                         00019400
019500     05  LAST-GROUP-NO         PIC X(09).                         00019500
019600     05  LAST-CODE-VALUE       PIC X(10).                         00019600
019700     05  LAST-PREFIX           PIC X(08).                         00019700
019800     05  HLD-NT-GOOD-NM-VALUE  PIC X(08).                         00019800
019900                                                                  00019900
020000 01  WS-DATE.                                                     00020000
020100     05  WS-DATE-YR            PIC X(02).                         00020100
020200     05  WS-DATE-MO            PIC X(02).                         00020200
020300     05  WS-DATE-DY            PIC X(02).                         00020300
020400                                                                  00020400
020500 01  WS-TIME.                                                     00020500
020600     05  WS-TIME-HR            PIC X(02).                         00020600
020700     05  WS-TIME-MN            PIC X(02).                         00020700
020800     05  WS-TIME-SC            PIC X(02).                         00020800
020900     05  WS-TIME-HH            PIC X(02).                         00020900
021000                                                                  00021000
021100 01  OPER-NAME.                                                   00021100
021200     05  HLD-FRST-BYTE         PIC X(01).                         00021200
021300     05  HLD-OPER-NAME         PIC X(49).                         00021300
021400                                                                  00021400
021500 01  FUNC-FIELD.                                                  00021500
021600     05  HLD-FUNC-FIELD.                                          00021600
021700         10  HLD-TWO-BYTES     PIC X(02).                         00021700
021800         10  HLD-FUNC-AREA     PIC X(06).                         00021800
021900                                                                  00021900
022000 01  BENEFIT-PROV.                                                00022000
022100     05  HOLD-BEN-PROV.                                           00022100
022200         10  HLD-BEN-TWO-BYTES  PIC X(02).                        00022200
022300         10  HLD-BEN-AREA       PIC X(06).                        00022300
022400                                                                  00022400
022500 01  BEN-DESCRIPTION.                                             00022500
022600     05  FILLER                PIC X(10)                          00022600
022700           VALUE 'PROVISION '.                                    00022700
022800     05  BEN-PROV-OUT          PIC X(06).                         00022800
022900                                                                  00022900
023000 01  WS-MESSAGES.                                                 00023000
023100     05  NO-NM-FND-MSG         PIC X(31)                          00023100
023200           VALUE 'NO NAME FOUND FOR THIS OPERATOR'.               00023200
023300                                                                  00023300
023400 01  RPT-MAIN-HDR-L1.                                             00023400
023500     05  FILLER                PIC X     VALUE SPACES.            00023500
023600     05  FILLER                PIC X(15)                          00023600
023700           VALUE 'REPORT ID:     '.                               00023700
023800     05  RPT-ID-FIELD          PIC X(04) VALUE SPACES.            00023800
023900     05  FILLER                PIC X(29).                         00023900
024000     05  FILLER                PIC X(34)                          00024000
024100           VALUE 'BLUE CROSS/BLUE SHIELD OF ILLINOIS'.            00024100
024200     05  FILLER                PIC X(31).                         00024200
024300     05  FILLER                PIC X(10)                          00024300
024400           VALUE 'PAGE:     '.                                    00024400
024500     05  RPT-PAGE-CNT          PIC 9(04).                         00024500
024600     05  FILLER                PIC X(05).                         00024600
024700                                                                  00024700
024800 01  RPT-MAIN-HDR-L2.                                             00024800
024900     05  FILLER                PIC X   VALUE SPACES.              00024900
025000     05  FILLER                PIC X(11)                          00025000
025100          VALUE 'RUN TIME : '.                                    00025100
025200     05  RPT-RUN-TIME.                                            00025200
025300         10  RPT-RUN-HR        PIC 99.                            00025300
025400         10  FILLER            PIC X  VALUE ':'.                  00025400
025500         10  RPT-RUN-MIN       PIC 99.                            00025500
025600         10  FILLER            PIC X  VALUE ':'.                  00025600
025700         10  RPT-RUN-SEC       PIC 99.                            00025700
025800     05  FILLER                PIC X(23).                         00025800
025900     05  FILLER                PIC X(46)                          00025900
026000          VALUE 'QUALITY CONTROL CONTRACT/GROUP SPECIFIC REPORT'. 00026000
026100     05  FILLER                PIC X(25).                         00026100
026200     05  FILLER                PIC X(10)                          00026200
026300          VALUE 'RUN DATE: '.                                     00026300
026400     05  RPT-RUN-DATE.                                            00026400
026500         10  RPT-RUN-MO        PIC 99.                            00026500
026600         10  FILLER            PIC X  VALUE '/'.                  00026600
026700         10  RPT-RUN-DY        PIC 99.                            00026700
026800         10  FILLER            PIC X  VALUE '/'.                  00026800
026900         10  RPT-RUN-YR        PIC 99.                            00026900
027000     05  FILLER                PIC X(04).                         00027000
027100                                                                  00027100
027200 01  RPT-OPER-HDR-L1.                                             00027200
027300     05  FILLER                PIC X   VALUE SPACES.              00027300
027400     05  FILLER                PIC X(16)                          00027400
027500           VALUE 'OPERATOR NAME:  '.                              00027500
027600     05  RPT-OPER-NAME         PIC X(50).                         00027600
027700     05  FILLER                PIC X(65).                         00027700
027800                                                                  00027800
027900 01  RPT-OPER-HDR-L2.                                             00027900
028000     05  FILLER                PIC X   VALUE SPACES.              00028000
028100     05  FILLER                PIC X(16)                          00028100
028200           VALUE 'OPERATOR ID:    '.                              00028200
028300     05  RPT-OPERATOR-ID       PIC X(08).                         00028300
028400     05  FILLER                PIC X(107).                        00028400
028500                                                                  00028500
028600 01  RPT-DETAIL-HDR-L1.                                           00028600
028700     05  FILLER                PIC X(40) VALUE SPACES.            00028700
028800     05  FILLER                PIC X(11)                          00028800
028900           VALUE 'MAINTENANCE'.                                   00028900
029000     05  FILLER                PIC X(19) VALUE SPACES.            00029000
029100     05  FILLER                PIC X(05)                          00029100
029200           VALUE 'TOTAL'.                                         00029200
029300                                                                  00029300
029400 01  RPT-DETAIL-HDR-L2.                                           00029400
029500     05  FILLER                PIC X(40) VALUE SPACES.            00029500
029600     05  FILLER                PIC X(11)                          00029600
029700           VALUE 'DESCRIPTION'.                                   00029700
029800     05  FILLER                PIC X(19) VALUE SPACES.            00029800
029900     05  FILLER                PIC X(09)                          00029900
030000           VALUE 'CHANGES  '.                                     00030000
030100                                                                  00030100
030200 01  RPT-DETAIL-HDR-L3.                                           00030200
030300     05  FILLER                PIC X(40) VALUE SPACES.            00030300
030400     05  FILLER                PIC X(11) VALUE ALL '_'.           00030400
030500     05  FILLER                PIC X(19).                         00030500
030600     05  FILLER                PIC X(09) VALUE ALL '_'.           00030600
030700                                                                  00030700
030800 01  RPT-DTL-L-1.                                                 00030800
030900     05  FILLER                PIC X(40).                         00030900
031000     05  FILLER                PIC X(17)                          00031000
031100           VALUE 'FIELD MAINTENANCE'.                             00031100
031200     05  FILLER                PIC X(13).                         00031200
031300     05  RPT-FLD-MNT           PIC ZZZZ9.                         00031300
031400     05  FILLER                PIC X(58).                         00031400
031500                                                                  00031500
031600 01  RPT-DTL-L-2.                                                 00031600
031700     05  FILLER                PIC X(40).                         00031700
031800     05  FILLER                PIC X(17)                          00031800
031900           VALUE 'BENEFIT PROVISION'.                             00031900
032000     05  FILLER                PIC X(13).                         00032000
032100     05  RPT-BEN-PRV           PIC ZZZZ9.                         00032100
032200                                                                  00032200
032300 01  RPT-DTL-L-3.                                                 00032300
032400     05  FILLER                PIC X(40).                         00032400
032500     05  FILLER                PIC X(17)                          00032500
032600           VALUE 'TABULARS         '.                             00032600
032700     05  FILLER                PIC X(13).                         00032700
032800     05  RPT-TABLRS            PIC ZZZZ9.                         00032800
032900                                                                  00032900
033000 01  RPT-DTL-L-4.                                                 00033000
033100     05  FILLER                PIC X(40).                         00033100
033200     05  FILLER                PIC X(17)                          00033200
033300           VALUE 'ACCUMS           '.                             00033300
033400     05  FILLER                PIC X(13).                         00033400
033500     05  RPT-ACCUMS            PIC ZZZZ9.                         00033500
033600                                                                  00033600
033700 01  RPT-DTL-L-5.                                                 00033700
033800     05  FILLER                PIC X(40).                         00033800
033900     05  FILLER                PIC X(17)                          00033900
034000           VALUE 'MAPPING          '.                             00034000
034100     05  FILLER                PIC X(13).                         00034100
034200     05  RPT-MAPPING           PIC ZZZZ9.                         00034200
034300                                                                  00034300
034400 01  RPT-DTL-L-6.                                                 00034400
034500     05  FILLER                PIC X(40).                         00034500
034600     05  FILLER                PIC X(17)                          00034600
034700           VALUE '#GCCP            '.                             00034700
034800     05  FILLER                PIC X(13).                         00034800
034900     05  RPT-GCCP              PIC ZZZZ9.                         00034900
035000                                                                  00035000
035100 01  RPT-DTL-L-7.                                                 00035100
035200     05  FILLER                PIC X(01).                         00035200
035300     05  FILLER                PIC X(17)                          00035300
035400           VALUE 'GROUPS CHANGED   '.                             00035400
035500     05  FILLER                PIC X(115).                        00035500
035600                                                                  00035600
035700 01  RPT-DTL-L-8.                                                 00035700
035800     05 FILLER                 PIC  X(01) VALUE SPACE.            00035800
035900     05 WS-TBL-COL  OCCURS 16 TIMES.                              00035900
036000        10  WS-TBL-GRP-NO.                                        00036000
036100            15  WS-TBL-GRP            PIC  X(6).                  00036100
036200            15  FILLER                PIC  X(2).                  00036200
036300                                                                  00036300
036400 01  RPT-DTL-L-9.                                                 00036400
036500     05 FILLER                 PIC  X(01) VALUE SPACES.           00036500
036600     05 FILLER                 PIC  X(132) VALUE SPACES.          00036600
036700                                                                  00036700
036800 01  RPT-OPER-LINE.                                               00036800
036900     05  FILLER                PIC X(50)                          00036900
037000          VALUE '**  NO NAME FOUND FOR THIS OPERATOR  **'.        00037000
037100                                                                  00037100
037200/                                                                 00037200
037300 01  WS-REC-LEN-AREA.                                             00037300
037400 COPY GCCDRLEN.                                                   00037400
037500/                                                                 00037500
037600******************************************************************00037600
037700****          SET PARAMETER FOR ALL VSAM FILES USED           ****00037700
037800******************************************************************00037800
037900 01  PARM-SET.                                                    00037900
038000     05  SET-RDW.                                                 00038000
038100         10  SET-RECORD-LENGTH             PIC 9(04) COMP VALUE 0.00038100
038200         10  SET-FEEDBACK-CODE             PIC 9(04) COMP VALUE 0.00038200
038300     05  SET-VALUE                         PIC 9(08) COMP VALUE 0.00038300
038400                                                                  00038400
038500******************************************************************00038500
038600****          VSAM CALL PARAMETERS                            ****00038600
038700******************************************************************00038700
038800 01  PARM-ONE.                                                    00038800
038900     05  RESERVED                          PIC 9(05) COMP VALUE 0.00038900
039000     05  RESERVED-X REDEFINES RESERVED.                           00039000
039100         10  REQUEST-TYPE                  PIC X(01).             00039100
039200         10  REQUEST-FILLER                PIC X(03).             00039200
039300                                                                  00039300
039400******************************************************************00039400
039500****          PARAMETERS FOR CODE/NAME RECORDS                ****00039500
039600******************************************************************00039600
039700 01  PARM-CV-NAME.                                                00039700
039800     05  CV-NAME-RDW.                                             00039800
039900         10  CVNM-RECORD-LENGTH            PIC 9(04) COMP VALUE 0.00039900
040000         10  CVNM-FEEDBACK-CODE            PIC 9(04) COMP VALUE 0.00040000
040100     COPY ELPCVC.                                                 00040100
040200/                                                                 00040200
040300                                                                  00040300
040400******************************************************************00040400
040500****          BIM/AIM CONTRACT FIELDS TABLE                   ****00040500
040600******************************************************************00040600
040700     COPY GCARCHCT.                                               00040700
040800                                                                  00040800
040900                                                                  00040900
041000******************************************************************00041000
041100****          BIM/AIM GROUP SPECIFIC FIELDS TABLE             ****00041100
041200******************************************************************00041200
041300     COPY GCARCHGS.                                               00041300
041400/                                                                 00041400
041500                                                                  00041500
041600******************************************************************00041600
041700****          TABULAR DESCRIPTION TABLES                      ****00041700
041800******************************************************************00041800
041900                                                                  00041900
042000*** GROUP SPECIFIC ***                                            00042000
042100     COPY GCGTABS.                                                00042100
042200     05  GRSP-DESCRIPTION-TABLE      REDEFINES                    00042200
042300         WT-02-DESCRIPTION-VALUES    OCCURS 058 TIMES             00042300
042400                                     INDEXED BY GRSP-INDEX.       00042400
042500         10  GRSP-ENTRY.                                          00042500
042600             15  GRSP-TAB-ID             PIC X(06).               00042600
042700             15  GRSP-TAB-DESCRIPTION    PIC X(25).               00042700
042800             15  GRSP-TAB-DELETE-PGM     PIC X(08).               00042800
042900             15  GRSP-TAB-ADD-PGM        PIC X(08).               00042900
043000/                                                                 00043000
043100*** CONTRACT ***                                                  00043100
043200     COPY GCCTABS.                                                00043200
043300     05  CONT-DESCRIPTION-TABLE      REDEFINES                    00043300
043400         WT-02-DESCRIPTION-VALUES    OCCURS 022 TIMES             00043400
043500                                     INDEXED BY CONT-INDEX.       00043500
043600         10  CONT-ENTRY.                                          00043600
043700             15  CONT-TAB-ID             PIC X(06).               00043700
043800             15  CONT-TAB-DESCRIPTION    PIC X(25).               00043800
043900             15  CONT-TAB-DELETE-PGM     PIC X(08).               00043900
044000             15  CONT-TAB-ADD-PGM        PIC X(08).               00044000
044100/                                                                 00044100
044200 PROCEDURE DIVISION.                                              00044200
044300 0000-MAINLINE.                                                   00044300
044400                                                                  00044400
044500     PERFORM 0025-OPEN-AND-INITIALIZE THRU 0025-EXIT.             00044500
044600     PERFORM 0250-PROCESS THRU 0250-EXIT                          00044600
044700             UNTIL END-OF-QCF-FILE.                               00044700
044800                                                                  00044800
044900     IF END-OF-QCF-FILE                                           00044900
045000        IF CV-RECORD-PREFIX = LAST-PREFIX                         00045000
045100           PERFORM 0090-READ-NAME-FILE    THRU 0090-EXIT          00045100
045200           PERFORM 0420-FINISH  THRU 0420-EXIT                    00045200
045300             UNTIL CV-RECORD-PREFIX NOT = LAST-PREFIX.            00045300
045400                                                                  00045400
045500     MOVE 'C' TO REQUEST-TYPE.                                    00045500
045600     CALL 'TSGVSAM1' USING PARM-ONE                               00045600
045700                           PARM-SET.                              00045700
045800                                                                  00045800
045900     IF  REQUEST-TYPE NOT = 'C'                                   00045900
046000         DISPLAY '*** INVALID CLOSE OF INPUT CODE FILE ***'       00046000
046100         MOVE SET-FEEDBACK-CODE TO ABEND-CODE                     00046100
046200                                   DISPLAY-FEEDBACK               00046200
046300         DISPLAY ' FEEDBACK CODE = ' DISPLAY-FEEDBACK             00046300
046400         GO TO 9999-ABEND-ROUTINE.                                00046400
046500                                                                  00046500
046600     CLOSE  QCF-FILE                                              00046600
046700            RPT-FILE.                                             00046700
046800     GOBACK.                                                      00046800
046900                                                                  00046900
047000 0000-EXIT.                                                       00047000
047100     EXIT.                                                        00047100
047200/                                                                 00047200
047300**************************************************************    00047300
047400*   OPEN FILES AND INITIALIZE THE REPORT HEADING             *    00047400
047500**************************************************************    00047500
047600 0025-OPEN-AND-INITIALIZE.                                        00047600
047700                                                                  00047700
047800     OPEN   INPUT      QCF-FILE.                                  00047800
047900     OPEN   OUTPUT     RPT-FILE.                                  00047900
048000                                                                  00048000
048100     MOVE 'S' TO REQUEST-TYPE.                                    00048100
048200     MOVE 8   TO SET-RECORD-LENGTH.                               00048200
048300     MOVE 3   TO SET-VALUE.                                       00048300
048400                                                                  00048400
048500     CALL 'TSGVSAM1' USING PARM-ONE                               00048500
048600                           PARM-SET.                              00048600
048700                                                                  00048700
048800     IF  REQUEST-TYPE NOT = 'S'                                   00048800
048900         DISPLAY '*** INVALID OPEN OF INPUT CODE FILE ***'        00048900
049000         MOVE SET-FEEDBACK-CODE TO ABEND-CODE                     00049000
049100                                   DISPLAY-FEEDBACK               00049100
049200         DISPLAY ' FEEDBACK CODE = ' DISPLAY-FEEDBACK             00049200
049300         GO TO 9999-ABEND-ROUTINE.                                00049300
049400     PERFORM 0261-TSGVSAM-POINT  THRU 0261-EXIT.                  00049400
049500     PERFORM 0090-READ-NAME-FILE THRU 0090-EXIT.                  00049500
049600     PERFORM 0200-READ-QCF-FILE THRU 0200-EXIT.                   00049600
049700     PERFORM 0050-FRMT-DT-TIME  THRU 0050-EXIT.                   00049700
049800                                                                  00049800
049900 0025-EXIT.                                                       00049900
050000     EXIT.                                                        00050000
050100/                                                                 00050100
050200**************************************************************    00050200
050300*   FORMAT DATE AND TIME FOR REPORT HEADING                  *    00050300
050400**************************************************************    00050400
050500 0050-FRMT-DT-TIME.                                               00050500
050600                                                                  00050600
050700     ACCEPT WS-DATE FROM DATE.                                    00050700
050800     MOVE WS-DATE-MO  TO RPT-RUN-MO.                              00050800
050900     MOVE WS-DATE-DY  TO RPT-RUN-DY.                              00050900
051000     MOVE WS-DATE-YR  TO RPT-RUN-YR.                              00051000
051100                                                                  00051100
051200     ACCEPT WS-TIME FROM TIME.                                    00051200
051300     MOVE WS-TIME-HR  TO RPT-RUN-HR.                              00051300
051400     MOVE WS-TIME-MN  TO RPT-RUN-MIN.                             00051400
051500     MOVE WS-TIME-SC  TO RPT-RUN-SEC.                             00051500
051600                                                                  00051600
051700 0050-EXIT.                                                       00051700
051800     EXIT.                                                        00051800
051900/                                                                 00051900
052000**************************************************************    00052000
052100*   READ THE VSAM NAME FILE SEQUENTIALLY                     *    00052100
052200**************************************************************    00052200
052300 0090-READ-NAME-FILE.                                             00052300
052400                                                                  00052400
052500     MOVE 'G' TO REQUEST-TYPE.                                    00052500
052600     CALL 'TSGVSAM1' USING PARM-ONE                               00052600
052700                           PARM-CV-NAME.                          00052700
052800                                                                  00052800
052900     IF  REQUEST-TYPE NOT = 'G'                                   00052900
053000         DISPLAY '*** INVALID READ NEXT OF INPUT CODE FILE ***'   00053000
053100         MOVE SET-FEEDBACK-CODE TO ABEND-CODE                     00053100
053200                                   DISPLAY-FEEDBACK               00053200
053300         DISPLAY ' FEEDBACK CODE = ' DISPLAY-FEEDBACK             00053300
053400         GO TO 9999-ABEND-ROUTINE.                                00053400
053500                                                                  00053500
053600     MOVE CV-CODE-VALUE TO WS-CD-CODE-VAL.                        00053600
053700     MOVE CV-CODE-NAME  TO OPER-NAME.                             00053700
053800                                                                  00053800
053900     IF HLD-FRST-BYTE = '*'                                       00053900
054000        SET NT-GOOD-NAME TO TRUE                                  00054000
054100     ELSE                                                         00054100
054200        SET GOOD-NAME    TO TRUE.                                 00054200
054300                                                                  00054300
054400                                                                  00054400
054500 0090-EXIT.                                                       00054500
054600     EXIT.                                                        00054600
054700/                                                                 00054700
054800**************************************************************    00054800
054900*   READ THE QUALITY CONTROL FILE                            *    00054900
055000**************************************************************    00055000
055100 0200-READ-QCF-FILE.                                              00055100
055200                                                                  00055200
055300     READ  QCF-FILE                                               00055300
055400           AT END  MOVE 'END'  TO END-OF-FILE-QCF                 00055400
055500                   GO TO 0200-EXIT.                               00055500
055600                                                                  00055600
055700 0200-EXIT.                                                       00055700
055800     EXIT.                                                        00055800
055900/                                                                 00055900
056000 0250-PROCESS.                                                    00056000
056100                                                                  00056100
056200     IF NT-GOOD-NAME                                              00056200
056300        PERFORM 0090-READ-NAME-FILE      THRU 0090-EXIT           00056300
056400        GO TO 0250-EXIT.                                          00056400
056500                                                                  00056500
056600     IF QCF-OPERATOR-ID  <  WS-CD-CODE                            00056600
056700        MOVE QCF-OPERATOR-ID  TO LAST-OPERATOR                    00056700
056800        MOVE RPT-OPER-LINE    TO RPT-OPER-NAME                    00056800
056900        MOVE LAST-OPERATOR    TO RPT-OPERATOR-ID                  00056900
057000        PERFORM 0400-PROCESS  THRU 0400-EXIT                      00057000
057100                UNTIL  QCF-OPERATOR-ID  >  WS-CD-CODE             00057100
057200                OR     QCF-OPERATOR-ID  =  WS-CD-CODE             00057200
057300                OR     QCF-OPERATOR-ID  NOT = LAST-OPERATOR       00057300
057400                OR     END-OF-QCF-FILE                            00057400
057500     ELSE                                                         00057500
057600     IF QCF-OPERATOR-ID  =  WS-CD-CODE                            00057600
057700        MOVE QCF-OPERATOR-ID  TO LAST-OPERATOR                    00057700
057800        MOVE CV-CODE-NAME     TO RPT-OPER-NAME                    00057800
057900        MOVE LAST-OPERATOR    TO RPT-OPERATOR-ID                  00057900
058000        PERFORM 0400-PROCESS  THRU 0400-EXIT                      00058000
058100                UNTIL  QCF-OPERATOR-ID  >  WS-CD-CODE             00058100
058200                OR     END-OF-QCF-FILE                            00058200
058300     ELSE                                                         00058300
058400     IF QCF-OPERATOR-ID  >  WS-CD-CODE                            00058400
058500        PERFORM 0410-PROCESS  THRU 0410-EXIT                      00058500
058600                UNTIL  WS-CD-CODE >  QCF-OPERATOR-ID              00058600
058700                OR     WS-CD-CODE =  QCF-OPERATOR-ID              00058700
058800                OR     END-OF-QCF-FILE.                           00058800
058900 0250-EXIT.                                                       00058900
059000     EXIT.                                                        00059000
059100/                                                                 00059100
059200 0260-PROCESS-QLTY-CNTRL-FILE.                                    00059200
059300                                                                  00059300
059400     PERFORM 0300-FUNCTION-FIELD  THRU 0300-EXIT.                 00059400
059500     PERFORM 0310-TEST-TABLE      THRU 0310-EXIT.                 00059500
059600     PERFORM 0320-ADD-GROUP       THRU 0320-EXIT.                 00059600
059700     PERFORM 0200-READ-QCF-FILE THRU 0200-EXIT.                   00059700
059800                                                                  00059800
059900 0260-EXIT.                                                       00059900
060000     EXIT.                                                        00060000
060100/                                                                 00060100
060200 0261-TSGVSAM-POINT.                                              00060200
060300                                                                  00060300
060400     MOVE 'BENANA'    TO CV-RECORD-PREFIX                         00060400
060500                         LAST-PREFIX.                             00060500
060600     MOVE 1           TO CV-ELEMENT-NBR.                          00060600
060700     MOVE LOW-VALUES  TO CV-CODE-VALUE.                           00060700
060800     MOVE ZEROS       TO CV-CODE-DESC-SEQ.                        00060800
060900     MOVE 'P'         TO REQUEST-TYPE.                            00060900
061000     MOVE 27          TO SET-RECORD-LENGTH.                       00061000
061100     MOVE 3           TO SET-VALUE.                               00061100
061200                                                                  00061200
061300     CALL 'TSGVSAM1' USING PARM-ONE                               00061300
061400                           PARM-CV-NAME.                          00061400
061500                                                                  00061500
061600     IF  REQUEST-TYPE NOT = 'P'                                   00061600
061700         DISPLAY '*** INVALID START OF INPUT CODE FILE ***'       00061700
061800         MOVE SET-FEEDBACK-CODE TO ABEND-CODE                     00061800
061900                                   DISPLAY-FEEDBACK               00061900
062000         DISPLAY ' FEEDBACK CODE = ' DISPLAY-FEEDBACK             00062000
062100         GO TO 9999-ABEND-ROUTINE.                                00062100
062200                                                                  00062200
062300 0261-EXIT.                                                       00062300
062400      EXIT.                                                       00062400
062500/                                                                 00062500
062600 0300-FUNCTION-FIELD.                                             00062600
062700                                                                  00062700
062800     MOVE QCF-FUNC-FIELD     TO  WS-QCF-FUNC-FIELD.               00062800
062900                                                                  00062900
063000     IF WS-QCF-ID-1  =  'TT'                                      00063000
063100        IF WS-QCF-ID-2  =  '#ABM' OR '#ACL' OR '#ADL' OR '#AOL'   00063100
063200           ADD 1  TO WS-RPT-ACCUMS                                00063200
063300        ELSE                                                      00063300
063400        IF WS-QCF-ID-2  =  '#GCC'                                 00063400
063500           ADD 1  TO WS-RPT-GCCP                                  00063500
063600        ELSE                                                      00063600
063700           ADD 1  TO WS-RPT-TABLRS                                00063700
063800     ELSE                                                         00063800
063900     IF WS-QCF-ID-1  =  'BB'                                      00063900
064000        ADD 1   TO WS-RPT-BEN-PRV                                 00064000
064100     ELSE                                                         00064100
064200     IF WS-QCF-FUNC  =  'CNTK' OR 'CNTD'                          00064200
064300        ADD 1  TO WS-RPT-MAPPING                                  00064300
064400     ELSE                                                         00064400
064500     IF WS-QCF-FUNC  =  'CNTC' OR 'CNTT'                          00064500
064600        ADD 1  TO WS-RPT-FLD-MNT                                  00064600
064700     ELSE                                                         00064700
064800     IF WS-QCF-FUNC  =  'GPSK' OR 'GPSD'                          00064800
064900        ADD 1  TO WS-RPT-MAPPING                                  00064900
065000     ELSE                                                         00065000
065100     IF WS-QCF-FUNC  =  'GPSC' OR 'GPST'                          00065100
065200        ADD 1  TO WS-RPT-FLD-MNT.                                 00065200
065300                                                                  00065300
065400 0300-EXIT.                                                       00065400
065500     EXIT.                                                        00065500
065600/                                                                 00065600
065700 0310-TEST-TABLE.                                                 00065700
065800                                                                  00065800
065900*--- SEE IF ALREADY IN TABLE                                      00065900
066000                                                                  00066000
066100     MOVE 'N'   TO WS-TABLE-SW.                                   00066100
066200     MOVE QCF-SCND-PRT-GROUP-NO   TO  WS-TEST-GRP-NO.             00066200
066300                                                                  00066300
066400     PERFORM VARYING WS-SUB-1 FROM 1 BY 1                         00066400
066500        UNTIL WS-SUB-1 = 200 OR FOUND-SW                          00066500
066600          IF WS-TEST-GRP-NO = WS-GRP-NO (WS-SUB-1)                00066600
066700             MOVE 'Y'   TO WS-TABLE-SW                            00066700
066800             MOVE 200   TO WS-SUB-1                               00066800
066900             GO TO 0310-EXIT                                      00066900
067000          ELSE                                                    00067000
067100           IF WS-GRP-NO (WS-SUB-1) = SPACE OR LOW-VALUES          00067100
067200              MOVE 200 TO WS-SUB-1                                00067200
067300              GO TO 0310-EXIT                                     00067300
067400           END-IF                                                 00067400
067500          END-IF                                                  00067500
067600     END-PERFORM.                                                 00067600
067700                                                                  00067700
067800 0310-EXIT.                                                       00067800
067900     EXIT.                                                        00067900
068000/                                                                 00068000
068100 0320-ADD-GROUP.                                                  00068100
068200                                                                  00068200
068300*--- ADD TO TABLE IF NOT CODED                                    00068300
068400                                                                  00068400
068500     IF NT-FOUND-SW                                               00068500
068600        PERFORM VARYING WS-SUB-1 FROM 1 BY 1                      00068600
068700           UNTIL WS-SUB-1 = 200                                   00068700
068800             IF WS-GRP-NO (WS-SUB-1) = SPACE OR LOW-VALUES        00068800
068900                MOVE WS-TEST-GRP-NO                               00068900
069000                TO   WS-GRP-NO (WS-SUB-1)                         00069000
069100                ADD 1   TO WS-TBL-CNT                             00069100
069200                MOVE 200 TO WS-SUB-1                              00069200
069300                GO TO 0320-EXIT                                   00069300
069400             END-IF                                               00069400
069500        END-PERFORM.                                              00069500
069600                                                                  00069600
069700 0320-EXIT.                                                       00069700
069800     EXIT.                                                        00069800
069900/                                                                 00069900
070000 0400-PROCESS.                                                    00070000
070100                                                                  00070100
070200     MOVE 0 TO  RPT-FLD-MNT RPT-BEN-PRV RPT-TABLRS RPT-ACCUMS     00070200
070300                RPT-MAPPING RPT-GCCP WS-RPT-FLD-MNT WS-RPT-GCCP   00070300
070400                WS-RPT-TABLRS WS-RPT-ACCUMS WS-RPT-BEN-PRV        00070400
070500                WS-RPT-MAPPING WS-TBL-CNT.                        00070500
070600                                                                  00070600
070700     PERFORM 0260-PROCESS-QLTY-CNTRL-FILE THRU 0260-EXIT          00070700
070800             UNTIL QCF-OPERATOR-ID  NOT = LAST-OPERATOR           00070800
070900             OR    END-OF-QCF-FILE.                               00070900
071000                                                                  00071000
071100     PERFORM 0500-WRITE-REPORT-LINE THRU 0500-EXIT.               00071100
071200     PERFORM 0800-WRITE-GROUPS      THRU 0800-EXIT.               00071200
071300     PERFORM 0900-CLEAN-TABLE       THRU 0900-EXIT.               00071300
071400                                                                  00071400
071500 0400-EXIT.                                                       00071500
071600     EXIT.                                                        00071600
071700/                                                                 00071700
071800 0410-PROCESS.                                                    00071800
071900                                                                  00071900
072000     IF GOOD-NAME                                                 00072000
072100        NEXT SENTENCE                                             00072100
072200     ELSE                                                         00072200
072300     IF NT-GOOD-NAME                                              00072300
072400        PERFORM 0090-READ-NAME-FILE    THRU 0090-EXIT             00072400
072500        GO TO 0410-EXIT.                                          00072500
072600                                                                  00072600
072700     PERFORM 0090-READ-NAME-FILE    THRU 0090-EXIT.               00072700
072800                                                                  00072800
072900     IF WS-CD-CODE >  QCF-OPERATOR-ID                             00072900
073000        GO TO 0410-EXIT.                                          00073000
073100                                                                  00073100
073200     IF NT-GOOD-NAME                                              00073200
073300        IF WS-CD-CODE =  QCF-OPERATOR-ID                          00073300
073400           PERFORM 0090-READ-NAME-FILE THRU 0090-EXIT             00073400
073500           PERFORM 0200-READ-QCF-FILE THRU 0200-EXIT              00073500
073600                   UNTIL QCF-OPERATOR-ID > WS-CD-CODE             00073600
073700           GO TO 0410-EXIT                                        00073700
073800        ELSE                                                      00073800
073900           PERFORM 0090-READ-NAME-FILE THRU 0090-EXIT             00073900
074000           GO TO 0410-EXIT.                                       00074000
074100                                                                  00074100
074200     IF GOOD-NAME                                                 00074200
074300        IF WS-CD-CODE =  QCF-OPERATOR-ID                          00074300
074400           GO TO 0410-EXIT                                        00074400
074500        ELSE                                                      00074500
074600           NEXT SENTENCE.                                         00074600
074700                                                                  00074700
074800     MOVE 0 TO  RPT-FLD-MNT RPT-BEN-PRV RPT-TABLRS RPT-ACCUMS     00074800
074900                RPT-MAPPING RPT-GCCP WS-RPT-FLD-MNT WS-RPT-GCCP   00074900
075000                WS-RPT-TABLRS WS-RPT-ACCUMS WS-RPT-BEN-PRV        00075000
075100                WS-RPT-MAPPING WS-TBL-CNT.                        00075100
075200                                                                  00075200
075300     IF NT-GOOD-NAME                                              00075300
075400        GO TO 0410-EXIT.                                          00075400
075500                                                                  00075500
075600     IF QCF-OPERATOR-ID  >  WS-CD-CODE                            00075600
075700        MOVE CV-CODE-NAME   TO RPT-OPER-NAME                      00075700
075800        MOVE LAST-OPERATOR  TO RPT-OPERATOR-ID                    00075800
075900        PERFORM 0500-WRITE-REPORT-LINE THRU 0500-EXIT.            00075900
076000                                                                  00076000
076100 0410-EXIT.                                                       00076100
076200     EXIT.                                                        00076200
076300/                                                                 00076300
076400 0420-FINISH.                                                     00076400
076500                                                                  00076500
076600     IF GOOD-NAME                                                 00076600
076700        NEXT SENTENCE                                             00076700
076800     ELSE                                                         00076800
076900     IF NT-GOOD-NAME                                              00076900
077000        PERFORM 0090-READ-NAME-FILE    THRU 0090-EXIT             00077000
077100        GO TO 0420-EXIT.                                          00077100
077200                                                                  00077200
077300     MOVE 0 TO  RPT-FLD-MNT RPT-BEN-PRV RPT-TABLRS RPT-ACCUMS     00077300
077400                RPT-MAPPING RPT-GCCP WS-RPT-FLD-MNT WS-RPT-GCCP   00077400
077500                WS-RPT-TABLRS WS-RPT-ACCUMS WS-RPT-BEN-PRV        00077500
077600                WS-RPT-MAPPING WS-TBL-CNT.                        00077600
077700                                                                  00077700
077800     MOVE CV-CODE-NAME   TO RPT-OPER-NAME.                        00077800
077900     MOVE WS-CD-CODE     TO RPT-OPERATOR-ID.                      00077900
078000     PERFORM 0500-WRITE-REPORT-LINE THRU 0500-EXIT.               00078000
078100                                                                  00078100
078200     PERFORM 0090-READ-NAME-FILE    THRU 0090-EXIT.               00078200
078300                                                                  00078300
078400 0420-EXIT.                                                       00078400
078500     EXIT.                                                        00078500
078600/                                                                 00078600
078700**************************************************************    00078700
078800*   WRITE THE DETAIL LINE OF INFORMATION TO THE REPORT       *    00078800
078900**************************************************************    00078900
079000 0500-WRITE-REPORT-LINE.                                          00079000
079100                                                                  00079100
079200     MOVE WS-RPT-FLD-MNT    TO RPT-FLD-MNT.                       00079200
079300     MOVE WS-RPT-BEN-PRV    TO RPT-BEN-PRV.                       00079300
079400     MOVE WS-RPT-TABLRS     TO RPT-TABLRS.                        00079400
079500     MOVE WS-RPT-ACCUMS     TO RPT-ACCUMS.                        00079500
079600     MOVE WS-RPT-MAPPING    TO RPT-MAPPING.                       00079600
079700     MOVE WS-RPT-GCCP       TO RPT-GCCP.                          00079700
079800                                                                  00079800
079900     PERFORM 0600-WRITE-HEADER THRU 0600-EXIT.                    00079900
080000     WRITE RPT-RECORD FROM RPT-DTL-L-1                            00080000
080100        AFTER ADVANCING 2 LINES.                                  00080100
080200     WRITE RPT-RECORD FROM RPT-DTL-L-2.                           00080200
080300     WRITE RPT-RECORD FROM RPT-DTL-L-3.                           00080300
080400     WRITE RPT-RECORD FROM RPT-DTL-L-4.                           00080400
080500     WRITE RPT-RECORD FROM RPT-DTL-L-5.                           00080500
080600     WRITE RPT-RECORD FROM RPT-DTL-L-6.                           00080600
080700     ADD 2   TO RPT-LINE-COUNT.                                   00080700
080800                                                                  00080800
080900 0500-EXIT.                                                       00080900
081000     EXIT.                                                        00081000
081100/                                                                 00081100
081200**************************************************************    00081200
081300*   WRITE THE PAGE HEADER AREA FOR THE REPORT                *    00081300
081400**************************************************************    00081400
081500 0600-WRITE-HEADER.                                               00081500
081600                                                                  00081600
081700     COMPUTE RPT-PAGE-COUNT = RPT-PAGE-COUNT + 1.                 00081700
081800     MOVE RPT-PAGE-COUNT  TO RPT-PAGE-CNT.                        00081800
081900     MOVE '2915'          TO RPT-ID-FIELD.                        00081900
082000     MOVE ZEROS           TO RPT-LINE-COUNT.                      00082000
082100                                                                  00082100
082200     WRITE RPT-RECORD FROM RPT-MAIN-HDR-L1                        00082200
082300        AFTER ADVANCING PAGE.                                     00082300
082400                                                                  00082400
082500     WRITE RPT-RECORD FROM RPT-MAIN-HDR-L2                        00082500
082600        AFTER ADVANCING 1 LINE.                                   00082600
082700                                                                  00082700
082800     WRITE RPT-RECORD FROM RPT-OPER-HDR-L1                        00082800
082900        AFTER ADVANCING 2 LINES.                                  00082900
083000                                                                  00083000
083100     WRITE RPT-RECORD FROM RPT-OPER-HDR-L2                        00083100
083200        AFTER ADVANCING 1 LINE.                                   00083200
083300                                                                  00083300
083400     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L1                      00083400
083500        AFTER ADVANCING 2 LINES.                                  00083500
083600                                                                  00083600
083700     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L2                      00083700
083800        AFTER ADVANCING 1 LINES.                                  00083800
083900                                                                  00083900
084000     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L3                      00084000
084100        AFTER ADVANCING 0 LINES.                                  00084100
084200                                                                  00084200
084300     ADD 9   TO RPT-LINE-COUNT.                                   00084300
084400                                                                  00084400
084500 0600-EXIT.                                                       00084500
084600     EXIT.                                                        00084600
084700/                                                                 00084700
084800**************************************************************    00084800
084900*   WRITE THE PAGE HEADER AREA FOR THE REPORT                *    00084900
085000**************************************************************    00085000
085100 0700-PROCESS-NAME-ONLY.                                          00085100
085200                                                                  00085200
085300     MOVE CV-CODE-NAME    TO RPT-OPER-NAME.                       00085300
085400     MOVE CV-CODE-VALUE   TO RPT-OPERATOR-ID                      00085400
085500                             LAST-CODE-VALUE.                     00085500
085600     MOVE ZEROS TO RPT-LINE-COUNT.                                00085600
085700     COMPUTE RPT-PAGE-COUNT = RPT-PAGE-COUNT + 1.                 00085700
085800     MOVE RPT-PAGE-COUNT  TO RPT-PAGE-CNT.                        00085800
085900                                                                  00085900
086000     WRITE RPT-RECORD FROM RPT-MAIN-HDR-L1                        00086000
086100        AFTER ADVANCING PAGE.                                     00086100
086200                                                                  00086200
086300     WRITE RPT-RECORD FROM RPT-MAIN-HDR-L2                        00086300
086400        AFTER ADVANCING 1 LINE.                                   00086400
086500                                                                  00086500
086600     WRITE RPT-RECORD FROM RPT-OPER-HDR-L1                        00086600
086700        AFTER ADVANCING 2 LINES.                                  00086700
086800                                                                  00086800
086900     WRITE RPT-RECORD FROM RPT-OPER-HDR-L2                        00086900
087000        AFTER ADVANCING 1 LINE.                                   00087000
087100                                                                  00087100
087200     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L1                      00087200
087300        AFTER ADVANCING 2 LINES.                                  00087300
087400                                                                  00087400
087500     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L2                      00087500
087600        AFTER ADVANCING 1 LINES.                                  00087600
087700                                                                  00087700
087800     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L3                      00087800
087900        AFTER ADVANCING 0 LINES.                                  00087900
088000                                                                  00088000
088100     WRITE RPT-RECORD FROM RPT-OPER-LINE                          00088100
088200        AFTER ADVANCING 2 LINES.                                  00088200
088300                                                                  00088300
088400     ADD 10  TO RPT-LINE-COUNT.                                   00088400
088500                                                                  00088500
088600 0700-EXIT.                                                       00088600
088700     EXIT.                                                        00088700
088800/                                                                 00088800
088900 0800-WRITE-GROUPS.                                               00088900
089000                                                                  00089000
089100     WRITE RPT-RECORD FROM RPT-DTL-L-7                            00089100
089200        AFTER ADVANCING 2 LINES.                                  00089200
089300                                                                  00089300
089400     WRITE RPT-RECORD FROM RPT-DTL-L-9                            00089400
089500        AFTER ADVANCING 2 LINES.                                  00089500
089600                                                                  00089600
089700     PERFORM 0810-MASS-MOVE  THRU 0810-EXIT                       00089700
089800             VARYING WS-SUB-1 FROM 1 BY 1                         00089800
089900                UNTIL WS-SUB-1 >  WS-TBL-CNT.                     00089900
090000                                                                  00090000
090100 0800-EXIT.                                                       00090100
090200     EXIT.                                                        00090200
090300/                                                                 00090300
090400 0810-MASS-MOVE.                                                  00090400
090500                                                                  00090500
090600     PERFORM 0820-MOVE-GRP   THRU 0820-EXIT                       00090600
090700             VARYING WS-SUB-2 FROM 1 BY 1                         00090700
090800                UNTIL WS-SUB-2 > 16.                              00090800
090900                                                                  00090900
091000     WRITE RPT-RECORD FROM RPT-DTL-L-8                            00091000
091100        AFTER ADVANCING 1 LINES.                                  00091100
091200                                                                  00091200
091300     MOVE SPACES   TO RPT-DTL-L-8.                                00091300
091400                                                                  00091400
091500     COMPUTE WS-SUB-1 = WS-SUB-1 - 1.                             00091500
091600                                                                  00091600
091700 0810-EXIT.                                                       00091700
091800     EXIT.                                                        00091800
091900/                                                                 00091900
092000 0820-MOVE-GRP.                                                   00092000
092100                                                                  00092100
092200     IF WS-GRP-NO ( WS-SUB-1 ) = SPACE OR LOW-VALUE               00092200
092300        COMPUTE WS-SUB-1 = WS-SUB-1 + 1                           00092300
092400        GO TO 0820-EXIT.                                          00092400
092500                                                                  00092500
092600     MOVE SPACES   TO WS-TBL-GRP-NO ( WS-SUB-2 ).                 00092600
092700     MOVE WS-GRP-NO   ( WS-SUB-1 )                                00092700
092800     TO   WS-TBL-GRP  ( WS-SUB-2 ).                               00092800
092900     ADD 1   TO  WS-SUB-1.                                        00092900
093000                                                                  00093000
093100 0820-EXIT.                                                       00093100
093200     EXIT.                                                        00093200
093300/                                                                 00093300
093400 0900-CLEAN-TABLE.                                                00093400
093500                                                                  00093500
093600     PERFORM 0910-MOVE-SPACES THRU 0910-EXIT                      00093600
093700             VARYING WS-SUB-1 FROM 1 BY 1                         00093700
093800                UNTIL WS-SUB-1 > 200.                             00093800
093900                                                                  00093900
094000 0900-EXIT.                                                       00094000
094100     EXIT.                                                        00094100
094200/                                                                 00094200
094300 0910-MOVE-SPACES.                                                00094300
094400                                                                  00094400
094500     MOVE SPACES   TO WS-GROUP-TBL ( WS-SUB-1 ).                  00094500
094600                                                                  00094600
094700 0910-EXIT.                                                       00094700
094800     EXIT.                                                        00094800
094900/                                                                 00094900
095000 9999-ABEND-ROUTINE.                                              00095000
095100                                                                  00095100
095200     CALL 'TSGEND' USING ABEND-CODE.                              00095200
095300                                                                  00095300
095400 9999-EXIT.                                                       00095400
095500     EXIT.                                                        00095500
