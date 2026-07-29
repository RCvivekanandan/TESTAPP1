000100 IDENTIFICATION DIVISION.                                         00000100
000200                                                                  00000200
000300 PROGRAM-ID. GC03510.                                             00000300
000400 AUTHOR. MELANIE MOSTACCIO.                                       00000400
000500                                                                  00000500
000600 INSTALLATION. HCSC-HCMS.                                         00000600
000700 DATE-WRITTEN.                                                    00000700
000800 DATE-COMPILED.                                                   00000800
000900******************************************************************00000900
001000*                                                                *00001000
001100*    THIS PROGRAM WILL WRITE A REPORT FOR QUALITY CONTROL        *00001100
001200*    USING A FILE CONTAINING RANDOMLY PICKED RECORDS FROM THE    *00001200
001300*    QUALITY CONTROL FILE.                                       *00001300
001400*                                                                *00001400
001500*    INPUT FILES:                                                *00001500
001600*       QCF-FILE - QUALITY CONTROL EXTRACT FILE                  *00001600
001700*       RND-FILE - RANDOMIZED VERSION OF THE QCF-FILE            *00001700
001800*       NME-FILE - CODES VSAM FILE WITH THE OPERATOR NAMES       *00001800
001900*                                                                *00001900
002000*    OUTPUT FILES:                                               *00002000
002100*       NONE                                                     *00002100
002200*                                                                *00002200
002300*    OUTPUT REPORTS:                                             *00002300
002400*       RPT-FILE - THE REPORT THAT WILL BE CREATED FROM THE      *00002400
002500*                  RANDOMIZED FILE USING THE ETRACT FILE DATA    *00002500
002600*                                                                *00002600
002700******************************************************************00002700
002800*                                                                *00002800
002900*      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *00002900
003000*      *-*         U P D A T E   H I S T O R Y         *-*       *00003000
003100*      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *00003100
003200*                                                                *00003200
003300* CHG #    DATE    BY              DESCRIPTION                   *00003300
003400* _____  ________  ___  ___________________________________      *00003400
003500* D15446 11/18/98  FRY   SUPPORT #CRS TABULAR                    *00003500
003600* D15182                 SUPPORT #ACP TABULAR                    *00003600
003700*         FROM:     WT-02-DESCRIPTION-VALUES    OCCURS 047 TIMES *00003700
003800*           TO:     WT-02-DESCRIPTION-VALUES    OCCURS 048 TIMES *00003800
003900*         FROM:     WT-02-DESCRIPTION-VALUES    OCCURS 019 TIMES *00003900
004000*           TO:     WT-02-DESCRIPTION-VALUES    OCCURS 021 TIMES *00004000
004100*                                                                *00004100
004200* D353   10/09/00  JP  SUPPORT NEW CONT/GS ARCHIVE COPAY FIELDS  *00004200
004300*                      CHANGED PERFORM VARYING UNTIL PARAMETERS: *00004300
004400*         FROM:     GCT-A-INDEX      >   89 TIMES                *00004400
004500*           TO:     GCT-A-INDEX      >   93 TIMES                *00004500
004600*         FROM:     GCG-A-INDEX      >  224 TIMES                *00004600
004700*           TO:     GCG-A-INDEX      >  249 TIMES                *00004700
004800*                                                                *00004800
004900* D356   03/23/01  JP  SUPPORT 3 NEW GRPSPEC FIELDS (ITS NONPAR) *00004900
005000*                      CHANGED PERFORM VARYING UNTIL PARAMETERS  *00005000
005100*                      TO MATCH # OF OCCURS IN GCARCHGS          *00005100
005200*                      FROM:  GCG-A-INDEX      >  249 TIMES      *00005200
005300*                        TO:  GCG-A-INDEX      >  252 TIMES      *00005300
005400*                                                                *00005400
005500* D365A  07/22/02  JP  SUPPORT NEW GRPSPEC FIELD:                *00005500
005600*                              GCG-ACCM-REL-IND*                 *00005600
005700*                      CHANGED PERFORM VARYING UNTIL PARAMETERS  *00005700
005800*                      TO MATCH # OF OCCURS IN GCARCHGS          *00005800
005900*                      FROM:  GCG-A-INDEX      >  252 TIMES      *00005900
006000*                        TO:  GCG-A-INDEX      >  253 TIMES      *00006000
006100*                                                                *00006100
006200* D374   04/01/03  GF  SUPPORT NEW GRPSPEC FIELD:                *00006200
006300*                  GCG-CONS-DRVN-IND                             *00006300
006400*                  GCG-CONS-DRVN-PENALTY-DATE                    *00006400
006500*                      CHANGED PERFORM VARYING UNTIL PARAMETERS  *00006500
006600*                      TO MATCH # OF OCCURS IN GCARCHGS          *00006600
006700*                      FROM:  GCG-A-INDEX      >  253 TIMES      *00006700
006800*                        TO:  GCG-A-INDEX      >  255 TIMES      *00006800
006900*                                                                *00006900
007000* DM9400 05/30/07  LR  MODIFIED TO SUPPORT #GMFH TABULAR:        *00007000
007100*         IN GROUP SPECIFIC ENTRY CHANGED                        *00007100
007200*         FROM:     WT-02-DESCRIPTION-VALUES    OCCURS 048 TIMES *00007200
007300*           TO:     WT-02-DESCRIPTION-VALUES    OCCURS 057 TIMES *00007300
007400*         IN CONTRACT ENTRY CHANGED                              *00007400
007500*         FROM:     WT-02-DESCRIPTION-VALUES    OCCURS 021 TIMES *00007500
007600*           TO:     WT-02-DESCRIPTION-VALUES    OCCURS 022 TIMES *00007600
007700*                                                                *00007700
007700* P21681 10/23/17 SRI RECOMPILE FOR CB GCARCHGS                  *00007710
      *                                                                *00007720
      *        06/26/22 TEB RECOMPILE FOR COPYBOOK CHG GCARCHCT        *00007730
      *                                                                *00007731
      *            P00027450 - NSA ASO OPT IN-OUT FIELD                *00007740
      *            08/03/22  TB   RECOMPILE FOR COPYBOOK GCYCNVTC CHG  *00007750
      *                                                                *00007760
      *            P00027450 - CORRECT NSA ASO OPT IN-OUT FIELD        *00007770
      *            11/30/22  TB   RECOMPILE FOR COPYBOOK GCYCNVTC CHG  *00007780
      *                                                                *00007790
      *            P00029516 - BALANCE BILLING PROTECTION OPTION FIELD *00007791
      *            02/22/24  TB   RECOMPILE FOR COPYBOOK GCYCNVTC CHG  *00007792
      *                                                                *00007793
      *            P00029711 - EXTERNAL FINANCE INDICATOR FIELD       * 00007794
      *            03/12/24  SI   RECOMPILE FOR COPYBOOK GCYCNVTC CHG * 00007795
      *                                                               * 00007796
      *            P00028776 - BALANCE BILLING PROTECTION OPTION FIELD* 00007797
      *            07/22/24  BB   RECOMPILE FOR COPYBOOK GCARCHCT CHG * 00007798
      *                                                                 00007799
      *            P00030333 - MULTI TIER INDICATOR FIELD             * 00007800
      *            07/14/25  CB   RECOMPILE FOR COPYBOOK GCYCNVTC CHG * 00007801
      *                                                               * 00007802
PG1125*            P000XXXXX - RX COUPON PROGRAM INDICATOR             *00007803
PG1125*            11/17/25  PG   RECOMPILE FOR COPYBOOK GCYCNVTC CHG  *00007804
PG1125*                                                                *00007805
TM0526*BBDA-66049 04/10/26 TM    MODIFIED TO SUPPORT #GHPA TABULAR:    *00007806
TM0526*         IN GROUP SPECIFIC ENTRY CHANGED                        *00007807
TM0526*         FROM:     WT-02-DESCRIPTION-VALUES    OCCURS 057 TIMES *00007808
TM0526*           TO:     WT-02-DESCRIPTION-VALUES    OCCURS 058 TIMES *00007809
PG0615* P30337     06/15/26    PG   RECOMPILE -  GCARCHGS.             *00007810
007800******************************************************************00007820
007900/                                                                 00007900
008000 ENVIRONMENT DIVISION.                                            00008000
008100 CONFIGURATION SECTION.                                           00008100
008200 SOURCE-COMPUTER. IBM-370.                                        00008200
008300 OBJECT-COMPUTER. IBM-370.                                        00008300
008400                                                                  00008400
008500 INPUT-OUTPUT SECTION.                                            00008500
008600 FILE-CONTROL.                                                    00008600
008700                                                                  00008700
008800     SELECT QCF-FILE   ASSIGN TO UT-S-GC03510A.                   00008800
008900     SELECT RND-FILE   ASSIGN TO UT-S-GC03510B.                   00008900
009000     SELECT RPT-FILE   ASSIGN TO UT-S-GC03510C.                   00009000
009100/                                                                 00009100
009200 DATA DIVISION.                                                   00009200
009300 FILE SECTION.                                                    00009300
009400                                                                  00009400
009500 FD  QCF-FILE                                                     00009500
009600     LABEL RECORDS ARE STANDARD                                   00009600
009700     RECORDING MODE IS F                                          00009700
009800     BLOCK CONTAINS 0 RECORDS.                                    00009800
009900                                                                  00009900
010000 01  QCF-RECORD.                                                  00010000
010100     COPY GCQCF.                                                  00010100
010200                                                                  00010200
010300 FD  RND-FILE                                                     00010300
010400     LABEL RECORDS ARE STANDARD                                   00010400
010500     RECORDING MODE IS F                                          00010500
010600     BLOCK CONTAINS 0 RECORDS.                                    00010600
010700                                                                  00010700
010800 01  RND-RECORD.                                                  00010800
010900     05  RND-OPERATOR-ID  PIC X(08).                              00010900
011000     05  RND-PLAN-CODE    PIC X(03).                              00011000
011100     05  RND-GROUP-NO     PIC X(09).                              00011100
011200                                                                  00011200
011300 FD  RPT-FILE                                                     00011300
011400     LABEL RECORDS ARE STANDARD                                   00011400
011500     RECORDING MODE IS F                                          00011500
011600     BLOCK CONTAINS 0 RECORDS.                                    00011600
011700                                                                  00011700
011800 01  RPT-RECORD           PIC X(132).                             00011800
011900/                                                                 00011900
012000 WORKING-STORAGE SECTION.                                         00012000
012100 01  FILLER                    PIC X(24)   VALUE                  00012100
012200     'GC03510 WORKING-STORAGE'.                                   00012200
012300                                                                  00012300
012400 01  ABEND-CODE                PIC 9(4)    COMP.                  00012400
012500 01  WS-OPER-ID                PIC X(05) VALUE SPACES.            00012500
012600 01  RPT-IND    VALUE '9999'   PIC X(4).                          00012600
012700                                                                  00012700
012800 COPY HSCDATES.                                                   00012800
012900 01  DATE-TIME-WORK.                                              00012900
013000     05  TIME-AREA.                                               00013000
013100         10  TIME-HH           PIC XX.                            00013100
013200         10  TIME-MM           PIC XX.                            00013200
013300         10  TIME-SS           PIC XX.                            00013300
013400     05  WS-DATE-AREA.                                            00013400
013500         10  WS-MDY.                                              00013500
013600             15  WS-M          PIC 99    VALUE ZERO.              00013600
013700             15  WS-D          PIC 99    VALUE ZERO.              00013700
013800             15  WS-Y          PIC 99    VALUE ZERO.              00013800
013900         10  WS-YYDDD          PIC 9(5)  VALUE ZERO.              00013900
014000         10  FILLER REDEFINES WS-YYDDD.                           00014000
014100             15  WS-YY         PIC 99.                            00014100
014200             15  WS-DDD        PIC 999.                           00014200
014300         10  HOLD-CURR-DATE-JUL  PIC S9(5) COMP-3 VALUE ZEROS.    00014300
014400                                                                  00014400
014500 01  CEN-DATE.                                                    00014500
014600     05  CEN-DT.                                                  00014600
014700         10  CEN-CC            PIC 99.                            00014700
014800         10  CEN-YY            PIC 99.                            00014800
014900                                                                  00014900
015000 01  HLD-DATE.                                                    00015000
015100     05  HLD-DATE-AREA         PIC 9(07).                         00015100
015200     05  HLD-DATE-WORK REDEFINES HLD-DATE-AREA.                   00015200
015300         10  HLD-CEN           PIC 99.                            00015300
015400         10  HLD-JUL.                                             00015400
015500             15  HLD-YY        PIC 99.                            00015500
015600             15  HLD-DAYS      PIC 999.                           00015600
015700                                                                  00015700
015800*--PHF                                                            00015800
015900 01  HLD-DATE-1.                                                  00015900
016000     05  HLD-DATE-AREA-1       PIC 9(10).                         00016000
016100     05  HLD-DATE-WORK-1 REDEFINES HLD-DATE-AREA-1.               00016100
016200         10  FILLER            PIC 9(05).                         00016200
016300         10  HLD-JUL-1.                                           00016300
016400             15  HLD-YY-1      PIC 99.                            00016400
016500             15  HLD-DAYS-1    PIC 999.                           00016500
016600                                                                  00016600
016700 01  WS-TERM-DATE-FIELD.                                          00016700
016800     05  FILLER                PIC X(02).                         00016800
016900     05  WS-TERM-DATE.                                            00016900
017000         10  WS-TERM-DATE-MM   PIC X(02).                         00017000
017100         10  SLASH-7           PIC X(01) VALUE '/'.               00017100
017200         10  WS-TERM-DATE-DD   PIC X(02).                         00017200
017300         10  SLASH-8           PIC X(01) VALUE '/'.               00017300
017400         10  WS-TERM-DATE-YY   PIC X(02).                         00017400
017500*--                                                               00017500
017600                                                                  00017600
017700 01  DATE-AREA.                                                   00017700
017800     05  GREG-DATE             PIC 9(6)  VALUE ZEROS.             00017800
017900     05  JULIAN-DATE           PIC 9(5)  VALUE ZEROS.             00017900
018000     05  WS-CHNG-DATE          PIC 9(5)  VALUE ZEROS.             00018000
018100**** THE ABOVE DATE FORMAT IS YYDDD ***                           00018100
018200                                                                  00018200
018300                                                                  00018300
018400*01  WS-DATE                               PIC X(08) VALUE ZEROS. 00018400
018500 01  WS-DATE-CCYYDDD.                                             00018500
018600     05  WS-DATE-CC                        PIC X(02) VALUE ZEROS. 00018600
018700     05  WS-DATE-YYDDD.                                           00018700
018800         10  FILLER                        PIC X(01) VALUE ZEROS. 00018800
018900         10  WS-DATE-YY                    PIC X(02) VALUE ZEROS. 00018900
019000         10  WS-DATE-DDD                   PIC X(03) VALUE ZEROS. 00019000
019100                                                                  00019100
019200 01  WS-DATE-YYDDD-9                       PIC 9(06) VALUE ZEROS. 00019200
019300                                                                  00019300
019400 01  WS-DATE-MMDDCCYY.                                            00019400
019500     05  WS-DATE-MM                        PIC X(02) VALUE ZEROS. 00019500
019600     05  WS-DATE-DD                        PIC X(02) VALUE ZEROS. 00019600
019700     05  WS-DATE-CCYY                      PIC X(04) VALUE ZEROS. 00019700
019800*---                                                              00019800
019900/                                                                 00019900
020000 01  END-OF-FILE-RND           PIC XXX   VALUE SPACES.            00020000
020100     88  END-OF-RND-FILE           VALUE 'END'.                   00020100
020200                                                                  00020200
020300 01  END-OF-FILE-QCF           PIC XXX   VALUE SPACES.            00020300
020400     88  END-OF-QCF-FILE           VALUE 'END'.                   00020400
020500                                                                  00020500
020600 01  NAME-SW                   PIC X     VALUE SPACES.            00020600
020700     88  GOOD-NAME                 VALUE 'Y'.                     00020700
020800     88  NT-GOOD-NAME              VALUE 'N'.                     00020800
020900                                                                  00020900
021000 01  TAB-SW                    PIC X     VALUE SPACES.            00021000
021100     88  TAB-FND                   VALUE 'Y'.                     00021100
021200     88  TAB-NT-FND                VALUE 'N'.                     00021200
021300                                                                  00021300
021400 01  FIELD-SW                  PIC X     VALUE SPACES.            00021400
021500     88  FIELD-FND                 VALUE 'Y'.                     00021500
021600     88  FIELD-NT-FND              VALUE 'N'.                     00021600
021700                                                                  00021700
021800 01  OPERATOR-SW               PIC X     VALUE SPACES.            00021800
021900     88  OPER-ON                   VALUE 'Y'.                     00021900
022000     88  OPER-OFF                  VALUE 'N'.                     00022000
022100                                                                  00022100
022200 01  GROUP-NO-SW               PIC X     VALUE SPACES.            00022200
022300     88  GROUP-ON                  VALUE 'Y'.                     00022300
022400     88  GROUP-OFF                 VALUE 'N'.                     00022400
022500                                                                  00022500
022600 01  COUNTER-AREA.                                                00022600
022700     05  WS-QCF-COUNT          PIC S9(9) VALUE ZEROS.             00022700
022800     05  RPT-LINE-COUNT        PIC S9(9) VALUE ZEROS.             00022800
022900     05  RPT-PAGE-COUNT        PIC S9(9) VALUE ZEROS.             00022900
023000     05  DISPLAY-FEEDBACK      PIC S9(04) COMP-3 VALUE +0.        00023000
023100                                                                  00023100
023200 01  HOLD-LAST-AREA.                                              00023200
023300     05  LAST-OPERATOR         PIC X(08).                         00023300
023400     05  LAST-GROUP-NO         PIC X(09).                         00023400
023500     05  LAST-CODE-VALUE       PIC X(10).                         00023500
023600     05  LAST-PREFIX           PIC X(08).                         00023600
023700     05  HLD-NT-GOOD-NM-VALUE  PIC X(08).                         00023700
023800                                                                  00023800
023900 01  WS-DATE.                                                     00023900
024000     05  WS-DATE-YR            PIC X(02).                         00024000
024100     05  WS-DATE-MO            PIC X(02).                         00024100
024200     05  WS-DATE-DY            PIC X(02).                         00024200
024300                                                                  00024300
024400 01  WS-TIME.                                                     00024400
024500     05  WS-TIME-HR            PIC X(02).                         00024500
024600     05  WS-TIME-MN            PIC X(02).                         00024600
024700     05  WS-TIME-SC            PIC X(02).                         00024700
024800     05  WS-TIME-HH            PIC X(02).                         00024800
024900                                                                  00024900
025000 01  OPER-NAME.                                                   00025000
025100     05  HLD-FRST-BYTE         PIC X(01).                         00025100
025200     05  HLD-OPER-NAME         PIC X(49).                         00025200
025300                                                                  00025300
025400 01  FUNC-FIELD.                                                  00025400
025500     05  HLD-FUNC-FIELD.                                          00025500
025600         10  HLD-TWO-BYTES     PIC X(02).                         00025600
025700         10  HLD-FUNC-AREA     PIC X(06).                         00025700
025800     05  HLD-FUNC-FLD REDEFINES                                   00025800
025900         HLD-FUNC-FIELD.                                          00025900
026000         10  HLD-FIVE-BYTES    PIC X(05).                         00026000
026100         10  HLD-LAST-THREE    PIC X(03).                         00026100
026200                                                                  00026200
026300 01  BENEFIT-PROV.                                                00026300
026400     05  HOLD-BEN-PROV.                                           00026400
026500         10  HLD-BEN-TWO-BYTES  PIC X(02).                        00026500
026600         10  HLD-BEN-AREA       PIC X(06).                        00026600
026700                                                                  00026700
026800 01  BEN-DESCRIPTION.                                             00026800
026900     05  FILLER                PIC X(10)                          00026900
027000           VALUE 'PROVISION '.                                    00027000
027100     05  BEN-PROV-OUT          PIC X(06).                         00027100
027200                                                                  00027200
027300 01  WS-MESSAGES.                                                 00027300
027400     05  NO-NM-FND-MSG         PIC X(31)                          00027400
027500           VALUE 'NO NAME FOUND FOR THIS OPERATOR'.               00027500
027600                                                                  00027600
027700 01  RPT-MAIN-HDR-L1.                                             00027700
027800     05  FILLER                PIC X     VALUE SPACES.            00027800
027900     05  FILLER                PIC X(15)                          00027900
028000           VALUE 'REPORT ID:     '.                               00028000
028100     05  RPT-ID-FIELD          PIC X(04) VALUE SPACES.            00028100
028200     05  FILLER                PIC X(29).                         00028200
028300     05  FILLER                PIC X(34)                          00028300
028400           VALUE 'BLUE CROSS/BLUE SHIELD OF ILLINOIS'.            00028400
028500     05  FILLER                PIC X(31).                         00028500
028600     05  FILLER                PIC X(10)                          00028600
028700           VALUE 'PAGE:     '.                                    00028700
028800     05  RPT-PAGE-CNT          PIC 9(04).                         00028800
028900     05  FILLER                PIC X(04).                         00028900
029000                                                                  00029000
029100 01  RPT-MAIN-HDR-L2.                                             00029100
029200     05  FILLER                PIC X   VALUE SPACES.              00029200
029300     05  FILLER                PIC X(11)                          00029300
029400          VALUE 'RUN TIME : '.                                    00029400
029500     05  RPT-RUN-TIME.                                            00029500
029600         10  RPT-RUN-HR        PIC 99.                            00029600
029700         10  FILLER            PIC X  VALUE ':'.                  00029700
029800         10  RPT-RUN-MIN       PIC 99.                            00029800
029900         10  FILLER            PIC X  VALUE ':'.                  00029900
030000         10  RPT-RUN-SEC       PIC 99.                            00030000
030100     05  FILLER                PIC X(23).                         00030100
030200     05  FILLER                PIC X(46)                          00030200
030300          VALUE 'QUALITY CONTROL CONTRACT/GROUP SPECIFIC REPORT'. 00030300
030400     05  FILLER                PIC X(25).                         00030400
030500     05  FILLER                PIC X(10)                          00030500
030600          VALUE 'RUN DATE: '.                                     00030600
030700     05  RPT-RUN-DATE.                                            00030700
030800         10  RPT-RUN-MO        PIC 99.                            00030800
030900         10  FILLER            PIC X  VALUE '/'.                  00030900
031000         10  RPT-RUN-DY        PIC 99.                            00031000
031100         10  FILLER            PIC X  VALUE '/'.                  00031100
031200         10  RPT-RUN-YR        PIC 99.                            00031200
031300     05  FILLER                PIC X(04).                         00031300
031400                                                                  00031400
031500 01  RPT-OPER-HDR-L1.                                             00031500
031600     05  FILLER                PIC X   VALUE SPACES.              00031600
031700     05  FILLER                PIC X(16)                          00031700
031800           VALUE 'OPERATOR NAME:  '.                              00031800
031900     05  RPT-OPER-NAME         PIC X(50).                         00031900
032000     05  FILLER                PIC X(65).                         00032000
032100                                                                  00032100
032200 01  RPT-OPER-HDR-L2.                                             00032200
032300     05  FILLER                PIC X   VALUE SPACES.              00032300
032400     05  FILLER                PIC X(16)                          00032400
032500           VALUE 'OPERATOR ID:    '.                              00032500
032600     05  RPT-OPERATOR-ID       PIC X(08).                         00032600
032700     05  FILLER                PIC X(107).                        00032700
032800                                                                  00032800
032900 01  RPT-DETAIL-HDR-L1.                                           00032900
033000     05  FILLER                PIC X   VALUE SPACES.              00033000
033100     05  FILLER                PIC X(17)                          00033100
033200           VALUE 'PLAN  GROUP     '.                              00033200
033300     05  FILLER                PIC X(15)                          00033300
033400           VALUE 'SECTION   PKG  '.                               00033400
033500     05  FILLER                PIC X(28)                          00033500
033600           VALUE '     PROV      EFFECTIVE    '.                  00033600
033700     05  FILLER                PIC X(16)                          00033700
033800           VALUE 'FUNC        FUNC'.                              00033800
033900     05  FILLER                PIC X(33) VALUE SPACES.            00033900
034000     05  FILLER                PIC X(22)                          00034000
034100           VALUE 'BEFORE      AFTER     '.                        00034100
034200                                                                  00034200
034300 01  RPT-DETAIL-HDR-L2.                                           00034300
034400     05  FILLER                PIC X   VALUE SPACES.              00034400
034500     05  FILLER                PIC X(17)                          00034500
034600           VALUE 'CODE  NUMBER     '.                             00034600
034700     05  FILLER                PIC X(15)                          00034700
034800           VALUE 'NUMBER    CODE '.                               00034800
034900     05  FILLER                PIC X(28)                          00034900
035000           VALUE 'LOB  CTRL FRL  DATE         '.                  00035000
035100     05  FILLER                PIC X(23)                          00035100
035200           VALUE 'DATE        DESCRIPTION'.                       00035200
035300     05  FILLER                PIC X(26) VALUE SPACES.            00035300
035400     05  FILLER                PIC X(22)                          00035400
035500           VALUE 'IMAGE       IMAGE     '.                        00035500
035600                                                                  00035600
035700 01  RPT-DETAIL-HDR-L3.                                           00035700
035800     05  FILLER                PIC X     VALUE SPACES.            00035800
035900     05  FILLER                PIC X(04) VALUE ALL '_'.           00035900
036000     05  FILLER                PIC X(02).                         00036000
036100     05  FILLER                PIC X(09) VALUE ALL '_'.           00036100
036200     05  FILLER                PIC X(02).                         00036200
036300     05  FILLER                PIC X(06) VALUE ALL '_'.           00036300
036400     05  FILLER                PIC X(04).                         00036400
036500     05  FILLER                PIC X(03) VALUE ALL '_'.           00036500
036600     05  FILLER                PIC X(02).                         00036600
036700     05  FILLER                PIC X(03) VALUE ALL '_'.           00036700
036800     05  FILLER                PIC X(02).                         00036800
036900     05  FILLER                PIC X(04) VALUE ALL '_'.           00036900
037000     05  FILLER                PIC X(01).                         00037000
037100     05  FILLER                PIC X(03) VALUE ALL '_'.           00037100
037200     05  FILLER                PIC X(02).                         00037200
037300     05  FILLER                PIC X(10) VALUE ALL '_'.           00037300
037400     05  FILLER                PIC X(03).                         00037400
037500     05  FILLER                PIC X(10) VALUE ALL '_'.           00037500
037600     05  FILLER                PIC X(02).                         00037600
037700     05  FILLER                PIC X(11) VALUE ALL '_'.           00037700
037800     05  FILLER                PIC X(26).                         00037800
037900     05  FILLER                PIC X(07) VALUE ALL '_'.           00037900
038000     05  FILLER                PIC X(05).                         00038000
038100     05  FILLER                PIC X(07) VALUE ALL '_'.           00038100
038200     05  FILLER                PIC X(03).                         00038200
038300                                                                  00038300
038400 01  RPT-DETAIL-LINE.                                             00038400
038500     05  FILLER                PIC X(01).                         00038500
038600     05  RPT-PLAN-CODE         PIC X(03).                         00038600
038700     05  FILLER                PIC X(03).                         00038700
038800     05  RPT-GROUP-NO          PIC X(09).                         00038800
038900     05  FILLER                PIC X(02).                         00038900
039000     05  RPT-SECTION-NO        PIC X(05).                         00039000
039100     05  FILLER                PIC X(05).                         00039100
039200     05  RPT-PKG-CODE          PIC X(03).                         00039200
039300     05  FILLER                PIC X(02).                         00039300
039400     05  RPT-L-O-B             PIC X.                             00039400
039500     05  FILLER                PIC X(04).                         00039500
039600     05  RPT-PROV-CTRL         PIC X(02).                         00039600
039700     05  FILLER                PIC X(03).                         00039700
039800     05  RPT-FAM-REL-LEVEL     PIC X(02).                         00039800
039900     05  FILLER                PIC X(03).                         00039900
040000     05  RPT-EFF-DATE.                                            00040000
040100         10  RPT-EFFDT-MO      PIC 99.                            00040100
040200         10  FILLER            PIC X     VALUE '/'.               00040200
040300         10  RPT-EFFDT-DY      PIC 99.                            00040300
040400         10  FILLER            PIC X     VALUE '/'.               00040400
040500         10  RPT-EFFDT-YR      PIC 9999.                          00040500
040600     05  FILLER                PIC X(03).                         00040600
040700     05  RPT-FUNC-DATE.                                           00040700
040800         10  RPT-FUNCD-MO      PIC 99.                            00040800
040900         10  FILLER            PIC X     VALUE '/'.               00040900
041000         10  RPT-FUNCD-DY      PIC 99.                            00041000
041100         10  FILLER            PIC X     VALUE '/'.               00041100
041200         10  RPT-FUNCD-YR      PIC 9999.                          00041200
041300     05  FILLER                PIC X(02).                         00041300
041400     05  RPT-TRAILER.                                             00041400
041500         10  RPT-FUNC-DESCR        PIC X(35).                     00041500
041600         10  FILLER                PIC X(02).                     00041600
041700         10  RPT-BIM-FIELD         PIC X(10).                     00041700
041800         10  FILLER                PIC X(02).                     00041800
041900         10  RPT-AIM-FIELD         PIC X(10).                     00041900
042000         10  FILLER                PIC X(01).                     00042000
042100     05  RPT-MASS-MAP-CON REDEFINES                               00042100
042200         RPT-TRAILER.                                             00042200
042300         10  RPT-CON-FUNC-DESCR          PIC X(21).               00042300
042400         10  FILLER                      PIC X(02).               00042400
042500         10  RPT-BIM-CON-GRP-NO          PIC X(09).               00042500
042600         10  FILLER                      PIC X(01).               00042600
042700         10  RPT-BIM-CON-SECTN-NO        PIC X(05).               00042700
042800         10  FILLER                      PIC X(01).               00042800
042900         10  RPT-BIM-CON-PKG-CODE        PIC X(03).               00042900
043000         10  FILLER                      PIC X(01).               00043000
043100         10  RPT-BIM-CON-L-O-B           PIC X(01).               00043100
043200         10  FILLER                      PIC X(01).               00043200
043300         10  RPT-BIM-CON-PROV-CTL        PIC X(02).               00043300
043400         10  FILLER                      PIC X(01).               00043400
043500         10  RPT-BIM-CON-FAM-REL-LV      PIC X(02).               00043500
043600         10  FILLER                      PIC X(01).               00043600
043700         10  RPT-BIM-CON-EFF-DT.                                  00043700
043800             15  RPT-CON-EFF-DT-MM       PIC X(02).               00043800
043900             15  SLASH-1                 PIC X(01).               00043900
044000             15  RPT-CON-EFF-DT-DD       PIC X(02).               00044000
044100             15  SLASH-2                 PIC X(01).               00044100
044200             15  RPT-CON-EFF-DT-YY       PIC X(02).               00044200
044300         10  FILLER                      PIC X(01).               00044300
044400     05  RPT-MASS-MAP-GPS REDEFINES                               00044400
044500         RPT-TRAILER.                                             00044500
044600         10  RPT-GPS-FUNC-DESCR          PIC X(25).               00044600
044700         10  FILLER                      PIC X(02).               00044700
044800         10  RPT-BIM-GPS-GRP-NO          PIC X(09).               00044800
044900         10  FILLER                      PIC X(01).               00044900
045000         10  RPT-BIM-GPS-SECTN-NO        PIC X(05).               00045000
045100         10  FILLER                      PIC X(02).               00045100
045200         10  RPT-BIM-GPS-PKG-CODE        PIC X(03).               00045200
045300         10  FILLER                      PIC X(01).               00045300
045400         10  RPT-BIM-GPS-FAM-REL-LV      PIC X(02).               00045400
045500         10  FILLER                      PIC X(01).               00045500
045600         10  RPT-BIM-GPS-EFF-DT.                                  00045600
045700             15  RPT-GPS-EFF-DT-MM       PIC X(02).               00045700
045800             15  SLASH-3                 PIC X(01).               00045800
045900             15  RPT-GPS-EFF-DT-DD       PIC X(02).               00045900
046000             15  SLASH-4                 PIC X(01).               00046000
046100             15  RPT-GPS-EFF-DT-YY       PIC X(02).               00046100
046200*        10  FILLER                      PIC X(06).               00046200
046300                                                                  00046300
046400 01  RPT-OPER-LINE.                                               00046400
046500     05  FILLER                PIC X(11).                         00046500
046600     05  FILLER                PIC X(43)                          00046600
046700          VALUE '**  NOTHING TO REPORT FOR THIS OPERATOR  **'.    00046700
046800     05  FILLER                PIC X(78).                         00046800
046900                                                                  00046900
047000/                                                                 00047000
047100 01  WS-REC-LEN-AREA.                                             00047100
047200 COPY GCCDRLEN.                                                   00047200
047300/                                                                 00047300
047400******************************************************************00047400
047500****          SET PARAMETER FOR ALL VSAM FILES USED           ****00047500
047600******************************************************************00047600
047700 01  PARM-SET.                                                    00047700
047800     05  SET-RDW.                                                 00047800
047900         10  SET-RECORD-LENGTH             PIC 9(04) COMP VALUE 0.00047900
048000         10  SET-FEEDBACK-CODE             PIC 9(04) COMP VALUE 0.00048000
048100     05  SET-VALUE                         PIC 9(08) COMP VALUE 0.00048100
048200                                                                  00048200
048300******************************************************************00048300
048400****          VSAM CALL PARAMETERS                            ****00048400
048500******************************************************************00048500
048600 01  PARM-ONE.                                                    00048600
048700     05  RESERVED                          PIC 9(05) COMP VALUE 0.00048700
048800     05  RESERVED-X REDEFINES RESERVED.                           00048800
048900         10  REQUEST-TYPE                  PIC X(01).             00048900
049000         10  REQUEST-FILLER                PIC X(03).             00049000
049100                                                                  00049100
049200******************************************************************00049200
049300****          PARAMETERS FOR CODE/NAME RECORDS                ****00049300
049400******************************************************************00049400
049500 01  PARM-CV-NAME.                                                00049500
049600     05  CV-NAME-RDW.                                             00049600
049700         10  CVNM-RECORD-LENGTH            PIC 9(04) COMP VALUE 0.00049700
049800         10  CVNM-FEEDBACK-CODE            PIC 9(04) COMP VALUE 0.00049800
049900     COPY ELPCVC.                                                 00049900
050000/                                                                 00050000
050100                                                                  00050100
050200******************************************************************00050200
050300****          BIM/AIM CONTRACT FIELDS TABLE                   ****00050300
050400******************************************************************00050400
050500     COPY GCARCHCT.                                               00050500
050600                                                                  00050600
050700                                                                  00050700
050800******************************************************************00050800
050900****          BIM/AIM GROUP SPECIFIC FIELDS TABLE             ****00050900
051000******************************************************************00051000
051100     COPY GCARCHGS.                                               00051100
051200/                                                                 00051200
051300                                                                  00051300
051400******************************************************************00051400
051500****          TABULAR DESCRIPTION TABLES                      ****00051500
051600******************************************************************00051600
051700                                                                  00051700
051800*** GROUP SPECIFIC ***                                            00051800
051900     COPY GCGTABS.                                                00051900
052000     05  GRSP-DESCRIPTION-TABLE      REDEFINES                    00052000
TM0526         WT-02-DESCRIPTION-VALUES    OCCURS 058 TIMES             00052100
052200                                     INDEXED BY GRSP-INDEX.       00052200
052300         10  GRSP-ENTRY.                                          00052300
052400             15  GRSP-TAB-ID             PIC X(06).               00052400
052500             15  GRSP-TAB-DESCRIPTION    PIC X(25).               00052500
052600             15  GRSP-TAB-DELETE-PGM     PIC X(08).               00052600
052700             15  GRSP-TAB-ADD-PGM        PIC X(08).               00052700
052800/                                                                 00052800
052900*** CONTRACT ***                                                  00052900
053000     COPY GCCTABS.                                                00053000
053100     05  CONT-DESCRIPTION-TABLE      REDEFINES                    00053100
053200         WT-02-DESCRIPTION-VALUES    OCCURS 022 TIMES             00053200
053300                                     INDEXED BY CONT-INDEX.       00053300
053400         10  CONT-ENTRY.                                          00053400
053500             15  CONT-TAB-ID             PIC X(06).               00053500
053600             15  CONT-TAB-DESCRIPTION    PIC X(25).               00053600
053700             15  CONT-TAB-DELETE-PGM     PIC X(08).               00053700
053800             15  CONT-TAB-ADD-PGM        PIC X(08).               00053800
053900/                                                                 00053900
054000 PROCEDURE DIVISION.                                              00054000
054100 0000-MAINLINE.                                                   00054100
054200                                                                  00054200
054300     PERFORM 0025-OPEN-AND-INITIALIZE THRU 0025-EXIT.             00054300
054400     PERFORM 0090-READ-NAME-FILE      THRU 0090-EXIT.             00054400
054500     PERFORM 0100-READ-RND-FILE       THRU 0100-EXIT.             00054500
054600                                                                  00054600
054700     MOVE RND-OPERATOR-ID   TO LAST-OPERATOR.                     00054700
054800     MOVE RND-GROUP-NO      TO LAST-GROUP-NO.                     00054800
054900     PERFORM 0200-READ-QCF-FILE THRU 0200-EXIT.                   00054900
055000                                                                  00055000
055100     IF CV-CODE-VALUE < RND-OPERATOR-ID                           00055100
055200        PERFORM UNTIL                                             00055200
055300          (CV-CODE-VALUE > RND-OPERATOR-ID) OR                    00055300
055400          (CV-CODE-VALUE = RND-OPERATOR-ID)                       00055400
055500           IF GOOD-NAME                                           00055500
055600              PERFORM 0700-PROCESS-NAME-ONLY THRU 0700-EXIT       00055600
055700           END-IF                                                 00055700
055800           PERFORM 0090-READ-NAME-FILE    THRU 0090-EXIT          00055800
055900        END-PERFORM                                               00055900
056000     END-IF.                                                      00056000
056100                                                                  00056100
056200     PERFORM 0300-EVALUATE-RND-NAME THRU 0300-EXIT                00056200
056300         UNTIL END-OF-RND-FILE                                    00056300
056400            OR END-OF-QCF-FILE.                                   00056400
056500                                                                  00056500
056600     MOVE 'C' TO REQUEST-TYPE.                                    00056600
056700     CALL 'TSGVSAM1' USING PARM-ONE                               00056700
056800                           PARM-SET.                              00056800
056900                                                                  00056900
057000     IF  REQUEST-TYPE NOT = 'C'                                   00057000
057100         DISPLAY '*** INVALID CLOSE OF INPUT CODE FILE ***'       00057100
057200         MOVE SET-FEEDBACK-CODE TO ABEND-CODE                     00057200
057300                                   DISPLAY-FEEDBACK               00057300
057400         DISPLAY ' FEEDBACK CODE = ' DISPLAY-FEEDBACK             00057400
057500         GO TO 9999-ABEND-ROUTINE.                                00057500
057600                                                                  00057600
057700     CLOSE  QCF-FILE                                              00057700
057800            RND-FILE                                              00057800
057900            RPT-FILE.                                             00057900
058000     GOBACK.                                                      00058000
058100                                                                  00058100
058200 0000-EXIT.                                                       00058200
058300     EXIT.                                                        00058300
058400/                                                                 00058400
058500**************************************************************    00058500
058600*   OPEN FILES AND INITIALIZE THE REPORT HEADING             *    00058600
058700**************************************************************    00058700
058800 0025-OPEN-AND-INITIALIZE.                                        00058800
058900                                                                  00058900
059000     OPEN   INPUT      QCF-FILE.                                  00059000
059100     OPEN   INPUT      RND-FILE.                                  00059100
059200     OPEN   OUTPUT     RPT-FILE.                                  00059200
059300                                                                  00059300
059400     MOVE 'S' TO REQUEST-TYPE.                                    00059400
059500     MOVE 8   TO SET-RECORD-LENGTH.                               00059500
059600     MOVE 3   TO SET-VALUE.                                       00059600
059700                                                                  00059700
059800     CALL 'TSGVSAM1' USING PARM-ONE                               00059800
059900                           PARM-SET.                              00059900
060000                                                                  00060000
060100     IF  REQUEST-TYPE NOT = 'S'                                   00060100
060200         DISPLAY '*** INVALID OPEN OF INPUT CODE FILE ***'        00060200
060300         MOVE SET-FEEDBACK-CODE TO ABEND-CODE                     00060300
060400                                   DISPLAY-FEEDBACK               00060400
060500         DISPLAY ' FEEDBACK CODE = ' DISPLAY-FEEDBACK             00060500
060600         GO TO 9999-ABEND-ROUTINE.                                00060600
060700                                                                  00060700
060800     MOVE 'BENANA'    TO CV-RECORD-PREFIX                         00060800
060900                         LAST-PREFIX.                             00060900
061000     MOVE 1           TO CV-ELEMENT-NBR.                          00061000
061100     MOVE LOW-VALUES  TO CV-CODE-VALUE.                           00061100
061200     MOVE ZEROS       TO CV-CODE-DESC-SEQ.                        00061200
061300     MOVE 'P'         TO REQUEST-TYPE.                            00061300
061400     MOVE 27          TO SET-RECORD-LENGTH.                       00061400
061500     MOVE 3           TO SET-VALUE.                               00061500
061600                                                                  00061600
061700     CALL 'TSGVSAM1' USING PARM-ONE                               00061700
061800                           PARM-CV-NAME.                          00061800
061900                                                                  00061900
062000     IF  REQUEST-TYPE NOT = 'P'                                   00062000
062100         DISPLAY '*** INVALID START OF INPUT CODE FILE ***'       00062100
062200         MOVE SET-FEEDBACK-CODE TO ABEND-CODE                     00062200
062300                                   DISPLAY-FEEDBACK               00062300
062400         DISPLAY ' FEEDBACK CODE = ' DISPLAY-FEEDBACK             00062400
062500         GO TO 9999-ABEND-ROUTINE.                                00062500
062600                                                                  00062600
062700     PERFORM 0050-FRMT-DT-TIME  THRU 0050-EXIT.                   00062700
062800                                                                  00062800
062900 0025-EXIT.                                                       00062900
063000     EXIT.                                                        00063000
063100/                                                                 00063100
063200**************************************************************    00063200
063300*   FORMAT DATE AND TIME FOR REPORT HEADING                  *    00063300
063400**************************************************************    00063400
063500 0050-FRMT-DT-TIME.                                               00063500
063600                                                                  00063600
063700     ACCEPT WS-DATE FROM DATE.                                    00063700
063800     MOVE WS-DATE-MO  TO RPT-RUN-MO.                              00063800
063900     MOVE WS-DATE-DY  TO RPT-RUN-DY.                              00063900
064000     MOVE WS-DATE-YR  TO RPT-RUN-YR.                              00064000
064100                                                                  00064100
064200     ACCEPT WS-TIME FROM TIME.                                    00064200
064300     MOVE WS-TIME-HR  TO RPT-RUN-HR.                              00064300
064400     MOVE WS-TIME-MN  TO RPT-RUN-MIN.                             00064400
064500     MOVE WS-TIME-SC  TO RPT-RUN-SEC.                             00064500
064600                                                                  00064600
064700 0050-EXIT.                                                       00064700
064800     EXIT.                                                        00064800
064900/                                                                 00064900
065000**************************************************************    00065000
065100*   READ THE VSAM NAME FILE SEQUENTIALLY                     *    00065100
065200**************************************************************    00065200
065300 0090-READ-NAME-FILE.                                             00065300
065400                                                                  00065400
065500     MOVE 'G' TO REQUEST-TYPE.                                    00065500
065600     CALL 'TSGVSAM1' USING PARM-ONE                               00065600
065700                           PARM-CV-NAME.                          00065700
065800                                                                  00065800
065900     IF  REQUEST-TYPE NOT = 'G'                                   00065900
066000         DISPLAY '*** INVALID READ NEXT OF INPUT CODE FILE ***'   00066000
066100         MOVE SET-FEEDBACK-CODE TO ABEND-CODE                     00066100
066200                                   DISPLAY-FEEDBACK               00066200
066300         DISPLAY ' FEEDBACK CODE = ' DISPLAY-FEEDBACK             00066300
066400         GO TO 9999-ABEND-ROUTINE.                                00066400
066500                                                                  00066500
066600     MOVE CV-CODE-NAME  TO OPER-NAME.                             00066600
066700                                                                  00066700
066800     IF HLD-FRST-BYTE = '*'                                       00066800
066900        SET NT-GOOD-NAME TO TRUE                                  00066900
067000     ELSE                                                         00067000
067100        SET GOOD-NAME    TO TRUE.                                 00067100
067200                                                                  00067200
067300 0090-EXIT.                                                       00067300
067400     EXIT.                                                        00067400
067500/                                                                 00067500
067600**************************************************************    00067600
067700*   READ THE RANDOMIZED FILE                                 *    00067700
067800**************************************************************    00067800
067900 0100-READ-RND-FILE.                                              00067900
068000                                                                  00068000
068100     INITIALIZE RND-RECORD.                                       00068100
068200                                                                  00068200
068300     READ  RND-FILE                                               00068300
068400           AT END  MOVE 'END'  TO END-OF-FILE-RND                 00068400
068500                   GO TO 0100-EXIT.                               00068500
068600                                                                  00068600
068700     IF RND-OPERATOR-ID NOT = LAST-OPERATOR                       00068700
068800        SET OPER-OFF  TO TRUE.                                    00068800
068900                                                                  00068900
069000     SET GROUP-OFF TO TRUE.                                       00069000
069100                                                                  00069100
069200 0100-EXIT.                                                       00069200
069300     EXIT.                                                        00069300
069400/                                                                 00069400
069500**************************************************************    00069500
069600*   READ THE QUALITY CONTROL FILE                            *    00069600
069700**************************************************************    00069700
069800 0200-READ-QCF-FILE.                                              00069800
069900                                                                  00069900
070000     INITIALIZE QCF-QUAL-CTRL-RECORD.                             00070000
070100                                                                  00070100
070200     READ  QCF-FILE                                               00070200
070300           AT END  MOVE 'END'  TO END-OF-FILE-QCF                 00070300
070400                   GO TO 0200-EXIT.                               00070400
070500                                                                  00070500
070600 0200-EXIT.                                                       00070600
070700     EXIT.                                                        00070700
070800/                                                                 00070800
070900 0300-EVALUATE-RND-NAME.                                          00070900
071000**************************************************************    00071000
071100*   PROCESS THE RANDOMIZED, QUALITY CONTROL, AND CODE FILES  *    00071100
071200**************************************************************    00071200
071300                                                                  00071300
071400     IF NT-GOOD-NAME                                              00071400
071500        MOVE CV-CODE-VALUE   TO HLD-NT-GOOD-NM-VALUE              00071500
071600        PERFORM 0090-READ-NAME-FILE THRU 0090-EXIT                00071600
071700          UNTIL GOOD-NAME.                                        00071700
071800                                                                  00071800
071900     IF ((CV-CODE-VALUE < RND-OPERATOR-ID  AND                    00071900
072000          CV-RECORD-PREFIX = LAST-PREFIX)  OR                     00072000
072100         (CV-CODE-VALUE < RND-OPERATOR-ID  AND                    00072100
072200          CV-CODE-VALUE > LAST-OPERATOR))                         00072200
072300          IF CV-CODE-VALUE NOT = LAST-CODE-VALUE AND              00072300
072400             GOOD-NAME                                            00072400
072500               PERFORM 0700-PROCESS-NAME-ONLY THRU 0700-EXIT      00072500
072600          END-IF                                                  00072600
072700          IF CV-RECORD-PREFIX NOT > LAST-PREFIX OR                00072700
072800             NT-GOOD-NAME                                         00072800
072900               PERFORM 0090-READ-NAME-FILE THRU 0090-EXIT         00072900
073000          END-IF                                                  00073000
073100*         GO TO 0300-EXIT                                         00073100
073200     ELSE                                                         00073200
073300         IF ((CV-CODE-VALUE = RND-OPERATOR-ID) OR                 00073300
073400             (CV-CODE-VALUE < RND-OPERATOR-ID  AND                00073400
073500              CV-RECORD-PREFIX = LAST-PREFIX)  OR                 00073500
073600             (CV-CODE-VALUE < RND-OPERATOR-ID  AND                00073600
073700              CV-RECORD-PREFIX NOT = LAST-PREFIX)) AND            00073700
073800              GOOD-NAME                                           00073800
073900              IF (CV-CODE-VALUE < RND-OPERATOR-ID     AND         00073900
074000                 CV-CODE-VALUE NOT = LAST-CODE-VALUE AND          00074000
074100                 CV-RECORD-PREFIX = LAST-PREFIX      AND          00074100
074200                 GOOD-NAME)                                       00074200
074300                   PERFORM 0700-PROCESS-NAME-ONLY THRU 0700-EXIT  00074300
074400              END-IF                                              00074400
074500              PERFORM 0400-PROCESS-RND-FILE THRU 0400-EXIT        00074500
074600                UNTIL  RND-OPERATOR-ID NOT = LAST-OPERATOR        00074600
074700                   OR  END-OF-RND-FILE                            00074700
074800                   OR  END-OF-QCF-FILE                            00074800
074900              MOVE RND-OPERATOR-ID  TO LAST-OPERATOR              00074900
075000*             GO TO 0300-EXIT                                     00075000
075100         ELSE                                                     00075100
075200             IF CV-CODE-VALUE > RND-OPERATOR-ID AND               00075200
075300                GOOD-NAME                                         00075300
075400                PERFORM 0400-PROCESS-RND-FILE THRU 0400-EXIT      00075400
075500                  UNTIL RND-OPERATOR-ID NOT = LAST-OPERATOR       00075500
075600                     OR END-OF-RND-FILE                           00075600
075700                     OR END-OF-QCF-FILE                           00075700
075800                MOVE RND-OPERATOR-ID  TO LAST-OPERATOR            00075800
075900*               GO TO 0300-EXIT                                   00075900
076000             END-IF                                               00076000
076100         END-IF                                                   00076100
076200     END-IF.                                                      00076200
076300                                                                  00076300
076400     IF  END-OF-RND-FILE OR                                       00076400
076500         END-OF-QCF-FILE                                          00076500
076600           PERFORM UNTIL CV-RECORD-PREFIX NOT = LAST-PREFIX       00076600
076700              PERFORM 0090-READ-NAME-FILE THRU 0090-EXIT          00076700
076800              IF GOOD-NAME AND                                    00076800
076900                 CV-RECORD-PREFIX = LAST-PREFIX                   00076900
077000                   PERFORM 0700-PROCESS-NAME-ONLY THRU 0700-EXIT  00077000
077100              END-IF                                              00077100
077200           END-PERFORM                                            00077200
077300     END-IF.                                                      00077300
077400                                                                  00077400
077500 0300-EXIT.                                                       00077500
077600     EXIT.                                                        00077600
077700/                                                                 00077700
077800 0400-PROCESS-RND-FILE.                                           00077800
077900**************************************************************    00077900
078000*   PROCESS THE RANDOMIZED FILE TO PRODUCE THE REPORT        *    00078000
078100**************************************************************    00078100
078200                                                                  00078200
078300     IF RND-OPERATOR-ID = QCF-OPERATOR-ID                         00078300
078400        IF RND-GROUP-NO = QCF-GROUP-NO                            00078400
078500           PERFORM 0500-WRITE-REPORT-LINE THRU 0500-EXIT          00078500
078600           PERFORM 0200-READ-QCF-FILE THRU 0200-EXIT              00078600
078700        ELSE                                                      00078700
078800           IF RND-GROUP-NO > QCF-GROUP-NO                         00078800
078900              PERFORM 0200-READ-QCF-FILE  THRU 0200-EXIT          00078900
079000           ELSE                                                   00079000
079100              IF RND-GROUP-NO < QCF-GROUP-NO                      00079100
079200                 MOVE RND-OPERATOR-ID  TO LAST-OPERATOR           00079200
079300                 PERFORM 0100-READ-RND-FILE  THRU 0100-EXIT       00079300
079400              END-IF                                              00079400
079500           END-IF                                                 00079500
079600        END-IF                                                    00079600
079700     ELSE                                                         00079700
079800        IF RND-OPERATOR-ID > QCF-OPERATOR-ID AND                  00079800
079900           NOT END-OF-QCF-FILE                                    00079900
080000            PERFORM 0200-READ-QCF-FILE  THRU 0200-EXIT            00080000
080100        ELSE                                                      00080100
080200            MOVE RND-OPERATOR-ID  TO LAST-OPERATOR                00080200
080300            PERFORM 0100-READ-RND-FILE  THRU 0100-EXIT            00080300
080400        END-IF                                                    00080400
080500     END-IF.                                                      00080500
080600                                                                  00080600
080700 0400-EXIT.                                                       00080700
080800     EXIT.                                                        00080800
080900/                                                                 00080900
081000**************************************************************    00081000
081100*   WRITE THE DETAIL LINE OF INFORMATION TO THE REPORT       *    00081100
081200**************************************************************    00081200
081300 0500-WRITE-REPORT-LINE.                                          00081300
081400                                                                  00081400
081500     IF RND-OPERATOR-ID = HLD-NT-GOOD-NM-VALUE                    00081500
081600        GO TO 0500-EXIT.                                          00081600
081700                                                                  00081700
081800     MOVE SPACES            TO RPT-TRAILER.                       00081800
081900                                                                  00081900
082000     MOVE QCF-SECTION-NO    TO RPT-SECTION-NO.                    00082000
082100     MOVE QCF-PKG-CODE      TO RPT-PKG-CODE.                      00082100
082200     MOVE QCF-L-O-B         TO RPT-L-O-B.                         00082200
082300     MOVE QCF-PROV-CTRL     TO RPT-PROV-CTRL.                     00082300
082400     MOVE QCF-FAM-REL-LEVEL TO RPT-FAM-REL-LEVEL.                 00082400
082500                                                                  00082500
082600     PERFORM 1400-CONV-TERM-DATE    THRU 1400-EXIT.               00082600
082700     PERFORM 0550-FIND-FUNC-DESCRP  THRU 0550-EXIT.               00082700
082800                                                                  00082800
082900                                                                  00082900
083000*    MOVE QCF-BIM-FIELD     TO RPT-BIM-FIELD.                     00083000
083100*    MOVE QCF-AIM-FIELD     TO RPT-AIM-FIELD.                     00083100
083200                                                                  00083200
083300     MOVE QCF-EFF-DATE      TO HLD-DATE-AREA.                     00083300
083400     MOVE HLD-JUL           TO WS-YYDDD.                          00083400
083500     CALL 'TSGGREG'         USING WS-YYDDD                        00083500
083600                                  WS-MDY.                         00083600
083700     MOVE WS-M              TO RPT-EFFDT-MO.                      00083700
083800     MOVE WS-D              TO RPT-EFFDT-DY.                      00083800
083900     MOVE WS-Y              TO CEN-YY.                            00083900
084000     MOVE HLD-CEN           TO CEN-CC                             00084000
084100     MOVE CEN-DT            TO RPT-EFFDT-YR.                      00084100
084200                                                                  00084200
084300     MOVE QCF-FUNC-DATE     TO HLD-DATE-AREA.                     00084300
084400     MOVE HLD-JUL           TO WS-YYDDD.                          00084400
084500     CALL 'TSGGREG'         USING WS-YYDDD                        00084500
084600                                  WS-MDY.                         00084600
084700     MOVE WS-M              TO RPT-FUNCD-MO.                      00084700
084800     MOVE WS-D              TO RPT-FUNCD-DY.                      00084800
084900     MOVE WS-Y              TO CEN-YY.                            00084900
085000     MOVE HLD-CEN           TO CEN-CC                             00085000
085100     MOVE CEN-DT            TO RPT-FUNCD-YR.                      00085100
085200                                                                  00085200
085300     IF CV-CODE-VALUE = RND-OPERATOR-ID                           00085300
085400        MOVE CV-CODE-VALUE  TO LAST-CODE-VALUE                    00085400
085500        MOVE CV-CODE-NAME   TO RPT-OPER-NAME                      00085500
085600     ELSE                                                         00085600
085700        IF (CV-CODE-VALUE > RND-OPERATOR-ID) OR                   00085700
085800           (CV-CODE-VALUE < RND-OPERATOR-ID) OR                   00085800
085900           (CV-RECORD-PREFIX NOT = LAST-PREFIX)                   00085900
086000            MOVE NO-NM-FND-MSG  TO RPT-OPER-NAME                  00086000
086100        END-IF                                                    00086100
086200     END-IF.                                                      00086200
086300                                                                  00086300
086400     IF RPT-LINE-COUNT > 52                                       00086400
086500        PERFORM 0600-WRITE-HEADER THRU 0600-EXIT                  00086500
086600        MOVE QCF-PLAN-CODE    TO RPT-PLAN-CODE                    00086600
086700        MOVE QCF-GROUP-NO     TO RPT-GROUP-NO                     00086700
086800        WRITE RPT-RECORD FROM RPT-DETAIL-LINE                     00086800
086900           AFTER ADVANCING 2 LINES                                00086900
087000        ADD 2   TO RPT-LINE-COUNT                                 00087000
087100        GO TO 0500-EXIT                                           00087100
087200     END-IF.                                                      00087200
087300                                                                  00087300
087400     IF OPER-OFF                                                  00087400
087500        PERFORM 0600-WRITE-HEADER THRU 0600-EXIT                  00087500
087600        MOVE QCF-PLAN-CODE    TO RPT-PLAN-CODE                    00087600
087700        MOVE QCF-GROUP-NO     TO RPT-GROUP-NO                     00087700
087800        WRITE RPT-RECORD FROM RPT-DETAIL-LINE                     00087800
087900           AFTER ADVANCING 2 LINES                                00087900
088000        SET OPER-ON           TO TRUE                             00088000
088100        SET GROUP-ON          TO TRUE                             00088100
088200        ADD 2   TO RPT-LINE-COUNT                                 00088200
088300        GO TO 0500-EXIT                                           00088300
088400     ELSE                                                         00088400
088500        MOVE SPACES      TO RPT-PLAN-CODE                         00088500
088600     END-IF.                                                      00088600
088700                                                                  00088700
088800     IF GROUP-ON                                                  00088800
088900        MOVE SPACES        TO RPT-GROUP-NO                        00088900
089000        WRITE RPT-RECORD FROM RPT-DETAIL-LINE                     00089000
089100           AFTER ADVANCING 1 LINES                                00089100
089200        ADD 1   TO RPT-LINE-COUNT                                 00089200
089300     ELSE                                                         00089300
089400        MOVE QCF-GROUP-NO  TO RPT-GROUP-NO                        00089400
089500        WRITE RPT-RECORD FROM RPT-DETAIL-LINE                     00089500
089600           AFTER ADVANCING 2 LINES                                00089600
089700        SET GROUP-ON       TO TRUE                                00089700
089800        ADD 2   TO RPT-LINE-COUNT                                 00089800
089900     END-IF.                                                      00089900
090000                                                                  00090000
090100 0500-EXIT.                                                       00090100
090200     EXIT.                                                        00090200
090300/                                                                 00090300
090400**************************************************************    00090400
090500*   DETERMINE WITH FUNCTION DESCRIPTION SHOULD BE USED       *    00090500
090600**************************************************************    00090600
090700 0550-FIND-FUNC-DESCRP.                                           00090700
090800                                                                  00090800
090900     MOVE QCF-FUNC-FIELD    TO HLD-FUNC-FIELD.                    00090900
091000                                                                  00091000
091100     EVALUATE HLD-TWO-BYTES                                       00091100
091200        WHEN  'TT'                                                00091200
091300             PERFORM 0560-SEARCH-TABS-TABLE  THRU 0560-EXIT       00091300
091400        WHEN  'BB'                                                00091400
091500             PERFORM 0570-FORMAT-BEN-DSCRPTN THRU 0570-EXIT       00091500
091600        WHEN  OTHER                                               00091600
091700             PERFORM 0580-SEARCH-BIMAIM-TBL  THRU 0580-EXIT       00091700
091800     END-EVALUATE.                                                00091800
091900                                                                  00091900
092000     EVALUATE HLD-FIVE-BYTES                                      00092000
092100        WHEN  'CNTKY'                                             00092100
092200             PERFORM 0590-FORMAT-CON-DSCRPTN THRU 0590-EXIT       00092200
092300        WHEN  'CNTDL'                                             00092300
092400             PERFORM 0590-FORMAT-CON-DSCRPTN THRU 0590-EXIT       00092400
092500        WHEN  'GPSKY'                                             00092500
092600             PERFORM 0595-FORMAT-GPS-DSCRPTN THRU 0595-EXIT       00092600
092700        WHEN  'GPSDL'                                             00092700
092800             PERFORM 0595-FORMAT-GPS-DSCRPTN THRU 0595-EXIT       00092800
092900     END-EVALUATE.                                                00092900
093000                                                                  00093000
093100 0550-EXIT.                                                       00093100
093200     EXIT.                                                        00093200
093300/                                                                 00093300
093400**************************************************************    00093400
093500*  SEARCH TABLES FOR CONTRACT AND GROUP SPECIFIC FOR THE     *    00093500
093600*  RIGHT DESCRIPTION TO BE USED FOR TABULARS                 *    00093600
093700**************************************************************    00093700
093800 0560-SEARCH-TABS-TABLE.                                          00093800
093900                                                                  00093900
094000     IF QCF-CONTRACT-GROUP-SP-IND = 'C'                           00094000
094100        SET TAB-NT-FND TO TRUE                                    00094100
094200        SET CONT-INDEX TO 1                                       00094200
094300        MOVE SPACES TO RPT-FUNC-DESCR                             00094300
094400        PERFORM VARYING CONT-INDEX FROM 1 BY 1                    00094400
094500         UNTIL CONT-INDEX > 19                                    00094500
094600            OR TAB-FND                                            00094600
094700            IF CONT-TAB-ID (CONT-INDEX) = HLD-FUNC-AREA           00094700
094800               MOVE CONT-TAB-DESCRIPTION (CONT-INDEX)             00094800
094900                                        TO RPT-FUNC-DESCR         00094900
095000               SET TAB-FND TO TRUE                                00095000
095100            END-IF                                                00095100
095200        END-PERFORM                                               00095200
095300     ELSE                                                         00095300
095400        SET TAB-NT-FND TO TRUE                                    00095400
095500        SET GRSP-INDEX TO 1                                       00095500
095600        MOVE SPACES TO RPT-FUNC-DESCR                             00095600
095700        PERFORM VARYING GRSP-INDEX FROM 1 BY 1                    00095700
095800         UNTIL GRSP-INDEX > 46                                    00095800
095900            OR TAB-FND                                            00095900
096000            IF GRSP-TAB-ID (GRSP-INDEX) = HLD-FUNC-AREA           00096000
096100               MOVE GRSP-TAB-DESCRIPTION (GRSP-INDEX)             00096100
096200                                        TO RPT-FUNC-DESCR         00096200
096300               SET TAB-FND TO TRUE                                00096300
096400            END-IF                                                00096400
096500        END-PERFORM                                               00096500
096600     END-IF.                                                      00096600
096700                                                                  00096700
096800 0560-EXIT.                                                       00096800
096900     EXIT.                                                        00096900
097000/                                                                 00097000
097100**************************************************************    00097100
097200*  FORMAT THE BENEFIT PROVISION DESCRIPTION                  *    00097200
097300**************************************************************    00097300
097400 0570-FORMAT-BEN-DSCRPTN.                                         00097400
097500                                                                  00097500
097600     MOVE QCF-FUNC-FIELD  TO HOLD-BEN-PROV.                       00097600
097700     MOVE HLD-BEN-AREA    TO BEN-PROV-OUT.                        00097700
097800                                                                  00097800
097900     MOVE BEN-DESCRIPTION TO RPT-FUNC-DESCR.                      00097900
098000                                                                  00098000
098100 0570-EXIT.                                                       00098100
098200     EXIT.                                                        00098200
098300/                                                                 00098300
098400**************************************************************    00098400
098500*  SEARCH TABLES FOR CONTRACT AND GROUP SPECIFIC FOR THE     *    00098500
098600*  RIGHT DESCRIPTION TO BE USED FOR BEFORE AND AFTER IMAGES  *    00098600
098700**************************************************************    00098700
098800 0580-SEARCH-BIMAIM-TBL.                                          00098800
098900                                                                  00098900
099000     IF QCF-CONTRACT-GROUP-SP-IND = 'C'                           00099000
099100        SET FIELD-NT-FND TO TRUE                                  00099100
099200        SET GCT-A-INDEX TO 1                                      00099200
099300        MOVE SPACES TO RPT-FUNC-DESCR                             00099300
099400        PERFORM VARYING GCT-A-INDEX FROM 1 BY 1                   00099400
099500          UNTIL GCT-A-INDEX > 93                                  00099500
099600             OR FIELD-FND                                         00099600
099700             IF GCT-A-DE-ID (GCT-A-INDEX) = QCF-FUNC-FIELD        00099700
099800                MOVE GCT-A-DE-TEXT (GCT-A-INDEX)                  00099800
099900                                          TO RPT-FUNC-DESCR       00099900
100000                SET FIELD-FND TO TRUE                             00100000
100100             END-IF                                               00100100
100200        END-PERFORM                                               00100200
100300     ELSE                                                         00100300
100400        SET FIELD-NT-FND TO TRUE                                  00100400
100500        SET GCG-A-INDEX TO 1                                      00100500
100600        MOVE SPACES TO RPT-FUNC-DESCR                             00100600
100700        PERFORM VARYING GCG-A-INDEX FROM 1 BY 1                   00100700
100800          UNTIL GCG-A-INDEX > 255                                 00100800
100900             OR FIELD-FND                                         00100900
101000             IF GCG-A-DE-ID (GCG-A-INDEX) = QCF-FUNC-FIELD        00101000
101100                MOVE GCG-A-DE-TEXT (GCG-A-INDEX)                  00101100
101200                                          TO RPT-FUNC-DESCR       00101200
101300                SET FIELD-FND TO TRUE                             00101300
101400             END-IF                                               00101400
101500        END-PERFORM                                               00101500
101600     END-IF.                                                      00101600
101700                                                                  00101700
101800 0580-EXIT.                                                       00101800
101900     EXIT.                                                        00101900
102000/                                                                 00102000
102100**************************************************************    00102100
102200*      DESCRIPTION FOR THE VARIOUS FUNCTION FIELDS           *    00102200
102300**************************************************************    00102300
102400 0590-FORMAT-CON-DSCRPTN.                                         00102400
102500                                                                  00102500
102600     MOVE SPACES TO RPT-TRAILER.                                  00102600
102700                                                                  00102700
102800     IF HLD-FUNC-FIELD = 'CNTKY000'                               00102800
102900        MOVE SPACES       TO RPT-TRAILER                          00102900
103000        MOVE 'CONTRACT KEY FIELD CHANGE'                          00103000
103100              TO RPT-CON-FUNC-DESCR                               00103100
103200        PERFORM 0800-CNTKY000-MOVE THRU 0800-EXIT                 00103200
103300        GO TO  0590-EXIT.                                         00103300
103400                                                                  00103400
103500     IF HLD-FUNC-FIELD = 'CNTKY001'                               00103500
103600        MOVE SPACES       TO RPT-TRAILER                          00103600
103700        MOVE 'CONTRACT MAPPED FROM KEY '                          00103700
103800              TO RPT-CON-FUNC-DESCR                               00103800
103900        PERFORM 0900-CNTKY001-MOVE THRU 0900-EXIT                 00103900
104000        GO TO  0590-EXIT.                                         00104000
104100                                                                  00104100
104200     IF HLD-FUNC-FIELD = 'CNTDL000'                               00104200
104300        MOVE SPACES       TO RPT-TRAILER                          00104300
104400        MOVE 'CONTRACT RECORD DELETED  '                          00104400
104500              TO RPT-CON-FUNC-DESCR                               00104500
104600        GO TO  0590-EXIT.                                         00104600
104700                                                                  00104700
104800 0590-EXIT.  EXIT.                                                00104800
104900/                                                                 00104900
105000**************************************************************    00105000
105100*      DESCRIPTION FOR THE VARIOUS FUNCTION FIELDS           *    00105100
105200**************************************************************    00105200
105300 0595-FORMAT-GPS-DSCRPTN.                                         00105300
105400                                                                  00105400
105500     IF HLD-FUNC-FIELD = 'GPSKY000'                               00105500
105600        MOVE SPACES       TO RPT-TRAILER                          00105600
105700        MOVE 'GRP SPEC KEY FIELD CHANGE'                          00105700
105800              TO RPT-GPS-FUNC-DESCR                               00105800
105900        PERFORM 1000-GPSKY000-MOVE THRU 1000-EXIT                 00105900
106000        GO TO  0595-EXIT.                                         00106000
106100                                                                  00106100
106200     IF HLD-FUNC-FIELD = 'GPSKY001'                               00106200
106300        MOVE SPACES       TO RPT-TRAILER                          00106300
106400        MOVE 'GRP SPEC MAPPED FROM KEY '                          00106400
106500              TO RPT-GPS-FUNC-DESCR                               00106500
106600        PERFORM 1100-GPSKY001-MOVE THRU 1100-EXIT                 00106600
106700        GO TO  0595-EXIT.                                         00106700
106800                                                                  00106800
106900     IF HLD-FUNC-FIELD = 'GPSDL000'                               00106900
107000        MOVE SPACES       TO RPT-TRAILER                          00107000
107100        MOVE 'GRP SPEC RECORD DELETED  '                          00107100
107200              TO RPT-GPS-FUNC-DESCR                               00107200
107300        GO TO  0595-EXIT.                                         00107300
107400                                                                  00107400
107500 0595-EXIT.  EXIT.                                                00107500
107600/                                                                 00107600
107700**************************************************************    00107700
107800*   WRITE THE PAGE HEADER AREA FOR THE REPORT                *    00107800
107900**************************************************************    00107900
108000 0600-WRITE-HEADER.                                               00108000
108100                                                                  00108100
108200     COMPUTE RPT-PAGE-COUNT = RPT-PAGE-COUNT + 1.                 00108200
108300     MOVE RPT-PAGE-COUNT  TO RPT-PAGE-CNT.                        00108300
108400     MOVE '2915'          TO RPT-ID-FIELD.                        00108400
108500     MOVE ZEROS           TO RPT-LINE-COUNT.                      00108500
108600     MOVE QCF-OPERATOR-ID TO RPT-OPERATOR-ID.                     00108600
108700                                                                  00108700
108800     WRITE RPT-RECORD FROM RPT-MAIN-HDR-L1                        00108800
108900        AFTER ADVANCING PAGE.                                     00108900
109000                                                                  00109000
109100     WRITE RPT-RECORD FROM RPT-MAIN-HDR-L2                        00109100
109200        AFTER ADVANCING 1 LINE.                                   00109200
109300                                                                  00109300
109400     WRITE RPT-RECORD FROM RPT-OPER-HDR-L1                        00109400
109500        AFTER ADVANCING 2 LINES.                                  00109500
109600                                                                  00109600
109700     WRITE RPT-RECORD FROM RPT-OPER-HDR-L2                        00109700
109800        AFTER ADVANCING 1 LINE.                                   00109800
109900                                                                  00109900
110000     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L1                      00110000
110100        AFTER ADVANCING 2 LINES.                                  00110100
110200                                                                  00110200
110300     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L2                      00110300
110400        AFTER ADVANCING 1 LINES.                                  00110400
110500                                                                  00110500
110600     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L3                      00110600
110700        AFTER ADVANCING 0 LINES.                                  00110700
110800                                                                  00110800
110900     ADD 9   TO RPT-LINE-COUNT.                                   00110900
111000                                                                  00111000
111100 0600-EXIT.                                                       00111100
111200     EXIT.                                                        00111200
111300/                                                                 00111300
111400**************************************************************    00111400
111500*   WRITE THE PAGE HEADER AREA FOR THE REPORT                *    00111500
111600**************************************************************    00111600
111700 0700-PROCESS-NAME-ONLY.                                          00111700
111800                                                                  00111800
111900     IF RND-OPERATOR-ID = HLD-NT-GOOD-NM-VALUE                    00111900
112000        GO TO 0700-EXIT.                                          00112000
112100                                                                  00112100
112200     MOVE CV-CODE-NAME    TO RPT-OPER-NAME.                       00112200
112300     MOVE CV-CODE-VALUE   TO RPT-OPERATOR-ID                      00112300
112400                             LAST-CODE-VALUE.                     00112400
112500     MOVE ZEROS TO RPT-LINE-COUNT.                                00112500
112600     COMPUTE RPT-PAGE-COUNT = RPT-PAGE-COUNT + 1.                 00112600
112700     MOVE RPT-PAGE-COUNT  TO RPT-PAGE-CNT.                        00112700
112800                                                                  00112800
112900     WRITE RPT-RECORD FROM RPT-MAIN-HDR-L1                        00112900
113000        AFTER ADVANCING PAGE.                                     00113000
113100                                                                  00113100
113200     WRITE RPT-RECORD FROM RPT-MAIN-HDR-L2                        00113200
113300        AFTER ADVANCING 1 LINE.                                   00113300
113400                                                                  00113400
113500     WRITE RPT-RECORD FROM RPT-OPER-HDR-L1                        00113500
113600        AFTER ADVANCING 2 LINES.                                  00113600
113700                                                                  00113700
113800     WRITE RPT-RECORD FROM RPT-OPER-HDR-L2                        00113800
113900        AFTER ADVANCING 1 LINE.                                   00113900
114000                                                                  00114000
114100     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L1                      00114100
114200        AFTER ADVANCING 2 LINES.                                  00114200
114300                                                                  00114300
114400     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L2                      00114400
114500        AFTER ADVANCING 1 LINES.                                  00114500
114600                                                                  00114600
114700     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L3                      00114700
114800        AFTER ADVANCING 0 LINES.                                  00114800
114900                                                                  00114900
115000     WRITE RPT-RECORD FROM RPT-OPER-LINE                          00115000
115100        AFTER ADVANCING 2 LINES.                                  00115100
115200                                                                  00115200
115300     ADD 10  TO RPT-LINE-COUNT.                                   00115300
115400                                                                  00115400
115500 0700-EXIT.                                                       00115500
115600     EXIT.                                                        00115600
115700/                                                                 00115700
115800 0800-CNTKY000-MOVE.                                              00115800
115900                                                                  00115900
116000     MOVE QCF-GROUP-NO             TO RPT-BIM-CON-GRP-NO.         00116000
116100     MOVE QCF-SECTION-NO           TO RPT-BIM-CON-SECTN-NO.       00116100
116200     MOVE QCF-PKG-CODE             TO RPT-BIM-CON-PKG-CODE.       00116200
116300     MOVE QCF-L-O-B                TO RPT-BIM-CON-L-O-B.          00116300
116400     MOVE QCF-PROV-CTRL            TO RPT-BIM-CON-PROV-CTL.       00116400
116500     MOVE QCF-FAM-REL-LEVEL        TO RPT-BIM-CON-FAM-REL-LV.     00116500
116600     MOVE '/'                      TO SLASH-1 SLASH-2.            00116600
116700     PERFORM 1200-CONVERT-DATE     THRU 1200-EXIT.                00116700
116800     MOVE WS-M                     TO RPT-CON-EFF-DT-MM.          00116800
116900     MOVE WS-D                     TO RPT-CON-EFF-DT-DD.          00116900
117000     MOVE WS-Y                     TO RPT-CON-EFF-DT-YY.          00117000
117100                                                                  00117100
117200 0800-EXIT.  EXIT.                                                00117200
117300/                                                                 00117300
117400 0900-CNTKY001-MOVE.                                              00117400
117500                                                                  00117500
117600     MOVE QCF-BIM-CON-GRP-NUMBER    TO RPT-BIM-CON-GRP-NO.        00117600
117700     MOVE QCF-BIM-CON-SECTN-NUMBER  TO RPT-BIM-CON-SECTN-NO.      00117700
117800     MOVE QCF-BIM-CON-PKG-CODE      TO RPT-BIM-CON-PKG-CODE.      00117800
117900     MOVE QCF-BIM-CON-L-O-B         TO RPT-BIM-CON-L-O-B.         00117900
118000     MOVE QCF-BIM-CON-PROV-CTL      TO RPT-BIM-CON-PROV-CTL.      00118000
118100     MOVE QCF-BIM-CON-FAM-REL-LV    TO RPT-BIM-CON-FAM-REL-LV.    00118100
118200     MOVE '/'                       TO SLASH-1 SLASH-2.           00118200
118300     PERFORM 1200-CONVERT-DATE     THRU 1200-EXIT.                00118300
118400     MOVE WS-M                     TO RPT-CON-EFF-DT-MM.          00118400
118500     MOVE WS-D                     TO RPT-CON-EFF-DT-DD.          00118500
118600     MOVE WS-Y                     TO RPT-CON-EFF-DT-YY.          00118600
118700                                                                  00118700
118800 0900-EXIT.  EXIT.                                                00118800
118900/                                                                 00118900
119000 1000-GPSKY000-MOVE.                                              00119000
119100                                                                  00119100
119200     MOVE QCF-GROUP-NO             TO RPT-BIM-GPS-GRP-NO.         00119200
119300     MOVE QCF-SECTION-NO           TO RPT-BIM-GPS-SECTN-NO.       00119300
119400     MOVE QCF-PKG-CODE             TO RPT-BIM-GPS-PKG-CODE.       00119400
119500     MOVE QCF-FAM-REL-LEVEL        TO RPT-BIM-GPS-FAM-REL-LV.     00119500
119600     MOVE '/'                      TO SLASH-3 SLASH-4.            00119600
119700     PERFORM 1200-CONVERT-DATE     THRU 1200-EXIT.                00119700
119800     MOVE WS-M                     TO RPT-GPS-EFF-DT-MM.          00119800
119900     MOVE WS-D                     TO RPT-GPS-EFF-DT-DD.          00119900
120000     MOVE WS-Y                     TO RPT-GPS-EFF-DT-YY.          00120000
120100                                                                  00120100
120200 1000-EXIT.  EXIT.                                                00120200
120300/                                                                 00120300
120400 1100-GPSKY001-MOVE.                                              00120400
120500                                                                  00120500
120600     MOVE QCF-BIM-GSP-GRP-NUMBER    TO RPT-BIM-GPS-GRP-NO.        00120600
120700     MOVE QCF-BIM-GSP-SECTN-NUMBER  TO RPT-BIM-GPS-SECTN-NO.      00120700
120800     MOVE QCF-BIM-GRP-SPEC-PKG-CODE TO RPT-BIM-GPS-PKG-CODE.      00120800
120900     MOVE QCF-BIM-GRP-SPEC-F-R-LVL  TO RPT-BIM-GPS-FAM-REL-LV.    00120900
121000     MOVE '/'                       TO SLASH-3 SLASH-4.           00121000
121100     PERFORM 1200-CONVERT-DATE     THRU 1200-EXIT.                00121100
121200     MOVE WS-M                     TO RPT-GPS-EFF-DT-MM.          00121200
121300     MOVE WS-D                     TO RPT-GPS-EFF-DT-DD.          00121300
121400     MOVE WS-Y                     TO RPT-GPS-EFF-DT-YY.          00121400
121500                                                                  00121500
121600                                                                  00121600
121700 1100-EXIT.  EXIT.                                                00121700
121800/                                                                 00121800
121900 1200-CONVERT-DATE.                                               00121900
122000                                                                  00122000
122100******************************************************************00122100
122200*          THE FOLLOWING WILL MOVE RESPECTIVE EFFECTIVE DATES    *00122200
122300******************************************************************00122300
122400                                                                  00122400
122500     EVALUATE TRUE                                                00122500
122600       WHEN HLD-FUNC-FIELD = 'CNTKY000'                           00122600
122700          MOVE QCF-EFF-DATE              TO HLD-DATE-AREA         00122700
122800       WHEN HLD-FUNC-FIELD = 'CNTKY001'                           00122800
122900          MOVE QCF-BIM-CON-EFF-DT        TO HLD-DATE-AREA         00122900
123000       WHEN HLD-FUNC-FIELD = 'GPSKY000'                           00123000
123100          MOVE QCF-EFF-DATE              TO HLD-DATE-AREA         00123100
123200       WHEN HLD-FUNC-FIELD = 'GPSKY001'                           00123200
123300          MOVE QCF-BIM-GRP-SPEC-EFF-DT   TO HLD-DATE-AREA         00123300
123400     END-EVALUATE.                                                00123400
123500                                                                  00123500
123600     MOVE HLD-JUL                 TO WS-YYDDD.                    00123600
123700                                                                  00123700
123800     CALL 'TSGGREG'       USING WS-YYDDD                          00123800
123900                                WS-MDY.                           00123900
124000                                                                  00124000
124100 1200-EXIT.  EXIT.                                                00124100
124200/                                                                 00124200
124300 1400-CONV-TERM-DATE.                                             00124300
124400                                                                  00124400
124500***PHF                                                            00124500
124600     IF QCF-CONTRACT-GROUP-SP-IND = 'C'                           00124600
124700        IF QCF-FUNC-FIELD = 'CNTT0001'                            00124700
124800          MOVE QCF-BIM-FIELD     TO HLD-DATE-AREA-1               00124800
124900          MOVE HLD-JUL-1         TO WS-YYDDD                      00124900
125000          CALL 'TSGGREG'         USING WS-YYDDD                   00125000
125100                                       WS-MDY                     00125100
125200          MOVE WS-M              TO WS-TERM-DATE-MM               00125200
125300          MOVE WS-D              TO WS-TERM-DATE-DD               00125300
125400          MOVE WS-Y              TO WS-TERM-DATE-YY               00125400
125500          MOVE WS-TERM-DATE-FIELD  TO RPT-BIM-FIELD               00125500
125600          MOVE QCF-AIM-FIELD     TO HLD-DATE-AREA-1               00125600
125700          MOVE HLD-JUL-1         TO WS-YYDDD                      00125700
125800          CALL 'TSGGREG'         USING WS-YYDDD                   00125800
125900                                       WS-MDY                     00125900
126000          MOVE WS-M              TO WS-TERM-DATE-MM               00126000
126100          MOVE WS-D              TO WS-TERM-DATE-DD               00126100
126200          MOVE WS-Y              TO WS-TERM-DATE-YY               00126200
126300          MOVE WS-TERM-DATE-FIELD  TO RPT-AIM-FIELD               00126300
126400        ELSE                                                      00126400
126500          MOVE QCF-BIM-FIELD     TO RPT-BIM-FIELD                 00126500
126600          MOVE QCF-AIM-FIELD     TO RPT-AIM-FIELD                 00126600
126700        END-IF                                                    00126700
126800     ELSE                                                         00126800
126900        IF QCF-FUNC-FIELD = 'GPST0001'                            00126900
127000          MOVE QCF-BIM-FIELD     TO HLD-DATE-AREA-1               00127000
127100          MOVE HLD-JUL-1         TO WS-YYDDD                      00127100
127200          CALL 'TSGGREG'         USING WS-YYDDD                   00127200
127300                                       WS-MDY                     00127300
127400          MOVE WS-M              TO WS-TERM-DATE-MM               00127400
127500          MOVE WS-D              TO WS-TERM-DATE-DD               00127500
127600          MOVE WS-Y              TO WS-TERM-DATE-YY               00127600
127700          MOVE WS-TERM-DATE-FIELD  TO RPT-BIM-FIELD               00127700
127800          MOVE QCF-AIM-FIELD     TO HLD-DATE-AREA-1               00127800
127900          MOVE HLD-JUL-1         TO WS-YYDDD                      00127900
128000          CALL 'TSGGREG'         USING WS-YYDDD                   00128000
128100                                       WS-MDY                     00128100
128200          MOVE WS-M              TO WS-TERM-DATE-MM               00128200
128300          MOVE WS-D              TO WS-TERM-DATE-DD               00128300
128400          MOVE WS-Y              TO WS-TERM-DATE-YY               00128400
128500          MOVE WS-TERM-DATE-FIELD  TO RPT-AIM-FIELD               00128500
128600        ELSE                                                      00128600
128700          MOVE QCF-BIM-FIELD     TO RPT-BIM-FIELD                 00128700
128800          MOVE QCF-AIM-FIELD     TO RPT-AIM-FIELD                 00128800
128900        END-IF                                                    00128900
129000      END-IF.                                                     00129000
129100                                                                  00129100
129200 1400-EXIT.  EXIT.                                                00129200
129300/                                                                 00129300
129400 9999-ABEND-ROUTINE.                                              00129400
129500                                                                  00129500
129600     CALL 'TSGEND' USING ABEND-CODE.                              00129600
129700                                                                  00129700
129800 9999-EXIT.                                                       00129800
129900     EXIT.                                                        00129900
