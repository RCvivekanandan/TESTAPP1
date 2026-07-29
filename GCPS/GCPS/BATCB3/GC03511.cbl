000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID. GC03511.                                             00000200
000300 AUTHOR. JUNE PON.                                                00000300
000400                                                                  00000400
000500 INSTALLATION. HCSC-HCMS.                                         00000500
000600 DATE-WRITTEN.                                                    00000600
000700 DATE-COMPILED.                                                   00000700
000800******************************************************************00000800
000900*                                                                *00000900
001000*    THIS PROGRAM WILL WRITE A REPORT FOR DAILY QUALITY CONTROL  *00001000
001100*    USING A PARM TO ACCESS THE QUALITY CONTROL FILE FOR THE     *00001100
001200*    SPECIFIED BCBS PEARL.                                       *00001200
001300*                                                                *00001300
001400*    INPUT FILES:                                                *00001400
001500*       QCF-FILE - QUALITY CONTROL EXTRACT FILE                  *00001500
001600*       RND-FILE - WORK KEY FILE TO PROCESS  QCF-FILE            *00001600
001700*       NME-FILE - CODES VSAM FILE WITH THE OPERATOR NAMES       *00001700
001800*                                                                *00001800
001900*    OUTPUT FILES:                                               *00001900
002000*       NONE                                                     *00002000
002100*                                                                *00002100
002200*    OUTPUT REPORTS:                                             *00002200
002300*       RPT-FILE - THE REPORT THAT WILL BE CREATED FROM THE      *00002300
002400*                  RANDOMIZED FILE USING THE EXTRACT FILE DATA   *00002400
002500*                                                                *00002500
002600******************************************************************00002600
002700*                                                                *00002700
002800*      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *00002800
002900*      *-*         U P D A T E   H I S T O R Y         *-*       *00002900
003000*      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *00003000
003100*                                                                *00003100
003200* CHG #    DATE    BY              DESCRIPTION                   *00003200
003300* _____  ________  ___  ___________________________________      *00003300
003400*        03/06/00  JP    CREATED (ADAPTED PGM FROM GC03510)      *00003400
003500*                                                                *00003500
003600* D353   10/09/00  JP  SUPPORT NEW CONT/GS ARCHIVE COPAY FIELDS  *00003600
003700*                      CHANGED PERFORM VARYING UNTIL PARAMETERS: *00003700
003800*                      FROM:  GCT-A-INDEX      >   89 TIMES      *00003800
003900*                        TO:  GCT-A-INDEX      >   93 TIMES      *00003900
004000*                      FROM:  GCG-A-INDEX      >  224 TIMES      *00004000
004100*                        TO:  GCG-A-INDEX      >  249 TIMES      *00004100
004200*                                                                *00004200
004300* D356   03/23/01  JP  SUPPORT GROUP SPEC ARCHIVE ITS FIELDS     *00004300
004400*                      CHANGED PERFORM VARYING UNTIL PARAMETERS: *00004400
004500*                      FROM:  GCG-A-INDEX      >  249 TIMES      *00004500
004600*                        TO:  GCG-A-INDEX      >  252 TIMES      *00004600
004700*                                                                *00004700
004800* D365A  07/22/02  JP  SUPPORT GROUP SPEC ARCHIVE FOR NEW FIELD: *00004800
004900*                      GCG-ACCM-REL-IND                          *00004900
005000*                      CHANGED PERFORM VARYING UNTIL PARAMETERS: *00005000
005100*                      FROM:  GCG-A-INDEX      >  252 TIMES      *00005100
005200*                        TO:  GCG-A-INDEX      >  253 TIMES      *00005200
005300*                                                                *00005300
005400* D374   04/01/03  GF  SUPPORT GROUP SPEC ARCHIVE FOR NEW FIELD: *00005400
005500*                      GCG-CONS-DRVN-IND                         *00005500
005600*                      GCG-CONS-DRVN-PENALTY-DATE                *00005600
005700*                      CHANGED PERFORM VARYING UNTIL PARAMETERS: *00005700
005800*                      FROM:  GCG-A-INDEX      >  253 TIMES      *00005800
005900*                        TO:  GCG-A-INDEX      >  255 TIMES      *00005900
006000*                                                                *00006000
006100* DM9400 05/30/07  LR  MODIFIED TO SUPPORT #GMFH TABULAR:        *00006100
006200*         IN GROUP SPECIFIC ENTRY CHANGED                        *00006200
006300*         FROM:     WT-02-DESCRIPTION-VALUES    OCCURS 048 TIMES *00006300
006400*           TO:     WT-02-DESCRIPTION-VALUES    OCCURS 057 TIMES *00006400
006500*         IN CONTRACT ENTRY CHANGED                              *00006500
006600*         FROM:     WT-02-DESCRIPTION-VALUES    OCCURS 021 TIMES *00006600
006700*           TO:     WT-02-DESCRIPTION-VALUES    OCCURS 022 TIMES *00006700
006800*                                                                *00006800
006800* P21681 10/23/17 SRI RECOMPILE FOR CB GCARCHGS                  *00006810
      *                                                                *00006820
      *        06/26/22 TEB RECOMPILE FOR COPYBOOK GCARCHCT CHANGE     *00006830
      *                                                                *00006831
      *        07/18/24 TEB RECOMPILE FOR COPYBOOK GCARCHCT CHANGE     *00006840
TM0526*BBDA-66049 04/10/26 TM    MODIFIED TO SUPPORT #GHPA TABULAR:    *00006850
TM0526*         IN GROUP SPECIFIC ENTRY CHANGED                        *00006860
TM0526*         FROM:     WT-02-DESCRIPTION-VALUES    OCCURS 057 TIMES *00006870
TM0526*           TO:     WT-02-DESCRIPTION-VALUES    OCCURS 058 TIMES *00006880
PG0626* P30337     06/15/26    PG   RECOMPILE -  GCARCHGS.             *00006890
006900******************************************************************00006900
007000/                                                                 00007000
007100 ENVIRONMENT DIVISION.                                            00007100
007200 CONFIGURATION SECTION.                                           00007200
007300 SOURCE-COMPUTER. IBM-370.                                        00007300
007400 OBJECT-COMPUTER. IBM-370.                                        00007400
007500                                                                  00007500
007600 INPUT-OUTPUT SECTION.                                            00007600
007700 FILE-CONTROL.                                                    00007700
007800                                                                  00007800
007900     SELECT QCF-FILE   ASSIGN TO UT-S-GC03511A.                   00007900
008000     SELECT RND-FILE   ASSIGN TO UT-S-GC03511B.                   00008000
008100     SELECT RPT-FILE   ASSIGN TO UT-S-GC03511C.                   00008100
008200/                                                                 00008200
008300 DATA DIVISION.                                                   00008300
008400 FILE SECTION.                                                    00008400
008500                                                                  00008500
008600 FD  QCF-FILE                                                     00008600
008700     LABEL RECORDS ARE STANDARD                                   00008700
008800     RECORDING MODE IS F                                          00008800
008900     BLOCK CONTAINS 0 RECORDS.                                    00008900
009000                                                                  00009000
009100 01  QCF-RECORD.                                                  00009100
009200     COPY GCQCF.                                                  00009200
009300                                                                  00009300
009400 FD  RND-FILE                                                     00009400
009500     LABEL RECORDS ARE STANDARD                                   00009500
009600     RECORDING MODE IS F                                          00009600
009700     BLOCK CONTAINS 0 RECORDS.                                    00009700
009800                                                                  00009800
009900 01  RND-RECORD.                                                  00009900
010000     05  RND-OPERATOR-ID  PIC X(08).                              00010000
010100     05  RND-PLAN-CODE    PIC X(03).                              00010100
010200     05  RND-GROUP-NO     PIC X(09).                              00010200
010300                                                                  00010300
010400 FD  RPT-FILE                                                     00010400
010500     LABEL RECORDS ARE STANDARD                                   00010500
010600     RECORDING MODE IS F                                          00010600
010700     BLOCK CONTAINS 0 RECORDS.                                    00010700
010800                                                                  00010800
010900 01  RPT-RECORD           PIC X(132).                             00010900
011000/                                                                 00011000
011100 WORKING-STORAGE SECTION.                                         00011100
011200 01  FILLER                    PIC X(24)   VALUE                  00011200
011300     'GC03511 WORKING-STORAGE'.                                   00011300
011400                                                                  00011400
011500 01  ABEND-CODE                PIC 9(4)    COMP.                  00011500
011600 01  WS-OPER-ID                PIC X(05) VALUE SPACES.            00011600
011700 01  NAME-RECS-READ            PIC 9(4) VALUE 0.                  00011700
011800                                                                  00011800
011900 01  WS-PEARL-IND              PIC X(8) VALUE SPACES.             00011900
012000     88  WS-ILLINOIS           VALUE 'ILBENANA'.                  00012000
012100     88  WS-TEXAS              VALUE 'TXBENANA'.                  00012100
012200                                                                  00012200
012300                                                                  00012300
012400 COPY HSCDATES.                                                   00012400
012500 01  DATE-TIME-WORK.                                              00012500
012600     05  TIME-AREA.                                               00012600
012700         10  TIME-HH           PIC XX.                            00012700
012800         10  TIME-MM           PIC XX.                            00012800
012900         10  TIME-SS           PIC XX.                            00012900
013000     05  WS-DATE-AREA.                                            00013000
013100         10  WS-MDY.                                              00013100
013200             15  WS-M          PIC 99    VALUE ZERO.              00013200
013300             15  WS-D          PIC 99    VALUE ZERO.              00013300
013400             15  WS-Y          PIC 99    VALUE ZERO.              00013400
013500         10  WS-YYDDD          PIC 9(5)  VALUE ZERO.              00013500
013600         10  FILLER REDEFINES WS-YYDDD.                           00013600
013700             15  WS-YY         PIC 99.                            00013700
013800             15  WS-DDD        PIC 999.                           00013800
013900         10  HOLD-CURR-DATE-JUL  PIC S9(5) COMP-3 VALUE ZEROS.    00013900
014000                                                                  00014000
014100 01  CEN-DATE.                                                    00014100
014200     05  CEN-DT.                                                  00014200
014300         10  CEN-CC            PIC 99.                            00014300
014400         10  CEN-YY            PIC 99.                            00014400
014500                                                                  00014500
014600 01  HLD-DATE.                                                    00014600
014700     05  HLD-DATE-AREA         PIC 9(07).                         00014700
014800     05  HLD-DATE-WORK REDEFINES HLD-DATE-AREA.                   00014800
014900         10  HLD-CEN           PIC 99.                            00014900
015000         10  HLD-JUL.                                             00015000
015100             15  HLD-YY        PIC 99.                            00015100
015200             15  HLD-DAYS      PIC 999.                           00015200
015300                                                                  00015300
015400*--PHF                                                            00015400
015500 01  HLD-DATE-1.                                                  00015500
015600     05  HLD-DATE-AREA-1       PIC 9(10).                         00015600
015700     05  HLD-DATE-WORK-1 REDEFINES HLD-DATE-AREA-1.               00015700
015800         10  FILLER            PIC 9(05).                         00015800
015900         10  HLD-JUL-1.                                           00015900
016000             15  HLD-YY-1      PIC 99.                            00016000
016100             15  HLD-DAYS-1    PIC 999.                           00016100
016200                                                                  00016200
016300 01  WS-TERM-DATE-FIELD.                                          00016300
016400     05  FILLER                PIC X(02).                         00016400
016500     05  WS-TERM-DATE.                                            00016500
016600         10  WS-TERM-DATE-MM   PIC X(02).                         00016600
016700         10  SLASH-7           PIC X(01) VALUE '/'.               00016700
016800         10  WS-TERM-DATE-DD   PIC X(02).                         00016800
016900         10  SLASH-8           PIC X(01) VALUE '/'.               00016900
017000         10  WS-TERM-DATE-YY   PIC X(02).                         00017000
017100*--                                                               00017100
017200                                                                  00017200
017300 01  DATE-AREA.                                                   00017300
017400     05  GREG-DATE             PIC 9(6)  VALUE ZEROS.             00017400
017500     05  JULIAN-DATE           PIC 9(5)  VALUE ZEROS.             00017500
017600     05  WS-CHNG-DATE          PIC 9(5)  VALUE ZEROS.             00017600
017700**** THE ABOVE DATE FORMAT IS YYDDD ***                           00017700
017800                                                                  00017800
017900                                                                  00017900
018000*01  WS-DATE                               PIC X(08) VALUE ZEROS. 00018000
018100 01  WS-DATE-CCYYDDD.                                             00018100
018200     05  WS-DATE-CC                        PIC X(02) VALUE ZEROS. 00018200
018300     05  WS-DATE-YYDDD.                                           00018300
018400         10  FILLER                        PIC X(01) VALUE ZEROS. 00018400
018500         10  WS-DATE-YY                    PIC X(02) VALUE ZEROS. 00018500
018600         10  WS-DATE-DDD                   PIC X(03) VALUE ZEROS. 00018600
018700                                                                  00018700
018800 01  WS-DATE-YYDDD-9                       PIC 9(06) VALUE ZEROS. 00018800
018900                                                                  00018900
019000 01  WS-DATE-MMDDCCYY.                                            00019000
019100     05  WS-DATE-MM                        PIC X(02) VALUE ZEROS. 00019100
019200     05  WS-DATE-DD                        PIC X(02) VALUE ZEROS. 00019200
019300     05  WS-DATE-CCYY                      PIC X(04) VALUE ZEROS. 00019300
019400*---                                                              00019400
019500/                                                                 00019500
019600 01  END-OF-FILE-RND           PIC XXX   VALUE SPACES.            00019600
019700     88  END-OF-RND-FILE           VALUE 'END'.                   00019700
019800                                                                  00019800
019900 01  END-OF-FILE-QCF           PIC XXX   VALUE SPACES.            00019900
020000     88  END-OF-QCF-FILE           VALUE 'END'.                   00020000
020100                                                                  00020100
020200 01  EOF-NAME-FILE             PIC X     VALUE 'N'.               00020200
020300                                                                  00020300
020400                                                                  00020400
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
028100     05  RPT-ID-FIELD          PIC X(05) VALUE SPACES.            00028100
028200     05  FILLER                PIC X(28).                         00028200
028300     05  FILLER                PIC X(26)                          00028300
028400           VALUE 'BLUE CROSS/BLUE SHIELD OF '.                    00028400
028500     05  RPT-PEARL-IND         PIC X(12) VALUE SPACES.            00028500
028600     05  FILLER                PIC X(27).                         00028600
028700     05  FILLER                PIC X(10)                          00028700
028800           VALUE 'PAGE:     '.                                    00028800
028900     05  RPT-PAGE-CNT          PIC ZZZ9.                          00028900
029000     05  FILLER                PIC X(04).                         00029000
029100                                                                  00029100
029200 01  RPT-MAIN-HDR-L2.                                             00029200
029300     05  FILLER                PIC X   VALUE SPACES.              00029300
029400     05  FILLER                PIC X(11)                          00029400
029500          VALUE 'RUN TIME : '.                                    00029500
029600     05  RPT-RUN-TIME.                                            00029600
029700         10  RPT-RUN-HR        PIC 99.                            00029700
029800         10  FILLER            PIC X  VALUE ':'.                  00029800
029900         10  RPT-RUN-MIN       PIC 99.                            00029900
030000         10  FILLER            PIC X  VALUE ':'.                  00030000
030100         10  RPT-RUN-SEC       PIC 99.                            00030100
030200     05  FILLER                PIC X(20).                         00030200
030300     05  FILLER                PIC X(52)                          00030300
030400     VALUE 'DAILY QUALITY CONTROL CONTRACT/GROUP SPECIFIC REPORT'.00030400
030500     05  FILLER                PIC X(22).                         00030500
030600     05  FILLER                PIC X(10)                          00030600
030700          VALUE 'RUN DATE: '.                                     00030700
030800     05  RPT-RUN-DATE.                                            00030800
030900         10  RPT-RUN-MO        PIC 99.                            00030900
031000         10  FILLER            PIC X  VALUE '/'.                  00031000
031100         10  RPT-RUN-DY        PIC 99.                            00031100
031200         10  FILLER            PIC X  VALUE '/'.                  00031200
031300         10  RPT-RUN-YR        PIC 99.                            00031300
031400     05  FILLER                PIC X(04).                         00031400
031500                                                                  00031500
031600 01  RPT-OPER-HDR-L1.                                             00031600
031700     05  FILLER                PIC X   VALUE SPACES.              00031700
031800     05  FILLER                PIC X(16)                          00031800
031900           VALUE 'OPERATOR NAME:  '.                              00031900
032000     05  RPT-OPER-NAME         PIC X(50).                         00032000
032100     05  FILLER                PIC X(65).                         00032100
032200                                                                  00032200
032300 01  RPT-OPER-HDR-L2.                                             00032300
032400     05  FILLER                PIC X   VALUE SPACES.              00032400
032500     05  FILLER                PIC X(16)                          00032500
032600           VALUE 'OPERATOR ID:    '.                              00032600
032700     05  RPT-OPERATOR-ID       PIC X(08).                         00032700
032800     05  FILLER                PIC X(107).                        00032800
032900                                                                  00032900
033000 01  RPT-DETAIL-HDR-L1.                                           00033000
033100     05  FILLER                PIC X   VALUE SPACES.              00033100
033200     05  FILLER                PIC X(17)                          00033200
033300           VALUE 'PLAN  GROUP     '.                              00033300
033400     05  FILLER                PIC X(15)                          00033400
033500           VALUE 'SECTION   PKG  '.                               00033500
033600     05  FILLER                PIC X(28)                          00033600
033700           VALUE '     PROV      EFFECTIVE    '.                  00033700
033800     05  FILLER                PIC X(16)                          00033800
033900           VALUE 'FUNC        FUNC'.                              00033900
034000     05  FILLER                PIC X(33) VALUE SPACES.            00034000
034100     05  FILLER                PIC X(22)                          00034100
034200           VALUE 'BEFORE      AFTER     '.                        00034200
034300                                                                  00034300
034400 01  RPT-DETAIL-HDR-L2.                                           00034400
034500     05  FILLER                PIC X   VALUE SPACES.              00034500
034600     05  FILLER                PIC X(17)                          00034600
034700           VALUE 'CODE  NUMBER     '.                             00034700
034800     05  FILLER                PIC X(15)                          00034800
034900           VALUE 'NUMBER    CODE '.                               00034900
035000     05  FILLER                PIC X(28)                          00035000
035100           VALUE 'LOB  CTRL FRL  DATE         '.                  00035100
035200     05  FILLER                PIC X(23)                          00035200
035300           VALUE 'DATE        DESCRIPTION'.                       00035300
035400     05  FILLER                PIC X(26) VALUE SPACES.            00035400
035500     05  FILLER                PIC X(22)                          00035500
035600           VALUE 'IMAGE       IMAGE     '.                        00035600
035700                                                                  00035700
035800 01  RPT-DETAIL-HDR-L3.                                           00035800
035900     05  FILLER                PIC X     VALUE SPACES.            00035900
036000     05  FILLER                PIC X(04) VALUE ALL '_'.           00036000
036100     05  FILLER                PIC X(02).                         00036100
036200     05  FILLER                PIC X(09) VALUE ALL '_'.           00036200
036300     05  FILLER                PIC X(02).                         00036300
036400     05  FILLER                PIC X(06) VALUE ALL '_'.           00036400
036500     05  FILLER                PIC X(04).                         00036500
036600     05  FILLER                PIC X(03) VALUE ALL '_'.           00036600
036700     05  FILLER                PIC X(02).                         00036700
036800     05  FILLER                PIC X(03) VALUE ALL '_'.           00036800
036900     05  FILLER                PIC X(02).                         00036900
037000     05  FILLER                PIC X(04) VALUE ALL '_'.           00037000
037100     05  FILLER                PIC X(01).                         00037100
037200     05  FILLER                PIC X(03) VALUE ALL '_'.           00037200
037300     05  FILLER                PIC X(02).                         00037300
037400     05  FILLER                PIC X(10) VALUE ALL '_'.           00037400
037500     05  FILLER                PIC X(03).                         00037500
037600     05  FILLER                PIC X(10) VALUE ALL '_'.           00037600
037700     05  FILLER                PIC X(02).                         00037700
037800     05  FILLER                PIC X(11) VALUE ALL '_'.           00037800
037900     05  FILLER                PIC X(26).                         00037900
038000     05  FILLER                PIC X(07) VALUE ALL '_'.           00038000
038100     05  FILLER                PIC X(05).                         00038100
038200     05  FILLER                PIC X(07) VALUE ALL '_'.           00038200
038300     05  FILLER                PIC X(03).                         00038300
038400                                                                  00038400
038500 01  RPT-DETAIL-LINE.                                             00038500
038600     05  FILLER                PIC X(01).                         00038600
038700     05  RPT-PLAN-CODE         PIC X(03).                         00038700
038800     05  FILLER                PIC X(03).                         00038800
038900     05  RPT-GROUP-NO          PIC X(09).                         00038900
039000     05  FILLER                PIC X(02).                         00039000
039100     05  RPT-SECTION-NO        PIC X(05).                         00039100
039200     05  FILLER                PIC X(05).                         00039200
039300     05  RPT-PKG-CODE          PIC X(03).                         00039300
039400     05  FILLER                PIC X(02).                         00039400
039500     05  RPT-L-O-B             PIC X.                             00039500
039600     05  FILLER                PIC X(04).                         00039600
039700     05  RPT-PROV-CTRL         PIC X(02).                         00039700
039800     05  FILLER                PIC X(03).                         00039800
039900     05  RPT-FAM-REL-LEVEL     PIC X(02).                         00039900
040000     05  FILLER                PIC X(03).                         00040000
040100     05  RPT-EFF-DATE.                                            00040100
040200         10  RPT-EFFDT-MO      PIC 99.                            00040200
040300         10  FILLER            PIC X     VALUE '/'.               00040300
040400         10  RPT-EFFDT-DY      PIC 99.                            00040400
040500         10  FILLER            PIC X     VALUE '/'.               00040500
040600         10  RPT-EFFDT-YR      PIC 9999.                          00040600
040700     05  FILLER                PIC X(03).                         00040700
040800     05  RPT-FUNC-DATE.                                           00040800
040900         10  RPT-FUNCD-MO      PIC 99.                            00040900
041000         10  FILLER            PIC X     VALUE '/'.               00041000
041100         10  RPT-FUNCD-DY      PIC 99.                            00041100
041200         10  FILLER            PIC X     VALUE '/'.               00041200
041300         10  RPT-FUNCD-YR      PIC 9999.                          00041300
041400     05  FILLER                PIC X(02).                         00041400
041500     05  RPT-TRAILER.                                             00041500
041600         10  RPT-FUNC-DESCR        PIC X(35).                     00041600
041700         10  FILLER                PIC X(02).                     00041700
041800         10  RPT-BIM-FIELD         PIC X(10).                     00041800
041900         10  FILLER                PIC X(02).                     00041900
042000         10  RPT-AIM-FIELD         PIC X(10).                     00042000
042100         10  FILLER                PIC X(01).                     00042100
042200     05  RPT-MASS-MAP-CON REDEFINES                               00042200
042300         RPT-TRAILER.                                             00042300
042400         10  RPT-CON-FUNC-DESCR          PIC X(21).               00042400
042500         10  FILLER                      PIC X(02).               00042500
042600         10  RPT-BIM-CON-GRP-NO          PIC X(09).               00042600
042700         10  FILLER                      PIC X(01).               00042700
042800         10  RPT-BIM-CON-SECTN-NO        PIC X(05).               00042800
042900         10  FILLER                      PIC X(01).               00042900
043000         10  RPT-BIM-CON-PKG-CODE        PIC X(03).               00043000
043100         10  FILLER                      PIC X(01).               00043100
043200         10  RPT-BIM-CON-L-O-B           PIC X(01).               00043200
043300         10  FILLER                      PIC X(01).               00043300
043400         10  RPT-BIM-CON-PROV-CTL        PIC X(02).               00043400
043500         10  FILLER                      PIC X(01).               00043500
043600         10  RPT-BIM-CON-FAM-REL-LV      PIC X(02).               00043600
043700         10  FILLER                      PIC X(01).               00043700
043800         10  RPT-BIM-CON-EFF-DT.                                  00043800
043900             15  RPT-CON-EFF-DT-MM       PIC X(02).               00043900
044000             15  SLASH-1                 PIC X(01).               00044000
044100             15  RPT-CON-EFF-DT-DD       PIC X(02).               00044100
044200             15  SLASH-2                 PIC X(01).               00044200
044300             15  RPT-CON-EFF-DT-YY       PIC X(02).               00044300
044400         10  FILLER                      PIC X(01).               00044400
044500     05  RPT-MASS-MAP-GPS REDEFINES                               00044500
044600         RPT-TRAILER.                                             00044600
044700         10  RPT-GPS-FUNC-DESCR          PIC X(25).               00044700
044800         10  FILLER                      PIC X(02).               00044800
044900         10  RPT-BIM-GPS-GRP-NO          PIC X(09).               00044900
045000         10  FILLER                      PIC X(01).               00045000
045100         10  RPT-BIM-GPS-SECTN-NO        PIC X(05).               00045100
045200         10  FILLER                      PIC X(02).               00045200
045300         10  RPT-BIM-GPS-PKG-CODE        PIC X(03).               00045300
045400         10  FILLER                      PIC X(01).               00045400
045500         10  RPT-BIM-GPS-FAM-REL-LV      PIC X(02).               00045500
045600         10  FILLER                      PIC X(01).               00045600
045700         10  RPT-BIM-GPS-EFF-DT.                                  00045700
045800             15  RPT-GPS-EFF-DT-MM       PIC X(02).               00045800
045900             15  SLASH-3                 PIC X(01).               00045900
046000             15  RPT-GPS-EFF-DT-DD       PIC X(02).               00046000
046100             15  SLASH-4                 PIC X(01).               00046100
046200             15  RPT-GPS-EFF-DT-YY       PIC X(02).               00046200
046300*        10  FILLER                      PIC X(06).               00046300
046400                                                                  00046400
046500 01  RPT-OPER-LINE.                                               00046500
046600     05  FILLER                PIC X(11).                         00046600
046700     05  FILLER                PIC X(43)                          00046700
046800          VALUE '**  NOTHING TO REPORT FOR THIS OPERATOR  **'.    00046800
046900     05  FILLER                PIC X(78).                         00046900
047000                                                                  00047000
047100/                                                                 00047100
047200 01  WS-REC-LEN-AREA.                                             00047200
047300 COPY GCCDRLEN.                                                   00047300
047400/                                                                 00047400
047500******************************************************************00047500
047600****          SET PARAMETER FOR ALL VSAM FILES USED           ****00047600
047700******************************************************************00047700
047800 01  PARM-SET.                                                    00047800
047900     05  SET-RDW.                                                 00047900
048000         10  SET-RECORD-LENGTH             PIC 9(04) COMP VALUE 0.00048000
048100         10  SET-FEEDBACK-CODE             PIC 9(04) COMP VALUE 0.00048100
048200     05  SET-VALUE                         PIC 9(08) COMP VALUE 0.00048200
048300                                                                  00048300
048400******************************************************************00048400
048500****          VSAM CALL PARAMETERS                            ****00048500
048600******************************************************************00048600
048700 01  PARM-ONE.                                                    00048700
048800     05  RESERVED                          PIC 9(05) COMP VALUE 0.00048800
048900     05  RESERVED-X REDEFINES RESERVED.                           00048900
049000         10  REQUEST-TYPE                  PIC X(01).             00049000
049100         10  REQUEST-FILLER                PIC X(03).             00049100
049200                                                                  00049200
049300******************************************************************00049300
049400****          PARAMETERS FOR CODE/NAME RECORDS                ****00049400
049500******************************************************************00049500
049600 01  PARM-CV-NAME.                                                00049600
049700     05  CV-NAME-RDW.                                             00049700
049800         10  CVNM-RECORD-LENGTH            PIC 9(04) COMP VALUE 0.00049800
049900         10  CVNM-FEEDBACK-CODE            PIC 9(04) COMP VALUE 0.00049900
050000     COPY ELPCVC.                                                 00050000
050100/                                                                 00050100
050200                                                                  00050200
050300******************************************************************00050300
050400****          BIM/AIM CONTRACT FIELDS TABLE                   ****00050400
050500******************************************************************00050500
050600     COPY GCARCHCT.                                               00050600
050700                                                                  00050700
050800                                                                  00050800
050900******************************************************************00050900
051000****          BIM/AIM GROUP SPECIFIC FIELDS TABLE             ****00051000
051100******************************************************************00051100
051200     COPY GCARCHGS.                                               00051200
051300/                                                                 00051300
051400                                                                  00051400
051500******************************************************************00051500
051600****          TABULAR DESCRIPTION TABLES                      ****00051600
051700******************************************************************00051700
051800                                                                  00051800
051900*** GROUP SPECIFIC ***                                            00051900
052000     COPY GCGTABS.                                                00052000
052100     05  GRSP-DESCRIPTION-TABLE      REDEFINES                    00052100
TM0526         WT-02-DESCRIPTION-VALUES    OCCURS 058 TIMES             00052200
052300                                     INDEXED BY GRSP-INDEX.       00052300
052400         10  GRSP-ENTRY.                                          00052400
052500             15  GRSP-TAB-ID             PIC X(06).               00052500
052600             15  GRSP-TAB-DESCRIPTION    PIC X(25).               00052600
052700             15  GRSP-TAB-DELETE-PGM     PIC X(08).               00052700
052800             15  GRSP-TAB-ADD-PGM        PIC X(08).               00052800
052900/                                                                 00052900
053000*** CONTRACT ***                                                  00053000
053100     COPY GCCTABS.                                                00053100
053200     05  CONT-DESCRIPTION-TABLE      REDEFINES                    00053200
053300         WT-02-DESCRIPTION-VALUES    OCCURS 022 TIMES             00053300
053400                                     INDEXED BY CONT-INDEX.       00053400
053500         10  CONT-ENTRY.                                          00053500
053600             15  CONT-TAB-ID             PIC X(06).               00053600
053700             15  CONT-TAB-DESCRIPTION    PIC X(25).               00053700
053800             15  CONT-TAB-DELETE-PGM     PIC X(08).               00053800
053900             15  CONT-TAB-ADD-PGM        PIC X(08).               00053900
054000                                                                  00054000
054100 LINKAGE SECTION.                                                 00054100
054200                                                                  00054200
054300 01  PARM-AREA.                                                   00054300
054400     05  PARM-LENGTH                     PIC S9(04) COMP.         00054400
054500     05  PARM-PEARL-IND                  PIC X(08).               00054500
054600     05  FILLER                          PIC X(92).               00054600
054700                                                                  00054700
054800/                                                                 00054800
054900 PROCEDURE DIVISION USING PARM-AREA.                              00054900
055000 0000-MAINLINE.                                                   00055000
055100                                                                  00055100
055200     PERFORM 0010-SET-UP-PEARL-PARM   THRU 0010-EXIT.             00055200
055300     PERFORM 0025-OPEN-AND-INITIALIZE THRU 0025-EXIT.             00055300
055400     PERFORM 0090-READ-NAME-FILE      THRU 0090-EXIT.             00055400
055500     PERFORM 0100-READ-RND-FILE       THRU 0100-EXIT.             00055500
055600                                                                  00055600
055700     MOVE RND-OPERATOR-ID   TO LAST-OPERATOR.                     00055700
055800     MOVE RND-GROUP-NO      TO LAST-GROUP-NO.                     00055800
055900     PERFORM 0200-READ-QCF-FILE THRU 0200-EXIT.                   00055900
056000                                                                  00056000
056100     IF CV-CODE-VALUE < RND-OPERATOR-ID                           00056100
056200        PERFORM UNTIL                                             00056200
056300          (CV-CODE-VALUE > RND-OPERATOR-ID) OR                    00056300
056400          (CV-CODE-VALUE = RND-OPERATOR-ID)                       00056400
056500           IF GOOD-NAME                                           00056500
056600              PERFORM 0700-PROCESS-NAME-ONLY THRU 0700-EXIT       00056600
056700           END-IF                                                 00056700
056800           PERFORM 0090-READ-NAME-FILE    THRU 0090-EXIT          00056800
056900        END-PERFORM                                               00056900
057000     END-IF.                                                      00057000
057100                                                                  00057100
057200     PERFORM 0300-EVALUATE-RND-NAME THRU 0300-EXIT                00057200
057300         UNTIL END-OF-RND-FILE                                    00057300
057400            OR END-OF-QCF-FILE.                                   00057400
057500                                                                  00057500
057600     MOVE 'C' TO REQUEST-TYPE.                                    00057600
057700     CALL 'TSGVSAM1' USING PARM-ONE                               00057700
057800                           PARM-SET.                              00057800
057900                                                                  00057900
058000     IF  REQUEST-TYPE NOT = 'C'                                   00058000
058100         DISPLAY '*** INVALID CLOSE OF INPUT CODE FILE ***'       00058100
058200         MOVE SET-FEEDBACK-CODE TO ABEND-CODE                     00058200
058300                                   DISPLAY-FEEDBACK               00058300
058400         DISPLAY ' FEEDBACK CODE = ' DISPLAY-FEEDBACK             00058400
058500         GO TO 9999-ABEND-ROUTINE.                                00058500
058600                                                                  00058600
058700     CLOSE  QCF-FILE                                              00058700
058800            RND-FILE                                              00058800
058900            RPT-FILE.                                             00058900
059000     GOBACK.                                                      00059000
059100                                                                  00059100
059200 0000-EXIT.                                                       00059200
059300     EXIT.                                                        00059300
059400/                                                                 00059400
059500                                                                  00059500
059600**************************************************************    00059600
059700*  SET UP PARM PASSED FROM JCL TO INDICATE WHICH PEARL'S     *    00059700
059800*  NAME FILE TO PROCESS                                      *    00059800
059900**************************************************************    00059900
060000 0010-SET-UP-PEARL-PARM.                                          00060000
060100                                                                  00060100
060200     IF PARM-LENGTH = 0                                           00060200
060300        DISPLAY 'ABENDED ON PARM-LENGTH: ' PARM-LENGTH            00060300
060400        DISPLAY 'NO PARM ENTERED IN JCL'                          00060400
060500        GO TO 9999-ABEND-ROUTINE                                  00060500
060600     ELSE                                                         00060600
060700        MOVE PARM-PEARL-IND  TO  WS-PEARL-IND                     00060700
060800        IF WS-ILLINOIS                                            00060800
060900           MOVE 'ILLINOIS' TO RPT-PEARL-IND                       00060900
061000        END-IF                                                    00061000
061100        IF WS-TEXAS                                               00061100
061200           MOVE 'TEXAS   ' TO RPT-PEARL-IND                       00061200
061300        END-IF                                                    00061300
061400     END-IF.                                                      00061400
061500                                                                  00061500
061600                                                                  00061600
061700                                                                  00061700
061800 0010-EXIT.                                                       00061800
061900      EXIT.                                                       00061900
062000                                                                  00062000
062100**************************************************************    00062100
062200*   OPEN FILES AND INITIALIZE THE REPORT HEADING             *    00062200
062300**************************************************************    00062300
062400 0025-OPEN-AND-INITIALIZE.                                        00062400
062500                                                                  00062500
062600     OPEN   INPUT      QCF-FILE.                                  00062600
062700     OPEN   INPUT      RND-FILE.                                  00062700
062800     OPEN   OUTPUT     RPT-FILE.                                  00062800
062900                                                                  00062900
063000     MOVE 'S' TO REQUEST-TYPE.                                    00063000
063100     MOVE 8   TO SET-RECORD-LENGTH.                               00063100
063200     MOVE 3   TO SET-VALUE.                                       00063200
063300                                                                  00063300
063400     CALL 'TSGVSAM1' USING PARM-ONE                               00063400
063500                           PARM-SET.                              00063500
063600                                                                  00063600
063700     IF  REQUEST-TYPE NOT = 'S'                                   00063700
063800         DISPLAY '*** INVALID OPEN OF INPUT CODE FILE ***'        00063800
063900         MOVE SET-FEEDBACK-CODE TO ABEND-CODE                     00063900
064000                                   DISPLAY-FEEDBACK               00064000
064100         DISPLAY ' FEEDBACK CODE = ' DISPLAY-FEEDBACK             00064100
064200         GO TO 9999-ABEND-ROUTINE.                                00064200
064300                                                                  00064300
064400     MOVE WS-PEARL-IND   TO CV-RECORD-PREFIX                      00064400
064500                            LAST-PREFIX.                          00064500
064600     MOVE 1           TO CV-ELEMENT-NBR.                          00064600
064700     MOVE LOW-VALUES  TO CV-CODE-VALUE.                           00064700
064800     MOVE ZEROS       TO CV-CODE-DESC-SEQ.                        00064800
064900     MOVE 'P'         TO REQUEST-TYPE.                            00064900
065000     MOVE 27          TO SET-RECORD-LENGTH.                       00065000
065100     MOVE 3           TO SET-VALUE.                               00065100
065200                                                                  00065200
065300     CALL 'TSGVSAM1' USING PARM-ONE                               00065300
065400                           PARM-CV-NAME.                          00065400
065500                                                                  00065500
065600     IF  REQUEST-TYPE NOT = 'P'                                   00065600
065700         DISPLAY '*** INVALID START OF INPUT CODE FILE ***'       00065700
065800         MOVE SET-FEEDBACK-CODE TO ABEND-CODE                     00065800
065900                                   DISPLAY-FEEDBACK               00065900
066000         DISPLAY ' FEEDBACK CODE = ' DISPLAY-FEEDBACK             00066000
066100         GO TO 9999-ABEND-ROUTINE.                                00066100
066200                                                                  00066200
066300     PERFORM 0050-FRMT-DT-TIME  THRU 0050-EXIT.                   00066300
066400                                                                  00066400
066500 0025-EXIT.                                                       00066500
066600     EXIT.                                                        00066600
066700/                                                                 00066700
066800**************************************************************    00066800
066900*   FORMAT DATE AND TIME FOR REPORT HEADING                  *    00066900
067000**************************************************************    00067000
067100 0050-FRMT-DT-TIME.                                               00067100
067200                                                                  00067200
067300     ACCEPT WS-DATE FROM DATE.                                    00067300
067400     MOVE WS-DATE-MO  TO RPT-RUN-MO.                              00067400
067500     MOVE WS-DATE-DY  TO RPT-RUN-DY.                              00067500
067600     MOVE WS-DATE-YR  TO RPT-RUN-YR.                              00067600
067700                                                                  00067700
067800     ACCEPT WS-TIME FROM TIME.                                    00067800
067900     MOVE WS-TIME-HR  TO RPT-RUN-HR.                              00067900
068000     MOVE WS-TIME-MN  TO RPT-RUN-MIN.                             00068000
068100     MOVE WS-TIME-SC  TO RPT-RUN-SEC.                             00068100
068200                                                                  00068200
068300 0050-EXIT.                                                       00068300
068400     EXIT.                                                        00068400
068500/                                                                 00068500
068600**************************************************************    00068600
068700*   READ THE VSAM NAME FILE SEQUENTIALLY                     *    00068700
068800**************************************************************    00068800
068900 0090-READ-NAME-FILE.                                             00068900
069000                                                                  00069000
069100     MOVE 'G' TO REQUEST-TYPE.                                    00069100
069200     CALL 'TSGVSAM1' USING PARM-ONE                               00069200
069300                           PARM-CV-NAME.                          00069300
069400                                                                  00069400
069500     IF REQUEST-TYPE = '2'                                        00069500
069600        MOVE 'Y' TO EOF-NAME-FILE                                 00069600
069700        GO TO 0090-EXIT                                           00069700
069800     END-IF                                                       00069800
069900                                                                  00069900
070000     IF  REQUEST-TYPE NOT = 'G'                                   00070000
070100         DISPLAY '*** INVALID READ NEXT OF INPUT CODE FILE ***'   00070100
070200         MOVE SET-FEEDBACK-CODE TO ABEND-CODE                     00070200
070300                                   DISPLAY-FEEDBACK               00070300
070400         DISPLAY ' FEEDBACK CODE = ' DISPLAY-FEEDBACK             00070400
070500         GO TO 9999-ABEND-ROUTINE.                                00070500
070600                                                                  00070600
070700     MOVE CV-CODE-NAME  TO OPER-NAME.                             00070700
070800                                                                  00070800
070900     IF HLD-FRST-BYTE = '*'                                       00070900
071000        SET NT-GOOD-NAME TO TRUE                                  00071000
071100     ELSE                                                         00071100
071200        SET GOOD-NAME    TO TRUE.                                 00071200
071300                                                                  00071300
071400 0090-EXIT.                                                       00071400
071500     EXIT.                                                        00071500
071600/                                                                 00071600
071700**************************************************************    00071700
071800*   READ THE RANDOMIZED FILE                                 *    00071800
071900**************************************************************    00071900
072000 0100-READ-RND-FILE.                                              00072000
072100                                                                  00072100
072200     INITIALIZE RND-RECORD.                                       00072200
072300                                                                  00072300
072400     READ  RND-FILE                                               00072400
072500           AT END  MOVE 'END'  TO END-OF-FILE-RND                 00072500
072600                   GO TO 0100-EXIT.                               00072600
072700                                                                  00072700
072800     IF RND-OPERATOR-ID NOT = LAST-OPERATOR                       00072800
072900        SET OPER-OFF  TO TRUE.                                    00072900
073000                                                                  00073000
073100     SET GROUP-OFF TO TRUE.                                       00073100
073200                                                                  00073200
073300 0100-EXIT.                                                       00073300
073400     EXIT.                                                        00073400
073500/                                                                 00073500
073600**************************************************************    00073600
073700*   READ THE QUALITY CONTROL FILE                            *    00073700
073800**************************************************************    00073800
073900 0200-READ-QCF-FILE.                                              00073900
074000                                                                  00074000
074100     INITIALIZE QCF-QUAL-CTRL-RECORD.                             00074100
074200                                                                  00074200
074300     READ  QCF-FILE                                               00074300
074400           AT END  MOVE 'END'  TO END-OF-FILE-QCF                 00074400
074500                   GO TO 0200-EXIT.                               00074500
074600                                                                  00074600
074700 0200-EXIT.                                                       00074700
074800     EXIT.                                                        00074800
074900/                                                                 00074900
075000 0300-EVALUATE-RND-NAME.                                          00075000
075100**************************************************************    00075100
075200*   PROCESS THE RANDOMIZED, QUALITY CONTROL, AND CODE FILES  *    00075200
075300**************************************************************    00075300
075400                                                                  00075400
075500     IF NT-GOOD-NAME                                              00075500
075600        MOVE CV-CODE-VALUE   TO HLD-NT-GOOD-NM-VALUE              00075600
075700        IF EOF-NAME-FILE = 'N'                                    00075700
075800        PERFORM 0090-READ-NAME-FILE THRU 0090-EXIT                00075800
075900          UNTIL GOOD-NAME.                                        00075900
076000                                                                  00076000
076100     IF ((CV-CODE-VALUE < RND-OPERATOR-ID  AND                    00076100
076200          CV-RECORD-PREFIX = LAST-PREFIX)  OR                     00076200
076300         (CV-CODE-VALUE < RND-OPERATOR-ID  AND                    00076300
076400          CV-CODE-VALUE > LAST-OPERATOR))                         00076400
076500          IF CV-CODE-VALUE NOT = LAST-CODE-VALUE AND              00076500
076600             GOOD-NAME                                            00076600
076700               PERFORM 0700-PROCESS-NAME-ONLY THRU 0700-EXIT      00076700
076800          END-IF                                                  00076800
076900          IF (CV-RECORD-PREFIX NOT > LAST-PREFIX OR               00076900
077000             NT-GOOD-NAME) AND (EOF-NAME-FILE = 'N')              00077000
077100               PERFORM 0090-READ-NAME-FILE THRU 0090-EXIT         00077100
077200          END-IF                                                  00077200
077300*         GO TO 0300-EXIT                                         00077300
077400     ELSE                                                         00077400
077500         IF ((CV-CODE-VALUE = RND-OPERATOR-ID) OR                 00077500
077600             (CV-CODE-VALUE < RND-OPERATOR-ID  AND                00077600
077700              CV-RECORD-PREFIX = LAST-PREFIX)  OR                 00077700
077800             (CV-CODE-VALUE < RND-OPERATOR-ID  AND                00077800
077900              CV-RECORD-PREFIX NOT = LAST-PREFIX)) AND            00077900
078000              GOOD-NAME                                           00078000
078100              IF (CV-CODE-VALUE < RND-OPERATOR-ID     AND         00078100
078200                 CV-CODE-VALUE NOT = LAST-CODE-VALUE AND          00078200
078300                 CV-RECORD-PREFIX = LAST-PREFIX      AND          00078300
078400                 GOOD-NAME)                                       00078400
078500                   PERFORM 0700-PROCESS-NAME-ONLY THRU 0700-EXIT  00078500
078600              END-IF                                              00078600
078700              PERFORM 0400-PROCESS-RND-FILE THRU 0400-EXIT        00078700
078800                UNTIL  RND-OPERATOR-ID NOT = LAST-OPERATOR        00078800
078900                   OR  END-OF-RND-FILE                            00078900
079000                   OR  END-OF-QCF-FILE                            00079000
079100              MOVE RND-OPERATOR-ID  TO LAST-OPERATOR              00079100
079200*             GO TO 0300-EXIT                                     00079200
079300         ELSE                                                     00079300
079400             IF CV-CODE-VALUE > RND-OPERATOR-ID AND               00079400
079500                GOOD-NAME                                         00079500
079600                PERFORM 0400-PROCESS-RND-FILE THRU 0400-EXIT      00079600
079700                  UNTIL RND-OPERATOR-ID NOT = LAST-OPERATOR       00079700
079800                     OR END-OF-RND-FILE                           00079800
079900                     OR END-OF-QCF-FILE                           00079900
080000                MOVE RND-OPERATOR-ID  TO LAST-OPERATOR            00080000
080100*               GO TO 0300-EXIT                                   00080100
080200             END-IF                                               00080200
080300         END-IF                                                   00080300
080400     END-IF.                                                      00080400
080500                                                                  00080500
080600     IF  END-OF-RND-FILE OR                                       00080600
080700         END-OF-QCF-FILE                                          00080700
080800           PERFORM UNTIL CV-RECORD-PREFIX NOT = LAST-PREFIX       00080800
080900              OR EOF-NAME-FILE = 'Y'                              00080900
081000              PERFORM 0090-READ-NAME-FILE THRU 0090-EXIT          00081000
081100              IF GOOD-NAME AND                                    00081100
081200                 CV-RECORD-PREFIX = LAST-PREFIX                   00081200
081300                   PERFORM 0700-PROCESS-NAME-ONLY THRU 0700-EXIT  00081300
081400              END-IF                                              00081400
081500           END-PERFORM                                            00081500
081600     END-IF.                                                      00081600
081700                                                                  00081700
081800 0300-EXIT.                                                       00081800
081900     EXIT.                                                        00081900
082000/                                                                 00082000
082100 0400-PROCESS-RND-FILE.                                           00082100
082200**************************************************************    00082200
082300*   PROCESS THE RANDOMIZED FILE TO PRODUCE THE REPORT        *    00082300
082400**************************************************************    00082400
082500                                                                  00082500
082600     IF RND-OPERATOR-ID = QCF-OPERATOR-ID                         00082600
082700        IF RND-GROUP-NO = QCF-GROUP-NO                            00082700
082800           PERFORM 0500-WRITE-REPORT-LINE THRU 0500-EXIT          00082800
082900           PERFORM 0200-READ-QCF-FILE THRU 0200-EXIT              00082900
083000        ELSE                                                      00083000
083100           IF RND-GROUP-NO > QCF-GROUP-NO                         00083100
083200              PERFORM 0200-READ-QCF-FILE  THRU 0200-EXIT          00083200
083300           ELSE                                                   00083300
083400              IF RND-GROUP-NO < QCF-GROUP-NO                      00083400
083500                 MOVE RND-OPERATOR-ID  TO LAST-OPERATOR           00083500
083600                 PERFORM 0100-READ-RND-FILE  THRU 0100-EXIT       00083600
083700              END-IF                                              00083700
083800           END-IF                                                 00083800
083900        END-IF                                                    00083900
084000     ELSE                                                         00084000
084100        IF RND-OPERATOR-ID > QCF-OPERATOR-ID AND                  00084100
084200           NOT END-OF-QCF-FILE                                    00084200
084300            PERFORM 0200-READ-QCF-FILE  THRU 0200-EXIT            00084300
084400        ELSE                                                      00084400
084500            MOVE RND-OPERATOR-ID  TO LAST-OPERATOR                00084500
084600            PERFORM 0100-READ-RND-FILE  THRU 0100-EXIT            00084600
084700        END-IF                                                    00084700
084800     END-IF.                                                      00084800
084900                                                                  00084900
085000 0400-EXIT.                                                       00085000
085100     EXIT.                                                        00085100
085200/                                                                 00085200
085300**************************************************************    00085300
085400*   WRITE THE DETAIL LINE OF INFORMATION TO THE REPORT       *    00085400
085500**************************************************************    00085500
085600 0500-WRITE-REPORT-LINE.                                          00085600
085700                                                                  00085700
085800     IF RND-OPERATOR-ID = HLD-NT-GOOD-NM-VALUE                    00085800
085900        GO TO 0500-EXIT.                                          00085900
086000                                                                  00086000
086100     MOVE SPACES            TO RPT-TRAILER.                       00086100
086200                                                                  00086200
086300     MOVE QCF-SECTION-NO    TO RPT-SECTION-NO.                    00086300
086400     MOVE QCF-PKG-CODE      TO RPT-PKG-CODE.                      00086400
086500     MOVE QCF-L-O-B         TO RPT-L-O-B.                         00086500
086600     MOVE QCF-PROV-CTRL     TO RPT-PROV-CTRL.                     00086600
086700     MOVE QCF-FAM-REL-LEVEL TO RPT-FAM-REL-LEVEL.                 00086700
086800                                                                  00086800
086900     PERFORM 1400-CONV-TERM-DATE    THRU 1400-EXIT.               00086900
087000     PERFORM 0550-FIND-FUNC-DESCRP  THRU 0550-EXIT.               00087000
087100                                                                  00087100
087200                                                                  00087200
087300*    MOVE QCF-BIM-FIELD     TO RPT-BIM-FIELD.                     00087300
087400*    MOVE QCF-AIM-FIELD     TO RPT-AIM-FIELD.                     00087400
087500                                                                  00087500
087600     MOVE QCF-EFF-DATE      TO HLD-DATE-AREA.                     00087600
087700     MOVE HLD-JUL           TO WS-YYDDD.                          00087700
087800     CALL 'TSGGREG'         USING WS-YYDDD                        00087800
087900                                  WS-MDY.                         00087900
088000     MOVE WS-M              TO RPT-EFFDT-MO.                      00088000
088100     MOVE WS-D              TO RPT-EFFDT-DY.                      00088100
088200     MOVE WS-Y              TO CEN-YY.                            00088200
088300     MOVE HLD-CEN           TO CEN-CC                             00088300
088400     MOVE CEN-DT            TO RPT-EFFDT-YR.                      00088400
088500                                                                  00088500
088600     MOVE QCF-FUNC-DATE     TO HLD-DATE-AREA.                     00088600
088700     MOVE HLD-JUL           TO WS-YYDDD.                          00088700
088800     CALL 'TSGGREG'         USING WS-YYDDD                        00088800
088900                                  WS-MDY.                         00088900
089000     MOVE WS-M              TO RPT-FUNCD-MO.                      00089000
089100     MOVE WS-D              TO RPT-FUNCD-DY.                      00089100
089200     MOVE WS-Y              TO CEN-YY.                            00089200
089300     MOVE HLD-CEN           TO CEN-CC                             00089300
089400     MOVE CEN-DT            TO RPT-FUNCD-YR.                      00089400
089500                                                                  00089500
089600     IF CV-CODE-VALUE = RND-OPERATOR-ID                           00089600
089700        MOVE CV-CODE-VALUE  TO LAST-CODE-VALUE                    00089700
089800        MOVE CV-CODE-NAME   TO RPT-OPER-NAME                      00089800
089900     ELSE                                                         00089900
090000        IF (CV-CODE-VALUE > RND-OPERATOR-ID) OR                   00090000
090100           (CV-CODE-VALUE < RND-OPERATOR-ID) OR                   00090100
090200           (CV-RECORD-PREFIX NOT = LAST-PREFIX)                   00090200
090300            MOVE NO-NM-FND-MSG  TO RPT-OPER-NAME                  00090300
090400        END-IF                                                    00090400
090500     END-IF.                                                      00090500
090600                                                                  00090600
090700     IF RPT-LINE-COUNT > 52                                       00090700
090800        PERFORM 0600-WRITE-HEADER THRU 0600-EXIT                  00090800
090900        MOVE QCF-PLAN-CODE    TO RPT-PLAN-CODE                    00090900
091000        MOVE QCF-GROUP-NO     TO RPT-GROUP-NO                     00091000
091100        WRITE RPT-RECORD FROM RPT-DETAIL-LINE                     00091100
091200           AFTER ADVANCING 2 LINES                                00091200
091300        ADD 2   TO RPT-LINE-COUNT                                 00091300
091400        GO TO 0500-EXIT                                           00091400
091500     END-IF.                                                      00091500
091600                                                                  00091600
091700     IF OPER-OFF                                                  00091700
091800        PERFORM 0600-WRITE-HEADER THRU 0600-EXIT                  00091800
091900        MOVE QCF-PLAN-CODE    TO RPT-PLAN-CODE                    00091900
092000        MOVE QCF-GROUP-NO     TO RPT-GROUP-NO                     00092000
092100        WRITE RPT-RECORD FROM RPT-DETAIL-LINE                     00092100
092200           AFTER ADVANCING 2 LINES                                00092200
092300        SET OPER-ON           TO TRUE                             00092300
092400        SET GROUP-ON          TO TRUE                             00092400
092500        ADD 2   TO RPT-LINE-COUNT                                 00092500
092600        GO TO 0500-EXIT                                           00092600
092700     ELSE                                                         00092700
092800        MOVE SPACES      TO RPT-PLAN-CODE                         00092800
092900     END-IF.                                                      00092900
093000                                                                  00093000
093100     IF GROUP-ON                                                  00093100
093200        MOVE SPACES        TO RPT-GROUP-NO                        00093200
093300        WRITE RPT-RECORD FROM RPT-DETAIL-LINE                     00093300
093400           AFTER ADVANCING 1 LINES                                00093400
093500        ADD 1   TO RPT-LINE-COUNT                                 00093500
093600     ELSE                                                         00093600
093700        MOVE QCF-GROUP-NO  TO RPT-GROUP-NO                        00093700
093800        WRITE RPT-RECORD FROM RPT-DETAIL-LINE                     00093800
093900           AFTER ADVANCING 2 LINES                                00093900
094000        SET GROUP-ON       TO TRUE                                00094000
094100        ADD 2   TO RPT-LINE-COUNT                                 00094100
094200     END-IF.                                                      00094200
094300                                                                  00094300
094400 0500-EXIT.                                                       00094400
094500     EXIT.                                                        00094500
094600/                                                                 00094600
094700**************************************************************    00094700
094800*   DETERMINE WITH FUNCTION DESCRIPTION SHOULD BE USED       *    00094800
094900**************************************************************    00094900
095000 0550-FIND-FUNC-DESCRP.                                           00095000
095100                                                                  00095100
095200     MOVE QCF-FUNC-FIELD    TO HLD-FUNC-FIELD.                    00095200
095300                                                                  00095300
095400     EVALUATE HLD-TWO-BYTES                                       00095400
095500        WHEN  'TT'                                                00095500
095600             PERFORM 0560-SEARCH-TABS-TABLE  THRU 0560-EXIT       00095600
095700        WHEN  'BB'                                                00095700
095800             PERFORM 0570-FORMAT-BEN-DSCRPTN THRU 0570-EXIT       00095800
095900        WHEN  OTHER                                               00095900
096000             PERFORM 0580-SEARCH-BIMAIM-TBL  THRU 0580-EXIT       00096000
096100     END-EVALUATE.                                                00096100
096200                                                                  00096200
096300     EVALUATE HLD-FIVE-BYTES                                      00096300
096400        WHEN  'CNTKY'                                             00096400
096500             PERFORM 0590-FORMAT-CON-DSCRPTN THRU 0590-EXIT       00096500
096600        WHEN  'CNTDL'                                             00096600
096700             PERFORM 0590-FORMAT-CON-DSCRPTN THRU 0590-EXIT       00096700
096800        WHEN  'GPSKY'                                             00096800
096900             PERFORM 0595-FORMAT-GPS-DSCRPTN THRU 0595-EXIT       00096900
097000        WHEN  'GPSDL'                                             00097000
097100             PERFORM 0595-FORMAT-GPS-DSCRPTN THRU 0595-EXIT       00097100
097200     END-EVALUATE.                                                00097200
097300                                                                  00097300
097400 0550-EXIT.                                                       00097400
097500     EXIT.                                                        00097500
097600/                                                                 00097600
097700**************************************************************    00097700
097800*  SEARCH TABLES FOR CONTRACT AND GROUP SPECIFIC FOR THE     *    00097800
097900*  RIGHT DESCRIPTION TO BE USED FOR TABULARS                 *    00097900
098000**************************************************************    00098000
098100 0560-SEARCH-TABS-TABLE.                                          00098100
098200                                                                  00098200
098300     IF QCF-CONTRACT-GROUP-SP-IND = 'C'                           00098300
098400        SET TAB-NT-FND TO TRUE                                    00098400
098500        SET CONT-INDEX TO 1                                       00098500
098600        MOVE SPACES TO RPT-FUNC-DESCR                             00098600
098700        PERFORM VARYING CONT-INDEX FROM 1 BY 1                    00098700
098800         UNTIL CONT-INDEX > 19                                    00098800
098900            OR TAB-FND                                            00098900
099000            IF CONT-TAB-ID (CONT-INDEX) = HLD-FUNC-AREA           00099000
099100               MOVE CONT-TAB-DESCRIPTION (CONT-INDEX)             00099100
099200                                        TO RPT-FUNC-DESCR         00099200
099300               SET TAB-FND TO TRUE                                00099300
099400            END-IF                                                00099400
099500        END-PERFORM                                               00099500
099600     ELSE                                                         00099600
099700        SET TAB-NT-FND TO TRUE                                    00099700
099800        SET GRSP-INDEX TO 1                                       00099800
099900        MOVE SPACES TO RPT-FUNC-DESCR                             00099900
100000        PERFORM VARYING GRSP-INDEX FROM 1 BY 1                    00100000
100100         UNTIL GRSP-INDEX > 46                                    00100100
100200            OR TAB-FND                                            00100200
100300            IF GRSP-TAB-ID (GRSP-INDEX) = HLD-FUNC-AREA           00100300
100400               MOVE GRSP-TAB-DESCRIPTION (GRSP-INDEX)             00100400
100500                                        TO RPT-FUNC-DESCR         00100500
100600               SET TAB-FND TO TRUE                                00100600
100700            END-IF                                                00100700
100800        END-PERFORM                                               00100800
100900     END-IF.                                                      00100900
101000                                                                  00101000
101100 0560-EXIT.                                                       00101100
101200     EXIT.                                                        00101200
101300/                                                                 00101300
101400**************************************************************    00101400
101500*  FORMAT THE BENEFIT PROVISION DESCRIPTION                  *    00101500
101600**************************************************************    00101600
101700 0570-FORMAT-BEN-DSCRPTN.                                         00101700
101800                                                                  00101800
101900     MOVE QCF-FUNC-FIELD  TO HOLD-BEN-PROV.                       00101900
102000     MOVE HLD-BEN-AREA    TO BEN-PROV-OUT.                        00102000
102100                                                                  00102100
102200     MOVE BEN-DESCRIPTION TO RPT-FUNC-DESCR.                      00102200
102300                                                                  00102300
102400 0570-EXIT.                                                       00102400
102500     EXIT.                                                        00102500
102600/                                                                 00102600
102700**************************************************************    00102700
102800*  SEARCH TABLES FOR CONTRACT AND GROUP SPECIFIC FOR THE     *    00102800
102900*  RIGHT DESCRIPTION TO BE USED FOR BEFORE AND AFTER IMAGES  *    00102900
103000**************************************************************    00103000
103100 0580-SEARCH-BIMAIM-TBL.                                          00103100
103200                                                                  00103200
103300     IF QCF-CONTRACT-GROUP-SP-IND = 'C'                           00103300
103400        SET FIELD-NT-FND TO TRUE                                  00103400
103500        SET GCT-A-INDEX TO 1                                      00103500
103600        MOVE SPACES TO RPT-FUNC-DESCR                             00103600
103700        PERFORM VARYING GCT-A-INDEX FROM 1 BY 1                   00103700
103800          UNTIL GCT-A-INDEX > 89                                  00103800
103900             OR FIELD-FND                                         00103900
104000             IF GCT-A-DE-ID (GCT-A-INDEX) = QCF-FUNC-FIELD        00104000
104100                MOVE GCT-A-DE-TEXT (GCT-A-INDEX)                  00104100
104200                                          TO RPT-FUNC-DESCR       00104200
104300                SET FIELD-FND TO TRUE                             00104300
104400             END-IF                                               00104400
104500        END-PERFORM                                               00104500
104600     ELSE                                                         00104600
104700        SET FIELD-NT-FND TO TRUE                                  00104700
104800        SET GCG-A-INDEX TO 1                                      00104800
104900        MOVE SPACES TO RPT-FUNC-DESCR                             00104900
105000        PERFORM VARYING GCG-A-INDEX FROM 1 BY 1                   00105000
105100          UNTIL GCG-A-INDEX > 255                                 00105100
105200             OR FIELD-FND                                         00105200
105300             IF GCG-A-DE-ID (GCG-A-INDEX) = QCF-FUNC-FIELD        00105300
105400                MOVE GCG-A-DE-TEXT (GCG-A-INDEX)                  00105400
105500                                          TO RPT-FUNC-DESCR       00105500
105600                SET FIELD-FND TO TRUE                             00105600
105700             END-IF                                               00105700
105800        END-PERFORM                                               00105800
105900     END-IF.                                                      00105900
106000                                                                  00106000
106100 0580-EXIT.                                                       00106100
106200     EXIT.                                                        00106200
106300/                                                                 00106300
106400**************************************************************    00106400
106500*      DESCRIPTION FOR THE VARIOUS FUNCTION FIELDS           *    00106500
106600**************************************************************    00106600
106700 0590-FORMAT-CON-DSCRPTN.                                         00106700
106800                                                                  00106800
106900     MOVE SPACES TO RPT-TRAILER.                                  00106900
107000                                                                  00107000
107100     IF HLD-FUNC-FIELD = 'CNTKY000'                               00107100
107200        MOVE SPACES       TO RPT-TRAILER                          00107200
107300        MOVE 'CONTRACT KEY FIELD CHANGE'                          00107300
107400              TO RPT-CON-FUNC-DESCR                               00107400
107500        PERFORM 0800-CNTKY000-MOVE THRU 0800-EXIT                 00107500
107600        GO TO  0590-EXIT.                                         00107600
107700                                                                  00107700
107800     IF HLD-FUNC-FIELD = 'CNTKY001'                               00107800
107900        MOVE SPACES       TO RPT-TRAILER                          00107900
108000        MOVE 'CONTRACT MAPPED FROM KEY '                          00108000
108100              TO RPT-CON-FUNC-DESCR                               00108100
108200        PERFORM 0900-CNTKY001-MOVE THRU 0900-EXIT                 00108200
108300        GO TO  0590-EXIT.                                         00108300
108400                                                                  00108400
108500     IF HLD-FUNC-FIELD = 'CNTDL000'                               00108500
108600        MOVE SPACES       TO RPT-TRAILER                          00108600
108700        MOVE 'CONTRACT RECORD DELETED  '                          00108700
108800              TO RPT-CON-FUNC-DESCR                               00108800
108900        GO TO  0590-EXIT.                                         00108900
109000                                                                  00109000
109100 0590-EXIT.  EXIT.                                                00109100
109200/                                                                 00109200
109300**************************************************************    00109300
109400*      DESCRIPTION FOR THE VARIOUS FUNCTION FIELDS           *    00109400
109500**************************************************************    00109500
109600 0595-FORMAT-GPS-DSCRPTN.                                         00109600
109700                                                                  00109700
109800     IF HLD-FUNC-FIELD = 'GPSKY000'                               00109800
109900        MOVE SPACES       TO RPT-TRAILER                          00109900
110000        MOVE 'GRP SPEC KEY FIELD CHANGE'                          00110000
110100              TO RPT-GPS-FUNC-DESCR                               00110100
110200        PERFORM 1000-GPSKY000-MOVE THRU 1000-EXIT                 00110200
110300        GO TO  0595-EXIT.                                         00110300
110400                                                                  00110400
110500     IF HLD-FUNC-FIELD = 'GPSKY001'                               00110500
110600        MOVE SPACES       TO RPT-TRAILER                          00110600
110700        MOVE 'GRP SPEC MAPPED FROM KEY '                          00110700
110800              TO RPT-GPS-FUNC-DESCR                               00110800
110900        PERFORM 1100-GPSKY001-MOVE THRU 1100-EXIT                 00110900
111000        GO TO  0595-EXIT.                                         00111000
111100                                                                  00111100
111200     IF HLD-FUNC-FIELD = 'GPSDL000'                               00111200
111300        MOVE SPACES       TO RPT-TRAILER                          00111300
111400        MOVE 'GRP SPEC RECORD DELETED  '                          00111400
111500              TO RPT-GPS-FUNC-DESCR                               00111500
111600        GO TO  0595-EXIT.                                         00111600
111700                                                                  00111700
111800 0595-EXIT.  EXIT.                                                00111800
111900/                                                                 00111900
112000**************************************************************    00112000
112100*   WRITE THE PAGE HEADER AREA FOR THE REPORT                *    00112100
112200**************************************************************    00112200
112300 0600-WRITE-HEADER.                                               00112300
112400                                                                  00112400
112500     COMPUTE RPT-PAGE-COUNT = RPT-PAGE-COUNT + 1.                 00112500
112600     MOVE RPT-PAGE-COUNT  TO RPT-PAGE-CNT.                        00112600
112700     MOVE '2915D'         TO RPT-ID-FIELD.                        00112700
112800     MOVE ZEROS           TO RPT-LINE-COUNT.                      00112800
112900     MOVE QCF-OPERATOR-ID TO RPT-OPERATOR-ID.                     00112900
113000                                                                  00113000
113100     WRITE RPT-RECORD FROM RPT-MAIN-HDR-L1                        00113100
113200        AFTER ADVANCING PAGE.                                     00113200
113300                                                                  00113300
113400     WRITE RPT-RECORD FROM RPT-MAIN-HDR-L2                        00113400
113500        AFTER ADVANCING 1 LINE.                                   00113500
113600                                                                  00113600
113700     WRITE RPT-RECORD FROM RPT-OPER-HDR-L1                        00113700
113800        AFTER ADVANCING 2 LINES.                                  00113800
113900                                                                  00113900
114000     WRITE RPT-RECORD FROM RPT-OPER-HDR-L2                        00114000
114100        AFTER ADVANCING 1 LINE.                                   00114100
114200                                                                  00114200
114300     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L1                      00114300
114400        AFTER ADVANCING 2 LINES.                                  00114400
114500                                                                  00114500
114600     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L2                      00114600
114700        AFTER ADVANCING 1 LINES.                                  00114700
114800                                                                  00114800
114900     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L3                      00114900
115000        AFTER ADVANCING 0 LINES.                                  00115000
115100                                                                  00115100
115200     ADD 9   TO RPT-LINE-COUNT.                                   00115200
115300                                                                  00115300
115400 0600-EXIT.                                                       00115400
115500     EXIT.                                                        00115500
115600/                                                                 00115600
115700**************************************************************    00115700
115800*   WRITE THE PAGE HEADER AREA FOR THE REPORT                *    00115800
115900**************************************************************    00115900
116000 0700-PROCESS-NAME-ONLY.                                          00116000
116100                                                                  00116100
116200     IF RND-OPERATOR-ID = HLD-NT-GOOD-NM-VALUE                    00116200
116300        GO TO 0700-EXIT.                                          00116300
116400                                                                  00116400
116500     MOVE CV-CODE-NAME    TO RPT-OPER-NAME.                       00116500
116600     MOVE CV-CODE-VALUE   TO RPT-OPERATOR-ID                      00116600
116700                             LAST-CODE-VALUE.                     00116700
116800     MOVE '2915D'         TO RPT-ID-FIELD.                        00116800
116900     MOVE ZEROS TO RPT-LINE-COUNT.                                00116900
117000     COMPUTE RPT-PAGE-COUNT = RPT-PAGE-COUNT + 1.                 00117000
117100     MOVE RPT-PAGE-COUNT  TO RPT-PAGE-CNT.                        00117100
117200                                                                  00117200
117300     WRITE RPT-RECORD FROM RPT-MAIN-HDR-L1                        00117300
117400        AFTER ADVANCING PAGE.                                     00117400
117500                                                                  00117500
117600     WRITE RPT-RECORD FROM RPT-MAIN-HDR-L2                        00117600
117700        AFTER ADVANCING 1 LINE.                                   00117700
117800                                                                  00117800
117900     WRITE RPT-RECORD FROM RPT-OPER-HDR-L1                        00117900
118000        AFTER ADVANCING 2 LINES.                                  00118000
118100                                                                  00118100
118200     WRITE RPT-RECORD FROM RPT-OPER-HDR-L2                        00118200
118300        AFTER ADVANCING 1 LINE.                                   00118300
118400                                                                  00118400
118500     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L1                      00118500
118600        AFTER ADVANCING 2 LINES.                                  00118600
118700                                                                  00118700
118800     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L2                      00118800
118900        AFTER ADVANCING 1 LINES.                                  00118900
119000                                                                  00119000
119100     WRITE RPT-RECORD FROM RPT-DETAIL-HDR-L3                      00119100
119200        AFTER ADVANCING 0 LINES.                                  00119200
119300                                                                  00119300
119400     WRITE RPT-RECORD FROM RPT-OPER-LINE                          00119400
119500        AFTER ADVANCING 2 LINES.                                  00119500
119600                                                                  00119600
119700     ADD 10  TO RPT-LINE-COUNT.                                   00119700
119800                                                                  00119800
119900 0700-EXIT.                                                       00119900
120000     EXIT.                                                        00120000
120100/                                                                 00120100
120200 0800-CNTKY000-MOVE.                                              00120200
120300                                                                  00120300
120400     MOVE QCF-GROUP-NO             TO RPT-BIM-CON-GRP-NO.         00120400
120500     MOVE QCF-SECTION-NO           TO RPT-BIM-CON-SECTN-NO.       00120500
120600     MOVE QCF-PKG-CODE             TO RPT-BIM-CON-PKG-CODE.       00120600
120700     MOVE QCF-L-O-B                TO RPT-BIM-CON-L-O-B.          00120700
120800     MOVE QCF-PROV-CTRL            TO RPT-BIM-CON-PROV-CTL.       00120800
120900     MOVE QCF-FAM-REL-LEVEL        TO RPT-BIM-CON-FAM-REL-LV.     00120900
121000     MOVE '/'                      TO SLASH-1 SLASH-2.            00121000
121100     PERFORM 1200-CONVERT-DATE     THRU 1200-EXIT.                00121100
121200     MOVE WS-M                     TO RPT-CON-EFF-DT-MM.          00121200
121300     MOVE WS-D                     TO RPT-CON-EFF-DT-DD.          00121300
121400     MOVE WS-Y                     TO RPT-CON-EFF-DT-YY.          00121400
121500                                                                  00121500
121600 0800-EXIT.  EXIT.                                                00121600
121700/                                                                 00121700
121800 0900-CNTKY001-MOVE.                                              00121800
121900                                                                  00121900
122000     MOVE QCF-BIM-CON-GRP-NUMBER    TO RPT-BIM-CON-GRP-NO.        00122000
122100     MOVE QCF-BIM-CON-SECTN-NUMBER  TO RPT-BIM-CON-SECTN-NO.      00122100
122200     MOVE QCF-BIM-CON-PKG-CODE      TO RPT-BIM-CON-PKG-CODE.      00122200
122300     MOVE QCF-BIM-CON-L-O-B         TO RPT-BIM-CON-L-O-B.         00122300
122400     MOVE QCF-BIM-CON-PROV-CTL      TO RPT-BIM-CON-PROV-CTL.      00122400
122500     MOVE QCF-BIM-CON-FAM-REL-LV    TO RPT-BIM-CON-FAM-REL-LV.    00122500
122600     MOVE '/'                       TO SLASH-1 SLASH-2.           00122600
122700     PERFORM 1200-CONVERT-DATE     THRU 1200-EXIT.                00122700
122800     MOVE WS-M                     TO RPT-CON-EFF-DT-MM.          00122800
122900     MOVE WS-D                     TO RPT-CON-EFF-DT-DD.          00122900
123000     MOVE WS-Y                     TO RPT-CON-EFF-DT-YY.          00123000
123100                                                                  00123100
123200 0900-EXIT.  EXIT.                                                00123200
123300/                                                                 00123300
123400 1000-GPSKY000-MOVE.                                              00123400
123500                                                                  00123500
123600     MOVE QCF-GROUP-NO             TO RPT-BIM-GPS-GRP-NO.         00123600
123700     MOVE QCF-SECTION-NO           TO RPT-BIM-GPS-SECTN-NO.       00123700
123800     MOVE QCF-PKG-CODE             TO RPT-BIM-GPS-PKG-CODE.       00123800
123900     MOVE QCF-FAM-REL-LEVEL        TO RPT-BIM-GPS-FAM-REL-LV.     00123900
124000     MOVE '/'                      TO SLASH-3 SLASH-4.            00124000
124100     PERFORM 1200-CONVERT-DATE     THRU 1200-EXIT.                00124100
124200     MOVE WS-M                     TO RPT-GPS-EFF-DT-MM.          00124200
124300     MOVE WS-D                     TO RPT-GPS-EFF-DT-DD.          00124300
124400     MOVE WS-Y                     TO RPT-GPS-EFF-DT-YY.          00124400
124500                                                                  00124500
124600 1000-EXIT.  EXIT.                                                00124600
124700/                                                                 00124700
124800 1100-GPSKY001-MOVE.                                              00124800
124900                                                                  00124900
125000     MOVE QCF-BIM-GSP-GRP-NUMBER    TO RPT-BIM-GPS-GRP-NO.        00125000
125100     MOVE QCF-BIM-GSP-SECTN-NUMBER  TO RPT-BIM-GPS-SECTN-NO.      00125100
125200     MOVE QCF-BIM-GRP-SPEC-PKG-CODE TO RPT-BIM-GPS-PKG-CODE.      00125200
125300     MOVE QCF-BIM-GRP-SPEC-F-R-LVL  TO RPT-BIM-GPS-FAM-REL-LV.    00125300
125400     MOVE '/'                       TO SLASH-3 SLASH-4.           00125400
125500     PERFORM 1200-CONVERT-DATE     THRU 1200-EXIT.                00125500
125600     MOVE WS-M                     TO RPT-GPS-EFF-DT-MM.          00125600
125700     MOVE WS-D                     TO RPT-GPS-EFF-DT-DD.          00125700
125800     MOVE WS-Y                     TO RPT-GPS-EFF-DT-YY.          00125800
125900                                                                  00125900
126000                                                                  00126000
126100 1100-EXIT.  EXIT.                                                00126100
126200/                                                                 00126200
126300 1200-CONVERT-DATE.                                               00126300
126400                                                                  00126400
126500******************************************************************00126500
126600*          THE FOLLOWING WILL MOVE RESPECTIVE EFFECTIVE DATES    *00126600
126700******************************************************************00126700
126800                                                                  00126800
126900     EVALUATE TRUE                                                00126900
127000       WHEN HLD-FUNC-FIELD = 'CNTKY000'                           00127000
127100          MOVE QCF-EFF-DATE              TO HLD-DATE-AREA         00127100
127200       WHEN HLD-FUNC-FIELD = 'CNTKY001'                           00127200
127300          MOVE QCF-BIM-CON-EFF-DT        TO HLD-DATE-AREA         00127300
127400       WHEN HLD-FUNC-FIELD = 'GPSKY000'                           00127400
127500          MOVE QCF-EFF-DATE              TO HLD-DATE-AREA         00127500
127600       WHEN HLD-FUNC-FIELD = 'GPSKY001'                           00127600
127700          MOVE QCF-BIM-GRP-SPEC-EFF-DT   TO HLD-DATE-AREA         00127700
127800     END-EVALUATE.                                                00127800
127900                                                                  00127900
128000     MOVE HLD-JUL                 TO WS-YYDDD.                    00128000
128100                                                                  00128100
128200     CALL 'TSGGREG'       USING WS-YYDDD                          00128200
128300                                WS-MDY.                           00128300
128400                                                                  00128400
128500 1200-EXIT.  EXIT.                                                00128500
128600/                                                                 00128600
128700 1400-CONV-TERM-DATE.                                             00128700
128800                                                                  00128800
128900***PHF                                                            00128900
129000     IF QCF-CONTRACT-GROUP-SP-IND = 'C'                           00129000
129100        IF QCF-FUNC-FIELD = 'CNTT0001'                            00129100
129200          MOVE QCF-BIM-FIELD     TO HLD-DATE-AREA-1               00129200
129300          MOVE HLD-JUL-1         TO WS-YYDDD                      00129300
129400          CALL 'TSGGREG'         USING WS-YYDDD                   00129400
129500                                       WS-MDY                     00129500
129600          MOVE WS-M              TO WS-TERM-DATE-MM               00129600
129700          MOVE WS-D              TO WS-TERM-DATE-DD               00129700
129800          MOVE WS-Y              TO WS-TERM-DATE-YY               00129800
129900          MOVE WS-TERM-DATE-FIELD  TO RPT-BIM-FIELD               00129900
130000          MOVE QCF-AIM-FIELD     TO HLD-DATE-AREA-1               00130000
130100          MOVE HLD-JUL-1         TO WS-YYDDD                      00130100
130200          CALL 'TSGGREG'         USING WS-YYDDD                   00130200
130300                                       WS-MDY                     00130300
130400          MOVE WS-M              TO WS-TERM-DATE-MM               00130400
130500          MOVE WS-D              TO WS-TERM-DATE-DD               00130500
130600          MOVE WS-Y              TO WS-TERM-DATE-YY               00130600
130700          MOVE WS-TERM-DATE-FIELD  TO RPT-AIM-FIELD               00130700
130800        ELSE                                                      00130800
130900          MOVE QCF-BIM-FIELD     TO RPT-BIM-FIELD                 00130900
131000          MOVE QCF-AIM-FIELD     TO RPT-AIM-FIELD                 00131000
131100        END-IF                                                    00131100
131200     ELSE                                                         00131200
131300        IF QCF-FUNC-FIELD = 'GPST0001'                            00131300
131400          MOVE QCF-BIM-FIELD     TO HLD-DATE-AREA-1               00131400
131500          MOVE HLD-JUL-1         TO WS-YYDDD                      00131500
131600          CALL 'TSGGREG'         USING WS-YYDDD                   00131600
131700                                       WS-MDY                     00131700
131800          MOVE WS-M              TO WS-TERM-DATE-MM               00131800
131900          MOVE WS-D              TO WS-TERM-DATE-DD               00131900
132000          MOVE WS-Y              TO WS-TERM-DATE-YY               00132000
132100          MOVE WS-TERM-DATE-FIELD  TO RPT-BIM-FIELD               00132100
132200          MOVE QCF-AIM-FIELD     TO HLD-DATE-AREA-1               00132200
132300          MOVE HLD-JUL-1         TO WS-YYDDD                      00132300
132400          CALL 'TSGGREG'         USING WS-YYDDD                   00132400
132500                                       WS-MDY                     00132500
132600          MOVE WS-M              TO WS-TERM-DATE-MM               00132600
132700          MOVE WS-D              TO WS-TERM-DATE-DD               00132700
132800          MOVE WS-Y              TO WS-TERM-DATE-YY               00132800
132900          MOVE WS-TERM-DATE-FIELD  TO RPT-AIM-FIELD               00132900
133000        ELSE                                                      00133000
133100          MOVE QCF-BIM-FIELD     TO RPT-BIM-FIELD                 00133100
133200          MOVE QCF-AIM-FIELD     TO RPT-AIM-FIELD                 00133200
133300        END-IF                                                    00133300
133400      END-IF.                                                     00133400
133500                                                                  00133500
133600 1400-EXIT.  EXIT.                                                00133600
133700/                                                                 00133700
133800 9999-ABEND-ROUTINE.                                              00133800
133900                                                                  00133900
134000     CALL 'TSGEND' USING ABEND-CODE.                              00134000
134100                                                                  00134100
134200 9999-EXIT.                                                       00134200
134300     EXIT.                                                        00134300
