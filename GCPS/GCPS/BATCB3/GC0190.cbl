000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.       GC0190.                                        00000200
000300 AUTHOR.           DELORES FRY.                                   00000300
000400 INSTALLATION.     HCSC.                                          00000400
000500 DATE-WRITTEN.     JANUARY 1988.                                  00000500
000600                                                                  00000600
000700******************************************************************00000700
000800* O B S O L E T E   P R O G R A M *- N O T   U S E D *************00000800
000900******************************************************************00000900
001000*                                                                *00001000
001100*          GENERIC CONTRACT PROCESSING SYSTEM (GCPS)             *00001100
001200*                                                                *00001200
001300*  THIS PROGRAM CREATES THE GROUP SPECIFIC TABULAR SPLIT-FILE.   *00001300
001400*                                                                *00001400
001500* INPUT FILES:   1. TABULAR FILE (VSAM KEY SEQUENCED) -TSGVSAM1  *00001500
001600*                2. RELEASED GRP-SPEC  TABULAR FILE   -GC0190A   *00001600
001700*                                                                *00001700
001800* OUTPUT FILE:   INTERMEDIATE GROUP SPECIFIC TABULAR FILE TO     *00001800
001900*                BE SORTED  -SEQUENTIAL               -GC0190B   *00001900
002000*                                                                *00002000
002100*                                                                *00002100
002200*   PROCESSING FUNCTIONS:                                        *00002200
002300*   ---------------------                                        *00002300
002400*   1. THIS PROGRAM READS THE RELEASED GROUP SPECIFIC TABULAR    *00002400
002500*      FILE CREATED FROM THE WORK FILE.  THE TYPES OF GROUP      *00002500
002600*      SPECIFIC TABULARS ON THIS FILE ARE:                       *00002600
002700*      #GCCP, #GFSB, #GHOB, #GHOR, #GMDB, #GMDN, #GMOB, #GMOR,   *00002700
002800*      #GMPB, #GMPR, #GMSB, #GMSC, #GMSR, #GPAB, #GPAC, #GPAD,   *00002800
002900*      #GPAR, #GPPO, #GRID, #GVLF, #GVLP, #GWCD, #GMCS, #GRPO,   *00002900
003000*      #GVLG, #GVLH, #GVLQ, #GVLR, #GMCG, #GMCD, #GMCR           *00003000
003100*                                                                *00003100
003200*   2. A SEQUENTIAL OUTPUT FILE IS CREATED CONTAINING ONLY       *00003200
003300*      THE FOLLOWING GROUP SPECIFIC TABULARS RECORDS:            *00003300
003400*      #GCCP, #GFSB, #GHOB, #GHOR, #GMDB, #GMDN, #GMOB, #GMOR,   *00003400
003500*      #GMPB, #GMPR, #GMSB, #GMSC, #GMSR, #GPAB, #GPAC, #GPAD,   *00003500
003600*      #GPAR, #GPPO, #GRID, #GVLF, #GVLP, #GWCD, #GMCS, #GRPO,   *00003600
003700*      #GVLG, #GVLH, #GVLQ, #GVLR, #GMCG, #GMCD, #GMCR           *00003700
003800*                                                                *00003800
003900*      A RECORD IS ALSO CREATED CONTAINING THE TABULAR ID,       *00003900
004000*      THE (LAST) SLOT-NUMBER, AND LOW-VALUES IN THE BODY OF     *00004000
004100*      THE RECORD FOR EACH OF THE FOLLOWING TABULARS:            *00004100
004200*      #GCCP, #GFSB, #GHOB, #GHOR, #GMDB, #GMDN, #GMOB, #GMOR,   *00004200
004300*      #GMPB, #GMPR, #GMSB, #GMSC, #GMSR, #GPAB, #GPAC, #GPAD,   *00004300
004400*      #GPAR, #GPPO, #GRID, #GVLF, #GVLP, #GWCD, #GMCS, #GRPO,   *00004400
004500*      #GVLG, #GVLH, #GVLQ, #GVLR, #GMCG, #GMCD, #GMCR           *00004500
004600*                                                                *00004600
004700*                                                                *00004700
004800*                                                                *00004800
004900*   NOTE:                                                        *00004900
005000*   -----                                                        *00005000
005100*      THE FOLLOWING GROUP SPECIFIC TABULARS ARE NOT REFERENCED  *00005100
005200*      ON THE INQUIRY OR MAINTENANCE SCREENS.  THEREFORE, THEY   *00005200
005300*      ARE NOT INCLUDED IN THIS PROGRAM FOR PROCESSING:          *00005300
005400*          #GMSS, #GPAS, #GTG, #GFG                              *00005400
005500*                                                                *00005500
005600*                                                                *00005600
005700*   MODULES CALLED:                                              *00005700
005800*   ---------------                                              *00005800
005900*      'TSGEND'   - ABEND ROUTINE                                *00005900
006000*                                                                *00006000
006100******************************************************************00006100
006200/*****************************************************************00006200
006300*                                                                *00006300
006400*       ***-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00006400
006500*       *-*         U P D A T E   H I S T O R Y         *-*      *00006500
006600*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00006600
006700*                                                                *00006700
006800*                                                                *00006800
006900**-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION--------------- *00006900
007000*                                                                *00007000
007100*   D1009     1/04/88  FRY   CREATED THIS PROGRAM.......         *00007100
007200*                                                                *00007200
007300*   D199      8/15/89  RKH   ADDED 4 GRP SPEC TABULARS:          *00007300
007400*                            #GVLG  #GVLH  #GVLQ  #GVLR          *00007400
007500*                                                                *00007500
007600* 11138 05/23/90  ENW  ADDED CODE TO BYPASS RECORDS WHERE SLOT    00007600
007700*                      NUMBERS ARE GREATER THAN +99999. RECORDS   00007700
007800*                      WITH SLOTS GREATER THAN +99999 ARE FOR     00007800
007900*                      CHAINED RECORDS AND CANNOT BE RUN THROUGH  00007900
008000*                      BATCH.                                     00008000
008100*                                                                *00008100
008200*   D249     08/01/90  GDM   ADDED GRP SPEC TABULAR: #GMCG       *00008200
008300*                                                                *00008300
008400*   D249.01  08/22/90  APH   ADDED 2 NEW TABULARS: #GMCD, #GMCR  *00008400
008500*                                                                *00008500
008600*   11154     3/06/91  FRY   INCREASE RECORD AREAS IN:           *00008600
008700*            FILE SECTION:                                       *00008700
008800*                       INPUT-RECORD.                            *00008800
008900*                       FILLER   PIC  X(3994)  CHANGED TO  7799. *00008900
009000*                OUTPUT-RECORD   PIC  X(4064)  CHANGED TO  7869. *00009000
009100*            WORKING STORAGE SECTION:                            *00009100
009200*             VSAM-RECORD-AREA   PIC  X(4000)  CHANGED TO  7805. *00009200
009300*                       FILLER   PIC  X(3960)  CHNAGED TO  7765. *00009300
009400*                                                                *00009400
009500* D11836     05/30/91  BSO   ADDED GRP SPEC TABULAR: #GMCT       *00009500
009600*                                                                *00009600
009700* D-292   03/02/92  KJD  ADDED GRP SPEC TABULAR: #GMCS           *00009700
009800*                                                                *00009800
009900* 12730   12/05/92  ENW  ADDED GRP SPEC TABULAR: #GRPO           *00009900
010000*                                                                *00010000
010100* D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES #GHOR,     *00010100
010200*                        #GMCD, #GMCR, #GMOR, #GMPR, #GMSR,      *00010200
010300*                        #GPAD, #GPAR, #GRID, #GWCD.             *00010300
010400*                                                                *00010400
010500*  P02384    09/13/05  GDM  ADDED THE FOLLOWING GROUP SPECIFIC   *00010500
010600*                           TABULARS: #GFHC, #GFSA, #GHCA        *00010600
010700*                           #GHSA, #GLPF, #GLPH, #GWHC           *00010700
010800*                                                                *00010800
010900*  DM9400    05/04/07  LR   ADDED #GMFH GROUP SPECIFIC TABULAR   *00010900
011000*                                                                *00011000
010900*  P00893    07/17/11  ART  OCTOBER RELEASE - BLUE DISTINCTION   *00011010
011000*                           RECOMPILE ONLY.                      *00011020
011000*                                                                *00011030
011000*  9-19-2016   RECOMPLIE PGM EXPANDED OCCURS VALUE IN GCTGPPOC   *00011040
011000*  4-08-2026   RECOMPLIE BACK PRODUCTION VERSION                 *00011050
011100******************************************************************00011100
011200******************************************************************00011200
011300 ENVIRONMENT DIVISION.                                            00011300
011400                                                                  00011400
011500 CONFIGURATION SECTION.                                           00011500
011600 SOURCE-COMPUTER.  IBM-370.                                       00011600
011700 OBJECT-COMPUTER.  IBM-370.                                       00011700
011800                                                                  00011800
011900                                                                  00011900
012000 INPUT-OUTPUT SECTION.                                            00012000
012100                                                                  00012100
012200 FILE-CONTROL.                                                    00012200
012300     SELECT   INPUT-FILE          ASSIGN  TO   UT-S-GC0190A.      00012300
012400     SELECT   OUTPUT-FILE         ASSIGN  TO   UT-S-GC0190B.      00012400
012500                                                                  00012500
012600                                                                  00012600
012700 DATA DIVISION.                                                   00012700
012800                                                                  00012800
012900 FILE SECTION.                                                    00012900
013000 FD  INPUT-FILE                                                   00013000
013100         LABEL RECORDS ARE STANDARD                               00013100
013200         RECORDING MODE IS V                                      00013200
013300         BLOCK CONTAINS  0  RECORDS.                              00013300
013400                                                                  00013400
013500 01  INPUT-RECORD.                                                00013500
013600     05  FILLER                            PIC  X(64).            00013600
013700     05  WRK-BEN-TAB-PROV-DATA.                                   00013700
013800         10  WRK-TAB-PROV-ID               PIC  X(6).             00013800
013900         10  FILLER                        PIC  X(7799).          00013900
014000/                                                                 00014000
014100                                                                  00014100
014200 FD  OUTPUT-FILE                                                  00014200
014300         LABEL RECORDS ARE STANDARD                               00014300
014400         RECORDING MODE IS V                                      00014400
014500         BLOCK CONTAINS  0  RECORDS.                              00014500
014600                                                                  00014600
014700 01  OUTPUT-RECORD                         PIC  X(7869).          00014700
014800                                                                  00014800
014900 01  OUTPUT-LAST-RECORD.                                          00014900
015000     05  FILLER                            PIC  X(64).            00015000
015100     05  LAST-RECORD-KEY.                                         00015100
015200        10  LAST-KEY-ID                    PIC  X(06).            00015200
015300        10  LAST-KEY-SLOT-NO               PIC S9(07)   COMP-3.   00015300
015400                                                                  00015400
015500                                                                  00015500
015600 01  OUTPUT-GCCP-RECORD.                                          00015600
015700     05  FILLER                            PIC  X(64).            00015700
015800/                                                                 00015800
015900*    05  GSS-COST-CONT-RECORD.                                    00015900
016000     COPY  GCTGCCPC.                                              00016000
016100/                                                                 00016100
016200                                                                  00016200
016300 01  OUTPUT-GFSB-RECORD.                                          00016300
016400     05  FILLER                            PIC  X(64).            00016400
016500/                                                                 00016500
016600*    05  GSU-RECORD.                                              00016600
016700     COPY  GCTGFSBC.                                              00016700
016800/                                                                 00016800
016900                                                                  00016900
017000 01  OUTPUT-GHOB-RECORD.                                          00017000
017100     05  FILLER                            PIC  X(64).            00017100
017200/                                                                 00017200
017300*    05  GSA-RECORD.                                              00017300
017400     COPY  GCTGHOBC.                                              00017400
017500/                                                                 00017500
017600                                                                  00017600
017700 01  OUTPUT-GHOR-RECORD.                                          00017700
017800     05  FILLER                            PIC  X(64).            00017800
017900/                                                                 00017900
018000*    05  GSO-RECORD.                                              00018000
018100     COPY  GCTGHORC.                                              00018100
018200/                                                                 00018200
018300 01  OUTPUT-GMCD-RECORD.                                          00018300
018400     05  FILLER                            PIC  X(64).            00018400
018500     COPY  GCTGMCDC.                                              00018500
018600/                                                                 00018600
018700 01  OUTPUT-GMCG-RECORD.                                          00018700
018800     05  FILLER                            PIC  X(64).            00018800
018900     COPY  GCTGMCGC.                                              00018900
019000/                                                                 00019000
019100 01  OUTPUT-GMCR-RECORD.                                          00019100
019200     05  FILLER                            PIC  X(64).            00019200
019300     COPY  GCTGMCRC.                                              00019300
019400/                                                                 00019400
019500 01  OUTPUT-GMCS-RECORD.                                          00019500
019600     05  FILLER                            PIC  X(64).            00019600
019700     COPY  GCTGMCSC.                                              00019700
019800/                                                                 00019800
019900 01  OUTPUT-GRPO-RECORD.                                          00019900
020000     05  FILLER                            PIC  X(64).            00020000
020100     COPY  GCTGRPOC.                                              00020100
020200/                                                                 00020200
020300                                                                  00020300
020400 01  OUTPUT-GMCT-RECORD.                                          00020400
020500     05  FILLER                            PIC  X(64).            00020500
020600     COPY  GCTGMCTC.                                              00020600
020700/                                                                 00020700
020800                                                                  00020800
020900 01  OUTPUT-GMDB-RECORD.                                          00020900
021000     05  FILLER                            PIC  X(64).            00021000
021100/                                                                 00021100
021200*    05  GSV-RECORD.                                              00021200
021300     COPY  GCTGMDBC.                                              00021300
021400/                                                                 00021400
021500                                                                  00021500
021600 01  OUTPUT-GMDN-RECORD.                                          00021600
021700     05  FILLER                            PIC  X(64).            00021700
021800/                                                                 00021800
021900*    05  GS3-RECORD.                                              00021900
022000     COPY  GCTGMDNC.                                              00022000
022100/                                                                 00022100
022200                                                                  00022200
022300 01  OUTPUT-GMOB-RECORD.                                          00022300
022400     05  FILLER                            PIC  X(64).            00022400
022500/                                                                 00022500
022600*    05  GSG-RECORD.                                              00022600
022700     COPY  GCTGMOBC.                                              00022700
022800/                                                                 00022800
022900                                                                  00022900
023000 01  OUTPUT-GMOR-RECORD.                                          00023000
023100     05  FILLER                            PIC  X(64).            00023100
023200/                                                                 00023200
023300*    05  GSN-RECORD.                                              00023300
023400     COPY  GCTGMORC.                                              00023400
023500/                                                                 00023500
023600                                                                  00023600
023700 01  OUTPUT-GMPB-RECORD.                                          00023700
023800     05  FILLER                            PIC  X(64).            00023800
023900/                                                                 00023900
024000*    05  GSH-RECORD.                                              00024000
024100     COPY  GCTGMPBC.                                              00024100
024200/                                                                 00024200
024300                                                                  00024300
024400 01  OUTPUT-GMPR-RECORD.                                          00024400
024500     05  FILLER                            PIC  X(64).            00024500
024600/                                                                 00024600
024700*    05  GSR-RECORD.                                              00024700
024800     COPY  GCTGMPRC.                                              00024800
024900/                                                                 00024900
025000                                                                  00025000
025100 01  OUTPUT-GMSB-RECORD.                                          00025100
025200     05  FILLER                            PIC  X(64).            00025200
025300/                                                                 00025300
025400*    05  GSB-RECORD.                                              00025400
025500     COPY  GCTGMSBC.                                              00025500
025600/                                                                 00025600
025700                                                                  00025700
025800 01  OUTPUT-GMSC-RECORD.                                          00025800
025900     05  FILLER                            PIC  X(64).            00025900
026000/                                                                 00026000
026100*    05  GSL-RECORD.                                              00026100
026200     COPY  GCTGMSCC.                                              00026200
026300/                                                                 00026300
026400                                                                  00026400
026500 01  OUTPUT-GMSR-RECORD.                                          00026500
026600     05  FILLER                            PIC  X(64).            00026600
026700/                                                                 00026700
026800*    05  GSM-RECORD.                                              00026800
026900     COPY  GCTGMSRC.                                              00026900
027000/                                                                 00027000
027100                                                                  00027100
027200 01  OUTPUT-GPAB-RECORD.                                          00027200
027300     05  FILLER                            PIC  X(64).            00027300
027400/                                                                 00027400
027500*    05  GST-RECORD.                                              00027500
027600     COPY  GCTGPABC.                                              00027600
027700/                                                                 00027700
027800                                                                  00027800
027900 01  OUTPUT-GPAC-RECORD.                                          00027900
028000     05  FILLER                            PIC  X(64).            00028000
028100/                                                                 00028100
028200*    05  GSJ-RECORD.                                              00028200
028300     COPY  GCTGPACC.                                              00028300
028400/                                                                 00028400
028500                                                                  00028500
028600 01  OUTPUT-GPAD-RECORD.                                          00028600
028700     05  FILLER                            PIC  X(64).            00028700
028800/                                                                 00028800
028900*    05  GSC-RECORD.                                              00028900
029000     COPY  GCTGPADC.                                              00029000
029100/                                                                 00029100
029200                                                                  00029200
029300 01  OUTPUT-GPAR-RECORD.                                          00029300
029400     05  FILLER                            PIC  X(64).            00029400
029500/                                                                 00029500
029600*    05  GSK-RECORD.                                              00029600
029700     COPY  GCTGPARC.                                              00029700
029800/                                                                 00029800
029900                                                                  00029900
030000 01  OUTPUT-GPPO-RECORD.                                          00030000
030100     05  FILLER                            PIC  X(64).            00030100
030200/                                                                 00030200
030300*    05  GSW-RECORD.                                              00030300
030400     COPY  GCTGPPOC.                                              00030400
030500/                                                                 00030500
030600                                                                  00030600
030700 01  OUTPUT-GRID-RECORD.                                          00030700
030800     05  FILLER                            PIC  X(64).            00030800
030900/                                                                 00030900
031000*    05  GSD-RECORD.                                              00031000
031100     COPY  GCTGRIDC.                                              00031100
031200/                                                                 00031200
031300                                                                  00031300
031400 01  OUTPUT-GVLF-RECORD.                                          00031400
031500     05  FILLER                            PIC  X(64).            00031500
031600/                                                                 00031600
031700*    05  GSX-RECORD.                                              00031700
031800     COPY  GCTGVLFC.                                              00031800
031900/                                                                 00031900
032000 01  OUTPUT-GVLG-RECORD.                                          00032000
032100     05  FILLER                            PIC  X(64).            00032100
032200*    05  GSF-RECORD.                                              00032200
032300     COPY  GCTGVLGC.                                              00032300
032400/                                                                 00032400
032500 01  OUTPUT-GVLH-RECORD.                                          00032500
032600     05  FILLER                            PIC  X(64).            00032600
032700*    05  GSQ-RECORD.                                              00032700
032800     COPY  GCTGVLHC.                                              00032800
032900/                                                                 00032900
033000                                                                  00033000
033100 01  OUTPUT-GVLP-RECORD.                                          00033100
033200     05  FILLER                            PIC  X(64).            00033200
033300/                                                                 00033300
033400*    05  GSY-RECORD.                                              00033400
033500     COPY  GCTGVLPC.                                              00033500
033600/                                                                 00033600
033700 01  OUTPUT-GVLQ-RECORD.                                          00033700
033800     05  FILLER                            PIC  X(64).            00033800
033900*    05  GSP-RECORD.                                              00033900
034000     COPY  GCTGVLQC.                                              00034000
034100/                                                                 00034100
034200 01  OUTPUT-GVLR-RECORD.                                          00034200
034300     05  FILLER                            PIC  X(64).            00034300
034400*    05  GSZ-RECORD.                                              00034400
034500     COPY  GCTGVLRC.                                              00034500
034600/                                                                 00034600
034700 01  OUTPUT-GWCD-RECORD.                                          00034700
034800     05  FILLER                            PIC  X(64).            00034800
034900/                                                                 00034900
035000*    05  GSI-RECORD.                                              00035000
035100     COPY  GCTGWCDC.                                              00035100
035200/                                                                 00035200
035300 01  OUTPUT-GFHC-RECORD.                                          00035300
035400     05  FILLER                            PIC  X(64).            00035400
035500/                                                                 00035500
035600*    05  GFHC-RECORD.                                             00035600
035700     COPY  GCTGFHCC.                                              00035700
035800/                                                                 00035800
035900 01  OUTPUT-GFSA-RECORD.                                          00035900
036000     05  FILLER                            PIC  X(64).            00036000
036100/                                                                 00036100
036200*    05  GFSA-RECORD.                                             00036200
036300     COPY  GCTGFSAC.                                              00036300
036400/                                                                 00036400
036500 01  OUTPUT-GHCA-RECORD.                                          00036500
036600     05  FILLER                            PIC  X(64).            00036600
036700/                                                                 00036700
036800*    05  GHCA-RECORD.                                             00036800
036900     COPY  GCTGHCAC.                                              00036900
037000/                                                                 00037000
037100 01  OUTPUT-GHSA-RECORD.                                          00037100
037200     05  FILLER                            PIC  X(64).            00037200
037300/                                                                 00037300
037400*    05  GHSA-RECORD.                                             00037400
037500     COPY  GCTGHSAC.                                              00037500
037600/                                                                 00037600
037700 01  OUTPUT-GLPF-RECORD.                                          00037700
037800     05  FILLER                            PIC  X(64).            00037800
037900/                                                                 00037900
038000*    05  GLPF-RECORD.                                             00038000
038100     COPY  GCTGLPFC.                                              00038100
038200/                                                                 00038200
038300 01  OUTPUT-GLPH-RECORD.                                          00038300
038400     05  FILLER                            PIC  X(64).            00038400
038500/                                                                 00038500
038600*    05  GLPH-RECORD.                                             00038600
038700     COPY  GCTGLPHC.                                              00038700
038800/                                                                 00038800
038900 01  OUTPUT-GWHC-RECORD.                                          00038900
039000     05  FILLER                            PIC  X(64).            00039000
039100/                                                                 00039100
039200*    05  GWHC-RECORD.                                             00039200
039300     COPY  GCTGWHCC.                                              00039300
039400/                                                                 00039400
039500 01  OUTPUT-GMFH-RECORD.                                          00039500
039600     05  FILLER                            PIC  X(64).            00039600
039700/                                                                 00039700
039800*    05  GMFH-RECORD.                                             00039800
039900     COPY  GCTGMFHC.                                              00039900
040000/                                                                 00040000
040100                                                                  00040100
040200                                                                  00040200
040300 WORKING-STORAGE SECTION.                                         00040300
040400                                                                  00040400
040500 77  FILLER                           PIC  X(27)    VALUE         00040500
040600                                    '* GC0190 WORKING STORAGE *'. 00040600
040700                                                                  00040700
040800 01  WS-HOLD-AREAS.                                               00040800
040900     05  FILLER                       PIC  X(29)    VALUE         00040900
041000                                      '***  ABEND CODE  ***'.     00041000
041100     05  WS-ABEND-CODE                PIC  9(04)    VALUE 0  COMP.00041100
041200                                                                  00041200
041300                                                                  00041300
041400 01  WS-WORK-AREAS.                                               00041400
041500     05  FILLER                       PIC  X(28)    VALUE         00041500
041600                                      '***  WORK AREA  ***'.      00041600
041700     05  WS-LAST-SLOT       COMP-3    PIC S9(07)    VALUE +0.     00041700
041800     05  WS-INPUT-RELEASED-RECORDS    PIC  9(08)    VALUE ZEROES. 00041800
041900     05  WS-INPUT-VSAM-RECORDS        PIC  9(08)    VALUE ZEROES. 00041900
042000     05  WS-OUTPUT-RECORDS            PIC  9(08)    VALUE ZEROES. 00042000
042100                                                                  00042100
042200                                                                  00042200
042300 01  WS-SWITCHES.                                                 00042300
042400     05  FILLER                       PIC  X(27)    VALUE         00042400
042500                                      '***  SWITCHES  ***'.       00042500
042600     05  WS-END-OF-INPUT-FILE-SW      PIC  X(01)    VALUE '0'.    00042600
042700         88  WS-END-OF-INPUT-FILE-SW-ON             VALUE '1'.    00042700
042800     05  WS-VSAM-READ-SWITCH          PIC  X(01)    VALUE '0'.    00042800
042900     05  WS-GCCP-SWITCH               PIC  X(01)    VALUE '0'.    00042900
043000     05  WS-GFSB-SWITCH               PIC  X(01)    VALUE '0'.    00043000
043100     05  WS-GHOB-SWITCH               PIC  X(01)    VALUE '0'.    00043100
043200     05  WS-GHOR-SWITCH               PIC  X(01)    VALUE '0'.    00043200
043300     05  WS-GMCD-SWITCH               PIC  X(01)    VALUE '0'.    00043300
043400     05  WS-GMCG-SWITCH               PIC  X(01)    VALUE '0'.    00043400
043500     05  WS-GMCR-SWITCH               PIC  X(01)    VALUE '0'.    00043500
043600     05  WS-GMCS-SWITCH               PIC  X(01)    VALUE '0'.    00043600
043700     05  WS-GMCT-SWITCH               PIC  X(01)    VALUE '0'.    00043700
043800     05  WS-GMDB-SWITCH               PIC  X(01)    VALUE '0'.    00043800
043900     05  WS-GMDN-SWITCH               PIC  X(01)    VALUE '0'.    00043900
044000     05  WS-GMOB-SWITCH               PIC  X(01)    VALUE '0'.    00044000
044100     05  WS-GMOR-SWITCH               PIC  X(01)    VALUE '0'.    00044100
044200     05  WS-GMPB-SWITCH               PIC  X(01)    VALUE '0'.    00044200
044300     05  WS-GMPR-SWITCH               PIC  X(01)    VALUE '0'.    00044300
044400     05  WS-GMSB-SWITCH               PIC  X(01)    VALUE '0'.    00044400
044500     05  WS-GMSC-SWITCH               PIC  X(01)    VALUE '0'.    00044500
044600     05  WS-GMSR-SWITCH               PIC  X(01)    VALUE '0'.    00044600
044700     05  WS-GPAB-SWITCH               PIC  X(01)    VALUE '0'.    00044700
044800     05  WS-GPAC-SWITCH               PIC  X(01)    VALUE '0'.    00044800
044900     05  WS-GPAD-SWITCH               PIC  X(01)    VALUE '0'.    00044900
045000     05  WS-GPAR-SWITCH               PIC  X(01)    VALUE '0'.    00045000
045100     05  WS-GPPO-SWITCH               PIC  X(01)    VALUE '0'.    00045100
045200     05  WS-GRID-SWITCH               PIC  X(01)    VALUE '0'.    00045200
045300     05  WS-GRPO-SWITCH               PIC  X(01)    VALUE '0'.    00045300
045400     05  WS-GVLF-SWITCH               PIC  X(01)    VALUE '0'.    00045400
045500     05  WS-GVLG-SWITCH               PIC  X(01)    VALUE '0'.    00045500
045600     05  WS-GVLH-SWITCH               PIC  X(01)    VALUE '0'.    00045600
045700     05  WS-GVLP-SWITCH               PIC  X(01)    VALUE '0'.    00045700
045800     05  WS-GVLQ-SWITCH               PIC  X(01)    VALUE '0'.    00045800
045900     05  WS-GVLR-SWITCH               PIC  X(01)    VALUE '0'.    00045900
046000     05  WS-GWCD-SWITCH               PIC  X(01)    VALUE '0'.    00046000
046100     05  WS-GFHC-SWITCH               PIC  X(01)    VALUE '0'.    00046100
046200     05  WS-GFSA-SWITCH               PIC  X(01)    VALUE '0'.    00046200
046300     05  WS-GHCA-SWITCH               PIC  X(01)    VALUE '0'.    00046300
046400     05  WS-GHSA-SWITCH               PIC  X(01)    VALUE '0'.    00046400
046500     05  WS-GLPF-SWITCH               PIC  X(01)    VALUE '0'.    00046500
046600     05  WS-GLPH-SWITCH               PIC  X(01)    VALUE '0'.    00046600
046700     05  WS-GWHC-SWITCH               PIC  X(01)    VALUE '0'.    00046700
046800     05  WS-GMFH-SWITCH               PIC  X(01)    VALUE '0'.    00046800
046900/                                                                 00046900
047000******************************************************************00047000
047100**                                                                00047100
047200**     PARAMETERS FOR THE TABULAR FILE        TSGVSAM1            00047200
047300**                                                                00047300
047400******************************************************************00047400
047500 01  FILLER                            PIC  X(22)  VALUE          00047500
047600                                       '***  TABULAR FILE  ***'.  00047600
047700                                                                  00047700
047800 01  PARM-SET.                                                    00047800
047900     05  SET-VSAM-RDW.                                            00047900
048000        10 SET-VSAM-RECORD-LENGTH      PIC 9(04)     COMP.        00048000
048100        10 SET-VSAM-FEEDBACK-CODE      PIC 9(04)     COMP.        00048100
048200     05  SET-VSAM-VALUE                PIC 9(08)     COMP.        00048200
048300                                                                  00048300
048400                                                                  00048400
048500 01  PARM-TAB-ONE-A.                                              00048500
048600     05  RESERVED-FLDS                 PIC  9(08) VALUE 0   COMP. 00048600
048700     05  RESERVED-ONE     REDEFINES     RESERVED-FLDS.            00048700
048800        10  VSAM-REQUEST-TYPE          PIC  X(01).                00048800
048900        10  FILLER                     PIC  X(03).                00048900
049000                                                                  00049000
049100                                                                  00049100
049200 01  PARM-TAB-ONE-B.                                              00049200
049300     05  VSAM-RDW.                                                00049300
049400        10  VSAM-RECORD-LENGTH         PIC  9(04)    COMP.        00049400
049500        10  VSAM-FEEDBACK-CODE         PIC  9(04)    COMP.        00049500
049600     05  VSAM-RECORD-AREA              PIC  X(7805).              00049600
049700     05  VSAM-RECORD-A       REDEFINES     VSAM-RECORD-AREA.      00049700
049800         10  VSAM-KEY-FIELD.                                      00049800
049900            15  VSAM-KEY-ID            PIC  X(06).                00049900
050000            15  VSAM-KEY-SLOT          PIC S9(07)    COMP-3.      00050000
050100         10  VSAM-KEY-DATA.                                       00050100
050200            15  FILLER                 PIC  X(27).                00050200
050300            15  VSAM-KEY-ENTRY-COUNT   PIC S9(05)    COMP-3.      00050300
050400            15  FILLER                 PIC  X(7765).              00050400
050500                                                                  00050500
050600/                                                                 00050600
050700 PROCEDURE DIVISION.                                              00050700
050800                                                                  00050800
050900******************************************************************00050900
051000**                                                                00051000
051100**               P R O C E S S    C O N T R O L                   00051100
051200**                                                                00051200
051300******************************************************************00051300
051400 0000-MAINLINE.                                                   00051400
051500                                                                  00051500
051600     PERFORM 1000-OPEN-THE-FILES  THRU  1000-EXIT.                00051600
051700                                                                  00051700
051800     PERFORM 2000-READ-AND-PROCESS-TABULARS  THRU  2000-EXIT      00051800
051900         UNTIL  WS-END-OF-INPUT-FILE-SW-ON.                       00051900
052000                                                                  00052000
052100     PERFORM 9000-CLOSE-THE-FILES  THRU  9000-EXIT.               00052100
052200                                                                  00052200
052300     STOP RUN.                                                    00052300
052400                                                                  00052400
052500 0000-EXIT.                                                       00052500
052600     EXIT.                                                        00052600
052700/                                                                 00052700
052800******************************************************************00052800
052900**                                                                00052900
053000**                O P E N   T H E   F I L E S                     00053000
053100**                                                                00053100
053200******************************************************************00053200
053300 1000-OPEN-THE-FILES.                                             00053300
053400                                                                  00053400
053500     OPEN INPUT  INPUT-FILE,                                      00053500
053600          OUTPUT OUTPUT-FILE.                                     00053600
053700                                                                  00053700
053800                                                                  00053800
053900     MOVE  'S'          TO  VSAM-REQUEST-TYPE.                    00053900
054000     MOVE   8           TO  SET-VSAM-RECORD-LENGTH.               00054000
054100     MOVE   3           TO  SET-VSAM-VALUE.                       00054100
054200     CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-SET.          00054200
054300                                                                  00054300
054400     IF  VSAM-REQUEST-TYPE   NOT =   'S'                          00054400
054500         DISPLAY 'BAD SET IN GC0190    1000-OPEN-THE-FILES'       00054500
054600         MOVE  SET-VSAM-FEEDBACK-CODE  TO  WS-ABEND-CODE          00054600
054700         GO TO  9999-ERROR-RTN.                                   00054700
054800                                                                  00054800
054900 1000-EXIT.                                                       00054900
055000     EXIT.                                                        00055000
055100/                                                                 00055100
055200******************************************************************00055200
055300**                                                                00055300
055400**   R E A D   T H E   S E Q U E N T I A L   I N P U T   F I L E  00055400
055500**                                                                00055500
055600******************************************************************00055600
055700 2000-READ-AND-PROCESS-TABULARS.                                  00055700
055800                                                                  00055800
055900**--READ THE SEQUENTIAL INPUT FILE.                               00055900
056000**                                                                00056000
056100     READ INPUT-FILE                                              00056100
056200         AT END                                                   00056200
056300             MOVE '1'  TO   WS-END-OF-INPUT-FILE-SW               00056300
056400             DISPLAY '  '                                         00056400
056500             DISPLAY ' RELEASED RECORDS READ   =  '               00056500
056600                                      WS-INPUT-RELEASED-RECORDS   00056600
056700             DISPLAY ' VSAM RECORDS READ       =  '               00056700
056800                                          WS-INPUT-VSAM-RECORDS   00056800
056900             DISPLAY ' RECORDS WRITTEN         =  '               00056900
057000                                              WS-OUTPUT-RECORDS   00057000
057100             GO TO 2000-EXIT.                                     00057100
057200                                                                  00057200
057300                                                                  00057300
057400     ADD  1  TO   WS-INPUT-RELEASED-RECORDS.                      00057400
057500                                                                  00057500
057600                                                                  00057600
057700**--PROCESS GROUP SPECIFIC TABULARS ONLY                          00057700
057800**                                                                00057800
057900     IF WRK-TAB-PROV-ID   EQUAL   '#GCCP  '                       00057900
058000         IF WS-GCCP-SWITCH   >   '0'                              00058000
058100             GO TO 2000-EXIT                                      00058100
058200         ELSE                                                     00058200
058300            MOVE  '1'   TO   WS-GCCP-SWITCH                       00058300
058400            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00058400
058500                                                                  00058500
058600                                                                  00058600
058700     IF WRK-TAB-PROV-ID   EQUAL   '#GFSB '                        00058700
058800         IF WS-GFSB-SWITCH   >   '0'                              00058800
058900             GO TO 2000-EXIT                                      00058900
059000         ELSE                                                     00059000
059100            MOVE  '1'   TO   WS-GFSB-SWITCH                       00059100
059200            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00059200
059300                                                                  00059300
059400                                                                  00059400
059500     IF WRK-TAB-PROV-ID   EQUAL   '#GHOB '                        00059500
059600         IF WS-GHOB-SWITCH   >   '0'                              00059600
059700             GO TO 2000-EXIT                                      00059700
059800         ELSE                                                     00059800
059900            MOVE  '1'   TO   WS-GHOB-SWITCH                       00059900
060000            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00060000
060100                                                                  00060100
060200                                                                  00060200
060300     IF WRK-TAB-PROV-ID   EQUAL   '#GHOR '                        00060300
060400         IF WS-GHOR-SWITCH   >   '0'                              00060400
060500             GO TO 2000-EXIT                                      00060500
060600         ELSE                                                     00060600
060700            MOVE  '1'   TO   WS-GHOR-SWITCH                       00060700
060800            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00060800
060900                                                                  00060900
061000     IF WRK-TAB-PROV-ID   EQUAL   '#GMCD '                        00061000
061100         IF WS-GMCD-SWITCH   >   '0'                              00061100
061200             GO TO 2000-EXIT                                      00061200
061300         ELSE                                                     00061300
061400            MOVE  '1'   TO   WS-GMCD-SWITCH                       00061400
061500            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00061500
061600                                                                  00061600
061700     IF WRK-TAB-PROV-ID   EQUAL   '#GMCG '                        00061700
061800         IF WS-GMCG-SWITCH   >   '0'                              00061800
061900             GO TO 2000-EXIT                                      00061900
062000         ELSE                                                     00062000
062100            MOVE  '1'   TO   WS-GMCG-SWITCH                       00062100
062200            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00062200
062300                                                                  00062300
062400     IF WRK-TAB-PROV-ID   EQUAL   '#GMCR '                        00062400
062500         IF WS-GMCR-SWITCH   >   '0'                              00062500
062600             GO TO 2000-EXIT                                      00062600
062700         ELSE                                                     00062700
062800            MOVE  '1'   TO   WS-GMCR-SWITCH                       00062800
062900            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00062900
063000                                                                  00063000
063100     IF WRK-TAB-PROV-ID   EQUAL   '#GMCS '                        00063100
063200         IF WS-GMCS-SWITCH   >   '0'                              00063200
063300             GO TO 2000-EXIT                                      00063300
063400         ELSE                                                     00063400
063500            MOVE  '1'   TO   WS-GMCS-SWITCH                       00063500
063600            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00063600
063700                                                                  00063700
063800     IF WRK-TAB-PROV-ID   EQUAL   '#GRPO '                        00063800
063900         IF WS-GRPO-SWITCH   >   '0'                              00063900
064000             GO TO 2000-EXIT                                      00064000
064100         ELSE                                                     00064100
064200            MOVE  '1'   TO   WS-GRPO-SWITCH                       00064200
064300            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00064300
064400                                                                  00064400
064500                                                                  00064500
064600     IF WRK-TAB-PROV-ID   EQUAL   '#GMCT '                        00064600
064700         IF WS-GMCT-SWITCH   >   '0'                              00064700
064800             GO TO 2000-EXIT                                      00064800
064900         ELSE                                                     00064900
065000            MOVE  '1'   TO   WS-GMCT-SWITCH                       00065000
065100            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00065100
065200                                                                  00065200
065300     IF WRK-TAB-PROV-ID   EQUAL   '#GMDB '                        00065300
065400         IF WS-GMDB-SWITCH   >   '0'                              00065400
065500             GO TO 2000-EXIT                                      00065500
065600         ELSE                                                     00065600
065700            MOVE  '1'   TO   WS-GMDB-SWITCH                       00065700
065800            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00065800
065900                                                                  00065900
066000                                                                  00066000
066100     IF WRK-TAB-PROV-ID   EQUAL   '#GMDN '                        00066100
066200         IF WS-GMDN-SWITCH   >   '0'                              00066200
066300             GO TO 2000-EXIT                                      00066300
066400         ELSE                                                     00066400
066500            MOVE  '1'   TO   WS-GMDN-SWITCH                       00066500
066600            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00066600
066700                                                                  00066700
066800                                                                  00066800
066900     IF WRK-TAB-PROV-ID   EQUAL   '#GMOB '                        00066900
067000         IF WS-GMOB-SWITCH   >   '0'                              00067000
067100             GO TO 2000-EXIT                                      00067100
067200         ELSE                                                     00067200
067300            MOVE  '1'   TO   WS-GMOB-SWITCH                       00067300
067400            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00067400
067500                                                                  00067500
067600                                                                  00067600
067700     IF WRK-TAB-PROV-ID   EQUAL   '#GMOR '                        00067700
067800         IF WS-GMOR-SWITCH   >   '0'                              00067800
067900             GO TO 2000-EXIT                                      00067900
068000         ELSE                                                     00068000
068100            MOVE  '1'   TO   WS-GMOR-SWITCH                       00068100
068200            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00068200
068300                                                                  00068300
068400                                                                  00068400
068500     IF WRK-TAB-PROV-ID   EQUAL   '#GMPB '                        00068500
068600         IF WS-GMPB-SWITCH   >   '0'                              00068600
068700             GO TO 2000-EXIT                                      00068700
068800         ELSE                                                     00068800
068900            MOVE  '1'   TO   WS-GMPB-SWITCH                       00068900
069000            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00069000
069100                                                                  00069100
069200                                                                  00069200
069300     IF WRK-TAB-PROV-ID   EQUAL   '#GMPR '                        00069300
069400         IF WS-GMPR-SWITCH   >   '0'                              00069400
069500             GO TO 2000-EXIT                                      00069500
069600         ELSE                                                     00069600
069700            MOVE  '1'   TO   WS-GMPR-SWITCH                       00069700
069800            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00069800
069900                                                                  00069900
070000                                                                  00070000
070100     IF WRK-TAB-PROV-ID   EQUAL   '#GMSB '                        00070100
070200         IF WS-GMSB-SWITCH   >   '0'                              00070200
070300             GO TO 2000-EXIT                                      00070300
070400         ELSE                                                     00070400
070500            MOVE  '1'   TO   WS-GMSB-SWITCH                       00070500
070600            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00070600
070700                                                                  00070700
070800                                                                  00070800
070900     IF WRK-TAB-PROV-ID   EQUAL   '#GMSC '                        00070900
071000         IF WS-GMSC-SWITCH   >   '0'                              00071000
071100             GO TO 2000-EXIT                                      00071100
071200         ELSE                                                     00071200
071300            MOVE  '1'   TO   WS-GMSC-SWITCH                       00071300
071400            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00071400
071500                                                                  00071500
071600                                                                  00071600
071700     IF WRK-TAB-PROV-ID   EQUAL   '#GMSR '                        00071700
071800         IF WS-GMSR-SWITCH   >   '0'                              00071800
071900             GO TO 2000-EXIT                                      00071900
072000         ELSE                                                     00072000
072100            MOVE  '1'   TO   WS-GMSR-SWITCH                       00072100
072200            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00072200
072300                                                                  00072300
072400                                                                  00072400
072500     IF WRK-TAB-PROV-ID   EQUAL   '#GPAB '                        00072500
072600         IF WS-GPAB-SWITCH   >   '0'                              00072600
072700             GO TO 2000-EXIT                                      00072700
072800         ELSE                                                     00072800
072900            MOVE  '1'   TO   WS-GPAB-SWITCH                       00072900
073000            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00073000
073100                                                                  00073100
073200                                                                  00073200
073300     IF WRK-TAB-PROV-ID   EQUAL   '#GPAC '                        00073300
073400         IF WS-GPAC-SWITCH   >   '0'                              00073400
073500             GO TO 2000-EXIT                                      00073500
073600         ELSE                                                     00073600
073700            MOVE  '1'   TO   WS-GPAC-SWITCH                       00073700
073800            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00073800
073900                                                                  00073900
074000                                                                  00074000
074100     IF WRK-TAB-PROV-ID   EQUAL   '#GPAD '                        00074100
074200         IF WS-GPAD-SWITCH   >   '0'                              00074200
074300             GO TO 2000-EXIT                                      00074300
074400         ELSE                                                     00074400
074500            MOVE  '1'   TO   WS-GPAD-SWITCH                       00074500
074600            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00074600
074700                                                                  00074700
074800                                                                  00074800
074900     IF WRK-TAB-PROV-ID   EQUAL   '#GPAR '                        00074900
075000         IF WS-GPAR-SWITCH   >   '0'                              00075000
075100             GO TO 2000-EXIT                                      00075100
075200         ELSE                                                     00075200
075300            MOVE  '1'   TO   WS-GPAR-SWITCH                       00075300
075400            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00075400
075500                                                                  00075500
075600                                                                  00075600
075700     IF WRK-TAB-PROV-ID   EQUAL   '#GPPO '                        00075700
075800         IF WS-GPPO-SWITCH   >   '0'                              00075800
075900             GO TO 2000-EXIT                                      00075900
076000         ELSE                                                     00076000
076100            MOVE  '1'   TO   WS-GPPO-SWITCH                       00076100
076200            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00076200
076300                                                                  00076300
076400                                                                  00076400
076500     IF WRK-TAB-PROV-ID   EQUAL   '#GRID '                        00076500
076600         IF WS-GRID-SWITCH   >   '0'                              00076600
076700             GO TO 2000-EXIT                                      00076700
076800         ELSE                                                     00076800
076900            MOVE  '1'   TO   WS-GRID-SWITCH                       00076900
077000            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00077000
077100                                                                  00077100
077200                                                                  00077200
077300     IF WRK-TAB-PROV-ID   EQUAL   '#GVLF '                        00077300
077400         IF WS-GVLF-SWITCH   >   '0'                              00077400
077500             GO TO 2000-EXIT                                      00077500
077600         ELSE                                                     00077600
077700            MOVE  '1'   TO   WS-GVLF-SWITCH                       00077700
077800            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00077800
077900                                                                  00077900
078000     IF WRK-TAB-PROV-ID   EQUAL   '#GVLG '                        00078000
078100         IF WS-GVLG-SWITCH   >   '0'                              00078100
078200             GO TO 2000-EXIT                                      00078200
078300         ELSE                                                     00078300
078400            MOVE  '1'   TO   WS-GVLG-SWITCH                       00078400
078500            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00078500
078600                                                                  00078600
078700     IF WRK-TAB-PROV-ID   EQUAL   '#GVLH '                        00078700
078800         IF WS-GVLH-SWITCH   >   '0'                              00078800
078900             GO TO 2000-EXIT                                      00078900
079000         ELSE                                                     00079000
079100            MOVE  '1'   TO   WS-GVLH-SWITCH                       00079100
079200            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00079200
079300                                                                  00079300
079400     IF WRK-TAB-PROV-ID   EQUAL   '#GVLP '                        00079400
079500         IF WS-GVLP-SWITCH   >   '0'                              00079500
079600             GO TO 2000-EXIT                                      00079600
079700         ELSE                                                     00079700
079800            MOVE  '1'   TO   WS-GVLP-SWITCH                       00079800
079900            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00079900
080000                                                                  00080000
080100     IF WRK-TAB-PROV-ID   EQUAL   '#GVLQ '                        00080100
080200         IF WS-GVLQ-SWITCH   >   '0'                              00080200
080300             GO TO 2000-EXIT                                      00080300
080400         ELSE                                                     00080400
080500            MOVE  '1'   TO   WS-GVLQ-SWITCH                       00080500
080600            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00080600
080700                                                                  00080700
080800     IF WRK-TAB-PROV-ID   EQUAL   '#GVLR '                        00080800
080900         IF WS-GVLR-SWITCH   >   '0'                              00080900
081000             GO TO 2000-EXIT                                      00081000
081100         ELSE                                                     00081100
081200            MOVE  '1'   TO   WS-GVLR-SWITCH                       00081200
081300            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00081300
081400                                                                  00081400
081500     IF WRK-TAB-PROV-ID   EQUAL   '#GWCD '                        00081500
081600         IF WS-GWCD-SWITCH   >   '0'                              00081600
081700             GO TO 2000-EXIT                                      00081700
081800         ELSE                                                     00081800
081900            MOVE  '1'   TO   WS-GWCD-SWITCH                       00081900
082000            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00082000
082100                                                                  00082100
082200                                                                  00082200
082300     IF WRK-TAB-PROV-ID   EQUAL   '#GFHC '                        00082300
082400         IF WS-GFHC-SWITCH   >   '0'                              00082400
082500             GO TO 2000-EXIT                                      00082500
082600         ELSE                                                     00082600
082700            MOVE  '1'   TO   WS-GFHC-SWITCH                       00082700
082800            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00082800
082900                                                                  00082900
083000                                                                  00083000
083100     IF WRK-TAB-PROV-ID   EQUAL   '#GFSA '                        00083100
083200         IF WS-GFSA-SWITCH   >   '0'                              00083200
083300             GO TO 2000-EXIT                                      00083300
083400         ELSE                                                     00083400
083500            MOVE  '1'   TO   WS-GFSA-SWITCH                       00083500
083600            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00083600
083700                                                                  00083700
083800                                                                  00083800
083900     IF WRK-TAB-PROV-ID   EQUAL   '#GHCA '                        00083900
084000         IF WS-GHCA-SWITCH   >   '0'                              00084000
084100             GO TO 2000-EXIT                                      00084100
084200         ELSE                                                     00084200
084300            MOVE  '1'   TO   WS-GHCA-SWITCH                       00084300
084400            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00084400
084500                                                                  00084500
084600                                                                  00084600
084700     IF WRK-TAB-PROV-ID   EQUAL   '#GHSA '                        00084700
084800         IF WS-GHSA-SWITCH   >   '0'                              00084800
084900             GO TO 2000-EXIT                                      00084900
085000         ELSE                                                     00085000
085100            MOVE  '1'   TO   WS-GHSA-SWITCH                       00085100
085200            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00085200
085300                                                                  00085300
085400                                                                  00085400
085500     IF WRK-TAB-PROV-ID   EQUAL   '#GLPF '                        00085500
085600         IF WS-GLPF-SWITCH   >   '0'                              00085600
085700             GO TO 2000-EXIT                                      00085700
085800         ELSE                                                     00085800
085900            MOVE  '1'   TO   WS-GLPF-SWITCH                       00085900
086000            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00086000
086100                                                                  00086100
086200                                                                  00086200
086300     IF WRK-TAB-PROV-ID   EQUAL   '#GLPH '                        00086300
086400         IF WS-GLPH-SWITCH   >   '0'                              00086400
086500             GO TO 2000-EXIT                                      00086500
086600         ELSE                                                     00086600
086700            MOVE  '1'   TO   WS-GLPH-SWITCH                       00086700
086800            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00086800
086900                                                                  00086900
087000                                                                  00087000
087100     IF WRK-TAB-PROV-ID   EQUAL   '#GWHC '                        00087100
087200         IF WS-GWHC-SWITCH   >   '0'                              00087200
087300             GO TO 2000-EXIT                                      00087300
087400         ELSE                                                     00087400
087500            MOVE  '1'   TO   WS-GWHC-SWITCH                       00087500
087600            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00087600
087700                                                                  00087700
087800                                                                  00087800
087900     IF WRK-TAB-PROV-ID   EQUAL   '#GMFH '                        00087900
088000         IF WS-GMFH-SWITCH   >   '0'                              00088000
088100             GO TO 2000-EXIT                                      00088100
088200         ELSE                                                     00088200
088300            MOVE  '1'   TO   WS-GMFH-SWITCH                       00088300
088400            PERFORM 3000-PROCESS-GRP-SPEC-TABULARS THRU 3000-EXIT.00088400
088500                                                                  00088500
088600                                                                  00088600
088700     PERFORM 7000-CHECK-ALL-SWITCHES THRU 7000-EXIT.              00088700
088800                                                                  00088800
088900 2000-EXIT.                                                       00088900
089000     EXIT.                                                        00089000
089100/                                                                 00089100
089200******************************************************************00089200
089300**                                                                00089300
089400**        PROCESS ONLY THE  GROUP SPECIFIC TABULARS               00089400
089500**                                                                00089500
089600******************************************************************00089600
089700 3000-PROCESS-GRP-SPEC-TABULARS.                                  00089700
089800                                                                  00089800
089900*--- INITIALIZE LAST SLOT NUMBER                                  00089900
090000**                                                                00090000
090100     MOVE  +10               TO  WS-LAST-SLOT.                    00090100
090200                                                                  00090200
090300                                                                  00090300
090400*--- BUILD KEY FOR TABULAR FILE                                   00090400
090500**                                                                00090500
090600     MOVE  WRK-TAB-PROV-ID   TO  VSAM-KEY-ID.                     00090600
090700     MOVE  +11               TO  VSAM-KEY-SLOT.                   00090700
090800     MOVE  14                TO  VSAM-RECORD-LENGTH.              00090800
090900     MOVE  'P'               TO  VSAM-REQUEST-TYPE.               00090900
091000                                                                  00091000
091100     PERFORM 4000-LOCATE-BEGIN-TABULAR THRU 4000-EXIT.            00091100
091200                                                                  00091200
091300                                                                  00091300
091400*--- IF THE RECORD IS NOT ON THE FILE, CREATE A RECORD            00091400
091500**   WITH THE TABULAR-ID AND ASSIGN A SLOT NUMBER OF 10.          00091500
091600**                                                                00091600
091700     IF  VSAM-REQUEST-TYPE     EQUAL   '2'                        00091700
091800         MOVE  LOW-VALUES        TO  OUTPUT-RECORD                00091800
091900         MOVE  WRK-TAB-PROV-ID   TO  LAST-KEY-ID                  00091900
092000         MOVE  WS-LAST-SLOT      TO  LAST-KEY-SLOT-NO             00092000
092100         WRITE OUTPUT-LAST-RECORD                                 00092100
092200         ADD  1  TO   WS-OUTPUT-RECORDS                           00092200
092300         GO TO 3000-EXIT.                                         00092300
092400                                                                  00092400
092500                                                                  00092500
092600                                                                  00092600
092700**--INITIALIZE THE SWITCH WHEN THE TABULAR ID CHANGES             00092700
092800*                                                                 00092800
092900     MOVE  '0'  TO  WS-VSAM-READ-SWITCH.                          00092900
093000                                                                  00093000
093100                                                                  00093100
093200     PERFORM 5000-READNEXT-VSAM-RECORD THRU 5000-EXIT             00093200
093300         UNTIL   WS-VSAM-READ-SWITCH   >   '0'.                   00093300
093400                                                                  00093400
093500                                                                  00093500
093600                                                                  00093600
093700**--CREATE A RECORD FOR EACH TYPE OF TABULAR, CONTAINING          00093700
093800**  THE VSAM-KEY-ID , (LAST) TAB-SLOT-NUMBER, AND LOW-VALUES      00093800
093900**  IN THE BODY OF THE RECORD.                                    00093900
094000**                                                                00094000
094100     MOVE  LOW-VALUES        TO  OUTPUT-RECORD.                   00094100
094200     MOVE  WRK-TAB-PROV-ID   TO  LAST-KEY-ID.                     00094200
094300     MOVE  WS-LAST-SLOT      TO  LAST-KEY-SLOT-NO.                00094300
094400                                                                  00094400
094500     WRITE OUTPUT-LAST-RECORD.                                    00094500
094600     ADD  1  TO   WS-OUTPUT-RECORDS.                              00094600
094700                                                                  00094700
094800 3000-EXIT.                                                       00094800
094900     EXIT.                                                        00094900
095000/                                                                 00095000
095100******************************************************************00095100
095200**                                                                00095200
095300**             V S A M   T A B U L A R   F I L E                  00095300
095400**                                                                00095400
095500******************************************************************00095500
095600 4000-LOCATE-BEGIN-TABULAR.                                       00095600
095700                                                                  00095700
095800     CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    00095800
095900                                                                  00095900
096000                                                                  00096000
096100     IF  VSAM-REQUEST-TYPE     EQUAL   '2'                        00096100
096200         GO TO 4000-EXIT.                                         00096200
096300                                                                  00096300
096400                                                                  00096400
096500     IF  VSAM-REQUEST-TYPE     NOT =   'P'                        00096500
096600         DISPLAY 'BAD POINT IN GC0190  4000-LOCATE-BEGIN-TABULAR' 00096600
096700         MOVE  VSAM-FEEDBACK-CODE   TO   WS-ABEND-CODE            00096700
096800         GO TO  9999-ERROR-RTN.                                   00096800
096900                                                                  00096900
097000 4000-EXIT.                                                       00097000
097100     EXIT.                                                        00097100
097200/                                                                 00097200
097300******************************************************************00097300
097400**                                                                00097400
097500**       R E A D    V S A M    T A B U L A R    F I L E           00097500
097600**                                                                00097600
097700******************************************************************00097700
097800 5000-READNEXT-VSAM-RECORD.                                       00097800
097900                                                                  00097900
098000     MOVE  'G'          TO  VSAM-REQUEST-TYPE.                    00098000
098100     CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    00098100
098200                                                                  00098200
098300                                                                  00098300
098400**--SET THE SWITCH IF END OF VSAM FILE                            00098400
098500*                                                                 00098500
098600     IF  VSAM-REQUEST-TYPE   EQUAL   '2'                          00098600
098700         MOVE '1' TO WS-VSAM-READ-SWITCH                          00098700
098800         GO TO 5000-EXIT.                                         00098800
098900                                                                  00098900
099000                                                                  00099000
099100     IF  VSAM-REQUEST-TYPE   NOT =   'G'                          00099100
099200         DISPLAY 'BAD GET IN GC0190    5000-READNEXT-VSAM-RECORD' 00099200
099300         MOVE  VSAM-FEEDBACK-CODE   TO   WS-ABEND-CODE            00099300
099400         GO TO  9999-ERROR-RTN.                                   00099400
099500                                                                  00099500
099600                                                                  00099600
099700     IF VSAM-KEY-ID    NOT EQUAL    WRK-TAB-PROV-ID               00099700
099800         MOVE '1'  TO  WS-VSAM-READ-SWITCH                        00099800
099900         GO TO 5000-EXIT.                                         00099900
100000                                                                  00100000
100100****************************************************              00100100
100200**  ADDED 05/23/90 BY ENW FOR PROJ. NUM. 11138.                   00100200
100300**  BYPASS SPECIAL \
100400**  IS GREATER THAN +99999.                                       00100400
100500                                                                  00100500
100600     IF VSAM-KEY-SLOT > +99999                                    00100600
100700         GO TO 5000-EXIT.                                         00100700
100800                                                                  00100800
100900**  END OF MODIFICATION FOR PROJ. NUM. 11138.                     00100900
101000****************************************************              00101000
101100                                                                  00101100
101200     ADD  1               TO  WS-INPUT-VSAM-RECORDS.              00101200
101300     MOVE VSAM-KEY-SLOT   TO  WS-LAST-SLOT.                       00101300
101400     MOVE LOW-VALUES      TO  OUTPUT-RECORD.                      00101400
101500                                                                  00101500
101600     PERFORM 6000-WRITE-GRP-SPEC-TABULARS   THRU  6000-EXIT.      00101600
101700                                                                  00101700
101800 5000-EXIT.                                                       00101800
101900     EXIT.                                                        00101900
102000/                                                                 00102000
102100******************************************************************00102100
102200**                                                                00102200
102300**   CREATE A RECORD FOR EACH TYPE OF GROUP SPECIFIC TABULAR      00102300
102400**                                                                00102400
102500******************************************************************00102500
102600 6000-WRITE-GRP-SPEC-TABULARS.                                    00102600
102700                                                                  00102700
102800     IF VSAM-KEY-ID    EQUAL    '#GCCP '                          00102800
102900         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSS-ENTRY-COUNT           00102900
103000         MOVE  VSAM-RECORD-AREA      TO GSS-COST-CONT-RECORD      00103000
103100         WRITE OUTPUT-GCCP-RECORD                                 00103100
103200         ADD  1  TO   WS-OUTPUT-RECORDS                           00103200
103300         GO TO 6000-EXIT.                                         00103300
103400                                                                  00103400
103500                                                                  00103500
103600     IF VSAM-KEY-ID    EQUAL    '#GFSB '                          00103600
103700         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSU-ENTRY-COUNT           00103700
103800         MOVE  VSAM-RECORD-AREA      TO GSU-RECORD                00103800
103900         WRITE OUTPUT-GFSB-RECORD                                 00103900
104000         ADD  1  TO   WS-OUTPUT-RECORDS                           00104000
104100         GO TO 6000-EXIT.                                         00104100
104200                                                                  00104200
104300                                                                  00104300
104400     IF VSAM-KEY-ID    EQUAL    '#GHOB '                          00104400
104500         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSA-ENTRY-COUNT           00104500
104600         MOVE  VSAM-RECORD-AREA      TO GSA-RECORD                00104600
104700         WRITE OUTPUT-GHOB-RECORD                                 00104700
104800         ADD  1  TO   WS-OUTPUT-RECORDS                           00104800
104900         GO TO 6000-EXIT.                                         00104900
105000                                                                  00105000
105100                                                                  00105100
105200     IF VSAM-KEY-ID    EQUAL    '#GHOR '                          00105200
105300         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSO-ENTRY-COUNT           00105300
105400         MOVE  VSAM-RECORD-AREA      TO GSO-RECORD                00105400
105500         WRITE OUTPUT-GHOR-RECORD                                 00105500
105600         ADD  1  TO   WS-OUTPUT-RECORDS                           00105600
105700         GO TO 6000-EXIT.                                         00105700
105800                                                                  00105800
105900     IF VSAM-KEY-ID    EQUAL    '#GMCD '                          00105900
106000         MOVE  VSAM-KEY-ENTRY-COUNT  TO GS2-ENTRY-COUNT           00106000
106100         MOVE  VSAM-RECORD-AREA      TO GS2-RECORD                00106100
106200         WRITE OUTPUT-GMCD-RECORD                                 00106200
106300         ADD  1  TO   WS-OUTPUT-RECORDS                           00106300
106400         GO TO 6000-EXIT.                                         00106400
106500                                                                  00106500
106600     IF VSAM-KEY-ID    EQUAL    '#GMCG '                          00106600
106700         MOVE  VSAM-KEY-ENTRY-COUNT  TO GS1-ENTRY-COUNT           00106700
106800         MOVE  VSAM-RECORD-AREA      TO GS1-RECORD                00106800
106900         WRITE OUTPUT-GMCG-RECORD                                 00106900
107000         ADD  1  TO   WS-OUTPUT-RECORDS                           00107000
107100         GO TO 6000-EXIT.                                         00107100
107200                                                                  00107200
107300     IF VSAM-KEY-ID    EQUAL    '#GMCR '                          00107300
107400         MOVE  VSAM-KEY-ENTRY-COUNT  TO GS4-ENTRY-COUNT           00107400
107500         MOVE  VSAM-RECORD-AREA      TO GS4-RECORD                00107500
107600         WRITE OUTPUT-GMCR-RECORD                                 00107600
107700         ADD  1  TO   WS-OUTPUT-RECORDS                           00107700
107800         GO TO 6000-EXIT.                                         00107800
107900                                                                  00107900
108000                                                                  00108000
108100     IF VSAM-KEY-ID    EQUAL    '#GMCS '                          00108100
108200         MOVE  VSAM-KEY-ENTRY-COUNT  TO GS6-ENTRY-COUNT           00108200
108300         MOVE  VSAM-RECORD-AREA      TO GS6-RECORD                00108300
108400         WRITE OUTPUT-GMCS-RECORD                                 00108400
108500         ADD  1  TO   WS-OUTPUT-RECORDS                           00108500
108600         GO TO 6000-EXIT.                                         00108600
108700                                                                  00108700
108800     IF VSAM-KEY-ID    EQUAL    '#GRPO '                          00108800
108900         MOVE  VSAM-KEY-ENTRY-COUNT  TO GS7-ENTRY-COUNT           00108900
109000         MOVE  VSAM-RECORD-AREA      TO GS7-RECORD                00109000
109100         WRITE OUTPUT-GRPO-RECORD                                 00109100
109200         ADD  1  TO   WS-OUTPUT-RECORDS                           00109200
109300         GO TO 6000-EXIT.                                         00109300
109400                                                                  00109400
109500                                                                  00109500
109600     IF VSAM-KEY-ID    EQUAL    '#GMCT '                          00109600
109700         MOVE  VSAM-KEY-ENTRY-COUNT  TO GS5-ENTRY-COUNT           00109700
109800         MOVE  VSAM-RECORD-AREA      TO GS5-RECORD                00109800
109900         WRITE OUTPUT-GMCT-RECORD                                 00109900
110000         ADD  1  TO   WS-OUTPUT-RECORDS                           00110000
110100         GO TO 6000-EXIT.                                         00110100
110200                                                                  00110200
110300                                                                  00110300
110400     IF VSAM-KEY-ID    EQUAL    '#GMDB '                          00110400
110500         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSV-ENTRY-COUNT           00110500
110600         MOVE  VSAM-RECORD-AREA      TO GSV-RECORD                00110600
110700         WRITE OUTPUT-GMDB-RECORD                                 00110700
110800         ADD  1  TO   WS-OUTPUT-RECORDS                           00110800
110900         GO TO 6000-EXIT.                                         00110900
111000                                                                  00111000
111100                                                                  00111100
111200     IF VSAM-KEY-ID    EQUAL    '#GMDN '                          00111200
111300         MOVE  VSAM-KEY-ENTRY-COUNT  TO GS3-ENTRY-COUNT           00111300
111400         MOVE  VSAM-RECORD-AREA      TO GS3-RECORD                00111400
111500         WRITE OUTPUT-GMDN-RECORD                                 00111500
111600         ADD  1  TO   WS-OUTPUT-RECORDS                           00111600
111700         GO TO 6000-EXIT.                                         00111700
111800                                                                  00111800
111900                                                                  00111900
112000     IF VSAM-KEY-ID    EQUAL    '#GMOB '                          00112000
112100         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSG-ENTRY-COUNT           00112100
112200         MOVE  VSAM-RECORD-AREA      TO GSG-RECORD                00112200
112300         WRITE OUTPUT-GMOB-RECORD                                 00112300
112400         ADD  1  TO   WS-OUTPUT-RECORDS                           00112400
112500         GO TO 6000-EXIT.                                         00112500
112600                                                                  00112600
112700                                                                  00112700
112800     IF VSAM-KEY-ID    EQUAL    '#GMOR '                          00112800
112900         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSN-ENTRY-COUNT           00112900
113000         MOVE  VSAM-RECORD-AREA      TO GSN-RECORD                00113000
113100         WRITE OUTPUT-GMOR-RECORD                                 00113100
113200         ADD  1  TO   WS-OUTPUT-RECORDS                           00113200
113300         GO TO 6000-EXIT.                                         00113300
113400                                                                  00113400
113500                                                                  00113500
113600     IF VSAM-KEY-ID    EQUAL    '#GMPB '                          00113600
113700         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSH-ENTRY-COUNT           00113700
113800         MOVE  VSAM-RECORD-AREA      TO GSH-RECORD                00113800
113900         WRITE OUTPUT-GMPB-RECORD                                 00113900
114000         ADD  1  TO   WS-OUTPUT-RECORDS                           00114000
114100         GO TO 6000-EXIT.                                         00114100
114200                                                                  00114200
114300                                                                  00114300
114400     IF VSAM-KEY-ID    EQUAL    '#GMPR '                          00114400
114500         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSR-ENTRY-COUNT           00114500
114600         MOVE  VSAM-RECORD-AREA      TO GSR-RECORD                00114600
114700         WRITE OUTPUT-GMPR-RECORD                                 00114700
114800         ADD  1  TO   WS-OUTPUT-RECORDS                           00114800
114900         GO TO 6000-EXIT.                                         00114900
115000                                                                  00115000
115100                                                                  00115100
115200     IF VSAM-KEY-ID    EQUAL    '#GMSB '                          00115200
115300         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSB-ENTRY-COUNT           00115300
115400         MOVE  VSAM-RECORD-AREA      TO GSB-RECORD                00115400
115500         WRITE OUTPUT-GMSB-RECORD                                 00115500
115600         ADD  1  TO   WS-OUTPUT-RECORDS                           00115600
115700         GO TO 6000-EXIT.                                         00115700
115800                                                                  00115800
115900                                                                  00115900
116000     IF VSAM-KEY-ID    EQUAL    '#GMSC '                          00116000
116100         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSL-ENTRY-COUNT           00116100
116200         MOVE  VSAM-RECORD-AREA      TO GSL-RECORD                00116200
116300         WRITE OUTPUT-GMSC-RECORD                                 00116300
116400         ADD  1  TO   WS-OUTPUT-RECORDS                           00116400
116500         GO TO 6000-EXIT.                                         00116500
116600                                                                  00116600
116700                                                                  00116700
116800     IF VSAM-KEY-ID    EQUAL    '#GMSR '                          00116800
116900         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSM-ENTRY-COUNT           00116900
117000         MOVE  VSAM-RECORD-AREA      TO GSM-RECORD                00117000
117100         WRITE OUTPUT-GMSR-RECORD                                 00117100
117200         ADD  1  TO   WS-OUTPUT-RECORDS                           00117200
117300         GO TO 6000-EXIT.                                         00117300
117400                                                                  00117400
117500                                                                  00117500
117600     IF VSAM-KEY-ID    EQUAL    '#GPAB '                          00117600
117700         MOVE  VSAM-KEY-ENTRY-COUNT  TO GST-ENTRY-COUNT           00117700
117800         MOVE  VSAM-RECORD-AREA      TO GST-RECORD                00117800
117900         WRITE OUTPUT-GPAB-RECORD                                 00117900
118000         ADD  1  TO   WS-OUTPUT-RECORDS                           00118000
118100         GO TO 6000-EXIT.                                         00118100
118200                                                                  00118200
118300                                                                  00118300
118400     IF VSAM-KEY-ID    EQUAL    '#GPAC '                          00118400
118500         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSJ-ENTRY-COUNT           00118500
118600         MOVE  VSAM-RECORD-AREA      TO GSJ-RECORD                00118600
118700         WRITE OUTPUT-GPAC-RECORD                                 00118700
118800         ADD  1  TO   WS-OUTPUT-RECORDS                           00118800
118900         GO TO 6000-EXIT.                                         00118900
119000                                                                  00119000
119100                                                                  00119100
119200     IF VSAM-KEY-ID    EQUAL    '#GPAD '                          00119200
119300         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSC-ENTRY-COUNT           00119300
119400         MOVE  VSAM-RECORD-AREA      TO GSC-RECORD                00119400
119500         WRITE OUTPUT-GPAD-RECORD                                 00119500
119600         ADD  1  TO   WS-OUTPUT-RECORDS                           00119600
119700         GO TO 6000-EXIT.                                         00119700
119800                                                                  00119800
119900                                                                  00119900
120000     IF VSAM-KEY-ID    EQUAL    '#GPAR '                          00120000
120100         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSK-ENTRY-COUNT           00120100
120200         MOVE  VSAM-RECORD-AREA      TO GSK-RECORD                00120200
120300         WRITE OUTPUT-GPAR-RECORD                                 00120300
120400         ADD  1  TO   WS-OUTPUT-RECORDS                           00120400
120500         GO TO 6000-EXIT.                                         00120500
120600                                                                  00120600
120700                                                                  00120700
120800     IF VSAM-KEY-ID    EQUAL    '#GPPO '                          00120800
120900         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSW-ENTRY-COUNT           00120900
121000         MOVE  VSAM-RECORD-AREA      TO GSW-RECORD                00121000
121100         WRITE OUTPUT-GPPO-RECORD                                 00121100
121200         ADD  1  TO   WS-OUTPUT-RECORDS                           00121200
121300         GO TO 6000-EXIT.                                         00121300
121400                                                                  00121400
121500                                                                  00121500
121600     IF VSAM-KEY-ID    EQUAL    '#GRID '                          00121600
121700         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSD-ENTRY-COUNT           00121700
121800         MOVE  VSAM-RECORD-AREA      TO GSD-RECORD                00121800
121900         WRITE OUTPUT-GRID-RECORD                                 00121900
122000         ADD  1  TO   WS-OUTPUT-RECORDS                           00122000
122100         GO TO 6000-EXIT.                                         00122100
122200                                                                  00122200
122300                                                                  00122300
122400     IF VSAM-KEY-ID    EQUAL    '#GVLF '                          00122400
122500         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSX-ENTRY-COUNT           00122500
122600         MOVE  VSAM-RECORD-AREA      TO GSX-RECORD                00122600
122700         WRITE OUTPUT-GVLF-RECORD                                 00122700
122800         ADD  1  TO   WS-OUTPUT-RECORDS                           00122800
122900         GO TO 6000-EXIT.                                         00122900
123000                                                                  00123000
123100     IF VSAM-KEY-ID    EQUAL    '#GVLG '                          00123100
123200         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSF-ENTRY-COUNT           00123200
123300         MOVE  VSAM-RECORD-AREA      TO GSF-RECORD                00123300
123400         WRITE OUTPUT-GVLG-RECORD                                 00123400
123500         ADD  1  TO   WS-OUTPUT-RECORDS                           00123500
123600         GO TO 6000-EXIT.                                         00123600
123700                                                                  00123700
123800     IF VSAM-KEY-ID    EQUAL    '#GVLH '                          00123800
123900         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSQ-ENTRY-COUNT           00123900
124000         MOVE  VSAM-RECORD-AREA      TO GSQ-RECORD                00124000
124100         WRITE OUTPUT-GVLH-RECORD                                 00124100
124200         ADD  1  TO   WS-OUTPUT-RECORDS                           00124200
124300         GO TO 6000-EXIT.                                         00124300
124400                                                                  00124400
124500                                                                  00124500
124600     IF VSAM-KEY-ID    EQUAL    '#GVLP '                          00124600
124700         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSY-ENTRY-COUNT           00124700
124800         MOVE  VSAM-RECORD-AREA      TO GSY-RECORD                00124800
124900         WRITE OUTPUT-GVLP-RECORD                                 00124900
125000         ADD  1  TO   WS-OUTPUT-RECORDS                           00125000
125100         GO TO 6000-EXIT.                                         00125100
125200                                                                  00125200
125300     IF VSAM-KEY-ID    EQUAL    '#GVLQ '                          00125300
125400         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSP-ENTRY-COUNT           00125400
125500         MOVE  VSAM-RECORD-AREA      TO GSP-RECORD                00125500
125600         WRITE OUTPUT-GVLQ-RECORD                                 00125600
125700         ADD  1  TO   WS-OUTPUT-RECORDS                           00125700
125800         GO TO 6000-EXIT.                                         00125800
125900                                                                  00125900
126000     IF VSAM-KEY-ID    EQUAL    '#GVLR '                          00126000
126100         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSZ-ENTRY-COUNT           00126100
126200         MOVE  VSAM-RECORD-AREA      TO GSZ-RECORD                00126200
126300         WRITE OUTPUT-GVLR-RECORD                                 00126300
126400         ADD  1  TO   WS-OUTPUT-RECORDS                           00126400
126500         GO TO 6000-EXIT.                                         00126500
126600                                                                  00126600
126700                                                                  00126700
126800     IF VSAM-KEY-ID    EQUAL    '#GWCD '                          00126800
126900         MOVE  VSAM-KEY-ENTRY-COUNT  TO GSI-ENTRY-COUNT           00126900
127000         MOVE  VSAM-RECORD-AREA      TO GSI-RECORD                00127000
127100         WRITE OUTPUT-GWCD-RECORD                                 00127100
127200         ADD  1  TO   WS-OUTPUT-RECORDS.                          00127200
127300                                                                  00127300
127400                                                                  00127400
127500     IF VSAM-KEY-ID    EQUAL    '#GFHC '                          00127500
127600         MOVE  VSAM-KEY-ENTRY-COUNT  TO GFHC-ENTRY-COUNT          00127600
127700         MOVE  VSAM-RECORD-AREA      TO GFHC-RECORD               00127700
127800         WRITE OUTPUT-GFHC-RECORD                                 00127800
127900         ADD  1  TO   WS-OUTPUT-RECORDS                           00127900
128000         GO TO 6000-EXIT.                                         00128000
128100                                                                  00128100
128200                                                                  00128200
128300     IF VSAM-KEY-ID    EQUAL    '#GFSA '                          00128300
128400         MOVE  VSAM-KEY-ENTRY-COUNT  TO GFSA-ENTRY-COUNT          00128400
128500         MOVE  VSAM-RECORD-AREA      TO GFSA-RECORD               00128500
128600         WRITE OUTPUT-GFSA-RECORD                                 00128600
128700         ADD  1  TO   WS-OUTPUT-RECORDS                           00128700
128800         GO TO 6000-EXIT.                                         00128800
128900                                                                  00128900
129000                                                                  00129000
129100     IF VSAM-KEY-ID    EQUAL    '#GHCA '                          00129100
129200         MOVE  VSAM-KEY-ENTRY-COUNT  TO GHCA-ENTRY-COUNT          00129200
129300         MOVE  VSAM-RECORD-AREA      TO GHCA-RECORD               00129300
129400         WRITE OUTPUT-GHCA-RECORD                                 00129400
129500         ADD  1  TO   WS-OUTPUT-RECORDS                           00129500
129600         GO TO 6000-EXIT.                                         00129600
129700                                                                  00129700
129800                                                                  00129800
129900     IF VSAM-KEY-ID    EQUAL    '#GHSA '                          00129900
130000         MOVE  VSAM-KEY-ENTRY-COUNT  TO GHSA-ENTRY-COUNT          00130000
130100         MOVE  VSAM-RECORD-AREA      TO GHSA-RECORD               00130100
130200         WRITE OUTPUT-GHSA-RECORD                                 00130200
130300         ADD  1  TO   WS-OUTPUT-RECORDS                           00130300
130400         GO TO 6000-EXIT.                                         00130400
130500                                                                  00130500
130600                                                                  00130600
130700     IF VSAM-KEY-ID    EQUAL    '#GLPF '                          00130700
130800         MOVE  VSAM-KEY-ENTRY-COUNT  TO GLPF-ENTRY-COUNT          00130800
130900         MOVE  VSAM-RECORD-AREA      TO GLPF-RECORD               00130900
131000         WRITE OUTPUT-GLPF-RECORD                                 00131000
131100         ADD  1  TO   WS-OUTPUT-RECORDS                           00131100
131200         GO TO 6000-EXIT.                                         00131200
131300                                                                  00131300
131400                                                                  00131400
131500     IF VSAM-KEY-ID    EQUAL    '#GLPH '                          00131500
131600         MOVE  VSAM-KEY-ENTRY-COUNT  TO GLPH-ENTRY-COUNT          00131600
131700         MOVE  VSAM-RECORD-AREA      TO GLPH-RECORD               00131700
131800         WRITE OUTPUT-GLPH-RECORD                                 00131800
131900         ADD  1  TO   WS-OUTPUT-RECORDS                           00131900
132000         GO TO 6000-EXIT.                                         00132000
132100                                                                  00132100
132200                                                                  00132200
132300     IF VSAM-KEY-ID    EQUAL    '#GWHC '                          00132300
132400         MOVE  VSAM-KEY-ENTRY-COUNT  TO GWHC-ENTRY-COUNT          00132400
132500         MOVE  VSAM-RECORD-AREA      TO GWHC-RECORD               00132500
132600         WRITE OUTPUT-GWHC-RECORD                                 00132600
132700         ADD  1  TO   WS-OUTPUT-RECORDS                           00132700
132800         GO TO 6000-EXIT.                                         00132800
132900                                                                  00132900
133000                                                                  00133000
133100     IF VSAM-KEY-ID    EQUAL    '#GMFH '                          00133100
133200         MOVE  VSAM-KEY-ENTRY-COUNT  TO GMFH-ENTRY-COUNT          00133200
133300         MOVE  VSAM-RECORD-AREA      TO GMFH-RECORD               00133300
133400         WRITE OUTPUT-GMFH-RECORD                                 00133400
133500         ADD  1  TO   WS-OUTPUT-RECORDS                           00133500
133600         GO TO 6000-EXIT.                                         00133600
133700                                                                  00133700
133800                                                                  00133800
133900 6000-EXIT.                                                       00133900
134000     EXIT.                                                        00134000
134100/                                                                 00134100
134200******************************************************************00134200
134300**                                                                00134300
134400**    IF ALL OF THE GROUP SPECIFIC TABULARS HAVE BEEN READ,       00134400
134500**    SET THE END OF FILE SWITCH FOR THE SEQUENTIAL INPUT         00134500
134600**    (RELEASED GROUP SPECIFIC TABULAR) FILE.                     00134600
134700**                                                                00134700
134800******************************************************************00134800
134900 7000-CHECK-ALL-SWITCHES.                                         00134900
135000                                                                  00135000
135100     IF WS-GCCP-SWITCH   >   '0'                                  00135100
135200       AND                                                        00135200
135300        WS-GFSB-SWITCH   >   '0'                                  00135300
135400       AND                                                        00135400
135500        WS-GHOB-SWITCH   >   '0'                                  00135500
135600       AND                                                        00135600
135700        WS-GHOR-SWITCH   >   '0'                                  00135700
135800       AND                                                        00135800
135900        WS-GMCD-SWITCH   >   '0'                                  00135900
136000       AND                                                        00136000
136100        WS-GMCG-SWITCH   >   '0'                                  00136100
136200       AND                                                        00136200
136300        WS-GMCR-SWITCH   >   '0'                                  00136300
136400       AND                                                        00136400
136500        WS-GMCS-SWITCH   >   '0'                                  00136500
136600       AND                                                        00136600
136700        WS-GRPO-SWITCH   >   '0'                                  00136700
136800       AND                                                        00136800
136900        WS-GMCT-SWITCH   >   '0'                                  00136900
137000       AND                                                        00137000
137100        WS-GMDB-SWITCH   >   '0'                                  00137100
137200       AND                                                        00137200
137300        WS-GMDN-SWITCH   >   '0'                                  00137300
137400       AND                                                        00137400
137500        WS-GMOB-SWITCH   >   '0'                                  00137500
137600       AND                                                        00137600
137700        WS-GMOR-SWITCH   >   '0'                                  00137700
137800       AND                                                        00137800
137900        WS-GMPB-SWITCH   >   '0'                                  00137900
138000       AND                                                        00138000
138100        WS-GMPR-SWITCH   >   '0'                                  00138100
138200       AND                                                        00138200
138300        WS-GMSB-SWITCH   >   '0'                                  00138300
138400       AND                                                        00138400
138500        WS-GMSC-SWITCH   >   '0'                                  00138500
138600       AND                                                        00138600
138700        WS-GMSR-SWITCH   >   '0'                                  00138700
138800       AND                                                        00138800
138900        WS-GPAB-SWITCH   >   '0'                                  00138900
139000       AND                                                        00139000
139100        WS-GPAC-SWITCH   >   '0'                                  00139100
139200       AND                                                        00139200
139300        WS-GPAD-SWITCH   >   '0'                                  00139300
139400       AND                                                        00139400
139500        WS-GPAR-SWITCH   >   '0'                                  00139500
139600       AND                                                        00139600
139700        WS-GPPO-SWITCH   >   '0'                                  00139700
139800       AND                                                        00139800
139900        WS-GRID-SWITCH   >   '0'                                  00139900
140000       AND                                                        00140000
140100        WS-GVLF-SWITCH   >   '0'                                  00140100
140200       AND                                                        00140200
140300        WS-GVLG-SWITCH   >   '0'                                  00140300
140400       AND                                                        00140400
140500        WS-GVLH-SWITCH   >   '0'                                  00140500
140600       AND                                                        00140600
140700        WS-GVLP-SWITCH   >   '0'                                  00140700
140800       AND                                                        00140800
140900        WS-GVLQ-SWITCH   >   '0'                                  00140900
141000       AND                                                        00141000
141100        WS-GVLR-SWITCH   >   '0'                                  00141100
141200       AND                                                        00141200
141300        WS-GWCD-SWITCH   >   '0'                                  00141300
141400       AND                                                        00141400
141500        WS-GFHC-SWITCH   >   '0'                                  00141500
141600       AND                                                        00141600
141700        WS-GFSA-SWITCH   >   '0'                                  00141700
141800       AND                                                        00141800
141900        WS-GHCA-SWITCH   >   '0'                                  00141900
142000       AND                                                        00142000
142100        WS-GHSA-SWITCH   >   '0'                                  00142100
142200       AND                                                        00142200
142300        WS-GLPF-SWITCH   >   '0'                                  00142300
142400       AND                                                        00142400
142500        WS-GLPH-SWITCH   >   '0'                                  00142500
142600       AND                                                        00142600
142700        WS-GWHC-SWITCH   >   '0'                                  00142700
142800       AND                                                        00142800
142900        WS-GMFH-SWITCH   >   '0'                                  00142900
143000         MOVE  '1'   TO   WS-END-OF-INPUT-FILE-SW.                00143000
143100                                                                  00143100
143200 7000-EXIT.                                                       00143200
143300     EXIT.                                                        00143300
143400/                                                                 00143400
143500******************************************************************00143500
143600**                                                                00143600
143700**            C L O S E   T H E   F I L E S                       00143700
143800**                                                                00143800
143900******************************************************************00143900
144000 9000-CLOSE-THE-FILES.                                            00144000
144100                                                                  00144100
144200     CLOSE INPUT-FILE,                                            00144200
144300           OUTPUT-FILE.                                           00144300
144400                                                                  00144400
144500                                                                  00144500
144600     MOVE     'C'           TO   VSAM-REQUEST-TYPE.               00144600
144700     CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    00144700
144800                                                                  00144800
144900     IF  VSAM-REQUEST-TYPE    NOT =   'C'                         00144900
145000         DISPLAY 'BAD CLOSE IN GC0190 AT 9000-CLOSE-THE-FILES'    00145000
145100         MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             00145100
145200         GO TO  9999-ERROR-RTN.                                   00145200
145300                                                                  00145300
145400 9000-EXIT.                                                       00145400
145500     EXIT.                                                        00145500
145600/                                                                 00145600
145700******************************************************************00145700
145800**                                                                00145800
145900**                       A B E N D                                00145900
146000**                                                                00146000
146100******************************************************************00146100
146200 9999-ERROR-RTN.                                                  00146200
146300                                                                  00146300
146400     CALL  'TSGEND' USING  WS-ABEND-CODE.                         00146400
146500                                                                  00146500
146600 9999-EXIT.                                                       00146600
146700     EXIT.                                                        00146700
146800/                                                                 00146800
