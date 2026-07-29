000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.         GC0200.                                      00020000
000300 AUTHOR.             DELORES FRY.                                 00030000
000400 INSTALLATION.       HCSC.                                        00040000
000500 DATE-WRITTEN.       JANUARY 1988.                                00050000
000600 DATE-COMPILED.                                                   00060000
000700******************************************************************00070000
000800*                                                                *00080000
000900*             GROUP SPECIFIC TABULAR RECORD                      *00090000
001000*             MATCH AND SLOT UPDATE PROGRAM                      *00100000
001100*                                                                *00110000
001200******************************************************************00120000
001300******************************************************************00130000
001400******************************************************************00140000
001500*                                                                *00150000
001600*       ***-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00160000
001700*       *-*         U P D A T E   H I S T O R Y         *-*      *00170000
001800*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00180000
001900*                                                                *00190000
002000*                                                                *00200000
002100**-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION--------------- *00210000
002200*                                                                *00220000
002300*   D1009     1/05/88  FRY   CREATED THIS PROGRAM.               *00230000
002400*                                                                *00240000
002500*   D199      8/15/89  RKH   ADDED 4 NEW TABULARS.               *00250000
002600*                            ADDED PROV CNTRL TO GVLF, GVLP      *00260000
002700*                                                                *00270000
002800*   D249      8/01/90  GDM   ADDED NEW TABULAR: #GMCG            *00280000
002900*                                                                *00290000
003000*   D249.01   8/22/90  APH   ADDED 2 NEW TABULARS: #GMCD, #GMCR. *00300000
003100*                                                                *00310000
003200*   11154     3/06/91  FRY   INCREASE RECORD AREAS IN FILE       *00320000
003300*                            SECTION:                            *00330000
003400*              INPUT-WRK-COMPARE.                                *00340000
003500*              INPUT-WRK-ENTRIES  PIC X(3960)  CHANGED TO  7765. *00350000
003600*             OUTPUT-WRK-RECORD   PIC X(4064)  CHANGED TO  7869. *00360000
003700*                                                                *00370000
003800*  D11836    05/30/91  BSO   ADDED NEW TABULAR: #GMCT            *00380000
003900*                                                                *00390000
004000* ?????  02/11/92  KJD  ADDED LOGIC TO ALLOW CHAINED TABS        *00400000
004100*                       TO BE WRITTEN AS EXISTING.               *00410000
004200*                                                                *00420000
004300* D-292  03/02/92  KJD  ADDED NEW TABULAR: #GMCS                 *00430000
004400*                                                                *00440000
004500* P-XXX  06/18/92  KJD  CORRECT CHAIN LOGIC                      *00450000
004600*                                                                *00460000
004700* 12730  12/05/92  ENW  ADDED NEW TABULAR: #GRPO                 *00470000
004800*                                                                *00480000
004900*  1293  02/05/93  JGR  REPLACE CURRENT PROCESSING WITH MATCHES  *00490000
005000*                       AGAINST EXISTING TABULARS USING THE      *00500000
005100*                       TABULAR HASHING PROGRAM.                 *00510000
005200*                                                                *00520000
005300*         1/17/95  EMS  CONVERTED TO COBOL II.                   *00530000
005400*                                                                *00540000
005500*D14045  02/14/95  KJD  ADDED NEW TABULAR: #GCPO                 *00550000
005600*                                                                *00560000
005700*D14631   3/11/96  FRY  ADDED NEW TABULARS:  #GCBL AND #GPAN     *00570000
005800*                                                                *00580000
005900*D14402   4/10/96  GDM  ADDED NEW TABULAR: #GSUB                 *00590000
006000*                                                                *00600000
006100* 14726/  11/11/97  GSP  MODIFIED TO BECOME MILLENNIUM           *00610000
006200* 15057                  COMPLIANT. INCREASED FILLERS FROM       *00620000
006300*                        64 TO 100 TO REFLECT INCREASE IN        *00630000
006400*                        INPUT WORK KEY (COPYBOOK GCWRKDCC).     *00640000
006500*                                                                *00650000
006600*  14726     07/28/98  AB   RECOMPILE  TO SUPPORT THE YEAR        00660000
006700*                           2000 GSUB CAPTURE CENTURY IN THE      00670000
006800*                           DATE FIELD(S).                        00680000
006900*P       12/23/98  KJD  CHNG TO USE INDIVIDUAL TAB WORK AREAS    *00690000
007000*                       PASSED TO HASHING PGM                    *00700000
007100*                                                                *00710000
007200* D15380     03/24/99  GDM   ADDED NEW TABULAR: #GBAE            *00720000
007300*                                                                *00730000
007400*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00740000
007500*                                                                *00750000
007600* D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES #GHOR,     *00760000
007700*                        #GMCD, #GMCR, #GMOR, #GMPR, #GMSR,      *00770000
007800*                        #GPAD, #GPAR, #GRID, #GWCD.             *00780000
007900*                                                                *00790000
008000*  P02384    09/14/05  GDM  ADD NEW TABULARS: #GFHC, #GFSA,      *00800000
008100*                           #GHCA, #GHSA, #GLPF, #GLPH, #GWHC    *00810000
008200*                                                                *00820000
008300*  DM9400    05/15/07  LR   ADD NEW TABULAR #GMFH                *00830000
008400*                                                                *00840000
00043 * DM9441   9/08/09  JJS  CHANGED FOR FILE CONVERSION             *00841002
008400*                                                                *00841003
008300*  P00893  07/15/11  ART  OCTOBER RELEASE - BLUE DISTINCTION     *00841004
008400*                         RECOMPILE ONLY.                        *00842002
      *                                                                *00842003
      * P20368   09/11/15  KIKI  BD / TC  COMPILE ONLY - GCTGCCP*      *00842004
      *                                                                *00842005
      * 9-19-2016   RECOMPLIE PGM EXPANDED OCCURS VALUE IN GCTGPPOC    *00842006
      *                                                                *00842007
      * P22147   05/03/17  TROY  CE COMPILE ONLY - GCTGCCP*            *00842008
      *                                                                *00842009
      * P22845   07/18/18  SRI   PC/EC COMPILE ONLY - GCTGCCP*         *00842010
      *                                                                *00842011
      * P23672   06/25/19  SRI   CU COMPILE ONLY - GCTGCCP*            *00842020
      *                                                                *00842030
      * P23805   06/25/19  SRI   PC COMPILE ONLY - GCTGCCP* NEW FIELDS *00842040
TM0526*BBDA-66049  08/10/26  TM   ADD NEW TABULAR #GHPA                *00842050
TM0526*                                                                *00842060
008500******************************************************************00850000
                                                                        00860000
008700 ENVIRONMENT DIVISION.                                            00870000
008800                                                                  00880000
008900 CONFIGURATION SECTION.                                           00890000
009000 SOURCE-COMPUTER.  IBM-370.                                       00900000
009100 OBJECT-COMPUTER.  IBM-370.                                       00910000
009200                                                                  00920000
009300 INPUT-OUTPUT SECTION.                                            00930000
009400                                                                  00940000
009500 FILE-CONTROL.                                                    00950000
009600     SELECT   INPUT-WRK-FILE     ASSIGN  TO   UT-S-GC0200A.       00960000
009700     SELECT   OUTPUT-WRK-FILE    ASSIGN  TO   UT-S-GC0200B.       00970000
009800                                                                  00980000
009900                                                                  00990000
010000 DATA DIVISION.                                                   01000000
010100 FILE SECTION.                                                    01010000
010200                                                                  01020000
010300 FD  INPUT-WRK-FILE                                               01030000
010400     LABEL RECORDS ARE STANDARD                                   01040000
010500     RECORDING MODE IS V                                          01050000
010600     BLOCK CONTAINS  0  RECORDS.                                  01060000
010700                                                                  01070000
010800 01  INPUT-WRK-RECORD.                                            01080000
010900*    05  INPUT-WRK-KEY                     PIC X(100).            01090000
011000         COPY GCWRKDCC.                                           01100000
011100     05  INPUT-WRK-TABULAR-RECORD.                                01110000
011200         10  INPUT-WRK-TAB-REC-KEY.                               01120000
011300             15  INPUT-WRK-TAB-REC-ID      PIC X(6).              01130000
011400             15  INPUT-WRK-TAB-REC-SLOT    PIC S9(7) COMP-3.      01140000
011500         10  FILLER                        PIC X(27).             01150000
011600         10  INPUT-WRK-COMPARE.                                   01160000
011700             15  INPUT-WRK-TAB-REC-CNT     PIC S9(5) COMP-3.      01170000
00043 * DM9441   9/08/09  JJS  CHANGED FOR FILE CONVERSION             *01171002
011800             15  INPUT-WRK-ENTRIES         PIC X(31330).          01180001
011900/                                                                 01190000
012000                                                                  01200000
012100 01  INPUT-GSUB-RECORD.                                           01210000
012200     05  FILLER                            PIC X(100).            01220000
012300     COPY GCTGSUB3.                                               01230000
012400/                                                                 01240000
012500                                                                  01250000
012600 01  INPUT-GCBL-RECORD.                                           01260000
012700     05  FILLER                            PIC X(100).            01270000
012800     COPY GCTGCBL3.                                               01280000
012900/                                                                 01290000
013000 01  INPUT-GCCP-RECORD.                                           01300000
013100     05  FILLER                            PIC X(100).            01310000
013200     COPY GCTGCCP3.                                               01320000
013300/                                                                 01330000
013400 01  INPUT-GCPO-RECORD.                                           01340000
013500     05  FILLER                            PIC X(100).            01350000
013600     COPY GCTGCPO3.                                               01360000
013700/                                                                 01370000
013800 01  INPUT-GFSB-RECORD.                                           01380000
013900     05  FILLER                            PIC X(100).            01390000
014000     COPY GCTGFSB3.                                               01400000
014100/                                                                 01410000
014200 01  INPUT-GHOB-RECORD.                                           01420000
014300     05  FILLER                            PIC X(100).            01430000
014400     COPY GCTGHOB3.                                               01440000
014500/                                                                 01450000
014600 01  INPUT-GHOR-RECORD.                                           01460000
014700     05  FILLER                            PIC X(100).            01470000
014800     COPY GCTGHOR3.                                               01480000
014900/                                                                 01490000
015000 01  INPUT-GMCD-RECORD.                                           01500000
015100     05  FILLER                            PIC X(100).            01510000
015200     COPY GCTGMCD3.                                               01520000
015300/                                                                 01530000
015400 01  INPUT-GMCG-RECORD.                                           01540000
015500     05  FILLER                            PIC X(100).            01550000
015600     COPY GCTGMCG3.                                               01560000
015700/                                                                 01570000
015800 01  INPUT-GMCR-RECORD.                                           01580000
015900     05  FILLER                            PIC X(100).            01590000
016000     COPY GCTGMCR3.                                               01600000
016100/                                                                 01610000
016200 01  INPUT-GMCS-RECORD.                                           01620000
016300     05  FILLER                            PIC X(100).            01630000
016400     COPY GCTGMCS3.                                               01640000
016500/                                                                 01650000
016600 01  INPUT-GMCT-RECORD.                                           01660000
016700     05  FILLER                            PIC X(100).            01670000
016800     COPY GCTGMCT3.                                               01680000
016900/                                                                 01690000
017000 01  INPUT-GMDB-RECORD.                                           01700000
017100     05  FILLER                            PIC X(100).            01710000
017200     COPY GCTGMDB3.                                               01720000
017300/                                                                 01730000
017400 01  INPUT-GMDN-RECORD.                                           01740000
017500     05  FILLER                            PIC X(100).            01750000
017600     COPY GCTGMDN3.                                               01760000
017700/                                                                 01770000
017800 01  INPUT-GMOB-RECORD.                                           01780000
017900     05  FILLER                            PIC X(100).            01790000
018000     COPY GCTGMOB3.                                               01800000
018100/                                                                 01810000
018200 01  INPUT-GMOR-RECORD.                                           01820000
018300     05  FILLER                            PIC X(100).            01830000
018400     COPY GCTGMOR3.                                               01840000
018500/                                                                 01850000
018600 01  INPUT-GMPB-RECORD.                                           01860000
018700     05  FILLER                            PIC X(100).            01870000
018800     COPY GCTGMPB3.                                               01880000
018900/                                                                 01890000
019000 01  INPUT-GMPR-RECORD.                                           01900000
019100     05  FILLER                            PIC X(100).            01910000
019200     COPY GCTGMPR3.                                               01920000
019300/                                                                 01930000
019400 01  INPUT-GMSB-RECORD.                                           01940000
019500     05  FILLER                            PIC X(100).            01950000
019600     COPY GCTGMSB3.                                               01960000
019700/                                                                 01970000
019800 01  INPUT-GMSC-RECORD.                                           01980000
019900     05  FILLER                            PIC X(100).            01990000
020000     COPY GCTGMSC3.                                               02000000
020100/                                                                 02010000
020200 01  INPUT-GMSR-RECORD.                                           02020000
020300     05  FILLER                            PIC X(100).            02030000
020400     COPY GCTGMSR3.                                               02040000
020500/                                                                 02050000
020600 01  INPUT-GPAB-RECORD.                                           02060000
020700     05  FILLER                            PIC X(100).            02070000
020800     COPY GCTGPAB3.                                               02080000
020900/                                                                 02090000
021000 01  INPUT-GPAC-RECORD.                                           02100000
021100     05  FILLER                            PIC X(100).            02110000
021200     COPY GCTGPAC3.                                               02120000
021300/                                                                 02130000
021400 01  INPUT-GPAD-RECORD.                                           02140000
021500     05  FILLER                            PIC X(100).            02150000
021600     COPY GCTGPAD3.                                               02160000
021700/                                                                 02170000
021800 01  INPUT-GPAN-RECORD.                                           02180000
021900     05  FILLER                            PIC X(100).            02190000
022000     COPY GCTGPAN3.                                               02200000
022100/                                                                 02210000
022200 01  INPUT-GPAR-RECORD.                                           02220000
022300     05  FILLER                            PIC X(100).            02230000
022400     COPY GCTGPAR3.                                               02240000
022500/                                                                 02250000
022600 01  INPUT-GPPO-RECORD.                                           02260000
022700     05  FILLER                            PIC X(100).            02270000
022800     COPY GCTGPPO3.                                               02280000
022900/                                                                 02290000
023000 01  INPUT-GRPO-RECORD.                                           02300000
023100     05  FILLER                            PIC X(100).            02310000
023200     COPY GCTGRPO3.                                               02320000
023300/                                                                 02330000
023400 01  INPUT-GRID-RECORD.                                           02340000
023500     05  FILLER                            PIC X(100).            02350000
023600     COPY GCTGRID3.                                               02360000
023700/                                                                 02370000
023800 01  INPUT-GVLF-RECORD.                                           02380000
023900     05  FILLER                            PIC X(100).            02390000
024000     COPY GCTGVLF3.                                               02400000
024100/                                                                 02410000
024200 01  INPUT-GVLG-RECORD.                                           02420000
024300     05  FILLER                            PIC X(100).            02430000
024400     COPY GCTGVLG3.                                               02440000
024500/                                                                 02450000
024600 01  INPUT-GVLH-RECORD.                                           02460000
024700     05  FILLER                            PIC X(100).            02470000
024800     COPY GCTGVLH3.                                               02480000
024900/                                                                 02490000
025000 01  INPUT-GVLP-RECORD.                                           02500000
025100     05  FILLER                            PIC X(100).            02510000
025200     COPY GCTGVLP3.                                               02520000
025300/                                                                 02530000
025400 01  INPUT-GVLQ-RECORD.                                           02540000
025500     05  FILLER                            PIC X(100).            02550000
025600     COPY GCTGVLQ3.                                               02560000
025700/                                                                 02570000
025800 01  INPUT-GVLR-RECORD.                                           02580000
025900     05  FILLER                            PIC X(100).            02590000
026000     COPY GCTGVLR3.                                               02600000
026100/                                                                 02610000
026200 01  INPUT-GWCD-RECORD.                                           02620000
026300     05  FILLER                            PIC X(100).            02630000
026400     COPY GCTGWCD3.                                               02640000
026500/                                                                 02650000
026600 01  INPUT-GBAE-RECORD.                                           02660000
026700     05  FILLER                            PIC X(100).            02670000
026800     COPY GCTGBAE3.                                               02680000
026900/                                                                 02690000
027000 01  INPUT-GFHC-RECORD.                                           02700000
027100     05  FILLER                            PIC X(100).            02710000
027200     COPY GCTGFHC3.                                               02720000
027300/                                                                 02730000
027400 01  INPUT-GFSA-RECORD.                                           02740000
027500     05  FILLER                            PIC X(100).            02750000
027600     COPY GCTGFSA3.                                               02760000
027700/                                                                 02770000
027800 01  INPUT-GHCA-RECORD.                                           02780000
027900     05  FILLER                            PIC X(100).            02790000
028000     COPY GCTGHCA3.                                               02800000
028100/                                                                 02810000
028200 01  INPUT-GHSA-RECORD.                                           02820000
028300     05  FILLER                            PIC X(100).            02830000
028400     COPY GCTGHSA3.                                               02840000
028500/                                                                 02850000
028600 01  INPUT-GLPF-RECORD.                                           02860000
028700     05  FILLER                            PIC X(100).            02870000
028800     COPY GCTGLPF3.                                               02880000
028900/                                                                 02890000
029000 01  INPUT-GLPH-RECORD.                                           02900000
029100     05  FILLER                            PIC X(100).            02910000
029200     COPY GCTGLPH3.                                               02920000
029300/                                                                 02930000
029400 01  INPUT-GWHC-RECORD.                                           02940000
029500     05  FILLER                            PIC X(100).            02950000
029600     COPY GCTGWHC3.                                               02960000
029700/                                                                 02970000
029800 01  INPUT-GMFH-RECORD.                                           02980000
029900     05  FILLER                            PIC X(100).            02990000
030000     COPY GCTGMFH3.                                               03000000
030100/                                                                 03010000
TM0526 01  INPUT-GHPA-RECORD.                                           03010100
TM0526     05  FILLER                            PIC X(100).            03010200
TM0526     COPY GCTGHPA3.                                               03010300
TM0526/                                                                 03010400
030200                                                                  03020000
030300                                                                  03030000
030400 FD  OUTPUT-WRK-FILE                                              03040000
030500     LABEL RECORDS ARE STANDARD                                   03050000
030600     RECORDING MODE IS V                                          03060000
030700     BLOCK CONTAINS  0  RECORDS.                                  03070000
030800                                                                  03080000
030900                                                                  03090000
00043 * DM9441   9/08/09  JJS  CHANGED        FOR FILE CONVERSION      *03091002
031000 01  OUTPUT-WRK-RECORD                PIC X(31470).               03100001
031100/                                                                 03110000
031200                                                                  03120000
031300 01  OUTPUT-GSUB-RECORD.                                          03130000
031400     05  FILLER                       PIC X(100).                 03140000
031500     COPY GCTGSUBC.                                               03150000
031600/                                                                 03160000
031700 01  OUTPUT-GCBL-RECORD.                                          03170000
031800     05  FILLER                       PIC X(100).                 03180000
031900     COPY GCTGCBLC.                                               03190000
032000/                                                                 03200000
032100 01  OUTPUT-GCCP-RECORD.                                          03210000
032200     05 FILLER                        PIC X(100).                 03220000
032300     COPY  GCTGCCPC.                                              03230000
032400/                                                                 03240000
032500 01  OUTPUT-GCPO-RECORD.                                          03250000
032600     05  FILLER                       PIC X(100).                 03260000
032700     COPY GCTGCPOC.                                               03270000
032800/                                                                 03280000
032900 01  OUTPUT-GFSB-RECORD.                                          03290000
033000     05 FILLER                        PIC X(100).                 03300000
033100     COPY  GCTGFSBC.                                              03310000
033200/                                                                 03320000
033300 01  OUTPUT-GHOB-RECORD.                                          03330000
033400     05 FILLER                        PIC X(100).                 03340000
033500     COPY  GCTGHOBC.                                              03350000
033600/                                                                 03360000
033700 01  OUTPUT-GHOR-RECORD.                                          03370000
033800     05 FILLER                        PIC X(100).                 03380000
033900     COPY  GCTGHORC.                                              03390000
034000/                                                                 03400000
034100 01  OUTPUT-GMCD-RECORD.                                          03410000
034200     05 FILLER                        PIC X(100).                 03420000
034300     COPY  GCTGMCDC.                                              03430000
034400/                                                                 03440000
034500 01  OUTPUT-GMCG-RECORD.                                          03450000
034600     05 FILLER                        PIC X(100).                 03460000
034700     COPY  GCTGMCGC.                                              03470000
034800/                                                                 03480000
034900 01  OUTPUT-GMCR-RECORD.                                          03490000
035000     05 FILLER                        PIC X(100).                 03500000
035100     COPY  GCTGMCRC.                                              03510000
035200/                                                                 03520000
035300 01  OUTPUT-GMCS-RECORD.                                          03530000
035400     05 FILLER                        PIC X(100).                 03540000
035500     COPY  GCTGMCSC.                                              03550000
035600/                                                                 03560000
035700 01  OUTPUT-GMCT-RECORD.                                          03570000
035800     05 FILLER                        PIC X(100).                 03580000
035900     COPY  GCTGMCTC.                                              03590000
036000/                                                                 03600000
036100 01  OUTPUT-GMDB-RECORD.                                          03610000
036200     05 FILLER                        PIC X(100).                 03620000
036300     COPY  GCTGMDBC.                                              03630000
036400/                                                                 03640000
036500 01  OUTPUT-GMDN-RECORD.                                          03650000
036600     05 FILLER                        PIC X(100).                 03660000
036700     COPY  GCTGMDNC.                                              03670000
036800/                                                                 03680000
036900 01  OUTPUT-GMOB-RECORD.                                          03690000
037000     05 FILLER                        PIC X(100).                 03700000
037100     COPY  GCTGMOBC.                                              03710000
037200/                                                                 03720000
037300 01  OUTPUT-GMOR-RECORD.                                          03730000
037400     05 FILLER                        PIC X(100).                 03740000
037500     COPY  GCTGMORC.                                              03750000
037600/                                                                 03760000
037700 01  OUTPUT-GMPB-RECORD.                                          03770000
037800     05  FILLER                            PIC X(100).            03780000
037900     COPY GCTGMPBC.                                               03790000
038000/                                                                 03800000
038100 01  OUTPUT-GMPR-RECORD.                                          03810000
038200     05  FILLER                            PIC X(100).            03820000
038300     COPY GCTGMPRC.                                               03830000
038400/                                                                 03840000
038500 01  OUTPUT-GMSB-RECORD.                                          03850000
038600     05  FILLER                            PIC X(100).            03860000
038700     COPY GCTGMSBC.                                               03870000
038800/                                                                 03880000
038900 01  OUTPUT-GMSC-RECORD.                                          03890000
039000     05  FILLER                            PIC X(100).            03900000
039100     COPY GCTGMSCC.                                               03910000
039200/                                                                 03920000
039300 01  OUTPUT-GMSR-RECORD.                                          03930000
039400     05  FILLER                            PIC X(100).            03940000
039500     COPY GCTGMSRC.                                               03950000
039600/                                                                 03960000
039700 01  OUTPUT-GPAB-RECORD.                                          03970000
039800     05  FILLER                            PIC X(100).            03980000
039900     COPY GCTGPABC.                                               03990000
040000/                                                                 04000000
040100 01  OUTPUT-GPAC-RECORD.                                          04010000
040200     05  FILLER                            PIC X(100).            04020000
040300     COPY GCTGPACC.                                               04030000
040400/                                                                 04040000
040500 01  OUTPUT-GPAD-RECORD.                                          04050000
040600     05  FILLER                            PIC X(100).            04060000
040700     COPY GCTGPADC.                                               04070000
040800/                                                                 04080000
040900 01  OUTPUT-GPAN-RECORD.                                          04090000
041000     05  FILLER                       PIC X(100).                 04100000
041100     COPY GCTGPANC.                                               04110000
041200/                                                                 04120000
041300                                                                  04130000
041400 01  OUTPUT-GPAR-RECORD.                                          04140000
041500     05  FILLER                            PIC X(100).            04150000
041600     COPY GCTGPARC.                                               04160000
041700/                                                                 04170000
041800 01  OUTPUT-GPPO-RECORD.                                          04180000
041900     05  FILLER                            PIC X(100).            04190000
042000     COPY GCTGPPOC.                                               04200000
042100/                                                                 04210000
042200 01  OUTPUT-GRPO-RECORD.                                          04220000
042300     05  FILLER                            PIC X(100).            04230000
042400     COPY GCTGRPOC.                                               04240000
042500/                                                                 04250000
042600 01  OUTPUT-GRID-RECORD.                                          04260000
042700     05  FILLER                            PIC X(100).            04270000
042800     COPY GCTGRIDC.                                               04280000
042900/                                                                 04290000
043000 01  OUTPUT-GVLF-RECORD.                                          04300000
043100     05  FILLER                            PIC X(100).            04310000
043200     COPY GCTGVLFC.                                               04320000
043300/                                                                 04330000
043400 01  OUTPUT-GVLG-RECORD.                                          04340000
043500     05  FILLER                            PIC X(100).            04350000
043600     COPY GCTGVLGC.                                               04360000
043700/                                                                 04370000
043800 01  OUTPUT-GVLH-RECORD.                                          04380000
043900     05  FILLER                            PIC X(100).            04390000
044000     COPY GCTGVLHC.                                               04400000
044100/                                                                 04410000
044200 01  OUTPUT-GVLP-RECORD.                                          04420000
044300     05  FILLER                            PIC X(100).            04430000
044400     COPY GCTGVLPC.                                               04440000
044500/                                                                 04450000
044600 01  OUTPUT-GVLQ-RECORD.                                          04460000
044700     05  FILLER                            PIC X(100).            04470000
044800     COPY GCTGVLQC.                                               04480000
044900/                                                                 04490000
045000 01  OUTPUT-GVLR-RECORD.                                          04500000
045100     05  FILLER                            PIC X(100).            04510000
045200     COPY GCTGVLRC.                                               04520000
045300/                                                                 04530000
045400 01  OUTPUT-GWCD-RECORD.                                          04540000
045500     05  FILLER                            PIC X(100).            04550000
045600     COPY GCTGWCDC.                                               04560000
045700/                                                                 04570000
045800 01  OUTPUT-GBAE-RECORD.                                          04580000
045900     05  FILLER                            PIC X(100).            04590000
046000     COPY GCTGBAEC.                                               04600000
046100/                                                                 04610000
046200 01  OUTPUT-GFHC-RECORD.                                          04620000
046300     05  FILLER                            PIC X(100).            04630000
046400     COPY GCTGFHCC.                                               04640000
046500/                                                                 04650000
046600 01  OUTPUT-GFSA-RECORD.                                          04660000
046700     05  FILLER                            PIC X(100).            04670000
046800     COPY GCTGFSAC.                                               04680000
046900/                                                                 04690000
047000 01  OUTPUT-GHCA-RECORD.                                          04700000
047100     05  FILLER                            PIC X(100).            04710000
047200     COPY GCTGHCAC.                                               04720000
047300/                                                                 04730000
047400 01  OUTPUT-GHSA-RECORD.                                          04740000
047500     05  FILLER                            PIC X(100).            04750000
047600     COPY GCTGHSAC.                                               04760000
047700/                                                                 04770000
047800 01  OUTPUT-GLPF-RECORD.                                          04780000
047900     05  FILLER                            PIC X(100).            04790000
048000     COPY GCTGLPFC.                                               04800000
048100/                                                                 04810000
048200 01  OUTPUT-GLPH-RECORD.                                          04820000
048300     05  FILLER                            PIC X(100).            04830000
048400     COPY GCTGLPHC.                                               04840000
048500/                                                                 04850000
048600 01  OUTPUT-GWHC-RECORD.                                          04860000
048700     05  FILLER                            PIC X(100).            04870000
048800     COPY GCTGWHCC.                                               04880000
048900/                                                                 04890000
049000 01  OUTPUT-GMFH-RECORD.                                          04900000
049100     05  FILLER                            PIC X(100).            04910000
049200     COPY GCTGMFHC.                                               04920000
049300/                                                                 04930000
TM0526 01  OUTPUT-GHPA-RECORD.                                          04930100
TM0526     05  FILLER                            PIC X(100).            04930200
TM0526     COPY GCTGHPAC.                                               04930300
TM0526/                                                                 04930400
049400                                                                  04940000
049500 WORKING-STORAGE SECTION.                                         04950000
049600                                                                  04960000
049700 01  WS-PROGRAM-ID                 PIC  X(27)  VALUE              04970000
049800                                    '* GC0200 WORKING STORAGE *'. 04980000
049900                                                                  04990000
050000 01  WORK-AREAS.                                                  05000000
050100     05  HASH-PROGRAM              PIC X(17) VALUE 'GHS1BAT'.     05010000
050200                                                                  05020000
050300**** GHS1BAT LINKAGE AREA                                         05030000
050400                                                                  05040000
050500 01  WS-GHS1BAT-CALL-AREA.                                        05050000
050600   03  WS-GHS1BAT-PROCESS-IND      PIC X.                         05060000
050700       88  CALL-FOR-OPEN                       VALUE 'O'.         05070000
050800       88  CALL-FOR-CLOSE                      VALUE 'C'.         05080000
050900       88  CALL-FOR-PROCESS                    VALUE 'P'.         05090000
051000                                                                  05100000
051100 01  ACCUM-SLOT-AREA.                                             05110000
051200     05  HASH-RETURN-CODE          PIC X(02).                     05120000
051300     05  ASUR-REC-AREA.                                           05130000
051400         10  ASUR-TAB-ID           PIC X(0006).                   05140000
051500         10  ASUR-SLOT             PIC S9(7)  COMP-3.             05150000
00043 * DM9441   9/08/09  JJS  CHANGED FILLER FOR FILE CONVERSION      *05151002
051600         10  FILLER                PIC X(31360).                  05160001
051700                                                                  05170000
051800****                                                              05180000
051900/                                                                 05190000
052000 01  WS-GSUB-RECORD.                                              05200000
052100     05  FILLER                            PIC X(100).            05210000
052200     COPY GCTGSUB2.                                               05220000
052300/                                                                 05230000
052400 01  WS-GCBL-RECORD.                                              05240000
052500     05  FILLER                            PIC X(100).            05250000
052600     COPY GCTGCBL2.                                               05260000
052700/                                                                 05270000
052800 01  WS-GCCP-RECORD.                                              05280000
052900     05  FILLER                            PIC X(100).            05290000
053000     COPY GCTGCCP2.                                               05300000
053100/                                                                 05310000
053200 01  WS-GCPO-RECORD.                                              05320000
053300     05  FILLER                            PIC X(100).            05330000
053400     COPY GCTGCPO2.                                               05340000
053500/                                                                 05350000
053600 01  WS-GFSB-RECORD.                                              05360000
053700     05  FILLER                            PIC X(100).            05370000
053800     COPY GCTGFSB2.                                               05380000
053900/                                                                 05390000
054000 01  WS-GHOB-RECORD.                                              05400000
054100     05  FILLER                            PIC X(100).            05410000
054200     COPY GCTGHOB2.                                               05420000
054300/                                                                 05430000
054400 01  WS-GHOR-RECORD.                                              05440000
054500     05  FILLER                            PIC X(100).            05450000
054600     COPY GCTGHOR2.                                               05460000
054700/                                                                 05470000
054800 01  WS-GMCD-RECORD.                                              05480000
054900     05  FILLER                            PIC X(100).            05490000
055000     COPY GCTGMCD2.                                               05500000
055100/                                                                 05510000
055200 01  WS-GMCG-RECORD.                                              05520000
055300     05  FILLER                            PIC X(100).            05530000
055400     COPY GCTGMCG2.                                               05540000
055500/                                                                 05550000
055600 01  WS-GMCR-RECORD.                                              05560000
055700     05  FILLER                            PIC X(100).            05570000
055800     COPY GCTGMCR2.                                               05580000
055900/                                                                 05590000
056000 01  WS-GMCS-RECORD.                                              05600000
056100     05  FILLER                            PIC X(100).            05610000
056200     COPY GCTGMCS2.                                               05620000
056300/                                                                 05630000
056400 01  WS-GMCT-RECORD.                                              05640000
056500     05  FILLER                            PIC X(100).            05650000
056600     COPY GCTGMCT2.                                               05660000
056700/                                                                 05670000
056800 01  WS-GMDB-RECORD.                                              05680000
056900     05  FILLER                            PIC X(100).            05690000
057000     COPY GCTGMDB2.                                               05700000
057100/                                                                 05710000
057200 01  WS-GMDN-RECORD.                                              05720000
057300     05  FILLER                            PIC X(100).            05730000
057400     COPY GCTGMDN2.                                               05740000
057500/                                                                 05750000
057600 01  WS-GMOB-RECORD.                                              05760000
057700     05  FILLER                            PIC X(100).            05770000
057800     COPY GCTGMOB2.                                               05780000
057900/                                                                 05790000
058000 01  WS-GMOR-RECORD.                                              05800000
058100     05  FILLER                            PIC X(100).            05810000
058200     COPY GCTGMOR2.                                               05820000
058300/                                                                 05830000
058400 01  WS-GMPB-RECORD.                                              05840000
058500     05  FILLER                            PIC X(100).            05850000
058600     COPY GCTGMPB2.                                               05860000
058700/                                                                 05870000
058800 01  WS-GMPR-RECORD.                                              05880000
058900     05  FILLER                            PIC X(100).            05890000
059000     COPY GCTGMPR2.                                               05900000
059100/                                                                 05910000
059200                                                                  05920000
059300 01  WS-GMSB-RECORD.                                              05930000
059400     05  FILLER                            PIC X(100).            05940000
059500     COPY GCTGMSB2.                                               05950000
059600/                                                                 05960000
059700 01  WS-GMSC-RECORD.                                              05970000
059800     05  FILLER                            PIC X(100).            05980000
059900     COPY GCTGMSC2.                                               05990000
060000/                                                                 06000000
060100 01  WS-GMSR-RECORD.                                              06010000
060200     05  FILLER                            PIC X(100).            06020000
060300     COPY GCTGMSR2.                                               06030000
060400/                                                                 06040000
060500 01  WS-GPAB-RECORD.                                              06050000
060600     05  FILLER                            PIC X(100).            06060000
060700     COPY GCTGPAB2.                                               06070000
060800/                                                                 06080000
060900 01  WS-GPAC-RECORD.                                              06090000
061000     05  FILLER                            PIC X(100).            06100000
061100     COPY GCTGPAC2.                                               06110000
061200/                                                                 06120000
061300 01  WS-GPAD-RECORD.                                              06130000
061400     05  FILLER                            PIC X(100).            06140000
061500     COPY GCTGPAD2.                                               06150000
061600/                                                                 06160000
061700 01  WS-GPAN-RECORD.                                              06170000
061800     05  FILLER                            PIC X(100).            06180000
061900     COPY GCTGPAN2.                                               06190000
062000/                                                                 06200000
062100 01  WS-GPAR-RECORD.                                              06210000
062200     05  FILLER                            PIC X(100).            06220000
062300     COPY GCTGPAR2.                                               06230000
062400/                                                                 06240000
062500 01  WS-GPPO-RECORD.                                              06250000
062600     05  FILLER                            PIC X(100).            06260000
062700     COPY GCTGPPO2.                                               06270000
062800/                                                                 06280000
062900 01  WS-GRPO-RECORD.                                              06290000
063000     05  FILLER                            PIC X(100).            06300000
063100     COPY GCTGRPO2.                                               06310000
063200/                                                                 06320000
063300 01  WS-GRID-RECORD.                                              06330000
063400     05  FILLER                            PIC X(100).            06340000
063500     COPY GCTGRID2.                                               06350000
063600/                                                                 06360000
063700 01  WS-GVLF-RECORD.                                              06370000
063800     05  FILLER                            PIC X(100).            06380000
063900     COPY GCTGVLF2.                                               06390000
064000/                                                                 06400000
064100 01  WS-GVLG-RECORD.                                              06410000
064200     05  FILLER                            PIC X(100).            06420000
064300     COPY GCTGVLG2.                                               06430000
064400/                                                                 06440000
064500 01  WS-GVLH-RECORD.                                              06450000
064600     05  FILLER                            PIC X(100).            06460000
064700     COPY GCTGVLH2.                                               06470000
064800/                                                                 06480000
064900 01  WS-GVLP-RECORD.                                              06490000
065000     05  FILLER                            PIC X(100).            06500000
065100     COPY GCTGVLP2.                                               06510000
065200/                                                                 06520000
065300 01  WS-GVLQ-RECORD.                                              06530000
065400     05  FILLER                            PIC X(100).            06540000
065500     COPY GCTGVLQ2.                                               06550000
065600/                                                                 06560000
065700 01  WS-GVLR-RECORD.                                              06570000
065800     05  FILLER                            PIC X(100).            06580000
065900     COPY GCTGVLR2.                                               06590000
066000/                                                                 06600000
066100 01  WS-GWCD-RECORD.                                              06610000
066200     05  FILLER                            PIC X(100).            06620000
066300     COPY GCTGWCD2.                                               06630000
066400/                                                                 06640000
066500 01  WS-GBAE-RECORD.                                              06650000
066600     05  FILLER                            PIC X(100).            06660000
066700     COPY GCTGBAE2.                                               06670000
066800/                                                                 06680000
066900 01  WS-GFHC-RECORD.                                              06690000
067000     05  FILLER                            PIC X(100).            06700000
067100     COPY GCTGFHC2.                                               06710000
067200/                                                                 06720000
067300 01  WS-GFSA-RECORD.                                              06730000
067400     05  FILLER                            PIC X(100).            06740000
067500     COPY GCTGFSA2.                                               06750000
067600/                                                                 06760000
067700 01  WS-GHCA-RECORD.                                              06770000
067800     05  FILLER                            PIC X(100).            06780000
067900     COPY GCTGHCA2.                                               06790000
068000/                                                                 06800000
068100 01  WS-GHSA-RECORD.                                              06810000
068200     05  FILLER                            PIC X(100).            06820000
068300     COPY GCTGHSA2.                                               06830000
068400/                                                                 06840000
068500 01  WS-GLPF-RECORD.                                              06850000
068600     05  FILLER                            PIC X(100).            06860000
068700     COPY GCTGLPF2.                                               06870000
068800/                                                                 06880000
068900 01  WS-GLPH-RECORD.                                              06890000
069000     05  FILLER                            PIC X(100).            06900000
069100     COPY GCTGLPH2.                                               06910000
069200/                                                                 06920000
069300 01  WS-GWHC-RECORD.                                              06930000
069400     05  FILLER                            PIC X(100).            06940000
069500     COPY GCTGWHC2.                                               06950000
069600/                                                                 06960000
069700 01  WS-GMFH-RECORD.                                              06970000
069800     05  FILLER                            PIC X(100).            06980000
069900     COPY GCTGMFH2.                                               06990000
070000/                                                                 07000000
TM0526 01  WS-GHPA-RECORD.                                              07000100
TM0526     05  FILLER                            PIC X(100).            07000200
TM0526     COPY GCTGHPA2.                                               07000300
TM0526/                                                                 07000400
070100                                                                  07010000
070200                                                                  07020000
070300 01  WS-SWITCHES.                                                 07030000
070400     05  FILLER                    PIC  X(16)  VALUE              07040000
070500                                   '*** SWITCHES ***'.            07050000
070600     05  WS-END-OF-FILE-SW         PIC  X(01)  VALUE '0'.         07060000
070700         88  WS-END-OF-FILE-ON                 VALUE '1'.         07070000
070800                                                                  07080000
070900 01  WS-WORK-AREA.                                                07090000
071000     05  FILLER                    PIC  X(17)  VALUE              07100000
071100                                   '*** WORK AREA ***'.           07110000
071200     05  WS-HOLD-LAST-SLOT         PIC S9(07)  VALUE +0    COMP-3.07120000
071300/                                                                 07130000
071400 PROCEDURE DIVISION.                                              07140000
071500                                                                  07150000
071600******************************************************************07160000
071700**                                                                07170000
071800**               P R O C E S S    C O N T R O L                   07180000
071900**                                                                07190000
072000******************************************************************07200000
072100 0000-MAINLINE.                                                   07210000
072200                                                                  07220000
072300     PERFORM 1000-OPEN-THE-FILES  THRU  1000-EXIT.                07230000
072400                                                                  07240000
072500     IF WS-END-OF-FILE-ON                                         07250000
072600         DISPLAY 'GC0200--NO INPUT RECORDS RECEIVED'              07260000
072700     ELSE                                                         07270000
072800         PERFORM 3000-PROCESS-ALL-INPUT-RECORDS THRU 3000-EXIT    07280000
072900            UNTIL  WS-END-OF-FILE-ON.                             07290000
073000                                                                  07300000
073100     PERFORM 9000-CLOSE-THE-FILES  THRU  9000-EXIT.               07310000
073200                                                                  07320000
073300     STOP RUN.                                                    07330000
073400                                                                  07340000
073500 0000-EXIT.                                                       07350000
073600     EXIT.                                                        07360000
073700/                                                                 07370000
073800******************************************************************07380000
073900***                                                               07390000
074000**       OPEN SEQUENTIAL FILES AND READ THE FIRST RECORD          07400000
074100***                                                               07410000
074200******************************************************************07420000
074300 1000-OPEN-THE-FILES.                                             07430000
074400                                                                  07440000
074500     OPEN INPUT    INPUT-WRK-FILE,                                07450000
074600          OUTPUT   OUTPUT-WRK-FILE.                               07460000
074700                                                                  07470000
074800     MOVE 'O'      TO WS-GHS1BAT-PROCESS-IND.                     07480000
074900     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA.                07490000
075000                                                                  07500000
075100**--- READ THE FIRST RECORD.                                      07510000
075200**                                                                07520000
075300     PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT.                 07530000
075400                                                                  07540000
075500 1000-EXIT.                                                       07550000
075600     EXIT.                                                        07560000
075700/                                                                 07570000
075800******************************************************************07580000
075900**         SEQUENTIAL GROUP SPECIFIC TABULAR FILE                 07590000
076000******************************************************************07600000
076100 2000-READ-INPUT-FILE.                                            07610000
076200                                                                  07620000
076300     READ INPUT-WRK-FILE                                          07630000
076400         AT END                                                   07640000
076500             MOVE  '1'   TO   WS-END-OF-FILE-SW.                  07650000
076600                                                                  07660000
076700 2000-EXIT.                                                       07670000
076800     EXIT.                                                        07680000
076900/                                                                 07690000
077000******************************************************************07700000
077100******************************************************************07710000
077200 3000-PROCESS-ALL-INPUT-RECORDS.                                  07720000
077300                                                                  07730000
077400     DISPLAY ' RECORD ID = ', INPUT-WRK-TAB-REC-ID.               07740000
077500                                                                  07750000
077600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GSUB '                  07760000
077700         PERFORM 3090-PROCESS-GSUB-TABULAR THRU 3090-EXIT         07770000
077800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              07780000
077900     ELSE                                                         07790000
078000                                                                  07800000
078100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GBAE '                  07810000
078200         PERFORM 4820-PROCESS-GBAE-TABULAR THRU 4820-EXIT         07820000
078300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              07830000
078400     ELSE                                                         07840000
078500                                                                  07850000
078600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GCBL '                  07860000
078700         PERFORM 3100-PROCESS-GCBL-TABULAR THRU 3100-EXIT         07870000
078800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              07880000
078900     ELSE                                                         07890000
079000                                                                  07900000
079100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GCCP '                  07910000
079200         PERFORM 3150-PROCESS-GCCP-TABULAR THRU 3150-EXIT         07920000
079300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              07930000
079400     ELSE                                                         07940000
079500                                                                  07950000
079600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GCPO '                  07960000
079700         PERFORM 3110-PROCESS-GCPO-TABULAR THRU 3110-EXIT         07970000
079800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              07980000
079900     ELSE                                                         07990000
080000                                                                  08000000
080100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GFSB '                  08010000
080200         PERFORM 3200-PROCESS-GFSB-TABULAR THRU 3200-EXIT         08020000
080300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08030000
080400     ELSE                                                         08040000
080500                                                                  08050000
080600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GHOB '                  08060000
080700         PERFORM 3300-PROCESS-GHOB-TABULAR THRU 3300-EXIT         08070000
080800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08080000
080900     ELSE                                                         08090000
081000                                                                  08100000
081100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GHOR '                  08110000
081200         PERFORM 3400-PROCESS-GHOR-TABULAR THRU 3400-EXIT         08120000
081300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08130000
081400     ELSE                                                         08140000
081500                                                                  08150000
081600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMCD '                  08160000
081700         PERFORM 3402-PROCESS-GMCD-TABULAR THRU 3402-EXIT         08170000
081800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08180000
081900     ELSE                                                         08190000
082000                                                                  08200000
082100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMCG '                  08210000
082200         PERFORM 3401-PROCESS-GMCG-TABULAR THRU 3401-EXIT         08220000
082300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08230000
082400     ELSE                                                         08240000
082500                                                                  08250000
082600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMCR '                  08260000
082700         PERFORM 3403-PROCESS-GMCR-TABULAR THRU 3403-EXIT         08270000
082800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08280000
082900     ELSE                                                         08290000
083000                                                                  08300000
083100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMCS'                   08310000
083200         PERFORM 3405-PROCESS-GMCS-TABULAR THRU 3405-EXIT         08320000
083300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08330000
083400     ELSE                                                         08340000
083500                                                                  08350000
083600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMCT '                  08360000
083700         PERFORM 3404-PROCESS-GMCT-TABULAR THRU 3404-EXIT         08370000
083800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08380000
083900     ELSE                                                         08390000
084000                                                                  08400000
084100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMDB '                  08410000
084200         PERFORM 3500-PROCESS-GMDB-TABULAR THRU 3500-EXIT         08420000
084300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08430000
084400     ELSE                                                         08440000
084500                                                                  08450000
084600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMDN '                  08460000
084700         PERFORM 3600-PROCESS-GMDN-TABULAR THRU 3600-EXIT         08470000
084800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08480000
084900     ELSE                                                         08490000
085000                                                                  08500000
085100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMOB '                  08510000
085200         PERFORM 3700-PROCESS-GMOB-TABULAR THRU 3700-EXIT         08520000
085300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08530000
085400     ELSE                                                         08540000
085500                                                                  08550000
085600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMOR '                  08560000
085700         PERFORM 3800-PROCESS-GMOR-TABULAR THRU 3800-EXIT         08570000
085800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08580000
085900     ELSE                                                         08590000
086000                                                                  08600000
086100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMPB '                  08610000
086200         PERFORM 3900-PROCESS-GMPB-TABULAR THRU 3900-EXIT         08620000
086300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08630000
086400     ELSE                                                         08640000
086500                                                                  08650000
086600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMPR '                  08660000
086700         PERFORM 4000-PROCESS-GMPR-TABULAR THRU 4000-EXIT         08670000
086800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08680000
086900     ELSE                                                         08690000
087000                                                                  08700000
087100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMSB '                  08710000
087200         PERFORM 4100-PROCESS-GMSB-TABULAR THRU 4100-EXIT         08720000
087300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08730000
087400     ELSE                                                         08740000
087500                                                                  08750000
087600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMSC '                  08760000
087700         PERFORM 4200-PROCESS-GMSC-TABULAR THRU 4200-EXIT         08770000
087800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08780000
087900     ELSE                                                         08790000
088000                                                                  08800000
088100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMSR '                  08810000
088200         PERFORM 4300-PROCESS-GMSR-TABULAR THRU 4300-EXIT         08820000
088300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08830000
088400     ELSE                                                         08840000
088500                                                                  08850000
088600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GPAB '                  08860000
088700         PERFORM 4400-PROCESS-GPAB-TABULAR THRU 4400-EXIT         08870000
088800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08880000
088900     ELSE                                                         08890000
089000                                                                  08900000
089100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GPAC '                  08910000
089200         PERFORM 4500-PROCESS-GPAC-TABULAR THRU 4500-EXIT         08920000
089300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08930000
089400     ELSE                                                         08940000
089500                                                                  08950000
089600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GPAD '                  08960000
089700         PERFORM 4600-PROCESS-GPAD-TABULAR THRU 4600-EXIT         08970000
089800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              08980000
089900     ELSE                                                         08990000
090000                                                                  09000000
090100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GPAN '                  09010000
090200         PERFORM 4655-PROCESS-GPAN-TABULAR THRU 4655-EXIT         09020000
090300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09030000
090400     ELSE                                                         09040000
090500                                                                  09050000
090600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GPAR '                  09060000
090700         PERFORM 4700-PROCESS-GPAR-TABULAR THRU 4700-EXIT         09070000
090800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09080000
090900     ELSE                                                         09090000
091000                                                                  09100000
091100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GPPO '                  09110000
091200         PERFORM 4800-PROCESS-GPPO-TABULAR THRU 4800-EXIT         09120000
091300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09130000
091400     ELSE                                                         09140000
091500                                                                  09150000
091600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GRPO '                  09160000
091700         PERFORM 4810-PROCESS-GRPO-TABULAR THRU 4810-EXIT         09170000
091800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09180000
091900     ELSE                                                         09190000
092000                                                                  09200000
092100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GRID '                  09210000
092200         PERFORM 4900-PROCESS-GRID-TABULAR THRU 4900-EXIT         09220000
092300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09230000
092400     ELSE                                                         09240000
092500                                                                  09250000
092600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GVLF '                  09260000
092700         PERFORM 5000-PROCESS-GVLF-TABULAR THRU 5000-EXIT         09270000
092800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09280000
092900     ELSE                                                         09290000
093000                                                                  09300000
093100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GVLG '                  09310000
093200         PERFORM 5001-PROCESS-GVLG-TABULAR THRU 5001-EXIT         09320000
093300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09330000
093400     ELSE                                                         09340000
093500                                                                  09350000
093600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GVLH '                  09360000
093700         PERFORM 5002-PROCESS-GVLH-TABULAR THRU 5002-EXIT         09370000
093800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09380000
093900     ELSE                                                         09390000
094000                                                                  09400000
094100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GVLP '                  09410000
094200         PERFORM 5100-PROCESS-GVLP-TABULAR THRU 5100-EXIT         09420000
094300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09430000
094400     ELSE                                                         09440000
094500                                                                  09450000
094600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GVLQ '                  09460000
094700         PERFORM 5101-PROCESS-GVLQ-TABULAR THRU 5101-EXIT         09470000
094800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09480000
094900     ELSE                                                         09490000
095000                                                                  09500000
095100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GVLR '                  09510000
095200         PERFORM 5102-PROCESS-GVLR-TABULAR THRU 5102-EXIT         09520000
095300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09530000
095400     ELSE                                                         09540000
095500                                                                  09550000
095600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GWCD '                  09560000
095700         PERFORM 5200-PROCESS-GWCD-TABULAR THRU 5200-EXIT         09570000
095800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09580000
095900     ELSE                                                         09590000
096000                                                                  09600000
096100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GFHC '                  09610000
096200         PERFORM 5300-PROCESS-GFHC-TABULAR THRU 5300-EXIT         09620000
096300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09630000
096400     ELSE                                                         09640000
096500                                                                  09650000
096600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GFSA '                  09660000
096700         PERFORM 5301-PROCESS-GFSA-TABULAR THRU 5301-EXIT         09670000
096800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09680000
096900     ELSE                                                         09690000
097000                                                                  09700000
097100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GHCA '                  09710000
097200         PERFORM 5302-PROCESS-GHCA-TABULAR THRU 5302-EXIT         09720000
097300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09730000
097400     ELSE                                                         09740000
097500                                                                  09750000
097600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GHSA '                  09760000
097700         PERFORM 5303-PROCESS-GHSA-TABULAR THRU 5303-EXIT         09770000
097800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09780000
097900     ELSE                                                         09790000
098000                                                                  09800000
098100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GLPF '                  09810000
098200         PERFORM 5304-PROCESS-GLPF-TABULAR THRU 5304-EXIT         09820000
098300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09830000
098400     ELSE                                                         09840000
098500                                                                  09850000
098600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GLPH '                  09860000
098700         PERFORM 5305-PROCESS-GLPH-TABULAR THRU 5305-EXIT         09870000
098800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09880000
098900     ELSE                                                         09890000
099000                                                                  09900000
099100     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GWHC '                  09910000
099200         PERFORM 5306-PROCESS-GWHC-TABULAR THRU 5306-EXIT         09920000
099300         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09930000
099400     ELSE                                                         09940000
099500                                                                  09950000
099600     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GMFH '                  09960000
099700         PERFORM 5350-PROCESS-GMFH-TABULAR THRU 5350-EXIT         09970000
099800         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09980000
099900     ELSE                                                         09990000
100200                                                                  09990010
TM0526     IF INPUT-WRK-TAB-REC-ID    EQUAL   '#GHPA '                  09990100
TM0526         PERFORM 5360-PROCESS-GHPA-TABULAR THRU 5360-EXIT         09990200
TM0526         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              09990300
TM0526     ELSE                                                         09990400
100000                                                                  10000000
100100         PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT.             10010000
100200                                                                  10020000
100300 3000-EXIT.                                                       10030000
100400     EXIT.                                                        10040000
100500/                                                                 10050000
100600******************************************************************10060000
100700****           GROUP SPECIFIC   'GSUB'   TABULAR                  10070000
100800******************************************************************10080000
100900 3090-PROCESS-GSUB-TABULAR.                                       10090000
101000                                                                  10100000
101100***************************************************************   10110000
101200***** IF SLOT IS LESS THAN 9,000,000                      *****   10120000
101300***** BYPASS THIS RECORD.                                 *****   10130000
101400***************************************************************   10140000
101500                                                                  10150000
101600     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                10160000
101700         GO TO 3090-EXIT.                                         10170000
101800                                                                  10180000
101900***************************************************************   10190000
102000***** CALL TABULAR HASHING PROGRAM                        *****   10200000
102100***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   10210000
102200***************************************************************   10220000
102300                                                                  10230000
102400     MOVE LOW-VALUES TO ASUR-REC-AREA.                            10240000
102500*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              10250000
102600     MOVE GS113-RECORD             TO ASUR-REC-AREA.              10260000
102700     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          10270000
102800     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 10280000
102900                             ACCUM-SLOT-AREA.                     10290000
103000                                                                  10300000
103100***************************************************************   10310000
103200***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   10320000
103300***************************************************************   10330000
103400                                                                  10340000
103500     IF HASH-RETURN-CODE = '00'                                   10350000
103600        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  10360000
103700     ELSE                                                         10370000
103800        IF HASH-RETURN-CODE = '01'                                10380000
103900           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  10390000
104000        ELSE                                                      10400000
104100           GO TO 3090-EXIT.                                       10410000
104200                                                                  10420000
104300     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            10430000
104400     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   10440000
104500                                                                  10450000
104600***************************************************************   10460000
104700***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   10470000
104800***** THE SEQUENTIAL FILE.                                *****   10480000
104900***************************************************************   10490000
105000                                                                  10500000
105100     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         10510000
105200     MOVE INPUT-GSUB-RECORD         TO OUTPUT-WRK-RECORD.         10520000
105300     MOVE ASUR-SLOT                 TO GS11-PROVISION-SLOT-NO.    10530000
105400     WRITE OUTPUT-GSUB-RECORD.                                    10540000
105500                                                                  10550000
105600 3090-EXIT.                                                       10560000
105700     EXIT.                                                        10570000
105800/                                                                 10580000
105900******************************************************************10590000
106000****           GROUP SPECIFIC   'GCBL'   TABULAR                  10600000
106100******************************************************************10610000
106200 3100-PROCESS-GCBL-TABULAR.                                       10620000
106300                                                                  10630000
106400***************************************************************   10640000
106500***** IF SLOT IS LESS THAN 9,000,000                      *****   10650000
106600***** BYPASS THIS RECORD.                                 *****   10660000
106700***************************************************************   10670000
106800                                                                  10680000
106900     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                10690000
107000         GO TO 3100-EXIT.                                         10700000
107100                                                                  10710000
107200**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           10720000
107300**                                                                10730000
107400     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          10740000
107500     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        10750000
107600         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        10760000
107700         MOVE INPUT-WRK-TAB-REC-CNT  TO  GS9-ENTRY-COUNT          10770000
107800         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL10780000
107900         MOVE INPUT-GCBL-RECORD      TO  OUTPUT-WRK-RECORD        10790000
108000         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GS9-PROVISION-SLOT-NO  10800000
108100         WRITE  OUTPUT-GCBL-RECORD                                10810000
108200         GO TO 3100-EXIT.                                         10820000
108300                                                                  10830000
108400***************************************************************   10840000
108500***** CALL TABULAR HASHING PROGRAM                        *****   10850000
108600***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   10860000
108700***************************************************************   10870000
108800                                                                  10880000
108900     MOVE LOW-VALUES TO ASUR-REC-AREA.                            10890000
109000*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              10900000
109100     MOVE GS93-RECORD              TO ASUR-REC-AREA.              10910000
109200     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          10920000
109300     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 10930000
109400                             ACCUM-SLOT-AREA.                     10940000
109500                                                                  10950000
109600***************************************************************   10960000
109700***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   10970000
109800***************************************************************   10980000
109900                                                                  10990000
110000     IF HASH-RETURN-CODE = '00'                                   11000000
110100        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  11010000
110200     ELSE                                                         11020000
110300        IF HASH-RETURN-CODE = '01'                                11030000
110400           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  11040000
110500        ELSE                                                      11050000
110600           GO TO 3100-EXIT.                                       11060000
110700                                                                  11070000
110800     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            11080000
110900     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   11090000
111000                                                                  11100000
111100***************************************************************   11110000
111200***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   11120000
111300***** THE SEQUENTIAL FILE.                                *****   11130000
111400***************************************************************   11140000
111500                                                                  11150000
111600     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         11160000
111700     MOVE INPUT-WRK-TAB-REC-CNT     TO GS9-ENTRY-COUNT.           11170000
111800     MOVE INPUT-GCBL-RECORD         TO OUTPUT-WRK-RECORD.         11180000
111900     MOVE ASUR-SLOT                 TO GS9-PROVISION-SLOT-NO.     11190000
112000     WRITE OUTPUT-GCBL-RECORD.                                    11200000
112100                                                                  11210000
112200 3100-EXIT.                                                       11220000
112300     EXIT.                                                        11230000
112400/                                                                 11240000
112500******************************************************************11250000
112600****           GROUP SPECIFIC   'GCCP'   TABULAR                  11260000
112700******************************************************************11270000
112800 3150-PROCESS-GCCP-TABULAR.                                       11280000
112900                                                                  11290000
113000***************************************************************   11300000
113100***** IF SLOT IS LESS THAN 9,000,000                      *****   11310000
113200***** BYPASS THIS RECORD.                                 *****   11320000
113300***************************************************************   11330000
113400                                                                  11340000
113500     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                11350000
113600         GO TO 3150-EXIT.                                         11360000
113700                                                                  11370000
113800***************************************************************   11380000
113900***** CALL TABULAR HASHING PROGRAM                        *****   11390000
114000***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   11400000
114100***************************************************************   11410000
114200                                                                  11420000
114300     MOVE LOW-VALUES TO ASUR-REC-AREA.                            11430000
114400*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              11440000
114500     MOVE GSS3-COST-CONT-RECORD    TO ASUR-REC-AREA.              11450000
114600     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          11460000
114700     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 11470000
114800                             ACCUM-SLOT-AREA.                     11480000
114900                                                                  11490000
115000***************************************************************   11500000
115100***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   11510000
115200***************************************************************   11520000
115300                                                                  11530000
115400     IF HASH-RETURN-CODE = '00'                                   11540000
115500        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  11550000
115600     ELSE                                                         11560000
115700        IF HASH-RETURN-CODE = '01'                                11570000
115800           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  11580000
115900        ELSE                                                      11590000
116000           GO TO 3150-EXIT.                                       11600000
116100                                                                  11610000
116200     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            11620000
116300     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   11630000
116400                                                                  11640000
116500***************************************************************   11650000
116600***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   11660000
116700***** THE SEQUENTIAL FILE.                                *****   11670000
116800***************************************************************   11680000
116900                                                                  11690000
117000     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         11700000
117100     MOVE INPUT-WRK-TAB-REC-CNT     TO GSS-ENTRY-COUNT.           11710000
117200     MOVE INPUT-GCCP-RECORD         TO OUTPUT-WRK-RECORD.         11720000
117300*    MOVE ASUR-SLOT                 TO GSS-PROVISION-SLOT-NO.     11730000
117400     MOVE ASUR-SLOT                 TO GSS-COST-TAB-SLOT-NO.      11740000
117500     WRITE OUTPUT-GCCP-RECORD.                                    11750000
117600                                                                  11760000
117700 3150-EXIT.                                                       11770000
117800     EXIT.                                                        11780000
117900/                                                                 11790000
118000******************************************************************11800000
118100****           GROUP SPECIFIC   'GCPO'   TABULAR                  11810000
118200******************************************************************11820000
118300 3110-PROCESS-GCPO-TABULAR.                                       11830000
118400                                                                  11840000
118500***************************************************************   11850000
118600***** IF SLOT IS LESS THAN 9,000,000                      *****   11860000
118700***** BYPASS THIS RECORD.                                 *****   11870000
118800***************************************************************   11880000
118900                                                                  11890000
119000     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                11900000
119100         GO TO 3110-EXIT.                                         11910000
119200                                                                  11920000
119300**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           11930000
119400**                                                                11940000
119500     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          11950000
119600     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        11960000
119700         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        11970000
119800         MOVE INPUT-WRK-TAB-REC-CNT  TO  GS8-ENTRY-COUNT          11980000
119900         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL11990000
120000         MOVE INPUT-GCPO-RECORD      TO  OUTPUT-WRK-RECORD        12000000
120100         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GS8-PROVISION-SLOT-NO  12010000
120200         WRITE  OUTPUT-GCPO-RECORD                                12020000
120300         GO TO 3110-EXIT.                                         12030000
120400                                                                  12040000
120500***************************************************************   12050000
120600***** CALL TABULAR HASHING PROGRAM                        *****   12060000
120700***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   12070000
120800***************************************************************   12080000
120900                                                                  12090000
121000     MOVE LOW-VALUES TO ASUR-REC-AREA.                            12100000
121100*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              12110000
121200     MOVE GS83-RECORD              TO ASUR-REC-AREA.              12120000
121300     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          12130000
121400     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 12140000
121500                             ACCUM-SLOT-AREA.                     12150000
121600                                                                  12160000
121700***************************************************************   12170000
121800***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   12180000
121900***************************************************************   12190000
122000                                                                  12200000
122100     IF HASH-RETURN-CODE = '00'                                   12210000
122200        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  12220000
122300     ELSE                                                         12230000
122400        IF HASH-RETURN-CODE = '01'                                12240000
122500           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  12250000
122600        ELSE                                                      12260000
122700           GO TO 3110-EXIT.                                       12270000
122800                                                                  12280000
122900     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            12290000
123000     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   12300000
123100                                                                  12310000
123200***************************************************************   12320000
123300***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   12330000
123400***** THE SEQUENTIAL FILE.                                *****   12340000
123500***************************************************************   12350000
123600                                                                  12360000
123700     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         12370000
123800     MOVE INPUT-WRK-TAB-REC-CNT     TO GS8-ENTRY-COUNT.           12380000
123900     MOVE INPUT-GCPO-RECORD         TO OUTPUT-WRK-RECORD.         12390000
124000     MOVE ASUR-SLOT                 TO GS8-PROVISION-SLOT-NO.     12400000
124100     WRITE OUTPUT-GCPO-RECORD.                                    12410000
124200                                                                  12420000
124300 3110-EXIT.                                                       12430000
124400     EXIT.                                                        12440000
124500/                                                                 12450000
124600******************************************************************12460000
124700****          GROUP SPECIFIC   'GFSB'   TABULAR                   12470000
124800******************************************************************12480000
124900 3200-PROCESS-GFSB-TABULAR.                                       12490000
125000                                                                  12500000
125100***************************************************************   12510000
125200***** IF SLOT IS LESS THAN 9,000,000                      *****   12520000
125300***** BYPASS THIS RECORD.                                 *****   12530000
125400***************************************************************   12540000
125500                                                                  12550000
125600     IF INPUT-WRK-TAB-REC-SLOT    NOT  >     +8999999             12560000
125700         GO TO 3200-EXIT.                                         12570000
125800                                                                  12580000
125900***************************************************************   12590000
126000***** CALL TABULAR HASHING PROGRAM                        *****   12600000
126100***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   12610000
126200***************************************************************   12620000
126300                                                                  12630000
126400     MOVE LOW-VALUES TO ASUR-REC-AREA.                            12640000
126500*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              12650000
126600     MOVE GSU3-RECORD              TO ASUR-REC-AREA.              12660000
126700     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          12670000
126800     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 12680000
126900                             ACCUM-SLOT-AREA.                     12690000
127000                                                                  12700000
127100***************************************************************   12710000
127200***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   12720000
127300***************************************************************   12730000
127400                                                                  12740000
127500     IF HASH-RETURN-CODE = '00'                                   12750000
127600        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  12760000
127700     ELSE                                                         12770000
127800        IF HASH-RETURN-CODE = '01'                                12780000
127900           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  12790000
128000        ELSE                                                      12800000
128100           GO TO 3200-EXIT.                                       12810000
128200                                                                  12820000
128300     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            12830000
128400     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   12840000
128500                                                                  12850000
128600***************************************************************   12860000
128700***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   12870000
128800***** THE SEQUENTIAL FILE.                                *****   12880000
128900***************************************************************   12890000
129000                                                                  12900000
129100     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         12910000
129200     MOVE INPUT-WRK-TAB-REC-CNT     TO GSU-ENTRY-COUNT.           12920000
129300     MOVE INPUT-GFSB-RECORD         TO OUTPUT-WRK-RECORD.         12930000
129400     MOVE ASUR-SLOT                 TO GSU-PROVISION-SLOT-NO.     12940000
129500     WRITE OUTPUT-GFSB-RECORD.                                    12950000
129600                                                                  12960000
129700 3200-EXIT.                                                       12970000
129800     EXIT.                                                        12980000
129900/                                                                 12990000
130000******************************************************************13000000
130100****          GROUP SPECIFIC   'GHOB'   TABULAR                   13010000
130200******************************************************************13020000
130300 3300-PROCESS-GHOB-TABULAR.                                       13030000
130400                                                                  13040000
130500***************************************************************   13050000
130600***** IF SLOT IS LESS THAN 9,000,000                      *****   13060000
130700***** BYPASS THIS RECORD.                                 *****   13070000
130800***************************************************************   13080000
130900                                                                  13090000
131000     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                13100000
131100         GO TO 3300-EXIT.                                         13110000
131200                                                                  13120000
131300***************************************************************   13130000
131400***** CALL TABULAR HASHING PROGRAM                        *****   13140000
131500***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   13150000
131600***************************************************************   13160000
131700                                                                  13170000
131800     MOVE LOW-VALUES TO ASUR-REC-AREA.                            13180000
131900*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              13190000
132000     MOVE GSA3-RECORD              TO ASUR-REC-AREA.              13200000
132100     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          13210000
132200     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 13220000
132300                             ACCUM-SLOT-AREA.                     13230000
132400                                                                  13240000
132500***************************************************************   13250000
132600***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   13260000
132700***************************************************************   13270000
132800                                                                  13280000
132900     IF HASH-RETURN-CODE = '00'                                   13290000
133000        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  13300000
133100     ELSE                                                         13310000
133200        IF HASH-RETURN-CODE = '01'                                13320000
133300           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  13330000
133400        ELSE                                                      13340000
133500           GO TO 3300-EXIT.                                       13350000
133600                                                                  13360000
133700     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            13370000
133800     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   13380000
133900                                                                  13390000
134000***************************************************************   13400000
134100***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   13410000
134200***** THE SEQUENTIAL FILE.                                *****   13420000
134300***************************************************************   13430000
134400                                                                  13440000
134500     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         13450000
134600     MOVE INPUT-WRK-TAB-REC-CNT     TO GSA-ENTRY-COUNT.           13460000
134700     MOVE INPUT-GHOB-RECORD         TO OUTPUT-WRK-RECORD.         13470000
134800     MOVE ASUR-SLOT                 TO GSA-PROVISION-SLOT-NO.     13480000
134900     WRITE OUTPUT-GHOB-RECORD.                                    13490000
135000                                                                  13500000
135100 3300-EXIT.                                                       13510000
135200     EXIT.                                                        13520000
135300/                                                                 13530000
135400******************************************************************13540000
135500****          GROUP SPECIFIC   'GHOR'   TABULAR                   13550000
135600******************************************************************13560000
135700 3400-PROCESS-GHOR-TABULAR.                                       13570000
135800                                                                  13580000
135900***************************************************************   13590000
136000***** IF SLOT IS LESS THAN 9,000,000                      *****   13600000
136100***** BYPASS THIS RECORD.                                 *****   13610000
136200***************************************************************   13620000
136300                                                                  13630000
136400     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                13640000
136500         GO TO 3400-EXIT.                                         13650000
136600                                                                  13660000
136700***************************************************************   13670000
136800***** CALL TABULAR HASHING PROGRAM                        *****   13680000
136900***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   13690000
137000***************************************************************   13700000
137100                                                                  13710000
137200     MOVE LOW-VALUES TO ASUR-REC-AREA.                            13720000
137300*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              13730000
137400     MOVE GSO3-RECORD              TO ASUR-REC-AREA.              13740000
137500     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          13750000
137600     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 13760000
137700                             ACCUM-SLOT-AREA.                     13770000
137800                                                                  13780000
137900***************************************************************   13790000
138000***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   13800000
138100***************************************************************   13810000
138200                                                                  13820000
138300     IF HASH-RETURN-CODE = '00'                                   13830000
138400        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  13840000
138500     ELSE                                                         13850000
138600        IF HASH-RETURN-CODE = '01'                                13860000
138700           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  13870000
138800        ELSE                                                      13880000
138900           GO TO 3400-EXIT.                                       13890000
139000                                                                  13900000
139100     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            13910000
139200     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   13920000
139300                                                                  13930000
139400***************************************************************   13940000
139500***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   13950000
139600***** THE SEQUENTIAL FILE.                                *****   13960000
139700***************************************************************   13970000
139800                                                                  13980000
139900     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         13990000
140000     MOVE INPUT-WRK-TAB-REC-CNT     TO GSO-ENTRY-COUNT.           14000000
140100     MOVE INPUT-GHOR-RECORD         TO OUTPUT-WRK-RECORD.         14010000
140200     MOVE ASUR-SLOT                 TO GSO-PROVISION-SLOT-NO.     14020000
140300     WRITE OUTPUT-GHOR-RECORD.                                    14030000
140400                                                                  14040000
140500 3400-EXIT.                                                       14050000
140600     EXIT.                                                        14060000
140700/                                                                 14070000
140800******************************************************************14080000
140900****          GROUP SPECIFIC   'GMCG'   TABULAR                   14090000
141000******************************************************************14100000
141100 3401-PROCESS-GMCG-TABULAR.                                       14110000
141200                                                                  14120000
141300***************************************************************   14130000
141400***** IF SLOT IS LESS THAN 9,000,000                      *****   14140000
141500***** BYPASS THIS RECORD.                                 *****   14150000
141600***************************************************************   14160000
141700                                                                  14170000
141800     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                14180000
141900         GO TO 3401-EXIT.                                         14190000
142000                                                                  14200000
142100***************************************************************   14210000
142200***** CALL TABULAR HASHING PROGRAM                        *****   14220000
142300***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   14230000
142400***************************************************************   14240000
142500                                                                  14250000
142600     MOVE LOW-VALUES TO ASUR-REC-AREA.                            14260000
142700*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              14270000
142800     MOVE GS13-RECORD              TO ASUR-REC-AREA.              14280000
142900     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          14290000
143000     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 14300000
143100                             ACCUM-SLOT-AREA.                     14310000
143200                                                                  14320000
143300***************************************************************   14330000
143400***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   14340000
143500***************************************************************   14350000
143600                                                                  14360000
143700     IF HASH-RETURN-CODE = '00'                                   14370000
143800        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  14380000
143900     ELSE                                                         14390000
144000        IF HASH-RETURN-CODE = '01'                                14400000
144100           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  14410000
144200        ELSE                                                      14420000
144300           GO TO 3401-EXIT.                                       14430000
144400                                                                  14440000
144500     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            14450000
144600     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   14460000
144700                                                                  14470000
144800***************************************************************   14480000
144900***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   14490000
145000***** THE SEQUENTIAL FILE.                                *****   14500000
145100***************************************************************   14510000
145200                                                                  14520000
145300     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         14530000
145400     MOVE INPUT-WRK-TAB-REC-CNT     TO GS1-ENTRY-COUNT.           14540000
145500     MOVE INPUT-GMCG-RECORD         TO OUTPUT-WRK-RECORD.         14550000
145600     MOVE ASUR-SLOT                 TO GS1-PROVISION-SLOT-NO.     14560000
145700     WRITE OUTPUT-GMCG-RECORD.                                    14570000
145800                                                                  14580000
145900 3401-EXIT.                                                       14590000
146000     EXIT.                                                        14600000
146100                                                                  14610000
146200/*****************************************************************14620000
146300****          GROUP SPECIFIC   'GMCD'   TABULAR                   14630000
146400******************************************************************14640000
146500 3402-PROCESS-GMCD-TABULAR.                                       14650000
146600                                                                  14660000
146700***************************************************************   14670000
146800***** IF SLOT IS LESS THAN 9,000,000                      *****   14680000
146900***** BYPASS THIS RECORD.                                 *****   14690000
147000***************************************************************   14700000
147100                                                                  14710000
147200     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                14720000
147300         GO TO 3402-EXIT.                                         14730000
147400                                                                  14740000
147500***************************************************************   14750000
147600***** CALL TABULAR HASHING PROGRAM                        *****   14760000
147700***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   14770000
147800***************************************************************   14780000
147900                                                                  14790000
148000     MOVE LOW-VALUES TO ASUR-REC-AREA.                            14800000
148100*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              14810000
148200     MOVE GS23-RECORD              TO ASUR-REC-AREA.              14820000
148300     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          14830000
148400     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 14840000
148500                             ACCUM-SLOT-AREA.                     14850000
148600                                                                  14860000
148700***************************************************************   14870000
148800***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   14880000
148900***************************************************************   14890000
149000                                                                  14900000
149100     IF HASH-RETURN-CODE = '00'                                   14910000
149200        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  14920000
149300     ELSE                                                         14930000
149400        IF HASH-RETURN-CODE = '01'                                14940000
149500           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  14950000
149600        ELSE                                                      14960000
149700           GO TO 3402-EXIT.                                       14970000
149800                                                                  14980000
149900     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            14990000
150000     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   15000000
150100                                                                  15010000
150200***************************************************************   15020000
150300***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   15030000
150400***** THE SEQUENTIAL FILE.                                *****   15040000
150500***************************************************************   15050000
150600                                                                  15060000
150700     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         15070000
150800     MOVE INPUT-WRK-TAB-REC-CNT     TO GS2-ENTRY-COUNT.           15080000
150900     MOVE INPUT-GMCD-RECORD         TO OUTPUT-WRK-RECORD.         15090000
151000     MOVE ASUR-SLOT                 TO GS2-PROVISION-SLOT-NO.     15100000
151100     WRITE OUTPUT-GMCD-RECORD.                                    15110000
151200                                                                  15120000
151300 3402-EXIT.                                                       15130000
151400     EXIT.                                                        15140000
151500                                                                  15150000
151600/*****************************************************************15160000
151700****          GROUP SPECIFIC   'GMCR'   TABULAR                   15170000
151800******************************************************************15180000
151900 3403-PROCESS-GMCR-TABULAR.                                       15190000
152000                                                                  15200000
152100***************************************************************   15210000
152200***** IF SLOT IS LESS THAN 9,000,000                      *****   15220000
152300***** BYPASS THIS RECORD.                                 *****   15230000
152400***************************************************************   15240000
152500                                                                  15250000
152600     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                15260000
152700         GO TO 3403-EXIT.                                         15270000
152800                                                                  15280000
152900***************************************************************   15290000
153000***** CALL TABULAR HASHING PROGRAM                        *****   15300000
153100***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   15310000
153200***************************************************************   15320000
153300                                                                  15330000
153400     MOVE LOW-VALUES TO ASUR-REC-AREA.                            15340000
153500*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              15350000
153600     MOVE GS43-RECORD              TO ASUR-REC-AREA.              15360000
153700     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          15370000
153800     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 15380000
153900                             ACCUM-SLOT-AREA.                     15390000
154000                                                                  15400000
154100***************************************************************   15410000
154200***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   15420000
154300***************************************************************   15430000
154400                                                                  15440000
154500     IF HASH-RETURN-CODE = '00'                                   15450000
154600        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  15460000
154700     ELSE                                                         15470000
154800        IF HASH-RETURN-CODE = '01'                                15480000
154900           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  15490000
155000        ELSE                                                      15500000
155100           GO TO 3403-EXIT.                                       15510000
155200                                                                  15520000
155300     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            15530000
155400     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   15540000
155500                                                                  15550000
155600***************************************************************   15560000
155700***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   15570000
155800***** THE SEQUENTIAL FILE.                                *****   15580000
155900***************************************************************   15590000
156000                                                                  15600000
156100     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         15610000
156200     MOVE INPUT-WRK-TAB-REC-CNT     TO GS4-ENTRY-COUNT.           15620000
156300     MOVE INPUT-GMCR-RECORD         TO OUTPUT-WRK-RECORD.         15630000
156400     MOVE ASUR-SLOT                 TO GS4-PROVISION-SLOT-NO.     15640000
156500     WRITE OUTPUT-GMCR-RECORD.                                    15650000
156600                                                                  15660000
156700 3403-EXIT.                                                       15670000
156800     EXIT.                                                        15680000
156900                                                                  15690000
157000/*****************************************************************15700000
157100****          GROUP SPECIFIC   'GMCT'   TABULAR                   15710000
157200******************************************************************15720000
157300 3404-PROCESS-GMCT-TABULAR.                                       15730000
157400                                                                  15740000
157500***************************************************************   15750000
157600***** IF SLOT IS LESS THAN 9,000,000                      *****   15760000
157700***** BYPASS THIS RECORD.                                 *****   15770000
157800***************************************************************   15780000
157900                                                                  15790000
158000     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                15800000
158100         GO TO 3404-EXIT.                                         15810000
158200                                                                  15820000
158300***************************************************************   15830000
158400***** CALL TABULAR HASHING PROGRAM                        *****   15840000
158500***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   15850000
158600***************************************************************   15860000
158700                                                                  15870000
158800     MOVE LOW-VALUES TO ASUR-REC-AREA.                            15880000
158900*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              15890000
159000     MOVE GS53-RECORD              TO ASUR-REC-AREA.              15900000
159100     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          15910000
159200     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 15920000
159300                             ACCUM-SLOT-AREA.                     15930000
159400                                                                  15940000
159500***************************************************************   15950000
159600***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   15960000
159700***************************************************************   15970000
159800                                                                  15980000
159900     IF HASH-RETURN-CODE = '00'                                   15990000
160000        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  16000000
160100     ELSE                                                         16010000
160200        IF HASH-RETURN-CODE = '01'                                16020000
160300           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  16030000
160400        ELSE                                                      16040000
160500           GO TO 3404-EXIT.                                       16050000
160600                                                                  16060000
160700     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            16070000
160800     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   16080000
160900                                                                  16090000
161000***************************************************************   16100000
161100***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   16110000
161200***** THE SEQUENTIAL FILE.                                *****   16120000
161300***************************************************************   16130000
161400                                                                  16140000
161500     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         16150000
161600     MOVE INPUT-WRK-TAB-REC-CNT     TO GS5-ENTRY-COUNT.           16160000
161700     MOVE INPUT-GMCT-RECORD         TO OUTPUT-WRK-RECORD.         16170000
161800     MOVE ASUR-SLOT                 TO GS5-PROVISION-SLOT-NO.     16180000
161900     WRITE OUTPUT-GMCT-RECORD.                                    16190000
162000                                                                  16200000
162100 3404-EXIT.                                                       16210000
162200     EXIT.                                                        16220000
162300                                                                  16230000
162400******************************************************************16240000
162500****          GROUP SPECIFIC   'GMCS'   TABULAR                   16250000
162600******************************************************************16260000
162700 3405-PROCESS-GMCS-TABULAR.                                       16270000
162800                                                                  16280000
162900***************************************************************   16290000
163000***** IF SLOT IS LESS THAN 9,000,000                      *****   16300000
163100***** BYPASS THIS RECORD.                                 *****   16310000
163200***************************************************************   16320000
163300                                                                  16330000
163400     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                16340000
163500         GO TO 3405-EXIT.                                         16350000
163600                                                                  16360000
163700***************************************************************   16370000
163800***** CALL TABULAR HASHING PROGRAM                        *****   16380000
163900***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   16390000
164000***************************************************************   16400000
164100                                                                  16410000
164200     MOVE LOW-VALUES TO ASUR-REC-AREA.                            16420000
164300*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              16430000
164400     MOVE GS63-RECORD              TO ASUR-REC-AREA.              16440000
164500     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          16450000
164600     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 16460000
164700                             ACCUM-SLOT-AREA.                     16470000
164800                                                                  16480000
164900***************************************************************   16490000
165000***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   16500000
165100***************************************************************   16510000
165200                                                                  16520000
165300     IF HASH-RETURN-CODE = '00'                                   16530000
165400        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  16540000
165500     ELSE                                                         16550000
165600        IF HASH-RETURN-CODE = '01'                                16560000
165700           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  16570000
165800        ELSE                                                      16580000
165900           GO TO 3405-EXIT.                                       16590000
166000                                                                  16600000
166100     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            16610000
166200     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   16620000
166300                                                                  16630000
166400***************************************************************   16640000
166500***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   16650000
166600***** THE SEQUENTIAL FILE.                                *****   16660000
166700***************************************************************   16670000
166800                                                                  16680000
166900     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         16690000
167000     MOVE INPUT-WRK-TAB-REC-CNT     TO GS6-ENTRY-COUNT.           16700000
167100     MOVE INPUT-GMCS-RECORD         TO OUTPUT-WRK-RECORD.         16710000
167200     MOVE ASUR-SLOT                 TO GS6-PROVISION-SLOT-NO.     16720000
167300     WRITE OUTPUT-GMCS-RECORD.                                    16730000
167400                                                                  16740000
167500 3405-EXIT.                                                       16750000
167600     EXIT.                                                        16760000
167700/                                                                 16770000
167800/*****************************************************************16780000
167900****           GROUP SPECIFIC   'GMDB'   TABULAR                  16790000
168000******************************************************************16800000
168100 3500-PROCESS-GMDB-TABULAR.                                       16810000
168200                                                                  16820000
168300***************************************************************   16830000
168400***** IF SLOT IS LESS THAN 9,000,000                      *****   16840000
168500***** BYPASS THIS RECORD.                                 *****   16850000
168600***************************************************************   16860000
168700                                                                  16870000
168800     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                16880000
168900         GO TO 3500-EXIT.                                         16890000
169000                                                                  16900000
169100***************************************************************   16910000
169200***** CALL TABULAR HASHING PROGRAM                        *****   16920000
169300***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   16930000
169400***************************************************************   16940000
169500                                                                  16950000
169600     MOVE LOW-VALUES TO ASUR-REC-AREA.                            16960000
169700*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              16970000
169800     MOVE GSV3-RECORD              TO ASUR-REC-AREA.              16980000
169900     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          16990000
170000     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 17000000
170100                             ACCUM-SLOT-AREA.                     17010000
170200                                                                  17020000
170300***************************************************************   17030000
170400***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   17040000
170500***************************************************************   17050000
170600                                                                  17060000
170700     IF HASH-RETURN-CODE = '00'                                   17070000
170800        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  17080000
170900     ELSE                                                         17090000
171000        IF HASH-RETURN-CODE = '01'                                17100000
171100           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  17110000
171200        ELSE                                                      17120000
171300           GO TO 3500-EXIT.                                       17130000
171400                                                                  17140000
171500     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            17150000
171600     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   17160000
171700                                                                  17170000
171800***************************************************************   17180000
171900***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   17190000
172000***** THE SEQUENTIAL FILE.                                *****   17200000
172100***************************************************************   17210000
172200                                                                  17220000
172300     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         17230000
172400     MOVE INPUT-WRK-TAB-REC-CNT     TO GSV-ENTRY-COUNT.           17240000
172500     MOVE INPUT-GMDB-RECORD         TO OUTPUT-WRK-RECORD.         17250000
172600     MOVE ASUR-SLOT                 TO GSV-PROVISION-SLOT-NO.     17260000
172700     WRITE OUTPUT-GMDB-RECORD.                                    17270000
172800                                                                  17280000
172900 3500-EXIT.                                                       17290000
173000     EXIT.                                                        17300000
173100/                                                                 17310000
173200******************************************************************17320000
173300****          GROUP SPECIFIC   'GMDN'   TABULAR                   17330000
173400******************************************************************17340000
173500 3600-PROCESS-GMDN-TABULAR.                                       17350000
173600                                                                  17360000
173700***************************************************************   17370000
173800***** IF SLOT IS LESS THAN 9,000,000                      *****   17380000
173900***** BYPASS THIS RECORD.                                 *****   17390000
174000***************************************************************   17400000
174100                                                                  17410000
174200     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                17420000
174300         GO TO 3600-EXIT.                                         17430000
174400                                                                  17440000
174500***************************************************************   17450000
174600***** CALL TABULAR HASHING PROGRAM                        *****   17460000
174700***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   17470000
174800***************************************************************   17480000
174900                                                                  17490000
175000     MOVE LOW-VALUES TO ASUR-REC-AREA.                            17500000
175100*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              17510000
175200     MOVE GS33-RECORD              TO ASUR-REC-AREA.              17520000
175300     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          17530000
175400     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 17540000
175500                             ACCUM-SLOT-AREA.                     17550000
175600                                                                  17560000
175700***************************************************************   17570000
175800***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   17580000
175900***************************************************************   17590000
176000                                                                  17600000
176100     IF HASH-RETURN-CODE = '00'                                   17610000
176200        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  17620000
176300     ELSE                                                         17630000
176400        IF HASH-RETURN-CODE = '01'                                17640000
176500           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  17650000
176600        ELSE                                                      17660000
176700           GO TO 3600-EXIT.                                       17670000
176800                                                                  17680000
176900     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            17690000
177000     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   17700000
177100                                                                  17710000
177200***************************************************************   17720000
177300***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   17730000
177400***** THE SEQUENTIAL FILE.                                *****   17740000
177500***************************************************************   17750000
177600                                                                  17760000
177700     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         17770000
177800     MOVE INPUT-WRK-TAB-REC-CNT     TO GS3-ENTRY-COUNT.           17780000
177900     MOVE INPUT-GMDN-RECORD         TO OUTPUT-WRK-RECORD.         17790000
178000     MOVE ASUR-SLOT                 TO GS3-PROVISION-SLOT-NO.     17800000
178100     WRITE OUTPUT-GMDN-RECORD.                                    17810000
178200                                                                  17820000
178300 3600-EXIT.                                                       17830000
178400     EXIT.                                                        17840000
178500/                                                                 17850000
178600******************************************************************17860000
178700****          GROUP SPECIFIC   'GMOB'   TABULAR                   17870000
178800******************************************************************17880000
178900 3700-PROCESS-GMOB-TABULAR.                                       17890000
179000                                                                  17900000
179100***************************************************************   17910000
179200***** IF SLOT IS LESS THAN 9,000,000                      *****   17920000
179300***** BYPASS THIS RECORD.                                 *****   17930000
179400***************************************************************   17940000
179500                                                                  17950000
179600     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                17960000
179700         GO TO 3700-EXIT.                                         17970000
179800                                                                  17980000
179900***************************************************************   17990000
180000***** CALL TABULAR HASHING PROGRAM                        *****   18000000
180100***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   18010000
180200***************************************************************   18020000
180300                                                                  18030000
180400     MOVE LOW-VALUES TO ASUR-REC-AREA.                            18040000
180500*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              18050000
180600     MOVE GSG3-RECORD              TO ASUR-REC-AREA.              18060000
180700     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          18070000
180800     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 18080000
180900                             ACCUM-SLOT-AREA.                     18090000
181000                                                                  18100000
181100***************************************************************   18110000
181200***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   18120000
181300***************************************************************   18130000
181400                                                                  18140000
181500     IF HASH-RETURN-CODE = '00'                                   18150000
181600        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  18160000
181700     ELSE                                                         18170000
181800        IF HASH-RETURN-CODE = '01'                                18180000
181900           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  18190000
182000        ELSE                                                      18200000
182100           GO TO 3700-EXIT.                                       18210000
182200                                                                  18220000
182300     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            18230000
182400     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   18240000
182500                                                                  18250000
182600***************************************************************   18260000
182700***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   18270000
182800***** THE SEQUENTIAL FILE.                                *****   18280000
182900***************************************************************   18290000
183000                                                                  18300000
183100     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         18310000
183200     MOVE INPUT-WRK-TAB-REC-CNT     TO GSG-ENTRY-COUNT.           18320000
183300     MOVE INPUT-GMOB-RECORD         TO OUTPUT-WRK-RECORD.         18330000
183400     MOVE ASUR-SLOT                 TO GSG-PROVISION-SLOT-NO.     18340000
183500     WRITE OUTPUT-GMOB-RECORD.                                    18350000
183600                                                                  18360000
183700 3700-EXIT.                                                       18370000
183800     EXIT.                                                        18380000
183900/                                                                 18390000
184000******************************************************************18400000
184100****          GROUP SPECIFIC   'GMOR'   TABULAR                   18410000
184200******************************************************************18420000
184300 3800-PROCESS-GMOR-TABULAR.                                       18430000
184400                                                                  18440000
184500***************************************************************   18450000
184600***** IF SLOT IS LESS THAN 9,000,000                      *****   18460000
184700***** BYPASS THIS RECORD.                                 *****   18470000
184800***************************************************************   18480000
184900                                                                  18490000
185000     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                18500000
185100         GO TO 3800-EXIT.                                         18510000
185200                                                                  18520000
185300***************************************************************   18530000
185400***** CALL TABULAR HASHING PROGRAM                        *****   18540000
185500***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   18550000
185600***************************************************************   18560000
185700                                                                  18570000
185800     MOVE LOW-VALUES TO ASUR-REC-AREA.                            18580000
185900*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              18590000
186000     MOVE GSN3-RECORD              TO ASUR-REC-AREA.              18600000
186100     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          18610000
186200     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 18620000
186300                             ACCUM-SLOT-AREA.                     18630000
186400                                                                  18640000
186500***************************************************************   18650000
186600***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   18660000
186700***************************************************************   18670000
186800                                                                  18680000
186900     IF HASH-RETURN-CODE = '00'                                   18690000
187000        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  18700000
187100     ELSE                                                         18710000
187200        IF HASH-RETURN-CODE = '01'                                18720000
187300           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  18730000
187400        ELSE                                                      18740000
187500           GO TO 3800-EXIT.                                       18750000
187600                                                                  18760000
187700     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            18770000
187800     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   18780000
187900                                                                  18790000
188000***************************************************************   18800000
188100***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   18810000
188200***** THE SEQUENTIAL FILE.                                *****   18820000
188300***************************************************************   18830000
188400                                                                  18840000
188500     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         18850000
188600     MOVE INPUT-WRK-TAB-REC-CNT     TO GSN-ENTRY-COUNT.           18860000
188700     MOVE INPUT-GMOR-RECORD         TO OUTPUT-WRK-RECORD.         18870000
188800     MOVE ASUR-SLOT                 TO GSN-PROVISION-SLOT-NO.     18880000
188900     WRITE OUTPUT-GMOR-RECORD.                                    18890000
189000                                                                  18900000
189100 3800-EXIT.                                                       18910000
189200     EXIT.                                                        18920000
189300/                                                                 18930000
189400******************************************************************18940000
189500****         GROUP SPECIFIC   'GMPB'   TABULAR                    18950000
189600******************************************************************18960000
189700 3900-PROCESS-GMPB-TABULAR.                                       18970000
189800                                                                  18980000
189900***************************************************************   18990000
190000***** IF SLOT IS LESS THAN 9,000,000                      *****   19000000
190100***** BYPASS THIS RECORD.                                 *****   19010000
190200***************************************************************   19020000
190300                                                                  19030000
190400     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                19040000
190500         GO TO 3900-EXIT.                                         19050000
190600                                                                  19060000
190700***************************************************************   19070000
190800***** CALL TABULAR HASHING PROGRAM                        *****   19080000
190900***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   19090000
191000***************************************************************   19100000
191100                                                                  19110000
191200     MOVE LOW-VALUES TO ASUR-REC-AREA.                            19120000
191300*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              19130000
191400     MOVE GSH3-RECORD              TO ASUR-REC-AREA.              19140000
191500     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          19150000
191600     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 19160000
191700                             ACCUM-SLOT-AREA.                     19170000
191800                                                                  19180000
191900***************************************************************   19190000
192000***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   19200000
192100***************************************************************   19210000
192200                                                                  19220000
192300     IF HASH-RETURN-CODE = '00'                                   19230000
192400        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  19240000
192500     ELSE                                                         19250000
192600        IF HASH-RETURN-CODE = '01'                                19260000
192700           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  19270000
192800        ELSE                                                      19280000
192900           GO TO 3900-EXIT.                                       19290000
193000                                                                  19300000
193100     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            19310000
193200     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   19320000
193300                                                                  19330000
193400***************************************************************   19340000
193500***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   19350000
193600***** THE SEQUENTIAL FILE.                                *****   19360000
193700***************************************************************   19370000
193800                                                                  19380000
193900     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         19390000
194000     MOVE INPUT-WRK-TAB-REC-CNT     TO GSH-ENTRY-COUNT.           19400000
194100     MOVE INPUT-GMPB-RECORD         TO OUTPUT-WRK-RECORD.         19410000
194200     MOVE ASUR-SLOT                 TO GSH-PROVISION-SLOT-NO.     19420000
194300     WRITE OUTPUT-GMPB-RECORD.                                    19430000
194400                                                                  19440000
194500 3900-EXIT.                                                       19450000
194600     EXIT.                                                        19460000
194700/                                                                 19470000
194800******************************************************************19480000
194900****         GROUP SPECIFIC   'GMPR'   TABULAR                    19490000
195000******************************************************************19500000
195100 4000-PROCESS-GMPR-TABULAR.                                       19510000
195200                                                                  19520000
195300***************************************************************   19530000
195400***** IF SLOT IS LESS THAN 9,000,000                      *****   19540000
195500***** BYPASS THIS RECORD.                                 *****   19550000
195600***************************************************************   19560000
195700                                                                  19570000
195800     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                19580000
195900         GO TO 4000-EXIT.                                         19590000
196000                                                                  19600000
196100***************************************************************   19610000
196200***** CALL TABULAR HASHING PROGRAM                        *****   19620000
196300***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   19630000
196400***************************************************************   19640000
196500                                                                  19650000
196600     MOVE LOW-VALUES TO ASUR-REC-AREA.                            19660000
196700*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              19670000
196800     MOVE GSR3-RECORD              TO ASUR-REC-AREA.              19680000
196900     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          19690000
197000     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 19700000
197100                             ACCUM-SLOT-AREA.                     19710000
197200                                                                  19720000
197300***************************************************************   19730000
197400***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   19740000
197500***************************************************************   19750000
197600                                                                  19760000
197700     IF HASH-RETURN-CODE = '00'                                   19770000
197800        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  19780000
197900     ELSE                                                         19790000
198000        IF HASH-RETURN-CODE = '01'                                19800000
198100           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  19810000
198200        ELSE                                                      19820000
198300           GO TO 4000-EXIT.                                       19830000
198400                                                                  19840000
198500     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            19850000
198600     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   19860000
198700                                                                  19870000
198800***************************************************************   19880000
198900***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   19890000
199000***** THE SEQUENTIAL FILE.                                *****   19900000
199100***************************************************************   19910000
199200                                                                  19920000
199300     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         19930000
199400     MOVE INPUT-WRK-TAB-REC-CNT     TO GSR-ENTRY-COUNT.           19940000
199500     MOVE INPUT-GMPR-RECORD         TO OUTPUT-WRK-RECORD.         19950000
199600     MOVE ASUR-SLOT                 TO GSR-PROVISION-SLOT-NO.     19960000
199700     WRITE OUTPUT-GMPR-RECORD.                                    19970000
199800                                                                  19980000
199900 4000-EXIT.                                                       19990000
200000     EXIT.                                                        20000000
200100/                                                                 20010000
200200******************************************************************20020000
200300****           GROUP SPECIFIC   'GMSB'   TABULAR                  20030000
200400******************************************************************20040000
200500 4100-PROCESS-GMSB-TABULAR.                                       20050000
200600                                                                  20060000
200700***************************************************************   20070000
200800***** IF SLOT IS LESS THAN 9,000,000                      *****   20080000
200900***** BYPASS THIS RECORD.                                 *****   20090000
201000***************************************************************   20100000
201100                                                                  20110000
201200     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                20120000
201300         GO TO 4100-EXIT.                                         20130000
201400                                                                  20140000
201500***************************************************************   20150000
201600***** CALL TABULAR HASHING PROGRAM                        *****   20160000
201700***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   20170000
201800***************************************************************   20180000
201900                                                                  20190000
202000     MOVE LOW-VALUES TO ASUR-REC-AREA.                            20200000
202100*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              20210000
202200     MOVE GSB3-RECORD              TO ASUR-REC-AREA.              20220000
202300     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          20230000
202400     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 20240000
202500                             ACCUM-SLOT-AREA.                     20250000
202600                                                                  20260000
202700***************************************************************   20270000
202800***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   20280000
202900***************************************************************   20290000
203000                                                                  20300000
203100     IF HASH-RETURN-CODE = '00'                                   20310000
203200        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  20320000
203300     ELSE                                                         20330000
203400        IF HASH-RETURN-CODE = '01'                                20340000
203500           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  20350000
203600        ELSE                                                      20360000
203700           GO TO 4100-EXIT.                                       20370000
203800                                                                  20380000
203900     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            20390000
204000     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   20400000
204100                                                                  20410000
204200***************************************************************   20420000
204300***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   20430000
204400***** THE SEQUENTIAL FILE.                                *****   20440000
204500***************************************************************   20450000
204600                                                                  20460000
204700     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         20470000
204800     MOVE INPUT-WRK-TAB-REC-CNT     TO GSB-ENTRY-COUNT.           20480000
204900     MOVE INPUT-GMSB-RECORD         TO OUTPUT-WRK-RECORD.         20490000
205000     MOVE ASUR-SLOT                 TO GSB-PROVISION-SLOT-NO.     20500000
205100     WRITE OUTPUT-GMSB-RECORD.                                    20510000
205200                                                                  20520000
205300 4100-EXIT.                                                       20530000
205400     EXIT.                                                        20540000
205500/                                                                 20550000
205600******************************************************************20560000
205700****           GROUP SPECIFIC   'GMSC'   TABULAR                  20570000
205800******************************************************************20580000
205900 4200-PROCESS-GMSC-TABULAR.                                       20590000
206000                                                                  20600000
206100***************************************************************   20610000
206200***** IF SLOT IS LESS THAN 9,000,000                      *****   20620000
206300***** BYPASS THIS RECORD.                                 *****   20630000
206400***************************************************************   20640000
206500                                                                  20650000
206600     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                20660000
206700         GO TO 4200-EXIT.                                         20670000
206800                                                                  20680000
206900**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           20690000
207000**                                                                20700000
207100     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          20710000
207200     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        20720000
207300         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        20730000
207400         MOVE INPUT-WRK-TAB-REC-CNT  TO  GSL-ENTRY-COUNT          20740000
207500         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL20750000
207600         MOVE INPUT-GMSC-RECORD      TO  OUTPUT-WRK-RECORD        20760000
207700         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GSL-PROVISION-SLOT-NO  20770000
207800         WRITE  OUTPUT-GMSC-RECORD                                20780000
207900         GO TO 4200-EXIT.                                         20790000
208000                                                                  20800000
208100***************************************************************   20810000
208200***** CALL TABULAR HASHING PROGRAM                        *****   20820000
208300***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   20830000
208400***************************************************************   20840000
208500                                                                  20850000
208600     MOVE LOW-VALUES TO ASUR-REC-AREA.                            20860000
208700*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              20870000
208800     MOVE GSL3-RECORD              TO ASUR-REC-AREA.              20880000
208900     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          20890000
209000     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 20900000
209100                             ACCUM-SLOT-AREA.                     20910000
209200                                                                  20920000
209300***************************************************************   20930000
209400***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   20940000
209500***************************************************************   20950000
209600                                                                  20960000
209700     IF HASH-RETURN-CODE = '00'                                   20970000
209800        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  20980000
209900     ELSE                                                         20990000
210000        IF HASH-RETURN-CODE = '01'                                21000000
210100           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  21010000
210200        ELSE                                                      21020000
210300           GO TO 4200-EXIT.                                       21030000
210400                                                                  21040000
210500     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            21050000
210600     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   21060000
210700                                                                  21070000
210800***************************************************************   21080000
210900***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   21090000
211000***** THE SEQUENTIAL FILE.                                *****   21100000
211100***************************************************************   21110000
211200                                                                  21120000
211300     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         21130000
211400     MOVE INPUT-WRK-TAB-REC-CNT     TO GSL-ENTRY-COUNT.           21140000
211500     MOVE INPUT-GMSC-RECORD         TO OUTPUT-WRK-RECORD.         21150000
211600     MOVE ASUR-SLOT                 TO GSL-PROVISION-SLOT-NO.     21160000
211700     WRITE OUTPUT-GMSC-RECORD.                                    21170000
211800                                                                  21180000
211900 4200-EXIT.                                                       21190000
212000     EXIT.                                                        21200000
212100/                                                                 21210000
212200******************************************************************21220000
212300****           GROUP SPECIFIC   'GMSR'   TABULAR                  21230000
212400******************************************************************21240000
212500 4300-PROCESS-GMSR-TABULAR.                                       21250000
212600                                                                  21260000
212700***************************************************************   21270000
212800***** IF SLOT IS LESS THAN 9,000,000                      *****   21280000
212900***** BYPASS THIS RECORD.                                 *****   21290000
213000***************************************************************   21300000
213100                                                                  21310000
213200     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                21320000
213300         GO TO 4300-EXIT.                                         21330000
213400                                                                  21340000
213500***************************************************************   21350000
213600***** CALL TABULAR HASHING PROGRAM                        *****   21360000
213700***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   21370000
213800***************************************************************   21380000
213900                                                                  21390000
214000     MOVE LOW-VALUES TO ASUR-REC-AREA.                            21400000
214100*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              21410000
214200     MOVE GSM3-RECORD              TO ASUR-REC-AREA.              21420000
214300     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          21430000
214400     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 21440000
214500                             ACCUM-SLOT-AREA.                     21450000
214600                                                                  21460000
214700***************************************************************   21470000
214800***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   21480000
214900***************************************************************   21490000
215000                                                                  21500000
215100     IF HASH-RETURN-CODE = '00'                                   21510000
215200        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  21520000
215300     ELSE                                                         21530000
215400        IF HASH-RETURN-CODE = '01'                                21540000
215500           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  21550000
215600        ELSE                                                      21560000
215700           GO TO 4300-EXIT.                                       21570000
215800                                                                  21580000
215900     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            21590000
216000     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   21600000
216100                                                                  21610000
216200***************************************************************   21620000
216300***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   21630000
216400***** THE SEQUENTIAL FILE.                                *****   21640000
216500***************************************************************   21650000
216600                                                                  21660000
216700     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         21670000
216800     MOVE INPUT-WRK-TAB-REC-CNT     TO GSM-ENTRY-COUNT.           21680000
216900     MOVE INPUT-GMSR-RECORD         TO OUTPUT-WRK-RECORD.         21690000
217000     MOVE ASUR-SLOT                 TO GSM-PROVISION-SLOT-NO.     21700000
217100     WRITE OUTPUT-GMSR-RECORD.                                    21710000
217200                                                                  21720000
217300 4300-EXIT.                                                       21730000
217400     EXIT.                                                        21740000
217500/                                                                 21750000
217600******************************************************************21760000
217700****           GROUP SPECIFIC   'GPAB'   TABULAR                  21770000
217800******************************************************************21780000
217900 4400-PROCESS-GPAB-TABULAR.                                       21790000
218000                                                                  21800000
218100***************************************************************   21810000
218200***** IF SLOT IS LESS THAN 9,000,000                      *****   21820000
218300***** BYPASS THIS RECORD.                                 *****   21830000
218400***************************************************************   21840000
218500                                                                  21850000
218600     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                21860000
218700         GO TO 4400-EXIT.                                         21870000
218800                                                                  21880000
218900***************************************************************   21890000
219000***** CALL TABULAR HASHING PROGRAM                        *****   21900000
219100***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   21910000
219200***************************************************************   21920000
219300                                                                  21930000
219400     MOVE LOW-VALUES TO ASUR-REC-AREA.                            21940000
219500*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              21950000
219600     MOVE GST3-RECORD              TO ASUR-REC-AREA.              21960000
219700     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          21970000
219800     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 21980000
219900                             ACCUM-SLOT-AREA.                     21990000
220000                                                                  22000000
220100***************************************************************   22010000
220200***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   22020000
220300***************************************************************   22030000
220400                                                                  22040000
220500     IF HASH-RETURN-CODE = '00'                                   22050000
220600        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  22060000
220700     ELSE                                                         22070000
220800        IF HASH-RETURN-CODE = '01'                                22080000
220900           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  22090000
221000        ELSE                                                      22100000
221100           GO TO 4400-EXIT.                                       22110000
221200                                                                  22120000
221300     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            22130000
221400     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   22140000
221500                                                                  22150000
221600***************************************************************   22160000
221700***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   22170000
221800***** THE SEQUENTIAL FILE.                                *****   22180000
221900***************************************************************   22190000
222000                                                                  22200000
222100     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         22210000
222200     MOVE INPUT-WRK-TAB-REC-CNT     TO GST-ENTRY-COUNT.           22220000
222300     MOVE INPUT-GPAB-RECORD         TO OUTPUT-WRK-RECORD.         22230000
222400     MOVE ASUR-SLOT                 TO GST-PROVISION-SLOT-NO.     22240000
222500     WRITE OUTPUT-GPAB-RECORD.                                    22250000
222600                                                                  22260000
222700 4400-EXIT.                                                       22270000
222800     EXIT.                                                        22280000
222900/                                                                 22290000
223000******************************************************************22300000
223100****           GROUP SPECIFIC   'GPAC'   TABULAR                  22310000
223200******************************************************************22320000
223300 4500-PROCESS-GPAC-TABULAR.                                       22330000
223400                                                                  22340000
223500***************************************************************   22350000
223600***** IF SLOT IS LESS THAN 9,000,000                      *****   22360000
223700***** BYPASS THIS RECORD.                                 *****   22370000
223800***************************************************************   22380000
223900                                                                  22390000
224000     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                22400000
224100         GO TO 4500-EXIT.                                         22410000
224200                                                                  22420000
224300**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           22430000
224400**                                                                22440000
224500     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          22450000
224600     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        22460000
224700         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        22470000
224800         MOVE INPUT-WRK-TAB-REC-CNT  TO  GSJ-ENTRY-COUNT          22480000
224900         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL22490000
225000         MOVE INPUT-GPAC-RECORD      TO  OUTPUT-WRK-RECORD        22500000
225100         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GSJ-PROVISION-SLOT-NO  22510000
225200         WRITE  OUTPUT-GPAC-RECORD                                22520000
225300         GO TO 4500-EXIT.                                         22530000
225400                                                                  22540000
225500***************************************************************   22550000
225600***** CALL TABULAR HASHING PROGRAM                        *****   22560000
225700***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   22570000
225800***************************************************************   22580000
225900                                                                  22590000
226000     MOVE LOW-VALUES TO ASUR-REC-AREA.                            22600000
226100*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              22610000
226200     MOVE GSJ3-RECORD              TO ASUR-REC-AREA.              22620000
226300     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          22630000
226400     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 22640000
226500                             ACCUM-SLOT-AREA.                     22650000
226600                                                                  22660000
226700***************************************************************   22670000
226800***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   22680000
226900***************************************************************   22690000
227000                                                                  22700000
227100     IF HASH-RETURN-CODE = '00'                                   22710000
227200        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  22720000
227300     ELSE                                                         22730000
227400        IF HASH-RETURN-CODE = '01'                                22740000
227500           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  22750000
227600        ELSE                                                      22760000
227700           GO TO 4500-EXIT.                                       22770000
227800                                                                  22780000
227900     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            22790000
228000     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   22800000
228100                                                                  22810000
228200***************************************************************   22820000
228300***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   22830000
228400***** THE SEQUENTIAL FILE.                                *****   22840000
228500***************************************************************   22850000
228600                                                                  22860000
228700     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         22870000
228800     MOVE INPUT-WRK-TAB-REC-CNT     TO GSJ-ENTRY-COUNT.           22880000
228900     MOVE INPUT-GPAC-RECORD         TO OUTPUT-WRK-RECORD.         22890000
229000     MOVE ASUR-SLOT                 TO GSJ-PROVISION-SLOT-NO.     22900000
229100     WRITE OUTPUT-GPAC-RECORD.                                    22910000
229200                                                                  22920000
229300 4500-EXIT.                                                       22930000
229400     EXIT.                                                        22940000
229500/                                                                 22950000
229600******************************************************************22960000
229700****           GROUP SPECIFIC   'GPAD'   TABULAR                  22970000
229800******************************************************************22980000
229900 4600-PROCESS-GPAD-TABULAR.                                       22990000
230000                                                                  23000000
230100***************************************************************   23010000
230200***** IF SLOT IS LESS THAN 9,000,000                      *****   23020000
230300***** BYPASS THIS RECORD.                                 *****   23030000
230400***************************************************************   23040000
230500                                                                  23050000
230600     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                23060000
230700         GO TO 4600-EXIT.                                         23070000
230800                                                                  23080000
230900***************************************************************   23090000
231000***** CALL TABULAR HASHING PROGRAM                        *****   23100000
231100***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   23110000
231200***************************************************************   23120000
231300                                                                  23130000
231400     MOVE LOW-VALUES TO ASUR-REC-AREA.                            23140000
231500*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              23150000
231600     MOVE GSC3-RECORD              TO ASUR-REC-AREA.              23160000
231700     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          23170000
231800     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 23180000
231900                             ACCUM-SLOT-AREA.                     23190000
232000                                                                  23200000
232100***************************************************************   23210000
232200***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   23220000
232300***************************************************************   23230000
232400                                                                  23240000
232500     IF HASH-RETURN-CODE = '00'                                   23250000
232600        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  23260000
232700     ELSE                                                         23270000
232800        IF HASH-RETURN-CODE = '01'                                23280000
232900           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  23290000
233000        ELSE                                                      23300000
233100           GO TO 4600-EXIT.                                       23310000
233200                                                                  23320000
233300     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            23330000
233400     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   23340000
233500                                                                  23350000
233600***************************************************************   23360000
233700***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   23370000
233800***** THE SEQUENTIAL FILE.                                *****   23380000
233900***************************************************************   23390000
234000                                                                  23400000
234100     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         23410000
234200     MOVE INPUT-WRK-TAB-REC-CNT     TO GSC-ENTRY-COUNT.           23420000
234300     MOVE INPUT-GPAD-RECORD         TO OUTPUT-WRK-RECORD.         23430000
234400     MOVE ASUR-SLOT                 TO GSC-PROVISION-SLOT-NO.     23440000
234500     WRITE OUTPUT-GPAD-RECORD.                                    23450000
234600                                                                  23460000
234700 4600-EXIT.                                                       23470000
234800     EXIT.                                                        23480000
234900/                                                                 23490000
235000******************************************************************23500000
235100****           GROUP SPECIFIC   'GPAN'   TABULAR                  23510000
235200******************************************************************23520000
235300 4655-PROCESS-GPAN-TABULAR.                                       23530000
235400                                                                  23540000
235500***************************************************************   23550000
235600***** IF SLOT IS LESS THAN 9,000,000                      *****   23560000
235700***** BYPASS THIS RECORD.                                 *****   23570000
235800***************************************************************   23580000
235900                                                                  23590000
236000     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                23600000
236100         GO TO 4655-EXIT.                                         23610000
236200                                                                  23620000
236300**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           23630000
236400**                                                                23640000
236500     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          23650000
236600     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        23660000
236700         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        23670000
236800         MOVE INPUT-WRK-TAB-REC-CNT  TO  GS10-ENTRY-COUNT         23680000
236900         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL23690000
237000         MOVE INPUT-GPAN-RECORD      TO  OUTPUT-WRK-RECORD        23700000
237100         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GS10-PROVISION-SLOT-NO 23710000
237200         WRITE  OUTPUT-GPAN-RECORD                                23720000
237300         GO TO 4655-EXIT.                                         23730000
237400                                                                  23740000
237500***************************************************************   23750000
237600***** CALL TABULAR HASHING PROGRAM                        *****   23760000
237700***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   23770000
237800***************************************************************   23780000
237900                                                                  23790000
238000     MOVE LOW-VALUES TO ASUR-REC-AREA.                            23800000
238100*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              23810000
238200     MOVE GS103-RECORD             TO ASUR-REC-AREA.              23820000
238300     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          23830000
238400     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 23840000
238500                             ACCUM-SLOT-AREA.                     23850000
238600                                                                  23860000
238700***************************************************************   23870000
238800***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   23880000
238900***************************************************************   23890000
239000                                                                  23900000
239100     IF HASH-RETURN-CODE = '00'                                   23910000
239200        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  23920000
239300     ELSE                                                         23930000
239400        IF HASH-RETURN-CODE = '01'                                23940000
239500           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  23950000
239600        ELSE                                                      23960000
239700           GO TO 4655-EXIT.                                       23970000
239800                                                                  23980000
239900     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            23990000
240000     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   24000000
240100                                                                  24010000
240200***************************************************************   24020000
240300***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   24030000
240400***** THE SEQUENTIAL FILE.                                *****   24040000
240500***************************************************************   24050000
240600                                                                  24060000
240700     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         24070000
240800     MOVE INPUT-WRK-TAB-REC-CNT     TO GS10-ENTRY-COUNT.          24080000
240900     MOVE INPUT-GPAN-RECORD         TO OUTPUT-WRK-RECORD.         24090000
241000     MOVE ASUR-SLOT                 TO GS10-PROVISION-SLOT-NO.    24100000
241100     WRITE OUTPUT-GPAN-RECORD.                                    24110000
241200                                                                  24120000
241300 4655-EXIT.                                                       24130000
241400     EXIT.                                                        24140000
241500/                                                                 24150000
241600******************************************************************24160000
241700****           GROUP SPECIFIC   'GPAR'   TABULAR                  24170000
241800******************************************************************24180000
241900 4700-PROCESS-GPAR-TABULAR.                                       24190000
242000                                                                  24200000
242100***************************************************************   24210000
242200***** IF SLOT IS LESS THAN 9,000,000                      *****   24220000
242300***** BYPASS THIS RECORD.                                 *****   24230000
242400***************************************************************   24240000
242500                                                                  24250000
242600     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                24260000
242700         GO TO 4700-EXIT.                                         24270000
242800                                                                  24280000
242900***************************************************************   24290000
243000***** CALL TABULAR HASHING PROGRAM                        *****   24300000
243100***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   24310000
243200***************************************************************   24320000
243300                                                                  24330000
243400     MOVE LOW-VALUES TO ASUR-REC-AREA.                            24340000
243500*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              24350000
243600     MOVE GSK3-RECORD              TO ASUR-REC-AREA.              24360000
243700     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          24370000
243800     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 24380000
243900                             ACCUM-SLOT-AREA.                     24390000
244000                                                                  24400000
244100***************************************************************   24410000
244200***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   24420000
244300***************************************************************   24430000
244400                                                                  24440000
244500     IF HASH-RETURN-CODE = '00'                                   24450000
244600        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  24460000
244700     ELSE                                                         24470000
244800        IF HASH-RETURN-CODE = '01'                                24480000
244900           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  24490000
245000        ELSE                                                      24500000
245100           GO TO 4700-EXIT.                                       24510000
245200                                                                  24520000
245300     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            24530000
245400     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   24540000
245500                                                                  24550000
245600***************************************************************   24560000
245700***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   24570000
245800***** THE SEQUENTIAL FILE.                                *****   24580000
245900***************************************************************   24590000
246000                                                                  24600000
246100     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         24610000
246200     MOVE INPUT-WRK-TAB-REC-CNT     TO GSK-ENTRY-COUNT.           24620000
246300     MOVE INPUT-GPAR-RECORD         TO OUTPUT-WRK-RECORD.         24630000
246400     MOVE ASUR-SLOT                 TO GSK-PROVISION-SLOT-NO.     24640000
246500     WRITE OUTPUT-GPAR-RECORD.                                    24650000
246600                                                                  24660000
246700 4700-EXIT.                                                       24670000
246800     EXIT.                                                        24680000
246900/                                                                 24690000
247000******************************************************************24700000
247100****           GROUP SPECIFIC   'GPPO'   TABULAR                  24710000
247200******************************************************************24720000
247300 4800-PROCESS-GPPO-TABULAR.                                       24730000
247400                                                                  24740000
247500***************************************************************   24750000
247600***** IF SLOT IS LESS THAN 9,000,000                      *****   24760000
247700***** BYPASS THIS RECORD.                                 *****   24770000
247800***************************************************************   24780000
247900                                                                  24790000
248000     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                24800000
248100         GO TO 4800-EXIT.                                         24810000
248200                                                                  24820000
248300**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           24830000
248400**                                                                24840000
248500     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          24850000
248600     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        24860000
248700         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        24870000
248800         MOVE INPUT-WRK-TAB-REC-CNT  TO  GSW-ENTRY-COUNT          24880000
248900         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL24890000
249000         MOVE INPUT-GPPO-RECORD      TO  OUTPUT-WRK-RECORD        24900000
249100         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GSW-PROVISION-SLOT-NO  24910000
249200         WRITE  OUTPUT-GPPO-RECORD                                24920000
249300         GO TO 4800-EXIT.                                         24930000
249400                                                                  24940000
249500***************************************************************   24950000
249600***** CALL TABULAR HASHING PROGRAM                        *****   24960000
249700***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   24970000
249800***************************************************************   24980000
249900                                                                  24990000
250000     MOVE LOW-VALUES TO ASUR-REC-AREA.                            25000000
250100*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              25010000
250200     MOVE GSW3-RECORD              TO ASUR-REC-AREA.              25020000
250300     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          25030000
250400     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 25040000
250500                             ACCUM-SLOT-AREA.                     25050000
250600                                                                  25060000
250700***************************************************************   25070000
250800***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   25080000
250900***************************************************************   25090000
251000                                                                  25100000
251100     IF HASH-RETURN-CODE = '00'                                   25110000
251200        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  25120000
251300     ELSE                                                         25130000
251400        IF HASH-RETURN-CODE = '01'                                25140000
251500           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  25150000
251600        ELSE                                                      25160000
251700           GO TO 4800-EXIT.                                       25170000
251800                                                                  25180000
251900     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            25190000
252000     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   25200000
252100                                                                  25210000
252200***************************************************************   25220000
252300***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   25230000
252400***** THE SEQUENTIAL FILE.                                *****   25240000
252500***************************************************************   25250000
252600                                                                  25260000
252700     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         25270000
252800     MOVE INPUT-WRK-TAB-REC-CNT     TO GSW-ENTRY-COUNT.           25280000
252900     MOVE INPUT-GPPO-RECORD         TO OUTPUT-WRK-RECORD.         25290000
253000     MOVE ASUR-SLOT                 TO GSW-PROVISION-SLOT-NO.     25300000
253100     WRITE OUTPUT-GPPO-RECORD.                                    25310000
253200                                                                  25320000
253300 4800-EXIT.                                                       25330000
253400     EXIT.                                                        25340000
253500/                                                                 25350000
253600******************************************************************25360000
253700****           GROUP SPECIFIC   'GRPO'   TABULAR                  25370000
253800******************************************************************25380000
253900 4810-PROCESS-GRPO-TABULAR.                                       25390000
254000                                                                  25400000
254100***************************************************************   25410000
254200***** IF SLOT IS LESS THAN 9,000,000                      *****   25420000
254300***** BYPASS THIS RECORD.                                 *****   25430000
254400***************************************************************   25440000
254500                                                                  25450000
254600     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                25460000
254700         GO TO 4810-EXIT.                                         25470000
254800                                                                  25480000
254900**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           25490000
255000**                                                                25500000
255100     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          25510000
255200     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        25520000
255300         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        25530000
255400         MOVE INPUT-WRK-TAB-REC-CNT  TO  GS7-ENTRY-COUNT          25540000
255500         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL25550000
255600         MOVE INPUT-GRPO-RECORD      TO  OUTPUT-WRK-RECORD        25560000
255700         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GS7-PROVISION-SLOT-NO  25570000
255800         WRITE  OUTPUT-GRPO-RECORD                                25580000
255900         GO TO 4810-EXIT.                                         25590000
256000                                                                  25600000
256100***************************************************************   25610000
256200***** CALL TABULAR HASHING PROGRAM                        *****   25620000
256300***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   25630000
256400***************************************************************   25640000
256500                                                                  25650000
256600     MOVE LOW-VALUES TO ASUR-REC-AREA.                            25660000
256700*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              25670000
256800     MOVE GS73-RECORD              TO ASUR-REC-AREA.              25680000
256900     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          25690000
257000     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 25700000
257100                             ACCUM-SLOT-AREA.                     25710000
257200                                                                  25720000
257300***************************************************************   25730000
257400***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   25740000
257500***************************************************************   25750000
257600                                                                  25760000
257700     IF HASH-RETURN-CODE = '00'                                   25770000
257800        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  25780000
257900     ELSE                                                         25790000
258000        IF HASH-RETURN-CODE = '01'                                25800000
258100           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  25810000
258200        ELSE                                                      25820000
258300           GO TO 4810-EXIT.                                       25830000
258400                                                                  25840000
258500     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            25850000
258600     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   25860000
258700                                                                  25870000
258800***************************************************************   25880000
258900***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   25890000
259000***** THE SEQUENTIAL FILE.                                *****   25900000
259100***************************************************************   25910000
259200                                                                  25920000
259300     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         25930000
259400     MOVE INPUT-WRK-TAB-REC-CNT     TO GS7-ENTRY-COUNT.           25940000
259500     MOVE INPUT-GRPO-RECORD         TO OUTPUT-WRK-RECORD.         25950000
259600     MOVE ASUR-SLOT                 TO GS7-PROVISION-SLOT-NO.     25960000
259700     WRITE OUTPUT-GRPO-RECORD.                                    25970000
259800                                                                  25980000
259900 4810-EXIT.                                                       25990000
260000     EXIT.                                                        26000000
260100/                                                                 26010000
260200******************************************************************26020000
260300****           GROUP SPECIFIC   'GBAE'   TABULAR                  26030000
260400******************************************************************26040000
260500 4820-PROCESS-GBAE-TABULAR.                                       26050000
260600                                                                  26060000
260700***************************************************************   26070000
260800***** IF SLOT IS LESS THAN 9,000,000                      *****   26080000
260900***** BYPASS THIS RECORD.                                 *****   26090000
261000***************************************************************   26100000
261100                                                                  26110000
261200     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                26120000
261300         GO TO 4810-EXIT.                                         26130000
261400                                                                  26140000
261500**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           26150000
261600**                                                                26160000
261700     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          26170000
261800     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        26180000
261900         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        26190000
262000         MOVE INPUT-WRK-TAB-REC-CNT  TO  GS16-ENTRY-COUNT         26200000
262100         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL26210000
262200         MOVE INPUT-GBAE-RECORD      TO  OUTPUT-WRK-RECORD        26220000
262300         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GS16-PROVISION-SLOT-NO 26230000
262400         WRITE  OUTPUT-GBAE-RECORD                                26240000
262500         GO TO 4820-EXIT.                                         26250000
262600                                                                  26260000
262700***************************************************************   26270000
262800***** CALL TABULAR HASHING PROGRAM                        *****   26280000
262900***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   26290000
263000***************************************************************   26300000
263100                                                                  26310000
263200     MOVE LOW-VALUES TO ASUR-REC-AREA.                            26320000
263300*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              26330000
263400     MOVE GS163-RECORD             TO ASUR-REC-AREA.              26340000
263500     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          26350000
263600     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 26360000
263700                             ACCUM-SLOT-AREA.                     26370000
263800                                                                  26380000
263900***************************************************************   26390000
264000***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   26400000
264100***************************************************************   26410000
264200                                                                  26420000
264300     IF HASH-RETURN-CODE = '00'                                   26430000
264400        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  26440000
264500     ELSE                                                         26450000
264600        IF HASH-RETURN-CODE = '01'                                26460000
264700           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  26470000
264800        ELSE                                                      26480000
264900           GO TO 4820-EXIT.                                       26490000
265000                                                                  26500000
265100     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            26510000
265200     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   26520000
265300                                                                  26530000
265400***************************************************************   26540000
265500***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   26550000
265600***** THE SEQUENTIAL FILE.                                *****   26560000
265700***************************************************************   26570000
265800                                                                  26580000
265900     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         26590000
266000     MOVE INPUT-WRK-TAB-REC-CNT     TO GS16-ENTRY-COUNT.          26600000
266100     MOVE INPUT-GBAE-RECORD         TO OUTPUT-WRK-RECORD.         26610000
266200     MOVE ASUR-SLOT                 TO GS16-PROVISION-SLOT-NO.    26620000
266300     WRITE OUTPUT-GBAE-RECORD.                                    26630000
266400                                                                  26640000
266500 4820-EXIT.                                                       26650000
266600     EXIT.                                                        26660000
266700/                                                                 26670000
266800******************************************************************26680000
266900****           GROUP SPECIFIC   'GRID'   TABULAR                  26690000
267000******************************************************************26700000
267100 4900-PROCESS-GRID-TABULAR.                                       26710000
267200                                                                  26720000
267300***************************************************************   26730000
267400***** IF SLOT IS LESS THAN 9,000,000                      *****   26740000
267500***** BYPASS THIS RECORD.                                 *****   26750000
267600***************************************************************   26760000
267700                                                                  26770000
267800     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                26780000
267900         GO TO 4900-EXIT.                                         26790000
268000                                                                  26800000
268100***************************************************************   26810000
268200***** CALL TABULAR HASHING PROGRAM                        *****   26820000
268300***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   26830000
268400***************************************************************   26840000
268500                                                                  26850000
268600     MOVE LOW-VALUES TO ASUR-REC-AREA.                            26860000
268700*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              26870000
268800     MOVE GSD3-RECORD              TO ASUR-REC-AREA.              26880000
268900     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          26890000
269000     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 26900000
269100                             ACCUM-SLOT-AREA.                     26910000
269200                                                                  26920000
269300***************************************************************   26930000
269400***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   26940000
269500***************************************************************   26950000
269600                                                                  26960000
269700     IF HASH-RETURN-CODE = '00'                                   26970000
269800        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  26980000
269900     ELSE                                                         26990000
270000        IF HASH-RETURN-CODE = '01'                                27000000
270100           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  27010000
270200        ELSE                                                      27020000
270300           GO TO 4900-EXIT.                                       27030000
270400                                                                  27040000
270500     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            27050000
270600     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   27060000
270700                                                                  27070000
270800***************************************************************   27080000
270900***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   27090000
271000***** THE SEQUENTIAL FILE.                                *****   27100000
271100***************************************************************   27110000
271200                                                                  27120000
271300     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         27130000
271400     MOVE INPUT-WRK-TAB-REC-CNT     TO GSD-ENTRY-COUNT.           27140000
271500     MOVE INPUT-GRID-RECORD         TO OUTPUT-WRK-RECORD.         27150000
271600     MOVE ASUR-SLOT                 TO GSD-PROVISION-SLOT-NO.     27160000
271700     WRITE OUTPUT-GRID-RECORD.                                    27170000
271800                                                                  27180000
271900 4900-EXIT.                                                       27190000
272000     EXIT.                                                        27200000
272100                                                                  27210000
272200/*****************************************************************27220000
272300****           GROUP SPECIFIC   'GVLF'   TABULAR                  27230000
272400******************************************************************27240000
272500 5000-PROCESS-GVLF-TABULAR.                                       27250000
272600                                                                  27260000
272700***************************************************************   27270000
272800***** IF SLOT IS LESS THAN 9,000,000                      *****   27280000
272900***** BYPASS THIS RECORD.                                 *****   27290000
273000***************************************************************   27300000
273100                                                                  27310000
273200     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                27320000
273300         GO TO 5000-EXIT.                                         27330000
273400                                                                  27340000
273500**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           27350000
273600**                                                                27360000
273700     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          27370000
273800     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        27380000
273900         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        27390000
274000         MOVE INPUT-WRK-TAB-REC-CNT  TO  GSX-ENTRY-COUNT          27400000
274100         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL27410000
274200         MOVE INPUT-GVLF-RECORD      TO  OUTPUT-WRK-RECORD        27420000
274300         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GSX-PROVISION-SLOT-NO  27430000
274400         WRITE  OUTPUT-GVLF-RECORD                                27440000
274500         GO TO 5000-EXIT.                                         27450000
274600                                                                  27460000
274700***************************************************************   27470000
274800***** CALL TABULAR HASHING PROGRAM                        *****   27480000
274900***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   27490000
275000***************************************************************   27500000
275100                                                                  27510000
275200     MOVE LOW-VALUES TO ASUR-REC-AREA.                            27520000
275300*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              27530000
275400     MOVE GSX3-RECORD              TO ASUR-REC-AREA.              27540000
275500     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          27550000
275600     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 27560000
275700                             ACCUM-SLOT-AREA.                     27570000
275800                                                                  27580000
275900***************************************************************   27590000
276000***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   27600000
276100***************************************************************   27610000
276200                                                                  27620000
276300     IF HASH-RETURN-CODE = '00'                                   27630000
276400        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  27640000
276500     ELSE                                                         27650000
276600        IF HASH-RETURN-CODE = '01'                                27660000
276700           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  27670000
276800        ELSE                                                      27680000
276900           GO TO 5000-EXIT.                                       27690000
277000                                                                  27700000
277100     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            27710000
277200     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   27720000
277300                                                                  27730000
277400***************************************************************   27740000
277500***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   27750000
277600***** THE SEQUENTIAL FILE.                                *****   27760000
277700***************************************************************   27770000
277800                                                                  27780000
277900     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         27790000
278000     MOVE INPUT-WRK-TAB-REC-CNT     TO GSX-ENTRY-COUNT.           27800000
278100     MOVE INPUT-GVLF-RECORD         TO OUTPUT-WRK-RECORD.         27810000
278200     MOVE ASUR-SLOT                 TO GSX-PROVISION-SLOT-NO.     27820000
278300     WRITE OUTPUT-GVLF-RECORD.                                    27830000
278400                                                                  27840000
278500 5000-EXIT.                                                       27850000
278600     EXIT.                                                        27860000
278700                                                                  27870000
278800/*****************************************************************27880000
278900****           GROUP SPECIFIC   'GVLG'   TABULAR                  27890000
279000******************************************************************27900000
279100 5001-PROCESS-GVLG-TABULAR.                                       27910000
279200                                                                  27920000
279300***************************************************************   27930000
279400***** IF SLOT IS LESS THAN 9,000,000                      *****   27940000
279500***** BYPASS THIS RECORD.                                 *****   27950000
279600***************************************************************   27960000
279700                                                                  27970000
279800     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                27980000
279900         GO TO 5001-EXIT.                                         27990000
280000                                                                  28000000
280100**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           28010000
280200**                                                                28020000
280300     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          28030000
280400     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        28040000
280500         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        28050000
280600         MOVE INPUT-WRK-TAB-REC-CNT  TO  GSF-ENTRY-COUNT          28060000
280700         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL28070000
280800         MOVE INPUT-GVLG-RECORD      TO  OUTPUT-WRK-RECORD        28080000
280900         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GSF-PROVISION-SLOT-NO  28090000
281000         WRITE  OUTPUT-GVLG-RECORD                                28100000
281100         GO TO 5001-EXIT.                                         28110000
281200                                                                  28120000
281300***************************************************************   28130000
281400***** CALL TABULAR HASHING PROGRAM                        *****   28140000
281500***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   28150000
281600***************************************************************   28160000
281700                                                                  28170000
281800     MOVE LOW-VALUES TO ASUR-REC-AREA.                            28180000
281900*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              28190000
282000     MOVE GSF3-RECORD              TO ASUR-REC-AREA.              28200000
282100     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          28210000
282200     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 28220000
282300                             ACCUM-SLOT-AREA.                     28230000
282400                                                                  28240000
282500***************************************************************   28250000
282600***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   28260000
282700***************************************************************   28270000
282800                                                                  28280000
282900     IF HASH-RETURN-CODE = '00'                                   28290000
283000        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  28300000
283100     ELSE                                                         28310000
283200        IF HASH-RETURN-CODE = '01'                                28320000
283300           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  28330000
283400        ELSE                                                      28340000
283500           GO TO 5001-EXIT.                                       28350000
283600                                                                  28360000
283700     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            28370000
283800     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   28380000
283900                                                                  28390000
284000***************************************************************   28400000
284100***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   28410000
284200***** THE SEQUENTIAL FILE.                                *****   28420000
284300***************************************************************   28430000
284400                                                                  28440000
284500     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         28450000
284600     MOVE INPUT-WRK-TAB-REC-CNT     TO GSF-ENTRY-COUNT.           28460000
284700     MOVE INPUT-GVLG-RECORD         TO OUTPUT-WRK-RECORD.         28470000
284800     MOVE ASUR-SLOT                 TO GSF-PROVISION-SLOT-NO.     28480000
284900     WRITE OUTPUT-GVLG-RECORD.                                    28490000
285000                                                                  28500000
285100 5001-EXIT.                                                       28510000
285200     EXIT.                                                        28520000
285300/*****************************************************************28530000
285400****           GROUP SPECIFIC   'GVLH'   TABULAR                  28540000
285500******************************************************************28550000
285600 5002-PROCESS-GVLH-TABULAR.                                       28560000
285700                                                                  28570000
285800***************************************************************   28580000
285900***** IF SLOT IS LESS THAN 9,000,000                      *****   28590000
286000***** BYPASS THIS RECORD.                                 *****   28600000
286100***************************************************************   28610000
286200                                                                  28620000
286300     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                28630000
286400         GO TO 5002-EXIT.                                         28640000
286500                                                                  28650000
286600**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           28660000
286700**                                                                28670000
286800     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          28680000
286900     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        28690000
287000         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        28700000
287100         MOVE INPUT-WRK-TAB-REC-CNT  TO  GSQ-ENTRY-COUNT          28710000
287200         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL28720000
287300         MOVE INPUT-GVLH-RECORD      TO  OUTPUT-WRK-RECORD        28730000
287400         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GSQ-PROVISION-SLOT-NO  28740000
287500         WRITE  OUTPUT-GVLH-RECORD                                28750000
287600         GO TO 5002-EXIT.                                         28760000
287700                                                                  28770000
287800***************************************************************   28780000
287900***** CALL TABULAR HASHING PROGRAM                        *****   28790000
288000***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   28800000
288100***************************************************************   28810000
288200                                                                  28820000
288300     MOVE LOW-VALUES TO ASUR-REC-AREA.                            28830000
288400*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              28840000
288500     MOVE GSQ3-RECORD              TO ASUR-REC-AREA.              28850000
288600     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          28860000
288700     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 28870000
288800                             ACCUM-SLOT-AREA.                     28880000
288900                                                                  28890000
289000***************************************************************   28900000
289100***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   28910000
289200***************************************************************   28920000
289300                                                                  28930000
289400     IF HASH-RETURN-CODE = '00'                                   28940000
289500        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  28950000
289600     ELSE                                                         28960000
289700        IF HASH-RETURN-CODE = '01'                                28970000
289800           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  28980000
289900        ELSE                                                      28990000
290000           GO TO 5002-EXIT.                                       29000000
290100                                                                  29010000
290200     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            29020000
290300     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   29030000
290400                                                                  29040000
290500***************************************************************   29050000
290600***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   29060000
290700***** THE SEQUENTIAL FILE.                                *****   29070000
290800***************************************************************   29080000
290900                                                                  29090000
291000     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         29100000
291100     MOVE INPUT-WRK-TAB-REC-CNT     TO GSQ-ENTRY-COUNT.           29110000
291200     MOVE INPUT-GVLH-RECORD         TO OUTPUT-WRK-RECORD.         29120000
291300     MOVE ASUR-SLOT                 TO GSQ-PROVISION-SLOT-NO.     29130000
291400     WRITE OUTPUT-GVLH-RECORD.                                    29140000
291500                                                                  29150000
291600 5002-EXIT.                                                       29160000
291700     EXIT.                                                        29170000
291800                                                                  29180000
291900/*****************************************************************29190000
292000****           GROUP SPECIFIC   'GVLP'   TABULAR                  29200000
292100******************************************************************29210000
292200 5100-PROCESS-GVLP-TABULAR.                                       29220000
292300                                                                  29230000
292400***************************************************************   29240000
292500***** IF SLOT IS LESS THAN 9,000,000                      *****   29250000
292600***** BYPASS THIS RECORD.                                 *****   29260000
292700***************************************************************   29270000
292800                                                                  29280000
292900     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                29290000
293000         GO TO 5100-EXIT.                                         29300000
293100                                                                  29310000
293200**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           29320000
293300**                                                                29330000
293400     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          29340000
293500     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        29350000
293600         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        29360000
293700         MOVE INPUT-WRK-TAB-REC-CNT  TO  GSY-ENTRY-COUNT          29370000
293800         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL29380000
293900         MOVE INPUT-GVLP-RECORD      TO  OUTPUT-WRK-RECORD        29390000
294000         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GSY-PROVISION-SLOT-NO  29400000
294100         WRITE  OUTPUT-GVLP-RECORD                                29410000
294200         GO TO 5100-EXIT.                                         29420000
294300                                                                  29430000
294400***************************************************************   29440000
294500***** CALL TABULAR HASHING PROGRAM                        *****   29450000
294600***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   29460000
294700***************************************************************   29470000
294800                                                                  29480000
294900     MOVE LOW-VALUES TO ASUR-REC-AREA.                            29490000
295000*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              29500000
295100     MOVE GSY3-RECORD              TO ASUR-REC-AREA.              29510000
295200     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          29520000
295300     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 29530000
295400                             ACCUM-SLOT-AREA.                     29540000
295500                                                                  29550000
295600***************************************************************   29560000
295700***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   29570000
295800***************************************************************   29580000
295900                                                                  29590000
296000     IF HASH-RETURN-CODE = '00'                                   29600000
296100        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  29610000
296200     ELSE                                                         29620000
296300        IF HASH-RETURN-CODE = '01'                                29630000
296400           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  29640000
296500        ELSE                                                      29650000
296600           GO TO 5100-EXIT.                                       29660000
296700                                                                  29670000
296800     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            29680000
296900     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   29690000
297000                                                                  29700000
297100***************************************************************   29710000
297200***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   29720000
297300***** THE SEQUENTIAL FILE.                                *****   29730000
297400***************************************************************   29740000
297500                                                                  29750000
297600     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         29760000
297700     MOVE INPUT-WRK-TAB-REC-CNT     TO GSY-ENTRY-COUNT.           29770000
297800     MOVE INPUT-GVLP-RECORD         TO OUTPUT-WRK-RECORD.         29780000
297900     MOVE ASUR-SLOT                 TO GSY-PROVISION-SLOT-NO.     29790000
298000     WRITE OUTPUT-GVLP-RECORD.                                    29800000
298100                                                                  29810000
298200 5100-EXIT.                                                       29820000
298300     EXIT.                                                        29830000
298400/*****************************************************************29840000
298500****           GROUP SPECIFIC   'GVLQ'   TABULAR                  29850000
298600******************************************************************29860000
298700 5101-PROCESS-GVLQ-TABULAR.                                       29870000
298800                                                                  29880000
298900***************************************************************   29890000
299000***** IF SLOT IS LESS THAN 9,000,000                      *****   29900000
299100***** BYPASS THIS RECORD.                                 *****   29910000
299200***************************************************************   29920000
299300                                                                  29930000
299400     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                29940000
299500         GO TO 5101-EXIT.                                         29950000
299600                                                                  29960000
299700**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           29970000
299800**                                                                29980000
299900     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          29990000
300000     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        30000000
300100         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        30010000
300200         MOVE INPUT-WRK-TAB-REC-CNT  TO  GSP-ENTRY-COUNT          30020000
300300         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL30030000
300400         MOVE INPUT-GVLQ-RECORD      TO  OUTPUT-WRK-RECORD        30040000
300500         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GSP-PROVISION-SLOT-NO  30050000
300600         WRITE  OUTPUT-GVLQ-RECORD                                30060000
300700         GO TO 5101-EXIT.                                         30070000
300800                                                                  30080000
300900***************************************************************   30090000
301000***** CALL TABULAR HASHING PROGRAM                        *****   30100000
301100***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   30110000
301200***************************************************************   30120000
301300                                                                  30130000
301400     MOVE LOW-VALUES TO ASUR-REC-AREA.                            30140000
301500*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              30150000
301600     MOVE GSP3-RECORD              TO ASUR-REC-AREA.              30160000
301700     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          30170000
301800     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 30180000
301900                             ACCUM-SLOT-AREA.                     30190000
302000                                                                  30200000
302100***************************************************************   30210000
302200***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   30220000
302300***************************************************************   30230000
302400                                                                  30240000
302500     IF HASH-RETURN-CODE = '00'                                   30250000
302600        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  30260000
302700     ELSE                                                         30270000
302800        IF HASH-RETURN-CODE = '01'                                30280000
302900           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  30290000
303000        ELSE                                                      30300000
303100           GO TO 5101-EXIT.                                       30310000
303200                                                                  30320000
303300     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            30330000
303400     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   30340000
303500                                                                  30350000
303600***************************************************************   30360000
303700***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   30370000
303800***** THE SEQUENTIAL FILE.                                *****   30380000
303900***************************************************************   30390000
304000                                                                  30400000
304100     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         30410000
304200     MOVE INPUT-WRK-TAB-REC-CNT     TO GSP-ENTRY-COUNT.           30420000
304300     MOVE INPUT-GVLQ-RECORD         TO OUTPUT-WRK-RECORD.         30430000
304400     MOVE ASUR-SLOT                 TO GSP-PROVISION-SLOT-NO.     30440000
304500     WRITE OUTPUT-GVLQ-RECORD.                                    30450000
304600                                                                  30460000
304700 5101-EXIT.                                                       30470000
304800     EXIT.                                                        30480000
304900/*****************************************************************30490000
305000****           GROUP SPECIFIC   'GVLR'   TABULAR                  30500000
305100******************************************************************30510000
305200 5102-PROCESS-GVLR-TABULAR.                                       30520000
305300                                                                  30530000
305400***************************************************************   30540000
305500***** IF SLOT IS LESS THAN 9,000,000                      *****   30550000
305600***** BYPASS THIS RECORD.                                 *****   30560000
305700***************************************************************   30570000
305800                                                                  30580000
305900     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                30590000
306000         GO TO 5102-EXIT.                                         30600000
306100                                                                  30610000
306200**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           30620000
306300**                                                                30630000
306400     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          30640000
306500     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        30650000
306600         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        30660000
306700         MOVE INPUT-WRK-TAB-REC-CNT  TO  GSZ-ENTRY-COUNT          30670000
306800         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL30680000
306900         MOVE INPUT-GVLR-RECORD      TO  OUTPUT-WRK-RECORD        30690000
307000         MOVE WRK-PROV-POOL-COPY-SLOT  TO  GSZ-PROVISION-SLOT-NO  30700000
307100         WRITE  OUTPUT-GVLR-RECORD                                30710000
307200         GO TO 5102-EXIT.                                         30720000
307300                                                                  30730000
307400***************************************************************   30740000
307500***** CALL TABULAR HASHING PROGRAM                        *****   30750000
307600***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   30760000
307700***************************************************************   30770000
307800                                                                  30780000
307900     MOVE LOW-VALUES TO ASUR-REC-AREA.                            30790000
308000*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              30800000
308100     MOVE GSZ3-RECORD              TO ASUR-REC-AREA.              30810000
308200     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          30820000
308300     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 30830000
308400                             ACCUM-SLOT-AREA.                     30840000
308500                                                                  30850000
308600***************************************************************   30860000
308700***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   30870000
308800***************************************************************   30880000
308900                                                                  30890000
309000     IF HASH-RETURN-CODE = '00'                                   30900000
309100        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  30910000
309200     ELSE                                                         30920000
309300        IF HASH-RETURN-CODE = '01'                                30930000
309400           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  30940000
309500        ELSE                                                      30950000
309600           GO TO 5102-EXIT.                                       30960000
309700                                                                  30970000
309800     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            30980000
309900     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   30990000
310000                                                                  31000000
310100***************************************************************   31010000
310200***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   31020000
310300***** THE SEQUENTIAL FILE.                                *****   31030000
310400***************************************************************   31040000
310500                                                                  31050000
310600     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         31060000
310700     MOVE INPUT-WRK-TAB-REC-CNT     TO GSZ-ENTRY-COUNT.           31070000
310800     MOVE INPUT-GVLR-RECORD         TO OUTPUT-WRK-RECORD.         31080000
310900     MOVE ASUR-SLOT                 TO GSZ-PROVISION-SLOT-NO.     31090000
311000     WRITE OUTPUT-GVLR-RECORD.                                    31100000
311100                                                                  31110000
311200 5102-EXIT.                                                       31120000
311300     EXIT.                                                        31130000
311400/                                                                 31140000
311500******************************************************************31150000
311600****           GROUP SPECIFIC   'GWCD'   TABULAR                  31160000
311700******************************************************************31170000
311800 5200-PROCESS-GWCD-TABULAR.                                       31180000
311900                                                                  31190000
312000***************************************************************   31200000
312100***** IF SLOT IS LESS THAN 9,000,000                      *****   31210000
312200***** BYPASS THIS RECORD.                                 *****   31220000
312300***************************************************************   31230000
312400                                                                  31240000
312500     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                31250000
312600         GO TO 5200-EXIT.                                         31260000
312700                                                                  31270000
312800***************************************************************   31280000
312900***** CALL TABULAR HASHING PROGRAM                        *****   31290000
313000***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   31300000
313100***************************************************************   31310000
313200                                                                  31320000
313300     MOVE LOW-VALUES TO ASUR-REC-AREA.                            31330000
313400*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              31340000
313500     MOVE GSI3-RECORD              TO ASUR-REC-AREA.              31350000
313600     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          31360000
313700     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 31370000
313800                             ACCUM-SLOT-AREA.                     31380000
313900                                                                  31390000
314000***************************************************************   31400000
314100***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   31410000
314200***************************************************************   31420000
314300                                                                  31430000
314400     IF HASH-RETURN-CODE = '00'                                   31440000
314500        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  31450000
314600     ELSE                                                         31460000
314700        IF HASH-RETURN-CODE = '01'                                31470000
314800           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  31480000
314900        ELSE                                                      31490000
315000           GO TO 5200-EXIT.                                       31500000
315100                                                                  31510000
315200     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            31520000
315300     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   31530000
315400                                                                  31540000
315500***************************************************************   31550000
315600***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   31560000
315700***** THE SEQUENTIAL FILE.                                *****   31570000
315800***************************************************************   31580000
315900                                                                  31590000
316000     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         31600000
316100     MOVE INPUT-WRK-TAB-REC-CNT     TO GSI-ENTRY-COUNT.           31610000
316200     MOVE INPUT-GWCD-RECORD         TO OUTPUT-WRK-RECORD.         31620000
316300     MOVE ASUR-SLOT                 TO GSI-PROVISION-SLOT-NO.     31630000
316400     WRITE OUTPUT-GWCD-RECORD.                                    31640000
316500                                                                  31650000
316600 5200-EXIT.                                                       31660000
316700     EXIT.                                                        31670000
316800/                                                                 31680000
316900******************************************************************31690000
317000****           GROUP SPECIFIC   'GFHC'   TABULAR                  31700000
317100******************************************************************31710000
317200 5300-PROCESS-GFHC-TABULAR.                                       31720000
317300                                                                  31730000
317400***************************************************************   31740000
317500***** IF SLOT IS LESS THAN 9,000,000                      *****   31750000
317600***** BYPASS THIS RECORD.                                 *****   31760000
317700***************************************************************   31770000
317800                                                                  31780000
317900     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                31790000
318000         GO TO 5300-EXIT.                                         31800000
318100                                                                  31810000
318200**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           31820000
318300**                                                                31830000
318400     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          31840000
318500     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        31850000
318600         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        31860000
318700         MOVE INPUT-WRK-TAB-REC-CNT  TO  GFHC-ENTRY-COUNT         31870000
318800         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL31880000
318900         MOVE INPUT-GFHC-RECORD      TO  OUTPUT-WRK-RECORD        31890000
319000         MOVE WRK-PROV-POOL-COPY-SLOT  TO GFHC-TABULAR-SLOT-NO    31900000
319100         WRITE  OUTPUT-GFHC-RECORD                                31910000
319200         GO TO 5300-EXIT.                                         31920000
319300                                                                  31930000
319400***************************************************************   31940000
319500***** CALL TABULAR HASHING PROGRAM                        *****   31950000
319600***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   31960000
319700***************************************************************   31970000
319800                                                                  31980000
319900     MOVE LOW-VALUES TO ASUR-REC-AREA.                            31990000
320000*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              32000000
320100     MOVE GFHC3-RECORD             TO ASUR-REC-AREA.              32010000
320200     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          32020000
320300     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 32030000
320400                             ACCUM-SLOT-AREA.                     32040000
320500                                                                  32050000
320600***************************************************************   32060000
320700***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   32070000
320800***************************************************************   32080000
320900                                                                  32090000
321000     IF HASH-RETURN-CODE = '00'                                   32100000
321100        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  32110000
321200     ELSE                                                         32120000
321300        IF HASH-RETURN-CODE = '01'                                32130000
321400           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  32140000
321500        ELSE                                                      32150000
321600           GO TO 5300-EXIT.                                       32160000
321700                                                                  32170000
321800     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            32180000
321900     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   32190000
322000                                                                  32200000
322100***************************************************************   32210000
322200***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   32220000
322300***** THE SEQUENTIAL FILE.                                *****   32230000
322400***************************************************************   32240000
322500                                                                  32250000
322600     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         32260000
322700     MOVE INPUT-WRK-TAB-REC-CNT     TO GFHC-ENTRY-COUNT.          32270000
322800     MOVE INPUT-GFHC-RECORD         TO OUTPUT-WRK-RECORD.         32280000
322900     MOVE ASUR-SLOT                 TO GFHC-TABULAR-SLOT-NO.      32290000
323000     WRITE OUTPUT-GFHC-RECORD.                                    32300000
323100                                                                  32310000
323200 5300-EXIT.                                                       32320000
323300     EXIT.                                                        32330000
323400/                                                                 32340000
323500******************************************************************32350000
323600****           GROUP SPECIFIC   'GFSA'   TABULAR                  32360000
323700******************************************************************32370000
323800 5301-PROCESS-GFSA-TABULAR.                                       32380000
323900                                                                  32390000
324000***************************************************************   32400000
324100***** IF SLOT IS LESS THAN 9,000,000                      *****   32410000
324200***** BYPASS THIS RECORD.                                 *****   32420000
324300***************************************************************   32430000
324400                                                                  32440000
324500     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                32450000
324600         GO TO 5301-EXIT.                                         32460000
324700                                                                  32470000
324800**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           32480000
324900**                                                                32490000
325000     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          32500000
325100     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        32510000
325200         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        32520000
325300         MOVE INPUT-WRK-TAB-REC-CNT  TO  GFSA-ENTRY-COUNT         32530000
325400         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL32540000
325500         MOVE INPUT-GFSA-RECORD      TO  OUTPUT-WRK-RECORD        32550000
325600         MOVE WRK-PROV-POOL-COPY-SLOT  TO GFSA-TABULAR-SLOT-NO    32560000
325700         WRITE  OUTPUT-GFSA-RECORD                                32570000
325800         GO TO 5301-EXIT.                                         32580000
325900                                                                  32590000
326000***************************************************************   32600000
326100***** CALL TABULAR HASHING PROGRAM                        *****   32610000
326200***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   32620000
326300***************************************************************   32630000
326400                                                                  32640000
326500     MOVE LOW-VALUES TO ASUR-REC-AREA.                            32650000
326600*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              32660000
326700     MOVE GFSA3-RECORD             TO ASUR-REC-AREA.              32670000
326800     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          32680000
326900     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 32690000
327000                             ACCUM-SLOT-AREA.                     32700000
327100                                                                  32710000
327200***************************************************************   32720000
327300***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   32730000
327400***************************************************************   32740000
327500                                                                  32750000
327600     IF HASH-RETURN-CODE = '00'                                   32760000
327700        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  32770000
327800     ELSE                                                         32780000
327900        IF HASH-RETURN-CODE = '01'                                32790000
328000           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  32800000
328100        ELSE                                                      32810000
328200           GO TO 5301-EXIT.                                       32820000
328300                                                                  32830000
328400     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            32840000
328500     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   32850000
328600                                                                  32860000
328700***************************************************************   32870000
328800***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   32880000
328900***** THE SEQUENTIAL FILE.                                *****   32890000
329000***************************************************************   32900000
329100                                                                  32910000
329200     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         32920000
329300     MOVE INPUT-WRK-TAB-REC-CNT     TO GFSA-ENTRY-COUNT.          32930000
329400     MOVE INPUT-GFSA-RECORD         TO OUTPUT-WRK-RECORD.         32940000
329500     MOVE ASUR-SLOT                 TO GFSA-TABULAR-SLOT-NO.      32950000
329600     WRITE OUTPUT-GFSA-RECORD.                                    32960000
329700                                                                  32970000
329800 5301-EXIT.                                                       32980000
329900     EXIT.                                                        32990000
330000/                                                                 33000000
330100******************************************************************33010000
330200****           GROUP SPECIFIC   'GHCA'   TABULAR                  33020000
330300******************************************************************33030000
330400 5302-PROCESS-GHCA-TABULAR.                                       33040000
330500                                                                  33050000
330600***************************************************************   33060000
330700***** IF SLOT IS LESS THAN 9,000,000                      *****   33070000
330800***** BYPASS THIS RECORD.                                 *****   33080000
330900***************************************************************   33090000
331000                                                                  33100000
331100     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                33110000
331200         GO TO 5302-EXIT.                                         33120000
331300                                                                  33130000
331400**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           33140000
331500**                                                                33150000
331600     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          33160000
331700     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        33170000
331800         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        33180000
331900         MOVE INPUT-WRK-TAB-REC-CNT  TO  GHCA-ENTRY-COUNT         33190000
332000         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL33200000
332100         MOVE INPUT-GHCA-RECORD      TO  OUTPUT-WRK-RECORD        33210000
332200         MOVE WRK-PROV-POOL-COPY-SLOT  TO GHCA-TABULAR-SLOT-NO    33220000
332300         WRITE  OUTPUT-GHCA-RECORD                                33230000
332400         GO TO 5302-EXIT.                                         33240000
332500                                                                  33250000
332600***************************************************************   33260000
332700***** CALL TABULAR HASHING PROGRAM                        *****   33270000
332800***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   33280000
332900***************************************************************   33290000
333000                                                                  33300000
333100     MOVE LOW-VALUES TO ASUR-REC-AREA.                            33310000
333200*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              33320000
333300     MOVE GHCA3-RECORD             TO ASUR-REC-AREA.              33330000
333400     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          33340000
333500     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 33350000
333600                             ACCUM-SLOT-AREA.                     33360000
333700                                                                  33370000
333800***************************************************************   33380000
333900***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   33390000
334000***************************************************************   33400000
334100                                                                  33410000
334200     IF HASH-RETURN-CODE = '00'                                   33420000
334300        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  33430000
334400     ELSE                                                         33440000
334500        IF HASH-RETURN-CODE = '01'                                33450000
334600           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  33460000
334700        ELSE                                                      33470000
334800           GO TO 5302-EXIT.                                       33480000
334900                                                                  33490000
335000     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            33500000
335100     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   33510000
335200                                                                  33520000
335300***************************************************************   33530000
335400***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   33540000
335500***** THE SEQUENTIAL FILE.                                *****   33550000
335600***************************************************************   33560000
335700                                                                  33570000
335800     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         33580000
335900     MOVE INPUT-WRK-TAB-REC-CNT     TO GHCA-ENTRY-COUNT.          33590000
336000     MOVE INPUT-GHCA-RECORD         TO OUTPUT-WRK-RECORD.         33600000
336100     MOVE ASUR-SLOT                 TO GHCA-TABULAR-SLOT-NO.      33610000
336200     WRITE OUTPUT-GHCA-RECORD.                                    33620000
336300                                                                  33630000
336400 5302-EXIT.                                                       33640000
336500     EXIT.                                                        33650000
336600/                                                                 33660000
336700******************************************************************33670000
336800****           GROUP SPECIFIC   'GHSA'   TABULAR                  33680000
336900******************************************************************33690000
337000 5303-PROCESS-GHSA-TABULAR.                                       33700000
337100                                                                  33710000
337200***************************************************************   33720000
337300***** IF SLOT IS LESS THAN 9,000,000                      *****   33730000
337400***** BYPASS THIS RECORD.                                 *****   33740000
337500***************************************************************   33750000
337600                                                                  33760000
337700     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                33770000
337800         GO TO 5303-EXIT.                                         33780000
337900                                                                  33790000
338000**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           33800000
338100**                                                                33810000
338200     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          33820000
338300     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        33830000
338400         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        33840000
338500         MOVE INPUT-WRK-TAB-REC-CNT  TO  GHSA-ENTRY-COUNT         33850000
338600         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL33860000
338700         MOVE INPUT-GHSA-RECORD      TO  OUTPUT-WRK-RECORD        33870000
338800         MOVE WRK-PROV-POOL-COPY-SLOT  TO GHSA-TABULAR-SLOT-NO    33880000
338900         WRITE  OUTPUT-GHSA-RECORD                                33890000
339000         GO TO 5303-EXIT.                                         33900000
339100                                                                  33910000
339200***************************************************************   33920000
339300***** CALL TABULAR HASHING PROGRAM                        *****   33930000
339400***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   33940000
339500***************************************************************   33950000
339600                                                                  33960000
339700     MOVE LOW-VALUES TO ASUR-REC-AREA.                            33970000
339800*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              33980000
339900     MOVE GHSA3-RECORD             TO ASUR-REC-AREA.              33990000
340000     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          34000000
340100     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 34010000
340200                             ACCUM-SLOT-AREA.                     34020000
340300                                                                  34030000
340400***************************************************************   34040000
340500***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   34050000
340600***************************************************************   34060000
340700                                                                  34070000
340800     IF HASH-RETURN-CODE = '00'                                   34080000
340900        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  34090000
341000     ELSE                                                         34100000
341100        IF HASH-RETURN-CODE = '01'                                34110000
341200           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  34120000
341300        ELSE                                                      34130000
341400           GO TO 5303-EXIT.                                       34140000
341500                                                                  34150000
341600     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            34160000
341700     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   34170000
341800                                                                  34180000
341900***************************************************************   34190000
342000***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   34200000
342100***** THE SEQUENTIAL FILE.                                *****   34210000
342200***************************************************************   34220000
342300                                                                  34230000
342400     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         34240000
342500     MOVE INPUT-WRK-TAB-REC-CNT     TO GHSA-ENTRY-COUNT.          34250000
342600     MOVE INPUT-GHSA-RECORD         TO OUTPUT-WRK-RECORD.         34260000
342700     MOVE ASUR-SLOT                 TO GHSA-TABULAR-SLOT-NO.      34270000
342800     WRITE OUTPUT-GHSA-RECORD.                                    34280000
342900                                                                  34290000
343000 5303-EXIT.                                                       34300000
343100     EXIT.                                                        34310000
343200/                                                                 34320000
343300******************************************************************34330000
343400****           GROUP SPECIFIC   'GLPF'   TABULAR                  34340000
343500******************************************************************34350000
343600 5304-PROCESS-GLPF-TABULAR.                                       34360000
343700                                                                  34370000
343800***************************************************************   34380000
343900***** IF SLOT IS LESS THAN 9,000,000                      *****   34390000
344000***** BYPASS THIS RECORD.                                 *****   34400000
344100***************************************************************   34410000
344200                                                                  34420000
344300     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                34430000
344400         GO TO 5304-EXIT.                                         34440000
344500                                                                  34450000
344600**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           34460000
344700**                                                                34470000
344800     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          34480000
344900     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        34490000
345000         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        34500000
345100         MOVE INPUT-WRK-TAB-REC-CNT  TO  GLPF-ENTRY-COUNT         34510000
345200         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL34520000
345300         MOVE INPUT-GLPF-RECORD      TO  OUTPUT-WRK-RECORD        34530000
345400         MOVE WRK-PROV-POOL-COPY-SLOT  TO GLPF-TABULAR-SLOT-NO    34540000
345500         WRITE  OUTPUT-GLPF-RECORD                                34550000
345600         GO TO 5304-EXIT.                                         34560000
345700                                                                  34570000
345800***************************************************************   34580000
345900***** CALL TABULAR HASHING PROGRAM                        *****   34590000
346000***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   34600000
346100***************************************************************   34610000
346200                                                                  34620000
346300     MOVE LOW-VALUES TO ASUR-REC-AREA.                            34630000
346400*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              34640000
346500     MOVE GLPF3-RECORD             TO ASUR-REC-AREA.              34650000
346600     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          34660000
346700     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 34670000
346800                             ACCUM-SLOT-AREA.                     34680000
346900                                                                  34690000
347000***************************************************************   34700000
347100***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   34710000
347200***************************************************************   34720000
347300                                                                  34730000
347400     IF HASH-RETURN-CODE = '00'                                   34740000
347500        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  34750000
347600     ELSE                                                         34760000
347700        IF HASH-RETURN-CODE = '01'                                34770000
347800           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  34780000
347900        ELSE                                                      34790000
348000           GO TO 5304-EXIT.                                       34800000
348100                                                                  34810000
348200     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            34820000
348300     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   34830000
348400                                                                  34840000
348500***************************************************************   34850000
348600***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   34860000
348700***** THE SEQUENTIAL FILE.                                *****   34870000
348800***************************************************************   34880000
348900                                                                  34890000
349000     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         34900000
349100     MOVE INPUT-WRK-TAB-REC-CNT     TO GLPF-ENTRY-COUNT.          34910000
349200     MOVE INPUT-GLPF-RECORD         TO OUTPUT-WRK-RECORD.         34920000
349300     MOVE ASUR-SLOT                 TO GLPF-TABULAR-SLOT-NO.      34930000
349400     WRITE OUTPUT-GLPF-RECORD.                                    34940000
349500                                                                  34950000
349600 5304-EXIT.                                                       34960000
349700     EXIT.                                                        34970000
349800/                                                                 34980000
349900******************************************************************34990000
350000****           GROUP SPECIFIC   'GLPH'   TABULAR                  35000000
350100******************************************************************35010000
350200 5305-PROCESS-GLPH-TABULAR.                                       35020000
350300                                                                  35030000
350400***************************************************************   35040000
350500***** IF SLOT IS LESS THAN 9,000,000                      *****   35050000
350600***** BYPASS THIS RECORD.                                 *****   35060000
350700***************************************************************   35070000
350800                                                                  35080000
350900     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                35090000
351000         GO TO 5305-EXIT.                                         35100000
351100                                                                  35110000
351200**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           35120000
351300**                                                                35130000
351400     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          35140000
351500     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        35150000
351600         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        35160000
351700         MOVE INPUT-WRK-TAB-REC-CNT  TO  GLPH-ENTRY-COUNT         35170000
351800         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL35180000
351900         MOVE INPUT-GLPH-RECORD      TO  OUTPUT-WRK-RECORD        35190000
352000         MOVE WRK-PROV-POOL-COPY-SLOT  TO GLPH-TABULAR-SLOT-NO    35200000
352100         WRITE  OUTPUT-GLPH-RECORD                                35210000
352200         GO TO 5305-EXIT.                                         35220000
352300                                                                  35230000
352400***************************************************************   35240000
352500***** CALL TABULAR HASHING PROGRAM                        *****   35250000
352600***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   35260000
352700***************************************************************   35270000
352800                                                                  35280000
352900     MOVE LOW-VALUES TO ASUR-REC-AREA.                            35290000
353000*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              35300000
353100     MOVE GLPH3-RECORD             TO ASUR-REC-AREA.              35310000
353200     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          35320000
353300     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 35330000
353400                             ACCUM-SLOT-AREA.                     35340000
353500                                                                  35350000
353600***************************************************************   35360000
353700***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   35370000
353800***************************************************************   35380000
353900                                                                  35390000
354000     IF HASH-RETURN-CODE = '00'                                   35400000
354100        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  35410000
354200     ELSE                                                         35420000
354300        IF HASH-RETURN-CODE = '01'                                35430000
354400           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  35440000
354500        ELSE                                                      35450000
354600           GO TO 5305-EXIT.                                       35460000
354700                                                                  35470000
354800     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            35480000
354900     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   35490000
355000                                                                  35500000
355100***************************************************************   35510000
355200***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   35520000
355300***** THE SEQUENTIAL FILE.                                *****   35530000
355400***************************************************************   35540000
355500                                                                  35550000
355600     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         35560000
355700     MOVE INPUT-WRK-TAB-REC-CNT     TO GLPH-ENTRY-COUNT.          35570000
355800     MOVE INPUT-GLPH-RECORD         TO OUTPUT-WRK-RECORD.         35580000
355900     MOVE ASUR-SLOT                 TO GLPH-TABULAR-SLOT-NO.      35590000
356000     WRITE OUTPUT-GLPH-RECORD.                                    35600000
356100                                                                  35610000
356200 5305-EXIT.                                                       35620000
356300     EXIT.                                                        35630000
356400/                                                                 35640000
356500******************************************************************35650000
356600****           GROUP SPECIFIC   'GWHC'   TABULAR                  35660000
356700******************************************************************35670000
356800 5306-PROCESS-GWHC-TABULAR.                                       35680000
356900                                                                  35690000
357000***************************************************************   35700000
357100***** IF SLOT IS LESS THAN 9,000,000                      *****   35710000
357200***** BYPASS THIS RECORD.                                 *****   35720000
357300***************************************************************   35730000
357400                                                                  35740000
357500     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                35750000
357600         GO TO 5306-EXIT.                                         35760000
357700                                                                  35770000
357800**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           35780000
357900**                                                                35790000
358000     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          35800000
358100     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        35810000
358200         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        35820000
358300         MOVE INPUT-WRK-TAB-REC-CNT  TO  GWHC-ENTRY-COUNT         35830000
358400         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL35840000
358500         MOVE INPUT-GWHC-RECORD      TO  OUTPUT-WRK-RECORD        35850000
358600         MOVE WRK-PROV-POOL-COPY-SLOT  TO GWHC-TABULAR-SLOT-NO    35860000
358700         WRITE  OUTPUT-GWHC-RECORD                                35870000
358800         GO TO 5306-EXIT.                                         35880000
358900                                                                  35890000
359000***************************************************************   35900000
359100***** CALL TABULAR HASHING PROGRAM                        *****   35910000
359200***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   35920000
359300***************************************************************   35930000
359400                                                                  35940000
359500     MOVE LOW-VALUES TO ASUR-REC-AREA.                            35950000
359600*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              35960000
359700     MOVE GWHC3-RECORD             TO ASUR-REC-AREA.              35970000
359800     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          35980000
359900     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 35990000
360000                             ACCUM-SLOT-AREA.                     36000000
360100                                                                  36010000
360200***************************************************************   36020000
360300***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   36030000
360400***************************************************************   36040000
360500                                                                  36050000
360600     IF HASH-RETURN-CODE = '00'                                   36060000
360700        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  36070000
360800     ELSE                                                         36080000
360900        IF HASH-RETURN-CODE = '01'                                36090000
361000           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  36100000
361100        ELSE                                                      36110000
361200           GO TO 5306-EXIT.                                       36120000
361300                                                                  36130000
361400     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            36140000
361500     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   36150000
361600                                                                  36160000
361700***************************************************************   36170000
361800***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   36180000
361900***** THE SEQUENTIAL FILE.                                *****   36190000
362000***************************************************************   36200000
362100                                                                  36210000
362200     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         36220000
362300     MOVE INPUT-WRK-TAB-REC-CNT     TO GWHC-ENTRY-COUNT.          36230000
362400     MOVE INPUT-GWHC-RECORD         TO OUTPUT-WRK-RECORD.         36240000
362500     MOVE ASUR-SLOT                 TO GWHC-TABULAR-SLOT-NO.      36250000
362600     WRITE OUTPUT-GWHC-RECORD.                                    36260000
362700                                                                  36270000
362800 5306-EXIT.                                                       36280000
362900     EXIT.                                                        36290000
363000/                                                                 36300000
363100******************************************************************36310000
363200****           GROUP SPECIFIC   'GMFH'   TABULAR                  36320000
363300******************************************************************36330000
363400 5350-PROCESS-GMFH-TABULAR.                                       36340000
363500                                                                  36350000
363600***************************************************************   36360000
363700***** IF SLOT IS LESS THAN 9,000,000                      *****   36370000
363800***** BYPASS THIS RECORD.                                 *****   36380000
363900***************************************************************   36390000
364000                                                                  36400000
364100     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                36410000
364200         GO TO 5350-EXIT.                                         36420000
364300                                                                  36430000
364400**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           36440000
364500**                                                                36450000
364600     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          36460000
364700     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        36470000
364800         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        36480000
364900         MOVE INPUT-WRK-TAB-REC-CNT  TO  GMFH-ENTRY-COUNT         36490000
365000         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL36500000
365100         MOVE INPUT-GMFH-RECORD      TO  OUTPUT-WRK-RECORD        36510000
365200         MOVE WRK-PROV-POOL-COPY-SLOT  TO GMFH-TABULAR-SLOT-NO    36520000
365300         WRITE  OUTPUT-GMFH-RECORD                                36530000
365400         GO TO 5350-EXIT.                                         36540000
365500                                                                  36550000
365600***************************************************************   36560000
365700***** CALL TABULAR HASHING PROGRAM                        *****   36570000
365800***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   36580000
365900***************************************************************   36590000
366000                                                                  36600000
366100     MOVE LOW-VALUES TO ASUR-REC-AREA.                            36610000
366200*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              36620000
366300     MOVE GMFH3-RECORD             TO ASUR-REC-AREA.              36630000
366400     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          36640000
366500     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 36650000
366600                             ACCUM-SLOT-AREA.                     36660000
366700                                                                  36670000
366800***************************************************************   36680000
366900***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   36690000
367000***************************************************************   36700000
367100                                                                  36710000
367200     IF HASH-RETURN-CODE = '00'                                   36720000
367300        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  36730000
367400     ELSE                                                         36740000
367500        IF HASH-RETURN-CODE = '01'                                36750000
367600           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  36760000
367700        ELSE                                                      36770000
367800           GO TO 5350-EXIT.                                       36780000
367900                                                                  36790000
368000     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            36800000
368100     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   36810000
368200                                                                  36820000
368300***************************************************************   36830000
368400***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   36840000
368500***** THE SEQUENTIAL FILE.                                *****   36850000
368600***************************************************************   36860000
368700                                                                  36870000
368800     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         36880000
368900     MOVE INPUT-WRK-TAB-REC-CNT     TO GMFH-ENTRY-COUNT.          36890000
369000     MOVE INPUT-GMFH-RECORD         TO OUTPUT-WRK-RECORD.         36900000
369100     MOVE ASUR-SLOT                 TO GMFH-TABULAR-SLOT-NO.      36910000
369200     WRITE OUTPUT-GMFH-RECORD.                                    36920000
369300                                                                  36930000
369400 5350-EXIT.                                                       36940000
369500     EXIT.                                                        36950000
369600/                                                                 36960000
TM0526******************************************************************36960100
TM0526****           GROUP SPECIFIC   'GHPA'   TABULAR                  36960200
TM0526******************************************************************36960300
TM0526 5360-PROCESS-GHPA-TABULAR.                                       36960400
TM0526                                                                  36960500
TM0526***************************************************************   36960600
TM0526***** IF SLOT IS LESS THAN 9,000,000                      *****   36960700
TM0526***** BYPASS THIS RECORD.                                 *****   36960800
TM0526***************************************************************   36960900
TM0526                                                                  36961000
TM0526     IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                36961100
TM0526         GO TO 5360-EXIT.                                         36961200
TM0526                                                                  36961300
TM0526**--- CHAINED TABULARS ARE TREATED AS AN EXISTING SLOT.           36961400
TM0526**                                                                36961500
TM0526     IF  WRK-PROV-POOL-COPY-SLOT > 99999                          36961600
TM0526     AND WRK-PROV-POOL-COPY-SLOT < 7000000                        36961700
TM0526         MOVE LOW-VALUES             TO  OUTPUT-WRK-RECORD        36961800
TM0526         MOVE INPUT-WRK-TAB-REC-CNT  TO  GHPA-ENTRY-COUNT         36961900
TM0526         MOVE 'E'                    TO  WRK-SIGNAL-BATCH-INTERNAL36962000
TM0526         MOVE INPUT-GHPA-RECORD      TO  OUTPUT-WRK-RECORD        36962100
TM0526         MOVE WRK-PROV-POOL-COPY-SLOT  TO GHPA-TABULAR-SLOT-NO    36962200
TM0526         WRITE  OUTPUT-GHPA-RECORD                                36962300
TM0526         GO TO 5360-EXIT.                                         36962400
TM0526                                                                  36962500
TM0526***************************************************************   36962600
TM0526***** CALL TABULAR HASHING PROGRAM                        *****   36962700
TM0526***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   36962800
TM0526***************************************************************   36962900
TM0526                                                                  36963000
TM0526     MOVE LOW-VALUES TO ASUR-REC-AREA.                            36963100
TM0526*    MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              36963200
TM0526     MOVE GHPA3-RECORD             TO ASUR-REC-AREA.              36963300
TM0526     MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          36963400
TM0526     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 36963500
TM0526                             ACCUM-SLOT-AREA.                     36963600
TM0526                                                                  36963700
TM0526***************************************************************   36963800
TM0526***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   36963900
TM0526***************************************************************   36964000
TM0526                                                                  36964100
TM0526     IF HASH-RETURN-CODE = '00'                                   36964200
TM0526        MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  36964300
TM0526     ELSE                                                         36964400
TM0526        IF HASH-RETURN-CODE = '01'                                36964500
TM0526           MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  36964600
TM0526        ELSE                                                      36964700
TM0526           GO TO 5360-EXIT.                                       36964800
TM0526                                                                  36964900
TM0526     DISPLAY ' HASH RETURN CODE = ', HASH-RETURN-CODE.            36965000
TM0526     DISPLAY ' SLOT NUMBER      = ', ASUR-SLOT.                   36965100
TM0526                                                                  36965200
TM0526***************************************************************   36965300
TM0526***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   36965400
TM0526***** THE SEQUENTIAL FILE.                                *****   36965500
TM0526***************************************************************   36965600
TM0526                                                                  36965700
TM0526     MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         36965800
TM0526     MOVE INPUT-WRK-TAB-REC-CNT     TO GHPA-ENTRY-COUNT.          36965900
TM0526     MOVE INPUT-GHPA-RECORD         TO OUTPUT-WRK-RECORD.         36966000
TM0526     MOVE ASUR-SLOT                 TO GHPA-TABULAR-SLOT-NO.      36966100
TM0526     WRITE OUTPUT-GHPA-RECORD.                                    36966200
TM0526                                                                  36966300
TM0526 5360-EXIT.                                                       36966400
TM0526     EXIT.                                                        36966500
TM0526/                                                                 36966600
369700******************************************************************36970000
369800**                                                                36980000
369900**                  CLOSE SEQUENTIAL FILES                        36990000
370000**                                                                37000000
370100******************************************************************37010000
370200 9000-CLOSE-THE-FILES.                                            37020000
370300                                                                  37030000
370400     CLOSE INPUT-WRK-FILE,                                        37040000
370500           OUTPUT-WRK-FILE.                                       37050000
370600                                                                  37060000
370700     MOVE 'C'      TO WS-GHS1BAT-PROCESS-IND.                     37070000
370800     CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA.                37080000
370900                                                                  37090000
371000 9000-EXIT.                                                       37100000
371100     EXIT.                                                        37110000
371200/                                                                 37120000
