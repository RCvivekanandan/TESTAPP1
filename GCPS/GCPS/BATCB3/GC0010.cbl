000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID. GC0010.                                              00020000
000300 AUTHOR. ROBERT MANN - DECISION CONSULTANTS INC.                  00030000
000400 INSTALLATION. HCSC.                                              00040000
000500 DATE-WRITTEN.  APR 23,1984.                                      00050000
000600 DATE-COMPILED.                                                   00060000
000700                                                                  00070000
000800******************************************************************00080000
000900*                                                                 00090000
001000*    THIS PROGRAM SPLITS THE ONLINE WORK FILE (VSAM)              00100000
001100*    INTO THE INDIVIDUAL RELEASE FILES (SEQ.)                     00110000
001200*                                                                 00120000
001300**INPUT FILE:                                                     00130000
001400*    TSGVSAM1 IS ONLINE WORK FILE -OWF (IN).                      00140000
001500**OUTPUT SEQUENTIAL FILES:                                        00150000
001600*    RBP-FILE IS RLSE-BEN-PROV-FILE (OUT).                        00160000
001700*    RBTP-FILE IS RLSE-BEN-TAB-PROV-FILE (OUT).                   00170000
001800*    RCL-FILE IS RLSE-CONT-LEVEL-FILE (OUT).                      00180000
001900*    RCTP-FILE IS RLSE-CONT-TAB-PROV-FILE (OUT).                  00190000
002000*    RGS-FILE IS RLSE-GROUP-SPEC-FILE (OUT).                      00200000
002100*    RGST-FILE IS RLSE-GROUP-SPEC-TAB-FILE (OUT).                 00210000
002200*    RALT-FILE IS RLSE-ALL-LEVEL-TAB-FILE (OUT).                  00220000
002300*    RACCUMT-FILE IS RLSE-ACCUM-TAB-FILE (OUT).                   00230000
002400*    RALI-FILE IS RLSE-ALL-LEVEL-INTERNAL-FILE (OUT).             00240000
002500*    RSMT-FILE IS RLSE-SYSTEM-MASTER-REC (OUT) AND                00250000
002600*              IS RLSE-SYSTEM-TABULAR-REC (OUT) AND               00260000
002700*              IS RLSE-SYSTEM-DCCTABLE-REC (OUT).                 00270000
002800*    R-CON-C9-FILE  -RELEASE-CONTRACT-AUDIT-WORK-FILE (OUT)      *00280000
002900*    R-GS-G9-FILE   -RELEASE-GROUP-SPECIFIC-AUDIT-WORK-FILE (OUT)*00290000
003000*    RCDRS-FILE IS RLSE-CONT-CDRS-TAB-FILE (OUT).                 00300000
003100*                                                                *00310000
003200*    RCRC-FILE IS RLSE-CONTROL-REC-CANCL-REC (OUT)                00320000
003300*    RCRC-FILE IS WRITTEN OUT USING BALX.                        *00330000
003400*                                                                *00340000
003500******************************************************************00350000
003600/*****************************************************************00360000
003700* LOG #    DATE    WHO               DESCRIPTION                 *00370000
003800* -----  --------  ---  ---------------------------------------- *00380000
003900*                                                                *00390000
004000*  PROD  05/16/02  DAF  REMOVED THE RECORD IS VARYING CLAUSE     *00400000
004100*                       FROM THE FDS FOR FILE R-CON-C9-FILE      *00410000
004200*                       AND R-GS-G9-FILE.  THE VARYING CLAUSES   *00420000
004300*                       WERE NEVER REMOVED AS STATED IN 9/24/01. *00430000
004400*                                                                *00440000
004500* D-358  09/24/01  GSP  REMOVED \
004600*                       FOR R-CON-C9-FILE AND R-GS-G9-FILE.      *00460000
004700*                       NOTE: JCL WILL BE CHANGED TO LRECL 5864. *00470000
004800*                                                                *00480000
004900* D-358  07/24/01  AKK  CHANGED TO CREATE GC0010N FILE TO        *00490000
005000*                       HOLD #CDRS TABS WITH NO INTERNALS ATTACHED00500000
005100*                                                                *00510000
005200* D-358  07/12/01  GSP  ADDED #CDRS CONTRACT TABULAR.            *00520000
005300*                       ADDED THE FOLLOWING INTERNAL TABULARS:   *00530000
005400*                         #IRDX, #IRIC, #IRPR & #IRPV            *00540000
005500*                                                                *00550000
005600*        06/28/00  GSP  ADD #IPGS INTERNAL TABULAR               *00560000
005700*                                                                *00570000
005800* D15380 03/24/99  GDM  ADDED #GBAE TABULAR.                     *00580000
005900*                                                                *00590000
006000* D15446 10/12/98  FRY  ADD #CRS CONTRACT TABULAR                *00600000
006100* D15182                ADD #ACP ACCUM TABULAR                   *00610000
006200*                                                                *00620000
006300*  14726     07/28/98  AB   RECOMPILE  TO SUPPORT THE YEAR       *00630000
006400*                           2000 GSUB CAPTURE CENTURY IN THE     *00640000
006500*                           DATE FIELD(S).                       *00650000
006600*                                                                *00660000
006700*  15057     02/17/98 KJD   FIX AUDIT REC FIXED LENGTHS          *00670000
006800*                                                                *00680000
006900*  14726/                                                        *00690000
007000*  15057     09/11/97  AB   ADDED CODE TO SUPPORT THE YEAR       *00700000
007100*                           2000 AND THE EXPANSION OF THE        *00710000
007200*                           CONTRACT KEY TO SUPPORT THE TX       *00720000
007300*                           MERGER.                              *00730000
007400*                                                                *00740000
007500*D14402   4/10/96  GDM  ADDED #GSUB TABULAR                      *00750000
007600*                                                                *00760000
007700*D14631   3/11/96  FRY  ADDED #GCBL AND #GPAN TABULARS.          *00770000
007800*                                                                *00780000
007900*D14045  02/14/95  KJD  ADDED #GCPO TABULAR                      *00790000
008000*                                                                *00800000
008100*XXXXX   01/25/95  JGR  CONTRACT RECORD EXPANSION.               *00810000
008200*                                                                *00820000
008300*        01-06-95  GDM  CONVERT TO COBOL II                      *00830000
008400*                                                                *00840000
008500*12730   12/05/92  ENW  ADDED #GRPO TABULAR.                     *00850000
008600*                                                                *00860000
008700*D-292    03-02-92 KJD  ADD #GMCS TABULAR                        *00870000
008800*                                                                *00880000
008900*D12009   09-06-91  BSO  CHANGES FOR FRL EXPANSION               *00890000
009000*                        CHANGES TO COMPENSATE MOVING PSEUDO EDIT*00900000
009100*                          INDICATOR FROM WORK RECORD TO CONTROL *00910000
009200*                          RECORD ON WORKFILE                    *00920000
009300*                                                                *00930000
009400*D11836    5-29-91  BSO  ADD #GMCT TABULAR                       *00940000
009500*                                                                *00950000
009600* 11154    3-07-91  FRY  INCREASE RECORD AREA IN FILE SECTION:   *00960000
009700*                         RALI-REC IS OUTPUT FILE -              *00970000
009800*                         RLSE-ALL-LEVEL-INTERNAL-FILE           *00980000
009900*                         FILLER  PIC X(3994)  CHANGED TO  7799. *00990000
010000*                                                                *01000000
010100* 11154   01/21/91  NGE  REDUCE THE ACCUM TAB MAX LEN TO 7805    *01010000
010200*                        BY DECREASIN MAX NUM OF OCCURS FROM     *01020000
010300*                        46 TO 44. RECL = 7805 + 64 = WRKFL TAB  *01030000
010400*                                                                *01040000
010500*                       ----ACCUM TABULAR RECORD MODIFICATION--- *01050000
010600* 11154   10/02/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *01060000
010700* D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *01070000
010800* D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *01080000
010900* D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *01090000
011000*                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *01100000
011100*                       6. INCREASE MAX REC LENGTH FOR ACCUM REC *01110000
011200*                          TO 8157, AND W/F RECORD TO 8192,      *01120000
011300*                          (8157 + 64 = 8221) BUT USING 8192 AS  *01130000
011400*                          RECORDS CREATED IN ONLINE WILL NOT    *01140000
011500*                          EXCEED TWO POINTERS.                  *01150000
011600*                                                                *01160000
011700* 11161   11/05/90 GDM  CHANGED THE SIZED OF RGS-RECORD FROM     *01170000
011800*                       774 TO 964 ON 01 LEVEL.                  *01180000
011900*                                                                *01190000
012000* D249.01 08/22/90 APH  ADDED NEW TABULARS #GMCD AND #GMCR       *01200000
012100*                                                                *01210000
012200* D249   07/02/90  ENW  ADDED #GMCG                              *01220000
012300*                                                                *01230000
012400* D184/  12/13/89  ENW  ADDED 2 NEW INTERNAL TABULARS: #IDGD,    *01240000
012500* D185                  #IPGP.                                   *01250000
012600*                                                                *01260000
012700* D199   08/10/89  RKH  ADDED 4 NEW GRP SPEC TABULARS:           *01270000
012800*                       #GVLG     #GVLH    # GVLQ   #GVLR        *01280000
012900*                                                                *01290000
013000* D170   01/31/89  FRY  ADDED LOGIC:                             *01300000
013100*                       -DISPLAY INFORMATIVE INFORMATION WHEN    *01310000
013200*                         THIS PROGRAM ABENDS,                   *01320000
013300*                       -CREATE TWO (2) FILES FOR:               *01330000
013400*                         'C9' WORK RECORDS  = R-CON-C9-FILE     *01340000
013500*                         'G9' WORK RECORDS  = R-GS-G9-FILE      *01350000
013600*                                                                *01360000
013700* D0186  05/11/88  ENW  ADDED LOGIC TO SKIP RECORDS WITH BAD     *01370000
013800*                       PSEUDO EDITS.                            *01380000
013900* D1009  01/07/88  ENW  ADDED LOGIC TO WRITE ACCUM RECORDS,      *01390000
014000*                       (#ABM, #ACL, #ADL, #AOL), TO THEIR OWN   *01400000
014100*                       FILE.                                     01410000
014200* XXXXX  09/16/87  RKH  CHANGED LOGIC FOR SPS & STS RECORDS, THEY*01420000
014300*                       WILL NOW BE WRITTEN OUT TO THE CANCELLED *01430000
014400*                       CONTROL RECORD FILE FOR DELETION EVERY   *01440000
014500*                       DAY IN GCD06A.                           *01450000
014600* D0098  06/18/87  FCG  ADDED LOGIC TO SPLIT OFF DCCTABLE RECORDS*01460000
014700*                        TO THE RSMT-FILE.                       *01470000
014800* D0120  02-11-87  JLA  BYPASS ANY I/P WORKFILE RECORDS THAT HAVE*01480000
014900*                        GROUP NUMBERS SPS000 OR STS000, THESE   *01490000
015000*                        ARE FOR SINGLE PROVISION/TABULAR SUPPORT*01500000
015100*                        AND ARE OF NO USE IN BATCH.             *01510000
015200* 02-05-87      RKH     ADDED LOGIC FOR THE KEY FIELD ADD AND    *01520000
015300*                       KEY FLD DELETE. (R & K).                 *01530000
015400* 10-28-86      NGE     ADD A CHECK FOR CRITICAL DATA ELEMENTS   *01540000
015500*                       IN RELEASE LOGIC FOR GCPS UPDATE RECORDS *01550000
015600* 08-20-86      RKH     ADDED TWO NEW TABULARS FOR THE GROUP     *01560000
015700*                       SPECIFIC RECORD.                         *01570000
015800*                       GVLFC- VARIABLE LEVEL FACILITY PROV      *01580000
015900*                       GVLPC- VARIABLE LEVEL PROFESSIONAL PROV  *01590000
016000* 06-30-86      RKH     ADDED LOGIC TO WRITE THE KEY FIELD       *01600000
016100*                       CHANGES 'K' RECORDS TO THE CANCELLED     *01610000
016200*                       RECORD FILE.                             *01620000
016300* 01-15-86      LET     DELETED CONTRACT TABULAR #CHOP AND ADDED *01630000
016400*                       CONTRACT TABULAR #CLDR IN IT'S PLACE FOR *01640000
016500*                       IMPLEMENTATION #9.                       *01650000
016600*                                                                *01660000
016700* 12-05-85      LET     ADDED A NEW GROUP SPECIFIC TABULAR #GPPO *01670000
016800*                       & ADDED ANOTHER OCCURS TO #GCCP.  IMPL 8 *01680000
016900*                                                                *01690000
017000*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *01700000
017100*                                                                *01710000
017200* 05-19-03      GTF     RECOMPILE FOR COPYBOOK CHANGES.          *01720000
017300*                                                                *01730000
017400* 07-17-03      AKK     CHANGE LENGTH OF FILLER FOR RSMT- TO     *01740000
017500*                       4006 TO ACCCOMODATE FILE EXPANSION TO    *01750000
017600*                       4012.                                    *01760000
017700*                                                                *01770000
017800* P02384 09/13/05  GDM  ADD THE FOLLOWING GROUP SPECIFIC TABULAR *01780000
017900*                       #GFHC, #GFSA, #GHCA, #GHSA, #GLPF        *01790000
018000*                       #GLPH, #GWHC                             *01800000
018100*                                                                *01810000
018200* DM9400 05/18/07  LR   ADD #GMFH GROUP SPECIFIC TABULAR         *01820000
018300*                                                                *01830000
018301*DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION        * 01831002
018302*                                                                *01832002
018200* P00893 07/15/11  ART  OCTOBER RELEASE - BLUE DISTINCTION       *01832003
      *                       RECOMPILE ONLY.                          *01832004
      *                                                                *01832005
      * P20368 09/11/15  KIKI BD / TC - RECOMPILE ONLY                 *01832006
      *                                 COPYBKS - GROUPSPC / GCTGCCPC  *01832007
      *                                                                *01832008
      * P22147 05/03/17  TROY CE - RECOMPILE ONLY                      *01832009
      *                                 COPYBKS - GROUPSPC / GCTGCCPC  *01832010
      *                                                                *01832020
      * P21681 10/23/17  SRI       RECOMPILE ONLY                      *01832030
      *                                 COPYBKS - GROUPSPC             *01832040
      * P22845 07/18/18  SRI       RECOMPILE ONLY                      *01832050
      *                                 COPYBKS - GCTGCCPC             *01832060
      * P23672 06/25/19  SRI       RECOMPILE ONLY                      *01832070
      *                                 COPYBKS - GCTGCCPC             *01832080
      * P23805 06/25/19  SRI       RECOMPILE ONLY                      *01832090
      *                                 COPYBKS - GCTGCCPC             *01832100
      * P28776 05/22/24  BRI       RECOMPILE ONLY                      *01832200
      *                                 COPYBKS - GCTGCCPC             *01832300
ED0624*BBDA-58217 06/04/24   ED   RECOMPILE FOR PEAQ COPYBOOK          *01832400
ED0624*                           EXPANSION:                           *01832500
ED0624*                                 COPYBKS - GCTABM*, GCTACL*,    *01832600
ED0624*                                 GCTACP*,  GCTADL*, GCTADL*     *01832700
018200*BBDA-66049 04/07/2026  ADD #GHPA GROUP SPECIFIC TABULAR         *01832800
018400******************************************************************01840000
018500*                                                                *01850000
018600*    PROGRAM UPDATED JULY 2001 TO INCLUDE:                       *01860000
018700*        CONTRACT TABULAR, GCTCDRS                               *01870000
018800*        INTERNAL TABS, #IRDX, #IRIC, #IRPR & #IRPV              *01880000
018900*        NEW FILE TO HOLD #CDRS WITH NO INTERNALS ATTACHED       *01890000
019000*                                                                *01900000
019100*    PROGRAM UPDATED OCTOBER 1998 TO INCLUDE:                    *01910000
019200*        CONTRACT TABULAR, GCTCRS                                *01920000
019300*        ACCUM TABULAR, GCTACP                                   *01930000
019400*                                                                *01940000
019500*    PROGRAM UPDATED MARCH 1996 TO INCLUDE:                      *01950000
019600*        GROUP SPECIFIC TABULARS:  GCTGCBL, GCTGPAN              *01960000
019700*                                                                *01970000
019800*    PROGRAM UPDATED MARCH 1985 TO INCLUDE:                       01980000
019900*        GROUP SPECIFIC TABULARS:                                 01990000
020000*            GCTGCCP   GCTGFSB   GCTGMDB                          02000000
020100*            GCTGMDN   GCTGMOB   GCTGMPB   GCTGPAB                02010000
020200*                                                                 02020000
020300*        ALL LEVEL TABULARS:                                      02030000
020400*            GCTACON   GCTACOS   GCTADIP   GCTADOP                02040000
020500*                                                                 02050000
020600*        SYSTEM MASTER RECORD AND SYSTEM TABULAR RECORDS          02060000
020700*        AND DCCTABLE RECORDS                                     02070000
020800*              (ALL THREE TYPES OF RECORDS WILL BE WRITTEN        02080000
020900*                 TO THE SAME FILE).                              02090000
021000*            GCXSTMR     (MASTER RECORD).                         02100000
021100*            GCTXCON   GCTXCOS   GCTXDIP   GCTXDOP                02110000
021200*            GCTTCON   GCTTCOS   GCTTDIP   GCTTDOP                02120000
021300*                                                                 02130000
021400*    PROGRAM UPDATED MAY 1985 TO SUPPORT BEFORE-AFTER IMAGE,      02140000
021500*        THE TABULAR GCTCRRC (WHICH IS A TABULAR FOR              02150000
021600*            CONTRACT AND BENEFIT PROVISION RECORDS)              02160000
021700*        AND THE GROUP SPECIFIC TABULARS:                         02170000
021800*            GCTGHOB  AND GCTGMSB.                                02180000
021900*                                                                 02190000
022000*    PROGRAM UPDATED SEPT 1985 TO SUPPORT                         02200000
022100*            THE GROUP SPECIFIC TABULARS:                         02210000
022200*            GCTGPAD  AND GCTGRID.                                02220000
022300*                                                                 02230000
022400*    PROGRAM UPDATE OCTOBER 1985 TO SUPPORT THE ALLOWANCE OF      02240000
022500*       DELETING RECORDS FROM PRODUCTION FILES.                   02250000
022600*                                                                 02260000
022600* 9-19-2016   RECOMPLIE PGM EXPANDED OCCURS VALUE IN GCTGPPOC     02260100
022600*                                                                 02260200
022600* 9-19-2016   RECOMPLIE PGM EXPANDED OCCURS VALUE IN GCTGPPOC     02260300
022600*                                                                 02260400
022700******************************************************************02270000
022800                                                                  02280000
022900 ENVIRONMENT DIVISION.                                            02290000
023000 CONFIGURATION SECTION.                                           02300000
023100 SOURCE-COMPUTER. IBM-370.                                        02310000
023200 OBJECT-COMPUTER. IBM-370.                                        02320000
023300 INPUT-OUTPUT SECTION.                                            02330000
023400 FILE-CONTROL.                                                    02340000
023500     SELECT RBP-FILE                                              02350000
023600                         ASSIGN TO UT-S-GC0010A.                  02360000
023700     SELECT RBTP-FILE                                             02370000
023800                         ASSIGN TO UT-S-GC0010B.                  02380000
023900     SELECT RCL-FILE                                              02390000
024000                         ASSIGN TO UT-S-GC0010C.                  02400000
024100     SELECT RCTP-FILE                                             02410000
024200                         ASSIGN TO UT-S-GC0010D.                  02420000
024300     SELECT RGS-FILE                                              02430000
024400                         ASSIGN TO UT-S-GC0010E.                  02440000
024500     SELECT RGST-FILE                                             02450000
024600                         ASSIGN TO UT-S-GC0010F.                  02460000
024700     SELECT RALT-FILE                                             02470000
024800                         ASSIGN TO UT-S-GC0010H.                  02480000
024900     SELECT RALI-FILE                                             02490000
025000                         ASSIGN TO UT-S-GC0010I.                  02500000
025100     SELECT RSMT-FILE                                             02510000
025200                         ASSIGN TO UT-S-GC0010J.                  02520000
025300     SELECT RACCUMT-FILE                                          02530000
025400                         ASSIGN TO UT-S-GC0010K.                  02540000
025500     SELECT R-CON-C9-FILE                                         02550000
025600                         ASSIGN TO UT-S-GC0010L.                  02560000
025700     SELECT R-GS-G9-FILE                                          02570000
025800                         ASSIGN TO UT-S-GC0010M.                  02580000
025900     SELECT RCDRS-FILE                                            02590000
026000                         ASSIGN TO UT-S-GC0010N.                  02600000
026100/                                                                 02610000
026200 DATA DIVISION.                                                   02620000
026300 FILE SECTION.                                                    02630000
026400                                                                  02640000
026500 FD  RBP-FILE                                                     02650000
026600     LABEL RECORDS ARE STANDARD                                   02660000
026700     RECORDING MODE IS V                                          02670000
026800     BLOCK CONTAINS 0 RECORDS.                                    02680000
026900 01  RBP-RECORD          PIC X(459).                              02690000
027000                                                                  02700000
027100 01  RBP-REC.                                                     02710000
027200     05  RBP-KEY         PIC X(100).                              02720000
027300     COPY GCBENPVC.                                               02730000
027400                                                                  02740000
027500 FD  RBTP-FILE                                                    02750000
027600     LABEL RECORDS ARE STANDARD                                   02760000
027700     RECORDING MODE IS V                                          02770000
027800     BLOCK CONTAINS 0 RECORDS.                                    02780000
027900 01  RBTP-REC.                                                    02790000
028000     05  RBTP-KEY        PIC X(100).                              02800000
028100     05  RBTP-PROV-ID    PIC X(6).                                02810000
028110*DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION        * 02811002
028200     05  FILLER          PIC X(31364).                            02820001
028300                                                                  02830000
028400 01  RBTP-REC-A.                                                  02840000
028500     05  FILLER          PIC X(100).                              02850000
028600     COPY GCTPAQC.                                                02860000
028700                                                                  02870000
028800 01  RBTP-REC-B.                                                  02880000
028900     05  FILLER          PIC X(100).                              02890000
029000     COPY GCTPPFC.                                                02900000
029100                                                                  02910000
029200 01  RBTP-REC-C.                                                  02920000
029300     05  FILLER          PIC X(100).                              02930000
029400     COPY GCTPCRC.                                                02940000
029500                                                                  02950000
029600 01  RBTP-REC-D.                                                  02960000
029700     05  FILLER          PIC X(100).                              02970000
029800     COPY GCTPCXC.                                                02980000
029900                                                                  02990000
030000 01  RBTP-REC-E.                                                  03000000
030100     05  FILLER          PIC X(100).                              03010000
030200     COPY GCTPRRC.                                                03020000
030300                                                                  03030000
030400 01  RBTP-REC-F.                                                  03040000
030500     05  FILLER          PIC X(100).                              03050000
030600     COPY GCTPDPC.                                                03060000
030700                                                                  03070000
030800 01  RBTP-REC-G.                                                  03080000
030900     05  FILLER          PIC X(100).                              03090000
031000     COPY GCTPDRC.                                                03100000
031100                                                                  03110000
031200 01  RBTP-REC-H.                                                  03120000
031300     05  FILLER          PIC X(100).                              03130000
031400     COPY GCTPRVC.                                                03140000
031500                                                                  03150000
031600 01  RBTP-REC-I.                                                  03160000
031700     05  FILLER          PIC X(100).                              03170000
031800     COPY GCTPSCC.                                                03180000
031900                                                                  03190000
032000 01  RBTP-REC-J.                                                  03200000
032100     05  FILLER          PIC X(100).                              03210000
032200     COPY GCTPVEC.                                                03220000
032300                                                                  03230000
032400 FD  RCL-FILE                                                     03240000
032500     LABEL RECORDS ARE STANDARD                                   03250000
032600     RECORDING MODE IS V                                          03260000
032700     BLOCK CONTAINS 0 RECORDS.                                    03270000
032800 01  RCL-RECORD          PIC X(8213).                             03280000
032900                                                                  03290000
033000 01  RCL-REC.                                                     03300000
033100     COPY GCWRKDC3.                                               03310000
033200     COPY GCCONTRC.                                               03320000
033300                                                                  03330000
033400 FD  RCTP-FILE                                                    03340000
033500     LABEL RECORDS ARE STANDARD                                   03350000
033600     RECORDING MODE IS V                                          03360000
033700     BLOCK CONTAINS 0 RECORDS.                                    03370000
033800 01  RCTP-REC.                                                    03380000
033900     05  RCTP-KEY        PIC X(100).                              03390000
034000     05  RCTP-PROV-ID    PIC X(6).                                03400000
034010*DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION        * 03401002
034100     05  FILLER          PIC X(31364).                            03410001
034200                                                                  03420000
034300 01  RCTP-REC-A.                                                  03430000
034400     05  FILLER          PIC X(100).                              03440000
034500     COPY GCTCLDRC.                                               03450000
034600                                                                  03460000
034700 01  RCTP-REC-B.                                                  03470000
034800     05  FILLER          PIC X(100).                              03480000
034900     COPY GCTCRRC.                                                03490000
035000                                                                  03500000
035100 01  RCTP-REC-C.                                                  03510000
035200     05  FILLER          PIC X(100).                              03520000
035300     COPY GCTCRSC.                                                03530000
035400                                                                  03540000
035500 FD  RCDRS-FILE                                                   03550000
035600     LABEL RECORDS ARE STANDARD                                   03560000
035700     RECORDING MODE IS V                                          03570000
035800     BLOCK CONTAINS 0 RECORDS.                                    03580000
035900 01  RCDRS-REC.                                                   03590000
036000     05  RCDRS-KEY       PIC X(100).                              03600000
036100     05  RCDRS-PROV-ID   PIC X(6).                                03610000
036110*DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION        * 03611002
036200     05  FILLER          PIC X(31364).                            03620001
036300                                                                  03630000
036400 01  RCDRS-REC-A.                                                 03640000
036500     05  FILLER          PIC X(100).                              03650000
036600     COPY GCTCDRSC.                                               03660000
036700                                                                  03670000
036800 FD  RGS-FILE                                                     03680000
036900     LABEL RECORDS ARE STANDARD                                   03690000
037000     RECORDING MODE IS V                                          03700000
037100     BLOCK CONTAINS 0 RECORDS.                                    03710000
037200 01  RGS-RECORD          PIC X(1300).                             03720000
037300                                                                  03730000
037400 01  RGS-REC.                                                     03740000
037500     COPY GCWRKDC4.                                               03750000
037600     COPY GCGROUPC.                                               03760000
037700                                                                  03770000
037800 FD  RGST-FILE                                                    03780000
037900     LABEL RECORDS ARE STANDARD                                   03790000
038000     RECORDING MODE IS V                                          03800000
038100     BLOCK CONTAINS 0 RECORDS.                                    03810000
038200 01  RGST-REC.                                                    03820000
038300     05  RGST-KEY        PIC X(100).                              03830000
038400     05  RGST-PROV-ID    PIC X(6).                                03840000
038410*DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION        * 03841002
038500     05  FILLER          PIC X(31364).                            03850001
038600                                                                  03860000
038700 01  RGST-REC-A.                                                  03870000
038800     05  FILLER          PIC X(100).                              03880000
038900     COPY GCTGFGC.                                                03890000
039000                                                                  03900000
039100*01  RGST-REC-B.                                                  03910000
039200*    05  FILLER          PIC X(100).                              03920000
039300*    COPY                                                         03930000
039400                                                                  03940000
039500 01  RGST-REC-C.                                                  03950000
039600     05  FILLER          PIC X(100).                              03960000
039700     COPY GCTGHORC.                                               03970000
039800                                                                  03980000
039900 01  RGST-REC-D.                                                  03990000
040000     05  FILLER          PIC X(100).                              04000000
040100     COPY GCTGMORC.                                               04010000
040200                                                                  04020000
040300 01  RGST-REC-E.                                                  04030000
040400     05  FILLER          PIC X(100).                              04040000
040500     COPY GCTGMSRC.                                               04050000
040600                                                                  04060000
040700 01  RGST-REC-F.                                                  04070000
040800     05  FILLER          PIC X(100).                              04080000
040900     COPY GCTGMSCC.                                               04090000
041000                                                                  04100000
041100 01  RGST-REC-G.                                                  04110000
041200     05  FILLER          PIC X(100).                              04120000
041300     COPY GCTGPARC.                                               04130000
041400                                                                  04140000
041500 01  RGST-REC-H.                                                  04150000
041600     05  FILLER          PIC X(100).                              04160000
041700     COPY GCTGPACC.                                               04170000
041800                                                                  04180000
041900 01  RGST-REC-I.                                                  04190000
042000     05  FILLER          PIC X(100).                              04200000
042100     COPY GCTGVLFC.                                               04210000
042200                                                                  04220000
042300 01  RGST-REC-J.                                                  04230000
042400     05  FILLER          PIC X(100).                              04240000
042500     COPY GCTGVLPC.                                               04250000
042600                                                                  04260000
042700 01  RGST-REC-K.                                                  04270000
042800     05  FILLER          PIC X(100).                              04280000
042900     COPY GCTGWCDC.                                               04290000
043000                                                                  04300000
043100 01  RGST-REC-L.                                                  04310000
043200     05  FILLER          PIC X(100).                              04320000
043300     COPY GCTGVLGC.                                               04330000
043400                                                                  04340000
043500 01  RGST-REC-M.                                                  04350000
043600     05  FILLER          PIC X(100).                              04360000
043700     COPY GCTGVLHC.                                               04370000
043800                                                                  04380000
043900 01  RGST-REC-N.                                                  04390000
044000     05  FILLER          PIC X(100).                              04400000
044100     COPY GCTGMPRC.                                               04410000
044200                                                                  04420000
044300 01  RGST-REC-O.                                                  04430000
044400     05  FILLER          PIC X(100).                              04440000
044500     COPY GCTGCCPC.                                               04450000
044600                                                                  04460000
044700 01  RGST-REC-P.                                                  04470000
044800     05  FILLER          PIC X(100).                              04480000
044900     COPY GCTGFSBC.                                               04490000
045000                                                                  04500000
045100 01  RGST-REC-Q.                                                  04510000
045200     05  FILLER          PIC X(100).                              04520000
045300     COPY GCTGMDBC.                                               04530000
045400                                                                  04540000
045500 01  RGST-REC-R.                                                  04550000
045600     05  FILLER          PIC X(100).                              04560000
045700     COPY GCTGMDNC.                                               04570000
045800                                                                  04580000
045900 01  RGST-REC-S.                                                  04590000
046000     05  FILLER          PIC X(100).                              04600000
046100     COPY GCTGMOBC.                                               04610000
046200                                                                  04620000
046300 01  RGST-REC-T.                                                  04630000
046400     05  FILLER          PIC X(100).                              04640000
046500     COPY GCTGMPBC.                                               04650000
046600                                                                  04660000
046700 01  RGST-REC-U.                                                  04670000
046800     05  FILLER          PIC X(100).                              04680000
046900     COPY GCTGPABC.                                               04690000
047000                                                                  04700000
047100 01  RGST-REC-V.                                                  04710000
047200     05  FILLER          PIC X(100).                              04720000
047300     COPY GCTGHOBC.                                               04730000
047400                                                                  04740000
047500 01  RGST-REC-W.                                                  04750000
047600     05  FILLER          PIC X(100).                              04760000
047700     COPY GCTGMSBC.                                               04770000
047800                                                                  04780000
047900 01  RGST-REC-X.                                                  04790000
048000     05  FILLER          PIC X(100).                              04800000
048100     COPY GCTGPADC.                                               04810000
048200                                                                  04820000
048300 01  RGST-REC-Y.                                                  04830000
048400     05  FILLER          PIC X(100).                              04840000
048500     COPY GCTGRIDC.                                               04850000
048600                                                                  04860000
048700 01  RGST-REC-Z.                                                  04870000
048800     05  FILLER          PIC X(100).                              04880000
048900     COPY GCTGPPOC.                                               04890000
049000                                                                  04900000
049100 01  RGST-REC-AA.                                                 04910000
049200     05  FILLER          PIC X(100).                              04920000
049300     COPY GCTGVLQC.                                               04930000
049400                                                                  04940000
049500 01  RGST-REC-AB.                                                 04950000
049600     05  FILLER          PIC X(100).                              04960000
049700     COPY GCTGVLRC.                                               04970000
049800                                                                  04980000
049900 01  RGST-REC-AC.                                                 04990000
050000     05  FILLER          PIC X(100).                              05000000
050100     COPY GCTGMCGC.                                               05010000
050200                                                                  05020000
050300 01  RGST-REC-AD.                                                 05030000
050400     05  FILLER          PIC X(100).                              05040000
050500     COPY GCTGMCDC.                                               05050000
050600                                                                  05060000
050700 01  RGST-REC-AE.                                                 05070000
050800     05  FILLER          PIC X(100).                              05080000
050900     COPY GCTGMCRC.                                               05090000
051000                                                                  05100000
051100 01  RGST-REC-AF.                                                 05110000
051200     05  FILLER          PIC X(100).                              05120000
051300     COPY GCTGMCTC.                                               05130000
051400                                                                  05140000
051500 01  RGST-REC-AG.                                                 05150000
051600     05  FILLER          PIC X(100).                              05160000
051700     COPY GCTGMCSC.                                               05170000
051800                                                                  05180000
051900 01  RGST-REC-AH.                                                 05190000
052000     05  FILLER          PIC X(100).                              05200000
052100     COPY GCTGRPOC.                                               05210000
052200                                                                  05220000
052300 01  RGST-REC-AI.                                                 05230000
052400     05  FILLER          PIC X(100).                              05240000
052500     COPY GCTGCPOC.                                               05250000
052600                                                                  05260000
052700 01  RGST-REC-AJ.                                                 05270000
052800     05  FILLER          PIC X(100).                              05280000
052900     COPY GCTGCBLC.                                               05290000
053000                                                                  05300000
053100 01  RGST-REC-AK.                                                 05310000
053200     05  FILLER          PIC X(100).                              05320000
053300     COPY GCTGPANC.                                               05330000
053400                                                                  05340000
053500 01  RGST-REC-AL.                                                 05350000
053600     05  FILLER          PIC X(100).                              05360000
053700     COPY GCTGSUBC.                                               05370000
053800                                                                  05380000
053900 01  RGST-REC-AM.                                                 05390000
054000     05  FILLER          PIC X(100).                              05400000
054100     COPY GCTGBAEC.                                               05410000
054200                                                                  05420000
054300 01  RGST-REC-AN.                                                 05430000
054400     05  FILLER          PIC X(100).                              05440000
054500     COPY GCTGFHCC.                                               05450000
054600                                                                  05460000
054700 01  RGST-REC-AO.                                                 05470000
054800     05  FILLER          PIC X(100).                              05480000
054900     COPY GCTGFSAC.                                               05490000
055000                                                                  05500000
055100 01  RGST-REC-AP.                                                 05510000
055200     05  FILLER          PIC X(100).                              05520000
055300     COPY GCTGHCAC.                                               05530000
055400                                                                  05540000
055500 01  RGST-REC-AQ.                                                 05550000
055600     05  FILLER          PIC X(100).                              05560000
055700     COPY GCTGHSAC.                                               05570000
055800                                                                  05580000
055900 01  RGST-REC-AR.                                                 05590000
056000     05  FILLER          PIC X(100).                              05600000
056100     COPY GCTGLPFC.                                               05610000
056200                                                                  05620000
056300 01  RGST-REC-AS.                                                 05630000
056400     05  FILLER          PIC X(100).                              05640000
056500     COPY GCTGLPHC.                                               05650000
056600                                                                  05660000
056700 01  RGST-REC-AT.                                                 05670000
056800     05  FILLER          PIC X(100).                              05680000
056900     COPY GCTGWHCC.                                               05690000
057000                                                                  05700000
057100 01  RGST-REC-AU.                                                 05710000
057200     05  FILLER          PIC X(100).                              05720000
057300     COPY GCTGMFHC.                                               05730000
TM0526 01  RGST-REC-AV.                                                 05730100
TM0526     05  FILLER          PIC X(100).                              05730200
TM0526     COPY GCTGHPAC.                                               05730300
057400                                                                  05740000
057500/                                                                 05750000
057600 FD  RACCUMT-FILE                                                 05760000
057700     LABEL RECORDS ARE STANDARD                                   05770000
057800     RECORDING MODE IS V                                          05780000
057900     BLOCK CONTAINS 0 RECORDS.                                    05790000
058000 01  RACCUMT-REC.                                                 05800000
058100     05  RACCUMT-KEY     PIC X(100).                              05810000
058200     05  RACCUMT-PROV-ID PIC X(6).                                05820000
058210*DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION        * 05821002
058300     05  FILLER          PIC X(31364).                            05830001
058400                                                                  05840000
058500 01  RACCUMT-REC-A.                                               05850000
058600     05  FILLER          PIC X(100).                              05860000
058700     COPY GCTABMC.                                                05870000
058800                                                                  05880000
058900 01  RACCUMT-REC-B.                                               05890000
059000     05  FILLER          PIC X(100).                              05900000
059100     COPY GCTACLC.                                                05910000
059200                                                                  05920000
059300 01  RACCUMT-REC-C.                                               05930000
059400     05  FILLER          PIC X(100).                              05940000
059500     COPY GCTACPC.                                                05950000
059600                                                                  05960000
059700 01  RACCUMT-REC-D.                                               05970000
059800     05  FILLER          PIC X(100).                              05980000
059900     COPY GCTADLC.                                                05990000
060000                                                                  06000000
060100 01  RACCUMT-REC-E.                                               06010000
060200     05  FILLER          PIC X(100).                              06020000
060300     COPY GCTAOLC.                                                06030000
060400/                                                                 06040000
060500 FD  RALT-FILE                                                    06050000
060600     LABEL RECORDS ARE STANDARD                                   06060000
060700     RECORDING MODE IS V                                          06070000
060800     BLOCK CONTAINS 0 RECORDS.                                    06080000
060900 01  RALT-REC.                                                    06090000
061000     05  RALT-KEY        PIC X(100).                              06100000
061100     05  RALT-PROV-ID    PIC X(6).                                06110000
061110*DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION        * 06111002
061200     05  FILLER          PIC X(31364).                            06120001
061300                                                                  06130000
061400 01  RALT-REC-E.                                                  06140000
061500     05  FILLER          PIC X(100).                              06150000
061600     COPY GCTAARC.                                                06160000
061700                                                                  06170000
061800 01  RALT-REC-F.                                                  06180000
061900     05  FILLER          PIC X(100).                              06190000
062000     COPY GCTACONC.                                               06200000
062100                                                                  06210000
062200 01  RALT-REC-G.                                                  06220000
062300     05  FILLER          PIC X(100).                              06230000
062400     COPY GCTACOSC.                                               06240000
062500                                                                  06250000
062600 01  RALT-REC-H.                                                  06260000
062700     05  FILLER          PIC X(100).                              06270000
062800     COPY GCTADIPC.                                               06280000
062900                                                                  06290000
063000 01  RALT-REC-I.                                                  06300000
063100     05  FILLER          PIC X(100).                              06310000
063200     COPY GCTADOPC.                                               06320000
063300                                                                  06330000
063400/                                                                 06340000
063500 FD  RALI-FILE                                                    06350000
063600     LABEL RECORDS ARE STANDARD                                   06360000
063700     RECORDING MODE IS V                                          06370000
063800     BLOCK CONTAINS 0 RECORDS.                                    06380000
063900 01  RALI-REC.                                                    06390000
064000     05  RALI-KEY        PIC X(100).                              06400000
064100     05  RALI-PROV-ID    PIC X(6).                                06410000
064110*DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION        * 06411002
064200     05  FILLER          PIC X(31364).                            06420001
064300                                                                  06430000
064400 01  RALI-REC-A.                                                  06440000
064500     05  FILLER          PIC X(100).                              06450000
064600     COPY GCTIBGRC.                                               06460000
064700                                                                  06470000
064800 01  RALI-REC-B.                                                  06480000
064900     05  FILLER          PIC X(100).                              06490000
065000     COPY GCTIPGNC.                                               06500000
065100                                                                  06510000
065200 01  RALI-REC-C.                                                  06520000
065300     05  FILLER          PIC X(100).                              06530000
065400     COPY GCTIPGTC.                                               06540000
065500                                                                  06550000
065600 01  RALI-REC-D.                                                  06560000
065700     05  FILLER          PIC X(100).                              06570000
065800     COPY GCTIDGDC.                                               06580000
065900                                                                  06590000
066000 01  RALI-REC-E.                                                  06600000
066100     05  FILLER          PIC X(100).                              06610000
066200     COPY GCTIPGPC.                                               06620000
066300                                                                  06630000
066400 01  RALI-REC-F.                                                  06640000
066500     05  FILLER          PIC X(100).                              06650000
066600     COPY GCTIPGSC.                                               06660000
066700                                                                  06670000
066800 01  RALI-REC-G.                                                  06680000
066900     05  FILLER          PIC X(100).                              06690000
067000     COPY GCTIRDXC.                                               06700000
067100                                                                  06710000
067200 01  RALI-REC-H.                                                  06720000
067300     05  FILLER          PIC X(100).                              06730000
067400     COPY GCTIRICC.                                               06740000
067500                                                                  06750000
067600 01  RALI-REC-I.                                                  06760000
067700     05  FILLER          PIC X(100).                              06770000
067800     COPY GCTIRPRC.                                               06780000
067900                                                                  06790000
068000 01  RALI-REC-J.                                                  06800000
068100     05  FILLER          PIC X(100).                              06810000
068200     COPY GCTIRPVC.                                               06820000
068300                                                                  06830000
068400/                                                                 06840000
068500 FD  RSMT-FILE                                                    06850000
068600     LABEL RECORDS ARE STANDARD                                   06860000
068700     RECORDING MODE IS V                                          06870000
068800     BLOCK CONTAINS 0 RECORDS.                                    06880000
068900 01  RSMT-REC.                                                    06890000
069000     05  RSMT-KEY        PIC X(100).                              06900000
069100     05  RSMT-PROV-ID    PIC X(6).                                06910000
069200     05  FILLER          PIC X(4006).                             06920000
069300                                                                  06930000
069400 01  RSMT-MAST-REC.                                               06940000
069500     05  FILLER          PIC X(100).                              06950000
069600     COPY GCXSTMRC.                                               06960000
069700                                                                  06970000
069800 01  RSMT-REC-A.                                                  06980000
069900     05  FILLER          PIC X(100).                              06990000
070000     COPY GCTXCONC.                                               07000000
070100                                                                  07010000
070200 01  RSMT-REC-B.                                                  07020000
070300     05  FILLER          PIC X(100).                              07030000
070400     COPY GCTXCOSC.                                               07040000
070500                                                                  07050000
070600 01  RSMT-REC-C.                                                  07060000
070700     05  FILLER          PIC X(100).                              07070000
070800     COPY GCTXDIPC.                                               07080000
070900                                                                  07090000
071000 01  RSMT-REC-D.                                                  07100000
071100     05  FILLER          PIC X(100).                              07110000
071200     COPY GCTXDOPC.                                               07120000
071300                                                                  07130000
071400 01  RSMT-REC-E.                                                  07140000
071500     05  FILLER          PIC X(100).                              07150000
071600     COPY GCTTCONC.                                               07160000
071700                                                                  07170000
071800 01  RSMT-REC-F.                                                  07180000
071900     05  FILLER          PIC X(100).                              07190000
072000     COPY GCTTCOSC.                                               07200000
072100                                                                  07210000
072200 01  RSMT-REC-G.                                                  07220000
072300     05  FILLER          PIC X(100).                              07230000
072400     COPY GCTTDIPC.                                               07240000
072500                                                                  07250000
072600 01  RSMT-REC-H.                                                  07260000
072700     05  FILLER          PIC X(100).                              07270000
072800     COPY GCTTDOPC.                                               07280000
072900                                                                  07290000
073000/                                                                 07300000
073100 FD  R-CON-C9-FILE                                                07310000
073200     LABEL RECORDS ARE STANDARD                                   07320000
073300     RECORDING MODE IS V                                          07330000
073400     BLOCK CONTAINS 0 RECORDS.                                    07340000
073500                                                                  07350000
073600 01  R-CON-C9-RECORD     PIC X(5860).                             07360000
073700                                                                  07370000
073800 01  R-CON-C9-AUDIT-WORK-RECORD.                                  07380000
073900     COPY GCWRKDC5.                                               07390000
074000     COPY GCAUDITC.                                               07400000
074100/                                                                 07410000
074200 FD  R-GS-G9-FILE                                                 07420000
074300     LABEL RECORDS ARE STANDARD                                   07430000
074400     RECORDING MODE IS V                                          07440000
074500     BLOCK CONTAINS 0 RECORDS.                                    07450000
074600                                                                  07460000
074700 01  R-GS-G9-RECORD      PIC X(5860).                             07470000
074800                                                                  07480000
074900 01  R-GS-G9-AUDIT-WORK-RECORD.                                   07490000
075000     COPY GCWRKDC6.                                               07500000
075100     COPY GCAUDIT2.                                               07510000
075200/                                                                 07520000
075300 WORKING-STORAGE SECTION.                                         07530000
075400                                                                  07540000
075500 01  FILLER                      PIC X(22)   VALUE                07550000
075600                                 'GC0010 WORKING STORAGE'.        07560000
075700                                                                  07570000
075800 01  WORK-PARM-1.                                                 07580000
075900     05  1-RESERVED-FLDS         PIC 9(8)    VALUE ZEROS COMP.    07590000
076000     05  1-RESERV-FLDS REDEFINES 1-RESERVED-FLDS.                 07600000
076100         10  1-REQUEST-TYPE      PIC X.                           07610000
076200         10  FILLER              PIC X(3).                        07620000
076300                                                                  07630000
076400 01  WORK-PARM-1A.                                                07640000
076500     02  1A-RDW.                                                  07650000
076600         05  1A-REC-LENG         PIC 9(4)    VALUE ZEROS COMP.    07660000
076700         05  1A-FEEDBACK         PIC 9(4)    VALUE ZEROS COMP.    07670000
076800     02  1A-REC-AREA             PIC X(31470).                    07680002
076900     02  1A-REC-A REDEFINES 1A-REC-AREA.                          07690000
077000         COPY GCWRKDCC.                                           07700000
077010*DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION        * 07701002
077100         05  1A-REC-DATA         PIC X(31370).                    07710002
077200     02  1A-REC-B REDEFINES 1A-REC-AREA.                          07720000
077300         COPY GCSYSDCC.                                           07730000
077310*DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION        * 07731002
077400         05  1B-REC-DATA         PIC X(31370).                    07740002
077500*  1A-REC-B IS REQUIRED BECAUSE SYSTEM MASTER/TAB RECORDS HAVE A  07750000
077600*         DIFFERENT WORK RECORD THAN OTHER GENERIC CONTRACT RECORD07760000
077700                                                                  07770000
077800                                                                  07780000
077900 01  WORK-PARM-SET.                                               07790000
078000     05  SET-RDW.                                                 07800000
078100         10  SET-REC-LENG        PIC 9(4)    VALUE ZEROS COMP.    07810000
078200         10  SET-FEEDBACK        PIC 9(4)    VALUE ZEROS COMP.    07820000
078300     05  SET-VALUE               PIC 9(8)    VALUE ZEROS COMP.    07830000
078400                                                                  07840000
078500                                                                  07850000
078600 01  GCWRKDC2-REC-WORK-AREA.                                      07860000
078700     COPY GCWRKDC2.                                               07870000
078800                                                                  07880000
078900 01  GCSTSDC2-REC-WORK-AREA.                                      07890000
079000     COPY GCSYSDC2.                                               07900000
079100                                                                  07910000
079200 01  GCCCRDCC-REC-WORK-AREA.                                      07920000
079300     COPY GCCCRDCC.                                               07930000
079400                                                                  07940000
079500 01  GCCDRLEN-REC-WORK-AREA.                                      07950000
079600     COPY GCCDRLEN.                                               07960000
079700                                                                  07970000
079800 01  ABEND-CODE                  PIC 9(4)    COMP.                07980000
079900                                                                  07990000
080000 01  WS-DISPLAY-SYS-KEY.                                          08000000
080100     05  WS-SYS-MAST-KEY.                                         08010000
080200         10  WS-SYS-MST-ID       PIC X(06)   VALUE SPACES.        08020000
080300         10  WS-SYS-MST-SLOT     PIC ZZZZZZ9.                     08030000
080400     05  FILLER                  PIC X(02)   VALUE SPACES.        08040000
080500     05  WS-SYS-SYS-KEY.                                          08050000
080600         10  WS-SYS-TBL-ID       PIC X(06)   VALUE SPACES.        08060000
080700         10  WS-SYS-TBL-SLOT     PIC ZZZZZZ9.                     08070000
080800     05  FILLER                  PIC X(03)   VALUE SPACES.        08080000
080900     05  WS-SYS-REC-TYPE         PIC X(02)   VALUE SPACES.        08090000
081000                                                                  08100000
081100 01  WS-DISPLAY-GS-KEY.                                           08110000
081200     05  WS-GRP-SPEC-KEY.                                         08120000
081300         10  WS-GRP-PLAN-CODE    PIC X(03)   VALUE SPACES.        08130000
081400         10  FILLER              PIC X(02)   VALUE SPACES.        08140000
081500         10  WS-GRP-GRP-NO       PIC X(09)   VALUE SPACES.        08150000
081600         10  FILLER              PIC X(02)   VALUE SPACES.        08160000
081700         10  WS-GRP-SEC-NO       PIC X(05)   VALUE SPACES.        08170000
081800         10  FILLER              PIC X(02)   VALUE SPACES.        08180000
081900         10  WS-GRP-PKG-CODE     PIC X(03)   VALUE SPACES.        08190000
082000         10  FILLER              PIC X(02)   VALUE SPACES.        08200000
082100         10  WS-GRP-FILLER       PIC X(03)   VALUE SPACES.        08210000
082200         10  FILLER              PIC X(02)   VALUE SPACES.        08220000
082300         10  WS-GRP-FRL          PIC X(02)   VALUE SPACES.        08230000
082400         10  FILLER              PIC X(02)   VALUE SPACES.        08240000
082500         10  WS-GRP-DATE         PIC ZZZZZZ9.                     08250000
082600     05  FILLER                  PIC X(04)   VALUE SPACES.        08260000
082700     05  WS-GRP-SPEC-REC-TYPE    PIC X(02)   VALUE SPACES.        08270000
082800                                                                  08280000
082900                                                                  08290000
083000 01  WS-DISPLAY-CON-KEY.                                          08300000
083100     05  WS-CONTRACT-KEY.                                         08310000
083200         10  WS-CON-PLAN-CODE    PIC X(03)   VALUE SPACES.        08320000
083300         10  FILLER              PIC X(02)   VALUE SPACES.        08330000
083400         10  WS-CON-GRP-NO       PIC X(09)   VALUE SPACES.        08340000
083500         10  FILLER              PIC X(02)   VALUE SPACES.        08350000
083600         10  WS-CON-SEC-NO       PIC X(05)   VALUE SPACES.        08360000
083700         10  FILLER              PIC X(02)   VALUE SPACES.        08370000
083800         10  WS-CON-PKG-CODE     PIC X(03)   VALUE SPACES.        08380000
083900         10  FILLER              PIC X(02)   VALUE SPACES.        08390000
084000         10  WS-CON-LOB          PIC X(01)   VALUE SPACES.        08400000
084100         10  FILLER              PIC X(02)   VALUE SPACES.        08410000
084200         10  WS-CON-PRV          PIC X(02)   VALUE SPACES.        08420000
084300         10  FILLER              PIC X(02)   VALUE SPACES.        08430000
084400         10  WS-CON-FRL          PIC X(02)   VALUE SPACES.        08440000
084500         10  FILLER              PIC X(02)   VALUE SPACES.        08450000
084600         10  WS-CON-DATE         PIC ZZZZZZ9.                     08460000
084700     05  FILLER                  PIC X(04)   VALUE SPACES.        08470000
084800     05  WS-CONTRACT-REC-TYPE    PIC X(02)   VALUE SPACES.        08480000
084900                                                                  08490000
085000                                                                  08500000
085100 01  HOLD-REC-AREA.                                               08510000
085200     05  HOLD-KEY        PIC X(100).                              08520000
085300     05  HOLD-PROV-ID    PIC X(6).                                08530000
085310*DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION        * 08531002
085400     05  FILLER          PIC X(31364).                            08540001
085500                                                                  08550000
085600 01  TAB-ID-WK.                                                   08560000
085700     05  TAB-ID-POS1             PIC X       VALUE SPACES.        08570000
085800     05  TAB-ID-POS2             PIC X       VALUE SPACES.        08580000
085900     05  TAB-ID-POS3-6           PIC X(4)    VALUE SPACES.        08590000
086000                                                                  08600000
086100 01  PROGRAM-VALUES.                                              08610000
086200     05  CANCEL-ID-SWITCH        PIC XXX     VALUE SPACES.        08620000
086300         88  CANCELLED-REC                   VALUE 'YES'.         08630000
086400                                                                  08640000
086500     05  SKELETON-FROM-SW        PIC XXX     VALUE SPACES.        08650000
086600         88  SKELETON-FROM-IND               VALUE 'YES'.         08660000
086700                                                                  08670000
086800     05  CAN-ID-INDEX            PIC S9(4)   VALUE ZERO COMP-3.   08680000
086900                                                                  08690000
087000     COPY HSCDATES.                                               08700000
087100                                                                  08710000
087200 01  DATE-WORK-AREA.                                              08720000
087300     05  JUL-DATE.                                                08730000
087400         10  JUL-YY              PIC 99.                          08740000
087500         10  JUL-DD              PIC 999.                         08750000
087600     05  JUL-DTE REDEFINES JUL-DATE PIC 9(5).                     08760000
087700                                                                  08770000
087800     05  MIL-DATE.                                                08780000
087900         10  MIL-J-CC            PIC 99.                          08790000
088000         10  MIL-J-YY            PIC 99.                          08800000
088100         10  MIL-J-DD            PIC 999.                         08810000
088200     05  MIL-JUL-DTE REDEFINES MIL-DATE PIC 9(7).                 08820000
088300                                                                  08830000
088400 01  IOSW            PIC  X     VALUE '0'.                        08840000
088500 01  IOFILE          PIC  9(4)   COMP SYNC  VALUE ZEROES.         08850000
088600 01  INLENGTH        PIC  9(4)   COMP SYNC  VALUE ZEROES.         08860000
088700 01  OUTLENGTH       PIC  9(4)   COMP SYNC  VALUE ZEROES.         08870000
088800 01  IAREA           PIC  X.                                      08880000
088900 01  OAREA           PIC  X(31470).                               08890002
089000 01  INPDDNAME       PIC  X(8)  VALUE 'BALXINFL'.                 08900000
089100 01  OUTDDNAMEA      PIC  X(8)  VALUE 'GC0010G '.                 08910000
089200/                                                                 08920000
089300 01  PROGRAM-TABLES.                                              08930000
089400     05  BEN-PROV-CAN-TABLE.                                      08940000
089500         10  BEN-PROV-CAN-ID-TABLE                                08950000
089600             OCCURS 755 TIMES.                                    08960000
089700           15 CAN-BP-ID                   PIC X(6).               08970000
089800/                                                                 08980000
089900 LINKAGE SECTION.                                                 08990000
090000/                                                                 09000000
090100 PROCEDURE DIVISION.                                              09010000
090200                                                                  09020000
090300 0000-MAINLINE.                                                   09030000
090400                                                                  09040000
090500     OPEN OUTPUT     RBP-FILE                                     09050000
090600                     RBTP-FILE                                    09060000
090700                     RCL-FILE                                     09070000
090800                     RCTP-FILE                                    09080000
090900                     RGS-FILE                                     09090000
091000                     RGST-FILE                                    09100000
091100                     RACCUMT-FILE                                 09110000
091200                     RALT-FILE                                    09120000
091300                     RSMT-FILE                                    09130000
091400                     RALI-FILE                                    09140000
091500                     R-CON-C9-FILE                                09150000
091600                     R-GS-G9-FILE                                 09160000
091700                     RCDRS-FILE.                                  09170000
091800                                                                  09180000
091900     MOVE 'S'    TO  1-REQUEST-TYPE.                              09190000
092000     MOVE 8      TO  SET-REC-LENG.                                09200000
092100     MOVE 3      TO  SET-VALUE.                                   09210000
092200     CALL 'TSGVSAM1' USING WORK-PARM-1 WORK-PARM-SET.             09220000
092300                                                                  09230000
092400     IF  1-REQUEST-TYPE NOT EQUAL 'S'                             09240000
092500         DISPLAY  '  '                                            09250000
092600         DISPLAY  ' BAD SET    GC0010     0000-MAINLINE'          09260000
092700         MOVE SET-FEEDBACK  TO  ABEND-CODE                        09270000
092800         GO TO 0900-ERROR-RTN.                                    09280000
092900                                                                  09290000
093000                                                                  09300000
093100     MOVE 'O'  TO  1-REQUEST-TYPE.                                09310000
093200     CALL 'TSGVSAM1' USING WORK-PARM-1 WORK-PARM-1A.              09320000
093300                                                                  09330000
093400     IF  1-REQUEST-TYPE NOT EQUAL 'O'                             09340000
093500         DISPLAY  '  '                                            09350000
093600         DISPLAY  ' BAD OPEN   GC0010     0000-MAINLINE'          09360000
093700         MOVE 1A-FEEDBACK  TO  ABEND-CODE                         09370000
093800         GO TO 0900-ERROR-RTN.                                    09380000
093900                                                                  09390000
094000     CALL 'TCDTES' USING HSCDATES.                                09400000
094100     MOVE JYR    TO JUL-YY.                                       09410000
094200     MOVE JDA    TO JUL-DD.                                       09420000
094300                                                                  09430000
094400     IF JUL-DTE < +70000                                          09440000
094500         MOVE 20          TO MIL-J-CC                             09450000
094600         MOVE JUL-YY      TO MIL-J-YY                             09460000
094700         MOVE JUL-DD      TO MIL-J-DD                             09470000
094800      ELSE                                                        09480000
094900         MOVE 19          TO MIL-J-CC                             09490000
095000         MOVE JUL-YY      TO MIL-J-YY                             09500000
095100         MOVE JUL-DD      TO MIL-J-DD                             09510000
095200     END-IF.                                                      09520000
095300                                                                  09530000
095400     PERFORM 0030-READ-OWF THRU 0030-EXIT.                        09540000
095500                                                                  09550000
095600     IF  WORK-RECORD-KEY EQUAL LOW-VALUES                         09560000
095700         PERFORM 0030-READ-OWF THRU 0030-EXIT.                    09570000
095800                                                                  09580000
095900                                                                  09590000
096000     PERFORM 0005-PROCESS THRU 0005-EXIT                          09600000
096100         UNTIL 1-REQUEST-TYPE EQUAL '2'.                          09610000
096200                                                                  09620000
096300     CLOSE           RBP-FILE                                     09630000
096400                     RBTP-FILE                                    09640000
096500                     RCL-FILE                                     09650000
096600                     RCTP-FILE                                    09660000
096700                     RGS-FILE                                     09670000
096800                     RGST-FILE                                    09680000
096900                     RACCUMT-FILE                                 09690000
097000                     RALT-FILE                                    09700000
097100                     RSMT-FILE                                    09710000
097200                     RALI-FILE                                    09720000
097300                     R-CON-C9-FILE                                09730000
097400                     R-GS-G9-FILE                                 09740000
097500                     RCDRS-FILE.                                  09750000
097600                                                                  09760000
097700     MOVE 'C'  TO  1-REQUEST-TYPE.                                09770000
097800     CALL 'TSGVSAM1' USING WORK-PARM-1 WORK-PARM-1A.              09780000
097900                                                                  09790000
098000     IF  1-REQUEST-TYPE NOT EQUAL 'C'                             09800000
098100         DISPLAY  '  '                                            09810000
098200         DISPLAY  ' BAD CLOSE   GC0010     0000-MAINLINE'         09820000
098300         MOVE 1A-FEEDBACK   TO  ABEND-CODE                        09830000
098400         GO TO 0900-ERROR-RTN.                                    09840000
098500                                                                  09850000
098600     MOVE '4'  TO IOSW.                                           09860000
098700     PERFORM 0210-CALL-BALX THRU 0210-EXIT.                       09870000
098800                                                                  09880000
098900     GOBACK.                                                      09890000
099000/                                                                 09900000
099100 0005-PROCESS.                                                    09910000
099200                                                                  09920000
099300     MOVE SPACES  TO  SKELETON-FROM-SW.                           09930000
099400                                                                  09940000
099500     IF  WRK-STATUS-CODE NOT = 'S'                                09950000
099600         MOVE HIGH-VALUES TO BEN-PROV-CAN-TABLE                   09960000
099700         MOVE 1 TO CAN-ID-INDEX                                   09970000
099800         MOVE WORK-RECORD            TO WORK-RECORD-2             09980000
099900         MOVE 1A-REC-DATA            TO CONTRACT-CONTROL-RECORD.  09990000
100000                                                                  10000000
100100     IF  WRK-REC-CONT-CON                                         10010000
100200         IF (WRK-DELETE-REQUEST OR                                10020000
100300             WRK-KEY-FLD-DEL-REQ OR                               10030000
100400             WRK-GROUP-NUM = '000SPS000'  OR                      10040000
100500             WRK-GROUP-NUM = '000STS000' )                        10050000
100600             PERFORM 0006-DEL-REC-FOUND  THRU  0006-EXIT          10060000
100700             GO  TO  0005-EXIT                                    10070000
100800         ELSE                                                     10080000
100900             PERFORM 0007-CHECK-SKELETON  THRU  0007-EXIT         10090000
101000             PERFORM 0010-PROCESS-CON     THRU  0010-EXIT         10100000
101100             GO  TO  0005-EXIT.                                   10110000
101200                                                                  10120000
101300     IF  WRK-REC-GROUP-SPEC-CTL                                   10130000
101400         IF (WRK-DELETE-REQUEST   OR                              10140000
101500             WRK-KEY-FLD-DEL-REQ  OR                              10150000
101600             WRK-GROUP-NUM = '000SPS000'  OR                      10160000
101700             WRK-GROUP-NUM = '000STS000' )                        10170000
101800             PERFORM 0006-DEL-REC-FOUND  THRU  0006-EXIT          10180000
101900             GO  TO  0005-EXIT                                    10190000
102000         ELSE                                                     10200000
102100             PERFORM 0007-CHECK-SKELETON THRU 0007-EXIT           10210000
102200             PERFORM 0015-PROCESS-GRP-SPEC THRU 0015-EXIT         10220000
102300             GO TO 0005-EXIT.                                     10230000
102400                                                                  10240000
102500     IF  WRK-STAT-SYSTEM-RECORD         AND                       10250000
102600         XWRK-NON-KEY-RELEASE-DATE-ALP EQUAL SPACES               10260000
102700         PERFORM 0030-READ-OWF    THRU 0030-EXIT                  10270000
102800         GO TO 0005-EXIT.                                         10280000
102900                                                                  10290000
103000     IF  WRK-STAT-SYSTEM-RECORD AND                               10300000
103100         XWRK-NON-KEY-RELEASE-DATE-ALP  NOT EQUAL SPACES          10310000
103200         PERFORM 0300-PROCESS-SYS-RECD THRU 0300-EXIT             10320000
103300         GO TO 0005-EXIT.                                         10330000
103400                                                                  10340000
103500                                                                  10350000
103600                                                                  10360000
103700**DISPLAY WORK RECORD KEY WHEN ABENDING.                          10370000
103800     DISPLAY  '  '.                                               10380000
103900     DISPLAY  ' GC0010       0005-PROCESS '.                      10390000
104000                                                                  10400000
104100     IF  WRK-STATUS-CODE   =   'C'                                10410000
104200         DISPLAY  ' WORK-CONTRACT-KEY    =  ' WS-DISPLAY-CON-KEY  10420000
104300     ELSE                                                         10430000
104400     IF  WRK-STATUS-CODE   =   'G'                                10440000
104500         DISPLAY  ' WORK-GROUP-SPEC-KEY  =  ' WS-DISPLAY-GS-KEY   10450000
104600     ELSE                                                         10460000
104700     IF  WRK-STATUS-CODE   =   'S'                                10470000
104800         DISPLAY  ' WORK-SYSTEM-RECORD   =  ' WS-DISPLAY-SYS-KEY  10480000
104900     ELSE                                                         10490000
105000     DISPLAY  ' WORK-RECORD-KEY  =  ' WORK-RECORD-KEY.            10500000
105100                                                                  10510000
105200     MOVE 0005  TO  ABEND-CODE.                                   10520000
105300     GO TO 0900-ERROR-RTN.                                        10530000
105400                                                                  10540000
105500 0005-EXIT.                                                       10550000
105600     EXIT.                                                        10560000
105700/                                                                 10570000
105800 0006-DEL-REC-FOUND.                                              10580000
105900                                                                  10590000
106000     PERFORM 0200-WRITE-RCRC                                      10600000
106100        THRU 0200-EXIT UNTIL (1-REQUEST-TYPE  =  '2')             10610000
106200                                     OR                           10620000
106300                       (WRK-MATCH-CONT NOT = WRK2-MATCH-CONT).    10630000
106400                                                                  10640000
106500 0006-EXIT.  EXIT.                                                10650000
106600/                                                                 10660000
106700 0007-CHECK-SKELETON.                                             10670000
106800                                                                  10680000
106900     MOVE SPACES  TO  SKELETON-FROM-SW.                           10690000
107000     IF (CCR-CON-SKELETON  OR  CCR-GRPSPEC-SKELETON)              10700000
107100        MOVE 'YES'  TO  SKELETON-FROM-SW.                         10710000
107200                                                                  10720000
107300 0007-EXIT.                                                       10730000
107400     EXIT.                                                        10740000
107500/                                                                 10750000
107600 0010-PROCESS-CON.                                                10760000
107700                                                                  10770000
107800     IF  CCR-CON-RLSEDT-CEN EQUAL +9999999                        10780000
107900         MOVE MIL-JUL-DTE        TO CCR-CON-RLSEDT-CEN            10790000
108000         MOVE 'X'                TO WRK2-SIGNAL-FROM-ONLINE       10800000
108100         PERFORM 0200-WRITE-RCRC THRU 0200-EXIT                   10810000
108200             UNTIL (1-REQUEST-TYPE EQUAL '2') OR                  10820000
108300             (WRK-MATCH-CONT NOT EQUAL WRK2-MATCH-CONT)           10830000
108400         GO TO 0010-EXIT.                                         10840000
108500                                                                  10850000
108600     IF  CCR-CON-RLSEDT-CEN = ZEROS       OR                      10860000
108700      CCR-CON-RLSEDT-CEN   >  MIL-JUL-DTE OR                      10870000
108800         ( WRK2-CDE-SP = '1U' OR '1P' OR '1H')  THEN              10880000
108900         MOVE SPACES  TO  SKELETON-FROM-SW                        10890000
109000         PERFORM 0030-READ-OWF THRU 0030-EXIT                     10900000
109100             UNTIL (1-REQUEST-TYPE EQUAL '2') OR                  10910000
109200             (WRK-MATCH-CONT NOT EQUAL WRK2-MATCH-CONT)           10920000
109300         GO TO 0010-EXIT.                                         10930000
109400                                                                  10940000
109500     PERFORM 0200-WRITE-RCRC THRU 0200-EXIT.                      10950000
109600                                                                  10960000
109700     PERFORM 0020-SPLIT-TYPES THRU 0020-EXIT                      10970000
109800         UNTIL (1-REQUEST-TYPE EQUAL '2') OR                      10980000
109900         (WRK-MATCH-CONT NOT EQUAL WRK2-MATCH-CONT).              10990000
110000                                                                  11000000
110100 0010-EXIT.                                                       11010000
110200     EXIT.                                                        11020000
110300/                                                                 11030000
110400 0015-PROCESS-GRP-SPEC.                                           11040000
110500                                                                  11050000
110600     IF  CCR-GRP-SPEC-RLSEDT-CEN EQUAL +9999999                   11060000
110700         MOVE MIL-JUL-DTE     TO CCR-GRP-SPEC-RLSEDT-CEN          11070000
110800         MOVE 'X'             TO WRK2-SIGNAL-FROM-ONLINE          11080000
110900         PERFORM 0200-WRITE-RCRC THRU 0200-EXIT                   11090000
111000             UNTIL (1-REQUEST-TYPE EQUAL '2') OR                  11100000
111100             (WRK-MATCH-CONT NOT EQUAL WRK2-MATCH-CONT)           11110000
111200         GO TO 0015-EXIT.                                         11120000
111300                                                                  11130000
111400     IF  CCR-PSEUDO-EDIT-IS-BAD                OR                 11140000
111500         CCR-GRP-SPEC-RLSEDT-CEN = ZEROS       OR                 11150000
111600     CCR-GRP-SPEC-RELEASE-DATE  > MIL-JUL-DTE  OR                 11160000
111700         (WRK2-CDE-SP = '1U' OR '1P' OR '1H')   THEN              11170000
111800             PERFORM 0030-READ-OWF THRU 0030-EXIT                 11180000
111900                 UNTIL (1-REQUEST-TYPE EQUAL '2') OR              11190000
112000                 (WRK-MATCH-CONT NOT EQUAL WRK2-MATCH-CONT)       11200000
112100             GO TO 0015-EXIT.                                     11210000
112200                                                                  11220000
112300     PERFORM 0200-WRITE-RCRC THRU 0200-EXIT.                      11230000
112400                                                                  11240000
112500     PERFORM 0020-SPLIT-TYPES THRU 0020-EXIT                      11250000
112600         UNTIL (1-REQUEST-TYPE EQUAL '2') OR                      11260000
112700         (WRK-MATCH-CONT NOT EQUAL WRK2-MATCH-CONT).              11270000
112800                                                                  11280000
112900 0015-EXIT.                                                       11290000
113000     EXIT.                                                        11300000
113100/                                                                 11310000
113200 0020-SPLIT-TYPES.                                                11320000
113300                                                                  11330000
113400     IF  WRK-REC-CONT-BEN-PROV AND                                11340000
113500         WRK-CANCEL-REQUEST                                       11350000
113600         MOVE 'X'                TO WRK2-SIGNAL-FROM-ONLINE       11360000
113700         PERFORM 0150-BUILD-TABLE THRU 0150-EXIT                  11370000
113800         PERFORM 0200-WRITE-RCRC THRU 0200-EXIT                   11380000
113900         GO TO 0020-EXIT.                                         11390000
114000                                                                  11400000
114100     IF  WRK-REC-CONT-BEN-PROV                                    11410000
114200         PERFORM 0040-WRITE-RBP THRU 0040-EXIT.                   11420000
114300                                                                  11430000
114400     IF  (WRK-REC-CONT-BEN-TAB-PROV OR                            11440000
114500          WRK-REC-CONT-BEN-TAB-TAB)                               11450000
114600          PERFORM 0160-SEARCH-TABLE THRU 0160-EXIT                11460000
114700       IF  CANCELLED-REC                                          11470000
114800           MOVE 'X'                TO WRK2-SIGNAL-FROM-ONLINE     11480000
114900           PERFORM 0200-WRITE-RCRC THRU 0200-EXIT                 11490000
115000           GO TO 0020-EXIT.                                       11500000
115100                                                                  11510000
115200     IF  WRK-REC-CONT-BEN-TAB-PROV                                11520000
115300         PERFORM 0050-WRITE-RBTP THRU 0050-EXIT.                  11530000
115400                                                                  11540000
115500     IF  WRK-REC-CONT                                             11550000
115600         PERFORM 0060-WRITE-RCL THRU 0060-EXIT.                   11560000
115700                                                                  11570000
115800     IF  WRK-REC-CONT-TAB                                         11580000
115900         PERFORM 0021-DETERMINE-TAB-TYPE THRU 0021-EXIT.          11590000
116000*        PERFORM 0070-WRITE-RCTP THRU 0070-EXIT.                  11600000
116100                                                                  11610000
116200     IF  WRK-REC-GROUP-SPEC                                       11620000
116300         PERFORM 0080-WRITE-RGS THRU 0080-EXIT.                   11630000
116400                                                                  11640000
116500     IF  WRK-REC-GROUP-SPEC-TAB                                   11650000
116600         PERFORM 0090-WRITE-RGST THRU 0090-EXIT.                  11660000
116700                                                                  11670000
116800     IF  WRK-REC-CONT-BEN-TAB-TAB OR                              11680000
116900         WRK-REC-GROUP-SPEC-TAB-TAB                               11690000
117000         PERFORM 0110-WRITE-RALI THRU 0110-EXIT.                  11700000
117100                                                                  11710000
117200**WRITE 'C9' AUDIT WORK RECORD TO SEPARATE FILE.                  11720000
117300     IF  WRK-REC-CONTRACT-AUDIT                                   11730000
117400         PERFORM 0120-WRITE-C9-AUDIT-WORK-REC.                    11740000
117500                                                                  11750000
117600**WRITE 'G9' AUDIT WORK RECORD TO SEPARATE FILE.                  11760000
117700     IF  WRK-REC-GROUP-SPEC-AUDIT                                 11770000
117800         PERFORM 0130-WRITE-G9-AUDIT-WORK-REC.                    11780000
117900                                                                  11790000
118000                                                                  11800000
118100**READ ANOTHER WORK RECORD.                                       11810000
118200     PERFORM 0030-READ-OWF THRU 0030-EXIT.                        11820000
118300                                                                  11830000
118400 0020-EXIT.                                                       11840000
118500     EXIT.                                                        11850000
118600*************************************************************     11860000
118700*                                                                 11870000
118800* THIS WILL DETERMINE IF C3 RECORD IS #CDRS OR AN ACCUM TYPE      11880000
118900* THE #CDRS WILL BE WRITTEN TO THE RCDRS AND THE ACCUMS TO RCTP   11890000
119000* AS BEFORE.                                                      11900000
119100******************************************************************11910000
119200 0021-DETERMINE-TAB-TYPE.                                         11920000
119300     MOVE 1A-REC-AREA TO HOLD-REC-AREA.                           11930000
119400     IF HOLD-PROV-ID = '#CDRS'                                    11940000
119500        PERFORM 0071-WRITE-RCDRS THRU 0071-EXIT                   11950000
119600     ELSE                                                         11960000
119700        PERFORM 0070-WRITE-RCTP THRU 0070-EXIT                    11970000
119800     END-IF.                                                      11980000
119900                                                                  11990000
120000 0021-EXIT.                                                       12000000
120100     EXIT.                                                        12010000
120200/                                                                 12020000
120300 0030-READ-OWF.                                                   12030000
120400                                                                  12040000
120500     MOVE 'G'                    TO 1-REQUEST-TYPE.               12050000
120600     CALL 'TSGVSAM1' USING WORK-PARM-1 WORK-PARM-1A.              12060000
120700                                                                  12070000
120800     IF  1-REQUEST-TYPE EQUAL '2'                                 12080000
120900         GO TO 0030-EXIT.                                         12090000
121000                                                                  12100000
121100     IF  1-REQUEST-TYPE NOT EQUAL 'G'                             12110000
121200         DISPLAY  '  '                                            12120000
121300         DISPLAY  ' BAD GET     GC0010    0030-READ-OWF'          12130000
121400         MOVE 1A-FEEDBACK  TO  ABEND-CODE                         12140000
121500         GO TO 0900-ERROR-RTN.                                    12150000
121600                                                                  12160000
121700     COMPUTE  WRK-RECORD-LENGTH EQUAL                             12170000
121800         1A-REC-LENG - 4.                                         12180000
121900                                                                  12190000
122000                                                                  12200000
122100     IF  WRK-STATUS-CODE   =   'C'                                12210000
122200         MOVE WRK-PLAN-CODE      TO  WS-CON-PLAN-CODE             12220000
122300         MOVE WRK-GROUP-NUM      TO  WS-CON-GRP-NO                12230000
122400         MOVE WRK-SECTION-NUM    TO  WS-CON-SEC-NO                12240000
122500         MOVE WRK-PKG-CODE       TO  WS-CON-PKG-CODE              12250000
122600         MOVE WRK-L-O-B          TO  WS-CON-LOB                   12260000
122700         MOVE WRK-PROV-CTL       TO  WS-CON-PRV                   12270000
122800         MOVE WRK-FAM-REL-LEVEL  TO  WS-CON-FRL                   12280000
122900         MOVE WRK-EFFECTIVE-DATE TO  WS-CON-DATE                  12290000
123000         MOVE WRK-REC-TYPE       TO  WS-CONTRACT-REC-TYPE         12300000
123100     ELSE                                                         12310000
123200     IF  WRK-STATUS-CODE   =   'G'                                12320000
123300         MOVE WRK-PLAN-CODE      TO  WS-GRP-PLAN-CODE             12330000
123400         MOVE WRK-GROUP-NUM      TO  WS-GRP-GRP-NO                12340000
123500         MOVE WRK-SECTION-NUM    TO  WS-GRP-SEC-NO                12350000
123600         MOVE WRK-PKG-CODE       TO  WS-GRP-PKG-CODE              12360000
123700         MOVE WRK-FAM-REL-LEVEL  TO  WS-GRP-FRL                   12370000
123800         MOVE WRK-EFFECTIVE-DATE TO  WS-GRP-DATE                  12380000
123900         MOVE WRK-REC-TYPE       TO  WS-GRP-SPEC-REC-TYPE         12390000
124000     ELSE                                                         12400000
124100     IF  WRK-STATUS-CODE   =   'S'                                12410000
124200         MOVE XWRK-SYS-TBL-MSTR-REC-ID      TO  WS-SYS-MST-ID     12420000
124300         MOVE XWRK-SYS-TBL-MSTR-REC-SLOT-NO TO  WS-SYS-MST-SLOT   12430000
124400         MOVE XWRK-SYS-TBL-ID               TO  WS-SYS-TBL-ID     12440000
124500         MOVE XWRK-SYS-TBL-SLOT-NO          TO  WS-SYS-TBL-SLOT   12450000
124600         MOVE XWRK-SYS-REC-TYPE             TO  WS-SYS-REC-TYPE.  12460000
124700                                                                  12470000
124800 0030-EXIT.                                                       12480000
124900     EXIT.                                                        12490000
125000/                                                                 12500000
125100 0040-WRITE-RBP.                                                  12510000
125200     MOVE GC-GCBENPRV-VARY-MAX-OCUR TO                            12520000
125300          GCP-COUNT-TAB-PROVN-POINTERS.                           12530000
125400     MOVE 1A-REC-AREA            TO RBP-REC.                      12540000
125500     WRITE RBP-REC.                                               12550000
125600                                                                  12560000
125700 0040-EXIT.                                                       12570000
125800     EXIT.                                                        12580000
125900/                                                                 12590000
126000 0050-WRITE-RBTP.                                                 12600000
126100                                                                  12610000
126200     MOVE 1A-REC-AREA            TO RBTP-REC.                     12620000
126300                                                                  12630000
126400     MOVE RBTP-PROV-ID           TO TAB-ID-WK.                    12640000
126500                                                                  12650000
126600     IF TAB-ID-WK   =                                             12660000
126700       '#ABM  ' OR '#ACL  '  OR '#ACP  ' OR '#ADL  ' OR '#AOL  '  12670000
126800         PERFORM 0095-WRITE-RACCUMT THRU 0095-EXIT                12680000
126900         GO TO 0050-EXIT.                                         12690000
127000                                                                  12700000
127100     IF  TAB-ID-POS2 EQUAL 'A'                                    12710000
127200         PERFORM 0100-WRITE-RALT THRU 0100-EXIT                   12720000
127300         GO TO 0050-EXIT.                                         12730000
127400                                                                  12740000
127500     IF  RBTP-PROV-ID EQUAL '#PAQ  '                              12750000
127600         MOVE GC-GCTABULR-PAQ-VARY-MAX-OCUR TO                    12760000
127700              GBA-ENTRY-COUNT                                     12770000
127800         MOVE 1A-REC-AREA        TO RBTP-REC-A                    12780000
127900         WRITE RBTP-REC-A                                         12790000
128000         GO TO 0050-EXIT.                                         12800000
128100                                                                  12810000
128200     IF  RBTP-PROV-ID EQUAL '#PPF  '                              12820000
128300         MOVE GC-GCTABULR-PPF-VARY-MAX-OCUR TO                    12830000
128400              GBB-ENTRY-COUNT                                     12840000
128500         MOVE 1A-REC-AREA        TO RBTP-REC-B                    12850000
128600         WRITE RBTP-REC-B                                         12860000
128700         GO TO 0050-EXIT.                                         12870000
128800                                                                  12880000
128900     IF  RBTP-PROV-ID EQUAL '#PCR  '                              12890000
129000         MOVE GC-GCTABULR-PCR-VARY-MAX-OCUR TO                    12900000
129100              GBC-ENTRY-COUNT                                     12910000
129200         MOVE 1A-REC-AREA        TO RBTP-REC-C                    12920000
129300         WRITE RBTP-REC-C                                         12930000
129400         GO TO 0050-EXIT.                                         12940000
129500                                                                  12950000
129600     IF  RBTP-PROV-ID EQUAL '#PCX  '                              12960000
129700         MOVE GC-GCTABULR-PCX-VARY-MAX-OCUR TO                    12970000
129800              GBD-ENTRY-COUNT                                     12980000
129900         MOVE 1A-REC-AREA        TO RBTP-REC-D                    12990000
130000         WRITE RBTP-REC-D                                         13000000
130100         GO TO 0050-EXIT.                                         13010000
130200                                                                  13020000
130300     IF  RBTP-PROV-ID EQUAL '#PRR  '                              13030000
130400         MOVE GC-GCTABULR-PRR-VARY-MAX-OCUR TO                    13040000
130500              GBE-ENTRY-COUNT                                     13050000
130600         MOVE 1A-REC-AREA        TO RBTP-REC-E                    13060000
130700         WRITE RBTP-REC-E                                         13070000
130800         GO TO 0050-EXIT.                                         13080000
130900                                                                  13090000
131000     IF  RBTP-PROV-ID EQUAL '#PDP  '                              13100000
131100         MOVE GC-GCTABULR-PDP-VARY-MAX-OCUR TO                    13110000
131200              GBF-ENTRY-COUNT                                     13120000
131300         MOVE 1A-REC-AREA        TO RBTP-REC-F                    13130000
131400         WRITE RBTP-REC-F                                         13140000
131500         GO TO 0050-EXIT.                                         13150000
131600                                                                  13160000
131700     IF  RBTP-PROV-ID EQUAL '#PDR  '                              13170000
131800         MOVE GC-GCTABULR-PDR-VARY-MAX-OCUR TO                    13180000
131900              GBG-ENTRY-COUNT                                     13190000
132000         MOVE 1A-REC-AREA        TO RBTP-REC-G                    13200000
132100         WRITE RBTP-REC-G                                         13210000
132200         GO TO 0050-EXIT.                                         13220000
132300                                                                  13230000
132400     IF  RBTP-PROV-ID EQUAL '#PRV  '                              13240000
132500         MOVE GC-GCTABULR-PRV-VARY-MAX-OCUR TO                    13250000
132600              GBH-ENTRY-COUNT                                     13260000
132700         MOVE 1A-REC-AREA        TO RBTP-REC-H                    13270000
132800         WRITE RBTP-REC-H                                         13280000
132900         GO TO 0050-EXIT.                                         13290000
133000                                                                  13300000
133100     IF  RBTP-PROV-ID EQUAL '#PSC  '                              13310000
133200         MOVE GC-GCTABULR-PSC-VARY-MAX-OCUR TO                    13320000
133300              GBI-ENTRY-COUNT                                     13330000
133400         MOVE 1A-REC-AREA        TO RBTP-REC-I                    13340000
133500         WRITE RBTP-REC-I                                         13350000
133600         GO TO 0050-EXIT.                                         13360000
133700                                                                  13370000
133800     IF  RBTP-PROV-ID EQUAL '#PVE  '                              13380000
133900         MOVE GC-GCTABULR-PVE-VARY-MAX-OCUR TO                    13390000
134000              GBJ-ENTRY-COUNT                                     13400000
134100         MOVE 1A-REC-AREA        TO RBTP-REC-J                    13410000
134200         WRITE RBTP-REC-J                                         13420000
134300         GO TO 0050-EXIT.                                         13430000
134400                                                                  13440000
134500                                                                  13450000
134600**DISPLAY WORK RECORD KEY WHEN ABENDING.                          13460000
134700     DISPLAY  '  '.                                               13470000
134800     DISPLAY  ' GC0010     0050-WRITE-RBTP'.                      13480000
134900                                                                  13490000
135000     IF  WRK-STATUS-CODE   =   'C'                                13500000
135100         DISPLAY  ' WORK-CONTRACT-KEY     =  ' WS-DISPLAY-CON-KEY 13510000
135200     ELSE                                                         13520000
135300     IF  WRK-STATUS-CODE   =   'G'                                13530000
135400         DISPLAY  ' WORK-GROUP-SPEC-KEY   =  ' WS-DISPLAY-GS-KEY  13540000
135500     ELSE                                                         13550000
135600     IF  WRK-STATUS-CODE   =   'S'                                13560000
135700         DISPLAY  ' WORK-SYSTEM-RECORD  =  ' WS-DISPLAY-SYS-KEY   13570000
135800     ELSE                                                         13580000
135900     DISPLAY  ' WORK-RECORD-KEY  =  ' WORK-RECORD-KEY.            13590000
136000                                                                  13600000
136100     DISPLAY ' BENEFIT TABULAR PROVISION =  '  RBTP-PROV-ID.      13610000
136200     MOVE  0050  TO  ABEND-CODE.                                  13620000
136300     GO TO 0900-ERROR-RTN.                                        13630000
136400                                                                  13640000
136500 0050-EXIT.                                                       13650000
136600     EXIT.                                                        13660000
136700/                                                                 13670000
136800 0060-WRITE-RCL.                                                  13680000
136900                                                                  13690000
137000     MOVE GC-GCCONTR-VARY-MAX-OCUR TO                             13700000
137100          GCT-COUNT-BEN-PROVN-POINTERS.                           13710000
137200     MOVE 1A-REC-AREA            TO RCL-REC.                      13720000
137300     IF SKELETON-FROM-IND                                         13730000
137400        MOVE 'S'            TO  WRK3-SIGNAL-BATCH-INTERNAL.       13740000
137500     WRITE RCL-REC.                                               13750000
137600     MOVE SPACES            TO  SKELETON-FROM-SW.                 13760000
137700                                                                  13770000
137800 0060-EXIT.                                                       13780000
137900     EXIT.                                                        13790000
138000/                                                                 13800000
138100 0070-WRITE-RCTP.                                                 13810000
138200                                                                  13820000
138300     MOVE 1A-REC-AREA            TO RCTP-REC.                     13830000
138400                                                                  13840000
138500     MOVE RCTP-PROV-ID           TO TAB-ID-WK.                    13850000
138600                                                                  13860000
138700     IF TAB-ID-WK   =                                             13870000
138800       '#ABM  ' OR '#ACL  '  OR '#ACP  ' OR '#ADL  ' OR '#AOL  '  13880000
138900         PERFORM 0095-WRITE-RACCUMT THRU 0095-EXIT                13890000
139000         GO TO 0070-EXIT.                                         13900000
139100                                                                  13910000
139200     IF  TAB-ID-POS2 EQUAL 'A'                                    13920000
139300         PERFORM 0100-WRITE-RALT THRU 0100-EXIT                   13930000
139400         GO TO 0070-EXIT.                                         13940000
139500                                                                  13950000
139600     IF  TAB-ID-POS2 EQUAL 'I'                                    13960000
139700         PERFORM 0110-WRITE-RALI THRU 0110-EXIT                   13970000
139800         GO TO 0070-EXIT.                                         13980000
139900                                                                  13990000
140000     IF  RCTP-PROV-ID EQUAL '#CLDR '                              14000000
140100         MOVE GC-GCTABULR-CLDR-VARY-MAX-OCUR TO                   14010000
140200              GTB-ENTRY-COUNT                                     14020000
140300         MOVE 1A-REC-AREA        TO RCTP-REC-A                    14030000
140400         WRITE RCTP-REC-A                                         14040000
140500         GO TO 0070-EXIT.                                         14050000
140600                                                                  14060000
140700     IF  RCTP-PROV-ID EQUAL '#CRR  '                              14070000
140800         MOVE GC-GCTABULR-CRR-VARY-MAX-OCUR TO                    14080000
140900              GTC-ENTRY-COUNT                                     14090000
141000         MOVE 1A-REC-AREA        TO RCTP-REC-B                    14100000
141100         WRITE RCTP-REC-B                                         14110000
141200         GO TO 0070-EXIT.                                         14120000
141300                                                                  14130000
141400     IF  RCTP-PROV-ID   =    '#CRS  '                             14140000
141500         MOVE  GC-GCTABULR-CRS-VARY-MAX-OCUR                      14150000
141600           TO  GTD-ENTRY-COUNT                                    14160000
141700         MOVE 1A-REC-AREA    TO  RCTP-REC-C                       14170000
141800         WRITE RCTP-REC-C                                         14180000
141900         GO TO 0070-EXIT.                                         14190000
142000                                                                  14200000
142100**DISPLAY WORK RECORD KEY WHEN ABENDING.                          14210000
142200     DISPLAY  '  '.                                               14220000
142300     DISPLAY  ' GC0010     0070-WRITE-RCTP'.                      14230000
142400                                                                  14240000
142500     IF  WRK-STATUS-CODE   =   'C'                                14250000
142600         DISPLAY  ' WORK-CONTRACT-KEY    =  ' WS-DISPLAY-CON-KEY  14260000
142700     ELSE                                                         14270000
142800     IF  WRK-STATUS-CODE   =   'G'                                14280000
142900         DISPLAY  ' WORK-GROUP-SPEC-KEY  =  ' WS-DISPLAY-GS-KEY   14290000
143000     ELSE                                                         14300000
143100     IF  WRK-STATUS-CODE   =   'S'                                14310000
143200         DISPLAY  ' WORK-SYSTEM-RECORD  =  ' WS-DISPLAY-SYS-KEY   14320000
143300     ELSE                                                         14330000
143400     DISPLAY  ' WORK-RECORD-KEY  =  ' WORK-RECORD-KEY.            14340000
143500                                                                  14350000
143600     DISPLAY ' CONTRACT TABULAR      =  '  RCTP-PROV-ID.          14360000
143700     MOVE  0070  TO ABEND-CODE.                                   14370000
143800     GO TO 0900-ERROR-RTN.                                        14380000
143900                                                                  14390000
144000 0070-EXIT.                                                       14400000
144100     EXIT.                                                        14410000
144200/                                                                 14420000
144300 0071-WRITE-RCDRS.                                                14430000
144400     IF  HOLD-PROV-ID   =    '#CDRS '                             14440000
144500         MOVE  GC-GCTABULR-CDRS-VARY-MAX-OCUR                     14450000
144600           TO  GTE-ENTRY-COUNT                                    14460000
144700         MOVE 1A-REC-AREA    TO  RCDRS-REC-A                      14470000
144800         WRITE RCDRS-REC-A                                        14480000
144900     END-IF                                                       14490000
145000     GO TO 0071-EXIT.                                             14500000
145100                                                                  14510000
145200**DISPLAY WORK RECORD KEY WHEN ABENDING.                          14520000
145300     DISPLAY  '  '.                                               14530000
145400     DISPLAY  ' GC0010     0071-WRITE-RCDRS'.                     14540000
145500                                                                  14550000
145600     IF  WRK-STATUS-CODE   =   'C'                                14560000
145700         DISPLAY  ' WORK-CONTRACT-KEY    =  ' WS-DISPLAY-CON-KEY  14570000
145800     ELSE                                                         14580000
145900     IF  WRK-STATUS-CODE   =   'G'                                14590000
146000         DISPLAY  ' WORK-GROUP-SPEC-KEY  =  ' WS-DISPLAY-GS-KEY   14600000
146100     ELSE                                                         14610000
146200     IF  WRK-STATUS-CODE   =   'S'                                14620000
146300         DISPLAY  ' WORK-SYSTEM-RECORD  =  ' WS-DISPLAY-SYS-KEY   14630000
146400     ELSE                                                         14640000
146500     DISPLAY  ' WORK-RECORD-KEY  =  ' WORK-RECORD-KEY.            14650000
146600                                                                  14660000
146700     DISPLAY ' CONTRACT TABULAR      =  '  RCDRS-PROV-ID.         14670000
146800     MOVE  0071 TO ABEND-CODE.                                    14680000
146900     GO TO 0900-ERROR-RTN.                                        14690000
147000 0071-EXIT.                                                       14700000
147100     EXIT.                                                        14710000
147200/                                                                 14720000
147300 0080-WRITE-RGS.                                                  14730000
147400                                                                  14740000
147500     MOVE GC-GCGRPSPC-VARY-MAX-OCUR TO                            14750000
147600          GCG-COUNT-TAB-PROVN-POINTERS.                           14760000
147700     MOVE 1A-REC-AREA            TO RGS-REC.                      14770000
147800     IF SKELETON-FROM-IND                                         14780000
147900        MOVE 'S'            TO  WRK4-SIGNAL-BATCH-INTERNAL.       14790000
148000     WRITE RGS-REC.                                               14800000
148100     MOVE SPACES            TO  SKELETON-FROM-SW.                 14810000
148200                                                                  14820000
148300 0080-EXIT.                                                       14830000
148400     EXIT.                                                        14840000
148500/                                                                 14850000
148600 0090-WRITE-RGST.                                                 14860000
148700                                                                  14870000
148800     MOVE 1A-REC-AREA            TO RGST-REC.                     14880000
148900                                                                  14890000
149000     MOVE RGST-PROV-ID           TO TAB-ID-WK.                    14900000
149100                                                                  14910000
149200     IF TAB-ID-WK   =                                             14920000
149300       '#ABM  ' OR '#ACL  '  OR '#ACP  ' OR '#ADL  ' OR '#AOL  '  14930000
149400         PERFORM 0095-WRITE-RACCUMT THRU 0095-EXIT                14940000
149500         GO TO 0090-EXIT.                                         14950000
149600                                                                  14960000
149700     IF  TAB-ID-POS2 EQUAL 'A'                                    14970000
149800         PERFORM 0100-WRITE-RALT THRU 0100-EXIT                   14980000
149900         GO TO 0090-EXIT.                                         14990000
150000                                                                  15000000
150100     IF  RGST-PROV-ID EQUAL '#GFG  '                              15010000
150200         MOVE GC-GCTABULR-GFG-VARY-MAX-OCUR TO                    15020000
150300              GSE-ENTRY-COUNT                                     15030000
150400         MOVE 1A-REC-AREA        TO RGST-REC-A                    15040000
150500         WRITE RGST-REC-A                                         15050000
150600         GO TO 0090-EXIT.                                         15060000
150700                                                                  15070000
150800*    IF  RGST-PROV-ID EQUAL '#     '                              15080000
150900*        MOVE 1A-REC-AREA        TO RGST-REC-B                    15090000
151000*        WRITE RGST-REC-B                                         15100000
151100*        GO TO 0090-EXIT.                                         15110000
151200                                                                  15120000
151300     IF  RGST-PROV-ID EQUAL '#GHOR '                              15130000
151400         MOVE GC-GCTABULR-GHOR-VARY-MAX-OCUR TO                   15140000
151500              GSO-ENTRY-COUNT                                     15150000
151600         MOVE 1A-REC-AREA        TO RGST-REC-C                    15160000
151700         WRITE RGST-REC-C                                         15170000
151800         GO TO 0090-EXIT.                                         15180000
151900                                                                  15190000
152000     IF  RGST-PROV-ID EQUAL '#GMOR '                              15200000
152100         MOVE GC-GCTABULR-GMOR-VARY-MAX-OCUR TO                   15210000
152200              GSN-ENTRY-COUNT                                     15220000
152300         MOVE 1A-REC-AREA        TO RGST-REC-D                    15230000
152400         WRITE RGST-REC-D                                         15240000
152500         GO TO 0090-EXIT.                                         15250000
152600                                                                  15260000
152700     IF  RGST-PROV-ID EQUAL '#GMSR '                              15270000
152800         MOVE GC-GCTABULR-GMSR-VARY-MAX-OCUR TO                   15280000
152900              GSM-ENTRY-COUNT                                     15290000
153000         MOVE 1A-REC-AREA        TO RGST-REC-E                    15300000
153100         WRITE RGST-REC-E                                         15310000
153200         GO TO 0090-EXIT.                                         15320000
153300                                                                  15330000
153400     IF  RGST-PROV-ID EQUAL '#GMSC '                              15340000
153500         MOVE GC-GCTABULR-GMSC-VARY-MAX-OCUR TO                   15350000
153600              GSL-ENTRY-COUNT                                     15360000
153700         MOVE 1A-REC-AREA        TO RGST-REC-F                    15370000
153800         WRITE RGST-REC-F                                         15380000
153900         GO TO 0090-EXIT.                                         15390000
154000                                                                  15400000
154100     IF  RGST-PROV-ID EQUAL '#GPAR '                              15410000
154200         MOVE GC-GCTABULR-GPAR-VARY-MAX-OCUR TO                   15420000
154300              GSK-ENTRY-COUNT                                     15430000
154400         MOVE 1A-REC-AREA        TO RGST-REC-G                    15440000
154500         WRITE RGST-REC-G                                         15450000
154600         GO TO 0090-EXIT.                                         15460000
154700                                                                  15470000
154800     IF  RGST-PROV-ID EQUAL '#GPAC '                              15480000
154900         MOVE GC-GCTABULR-GPAC-VARY-MAX-OCUR TO                   15490000
155000              GSJ-ENTRY-COUNT                                     15500000
155100         MOVE 1A-REC-AREA        TO RGST-REC-H                    15510000
155200         WRITE RGST-REC-H                                         15520000
155300         GO TO 0090-EXIT.                                         15530000
155400                                                                  15540000
155500     IF  RGST-PROV-ID EQUAL '#GVLF '                              15550000
155600         MOVE GC-GCTABULR-GVLF-VARY-MAX-OCUR TO                   15560000
155700              GSX-ENTRY-COUNT                                     15570000
155800         MOVE 1A-REC-AREA        TO RGST-REC-I                    15580000
155900         WRITE RGST-REC-I                                         15590000
156000         GO TO 0090-EXIT.                                         15600000
156100                                                                  15610000
156200     IF  RGST-PROV-ID EQUAL '#GVLG '                              15620000
156300         MOVE GC-GCTABULR-GVLG-VARY-MAX-OCUR TO                   15630000
156400              GSF-ENTRY-COUNT                                     15640000
156500         MOVE 1A-REC-AREA        TO RGST-REC-L                    15650000
156600         WRITE RGST-REC-L                                         15660000
156700         GO TO 0090-EXIT.                                         15670000
156800                                                                  15680000
156900     IF  RGST-PROV-ID EQUAL '#GVLH '                              15690000
157000         MOVE GC-GCTABULR-GVLH-VARY-MAX-OCUR TO                   15700000
157100              GSQ-ENTRY-COUNT                                     15710000
157200         MOVE 1A-REC-AREA        TO RGST-REC-M                    15720000
157300         WRITE RGST-REC-M                                         15730000
157400         GO TO 0090-EXIT.                                         15740000
157500                                                                  15750000
157600     IF  RGST-PROV-ID EQUAL '#GVLP '                              15760000
157700         MOVE GC-GCTABULR-GVLP-VARY-MAX-OCUR TO                   15770000
157800              GSY-ENTRY-COUNT                                     15780000
157900         MOVE 1A-REC-AREA        TO RGST-REC-J                    15790000
158000         WRITE RGST-REC-J                                         15800000
158100         GO TO 0090-EXIT.                                         15810000
158200                                                                  15820000
158300     IF  RGST-PROV-ID EQUAL '#GWCD '                              15830000
158400         MOVE GC-GCTABULR-GWCD-VARY-MAX-OCUR TO                   15840000
158500              GSI-ENTRY-COUNT                                     15850000
158600         MOVE 1A-REC-AREA        TO RGST-REC-K                    15860000
158700         WRITE RGST-REC-K                                         15870000
158800         GO TO 0090-EXIT.                                         15880000
158900                                                                  15890000
159000     IF  RGST-PROV-ID EQUAL '#GMPR '                              15900000
159100         MOVE GC-GCTABULR-GMPR-VARY-MAX-OCUR TO                   15910000
159200              GSR-ENTRY-COUNT                                     15920000
159300         MOVE 1A-REC-AREA        TO RGST-REC-N                    15930000
159400         WRITE RGST-REC-N                                         15940000
159500         GO TO 0090-EXIT.                                         15950000
159600                                                                  15960000
159700     IF  RGST-PROV-ID EQUAL '#GCCP '                              15970000
159800         MOVE GC-GCTABULR-GCCP-VARY-MAX-OCUR TO                   15980000
159900              GSS-ENTRY-COUNT                                     15990000
160000         MOVE 1A-REC-AREA        TO RGST-REC-O                    16000000
160100         WRITE RGST-REC-O                                         16010000
160200         GO TO 0090-EXIT.                                         16020000
160300                                                                  16030000
160400     IF  RGST-PROV-ID EQUAL '#GFSB '                              16040000
160500         MOVE GC-GCTABULR-GFSB-VARY-MAX-OCUR TO                   16050000
160600              GSU-ENTRY-COUNT                                     16060000
160700         MOVE 1A-REC-AREA        TO RGST-REC-P                    16070000
160800         WRITE RGST-REC-P                                         16080000
160900         GO TO 0090-EXIT.                                         16090000
161000                                                                  16100000
161100     IF  RGST-PROV-ID EQUAL '#GMDB '                              16110000
161200         MOVE GC-GCTABULR-GMDB-VARY-MAX-OCUR TO                   16120000
161300              GSV-ENTRY-COUNT                                     16130000
161400         MOVE 1A-REC-AREA        TO RGST-REC-Q                    16140000
161500         WRITE RGST-REC-Q                                         16150000
161600         GO TO 0090-EXIT.                                         16160000
161700                                                                  16170000
161800     IF  RGST-PROV-ID EQUAL '#GMDN '                              16180000
161900         MOVE GC-GCTABULR-GMDN-VARY-MAX-OCUR TO                   16190000
162000              GS3-ENTRY-COUNT                                     16200000
162100         MOVE 1A-REC-AREA        TO RGST-REC-R                    16210000
162200         WRITE RGST-REC-R                                         16220000
162300         GO TO 0090-EXIT.                                         16230000
162400                                                                  16240000
162500     IF  RGST-PROV-ID EQUAL '#GMOB '                              16250000
162600         MOVE GC-GCTABULR-GMOB-VARY-MAX-OCUR TO                   16260000
162700              GSG-ENTRY-COUNT                                     16270000
162800         MOVE 1A-REC-AREA        TO RGST-REC-S                    16280000
162900         WRITE RGST-REC-S                                         16290000
163000         GO TO 0090-EXIT.                                         16300000
163100                                                                  16310000
163200     IF  RGST-PROV-ID EQUAL '#GMPB '                              16320000
163300         MOVE GC-GCTABULR-GMPB-VARY-MAX-OCUR TO                   16330000
163400              GSH-ENTRY-COUNT                                     16340000
163500         MOVE 1A-REC-AREA        TO RGST-REC-T                    16350000
163600         WRITE RGST-REC-T                                         16360000
163700         GO TO 0090-EXIT.                                         16370000
163800                                                                  16380000
163900     IF  RGST-PROV-ID EQUAL '#GPAB '                              16390000
164000         MOVE GC-GCTABULR-GPAB-VARY-MAX-OCUR TO                   16400000
164100              GST-ENTRY-COUNT                                     16410000
164200         MOVE 1A-REC-AREA        TO RGST-REC-U                    16420000
164300         WRITE RGST-REC-U                                         16430000
164400         GO TO 0090-EXIT.                                         16440000
164500                                                                  16450000
164600     IF  RGST-PROV-ID EQUAL '#GHOB '                              16460000
164700         MOVE GC-GCTABULR-GHOB-VARY-MAX-OCUR TO                   16470000
164800              GSA-ENTRY-COUNT                                     16480000
164900         MOVE 1A-REC-AREA        TO RGST-REC-V                    16490000
165000         WRITE RGST-REC-V                                         16500000
165100         GO TO 0090-EXIT.                                         16510000
165200                                                                  16520000
165300     IF  RGST-PROV-ID EQUAL '#GMSB '                              16530000
165400         MOVE GC-GCTABULR-GMSB-VARY-MAX-OCUR TO                   16540000
165500              GSB-ENTRY-COUNT                                     16550000
165600         MOVE 1A-REC-AREA        TO RGST-REC-W                    16560000
165700         WRITE RGST-REC-W                                         16570000
165800         GO TO 0090-EXIT.                                         16580000
165900                                                                  16590000
166000     IF  RGST-PROV-ID EQUAL '#GPAD '                              16600000
166100         MOVE GC-GCTABULR-GPAD-VARY-MAX-OCUR TO                   16610000
166200              GSC-ENTRY-COUNT                                     16620000
166300         MOVE 1A-REC-AREA        TO RGST-REC-X                    16630000
166400         WRITE RGST-REC-X                                         16640000
166500         GO TO 0090-EXIT.                                         16650000
166600                                                                  16660000
166700     IF  RGST-PROV-ID EQUAL '#GRID '                              16670000
166800         MOVE GC-GCTABULR-GRID-VARY-MAX-OCUR TO                   16680000
166900              GSD-ENTRY-COUNT                                     16690000
167000         MOVE 1A-REC-AREA        TO RGST-REC-Y                    16700000
167100         WRITE RGST-REC-Y                                         16710000
167200         GO TO 0090-EXIT.                                         16720000
167300                                                                  16730000
167400     IF  RGST-PROV-ID EQUAL '#GPPO '                              16740000
167500         MOVE GC-GCTABULR-GPPO-VARY-MAX-OCUR TO                   16750000
167600              GSW-ENTRY-COUNT                                     16760000
167700         MOVE 1A-REC-AREA        TO RGST-REC-Z                    16770000
167800         WRITE RGST-REC-Z                                         16780000
167900         GO TO 0090-EXIT.                                         16790000
168000                                                                  16800000
168100     IF  RGST-PROV-ID EQUAL '#GVLQ '                              16810000
168200         MOVE GC-GCTABULR-GVLQ-VARY-MAX-OCUR TO                   16820000
168300              GSP-ENTRY-COUNT                                     16830000
168400         MOVE 1A-REC-AREA        TO RGST-REC-AA                   16840000
168500         WRITE RGST-REC-AA                                        16850000
168600         GO TO 0090-EXIT.                                         16860000
168700                                                                  16870000
168800     IF  RGST-PROV-ID EQUAL '#GVLR '                              16880000
168900         MOVE GC-GCTABULR-GVLR-VARY-MAX-OCUR TO                   16890000
169000              GSZ-ENTRY-COUNT                                     16900000
169100         MOVE 1A-REC-AREA        TO RGST-REC-AB                   16910000
169200         WRITE RGST-REC-AB                                        16920000
169300         GO TO 0090-EXIT.                                         16930000
169400                                                                  16940000
169500     IF  RGST-PROV-ID EQUAL '#GMCG '                              16950000
169600         MOVE GC-GCTABULR-GMCG-VARY-MAX-OCUR TO                   16960000
169700              GS1-ENTRY-COUNT                                     16970000
169800         MOVE 1A-REC-AREA        TO RGST-REC-AC                   16980000
169900         WRITE RGST-REC-AC                                        16990000
170000         GO TO 0090-EXIT.                                         17000000
170100                                                                  17010000
170200     IF  RGST-PROV-ID EQUAL '#GMCD '                              17020000
170300         MOVE GC-GCTABULR-GMCD-VARY-MAX-OCUR TO                   17030000
170400              GS2-ENTRY-COUNT                                     17040000
170500         MOVE 1A-REC-AREA        TO RGST-REC-AD                   17050000
170600         WRITE RGST-REC-AD                                        17060000
170700         GO TO 0090-EXIT.                                         17070000
170800                                                                  17080000
170900     IF  RGST-PROV-ID EQUAL '#GMCR '                              17090000
171000         MOVE GC-GCTABULR-GMCR-VARY-MAX-OCUR TO                   17100000
171100              GS4-ENTRY-COUNT                                     17110000
171200         MOVE 1A-REC-AREA        TO RGST-REC-AE                   17120000
171300         WRITE RGST-REC-AE                                        17130000
171400         GO TO 0090-EXIT.                                         17140000
171500                                                                  17150000
171600     IF  RGST-PROV-ID EQUAL '#GMCT '                              17160000
171700         MOVE GC-GCTABULR-GMCT-VARY-MAX-OCUR TO                   17170000
171800              GS5-ENTRY-COUNT                                     17180000
171900         MOVE 1A-REC-AREA        TO RGST-REC-AF                   17190000
172000         WRITE RGST-REC-AF                                        17200000
172100         GO TO 0090-EXIT.                                         17210000
172200                                                                  17220000
172300     IF  RGST-PROV-ID EQUAL '#GMCS '                              17230000
172400         MOVE GC-GCTABULR-GMCS-VARY-MAX-OCUR TO                   17240000
172500              GS6-ENTRY-COUNT                                     17250000
172600         MOVE 1A-REC-AREA        TO RGST-REC-AG                   17260000
172700         WRITE RGST-REC-AG                                        17270000
172800         GO TO 0090-EXIT.                                         17280000
172900                                                                  17290000
173000     IF  RGST-PROV-ID EQUAL '#GRPO '                              17300000
173100         MOVE GC-GCTABULR-GRPO-VARY-MAX-OCUR TO                   17310000
173200              GS7-ENTRY-COUNT                                     17320000
173300         MOVE 1A-REC-AREA        TO RGST-REC-AH                   17330000
173400         WRITE RGST-REC-AH                                        17340000
173500         GO TO 0090-EXIT.                                         17350000
173600                                                                  17360000
173700     IF  RGST-PROV-ID EQUAL '#GCPO '                              17370000
173800         MOVE GC-GCTABULR-GCPO-VARY-MAX-OCUR TO                   17380000
173900              GS8-ENTRY-COUNT                                     17390000
174000         MOVE 1A-REC-AREA        TO RGST-REC-AI                   17400000
174100         WRITE RGST-REC-AI                                        17410000
174200         GO TO 0090-EXIT.                                         17420000
174300                                                                  17430000
174400     IF  RGST-PROV-ID EQUAL '#GCBL '                              17440000
174500         MOVE GC-GCTABULR-GCPO-VARY-MAX-OCUR TO                   17450000
174600              GS9-ENTRY-COUNT                                     17460000
174700         MOVE 1A-REC-AREA        TO RGST-REC-AJ                   17470000
174800         WRITE RGST-REC-AJ                                        17480000
174900         GO TO 0090-EXIT.                                         17490000
175000                                                                  17500000
175100     IF  RGST-PROV-ID EQUAL '#GPAN '                              17510000
175200         MOVE GC-GCTABULR-GCPO-VARY-MAX-OCUR TO                   17520000
175300              GS10-ENTRY-COUNT                                    17530000
175400         MOVE 1A-REC-AREA        TO RGST-REC-AK                   17540000
175500         WRITE RGST-REC-AK                                        17550000
175600         GO TO 0090-EXIT.                                         17560000
175700                                                                  17570000
175800     IF  RGST-PROV-ID EQUAL '#GSUB '                              17580000
175900         MOVE 1A-REC-AREA        TO RGST-REC-AL                   17590000
176000         WRITE RGST-REC-AL                                        17600000
176100         GO TO 0090-EXIT.                                         17610000
176200                                                                  17620000
176300     IF  RGST-PROV-ID EQUAL '#GBAE '                              17630000
176400         MOVE GC-GCTABULR-GBAE-VARY-MAX-OCUR TO                   17640000
176500              GS16-ENTRY-COUNT                                    17650000
176600         MOVE 1A-REC-AREA        TO RGST-REC-AM                   17660000
176700         WRITE RGST-REC-AM                                        17670000
176800         GO TO 0090-EXIT.                                         17680000
176900                                                                  17690000
177000     IF  RGST-PROV-ID EQUAL '#GFHC '                              17700000
177100         MOVE GC-GCTABULR-GFHC-VARY-MAX-OCUR TO                   17710000
177200              GFHC-ENTRY-COUNT                                    17720000
177300         MOVE 1A-REC-AREA        TO RGST-REC-AN                   17730000
177400         WRITE RGST-REC-AN                                        17740000
177500         GO TO 0090-EXIT.                                         17750000
177600                                                                  17760000
177700     IF  RGST-PROV-ID EQUAL '#GFSA '                              17770000
177800         MOVE GC-GCTABULR-GFSA-VARY-MAX-OCUR TO                   17780000
177900              GFSA-ENTRY-COUNT                                    17790000
178000         MOVE 1A-REC-AREA        TO RGST-REC-AO                   17800000
178100         WRITE RGST-REC-AO                                        17810000
178200         GO TO 0090-EXIT.                                         17820000
178300                                                                  17830000
178400     IF  RGST-PROV-ID EQUAL '#GHCA '                              17840000
178500         MOVE GC-GCTABULR-GHCA-VARY-MAX-OCUR TO                   17850000
178600              GHCA-ENTRY-COUNT                                    17860000
178700         MOVE 1A-REC-AREA        TO RGST-REC-AP                   17870000
178800         WRITE RGST-REC-AP                                        17880000
178900         GO TO 0090-EXIT.                                         17890000
179000                                                                  17900000
179100     IF  RGST-PROV-ID EQUAL '#GHSA '                              17910000
179200         MOVE GC-GCTABULR-GHSA-VARY-MAX-OCUR TO                   17920000
179300              GHSA-ENTRY-COUNT                                    17930000
179400         MOVE 1A-REC-AREA        TO RGST-REC-AQ                   17940000
179500         WRITE RGST-REC-AQ                                        17950000
179600         GO TO 0090-EXIT.                                         17960000
179700                                                                  17970000
179800     IF  RGST-PROV-ID EQUAL '#GLPF '                              17980000
179900         MOVE GC-GCTABULR-GLPF-VARY-MAX-OCUR TO                   17990000
180000              GLPF-ENTRY-COUNT                                    18000000
180100         MOVE 1A-REC-AREA        TO RGST-REC-AR                   18010000
180200         WRITE RGST-REC-AR                                        18020000
180300         GO TO 0090-EXIT.                                         18030000
180400                                                                  18040000
180500     IF  RGST-PROV-ID EQUAL '#GLPH '                              18050000
180600         MOVE GC-GCTABULR-GLPH-VARY-MAX-OCUR TO                   18060000
180700              GLPH-ENTRY-COUNT                                    18070000
180800         MOVE 1A-REC-AREA        TO RGST-REC-AS                   18080000
180900         WRITE RGST-REC-AS                                        18090000
181000         GO TO 0090-EXIT.                                         18100000
181100                                                                  18110000
181200     IF  RGST-PROV-ID EQUAL '#GWHC '                              18120000
181300         MOVE GC-GCTABULR-GWHC-VARY-MAX-OCUR TO                   18130000
181400              GWHC-ENTRY-COUNT                                    18140000
181500         MOVE 1A-REC-AREA        TO RGST-REC-AT                   18150000
181600         WRITE RGST-REC-AT                                        18160000
181700         GO TO 0090-EXIT.                                         18170000
181800                                                                  18180000
181900     IF  RGST-PROV-ID EQUAL '#GMFH '                              18190000
182000         MOVE GC-GCTABULR-GMFH-VARY-MAX-OCUR TO                   18200000
182100              GMFH-ENTRY-COUNT                                    18210000
182200         MOVE 1A-REC-AREA        TO RGST-REC-AU                   18220000
182300         WRITE RGST-REC-AU                                        18230000
182400         GO TO 0090-EXIT.                                         18240000
182500                                                                  18250000
TM0526     IF  RGST-PROV-ID EQUAL '#GHPA '                              18240100
TM0526         MOVE GC-GCTABULR-GHPA-VARY-MAX-OCUR TO                   18240200
TM0526              GHPA-ENTRY-COUNT                                    18240300
TM0526         MOVE 1A-REC-AREA        TO RGST-REC-AV                   18240400
TM0526         WRITE RGST-REC-AV                                        18240500
TM0526         GO TO 0090-EXIT.                                         18240600
182500                                                                  18250000
182600**DISPLAY WORK RECORD KEY WHEN ABENDING.                          18260000
182700      DISPLAY  '  '.                                              18270000
182800      DISPLAY  ' GC0010     0090-WRITE-RGST'.                     18280000
182900                                                                  18290000
183000     IF  WRK-STATUS-CODE   =   'C'                                18300000
183100         DISPLAY  ' WORK-CONTRACT-KEY    =  ' WS-DISPLAY-CON-KEY  18310000
183200     ELSE                                                         18320000
183300     IF  WRK-STATUS-CODE   =   'G'                                18330000
183400         DISPLAY  ' WORK-GROUP-SPEC-KEY   =  ' WS-DISPLAY-GS-KEY  18340000
183500     ELSE                                                         18350000
183600     IF  WRK-STATUS-CODE   =   'S'                                18360000
183700         DISPLAY  ' WORK-SYSTEM-RECORD  =  ' WS-DISPLAY-SYS-KEY   18370000
183800     ELSE                                                         18380000
183900     DISPLAY  ' WORK-RECORD-KEY  =  ' WORK-RECORD-KEY.            18390000
184000                                                                  18400000
184100     DISPLAY ' GROUP SPECIFIC TABULAR =  ' RGST-PROV-ID.          18410000
184200     MOVE  0090  TO ABEND-CODE.                                   18420000
184300     GO TO 0900-ERROR-RTN.                                        18430000
184400                                                                  18440000
184500 0090-EXIT.                                                       18450000
184600     EXIT.                                                        18460000
184700/                                                                 18470000
184800 0095-WRITE-RACCUMT.                                              18480000
184900                                                                  18490000
185000     MOVE 1A-REC-AREA            TO RACCUMT-REC.                  18500000
185100                                                                  18510000
185200     IF  RACCUMT-PROV-ID EQUAL '#ABM  '                           18520000
185300         MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR TO                    18530000
185400              GAA-ENTRY-COUNT                                     18540000
185500         MOVE 1A-REC-AREA        TO RACCUMT-REC-A                 18550000
185600         WRITE RACCUMT-REC-A                                      18560000
185700         GO TO 0095-EXIT.                                         18570000
185800                                                                  18580000
185900     IF  RACCUMT-PROV-ID EQUAL '#ACL  '                           18590000
186000         MOVE GC-GCTABULR-ACL-VARY-MAX-OCUR TO                    18600000
186100              GAB-ENTRY-COUNT                                     18610000
186200         MOVE 1A-REC-AREA        TO RACCUMT-REC-B                 18620000
186300         WRITE RACCUMT-REC-B                                      18630000
186400         GO TO 0095-EXIT.                                         18640000
186500                                                                  18650000
186600     IF  RACCUMT-PROV-ID   =   '#ACP  '                           18660000
186700         MOVE  GC-GCTABULR-ACP-VARY-MAX-OCUR                      18670000
186800           TO  GAF-ENTRY-COUNT                                    18680000
186900         MOVE 1A-REC-AREA     TO  RACCUMT-REC-C                   18690000
187000         WRITE RACCUMT-REC-C                                      18700000
187100         GO TO 0095-EXIT.                                         18710000
187200                                                                  18720000
187300     IF  RACCUMT-PROV-ID EQUAL '#ADL  '                           18730000
187400         MOVE GC-GCTABULR-ADL-VARY-MAX-OCUR TO                    18740000
187500              GAC-ENTRY-COUNT                                     18750000
187600         MOVE 1A-REC-AREA        TO RACCUMT-REC-D                 18760000
187700         WRITE RACCUMT-REC-D                                      18770000
187800         GO TO 0095-EXIT.                                         18780000
187900                                                                  18790000
188000     IF  RACCUMT-PROV-ID EQUAL '#AOL  '                           18800000
188100         MOVE GC-GCTABULR-AOL-VARY-MAX-OCUR TO                    18810000
188200              GAD-ENTRY-COUNT                                     18820000
188300         MOVE 1A-REC-AREA        TO RACCUMT-REC-E                 18830000
188400         WRITE RACCUMT-REC-E                                      18840000
188500         GO TO 0095-EXIT.                                         18850000
188600                                                                  18860000
188700 0095-EXIT.                                                       18870000
188800     EXIT.                                                        18880000
188900/                                                                 18890000
189000 0100-WRITE-RALT.                                                 18900000
189100                                                                  18910000
189200     MOVE 1A-REC-AREA            TO RALT-REC.                     18920000
189300                                                                  18930000
189400     IF  RALT-PROV-ID EQUAL '#AAR  '                              18940000
189500         MOVE GC-GCTABULR-AAR-VARY-MAX-OCUR TO                    18950000
189600              GAE-ENTRY-COUNT                                     18960000
189700         MOVE 1A-REC-AREA        TO RALT-REC-E                    18970000
189800         WRITE RALT-REC-E                                         18980000
189900         GO TO 0100-EXIT.                                         18990000
190000                                                                  19000000
190100     IF  RALT-PROV-ID EQUAL '#ACON '                              19010000
190200         MOVE GC-GCTABULR-ACON-VARY-MAX-OCUR TO                   19020000
190300              GAI-ENTRY-COUNT                                     19030000
190400         MOVE 1A-REC-AREA        TO RALT-REC-F                    19040000
190500         WRITE RALT-REC-F                                         19050000
190600         GO TO 0100-EXIT.                                         19060000
190700                                                                  19070000
190800     IF  RALT-PROV-ID EQUAL '#ACOS '                              19080000
190900         MOVE GC-GCTABULR-ACOS-VARY-MAX-OCUR TO                   19090000
191000              GAJ-ENTRY-COUNT                                     19100000
191100         MOVE 1A-REC-AREA        TO RALT-REC-G                    19110000
191200         WRITE RALT-REC-G                                         19120000
191300         GO TO 0100-EXIT.                                         19130000
191400                                                                  19140000
191500     IF  RALT-PROV-ID EQUAL '#ADIP '                              19150000
191600         MOVE GC-GCTABULR-ADIP-VARY-MAX-OCUR TO                   19160000
191700              GAG-ENTRY-COUNT                                     19170000
191800         MOVE 1A-REC-AREA        TO RALT-REC-H                    19180000
191900         WRITE RALT-REC-H                                         19190000
192000         GO TO 0100-EXIT.                                         19200000
192100                                                                  19210000
192200     IF  RALT-PROV-ID EQUAL '#ADOP '                              19220000
192300         MOVE GC-GCTABULR-ADOP-VARY-MAX-OCUR TO                   19230000
192400              GAH-ENTRY-COUNT                                     19240000
192500         MOVE 1A-REC-AREA        TO RALT-REC-I                    19250000
192600         WRITE RALT-REC-I                                         19260000
192700         GO TO 0100-EXIT.                                         19270000
192800                                                                  19280000
192900                                                                  19290000
193000**DISPLAY WORK RECORD KEY WHEN ABENDING.                          19300000
193100     DISPLAY  '  '.                                               19310000
193200     DISPLAY  ' GC0010     0100-WRITE-RALT'.                      19320000
193300                                                                  19330000
193400     IF  WRK-STATUS-CODE   =   'C'                                19340000
193500         DISPLAY  ' WORK-CONTRACT-KEY    =  ' WS-DISPLAY-CON-KEY  19350000
193600     ELSE                                                         19360000
193700     IF  WRK-STATUS-CODE   =   'G'                                19370000
193800         DISPLAY  ' WORK-GROUP-SPEC-KEY  =  ' WS-DISPLAY-GS-KEY   19380000
193900     ELSE                                                         19390000
194000     IF  WRK-STATUS-CODE   =   'S'                                19400000
194100         DISPLAY  ' WORK-SYSTEM-RECORD  =  ' WS-DISPLAY-SYS-KEY   19410000
194200     ELSE                                                         19420000
194300     DISPLAY  ' WORK-RECORD-KEY  =  ' WORK-RECORD-KEY.            19430000
194400                                                                  19440000
194500     DISPLAY ' ALL LEVEL TABULAR     =  ' RALT-PROV-ID.           19450000
194600     MOVE  0100  TO ABEND-CODE.                                   19460000
194700     GO TO 0900-ERROR-RTN.                                        19470000
194800                                                                  19480000
194900 0100-EXIT.                                                       19490000
195000     EXIT.                                                        19500000
195100/                                                                 19510000
195200 0110-WRITE-RALI.                                                 19520000
195300                                                                  19530000
195400     MOVE 1A-REC-AREA            TO RALI-REC.                     19540000
195500                                                                  19550000
195600     IF  RALI-PROV-ID EQUAL '#IBGR '                              19560000
195700         MOVE GC-GCTABULR-IBGR-VARY-MAX-OCUR TO                   19570000
195800              GX1-ENTRY-COUNT                                     19580000
195900         MOVE 1A-REC-AREA        TO RALI-REC-A                    19590000
196000         WRITE RALI-REC-A                                         19600000
196100         GO TO 0110-EXIT.                                         19610000
196200                                                                  19620000
196300     IF  RALI-PROV-ID EQUAL '#IPGN '                              19630000
196400         MOVE GC-GCTABULR-IPGN-VARY-MAX-OCUR TO                   19640000
196500              GX2-ENTRY-COUNT                                     19650000
196600         MOVE 1A-REC-AREA        TO RALI-REC-B                    19660000
196700         WRITE RALI-REC-B                                         19670000
196800         GO TO 0110-EXIT.                                         19680000
196900                                                                  19690000
197000     IF  RALI-PROV-ID EQUAL '#IPGT '                              19700000
197100         MOVE GC-GCTABULR-IPGT-VARY-MAX-OCUR TO                   19710000
197200              GX3-ENTRY-COUNT                                     19720000
197300         MOVE 1A-REC-AREA        TO RALI-REC-C                    19730000
197400         WRITE RALI-REC-C                                         19740000
197500         GO TO 0110-EXIT.                                         19750000
197600                                                                  19760000
197700     IF  RALI-PROV-ID EQUAL '#IDGD '                              19770000
197800         MOVE GC-GCTABULR-IDGD-VARY-MAX-OCUR TO                   19780000
197900              GX9-ENTRY-COUNT                                     19790000
198000         MOVE 1A-REC-AREA        TO RALI-REC-D                    19800000
198100         WRITE RALI-REC-D                                         19810000
198200         GO TO 0110-EXIT.                                         19820000
198300                                                                  19830000
198400     IF  RALI-PROV-ID EQUAL '#IPGP '                              19840000
198500         MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR TO                   19850000
198600              GXA-ENTRY-COUNT                                     19860000
198700         MOVE 1A-REC-AREA        TO RALI-REC-E                    19870000
198800         WRITE RALI-REC-E                                         19880000
198900         GO TO 0110-EXIT.                                         19890000
199000                                                                  19900000
199100     IF  RALI-PROV-ID EQUAL '#IPGS '                              19910000
199200         MOVE GC-GCTABULR-IPGS-VARY-MAX-OCUR TO                   19920000
199300              GXS-ENTRY-COUNT                                     19930000
199400         MOVE 1A-REC-AREA        TO RALI-REC-F                    19940000
199500         WRITE RALI-REC-F                                         19950000
199600         GO TO 0110-EXIT.                                         19960000
199700                                                                  19970000
199800     IF  RALI-PROV-ID EQUAL '#IRDX '                              19980000
199900         MOVE GC-GCTABULR-IRDX-VARY-MAX-OCUR TO                   19990000
200000              GXG-ENTRY-COUNT                                     20000000
200100         MOVE 1A-REC-AREA        TO RALI-REC-G                    20010000
200200         WRITE RALI-REC-G                                         20020000
200300         GO TO 0110-EXIT.                                         20030000
200400                                                                  20040000
200500     IF  RALI-PROV-ID EQUAL '#IRIC '                              20050000
200600         MOVE GC-GCTABULR-IRIC-VARY-MAX-OCUR TO                   20060000
200700              GXH-ENTRY-COUNT                                     20070000
200800         MOVE 1A-REC-AREA        TO RALI-REC-H                    20080000
200900         WRITE RALI-REC-H                                         20090000
201000         GO TO 0110-EXIT.                                         20100000
201100                                                                  20110000
201200     IF  RALI-PROV-ID EQUAL '#IRPR '                              20120000
201300         MOVE GC-GCTABULR-IRPR-VARY-MAX-OCUR TO                   20130000
201400              GXI-ENTRY-COUNT                                     20140000
201500         MOVE 1A-REC-AREA        TO RALI-REC-I                    20150000
201600         WRITE RALI-REC-I                                         20160000
201700         GO TO 0110-EXIT.                                         20170000
201800                                                                  20180000
201900     IF  RALI-PROV-ID EQUAL '#IRPV '                              20190000
202000         MOVE GC-GCTABULR-IRPV-VARY-MAX-OCUR TO                   20200000
202100              GXJ-ENTRY-COUNT                                     20210000
202200         MOVE 1A-REC-AREA        TO RALI-REC-J                    20220000
202300         WRITE RALI-REC-J                                         20230000
202400         GO TO 0110-EXIT.                                         20240000
202500                                                                  20250000
202600                                                                  20260000
202700**DISPLAY WORK RECORD KEY WHEN ABENDING.                          20270000
202800     DISPLAY  '  '.                                               20280000
202900     DISPLAY  ' GC0010     0110-WRITE-RALI'.                      20290000
203000                                                                  20300000
203100     IF  WRK-STATUS-CODE   =   'C'                                20310000
203200         DISPLAY  ' WORK-CONTRACT-KEY    =  ' WS-DISPLAY-CON-KEY  20320000
203300     ELSE                                                         20330000
203400     IF  WRK-STATUS-CODE   =   'G'                                20340000
203500         DISPLAY  ' WORK-GROUP-SPEC-KEY  =  ' WS-DISPLAY-GS-KEY   20350000
203600     ELSE                                                         20360000
203700     IF  WRK-STATUS-CODE   =   'S'                                20370000
203800         DISPLAY  ' WORK-SYSTEM-RECORD  =  ' WS-DISPLAY-SYS-KEY   20380000
203900     ELSE                                                         20390000
204000     DISPLAY  ' WORK-RECORD-KEY  =  ' WORK-RECORD-KEY.            20400000
204100                                                                  20410000
204200     DISPLAY ' INTERNAL TABULAR      =  '  RALI-PROV-ID.          20420000
204300     MOVE  0110  TO ABEND-CODE.                                   20430000
204400     GO TO 0900-ERROR-RTN.                                        20440000
204500                                                                  20450000
204600 0110-EXIT.                                                       20460000
204700     EXIT.                                                        20470000
204800/                                                                 20480000
204900******************************************************************20490000
205000***                                                            ***20500000
205100***      'C9' AUDIT WORK RECORD HAS A SEPARATE FILE.           ***20510000
205200***                                                            ***20520000
205300******************************************************************20530000
205400 0120-WRITE-C9-AUDIT-WORK-REC.                                    20540000
205500                                                                  20550000
205600     MOVE GC-WORKFILE-AUDIT-VAR-MAX-OCUR                          20560000
205700     TO   GCAUD-TABLE-FLDS-OCCURS-CNT.                            20570000
205800     MOVE 1A-REC-AREA  TO  R-CON-C9-AUDIT-WORK-RECORD.            20580000
205900     WRITE R-CON-C9-AUDIT-WORK-RECORD.                            20590000
206000                                                                  20600000
206100 0120-EXIT.                                                       20610000
206200     EXIT.                                                        20620000
206300/                                                                 20630000
206400******************************************************************20640000
206500***                                                            ***20650000
206600***      'G9' AUDIT WORK RECORD HAS A SEPARATE FILE.           ***20660000
206700***                                                            ***20670000
206800******************************************************************20680000
206900 0130-WRITE-G9-AUDIT-WORK-REC.                                    20690000
207000                                                                  20700000
207100     MOVE GC-WORKFILE-AUDIT-VAR-MAX-OCUR                          20710000
207200     TO   GCAUD2-TABLE-FLDS-OCCURS-CNT.                           20720000
207300     MOVE 1A-REC-AREA   TO  R-GS-G9-AUDIT-WORK-RECORD.            20730000
207400     WRITE R-GS-G9-AUDIT-WORK-RECORD.                             20740000
207500                                                                  20750000
207600 0130-EXIT.                                                       20760000
207700     EXIT.                                                        20770000
207800/                                                                 20780000
207900 0150-BUILD-TABLE.                                                20790000
208000                                                                  20800000
208100      MOVE  WRK-BN-PROV-ID    TO  CAN-BP-ID (CAN-ID-INDEX).       20810000
208200      ADD 1                   TO  CAN-ID-INDEX.                   20820000
208300                                                                  20830000
208400 0150-EXIT.                                                       20840000
208500     EXIT.                                                        20850000
208600/                                                                 20860000
208700 0160-SEARCH-TABLE.                                               20870000
208800                                                                  20880000
208900      MOVE '   '  TO CANCEL-ID-SWITCH.                            20890000
209000                                                                  20900000
209100      PERFORM 0170-TABLE-LOOKUP THRU 0170-EXIT                    20910000
209200          VARYING CAN-ID-INDEX FROM 1 BY 1                        20920000
209300              UNTIL CAN-ID-INDEX IS GREATER THAN 755  OR          20930000
209400              (CAN-BP-ID (CAN-ID-INDEX) = HIGH-VALUES).           20940000
209500                                                                  20950000
209600 0160-EXIT.                                                       20960000
209700     EXIT.                                                        20970000
209800/                                                                 20980000
209900 0170-TABLE-LOOKUP.                                               20990000
210000                                                                  21000000
210100      IF  CAN-BP-ID (CAN-ID-INDEX) EQUAL                          21010000
210200           (WRK-BN-PROV-ID OR WRK-ALL-LEV-BEN-PROV)               21020000
210300           MOVE 'YES' TO CANCEL-ID-SWITCH                         21030000
210400           MOVE 755 TO CAN-ID-INDEX.                              21040000
210500                                                                  21050000
210600 0170-EXIT.                                                       21060000
210700     EXIT.                                                        21070000
210800/                                                                 21080000
210900 0200-WRITE-RCRC.                                                 21090000
211000                                                                  21100000
211100     MOVE WRK2-SIGNAL-FROM-ONLINE    TO WRK-SIGNAL-FROM-ONLINE.   21110000
211200     MOVE '2'                        TO IOSW.                     21120000
211300     MOVE +1                         TO IOFILE.                   21130000
211400     MOVE WRK-RECORD-LENGTH          TO OUTLENGTH.                21140000
211500     MOVE 1A-REC-AREA                TO OAREA.                    21150000
211600                                                                  21160000
211700     PERFORM 0210-CALL-BALX THRU 0210-EXIT.                       21170000
211800                                                                  21180000
211900     PERFORM 0030-READ-OWF THRU 0030-EXIT.                        21190000
212000                                                                  21200000
212100 0200-EXIT.                                                       21210000
212200     EXIT.                                                        21220000
212300/                                                                 21230000
212400 0210-CALL-BALX.                                                  21240000
212500                                                                  21250000
212600     CALL 'BALX' USING                                            21260000
212700         IOSW                                                     21270000
212800         IOFILE                                                   21280000
212900         IAREA                                                    21290000
213000         INLENGTH                                                 21300000
213100         OAREA                                                    21310000
213200         OUTLENGTH                                                21320000
213300         INPDDNAME                                                21330000
213400         OUTDDNAMEA.                                              21340000
213500                                                                  21350000
213600 0210-EXIT.                                                       21360000
213700     EXIT.                                                        21370000
213800/                                                                 21380000
213900 0300-PROCESS-SYS-RECD.                                           21390000
214000                                                                  21400000
214100     IF  XWRK-NON-KEY-RELEASE-DATE      EQUAL ZEROS       OR      21410000
214200         XWRK-NON-KEY-RELEASE-DATE  GREATER THAN JUL-DTE          21420000
214300         NEXT SENTENCE                                            21430000
214400     ELSE                                                         21440000
214500         PERFORM 0320-WRITE-RSMT-REC THRU                         21450000
214600                 0320-EXIT                                        21460000
214700         UNTIL XWRK-MATCH-CONT NOT EQUAL 'S' OR                   21470000
214800               1-REQUEST-TYPE  EQUAL '2'.                         21480000
214900                                                                  21490000
215000 0300-EXIT.                                                       21500000
215100     EXIT.                                                        21510000
215200/                                                                 21520000
215300 0320-WRITE-RSMT-REC.                                             21530000
215400                                                                  21540000
215500      MOVE 1A-REC-AREA          TO  RSMT-REC.                     21550000
215600                                                                  21560000
215700      IF  RSMT-PROV-ID = 'XSTMR '                                 21570000
215800          MOVE GC-GCSYSTBL-MAX-OCUR TO                            21580000
215900               GCG-COUNT-TAB-PROVN-POINTERS                       21590000
216000          MOVE 1A-REC-AREA      TO  RSMT-MAST-REC                 21600000
216100          WRITE   RSMT-MAST-REC                                   21610000
216200          GO TO 0320-READ-NEXT-RECORD.                            21620000
216300                                                                  21630000
216400      IF  RSMT-PROV-ID = '#XCON '                                 21640000
216500          MOVE GC-GCSYSTBL-XCON-VARY-MAX-OCUR TO                  21650000
216600               GX6-ENTRY-COUNT                                    21660000
216700          MOVE 1A-REC-AREA      TO  RSMT-REC-A                    21670000
216800          WRITE   RSMT-REC-A                                      21680000
216900          GO TO 0320-READ-NEXT-RECORD.                            21690000
217000                                                                  21700000
217100      IF  RSMT-PROV-ID = '#XCOS '                                 21710000
217200          MOVE GC-GCSYSTBL-XCOS-VARY-MAX-OCUR TO                  21720000
217300               GX7-ENTRY-COUNT                                    21730000
217400          MOVE 1A-REC-AREA      TO  RSMT-REC-B                    21740000
217500          WRITE   RSMT-REC-B                                      21750000
217600          GO TO 0320-READ-NEXT-RECORD.                            21760000
217700                                                                  21770000
217800      IF  RSMT-PROV-ID = '#XDIP '                                 21780000
217900          MOVE GC-GCSYSTBL-XDIP-VARY-MAX-OCUR TO                  21790000
218000               GX4-ENTRY-COUNT                                    21800000
218100          MOVE 1A-REC-AREA      TO  RSMT-REC-C                    21810000
218200          WRITE   RSMT-REC-C                                      21820000
218300          GO TO 0320-READ-NEXT-RECORD.                            21830000
218400                                                                  21840000
218500      IF  RSMT-PROV-ID = '#XDOP '                                 21850000
218600          MOVE GC-GCSYSTBL-XDOP-VARY-MAX-OCUR TO                  21860000
218700               GX5-ENTRY-COUNT                                    21870000
218800          MOVE 1A-REC-AREA      TO  RSMT-REC-D                    21880000
218900          WRITE   RSMT-REC-D                                      21890000
219000          GO TO 0320-READ-NEXT-RECORD.                            21900000
219100                                                                  21910000
219200      IF  RSMT-PROV-ID = '#TXCON'                                 21920000
219300          MOVE GC-GCSYSTBL-TXCON-VARY-MAX-OCR TO                  21930000
219400               GT1-ENTRY-COUNT                                    21940000
219500          MOVE 1A-REC-AREA      TO  RSMT-REC-E                    21950000
219600          WRITE   RSMT-REC-E                                      21960000
219700          GO TO 0320-READ-NEXT-RECORD.                            21970000
219800                                                                  21980000
219900      IF  RSMT-PROV-ID = '#TXCOS'                                 21990000
220000          MOVE GC-GCSYSTBL-TXCOS-VARY-MAX-OCR TO                  22000000
220100               GT2-ENTRY-COUNT                                    22010000
220200          MOVE 1A-REC-AREA      TO  RSMT-REC-F                    22020000
220300          WRITE   RSMT-REC-F                                      22030000
220400          GO TO 0320-READ-NEXT-RECORD.                            22040000
220500                                                                  22050000
220600      IF  RSMT-PROV-ID = '#TXDIP'                                 22060000
220700          MOVE GC-GCSYSTBL-TXDIP-VARY-MAX-OCR TO                  22070000
220800               GT3-ENTRY-COUNT                                    22080000
220900          MOVE 1A-REC-AREA      TO  RSMT-REC-G                    22090000
221000          WRITE   RSMT-REC-G                                      22100000
221100          GO TO 0320-READ-NEXT-RECORD.                            22110000
221200                                                                  22120000
221300      IF  RSMT-PROV-ID = '#TXDOP'                                 22130000
221400          MOVE GC-GCSYSTBL-TXDOP-VARY-MAX-OCR TO                  22140000
221500               GT4-ENTRY-COUNT                                    22150000
221600          MOVE 1A-REC-AREA      TO  RSMT-REC-H                    22160000
221700          WRITE   RSMT-REC-H                                      22170000
221800          GO TO 0320-READ-NEXT-RECORD.                            22180000
221900                                                                  22190000
222000                                                                  22200000
222100**DISPLAY WORK RECORD KEY WHEN ABENDING.                          22210000
222200     DISPLAY  '  '.                                               22220000
222300     DISPLAY  ' GC0010     0320-WRITE-RSMT-REC'.                  22230000
222400                                                                  22240000
222500     IF  WRK-STATUS-CODE   =   'C'                                22250000
222600         DISPLAY  ' WORK-CONTRACT-KEY    =  ' WS-DISPLAY-CON-KEY  22260000
222700     ELSE                                                         22270000
222800     IF  WRK-STATUS-CODE   =   'G'                                22280000
222900         DISPLAY  ' WORK-GROUP-SPEC-KEY  =  ' WS-DISPLAY-GS-KEY   22290000
223000     ELSE                                                         22300000
223100     IF  WRK-STATUS-CODE   =   'S'                                22310000
223200         DISPLAY  ' WORK-SYSTEM-RECORD  =  ' WS-DISPLAY-SYS-KEY   22320000
223300     ELSE                                                         22330000
223400     DISPLAY  ' WORK-RECORD-KEY  =  ' WORK-RECORD-KEY.            22340000
223500                                                                  22350000
223600     DISPLAY ' SYSTEM TABULAR       =  '  RSMT-PROV-ID.           22360000
223700     MOVE  0320  TO ABEND-CODE.                                   22370000
223800     GO TO 0900-ERROR-RTN.                                        22380000
223900                                                                  22390000
224000                                                                  22400000
224100 0320-READ-NEXT-RECORD.                                           22410000
224200                                                                  22420000
224300     PERFORM  0030-READ-OWF  THRU 0030-EXIT.                      22430000
224400                                                                  22440000
224500 0320-EXIT.                                                       22450000
224600     EXIT.                                                        22460000
224700/                                                                 22470000
224800 0900-ERROR-RTN.                                                  22480000
224900                                                                  22490000
225000     CALL 'TSGEND' USING ABEND-CODE.                              22500000
225100                                                                  22510000
225200 0900-EXIT.                                                       22520000
225300     EXIT.                                                        22530000
225400/                                                                 22540000
