000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSSUBTP                                        *00030000
000400*    DATE:       24-OCT-1986                                     *00040000
000500*    AUTHOR:     JOHN CURIN, KEANE, INC.                         *00050000
000600*    FUNCTION:   FIXED SUB-TOPIC TITLES AND TEXT FOR SUB-        *00060000
000700*                TOPIC MENU PROGRAM                              *00070000
000800*                                                                *00080000
000900******************************************************************00090000
001000*                                                                *00100000
001100*                      MAINTENANCE HISTORY                       *00110000
001200*                                                                *00120000
001300*  MOD     DATE     BY  DRPT                ACTION               *00130000
001400* ----- ----------- --- ----- ---------------------------------- *00140000
001500* 01.00 24-OCT-1986 JTC       CREATED                            *00150000
001510*                                                                *00151000
001511* 01.01 27-OCT-1986 JTC       CORRECTED TYPING ERRORS IN         *00151100
001512*                             HEADING AND TOPIC TABLES           *00151200
001513*                                                                *00151300
001514* 01.02 30-OCT-1986 JTC       CORRECTED TYPING ERRORS IN         *00151400
001515*                             TOPIC TABLES                       *00151500
001516*                                                                *00151600
001517* 01.03 24-NOV-1986 JTC       ADD PATIENT ELIGIBILITY (PTE)      *00151700
001518*                             TO RDW, AND ADDED NUMBERS TO       *00151800
001519*                             THE TITLE PORTION OF THE TABLE     *00151900
001520*                                                                *00152000
001530* 01.04 25-NOV-1986 JTC       CORRECT SPELLING IN PROST/ORTHO    *00153000
001540*                             HEADINGS                           *00154000
001550*                                                                *00155000
001560* 01.05 04-FEB-1987 JTC       CORRECT SPELLING IN PROST/ORTHO    *00156000
001570*                             SUB TOPIC MENU                     *00157000
001580*                                                                *00158000
001590* 01.06 16-FEB-1987 JTC       ADDED KEYWORD TO SCREEN DISPLAY    *00159000
001600*                             FIELDS.                            *00160000
001610*                                                                *00161000
001620* 01.07 24-FEB-1987 JTC       MOVED KEYWORD FROM RIGHT BEHIND    *00162000
001621*                             TOPIC NAME TO COLUMN 61            *00162100
001622*                                                                *00162200
001623* 01.08 04-MAR-1987 JTC       CHANGE PATIENT ELIGIBILITY TO      *00162300
001624*                             GENERAL ADMINISTRATION RULES       *00162400
001625*                                                                *00162500
001626* 01.09 11-MAR-1987 JTC       CHANGE KEYWORDS ON MEDICAL         *00162600
001627*                             PROCEDURES, CONGENTIAL SURGERY,    *00162700
001628*                             OTHER SURGERY, CHEMOTHERAPY,       *00162800
001629*                             SHOCK THERAPY, AND OTHER THERAPY   *00162900
001630*                                                                *00163000
001631* 01.10 13-MAR-1987 JTC       ELIMINATED COST CONTAINMENT FROM   *00163100
001632*                             COPYBOOK.                          *00163200
001630*                                                                *00163301
001631* 01.11 16-AUG-1995 AKK       ADDED SUBTOPICS FOR OB TOPIC.      *00163401
001630*                                                                *00163506
001631* 01.12 09-OCT-1995 AKK       DELETED CURRENT DIAGNOSTIC TOPICS  *00163606
001631*                             ADDED NEW ONES.                    *00163706
001630*                                                                *00163811
001630* 01.13 29-DEC-1995 RGO       INCREASED THE NUMBER OF THERAPY    *00163911
001630*                             SUB-TOPICS FROM 4 TO 7.            *00164011
001630*                             DO A FIND ON RGO TO SEE ALL THE    *00164115
001630*                             CHANGES DONE.                      *00164215
001633******************************************************************00164300
001634                                                                  00164400
001635 01  SBT-DI-TABLE-COUNTS.                                         00164500
001636     05  SBT-DI-NUMBER-CODES   PIC S9(4)  COMP    VALUE +4.       00164606
001637     05  SBT-DI-NUMBER-HEADINGS PIC S9(4) COMP    VALUE +6.       00164706
001638     05  SBT-DI-MENU-LINE-COUNT PIC S9(4) COMP    VALUE +2.       00164800
001639                                                                  00164900
001640 01  SBT-DI-TOPIC-TITLE         PIC X(37)            VALUE        00165000
001650     'SELECT DIAGNOSTIC SERVICES SUB-TOPIC'.                      00165100
001660 01  SBT-DI-TOPIC-HEADINGS.                                       00166000
001670     05  FILLER                 PIC X(78)            VALUE        00167000
001680     'SELECT THE PARTICULAR CATEGORY OF DIAGNOSTIC SERVICE YOU WIS00168000
001690-    'H TO DISPLAY FROM '.                                        00169000
001700     05  FILLER                 PIC X(78)            VALUE        00170000
001800     'THE LIST BELOW AND PRESS THE <ENTER> KEY.                   00180000
001900-    '                 '.                                         00190000
002000     05  FILLER                 PIC X(78)            VALUE        00200000
002100     '                                                            00210000
002200-    '                 '.                                         00220000
002300     05  FILLER                 PIC X(78)            VALUE        00230000
002400     '                                                            00240000
002500-    '                 '.                                         00250000
002600     05  FILLER                 PIC X(78)            VALUE        00260000
002700     '                                                          KE00270000
002800-    'YWORD            '.                                         00280000
002900 01  SBT-DI-TOPIC-HEAD REDEFINES SBT-DI-TOPIC-HEADINGS.           00290000
003000     05  SBT-DI-TOPIC-HEADING-LINE                                00300000
003100                     OCCURS 5 TIMES                               00310000
003200                     INDEXED BY SBT-DI-HEAD-INDEX                 00320000
003300                         PIC X(78).                               00330000
003400                                                                  00340000
003500 01  SBT-DI-TOPIC-VALID-DEFINITION.                               00350000
003600     05  FILLER                 PIC X(78)            VALUE        00360000
003700     '01 1 INPATIENT DIAGNOSTIC SERVICES                          00370008
003800-    '  IPS             '.                                        00380008
003900     05  FILLER                 PIC X(78)            VALUE        00390000
004000     '02 2 OUTPATIENT DIAGNOSTIC SERVICES                         00400008
004100-    '  OPS             '.                                        00410008
 04200     05  FILLER                 PIC X(78)            VALUE        00420007
004300     '03 3 EMERGENCY DIAGNOSTIC SERVICES                          00430006
004400-    '  EMR             '.                                        00440006
 04200     05  FILLER                 PIC X(78)            VALUE        00440107
004300     '04 4 ROUTINE DIAGNOSTIC SERVICES                            00441007
004400-    '  RTN             '.                                        00442006
004500 01  SBT-DI-TOPIC-TABLE REDEFINES SBT-DI-TOPIC-VALID-DEFINITION.  00450000
004600     05  SBT-DI-TOPIC-INFO                                        00460000
004700                     OCCURS 4 TIMES                               00470006
004800                     INDEXED BY SBT-DI-TOPIC-INDEX.               00480000
004900         10  SBT-DI-VALID-SELECT PIC XX.                          00490000
005000         10  SBT-DI-TOPIC-NAME   PIC X(76).                       00500000
005100         10  SBT-DI-REF-KEYWORD REDEFINES SBT-DI-TOPIC-NAME.      00510000
005200             15  FILLER              PIC X(60).                   00520000
005300             15  SBT-DI-KEYWORD      PIC X(16).                   00530000
005400                                                                  00540000
005500                                                                  00550000
005600     EJECT                                                        00560000
005700                                                                  00570000
005800 01  SBT-ER-TABLE-COUNTS.                                         00580000
005900     05  SBT-ER-NUMBER-CODES   PIC S9(4)  COMP    VALUE +2.       00590000
006000     05  SBT-ER-NUMBER-HEADINGS PIC S9(4) COMP    VALUE +9.       00600000
006100     05  SBT-ER-MENU-LINE-COUNT PIC S9(4) COMP    VALUE +2.       00610000
006200                                                                  00620000
006300 01  SBT-ER-TOPIC-TITLE         PIC X(31)            VALUE        00630000
006400     'SELECT EMERGENCY CARE SUB-TOPIC'.                           00640000
006500                                                                  00650000
006600 01  SBT-ER-TOPIC-HEADINGS.                                       00660000
006700     05  FILLER                 PIC X(78)            VALUE        00670000
006800     'SELECT THE PARTICULAR CATEGORY OF EMERGENCY CARE YOU WISH TO00680000
006900-    ' DISPLAY FROM THE '.                                        00690000
007000     05  FILLER                 PIC X(78)            VALUE        00700000
007100     'THE LIST BELOW AND PRESS THE <ENTER> KEY.                   00710000
007200-    '                 '.                                         00720000
007300     05  FILLER                 PIC X(78)            VALUE        00730000
007400     '                                                            00740000
007500-    '                 '.                                         00750000
007600     05  FILLER                 PIC X(78)            VALUE        00760000
007700     'SELECT EMERGENCY ACCIDENT CARE IF THE EMERGENCY CARE IS DUE 00770000
007800-    'TO AN ACCIDENT.   '.                                        00780000
007900     05  FILLER                 PIC X(78)            VALUE        00790000
008000     '                                                            00800000
008100-    '                 '.                                         00810000
008200     05  FILLER                 PIC X(78)            VALUE        00820000
008300     'SELECT EMERGENCY MEDICAL CARE IF THE EMERGENCY CARE IS NOT D00830000
008400-    'UE TO AN ACCIDENT.'.                                        00840000
008500     05  FILLER                 PIC X(78)            VALUE        00850000
008600     '                                                            00860000
008700-    '                 '.                                         00870000
008800     05  FILLER                 PIC X(78)            VALUE        00880000
008900     '                                                            00890000
009000-    '                 '.                                         00900000
009100     05  FILLER                 PIC X(78)            VALUE        00910000
009200     '                                                          KE00920000
009300-    'YWORD            '.                                         00930000
009400 01  SBT-ER-TOPIC-HEAD REDEFINES SBT-ER-TOPIC-HEADINGS.           00940000
009500     05  SBT-ER-TOPIC-HEADING-LINE                                00950000
009600                     OCCURS 9 TIMES                               00960000
009700                     INDEXED BY SBT-ER-HEAD-INDEX                 00970000
009800                         PIC X(78).                               00980000
009900                                                                  00990000
010000 01  SBT-ER-TOPIC-VALID-DEFINITION.                               01000000
010100     05  FILLER                 PIC X(78)            VALUE        01010000
010200     '01 1 EMERGENCY ACCIDENT CARE                                01020000
010300-    '  EAC             '.                                        01030000
010400     05  FILLER                 PIC X(78)            VALUE        01040000
010500     '02 2 EMERGENCY MEDICAL CARE                                 01050000
010600-    '  EMC             '.                                        01060000
010700 01  SBT-ER-TOPIC-TABLE REDEFINES SBT-ER-TOPIC-VALID-DEFINITION.  01070000
010800     05  SBT-ER-TOPIC-INFO                                        01080000
010900                     OCCURS 2 TIMES                               01090000
011000                     INDEXED BY SBT-ER-TOPIC-INDEX.               01100000
011100         10  SBT-ER-VALID-SELECT PIC XX.                          01110000
011200         10  SBT-ER-TOPIC-NAME   PIC X(76).                       01120000
011300         10  SBT-ER-REDF-KEYWORD REDEFINES SBT-ER-TOPIC-NAME.     01130000
011400             15  FILLER              PIC X(60).                   01140000
011500             15  SBT-ER-KEYWORD      PIC X(16).                   01150000
011600                                                                  01160000
011700    EJECT                                                         01170000
011800 01  SBT-PO-TABLE-COUNTS.                                         01180000
011900     05  SBT-PO-NUMBER-CODES    PIC S9(4) COMP    VALUE +2.       01190000
012000     05  SBT-PO-NUMBER-HEADINGS PIC S9(4) COMP    VALUE +9.       01200000
012100     05  SBT-PO-MENU-LINE-COUNT PIC S9(4) COMP    VALUE +2.       01210000
012200                                                                  01220000
012300 01  SBT-PO-TOPIC-TITLE         PIC X(38)            VALUE        01230000
012400     'SELECT PROSTHETICS/ORTHOTICS SUB-TOPIC'.                    01240000
012500                                                                  01250000
012600 01  SBT-PO-TOPIC-HEADINGS.                                       01260000
012700     05  FILLER                 PIC X(78)            VALUE        01270000
012800     'SELECT THE PARTICULAR CATEGORY OF PROSTHETIC OR ORTHOITIC CO01280000
012900-    'VERAGE YOU WISH TO'.                                        01290000
013000     05  FILLER                 PIC X(78)            VALUE        01300000
013100     'DISPLAY FROM THE LIST BELOW AND PRESS THE <ENTER> KEY.      01310000
013200-    '                 '.                                         01320000
013300     05  FILLER                 PIC X(78)            VALUE        01330000
013400     '                                                            01340000
013500-    '                 '.                                         01350000
013600     05  FILLER                 PIC X(78)            VALUE        01360000
013700     'SELECT PROSTHETICS FOR INFORMATION REGARDING SUCH THINGS AS 01370000
013800-    'ARTIFICAL LIMBS.  '.                                        01380000
013900     05  FILLER                 PIC X(78)            VALUE        01390000
014000     '                                                            01400000
014100-    '                 '.                                         01410000
014200     05  FILLER                 PIC X(78)            VALUE        01420000
014300     'SELECT ORTHOTICS FOR INFORMATION REGARDING SUCH THINGS AS BR01430000
014400-    'ACES.            '.                                         01440000
014500      05  FILLER                 PIC X(78)            VALUE       01450000
014600      '                                                           01460000
014700-    '                 '.                                         01470000
014800     05  FILLER                 PIC X(78)            VALUE        01480000
014900     '                                                            01490000
015000-    '                 '.                                         01500000
015100     05  FILLER                 PIC X(78)            VALUE        01510000
015200     '                                                          KE01520000
015300-    'YWORD            '.                                         01530000
015400 01  SBT-PO-TOPIC-HEAD REDEFINES SBT-PO-TOPIC-HEADINGS.           01540000
015500     05  SBT-PO-TOPIC-HEADING-LINE                                01550000
015600                     OCCURS 9 TIMES                               01560000
015700                     INDEXED BY SBT-PO-HEAD-INDEX                 01570000
015800                         PIC X(78).                               01580000
015900                                                                  01590000
016000 01  SBT-PO-TOPIC-VALID-DEFINITION.                               01600000
016100     05  FILLER                 PIC X(78)            VALUE        01610000
016200     '01 1 PROSTHETICS                                            01620000
016300-    '  PROST           '.                                        01630000
016400     05  FILLER                 PIC X(78)            VALUE        01640000
016500     '02 2 ORTHOTICS                                              01650000
016600-    '  ORTHO           '.                                        01660000
016700 01  SBT-PO-TOPIC-TABLE REDEFINES SBT-PO-TOPIC-VALID-DEFINITION.  01670000
016800     05  SBT-PO-TOPIC-INFO                                        01680000
016900                     OCCURS 2 TIMES                               01690000
017000                     INDEXED BY SBT-PO-TOPIC-INDEX.               01700000
017100         10  SBT-PO-VALID-SELECT PIC XX.                          01710000
017200         10  SBT-PO-TOPIC-NAME   PIC X(76).                       01720000
017300         10  SBT-PO-REDF-KEYWORD REDEFINES SBT-PO-TOPIC-NAME.     01730000
017400             15  FILLER              PIC X(60).                   01740000
017500             15  SBT-PO-KEYWORD      PIC X(16).                   01750000
017600                                                                  01760000
017700    EJECT                                                         01770000
017800 01  SBT-SG-TABLE-COUNTS.                                         01780000
017900     05  SBT-SG-NUMBER-CODES   PIC S9(4)  COMP    VALUE +4.       01790000
018000     05  SBT-SG-NUMBER-HEADINGS PIC S9(4) COMP    VALUE +5.       01800000
018100     05  SBT-SG-MENU-LINE-COUNT PIC S9(4) COMP    VALUE +2.       01810000
018200                                                                  01820000
018300 01  SBT-SG-TOPIC-TITLE         PIC X(24)            VALUE        01830000
018400     'SELECT SURGERY SUB-TOPIC'.                                  01840000
018500                                                                  01850000
018600 01  SBT-SG-TOPIC-HEADINGS.                                       01860000
018700     05  FILLER                 PIC X(78)            VALUE        01870000
018800     'SELECT THE PARTICULAR CATEGORY OF SURGERY YOU WISH TO DISPLA01880000
018900-    'Y FROM THE LIST  '.                                         01890000
019000     05  FILLER                 PIC X(78)            VALUE        01900000
019100     'BELOW AND PRESS THE <ENTER> KEY.                            01910000
019200-    '                 '.                                         01920000
019300     05  FILLER                 PIC X(78)            VALUE        01930000
019400     '                                                            01940000
019500-    '                 '.                                         01950000
019600     05  FILLER                 PIC X(78)            VALUE        01960000
019700     '                                                            01970000
019800-    '                 '.                                         01980000
019900     05  FILLER                 PIC X(78)            VALUE        01990000
020000     '                                                          KE02000000
020100-    'YWORD            '.                                         02010000
020200 01  SBT-SG-TOPIC-HEAD REDEFINES SBT-SG-TOPIC-HEADINGS.           02020000
020300     05  SBT-SB-TOPIC-HEADING-LINE                                02030000
020400                     OCCURS 5 TIMES                               02040000
020500                     INDEXED BY SBT-SG-HEAD-INDEX                 02050000
020600                         PIC X(78).                               02060000
020700                                                                  02070000
020800 01  SBT-SG-TOPIC-VALID-DEFINITION.                               02080000
020900     05  FILLER                 PIC X(78)            VALUE        02090000
021000     '01 1 CONGENTIAL DEFECTS SURGERY                             02100000
021100-    '  CONG            '.                                        02110000
021200     05  FILLER                 PIC X(78)            VALUE        02120000
021300     '02 2 COSMETIC SURGERY                                       02130000
021400-    '  COS             '.                                        02140000
021500     05  FILLER                 PIC X(78)            VALUE        02150000
021600     '03 3 DENTAL SURGERY                                         02160000
021700-    '  DEN             '.                                        02170000
021800     05  FILLER                 PIC X(78)            VALUE        02180000
021900     '04 4 OTHER SURGERY                                          02190000
022000-    '  GEN             '.                                        02200000
022100 01  SBT-SG-TOPIC-TABLE REDEFINES SBT-SG-TOPIC-VALID-DEFINITION.  02210000
022200     05  SBT-SG-TOPIC-INFO                                        02220000
022300                     OCCURS 4 TIMES                               02230000
022400                     INDEXED BY SBT-SG-TOPIC-INDEX.               02240000
022500         10  SBT-SG-VALID-SELECT PIC XX.                          02250000
022600         10  SBT-SG-TOPIC-NAME   PIC X(76).                       02260000
022700         10  SBT-SG-REDF-KEYWORD REDEFINES SBT-SG-TOPIC-NAME.     02270000
022800             15  FILLER              PIC X(60).                   02280000
022900             15  SBT-SG-KEYWORD      PIC X(16).                   02290000
023000                                                                  02300000
023100    EJECT                                                         02310000
000800*  RGO 1) CHANGED TR-NUMBER FROM 4 TO 7.                          02311015
000800*      2) CHANGED TR-MENU LINE COUNT FROM 2 TO 1.                 02312015
000800*         TR-MENU-LINE-COUNT CONTROLS THE LINE SPACING.           02313015
023200 01  SBT-TR-TABLE-COUNTS.                                         02320000
023300     05  SBT-TR-NUMBER-CODES   PIC S9(4)  COMP    VALUE +7.       02330013
023400     05  SBT-TR-NUMBER-HEADINGS PIC S9(4) COMP    VALUE +5.       02340000
023500     05  SBT-TR-MENU-LINE-COUNT PIC S9(4) COMP    VALUE +1.       02350014
023600                                                                  02360000
023700 01  SBT-TR-TOPIC-TITLE         PIC X(26)            VALUE        02370000
023800     'SELECT THERAPIES SUB-TOPIC'.                                02380000
023900                                                                  02390000
024000 01  SBT-TR-TOPIC-HEADINGS.                                       02400000
024100     05  FILLER                 PIC X(78)            VALUE        02410000
024200     'SELECT THE PARTICULAR TYPE OF THERAPY YOU WISH TO DISPLAY FR02420000
024300-    'OM THE LIST BELOW '.                                        02430000
024400     05  FILLER                 PIC X(78)            VALUE        02440000
024500     'AND PRESS THE <ENTER> KEY.                                  02450000
024600-    '                 '.                                         02460000
024700     05  FILLER                 PIC X(78)            VALUE        02470000
024800     '                                                            02480000
024900-    '                 '.                                         02490000
025000     05  FILLER                 PIC X(78)            VALUE        02500000
025100     '                                                            02510000
025200-    '                 '.                                         02520000
025300     05  FILLER                 PIC X(78)            VALUE        02530000
025400     '                                                          KE02540000
025500-    'YWORD            '.                                         02550000
025600 01  SBT-TR-TOPIC-HEAD REDEFINES SBT-TR-TOPIC-HEADINGS.           02560000
025700     05  SBT-TR-TOPIC-HEADING-LINE                                02570000
025800                     OCCURS 5 TIMES                               02580000
025900                     INDEXED BY SBT-TR-HEAD-INDEX                 02590000
026000                         PIC X(78).                               02600000
026100                                                                  02610000
026200 01  SBT-TR-TOPIC-VALID-DEFINITION.                               02620000
026300     05  FILLER                 PIC X(78)            VALUE        02630000
026400     '01 1 CARDIAC THERAPY                                        02640011
026500-    '  CARDIAC         '.                                        02650011
026600     05  FILLER                 PIC X(78)            VALUE        02660000
026700     '02 2 CHEMOTHERAPY                                           02670011
026800-    '  CHEMO           '.                                        02680000
026900     05  FILLER                 PIC X(78)            VALUE        02690000
027000     '03 3 PHYSICAL THERAPY                                       02700011
027100-    '  PT              '.                                        02710011
027200     05  FILLER                 PIC X(78)            VALUE        02720000
027300     '04 4 RADIATION THERAPY                                      02730011
027400-    '  RADIATION       '.                                        02740011
026300     05  FILLER                 PIC X(78)            VALUE        02741011
026400     '05 5 SHOCK THERAPY                                          02742016
026500-    '  SHOCK           '.                                        02743011
026600     05  FILLER                 PIC X(78)            VALUE        02744011
026700     '06 6 SPEECH THERAPY                                         02745016
026800-    '  SPEECH          '.                                        02746017
026900     05  FILLER                 PIC X(78)            VALUE        02747011
027000     '07 7 OTHER THERAPIES                                        02748016
027100-    '  OTHER           '.                                        02749011
027500 01  SBT-TR-TOPIC-TABLE REDEFINES SBT-TR-TOPIC-VALID-DEFINITION.  02750000
027600     05  SBT-TR-TOPIC-INFO                                        02760000
027700                     OCCURS 7 TIMES                               02770011
027800                     INDEXED BY SBT-TR-TOPIC-INDEX.               02780000
027900         10  SBT-TR-VALID-SELECT PIC XX.                          02790000
028000         10  SBT-TR-TOPIC-NAME   PIC X(76).                       02800000
028100         10  SBT-TR-REDF-KEYWORD REDEFINES SBT-TR-TOPIC-NAME.     02810000
028200             15  FILLER              PIC X(60).                   02820000
028300             15  SBT-TR-KEYWORD      PIC X(16).                   02830000
028400                                                                  02840000
028500    EJECT                                                         02850000
028600 01  SBT-GENRL-TABLE-COUNTS.                                      02860000
028700     05  SBT-GENRL-NUMBER-CODES PIC S9(4) COMP    VALUE +5.       02870000
028800     05  SBT-GENRL-NUMBER-HEADINGS PIC S9(4) COMP VALUE +5.       02880000
028900     05  SBT-GENRL-MENU-LINE-COUNT PIC S9(4) COMP VALUE +2.       02890000
029000                                                                  02900000
029100 01  SBT-GENRL-TOPIC-TITLE      PIC X(45)            VALUE        02910000
029200     'SELECT GENERAL ADMINISTRATION RULES SUB-TOPIC'.             02920000
029300                                                                  02930000
029400 01  SBT-GENRL-TOPIC-HEADINGS.                                    02940000
029500     05  FILLER                 PIC X(78)            VALUE        02950000
029600     'SELECT THE PARTICULAR CATEGORY OF GENERAL ADMINISTRATION RUL02960000
029700-    'ES YOU WISH TO DIS'.                                        02970000
029800     05  FILLER                 PIC X(78)            VALUE        02980000
029900     'PLAY FROM THE LIST BELOW AND PRESS THE <ENTER> KEY.         02990000
030000-    '                 '.                                         03000000
030100     05  FILLER                 PIC X(78)            VALUE        03010000
030200     '                                                            03020000
030300-    '                 '.                                         03030000
030400     05  FILLER                 PIC X(78)            VALUE        03040000
030500     '                                                            03050000
030600-    '                 '.                                         03060000
030700     05  FILLER                 PIC X(78)            VALUE        03070000
030800     '                                                          KE03080000
030900-    'YWORD            '.                                         03090000
031000 01  SBT-GENRL-TOPIC-HEAD REDEFINES SBT-GENRL-TOPIC-HEADINGS.     03100000
031100     05  SBT-GENRL-TOPIC-HEADING-LINE                             03110000
031200                     OCCURS 5 TIMES                               03120000
031300                     INDEXED BY SBT-GENRL-HEAD-INDEX              03130000
031400                         PIC X(78).                               03140000
031500                                                                  03150000
031600 01  SBT-GENRL-TOP-VALID-DEFINITION.                              03160000
031700     05  FILLER                 PIC X(78)            VALUE        03170000
031800     '01 1 DEPENDANT COVERAGE                                     03180000
031900-    '  DEP             '.                                        03190000
032000     05  FILLER                 PIC X(78)            VALUE        03200000
032100     '02 2 EXTENSION OF BENEFITS                                  03210000
032200-    '  TOB             '.                                        03220000
032300     05  FILLER                 PIC X(78)            VALUE        03230000
032400     '03 3 FAMILY SECURITY PROVISION                              03240000
032500-    '  FAMSEC          '.                                        03250000
032600     05  FILLER                 PIC X(78)            VALUE        03260000
032700     '04 4 REINSTATEMENT                                          03270000
032800-    '  REIN            '.                                        03280000
032900     05  FILLER                 PIC X(78)            VALUE        03290000
033000     '05 5 TIMELY FILING                                          03300000
033100-    '  TIME            '.                                        03310000
033200 01  SBT-GENRL-TOPIC-TABLE REDEFINES                              03320000
033300              SBT-GENRL-TOP-VALID-DEFINITION.                     03330000
033400     05  SBT-GENRL-TOPIC-INFO                                     03340000
033500                     OCCURS 5 TIMES                               03350001
033600                     INDEXED BY SBT-GENRL-TOPIC-INDEX.            03360000
033700         10  SBT-GENRL-VALID-SELECT PIC XX.                       03370000
033800         10  SBT-GENRL-TOPIC-NAME PIC X(76).                      03380000
033900         10  SBT-GENRL-REDF-KEYWORD REDEFINES                     03390000
034000                    SBT-GENRL-TOPIC-NAME.                         03400000
034100             15  FILLER              PIC X(60).                   03410000
034200             15  SBT-GENRL-KEYWORD   PIC X(16).                   03420000
      *                                                                 03421001
028600 01  SBT-OB-TABLE-COUNTS.                                         03430001
028700     05  SBT-OB-NUMBER-CODES PIC S9(4) COMP    VALUE +2.          03440001
028800     05  SBT-OB-NUMBER-HEADINGS PIC S9(4) COMP VALUE +5.          03450001
028900     05  SBT-OB-MENU-LINE-COUNT PIC S9(4) COMP VALUE +2.          03460001
029000                                                                  03470001
029100 01  SBT-OB-TOPIC-TITLE      PIC X(37)            VALUE           03480003
029200     'SELECT OBSTETRICAL SERVICES SUB-TOPIC'.                     03490001
   300                                                                  03500002
029400 01  SBT-OB-TOPIC-HEADINGS.                                       03510001
029500     05  FILLER                 PIC X(78)            VALUE        03520001
029600     'SELECT THE PARTICULAR CATEGORY OF OBSTETRICAL SERVICES YOU W03530001
029700-    'ISH TO DISPLAY   '.                                         03540002
029800     05  FILLER                 PIC X(78)            VALUE        03550001
029900     'FROM THE LIST BELOW AND PRESS THE <ENTER> KEY.              03560001
030000-    '                 '.                                         03570001
030100     05  FILLER                 PIC X(78)            VALUE        03580001
030200     '                                                            03590001
030300-    '                 '.                                         03600001
030400     05  FILLER                 PIC X(78)            VALUE        03610001
030500     '                                                            03620001
030600-    '                 '.                                         03630001
030700     05  FILLER                 PIC X(78)            VALUE        03640001
030800     '                                                          KE03650001
030900-    'YWORD            '.                                         03660001
031000 01  SBT-OB-TOPIC-HEAD REDEFINES SBT-OB-TOPIC-HEADINGS.           03670001
031100     05  SBT-OB-TOPIC-HEADING-LINE                                03680001
031200                     OCCURS 5 TIMES                               03690001
031300                     INDEXED BY SBT-OB-HEAD-INDEX                 03700001
031400                         PIC X(78).                               03710001
031500                                                                  03720001
031600 01  SBT-OB-TOP-VALID-DEFINITION.                                 03730001
031700     05  FILLER                 PIC X(78)            VALUE        03740001
031800     '01 1 STANDARD OBSTETRICAL PROCEDURES                        03750009
031900-    '  OB             '.                                         03760002
032000     05  FILLER                 PIC X(78)            VALUE        03770001
032100     '02 2 ADDITIONAL OBSTETRICAL PROCEDURES                      03780010
032200-    '  OBREL          '.                                         03790002
033200 01  SBT-OB-TOPIC-TABLE REDEFINES                                 03890001
033300              SBT-OB-TOP-VALID-DEFINITION.                        03900001
033400     05  SBT-OB-TOPIC-INFO                                        03910001
033500                     OCCURS 2 TIMES                               03920001
033600                     INDEXED BY SBT-OB-TOPIC-INDEX.               03930001
033700         10  SBT-OB-VALID-SELECT PIC XX.                          03940001
033800         10  SBT-OB-TOPIC-NAME PIC X(76).                         03950001
033900         10  SBT-OB-REDF-KEYWORD REDEFINES                        03960001
034000                    SBT-OB-TOPIC-NAME.                            03970003
034100             15  FILLER              PIC X(60).                   03980001
034200             15  SBT-OB-KEYWORD   PIC X(16).                      03990001
