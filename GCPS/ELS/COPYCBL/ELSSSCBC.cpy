000100******************************************************************00010001
000200*                                                                *00020001
000300*    COPYBOOK:   ELSSSCBC                                        *00030001
000400*    DATE:       10-SEP-1986                                     *00040001
000500*    AUTHOR:     EDWARD G. LISS                                  *00050001
000600*                RICHARD J. LUKETICH                             *00060001
000700*    FUNCTION:   SELECTOR STATUS CONTROL BLOCK.                  *00070001
000800*                                                                *00080001
000900*                CONTAINS INFORMATION REQUIRED BY THE SELECTOR   *00090001
001000*                MAINLINE AND SUBROUTINES TO KEEP TRACK OF THE   *00100001
001100*                CURRENT POSITION OF THE SYSTEM WITHIN THE       *00110001
001200*                SELECTION PROCESS.                              *00120001
001300*                                                                *00130001
001400******************************************************************00140001
001500*                                                                *00150001
001600*                      MAINTENANCE HISTORY                       *00160001
001700*                                                                *00170001
001800*  MOD     DATE     BY  DRPT                ACTION               *00180001
001900* ----- ----------- --- ----- ---------------------------------- *00190001
002000* 01.00 10-SEP-1986 EGL       CREATED                            *00200001
002100*                   RJL                                          *00210001
002200* 02.00 30-NOV-1993 RJL       REVISED FOR CONSISTENCY & TO       *00220001
002300*                             TOTAL NUMBER OF LINES              *00230001
002400*                                                                *00240001
002500* 02.01 14-AUG-1997 AKK       EXPANDED GROUP AND CONTRACT SIZE   *00250001
002600*                             ADDED SUPPORT FOR YEAR 2000.  ALSO *00260001
002700*                             ADDED PACKAGE CODE.                *00270001
002800******************************************************************00280001
002900                                                                  00290001
003000 01  SSB-SELECTOR-STATUS-CTL-BLK SYNCHRONIZED.                    00300001
003100                                                                  00310001
003200     02 SSB-MODULE-STATUS-AREA.                                   00320001
003300                                                                  00330001
003400        03 SSB-SELECTOR-STATE             PIC S9(04)       COMP.  00340001
003500           88 SSB-SS-INITIAL              VALUE +00.              00350001
003600           88 SSB-SS-GET-GROUP            VALUE +01.              00360001
003700           88 SSB-SS-GET-SECTION          VALUE +02.              00370001
003800           88 SSB-SS-DATE-REJECT          VALUE +03.              00380001
003900           88 SSB-SS-SHOW-NOTICE          VALUE +04.              00390001
004000           88 SSB-SS-GET-TOPIC            VALUE +05.              00400001
004100           88 SSB-SS-GET-SUBTOPIC         VALUE +06.              00410001
004200           88 SSB-SS-GET-PROVIDER-CLASS   VALUE +07.              00420001
004300           88 SSB-SS-GET-MEDCA-ELIG       VALUE +08.              00430001
004400           88 SSB-SS-GET-FAM-REL          VALUE +09.              00440001
004500           88 SSB-SS-GET-PT-AGE           VALUE +10.              00450001
004600           88 SSB-SS-GET-GRP-SPEC-EFF-DATE   VALUE +11.           00460001
004700           88 SSB-SS-GET-PROV-CTL-BAS-INST   VALUE +12.           00470001
004800           88 SSB-SS-GET-PROV-CTL-SUP-INST   VALUE +13.           00480001
004900           88 SSB-SS-GET-PROV-CTL-BAS-PROF   VALUE +14.           00490001
005000           88 SSB-SS-GET-PROV-CTL-SUP-PROF   VALUE +15.           00500001
005100           88 SSB-SS-GET-CONT-EFF-INST-BAS   VALUE +16.           00510001
005200           88 SSB-SS-GET-CONT-EFF-INST-SUP   VALUE +17.           00520001
005300           88 SSB-SS-GET-CONT-EFF-PROF-BAS   VALUE +18.           00530001
005400           88 SSB-SS-GET-CONT-EFF-PROF-SUP   VALUE +19.           00540001
005500           88 SSB-SS-GET-SERVICE-LOC      VALUE +20.              00550001
005600           88 SSB-SS-GET-MODIFIER-1       VALUE +21.              00560001
005700           88 SSB-SS-GET-MODIFIER-2       VALUE +22.              00570001
005800           88 SSB-SS-GET-RESELECT         VALUE +23.              00580001
005900           88 SSB-SS-SELECTION-DONE       VALUE +24 THRU +99.     00590001
006000        03 SSB-ACTION-MODULE              PIC  X(08).             00600001
006100        03 SSB-MAX-MODULES                PIC S9(04)       COMP.  00610001
006200        03 SSB-MODULE-STATUS-TABLE.                               00620001
006300           04 SSB-MODULE-STATUS           PIC  X(01)              00630001
006400                 OCCURS 25 TIMES                                  00640001
006500                 INDEXED BY SSB-MODULE-STATUS-IDX.                00650001
006600              88 SSB-INITIAL-CALL         VALUE '0'.              00660001
006700              88 SSB-PRIMARY-SCREEN-OUT   VALUE '1'.              00670001
006800              88 SSB-SECONDARY-SCREEN-OUT VALUE '2'.              00680001
006900              88 SSB-START-MENU           VALUE '3'.              00690001
007000              88 SSB-IN-MENU              VALUE '4'.              00700001
007100              88 SSB-MENU-COMPLETE        VALUE '5'.              00710001
007200              88 SSB-CMDLN-INPUT          VALUE '6'.              00720001
007300              88 SSB-RESELECTION          VALUE '7'.              00730001
007400              88 SSB-REPROCESS            VALUE '8'.              00740001
007500              88 SSB-COMPLETED            VALUE '9', 'X',         00750001
007600                                                'D', 'K'.         00760001
007700              88 SSB-SEL-DATA-AVAIL       VALUE '9'.              00770001
007800              88 SSB-NOT-USED             VALUE 'X'.              00780001
007900              88 SSB-DATA-DERIVED         VALUE 'D'.              00790001
008000              88 SSB-DATA-KNOWN-NOT-AVAIL VALUE 'K'.              00800001
008100     02 SSB-GCPS-KEY-SELECTIONS.                                  00810001
008200        03 SSB-PLAN-CODE                  PIC  X(03).             00820001
008300           88 SSB-NO-PLAN-CODE            VALUE LOW-VALUES.       00830002
008400        03 SSB-GROUP-NUMBER.                                      00840001
008500           06 SSB-GRP-NO-1-3              PIC  X(03).             00850001
008600           06 SSB-GRP-NO                  PIC  X(06).             00860001
008700        03 SSB-GRP-NO-R REDEFINES SSB-GROUP-NUMBER                00870002
008800                                          PIC X(09).              00880002
008900              88 SSB-NO-GRP-NO               VALUE LOW-VALUES.    00890002
009000        03 SSB-SECTN-NO.                                          00900001
009100           06 SSB-SECT-NO-1               PIC  X(01).             00910001
009200           06 SSB-SECT-NO                 PIC  X(04).             00920001
009210        03 SSB-SECT-NO-R REDEFINES SSB-SECTN-NO                   00921002
009220                                          PIC X(05).              00922002
009300           88 SSB-NO-SECTN-NO             VALUE LOW-VALUES.       00930007
009400        03 SSB-PKG-CODE                  PIC  X(03).              00940001
009500           88 SSB-NO-PKG-CODE            VALUE LOW-VALUES.        00950001
009600        03 SSB-SERVICE-DATES.                                     00960001
009700           04 SSB-SERV-FROM-DT.                                   00970001
009800              06 SSB-SRV-FROM-DATE-CC     PIC X.                  00980001
009900              06 SSB-SRV-FROM-DATE        PIC S9(05)    COMP-3.   00990001
010000           04 SSB-SRV-FROM-DT-CEN REDEFINES                       01000001
010100                SSB-SERV-FROM-DT          PIC S9(07)    COMP-3.   01010001
010200           04 SSB-SERV-TO-DT.                                     01020002
010300              06 SSB-SRV-TO-DATE-CC       PIC X.                  01030001
010400              06 SSB-SRV-TO-DATE          PIC S9(05)    COMP-3.   01040001
010500           04 SSB-SRV-TO-DT-CEN REDEFINES                         01050002
010600                SSB-SERV-TO-DT            PIC S9(07)    COMP-3.   01060002
010700        03 SSB-FAM-REL-VAR-FLAGS.                                 01070001
010800           04 SSB-FR-MED-GRP              PIC  X(01).             01080001
010900              88 SSB-FR-MED-GRP-VAR       VALUE 'Y'.              01090001
011000              88 SSB-FR-MED-GRP-NVAR      VALUE 'N'.              01100001
011100           04 SSB-FR-FR-GRP               PIC  X(01).             01110001
011200              88 SSB-FR-FR-GRP-VAR        VALUE 'Y'.              01120001
011300              88 SSB-FR-FR-GRP-NVAR       VALUE 'N'.              01130001
011400           04 SSB-FR-PT-AGE-GRP           PIC  X(01).             01140001
011500              88 SSB-FR-PT-AGE-GRP-VAR    VALUE 'Y'.              01150001
011600              88 SSB-FR-PT-AGE-GRP-NVAR   VALUE 'N'.              01160001
011700           04 SSB-FR-CONTRACT-VAR-IND-TABLE.                      01170001
011800              05 SSB-FR-IB-CONTRACT.                              01180001
011900                 06 SSB-FR-MED-IB-CONT    PIC  X(01).             01190001
012000                    88 SSB-FR-MED-IB-CONT-VAR VALUE 'Y'.          01200001
012100                    88 SSB-FR-MED-IB-CONT-NVAR VALUE 'N'.         01210001
012200                 06 SSB-FR-FR-IB-CONT     PIC  X(01).             01220001
012300                    88 SSB-FR-FR-IB-CONT-VAR VALUE 'Y'.           01230001
012400                    88 SSB-FR-FR-IB-CONT-NVAR VALUE 'N'.          01240001
012500                 06 SSB-FR-PT-AGE-IB-CONT PIC  X(01).             01250001
012600                    88 SSB-FR-PT-AGE-IB-CONT-VAR VALUE 'Y'.       01260001
012700                    88 SSB-FR-PT-AGE-IB-CONT-NVAR VALUE 'N'.      01270001
012800              05 SSB-FR-IS-CONTRACT.                              01280001
012900                 06 SSB-FR-MED-IS-CONT    PIC  X(01).             01290001
013000                    88 SSB-FR-MED-IS-CONT-VAR VALUE 'Y'.          01300001
013100                    88 SSB-FR-MED-IS-CONT-NVAR VALUE 'N'.         01310001
013200                 06 SSB-FR-FR-IS-CONT     PIC  X(01).             01320001
013300                    88 SSB-FR-FR-IS-CONT-VAR VALUE 'Y'.           01330001
013400                    88 SSB-FR-FR-IS-CONT-NVAR VALUE 'N'.          01340001
013500                 06 SSB-FR-PT-AGE-IS-CONT PIC  X(01).             01350001
013600                    88 SSB-FR-PT-AGE-IS-CONT-VAR VALUE 'Y'.       01360001
013700                    88 SSB-FR-PT-AGE-IS-CONT-NVAR VALUE 'N'.      01370001
013800              05 SSB-FR-PB-CONTRACT.                              01380001
013900                 06 SSB-FR-MED-PB-CONT    PIC  X(01).             01390001
014000                    88 SSB-FR-MED-PB-CONT-VAR VALUE 'Y'.          01400001
014100                    88 SSB-FR-MED-PB-CONT-NVAR VALUE 'N'.         01410001
014200                 06 SSB-FR-FR-PB-CONT     PIC  X(01).             01420001
014300                    88 SSB-FR-FR-PB-CONT-VAR VALUE 'Y'.           01430001
014400                    88 SSB-FR-FR-PB-CONT-NVAR VALUE 'N'.          01440001
014500                 06 SSB-FR-PT-AGE-PB-CONT PIC  X(01).             01450001
014600                    88 SSB-FR-PT-AGE-PB-CONT-VAR VALUE 'Y'.       01460001
014700                    88 SSB-FR-PT-AGE-PB-CONT-NVAR VALUE 'N'.      01470001
014800              05 SSB-FR-PS-CONTRACT.                              01480001
014900                 06 SSB-FR-MED-PS-CONT    PIC  X(01).             01490001
015000                    88 SSB-FR-MED-PS-CONT-VAR VALUE 'Y'.          01500001
015100                    88 SSB-FR-MED-PS-CONT-NVAR VALUE 'N'.         01510001
015200                 06 SSB-FR-FR-PS-CONT     PIC  X(01).             01520001
015300                    88 SSB-FR-FR-PS-CONT-VAR VALUE 'Y'.           01530001
015400                    88 SSB-FR-FR-PS-CONT-NVAR VALUE 'N'.          01540001
015500                 06 SSB-FR-PT-AGE-PS-CONT PIC  X(01).             01550001
015600                    88 SSB-FR-PT-AGE-PS-CONT-VAR VALUE 'Y'.       01560001
015700                    88 SSB-FR-PT-AGE-PS-CONT-NVAR VALUE 'N'.      01570001
015800           04 SSB-FR-CONTRACT-VAR-IND-TBL                         01580001
015900                 REDEFINES SSB-FR-CONTRACT-VAR-IND-TABLE.         01590001
016000              05 SSB-FR-CONTRACT                                  01600001
016100                    OCCURS 4 TIMES                                01610001
016200                    INDEXED BY SSB-CONT-VAR-IDX.                  01620001
016300                 06 SSB-FR-MED-CONT       PIC  X(01).             01630001
016400                    88 SSB-FR-MED-CONT-VAR   VALUE 'Y'.           01640001
016500                    88 SSB-FR-MED-CONT-NVAR  VALUE 'N'.           01650001
016600                 06 SSB-FR-FR-CONT        PIC  X(01).             01660001
016700                    88 SSB-FR-FR-CONT-VAR VALUE 'Y'.              01670001
016800                    88 SSB-FR-FR-CONT-NVAR VALUE 'N'.             01680001
016900                 06 SSB-FR-PT-AGE-CONT    PIC  X(01).             01690001
017000                    88 SSB-FR-PT-AGE-CONT-VAR VALUE 'Y'.          01700001
017100                    88 SSB-FR-PT-AGE-CONT-NVAR VALUE 'N'.         01710001
017200        03 SSB-FAM-REL-SOURCE.                                    01720001
017300           04 SSB-MEDCA-ELIGY             PIC  X(01).             01730001
017400              88 SSB-MEDCA-ELIG           VALUE 'Y'.              01740001
017500              88 SSB-MEDCA-INELIG         VALUE 'N'.              01750001
017600              88 SSB-MEDCA-UNDEF          VALUE SPACE,            01760001
017700                                                LOW-VALUE.        01770001
017800           04 SSB-FAM-REL                 PIC  X(01).             01780001
017900              88 SSB-MEMBER               VALUE 'M'.              01790001
018000              88 SSB-SPOUSE               VALUE 'S'.              01800001
018100              88 SSB-DEPENDENT            VALUE 'D'.              01810001
018200              88 SSB-FR-UNDEF             VALUE SPACE,            01820001
018300                                                LOW-VALUE.        01830001
018400           04 SSB-PT-AGE                  PIC  X(02).             01840001
018500              88 SSB-PT-AGE-UNDEF         VALUE SPACE,            01850001
018600                                                LOW-VALUE.        01860001
018700        03 SSB-SUBSCRIBER-KEYS.                                   01870001
018800           04 SSB-SUBSCRIBER-NBR          PIC  X(12).             01880001
018900              88 SSB-NO-SUBSCRIBER-NBR    VALUE LOW-VALUES.       01890001
019020           04 SSB-SUB-SECTN-EFF-DT        PIC S9(07).             01902003
019020           04 SSB-SUB-SECTN-TERMN-DT      PIC S9(07).             01902003
019200        03 SSB-COVRD-DATE-RANGE.                                  01920001
019300           04 SSB-COVRD-DATE-RANGE-SOURCE PIC  X(01).             01930001
019400              88 SSB-CDRS-NONE            VALUE SPACES.           01940001
019500              88 SSB-CDRS-SEL             VALUE 'A'.              01950001
019600              88 SSB-CDRS-SUB             VALUE 'B'.              01960001
019700              88 SSB-CDRS-GRP             VALUE 'C'.              01970001
019800              88 SSB-CDRS-CONT            VALUE 'D'.              01980001
019900           04 SSB-COVRD-FROM-DATE.                                01990007
019910              06 SSB-COVRD-FROM-DT-CC  PIC X.                     01991007
019920              06 SSB-COVRD-FROM-DT     PIC S9(05)     COMP-3.     01992007
019930           04 SSB-COVRD-FROM-DATE-CEN REDEFINES                   01993007
019940                   SSB-COVRD-FROM-DATE PIC S9(07) COMP-3.         01994007
020000           04 SSB-COVRD-TO-DATE.                                  02000007
020010              06 SSB-COVRD-TO-DT-CC    PIC X.                     02001007
020020              06 SSB-COVRD-TO-DT       PIC S9(05)       COMP-3.   02002007
020030           04 SSB-COVRD-TO-DATE-CEN REDEFINES                     02003007
020040                   SSB-COVRD-TO-DATE   PIC S9(07) COMP-3.         02004007
020100        03 SSB-GRP-SPECIF-DATA.                                   02010001
020200           04 SSB-GRP-FAM-REL-LVL         PIC  X(02).             02020001
020300           04 SSB-GROUP-EFFECTIVE-DATE.                           02030004
020310              06 SSB-GRP-EFF-DT-CC  PIC X.                        02031007
020320              06 SSB-GRP-EFF-DT     PIC S9(05)       COMP-3.      02032007
020330           04 SSB-GROUP-EFF-DATE-CEN REDEFINES                    02033004
020340               SSB-GROUP-EFFECTIVE-DATE   PIC S9(07)   COMP-3.    02034004
020400           04 SSB-GRP-TERMINATION-DATE.                           02040004
020410              06 SSB-GRP-TERM-DT-CC  PIC X.                       02041007
020420              06 SSB-GRP-TERM-DT     PIC S9(05)       COMP-3.     02042007
020430           04 SSB-GROUP-TERM-DATE-CEN REDEFINES                   02043004
020440                SSB-GRP-TERMINATION-DATE PIC S9(07)    COMP-3.    02044004
020500        03 SSB-CONTRACT-KEY-TABLE.                                02050001
020600           04 SSB-INST-BAS-CONTRACT.                              02060001
020700              05 SSB-INST-BAS-L-O-B       PIC      X(01).         02070001
020800              05 SSB-INST-BAS-PROVDR-CONTROL PIC  X(02).          02080001
020900              05 SSB-INST-BAS-FAM-REL-LVL PIC  X(02).             02090001
021010              05 SSB-INST-BAS-EFF-DATE.                           02101007
021020                 07 SSB-INST-BAS-EFF-DT-CC  PIC X.                02102007
021030                 07 SSB-INST-BAS-EFF-DT     PIC S9(05)   COMP-3.  02103007
021040              05 SSB-INST-BAS-EFF-DT-CEN REDEFINES                02104007
021050                 SSB-INST-BAS-EFF-DATE     PIC S9(07) COMP-3.     02105007
021060                                                                  02106007
021110              05 SSB-INST-BAS-TERM-DATE.                                  
021120                 07 SSB-INST-BAS-TERMN-DT-CC  PIC X.              02112007
021130                 07 SSB-INST-BAS-TERMN-DT    PIC S9(05)  COMP-3.  02113007
021140              05 SSB-INST-BAS-TERMN-DT-CEN REDEFINES              02114007
021150                 SSB-INST-BAS-TERM-DATE     PIC S9(07) COMP-3.    02115007
021160                                                                  02116007
021200           04 SSB-INST-SUP-CONTRACT.                              02120001
021300              05 SSB-INST-SUP-L-O-B       PIC  X(01).             02130001
021400              05 SSB-INST-SUP-PROVDR-CONTROL PIC  X(02).          02140001
021500              05 SSB-INST-SUP-FAM-REL-LVL PIC  X(02).             02150001
021600              05 SSB-INST-SUP-EFF-DATE.                           02160007
021610                 07 SSB-INST-SUP-EFF-DT-CC  PIC X.                02161007
021620                 07 SSB-INST-SUP-EFF-DT     PIC S9(05)   COMP-3.  02162007
021630              05 SSB-INST-SUP-EFF-DT-CEN REDEFINES                02163006
021640                 SSB-INST-SUP-EFF-DATE     PIC S9(07) COMP-3.     02164007
021650                                                                  02165007
021700              05 SSB-INST-SUP-TERM-DATE.                                  
021710                 07 SSB-INST-SUP-TERMN-DT-CC  PIC X.              02171007
021720                 07 SSB-INST-SUP-TERMN-DT    PIC S9(05)  COMP-3.  02172007
021730              05 SSB-INST-SUP-TERMN-DT-CEN REDEFINES              02173006
021740                 SSB-INST-SUP-TERM-DATE     PIC S9(07) COMP-3.    02174007
021750                                                                  02175007
021800           04 SSB-PROF-BAS-CONTRACT.                              02180001
021900              05 SSB-PROF-BAS-L-O-B       PIC  X(01).             02190001
022000              05 SSB-PROF-BAS-PROVDR-CONTROL PIC  X(02).          02200001
022100              05 SSB-PROF-BAS-FAM-REL-LVL PIC  X(02).             02210001
022200              05 SSB-PROF-BAS-EFF-DATE.                           02220007
022210                 10 SSB-PROF-BAS-EFF-DT-CC PIC X.                 02221006
022220                 10 SSB-PROF-BAS-EFF-DT    PIC S9(05)  COMP-3.    02222006
022230              05 SSB-PROF-BAS-EFF-DT-CEN REDEFINES                02223006
022240                  SSB-PROF-BAS-EFF-DATE PIC S9(07) COMP-3.        02224007
022250                                                                  02225007
022300              05 SSB-PROF-BAS-TERM-DATE.                          02230007
022310                 10 SSB-PROF-BAS-TERMN-DT-CC PIC X.               02231006
022320                 10 SSB-PROF-BAS-TERMN-DT    PIC S9(05)  COMP-3.  02232006
022330              05 SSB-PROF-BAS-TERM-DT-CEN REDEFINES               02233006
022340                  SSB-PROF-BAS-TERM-DATE PIC S9(07) COMP-3.       02234007
022350                                                                  02235007
022400           04 SSB-PROF-SUP-CONTRACT.                              02240001
022500              05 SSB-PROF-SUP-L-O-B       PIC  X(01).             02250001
022600              05 SSB-PROF-SUP-PROVDR-CONTROL PIC  X(02).          02260001
022700              05 SSB-PROF-SUP-FAM-REL-LVL PIC  X(02).             02270001
022800              05 SSB-PROF-SUP-EFF-DATE.                           02280007
022810                 07 SSB-PROF-SUP-EFF-DT-CC PIC X.                 02281006
022820                 07 SSB-PROF-SUP-EFF-DT    PIC S9(05)   COMP-3.   02282006
022830              05 SSB-PROF-SUP-EFF-DATE-CC REDEFINES               02283006
022840                 SSB-PROF-SUP-EFF-DATE     PIC S9(07)   COMP-3.   02284007
022850                                                                  02285007
022900              05 SSB-PROF-SUP-TERMIN-DATE.                                
022910                 07 SSB-PROF-SUP-TERM-DT-CC PIC X.                02291006
022920                 07 SSB-PROF-SUP-TERM-DT    PIC S9(05)   COMP-3.  02292006
022930              05 SSB-PROF-SUP-TERMIN-DATE-CC  REDEFINES           02293006
022940                    SSB-PROF-SUP-TERMIN-DATE PIC S9(07)  COMP-3.  02294006
022950                                                                  02295007
023000        03 SSB-CONTRACT-KEY-TBL                                   02300001
023100              REDEFINES SSB-CONTRACT-KEY-TABLE.                   02310001
023200           04 SSB-CONTRACT                                        02320001
023300                 OCCURS 4 TIMES                                   02330001
023400                 INDEXED BY SSB-CONT-IDX.                         02340001
023500              05 SSB-CONT-L-O-B           PIC  X(01).             02350001
023600              05 SSB-CONT-PROVDR-CONTROL  PIC  X(02).             02360001
023700              05 SSB-CONT-FAM-REL-LVL     PIC  X(02).             02370001
023800              05 SSB-CONT-EFF-DATE.                               02380007
023810                 10 SSB-CONT-EFF-DT-CC    PIC X.                  02381007
023820                 10 SSB-CONT-EFF-DT       PIC S9(05)   COMP-3.    02382007
023830              05 SSB-CONT-EFF-DATE-CEN REDEFINES                  02383007
023840                   SSB-CONT-EFF-DATE      PIC S9(07)   COMP-3.    02384007
023850                                                                  02385007
023900              05 SSB-CONT-TERMN-DT.                               02390001
023910                 10 SSB-CONT-TERMN-DT-CC    PIC X.                02391007
023920                 10 SSB-CONT-TERMN-DT       PIC S9(05)   COMP-3.  02392007
023930              05 SSB-CONT-TERMN-DATE-CEN REDEFINES                02393007
023940                   SSB-CONT-TERMN-DT        PIC S9(07)   COMP-3.  02394007
023950                                                                  02395007
024000     02 SSB-TOPIC-SELECTIONS.                                     02400001
024100        03 SSB-TOP-SEL-FIRST              PIC S9(04)       COMP.  02410001
024200        03 SSB-TOP-SEL-LAST               PIC S9(04)       COMP.  02420001
024300        03 SSB-TOPIC-PGM                  PIC  X(08).             02430001
024400        03 SSB-TOPIC                      PIC  X(16).             02440001
024500        03 SSB-SUB-TOPIC                  PIC  X(16).             02450001
024600        03 SSB-MODIFIER-1                 PIC  X(16).             02460001
024700        03 SSB-MODIFIER-2                 PIC  X(16).             02470001
024800        03 SSB-MODIFIER-3                 PIC  X(16).             02480001
024900        03 SSB-MODIFIER-4                 PIC  X(16).             02490001
025000        03 SSB-PROVIDER-CLASS             PIC  X(01).             02500001
025100           88 SSB-PROV-CLASS-INST         VALUE 'I'.              02510001
025200           88 SSB-PROV-CLASS-PROF         VALUE 'P'.              02520001
025300           88 SSB-PROV-CLASS-BOTH         VALUE 'B'.              02530001
025400        03 SSB-SERVICE-CLASS              PIC  X(01).             02540001
025500           88 SSB-SERV-CLASS-IP           VALUE 'I'.              02550001
025600           88 SSB-SERV-CLASS-OP           VALUE 'O'.              02560001
025700           88 SSB-SERV-CLASS-BOTH         VALUE 'B'.              02570001
025800        03 SSB-TOPIC-PHRASE               PIC  X(80).             02580001
025900        03 SSB-SUB-TOPIC-PHRASE           PIC  X(80).             02590001
026000        03 SSB-MODIFIER-1-PHRASE          PIC  X(80).             02600001
026100        03 SSB-MODIFIER-2-PHRASE          PIC  X(80).             02610001
026200        03 SSB-CS-MNU-RESPONSE-TABLE.                             02620001
026300           04 SSB-CS-RESPONSE             PIC  X(16)              02630001
026400                 OCCURS 10 TIMES                                  02640001
026500                 INDEXED BY SSB-CS-RESP-IDX.                      02650001
026600                                                                  02660001
026700     02 SSB-CMDLN-DATA.                                           02670001
026800        03 SSB-CMDLN-GROUP-NUMBER.                                02680006
026801           05 SSB-CMDLN-GRP-NO-1-3        PIC  X(03).             02680106
026802           05 SSB-CMDLN-GRP-NO            PIC  X(06).             02680206
026900        03 SSB-CMDLN-SECTN-NO.                                    02690007
026901           05 SSB-CMDLN-SECTN-1           PIC  X.                 02690106
026902           05 SSB-CMDLN-SECTN             PIC  X(04).             02690206
026910        03 SSB-CMDLN-PKG-CODE             PIC  X(03).             02691006
027000        03 SSB-CMDLN-SUBSCRIBER-NBR       PIC  X(12).             02700006
027100        03 SSB-CMDLN-SRV-FROM-DATE.                               02710007
027110           05 SSB-CMDLN-SRV-FROM-DT-CC   PIC X.                   02711007
027120           05 SSB-CMDLN-SRV-FROM-DT      PIC S9(05)     COMP-3.   02712007
027130        03 SSB-CMDLN-SRV-FROM-DATE-CC      REDEFINES              02713007
027140           SSB-CMDLN-SRV-FROM-DATE       PIC  S9(07) COMP-3.      02714007
027150                                                                  02715007
027200        03 SSB-CMDLN-SRV-TO-DATE.                                 02720007
027210           05 SSB-CMDLN-SRV-TO-DT-CC      PIC X.                  02721007
027220           05 SSB-CMDLN-SRV-TO-DT         PIC S9(05)     COMP-3.  02722007
027230        03 SSB-CMDLN-SRV-TO-DATE-CC      REDEFINES                02723007
027240           SSB-CMDLN-SRV-TO-DATE          PIC  S9(07) COMP-3.     02724007
027300                                                                  02730001
027400     02 SSB-MENU-CONTROLS.                                        02740001
027500        03 SSB-MNU-CUR-TOP-ITM            PIC S9(04)       COMP.  02750001
027600        03 SSB-MNU-CUR-BOT-ITM            PIC S9(04)       COMP.  02760001
027700        03 SSB-MNU-VAL-COUNT              PIC S9(04)       COMP.  02770001
027800        03 SSB-MNU-TITLE                  PIC  X(50).             02780001
027900        03 SSB-MNU-NUM-CHOICES            PIC S9(04)       COMP.  02790001
028000           88 SSB-MNU-MAX-CHOICES         VALUE 30.               02800001
028100        03 SSB-MNU-CHOICE-TABLE.                                  02810001
028200           04 SSB-MNU-CHOICE              PIC  X(16)              02820001
028300                 OCCURS 30 TIMES                                  02830001
028400                 INDEXED BY SSB-MNU-IDX.                          02840001
028500                                                                  02850001
028600     02 SSB-PAGE-DISPLAY.                                         02860001
028700        03 SSB-PD-CURRENT-PAGE            PIC S9(04)       COMP.  02870001
028800        03 SSB-PD-LAST-PAGE               PIC S9(04)       COMP.  02880001
028900        03 SSB-PD-STATUS-SW               PIC  X(01).             02890001
029000           88 SSB-TERM-TASK               VALUE 'T'.              02900001
029100           88 SSB-CONT-TASK               VALUE 'C'.              02910001
029200                                                                  02920001
029300     02 SSB-MENU-STACK.                                           02930001
029400        03 SSB-STACK-CURRENT-ITEM         PIC S9(04)       COMP.  02940001
029500           88 SSB-STACK-EMPTY             VALUE 0.                02950001
029600           88 SSB-VALID-STACK-ITEM        VALUE 1 THRU 10.        02960001
029700        03 SSB-PUSH-INDICATOR             PIC      X(01).         02970001
029800           88 SSB-PUSH-MENU               VALUE 'Y'.              02980001
029900        03 SSB-STACK-AREA                                         02990001
030000              OCCURS 10 TIMES.                                    03000001
030100           05 SSB-STACK-STATE             PIC S9(04)       COMP.  03010001
