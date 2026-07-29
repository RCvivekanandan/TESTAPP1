000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSTOPIC                                        *00030000
000400*    DATE:       21-OCT-1986                                     *00040000
000500*    AUTHOR:     JOHN T. CURIN,  KEANE, INC.                     *00050000
000600*    FUNCTION:   FIXED TOPIC TITLES AND TEXT FOR TOPIC           *00060000
000610*                MENU PROGRAM                                    *00070000
000700*                                                                *00080000
001000******************************************************************00090000
001100*                                                                *00100000
001200*                      MAINTENANCE HISTORY                       *00110000
001300*                                                                *00120000
001400*  MOD     DATE     BY  DRPT                ACTION               *00130000
001500* ----- ----------- --- ----- ---------------------------------- *00140000
001600* 01.00 21-OCT-1986 JTC       CREATED                            *00150000
001610*                                                                *00160000
001620* 01.01 23-OCT-1986 JTC       EXPAND WS-TOPIC-NAME IN TABLE      *00170000
001630*                             FROM 31 TO 50 BYTES.               *00180000
001640*                                                                *00190000
001650* 01.02 24-OCT-1986 JTC       CORRECTED TYPO IN REDEFINES OF     *00200000
001660*                             TOP-TOPIC-HEADING TABLE AND        *00210000
001670*                             CHANGED DATA NAME PREFIX FROM      *00220000
001680*                             WS-  TO TOP-, AND MOVED THE TABLE  *00230000
001690*                             COUNTS TO THE BEGINNING OF THE     *00240000
001700*                             COPYBOOK.                          *00250000
001701*                                                                *00260000
001702* 01.03 27-OCT-1986 JTC       CORRECTED TYPING ERRORS IN THE     *00270000
001703*                             HEADING AND TOPIC TABLES           *00280000
001710*                                                                *00290000
001720* 01.04 20-NOV-1986 JTC       ELIMINATED CHIROPRACTOR FROM       *00300000
001721*                              TOP-TOPIC-VALID-DEFINITION TABLE  *00310000
001722*                              AND RENUMBERED CODES              *00320000
001730*                             CHANGE VALUE OF TOP-NUMBER-CODES   *00330000
001740*                             FROM +36 TO +35                    *00340000
001750*                                                                *00350000
001760* 01.05 24-NOV-1986 JTC       ADD NUMBER SELECTION TO TITLE      *00360000
001761*                             PART OF MENU.  CORRECTED SPELLING  *00370000
001762*                             IN HEADING ADDED ONE BLANK LINE    *00380000
001763*                             TO HEADING                         *00390000
001750*                                                                *00400000
001764* 01.17 23-JAN-1987 RJL       CORRECTED SPELLING.                *00410000
001770*                                                                *00420000
001770* 01.18 17-FEB-1987 JTC       ADDED KEYWORDS TO SCREEN DISPLAY   *00430000
001770*                             FIELDS                             *00440000
001770*                                                                *00450000
      * 01.19 25-FEB-1987 JTC       MOVE KEYWORD FROM RIGHT BEHIND     *00460000
      *                             TOPIC NAME TO COLUMN 51            *00470000
      *                                                                *00480000
      * 01.20 05-MAR-1987 JTC       RESEQUENCED TOPICS DUE TO THE      *00490000
      *                             CHANGING OF PATIENT ELIGIBLITY TO  *00500000
      *                             GENERAL ADMINISTRATION RULES       *00510000
      *                                                                *00520000
      * 01.21 11-MAR-1987 JTC       CHANGED KEYWORDS ON MAXIMUM,       *00530000
      *                             MEDICARE, SURGERY, THERAPY,        *00540000
      *                             VISION AND GENERAL ADMINISTRATION  *00550000
      *                             RULES                              *00560000
      *                                                                *00570000
      * 01.22 12-JAN-1988 REB/LET   ADDED NEW TOPIC TO BE DISPLAYED ON *00580000
      *                             PRIMARY MENU WITH THE KEYWORD      *00590000
      *                             'PPONET'.                          *00600000
      *                                                                *00610000
      * 01.23 25-JAN-1988 REB/LET   ADDED NEW TOPIC TO BE DISPLAYED ON *00620000
      *                             PRIMARY MENU WITH THE KEYWORD 'CS'.*00630000
      *                                                                *00640000
      * 01.24 11-APR-1988 LET       CHANGED PARTICIPATING TO PREFERRED *00650000
      *                             IN PPO NETWORK AS REQUESTED BY AKK.*00660000
      *                                                                *00670000
      * 01.25 27-MAR-1990 GEM       ADDED NEW TOPIC TO BE DISPLAYED ON *00680000
      *                             PRIMARY MENU WITH KEYWORD 'GVL'.   *00690000
      *                                                                *00700000
      * 01.26 25-FEB-1991 GEM       CHANGED INPATIENTS MEDICAL SERVICES*00710000
      *                             TOPIC TITLE TO 'MED/PSYCH/VISITS'  *00720000
      *                                                                *00730000
      * 01.27 25-FEB-1991 GEM       CHANGED GROUP SPECIFIC TABULAR     *00731000
      *                             TOPIC TITLE TO;                    *00732000
      *                             'SPECIAL PROVIDER CONSIDERATIONS'. *00733000
      *                             '2727 PRESCRIPTION DRUGS           *00734000
      *                                                                *00735000
      * 01.28 11-MAR-1991 GEM       CHANGED 'PRESCRIPTION DRUGS' TOPIC *00736000
      *                             TITLE TO; 'PHARMACEUTICAL SERVICES *00737000
      *                                       /MATERIALS AND SUPPLIES' *00738000
      *                                                                *00739001
      * 01.29 19-JAN-1993 AKK       ADDED RESTRICTED PROVIDER NETWORK  *00739101
      *                             OPTION                             *00739201
      *                                                                *00739302
      * 01.30 08-FEB-1993 AKK       HAD TWO TOPICS NUMBERED 32.        *00739402
      *                                                                *00739502
      * 01.31 08-FEB-1994 AKK       ADDED BENEFIT EXCEPTION NARRATIVE  *00739603
      *                             (BEN).                             *00739703
      *                                                                *00739804
      * 01.32 08-MAR-1995 AKK       ADDED COMMUNITY PROVIDER OPTION    *00739904
      *                             (CPO).                             *00740004
      *                                                                *00740110
      * 01.33 13-MAR-1996 AKK       ADDED COMMUNITY BLUE OPTION (CBL)  *00740210
      *                             AND PREFERRED ANCILLARY NETWORK   * 00740310
      *                             (PAN).                              00740410
      * 01.34 04-MAR-1999 AKK       ADDED BAE AND BAENET            )  *00740513
001800******************************************************************00741000
001810                                                                  00750000
001820 01  TOP-TABLE-COUNTS.                                            00760000
001830     05  TOP-NUMBER-CODES    PIC S9(4) COMP    VALUE +46.         00770014
001840     05  TOP-NUMBER-HEADINGS PIC S9(4) COMP    VALUE +4.          00780000
001850     05  TOP-MENU-LINE-COUNT PIC S9(4) COMP    VALUE +1.          00790000
001900                                                                  00800000
001910 01  TOP-TOPIC-TITLE           PIC X(20)            VALUE         00810000
001920     'SELECT INQUIRY TOPIC'.                                      00820000
001930                                                                  00830000
002000 01  TOP-TOPIC-HEADINGS.                                          00840000
002100     05  FILLER                PIC X(78)            VALUE         00850000
002200     'SELECT THE TOPIC YOU WISH TO DISPLAY FROM THE LIST BELOW AND00860000
002300-    ' PRESS THE <ENTER>'.                                        00870000
002400     05  FILLER                PIC X(78)            VALUE         00880000
002500     'KEY.  PRESS THE <ENTER> KEY WITHOUT SELECTING A TOPIC TO SEE00890000
002600-    ' MORE TOPICS YOU '.                                         00900000
002610     05  FILLER                 PIC X(78)            VALUE        00910000
002620     'CAN SELECT.                                                 00920000
002630-    '                 '.                                         00930000
002700     05  FILLER                 PIC X(78)            VALUE        00940000
002800     '                                                KEYWORD     00950000
002810-    '                 '.                                         00960000
002820 01  TOP-TOPIC-HEAD REDEFINES TOP-TOPIC-HEADINGS.                 00970000
002821     05  TOP-TOPIC-HEADING-LINE                                   00980000
002830                     OCCURS 4 TIMES                               00990000
002840                     INDEXED BY TOP-HEAD-INDEX                    01000000
002850                         PIC X(78).                               01010000
002900                                                                  01020000
003300 01  TOP-TOPIC-VALID-DEFINITION.                                  01030000
003400     05  FILLER                 PIC X(68)            VALUE        01040000
003500     '01 1 AMBULANCE                                      AMB     01050000
003510-    '       '.                                                   01060000
003600     05  FILLER                 PIC X(68)            VALUE        01070000
003700     '02 2 ANESTHESIA                                     ANES    01080000
003710-    '       '.                                                   01090000
003800     05  FILLER                 PIC X(68)            VALUE        01100000
003900     '03 3 ASSISTANT SURGEON                              ASSTS   01110000
003910-    '       '.                                                   01120000
004200     05  FILLER                 PIC X(68)            VALUE        01130000
004300     '04 4 BENEFIT EXCEPTION NARRATIVE                    BEN     01140003
004310-    '       '.                                                   01150000
004200     05  FILLER                 PIC X(68)            VALUE        01150413
004300     '05 5 BLUE ADVANTAGE ENTREPRENEUR NETWORK            BAENET  01150515
004310-    '       '.                                                   01150613
004200     05  FILLER                 PIC X(68)            VALUE        01151003
004300     '06 6 COINSURANCE                                    COINS   01152014
004310-    '       '.                                                   01153003
004400     05  FILLER                 PIC X(68)            VALUE        01160000
004500     '07 7 COMMUNITY BLUE NETWORK                         CBLNET  01170014
004510-    '       '.                                                   01180000
004400     05  FILLER                 PIC X(68)            VALUE        01180110
004500     '08 8 COMMUNITY PARTICIPATING NETWORK                CPONET  01180214
004510-    '       '.                                                   01180310
004400     05  FILLER                 PIC X(68)            VALUE        01181004
004500     '09 9 CONSULTATION                                   CON     01182014
004510-    '       '.                                                   01183004
004400     05  FILLER                 PIC X(68)            VALUE        01190000
004500     '1010 CONTRACT SUMMARY                               CS      01200014
004510-    '       '.                                                   01210000
004600     05  FILLER                 PIC X(68)            VALUE        01220000
004700     '1111 COORDINATED HOME CARE                          CHC     01230014
004710-    '       '.                                                   01240000
004800     05  FILLER                 PIC X(68)            VALUE        01250000
004900     '1212 COORDINATION OF BENEFITS                       COB     01260014
004910-    '       '.                                                   01270000
004800     05  FILLER                 PIC X(68)            VALUE        01271011
004900     '1313 COPAY ADMINISTRATION                           COPAY   01272014
004910-    '       '.                                                   01273011
005000     05  FILLER                 PIC X(68)            VALUE        01280000
005100     '1414 COST CONTAINMENT                               CCP     01290014
005110-    '       '.                                                   01300000
005200     05  FILLER                 PIC X(68)            VALUE        01310000
005300     '1515 DEDUCTIBLE                                     DED     01320014
005310-    '       '.                                                   01330000
005400     05  FILLER                 PIC X(68)            VALUE        01340000
005500     '1616 DIAGNOSTIC SERVICES                            DX      01350014
005510-    '       '.                                                   01360000
005600     05  FILLER                 PIC X(68)            VALUE        01370000
005700     '1717 DURABLE MEDICAL EQUIPMENT                      DME     01380014
005710-    '       '.                                                   01390000
005800     05  FILLER                 PIC X(68)            VALUE        01400000
005900     '1818 EMERGENCY ACCIDENT/MEDICAL CARE                EMER    01410014
005910-    '       '.                                                   01420000
006000     05  FILLER                 PIC X(68)            VALUE        01430000
006100     '1919 EXCLUDED BENEFITS                              EXCL    01440014
006110-    '       '.                                                   01450000
006200     05  FILLER                 PIC X(68)            VALUE        01460000
006300     '2020 EXTENDED CARE FACILITY                         ECF     01470014
006310-    '       '.                                                   01480000
008000     05  FILLER                 PIC X(68)            VALUE        01490000
008100     '2121 GENERAL ADMINISTRATION RULES                   ADMIN   01500014
008110-    '       '.                                                   01510000
006400     05  FILLER                 PIC X(68)            VALUE        01550000
006500     '2222 HEARING CARE                                   HEAR    01560014
006510-    '       '.                                                   01570000
006800     05  FILLER                 PIC X(68)            VALUE        01580000
006900     '2323 MAXIMUMS                                       MAX     01590014
006910-    '       '.                                                   01600000
007000     05  FILLER                 PIC X(68)            VALUE        01610000
007100     '2424 MEDICARE                                       MEDI    01620014
007110-    '       '.                                                   01630000
006600     05  FILLER                 PIC X(68)            VALUE        01631000
006700     '2525 MED/PSYCH/VISITS                               VISITS  01632014
006710-    '       '.                                                   01633000
007200     05  FILLER                 PIC X(68)            VALUE        01640000
007300     '2626 NURSING                                        NURS    01650014
007310-    '       '.                                                   01660000
007400     05  FILLER                 PIC X(68)            VALUE        01670000
007500     '2727 OBSTERICAL SERVICES                            OB      01680014
007510-    '       '.                                                   01690000
007600     05  FILLER                 PIC X(68)            VALUE        01700000
007700     '2828 OUT-OF-COUNTRY CLAIMS                          OCC     01710014
007710-    '       '.                                                   01720000
007800     05  FILLER                 PIC X(68)            VALUE        01730000
007900     '2929 OUT-OF-POCKET EXPENSE LIMITS                   OPX     01740014
007910-    '       '.                                                   01750000
008400     05  FILLER                 PIC X(68)            VALUE        01751000
008500     '3030 PHARMACEUTICAL SERVICES/MATERIAL AND SUPPLIES  RX      01752014
008510-    '       '.                                                   01753000
007800     05  FILLER                 PIC X(68)            VALUE        01760000
007900     '3131 PREFERRED ANCILLARY NETWORK                    PANNET  01770014
007910-    '       '.                                                   01780000
007800     05  FILLER                 PIC X(68)            VALUE        01781010
007900     '3232 PREFERRED PROVIDER NETWORK                     PPONET  01782014
007910-    '       '.                                                   01783010
008200     05  FILLER                 PIC X(68)            VALUE        01790000
008300     '3333 PODIATRY                                       POD     01800014
008310-    '       '.                                                   01810000
008600     05  FILLER                 PIC X(68)            VALUE        01850000
008700     '3434 PROSTHETICS AND ORTHOTICS                      PROST   01860014
008710-    '       '.                                                   01870000
008800     05  FILLER                 PIC X(68)            VALUE        01880000
008900     '3535 PROVIDER ELIGIBILITY                           PVE     01890014
008910-    '       '.                                                   01900000
009000     05  FILLER                 PIC X(68)            VALUE        01910000
009100     '3636 PSYCHIATRIC SERVICES                           PSYCH   01920014
009110-    '       '.                                                   01930000
009200     05  FILLER                 PIC X(68)            VALUE        01940000
009300     '3737 RESTRICTED PROVIDER NETWORK                    RPONET  01950014
009310-    '       '.                                                   01960000
009200     05  FILLER                 PIC X(68)            VALUE        01960101
009300     '3838 ROOM AND BOARD                                 ROOM    01960214
009310-    '       '.                                                   01960301
008000     05  FILLER                 PIC X(68)            VALUE        01961000
008100     '3939 SPECIAL PROVIDER CONSIDERATIONS                SPC     01962014
008110-    '       '.                                                   01963000
009400     05  FILLER                 PIC X(68)            VALUE        01970000
009500     '4040 SUBSTANCE ABUSE                                ABUSE   01980014
009510-    '       '.                                                   01990000
009600     05  FILLER                 PIC X(68)            VALUE        02000000
009700     '4141 SURGERY                                        SURG    02010014
009710-    '       '.                                                   02020000
009800     05  FILLER                 PIC X(68)            VALUE        02030000
009900     '4242 THERAPIES                                      THER    02040014
009910-    '       '.                                                   02050000
010000     05  FILLER                 PIC X(68)            VALUE        02060000
010100     '4343 VISION CARE                                    VIS     02070014
010110-    '       '.                                                   02080000
010200     05  FILLER                 PIC X(68)            VALUE        02120000
010300     '4444 WAITING PERIODS                                WAIT    02130014
010310-    '       '.                                                   02140000
010400     05  FILLER                 PIC X(68)            VALUE        02150000
010500     '4545 WELLNESS/PREVENTIVE CARE                       WELL    02160014
010510-    '       '.                                                   02170000
010400     05  FILLER                 PIC X(68)            VALUE        02171008
010500     '4646 WORKERS COMPENSATION                           WC      02172014
010510-    '       '.                                                   02173008
010600 01  TOP-TOPIC-TABLE REDEFINES TOP-TOPIC-VALID-DEFINITION.        02180000
010610     05  TOP-TOPIC-INFO                                           02190000
010700                     OCCURS 46 TIMES                              02200014
010800                     INDEXED BY TOP-TOPIC-INDEX.                  02210000
010900         10  TOP-VALID-SELECT PIC XX.                             02220000
011000         10  TOP-TOPIC-NAME   PIC X(66).                          02230000
               10  TOP-REDF-KEYWORD REDEFINES TOP-TOPIC-NAME.           02240000
                   15  FILLER           PIC X(50).                      02250000
011010             15  TOP-KEYWORD      PIC X(16).                      02260000
011500                                                                  02270000
