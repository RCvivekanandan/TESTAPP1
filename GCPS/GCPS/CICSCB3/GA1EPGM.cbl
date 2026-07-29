000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID. GA1EPGM.                                             00000200
000300**** THIS IS A COBOL/2 PROGRAM ***                                00000300
000400 AUTHOR. D SECOR  -  A C I.                                       00000400
000500 DATE-WRITTEN.   01/11/84.                                        00000500
000600 DATE-COMPILED.                                                   00000600
000700     SKIP3                                                        00000700
000800******************************************************************00000800
000900*   GA1EPGM         ALL LEVEL TABULAR MAINTENANCE PROGRAM        *00000900
001000*                   OUT-OF-POCKET LIMITS TABULAR         - #AOL  *00001000
001100*                                                                *00001100
001200*     THIS PROGRAM WILL PERFORM ADD/CHANGE/DELETE MAINTENANCE TO *00001200
001300*   ENTRIES ON THE ALL LEVEL TABULAR RECORD.  THE TABULAR RECORD *00001300
001400*   CAN CONTAIN UP TO 29 ENTRIES IN A TABLE, EACH ENTRY HAS A    *00001400
001500*   NUMBER OF FIELDS AND ANOTHER SMALL TABLE, THIS 2NDARY TABLE  *00001500
001600*   IS A POINTER TO AN INTERNAL TABULAR RECORD.  THE PROGRAM     *00001600
001700*   OPERATES IN TWO MODES AN ADD/CHANGE AND A CHANGE/DELETE MODE.*00001700
001800*                                                                *00001800
001900*     THE CHG/DEL SCREEN WILL DISPLAY AN ENTRY CURRENTLY ON THE  *00001900
002000*   ALL LEVEL TABULAR RECORD.  THE OPERATOR WILL THEN CHANGE ANY *00002000
002100*   FIELD OR ADD, CHANGE, OR DELETE AN INTERNAL TABULAR; THERE IS*00002100
002200*   ALSO THE OPTION OF DELETING THE WHOLE ENTRY IN THE TABULAR,  *00002200
002300*   INTERNAL TABULARS INCLUDED, THIS OPTION CAN BE SELECTED BY   *00002300
002400*   PLACING A 'D' IN THE DELETE OPTION FIELD.                    *00002400
002500*                                                                *00002500
002600*    THE CHG/ADD SCREEN WILL BE SHOWN THE OPERATOR WHEN THEY WANT*00002600
002700*   TO ADD A NEW ENTRY INTO THE TABLE. FROM HERE THE OPERATOR CAN*00002700
002800*   FILL THE ENTRY, THEN REVIEW AND CHANGE THE NEW ENTRY.  AFTER *00002800
002900*   THE OPERATOR KEYS ENTER ON THE REVIEW SCREEN, THE PROGRAM    *00002900
003000*   ASSUMES THAT THEY WANT TO ADD ANOTHER ENTRY AND SO DISPLAYS  *00003000
003100*   THE SKELETON FOR THE OPERATOR TO OVERLAY.                    *00003100
003200*                                                                *00003200
003300*   FUNC CODE: GA1E                                              *00003300
003400*                          ********************************      *00003400
003500*                          *   THIS MAPSET IS SHARED BY   *      *00003500
003600*                          *   THE FOLLOWING MODULES:     *      *00003600
003700*                          *   1. GA1BPGM                 *      *00003700
003800*                          *   2. GA1CPGM                 *      *00003800
003900*                          *   3. GA1DPGM                 *      *00003900
004000*   MAPSET:    GA1XSETC ==>*   4. GA1EPGM                 *      *00004000
004100*                          *   5. GASEDIT1                *      *00004100
004200*                          *   6. GACDEPGM                *      *00004200
004300*                          *   7. GK1BPGM                 *      *00004300
004400*                          *   8. GK1CPGM                 *      *00004400
004500*                          *   9. GK1DPGM                 *      *00004500
004600*                          *  10. GK1EPGM                 *      *00004600
004700*                          *  11. GAS1UPD                 *      *00004700
004800*                          *  12. GAS2UPD                 *      *00004800
004900*                          *  13. GAS3UPD                 *      *00004900
005000*                          *  14. GAS4UPD                 *      *00005000
005100*                          ********************************      *00005100
005200*                                                                *00005200
005300*   FILES:     GCPSWORK     GCTABULR                             *00005300
005400*              GCCONTR      GCGRPSPC                             *00005400
005500*              GCSTABLR     GCSPROVN                             *00005500
005600*                                                                *00005600
005700******************************************************************00005700
005800                                                                  00005800
005900/    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          00005900
006000*    *-*         U P D A T E   H I S T O R Y         *-*          00006000
006100*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          00006100
006200                                                                  00006200
006300*NUM-* *-DATE-* *WHO* *-----------DESCRIPTION--------------------*00006300
006400*                                                                *00006400
006500*        06/25/03 DAF USE COPYBOOK GCTIPGPC INSTEAD OF GCTIPGTC  *00006500
006600*                                                                *00006600
006700*  D368  06/04/02  JP ADD SELECTIVE ADDITIONAL BENEFIT           *00006700
006800*                         DETERMINATION (SABD)                   *00006800
006900*                                                                *00006900
007000*  D365B 06/03/02  JP ADD COMBINATION APPLIED IND (CAPI)         *00007000
007100*                                                                *00007100
007200*  P???  11/14/01 AKK  ADD SUPPORT FOR 4 NEW BITS, 2 FOR EMER    *00007200
007300*                      TWO FOR SERIOUS MENTAL ILLNESS.           *00007300
007400*                                                                *00007400
007500* D352 09/19/00  GDM   ADD ACCUMULATOR IDENTIFIER                *00007500
007600*                                                                *00007600
007700* P????  07/10/00 GSP   ADDED LOGIC FOR NEW #IPGS INTERNAL       *00007700
007800*                       TABULAR.                                 *00007800
007900*                                                                *00007900
008000* P????  11/19/99 FRY   ADD LENGTH PARAMETER TO THE RETURN       *00008000
008100*                       COMMAND WHEN DFHCOMMAREA IS SPECIFIED.   *00008100
008200*                                                                *00008200
008300* D341   10/07/98  GDM  HIDE TIME/DOLLAR FIELD FROM SCREEN       *00008300
008400*                                                                *00008400
008500* 14726/ 04/29/98  AB   MODIFIED TO BECOME MILLENNIUM COMPLIANT  *00008500
008600* 15057                 AND TO ADD PACKAGE CODE, PLAN CODE, AND  *00008600
008700*                       INCREASE GROUP AND SECTION NUMBERS.      *00008700
008800*                                                                *00008800
008900* 14726/ 10/22/97 DAU ADDED CODE TO SUPPORT THE YEAR 2000 AND    *00008900
009000* 15057               THE EXPANSION OF THE GROUP SPECIFIC AND    *00009000
009100*                     CONTRACT KEY TO SUPPORT THE TEXAS MERGER.  *00009100
009200*                                                                *00009200
009300*  D303  02/03/97 DAU ADD FEAK INDICATOR                         *00009300
009400*                                                                *00009400
009500* 12262  02/28/92 TPM ADD NEW COND-BIT LIF  (LIFE-THREATING)     *00009500
009600*                     COND-LIFE-THREAT-BIT                       *00009600
009700*                                                                *00009700
009800*                                                                *00009800
009900*D12009 08/28/91  TPM   INCREASED THE FAMILY-RELATION FIELD      *00009900
010000*                           FROM ONE POSITION TO TWO POSITIONS.  *00010000
010100*                                                                *00010100
010200* 11836 07/09/91  ENW  INCLUDED THE FYI FIELD IN THE COMPARE     *00010200
010300*                      AREA.                                     *00010300
010400*                                                                *00010400
010500* 11154 03/06/91 ENW  CORRECTED TYPO FOR '1EF3' DISCREPANCY.     *00010500
010600*                     CODE WAS COPIED BUT NOT CHANGED.           *00010600
010700*                                                                *00010700
010800* 11154 02/19/91  NGE  REDUCE OCCURS MAX NUM FROM 46 TO 44.      *00010800
010900*                                                                *00010900
011000* 11154 01/17/91  NGE 1. ADD AGE-QUAL-IND-FROM AND AGE-QUAL-TO   *00011000
011100*                        TO ALL ACCUM TABULARS, AS CDE FIELDS.   *00011100
011200*                       2. REMOVE RELATIONSHIP-IND FROM CDE LOGIC*00011200
011300*                                                                *00011300
011400* 11154   10/23/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *00011400
011500* D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *00011500
011600* D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *00011600
011700* D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *00011700
011800*                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *00011800
011900*                       6. ADD NEW INTERNAL TABS #IDGD AND #IPGP.*00011900
012000*                       7. >>> CONVERT TO COBOL/2 <<<.           *00012000
012100*                                                                *00012100
012200* PG008 07/18/89 NGE  CDE MESSAGE SHOULD BE DISPLAYED WHEN ADDING*00012200
012300*                     OCCURS IN ADD/DEL MODE.                    *00012300
012400*                                                                *00012400
012500* D210 06/23/89  ENW   ADDED NEW EDIT TO CHECK THE PERCENT       *00012500
012600*                      VALUE IN EACH OCCURS. IF THE VALUE IS NOT  00012600
012700*                      EQUAL TO 100, THEN ALL OF THE VALUES HAVE  00012700
012800*                      TO BE THE SAME. PLEASE NOTE THAT A         00012800
012900*                      DIFFERENT APPROACH WAS TAKEN FOR THIS EDIT.00012900
013000*                      IT IS DONE AFTER PF3/PF15 WAS PRESSED AND  00013000
013100*                      BEFORE XCTL'ING TO THE PREVIOUS MENU.  THE 00013100
013200*                      REASON FOR THIS IS BECAUSE WE DON'T WANT   00013200
013300*                      THE OPERATOR TO EXIT THE SCREEN WITHOUT    00013300
013400*                      FIXING THE PROBLEM, HOWEVER, THE OPERATOR  00013400
013500*                      CAN SELECT TO PRESS THE CLEAR KEY AND IN   00013500
013600*                      THIS INSTANCE, WE WANT TO MAKE SURE THE    00013600
013700*                      PROCESSING FOR THIS RECORD IS COMPLETE. OK?00013700
013800*                                                                 00013800
013900* D200 05/18/89  NGE   ADD TWO NEW COND-BITS TMJ AND INF,        *00013900
014000*                      TEMPROMAND-JOINT AND INFERTILITY-COND.    *00014000
014100*                                                                *00014100
014200* ???? 03/13/89  ENW   CHANGED 'DFHBMASK' TO 'DFHBMASF' IN       *00014200
014300*                      7900-000-RESET-ATTRIBUTES SECTION BECAUSE *00014300
014400*                      FIELDS THAT WEREN'T BEING RETURNED WERE   *00014400
014500*                      CAUSING EDIT PROBLEMS.                    *00014500
014600*                                                                *00014600
014700*D201  01/17/89  ENW  ADDED LOGIC FOR BISCENDING INDICATOR.      *00014700
014800*                                                                *00014800
014900* ????    09/14/88  NGE  FIX INTERNAL TABS DELETE LOGIC FOR      *00014900
015000*                        UPDATING CDE COUNTERS DEPENDING ON THE  *00015000
015100*                        INTRNL TAB RECORD NOT THE CDE STATUS    *00015100
015200*                        IN THE ACCUM RECORD ATTACHED. ALLOW     *00015200
015300*                        +CDE+ DISPLAY RETURNING FROM INTRNL PGM.*00015300
015400*                                                                *00015400
015500* ????    08/03/88  NGE  FIX ADDING ACCURS LOGIC WILL FLAG THE   *00015500
015600*                            ACCUMS AS A CDE & GAS1PGM INTERNAL  *00015600
015700*                            TAB LOGIC TO FLAG ITS ACCUM RECORD  *00015700
015800*                            WHEN THE INTERNL FLAGED CDE.        *00015800
015900*                                                                *00015900
016000*                                                                *00016000
016100*  D126      02/24/88  JLA  1. CHANGE OPTION FILE SELECTION 'S'  *00016100
016200*                              TO 'A'.                           *00016200
016300*                                                                *00016300
016400* D143 01/29/88  DES  ADD CDE/NON-CDE CHANGES USING JERRY'S      *00016400
016500*                     SCHEME WHERE THE SPLIT IS PERFORMED        *00016500
016600*                     IN BATCH AND THEN MERGED BACK ONTO W/F     *00016600
016700*                                                                *00016700
016800*  ????      09/29/87  JLA  FIX EXISTING CDE PROBLEM IN THE      *00016800
016900*                             4600- SECTION THAT CAUSED THE CDE  *00016900
017000*                             MODIFIED STATUS TO BE SET.         *00017000
017100*                                                                *00017100
017200* N126 08/28/87  JLA  ADD LOGIC FOR SUICIDE BIT.                 *00017200
017300*                                                                *00017300
017400* N121 08/09/87  NE   ADD A NEW FIELD -DEFINITION-               *00017400
017500*                                                                *00017500
017600* D200 04/24/87  JLA  BREAK INTO MULTIPLE MODULES.               *00017600
017700*                                                                *00017700
017800*D0120 03/16/87  JLA  CHANGES FOR SINGLE TABULAR SUPPORT THAT    *00017800
017900*                     ARE EXECUTED FROM TRANSACTION GTM1:        *00017900
018000*                     1. WHEN CHECKING ENTRY TRANSACTION CODE    *00018000
018100*                        TREAT GTM1 THE SAME AS GC4A.            *00018100
018200*                     2. PF1/PF13 - TREAT THE SAME AS IF GC4A    *00018200
018300*                        HAD CALLED, XCTL TO ADD SCREEN PROGRAM  *00018300
018400*                     3. PF3/PF15 - CONSTRUCT COMMAREA AS IF     *00018400
018500*                        GC4A HAD CALLED, XCTL TO GTM1PGM.       *00018500
018600*                     4. ALLOW ATTACHMENT (MAP FROM) OF PROD-    *00018600
018700*                        UCTION TABULARS, BUT PROHIBIT ATTACH-   *00018700
018800*                        ING SINGLE TABULARS UNDER SINGLE TAB-   *00018800
018900*                        ULAR SUPPORT.  DON'T CONSTRUCT C3       *00018900
019000*                        WORKFILE RECORD.  DON'T PASS CONTROL    *00019000
019100*                        TO INTERNAL TABULAR MAINTENANCE PGM.    *00019100
019200*                        DON'T CHANGE INTERNAL SLOT# ON SCREEN   *00019200
019300*                        TO ALL 9K NUMBER.                       *00019300
019400*                     5. PROHIBIT INTERNAL TAB CHANGES UNDER     *00019400
019500*                        STS.                                    *00019500
019600*                     6. PROHIBIT MAPPING FROM SKELETON UNDER    *00019600
019700*                        STS.                                    *00019700
019800*                                                                *00019800
019900*N106    02/26/87 RKH   ADDED LOGIC FOR THE FYI FIELD WHICH IS   *00019900
020000*N118                      TO BE VALIDATED & THE LOGIC TO ONLY   *00020000
020100*                          DISPLAY THE TABULAR OCCURANCE NUMBER. *00020100
020200*                                                                *00020200
020300*CDEL502 9/29/86 JLA    1. CHANGE COPY-SORTABLE-FLDS FROM X(128  *00020300
020400*                          TO X(125) AND REPLACE LAST THREE      *00020400
020500*                          BYTES WITH COPY-SORT-FYI X(3) NOT     *00020500
020600*                          INCLUDED IN TABULAR ENTRY SORT.       *00020600
020700*                       2. INITIAL THE CDE STATUS IN ANY         *00020700
020800*                          WORKFILE RECORDS CREATED TO \
020900*                       3. IF PRODUCTION TABULAR RECORD IS       *00020900
021000*                          BEING CHANGED AND CONTAINS CRITICAL   *00021000
021100*                          DATA ELEMENTS:                        *00021100
021200*                          A. INITIAL SCREEN :                   *00021200
021300*                             1) CDE STATUS(\
021400*                                 - HIGH-LIGHT CDE LABELS,       *00021400
021500*                                   SHOW +CDE+ INDICATOR.        *00021500
021600*                             2) CDE STATUS NOT (\
021700*                                 - HIGH-LIGHT CDE LABELS,       *00021700
021800*                                   HIGH-LIGHT AND PROTECT CDE   *00021800
021900*                                   ELEMENTS,                    *00021900
022000*                                   SHOW +CDE+ INDICATOR.        *00022000
022100*                          B. IF CDE ELEMENTS ARE CHANGED, SET   *00022100
022200*                             WORKFILE TABULAR CDE STATUS CODE   *00022200
022300*                             TO \
022400*                             OR GROUP SPECIFIC CONTROL          *00022400
022500*                             RECORD CDE STATUS APPROPRIATELY,   *00022500
022600*                             ISSUE CDE CHANGE MESSAGE AND       *00022600
022700*                             POSITION CURSOR ON +CDE+.  THE     *00022700
022800*                             OPERATOR THEN ADVANCES TO NEXT     *00022800
022900*                             SCREEN BY PRESSING ENTER A SECOND  *00022900
023000*                             TIME.                              *00023000
023100*                                                                *00023100
023200*CDEL501 8/22/86 JLA    DETERMINE IF POTENTIALLY CRITICAL DATA   *00023200
023300*                       ELEMENTS ARE CRITICAL BASED ON THE TRANS *00023300
023400*                       ROUTING FILE (PGM=GCTRSRT).  IF THEY     *00023400
023500*                       ARE CRITICAL AND THE USER IS DOING A     *00023500
023600*                       CHANGE TO A WORKFILE GROUP SPECIFIC      *00023600
023700*                       RECORD, PROTECT THE CRITICAL DATA ELE-   *00023700
023800*                       MENT ON THE SCREEN.                      *00023800
023900*  ?   08/12/86  AHL/DF MODIFIED 1100- ROUTINE SO THAT IT        *00023900
024000*                       FINISHES VALIDATING EACH DATA ELEMENT    *00024000
024100*                       IN THE CORRECT SEQUENCE ACCORDING TO     *00024100
024200*                       THE SCREEN LAYOUT.                       *00024200
024300*                                                                *00024300
024400*D094  08/04/86  AHL  REVISED 'NEG' LOGIC TO LET OPERATOR USE    *00024400
024500*                     EITHER 'NEG' OR DOLLARS & CENTS WITH       *00024500
024600*                     DECIMAL POINT FOR VALUE LIMIT FIELD WHEN   *00024600
024700*                     VALUE QUALIFIER = '5'.                     *00024700
024800*                                                                *00024800
024900*N112  08/01/86  AMJ  ADDED DAY FACTOR INDICATOR                 *00024900
025000*                                                                *00025000
025100*      07/30/86  AMJ  FIXED ERROR MESSAGES                       *00025100
025200*                                                                *00025200
025300*N108  07/28/86  RKH  ADDED TWO NEW CONDITION BITS               *00025300
025400*                     PRE-EXISTING CONDITIONS                    *00025400
025500*                     NON-EMERGENCY CONDITION.                   *00025500
025600*                                                                *00025600
025700*D094  07/23/86  AKM  ALLOWED 10 POSITIONS FOR VALUE LIMIT       *00025700
025800*                     FIELD SO THAT OPERATORS CAN ENTER          *00025800
025900*                     1 MILLION AS '1000000.00'.                 *00025900
026000*                                                                *00026000
026100*      06/26/86  JTC  ADDED LOGICAL EDITS                        *00026100
026200*                                                                *00026200
026300*      06/19/86  JTC  MOVED PF4/PF16 LOGIC TO AFTER VALIDATION   *00026300
026400*                     SO THAT INCORRECT RECORDS WOULD NOT BE     *00026400
026500*                     ADDED TO THE FILE.  ADDED AN INVALID       *00026500
026600*                     PF MESSAGE TO COVER THE ABOVE CASE.        *00026600
026700*                                                                *00026700
026800*                     ADDED A CHANGE TO ALLOW SCROLLING FORWARD  *00026800
026900*                     IF THE ONLY THING 'WRONG' IS AN EMPTY      *00026900
027000*                     TABLE.                                     *00027000
027100*                                                                *00027100
027200*                     FIXED THE SCREEN NOT BEING REFRESHED       *00027200
027300*                     PROPERLY FOLLOWING AN ADD WITH PF4/PF16    *00027300
027400*                                                                *00027400
027500*P495  05/30/86  AMJ  CHANGED TO ALLOW IPGT AND IPGN AT THE      *00027500
027600*                     SAME TIME                                  *00027600
027700*                                                                *00027700
027800*M106  05/30/86  AMJ  FIXED ATTRIBUTE ON ADD TO/OVERLAY FIELD    *00027800
027900*                                                                *00027900
028000*      05/14/86  MDD  CHANGED THE SEQUENCE OF THE EDITS TO BE    *00028000
028100*                      IN SYNC WITH THE SCREEN.                  *00028100
028200*                                                                *00028200
028300*      02/21/86  MDD  ADDED VALIDATION FOR FOLLOWING FIELDS:     *00028300
028400*                     'ADD-TO-OVERLAY INDICATOR',                *00028400
028500*                     'BENEFIT PERIOD',                          *00028500
028600*                     'FAMILY OR INDIVIDUAL INDICATOR',          *00028600
028700*                     'LINE OF BUSINESS',                        *00028700
028800*                     'INTERNAL DESCRIPTOR',                     *00028800
028900*                     'SERVICE GROUP',                           *00028900
029000*                     'CO-PAY INDICATOR',                        *00029000
029100*                     'COST CONTAINMENT INDICATOR',              *00029100
029200*                     'REINSTATEMENT INDICATOR',                 *00029200
029300*                     'BENEFIT PERIOD TIME QUALIFIER',           *00029300
029400*                     'BAMA BENEFIT PERIOD OVERRIDE',            *00029400
029500*                     'INTERVAL TYPE',                           *00029500
029600*                     'INTERVAL OVERRIDE INDICATOR',             *00029600
029700*                     'PLACE OF TREATMENT INDICATOR',            *00029700
029800*                     'VALUE QUALIFIER'                          *00029800
029900*                                                                *00029900
030000*      01/15/86  RKH   ADDED CODE FOR THE NEG VALUE LIMIT        *00030000
030100*                                                                *00030100
030200*      11/18/85  LET   ADDED CODE FOR THE NEW CONDITION BIT      *00030200
030300*                      NAMED ACCIDENT.                           *00030300
030400*                                                                *00030400
030500*      11/08/85  ENW   REVISED LOGIC TO ACCEPT SPACES IN THE     *00030500
030600*                      INTERNAL DESCRIPTOR FIELD INSTEAD OF      *00030600
030700*                      ZEROS.  ALSO ADDED NEW COPY MEMBER        *00030700
030800*                      'GCVALTAB'.                               *00030800
030900*                                                                *00030900
031000*      08-14-02  GTF   RECOMPILE FOR OPID EXPANSION              *00031000
031100*                                                                *00031100
031200*      05-07-07  LR    RECOMPILE FOR CHANGES IN GASEDIT1         *00031200
031300*                                                                *00031300
031200*      10-15-10  MJL   ALLOW 'UNL' VALUE.                        *00031310
      *                                                                *00031311
      ****   03/19/15  KIKI  CHANGED - #AOL PERCENT FIELD VALIDATION   *00031316
      ****                             1200-D210-EDIT  SECTION         *00031317
      ****                             NO LONGER IS EXECUTED FROM      *00031318
      ****                             5000-000-XCTL-TO-PREVIOUS-MENU  *00031319
      ****                                                             *00031320
      ****                             NO LONGER THE PERCENT FIELD     *00031321
      ****                             MUST BE SAME IN ALL OCCURS.,    *00031322
      ****                             WHEN OTHER THAN +100            *00031323
      *                                                                *00031330
      * P21595 09/19/16   HSB CHANGES FOR GCPS NEW FIELDS BENEFIT      *00031350
      *                       TYPE CODE,TIER CODE,TIER LEVEL.          *00031370
031400******************************************************************00031400
031500    SKIP3                                                         00031500
031600 ENVIRONMENT DIVISION.                                            00031600
031700/    D A T A   D I V I S I O N                                    00031700
031800 DATA DIVISION.                                                   00031800
031900 WORKING-STORAGE SECTION.                                         00031900
032000 01  WS-BEGIN                    PIC X(24)  VALUE                 00032000
032100     '***GA1EPGM WS BEGINS***'.                                   00032100
032200                                                                  00032200
032300*     T I T L E   L I N E S                                       00032300
032400 01  WS-TITLE-LINES.                                              00032400
032500 COPY GCMHLINE.                                                   00032500
032600*****05  GROUP-SPECIFIC-TITLE-LINE       PIC X(42)                00032600
032700*      VALUE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'.        00032700
032800*    05  GROUP-SPECIFIC-ID-LINE.                                  00032800
032900*      10  FILLER                        PIC X(20)                00032900
033000*        VALUE 'GROUP SPECIFIC ID= '.                             00033000
033100*      10  FILLER                        PIC X(5) VALUE 'GRP= '.  00033100
033200*      10  GRP-SPEC-GROUP-NO             PIC X(6).                00033200
033300*      10  FILLER                        PIC X(6) VALUE ' SEC= '. 00033300
033400*      10  GRP-SPEC-SECTION-NO           PIC X(4).                00033400
033500*      10  FILLER                        PIC X(5) VALUE ' FR= '.  00033500
033600*      10  GRP-SPEC-FAM-REL-LVL          PIC XX.                  00033600
033700*      10  FILLER                        PIC X(7) VALUE ' EFDT= '.00033700
033800*      10  GRP-SPEC-EFF-DATE             PIC X(6).                00033800
033900*    05  CONTRACT-TITLE-LINE             PIC X(42)                00033900
034000*      VALUE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'.         00034000
034100*    05  CONTRACT-ID-LINE.                                        00034100
034200*      10  FILLER                      PIC X(14)                  00034200
034300*        VALUE 'CONTRACT ID= '.                                   00034300
034400*      10  FILLER                      PIC X(5) VALUE 'GRP= '.    00034400
034500*      10  CONTRACT-GROUP-NO           PIC X(6).                  00034500
034600*      10  FILLER                      PIC X(6) VALUE ' SEC= '.   00034600
034700*      10  CONTRACT-SECTION-NO         PIC X(4).                  00034700
034800*      10  FILLER                      PIC X(6) VALUE ' LOB= '.   00034800
034900*      10  CONTRACT-LOB                PIC X.                     00034900
035000*      10  FILLER                      PIC X(6) VALUE ' PRV= '.   00035000
035100*      10  CONTRACT-PROV-CTL           PIC XX.                    00035100
035200*      10  FILLER                      PIC X(5) VALUE ' FR= '.    00035200
035300*      10  CONTRACT-FAM-REL-LVL        PIC XX.                    00035300
035400*      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  00035400
035500*      10  CONTRACT-EFF-DATE               PIC X(6).              00035500
035600*    05  BENEFIT-PROVISION-TITLE-LINE    PIC X(42)                00035600
035700*      VALUE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'.        00035700
035800*    05  BENEFIT-PROVISION-ID-LINE.                               00035800
035900*      10  FILLER                      PIC X(5) VALUE 'GRP= '.    00035900
036000*      10  BEN-PROV-GROUP-NO           PIC X(6).                  00036000
036100*      10  FILLER                      PIC X(6) VALUE ' SEC= '.   00036100
036200*      10  BEN-PROV-SECTION-NO         PIC X(4).                  00036200
036300*      10  FILLER                      PIC X(6) VALUE ' LOB= '.   00036300
036400*      10  BEN-PROV-LOB                PIC X.                     00036400
036500*      10  FILLER                      PIC X(6) VALUE ' PRV= '.   00036500
036600*      10  BEN-PROV-PROV-CTL           PIC XX.                    00036600
036700*      10  FILLER                      PIC X(5) VALUE ' FR= '.    00036700
036800*      10  BEN-PROV-FAM-REL-LVL        PIC XX.                    00036800
036900*      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  00036900
037000*      10  BEN-PROV-EFF-DATE           PIC X(6).                  00037000
037100*      10  FILLER                      PIC X(8) VALUE ' BPVID= '. 00037100
037200*      10  BEN-PROV-ID-NO              PIC X(6).                  00037200
037300*    05  AOL-TITLE-LINE                PIC X(26)                  00037300
037400*********VALUE '  OUT-OF-POCKET LIMITS    '.                      00037400
037500/     A L T E R N A T I V E   W O R K F I L E   K E Y S           00037500
037600 01  FILLER                      PIC X(32)  VALUE                 00037600
037700     '*** ALTERNATIVE WORKFILE KEY ***'.                          00037700
037800 01  SAVE-WS-ALT-WORKFILE-KEYS.                                   00037800
037900     05 FILLER                   PIC X(63) VALUE SPACES.          00037900
038000                                                                  00038000
038100 01  WS-ALT-WORKFILE-KEYS.                                        00038100
038200 COPY GCWRKKEY.                                                   00038200
038300                                                                  00038300
038400                                                                  00038400
038500/    D A T E   F O R M A T T I N G   A R E A                      00038500
038600 01  HGADATES-COMMAREA.                                           00038600
038700 COPY HGCDAT01.                                                   00038700
038800                                                                  00038800
038900                                                                  00038900
039000*  *** WORKFIELDS, AND SWITCHES **                                00039000
039100 01  WS-WORK-FIELDS.                                              00039100
039200                                                                  00039200
039300     05  GCTRSRT-COMMAREA-LEN      PIC S9(4)  COMP VALUE +100.    00039300
039400     05  WS-HEX-00                     PIC X    VALUE LOW-VALUE.  00039400
039500     05  WS-ONE-LOW                    PIC X VALUE LOW-VALUES.    00039500
039600     05  SAVE-COPY-FROM-SLOT           PIC 9(7).                  00039600
039700     05  WS-HOLD-PCT-LVL           PIC S9(3)  COMP-3 VALUE +0.    00039700
039800     05  WS-UNPK-SLOT              PIC  9(7)  COMP-3 VALUE ZEROS. 00039800
039900                                                                  00039900
040000     05  WS-CDE-REQUEST-CODES.                                    00040000
040100         10  WS-REQUEST-4500-CDE-PROTECT    PIC X(4) VALUE '4500'.00040100
040200         10  WS-REQUEST-4600-CDE-STATUS     PIC X(4) VALUE '4600'.00040200
040300         10  WS-REQUEST-4700-CNTL-UPDATE    PIC X(4) VALUE '4700'.00040300
040400                                                                  00040400
040500*     I N T E R N A L   T A B U L A R   P R O G R A M   N A M E   00040500
040600 01  WS-INTERNAL-TABULAR-PGM-ID        PIC X(8).                  00040600
040700                                                                  00040700
040800                                                                  00040800
040900** ***ALL LEVEL TABULAR ENTRY SAVED HERE DURING SORT ***          00040900
041000 01  WS-ENTRY                          PIC X(176).                00041000
041100     SKIP3                                                        00041100
041200/    A T T R I B U T E S                                          00041200
041300 COPY DFHBMSCA.                                                   00041300
041400     02  DFHBMABF                PIC X VALUE '9'.                 00041400
041500/    A T T E N T I O N   I D E N T I F I E R S                    00041500
041600 COPY DFHAID.                                                     00041600
041700/    R E C O R D   L E N G T H S                                  00041700
041800                                                                  00041800
041900 01  WS-RECORD-LENGTHS.                                           00041900
042000*   05 WS-COMM-KEY-PNTR-LEN           PIC S9(4) COMP  VALUE +4.   00042000
042100*   05 GAS4UPD-COMMAREA-LEN           PIC S9(4) COMP  VALUE +420. 00042100
042200*   05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP  VALUE +100. 00042200
042300    05 WS-COPY-TABLE-LEN              PIC S9(4) COMP  VALUE +7744.00042300
042400    05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP  VALUE +0.   00042400
042500    05 WS-IO-PARM-WRK-BEN-PROV-LEN    PIC S9(4) COMP  VALUE +0.   00042500
042600    05 WS-IO-PARM-WRK-CONTRACT-LEN    PIC S9(4) COMP  VALUE +0.   00042600
042700    05 WS-IO-PARM-WRK-CONTROL-LEN     PIC S9(4) COMP  VALUE +0.   00042700
042800    05 WS-IO-PARM-WRK-GRP-SPEC-LEN    PIC S9(4) COMP  VALUE +0.   00042800
042900    05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP  VALUE +0.   00042900
043000    05 WS-WRK-BEN-PROV-LEN            PIC S9(4) COMP  VALUE +0.   00043000
043100    05 WS-WRK-CONTRACT-LEN            PIC S9(4) COMP  VALUE +0.   00043100
043200    05 WS-WRK-GRP-SPEC-LEN            PIC S9(4) COMP  VALUE +0.   00043200
043300                                                                  00043300
043400******************************************************************00043400
043500** REQUIRED FOR N118 - DISPLAY OF TABULAR OCCURS (INDEX)        **00043500
043600******************************************************************00043600
043700 01  CURNT-OCURS-BIN             PIC 9(4)  COMP.                  00043700
043800 01  CURNT-OCURS-PKD             PIC 9(4).                        00043800
043900 01  CURNT-OCURS-ALH      REDEFINES   CURNT-OCURS-PKD.            00043900
NSK24 *    05  FILLER                  PIC XX.                          00044000
NSK24 *    05  CURNT-OCCURS-OUT        PIC XX.                          00044100
NSK24      05  FILLER                  PIC X.                           00044110
NSK24      05  CURNT-OCCURS-OUT        PIC XXX.                         00044120
044200                                                                  00044200
044300 01  TOTAL-OCURS-UNK             PIC 9(5).                        00044300
044400 01  TOTAL-OCURS-ALH      REDEFINES   TOTAL-OCURS-UNK.            00044400
NSK24 *    05  FILLER                  PIC XXX.                         00044500
NSK24 *    05  TOTAL-OCCURS-OUT        PIC XX.                          00044600
NSK24      05  FILLER                  PIC XX.                          00044610
NSK24      05  TOTAL-OCCURS-OUT        PIC XXX.                         00044620
044700/                                                                 00044700
044800 01  WS-GC-RECORD-LENGTHS.                                        00044800
044900     COPY GCCDRLEN.                                               00044900
045000/    A B E N D   A R E A                                          00045000
045100                                                                  00045100
045200 01  WS-01-ABEND-AREA.                                            00045200
045300     05  FILLER                   PIC X(16)  VALUE                00045300
045400         '** ABEND AREA **'.                                      00045400
045500                                                                  00045500
045600     05  WS-ABCODE-CODES-AND-MSG.                                 00045600
045700         10  WS-ABCODE                  PIC X(04)  VALUE  SPACES. 00045700
045800         10  WS-ABCODE-MSG              PIC X(79)  VALUE  SPACES. 00045800
045900                                                                  00045900
046000         10  WS-ABCODE-1EC1             PIC X(04)  VALUE  '1EC1'. 00046000
046100         10  WS-ABCODE-1EC1-MSG         PIC X(79)  VALUE          00046100
046200             '*** INVALID PARAMETER LENGTH FOUND ***              00046200
046300-            '                           '.                       00046300
046400         10  WS-ABCODE-1EC2             PIC X(04)  VALUE  '1EC2'. 00046400
046500         10  WS-ABCODE-1EC2-MSG         PIC X(79)  VALUE          00046500
046600             '*** WRONG RECORD STATUS PASSED TO THIS PGM ***      00046600
046700-            '                           '.                       00046700
046800         10  WS-ABCODE-1EC3             PIC X(04)  VALUE  '1EC3'. 00046800
046900         10  WS-ABCODE-1EC3-MSG         PIC X(79)  VALUE          00046900
047000             '*** WRONG RECORD TYPE PASSED TO THIS PGM ***        00047000
047100-            '                           '.                       00047100
047200         10  WS-ABCODE-1EF1             PIC X(04)  VALUE  '1EF1'. 00047200
047300         10  WS-ABCODE-1EF1-MSG         PIC X(79)  VALUE          00047300
047400             '*** A SKELETON CAN NOT BE FOUND FOR AN INTERNAL TABU00047400
047500-            'LAR.  CONTACT SYSTEMS ***  '.                       00047500
047600         10  WS-ABCODE-1EF2             PIC X(04)  VALUE  '1EF2'. 00047600
047700         10  WS-ABCODE-1EF2-MSG         PIC X(79)  VALUE          00047700
047800             '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASE00047800
047900-            ' CONTACT SYSTEMS ***       '.                       00047900
048000         10  WS-ABCODE-1EF3             PIC X(04)  VALUE  '1EF3'. 00048000
048100         10  WS-ABCODE-1EF3-MSG         PIC X(79)  VALUE          00048100
048200             '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASE00048200
048300-            ' CONTACT SYSTEMS ***       '.                       00048300
048400         10  WS-ABCODE-1EF4             PIC X(04)  VALUE  '1EF4'. 00048400
048500         10  WS-ABCODE-1EF4-MSG         PIC X(79)  VALUE          00048500
048600             '*** ERROR REWRITING ALL LEVEL TABULAR.  PLEASE CONTA00048600
048700-            'CT SYSTEMS ***             '.                       00048700
048800         10  WS-ABCODE-1EF5             PIC X(04)  VALUE  '1EF5'. 00048800
048900         10  WS-ABCODE-1EF5-MSG         PIC X(79)  VALUE          00048900
049000             'THE INTERNAL TABULAR CAN NOT BE READ FROM THE WORKFI00049000
049100-            'LE.  PLEASE CONTACT SYSTEMS'.                       00049100
049200         10  WS-ABCODE-1EF6             PIC X(04)  VALUE  '1EF6'. 00049200
049300         10  WS-ABCODE-1EF6-MSG         PIC X(79)  VALUE          00049300
049400             'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORKFIL00049400
049500-            'E.  PLEASE CONTACT SYSTEMS '.                       00049500
049600         10  WS-ABCODE-1EF7             PIC X(04)  VALUE  '1EF7'. 00049600
049700         10  WS-ABCODE-1EF7-MSG         PIC X(79)  VALUE          00049700
049800             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00049800
049900-            ' SYSTEMS ***               '.                       00049900
050000         10  WS-ABCODE-1EF8             PIC X(04)  VALUE  '1EF8'. 00050000
050100         10  WS-ABCODE-1EF8-MSG         PIC X(79)  VALUE          00050100
050200             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00050200
050300-            ' SYSTEMS ***               '.                       00050300
050400         10  WS-ABCODE-1EF9             PIC X(04)  VALUE  '1EF9'. 00050400
050500         10  WS-ABCODE-1EF9-MSG         PIC X(79)  VALUE          00050500
050600             '*** ERROR READING GROUP SPECIFIC RECORD TO RETURN TO00050600
050700-            'MENU.  CONTACT SYSTEMS *** '.                       00050700
050800         10  WS-ABCODE-1EFA             PIC X(04)  VALUE  '1EFA'. 00050800
050900         10  WS-ABCODE-1EFA-MSG         PIC X(79)  VALUE          00050900
051000             '*** ERROR READING CONTRACT MASTER TO RETURN TO THE  00051000
051100-            'MENU.  CONTACT SYSTEMS *** '.                       00051100
051200         10  WS-ABCODE-1EFB             PIC X(04)  VALUE  '1EFB'. 00051200
051300         10  WS-ABCODE-1EFB-MSG         PIC X(79)  VALUE          00051300
051400             '*** ERROR READING BENEFIT PROV RECORD TO RETURN TO M00051400
051500-            'ENU.  CONTACT SYSTEMS ***  '.                       00051500
051600         10  WS-ABCODE-1EL1             PIC X(04)  VALUE  '1EL1'. 00051600
051700         10  WS-ABCODE-1EL1-MSG         PIC X(79)  VALUE          00051700
051800             '*** THE OCCURS WE ARE TO UPDATE HAS BEEN DELETED ***00051800
051900-            '                           '.                       00051900
052000         10  WS-ABCODE-1EP1             PIC X(04)  VALUE  '1EP1'. 00052000
052100         10  WS-ABCODE-1EP1-MSG         PIC X(79)  VALUE          00052100
052200             '????????????????????????????????????????????????????00052200
052300-            '???????????????????????????'.                       00052300
052400                                                                  00052400
052500/    M E S S A G E   T A B L E                                    00052500
052600******************************************************************00052600
052700 01  WT-01-TABLE.                                                 00052700
052800     05  FILLER                  PIC X(16) VALUE                  00052800
052900         '* WT-01-TABLE  *'.                                      00052900
053000                                                                  00053000
053100 01  FILLER.                                                      00053100
053200     05  WT-01-MESSAGE-VALUES.                                    00053200
053300*----------------------------------------------------------------*00053300
053400         10  WT-01-ENTRY-001.                                     00053400
053500             15  FILLER              PIC X(2)  VALUE '¬>'.        00053500
053600             15  WT-01-MESSAGE-TEXT-001.                          00053600
053700                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00053700
053800                 20  FILLER          PIC X(1)  VALUE  '-'.        00053800
053900                 20  FILLER          PIC X(3)  VALUE  '001'.      00053900
054000                 20  FILLER          PIC X(1)  VALUE  ' '.        00054000
054100                 20  FILLER          PIC X(70) VALUE              00054100
054200                     '#IBGR HAS BEEN SUCCESSFULLY MAPPED          00054200
054300-                    '                         '.                 00054300
054400             15  FILLER              PIC X(2)  VALUE '<¬'.        00054400
054500*----------------------------------------------------------------*00054500
054600         10  WT-01-ENTRY-002.                                     00054600
054700             15  FILLER              PIC X(2)  VALUE '¬>'.        00054700
054800             15  WT-01-MESSAGE-TEXT-002.                          00054800
054900                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00054900
055000                 20  FILLER          PIC X(1)  VALUE  '-'.        00055000
055100                 20  FILLER          PIC X(3)  VALUE  '002'.      00055100
055200                 20  FILLER          PIC X(1)  VALUE  ' '.        00055200
055300                 20  FILLER          PIC X(70) VALUE              00055300
055400                     '#IPGN HAS BEEN SUCCESSFULLY MAPPED          00055400
055500-                    '                         '.                 00055500
055600             15  FILLER              PIC X(2)  VALUE '<¬'.        00055600
055700*----------------------------------------------------------------*00055700
055800         10  WT-01-ENTRY-003.                                     00055800
055900             15  FILLER              PIC X(2)  VALUE '¬>'.        00055900
056000             15  WT-01-MESSAGE-TEXT-003.                          00056000
056100                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00056100
056200                 20  FILLER          PIC X(1)  VALUE  '-'.        00056200
056300                 20  FILLER          PIC X(3)  VALUE  '003'.      00056300
056400                 20  FILLER          PIC X(1)  VALUE  ' '.        00056400
056500                 20  FILLER          PIC X(70) VALUE              00056500
056600                     '#IPGT HAS BEEN SUCCESSFULLY MAPPED          00056600
056700-                    '                         '.                 00056700
056800             15  FILLER              PIC X(2)  VALUE '<¬'.        00056800
056900*----------------------------------------------------------------*00056900
057000         10  WT-01-ENTRY-004.                                     00057000
057100             15  FILLER              PIC X(2)  VALUE '¬>'.        00057100
057200             15  WT-01-MESSAGE-TEXT-004.                          00057200
057300                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00057300
057400                 20  FILLER          PIC X(1)  VALUE  '-'.        00057400
057500                 20  FILLER          PIC X(3)  VALUE  '004'.      00057500
057600                 20  FILLER          PIC X(1)  VALUE  ' '.        00057600
057700                 20  FILLER          PIC X(70) VALUE              00057700
057800                     '#IDGD HAS BEEN SUCCESSFULLY MAPPED          00057800
057900-                    '                         '.                 00057900
058000             15  FILLER              PIC X(2)  VALUE '<¬'.        00058000
058100*----------------------------------------------------------------*00058100
058200         10  WT-01-ENTRY-005.                                     00058200
058300             15  FILLER              PIC X(2)  VALUE '¬>'.        00058300
058400             15  WT-01-MESSAGE-TEXT-005.                          00058400
058500                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00058500
058600                 20  FILLER          PIC X(1)  VALUE  '-'.        00058600
058700                 20  FILLER          PIC X(3)  VALUE  '005'.      00058700
058800                 20  FILLER          PIC X(1)  VALUE  ' '.        00058800
058900                 20  FILLER          PIC X(70) VALUE              00058900
059000                     '#IPGP HAS BEEN SUCCESSFULLY MAPPED          00059000
059100-                    '                         '.                 00059100
059200             15  FILLER              PIC X(2)  VALUE '<¬'.        00059200
059300*----------------------------------------------------------------*00059300
059400         10  WT-01-ENTRY-006.                                     00059400
059500             15  FILLER              PIC X(2)  VALUE '¬>'.        00059500
059600             15  WT-01-MESSAGE-TEXT-006.                          00059600
059700                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00059700
059800                 20  FILLER          PIC X(1)  VALUE  '-'.        00059800
059900                 20  FILLER          PIC X(3)  VALUE  '006'.      00059900
060000                 20  FILLER          PIC X(1)  VALUE  ' '.        00060000
060100                 20  FILLER          PIC X(70) VALUE              00060100
060200                     'DELETE OPTION MUST BE \
060300-                    'VALID                    '.                 00060300
060400             15  FILLER              PIC X(2)  VALUE '<¬'.        00060400
060500*----------------------------------------------------------------*00060500
060600         10  WT-01-ENTRY-007.                                     00060600
060700             15  FILLER              PIC X(2)  VALUE '¬>'.        00060700
060800             15  WT-01-MESSAGE-TEXT-007.                          00060800
060900                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00060900
061000                 20  FILLER          PIC X(1)  VALUE  '-'.        00061000
061100                 20  FILLER          PIC X(3)  VALUE  '007'.      00061100
061200                 20  FILLER          PIC X(1)  VALUE  ' '.        00061200
061300                 20  FILLER          PIC X(70) VALUE              00061300
061400                     'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESS00061400
061500-                    ' PF4/PF16 TO CONTINUE    '.                 00061500
061600             15  FILLER              PIC X(2)  VALUE '<¬'.        00061600
061700*----------------------------------------------------------------*00061700
061800         10  WT-01-ENTRY-008.                                     00061800
061900             15  FILLER              PIC X(2)  VALUE '¬>'.        00061900
062000             15  WT-01-MESSAGE-TEXT-008.                          00062000
062100                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00062100
062200                 20  FILLER          PIC X(1)  VALUE  '-'.        00062200
062300                 20  FILLER          PIC X(3)  VALUE  '008'.      00062300
062400                 20  FILLER          PIC X(1)  VALUE  ' '.        00062400
062500                 20  FILLER          PIC X(70) VALUE              00062500
062600                     'GROUP IN CONVERSION STATUS, CANNOT CHANGE HI00062600
062700-                    'GH-LIGHTED ELEMENTS      '.                 00062700
062800             15  FILLER              PIC X(2)  VALUE '<¬'.        00062800
062900*----------------------------------------------------------------*00062900
063000         10  WT-01-ENTRY-009.                                     00063000
063100             15  FILLER              PIC X(2)  VALUE '¬>'.        00063100
063200             15  WT-01-MESSAGE-TEXT-009.                          00063200
063300                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00063300
063400                 20  FILLER          PIC X(1)  VALUE  '-'.        00063400
063500                 20  FILLER          PIC X(3)  VALUE  '009'.      00063500
063600                 20  FILLER          PIC X(1)  VALUE  ' '.        00063600
063700                 20  FILLER          PIC X(70) VALUE              00063700
063800                     'INVALID PFKEY SELECTION                     00063800
063900-                    '                         '.                 00063900
064000             15  FILLER              PIC X(2)  VALUE '<¬'.        00064000
064100*----------------------------------------------------------------*00064100
064200         10  WT-01-ENTRY-010.                                     00064200
064300             15  FILLER              PIC X(2)  VALUE '¬>'.        00064300
064400             15  WT-01-MESSAGE-TEXT-010.                          00064400
064500                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00064500
064600                 20  FILLER          PIC X(1)  VALUE  '-'.        00064600
064700                 20  FILLER          PIC X(3)  VALUE  '010'.      00064700
064800                 20  FILLER          PIC X(1)  VALUE  ' '.        00064800
064900                 20  FILLER          PIC X(70) VALUE              00064900
065000                     'INVALID REQUEST.  THAT PF KEY HAS NO MEANING00065000
065100-                    ' TO THIS PROGRAM         '.                 00065100
065200             15  FILLER              PIC X(2)  VALUE '<¬'.        00065200
065300*----------------------------------------------------------------*00065300
065400         10  WT-01-ENTRY-011.                                     00065400
065500             15  FILLER              PIC X(2)  VALUE '¬>'.        00065500
065600             15  WT-01-MESSAGE-TEXT-011.                          00065600
065700                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00065700
065800                 20  FILLER          PIC X(1)  VALUE  '-'.        00065800
065900                 20  FILLER          PIC X(3)  VALUE  '011'.      00065900
066000                 20  FILLER          PIC X(1)  VALUE  ' '.        00066000
066100                 20  FILLER          PIC X(70) VALUE              00066100
066200                     'NO CHANGE FOUND - NO CHANGE MADE, WHAT NEXT 00066200
066300-                    '                         '.                 00066300
066400             15  FILLER              PIC X(2)  VALUE '<¬'.        00066400
066500*----------------------------------------------------------------*00066500
066600         10  WT-01-ENTRY-012.                                     00066600
066700             15  FILLER              PIC X(2)  VALUE '¬>'.        00066700
066800             15  WT-01-MESSAGE-TEXT-012.                          00066800
066900                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00066900
067000                 20  FILLER          PIC X(1)  VALUE  '-'.        00067000
067100                 20  FILLER          PIC X(3)  VALUE  '012'.      00067100
067200                 20  FILLER          PIC X(1)  VALUE  ' '.        00067200
067300                 20  FILLER          PIC X(70) VALUE              00067300
067400                     'NO ENTRIES TO DISPLAY                       00067400
067500-                    '                         '.                 00067500
067600             15  FILLER              PIC X(2)  VALUE '<¬'.        00067600
067700*----------------------------------------------------------------*00067700
067800         10  WT-01-ENTRY-013.                                     00067800
067900             15  FILLER              PIC X(2)  VALUE '¬>'.        00067900
068000             15  WT-01-MESSAGE-TEXT-013.                          00068000
068100                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00068100
068200                 20  FILLER          PIC X(1)  VALUE  '-'.        00068200
068300                 20  FILLER          PIC X(3)  VALUE  '013'.      00068300
068400                 20  FILLER          PIC X(1)  VALUE  ' '.        00068400
068500                 20  FILLER          PIC X(70) VALUE              00068500
068600                     'PFKEY INVALID WHILE ERRORS NOT CORRECTED, HI00068600
068700-                    'T ENTER FOR ERR MSG      '.                 00068700
068800             15  FILLER              PIC X(2)  VALUE '<¬'.        00068800
068900*----------------------------------------------------------------*00068900
069000         10  WT-01-ENTRY-014.                                     00069000
069100             15  FILLER              PIC X(2)  VALUE '¬>'.        00069100
069200             15  WT-01-MESSAGE-TEXT-014.                          00069200
069300                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00069300
069400                 20  FILLER          PIC X(1)  VALUE  '-'.        00069400
069500                 20  FILLER          PIC X(3)  VALUE  '014'.      00069500
069600                 20  FILLER          PIC X(1)  VALUE  ' '.        00069600
069700                 20  FILLER          PIC X(70) VALUE              00069700
069800                     'PROCESSING FROM THE TOP OF THE LIST         00069800
069900-                    '                         '.                 00069900
070000             15  FILLER              PIC X(2)  VALUE '<¬'.        00070000
070100*----------------------------------------------------------------*00070100
070200         10  WT-01-ENTRY-015.                                     00070200
070300             15  FILLER              PIC X(2)  VALUE '¬>'.        00070300
070400             15  WT-01-MESSAGE-TEXT-015.                          00070400
070500                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00070500
070600                 20  FILLER          PIC X(1)  VALUE  '-'.        00070600
070700                 20  FILLER          PIC X(3)  VALUE  '015'.      00070700
070800                 20  FILLER          PIC X(1)  VALUE  ' '.        00070800
070900                 20  FILLER          PIC X(70) VALUE              00070900
071000                     'THE MAXIMUM NUMBER OF ENTRIES HAVE BEEN ADDE00071000
071100-                    'D                        '.                 00071100
071200             15  FILLER              PIC X(2)  VALUE '<¬'.        00071200
071300*----------------------------------------------------------------*00071300
071400         10  WT-01-ENTRY-016.                                     00071400
071500             15  FILLER              PIC X(2)  VALUE '¬>'.        00071500
071600             15  WT-01-MESSAGE-TEXT-016.                          00071600
071700                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00071700
071800                 20  FILLER          PIC X(1)  VALUE  '-'.        00071800
071900                 20  FILLER          PIC X(3)  VALUE  '016'.      00071900
072000                 20  FILLER          PIC X(1)  VALUE  ' '.        00072000
072100                 20  FILLER          PIC X(70) VALUE              00072100
072200                     'THE TABULAR ALREADY CONTAINS THE MAXIMUM NUM00072200
072300-                    'BER OF OCCURANCES        '.                 00072300
072400             15  FILLER              PIC X(2)  VALUE '<¬'.        00072400
072500*----------------------------------------------------------------*00072500
072600         10  WT-01-ENTRY-017.                                     00072600
072700             15  FILLER              PIC X(2)  VALUE '¬>'.        00072700
072800             15  WT-01-MESSAGE-TEXT-017.                          00072800
072900                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00072900
073000                 20  FILLER          PIC X(1)  VALUE  '-'.        00073000
073100                 20  FILLER          PIC X(3)  VALUE  '017'.      00073100
073200                 20  FILLER          PIC X(1)  VALUE  ' '.        00073200
073300                 20  FILLER          PIC X(70) VALUE              00073300
073400                     'THE TABULAR RECORD DOES NOT EXIST, AND CANNO00073400
073500-                    'T BE CHANGED             '.                 00073500
073600             15  FILLER              PIC X(2)  VALUE '<¬'.        00073600
073700*----------------------------------------------------------------*00073700
073800         10  WT-01-ENTRY-018.                                     00073800
073900             15  FILLER              PIC X(2)  VALUE '¬>'.        00073900
074000             15  WT-01-MESSAGE-TEXT-018.                          00074000
074100                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00074100
074200                 20  FILLER          PIC X(1)  VALUE  '-'.        00074200
074300                 20  FILLER          PIC X(3)  VALUE  '018'.      00074300
074400                 20  FILLER          PIC X(1)  VALUE  ' '.        00074400
074500                 20  FILLER          PIC X(70) VALUE              00074500
074600                     'THE TABULAR RECORD DOES NOT EXIST, AND CANNO00074600
074700-                    'T BE MAPPED              '.                 00074700
074800             15  FILLER              PIC X(2)  VALUE '<¬'.        00074800
074900*----------------------------------------------------------------*00074900
075000         10  WT-01-ENTRY-019.                                     00075000
075100             15  FILLER              PIC X(2)  VALUE '¬>'.        00075100
075200             15  WT-01-MESSAGE-TEXT-019.                          00075200
075300                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00075300
075400                 20  FILLER          PIC X(1)  VALUE  '-'.        00075400
075500                 20  FILLER          PIC X(3)  VALUE  '019'.      00075500
075600                 20  FILLER          PIC X(1)  VALUE  ' '.        00075600
075700                 20  FILLER          PIC X(70) VALUE              00075700
075800                     'THERE ARE NO MORE ENTRIES TO DISPLAY        00075800
075900-                    '                         '.                 00075900
076000             15  FILLER              PIC X(2)  VALUE '<¬'.        00076000
076100*----------------------------------------------------------------*00076100
076200         10  WT-01-ENTRY-020.                                     00076200
076300             15  FILLER              PIC X(2)  VALUE '¬>'.        00076300
076400             15  WT-01-MESSAGE-TEXT-020.                          00076400
076500                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00076500
076600                 20  FILLER          PIC X(1)  VALUE  '-'.        00076600
076700                 20  FILLER          PIC X(3)  VALUE  '020'.      00076700
076800                 20  FILLER          PIC X(1)  VALUE  ' '.        00076800
076900                 20  FILLER          PIC X(70) VALUE              00076900
077000                     'THIS IS THE FIRST ON THE TABLE              00077000
077100-                    '                         '.                 00077100
077200             15  FILLER              PIC X(2)  VALUE '<¬'.        00077200
077300*----------------------------------------------------------------*00077300
077400         10  WT-01-ENTRY-021.                                     00077400
077500             15  FILLER              PIC X(2)  VALUE '¬>'.        00077500
077600             15  WT-01-MESSAGE-TEXT-021.                          00077600
077700                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00077700
077800                 20  FILLER          PIC X(1)  VALUE  '-'.        00077800
077900                 20  FILLER          PIC X(3)  VALUE  '021'.      00077900
078000                 20  FILLER          PIC X(1)  VALUE  ' '.        00078000
078100                 20  FILLER          PIC X(70) VALUE              00078100
078200                     'THIS IS THE LAST ON THE TABLE               00078200
078300-                    '                         '.                 00078300
078400             15  FILLER              PIC X(2)  VALUE '<¬'.        00078400
078500*----------------------------------------------------------------*00078500
078600         10  WT-01-ENTRY-022.                                     00078600
078700             15  FILLER              PIC X(2)  VALUE '¬>'.        00078700
078800             15  WT-01-MESSAGE-TEXT-022.                          00078800
078900                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00078900
079000                 20  FILLER          PIC X(1)  VALUE  '-'.        00079000
079100                 20  FILLER          PIC X(3)  VALUE  '022'.      00079100
079200                 20  FILLER          PIC X(1)  VALUE  ' '.        00079200
079300                 20  FILLER          PIC X(70) VALUE              00079300
079400                     'THIS PFKEY NOT VALID WHILE IN CHG/ADD MODE  00079400
079500-                    '                         '.                 00079500
079600             15  FILLER              PIC X(2)  VALUE '<¬'.        00079600
079700*----------------------------------------------------------------*00079700
079800         10  WT-01-ENTRY-023.                                     00079800
079900             15  FILLER              PIC X(2)  VALUE '¬>'.        00079900
080000             15  WT-01-MESSAGE-TEXT-023.                          00080000
080100                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00080100
080200                 20  FILLER          PIC X(1)  VALUE  '-'.        00080200
080300                 20  FILLER          PIC X(3)  VALUE  '023'.      00080300
080400                 20  FILLER          PIC X(1)  VALUE  ' '.        00080400
080500                 20  FILLER          PIC X(61) VALUE              00080500
080600                     'INVALID PERCENT DETECTED. CHECK OCCURS WHERE00080600
080700-                    ' ENTRY COUNTER = '.                         00080700
080800                 20  WT-01-ENT-CTR   PIC X(7)  VALUE  SPACES.     00080800
080900                 20  FILLER          PIC X(2)  VALUE  '. '.       00080900
081000             15  FILLER              PIC X(2)  VALUE '<¬'.        00081000
081100*----------------------------------------------------------------*00081100
081200         10  WT-01-ENTRY-024.                                     00081200
081300             15  FILLER              PIC X(2)  VALUE '¬>'.        00081300
081400             15  WT-01-MESSAGE-TEXT-003.                          00081400
081500                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00081500
081600                 20  FILLER          PIC X(1)  VALUE  '-'.        00081600
081700                 20  FILLER          PIC X(3)  VALUE  '024'.      00081700
081800                 20  FILLER          PIC X(1)  VALUE  ' '.        00081800
081900                 20  FILLER          PIC X(70) VALUE              00081900
082000                     '#IPGS HAS BEEN SUCCESSFULLY MAPPED          00082000
082100-                    '                         '.                 00082100
082200             15  FILLER              PIC X(2)  VALUE '<¬'.        00082200
082300*----------------------------------------------------------------*00082300
082400         10  WT-01-ENTRY-025.                                     00082400
082500             15  FILLER              PIC X(2)  VALUE '¬>'.        00082500
082600             15  WT-01-MESSAGE-TEXT-024.                          00082600
082700                 20  FILLER          PIC X(4)  VALUE  'GA1E'.     00082700
082800                 20  FILLER          PIC X(1)  VALUE  '-'.        00082800
082900                 20  FILLER          PIC X(3)  VALUE  '025'.      00082900
083000                 20  FILLER          PIC X(1)  VALUE  ' '.        00083000
083100                 20  FILLER          PIC X(70) VALUE              00083100
083200                     '********** F U T U R E   U S E *************00083200
083300-                    '*************************'.                 00083300
083400             15  FILLER              PIC X(2)  VALUE '<¬'.        00083400
083500*----------------------------------------------------------------*00083500
083600                                                                  00083600
083700     05  WT-01-MESSAGE-TABLE         REDEFINES                    00083700
083800         WT-01-MESSAGE-VALUES         OCCURS 025 TIMES            00083800
083900                                     INDEXED BY WT-01-INDEX.      00083900
084000         10  WT-01-ENTRY.                                         00084000
084100             15  FILLER              PIC X(02).                   00084100
084200             15  WT-01-MESSAGE-TEXT  PIC X(79).                   00084200
084300             15  FILLER              PIC X(02).                   00084300
084400                                                                  00084400
084500 01  WS-END                      PIC X(16)  VALUE                 00084500
084600     '*** W/S ENDS ***'.                                          00084600
084700/    L I N K A G E   S E C T I O N                                00084700
084800 LINKAGE SECTION.                                                 00084800
084900 01  DFHCOMMAREA.                                                 00084900
085000 COPY G2ALCKEC.                                                   00085000
085100*    05  COMMAREA-RECORD-POINTER  USAGE IS POINTER.               00085100
085200 COPY GACDACWB.                                                   00085200
085300                                                                  00085300
085400     05  GAS4UPD-PASSED-AREA-2.                                   00085400
085500         07  LVL2-B-SW-2              PIC X.                      00085500
085600         07  LVL2-F-SW-2              PIC X.                      00085600
085700         07  LVL2-G-SW-2              PIC X.                      00085700
085800         07  INTR-TAB-PGM-ID-2        PIC X(8).                   00085800
085900         07  FILLER-2                 PIC X(09).                  00085900
086000     05  DELADD-OPTION-2              PIC X(7).                   00086000
086100                                                                  00086100
086200                                                                  00086200
086300/*****************************************************************00086300
086400* W O R K F I L E   -   A L L   L E V E L   T A B   R E C O R D   00086400
086500******************************************************************00086500
086600 01  WF-IO-PARM-ALL-LVL-TAB-RECORD.                               00086600
086700 COPY GCIOPRM1.                                                   00086700
086800/                                                                 00086800
086900 COPY GCWRKDCC.                                                   00086900
087000/                                                                 00087000
087100 COPY GCTAOLC.                                                    00087100
087200/    C O M M U N I C A T I O N    K E Y   A R E A                 00087200
087300*01  COMMUNICATION-KEY-AREA.                                      00087300
087400*COPY G2ALCKEC.                                                   00087400
087500                                                                  00087500
087600/    C O P Y   T A B U L A R   T A B L E   A R E A                00087600
087700 01  COPY-TABULAR-TABLE-AREA.                                     00087700
087800     05  COPY-TABULAR-TABLE  OCCURS 175 TIMES INDEXED BY          00087800
087900         COPY-IDX, COPY-IDX2, COPY-IDX3, COPY-IDX4.               00087900
088000       10  COPY-SORTABLE-FLDS.                                    00088000
088100           15  FILLER                      PIC X(169).            00088100
088200           15  COPY-SORT-FYI               PIC X(003).            00088200
088300       10  COPY-SORT-ENTRY-CNTR            PIC S9(7) COMP-3.      00088300
088400                                                                  00088400
088500/*****************************************************************00088500
088600* W O R K F I L E   -   I N T E R N A L   T A B U L A R   R E C   00088600
088700******************************************************************00088700
088800 01  WF-IO-PARM-INTERNAL-TAB-RECORD.                              00088800
088900 COPY GCIOPRM2.                                                   00088900
089000/                                                                 00089000
089100 COPY GCWRKDC2.                                                   00089100
089200/                                                                 00089200
089300 COPY GCTIPGPC.                                                   00089300
089400/*****************************************************************00089400
089500* W O R K F I L E   -   G R O U P   S P E C I F I C   R E C       00089500
089600******************************************************************00089600
089700 01  WF-IO-PARM-WRK-GRP-SPEC-REC.                                 00089700
089800 COPY GCIOPRM3.                                                   00089800
089900/                                                                 00089900
090000 COPY GCWRKDC3.                                                   00090000
090100/                                                                 00090100
090200 COPY GCGROUPC.                                                   00090200
090300/*****************************************************************00090300
090400* W O R K F I L E   -   C O N T R A C T   R E C O R D             00090400
090500******************************************************************00090500
090600 01  WF-IO-PARM-WRK-CONTRACT-REC.                                 00090600
090700 COPY GCIOPRM4.                                                   00090700
090800/                                                                 00090800
090900 COPY GCWRKDC4.                                                   00090900
091000/                                                                 00091000
091100 COPY GCCONTRC.                                                   00091100
091200/*****************************************************************00091200
091300* W O R K F I L E   -   B E N E F I T   P R O V I S I O N   R E C 00091300
091400******************************************************************00091400
091500 01  WF-IO-PARM-WRK-BEN-PROV-REC.                                 00091500
091600 COPY GCIOPRM5.                                                   00091600
091700/                                                                 00091700
091800 COPY GCWRKDC5.                                                   00091800
091900/                                                                 00091900
092000 COPY GCBENPVC.                                                   00092000
092100/*****************************************************************00092100
092200* W O R K F I L E   -  C O N T R O L   R E C O R D                00092200
092300******************************************************************00092300
092400 01  WF-IO-PARM-WRK-CONTROL-REC.                                  00092400
092500 COPY GCIOPRM6.                                                   00092500
092600/                                                                 00092600
092700 COPY GCWRKDC6.                                                   00092700
092800/                                                                 00092800
092900 COPY GCCCRDCC.                                                   00092900
093000/*****************************************************************00093000
093100* P R O D U C T I O N   -   C O N T R A C T   R E C O R D         00093100
093200******************************************************************00093200
093300 01  PR-IO-PARM-WRK-CONTRACT-REC.                                 00093300
093400 COPY GCIOPRM7   SUPPRESS.                                        00093400
093500                                                                  00093500
093600 COPY GCWRKDC7   SUPPRESS.                                        00093600
093700                                                                  00093700
093800 COPY GCCONTR2   SUPPRESS.                                        00093800
093900******************************************************************00093900
094000* P R O D U C T I O N   -   G R O U P   S P E C I F I C   R E C   00094000
094100******************************************************************00094100
094200 01  PR-IO-PARM-WRK-GRP-SPEC-REC.                                 00094200
094300 COPY GCIOPRM8   SUPPRESS.                                        00094300
094400                                                                  00094400
094500 COPY GCWRKDC8   SUPPRESS.                                        00094500
094600                                                                  00094600
094700 COPY GCGROUP2   SUPPRESS.                                        00094700
094800******************************************************************00094800
094900* P R O D U C T I O N   -   B E N E F I T   P V S N   R E C O R D 00094900
095000******************************************************************00095000
095100 01  PR-IO-PARM-WRK-BEN-PROV-REC.                                 00095100
095200 COPY GCIOPRM9   SUPPRESS.                                        00095200
095300                                                                  00095300
095400 COPY GCWRKDC9   SUPPRESS.                                        00095400
095500                                                                  00095500
095600 COPY GCBENPV2   SUPPRESS.                                        00095600
095700******************************************************************00095700
095800* P R O D U C T I O N   -   A L L   L E V E L   T A B   R E C     00095800
095900******************************************************************00095900
096000 01  PR-IO-PARM-ALL-LVL-TAB-RECORD.                               00096000
096100 COPY GCIOPRMA   SUPPRESS.                                        00096100
096200                                                                  00096200
096300 COPY GCWRKDCA   SUPPRESS.                                        00096300
096400                                                                  00096400
096500 COPY GCTAOL2    SUPPRESS.                                        00096500
096600                                                                  00096600
096700/*****************************************************************00096700
096800*    M A P S E T   A R E A                                        00096800
096900******************************************************************00096900
097000     COPY GA1XSETC.                                               00097000
097100                                                                  00097100
097200/*****************************************************************00097200
097300*     A L L   L V L   A C C U M   C O M M O N   W O R K A R E A S 00097300
097400******************************************************************00097400
097500*  *** UPDATE/DELETE MODULE GAS4UPD COMMAREA ***                  00097500
097600*  *** WILL BE THE SAME COMMON WORK AREA + GAS4UPD COMMAREA ***   00097600
097700 01  COMMON-WORKAREAS.                                            00097700
097800 COPY G2ALCKE2.                                                   00097800
097900 COPY GACDACWA.                                                   00097900
098000                                                                  00098000
098100     05  GAS4UPD-PASSED-AREA.                                     00098100
098200         07  LVL2-B-SW                PIC X.                      00098200
098300         07  LVL2-F-SW                PIC X.                      00098300
098400         07  LVL2-G-SW                PIC X.                      00098400
098500         07  INTR-TAB-PGM-ID          PIC X(8).                   00098500
098600         07  FILLER                   PIC X(09).                  00098600
098700     05  DELADD-OPTION                PIC X(7).                   00098700
098800                                                                  00098800
098900/    P R O C E D U R E   D I V I S I O N                          00098900
099000 PROCEDURE DIVISION.                                              00099000
099100                                                                  00099100
099200******************************************************************00099200
099300* 0000  HOUSEKEEPING                                             *00099300
099400******************************************************************00099400
099500 0000-000-HOUSEKEEPING          SECTION.                          00099500
099600 0000-010.                                                        00099600
099700                                                                  00099700
099800     EXEC CICS GETMAIN                                            00099800
099900               SET(ADDRESS OF COMMON-WORKAREAS)                   00099900
100000               INITIMG(WS-HEX-00)                                 00100000
100100               LENGTH(LENGTH OF COMMON-WORKAREAS)                 00100100
100200               END-EXEC.                                          00100200
100300                                                                  00100300
100400     MOVE ZEROES  TO  ACWA-CDE-1U-COUNT,  ACWA-CDE-2B-COUNT.      00100400
100500                                                                  00100500
100600     EXEC CICS GETMAIN                                            00100600
100700               SET(ADDRESS OF GA1XI01I)                           00100700
100800               INITIMG(WS-HEX-00)                                 00100800
100900               LENGTH(LENGTH OF GA1XI01I)                         00100900
101000               END-EXEC.                                          00101000
101100                                                                  00101100
101200     SET ACWA-MAPSET-PNTR  TO  ADDRESS OF  GA1XI01I.              00101200
101300                                                                  00101300
101400     MOVE  +19   TO  GCVI-COMMAREA-LEN.                           00101400
101500     MOVE  'N'   TO  ACWA-ERROR-SW                                00101500
101600                     ACWA-CDE-FIELD-CHANGE-IND                    00101600
101700                     ACWA-CDE-REC-CHANGE-IND                      00101700
101800                     ACWA-CDE-RESET-WF-IND.                       00101800
101900     MOVE  ZERO  TO  ACWA-FIELD-CHG-CNT.                          00101900
102000     MOVE  SPACE TO  ACWA-CDE-STATUS-CHANGE-IND                   00102000
102100                     ACWA-CDE-INTERNAL-TAB-IND.                   00102100
102200                                                                  00102200
102300     COMPUTE WS-IO-PARM-WRK-GRP-SPEC-LEN =                        00102300
102400             GC-GCIOPARM-LEN             +                        00102400
102500             GC-WORKFILE-KEY-LEN         +                        00102500
102600             GC-GCGRPSPC-FIXED-LEN       +                        00102600
102700            (GC-GCGRPSPC-VARY-LEN        *                        00102700
102800             GC-GCGRPSPC-VARY-MAX-OCUR).                          00102800
102900                                                                  00102900
103000     COMPUTE WS-WRK-GRP-SPEC-LEN         =                        00103000
103100             GC-WORKFILE-KEY-LEN         +                        00103100
103200             GC-GCGRPSPC-FIXED-LEN       +                        00103200
103300            (GC-GCGRPSPC-VARY-LEN        *                        00103300
103400             GC-GCGRPSPC-VARY-MAX-OCUR).                          00103400
103500                                                                  00103500
103600     COMPUTE WS-IO-PARM-WRK-CONTRACT-LEN =                        00103600
103700             GC-GCIOPARM-LEN             +                        00103700
103800             GC-WORKFILE-KEY-LEN         +                        00103800
103900             GC-GCCONTR-FIXED-LEN        +                        00103900
104000            (GC-GCCONTR-VARY-LEN         *                        00104000
104100             GC-GCCONTR-VARY-MAX-OCUR).                           00104100
104200                                                                  00104200
104300     COMPUTE WS-WRK-CONTRACT-LEN         =                        00104300
104400             GC-WORKFILE-KEY-LEN         +                        00104400
104500             GC-GCCONTR-FIXED-LEN        +                        00104500
104600            (GC-GCCONTR-VARY-LEN         *                        00104600
104700             GC-GCCONTR-VARY-MAX-OCUR).                           00104700
104800                                                                  00104800
104900     COMPUTE WS-IO-PARM-WRK-BEN-PROV-LEN =                        00104900
105000             GC-GCIOPARM-LEN             +                        00105000
105100             GC-WORKFILE-KEY-LEN         +                        00105100
105200             GC-GCBENPRV-FIXED-LEN       +                        00105200
105300            (GC-GCBENPRV-VARY-LEN        *                        00105300
105400             GC-GCBENPRV-VARY-MAX-OCUR).                          00105400
105500                                                                  00105500
105600     COMPUTE WS-WRK-BEN-PROV-LEN         =                        00105600
105700             GC-WORKFILE-KEY-LEN         +                        00105700
105800             GC-GCBENPRV-FIXED-LEN       +                        00105800
105900            (GC-GCBENPRV-VARY-LEN        *                        00105900
106000             GC-GCBENPRV-VARY-MAX-OCUR).                          00106000
106100                                                                  00106100
106200     COMPUTE WS-IO-PARM-WRK-CONTROL-LEN  =                        00106200
106300             GC-GCIOPARM-LEN             +                        00106300
106400             GC-WORKFILE-KEY-LEN         +                        00106400
106500             GC-WORKFILE-CONTROL-REC-LEN.                         00106500
106600                                                                  00106600
106700     IF EIBAID  =  DFHCLEAR                                       00106700
106800         EXEC CICS SEND FROM(WS-ONE-LOW)                          00106800
106900                        ERASE                                     00106900
107000         END-EXEC                                                 00107000
107100         EXEC CICS RETURN                                         00107100
107200         END-EXEC.                                                00107200
107300                                                                  00107300
107400     EXEC CICS  HANDLE  CONDITION                                 00107400
107500                MAPFAIL(6400-000-XCTL-TO-MAIN-MENU)  END-EXEC.    00107500
107600                                                                  00107600
107700 0000-900-EXIT.                                                   00107700
107800          EXIT.                                                   00107800
107900/*****************************************************************00107900
108000* 1000  MAIN LINE                                                *00108000
108100******************************************************************00108100
108200 1000-000-MAIN-LINE             SECTION.                          00108200
108300 1000-010.                                                        00108300
108400                                                                  00108400
108500     IF  EIBTRNID  NOT =  'GA1E'                                  00108500
108600         PERFORM 4000-000-DISPLAY-FIRST-SCREEN.                   00108600
108700                                                                  00108700
108800     EXEC CICS  RECEIVE   MAP('GA1XI01')  MAPSET('GA1XSET')       00108800
108900                END-EXEC.                                         00108900
109000                                                                  00109000
109100     IF  SCRNIDNI  NOT =  '001E00'                                00109100
109200         PERFORM 6400-000-XCTL-TO-MAIN-MENU.                      00109200
109300                                                                  00109300
109400     IF  FRMNUIDI = 'GS3A'                                        00109400
109500         MOVE IDLINEI   TO   GROUP-SPECIFIC-ID-LINE.              00109500
109600     IF  FRMNUIDI = 'GC4A' OR 'GTM1'                              00109600
109700         MOVE IDLINEI   TO   CONTRACT-ID-LINE.                    00109700
109800     IF  FRMNUIDI = 'GC8A'                                        00109800
109900         MOVE IDLINEI   TO   BENEFIT-PROVISION-ID-LINE.           00109900
110000                                                                  00110000
110100     IF EIBAID = DFHPF1 OR DFHPF13                                00110100
110200        PERFORM 8000-000-SWITCH-ADD-DEL-MODE.                     00110200
110300                                                                  00110300
110400     IF EIBAID = DFHPF3 OR DFHPF15                                00110400
110500        PERFORM 5000-000-XCTL-TO-PREVIOUS-MENU.                   00110500
110600                                                                  00110600
110700     IF (EIBAID  =  DFHPF7 OR  DFHPF19 OR  DFHPF8 OR  DFHPF20) AND00110700
110800        DELADDI  =  'CHG/ADD'                                     00110800
110900     THEN                                                         00110900
111000         SET  WT-01-INDEX                     TO +22              00111000
111100         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00111100
111200         MOVE -1                              TO  PERIODL         00111200
111300         GO TO 1000-900-EXIT.                                     00111300
111400                                                                  00111400
111500                                                                  00111500
111600     IF (EIBAID  =  DFHPF7 OR  DFHPF19 OR  DFHPF8 OR  DFHPF20) AND00111600
111700        DELOPTNI  =  'D'                                          00111700
111800     THEN                                                         00111800
111900         SET  WT-01-INDEX                     TO +06              00111900
112000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00112000
112100         MOVE -1                              TO  DELOPTNL        00112100
112200         GO TO 1000-900-EXIT.                                     00112200
112300                                                                  00112300
112400     PERFORM 1100-000-VALIDATE-SCREEN.                            00112400
112500                                                                  00112500
112600     IF  EIBAID  = DFHPF4 OR DFHPF16  AND                         00112600
112700         ACWA-SCREEN-HAS-ERRORS                                   00112700
112800     THEN                                                         00112800
112900         SET  WT-01-INDEX                     TO +13              00112900
113000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00113000
113100         GO TO 1000-900-EXIT.                                     00113100
113200                                                                  00113200
113300     IF  EIBAID  = DFHPF4 OR DFHPF16                              00113300
113400     THEN                                                         00113400
113500         IF  ACWA-SCREEN-HAS-NO-ERRORS                            00113500
113600         THEN                                                     00113600
113700             IF  GCVI-TABLE-SW = 'N'                              00113700
113800             THEN                                                 00113800
113900                 PERFORM 2000-000-PROCESS-REQUEST                 00113900
114000                 GO TO  1000-990-RETURN                           00114000
114100             ELSE                                                 00114100
114200                 SET  WT-01-INDEX                     TO +09      00114200
114300                 MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO  00114300
114400                 MOVE -1                              TO PERIODL  00114400
114500                 GO TO 1000-900-EXIT                              00114500
114600         ELSE                                                     00114600
114700             NEXT SENTENCE                                        00114700
114800     ELSE                                                         00114800
114900         NEXT SENTENCE.                                           00114900
115000                                                                  00115000
115100     IF  ACWA-SCREEN-HAS-ERRORS                                   00115100
115200         GO TO 1000-900-EXIT.                                     00115200
115300                                                                  00115300
115400     IF  EIBAID  =  DFHENTER OR                                   00115400
115500                    DFHPF7   OR  DFHPF19 OR   DFHPF8 OR  DFHPF20  00115500
115600     THEN                                                         00115600
115700         PERFORM 2000-000-PROCESS-REQUEST                         00115700
115800                 GO TO  1000-990-RETURN.                          00115800
115900                                                                  00115900
116000     PERFORM 7900-000-RESET-ATTRIBUTES.                           00116000
116100     MOVE -1                              TO PERIODL.             00116100
116200     SET  WT-01-INDEX                     TO +10                  00116200
116300     MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.             00116300
116400                                                                  00116400
116500                                                                  00116500
116600 1000-900-EXIT.                                                   00116600
116700                                                                  00116700
116800     PERFORM 3100-000-READ-RECORD.                                00116800
116900     MOVE GAD-ENTRY-COUNT  TO  GAD-ENTRY-COUNT.                   00116900
117000     PERFORM 9010-000-SEND-DATAONLY-RETURN.                       00117000
117100 1000-990-RETURN.                                                 00117100
117200*    EXEC CICS  RETURN  END-EXEC.                                 00117200
117300     IF DELADD-OPTION = 'GAS4UPD'                                 00117300
117400         IF INTR-TAB-PGM-ID = 'GA1GPGM'                           00117400
117500             MOVE SPACES TO DELADD-OPTION                         00117500
117600             EXEC CICS RETURN TRANSID('GA1G')                     00117600
117700                       COMMAREA(COMMON-WORKAREAS)                 00117700
117800                       END-EXEC                                   00117800
117900         ELSE                                                     00117900
118000         IF INTR-TAB-PGM-ID = 'GA2GPGM'                           00118000
118100             MOVE SPACES TO DELADD-OPTION                         00118100
118200             EXEC CICS RETURN TRANSID('GA2G')                     00118200
118300                       COMMAREA(COMMON-WORKAREAS)                 00118300
118400                       END-EXEC                                   00118400
118500         ELSE                                                     00118500
118600         IF INTR-TAB-PGM-ID = 'GA1HPGM'                           00118600
118700             MOVE SPACES TO DELADD-OPTION                         00118700
118800             EXEC CICS  RETURN TRANSID('GA1H')                    00118800
118900                        COMMAREA(COMMON-WORKAREAS)                00118900
119000                        END-EXEC                                  00119000
119100         ELSE                                                     00119100
119200         IF INTR-TAB-PGM-ID = 'GA2HPGM'                           00119200
119300             MOVE SPACES TO DELADD-OPTION                         00119300
119400             EXEC CICS  RETURN TRANSID('GA2H')                    00119400
119500                        COMMAREA(COMMON-WORKAREAS)                00119500
119600                        END-EXEC                                  00119600
119700         ELSE                                                     00119700
119800         IF INTR-TAB-PGM-ID = 'GA1SPGM'                           00119800
119900             MOVE SPACES TO DELADD-OPTION                         00119900
120000             EXEC CICS  RETURN TRANSID('GA1S')                    00120000
120100                        COMMAREA(COMMON-WORKAREAS)                00120100
120200                        END-EXEC                                  00120200
120300         ELSE                                                     00120300
120400         IF INTR-TAB-PGM-ID = 'GA1IPGM'                           00120400
120500             MOVE SPACES TO DELADD-OPTION                         00120500
120600             EXEC CICS  RETURN TRANSID('GA1I')                    00120600
120700                        COMMAREA(COMMON-WORKAREAS)                00120700
120800                        END-EXEC                                  00120800
120900         ELSE                                                     00120900
121000         IF INTR-TAB-PGM-ID = 'GA2SPGM'                           00121000
121100             MOVE SPACES TO DELADD-OPTION                         00121100
121200             EXEC CICS  RETURN TRANSID('GA2S')                    00121200
121300                        COMMAREA(COMMON-WORKAREAS)                00121300
121400                        END-EXEC                                  00121400
121500         ELSE                                                     00121500
121600         IF INTR-TAB-PGM-ID = 'GA2IPGM'                           00121600
121700             MOVE SPACES TO DELADD-OPTION                         00121700
121800             EXEC CICS  RETURN TRANSID('GA2I')                    00121800
121900                        COMMAREA(COMMON-WORKAREAS)                00121900
122000                        END-EXEC                                  00122000
122100         ELSE                                                     00122100
122200         IF INTR-TAB-PGM-ID = 'GA1NPGM'                           00122200
122300             MOVE SPACES TO DELADD-OPTION                         00122300
122400             EXEC CICS  RETURN TRANSID('GA1N')                    00122400
122500                        COMMAREA(COMMON-WORKAREAS)                00122500
122600                        END-EXEC                                  00122600
122700         ELSE                                                     00122700
122800         IF INTR-TAB-PGM-ID = 'GA2NPGM'                           00122800
122900             MOVE SPACES TO DELADD-OPTION                         00122900
123000             EXEC CICS  RETURN TRANSID('GA2N')                    00123000
123100                        COMMAREA(COMMON-WORKAREAS)                00123100
123200                        END-EXEC                                  00123200
123300         ELSE                                                     00123300
123400         IF INTR-TAB-PGM-ID = 'GA1OPGM'                           00123400
123500             MOVE SPACES TO DELADD-OPTION                         00123500
123600             EXEC CICS  RETURN TRANSID('GA1O')                    00123600
123700                        COMMAREA(COMMON-WORKAREAS)                00123700
123800                        END-EXEC                                  00123800
123900         ELSE                                                     00123900
124000         IF INTR-TAB-PGM-ID = 'GA2OPGM'                           00124000
124100             MOVE SPACES TO DELADD-OPTION                         00124100
124200             EXEC CICS  RETURN TRANSID('GA2O')                    00124200
124300                        COMMAREA(COMMON-WORKAREAS)                00124300
124400                        END-EXEC                                  00124400
124500         ELSE                                                     00124500
124600         EXEC CICS  RETURN TRANSID('GA1E')                        00124600
124700                    COMMAREA(DFHCOMMAREA)                         00124700
124800                    LENGTH  (EIBCALEN)                            00124800
124900                    END-EXEC                                      00124900
125000     ELSE                                                         00125000
125100     EXEC CICS  RETURN TRANSID('GA1E')                            00125100
125200                COMMAREA(DFHCOMMAREA)                             00125200
125300                LENGTH  (EIBCALEN)                                00125300
125400                END-EXEC.                                         00125400
125500                                                                  00125500
125600     GOBACK.                                                      00125600
125700 1000-999-EXIT.                                                   00125700
125800          EXIT.                                                   00125800
125900/*****************************************************************00125900
126000* 1100  VALIDATE SCREEN                                          *00126000
126100*                                                                *00126100
126200*    THIS IS PRIMARILY A VALIDATION ROUTINE OF DATA BEING ENTERED*00126200
126300*  BY THE OPERATOR, PLUS THE ADDITION OF SOME REINITIALIZATION.  *00126300
126400*  1. REINITIALIZE ATTRIBUTES THAT THE PROGRAM MIGHT MODIFY, AND *00126400
126500*     RESET THE ERROR MESSAGE AND DELETE OPTION TO BLANKS.       *00126500
126600*  2. INSURE THE VALIDITY OF THE OPTIONS THAT CAN BE USED FOR THE*00126600
126700*     INTERNAL TABULAR.                                          *00126700
126800******************************************************************00126800
126900 1100-000-VALIDATE-SCREEN       SECTION.                          00126900
127000 1100-010.                                                        00127000
127100                                                                  00127100
127200     SET ACWA-WF-ALL-LEVEL-TAB-PNTR TO                            00127200
127300         ADDRESS OF  WF-IO-PARM-ALL-LVL-TAB-RECORD.               00127300
127400                                                                  00127400
127500     SET ACWA-COPY-TAB-PNTR         TO                            00127500
127600         ADDRESS OF  COPY-TABULAR-TABLE-AREA.                     00127600
127700                                                                  00127700
127800     SET ACWA-WF-INTERNAL-TAB-PNTR  TO                            00127800
127900         ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD.              00127900
128000                                                                  00128000
128100     SET ACWA-WF-GRP-SPEC-PNTR      TO                            00128100
128200         ADDRESS OF  WF-IO-PARM-WRK-GRP-SPEC-REC.                 00128200
128300                                                                  00128300
128400     SET ACWA-WF-CONTRACT-PNTR      TO                            00128400
128500         ADDRESS OF  WF-IO-PARM-WRK-CONTRACT-REC.                 00128500
128600                                                                  00128600
128700     SET ACWA-WF-BEN-PROV-PNTR      TO                            00128700
128800         ADDRESS OF  WF-IO-PARM-WRK-BEN-PROV-REC.                 00128800
128900                                                                  00128900
129000     SET ACWA-WF-CONTROL-RECORD-PNTR    TO                        00129000
129100         ADDRESS OF  WF-IO-PARM-WRK-CONTROL-REC.                  00129100
129200                                                                  00129200
129300     SET ACWA-PR-CONTRACT-PNTR      TO                            00129300
129400         ADDRESS OF  PR-IO-PARM-WRK-CONTRACT-REC.                 00129400
129500                                                                  00129500
129600     SET ACWA-PR-GRP-SPEC-PNTR      TO                            00129600
129700         ADDRESS OF  PR-IO-PARM-WRK-GRP-SPEC-REC.                 00129700
129800                                                                  00129800
129900     SET ACWA-PR-BEN-PROV-PNTR      TO                            00129900
130000         ADDRESS OF  PR-IO-PARM-WRK-BEN-PROV-REC.                 00130000
130100                                                                  00130100
130200     SET ACWA-PR-ALL-LEVEL-TAB-PNTR   TO                          00130200
130300         ADDRESS OF  PR-IO-PARM-ALL-LVL-TAB-RECORD.               00130300
130400                                                                  00130400
130500                                                                  00130500
130600     MOVE 'N'              TO  ACWA-ERROR-SW.                     00130600
130700     MOVE 'Y'              TO  GCVI-TABLE-SW.                     00130700
130800     MOVE SPACES           TO  ERRMSGO.                           00130800
130900     MOVE DFHBMFSE         TO  PERIODA.                           00130900
131000     MOVE DFHBMASF         TO  IBGRIDA    IPGNIDA   IPGTIDA       00131000
131100                               IDGDIDA    IPGPIDA   IPGSIDA       00131100
131200                               IBGRSLTA   IPGNSLTA  IPGTSLTA      00131200
131300                               IDGDSLTA   IPGPSLTA  IPGSSLTA.     00131300
131400                                                                  00131400
131500     PERFORM 7900-000-RESET-ATTRIBUTES.                           00131500
131600                                                                  00131600
131700*------------- LINK TO SCREEN EDIT MODULE -----------------------*00131700
131800                                                                  00131800
131900     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00131900
132000                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00132000
132100     EXEC CICS  LINK  PROGRAM('GASEDIT1')                         00132100
132200                COMMAREA (COMMON-WORKAREAS)                       00132200
132300                LENGTH(LENGTH OF COMMON-WORKAREAS)  END-EXEC.     00132300
132400                                                                  00132400
132500     IF  ACWA-SCREEN-HAS-ERRORS                                   00132500
132600         GO TO 1100-900-EXIT.                                     00132600
132700                                                                  00132700
132800     IF  ACWA-FIELD-CHG-CNT > ZEROS                               00132800
132900         GO TO 1100-900-EXIT.                                     00132900
133000                                                                  00133000
133100     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00133100
133200         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00133200
133300         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00133300
133400         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00133400
133500         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00133500
133600         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00133600
133700     THEN                                                         00133700
133800         GO TO 1100-900-EXIT.                                     00133800
133900                                                                  00133900
134000                                                                  00134000
134100     COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   00134100
134200              GC-GCIOPARM-LEN  +  GC-WORKFILE-KEY-LEN  +          00134200
134300              GC-GCTABULR-IPGP-FIXED-LEN  +                       00134300
134400             (GC-GCTABULR-IPGP-VARY-LEN  *                        00134400
134500                                 GC-GCTABULR-IPGP-VARY-MAX-OCUR). 00134500
134600                                                                  00134600
134700        EXEC CICS GETMAIN                                         00134700
134800               SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     00134800
134900               INITIMG(WS-HEX-00)                                 00134900
135000               LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)             00135000
135100               END-EXEC                                           00135100
135200                                                                  00135200
135300     SET ACWA-WF-INTERNAL-TAB-PNTR  TO                            00135300
135400         ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD.              00135400
135500                                                                  00135500
135600                                                                  00135600
135700     IF  IBGROPTI  =  'C'                                         00135700
135800     THEN                                                         00135800
135900         IF  IBGRSLTI  >  '8999999'                               00135900
136000         THEN                                                     00136000
136100             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00136100
136200             GO TO 1100-100-GET-INTERNAL-TAB                      00136200
136300         ELSE                                                     00136300
136400             ADD 100        TO  ACWA-FIELD-CHG-CNT                00136400
136500             MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID               00136500
136600             MOVE IBGRSLTI  TO  GCIO-TAB-SLOT-NO                  00136600
136700     ELSE                                                         00136700
136800         NEXT SENTENCE.                                           00136800
136900                                                                  00136900
137000     IF  IDGDOPTI  =  'C'                                         00137000
137100     THEN                                                         00137100
137200         IF  IDGDSLTI  >  '8999999'                               00137200
137300         THEN                                                     00137300
137400             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00137400
137500             GO TO 1100-100-GET-INTERNAL-TAB                      00137500
137600         ELSE                                                     00137600
137700             ADD 100        TO  ACWA-FIELD-CHG-CNT                00137700
137800             MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID               00137800
137900             MOVE IDGDSLTI  TO  GCIO-TAB-SLOT-NO                  00137900
138000     ELSE                                                         00138000
138100         NEXT SENTENCE.                                           00138100
138200                                                                  00138200
138300     IF  IPGNOPTI  =  'C'                                         00138300
138400     THEN                                                         00138400
138500         IF  IPGNSLTI  > '8999999'                                00138500
138600         THEN                                                     00138600
138700             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00138700
138800             GO TO 1100-100-GET-INTERNAL-TAB                      00138800
138900         ELSE                                                     00138900
139000           ADD 100        TO  ACWA-FIELD-CHG-CNT                  00139000
139100           MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                 00139100
139200           MOVE IPGNSLTI  TO  GCIO-TAB-SLOT-NO                    00139200
139300     ELSE                                                         00139300
139400         NEXT SENTENCE.                                           00139400
139500                                                                  00139500
139600     IF  IPGPOPTI  =  'C'                                         00139600
139700     THEN                                                         00139700
139800         IF  IPGPSLTI  > '8999999'                                00139800
139900         THEN                                                     00139900
140000             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00140000
140100             GO TO 1100-100-GET-INTERNAL-TAB                      00140100
140200         ELSE                                                     00140200
140300           ADD 100        TO  ACWA-FIELD-CHG-CNT                  00140300
140400           MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                 00140400
140500           MOVE IPGPSLTI  TO  GCIO-TAB-SLOT-NO                    00140500
140600     ELSE                                                         00140600
140700         NEXT SENTENCE.                                           00140700
140800                                                                  00140800
140900     IF  IPGTOPTI  =  'C'                                         00140900
141000     THEN                                                         00141000
141100         IF  IPGTSLTI  >  '8999999'                               00141100
141200         THEN                                                     00141200
141300             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00141300
141400             GO TO 1100-100-GET-INTERNAL-TAB                      00141400
141500         ELSE                                                     00141500
141600             ADD 100        TO  ACWA-FIELD-CHG-CNT                00141600
141700             MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID               00141700
141800             MOVE IPGTSLTI  TO  GCIO-TAB-SLOT-NO                  00141800
141900     ELSE                                                         00141900
142000         NEXT SENTENCE.                                           00142000
142100                                                                  00142100
142200     IF  IPGSOPTI  =  'C'                                         00142200
142300     THEN                                                         00142300
142400         IF  IPGSSLTI  >  '8999999'                               00142400
142500         THEN                                                     00142500
142600             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00142600
142700             GO TO 1100-100-GET-INTERNAL-TAB                      00142700
142800         ELSE                                                     00142800
142900             ADD 100        TO  ACWA-FIELD-CHG-CNT                00142900
143000             MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID               00143000
143100             MOVE IPGSSLTI  TO  GCIO-TAB-SLOT-NO                  00143100
143200     ELSE                                                         00143200
143300         NEXT SENTENCE.                                           00143300
143400                                                                  00143400
143500     IF IBGROPTI  =  'MT'                                         00143500
143600        MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID                    00143600
143700        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00143700
143800                                                                  00143800
143900     IF IBGROPTI  =  'A'                                          00143900
144000        MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID                    00144000
144100        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00144100
144200                                                                  00144200
144300     IF IDGDOPTI  =  'MT'                                         00144300
144400        MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID                    00144400
144500        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00144500
144600                                                                  00144600
144700     IF IDGDOPTI  =  'A'                                          00144700
144800        MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID                    00144800
144900        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00144900
145000                                                                  00145000
145100     IF IPGNOPTI  =  'MT'                                         00145100
145200        MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                    00145200
145300        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00145300
145400                                                                  00145400
145500     IF IPGNOPTI  =  'A'                                          00145500
145600        MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                    00145600
145700        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00145700
145800                                                                  00145800
145900     IF IPGPOPTI  =  'MT'                                         00145900
146000        MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                    00146000
146100        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00146100
146200                                                                  00146200
146300     IF IPGPOPTI  =  'A'                                          00146300
146400        MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                    00146400
146500        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00146500
146600                                                                  00146600
146700     IF IPGTOPTI  =  'MT'                                         00146700
146800        MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID                    00146800
146900        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00146900
147000                                                                  00147000
147100     IF IPGTOPTI  =  'A'                                          00147100
147200        MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID                    00147200
147300        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00147300
147400                                                                  00147400
147500     IF IPGSOPTI  =  'MT'                                         00147500
147600        MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID                    00147600
147700        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00147700
147800                                                                  00147800
147900     IF IPGSOPTI  =  'A'                                          00147900
148000        MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID                    00148000
148100        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00148100
148200                                                                  00148200
148300     MOVE GCIO-TABULAR-FILE      TO GCIO2-FILE-KEY.               00148300
148400     MOVE GC-GCTABULR-DDNAME     TO GCIO2-FILE-DDNAME.            00148400
148500     MOVE GC-GCIO-AREA-2         TO GCIO2-IO-AREA-TO-USE.         00148500
148600     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO2-FILE-ACCESS-CODE.       00148600
148700     MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          00148700
148800          TO  GXA-ENTRY-COUNT.                                    00148800
148900                                                                  00148900
149000     EXEC CICS  LINK  PROGRAM('GCIOPGM')                          00149000
149100                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00149100
149200                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)   END-EXEC.00149200
149300                                                                  00149300
149400     MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                   00149400
149500                                                                  00149500
149600     IF  NOT GCIO2-GOOD-RETURN AND  ACWA-PROD-INTERNAL-CHG        00149600
149700     THEN                                                         00149700
149800         SET  WT-01-INDEX                     TO +17              00149800
149900         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00149900
150000         MOVE 'Y'                             TO ACWA-ERROR-SW    00150000
150100         IF  IBGROPTI  =  'C'                                     00150100
150200         THEN                                                     00150200
150300             MOVE -1        TO  IBGROPTL                          00150300
150400             MOVE DFHBMUBF  TO  IBGROPTA                          00150400
150500             MOVE DFHBMASB  TO  IBGRIDA  IBGRSLTA                 00150500
150600             GO TO 1100-900-EXIT                                  00150600
150700         ELSE                                                     00150700
150800         IF  IPGNOPTI  =  'C'                                     00150800
150900         THEN                                                     00150900
151000             MOVE -1        TO  IPGNOPTL                          00151000
151100             MOVE DFHBMUBF  TO  IPGNOPTA                          00151100
151200             MOVE DFHBMASB  TO  IPGNIDA  IPGNSLTA                 00151200
151300             GO TO 1100-900-EXIT                                  00151300
151400         ELSE                                                     00151400
151500         IF  IPGTOPTI  =  'C'                                     00151500
151600         THEN                                                     00151600
151700             MOVE -1        TO  IPGTOPTL                          00151700
151800             MOVE DFHBMUBF  TO  IPGTOPTA                          00151800
151900             MOVE DFHBMASB  TO  IPGTIDA  IPGTSLTA                 00151900
152000             GO TO 1100-900-EXIT                                  00152000
152100         ELSE                                                     00152100
152200         IF  IPGSOPTI  =  'C'                                     00152200
152300         THEN                                                     00152300
152400             MOVE -1        TO  IPGSOPTL                          00152400
152500             MOVE DFHBMUBF  TO  IPGSOPTA                          00152500
152600             MOVE DFHBMASB  TO  IPGSIDA  IPGSSLTA                 00152600
152700             GO TO 1100-900-EXIT                                  00152700
152800         ELSE                                                     00152800
152900         IF  IDGDOPTI  =  'C'                                     00152900
153000         THEN                                                     00153000
153100             MOVE -1        TO  IDGDOPTL                          00153100
153200             MOVE DFHBMUBF  TO  IDGDOPTA                          00153200
153300             MOVE DFHBMASB  TO  IDGDIDA  IDGDSLTA                 00153300
153400             GO TO 1100-900-EXIT                                  00153400
153500         ELSE                                                     00153500
153600         IF  IPGPOPTI  =  'C'                                     00153600
153700         THEN                                                     00153700
153800             MOVE -1        TO  IPGPOPTL                          00153800
153900             MOVE DFHBMUBF  TO  IPGPOPTA                          00153900
154000             MOVE DFHBMASB  TO  IPGPIDA  IPGPSLTA                 00154000
154100             GO TO 1100-900-EXIT                                  00154100
154200         ELSE                                                     00154200
154300             NEXT SENTENCE                                        00154300
154400     ELSE                                                         00154400
154500         NEXT SENTENCE.                                           00154500
154600                                                                  00154600
154700                                                                  00154700
154800     IF  NOT GCIO2-GOOD-RETURN AND  GCIO-TAB-SLOT-NO  NOT =  1    00154800
154900     THEN                                                         00154900
155000         SET  WT-01-INDEX                     TO +18              00155000
155100         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00155100
155200         MOVE 'Y'                             TO ACWA-ERROR-SW    00155200
155300         MOVE -1                              TO MFRMSLTL         00155300
155400         MOVE DFHBMUBF                        TO MFRMSLTA         00155400
155500         GO TO 1100-900-EXIT.                                     00155500
155600                                                                  00155600
155700     IF  NOT GCIO2-GOOD-RETURN                                    00155700
155800     THEN                                                         00155800
155900         MOVE WS-ABCODE-1EF1        TO WS-ABCODE                  00155900
156000         MOVE WS-ABCODE-1EF1-MSG    TO WS-ABCODE-MSG              00156000
156100         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00156100
156200                                                                  00156200
156300     MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                   00156300
156400                                                                  00156400
156500     COMPUTE  GCIO2-RECORD-LENGTH  =                              00156500
156600              GC-WORKFILE-KEY-LEN  +  GCIO2-RECORD-LENGTH.        00156600
156700                                                                  00156700
156800     IF IBGROPTI  =  'C' OR  'MT' OR 'A'                          00156800
156900        IF  GXA-ENTRY-COUNT  NOT >  1                             00156900
157000            MOVE 'GA2GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00157000
157100                                INTR-TAB-PGM-ID                   00157100
157200        ELSE                                                      00157200
157300            MOVE 'GA1GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00157300
157400                                INTR-TAB-PGM-ID.                  00157400
157500                                                                  00157500
157600     IF IPGNOPTI  =  'C' OR  'MT' OR 'A'                          00157600
157700        IF  GXA-ENTRY-COUNT  NOT >  1                             00157700
157800            MOVE 'GA2HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00157800
157900                                INTR-TAB-PGM-ID                   00157900
158000        ELSE                                                      00158000
158100            MOVE 'GA1HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00158100
158200                                INTR-TAB-PGM-ID.                  00158200
158300                                                                  00158300
158400     IF IPGTOPTI  =  'C' OR  'MT' OR 'A'                          00158400
158500        IF  GXA-ENTRY-COUNT  NOT >  1                             00158500
158600            MOVE 'GA2IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00158600
158700                                INTR-TAB-PGM-ID                   00158700
158800        ELSE                                                      00158800
158900            MOVE 'GA1IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00158900
159000                                INTR-TAB-PGM-ID.                  00159000
159100                                                                  00159100
159200     IF IPGSOPTI  =  'C' OR  'MT' OR 'A'                          00159200
159300        IF  GXA-ENTRY-COUNT  NOT >  1                             00159300
159400            MOVE 'GA2SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00159400
159500                                INTR-TAB-PGM-ID                   00159500
159600        ELSE                                                      00159600
159700            MOVE 'GA1SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00159700
159800                                INTR-TAB-PGM-ID.                  00159800
159900                                                                  00159900
160000     IF IDGDOPTI  =  'C' OR  'MT' OR 'A'                          00160000
160100        IF  GXA-ENTRY-COUNT  NOT >  1                             00160100
160200            MOVE 'GA2NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00160200
160300                                INTR-TAB-PGM-ID                   00160300
160400        ELSE                                                      00160400
160500            MOVE 'GA1NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00160500
160600                                INTR-TAB-PGM-ID.                  00160600
160700                                                                  00160700
160800     IF IPGPOPTI  =  'C' OR  'MT' OR 'A'                          00160800
160900        IF  GXA-ENTRY-COUNT  NOT >  1                             00160900
161000            MOVE 'GA2OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00161000
161100                                INTR-TAB-PGM-ID                   00161100
161200        ELSE                                                      00161200
161300            MOVE 'GA1OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00161300
161400                                INTR-TAB-PGM-ID.                  00161400
161500                                                                  00161500
161600     GO TO 1100-900-EXIT.                                         00161600
161700                                                                  00161700
161800/                                                                 00161800
161900 1100-100-GET-INTERNAL-TAB.                                       00161900
162000                                                                  00162000
162100                                                                  00162100
162200     IF FRMNUIDI  =  'GS3A'                                       00162200
162300        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00162300
162400        MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      00162400
162500                                                                  00162500
162600     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00162600
162700        PERFORM 6100-000-BUILD-CONTRACT-KEY                       00162700
162800        MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      00162800
162900                                                                  00162900
163000     IF FRMNUIDI  =  'GC8A'                                       00163000
163100        PERFORM 6200-000-BUILD-BEN-PROV-KEY                       00163100
163200        MOVE 'C6'               TO GCIO-WRK-RECORD-TYPE           00163200
163300        MOVE TABIDI             TO GCIO-WRK-PROVISION-ID          00163300
163400        MOVE TABSLTNI           TO ACWA-DISPLAY-LEN-7             00163400
163500        MOVE ACWA-DISPLAY-LEN-7 TO GCIO-WRK-PROVISION-SLOT-NO.    00163500
163600                                                                  00163600
163700     MOVE GC-GCPSWORK-DDNAME TO GCIO2-FILE-DDNAME.                00163700
163800     MOVE GC-GCIO-AREA-1     TO GCIO2-IO-AREA-TO-USE.             00163800
163900                                                                  00163900
164000     IF IBGROPTI  =  'C'                                          00164000
164100        MOVE '#IBGR '  TO  GCIO-WRK-TAB-PROVISION-ID              00164100
164200        MOVE IBGRSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00164200
164300                                                                  00164300
164400     IF IDGDOPTI  =  'C'                                          00164400
164500        MOVE '#IDGD '  TO  GCIO-WRK-TAB-PROVISION-ID              00164500
164600        MOVE IDGDSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00164600
164700                                                                  00164700
164800     IF IPGNOPTI  =  'C'                                          00164800
164900        MOVE '#IPGN '  TO  GCIO-WRK-TAB-PROVISION-ID              00164900
165000        MOVE IPGNSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00165000
165100                                                                  00165100
165200     IF IPGPOPTI  =  'C'                                          00165200
165300        MOVE '#IPGP '  TO  GCIO-WRK-TAB-PROVISION-ID              00165300
165400        MOVE IPGPSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00165400
165500                                                                  00165500
165600     IF IPGTOPTI  =  'C'                                          00165600
165700        MOVE '#IPGT '  TO  GCIO-WRK-TAB-PROVISION-ID              00165700
165800        MOVE IPGTSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00165800
165900                                                                  00165900
166000     IF IPGSOPTI  =  'C'                                          00166000
166100        MOVE '#IPGS '  TO  GCIO-WRK-TAB-PROVISION-ID              00166100
166200        MOVE IPGSSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00166200
166300                                                                  00166300
166400     MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              00166400
166500     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO2-FILE-ACCESS-CODE.       00166500
166600     MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          00166600
166700          TO  GXA-ENTRY-COUNT.                                    00166700
166800                                                                  00166800
166900     EXEC CICS  LINK  PROGRAM('GCIOPGM')                          00166900
167000                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00167000
167100                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. 00167100
167200                                                                  00167200
167300     IF NOT GCIO2-GOOD-RETURN                                     00167300
167400        MOVE WS-ABCODE-1EF2        TO WS-ABCODE                   00167400
167500        MOVE WS-ABCODE-1EF2-MSG    TO WS-ABCODE-MSG               00167500
167600        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00167600
167700                                                                  00167700
167800     MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                   00167800
167900                                                                  00167900
168000     IF IBGROPTI  =  'C'                                          00168000
168100        IF GXA-ENTRY-COUNT  NOT >  1                              00168100
168200           MOVE 'GA2GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00168200
168300                                INTR-TAB-PGM-ID                   00168300
168400        ELSE                                                      00168400
168500           MOVE 'GA1GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00168500
168600                                INTR-TAB-PGM-ID.                  00168600
168700                                                                  00168700
168800     IF IPGNOPTI  =  'C'                                          00168800
168900        IF GXA-ENTRY-COUNT  NOT >  1                              00168900
169000           MOVE 'GA2HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00169000
169100                                INTR-TAB-PGM-ID                   00169100
169200        ELSE                                                      00169200
169300           MOVE 'GA1HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00169300
169400                                INTR-TAB-PGM-ID.                  00169400
169500                                                                  00169500
169600     IF IPGTOPTI  =  'C'                                          00169600
169700        IF GXA-ENTRY-COUNT  NOT >  1                              00169700
169800           MOVE 'GA2IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00169800
169900                                INTR-TAB-PGM-ID                   00169900
170000        ELSE                                                      00170000
170100           MOVE 'GA1IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00170100
170200                                INTR-TAB-PGM-ID.                  00170200
170300                                                                  00170300
170400     IF IPGSOPTI  =  'C'                                          00170400
170500        IF GXA-ENTRY-COUNT  NOT >  1                              00170500
170600           MOVE 'GA2SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00170600
170700                                INTR-TAB-PGM-ID                   00170700
170800        ELSE                                                      00170800
170900           MOVE 'GA1SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00170900
171000                                INTR-TAB-PGM-ID.                  00171000
171100                                                                  00171100
171200     IF IDGDOPTI  =  'C'                                          00171200
171300        IF GXA-ENTRY-COUNT  NOT >  1                              00171300
171400           MOVE 'GA2NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00171400
171500                                INTR-TAB-PGM-ID                   00171500
171600        ELSE                                                      00171600
171700           MOVE 'GA1NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00171700
171800                                INTR-TAB-PGM-ID.                  00171800
171900                                                                  00171900
172000     IF IPGPOPTI  =  'C'                                          00172000
172100        IF GXA-ENTRY-COUNT  NOT >  1                              00172100
172200           MOVE 'GA2OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00172200
172300                                INTR-TAB-PGM-ID                   00172300
172400        ELSE                                                      00172400
172500           MOVE 'GA1OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00172500
172600                                INTR-TAB-PGM-ID.                  00172600
172700                                                                  00172700
172800 1100-900-EXIT.                                                   00172800
172900     EXIT.                                                        00172900
173000                                                                  00173000
      ****   03/19/15  KIKI  CHANGED - #AOL PERCENT FIELD VALIDATION   *00173060
      ****                             1200-D210-EDIT  SECTION         *00173070
      ****                             NO LONGER IS EXECUTED FROM      *00173080
      ****                             5000-000-XCTL-TO-PREVIOUS-MENU  *00173090
      ****                                                             *00173091
      ****                             NO LONGER THE PERCENT FIELD     *00173092
      ****                             MUST BE SAME IN ALL OCCURS.,    *00173093
      ****                             WHEN OTHER THAN +100            *00173094
      ****                                                             *00173095
       1200-D210-EDIT  SECTION.                                         00173100
                                                                        00173110
173200     IF GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX) = +100                00173200
               GO TO 1200-EXIT.                                         00173300
                                                                        00173310
173400     IF WS-HOLD-PCT-LVL = +999                                    00173400
173500         MOVE GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX)                 00173500
173600           TO WS-HOLD-PCT-LVL                                     00173600
               GO TO 1200-EXIT.                                         00173700
                                                                        00173710
173800     IF GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX)                       00173800
173900       NOT = WS-HOLD-PCT-LVL                                      00173900
174000         SET  WT-01-INDEX                     TO +23              00174000
174100         MOVE GAD-OCCURS-ENTRY-COUNTER (GAD-INDEX)                00174100
174200           TO WS-UNPK-SLOT                                        00174200
174300         MOVE WS-UNPK-SLOT TO WT-01-ENT-CTR                       00174300
174400         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00174400
174500         MOVE 'Y'                             TO ACWA-ERROR-SW    00174500
174600         MOVE -1                              TO PERLIMTL         00174600
               MOVE DFHBMUBF                        TO PERLIMTA.        00174700
                                                                        00174710
174800 1200-EXIT.                                                       00174800
174900     EXIT.                                                        00174900
175000                                                                  00175000
175100/*****************************************************************00175100
175200* 2000  PROCESS REQUEST                                          *00175200
175300*                                                                *00175300
175400*   THIS ROUTINE CHECKS THE CHARACTERISTICS OF THE INCOMING TRANS-00175400
175500* ACTION AND ROUTES THEM TO THE APPROPRIATE ROUTINE TO PROCESS   *00175500
175600* THE REQUEST.                                                   *00175600
175700******************************************************************00175700
175800 2000-000-PROCESS-REQUEST       SECTION.                          00175800
175900 2000-010.                                                        00175900
176000                                                                  00176000
176100     IF (EIBAID       =  DFHENTER OR DFHPF4 OR DFHPF16) AND       00176100
176200        DELADDI       =  'CHG/ADD'                      AND       00176200
176300        OENTCTRI      =  '0000000'                                00176300
176400        PERFORM 2100-000-INSERT-SKELETON.                         00176400
176500                                                                  00176500
176600** LINK TO THE CHANGE/DELETE MODULE TO PROCESS FIVE DIFFERENT     00176600
176700** REQUESTS DEPENDING ON THE USER RESPONSE                        00176700
176800**                                                                00176800
176900     MOVE 'CHG/DEL' TO DELADD-OPTION.                             00176900
177000     MOVE SPACES   TO   LVL2-B-SW  LVL2-F-SW  LVL2-G-SW.          00177000
177100                                                                  00177100
177200     MOVE WS-ALT-WORKFILE-KEYS      TO  ACWA-ALT-WORKFILE-KEYS.   00177200
177300***  SET  ACWA-INDEX-1              TO  GAD-INDEX.                00177300
177400     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00177400
177500                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00177500
177600                                                                  00177600
177700     EXEC CICS  LINK  PROGRAM ('GAS4UPD')                         00177700
177800                COMMAREA(COMMON-WORKAREAS)                        00177800
177900                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00177900
178000                                                                  00178000
178100     SET ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD  TO             00178100
178200                 ACWA-WF-ALL-LEVEL-TAB-PNTR.                      00178200
178300                                                                  00178300
178400     SET ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD TO             00178400
178500                 ACWA-WF-INTERNAL-TAB-PNTR.                       00178500
178600                                                                  00178600
178700     IF LVL2-B-SW  =  'Y'                                         00178700
178800        PERFORM  7900-000-RESET-ATTRIBUTES                        00178800
178900        PERFORM  8100-000-DISPLAY-ADD-SCREEN.                     00178900
179000                                                                  00179000
179100     IF LVL2-F-SW  =  'Y'                                         00179100
179200        PERFORM  3100-000-READ-RECORD                             00179200
179300        PERFORM  4300-000-DISPLAY-PREV.                           00179300
179400                                                                  00179400
179500     IF LVL2-G-SW  =  'Y'                                         00179500
179600        PERFORM  3100-000-READ-RECORD                             00179600
179700        PERFORM  4200-000-DISPLAY-NEXT.                           00179700
179800                                                                  00179800
179900                                                                  00179900
180000 2000-900-EXIT. EXIT.                                             00180000
180100                                                                  00180100
180200/*****************************************************************00180200
180300* 2100  INSERT SKELETON                                          *00180300
180400*                                                                *00180400
180500*    THIS ROUTINE WILL ADD A NEW ENTRY INTO THE TABLE.  IF THE   *00180500
180600*  TABLE ALREADY CONTAINS THE MAXIMUM NUMBER OF 29 ENTRIES THE   *00180600
180700*  SORT ROUTINE MAY REDUCE THAT NUMBER AS IT WILL DELETE ALL     *00180700
180800*  DUPLICATES.  IF THE NEW ENTRY CAN BE ADDED AND THE OPERATOR   *00180800
180900*  REQUESTED THE ADDITION OF AN INTERNAL TABULAR THIS ROUTINE    *00180900
181000*  WILL WRITE THE NEW INTERNAL TABULAR TO THE WORKFILE AND THEN  *00181000
181100*  PASS IT TO THE ADD VERSION OF THE INTERNAL TABULAR PROGRAM.   *00181100
181200******************************************************************00181200
181300 2100-000-INSERT-SKELETON       SECTION.                          00181300
181400 2100-010.                                                        00181400
181500                                                                  00181500
181600     IF  EIBAID  =  DFHENTER       AND                            00181600
181700         ACWA-SCREEN-HAS-NO-ERRORS AND                            00181700
181800         GCVI-TABLE-SW  =  'N'                                    00181800
181900     THEN                                                         00181900
182000         SET  WT-01-INDEX                     TO +07              00182000
182100         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00182100
182200         PERFORM 9010-000-SEND-DATAONLY-RETURN.                   00182200
182300                                                                  00182300
182400     IF  EIBAID  =  DFHPF4 OR  DFHPF16                            00182400
182500         PERFORM 7900-000-RESET-ATTRIBUTES.                       00182500
182600                                                                  00182600
182700     PERFORM 3200-000-READ-REC-FOR-UPDATE.                        00182700
182800                                                                  00182800
182900     IF NOT GCIO-GOOD-RETURN                                      00182900
183000        MOVE WS-ABCODE-1EF5        TO WS-ABCODE                   00183000
183100        MOVE WS-ABCODE-1EF5-MSG    TO WS-ABCODE-MSG               00183100
183200        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00183200
183300                                                                  00183300
183400                                                                  00183400
183500     IF GAD-OCC-ENTRY-TAB-SLOT-CNTR  >  9999900                   00183500
183600        SET  WT-01-INDEX                     TO +15               00183600
183700        MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO           00183700
183800        MOVE -1                              TO PERIODL           00183800
183900        PERFORM 9010-000-SEND-DATAONLY-RETURN.                    00183900
184000                                                                  00184000
184100                                                                  00184100
184200     MOVE GAD-ENTRY-COUNT  TO  GAD-ENTRY-COUNT.                   00184200
184300                                                                  00184300
184400     IF  GAD-ENTRY-COUNT  NOT <  GC-GCTABULR-AOL-VARY-MAX-OCUR    00184400
184500     THEN                                                         00184500
184600         PERFORM 6500-000-SORT-COMPRESS-ALL-LVL                   00184600
184700         IF  ACWA-SCREEN-HAS-NO-ERRORS                            00184700
184800         THEN                                                     00184800
184900             PERFORM 2500-000-ADD-NEW-OCCURS                      00184900
185000****         PERFORM 4600-000-UPDATE-CDE-STATUS                   00185000
185100             IF  WRK-CDE-SP = '2 '     AND                        00185100
185200                 CDEINDO    = ('+CDE+'  OR '+CDE-')               00185200
185300                 ADD 1      TO   ACWA-CDE-1U-COUNT                00185300
185400                 SUBTRACT 1 FROM ACWA-CDE-2B-COUNT                00185400
185500                 MOVE '1U'  TO   WRK-CDE-SP                       00185500
185600                 PERFORM 3000-000-UPDATE-GAD-RECORD               00185600
185700             ELSE                                                 00185700
185800                 PERFORM 3000-000-UPDATE-GAD-RECORD               00185800
185900         ELSE                                                     00185900
186000             SET  WT-01-INDEX                     TO +16          00186000
186100             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00186100
186200             MOVE -1                              TO  PERIODL     00186200
186300             PERFORM 9010-000-SEND-DATAONLY-RETURN                00186300
186400      ELSE                                                        00186400
186500          PERFORM 2500-000-ADD-NEW-OCCURS                         00186500
186600****      PERFORM 4600-000-UPDATE-CDE-STATUS                      00186600
186700             IF  WRK-CDE-SP = '2 '      AND                       00186700
186800                 CDEINDO    = ('+CDE+'  OR '+CDE-')               00186800
186900                 ADD 1      TO   ACWA-CDE-1U-COUNT                00186900
187000                 SUBTRACT 1 FROM ACWA-CDE-2B-COUNT                00187000
187100                 MOVE '1U'  TO   WRK-CDE-SP                       00187100
187200                 PERFORM 3000-000-UPDATE-GAD-RECORD               00187200
187300             ELSE                                                 00187300
187400                 PERFORM 3000-000-UPDATE-GAD-RECORD.              00187400
187500                                                                  00187500
187600     SET GAD-INDEX  DOWN BY  1.                                   00187600
187700                                                                  00187700
187800     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00187800
187900         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00187900
188000         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00188000
188100         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00188100
188200         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00188200
188300         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00188300
188400     THEN                                                         00188400
188500         PERFORM 4700-000-UPDATE-CONTROL-RECORD                   00188500
188600         MOVE GAD-OCCURS-ENTRY-COUNTER (GAD-INDEX)                00188600
188700                                  TO  ACWA-DISPLAY-LEN-7          00188700
188800         MOVE ACWA-DISPLAY-LEN-7  TO  OENTCTRO                    00188800
188900         MOVE -1                  TO  PERIODL                     00188900
189000         PERFORM 9010-000-SEND-DATAONLY-RETURN.                   00189000
189100                                                                  00189100
189200*    EXEC CICS GETMAIN                                            00189200
189300*              SET(ADDRESS OF COMMUNICATION-KEY-AREA)             00189300
189400*              INITIMG(WS-HEX-00)                                 00189400
189500*              LENGTH(WS-COMMUNICATION-KEY-LEN)                   00189500
189600*              END-EXEC.                                          00189600
189700                                                                  00189700
189800*    SET ACWA-COMM-KEY-PNTR  TO                                   00189800
189900*                      ADDRESS OF COMMUNICATION-KEY-AREA.         00189900
190000                                                                  00190000
190100     IF FRMNUIDI  =  'GS3A'                                       00190100
190200        MOVE 'G4'    TO  GCIO-WRK-RECORD-TYPE                     00190200
190300        MOVE SPACES  TO  GCA-L-O-B  GCA-PROV-CTL                  00190300
190400                         GCA-BEN-PROV-ID.                         00190400
190500                                                                  00190500
190600     IF FRMNUIDI = 'GC4A' OR 'GTM1'                               00190600
190700        MOVE 'C3'                       TO  GCIO-WRK-RECORD-TYPE  00190700
190800        MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B             00190800
190900        MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL          00190900
191000        MOVE SPACES  TO  GCA-BEN-PROV-ID.                         00191000
191100                                                                  00191100
191200     IF FRMNUIDI  =  'GC8A'                                       00191200
191300        MOVE 'C6'                       TO  GCIO-WRK-RECORD-TYPE  00191300
191400        MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B             00191400
191500        MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL          00191500
191600        MOVE GCIO-WRK-PROVISION-ID      TO  GCA-BEN-PROV-ID       00191600
191700        MOVE GCIO-WRK-TABULAR-PROVISION TO                        00191700
191800                                     GCIO-WRK-BENEFIT-PROVISION.  00191800
191900                                                                  00191900
192000     MOVE GCIO-WRK-EFFDT-CEN       TO GCA-EFFDT-CEN.              00192000
192100     MOVE GCIO-WRK-EFFECTIVE-DATE  TO HGADATE-JULIAN1.            00192100
192200     PERFORM 9300-000-JULIAN-TO-GREGORIAN.                        00192200
192300     MOVE HGADATE-DATE2            TO GCA-EFFECTIVE-DATE.         00192300
192400     MOVE GC-GCPSWORK-DDNAME       TO GCIO2-FILE-DDNAME.          00192400
192500     MOVE GC-GCIO-AREA-1           TO GCIO2-IO-AREA-TO-USE.       00192500
192600     MOVE TABIDI                   TO GCA-ALL-LEVEL-TAB-ID.       00192600
192700     MOVE TABSLTNI                 TO ACWA-DISPLAY-LEN-7.         00192700
192800     MOVE ACWA-DISPLAY-LEN-7       TO GCA-ALL-LEVEL-TAB-SLOT.     00192800
192900     MOVE GXA-PROVISION-ID         TO GCIO-WRK-TAB-PROVISION-ID,  00192900
193000                                      GCA-INTERNAL-TAB-ID.        00193000
193100     MOVE GXA-INCLUDE-EXCLUDE-IND  TO GCA-I-E-INDC.               00193100
193200     MOVE GXA-PROVISION-SLOT-NO    TO SAVE-COPY-FROM-SLOT.        00193200
193300     MOVE GAD-OCCURS-ENTRY-COUNTER (GAD-INDEX)                    00193300
193400                                   TO GCIO-WRK-TAB-PROV-SLOT-NO   00193400
193500                                      GXA-PROVISION-SLOT-NO       00193500
193600                                      ACWA-DISPLAY-LEN-7.         00193600
193700     MOVE ACWA-DISPLAY-LEN-7       TO GCA-INTERNAL-TAB-SLOT       00193700
193800                                      GCA-OCCURS-ENTRY-COUNTER.   00193800
193900     MOVE 'A'                      TO GCA-ADD-DEL-IND.            00193900
194000     MOVE 'CHG/ADD'                TO DELADD-OPTION.              00194000
194100     MOVE FRMNUIDI                 TO GCA-FROM-MENU-ID.           00194100
194200     MOVE FUNCTONI                 TO GCA-ALL-LEVEL-TAB-FUNC-CODE.00194200
194300     MOVE GCIO-WRK-PLAN-CODE       TO GCA-PLAN-CODE.              00194300
194400     MOVE GCIO-WRK-GROUP-NUM       TO GCA-GROUP-NUM.              00194400
194500     MOVE GCIO-WRK-SECTION-NUM     TO GCA-SECTION-NUM.            00194500
194600     MOVE GCIO-WRK-PKG-CODE        TO GCA-PKG-CODE.               00194600
194700     MOVE GCIO-WRK-FAMILY-RELATION-LVL                            00194700
194800                                   TO GCA-FAM-REL-LVL.            00194800
194900     MOVE SPACES                   TO WORK-RECORD-2.              00194900
195000     MOVE GCIO-WORKFILE-KEY        TO GCIO2-FILE-KEY              00195000
195100                                      WORK-RECORD-KEY-2.          00195100
195200     MOVE SAVE-COPY-FROM-SLOT      TO WRK2-PROV-POOL-COPY-SLOT.   00195200
195300                                                                  00195300
195400     IF FRMNUIDI  =  'GC8A'                                       00195400
195500        MOVE GCIO-WRK-PROVISION-ID TO WRK2-ALL-LEV-BEN-PROV.      00195500
195600                                                                  00195600
195700     IF WRK-SIGNAL-FROM-ONLINE  =  'W'                            00195700
195800        MOVE 'W'       TO  WRK2-SIGNAL-FROM-ONLINE                00195800
195900        MOVE '1U'      TO  WRK2-CDE-SP                            00195900
196000        ADD   1        TO  ACWA-CDE-1U-COUNT                      00196000
196100     ELSE                                                         00196100
196200        IF  CDEINDO  = ('+CDE+'  OR '+CDE-')                      00196200
196300            MOVE '1U'   TO  WRK2-CDE-SP                           00196300
196400            ADD   1     TO  ACWA-CDE-1U-COUNT                     00196400
196500        ELSE                                                      00196500
196600            MOVE '2 '   TO  WRK2-CDE-SP                           00196600
196700            ADD   1     TO  ACWA-CDE-2B-COUNT.                    00196700
196800                                                                  00196800
196900** SET INDICATOR TO CAPTURE OPERATOR-ID.                          00196900
197000     MOVE '1'          TO  GCIO2-OPER-ID-IND.                     00197000
197100                                                                  00197100
197200     PERFORM 4700-000-UPDATE-CONTROL-RECORD.                      00197200
197300     MOVE GC-GCIO-ACCESS-CODE-WR   TO  GCIO2-FILE-ACCESS-CODE.    00197300
197400                                                                  00197400
197500     COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   00197500
197600              GC-GCIOPARM-LEN  +  GCIO2-RECORD-LENGTH.            00197600
197700                                                                  00197700
197800     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00197800
197900                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00197900
198000                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. 00198000
198100                                                                  00198100
198200     IF NOT GCIO2-GOOD-RETURN                                     00198200
198300        MOVE WS-ABCODE-1EF6        TO WS-ABCODE                   00198300
198400        MOVE WS-ABCODE-1EF6-MSG    TO WS-ABCODE-MSG               00198400
198500        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00198500
198600                                                                  00198600
198700     SET GCA-RECORD-POINTER  TO                                   00198700
198800                    ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.    00198800
198900                                                                  00198900
199000     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00199000
199100                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00199100
199200     EXEC CICS  XCTL  PROGRAM(WS-INTERNAL-TABULAR-PGM-ID)         00199200
199300                COMMAREA(COMMON-WORKAREAS)                        00199300
199400                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00199400
199500                                                                  00199500
199600 2100-900-EXIT.                                                   00199600
199700          EXIT.                                                   00199700
199800/*****************************************************************00199800
199900* 2500  ADD NEW OCCURS                                           *00199900
200000*                                                                *00200000
200100*     INITIALIZE ENTRY IN THE TABLE TO EITHER SPACES OR ZEROS,   *00200100
200200*  THEN IF A FIELD WAS ENTERED BY THE OPERATOR MOVE IT TO THE    *00200200
200300*  TABLE IN THE RECORD.                                          *00200300
200400******************************************************************00200400
200500 2500-000-ADD-NEW-OCCURS        SECTION.                          00200500
200600 2500-010.                                                        00200600
200700                                                                  00200700
200800     MOVE GAD-ENTRY-COUNT  TO  GAD-ENTRY-COUNT.                   00200800
200900     SET GAD-INDEX         TO  GAD-ENTRY-COUNT.                   00200900
201000     MOVE LOW-VALUES       TO  GAD-ENTRY (GAD-INDEX).             00201000
201100                                                                  00201100
201200     MOVE ZEROS  TO  GAD-O-P-X-INTL-TAB-SLOT-2 (GAD-INDEX),       00201200
201300                     GAD-O-P-X-INTL-TAB-SLOT-3 (GAD-INDEX),       00201300
201400                     GAD-O-P-X-INTL-TAB-SLOT-4 (GAD-INDEX),       00201400
201500                     GAD-O-P-X-INTL-TAB-SLOT-5 (GAD-INDEX).       00201500
201600                                                                  00201600
201700                                                                  00201700
201800     MOVE SPACES  TO  GAD-O-P-X-INTL-TAB-TAB-ID-2 (GAD-INDEX)     00201800
201900                      GAD-O-P-X-INTL-TAB-TAB-ID-3 (GAD-INDEX)     00201900
202000                      GAD-O-P-X-INTL-TAB-TAB-ID-4 (GAD-INDEX)     00202000
202100                      GAD-O-P-X-INTL-TAB-TAB-ID-5 (GAD-INDEX).    00202100
202200                                                                  00202200
202300     MOVE GAD-OCC-ENTRY-TAB-SLOT-CNTR                             00202300
202400                    TO  GAD-OCCURS-ENTRY-COUNTER      (GAD-INDEX).00202400
202500     ADD 1          TO  GAD-OCC-ENTRY-TAB-SLOT-CNTR.              00202500
202600     MOVE DAYFACII  TO  GAD-O-P-X-DAY-FACTOR-IND      (GAD-INDEX).00202600
202700     MOVE COPAYINI  TO  GAD-O-P-X-CO-PAY-IND          (GAD-INDEX).00202700
202800     MOVE BISNDINI  TO  GAD-O-P-X-BISCENDING-IND      (GAD-INDEX).00202800
202900     MOVE CARYOVRI  TO  GAD-CARRY-OVER-CREDIT-IND     (GAD-INDEX).00202900
203000     MOVE ASCDSCDI  TO  GAD-O-P-X-ASCEND-DESCEND-IND  (GAD-INDEX).00203000
      *P21595 CHANGES STARTS                                            00203010
203000     MOVE BENTYPI   TO  GAD-O-P-X-BEN-TYPE            (GAD-INDEX).00203011
203000     MOVE TIERCDI   TO  GAD-O-P-X-TIER-CODE           (GAD-INDEX).00203012
203000     MOVE TIERLVI   TO  GAD-O-P-X-TIER-LVL            (GAD-INDEX).00203013
      *P21595 CHANGES ENDS                                              00203014
203100     MOVE DEFINTNI  TO  GAD-O-P-X-DEFINITION          (GAD-INDEX).00203100
203200     MOVE FYIVALI   TO  GAD-O-P-X-FYI-VALUE           (GAD-INDEX).00203200
203300     MOVE CSTCONTI  TO  GAD-O-P-X-COST-CONTAIN-IND    (GAD-INDEX).00203300
203400     MOVE PERIODI   TO  GAD-O-P-X-BENEFIT-PERIOD      (GAD-INDEX).00203400
203500     MOVE PERTQALI  TO  GAD-O-P-X-BEN-PER-TIME-QUAL   (GAD-INDEX).00203500
203600     MOVE FAMINDII  TO  GAD-O-P-X-FAM-OR-INDIV        (GAD-INDEX).00203600
203700     MOVE PLCTRMTI  TO  GAD-O-P-X-PLACE-OF-TREATMENT  (GAD-INDEX).00203700
203800     MOVE SRVGRUPI  TO  GAD-O-P-X-SERVICE-GROUP       (GAD-INDEX).00203800
203900     MOVE AGELIMLI  TO ACWA-DISPLAY-LEN-3-X.                      00203900
204000     MOVE ACWA-DISPLAY-LEN-3                                      00204000
204100                    TO GAD-O-P-X-AGE-LIMIT-FROM       (GAD-INDEX).00204100
204200     MOVE AGELIMHI  TO ACWA-DISPLAY-LEN-3-X.                      00204200
204300     MOVE ACWA-DISPLAY-LEN-3                                      00204300
204400                    TO GAD-O-P-X-AGE-LIMIT-TO         (GAD-INDEX).00204400
204500     MOVE FEAKINDI  TO GAD-O-P-X-FEAK-IND             (GAD-INDEX).00204500
204600     MOVE ACCUMIDI  TO GAD-O-P-X-ACCUMID              (GAD-INDEX).00204600
204700     MOVE CAPINDI   TO GAD-O-P-X-COMB-APPLIED-IND     (GAD-INDEX).00204700
204800     MOVE SABDINDI  TO GAD-O-P-X-SEL-ADDL-BEN-DET     (GAD-INDEX).00204800
204900     MOVE AGEQLLI   TO GAD-O-P-X-AGE-QUAL-IND-FROM    (GAD-INDEX).00204900
205000     MOVE AGEQLHI   TO GAD-O-P-X-AGE-QUAL-IND-TO      (GAD-INDEX).00205000
205100     MOVE RELPINDI  TO GAD-O-P-X-RELATIONSHIP-IND     (GAD-INDEX).00205100
205200                                                                  00205200
205300     MOVE PRTIMEFI  TO ACWA-DISPLAY-LEN-3-X.                      00205300
205400     MOVE ACWA-DISPLAY-LEN-3                                      00205400
205500                    TO  GAD-O-P-X-BEN-PER-TIME-FCTR   (GAD-INDEX).00205500
205600     MOVE CLMLVLII  TO  GAD-O-P-X-CLAIM-LVL-ACCUM-IND (GAD-INDEX).00205600
205700     MOVE INTRVALI  TO  ACWA-DISPLAY-LEN-3-X.                     00205700
205800     MOVE ACWA-DISPLAY-LEN-3                                      00205800
205900                    TO  GAD-O-P-X-INTERVAL-TIME-FCTR  (GAD-INDEX).00205900
206000     MOVE INTTYPEI  TO  GAD-O-P-X-INTERVAL-TYPE       (GAD-INDEX).00206000
206100     MOVE LOBI      TO  GAD-O-P-X-L-O-B               (GAD-INDEX).00206100
206200                                                                  00206200
206300     PERFORM 2600-000-PROCESS-VAL-LIMIT.                          00206300
206400                                                                  00206400
206500     MOVE ACWA-VALUE-LIMIT-9                                      00206500
206600                    TO  GAD-O-P-X-VALUE-LIMIT         (GAD-INDEX).00206600
206700     MOVE BENVLQLI  TO  GAD-O-P-X-VALUE-QUALIFIER     (GAD-INDEX).00206700
206800     MOVE PERLIMTI  TO  ACWA-DISPLAY-LEN-3-X.                     00206800
206900     MOVE ACWA-DISPLAY-LEN-3                                      00206900
207000                    TO  GAD-O-P-X-PERCENT-LEVEL       (GAD-INDEX).00207000
207100     MOVE NEWVALUI  TO  ACWA-DISPLAY-LEN-5-X.                     00207100
207200     MOVE ACWA-DISPLAY-LEN-5                                      00207200
207300                    TO  GAD-O-P-X-INTERVAL-OVRD-VALUE (GAD-INDEX).00207300
207400     MOVE OVRDINDI  TO  GAD-O-P-X-INTERVAL-OVRD-IND   (GAD-INDEX).00207400
207500     MOVE INTDESKI  TO  GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX).00207500
207600     MOVE CONDALLI  TO  GAD-COND-ALL-BIT              (GAD-INDEX).00207600
207700     MOVE CONDEXCI  TO  GAD-COND-EXCLUSION-BIT        (GAD-INDEX).00207700
207800     MOVE CONDICDI  TO  GAD-COND-ICD-BIT              (GAD-INDEX).00207800
207900     MOVE CONDTABI  TO  GAD-COND-TB-BIT               (GAD-INDEX).00207900
208000     MOVE CONDMENI  TO  GAD-COND-MENTAL-BIT           (GAD-INDEX).00208000
208100     MOVE CONDDRGI  TO  GAD-COND-DRUG-BIT             (GAD-INDEX).00208100
208200     MOVE CONDALCI  TO  GAD-COND-ALCOHOL-BIT          (GAD-INDEX).00208200
208300     MOVE CONDOBCI  TO  GAD-COND-OB-COMP-BIT          (GAD-INDEX).00208300
208400     MOVE CONDOBNI  TO  GAD-COND-OB-NORM-BIT          (GAD-INDEX).00208400
208500     MOVE CONDMALI  TO  GAD-COND-MALIGNANCY-BIT       (GAD-INDEX).00208500
208600     MOVE CONDCARI  TO  GAD-COND-CARDIAC-DISEASE-BIT  (GAD-INDEX).00208600
208700     MOVE CONDOBSI  TO  GAD-COND-OBESITY-BIT          (GAD-INDEX).00208700
208800     MOVE CONDKDYI  TO  GAD-COND-KIDNEY-DISEASE-BIT   (GAD-INDEX).00208800
208900     MOVE CONDACCI  TO  GAD-COND-ACCIDENT-BIT         (GAD-INDEX).00208900
209000     MOVE CONDPECI  TO  GAD-COND-PRE-EXIST-BIT        (GAD-INDEX).00209000
209100     MOVE CONDNEMI  TO  GAD-COND-NON-EMER-BIT         (GAD-INDEX).00209100
209200     MOVE CONDSUII  TO  GAD-COND-SUICIDE-BIT          (GAD-INDEX).00209200
209300     MOVE CONDTMJI  TO  GAD-COND-TMJ-BIT              (GAD-INDEX).00209300
209400     MOVE CONDINFI  TO  GAD-COND-INF-BIT              (GAD-INDEX).00209400
209500     MOVE CONDLIFI  TO  GAD-COND-LIFE-THREAT-BIT      (GAD-INDEX).00209500
209600     MOVE CONDEMCI  TO  GAD-COND-EMER-MED-BIT         (GAD-INDEX).00209600
209700     MOVE CONDEACI  TO  GAD-COND-EMER-ACC-BIT         (GAD-INDEX).00209700
209800     MOVE CONDSMII  TO  GAD-COND-SER-MEN-ILL-BIT      (GAD-INDEX).00209800
209900     MOVE CONDNSMI  TO  GAD-COND-NON-SER-MEN-ILL-BIT  (GAD-INDEX).00209900
210000                                                                  00210000
210100                                                                  00210100
210200     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00210200
210300         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00210300
210400         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00210400
210500         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00210500
210600         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00210600
210700         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00210700
210800     THEN                                                         00210800
210900        MOVE 1      TO  GAD-INTERNAL-TABULAR-COUNT     (GAD-INDEX)00210900
211000        MOVE HIGH-VALUES    TO   GAD-O-P-X-INTL-TAB-1  (GAD-INDEX)00211000
211100        SET  GAD-INDEX  UP BY  1                                  00211100
211200        MOVE HIGH-VALUES     TO  GAD-ENTRY (GAD-INDEX)            00211200
211300        SET GAD-ENTRY-COUNT  TO  GAD-INDEX                        00211300
211400        GO TO 2500-900-EXIT.                                      00211400
211500                                                                  00211500
211600                                                                  00211600
211700     IF IBGROPTI  =  'MT' OR 'A'                                  00211700
211800        MOVE 2           TO  GAD-INTERNAL-TABULAR-COUNT(GAD-INDEX)00211800
211900        MOVE HIGH-VALUES TO  GAD-O-P-X-INTL-TAB-2      (GAD-INDEX)00211900
212000        MOVE '#IBGR '   TO  GAD-O-P-X-INTL-TAB-TAB-ID-1(GAD-INDEX)00212000
212100        MOVE GAD-OCCURS-ENTRY-COUNTER                  (GAD-INDEX)00212100
212200                         TO  GAD-O-P-X-INTL-TAB-SLOT-1(GAD-INDEX).00212200
212300                                                                  00212300
212400     IF IDGDOPTI  =  'MT' OR 'A'                                  00212400
212500        MOVE 2           TO  GAD-INTERNAL-TABULAR-COUNT(GAD-INDEX)00212500
212600        MOVE HIGH-VALUES TO  GAD-O-P-X-INTL-TAB-2      (GAD-INDEX)00212600
212700        MOVE '#IDGD '   TO  GAD-O-P-X-INTL-TAB-TAB-ID-1(GAD-INDEX)00212700
212800        MOVE GAD-OCCURS-ENTRY-COUNTER                  (GAD-INDEX)00212800
212900                         TO  GAD-O-P-X-INTL-TAB-SLOT-1(GAD-INDEX).00212900
213000                                                                  00213000
213100     IF IPGNOPTI  =  'MT' OR 'A'                                  00213100
213200        MOVE 2           TO  GAD-INTERNAL-TABULAR-COUNT(GAD-INDEX)00213200
213300        MOVE HIGH-VALUES TO  GAD-O-P-X-INTL-TAB-2      (GAD-INDEX)00213300
213400        MOVE '#IPGN '   TO  GAD-O-P-X-INTL-TAB-TAB-ID-1(GAD-INDEX)00213400
213500        MOVE GAD-OCCURS-ENTRY-COUNTER                 (GAD-INDEX) 00213500
213600                         TO  GAD-O-P-X-INTL-TAB-SLOT-1(GAD-INDEX).00213600
213700                                                                  00213700
213800     IF IPGPOPTI  =  'MT' OR 'A'                                  00213800
213900        MOVE 2           TO  GAD-INTERNAL-TABULAR-COUNT(GAD-INDEX)00213900
214000        MOVE HIGH-VALUES TO  GAD-O-P-X-INTL-TAB-2      (GAD-INDEX)00214000
214100        MOVE '#IPGP '   TO  GAD-O-P-X-INTL-TAB-TAB-ID-1(GAD-INDEX)00214100
214200        MOVE GAD-OCCURS-ENTRY-COUNTER                 (GAD-INDEX) 00214200
214300                         TO  GAD-O-P-X-INTL-TAB-SLOT-1(GAD-INDEX).00214300
214400                                                                  00214400
214500     IF IPGTOPTI  =  'MT' OR 'A'                                  00214500
214600        MOVE 2           TO  GAD-INTERNAL-TABULAR-COUNT(GAD-INDEX)00214600
214700        MOVE HIGH-VALUES TO  GAD-O-P-X-INTL-TAB-2      (GAD-INDEX)00214700
214800        MOVE '#IPGT '   TO  GAD-O-P-X-INTL-TAB-TAB-ID-1(GAD-INDEX)00214800
214900        MOVE GAD-OCCURS-ENTRY-COUNTER                 (GAD-INDEX) 00214900
215000                         TO  GAD-O-P-X-INTL-TAB-SLOT-1(GAD-INDEX).00215000
215100                                                                  00215100
215200     IF IPGSOPTI  =  'MT' OR 'A'                                  00215200
215300        MOVE 2           TO  GAD-INTERNAL-TABULAR-COUNT(GAD-INDEX)00215300
215400        MOVE HIGH-VALUES TO  GAD-O-P-X-INTL-TAB-2      (GAD-INDEX)00215400
215500        MOVE '#IPGS '   TO  GAD-O-P-X-INTL-TAB-TAB-ID-1(GAD-INDEX)00215500
215600        MOVE GAD-OCCURS-ENTRY-COUNTER                 (GAD-INDEX) 00215600
215700                         TO  GAD-O-P-X-INTL-TAB-SLOT-1(GAD-INDEX).00215700
215800                                                                  00215800
215900                                                                  00215900
216000*******                                                           00216000
216100* STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  00216100
216200*     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THE00216200
216300*     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  00216300
216400*******                                                           00216400
216500                                                                  00216500
216600     IF  FRMNUIDI =  'GTM1'  AND  IBGROPTI =  'MT'                00216600
216700     THEN                                                         00216700
216800         MOVE MFRMSLTI  TO  IBGRSLTI  ACWA-DISPLAY-LEN-7          00216800
216900         SET  WT-01-INDEX                     TO +01              00216900
217000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00217000
217100         MOVE -1        TO  IBGROPTL                              00217100
217200         MOVE DFHBMABF  TO  IBGRSLTA  IBGRIDA                     00217200
217300         MOVE SPACES    TO  IBGROPTI                              00217300
217400         MOVE DFHBMUNP  TO  MFRMSLTA                              00217400
217500         MOVE ZEROS     TO  MFRMSLTI  MFRMSLTL                    00217500
217600         MOVE ACWA-DISPLAY-LEN-7                                  00217600
217700                        TO  GAD-O-P-X-INTL-TAB-SLOT-1(GAD-INDEX)  00217700
217800     ELSE                                                         00217800
217900         NEXT SENTENCE.                                           00217900
218000                                                                  00218000
218100     IF  FRMNUIDI =  'GTM1'  AND  IDGDOPTI =  'MT'                00218100
218200     THEN                                                         00218200
218300         MOVE MFRMSLTI  TO  IDGDSLTI  ACWA-DISPLAY-LEN-7          00218300
218400         SET  WT-01-INDEX                     TO +04              00218400
218500         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00218500
218600         MOVE -1        TO  IDGDOPTL                              00218600
218700         MOVE DFHBMABF  TO  IDGDSLTA  IBGRIDA                     00218700
218800         MOVE SPACES    TO  IDGDOPTI                              00218800
218900         MOVE DFHBMUNP  TO  MFRMSLTA                              00218900
219000         MOVE ZEROS     TO  MFRMSLTI  MFRMSLTL                    00219000
219100         MOVE ACWA-DISPLAY-LEN-7                                  00219100
219200                        TO  GAD-O-P-X-INTL-TAB-SLOT-1(GAD-INDEX)  00219200
219300     ELSE                                                         00219300
219400         NEXT SENTENCE.                                           00219400
219500                                                                  00219500
219600     IF  FRMNUIDI =  'GTM1'  AND  IPGNOPTI =  'MT'                00219600
219700     THEN                                                         00219700
219800         MOVE MFRMSLTI  TO  IPGNSLTI  ACWA-DISPLAY-LEN-7          00219800
219900         SET  WT-01-INDEX                     TO +02              00219900
220000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00220000
220100         MOVE -1        TO  IPGNOPTL                              00220100
220200         MOVE DFHBMABF  TO  IPGNSLTA  IPGNIDA                     00220200
220300         MOVE SPACES    TO  IPGNOPTI                              00220300
220400         MOVE DFHBMUNP  TO  MFRMSLTA                              00220400
220500         MOVE ZEROS     TO  MFRMSLTI  MFRMSLTL                    00220500
220600         MOVE ACWA-DISPLAY-LEN-7                                  00220600
220700                        TO  GAD-O-P-X-INTL-TAB-SLOT-1(GAD-INDEX)  00220700
220800     ELSE                                                         00220800
220900         NEXT SENTENCE.                                           00220900
221000                                                                  00221000
221100     IF  FRMNUIDI =  'GTM1'  AND  IPGPOPTI =  'MT'                00221100
221200     THEN                                                         00221200
221300         MOVE MFRMSLTI  TO  IPGPSLTI  ACWA-DISPLAY-LEN-7          00221300
221400         SET  WT-01-INDEX                     TO +05              00221400
221500         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00221500
221600         MOVE -1        TO  IPGPOPTL                              00221600
221700         MOVE DFHBMABF  TO  IPGPSLTA  IPGNIDA                     00221700
221800         MOVE SPACES    TO  IPGPOPTI                              00221800
221900         MOVE DFHBMUNP  TO  MFRMSLTA                              00221900
222000         MOVE ZEROS     TO  MFRMSLTI  MFRMSLTL                    00222000
222100         MOVE ACWA-DISPLAY-LEN-7                                  00222100
222200                        TO  GAD-O-P-X-INTL-TAB-SLOT-1(GAD-INDEX)  00222200
222300     ELSE                                                         00222300
222400         NEXT SENTENCE.                                           00222400
222500                                                                  00222500
222600     IF  FRMNUIDI =  'GTM1'  AND  IPGTOPTI =  'MT'                00222600
222700     THEN                                                         00222700
222800         MOVE MFRMSLTI  TO  IPGTSLTI  ACWA-DISPLAY-LEN-7          00222800
222900         SET  WT-01-INDEX                     TO +03              00222900
223000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00223000
223100         MOVE -1        TO  IPGTOPTL                              00223100
223200         MOVE DFHBMABF  TO  IPGTSLTA  IPGTIDA                     00223200
223300         MOVE SPACES    TO  IPGTOPTI                              00223300
223400         MOVE DFHBMUNP  TO  MFRMSLTA                              00223400
223500         MOVE ZEROS     TO  MFRMSLTI  MFRMSLTL                    00223500
223600         MOVE ACWA-DISPLAY-LEN-7                                  00223600
223700                        TO  GAD-O-P-X-INTL-TAB-SLOT-1(GAD-INDEX)  00223700
223800     ELSE                                                         00223800
223900         NEXT SENTENCE.                                           00223900
224000                                                                  00224000
224100     IF  FRMNUIDI =  'GTM1'  AND  IPGSOPTI =  'MT'                00224100
224200     THEN                                                         00224200
224300         MOVE MFRMSLTI  TO  IPGSSLTI  ACWA-DISPLAY-LEN-7          00224300
224400         SET  WT-01-INDEX                     TO +03              00224400
224500         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00224500
224600         MOVE -1        TO  IPGSOPTL                              00224600
224700         MOVE DFHBMABF  TO  IPGSSLTA  IPGSIDA                     00224700
224800         MOVE SPACES    TO  IPGSOPTI                              00224800
224900         MOVE DFHBMUNP  TO  MFRMSLTA                              00224900
225000         MOVE ZEROS     TO  MFRMSLTI  MFRMSLTL                    00225000
225100         MOVE ACWA-DISPLAY-LEN-7                                  00225100
225200                        TO  GAD-O-P-X-INTL-TAB-SLOT-1(GAD-INDEX)  00225200
225300     ELSE                                                         00225300
225400         NEXT SENTENCE.                                           00225400
225500*******                                                          |00225500
225600* STS *----------------------------------------------------------*00225600
225700*******                                                           00225700
225800                                                                  00225800
225900                                                                  00225900
226000     SET  GAD-INDEX  UP BY  1.                                    00226000
226100     MOVE HIGH-VALUES     TO  GAD-ENTRY (GAD-INDEX).              00226100
226200     SET GAD-ENTRY-COUNT  TO  GAD-INDEX.                          00226200
226300                                                                  00226300
226400 2500-900-EXIT. EXIT.                                             00226400
226500                                                                  00226500
226600/*****************************************************************00226600
226700*         P R O C E S S   V A L U E   L I M I T                   00226700
226800******************************************************************00226800
226900 2600-000-PROCESS-VAL-LIMIT     SECTION.                          00226900
227000 2600-010.                                                        00227000
227100                                                                  00227100
227200     IF (ACWA-VAL-LIM-SCREEN-NEG1-3  =  'NEG' OR                  00227200
227300         ACWA-VAL-LIM-SCREEN-NEG2-3  =  'NEG') OR                 00227300
227200        (ACWA-VAL-LIM-SCREEN-NEG1-3  =  'UNL' OR                  00227310
227300         ACWA-VAL-LIM-SCREEN-NEG2-3  =  'UNL')                    00227320
227400         GO TO 2600-900-EXIT.                                     00227400
227500                                                                  00227500
227600     IF  ACWA-BNMXVALI-N NUMERIC                                  00227600
227700     THEN                                                         00227700
227800         IF  BENVLQLI  =  '5'                                     00227800
227900         THEN                                                     00227900
228000             MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-7         00228000
228100             MOVE ZEROS            TO  ACWA-VALUE-LIMIT-2         00228100
228200             GO TO 2600-900-EXIT                                  00228200
228300         ELSE                                                     00228300
228400             MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-9-9       00228400
228500             GO TO 2600-900-EXIT                                  00228500
228600     ELSE                                                         00228600
228700         NEXT SENTENCE.                                           00228700
228800                                                                  00228800
228900     IF  ACWA-VAL-LIM-SCREEN-1  =  '.'                            00228900
229000         MOVE ACWA-VAL-LIM-SCREEN-7  TO  ACWA-VALUE-LIMIT-7       00229000
229100         MOVE ACWA-VAL-LIM-SCREEN-2  TO  ACWA-VALUE-LIMIT-2       00229100
229200         GO TO 2600-900-EXIT.                                     00229200
229300                                                                  00229300
229400 2600-900-EXIT. EXIT.                                             00229400
229500                                                                  00229500
229600/*****************************************************************00229600
229700* 3000 UPDATE GAD RECORD                                         *00229700
229800*                                                                *00229800
229900*    THIS ROUTINE REWRITES THE UPDATED RECORD TO THE WORK FILE.  *00229900
230000******************************************************************00230000
230100 3000-000-UPDATE-GAD-RECORD     SECTION.                          00230100
230200 3000-010.                                                        00230200
230300                                                                  00230300
230400     COMPUTE  GCIO-RECORD-LENGTH   =                              00230400
230500         GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-AOL-FIXED-LEN  +     00230500
230600         (GC-GCTABULR-AOL-VARY-LEN  *  GAD-ENTRY-COUNT).          00230600
230700                                                                  00230700
230800     MOVE  GC-GCIO-ACCESS-CODE-WU  TO  GCIO-FILE-ACCESS-CODE.     00230800
230900                                                                  00230900
231000     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00231000
231100                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00231100
231200                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)  END-EXEC.     00231200
231300                                                                  00231300
231400     IF NOT GCIO-GOOD-RETURN                                      00231400
231500        MOVE WS-ABCODE-1EF4        TO WS-ABCODE                   00231500
231600        MOVE WS-ABCODE-1EF4-MSG    TO WS-ABCODE-MSG               00231600
231700        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00231700
231800                                                                  00231800
231900 3000-900-EXIT. EXIT.                                             00231900
232000                                                                  00232000
232100/*****************************************************************00232100
232200* 3100  READ RECORD                                              *00232200
232300*                                                                *00232300
232400*    THIS ROUTINE READS THE RECORD THAT CORRESPONDS TO THE KEY   *00232400
232500*  FIELDS FOUND ON THE SCREEN'S HEADING.                         *00232500
232600******************************************************************00232600
232700 3100-000-READ-RECORD           SECTION.                          00232700
232800 3100-010.                                                        00232800
232900                                                                  00232900
233000     COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   00233000
233100              GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-AOL-FIXED-LEN  +00233100
233200             (GC-GCTABULR-AOL-VARY-LEN  *                         00233200
233300                                   GC-GCTABULR-AOL-VARY-MAX-OCUR).00233300
233400                                                                  00233400
233500     IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       00233500
233600        NEXT SENTENCE                                             00233600
233700     ELSE                                                         00233700
233800        EXEC CICS GETMAIN                                         00233800
233900               SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      00233900
234000               INITIMG(WS-HEX-00)                                 00234000
234100               LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 00234100
234200               END-EXEC                                           00234200
234300        SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     00234300
234400                 ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        00234400
234500                                                                  00234500
234600                                                                  00234600
234700     IF FRMNUIDI  =  'GS3A'                                       00234700
234800        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00234800
234900     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00234900
235000        PERFORM 6100-000-BUILD-CONTRACT-KEY.                      00235000
235100     IF FRMNUIDI  =  'GC8A'                                       00235100
235200        PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      00235200
235300                                                                  00235300
235400     IF GCIO-WORKFILE-KEY  =  WORK-RECORD-KEY                     00235400
235500        GO TO 3100-900-EXIT.                                      00235500
235600                                                                  00235600
235700     MOVE GC-GCPSWORK-DDNAME      TO  GCIO-FILE-DDNAME.           00235700
235800     MOVE GC-GCIO-AREA-1          TO  GCIO-IO-AREA-TO-USE.        00235800
235900     MOVE GCIO-WORKFILE-KEY       TO  GCIO-FILE-KEY.              00235900
236000     MOVE GC-GCIO-ACCESS-CODE-RD  TO  GCIO-FILE-ACCESS-CODE.      00236000
236100     MOVE GC-GCTABULR-AOL-VARY-MAX-OCUR  TO  GAD-ENTRY-COUNT.     00236100
236200                                                                  00236200
236300     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00236300
236400                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00236400
236500                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)     END-EXEC.  00236500
236600                                                                  00236600
236700     IF NOT GCIO-GOOD-RETURN                                      00236700
236800        MOVE WS-ABCODE-1EF7        TO WS-ABCODE                   00236800
236900        MOVE WS-ABCODE-1EF7-MSG    TO WS-ABCODE-MSG               00236900
237000        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00237000
237100                                                                  00237100
237200 3100-900-EXIT. EXIT.                                             00237200
237300                                                                  00237300
237400/*****************************************************************00237400
237500* 3200  READ REC FOR UPDATE                                      *00237500
237600*                                                                *00237600
237700*    THIS ROUTINE READS THE RECORD FOR UPDATE THAT CORRESPONDS   *00237700
237800*  TO THE KEY FIELDS FOUND ON THE SCREEN'S HEADING.              *00237800
237900******************************************************************00237900
238000 3200-000-READ-REC-FOR-UPDATE   SECTION.                          00238000
238100 3200-010.                                                        00238100
238200                                                                  00238200
238300     COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   00238300
238400              GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-AOL-FIXED-LEN  +00238400
238500             (GC-GCTABULR-AOL-VARY-LEN  *                         00238500
238600                                   GC-GCTABULR-AOL-VARY-MAX-OCUR).00238600
238700                                                                  00238700
238800     IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       00238800
238900        NEXT SENTENCE                                             00238900
239000     ELSE                                                         00239000
239100        EXEC CICS GETMAIN                                         00239100
239200               SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      00239200
239300               INITIMG(WS-HEX-00)                                 00239300
239400               LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 00239400
239500               END-EXEC                                           00239500
239600        SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     00239600
239700                 ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        00239700
239800                                                                  00239800
239900                                                                  00239900
240000     IF FRMNUIDI  =  'GS3A'                                       00240000
240100        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00240100
240200     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00240200
240300        PERFORM 6100-000-BUILD-CONTRACT-KEY.                      00240300
240400     IF FRMNUIDI  =  'GC8A'                                       00240400
240500        PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      00240500
240600                                                                  00240600
240700     MOVE GC-GCPSWORK-DDNAME    TO  GCIO-FILE-DDNAME.             00240700
240800     MOVE GC-GCIO-AREA-1        TO  GCIO-IO-AREA-TO-USE.          00240800
240900     MOVE GCIO-WORKFILE-KEY     TO  GCIO-FILE-KEY.                00240900
241000     MOVE GC-GCIO-ACCESS-CODE-RU  TO  GCIO-FILE-ACCESS-CODE.      00241000
241100     MOVE GC-GCTABULR-AOL-VARY-MAX-OCUR  TO  GAD-ENTRY-COUNT.     00241100
241200                                                                  00241200
241300     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00241300
241400                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00241400
241500                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)  END-EXEC.     00241500
241600                                                                  00241600
241700 3200-900-EXIT. EXIT.                                             00241700
241800                                                                  00241800
241900/*****************************************************************00241900
242000* 4000  DISPLAY FIRST SCREEN                                     *00242000
242100*                                                                *00242100
242200*    THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM ONE OF THE*00242200
242300*  TABULAR MENUS, THE MENU WILL READ THE ALL LEVEL TABULAR IF IT *00242300
242400*  EXISTS (IF IT DOESN'T EXIST THE MENU WILL ADD A NEW ONE TO THE*00242400
242500*  WORK FILE) THEN PLACE THE ADDRESS OF THE TABULAR RECORD WITHIN*00242500
242600*  A COMMON AREA PARAMETER LIST.  THE MENU THEN MOVES THE KEY    *00242600
242700*  FIELDS TO THE COMMON AREA AND PASSES THE ADDRESS OF THE       *00242700
242800*  PARAMETER LIST IN A FULLWORD TO THIS PROGRAM.                 *00242800
242900*    WE THEN SET THIS ADDRESS INTO A BLL CELL AND ACCESS THE     *00242900
243000*  INFORMATION NEEDED TO BUILD THE SCREEN IMAGE.                 *00243000
243100******************************************************************00243100
243200 4000-000-DISPLAY-FIRST-SCREEN  SECTION.                          00243200
243300 4000-010.                                                        00243300
243400                                                                  00243400
243500     IF EIBCALEN  >  0                                            00243500
243600        NEXT SENTENCE                                             00243600
243700     ELSE                                                         00243700
243800        MOVE WS-ABCODE-1EC1        TO  WS-ABCODE                  00243800
243900        MOVE WS-ABCODE-1EC1-MSG    TO  WS-ABCODE-MSG              00243900
244000        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00244000
244100                                                                  00244100
244200*    SET ADDRESS OF COMMUNICATION-KEY-AREA  TO                    00244200
244300*                   COMMAREA-RECORD-POINTER.                      00244300
244400                                                                  00244400
244500*    SET  ACWA-COMM-KEY-PNTR       TO                             00244500
244600*                   ADDRESS OF COMMUNICATION-KEY-AREA.            00244600
244700     MOVE LOW-VALUES               TO GA1XI01I.                   00244700
244800                                                                  00244800
244900     MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         00244900
245000                                                                  00245000
245100     IF GCA-FROM-MENU-ID  =  'GS3A'                               00245100
245200        MOVE GCA-PLAN-CODE             TO GRP-SPEC-PLAN-CODE      00245200
245300        MOVE GCA-GROUP-NUM             TO GRP-SPEC-GROUP-NUM      00245300
245400        MOVE GCA-SECTION-NUM           TO GRP-SPEC-SECTION-NUM    00245400
245500        MOVE GCA-PKG-CODE              TO GRP-SPEC-PKG-CODE       00245500
245600        MOVE GCA-FAM-REL-LVL            TO  GRP-SPEC-FAM-REL-LVL  00245600
245700        MOVE GCA-EFFECTIVE-DATE         TO  GRP-SPEC-EFF-DATE     00245700
245800        MOVE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'         00245800
245900                                        TO  TTLELNEO              00245900
246000*AB*****MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              00246000
246100        MOVE GROUP-SPECIFIC-ID-LINE     TO  IDLINEO.              00246100
246200                                                                  00246200
246300     IF GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                     00246300
246400        MOVE GCA-PLAN-CODE             TO CONTRACT-PLAN-CODE      00246400
246500        MOVE GCA-GROUP-NUM             TO CONTRACT-GROUP-NUM      00246500
246600        MOVE GCA-SECTION-NUM           TO CONTRACT-SECTION-NUM    00246600
246700        MOVE GCA-PKG-CODE              TO CONTRACT-PKG-CODE       00246700
246800        MOVE GCA-L-O-B                 TO  CONTRACT-LOB           00246800
246900        MOVE GCA-PROV-CTL              TO  CONTRACT-PROV-CTL      00246900
247000        MOVE GCA-FAM-REL-LVL           TO  CONTRACT-FAM-REL-LVL   00247000
247100        MOVE GCA-EFFECTIVE-DATE        TO  CONTRACT-EFF-DATE      00247100
247200        MOVE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'          00247200
247300                                        TO  TTLELNEO              00247300
247400*AB*****MOVE CONTRACT-TITLE-LINE        TO  TTLELNEO              00247400
247500        MOVE CONTRACT-ID-LINE           TO  IDLINEO.              00247500
247600                                                                  00247600
247700     IF GCA-FROM-MENU-ID  =  'GC8A'                               00247700
247800        MOVE GCA-PLAN-CODE             TO BEN-PROV-PLAN-CODE      00247800
247900        MOVE GCA-GROUP-NUM             TO BEN-PROV-GROUP-NO       00247900
248000        MOVE GCA-SECTION-NUM           TO BEN-PROV-SECTION-NO     00248000
248100        MOVE GCA-PKG-CODE              TO BEN-PROV-PKG-CODE       00248100
248200        MOVE GCA-L-O-B                  TO  BEN-PROV-LOB          00248200
248300        MOVE GCA-PROV-CTL               TO  BEN-PROV-PROV-CTL     00248300
248400        MOVE GCA-FAM-REL-LVL            TO  BEN-PROV-FAM-REL-LVL  00248400
248500        MOVE GCA-EFFECTIVE-DATE         TO  BEN-PROV-EFF-DATE     00248500
248600        MOVE GCA-BEN-PROV-ID            TO  BEN-PROV-ID-NO        00248600
248700        MOVE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'         00248700
248800                                        TO  TTLELNEO              00248800
248900*AB*****MOVE BENEFIT-PROVISION-TITLE-LINE TO  TTLELNEO            00248900
249000        MOVE BENEFIT-PROVISION-ID-LINE  TO  IDLINEO.              00249000
249100                                                                  00249100
249200     MOVE 'GA1E'            TO   FUNCTONO.                        00249200
249300     MOVE '001E00'          TO   SCRNIDNO.                        00249300
249400     MOVE AOL-TITLE-LINE    TO   TITLEO.                          00249400
249500                                                                  00249500
249600     MOVE GCA-RECORD-POINTER-COMP  TO ACWA-WF-ALL-LEVEL-TAB-COMP. 00249600
249700     SET ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD  TO             00249700
249800                    GCA-RECORD-POINTER.                           00249800
249900                                                                  00249900
250000     IF  NOT WRK-STAT-CONT-MAINT AND                              00250000
250100         NOT WRK-STAT-GRP-SPEC-MAINT                              00250100
250200     THEN                                                         00250200
250300         MOVE WS-ABCODE-1EC2        TO  WS-ABCODE                 00250300
250400         MOVE WS-ABCODE-1EC2-MSG    TO  WS-ABCODE-MSG             00250400
250500         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00250500
250600                                                                  00250600
250700     IF  NOT WRK-REC-GROUP-SPEC-TAB AND                           00250700
250800         NOT WRK-REC-CONT-TAB       AND                           00250800
250900         NOT WRK-REC-CONT-BEN-TAB-PROV                            00250900
251000     THEN                                                         00251000
251100         MOVE WS-ABCODE-1EC3        TO  WS-ABCODE                 00251100
251200         MOVE WS-ABCODE-1EC3-MSG    TO  WS-ABCODE-MSG             00251200
251300         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00251300
251400                                                                  00251400
251500     MOVE GAD-ENTRY-COUNT         TO  GAD-ENTRY-COUNT.            00251500
251600     MOVE GCA-ALL-LEVEL-TAB-ID    TO  TABIDO.                     00251600
251700     MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  TABSLTNO.                   00251700
251800     SET GAD-INDEX  TO  1.                                        00251800
251900                                                                  00251900
252000     IF EIBTRNID  =  'GC4A' OR  'GC3A' OR  'GC8A'  OR 'GTM1'      00252000
252100        MOVE '0000000'  TO GCA-OCCURS-ENTRY-COUNTER.              00252100
252200                                                                  00252200
252300     IF GCA-OCCURS-ENTRY-COUNTER  =  '0000000'                    00252300
252400        GO TO 4000-200-BUILD-SCREEN.                              00252400
252500                                                                  00252500
252600     MOVE GCA-OCCURS-ENTRY-COUNTER  TO  ACWA-DISPLAY-LEN-7.       00252600
252700                                                                  00252700
252800 4000-100-FIND-RIGHT-OCCURS.                                      00252800
252900     IF GAD-O-P-X-BENEFIT-PERIOD(GAD-INDEX)  NOT = HIGH-VALUES AND00252900
253000        GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX)  NOT =                00253000
253100                                                ACWA-DISPLAY-LEN-700253100
253200     THEN                                                         00253200
253300         IF GAD-INDEX  <  GAD-ENTRY-COUNT                         00253300
253400            SET GAD-INDEX  UP BY  1                               00253400
253500            GO TO 4000-100-FIND-RIGHT-OCCURS                      00253500
253600         ELSE                                                     00253600
253700             MOVE WS-ABCODE-1EL1       TO  WS-ABCODE              00253700
253800             MOVE WS-ABCODE-1EL1-MSG   TO  WS-ABCODE-MSG          00253800
253900             MOVE -1                   TO  MFRMSLTL               00253900
254000             PERFORM 9800-000-ERROR-MSG-THEN-ABEND                00254000
254100     ELSE                                                         00254100
254200         NEXT SENTENCE.                                           00254200
254300                                                                  00254300
254400                                                                  00254400
254500 4000-200-BUILD-SCREEN.                                           00254500
254600                                                                  00254600
254700     IF  GCA-ADD-DEL-IND  =  'A' OR                               00254700
254800         GAD-ENTRY-COUNT  =  1                                    00254800
254900     THEN                                                         00254900
255000         MOVE 'CHG/ADD'  TO  DELADDO                              00255000
255100         MOVE DFHBMASD   TO  DLOPTLTA,  DELOPTNA                  00255100
255200         IF  GCA-OCCURS-ENTRY-COUNTER  =  '0000000'               00255200
255300         THEN                                                     00255300
255400             PERFORM 4100-000-DISPLAY-SKELETON                    00255400
255500         ELSE                                                     00255500
255600             PERFORM 4400-000-BUILD-DISPLAY                       00255600
255700     ELSE                                                         00255700
255800         MOVE 'CHG/DEL'  TO  DELADDO                              00255800
255900         MOVE 'D'        TO  DELOLITO                             00255900
256000         PERFORM 4400-000-BUILD-DISPLAY.                          00256000
256100                                                                  00256100
256200 4000-900-EXIT. EXIT.                                             00256200
256300                                                                  00256300
256400/*****************************************************************00256400
256500* 4100  DISPLAY SKELETON                                         *00256500
256600*                                                                *00256600
256700*    THIS ROUTINE REINITIALIZES THE SCREEN FOR THE OPERATOR      *00256700
256800*  AFTER THEY HAVE REVIEWED THE ENTRY THEY JUST ADDED AND        *00256800
256900*  INDICATED THAT THEY WANTED TO ADD MORE BY KEYING 'ENTER'.     *00256900
257000******************************************************************00257000
257100 4100-000-DISPLAY-SKELETON      SECTION.                          00257100
257200 4100-010.                                                        00257200
257300                                                                  00257300
257400     MOVE SPACES TO ERRMSGO.                                      00257400
257500                                                                  00257500
257600     MOVE DFHBMFSE  TO  PERIODA.                                  00257600
257700                                                                  00257700
257800     MOVE DFHBMUNP  TO  BENVLQLA FAMINDIA  INTDESKA  LOBA         00257800
257900                        IBGROPTA IPGNOPTA  IPGTOPTA  MFRMSLTA     00257900
258000                        IDGDOPTA IPGPOPTA  IPGSOPTA.              00258000
258100                                                                  00258100
258200     MOVE ALL '_'  TO  PERIODO   BENVLQLO  LOBO                   00258200
258300                       FAMINDIO  PLCTRMTO.                        00258300
258400                                                                  00258400
258500     MOVE LOW-VALUES  TO  INTDESKO  MFRMSLTO  IDGDOPTO IPGPOPTO   00258500
258600                          IBGROPTO  IPGNOPTO  IPGTOPTO IPGSOPTO.  00258600
258700                                                                  00258700
258800     MOVE ZEROS  TO  COPAYINO   CSTCONTO  PERTQALO  ASCDSCDO      00258800
258900                     DAYFACIO   SRVGRUPO  PRTIMEFO  PERLIMTO      00258900
259000           CARYOVRO  CLMLVLIO  INTRVALO  INTTYPEO  BNMXVALO       00259000
259100           FYIVALO   OVRDINDO  NEWVALUO  DEFINTNO  CONDLIFO       00259100
259200           CONDALLO  CONDEXCO  CONDICDO  CONDTABO  CONDMENO       00259200
259300           CONDEMCO  CONDEACO  CONDSMIO  CONDNSMO                 00259300
259400           CONDDRGO  CONDALCO  CONDOBNO  CONDOBCO  CONDMALO       00259400
259500           CONDCARO  CONDOBSO  CONDKDYO  CONDACCO  CONDSUIO       00259500
259600          CONDPECO CONDNEMO  BISNDINO  CONDTMJO  CONDINFO AGEQLLO 00259600
259700          OENTCTRO IBGRSLTO  IPGNSLTO  IPGTSLTO  TOCURANO AGEQLHO 00259700
259800          IDGDSLTO IPGPSLTO  AGELIMLO  AGELIMHO  RELPINDO         00259800
259900          FEAKINDO IPGSSLTO  ACCUMIDO  CAPINDO   SABDINDO         00259900
259900          BENTYPO  TIERCDO   TIERLVO.                             00259910
                                                                        00259920
260000                                                                  00260000
260100     MOVE '01'    TO  COCURANO.                                   00260100
260200     MOVE -1      TO  PERIODL.                                    00260200
260300                                                                  00260300
260400     PERFORM 9000-000-SEND-ERASE-RETURN.                          00260400
260500                                                                  00260500
260600 4100-900-EXIT. EXIT.                                             00260600
260700                                                                  00260700
260800/*****************************************************************00260800
260900* 4200 DISPLAY NEXT                                              *00260900
261000*                                                                *00261000
261100*    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *00261100
261200*  SCREEN'S DISPLAY AND THEN POSITION TO THE NEXT ENTRY, IF THE  *00261200
261300*  NEXT ENTRY IS THE LAST IN THE LIST THE CODE WILL RECOGNIZE    *00261300
261400*  THAT AND POSITION TO THE FIRST ENTRY, ALSO DISPLAYING AN      *00261400
261500*  INFORMATIONAL MESSAGE.                                        *00261500
261600******************************************************************00261600
261700 4200-000-DISPLAY-NEXT          SECTION.                          00261700
261800 4200-010.                                                        00261800
261900                                                                  00261900
262000     MOVE GAD-ENTRY-COUNT  TO  GAD-ENTRY-COUNT.                   00262000
262100     SET GAD-INDEX         TO  1.                                 00262100
262200     MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                00262200
262300                                                                  00262300
262400                                                                  00262400
262500     SET  CURNT-OCURS-BIN  TO  GAD-INDEX.                         00262500
262600     MOVE CURNT-OCURS-BIN  TO  CURNT-OCURS-PKD.                   00262600
262700     MOVE CURNT-OCCURS-OUT TO  COCURANO.                          00262700
262800                                                                  00262800
262900     IF GAD-ENTRY-COUNT  >  1                                     00262900
263000        COMPUTE  TOTAL-OCURS-UNK  =  GAD-ENTRY-COUNT  -  1        00263000
263100        MOVE  TOTAL-OCCURS-OUT TO TOCURANO                        00263100
263200     ELSE                                                         00263200
263300        MOVE  '01'             TO TOCURANO.                       00263300
263400                                                                  00263400
263500                                                                  00263500
263600 4200-100-FIND-RIGHT-OCCURS.                                      00263600
263700                                                                  00263700
263800     IF GAD-O-P-X-BENEFIT-PERIOD(GAD-INDEX)  NOT = HIGH-VALUES AND00263800
263900        GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX)  NOT =                00263900
264000                                                ACWA-DISPLAY-LEN-700264000
264100     THEN                                                         00264100
264200         IF  GAD-INDEX  <  (GAD-ENTRY-COUNT - 1)                  00264200
264300         THEN                                                     00264300
264400             SET GAD-INDEX  UP BY  1                              00264400
264500             GO TO 4200-100-FIND-RIGHT-OCCURS                     00264500
264600         ELSE                                                     00264600
264700             SET GAD-INDEX  TO  1                                 00264700
264800             SET  WT-01-INDEX                     TO +20          00264800
264900             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00264900
265000     ELSE                                                         00265000
265100         IF  GAD-INDEX  <  (GAD-ENTRY-COUNT - 1)                  00265100
265200         THEN                                                     00265200
265300             SET GAD-INDEX  UP BY  1                              00265300
265400         ELSE                                                     00265400
265500             SET GAD-INDEX  TO  1                                 00265500
265600             SET  WT-01-INDEX                     TO +20          00265600
265700             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.     00265700
265800                                                                  00265800
265900     PERFORM 4400-000-BUILD-DISPLAY.                              00265900
266000                                                                  00266000
266100 4200-900-EXIT. EXIT.                                             00266100
266200                                                                  00266200
266300/*****************************************************************00266300
266400* 4300 DISPLAY PREV                                              *00266400
266500*                                                                *00266500
266600*    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *00266600
266700*  SCREEN'S DISPLAY AND THEN POSITION TO THE NEXT PREVIOUS ENTRY,*00266700
266800*  IF THE CURRENT ENTRY IS THE FIRST IN THE LIST THE CODE WILL   *00266800
266900*  RECOGNIZE THAT AND POSITION TO THE LAST ENTRY, ALSO DISPLAYING*00266900
267000*  AN INFORMATIONAL MESSAGE.                                     *00267000
267100******************************************************************00267100
267200 4300-000-DISPLAY-PREV          SECTION.                          00267200
267300 4300-010.                                                        00267300
267400                                                                  00267400
267500     MOVE GAD-ENTRY-COUNT  TO  GAD-ENTRY-COUNT.                   00267500
267600     SET  GAD-INDEX        TO  1.                                 00267600
267700     MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                00267700
267800                                                                  00267800
267900 4300-100-FIND-RIGHT-OCCURS.                                      00267900
268000                                                                  00268000
268100     IF GAD-O-P-X-BENEFIT-PERIOD(GAD-INDEX)  NOT = HIGH-VALUES AND00268100
268200        GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX)  NOT =                00268200
268300                                                ACWA-DISPLAY-LEN-700268300
268400     THEN                                                         00268400
268500         IF  GAD-INDEX  <  (GAD-ENTRY-COUNT - 1)                  00268500
268600         THEN                                                     00268600
268700             SET GAD-INDEX  UP BY  1                              00268700
268800             GO TO 4300-100-FIND-RIGHT-OCCURS                     00268800
268900         ELSE                                                     00268900
269000             SET GAD-INDEX  TO  1                                 00269000
269100             SET  WT-01-INDEX                     TO +20          00269100
269200             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00269200
269300     ELSE                                                         00269300
269400         IF  GAD-INDEX  NOT =  1                                  00269400
269500         THEN                                                     00269500
269600             SET GAD-INDEX  DOWN BY  1                            00269600
269700         ELSE                                                     00269700
269800             SET GAD-INDEX  TO  GAD-ENTRY-COUNT                   00269800
269900             SET GAD-INDEX  DOWN BY  1                            00269900
270000             SET  WT-01-INDEX                     TO +21          00270000
270100             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.     00270100
270200                                                                  00270200
270300     PERFORM 4400-000-BUILD-DISPLAY.                              00270300
270400                                                                  00270400
270500 4300-900-EXIT. EXIT.                                             00270500
270600                                                                  00270600
270700/*****************************************************************00270700
270800* 4400 BUILD DISPLAY                                             *00270800
270900*                                                                *00270900
271000*    THIS ROUTINE WILL MOVE ALL THE FIELDS FROM THE OCCURENCE    *00271000
271100*  SPECIFIED BY INDEX GAD-INDEX TO THE SCREEN.                   *00271100
271200******************************************************************00271200
271300 4400-000-BUILD-DISPLAY         SECTION.                          00271300
271400 4400-010.                                                        00271400
271500                                                                  00271500
271600     IF DELADDI  =  'CHG/DEL'                                     00271600
271700        MOVE 'D'  TO  DELOLITO                                    00271700
271800     ELSE                                                         00271800
271900        MOVE SPACE  TO  DELOLITO.                                 00271900
272000                                                                  00272000
272100     MOVE SPACE                                       TO DELOPTNO.00272100
272200     MOVE GAD-OCCURS-ENTRY-COUNTER      (GAD-INDEX)  TO           00272200
272300                                               ACWA-DISPLAY-LEN-7.00272300
272400     MOVE ACWA-DISPLAY-LEN-7                         TO  OENTCTRO.00272400
272500     MOVE GAD-O-P-X-DAY-FACTOR-IND      (GAD-INDEX)  TO  DAYFACIO.00272500
272600     MOVE GAD-O-P-X-CO-PAY-IND          (GAD-INDEX)  TO  COPAYINO.00272600
272700     MOVE GAD-O-P-X-BISCENDING-IND      (GAD-INDEX)  TO  BISNDINO.00272700
272800     MOVE GAD-CARRY-OVER-CREDIT-IND     (GAD-INDEX)  TO  CARYOVRO.00272800
672900     MOVE GAD-O-P-X-ASCEND-DESCEND-IND  (GAD-INDEX)  TO  ASCDSCDO.00272900
      *P21595 CHANGES STARTS                                            00272910
672900     MOVE GAD-O-P-X-BEN-TYPE            (GAD-INDEX)  TO  BENTYPO. 00272920
672900     MOVE GAD-O-P-X-TIER-CODE           (GAD-INDEX)  TO  TIERCDO. 00272921
672900     MOVE GAD-O-P-X-TIER-LVL            (GAD-INDEX)  TO  TIERLVO. 00272922
      *P21595 CHANGES ENDS                                              00272930
273000     MOVE GAD-O-P-X-DEFINITION          (GAD-INDEX)  TO  DEFINTNO.00273000
273100     MOVE GAD-O-P-X-COST-CONTAIN-IND    (GAD-INDEX)  TO  CSTCONTO.00273100
273200     MOVE GAD-O-P-X-BENEFIT-PERIOD      (GAD-INDEX)  TO  PERIODO. 00273200
273300     MOVE GAD-O-P-X-BEN-PER-TIME-QUAL   (GAD-INDEX)  TO  PERTQALO.00273300
273400     MOVE GAD-O-P-X-FAM-OR-INDIV        (GAD-INDEX)  TO  FAMINDIO.00273400
273500     MOVE GAD-O-P-X-PLACE-OF-TREATMENT  (GAD-INDEX)  TO  PLCTRMTO.00273500
273600     MOVE GAD-O-P-X-SERVICE-GROUP       (GAD-INDEX)  TO  SRVGRUPO.00273600
273700     MOVE GAD-O-P-X-AGE-LIMIT-FROM      (GAD-INDEX)  TO           00273700
273800                                               ACWA-DISPLAY-LEN-3.00273800
273900     MOVE ACWA-DISPLAY-LEN-3                          TO AGELIMLO.00273900
274000     MOVE GAD-O-P-X-AGE-LIMIT-TO        (GAD-INDEX)  TO           00274000
274100                                               ACWA-DISPLAY-LEN-3.00274100
274200     MOVE ACWA-DISPLAY-LEN-3                          TO AGELIMHO.00274200
274300     MOVE GAD-O-P-X-FEAK-IND            (GAD-INDEX)  TO FEAKINDO. 00274300
274400     MOVE GAD-O-P-X-ACCUMID             (GAD-INDEX)  TO ACCUMIDO. 00274400
274500     MOVE GAD-O-P-X-COMB-APPLIED-IND    (GAD-INDEX)  TO CAPINDO.  00274500
274600     MOVE GAD-O-P-X-SEL-ADDL-BEN-DET    (GAD-INDEX)  TO SABDINDO. 00274600
274700     MOVE GAD-O-P-X-AGE-QUAL-IND-FROM   (GAD-INDEX)  TO AGEQLLO.  00274700
274800     MOVE GAD-O-P-X-AGE-QUAL-IND-TO     (GAD-INDEX)  TO AGEQLHO.  00274800
274900     MOVE GAD-O-P-X-RELATIONSHIP-IND    (GAD-INDEX)  TO RELPINDO. 00274900
275000                                                                  00275000
275100     MOVE GAD-O-P-X-BEN-PER-TIME-FCTR   (GAD-INDEX)  TO           00275100
275200                                               ACWA-DISPLAY-LEN-3.00275200
275300     MOVE ACWA-DISPLAY-LEN-3                         TO  PRTIMEFO.00275300
275400     MOVE GAD-O-P-X-CLAIM-LVL-ACCUM-IND (GAD-INDEX)  TO  CLMLVLIO.00275400
275500     MOVE GAD-O-P-X-INTERVAL-TIME-FCTR  (GAD-INDEX)  TO           00275500
275600                                               ACWA-DISPLAY-LEN-3.00275600
275700     MOVE ACWA-DISPLAY-LEN-3                         TO  INTRVALO.00275700
275800     MOVE GAD-O-P-X-INTERVAL-TYPE       (GAD-INDEX)  TO  INTTYPEO.00275800
275900     MOVE GAD-O-P-X-L-O-B               (GAD-INDEX)  TO  LOBO.    00275900
276000     MOVE GAD-O-P-X-VALUE-LIMIT         (GAD-INDEX)  TO           00276000
276100                                               ACWA-VALUE-LIMIT-9.00276100
276200     IF  ACWA-VALUE-LIMIT-9-9  =  -1                              00276200
276300     THEN                                                         00276300
276400         MOVE 'NEG'  TO  BNMXVALO                                 00276400
276500     ELSE                                                         00276500
276200     IF  ACWA-VALUE-LIMIT-9-9  =  -2                              00276510
276300     THEN                                                         00276520
276400         MOVE 'UNL'  TO  BNMXVALO                                 00276530
276500     ELSE                                                         00276540
276600         IF  GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)  =  '5'        00276600
276700         THEN                                                     00276700
276800             MOVE ACWA-VALUE-LIMIT-9    TO  ACWA-EDIT-VALUE-LIMIT 00276800
276900             MOVE ACWA-EDIT-VALUE-LIMIT TO  BNMXVALO              00276900
277000         ELSE                                                     00277000
277100             MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)  TO           00277100
277200                                              ACWA-DISPLAY-LEN-9-200277200
277300             MOVE ACWA-DISPLAY-LEN-9-X     TO  ACWA-DISPLAY-9     00277300
277400             MOVE SPACES                   TO  ACWA-DISPLAY-1     00277400
277500             MOVE ACWA-DISPLAY-VALUE-LIMIT TO  BNMXVALO.          00277500
277600                                                                  00277600
277700     MOVE GAD-O-P-X-PERCENT-LEVEL       (GAD-INDEX)  TO           00277700
277800                                               ACWA-DISPLAY-LEN-3.00277800
277900     MOVE ACWA-DISPLAY-LEN-3                         TO  PERLIMTO.00277900
278000     MOVE GAD-O-P-X-VALUE-QUALIFIER     (GAD-INDEX)  TO  BENVLQLO.00278000
278100     MOVE GAD-O-P-X-INTERVAL-OVRD-VALUE (GAD-INDEX)  TO           00278100
278200                                               ACWA-DISPLAY-LEN-5.00278200
278300     MOVE ACWA-DISPLAY-LEN-5                         TO  NEWVALUO.00278300
278400     MOVE GAD-O-P-X-INTERVAL-OVRD-IND   (GAD-INDEX)  TO  OVRDINDO.00278400
278500     MOVE GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX)  TO  INTDESKO.00278500
278600     MOVE GAD-COND-ALL-BIT              (GAD-INDEX)  TO  CONDALLO.00278600
278700     MOVE GAD-COND-EXCLUSION-BIT        (GAD-INDEX)  TO  CONDEXCO.00278700
278800     MOVE GAD-COND-ICD-BIT              (GAD-INDEX)  TO  CONDICDO.00278800
278900     MOVE GAD-COND-TB-BIT               (GAD-INDEX)  TO  CONDTABO.00278900
279000     MOVE GAD-COND-MENTAL-BIT           (GAD-INDEX)  TO  CONDMENO.00279000
279100     MOVE GAD-COND-DRUG-BIT             (GAD-INDEX)  TO  CONDDRGO.00279100
279200     MOVE GAD-COND-ALCOHOL-BIT          (GAD-INDEX)  TO  CONDALCO.00279200
279300     MOVE GAD-COND-OB-COMP-BIT          (GAD-INDEX)  TO  CONDOBCO.00279300
279400     MOVE GAD-COND-OB-NORM-BIT          (GAD-INDEX)  TO  CONDOBNO.00279400
279500     MOVE GAD-COND-MALIGNANCY-BIT       (GAD-INDEX)  TO  CONDMALO.00279500
279600     MOVE GAD-COND-CARDIAC-DISEASE-BIT  (GAD-INDEX)  TO  CONDCARO.00279600
279700     MOVE GAD-COND-OBESITY-BIT          (GAD-INDEX)  TO  CONDOBSO.00279700
279800     MOVE GAD-COND-KIDNEY-DISEASE-BIT   (GAD-INDEX)  TO  CONDKDYO.00279800
279900     MOVE GAD-COND-ACCIDENT-BIT         (GAD-INDEX)  TO  CONDACCO.00279900
280000     MOVE GAD-COND-PRE-EXIST-BIT        (GAD-INDEX)  TO  CONDPECO.00280000
280100     MOVE GAD-COND-NON-EMER-BIT         (GAD-INDEX)  TO  CONDNEMO.00280100
280200     MOVE GAD-COND-SUICIDE-BIT          (GAD-INDEX)  TO  CONDSUIO.00280200
280300     MOVE GAD-COND-TMJ-BIT              (GAD-INDEX)  TO  CONDTMJO.00280300
280400     MOVE GAD-COND-INF-BIT              (GAD-INDEX)  TO  CONDINFO.00280400
280500     MOVE GAD-COND-EMER-MED-BIT         (GAD-INDEX)  TO  CONDLIFO.00280500
280600     MOVE GAD-COND-EMER-ACC-BIT         (GAD-INDEX)  TO  CONDEMCO.00280600
280700     MOVE GAD-COND-LIFE-THREAT-BIT      (GAD-INDEX)  TO  CONDEACO.00280700
280800     MOVE GAD-COND-SER-MEN-ILL-BIT      (GAD-INDEX)  TO  CONDSMIO.00280800
280900     MOVE GAD-COND-NON-SER-MEN-ILL-BIT  (GAD-INDEX)  TO  CONDNSMO.00280900
281000                                                                  00281000
281100     MOVE -1  TO PERIODL.                                         00281100
281200                                                                  00281200
281300     MOVE GAD-O-P-X-FYI-VALUE (GAD-INDEX)  TO  FYIVALO.           00281300
281400     SET  CURNT-OCURS-BIN                  TO  GAD-INDEX.         00281400
281500     MOVE CURNT-OCURS-BIN                TO CURNT-OCURS-PKD.      00281500
281600     MOVE CURNT-OCCURS-OUT               TO COCURANO.             00281600
281700                                                                  00281700
281800     IF GAD-ENTRY-COUNT  >  1                                     00281800
281900     THEN                                                         00281900
282000         COMPUTE  TOTAL-OCURS-UNK  =  GAD-ENTRY-COUNT  -  1       00282000
282100         MOVE  TOTAL-OCCURS-OUT TO  TOCURANO                      00282100
282200     ELSE                                                         00282200
282300         MOVE  '01'             TO  TOCURANO.                     00282300
282400                                                                  00282400
282500                                                                  00282500
282600     MOVE ZEROS   TO  IBGRSLTO,  IPGNSLTO,  IPGTSLTO              00282600
282700                      IDGDSLTO,  IPGPSLTO,  IPGSSLTO.             00282700
282800                                                                  00282800
282900     SET GAD-INT-INDEX  TO       1.                               00282900
283000     SET GAD-INT-INDEX  DOWN BY  1.                               00283000
283100                                                                  00283100
283200 4400-300-DISPLAY-LOOP.                                           00283200
283300                                                                  00283300
283400     SET GAD-INT-INDEX  UP BY  1.                                 00283400
283500     IF  GAD-INT-INDEX  >  5                                      00283500
283600         GO TO 4400-800-SEND.                                     00283600
283700                                                                  00283700
283800     IF  GAD-INT-ID (GAD-INDEX GAD-INT-INDEX)  =  HIGH-VALUES     00283800
283900         GO TO 4400-800-SEND.                                     00283900
284000                                                                  00284000
284100     IF  GAD-INT-ID (GAD-INDEX GAD-INT-INDEX)       = '#IBGR '    00284100
284200         MOVE GAD-INT-SLOT (GAD-INDEX GAD-INT-INDEX)              00284200
284300                                  TO  ACWA-DISPLAY-LEN-7          00284300
284400         MOVE ACWA-DISPLAY-LEN-7  TO  IBGRSLTO                    00284400
284500         GO TO 4400-300-DISPLAY-LOOP.                             00284500
284600                                                                  00284600
284700     IF  GAD-INT-ID (GAD-INDEX GAD-INT-INDEX)       = '#IDGD '    00284700
284800         MOVE GAD-INT-SLOT (GAD-INDEX GAD-INT-INDEX)              00284800
284900                                  TO  ACWA-DISPLAY-LEN-7          00284900
285000         MOVE ACWA-DISPLAY-LEN-7  TO  IDGDSLTO                    00285000
285100         GO TO 4400-300-DISPLAY-LOOP.                             00285100
285200                                                                  00285200
285300     IF  GAD-INT-ID (GAD-INDEX GAD-INT-INDEX)       = '#IPGN '    00285300
285400         MOVE GAD-INT-SLOT (GAD-INDEX GAD-INT-INDEX)              00285400
285500                                  TO  ACWA-DISPLAY-LEN-7          00285500
285600         MOVE ACWA-DISPLAY-LEN-7  TO  IPGNSLTO                    00285600
285700         GO TO 4400-300-DISPLAY-LOOP.                             00285700
285800                                                                  00285800
285900     IF  GAD-INT-ID (GAD-INDEX GAD-INT-INDEX)       = '#IPGP '    00285900
286000         MOVE GAD-INT-SLOT (GAD-INDEX GAD-INT-INDEX)              00286000
286100                                  TO  ACWA-DISPLAY-LEN-7          00286100
286200         MOVE ACWA-DISPLAY-LEN-7  TO  IPGPSLTO                    00286200
286300         GO TO 4400-300-DISPLAY-LOOP.                             00286300
286400                                                                  00286400
286500     IF  GAD-INT-ID (GAD-INDEX GAD-INT-INDEX)       = '#IPGT '    00286500
286600         MOVE GAD-INT-SLOT (GAD-INDEX GAD-INT-INDEX)              00286600
286700                                  TO  ACWA-DISPLAY-LEN-7          00286700
286800         MOVE ACWA-DISPLAY-LEN-7  TO  IPGTSLTO                    00286800
286900         GO TO 4400-300-DISPLAY-LOOP.                             00286900
287000                                                                  00287000
287100     IF  GAD-INT-ID (GAD-INDEX GAD-INT-INDEX)       = '#IPGS '    00287100
287200         MOVE GAD-INT-SLOT (GAD-INDEX GAD-INT-INDEX)              00287200
287300                                  TO  ACWA-DISPLAY-LEN-7          00287300
287400         MOVE ACWA-DISPLAY-LEN-7  TO  IPGSSLTO                    00287400
287500         GO TO 4400-300-DISPLAY-LOOP.                             00287500
287600                                                                  00287600
287700     MOVE WS-ABCODE-1EF3        TO  WS-ABCODE                     00287700
287800     MOVE WS-ABCODE-1EF3-MSG    TO  WS-ABCODE-MSG                 00287800
287900     PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                       00287900
288000                                                                  00288000
288100                                                                  00288100
288200 4400-800-SEND.                                                   00288200
288300                                                                  00288300
288400     PERFORM 4500-000-PROTECT-CRIT-DATA-ELE.                      00288400
288500                                                                  00288500
288600*-------RESET ATTR. 'CAUSE INTDESK & IDPROD CHANGED IN 4500- CALL 00288600
288700     PERFORM 7900-000-RESET-ATTRIBUTES.                           00288700
288800                                                                  00288800
288900     PERFORM 9000-000-SEND-ERASE-RETURN.                          00288900
289000                                                                  00289000
289100 4400-900-EXIT. EXIT.                                             00289100
289200                                                                  00289200
289300/*****************************************************************00289300
289400*  4500  -  PROTECT CRITICAL DATA ELEMENTS                       *00289400
289500*                                                                *00289500
289600*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00289600
289700*          1. DETERMINE IF GROUP IS CRITICAL (CALL GCTRSRT).     *00289700
289800*          2. IF GROUP IS CRITICAL:                              *00289800
289900*              - READ PRODUCTION CONTRACT, GROUP SPECIFIC, OR    *00289900
290000*                BENEFIT PROVISION.                              *00290000
290100*                - IF ON DATA BASE:                              *00290100
290200*                  - SCAN FOR #AOL TABULAR                       *00290200
290300*                    - IF TABULAR PRESENT AND ACTIVE, TABULAR IS *00290300
290400*                      CRITICAL, PROTECT CRITICAL DATA ELEMENTS  *00290400
290500*                      ON SCREEN AND ISSUE MESSAGE.              *00290500
290600******************************************************************00290600
290700 4500-000-PROTECT-CRIT-DATA-ELE SECTION.                          00290700
290800 4500-010.                                                        00290800
290900                                                                  00290900
291000     IF DELADDI  =  'CHG/DEL'   OR                                00291000
291100        DELOLITI  =  SPACES                                       00291100
291200        NEXT SENTENCE                                             00291200
291300     ELSE                                                         00291300
291400        GO TO 4500-900-EXIT.                                      00291400
291500                                                                  00291500
291600     MOVE WS-REQUEST-4500-CDE-PROTECT TO ACWA-CDE-REQUEST-CODE.   00291600
291700     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00291700
291800                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00291800
291900                                                                  00291900
292000     EXEC CICS  LINK   PROGRAM('GACDEPGM')                        00292000
292100                COMMAREA (COMMON-WORKAREAS)                       00292100
292200                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00292200
292300                                                                  00292300
292400     GO TO 4500-900-EXIT.                                         00292400
292500                                                                  00292500
292600 4500-900-EXIT. EXIT.                                             00292600
292700                                                                  00292700
292800/*****************************************************************00292800
292900*  4600  -  UPDATE CRITICAL DATA ELEMENT STATUS                  *00292900
293000*                                                                *00293000
293100*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00293100
293200*           1. READ ALL LEVEL TABULAR FROM PROVISION POOL        *00293200
293300*           2. COMPARE CDE ELEMENTS ON W/F ALL LVL TAB TO THOSE  *00293300
293400*               ON THE PROVISION POOL ALL LVL TABULAR RECORD.    *00293400
293500*           3. IF CDE ELEMENTS ON W/F ALL LVL TAB HAVE BEEN      *00293500
293600*               CHANGED, ISSUE MESSAGE AND POSITION CURSOR ON    *00293600
293700*               +CDE+ INDICATOR (POSITION=8).                    *00293700
293800*              IF MESSAGE HAS BEEN ISSUED AND OPERATOR HAS HIT   *00293800
293900*               ENTER, CONTINUE PROCESSING.                      *00293900
294000******************************************************************00294000
294100 4600-000-UPDATE-CDE-STATUS     SECTION.                          00294100
294200 4600-010.                                                        00294200
294300                                                                  00294300
294400     IF CDEINDO = ('+CDE+' OR '+CDE-') AND                        00294400
294500        (DELADDI = 'CHG/DEL' OR                                   00294500
294600        (DELADDI = 'CHG/ADD' AND                                  00294600
294700        WRK-SIGNAL-FROM-ONLINE  =  'W'))                          00294700
294800        NEXT SENTENCE                                             00294800
294900     ELSE                                                         00294900
295000        IF CDEINDO = ('+CDE+' OR '+CDE-') AND                     00295000
295100           (DELADDI = 'CHG/ADD')                                  00295100
295200           NEXT SENTENCE                                          00295200
295300        ELSE                                                      00295300
295400         GO TO 4600-900-EXIT.                                     00295400
295500                                                                  00295500
295600     MOVE WS-ALT-WORKFILE-KEYS        TO  ACWA-ALT-WORKFILE-KEYS. 00295600
295700     SET  ACWA-INDEX-1                TO  GAD-INDEX.              00295700
295800     MOVE WS-REQUEST-4600-CDE-STATUS  TO  ACWA-CDE-REQUEST-CODE.  00295800
295900     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00295900
296000                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00296000
296100                                                                  00296100
296200     EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      00296200
296300                COMMAREA (COMMON-WORKAREAS)                       00296300
296400                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00296400
296500                                                                  00296500
296600     IF  ACWA-CDE-RETURN-DONT-SEND                                00296600
296700*        EXEC CICS  RETURN  END-EXEC.                             00296700
296800         EXEC CICS  RETURN TRANSID('GA1E')                        00296800
296900                    COMMAREA(DFHCOMMAREA)                         00296900
297000                    LENGTH  (EIBCALEN)                            00297000
297100                    END-EXEC.                                     00297100
297200                                                                  00297200
297300 4600-900-EXIT. EXIT.                                             00297300
297400                                                                  00297400
297500/*****************************************************************00297500
297600*  4700  -  UPDATE W/F CONTROL RECORD                            *00297600
297700*                                                                *00297700
297800*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00297800
297900*          1. READ W/F CONTROL RECORD, UPDATE CDE RECORD COUNTS  *00297900
298000*             WITH THE ACTION TAKEN ON THE ALL LEVEL TABULAR     *00298000
298100*             RECORD IF ITS CDE STATUS CHANGED.                  *00298100
298200*          2. REWRITE W/F CONTROL RECORD                         *00298200
298300******************************************************************00298300
298400 4700-000-UPDATE-CONTROL-RECORD SECTION.                          00298400
298500 4700-010.                                                        00298500
298600                                                                  00298600
298700     MOVE WS-ALT-WORKFILE-KEYS    TO   ACWA-ALT-WORKFILE-KEYS.    00298700
298800     SET  ACWA-INDEX-1            TO   GAD-INDEX.                 00298800
298900     MOVE WS-REQUEST-4700-CNTL-UPDATE  TO  ACWA-CDE-REQUEST-CODE. 00298900
299000     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00299000
299100                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00299100
299200                                                                  00299200
299300     EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      00299300
299400                COMMAREA (COMMON-WORKAREAS)                       00299400
299500                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00299500
299600                                                                  00299600
299700 4700-900-EXIT. EXIT.                                             00299700
299800                                                                  00299800
299900/*****************************************************************00299900
300000* 5000  XCTL TO PREVIOUS MENU                                    *00300000
300100*                                                                *00300100
300200*   THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM      *00300200
300300*  ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD AND    *00300300
300400*  INSURE THAT THE TABLE OF OCCURRENCES IS SORTED AND THAT ANY   *00300400
300500*  DUPLICATES ARE DROPPED FROM THE LIST.  WE THEN REWRITE THE    *00300500
300600*  ALL LEVEL TABULAR AND READ THE PARTICULAR RECORD THAT THE     *00300600
300700*  MENU WHICH PASSED US CONTROL WOULD REQUIRE.  FINALLY BASED    *00300700
300800*  ON THE PREVIOUS MENU FIELD CARRIED THROUGHOUT THIS PART OF    *00300800
300900*  THE SYSTEM WE RETURN TO THE PREVIOUS MENU.                    *00300900
301000******************************************************************00301000
301100 5000-000-XCTL-TO-PREVIOUS-MENU SECTION.                          00301100
301200 5000-010.                                                        00301200
301300                                                                  00301300
301400     PERFORM 3200-000-READ-REC-FOR-UPDATE.                        00301400
301500                                                                  00301500
301600     IF NOT GCIO-GOOD-RETURN                                      00301600
301700        MOVE WS-ABCODE-1EF8        TO  WS-ABCODE                  00301700
301800        MOVE WS-ABCODE-1EF8-MSG    TO  WS-ABCODE-MSG              00301800
301900        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00301900
302000                                                                  00302000
302100     PERFORM 6500-000-SORT-COMPRESS-ALL-LVL.                      00302100
302000                                                                  00302101
      ****   03/19/15  KIKI  CHANGED - #AOL PERCENT FIELD VALIDATION   *00302110
      ****                             1200-D210-EDIT  SECTION         *00302120
      ****                             NO LONGER IS EXECUTED FROM      *00302130
      ****                             5000-000-XCTL-TO-PREVIOUS-MENU  *00302140
      ****                                                             *00302150
      ****                             NO LONGER THE PERCENT FIELD     *00302160
      ****                             MUST BE SAME IN ALL OCCURS.,    *00302170
      ****                             WHEN OTHER THAN +100            *00302180
      ****                                                             *00302190
302200**<< PERFORM 4600-000-UPDATE-CDE-STATUS.                          00302200
302300     PERFORM 3000-000-UPDATE-GAD-RECORD.                          00302300
302400**<<                                            D210 FIX  6/23/89 00302400
      **** IF EIBAID = DFHPF3 OR DFHPF15                                00302500
302600****     IF GAD-ENTRY-COUNT > 2                                   00302600
302700****         MOVE 'N' TO ACWA-ERROR-SW                            00302700
302800****         MOVE +999 TO WS-HOLD-PCT-LVL                         00302800
302900****         PERFORM 1200-D210-EDIT THRU 1200-EXIT                00302900
303000****           VARYING GAD-INDEX FROM 1 BY 1                      00303000
303100****           UNTIL GAD-INDEX = GAD-ENTRY-COUNT                  00303100
303200****              OR                                              00303200
303300****                 ACWA-ERROR-SW = 'Y'                          00303300
303400****         IF ACWA-ERROR-SW = 'Y'                               00303400
303500****             PERFORM 9010-000-SEND-DATAONLY-RETURN.           00303500
      **<<                                                              00303600
                                                                        00303610
303700     PERFORM 4600-000-UPDATE-CDE-STATUS.                          00303700
                                                                        00303800
303900     IF ACWA-CDE-FIELD-CHANGED OR  ACWA-CDE-REC-CHANGED           00303900
304000        IF EIBCPOSN = 8                                           00304000
304100           NEXT SENTENCE                                          00304100
304200        ELSE                                                      00304200
304300*          EXEC CICS  RETURN  END-EXEC.                           00304300
304400           EXEC CICS  RETURN TRANSID('GA1E')                      00304400
304500                      COMMAREA(DFHCOMMAREA)                       00304500
304600                      LENGTH  (EIBCALEN)                          00304600
304700                      END-EXEC.                                   00304700
304800                                                                  00304800
304900     IF FRMNUIDI  =  'GS3A'                                       00304900
305000        PERFORM 5100-000-RETURN-TO-GRP-SPEC                       00305000
305100        EXEC CICS  XCTL  PROGRAM ('GS3APGM')                      00305100
305200                   COMMAREA(WORK-RECORD-3)                        00305200
305300                   LENGTH (WS-WRK-GRP-SPEC-LEN)   END-EXEC.       00305300
305400                                                                  00305400
305500     IF  FRMNUIDI  =  'GC4A'                                      00305500
305600        PERFORM 5200-000-RETURN-TO-CONTRACT                       00305600
305700        EXEC CICS  XCTL  PROGRAM ('GC4APGM')                      00305700
305800                   COMMAREA(WORK-RECORD-4)                        00305800
305900                   LENGTH (WS-WRK-CONTRACT-LEN)   END-EXEC.       00305900
306000                                                                  00306000
306100     IF FRMNUIDI  =  'GC8A'                                       00306100
306200        PERFORM 5300-000-RETURN-TO-BEN-PROV                       00306200
306300        EXEC CICS  XCTL  PROGRAM ('GC8APGM')                      00306300
306400                   COMMAREA(WORK-RECORD-5)                        00306400
306500                   LENGTH (WS-WRK-BEN-PROV-LEN)   END-EXEC.       00306500
306600                                                                  00306600
306700*******                                                           00306700
306800* STS *===> RETURN TO SINGLE TABULAR SUPPORT MENU, NO COMMAREA    00306800
306900*******                                                          |00306900
307000     IF  FRMNUIDI  =  'GTM1'                                      00307000
307100         EXEC CICS  XCTL  PROGRAM('GTM1PGM')   END-EXEC.          00307100
307200*******                                                          |00307200
307300* STS *----------------------------------------------------------*00307300
307400*******                                                           00307400
307500                                                                  00307500
307600 5000-900-EXIT. EXIT.                                             00307600
307700                                                                  00307700
307800/*****************************************************************00307800
307900* 5100  RETURN TO GRP SPEC                                       *00307900
308000*                                                                *00308000
308100*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00308100
308200*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00308200
308300******************************************************************00308300
308400 5100-000-RETURN-TO-GRP-SPEC    SECTION.                          00308400
308500 5100-010.                                                        00308500
308600                                                                  00308600
308700        EXEC CICS GETMAIN                                         00308700
308800               SET(ADDRESS OF WF-IO-PARM-WRK-GRP-SPEC-REC)        00308800
308900               INITIMG(WS-HEX-00)                                 00308900
309000               LENGTH(WS-IO-PARM-WRK-GRP-SPEC-LEN)                00309000
309100               END-EXEC.                                          00309100
309200                                                                  00309200
309300        SET ACWA-WF-GRP-SPEC-PNTR     TO                          00309300
309400                 ADDRESS OF WF-IO-PARM-WRK-GRP-SPEC-REC.          00309400
309500                                                                  00309500
309600     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00309600
309700     MOVE 'G'                  TO GCIO-WRK-STATUS-CODE.           00309700
309800     MOVE 'G2'                 TO GCIO-WRK-RECORD-TYPE.           00309800
309900     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00309900
310000     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00310000
310100     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00310100
310200     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00310200
310300     MOVE SPACES               TO GCIO-WRK-LINE-OF-BUS            00310300
310400                                  GCIO-WRK-PROVISION-ID           00310400
310500                                  GCIO-WRK-PROVIDER-CONTROL       00310500
310600                                  GCIO-WRK-TAB-PROVISION-ID.      00310600
310700     MOVE ZEROS                TO GCIO-WRK-PROVISION-SLOT-NO,     00310700
310800                                  GCIO-WRK-TAB-PROV-SLOT-NO.      00310800
310900     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00310900
311000     MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             00311000
311100                                                                  00311100
311200     MOVE GC-GCPSWORK-DDNAME     TO GCIO3-FILE-DDNAME.            00311200
311300     MOVE GCIO-WORKFILE-KEY      TO GCIO3-FILE-KEY.               00311300
311400     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO3-FILE-ACCESS-CODE.       00311400
311500     MOVE GC-GCIO-AREA-1         TO GCIO3-IO-AREA-TO-USE.         00311500
311600     MOVE GC-GCGRPSPC-VARY-MAX-OCUR  TO                           00311600
311700                      GCG-COUNT-TAB-PROVN-POINTERS.               00311700
311800                                                                  00311800
311900     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00311900
312000                COMMAREA(WF-IO-PARM-WRK-GRP-SPEC-REC)             00312000
312100                LENGTH (WS-IO-PARM-WRK-GRP-SPEC-LEN)  END-EXEC.   00312100
312200                                                                  00312200
312300     IF NOT GCIO3-GOOD-RETURN                                     00312300
312400        MOVE WS-ABCODE-1EF9        TO WS-ABCODE                   00312400
312500        MOVE WS-ABCODE-1EF9-MSG    TO WS-ABCODE-MSG               00312500
312600        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00312600
312700                                                                  00312700
312800 5100-900-EXIT. EXIT.                                             00312800
312900                                                                  00312900
313000/*****************************************************************00313000
313100* 5200  RETURN TO CONTRACT                                       *00313100
313200*                                                                *00313200
313300*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00313300
313400*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00313400
313500******************************************************************00313500
313600 5200-000-RETURN-TO-CONTRACT    SECTION.                          00313600
313700 5200-010.                                                        00313700
313800                                                                  00313800
313900        EXEC CICS GETMAIN                                         00313900
314000               SET(ADDRESS OF WF-IO-PARM-WRK-CONTRACT-REC)        00314000
314100               INITIMG(WS-HEX-00)                                 00314100
314200               LENGTH(WS-IO-PARM-WRK-CONTRACT-LEN)                00314200
314300               END-EXEC.                                          00314300
314400                                                                  00314400
314500        SET ACWA-WF-CONTRACT-PNTR     TO                          00314500
314600                 ADDRESS OF WF-IO-PARM-WRK-CONTRACT-REC.          00314600
314700                                                                  00314700
314800     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00314800
314900     MOVE 'C'                  TO GCIO-WRK-STATUS-CODE.           00314900
315000     MOVE 'C2'                 TO GCIO-WRK-RECORD-TYPE.           00315000
315100     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00315100
315200     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00315200
315300     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00315300
315400     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00315400
315500     MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           00315500
315600     MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      00315600
315700     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00315700
315800     MOVE SPACES               TO GCIO-WRK-PROVISION-ID           00315800
315900                                  GCIO-WRK-TAB-PROVISION-ID.      00315900
316000     MOVE ZEROS                TO GCIO-WRK-PROVISION-SLOT-NO      00316000
316100                                  GCIO-WRK-TAB-PROV-SLOT-NO.      00316100
316200                                                                  00316200
316300     MOVE GCA-EFFDT-CEN          TO GCIO-WRK-EFFDT-CEN.           00316300
316400     MOVE GC-GCPSWORK-DDNAME     TO GCIO4-FILE-DDNAME.            00316400
316500     MOVE GCIO-WORKFILE-KEY      TO GCIO4-FILE-KEY.               00316500
316600     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO4-FILE-ACCESS-CODE.       00316600
316700     MOVE GC-GCIO-AREA-1         TO GCIO4-IO-AREA-TO-USE.         00316700
316800     MOVE GC-GCCONTR-VARY-MAX-OCUR  TO                            00316800
316900                      GCT-COUNT-BEN-PROVN-POINTERS.               00316900
317000                                                                  00317000
317100     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00317100
317200                COMMAREA(WF-IO-PARM-WRK-CONTRACT-REC)             00317200
317300                LENGTH (WS-IO-PARM-WRK-CONTRACT-LEN)   END-EXEC.  00317300
317400                                                                  00317400
317500     IF NOT GCIO4-GOOD-RETURN                                     00317500
317600        MOVE WS-ABCODE-1EFA        TO WS-ABCODE                   00317600
317700        MOVE WS-ABCODE-1EFA-MSG    TO WS-ABCODE-MSG               00317700
317800        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00317800
317900                                                                  00317900
318000 5200-900-EXIT. EXIT.                                             00318000
318100                                                                  00318100
318200/*****************************************************************00318200
318300* 5300  RETURN TO BEN PROV                                       *00318300
318400*                                                                *00318400
318500*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00318500
318600*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00318600
318700******************************************************************00318700
318800 5300-000-RETURN-TO-BEN-PROV    SECTION.                          00318800
318900 5300-010.                                                        00318900
319000                                                                  00319000
319100        EXEC CICS GETMAIN                                         00319100
319200               SET(ADDRESS OF WF-IO-PARM-WRK-BEN-PROV-REC)        00319200
319300               INITIMG(WS-HEX-00)                                 00319300
319400               LENGTH(WS-IO-PARM-WRK-BEN-PROV-LEN)                00319400
319500               END-EXEC.                                          00319500
319600                                                                  00319600
319700        SET ACWA-WF-BEN-PROV-PNTR     TO                          00319700
319800                 ADDRESS OF WF-IO-PARM-WRK-BEN-PROV-REC.          00319800
319900                                                                  00319900
320000     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00320000
320100     MOVE 'C'                   TO GCIO-WRK-STATUS-CODE.          00320100
320200     MOVE 'C4'                  TO GCIO-WRK-RECORD-TYPE.          00320200
320300     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00320300
320400     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00320400
320500     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00320500
320600     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00320600
320700     MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS.          00320700
320800     MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL.     00320800
320900     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00320900
321000     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00321000
321100     MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID.         00321100
321200     MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID.     00321200
321300     MOVE 9999999               TO GCIO-WRK-PROVISION-SLOT-NO.    00321300
321400     MOVE ZEROS                 TO GCIO-WRK-TAB-PROV-SLOT-NO.     00321400
321500                                                                  00321500
321600     MOVE GC-GCPSWORK-DDNAME     TO GCIO5-FILE-DDNAME.            00321600
321700     MOVE GCIO-WORKFILE-KEY      TO GCIO5-FILE-KEY.               00321700
321800     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO5-FILE-ACCESS-CODE.       00321800
321900     MOVE GC-GCIO-AREA-1         TO GCIO5-IO-AREA-TO-USE.         00321900
322000     MOVE GC-GCBENPRV-VARY-MAX-OCUR  TO                           00322000
322100                      GCP-COUNT-TAB-PROVN-POINTERS.               00322100
322200                                                                  00322200
322300     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00322300
322400                COMMAREA(WF-IO-PARM-WRK-BEN-PROV-REC)             00322400
322500                LENGTH (WS-IO-PARM-WRK-BEN-PROV-LEN)  END-EXEC.   00322500
322600                                                                  00322600
322700     IF NOT GCIO5-GOOD-RETURN                                     00322700
322800        MOVE WS-ABCODE-1EFB        TO WS-ABCODE                   00322800
322900        MOVE WS-ABCODE-1EFB-MSG    TO WS-ABCODE-MSG               00322900
323000        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00323000
323100                                                                  00323100
323200 5300-900-EXIT. EXIT.                                             00323200
323300                                                                  00323300
323400/*****************************************************************00323400
323500* 6000  BUILD GROUP SPEC KEY                                     *00323500
323600*                                                                *00323600
323700*    BUILD THE GROUP SPECIFIC KEY FOR WORKFILE READS             *00323700
323800******************************************************************00323800
323900 6000-000-BUILD-GROUP-SPEC-KEY  SECTION.                          00323900
324000 6000-010.                                                        00324000
324100                                                                  00324100
324200     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00324200
324300     MOVE  'G'                  TO GCIO-WRK-STATUS-CODE.          00324300
324400     MOVE  'G3'                 TO GCIO-WRK-RECORD-TYPE.          00324400
324500     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00324500
324600     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00324600
324700     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00324700
324800     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00324800
324900     MOVE SPACES                TO GCIO-WRK-LINE-OF-BUS,          00324900
325000                                   GCIO-WRK-PROVIDER-CONTROL.     00325000
325100     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00325100
325200     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00325200
325300     MOVE TABIDI                TO GCIO-WRK-PROVISION-ID.         00325300
325400     MOVE TABSLTNI              TO ACWA-DISPLAY-LEN-7.            00325400
325500     MOVE ACWA-DISPLAY-LEN-7    TO GCIO-WRK-PROVISION-SLOT-NO.    00325500
325600     MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID.     00325600
325700     MOVE ZEROS                 TO GCIO-WRK-TAB-PROV-SLOT-NO.     00325700
325800                                                                  00325800
325900 6000-900-EXIT. EXIT.                                             00325900
326000                                                                  00326000
326100******************************************************************00326100
326200* 6100  BUILD CONTRACT KEY                                       *00326200
326300*                                                                *00326300
326400*    BUILD THE CONTRACT KEY FOR WORKFILE READS                   *00326400
326500******************************************************************00326500
326600 6100-000-BUILD-CONTRACT-KEY    SECTION.                          00326600
326700 6100-010.                                                        00326700
326800                                                                  00326800
326900     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00326900
327000     MOVE  'C'                 TO GCIO-WRK-STATUS-CODE.           00327000
327100     MOVE  'C3'                TO GCIO-WRK-RECORD-TYPE.           00327100
327200     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00327200
327300     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00327300
327400     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00327400
327500     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00327500
327600     MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           00327600
327700     MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      00327700
327800     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00327800
327900     MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             00327900
328000     MOVE TABIDI               TO GCIO-WRK-PROVISION-ID.          00328000
328100     MOVE TABSLTNI             TO ACWA-DISPLAY-LEN-7.             00328100
328200     MOVE ACWA-DISPLAY-LEN-7   TO GCIO-WRK-PROVISION-SLOT-NO.     00328200
328300     MOVE SPACES               TO GCIO-WRK-TAB-PROVISION-ID.      00328300
328400     MOVE ZEROS                TO GCIO-WRK-TAB-PROV-SLOT-NO.      00328400
328500                                                                  00328500
328600 6100-900-EXIT. EXIT.                                             00328600
328700                                                                  00328700
328800/*****************************************************************00328800
328900* 6200  BUILD BEN PROV KEY                                       *00328900
329000*                                                                *00329000
329100*    BUILD THE BEN PROV KEY FOR WORKFILE READS                   *00329100
329200******************************************************************00329200
329300 6200-000-BUILD-BEN-PROV-KEY    SECTION.                          00329300
329400 6200-010.                                                        00329400
329500                                                                  00329500
329600     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00329600
329700     MOVE  'C'                  TO GCIO-WRK-STATUS-CODE.          00329700
329800     MOVE  'C5'                 TO GCIO-WRK-RECORD-TYPE.          00329800
329900     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00329900
330000     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00330000
330100     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00330100
330200     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00330200
330300     MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS.          00330300
330400     MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL.     00330400
330500     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00330500
330600     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00330600
330700     MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID.         00330700
330800     MOVE +9999999              TO GCIO-WRK-PROVISION-SLOT-NO.    00330800
330900     MOVE TABIDI                TO GCIO-WRK-TAB-PROVISION-ID.     00330900
331000     MOVE TABSLTNI              TO ACWA-DISPLAY-LEN-7.            00331000
331100     MOVE ACWA-DISPLAY-LEN-7    TO GCIO-WRK-TAB-PROV-SLOT-NO.     00331100
331200                                                                  00331200
331300 6200-900-EXIT. EXIT.                                             00331300
331400                                                                  00331400
331500/*****************************************************************00331500
331600*  XCTL TO MAIN MENU                                             *00331600
331700*                                                                *00331700
331800*    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO   *00331800
331900*  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE*00331900
332000*  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE MENUS00332000
332100*  TO GET TO US; WE ARE A MODULE AT THE BOTTOM OF A PYRAMID TO GET00332100
332200*  HERE YOU MUST START AT THE TOP (THE MAIN MENU).               *00332200
332300******************************************************************00332300
332400 6400-000-XCTL-TO-MAIN-MENU     SECTION.                          00332400
332500 6400-010.                                                        00332500
332600                                                                  00332600
332700     MOVE WS-ABCODE-1EP1       TO  WS-ABCODE.                     00332700
332800     MOVE WS-ABCODE-1EP1-MSG   TO  WS-ABCODE-MSG.                 00332800
332900                                                                  00332900
333000     EXEC CICS  XCTL  PROGRAM('GCPSPGM')   END-EXEC.              00333000
333100                                                                  00333100
333200 6400-900-EXIT. EXIT.                                             00333200
333300                                                                  00333300
333400/*****************************************************************00333400
333500* 6500  SORT COMPRESS ALL LVL                                    *00333500
333600*                                                                *00333600
333700*    THIS ROUTINE WILL COPY ALL ENTRIES FROM THE TABULAR PORTION *00333700
333800*  TO A COPY OF THE TABULAR, THEN SORT THE COPY INTO ASCENDING   *00333800
333900*  SEQUENCE, ANY DUPLICATES ARE REMOVED FROM THE TABLE.          *00333900
334000******************************************************************00334000
334100 6500-000-SORT-COMPRESS-ALL-LVL SECTION.                          00334100
334200 6500-010.                                                        00334200
334300                                                                  00334300
334400        EXEC CICS GETMAIN                                         00334400
334500               SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)            00334500
334600               INITIMG(WS-HEX-00)                                 00334600
334700               LENGTH(WS-COPY-TABLE-LEN)                          00334700
334800               END-EXEC.                                          00334800
334900                                                                  00334900
335000        SET ACWA-COPY-TAB-PNTR        TO                          00335000
335100                 ADDRESS OF COPY-TABULAR-TABLE-AREA.              00335100
335200                                                                  00335200
335300     MOVE GAD-ENTRY-COUNT  TO  GAD-ENTRY-COUNT.                   00335300
335400     SET GAD-INDEX,  COPY-IDX  TO  1.                             00335400
335500                                                                  00335500
335600 6500-100-COPY-TABLE.                                             00335600
335700                                                                  00335700
335800     IF GAD-INDEX  NOT >  GAD-ENTRY-COUNT                         00335800
335900        MOVE GAD-ENTRY(GAD-INDEX)  TO COPY-TABULAR-TABLE(COPY-IDX)00335900
336000        SET GAD-INDEX,  COPY-IDX  UP BY  1                        00336000
336100        GO TO 6500-100-COPY-TABLE.                                00336100
336200                                                                  00336200
336300     SET  COPY-IDX   TO  1.                                       00336300
336400     SET  COPY-IDX2  TO  2.                                       00336400
336500                                                                  00336500
336600 6500-200-SORT-TABLE.                                             00336600
336700                                                                  00336700
336800     IF COPY-IDX2  >  GAD-ENTRY-COUNT                             00336800
336900        GO TO 6500-400-ARE-WE-DONE-SORTING.                       00336900
337000                                                                  00337000
337100     IF  COPY-SORTABLE-FLDS(COPY-IDX)  >                          00337100
337200                                    COPY-SORTABLE-FLDS(COPY-IDX2) 00337200
337300     THEN                                                         00337300
337400         MOVE COPY-TABULAR-TABLE(COPY-IDX)  TO  WS-ENTRY          00337400
337500         MOVE COPY-TABULAR-TABLE(COPY-IDX2)                       00337500
337600                                  TO COPY-TABULAR-TABLE(COPY-IDX) 00337600
337700         MOVE WS-ENTRY  TO  COPY-TABULAR-TABLE(COPY-IDX2)         00337700
337800         SET COPY-IDX2  UP BY  1                                  00337800
337900         GO TO 6500-200-SORT-TABLE.                               00337900
338000                                                                  00338000
338100     IF COPY-SORTABLE-FLDS(COPY-IDX)  <                           00338100
338200                                    COPY-SORTABLE-FLDS(COPY-IDX2) 00338200
338300        SET COPY-IDX2  UP BY  1                                   00338300
338400        GO TO 6500-200-SORT-TABLE.                                00338400
338500                                                                  00338500
338600     SET COPY-IDX3,  COPY-IDX4  TO  COPY-IDX2.                    00338600
338700     SET COPY-IDX4   UP BY  1.                                    00338700
338800                                                                  00338800
338900 6500-300-ELIMINATE-DUPLICATES.                                   00338900
339000                                                                  00339000
339100     IF COPY-IDX4  NOT >  GAD-ENTRY-COUNT                         00339100
339200        MOVE COPY-TABULAR-TABLE(COPY-IDX4)  TO                    00339200
339300                                    COPY-TABULAR-TABLE(COPY-IDX3) 00339300
339400        SET COPY-IDX3,  COPY-IDX4  UP BY  1                       00339400
339500        GO TO 6500-300-ELIMINATE-DUPLICATES.                      00339500
339600                                                                  00339600
339700     SUBTRACT 1  FROM  GAD-ENTRY-COUNT.                           00339700
339800     GO TO 6500-200-SORT-TABLE.                                   00339800
339900                                                                  00339900
340000 6500-400-ARE-WE-DONE-SORTING.                                    00340000
340100                                                                  00340100
340200     IF COPY-IDX  <  GAD-ENTRY-COUNT                              00340200
340300        SET COPY-IDX   UP BY  1                                   00340300
340400        SET COPY-IDX2  TO COPY-IDX                                00340400
340500        SET COPY-IDX2  UP BY 1                                    00340500
340600        GO TO 6500-200-SORT-TABLE.                                00340600
340700                                                                  00340700
340800     MOVE GAD-ENTRY-COUNT  TO  GAD-ENTRY-COUNT.                   00340800
340900     SET GAD-INDEX,  COPY-IDX  TO  1.                             00340900
341000                                                                  00341000
341100 6500-500-MOVE-COPY-BACK.                                         00341100
341200                                                                  00341200
341300     IF GAD-INDEX  NOT >  GAD-ENTRY-COUNT                         00341300
341400        MOVE COPY-TABULAR-TABLE(COPY-IDX)  TO                     00341400
341500                                           GAD-ENTRY(GAD-INDEX)   00341500
341600        SET GAD-INDEX,  COPY-IDX  UP BY  1                        00341600
341700        GO TO 6500-500-MOVE-COPY-BACK.                            00341700
341800                                                                  00341800
341900     MOVE GAD-ENTRY-COUNT  TO  GAD-ENTRY-COUNT.                   00341900
342000     IF GAD-ENTRY-COUNT  NOT <  GC-GCTABULR-AOL-VARY-MAX-OCUR     00342000
342100        MOVE 'Y'  TO  ACWA-ERROR-SW.                              00342100
342200                                                                  00342200
342300 6500-900-EXIT. EXIT.                                             00342300
342400                                                                  00342400
342500/*****************************************************************00342500
342600* 7900  RESET ATTRIBUTES                                         *00342600
342700******************************************************************00342700
342800 7900-000-RESET-ATTRIBUTES      SECTION.                          00342800
342900 7900-010.                                                        00342900
343000                                                                  00343000
343100     MOVE DFHBMUNF  TO CONDLIFA                                   00343100
343200          BENVLQLA  COPAYINA  CSTCONTA  FAMINDIA  LOBA            00343200
343300          PERIODA   PLCTRMTA  SRVGRUPA  PRTIMEFA  PERLIMTA        00343300
343400          ASCDSCDA  INTRVALA  INTTYPEA  CLMLVLIA  BNMXVALA        00343400
343500          DAYFACIA  OVRDINDA  NEWVALUA  INTDESKA  FYIVALA         00343500
343600          CONDALLA  CONDEXCA  CONDICDA  CONDTABA  CONDMENA        00343600
343700          CONDEMCA  CONDEACA  CONDSMIA  CONDNSMA                  00343700
343800          CONDDRGA  CONDALCA  CONDOBNA  CONDOBCA  CONDMALA        00343800
343900       CONDCARA  CONDOBSA  CONDKDYA  CONDACCA  CONDPECA CONDTMJA  00343900
344000       CONDNEMA  CONDSUIA  CARYOVRA  DEFINTNA  BISNDINA CONDINFA  00344000
344100       IBGROPTA  IPGNOPTA  IPGTOPTA  MFRMSLTA  PERTQALA AGEQLLA   00344100
344200       IDGDOPTA  IPGPOPTA  AGELIMLA  AGELIMHA  RELPINDA AGEQLHA   00344200
344300       FEAKINDA  IPGSOPTA  ACCUMIDA  CAPINDA   SABDINDA           00344300
344400       BENTYPA   TIERCDA   TIERLVA.                               00344400
344500                                                                  00344500
344600     IF  DELADDO  =  'CHG/DEL'                                    00344600
344700     THEN                                                         00344700
344800         NEXT SENTENCE                                            00344800
344900     ELSE                                                         00344900
345000         GO TO 7900-900-EXIT.                                     00345000
345100                                                                  00345100
345200                                                                  00345200
345300     IF  CDEINDO = '+CDE+'                                        00345300
345400     THEN                                                         00345400
345500*---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         00345500
345600         MOVE DFHBMABF TO DLOPTLTA  PERIOTA   BENVLQTA  LOTA      00345600
345700              AGELIMA     PLCTRTTA  FAMINDTA  SRVGRUTA  CSTCOTTA  00345700
345800              AGEQLTA     COPAYITA  INTDESTA  CONDTG1A  CONDTG2A  00345800
345900                          PERLITTA  BISNDITA                      00345900
346000         IF INTDESKO  NOT =  IDPRODO                              00346000
346100            MOVE DFHBMASB  TO  IBGRIDA,  IPGNIDA,  IPGTIDA        00346100
346200                               IDGDIDA,  IPGPIDA,  IPGSIDA        00346200
346300            MOVE DFHBMABF  TO  IBGRSLTA, IPGNSLTA, IPGTSLTA       00346300
346400                               IDGDSLTA, IPGPSLTA, IPGSSLTA       00346400
346500            MOVE DFHBMUBF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00346500
346600                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00346600
346700         ELSE                                                     00346700
346800            MOVE DFHBMASF  TO  IBGRIDA,  IPGNIDA,  IPGTIDA        00346800
346900                               IDGDIDA,  IPGPIDA,  IPGSIDA        00346900
347000            MOVE DFHBMASF  TO  IBGRSLTA, IPGNSLTA, IPGTSLTA       00347000
347100                               IDGDSLTA, IPGPSLTA, IPGSSLTA       00347100
347200            MOVE DFHBMUNF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00347200
347300                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00347300
347400     ELSE                                                         00347400
347500         NEXT SENTENCE.                                           00347500
347600                                                                  00347600
347700                                                                  00347700
347800     IF  CDEINDO = '+CDE-'                                        00347800
347900     THEN                                                         00347900
348000*---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         00348000
348100         MOVE DFHBMABF TO DLOPTLTA  PERIOTA   BENVLQTA  LOTA      00348100
348200              AGELIMA     PLCTRTTA  FAMINDTA  SRVGRUTA  CSTCOTTA  00348200
348300              AGEQLTA     COPAYITA  INTDESTA  CONDTG1A  CONDTG2A  00348300
348400                          PERLITTA  BISNDITA                      00348400
348500*---------------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS        00348500
348600         MOVE DFHBMASF TO DELOPTNA  PERIODA   BENVLQLA  LOBA      00348600
348700        AGELIMLA AGELIMHA PLCTRMTA  FAMINDIA  SRVGRUPA  CSTCONTA  00348700
348800        AGEQLLA  AGEQLHA  COPAYINA  INTDESKA  CONDALLA  CONDEXCA  00348800
348900                          CONDICDA  CONDTABA  CONDMENA  CONDDRGA  00348900
349000                          CONDALCA  CONDOBCA  CONDOBNA  CONDMALA  00349000
349100                          CONDCARA  CONDOBSA  CONDKDYA  CONDACCA  00349100
349200                          CONDPECA  CONDNEMA  CONDSUIA  CONDTMJA  00349200
349300                          CONDEMCA  CONDEACA  CONDSMIA  CONDNSMA  00349300
349400                          PERLITTA  BISNDINA  CONDINFA  CONDLIFA  00349400
                                BENTYPA   TIERCDA   TIERLVA             00349410
349500         IF INTDESKO  NOT =  IDPRODO                              00349500
349600*--------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS               00349600
349700            MOVE DFHBMASF  TO  IBGROPTA,  IPGNOPTA,  IPGTOPTA     00349700
349800                               IDGDOPTA,  IPGPOPTA,  IPGSOPTA     00349800
349900            MOVE DFHBMABF  TO  IBGRIDA,   IPGNIDA,   IPGTIDA,     00349900
350000                               IDGDIDA,   IPGPIDA,   IPGSIDA      00350000
350100                               IBGRSLTA,  IPGNSLTA,  IPGTSLTA     00350100
350200                               IDGDSLTA,  IPGPSLTA,  IPGSSLTA     00350200
350300            IF  ERRMSGO > SPACES                                  00350300
350400            THEN                                                  00350400
350500                NEXT SENTENCE                                     00350500
350600            ELSE                                                  00350600
350700                SET  WT-01-INDEX  TO  +08                         00350700
350800                MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO 00350800
350900         ELSE                                                     00350900
351000            MOVE DFHBMUNF  TO  IBGROPTA,  IPGNOPTA,  IPGTOPTA     00351000
351100                               IDGDOPTA,  IPGPOPTA,  IPGSOPTA     00351100
351200            MOVE DFHBMASF  TO  IBGRIDA,   IPGNIDA,   IPGTIDA,     00351200
351300                               IDGDIDA,   IPGPIDA,   IPGSIDA      00351300
351400                               IBGRSLTA,  IPGNSLTA,  IPGTSLTA     00351400
351500                               IDGDSLTA,  IPGPSLTA,  IPGSSLTA     00351500
351600            IF  ERRMSGO > SPACES                                  00351600
351700            THEN                                                  00351700
351800                NEXT SENTENCE                                     00351800
351900            ELSE                                                  00351900
352000                SET  WT-01-INDEX  TO  +08                         00352000
352100                MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO 00352100
352200     ELSE                                                         00352200
352300         NEXT SENTENCE.                                           00352300
352400                                                                  00352400
352500                                                                  00352500
352600 7900-900-EXIT. EXIT.                                             00352600
352700                                                                  00352700
352800/*****************************************************************00352800
352900* 8000  XCTL SWITCH ADD DEL MODE                                 *00352900
353000*                                                                *00353000
353100*   THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO *00353100
353200*  ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS*00353200
353300*  THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL     *00353300
353400*  TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE*00353400
353500*  PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE   *00353500
353600*  IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).             *00353600
353700******************************************************************00353700
353800 8000-000-SWITCH-ADD-DEL-MODE   SECTION.                          00353800
353900 8000-010.                                                        00353900
354000                                                                  00354000
354100     IF DELADDI  =  'CHG/DEL'                                     00354100
354200        PERFORM 8100-000-DISPLAY-ADD-SCREEN.                      00354200
354300                                                                  00354300
354400     PERFORM 3100-000-READ-RECORD.                                00354400
354500     MOVE GAD-ENTRY-COUNT  TO  GAD-ENTRY-COUNT.                   00354500
354600                                                                  00354600
354700     IF  GAD-ENTRY-COUNT  >  1                                    00354700
354800     THEN                                                         00354800
354900         MOVE 'CHG/DEL'  TO  DELADDO                              00354900
355000         MOVE 'D'        TO  DELOLITO                             00355000
355100         MOVE SPACES     TO  COCURANO                             00355100
355200         MOVE DFHBMASK   TO  DLOPTLTA                             00355200
355300         MOVE DFHBMUNP   TO  DELOPTNA                             00355300
355400         SET  WT-01-INDEX                     TO +20              00355400
355500         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00355500
355600         SET GAD-INDEX   TO  1                                    00355600
355700         PERFORM 4400-000-BUILD-DISPLAY                           00355700
355800     ELSE                                                         00355800
355900         SET  WT-01-INDEX                     TO +12              00355900
356000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.         00356000
356100                                                                  00356100
356200 8000-900-EXIT. EXIT.                                             00356200
356300                                                                  00356300
356400/*****************************************************************00356400
356500* 8100  DISPLAY ADD SCREEN                                       *00356500
356600******************************************************************00356600
356700 8100-000-DISPLAY-ADD-SCREEN    SECTION.                          00356700
356800                                                                  00356800
356900     MOVE 'CHG/ADD'  TO  DELADDO.                                 00356900
357000     MOVE SPACES     TO  COCURANO.                                00357000
357100     MOVE DFHBMASD   TO  DLOPTLTA   DELOPTNA.                     00357100
357200     PERFORM 4100-000-DISPLAY-SKELETON.                           00357200
357300                                                                  00357300
357400 8100-900-EXIT. EXIT.                                             00357400
357500                                                                  00357500
357600/*****************************************************************00357600
357700* 9000  SEND ERASE THEN RETURN                                   *00357700
357800******************************************************************00357800
357900 9000-000-SEND-ERASE-RETURN     SECTION.                          00357900
358000 9000-010.                                                        00358000
358100                                                                  00358100
358200     MOVE DFHBMASD  TO  MAXOVRTA  REININTA  MANAPLTA  FDLRCLTA    00358200
358300                        MAXOVRDA  REININDA  MANAPLIA  FDLRCLIA    00358300
358400                        TIMEDLRA  TIMEDOLA.                       00358400
358500                                                                  00358500
358600     MOVE -1  TO  ERRMSGL.                                        00358600
358700                                                                  00358700
358800     EXEC CICS  SEND   MAP ('GA1XI01')  ERASE  CURSOR             00358800
358900                MAPSET('GA1XSET')   END-EXEC.                     00358900
359000                                                                  00359000
359100*    EXEC CICS  RETURN   END-EXEC.                                00359100
359200     EXEC CICS  RETURN TRANSID('GA1E')                            00359200
359300                COMMAREA(DFHCOMMAREA)                             00359300
359400                LENGTH  (EIBCALEN)                                00359400
359500                END-EXEC.                                         00359500
359600                                                                  00359600
359700 9000-900-EXIT. EXIT.                                             00359700
359800                                                                  00359800
359900/*****************************************************************00359900
360000* 9010  SEND DATAONLY AND RETURN                                 *00360000
360100******************************************************************00360100
360200 9010-000-SEND-DATAONLY-RETURN  SECTION.                          00360200
360300 9010-010.                                                        00360300
360400                                                                  00360400
360500     MOVE -1  TO  ERRMSGL.                                        00360500
360600                                                                  00360600
360700     EXEC CICS  SEND   MAP ('GA1XI01')  DATAONLY  CURSOR          00360700
360800                MAPSET('GA1XSET')  END-EXEC.                      00360800
360900                                                                  00360900
361000*    EXEC CICS  RETURN   END-EXEC.                                00361000
361100     EXEC CICS  RETURN TRANSID('GA1E')                            00361100
361200                COMMAREA(DFHCOMMAREA)                             00361200
361300                LENGTH  (EIBCALEN)                                00361300
361400                END-EXEC.                                         00361400
361500                                                                  00361500
361600 9010-900-EXIT. EXIT.                                             00361600
361700/*****************************************************************00361700
361800* 9200  GREGORIAN TO JULIAN                                      *00361800
361900*                                                                *00361900
362000*         MMDDYY---->YYDDD                                       *00362000
362100******************************************************************00362100
362200 9200-000-GREGORIAN-TO-JULIAN   SECTION.                          00362200
362300 9200-010.                                                        00362300
362400                                                                  00362400
362500     MOVE 'CNV'  TO  HGADATE-FUNC.                                00362500
362600     MOVE 'M'    TO  HGADATE-FORM1.                               00362600
362700     MOVE 'J'    TO  HGADATE-FORM2.                               00362700
362800     MOVE ZEROS  TO HGADATE-RETURN   HGADATE-AMOUNT.              00362800
362900                                                                  00362900
363000     EXEC  CICS LINK PROGRAM ('HGADATES')                         00363000
363100                     COMMAREA(HGADATES-COMMAREA)                  00363100
363200                     LENGTH  (LENGTH OF HGADATES-COMMAREA)        00363200
363300                     END-EXEC.                                    00363300
363400                                                                  00363400
363500                                                                  00363500
363600 9200-900-EXIT. EXIT.                                             00363600
363700                                                                  00363700
363800/*****************************************************************00363800
363900* 9300  JULIAN TO GREGORIAN                                      *00363900
364000*                                                                *00364000
364100*          YYDDD---->MMDDYY                                      *00364100
364200******************************************************************00364200
364300 9300-000-JULIAN-TO-GREGORIAN   SECTION.                          00364300
364400 9300-010.                                                        00364400
364500                                                                  00364500
364600     MOVE 'CNV'  TO  HGADATE-FUNC.                                00364600
364700     MOVE 'J'    TO  HGADATE-FORM1.                               00364700
364800     MOVE 'M'    TO  HGADATE-FORM2.                               00364800
364900     MOVE ZEROS  TO HGADATE-RETURN   HGADATE-AMOUNT.              00364900
365000                                                                  00365000
365100     EXEC  CICS LINK PROGRAM ('HGADATES')                         00365100
365200                     COMMAREA(HGADATES-COMMAREA)                  00365200
365300                     LENGTH  (LENGTH OF HGADATES-COMMAREA)        00365300
365400                     END-EXEC.                                    00365400
365500                                                                  00365500
365600 9300-900-EXIT. EXIT.                                             00365600
365700                                                                  00365700
365800/*****************************************************************00365800
365900* 9800  E R R O R   M S G   T H E N   A B E N D                  *00365900
366000*                                                                *00366000
366100*    THIS ROUTINE DISPLAYS THE PREVIOUSLY BUILT ERROR MESSAGE    *00366100
366200*  AND THEN ABENDS USING THE ABEND CODE EARLIER DEFINED.         *00366200
366300******************************************************************00366300
366400 9800-000-ERROR-MSG-THEN-ABEND  SECTION.                          00366400
366500 9800-010.                                                        00366500
366600                                                                  00366600
366700     MOVE -1               TO   MFRMSLTL.                         00366700
366800     MOVE WS-ABCODE-MSG    TO   ERRMSGO.                          00366800
366900                                                                  00366900
367000     EXEC CICS  SEND   MAP ('GA1XI01')  ERASE  CURSOR  WAIT       00367000
367100                MAPSET('GA1XSET')   END-EXEC.                     00367100
367200                                                                  00367200
367300     EXEC CICS  ABEND   ABCODE(WS-ABCODE)  END-EXEC.              00367300
367400                                                                  00367400
367500 9800-900-EXIT. EXIT.                                             00367500
