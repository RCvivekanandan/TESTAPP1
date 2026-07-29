000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID. GA1BPGM.                                             00000200
000300**** THIS IS A COBOL/2 PROGRAM ***                                00000300
000400 AUTHOR. T RAAK.                                                  00000400
000500 DATE-WRITTEN.   07/10/85.                                        00000500
000600 DATE-COMPILED.                                                   00000600
000700     SKIP3                                                        00000700
000800******************************************************************00000800
000900*   GA1BPGM         ALL LEVEL TABULAR MAINTENANCE PROGRAM        *00000900
001000*                        BENEFIT AGGREGATE MAXIMUMS     -   GA1B *00001000
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
003300*   FUNC CODE: GA1B                                              *00003300
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
006300*-NUM-* *-DATE-* *WHO* *-----------DESCRIPTION-------------------*00006300
006400******************************************************************00006400
006500*                                                                *00006500
006600*        06/25/03 DAF USE COPYBOOK GCTIPGPC INSTEAD OF GCTIPGTC  *00006600
006700*                                                                *00006700
006800*  D368  06/04/02  JP ADD SELECTIVE ADDITIONAL BENEFIT           *00006800
006900*                         DETERMINATION (SABD)                   *00006900
007000*                                                                *00007000
007100*  D365B 06/03/02  JP ADD COMBINATION APPLIED IND (CAPI)         *00007100
007200*                                                                *00007200
007300*  P???  11/14/01 AKK  ADD SUPPORT FOR 4 NEW BITS, 2 FOR EMER    *00007300
007400*                      TWO FOR SERIOUS MENTAL ILLNESS.           *00007400
007500*                                                                *00007500
007600*  D352  09/19/00 GDM ADD ACCUMULATOR IDENTIFIER                 *00007600
007700*                                                                *00007700
007800* P????  07/07/00 GSP   ADDED LOGIC FOR NEW #IPGS INTERNAL       *00007800
007900*                       TABULAR.                                 *00007900
008000*                                                                *00008000
008100* P????  11/19/99 FRY   ADD LENGTH PARAMETER TO THE RETURN       *00008100
008200*                       COMMAND WHEN DFHCOMMAREA IS SPECIFIED.   *00008200
008300*                                                                *00008300
008400*                                                                *00008400
008500*  D341  10/07/98 GDM   HIDE TIME/DOLLAR FIELD FROM SCREEN       *00008500
008600*                                                                *00008600
008700* 14726/ 04/29/98  AB   MODIFIED TO BECOME MILLENNIUM COMPLIANT  *00008700
008800* 15057                 AND TO ADD PACKAGE CODE, PLAN CODE, AND  *00008800
008900*                       INCREASE GROUP AND SECTION NUMBERS.      *00008900
009000*                                                                *00009000
009100* 14726/ 10/22/97 DAU ADDED CODE TO SUPPORT THE YEAR 2000 AND    *00009100
009200* 15057               THE EXPANSION OF THE GROUP SPECIFIC AND    *00009200
009300*                     CONTRACT KEY TO SUPPORT THE TEXAS MERGER.  *00009300
009400*                                                                *00009400
009500*  D303  02/03/97 DAU ADD FEAK INDICATOR                         *00009500
009600*                                                                *00009600
009700* 12262  02/28/92 TPM ADD NEW COND-BIT LIF  (LIFE-THREATING)     *00009700
009800*                     COND-LIFE-THREAT-BIT                       *00009800
009900*                                                                *00009900
010000*                                                                *00010000
010100*D12009 09/23/91  BSO   ADDED ERROR CHECKING ON ACTION CODE FOR  *00010100
010200*                       INTERNAL TABULARS AT BOTTOM OF SCREEN.   *00010200
010300*                                                                *00010300
010400*                                                                *00010400
010500*D12009 08/28/91  TPM   INCREASED THE FAMILY RELATION            *00010500
010600*                  FIELD    FROM ONE POSITION TO TWO POSITIONS.  *00010600
010700*                                                                *00010700
010800*                                                                *00010800
010900* 11836 07/09/91  ENW  INCLUDED THE FYI FIELD IN THE COMPARE     *00010900
011000*                      AREA.                                     *00011000
011100*                                                                *00011100
011200* 11154 02/19/91  NGE  REDUCE OCCURS MAX NUM FROM 46 TO 44.      *00011200
011300*                                                                *00011300
011400* 11154 01/17/91  NGE 1. ADD AGE-QUAL-IND-FROM AND AGE-QUAL-TO   *00011400
011500*                        TO ALL ACCUM TABULARS, AS CDE FIELDS.   *00011500
011600*                       2. REMOVE RELATIONSHIP-IND FROM CDE LOGIC*00011600
011700*                                                                *00011700
011800* 11154   10/23/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *00011800
011900* D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *00011900
012000* D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *00012000
012100* D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *00012100
012200*          |            5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *00012200
012300*          |            6. ADD NEW INTERNAL TABS #IDGD AND #IPGP.*00012300
012400*          V            7. >>> CONVERT TO COBOL/2 <<<.           *00012400
012500*                                                                *00012500
012600* D184/  04/05/90 ENW  ADD #IDGD AND #IPGP INTERNALS             *00012600
012700* D185/                ADD AGE LIMIT FIELDS.                     *00012700
012800* D238                                                           *00012800
012900*                                                                *00012900
013000* PG008 07/18/89 NGE  CDE MESSAGE SHOULD BE DISPLAYED WHEN ADDING*00013000
013100*                     OCCURS IN ADD/DEL MODE.                    *00013100
013200*                                                                *00013200
013300* D200 05/18/89  NGE   ADD TWO NEW COND-BITS INF AND TMJ         *00013300
013400*                      TEMPROMAND-JOINT AND INFERTILITY-CIND.    *00013400
013500*                                                                *00013500
013600* ???? 03/13/89  ENW   CHANGED 'DFHBMASK' TO 'DFHBMASF' IN       *00013600
013700*                      7900-000-RESET-ATTRIBUTES SECTION BECAUSE *00013700
013800*                      FIELDS THAT WEREN'T BEING RETURNED WERE   *00013800
013900*                      CAUSING EDIT PROBLEMS.                    *00013900
014000*                                                                *00014000
014100* D201    01/11/89  ENW  ADDED ASKIP, DARK FOR BISNDIN.          *00014100
014200*                                                                *00014200
014300* ????    09/14/88  NGE  FIX INTERNAL TABS DELETE LOGIC FOR      *00014300
014400*                        UPDATING CDE COUNTERS DEPENDING ON THE  *00014400
014500*                        INTRNL TAB RECORD NOT THE CDE STATUS    *00014500
014600*                        IN THE ACCUM RECORD ATTACHED. ALLOW     *00014600
014700*                        +CDE+ DISPLAY RETURNING FROM INTRN PGM. *00014700
014800* ????    08/03/88  NGE  FIX ADDING ACCURS LOGIC TO FLAG THE     *00014800
014900*                            ACCUMS AS A CDE & GAS1PGM INTERNAL  *00014900
015000*                            TAB LOGIC TO FLAG ITS ACCUM CDE     *00015000
015100*                            WHEN THE INTERNL FLAGED CDE.        *00015100
015200*                                                                *00015200
015300*  D126      02/24/88  JLA  1. CHANGE OPTION FILE SELECTION 'S'  *00015300
015400*                              TO 'A'.                           *00015400
015500*                                                                *00015500
015600* D143 01/29/88  DES  ADD CDE/NON-CDE CHANGES USING JERRY'S      *00015600
015700*                     SCHEME WHERE THE SPLIT IS PERFORMED        *00015700
015800*                     IN BATCH AND THEN MERGED BACK ONTO W/F     *00015800
015900*                                                                *00015900
016000*  ????      09/29/87  JLA  FIX EXISTING CDE PROBLEM IN THE      *00016000
016100*                             4600- SECTION THAT CAUSED THE CDE  *00016100
016200*                             MODIFIED STATUS TO BE SET.         *00016200
016300*                                                                *00016300
016400* N126 08/28/87  JLA  ADD LOGIC FOR SUICIDE BIT.                 *00016400
016500*                                                                *00016500
016600* N121 08/09/87  NE   ADD A NEW FIELD -DEFINITION-               *00016600
016700*                                                                *00016700
016800* D200 04/24/87  JLA  BREAK INTO MULTIPLE MODULES.               *00016800
016900*                                                                *00016900
017000*D0120 03/16/87  JLA  CHANGES FOR SINGLE TABULAR SUPPORT THAT    *00017000
017100*                     ARE EXECUTED FROM TRANSACTION GTM1:        *00017100
017200*                     1. WHEN CHECKING ENTRY TRANSACTION CODE    *00017200
017300*                        TREAT GTM1 THE SAME AS GC4A.            *00017300
017400*                     2. PF1/PF13 - TREAT THE SAME AS IF GC4A    *00017400
017500*                        HAD CALLED, XCTL TO ADD SCREEN PROGRAM  *00017500
017600*                     3. PF3/PF15 - CONSTRUCT COMMAREA AS IF     *00017600
017700*                        GC4A HAD CALLED, XCTL TO GTM1PGM.       *00017700
017800*                     4. ALLOW ATTACHMENT (MAP FROM) OF PROD-    *00017800
017900*                        UCTION TABULARS, BUT PROHIBIT ATTACH-   *00017900
018000*                        ING SINGLE TABULARS UNDER SINGLE TAB-   *00018000
018100*                        ULAR SUPPORT.  DON'T CONSTRUCT C3       *00018100
018200*                        WORKFILE RECORD.  DON'T PASS CONTROL    *00018200
018300*                        TO INTERNAL TABULAR MAINTENANCE PGM.    *00018300
018400*                        DON'T CHANGE INTERNAL SLOT# ON SCREEN   *00018400
018500*                        TO ALL 9K NUMBER.                       *00018500
018600*                     5. PROHIBIT INTERNAL TAB CHANGES UNDER     *00018600
018700*                        STS.                                    *00018700
018800*                     6. PROHIBIT MAPPING FROM SKELETON UNDER    *00018800
018900*                        STS.                                    *00018900
019000*                                                                *00019000
019100*N106    02/26/87 RKH   ADDED LOGIC FOR THE FYI FIELD WHICH IS   *00019100
019200*N118                      TO BE VALIDATED & THE LOGIC TO ONLY   *00019200
019300*                          DISPLAY THE TABULAR OCCURANCE NUMBER. *00019300
019400*                                                                *00019400
019500*CDEL502 9/29/86 JLA    1. CHANGE COPY-SORTABLE-FLDS FROM X(128  *00019500
019600*                          TO X(125) AND REPLACE LAST THREE      *00019600
019700*                          BYTES WITH COPY-SORT-FYI X(3) NOT     *00019700
019800*                          INCLUDED IN TABULAR ENTRY SORT.       *00019800
019900*                       2. INITIAL THE CDE STATUS IN ANY         *00019900
020000*                          WORKFILE RECORDS CREATED TO \
020100*                       3. IF PRODUCTION TABULAR RECORD IS       *00020100
020200*                          BEING CHANGED AND CONTAINS CRITICAL   *00020200
020300*                          DATA ELEMENTS:                        *00020300
020400*                          A. INITIAL SCREEN :                   *00020400
020500*                             1) CDE STATUS(\
020600*                                 - HIGH-LIGHT CDE LABELS,       *00020600
020700*                                   SHOW +CDE+ INDICATOR.        *00020700
020800*                             2) CDE STATUS NOT (\
020900*                                 - HIGH-LIGHT CDE LABELS,       *00020900
021000*                                   HIGH-LIGHT AND PROTECT CDE   *00021000
021100*                                   ELEMENTS,                    *00021100
021200*                                   SHOW +CDE+ INDICATOR.        *00021200
021300*                          B. IF CDE ELEMENTS ARE CHANGED, SET   *00021300
021400*                             WORKFILE TABULAR CDE STATUS CODE   *00021400
021500*                             TO \
021600*                             OR GROUP SPECIFIC CONTROL          *00021600
021700*                             RECORD CDE STATUS APPROPRIATELY,   *00021700
021800*                             ISSUE CDE CHANGE MESSAGE AND       *00021800
021900*                             POSITION CURSOR ON +CDE+.  THE     *00021900
022000*                             OPERATOR THEN ADVANCES TO NEXT     *00022000
022100*                             SCREEN BY PRESSING ENTER A SECOND  *00022100
022200*                             TIME.                              *00022200
022300*                                                                *00022300
022400*CDEL501 8/22/86 JLA    DETERMINE IF POTENTIALLY CRITICAL DATA   *00022400
022500*                       ELEMENTS ARE CRITICAL BASED ON THE TRANS *00022500
022600*                       ROUTING FILE (PGM=GCTRSRT).  IF THEY     *00022600
022700*                       ARE CRITICAL AND THE USER IS DOING A     *00022700
022800*                       CHANGE TO A WORKFILE GROUP SPECIFIC      *00022800
022900*                       RECORD, PROTECT THE CRITICAL DATA ELE-   *00022900
023000*                       MENT ON THE SCREEN.                      *00023000
023100*  ?   08/12/86  AHL/DF MODIFIED 1100- ROUTINE SO THAT IT        *00023100
023200*                       FINISHES VALIDATING EACH DATA ELEMENT    *00023200
023300*                       IN THE CORRECT SEQUENCE ACCORDING TO     *00023300
023400*                       THE SCREEN LAYOUT.                       *00023400
023500*                                                                *00023500
023600*D094  08/04/86  AHL  REVISED 'NEG' LOGIC TO LET OPERATOR USE    *00023600
023700*                     EITHER 'NEG' OR DOLLARS & CENTS WITH       *00023700
023800*                     DECIMAL POINT FOR VALUE LIMIT FIELD WHEN   *00023800
023900*                     VALUE QUALIFIER = '5'.                     *00023900
024000*                                                                *00024000
024100*N112  08/01/86  AMJ  ADDED DAY FACTOR INDICATOR                 *00024100
024200*                                                                *00024200
024300*      07/30/86  AMJ  FIXED ERROR MESSAGES                       *00024300
024400*                                                                *00024400
024500*N108  07/28/86  RKH  ADDED TWO NEW CONDITION BITS               *00024500
024600*                     PRE-EXISTING CONDITIONS                    *00024600
024700*                     NON-EMERGENCY CONDITION.                   *00024700
024800*                                                                *00024800
024900*D094  07/23/86  AKM  ALLOWED 10 POSITIONS FOR VALUE LIMIT       *00024900
025000*                     FIELD SO THAT OPERATORS CAN ENTER          *00025000
025100*                     1 MILLION AS '1000000.00'.                 *00025100
025200*                                                                *00025200
025300*      06/26/86  JTC  ADDED LOGICAL EDITS                        *00025300
025400*                                                                *00025400
025500*      06/19/86  JTC  MOVED PF4/PF16 LOGIC TO AFTER VALIDATION   *00025500
025600*                     SO THAT INCORRECT RECORDS WOULD NOT BE     *00025600
025700*                     ADDED TO THE FILE.  ADDED AN INVALID       *00025700
025800*                     PF MESSAGE TO COVER THE ABOVE CASE.        *00025800
025900*                                                                *00025900
026000*                     ADDED A CHANGE TO ALLOW SCROLLING FORWARD  *00026000
026100*                     IF THE ONLY THING 'WRONG' IS AN EMPTY      *00026100
026200*                     TABLE.                                     *00026200
026300*                                                                *00026300
026400*                     FIXED THE SCREEN NOT BEING REFRESHED       *00026400
026500*                     PROPERLY FOLLOWING AN ADD WITH PF4/PF16    *00026500
026600*                                                                *00026600
026700*P495  05/30/86  AMJ  CHANGED TO ALLOW IPGT AND IPGN AT THE      *00026700
026800*                     SAME TIME                                  *00026800
026900*                                                                *00026900
027000*M106  05/30/86  AMJ  FIXED ATTRIBUTE ON ADD TO/OVERLAY FIELD    *00027000
027100*                                                                *00027100
027200*      05/14/86  MDD  CHANGED THE SEQUENCE OF THE EDITS TO BE    *00027200
027300*                      IN SYNC WITH THE SCREEN.                  *00027300
027400*                                                                *00027400
027500*      02/21/86  MDD  ADDED VALIDATION FOR FOLLOWING FIELDS:     *00027500
027600*                     'ADD-TO-OVERLAY INDICATOR',                *00027600
027700*                     'BENEFIT PERIOD',                          *00027700
027800*                     'FAMILY OR INDIVIDUAL INDICATOR',          *00027800
027900*                     'LINE OF BUSINESS',                        *00027900
028000*                     'INTERNAL DESCRIPTOR',                     *00028000
028100*                     'SERVICE GROUP',                           *00028100
028200*                     'CO-PAY INDICATOR',                        *00028200
028300*                     'COST CONTAINMENT INDICATOR',              *00028300
028400*                     'REINSTATEMENT INDICATOR',                 *00028400
028500*                     'BENEFIT PERIOD TIME QUALIFIER',           *00028500
028600*                     'BAMA BENEFIT PERIOD OVERRIDE',            *00028600
028700*                     'INTERVAL TYPE',                           *00028700
028800*                     'INTERVAL OVERRIDE INDICATOR',             *00028800
028900*                     'PLACE OF TREATMENT INDICATOR',            *00028900
029000*                     'VALUE QUALIFIER'                          *00029000
029100*                                                                *00029100
029200*      01/15/86  RKH   ADDED CODE FOR THE NEG VALUE LIMIT        *00029200
029300*                                                                *00029300
029400*      11/18/85  LET   ADDED CODE FOR THE NEW CONDITION BIT      *00029400
029500*                      NAMED ACCIDENT.                           *00029500
029600*                                                                *00029600
029700*      11/08/85  ENW   REVISED LOGIC TO ACCEPT SPACES IN THE     *00029700
029800*                      INTERNAL DESCRIPTOR FIELD INSTEAD OF      *00029800
029900*                      ZEROS.  ALSO ADDED NEW COPY MEMBER        *00029900
030000*                      'GCVALTAB'.                               *00030000
030100*                                                                *00030100
030200*                                                                *00030200
030300*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00030300
030400*                                                                *00030400
030500* P09400     11-07-06   GF    ADD ASCEND/DESCEND AND BISCENDING  *00030500
030600*                             INDICATORS                         *00030600
030700*                                                                *00030700
030800*            05-07-07   LR    RECOMPILE FOR CHANGES IN GASEDIT1  *00030800
030900*                                                                *00030900
030800*            10-15-10   MJL   ALLOW 'UNL' VALUE.                 *00030910
030900*                                                                *00030920
      * P21595 09/19/16   HSB CHANGES FOR GCPS NEW FIELDS BENEFIT      *00030930
      *                       TYPE CODE,TIER CODE,TIER LEVEL.          *00030940
SI0724*                                                                *00030941
SI0724* P56703  05/08/24  SI  CHANGES FOR-PEAQ COPYBOOK EXPANSION      *00030950
SI0724*                       COPY ABM, ACP, ACL, ADL, AOL,            *00030960
SI0724*                       GCCDRLEN                                 *00030970
031000******************************************************************00031000
031100    SKIP3                                                         00031100
031200 ENVIRONMENT DIVISION.                                            00031200
031300/    D A T A   D I V I S I O N                                    00031300
031400 DATA DIVISION.                                                   00031400
031500 WORKING-STORAGE SECTION.                                         00031500
031600 01  WS-BEGIN                    PIC X(24)  VALUE                 00031600
031700     '***GA1BPGM WS BEGINS***'.                                   00031700
031800                                                                  00031800
031900*     T I T L E   L I N E S                                       00031900
032000 01  WS-TITLE-LINES.                                              00032000
032100 COPY GCMHLINE.                                                   00032100
032200*****05  GROUP-SPECIFIC-TITLE-LINE       PIC X(42)                00032200
032300*      VALUE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'.        00032300
032400*    05  GROUP-SPECIFIC-ID-LINE.                                  00032400
032500*      10  FILLER                        PIC X(20)                00032500
032600*        VALUE 'GROUP SPECIFIC ID= '.                             00032600
032700*      10  FILLER                        PIC X(5) VALUE 'GRP= '.  00032700
032800*      10  GRP-SPEC-GROUP-NO             PIC X(6).                00032800
032900*      10  FILLER                        PIC X(6) VALUE ' SEC= '. 00032900
033000*      10  GRP-SPEC-SECTION-NO           PIC X(4).                00033000
033100*      10  FILLER                        PIC X(5) VALUE ' FR= '.  00033100
033200*      10  GRP-SPEC-FAM-REL-LVL          PIC XX.                  00033200
033300*      10  FILLER                        PIC X(7) VALUE ' EFDT= '.00033300
033400*      10  GRP-SPEC-EFF-DATE             PIC X(6).                00033400
033500*    05  CONTRACT-TITLE-LINE             PIC X(42)                00033500
033600*      VALUE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'.         00033600
033700*    05  CONTRACT-ID-LINE.                                        00033700
033800*      10  FILLER                      PIC X(14)                  00033800
033900*        VALUE 'CONTRACT ID= '.                                   00033900
034000*      10  FILLER                      PIC X(5) VALUE 'GRP= '.    00034000
034100*      10  CONTRACT-GROUP-NO           PIC X(6).                  00034100
034200*      10  FILLER                      PIC X(6) VALUE ' SEC= '.   00034200
034300*      10  CONTRACT-SECTION-NO         PIC X(4).                  00034300
034400*      10  FILLER                      PIC X(6) VALUE ' LOB= '.   00034400
034500*      10  CONTRACT-LOB                PIC X.                     00034500
034600*      10  FILLER                      PIC X(6) VALUE ' PRV= '.   00034600
034700*      10  CONTRACT-PROV-CTL           PIC XX.                    00034700
034800*      10  FILLER                      PIC X(5) VALUE ' FR= '.    00034800
034900*      10  CONTRACT-FAM-REL-LVL        PIC XX.                    00034900
035000*      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  00035000
035100*      10  CONTRACT-EFF-DATE               PIC X(6).              00035100
035200*    05  BENEFIT-PROVISION-TITLE-LINE    PIC X(42)                00035200
035300*      VALUE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'.        00035300
035400*    05  BENEFIT-PROVISION-ID-LINE.                               00035400
035500*      10  FILLER                      PIC X(5) VALUE 'GRP= '.    00035500
035600*      10  BEN-PROV-GROUP-NO           PIC X(6).                  00035600
035700*      10  FILLER                      PIC X(6) VALUE ' SEC= '.   00035700
035800*      10  BEN-PROV-SECTION-NO         PIC X(4).                  00035800
035900*      10  FILLER                      PIC X(6) VALUE ' LOB= '.   00035900
036000*      10  BEN-PROV-LOB                PIC X.                     00036000
036100*      10  FILLER                      PIC X(6) VALUE ' PRV= '.   00036100
036200*      10  BEN-PROV-PROV-CTL           PIC XX.                    00036200
036300*      10  FILLER                      PIC X(5) VALUE ' FR= '.    00036300
036400*      10  BEN-PROV-FAM-REL-LVL        PIC XX.                    00036400
036500*      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  00036500
036600*      10  BEN-PROV-EFF-DATE           PIC X(6).                  00036600
036700*      10  FILLER                      PIC X(8) VALUE ' BPVID= '. 00036700
036800*      10  BEN-PROV-ID-NO              PIC X(6).                  00036800
036900*    05  ABM-TITLE-LINE                PIC X(26)                  00036900
037000*********VALUE 'BENEFIT AGGREGATE MAXIMUMS'.                      00037000
037100/     A L T E R N A T I V E   W O R K F I L E   K E Y S           00037100
037200 01  FILLER                      PIC X(32)  VALUE                 00037200
037300     '*** ALTERNATIVE WORKFILE KEY ***'.                          00037300
037400 01  SAVE-WS-ALT-WORKFILE-KEYS.                                   00037400
037500     05 FILLER                   PIC X(63) VALUE SPACES.          00037500
037600                                                                  00037600
037700 01  WS-ALT-WORKFILE-KEYS.                                        00037700
037800 COPY GCWRKKEY.                                                   00037800
037900                                                                  00037900
038000                                                                  00038000
038100/    D A T E   F O R M A T T I N G   A R E A                      00038100
038200 01  HGADATES-COMMAREA.                                           00038200
038300 COPY HGCDAT01.                                                   00038300
038400                                                                  00038400
038500*  *** WORKFIELDS, AND SWITCHES **                                00038500
038600 01  WS-WORK-FIELDS.                                              00038600
038700                                                                  00038700
038800     05  GCTRSRT-COMMAREA-LEN      PIC S9(4)  COMP VALUE +100.    00038800
038900     05  WS-HEX-00                     PIC X    VALUE LOW-VALUE.  00038900
039000     05  WS-ONE-LOW                PIC X(01) VALUE LOW-VALUES.    00039000
039100     05  SAVE-COPY-FROM-SLOT           PIC 9(7).                  00039100
039200                                                                  00039200
039300     05  WS-CDE-REQUEST-CODES.                                    00039300
039400         10  WS-REQUEST-4500-CDE-PROTECT    PIC X(4) VALUE '4500'.00039400
039500         10  WS-REQUEST-4600-CDE-STATUS     PIC X(4) VALUE '4600'.00039500
039600         10  WS-REQUEST-4700-CNTL-UPDATE    PIC X(4) VALUE '4700'.00039600
039700                                                                  00039700
039800*     I N T E R N A L   T A B U L A R   P R O G R A M   N A M E   00039800
039900 01  WS-INTERNAL-TABULAR-PGM-ID        PIC X(8).                  00039900
040000                                                                  00040000
040100** ***ALL LEVEL TABULAR ENTRY SAVED HERE DURING SORT ***          00040100
040200 01  WS-ENTRY                          PIC X(176).                00040200
040300     SKIP3                                                        00040300
040400/    A T T R I B U T E S                                          00040400
040500 COPY DFHBMSCA.                                                   00040500
040600     02  DFHBMABF                PIC X VALUE '9'.                 00040600
040700/    A T T E N T I O N   I D E N T I F I E R S                    00040700
040800 COPY DFHAID.                                                     00040800
040900/    R E C O R D   L E N G T H S                                  00040900
041000                                                                  00041000
041100 01  WS-RECORD-LENGTHS.                                           00041100
041200*   05 WS-COMM-KEY-PNTR-LEN           PIC S9(4) COMP  VALUE +4.   00041200
041300*   05 GAS1UPD-COMMAREA-LEN           PIC S9(4) COMP  VALUE +420. 00041300
041400*   05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP  VALUE +100. 00041400
SI0724*   05 WS-COPY-TABLE-LEN              PIC S9(4) COMP  VALUE +7744.00041500
SI0724    05 WS-COPY-TABLE-LEN              PIC S9(4) COMP VALUE +30800.00041510
041600    05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP  VALUE +0.   00041600
041700    05 WS-IO-PARM-WRK-BEN-PROV-LEN    PIC S9(4) COMP  VALUE +0.   00041700
041800    05 WS-IO-PARM-WRK-CONTRACT-LEN    PIC S9(4) COMP  VALUE +0.   00041800
041900    05 WS-IO-PARM-WRK-CONTROL-LEN     PIC S9(4) COMP  VALUE +0.   00041900
042000    05 WS-IO-PARM-WRK-GRP-SPEC-LEN    PIC S9(4) COMP  VALUE +0.   00042000
042100    05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP  VALUE +0.   00042100
042200    05 WS-WRK-BEN-PROV-LEN            PIC S9(4) COMP  VALUE +0.   00042200
042300    05 WS-WRK-CONTRACT-LEN            PIC S9(4) COMP  VALUE +0.   00042300
042400    05 WS-WRK-GRP-SPEC-LEN            PIC S9(4) COMP  VALUE +0.   00042400
042500                                                                  00042500
042600******************************************************************00042600
042700** REQUIRED FOR N118 - DISPLAY OF TABULAR OCCURS (INDEX)        **00042700
042800******************************************************************00042800
042900 01  CURNT-OCURS-BIN             PIC 9(4)  COMP.                  00042900
043000 01  CURNT-OCURS-PKD             PIC 9(4).                        00043000
043100 01  CURNT-OCURS-ALH      REDEFINES   CURNT-OCURS-PKD.            00043100
SI0724*    05  FILLER                  PIC XX.                          00043200
SI0724*    05  CURNT-OCCURS-OUT        PIC XX.                          00043300
SI0724     05  FILLER                  PIC X.                           00043310
SI0724     05  CURNT-OCCURS-OUT        PIC XXX.                         00043320
043400                                                                  00043400
043500 01  TOTAL-OCURS-UNK             PIC 9(5).                        00043500
043600 01  TOTAL-OCURS-ALH      REDEFINES   TOTAL-OCURS-UNK.            00043600
SI0724*    05  FILLER                  PIC XXX.                         00043700
SI0724*    05  TOTAL-OCCURS-OUT        PIC XX.                          00043800
SI0724     05  FILLER                  PIC XX.                          00043810
SI0724     05  TOTAL-OCCURS-OUT        PIC XXX.                         00043820
043900/                                                                 00043900
044000 01  WS-GC-RECORD-LENGTHS.                                        00044000
044100     COPY GCCDRLEN.                                               00044100
044200/    A B E N D   A R E A                                          00044200
044300                                                                  00044300
044400 01  WS-01-ABEND-AREA.                                            00044400
044500     05  FILLER                   PIC X(16)  VALUE                00044500
044600         '** ABEND AREA **'.                                      00044600
044700                                                                  00044700
044800     05  WS-ABCODE-CODES-AND-MSG.                                 00044800
044900         10  WS-ABCODE                  PIC X(04)  VALUE  SPACES. 00044900
045000         10  WS-ABCODE-MSG              PIC X(79)  VALUE  SPACES. 00045000
045100                                                                  00045100
045200         10  WS-ABCODE-1BC1             PIC X(04)  VALUE  '1BC1'. 00045200
045300         10  WS-ABCODE-1BC1-MSG         PIC X(79)  VALUE          00045300
045400             '*** INVALID PARAMETER LENGTH FOUND ***              00045400
045500-            '                           '.                       00045500
045600         10  WS-ABCODE-1BC2             PIC X(04)  VALUE  '1BC2'. 00045600
045700         10  WS-ABCODE-1BC2-MSG         PIC X(79)  VALUE          00045700
045800             '*** WRONG RECORD STATUS PASSED TO THIS PGM ***      00045800
045900-            '                           '.                       00045900
046000         10  WS-ABCODE-1BC3             PIC X(04)  VALUE  '1BC3'. 00046000
046100         10  WS-ABCODE-1BC3-MSG         PIC X(79)  VALUE          00046100
046200             '*** WRONG RECORD TYPE PASSED TO THIS PGM ***        00046200
046300-            '                           '.                       00046300
046400         10  WS-ABCODE-1BF1             PIC X(04)  VALUE  '1BF1'. 00046400
046500         10  WS-ABCODE-1BF1-MSG         PIC X(79)  VALUE          00046500
046600             '*** A SKELETON CAN NOT BE FOUND FOR AN INTERNAL TABU00046600
046700-            'LAR.  CONTACT SYSTEMS ***  '.                       00046700
046800         10  WS-ABCODE-1BF2             PIC X(04)  VALUE  '1BF2'. 00046800
046900         10  WS-ABCODE-1BF2-MSG         PIC X(79)  VALUE          00046900
047000             '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASE00047000
047100-            ' CONTACT SYSTEMS ***       '.                       00047100
047200         10  WS-ABCODE-1BF3             PIC X(04)  VALUE  '1BF3'. 00047200
047300         10  WS-ABCODE-1BF3-MSG         PIC X(79)  VALUE          00047300
047400             '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASE00047400
047500-            ' CONTACT SYSTEMS ***       '.                       00047500
047600         10  WS-ABCODE-1BF4             PIC X(04)  VALUE  '1BF4'. 00047600
047700         10  WS-ABCODE-1BF4-MSG         PIC X(79)  VALUE          00047700
047800             '*** ERROR REWRITING ALL LEVEL TABULAR.  PLEASE CONTA00047800
047900-            'CT SYSTEMS ***             '.                       00047900
048000         10  WS-ABCODE-1BF5             PIC X(04)  VALUE  '1BF5'. 00048000
048100         10  WS-ABCODE-1BF5-MSG         PIC X(79)  VALUE          00048100
048200             'THE INTERNAL TABULAR CAN NOT BE READ FROM THE WORKFI00048200
048300-            'LE.  PLEASE CONTACT SYSTEMS'.                       00048300
048400         10  WS-ABCODE-1BF6             PIC X(04)  VALUE  '1BF6'. 00048400
048500         10  WS-ABCODE-1BF6-MSG         PIC X(79)  VALUE          00048500
048600             'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORKFIL00048600
048700-            'E.  PLEASE CONTACT SYSTEMS '.                       00048700
048800         10  WS-ABCODE-1BF7             PIC X(04)  VALUE  '1BF7'. 00048800
048900         10  WS-ABCODE-1BF7-MSG         PIC X(79)  VALUE          00048900
049000             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00049000
049100-            ' SYSTEMS ***               '.                       00049100
049200         10  WS-ABCODE-1BF9             PIC X(04)  VALUE  '1BF9'. 00049200
049300         10  WS-ABCODE-1BF9-MSG         PIC X(79)  VALUE          00049300
049400             'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORFILE00049400
049500-            '.  PLEASE CONTACT SYSTEMS  '.                       00049500
049600         10  WS-ABCODE-1BFA             PIC X(04)  VALUE  '1BFA'. 00049600
049700         10  WS-ABCODE-1BFA-MSG         PIC X(79)  VALUE          00049700
049800             '*** THE INTERNAL TABULAR CAN NOT BE DELETED, PLEASE 00049800
049900-            'CONTACT SYSTEMS ***        '.                       00049900
050000         10  WS-ABCODE-1BFB             PIC X(04)  VALUE  '1BFB'. 00050000
050100         10  WS-ABCODE-1BFB-MSG         PIC X(79)  VALUE          00050100
050200             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00050200
050300-            ' SYSTEMS ***               '.                       00050300
050400         10  WS-ABCODE-1BFC             PIC X(04)  VALUE  '1BFC'. 00050400
050500         10  WS-ABCODE-1BFC-MSG         PIC X(79)  VALUE          00050500
050600             '*** ERROR WHEN DELETING INTERNAL TAB.  PLEASE CONTAC00050600
050700-            'T SYSTEMS ***              '.                       00050700
050800         10  WS-ABCODE-1BFJ             PIC X(04)  VALUE  '1BFJ'. 00050800
050900         10  WS-ABCODE-1BFJ-MSG         PIC X(79)  VALUE          00050900
051000             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00051000
051100-            ' SYSTEMS ***               '.                       00051100
051200         10  WS-ABCODE-1BFK             PIC X(04)  VALUE  '1BFK'. 00051200
051300         10  WS-ABCODE-1BFK-MSG         PIC X(79)  VALUE          00051300
051400             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00051400
051500-            ' SYSTEMS ***               '.                       00051500
051600         10  WS-ABCODE-1BFL             PIC X(04)  VALUE  '1BFL'. 00051600
051700         10  WS-ABCODE-1BFL-MSG         PIC X(79)  VALUE          00051700
051800             '*** ERROR READING GROUP SPECIFIC RECORD TO RETURN TO00051800
051900-            'MENU.  CONTACT SYSTEMS *** '.                       00051900
052000         10  WS-ABCODE-1BFM             PIC X(04)  VALUE  '1BFM'. 00052000
052100         10  WS-ABCODE-1BFM-MSG         PIC X(79)  VALUE          00052100
052200             '*** ERROR READING CONTRACT MASTER TO RETURN TO THE  00052200
052300-            'MENU.  CONTACT SYSTEMS *** '.                       00052300
052400         10  WS-ABCODE-1BFN             PIC X(04)  VALUE  '1BFN'. 00052400
052500         10  WS-ABCODE-1BFN-MSG         PIC X(79)  VALUE          00052500
052600             '*** ERROR READING BENEFIT PROV RECORD TO RETURN TO M00052600
052700-            'ENU.  CONTACT SYSTEMS ***  '.                       00052700
052800         10  WS-ABCODE-1BFO             PIC X(04)  VALUE  '1BFO'. 00052800
052900         10  WS-ABCODE-1BFO-MSG         PIC X(79)  VALUE          00052900
053000             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00053000
053100-            ' SYSTEMS ***               '.                       00053100
053200         10  WS-ABCODE-1BFP             PIC X(04)  VALUE  '1BFP'. 00053200
053300         10  WS-ABCODE-1BFP-MSG         PIC X(79)  VALUE          00053300
053400             '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACT00053400
053500-            ' SYSTEMS ***               '.                       00053500
053600         10  WS-ABCODE-1BFQ             PIC X(04)  VALUE  '1BFQ'. 00053600
053700         10  WS-ABCODE-1BFQ-MSG         PIC X(79)  VALUE          00053700
053800             '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTA00053800
053900-            'CT SYSTEMS ***             '.                       00053900
054000         10  WS-ABCODE-1BFR             PIC X(04)  VALUE  '1BFR'. 00054000
054100         10  WS-ABCODE-1BFR-MSG         PIC X(79)  VALUE          00054100
054200             '*** ERROR READING W/F ALL LVL TAB.    PLEASE CONTACT00054200
054300-            ' SYSTEMS ***               '.                       00054300
054400         10  WS-ABCODE-1BFS             PIC X(04)  VALUE  '1BFS'. 00054400
054500         10  WS-ABCODE-1BFS-MSG         PIC X(79)  VALUE          00054500
054600             '*** ERROR REWRITING W/F ALL LVL TAB.  PLEASE CONTACT00054600
054700-            ' SYSTEMS ***               '.                       00054700
054800         10  WS-ABCODE-1BFT             PIC X(04)  VALUE  '1BFT'. 00054800
054900         10  WS-ABCODE-1BFT-MSG         PIC X(79)  VALUE          00054900
055000             '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACT00055000
055100-            ' SYSTEMS ***               '.                       00055100
055200         10  WS-ABCODE-1BFU             PIC X(04)  VALUE  '1BFU'. 00055200
055300         10  WS-ABCODE-1BFU-MSG         PIC X(79)  VALUE          00055300
055400             '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTA00055400
055500-            'CT SYSTEMS ***             '.                       00055500
055600         10  WS-ABCODE-1BL1             PIC X(04)  VALUE  '1BL1'. 00055600
055700         10  WS-ABCODE-1BL1-MSG         PIC X(79)  VALUE          00055700
055800             '*** THE OCCURS WE ARE TO UPDATE HAS BEEN DELETED ***00055800
055900-            '                           '.                       00055900
056000         10  WS-ABCODE-1BL2             PIC X(04)  VALUE  '1BL2'. 00056000
056100         10  WS-ABCODE-1BL2-MSG         PIC X(79)  VALUE          00056100
056200             '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTE00056200
056300-            'MS ***                     '.                       00056300
056400         10  WS-ABCODE-1BL3             PIC X(04)  VALUE  '1BL3'. 00056400
056500         10  WS-ABCODE-1BL3-MSG         PIC X(79)  VALUE          00056500
056600             '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTE00056600
056700-            'EMS ***                    '.                       00056700
056800         10  WS-ABCODE-1BL4             PIC X(04)  VALUE  '1BL4'. 00056800
056900         10  WS-ABCODE-1BL4-MSG         PIC X(79)  VALUE          00056900
057000             '*** THE OCCURS WE ARE TO DISPLAY HAS BEEN DELETED   00057000
057100-            '                           '.                       00057100
057200         10  WS-ABCODE-1BLX             PIC X(04)  VALUE  '1BLX'. 00057200
057300         10  WS-ABCODE-1BLX-MSG         PIC X(79)  VALUE          00057300
057400             '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTE00057400
057500-            'MS ***                     '.                       00057500
057600         10  WS-ABCODE-1BP1             PIC X(04)  VALUE  '1BP1'. 00057600
057700         10  WS-ABCODE-1BP1-MSG         PIC X(79)  VALUE          00057700
057800             '????????????????????????????????????????????????????00057800
057900-            '???????????????????????????'.                       00057900
058000                                                                  00058000
058100/    M E S S A G E   T A B L E                                    00058100
058200******************************************************************00058200
058300 01  WT-01-TABLE.                                                 00058300
058400     05  FILLER                  PIC X(16) VALUE                  00058400
058500         '* WT-01-TABLE  *'.                                      00058500
058600                                                                  00058600
058700 01  FILLER.                                                      00058700
058800     05  WT-01-MESSAGE-VALUES.                                    00058800
058900*----------------------------------------------------------------*00058900
059000         10  WT-01-ENTRY-001.                                     00059000
059100             15  FILLER              PIC X(2)  VALUE '¬>'.        00059100
059200             15  WT-01-MESSAGE-TEXT-001.                          00059200
059300                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00059300
059400                 20  FILLER          PIC X(1)  VALUE  '-'.        00059400
059500                 20  FILLER          PIC X(3)  VALUE  '001'.      00059500
059600                 20  FILLER          PIC X(1)  VALUE  ' '.        00059600
059700                 20  FILLER          PIC X(70) VALUE              00059700
059800                     '#IBGR HAS BEEN SUCCESSFULLY MAPPED          00059800
059900-                    '                         '.                 00059900
060000             15  FILLER              PIC X(2)  VALUE '<¬'.        00060000
060100*----------------------------------------------------------------*00060100
060200         10  WT-01-ENTRY-002.                                     00060200
060300             15  FILLER              PIC X(2)  VALUE '¬>'.        00060300
060400             15  WT-01-MESSAGE-TEXT-002.                          00060400
060500                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00060500
060600                 20  FILLER          PIC X(1)  VALUE  '-'.        00060600
060700                 20  FILLER          PIC X(3)  VALUE  '002'.      00060700
060800                 20  FILLER          PIC X(1)  VALUE  ' '.        00060800
060900                 20  FILLER          PIC X(70) VALUE              00060900
061000                     '#IPGN HAS BEEN SUCCESSFULLY MAPPED          00061000
061100-                    '                         '.                 00061100
061200             15  FILLER              PIC X(2)  VALUE '<¬'.        00061200
061300*----------------------------------------------------------------*00061300
061400         10  WT-01-ENTRY-003.                                     00061400
061500             15  FILLER              PIC X(2)  VALUE '¬>'.        00061500
061600             15  WT-01-MESSAGE-TEXT-003.                          00061600
061700                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00061700
061800                 20  FILLER          PIC X(1)  VALUE  '-'.        00061800
061900                 20  FILLER          PIC X(3)  VALUE  '003'.      00061900
062000                 20  FILLER          PIC X(1)  VALUE  ' '.        00062000
062100                 20  FILLER          PIC X(70) VALUE              00062100
062200                     '#IPGT HAS BEEN SUCCESSFULLY MAPPED          00062200
062300-                    '                         '.                 00062300
062400             15  FILLER              PIC X(2)  VALUE '<¬'.        00062400
062500*----------------------------------------------------------------*00062500
062600         10  WT-01-ENTRY-004.                                     00062600
062700             15  FILLER              PIC X(2)  VALUE '¬>'.        00062700
062800             15  WT-01-MESSAGE-TEXT-004.                          00062800
062900                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00062900
063000                 20  FILLER          PIC X(1)  VALUE  '-'.        00063000
063100                 20  FILLER          PIC X(3)  VALUE  '004'.      00063100
063200                 20  FILLER          PIC X(1)  VALUE  ' '.        00063200
063300                 20  FILLER          PIC X(70) VALUE              00063300
063400                     '#IDGD HAS BEEN SUCCESSFULLY MAPPED          00063400
063500-                    '                         '.                 00063500
063600             15  FILLER              PIC X(2)  VALUE '<¬'.        00063600
063700*----------------------------------------------------------------*00063700
063800         10  WT-01-ENTRY-005.                                     00063800
063900             15  FILLER              PIC X(2)  VALUE '¬>'.        00063900
064000             15  WT-01-MESSAGE-TEXT-005.                          00064000
064100                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00064100
064200                 20  FILLER          PIC X(1)  VALUE  '-'.        00064200
064300                 20  FILLER          PIC X(3)  VALUE  '005'.      00064300
064400                 20  FILLER          PIC X(1)  VALUE  ' '.        00064400
064500                 20  FILLER          PIC X(70) VALUE              00064500
064600                     '#IPGP HAS BEEN SUCCESSFULLY MAPPED          00064600
064700-                    '                         '.                 00064700
064800             15  FILLER              PIC X(2)  VALUE '<¬'.        00064800
064900*----------------------------------------------------------------*00064900
065000         10  WT-01-ENTRY-006.                                     00065000
065100             15  FILLER              PIC X(2)  VALUE '¬>'.        00065100
065200             15  WT-01-MESSAGE-TEXT-006.                          00065200
065300                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00065300
065400                 20  FILLER          PIC X(1)  VALUE  '-'.        00065400
065500                 20  FILLER          PIC X(3)  VALUE  '006'.      00065500
065600                 20  FILLER          PIC X(1)  VALUE  ' '.        00065600
065700                 20  FILLER          PIC X(70) VALUE              00065700
065800                     'DELETE OPTION MUST BE \
065900-                    'VALID                    '.                 00065900
066000             15  FILLER              PIC X(2)  VALUE '<¬'.        00066000
066100*----------------------------------------------------------------*00066100
066200         10  WT-01-ENTRY-007.                                     00066200
066300             15  FILLER              PIC X(2)  VALUE '¬>'.        00066300
066400             15  WT-01-MESSAGE-TEXT-007.                          00066400
066500                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00066500
066600                 20  FILLER          PIC X(1)  VALUE  '-'.        00066600
066700                 20  FILLER          PIC X(3)  VALUE  '007'.      00066700
066800                 20  FILLER          PIC X(1)  VALUE  ' '.        00066800
066900                 20  FILLER          PIC X(70) VALUE              00066900
067000                     'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESS00067000
067100-                    ' PF4/PF16 TO CONTINUE    '.                 00067100
067200             15  FILLER              PIC X(2)  VALUE '<¬'.        00067200
067300*----------------------------------------------------------------*00067300
067400         10  WT-01-ENTRY-008.                                     00067400
067500             15  FILLER              PIC X(2)  VALUE '¬>'.        00067500
067600             15  WT-01-MESSAGE-TEXT-008.                          00067600
067700                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00067700
067800                 20  FILLER          PIC X(1)  VALUE  '-'.        00067800
067900                 20  FILLER          PIC X(3)  VALUE  '008'.      00067900
068000                 20  FILLER          PIC X(1)  VALUE  ' '.        00068000
068100                 20  FILLER          PIC X(70) VALUE              00068100
068200                     'GROUP IN CONVERSION STATUS, CANNOT CHANGE HI00068200
068300-                    'GH-LIGHTED ELEMENTS      '.                 00068300
068400             15  FILLER              PIC X(2)  VALUE '<¬'.        00068400
068500*----------------------------------------------------------------*00068500
068600         10  WT-01-ENTRY-009.                                     00068600
068700             15  FILLER              PIC X(2)  VALUE '¬>'.        00068700
068800             15  WT-01-MESSAGE-TEXT-009.                          00068800
068900                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00068900
069000                 20  FILLER          PIC X(1)  VALUE  '-'.        00069000
069100                 20  FILLER          PIC X(3)  VALUE  '009'.      00069100
069200                 20  FILLER          PIC X(1)  VALUE  ' '.        00069200
069300                 20  FILLER          PIC X(70) VALUE              00069300
069400                     'INVALID PFKEY SELECTION                     00069400
069500-                    '                         '.                 00069500
069600             15  FILLER              PIC X(2)  VALUE '<¬'.        00069600
069700*----------------------------------------------------------------*00069700
069800         10  WT-01-ENTRY-010.                                     00069800
069900             15  FILLER              PIC X(2)  VALUE '¬>'.        00069900
070000             15  WT-01-MESSAGE-TEXT-010.                          00070000
070100                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00070100
070200                 20  FILLER          PIC X(1)  VALUE  '-'.        00070200
070300                 20  FILLER          PIC X(3)  VALUE  '010'.      00070300
070400                 20  FILLER          PIC X(1)  VALUE  ' '.        00070400
070500                 20  FILLER          PIC X(70) VALUE              00070500
070600                     'INVALID REQUEST.  THAT PF KEY HAS NO MEANING00070600
070700-                    ' TO THIS PROGRAM         '.                 00070700
070800             15  FILLER              PIC X(2)  VALUE '<¬'.        00070800
070900*----------------------------------------------------------------*00070900
071000         10  WT-01-ENTRY-011.                                     00071000
071100             15  FILLER              PIC X(2)  VALUE '¬>'.        00071100
071200             15  WT-01-MESSAGE-TEXT-011.                          00071200
071300                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00071300
071400                 20  FILLER          PIC X(1)  VALUE  '-'.        00071400
071500                 20  FILLER          PIC X(3)  VALUE  '011'.      00071500
071600                 20  FILLER          PIC X(1)  VALUE  ' '.        00071600
071700                 20  FILLER          PIC X(70) VALUE              00071700
071800                     'NO CHANGE FOUND - NO CHANGE MADE, WHAT NEXT 00071800
071900-                    '                         '.                 00071900
072000             15  FILLER              PIC X(2)  VALUE '<¬'.        00072000
072100*----------------------------------------------------------------*00072100
072200         10  WT-01-ENTRY-012.                                     00072200
072300             15  FILLER              PIC X(2)  VALUE '¬>'.        00072300
072400             15  WT-01-MESSAGE-TEXT-012.                          00072400
072500                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00072500
072600                 20  FILLER          PIC X(1)  VALUE  '-'.        00072600
072700                 20  FILLER          PIC X(3)  VALUE  '012'.      00072700
072800                 20  FILLER          PIC X(1)  VALUE  ' '.        00072800
072900                 20  FILLER          PIC X(70) VALUE              00072900
073000                     'NO ENTRIES TO DISPLAY                       00073000
073100-                    '                         '.                 00073100
073200             15  FILLER              PIC X(2)  VALUE '<¬'.        00073200
073300*----------------------------------------------------------------*00073300
073400         10  WT-01-ENTRY-013.                                     00073400
073500             15  FILLER              PIC X(2)  VALUE '¬>'.        00073500
073600             15  WT-01-MESSAGE-TEXT-013.                          00073600
073700                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00073700
073800                 20  FILLER          PIC X(1)  VALUE  '-'.        00073800
073900                 20  FILLER          PIC X(3)  VALUE  '013'.      00073900
074000                 20  FILLER          PIC X(1)  VALUE  ' '.        00074000
074100                 20  FILLER          PIC X(70) VALUE              00074100
074200                     'PFKEY INVALID WHILE ERRORS NOT CORRECTED, HI00074200
074300-                    'T ENTER FOR ERR MSG      '.                 00074300
074400             15  FILLER              PIC X(2)  VALUE '<¬'.        00074400
074500*----------------------------------------------------------------*00074500
074600         10  WT-01-ENTRY-014.                                     00074600
074700             15  FILLER              PIC X(2)  VALUE '¬>'.        00074700
074800             15  WT-01-MESSAGE-TEXT-014.                          00074800
074900                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00074900
075000                 20  FILLER          PIC X(1)  VALUE  '-'.        00075000
075100                 20  FILLER          PIC X(3)  VALUE  '014'.      00075100
075200                 20  FILLER          PIC X(1)  VALUE  ' '.        00075200
075300                 20  FILLER          PIC X(70) VALUE              00075300
075400                     'PROCESSING FROM THE TOP OF THE LIST         00075400
075500-                    '                         '.                 00075500
075600             15  FILLER              PIC X(2)  VALUE '<¬'.        00075600
075700*----------------------------------------------------------------*00075700
075800         10  WT-01-ENTRY-015.                                     00075800
075900             15  FILLER              PIC X(2)  VALUE '¬>'.        00075900
076000             15  WT-01-MESSAGE-TEXT-015.                          00076000
076100                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00076100
076200                 20  FILLER          PIC X(1)  VALUE  '-'.        00076200
076300                 20  FILLER          PIC X(3)  VALUE  '015'.      00076300
076400                 20  FILLER          PIC X(1)  VALUE  ' '.        00076400
076500                 20  FILLER          PIC X(70) VALUE              00076500
076600                     'THE MAXIMUM NUMBER OF ENTRIES HAVE BEEN ADDE00076600
076700-                    'D                        '.                 00076700
076800             15  FILLER              PIC X(2)  VALUE '<¬'.        00076800
076900*----------------------------------------------------------------*00076900
077000         10  WT-01-ENTRY-016.                                     00077000
077100             15  FILLER              PIC X(2)  VALUE '¬>'.        00077100
077200             15  WT-01-MESSAGE-TEXT-016.                          00077200
077300                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00077300
077400                 20  FILLER          PIC X(1)  VALUE  '-'.        00077400
077500                 20  FILLER          PIC X(3)  VALUE  '016'.      00077500
077600                 20  FILLER          PIC X(1)  VALUE  ' '.        00077600
077700                 20  FILLER          PIC X(70) VALUE              00077700
077800                     'THE TABULAR ALREADY CONTAINS THE MAXIMUM NUM00077800
077900-                    'BER OF OCCURANCES        '.                 00077900
078000             15  FILLER              PIC X(2)  VALUE '<¬'.        00078000
078100*----------------------------------------------------------------*00078100
078200         10  WT-01-ENTRY-017.                                     00078200
078300             15  FILLER              PIC X(2)  VALUE '¬>'.        00078300
078400             15  WT-01-MESSAGE-TEXT-017.                          00078400
078500                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00078500
078600                 20  FILLER          PIC X(1)  VALUE  '-'.        00078600
078700                 20  FILLER          PIC X(3)  VALUE  '017'.      00078700
078800                 20  FILLER          PIC X(1)  VALUE  ' '.        00078800
078900                 20  FILLER          PIC X(70) VALUE              00078900
079000                     'THE TABULAR RECORD DOES NOT EXIST, AND CANNO00079000
079100-                    'T BE CHANGED             '.                 00079100
079200             15  FILLER              PIC X(2)  VALUE '<¬'.        00079200
079300*----------------------------------------------------------------*00079300
079400         10  WT-01-ENTRY-018.                                     00079400
079500             15  FILLER              PIC X(2)  VALUE '¬>'.        00079500
079600             15  WT-01-MESSAGE-TEXT-018.                          00079600
079700                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00079700
079800                 20  FILLER          PIC X(1)  VALUE  '-'.        00079800
079900                 20  FILLER          PIC X(3)  VALUE  '018'.      00079900
080000                 20  FILLER          PIC X(1)  VALUE  ' '.        00080000
080100                 20  FILLER          PIC X(70) VALUE              00080100
080200                     'THE TABULAR RECORD DOES NOT EXIST, AND CANNO00080200
080300-                    'T BE MAPPED              '.                 00080300
080400             15  FILLER              PIC X(2)  VALUE '<¬'.        00080400
080500*----------------------------------------------------------------*00080500
080600         10  WT-01-ENTRY-019.                                     00080600
080700             15  FILLER              PIC X(2)  VALUE '¬>'.        00080700
080800             15  WT-01-MESSAGE-TEXT-019.                          00080800
080900                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00080900
081000                 20  FILLER          PIC X(1)  VALUE  '-'.        00081000
081100                 20  FILLER          PIC X(3)  VALUE  '019'.      00081100
081200                 20  FILLER          PIC X(1)  VALUE  ' '.        00081200
081300                 20  FILLER          PIC X(70) VALUE              00081300
081400                     'THERE ARE NO MORE ENTRIES TO DISPLAY        00081400
081500-                    '                         '.                 00081500
081600             15  FILLER              PIC X(2)  VALUE '<¬'.        00081600
081700*----------------------------------------------------------------*00081700
081800         10  WT-01-ENTRY-020.                                     00081800
081900             15  FILLER              PIC X(2)  VALUE '¬>'.        00081900
082000             15  WT-01-MESSAGE-TEXT-020.                          00082000
082100                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00082100
082200                 20  FILLER          PIC X(1)  VALUE  '-'.        00082200
082300                 20  FILLER          PIC X(3)  VALUE  '020'.      00082300
082400                 20  FILLER          PIC X(1)  VALUE  ' '.        00082400
082500                 20  FILLER          PIC X(70) VALUE              00082500
082600                     'THIS IS THE FIRST ON THE TABLE              00082600
082700-                    '                         '.                 00082700
082800             15  FILLER              PIC X(2)  VALUE '<¬'.        00082800
082900*----------------------------------------------------------------*00082900
083000         10  WT-01-ENTRY-021.                                     00083000
083100             15  FILLER              PIC X(2)  VALUE '¬>'.        00083100
083200             15  WT-01-MESSAGE-TEXT-021.                          00083200
083300                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00083300
083400                 20  FILLER          PIC X(1)  VALUE  '-'.        00083400
083500                 20  FILLER          PIC X(3)  VALUE  '021'.      00083500
083600                 20  FILLER          PIC X(1)  VALUE  ' '.        00083600
083700                 20  FILLER          PIC X(70) VALUE              00083700
083800                     'THIS IS THE LAST ON THE TABLE               00083800
083900-                    '                         '.                 00083900
084000             15  FILLER              PIC X(2)  VALUE '<¬'.        00084000
084100*----------------------------------------------------------------*00084100
084200         10  WT-01-ENTRY-022.                                     00084200
084300             15  FILLER              PIC X(2)  VALUE '¬>'.        00084300
084400             15  WT-01-MESSAGE-TEXT-022.                          00084400
084500                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00084500
084600                 20  FILLER          PIC X(1)  VALUE  '-'.        00084600
084700                 20  FILLER          PIC X(3)  VALUE  '022'.      00084700
084800                 20  FILLER          PIC X(1)  VALUE  ' '.        00084800
084900                 20  FILLER          PIC X(70) VALUE              00084900
085000                     'THIS PFKEY NOT VALID WHILE IN CHG/ADD MODE  00085000
085100-                    '                         '.                 00085100
085200             15  FILLER              PIC X(2)  VALUE '<¬'.        00085200
085300*----------------------------------------------------------------*00085300
085400         10  WT-01-ENTRY-023.                                     00085400
085500             15  FILLER              PIC X(2)  VALUE '¬>'.        00085500
085600             15  WT-01-MESSAGE-TEXT-023.                          00085600
085700                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00085700
085800                 20  FILLER          PIC X(1)  VALUE  '-'.        00085800
085900                 20  FILLER          PIC X(3)  VALUE  '023'.      00085900
086000                 20  FILLER          PIC X(1)  VALUE  ' '.        00086000
086100                 20  FILLER          PIC X(70) VALUE              00086100
086200                     'INVALID INTERNAL TABULAR ACTION CODE        00086200
086300-                    '                         '.                 00086300
086400             15  FILLER              PIC X(2)  VALUE '<¬'.        00086400
086500*----------------------------------------------------------------*00086500
086600         10  WT-01-ENTRY-024.                                     00086600
086700             15  FILLER              PIC X(2)  VALUE '¬>'.        00086700
086800             15  WT-01-MESSAGE-TEXT-003.                          00086800
086900                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00086900
087000                 20  FILLER          PIC X(1)  VALUE  '-'.        00087000
087100                 20  FILLER          PIC X(3)  VALUE  '024'.      00087100
087200                 20  FILLER          PIC X(1)  VALUE  ' '.        00087200
087300                 20  FILLER          PIC X(70) VALUE              00087300
087400                     '#IPGS HAS BEEN SUCCESSFULLY MAPPED          00087400
087500-                    '                         '.                 00087500
087600             15  FILLER              PIC X(2)  VALUE '<¬'.        00087600
087700*----------------------------------------------------------------*00087700
087800         10  WT-01-ENTRY-025.                                     00087800
087900             15  FILLER              PIC X(2)  VALUE '¬>'.        00087900
088000             15  WT-01-MESSAGE-TEXT-024.                          00088000
088100                 20  FILLER          PIC X(4)  VALUE  'GA1B'.     00088100
088200                 20  FILLER          PIC X(1)  VALUE  '-'.        00088200
088300                 20  FILLER          PIC X(3)  VALUE  '025'.      00088300
088400                 20  FILLER          PIC X(1)  VALUE  ' '.        00088400
088500                 20  FILLER          PIC X(70) VALUE              00088500
088600                     '********** F U T U R E   U S E *************00088600
088700-                    '*************************'.                 00088700
088800             15  FILLER              PIC X(2)  VALUE '<¬'.        00088800
088900*----------------------------------------------------------------*00088900
089000                                                                  00089000
089100     05  WT-01-MESSAGE-TABLE         REDEFINES                    00089100
089200         WT-01-MESSAGE-VALUES         OCCURS 025 TIMES            00089200
089300                                     INDEXED BY WT-01-INDEX.      00089300
089400         10  WT-01-ENTRY.                                         00089400
089500             15  FILLER              PIC X(02).                   00089500
089600             15  WT-01-MESSAGE-TEXT  PIC X(79).                   00089600
089700             15  FILLER              PIC X(02).                   00089700
089800                                                                  00089800
089900 01  WS-ACTION-CODE-TEST-AREA      PIC X(02)  VALUE SPACES.       00089900
090000     88  VALID-ACTION-CODE         VALUE 'A ' 'C ' 'D ' 'MT'      00090000
090100                                         SPACES.                  00090100
090200                                                                  00090200
090300                                                                  00090300
090400                                                                  00090400
090500 01  WS-END                      PIC X(16)  VALUE                 00090500
090600     '*** W/S ENDS ***'.                                          00090600
090700/    L I N K A G E   S E C T I O N                                00090700
090800 LINKAGE SECTION.                                                 00090800
090900 01  DFHCOMMAREA.                                                 00090900
091000 COPY G2ALCKEC.                                                   00091000
091100*    05  COMMAREA-RECORD-POINTER   USAGE IS POINTER.              00091100
091200 COPY GACDACWB.                                                   00091200
091300                                                                  00091300
091400     05  GAS1UPD-PASSED-AREA-2.                                   00091400
091500         07  LVL2-B-SW-2              PIC X.                      00091500
091600         07  LVL2-F-SW-2              PIC X.                      00091600
091700         07  LVL2-G-SW-2              PIC X.                      00091700
091800         07  INTR-TAB-PGM-ID-2        PIC X(8).                   00091800
091900         07  FILLER-2                 PIC X(09).                  00091900
092000     05  DELADD-OPTION-2              PIC X(7).                   00092000
092100                                                                  00092100
092200/*****************************************************************00092200
092300* W O R K F I L E   -   A L L   L E V E L   T A B   R E C O R D   00092300
092400******************************************************************00092400
092500 01  WF-IO-PARM-ALL-LVL-TAB-RECORD.                               00092500
092600 COPY GCIOPRM1.                                                   00092600
092700/                                                                 00092700
092800 COPY GCWRKDCC.                                                   00092800
092900/                                                                 00092900
093000 COPY GCTABMC.                                                    00093000
093100/    C O M M U N I C A T I O N    K E Y   A R E A                 00093100
093200*01  COMMUNICATION-KEY-AREA.                                      00093200
093300*COPY G2ALCKEC.                                                   00093300
093400                                                                  00093400
093500/    C O P Y   T A B U L A R   T A B L E   A R E A                00093500
093600 01  COPY-TABULAR-TABLE-AREA.                                     00093600
SI0724*    05  COPY-TABULAR-TABLE  OCCURS  44 TIMES INDEXED BY          00093700
SI0724     05  COPY-TABULAR-TABLE  OCCURS 175 TIMES INDEXED BY          00093710
093800         COPY-IDX, COPY-IDX2, COPY-IDX3, COPY-IDX4.               00093800
093900       10  COPY-SORTABLE-FLDS.                                    00093900
094000           15  FILLER                      PIC X(169).            00094000
094100           15  COPY-SORT-FYI               PIC X(003).            00094100
094200       10  COPY-SORT-ENTRY-CNTR            PIC S9(7) COMP-3.      00094200
094300                                                                  00094300
094400/*****************************************************************00094400
094500* W O R K F I L E   -   I N T E R N A L   T A B U L A R   R E C   00094500
094600******************************************************************00094600
094700 01  WF-IO-PARM-INTERNAL-TAB-RECORD.                              00094700
094800 COPY GCIOPRM2.                                                   00094800
094900/                                                                 00094900
095000 COPY GCWRKDC2.                                                   00095000
095100 COPY GCTIPGPC.                                                   00095100
095200/*****************************************************************00095200
095300* W O R K F I L E   -   G R O U P   S P E C I F I C   R E C       00095300
095400******************************************************************00095400
095500 01  WF-IO-PARM-WRK-GRP-SPEC-REC.                                 00095500
095600 COPY GCIOPRM3.                                                   00095600
095700/                                                                 00095700
095800 COPY GCWRKDC3.                                                   00095800
095900/                                                                 00095900
096000 COPY GCGROUPC.                                                   00096000
096100/*****************************************************************00096100
096200* W O R K F I L E   -   C O N T R A C T   R E C O R D             00096200
096300******************************************************************00096300
096400 01  WF-IO-PARM-WRK-CONTRACT-REC.                                 00096400
096500 COPY GCIOPRM4.                                                   00096500
096600/                                                                 00096600
096700 COPY GCWRKDC4.                                                   00096700
096800/                                                                 00096800
096900 COPY GCCONTRC.                                                   00096900
097000/*****************************************************************00097000
097100* W O R K F I L E   -   B E N E F I T   P R O V I S I O N   R E C 00097100
097200******************************************************************00097200
097300 01  WF-IO-PARM-WRK-BEN-PROV-REC.                                 00097300
097400 COPY GCIOPRM5.                                                   00097400
097500/                                                                 00097500
097600 COPY GCWRKDC5.                                                   00097600
097700/                                                                 00097700
097800 COPY GCBENPVC.                                                   00097800
097900/*****************************************************************00097900
098000* W O R K F I L E   -  C O N T R O L   R E C O R D                00098000
098100******************************************************************00098100
098200 01  WF-IO-PARM-WRK-CONTROL-REC.                                  00098200
098300 COPY GCIOPRM6.                                                   00098300
098400/                                                                 00098400
098500 COPY GCWRKDC6.                                                   00098500
098600/                                                                 00098600
098700 COPY GCCCRDCC.                                                   00098700
098800/*****************************************************************00098800
098900* P R O D U C T I O N   -   C O N T R A C T   R E C O R D         00098900
099000******************************************************************00099000
099100 01  PR-IO-PARM-WRK-CONTRACT-REC.                                 00099100
099200 COPY GCIOPRM7   SUPPRESS.                                        00099200
099300                                                                  00099300
099400 COPY GCWRKDC7   SUPPRESS.                                        00099400
099500                                                                  00099500
099600 COPY GCCONTR2   SUPPRESS.                                        00099600
099700******************************************************************00099700
099800* P R O D U C T I O N   -   G R O U P   S P E C I F I C   R E C   00099800
099900******************************************************************00099900
100000 01  PR-IO-PARM-WRK-GRP-SPEC-REC.                                 00100000
100100 COPY GCIOPRM8   SUPPRESS.                                        00100100
100200                                                                  00100200
100300 COPY GCWRKDC8   SUPPRESS.                                        00100300
100400                                                                  00100400
100500 COPY GCGROUP2   SUPPRESS.                                        00100500
100600******************************************************************00100600
100700* P R O D U C T I O N   -   B E N E F I T   P V S N   R E C O R D 00100700
100800******************************************************************00100800
100900 01  PR-IO-PARM-WRK-BEN-PROV-REC.                                 00100900
101000 COPY GCIOPRM9   SUPPRESS.                                        00101000
101100                                                                  00101100
101200 COPY GCWRKDC9   SUPPRESS.                                        00101200
101300                                                                  00101300
101400 COPY GCBENPV2   SUPPRESS.                                        00101400
101500******************************************************************00101500
101600* P R O D U C T I O N   -   A L L   L E V E L   T A B   R E C     00101600
101700******************************************************************00101700
101800 01  PR-IO-PARM-ALL-LVL-TAB-RECORD.                               00101800
101900 COPY GCIOPRMA   SUPPRESS.                                        00101900
102000                                                                  00102000
102100 COPY GCWRKDCA   SUPPRESS.                                        00102100
102200                                                                  00102200
102300 COPY GCTABM2    SUPPRESS.                                        00102300
102400                                                                  00102400
102500/*****************************************************************00102500
102600*    M A P S E T   A R E A                                        00102600
102700******************************************************************00102700
102800     COPY GA1XSETC.                                               00102800
102900                                                                  00102900
103000/*****************************************************************00103000
103100*     A L L   L V L   A C C U M   C O M M O N   W O R K A R E A S 00103100
103200*  *** UPDATE/DELETE MODULE GAS1UPD COMMAREA ***                  00103200
103300*  *** WILL BE THE SAME COMMON WORK AREA + GAS1UPD COMMAREA ***   00103300
103400******************************************************************00103400
103500 01  COMMON-WORKAREAS.                                            00103500
103600 COPY G2ALCKE2.                                                   00103600
103700 COPY GACDACWA.                                                   00103700
103800                                                                  00103800
103900     05  GAS1UPD-PASSED-AREA.                                     00103900
104000         07  LVL2-B-SW                PIC X.                      00104000
104100         07  LVL2-F-SW                PIC X.                      00104100
104200         07  LVL2-G-SW                PIC X.                      00104200
104300         07  INTR-TAB-PGM-ID          PIC X(8).                   00104300
104400         07  FILLER                   PIC X(09).                  00104400
104500     05  DELADD-OPTION                PIC X(7).                   00104500
104600                                                                  00104600
104700/    P R O C E D U R E   D I V I S I O N                          00104700
104800 PROCEDURE DIVISION.                                              00104800
104900                                                                  00104900
105000******************************************************************00105000
105100* 0000  HOUSEKEEPING                                             *00105100
105200******************************************************************00105200
105300 0000-000-HOUSEKEEPING          SECTION.                          00105300
105400 0000-010.                                                        00105400
105500                                                                  00105500
105600     EXEC CICS GETMAIN                                            00105600
105700               SET(ADDRESS OF COMMON-WORKAREAS)                   00105700
105800               INITIMG(WS-HEX-00)                                 00105800
105900               LENGTH(LENGTH OF COMMON-WORKAREAS)                 00105900
106000               END-EXEC.                                          00106000
106100                                                                  00106100
106200     MOVE ZEROES  TO  ACWA-CDE-1U-COUNT,  ACWA-CDE-2B-COUNT.      00106200
106300                                                                  00106300
106400     EXEC CICS GETMAIN                                            00106400
106500               SET(ADDRESS OF GA1XI01I)                           00106500
106600               INITIMG(WS-HEX-00)                                 00106600
106700               LENGTH(LENGTH OF GA1XI01I)                         00106700
106800               END-EXEC.                                          00106800
106900                                                                  00106900
107000     SET ACWA-MAPSET-PNTR  TO  ADDRESS OF  GA1XI01I.              00107000
107100                                                                  00107100
107200                                                                  00107200
107300     MOVE  +19   TO  GCVI-COMMAREA-LEN.                           00107300
107400     MOVE  'N'   TO  ACWA-ERROR-SW                                00107400
107500                     ACWA-CDE-FIELD-CHANGE-IND                    00107500
107600                     ACWA-CDE-REC-CHANGE-IND                      00107600
107700                     ACWA-CDE-RESET-WF-IND.                       00107700
107800     MOVE  ZERO  TO  ACWA-FIELD-CHG-CNT.                          00107800
107900     MOVE  SPACE TO  ACWA-CDE-STATUS-CHANGE-IND                   00107900
108000                     ACWA-CDE-INTERNAL-TAB-IND.                   00108000
108100                                                                  00108100
108200     COMPUTE WS-IO-PARM-WRK-GRP-SPEC-LEN =                        00108200
108300             GC-GCIOPARM-LEN             +                        00108300
108400             GC-WORKFILE-KEY-LEN         +                        00108400
108500             GC-GCGRPSPC-FIXED-LEN       +                        00108500
108600            (GC-GCGRPSPC-VARY-LEN        *                        00108600
108700             GC-GCGRPSPC-VARY-MAX-OCUR).                          00108700
108800                                                                  00108800
108900     COMPUTE WS-WRK-GRP-SPEC-LEN         =                        00108900
109000             GC-WORKFILE-KEY-LEN         +                        00109000
109100             GC-GCGRPSPC-FIXED-LEN       +                        00109100
109200            (GC-GCGRPSPC-VARY-LEN        *                        00109200
109300             GC-GCGRPSPC-VARY-MAX-OCUR).                          00109300
109400                                                                  00109400
109500     COMPUTE WS-IO-PARM-WRK-CONTRACT-LEN =                        00109500
109600             GC-GCIOPARM-LEN             +                        00109600
109700             GC-WORKFILE-KEY-LEN         +                        00109700
109800             GC-GCCONTR-FIXED-LEN        +                        00109800
109900            (GC-GCCONTR-VARY-LEN         *                        00109900
110000             GC-GCCONTR-VARY-MAX-OCUR).                           00110000
110100                                                                  00110100
110200     COMPUTE WS-WRK-CONTRACT-LEN         =                        00110200
110300             GC-WORKFILE-KEY-LEN         +                        00110300
110400             GC-GCCONTR-FIXED-LEN        +                        00110400
110500            (GC-GCCONTR-VARY-LEN         *                        00110500
110600             GC-GCCONTR-VARY-MAX-OCUR).                           00110600
110700                                                                  00110700
110800     COMPUTE WS-IO-PARM-WRK-BEN-PROV-LEN =                        00110800
110900             GC-GCIOPARM-LEN             +                        00110900
111000             GC-WORKFILE-KEY-LEN         +                        00111000
111100             GC-GCBENPRV-FIXED-LEN       +                        00111100
111200            (GC-GCBENPRV-VARY-LEN        *                        00111200
111300             GC-GCBENPRV-VARY-MAX-OCUR).                          00111300
111400                                                                  00111400
111500     COMPUTE WS-WRK-BEN-PROV-LEN         =                        00111500
111600             GC-WORKFILE-KEY-LEN         +                        00111600
111700             GC-GCBENPRV-FIXED-LEN       +                        00111700
111800            (GC-GCBENPRV-VARY-LEN        *                        00111800
111900             GC-GCBENPRV-VARY-MAX-OCUR).                          00111900
112000                                                                  00112000
112100     COMPUTE WS-IO-PARM-WRK-CONTROL-LEN  =                        00112100
112200             GC-GCIOPARM-LEN             +                        00112200
112300             GC-WORKFILE-KEY-LEN         +                        00112300
112400             GC-WORKFILE-CONTROL-REC-LEN.                         00112400
112500                                                                  00112500
112600     IF EIBAID  =  DFHCLEAR                                       00112600
112700         EXEC CICS SEND FROM(WS-ONE-LOW)                          00112700
112800                        ERASE                                     00112800
112900         END-EXEC                                                 00112900
113000         EXEC CICS RETURN                                         00113000
113100         END-EXEC.                                                00113100
113200                                                                  00113200
113300     EXEC CICS  HANDLE  CONDITION                                 00113300
113400                MAPFAIL(6400-000-XCTL-TO-MAIN-MENU)  END-EXEC.    00113400
113500                                                                  00113500
113600 0000-900-EXIT.                                                   00113600
113700         EXIT.                                                    00113700
113800/*****************************************************************00113800
113900* 1000  MAIN LINE                                                *00113900
114000******************************************************************00114000
114100 1000-000-MAIN-LINE             SECTION.                          00114100
114200 1000-010.                                                        00114200
114300                                                                  00114300
114400     IF  EIBTRNID  NOT =  'GA1B'                                  00114400
114500         PERFORM 4000-000-DISPLAY-FIRST-SCREEN.                   00114500
114600                                                                  00114600
114700     EXEC CICS  RECEIVE   MAP('GA1XI01')  MAPSET('GA1XSET')       00114700
114800                END-EXEC.                                         00114800
114900                                                                  00114900
115000     IF  SCRNIDNI  NOT =  '001B00'                                00115000
115100         PERFORM 6400-000-XCTL-TO-MAIN-MENU.                      00115100
115200                                                                  00115200
115300     IF  FRMNUIDI = 'GS3A'                                        00115300
115400         MOVE IDLINEI   TO   GROUP-SPECIFIC-ID-LINE.              00115400
115500     IF  FRMNUIDI = 'GC4A' OR 'GTM1'                              00115500
115600         MOVE IDLINEI   TO   CONTRACT-ID-LINE.                    00115600
115700     IF  FRMNUIDI = 'GC8A'                                        00115700
115800         MOVE IDLINEI   TO   BENEFIT-PROVISION-ID-LINE.           00115800
115900                                                                  00115900
116000     IF EIBAID = DFHPF1 OR DFHPF13                                00116000
116100        PERFORM 8000-000-SWITCH-ADD-DEL-MODE.                     00116100
116200                                                                  00116200
116300     IF EIBAID = DFHPF3 OR DFHPF15                                00116300
116400        PERFORM 5000-000-XCTL-TO-PREVIOUS-MENU.                   00116400
116500                                                                  00116500
116600     IF (EIBAID  =  DFHPF7 OR  DFHPF19 OR  DFHPF8 OR  DFHPF20) AND00116600
116700        DELADDI  =  'CHG/ADD'                                     00116700
116800     THEN                                                         00116800
116900         SET  WT-01-INDEX                     TO +22              00116900
117000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00117000
117100         MOVE -1                              TO  PERIODL         00117100
117200         GO TO 1000-900-EXIT.                                     00117200
117300                                                                  00117300
117400                                                                  00117400
117500     IF (EIBAID  =  DFHPF7 OR  DFHPF19 OR  DFHPF8 OR  DFHPF20) AND00117500
117600        DELOPTNI  =  'D'                                          00117600
117700     THEN                                                         00117700
117800         SET  WT-01-INDEX                     TO +06              00117800
117900         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00117900
118000         MOVE -1                              TO  DELOPTNL        00118000
118100         GO TO 1000-900-EXIT.                                     00118100
118200                                                                  00118200
118300     PERFORM 1100-000-VALIDATE-SCREEN.                            00118300
118400                                                                  00118400
118500     IF  EIBAID  = DFHPF4 OR DFHPF16  AND                         00118500
118600         ACWA-SCREEN-HAS-ERRORS                                   00118600
118700     THEN                                                         00118700
118800         SET  WT-01-INDEX                     TO +13              00118800
118900         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00118900
119000         GO TO 1000-900-EXIT.                                     00119000
119100                                                                  00119100
119200     IF  EIBAID  = DFHPF4 OR DFHPF16                              00119200
119300     THEN                                                         00119300
119400         IF  ACWA-SCREEN-HAS-NO-ERRORS                            00119400
119500         THEN                                                     00119500
119600             IF  GCVI-TABLE-SW = 'N'                              00119600
119700             THEN                                                 00119700
119800                 PERFORM 2000-000-PROCESS-REQUEST                 00119800
119900                 GO TO  1000-990-RETURN                           00119900
120000             ELSE                                                 00120000
120100                 SET  WT-01-INDEX                     TO +09      00120100
120200                 MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO  00120200
120300                 MOVE -1                              TO PERIODL  00120300
120400                 GO TO 1000-900-EXIT                              00120400
120500         ELSE                                                     00120500
120600             NEXT SENTENCE                                        00120600
120700     ELSE                                                         00120700
120800         NEXT SENTENCE.                                           00120800
120900                                                                  00120900
121000     PERFORM 1001-000-VALIDATE-ACTION-CODES.                      00121000
121100                                                                  00121100
121200     IF  ACWA-SCREEN-HAS-ERRORS                                   00121200
121300         GO TO 1000-900-EXIT.                                     00121300
121400                                                                  00121400
121500     IF  EIBAID  =  DFHENTER OR                                   00121500
121600                    DFHPF7   OR  DFHPF19 OR   DFHPF8 OR  DFHPF20  00121600
121700     THEN                                                         00121700
121800         PERFORM 2000-000-PROCESS-REQUEST                         00121800
121900                 GO TO  1000-990-RETURN.                          00121900
122000                                                                  00122000
122100     PERFORM 7900-000-RESET-ATTRIBUTES.                           00122100
122200     MOVE -1                              TO PERIODL.             00122200
122300     SET  WT-01-INDEX                     TO +10                  00122300
122400     MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.             00122400
122500                                                                  00122500
122600                                                                  00122600
122700 1000-900-EXIT.                                                   00122700
122800                                                                  00122800
122900     PERFORM 3100-000-READ-RECORD.                                00122900
123000     MOVE GAA-ENTRY-COUNT  TO  GAA-ENTRY-COUNT.                   00123000
123100     PERFORM 9010-000-SEND-DATAONLY-RETURN.                       00123100
123200 1000-990-RETURN.                                                 00123200
123300*    EXEC CICS  RETURN  END-EXEC.                                 00123300
123400     IF DELADD-OPTION = 'GAS1UPD'                                 00123400
123500         IF INTR-TAB-PGM-ID = 'GA1GPGM'                           00123500
123600             MOVE SPACES TO DELADD-OPTION                         00123600
123700             EXEC CICS RETURN TRANSID('GA1G')                     00123700
123800                       COMMAREA(COMMON-WORKAREAS)                 00123800
123900                       END-EXEC                                   00123900
124000         ELSE                                                     00124000
124100         IF INTR-TAB-PGM-ID = 'GA2GPGM'                           00124100
124200             MOVE SPACES TO DELADD-OPTION                         00124200
124300             EXEC CICS RETURN TRANSID('GA2G')                     00124300
124400                       COMMAREA(COMMON-WORKAREAS)                 00124400
124500                       END-EXEC                                   00124500
124600         ELSE                                                     00124600
124700         IF INTR-TAB-PGM-ID = 'GA1HPGM'                           00124700
124800             MOVE SPACES TO DELADD-OPTION                         00124800
124900             EXEC CICS  RETURN TRANSID('GA1H')                    00124900
125000                        COMMAREA(COMMON-WORKAREAS)                00125000
125100                        END-EXEC                                  00125100
125200         ELSE                                                     00125200
125300         IF INTR-TAB-PGM-ID = 'GA2HPGM'                           00125300
125400             MOVE SPACES TO DELADD-OPTION                         00125400
125500             EXEC CICS  RETURN TRANSID('GA2H')                    00125500
125600                        COMMAREA(COMMON-WORKAREAS)                00125600
125700                        END-EXEC                                  00125700
125800         ELSE                                                     00125800
125900         IF INTR-TAB-PGM-ID = 'GA1IPGM'                           00125900
126000             MOVE SPACES TO DELADD-OPTION                         00126000
126100             EXEC CICS  RETURN TRANSID('GA1I')                    00126100
126200                        COMMAREA(COMMON-WORKAREAS)                00126200
126300                        END-EXEC                                  00126300
126400         ELSE                                                     00126400
126500         IF INTR-TAB-PGM-ID = 'GA1SPGM'                           00126500
126600             MOVE SPACES TO DELADD-OPTION                         00126600
126700             EXEC CICS  RETURN TRANSID('GA1S')                    00126700
126800                        COMMAREA(COMMON-WORKAREAS)                00126800
126900                        END-EXEC                                  00126900
127000         ELSE                                                     00127000
127100         IF INTR-TAB-PGM-ID = 'GA2IPGM'                           00127100
127200             MOVE SPACES TO DELADD-OPTION                         00127200
127300             EXEC CICS  RETURN TRANSID('GA2I')                    00127300
127400                        COMMAREA(COMMON-WORKAREAS)                00127400
127500                        END-EXEC                                  00127500
127600         ELSE                                                     00127600
127700         IF INTR-TAB-PGM-ID = 'GA2SPGM'                           00127700
127800             MOVE SPACES TO DELADD-OPTION                         00127800
127900             EXEC CICS  RETURN TRANSID('GA2S')                    00127900
128000                        COMMAREA(COMMON-WORKAREAS)                00128000
128100                        END-EXEC                                  00128100
128200         ELSE                                                     00128200
128300         IF INTR-TAB-PGM-ID = 'GA1NPGM'                           00128300
128400             MOVE SPACES TO DELADD-OPTION                         00128400
128500             EXEC CICS  RETURN TRANSID('GA1N')                    00128500
128600                        COMMAREA(COMMON-WORKAREAS)                00128600
128700                        END-EXEC                                  00128700
128800         ELSE                                                     00128800
128900         IF INTR-TAB-PGM-ID = 'GA2NPGM'                           00128900
129000             MOVE SPACES TO DELADD-OPTION                         00129000
129100             EXEC CICS  RETURN TRANSID('GA2N')                    00129100
129200                        COMMAREA(COMMON-WORKAREAS)                00129200
129300                        END-EXEC                                  00129300
129400         ELSE                                                     00129400
129500         IF INTR-TAB-PGM-ID = 'GA1OPGM'                           00129500
129600             MOVE SPACES TO DELADD-OPTION                         00129600
129700             EXEC CICS  RETURN TRANSID('GA1O')                    00129700
129800                        COMMAREA(COMMON-WORKAREAS)                00129800
129900                        END-EXEC                                  00129900
130000         ELSE                                                     00130000
130100         IF INTR-TAB-PGM-ID = 'GA2OPGM'                           00130100
130200             MOVE SPACES TO DELADD-OPTION                         00130200
130300             EXEC CICS  RETURN TRANSID('GA2O')                    00130300
130400                        COMMAREA(COMMON-WORKAREAS)                00130400
130500                        END-EXEC                                  00130500
130600         ELSE                                                     00130600
130700         EXEC CICS  RETURN TRANSID('GA1B')                        00130700
130800                    COMMAREA(DFHCOMMAREA)                         00130800
130900                    LENGTH  (EIBCALEN)                            00130900
131000                    END-EXEC                                      00131000
131100     ELSE                                                         00131100
131200     EXEC CICS  RETURN TRANSID('GA1B')                            00131200
131300                COMMAREA(DFHCOMMAREA)                             00131300
131400                LENGTH  (EIBCALEN)                                00131400
131500                END-EXEC.                                         00131500
131600                                                                  00131600
131700     GOBACK.                                                      00131700
131800 1000-999-EXIT.                                                   00131800
131900        EXIT.                                                     00131900
132000/*****************************************************************00132000
132100* 1001  VALIDATE ACTION CODES                                    *00132100
132200*                                                                *00132200
132300* DOES ERROR CHECKING ON ACTION CODES IN THE INTERNAL TABULAR    *00132300
132400* AREA OF THE SCREEN.  WITHOUT THIS ROUTINE, WILL DROP THROUGH   *00132400
132500* EXISTING EDITS AND CAUSE ASRA.                                 *00132500
132600*                                                                *00132600
132700******************************************************************00132700
132800 1001-000-VALIDATE-ACTION-CODES SECTION.                          00132800
132900 1001-010.                                                        00132900
133000                                                                  00133000
133100     MOVE IBGROPTI TO WS-ACTION-CODE-TEST-AREA.                   00133100
133200     INSPECT WS-ACTION-CODE-TEST-AREA                             00133200
133300         REPLACING ALL LOW-VALUES BY SPACES.                      00133300
133400     IF NOT VALID-ACTION-CODE                                     00133400
133500         MOVE DFHBMUBF TO IBGROPTA                                00133500
133600         IF NOT ACWA-SCREEN-HAS-ERRORS                            00133600
133700             MOVE -1 TO IBGROPTL                                  00133700
133800             SET WT-01-INDEX TO 23                                00133800
133900             MOVE WT-01-MESSAGE-TEXT (WT-01-INDEX) TO ERRMSGO     00133900
134000             MOVE 'Y' TO ACWA-ERROR-SW                            00134000
134100         END-IF                                                   00134100
134200     END-IF.                                                      00134200
134300                                                                  00134300
134400     MOVE IPGNOPTI TO WS-ACTION-CODE-TEST-AREA.                   00134400
134500     INSPECT WS-ACTION-CODE-TEST-AREA                             00134500
134600         REPLACING ALL LOW-VALUES BY SPACES.                      00134600
134700     IF NOT VALID-ACTION-CODE                                     00134700
134800         MOVE DFHBMUBF TO IPGNOPTA                                00134800
134900         IF NOT ACWA-SCREEN-HAS-ERRORS                            00134900
135000             MOVE -1 TO IPGNOPTL                                  00135000
135100             SET WT-01-INDEX TO 23                                00135100
135200             MOVE WT-01-MESSAGE-TEXT (WT-01-INDEX) TO ERRMSGO     00135200
135300             MOVE 'Y' TO ACWA-ERROR-SW                            00135300
135400         END-IF                                                   00135400
135500     END-IF.                                                      00135500
135600                                                                  00135600
135700     MOVE IPGTOPTI TO WS-ACTION-CODE-TEST-AREA.                   00135700
135800     INSPECT WS-ACTION-CODE-TEST-AREA                             00135800
135900         REPLACING ALL LOW-VALUES BY SPACES.                      00135900
136000     IF NOT VALID-ACTION-CODE                                     00136000
136100         MOVE DFHBMUBF TO IPGTOPTA                                00136100
136200         IF NOT ACWA-SCREEN-HAS-ERRORS                            00136200
136300             MOVE -1 TO IPGTOPTL                                  00136300
136400             SET WT-01-INDEX TO 23                                00136400
136500             MOVE WT-01-MESSAGE-TEXT (WT-01-INDEX) TO ERRMSGO     00136500
136600             MOVE 'Y' TO ACWA-ERROR-SW                            00136600
136700         END-IF                                                   00136700
136800     END-IF.                                                      00136800
136900                                                                  00136900
137000     MOVE IPGSOPTI TO WS-ACTION-CODE-TEST-AREA.                   00137000
137100     INSPECT WS-ACTION-CODE-TEST-AREA                             00137100
137200         REPLACING ALL LOW-VALUES BY SPACES.                      00137200
137300     IF NOT VALID-ACTION-CODE                                     00137300
137400         MOVE DFHBMUBF TO IPGSOPTA                                00137400
137500         IF NOT ACWA-SCREEN-HAS-ERRORS                            00137500
137600             MOVE -1 TO IPGSOPTL                                  00137600
137700             SET WT-01-INDEX TO 23                                00137700
137800             MOVE WT-01-MESSAGE-TEXT (WT-01-INDEX) TO ERRMSGO     00137800
137900             MOVE 'Y' TO ACWA-ERROR-SW                            00137900
138000         END-IF                                                   00138000
138100     END-IF.                                                      00138100
138200                                                                  00138200
138300     MOVE IDGDOPTI TO WS-ACTION-CODE-TEST-AREA.                   00138300
138400     INSPECT WS-ACTION-CODE-TEST-AREA                             00138400
138500         REPLACING ALL LOW-VALUES BY SPACES.                      00138500
138600     IF NOT VALID-ACTION-CODE                                     00138600
138700         MOVE DFHBMUBF TO IDGDOPTA                                00138700
138800         IF NOT ACWA-SCREEN-HAS-ERRORS                            00138800
138900             MOVE -1 TO IDGDOPTL                                  00138900
139000             SET WT-01-INDEX TO 23                                00139000
139100             MOVE WT-01-MESSAGE-TEXT (WT-01-INDEX) TO ERRMSGO     00139100
139200             MOVE 'Y' TO ACWA-ERROR-SW                            00139200
139300         END-IF                                                   00139300
139400     END-IF.                                                      00139400
139500                                                                  00139500
139600     MOVE IPGPOPTI TO WS-ACTION-CODE-TEST-AREA.                   00139600
139700     INSPECT WS-ACTION-CODE-TEST-AREA                             00139700
139800         REPLACING ALL LOW-VALUES BY SPACES.                      00139800
139900     IF NOT VALID-ACTION-CODE                                     00139900
140000         MOVE DFHBMUBF TO IPGPOPTA                                00140000
140100         IF NOT ACWA-SCREEN-HAS-ERRORS                            00140100
140200             MOVE -1 TO IPGPOPTL                                  00140200
140300             SET WT-01-INDEX TO 23                                00140300
140400             MOVE WT-01-MESSAGE-TEXT (WT-01-INDEX) TO ERRMSGO     00140400
140500             MOVE 'Y' TO ACWA-ERROR-SW                            00140500
140600         END-IF                                                   00140600
140700     END-IF.                                                      00140700
140800                                                                  00140800
140900 1001-999-EXIT.                                                   00140900
141000        EXIT.                                                     00141000
141100/*****************************************************************00141100
141200* 1100  VALIDATE SCREEN                                          *00141200
141300*                                                                *00141300
141400*    THIS IS PRIMARILY A VALIDATION ROUTINE OF DATA BEING ENTERED*00141400
141500*  BY THE OPERATOR, PLUS THE ADDITION OF SOME REINITIALIZATION.  *00141500
141600*  1. REINITIALIZE ATTRIBUTES THAT THE PROGRAM MIGHT MODIFY, AND *00141600
141700*     RESET THE ERROR MESSAGE AND DELETE OPTION TO BLANKS.       *00141700
141800*  2. INSURE THE VALIDITY OF THE OPTIONS THAT CAN BE USED FOR THE*00141800
141900*     INTERNAL TABULAR.                                          *00141900
142000******************************************************************00142000
142100 1100-000-VALIDATE-SCREEN       SECTION.                          00142100
142200 1100-010.                                                        00142200
142300                                                                  00142300
142400     SET ACWA-WF-ALL-LEVEL-TAB-PNTR TO                            00142400
142500         ADDRESS OF  WF-IO-PARM-ALL-LVL-TAB-RECORD.               00142500
142600                                                                  00142600
142700     SET ACWA-COPY-TAB-PNTR         TO                            00142700
142800         ADDRESS OF  COPY-TABULAR-TABLE-AREA.                     00142800
142900                                                                  00142900
143000     SET ACWA-WF-INTERNAL-TAB-PNTR  TO                            00143000
143100         ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD.              00143100
143200                                                                  00143200
143300     SET ACWA-WF-GRP-SPEC-PNTR      TO                            00143300
143400         ADDRESS OF  WF-IO-PARM-WRK-GRP-SPEC-REC.                 00143400
143500                                                                  00143500
143600     SET ACWA-WF-CONTRACT-PNTR      TO                            00143600
143700         ADDRESS OF  WF-IO-PARM-WRK-CONTRACT-REC.                 00143700
143800                                                                  00143800
143900     SET ACWA-WF-BEN-PROV-PNTR      TO                            00143900
144000         ADDRESS OF  WF-IO-PARM-WRK-BEN-PROV-REC.                 00144000
144100                                                                  00144100
144200     SET ACWA-WF-CONTROL-RECORD-PNTR    TO                        00144200
144300         ADDRESS OF  WF-IO-PARM-WRK-CONTROL-REC.                  00144300
144400                                                                  00144400
144500     SET ACWA-PR-CONTRACT-PNTR      TO                            00144500
144600         ADDRESS OF  PR-IO-PARM-WRK-CONTRACT-REC.                 00144600
144700                                                                  00144700
144800     SET ACWA-PR-GRP-SPEC-PNTR      TO                            00144800
144900         ADDRESS OF  PR-IO-PARM-WRK-GRP-SPEC-REC.                 00144900
145000                                                                  00145000
145100     SET ACWA-PR-BEN-PROV-PNTR      TO                            00145100
145200         ADDRESS OF  PR-IO-PARM-WRK-BEN-PROV-REC.                 00145200
145300                                                                  00145300
145400     SET ACWA-PR-ALL-LEVEL-TAB-PNTR   TO                          00145400
145500         ADDRESS OF  PR-IO-PARM-ALL-LVL-TAB-RECORD.               00145500
145600                                                                  00145600
145700     MOVE 'N'              TO ACWA-ERROR-SW.                      00145700
145800     MOVE 'Y'              TO GCVI-TABLE-SW.                      00145800
145900     MOVE SPACES           TO ERRMSGO.                            00145900
146000     MOVE DFHBMFSE         TO PERIODA.                            00146000
146100     MOVE DFHBMASF         TO IBGRIDA    IPGNIDA   IPGTIDA        00146100
146200                              IDGDIDA    IPGPIDA   IPGSIDA        00146200
146300                              IBGRSLTA   IPGNSLTA  IPGTSLTA       00146300
146400                              IDGDSLTA   IPGPSLTA  IPGSSLTA.      00146400
146500                                                                  00146500
146600     PERFORM 7900-000-RESET-ATTRIBUTES.                           00146600
146700                                                                  00146700
146800*------------- LINK TO SCREEN EDIT MODULE -----------------------*00146800
146900                                                                  00146900
147000     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00147000
147100                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00147100
147200     EXEC CICS  LINK  PROGRAM('GASEDIT1')                         00147200
147300                COMMAREA (COMMON-WORKAREAS)                       00147300
147400                LENGTH(LENGTH OF COMMON-WORKAREAS)   END-EXEC.    00147400
147500                                                                  00147500
147600     IF  ACWA-SCREEN-HAS-ERRORS                                   00147600
147700         GO TO 1100-900-EXIT.                                     00147700
147800                                                                  00147800
147900     IF  ACWA-FIELD-CHG-CNT > ZEROS                               00147900
148000         GO TO 1100-900-EXIT.                                     00148000
148100                                                                  00148100
148200     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00148200
148300         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00148300
148400         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00148400
148500         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00148500
148600         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00148600
148700         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00148700
148800     THEN                                                         00148800
148900         GO TO 1100-900-EXIT.                                     00148900
149000                                                                  00149000
149100                                                                  00149100
149200     COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   00149200
149300              GC-GCIOPARM-LEN                 +                   00149300
149400              GC-WORKFILE-KEY-LEN             +                   00149400
149500              GC-GCTABULR-IPGP-FIXED-LEN      +                   00149500
149600             (GC-GCTABULR-IPGP-VARY-LEN       *                   00149600
149700              GC-GCTABULR-IPGP-VARY-MAX-OCUR)                     00149700
149800                                                                  00149800
149900        EXEC CICS GETMAIN                                         00149900
150000               SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     00150000
150100               INITIMG(WS-HEX-00)                                 00150100
150200               LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)             00150200
150300               END-EXEC                                           00150300
150400                                                                  00150400
150500     SET ACWA-WF-INTERNAL-TAB-PNTR  TO                            00150500
150600         ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD.              00150600
150700                                                                  00150700
150800     IF  IBGROPTI  =  'C'                                         00150800
150900     THEN                                                         00150900
151000         IF  IBGRSLTI  >  '8999999'                               00151000
151100         THEN                                                     00151100
151200             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00151200
151300             GO TO 1100-100-GET-INTERNAL-TAB                      00151300
151400         ELSE                                                     00151400
151500             ADD 100        TO  ACWA-FIELD-CHG-CNT                00151500
151600             MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID               00151600
151700             MOVE IBGRSLTI  TO  GCIO-TAB-SLOT-NO                  00151700
151800     ELSE                                                         00151800
151900         NEXT SENTENCE.                                           00151900
152000                                                                  00152000
152100                                                                  00152100
152200     IF  IPGNOPTI  =  'C'                                         00152200
152300     THEN                                                         00152300
152400         IF  IPGNSLTI  > '8999999'                                00152400
152500         THEN                                                     00152500
152600             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00152600
152700             GO TO 1100-100-GET-INTERNAL-TAB                      00152700
152800         ELSE                                                     00152800
152900           ADD 100        TO  ACWA-FIELD-CHG-CNT                  00152900
153000           MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                 00153000
153100           MOVE IPGNSLTI  TO  GCIO-TAB-SLOT-NO                    00153100
153200     ELSE                                                         00153200
153300         NEXT SENTENCE.                                           00153300
153400                                                                  00153400
153500                                                                  00153500
153600     IF  IPGTOPTI  =  'C'                                         00153600
153700     THEN                                                         00153700
153800         IF  IPGTSLTI  >  '8999999'                               00153800
153900         THEN                                                     00153900
154000             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00154000
154100             GO TO 1100-100-GET-INTERNAL-TAB                      00154100
154200         ELSE                                                     00154200
154300             ADD 100        TO  ACWA-FIELD-CHG-CNT                00154300
154400             MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID               00154400
154500             MOVE IPGTSLTI  TO  GCIO-TAB-SLOT-NO                  00154500
154600     ELSE                                                         00154600
154700         NEXT SENTENCE.                                           00154700
154800                                                                  00154800
154900     IF  IPGSOPTI  =  'C'                                         00154900
155000     THEN                                                         00155000
155100         IF  IPGSSLTI  >  '8999999'                               00155100
155200         THEN                                                     00155200
155300             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00155300
155400             GO TO 1100-100-GET-INTERNAL-TAB                      00155400
155500         ELSE                                                     00155500
155600             ADD 100        TO  ACWA-FIELD-CHG-CNT                00155600
155700             MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID               00155700
155800             MOVE IPGSSLTI  TO  GCIO-TAB-SLOT-NO                  00155800
155900     ELSE                                                         00155900
156000         NEXT SENTENCE.                                           00156000
156100                                                                  00156100
156200     IF  IDGDOPTI  =  'C'                                         00156200
156300     THEN                                                         00156300
156400         IF  IDGDSLTI  >  '8999999'                               00156400
156500         THEN                                                     00156500
156600             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00156600
156700             GO TO 1100-100-GET-INTERNAL-TAB                      00156700
156800         ELSE                                                     00156800
156900             ADD 100        TO  ACWA-FIELD-CHG-CNT                00156900
157000             MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID               00157000
157100             MOVE IDGDSLTI  TO  GCIO-TAB-SLOT-NO                  00157100
157200     ELSE                                                         00157200
157300         NEXT SENTENCE.                                           00157300
157400                                                                  00157400
157500     IF  IPGPOPTI  =  'C'                                         00157500
157600     THEN                                                         00157600
157700         IF  IPGPSLTI  >  '8999999'                               00157700
157800         THEN                                                     00157800
157900             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00157900
158000             GO TO 1100-100-GET-INTERNAL-TAB                      00158000
158100         ELSE                                                     00158100
158200             ADD 100        TO  ACWA-FIELD-CHG-CNT                00158200
158300             MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID               00158300
158400             MOVE IPGPSLTI  TO  GCIO-TAB-SLOT-NO                  00158400
158500     ELSE                                                         00158500
158600         NEXT SENTENCE.                                           00158600
158700                                                                  00158700
158800                                                                  00158800
158900     IF IBGROPTI  =  'MT'                                         00158900
159000        MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID                    00159000
159100        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00159100
159200                                                                  00159200
159300     IF IBGROPTI  =  'A'                                          00159300
159400        MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID                    00159400
159500        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00159500
159600                                                                  00159600
159700     IF IPGNOPTI  =  'MT'                                         00159700
159800        MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                    00159800
159900        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00159900
160000                                                                  00160000
160100     IF IPGNOPTI  =  'A'                                          00160100
160200        MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                    00160200
160300        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00160300
160400                                                                  00160400
160500     IF IPGTOPTI  =  'MT'                                         00160500
160600        MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID                    00160600
160700        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00160700
160800                                                                  00160800
160900     IF IPGTOPTI  =  'A'                                          00160900
161000        MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID                    00161000
161100        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00161100
161200                                                                  00161200
161300     IF IPGSOPTI  =  'MT'                                         00161300
161400        MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID                    00161400
161500        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00161500
161600                                                                  00161600
161700     IF IPGSOPTI  =  'A'                                          00161700
161800        MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID                    00161800
161900        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00161900
162000                                                                  00162000
162100     IF IDGDOPTI  =  'MT'                                         00162100
162200        MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID                    00162200
162300        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00162300
162400                                                                  00162400
162500     IF IDGDOPTI  =  'A'                                          00162500
162600        MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID                    00162600
162700        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00162700
162800                                                                  00162800
162900     IF IPGPOPTI  =  'MT'                                         00162900
163000        MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                    00163000
163100        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00163100
163200                                                                  00163200
163300     IF IPGPOPTI  =  'A'                                          00163300
163400        MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                    00163400
163500        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00163500
163600                                                                  00163600
163700     MOVE GCIO-TABULAR-FILE      TO GCIO2-FILE-KEY.               00163700
163800     MOVE GC-GCTABULR-DDNAME     TO GCIO2-FILE-DDNAME.            00163800
163900     MOVE GC-GCIO-AREA-2         TO GCIO2-IO-AREA-TO-USE.         00163900
164000     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO2-FILE-ACCESS-CODE.       00164000
164100     MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          00164100
164200          TO  GXA-ENTRY-COUNT.                                    00164200
164300                                                                  00164300
164400     EXEC CICS  LINK  PROGRAM('GCIOPGM')                          00164400
164500                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00164500
164600                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)   END-EXEC.00164600
164700*????                                                             00164700
164800     MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                   00164800
164900                                                                  00164900
165000     IF  NOT GCIO2-GOOD-RETURN AND  ACWA-PROD-INTERNAL-CHG        00165000
165100     THEN                                                         00165100
165200         SET  WT-01-INDEX                     TO +17              00165200
165300         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00165300
165400         MOVE 'Y'                             TO ACWA-ERROR-SW    00165400
165500         IF  IBGROPTI  =  'C'                                     00165500
165600         THEN                                                     00165600
165700             MOVE -1        TO  IBGROPTL                          00165700
165800             MOVE DFHBMUBF  TO  IBGROPTA                          00165800
165900             MOVE DFHBMASB  TO  IBGRIDA  IBGRSLTA                 00165900
166000             GO TO 1100-900-EXIT                                  00166000
166100         ELSE                                                     00166100
166200         IF  IPGNOPTI  =  'C'                                     00166200
166300         THEN                                                     00166300
166400             MOVE -1        TO  IPGNOPTL                          00166400
166500             MOVE DFHBMUBF  TO  IPGNOPTA                          00166500
166600             MOVE DFHBMASB  TO  IPGNIDA  IPGNSLTA                 00166600
166700             GO TO 1100-900-EXIT                                  00166700
166800         ELSE                                                     00166800
166900         IF  IPGTOPTI  =  'C'                                     00166900
167000         THEN                                                     00167000
167100             MOVE -1        TO  IPGTOPTL                          00167100
167200             MOVE DFHBMUBF  TO  IPGTOPTA                          00167200
167300             MOVE DFHBMASB  TO  IPGTIDA  IPGTSLTA                 00167300
167400             GO TO 1100-900-EXIT                                  00167400
167500         ELSE                                                     00167500
167600         IF  IPGSOPTI  =  'C'                                     00167600
167700         THEN                                                     00167700
167800             MOVE -1        TO  IPGSOPTL                          00167800
167900             MOVE DFHBMUBF  TO  IPGSOPTA                          00167900
168000             MOVE DFHBMASB  TO  IPGSIDA  IPGSSLTA                 00168000
168100             GO TO 1100-900-EXIT                                  00168100
168200         ELSE                                                     00168200
168300         IF  IDGDOPTI  =  'C'                                     00168300
168400         THEN                                                     00168400
168500             MOVE -1        TO  IDGDOPTL                          00168500
168600             MOVE DFHBMUBF  TO  IDGDOPTA                          00168600
168700             MOVE DFHBMASB  TO  IDGDIDA  IDGDSLTA                 00168700
168800             GO TO 1100-900-EXIT                                  00168800
168900         ELSE                                                     00168900
169000         IF  IPGPOPTI  =  'C'                                     00169000
169100         THEN                                                     00169100
169200             MOVE -1        TO  IPGPOPTL                          00169200
169300             MOVE DFHBMUBF  TO  IPGPOPTA                          00169300
169400             MOVE DFHBMASB  TO  IPGPIDA  IPGPSLTA                 00169400
169500             GO TO 1100-900-EXIT                                  00169500
169600         ELSE                                                     00169600
169700             NEXT SENTENCE                                        00169700
169800     ELSE                                                         00169800
169900         NEXT SENTENCE.                                           00169900
170000                                                                  00170000
170100                                                                  00170100
170200     IF  NOT GCIO2-GOOD-RETURN AND  GCIO-TAB-SLOT-NO  NOT =  1    00170200
170300     THEN                                                         00170300
170400         SET  WT-01-INDEX                     TO +18              00170400
170500         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00170500
170600         MOVE 'Y'                             TO ACWA-ERROR-SW    00170600
170700         MOVE -1                              TO MFRMSLTL         00170700
170800         MOVE DFHBMUBF                        TO MFRMSLTA         00170800
170900         GO TO 1100-900-EXIT.                                     00170900
171000                                                                  00171000
171100     IF  NOT GCIO2-GOOD-RETURN                                    00171100
171200     THEN                                                         00171200
171300         MOVE WS-ABCODE-1BF1        TO WS-ABCODE                  00171300
171400         MOVE WS-ABCODE-1BF1-MSG    TO WS-ABCODE-MSG              00171400
171500         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00171500
171600                                                                  00171600
171700*????                                                             00171700
171800     MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                   00171800
171900                                                                  00171900
172000     COMPUTE  GCIO2-RECORD-LENGTH  =                              00172000
172100              GC-WORKFILE-KEY-LEN  +                              00172100
172200              GCIO2-RECORD-LENGTH.                                00172200
172300                                                                  00172300
172400     IF IBGROPTI  =  'C' OR  'MT' OR 'A'                          00172400
172500        IF  GXA-ENTRY-COUNT  NOT >  1                             00172500
172600            MOVE 'GA2GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00172600
172700                                INTR-TAB-PGM-ID                   00172700
172800        ELSE                                                      00172800
172900            MOVE 'GA1GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00172900
173000                                INTR-TAB-PGM-ID.                  00173000
173100                                                                  00173100
173200     IF IPGNOPTI  =  'C' OR  'MT' OR 'A'                          00173200
173300        IF  GXA-ENTRY-COUNT  NOT >  1                             00173300
173400            MOVE 'GA2HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00173400
173500                                INTR-TAB-PGM-ID                   00173500
173600        ELSE                                                      00173600
173700            MOVE 'GA1HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00173700
173800                                INTR-TAB-PGM-ID.                  00173800
173900                                                                  00173900
174000     IF IPGTOPTI  =  'C' OR  'MT' OR 'A'                          00174000
174100        IF  GXA-ENTRY-COUNT  NOT >  1                             00174100
174200            MOVE 'GA2IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00174200
174300                                INTR-TAB-PGM-ID                   00174300
174400        ELSE                                                      00174400
174500            MOVE 'GA1IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00174500
174600                                INTR-TAB-PGM-ID.                  00174600
174700                                                                  00174700
174800     IF IPGSOPTI  =  'C' OR  'MT' OR 'A'                          00174800
174900        IF  GXA-ENTRY-COUNT  NOT >  1                             00174900
175000            MOVE 'GA2SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00175000
175100                                INTR-TAB-PGM-ID                   00175100
175200        ELSE                                                      00175200
175300            MOVE 'GA1SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00175300
175400                                INTR-TAB-PGM-ID.                  00175400
175500                                                                  00175500
175600     IF IDGDOPTI  =  'C' OR  'MT' OR 'A'                          00175600
175700        IF  GXA-ENTRY-COUNT  NOT >  1                             00175700
175800            MOVE 'GA2NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00175800
175900                                INTR-TAB-PGM-ID                   00175900
176000        ELSE                                                      00176000
176100            MOVE 'GA1NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00176100
176200                                INTR-TAB-PGM-ID.                  00176200
176300                                                                  00176300
176400     IF IPGPOPTI  =  'C' OR  'MT' OR 'A'                          00176400
176500        IF  GXA-ENTRY-COUNT  NOT >  1                             00176500
176600            MOVE 'GA2OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00176600
176700                                INTR-TAB-PGM-ID                   00176700
176800        ELSE                                                      00176800
176900            MOVE 'GA1OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00176900
177000                                INTR-TAB-PGM-ID.                  00177000
177100                                                                  00177100
177200     GO TO 1100-900-EXIT.                                         00177200
177300                                                                  00177300
177400/                                                                 00177400
177500 1100-100-GET-INTERNAL-TAB.                                       00177500
177600                                                                  00177600
177700                                                                  00177700
177800     IF FRMNUIDI  =  'GS3A'                                       00177800
177900        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00177900
178000        MOVE 'G4'  TO GCIO-WRK-RECORD-TYPE.                       00178000
178100                                                                  00178100
178200     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00178200
178300        PERFORM 6100-000-BUILD-CONTRACT-KEY                       00178300
178400        MOVE 'C3'  TO GCIO-WRK-RECORD-TYPE.                       00178400
178500                                                                  00178500
178600     IF FRMNUIDI  =  'GC8A'                                       00178600
178700        PERFORM 6200-000-BUILD-BEN-PROV-KEY                       00178700
178800        MOVE 'C6'               TO GCIO-WRK-RECORD-TYPE           00178800
178900        MOVE TABIDI             TO GCIO-WRK-PROVISION-ID          00178900
179000        MOVE TABSLTNI           TO ACWA-DISPLAY-LEN-7             00179000
179100        MOVE ACWA-DISPLAY-LEN-7 TO GCIO-WRK-PROVISION-SLOT-NO.    00179100
179200                                                                  00179200
179300     MOVE GC-GCPSWORK-DDNAME TO GCIO2-FILE-DDNAME.                00179300
179400     MOVE GC-GCIO-AREA-1     TO GCIO2-IO-AREA-TO-USE.             00179400
179500                                                                  00179500
179600     IF IBGROPTI  =  'C'                                          00179600
179700        MOVE '#IBGR '  TO  GCIO-WRK-TAB-PROVISION-ID              00179700
179800        MOVE IBGRSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00179800
179900                                                                  00179900
180000     IF IPGNOPTI  =  'C'                                          00180000
180100        MOVE '#IPGN '  TO  GCIO-WRK-TAB-PROVISION-ID              00180100
180200        MOVE IPGNSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00180200
180300                                                                  00180300
180400     IF IPGTOPTI  =  'C'                                          00180400
180500        MOVE '#IPGT '  TO  GCIO-WRK-TAB-PROVISION-ID              00180500
180600        MOVE IPGTSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00180600
180700                                                                  00180700
180800     IF IPGSOPTI  =  'C'                                          00180800
180900        MOVE '#IPGS '  TO  GCIO-WRK-TAB-PROVISION-ID              00180900
181000        MOVE IPGSSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00181000
181100                                                                  00181100
181200     IF IDGDOPTI  =  'C'                                          00181200
181300        MOVE '#IDGD '  TO  GCIO-WRK-TAB-PROVISION-ID              00181300
181400        MOVE IDGDSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00181400
181500                                                                  00181500
181600     IF IPGPOPTI  =  'C'                                          00181600
181700        MOVE '#IPGP '  TO  GCIO-WRK-TAB-PROVISION-ID              00181700
181800        MOVE IPGPSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00181800
181900                                                                  00181900
182000     MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              00182000
182100     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO2-FILE-ACCESS-CODE.       00182100
182200     MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          00182200
182300          TO  GXA-ENTRY-COUNT.                                    00182300
182400                                                                  00182400
182500                                                                  00182500
182600     EXEC CICS  LINK  PROGRAM('GCIOPGM')                          00182600
182700                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00182700
182800                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. 00182800
182900                                                                  00182900
183000     IF NOT GCIO2-GOOD-RETURN                                     00183000
183100        MOVE WS-ABCODE-1BF2        TO WS-ABCODE                   00183100
183200        MOVE WS-ABCODE-1BF2-MSG    TO WS-ABCODE-MSG               00183200
183300        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00183300
183400*????                                                             00183400
183500     MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                   00183500
183600                                                                  00183600
183700     IF IBGROPTI  =  'C'                                          00183700
183800        IF GXA-ENTRY-COUNT  NOT >  1                              00183800
183900           MOVE 'GA2GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00183900
184000                                INTR-TAB-PGM-ID                   00184000
184100        ELSE                                                      00184100
184200           MOVE 'GA1GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00184200
184300                                INTR-TAB-PGM-ID.                  00184300
184400                                                                  00184400
184500     IF IPGNOPTI  =  'C'                                          00184500
184600        IF GXA-ENTRY-COUNT  NOT >  1                              00184600
184700           MOVE 'GA2HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00184700
184800                                INTR-TAB-PGM-ID                   00184800
184900        ELSE                                                      00184900
185000           MOVE 'GA1HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00185000
185100                                INTR-TAB-PGM-ID.                  00185100
185200                                                                  00185200
185300     IF IPGTOPTI  =  'C'                                          00185300
185400        IF GXA-ENTRY-COUNT  NOT >  1                              00185400
185500           MOVE 'GA2IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00185500
185600                                INTR-TAB-PGM-ID                   00185600
185700        ELSE                                                      00185700
185800           MOVE 'GA1IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00185800
185900                                INTR-TAB-PGM-ID.                  00185900
186000                                                                  00186000
186100     IF IPGSOPTI  =  'C'                                          00186100
186200        IF GXA-ENTRY-COUNT  NOT >  1                              00186200
186300           MOVE 'GA2SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00186300
186400                                INTR-TAB-PGM-ID                   00186400
186500        ELSE                                                      00186500
186600           MOVE 'GA1SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00186600
186700                                INTR-TAB-PGM-ID.                  00186700
186800                                                                  00186800
186900     IF IDGDOPTI  =  'C'                                          00186900
187000        IF GXA-ENTRY-COUNT  NOT >  1                              00187000
187100           MOVE 'GA2NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00187100
187200                                INTR-TAB-PGM-ID                   00187200
187300        ELSE                                                      00187300
187400           MOVE 'GA1NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00187400
187500                                INTR-TAB-PGM-ID.                  00187500
187600                                                                  00187600
187700     IF IPGPOPTI  =  'C'                                          00187700
187800        IF GXA-ENTRY-COUNT  NOT >  1                              00187800
187900           MOVE 'GA2OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00187900
188000                                INTR-TAB-PGM-ID                   00188000
188100        ELSE                                                      00188100
188200           MOVE 'GA1OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00188200
188300                                INTR-TAB-PGM-ID.                  00188300
188400                                                                  00188400
188500     GO TO 1100-900-EXIT.                                         00188500
188600                                                                  00188600
188700 1100-900-EXIT. EXIT.                                             00188700
188800/*****************************************************************00188800
188900* 2000  PROCESS REQUEST                                          *00188900
189000*                                                                *00189000
189100*   THIS ROUTINE CHECKS THE CHARACTERISTICS OF THE INCOMING TRANS-00189100
189200* ACTION AND ROUTES THEM TO THE APPROPRIATE ROUTINE TO PROCESS   *00189200
189300* THE REQUEST.                                                   *00189300
189400******************************************************************00189400
189500 2000-000-PROCESS-REQUEST       SECTION.                          00189500
189600 2000-010.                                                        00189600
189700                                                                  00189700
189800     IF (EIBAID       =  DFHENTER OR DFHPF4 OR DFHPF16) AND       00189800
189900        DELADDI       =  'CHG/ADD'                      AND       00189900
190000        OENTCTRI      =  '0000000'                                00190000
190100        PERFORM 2100-000-INSERT-SKELETON.                         00190100
190200                                                                  00190200
190300** LINK TO THE CHANGE/DELETE MODULE TO PROCESS FIVE DIFFERENT     00190300
190400** REQUESTS DEPENDING ON THE USER RESPONSE                        00190400
190500**                                                                00190500
190600     MOVE 'CHG/DEL' TO DELADD-OPTION.                             00190600
190700     MOVE SPACES TO LVL2-B-SW  LVL2-F-SW  LVL2-G-SW.              00190700
190800                                                                  00190800
190900     MOVE WS-ALT-WORKFILE-KEYS       TO ACWA-ALT-WORKFILE-KEYS.   00190900
191000***  SET  ACWA-INDEX-1               TO GAA-INDEX.                00191000
191100                                                                  00191100
191200     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00191200
191300                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00191300
191400                                                                  00191400
191500     EXEC CICS  LINK  PROGRAM ('GAS1UPD')                         00191500
191600                COMMAREA(COMMON-WORKAREAS)                        00191600
191700                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00191700
191800                                                                  00191800
191900     SET ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD  TO             00191900
192000                 ACWA-WF-ALL-LEVEL-TAB-PNTR.                      00192000
192100                                                                  00192100
192200     SET ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD TO             00192200
192300                 ACWA-WF-INTERNAL-TAB-PNTR.                       00192300
192400                                                                  00192400
192500                                                                  00192500
192600     IF LVL2-B-SW =  'Y'                                          00192600
192700        PERFORM  7900-000-RESET-ATTRIBUTES                        00192700
192800        PERFORM  8100-000-DISPLAY-ADD-SCREEN.                     00192800
192900                                                                  00192900
193000     IF LVL2-F-SW =  'Y'                                          00193000
193100        PERFORM  3100-000-READ-RECORD                             00193100
193200        PERFORM  4300-000-DISPLAY-PREV.                           00193200
193300                                                                  00193300
193400     IF LVL2-G-SW =  'Y'                                          00193400
193500        PERFORM  3100-000-READ-RECORD                             00193500
193600        PERFORM  4200-000-DISPLAY-NEXT.                           00193600
193700                                                                  00193700
193800                                                                  00193800
193900                                                                  00193900
194000 2000-900-EXIT. EXIT.                                             00194000
194100                                                                  00194100
194200/*****************************************************************00194200
194300* 2100  INSERT SKELETON                                          *00194300
194400*                                                                *00194400
194500*    THIS ROUTINE WILL ADD A NEW ENTRY INTO THE TABLE.  IF THE   *00194500
194600*  TABLE ALREADY CONTAINS THE MAXIMUM NUMBER OF 29 ENTRIES THE   *00194600
194700*  SORT ROUTINE MAY REDUCE THAT NUMBER AS IT WILL DELETE ALL     *00194700
194800*  DUPLICATES.  IF THE NEW ENTRY CAN BE ADDED AND THE OPERATOR   *00194800
194900*  REQUESTED THE ADDITION OF AN INTERNAL TABULAR THIS ROUTINE    *00194900
195000*  WILL WRITE THE NEW INTERNAL TABULAR TO THE WORKFILE AND THEN  *00195000
195100*  PASS IT TO THE ADD VERSION OF THE INTERNAL TABULAR PROGRAM.   *00195100
195200******************************************************************00195200
195300 2100-000-INSERT-SKELETON       SECTION.                          00195300
195400 2100-010.                                                        00195400
195500                                                                  00195500
195600     IF  EIBAID = DFHENTER         AND                            00195600
195700         ACWA-SCREEN-HAS-NO-ERRORS AND                            00195700
195800         GCVI-TABLE-SW = 'N'                                      00195800
195900     THEN                                                         00195900
196000         SET  WT-01-INDEX                     TO +07              00196000
196100         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00196100
196200         PERFORM 9010-000-SEND-DATAONLY-RETURN.                   00196200
196300                                                                  00196300
196400     IF  EIBAID  = DFHPF4 OR DFHPF16                              00196400
196500         PERFORM 7900-000-RESET-ATTRIBUTES.                       00196500
196600                                                                  00196600
196700     PERFORM 3200-000-READ-REC-FOR-UPDATE.                        00196700
196800                                                                  00196800
196900     IF NOT GCIO-GOOD-RETURN                                      00196900
197000        MOVE WS-ABCODE-1BF5        TO WS-ABCODE                   00197000
197100        MOVE WS-ABCODE-1BF5-MSG    TO WS-ABCODE-MSG               00197100
197200        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00197200
197300                                                                  00197300
197400                                                                  00197400
197500     IF GAA-OCC-ENTRY-TAB-SLOT-CNTR > 9999900                     00197500
197600        SET  WT-01-INDEX                     TO +15               00197600
197700        MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO           00197700
197800        MOVE -1                              TO PERIODL           00197800
197900        PERFORM 9010-000-SEND-DATAONLY-RETURN.                    00197900
198000                                                                  00198000
198100                                                                  00198100
198200     MOVE GAA-ENTRY-COUNT  TO  GAA-ENTRY-COUNT.                   00198200
198300                                                                  00198300
198400     IF  GAA-ENTRY-COUNT  NOT <  GC-GCTABULR-ABM-VARY-MAX-OCUR    00198400
198500     THEN                                                         00198500
198600         PERFORM 6500-000-SORT-COMPRESS-ALL-LVL                   00198600
198700         IF  ACWA-SCREEN-HAS-NO-ERRORS                            00198700
198800         THEN                                                     00198800
198900             PERFORM 2500-000-ADD-NEW-OCCURS                      00198900
199000****         PERFORM 4600-000-UPDATE-CDE-STATUS                   00199000
199100             IF  WRK-CDE-SP = '2 '    AND                         00199100
199200                 CDEINDO    = ('+CDE+'  OR '+CDE-')               00199200
199300                 ADD 1      TO   ACWA-CDE-1U-COUNT                00199300
199400                 SUBTRACT 1 FROM ACWA-CDE-2B-COUNT                00199400
199500                 MOVE '1U'  TO   WRK-CDE-SP                       00199500
199600                 PERFORM 3000-000-UPDATE-GAA-RECORD               00199600
199700             ELSE                                                 00199700
199800                 PERFORM 3000-000-UPDATE-GAA-RECORD               00199800
199900         ELSE                                                     00199900
200000             SET  WT-01-INDEX                     TO +16          00200000
200100             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00200100
200200             MOVE -1                              TO  PERIODL     00200200
200300             PERFORM 9010-000-SEND-DATAONLY-RETURN                00200300
200400      ELSE                                                        00200400
200500          PERFORM 2500-000-ADD-NEW-OCCURS                         00200500
200600****      PERFORM 4600-000-UPDATE-CDE-STATUS                      00200600
200700             IF  WRK-CDE-SP = '2 '      AND                       00200700
200800                 CDEINDO    = ('+CDE+'  OR '+CDE-')               00200800
200900                 ADD 1      TO   ACWA-CDE-1U-COUNT                00200900
201000                 SUBTRACT 1 FROM ACWA-CDE-2B-COUNT                00201000
201100                 MOVE '1U'  TO   WRK-CDE-SP                       00201100
201200                 PERFORM 3000-000-UPDATE-GAA-RECORD               00201200
201300             ELSE                                                 00201300
201400                 PERFORM 3000-000-UPDATE-GAA-RECORD.              00201400
201500                                                                  00201500
201600     SET GAA-INDEX  DOWN BY  1.                                   00201600
201700                                                                  00201700
201800     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00201800
201900         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00201900
202000         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00202000
202100         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00202100
202200         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00202200
202300         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00202300
202400     THEN                                                         00202400
202500         PERFORM 4700-000-UPDATE-CONTROL-RECORD                   00202500
202600         MOVE GAA-OCCURS-ENTRY-COUNTER (GAA-INDEX)                00202600
202700                                 TO  ACWA-DISPLAY-LEN-7           00202700
202800         MOVE ACWA-DISPLAY-LEN-7 TO OENTCTRO                      00202800
202900         MOVE -1                 TO  PERIODL                      00202900
203000         PERFORM 9010-000-SEND-DATAONLY-RETURN.                   00203000
203100                                                                  00203100
203200*    EXEC CICS GETMAIN                                            00203200
203300*              SET(ADDRESS OF COMMUNICATION-KEY-AREA)             00203300
203400*              INITIMG(WS-HEX-00)                                 00203400
203500*              LENGTH(WS-COMMUNICATION-KEY-LEN)                   00203500
203600*              END-EXEC.                                          00203600
203700                                                                  00203700
203800*    SET ACWA-COMM-KEY-PNTR  TO                                   00203800
203900*                      ADDRESS OF COMMUNICATION-KEY-AREA.         00203900
204000                                                                  00204000
204100     IF FRMNUIDI  =  'GS3A'                                       00204100
204200        MOVE 'G4'    TO  GCIO-WRK-RECORD-TYPE                     00204200
204300        MOVE SPACES  TO  GCA-L-O-B                                00204300
204400                         GCA-PROV-CTL                             00204400
204500                         GCA-BEN-PROV-ID.                         00204500
204600                                                                  00204600
204700     IF FRMNUIDI = 'GC4A' OR 'GTM1'                               00204700
204800        MOVE 'C3'                       TO  GCIO-WRK-RECORD-TYPE  00204800
204900        MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B             00204900
205000        MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL          00205000
205100        MOVE SPACES  TO  GCA-BEN-PROV-ID.                         00205100
205200                                                                  00205200
205300     IF FRMNUIDI  =  'GC8A'                                       00205300
205400        MOVE 'C6'                       TO  GCIO-WRK-RECORD-TYPE  00205400
205500        MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B             00205500
205600        MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL          00205600
205700        MOVE GCIO-WRK-PROVISION-ID      TO  GCA-BEN-PROV-ID       00205700
205800        MOVE GCIO-WRK-TABULAR-PROVISION TO                        00205800
205900                                     GCIO-WRK-BENEFIT-PROVISION.  00205900
206000                                                                  00206000
206100                                                                  00206100
206200     MOVE GCIO-WRK-EFFDT-CEN       TO GCA-EFFDT-CEN.              00206200
206300     MOVE GCIO-WRK-EFFECTIVE-DATE  TO HGADATE-JULIAN1.            00206300
206400     PERFORM 9300-000-JULIAN-TO-GREGORIAN.                        00206400
206500     MOVE HGADATE-DATE2            TO GCA-EFFECTIVE-DATE.         00206500
206600     MOVE GC-GCPSWORK-DDNAME       TO GCIO2-FILE-DDNAME.          00206600
206700     MOVE GC-GCIO-AREA-1           TO GCIO2-IO-AREA-TO-USE.       00206700
206800     MOVE TABIDI                   TO GCA-ALL-LEVEL-TAB-ID.       00206800
206900     MOVE TABSLTNI                 TO ACWA-DISPLAY-LEN-7.         00206900
207000     MOVE ACWA-DISPLAY-LEN-7       TO GCA-ALL-LEVEL-TAB-SLOT.     00207000
207100     MOVE GXA-PROVISION-ID         TO GCIO-WRK-TAB-PROVISION-ID,  00207100
207200                                      GCA-INTERNAL-TAB-ID.        00207200
207300     MOVE GXA-INCLUDE-EXCLUDE-IND  TO GCA-I-E-INDC.               00207300
207400     MOVE GXA-PROVISION-SLOT-NO    TO SAVE-COPY-FROM-SLOT.        00207400
207500     MOVE GAA-OCCURS-ENTRY-COUNTER (GAA-INDEX)                    00207500
207600                                   TO GCIO-WRK-TAB-PROV-SLOT-NO   00207600
207700                                      GXA-PROVISION-SLOT-NO       00207700
207800                                      ACWA-DISPLAY-LEN-7.         00207800
207900     MOVE ACWA-DISPLAY-LEN-7       TO GCA-INTERNAL-TAB-SLOT       00207900
208000                                      GCA-OCCURS-ENTRY-COUNTER.   00208000
208100     MOVE 'A'                      TO GCA-ADD-DEL-IND.            00208100
208200     MOVE 'CHG/ADD'                TO DELADD-OPTION.              00208200
208300     MOVE FRMNUIDI                 TO GCA-FROM-MENU-ID.           00208300
208400     MOVE FUNCTONI                 TO GCA-ALL-LEVEL-TAB-FUNC-CODE.00208400
208500     MOVE GCIO-WRK-PLAN-CODE       TO GCA-PLAN-CODE.              00208500
208600     MOVE GCIO-WRK-GROUP-NUM       TO GCA-GROUP-NUM.              00208600
208700     MOVE GCIO-WRK-SECTION-NUM     TO GCA-SECTION-NUM.            00208700
208800     MOVE GCIO-WRK-PKG-CODE        TO GCA-PKG-CODE.               00208800
208900     MOVE GCIO-WRK-FAMILY-RELATION-LVL                            00208900
209000                                   TO GCA-FAM-REL-LVL.            00209000
209100     MOVE SPACES                   TO WORK-RECORD-2.              00209100
209200     MOVE GCIO-WORKFILE-KEY        TO GCIO2-FILE-KEY              00209200
209300                                      WORK-RECORD-KEY-2.          00209300
209400     MOVE SAVE-COPY-FROM-SLOT      TO WRK2-PROV-POOL-COPY-SLOT.   00209400
209500                                                                  00209500
209600     IF FRMNUIDI  =  'GC8A'                                       00209600
209700        MOVE GCIO-WRK-PROVISION-ID TO WRK2-ALL-LEV-BEN-PROV.      00209700
209800                                                                  00209800
209900                                                                  00209900
210000     IF WRK-SIGNAL-FROM-ONLINE  =  'W'                            00210000
210100        MOVE 'W'       TO  WRK2-SIGNAL-FROM-ONLINE                00210100
210200        MOVE '1U'      TO  WRK2-CDE-SP                            00210200
210300        ADD   1        TO  ACWA-CDE-1U-COUNT                      00210300
210400     ELSE                                                         00210400
210500        IF  CDEINDO =  '+CDE+'  OR '+CDE-'                        00210500
210600            MOVE '1U'      TO  WRK2-CDE-SP                        00210600
210700            ADD   1        TO  ACWA-CDE-1U-COUNT                  00210700
210800        ELSE                                                      00210800
210900            MOVE '2 '      TO  WRK2-CDE-SP                        00210900
211000            ADD   1        TO  ACWA-CDE-2B-COUNT.                 00211000
211100                                                                  00211100
211200** SET INDICATOR TO CAPTURE OPERATOR-ID.                          00211200
211300     MOVE '1'          TO  GCIO2-OPER-ID-IND.                     00211300
211400     PERFORM 4700-000-UPDATE-CONTROL-RECORD.                      00211400
211500 2100-100-WRITE-INT-TAB.                                          00211500
211600                                                                  00211600
211700     MOVE GC-GCIO-ACCESS-CODE-WR   TO  GCIO2-FILE-ACCESS-CODE.    00211700
211800                                                                  00211800
211900     COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   00211900
212000              GC-GCIOPARM-LEN  +  GCIO2-RECORD-LENGTH.            00212000
212100                                                                  00212100
212200     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00212200
212300                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00212300
212400                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. 00212400
212500                                                                  00212500
212600     IF NOT GCIO2-GOOD-RETURN                                     00212600
212700        MOVE WS-ABCODE-1BF6        TO WS-ABCODE                   00212700
212800        MOVE WS-ABCODE-1BF6-MSG    TO WS-ABCODE-MSG               00212800
212900        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00212900
213000                                                                  00213000
213100     SET GCA-RECORD-POINTER  TO                                   00213100
213200                    ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.    00213200
213300                                                                  00213300
213400     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00213400
213500                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00213500
213600     EXEC CICS  XCTL  PROGRAM(WS-INTERNAL-TABULAR-PGM-ID)         00213600
213700                COMMAREA(COMMON-WORKAREAS)                        00213700
213800                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00213800
213900                                                                  00213900
214000 2100-900-EXIT.                                                   00214000
214100          EXIT.                                                   00214100
214200/*****************************************************************00214200
214300* 2500  ADD NEW OCCURS                                           *00214300
214400*                                                                *00214400
214500*     INITIALIZE ENTRY IN THE TABLE TO EITHER SPACES OR ZEROS,   *00214500
214600*  THEN IF A FIELD WAS ENTERED BY THE OPERATOR MOVE IT TO THE    *00214600
214700*  TABLE IN THE RECORD.                                          *00214700
214800******************************************************************00214800
214900 2500-000-ADD-NEW-OCCURS        SECTION.                          00214900
215000 2500-010.                                                        00215000
215100                                                                  00215100
215200     MOVE GAA-ENTRY-COUNT  TO GAA-ENTRY-COUNT.                    00215200
215300     SET GAA-INDEX         TO GAA-ENTRY-COUNT.                    00215300
215400     MOVE LOW-VALUES       TO GAA-ENTRY (GAA-INDEX).              00215400
215500                                                                  00215500
215600     MOVE ZEROS TO GAA-BAMA-INTL-TAB-SLOT-2 (GAA-INDEX),          00215600
215700                   GAA-BAMA-INTL-TAB-SLOT-3 (GAA-INDEX),          00215700
215800                   GAA-BAMA-INTL-TAB-SLOT-4 (GAA-INDEX),          00215800
215900                   GAA-BAMA-INTL-TAB-SLOT-5 (GAA-INDEX).          00215900
216000                                                                  00216000
216100                                                                  00216100
216200     MOVE SPACES TO GAA-BAMA-INTL-TAB-TAB-ID-2 (GAA-INDEX)        00216200
216300                    GAA-BAMA-INTL-TAB-TAB-ID-3 (GAA-INDEX)        00216300
216400                    GAA-BAMA-INTL-TAB-TAB-ID-4 (GAA-INDEX)        00216400
216500                    GAA-BAMA-INTL-TAB-TAB-ID-5 (GAA-INDEX).       00216500
216600                                                                  00216600
216700     MOVE GAA-OCC-ENTRY-TAB-SLOT-CNTR                             00216700
216800                    TO GAA-OCCURS-ENTRY-COUNTER       (GAA-INDEX).00216800
216900     ADD 1          TO GAA-OCC-ENTRY-TAB-SLOT-CNTR.               00216900
217000     MOVE DAYFACII  TO GAA-BAMA-DAY-FACTOR-IND        (GAA-INDEX).00217000
217100     MOVE COPAYINI  TO GAA-BAMA-CO-PAY-IND            (GAA-INDEX).00217100
217200     MOVE BISNDINI  TO GAA-BAMA-BISCENDING-IND-RSV    (GAA-INDEX).00217200
217300     MOVE ASCDSCDI  TO GAA-BAMA-ASCEND-DESCEND-IND    (GAA-INDEX).00217300
      *P21595 CHANGES STARTS                                            00217310
           MOVE BENTYPI   TO GAA-BAMA-BEN-TYPE              (GAA-INDEX).00217311
           MOVE TIERCDI   TO GAA-BAMA-TIER-CODE             (GAA-INDEX).00217312
           MOVE TIERLVI   TO GAA-BAMA-TIER-LVL              (GAA-INDEX).00217313
      *P21595 CHANGES ENDS                                              00217320
217400     MOVE FYIVALI   TO GAA-BAMA-FYI-VALUE             (GAA-INDEX).00217400
217500     MOVE CSTCONTI  TO GAA-BAMA-COST-CONTAIN-IND      (GAA-INDEX).00217500
217600     MOVE PERIODI   TO GAA-BAMA-BENEFIT-PERIOD        (GAA-INDEX).00217600
217700     MOVE DEFINTNI  TO GAA-BAMA-DEFINITION            (GAA-INDEX).00217700
217800     MOVE PERTQALI  TO GAA-BAMA-BEN-PER-TIME-QUAL     (GAA-INDEX).00217800
217900     MOVE FAMINDII  TO GAA-BAMA-FAM-OR-INDIV          (GAA-INDEX).00217900
218000     MOVE PLCTRMTI  TO GAA-BAMA-PLACE-OF-TREATMENT    (GAA-INDEX).00218000
218100     MOVE SRVGRUPI  TO GAA-BAMA-SERVICE-GROUP         (GAA-INDEX).00218100
218200                                                                  00218200
218300     MOVE AGELIMLI  TO ACWA-DISPLAY-LEN-3-X.                      00218300
218400     MOVE ACWA-DISPLAY-LEN-3                                      00218400
218500                    TO GAA-BAMA-AGE-LIMIT-FROM        (GAA-INDEX).00218500
218600     MOVE AGELIMHI  TO ACWA-DISPLAY-LEN-3-X.                      00218600
218700     MOVE ACWA-DISPLAY-LEN-3                                      00218700
218800                    TO GAA-BAMA-AGE-LIMIT-TO          (GAA-INDEX).00218800
218900     MOVE FEAKINDI  TO GAA-BAMA-FEAK-IND              (GAA-INDEX).00218900
219000     MOVE ACCUMIDI  TO GAA-BAMA-ACCUMID               (GAA-INDEX).00219000
219100     MOVE CAPINDI   TO GAA-BAMA-COMB-APPLIED-IND      (GAA-INDEX).00219100
219200     MOVE SABDINDI  TO GAA-BAMA-SEL-ADDL-BEN-DET      (GAA-INDEX).00219200
219300     MOVE AGEQLLI   TO GAA-BAMA-AGE-QUAL-IND-FROM     (GAA-INDEX).00219300
219400     MOVE AGEQLHI   TO GAA-BAMA-AGE-QUAL-IND-TO       (GAA-INDEX).00219400
219500     MOVE RELPINDI  TO GAA-BAMA-RELATIONSHIP-IND      (GAA-INDEX).00219500
219600                                                                  00219600
219700     MOVE PRTIMEFI  TO ACWA-DISPLAY-LEN-3-X.                      00219700
219800     MOVE ACWA-DISPLAY-LEN-3                                      00219800
219900                    TO GAA-BAMA-BEN-PER-TIME-FCTR     (GAA-INDEX).00219900
220000     MOVE MAXOVRDI  TO GAA-BAMA-BEN-PER-MAX-OVRD-IND  (GAA-INDEX).00220000
220100     MOVE REININDI  TO GAA-BAMA-REINSTATEMENT-IND     (GAA-INDEX).00220100
220200     MOVE CLMLVLII  TO GAA-BAMA-CLAIM-LVL-ACCUM-IND   (GAA-INDEX).00220200
220300     MOVE INTRVALI  TO ACWA-DISPLAY-LEN-3-X.                      00220300
220400     MOVE ACWA-DISPLAY-LEN-3                                      00220400
220500                    TO GAA-BAMA-INTERVAL-TIME-FCTR    (GAA-INDEX).00220500
220600     MOVE INTTYPEI  TO GAA-BAMA-INTERVAL-TYPE         (GAA-INDEX).00220600
220700     MOVE LOBI      TO GAA-BAMA-L-O-B                 (GAA-INDEX).00220700
220800                                                                  00220800
220900     PERFORM 2600-000-PROCESS-VAL-LIMIT.                          00220900
221000                                                                  00221000
221100     MOVE ACWA-VALUE-LIMIT-9                                      00221100
221200                    TO GAA-BAMA-VALUE-LIMIT           (GAA-INDEX).00221200
221300     MOVE BENVLQLI  TO GAA-BAMA-VALUE-QUALIFIER       (GAA-INDEX).00221300
221400     MOVE ZEROS     TO ACWA-DISPLAY-LEN-3-X.                      00221400
221500     MOVE NEWVALUI  TO ACWA-DISPLAY-LEN-5-X.                      00221500
221600     MOVE ACWA-DISPLAY-LEN-5                                      00221600
221700                    TO GAA-BAMA-INTERVAL-OVRD-VALUE   (GAA-INDEX).00221700
221800     MOVE OVRDINDI  TO GAA-BAMA-INTERVAL-OVRD-IND     (GAA-INDEX).00221800
221900     MOVE INTDESKI  TO GAA-BAMA-INTERNAL-DESCRIPTOR   (GAA-INDEX).00221900
222000     MOVE CONDALLI  TO GAA-COND-ALL-BIT               (GAA-INDEX).00222000
222100     MOVE CONDEXCI  TO GAA-COND-EXCLUSION-BIT         (GAA-INDEX).00222100
222200     MOVE CONDICDI  TO GAA-COND-ICD-BIT               (GAA-INDEX).00222200
222300     MOVE CONDTABI  TO GAA-COND-TB-BIT                (GAA-INDEX).00222300
222400     MOVE CONDMENI  TO GAA-COND-MENTAL-BIT            (GAA-INDEX).00222400
222500     MOVE CONDDRGI  TO GAA-COND-DRUG-BIT              (GAA-INDEX).00222500
222600     MOVE CONDALCI  TO GAA-COND-ALCOHOL-BIT           (GAA-INDEX).00222600
222700     MOVE CONDOBCI  TO GAA-COND-OB-COMP-BIT           (GAA-INDEX).00222700
222800     MOVE CONDOBNI  TO GAA-COND-OB-NORM-BIT           (GAA-INDEX).00222800
222900     MOVE CONDMALI  TO GAA-COND-MALIGNANCY-BIT        (GAA-INDEX).00222900
223000     MOVE CONDCARI  TO GAA-COND-CARDIAC-DISEASE-BIT   (GAA-INDEX).00223000
223100     MOVE CONDOBSI  TO GAA-COND-OBESITY-BIT           (GAA-INDEX).00223100
223200     MOVE CONDKDYI  TO GAA-COND-KIDNEY-DISEASE-BIT    (GAA-INDEX).00223200
223300     MOVE CONDACCI  TO GAA-COND-ACCIDENT-BIT          (GAA-INDEX).00223300
223400     MOVE CONDPECI  TO GAA-COND-PRE-EXIST-BIT         (GAA-INDEX).00223400
223500     MOVE CONDNEMI  TO GAA-COND-NON-EMER-BIT          (GAA-INDEX).00223500
223600     MOVE CONDSUII  TO GAA-COND-SUICIDE-BIT           (GAA-INDEX).00223600
223700     MOVE CONDTMJI  TO GAA-COND-TMJ-BIT               (GAA-INDEX).00223700
223800     MOVE CONDINFI  TO GAA-COND-INF-BIT               (GAA-INDEX).00223800
223900     MOVE CONDLIFI  TO GAA-COND-LIFE-THREAT-BIT       (GAA-INDEX).00223900
224000     MOVE CONDEMCI  TO GAA-COND-EMER-MED-BIT          (GAA-INDEX).00224000
224100     MOVE CONDEACI  TO GAA-COND-EMER-ACC-BIT          (GAA-INDEX).00224100
224200     MOVE CONDSMII  TO GAA-COND-SER-MEN-ILL-BIT       (GAA-INDEX).00224200
224300     MOVE CONDNSMI  TO GAA-COND-NON-SER-MEN-ILL-BIT   (GAA-INDEX).00224300
224400                                                                  00224400
224500     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00224500
224600         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00224600
224700         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00224700
224800         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00224800
224900         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00224900
225000         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00225000
225100     THEN                                                         00225100
225200        MOVE 1               TO GAA-INTERNAL-TABULAR-COUNT        00225200
225300                                                       (GAA-INDEX)00225300
225400        MOVE HIGH-VALUES     TO GAA-BAMA-INTL-TAB-1    (GAA-INDEX)00225400
225500        SET  GAA-INDEX  UP BY  1                                  00225500
225600        MOVE HIGH-VALUES     TO GAA-ENTRY (GAA-INDEX)             00225600
225700        SET GAA-ENTRY-COUNT  TO GAA-INDEX                         00225700
225800        GO TO 2500-900-EXIT.                                      00225800
225900                                                                  00225900
226000                                                                  00226000
226100     IF IBGROPTI  =  'MT' OR 'A'                                  00226100
226200        MOVE 2           TO GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX)00226200
226300        MOVE HIGH-VALUES TO GAA-BAMA-INTL-TAB-2        (GAA-INDEX)00226300
226400        MOVE '#IBGR '    TO GAA-BAMA-INTL-TAB-TAB-ID-1 (GAA-INDEX)00226400
226500        MOVE GAA-OCCURS-ENTRY-COUNTER                  (GAA-INDEX)00226500
226600                         TO GAA-BAMA-INTL-TAB-SLOT-1  (GAA-INDEX).00226600
226700                                                                  00226700
226800     IF IPGNOPTI  =  'MT' OR 'A'                                  00226800
226900        MOVE 2           TO GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX)00226900
227000        MOVE HIGH-VALUES TO GAA-BAMA-INTL-TAB-2        (GAA-INDEX)00227000
227100        MOVE '#IPGN '    TO GAA-BAMA-INTL-TAB-TAB-ID-1 (GAA-INDEX)00227100
227200        MOVE GAA-OCCURS-ENTRY-COUNTER                  (GAA-INDEX)00227200
227300                         TO GAA-BAMA-INTL-TAB-SLOT-1  (GAA-INDEX).00227300
227400                                                                  00227400
227500     IF IPGTOPTI  =  'MT' OR 'A'                                  00227500
227600        MOVE 2           TO GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX)00227600
227700        MOVE HIGH-VALUES TO GAA-BAMA-INTL-TAB-2        (GAA-INDEX)00227700
227800        MOVE '#IPGT '    TO GAA-BAMA-INTL-TAB-TAB-ID-1 (GAA-INDEX)00227800
227900        MOVE GAA-OCCURS-ENTRY-COUNTER                  (GAA-INDEX)00227900
228000                         TO GAA-BAMA-INTL-TAB-SLOT-1  (GAA-INDEX).00228000
228100                                                                  00228100
228200     IF IPGSOPTI  =  'MT' OR 'A'                                  00228200
228300        MOVE 2           TO GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX)00228300
228400        MOVE HIGH-VALUES TO GAA-BAMA-INTL-TAB-2        (GAA-INDEX)00228400
228500        MOVE '#IPGS '    TO GAA-BAMA-INTL-TAB-TAB-ID-1 (GAA-INDEX)00228500
228600        MOVE GAA-OCCURS-ENTRY-COUNTER                  (GAA-INDEX)00228600
228700                         TO GAA-BAMA-INTL-TAB-SLOT-1  (GAA-INDEX).00228700
228800                                                                  00228800
228900     IF IDGDOPTI  =  'MT' OR 'A'                                  00228900
229000        MOVE 2           TO GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX)00229000
229100        MOVE HIGH-VALUES TO GAA-BAMA-INTL-TAB-2        (GAA-INDEX)00229100
229200        MOVE '#IDGD '    TO GAA-BAMA-INTL-TAB-TAB-ID-1 (GAA-INDEX)00229200
229300        MOVE GAA-OCCURS-ENTRY-COUNTER                  (GAA-INDEX)00229300
229400                         TO GAA-BAMA-INTL-TAB-SLOT-1  (GAA-INDEX).00229400
229500                                                                  00229500
229600     IF IPGPOPTI  =  'MT' OR 'A'                                  00229600
229700        MOVE 2           TO GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX)00229700
229800        MOVE HIGH-VALUES TO GAA-BAMA-INTL-TAB-2        (GAA-INDEX)00229800
229900        MOVE '#IPGP '    TO GAA-BAMA-INTL-TAB-TAB-ID-1 (GAA-INDEX)00229900
230000        MOVE GAA-OCCURS-ENTRY-COUNTER                  (GAA-INDEX)00230000
230100                         TO GAA-BAMA-INTL-TAB-SLOT-1  (GAA-INDEX).00230100
230200                                                                  00230200
230300                                                                  00230300
230400*******                                                           00230400
230500*     *                                                           00230500
230600* STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  00230600
230700*     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THE00230700
230800*     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  00230800
230900*******                                                           00230900
231000                                                                  00231000
231100     IF  FRMNUIDI =  'GTM1'  AND                                  00231100
231200         IBGROPTI =  'MT'                                         00231200
231300     THEN                                                         00231300
231400         MOVE MFRMSLTI  TO  IBGRSLTI                              00231400
231500                            ACWA-DISPLAY-LEN-7                    00231500
231600         SET  WT-01-INDEX                     TO +01              00231600
231700         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00231700
231800         MOVE -1        TO  IBGROPTL                              00231800
231900         MOVE DFHBMABF  TO  IBGRSLTA                              00231900
232000                            IBGRIDA                               00232000
232100         MOVE SPACES    TO  IBGROPTI                              00232100
232200         MOVE DFHBMUNP  TO  MFRMSLTA                              00232200
232300         MOVE ZEROS     TO  MFRMSLTI                              00232300
232400                            MFRMSLTL                              00232400
232500         MOVE ACWA-DISPLAY-LEN-7                                  00232500
232600                        TO  GAA-BAMA-INTL-TAB-SLOT-1 (GAA-INDEX)  00232600
232700     ELSE                                                         00232700
232800         NEXT SENTENCE.                                           00232800
232900                                                                  00232900
233000     IF  FRMNUIDI =  'GTM1'  AND                                  00233000
233100         IPGNOPTI =  'MT'                                         00233100
233200     THEN                                                         00233200
233300         MOVE MFRMSLTI  TO  IPGNSLTI                              00233300
233400                            ACWA-DISPLAY-LEN-7                    00233400
233500         SET  WT-01-INDEX                     TO +02              00233500
233600         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00233600
233700         MOVE -1        TO  IPGNOPTL                              00233700
233800         MOVE DFHBMABF  TO  IPGNSLTA                              00233800
233900                            IPGNIDA                               00233900
234000         MOVE SPACES    TO  IPGNOPTI                              00234000
234100         MOVE DFHBMUNP  TO  MFRMSLTA                              00234100
234200         MOVE ZEROS     TO  MFRMSLTI                              00234200
234300                            MFRMSLTL                              00234300
234400         MOVE ACWA-DISPLAY-LEN-7                                  00234400
234500                        TO  GAA-BAMA-INTL-TAB-SLOT-1 (GAA-INDEX)  00234500
234600     ELSE                                                         00234600
234700         NEXT SENTENCE.                                           00234700
234800                                                                  00234800
234900     IF  FRMNUIDI =  'GTM1'  AND                                  00234900
235000         IPGTOPTI =  'MT'                                         00235000
235100     THEN                                                         00235100
235200         MOVE MFRMSLTI  TO  IPGTSLTI                              00235200
235300                            ACWA-DISPLAY-LEN-7                    00235300
235400         SET  WT-01-INDEX                     TO +03              00235400
235500         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00235500
235600         MOVE -1        TO  IPGTOPTL                              00235600
235700         MOVE DFHBMABF  TO  IPGTSLTA                              00235700
235800                            IPGTIDA                               00235800
235900         MOVE SPACES    TO  IPGTOPTI                              00235900
236000         MOVE DFHBMUNP  TO  MFRMSLTA                              00236000
236100         MOVE ZEROS     TO  MFRMSLTI                              00236100
236200                            MFRMSLTL                              00236200
236300         MOVE ACWA-DISPLAY-LEN-7                                  00236300
236400                        TO  GAA-BAMA-INTL-TAB-SLOT-1 (GAA-INDEX)  00236400
236500     ELSE                                                         00236500
236600         NEXT SENTENCE.                                           00236600
236700                                                                  00236700
236800     IF  FRMNUIDI =  'GTM1'  AND                                  00236800
236900         IPGSOPTI =  'MT'                                         00236900
237000     THEN                                                         00237000
237100         MOVE MFRMSLTI  TO  IPGSSLTI                              00237100
237200                            ACWA-DISPLAY-LEN-7                    00237200
237300         SET  WT-01-INDEX                     TO +03              00237300
237400         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00237400
237500         MOVE -1        TO  IPGSOPTL                              00237500
237600         MOVE DFHBMABF  TO  IPGSSLTA                              00237600
237700                            IPGSIDA                               00237700
237800         MOVE SPACES    TO  IPGSOPTI                              00237800
237900         MOVE DFHBMUNP  TO  MFRMSLTA                              00237900
238000         MOVE ZEROS     TO  MFRMSLTI                              00238000
238100                            MFRMSLTL                              00238100
238200         MOVE ACWA-DISPLAY-LEN-7                                  00238200
238300                        TO  GAA-BAMA-INTL-TAB-SLOT-1 (GAA-INDEX)  00238300
238400     ELSE                                                         00238400
238500         NEXT SENTENCE.                                           00238500
238600                                                                  00238600
238700     IF  FRMNUIDI =  'GTM1'  AND                                  00238700
238800         IDGDOPTI =  'MT'                                         00238800
238900     THEN                                                         00238900
239000         MOVE MFRMSLTI  TO  IDGDSLTI                              00239000
239100                            ACWA-DISPLAY-LEN-7                    00239100
239200         SET  WT-01-INDEX                     TO +04              00239200
239300         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00239300
239400         MOVE -1        TO  IDGDOPTL                              00239400
239500         MOVE DFHBMABF  TO  IDGDSLTA                              00239500
239600                            IDGDIDA                               00239600
239700         MOVE SPACES    TO  IDGDOPTI                              00239700
239800         MOVE DFHBMUNP  TO  MFRMSLTA                              00239800
239900         MOVE ZEROS     TO  MFRMSLTI                              00239900
240000                            MFRMSLTL                              00240000
240100         MOVE ACWA-DISPLAY-LEN-7                                  00240100
240200                        TO  GAA-BAMA-INTL-TAB-SLOT-1 (GAA-INDEX)  00240200
240300     ELSE                                                         00240300
240400         NEXT SENTENCE.                                           00240400
240500                                                                  00240500
240600     IF  FRMNUIDI =  'GTM1'  AND                                  00240600
240700         IPGPOPTI =  'MT'                                         00240700
240800     THEN                                                         00240800
240900         MOVE MFRMSLTI  TO  IPGPSLTI                              00240900
241000                            ACWA-DISPLAY-LEN-7                    00241000
241100         SET  WT-01-INDEX                     TO +05              00241100
241200         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00241200
241300         MOVE -1        TO  IPGPOPTL                              00241300
241400         MOVE DFHBMABF  TO  IPGPSLTA                              00241400
241500                            IPGPIDA                               00241500
241600         MOVE SPACES    TO  IPGPOPTI                              00241600
241700         MOVE DFHBMUNP  TO  MFRMSLTA                              00241700
241800         MOVE ZEROS     TO  MFRMSLTI                              00241800
241900                            MFRMSLTL                              00241900
242000         MOVE ACWA-DISPLAY-LEN-7                                  00242000
242100                        TO  GAA-BAMA-INTL-TAB-SLOT-1 (GAA-INDEX)  00242100
242200     ELSE                                                         00242200
242300         NEXT SENTENCE.                                           00242300
242400                                                                  00242400
242500                                                                  00242500
242600*******                                                           00242600
242700* STS *----------------------------------------------------------*00242700
242800*******                                                           00242800
242900                                                                  00242900
243000                                                                  00243000
243100     SET  GAA-INDEX  UP BY  1.                                    00243100
243200     MOVE HIGH-VALUES     TO  GAA-ENTRY (GAA-INDEX).              00243200
243300     SET GAA-ENTRY-COUNT  TO  GAA-INDEX.                          00243300
243400                                                                  00243400
243500 2500-900-EXIT. EXIT.                                             00243500
243600/                                                                 00243600
243700 2600-000-PROCESS-VAL-LIMIT     SECTION.                          00243700
243800 2600-010.                                                        00243800
243900                                                                  00243900
244000     IF (ACWA-VAL-LIM-SCREEN-NEG1-3 = 'NEG' OR                    00244000
244100         ACWA-VAL-LIM-SCREEN-NEG2-3 = 'NEG') OR                   00244100
244000        (ACWA-VAL-LIM-SCREEN-NEG1-3 = 'UNL' OR                    00244110
244100         ACWA-VAL-LIM-SCREEN-NEG2-3 = 'UNL')                      00244120
244200         GO TO 2600-900-EXIT.                                     00244200
244300                                                                  00244300
244400     IF  ACWA-BNMXVALI-N NUMERIC                                  00244400
244500     THEN                                                         00244500
244600         IF  BENVLQLI  = '5'                                      00244600
244700         THEN                                                     00244700
244800             MOVE ACWA-BNMXVALI-N TO ACWA-VALUE-LIMIT-7           00244800
244900             MOVE ZEROS           TO ACWA-VALUE-LIMIT-2           00244900
245000             GO TO 2600-900-EXIT                                  00245000
245100         ELSE                                                     00245100
245200             MOVE ACWA-BNMXVALI-N TO ACWA-VALUE-LIMIT-9-9         00245200
245300             GO TO 2600-900-EXIT                                  00245300
245400     ELSE                                                         00245400
245500         NEXT SENTENCE.                                           00245500
245600                                                                  00245600
245700     IF  ACWA-VAL-LIM-SCREEN-1 = '.'                              00245700
245800         MOVE ACWA-VAL-LIM-SCREEN-7 TO ACWA-VALUE-LIMIT-7         00245800
245900         MOVE ACWA-VAL-LIM-SCREEN-2 TO ACWA-VALUE-LIMIT-2         00245900
246000         GO TO 2600-900-EXIT.                                     00246000
246100                                                                  00246100
246200 2600-900-EXIT. EXIT.                                             00246200
246300/*****************************************************************00246300
246400* 3000 UPDATE GAA RECORD                                         *00246400
246500*                                                                *00246500
246600*    THIS ROUTINE REWRITES THE UPDATED RECORD TO THE WORK FILE.  *00246600
246700******************************************************************00246700
246800 3000-000-UPDATE-GAA-RECORD     SECTION.                          00246800
246900 3000-010.                                                        00246900
247000                                                                  00247000
247100     COMPUTE  GCIO-RECORD-LENGTH   =                              00247100
247200         GC-WORKFILE-KEY-LEN       +                              00247200
247300         GC-GCTABULR-ABM-FIXED-LEN +                              00247300
247400         (GC-GCTABULR-ABM-VARY-LEN * GAA-ENTRY-COUNT).            00247400
247500                                                                  00247500
247600     MOVE  GC-GCIO-ACCESS-CODE-WU TO GCIO-FILE-ACCESS-CODE.       00247600
247700                                                                  00247700
247800     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00247800
247900                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00247900
248000                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)  END-EXEC.     00248000
248100                                                                  00248100
248200     IF NOT GCIO-GOOD-RETURN                                      00248200
248300        MOVE WS-ABCODE-1BF4        TO WS-ABCODE                   00248300
248400        MOVE WS-ABCODE-1BF4-MSG    TO WS-ABCODE-MSG               00248400
248500        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00248500
248600                                                                  00248600
248700 3000-900-EXIT. EXIT.                                             00248700
248800                                                                  00248800
248900/*****************************************************************00248900
249000* 3100  READ RECORD                                              *00249000
249100*                                                                *00249100
249200*    THIS ROUTINE READS THE RECORD THAT CORRESPONDS TO THE KEY   *00249200
249300*  FIELDS FOUND ON THE SCREEN'S HEADING.                         *00249300
249400******************************************************************00249400
249500 3100-000-READ-RECORD           SECTION.                          00249500
249600 3100-010.                                                        00249600
249700                                                                  00249700
249800     COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =                       00249800
249900              GC-GCIOPARM-LEN             +                       00249900
250000              GC-WORKFILE-KEY-LEN         +                       00250000
250100              GC-GCTABULR-ABM-FIXED-LEN   +                       00250100
250200             (GC-GCTABULR-ABM-VARY-LEN    *                       00250200
250300              GC-GCTABULR-ABM-VARY-MAX-OCUR).                     00250300
250400                                                                  00250400
250500                                                                  00250500
250600     IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       00250600
250700        NEXT SENTENCE                                             00250700
250800     ELSE                                                         00250800
250900        EXEC CICS GETMAIN                                         00250900
251000               SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      00251000
251100               INITIMG(WS-HEX-00)                                 00251100
251200               LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 00251200
251300               END-EXEC                                           00251300
251400        SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     00251400
251500                 ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        00251500
251600                                                                  00251600
251700     IF FRMNUIDI  =  'GS3A'                                       00251700
251800        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00251800
251900     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00251900
252000        PERFORM 6100-000-BUILD-CONTRACT-KEY.                      00252000
252100     IF FRMNUIDI  =  'GC8A'                                       00252100
252200        PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      00252200
252300                                                                  00252300
252400     IF GCIO-WORKFILE-KEY  =  WORK-RECORD-KEY                     00252400
252500        GO TO 3100-900-EXIT.                                      00252500
252600                                                                  00252600
252700     MOVE GC-GCPSWORK-DDNAME     TO GCIO-FILE-DDNAME.             00252700
252800     MOVE GC-GCIO-AREA-1         TO GCIO-IO-AREA-TO-USE.          00252800
252900     MOVE GCIO-WORKFILE-KEY      TO GCIO-FILE-KEY.                00252900
253000     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO-FILE-ACCESS-CODE.        00253000
253100                                                                  00253100
253200     MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR  TO  GAA-ENTRY-COUNT.     00253200
253300                                                                  00253300
253400     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00253400
253500                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00253500
253600                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)     END-EXEC.  00253600
253700                                                                  00253700
253800     IF NOT GCIO-GOOD-RETURN                                      00253800
253900        MOVE WS-ABCODE-1BFJ        TO WS-ABCODE                   00253900
254000        MOVE WS-ABCODE-1BFJ-MSG    TO WS-ABCODE-MSG               00254000
254100        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00254100
254200                                                                  00254200
254300 3100-900-EXIT. EXIT.                                             00254300
254400                                                                  00254400
254500/*****************************************************************00254500
254600* 3200  READ REC FOR UPDATE                                      *00254600
254700*                                                                *00254700
254800*    THIS ROUTINE READS THE RECORD FOR UPDATE THAT CORRESPONDS   *00254800
254900*  TO THE KEY FIELDS FOUND ON THE SCREEN'S HEADING.              *00254900
255000******************************************************************00255000
255100 3200-000-READ-REC-FOR-UPDATE   SECTION.                          00255100
255200 3200-010.                                                        00255200
255300                                                                  00255300
255400     COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =                       00255400
255500              GC-GCIOPARM-LEN             +                       00255500
255600              GC-WORKFILE-KEY-LEN         +                       00255600
255700              GC-GCTABULR-ABM-FIXED-LEN   +                       00255700
255800             (GC-GCTABULR-ABM-VARY-LEN    *                       00255800
255900              GC-GCTABULR-ABM-VARY-MAX-OCUR).                     00255900
256000                                                                  00256000
256100     IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       00256100
256200        NEXT SENTENCE                                             00256200
256300     ELSE                                                         00256300
256400        EXEC CICS GETMAIN                                         00256400
256500               SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      00256500
256600               INITIMG(WS-HEX-00)                                 00256600
256700               LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 00256700
256800               END-EXEC                                           00256800
256900        SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     00256900
257000                 ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        00257000
257100                                                                  00257100
257200     IF FRMNUIDI  =  'GS3A'                                       00257200
257300        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00257300
257400     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00257400
257500        PERFORM 6100-000-BUILD-CONTRACT-KEY.                      00257500
257600     IF FRMNUIDI  =  'GC8A'                                       00257600
257700        PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      00257700
257800                                                                  00257800
257900     MOVE GC-GCPSWORK-DDNAME     TO GCIO-FILE-DDNAME.             00257900
258000     MOVE GC-GCIO-AREA-1         TO GCIO-IO-AREA-TO-USE.          00258000
258100     MOVE GCIO-WORKFILE-KEY      TO GCIO-FILE-KEY.                00258100
258200     MOVE GC-GCIO-ACCESS-CODE-RU TO GCIO-FILE-ACCESS-CODE.        00258200
258300                                                                  00258300
258400     MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR  TO  GAA-ENTRY-COUNT.     00258400
258500                                                                  00258500
258600     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00258600
258700                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00258700
258800                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)  END-EXEC.     00258800
258900                                                                  00258900
259000 3200-900-EXIT. EXIT.                                             00259000
259100/*****************************************************************00259100
259200* 4000  DISPLAY FIRST SCREEN                                     *00259200
259300*                                                                *00259300
259400*    THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM ONE OF THE*00259400
259500*  TABULAR MENUS, THE MENU WILL READ THE ALL LEVEL TABULAR IF IT *00259500
259600*  EXISTS (IF IT DOESN'T EXIST THE MENU WILL ADD A NEW ONE TO THE*00259600
259700*  WORK FILE) THEN PLACE THE ADDRESS OF THE TABULAR RECORD WITHIN*00259700
259800*  A COMMON AREA PARAMETER LIST.  THE MENU THEN MOVES THE KEY    *00259800
259900*  FIELDS TO THE COMMON AREA AND PASSES THE ADDRESS OF THE       *00259900
260000*  PARAMETER LIST IN A FULLWORD TO THIS PROGRAM.                 *00260000
260100*    WE THEN SET THIS ADDRESS INTO A BLL CELL AND ACCESS THE     *00260100
260200*  INFORMATION NEEDED TO BUILD THE SCREEN IMAGE.                 *00260200
260300******************************************************************00260300
260400 4000-000-DISPLAY-FIRST-SCREEN  SECTION.                          00260400
260500 4000-010.                                                        00260500
260600                                                                  00260600
260700     IF EIBCALEN  >  0                                            00260700
260800        NEXT SENTENCE                                             00260800
260900     ELSE                                                         00260900
261000        MOVE WS-ABCODE-1BC1        TO WS-ABCODE                   00261000
261100        MOVE WS-ABCODE-1BC1-MSG    TO WS-ABCODE-MSG               00261100
261200        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00261200
261300                                                                  00261300
261400*    SET ADDRESS OF COMMUNICATION-KEY-AREA  TO                    00261400
261500*                   COMMAREA-RECORD-POINTER.                      00261500
261600                                                                  00261600
261700*    SET  ACWA-COMM-KEY-PNTR       TO                             00261700
261800*                   ADDRESS OF COMMUNICATION-KEY-AREA.            00261800
261900     MOVE LOW-VALUES               TO GA1XI01I.                   00261900
262000                                                                  00262000
262100     MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         00262100
262200                                                                  00262200
262300     IF GCA-FROM-MENU-ID  =  'GS3A'                               00262300
262400        MOVE GCA-PLAN-CODE             TO GRP-SPEC-PLAN-CODE      00262400
262500        MOVE GCA-GROUP-NUM             TO GRP-SPEC-GROUP-NUM      00262500
262600        MOVE GCA-SECTION-NUM           TO GRP-SPEC-SECTION-NUM    00262600
262700        MOVE GCA-PKG-CODE              TO GRP-SPEC-PKG-CODE       00262700
262800        MOVE GCA-FAM-REL-LVL           TO GRP-SPEC-FAM-REL-LVL    00262800
262900        MOVE GCA-EFFECTIVE-DATE        TO GRP-SPEC-EFF-DATE       00262900
263000        MOVE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'         00263000
263100                                       TO TTLELNEO                00263100
263200*AB*****MOVE GROUP-SPECIFIC-TITLE-LINE TO TTLELNEO                00263200
263300        MOVE GROUP-SPECIFIC-ID-LINE    TO IDLINEO.                00263300
263400                                                                  00263400
263500     IF GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                     00263500
263600        MOVE GCA-PLAN-CODE             TO CONTRACT-PLAN-CODE      00263600
263700        MOVE GCA-GROUP-NUM             TO CONTRACT-GROUP-NUM      00263700
263800        MOVE GCA-SECTION-NUM           TO CONTRACT-SECTION-NUM    00263800
263900        MOVE GCA-PKG-CODE              TO CONTRACT-PKG-CODE       00263900
264000        MOVE GCA-L-O-B                 TO CONTRACT-LOB            00264000
264100        MOVE GCA-PROV-CTL              TO CONTRACT-PROV-CTL       00264100
264200        MOVE GCA-FAM-REL-LVL           TO CONTRACT-FAM-REL-LVL    00264200
264300        MOVE GCA-EFFECTIVE-DATE        TO CONTRACT-EFF-DATE       00264300
264400        MOVE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'          00264400
264500                                       TO TTLELNEO                00264500
264600*AB*****MOVE CONTRACT-TITLE-LINE       TO TTLELNEO                00264600
264700        MOVE CONTRACT-ID-LINE          TO IDLINEO.                00264700
264800                                                                  00264800
264900     IF GCA-FROM-MENU-ID  =  'GC8A'                               00264900
265000        MOVE GCA-PLAN-CODE             TO BEN-PROV-PLAN-CODE      00265000
265100        MOVE GCA-GROUP-NUM             TO BEN-PROV-GROUP-NO       00265100
265200        MOVE GCA-SECTION-NUM           TO BEN-PROV-SECTION-NO     00265200
265300        MOVE GCA-PKG-CODE              TO BEN-PROV-PKG-CODE       00265300
265400        MOVE GCA-L-O-B                 TO BEN-PROV-LOB            00265400
265500        MOVE GCA-PROV-CTL              TO BEN-PROV-PROV-CTL       00265500
265600        MOVE GCA-FAM-REL-LVL           TO BEN-PROV-FAM-REL-LVL    00265600
265700        MOVE GCA-EFFECTIVE-DATE        TO BEN-PROV-EFF-DATE       00265700
265800        MOVE GCA-BEN-PROV-ID           TO BEN-PROV-ID-NO          00265800
265900        MOVE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'         00265900
266000                                       TO  TTLELNEO               00266000
266100*AB*****MOVE BENEFIT-PROVISION-TITLE-LINE TO  TTLELNEO            00266100
266200        MOVE BENEFIT-PROVISION-ID-LINE TO IDLINEO.                00266200
266300                                                                  00266300
266400     MOVE 'GA1B'                       TO FUNCTONO.               00266400
266500     MOVE '001B00'                     TO SCRNIDNO.               00266500
266600     MOVE ABM-TITLE-LINE               TO TITLEO.                 00266600
266700                                                                  00266700
266800     MOVE GCA-RECORD-POINTER-COMP  TO ACWA-WF-ALL-LEVEL-TAB-COMP. 00266800
266900     SET ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD  TO             00266900
267000                    GCA-RECORD-POINTER.                           00267000
267100                                                                  00267100
267200     IF  NOT WRK-STAT-CONT-MAINT AND                              00267200
267300         NOT WRK-STAT-GRP-SPEC-MAINT                              00267300
267400     THEN                                                         00267400
267500         MOVE WS-ABCODE-1BC2        TO WS-ABCODE                  00267500
267600         MOVE WS-ABCODE-1BC2-MSG    TO WS-ABCODE-MSG              00267600
267700         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00267700
267800                                                                  00267800
267900     IF  NOT WRK-REC-GROUP-SPEC-TAB AND                           00267900
268000         NOT WRK-REC-CONT-TAB       AND                           00268000
268100         NOT WRK-REC-CONT-BEN-TAB-PROV                            00268100
268200     THEN                                                         00268200
268300         MOVE WS-ABCODE-1BC3        TO WS-ABCODE                  00268300
268400         MOVE WS-ABCODE-1BC3-MSG    TO WS-ABCODE-MSG              00268400
268500         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00268500
268600                                                                  00268600
268700     MOVE GAA-ENTRY-COUNT         TO  GAA-ENTRY-COUNT.            00268700
268800     MOVE GCA-ALL-LEVEL-TAB-ID    TO  TABIDO.                     00268800
268900     MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  TABSLTNO.                   00268900
269000     SET GAA-INDEX  TO  1.                                        00269000
269100                                                                  00269100
269200     IF EIBTRNID  =  'GC4A' OR  'GC3A' OR  'GC8A'  OR 'GTM1'      00269200
269300        MOVE '0000000'  TO GCA-OCCURS-ENTRY-COUNTER.              00269300
269400                                                                  00269400
269500     IF GCA-OCCURS-ENTRY-COUNTER  =  '0000000'                    00269500
269600        GO TO 4000-200-BUILD-SCREEN.                              00269600
269700                                                                  00269700
269800     MOVE GCA-OCCURS-ENTRY-COUNTER  TO  ACWA-DISPLAY-LEN-7.       00269800
269900                                                                  00269900
270000 4000-100-FIND-RIGHT-OCCURS.                                      00270000
270100     IF GAA-BAMA-BENEFIT-PERIOD(GAA-INDEX)  NOT = HIGH-VALUES AND 00270100
270200        GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX) NOT =                 00270200
270300                                                ACWA-DISPLAY-LEN-700270300
270400     THEN                                                         00270400
270500         IF GAA-INDEX  <  GAA-ENTRY-COUNT                         00270500
270600            SET GAA-INDEX  UP BY  1                               00270600
270700            GO TO 4000-100-FIND-RIGHT-OCCURS                      00270700
270800         ELSE                                                     00270800
270900             MOVE WS-ABCODE-1BL4        TO WS-ABCODE              00270900
271000             MOVE WS-ABCODE-1BL4-MSG    TO WS-ABCODE-MSG          00271000
271100             MOVE -1                    TO MFRMSLTL               00271100
271200             PERFORM 9800-000-ERROR-MSG-THEN-ABEND                00271200
271300     ELSE                                                         00271300
271400         NEXT SENTENCE.                                           00271400
271500                                                                  00271500
271600                                                                  00271600
271700 4000-200-BUILD-SCREEN.                                           00271700
271800                                                                  00271800
271900     IF  GCA-ADD-DEL-IND  =  'A' OR                               00271900
272000         GAA-ENTRY-COUNT  =  1                                    00272000
272100     THEN                                                         00272100
272200         MOVE 'CHG/ADD'  TO  DELADDO                              00272200
272300         MOVE DFHBMASD   TO  DLOPTLTA,  DELOPTNA                  00272300
272400         IF  GCA-OCCURS-ENTRY-COUNTER  =  '0000000'               00272400
272500         THEN                                                     00272500
272600             PERFORM 4100-000-DISPLAY-SKELETON                    00272600
272700         ELSE                                                     00272700
272800             PERFORM 4400-000-BUILD-DISPLAY                       00272800
272900     ELSE                                                         00272900
273000         MOVE 'CHG/DEL'  TO  DELADDO                              00273000
273100         MOVE 'D'        TO  DELOLITO                             00273100
273200         PERFORM 4400-000-BUILD-DISPLAY.                          00273200
273300                                                                  00273300
273400 4000-900-EXIT. EXIT.                                             00273400
273500/*****************************************************************00273500
273600* 4100  DISPLAY SKELETON                                         *00273600
273700*                                                                *00273700
273800*    THIS ROUTINE REINITIALIZES THE SCREEN FOR THE OPERATOR      *00273800
273900*  AFTER THEY HAVE REVIEWED THE ENTRY THEY JUST ADDED AND        *00273900
274000*  INDICATED THAT THEY WANTED TO ADD MORE BY KEYING 'ENTER'.     *00274000
274100******************************************************************00274100
274200 4100-000-DISPLAY-SKELETON      SECTION.                          00274200
274300 4100-010.                                                        00274300
274400                                                                  00274400
274500     MOVE SPACES TO ERRMSGO.                                      00274500
274600                                                                  00274600
274700     MOVE DFHBMFSE                                                00274700
274800       TO PERIODA.                                                00274800
274900                                                                  00274900
275000     MOVE DFHBMUNP                                                00275000
275100       TO BENVLQLA FAMINDIA  INTDESKA  LOBA                       00275100
275200          IBGROPTA IPGNOPTA  IPGTOPTA  MFRMSLTA                   00275200
275300          IDGDOPTA IPGPOPTA  IPGSOPTA.                            00275300
275400                                                                  00275400
275500     MOVE ALL '_'                                                 00275500
275600       TO PERIODO  BENVLQLO  LOBO      FAMINDIO  PLCTRMTO.        00275600
275700                                                                  00275700
275800     MOVE LOW-VALUES                                              00275800
275900       TO INTDESKO IBGROPTO  IPGNOPTO  IPGTOPTO  MFRMSLTO         00275900
276000          IDGDOPTO IPGPOPTO  IPGSOPTO.                            00276000
276100                                                                  00276100
276200     MOVE ZEROS                                                   00276200
276300       TO COPAYINO CSTCONTO  PERTQALO  AGEQLLO AGEQLHO CONDLIFO   00276300
276400          DAYFACIO SRVGRUPO  PRTIMEFO  MAXOVRDO   CONDTMJO        00276400
276500          REININDO CLMLVLIO  INTRVALO  INTTYPEO  BNMXVALO         00276500
276600          FYIVALO  OVRDINDO  NEWVALUO  DEFINTNO   CONDINFO        00276600
276700          CONDALLO CONDEXCO  CONDICDO  CONDTABO  CONDMENO         00276700
276800          CONDDRGO CONDALCO  CONDOBNO  CONDOBCO  CONDMALO         00276800
276900          CONDCARO CONDOBSO  CONDKDYO  CONDACCO  CONDSUIO         00276900
277000          CONDEMCO  CONDEACO CONDSMIO  CONDNSMO ASCDSCDO BISNDINO 00277000
277100          PRTIMEFO INTRVALO  CONDPECO  CONDNEMO  NEWVALUO BENTYPO 00277100
277200          OENTCTRO IBGRSLTO  IPGNSLTO  IPGTSLTO  TOCURANO TIERCDO 00277200
277300          IDGDSLTO IPGPSLTO  AGELIMLO  AGELIMHO  RELPINDO TIERLVO 00277300
277400          FEAKINDO IPGSSLTO  ACCUMIDO  CAPINDO   SABDINDO.        00277400
277500                                                                  00277500
SI0724*    MOVE '01'    TO  COCURANO.                                   00277600
SI0724     MOVE '001'   TO  COCURANO.                                   00277610
277700     MOVE -1      TO  PERIODL.                                    00277700
277800                                                                  00277800
277900     PERFORM 9000-000-SEND-ERASE-RETURN.                          00277900
278000                                                                  00278000
278100 4100-900-EXIT. EXIT.                                             00278100
278200/*****************************************************************00278200
278300* 4200 DISPLAY NEXT                                              *00278300
278400*                                                                *00278400
278500*    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *00278500
278600*  SCREEN'S DISPLAY AND THEN POSITION TO THE NEXT ENTRY, IF THE  *00278600
278700*  NEXT ENTRY IS THE LAST IN THE LIST THE CODE WILL RECOGNIZE    *00278700
278800*  THAT AND POSITION TO THE FIRST ENTRY, ALSO DISPLAYING AN      *00278800
278900*  INFORMATIONAL MESSAGE.                                        *00278900
279000******************************************************************00279000
279100 4200-000-DISPLAY-NEXT          SECTION.                          00279100
279200 4200-010.                                                        00279200
279300                                                                  00279300
279400     MOVE GAA-ENTRY-COUNT  TO  GAA-ENTRY-COUNT.                   00279400
279500     SET GAA-INDEX         TO  1.                                 00279500
279600     MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                00279600
279700                                                                  00279700
279800                                                                  00279800
279900     SET  CURNT-OCURS-BIN  TO  GAA-INDEX.                         00279900
280000     MOVE CURNT-OCURS-BIN  TO  CURNT-OCURS-PKD.                   00280000
280100     MOVE CURNT-OCCURS-OUT TO  COCURANO.                          00280100
280200                                                                  00280200
280300     IF GAA-ENTRY-COUNT > 1                                       00280300
280400        COMPUTE  TOTAL-OCURS-UNK =  GAA-ENTRY-COUNT  - 1          00280400
280500        MOVE  TOTAL-OCCURS-OUT TO TOCURANO                        00280500
280600     ELSE                                                         00280600
280700        MOVE  '01'             TO TOCURANO.                       00280700
280800                                                                  00280800
280900                                                                  00280900
281000 4200-100-FIND-RIGHT-OCCURS.                                      00281000
281100                                                                  00281100
281200     IF  GAA-BAMA-BENEFIT-PERIOD(GAA-INDEX)  NOT = HIGH-VALUES AND00281200
281300         GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX) NOT =                00281300
281400                                                ACWA-DISPLAY-LEN-700281400
281500     THEN                                                         00281500
281600         IF  GAA-INDEX  <  (GAA-ENTRY-COUNT - 1)                  00281600
281700         THEN                                                     00281700
281800             SET GAA-INDEX  UP BY  1                              00281800
281900             GO TO 4200-100-FIND-RIGHT-OCCURS                     00281900
282000         ELSE                                                     00282000
282100             SET GAA-INDEX  TO  1                                 00282100
282200             SET  WT-01-INDEX                     TO +20          00282200
282300             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00282300
282400     ELSE                                                         00282400
282500         IF  GAA-INDEX  <  (GAA-ENTRY-COUNT - 1)                  00282500
282600         THEN                                                     00282600
282700             SET GAA-INDEX  UP BY  1                              00282700
282800         ELSE                                                     00282800
282900             SET GAA-INDEX  TO  1                                 00282900
283000             SET  WT-01-INDEX                     TO +20          00283000
283100             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.     00283100
283200                                                                  00283200
283300     PERFORM 4400-000-BUILD-DISPLAY.                              00283300
283400                                                                  00283400
283500 4200-900-EXIT. EXIT.                                             00283500
283600/*****************************************************************00283600
283700* 4300 DISPLAY PREV                                              *00283700
283800*                                                                *00283800
283900*    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *00283900
284000*  SCREEN'S DISPLAY AND THEN POSITION TO THE NEXT PREVIOUS ENTRY,*00284000
284100*  IF THE CURRENT ENTRY IS THE FIRST IN THE LIST THE CODE WILL   *00284100
284200*  RECOGNIZE THAT AND POSITION TO THE LAST ENTRY, ALSO DISPLAYING*00284200
284300*  AN INFORMATIONAL MESSAGE.                                     *00284300
284400******************************************************************00284400
284500 4300-000-DISPLAY-PREV          SECTION.                          00284500
284600 4300-010.                                                        00284600
284700                                                                  00284700
284800     MOVE GAA-ENTRY-COUNT  TO  GAA-ENTRY-COUNT.                   00284800
284900     SET  GAA-INDEX        TO  1.                                 00284900
285000     MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                00285000
285100                                                                  00285100
285200 4300-100-FIND-RIGHT-OCCURS.                                      00285200
285300                                                                  00285300
285400     IF  GAA-BAMA-BENEFIT-PERIOD(GAA-INDEX)  NOT = HIGH-VALUES AND00285400
285500         GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX) NOT =                00285500
285600                                                ACWA-DISPLAY-LEN-700285600
285700     THEN                                                         00285700
285800         IF  GAA-INDEX  <  (GAA-ENTRY-COUNT - 1)                  00285800
285900         THEN                                                     00285900
286000             SET GAA-INDEX  UP BY  1                              00286000
286100             GO TO 4300-100-FIND-RIGHT-OCCURS                     00286100
286200         ELSE                                                     00286200
286300             SET GAA-INDEX  TO  1                                 00286300
286400             SET  WT-01-INDEX                     TO +20          00286400
286500             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00286500
286600     ELSE                                                         00286600
286700         IF  GAA-INDEX  NOT =  1                                  00286700
286800         THEN                                                     00286800
286900             SET GAA-INDEX  DOWN BY  1                            00286900
287000         ELSE                                                     00287000
287100             SET GAA-INDEX  TO  GAA-ENTRY-COUNT                   00287100
287200             SET GAA-INDEX  DOWN BY  1                            00287200
287300             SET  WT-01-INDEX                     TO +21          00287300
287400             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.     00287400
287500                                                                  00287500
287600     PERFORM 4400-000-BUILD-DISPLAY.                              00287600
287700                                                                  00287700
287800 4300-900-EXIT. EXIT.                                             00287800
287900/*****************************************************************00287900
288000* 4400 BUILD DISPLAY                                             *00288000
288100*                                                                *00288100
288200*    THIS ROUTINE WILL MOVE ALL THE FIELDS FROM THE OCCURENCE    *00288200
288300*  SPECIFIED BY INDEX GAA-INDEX TO THE SCREEN.                   *00288300
288400******************************************************************00288400
288500 4400-000-BUILD-DISPLAY         SECTION.                          00288500
288600 4400-010.                                                        00288600
288700                                                                  00288700
288800     IF  DELADDI  =  'CHG/DEL'                                    00288800
288900     THEN                                                         00288900
289000         MOVE 'D'  TO  DELOLITO                                   00289000
289100     ELSE                                                         00289100
289200         MOVE SPACE  TO  DELOLITO.                                00289200
289300                                                                  00289300
289400     MOVE SPACE                                       TO DELOPTNO.00289400
289500     MOVE GAA-OCCURS-ENTRY-COUNTER       (GAA-INDEX)  TO          00289500
289600                                               ACWA-DISPLAY-LEN-7.00289600
289700     MOVE ACWA-DISPLAY-LEN-7                          TO OENTCTRO.00289700
289800     MOVE GAA-BAMA-BISCENDING-IND-RSV    (GAA-INDEX)  TO BISNDINO.00289800
289900     MOVE GAA-BAMA-ASCEND-DESCEND-IND    (GAA-INDEX)  TO ASCDSCDO.00289900
      *P21595 CHANGES STARTS                                            00289910
           MOVE GAA-BAMA-BEN-TYPE              (GAA-INDEX)  TO BENTYPO. 00289920
           MOVE GAA-BAMA-TIER-CODE             (GAA-INDEX)  TO TIERCDO. 00289930
           MOVE GAA-BAMA-TIER-LVL              (GAA-INDEX)  TO TIERLVO. 00289940
      *P21595 CHANGES ENDS                                              00289950
290000     MOVE GAA-BAMA-DAY-FACTOR-IND        (GAA-INDEX)  TO DAYFACIO.00290000
290100     MOVE GAA-BAMA-CO-PAY-IND            (GAA-INDEX)  TO COPAYINO.00290100
290200     MOVE GAA-BAMA-COST-CONTAIN-IND      (GAA-INDEX)  TO CSTCONTO.00290200
290300     MOVE GAA-BAMA-BENEFIT-PERIOD        (GAA-INDEX)  TO PERIODO. 00290300
290400     MOVE GAA-BAMA-DEFINITION            (GAA-INDEX)  TO DEFINTNO.00290400
290500     MOVE GAA-BAMA-BEN-PER-TIME-QUAL     (GAA-INDEX)  TO PERTQALO.00290500
290600     MOVE GAA-BAMA-FAM-OR-INDIV          (GAA-INDEX)  TO FAMINDIO.00290600
290700     MOVE GAA-BAMA-PLACE-OF-TREATMENT    (GAA-INDEX)  TO PLCTRMTO.00290700
290800     MOVE GAA-BAMA-SERVICE-GROUP         (GAA-INDEX)  TO SRVGRUPO.00290800
290900     MOVE GAA-BAMA-BEN-PER-TIME-FCTR     (GAA-INDEX)  TO          00290900
291000                                               ACWA-DISPLAY-LEN-3.00291000
291100     MOVE ACWA-DISPLAY-LEN-3                          TO PRTIMEFO.00291100
291200     MOVE GAA-BAMA-BEN-PER-MAX-OVRD-IND  (GAA-INDEX)  TO MAXOVRDO.00291200
291300     MOVE GAA-BAMA-REINSTATEMENT-IND     (GAA-INDEX)  TO REININDO.00291300
291400     MOVE GAA-BAMA-CLAIM-LVL-ACCUM-IND   (GAA-INDEX)  TO CLMLVLIO.00291400
291500                                                                  00291500
291600     MOVE GAA-BAMA-AGE-LIMIT-FROM        (GAA-INDEX)  TO          00291600
291700                                               ACWA-DISPLAY-LEN-3.00291700
291800     MOVE ACWA-DISPLAY-LEN-3                          TO AGELIMLO.00291800
291900     MOVE GAA-BAMA-AGE-LIMIT-TO          (GAA-INDEX)  TO          00291900
292000                                               ACWA-DISPLAY-LEN-3.00292000
292100     MOVE ACWA-DISPLAY-LEN-3                          TO AGELIMHO.00292100
292200     MOVE GAA-BAMA-FEAK-IND              (GAA-INDEX)  TO FEAKINDO.00292200
292300     MOVE GAA-BAMA-ACCUMID               (GAA-INDEX)  TO ACCUMIDO.00292300
292400     MOVE GAA-BAMA-COMB-APPLIED-IND      (GAA-INDEX)  TO CAPINDO. 00292400
292500     MOVE GAA-BAMA-SEL-ADDL-BEN-DET      (GAA-INDEX)  TO SABDINDO.00292500
292600     MOVE GAA-BAMA-AGE-QUAL-IND-FROM     (GAA-INDEX)  TO AGEQLLO. 00292600
292700     MOVE GAA-BAMA-AGE-QUAL-IND-TO       (GAA-INDEX)  TO AGEQLHO. 00292700
292800     MOVE GAA-BAMA-RELATIONSHIP-IND      (GAA-INDEX)  TO RELPINDO.00292800
292900                                                                  00292900
293000     MOVE GAA-BAMA-INTERVAL-TIME-FCTR    (GAA-INDEX)  TO          00293000
293100                                               ACWA-DISPLAY-LEN-3.00293100
293200     MOVE ACWA-DISPLAY-LEN-3                          TO INTRVALO.00293200
293300     MOVE GAA-BAMA-INTERVAL-TYPE         (GAA-INDEX)  TO INTTYPEO.00293300
293400     MOVE GAA-BAMA-L-O-B                 (GAA-INDEX)  TO LOBO.    00293400
293500     MOVE GAA-BAMA-VALUE-LIMIT           (GAA-INDEX)  TO          00293500
293600                                               ACWA-VALUE-LIMIT-9.00293600
293700     IF  ACWA-VALUE-LIMIT-9-9 = -1                                00293700
293800     THEN                                                         00293800
293900         MOVE 'NEG' TO BNMXVALO                                   00293900
294000     ELSE                                                         00294000
293700     IF  ACWA-VALUE-LIMIT-9-9 = -2                                00294010
293800     THEN                                                         00294020
293900         MOVE 'UNL' TO BNMXVALO                                   00294030
294000     ELSE                                                         00294040
294100         IF  GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'           00294100
294200         THEN                                                     00294200
294300             MOVE ACWA-VALUE-LIMIT-9    TO ACWA-EDIT-VALUE-LIMIT  00294300
294400             MOVE ACWA-EDIT-VALUE-LIMIT TO BNMXVALO               00294400
294500         ELSE                                                     00294500
294600             MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX) TO             00294600
294700                                              ACWA-DISPLAY-LEN-9-200294700
294800             MOVE ACWA-DISPLAY-LEN-9-X     TO ACWA-DISPLAY-9      00294800
294900             MOVE SPACES                   TO ACWA-DISPLAY-1      00294900
295000             MOVE ACWA-DISPLAY-VALUE-LIMIT TO BNMXVALO.           00295000
295100                                                                  00295100
295200     MOVE GAA-BAMA-VALUE-QUALIFIER       (GAA-INDEX)  TO BENVLQLO.00295200
295300     MOVE GAA-BAMA-INTERVAL-OVRD-VALUE   (GAA-INDEX)  TO          00295300
295400                                               ACWA-DISPLAY-LEN-5.00295400
295500     MOVE ACWA-DISPLAY-LEN-5                          TO NEWVALUO.00295500
295600     MOVE GAA-BAMA-INTERVAL-OVRD-IND     (GAA-INDEX)  TO OVRDINDO.00295600
295700     MOVE GAA-BAMA-INTERNAL-DESCRIPTOR   (GAA-INDEX)  TO INTDESKO.00295700
295800     MOVE GAA-COND-ALL-BIT               (GAA-INDEX)  TO CONDALLO.00295800
295900     MOVE GAA-COND-EXCLUSION-BIT         (GAA-INDEX)  TO CONDEXCO.00295900
296000     MOVE GAA-COND-ICD-BIT               (GAA-INDEX)  TO CONDICDO.00296000
296100     MOVE GAA-COND-TB-BIT                (GAA-INDEX)  TO CONDTABO.00296100
296200     MOVE GAA-COND-MENTAL-BIT            (GAA-INDEX)  TO CONDMENO.00296200
296300     MOVE GAA-COND-DRUG-BIT              (GAA-INDEX)  TO CONDDRGO.00296300
296400     MOVE GAA-COND-ALCOHOL-BIT           (GAA-INDEX)  TO CONDALCO.00296400
296500     MOVE GAA-COND-OB-COMP-BIT           (GAA-INDEX)  TO CONDOBCO.00296500
296600     MOVE GAA-COND-OB-NORM-BIT           (GAA-INDEX)  TO CONDOBNO.00296600
296700     MOVE GAA-COND-MALIGNANCY-BIT        (GAA-INDEX)  TO CONDMALO.00296700
296800     MOVE GAA-COND-CARDIAC-DISEASE-BIT   (GAA-INDEX)  TO CONDCARO.00296800
296900     MOVE GAA-COND-OBESITY-BIT           (GAA-INDEX)  TO CONDOBSO.00296900
297000     MOVE GAA-COND-KIDNEY-DISEASE-BIT    (GAA-INDEX)  TO CONDKDYO.00297000
297100     MOVE GAA-COND-ACCIDENT-BIT          (GAA-INDEX)  TO CONDACCO.00297100
297200     MOVE GAA-COND-PRE-EXIST-BIT         (GAA-INDEX)  TO CONDPECO.00297200
297300     MOVE GAA-COND-NON-EMER-BIT          (GAA-INDEX)  TO CONDNEMO.00297300
297400     MOVE GAA-COND-SUICIDE-BIT           (GAA-INDEX)  TO CONDSUIO.00297400
297500     MOVE GAA-COND-TMJ-BIT               (GAA-INDEX)  TO CONDTMJO.00297500
297600     MOVE GAA-COND-INF-BIT               (GAA-INDEX)  TO CONDINFO.00297600
297700     MOVE GAA-COND-LIFE-THREAT-BIT       (GAA-INDEX)  TO CONDLIFO.00297700
297800     MOVE GAA-COND-EMER-MED-BIT          (GAA-INDEX)  TO CONDEMCO.00297800
297900     MOVE GAA-COND-EMER-ACC-BIT          (GAA-INDEX)  TO CONDEACO.00297900
298000     MOVE GAA-COND-SER-MEN-ILL-BIT       (GAA-INDEX)  TO CONDSMIO.00298000
298100     MOVE GAA-COND-NON-SER-MEN-ILL-BIT   (GAA-INDEX)  TO CONDNSMO.00298100
298200                                                                  00298200
298300     MOVE -1  TO PERIODL.                                         00298300
298400                                                                  00298400
298500     MOVE GAA-BAMA-FYI-VALUE (GAA-INDEX) TO FYIVALO.              00298500
298600     SET  CURNT-OCURS-BIN                TO GAA-INDEX.            00298600
298700     MOVE CURNT-OCURS-BIN                TO CURNT-OCURS-PKD.      00298700
298800     MOVE CURNT-OCCURS-OUT               TO COCURANO.             00298800
298900                                                                  00298900
299000     IF GAA-ENTRY-COUNT > 1                                       00299000
299100     THEN                                                         00299100
299200         COMPUTE  TOTAL-OCURS-UNK = GAA-ENTRY-COUNT  - 1          00299200
299300         MOVE  TOTAL-OCCURS-OUT TO  TOCURANO                      00299300
299400     ELSE                                                         00299400
299500         MOVE  '01'             TO  TOCURANO.                     00299500
299600                                                                  00299600
299700                                                                  00299700
299800     MOVE ZEROS TO IBGRSLTO,  IPGNSLTO,  IPGTSLTO                 00299800
299900                   IDGDSLTO,  IPGPSLTO,  IPGSSLTO.                00299900
300000                                                                  00300000
300100     SET GAA-INT-INDEX TO      1.                                 00300100
300200     SET GAA-INT-INDEX DOWN BY 1.                                 00300200
300300                                                                  00300300
300400 4400-300-DISPLAY-LOOP.                                           00300400
300500                                                                  00300500
300600     SET GAA-INT-INDEX UP BY 1.                                   00300600
300700     IF  GAA-INT-INDEX > 5                                        00300700
300800         GO TO 4400-800-SEND.                                     00300800
300900                                                                  00300900
301000     IF  GAA-INT-ID (GAA-INDEX GAA-INT-INDEX) = HIGH-VALUES       00301000
301100         GO TO 4400-800-SEND.                                     00301100
301200                                                                  00301200
301300     IF  GAA-INT-ID (GAA-INDEX GAA-INT-INDEX)       = '#IBGR '    00301300
301400         MOVE GAA-INT-SLOT (GAA-INDEX GAA-INT-INDEX)              00301400
301500                                TO ACWA-DISPLAY-LEN-7             00301500
301600         MOVE ACWA-DISPLAY-LEN-7 TO IBGRSLTO                      00301600
301700         GO TO 4400-300-DISPLAY-LOOP.                             00301700
301800                                                                  00301800
301900     IF  GAA-INT-ID (GAA-INDEX GAA-INT-INDEX)       = '#IPGN '    00301900
302000         MOVE GAA-INT-SLOT (GAA-INDEX GAA-INT-INDEX)              00302000
302100                                TO ACWA-DISPLAY-LEN-7             00302100
302200         MOVE ACWA-DISPLAY-LEN-7 TO IPGNSLTO                      00302200
302300         GO TO 4400-300-DISPLAY-LOOP.                             00302300
302400                                                                  00302400
302500     IF  GAA-INT-ID (GAA-INDEX GAA-INT-INDEX)       = '#IPGT '    00302500
302600         MOVE GAA-INT-SLOT (GAA-INDEX GAA-INT-INDEX)              00302600
302700                                TO ACWA-DISPLAY-LEN-7             00302700
302800         MOVE ACWA-DISPLAY-LEN-7 TO IPGTSLTO                      00302800
302900         GO TO 4400-300-DISPLAY-LOOP.                             00302900
303000                                                                  00303000
303100     IF  GAA-INT-ID (GAA-INDEX GAA-INT-INDEX)       = '#IPGS '    00303100
303200         MOVE GAA-INT-SLOT (GAA-INDEX GAA-INT-INDEX)              00303200
303300                                TO ACWA-DISPLAY-LEN-7             00303300
303400         MOVE ACWA-DISPLAY-LEN-7 TO IPGSSLTO                      00303400
303500         GO TO 4400-300-DISPLAY-LOOP.                             00303500
303600                                                                  00303600
303700     IF GAA-INT-ID (GAA-INDEX GAA-INT-INDEX) = '#IDGD '           00303700
303800         MOVE GAA-INT-SLOT (GAA-INDEX GAA-INT-INDEX)              00303800
303900                                TO ACWA-DISPLAY-LEN-7             00303900
304000         MOVE ACWA-DISPLAY-LEN-7 TO IDGDSLTO                      00304000
304100         GO TO 4400-300-DISPLAY-LOOP.                             00304100
304200                                                                  00304200
304300     IF GAA-INT-ID (GAA-INDEX GAA-INT-INDEX) = '#IPGP '           00304300
304400         MOVE GAA-INT-SLOT (GAA-INDEX GAA-INT-INDEX)              00304400
304500                                TO ACWA-DISPLAY-LEN-7             00304500
304600         MOVE ACWA-DISPLAY-LEN-7 TO IPGPSLTO                      00304600
304700         GO TO 4400-300-DISPLAY-LOOP.                             00304700
304800                                                                  00304800
304900     MOVE WS-ABCODE-1BF3        TO WS-ABCODE                      00304900
305000     MOVE WS-ABCODE-1BF3-MSG    TO WS-ABCODE-MSG                  00305000
305100     PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                       00305100
305200                                                                  00305200
305300                                                                  00305300
305400 4400-800-SEND.                                                   00305400
305500                                                                  00305500
305600     PERFORM 4500-000-PROTECT-CRIT-DATA-ELE.                      00305600
305700                                                                  00305700
305800*-------RESET ATTR. 'CAUSE INTDESK & IDPROD CHANGED IN 4500- CALL 00305800
305900     PERFORM 7900-000-RESET-ATTRIBUTES.                           00305900
306000                                                                  00306000
306100     PERFORM 9000-000-SEND-ERASE-RETURN.                          00306100
306200                                                                  00306200
306300 4400-900-EXIT. EXIT.                                             00306300
306400                                                                  00306400
306500/*****************************************************************00306500
306600*  4500  -  PROTECT CRITICAL DATA ELEMENTS                       *00306600
306700*                                                                *00306700
306800*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00306800
306900*          1. DETERMINE IF GROUP IS CRITICAL (CALL GCTRSRT).     *00306900
307000*          2. IF GROUP IS CRITICAL:                              *00307000
307100*              - READ PRODUCTION CONTRACT, GROUP SPECIFIC, OR    *00307100
307200*                BENEFIT PROVISION.                              *00307200
307300*                - IF ON DATA BASE:                              *00307300
307400*                  - SCAN FOR #ABM TABULAR                       *00307400
307500*                    - IF TABULAR PRESENT AND ACTIVE, TABULAR IS *00307500
307600*                      CRITICAL, PROTECT CRITICAL DATA ELEMENTS  *00307600
307700*                      ON SCREEN AND ISSUE MESSAGE.              *00307700
307800******************************************************************00307800
307900 4500-000-PROTECT-CRIT-DATA-ELE SECTION.                          00307900
308000 4500-010.                                                        00308000
308100                                                                  00308100
308200     IF   DELADDI = 'CHG/DEL'  OR                                 00308200
308300          DELOLITI =  SPACE                                       00308300
308400     THEN NEXT SENTENCE                                           00308400
308500     ELSE GO TO 4500-900-EXIT.                                    00308500
308600                                                                  00308600
308700     MOVE WS-REQUEST-4500-CDE-PROTECT TO ACWA-CDE-REQUEST-CODE.   00308700
308800     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00308800
308900                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00308900
309000                                                                  00309000
309100     EXEC CICS  LINK   PROGRAM('GACDEPGM')                        00309100
309200                COMMAREA (COMMON-WORKAREAS)                       00309200
309300                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00309300
309400                                                                  00309400
309500     GO TO 4500-900-EXIT.                                         00309500
309600                                                                  00309600
309700 4500-900-EXIT. EXIT.                                             00309700
309800                                                                  00309800
309900/*****************************************************************00309900
310000*  4600  -  UPDATE CRITICAL DATA ELEMENT STATUS                  *00310000
310100*                                                                *00310100
310200*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00310200
310300*           1. READ ALL LEVEL TABULAR FROM PROVISION POOL        *00310300
310400*           2. COMPARE CDE ELEMENTS ON W/F ALL LVL TAB TO THOSE  *00310400
310500*               ON THE PROVISION POOL ALL LVL TABULAR RECORD.    *00310500
310600*           3. IF CDE ELEMENTS ON W/F ALL LVL TAB HAVE BEEN      *00310600
310700*               CHANGED, ISSUE MESSAGE AND POSITION CURSOR ON    *00310700
310800*               +CDE+ INDICATOR (POSITION=8).                    *00310800
310900*              IF MESSAGE HAS BEEN ISSUED AND OPERATOR HAS HIT   *00310900
311000*               ENTER, CONTINUE PROCESSING.                      *00311000
311100******************************************************************00311100
311200 4600-000-UPDATE-CDE-STATUS     SECTION.                          00311200
311300 4600-010.                                                        00311300
311400                                                                  00311400
311500     IF CDEINDO = ('+CDE+' OR '+CDE-') AND                        00311500
311600        (DELADDI = 'CHG/DEL' OR                                   00311600
311700        (DELADDI = 'CHG/ADD' AND                                  00311700
311800        WRK-SIGNAL-FROM-ONLINE  =  'W'))                          00311800
311900        NEXT SENTENCE                                             00311900
312000     ELSE                                                         00312000
312100        IF CDEINDO = ('+CDE+' OR '+CDE-') AND                     00312100
312200           (DELADDI = 'CHG/ADD')                                  00312200
312300           NEXT SENTENCE                                          00312300
312400        ELSE                                                      00312400
312500            GO TO 4600-900-EXIT.                                  00312500
312600                                                                  00312600
312700     MOVE WS-ALT-WORKFILE-KEYS       TO ACWA-ALT-WORKFILE-KEYS.   00312700
312800     SET  ACWA-INDEX-1               TO GAA-INDEX.                00312800
312900     MOVE WS-REQUEST-4600-CDE-STATUS TO ACWA-CDE-REQUEST-CODE.    00312900
313000     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00313000
313100                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00313100
313200                                                                  00313200
313300     EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      00313300
313400                COMMAREA (COMMON-WORKAREAS)                       00313400
313500                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00313500
313600                                                                  00313600
313700     IF  ACWA-CDE-RETURN-DONT-SEND                                00313700
313800*        EXEC CICS  RETURN  END-EXEC.                             00313800
313900         EXEC CICS  RETURN TRANSID('GA1B')                        00313900
314000                    COMMAREA(DFHCOMMAREA)                         00314000
314100                    LENGTH  (EIBCALEN)                            00314100
314200                    END-EXEC.                                     00314200
314300                                                                  00314300
314400 4600-900-EXIT. EXIT.                                             00314400
314500                                                                  00314500
314600/*****************************************************************00314600
314700*  4700  -  UPDATE W/F CONTROL RECORD                            *00314700
314800*                                                                *00314800
314900*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00314900
315000*          1. READ W/F CONTROL RECORD, UPDATE CDE RECORD COUNTS  *00315000
315100*             WITH THE ACTION TAKEN ON THE ALL LEVEL TABULAR     *00315100
315200*             RECORD IF ITS CDE STATUS CHANGED.                  *00315200
315300*          2. REWRITE W/F CONTROL RECORD                         *00315300
315400******************************************************************00315400
315500 4700-000-UPDATE-CONTROL-RECORD SECTION.                          00315500
315600 4700-010.                                                        00315600
315700                                                                  00315700
315800     MOVE WS-ALT-WORKFILE-KEYS        TO ACWA-ALT-WORKFILE-KEYS.  00315800
315900     SET  ACWA-INDEX-1                TO GAA-INDEX.               00315900
316000     MOVE WS-REQUEST-4700-CNTL-UPDATE TO ACWA-CDE-REQUEST-CODE.   00316000
316100     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00316100
316200                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00316200
316300                                                                  00316300
316400     EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      00316400
316500                COMMAREA (COMMON-WORKAREAS)                       00316500
316600                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00316600
316700                                                                  00316700
316800 4700-900-EXIT. EXIT.                                             00316800
316900                                                                  00316900
317000/*****************************************************************00317000
317100* 5000  XCTL TO PREVIOUS MENU                                    *00317100
317200*                                                                *00317200
317300*   THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM      *00317300
317400*  ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD AND    *00317400
317500*  INSURE THAT THE TABLE OF OCCURRENCES IS SORTED AND THAT ANY   *00317500
317600*  DUPLICATES ARE DROPPED FROM THE LIST.  WE THEN REWRITE THE    *00317600
317700*  ALL LEVEL TABULAR AND READ THE PARTICULAR RECORD THAT THE     *00317700
317800*  MENU WHICH PASSED US CONTROL WOULD REQUIRE.  FINALLY BASED    *00317800
317900*  ON THE PREVIOUS MENU FIELD CARRIED THROUGHOUT THIS PART OF    *00317900
318000*  THE SYSTEM WE RETURN TO THE PREVIOUS MENU.                    *00318000
318100******************************************************************00318100
318200 5000-000-XCTL-TO-PREVIOUS-MENU SECTION.                          00318200
318300 5000-010.                                                        00318300
318400                                                                  00318400
318500     PERFORM 3200-000-READ-REC-FOR-UPDATE.                        00318500
318600                                                                  00318600
318700     IF NOT GCIO-GOOD-RETURN                                      00318700
318800        MOVE WS-ABCODE-1BFK        TO WS-ABCODE                   00318800
318900        MOVE WS-ABCODE-1BFK-MSG    TO WS-ABCODE-MSG               00318900
319000        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00319000
319100                                                                  00319100
319200     PERFORM 6500-000-SORT-COMPRESS-ALL-LVL.                      00319200
319300     PERFORM 4600-000-UPDATE-CDE-STATUS.                          00319300
319400     PERFORM 3000-000-UPDATE-GAA-RECORD.                          00319400
319500                                                                  00319500
319600     IF ACWA-CDE-FIELD-CHANGED OR  ACWA-CDE-REC-CHANGED           00319600
319700        IF EIBCPOSN = 8                                           00319700
319800           NEXT SENTENCE                                          00319800
319900        ELSE                                                      00319900
320000*          EXEC CICS  RETURN  END-EXEC.                           00320000
320100           EXEC CICS  RETURN TRANSID('GA1B')                      00320100
320200                      COMMAREA(DFHCOMMAREA)                       00320200
320300                      LENGTH  (EIBCALEN)                          00320300
320400                      END-EXEC.                                   00320400
320500                                                                  00320500
320600     IF FRMNUIDI  =  'GS3A'                                       00320600
320700        PERFORM 5100-000-RETURN-TO-GRP-SPEC                       00320700
320800        EXEC CICS  XCTL  PROGRAM ('GS3APGM')                      00320800
320900                   COMMAREA(WORK-RECORD-3)                        00320900
321000                   LENGTH (WS-WRK-GRP-SPEC-LEN)   END-EXEC.       00321000
321100                                                                  00321100
321200     IF  FRMNUIDI  =  'GC4A'                                      00321200
321300        PERFORM 5200-000-RETURN-TO-CONTRACT                       00321300
321400        EXEC CICS  XCTL  PROGRAM ('GC4APGM')                      00321400
321500                   COMMAREA(WORK-RECORD-4)                        00321500
321600                   LENGTH (WS-WRK-CONTRACT-LEN)   END-EXEC.       00321600
321700                                                                  00321700
321800     IF FRMNUIDI  =  'GC8A'                                       00321800
321900        PERFORM 5300-000-RETURN-TO-BEN-PROV                       00321900
322000        EXEC CICS  XCTL  PROGRAM ('GC8APGM')                      00322000
322100                   COMMAREA(WORK-RECORD-5)                        00322100
322200                   LENGTH (WS-WRK-BEN-PROV-LEN)   END-EXEC.       00322200
322300                                                                  00322300
322400*******                                                           00322400
322500* STS *===> RETURN TO SINGLE TABULAR SUPPORT MENU, NO COMMAREA    00322500
322600*******                                                          |00322600
322700     IF  FRMNUIDI  =  'GTM1'                                      00322700
322800         EXEC CICS  XCTL  PROGRAM('GTM1PGM')   END-EXEC.          00322800
322900*******                                                          |00322900
323000* STS *----------------------------------------------------------*00323000
323100*******                                                           00323100
323200                                                                  00323200
323300 5000-900-EXIT. EXIT.                                             00323300
323400                                                                  00323400
323500/*****************************************************************00323500
323600* 5100  RETURN TO GRP SPEC                                       *00323600
323700*                                                                *00323700
323800*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00323800
323900*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00323900
324000******************************************************************00324000
324100 5100-000-RETURN-TO-GRP-SPEC    SECTION.                          00324100
324200 5100-010.                                                        00324200
324300                                                                  00324300
324400        EXEC CICS GETMAIN                                         00324400
324500               SET(ADDRESS OF WF-IO-PARM-WRK-GRP-SPEC-REC)        00324500
324600               INITIMG(WS-HEX-00)                                 00324600
324700               LENGTH(WS-IO-PARM-WRK-GRP-SPEC-LEN)                00324700
324800               END-EXEC.                                          00324800
324900                                                                  00324900
325000        SET ACWA-WF-GRP-SPEC-PNTR     TO                          00325000
325100                 ADDRESS OF WF-IO-PARM-WRK-GRP-SPEC-REC.          00325100
325200                                                                  00325200
325300     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00325300
325400     MOVE 'G'                  TO GCIO-WRK-STATUS-CODE.           00325400
325500     MOVE 'G2'                 TO GCIO-WRK-RECORD-TYPE.           00325500
325600     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00325600
325700     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00325700
325800     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00325800
325900     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00325900
326000     MOVE SPACES               TO GCIO-WRK-LINE-OF-BUS            00326000
326100                                  GCIO-WRK-PROVISION-ID           00326100
326200                                  GCIO-WRK-PROVIDER-CONTROL       00326200
326300                                  GCIO-WRK-TAB-PROVISION-ID.      00326300
326400     MOVE ZEROS                TO GCIO-WRK-PROVISION-SLOT-NO,     00326400
326500                                  GCIO-WRK-TAB-PROV-SLOT-NO.      00326500
326600     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00326600
326700     MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             00326700
326800                                                                  00326800
326900     MOVE GC-GCPSWORK-DDNAME     TO GCIO3-FILE-DDNAME.            00326900
327000     MOVE GCIO-WORKFILE-KEY      TO GCIO3-FILE-KEY.               00327000
327100     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO3-FILE-ACCESS-CODE.       00327100
327200     MOVE GC-GCIO-AREA-1         TO GCIO3-IO-AREA-TO-USE.         00327200
327300                                                                  00327300
327400     MOVE GC-GCGRPSPC-VARY-MAX-OCUR  TO                           00327400
327500                      GCG-COUNT-TAB-PROVN-POINTERS.               00327500
327600                                                                  00327600
327700     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00327700
327800                COMMAREA(WF-IO-PARM-WRK-GRP-SPEC-REC)             00327800
327900                LENGTH (WS-IO-PARM-WRK-GRP-SPEC-LEN)  END-EXEC.   00327900
328000                                                                  00328000
328100     IF NOT GCIO3-GOOD-RETURN                                     00328100
328200        MOVE WS-ABCODE-1BFL        TO WS-ABCODE                   00328200
328300        MOVE WS-ABCODE-1BFL-MSG    TO WS-ABCODE-MSG               00328300
328400        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00328400
328500                                                                  00328500
328600 5100-900-EXIT. EXIT.                                             00328600
328700                                                                  00328700
328800/*****************************************************************00328800
328900* 5200  RETURN TO CONTRACT                                       *00328900
329000*                                                                *00329000
329100*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00329100
329200*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00329200
329300******************************************************************00329300
329400 5200-000-RETURN-TO-CONTRACT    SECTION.                          00329400
329500 5200-010.                                                        00329500
329600                                                                  00329600
329700                                                                  00329700
329800        EXEC CICS GETMAIN                                         00329800
329900               SET(ADDRESS OF WF-IO-PARM-WRK-CONTRACT-REC)        00329900
330000               INITIMG(WS-HEX-00)                                 00330000
330100               LENGTH(WS-IO-PARM-WRK-CONTRACT-LEN)                00330100
330200               END-EXEC.                                          00330200
330300                                                                  00330300
330400        SET ACWA-WF-CONTRACT-PNTR     TO                          00330400
330500                 ADDRESS OF WF-IO-PARM-WRK-CONTRACT-REC.          00330500
330600                                                                  00330600
330700     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00330700
330800     MOVE 'C'                  TO GCIO-WRK-STATUS-CODE.           00330800
330900     MOVE 'C2'                 TO GCIO-WRK-RECORD-TYPE.           00330900
331000     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00331000
331100     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00331100
331200     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00331200
331300     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00331300
331400     MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           00331400
331500     MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      00331500
331600     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00331600
331700     MOVE SPACES               TO GCIO-WRK-PROVISION-ID           00331700
331800                                  GCIO-WRK-TAB-PROVISION-ID.      00331800
331900     MOVE ZEROS                TO GCIO-WRK-PROVISION-SLOT-NO      00331900
332000                                  GCIO-WRK-TAB-PROV-SLOT-NO.      00332000
332100                                                                  00332100
332200     MOVE GCA-EFFDT-CEN          TO GCIO-WRK-EFFDT-CEN.           00332200
332300     MOVE GC-GCPSWORK-DDNAME     TO GCIO4-FILE-DDNAME.            00332300
332400     MOVE GCIO-WORKFILE-KEY      TO GCIO4-FILE-KEY.               00332400
332500     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO4-FILE-ACCESS-CODE.       00332500
332600     MOVE GC-GCIO-AREA-1         TO GCIO4-IO-AREA-TO-USE.         00332600
332700                                                                  00332700
332800     MOVE GC-GCCONTR-VARY-MAX-OCUR  TO                            00332800
332900                      GCT-COUNT-BEN-PROVN-POINTERS.               00332900
333000                                                                  00333000
333100     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00333100
333200                COMMAREA(WF-IO-PARM-WRK-CONTRACT-REC)             00333200
333300                LENGTH (WS-IO-PARM-WRK-CONTRACT-LEN)   END-EXEC.  00333300
333400                                                                  00333400
333500     IF NOT GCIO4-GOOD-RETURN                                     00333500
333600        MOVE WS-ABCODE-1BFM        TO WS-ABCODE                   00333600
333700        MOVE WS-ABCODE-1BFM-MSG    TO WS-ABCODE-MSG               00333700
333800        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00333800
333900                                                                  00333900
334000 5200-900-EXIT. EXIT.                                             00334000
334100                                                                  00334100
334200/*****************************************************************00334200
334300* 5300  RETURN TO BEN PROV                                       *00334300
334400*                                                                *00334400
334500*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00334500
334600*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00334600
334700******************************************************************00334700
334800 5300-000-RETURN-TO-BEN-PROV    SECTION.                          00334800
334900 5300-010.                                                        00334900
335000                                                                  00335000
335100                                                                  00335100
335200        EXEC CICS GETMAIN                                         00335200
335300               SET(ADDRESS OF WF-IO-PARM-WRK-BEN-PROV-REC)        00335300
335400               INITIMG(WS-HEX-00)                                 00335400
335500               LENGTH(WS-IO-PARM-WRK-BEN-PROV-LEN)                00335500
335600               END-EXEC.                                          00335600
335700                                                                  00335700
335800        SET ACWA-WF-BEN-PROV-PNTR     TO                          00335800
335900                 ADDRESS OF WF-IO-PARM-WRK-BEN-PROV-REC.          00335900
336000                                                                  00336000
336100     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00336100
336200     MOVE 'C'                   TO GCIO-WRK-STATUS-CODE.          00336200
336300     MOVE 'C4'                  TO GCIO-WRK-RECORD-TYPE.          00336300
336400     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00336400
336500     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00336500
336600     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00336600
336700     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00336700
336800     MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS.          00336800
336900     MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL.     00336900
337000     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00337000
337100     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00337100
337200     MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID.         00337200
337300     MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID.     00337300
337400     MOVE 9999999               TO GCIO-WRK-PROVISION-SLOT-NO.    00337400
337500     MOVE ZEROS                 TO GCIO-WRK-TAB-PROV-SLOT-NO.     00337500
337600                                                                  00337600
337700     MOVE GC-GCPSWORK-DDNAME     TO GCIO5-FILE-DDNAME.            00337700
337800     MOVE GCIO-WORKFILE-KEY      TO GCIO5-FILE-KEY.               00337800
337900     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO5-FILE-ACCESS-CODE.       00337900
338000     MOVE GC-GCIO-AREA-1         TO GCIO5-IO-AREA-TO-USE.         00338000
338100                                                                  00338100
338200     MOVE GC-GCBENPRV-VARY-MAX-OCUR  TO                           00338200
338300                      GCP-COUNT-TAB-PROVN-POINTERS.               00338300
338400                                                                  00338400
338500     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00338500
338600                COMMAREA(WF-IO-PARM-WRK-BEN-PROV-REC)             00338600
338700                LENGTH (WS-IO-PARM-WRK-BEN-PROV-LEN)  END-EXEC.   00338700
338800                                                                  00338800
338900     IF NOT GCIO5-GOOD-RETURN                                     00338900
339000        MOVE WS-ABCODE-1BFN        TO WS-ABCODE                   00339000
339100        MOVE WS-ABCODE-1BFN-MSG    TO WS-ABCODE-MSG               00339100
339200        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00339200
339300                                                                  00339300
339400 5300-900-EXIT. EXIT.                                             00339400
339500                                                                  00339500
339600/*****************************************************************00339600
339700* 6000  BUILD GROUP SPEC KEY                                     *00339700
339800*                                                                *00339800
339900*    BUILD THE GROUP SPECIFIC KEY FOR WORKFILE READS             *00339900
340000******************************************************************00340000
340100 6000-000-BUILD-GROUP-SPEC-KEY  SECTION.                          00340100
340200 6000-010.                                                        00340200
340300                                                                  00340300
340400     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00340400
340500     MOVE  'G'                  TO GCIO-WRK-STATUS-CODE.          00340500
340600     MOVE  'G3'                 TO GCIO-WRK-RECORD-TYPE.          00340600
340700     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00340700
340800     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00340800
340900     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00340900
341000     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00341000
341100     MOVE SPACES                TO GCIO-WRK-LINE-OF-BUS,          00341100
341200                                   GCIO-WRK-PROVIDER-CONTROL.     00341200
341300     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00341300
341400     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00341400
341500     MOVE TABIDI                TO GCIO-WRK-PROVISION-ID.         00341500
341600     MOVE TABSLTNI              TO ACWA-DISPLAY-LEN-7.            00341600
341700     MOVE ACWA-DISPLAY-LEN-7    TO GCIO-WRK-PROVISION-SLOT-NO.    00341700
341800     MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID.     00341800
341900     MOVE ZEROS                 TO GCIO-WRK-TAB-PROV-SLOT-NO.     00341900
342000                                                                  00342000
342100 6000-900-EXIT. EXIT.                                             00342100
342200                                                                  00342200
342300******************************************************************00342300
342400* 6100  BUILD CONTRACT KEY                                       *00342400
342500*                                                                *00342500
342600*    BUILD THE CONTRACT KEY FOR WORKFILE READS                   *00342600
342700******************************************************************00342700
342800 6100-000-BUILD-CONTRACT-KEY    SECTION.                          00342800
342900 6100-010.                                                        00342900
343000                                                                  00343000
343100     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00343100
343200     MOVE  'C'                 TO GCIO-WRK-STATUS-CODE.           00343200
343300     MOVE  'C3'                TO GCIO-WRK-RECORD-TYPE.           00343300
343400     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00343400
343500     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00343500
343600     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00343600
343700     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00343700
343800     MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           00343800
343900     MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      00343900
344000     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00344000
344100     MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             00344100
344200     MOVE TABIDI               TO GCIO-WRK-PROVISION-ID.          00344200
344300     MOVE TABSLTNI             TO ACWA-DISPLAY-LEN-7.             00344300
344400     MOVE ACWA-DISPLAY-LEN-7   TO GCIO-WRK-PROVISION-SLOT-NO.     00344400
344500     MOVE SPACES               TO GCIO-WRK-TAB-PROVISION-ID.      00344500
344600     MOVE ZEROS                TO GCIO-WRK-TAB-PROV-SLOT-NO.      00344600
344700                                                                  00344700
344800 6100-900-EXIT. EXIT.                                             00344800
344900                                                                  00344900
345000/*****************************************************************00345000
345100* 6200  BUILD BEN PROV KEY                                       *00345100
345200*                                                                *00345200
345300*    BUILD THE BEN PROV KEY FOR WORKFILE READS                   *00345300
345400******************************************************************00345400
345500 6200-000-BUILD-BEN-PROV-KEY    SECTION.                          00345500
345600 6200-010.                                                        00345600
345700                                                                  00345700
345800     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00345800
345900     MOVE  'C'                  TO GCIO-WRK-STATUS-CODE.          00345900
346000     MOVE  'C5'                 TO GCIO-WRK-RECORD-TYPE.          00346000
346100     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00346100
346200     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00346200
346300     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00346300
346400     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00346400
346500     MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS.          00346500
346600     MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL.     00346600
346700     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00346700
346800     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00346800
346900     MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID.         00346900
347000     MOVE +9999999              TO GCIO-WRK-PROVISION-SLOT-NO.    00347000
347100     MOVE TABIDI                TO GCIO-WRK-TAB-PROVISION-ID.     00347100
347200     MOVE TABSLTNI              TO ACWA-DISPLAY-LEN-7.            00347200
347300     MOVE ACWA-DISPLAY-LEN-7    TO GCIO-WRK-TAB-PROV-SLOT-NO.     00347300
347400                                                                  00347400
347500 6200-900-EXIT. EXIT.                                             00347500
347600                                                                  00347600
347700/*****************************************************************00347700
347800*  XCTL TO MAIN MENU                                             *00347800
347900*                                                                *00347900
348000*    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO   *00348000
348100*  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE*00348100
348200*  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE MENUS00348200
348300*  TO GET TO US; WE ARE A MODULE AT THE BOTTOM OF A PYRAMID TO GET00348300
348400*  HERE YOU MUST START AT THE TOP (THE MAIN MENU).               *00348400
348500******************************************************************00348500
348600 6400-000-XCTL-TO-MAIN-MENU     SECTION.                          00348600
348700 6400-010.                                                        00348700
348800                                                                  00348800
348900     MOVE WS-ABCODE-1BP1        TO WS-ABCODE.                     00348900
349000     MOVE WS-ABCODE-1BP1-MSG    TO WS-ABCODE-MSG.                 00349000
349100                                                                  00349100
349200     EXEC CICS  XCTL  PROGRAM('GCPSPGM')   END-EXEC.              00349200
349300                                                                  00349300
349400 6400-900-EXIT. EXIT.                                             00349400
349500                                                                  00349500
349600/*****************************************************************00349600
349700* 6500  SORT COMPRESS ALL LVL                                    *00349700
349800*                                                                *00349800
349900*    THIS ROUTINE WILL COPY ALL ENTRIES FROM THE TABULAR PORTION *00349900
350000*  TO A COPY OF THE TABULAR, THEN SORT THE COPY INTO ASCENDING   *00350000
350100*  SEQUENCE, ANY DUPLICATES ARE REMOVED FROM THE TABLE.          *00350100
350200******************************************************************00350200
350300 6500-000-SORT-COMPRESS-ALL-LVL SECTION.                          00350300
350400 6500-010.                                                        00350400
350500                                                                  00350500
350600        EXEC CICS GETMAIN                                         00350600
350700               SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)            00350700
350800               INITIMG(WS-HEX-00)                                 00350800
350900               LENGTH(WS-COPY-TABLE-LEN)                          00350900
351000               END-EXEC.                                          00351000
351100                                                                  00351100
351200        SET ACWA-COPY-TAB-PNTR        TO                          00351200
351300                 ADDRESS OF COPY-TABULAR-TABLE-AREA.              00351300
351400                                                                  00351400
351500     MOVE GAA-ENTRY-COUNT  TO  GAA-ENTRY-COUNT.                   00351500
351600     SET GAA-INDEX,  COPY-IDX  TO  1.                             00351600
351700                                                                  00351700
351800 6500-100-COPY-TABLE.                                             00351800
351900                                                                  00351900
352000     IF GAA-INDEX  NOT >  GAA-ENTRY-COUNT                         00352000
352100        MOVE GAA-ENTRY (GAA-INDEX)                                00352100
352200          TO COPY-TABULAR-TABLE (COPY-IDX)                        00352200
352300        SET GAA-INDEX,  COPY-IDX  UP BY  1                        00352300
352400        GO TO 6500-100-COPY-TABLE.                                00352400
352500                                                                  00352500
352600     SET  COPY-IDX  TO  1.                                        00352600
352700     SET  COPY-IDX2 TO  2.                                        00352700
352800                                                                  00352800
352900 6500-200-SORT-TABLE.                                             00352900
353000                                                                  00353000
353100     IF COPY-IDX2  >  GAA-ENTRY-COUNT                             00353100
353200        GO TO 6500-400-ARE-WE-DONE-SORTING.                       00353200
353300                                                                  00353300
353400     IF  COPY-SORTABLE-FLDS (COPY-IDX)  >                         00353400
353500         COPY-SORTABLE-FLDS (COPY-IDX2)                           00353500
353600     THEN                                                         00353600
353700         MOVE COPY-TABULAR-TABLE (COPY-IDX)                       00353700
353800           TO WS-ENTRY                                            00353800
353900         MOVE COPY-TABULAR-TABLE (COPY-IDX2)                      00353900
354000           TO COPY-TABULAR-TABLE (COPY-IDX)                       00354000
354100         MOVE WS-ENTRY                                            00354100
354200           TO COPY-TABULAR-TABLE (COPY-IDX2)                      00354200
354300         SET COPY-IDX2  UP BY  1                                  00354300
354400         GO TO 6500-200-SORT-TABLE.                               00354400
354500                                                                  00354500
354600     IF COPY-SORTABLE-FLDS (COPY-IDX)  <                          00354600
354700        COPY-SORTABLE-FLDS (COPY-IDX2)                            00354700
354800        SET COPY-IDX2  UP BY  1                                   00354800
354900        GO TO 6500-200-SORT-TABLE.                                00354900
355000                                                                  00355000
355100     SET COPY-IDX3,  COPY-IDX4  TO  COPY-IDX2.                    00355100
355200     SET COPY-IDX4  UP BY 1.                                      00355200
355300                                                                  00355300
355400 6500-300-ELIMINATE-DUPLICATES.                                   00355400
355500                                                                  00355500
355600     IF COPY-IDX4  NOT >  GAA-ENTRY-COUNT                         00355600
355700        MOVE COPY-TABULAR-TABLE (COPY-IDX4)  TO                   00355700
355800             COPY-TABULAR-TABLE (COPY-IDX3)                       00355800
355900        SET COPY-IDX3,  COPY-IDX4  UP BY  1                       00355900
356000        GO TO 6500-300-ELIMINATE-DUPLICATES.                      00356000
356100                                                                  00356100
356200     SUBTRACT 1  FROM  GAA-ENTRY-COUNT.                           00356200
356300     GO TO 6500-200-SORT-TABLE.                                   00356300
356400                                                                  00356400
356500 6500-400-ARE-WE-DONE-SORTING.                                    00356500
356600                                                                  00356600
356700     IF COPY-IDX  <  GAA-ENTRY-COUNT                              00356700
356800        SET COPY-IDX   UP BY  1                                   00356800
356900        SET COPY-IDX2  TO COPY-IDX                                00356900
357000        SET COPY-IDX2  UP BY 1                                    00357000
357100        GO TO 6500-200-SORT-TABLE.                                00357100
357200                                                                  00357200
357300     MOVE GAA-ENTRY-COUNT  TO  GAA-ENTRY-COUNT.                   00357300
357400     SET GAA-INDEX,  COPY-IDX  TO  1.                             00357400
357500                                                                  00357500
357600 6500-500-MOVE-COPY-BACK.                                         00357600
357700                                                                  00357700
357800     IF GAA-INDEX  NOT >  GAA-ENTRY-COUNT                         00357800
357900        MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                    00357900
358000             GAA-ENTRY (GAA-INDEX)                                00358000
358100        SET GAA-INDEX,  COPY-IDX  UP BY  1                        00358100
358200        GO TO 6500-500-MOVE-COPY-BACK.                            00358200
358300                                                                  00358300
358400     MOVE GAA-ENTRY-COUNT  TO  GAA-ENTRY-COUNT.                   00358400
358500     IF GAA-ENTRY-COUNT  NOT <  GC-GCTABULR-ABM-VARY-MAX-OCUR     00358500
358600        MOVE 'Y'  TO  ACWA-ERROR-SW.                              00358600
358700                                                                  00358700
358800 6500-900-EXIT. EXIT.                                             00358800
358900                                                                  00358900
359000/*****************************************************************00359000
359100* 7900  RESET ATTRIBUTES                                         *00359100
359200******************************************************************00359200
359300 7900-000-RESET-ATTRIBUTES      SECTION.                          00359300
359400 7900-010.                                                        00359400
359500                                                                  00359500
359600     MOVE DFHBMUNF                                                00359600
359700       TO BENVLQLA  COPAYINA  CSTCONTA  FAMINDIA  LOBA            00359700
359800          PERIODA   PLCTRMTA  SRVGRUPA  PRTIMEFA  MAXOVRDA        00359800
359900          REININDA  INTRVALA  INTTYPEA  CLMLVLIA  BNMXVALA        00359900
360000          DAYFACIA  OVRDINDA  NEWVALUA  INTDESKA  FYIVALA         00360000
360100          CONDALLA  CONDEXCA  CONDICDA  CONDTABA  CONDMENA        00360100
360200          CONDDRGA  CONDALCA  CONDOBNA  CONDOBCA  CONDMALA        00360200
360300          CONDCARA  CONDOBSA  CONDKDYA  CONDACCA  CONDPECA        00360300
360400          CONDNEMA  DEFINTNA  CONDSUIA  CONDTMJA  CONDINFA        00360400
360500          IBGROPTA  IPGNOPTA  IPGTOPTA  MFRMSLTA  PERTQALA        00360500
360600          AGEQLLA  AGEQLHA    CONDLIFA  IPGSOPTA  ASCDSCDA        00360600
360700          IDGDOPTA  IPGPOPTA  AGELIMLA  AGELIMHA  RELPINDA        00360700
360800          CONDEMCA  CONDEACA  CONDSMIA  CONDNSMA  BISNDINA        00360800
360900          FEAKINDA  ACCUMIDA  CAPINDA   SABDINDA                  00360900
360900          BENTYPA   TIERCDA   TIERLVA.                            00360910
361000                                                                  00361000
361100     IF  DELADDO  =  'CHG/DEL'                                    00361100
361200     THEN                                                         00361200
361300         NEXT SENTENCE                                            00361300
361400     ELSE                                                         00361400
361500         GO TO 7900-900-EXIT.                                     00361500
361600                                                                  00361600
361700                                                                  00361700
361800     IF  CDEINDO = '+CDE+'                                        00361800
361900     THEN                                                         00361900
362000*---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         00362000
362100         MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA       00362100
362200               AGELIMA    PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA   00362200
362300               AGEQLTA    COPAYITA  INTDESTA CONDTG1A  CONDTG2A   00362300
362400               BISNDITA                                           00362400
362500         IF INTDESKO  NOT =  IDPRODO                              00362500
362600            MOVE DFHBMASB  TO  IBGRIDA,  IPGNIDA,  IPGTIDA        00362600
362700                               IDGDIDA,  IPGPIDA,  IPGSIDA        00362700
362800            MOVE DFHBMABF  TO  IBGRSLTA, IPGNSLTA, IPGTSLTA       00362800
362900                               IDGDSLTA, IPGPSLTA, IPGSSLTA       00362900
363000            MOVE DFHBMUBF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00363000
363100                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00363100
363200         ELSE                                                     00363200
363300            MOVE DFHBMASF  TO  IBGRIDA,  IPGNIDA,  IPGTIDA        00363300
363400                               IDGDIDA,  IPGPIDA,  IPGSIDA        00363400
363500            MOVE DFHBMASF  TO  IBGRSLTA, IPGNSLTA, IPGTSLTA       00363500
363600                               IDGDSLTA, IPGPSLTA, IPGSSLTA       00363600
363700            MOVE DFHBMUNF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00363700
363800                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00363800
363900     ELSE                                                         00363900
364000         NEXT SENTENCE.                                           00364000
364100                                                                  00364100
364200                                                                  00364200
364300     IF  CDEINDO = '+CDE-'                                        00364300
364400     THEN                                                         00364400
364500*---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         00364500
364600         MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA       00364600
364700               AGELIMA    PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA   00364700
364800               AGEQLTA    COPAYITA  INTDESTA CONDTG1A  CONDTG2A   00364800
364900               BISNDITA                                           00364900
365000*---------------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS        00365000
365100         MOVE DFHBMASF TO DELOPTNA  PERIODA  BENVLQLA  LOBA       00365100
365200        AGELIMLA AGELIMHA PLCTRMTA  FAMINDIA SRVGRUPA  CSTCONTA   00365200
365300        AGEQLLA  AGEQLHA  COPAYINA  INTDESKA CONDALLA  CONDEXCA   00365300
365400                          CONDICDA  CONDTABA CONDMENA  CONDDRGA   00365400
365500                          CONDALCA  CONDOBCA CONDOBNA  CONDMALA   00365500
365600                CONDLIFA  CONDCARA  CONDOBSA CONDKDYA  CONDACCA   00365600
365700                CONDPECA  CONDNEMA CONDSUIA  CONDTMJA  CONDINFA   00365700
365800                CONDEMCA  CONDEACA  CONDSMIA  CONDNSMA BISNDINA   00365800
365900                                                                  00365900
366000         IF INTDESKO  NOT =  IDPRODO                              00366000
366100*--------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS               00366100
366200            MOVE DFHBMASF  TO  IBGROPTA,  IPGNOPTA,  IPGTOPTA     00366200
366300                               IDGDOPTA,  IPGPOPTA,  IPGSOPTA     00366300
366400            MOVE DFHBMABF  TO  IBGRIDA,  IBGRSLTA,                00366400
366500                       IPGNIDA,  IPGNSLTA,   IPGTIDA,  IPGTSLTA   00366500
366600                       IDGDIDA,  IDGDSLTA,   IPGPIDA,  IPGPSLTA   00366600
366700                       IPGSIDA,  IPGSSLTA                         00366700
366800            IF  ERRMSGO > SPACES                                  00366800
366900            THEN                                                  00366900
367000                NEXT SENTENCE                                     00367000
367100            ELSE                                                  00367100
367200                SET  WT-01-INDEX  TO  +08                         00367200
367300                MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO 00367300
367400         ELSE                                                     00367400
367500            MOVE DFHBMASF  TO  IBGRIDA,  IBGRSLTA,                00367500
367600                       IPGNIDA,  IPGNSLTA,   IPGTIDA,  IPGTSLTA   00367600
367700                       IDGDIDA,  IDGDSLTA,   IPGPIDA,  IPGPSLTA   00367700
367800                       IPGSIDA,  IPGSSLTA                         00367800
367900            MOVE DFHBMUNF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00367900
368000                               IDGDOPTA,  IPGPOPTA, IPGSOPTA      00368000
368100            IF  ERRMSGO > SPACES                                  00368100
368200            THEN                                                  00368200
368300                NEXT SENTENCE                                     00368300
368400            ELSE                                                  00368400
368500                SET  WT-01-INDEX  TO  +08                         00368500
368600                MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO 00368600
368700     ELSE                                                         00368700
368800         NEXT SENTENCE.                                           00368800
368900                                                                  00368900
369000                                                                  00369000
369100 7900-900-EXIT. EXIT.                                             00369100
369200                                                                  00369200
369300/*****************************************************************00369300
369400* 8000  XCTL SWITCH ADD DEL MODE                                 *00369400
369500*                                                                *00369500
369600*   THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO *00369600
369700*  ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS*00369700
369800*  THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL     *00369800
369900*  TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE*00369900
370000*  PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE   *00370000
370100*  IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).             *00370100
370200******************************************************************00370200
370300 8000-000-SWITCH-ADD-DEL-MODE   SECTION.                          00370300
370400 8000-010.                                                        00370400
370500                                                                  00370500
370600     IF DELADDI  =  'CHG/DEL'                                     00370600
370700        PERFORM 8100-000-DISPLAY-ADD-SCREEN.                      00370700
370800                                                                  00370800
370900     PERFORM 3100-000-READ-RECORD.                                00370900
371000     MOVE GAA-ENTRY-COUNT  TO  GAA-ENTRY-COUNT.                   00371000
371100                                                                  00371100
371200     IF  GAA-ENTRY-COUNT  >  1                                    00371200
371300     THEN                                                         00371300
371400         MOVE 'CHG/DEL'  TO DELADDO                               00371400
371500         MOVE 'D'        TO DELOLITO                              00371500
371600         MOVE SPACES     TO COCURANO                              00371600
371700         MOVE DFHBMASK   TO DLOPTLTA                              00371700
371800         MOVE DFHBMUNP   TO DELOPTNA                              00371800
371900         SET  WT-01-INDEX                     TO +20              00371900
372000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00372000
372100         SET GAA-INDEX   TO 1                                     00372100
372200         PERFORM 4400-000-BUILD-DISPLAY                           00372200
372300     ELSE                                                         00372300
372400         SET  WT-01-INDEX                     TO +12              00372400
372500         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.         00372500
372600                                                                  00372600
372700 8000-900-EXIT. EXIT.                                             00372700
372800                                                                  00372800
372900/*****************************************************************00372900
373000* 8100  DISPLAY ADD SCREEN                                       *00373000
373100******************************************************************00373100
373200 8100-000-DISPLAY-ADD-SCREEN    SECTION.                          00373200
373300                                                                  00373300
373400     MOVE 'CHG/ADD'  TO DELADDO.                                  00373400
373500     MOVE SPACES     TO COCURANO.                                 00373500
373600     MOVE DFHBMASD   TO DLOPTLTA   DELOPTNA.                      00373600
373700     PERFORM 4100-000-DISPLAY-SKELETON.                           00373700
373800                                                                  00373800
373900 8100-900-EXIT. EXIT.                                             00373900
374000                                                                  00374000
374100/*****************************************************************00374100
374200* 9000  SEND ERASE THEN RETURN                                   *00374200
374300******************************************************************00374300
374400 9000-000-SEND-ERASE-RETURN     SECTION.                          00374400
374500 9000-010.                                                        00374500
374600                                                                  00374600
374700     MOVE DFHBMASD TO                                             00374700
374800                   PERLITTA  MANAPLTA CARYOVTA FDLRCLTA           00374800
374900                   PERLIMTA  MANAPLIA CARYOVRA FDLRCLIA           00374900
375000                   TIMEDLRA TIMEDOLA.                             00375000
375100                                                                  00375100
375200     MOVE -1  TO  ERRMSGL.                                        00375200
375300                                                                  00375300
375400     EXEC CICS  SEND   MAP ('GA1XI01')  ERASE  CURSOR             00375400
375500                MAPSET('GA1XSET')   END-EXEC.                     00375500
375600                                                                  00375600
375700*    EXEC CICS  RETURN   END-EXEC.                                00375700
375800     EXEC CICS  RETURN TRANSID('GA1B')                            00375800
375900                COMMAREA(DFHCOMMAREA)                             00375900
376000                LENGTH  (EIBCALEN)                                00376000
376100                END-EXEC.                                         00376100
376200                                                                  00376200
376300 9000-900-EXIT. EXIT.                                             00376300
376400                                                                  00376400
376500/*****************************************************************00376500
376600* 9010  SEND DATAONLY AND RETURN                                 *00376600
376700******************************************************************00376700
376800 9010-000-SEND-DATAONLY-RETURN  SECTION.                          00376800
376900 9010-010.                                                        00376900
377000                                                                  00377000
377100     MOVE -1  TO  ERRMSGL.                                        00377100
377200                                                                  00377200
377300     EXEC CICS  SEND   MAP ('GA1XI01')  DATAONLY  CURSOR          00377300
377400                MAPSET('GA1XSET')  END-EXEC.                      00377400
377500                                                                  00377500
377600*    EXEC CICS  RETURN   END-EXEC.                                00377600
377700     EXEC CICS  RETURN TRANSID('GA1B')                            00377700
377800                COMMAREA(DFHCOMMAREA)                             00377800
377900                LENGTH  (EIBCALEN)                                00377900
378000                END-EXEC.                                         00378000
378100                                                                  00378100
378200 9010-900-EXIT. EXIT.                                             00378200
378300/*****************************************************************00378300
378400* 9200  GREGORIAN TO JULIAN                                      *00378400
378500*                                                                *00378500
378600*         MMDDYY---->YYDDD                                       *00378600
378700******************************************************************00378700
378800 9200-000-GREGORIAN-TO-JULIAN   SECTION.                          00378800
378900 9200-010.                                                        00378900
379000                                                                  00379000
379100     MOVE 'CNV'  TO  HGADATE-FUNC.                                00379100
379200     MOVE 'M'    TO  HGADATE-FORM1.                               00379200
379300     MOVE 'J'    TO  HGADATE-FORM2.                               00379300
379400     MOVE ZEROS  TO HGADATE-RETURN   HGADATE-AMOUNT.              00379400
379500                                                                  00379500
379600     EXEC  CICS LINK PROGRAM ('HGADATES')                         00379600
379700                     COMMAREA(HGADATES-COMMAREA)                  00379700
379800                     LENGTH  (LENGTH OF HGADATES-COMMAREA)        00379800
379900                     END-EXEC.                                    00379900
380000                                                                  00380000
380100 9200-900-EXIT. EXIT.                                             00380100
380200                                                                  00380200
380300/*****************************************************************00380300
380400* 9300  JULIAN TO GREGORIAN                                      *00380400
380500*                                                                *00380500
380600*          YYDDD---->MMDDYY                                      *00380600
380700******************************************************************00380700
380800 9300-000-JULIAN-TO-GREGORIAN   SECTION.                          00380800
380900 9300-010.                                                        00380900
381000                                                                  00381000
381100     MOVE 'CNV'  TO  HGADATE-FUNC.                                00381100
381200     MOVE 'J'    TO  HGADATE-FORM1.                               00381200
381300     MOVE 'M'    TO  HGADATE-FORM2.                               00381300
381400     MOVE ZEROS  TO HGADATE-RETURN   HGADATE-AMOUNT.              00381400
381500                                                                  00381500
381600     EXEC  CICS LINK PROGRAM ('HGADATES')                         00381600
381700                     COMMAREA(HGADATES-COMMAREA)                  00381700
381800                     LENGTH  (LENGTH OF HGADATES-COMMAREA)        00381800
381900                     END-EXEC.                                    00381900
382000                                                                  00382000
382100 9300-900-EXIT. EXIT.                                             00382100
382200                                                                  00382200
382300/*****************************************************************00382300
382400* 9800  ERROR MSG THEN ABEND                                     *00382400
382500*                                                                *00382500
382600*    THIS ROUTINE DISPLAYS THE PREVIOUSLY BUILT ERROR MESSAGE    *00382600
382700*  AND THEN ABENDS USING THE ABEND CODE EARLIER DEFINED.         *00382700
382800******************************************************************00382800
382900 9800-000-ERROR-MSG-THEN-ABEND  SECTION.                          00382900
383000 9800-010.                                                        00383000
383100                                                                  00383100
383200     MOVE -1               TO MFRMSLTL.                           00383200
383300     MOVE WS-ABCODE-MSG    TO ERRMSGO.                            00383300
383400                                                                  00383400
383500     EXEC CICS  SEND   MAP ('GA1XI01')  ERASE  CURSOR  WAIT       00383500
383600                MAPSET('GA1XSET')   END-EXEC.                     00383600
383700                                                                  00383700
383800     EXEC CICS  ABEND   ABCODE(WS-ABCODE)  END-EXEC.              00383800
383900                                                                  00383900
384000 9800-900-EXIT. EXIT.                                             00384000
